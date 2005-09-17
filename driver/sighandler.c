/*
    Title:      Signal handling
    Author:     David C.J. Matthews

	Copyright (c) 2000 David C.J. Matthews

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later version.
	
	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.
	
	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/
#include <signal.h>
#include <errno.h>
#include <assert.h>
#include <string.h>

#ifdef WINDOWS_PC
#include <io.h>
#else
#include <unistd.h>
#endif

#include "globals.h"
#include "mm.h"
#include "mmap.h"
#include "mpoly.h"
#include "arb.h"
#include "run_time.h"
#include "sighandler.h"
#include "proper_io.h"
#include "processes.h"
#include "machine_dep.h"

#ifdef WINDOWS_PC
#include "Console.h"
#endif

#define ASSIGN(p,v) runtime_assign_word((word *)(&(p)),(word)(v))
#define MUTABLE(x) (OBJ_MUTABLE_BIT | (x))
#define SAVE(x) push_to_save_vec((word)(x))
#define ALLOC(n) alloc_and_save(n)
#define SIZEOF(x) (sizeof(x)/sizeof(word))

#define DEFAULT_SIG		0
#define IGNORE_SIG		1

/* This table counts the number of signals received since the last time
   we were in a position to create the handlers. */
static int sigCount[NSIG];
/* This represents a set of signals which are used within the RTS.  They
   must not be ignored or replaced by another handler. */
static int nonMaskable[NSIG];

static char consoleCode  = 0;
 /* flag indicating if already handling a CTRL+C event. */
static int already_handling = 0;

#ifdef WINDOWS_PC
void handleINT(void)
#elif defined(LINUX)
static void catchINT(int sig, struct sigcontext context)
#elif defined(SOLARIS2) || defined(HPUX64)
static void catchINT(int sig, siginfo_t  *info, SIGNALCONTEXT *scp)
#else
static void catchINT(int sig, int code, SIGNALCONTEXT *scp)
#endif
{
    char   comch        = '\n';
#ifdef WINDOWS_PC
	int    sig = SIGINT;
#endif

    assert(sig == SIGINT); /* SPF */
#ifdef WINDOWS_PC
#elif !defined(LINUX)
    assert(scp != NULL);
#endif

    trace_allowed = 0; /* Switch off tracing. */

    if (already_handling) return; /* Don't recurse. */    
    already_handling = 1;

    /* Allow interrupts again. */
#if !defined(WINDOWS_PC)
    { /* use standard SYSV calls */
      sigset_t mask;
      assert(sigemptyset(&mask) == 0);
      assert(sigaddset(&mask,SIGINT) == 0);
      assert(sigprocmask(SIG_UNBLOCK,&mask,NULL) == 0);
    }
#endif

    proper_putc('\n', stdout);


    do 
    {
       /* Read directly from the input. */
       if (comch == '\n')
       {
          proper_fputs("=>", stdout);
       }
       proper_fflush(stdout);

#ifdef WINDOWS_PC
	   {
		   int nChars;
		   if (useConsole)
		   {
			   /* Use our console.  Ignore extra control-Cs. */
			   while ((nChars = getConsoleInput(&comch, 1)) < 0);
		   }
		   else nChars = read(fileno(stdin), &comch, 1);
		   if (nChars != 1)
		   {
			   comch = 'c';
			   break;
		   }
	   }
#else
	   /* Unix. */
       if (read(fileno(stdin), &comch, 1) != 1)
       {
          comch = 'q';
          break;
       }
#endif
       
       if (comch == '?')
       {
          proper_fputs("Type q(uit - Exit from system)\n", stdout);
          proper_fputs("     c(ontinue running)\n", stdout);
          proper_fputs("     f(ail - Raise an exception)\n", stdout);
          proper_fputs("     s(witch - Switch shells)\n", stdout);
          proper_fputs("or   t(race - Get a trace of calls)\n", stdout);
       }
       else if (comch == 't')
       {
         if (A.garbage_collecting)
         {
            /* Actually, it might be safe to give a trace in the mark phase? */
            proper_printf("Garbage collecting; stack trace unavailable\n");
            proper_fflush(stdout);
         }
         else
         {
            /* 
               If we are running ML code, we need to dump the stack
               pointer, handler register etc. into the poly_stack
               data-structure, otherwise we get a bogus stack trace.
               We need to be very careful here, since this will
               corrupt the rest of the poly_stack data-structure
               (since we can't rely on the registers we're dumping
               being properly tagged). This should be OK though,
               as we'll dump the updated versions before using
               the data-structure for anything else (a garbage-collect,
               emulator trap or RTS call). The result is still not
               perfect, because it ignores the function that we're
               actually executing, but it's better than what we
               had before. SPF 11/9/95
            */
#if defined(POWER2) || defined(HPPA)
           /* 
              Unfortunately, the necessary MD_save_ML_state function
              isn't implemented on all machines yet. SPF 11/9/95
           */
           if (!in_run_time_system)
           {
#ifdef LINUX
              MD_save_ML_state(&context);
#else
              MD_save_ML_state(scp);
#endif
           }
#endif       
           give_stack_trace(end_of_stack);
         }
      }
    } while (comch != 'q' && comch != 'c' && comch != 'f' && comch != 's');

    already_handling = 0; /* About to leave */
    if (comch == 'q') 
	{
#ifdef WINDOWS_PC
		RequestFinish(0);
		/* Continue running while we close down. */
#else
		finish(0); /* And exit */
#endif
	}

    if (comch == 's')
    {
        consoleCode = 's'; /* Switch shells later. */
        proper_fflush(stdin);
        interrupted = sig;
        
#if defined(WINDOWS_PC)
        MD_interrupt_code(); /* Set up interrupt */
#elif defined(LINUX)
        MD_interrupt_code_using_context(&context);
#else
        MD_interrupt_code_using_context(scp);
#endif
   }

    if (comch == 'f') 
    { 
        consoleCode = 'f'; /* Raise an exception later. */
#ifndef WINDOWS_PC
        proper_fflush(stdin);
#endif
        interrupted = sig;
        
#if defined(WINDOWS_PC)
        MD_interrupt_code(); /* Set up interrupt */
#elif defined(LINUX)
        MD_interrupt_code_using_context(&context);
#else
        MD_interrupt_code_using_context(scp);
#endif

        /* The exception is not raised yet. Instead we set up the current
           process so that it will give an interrupt in due course. It will
           eventually enter select_next_process which will look at
           consoleCode and set all the console processes to raise
           exceptions. This may seem a rather roundabout way of doing it but
           it avoids problems with raising the exception in an awkward place. */
    }
} /* catchINT */

#ifdef WINDOWS_PC

static BOOL WINAPI catchINT(DWORD event)
{
    /* default handling for other events */
    if (event != CTRL_C_EVENT) return FALSE;
	RequestConsoleInterrupt();
	return TRUE; /* control-C successfully handled */
}

static int enableInterrupt = 1;

// Request an interrupt.
void RequestConsoleInterrupt(void)
{
	addSigCount(SIGINT);
}

/* Disable interrupts during commit.  This isn't really needed because
   we don't actually do anything until the our interrupt handler is called. */
void SetResetCC(const int enable)
{
	/* N.B. This only works in Windows NT. */
	SetConsoleCtrlHandler(NULL, enable ? TRUE : FALSE);
}
#endif

/* Called whenever a signal is raised.  This is set up as a handler for
   all signals which are not otherwise handled and can also be called
   by the RTS handlers if a signal is received which is not processed
   within the RTS. */
void addSigCount(int sig)
{
	sigCount[sig]++;
}

/* Called whenever a signal handler is installed other than in this
   module. */
void markSignalInuse(int sig)
{
	nonMaskable[sig] = 1;
}

void uninit_sig_handler(void)
{
}

/* Find the existing handler for this signal. */
static word findHandler(int sig)
{
	Header h = H;
	while (h)
	{
		root_struct *root = Root(h);
		if (root->signal_vector == 0 ||
			root->signal_vector == (word*)TAGGED(0) || /* No signal vector. */
			sig >= OBJECT_LENGTH(root->signal_vector) || /* Not in the vector??? */
			root->signal_vector[sig] == 0 || /* Never set for this signal */
			root->signal_vector[sig] == TAGGED(DEFAULT_SIG))
		{
			/* In all these cases we need to look in the parent. */
			h = ParentDatabase(h);
			continue;
		}
		else return root->signal_vector[sig];
	}
	return TAGGED(DEFAULT_SIG); /* Not there - default action. */
}

/* CALL_IO2(Sig_dispatch_,IND) */
/* This function behaves fairly similarly to the Unix and Windows signal
   handler.  It takes a signal number and a handler which may be a function
   or may be 0 (default) or 1 (ignore) and returns the value corresponding
   to the previous handler.  It is complicated because we want to be able
   to inherit handlers from parent databases. 
   I've used a general dispatch function here to allow for future expansion. */
Handle Sig_dispatch_c(Handle args, Handle code)
{
	int c = get_C_long(DEREFWORDHANDLE(code));
	switch (c)
	{
	case 0: /* Set up signal handler. */
		{
			root_struct *root = Root(H);
			int sign = get_C_long(DEREFHANDLE(args)[0]);
			int action;
			Handle oldaction;
			/* Decode the action if it is Ignore or Default. */
			if (IS_INT(DEREFHANDLE(args)[1]))
				action = UNTAGGED(DEREFHANDLE(args)[1]);
			else action = 2; /* Set the handler. */
			if (sign <= 0 || sign >= NSIG)
				raise_syscall("Invalid signal value", EINVAL);

			/* If no signal_vector has ever been created this value may be either
			   zero or TAGGED(0).  The parent database seems to have this as
			   TAGGED(0) but child databases are reated with this
			   containing the default value used by alloc, currently zero. */
			if (root->signal_vector == 0 ||
				root->signal_vector == (word*)TAGGED(0))
			{
				int i;
				ASSIGN(root->signal_vector, alloc(MUTABLE(NSIG)));
				for (i = 0; i < NSIG; i++)
					root->signal_vector[i] = TAGGED(DEFAULT_SIG);
			}

			/* Get the old action before updating the vector. */
			oldaction = SAVE(findHandler(sign));
			/* Now update it. */
			ASSIGN(root->signal_vector[sign], DEREFHANDLE(args)[1]);

			/* If the behaviour has changed we may need to register/unregister a
			   handler with the OS. */
			if (! nonMaskable[sign])
			{
#ifdef WINDOWS_PC
				if (sign == SIGINT)
				{
					if (action == IGNORE_SIG) enableInterrupt = 0;
					else enableInterrupt = 1;
				}
				else if (action == IGNORE_SIG)
				{
					if (signal(sign, (action == IGNORE_SIG) ? SIG_IGN: addSigCount)
							== SIG_ERR)
						raise_syscall("signal failed", errno);
				}
#else
				signal_handler_type func;
				if (root->signal_vector[sign] == TAGGED(IGNORE_SIG))
					func = SIG_IGN;
				else if (root->signal_vector[sign] == TAGGED(DEFAULT_SIG))
				{
					if (sign == SIGINT)
						/* If we set the handler for SIGINT to the default
						   that means we should run our default handler. */
						func = (signal_handler_type)catchINT;
					else func = SIG_DFL;
				}
				else func = addSigCount;
				if (setSignalHandler(sign, func) < 0)
					raise_syscall("sigaction failed", errno);
#endif
			}
			return oldaction;
		}

	default: crash("Unknown signal function: %d\n", c);
	}
}

/* This function is called when the system is in a safe state, some
   time after an interrupt.
   We can then fork any processes needed to handle the interrupts.
*/
static void SigInterrupts(int signum /* not used */)
{
	int i;
	int haveSignalled = 0;
	for (i = 0; i < NSIG; i++)
	{
		while (sigCount[i] > 0)
		{
			word hand;
			haveSignalled = 1;
			sigCount[i]--;
			hand = findHandler(i);
			if (!IS_INT(hand)) /* If it's not DEFAULT or IGNORE. */
			{
				/* We may go round this loop an indeterminate number of
				   times.  To prevent the save vec from overflowing we
				   mark it and reset it. */
				Handle saved = mark_save_vec();
				/* Create a new process to run the handler. */
				Handle h = SAVE(hand);
				(void)fork_function(h, SAVE(TAGGED(i)));
				reset_save_vec(saved);
			}
#ifdef WINDOWS_PC
	/* 
	I've tried several different ways of dealing with ctrl-C in Windows and
	none of them was satisfactory.  The difficulty is that there is no way,
	as far as I can tell, to cause a running thread to asynchronously execute
	a handler.  AHL's original idea was to use a separate thread for the
	interrupt handler (this happens anyway with SetConsoleCtrlHandler)
	and then call SuspendThread to suspend the ML thread.  That's probably
	necessary to generate a stack trace at least, but runs into trouble with
	the multithreaded C run-time library.  That uses an interlock to prevent
	more than one thread reading or writing a stream.  That includes the
	low-level IO functions, "read" and "write", which are not system calls
	in Windows.  If the main thread happens to be doing IO when we start the
	control-C handler the whole thing will lock up.  The only solution seems
	to be to handle control-C synchronously.
	DCJM June 2000.
	*/
			else if (hand == TAGGED(DEFAULT_SIG) && i == SIGINT)
			{
				handleINT();
			}
#endif
		}
	}
	if (consoleCode == 's')
	{
		consoleCode = 0;
		switch_subshells_c();
	}
	if (consoleCode == 'f')
	{
		consoleCode = 0;
		interrupt_console_processes();
		interrupt_signal_processes();
	}
	else if (haveSignalled) interrupt_signal_processes();
}

void init_sig_handler(void)
{
	register_interrupt_proc(SigInterrupts);
}

/* This function is called when the database has been loaded.
   We can examine the signal handler vectors for the child and
   parent databases and set up signal handlers as appropriate. */
void re_init_sig_handler(void)
{
	Header h = H;
	int done[NSIG];
	memset(done, 0, sizeof(done));
	while (h)
	{
		root_struct *root = Root(h);
		if (root->signal_vector != 0 &&
			root->signal_vector != (word*)TAGGED(0))
		{
			int i;
			int len = OBJECT_LENGTH(root->signal_vector);
			for (i = 0; i < len; i++)
			{
				if (done[i] == 0 &&
					root->signal_vector[i] != 0 && /* Never set for this signal */
					root->signal_vector[i] != TAGGED(DEFAULT_SIG))
				{
#ifdef WINDOWS_PC
					if (root->signal_vector[i] == TAGGED(IGNORE_SIG))
						signal(i, SIG_IGN);
					else signal(i, addSigCount);
#else
					if (root->signal_vector[i] == TAGGED(IGNORE_SIG))
						setSignalHandler(i, SIG_IGN);
					else setSignalHandler(i, addSigCount);
#endif
					done[i] = 1; /* Don't process this signal again. */
				}
			}
		}
		h = ParentDatabase(h);
	}
	if (! done[SIGINT])
	{
		/* If there is no specific handler for SIGINT we set the default one. */
		/* Set up handler for control-C */
#if defined(WINDOWS_PC)
		/* Set up a console handler.  Actually this is no longer used since
		   the process is not a console process. */
		SetConsoleCtrlHandler(catchINT, TRUE);

#else /* UNIX version */

	    /* Set up a handler for the interrupt exception unless it is
	      currently being ignored. */
		{
			VoidFunc old_status = signal(SIGINT,SIG_IGN);
			assert(old_status != SIG_ERR);
			if (old_status != SIG_IGN)
				setSignalHandler(SIGINT,
					(signal_handler_type)catchINT);
		}
#endif /* END of UNIX-specific signal setting */
	}
}

/* General purpose function to set up a signal handler. */
#ifndef WINDOWS_PC
/* Different versions of Unix define different combinations of these
   flags.  If they are undefined define them here as 0. */
#ifndef SA_ONSTACK
#define SA_ONSTACK	0
#endif
#ifndef SA_RESTART
#define SA_RESTART	0
#endif
#ifndef SA_SIGINFO
#define SA_SIGINFO	0
#endif

int setSignalHandler(int sig, signal_handler_type func)
{
	int res;
	struct sigaction sigcatch;
	memset(&sigcatch, 0, sizeof(sigcatch));
	sigcatch.sa_handler = func;

	init_asyncmask(&sigcatch.sa_mask);
#ifdef LINUX
	/* Must not define SA_SIGINFO in Linux.  Doing so causes the
	   signal to provide a siginfo structure not a context. */
	sigcatch.sa_flags = SA_ONSTACK | SA_RESTART;
#else
	sigcatch.sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
#endif
#ifdef MACOSX
        sigcatch.sa_flags |= SV_SAVE_REGS;
#endif
	res = sigaction(sig, &sigcatch,NULL);

	/* Mark this signal in use, except for SIGINT which is treated specially. */
	if (sig != SIGINT) markSignalInuse(sig);
	return res;
}

/******************************************************************************/
/*                                                                            */
/*      init_asyncmask - utility function - doesn't allocate                  */
/*                                                                            */
/******************************************************************************/
void init_asyncmask(sigset_t *mask)
{
  /* disable asynchronous interrupts while servicing interrupt */
  sigemptyset(mask);
  sigaddset(mask,SIGVTALRM);
  sigaddset(mask,SIGINT);
  sigaddset(mask,SIGALRM);
  sigaddset(mask,SIGWINCH);
  sigaddset(mask,SIGILL);
#if !defined(LINUX)
  sigaddset(mask,SIGEMT);
#endif
  sigaddset(mask,SIGFPE);

  /* Mask off SIGSEGV.  This is definitely needed when we are
     installing a handler for SIGINT under Linux and may also
	 be needed in other cases as well e.g. SIGVTALRM. Without
	 it typing control-C to a program which is taking lots
	 of emulation traps can cause a crash because the signals
	 are delivered in the "wrong" order and the pc value given
     to catchSEGV can point at the handler for SIGINT.
     DCJM 7/2/01. */
  sigaddset(mask,SIGSEGV);
#if (defined(MACOSX))
  /* Add SIGTRAP for a similar reason in Mac OS X. DCJM 22/5/02. */
  sigaddset(mask,SIGTRAP);
#endif
  /* And, just to be sure, include SIGBUS.  DCJM 22/5/02. */
  sigaddset(mask,SIGBUS);

  return;
}
#endif
