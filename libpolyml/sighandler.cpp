/*
    Title:      Signal handling
    Author:     David C.J. Matthews

    Copyright (c) 2000-7 David C.J. Matthews

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
#ifdef WIN32
#include "winconfig.h"
#else
#include "config.h"
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_IO_H
#include <io.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h> // For malloc
#endif


#if (defined(HAVE_STACK_T) && defined(HAVE_SIGALTSTACK))
extern "C" {
// This is missing in older versions of Mac OS X 
int sigaltstack(const stack_t *, stack_t *);
}
#endif

#include "globals.h"
#include "arb.h"
#include "run_time.h"
#include "sighandler.h"
#include "processes.h"
#include "machine_dep.h"
#include "sys.h"
#include "save_vec.h"
#include "rts_module.h"
#include "gc.h" // For gc_phase
#include "scanaddrs.h"
#include "mpoly.h" // For finish

#ifdef WINDOWS_PC
#include "Console.h"
#endif

#define SAVE(x) taskData->saveVec.push(x)
#define SIZEOF(x) (sizeof(x)/sizeof(word))

#define DEFAULT_SIG     0
#define IGNORE_SIG      1

static struct _sigData
{
    unsigned    sigCount; // Number of signals received since last time
    bool        nonMaskable; // True if this sig is used within the RTS.  Must not be ignored or replaced
    PolyWord    handler; // User-installed handler or TAGGED(DEFAULT_SIG) or TAGGED(IGNORE_SIG)
} sigData[NSIG];

static char consoleCode  = 0;
 /* flag indicating if already handling a CTRL+C event. */
static bool already_handling = false;

unsigned receivedSignalCount = 0; // Incremented each time we get a signal

static bool setSimpleSignalHandler(int sig, void (*)(int));


// This function is called under slightly different circumstances in
// Windows and Unix.  In Unix it is a signal handler and is called from
// one of the threads when the signal is delivered.  In Windows it is
// called from ProcessSignalsInMLThread some time after the console
// thread has requested a trap.
#ifdef WINDOWS_PC
void handleINT(void)
#else
static void catchINT(SIG_HANDLER_ARGS(sig, context))
#endif
{
    char   comch        = '\n';
#ifdef WINDOWS_PC
    int    sig = SIGINT;
#else
    SIGNALCONTEXT *scp = (SIGNALCONTEXT *)context;
#endif

    ASSERT(sig == SIGINT); /* SPF */

    trace_allowed = false; /* Switch off tracing. */

    if (already_handling) return; /* Don't recurse. */    
    already_handling = true;

    /* Allow interrupts again. */
#if !defined(WINDOWS_PC)
    { /* use standard SYSV calls */
      sigset_t mask;
      ASSERT(sigemptyset(&mask) == 0);
      ASSERT(sigaddset(&mask,SIGINT) == 0);
      ASSERT(sigprocmask(SIG_UNBLOCK,&mask,NULL) == 0);
    }
#endif

    putc('\n', stdout);

    do 
    {
       /* Read directly from the input. */
       if (comch == '\n') fputs("=>", stdout);
       fflush(stdout);

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
           fputs("Type q(uit - Exit from system)\n", stdout);
           fputs("     c(ontinue running)\n", stdout);
           fputs("     f(ail - Raise an exception)\n", stdout);
           fputs("or   t(race - Get a trace of calls)\n", stdout);
       }
       else if (comch == 't')
       {
            if (gc_phase)
            {
                /* Actually, it might be safe to give a trace in the mark phase? */
                printf("Garbage collecting; stack trace unavailable\n");
                fflush(stdout);
            }
            else
            {
                // It's possible that this could be delivered on a thread
                // other than an ML thread.
                TaskData *taskData = processes->GetTaskDataForThread();
                if (taskData == 0)
                    printf("Unable to get trace information at this point\n");
                else
                {
                    StackObject *stack = taskData->stack;
                    PolyWord *endStack = stack->Offset(stack->Length());
#ifdef WINDOWS_PC
                    // In Windows we only come here once we have saved the state onto the
                    // stack.
                    give_stack_trace(taskData, stack->p_sp, endStack);
#else
                    // The signal can be delivered at any time; possibly while we are
                    // in the run-time system, in which case poly_stack->p_sp will have been
                    // saved on entry and will be valid, and possibly while executing ML
                    // code.  In the latter case we need to try to get the SP value from the
                    // signal context.
                    PolyWord *sp;
                    POLYCODEPTR pc;
                    if (machineDependent->GetPCandSPFromContext(taskData, scp, sp, pc))
                       give_stack_trace(taskData, sp, endStack);
                    else printf("Unable to get trace information at this point\n");
#endif
                }
            }
        }
    } while (comch != 'q' && comch != 'c' && comch != 'f');
    
    already_handling = false; /* About to leave */

    if (comch == 'q') 
    {
        processes->Exit(0);
    }

    if (comch == 'f') 
    { 
        consoleCode = 'f'; /* Raise an exception later. */
#ifndef WINDOWS_PC
        fflush(stdin);
#endif
        processes->RequestThreadsEnterRTS();
        /* The exception is not raised yet. Instead we set up the current
           process so that it will give an interrupt in due course. It will
           eventually enter select_next_process which will look at
           consoleCode and set all the console processes to raise
           exceptions. This may seem a rather roundabout way of doing it but
           it avoids problems with raising the exception in an awkward place. */
    }
} /* catchINT */

#ifdef WINDOWS_PC
// Request an interrupt.
void RequestConsoleInterrupt(void)
{
    addSigCount(SIGINT);
}
#endif

/* Called whenever a signal is raised.  This is set up as a handler for
   all signals which are not otherwise handled and can also be called
   by the RTS handlers if a signal is received which is not processed
   within the RTS. */
void addSigCount(int sig)
{
    sigData[sig].sigCount++;
    receivedSignalCount++;
    processes->RequestThreadsEnterRTS();
}

/* Called whenever a signal handler is installed other than in this
   module. */
void markSignalInuse(int sig)
{
    sigData[sig].nonMaskable = true;
}

/* Find the existing handler for this signal. */
static PolyWord findHandler(int sig)
{
    if ((unsigned)sig >= NSIG) // Check it's in range.
        return TAGGED(DEFAULT_SIG); /* Not there - default action. */
    else return sigData[sig].handler;
}

/* CALL_IO2(Sig_dispatch_,IND) */
/* This function behaves fairly similarly to the Unix and Windows signal
   handler.  It takes a signal number and a handler which may be a function
   or may be 0 (default) or 1 (ignore) and returns the value corresponding
   to the previous handler.  It is complicated because we want to be able
   to inherit handlers from parent databases. 
   I've used a general dispatch function here to allow for future expansion. */
Handle Sig_dispatch_c(TaskData *taskData, Handle args, Handle code)
{
    int c = get_C_long(taskData, DEREFWORDHANDLE(code));
    switch (c)
    {
    case 0: /* Set up signal handler. */
        {
            int sign = get_C_long(taskData, DEREFHANDLE(args)->Get(0));
            int action;
            Handle oldaction;
            /* Decode the action if it is Ignore or Default. */
            if (IS_INT(DEREFHANDLE(args)->Get(1)))
                action = UNTAGGED(DEREFHANDLE(args)->Get(1));
            else action = 2; /* Set the handler. */
            if (sign <= 0 || sign >= NSIG)
                raise_syscall(taskData, "Invalid signal value", EINVAL);

            /* Get the old action before updating the vector. */
            oldaction = SAVE(findHandler(sign));
            /* Now update it. */
            sigData[sign].handler = DEREFWORDHANDLE(args)->Get(1);

            /* If the behaviour has changed we may need to register/unregister a
               handler with the OS. */
            if (! sigData[sign].nonMaskable)
            {
#ifdef WINDOWS_PC
                if (sign == SIGINT)
                {
                }
                else if (action == IGNORE_SIG)
                {
                    if (signal(sign, (action == IGNORE_SIG) ? SIG_IGN: addSigCount)
                            == SIG_ERR)
                        raise_syscall(taskData, "signal failed", errno);
                }
#else
                bool fOK = true;
                if (sigData[sign].handler == TAGGED(IGNORE_SIG))
                    fOK = setSimpleSignalHandler(sign, SIG_IGN);
                else if (sigData[sign].handler == TAGGED(DEFAULT_SIG))
                {
                    if (sign == SIGINT)
                        /* If we set the handler for SIGINT to the default
                           that means we should run our default handler. */
                        fOK = setSignalHandler(sign, catchINT);
                    else fOK = setSimpleSignalHandler(sign, SIG_DFL);
                }
                else fOK = setSimpleSignalHandler(sign, addSigCount);
                if (! fOK)
                    raise_syscall(taskData, "sigaction failed", errno);
#endif
            }
            return oldaction;
        }

    default:
        {
            char msg[100];
            sprintf(msg, "Unknown signal function: %d", c);
            raise_exception_string(taskData, EXC_Fail, msg);
			return 0;
        }
    }
}

// Set up per-thread signal data: basically signal stack.
void initThreadSignals(TaskData *taskData)
{
#if (!(defined(WINDOWS_PC)||defined(MACOSX)))
    // On the i386, at least, we need to set up a signal stack for
    // each thread if it might receive a signal.  ML code checks for
    // stack overflow but a signal could result in C code being
    // executed on the ML stack.  The signal stack avoids this.
    // On some architectures the C stack pointer is left unused
    // when executing ML code so this isn't a problem.
    // In Linux each thread can receive a SIGVTALRM signal when
    // profiling.
    // This is currently disabled in Mac OS X.  In 10.4 and before
    // setting a signal stack in a thread seemed to set it for the
    // whole process and crash with an illegal instruction on the
    // second signal.  This isn't currently a problem since only the
    // main thread receives signals in Mac OS X.
#if (defined(SA_ONSTACK) && defined(HAVE_SIGALTSTACK))
    taskData->signalStack = malloc(SIGSTKSZ);
#ifdef HAVE_STACK_T
    stack_t ex_stack;
#else
    // This used to be used in FreeBSD and Mac OS X
    struct sigaltstack ex_stack;
#endif
    memset(&ex_stack, 0, sizeof(ex_stack));
    ex_stack.ss_sp    = taskData->signalStack;
    ex_stack.ss_size  = SIGSTKSZ;
    ex_stack.ss_flags = 0; /* not SS_DISABLE */
    int sigaltstack_result = sigaltstack(&ex_stack, NULL);
    ASSERT(sigaltstack_result == 0);
#endif
#endif /* not the PC */
}


/* General purpose function to set up a signal handler. */
#ifndef WINDOWS_PC

bool setSignalHandler(int sig, signal_handler_type func)
{
    struct sigaction sigcatch;
    memset(&sigcatch, 0, sizeof(sigcatch));
    sigcatch.sa_sigaction = func;

    /*
    Both Linux and FreeBSD now use SA_SIGINFO in a similar way.  If SA_SIGINFO is set the
    handler is supposed to be in sa_sigaction rather than sa_handler (actually this is a union
    so they're in the same place).  
    */

    init_asyncmask(&sigcatch.sa_mask);
    sigcatch.sa_flags = 0;
#if defined(SA_ONSTACK) && defined(HAVE_SIGALTSTACK)
    sigcatch.sa_flags |= SA_ONSTACK;
#endif
#ifdef SA_RESTART
    sigcatch.sa_flags |= SA_RESTART;
#endif
#ifdef SA_SIGINFO
    sigcatch.sa_flags |= SA_SIGINFO;
#endif
#ifdef SV_SAVE_REGS
    sigcatch.sa_flags |= SV_SAVE_REGS;
#endif

    return sigaction(sig, &sigcatch,NULL) >= 0;
}

// Simpler version for SIG_IGN and SIG_DFL
bool setSimpleSignalHandler(int sig, void (*func)(int))
{
    struct sigaction sigcatch;
    memset(&sigcatch, 0, sizeof(sigcatch));
    sigcatch.sa_handler = func;

    init_asyncmask(&sigcatch.sa_mask);
    sigcatch.sa_flags = 0;
    if (func != SIG_IGN && func != SIG_DFL)
    {
#if defined(SA_ONSTACK) && defined(HAVE_SIGALTSTACK)
        sigcatch.sa_flags |= SA_ONSTACK;
#endif
#ifdef SA_RESTART
        sigcatch.sa_flags |= SA_RESTART;
#endif
#ifdef SA_SIGINFO
        sigcatch.sa_flags |= SA_SIGINFO;
#endif
#ifdef SV_SAVE_REGS
        sigcatch.sa_flags |= SV_SAVE_REGS;
#endif
    }
    return sigaction(sig, &sigcatch,NULL) >= 0;
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
    sigaddset(mask,SIGUSR2);
    sigaddset(mask,SIGWINCH);

    // This next used to be needed when emulation traps resulted in
    // signals.  This no longer applies except on the Sparc.
#ifdef SPARC
    sigaddset(mask,SIGILL);
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
    /* And, just to be sure, include SIGBUS.  DCJM 22/5/02. */
    sigaddset(mask,SIGBUS);
#endif
}

#endif

class SigHandler: public RtsModule
{
public:
    virtual void Init(void);
    virtual void Reinit(void);
    virtual void GarbageCollect(ScanAddress * /*process*/);
};

// Declare this.  It will be automatically added to the table.
static SigHandler sighandlerModule;

void SigHandler::Init(void)
{
}

void SigHandler::Reinit(void)
{
    // Reset the signal vector.
    for (unsigned i = 0; i < NSIG; i++)
        sigData[i].handler = TAGGED(0);

#if defined(WINDOWS_PC)
    // Create a thread to handle ^C.

#else /* UNIX version */

    // Set up a handler for the interrupt exception unless it is
    // currently being ignored.
    {
        void (*old_status)(int) = signal(SIGINT,SIG_IGN);
        ASSERT(old_status != SIG_ERR);
        if (old_status != SIG_IGN)
            setSignalHandler(SIGINT, catchINT);
    }
#endif /* END of UNIX-specific signal setting */
}


// This is called by an ML thread to create new threads to handle
// the signals.
void ProcessSignalsInMLThread(TaskData *taskData)
{
    int i;
    for (i = 0; i < NSIG; i++)
    {
        while (sigData[i].sigCount > 0)
        {
            sigData[i].sigCount--;
            PolyWord hand = findHandler(i);
            if (!IS_INT(hand)) /* If it's not DEFAULT or IGNORE. */
            {
                /* We may go round this loop an indeterminate number of
                   times.  To prevent the save vec from overflowing we
                   mark it and reset it. */
                Handle saved = taskData->saveVec.mark();
                // Create a new thread to run the handler.
                Handle h = SAVE(hand);
                processes->ForkFromRTS(taskData, h, SAVE(TAGGED(i)));
                taskData->saveVec.reset(saved);
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
    if (consoleCode == 'f')
    {
        consoleCode = 0;
        processes->BroadcastInterrupt();
    }
}

void SigHandler::GarbageCollect(ScanAddress *process)
{
    for (unsigned i = 0; i < NSIG; i++)
    {
        if (sigData[i].handler != PolyWord::FromUnsigned(0) && sigData[i].handler.IsDataPtr())
        {
            PolyObject *obj = sigData[i].handler.AsObjPtr();
            process->ScanRuntimeAddress(&obj, ScanAddress::STRENGTH_STRONG);
            sigData[i].handler = obj;
        }
    }
}
