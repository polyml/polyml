/*
    Title:      Signal handling
    Author:     David C.J. Matthews

    Copyright (c) 2000-8 David C.J. Matthews

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

/*
Signal handling is complicated in a multi-threading environment
and because it differs between versions of Unix.

Signal handlers as such are not used.  Instead we use sigwait to
detect signals synchronously on a separate signal detection thread.
For this to work, the signals we are interested in must be blocked
from delivery to any thread.  Signals set to SIG_IGN or SIG_DFL must
not be completely blocked otherwise they will be queued.  To enable
a signal's state to be changed we block all signals, with the exception
of signals such as SIGSEGV that indicate an error or SIGVTALRM that are
used by the RTS.  Signals that are not handled are unblocked on the
main thread.

In Linux if a signal is unblocked on any thread and
set to SIG_DFL the default action will be performed.  In Mac OS X
that only happens if the signal is unblocked in the main thread.

This is messier still because of what seems like a bug in Cygwin.
In Cygwin if a signal is included in the signal mask argument it
is blocked and is delivered as the result of sigwait even if it is
not otherwise blocked.  So, we can only include signals in the
argument to sigwait if there is currently a handler installed and
we need to wake up sigwait, by sending it a SIGUSR1, if a handler
is installed or removed.
DCJM Feb 2008.
*/


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
#include "gc.h" // For convertedWeak
#include "scanaddrs.h"
#include "locking.h"

#ifdef WINDOWS_PC
#include "Console.h"
#endif

#define SAVE(x) taskData->saveVec.push(x)
#define SIZEOF(x) (sizeof(x)/sizeof(word))

#define DEFAULT_SIG     0
#define IGNORE_SIG      1
#define HANDLE_SIG      2 // This is only used in SignalRequest

static struct _sigData
{
    bool        nonMaskable; // True if this sig is used within the RTS.  Must not be ignored or replaced
    PolyWord    handler; // User-installed handler, TAGGED(DEFAULT_SIG) or TAGGED(IGNORE_SIG)
    int         signalCount;
} sigData[NSIG];

unsigned receivedSignalCount = 0; // Incremented each time we get a signal

// sigLock protects access to the signalCount values in sigData but
// not the "handler" field.
static PLock sigLock;

#if (defined(HAVE_LIBPTHREAD) && defined(HAVE_PTHREAD_H))
pthread_t detectionThreadId; // Thread processing signals.
#endif

// This must not be called from an asynchronous signal handler.
static void signalArrived(int sig)
{
    sigLock.Lock();
    receivedSignalCount++;
    sigData[sig].signalCount++;
    sigLock.Unlock();
    // To avoid deadlock we must release sigLock first.
    processes->SignalArrived();
}

// Called whenever a signal handler is installed other than in this
// module.   Because modules are initialised in an unspecified order
// we may have already masked off this signal.
void markSignalInuse(int sig)
{
    sigData[sig].nonMaskable = true;
#if (defined(HAVE_LIBPTHREAD) && defined(HAVE_PTHREAD_H))
    // Enable this signal.
    sigset_t sigset;
    sigemptyset(&sigset);
    sigaddset(&sigset, sig);
    pthread_sigmask(SIG_UNBLOCK, &sigset, NULL);
#endif
}

/* Find the existing handler for this signal. */
static PolyWord findHandler(int sig)
{
    if ((unsigned)sig >= NSIG) // Check it's in range.
        return TAGGED(DEFAULT_SIG); /* Not there - default action. */
    else return sigData[sig].handler;
}

#ifdef WINDOWS_PC
// This is called to simulate a SIGINT in Windows.
void RequestConsoleInterrupt(void)
{
    // The default action for SIGINT is to exit.
    if (findHandler(SIGINT) == TAGGED(DEFAULT_SIG))
        processes->Exit(2); // Exit with the signal value.
    else signalArrived(SIGINT);
}
#endif

#if (defined(HAVE_LIBPTHREAD) && defined(HAVE_PTHREAD_H))
// Request the main thread to change the blocking state of a signal.
class SignalRequest: public MainThreadRequest
{
public:
    SignalRequest(int s, int r): signl(s), state(r) {}

    virtual void Perform();
    int signl, state;
};

void SignalRequest::Perform()
{
    sigset_t sigset;
    sigemptyset(&sigset);
    sigaddset(&sigset, signl);

    switch (state)
    {
    case DEFAULT_SIG:
        pthread_sigmask(SIG_UNBLOCK, &sigset, NULL);
        signal(signl, SIG_DFL);
        break;
    case IGNORE_SIG:
        pthread_sigmask(SIG_UNBLOCK, &sigset, NULL);
        signal(signl, SIG_IGN);
        break;
    case HANDLE_SIG:
        pthread_sigmask(SIG_BLOCK, &sigset, NULL);
        signal(signl, SIG_DFL);
        break;
    }
#ifdef __CYGWIN__
    // Send a SIGUSR1 to wake up the signal detection thread and
    // allow it to change its mask.
    sigData[SIGUSR1].signalCount--;
    pthread_kill(detectionThreadId, SIGUSR1);
#endif
}
#endif


/* CALL_IO2(Sig_dispatch_,IND) */
/* This function behaves fairly similarly to the Unix and Windows signal
   handler.  It takes a signal number and a handler which may be a function
   or may be 0 (default) or 1 (ignore) and returns the value corresponding
   to the previous handler. 
   I've used a general dispatch function here to allow for future expansion. */
Handle Sig_dispatch_c(TaskData *taskData, Handle args, Handle code)
{
    int c = get_C_long(taskData, DEREFWORDHANDLE(code));
    switch (c)
    {
    case 0: /* Set up signal handler. */
        {
            int sign;
            int action;
            Handle oldaction;
            {
                // Lock while we look at the signal vector but release
                // it before making a root request.
                PLocker locker(&sigLock);
                // We have to pass this to the main thread to ensure there
                // is only one thread that blocks and unblocks signals.
                sign = get_C_long(taskData, DEREFHANDLE(args)->Get(0));
                /* Decode the action if it is Ignore or Default. */
                if (IS_INT(DEREFHANDLE(args)->Get(1)))
                    action = UNTAGGED(DEREFHANDLE(args)->Get(1));
                else action = HANDLE_SIG; /* Set the handler. */
                if (sign <= 0 || sign >= NSIG)
                    raise_syscall(taskData, "Invalid signal value", EINVAL);

                /* Get the old action before updating the vector. */
                oldaction = SAVE(findHandler(sign));
                // Now update it.
                sigData[sign].handler = DEREFWORDHANDLE(args)->Get(1);
            }

            // Request a change in the masking by the root thread.
            // This doesn't do anything in Windows so the only "signal"
            // we affect is SIGINT and that is handled by RequestConsoleInterrupt.
            if (! sigData[sign].nonMaskable)
            {
#if (defined(HAVE_LIBPTHREAD) && defined(HAVE_PTHREAD_H))
                SignalRequest request(sign, action);
                processes->MakeRootRequest(taskData, &request);
#endif
            }
            return oldaction;
        }

    case 1: // Called by the signal handler thread.  Blocks until a signal
            // is available.
        {
            while (true)
            {
                processes->ProcessAsynchRequests(taskData); // Check for kill.
                sigLock.Lock();
                // Any pending signals?
                for (int sig = 0; sig < NSIG; sig++)
                {
                    if (sigData[sig].signalCount > 0)
                    {
                        sigData[sig].signalCount--;
                        if (!IS_INT(findHandler(sig))) /* If it's not DEFAULT or IGNORE. */
                        {
                            // Create a pair of the handler and signal and pass
                            // them back to be run.
                            Handle pair = alloc_and_save(taskData, 2);
                            // Have to call findHandler again here because that
                            // allocation could have garbage collected.
                            DEREFHANDLE(pair)->Set(0, findHandler(sig));
                            DEREFHANDLE(pair)->Set(1, TAGGED(sig));
                            sigLock.Unlock();
                            return pair;
                        }
                    }
                }
                if (convertedWeak)
                {
                    // Last GC converted a weak SOME into NONE.  This isn't
                    // anything to do with signals but the signal thread can
                    // deal with this.
                    sigLock.Unlock();
                    convertedWeak = false;
                    return SAVE(TAGGED(0));
                }
                // No pending signal.  Wait until we're woken up.
                // This releases sigLock after acquiring schedLock.
                if (! processes->WaitForSignal(taskData, &sigLock))
                    raise_exception_string(taskData, EXC_Fail, "Only one thread may wait for signals");
            }
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
// This is really only needed for profiling timer signals.
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
#if (defined(HAVE_LIBPTHREAD) && defined(HAVE_PTHREAD_H))
    // Block all signals except those marked as in use by the RTS so
    // that they will only be picked up by the signal detection thread.
    // Since the signal mask is inherited we really don't need to do
    // this for every thread, just the initial one.
    sigset_t sigset;
    sigfillset(&sigset);
    for (int i = 0; i < NSIG; i++)
    {
        if (sigData[i].nonMaskable)
            sigdelset(&sigset, i);
    }
    pthread_sigmask(SIG_SETMASK, &sigset, NULL);
#endif
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

// Signals to mask off when handling a signal.  The signal being handled
// is always masked off.  This really only applied when emulation traps
// and requests to GC involved signals.  That no longer applies except
// on the Sparc.
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

#if (defined(HAVE_LIBPTHREAD) && defined(HAVE_PTHREAD_H))
// This thread simply detects signals synchronously.  It is unsafe
// to call pthread functions from an asynchronous signal handler
// so this waits for the signal and then passes this on to the ML
// signal handler thread.
static void *SignalDetectionThread(void *)
{
    // Block all signals so they will be picked up by sigwait.
    sigset_t active_signals;    
    sigfillset(&active_signals);
    pthread_sigmask(SIG_SETMASK, &active_signals, NULL);

    while (true)
    {
#ifdef __CYGWIN__
        // Work around for Cygwin bug.  In Cygwin, if a signal is included
        // in the signal set argument to sigwait it will be blocked from 
        // being handled by default even if it another thread has it
        // unblocked.
        sigLock.Lock();
        sigemptyset(&active_signals);
        sigaddset(&active_signals, SIGUSR1); // We need this to be able to wake up.
        signal(SIGUSR1, SIG_DFL);
        int i;
        // First compute the mask.
        for (i = 0; i < NSIG; i++)
        {
            if (! IS_INT(sigData[i].handler)) // There's a handler.
                sigaddset(&active_signals, i);
        }
        sigLock.Unlock();
#endif

        int sig = -1;
        int result = sigwait(&active_signals, &sig);
        if (result == 0)
            signalArrived(sig);
        else if (result != EINTR) // Can get EINTR when running in gdb.
        {
            fprintf(stderr, "sigwait returned with %d.  errno = %d\n", result, errno);
            return 0;
        }
    }
}
#endif


void SigHandler::Init(void)
{
    // Mark certain signals as non-maskable since they really
    // indicate a fatal error.
#ifdef SIGSEGV
    sigData[SIGSEGV].nonMaskable = true;
#endif
#ifdef SIGBUS
    sigData[SIGBUS].nonMaskable = true;
#endif
#ifdef SIGILL
    sigData[SIGILL].nonMaskable = true;
#endif
#if (defined(HAVE_LIBPTHREAD) && defined(HAVE_PTHREAD_H))
    // Create a new thread to handle signals synchronously.
    // for it to finish.
    pthread_attr_t attrs;
    pthread_attr_init(&attrs);
    pthread_attr_setdetachstate(&attrs, PTHREAD_CREATE_DETACHED);
#ifdef PTHREAD_STACK_MIN
    pthread_attr_setstacksize(&attrs, PTHREAD_STACK_MIN); // Only small stack.
#endif
    pthread_create(&detectionThreadId, &attrs, SignalDetectionThread, 0);
    pthread_attr_destroy(&attrs);
#endif
}

void SigHandler::Reinit(void)
{
    // Reset the signal vector and determine the initial settings.
    for (unsigned i = 0; i < NSIG; i++)
    {
        if (sigData[i].nonMaskable)
            // Don't mess with any installed signal handlers.
            sigData[i].handler = TAGGED(IGNORE_SIG);
        else
        {
            void (*old_status)(int) = signal(i, SIG_IGN);
            if (old_status != SIG_ERR)
                signal(i, old_status);
            if (old_status == SIG_IGN)
                sigData[i].handler = TAGGED(IGNORE_SIG);
            else sigData[i].handler = TAGGED(DEFAULT_SIG);
        }
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
