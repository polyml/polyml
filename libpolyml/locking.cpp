/*
    Title:      Mutex and Condition Variable library.

    Copyright (c) 2007, 2012 David C. J. Matthews

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#if ((!defined(_WIN32) || defined(__CYGWIN__)) && defined(HAVE_PTHREAD_H))
#define HAVE_PTHREAD 1
#include <pthread.h>
#elif (defined(HAVE_WINDOWS_H))
// We need the next define to get TryEnterCriticalSection and
// InitializeCriticalSectionAndSpinCount.
#define _WIN32_WINNT 0x0500
#include <windows.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#if ((!defined(_WIN32) || defined(__CYGWIN__)) && defined(HAVE_SEMAPHORE_H))
// Don't include semaphore.h on Mingw.  It's provided but doesn't compile.
#include <semaphore.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#include "locking.h"
#include "diagnostics.h"

// Report contended locks after this many attempts
#define LOCK_REPORT_COUNT   50

PLock::PLock(const char *n): lockName(n), lockCount(0)
{
#ifdef HAVE_PTHREAD
    pthread_mutex_init(&lock, 0);
#elif defined(HAVE_WINDOWS_H)
    InitializeCriticalSectionAndSpinCount(&lock, 12000);
#endif
}

PLock::~PLock()
{
#ifdef HAVE_PTHREAD
    pthread_mutex_destroy(&lock);
#elif defined(HAVE_WINDOWS_H)
    DeleteCriticalSection(&lock);
#endif
}

void PLock::Lock(void)
{
#if (defined(HAVE_PTHREAD) || defined(HAVE_WINDOWS_H))
    if (debugOptions & DEBUG_CONTENTION)
    {
        // Report a heavily contended lock.
        if (Trylock())
            return;
        if (++lockCount > LOCK_REPORT_COUNT)
        {
            if (lockName != 0)
                Log("Lock: contention on lock: %s\n", lockName);
            else
                Log("Lock: contention on lock at %p\n", &lock);
            lockCount = 0;
        }
        // Drop through to a normal lock
    }
#endif
#ifdef HAVE_PTHREAD
    pthread_mutex_lock(&lock);
#elif defined(HAVE_WINDOWS_H)
    EnterCriticalSection(&lock);
#endif
    // If we don't support threads this does nothing.
}

void PLock::Unlock(void)
{
#ifdef HAVE_PTHREAD
    pthread_mutex_unlock(&lock);
#elif defined(HAVE_WINDOWS_H)
    LeaveCriticalSection(&lock);
#endif
}

bool PLock::Trylock(void)
{
#ifdef HAVE_PTHREAD
    // Since we use normal mutexes this returns EBUSY if the
    // current thread owns the mutex.
    return pthread_mutex_trylock(&lock) != EBUSY;
#elif defined(HAVE_WINDOWS_H)
    // This is not implemented properly in Windows.  There is
    // TryEnterCriticalSection in Win NT and later but that
    // returns TRUE if the current thread owns the mutex.
   return TryEnterCriticalSection(&lock) == TRUE;
#else
   return true; // Single-threaded.
#endif
}

PCondVar::PCondVar()
{
#ifdef HAVE_PTHREAD
    pthread_cond_init(&cond, NULL);
#elif defined(HAVE_WINDOWS_H)
    // Create a manually set event initially set.
    cond = CreateEvent(NULL, TRUE, TRUE, NULL);
#endif
}

PCondVar::~PCondVar()
{
#ifdef HAVE_PTHREAD
    pthread_cond_destroy(&cond);
#elif defined(HAVE_WINDOWS_H)
    CloseHandle(cond);
#endif
}

// Wait indefinitely.  Drops the lock and reaquires it.
void PCondVar::Wait(PLock *pLock)
{
#ifdef HAVE_PTHREAD
    pthread_cond_wait(&cond, &pLock->lock);
#elif defined(HAVE_WINDOWS_H)
    // This code will NOT work as a general implementation of a
    // condition variable.  It works provided we use it carefully.
    // We only use this in the situation where all the threads
    // that can wait on this condvar have waited before we then
    // release all of them. 
    ResetEvent(cond); // Do this with the lock held.
    // Can now release the lock.  It doesn't matter that releasing
    // the lock and waiting isn't atomic because the event is manually
    // reset.  If another thread sets the event before we call WFSO we'll
    // simply return immediately.
    LeaveCriticalSection(&pLock->lock);
    WaitForSingleObject(cond, INFINITE);
    EnterCriticalSection(&pLock->lock);
#endif
}

// Wait until a specified absolute time.  Drops the lock and reaquires it.
#if (defined(_WIN32) && ! defined(__CYGWIN__))
// Windows with Windows-style times
void PCondVar::WaitUntil(PLock *pLock, const FILETIME *time)
{
    FILETIME now;
    GetSystemTimeAsFileTime(&now);
    LARGE_INTEGER liNow, liTime;
    liNow.HighPart = now.dwHighDateTime;
    liNow.LowPart = now.dwLowDateTime;
    liTime.HighPart = time->dwHighDateTime;
    liTime.LowPart = time->dwLowDateTime;
    if (liNow.QuadPart >= liTime.QuadPart) // Already past the time
        return;
    DWORD toWait = (DWORD)((liTime.QuadPart - liNow.QuadPart) / (LONGLONG)10000);
    (void)WaitFor(pLock, toWait);
}
#else
// Unix-style times
void PCondVar::WaitUntil(PLock *pLock, const timespec *time)
{
#ifdef HAVE_PTHREAD
    pthread_cond_timedwait(&cond, &pLock->lock, time);
#elif defined(HAVE_WINDOWS_H)
    // This must be Cygwin but compiled with --without-threads
    struct timeval tv;
    if (gettimeofday(&tv, NULL) != 0)
        return;
    if (tv.tv_sec > time->tv_sec || (tv.tv_sec == time->tv_sec && tv.tv_usec >= time->tv_nsec/1000))
        return; // Already past the time
    WaitFor(pLock, (time->tv_sec - tv.tv_sec) * 1000 + time->tv_nsec/1000000 - tv.tv_usec/1000);
#endif
}
#endif

// Wait for a number of milliseconds.  Used within the RTS.  Drops the lock and reaquires it.
bool PCondVar::WaitFor(PLock *pLock, unsigned milliseconds)
{
#ifdef HAVE_PTHREAD
    struct timespec waitTime;
    struct timeval tv;
    if (gettimeofday(&tv, NULL) != 0)
        return false;
    waitTime.tv_sec = tv.tv_sec + milliseconds / 1000;
    waitTime.tv_nsec = (tv.tv_usec + (milliseconds % 1000) * 1000) * 1000;
    if (waitTime.tv_nsec >= 1000*1000*1000)
    {
        waitTime.tv_nsec -= 1000*1000*1000;
        waitTime.tv_sec += 1;
    }
    return pthread_cond_timedwait(&cond, &pLock->lock, &waitTime) == 0;
#elif defined(HAVE_WINDOWS_H)
    ResetEvent(cond); // Do this with the lock held.
    LeaveCriticalSection(&pLock->lock);
    DWORD dwResult = WaitForSingleObject(cond, milliseconds);
    EnterCriticalSection(&pLock->lock);
    return dwResult == WAIT_OBJECT_0;
#else
    return true; // Single-threaded.  Return immediately.
#endif
}

// Wake up all the waiting threads. 
void PCondVar::Signal(void)
{
#ifdef HAVE_PTHREAD
    pthread_cond_broadcast(&cond);
#elif defined(HAVE_WINDOWS_H)
    // N.B.  This assumes that we have the same lock that is used
    // in Wait and WaitFor otherwise we set the event before the reset.
    SetEvent(cond);
#endif
}


// Initialise a semphore.  Tries to create an unnamed semaphore if
// it can but tries a named semaphore if it can't.  Mac OS X only
// supports named semaphores.
// The semaphore is initialised with a count of zero.
PSemaphore::PSemaphore()
{
#if ((!defined(_WIN32) || defined(__CYGWIN__)) && defined(HAVE_SEMAPHORE_H))
    sema = 0;
    isLocal = true;
#elif defined(HAVE_WINDOWS_H)
    sema = NULL;
#endif
}

PSemaphore::~PSemaphore()
{
#if ((!defined(_WIN32) || defined(__CYGWIN__)) && defined(HAVE_SEMAPHORE_H))
    if (sema && isLocal) sem_destroy(sema);
    else if (sema && !isLocal) sem_close(sema);
#elif defined(HAVE_WINDOWS_H)
    if (sema != NULL) CloseHandle(sema);
#endif
}

bool PSemaphore::Init(unsigned init, unsigned max)
{
#if ((!defined(_WIN32) || defined(__CYGWIN__)) && defined(HAVE_SEMAPHORE_H))
    isLocal = true;
    if (sem_init(&localSema, 0, init) == 0) {
        sema = &localSema;
        return true;
    }
#if (defined(__CYGWIN__))
    // Cygwin doesn't define sem_unlink but that doesn't matter
    // since sem_init works.
    sema = 0;
    return false;
#else
    isLocal = false;
    char semname[30];
    static int count=0;
    sprintf(semname, "poly%0d-%0d", (int)getpid(), count++);
    sema = sem_open(semname, O_CREAT|O_EXCL, 00666, init);
    if (sema == (sem_t*)SEM_FAILED) {
        sema = 0;
        return false;
    }
    sem_unlink(semname);
    return true;
#endif
#elif defined(HAVE_WINDOWS_H)
    sema = CreateSemaphore(NULL, init, max, NULL);
    return sema != NULL;
#endif
}

bool PSemaphore::Wait(void)
{
#if ((!defined(_WIN32) || defined(__CYGWIN__)) && defined(HAVE_SEMAPHORE_H))
    // Wait until the semaphore is signalled.  A Unix signal may interrupt
    // it so we need to retry in that case.
    while (sem_wait(sema) == -1)
    {
        if (errno != EINTR)
            return false;
    }
    return true;
#elif defined(HAVE_WINDOWS_H)
    return WaitForSingleObject(sema, INFINITE) == WAIT_OBJECT_0;
#endif
}

void PSemaphore::Signal(void)
{
#if ((!defined(_WIN32) || defined(__CYGWIN__)) && defined(HAVE_SEMAPHORE_H))
    sem_post(sema);
#elif defined(HAVE_WINDOWS_H)
    ReleaseSemaphore(sema, 1, NULL);
#endif
}



