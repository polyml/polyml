/*
    Title:      Mutex and Condition Variable library.

    Copyright (c) 2007 David C. J. Matthews

    Portions of this code are derived from the original stream io
    package copyright CUTS 1983-2000.

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
#elif defined(WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#if ((!defined(WIN32) || defined(__CYGWIN__)) && defined(HAVE_PTHREAD_H))
#define HAVE_PTHREAD 1
#include <pthread.h>
#elif (defined(HAVE_WINDOWS_H))
// We need the next define since TryEnterCriticalSection is only
// defined in Win NT.  This could mean that this code won't run under
// Windows 95 or 98.
#define _WIN32_WINNT 0x0400
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

#include "locking.h"

PLock::PLock()
{
#ifdef HAVE_PTHREAD
    pthread_mutex_init(&lock, 0);
#elif defined(HAVE_WINDOWS_H)
    InitializeCriticalSection(&lock);
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
void PCondVar::WaitUntil(PLock *pLock, const void *timeArg)
{
#ifdef HAVE_PTHREAD
    pthread_cond_timedwait(&cond, &pLock->lock, (const struct timespec *)timeArg);
#elif defined(HAVE_WINDOWS_H)
    FILETIME now;
    FILETIME *time = (FILETIME *)timeArg;
    GetSystemTimeAsFileTime(&now);
    LARGE_INTEGER liNow, liTime;
    liNow.HighPart = now.dwHighDateTime;
    liNow.LowPart = now.dwLowDateTime;
    liTime.HighPart = time->dwHighDateTime;
    liTime.LowPart = time->dwLowDateTime;
    if (liNow.QuadPart >= liTime.QuadPart) // Already past the time
        return;
    DWORD toWait = (DWORD)((liTime.QuadPart - liNow.QuadPart) / (LONGLONG)10000);
    ResetEvent(cond); // Do this with the lock held.
    LeaveCriticalSection(&pLock->lock);
    WaitForSingleObject(cond, toWait);
    EnterCriticalSection(&pLock->lock);
#endif
}

// Wait for a number of milliseconds.  Used within the RTS.  Drops the lock and reaquires it.
bool PCondVar::WaitFor(PLock *pLock, unsigned milliseconds)
{
#ifdef HAVE_PTHREAD
    struct timespec waitTime;
    struct timeval tv;
    if (gettimeofday(&tv, NULL) != 0)
        return false;
    waitTime.tv_sec = tv.tv_sec + milliseconds / 1000;
    waitTime.tv_nsec = tv.tv_usec * 1000 + milliseconds % 1000;
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
    SetEvent(cond);
#endif
}


