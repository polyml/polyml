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

#ifdef WIN32
#include "winconfig.h"
#else
#include "config.h"
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

#if (defined(HAVE_LIBPTHREAD) && defined(HAVE_PTHREAD_H))
#define HAVE_PTHREAD 1
#include <pthread.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
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
    return false;
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

// Wake up all the waiting threads. 
void PCondVar::Signal(void)
{
#ifdef HAVE_PTHREAD
    pthread_cond_broadcast(&cond);
#elif defined(HAVE_WINDOWS_H)
    SetEvent(cond);
#endif
}


