/*
    Title:  sighandler.h

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

#ifndef _SIGHANDLER_H
#define _SIGHANDLER_H 1

class SaveVecEntry;
typedef SaveVecEntry *Handle;

extern Handle Sig_dispatch_c(Handle handler, Handle sig);

extern void addSigCount(int sig);
extern void markSignalInuse(int sig);

#ifdef WINDOWS_PC
extern void handleINT(void);
extern void RequestConsoleInterrupt(void);
extern void SetResetCC(const int enable);
#else

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

extern void init_asyncmask(sigset_t *mask);

#define SIG_HANDLER_ARGS(_sig,_contxt)    int _sig, siginfo_t *, void *_contxt

typedef void (*signal_handler_type)(SIG_HANDLER_ARGS(s, c));

extern bool setSignalHandler(int sig, signal_handler_type func);

#endif /* ! WIN32 */

#endif
