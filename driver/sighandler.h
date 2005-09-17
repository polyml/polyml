/*
    Title: 	sighandler.h

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

extern Handle Sig_dispatch_c(Handle handler, Handle sig);

extern void addSigCount(int sig);
extern void markSignalInuse(int sig);

extern void uninit_sig_handler(void);
extern void init_sig_handler(void);
extern void re_init_sig_handler(void);

#ifdef WINDOWS_PC
extern void handleINT(void);
extern void RequestConsoleInterrupt(void);
extern void SetResetCC(const int enable);
#else
#include <signal.h>
extern void init_asyncmask(sigset_t *mask);
typedef void (*signal_handler_type)(int);
extern int setSignalHandler(int sig, signal_handler_type func);
#endif /* ! WINDOWS_PC */

#endif
