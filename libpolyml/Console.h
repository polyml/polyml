/*
    Title:      Poly/ML Console Window.

    Copyright (c) 2000 David C. J. Matthews

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

#ifndef _CONSOLE_H
#define _CONSOLE_H

#include <windows.h>

/* Test whether input is available. */
extern bool isConsoleInput(void);
/* Read characters from the input.  Only returns zero on EOF. */
extern unsigned getConsoleInput(char *buff, int nChars);

// Create a copying thread that can also signal an event.
// This is used both in stdin and also in Windows.execute.
extern HANDLE CreateCopyPipe(HANDLE hInput, HANDLE hEvent);

extern HWND hMainWindow; /* Handle to main window - NULL if none. */
extern bool useConsole; /* non-zero if we should use the console for input. */
extern HINSTANCE hApplicationInstance; /* Application instance */

/* DDE requests. */
extern HCONV StartDDEConversation(char *serviceName, char *topicName);
extern void CloseDDEConversation(HCONV hConv);
extern LRESULT ExecuteDDE(char *command, HCONV hConv);

extern HANDLE hInputEvent; // Handle to console input event

#endif
