/*
    Title:      Poly/ML Console Window.

    Copyright (c) 2000, 2015, 2018 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/

#ifndef _WINGUICONSOLE_H
#define _WINGUICONSOLE_H

#include <windows.h>

extern bool useConsole;

// Create a copying thread that can also signal an event.
// This is used both in stdin and also in Windows.execute.
extern HANDLE CreateCopyPipe(HANDLE hInput, HANDLE hEvent);

extern HWND hMainWindow; /* Handle to main window - NULL if none. */

extern HINSTANCE hApplicationInstance; /* Application instance */

/* DDE requests. */
extern HCONV StartDDEConversation(TCHAR *serviceName, TCHAR *topicName);
extern void CloseDDEConversation(HCONV hConv);
extern LRESULT ExecuteDDE(char *command, HCONV hConv);

extern HANDLE hInputEvent; // Handle to console input event

extern void SetupDDEHandler(const TCHAR *lpszName);

class WinStream;
// The stream object is created by the Console package.
extern WinStream *stdInStream;

#endif
