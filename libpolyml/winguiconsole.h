/*
    Title:      Poly/ML Console Window.

    Copyright (c) 2019 David C. J. Matthews

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
class WinStream;

extern HWND hMainWindow; /* Handle to main window - NULL if none. */
// Create the console window and return the handle to stream used to
// write to it.
HANDLE createConsoleWindow(int nCmdShow);
// Create a stream that can read from the console.
extern WinStream *createConsoleStream();
#endif
