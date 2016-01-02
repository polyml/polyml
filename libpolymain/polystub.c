/*
    Title:  polystub.c 

    Copyright (c) 2006, 2015 David C.J. Matthews

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

// This is the start-up function for Poly/ML. It simply picks up the
// pointer to the exported data and calls the main program.

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#include "../polyexports.h"

#if (defined(_WIN32) && ! defined(__CYGWIN__))
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
    return PolyWinMain(hInstance, hPrevInstance, lpCmdLine, nCmdShow, &poly_exports);
}

#else
int main(int argc, char *argv[])
{
    return polymain(argc, argv, &poly_exports);
}

#endif

