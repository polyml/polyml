/*
    Title:  statics.h - Interface to profiling statistics

    Copyright (c) 2011 David C.J. Matthews

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

#ifndef STATISTICS_INCLUDED
#define STATISTICS_INCLUDED

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

#include "globals.h"

class Statistics  
{
public:
	Statistics();
    ~Statistics();

    bool getLocalsStatistics(struct polystatistics *statCopy);
    bool getRemoteStatistics(POLYUNSIGNED processId, struct polystatistics *statCopy); 

private:
#ifdef HAVE_WINDOWS_H
    // File mapping handle
    HANDLE hFileMap;
#else
    char mapFileName[40];
    int mapFd;
    size_t memSize;
#endif
    struct polystatistics *statMemory;
};

extern Statistics *gStats;

#endif // STATISTICS_INCLUDED
