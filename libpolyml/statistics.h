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
#include "locking.h"
#include "../polystatistics.h"

class Statistics  
{
public:
    Statistics();
    ~Statistics();

    bool getLocalsStatistics(struct polystatistics *statCopy);
    bool getRemoteStatistics(POLYUNSIGNED processId, struct polystatistics *statCopy);

    void incCount(int which);
    void decCount(int which);

    void setSize(int which, size_t s);
    void incSize(int which, size_t s);
    void decSize(int which, size_t s);
    size_t getSize(int which);

    void setUserCounter(unsigned which, int value);

#if (defined(_WIN32) && ! defined(__CYGWIN__))
    // Native Windows
    void copyGCTimes(const FILETIME &gcUtime, const FILETIME &gcStime);
#else
    // Unix and Cygwin
    void copyGCTimes(const struct timeval &gcUtime, const struct timeval &gcStime);
#endif
    
    void updatePeriodicStats(POLYUNSIGNED freeSpace, unsigned threadsInML);

private:
    PLock accessLock;
#ifdef HAVE_WINDOWS_H
    // File mapping handle
    HANDLE hFileMap;
#else
    char *mapFileName;
    int mapFd;
    size_t memSize;
#endif
    struct polystatistics *statMemory;
};

extern Statistics globalStats;

#endif // STATISTICS_INCLUDED
