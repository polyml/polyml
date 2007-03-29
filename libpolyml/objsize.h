/*
    Title:  objsize.h - export signature for objsize.c

    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

#ifndef OBJSIZE_H_DEFINED
#define OBJSIZE_H_DEFINED

class SaveVecEntry;
typedef SaveVecEntry *Handle;
class TaskData;

extern Handle ObjSize(TaskData *taskData, Handle obj);
extern Handle ShowSize(TaskData *taskData, Handle obj);
extern Handle ObjProfile(TaskData *taskData, Handle obj);
#endif
