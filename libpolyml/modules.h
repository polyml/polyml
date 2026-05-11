#pragma once

#ifndef MODULES_H_INCLUDED
#define MODULES_H_INCLUDED

extern struct _entrypts modulesEPT[];

class TaskData;
class SaveVecEntry;
typedef SaveVecEntry* Handle;

Handle moduleIdAsByteVector(TaskData* taskData, struct _moduleId modId);

#endif
