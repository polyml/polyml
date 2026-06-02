#pragma once

#ifndef MODULES_H_INCLUDED
#define MODULES_H_INCLUDED

extern struct _entrypts modulesEPT[];

// Functions exported to savestate.cpp
class TaskData;
class SaveVecEntry;
typedef SaveVecEntry* Handle;

Handle moduleIdAsByteVector(TaskData* taskData, struct _moduleId modId);

// When a new saved state is loaded all modules are freed.
bool freeAllModules();

#endif
