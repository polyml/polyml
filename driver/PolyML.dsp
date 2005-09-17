# Microsoft Developer Studio Project File - Name="PolyML" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=PolyML - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "PolyML.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "PolyML.mak" CFG="PolyML - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "PolyML - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "PolyML - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath "H/PC Ver. 2.00"
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "PolyML - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "WINDOWS_PC" /D "i386" /YX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib wsock32.lib gdi32.lib comdlg32.lib advapi32.lib shell32.lib /nologo /subsystem:windows /map /machine:I386
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "PolyML - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "WINDOWS_PC" /D "i386" /YX /FD /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib wsock32.lib gdi32.lib comdlg32.lib advapi32.lib shell32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "PolyML - Win32 Release"
# Name "PolyML - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "c;cpp;rc"
# Begin Source File

SOURCE=.\alloc.c
# End Source File
# Begin Source File

SOURCE=.\arb.c
# End Source File
# Begin Source File

SOURCE=.\basicio.c
# End Source File
# Begin Source File

SOURCE=.\Console.c
# End Source File
# Begin Source File

SOURCE=.\copygc.c
# End Source File
# Begin Source File

SOURCE=.\cwd.c
# End Source File
# Begin Source File

SOURCE=.\diagnostics.c
# End Source File
# Begin Source File

SOURCE=.\discgc.c
# End Source File
# Begin Source File

SOURCE=.\foreign.c
# End Source File
# Begin Source File

SOURCE=.\gc.c
# End Source File
# Begin Source File

SOURCE=.\i386.asm

!IF  "$(CFG)" == "PolyML - Win32 Release"

# Begin Custom Build - Assembling...
IntDir=.\Release
InputPath=.\i386.asm
InputName=i386

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	ml /nologo /DWINDOWS /Fo $(IntDir)\$(InputName).obj /c /coff  $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "PolyML - Win32 Debug"

# Begin Custom Build - Assembling...
IntDir=.\Debug
InputPath=.\i386.asm
InputName=i386

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	ml /nologo /DWINDOWS /Fo $(IntDir)\$(InputName).obj /c /coff  $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\i386_dep.c
# End Source File
# Begin Source File

SOURCE=.\improve.c
# End Source File
# Begin Source File

SOURCE=.\mmap.c
# End Source File
# Begin Source File

SOURCE=.\mpoly.c
# End Source File
# Begin Source File

SOURCE=.\network.c
# End Source File
# Begin Source File

SOURCE=.\noxwindows.c
# End Source File
# Begin Source File

SOURCE=.\objsize.c
# End Source File
# Begin Source File

SOURCE=.\PolyML.rc
# End Source File
# Begin Source File

SOURCE=.\process_env.c
# End Source File
# Begin Source File

SOURCE=.\processes.c
# End Source File
# Begin Source File

SOURCE=.\profiling.c
# End Source File
# Begin Source File

SOURCE=.\proper_io.c
# End Source File
# Begin Source File

SOURCE=.\realconv.c
# End Source File
# Begin Source File

SOURCE=.\reals.c
# End Source File
# Begin Source File

SOURCE=.\run_time.c
# End Source File
# Begin Source File

SOURCE=.\sighandler.c
# End Source File
# Begin Source File

SOURCE=.\timing.c
# End Source File
# Begin Source File

SOURCE=.\version.c
# End Source File
# Begin Source File

SOURCE=.\windows_specific.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h"
# Begin Source File

SOURCE=.\addresses.h
# End Source File
# Begin Source File

SOURCE=.\alloc.h
# End Source File
# Begin Source File

SOURCE=.\arb.h
# End Source File
# Begin Source File

SOURCE=.\basicio.h
# End Source File
# Begin Source File

SOURCE=.\Console.h
# End Source File
# Begin Source File

SOURCE=.\copygc.h
# End Source File
# Begin Source File

SOURCE=.\cwd.h
# End Source File
# Begin Source File

SOURCE=.\diagnostics.h
# End Source File
# Begin Source File

SOURCE=.\discgc.h
# End Source File
# Begin Source File

SOURCE=.\errors.h
# End Source File
# Begin Source File

SOURCE=.\foreign.h
# End Source File
# Begin Source File

SOURCE=.\gc.h
# End Source File
# Begin Source File

SOURCE=.\globals.h
# End Source File
# Begin Source File

SOURCE=.\improve.h
# End Source File
# Begin Source File

SOURCE=.\io_internal.h
# End Source File
# Begin Source File

SOURCE=.\machine_assembly.h
# End Source File
# Begin Source File

SOURCE=.\machine_dep.h
# End Source File
# Begin Source File

SOURCE=.\memory.h
# End Source File
# Begin Source File

SOURCE=.\mm.h
# End Source File
# Begin Source File

SOURCE=.\mmap.h
# End Source File
# Begin Source File

SOURCE=.\mpoly.h
# End Source File
# Begin Source File

SOURCE=.\network.h
# End Source File
# Begin Source File

SOURCE=.\objects.h
# End Source File
# Begin Source File

SOURCE=.\objsize.h
# End Source File
# Begin Source File

SOURCE=.\os_specific.h
# End Source File
# Begin Source File

SOURCE=.\PolyControl.h
# End Source File
# Begin Source File

SOURCE=.\process_env.h
# End Source File
# Begin Source File

SOURCE=.\processes.h
# End Source File
# Begin Source File

SOURCE=.\profiling.h
# End Source File
# Begin Source File

SOURCE=.\proper_io.h
# End Source File
# Begin Source File

SOURCE=.\realconv.h
# End Source File
# Begin Source File

SOURCE=.\reals.h
# End Source File
# Begin Source File

SOURCE=.\resource.h
# End Source File
# Begin Source File

SOURCE=.\run_time.h
# End Source File
# Begin Source File

SOURCE=.\sighandler.h
# End Source File
# Begin Source File

SOURCE=.\sys.h
# End Source File
# Begin Source File

SOURCE=.\timing.h
# End Source File
# Begin Source File

SOURCE=.\version.h
# End Source File
# Begin Source File

SOURCE=.\xwindows.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico"
# Begin Source File

SOURCE=.\poly.ico
# End Source File
# End Group
# Begin Source File

SOURCE=.\COPYING
# End Source File
# End Target
# End Project
