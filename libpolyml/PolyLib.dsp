# Microsoft Developer Studio Project File - Name="PolyLib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=PolyLib - Win32 IntDebug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "PolyLib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "PolyLib.mak" CFG="PolyLib - Win32 IntDebug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "PolyLib - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "PolyLib - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "PolyLib - Win32 IntDebug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "PolyLib - Win32 IntRelease" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "PolyLib - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "PolyLib___Win32_Release"
# PROP BASE Intermediate_Dir "PolyLib___Win32_Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "POLYLIB_EXPORTS" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I ".." /I "..\libffi\include" /I "..\libffi\msvc32include" /I "..\libffi\src\x86" /D "HOSTARCHITECTURE_X86" /D "_MBCS" /D "_USRDLL" /D "POLYLIB_EXPORTS" /D LONG_LONG_MAX=_I64_MAX /FD /c
# SUBTRACT CPP /YX /Yc /Yu
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 user32.lib wsock32.lib gdi32.lib advapi32.lib shell32.lib /nologo /dll /map /machine:I386

!ELSEIF  "$(CFG)" == "PolyLib - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "PolyLib___Win32_Debug"
# PROP BASE Intermediate_Dir "PolyLib___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "POLYLIB_EXPORTS" /Yu"stdafx.h" /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I ".." /I "..\libffi\include" /I "..\libffi\msvc32include" /I "..\libffi\src\x86" /D "HOSTARCHITECTURE_X86" /D "_MBCS" /D "_USRDLL" /D "POLYLIB_EXPORTS" /D LONG_LONG_MAX=_I64_MAX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 user32.lib wsock32.lib gdi32.lib advapi32.lib shell32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept

!ELSEIF  "$(CFG)" == "PolyLib - Win32 IntDebug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "PolyLib___Win32_IntDebug"
# PROP BASE Intermediate_Dir "PolyLib___Win32_IntDebug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\IntDebug"
# PROP Intermediate_Dir "IntDebug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I ".." /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "POLYLIB_EXPORTS" /D "WINDOWS_PC" /D "i386" /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I ".." /I "..\libffi\include" /I "..\libffi\msvc32include" /I "..\libffi\src\x86" /D "INTERPRETED" /D "_MBCS" /D "_USRDLL" /D "POLYLIB_EXPORTS" /D LONG_LONG_MAX=_I64_MAX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib wsock32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib wsock32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept

!ELSEIF  "$(CFG)" == "PolyLib - Win32 IntRelease"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "PolyLib___Win32_IntRelease"
# PROP BASE Intermediate_Dir "PolyLib___Win32_IntRelease"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\IntRelease"
# PROP Intermediate_Dir "IntRelease"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /I ".." /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "POLYLIB_EXPORTS" /D "WINDOWS_PC" /D "i386" /FD /c
# SUBTRACT BASE CPP /YX /Yc /Yu
# ADD CPP /nologo /MT /W3 /GX /O2 /I ".." /I "..\libffi\include" /I "..\libffi\msvc32include" /I "..\libffi\src\x86" /D "INTERPRTED" /D "_MBCS" /D "_USRDLL" /D "POLYLIB_EXPORTS" /D LONG_LONG_MAX=_I64_MAX /FD /c
# SUBTRACT CPP /YX /Yc /Yu
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib wsock32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib wsock32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386

!ENDIF 

# Begin Target

# Name "PolyLib - Win32 Release"
# Name "PolyLib - Win32 Debug"
# Name "PolyLib - Win32 IntDebug"
# Name "PolyLib - Win32 IntRelease"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\arb.cpp
# End Source File
# Begin Source File

SOURCE=.\basicio.cpp
# End Source File
# Begin Source File

SOURCE=.\bitmap.cpp
# End Source File
# Begin Source File

SOURCE=.\check_objects.cpp
# End Source File
# Begin Source File

SOURCE=.\Console.cpp
# End Source File
# Begin Source File

SOURCE=.\diagnostics.cpp
# End Source File
# Begin Source File

SOURCE=.\exporter.cpp
# End Source File
# Begin Source File

SOURCE=.\foreign.cpp
# End Source File
# Begin Source File

SOURCE=.\gc.cpp
# End Source File
# Begin Source File

SOURCE=.\gc_check_weak_ref.cpp
# End Source File
# Begin Source File

SOURCE=.\gc_copy_phase.cpp
# End Source File
# Begin Source File

SOURCE=.\gc_mark_phase.cpp
# End Source File
# Begin Source File

SOURCE=.\gc_share_phase.cpp
# End Source File
# Begin Source File

SOURCE=.\gc_update_phase.cpp
# End Source File
# Begin Source File

SOURCE=.\gctaskfarm.cpp
# End Source File
# Begin Source File

SOURCE=.\interpret.cpp

!IF  "$(CFG)" == "PolyLib - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "PolyLib - Win32 Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "PolyLib - Win32 IntDebug"

!ELSEIF  "$(CFG)" == "PolyLib - Win32 IntRelease"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\locking.cpp
# End Source File
# Begin Source File

SOURCE=.\memmgr.cpp
# End Source File
# Begin Source File

SOURCE=.\mpoly.cpp
# End Source File
# Begin Source File

SOURCE=.\network.cpp
# End Source File
# Begin Source File

SOURCE=.\objsize.cpp
# End Source File
# Begin Source File

SOURCE=.\osmem.cpp
# End Source File
# Begin Source File

SOURCE=.\pecoffexport.cpp
# End Source File
# Begin Source File

SOURCE=.\pexport.cpp
# End Source File
# Begin Source File

SOURCE=.\poly_specific.cpp
# End Source File
# Begin Source File

SOURCE=.\polystring.cpp
# End Source File
# Begin Source File

SOURCE=.\process_env.cpp
# End Source File
# Begin Source File

SOURCE=.\processes.cpp
# End Source File
# Begin Source File

SOURCE=.\profiling.cpp
# End Source File
# Begin Source File

SOURCE=.\quick_gc.cpp
# End Source File
# Begin Source File

SOURCE=.\realconv.cpp
# End Source File
# Begin Source File

SOURCE=.\reals.cpp
# End Source File
# Begin Source File

SOURCE=.\rts_module.cpp
# End Source File
# Begin Source File

SOURCE=.\run_time.cpp
# End Source File
# Begin Source File

SOURCE=.\save_vec.cpp
# End Source File
# Begin Source File

SOURCE=.\savestate.cpp
# End Source File
# Begin Source File

SOURCE=.\scanaddrs.cpp
# End Source File
# Begin Source File

SOURCE=.\sharedata.cpp
# End Source File
# Begin Source File

SOURCE=.\sighandler.cpp
# End Source File
# Begin Source File

SOURCE=.\statistics.cpp
# End Source File
# Begin Source File

SOURCE=.\timing.cpp
# End Source File
# Begin Source File

SOURCE=.\windows_specific.cpp
# End Source File
# Begin Source File

SOURCE=.\x86_dep.cpp

!IF  "$(CFG)" == "PolyLib - Win32 Release"

!ELSEIF  "$(CFG)" == "PolyLib - Win32 Debug"

!ELSEIF  "$(CFG)" == "PolyLib - Win32 IntDebug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "PolyLib - Win32 IntRelease"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\x86asm.asm

!IF  "$(CFG)" == "PolyLib - Win32 Release"

# Begin Custom Build
IntDir=.\Release
InputPath=.\x86asm.asm
InputName=x86asm

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	ml /nologo /DWINDOWS /Fo $(IntDir)\$(InputName).obj /c /coff $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "PolyLib - Win32 Debug"

# Begin Custom Build
IntDir=.\Debug
InputPath=.\x86asm.asm
InputName=x86asm

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	ml /nologo /DWINDOWS /Fo $(IntDir)\$(InputName).obj /c /coff $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "PolyLib - Win32 IntDebug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "PolyLib - Win32 IntRelease"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\xwindows.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\arb.h
# End Source File
# Begin Source File

SOURCE=.\basicio.h
# End Source File
# Begin Source File

SOURCE=.\bitmap.h
# End Source File
# Begin Source File

SOURCE=.\check_objects.h
# End Source File
# Begin Source File

SOURCE=.\Console.h
# End Source File
# Begin Source File

SOURCE=.\cwd.h
# End Source File
# Begin Source File

SOURCE=.\diagnostics.h
# End Source File
# Begin Source File

SOURCE=.\errors.h
# End Source File
# Begin Source File

SOURCE=.\exporter.h
# End Source File
# Begin Source File

SOURCE=.\foreign.h
# End Source File
# Begin Source File

SOURCE=.\gc.h
# End Source File
# Begin Source File

SOURCE=.\gctaskfarm.h
# End Source File
# Begin Source File

SOURCE=.\globals.h
# End Source File
# Begin Source File

SOURCE=.\int_opcodes.h
# End Source File
# Begin Source File

SOURCE=.\io_internal.h
# End Source File
# Begin Source File

SOURCE=.\locking.h
# End Source File
# Begin Source File

SOURCE=.\machine_dep.h
# End Source File
# Begin Source File

SOURCE=.\memmgr.h
# End Source File
# Begin Source File

SOURCE=.\mpoly.h
# End Source File
# Begin Source File

SOURCE=.\network.h
# End Source File
# Begin Source File

SOURCE=.\noreturn.h
# End Source File
# Begin Source File

SOURCE=.\objsize.h
# End Source File
# Begin Source File

SOURCE=.\os_specific.h
# End Source File
# Begin Source File

SOURCE=.\osmem.h
# End Source File
# Begin Source File

SOURCE=.\pecoffexport.h
# End Source File
# Begin Source File

SOURCE=.\pexport.h
# End Source File
# Begin Source File

SOURCE=.\poly_specific.h
# End Source File
# Begin Source File

SOURCE=.\PolyControl.h
# End Source File
# Begin Source File

SOURCE=..\polyexports.h
# End Source File
# Begin Source File

SOURCE=..\polystatistics.h
# End Source File
# Begin Source File

SOURCE=.\polystring.h
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

SOURCE=.\realconv.h
# End Source File
# Begin Source File

SOURCE=.\reals.h
# End Source File
# Begin Source File

SOURCE=.\resource.h
# End Source File
# Begin Source File

SOURCE=.\rts_module.h
# End Source File
# Begin Source File

SOURCE=.\run_time.h
# End Source File
# Begin Source File

SOURCE=.\save_vec.h
# End Source File
# Begin Source File

SOURCE=.\savestate.h
# End Source File
# Begin Source File

SOURCE=.\scanaddrs.h
# End Source File
# Begin Source File

SOURCE=.\sharedata.h
# End Source File
# Begin Source File

SOURCE=.\sighandler.h
# End Source File
# Begin Source File

SOURCE=.\statistics.h
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

SOURCE=..\winconfig.h
# End Source File
# Begin Source File

SOURCE=.\xwindows.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Source File

SOURCE=.\COPYING
# End Source File
# End Target
# End Project
