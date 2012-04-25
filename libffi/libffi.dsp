# Microsoft Developer Studio Project File - Name="libffi" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=libffi - Win32 IntDebug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libffi.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libffi.mak" CFG="libffi - Win32 IntDebug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libffi - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libffi - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "libffi - Win32 IntDebug" (based on "Win32 (x86) Static Library")
!MESSAGE "libffi - Win32 IntRelease" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libffi - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "msvc32include" /I "src\x86" /I "include" /I "." /D "_MBCS" /D "_LIB" /D LONG_LONG_MAX=_I64_MAX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "libffi - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "msvc32include" /I "src\x86" /I "include" /I "." /D "_MBCS" /D "_LIB" /D LONG_LONG_MAX=_I64_MAX /FD /GZ /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "libffi - Win32 IntDebug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "IntDebug"
# PROP BASE Intermediate_Dir "IntDebug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "IntDebug"
# PROP Intermediate_Dir "IntDebug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "include" /I "." /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D LONG_LONG_MAX=_I64_MAX /FD /GZ /c
# SUBTRACT BASE CPP /YX
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "msvc32include" /I "src\x86" /I "include" /I "." /D "_MBCS" /D "_LIB" /D LONG_LONG_MAX=_I64_MAX /FD /GZ /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "libffi - Win32 IntRelease"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "IntRelease"
# PROP BASE Intermediate_Dir "IntRelease"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "IntRelease"
# PROP Intermediate_Dir "IntRelease"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /I "include" /I "." /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D LONG_LONG_MAX=_I64_MAX /FD /c
# SUBTRACT BASE CPP /YX
# ADD CPP /nologo /MT /W3 /GX /O2 /I "msvc32include" /I "src\x86" /I "include" /I "." /D "_MBCS" /D "_LIB" /D LONG_LONG_MAX=_I64_MAX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "libffi - Win32 Release"
# Name "libffi - Win32 Debug"
# Name "libffi - Win32 IntDebug"
# Name "libffi - Win32 IntRelease"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\src\closures.c
# End Source File
# Begin Source File

SOURCE=.\src\debug.c
# End Source File
# Begin Source File

SOURCE=.\src\x86\ffi.c
# End Source File
# Begin Source File

SOURCE=.\src\java_raw_api.c
# End Source File
# Begin Source File

SOURCE=.\src\prep_cif.c
# End Source File
# Begin Source File

SOURCE=.\src\raw_api.c
# End Source File
# Begin Source File

SOURCE=.\src\types.c
# End Source File
# Begin Source File

SOURCE=.\src\x86\win32.S

!IF  "$(CFG)" == "libffi - Win32 Release"

# Begin Custom Build
IntDir=.\Release
InputPath=.\src\x86\win32.S
InputName=win32

BuildCmds= \
	cl /nologo /EP /Imsvc32include /Isrc\x86 /I. /Iinclude  /D_MSC_VER $(InputPath) > $(IntDir)\$(InputName).asm \
	ml /nologo /Fo $(IntDir)\$(InputName).obj /c /coff $(IntDir)\$(InputName).asm \
	

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"$(IntDir)\$(InputName).asm" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "libffi - Win32 Debug"

# Begin Custom Build
IntDir=.\Debug
InputPath=.\src\x86\win32.S
InputName=win32

BuildCmds= \
	cl /nologo /EP /Imsvc32include /Isrc\x86 /I. /Iinclude  /D_MSC_VER $(InputPath) > $(IntDir)\$(InputName).asm \
	ml /nologo /Fo $(IntDir)\$(InputName).obj /c /coff $(IntDir)\$(InputName).asm \
	

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"$(IntDir)\$(InputName).asm" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "libffi - Win32 IntDebug"

# Begin Custom Build
IntDir=.\IntDebug
InputPath=.\src\x86\win32.S
InputName=win32

BuildCmds= \
	cl /nologo /EP /Imsvc32include /Isrc\x86 /I. /Iinclude  /D_MSC_VER $(InputPath) > $(IntDir)\$(InputName).asm \
	ml /nologo /Fo $(IntDir)\$(InputName).obj /c /coff $(IntDir)\$(InputName).asm \
	

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"$(IntDir)\$(InputName).asm" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "libffi - Win32 IntRelease"

# Begin Custom Build
IntDir=.\IntRelease
InputPath=.\src\x86\win32.S
InputName=win32

BuildCmds= \
	cl /nologo /EP /Imsvc32include /Isrc\x86 /I. /Iinclude  /D_MSC_VER $(InputPath) > $(IntDir)\$(InputName).asm \
	ml /nologo /Fo $(IntDir)\$(InputName).obj /c /coff $(IntDir)\$(InputName).asm \
	

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"$(IntDir)\$(InputName).asm" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# End Target
# End Project
