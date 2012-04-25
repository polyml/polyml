# Microsoft Developer Studio Project File - Name="PolyML" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=PolyML - Win32 IntDebug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "PolyML.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "PolyML.mak" CFG="PolyML - Win32 IntDebug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "PolyML - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "PolyML - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "PolyML - Win32 IntDebug" (based on "Win32 (x86) Console Application")
!MESSAGE "PolyML - Win32 IntRelease" (based on "Win32 (x86) Console Application")
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
# ADD CPP /nologo /MT /W3 /GX /O2 /D "_MBCS" /D "HOSTARCHITECTURE_X86" /YX /FD /Zm500 /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 libcmt.lib /nologo /subsystem:windows /incremental:yes /map /machine:I386
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
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "_MBCS" /D "HOSTARCHITECTURE_X86" /YX /FD /Zm500 /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 libcmtd.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept /libpath:"llibpolyml\Debug libpolymain\Debug"
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "PolyML - Win32 IntDebug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "PolyML___Win32_IntDebug"
# PROP BASE Intermediate_Dir "PolyML___Win32_IntDebug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "IntDebug"
# PROP Intermediate_Dir "IntDebug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "WINDOWS_PC" /D "i386" /YX /FD /Zm500 /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "_MBCS" /D "INTERPRETED" /YX /FD /Zm500 /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 libcmtd.lib kernel32.lib user32.lib wsock32.lib gdi32.lib comdlg32.lib advapi32.lib shell32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept /libpath:"llibpolyml\Debug libpolymain\Debug"
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 libcmtd.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept /libpath:"llibpolyml\Debug libpolymain\Debug"
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "PolyML - Win32 IntRelease"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "PolyML___Win32_IntRelease"
# PROP BASE Intermediate_Dir "PolyML___Win32_IntRelease"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "IntRelease"
# PROP Intermediate_Dir "IntRelease"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "WINDOWS_PC" /D "i386" /YX /FD /Zm500 /c
# ADD CPP /nologo /MT /W3 /GX /O2 /D "_MBCS" /D "INTERPRETED" /YX /FD /Zm500 /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 libcmt.lib kernel32.lib user32.lib wsock32.lib gdi32.lib comdlg32.lib advapi32.lib shell32.lib /nologo /subsystem:windows /incremental:yes /map /machine:I386
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 libcmt.lib /nologo /subsystem:windows /incremental:yes /map /machine:I386
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "PolyML - Win32 Release"
# Name "PolyML - Win32 Debug"
# Name "PolyML - Win32 IntDebug"
# Name "PolyML - Win32 IntRelease"
# Begin Group "Source Files"

# PROP Default_Filter "c;cpp;rc"
# Begin Source File

SOURCE=.\imports\polymli386.txt

!IF  "$(CFG)" == "PolyML - Win32 Release"

# Begin Custom Build
InputPath=.\imports\polymli386.txt

"polytemp.txt" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy $(InputPath) polytemp.txt

# End Custom Build

!ELSEIF  "$(CFG)" == "PolyML - Win32 Debug"

# Begin Custom Build
InputPath=.\imports\polymli386.txt

"polytemp.txt" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy $(InputPath) polytemp.txt

# End Custom Build

!ELSEIF  "$(CFG)" == "PolyML - Win32 IntDebug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "PolyML - Win32 IntRelease"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\imports\polymlint.txt

!IF  "$(CFG)" == "PolyML - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "PolyML - Win32 Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "PolyML - Win32 IntDebug"

# Begin Custom Build
InputPath=.\imports\polymlint.txt

"polytemp.txt" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy $(InputPath) polytemp.txt

# End Custom Build

!ELSEIF  "$(CFG)" == "PolyML - Win32 IntRelease"

# Begin Custom Build
InputPath=.\imports\polymlint.txt

"polytemp.txt" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy $(InputPath) polytemp.txt

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\polytemp.txt

!IF  "$(CFG)" == "PolyML - Win32 Release"

# Begin Custom Build
IntDir=.\Release
InputPath=.\polytemp.txt

"$(IntDir)\polyexport.obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\PolyImport.exe -H 32 $(InputPath) -o $(IntDir)\polyexport.obj < exportPoly.sml

# End Custom Build

!ELSEIF  "$(CFG)" == "PolyML - Win32 Debug"

# Begin Custom Build
IntDir=.\Debug
InputPath=.\polytemp.txt

"$(IntDir)\polyexport.obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\PolyImport.exe -H 32 $(InputPath) -o $(IntDir)\polyexport.obj < exportPoly.sml

# End Custom Build

!ELSEIF  "$(CFG)" == "PolyML - Win32 IntDebug"

# Begin Custom Build
IntDir=.\IntDebug
InputPath=.\polytemp.txt

"$(IntDir)\polyexport.obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\PolyImport.exe -H 32 $(InputPath) -o $(IntDir)\polyexport.obj < exportPoly.sml

# End Custom Build

!ELSEIF  "$(CFG)" == "PolyML - Win32 IntRelease"

# Begin Custom Build
IntDir=.\IntRelease
InputPath=.\polytemp.txt

"$(IntDir)\polyexport.obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(IntDir)\PolyImport.exe -H 32 $(InputPath) -o $(IntDir)\polyexport.obj < exportPoly.sml

# End Custom Build

!ENDIF 

# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico"
# Begin Source File

SOURCE=.\poly.ico
# End Source File
# Begin Source File

SOURCE=.\PolyML.rc
# End Source File
# End Group
# End Target
# End Project
