# Microsoft Developer Studio Project File - Name="wininstall" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Generic Project" 0x010a

CFG=wininstall - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "wininstall.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wininstall.mak" CFG="wininstall - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wininstall - Win32 Release" (based on "Win32 (x86) Generic Project")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
MTL=midl.exe
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
# Begin Target

# Name "wininstall - Win32 Release"
# Begin Source File

SOURCE=.\Licence.rtf
# End Source File
# Begin Source File

SOURCE=.\PolyML.wxs
USERDEP__POLYM="Licence.rtf"	
# Begin Custom Build
InputPath=.\PolyML.wxs

"Release\PolyML.msi" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	candle.exe PolyML.wxs -o Release\PolyML.wixobj  -ext WixUtilExtension 
	light.exe Release\PolyML.wixobj -out Release\PolyML.msi -ext WixUIExtension -cultures:en-us -ext WixUtilExtension 
	
# End Custom Build
# End Source File
# End Target
# End Project
