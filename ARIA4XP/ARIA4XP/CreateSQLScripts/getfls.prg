_SCREEN.Visible = .F. 
lcSysFilePath = GETDIR('','Select Aria27 SysFiles Folder')
IF EMPTY(lcSysFilePath) OR !DIRECTORY(lcSysFilePath) OR !FILE(ADDBS(lcSysFilePath)+'SYCCOMP.DBF')
  MESSAGEBOX("Invalid SYSFILES Folder")
  RETURN .F.
ENDIF
DO crtacccar.Fxp WITH lcSysFilePath 
DO fixcodes.FXP WITH lcSysFilePath 
MESSAGE("Files are created successfully")