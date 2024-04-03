PARAMETER lcSysPath
PRIVATE lcAriaPath,lcCommand
IF TYPE('lcSysPath') <> 'C'
  lcSysPath = GETDIR('','Select System Files Directory')
ENDIF
IF EMPTY(lcSysPath)
  WAIT WINDOW 'No Sysfiles directory found...!'
  RETURN
ENDIF
lcAriaPath = SUBSTR(lcSysPath,1,LEN(lcSysPath) - 9)
lcCommand = "!ATTRIB " + lcAriaPath + "PRGS\SO\SOESSQTY.FXP -R"
&lcCommand
ERASE (lcAriaPath + 'PRGS\SO\SOESSQTY.FXP')

lcCommand = "!ATTRIB " + lcAriaPath + "SCREENS\SO\SOSSCL.SPX -R"
&lcCommand
ERASE (lcAriaPath + 'SCREENS\SO\SOSSCL.SPX')
