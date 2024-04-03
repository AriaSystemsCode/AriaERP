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
lcCommand = "!ATTRIB " + lcAriaPath + "REPORTS\IC\ICSRANK*.* -R"
&lcCommand
ERASE (lcAriaPath + 'REPORTS\IC\ICSRANK.FXP')
ERASE (lcAriaPath + 'REPORTS\IC\ICSRANK.FRX')
ERASE (lcAriaPath + 'REPORTS\IC\ICSRANK.FRT')

lcCommand = "!ATTRIB " + lcAriaPath + "REPORTS\IC\ICSTYUTL*.* -R"
&lcCommand
ERASE (lcAriaPath + 'REPORTS\IC\ICSTYUTL.FXP')
