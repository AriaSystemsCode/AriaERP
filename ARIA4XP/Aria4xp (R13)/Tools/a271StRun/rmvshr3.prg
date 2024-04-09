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

*-- RMCRMEM.PRG
lcCommand = "!ATTRIB " + lcAriaPath + "PRGS\RM\RMCRMEM.FXP -R"
&lcCommand
ERASE (lcAriaPath + 'PRGS\RM\RMCRMEM.FXP')

*-- RMCRMEM.SPR
lcCommand = "!ATTRIB " + lcAriaPath + "SCREENS\RM\RMCRMEM.SPX -R"
&lcCommand
ERASE (lcAriaPath + 'SCREENS\RM\RMCRMEM.SPX')

*-- RMEDCST.SPR
lcCommand = "!ATTRIB " + lcAriaPath + "SCREENS\RM\RMEDCST.SPX -R"
&lcCommand
ERASE (lcAriaPath + 'SCREENS\RM\RMEDCST.SPX')

*-- RMVDDAT.SPR
lcCommand = "!ATTRIB " + lcAriaPath + "SCREENS\RM\RMVDDAT.SPX -R"
&lcCommand
ERASE (lcAriaPath + 'SCREENS\RM\RMVDDAT.SPX')

*-- POBARCD.*
lcCommand = "!ATTRIB " + lcAriaPath + "REPORTS\POBARCD.FXP -R"
&lcCommand
lcCommand = "!ATTRIB " + lcAriaPath + "REPORTS\POBARCD.FRX -R"
&lcCommand
lcCommand = "!ATTRIB " + lcAriaPath + "REPORTS\POBARCD.FRT -R"
&lcCommand
ERASE (lcAriaPath + 'REPORTS\POBARCD.FXP')
ERASE (lcAriaPath + 'REPORTS\POBARCD.FRX')
ERASE (lcAriaPath + 'REPORTS\POBARCD.FRT')

lcSysPath = ALLTRIM(lcSysPath) + IIF(RIGHT(lcSysPath,1) <> "\", "\","")
*-- Fix System Files
IF FILE(lcSysPath+"SYDREPRT.DBF") 
  SELECT 0 
  USE (lcSysPath+"SYDREPRT.DBF") AGAIN ALIAS DREPRT ORDER TAG CREP_ID
  DELETE FOR cRep_ID = "POBARCD"
  IF USED("DREPRT")
    USE IN DREPRT
  ENDIF  
ENDIF

*-- Fix System Files
IF FILE(lcSysPath+"SYREPUVR.DBF") 
  SELECT 0 
  USE (lcSysPath+"SYREPUVR.DBF") AGAIN ALIAS REPUVR ORDER TAG FLD_NAME
  DELETE FOR cRep_ID = "POBARCD"
  IF USED("REPUVR")
    USE IN REPUVR
  ENDIF  
ENDIF
  
