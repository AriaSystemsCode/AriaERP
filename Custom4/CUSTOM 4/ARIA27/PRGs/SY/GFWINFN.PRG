*:************************************************************************
*: Program file  : GFWINFN.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
PARAMETERS lcCommand
*lcLib=SYS(2004)+"foxtools.fll"
*IF FILE(lcLib)
  SET LIBRARY TO FOXTOOLS.FLL ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
   ",'control.exe &lcCommand',"+STR(SW_Hide)+")")
   RELEASE LIBRARY FOXTOOLS.FLL
*ELSE
*  WAIT "LIBRARY NOT FOUND" WINDOW
*  RETURN .F.
*ENDIF   



  
  