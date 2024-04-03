************************************************************
*! Name      : MSG
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/05/2012
*! Purpose   : MSG, can run only on my PC
************************************************************
*FUNCTION msg
*B610170
PARAMETERS llDoNotUseStep
IF SYS(0)='DEV4 # tarek'
  ON ERROR
  _SCREEN.Visible=.T.
  IF !llDoNotUseStep
    IF FILE('C:\TEMP\X.X')
      SET STEP ON
    ENDIF 
  ENDIF 
ENDIF 
*- End of MSG.

