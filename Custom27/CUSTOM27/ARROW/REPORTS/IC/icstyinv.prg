**************************************************************************
*: Program file       : ICStyInv.PRG (C# )
*: Program desc.      : Stock On Hand Report For Arrow Misr
*:         System     : Aria Apparel System (A27).
*:      Developer     : AHMED SALAH SHALABY - (SSH)
*:************************************************************************
*: Calls : FUNCTIONS  : lfwRepWhen,lfVEndDate,lfClearRep
*:         PROCEDURE  : gfDispRe
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************

*--- Global variable to indicate if the selection criteria has been changed or not.
IF ldRpEndDate = {}
  WAIT WINDOW "Ending Period can not be empty..."
  RETURN
ENDIF
IF llOgFltCh
  IF FILE(gcWorkDir+lcRpStyTmp+'.DBF')
    USE IN IIF(USED(lcRpStyTmp),lcRpStyTmp,0)
    ERASE &gcWorkDir.&lcRpStyTmp+'.DBF'
    ERASE &gcWorkDir.&lcRpStyTmp+'.CDX'
  ENDIF
  CREATE TABLE (lcRpStyTmp) ;
	(Style C(19), nTotStk N(12,0), nStkVal N(15,2))
  INDEX ON Style TAG (lcRpStyTmp)
  SELECT STYINVJL
  GOTO TOP
  DO WHILE !EOF()
    lcStyle = Style
    SCAN REST WHILE Style+cwarecode+csession+DTOS(dtrdate)+ctrcode = lcStyle;
              FOR (dTrDate <= ldRpEndDate)
      SCAT MEMVAR MEMO
      WAIT WINDOW "Selctind transaction for Style: "+lcStyle +"Ending period: "+DTOC(ldRpEndDate) NOWAIT
      IF SEEK(m.STYLE,lcRpStyTmp)
        SELECT (lcRpStyTmp)
        REPLACE NTotStk WITH  NTotStk + m.NTotStk;
                nStkVal WITH  nStkVal + m.nStkVal
      ELSE
        SELECT (lcRpStyTmp)
        APPEND BLANK
        GATH MEMVAR MEMO
      ENDIF
    ENDSCAN
    SELECT STYINVJL
  ENDDO
  SELECT (lcRpStyTmp)
  SET RELATION TO STYLE INTO STYLE ADDI
ENDIF
SELECT (lcRpStyTmp)
SET FILTER TO (nTOTSTK<>0) .OR. (nStkVal<>0)
GOTO TOP
IF EOF()
  =gfModalGen('TRM38155B00000','DIALOG' )
  RETURN
ENDIF
*--- Call the FRX.
DO gfDispRe WITH EVAL('lcRpForm')
RETURN

*!*************************************************************
*! Name               : lfwRepWhen
*! Developer          : Ahmed Salah Shalaby -(SSH)
*! Date               : 05/05/99
*! Purpose            : Option grid when function.
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

ldRpEndDate = GDSYSDATE - Day(GDSYSDATE)

*!*************************************************************
*! Name               : lfVEndDate
*! Developer          : Ahmed Salah Shalaby -(SSH)
*! Date               : 05/05/99
*! Purpose            : Function to Valid Ending date.
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfVEndDate()
*!*************************************************************
FUNCTION lfVEndDate

*--- If the ending period date is empty inform the user then return.
IF ldRpEndDate = {}
  WAIT WINDOW "Ending Period can not be empty..."
  RETURN
ENDIF
*--- If the ending period not the end of the last month get the last of the
*--- enterd month.
IF DAY(ldRpEndDate+1) <> 1
  ldRpEndDate = ldRpEndDate - Day(ldRpEndDate)
ENDIF

*!*************************************************************
*! Name               : lfClearRep
*! Developer          : Ahmed Salah Shalaby -(SSH)
*! Date               : 05/05/99
*! Purpose            : Function to Clear temp file.
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

*--- Global variable to indicate if the selection criteria has been changed or not.
llOgFltCh = .T.
*---Erase the temp file and index.
USE IN IIF(USED(lcRpStyTmp),lcRpStyTmp,0)
ERASE &gcWorkDir.&lcRpStyTmp+'.DBF'
ERASE &gcWorkDir.&lcRpStyTmp+'.CDX'