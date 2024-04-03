*:*********************************************************************************
*: Program file  : SMINTER.PRG
*: Program desc. : Print Company Information Report
*:        System : Aria4 XP.
*:        Module : SM (System Manager).
*:     Developer : Saber A Razek (SAB)
*:     Entry     : E303336
*:*********************************************************************************

SELECT SYCINT

GO TOP

DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : The when function of the option grid
*!*************************************************************
FUNCTION lfwRepWhen

DECLARE laRpCntDsc[1,1] , laRpCntNo[1,1] , laRpCntCod[1,5]
STORE SPACE(0) TO laRpCntDsc , laRpCntNo , laRpCntCod

*- Fill Country Array.
=lfGetCounts()

ENDFUNC
*- End of 


*!*************************************************************
*! Name      : lfvCountID
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Valid function for the field Country ID
*!*************************************************************
FUNCTION lfvCountID


ENDFUNC
*-End of lfvCountID


*!*************************************************************
*! Name      : lfGetCounts
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Get countries array
*!*************************************************************
FUNCTION lfGetCounts

LOCAL lnComNo , lnI
*- Collect all companies
SELECT cCont_Code+" - "+cCont_Desc FROM SYCINT INTO ARRAY laRpCntCod ORDER BY 1

lnCntNo = _TALLY

DECLARE laRpCntDsc[lnCntNo, 1] , laRpCntNo[lnCntNo, 1]
IF lnCntNo > 0
  lnI = 1
  FOR lnI = 1 TO lnCntNo
    *laRpCntDsc[lnI, 1] = laRpCntCod[lnI, 1]
    laRpCntDsc[lnI, 1]  = SUBSTR(laRpCntCod[lnI, 1], AT('-', laRpCntCod[lnI, 1], 1)+2)
    laRpCntNo[lnI, 1]  = LEFT(laRpCntCod[lnI, 1], 6)    
  ENDFOR
ENDIF

ENDFUNC
*--- end of lfGetCounts.


*!*************************************************************
*! Name      : lfDefCount
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Function to get the default value of the country id
*!*************************************************************
FUNCTION lfDefCount

LOCAL lnAlias
lnAlias = SELECT(0)
SELECT SYCINT
GO TOP
SELECT (lnAlias)
RETURN SYCINT.cCont_Code

ENDFUNC
*-End of lfDefCount