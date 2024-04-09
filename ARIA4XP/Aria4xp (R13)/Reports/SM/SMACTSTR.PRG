*:*********************************************************************************
*: Program file  : SMACTSTR.PRG
*: Program desc. : Print Acount Code Structure Report
*:        System : Aria4 XP.
*:        Module : SM (System Manager).
*:     Developer : Saber A Razek (SAB)
*:     Entry     : E303336
*:*********************************************************************************

SELECT SYCCOMP
SET ORDER TO TAG CCOMP_ID
=SEEK(lcRpCompID)

IF !USED('ACCOD')
  =gfOpenFile(ALLTRIM(SYCCOMP.cCom_DDir) + 'ACCOD' , 'Accsegno' , 'SH')
ENDIF
SELECT ACCOD

DO gfDispRe WITH EVAL('lcRpForm')

IF USED('ACCOD')
  USE IN ACCOD
ENDIF


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : The when function of the option grid
*!*************************************************************
FUNCTION lfwRepWhen

DECLARE laRpCmpDsc[1,1] , laRpCmpNo[1,1] &&, laRpCmpCod[1,5]
STORE SPACE(0) TO laRpCmpDsc , laRpCmpNo &&, laRpCmpCod

*- Fill Company Array.
=lfGetComps()

ENDFUNC
*- End of 


*!*************************************************************
*! Name      : lfvCompID
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Valid function for the field Company ID
*!*************************************************************
FUNCTION lfvCompID

lnPos = ASCAN(loOGScroll.laOGVrFlt, 'lcRpCompID')
IF lnPos <> 0 
  lnPos = ASUBSCRIPT(loOGScroll.laOGVrFlt, lnPos, 1)
  lcRpCompID = IIF(EMPTY(loOGScroll.laOGVrFlt[lnPos,6]), '', loOGScroll.laOGVrFlt[lnPos, 6])
ENDIF

ENDFUNC
*-End of lfvCompId


*!*************************************************************
*! Name      : lfGetComps
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Get company array
*!*************************************************************
FUNCTION lfGetComps

LOCAL lnComNo , lnI
*- Collect all companies
SELECT cComp_ID+" - "+cCom_Name FROM SYCCOMP INTO ARRAY laRpCmpCod ORDER BY 1

lnComNo = _TALLY

DECLARE laRpCmpDsc[lnComNo, 1] , laRpCmpNo[lnComNo, 1]
IF lnComNo > 0
  lnI = 1
  FOR lnI = 1 TO lnComNo
    laRpCmpDsc[lnI, 1] = laRpCmpCod[lnI, 1]
    laRpCmpNo[lnI, 1]  = LEFT(laRpCmpCod[lnI, 1], 2)
  ENDFOR
ENDIF

ENDFUNC
*--- end of lfGetComps.


*!*************************************************************
*! Name      : lfDefComp
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Function to get the default value of the company id
*!*************************************************************
FUNCTION lfDefComp

*IF Statment to check if there is an active company
IF !EMPTY(oAriaApplication.ActiveCompanyID) &&!EMPTY(gcAct_Comp)
  RETURN oAriaApplication.ActiveCompanyID &&gcAct_Comp
ELSE
  LOCAL lnAlias
  lnAlias = SELECT(0)
  SELECT SYCCOMP
  GO TOP
  SELECT (lnAlias)
  RETURN SYCCOMP.cComp_Id
ENDIF

ENDFUNC
*-End of lfDefComp