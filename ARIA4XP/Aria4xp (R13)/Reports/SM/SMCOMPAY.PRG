* Modifications:
*: N000682,1 HES Globalization Changes [Globalization Project]

#INCLUDE R:\Aria4xp\REPORTS\SM\SMREP.H
*:*********************************************************************************
*: Program file  : SMCOMPAY.PRG
*: Program desc. : Print Company Information Report
*:        System : Aria4 XP.
*:        Module : SM (System Manager).
*:     Developer : Saber A Razek (SAB)
*:     Entry     : E303336
*:*********************************************************************************

IF !EMPTY(lcRpCompId)
  lcRpExp = IIF(EMPTY(lcRpExp),lcRpExp,"(" + lcRpExp + ") .AND." )+;
            "(SYCCOMP.cComp_id = '"+lcRpCompId+ "')"
ENDIF

DO CASE
  CASE lcRpForm = "SMCOMPAS"

    SELECT SYCCOMP
    SET ORDER TO TAG CCOMP_ID
    SET FILTER TO EMPTY(ccompprnt)
    SET RELATION TO SYCCOMP.cComp_id INTO SYCCOMP_A ADDITIVE
    SET SKIP TO SYCCOMP_A

    GO TOP
    DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp

    SELECT SYCCOMP
    SET FILTER TO
    SET RELATION TO
  CASE lcRpForm = "SMCOMPAD"
    SELECT SYCCOMP
    GO TOP
    DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp
ENDCASE

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

SELECT 0
IF !USED('SYCCOMP_A')
  USE (oAriaApplication.SysPath+"SYCCOMP") AGAIN ALIAS SYCCOMP_A
  INDEX ON CCOMPPRNT TO oAriaApplication.WorkDir+lcRpIndex+'.CDX'
  SET ORDER TO TAG &lcRpIndex
ENDIF

ENDFUNC
*- End of


*!*************************************************************
*! Name      : lfvCompID
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Valid function for the field Company ID
*!*************************************************************
FUNCTION lfvCompID


ENDFUNC
*-End of lfvCompID


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

DECLARE laRpCmpDsc[lnComNo+1, 1] , laRpCmpNo[lnComNo+1, 1]
*: N000682,1 HES Globalization Changes [Start]
*!*	laRpCmpDsc[1, 1] = 'All'
laRpCmpDsc[1, 1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))
*: N000682,1 HES Globalization Changes [End  ]
laRpCmpNo[1, 1]  = ''
IF lnComNo > 0
  lnI = 1
  FOR lnI = 1 TO lnComNo
    laRpCmpDsc[lnI+1, 1] = laRpCmpCod[lnI, 1]
    laRpCmpNo[lnI+1, 1]  = LEFT(laRpCmpCod[lnI, 1], 2)
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
