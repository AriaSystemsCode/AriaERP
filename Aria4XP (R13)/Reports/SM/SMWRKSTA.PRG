*: Modifications:
*:=============:*
*: N000682,1 HES Globalization Changes [Globalization Project]

#INCLUDE R:\Aria4xp\REPORTS\SM\SMREP.H
*:*********************************************************************************
*: Program file  : SMWRKSTA.PRG
*: Program desc. : Print Workstation Report
*:        System : Aria4 XP.
*:        Module : SM (System Manager).
*:     Developer : Saber A Razek (SAB)
*:     Entry     : E303336
*:*********************************************************************************

SELECT SYCWRKST

GO TOP

DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : The when function of the option grid
*!*************************************************************
FUNCTION lfwRepWhen

DECLARE laRpWrkDsc[1,1] , laRpWrkNo[1,1] , laRpWrkCod[1,5]
STORE SPACE(0) TO laRpWrkDsc , laRpWrkNo , laRpWrkCod

*- Fill Workstation Array.
=lfGetWorks()

ENDFUNC
*- End of


*!*************************************************************
*! Name      : lfvWorkID
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Valid function for the field Country ID
*!*************************************************************
FUNCTION lfvWorkID


ENDFUNC
*-End of lfvWorkID


*!*************************************************************
*! Name      : lfGetWorks
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Get countries array
*!*************************************************************
FUNCTION lfGetWorks

LOCAL lnComNo , lnI
*- Collect all workstations
SELECT cWrkst_ID+" - "+cWrk_Name FROM SYCWRKST INTO ARRAY laRpWrkCod ORDER BY 1

lnWrkNo = _TALLY

DECLARE laRpWrkDsc[lnWrkNo+1, 1] , laRpWrkNo[lnWrkNo+1, 1]

*: N000682,1 HES Globalization Changes [Start]
*!*	laRpWrkDsc[1, 1] = 'All'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laRpWrkDsc[1, 1] = LANG_ALL
laRpWrkDsc[1, 1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*: N000682,1 HES Globalization Changes [End  ]

laRpWrkNo[1, 1] = ''
IF lnWrkNo > 0
  lnI = 1
  FOR lnI = 1 TO lnWrkNo
    laRpWrkDsc[lnI+1, 1] = SUBSTR(laRpWrkCod[lnI, 1], AT('-', laRpWrkCod[lnI, 1], 1)+2)
    laRpWrkNo[lnI+1, 1] = LEFT(laRpWrkCod[lnI, 1], 6)
  ENDFOR
ENDIF

ENDFUNC
*--- end of lfGetWorks.
