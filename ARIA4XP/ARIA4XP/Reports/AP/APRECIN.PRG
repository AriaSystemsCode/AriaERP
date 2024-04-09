*:***************************************************************************
*: Program file       : APRECIN
*: Program description: Recurring Invoice Report
*: Module             : Accounts Payable (AP)
*: Developer          : Saber A.Razek (SAB)
*: Tracking Job Number: E303061.EXE
*: Date               : 02/13/2012
*:***************************************************************************
*Modifications:
*:***************************************************************************
PRIVATE lcRpCurr, ldRpExDate, lcRpTmpNam
STORE '' TO lcRpCurr, lcRpTmpNam
STORE {} TO ldRpExDate

SET STEP ON &&SABER

SELECT APINVAHD
SET ORDER TO TAG HTYPCOD
IF lcRpForm == "APRECIND"
  SET ORDER TO TAG VENCODE IN APVENDOR
  SET ORDER TO TAG DTYPCOD IN APINVADT
  SET RELATION TO apinvahd.cautmtype+ apinvahd.cautmcode INTO APINVADT ADDITIVE
  SET RELATION TO apinvahd.cvendcode INTO APVENDOR ADDITIVE
  SET SKIP TO APINVADT
ELSE
  SET ORDER TO TAG VENCODE IN APVENDOR
  SET RELATION TO apinvahd.cvendcode INTO APVENDOR ADDITIVE
ENDIF


lcInvHdrTmp = gfTempName()
=lfCreateTemp()

*- Handel All Payment Method Case [Start]
lcRpExp = STRTRAN(lcRpExp, "APINVAHD.CVENPMETH = 'A'", "APINVAHD.CVENPMETH = ''")
*- Handel All Payment Method Case [End]

SELECT APINVAHD
SCAN FOR &lcRpExp.
  SELECT (lcInvHdrTmp)
  APPEND BLANK
  m.cAutmCode  = IIF(lcRpForm == "APRECIND", APINVADT.CAUTMCODE, APINVAHD.CAUTMCODE)
  m.cAutmType  = IIF(lcRpForm == "APRECIND", APINVADT.CAUTMTYPE, APINVAHD.CAUTMTYPE)
  m.cInstTyp   = IIF(APINVAHD.CAUTMBASE="P","Percent","Amount")
  m.cVendCode  = APINVAHD.CVENDCODE
  m.cVenComp   = APVENDOR.CVENCOMP
  m.dAutStGen  = APINVAHD.DAUTSTGEN
  m.dAutNxGen  = APINVAHD.DAUTNXGEN
  m.dAutEnGen  = APINVAHD.DAUTENGEN
  m.dAutLGen   = APINVAHD.DAUTLGEN
  m.nAutFstNo  = gfAmntDisp(APINVAHD.NAUTFSTNO,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
  m.nAutNxtNo  = APINVAHD.NAUTNXTNO
  m.cCurrCode  = APINVAHD.CCURRCODE
  m.cDivision  = LOOKUP(CODES.CDISCREP,'N'+"CDIVISION "+APINVAHD.CDIVISION,CODES.CFLD_NAME,"CCODE_NO")
  m.cPymntTerm = LOOKUP(CODES.CDISCREP,'N'+"CTERMCODE "+APINVAHD.CTERMCODE,CODES.CFLD_NAME,"CCODE_NO")
  m.cVenPrior  = APINVAHD.CVENPRIOR
  m.cVenPMeth  = IIF(APINVAHD.CVENPMETH="M","Manual chk",IIF(APINVAHD.CVENPMETH="P","Printed chk",IIF(APINVAHD.CVENPMETH="N","Non chk","Cash")))
  m.nInvAmnt   = gfAmntDisp(APINVAHD.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
  m.cAPAcct    = APINVAHD.CAPACCT
  m.nAPDLinNo  = APINVADT.NAPDLINNO
  m.cChkGlAcc  = APINVADT.CAPDGLACT  
  m.nAPDAmnt   = gfAmntDisp(APINVADT.NAPDAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
    
  GATHER MEMO MEMVAR
ENDSCAN

IF RECCOUNT(lcInvHdrTmp) <= 0
  *- There are no records to display.
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN .F.
ENDIF

=lfAdjustCRSettings()

IF USED(lcInvHdrTmp)
  USE IN (lcInvHdrTmp)
ENDIF


DO gfDispRe WITH EVAL('LCRPFORM') &&,'FOR '+lcRpExp
SET RELATION TO


*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Option Grid When function
*!*************************************************************
FUNCTION lfwRepWhen

ENDFUNC


*!*************************************************************
*! Name      : lfRepShow
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Option Grid Show
*!*************************************************************
FUNCTION lfRepShow
laOGObjCnt[3] = gfGetMemVar('LLMULCURR')
=lfOGShowGet("lnRepCurr")

ENDFUNC


*!*************************************************************
*! Name      : lfvVend
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Validate Vendor
*!*************************************************************
FUNCTION lfvVend
LOCAL lcAlias, lcOrder, lcVendCode
lcAlias    = ALIAS()
lcVendCode = laOgVrFlt[1,6]

IF !EMPTY(lcVendCode)
  SELECT APVENDOR
  lcOrder = SET("Order")
  SET ORDER TO VENCODE   && CVENDCODE
  IF !SEEK(lcVendCode, 'APVENDOR')
    =gfApVnBrow(@lcVendCode)
    laOgVrFlt[1,6] = lcVendCode
  ENDIF
  SET ORDER TO &lcOrder.
ENDIF
SELECT (lcAlias)

ENDFUNC


*!*************************************************************
*! Name      : lfvParior
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Validate Payment Priority
*!*************************************************************
FUNCTION lfvParior
lcFldNam=SYS(18)
IF !INLIST(EVAL(SYS(18)),'0','1','2','3','4','5','6','7','8','9')
  &lcFldNam  = " "
ENDIF

ENDFUNC


*!*************************************************************
*! Name      : lfvCurDisp
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Validate Currency Display
*!*************************************************************
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

ENDFUNC


*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Saber A.Razek (SAB)
*! Date      : 02/08/2012
*! Purpose   : Procedure to create temp.file 
*!*************************************************************
PROCEDURE lfCreateTemp
LOCAL lnItemPos
lnItemPos = 0
DIMENSION laStruArr[21, 4]

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cAutmType'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 1
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cAutmCode'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 8
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cInstTyp'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 10
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cVendCode'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 8
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cVenComp'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 30
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'dAutStGen'
laStruArr[lnItemPos, 2] = 'D'
laStruArr[lnItemPos, 3] = 8
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'dAutNxGen'
laStruArr[lnItemPos, 2] = 'D'
laStruArr[lnItemPos, 3] = 8
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'dAutEnGen'
laStruArr[lnItemPos, 2] = 'D'
laStruArr[lnItemPos, 3] = 8
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'dAutLGen'
laStruArr[lnItemPos, 2] = 'D'
laStruArr[lnItemPos, 3] = 8
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'nAutFstNo'
laStruArr[lnItemPos, 2] = 'N'
laStruArr[lnItemPos, 3] = 12
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'nAutNxtNo'
laStruArr[lnItemPos, 2] = 'N'
laStruArr[lnItemPos, 3] = 12
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cCurrCode'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 3
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cDivision'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 30
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cPymntTerm'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 30
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cVenPrior'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 1
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cVenPMeth'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 15
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'nInvAmnt'
laStruArr[lnItemPos, 2] = 'N'
laStruArr[lnItemPos, 3] = 15
laStruArr[lnItemPos, 4] = 2

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cAPAcct'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 24
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'nAPDLinNo'
laStruArr[lnItemPos, 2] = 'N'
laStruArr[lnItemPos, 3] = 4
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cChkGlAcc'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 24
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'nAPDAmnt'
laStruArr[lnItemPos, 2] = 'N'
laStruArr[lnItemPos, 3] = 15
laStruArr[lnItemPos, 4] = 2

=gfCrtTmp(lcInvHdrTmp,@laStruArr,'CAUTMTYPE+CAUTMCODE','HTYPCOD',.F.)

ENDPROC


*************************************************************
*! Name      : lfAdjustCRSettings
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : To set the report data files and parameters
*!*************************************************************
PROCEDURE lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[5,2]

LoOGScroll.llCrystal = .T.
LoOGScroll.lcOGLastForm = lcRpForm
loOGScroll.cCROrientation='L'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir + lcInvHdrTmp + ".DBF"

LOCAL lnI 
lnI  = 0
lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'ReportName'
loOgScroll.laCRParams[lnI ,2] = "Recurring payable invoices"
 
lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'Layout'
loOgScroll.laCRParams[lnI, 2] = ""&&"Account\Store"

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'SortBy'
loOgScroll.laCRParams[lnI, 2] = "Vendor"

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'OpTitle'
loOgScroll.laCRParams[lnI, 2] = lcRpOpTitl

*lnI = lnI + 1
*loOgScroll.laCRParams[lnI, 1] = 'SysDate'
*loOgScroll.laCRParams[lnI, 2] = oAriaApplication.SYSTEMDATE

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'lnDateFormat'
loOgScroll.laCRParams[lnI, 2] = IIF(UPPER(ALLTRIM(oAriaApplication.cActCompDateFormat)) == 'BRITISH', 1, 0)

ENDPROC
