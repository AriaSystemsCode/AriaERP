*:***************************************************************************
*: Program file       : APINSTM
*: Program description: Intallment Report
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

SELECT APINVAHD
SET ORDER TO TAG HTYPCOD 
SET ORDER TO TAG VENCODE IN APVENDOR
SET ORDER TO TAG INVVEND IN APINVHDR
SET RELATION TO apinvahd.cvendcode INTO APVENDOR ADDITIVE
SET RELATION TO apinvahd.cinvno+ apinvahd.cvendcode INTO APINVHDR ADDITIVE
SET SKIP TO APINVHDR

lcInvHdrTmp = gfTempName()
=lfCreateTemp()

SELECT APINVAHD
SCAN FOR &lcRpExp.
  SELECT (lcInvHdrTmp)
  APPEND BLANK
  m.cAutmType = APINVAHD.CAUTMTYPE
  m.cAutmCode = APINVAHD.CAUTMCODE
  m.cVendCode = APINVAHD.CVENDCODE
  m.cVenComp  = APVENDOR.CVENCOMP
  m.cInvNo    = APINVAHD.CINVNO
  m.cInvRef   = APINVHDR.CINVREF
  m.dInvDate  = APINVHDR.DINVDATE
  m.cVenPrior = APINVHDR.CVENPRIOR
  m.cVenPMeth = IIF(APINVHDR.CVENPMETH='M','Manual Chk', IIF(APINVHDR.CVENPMETH='P','Printed Chk', IIF(APINVHDR.CVENPMETH='H','Cash','Non Chk')))
  m.nOpenAmnt = gfAmntDisp(APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
  m.cInstTyp  = IIF(APINVAHD.CAUTMBASE='P','Percent','Amount')
  m.dAutNxGen = APINVAHD.DAUTNXGEN
  m.nInvAmtAP = APINVAHD.NINVAMTAP
  m.nInvA1099 = APINVAHD.NINVA1099
  m.cBnkCode  = APINVAHD.CBNKCODE
  m.cChkAcct  = APINVAHD.CCHKACCT
  m.cChkGlAcc = APINVAHD.CCHKGLACC
  m.cCurrCode = APINVHDR.CCURRCODE
    
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
*! Name      : lfvInvoice
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Validate Invoice
*!*************************************************************
FUNCTION lfvInvoice

DO CASE
  CASE lcRpStatus = 'O'
    DIMENSION laOGHDFlt[2,7]
    laOGHDFlt[2,1] = [ABS(APINVHDR.NINVAMNT)]
    laOGHDFlt[2,2] = 'F'    
    laOGHDFlt[2,3] = 'N'    
    laOGHDFlt[2,4] = .T.    
    laOGHDFlt[2,5] = 'Greater Than'    
    laOGHDFlt[2,6] = [ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK)]    
    laOGHDFlt[2,7] = 'E'

  CASE lcRpStatus = 'P'
    DIMENSION laOGHDFlt[2,7]
    laOGHDFlt[2,1] = [APINVHDR.NINVAMNT]
    laOGHDFlt[2,2] = 'F'    
    laOGHDFlt[2,3] = 'N'    
    laOGHDFlt[2,4] = .T.    
    laOGHDFlt[2,5] = 'Like'    
    laOGHDFlt[2,6] = [APINVHDR.NINVPAID+APINVHDR.NINVADJ+APINVHDR.NINVDISTK]        
    laOGHDFlt[2,7] = 'E'

  CASE lcRpStatus = 'A'  
    DIMENSION laOGHDFlt[2,7]
    laOGHDFlt[2,1] = [APINVHDR.CINVSTAT]
    laOGHDFlt[2,2] = 'F'    
    laOGHDFlt[2,3] = 'C'    
    laOGHDFlt[2,4] = .F.    
    laOGHDFlt[2,5] = 'Like'    
    laOGHDFlt[2,6] = 'V'    
    laOGHDFlt[2,7] = 'V'        
ENDCASE

ENDFUNC 


*!*************************************************************
*! Name      : lfvParior
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Validate Priority
*!*************************************************************
FUNCTION lfvParior
lcFldNam=SYS(18)
IF !INLIST(EVAL(SYS(18)),'0','1','2','3','4','5','6','7','8','9')
  &lcFldNam  = " "
ENDIF

ENDFUNC


*!*************************************************************
*! Name      : lfvChecks
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Validate Vendor
*!*************************************************************
FUNCTION lfvChecks

SELECT APCHECKS
lcSavOrder = SET('ORDER')   && Save old order
SET ORDER TO TAG BANKCHECK
lcFldNam = SYS(18)
IF SEEK(EVAL(SYS(18)))
  &lcFldNam=APCHECKS.CCHKACCT
ELSE
  IF !SEEK(EVAL(SYS(18))) .OR. ATC("?",EVAL(SYS(18))) > 0
    DIMENSION laTemp[2]  
    laTemp = ''
    lcSavBrFld=lcBrFields
    lcSavTitle=lcFile_Ttl
  
    lcBrFields="CBNKCODE :H= 'Bank Code',;
                CCHKACCT :H= 'Check Code'"
    lcFile_Ttl= "Checks"

    =gfBrows(.F.,'CBNKCODE,CCHKACCT','laTemp')

    lcBrFields=lcSavBrFld
    lcFile_Ttl=lcSavTitle
    
    IF !EMPTY(laTemp[2])
      &lcFldNam=laTemp[2]
    ELSE
      &lcFldNam=SPACE(24)
    ENDIF
  ENDIF
ENDIF
SET ORDER TO &lcSavOrder
SELECT APPAYMNT

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
DIMENSION laStruArr[18, 4]

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
laStruArr[lnItemPos, 1] = 'cInvNo'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 12
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cInvRef'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 15
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'dInvDate'
laStruArr[lnItemPos, 2] = 'D'
laStruArr[lnItemPos, 3] = 8
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
laStruArr[lnItemPos, 1] = 'nOpenAmnt'
laStruArr[lnItemPos, 2] = 'N'
laStruArr[lnItemPos, 3] = 15
laStruArr[lnItemPos, 4] = 2

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cInstTyp'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 10
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'dAutNxGen'
laStruArr[lnItemPos, 2] = 'D'
laStruArr[lnItemPos, 3] = 8
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'nInvAmtAP'
laStruArr[lnItemPos, 2] = 'N'
laStruArr[lnItemPos, 3] = 15
laStruArr[lnItemPos, 4] = 2

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'nInvA1099'
laStruArr[lnItemPos, 2] = 'N'
laStruArr[lnItemPos, 3] = 15
laStruArr[lnItemPos, 4] = 2

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cBnkCode'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 8
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cChkAcct'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 12
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cChkGlAcc'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 24
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cCurrCode'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 3
laStruArr[lnItemPos, 4] = 0

*lcWorkFile = lcInvHdrTmp
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

loOGScroll.cCROrientation='L'

loOgScroll.laCRTables[1] = oAriaApplication.WorkDir + lcInvHdrTmp + ".DBF"

LOCAL lnI 
lnI  = 0
lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'ReportName'
loOgScroll.laCRParams[lnI ,2] = "Installments"
 
lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'Layout'
loOgScroll.laCRParams[lnI, 2] = ""

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'SortBy'
loOgScroll.laCRParams[lnI, 2] = "Invoice No."

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


