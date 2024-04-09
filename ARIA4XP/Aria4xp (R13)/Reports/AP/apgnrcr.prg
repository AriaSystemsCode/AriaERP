*:***************************************************************************
*: Program file       : APDISC
*: Program description: Discount Report
*: Module             : Accounts Payable (AP)
*: Developer          : Saber A.Razek (SAB)
*: Tracking Job Number: E303061.EXE
*: Date               : 02/13/2012
*:***************************************************************************
*Modifications:
*:***************************************************************************

*!*	PRIVATE lcRpCurr, ldRpExDate, lcRpTmpNam
*!*	STORE '' TO lcRpCurr, lcRpTmpNam
*!*	STORE {} TO ldRpExDate

*!*	SELECT APINVHDR
*!*	SET ORDER TO TAG VENCODE IN APVENDOR
*!*	SET RELATION TO APINVHDR.cVendCode INTO APVENDOR ADDITIVE

*!*	lcInvHdrTmp = gfTempName()
*!*	=lfCreateTemp()

*!*	*- Handel All Payment Method Case [Start]
*!*	lcRpExp = STRTRAN(lcRpExp, "APINVHDR.CVENPMETH = 'A'", "APINVHDR.CVENPMETH = ''")
*!*	*- Handel All Payment Method Case [End]

*!*	SELECT APINVHDR
*!*	SCAN FOR &lcRpExp.
*!*	  SELECT (lcInvHdrTmp)
*!*	  APPEND BLANK
*!*	  m.cVendCode  = APINVHDR.CVENDCODE
*!*	  m.cVenComp   = APVENDOR.CVENCOMP
*!*	  m.cInvNo     = APINVHDR.CINVNO
*!*	  m.dInvDate   = APINVHDR.DINVDATE
*!*	  m.cVenPrior  = APINVHDR.CVENPRIOR
*!*	  m.dInvDuDat  = APINVHDR.DINVDUDAT
*!*	  m.dInvDisDat = APINVHDR.DINVDATE + APINVHDR.NTERDISCD
*!*	  m.nInvAmnt   = gfAmntDisp(APINVHDR.NINVAMNT, lcRpCurr, ldRpExDate, lcRpTmpNam, .F.)
*!*	  m.nPrvPymnt  = gfAmntDisp(APINVHDR.NINVPAID+APINVHDR.NINVDISTK+APINVHDR.NINVADJ, lcRpCurr, ldRpExDate, lcRpTmpNam, .F.)
*!*	  m.nInvDisOf  = APINVHDR.NINVDISOF
*!*	  m.nInvDisTkn = APINVHDR.NINVDISTK
*!*	  m.nInvDisLos = IIF(APINVHDR.DINVDATE+APINVHDR.NTERDISCD < DATE(), MAX(APINVHDR.NINVDISOF - APINVHDR.NINVDISTK, 0.00), 0.00)
*!*	  m.nInvDisAvl = IIF(APINVHDR.DINVDATE+APINVHDR.NTERDISCD > DATE(), MAX(APINVHDR.NINVDISOF - APINVHDR.NINVDISTK, 0.00), 0.00)
*!*	  
*!*	  GATHER MEMO MEMVAR
*!*	ENDSCAN

*!*	IF RECCOUNT(lcInvHdrTmp) <= 0
*!*	  *- There are no records to display.
*!*	  =gfModalGen('TRM00052B40011','ALERT')
*!*	  RETURN .F.
*!*	ENDIF

*!*	=lfAdjustCRSettings()

*!*	IF USED(lcInvHdrTmp)
*!*	  USE IN (lcInvHdrTmp)
*!*	ENDIF

*!*	SET STEP ON &&SABER
*!*	DO gfDispRe WITH EVAL('LCRPFORM') &&,'FOR '+lcRpExp
*!*	SET RELATION TO

*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Option Grid When function
*!*************************************************************
FUNCTION lfwRepWhen

WAIT WINDOW LANG_WHEN NOWAIT 
lcRpGenDat = oAriaApplication.SystemDate

ENDFUNC


*!*************************************************************
*! Name      : lfRepShow
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : Option Grid Show
*!*************************************************************
FUNCTION lfRepShow
*laOGObjCnt[2] = gfGetMemVar('LLMULCURR')
*=lfOGShowGet("lnRepCurr")

ENDFUNC

FUNCTION ldDefGenDat

RETURN oAriaApplication.SystemDate
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