*:***************************************************************************
*: Program file  : ALAUTP
*: Program desc. : automatic Allocation program
*: System        : Aria Advantage Series.4XP (NEW Framework)
*: Module        : Allocation (AL)
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*:***************************************************************************
*N000624,1 Convert Program to work from request Handler[T20080413.0001]
FUNCTION lfPltCrts

PRIVATE laAccount,lnSlct,lcPkAls
DIMENSION laAccount[1]
laAccount[1] = .F.
lnSlct = SELECT()

IF llEdiSys AND llRpBolAsi
  =gfOpenFile(oAriaApplication.DataDir + 'EDIACPRT' , oAriaApplication.DataDir + 'ACCFACT' , 'SH')
  =gfOpenFile(oAriaApplication.DataDir + 'EDIPH' , oAriaApplication.DataDir + 'PARTNER' , 'SH')

	lnPikTkPos = ASUBSCRIPT( laOgFxFlt , ASCAN(laOgFxFlt,'PIKTKT.PIKTKT') , 1 )
	lcPkAls = laOgFxFlt[lnPikTkPos,6]
	IF USED(lcPkAls) AND RECCOUNT(lcPkAls) > 0

		SELECT DISTINCT ACCOUNT ;
			FROM PIKTKT , &lcPkAls , EDIACPRT , EDIPH ;
			WHERE PIKTKT.PIKTKT = &lcPkAls..PIKTKT ;
			AND PIKTKT.STATUS $ 'OP' ;
			AND PIKTKT.ACCOUNT = EDIACPRT.CPARTNER ;
			AND EDIACPRT.CPARTCODE = EDIPH.CPARTCODE ;
			AND EDIPH.LPLTSHP INTO ARRAY laAccount

	ELSE

		SELECT DISTINCT ACCOUNT ;
			FROM PIKTKT , EDIACPRT , EDIPH ;
			WHERE PIKTKT.STATUS $ 'OP' ;
			AND PIKTKT.ACCOUNT = EDIACPRT.CPARTNER ;
			AND EDIACPRT.CPARTCODE = EDIPH.CPARTCODE ;
			AND EDIPH.LPLTSHP INTO ARRAY laAccount
	ENDIF

	RETURN EMPTY(laAccount[1])

ENDIF

FUNCTION lfPktSet
PARAMETERS lcParm
IF lcParm = 'R'
  CLEARREAD()
ENDIF

FUNCTION lfvUnits
PRIVATE lcStats
lcStats = IIF(lnRpMnlQty > 0,'ENABLE','DISABLE')
SHOW GET pbRun &lcStats
IF lnOgSeting = 1 AND lnRpMnlQty > 0
	SAVE ALL LIKE lnRpMnlQty* TO MnlQty.MEM
ENDIF

FUNCTION lfvWieght
PRIVATE lcStats
lcStats = IIF(lnRpWght > 0,'ENABLE','DISABLE')
SHOW GET pbRun &lcStats

FUNCTION lfwOGWhen


*loOgScroll.laOgFxFlt
IF lnOgSeting = 1 AND FILE('MnlQty.MEM')
  RESTORE FROM MnlQty.MEM ADDITIVE
ENDIF
*!*	PRIVATE lnCartNo,lnFromCrt,lnBookQty,lnBookAmt,lnOldOQty,lnOldOAmt,lnLineNo,llOverWrite
*!*	llOverWrite = .F.
*!*	llPrnLbl = .T.
*!*	*llEdiSys = This.AssignBol AND ('AS' $ oAriaApplication.CompanySetupModules)
*!*	lcRPCart   = ''
*!*	lnRpTtCrtP = 0
*!*	llDyelot = gfGetMemVar('M_DYELOT',oAriaApplication.ActiveCompanyID) = 'Y'

*!*	*--Define variables that used in OG
*!*	STORE 0 TO lnRpWght, lnRpTtCrtP, lnRpMnlQty
*!*	STORE .F. TO llRpMltSku, llRp1BoxPk
*!*	STORE ' ' TO lcRpBrkTyp, lcRpSortBy, lcRPCart
*!*	STORE .F. TO llRpBolAsi, llRpUseExs, llRpUsePre
*!*	STORE .F. TO llCheck2 ,llChngPrpk

*!*	STORE 0 TO lnFrmCrt,lnToCrt,lnNoCrt
*!*	*--variable to Store the FRom Carton Number
*!*	*-- initialize variables
*!*	STORE 0 TO lnRemendr , lnPicQty
*!*	STORE 0 TO lnStorFrm
*!*	STORE 0 TO lnOldCrtNo, lnOldweght, lnWgInCrt, lnQtyInCrt
*!*	lcLastSty = ' '
*!*	llEdiSys  =  ('AS' $ oAriaApplication.CompanySetupModules)



FUNCTION lfvWghtQty
CLEARREAD()

*:**************************************************************************
*:* Name        : lfMltSku
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/15/2002
*:* Purpose     : Valid fn. for "Multiple SKU" Option
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfMltSku()
*:***************************************************************************
FUNCTION lfMltSku
CLEARREAD()
*-- end of lfMltSku.

*:**************************************************************************
*:* Name        : lfBoxPkt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/12/2002
*:* Purpose     : Valid fn. for "Single Box per Pick Tkt"
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfBoxPkt()
*:***************************************************************************
FUNCTION lfBoxPkt

CLEARREAD()

*-- end of lfBoxPkt.

FUNCTION lfvBolAsgn
CLEARREAD()

