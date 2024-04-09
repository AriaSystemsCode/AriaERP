*E302908,1 MMT 06/01/2011 Add TotQty Field to Piktkt Browse in Automtic PL option grid[T20110524.0036]
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[T20100922.0014]
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleId+"\ALAUTP") 
=gfCallForm('ALAUTP',oAriaApplication.ActiveModuleId)
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
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
*-- end of lfPltCrts.

FUNCTION lfvWghtQty
CLEARREAD()

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

*!*************************************************************
*! Name      : lfInvSet
*! Developer : Haytham El_Sheltawi
*! Date      : 08/19/1998
*! Purpose   : Set function of In Range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfPktSet
PARAMETERS lcParm
IF lcParm = 'R'
  CLEARREAD()
ENDIF

FUNCTION lfGetTkt

STORE ""  TO lcPikTktFile
STORE .F. TO llPikTktRng
lnInd = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'PIKTKT.PIKTKT'),1)
IF lnInd <> 0
  lcPikTktFile = laOgFxFlt[lnInd,6]
  llPikTktRng  = (!EMPTY(lcPikTktFile) .AND. USED(lcPikTktFile) .AND. RECCOUNT(lcPikTktFile)>0)
ENDIF

FUNCTION mOptionGrid
lcExpr = gfOpGrid('ALAUTP',.T. ,.F. ,.F. ,.T. ,.T.)


*E302908,1 MMT 06/01/2011 Add TotQty Field to Piktkt Browse in Automtic PL option grid[Start]
*!*************************************************************
*! Name      : lfPikQty
*! Developer : Mariam Mazhar[MMT]
*! Date      : 06/01/2011
*! Purpose   : Get Picked Qty for each Pitkt
*!*************************************************************
FUNCTION lfPikQty
lnOldAl = SELECT()
lcPiktk = Piktkt.Piktkt
lcOrderN = Piktkt.Order
lcStore = Piktkt.Store
lnPikQ = 0
IF !USED("Ordline_P")
  =gfOPenTable('Ordline','ORDLINST','SH',"Ordline_P")
ELSE
  SELECT Ordline_P    
  =gfSetOrder('ORDLINST')
ENDIF
SELECT Ordline_P
=gfSeek('O'+lcOrderN+lcStore)
SUM PIK1+PIK2+PIK3+PIK4+PIK5+PIK6+PIK7+PIK8 REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+lcOrderN+lcStore  FOR Piktkt = lcPiktk  TO lnPikQ
SELECT(lnOldAl)
RETURN lnPikQ
*E302908,1 MMT 06/01/2011 Add TotQty Field to Piktkt Browse in Automtic PL option grid[End]