**:***********************************************************************
*:  Program file : ALPLCAC.PRG
*:  Program desc.: Mass Packing List Cancellation 
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar
*:           Date: 09/16/2015
*:      Reference: E303605[T20150821.0003]
*:************************************************************************
*: Modifications: 
*: B611107,1 MMT 01/26/2016 Pack_hdr is not deleted from custom Mass Packing list cancellation program[T20160120.0004]
*: C202045,1 AHH 12/07/2017  Packing is not working [Begin][T20170628.0003]

*:************************************************************************
#INCLUDE r:\aria4xp\screens\al\ALplCAN.h
*!*************************************************************
*! Name      : lfvScope
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/16/2015
*! Purpose   : Valid function of push button Scope
*!*************************************************************
*! Called from : Control Panel , lpShow
*!*************************************************************
*! Calls       : gfOpGrid() , lfCrTmKtTk() , lfDispBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfvScope
PARAMETERS loFormSet
 lndataSessPre = SET("Datasession" )
 IF loFormSet.llCalledFromOp
    loFormSet.llCallOption = loFormSet.llCalledFromOp
 ENDIF
 loFormSet.llCallScop = .F.             && Screen Already Initialized
 lcDataSessI = SET("Datasession" )&&THIS.loFormSet.DatasessionID
 DIMENSION laOgSelect[1]
 laOgSelect =''
 lcExpr = gfOpGrid('ALPLCAN' , .T.)&&,.F.,.F.,.T.,.T.)
 SET DATASESSION TO lcDataSessI


 IF lcExpr <> ".F."
   IF USED(loFormSet.lcTempFileHdr)
     SELECT (loFormSet.lcTempFileHdr) 
     ZAP
   ENDIF
   *!*	PACK_HDR.PACK_NO
   llSelePack = .F. && flag to indicate if there is any piktkt selected
   lnPosPik = ASCAN(laOgSelect,"PACK_HDR.PACK_NO")
   IF lnPosPik > 0
     lnPosPik = ASUBSCRIPT(laOgSelect,lnPosPik,1)
     lcFilePik =IIF(!EMPTY(laOgSelect[lnPosPik,6]),laOgSelect[lnPosPik,6],'')
     IF !EMPTY(lcFilePik) AND USED(lcFilePik) AND RECCOUNT(lcFilePik)> 0
       SELECT(lcFilePik)
       LOCATE For !DELETED()
       IF !EOF()
         llSelePack= .T.
       ENDIF  
     ENDIF
   ENDIF
   
   *!*	BOL_HDR.BOL_NO
   llSeleBol= .F. && flag to indicate if there is any piktkt selected
   lnPosBol= ASCAN(laOgSelect,"BOL_HDR.BOL_NO")
   IF lnPosPik > 0
     lnPosBol = ASUBSCRIPT(laOgSelect,lnPosBol,1)
     lcFileBol=IIF(!EMPTY(laOgSelect[lnPosBol,6]),laOgSelect[lnPosBol,6],'')
     IF !EMPTY(lcFileBol) AND USED(lcFileBol) AND RECCOUNT(lcFileBol)> 0
       SELECT(lcFileBol)
       LOCATE For !DELETED()
       IF !EOF()
         llSeleBol = .T.
       ENDIF  
     ENDIF
   ENDIF
   
   *!*	PACK_HDR.CWARECODE 
   llSeleWare= .F. && flag to indicate if there is any piktkt selected
   lnPosWare= ASCAN(laOgSelect,"PACK_HDR.CWARECODE")
   IF lnPosWare > 0
     lnPosWare= ASUBSCRIPT(laOgSelect,lnPosWare,1)
     lcFileWare=IIF(!EMPTY(laOgSelect[lnPosWare,6]),laOgSelect[lnPosWare,6],'')
     IF !EMPTY(lcFileWare) AND USED(lcFileWare) AND RECCOUNT(lcFileWare)> 0
       SELECT(lcFileWare)
       LOCATE For !DELETED()
       IF !EOF()
         llSeleWare= .T.
       ENDIF  
     ENDIF
   ENDIF

   *!*	PACK_HDR.ORDER     
   llSeleOrder= .F. && flag to indicate if there is any piktkt selected
   lnPosOrder= ASCAN(laOgSelect,"PACK_HDR.ORDER")
   IF lnPosOrder> 0
	 lnPosOrder= ASUBSCRIPT(laOgSelect,lnPosOrder,1)
	 lcFileOrder=IIF(!EMPTY(laOgSelect[lnPosOrder,6]),laOgSelect[lnPosOrder,6],'')
	 IF !EMPTY(lcFileOrder) AND USED(lcFileOrder) AND RECCOUNT(lcFileOrder)> 0
	   SELECT(lcFileOrder)
	   LOCATE For !DELETED()
	   IF !EOF()
	     llSeleOrder= .T.
	   ENDIF  
	 ENDIF
   ENDIF
   *!*	PACK_HDR.ACCOUNT   
   llSeleAcc= .F. && flag to indicate if there is any piktkt selected
   lnPosAcc= ASCAN(laOgSelect,"PACK_HDR.ACCOUNT")
   IF lnPosAcc> 0
     lnPosAcc= ASUBSCRIPT(laOgSelect,lnPosAcc,1)
     lcFileAccount=IIF(!EMPTY(laOgSelect[lnPosAcc,6]),laOgSelect[lnPosAcc,6],'')
     IF !EMPTY(lcFileAccount) AND USED(lcFileAccount) AND RECCOUNT(lcFileAccount)> 0
       SELECT(lcFileAccount)
       LOCATE For !DELETED()
       IF !EOF()
         llSeleAcc= .T.
       ENDIF  
     ENDIF
   ENDIF
       
   DO CASE 
     CASE llSelePack && Pack_No selected 
       SELECT PACK_HDR  
       =gfSetOrder("PACK_HDR")
       SELECT (lcFilePik)
       LOCATE
       SCAN FOR !DELETED()
         lcPack= Pack_No
         =gfSeek(lcPack,"PACK_HDR","PACK_HDR")
         SELECT PACK_HDR
         IF IIF(llSeleAcc ,SEEK(PACK_HDR.Account,lcFileAccount),.T.) AND;
            IIF(llSeleOrder, SEEK(PACK_HDR.ORDER,lcFileOrder),.T.) AND ;
            IIF(llSeleWare, SEEK(PACK_HDR.CWARECODE,lcFileWare),.T.) AND ;
            IIF(llSeleBol, SEEK(PACK_HDR.bill_ladg,lcFileBol),.T.) AND Status <> 'C'
           SCATTER MEMO MEMVAR 
           INSERT INTO (loFormSet.lcTempFileHdr) FROM MEMVAR 
         ENDIF  
         SELECT (lcFilePik)
       ENDSCAN 
     CASE llSeleAcc && Account is Selected
        SELECT PACK_HDR     
        =gfSetOrder("ACCPACK")
        SELECT(lcFileAccount)
        LOCATE
        SCAN FOR !DELETED()
          lcAccnt = Account
          SELECT PACK_HDR
          =gfSeek(lcAccnt)
          SCAN REST WHILE  ACCOUNT+PACK_NO = lcAccnt FOR IIF(llSeleOrder, SEEK(PACK_HDR.ORDER,lcFileOrder),.T.) AND ;
			                IIF(llSeleWare, SEEK(PACK_HDR.CWARECODE,lcFileWare),.T.) AND IIF(llSeleBol, SEEK(PACK_HDR.bill_ladg,lcFileBol),.T.) AND Status <> 'C'
            SCATTER MEMO MEMVAR 
            INSERT INTO (loFormSet.lcTempFileHdr) FROM MEMVAR 
          ENDSCAN
          SELECT(lcFileAccount) 
        ENDSCAN 
     CASE llSeleOrder&& Order is Selected
       SELECT PACK_HDR     
       =gfSetOrder("ORDERPCK")    
       SELECT(lcFileOrder)
       LOCATE 
       SCAN FOR !DELETED()
         lcOrder = Order
         SELECT PACK_HDR
         =gfSeek(lcOrder)
         SCAN REST WHILE ORDER+STORE+PACK_NO =lcOrder FOR IIF(llSeleWare, SEEK(PACK_HDR.CWARECODE,lcFileWare),.T.) AND;
              IIF(llSeleBol, SEEK(PACK_HDR.bill_ladg,lcFileBol),.T.) AND Status <> 'C'
           SCATTER MEMO MEMVAR 
           INSERT INTO (loFormSet.lcTempFileHdr) FROM MEMVAR     
         ENDSCAN
         SELECT(lcFileOrder)  
       ENDSCAN 
     OTHERWISE 
       SELECT PACK_HDR  
       =gfSetOrder("PACK_HDR")
       LOCATE 
       SCAN FOR IIF(llSeleWare, SEEK(PACK_HDR.CWARECODE,lcFileWare),.T.) AND;
              IIF(llSeleBol, SEEK(PACK_HDR.bill_ladg,lcFileBol),.T.) AND Status <> 'C'
         SCATTER MEMO MEMVAR 
         INSERT INTO (loFormSet.lcTempFileHdr) FROM MEMVAR           
   	   ENDSCAN
   ENDCASE 


   SELECT (loFormSet.lcTempFileHdr)
   LOCATE
   lfAddControlSource(loFormSet)
   loFormSet.lnSelRec   = 0
   loFormSet.lnDelRec   = 0
   loFormSet.llenablerel = .F.
   loFormSet.lnUnSelRec = RECCOUNT()
   IF !EOF()
     loFormSet.llEnableInvert = .T.
     loFormSet.llEnableSelect = .T.
     loFormSet.llEnableSelectall = .T.
     loFormSet.llEnableSelectnone = .F.
   ELSE    && Else
     loFormSet.llEnableInvert = .F.
     loFormSet.llEnableSelect = .F.
     loFormSet.llEnableSelectAll = .F.
     loFormSet.llEnableSelectNone = .F.
   ENDIF    && End of IF
 ELSE
 RETURN
ENDIF
*!*************************************************************
*! Name      : lfInit
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 09/16/2015
*! Purpose   : init function of th from
*!*************************************************************
FUNCTION lfInit
  LPARAMETERS loFormSet
  loFormSet.lcTempFileHdr = gfTempName()
  SET MULTILOCKS ON
  gfOpenTable('Pack_hdr','Pack_hdr')
  gfOpenTable('Pack_lin','Pack_lin')
  gfOpenTable('ordline','ORDLINST')
  
  IF ('AS' $ oAriaApplication.CompanyInstalledModules)
    gfOpenTable('EDICRTSQ','EDICRTSQ','SH','EDICRTSQ')
    gfOpenTable('BOL_HDR','BOL_HDR')
    gfOpenTable('BOL_LIN','BOL_LIN')
  ENDIF
  
  DIMENSION laFileStru[10,4]

  laFileStru[1,1] = 'LLSEL'
  laFileStru[1,2] = 'L'
  laFileStru[1,3] = 1
  laFileStru[1,4] = 0

  laFileStru[2,1] = 'PACK_NO'
  laFileStru[2,2] = 'C'
  laFileStru[2,3] = 6
  laFileStru[2,4] = 0

  laFileStru[3,1] = 'Account'
  laFileStru[3,2] = 'C'
  laFileStru[3,3] = 5
  laFileStru[3,4] = 0
  
  laFileStru[4,1] = 'bill_ladg'
  laFileStru[4,2] = 'C'
  laFileStru[4,3] = 6
  laFileStru[4,4] = 0

  laFileStru[5,1] = 'CWARECODE'
  laFileStru[5,2] = 'C'
  laFileStru[5,3] = 6
  laFileStru[5,4] = 0

  laFileStru[6,1] = 'STORE'
  laFileStru[6,2] = 'C'
  laFileStru[6,3] = 8
  laFileStru[6,4] = 0

  laFileStru[7,1] = 'ORDER'
  laFileStru[7,2] = 'C'
  laFileStru[7,3] = 6
  laFileStru[7,4] = 0

  laFileStru[8,1] = 'TOT_PCS'
  laFileStru[8,2] = 'N'
  laFileStru[8,3] = 8
  laFileStru[8,4] = 0

  laFileStru[9,1] = 'TOT_CART'
  laFileStru[9,2] = 'N'
  laFileStru[9,3] = 8
  laFileStru[9,4] = 0

  laFileStru[10,1] = 'TOT_WGHT'
  laFileStru[10,2] = 'N'
  laFileStru[10,3] = 13
  laFileStru[10,4] = 2
  
  =gfCrtTmp(loFormSet.lcTempFileHdr ,@laFileStru,"PACK_NO",  loFormSet.lcTempFileHdr ,.T.)
  lfAddControlSource(loFormSet)

  WITH loFormSet
    .nWorkArea        = 'PACK_HDR'
    .DataEnvironment.InitialSelectedAlias = 'PACK_HDR'
    .cbrowsetabledbengine   = 'NATIVE'
  ENDWITH

  loFormSet.llMultiWH  = (ALLTRIM(gfGetMemVar('M_WareHouse')) = 'Y')

  loFormSet.llCallScop = .F.   &&Flag to hold the first time of the session
  loFormSet.llFrstTime = .T.       && Flag to know if we are going to call lpShow for the first time
ENDFUNC
*!*************************************************************
*! Name      : lfAddControlSource
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/16/2015
*! Purpose   : Adjust grid control source
*!*************************************************************
FUNCTION lfAddControlSource
PARAMETERS loFormSet
WITH loFormSet.ariaform1.grdPack.grdMultiSelectionGrid
    .RecordSource = ""
    .RecordSource = loFormSet.lcTempFileHdr 
    .Column1.Header1.Caption = ""
    .Column1.CurrentControl = "AriaCheckBox1"
    .column1.ControlSource ='Thisformset.mgetValueLogic()'
    .column2.ControlSource = loFormSet.lcTempFileHdr +'.PACK_NO'
    .column3.ControlSource = loFormSet.lcTempFileHdr +'.ACCOUNT'
    .column4.ControlSource = loFormSet.lcTempFileHdr +'.STORE'
    .column5.ControlSource = loFormSet.lcTempFileHdr +'.ORDER'
    .column6.ControlSource = loFormSet.lcTempFileHdr +'.bill_ladg'
    .column6.VISIBLE = ('AS' $ oAriaApplication.CompanyInstalledModules)
    .column7.ControlSource = loFormSet.lcTempFileHdr +'.CWARECODE'
    .column8.ControlSource = loFormSet.lcTempFileHdr +'.TOT_WGHT'
    .column10.ControlSource = loFormSet.lcTempFileHdr +'.TOT_PCS'
    .column9.ControlSource  = loFormSet.lcTempFileHdr +'.TOT_CART'
   
    .Column1.Enabled = .T.
    .SETALL('ReadOnly',.T.,'COLUMN')
    .Column1.readonly = .F.
	.Enabled = .T.
    .Column1.AriaCheckBox1.Enabled = .T.
    .refresh()
  ENDWITH
ENDFUNC
*!*************************************************************
*! Name      : lfvSelAll
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/16/2015
*! Purpose   : Valid function of push button Select all
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelAll
PARAMETERS loFormSet
SELECT(loFormSet.lcTempFileHdr)
lnRecCurrn = RECNO()

REPLACE ALL LLSEL WITH .T.
loFormSet.lnSelRec   = RECCOUNT() - loFormSet.lnDelRec
loFormSet.lnUnSelRec = 0
GO lnRecCurrn

loFormSet.lcCaptionSel = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_unSelect,loFormSet.GetHeaderText("LANG_unSelect",loFormSet.HeaderAlias))

loFormSet.llenableselectall = .F.
loFormSet.llenableselectnone = .T.
loFormSet.llenablerel = .T.
*!*************************************************************
*! Name      : lfvSelNon
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/16/2015
*! Purpose   : Valid function of push button Select none
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelNon
PARAMETERS loFormSet
SELECT(loFormSet.lcTempFileHdr)

lnRecCurr = RECNO()

REPLACE ALL LLSEL WITH .F.

loFormSet.lnSelRec   = 0
loFormSet.lnUnSelRec = RECCOUNT() - loFormSet.lnDelRec
GO lnRecCurr
loFormSet.lcCaptionSel = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select,loFormSet.GetHeaderText("LANG_Select",loFormSet.HeaderAlias))

loFormSet.llEnableSelectAll  = .T.
loFormSet.llEnableSelectNone = .F.
loFormSet.llEnableRel = .F.
*!*************************************************************
*! Name      : lfvInvert
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/16/2015
*! Purpose   : Valid function of push button Invert
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : lfvpbSel()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvInvert
PARAMETERS loFormSet
SELECT(loFormSet.lcTempFileHdr)
lnRecNOCurr = RECNO()
REPLACE ALL LLSEL WITH !LLSEL
GO lnRecNOCurr

lfvpbSel(loFormSet)

loFormSet.lnUnSelRec = loFormSet.lnSelRec
loFormSet.lnSelRec   = RECCOUNT() - loFormSet.lnDelRec - loFormSet.lnSelRec

*there is no selected records
IF loFormSet.lnSelRec = 0
  loFormSet.llenableselectall = .T.
  loFormSet.llenableselectnone = .F.
  loFormSet.llenablerel = .F.
ELSE
  loFormSet.llenableselectnone = .T.
  loFormSet.llenablerel = .T.

  *--All the records were selected
  IF loFormSet.lnUnSelRec = 0
    loFormSet.llenableselectall = .F.
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvSelect
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/16/2015
*! Purpose   : Valid function of push button Select
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : lfvpbSel()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelect
PARAMETERS loFormSet
SELECT(loFormSet.lcTempFileHdr)
REPLACE LLSEL WITH !LLSEL
lfvpbSel(loFormSet)

loFormSet.lnSelRec   = IIF(llSel , loFormSet.lnSelRec + 1 , loFormSet.lnSelRec - 1)
loFormSet.lnUnSelRec = IIF(llSel , loFormSet.lnUnSelRec - 1 , loFormSet.lnUnSelRec + 1)

*No records was selected
IF loFormSet.lnSelRec = 0
  loFormSet.llenableinvert = .T.
  loFormSet.llenableselect = .T.
  loFormSet.llenableselectall = .T.
  loFormSet.llenableselectnone = .F.
  loFormSet.llenablerel = .F.
ELSE    && Else
  loFormSet.llenableselectnone = .T.
  loFormSet.llenablerel = .T.

  *-- All the records were selected
  IF loFormSet.lnUnSelRec = 0
    loFormSet.llenableselectall = .F.
  ELSE
    loFormSet.llenableselectall = .T.
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvpbSel
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/16/2015
*! Purpose   : Function to arange the push button select prompt
*!*************************************************************
*! Called from : lfvSelect() , lfvInvert() , The Browse [lcPickBrow]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T.
*!*************************************************************
FUNCTION lfvpbSel
PARAMETERS loFormSet
IF LLSEL
  loFormSet.lcCaptionSel = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_unSelect,loFormSet.GetHeaderText("LANG_unSelect",loFormSet.HeaderAlias))
ELSE
  loFormSet.lcCaptionSel = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select,loFormSet.GetHeaderText("LANG_Select",loFormSet.HeaderAlias))
ENDIF

RETURN .T.
*!*************************************************************
*! Name      : lfRelScr
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 09/16/2015
*! Purpose   : function to confirm release Packing List(s)
*!*************************************************************
FUNCTION lfRelScr
PARAMETERS loFormSet
  *** Message : "Are you sure you want to release the selected picking ticket(s) "
  ***           "                   < Yes >           < No >                    "
  IF gfModalGen('QRM32057B00006','DIALOG', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SelectedPL,loFormSet.GetHeaderText("LANG_SelectedPL",loFormSet.HeaderAlias))) = 1
    SELECT(loFormSet.lcTempFileHdr)
    LOCATE 
    *!* C202045,1 AHH 12/07/2017  Packing is not working [Begin][T20170628.0003]
    *!*	    SCAN  
    SCAN FOR LLSEL 
    *!* C202045,1 AHH 12/07/2017  Packing is not working [End][T20170628.0003]
      lfDelScrPl(loFormSet,EVALUATE(loFormSet.lcTempFileHdr+'.Pack_NO'),EVALUATE(loFormSet.lcTempFileHdr+'.Order'),;
      EVALUATE(loFormSet.lcTempFileHdr+'.Store'),EVALUATE(loFormSet.lcTempFileHdr+'.bill_ladg'))
    ENDSCAN  
  ENDIF    && End of IF
ENDFUNC
*!*************************************************************
*! Name      : lfDelScrPl
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/16/2015
*! Purpose   : Cancel the paramter Packing list
*!*************************************************************
FUNCTION lfDelScrPl
PARAMETERS loFormSet,lcPackNo,lcOrder,lcStore,lcBOL 
PRIVATE lnCurAlias,lcHdrTag,lcLinTag,llShiped
lnCurAlias = SELECT(0)
SELECT PACK_LIN
=gfSeek(lcPackNo,'PACK_LIN','PACK_LIN')
SELECT Ordline 
=gfSetOrder('Ordlinst')
*!*	SELECT PACK_LIN
*!*	SET FILTER TO
*!*	SET RELATION TO
*!*	SET KEY TO
*!*	SCAN REST WHILE PACK_NO+STR(LINE_NO,6)+STYLE+CPACKCOLOR =  lcPackNo
*!*	  SELECT ORDLINE
*!*	  IF gfSEEK('O'+lcOrder+lcStore+PACK_LIN.STYLE+STR(PACK_LIN.nOrdLineNo,6))
*!*	    SELECT OrdLine
*!*	    FOR lnCnt = 1 TO 8
*!*	      lcSze  =STR(lnCnt,1)
*!*	      gfREPLACE("nPck&lcSze  WITH IIF((PACK_LIN.Qty&lcSze) <> 0,MAX(nPck&lcSze  - (PACK_LIN.Qty&lcSze),0),0)")
*!*	    ENDFOR
*!*	      gfREPLACE("nPwght WITH IIF((PACK_LIN.Weight) <> 0, MAX(nPwght - (PACK_LIN.Weight),0),0)") 
*!*	  ENDIF
*!*	ENDSCAN
llEdiSys   = ('AS' $ oAriaApplication.CompanyInstalledModules)

IF llEdiSys AND gfSeek(lcPackNo,'Pack_HDR','Pack_HDR')
  =lfDelBolPL(loFormSet)
ENDIF

lcHdrTag = ORDER('Pack_Hdr')
SELECT PACK_HDR
*: B611107,1 MMT 01/26/2016 Pack_hdr is not deleted from custom Mass Packing list cancellation program[T20160120.0004][Start]
=gfSetOrder('PACK_HDR')
*: B611107,1 MMT 01/26/2016 Pack_hdr is not deleted from custom Mass Packing list cancellation program[T20160120.0004][End]
IF gfSEEK(lcPackNo)
  gfDELETE()
ENDIF
gfSetOrder(lcHdrTag)

lcLinTag = ORDER('Pack_Lin')
SELECT Pack_Lin
=gfSetOrder('PACKSTYLE')
SET RELATION TO
SET FILTER TO
SET KEY TO
IF gfSEEK(lcPackNo)
  SELECT Pack_Lin
  SCAN REST WHILE Pack_No+STR(No_Cart,4)+STYLE = lcPackNo
    SELECT ORDLINE
    IF gfSEEK('O'+lcOrder+lcStore+Pack_Lin.STYLE+STR(Pack_Lin.nOrdLineNo,6))
      SELECT OrdLine
      FOR lnCnt = 1 TO 8
        lcSze  =STR(lnCnt,1)
        gfREPLACE("nPck&lcSze  WITH IIF((PACK_LIN.Qty&lcSze) <> 0,MAX(nPck&lcSze  - (PACK_LIN.Qty&lcSze),0),0)")
      ENDFOR
      gfREPLACE("nPwght WITH IIF((PACK_LIN.Weight) <> 0, MAX(nPwght - (PACK_LIN.Weight),0),0)") 
      gfREPLACE("nPkPack WITH MAX(nPkPack  - Pack_Lin.nPackNo,0)")
      SELECT Pack_Lin
    ENDIF
    SELECT Pack_Lin
    gfDELETE()
  ENDSCAN
ENDIF
IF llEdiSys
  SELECT EDICRTSQ 
  gfSetOrder('PCKCRTSQ')
  lcSetDele = SET('DELETE')
  SET DELETE ON
  IF gfSEEK(lcPackNo)
    SELECT EDICRTSQ
    DELETE FOR pack_no+STR(cart_no,6) = lcPackNo
  ENDIF
  SET DELETE &lcSetDele

  IF !USED('ASN_SHIP')
    =gfOpenTable('Asn_Ship','Asn_Ship','SH','Asn_Ship')
  ENDIF 

  SELECT Asn_Ship
  =gfSeek('')
  DELETE FOR bol_no+pack_no+STR(cart_no,6)+asn_ver = lcBol+lcPackNo
ENDIF

SELECT Pack_Lin
gfSetOrder(lcLinTag)
=gfTableUpdate()

SELECT OrdLine
=gfTableUpdate()


SELECT Pack_hdr
=gfTableUpdate()

IF llEdiSys
  SELECT EDICRTSQ
  =gfTableUpdate()
  SELECT Asn_Ship
  =gfTableUpdate()
  SELECT BOL_LIN
  =gfTableUpdate()
  SELECT BOL_HDR
  =gfTableUpdate()
ENDIF
SELECT(lnCurAlias)
loFormSet.ChangeMode('S')

*!*************************************************************
*! Name      : lfCollectData
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/16/2015
*! Purpose   : Copy OG array to screen array
*!*************************************************************
FUNCTION lfCollectData
ACOPY(loogscroll.laogFxflt,laOgSelect)
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/16/2015
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol isReset
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm

SELECT Customer
SET ORDER TO Customer
LOCATE
*-- End of lfsrAcc.
*!*************************************************************
*! Name      : lfDelBol
*! Developer : Mariam Mazhar(MMT)
*! Date      : 09/16/2015
*! Purpose   : Delete BOL Line of the delete Packing List
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfDelBolPL
LPARAMETERS loFormSet
LOCAL lcBOLOrd
lcBOLOrd = ORDER('BOL_HDR')
SELECT BOL_HDR
gfSETORDER('BOL_HDR')


IF gfSEEK(PACK_HDR.Bill_Ladg) AND ;
    gfSEEK(PACK_HDR.Bill_Ladg+PACK_HDR.ORDER+PACK_HDR.Pack_No,'BOL_LIN','BOL_LIN')
   SELECT BOL_LIN 
   gfDELETE()
   SELECT BOL_HDR
   gfREPLACE("TOT_WGHT WITH TOT_WGHT - PACK_HDR.Tot_Wght ,;
          TOT_CART WITH TOT_CART - PACK_HDR.Tot_Cart ,TOT_PCS  WITH TOT_PCS  - PACK_HDR.Tot_Pcs")
ENDIF
IF !EMPTY(lcBOLOrd)
  SELECT BOL_HDR
  gfSETORDER(lcBOLOrd)
ENDIF