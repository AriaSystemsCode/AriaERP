**:***********************************************************************
*:  Program file : ARINVDR.PRG
*:  Program desc.: Generate AR invoices from Dropship invoices for BBC10
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar[MMT]
*:           Date: 09/15/2021
*:      Reference: C202430,1
*:************************************************************************
PUBLIC ARRAY laFxFltArr[1]
laFxFltArr = ''
lcExpr = gfOpGrid('ARINVDRP' , .T.)&&,.F.,.F.,.T.,.T.)

IF TYPE('lcExpr') <> 'U' AND lcExpr <> '.F.'
  =gfOpenTable('SMART_INVOICE_HEADER','SMINVACS')

  *!*	"INVHDR.RECDATE"  && Receiving Date
  ldRECStartDate = {}
  ldRECEndDate   = {}
  llDateRecSelect = .F.
  lnStDatePos = ASCAN(laFxFltArr,"INVHDR.RECDATE")
  IF lnStDatePos > 0
    lnStDatePos= ASUBSCRIPT(laFxFltArr,lnStDatePos,1)
    llDateRecSelect = !EMPTY(laFxFltArr[lnStDatePos,6])
    IF llDateRecSelect 
      ldRECStartDate = CTOD(SUBSTR(laFxFltArr[lnStDatePos,6],1,10))
      ldRECEndDate   = CTOD(SUBSTR(laFxFltArr[lnStDatePos,6],12,21))
      IF SUBSTR(laFxFltArr[lnStDatePos,6],1,1) = "|"
        ldRECStartDate = {}
        ldRECEndDate   = CTOD(SUBSTR(laFxFltArr[lnStDatePos,6],2,11))
      ENDIF
    ENDIF
  ENDIF
*!*    *!*	"INVHDR.INVDATE" && AR Invoice Date
*!*    ldInvStartDate = {}
*!*    ldInvEndDate   = {}
*!*    llDateInvSelect = .F.
*!*    lnStDatePos = ASCAN(laFxFltArr,"INVHDR.INVDATE")
*!*    IF lnStDatePos > 0
*!*      lnStDatePos = ASUBSCRIPT(laFxFltArr,lnStDatePos,1)
*!*      llDateInvSelect = !EMPTY(laFxFltArr[lnStDatePos,6])
*!*      IF llDateInvSelect 
*!*        ldInvStartDate = CTOD(SUBSTR(laFxFltArr[lnStDatePos,6],1,10))
*!*        ldInvEndDate   = CTOD(SUBSTR(laFxFltArr[lnStDatePos,6],12,21))
*!*        IF SUBSTR(laFxFltArr[lnStDatePos,6],1,1) = "|"
*!*          ldInvStartDate = {}
*!*          ldInvEndDate   = CTOD(SUBSTR(laFxFltArr[lnStDatePos,6],2,11))
*!*        ENDIF
*!*      ENDIF
*!*    ENDIF
*!*    *!*	"INVHDR.PAYDATE"  && Pay
*!*    ldPayStartDate = {}
*!*    ldPayEndDate   = {}
*!*    llDatePaySelect = .F.
*!*    lnStDatePos = ASCAN(laFxFltArr,"INVHDR.PAYDATE")
*!*    IF lnStDatePos > 0
*!*      lnStDatePos = ASUBSCRIPT(laFxFltArr,lnStDatePos,1)
*!*      llDatePaySelect = !EMPTY(laFxFltArr[lnStDatePos,6])
*!*      IF llDatePaySelect 
*!*        ldPayStartDate = CTOD(SUBSTR(laFxFltArr[lnStDatePos,6],1,10))
*!*        ldPayEndDate   = CTOD(SUBSTR(laFxFltArr[lnStDatePos,6],12,21))
*!*        IF SUBSTR(laFxFltArr[lnStDatePos,6],1,1) = "|"
*!*          ldPayStartDate = {}
*!*          ldPayEndDate   = CTOD(SUBSTR(laFxFltArr[lnStDatePos,6],2,11))
*!*        ENDIF
*!*      ENDIF
*!*    ENDIF
  *!*	"INVHDR.DATE" && Drop
  ldDropStartDate = {}
  ldDropEndDate   = {}
  llDateDropSelect = .F.
  lnStDatePos = ASCAN(laFxFltArr,"INVHDR.DATE")
  IF lnStDatePos > 0
    lnStDatePos = ASUBSCRIPT(laFxFltArr,lnStDatePos,1)
    llDateDropSelect = !EMPTY(laFxFltArr[lnStDatePos,6])
    IF llDateDropSelect 
      ldDropStartDate = CTOD(SUBSTR(laFxFltArr[lnStDatePos,6],1,10))
      ldDropEndDate   = CTOD(SUBSTR(laFxFltArr[lnStDatePos,6],12,21))
      IF SUBSTR(laFxFltArr[lnStDatePos,6],1,1) = "|"
        ldDropStartDate = {}
        ldDropEndDate   = CTOD(SUBSTR(laFxFltArr[lnStDatePos,6],2,11))
      ENDIF
    ENDIF
  ENDIF
  *!*	"CUSTOMER.ACCOUNT
  llAccSelected = .F.
  lcAccSel = ''
  lnPosAcc = ASCAN(laFxFltArr,"CUSTOMER.ACCOUNT")
  IF lnPosAcc > 0
    lnPosAcc = ASUBSCRIPT(laFxFltArr,lnPosAcc,1)
    lcAccSel =IIF(!EMPTY(laFxFltArr[lnPosAcc,6]),laFxFltArr[lnPosAcc,6],'')
    IF !EMPTY(lcAccSel) AND USED(lcAccSel)
      SELECT(lcAccSel)
      LOCATE
      IF !EOF()
        llAccSelected = .T.
      ENDIF
    ENDIF
  ENDIF
  *!*	"INVHDR.INVOICE"
  llInvSelected = .F.
  lcInvSel = ''
  lnPosInv = ASCAN(laFxFltArr,"INVHDR.INVOICE")
  IF lnPosInv > 0
    lnPosInv = ASUBSCRIPT(laFxFltArr,lnPosInv ,1)
    lcInvSel =IIF(!EMPTY(laFxFltArr[lnPosInv ,6]),laFxFltArr[lnPosInv ,6],'')
    IF !EMPTY(lcInvSel ) AND USED(lcInvSel )
      SELECT(lcInvSel )
      LOCATE
      IF !EOF()
        llInvSelected = .T.
      ENDIF
    ENDIF
  ENDIF

  *!*	"INVHDR.BATCH"
  llBatSelected = .F.
  lcBatSel = ''
  lnPosBat = ASCAN(laFxFltArr,"INVHDR.BATCH")
  IF lnPosBat > 0
    lnPosBat = ASUBSCRIPT(laFxFltArr,lnPosBat ,1)
    lcBatSel =IIF(!EMPTY(laFxFltArr[lnPosBat ,6]),laFxFltArr[lnPosBat ,6],'')
    IF !EMPTY(lcBatSel ) AND USED(lcBatSel )
      SELECT(lcBatSel )
      LOCATE
      IF !EOF()
        llBatSelected = .T.
      ENDIF
    ENDIF
  ENDIF
  *lcRpInvcd
  *lcRpPaid
  SELECT SMART_INVOICE_HEADER
  SELECT * FROM SMART_INVOICE_HEADER WHERE .F. INTO CURSOR 'TempInvH' READWRITE
  DO CASE 
    CASE llBatSelected 
      SELECT SMART_INVOICE_HEADER
      =GFSETORDER('SMINVBTH')
      SELECT (lcBatSel)
      LOCATE
      SCAN
        IF gfSeek(&lcBatSel..S_batch_no,'SMART_INVOICE_HEADER')
          SELECT SMART_INVOICE_HEADER
          SCAN REST WHILE S_BATCH_NO+SMART_INVOICE_NO =&lcBatSel..S_batch_no FOR ;
            IIF(llInvSelected,SEEK(SMART_INVOICE_HEADER.SMART_INVOICE_NO,lcInvSel),.T.) AND ;
            IIF(llAccSelected ,SEEK(SMART_INVOICE_HEADER.Account,lcAccSel),.T.) AND ;
            IIF(llDateDropSelect, BETWEEN(SMART_INVOICE_HEADER.DROP_SHIP_DATE,ldDropStartDate,ldDropEndDate),.T.) AND;
            IIF(llDateRecSelect, BETWEEN(SMART_INVOICE_HEADER.REC_DATE,ldRECStartDate,ldRECEndDate),.T.) AND;
            EMPTY(Invoice)
            SCATTER MEMO memVA
            INSERT INTO 'TempInvH' FROM MEMVAR 
          ENDSCAN 
        ENDIF
      ENDSCAN 
    CASE llInvSelected
      SELECT SMART_INVOICE_HEADER
      =GFSETORDER('SMINVHDR')
      SELECT (lcInvSel)
      LOCATE
      SCAN
        IF gfSeek(&lcInvSel..SMART_INVOICE_NO,'SMART_INVOICE_HEADER')
          SELECT SMART_INVOICE_HEADER
          IF IIF(llAccSelected ,SEEK(SMART_INVOICE_HEADER.Account,lcAccSel),.T.) AND ;
            IIF(llDateDropSelect, BETWEEN(SMART_INVOICE_HEADER.DROP_SHIP_DATE,ldDropStartDate,ldDropEndDate),.T.) AND;
            IIF(llDateRecSelect, BETWEEN(SMART_INVOICE_HEADER.REC_DATE,ldRECStartDate,ldRECEndDate),.T.) AND;
            EMPTY(Invoice)
            SCATTER MEMO memVA
            INSERT INTO 'TempInvH' FROM MEMVAR 
          ENDIF
        ENDIF
      ENDSCAN 
    CASE llAccSelected 
      SELECT SMART_INVOICE_HEADER
      =GFSETORDER('SMINVACS')
      SELECT (lcAccSel)
      LOCATE
      SCAN
        IF gfSeek(&lcAccSel..Account,'SMART_INVOICE_HEADER')
          SELECT SMART_INVOICE_HEADER
          SCAN REST WHILE ACCOUNT+STORE+SMART_INVOICE_NO=&lcAccSel..Account FOR ;
            IIF(llDateDropSelect, BETWEEN(SMART_INVOICE_HEADER.DROP_SHIP_DATE,ldDropStartDate,ldDropEndDate),.T.) AND;
            IIF(llDateRecSelect, BETWEEN(SMART_INVOICE_HEADER.REC_DATE,ldRECStartDate,ldRECEndDate),.T.) AND;
            EMPTY(Invoice)
            SCATTER MEMO memVA
            INSERT INTO 'TempInvH' FROM MEMVAR 
          ENDSCAN 
        ENDIF
      ENDSCAN     
   OTHERWISE  
     SELECT SMART_INVOICE_HEADER
     =gfSeek('')
     LOCATE 
     SCAN FOR ;
        IIF(llDateDropSelect, BETWEEN(SMART_INVOICE_HEADER.DROP_SHIP_DATE,ldDropStartDate,ldDropEndDate),.T.) AND;
        IIF(llDateRecSelect, BETWEEN(SMART_INVOICE_HEADER.REC_DATE,ldRECStartDate,ldRECEndDate),.T.) AND;
        EMPTY(Invoice)
        SCATTER MEMO memVA
        INSERT INTO 'TempInvH' FROM MEMVAR 
     ENDSCAN 
  ENDCASE 
  SELECT  'TempInvH'
  LOCATE
  IF !EOF()
    =gfOpenTable('SMART_INVOICE_LINES','SMINVLINE','SH')
    =gfOpenTable('STYLE','STYLE','SH')
    SELECT *, SPACE(6) as CDIVISION, SPACE(6) as SEASON FROM SMART_INVOICE_LINES WHERE .F. INTO CURSOR 'TempInvL' READWRITE
    SELECT  'TempInvH'    
    SCAN
      =gfSeek(TempInvH.SMART_INVOICE_NO,'SMART_INVOICE_LINES')
      SELECT SMART_INVOICE_LINES
      SCAN REST WHILE SMART_INVOICE_NO+STR(LINENO,6) =   TempInvH.SMART_INVOICE_NO FOR TotQty > 0  &&AND (EMPTY(CTRCODE) OR ISNULL(cTrCode))
        SCATTER MEMO MEMVAR 
        =gfSeek(m.Style,'STYLE','STYLE')
        m.CDIVISION = Style.CDIVISION 
        m.SEASON = Style.SEASON 
        INSERT INTO TempInvL FROM MEMVAR
      ENDSCAN                                                                                        
    ENDSCAN 
    SELECT TempInvL 
    LOCATE 
    IF !EOF()
      CREATE CURSOR CRDIRECT (INVOICE C(6))
      SELECT CRDIRECT
      INDEX on INVOICE TAG CRDIRECT
    
      lcCurProc = SET("Procedure")
      lcCurProc = ''+ADDBS(UPPER(oAriaApplication.clientapplicationhome))+'AR\GFMODALGEN.FXP'+','+lcCurProc 
      SET PROCEDURE TO &lcCurProc.

      lfCreateDirectInvoice('TempInvL')
   
      IF (UPPER(ADDBS(oAriaApplication.clientapplicationhome))+'AR\GFMODALGEN.FXP') $ UPPER(SET("Procedure"))
        RELEASE PROCEDURE (UPPER(ADDBS(oAriaApplication.clientapplicationhome))+'AR\GFMODALGEN.FXP')
      ENDIF
    
      SELECT CRDIRECT
      LOCATE 
      IF !Eof()
        LOCATE
        lcInvNOst = INVOICE
        GO BOTTOM 
        lcInvNOEnd = INVOICE    
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(RECCOUNT('CRDIRECT')>1,"AR invoices range from "+lcInvNOst +" to "+lcInvNOEnd +" has been generated",;
        "AR Invoice#"+lcInvNOEnd+" has been generated"))
        RETURN
      ELSE
        =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No AR invoices generated.')
        RETURN
      ENDIF  
    ELSE
      =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No Dropship invoices matched the selected criteria.')
      RETURN
    ENDIF
  ELSE
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No Dropship invoices matched the selected criteria.')
    RETURN
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar
*! Date      : 09/15/2021
*! Purpose   : Option Grid when function
*!*************************************************************
FUNCTION lfwRepWhen
=gfOpenTable('SMART_INVOICE_HEADER','SMINVACS')
IF llFirstTIme
  SELECT SMART_INVOICE_HEADER
  =gfSeek('')
  SELECT DISTINCT S_BATCH_NO FROM SMART_INVOICE_HEADER WHERE;
         !ISNULL(S_BATCH_NO) ORDER BY S_Batch_no INTO CURSOR 'Batches'
  llFirstTIme = .F.
ENDIF
*!*************************************************************
*! Name      : lfCreatExp
*! Developer : Mariam Mazhar
*! Date      : 09/15/2021
*! Purpose   : Copy Fixed filter array to public array
*!*************************************************************
FUNCTION lfCreatExp
DIMENSION laFxFltArr[ALEN(loogscroll.laogfxflt,1),ALEN(loogscroll.laogfxflt,2)]
ACOPY(loogscroll.laogfxflt,laFxFltArr)


*!*************************************************************
*! Name      : lfCreateDirectInvoice
*! Developer : Mariam Mazhar
*! Date      : 09/15/2021
*! Purpose   : Create direct invoice
*!*************************************************************
FUNCTION lfCreateDirectInvoice
PARAMETERS lcTempLine

=gfOpenTable('WAREHOUS','WAREHOUS')
SELECT WAREHOUS
=gfSeek('')
LOCATE FOR ldefware
lcWareHous = WAREHOUS.cwarecode
 lcOldhand = ON("Error")
 ON ERROR x=10
 
 SELECT Distinct Account,Store,CDIVISION,SEASON  FROM (lcTempLine) WHERE totqty > 0  INTO CURSOR 'INVStore'
 *XX1
 *MMT
 *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][End]
 loDInv =''
 IF TYPE('loDInv') <> "O"
     DO FORM (oAriaApplication.ScreenHome+"ar\ardinv.scx") NOSHOW NAME loDInv LINKED
 ENDIF
 *MMT
 lnCountLine = 0
 *MMT  
 SELECT 'INVStore'
 SCAN 
   lcAccountID = INVStore.Account
   lnCountLine = lnCountLine + 1
   Wait window 'Create Direct Invoice...'+Allt(STR((lnCountLine /Reccount('INVStore'))*100,3))+"%" nowait 
   lnChoice = 1  
   loDInv.changemode ('A')
   loDInv.DefaultInvoiceDate = oAriaApplication.SystemDate 
   loDInv.DefaultPostingDate = oAriaApplication.SystemDate
   loDInv.laSetups[18,2] = 'N'
   loDInv.mCreateTempfiles
   loDInv.DefaultWarecode = 'BOL'&&lcWareHous
   WITH loDInv.AriaForm1.AriaPageframe1.Page2.InvoiceEditRegion1
     STORE 0 TO .TaxDueAmount, .Merchandisetax, .TotalCartons
   ENDWITH
   loDInv.DefaultSeason =  INVStore.SEASON &&'*'
   loDInv.DefaultDivision = INVStore.CDIVISION &&loDInv.AriaForm1.AriaPageFrame1.Page1.cboDivision.CodeDefaultValue
   loDInv.ariaform1.keyAccount.keytextbox.oldValue = SPACE(5)
   loDInv.ariaform1.keyAccount.keytextbox.VALUE = INVStore.Account
   loDInv.ariaform1.keyAccount.keytextbox.VALID
   IF !EMPTY(INVStore.Store)
     loDInv.ariaform1.keySTORE.keytextbox.oldValue = SPACE(5)
     loDInv.ariaform1.keySTORE.keytextbox.VALUE = INVStore.Store
     loDInv.ariaform1.keySTORE.keytextbox.VALID
     =SEEK('S'+lcAccountID +INVStore.Store,'CUSTOMER','CUSTOMER')
   ELSE
     =SEEK('M'+lcAccountID,'CUSTOMER','CUSTOMER')        
   ENDIF  
   *XX
   IF INVStore.CDIVISION = 'BLANKM'
     loDInv.AriaForm1.AriaPageFrame1.Page1.keySalesRep1.KeyTextBox.value = '640'
     loDInv.AriaForm1.AriaPageFrame1.Page1.spnComm1.value = 2.5
   ENDIF
   IF INVStore.CDIVISION = 'BLANKW'
     loDInv.AriaForm1.AriaPageFrame1.Page1.keySalesRep1.KeyTextBox.value = '626'
     loDInv.AriaForm1.AriaPageFrame1.Page1.spnComm1.value = 2.5
   ENDIF
   *XX
   lnDisc = Customer.Disc
   SELECT(lcTempLine)
   lnTotAmt = 0
   SCAN FOR Account=INVStore.Account  AND Store = INVStore.Store AND totqty > 0 AND CDIVISION = INVStore.CDIVISION AND Season = INVStore.Season
     lnTotAmt = lnTotAmt + &lcTempLine..PRICE * &lcTempLine..TotQty
     lnRetPriceValue = &lcTempLine..PRICE
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.mResetControlSource ()
     STORE .T. TO loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.llNewline,loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.llAddLine
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.ENABLED = .T.
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.TxtItem.VALUE = &lcTempLine..STYLE
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.VALID(.T.,0,&lcTempLine..STYLE,SPACE(19),&lcTempLine..STYLE)
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.mResetControlSource ()

     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.OldValue  = 0
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.VALUE  = &lcTempLine..PRICE
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.LOSTFOCUS()
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.oldValue = 0
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.VALUE = 0
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.LOSTFOCUS()
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty1.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty2.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty3.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty4.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty5.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty6.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty7.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty8.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtTotQty.CONTROLSOURCE = ''
     FOR lnX= 1 TO 8
       lcX = STR(lnX,1)
       IF  &lcTempLine..Qty&lcX. > 0
         loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty&lcX..VALUE  =  &lcTempLine..Qty&lcX.
         loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty&lcX..VALID
       ENDIF
     ENDFOR
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.LOSTFOCUS()
      =gfAdd_Info(loDInv.lcInvLine,loDInv)
   ENDSCAN
   loDInv.AriaForm1.AriaPageFrame1.Page3.ACTIVATE()
   lnChoice = 2
   DIMENSION  laInv[1]
   laInv[1] = ''  
   IF lnTotAmt <> 0
     loDInv.HIDE()
     loDInv.SaveFiles(.F.)
   ENDIF  
   IF !EMPTY(laInv[1])
     INSERT INTO  'CRDIRECT' VALUES (laInv[1])
     SELECT(lcTempLine)
     LOCATE
     SCAN FOR Account = INVStore.Account AND  Store = INVStore.Store AND totqty > 0 AND CDIVISION = INVStore.CDIVISION AND Season = INVStore.Season&&and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv')
*!*          lnConnectionHandle = 0
*!*          lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_LINES Set cTrCode ='"+laInv[1]+;
*!*                   "'  Where SMART_INVOICE_NO = '"+&lcTempLine..SMART_INVOICE_NO +"' AND STYLE ='"+&lcTempLine..STYLE+"' AND TotQty > 0  AND (CTRCODE ='' OR ISNULL(CTRCODE,'S')='S')",'SMRTSHPLINUP' , ;
*!*                   "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"),.T.,@lnConnectionHandle)
*!*          If lnResult<>1
*!*            oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
*!*            Wait Window 'SMART_INVOICE_LINES'+'.SqlRun()'
*!*            Return .F.
*!*          ELSE
*!*            SQLCOMMIT(lnConnectionHandle)  
*!*          ENDIF
        lnConnectionHandleLine = 0
        lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_HEADER Set INVOICE ='"+laInv[1]+"',"+;
                 " INVDATE='"+DTOC(oAriaApplication.SystemDate)+"'"+;
                 "  Where SMART_INVOICE_NO = '"+&lcTempLine..SMART_INVOICE_NO +"'",'SMRTSHPLINUP' , ;
                 "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"),.T.,@lnConnectionHandleLine)
        If lnResult<>1
          oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
          Wait Window 'SMART_INVOICE_HEADER'+'.SqlRun()'
          Return .F.
        ELSE
          SQLCOMMIT(lnConnectionHandleLine) 
          SQLDISCONNECT(lnConnectionHandleLine)         
        ENDIF
     ENDSCAN 
   ENDIF  
 ENDSCAN 
 IF TYPE('loDInv') ='O'
   loDInv.Release()
   loDInv = Null
 ENDIF 
On Error &lcOldhand.

