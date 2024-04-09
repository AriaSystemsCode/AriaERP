*!**************************************************************************
*! Name      : IKEMAIN.PRG
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/12/2013
*! Purpose   : IKEDDI Custom Process Program. C201577 [T20130312.0031]
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       
*!**************************************************************************
*! Modifications
*B610342,1 MMT 05/29/2013 Add trigger to Material vendor reference screen to open in edit mode[T20130312.0031]
*C201583,1 MMT 06/16/2013 Add trigger to copy reference field from header to distribution lines note[T20130305.0031]
*C201589,1 MMT 08/07/2013 Add trigger to Payable invoice screen to show notes in Distribution lines grid[T20130305.0031]
*C201743,1 MMT 12/02/2015 Add trigger to complete Inter-location PO related Picking ticket[T20151014.0017]
*C201748,1 MMT 12/21/2015 Add triggers to filter stores by classification and ranking[T20151014.0017]
*B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017]
*B611118,1 MMT 02/29/2016 Issue#4 - Cannot link order lines has no prepack to bulk has prepack[T20151014.0017]
*B611130,1 MMT 03/23/2016 Issue#6 Use Pack_ID while linking Multi-store order with Bulk order [T20151014.0017]
*B611130,2 MMT 03/24/2016 Issue#6 Linking orders to bulk even if it is not multi-store order[T20151014.0017]
*B611130,3 MMT 03/24/2016 Issue#6 Consider preppack within pack Linking orders to bulk even if it is not multi-store order[T20151014.0017]
*E303748,1 MAA 01/30/2017 Add trigger to copy styles UPCs to Order notes[P20160426.0001]
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name      : lfCHNGKEYV
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/12/2013
*! Purpose   : Change the key value in Material Vendor reference screen while saving
*!**************************************************************************
FUNCTION lfCHNGKEYV
LOCAL lcKeyValue
lcKeyValue = Vendor +;
             IIF(!EMPTY(OldFbCl), PADR(SUBSTR(OldFbCl, 1, loFormSet.lnMajLen), loFormSet.lnFabLenInMATL) +  SUBSTR(OldFbCl, loFormSet.lnMajLen + 2,6),fabric + Color)+;
             IIF(TYPE('OldVenFab')<>'U' AND !EMPTY(ALLTRIM(OldVenFab)),OldVenFab, cvenfab)+;
             IIF(TYPE('OldVenClr')<>'U' AND !EMPTY(ALLTRIM(OldVenClr)),OldVenClr, cvencolr)
RETURN lcKeyValue

*!**************************************************************************
*! Name      : lfUPTMPSTRU
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/12/2013
*! Purpose   : Add 2 new fields to the temp. file to hold the old values of supplier item and color
*!**************************************************************************
FUNCTION lfUPTMPSTRU
loFormSet.AriaForm1.grdLines.RecordSource = ''
SELECT (loFormSet.lcTmpFile)
=CURSORSETPROP("Buffering", 1)
ALTER table (loFormSet.lcTmpFile) ADD COLUMN OldVenFab C(10)
ALTER table (loFormSet.lcTmpFile) ADD COLUMN OldVenClr C(10)
SELECT (loFormSet.lcTmpFile)
=CURSORSETPROP("Buffering", 5)
SET ORDER TO MATCOL
lfAdjustGrid(loFormSet)
*!**************************************************************************
*! Name      : lfUPVENOLDF
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/12/2013
*! Purpose   : Update the Old Supplier Fabric 
*!**************************************************************************
FUNCTION lfUPVENOLDF
IF TYPE('OldVenFab')<>'U'
  REPLACE OldVenFab WITH cvenfab IN (loFormSet.lcTmpFile)
ENDIF
*!**************************************************************************
*! Name      : lfUPVENOLDC
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/12/2013
*! Purpose   : Update the Old Supplier Color
*!**************************************************************************
FUNCTION lfUPVENOLDC
IF TYPE('OldVenClr')<>'U'
  REPLACE OldVenClr WITH cvencolr IN (loFormSet.lcTmpFile)
ENDIF  
*!**************************************************************************
*! Name      : lfUPDMASTDT
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/12/2013
*! Purpose   : Update Vendor Material Reference detail table
*!**************************************************************************
FUNCTION lfUPDMASTDT

IF !USED('VENDMATL_UP')
  =gfOpenTable('VENDMATL','MATCOL','SH','VENDMATL_UP')
ENDIF
SELECT (loFormSet.lcTmpFile)
LOCATE 
SCAN FOR (TYPE('OldVenFab')<>'U' AND !EMPTY(ALLTRIM(OldVenFab))) OR (TYPE('OldVenClr')<>'U' AND !EMPTY(ALLTRIM(OldVenClr)))
  lcKeyValue = Vendor +;
             IIF(!EMPTY(OldFbCl), PADR(SUBSTR(OldFbCl, 1, loFormSet.lnMajLen), loFormSet.lnFabLenInMATL) +  SUBSTR(OldFbCl, loFormSet.lnMajLen + 2,6),fabric + Color)+;
             IIF(TYPE('OldVenFab')<>'U' AND !EMPTY(ALLTRIM(OldVenFab)),OldVenFab, cvenfab)+;
             IIF(TYPE('OldVenClr')<>'U' AND !EMPTY(ALLTRIM(OldVenClr)),OldVenClr, cvencolr)
  IF gfSeek(lcKeyValue,'VENDMATL_UP','MATCOL') 
    SELECT 'VENDMATL_UP'
    =gfDelete()
  ENDIF  
  SELECT (loFormSet.lcTmpFile)
ENDSCAN 
SELECT 'VENDMATL_UP'
=gfTableUpdate()
=gfCloseTable('VENDMATL_UP')
*!**************************************************************************
*! Name      : lfUPDSTYCONT
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/12/2013
*! Purpose   : ask user if he wants to update the item table Contents field
*!**************************************************************************
FUNCTION lfUPDSTYCONT
lnAliasSel = SELECT()
lcTmpFileLn = loFormSet.ariaForm1.mainworkorder.cPoLine
lnMajLen = LEN(gfItemMask("PM",'0002'))
lcStyleMaj = SUBSTR(&lcTmpFileLn..Style,1,lnMajLen)
IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Do you like to update the Fabric master contents') <> 1
  RETURN 
ENDIF 
IF TYPE('loFormSet.cContent') <> 'U' 
  IF !SEEK(lcStyleMaj,loFormSet.cContent)
    INSERT INTO (loFormSet.cContent) VALUES (lcStyleMaj,loFormSet.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.txtContent.Value)
  ELSE
    REPLACE CITEMFLD3 WITH loFormSet.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.txtContent.Value IN (loFormSet.cContent)
  ENDIF  
ENDIF  
SELECT(lnAliasSel)
*!**************************************************************************
*! Name      : lfADDCONFLD
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/12/2013
*! Purpose   : Create cursor to hold the contents field of each item to update it in Item table
*!**************************************************************************
FUNCTION lfADDCONFLD
lnAliasSel = SELECT()
lnMajLen = LEN(gfItemMask("PM",'0002'))
IF TYPE('loFormSet.cContent') = 'U'
 loFormSet.AddProperty('cContent',gfTempName())
ENDIF
CREATE CURSOR (loFormSet.cContent) (Style C(lnMajLen),CITEMFLD3 C(60))
SELECT (loFormSet.cContent)
INDEX on STYLE TAG (loFormSet.cContent)
SELECT(lnAliasSel)
*!**************************************************************************
*! Name      : lfGETCONTVAL
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/12/2013
*! Purpose   : get the updated contents field of item 
*!**************************************************************************
FUNCTION lfGETCONTVAL
lnAliasSel = SELECT()

lcTmpFileLn = loFormSet.ariaForm1.mainworkorder.cPoLine
lnMajLen = LEN(gfItemMask("PM",'0002'))
lcStyleMaj = lcStyMaj&&SUBSTR(&lcTmpFileLn..Style,1,lnMajLen)
lcContentV =  m.CITEMFLD3
IF TYPE('loFormSet.cContent') <> 'U' AND SEEK(lcStyleMaj,loFormSet.cContent)
  lcContentV =  EVALUATE(loFormSet.cContent+'.CITEMFLD3')
ENDIF
loFormSet.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.txtContent.Value = lcContentV
SELECT (lcTmpFileLn)
*REPLACE CITEMFLD3 WITH  lcContentV IN (lcTmpFileLn)
m.CITEMFLD3 =   lcContentV 
SELECT(lnAliasSel)
*!**************************************************************************
*! Name      : lfSAVECONT
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/12/2013
*! Purpose   : Update the contents field in the item table
*!**************************************************************************
FUNCTION lfSAVECONT
IF (TYPE('loFormSet.cContent') ='U')
  RETURN 
ENDIF 
lnAliasSel = SELECT()
lnMajLen = LEN(gfItemMask("PM",'0002'))
IF !USED('ITEM_UP')
  =gfOpenTable('ITEM','STYLE','SH','ITEM_UP')
ENDIF
SELECT (loFormSet.cContent)
lcConFile = loFormSet.cContent
LOCATE 
SCAN 
  lcStyMaj=  SUBSTR(EVALUATE(loFormSet.cContent+'.Style'),1,lnMajLen)
  SELECT ITEM_UP
  =gfSeek('0002'+lcStyMaj,'ITEM_UP','STYLE')
  SCAN REST WHILE cinvtype+STYLE = '0002'+lcStyMaj
    =gfReplace('CITEMFLD3 WITH &lcConFile..CITEMFLD3') 
  ENDSCAN 
ENDSCAN 
SELECT ITEM_UP
=gfTableUpdate()
=gfCloseTable('ITEM_UP')
SELECT(lnAliasSel)

*B610342,1 MMT 05/29/2013 Add trigger to Material vendor reference screen to open in edit mode[T20130312.0031][Start]
*!**************************************************************************
*! Name      : lfSCRCHGMOD  
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/29/2013
*! Purpose   : vendor reference screen open in edit mode
*!**************************************************************************
FUNCTION lfSCRCHGMOD  
loFormSet.changemode ('E')
loFormSet.ariaForm1.cmdNew.Click 
loFormSet.lValidItem = .t.
loFormSet.ariaForm1.kbItem.txtItem.value = lcFabricValue
loFormSet.ariaForm1.kbItem.txtItem.Valid()
loFormSet.ariaForm1.txtSuppCode.SetFocus 
*B610342,1 MMT 05/29/2013 Add trigger to Material vendor reference screen to open in edit mode[T20130312.0031][End]

*C201583,1 MMT 06/16/2013 Add trigger to copy reference field from header to distribution lines note[T20130305.0031][Start]
*!**************************************************************************
*! Name      : lfCOPYREF
*! Developer : Mariam Mazhar[MMT]
*! Date      : 06/16/2013
*! Purpose   : copy reference field from header to distribution lines note
*!**************************************************************************
FUNCTION lfCOPYREF
IF EMPTY(ALLTRIM(EVALUATE(LOFORMSET.LCTMPDIST+'.mApDist')))
  REPLACE mApDist WITH ALLTRIM(loFormSet.AriaForm1.pgfpayInv.pgHead.txtInvRef.value) IN (LOFORMSET.LCTMPDIST)
ENDIF
*C201583,1 MMT 06/16/2013 Add trigger to copy reference field from header to distribution lines note[T20130305.0031][End]

*C201589,1 MMT 08/07/2013 Add trigger to Payable invoice screen to show notes in Distribution lines grid[T20130305.0031][Start]
*!**************************************************************************
*! Name      : lfADDNOTEGRD
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/07/2013
*! Purpose   : Add trigger to Payable invoice screen to show notes in Distribution lines grid
*!**************************************************************************
FUNCTION lfADDNOTEGRD
WITH loFormSet.ariaform1.pgfpayInv.pgDist.grdDist
  .columncount = .columncount +1
  lcCntCol = allt(STR(.columncount))
  .column&lcCntCol..Width = 100
  .column&lcCntCol..ControlSource = "Substr(mApdist,1,100)"
  .column&lcCntCol..Header1.Caption = 'Notes'    
ENDWITH
*C201589,1 MMT 08/07/2013 Add trigger to Payable invoice screen to show notes in Distribution lines grid[T20130305.0031][End]
*C201743,1 MMT 12/02/2015 Add trigger to complete Inter-location PO related Picking ticket[T20151014.0017][Start]
*!**************************************************************************
*! Name      : lfCOMPPIKTKT
*! Developer : Mariam Mazhar[MMT]
*! Date      : 12/02/2015 
*! Purpose   : Add trigger to complete Inter-location PO related Picking ticket
*!**************************************************************************
FUNCTION lfCOMPPIKTKT
IF lcRecvType ='O' AND !EMPTY(&lcMastPoHd..CPIKTKT)
  lcPickTicket = &lcMastPoHd..CPIKTKT
  lnSelect = SELECT(0)
  IF !USED("PIKTKT_UPD")
    =gfOpenTable("PIKTKT",'PIKTKT',"SH","PIKTKT_UPD")
  ENDIF
  IF !USED("Ordline_UPD")
    =gfOpenTable("Ordline",'Ordline',"SH","Ordline_UPD")
  ENDIF
  IF !USED("PIKLINE_UPD")
    =gfOpenTable("PIKLINE",'PIKLINE',"SH","PIKLINE_UPD")
  ENDIF
  IF gfSeek(lcPickTicket,'PIKTKT_UPD','PIKTKT') AND PIKTKT_UPD.Status <> 'C'
    SELECT Ordline_UPD
    =gfSeek('O'+PIKTKT_UPD.Order,'Ordline_UPD','Ordline') 
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+PIKTKT_UPD.Order FOR Piktkt = lcPickTicket
      SCATTER MEMO MEMVAR 
      INSERT INTO 'PIKLINE_UPD' FROM MEMVAR
      SELECT 'PIKLINE_UPD'
      =gfReplace('')
      SELECT Ordline_UPD
      REPLACE PIKTKT WITH '',;
              Picked WITH .F.,;
              PikDate with {} ,;
              Pik1 WITH 0,;
              Pik2 WITH 0,;
              Pik3 WITH 0,;
              Pik4 WITH 0,;
              Pik5 WITH 0,;
              Pik6 WITH 0,;
              Pik7 WITH 0,;
              Pik8 WITH 0,;
              TotPik WITH 0 
      =gfReplace('')                                                                                               
    ENDSCAN 
    SELECT 'PIKLINE_UPD'
    =gfTableUpdate()
    SELECT Ordline_UPD
    =gfTableUpdate()
    SELECT PIKTKT_UPD
    REPLACE Status WITH 'C' 
    =gfReplace('')                                                                                               
    =gfTableUpdate()
  ENDIF
  SELECT(lnSelect)
ENDIF
*!**************************************************************************
*! Name      : lfASSGNBULK
*! Developer : Mariam Mazhar[MMT]
*! Date      : 12/07/2015 
*! Purpose   : Add trigger to assign bulk order 
*!**************************************************************************
FUNCTION lfASSGNBULK
lcBrFields = [ORDER:H="Bulk Order#",Start :H="Start Date",Entered :H="Entered Date",Complete:H="Complete Date"]
lcHdrFile = loFormSet.oFormEnvironment.lcOrdHdr
lcLineFile = loFormSet.oFormEnvironment.lcOrdLine
lcOrgnStatus = OrdHdr.STATUS
lcOrdAccount = &lcHdrFile..Account
*B611130,2 MMT 03/24/2016 Issue#6 Linking orders to bulk even if it is not multi-store order[T20151014.0017][Start]
*IF loFormSet.ActiveMode = 'E' AND &lcHdrFile..multi ='Y' AND EMPTY(&lcHdrFile..cFromOrder) AND lcOrgnStatus ='B' AND &lcHdrFile..Status ='O'
IF loFormSet.ActiveMode = 'E' AND &lcHdrFile..multi ='Y' AND EMPTY(&lcHdrFile..cFromOrder) AND lcOrgnStatus ='B' AND &lcHdrFile..Status ='O'
*B611130,2 MMT 03/24/2016 Issue#6 Linking orders to bulk even if it is not multi-store order[T20151014.0017][End]  
  IF !USED('Ordline_IKE')  
    =gfOpenTable('Ordline','Ordline','SH','Ordline_IKE')
  ENDIF
  IF !USED('Ordhdr_IKE')  
    =gfOpenTable('ORDHDR','ORDBULK','SH','Ordhdr_IKE') && ACCOUNT+STATUS+BULK+CORDTYPE+ORDER
  ENDIF 
  *B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017][Start]
*!*	  SELECT Ordhdr_IKE.Order,Ordhdr_IKE.Complete,Ordhdr_IKE.entered,Ordhdr_IKE.start  ,Ordhdr_IKE.Store,Ordline_IKE.Style,;
*!*	  Ordline_IKE.LineNo,Ordline_IKE.Qty1,Ordline_IKE.Qty2,Ordline_IKE.Qty3,Ordline_IKE.Qty4,Ordline_IKE.Qty5,;
*!*	  Ordline_IKE.Qty6,Ordline_IKE.Qty7,Ordline_IKE.Qty8   FROM Ordhdr_IKE INNER JOIN Ordline_IKE ON ;
*!*	  Ordline_IKE.CORDTYPE+Ordline_IKE.ORDER+STR(Ordline_IKE.LINENO,6)= Ordhdr_IKE.CORDTYPE+Ordhdr_IKE.Order;
*!*	  WHERE (Ordhdr_IKE.ACCOUNT+Ordhdr_IKE.STATUS+Ordhdr_IKE.BULK+Ordhdr_IKE.CORDTYPE+Ordhdr_IKE.ORDER = lcOrdAccount+'O'+'Y'+'O' OR;
*!*	  Ordhdr_IKE.ACCOUNT+Ordhdr_IKE.STATUS+Ordhdr_IKE.BULK+Ordhdr_IKE.CORDTYPE+Ordhdr_IKE.ORDER = lcOrdAccount+'H'+'Y'+'O') AND;
*!*	  !EMPTY(ALLTRIM(Ordhdr_IKE.Store)) AND Ordline_IKE.TotQty > 0  INTO CURSOR 'Acc_BlkOrder' READWRITE 
  *B611118,1 MMT 02/29/2016 Issue#4 - Cannot link order lines has no prepack to bulk has prepack[T20151014.0017][Start]
*!*    SELECT Ordhdr_IKE.Order,Ordhdr_IKE.Complete,Ordhdr_IKE.entered,Ordhdr_IKE.start  ,Ordhdr_IKE.Store,Ordline_IKE.Style,Ordline_IKE.prepak,;
*!*    Ordline_IKE.LineNo,Ordline_IKE.Qty1,Ordline_IKE.Qty2,Ordline_IKE.Qty3,Ordline_IKE.Qty4,Ordline_IKE.Qty5,;
*!*    Ordline_IKE.Qty6,Ordline_IKE.Qty7,Ordline_IKE.Qty8   FROM Ordhdr_IKE INNER JOIN Ordline_IKE ON ;
*!*    Ordline_IKE.CORDTYPE+Ordline_IKE.ORDER+STR(Ordline_IKE.LINENO,6)= Ordhdr_IKE.CORDTYPE+Ordhdr_IKE.Order;
*!*    WHERE (Ordhdr_IKE.ACCOUNT+Ordhdr_IKE.STATUS+Ordhdr_IKE.BULK+Ordhdr_IKE.CORDTYPE+Ordhdr_IKE.ORDER = lcOrdAccount+'O'+'Y'+'O' OR;
*!*    Ordhdr_IKE.ACCOUNT+Ordhdr_IKE.STATUS+Ordhdr_IKE.BULK+Ordhdr_IKE.CORDTYPE+Ordhdr_IKE.ORDER = lcOrdAccount+'H'+'Y'+'O') AND;
*!*    !EMPTY(ALLTRIM(Ordhdr_IKE.Store)) AND Ordline_IKE.TotQty > 0  INTO CURSOR 'Acc_BlkOrder' READWRITE 
  *MT
*!*    SELECT Ordhdr_IKE.Order,Ordhdr_IKE.Complete,Ordhdr_IKE.entered,Ordhdr_IKE.start  ,Ordhdr_IKE.Store,Ordline_IKE.Style,Ordline_IKE.prepak,;
*!*    Ordline_IKE.LineNo,Ordline_IKE.Qty1,Ordline_IKE.Qty2,Ordline_IKE.Qty3,Ordline_IKE.Qty4,Ordline_IKE.Qty5,;
*!*    Ordline_IKE.Qty6,Ordline_IKE.Qty7,Ordline_IKE.Qty8 ,Ordline_IKE.TotQty   FROM Ordhdr_IKE INNER JOIN Ordline_IKE ON ;
*!*    Ordline_IKE.CORDTYPE+Ordline_IKE.ORDER+STR(Ordline_IKE.LINENO,6)= Ordhdr_IKE.CORDTYPE+Ordhdr_IKE.Order;
*!*    WHERE (Ordhdr_IKE.ACCOUNT+Ordhdr_IKE.STATUS+Ordhdr_IKE.BULK+Ordhdr_IKE.CORDTYPE+Ordhdr_IKE.ORDER = lcOrdAccount+'O'+'Y'+'O' OR;
*!*    Ordhdr_IKE.ACCOUNT+Ordhdr_IKE.STATUS+Ordhdr_IKE.BULK+Ordhdr_IKE.CORDTYPE+Ordhdr_IKE.ORDER = lcOrdAccount+'H'+'Y'+'O') AND;
*!*    !EMPTY(ALLTRIM(Ordhdr_IKE.Store)) AND Ordline_IKE.TotQty > 0  INTO CURSOR 'Acc_BlkOrder' READWRITE 
  SELECT Ordhdr_IKE.Order,Ordhdr_IKE.Complete,Ordhdr_IKE.entered,Ordhdr_IKE.start  ,Ordhdr_IKE.Store,Ordline_IKE.Style,Ordline_IKE.prepak,;
  Ordline_IKE.LineNo,Ordline_IKE.Qty1,Ordline_IKE.Qty2,Ordline_IKE.Qty3,Ordline_IKE.Qty4,Ordline_IKE.Qty5,;
  Ordline_IKE.Qty6,Ordline_IKE.Qty7,Ordline_IKE.Qty8 ,Ordline_IKE.TotQty,Ordline_IKE.PACK_ID   FROM Ordhdr_IKE INNER JOIN Ordline_IKE ON ;
  Ordline_IKE.CORDTYPE+Ordline_IKE.ORDER+STR(Ordline_IKE.LINENO,6)= Ordhdr_IKE.CORDTYPE+Ordhdr_IKE.Order;
  WHERE (Ordhdr_IKE.ACCOUNT+Ordhdr_IKE.STATUS+Ordhdr_IKE.BULK+Ordhdr_IKE.CORDTYPE+Ordhdr_IKE.ORDER = lcOrdAccount+'O'+'Y'+'O' OR;
  Ordhdr_IKE.ACCOUNT+Ordhdr_IKE.STATUS+Ordhdr_IKE.BULK+Ordhdr_IKE.CORDTYPE+Ordhdr_IKE.ORDER = lcOrdAccount+'H'+'Y'+'O') AND;
  !EMPTY(ALLTRIM(Ordhdr_IKE.Store)) AND Ordline_IKE.TotQty > 0  INTO CURSOR 'Acc_BlkOrder' READWRITE 
  *MT
  *B611118,1 MMT 02/29/2016 Issue#4 - Cannot link order lines has no prepack to bulk has prepack[T20151014.0017][End]
  *B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017][End]
  
  SELECT 'Acc_BlkOrder'
  LOCATE 
  IF EOF()
    RETURN 
  ENDIF

  IF gfModalgen("QRM00000B00006","DIALOG",.F.,.F.,"There are open bulk orders for account "+lcOrdAccount+". Do you want to use them in generating this order?") <> 1
    RETURN 
  ENDIF
  

  IF !USED('Customer_IKE')  
    =gfOpenTable('Customer','Customer','SH','Customer_IKE') && ACCOUNT+STATUS+BULK+CORDTYPE+ORDER
  ENDIF 
 
  CREATE CURSOR 'BlkOrders' (Order C(6),complete D(8),entered D(8),start D(8),LineNo N(6),Style C(19),;
   Qty1 N(7),Qty2 N(7), Qty3 N(7), Qty4 N(7), Qty5 N(7), Qty6 N(7), Qty7 N(7), Qty8 N(7))
  SELECT  'BlkOrders' 
  INDEX ON ORDER + Style TAG 'BlkOrders' 
  
  CREATE CURSOR 'UsedBlk' (Order C(6),LineNo N(6),Style C(19),Store C(8))
  SELECT 'UsedBlk' 
  INDEX on Store+Style+Order+STR(LineNo,6) TAG 'UsedBlk'
  
  SELECT 'Acc_BlkOrder'
  *B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017][Start]
  *INDEX on STORE+STYLE+Order+STR(LineNo,6) TAG 'AcBlkOrder' 
  INDEX on STORE+STYLE+DTOS(entered)+Order+STR(LineNo,6) TAG 'AcBlkOrder' 
  *B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017][End]
  SELECT (lcLineFile)
  LOCATE 
  SCAN FOR !DELETED() AND TotQty > 0
    lcLineStore = &lcLineFile..Store
    lcLineStyle = &lcLineFile..Style
    =gfSeek('S'+lcOrdAccount+lcLineStore,'Customer_IKE' ,'Customer')
    lcStoreDC = Customer_IKE.Dist_Ctr
    *B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017][Start]
*!*	    llFoundBefore =.F.
*!*	 
*!*	    IF SEEK(lcStoreDC+lcLineStyle,'UsedBlk')    
*!*	      SELECT  'Acc_BlkOrder' 
*!*	      IF SEEK(lcStoreDC+lcLineStyle+UsedBlk.Order+STR(UsedBlk.LineNo,6),'Acc_BlkOrder') AND Acc_BlkOrder.Qty1 >= &lcLineFile..Qty1 AND ;
*!*	           Acc_BlkOrder.Qty2 >= &lcLineFile..Qty2 AND ;
*!*	           Acc_BlkOrder.Qty3 >= &lcLineFile..Qty3 AND Acc_BlkOrder.Qty4 >= &lcLineFile..Qty4 AND Acc_BlkOrder.Qty5 >= &lcLineFile..Qty5 AND;
*!*	           Acc_BlkOrder.Qty6 >= &lcLineFile..Qty6 AND Acc_BlkOrder.Qty7 >= &lcLineFile..Qty7 AND Acc_BlkOrder.Qty8 >= &lcLineFile..Qty8
*!*	           
*!*	        REPLACE cFromOrder WITH UsedBlk.Order,bulklineno WITH UsedBlk.LineNo, Flag WITH IIF(Flag='N' ,Flag,'M') IN (lcLineFile)
*!*	        REPLACE cFromOrder WITH UsedBlk.Order IN (lcHdrFile)
*!*	        REPLACE QTY1 WITH MAX(QTY1 - &lcLineFile..Qty1,0),;
*!*	                QTY2 WITH MAX(QTY2 - &lcLineFile..Qty2,0),;
*!*	                QTY3 WITH MAX(QTY3 - &lcLineFile..Qty3,0),;
*!*	                QTY4 WITH MAX(QTY4 - &lcLineFile..Qty4,0),;
*!*	                QTY5 WITH MAX(QTY5 - &lcLineFile..Qty5,0),;
*!*	                QTY6 WITH MAX(QTY6 - &lcLineFile..Qty6,0),;
*!*	                QTY7 WITH MAX(QTY7 - &lcLineFile..Qty7,0),;
*!*	                QTY8 WITH MAX(QTY8 - &lcLineFile..Qty8,0) IN 'Acc_BlkOrder'   
*!*	        llFoundBefore =.T.        
*!*	      ENDIF
*!*	    ENDIF
*!*	    IF llFoundBefore
*!*	      LOOP
*!*	    ENDIF
    *B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017][End]
    IF RECCOUNT('BlkOrders')> 0
      ZAP IN 'BlkOrders'
    ENDIF
    IF SEEK(lcStoreDC+lcLineStyle, 'Acc_BlkOrder')
      SELECT  'Acc_BlkOrder' 
      lnCntOrder = 0
      *B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017][Start]
*!*	      SCAN REST WHILE STORE+STYLE+Order+STR(LineNo,6) = lcStoreDC+lcLineStyle FOR Acc_BlkOrder.Qty1 >= &lcLineFile..Qty1 AND Acc_BlkOrder.Qty2 >= &lcLineFile..Qty2 AND ;
*!*	           Acc_BlkOrder.Qty3 >= &lcLineFile..Qty3 AND Acc_BlkOrder.Qty4 >= &lcLineFile..Qty4 AND Acc_BlkOrder.Qty5 >= &lcLineFile..Qty5 AND;
*!*	           Acc_BlkOrder.Qty6 >= &lcLineFile..Qty6 AND Acc_BlkOrder.Qty7 >= &lcLineFile..Qty7 AND Acc_BlkOrder.Qty8 >= &lcLineFile..Qty8
 	   *B611118,1 MMT 02/29/2016 Issue#4 - Cannot link order lines has no prepack to bulk has prepack[T20151014.0017][Start]
*!*	      SCAN REST WHILE STORE+STYLE+DTOS(entered)+Order+STR(LineNo,6) = lcStoreDC+lcLineStyle FOR Acc_BlkOrder.Qty1 >= &lcLineFile..Qty1 AND Acc_BlkOrder.Qty2 >= &lcLineFile..Qty2 AND ;
*!*	           Acc_BlkOrder.Qty3 >= &lcLineFile..Qty3 AND Acc_BlkOrder.Qty4 >= &lcLineFile..Qty4 AND Acc_BlkOrder.Qty5 >= &lcLineFile..Qty5 AND;
*!*	           Acc_BlkOrder.Qty6 >= &lcLineFile..Qty6 AND Acc_BlkOrder.Qty7 >= &lcLineFile..Qty7 AND Acc_BlkOrder.Qty8 >= &lcLineFile..Qty8 AND ;
*!*	           IIF(!EMPTY(&lcLineFile..prepak),prepak =&lcLineFile..prepak,EMPTY(prepak))
      *B611130,1 MMT 03/23/2016 Issue#6 Use Pack_ID while linking Multi-store order with Bulk order [T20151014.0017][Start]
*!*        SCAN REST WHILE STORE+STYLE+DTOS(entered)+Order+STR(LineNo,6) = lcStoreDC+lcLineStyle FOR Acc_BlkOrder.Qty1 >= &lcLineFile..Qty1 AND Acc_BlkOrder.Qty2 >= &lcLineFile..Qty2 AND ;
*!*             Acc_BlkOrder.Qty3 >= &lcLineFile..Qty3 AND Acc_BlkOrder.Qty4 >= &lcLineFile..Qty4 AND Acc_BlkOrder.Qty5 >= &lcLineFile..Qty5 AND;
*!*             Acc_BlkOrder.Qty6 >= &lcLineFile..Qty6 AND Acc_BlkOrder.Qty7 >= &lcLineFile..Qty7 AND Acc_BlkOrder.Qty8 >= &lcLineFile..Qty8 
*!*          IF (!EMPTY(&lcLineFile..prepak) OR !EMPTY(Acc_BlkOrder.prepak)) AND ;
*!*             Acc_BlkOrder.TotQTy >= &lcLineFile..TOTQTY &&AND MOD(Acc_BlkOrder.TotQTy,&lcLineFile..TotQTy)=0
*!*  	        STORE .F. TO llQty1, llQty2 ,llQty3 ,llQty4 ,llQty5 ,llQty6 ,llQty7 ,llQty8
*!*            
*!*            IF &lcLineFile..Qty1 = 0 OR (&lcLineFile..Qty1 > 0 AND  Acc_BlkOrder.Qty1>= &lcLineFile..Qty1 AND (Acc_BlkOrder.Qty1/&lcLineFile..Qty1)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
*!*              llQty1 = .T.
*!*            ENDIF
*!*            IF &lcLineFile..Qty2 = 0 OR (&lcLineFile..Qty2 > 0 AND  Acc_BlkOrder.Qty2>= &lcLineFile..Qty2 AND (Acc_BlkOrder.Qty2/&lcLineFile..Qty2)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
*!*              llQty2 = .T.
*!*            ENDIF
*!*            IF &lcLineFile..Qty3 = 0 OR (&lcLineFile..Qty3 > 0 AND  Acc_BlkOrder.Qty3>= &lcLineFile..Qty3 AND (Acc_BlkOrder.Qty3/&lcLineFile..Qty3)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
*!*              llQty3 = .T.
*!*            ENDIF
*!*            IF &lcLineFile..Qty4 = 0 OR (&lcLineFile..Qty4 > 0 AND  Acc_BlkOrder.Qty4>= &lcLineFile..Qty4 AND (Acc_BlkOrder.Qty4/&lcLineFile..Qty4)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
*!*              llQty4 = .T.
*!*            ENDIF
*!*            IF &lcLineFile..Qty5 = 0 OR (&lcLineFile..Qty5 > 0 AND  Acc_BlkOrder.Qty5>= &lcLineFile..Qty5 AND (Acc_BlkOrder.Qty5/&lcLineFile..Qty5)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
*!*              llQty5 = .T.
*!*            ENDIF
*!*            IF &lcLineFile..Qty6 = 0 OR (&lcLineFile..Qty6 > 0 AND  Acc_BlkOrder.Qty6>= &lcLineFile..Qty6 AND (Acc_BlkOrder.Qty6/&lcLineFile..Qty6)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
*!*              llQty6 = .T.
*!*            ENDIF
*!*            IF &lcLineFile..Qty7 = 0 OR (&lcLineFile..Qty7 > 0 AND  Acc_BlkOrder.Qty7>= &lcLineFile..Qty7 AND (Acc_BlkOrder.Qty7/&lcLineFile..Qty7)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
*!*              llQty7 = .T.
*!*            ENDIF
*!*            IF &lcLineFile..Qty8 = 0 OR (&lcLineFile..Qty8 > 0 AND  Acc_BlkOrder.Qty8>= &lcLineFile..Qty8 AND (Acc_BlkOrder.Qty8/&lcLineFile..Qty8)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
*!*              llQty8 = .T.
*!*            ENDIF

*!*            IF !(llQty1 AND llQty2 AND llQty3 AND llQty4 AND llQty5  AND llQty6  AND llQty7  AND llQty8)
*!*              LOOP
*!*            ENDIF
*!*          ENDIF
        *B611118,1 MMT 02/29/2016 Issue#4 - Cannot link order lines has no prepack to bulk has prepack[T20151014.0017][End]     
      SCAN REST WHILE STORE+STYLE+DTOS(entered)+Order+STR(LineNo,6) = lcStoreDC+lcLineStyle FOR Acc_BlkOrder.PACK_ID = &lcLineFile..PACK_ID AND;
           Acc_BlkOrder.Qty1 >= &lcLineFile..Qty1 AND Acc_BlkOrder.Qty2 >= &lcLineFile..Qty2 AND ;
           Acc_BlkOrder.Qty3 >= &lcLineFile..Qty3 AND Acc_BlkOrder.Qty4 >= &lcLineFile..Qty4 AND Acc_BlkOrder.Qty5 >= &lcLineFile..Qty5 AND;
           Acc_BlkOrder.Qty6 >= &lcLineFile..Qty6 AND Acc_BlkOrder.Qty7 >= &lcLineFile..Qty7 AND Acc_BlkOrder.Qty8 >= &lcLineFile..Qty8 
      *B611130,1 MMT 03/23/2016 Issue#6 Use Pack_ID while linking Multi-store order with Bulk order [T20151014.0017][End]        
        
        *B611130,3 MMT 03/24/2016 Issue#6 Consider preppack within pack Linking orders to bulk even if it is not multi-store order[T20151014.0017][Start]
        IF (!EMPTY(&lcLineFile..prepak) OR !EMPTY(Acc_BlkOrder.prepak)) AND ;
           Acc_BlkOrder.TotQTy >= &lcLineFile..TOTQTY &&AND MOD(Acc_BlkOrder.TotQTy,&lcLineFile..TotQTy)=0
           
          STORE .F. TO llQty1, llQty2 ,llQty3 ,llQty4 ,llQty5 ,llQty6 ,llQty7 ,llQty8
          
          IF &lcLineFile..Qty1 = 0 OR (&lcLineFile..Qty1 > 0 AND  Acc_BlkOrder.Qty1>= &lcLineFile..Qty1 AND (Acc_BlkOrder.Qty1/&lcLineFile..Qty1)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
            llQty1 = .T.
          ENDIF
          IF &lcLineFile..Qty2 = 0 OR (&lcLineFile..Qty2 > 0 AND  Acc_BlkOrder.Qty2>= &lcLineFile..Qty2 AND (Acc_BlkOrder.Qty2/&lcLineFile..Qty2)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
            llQty2 = .T.
          ENDIF
          IF &lcLineFile..Qty3 = 0 OR (&lcLineFile..Qty3 > 0 AND  Acc_BlkOrder.Qty3>= &lcLineFile..Qty3 AND (Acc_BlkOrder.Qty3/&lcLineFile..Qty3)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
            llQty3 = .T.
          ENDIF
          IF &lcLineFile..Qty4 = 0 OR (&lcLineFile..Qty4 > 0 AND  Acc_BlkOrder.Qty4>= &lcLineFile..Qty4 AND (Acc_BlkOrder.Qty4/&lcLineFile..Qty4)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
            llQty4 = .T.
          ENDIF
          IF &lcLineFile..Qty5 = 0 OR (&lcLineFile..Qty5 > 0 AND  Acc_BlkOrder.Qty5>= &lcLineFile..Qty5 AND (Acc_BlkOrder.Qty5/&lcLineFile..Qty5)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
            llQty5 = .T.
          ENDIF
          IF &lcLineFile..Qty6 = 0 OR (&lcLineFile..Qty6 > 0 AND  Acc_BlkOrder.Qty6>= &lcLineFile..Qty6 AND (Acc_BlkOrder.Qty6/&lcLineFile..Qty6)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
            llQty6 = .T.
          ENDIF
          IF &lcLineFile..Qty7 = 0 OR (&lcLineFile..Qty7 > 0 AND  Acc_BlkOrder.Qty7>= &lcLineFile..Qty7 AND (Acc_BlkOrder.Qty7/&lcLineFile..Qty7)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
            llQty7 = .T.
          ENDIF
          IF &lcLineFile..Qty8 = 0 OR (&lcLineFile..Qty8 > 0 AND  Acc_BlkOrder.Qty8>= &lcLineFile..Qty8 AND (Acc_BlkOrder.Qty8/&lcLineFile..Qty8)= (Acc_BlkOrder.TotQTy/&lcLineFile..TotQTy))
            llQty8 = .T.
          ENDIF

          IF !(llQty1 AND llQty2 AND llQty3 AND llQty4 AND llQty5  AND llQty6  AND llQty7  AND llQty8)
            LOOP
          ENDIF
        ENDIF
        *B611130,3 MMT 03/24/2016 Issue#6 Consider preppack within pack Linking orders to bulk even if it is not multi-store order[T20151014.0017][End]
        
        
        *B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017][End]     
        INSERT INTO 'BlkOrders' VALUES (Acc_BlkOrder.Order,Acc_BlkOrder.Complete,Acc_BlkOrder.ENTERED,Acc_BlkOrder.Start,Acc_BlkOrder.LineNo,Acc_BlkOrder.Style,;
                                          Acc_BlkOrder.Qty1,Acc_BlkOrder.Qty2,Acc_BlkOrder.Qty3,Acc_BlkOrder.Qty4,Acc_BlkOrder.Qty5,Acc_BlkOrder.Qty6,Acc_BlkOrder.Qty7,Acc_BlkOrder.Qty8)
        
        lnCntOrder = lnCntOrder + 1  
        *B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017][Start]                                  
        EXIT
        *B611111,1 MMT 02/04/2016 IKE00 SBT process issues in SO and Inter-location PO[T20151014.0017][end]
      ENDSCAN
      IF lnCntOrder > 1
         lcOrderNo =''
         SELECT 'BlkOrders'  
         lcOrderNo= IIF(ARIABROW('lcOrderNo',"Bulk Orders for Style: "+lcLineStyle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),BlkOrders.Order,'')
         IF !EMPTY(lcOrderNo)
           lcLinenO = BlkOrders.LineNO
           REPLACE cFromOrder WITH lcOrderNo,bulklineno WITH lcLinenO, Flag WITH IIF(Flag='N' ,Flag,'M') IN (lcLineFile)
           REPLACE cFromOrder WITH lcOrderNo IN (lcHdrFile)
           =SEEK(lcStoreDC+lcLineStyle+lcOrderNo +STR(lcLinenO ,6),'Acc_BlkOrder')
           REPLACE QTY1 WITH MAX(QTY1 - &lcLineFile..Qty1,0),;
                   QTY2 WITH MAX(QTY2 - &lcLineFile..Qty2,0),;
                   QTY3 WITH MAX(QTY3 - &lcLineFile..Qty3,0),;
                   QTY4 WITH MAX(QTY4 - &lcLineFile..Qty4,0),;
                   QTY5 WITH MAX(QTY5 - &lcLineFile..Qty5,0),;
                   QTY6 WITH MAX(QTY6 - &lcLineFile..Qty6,0),;
                   QTY7 WITH MAX(QTY7 - &lcLineFile..Qty7,0),;
                   QTY8 WITH MAX(QTY8 - &lcLineFile..Qty8,0) IN 'Acc_BlkOrder'
           
           IF !SEEK(lcStoreDC+lcLineStyle+lcOrderNo+STR(lcLinenO ,6),'UsedBlk')        
             INSERT INTO 'UsedBlk' VALUES (lcOrderNo,lcLinenO,lcLineStyle,lcStoreDC)       
           ENDIF  
         ENDIF       
      ELSE
        IF lnCntOrder = 1    
          SELECT 'BlkOrders'  
          LOCATE
          lcOrderNo = BlkOrders.Order
          lcLinenO = BlkOrders.LineNO
          REPLACE cFromOrder WITH lcOrderNo,bulklineno WITH lcLinenO, Flag WITH IIF(Flag='N' ,Flag,'M') IN (lcLineFile)
          REPLACE cFromOrder WITH lcOrderNo IN (lcHdrFile)
          =SEEK(lcStoreDC+lcLineStyle+lcOrderNo +STR(lcLinenO ,6),'Acc_BlkOrder')
          REPLACE QTY1 WITH MAX(QTY1 - &lcLineFile..Qty1,0),;
                  QTY2 WITH MAX(QTY2 - &lcLineFile..Qty2,0),;
                  QTY3 WITH MAX(QTY3 - &lcLineFile..Qty3,0),;
                  QTY4 WITH MAX(QTY4 - &lcLineFile..Qty4,0),;
                  QTY5 WITH MAX(QTY5 - &lcLineFile..Qty5,0),;
                  QTY6 WITH MAX(QTY6 - &lcLineFile..Qty6,0),;
                  QTY7 WITH MAX(QTY7 - &lcLineFile..Qty7,0),;
                  QTY8 WITH MAX(QTY8 - &lcLineFile..Qty8,0) IN 'Acc_BlkOrder'
		  IF !SEEK(lcStoreDC+lcLineStyle+lcOrderNo+STR(lcLinenO ,6),'UsedBlk')        
            INSERT INTO 'UsedBlk' VALUES (lcOrderNo,lcLinenO,lcLineStyle,lcStoreDC)       
          ENDIF                    
        ENDIF
      ENDIF
    ENDIF
    SELECT (lcLineFile)
  ENDSCAN
ENDIF
*C201743,1 MMT 12/02/2015 Add trigger to complete Inter-location PO related Picking ticket[T20151014.0017][End]

*C201748,1 MMT 12/21/2015 Add triggers to filter stores by classification and ranking[T20151014.0017][Start]
*!**************************************************************************
*! Name      : lfFLTRSTORES
*! Developer : Mariam Mazhar[MMT]
*! Date      : 12/21/2015 
*! Purpose   : Add trigger to filter Stores
*!**************************************************************************
FUNCTION lfFLTRSTORES
llValueToReturn = .T.
oFormEnvironment = loFormSet.oFormEnvironment
IF TYPE("oFormEnvironment.StoreClass") ='U'
  oFormEnvironment.AddProperty ("StoreClass","")
ENDIF
IF TYPE("oFormEnvironment.StoreRank") ='U'
  oFormEnvironment.AddProperty ("StoreRank","")
ENDIF  
lcOrdHDr = oFormEnvironment.lcOrdHDr
lcAccount = &lcOrdHDr..Account
SELECT (oFormEnvironment.lcStores)  
SET FILTER TO 
IF !USED('STORESCLASSIFICATIONS') 
  = gfOpenTable('STORESCLASSIFICATIONS','ACCID')
ENDIF
IF gfSeek(lcAccount,'STORESCLASSIFICATIONS','ACCID')
  DIMENSION laClassification[1],laRanking[1]
  laClassification = ''
  laClassification[1] = "--------------Select--------------"
  laRanking = ''
  laRanking[1] = "All"
*!*	  IF !USED('CLASSIFICATIONSTORES') 
*!*	    = gfOpenTable('CLASSIFICATIONSTORES','CLSACCSTR')
*!*	  ENDIF
  SELECT STORESCLASSIFICATIONS
  SCAN REST WHILE ACCOUNT+CID = lcAccount
    IF ASCAN(laClassification,STORESCLASSIFICATIONS.CID,1) = 0 
      DIMENSION laClassification[ALEN(laClassification,1)+1]
      laClassification[ALEN(laClassification,1)]=STORESCLASSIFICATIONS.CID
    ENDIF
  ENDSCAN 
  DO FORM (oAriaApplication.ClientScreenHome+"SO\SOSTRCLS.SCX")
ENDIF  
RETURN llValueToReturn 

*!**************************************************************************
*! Name      : lfChangeClass
*! Developer : Mariam Mazhar[MMT]
*! Date      : 12/21/2015 
*! Purpose   : Valid of change classification
*!**************************************************************************
FUNCTION lfChangeClass
LPARAMETERS lcSelectedClss,loFormBranch

IF !USED('CLASSIFICATIONSTORES') 
  = gfOpenTable('CLASSIFICATIONSTORES','STRCLAS')
ENDIF
SELECT CLASSIFICATIONSTORES
IF gfSqlRun("Select DISTINCT RANKING from CLASSIFICATIONSTORES Where CID='"+PADR(lcSelectedClss,20)+"' order by RANKING","CLASSIFICATIONSTORES",.T.,"Ranks")
  DIMENSION laRanking[1]
  laRanking = ''
  laRanking[1] = "All"
  SELECT "Ranks"
  LOCATE
  SCAN
    DIMENSION laRanking[ALEN(laRanking,1)+1]
    laRanking[ALEN(laRanking,1)] = Ranks.RANKING 
  ENDSCAN
ENDIF
IF ALLTRIM(lcSelectedClss) <> "--------------Select--------------"
  loFormBranch.ariaForm1.cmdOk.Enabled = .T.
  loFormBranch.ariaForm1.cboRank.Enabled = .T.
ELSE
  loFormBranch.ariaForm1.cmdOk.Enabled = .F.
  loFormBranch.ariaForm1.cboRank.Enabled = .F.
ENDIF
loFormBranch.ariaForm1.cboRank.reQuery()
loFormBranch.ariaForm1.cboRank.DisplayValue = laRanking[1]

*!**************************************************************************
*! Name      : lfvOKClass
*! Developer : Mariam Mazhar[MMT]
*! Date      : 12/21/2015 
*! Purpose   : OK of classification screen
*!**************************************************************************
FUNCTION lfvOKClass
PARAMETERS loFormBranch

oFormEnvironment = loFormSet.oFormEnvironment
lcOrdHDr = oFormEnvironment.lcOrdHDr
lcAccount = &lcOrdHDr..Account
lcSelectedClass = loFormBranch.ariaForm1.cboClass.DisplayValue
lcSelectedRank = loFormBranch.ariaForm1.cboRank.DisplayValue
oFormEnvironment.StoreRank = lcSelectedRank 
oFormEnvironment.StoreClass = lcSelectedClass 
IF !USED('CLASSIFICATIONSTORES') 
  = gfOpenTable('CLASSIFICATIONSTORES','STRCLAS')
ENDIF
SELECT CLASSIFICATIONSTORES
IF gfSqlRun("Select DISTINCT STORE from CLASSIFICATIONSTORES Where CID='"+PADR(lcSelectedClass,20)+"'"+IIF(lcSelectedRank <> "All"," AND RANKING ='"+PADR(ALLTRIM(lcSelectedRank),1)+"'","")+" order by STORE","CLASSIFICATIONSTORES",.T.,"Stores")
  SELECT "Stores"
  IF RECCOUNT() > 0
    CURSORSETPROP("Buffering" ,3) 
    INDEX ON STORE TAG 'Stores'
    TRY 
    ALTER Table (oFormEnvironment.lcStores) ADD COLUMN storeclass C(20) 
    ALTER Table (oFormEnvironment.lcStores) ADD COLUMN  strranking C(1) 
    CATCH
    ENDTRY
    SELECT (oFormEnvironment.lcStores)  
    SET FILTER TO SEEK(Store,'Stores','Stores') 
    REPLACE storeclass WITH lcSelectedClass FOR SEEK(Store,'Stores','Stores') 
    IF lcSelectedRank <> "All"
      REPLACE strranking WITH lcSelectedRank FOR SEEK(Store,'Stores','Stores') 
    ELSE
      LOCATE 
      SCAN 
        IF gfSeek(PADR(lcSelectedClass,20)+lcAccount +EVAL(oFormEnvironment.lcStores+'.Store'),'CLASSIFICATIONSTORES','STRCLAS')
          REPLACE strranking WITH CLASSIFICATIONSTORES.RANKING 
        ENDIF 
      ENDSCAN 
    ENDIF  
    LOCATE
  ENDIF  
ENDIF
llValueToReturn = .T.
loFormBranch.Release()
*!**************************************************************************
*! Name      : lfUPDCLSSRNK
*! Developer : Mariam Mazhar[MMT]
*! Date      : 12/21/2015 
*! Purpose   : Update Ordline custom columns
*!**************************************************************************
FUNCTION lfUPDCLSSRNK
oFormEnvironment = loFormSet.oFormEnvironment
m.storeclass  = oFormEnvironment.StoreClass
TRY 
  m.strranking =  EVALUATE(oFormEnvironment.lcStores + '.strranking')
CATCH
  m.strranking =  ""
  m.storeclass  = ""
ENDTRY   
*!**************************************************************************
*! Name      : lfADDCOLGRD
*! Developer : Mariam Mazhar[MMT]
*! Date      : 12/21/2015 
*! Purpose   : Add custom columns
*!**************************************************************************
FUNCTION lfADDCOLGRD
oFormEnvironment = loFormSet.oFormEnvironment
WITH loFormSet.AriaForm1.Ariamultiselectiongrid1.grdMultiSelectionGrid
  lnOrgCount = .ColumnCount
  .ColumnCount = .ColumnCount + 2
  TRY 
    .Columns(lnOrgCount +1).controlsource= oFormEnvironment.lcStores + '.STORECLASS'
    .Columns(lnOrgCount + 1).Header1.Caption = 'Classification'
    .Columns(lnOrgCount + 1).Width = 70
    .Columns(lnOrgCount +2).controlsource= oFormEnvironment.lcStores + '.strranking'
    .Columns(lnOrgCount + 2).Header1.Caption = 'Ranking'
    .Columns(lnOrgCount + 2).Width = 30
  CATCH
    .ColumnCount = lnOrgCount 
  ENDTRY   
ENDWITH   
*C201748,1 MMT 12/21/2015 Add triggers to filter stores by classification and ranking[T20151014.0017][END]
*E303748,1 MAA 01/30/2017 Add trigger to copy styles UPCs to Order notes[P20160426.0001][Start]
*!**************************************************************************
*! Name      : lfCPYUPCSTY
*! Developer : Mostafa Abou Shady[MAA]
*! Date      : 01/30/2017 
*! Purpose   : Copying Style UPCs to Order notes
*!**************************************************************************
FUNCTION lfCPYUPCSTY

IF TYPE('loFormSet.llAddStyUPCs') <> 'L'
  RETURN 
ENDIF
llAddUpc = loFormSet.llAddStyUPCs 
IF llAddUpc
  lnMajlength = LEN(gfItemMask("PM"))
  STORE 0 TO lnClrLnAv , lnClrPosAv
  DECLARE laItemSeg[1]
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnCount,1] = 'C'
     lnClrLnAv  = LEN(laItemSeg[lnCount,3])
     lnClrPosAv = laItemSeg[lnCount,4]
     EXIT
    ENDIF
  ENDFOR
  CREATE CURSOR TMPSTR (mStrRep M(10))
  IF !USED('OrderLine')
    =gfOpenTable('ORDLINE', 'ORDLINE', 'SH', 'OrderLine')
  ENDIF 
  
  IF !USED('NotePd')
    =gfOpenTable('NOTEPAD', 'NOTEPAD', 'SH', 'NotePad')
  ENDIF 
  
  IF !USED('SCL')
    =gfOpenTable('SCALE', 'SCALE', 'SH', 'SCL')
  ENDIF 
  IF !USED('STYLE_SC')
    =gfOpenTable('STYLE', 'STYLE', 'SH', 'STYLE_SC')
  ENDIF 

  SELECT 'NotePad'
  llAddNotePad = !gfSeek('B'+ORDHDR.Order)
  
  IF !USED('STYUPC')
    =gfOpenTable('STYLEUPC', 'STYLEUPC', 'SH', 'STYUPC')
  ENDIF 
 
  SELECT OrderLine
  =gfSeek('O'+ORDHDR.ORDER)
  SELECT DISTINCT STYLE FROM OrderLine WHERE cOrdType + ORDER  = 'O'+ORDHDR.ORDER INTO CURSOR 'OrderStyle'
  SELECT OrderStyle
  LOCATE
  SCAN
    =gfSeek(OrderStyle.Style,'STYLE_SC')
    =gfSeek('S'+STYLE_SC.SCALE,'SCL') 
    lcStructFullNote = ''
    lcStructFullNote = SUBSTR(OrderStyle.Style,1,lnMajlength)+":"+SUBSTR(OrderStyle.Style,lnClrPosAv,lnClrLnAv) + CHR(13)+CHR(10)
    llAddUpc = .F.
    lcNoUPC = ''
    FOR lnE =  1 TO  SCL.CNT
      lcE = Str(lnE,1)
      IF gfSeek(OrderStyle.Style+lcE,'STYUPC')
        lcStructFullNote = lcStructFullNote + IIF(llAddUpc,',','')+ ALLTRIM(Scale.Sz&lcE.) + '-' + Alltrim(STYUPC.Cupcnum1 + STYUPC.Cupcnum2 + STYUPC.Cupcnum3)
        llAddUpc = .T.
      ELSE
        lcNoUPC = lcNoUPC +IIF(!EMPTY(ALLTRIM(lcNoUPC)),',','')+ ALLTRIM(Scale.Sz&lcE.) 
      ENDIF 
    ENDFOR
    
    IF !EMPTY(ALLTRIM(lcNoUPC))
      SELECT TMPSTR 
      LOCATE
      IF EOF()
        APPEND BLANK
        REPLACE mStrRep WITH "Missing UPCs for the following styles:"+ CHR(13)+ CHR(10)
      ENDIF
      REPLACE mStrRep WITH mStrRep +"Style:"+ALLTRIM(OrderStyle.Style)+CHR(13)+CHR(10)+"Sizes:"+ALLTRIM(lcNoUPC)+CHR(13)+CHR(10)  
    ENDIF
    IF llAddUpc
      IF llAddNotePad
        SELECT 'NotePad' 
        APPEND BLANK
        REPLACE Type   WITH 'B',;
                KEY    WITH ORDHDR.ORDER,;
                cDesc  WITH 'Notes For Order Number : '+ORDHDR.ORDER,;
                Mnotes WITH lcStructFullNote 
        llAddNotePad = .F.    
      ELSE
        SELECT 'NotePad' 
        IF ! (lcStructFullNote $ Mnotes)
          IF (SUBSTR(OrderStyle.Style,1,lnMajlength)+":"+SUBSTR(OrderStyle.Style,lnClrPosAv,lnClrLnAv) + CHR(13)+CHR(10) $ Mnotes)
            lnPos =  ATC(SUBSTR(OrderStyle.Style,1,lnMajlength)+":"+SUBSTR(OrderStyle.Style,lnClrPosAv,lnClrLnAv) + CHR(13)+CHR(10) ,Mnotes)
            IF lnPos > 0 
              lnPos = lnPos + LEN(SUBSTR(OrderStyle.Style,1,lnMajlength)+":"+SUBSTR(OrderStyle.Style,lnClrPosAv,lnClrLnAv) + CHR(13)+CHR(10))
              lnSecPos = ATC(CHR(13),SUBSTR(Mnotes,lnPos))
              IF lnSecPos > 0
                lnSecPos = lnSecPos + 1
                REPLACE mnotes WITH STRTRAN(mnotes,SUBSTR(mnotes,lnPos,lnSecPos),'')
                REPLACE mnotes WITH STRTRAN(mnotes,SUBSTR(OrderStyle.Style,1,lnMajlength)+":"+SUBSTR(OrderStyle.Style,lnClrPosAv,lnClrLnAv) + CHR(13)+CHR(10),'')
              ENDIF
            ENDIF
          ENDIF
          REPLACE Mnotes WITH Mnotes  + CHR(13)+CHR(10)+ lcStructFullNote 
        ENDIF  
      ENDIF  
    ENDIF
    SELECT 'NotePad' 
    =gfReplace('')
  ENDSCAN  
  SELECT 'NotePad' 
  =gfTableUpdate()
  SELECT TMPSTR
  LOCATE
  IF !EOF()
    DO FORM (oAriaApplication.ClientScreenHome+"SO\soimpre.scx")
    RETURN 
  ENDIF
ELSE  
  RETURN  
ENDIF 
*!**************************************************************************
*! Name      : lfSHWMSG
*! Developer : Mostafa Abou Shady[MAA]
*! Date      : 01/30/2017 
*! Purpose   : Show copying message
*!**************************************************************************
FUNCTION lfSHWMSG
 
IF TYPE('loFormSet.llAddStyUPCs') <> 'L'
  loFormSet.AddProperty('llAddStyUPCs',.F.)
ENDIF
IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,"Do you want to copy Style UPC into order notes?") = 1
  loFormSet.llAddStyUPCs = .T.
ELSE
  loFormSet.llAddStyUPCs = .F.
ENDIF
*E303748,1 MAA 01/30/2017 Add trigger to copy styles UPCs to Order notes[P20160426.0001][End]