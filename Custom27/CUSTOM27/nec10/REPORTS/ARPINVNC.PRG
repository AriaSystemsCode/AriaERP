*:***************************************************************************
*: Program file  : ARPINVNC.PRG
*: Program desc. : Customized Invoice for NeckWork (NEC10).
*: 				   The program is a converted from 2.6 (REFER TO C100422)
*: Date          : 06/17/2002
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Sameh Saiid Ezzat(SSE)
*: Tracking Job Number: C102637
*:***************************************************************************
*: Example : DO ARPINVNC
*:***************************************************************************
*

llNoRec = .F.

SELECT InvHdr
LOCATE FOR &lcRpExp
IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcRpExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)

PRIVATE llNote_Loop , llNewDoc , lnMaxRow , lnNotLine , lcTaxRefr , lcPrePack , ;
        lnPieces , lnSubTotal , lnPageNo , lcInvPTemp , lcInvoice , lcOrder , ;
        lcPhone , lcNote1 , lcNote2 , lcAccount , lcStore , lcZone , lcCurLoop , ;
        lcEndPage , lnStylePos , lnStyleLen , lnClrPos , lnClrLen , laItemSeg , ;
        llWareHous , llOpenWarh

lcZone = ''
DECLARE laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcZone'

llNote_Loop = .F.  && to indicate whether we have finished printing Notepad or not.
llNewDoc = .T.
lnMaxRow = 47
lnNotLine = 1
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)
llWareHous = gfGetMemVar('M_WareHouse',gcAct_Comp)='Y'
lcPrePack  = ' '
lnPageNo = 0

llOpenWarh = gfOpenFile(gcDataDir+'WareHous',gcDataDir+'WareHous','SH')

STORE 0 TO lnStylePos , lnStyleLen , lnClrPos , lnClrLen
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'   && Free (Style)
    lnStyleLen = LEN(laItemSeg[lnCount,3])
    lnStylePos = laItemSeg[lnCount,4]
  ENDIF
  IF laItemSeg[lnCount,1]='C'   && Color
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

lcInvPTemp = gfTempName()
SELECT InvLine
COPY STRUCTURE TO (gcWorkDir + lcInvPTemp)
=gfOpenFile(gcWorkDir+lcInvPTemp,'','EX')
INDEX ON Invoice + Store + Pack_ID + Style + STR(LineNo,6) TAG (lcInvPTemp)

*-- This scan can also be inserted in the next scan
SELECT InvHdr
SCAN FOR &lcRpExp
  lnInvoice = Invoice
  =SEEK(lnInvoice,'InvLine') 

  DO WHILE lnInvoice = InvLine.Invoice
    IF InvHdr.Consol = 'Y'
      =SEEK(lnInvoice,'ConsInvL')
      SELECT ConsInvL
      DO WHILE lnInvoice = ConsInvL.Invoice
        SELECT (lcInvPTemp)
        APPEND BLANK 
        REPLACE  Invoice WITH ConsInvL.Invoice ; 
                 Account WITH ConsInvL.Account ;
                 Style   WITH ConsInvL.Style   ;
                 Store   WITH ConsInvL.Store   ;
                 Qty1    WITH ConsInvL.Qty1    ;
                 Qty2    WITH ConsInvL.Qty2    ;                
                 Qty3    WITH ConsInvL.Qty3    ;
                 Qty4    WITH ConsInvL.Qty4    ;
                 Qty5    WITH ConsInvL.Qty5    ;
                 Qty6    WITH ConsInvL.Qty6    ;                
                 Qty7    WITH ConsInvL.Qty7    ;
                 Qty8    WITH ConsInvL.Qty8    ;                
                 PRICE   WITH ConsInvL.PRICE   ;
                 TotQty  WITH ConsInvL.TotQty  ; 
                 LineNo  WITH ConsInvL.LineNo  ;
                 Pack_ID WITH ConsInvL.Pack_ID
        SELECT ConsInvL
        SKIP
      ENDDO
    ENDIF  
    IF InvHdr.Consol = 'Y'
      EXIT
    ENDIF  
    IF InvHdr.Consol <> 'Y'
      SELECT (lcInvPTemp)  
      APPEND BLANK 
      REPLACE Invoice WITH InvLine.Invoice ; 
              Account WITH InvLine.Account ;
              Style   WITH InvLine.Style   ;
              Store   WITH InvLine.Store   ;
              Qty1    WITH InvLine.Qty1    ;
              Qty2    WITH InvLine.Qty2    ;
              Qty3    WITH InvLine.Qty3    ;
              Qty4    WITH InvLine.Qty4    ;
              Qty5    WITH InvLine.Qty5    ;
              Qty6    WITH InvLine.Qty6    ;
              Qty7    WITH InvLine.Qty7    ;
              Qty8    WITH InvLine.Qty8    ;
              PRICE   WITH InvLine.PRICE   ;
              TotQty  WITH InvLine.TotQty  ;
              LineNo  WITH InvLine.LineNo  ;
              Pack_ID WITH InvLine.Pack_ID
    ENDIF           
    SELECT InvLine
    SKIP
  ENDDO 
ENDSCAN

SELECT (lcInvPTemp)
LOCATE

*------------------------------
* SECTION: MAIN LOOP
*------------------------------

CLEAR TYPEAHEAD
WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT

SELECT InvHdr
*SCAN WHILE INKEY() <> 32 FOR &lcRpExp
SCAN FOR &lcRpExp
  IF FLAG = 'N'
    LOOP
  ENDIF
  IF EOF()
    EXIT
  ENDIF
  lcInvoice = Invoice

  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * If any records are not found, skip to next invoice.
  * Initialize document totals.
  *-----------------------------------------------------------------

  IF llNewDoc
    STORE 0 TO lnPieces , lnSubTotal , lnPageNo
     
    SELECT InvHdr
    SEEK lcInvoice
    llNewDoc  = .F.
    lcOrder   = ORDER
    lcPhone   = IIF(EMPTY(PHONE),'',PHONE)
    lcNote1   = IIF(Note1<>'*', Note1, '')
    lcNote2   = IIF(Note2<>'*', Note2, '')
    lcAccount = Account
    lcStore   = Store

    SELECT Customer
    SEEK IIF(EMPTY(lcStore),'M'+lcAccount,'S'+lcAccount+lcStore)
    
    =lfBillShip()

    *-- FIND THE INVOICE LINES
    SELECT InvLine
    SEEK lcInvoice
     
    *- GET THE DESCRIPTION ABOUT THE CODES
    = gfRltFld(InvHdr.ShipVia , @laZone , 'SHIPVIA')    

    IF llWareHous
      lcFromZone = IIF(SEEK(InvHdr.cWareCode,'WareHous'),WareHous.UPS,lcFromZone)
    ENDIF  

    DO CASE
      CASE 'G' $ lcZone
        lcUpsDesc = lfGetZone(PADR(lcZone,7),lcFromZone,SUBSTR(CUSTOMER.cAddress5,1,3))
        lcUpsDesc = IIF(!EMPTY(lcUpsDesc),'('+ ALLTRIM(lcUpsDesc) +')' ,lcUpsDesc)
      CASE '2' $ lcZone
        lcUpsDesc  = '(12)'
      CASE 'N' $ lcZone
        lcUpsDesc  = '(22)'
      OTHERWISE
        lcUpsDesc = ''
    ENDCASE
    lcShipVia = ALLTRIM(lcShipVia) + lcUpsDesc
    lcCurLoop = '1'
  ENDIF
  *-- END NEWDOC

  *-----------------
  * START PRINT
  *-----------------
  =lfGetInHdr()
    
  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
  SELECT InvLine
  ROW  = 23

  DO WHILE lcCurLoop = '1' AND !llNote_Loop
    SELECT (lcInvPTemp)
    IF EOF() OR Invoice <> lcInvoice OR ROW >= lnMaxRow
      IF ROW >= lnMaxRow
        =lfGetCont()
        =lfGetInHdr()
        LOOP
      ENDIF
      EXIT
    ENDIF
      
    KEY = InvLine.Style
    SELECT Style
    SEEK KEY
    SELECT InvLine

    =SEEK(&lcInvPTemp..Invoice +STR(&lcInvPTemp..LineNo,6)) 
    =SEEK(&lcInvPTemp..Invoice+&lcInvPTemp..Store+&lcInvPTemp..Style+STR(&lcInvPTemp..LineNo,6),'ConsInvL')

    *=SEEK('P'+&lcInvPTemp..Account + &lcInvPTemp..Style + &lcInvPTemp..Pack_ID,'Spck_Lin')
    *=SEEK('P'+&lcInvPTemp..Account + SPACE(19) + &lcInvPTemp..Pack_ID,'Spck_Hdr')
    IF !SEEK('P'+&lcInvPTemp..Account + &lcInvPTemp..Style + &lcInvPTemp..Pack_ID,'Spck_Lin')
      =SEEK('P'+'*****' + &lcInvPTemp..Style + &lcInvPTemp..Pack_ID,'Spck_Lin')
    ENDIF
    IF !SEEK('P'+&lcInvPTemp..Account + SPACE(19) + &lcInvPTemp..Pack_ID,'Spck_Hdr')
      =SEEK('P'+ '*****' + SPACE(19) + &lcInvPTemp..Pack_ID,'Spck_Hdr')
    ENDIF
    
    IF &lcInvPTemp..Pack_ID <> lcPrePack
      lcOldAlias = ALIAS()
      SELECT Spck_Lin
      SUM REST WHILE Pack_ID = &lcInvPTemp..Pack_ID Spck_Lin.Qty1,;
               Spck_Lin.Qty2,Spck_Lin.Qty3,Spck_Lin.Qty4,;
               Spck_Lin.Qty5,Spck_Lin.Qty6,Spck_Lin.Qty7,Spck_Lin.Qty8 TO ARRAY lnQty
      lnMQty=lnQty[3]+lnQty[4]+lnQty[5]+lnQty[6]+lnQty[7]+lnQty[8]
      SELECT (lcOldAlias)
      lcPrePack = &lcInvPTemp..Pack_ID
      IF !EMPTY(&lcInvPTemp..Pack_ID)
        ROW = ROW + 2
        @ ROW,00 SAY SUBSTR(&lcInvPTemp..Pack_ID,1,4)+'    '+SUBSTR(&lcInvPTemp..Pack_ID,5,3)+' '+Spck_Hdr.DESC;
                            +STR(lnQty[1],3)+' '+ STR(lnQty[2],3)+' '+STR(lnMqty,3)
      ENDIF 
    ENDIF
       
    IF !EMPTY(Pack_ID)
      IF ROW = 23
        @ ROW,00 SAY SUBSTR(&lcInvPTemp..Pack_ID,1,4)+'    '+SUBSTR(&lcInvPTemp..Pack_ID,5,3)+' '+Spck_Hdr.DESC;
                            +STR(lnQty[1],3)+' '+ STR(lnQty[2],3)+' '+STR(lnMqty,3)
      ENDIF  
    ENDIF  
    ROW = ROW + 1
    @ ROW,01 SAY SUBSTR(&lcInvPTemp..Style,lnStylePos,lnStyleLen)
    @ ROW,15 SAY SUBSTR(&lcInvPTemp..Style,lnClrPos,lnClrLen)
    @ ROW,23 SAY IIF(SEEK(&lcInvPTemp..Style,'Style'),Style.Desc,SPACE(20))
    @ ROW,49 SAY IIF(SEEK(&lcInvPTemp..Style,'Style'),Style.Scale,'')
    @ ROW,64 SAY &lcInvPTemp..TotQty PICTURE '99999'
    lnPieces = lnPieces + &lcInvPTemp..TotQty
    lnSubTotal = lnSubTotal + (&lcInvPTemp..Price * &lcInvPTemp..TotQty)
    @ ROW,68 SAY &lcInvPTemp..Price PICTURE '9999.99'
    @ ROW,75 SAY &lcInvPTemp..Price * &lcInvPTemp..TotQty PICTURE '99999.99'

    *-- GET NEXT LINE ITEM
    SELECT (lcInvPTemp)
    SKIP
    *SELECT InvLine
    *IF InvHdr.Consol = 'N'
    *  SKIP
    *ENDIF  
  ENDDO

  ROW = ROW + 1
  *-- END LINE PROCESSING

  *---------------------------------------------------------------
  * CURRENTLY PROCESSING REGULAR LINE ITEMS
  *---------------------------------------------------------------
  lcEndPage = IIF(&lcInvPTemp..Invoice = lcInvoice ,'1','0')

  IF lcEndPage = '1' AND !llNote_Loop
    =lfGetCont()
    LOOP
  ENDIF
  
  IF llRpInvNot
    *** Print invoice Notepad.
    SELECT NotePad
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75

    IF TYPE + KEY <> 'C' + lcInvoice
      SEEK 'C' + lcInvoice
      lnMemLins = MEMLINES(NotePad.MNotes)
    ENDIF

    *lnMemLins = MEMLINES(NOTEPAD.MNOTES)
    
    IF TYPE + KEY = 'C' + lcInvoice
      @ ROW,02 SAY '* -- N O T E S -- *' 
      ROW = ROW + 1 
   
      DO WHILE lnNotLine <= lnMemLins
        IF ROW >= lnMaxRow
          llNote_Loop = .T.
          EXIT
        ELSE
          llNote_Loop = .F.
          @ ROW,02 SAY MLINE(MNoteS,lnNotLine)
          ROW = ROW + 1
        ENDIF
        IF ROW >= lnMaxRow
          =lfGetCont()
          =lfGetInHdr()
        ENDIF
        lnNotLine = lnNotLine + 1
      ENDDO
     
      IF !llNote_Loop
        @ ROW,02 SAY '* -- END OF NOTES -- *'
        lnNotLine = 1
        ROW = ROW + 1 
      ELSE
        =lfGetCont() 
        LOOP
      ENDIF
    ENDIF
    SET MEMOWIDTH TO lnOldMemW
  ENDIF
    
  SELECT InvHdr
  @ ROW,12 SAY 'TOTAL - M E R C H A N D I S E'
  @ ROW,61 SAY lnPieces   PICTURE '999999'
  @ ROW,72 SAY lnSubTotal PICTURE '9999999.99'
  ROW = ROW + 1

  IF DISCOUNT <> 0
    @ ROW,12 SAY 'TOTAL - D I S C O U N T'
    @ ROW,71 SAY DISCOUNT         PICTURE '99999999.99'
    ROW = ROW + 1
  ENDIF

  *** Print the tax rate and tax amount 
  IF llTax AND lcTaxMeth = 'M' 
    @ ROW,12 SAY lcTaxDesc

    IF !EMPTY(lcTaxRefr)
      @ ROW,33 SAY lcTaxRefr
    ENDIF  

    @ ROW,64 SAY ALLTRIM(STR(InvHdr.Tax_Rate,5,2)) + ' %'
    @ ROW,72 SAY InvHdr.TAX_AMT  PICTURE '9999999.99'
    ROW = ROW + 1
  ENDIF  
   
  IF (Freight + Insur + Cod) <> 0
     @ ROW,12 SAY 'TOTAL - F R E I G H T'
     @ ROW,72 SAY Freight + Insur + Cod PICTURE '9999999.99'
     ROW = ROW + 1
  ENDIF

  *** Print the tax rate and tax amount 
  IF llTax AND lcTaxMeth = 'A'
    @ ROW,12 SAY lcTaxDesc

    IF !EMPTY(lcTaxRefr)
      @ Row,33 SAY lcTaxRefr
    ENDIF  
    @ ROW,64 SAY ALLTRIM(STR(InvHdr.Tax_Rate,5,2)) + ' %'
    @ ROW,72 SAY InvHdr.TAX_AMT  PICTURE '9999999.99'
    ROW = ROW + 1
  ENDIF  

  @ 54,01 SAY lcPhone + ' ' + lcNote1 + ' ' + lcNote2
  IF LEN(TRIM(lcRpMsg1 + lcRpMsg2)) >0
    @ 56,10 SAY lcRpMsg1
    @ 57,10 SAY lcRpMsg2
  ENDIF

  IF InvHdr.Approval<>' ' AND UPPER(Approval)<>'DEC'
    @ 59,56 SAY 'APPROVAL: ' + InvHdr.Approval
  ENDIF

  @ 60,01 SAY InvHdr.PikTkt
  lnPageNo = lnPageNo + 1
  @ 60,08 SAY ShipDate
  @ 60,18 SAY Cartons PICTURE '@Z 999'
  @ 60,25 SAY Weight  PICTURE '@Z 99999'
  @ 60,35 SAY 'PAGE '+STR(lnPageNo,2)
  @ 60,45 SAY InvHdr.Invoice
  @ 60,72 SAY InvHdr.TotalChg PICTURE '9999999.99'
   
  SELECT InvHdr 
  SEEK lcInvoice
  IF gcDevice <> 'SCREEN'
    REPLACE PrtFlag WITH 'P'
  ENDIF  
   
  *-- GET NEXT HEADER RECORD
  SELECT InvHdr  
  llNewDoc = .T.
ENDSCAN

IF llOpenWarh
  USE IN WareHous
ENDIF

SET DEVICE TO SCREEN
RETURN
*-- End of program.

*!***************************************************************************
*! Name      : lfBillShip
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/18/2002
*! Purpose   : Function to Get the Sold to Address & Ship to Address
*!             & the Description of the Ship Via , Terms
*!***************************************************************************
*! Called from : ARPINVNC.PRG
*!***************************************************************************
*
FUNCTION lfBillShip
PRIVATE lnCusRec
*-- Fill laFactor with factor address
*IF !EMPTY(InvHdr.cFacCode)
*  =SEEK(InvHdr.cFacCode,'SycFact')
*  lcFacName   = SycFact.cFacComp
*  laFactor[1] = gfGetAdr('SycFact' , '' , '' , '' , 1)
*  laFactor[2] = gfGetAdr('SycFact' , '' , '' , '' , 2)
*  laFactor[3] = gfGetAdr('SycFact' , '' , '' , '' , 3)
*  laFactor[4] = gfGetAdr('SycFact' , '' , '' , '' , 4)
*  laFactor[5] = gfGetAdr('SycFact' , '' , '' , '' , 5)
*  =lfAdrShift('laFactor')
*ENDIF

lcShipVia = gfCodDes(InvHdr.ShipVia , 'SHIPVIA')
lcTerms = gfCodDes(InvHdr.cTermCode , 'CTERMCODE')

lcSolTName = Customer.BTName

laSoldTo[1] = gfGetAdr('Customer' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('Customer' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('Customer' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('Customer' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('Customer' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

IF OrdHdr.Alt_ShpTo
  lcShpTName  = OrdHdr.STName
  laShipTo[1] = OrdHdr.cAddress1
  laShipTo[2] = OrdHdr.cAddress2
  laShipTo[3] = OrdHdr.cAddress3
  laShipTo[4] = OrdHdr.cAddress4
  laShipTo[5] = OrdHdr.cAddress5
ELSE    && Else
  
  *-- the below lines regarding the distribution center will not be used.
  lnCusRec = 0
  IF !EMPTY(Customer.Store) AND !EMPTY(Customer.Dist_ctr)
    lnCusRec = IIF(!EOF('Customer'),RECNO('Customer'),0)
    =SEEK('S'+Customer.Account+Customer.Dist_ctr)
    lcDCCode    = Customer.Store
  ELSE
    lcDCCode = ''
  ENDIF
  
  IF InvHdr.Consol = 'Y'       
    SELECT ConsInvH
    SEEK lcInvoice
    SELECT Customer
    SEEK 'S' + lcAccount + ConsInvH.Store
  ENDIF
  
  lcShpTName  = IIF(EMPTY(Customer.DBA) , Customer.STNAME , Customer.DBA)
  laShipTo[1] = gfGetAdr('Customer' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('Customer' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('Customer' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('Customer' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('Customer' , '' , '' , '' , 5)

  IF lnCusRec <> 0 
    GOTO lnCusRec IN Customer
  ENDIF
ENDIF

=lfAdrShift('laShipTo')

RETURN ''
*-- End of lfBillShip

*!***************************************************************************
*! Name      : lfGetZone
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/18/2002
*! Purpose   : Get the zone to be printed in the invoice format.
*!***************************************************************************
*! Called from : ARPINVIN.PRG
*!***************************************************************************
*
FUNCTION lfGetZone
PARAMETERS lcUpsType,lcUpsFrom,lcToZip
RETURN IIF(!SEEK(lcUpsType+lcUpsFrom+lcToZip,'SysZones'),'',SysZones.Zone)
*--End of lfGetZone.

*!***************************************************************************
*! Name      : lfGetCont
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/18/2002
*! Purpose   : PRINT THE FOOTER OF THE INVOICE
*!***************************************************************************
*! Called from : ARPINVNC
*!***************************************************************************
*! Example     : =lfGetCont()
*!***************************************************************************
*
FUNCTION lfGetCont
PRIVATE lcAlias
lcAlias = ALIAS()
*@ ROW+1,12 SAY 'C O N T I N U E D ...'
@ ROW+2,12 SAY 'C O N T I N U E D ...'
SELECT InvHdr
@ 54,01 SAY lcPhone + ' ' + InvHdr.Note1 + ' ' + InvHdr.Note2
IF LEN(TRIM(lcRpMsg1 + lcRpMsg2)) >0
  @ 56,10 SAY lcRpMsg1
  @ 57,10 SAY lcRpMsg2
ENDIF
@ 60,01 SAY InvHdr.PikTkt
lnPageNo = lnPageNo + 1
@ 60,08 SAY ShipDate
@ 60,18 SAY '***'
@ 60,25 SAY '*****'
@ 60,35 SAY 'PAGE ' +STR(lnPageNo,2)
@ 60,45 SAY lcInvoice
@ 60,73 SAY '******.**'
SELECT (lcAlias)
*-- End of lfGetCont.

*!***************************************************************************
*! Name      : lfGetInHdr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/18/2002
*! Purpose   : PRINT THE HEADER OF THE INVOICE
*!***************************************************************************
*! Called from : ARPINVNC
*!***************************************************************************
*! Example     : = lfGetInHdr()
*!***************************************************************************
*
FUNCTION lfGetInHdr
PRIVATE lcAlias
lcAlias = ALIAS()

* LINE 2
SELECT InvHdr
@ 02,50 SAY lcInvoice

* LINE 10
@ 10,10 SAY lcSolTName
IF lcCurLoop = '1'
  @ 10,47 SAY lcShpTName
ENDIF

* LINE 11
@ 11,10 SAY laSoldTo[1]
IF lcCurLoop = '1'
  @ 11,47 SAY laShipTo[1]
ENDIF

* LINE 12
@ 12,10 SAY laSoldTo[2]
IF lcCurLoop = '1'
  @ 12,47 SAY laShipTo[2]
ENDIF

* LINE 13
@ 13,10 SAY ALLTRIM(laSoldTo[3]) + ' ' + ALLTRIM(laSoldTo[4]) + ' ' + laSoldTo[5]
IF lcCurLoop = '1'
  @ 13,47 SAY ALLTRIM(laShipTo[3]) + ' ' +ALLTRIM(laShipTo[4]) + ' ' + laShipTo[5]
ENDIF

@ 17,01 SAY Account
@ 17,08 SAY SUBSTR(CustPo,1,11)
@ 17,20 SAY lcStore
@ 17,30 SAY Dept
@ 17,37 SAY Order
@ 17,45 SAY SUBSTR(lcTerms,1,15)
@ 17,60 SAY Rep1
@ 17,64 SAY Rep2
@ 17,68 SAY SUBSTR(lcShipVia,1,15)   
ROW = 19
SELECT (lcAlias)
*--End of lfGetInHdr.
