*:***************************************************************************
*: Program file  : ARPINVVM.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR VAN MAR.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO C101017)
*! Date          : 01/15/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*: Tracking Job Number: C102150
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVVM
*:***************************************************************************
*:B605824,1 NAD 06/27/2002 Invoice doubling order quantity on voided invoices
*:B606485,1 RAE 10/23/2002 Print lcMess1 to lcMess5 if the setting "Print 
*						   Factor Information" is "Yes"
*:B606732,1 BWA 12/22/2002 Print the standered Factor information and comment the hardcoded.
*:B607167,1 NAD 04/14/2003 Print 13 characters of the customer PO 
*:***************************************************************************

IF !USED(lcOrdLine)
  =gfOpenFile(gcDataDir+"Ordline","Ordline", 'SH', @lcOrdLine, .T.)
ENDIF

llNoRec = .F.   && Variable used to make the endreport function in the main program.
NEWDOC  = .T.

STORE 1   TO lnCntAr , lnNotLine1 , lnNotLine2
STORE 0   TO lnCntSkuSz , lnCntSkuln , lnLinRow , MaxRow , lnHdrRow , lnBotRow , lnTotQty , lnSubTotal
STORE .F. TO llNewInvs , llNewInvo , llNewInvi , llSecDesc
STORE SPACE(1) TO lcOldInv , lcBillTo , lcOrdNot1 , lcOrdNot2 , lcSku

*-- Get the STYLE and COLOR length
DECLARE laItemSeg[1]
STORE 0 TO lnClrLen,lnClrPos , lnLenth
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
lnLenth  = LEN(gfItemMask('PM'))

lcMess1 = "PAY ONLY IN U.S. FUNDS TO CONGRESS TALCOTT CORP., 11 PENN PLAZA, NEW YORK, NY 10001-2006."
lcMess2 = "THIS ACCOUNT AND THE MERCHANDISE COVERED HEREBY IS ASSIGNED TO, OWNED BY, AND IS PAYABLE"
lcMess3 = "TO CONGRESS TALCOTT CORP., TO WHOM NOTICE MUST BE GIVEN OF ANY MERCHANDISE RETURNS OR ANY"
lcMess4 = "CLAIMS FOR SHORTAGE, NON-DELIVERY, OR OTHERWISE, WITHIN 10 DAYS AFTER RECEIPT OF GOODS."
lcMess5 = "50 M 7/95"


STORE "" TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,;
            lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3
STORE {} TO ldEnterDat , ldComplDat

lnInvNo  = 22
lnOrdQty = 0    && The order line quantity
lnAlias  = 0
DIMENSION laArSize(8)  && Array hold Sku sizes
STORE ' ' TO laArSize

SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
SET DEVICE TO PRINT

SCAN FOR &lcASExp
  WAIT WINDOW 'Selecting Records For The Report ...' + Invoice NOWAIT
  lcInvoice = Invoice

  IF NEWDOC
    lnMesNo    = 1
    NEWDOC     = .F.
    ldInvDate  = InvHdr.InvDate
    lcOrder    = InvHdr.Order
    lcDept     = InvHdr.Dept
    lcCustPo   = InvHdr.CustPo
    lcAccount  = InvHdr.Account
    lcStore    = InvHdr.Store
    lnCartons  = InvHdr.Cartons
    lnWeight   = InvHdr.Weight
    lcSalesRep = InvHdr.Rep1
    lcFactor   = InvHdr.cFacCode
    lnTotQty   = 0
    lnSubTotal = 0

    IF SEEK('O' + lcOrder,'OrdHdr')
      lcOrdNot1  = OrdHdr.Note1
      lcOrdNot2  = OrdHdr.Note2
      ldEnterDat = OrdHdr.Entered
      ldComplDat = OrdHdr.Complete
    ENDIF

    lcTerms   = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    lcShipVia = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')

    *-- GET THE BILL TO AND SHIP ADDRESS
    IF SEEK (IIF(EMPTY(lcStore),'M'+lcAccount,'S'+lcAccount+lcStore),'Customer')

      =lfSolSpAdr()               && Get the Sold to Address & Ship to Address
      lcBillTo  = Customer.BillTo
      lcBtName  = lcSolTName
      lcBtAddr1 = laSoldTo[1]
      lcBtAddr2 = laSoldTo[2]
      lcBtAddr3 = TRIM(laSoldTo[3])
      IF EMPTY(lcBtAddr2)
        lcBtAddr2 = lcBtAddr3
        lcBtAddr3 = ''
      ENDIF

      lcStName  = lcShpTName
      lcStAddr1 = laShipTo[1]
      lcStAddr2 = laShipTo[2]
      lcStAddr3 = TRIM(laShipTo[3])

      IF EMPTY(lcStAddr2)
        lcStAddr2 = lcStAddr3
        lcStAddr3 = ''
      ENDIF
    ENDIF
  ENDIF

  *--------------
  *-- START PRINT
  *--------------
  =lfGetInHdr()

  *--------------
  *-- Section for the order and invoice notepad.
  *--------------
  DO WHILE !NEWDOC
    IF !NEWDOC
      lnLinRow = lnLinRow + 1 
      @ lnLinRow , 0 SAY PADC("****** CONTINUED ON NEXT PAGE ******",80," ")
      lcOldInv = lcInvoice
      =lfGetInHdr()
    ENDIF
  ENDDO

  *--Print the Footer
  IF llNewInvs = .T.
    lnLinRow = lnLinRow + 2
    @ lnLinRow ,02 SAY lcOrdNot1 + lcOrdNot2
    llNewInvs = .F.
  ENDIF

  lnAlias=SELECT(0)
  *-- For the order notepade
  IF llRpordnot
    XNOTE_LOOP = .F.
    IF lnLinRow >= MaxRow - 2 AND lcOldInv <> lcInvoice
      NEWDOC = .F.
      lnLinRow = lnLinRow + 1 
      @ lnLinRow , 0 SAY PADC("****** CONTINUED ON NEXT PAGE ******",80," ")
      lcOldInv = lcInvoice
      =lfGetInHdr()
    ENDIF
    IF llNewInvo = .T.
      lnLinRow = lnLinRow + 1
      @ lnLinRow,01 SAY '.........NOTEPAD  FOR  ORDER.........'
      lnLinRow = lnLinRow + 1
      llNewInvo = .F.
    ENDIF

    IF SEEK('B'+lcOrder,'NotePad')
      lnMemLins = MEMLINES(NOTEPAD.MNOTES)
      DO WHILE lnNotLine1 <= lnMemLins
        IF lnLinRow >= MaxRow
          NEWDOC = .F.
          XNOTE_LOOP = .T.
        ELSE
          XNOTE_LOOP = .F.
          @ lnLinRow,02 SAY MLINE(NotePad.Mnotes,lnNotLine1)
          lnLinRow = lnLinRow + 1
        ENDIF
        IF (lnLinRow >= MaxRow - 1) AND (lnMemLins # lnNotLine1)
          lnLinRow = lnLinRow + 1
          @ lnLinRow , 0 SAY PADC("****** CONTINUED ON NEXT PAGE ******",80," ")
          lnLinRow = lnLinRow + 1
        ENDIF
        IF lnLinRow >= MaxRow
          =lfGetInHdr()
        ENDIF
        lnNotLine1 = lnNotLine1 + 1
      ENDDO
    ENDIF
  ENDIF

  *-- For the invoice notepade
  IF llRpInvNot
    XNOTE_LOOP = .F.
    IF lnLinRow >= MaxRow - 1 AND lcOldInv <>lcInvoice
      NEWDOC = .F.
      lnLinRow = lnLinRow + 1 
      @ lnLinRow , 0 SAY PADC("****** CONTINUED ON NEXT PAGE ******",80," ")
      lcOldInv = lcInvoice
      LOOP
    ENDIF

    IF llNewInvi = .T.
      @ lnLinRow,01 SAY '.........NOTEPAD  FOR  INVOICE.........'
      lnLinRow = lnLinRow + 1
      llNewInvi = .F.
    ENDIF   

    IF SEEK('C'+lcInvoice,'NotePad')
      lnMemLins = MEMLINES(NOTEPAD.MNOTES)
      DO WHILE lnNotLine2 <= lnMemLins
        IF lnLinRow >= MaxRow
          NEWDOC = .F.
          XNOTE_LOOP = .T.
        ELSE
          XNOTE_LOOP = .F.
          @ lnLinRow,02 SAY MLINE(NotePad.Mnotes,lnNotLine2)
          lnLinRow = lnLinRow + 1
        ENDIF
        IF (lnLinRow >= MaxRow - 1 ) AND (lnMemLins # lnNotLine2 )
          lnLinRow = lnLinRow + 1
          @ lnLinRow , 0 SAY PADC("****** CONTINUED ON NEXT PAGE ******",80," ")
          lnLinRow = lnLinRow + 1
        ENDIF
        IF lnLinRow >= MaxRow
          =lfGetInHdr()
        ENDIF
        lnNotLine2 = lnNotLine2 + 1
      ENDDO
    ENDIF
  ENDIF

          *-- End Section for the order and invoice notepad. --*

  IF INVHDR.Freight <> 0
    @ lnLinRow+1,45 SAY "Freight charge"
    @ lnLinRow+1,85 SAY INVHDR.Freight PICTURE "9999999.99"
  ENDIF
  IF INVHDR.Discount <> 0
    @ lnLinRow+2 ,45 SAY "Discount"
    @ lnLinRow+2 ,84 SAY -INVHDR.Discount PICTURE "99999999.99"
  ENDIF

  lnLinRow = lnLinRow + 3
  *B606732,1 BWA 12/22/2002 Print the standered Factor information and comment the hardcoded.[START]
  *B606485,1 RAE Print lcMess1 to lcMess5 if the setting "Print Factor Information" is "Yes". [start]
  *IF !EMPTY(lcFactor)
  *  FOR lnCount = lnMesNo TO 5
  *    lcCount = STR(lnCount,1)
  *    @ lnLinRow+lnCount,01 SAY lcMess&lcCount
  *  ENDFOR
  *ENDIF
  
  *IF !EMPTY(lcFactor)
  *  IF llFactor
  *    FOR lnCount = lnMesNo TO 5
  *      lcCount = STR(lnCount,1)
  *      @ lnLinRow+lnCount,01 SAY lcMess&lcCount
  *    ENDFOR
  *  ENDIF
  *ENDIF

  IF !EMPTY(lcFactor)
    IF llFactor
      FOR lnCount = 1 TO 6
        IF lnCount = 1
          @ lnLinRow+lnCount,01 SAY lcFacName
        ELSE
          @ lnLinRow+lnCount,01 SAY laFactor[lnCount-1]
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
  *B606485,1 RAE [end]
  *B606732,1 BWA 12/22/2002.[END]
  
  @ lnBotRow + 1 , 48 SAY ldEnterDat
  @ lnBotRow + 1 , 57 SAY ldInvDate
  @ lnBotRow + 1 , 66 SAY ldComplDat
  @ lnBotRow + 1 , 85 SAY PADL('$'+ALLTRIM(STR(INVHDR.TotalChg,10,2)),10)

  SELECT INVHDR
  IF EOF()
    NEWDOC = .F.
    SET DEVICE TO SCREEN
    RETURN
  ELSE
     NEWDOC = .T.
  ENDIF

  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF

ENDSCAN
WAIT CLEAR
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lpPrnSku
*! Developer : BASSEM RAFAAT (BWA) 
*! Date      : 01/15/2001
*! Purpose   : Printing Sku.
*!*************************************************************
*! Called from : ARPINVVM.PRG
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lpPrnSku()
 *!*************************************************************
PROCEDURE lpPrnSku

PRIVATE lnAlias
lnAlias = SELECT(0)

SELECT Spck_Lin
GO TOP
IF !SEEK('S'+lcAccount+lcStyle+'-'+lcColor,'Spck_Lin')
  lcSku = "    "
  SELECT (lnAlias)
  RETURN 
ENDIF
= SEEK('M'+lcAccount,'Customer')
lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
  lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
  lnDime2 = SkuTmpl.Len4
ELSE
  lnDime1 = 8  &&Default
  lnDime2 = 8  &&Default
ENDIF

lcSku = SUBSTR(Spck_Lin.Pack_Id,1,lnDime1)
STORE 1 TO   lnCntAr
STORE 0 TO   lnCntSkuSz 
STORE ' ' TO laArSize
lnCntSkuSz = 8

SEEK 'S'+lcAccount+lcStyle+'-'+lcColor
SCAN REST WHILE 'S'+Account+Style = 'S'+lcAccount+lcStyle+'-'+lcColor FOR TotQty <> 0
  FOR lnCount = 1 TO 8
    lcCount = ALLTRIM(STR(lnCount))
    IF QTY&lcCount <> 0
      lnCntAr = lnCount
      EXIT
    ENDIF
  ENDFOR
  laArSize(lnCntAr) = ' S' + ALLTRIM(STR(lnCntAr)) + ':' + SUBSTR(Spck_Lin.Pack_Id,lnDime1+1,lnDime2)
ENDSCAN
SELECT (lnAlias)
RETURN

*--End of lpPrnSku.
*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT (BWA) 
*! Date      : 01/15/2001
*! Purpose   : PRINT THE HEADER OF THE INVOICE
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetInHdr()
*!*************************************************************
FUNCTION lfGetInHdr

IF lnInvNo = 22
  lnHdrRow = 05                   && Start header row
  lnLinRow = 27                   && Start lines printing row.
  lnBotRow = 49                   && Start footer row.
  MaxRow   = lnBotRow - 5         && Max row number for lines printing.
  lnInvNo  = 1
ELSE
  lnHdrRow = lnBotRow + 07        && Start header row.
  lnLinRow = lnHdrRow + 22        && Start lines printing row.
  lnBotRow = lnLinRow + 22        && Start footer row.
  MaxRow   = lnBotRow - 05        && Max row number for lines printing.
  lnInvNo  = lnInvNo  + 01
ENDIF
NEWDOC = .T.
  
*-- Print the Invoice header
@ lnHdrRow,83 SAY lcInvoice
lnHdrRow = lnHdrRow + 9
     
@ lnHdrRow,08 SAY lcBtName
@ lnHdrRow,45 SAY lcStName
   
lnHdrRow = lnHdrRow + 1
   
@ lnHdrRow,08 SAY lcBtAddr1
@ lnHdrRow,45 SAY lcStAddr1
   
lnHdrRow = lnHdrRow + 1
   
@ lnHdrRow,08 SAY lcBtAddr2
@ lnHdrRow,45 SAY lcStAddr2
@ lnHdrRow,79 SAY lcOrder
@ lnHdrRow,86 SAY ldInvDate
  
lnHdrRow = lnHdrRow + 1
  
@ lnHdrRow,08 SAY lcBtAddr3
@ lnHdrRow,45 SAY lcStAddr3
  
lnHdrRow = lnHdrRow + 3 
@ lnHdrRow,42 SAY IIF(!EMPTY(INVHDR.Note2),"Tracking #: " + ALLTRIM(INVHDR.Note2),"")
lnHdrRow = lnHdrRow + 3 
  
@ lnHdrRow,02 SAY LEFT(lcStore,7)

IF LEN(ALLTRIM(lcStore)) > 0
  DO CASE
    CASE lcBillTo = 'M'
      @ lnHdrRow,09 SAY lcAccount
    CASE lcBillTo = 'S'
      @ lnHdrRow,09 SAY LEFT(lcStore,7)
    CASE lcBillTo = 'A'
      @ lnHdrRow,09 SAY 'Altern'
  ENDCASE
ELSE
  @ lnHdrRow,09 SAY lcAccount
ENDIF

@ lnHdrRow,17 SAY LEFT(lcTerms,15)
@ lnHdrRow,38 SAY lcSalesRep
@ lnHdrRow,42 SAY LEFT(lcShipVia,15)
*:B607167,1 NAD 04/14/2003 Print 13 characters of the customer PO (Start)
*@ lnHdrRow,63 SAY LEFT(lcCustPo,10)
@ lnHdrRow,63 SAY LEFT(lcCustPo,13)
*:B607167,1 NAD 04/14/2003 Print 13 characters of the customer PO (End)
@ lnHdrRow,78 SAY lcDept
@ lnHdrRow,83 SAY lnCartons PICTURE '99999'
@ lnHdrRow,89 SAY lnWeight  PICTURE '999999'

SELECT INVLINE
DO WHILE !EOF()
  lcStyle   = LEFT(Style,lnLenth)
  lcColor   = SUBSTR(STYLE,lnClrPos,lnClrLen)
  lcScale   = Scale
  lnLinNo   = LineNo

  STORE .F. TO llSecDesc , llClrDesc
  IF SEEK(lcStyle+'-'+lcColor,'STYLE')
    lcGroup = STYLE.cstygroup
  ENDIF
  IF llRpMsgSku
    RELEASE laArSize
    DIMENSION laArSize(8)          && Array hold Sku sizes
    STORE ' ' TO laArSize
    lnCntSkuSz = 0
    Do lpPrnSku
  ENDIF
  DO CASE
    CASE EOF() OR lcInvoice # INVLINE.INVOICE
      NEWDOC = .T.
      llNewInvs = .T.
      llNewInvo = .T.
      llNewInvi = .T.        
      EXIT
    CASE TotQty <= 0
      SKIP
      LOOP
    CASE lnLinRow >= MaxRow
      NEWDOC = .F.
      EXIT
  ENDCASE

  IF lnLenth <= 8
    @ lnLinRow,02 SAY LEFT(ALLTRIM(STYLE),8)
  ELSE
    @ lnLinRow,02 SAY LEFT(ALLTRIM(STYLE),12)
  ENDIF

  SET ORDER TO TAG INVLINEO IN INVLINE
  lcCurRecno = RECNO()
  IF SEEK(lcOrder+STR(lnLinNo,6),'INVLINE')
    *B605824,1 NAD (Start)
    lnOrdQty=0
    lnln=0
    SELECT INVLINE
    
    SCAN REST WHILE Order+STR(lineno,6)+Invoice = lcOrder+STR(lnLinNo,6)
      IF lnLinNo <> lnln 
        lnOrdQty = lnOrdQty + INVLINE.TotQty
        lnln= lnLinNo
      ENDIF   
    ENDSCAN
    
    *SELECT INVLINE
    *  SUM TotQty to lnOrdQty ;
    *    WHILE Order+STR(lineno,6)+Invoice = lcOrder+STR(lnLinNo,6)
    *B605824,1 NAD (End)
    GOTO lcCurRecno
 
  ENDIF
  SET ORDER TO TAG INVLINE IN INVLINE

  IF SEEK(lcOrder+STR(lnLinNo,6),lcOrdLine)
    lnOrdQty = lnOrdQty + &lcOrdLine..TotQty
  ENDIF

  @ lnLinRow,18 SAY lnOrdQty PICTURE '999999'
  @ lnLinRow,29 SAY TotQty   PICTURE '999999'

  @ lnLinRow,40 SAY LEFT(gfCodDes(STYLE.CSTYGROUP,'CSTYGROUP'),15)
  @ lnLinRow,73 SAY PADL('$'+ALLTRIM(STR(Price,8,2)),9)
  lnExtended = Price*TotQty
  @ lnLinRow,85 SAY PADL('$'+ALLTRIM(STR(lnExtended,10,2)),10)

  lnLinRow =  lnLinRow  + 1 
  *B605824,1 NAD (Start)
  *IF SEEK(lcStyle+'-'+lcColor,'STYLE')
  *  @ lnLinRow,40 SAY STYLE.Desc
  *ENDIF
  *lnLinRow =  lnLinRow  + 1
  *B605824,1 NAD (End)
  DO CASE
    CASE llRpMsgSku .AND. !llRpSkuSze
      @ lnLinRow,2 SAY 'SKU#:' + ALLTRIM(lcSku)
    CASE llRpMsgSku .AND. llRpSkuSze
      @ lnLinRow,2 SAY 'SKU#:' + ALLTRIM(lcSku)
      DO CASE
        CASE lnCntSkuSz = 1
          @ lnLinRow,8+LEN(ALLTRIM(lcSku)) SAY laArSize(1)
        CASE lnCntSkuSz > 1
          @ lnLinRow,8+LEN(ALLTRIM(lcSku)) SAY laArSize(1) + laArSize(2)
      ENDCASE
  ENDCASE
   
  llStyFnd = SEEK(lcStyle+'-'+lcColor,'STYLE')
  lnChkDesc = 0
  DO CASE
    CASE llStyFnd .AND. LEN(ALLTRIM(STYLE.Desc1)) > 33
     *B605824,1 NAD (Start)
      *@ lnLinRow,40 SAY SUBSTR(STYLE.Desc1,1,30)
      @ lnLinRow,40 SAY SUBSTR(STYLE.Desc1,1,28)
     *B605824,1 NAD (End)
      lnChkDesc = 1
    CASE llStyFnd .AND. LEN(ALLTRIM(STYLE.Desc1)) < 33 .AND. LEN(ALLTRIM(STYLE.Desc1)) <> 0
      *B605824,1 NAD (Start)
      *@ lnLinRow,40 SAY SUBSTR(ALLTRIM(STYLE.Desc1),1,33)
      @ lnLinRow,40 SAY SUBSTR(ALLTRIM(STYLE.Desc1),1,28)
      *B605824,1 NAD (End)
      lnChkDesc = 1
    OTHERWISE
      lcColrDsc = gfCodDes(lcColor,'COLOR')
      IF LEN(lcColrDsc) > 0  .AND. !llClrDesc
          @ lnLinRow,40 SAY LEFT(lcColrDsc,15)
          llClrDesc = .T.
      ENDIF
  ENDCASE

  lcSize = ''
  IF SEEK('S'+lcScale,'Scale')
    FOR lnCount = 1 TO Scale.Cnt
      lcCount = STR(lnCount,1)
      lcSize = lcSize + ALLTRIM(Scale.Sz&lcCount) + '/';
             + ALLTRIM(STR(Qty&lcCount)) + ' '
    ENDFOR
  ENDIF
  lnLnSz1 = INT(LEN(lcSize)/33)  && Number of line to be printed
  lnLnSz2 = MOD(LEN(lcSize),33)  && The reminder
  lnSizeLen = 0        && count printed length of lcSize
  IF lnLnSz2 > 0 
    lnLnSz1 = lnLnSz1 + 1
  ENDIF      
  lnCntSkuln = 0        && Count printed sku sizes line    
  IF lnChkDesc > 0
    IF llRpSkuSze
      DO CASE
        CASE lnCntSkuSz = 3
          lnLinRow =  lnLinRow  + 1 
          @ lnLinRow,8+LEN(ALLTRIM(lcSku)) SAY laArSize(3)
        CASE lnCntSkuSz > 3
          lnLinRow =  lnLinRow  + 1 
          @ lnLinRow,8+LEN(ALLTRIM(lcSku)) SAY laArSize(3) + laArSize(4) 
        OTHERWISE 
          IF lnCntSkuSz > 0 
            lnLinRow =  lnLinRow  + 1   
          ENDIF  
      ENDCASE
    ENDIF  
    
    DO CASE
      CASE llStyFnd .AND. LEN(ALLTRIM(STYLE.Desc1)) > 33 .AND. lnCntSkuSz >= 3
        *B605824,1 NAD (Start)
        *@ lnLinRow ,40 SAY SUBSTR(STYLE.Desc1,31,30)
        @ lnLinRow ,40 SAY SUBSTR(STYLE.Desc1,31,28)
        *B605824,1 NAD (End)
        llSecDesc = .T.
      CASE llStyFnd .AND. LEN(ALLTRIM(STYLE.Desc1)) > 33 .AND. lnCntSkuSz < 3
        lnLinRow =  lnLinRow  + 1 
        *B605824,1 NAD (Start)
        *@ lnLinRow ,40 SAY SUBSTR(STYLE.Desc1,31,30)
        @ lnLinRow ,40 SAY SUBSTR(STYLE.Desc1,31,28)
        *B605824,1 NAD (End)
        llSecDesc = .T.
      OTHERWISE
      lcColrDsc = gfCodDes(lcColor,'COLOR')
      IF LEN(lcColrDsc) > 0 .AND. !llClrDesc .AND. lnCntSkuSz = 0
        lnLinRow =  lnLinRow  + 1               
        @ lnLinRow,40 SAY LEFT(lcColrDsc,15)
        llClrDesc = .T.
      ENDIF  
    ENDCASE
  ELSE
    lcColrDsc = gfCodDes(lcColor,'COLOR')
    IF LEN(lcColrDsc) > 0 .AND. !llClrDesc 
      llClrDesc = .T.
      @ lnLinRow,40 SAY LEFT(lcColrDsc,15)
      lnLinRow =  lnLinRow  + 1 
    ENDIF  
    DO CASE
      CASE lnCntSkuSz = 3
        lnLinRow =  lnLinRow  + 1 
        IF llRpSkuSze
          @ lnLinRow,8+LEN(ALLTRIM(lcSku)) SAY laArSize(3)
        ENDIF
        @ lnLinRow , 40 SAY SUBSTR(lcSize,1+lnSizeLen,33)
        lnCntSkuln = lnCntSkuln + 1  
        lnSizeLen = lnSizeLen + 33
      CASE lnCntSkuSz > 3
        lnLinRow =  lnLinRow  + 1 
        IF llRpSkuSze
          @ lnLinRow,8+LEN(ALLTRIM(lcSku)) SAY laArSize(3) + laArSize(4) 
        ENDIF  
        @ lnLinRow , 40 SAY SUBSTR(lcSize,1+lnSizeLen,33)
        lnCntSkuln = lnCntSkuln + 1  
        lnSizeLen = lnSizeLen + 33
    ENDCASE
  ENDIF
  IF !llClrDesc 
    llClrDesc = .T.
    lcColrDsc = gfCodDes(lcColor,'COLOR')
    IF LEN(lcColrDsc) > 0
      IF llSecDesc = .T.
        lnLinRow =  lnLinRow  + 1 
        DO CASE
          CASE lnCntSkuSz = 5
            IF llRpSkuSze
              @ lnLinRow,8+LEN(ALLTRIM(lcSku)) SAY laArSize(5)
            ENDIF  
          CASE lnCntSkuSz > 5
            IF llRpSkuSze
              @ lnLinRow,8+LEN(ALLTRIM(lcSku)) SAY laArSize(5) + laArSize(6) 
            ENDIF  
        ENDCASE
        @ lnLinRow,40 SAY LEFT(lcColrDsc,15)
      ELSE  
        @ lnLinRow,40 SAY LEFT(lcColrDsc,15)
        DO CASE
          CASE lnCntSkuSz = 5
            lnLinRow =  lnLinRow  + 1 
            IF llRpSkuSze
              @ lnLinRow,8+LEN(ALLTRIM(lcSku)) SAY laArSize(5)
            ENDIF  
          CASE lnCntSkuSz > 5
            lnLinRow =  lnLinRow  + 1 
            IF llRpSkuSze
              @ lnLinRow,8+LEN(ALLTRIM(lcSku)) SAY laArSize(5) + laArSize(6) 
              @ lnLinRow , 40 SAY SUBSTR(lcSize,1+lnSizeLen,33)
              lnCntSkuln = lnCntSkuln + 1  
              lnSizeLen = lnSizeLen + 33
            ENDIF  
        ENDCASE
      ENDIF
    ENDIF  
  ENDIF  
  lcNum = 7
  DO WHILE (lnCntSkuln < lnLnSz1 .AND. LEN(SUBSTR(lcSize,1+lnSizeLen,33)) > 0 )  .OR. lcNum <= lnCntSkuSz
    lcCheck2 = 0
    DO CASE
      CASE lnCntSkuSz = lcNum
        lnLinRow =  lnLinRow  + 1 
        IF llRpSkuSze
          @ lnLinRow,8+LEN(ALLTRIM(lcSku)) SAY laArSize(lcNum)
        ENDIF
        lcCheck2 = 1
      CASE lnCntSkuSz > lcNum
        lnLinRow =  lnLinRow  + 1 
        IF llRpSkuSze
          @ lnLinRow,8+LEN(ALLTRIM(lcSku)) SAY laArSize(lcNum) + laArSize(lcNum + 1)
        ENDIF
        lcCheck2 = 1
    ENDCASE
    lcNum = lcNum + 2
    IF lnCntSkuln < lnLnSz1      
      IF lcCheck2 = 0
        lnLinRow =  lnLinRow  + 1 
      ENDIF
      @ lnLinRow , 40 SAY SUBSTR(lcSize,1+lnSizeLen,33)
      lnCntSkuln = lnCntSkuln + 1  
      lnSizeLen = lnSizeLen + 33
    ENDIF
  ENDDO
  lnTotQty = lnTotQty + TotQty
  lnSubTotal = lnSubTotal + lnExtended

  SELECT INVLINE
  SKIP

  IF lcInvoice = INVLINE.INVOICE
    lnLinRow = lnLinRow + 1 
  ELSE
    STORE .T. TO llNewInvs , llNewInvo , llNewInvi  
  ENDIF  
ENDDO

*--End of lfGetInHdr.