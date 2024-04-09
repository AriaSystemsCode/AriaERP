*:***************************************************************************
*: Program file  : ARPINVIN.PRG
*: Program desc. : Customized Invoice for Infinity Classics (INF10).
*: 				   The program is a converted from 2.6 (REFER TO C100605,C100755)
*: Date          : 07/18/2002
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Tracking Job Number: C102647
*:***************************************************************************
*: Example : DO ARPINVIN
*:***************************************************************************
*: Notes   : All the report is shifted 3 lines up when converting it from A26 to A27
*:***************************************************************************
*:Modifications :
*:B606390,1 SSE   08/15/2002 Fix bug of shifting Cust PO# , Entered date ... one line up.
*:B125388,1 BWA   11/24/2004 Fix bug of not printing the style info in case deleting the style.
*:B125992,1 NADER 01/12/2005 Fix bug that in many invoices you find that they have header and no
*:B125992,1 NADER Body , it just write (Continued) even this invoive has one record
*:***************************************************************************

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

PRIVATE llNote_Loop , llNewDoc , lnMaxRow , lnNotOLine , lnNotILine , llOrdNFin , ;
        llWareHous , lcWhCity , lnPieces , lnSubTotal , lnPage , lcDuns , lcInvoice , ;
        lcOrder , lcAccount , lcStore , lcZone , lcCurLoop , lcEndPage , lnStylePos , ;
        lnStyleLen , lnClrPos , lnClrLen , laItemSeg , lnCol , lcI , llOpenWarh , ;
        llOpnOrdLn , llOpnSlsRp , llOpnSyZon , lcFromZone , lcUpsDesc

STORE '' TO lcZone , lcDuns
DECLARE laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcZone'

llNote_Loop = .F.
llNewDoc = .T.
lnMaxRow = 52
lnPage = 0
lnNotOLine = 1
lnNotILine = 1
llOrdNFin  = .F.
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp))
llWareHous = gfGetMemVar('M_WareHouse',gcAct_Comp)='Y'

*-- Get position of both Style and Color.
STORE 0 TO lnStylePos , lnClrPos
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

llOpenWarh = gfOpenFile(gcDataDir+'WareHous',gcDataDir+'WareHous','SH')
lcWhCity   = WareHous.cAddress3
llOpnSlsRp = gfOpenFile(gcDataDir+'SalesRep',gcDataDir+'SalesRep','SH')
llOpnOrdLn = gfOpenFile(gcDataDir+'OrdLine',gcDataDir+'OrdLine','SH')

SELECT InvLine
SET RELATION TO "O"+InvLine.Order+STR(InvLine.LineNo,6) INTO OrdLine ADDITIVE
LOCATE

SELECT InvHdr
SET DEVICE TO PRINT
STORE '' TO lcInvoice,lcEndPage
SCAN FOR &lcRpExp
  
  *B125992,1 NADER 01/12/2005 (Begin) Re initiate the followinf variables with every new invoice
  IF !(InvHdr.Invoice==lcInvoice)
    llNewDoc = .T.
    lnNotOLine=1
    llNote_Loop = .F.    
  ENDIF
  *B125992,1 NADER (End)
  
  IF FLAG = 'N'
    LOOP
  ENDIF
  IF EOF()
    EXIT
  ENDIF
  lcInvoice = Invoice

  *-----------------------------------------------------------------
  * Get Invoice header, line items, and financial history records.
  * If any records are not found, skip to next Invoice.
  * Initialize document totals.
  *-----------------------------------------------------------------
  IF llNewDoc
    STORE 0 TO lnPieces , lnSubTotal
    SELECT InvHdr
    SEEK lcInvoice
    IF EOF()
      SELECT InvHdr
      LOOP
    ENDIF
    llNewDoc = .F.
    lcOrder  = Order
    lcAccount = Account
    lcStore = Store
    
    =lfBillShip()

    *-- FIND THE Invoice LINES
    SELECT InvLine
    SEEK lcInvoice
     
    *-- GET THE DESCRIPTION ABOUT THE CODES
    = gfRltFld(InvHdr.ShipVia , @laZone , 'SHIPVIA')    
    lcFromZone = IIF(SEEK(InvHdr.cWareCode,'WareHous'),WareHous.UPS,lcFromZone)
    
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
    lnPage = 0
  ENDIF

  *-- Start print
  =lfGetInHdr()
  
  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
  SELECT InvLine
  lcStore = Store
  ROW = 26

  DO WHILE lcCurLoop = '1' AND !llNote_Loop
    SELECT InvLine
    IF EOF() OR Invoice <> lcInvoice OR ROW >= lnMaxRow
      IF ROW >= lnMaxRow
        =lfGetCont()
        =lfGetInHdr()
        LOOP
      ENDIF
      EXIT
    ENDIF

    IF TotQty = 0
      SKIP
      LOOP
    ENDIF

    SELECT STYLE

    *B125388,1 BWA 11/24/2004 Check if the style exsit in the style file or not.[START]
    *SEEK InvLine.Style
    llFondSty = IIF(SEEK(InvLine.Style , "STYLE") , .T. , .F.)
    *B125388,1 BWA 11/24/2004.[END]

    SELECT InvLine
    @ ROW,00 SAY TotQty + OrdLine.TotQty PICTURE '999999'
    @ ROW,06 SAY TotQty PICTURE '999999'
    @ ROW,14 SAY OrdLine.TotQty PICTURE '99999'
    @ ROW,20 SAY SUBSTR(Style,lnStylePos,lnStyleLen)

    *B125388,1 BWA 11/24/2004 Print the style descreption.[START]
    *@ ROW,33 SAY Style.Desc
    @ ROW,33 SAY IIF(llFondSty , Style.Desc, Desc1)
    *B125388,1 BWA 11/24/2004.[END]

    ROW = ROW + 1
    @ ROW,20 SAY SUBSTR(Style,lnClrPos,lnClrLen)

    *B125388,1 BWA 11/24/2004 Seek for the scale record.[START]
    *=SEEK('S'+Style.Scale,'Scale')
    =SEEK(IIF(llFondSty , 'S'+Style.Scale ,'S'+ORDLINE.Scale) ,'Scale')
    *B125388,1 BWA 11/24/2004.[END]

    @ ROW,33 SAY LEFT(ALLTRIM(Scale.Sz1),5)
    @ Row,39 SAY LEFT(ALLTRIM(Scale.Sz2),5)
    @ Row,45 SAY LEFT(ALLTRIM(Scale.Sz3),5)
    @ Row,51 SAY LEFT(ALLTRIM(Scale.Sz4),5)
    @ Row,57 SAY LEFT(ALLTRIM(Scale.Sz5),5)
    @ Row,63 SAY LEFT(ALLTRIM(Scale.Sz6),5)
    ROW = ROW + 1

    lnPieces = lnPieces + TotQty
    lnSubTotal = lnSubTotal + (Price * TotQty)

    lnCol = 33
    FOR lnI = 1 TO Scale.Cnt
      lcI = ALLTRIM(STR(lnI))
      @ ROW,lnCol SAY Qty&lcI PICTURE '99999'
      lnCol = lnCol + 6
    ENDFOR
      
    @ ROW,62 SAY Price PICTURE '9999.99'
    @ ROW,71 SAY (Price * TotQty) PICTURE '9999999.99' 
    ROW = ROW + 1

    SELECT InvLine
    SKIP
  ENDDO
  *** END LINE PROCESSING

  *---------------------------------------------------------------
  * CURRENTLY PROCESSING REGULAR LINE ITEMS
  *---------------------------------------------------------------
  lcEndPage = IIF(InvLine.Invoice = lcInvoice ,'1','0')

  IF lcEndPage = '1' AND !llNote_Loop
    =lfGetCont()   
    LOOP
  ENDIF

  *IF !llOrdNFin AND lfOrdNote()
  *  llOrdNFin = .T.
  *ENDIF  
  *=lfInvNote()

  IF llRpInvNot
    IF !llOrdNFin
      SELECT NotePad
      lnOldMemW = SET("MEMOWIDTH")
      IF TYPE + KEY <> 'B' + lcOrder
        SEEK 'B' + lcOrder
        ROW = ROW + 4
      ENDIF
      ROW = ROW + 1
      lnMemLins = MEMLINES(NotePad.MNOTES)
      DO WHILE lnNotOLine <= lnMemLins
        IF ROW >= lnMaxRow
          llNote_Loop = .T.
          *EXIT
        ELSE
          llNote_Loop = .F.
          @ ROW,05 SAY MLINE(MNOTES,lnNotOLine)
          ROW = ROW + 1
        ENDIF
        IF ROW >= lnMaxRow
          =lfGetCont()
          =lfGetInHdr()
        ENDIF  
        lnNotOLine = lnNotOLine + 1
      ENDDO
      IF !llNote_Loop
        lnNotOLine = 1
        ROW = ROW + 1 
      ELSE
        =lfGetCont() 
        LOOP
      ENDIF
      SET MEMOWIDTH TO lnOldMemW
    ENDIF

    SELECT NotePad
    IF TYPE + KEY <> 'C' + lcInvoice
      SEEK 'C' + lcInvoice
    ENDIF
    lnOldMemW = SET("MEMOWIDTH")
    ROW = ROW + 1 
    lnMemLins = MEMLINES(NotePad.MNOTES)
    DO WHILE lnNotILine <= lnMemLins
      IF ROW >= lnMaxRow
        llNote_Loop = .T.
        *EXIT
      ELSE
        llNote_Loop = .F.
        @ ROW,05 SAY MLINE(MNOTES,lnNotILine)
        ROW = ROW + 1    
      ENDIF
      IF ROW >= lnMaxRow
        =lfGetCont()
        =lfGetInHdr()
      ENDIF
      lnNotILine = lnNotILine + 1    
    ENDDO
    IF !llNote_Loop
      lnNotILine = 1
      ROW = ROW + 1 
    ELSE
      =lfGetCont() 
      LOOP
    ENDIF
    SET MEMOWIDTH TO lnOldMemW
  ENDIF
  
  SELECT InvHdr
  *-- Print footer.
  lnFRow=53
  @ lnFRow,33 SAY 'Total cartons    :'
  @ lnFRow,57 SAY InvHdr.Cartons PICTURE '99999'
  lnFRow = lnFRow + 2
  @ lnFRow,33 SAY 'Merchandise total:' 
  @ lnFRow,70 SAY lnSubTotal PICTURE '99999999.99'
  lnFRow = lnFRow + 1
  IF (Freight + Insur + COD) <> 0
    @ lnFRow,33 SAY 'Freight          :'
    @ lnFRow,70 SAY Freight + Insur + COD PICTURE '99999999.99' 
    lnFRow = lnFRow + 1
  ENDIF
  IF Discount <> 0
    @ lnFRow,33 SAY 'Discount         :'
    @ lnFRow,70 SAY Discount PICTURE '99999999.99'
    lnFRow = lnFRow + 1
  ENDIF
  @ lnFRow,33 SAY 'TOTAL            :'
  @ lnFRow,70 SAY InvHdr.TotalChg PICTURE '99999999.99'
  llOrdNFin  = .F.
  lnNotILine = 1
  lnNotOLine = 1
  lnPage = lnPage + 1
  @ 62,72 SAY 'PAGE : ' + ALLTR(STR(lnPage))
   
  SELECT InvHdr 
  SEEK lcInvoice
  IF gcDevice <> 'SCREEN'
    REPLACE PRTFLAG WITH 'P'
  ENDIF  
  llNewDoc = .T.
ENDSCAN
SET DEVICE TO SCREEN
RETURN

IF llOpenWarh
  USE IN WareHous
ENDIF
IF llOpnOrdLn
  USE IN OrdLine
ENDIF
IF llOpnSlsRp
  USE IN SalesRep
ENDIF
*-- End of report.

*!***************************************************************************
*! Name      : lfOrdNote
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/18/2002
*! Purpose   : Function to print Order NotePad function.
*!***************************************************************************
*! Called from : ARPINVIN.PRG
*!***************************************************************************
*Uptil now this function is not used anymore.
FUNCTION lfOrdNote
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT NotePad
lnOldMemW = SET("MEMOWIDTH")
IF TYPE + KEY <> 'B' + lcOrder
  SEEK 'B' + lcOrder
  ROW = ROW + 4
ENDIF
ROW = ROW + 1
lnMemLins = MEMLINES(NotePad.MNOTES)
DO WHILE lnNotOLine <= lnMemLins
  IF ROW >= lnMaxRow
    llNote_Loop = .T.
    EXIT
  ELSE
    llNote_Loop = .F.
    @ ROW,05 SAY MLINE(MNOTES,lnNotOLine)
    ROW = ROW + 1
  ENDIF
  IF ROW >= lnMaxRow
    =lfGetCont()
    =lfGetInHdr()
  ENDIF  
  lnNotOLine = lnNotOLine + 1
ENDDO
IF !llNote_Loop
  lnNotOLine = 1
  ROW = ROW + 1 
ELSE
  =lfGetCont() 
  LOOP
ENDIF

SET MEMOWIDTH TO lnOldMemW
SELECT (lcAlias)
RETURN (!llNote_Loop)
*-- End of lfOrdNote.

*!***************************************************************************
*! Name      : lfInvNote
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/18/2002
*! Purpose   : Function to print Invoice NotePad function.
*!***************************************************************************
*! Called from : ARPINVIN.PRG
*!***************************************************************************
*Uptil now this function is not used anymore.
FUNCTION lfInvNote
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT NotePad
IF TYPE + KEY <> 'C' + lcInvoice
  SEEK 'C' + lcInvoice
ENDIF
lnOldMemW = SET("MEMOWIDTH")
ROW = ROW + 1 
lnMemLins = MEMLINES(NotePad.MNOTES)
DO WHILE lnNotILine <= lnMemLins
  IF ROW >= lnMaxRow
    llNote_Loop = .T.
    EXIT
  ELSE
    llNote_Loop = .F.
    @ ROW,05 SAY MLINE(MNOTES,lnNotILine)
    ROW = ROW + 1    
  ENDIF
  IF ROW >= lnMaxRow
    =lfGetCont()
    =lfGetInHdr()
  ENDIF
  lnNotILine = lnNotILine + 1
    
ENDDO
IF !llNote_Loop
  lnNotILine = 1
  ROW = ROW + 1 
ELSE
  =lfGetCont() 
  LOOP
ENDIF
SET MEMOWIDTH TO lnOldMemW
SELECT (lcAlias)
RETURN (!llNote_Loop)
*-- End of lfInvNote.

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
IF !EMPTY(InvHdr.cFacCode)
  =SEEK(InvHdr.cFacCode,'SycFact')
  lcFacName   = SycFact.cFacComp
  laFactor[1] = gfGetAdr('SycFact' , '' , '' , '' , 1)
  laFactor[2] = gfGetAdr('SycFact' , '' , '' , '' , 2)
  laFactor[3] = gfGetAdr('SycFact' , '' , '' , '' , 3)
  *laFactor[4] = gfGetAdr('SycFact' , '' , '' , '' , 4)
  *laFactor[5] = gfGetAdr('SycFact' , '' , '' , '' , 5)
  =lfAdrShift('laFactor')
ENDIF

lcShipVia = gfCodDes(InvHdr.ShipVia , 'SHIPVIA')
lcTerms = gfCodDes(InvHdr.cTermCode , 'CTERMCODE')

SELECT Customer
=SEEK ('M'+lcAccount)
lcDuns = Duns
=SEEK(IIF(EMPTY(lcStore),'M'+lcAccount,'S'+lcAccount+lcStore))

lcSolTName = Customer.BTName
laSoldTo[1] = gfGetAdr('Customer' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('Customer' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('Customer' , '' , '' , '' , 3 , '2')
*laSoldTo[4] = gfGetAdr('Customer' , '' , '' , '' , 4 , '2')
*laSoldTo[5] = gfGetAdr('Customer' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

SELECT OrdHdr
SEEK 'O' + lcOrder
IF OrdHdr.Alt_ShpTo
  lcShpTName  = OrdHdr.STName
  laShipTo[1] = OrdHdr.cAddress1
  laShipTo[2] = OrdHdr.cAddress2
  laShipTo[3] = OrdHdr.cAddress3
  *laShipTo[4] = OrdHdr.cAddress4
  *laShipTo[5] = OrdHdr.cAddress5
ELSE    && Else
  
  *-- the below lines regarding the distribution center will not be used.
  lnCusRec = 0
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
  *laShipTo[4] = gfGetAdr('Customer' , '' , '' , '' , 4)
  *laShipTo[5] = gfGetAdr('Customer' , '' , '' , '' , 5)

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

*B125992,1 NADER 01/12/2005 (Begin) don't write 'C O N T I N U E D ...' if ROW<52 line because it
*B125992,1 NADER             is meaning that this invoice has one page only
IF ROW>=52
*B125992,1 NADER (End)

@ ROW+1,33 SAY 'C O N T I N U E D ...'
ENDIF
SELECT InvHdr
lnPage = lnPage + 1
@ 62,72 SAY 'PAGE : ' + ALLTRIM(STR(lnPage))
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

SELECT InvHdr
@ 01,70 SAY lcInvoice

IF lcCurLoop = '1'
  @ 05,57 SAY lcAccount
ENDIF

@ 10,20 SAY 'VENDOR #: '+ lcDuns
IF lcCurLoop = '1' AND !EMPTY(lcStore)
  @ 10,60 SAY 'STORE #: ' + lcStore
ENDIF
   
@ 12,10 SAY lcSolTName
IF lcCurLoop = '1'
  @ 12,48 SAY lcShpTName
ENDIF

@ 13,10 SAY laSoldTo[1]
IF lcCurLoop = '1'
  @ 13,48 SAY laShipTo[1]
ENDIF

@ 14,10 SAY laSoldTo[2]
IF lcCurLoop = '1'
  @ 14,48 SAY laShipTo[2]
ENDIF

@ 15,10 SAY laSoldTo[3]
IF lcCurLoop = '1'
  @ 15,48 SAY laShipTo[3]
ENDIF

IF !EMPTY(InvHdr.cFacCode) AND !EMPTY(lcFacName) AND llPrnFact
  @ 17,22 SAY 'REMIT TO: '
  @ 17,32 SAY lcFacName
  @ 18,32 SAY RTRIM(laFactor[1])+IIF(EMPTY(laFactor[3]),'',RTRIM(laFactor[2]))
  @ 19,32 SAY IIF(EMPTY(laFactor[3]),RTRIM(laFactor[2]),laFactor[3])
ENDIF

IF llWareHous
  =SEEK(InvHdr.cWareCode,'WAREHOUS')
  lcWhCity = WareHous.cAddress3
ENDIF

@ 20,02 SAY InvDate
@ 20,13 SAY SUBSTR(lcShipVia,1,19)
@ 20,33 SAY SUBSTR(lcWhCity,1,15)
@ 20,50 SAY lcTerms

*B606390,1 Shift this line one line up. [Begin]
*@ 23,02 SAY CustPo
*@ 23,21 SAY OrdHdr.Entered
*=SEEK(Rep1,'SalesRep')
*@ 23,34 SAY SalesRep.Name
*@ 23,66 SAY lcOrder
@ 22,02 SAY CustPo
@ 22,21 SAY OrdHdr.Entered
=SEEK(Rep1,'SalesRep')
@ 22,34 SAY SalesRep.Name
@ 22,66 SAY lcOrder
*B606390,1 Shift this line one line up. [End]

ROW = 26
SELECT (lcAlias)
*--End of lfGetInHdr.
