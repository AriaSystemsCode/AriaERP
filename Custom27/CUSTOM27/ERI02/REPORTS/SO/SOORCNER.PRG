*:***************************************************************************
*: Program file  : SOORCNER
*: Program desc. : Order confirmation for Eric Javits
*: For Report    : Order confirmation
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Sales Order (SO)
*: Developer     : AHMED MAHER (AMH)
*: Reference     : *C200515
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetCodes, lfEndGroup, lfwOldVal, lfvPO, lfvVend, lfGetLogo,
*:                 lfShiftArr
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOORCNER
*:***************************************************************************
*:Modifications  :
*:B607154,1 KHM 04/09/2003 Remove the booked and open qty as the customer uses one size
*:***************************************************************************

*-- If No records selected.
SELECT ORDHDR
SET SKIP TO
LOCATE FOR &lcRpExp
IF !FOUND()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ELSE
  llNoRec = .F.
ENDIF

IF FILE('&gcDataDir.SOORCNZ.MEM')
  RESTORE FROM &gcDataDir.SOORCNZ.MEM ADDITIVE
ELSE
  lcDevice = SET('DEVICE')
  *--Would you like to print the report by Pack_Id ?
  lcMpack  = IIF(gfModalGen('TRM32063B32000','DIALOG' )=1,'Y','N')
ENDIF

llPack = (lcMpack = 'Y')
SAVE ALL LIKE lcM* TO &gcDataDir.SOORCNZ.MEM
RELEASE ALL LIKE lcM*

*--Open needed files.
=gfOpenFile(gcDataDir+'Spck_Lin',gcDataDir+'Spck_Lin','SH')
=gfOpenFile(gcDataDir+'Spck_Hdr',gcDataDir+'Spck_Hdr','SH')

*--Initilize variables
DIMENSION laAddress[1]
STORE ''  TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6
STORE ''  TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6,laAddress
STORE ''  TO lcOrder,lcAccount,lcMulti,lcStore,laAddress,lcTermData,lcShipVia,lcDivison
STORE ' ' TO lcSpcInst,lcSeason,lcPrePack,XMER_DISC,lcScale,lcPrevScale
STORE 0   TO lnTotQty,lnTotOrd,XMER_AMNT,XNET_AMNT,ROW,lnScount
STORE 0   TO lnTotBook
STORE ''  To lcDcStore

llNrmPrt = .T.
DECLARE laDivLName[1,2]
laDivLName[1,1] = 'CDIVISION'
laDivLName[1,2] = 'lcDivLName'

*-- Get the style and color
lcMajTitl  = ALLTRIM(gfItemMask('HM'))
lcNonMajTl = ''
=lfGetColor()
A= REPLICATE('*',80)

*   ....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
B='|  START  | COMPLETE |    TERMS      |   SHIPVIA     | SEASON   |REP1 REP2'
*   MM/DD/YY   MM/DD/YY  123456789012345 123456789012345 1234567890 123  123

IF lcRpOrdTyp = 'T'
  C='      GRP '+lcMajTitl+SPACE(19-LEN(lcMajTitl+lcNonMajTl))+lcNonMajTl+'            TRANSMITTED PRICE  PIECES WH.SALE PRICE'
ELSE
  C='    GRP '+lcMajTitl+SPACE(19-LEN(lcMajTitl+lcNonMajTl))+lcNonMajTl+'                              PRICE PIECES     AMOUNT'
ENDIF

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*    STYLE   CLR                                     PRICE   PIECES       AMOUNT
*123 1234567 123 12345678901234567890              1234.99   123456   123,456.99
*--Fill the company address variables.
*--Center the address
laCompAdd1 = ''
laCompAdd2 = SPACE(ROUND((80-LEN(laCompAdd[2]))/2,0))+laCompAdd[2]
laCompAdd3 = SPACE(ROUND((80-LEN(laCompAdd[3]))/2,0))+laCompAdd[3]
laCompAdd4 = SPACE(ROUND((80-LEN(laCompAdd[4]))/2,0))+laCompAdd[4]
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
laCompAdd7 = ' Fax : ' + TRANSFORM(SyCcomp.Ccom_Fax, lcPhonPict)
laCompAdd[5] = 'Phone : '+TRANSFORM(SyCcomp.Ccom_phon, lcPhonPict)+ '   ' +laCompAdd7
laCompAdd6 = SPACE(ROUND((80-LEN(gcCom_Name))/2,0))+gcCom_Name
laCompAdd5 = SPACE(ROUND((80-LEN(laCompAdd[5]))/2,0))+laCompAdd[5]
lnMaxRow = 51
STORE 1 TO lnNoteLn,lnSvNoteLn 
lcMulTOrDT = gfTempName()

*-- SET ORDER TO cordtype+order+STR(lineno,6)
SET ORDER TO TAG ORDLINE IN ORDLINE
SET DEVICE TO PRINT

*------------------------------
* SECTION: MAIN LOOP
*------------------------------
lnPageNo=1
SELECT ORDHDR
*--Refresh the relation.
GO TOP

SCAN REST WHILE INKEY()<> 32 FOR &lcRpExp
  WAIT WINDOW 'PRINTING - <Space Bar> TO ABORT' NOWAIT
  *-----------------------------------------------------------------
  * Get and print invoice header.
  * Initialize document totals.
  *-----------------------------------------------------------------
  *-- Function to get order HEADER information.
  lnPageNo=1
  =lfGetOdHdr()
  SELECT OrdHdr
  *-- Function to print order HEADER information.  
  =lfPrintHdr()
  llRowInc= .F.
  IF !EMPTY(Note1) .AND. SUBSTR(Note1,1,1) <> '*' 
     llRowInc = .T.
     ROW = ROW +1
    @ ROW,00 SAY  Note1
  ENDIF       
  IF !EMPTY(Note2) .AND. SUBSTR(Note2,1,1) <> '*' 
     IF llRowInc = .F.
       ROW = ROW + 1
     ENDIF
    @ ROW,32 SAY Note2
  ENDIF       
  *-- START PRINTING THE ORDER LINES
  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
  *LN#   GRP STYLE           CLR                        PRICE   PIECES       AMOUNT
  *123456 X  123456789012345 123 12345678901234567890 1234.99   123456   123,456.99
  *            11111 22222 33333 44444 55555 66666 77777 88888
  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
  SET ORDER TO TAG ORDLINE IN ORDLINE

  IF lcMulti = 'Y'
    IF !USED(lcMulTOrDT)
      =gfOpenFile(gcWorkDir+lcMulTOrDT,'','EX')
    ELSE
      SELECT (lcMulTOrDT)
    ENDIF  
  ELSE
    SELECT ORDLINE
    SET ORDER TO TAG IIF(lcRpSortBy = 'L' , 'ORDLINE' , 'ORDLINST' ) IN ORDLINE
    LOCATE
    =SEEK(ORDHDR.cOrdType+lcOrder)
  ENDIF
  ROW =ROW +1
  SCAN WHILE EVAL(KEY()) = ORDHDR.cOrdType+lcOrder FOR IIF(lcRpBook = 'N',TOTQTY > 0,TOTQTY > 0 OR TOTQTY = 0 )
    IF lcMulti # 'Y' OR &lcMulTOrDT..Store = lcStore  &&this is the reverse of the second case
      IF ROW >= lnMaxRow
        lnPageNo = IIF(lcMulti <> 'Y' OR &lcMulTOrDT..Store = lcStore,lnPageNo+1,1 )
        *-- Function to print order HEADER information.  
        ROW = ROW+1
        @ ROW,00 SAY "--------------------C O N T I N U E D    N E X T    P A G E --------------------"
        =lfPrintHdr()
        
        ROW = ROW+1
      ENDIF  && ROW >= lnMaxRow
    ELSE
      *-- Print the note pad
      =lfPrtNtPad()
      *-- Say the total for the previous store
      XMER_DISC = OrdHdr->Disc
      XMER_AMNT = (XMER_DISC * lnTotOrd)/100
      XNET_AMNT = lnTotOrd - XMER_AMNT
      @ 53,00 SAY A
      IF lcRpBook = 'Y'
        @ 54,10 SAY 'GROSS OPEN AMOUNT      =>'          
      ELSE
        @ 54,10 SAY 'GROSS AMOUNT      =>'
      ENDIF
      IF lcRpDeciml = "Y"
        @ 54,70 SAY lnTotOrd PICTURE '999,999.99'
        @ 55,10 SAY 'MERCHANDISE DISC. =>'
        @ 55,40 SAY XMER_DISC PICTURE '999.99' + " %"
        @ 55,70 SAY XMER_AMNT PICTURE '999,999.99'
        IF lcRpBook = 'Y'
          @ 56,10 SAY 'NET OPEN AMOUNT      =>'
        ELSE
          @ 56,10 SAY 'NET AMOUNT      =>'
        ENDIF
        @ 56,63 SAY lnTotQty PICTURE '999999'
        @ 56,70 SAY ROUND(XNET_AMNT,2) PICTURE '999,999.99'
      ELSE
        @ 54,70 SAY lnTotOrd PICTURE '999,999999'
        @ 55,10 SAY 'MERCHANDISE DISC. =>'
        @ 55,40 SAY XMER_DISC PICTURE '999999' + " %"
        @ 55,70 SAY XMER_AMNT PICTURE '999,999999'
        IF lcRpBook = 'Y'
          @ 56,10 SAY 'NET OPEN AMOUNT        =>'
        ELSE
          @ 56,10 SAY 'NET AMOUNT        =>'
        ENDIF
        @ 56,63 SAY lnTotQty PICTURE '999999'
        @ 56,70 SAY XNET_AMNT PICTURE '999,999999'
      ENDIF
      IF lcRpBook = 'Y'
        @ 57,10 SAY 'BOOK QTY      =>'
        @ 57,63 SAY lnTotBook PICTURE '999999'
        @ 58,00 SAY A
        @ 59,22 SAY '______________________________________   ____________'
        @ 60,22 SAY 'CUSTOMER APPROVAL SIGNATURE                  DATE    '          
        @ 61,00 SAY REPLICATE('*',80)
        @ 62,00 SAY lcRpMsg1
        @ 63,00 SAY lcRpMsg2
        @ 64,00 SAY lcRpMsg3
      ELSE
        @ 57,00 SAY A
        @ 58,22 SAY '______________________________________   ____________'
        @ 59,22 SAY 'CUSTOMER APPROVAL SIGNATURE                  DATE    '
        @ 60,00 SAY REPLICATE('*',80)
        @ 61,00 SAY lcRpMsg1
        @ 62,00 SAY lcRpMsg2
        @ 63,00 SAY lcRpMsg3          
      ENDIF
      *-- Get the new store address.
      =lfGetStAd()
      *-- Function to print order HEADER information.
      lnPageNo=1
      =lfPrintHdr()
      ROW = ROW+1        && Increase row line
    ENDIF  && lcMulti # 'Y' OR &lcMulTOrDT..Store = lcStore  
    =SEEK(IIF(lcMulti = 'Y' , &lcMulTOrDT..Style,Ordline.Style),'STYLE' )
    lcStyDesc  = STYLE.Desc
    IF lcMulti = 'Y'
      =SEEK(ORDHDR.cOrdType+Order+STR(LineNo,6),'OrdLine')
    ENDIF
    lcPackKey = 'P'+lcAccount+OrdLine.Pack_Id
    IF !SEEK('P'+lcAccount+OrdLine.Pack_Id+Style,'Spck_Lin')
      =SEEK('P'+'*****'+OrdLine.Pack_Id+Style,'Spck_Lin')
      lcPackKey = 'P'+'*****'+OrdLine.Pack_Id
    ENDIF
    IF !SEEK('P'+lcAccount+OrdLine.Pack_Id,'Spck_Hdr')
      =SEEK('P'+'*****'+OrdLine.Pack_Id,'Spck_Hdr')
    ENDIF
    IF OrdLine.Pack_Id <> lcPrePack .AND. llPack
      lcOldAlias = ALIAS()
      SELECT Spck_Lin
      SUM REST WHILE Type+Account+Pack_Id+Style = lcPackKey ;
                     Spck_Lin.Qty1,Spck_Lin.Qty2,Spck_Lin.Qty3,;
                     Spck_Lin.Qty4,Spck_Lin.Qty5,Spck_Lin.Qty6 TO ARRAY lnQty

      SELECT (lcOldAlias)
      lcPrePack = OrdLine.Pack_Id
      IF !EMPTY(OrdLine.Pack_Id)
        @ ROW,00 SAY 'PACK_ID : '+OrdLine.Pack_Id+'  '+'DESC.:'+Spck_Hdr.Desc+' ' +;
                     +STR(lnQty[1],3)+' '+ STR(lnQty[2],3)+' '+STR(lnQty[3],3)+' ' +;
                     STR(lnQty[4],3)+' '+STR(lnQty[5],3)+' '+STR(lnQty[6],3)
        ROW=ROW+1
      ENDIF 
    ENDIF
    IF Scale<>lcPrevScale .AND. llNrmPrt   
      lcPrevScale = Scale 
      lcScale     = GETSCALE(Style.Scale,SPACE(1))   
      lnScount    = Scale.Cnt
      IF lnScount >1
        @ ROW,18 SAY lcScale
        ROW = ROW+1
      ENDIF
    ENDIF
    IF llNrmPrt
      @ ROW,00 SAY LineNo PICTURE '9999'
      @ ROW,05 SAY Group
      @ ROW,08 SAY SUBSTR(STYLE,1,12)
      @ ROW,21 SAY ALLTRIM(SUBSTR(gfCodDes(SUBSTR(STYLE,14) , 'COLOR'),1,12))
      @ ROW,34 SAY lcStyDesc
      IF lcRpDeciml = "Y"
        @ ROW,55 SAY Price    PICTURE '9999.99'
        @ ROW,63 SAY TotQty   PICTURE '999999'
        lnAmount= TotQty * Price
        @ ROW,70 SAY lnAmount  PICTURE '999,999.99'
      ELSE
        @ ROW,55 SAY Price    PICTURE '9999999'
        @ ROW,63 SAY TotQty   PICTURE '999999'
        lnAmount= TotQty * Price
        @ ROW,70 SAY lnAmount  PICTURE '999,999999'
      ENDIF
      ROW =ROW+1
      
      *B607154,1 KHM 04/09/2003 (Begin) Comment the following lines because all the styles
      *B607154,1                have only one size.      
      *IF lcRpBook = 'Y'
      *  @ ROW,00 SAY 'BOOK QTY'
      *  X = 1
      *  DO WHILE lnScount >1 .AND. X<=8
      *    Z =STR(X,1)
      *    @ ROW,(X*6)+12 SAY BOOK&Z   PICTURE '@Z 99999'
      *    X = X+1
      *  ENDDO
      *ENDIF      
      *ROW = ROW+1
      *@ ROW,00 SAY 'OPEN QTY'            
      *X = 1
      *DO WHILE lnScount >1 .AND. X<=8
      *  Z =STR(X,1)
      *  @ ROW,(X*6)+12 SAY QTY&Z   PICTURE '@Z 99999'
      *  X = X+1
      *ENDDO
      *ROW = ROW+1
      *B607154,1 KHM 04/09/2003 (End)
      
      * SUM ORDER TOTALS
      lnTotQty  = lnTotQty+ TotQty
      lnTotOrd  = lnTotOrd+ lnAmount
      lnTotBook = lnTotBook + TotBook
    ENDIF
    *-- Print the order line notes.
    IF llRpOrdLnt .AND. MEMLINES(Note_Mem) <> 0 .AND. ! lfPrtONte() 
      llNrmPrt = .F.
      lcAlias = ALIAS()
      SELECT ORDHDR
      *-- Function to print order HEADER information.  
      lnPageNo=lnPageNo+1
      =lfPrintHdr()
      llRowInc= .F.
      IF !EMPTY(Note1) .AND. SUBSTR(Note1,1,1) <> '*' 
        llRowInc= .F.
        ROW = ROW +1
        @ ROW,00 SAY  Note1
      ENDIF       
      IF !EMPTY(Note2) .AND. SUBSTR(Note2,1,1) <> '*' 
        IF llRowInc = .F.
          ROW = ROW +1
        ENDIF
        @ ROW,32 SAY Note2
      ENDIF
      SELECT (lcAlias)
      SKIP -1
      LOOP
    ELSE
      llNrmPrt = .T.
    ENDIF
    ROW = ROW+1
  ENDSCAN
  *------------------------ END PRINT LINE LOOP ----------------------
  *-- Print the note pad
  =lfPrtNtPad()
  *-- Print totals.
  XMER_DISC = OrdHdr->Disc
  XMER_AMNT = (XMER_DISC * lnTotOrd)/100
  XNET_AMNT = lnTotOrd - XMER_AMNT

  @ 53,00 SAY A
  
  IF lcRpBook = 'Y'
    @ 54,10 SAY 'GROSS OPEN AMOUNT      =>'          
  ELSE
    @ 54,10 SAY 'GROSS AMOUNT      =>'
  ENDIF     

  IF lcRpDeciml = "Y"
    @ 54,70 SAY lnTotOrd PICTURE '999,999.99'
    @ 55,10 SAY 'MERCHANDISE DISC. =>'
    @ 55,40 SAY XMER_DISC PICTURE '999.99' + " %"
    @ 55,70 SAY XMER_AMNT PICTURE '999,999.99'
    IF lcRpBook = 'Y'
      @ 56,10 SAY 'NET OPEN AMOUNT        =>'
    ELSE
      @ 56,10 SAY 'NET AMOUNT        =>'
    ENDIF
    @ 56,63 SAY lnTotQty PICTURE '999999'
    @ 56,70 SAY ROUND(XNET_AMNT,2) PICTURE '999,999.99'
  ELSE
    @ 54,70 SAY lnTotOrd PICTURE '999,999999'
    @ 55,10 SAY 'MERCHANDISE DISC. =>'
    @ 55,40 SAY XMER_DISC PICTURE '999999' + " %"
    @ 55,70 SAY XMER_AMNT PICTURE '999,999999'
    IF lcRpBook = 'Y'
      @ 56,10 SAY 'NET OPEN AMOUNT        =>'
    ELSE
      @ 56,10 SAY 'NET AMOUNT        =>'
    ENDIF
    @ 56,63 SAY lnTotQty PICTURE '999999'
    @ 56,70 SAY XNET_AMNT PICTURE '999,999999'
  ENDIF
 
  IF lcRpBook = 'Y'
    @ 57,10 SAY 'BOOK QTY      =>'
    @ 57,63 SAY lnTotBook PICTURE '999999'

    @ 58,00 SAY A
    @ 59,22 SAY '______________________________________   ____________'
    @ 60,22 SAY 'CUSTOMER APPROVAL SIGNATURE                  DATE    '
    @ 61,00 SAY REPLICATE('*',80)
    @ 62,00 SAY lcRpMsg1
    @ 63,00 SAY lcRpMsg2
    @ 64,00 SAY lcRpMsg3
  ELSE
    @ 57,00 SAY A
    @ 58,22 SAY '______________________________________   ____________'
    @ 59,22 SAY 'CUSTOMER APPROVAL SIGNATURE                  DATE    '
    @ 60,00 SAY REPLICATE('*',80)
    @ 61,00 SAY lcRpMsg1
    @ 62,00 SAY lcRpMsg2
    @ 63,00 SAY lcRpMsg3          
  ENDIF
ENDSCAN

*--Close files opened in this file.
IF USED(lcMulTOrDT)
  USE IN (lcMulTOrDT)
  ERASE (gcWorkDir+lcMulTOrDT) + '.DBF'
  ERASE (gcWorkDir+lcMulTOrDT) + '.CDX'
  ERASE (gcWorkDir+lcMulTOrDT) + '.FPT'
ENDIF
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfPrtONte()
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/16/2003
*! Synopsis  : Print Order line notes.
*!*************************************************************
FUNCTION lfPrtONte

ROW = ROW-1
lnNoteLn = lnSvNoteLn 
DO WHILE lnNoteLn <= MEMLINES(Note_Mem)
  IF ROW >= lnMaxRow - 1
    lnSvNoteLn = lnNoteLn
    RETURN(.F.)
  ELSE
    ROW = ROW + 1
    @ ROW,01 SAY MLINE(Note_Mem,lnNoteLn)
  ENDIF
  lnNoteLn = lnNoteLn + 1
ENDDO
IF lnNoteLn > MEMLINES(Note_Mem)
  ROW = ROW + 1
ENDIF
lnSvNoteLn = 1
RETURN(.T.)

*!*************************************************************
*! Name      : lfGetOdHdr
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/16/2003
*! Purpose   : TO get the information of the ORDER header.
*!*************************************************************
FUNCTION lfGetOdHdr

PRIVATE lcAlias , lcCurrKey
lcCurrKey = ""

lcAlias = ALIAS()
SELECT ORDHDR
lcOrder   = Order
lcAccount = Account
lcMulti   = Multi
IF lcMulti = 'Y'
  IF USED(lcMulTOrDT)
    SELECT (lcMulTOrDT)
    USE
  ENDIF
  SELECT ORDLINE
  =SEEK(ordhdr.cOrdtype+lcOrder)  
  COPY REST TO (gcWorkDir+lcMulTOrDT) WHILE cordtype+order+STR(lineno,6) = ordhdr.cOrdtype+lcOrder
  =gfOpenFile(gcWorkDir+lcMulTOrDT,'','EX')
  IF lcRpSortBy = 'S'
    IF llPack
      INDEX ON cOrdType+Order+Store+Style+STR(LineNo,6)+Pack_Id TAG (lcMulTOrDT)
    ELSE 
      INDEX ON cOrdType+Order+Store+Style+STR(LineNo,6) TAG (lcMulTOrDT)
    ENDIF
  ELSE
    INDEX ON CORDTYPE+ORDER+STORE+STR(LINENO,6) TAG (lcMulTOrDT)
  ENDIF
  GO TOP  
  lcStore = Store
  =SEEK('S'+lcAccount+lcStore,'CUSTOMER')  
ENDIF
lcStore = IIF(lcMulti <> 'Y',ORDHDR.STORE,lcStore)

IF lcMulti = 'Y'
  IF !EMPTY(CUSTOMER.DIST_CTR)  
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    lcDCStore = lcStore + ' DC:' + Customer.Store
  ELSE
    lcDcStore = lcStore 
  ENDIF
  IF !EMPTY(lcCurrKey)
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF
ELSE
  lcDcStore = lcStore 
ENDIF  

STORE '' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6
lcBtName  = CUSTOMER.BTNAME
=gfGetAdr('Customer','','','',1,'2')
*--Get the Bill To adddess except the country.
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcBtAddr&lcCount = lcBtAddr&lcCount + IIF(EMPTY(lcBtAddr&lcCount),'',',')+;
  ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
ENDFOR
*-- SHIP_TO ADDRESS FOR THIS STORE
STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
IF OrdHdr.Alt_ShpTo
  lcStName  = ALLTRIM(OrdHdr.STNAME)
  lcStAddr1 = ALLTRIM(OrdHdr.cAddress1)
  lcStAddr2 = ALLTRIM(OrdHdr.cAddress2)
  lcStAddr3 = ALLTRIM(OrdHdr.cAddress3)+','+ALLTRIM(OrdHdr.cAddress4)+','+ALLTRIM(OrdHdr.cAddress5)
ELSE
  *--Get the Ship To adddess except the country.    
  IF !EMPTY(CUSTOMER.DIST_CTR)  
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
  ENDIF
  lcStName  = ALLTRIM(IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA))
  lcStAddr1 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  lcStAddr2 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  lcStAddr3 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  lcStAddr4 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  lcStAddr5 = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5))
  IF !EMPTY(lcCurrKey)
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF
ENDIF  

*-- GET DESCRIPTIONS FOR CODED FIELDS
SELECT CODES

*--Terms
lcTermData =SUBSTR(gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE'),1,15)
*--ShipVia
IF lcMulti = 'Y'
  IF ALLTRIM(ORDHDR.ShipVia) = '*'
    lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
  ELSE
    lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,15)
  ENDIF  
ELSE
  lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,15)
ENDIF

*--Division long name.
STORE '' TO lcDivLName
=gfRltFld(OrdHdr.cDivision,@laDivLName,'CDIVISION')
*--Special instruction
lcSpcInst = SUBSTR(gfCodDes(OrdHdr.SPCINST , 'SPCINST'),1,15)
*---Season
lcSeason = SUBSTR(gfCodDes(OrdHdr.SEASON , 'SEASON'),1,10)
*--Get the first line of the company address.
laCompAdd1 = IIF(!EMPTY(lcDivLName),lcDivLName,laCompAdd[1])
laCompAdd1 = SPACE(ROUND((80-LEN( ALLTRIM(laCompAdd1) ))/2,0))+laCompAdd1
NoteLoop = .F.
STORE 0.00 TO lnTotQty , lnTotOrd , lnTotBook
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfPrintHdr
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/16/2003
*! Purpose   : TO print the information of the ORDER header.
*!*************************************************************
FUNCTION lfPrintHdr
PRIVATE lcAlias

lcAlias = ALIAS()
@ 01,01 SAY laCompAdd6 
@ 02,01 SAY laCompAdd1
@ 03,01 SAY laCompAdd2
@ 04,01 SAY laCompAdd3
@ 05,01 SAY laCompAdd4
@ 06,01 SAY laCompAdd5
IF lnPageNo=1
  @ 08,24 SAY 'O R D E R   C O N F I R M A T I O N'
ENDIF
@ 09,60 SAY 'Page : ' +ALLTRIM(STR(lnPageNo))
SELECT OrdHdr
@ 10,03 SAY 'ORDER: '+Order
@ 10,60 SAY 'DATE: '
@ 10,66 SAY ENTERED
IF lnPageNo>1
  @ 11,03 SAY 'STORE: ' + lcStore
ENDIF
@ 11,60 SAY 'PO #: '
@ 11,66 SAY IIF(MultiPO,&lcMulTOrDT..CUSTPO,CUSTPO) 
ROW=13  
IF lnPageNo=1
  @ 13,04 SAY '........ SOLD TO ........'
  @ 13,46 SAY '........ SHIP TO ........'
  @ 14,04 SAY Account
  
  @ 14,40 SAY 'STORE:' + lcDCStore + '  DEPT:' + Dept 
  @ 15,04 SAY lcBtName
  @ 15,40 SAY lcStName
  @ 16,04 SAY lcBtAddr1
  @ 16,40 SAY lcStAddr1
  @ 17,04 SAY lcBtAddr2
  @ 17,40 SAY lcStAddr2
  @ 18,04 SAY lcBtAddr3
  @ 18,40 SAY lcStAddr3

  @ 20,00 SAY A
  @ 21,00 SAY B
  @ 22,00 SAY A
  @ 23,01 SAY START
  @ 23,12 SAY COMPLETE
  @ 23,22 SAY lcTermData  
  @ 23,38 SAY SUBSTR(lcShipVia,1,15)
  @ 23,54 SAY lcSeason
  @ 23,65 SAY REP1
  @ 23,70 SAY REP2
  ROW=24
ENDIF
@ ROW,00 SAY A
@ ROW+1,00 SAY C
@ ROW+2,00 SAY A
ROW = ROW+2
lcScale     = ' '
lcPrevScale = ' ' 
lnScount    = 0
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfGetStAd
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/16/2003
*! Purpose   : TO get the store address.
*!*************************************************************
FUNCTION lfGetStAd

PRIVATE lcAlias , lcCurrKey
lcCurrKey = ""

lcAlias = ALIAS()
lcStore   = Store
lcAccount = Account
=SEEK('S'+lcAccount+lcStore,'CUSTOMER')    
STORE '' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6

IF ALLTRIM(ORDHDR.ShipVia) = '*'
  lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
ELSE
  lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,15)
ENDIF  

IF lcMulti = 'Y'
  IF !EMPTY(CUSTOMER.DIST_CTR)  
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    lcDCStore = lcStore + ' DC:' + Customer.Store
  ELSE
    lcDcStore = lcStore 
  ENDIF
  IF !EMPTY(lcCurrKey)
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF
ELSE
  lcDcStore = lcStore 
ENDIF  

lcBtName  = CUSTOMER.BTNAME
=gfGetAdr('Customer','','','',1,'2')

*--Get the Bill To adddess except the country.
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcBtAddr&lcCount = lcBtAddr&lcCount + IIF(EMPTY(lcBtAddr&lcCount),'',',')+;
  ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
ENDFOR

*--Get the Ship To adddess except the country.    
IF !EMPTY(CUSTOMER.DIST_CTR)  
  lcCurrKey = 'S' + Customer.Account + Customer.Store
  =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
ENDIF

lcStName  = IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA)
lcStAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
lcStAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
lcStAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
lcStAddr4 = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
lcStAddr5 = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

IF !EMPTY(lcCurrKey)
  = SEEK(lcCurrKey , 'CUSTOMER')
ENDIF  

STORE 0.00 TO lnTotQty , lnTotOrd , lnTotBook
NoteLoop = .F.    
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfPrtNtPad
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/16/2003
*! Purpose   : TO print the note pad.
*!*************************************************************
FUNCTION lfPrtNtPad
PRIVATE lcAlias

lcAlias = ALIAS()
IF llRpOrdNot
  SELECT NotePad
  SEEK 'B'+lcOrder
  lnMline = 1
  *---------------------------------------------------
  * [NOTEPAD] COMMENT LINES
  *---------------------------------------------------
  lnMemWidth = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  lnMTotLin = MEMLINES(NotePad.MNOTES)
  DO WHILE lnMline <= lnMTotLin
    SELECT NotePad
    IF ROW >= lnMaxRow
      =lfPrnConti()
      =lfPrintHdr()
      ROW = ROW+1
    ENDIF
    
    IF  '*' <> LEFT(MLINE(MNOTES,lnMline),1)  
      @ ROW,04 SAY MLINE(MNOTES,lnMline)
      ROW = ROW+1
    ENDIF
    lnMline = lnMline + 1
  ENDDO
  IF llRpOrdNot
    SET MEMOWIDTH TO lnMemWidth
  ENDIF  
ENDIF
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfGetColor
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/16/2003
*! Purpose   : To get the length to of the color.
*!*************************************************************
FUNCTION lfGetColor

lcNonMajTl = ''
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR

*!*************************************************************
*! Name      : lfPrnConti
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/16/2003
*! Purpose   : Function to print the continue if the order
*!           : Was more than one page
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None. 
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  lfPrnConti()
*!*************************************************************
FUNCTION lfPrnConti

@ 53,00 SAY REPLICATE('-',80)
@ 54,13 SAY 'C O N T I N U E D ...'
@ 54,70    SAY '**********'
@ 55,00 SAY REPLICATE('-',80)
*--End Of lfPrnConti.