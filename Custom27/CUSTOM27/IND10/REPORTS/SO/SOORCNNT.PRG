*****************************************************************************
*: Program file  : SOORCNNT.PRG  (C102197,1)
*: Program desc. : ORDER CONFIRMATION FORMAT Z 8 1/2" * 11" FOR INDUSTRIE WEAR, INC.
*: Module        : Aria Apparel Series.
*: Developer     : Mohamed Shokry -(MHM)
*: Date          : 02/25/2001
*****************************************************************************
*B804131,1 AAN 04/17/2001 Change "DOZENS" word in the header to "PIECES"
*B804131,1                and qualifier "DZ" to "UNITS"
*B129386,1 NNA 11/10/2005 Fix bug that if the Row >=(50) the Ordline.Note_Mem will not printing
*****************************************************************************
*--to get three messages in Option Grid
STORE SPACE(75) TO lcMnote1 ,lcMnote2 ,lcMnote3 

lcStrToPrn = SPACE(1)
STORE 0 TO lnTotCtn, lnWeight,lnLWeight,lnCartons, lnTWeight, ROW

lcCustPo  = SPACE(1)
lnMTotLin = 0

*--MESSAGES  
lcNote1 = lcRpMsg1
lcNote2 = lcRpMsg2 
lcNote3 = lcRpMsg3

**********************
=gfOpenFile(gcDataDir+'SPCK_LIN',gcDataDir+'SPCK_LIN','SH')
=gfOpenFile(gcDataDir+'SPCK_HDR',gcDataDir+'SPCK_HDR','SH')
=gfOpenFile(gcDataDir+'SkuTmpl',gcDataDir+'SkuTmpl','SH')

OrdLTemp = gfTempName()
OrdPTemp = gfTempName()
SELECT OrdLine 
COPY STRUCTURE TO &gcWorkDir.&OrdPTemp 
=gfOpenFile(gcWorkDir+OrdPTemp,'','EX')

*--To get orders from order temp file [start]
*:C102197,4 change collecting data [start]

*OrdHtemp = ''
*lnPosition = ASUBSCRIPT(laOGVrFlt,ASCAN(laOGVrFlt,'ORDHDR.ORDER'),1)
*IF lnPosition > 0
*  OrdHtemp = laOGVrFlt[lnPosition,6]
*  IF !EMPTY(OrdHtemp) AND USED(OrdHtemp)
*    SELECT (OrdHtemp)
*    LOCATE
 *   llchkfile = .F.
*  ELSE
*    *--if you did not choose any we will get all selected record from filter
*    CREATE CURSOR (OrdHtemp)  (ORDER C(6))
*    SELECT (lcTempOrd)
*    LOCATE
*    lcOldOrd = ''
*    llchkfile = .T.
*    SCAN 
*      IF lcOldOrd # &lcTempOrd..Order
*        SELECT (OrdHtemp)
*        APPEND BLANK
*        REPLACE ORDER WITH &lcTempOrd..Order
*        lcOldOrd = &lcTempOrd..Order
*      ENDIF  
*    ENDSCAN
*  ENDIF  
*ENDIF
STORE '' TO lcOldOrder 
OrdHtemp = gfTempName()
CREATE CURSOR (OrdHtemp) (ORDER C(6) )

SELECT (lcTempOrd)
LOCATE
IF EOF()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  *-- Variable to do EndReport Or Don't.
  llNoRec = .T.
  RETURN
ELSE
  *-- Variable to do EndReport Or Don't.
  llNoRec = .F.
ENDIF

SCAN 
  IF lcOldOrder # &lcTempOrd..Order
    INSERT INTO (OrdHtemp) (ORDER ) VALUES (&lcTempOrd..Order)
    lcOldOrder = &lcTempOrd..Order
  ENDIF  
ENDSCAN
*--To get orders from order temp file [end]
*:C102197,4 change collecting data  [end]

*-- Print the Form.
SET DEVICE TO PRINT

SELECT (OrdHtemp)
SCAN
  lnOrder = Order
  =SEEK("O" +  lnOrder,'OrdLine') 
  DO WHILE  lnOrder = OrdLine.Order
    SELECT (OrdPTemp)  
    APPEND BLANK 
    REPLACE  Order    WITH OrdLine.Order            ; 
             Style    WITH OrdLine.Style            ;
             Store    WITH OrdLine.Store            ;
             LineNo   WITH OrdLine.LineNo           ;
             Pack_Id  WITH OrdLine.Pack_Id
    SELECT OrdLine
    SKIP
  ENDDO 
ENDSCAN
SELECT (OrdPTemp)
LOCATE
IF llPack
  IF lcRpSortBy = 'S'
    INDEX ON Order+Store+Style+STR(LineNo,6)+Pack_Id  TAG &OrdPTemp
  ELSE
    INDEX ON Order+Store+STR(LineNo,6)+Style+Pack_Id  TAG &OrdPTemp
  ENDIF
ELSE
  IF lcRpSortBy = 'S'
    INDEX ON Order+Store+Style+STR(LineNo,6)  TAG &OrdPTemp
  ELSE
    INDEX ON Order+Store+STR(LineNo,6)+Style  TAG &OrdPTemp
  ENDIF
ENDIF
SET ORDER TO TAG (OrdPTemp)

SELECT (OrdHtemp)
GO TOP

A= REPLICATE('*',80)
lcPrePack  = ' '
*   ....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
B='|  START  | COMPLETE |    TERMS      |   SHIPVIA     | SEASON   |REP1 REP2'
*   MM/DD/YY   MM/DD/YY  123456789012345 123456789012345 1234567890 123  123

*-- B804131,1 AAN Change "DOZENS" word to "PIECES" [Begin]
*C='      GRP STYLE        COLOR                         PRICE  DOZENS       AMOUNT'
C='       STYLE       SUB COLOR                         PRICE  PIECES       AMOUNT'
*-- B804131,1 AAN Change "DOZENS" word to "PIECES" [End]

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*    STYLE   CLR                                     PRICE   PIECES       AMOUNT
*123 1234567 123 12345678901234567890              1234.99   123456   123,456.99

*-------------------------------
* SETUP HEADINGS
*-------------------------------

laCompAdd1 = ''
laCompAdd2 = SPACE(ROUND((80-LEN(laCompAdd[1]))/2,0))+laCompAdd[1]
laCompAdd3 = SPACE(ROUND((80-LEN(laCompAdd[2]))/2,0))+laCompAdd[2]
laCompAdd4 = laCompAdd[3]
laCompAdd5 = SycComp.cCom_phon
laCompAdd6 = ''
IF EMPTY(ALLTRIM(laCompAdd5))
  lcLen=LEN(laCompAdd4)
  laCompAdd4=SPACE(ROUND((75-lcLen)/2,0))+'TEL :'+SycComp.cCom_phon
  laCompAdd5=SPACE(ROUND((75-lcLen)/2,0))+'FAX :'+SycComp.cCom_Fax
ELSE
  lcLen=LEN(laCompAdd5)
  laCompAdd4=SPACE(ROUND((80-LEN(laCompAdd4))/2,0))+laCompAdd4
  laCompAdd5=SPACE(ROUND((75-lcLen)/2,0))+'TEL :'+SycComp.cCom_phon
  laCompAdd6=SPACE(ROUND((75-lcLen)/2,0))+'FAX :'+SycComp.cCom_Fax
ENDIF

MaxRow = 51

STORE 1 TO lnNoteLn,lnSvNoteLn 
llNrmPrt = .T.

SELECT OrdHdr

*------------------------------
* SECTION: MAIN LOOP
*------------------------------
NewDoc = .T.

WAIT WINDOW 'PRINTING - <Space Bar> TO ABORT' NOWAIT

*:C102197,4  [start]
lcDuns   = gfGetMemVar('XDUNS')
*:C102197,4 [end]

DO WHILE INKEY() <>32
  SELECT &OrdHTemp
  IF EOF()
    EXIT
  ENDIF
  IF FLAG = 'N'
    SKIP
    LOOP
  ENDIF
  IF NewDoc
    
    STORE 0 TO lnCartons, lnLweight
    
    XOrder = &OrdPTemp->Order
    SELECT OrdHdr
    SEEK "O" + XOrder
    IF EOF()
      SELECT &OrdHTemp
      SKIP
      LOOP
    ENDIF
    xMulti = Multi
    xAccount = OrdHdr->Account
    SELECT OrdLine
    SEEK "O" + XOrder
    IF EOF()
      SELECT &OrdHTemp
      SKIP
      LOOP
    ENDIF
    IF OPENED(OrdLTemp)
      SELECT &OrdLTemp
      USE
    ENDIF
    SELECT OrdLine
    IF xMulti = 'Y'
      COPY REST TO &gcWorkDir.&OrdLTemp FOR "O"+Order = "O" + XOrder
      =gfOpenFile(gcWorkDir+OrdLTemp,'','EX')
      IF llPack
        INDEX ON Store+Style+STR(LineNo,6)+Pack_Id TAG &OrdLTemp
      ELSE 
        INDEX ON Store+Style+STR(LineNo,6) TAG &OrdLTemp
      ENDIF
      GO TOP
      xStore  = Store
      lcCustPo = CUSTPO
      SELECT Customer
      SEEK 'S'+xAccount+xStore
      XBTNAME  = BTNAME
      XBTADDR1 = caddress12
      XBTADDR2 = caddress22
      XBTADDR3 = TRIM(LEFT(cAddress32,15)) + ' ' +TRIM(LEFT(cAddress42,3)) + ' ' + LEFT(cAddress52,10)
      IF LEN(TRIM(XBTADDR2)) =0
        XBTADDR2 = XBTADDR3
        XBTADDR3 = ''
      ENDIF

      *-- Initialize the alt address if ther is any. 04/07/94 MFM.
      SELECT OrdHdr
      SEEK "O" + xOrder
      IF Alt_ShpTo
        SELECT Customer
        SEEK 'S'+xAccount+xStore
        XSTNAME  = STNAME
        XSTADDR1 = cAddress1
        XSTADDR2 = cAddress2
        XSTADDR3 = TRIM(LEFT(cAddress3,15)) + ' ' +TRIM(LEFT(cAddress4,3)) + ' ' + LEFT(cAddress5,10)
        IF LEN(TRIM(XSTADDR2)) =0
          XSTADDR2 = XSTADDR3
          XSTADDR3 = ''
        ENDIF
      ELSE
        SELECT Customer
        SEEK 'S'+xAccount+xStore
        XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
        XSTADDR1 = cAddress1
        XSTADDR2 = cAddress2
        XSTADDR3 = TRIM(LEFT(cAddress3,15)) + ' ' +TRIM(LEFT(cAddress4,3)) + ' ' + LEFT(cAddress5,10)
        IF LEN(TRIM(XSTADDR2)) =0
          XSTADDR2 = XSTADDR3
          XSTADDR3 = ''
        ENDIF
      ENDIF
    ENDIF           && IF MULTI STORE

    SELECT OrdHdr

    *------------------------------------
    * GET DESCRIPTIONS FOR CODED FIELDS
    *------------------------------------
    XTERM_DATA = gfCodDes(ORDHDR.cTermCode, 'CTERMCODE '  )

    XSHIP_DATA = gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'  )

    *--get division long name
    *XDIVI_DATA = gfCodDes(ORDHDR.cDivision, 'CDIVISION '  )
    XDIVI_DATA = ''
    DIMENSION laDivLName[1,2]
    laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
    laDivLName[1,2] = 'XDIVI_DATA'
    =gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')

    XSEAS_DATA = gfCodDes(ORDHDR.Season, 'SEASON    '  )

    * LINE 1
    laCompAdd1 = IIF( !EMPTY(xDivi_Data) , xDivi_Data, gcCom_Name)
    laCompAdd1 = SPACE(ROUND((80-LEN( ALLTRIM(laCompAdd1) ))/2,0))+ALLTRIM(laCompAdd1)
    ***
    ****** IF NOT MULTIPLE STORE Order GET SHIPTO AND BILLTO
    ***
    IF xMulti <> 'Y'
      SELECT Customer
      SEEK IIF(OrdHdr->Store=SPACE(8),'M'+xAccount,'S'+xAccount+OrdHdr->Store)
      XBTNAME  = BTNAME
      XBTADDR1 = cAddress12
      XBTADDR2 = cAddress22
      XBTADDR3 = TRIM(LEFT(cAddress32,15)) + ' ' +TRIM(LEFT(cAddress42,3)) + ' ' + LEFT(cAddress52,10)
      IF LEN(TRIM(XBTADDR2)) =0
        XBTADDR2 = XBTADDR3
        XBTADDR3 = ''
      ENDIF
      xStore   = Store
      *-- Initialize the alt address if ther is any. 04/07/94 MFM.
      SELECT OrdHdr
      SEEK "O" + xOrder
      lcCustPo = CUSTPO
      IF Alt_ShpTo
        SELECT Customer
        XSTNAME  = STNAME
        XSTADDR1 = cAddress1
        XSTADDR2 = cAddress2
        XSTADDR3 = TRIM(LEFT(cAddress3,15)) + ' ' +TRIM(LEFT(cAddress4,3)) + ' ' + LEFT(cAddress5,10)
        IF LEN(TRIM(XSTADDR2)) =0
          XSTADDR2 = XSTADDR3
          XSTADDR3 = ''
        ENDIF
      ELSE
        SELECT Customer
        XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
        XSTADDR1 = cAddress1
        XSTADDR2 = cAddress2
        XSTADDR3 = TRIM(LEFT(cAddress3,15)) + ' ' +TRIM(LEFT(cAddress4,3)) + ' ' + LEFT(cAddress5,10)
        IF LEN(TRIM(XSTADDR2)) =0
          XSTADDR2 = XSTADDR3
          XSTADDR3 = ''
        ENDIF
      ENDIF
    ENDIF

    NoteLoop = .F.
    xTotQty  = 0.00
    xTotOrd  = 0.00
  ENDIF &&& NewDoc

  DO WHILE  xMulti = 'Y'
    IF (&OrdLTemp->Store=xStore)  .OR. (NoteLoop)
      EXIT
    ENDIF
    xStore   = &OrdLTemp->Store
    lcCustPo  = &OrdLTemp->CUSTPO
    xAccount = &OrdLTemp->Account

    SELECT Customer
    SEEK 'S'+xAccount+xStore
    XBTNAME  = BTNAME
    XBTADDR1 = cAddress12
    XBTADDR2 = cAddress22
    XBTADDR3 =  TRIM(LEFT(cAddress32,15)) + ' ' +TRIM(LEFT(cAddress42,3)) + ' ' + LEFT(cAddress52,10)
    IF LEN(TRIM(XBTADDR2)) =0
      XBTADDR2 = XBTADDR3
      XBTADDR3 = ''
    ENDIF
    XSTNAME  = IIF( EMPTY(DBA),STNAME,DBA)
    XSTADDR1 = cAddress1
    XSTADDR2 = cAddress2
    XSTADDR3 = TRIM(LEFT(cAddress3,15)) + ' ' +TRIM(LEFT(cAddress4,3)) + ' ' + LEFT(cAddress5,10)
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
    xTotQty  = 0.00
    xTotOrd  = 0.00
    NoteLoop = .F.
    EXIT
  ENDDO

  SELECT OrdHdr
  IF xMulti = 'Y'
    SELECT &OrdLTemp
    IF Totqty <=0
      NewDoc = .F.
      IF EOF() .AND. !NoteLoop
        EXIT
      ELSE
        IF !EOF() 
          SKIP
          SKIP IN &OrdPTemp  
          LOOP
        ENDIF        
      ENDIF
    ENDIF
  ENDIF
  =lfPrnHdr()

  ****** START PRINTING THE ORDER LINES
   
  NewDoc   = .T.
  xScale   = ' '

  xPrevScale = ' ' 
  xCount     = 0

  IF xMulti = 'Y'
    SELECT &OrdLTemp
    IF llPack
      =SEEK(&OrdPTemp->Store+;
      &OrdPTemp->Style+STR(&OrdPTemp->LineNo,6)+&OrdPTemp->Pack_Id)
    ELSE 
      =SEEK(&OrdPTemp->Store+;
      &OrdPTemp->Style+STR(&OrdPTemp->LineNo,6))
    ENDIF
  ELSE
    SELECT OrdLine
  ENDIF

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*LN#   STYLE         SUB  CLR                        PRICE   PIECES       AMOUNT
*123456123456789012  123  12345678901234567890 1234.99   123456   123,456.99
*            11111 22222 33333 44444 55555 66666 77777 88888
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....

  DO WHILE .T.
    DO CASE
      CASE EOF() .OR. &OrdPTemp->Order <> xOrder
        NewDoc   = .T.
        EXIT
      CASE Store # xStore .AND. xMulti = 'Y'
        NewDoc = .F.
        EXIT
      CASE TotQty<=0
        SKIP
        SKIP IN (OrdPTemp)  
        LOOP
      CASE Row>=MaxRow
        lcPrePack  = ' '
        NewDoc = .F.
        =lfPrnCont()
        =lfPrnHdr()
    ENDCASE
    Key = Style
    SELECT Style
    SEEK Key
    xStyDesc = Desc

    lnQty_Ctn = Qty_Ctn
    lnWeight  = Style.nstyweight
    IF xMulti = 'Y'
      IF llPack
        =SEEK(&OrdPTemp->Store+&OrdPTemp->Style+STR(&OrdPTemp->LineNo,6)+&OrdPTemp->Pack_Id,'&OrdLTemp')
      ELSE
        =SEEK(&OrdPTemp->Store+&OrdPTemp->Style+STR(&OrdPTemp->LineNo,6),'&OrdLTemp')
      ENDIF
      =SEEK(&OrdPTemp->Order+STR(&OrdPTemp->LineNo,6),'OrdLine')
    ENDIF

    =SEEK('P'+xAccount+OrdLine.Pack_Id+&OrdPTemp->Style,'Spck_Lin')
    IF !SEEK('P'+xAccount+OrdLine.Pack_Id,'Spck_Hdr')
      =SEEK('P'+'*****'+OrdLine.Pack_Id,'Spck_Hdr')
    ENDIF
            
    IF OrdLine.Pack_Id <> lcPrePack .AND. llPack
      lcOldAlias = ALIAS()
      SELECT Spck_Lin
      SUM REST WHILE OrdLine.Pack_Id = Spck_Lin.Pack_Id Spck_Lin.Qty1,;
               Spck_Lin.Qty2,Spck_Lin.Qty3,Spck_Lin.Qty4,;
               Spck_Lin.Qty5,Spck_Lin.Qty6 TO ARRAY lnQty
      SELECT (lcOldAlias)
      lcPrePack = OrdLine.Pack_Id
      IF !EMPTY(OrdLine.Pack_Id)
        @ ROW,00 SAY 'PACK_ID : '+OrdLine.Pack_Id+'  '+'DESC.:'+Spck_Hdr.Desc+' ' +;
                     +STR(lnQty[1],3)+' '+ STR(lnQty[2],3)+' '+STR(lnQty[3],3)+' ' +;
                     STR(lnQty[4],3)+' '+STR(lnQty[5],3)+' '+STR(lnQty[6],3)
        ROW=ROW+1
      ENDIF 
    ENDIF

    lnTotCtn    =lnTotCtn  + lnCartons
    lnTWeight   =lnLWeight + lnTWeight
    IF Scale<>xPrevScale .AND. llNrmPrt  
      xPrevScale=Scale 
     =SEEK ('S'+Style.Scale,'Scale')
     xScale=PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,5),5,' ')+' '+;
            PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,5),5,' ')+' '+;
            PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,5),5,' ')+' '+;
            PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,5),5,' ')   
      xCount    =Scale->Cnt
      IF xCount >1
        @ ROW,18 SAY xScale      
        ROW = ROW+1
      ENDIF
    ENDIF

    IF xMulti = 'Y'
      SELECT &OrdLTemp
    ELSE
      SELECT OrdLine
    ENDIF

    IF llNrmPrt       
      @ ROW,00 SAY LineNo
      @ ROW,07 SAY Style
      @ ROW,30 SAY xStyDesc
      @ ROW,51 SAY Price    PICTURE '9999.99'
      @ ROW,60 SAY TotQty   PICTURE '999999'
      xAmount= TotQty * Price
      @ ROW,69 SAY xAmount  PICTURE '999,999.99'
      ROW =ROW+1

      X = 1
      DO WHILE xCount >1 .AND. X<=8
        Z =STR(X,1)
        @ ROW,(X*6)+12 SAY Qty&Z   PICTURE '@Z 99999'
        X = X+1
      ENDDO
      ROW = ROW+1
      IF lnQty_Ctn <> 0
        lnCartons = CEILING(TotQty/lnQty_Ctn)
      ELSE
        lnCartons = 0
      ENDIF
      lnLWeight = lnWeight*TotQty
      = lfPrtSku(Style)
      *-- B804131,1 AAN Change "DZ" qualifier to "UNITS" [Begin]
      *@ ROW,1 SAY STR(lnCartons) + ' CTN  X ' + ALLTRIM(STR(lnQty_Ctn)) + ' DZ   ' + lcStrToPrn
       @ ROW,1 SAY STR(lnCartons) + ' CTN  X ' + ALLTRIM(STR(lnQty_Ctn)) + ' UNITS' + lcStrToPrn
      *-- B804131,1 AAN Change "DZ" qualifier to "UNITS" [End]      
      IF Row>=MaxRow
       =lfPrnCont()
       =lfPrnHdr()
      ELSE
        ROW = ROW + 1
      ENDIF
      xTotQty  = xTotQty+ TotQty
      xTotOrd  = xTotOrd+ xAmount
    ENDIF
    IF llRpOrdLnt .AND. MEMLINES(Note_Mem) <> 0 
      =lfPrtONte()
    ENDIF
    ROW = ROW+1
    SELECT (OrdPTemp) 
    SKIP
    IF xMulti = 'Y'
      SELECT &OrdLTemp
    ELSE
      SELECT OrdLine
    ENDIF
    SKIP
  ENDDO
  IF EOF() .OR. &OrdPTemp->Order <> xOrder OR &OrdPTemp->Order <> XSTORE
    lnTotCtn  = lnTotCtn+lnCartons
    lnTWeight = lnTWeight+lnLWeight
    @ ROW,1 SAY 'TOTAL WEIGHT       : '+PADL(STR(lnTWeight, 9, 2), 9) + ' LBS.'
    IF Row>=MaxRow
      =lfPrnCont()
      =lfPrnHdr()
    ELSE
      ROW = ROW + 1
    ENDIF
    @ ROW,1 SAY 'TOTAL # OF CARTONS : '+PADL(lnTotCtn, 6)
    IF Row>=MaxRow
      =lfPrnCont()
      =lfPrnHdr()
    ELSE
      ROW = ROW + 1
    ENDIF
    IF !EMPTY(Customer.Note)
      @ ROW,1 SAY Customer.Note
    ENDIF
    lnTotCtn  = 0
    lnTWeight = 0
    lnCartons = 0
    lnLWeight = 0

  ENDIF
    
  *------------------------ END PRINT LINE LOOP ----------------------

  IF (NewDoc .AND. !NoteLoop .AND. llRpOrdNot ) .OR. ;
     (xMulti='Y' .AND. (!NoteLoop) .AND. (!NewDoc) .AND. ;
    XStore <> &OrdLTemp->Store )
    SELECT NotePad
    SEEK 'B'+xOrder
    NoteLoop = IIF(FOUND(), .T., .F.)
    lnMline = 1
  ENDIF

  *---------------------------------------------------
  * [NOTEPAD] COMMENT LINES
  *---------------------------------------------------

  IF llRpOrdNot
    lnMemWidth = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75
    lnMTotLin = MEMLINES(NotePad.MNOTES)
  ENDIF
  IF Row>=MaxRow AND  lnMTotLin<>0
    =lfPrnCont()
    =lfPrnHdr()
  ELSE
    ROW = ROW + 1
    IF Row>=MaxRow AND lnMTotLin<>0
      =lfPrnCont()
      =lfPrnHdr()
    ELSE
      ROW = ROW + 1
    ENDIF
  ENDIF
  DO WHILE NoteLoop .AND. llRpOrdNot
    SELECT NotePad
    IF lnMline > lnMTotLin
      IF xMulti='Y' .AND. (XOrder=&OrdLTemp->Order)
        NewDoc   = .F.
      ELSE
        NewDoc   = .T.
      ENDIF
      NoteLoop = .F.
      EXIT
    ENDIF
    IF ROW >= MaxRow
      NewDoc = .T.
      NoteLoop = IIF(lnMline = lnMTotLin,.F.,.T.)   
      IF NoteLoop=.T.
        =lfPrnCont()
        =lfPrnHdr() 
      ENDIF     
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

  IF ((xMulti='Y' .AND. XStore= &OrdLTemp->Store .AND. !NewDoc ).OR. NoteLoop ) .OR. ;
      (xMulti <> 'Y' .AND. !NewDoc)
    @ 53,00 SAY REPLICATE('-',80)
    @ 54,13 SAY 'C O N T I N U E D ...'
    @ 54,69 SAY '**********'
    @ 55,00 SAY REPLICATE('-',80)
    LOOP
  ENDIF

  xMer_Disc = OrdHdr->Disc
  xMer_Amnt = (xMer_Disc * xTotOrd)/100
  xNet_Amnt = xTotOrd - xMer_Amnt
  @ 52,00 SAY A
  @ 53,10 SAY 'GROSS AMOUNT      =>'
  @ 53,69 SAY xTotOrd PICTURE '999,999.99'
  @ 54,10 SAY 'MERCHANDISE DISC. =>'
  @ 54,40 SAY xMer_Disc PICTURE '999.99' + " %"
  @ 54,69 SAY xMer_Amnt PICTURE '999,999.99'
  @ 55,10 SAY 'NET AMOUNT        =>'
  @ 55,60 SAY xTotQty PICTURE '999999'
  @ 55,69 SAY xNet_Amnt PICTURE '999,999.99'
  @ 56,00 SAY A
  @ 57,01 SAY lcNote1
  @ 58,01 SAY lcNote2
  @ 59,01 SAY lcNote3
  @ 60,00 SAY A
  IF !NewDoc
    LOOP
  ENDIF

  * GET NEXT ORDER
  SELECT &OrdHTemp
  SKIP
  IF EOF()
    EXIT
  ENDIF

ENDDO
SET DEVICE TO SCREEN
RETURN
******    END ORD510Z.PRG

*!*************************************************************
*! Name      : lfPrtONte
*! Developer : Mohamed Shokry(MHM)
*! Date      : 02/26/2001
*! Purpose   : Print Order line notes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Non
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPrtONte()
*!*************************************************************
FUNCTION lfPrtONte
Row=Row-1
lnNoteLn = lnSvNoteLn 
DO WHILE lnNoteLn <= MEMLINES(Note_Mem)
  IF Row >= MaxRow - 1
    NewDoc   = .F.
    lnSvNoteLn = lnNoteLn
    =lfPrnCont()
    =lfPrnHdr()

    *B129386,1 NNA 11/10/2005 (Begin) print the Line Notes At the first of the next page if we are in the last row of the page
    Row = Row + 1
    @ Row,01 SAY MLINE(Note_Mem,lnNoteLn)
    *B129386,1 NNA (END)

  ELSE
    Row = Row + 1
    @ Row,01 SAY MLINE(Note_Mem,lnNoteLn)
  ENDIF
  lnNoteLn = lnNoteLn + 1
ENDDO
IF lnNoteLn > MEMLINES(Note_Mem)
  Row = Row + 1
ENDIF
lnSvNoteLn = 1
RETURN(.T.)
*End...

*!*************************************************************
*! Name      : lfPrnHdr
*! Developer : Mohamed Shokry(MHM)
*! Date      : 02/26/2001
*! Purpose   : Print Order Header.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Non
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPrnHdr()
*!*************************************************************
FUNCTION lfPrnHdr

lnAlias = SELECT(0)
@ 01,01 SAY laCompAdd1
@ 01,61 SAY 'DUNS #'
@ 01,68 SAY ALLTRIM(lcDuns)
@ 02,01 SAY laCompAdd2
@ 03,01 SAY laCompAdd3
@ 04,01 SAY laCompAdd4
@ 05,01 SAY laCompAdd5
@ 06,01 SAY laCompAdd6
@ 07,32 SAY 'P A C K I N G   L I S T'

SELECT OrdHdr
@ 09,03 SAY 'ORDER: '+Order
@ 09,60 SAY 'DATE: '
@ 09,66 SAY Entered
@ 10,60 SAY 'PO #: '
@ 10,66 SAY IIF(MultiPO,lcCustPo,CustPo)
@ 12,04 SAY '........ SOLD TO ........'
@ 12,46 SAY '........ SHIP TO ........'
@ 13,04 SAY Account
@ 13,46 SAY 'STORE: ' + xStore + '  DEPT: ' + Dept

@ 14,04 SAY XBTNAME
@ 14,46 SAY XSTNAME

@ 15,04 SAY XBTADDR1
@ 15,46 SAY XSTADDR1

@ 16,04 SAY XBTADDR2
@ 16,46 SAY XSTADDR2

@ 17,04 SAY XBTADDR3
@ 17,46 SAY XSTADDR3

@ 19,00 SAY A
@ 20,00 SAY B
@ 21,00 SAY A

@ 22,01 SAY Start
@ 22,12 SAY Complete
@ 22,22 SAY LEFT(xTerm_Data,15)
@ 22,38 SAY LEFT(xShip_Data,15)
@ 22,54 SAY LEFT(xSeas_Data,10)
@ 22,65 SAY Rep1
@ 22,70 SAY Rep2

@ 23,00 SAY A
@ 24,00 SAY C
@ 25,00 SAY A
ROW = 26
IF !EMPTY(Note1) .AND. SUBSTR(Note1,1,1) <> '*'      
  ROW = 26
  @ ROW,00 SAY  Note1
ENDIF       
 
IF !EMPTY(Note2) .AND. SUBSTR(Note2,1,1) <> '*'
  ROW = 26
  @ ROW,32 SAY Note2
ENDIF       

ROW = IIF(Row = 26,27,26)
SELECT (lnAlias)
*!*************************************************************
*! Name      : lfPrnCont
*! Developer : Mohamed Shokry(MHM)
*! Date      : 02/26/2001
*! Purpose   : Print Page Footer when extended to the next page.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Non
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPrnCont()
*!*************************************************************
FUNCTION lfPrnCont

@ 53,00 SAY REPLICATE('-',80)
@ 54,13 SAY 'C O N T I N U E D ...'
@ 54,69 SAY '**********'
@ 55,00 SAY REPLICATE('-',80)

*!*************************************************************
*! Name      : lfPrtSku
*! Developer : Mohamed Shokry(MHM)
*! Date      : 02/26/2001
*! Purpose   : Get the SKU number of the Account/Style/Color.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Non
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPrtSku()
*!*************************************************************
FUNCTION lfPrtSku
PARA lcStyKey
lnAlias=SELECT()
SELECT Spck_Lin
lcOrd=SET('ORDER')
SET ORDER TO TAG SPCKLINS

IF !SEEK('S'+xAccount+lcStyKey,'Spck_Lin')
  lcStrToPrn = " "
  SET ORDER TO &lcOrd
  SELECT (lnAlias)
  RETURN 
ENDIF
= SEEK('M'+xAccount,'Customer')
lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
  lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
  lnDime2 = SkuTmpl.Len4
ELSE
  lnDime1 = 8  &&Default
  lnDime2 = 8  &&Default
ENDIF 
lcStrToPrn = 'SKU#: ' + SUBSTR(Spck_Lin.Pack_Id,1,lnDime1)
lcStrToPrn = IIF(LEN(lcStrToPrn)>65,SUBSTR(lcStrToPrn,1,65),ALLTRIM(lcStrToPrn))
SET ORDER TO &lcOrd
SELECT (lnAlias)