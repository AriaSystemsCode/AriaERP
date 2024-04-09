*:************************************************************************
*: PROGRAM   : SOORCNMR.PRG       (C101973)
*: DESC.     : Order Confirmation Form (MOR10)(Conversion of ORD510Z.PRG FROM 2.6)
*: System    : Aria Apparel System    (A27)
*: Module    : SALES ORDER            (SO )
*: Developer : MOHAMED SHOKRY        -(MHM)
*: DATE      : 09/24/2000
*: REFER TO  : (C101973)
*:************************************************************************
*: Calls : 
*:         FUNCTION  : lfPrtONte , lfGetClrD  , lfPrnConti , lfPrnLast
*:                   : lfPrnHdr  , lfPrnNotPd , lfGetOdHdr
*:         --------------------------------------------------------------
*:         PROCEDURE : 
*:************************************************************************
*: Example : DO SOOECNMR
*:************************************************************************
*: This Program is due to C101973 ...
*:************************************************************************
*:Modifications:
*:************************************************************************
*
*-- Set the memory variables.
*-- llPack      --> Print the Pack_Id (.T.-->.F.).
*-- llRpOrdLNt  --> IF The Add notes to order lines seting is set to No
STORE ' ' TO lcOrder,lcAccount,lcMulti,lcStore,laAddress,lcTermData,lcShipVia,lcDivison,;
            lcBtName ,lcPrePack,lcScale, lcPrevScale 
STORE 0 TO lnNum ,lnScount

*-- If No records selected.
SELECT ORDHDR
SET SKIP TO
LOCATE FOR &lcRpExp
IF !FOUND()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  *-- Variable to do EndReport Or Don't.
  llNoRec = .T.
  RETURN
ELSE
  *-- Get first order to seek for
  lcOrder = OrdHdr.Order
  *-- Variable to do EndReport Or Don't.
  llNoRec = .F.
ENDIF

DECLARE laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]
laSoldTo = ''                     && Array to hold the Sold To address
laShipTo = ''                     && Array to hold the Ship To address
lcDivLName = ''
laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'


SELECT ORDHDR
SET RELATION TO cordtype+ order INTO Ordline ADDITIVE

SELECT (lcTempOrd)
*-- Get the style and color
lcMajTitl  = ALLTRIM(gfItemMask('HI'))
*--  Open needed files That didn't open before.
=gfOpenFile(gcDataDir+'Spck_Lin',gcDataDir+'Spck_Lin','SH')
=gfOpenFile(gcDataDir+'Spck_Hdr',gcDataDir+'Spck_Hdr','SH')
*--Initilize variables
*-- We Have Temp File Called lcTempOrd Fill Of All Data Came from standaerd form.
SELECT (lcTempOrd)
IF llPack
  IF lcRpSortBy = 'S'
    INDEX ON CORDTYPE + ORDER + STORE + STYLE + STR(LINENO,6) + PACK_ID TAG (lcTempOrd)
    SET ORDER TO TAG (lcTempOrd)
  ELSE
    INDEX ON CORDTYPE + ORDER + STORE + STR(LINENO,6) + STYLE + PACK_ID TAG (lcTempOrd)
    SET ORDER TO TAG (lcTempOrd)
  ENDIF
ENDIF

A= REPLICATE('*',80)
*   ....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
B='|  START  | COMPLETE |    TERMS      |   SHIPVIA     | SEASON   |REP1 REP2'
*   MM/DD/YY   MM/DD/YY  123456789012345 123456789012345 1234567890 123  123
C='      GRP STYLE        COLOR                         PRICE  PIECES       AMOUNT'
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*    STYLE   CLR                                     PRICE   PIECES       AMOUNT
*123 1234567 123 12345678901234567890              1234.99   123456   123,456.99

*-------------------------------
* SETUP HEADINGS
*-------------------------------
*--Fill the company address variables.
*--Center the address
laCompAdd1 = ''
laCompAdd2 = SPACE(ROUND((80-LEN(laCompAdd[2]))/2,0))+laCompAdd[2]
laCompAdd3 = SPACE(ROUND((80-LEN(laCompAdd[3]))/2,0))+laCompAdd[3]
laCompAdd4 = SPACE(ROUND((80-LEN(laCompAdd[4]))/2,0))+laCompAdd[4]
laCompAdd[5] = SyCcomp.Ccom_phon
laCompAdd5 = SPACE(ROUND((80-LEN(ALLTRIM(laCompAdd[5])))/2,0))+laCompAdd[5]
MAXROW = 51

STORE 1 TO lnNoteLn,lnSvNoteLn 
llNrmPrt = .T.

*-- Print the Form.
SELECT Ordhdr
SET DEVICE TO PRINT
*--lcRpOrdTyp come from standerd form [define at Syrepuver]
=SEEK(lcRpOrdTyp+lcOrder)
SCAN REST WHILE INKEY()<> 32 FOR &lcRpExp
  
  WAIT WINDOW 'Printing Order #:...'+Order+'..<Space Bar> TO ABORT' NOWAIT
  *--Get the Order header information.
  =lfGetOdHdr()
  lnTotQty  = 0.00
  lnTotORd  = 0.00
  *-- Start Printing  The header.
  =lfPrnHdr()
  ROW = 25
  IF !EMPTY(OrdHdr.Note1) .AND. SUBSTR(OrdHdr.Note1,1,1) <> '*'      
    ROW = 26
    @ ROW,00 SAY  OrdHdr.Note1
  ENDIF       
  IF !EMPTY(OrdHdr.Note2) .AND. SUBSTR(OrdHdr.Note2,1,1) <> '*'
    ROW = 26
    @ ROW,32 SAY OrdHdr.Note2
  ENDIF       
  
  * The line must be either 26 or 27
  ROW =  ROW + 1
  *--- START PRINTING THE ORDER LINES
  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
  *LN#   GRP STYLE           CLR                        PRICE   PIECES       AMOUNT
  *123456 X  123456789012345 123 12345678901234567890 1234.99   123456   123,456.99
  *            11111 22222 33333 44444 55555 66666 77777 88888
  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
  SELECT (lcTempOrd)
  lcStore = Store 
  *-- Main loop of printing 
  SCAN WHILE cOrdType+Order = ORDHDR.cOrdType+lcOrder
    DO CASE
      CASE lcMulti = 'Y' .AND. Store # lcStore
          IF  llRpOrdLNt .AND. llRpOrdNot
            IF MEMLINES(Note_Mem) <> 0
              *-- Print order notepade lines.
              =lfPrtOnte() 
            ENDIF  
            *-- Print order notepade
            ROW = ROW+1
            =lfPrnNotPd()
            *-- Print page footer
            =lfPrnLast()
            *-- get page header for the new page, then print it.
            =lfGetOdHdr()            
            =lfPrnHdr()
            ROW = ROW+1
          ENDIF
          *--to get new page in case of row greater than maxrow or we did not print order notes 
          IF ROW >= MAXROW .OR. !(llRpOrdLNt .AND. llRpOrdNot)
            =lfPrnConti()
            =lfGetOdHdr()            
            =lfPrnHdr()
            ROW = ROW+1
          ENDIF
          lcStore = Store 
      CASE ROW >= MAXROW
        =lfPrnConti()
        =lfGetOdHdr()            
        =lfPrnHdr()
        ROW = ROW+1
    ENDCASE
    
    =SEEK(IIF(lcMulti = 'Y' , &lcTempOrd..Style,Ordline.style),'STYLE' )
    lcStyDesc  = STYLE.Desc
    IF llPack  .AND. &lcTempOrd..Pack_Id <> lcPrePack .AND. !EMPTY(&lcTempOrd..Pack_Id)
      IF !SEEK('P'+lcAccount+&lcTempOrd..Pack_Id,'Spck_Lin')
        =SEEK('P'+'*****'+&lcTempOrd..Pack_Id,'Spck_Lin')
      ENDIF
      IF !SEEK('P'+lcAccount+&lcTempOrd..Pack_Id,'Spck_Hdr')
        =SEEK('P'+'*****'+&lcTempOrd..Pack_Id,'Spck_Hdr')
      ENDIF
      *-- To print the pack_id Total Quantity in case of print pack yes
      lcOldAlias = ALIAS()
      SELECT SPCK_LIN
      SUM REST WHILE &lcTempOrd..Pack_Id = SPCK_LIN.PACK_ID SPCK_LIN.QTY1,;
               SPCK_LIN.QTY2,SPCK_LIN.QTY3,SPCK_LIN.QTY4,;
               SPCK_LIN.QTY5,SPCK_LIN.QTY6,SPCK_LIN.QTY7,SPCK_LIN.QTY8 TO ARRAY lnQty
      SELECT (lcOldAlias)

      lcPrePack = &lcTempOrd..Pack_Id
      IF !EMPTY(&lcTempOrd..Pack_Id)
        @ ROW,00 SAY 'PACK_ID :'+&lcTempOrd..Pack_Id+' '+'DESC.:'+LEFT(Spck_Hdr.Desc,15)+' ' +;
                     +STR(lnQty[1],3)+' '+ STR(lnQty[2],3)+' '+STR(lnQty[3],3)+' ' +;
                     STR(lnQty[4],3)+' '+STR(lnQty[5],3)+' '+STR(lnQty[6],3)+' '+STR(lnQty[7],3)+' '+STR(lnQty[8],3)
        ROW=ROW+1
      ENDIF 
    ENDIF
    IF Scale <> lcPrevScale .AND. llNrmPrt  .AND. llsizes 
      lcPrevScale = Scale 
      lcScale     = GETSCALE(Style.Scale,SPACE(1))   
      lnScount    = Scale.Cnt
      IF lnScount > 1
        @ ROW,18 SAY lcScale
        ROW = ROW+1
      ENDIF
    ENDIF
    IF llNrmPrt
      @ ROW,00 SAY LineNo
      @ ROW,07 SAY Group
      @ ROW,10 SAY Style
      @ ROW,30 SAY lcStyDesc
      @ ROW,51 SAY Price    PICTURE '9999.99'
      @ ROW,60 SAY TotQty   PICTURE '999999'
      lnAmount= TotQty * Price
      @ ROW,69 SAY lnAmount  PICTURE '999,999.99'
      IF llsizes
        ROW =ROW+1
        @ ROW, 2 SAY lfGetClrD()
        X = 1
        DO WHILE lnScount >1 .AND. X <= 8
          Z =STR(X,1)
          @ ROW,(X*6)+12 SAY QTY&Z   PICTURE '@Z 99999'
          X = X+1
        ENDDO
      ENDIF  
      ROW = ROW+1       
      * CUM ORDER TOTALS
      lnTotQty  = lnTotQty+ TotQty
      lnTotORd  = lnTotORd+ lnAmount
    ENDIF
    *-- Print the order line notes.
    IF  llRpOrdLNt
      IF MEMLINES(ALLTRIM(Note_Mem)) <> 0
        *-- Print order notepade lines.
        =lfPrtOnte() 
      ENDIF  
    ENDIF  
    ROW = ROW+1    
  ENDSCAN
  *------------------------ END PRINT LINE LOOP ----------------------*
  IF llRpOrdNot
    *-- Print order notepade
    =lfPrnNotPd()
    ROW = ROW+1
  ENDIF  
  =lfPrnLast()
ENDSCAN

SELECT ORDHDR
SET RELATION OFF INTO ORDLINE


SET DEVICE TO SCREEN
RETURN
*-- End OF Print.
*!*************************************************************
*! Name      : lfPrtONte
*: Developer : MOHAMED SHOKRY - (MHM)
*: DATE      : 09/24/2000
*! Purpose   : Function to print Order line notes.
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            : =lfPrtONte()
*!*************************************************************
*
FUNCTION lfPrtONte
ROW = ROW-1
lnNoteLn = lnSvNoteLn 
DO WHILE lnNoteLn <= MEMLINES(Note_Mem)
  IF ROW >= MaxRow - 1
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

*-- End OF lfPrtONte.
*!*************************************************************
*! Name      : lfPrnConti
*: Developer : MOHAMED SHOKRY - (MHM)
*: DATE      : 09/24/2000
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
*
FUNCTION lfPrnConti

@ 53,00 SAY REPLICATE('-',80)
@ 54,13 SAY 'C O N T I N U E D ...'
@ 54,69    SAY '**********'
@ 55,00 SAY REPLICATE('-',80)

*--End Of lfPrnConti.
*!*************************************************************
*! Name      : lfPrnLast
*: Developer : MOHAMED SHOKRY - (MHM)
*: DATE      : 09/24/2000
*! Purpose   : Function to print the total
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  lfPrnLast()
*!*************************************************************
*
FUNCTION lfPrnLast

lnMer_Disc = ORDHDR.DISC
lnMer_Amnt = (lnMer_Disc * lnTotORd)/100
lnNet_Amnt = lnTotORd - lnMer_Amnt
@ 52,00 SAY A
@ 53,10 SAY 'GROSS AMOUNT      =>'
@ 53,69 SAY lnTotORd PICTURE '999,999.99'
@ 54,10 SAY 'MERCHANDISE DISC. =>'
@ 54,40 SAY lnMer_Disc PICTURE '999.99' + " %"
@ 54,69 SAY lnMer_Amnt PICTURE '999,999.99'
@ 55,10 SAY 'NET AMOUNT        =>'
@ 55,63 SAY lnTotQty PICTURE '999999'
@ 55,69 SAY lnNet_Amnt PICTURE '999,999.99'
@ 56,00 SAY A
@ 57,22 SAY '______________________________________   ____________'
@ 58,22 SAY 'CUSTOMER APPROVAL SIGNATURE                  DATE    '
lnTotQty  = 0.00
lnTotORd  = 0.00


*-- End Of lfPrnLast.
*!*************************************************************
*! Name      : lfPrnHdr
*: Developer : MOHAMED SHOKRY - (MHM)
*: DATE      : 09/24/2000
*! Purpose   : Function to print the header
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  lfPrnHdr()
*!*************************************************************
*
FUNCTION lfPrnHdr

*-- Print the company address.
@ 01,01 SAY SPACE(ROUND((80-LEN(gccom_name))/2,0))+gccom_name
@ 02,01 SAY laCompAdd1
@ 03,01 SAY laCompAdd2
@ 04,01 SAY laCompAdd3
@ 05,01 SAY laCompAdd5
@ 07,24 SAY 'O R D E R   C O N F I R M A T I O N'
@ 09,03 SAY 'ORDER: '+OrdHdr.Order
@ 09,60 SAY 'DATE: '
@ 09,66 SAY OrdHdr.Entered
@ 10,60 SAY 'PO #: '
@ 10,66 SAY IIF(OrdHdr.MultiPO,&lcTempOrd..CUSTPO,OrdHdr.CUSTPO)

@ 12,04 SAY '........ SOLD TO ........'
@ 12,46 SAY '........ SHIP TO ........'
@ 13,04 SAY OrdHdr.Account
@ 13,46 SAY 'STORE: ' + &lcTempOrd..Store + '  DEPT: ' + OrdHdr.Dept

@ 14,04 SAY lcBtName
@ 14,46 SAY lcShptName
@ 15,04 SAY laSoldTo[1]
@ 15,46 SAY laShipTo[1]
@ 16,04 SAY laSoldTo[2]
@ 16,46 SAY laShipTo[2]
@ 17,04 SAY laSoldTo[3]
@ 17,46 SAY laShipTo[3]
@ 19,00 SAY A
@ 20,00 SAY B
@ 21,00 SAY A

@ 22,01 SAY OrdHdr.Start
@ 22,12 SAY OrdHdr.Complete
@ 22,22 SAY lcTermData
@ 22,38 SAY lcShipVia
@ 22,54 SAY lcSeason
@ 22,65 SAY OrdHdr.REP1
@ 22,70 SAY OrdHdr.REP2
@ 23,00 SAY A
@ 24,00 SAY C
@ 25,00 SAY A
ROW = 25
*-- End Of lfPrnHdr.
*!*************************************************************
*! Name      : lfPrnNotPd
*: Developer : MOHAMED SHOKRY - (MHM)
*: DATE      : 09/24/2000
*! Purpose   : Function to print the notepad
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  lfPrnNotPd()
*!*************************************************************
*
FUNCTION lfPrnNotPd
PRIVATE  lnAlias,lnOldMemW, lnMemLins, lnNotLine

lnAlias = SELECT()
IF llRpOrdNot && if Print NotePade. 
  lnNotLine = 1
  SELECT NotePad
  lnMemWidth = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  IF SEEK ('B'+lcOrder)
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)
    DO WHILE lnNotLine <= lnMemLins
      IF Row > MAXROW
        =lfPrnConti()
        *-to get the current rcord 
        SELECT (lcTempOrd)
        SKIP -1
        =lfGetOdHdr()            
        =lfPrnHdr()
        SKIP
        SELECT NotePad
        ROW = ROW+1
      ENDIF          
      IF  LEFT(MLINE(MNOTES,lnNotLine),1) <> '*'
        @ ROW,02 SAY MLINE(MNOTES,lnNotLine)
        ROW = ROW + 1
      ENDIF  
      lnNotLine = lnNotLine + 1
    ENDDO     
  ENDIF
  SET MEMOWIDTH TO lnMemWidth
ENDIF  
SELECT(lnAlias)

*-- End OF lfPrnNotPd.
*!***************************************************************************
*! Name      : lfGetOdHdr
*: Developer : MOHAMED SHOKRY - (MHM)
*: DATE      : 09/24/2000
*! Purpose   : Function to get the information of the ORDER header.
*!***************************************************************************
*! Called from : None.
*!***************************************************************************
*! Calls       : None.
*!***************************************************************************
*! Passed Parameters : None.
*!***************************************************************************
*! Return            : None
*!***************************************************************************
*! Example           : = lfGetOdHdr()
*!***************************************************************************
*
FUNCTION lfGetOdHdr
PRIVATE lcAlias

lcAlias = ALIAS()

lcOrder = &lcTempOrd..Order
lcMulti   = OrdHdr.Multi
lcAccount = Ordhdr.Account
SELECT (lcTempOrd)
lcStore = IIF(lcMulti <> 'Y',ORDHDR.STORE,lcStore)  
STORE '' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6

*-- GET DESCRIPTIONS FOR CODED FIELDS
SELECT CODES
*--Terms
lcTermData =SUBSTR(gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE'),1,15)
*--ShipVia
lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,15)
*--Division long name.
STORE '' TO lcDivLName
=gfRltFld(OrdHdr.cDivision,@laDivLName,'CDIVISION')
*--Special instruction
lcSpcInst = SUBSTR(gfCodDes(OrdHdr.SPCINST , 'SPCINST'),1,15)
*---Season
lcSeason = SUBSTR(gfCodDes(OrdHdr.SEASON , 'SEASON'),1,10)
*--Get the first line of the company address.


IF lcMulti = "Y" 
  SELECT CUSTOMER
  =SEEK('S'+ &lcTempOrd.. Account+ &lcTempOrd..Store)
  lcBtName  = CUSTOMER.BTNAME
  laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
  laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
  laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
  laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
  laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
  = lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

  *-- SHIP_TO ADDRESS FOR THIS STORE
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
  IF OrdHdr.Alt_ShpTo
    lcShptName  = OrdHdr.STNAME
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2
    laShipTo[3] = OrdHdr.cAddress3+','+OrdHdr.cAddress4+','+OrdHdr.cAddress5
    IF Len(ALLTRIM(lcStAddr2)) = 0
      lcStAddr2 = lcStAddr3 
    ENDIF
  ELSE
    SELECT CUSTOMER
    =SEEK('S'+ &lcTempOrd.. Account+ &lcTempOrd..Store)
    lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
    = gfGetAdr('CUSTOMER','','','',@laShipTo,'')
    = lfAdrShift('laShipTo')  && Shift Sold To address if there is empty line.
  ENDIF 
ELSE
  lcBtName  = CUSTOMER.BTNAME
  SELECT CUSTOMER
  = gfGetAdr('CUSTOMER','','','',@laSoldTo,'')
  = lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

  *-- SHIP_TO ADDRESS FOR THIS STORE
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
  IF OrdHdr.Alt_ShpTo
    lcShptName  = OrdHdr.STNAME
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2
    laShipTo[3] = OrdHdr.cAddress3+','+OrdHdr.cAddress4+','+OrdHdr.cAddress5
    IF Len(ALLTRIM(lcStAddr2)) = 0
      lcStAddr2 = lcStAddr3 
    ENDIF
  ELSE
    SELECT CUSTOMER
    lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
    = gfGetAdr('CUSTOMER','','','',@laShipTo,'')
    = lfAdrShift('laShipTo')  && Shift Sold To address if there is empty line.
  ENDIF 

ENDIF

laCompAdd1 = IIF(!EMPTY(lcDivLName),lcDivLName,laCompAdd[1])
laCompAdd1 = SPACE(ROUND((80-LEN( ALLTRIM(laCompAdd1) ))/2,0))+laCompAdd1

SELECT (lcTempOrd)
IF lcOrder <> order
  lnTotQty  = 0.00
  lnTotORd  = 0.00
ENDIF

SELECT (lcAlias)

*-- End OF lfGetOdHdr.
*!***************************************************************************
*! Name      : lfGetClrD
*: Developer : MOHAMED SHOKRY - (MHM)
*: DATE      : 09/24/2000
*! Purpose   : Function to Print Color Descreption
*!***************************************************************************
*! Calls     : None.
*!***************************************************************************
*! Passed Parameters  : None.
*!***************************************************************************
*! Returns            : None.
*!***************************************************************************
*! Example            :  lfGetClrD()
*!***************************************************************************
*
FUNCTION lfGetClrD
PRIVATE lnAlias ,lnClrLen ,lcStyNMj ,lcColorNm,lnClrPos
STORE 0 TO lnClrLen ,lnClrPos
STORE '' TO lcStyNMj
*--get color description
lnAlias = SELECT()
SELECT (lcTempOrd)

DECLARE laItemSeg[1]
STORE 0 TO lnClrLen,lnClrPos
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
lcStyNMj = SUBSTR(STYLE,lnClrPos,lnClrLen)
*--Get Color Discreption
SELECT CODES
=SEEK('N'+lcStyNMj+'N'+'COLOR     ')
lcColorNm = LEFT(cdiscrep,15)
SELECT(lnAlias)
RETURN (lcColorNm)
*-- End Of lfGetClrD.
