*:***************************************************************************
*: Program file  : ICCTSLK4.PRG
*: Program desc. : CUSTOMIZED CUT & SOLD FOR CUSTOMER KAZU.
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102580
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICCTSLK4.PRG
*:***************************************************************************
*: Modifications :
*****************************************************************************

*--Make the necessary relation.
=lfRelation()

*--Printing portion.
R_TITLE   = 'CUSTOMIZED CUT & SOLD'
R_WIDTH   = 'W'
PAGENO    = 0
lnRow     = 99
lnMaxRow  = 54
llPrnGrnd = .F.

*-- Start print
=lpPrint()

*-- End printing process.
IF llPrnGrnd
  DO ENDREPORT     && End of Report or Display on Screen
ELSE
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
ENDIF
SET DEVICE TO SCREEN
RETURN
                       *-- End of the Program --*
*!*****************************************************************
*! Name      : lfRelation
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/17/2002
*! Purpose   : Function to creat the relations between the files.
*!*****************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK4.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfRelation()
*!*************************************************************
FUNCTION lfRelation

SELECT OrdLine
SET RELATION TO "O" + ORDER INTO OrdHdr

SELECT STYLE
SET RELATION TO 'S' + SCALE INTO SCALE ADDITTIVE

*--End of lfRelation.
*!*************************************************************
*! Name      : lpPrint
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/17/2002
*! Purpose   : Print the Report.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK4.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns  : None
*!*************************************************************
*! Example  : =lpPrint()
*!*************************************************************

*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Report LayOut :
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*PROGID                                                           COMPANY NAME                                              DATE      
*TIME                                                             PROGRAM NAME                                              PAGE#     
*                                                                                                                                     
*-------------------------------------------------------------------------------------------------------------------------------------
*STYLE #      S FABRIC  STYLE DESCR.              COST    BREAKDOWN                                           TOT. UNITS TOT. $AMOUNT
*XXXXXXXXXXXX X XXXXXXX XXXXXXXXXXXXXXXXXXXX 999999.99 999999 999999 999999 999999 999999 999999 999999 999999 
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*                                       TOTAL BOOKED : 999999 999999 999999 999999 999999 999999 999999 999999  99999999  99999999.99
*XXXXXX XXXXXXXXXXXXXXX                          WIP : 999999 999999 999999 999999 999999 999999 999999 999999  99999999  99999999.99
*                              RECEIVED IN WAREHOUSE : 999999 999999 999999 999999 999999 999999 999999 999999  99999999  99999999.99
*                                       UNITS PICKED : 999999 999999 999999 999999 999999 999999 999999 999999  99999999  99999999.99
*                                            SHIPPED : 999999 999999 999999 999999 999999 999999 999999 999999  99999999  99999999.99
*                                 UNALLOCATED ORDERS : 999999 999999 999999 999999 999999 999999 999999 999999  99999999  99999999.99
*                                  UNALLOCATED STOCK : 999999 999999 999999 999999 999999 999999 999999 999999  99999999  99999999.99
*                AVAILABLE TO SELL AFTER OPEN ORDERS : 999999 999999 999999 999999 999999 999999 999999 999999  99999999  99999999.99
*         AVAILABLE TO SELL AFTER OPEN & HOLD ORDERS : 999999 999999 999999 999999 999999 999999 999999 999999  99999999  99999999.99
*-------------------------------------------------------------------------------------------------------------------------------------
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�

FUNCTION lpPrint

SET DEVICE TO PRINT

DIMENSION laGtot[9,10]
laGtot = 0
STORE 0 TO lnOOrd1,lnOOrd2,lnOOrd3,lnOOrd4,lnOOrd5,lnOOrd6,lnOOrd7,lnOOrd8,lnTotOOrd
STORE 0 TO lnRcvWH1,lnRcvWH2,lnRcvWH3,lnRcvWH4,lnRcvWH5,lnRcvWH6,lnRcvWH7,lnRcvWH8,lnTRcvWH
STORE 0 TO lntATSo,lntATSh

SELECT Style
SCAN FOR &lcRpexp
  lcKey = Style
  WAIT WINDOW 'Selecting data for style...' +  lcKey NOWAIT
  llPrnGrnd = .T.

  *--Print the header.
  =lfPrntHdr()

  *--Calculate the needed fields.
  =lfCalculat()

  *--Report detail.
  *-Line 0........................................................

  @ lnRow,000 SAY LEFT(Style,12)
  @ lnRow,013 SAY Status
  @ lnRow,015 SAY Fabric
  @ lnRow,023 SAY Desc
  lnCost = PriceA

  @ lnRow,044 SAY lnCost PICTURE '999999.99'
  lnCol = lnMaxRow + 1
  FOR I=1 TO SCALE.Cnt
    Z = STR(I,1)
    @ lnRow,lnCol SAY SPACE(5-LEN(ALLTRIM(SCALE.SZ&Z)))+ALLTRIM(SCALE.SZ&Z)
    lnCol = lnCol + 7
  ENDFOR
  lnRow = lnRow + 1

  *-Line 1........................................................
  @ lnRow,039 SAY 'TOTAL BOOKED :'
  lnCol = lnMaxRow
  FOR I=1 TO SCALE.Cnt
    Z = STR(I,1)
    @ lnRow,lnCol SAY (Ord&Z+Shp&Z)  PICTURE '999999'
    laGtot(1,I) = laGtot(1,I) + (Ord&Z+Shp&Z)
    lnCol = lnCol + 7
  ENDFOR
  @ lnRow,111 SAY (TotOrd+TotShp) PICTURE '99999999'
  @ lnRow,121 SAY ((TotOrd+TotShp)*lnCost) PICTURE '99999999.99'
  lnRow = lnRow + 1
  laGtot(1,9)  = laGtot(1,9)  + (TotOrd+TotShp)
  laGtot(1,10) = laGtot(1,10) + ((TotOrd+TotShp)*lnCost)

  *-Line 2........................................................
  @ lnRow,000 SAY SUBSTR(LCKEY,14,6)
  @ lnRow,007 SAY LEFT(gfCodDes(SUBSTR(LCKEY,14,6),'COLOR'),15)
  @ lnRow,048 SAY 'WIP :'
  lnCol = lnMaxRow
  FOR I = 1 TO SCALE.Cnt
    Z = STR(I,1)
    @ lnRow,lnCol SAY WIP&Z PICTURE '999999'
    laGtot(2,I) = laGtot(2,I) + WIP&Z
    lnCol = lnCol + 7
  ENDFOR
  @ lnRow,111 SAY TotWIP PICTURE '99999999'
  @ lnRow,121 SAY (TotWIP*lnCost) PICTURE '99999999.99'
  laGtot(2,9)  = laGtot(2,9)  + TotWIP
  laGtot(2,10) = laGtot(2,10) + (TotWIP*lnCost)
  lnRow = lnRow + 1

  *-Line 3.......................................................
  @ lnRow,030 SAY 'RECEIVED IN WAREHOUSE :'
  lnCol = lnMaxRow
  FOR I = 1 TO SCALE.Cnt
    Z = STR(I,1)
    @ lnRow,lnCol SAY lnRcvWH&Z PICTURE '999999'
    laGtot(3,I) = laGtot(3,I) + lnRcvWH&Z
    lnCol = lnCol + 7
  ENDFOR
  @ lnRow,111 SAY lnTRcvWH PICTURE '99999999'
  @ lnRow,121 SAY (lnTRcvWH*lnCost) PICTURE '99999999.99'
  laGtot(3,9)  = laGtot(3,9)  + lnTRcvWH
  laGtot(3,10) = laGtot(3,10) + (lnTRcvWH*lnCost)
  lnRow = lnRow + 1

  *-Line 4........................................................
  @ lnRow,039 SAY 'UNITS PICKED :'
  lnCol = lnMaxRow
  FOR I = 1 TO SCALE.Cnt
    Z = STR(I,1)
    @ lnRow,lnCol SAY Alo&Z PICTURE '999999'
    laGtot(4,I) = laGtot(4,I) + Alo&Z
    lnCol = lnCol + 7
  ENDFOR
  @ lnRow,111 SAY TotAlo PICTURE '99999999'
  @ lnRow,121 SAY (TotAlo*lnCost) PICTURE '99999999.99'
  laGtot(4,9)  = laGtot(4,9)  + TotAlo
  laGtot(4,10) = laGtot(4,10) + (TotAlo*lnCost)
  lnRow = lnRow + 1

  *-Line 5........................................................
  @ lnRow,044 SAY 'SHIPPED :'
  lnCol = lnMaxRow
  FOR I = 1 TO SCALE.Cnt
    Z = STR(I,1)
    @ lnRow,lnCol SAY Shp&Z PICTURE '999999'
    laGtot(5,I) = laGtot(5,I) + Shp&Z
    lnCol = lnCol + 7
  ENDFOR
  @ lnRow,111 SAY TotShp PICTURE '99999999'
  @ lnRow,121 SAY (TotShp*lnCost) PICTURE '99999999.99'
  laGtot(5,9)  = laGtot(5,9)  + TotShp
  laGtot(5,10) = laGtot(5,10) + (TotShp*lnCost)
  lnRow = lnRow + 1

  *-Line 6........................................................
  @ lnRow,033 SAY 'UNALLOCATED ORDERS :'
  lnCol = lnMaxRow
  FOR I = 1 TO SCALE.Cnt
    Z = STR(I,1)
    @ lnRow,lnCol SAY (Ord&Z+Shp&Z)-(Alo&Z+Shp&z) PICTURE '999999'
    laGtot(6,I) = laGtot(6,I) + (Ord&Z+Shp&Z)-(Alo&Z+Shp&z)
    lnCol = lnCol + 7
  ENDFOR
   @ lnRow,111 SAY (TotOrd+TotShp)-(TotAlo+TotShp) PICTURE '99999999'
   @ lnRow,121 SAY ((TotOrd+TotShp)-(TotAlo+TotShp))*lnCost PICTURE '99999999.99'
   laGtot(6,9)  = laGtot(6,9)  + (TotOrd+TotShp)-(TotAlo+TotShp)
   laGtot(6,10) = laGtot(6,10) + ((TotOrd+TotShp)-(TotAlo+TotShp))*lnCost
  lnRow = lnRow + 1

  *-Line 7........................................................
  @ lnRow,034 SAY 'UNALLOCATED STOCK :'
  lnCol = lnMaxRow
  FOR I = 1 TO SCALE.Cnt
    Z = STR(I,1)
    @ lnRow,lnCol SAY lnRcvWH&Z - (Alo&Z+Shp&z) PICTURE '999999'
    laGtot(7,I) = laGtot(7,I) + (lnRcvWH&Z-(Alo&Z+Shp&z))
    lnCol = lnCol + 7
  ENDFOR
  @ lnRow,111 SAY lnTRcvWH - (TotAlo + TotShp) PICTURE '99999999'
  @ lnRow,121 SAY ((lnTRcvWH-(TotAlo + TotShp))*lnCost) PICTURE '99999999.99'
  laGtot(7,9)  = laGtot(7,9) +(lnTRcvWH-(TotAlo + TotShp))
  laGtot(7,10) = laGtot(7,10)+((lnTRcvWH-(TotAlo + TotShp))*lnCost)
  lnRow = lnRow + 1

  *-Line 8........................................................
  @ lnRow,031 SAY 'O.T.S. (OPEN ORDERS) :'
  STORE 0 TO lntATSo,lntATSh
  lnCol = lnMaxRow
  FOR I = 1 TO SCALE.Cnt
    Z = STR(I,1)
    lnATSo = (WIP&Z+lnRcvWH&Z)-(lnOOrd&Z+SHP&Z)
    @ lnRow,lnCol SAY lnATSo PICTURE '999999'
    lnCol = lnCol + 7
    laGtot(8,I) = laGtot(8,I)+lnATSo
    lntATSo = lntATSo + IIF(lnATSo > 0 , lnATSo , 0 )
  ENDFOR
  @ lnRow,111 SAY lntATSo PICTURE '99999999'
  @ lnRow,121 SAY (lntATSo*lnCost) PICTURE '99999999.99'
  laGtot(8,9)  = laGtot(8,9)  + lntATSo
  laGtot(8,10) = laGtot(8,10) + (lntATSo*lnCost)
  lnRow = lnRow + 1

  *-Line 9........................................................
  @ lnRow,031 SAY 'O.T.S. (OPEN + HOLD) :'
  lnCol = lnMaxRow
  FOR I = 1 TO SCALE.Cnt
    Z = STR(I,1)
    lnATSh = (WIP&Z+lnRcvWH&Z) - (ORD&Z+SHP&Z)
    @ lnRow,lnCol SAY lnATSh PICTURE '999999'
    laGtot(9,I) = laGtot(9,I) + lnATSh
    lntATSh = lntATSh + IIF(lnATSh > 0 , lnATSh , 0 )
    lnCol = lnCol + 7
  ENDFOR
  @ lnRow,111 SAY lntATSh PICTURE '99999999'
  @ lnRow,121 SAY (lntATSh*lnCost) PICTURE '99999999.99'
  laGtot(9,9)  = laGtot(9,9)  + lntATSh
  laGtot(9,10) = laGtot(9,10) + (lntATSh*lnCost)
  lnRow = lnRow + 1
  @ lnRow,000 SAY REPLICATE('-',132)
  lnRow = lnRow + 1

ENDSCAN
WAIT CLEAR

IF llPrnGrnd
  =lpGrand()
ENDIF

*--End of lpPrint.
*!*************************************************************
*! Name      : lfPrntHdr
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/17/2002
*! Purpose   : Function to print the header when change.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK4.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfPrntHdr()
*!*************************************************************
FUNCTION lfPrntHdr

IF lnRow > 53
  PAGENO = PAGENO + 1
  DO RPT_HDR WITH "ICCTSLK4",lcRPTitle,R_WIDTH
  @ 05,00 SAY 'STYLE #      S FABRIC  STYLE DESCR.              COST    BREAKDOWN                                           TOT. UNITS TOT. $AMOUNT'
  @ 06,00 SAY REPLICATE('=',132)
  lnRow = 07
ENDIF

*--End of lfPrntHdr.
*!*************************************************************
*! Name      : lfCalculat
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/17/2002
*! Purpose   : Calculate needed fields.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK4.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfCalculat()
*!*************************************************************
FUNCTION lfCalculat

STORE 0 TO lnCRvW1,lnCRvW2,lnCRvW3,lnCRvW4,lnCRvW5,lnCRvW6,lnCRvW7,lnCRvW8,lnCRvWT
STORE 0 TO lnPRvW1,lnPRvW2,lnPRvW3,lnPRvW4,lnPRvW5,lnPRvW6,lnPRvW7,lnPRvW8,lnPRvWT
STORE 0 TO lnOOrd1,lnOOrd2,lnOOrd3,lnOOrd4,lnOOrd5,lnOOrd6,lnOOrd7,lnOOrd8,lnTotOOrd

SELECT CUTTKTL
IF SEEK(lcKey)
  SUM REST QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY ;
      WHILE Style = lcKey FOR Trancd = '2'                ;
      TO lnCRvW1,lnCRvW2,lnCRvW3,lnCRvW4,                 ;
         lnCRvW5,lnCRvW6,lnCRvW7,lnCRvW8,lnCRvWT
ENDIF

SELECT POSLN
IF SEEK(lcKey)
  SUM REST QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY ;
      WHILE Style = lcKey FOR Trancd = '2'                ;
      TO lnPRvW1,lnPRvW2,lnPRvW3,lnPRvW4,                 ;
         lnPRvW5,lnPRvW6,lnPRvW7,lnPRvW8,lnPRvWT
ENDIF

FOR I = 1 TO SCALE.Cnt
  Z = STR(I,1)
  lnRcvWH&z = lnCRvW&z + lnPRvW&z
ENDFOR
lnTRcvWH = lnCRvWT + lnPRvWT

SELECT ORDLINE
IF SEEK(lcKey)
  SUM REST QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY ;
      WHILE Style = lcKey FOR ORDHDR.Status = 'O'         ;
      TO lnOOrd1,lnOOrd2,lnOOrd3,lnOOrd4,                 ;
           lnOOrd5,lnOOrd6,lnOOrd7,lnOOrd8,lnTotOOrd
ENDIF
SELECT Style

*--End of lfCalculat.
*!*************************************************************
*! Name      : lpGrand
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/17/2002
*! Purpose   : Print the grand totals. 
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK4.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpGrand()
*!*************************************************************
FUNCTION lpGrand

*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! GRAND TOTAL
*! TOTAL BOOKED.............................. :
*! WIP....................................... :
*! RECEIVED IN WAREHOUSE..................... :
*! UNITS PICKED.............................. :
*! SHIPPED................................... :
*! UNALLOCATED ORDERS........................ :
*! NALLOCATED STOCK.......................... :
*! AVAILABLE TO SELL AFTER OPEN ORDERS....... :
*! AVAILABLE TO SELL AFTER OPEN & HOLD ORDERS :
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

*-Print the header.
=lfPrntHdr()

@ lnRow,000 SAY '*** GRAND TOTAL'
lnRow = lnRow + 1

@ lnRow,000 SAY ' TOTAL BOOKED.............................. :'
lnCol = 53
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(1,I) PICTURE '9999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,110 SAY laGtot(1,9) PICTURE '999999999'
@ lnRow,120 SAY laGtot(1,10) PICTURE '999999999.99'
lnRow = lnRow + 1

@ lnRow,000 SAY ' WIP....................................... :' 
lnCol = 53
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(2,I) PICTURE '9999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,110 SAY laGtot(2,9) PICTURE '999999999'
@ lnRow,120 SAY laGtot(2,10) PICTURE '999999999.99'
lnRow = lnRow + 1

@ lnRow,000 SAY ' RECEIVED IN WAREHOUSE..................... :'
lnCol = 53
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(3,I) PICTURE '9999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,110 SAY laGtot(3,9) PICTURE '999999999'
@ lnRow,120 SAY laGtot(3,10) PICTURE '999999999.99'
lnRow = lnRow + 1

@ lnRow,000 SAY ' UNITS PICKED.............................. :'
lnCol = 53
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(4,I) PICTURE '9999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,110 SAY laGtot(4,9) PICTURE '999999999'
@ lnRow,120 SAY laGtot(4,10) PICTURE '999999999.99'
lnRow = lnRow + 1

@ lnRow,000 SAY ' SHIPPED................................... :'
lnCol = 53
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(5,I) PICTURE '9999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,110 SAY laGtot(5,9) PICTURE '999999999'
@ lnRow,120 SAY laGtot(5,10) PICTURE '999999999.99'
lnRow = lnRow + 1

@ lnRow,000 SAY ' UNALLOCATED ORDERS........................ :'
lnCol = 53
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(6,I) PICTURE '9999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,110 SAY laGtot(6,9) PICTURE '999999999'
@ lnRow,120 SAY laGtot(6,10) PICTURE '999999999.99'
lnRow = lnRow + 1

@ lnRow,000 SAY ' UALLOCATED STOCK.......................... :'
lnCol = 53
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(7,I) PICTURE '9999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,110 SAY laGtot(7,9) PICTURE '999999999'
@ lnRow,120 SAY laGtot(7,10) PICTURE '999999999.99'
lnRow = lnRow + 1

@ lnRow,000 SAY ' O.T.S. (OPEN ORDERS)...................... :' 
lnCol = 53
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(8,I) PICTURE '9999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,110 SAY laGtot(8,9) PICTURE '999999999'
@ lnRow,120 SAY laGtot(8,10) PICTURE '999999999.99'
lnRow = lnRow + 1

@ lnRow,000 SAY ' O.T.S. (OPEN + HOLD)...................... :'
lnCol = 53
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(9,I) PICTURE '9999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,110 SAY laGtot(9,9) PICTURE '999999999'
@ lnRow,120 SAY laGtot(9,10) PICTURE '999999999.99'
lnRow = lnRow + 1

@ lnRow,000 SAY REPLICATE('-',132)
lnRow = lnRow + 1

*- End of lpGrand.
*!*************************************************************
*! Name      : lfNonMaj
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/17/2002
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      *-- this means that another type is found rather than color or free.
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*--End of lfNonMaj.
*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/17/2002
*! Purpose   : To get the style major segement title.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*--End of lfMajTtGet.
*!*************************************************************
*! Name      : lfMajPic
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/17/2002
*! Purpose   : Get major seg. picture.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************
FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic

*--End of lfMajPic.
*!*************************************************************
*! Name      : lfSetSTY
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/17/2002
*! Purpose   : Go top in the style IN RANGE.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSetSty()
*!*************************************************************
FUNCTION lfSetSty
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   SET ORDER TO TAG CSTYLE IN STYLE
   GO TOP IN STYLE

  CASE OpGrdParm = 'R'
    SET ORDER TO TAG STYLE IN STYLE
ENDCASE

*--End of lfSetSty.
*!*************************************************************
*! Name      : lfvFabric
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/17/2002
*! Purpose   : Validate fabric.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
FUNCTION lfvFabric

lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC')
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF

SET ORDER TO FABRIC IN FABRIC

IF llUseByMe
  USE IN FABRIC
ENDIF

*--End of lfvFabric.
*!*************************************************************
*! Name      : lfvStyle
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/17/2002
*! Purpose   : Validate style.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

lcStyle = VARREAD()
lcTag = ORDER('STYLE')
SET ORDER TO cStyle IN STYLE
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style')
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE

*--End of lfvStyle.