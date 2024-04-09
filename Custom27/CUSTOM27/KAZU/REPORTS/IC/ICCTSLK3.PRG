*:***************************************************************************
*: Program file  : ICCTSLK3.PRG
*: Program desc. : CUSTOMIZED CUT & SOLD FOR CUSTOMER KAZU.
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102579
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICCTSLK3.PRG
*:***************************************************************************
*: Modifications :
*****************************************************************************

*--Make the necessary relation.
=lfRelation()

*-- Init Var used for printing process.
R_Title  = 'SPECIAL CUT & SOLD - SUMMARY REPORT'
R_Width  = 'W'
PageNo   = 00
lnMaxRow = 54
lnRow    = 99
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
*!*************************************************************
*! Name      : lfRelation
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
*! Purpose   : Function to creat the relations between the files.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK3.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfRelation()
*!*************************************************************
FUNCTION lfRelation

SELECT OrdLine
SET RELATION TO "O" + ORDER INTO OrdHdr

SELECT InvLine
SET RELATION TO INVOICE INTO InvHdr

SELECT CuttktL
SET RELATION TO CUTTKT INTO CutTktH

SELECT PoSLn
SET RELATION TO "P" + PO INTO PoSHdr

SELECT STYLE
SET RELATION TO 'S'+Scale INTO Scale ADDI

*--End of lfRelation.
*!*************************************************************
*! Name      : lpPrint
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
*! Purpose   : Print the Report.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK3.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns  : None
*!*************************************************************
*! Example  : =lpPrint()
*!*************************************************************

*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*!Report layout :
*!PROGID                                                           COMPANY NAME                                              DATE      
*!TIME                                                             PROGRAM NAME                                              PAGE#     
*!**************************************************************************************************************************************
*!0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*!°style  °clr°style desc      °fabric °prci.a   °   °z1    °°°    °°°    °°°    °°°    °°°    °°°    z8°   TOTAL  TOTAL$'S %SELL THRU
*! XXXXXXX XXX XXXXXXXXXXXXXXXX XXXXXXX 999999.99 999999 999999 999999 999999 999999 999999 999999 999999 9999999 999999.99    99.99 %
*!-------------------------------------------------------------------------------------------------------------------------------------
*!                CUT QUANTITY BASE AT 99/99/99   °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°°° °°°°°°°°°
*!               UNIT BASE BOOKINGS UP TO 99/99   °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°°° °°°°°°°°°
*!                CUT QUANTITY BASE AT 99/99/99   °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°°° °°°°°°°°°
*!               UNIT BASE BOOKINGS UP TO 99/99   °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°°° °°°°°°°°°
*!                         ------------------------------------------------------------------------------------------------------------
*!                CUT QUANTITY BASE ............  °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°°° °°°°°°°°°
*!               UNIT BASE BOOKINGS ............  °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°°° °°°°°°°°°

*!                            W.I.P ............  °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°°° °°°°°°°°°
*!           UNITS RECEVIED IN W.H. ............  °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°°° °°°°°°°°°
*!                         BOOKINGS ............  °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°°° °°°°°°°°°
*!                         ------------------------------------------------------------------------------------------------------------
*!        O.T.S. (OPEN+HOLD ORDERS) ............  °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°°°               
*!        O.T.S. (OPEN ORDERS ONLY) ............  °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°° °°°°°°°              °°°.°° %
*!0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
FUNCTION lpPrint

SET DEVICE TO PRINT

*-- Headers variabls.
lcCutHdr = 'CUT QUANTITY BASE ............'
lcUntHdr = 'UNIT BASE BOOKINGS ............'
lcCut1Hd = 'CUT QUANTITY BASE AT '
lcUnt1Hd = 'UNIT BASE BOOKINGS UP TO '
lcWipHdr = 'W.I.P ............'
lcRecHdr = 'UNITS RECEVIED IN W.H. ............'
lcBokHdr = 'BOOKINGS ............'
lcHldHdr = 'O.T.S. (OPEN+HOLD ORDERS) ............'
lcOpnHdr = 'O.T.S. (OPEN ORDERS ONLY) ............'

*--Initialize needed variables.
*-Qty base var.
STORE 0 TO lnCtQty1,lnCtQty2,lnCtQty3,lnCtQty4,lnCtQty5,lnCtQty6,lnCtQty7,lnCtQty8,lnCtTot
STORE 0 TO lnBook1,lnBook2,lnBook3,lnBook4,lnBook5,lnBook6,lnBook7,lnBook8,lnTotBook

*-Unt Recv in WH var.
STORE 0 TO lnRcvWH1,lnRcvWH2,lnRcvWH3,lnRcvWH4,lnRcvWH5,lnRcvWH6,lnRcvWH7,lnRcvWH8,lnTRcvWH

*-Open orders var.
STORE 0 TO OOrd1,OOrd2,OOrd3,OOrd4,OOrd5,OOrd6,OOrd7,OOrd8,TotOOrd

DIMENSION laGtot[7,10]
laGtot  = 00
lnFstSz = 50
lnLstSZ = 106

SELECT Style
SCAN FOR &lcRpexp
  lcKey = Style
  WAIT WINDOW 'Selecting data for style...' +  lcKey NOWAIT
  llPrnGrnd = .T.

  *--Check page and header.
  =lfChkPage()

  *--Report detail.
  *-Line 1........................................................
  @ lnRow,000 SAY Style
  @ lnRow,020 SAY LEFT(Desc,12)
  @ lnRow,033 SAY Fabric
  @ lnRow,041 SAY PriceA PICTURE '999999.99'

  lnCost = PriceA
  lnCol  = 51
  FOR I = 1 TO SCALE.Cnt
    Z = STR(I,1)
    @ lnRow,lnCol SAY SCALE.SZ&Z
    lnCol = lnCol + 7
  ENDFOR
  @ lnRow,108 SAY "TOTAL  TOTAL$'S %SELL TH"
  lnRow = lnRow + 1
  @ lnRow,000 SAY REPLICATE('-',132)
  lnRow = lnRow + 1

  *-...................................................

  =lfQtyBase()

  *-...................................................
  @ lnRow,028 SAY lcWipHdr

  =lfQtyWIP()

  *-...................................................
  @ lnRow,011 SAY lcRecHdr

  =lfRcvWhs()

  *-...................................................
  @ lnRow,025 SAY lcBokHdr

  =lfQtyBok()

  @ lnRow,027 SAY REPLICATE('-',105)
  lnRow = lnRow + 1

  *-...................................................
  @ lnRow,008 SAY lcHldHdr

  =lfOHots()

  *-...................................................
  @ lnRow,008 SAY lcOpnHdr

  =lfOots()

  *--Accomulate fields.
  =lpAccuml()

ENDSCAN
WAIT CLEAR

IF llPrnGrnd
  =lpGrand()
ENDIF

*--End of lpPrint.
*!*************************************************************
*! Name      : lfChkPage
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
*! Purpose   : Function to print the header when change.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK3.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfChkPage()
*!*************************************************************
FUNCTION lfChkPage

IF lnRow > lnMaxRow
  PAGENO = PAGENO + 1
  DO RPT_HDR WITH 'ICCTSLK3',lcRPTitle,R_WIDTH
  lnRow = 05
ENDIF

*--End of lfChkPage.
*!*************************************************************
*! Name      : lpAccuml
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
*! Purpose   : Accomulate the fields.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK3.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpAccuml()
*!*************************************************************
FUNCTION lpAccuml

FOR I = 1 TO 8
  Z = STR(I,1)
  laGtot(1,I) = laGtot(1,I) + lnCtQty&Z
  laGtot(2,I) = laGtot(2,I) + lnBook&Z
  laGtot(3,I) = laGtot(3,I) + WIP&Z
  laGtot(4,I) = laGtot(4,I) + lnRcvWH&Z
  laGtot(5,I) = laGtot(5,I) + (Ord&Z+Shp&Z)
  laGtot(6,I) = laGtot(6,I) + (WIP&Z+lnRcvWH&Z) - (Ord&Z+Shp&Z)
  laGtot(7,I) = laGtot(7,I) + (WIP&Z+lnRcvWH&Z) - (OOrd&Z+SHP&Z)
ENDFOR

laGtot(1,9)  = laGtot(1,9)  + lnCtTot
laGtot(2,9)  = laGtot(2,9)  + lnTotBook
laGtot(3,9)  = laGtot(3,9)  + TotWIP
laGtot(4,9)  = laGtot(4,9)  + lnTRcvWH
laGtot(5,9)  = laGtot(5,9)  + (TotOrd+TotShp)
laGtot(6,9)  = laGtot(6,9)  + (TotWIP+lnTRcvWH) - (TotOrd+TotShp)
laGtot(7,9)  = laGtot(7,9)  + (TotWIP+lnTRcvWH) - (TotOOrd+TotShp)
laGtot(1,10) = laGtot(1,10) + (lnCtTot*lnCost)
laGtot(2,10) = laGtot(2,10) + (lnTotBook*lnCost)
laGtot(3,10) = laGtot(3,10) + (TotWIP*lnCost)
laGtot(4,10) = laGtot(4,10) + (lnTRcvWH*lnCost)
laGtot(5,10) = laGtot(5,10) + ((TotOrd+TotShp)*lnCost)

*--End of lpAccuml.
*!*************************************************************
*! Name      : lfQtyBase
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
*! Purpose   : Collect the quantity data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK3.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns  : None
*!*************************************************************
*! Example  : =lfQtyBase()
*!*************************************************************
FUNCTION lfQtyBase

IF Style.Make
  SELECT CutTktH.Entered, CutTktL.Cuttkt, CutTktL.Style,CutTktL.TranCd ,;
         SUM(CutTktL.Qty1) AS Qty1, SUM(CutTktL.Qty2) AS Qty2          ,;
         SUM(CutTktL.Qty3) AS Qty3, SUM(CutTktL.Qty4) AS Qty4          ,;
         SUM(CutTktL.Qty5) AS Qty5, SUM(CutTktL.Qty6) AS Qty6          ,;
         SUM(CutTktL.Qty7) AS Qty7, SUM(CutTktL.Qty8) AS Qty8          ,;
         SUM(CutTktL.TotQty) AS TotQty                                  ;
    FROM CutTktH,CutTktL                                                ;
   WHERE CutTktL.CutTkt = CutTktH.CutTkt                                ;
     AND (CutTktH.Status <> 'X'                                         ;
     AND CutTktl.Style  = lcKey                                         ;
     AND CutTktL.TranCd = '1')                                          ;
   GROUP BY CutTktH.Entered                                             ;
    INTO DBF (GCWORKDIR+lcCtkBaz)                                       ;
   ORDER BY 1
ELSE
  SELECT PoSHdr.Entered, PoSln.Po, PoSln.Style,PoSln.TranCd            ,;
         SUM(PoSln.Qty1) AS Qty1, SUM(PoSln.Qty2) AS Qty2              ,;
         SUM(PoSln.Qty3) AS Qty3, SUM(PoSln.Qty4) AS Qty4              ,;
         SUM(PoSln.Qty5) AS Qty5, SUM(PoSln.Qty6) AS Qty6              ,;
         SUM(PoSln.Qty7) AS Qty7, SUM(PoSln.Qty8) AS Qty8              ,;
         SUM(PoSln.TotQty) AS TotQty                                    ;
    FROM PoSHdr,PoSln                                                   ;
   WHERE PoSln.Po = PoSHdr.Po                                           ;
     AND (PoSHdr.Status <> 'X'                                          ;
     AND PoSln.Style  = lcKey                                           ;
     AND PoSln.TranCd = '1')                                            ;
   GROUP BY PoSHdr.Entered                                              ;
    INTO DBF (GCWORKDIR+lcCtkBaz)                                       ;
   ORDER BY 1
ENDIF

STORE 0 TO lnCtQty1,lnCtQty2,lnCtQty3,lnCtQty4,lnCtQty5,lnCtQty6,lnCtQty7,lnCtQty8,lnCtTot
STORE 0 TO lnOBook1,lnOBook2,lnOBook3,lnOBook4,lnOBook5,lnOBook6,lnOBook7,lnOBook8,lnTotOBook
STORE 0 TO lnIBook1,lnIBook2,lnIBook3,lnIBook4,lnIBook5,lnIBook6,lnIBook7,lnIBook8,lnTotIBook

SELECT (lcCtkBaz)
SCAN
  @ lnRow,016 SAY lcCut1Hd + DTOC(Entered)
  ldEntered = Entered
  lnCol = lnFstSz
  FOR I = 1 TO SCALE.Cnt
    Z = STR(I,1)
    @ lnRow,lnCol SAY Qty&Z PICTURE '999999'
    lnCtQty&Z = lnCtQty&Z + Qty&Z
    lnCol = lnCol+7
  ENDFOR
  @ lnRow,lnLstSZ SAY TotQty PICTURE '9999999'
  lnCtTot  = lnCtTot + TotQty
  lnRow = lnRow + 1
  @ lnRow,015 SAY lcUnt1Hd + SUBSTR(DTOC(Entered),1,5)
  SELECT ORDLINE
  IF SEEK(lcKey)
    SUM REST QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY ;
        WHILE Style = lcKey ;
        FOR ORDHDR.Status$'OH' .AND. ORDHDR.Entered<=ldEntered ;
        TO lnOBook1,lnOBook2,lnOBook3,lnOBook4,lnOBook5,lnOBook6,lnOBook7,lnOBook8,lnTotOBook
  ENDIF
  SELECT INVLINE
  IF SEEK(lcKey)
    SUM REST QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY ;
    WHILE Style = lcKey ;
    FOR INVHDR.Status <> 'V' .AND. InvDate<=ldEntered ;
    TO lnIBook1,lnIBook2,lnIBook3,lnIBook4,lnIBook5,lnIBook6,lnIBook7,lnIBook8,lnTotIBook
  ENDIF
  lnCol = lnFstSz
  FOR I = 1 TO SCALE.Cnt
    Z = STR(I,1)
    @ lnRow,lnCol SAY (lnOBook&Z+lnIBook&Z) PICTURE '999999'
    lnBook&Z = (lnOBook&Z+lnIBook&Z)
    lnCol = lnCol + 7
  ENDFOR
  @ lnRow,lnLstSZ SAY (lnTotOBook+lnTotIBook) PICTURE '9999999'
  lnTotBook = (lnTotOBook+lnTotIBook)
  lnRow = lnRow + 1
  =lfChkPage()
ENDSCAN

SELECT (lcCtkBaz)
LOCATE
IF !EOF()
  @ lnRow,027 SAY REPLICATE('-',105)
  lnRow = lnRow+1
ENDIF

SELECT STYLE
@ lnRow,016 SAY lcCutHdr
lnCol = lnFstSz
FOR I = 1 TO SCALE.Cnt
  Z = STR(I,1)
  @ lnRow,lnCol SAY lnCtQty&Z PICTURE '999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,lnLstSZ SAY lnCtTot PICTURE '9999999'
@ lnRow,114 SAY (lnCtTot*lnCost) PICTURE '999999.99'

IF lnCtTot <> 0
  @ lnRow,124 SAY ((TotOrd+TotShp)/lnCtTot)*100 PICTURE '999.999'
ELSE
  @ lnRow,124 SAY 0 PICTURE '999.999'
ENDIF
@ lnRow,131 SAY '%'
lnRow = lnRow + 1

@ lnRow,015 SAY lcUntHdr
lnCol = lnFstSz
FOR I = 1 TO SCALE.Cnt
  Z = STR(I,1)
  lnBook&Z = (lnOBook&Z+lnIBook&Z)
  @ lnRow,lnCol SAY lnBook&Z PICTURE '999999'
  lnCol = lnCol + 7
ENDFOR
lnTotBook = (lnTotOBook+lnTotIBook)
@ lnRow,lnLstSZ SAY lnTotBook PICTURE '9999999'
@ lnRow,114 SAY (lnTotBook*lnCost) PICTURE '999999.99'

lnRow = lnRow + 2

*--End of lfQtyBase.
*!*************************************************************
*! Name      : lfQtyWIP
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
*! Purpose   : Collect the quantity data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK3.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfQtyWIP()
*!*************************************************************
FUNCTION lfQtyWIP

lnCol = lnFstSz
FOR I = 1 TO SCALE.Cnt
  Z = STR(I,1)
  @ lnRow,lnCol SAY WIP&Z PICTURE '999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,lnLstSZ SAY TotWIP PICTURE '9999999'
@ lnRow,114 SAY (TotWIP*lnCost) PICTURE '999999.99'
lnRow = lnRow + 1

*--End of lfQtyWIP.
*!*************************************************************
*! Name      : lfRcvWhs
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
*! Purpose   : Collect the quantity data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK3.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfRcvWhs()
*!*************************************************************
FUNCTION lfRcvWhs

SELECT CUTTKTL
=SEEK(lcKey)
SUM REST QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY ;
    WHILE Style = lcKey FOR Trancd = '2'                ;
    TO lnCRvW1,lnCRvW2,lnCRvW3,lnCRvW4,lnCRvW5,lnCRvW6,lnCRvW7,lnCRvW8,lnCRvWT

SELECT POSLN
=SEEK(lcKey)
SUM REST QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY ;
    WHILE Style = lcKey FOR Trancd='2'                  ;
    TO lnPRvW1,lnPRvW2,lnPRvW3,lnPRvW4,lnPRvW5,lnPRvW6,lnPRvW7,lnPRvW8,lnPRvWT

lnCol = lnFstSz
FOR I = 1 TO SCALE.Cnt
  Z = STR(I,1)
  lnRcvWH&Z = (lnCRvW&Z+lnPRvW&Z)
  @ lnRow,lnCol SAY lnRcvWH&Z PICTURE '999999'
  lnCol = lnCol + 7
ENDFOR
lnTRcvWH = (lnCRvWT+lnPRvWT)
@ lnRow,lnLstSZ SAY lnTRcvWH PICTURE '9999999'
@ lnRow,114 SAY (lnTRcvWH*lnCost) PICTURE '999999.99'
SELECT STYLE

IF lnTrcvWh <> 0
  @ lnRow,124 SAY (TotOrd+TotShp/lnTrcvWh)*100 PICTURE '999.999'
ELSE
  @ lnRow,124 SAY 0 PICTURE '999.999'
ENDIF
@ lnRow,131 SAY '%'

lnRow = lnRow + 1

*--End of lfRcvWhs.
*!*************************************************************
*! Name      : lfQtyBok
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
*! Purpose   : Collect the quantity data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK3.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfQtyBok()
*!*************************************************************
FUNCTION lfQtyBok

lnCol = lnFstSz
FOR I = 1 TO SCALE.Cnt
  Z = STR(I,1)
  @ lnRow,lnCol SAY (Ord&Z+Shp&Z)  PICTURE '999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,lnLstSZ SAY (TotOrd+TotShp) PICTURE '9999999'
@ lnRow,114 SAY ((TotOrd+TotShp)*lnCost) PICTURE '999999.99'
lnRow = lnRow + 1

*--End of lfQtyBok.
*!*************************************************************
*! Name      : lfOHots
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
*! Purpose   : Collect the quantity data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK3.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfOHots()
*!*************************************************************
FUNCTION lfOHots

lnCol = lnFstSz
FOR I = 1 TO SCALE.Cnt
  Z = STR(I,1)
  @ lnRow,lnCol SAY (WIP&Z+lnRcvWH&Z)-(Ord&Z+Shp&Z) PICTURE '999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,lnLstSZ SAY (TotWIP+lnTRcvWH)-(TotOrd+TotShp) PICTURE '9999999'
lnRow = lnRow + 1

*--End of lfOHots.
*!*************************************************************
*! Name      : lfOots
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
*! Purpose   : Collect the quantity data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK3.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfOots()
*!*************************************************************
FUNCTION lfOots

SELECT ORDLINE
=SEEK(lcKey)
SUM REST QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY ;
    WHILE Style = lcKey FOR ORDHDR.Status='O'           ;
    TO OOrd1,OOrd2,OOrd3,OOrd4,OOrd5,OOrd6,OOrd7,OOrd8,TotOOrd

SELECT STYLE
lnCol = lnFstSz
FOR I = 1 TO SCALE.Cnt
  Z = STR(I,1)
  @ lnRow,lnCol SAY (WIP&Z+lnRcvWH&Z)-(OOrd&Z+Shp&Z) PICTURE '999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,lnLstSZ SAY (TotWIP+lnTRcvWH)-(TotOOrd+TotShp) PICTURE '9999999'

lnRow = lnRow + 3

*--End of lfOots.
*!*************************************************************
*! Name      : lpGrand
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
*! Purpose   : Print the grand totals. 
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ICCTSLK3.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpGrand()
*!*************************************************************
FUNCTION lpGrand

=lfChkPage()

IF lnRow<>5
  @ lnRow,000 SAY REPLICATE('=',132)
ENDIF
lnRow = lnRow + 1
@ lnRow,000 SAY '*** GRAND TOTAL'
lnRow = lnRow + 1

@ lnRow,016 SAY lcCutHdr
lnCol = lnFstSz
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(1,I) PICTURE '999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,lnLstSZ SAY laGtot(1,9) PICTURE '9999999'
@ lnRow,113 SAY laGtot(1,10) PICTURE '9999999.99'
@ lnRow,124 SAY (laGtot(5,9)/laGtot(1,9))*100 PICTURE '999.999'
@ lnRow,131 SAY '%'

lnRow = lnRow + 1

@ lnRow,015 SAY lcUntHdr
lnCol = lnFstSz
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(2,I) PICTURE '999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,lnLstSZ SAY laGtot(2,9) PICTURE '9999999'
@ lnRow,113 SAY laGtot(2,10) PICTURE '9999999.99'
lnRow = lnRow + 1

@ lnRow,028 SAY lcWipHdr
lnCol = lnFstSz
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(3,I) PICTURE '999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,lnLstSZ SAY laGtot(3,9) PICTURE '9999999'
@ lnRow,113 SAY laGtot(3,10) PICTURE '9999999.99'
lnRow = lnRow + 1

@ lnRow,011 SAY lcRecHdr
lnCol = lnFstSz
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(4,I) PICTURE '999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,lnLstSZ SAY laGtot(4,9) PICTURE '9999999'
@ lnRow,113 SAY laGtot(4,10) PICTURE '9999999.99'

IF laGtot(3,9)+laGtot(4,9) <> 0
  @ lnRow,124 SAY (laGtot(2,9)/(laGtot(3,9)+laGtot(4,9)))*100 PICTURE '999.999'
ELSE
  @ lnRow,124 SAY 0 PICTURE '999.999'
ENDIF
@ lnRow,131 SAY '%'

lnRow = lnRow + 1
@ lnRow,025 SAY lcBokHdr
lnCol = lnFstSz
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(5,I) PICTURE '999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,lnLstSZ SAY laGtot(5,9) PICTURE '9999999'
@ lnRow,113 SAY laGtot(5,10) PICTURE '9999999.99'
lnRow = lnRow + 1
@ lnRow,027 SAY REPLICATE('-',105)
lnRow = lnRow + 1

@ lnRow,008 SAY lcHldHdr
lnCol = lnFstSz
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(6,I) PICTURE '999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,lnLstSZ SAY laGtot(6,9) PICTURE '9999999'
lnRow = lnRow + 1

@ lnRow,008 SAY lcOpnHdr
lnCol = lnFstSz
FOR I = 1 TO 8
  @ lnRow,lnCol SAY laGtot(7,I) PICTURE '999999'
  lnCol = lnCol + 7
ENDFOR
@ lnRow,lnLstSZ SAY laGtot(7,9) PICTURE '9999999'

lnRow = lnRow + 1
@ lnRow,000 SAY REPLICATE('=',132)
lnRow = lnRow + 1

*- End of lpGrand.
*!*************************************************************
*! Name      : lfNonMaj
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
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
      lnClrPo   = laMajSeg[lnI,4]
      lcFreeClr = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      *-- this means that another type is found rather than color or free
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
*! Name      : lfvStyle
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
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
*!**************************************************************************
*! Name      : lfSetSTY
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
*! Purpose   : Go top in the style IN RANGE
*!**************************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetSty()
*!**************************************************************************
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
*! Name      : lfMajTtlGet
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
*! Purpose   : To get the style major segement title
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
*! Date      : 03/11/2002
*! Purpose   : Get major seg. picture
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
*! Name      : lfvFabric
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/11/2002
*! Purpose   : Validate fabric
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