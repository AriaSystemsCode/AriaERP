*:***************************************************************************
*: Program file  : POSTYDMO.PRG
*: Program desc. : Custom P/O Detail Report. [For Mom10]
*: Date          : 12/14/2003
*: System        : Aria Advantage Series.
*: Module        : STYLE PURCHASE ORDER [PO]
*: Developer     : Abdou Elgendy [ABD]
*: Tracking #    : C#120990,1.
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POSTYDMO
*:***************************************************************************
*: Modifications  : None.
*:***************************************************************************
*:C121153,1 ASH 01/11/2004 Print cost from style.TotCost and price from style.priceA
*:***************************************************************************
XREP     = SPACE(0)
R_WIDTH  = 'W'    && STANDARD REPORT IS 'WIDE'
XREPORT  = 'POSTYDRE'
R_TITLE  = 'STYLE PURCHASE ORDER DETAIL REPORT'

WorkFile   = gfTEMPNAME()
lcOpenLine = gfTempName()

lnReportPO = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSLN.TRANCD'),1)

lcRep      = ALLTRIM(laOGFxFlt[lnReportPO,6])
lcRep      = STRTRAN(lcRep,'|',SPACE(0))
lcShpTrnCd = IIF(lcRpPoType='P','3','6')
lcOldRep   = lcRep
lcRep      = IIF(lcRpPoType='P',lcRep,STRTRAN(lcRep,"3","6"))


IF EMPTY(lcRep)
  XREP = "ORDCS"
ELSE  
  FOR lnI = 1 To LEN(lcRep)
    lcType = SUBSTR(lcRep,lnI,1)
    XREP = XREP + lfType(lcType)
  ENDFOR
ENDIF  

XREP1   = TRIM(XREP)

XTITLE  = lcRpTitle
lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),""," AND ") + "Posln.cStyType = lcRpPoType"

IF lcRpPoType='N'
  lnTrPos   = AT("POSLN.TRANCD",lcRpExp)
  lcMedStr  = SUBSTR(lcRpExp,lnTrPos)
  lnBrakPos = AT(")",lcMedStr)
  lcMedStr  = SUBSTR(lcMedStr,1,lnBrakPos)
  lcNewTrns = STRTRAN(lcMedStr,lcOldRep,lcRep)
  lcRpExp   = STRTRAN(lcRpExp,lcMedStr,lcNewTrns)
ENDIF

lcTrnsCods = '1245' + lcShpTrnCd
XF = IIF(EMPTY(laOGFxFlt[lnReportPO,6]),lcTrnsCods,laOGFxFlt[lnReportPO,6])

IF lnReportPO > 0
  =ACOPY(laOgFxFlt,laCopyFxFl)
  =ADEL(laCopyFxFl,lnReportPO)
  DIMENSION laCopyFxFl[ALEN(laCopyFxFl,1) - 1,7]
  lcOGFxFlt = gfGenFlt('laCopyFxFl' , llOGFilter , .T.)
  lcRpExp   = IIF(EMPTY(lcOGFxFlt),'.T.',lcOGFxFlt)
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),""," AND ") + "Posln.cStyType = lcRpPoType"
ENDIF

STORE 0 TO lnGrn1 ,lnGrn2 ,lnGrn3 ,lnGrn4 ,lnGrn5 ,lnGrn6 ,lnGrn7 ,lnGrn8 ,lnGrnAmt

SELECT POSLN
SET ORDER TO 0
SET RELATION TO
SET FILTER   TO
GOTO TOP

SORTFIELD = ' '
BREAK     = ' '
XNOTEPAD  = .F.

llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
             (!EMPTY(laOGFxFlt[lnCurrPos,6]) AND ATC("|",laOGFxFlt[lnCurrPos,6])=0)
llCurInGrp = !llPrintTot
lnMajPic = 0
=lfMajGet()

*-- This is for sort selection
DO CASE
  
  * SUMMARY BY ORDER#
  CASE lcRpSortBy = 'P'
    XNOTEPAD  = (lcRpNote = 'Y') AND (lcRpSumDet = "D")
    IF lcRpFormat = 'B'
      IF llCurInGrp
        SORTFIELD = 'cCurrCode+PO+STYLE+TRANCD'
      ELSE
        SORTFIELD = 'PO+STYLE+TRANCD'
      ENDIF
    ELSE
      IF llCurInGrp
        SORTFIELD = 'cCurrCode+PO+TRANCD+STYLE'
      ELSE
        SORTFIELD = 'PO+TRANCD+STYLE'
      ENDIF
    ENDIF
    IF llCurInGrp
      BREAK    = 'cCurrCode + "/" + PO'
    ELSE
      BREAK    = 'PO'
    ENDIF

  * SUMMARY BY VENDOR ACCOUNT
  CASE lcRpSortBy = 'V'
    IF lcRpFormat = 'B'
      IF llCurInGrp
        SORTFIELD = 'POSHDR.VENDOR+cCurrCode+PO+STYLE+TRANCD'
      ELSE
        SORTFIELD = 'POSHDR->VENDOR+PO+STYLE+TRANCD'
      ENDIF
    ELSE
      IF llCurInGrp
        SORTFIELD = 'POSHDR.VENDOR+cCurrCode+PO+TRANCD+STYLE'
      ELSE
        SORTFIELD = 'POSHDR->VENDOR+PO+TRANCD+STYLE'
      ENDIF
    ENDIF

    IF llCurInGrp
      BREAK    = 'POSHDR.VENDOR + "/" + cCurrCode'
    ELSE
      BREAK    = 'POSHDR.VENDOR'
    ENDIF

  * SUMMARY BY PRIOIRTY
  CASE lcRpSortBy = 'S'
    IF lcRpFormat = 'B'
      IF llCurInGrp
        SORTFIELD = 'PADR(STYLE,lnMajPic)+cCurrCode+PO+TRANCD'
      ELSE
        SORTFIELD = 'STYLE+PO+TRANCD'
      ENDIF
    ELSE
      IF llCurInGrp
        SORTFIELD = 'PADR(STYLE,lnMajPic)+cCurrCode+TRANCD+PO'
      ELSE
        SORTFIELD = 'STYLE+TRANCD+PO'
      ENDIF
    ENDIF

    IF llCurInGrp
      BREAK    = 'SUBSTR(STYLE,1,lnMajPic) + "/" + cCurrCode'
    ELSE
      BREAK    = 'SUBSTR(STYLE,1,lnMajPic)'
    ENDIF

ENDCASE

XPO=SPACE(6)
SET DEVICE TO PRINT
DO lpCollData
SET DEVICE TO SCREEN

IF USED(WorkFile)
  USE IN (WorkFile)
ENDIF
ERASE (gcWorkDir+WorkFile+'.DBF')
ERASE (gcWorkDir+WorkFile+'.CDX')
IF USED(lcOpenLine)
  USE IN (lcOpenLine)
ENDIF

*-- end of program code.
*:*************************************************************
*: Name      : lpCollData
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : Collecting data
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : DO lpCollData
*:*************************************************************
*:
PROCEDURE lpCollData
llContnPrn = .T.
SELECT POSLN
SET FILTER TO
SET RELATION TO
SET RELATION TO cStyType+PO INTO POSHDR
SET RELATION TO STYLE INTO STYLE ADDITIVE

WAIT 'Selecting records for report ...' WINDOW NOWAIT
LOCATE ALL FOR &lcRpExp
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  WAIT CLEAR
  =gfModalGen('TRM00052B40011','ALERT')
  llContnPrn = .F.
ENDIF

IF llContnPrn

  lnPrnPrice = 0
  =lfCreatCur()
  SELECT POSLN
  DIMENSION laTempStru[1,4]
  lnFldsLen = AFIELDS(laTempStru)
  DIMENSION laTempStru[lnFldsLen + 1,4]
  laTempStru[lnFldsLen + 1,1] = "CCURRCODE"
  laTempStru[lnFldsLen + 1,2] = "C"
  laTempStru[lnFldsLen + 1,3] = 3
  laTempStru[lnFldsLen + 1,4] = 0
  CREATE TABLE &gcWorkDir.&WorkFile FROM ARRAY laTempStru
  SELECT POSLN
  SCAN FOR &lcRpExp
    SCATTER MEMVAR MEMO
    m.cCurrCode = POSHDR.CPRICECUR
    INSERT INTO (WorkFile) FROM MEMVAR
  ENDSCAN

  SELECT POSLN
  SET RELATION TO
  SELECT (WorkFile)
  SET RELATION TO cStyType+PO INTO POSHDR
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  SET RELATION TO 'S' + SCALE INTO SCALE ADDITIVE
  SET RELATION TO ShipNo INTO ShpmtHdr ADDITIVE

  INDEX ON PO+STYLE+TRANCD TAG (WorkFile) OF (gcWorkDir+WorkFile+'.CDX')
  GO TOP
  xPo=SPACE(6)
  xStyClr=SPACE(1)
  DO WHILE !EOF()
    IF xPO+xStyClr<> PO+Style
      xPo=Po
      xStyClr=Style
      IF POSHDR.STATUS = 'X' OR POSHDR.STATUS='C'
        STORE 0 TO lnOpen1,lnOpen2,lnOpen3,lnOpen4,lnOpen5,lnOpen6,lnOpen7,lnOpen8
        SCATTER MEMVAR MEMO
        m.Date=POSHDR.POCANCEL
        SUM REST IIF(Trancd='1',Qty1,-Qty1),IIF(Trancd='1',Qty2,-Qty2), IIF(Trancd='1',Qty3,-Qty3),;
                 IIF(Trancd='1',Qty4,-Qty4), IIF(Trancd='1',Qty5,-Qty5), IIF(Trancd='1',Qty6,-Qty6), ;
                 IIF(Trancd='1',Qty7,-Qty7), IIF(Trancd='1',Qty8,-Qty8) ;
            TO lnOpen1,lnOpen2,lnOpen3,lnOpen4,lnOpen5,lnOpen6,lnOpen7,lnOpen8;
            WHILE PO+STYLE=m.PO+m.STYLE
        IF MAX(lnOpen1,0)+MAX(lnOpen2,0)+MAX(lnOpen3,0)+MAX(lnOpen4,0)+;
           MAX(lnOpen5,0)+MAX(lnOpen6,0)+MAX(lnOpen7,0)+MAX(lnOpen8,0) > 0
          lnRecNo=RECNO()
          APPEND BLANK
          m.Trancd='5'
          FOR lnCounter=1 TO 8
            lnC = STR(lnCounter,1)
            m.Qty&lnC=MAX(lnOpen&lnC,0)
          ENDFOR
          m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
          GATHER MEMVAR MEMO
          GOTO lnRecNo
        ENDIF          
      ELSE 
        SKIP
      ENDIF
    ELSE
      SKIP
    ENDIF
  ENDDO
   
  IF SORTFIELD<>' '
    Z = LTRIM(STR(RECCOUNT(),7))
    WAIT 'SORTING ' + Z + ' RECORDS FOR STYLE PURCHASE ORDER DETAIL REPORT...' WINDOW NOWAIT
    INDEX ON &SORTFIELD TAG (WorkFile) OF (gcWorkDir+WorkFile+'.CDX')
    SET ORDER TO TAG (WorkFile)
  ENDIF
  
  DO lpPrint
  DO ENDREPORT

ENDIF

*:*************************************************************
*: Name      : lpPrint
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : Print the report
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : DO lpPrint
*:*************************************************************
*:
PROCEDURE lpPrint
DIMENSION XTOTAL(5,10),XTOTAL1(5,10)
XTOTAL  = 0.00
XTOTAL1 = 0.00
PAGENO  = 0
ROW     = 99
XTIME   = TIME()

SELECT (WorkFile)
GOTO TOP

IF LEN(TRIM(BREAK)) <>0
  HBREAK = &BREAK
ENDIF

SELE (WorkFile)

PTRANCD=SPACE(1)
*---------------------------------------------------------
* [REPORT] LOOP
*---------------------------------------------------------
lnLineNo = LineNo
lnOldPo  = PO

lcScale = SPACE(0)
llNewPg = .T.

DO WHILE INKEY() <>32
  IF TRANCD = '1'
    =lfChkAddLn()
  ENDIF

  SELE (WorkFile)
  IF ROW >=53

    PAGENO = PAGENO+1

    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH

    @ 05,00 SAY '                AVAILABLE'
    @ 06,00 SAY '                RECEIVED'


*-- format is 'B'
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*PO#    VENDOR   E.T.A.   SHPMT# S STYLE               DYELOT     TYPQTY1  QTY2  QTY3  QTY4 QTY5 QTY6 QTY7 QTY8 TOTQTY $PRICE AMOUNT

*-- format is 'A'
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*PO#    VENDOR   E.T.A.   SHPMT# S STYLE               DYELOT        QTY1  QTY2  QTY3  QTY4 QTY5 QTY6 QTY7 QTY8 TOTQTY $PRICE AMOUNT

    IF lcRpFormat = 'B'	
      IF lcUseDye = 'Y'
        @ 07,00 SAY 'PO#    VENDOR   E.T.A.   SHPMT# S STYLE               DYELOT     TYP QTY1 QTY2 QTY3 QTY4 QTY5 QTY6 QTY7 QTY8 TOTQTY '+IIF(lcRPSumDet ='D','PRICE'+SPACE(4),SPACE(9))+'AMOUNT'
      ELSE
        @ 07,00 SAY 'PO#    VENDOR   E.T.A.   SHPMT# S STYLE              TYP  QTY1   QTY2   QTY3   QTY4   QTY5   QTY6   QTY7   QTY8  TOTQTY'+IIF(lcRPSumDet ='D',' PRICE'+SPACE(1),SPACE(7))+'AMOUNT'
      ENDIF
    ELSE
      IF lcUseDye = 'Y'
         @ 07,00 SAY 'PO#    VENDOR   E.T.A.   SHPMT# S STYLE               DYELOT     QTY1 QTY2 QTY3 QTY4 QTY5 QTY6 QTY7 QTY8  TOTQTY '+IIF(lcRPSumDet ='D','  PRICE'+SPACE(5),SPACE(12))+'AMOUNT'
      ELSE
         @ 07,00 SAY 'PO#    VENDOR   E.T.A.   SHPMT# S STYLE                 QTY1   QTY2   QTY3   QTY4   QTY5   QTY6   QTY7   QTY8  TOTQTY '+IIF(lcRPSumDet ='D','PRICE'+SPACE(3),SPACE(8))+'AMOUNT'
      ENDIF
    ENDIF
    @ 08,00 SAY REPLICATE('=',132)
    ROW = 09
    llNewPg = .T.
  ENDIF

  DO WHILE LEN(TRIM(BREAK)) <>0
    IF &BREAK = HBREAK
      WAIT 'Printing PO#' + PO + ' - <SPACE BAR> to abort' WINDOW NOWAIT
      EXIT
    ENDIF

    ROW=ROW+1
    @ ROW,00 SAY REPLICATE('=',132)
    
    IF XNOTEPAD
      ROW=ROW+1
      DO PRT_NOTE WITH 'P',XPO
    ENDIF
    ROW = ROW+1
    @ ROW,000 SAY '* SUB TOTAL * '
    @ ROW,015 SAY HBREAK
    ROW=ROW+1

    IF lcRpFormat = 'A'
      IF (LEN(XREP)=0) .OR. ('O'$XREP)
        @ ROW,000 SAY '  ORDERED................................'
        IF lcUseDye = 'Y'
          lnCol  = 065
          lnIncr = 5
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(1,lnI)  PICTURE '9999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,104 SAY XTOTAL(1,9)  PICTURE '99999999'
          @ ROW,117 SAY XTOTAL(1,10) PICTURE '99999999999.99' 
        ELSE
          lnCol  = 054
          lnIncr = 7
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(1,lnI)  PICTURE '999999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,108 SAY XTOTAL(1,9)  PICTURE '99999999'
          @ ROW,118 SAY XTOTAL(1,10) PICTURE '99999999999.99'
        ENDIF
        ROW = ROW+1
      ENDIF
    ENDIF
		 			
    IF lcRpFormat = 'B'
      IF (LEN(XREP)=0) .OR. ('O'$XREP)
        @ ROW,000 SAY '  ORDERED................................'
        IF lcUseDye = 'Y'
          lnCol  = 069
          lnIncr = 5
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(1,lnI)  PICTURE '9999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,lnCol SAY XTOTAL(1,9)  PICTURE '999999'
          @ ROW,118 SAY XTOTAL(1,10) PICTURE '99999999999.99'
        ELSE
          lnCol  = 056
          lnIncr = 7
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(1,lnI)  PICTURE '999999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,lnCol SAY XTOTAL(1,9)  PICTURE '9999999'
          @ ROW,119 SAY XTOTAL(1,10) PICTURE '9999999999.99'
        ENDIF
        ROW = ROW+1
      ENDIF
    ENDIF

    IF lcRpFormat = 'A'
      IF (LEN(XREP)=0) .OR. ('R'$XREP)
        @ ROW,000 SAY '  RECEIVED...............................'
        IF lcUseDye = 'Y'
          lnCol  = 065
          lnIncr = 5
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(2,lnI)  PICTURE '9999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,lnCol-1 SAY XTOTAL(2,9)  PICTURE '99999999'
          @ ROW,117 SAY XTOTAL(2,10) PICTURE '99999999999.99'
        ELSE
          lnCol  = 054
          lnIncr = 7
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(2,lnI)  PICTURE '999999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,lnCol SAY XTOTAL(2,9)  PICTURE '9999999'
          @ ROW,118 SAY XTOTAL(2,10) PICTURE '99999999999.99'
        ENDIF
        ROW = ROW+1
      ENDIF
    ENDIF
		
    IF lcRpFormat = 'B'
      IF (LEN(XREP)=0) .OR. ('R'$XREP)
        @ ROW,000 SAY '  RECEIVED...............................'
        IF lcUseDye = 'Y'
          lnCol  = 069
          lnIncr = 5
          
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(2,lnI)  PICTURE '9999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,lnCol SAY XTOTAL(2,9)  PICTURE '999999'
          lnCol = lnCol + lnIncr
          @ ROW,118 SAY XTOTAL(2,10) PICTURE '99999999999.99'
        ELSE
          lnCol  = 056
          lnIncr = 7
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(2,lnI)  PICTURE '999999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,lnCol SAY XTOTAL(2,9)  PICTURE '9999999'
          lnCol = lnCol + lnIncr
          @ ROW,119 SAY XTOTAL(2,10) PICTURE '9999999999.99'
        ENDIF
        ROW = ROW+1
      ENDIF
    ENDIF
    
    ** DAMAGED SUB TOTALS **

    IF lcRpFormat = 'A'
      IF (LEN(XREP)=0) .OR. ('D'$XREP)
        @ ROW,000 SAY '  OTHERS.................................'
        IF lcUseDye = 'Y'
          lnCol  = 065
          lnIncr = 5
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(4,lnI)  PICTURE '9999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,lnCol-1 SAY XTOTAL(4,9)  PICTURE '99999999'
          @ ROW,117 SAY XTOTAL(4,10) PICTURE '99999999999.99'
        ELSE
          lnCol  = 054
          lnIncr = 7
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(4,lnI)  PICTURE '999999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,lnCol SAY XTOTAL(4,9)  PICTURE '9999999'
          @ Row,118 SAY XTOTAL(4,10) PICTURE '99999999999.99'
        ENDIF
        ROW = ROW+1
      ENDIF
    ENDIF

    IF lcRpFormat = 'B'
      IF (LEN(XREP)=0) .OR. ('D'$XREP)
        @ ROW,000 SAY '  OTHERS.................................'
        IF lcUseDye = 'Y'
          lnCol  = 069
          lnIncr = 5
          
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(4,lnI)  PICTURE '9999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,lnCol SAY XTOTAL(4,9)  PICTURE '999999'
          @ ROW,118 SAY XTOTAL(4,10) PICTURE '99999999999.99'
        ELSE
          lnCol  = 056
          lnIncr = 7
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(4,lnI)  PICTURE '999999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,lnCol SAY XTOTAL(4,9)  PICTURE '9999999'
          @ ROW,119 SAY XTOTAL(4,10) PICTURE '9999999999.99'
        ENDIF
        ROW = ROW+1
      ENDIF
    ENDIF

    ** CANCELED SUB TOTALS **
    IF lcRpFormat = 'A'
      IF (LEN(XREP)=0) .OR. ('C'$XREP)
        @ ROW,000 SAY '  CANCELED...............................'
        IF lcUseDye = 'Y'
          lnCol  = 065
          lnIncr = 5
          
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(5,lnI)  PICTURE '9999'
            *@ ROW,lnCol SAY XTOTAL(5,lnI)  PICTURE '99999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,104 SAY XTOTAL(5,9)  PICTURE '99999999'
          @ ROW,117 SAY XTOTAL(5,10) PICTURE '99999999999.99'
        ELSE
          lnCol  = 054
          lnIncr = 7
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(5,lnI)  PICTURE '999999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,lnCol SAY XTOTAL(5,9)  PICTURE '9999999'
          lnCol = lnCol + lnIncr
          @ Row,118  SAY XTOTAL(5,10) PICTURE '99999999999.99'
        ENDIF
        ROW = ROW+1
      ENDIF
    ENDIF

    IF lcRpFormat = 'B'
      IF (LEN(XREP)=0) .OR. ('C'$XREP)
        @ ROW,000 SAY '  CANCELED...............................'
        IF lcUseDye = 'Y'
          lnCol  = 069
          lnIncr = 5
          
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(5,lnI)  PICTURE '9999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,lnCol SAY XTOTAL(5,9)  PICTURE '999999'
          lnCol = lnCol + lnIncr
          @ ROW,118 SAY XTOTAL(5,10) PICTURE '99999999999.99'
        ELSE
          lnCol  = 056
          lnIncr = 7
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(5,lnI)  PICTURE '999999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,lnCol SAY XTOTAL(5,9)  PICTURE '9999999'
          lnCol = lnCol + lnIncr
          @ ROW,119 SAY XTOTAL(5,10) PICTURE '9999999999.99'
        ENDIF
        ROW = ROW+1
      ENDIF
    ENDIF

    SELECT &lcOpenLine
    lnRecNo = RECNO()
    GOTO TOP
    SUM  REST Opn1 , Opn2 , Opn3 , Opn4 , Opn5 , Opn6 , Opn7 ,;
         Opn8 , AMOUNT TO lnOpen1 , lnOpen2 , lnOpen3 , lnOpen4   ,;
         lnOpen5 , lnOpen6 , lnOpen7 , lnOpen8 , lnAmount

    *-- Added Qtys to vars. to calculate grand totals.
    lnGrn1   = lnGrn1   + lnOpen1
    lnGrn2   = lnGrn2   + lnOpen2
    lnGrn3   = lnGrn3   + lnOpen3
    lnGrn4   = lnGrn4   + lnOpen4
    lnGrn5   = lnGrn5   + lnOpen5
    lnGrn6   = lnGrn6   + lnOpen6
    lnGrn7   = lnGrn7   + lnOpen7
    lnGrn8   = lnGrn8   + lnOpen8
    lnGrnAmt = lnGrnAmt + lnAmount

    SELECT (WorkFile)
    IF lcRpFormat = 'A'
      @ ROW,000 SAY '  OPEN...................................'
      lnTotal = lnOpen1+lnOpen2+lnOpen3+lnOpen4+lnOpen5+lnOpen6+lnOpen7+lnOpen8
      IF lcUseDye = 'Y'
        lnCol  = 065
        lnIncr = 5
        
        FOR lnCount = 1 To 8
          lcCount = STR(lnCount,1)
          @ ROW,lnCol SAY EVAL('lnOpen'+lcCount) PICTURE '9999'
          lnCol = lnCol + lnIncr
        ENDFOR
        @ ROW,lnCol-1 SAY lnTotal       PICTURE '99999999'        
        @ ROW,117 SAY lnAmount PICTURE '99999999999.99'
      ELSE
        lnCol  = 054
        lnIncr = 7
        FOR lnCount = 1 To 8
          lcCount = STR(lnCount,1)
          @ ROW,lnCol SAY EVAL('lnOpen'+lcCount) PICTURE '999999'
          lnCol = lnCol + lnIncr
        ENDFOR
        @ ROW,lnCol SAY lnTotal       PICTURE '9999999'
        @ ROW,118 SAY lnAmount PICTURE '99999999999.99'
      ENDIF

      ROW = ROW+1
    ENDIF

    IF lcRpFormat = 'B'
      IF  (XTOTAL(1,10)-XTOTAL(2,10)) <> 0
        @ ROW,000 SAY '  OPEN...................................'
        lnTotal = lnOpen1+lnOpen2+lnOpen3+lnOpen4+lnOpen5+lnOpen6+lnOpen7+lnOpen8
        IF lcUseDye = 'Y'
          lnCol  = 069
          lnIncr = 5
          FOR lnCount = 1 To 8
            lcCount = STR(lnCount,1)
            @ ROW,lnCol SAY EVAL('lnOpen'+lcCount) PICTURE '9999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,109 SAY lnTotal   PICTURE '999999'
          @ ROW,118 SAY lnAmount PICTURE '99999999999.99'
        ELSE
          lnCol  = 056
          lnIncr = 7
          FOR lnCount = 1 To 8
            lcCount = STR(lnCount,1)
            @ ROW,lnCol SAY EVAL('lnOpen'+lcCount) PICTURE '999999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,112 SAY lnTotal   PICTURE '9999999'
          @ ROW,119 SAY lnAmount PICTURE '9999999999.99'
        ENDIF
        ROW = ROW+1
      ENDIF
    ENDIF
         
    IF &WorkFile->PO <> lnOldPo .OR. lcRpSortBy = 'S'
      SELECT &lcOpenLine
      USE

      =lfCreatCur()
      
      IF &WorkFile..TRANCD = '1'
        =lfChkAddLn()
      ENDIF
    ENDIF
    SELECT (WorkFile)
    IF lcRpFormat = 'A'
      IF (LEN(XREP)=0) .OR. ('S'$XREP)
        @ ROW,000 SAY '  IN-TRANSIT.............................'
        IF lcUseDye = 'Y'
          lnCol  = 065
          lnIncr = 5
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(3,lnI)  PICTURE '9999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,104 SAY XTOTAL(3,lnI)  PICTURE '99999999'
          @ ROW,117 SAY XTOTAL(3,10) PICTURE '99999999999.99'
        ELSE
          lnCol  = 054
          lnIncr = 7
          FOR lnI = 1 TO 8
            @ ROW,lnCol SAY XTOTAL(3,lnI)  PICTURE '999999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,lnCol SAY XTOTAL(3,9)  PICTURE '9999999'
          lnCol = lnCol + lnIncr
          @ ROW,118 SAY XTOTAL(3,10) PICTURE '99999999999.99'
        ENDIF
        ROW = ROW+1
      ENDIF
    ENDIF

    IF lcRpFormat = 'B'
      IF XTOTAL(3,10) <> 0
        IF (LEN(XREP)=0) .OR. ('S'$XREP)
          @ ROW,000 SAY '  IN-TRANSIT.............................'
          IF lcUseDye = 'Y'
          lnCol  = 069
          lnIncr = 5
            FOR lnI = 1 TO 8
              @ ROW,lnCol SAY XTOTAL(3,lnI)  PICTURE '9999'
              lnCol = lnCol + lnIncr
            ENDFOR
            @ ROW,lnCol SAY XTOTAL(3,9)  PICTURE '99999'
            lnCol = lnCol + lnIncr
            @ ROW,118 SAY XTOTAL(3,10) PICTURE '99999999999.99'
          ELSE
            lnCol  = 056
            lnIncr = 7
            FOR lnI = 1 TO 8
              @ ROW,lnCol SAY XTOTAL(3,lnI)  PICTURE '999999'
              lnCol = lnCol + lnIncr
            ENDFOR
            @ ROW,lnCol SAY XTOTAL(3,9)  PICTURE '9999999'
            lnCol = lnCol + lnIncr
            @ row,122 SAY XTOTAL(3,10) PICTURE '9999999.99'
          ENDIF
          ROW = ROW+1
        ENDIF
      ENDIF
    ENDIF

    @ ROW,00 SAY REPLICATE('=',132)
    ROW = ROW+1
    X = 1
    DO WHILE X<=10
      XTOTAL1(1,X) = XTOTAL1(1,X) + XTOTAL(1,X)
      XTOTAL1(2,X) = XTOTAL1(2,X) + XTOTAL(2,X)
      XTOTAL1(3,X) = XTOTAL1(3,X) + XTOTAL(3,X)

      XTOTAL1(4,X) = XTOTAL1(4,X) + XTOTAL(4,X)
      XTOTAL1(5,X) = XTOTAL1(5,X) + XTOTAL(5,X)

      XTOTAL(1,X) = 0.00
      XTOTAL(2,X) = 0.00
      XTOTAL(3,X) = 0.00

      XTOTAL(4,X) = 0.00
      XTOTAL(5,X) = 0.00

      X = X+1
    ENDDO

    HBREAK = &BREAK
    PTRANCD = SPACE(1)
    EXIT
  ENDDO

  *--------------------- END SUBTOTALS ----------------------------

  IF EOF()
    EXIT
  ENDIF

  IF ROW >=53
    ROW = 99
    LOOP
  ENDIF

  SELECT (WorkFile)

  IF EMPTY(TRANCD)
    SKIP
    LOOP
  ENDIF

  lnPrnPrice = lfCalAmnts()

  DO WHILE lcRpSumDet='D'
    IF EMPTY(XF) .OR. TRANCD $ XF

      XPO=PO
      ROW=ROW+1

      IF (TRANCD='1') .AND. (TRANCD<>PTRANCD) .AND. ((LEN(XREP)=0) .OR. ('O'$XREP))
        PTRANCD=TRANCD
        IF lcRpFormat = 'A'
          @ ROW,000 SAY '--------------------------------------------------------- ORDERED LINES ------------------------------------------------------------'
          ROW=ROW+1
        ENDIF
        LNTYPE = 'BUD'
      ENDIF

      IF (TRANCD='2') .AND. (TRANCD<>PTRANCD) .AND. ((LEN(XREP)=0) .OR. ('R'$XREP))
        PTRANCD=TRANCD
        IF lcRpFormat = 'A'
          @ ROW,000 SAY '--------------------------------------------------------- RECEIVED LINES -----------------------------------------------------------'
          ROW=ROW+1
        ENDIF
        LNTYPE = 'REC'
      ENDIF
      IF (TRANCD=lcShpTrnCd) .AND. (TRANCD<>PTRANCD) .AND. ((LEN(XREP)=0) .OR. ('S'$XREP))       
        PTRANCD=TRANCD
        IF lcRpFormat = 'A'
          @ ROW,000 SAY '-------------------------------------------------------- IN-TRANSIT LINES ----------------------------------------------------------'
          ROW=ROW+1
        ENDIF
        LNTYPE = 'TRA'
      ENDIF

      IF (TRANCD='4') .AND. (TRANCD<>PTRANCD) .AND. ((LEN(XREP)=0) .OR. ('D'$XREP)) && DAMAGED QUANTITIES
        PTRANCD=TRANCD
        IF lcRpFormat = 'A'
          @ ROW,000 SAY '--------------------------------------------------------- OTHERS  LINES ------------------------------------------------------------'
          ROW=ROW+1
        ENDIF
        LNTYPE = 'DAM'
      ENDIF

      IF (TRANCD='5') .AND. (TRANCD<>PTRANCD) .AND. ((LEN(XREP)=0) .OR. ('C'$XREP))
        PTRANCD=TRANCD
        IF lcRpFormat = 'A'
          @ ROW,000 SAY '--------------------------------------------------------- CANCELED LINES -----------------------------------------------------------'
          ROW=ROW+1
        ENDIF
        LNTYPE = 'CAN'
      ENDIF
      
      IF llNewPg .OR. (!EOF('SCALE') .AND. lcScale # SCALE.SCALE)
        llNewPg = .F.
        @ ROW,025 SAY 'Scale       :' + SCALE.cscl_desc
        IF lcUseDye = 'Y'
          @ ROW,IIF(lcRpFormat = 'B',69,65) SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ1,1,4)),4)+' '+PADL(ALLTRIM(SUBSTR(SCALE.SZ2,1,4)),4)+' '+;
                    PADL(ALLTRIM(SUBSTR(SCALE.SZ3,1,4)),4)+' '+PADL(ALLTRIM(SUBSTR(SCALE.SZ4,1,4)),4)+' '+;
                    PADL(ALLTRIM(SUBSTR(SCALE.SZ5,1,4)),4)+' '+PADL(ALLTRIM(SUBSTR(SCALE.SZ6,1,4)),4)+' '+;
                    PADL(ALLTRIM(SUBSTR(SCALE.SZ7,1,4)),4)+' '+PADL(ALLTRIM(SUBSTR(SCALE.SZ8,1,4)),4)
        ELSE
          @ ROW,IIF(lcRpFormat = 'B',57,55) SAY PADL(ALLTRIM(SCALE.SZ1),5)+'  '+PADL(ALLTRIM(SCALE.SZ2),5)+'  '+;
                    PADL(ALLTRIM(SCALE.SZ3),5)+'  '+PADL(ALLTRIM(SCALE.SZ4),5)+'  '+;
                    PADL(ALLTRIM(SCALE.SZ5),5)+'  '+PADL(ALLTRIM(SCALE.SZ6),5)+'  '+;
                    PADL(ALLTRIM(SCALE.SZ7),5)+'  '+PADL(ALLTRIM(SCALE.SZ8),5)
        ENDIF
        lcScale = SCALE.SCALE
        ROW = ROW + 1        
      ENDIF
      @ ROW,000 SAY PO
      @ ROW,007 SAY POSHDR->VENDOR

      IF (TRANCD='2') .OR. (TRANCD=lcShpTrnCd) .OR. (TRANCD='4') .OR. (TRANCD='5')
        @ ROW,016 SAY IIF(TRANCD = '3', ShpmtHdr.ETA , DATE)
        @ ROW,025 SAY SHIPNO
      ENDIF

      IF TRANCD='1'
        @ ROW,016 SAY POSHDR->Available
      ENDIF

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*PO#    VENDOR   E.T.A.   SHPMT# S STYLE               DYELOT     TYP QTY1 QTY2 QTY3 QTY4 QTY5 QTY6 QTY7 QTY8 TOTQTY $PRICE     AMOUNT
      @ ROW,032 SAY POSHDR->STATUS
      @ ROW,034 SAY STYLE
      IF lcUseDye = 'Y'
        @ ROW,054 SAY DYELOT
      ENDIF
      
      IF lcRpFormat = 'A'
        IF lcUseDye = 'Y'
          lnCol  = 065
          lnIncr = 5
          FOR lnCount = 1 To 8
            lcCount = STR(lnCount,1)
            @ ROW,lnCol SAY EVAL('Qty'+lcCount) PICTURE '9999'
            lnCol = lnCol + lnIncr
          ENDFOR
           @ ROW,lnCol+1 SAY TOTQTY PICTURE '999999'
         ELSE
          lnCol  = 054
          lnIncr = 7
          FOR lnCount = 1 To 8
            lcCount = STR(lnCount,1)
            @ ROW,lnCol SAY EVAL('Qty'+lcCount) PICTURE '999999'
            lnCol = lnCol + lnIncr
          ENDFOR
          lnCol = lnCol- lnIncr
          @ ROW,lnCol SAY TOTQTY PICTURE '99999999'
        ENDIF
        @ ROW,IIF(lcUseDye='Y',114,112) SAY lnPrnPrice PICTURE IIF(llRpPrtDec,'999.99','999999')
        @ ROW,IIF(lcUseDye='Y',122,120) SAY TOTQTY*lnPrnPrice PICTURE IIF(llRpPrtDec,'999999.99','999999999')
      ENDIF

      IF lcRpFormat = 'B'
        IF lcUseDye = 'Y'
          lnCol  = 069
          lnIncr = 5
          @ ROW,063 SAY LNTYPE
          FOR lnCount = 1 To 8
            lcCount = STR(lnCount,1)
            @ ROW,lnCol SAY EVAL('Qty'+lcCount) PICTURE '9999'
            lnCol = lnCol + lnIncr
          ENDFOR
          @ ROW,lnCol SAY TOTQTY PICTURE  '999999'
        ELSE
          lnCol  = 056
          lnIncr = 7
          @ ROW,053 SAY LNTYPE
          FOR lnCount = 1 To 8
            lcCount = STR(lnCount,1)
            @ ROW,lnCol SAY EVAL('Qty'+lcCount) PICTURE '999999'
            lnCol = lnCol + lnIncr
          ENDFOR
          lnCol = lnCol- lnIncr

           @ ROW,lnCol SAY TOTQTY PICTURE '99999999'
        ENDIF
        IF lcUseDye = 'Y'
          @ ROW,116 SAY lnPrnPrice PICTURE IIF(llRpPrtDec,'999.99','999999')
        ELSE
          @ ROW,118 SAY lnPrnPrice PICTURE IIF(llRpPrtDec,'99.99','99999')
        ENDIF
        IF lcUseDye = 'Y'
          @ ROW,122 SAY TOTQTY*lnPrnPrice PICTURE IIF(llRpPrtDec,'999999.99','9999999999')
        ELSE
          @ ROW,124 SAY TOTQTY*lnPrnPrice PICTURE IIF(llRpPrtDec,'99999.99','99999999')
        ENDIF
      ENDIF
    ENDIF
    EXIT
  ENDDO

  DO CASE
    CASE TRANCD='1'
      XTRNCD=1
    CASE TRANCD='2'
      XTRNCD=2

    *--E301108  (Start)
    *CASE TRANCD='3'
    CASE TRANCD=lcShpTrnCd
      XTRNCD=3
    *--E301108  (End)

    CASE TRANCD='4'
      XTRNCD=4
    CASE TRANCD='5'
      XTRNCD=5
  ENDCASE

  XTOTAL(XTRNCD,1)=XTOTAL(XTRNCD,1)+QTY1
  XTOTAL(XTRNCD,2)=XTOTAL(XTRNCD,2)+QTY2
  XTOTAL(XTRNCD,3)=XTOTAL(XTRNCD,3)+QTY3
  XTOTAL(XTRNCD,4)=XTOTAL(XTRNCD,4)+QTY4
  XTOTAL(XTRNCD,5)=XTOTAL(XTRNCD,5)+QTY5
  XTOTAL(XTRNCD,6)=XTOTAL(XTRNCD,6)+QTY6
  XTOTAL(XTRNCD,7)=XTOTAL(XTRNCD,7)+QTY7
  XTOTAL(XTRNCD,8)=XTOTAL(XTRNCD,8)+QTY8
  XTOTAL(XTRNCD,9)=XTOTAL(XTRNCD,9)+TOTQTY
  XTOTAL(XTRNCD,10)=XTOTAL(XTRNCD,10)+(TOTQTY*lnPrnPrice)
  
  =SEEK(PO+STYLE+STR(lineNo,6),lcOpenLine)
  SELECT &lcOpenLine
  IF &WorkFile->TRANCD = '1'
    REPLACE OPN1 WITH OPN1 + &WorkFile->Qty1 ;
            OPN2 WITH OPN2 + &WorkFile->Qty2 ;
            OPN3 WITH OPN3 + &WorkFile->Qty3 ;
            OPN4 WITH OPN4 + &WorkFile->Qty4 ;
            OPN5 WITH OPN5 + &WorkFile->Qty5 ;
            OPN6 WITH OPN6 + &WorkFile->Qty6 ;
            OPN7 WITH OPN7 + &WorkFile->Qty7 ;
            OPN8 WITH OPN8 + &WorkFile->Qty8
  ELSE
    REPLACE OPN1 WITH MAX(OPN1 - &WorkFile->Qty1,0) ;
            OPN2 WITH MAX(OPN2 - &WorkFile->Qty2,0) ;
            OPN3 WITH MAX(OPN3 - &WorkFile->Qty3,0) ;
            OPN4 WITH MAX(OPN4 - &WorkFile->Qty4,0) ;
            OPN5 WITH MAX(OPN5 - &WorkFile->Qty5,0) ;
            OPN6 WITH MAX(OPN6 - &WorkFile->Qty6,0) ;
            OPN7 WITH MAX(OPN7 - &WorkFile->Qty7,0) ;
            OPN8 WITH MAX(OPN8 - &WorkFile->Qty8,0)
  ENDIF
  lnTot = Opn1+Opn2+Opn3+Opn4+Opn5+Opn6+Opn7+Opn8
  

  REPLACE PRICE   WITH lnPrnPrice ;
          TotQty  WITH lnTot      ;
          AMOUNT  WITH lnTot * PRICE


  SELECT (WorkFile)
  SKIP
ENDDO
*------------------ END MAIN REPORT LOOP --------------------

X = 2
IF LEN(TRIM(BREAK)) =0
  X =1
ENDIF

ROW=ROW+2

IF ROW>53
  PAGENO = PAGENO+1
  DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
  IF lcRpFormat = 'A'
     @ 07,00 SAY '                                                                  QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7 QTY8 TOTQTY       AMOUNT'
  ELSE
     @ 07,00 SAY '                                                              QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8  TOTQTY          AMOUNT'
  ENDIF
  @ 08,00 SAY REPLICATE('=',132)
  ROW = 09
ENDIF

@ ROW,00 SAY REPLICATE('*',132)
ROW = ROW+1

@ ROW,000 SAY '* GRAND TOTAL *'
ROW=ROW+1

IF lcRpFormat = 'A'
  IF (LEN(XREP))=0 .OR. ('O'$XREP)
    @ ROW,000 SAY '  ORDERED................................'
    IF lcUseDye = 'Y'
      lnCol  = 065
      lnIncr = 5
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(1,lnI)  PICTURE '9999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,lnCol SAY XTOTAL1(1,9)  PICTURE '9999999'
    ELSE
      lnCol  = 054
      lnIncr = 7
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(1,lnI)  PICTURE '999999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,110 SAY XTOTAL1(1,9)  PICTURE '9999999'      
    ENDIF
    IF llPrintTot
      IF lcUseDye = 'Y'
        @ ROW,120 SAY XTOTAL1(1,10) PICTURE '99999999.99'
      ELSE
        @ ROW,121 SAY XTOTAL1(1,10) PICTURE '99999999.99'
      ENDIF
    ENDIF  
    ROW = ROW+1
  ENDIF
ENDIF

IF lcRpFormat = 'B'
  IF (LEN(XREP)=0) .OR. ('O'$XREP)
    @ ROW,000 SAY '  ORDERED................................'
    IF lcUseDye = 'Y'
      lnCol  = 069
      lnIncr = 5  
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(1,lnI)  PICTURE '9999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,lnCol SAY XTOTAL1(1,9)  PICTURE '999999'
      lnCol = lnCol + lnIncr
    ELSE
      lnCol  = 056      
      lnIncr = 7
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(1,lnI)  PICTURE '999999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,112 SAY XTOTAL1(1,9)  PICTURE '9999999'
    ENDIF

    IF llPrintTot
      @ row,121 SAY XTOTAL1(1,10) PICTURE '99999999.99'
    ENDIF  
    ROW = ROW+1
  ENDIF
ENDIF

IF lcRpFormat = 'A'
  IF (LEN(XREP))=0 .OR. ('R'$XREP)
    @ ROW,000 SAY '  RECEIVED...............................'
    IF lcUseDye = 'Y'
      lnCol  = 065
      lnIncr = 5
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(2,lnI)  PICTURE '9999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,lnCol SAY XTOTAL1(2,9)  PICTURE '9999999'
    ELSE
      lnCol  = 054
      lnIncr = 7
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(2,lnI)  PICTURE '999999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,lnCol SAY XTOTAL1(2,9)  PICTURE '9999999'
    ENDIF

    IF llPrintTot
      IF lcUseDye = 'Y'
        @ ROW,120 SAY XTOTAL1(2,10) PICTURE '99999999.99'
      ELSE
        @ ROW,121 SAY XTOTAL1(2,10) PICTURE '99999999.99'
      ENDIF
    ENDIF
    ROW = ROW+1
  ENDIF
ENDIF

IF lcRpFormat = 'B'
  IF (LEN(XREP)=0) .OR. ('R'$XREP)
    @ ROW,000 SAY '  RECEIVED...............................'
    IF lcUseDye = 'Y'
      lnCol  = 069
      lnIncr = 5  
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(2,lnI)  PICTURE '9999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,lnCol SAY XTOTAL1(2,9)  PICTURE '999999'
    ELSE
      lnCol  = 056
      lnIncr = 7
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(2,lnI)  PICTURE '999999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,lnCol SAY XTOTAL1(2,9)  PICTURE '9999999'
    ENDIF
    IF llPrintTot
      @ ROW,121 SAY XTOTAL1(2,10) PICTURE '99999999.99'
    ENDIF  

    ROW = ROW+1
  ENDIF
ENDIF

*** DAMAGED GRAND TOTALS ***
IF lcRpFormat = 'A'
  IF (LEN(XREP)=0) .OR. ('D'$XREP)
    @ ROW,000 SAY '  OTHERS.................................'
    IF lcUseDye = 'Y'
      lnCol  = 065
      lnIncr = 5
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(4,lnI)  PICTURE '9999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,lnCol SAY XTOTAL1(4,9)  PICTURE '9999999'
    ELSE
      lnCol  = 054
      lnIncr = 7
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(4,lnI)  PICTURE '999999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,lnCol SAY XTOTAL1(4,9)  PICTURE '9999999'
    ENDIF
    IF llPrintTot
      IF lcUseDye = 'Y'
        @ ROW,120 SAY XTOTAL1(4,10) PICTURE '99999999.99'
      ELSE
        @ row,121 SAY XTOTAL1(4,10) PICTURE '99999999.99'
      ENDIF
    ENDIF  

    ROW = ROW+1
  ENDIF
ENDIF

IF lcRpFormat = 'B'
  IF (LEN(XREP)=0) .OR. ('D'$XREP)
    @ ROW,000 SAY '  OTHERS.................................'
    IF lcUseDye = 'Y'
      lnCol  = 069
      lnIncr = 5  
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(4,lnI)  PICTURE '9999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,lnCol SAY XTOTAL1(4,9)  PICTURE '999999'
    ELSE
      lnCol  = 056      
      lnIncr = 7
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(4,lnI)  PICTURE '999999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,lnCol SAY XTOTAL1(4,9)  PICTURE '9999999'
    ENDIF

    IF llPrintTot
      IF lcUseDye = 'Y'
        @ ROW,123 SAY XTOTAL1(4,10) PICTURE '999999.99'
      ELSE
        @ row,122 SAY XTOTAL1(4,10) PICTURE '9999999.99'
      ENDIF
    ENDIF  

    ROW = ROW+1
  ENDIF
ENDIF

*** CANCELED GRAND TOTALS
IF lcRpFormat = 'A'
  IF (LEN(XREP)=0) .OR. ('C'$XREP)
    @ ROW,000 SAY '  CANCELED...............................'
    IF lcUseDye = 'Y'
      lnCol  = 065
      lnIncr = 5
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(5,lnI)  PICTURE '9999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,lnCol SAY XTOTAL1(5,9)  PICTURE '9999999'
    ELSE
      lnCol  = 054
      lnIncr = 7
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(5,lnI)  PICTURE '999999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,lnCol SAY XTOTAL1(5,9)  PICTURE '9999999'
    ENDIF
    IF llPrintTot
      IF lcUseDye = 'Y'
        @ ROW,120 SAY XTOTAL1(5,10) PICTURE '99999999.99' && ahmed
      ELSE
        @ row,121 SAY XTOTAL1(5,10) PICTURE '99999999.99' && ahmed
      ENDIF
    ENDIF  

    ROW = ROW+1
  ENDIF
ENDIF

IF lcRpFormat = 'B'
  IF (LEN(XREP)=0) .OR. ('C'$XREP)
    @ ROW,000 SAY '  CANCELED...............................'
    IF lcUseDye = 'Y'
      lnCol  = 069
      lnIncr = 5  
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(5,lnI)  PICTURE '9999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,lnCol SAY XTOTAL1(5,9)  PICTURE '999999'
    ELSE
      lnCol  = 056      
      lnIncr = 7
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(5,lnI)  PICTURE '999999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,lnCol SAY XTOTAL1(5,9)  PICTURE '9999999'
    ENDIF
    IF llPrintTot
      IF lcUseDye = 'Y'
        @ ROW,123 SAY XTOTAL1(5,10) PICTURE '999999.99' && ahmed
      ELSE
        @ row,122 SAY XTOTAL1(5,10) PICTURE '9999999.99' && ahmed
      ENDIF
    ENDIF  

    ROW = ROW+1
  ENDIF
ENDIF

IF lcRpFormat = 'A'
  @ ROW,000 SAY '  OPEN...................................'

  lnTotal = lnGrn1+lnGrn2+lnGrn3+lnGrn4+lnGrn5+lnGrn6+lnGrn7+lnGrn8   
  IF lcUseDye = 'Y'
    lnCol  = 065
    lnIncr = 5
    FOR lnCount = 1 To 8
      lcCount = STR(lnCount,1)
      @ ROW,lnCol SAY EVAL('lnGrn'+lcCount) PICTURE '9999'
      lnCol = lnCol + lnIncr
    ENDFOR
    @ ROW,105 SAY lnTotal   PICTURE '9999999'
  ELSE
    lnCol  = 054
    lnIncr = 7
    FOR lnCount = 1 To 8
      lcCount = STR(lnCount,1)
      @ ROW,lnCol SAY EVAL('lnGrn'+lcCount) PICTURE '999999'
      lnCol = lnCol + lnIncr
    ENDFOR
    @ ROW,lnCol SAY lnTotal   PICTURE '9999999'
  ENDIF

  IF llPrintTot
 
    IF lcUseDye = 'Y'
      @ ROW,120 SAY lnGrnAmt    PICTURE '99999999.99' && ahmed
    ELSE
      @ row,121 SAY lnGrnAmt    PICTURE '99999999.99' && ahmed
    ENDIF
  ENDIF  

  ROW = ROW+1
ENDIF

IF lcRpFormat = 'B'
  IF (XTOTAL1(1,10)-XTOTAL1(2,10)) <> 0
    @ ROW,000 SAY '  OPEN...................................'
    lnTotal = lnGrn1+lnGrn2+lnGrn3+lnGrn4+lnGrn5+lnGrn6+lnGrn7+lnGrn8
  IF lcUseDye = 'Y'
    lnCol  = 069
    lnIncr = 5      
    FOR lnCount = 1 To 8
      lcCount = STR(lnCount,1)
      @ ROW,lnCol SAY EVAL('lnGrn'+lcCount) PICTURE '9999'
      lnCol = lnCol + lnIncr
    ENDFOR
     @ ROW,lnCol SAY lnTotal   PICTURE '999999'
  ELSE
    lnCol  = 056    
    lnIncr = 7
    FOR lnCount = 1 To 8
      lcCount = STR(lnCount,1)
      @ ROW,lnCol SAY EVAL('lnGrn'+lcCount) PICTURE '999999'
      lnCol = lnCol + lnIncr
    ENDFOR
    @ ROW,lnCol SAY lnTotal   PICTURE '9999999'
  ENDIF


  IF llPrintTot
    IF lcUseDye = 'Y'
      @ ROW,121 SAY lnGrnAmt    PICTURE '99999999.99' && ahmed
    ELSE
      @ row,121 SAY lnGrnAmt    PICTURE '99999999.99' && ahmed
    ENDIF
  ENDIF
  
    ROW = ROW+1
  ENDIF
ENDIF

IF lcRpFormat = 'A'
  IF (LEN(XREP))=0 .OR. ('S'$XREP)
    @ ROW,000 SAY '  IN-TRANSIT.............................'
    IF lcUseDye = 'Y'
      lnCol  = 065
      lnIncr = 5
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(3,lnI)  PICTURE '9999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,lnCol SAY XTOTAL1(3,9)  PICTURE '9999999'
      lnCol = lnCol + lnIncr
    ELSE
      lnCol  = 054
      lnIncr = 7
      FOR lnI = 1 TO 8
        @ ROW,lnCol SAY XTOTAL1(3,lnI)  PICTURE '999999'
        lnCol = lnCol + lnIncr
      ENDFOR
      @ ROW,111 SAY XTOTAL1(3,9)  PICTURE '999999'
    ENDIF
    IF llPrintTot
      IF lcUseDye = 'Y'
        @ ROW,120 SAY XTOTAL1(3,10) PICTURE '99999999.99'
      ELSE
        @ row,121 SAY XTOTAL1(3,10) PICTURE '99999999.99'
      ENDIF
    ENDIF  

    ROW = ROW+1
  ENDIF
ENDIF

IF lcRpFormat = 'B'
  IF XTOTAL1(3,10) <> 0
    IF (LEN(XREP)=0) .OR. ('S'$XREP)
      @ ROW,000 SAY '  IN-TRANSIT.............................'
      IF lcUseDye = 'Y'
        lnCol  = 069
        lnIncr = 5
        FOR lnI = 1 TO 8
          @ ROW,lnCol SAY XTOTAL1(3,lnI)  PICTURE '9999'
          lnCol = lnCol + lnIncr
        ENDFOR
        @ ROW,lnCol SAY XTOTAL1(3,9)  PICTURE '999999'
      ELSE
        lnCol  = 056        
        lnIncr = 7
        FOR lnI = 1 TO 8
          @ ROW,lnCol SAY XTOTAL1(3,lnI)  PICTURE '999999'
          lnCol = lnCol + lnIncr
        ENDFOR
        @ ROW,lnCol SAY XTOTAL1(3,9)  PICTURE '9999999'
      ENDIF
      IF llPrintTot
        IF lcUseDye = 'Y'
          @ ROW,123 SAY XTOTAL1(3,10) PICTURE '999999.99'
        ELSE
          @ row,122 SAY XTOTAL1(3,10) PICTURE '9999999.99'
        ENDIF
      ENDIF  
      ROW = ROW+1
    ENDIF
  ENDIF
ENDIF

@ ROW,00 SAY REPLICATE('*',132)
*---------------------------
*   END lpPrint 
*---------------------------
*:*************************************************************
*: Name      : lfMajInfGet
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : To get the title and picture of style major segement 
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfMajInfGet()
*:*************************************************************
*:
FUNCTION lfMajGet

lcMajPic = gfItemMask("PM")
lnMajPic = LEN(lcMajPic)
lcMajPic = "@! " + lcMajPic
lcMajTtl = gfItemMask("HM")

*:*************************************************************
*: Name      : lfvStyle
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : validate style
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvStyle()
*:*************************************************************

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

*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : Option Grid When function
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwRepWhen()
*:*************************************************************

FUNCTION lfwRepWhen

IF llMultCurr
  lnCurrPos  = lfItmPos('POSHDR.CPRICECUR')
  =lfvPrice()
ELSE
  lcRpCurr = "O"
ENDIF  

= lfvSumDet()
*-- end of lfwRepWhen.

*:*************************************************************
*: Name      : lfvSumDet
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : Summary/Detail Validation
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : lfOGShowGet
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvSumDet()
*:*************************************************************
FUNCTION lfvSumDet
lnPrnNotePo = ASCAN(laOGObjType,'lcRPNote')
IF lnPrnNotePo > 0
  lnPrnNotePo = ASUBSCRIPT(laOGObjType,lnPrnNotePo,1)
  laOGObjCnt[lnPrnNotePo] = (lcRpSumDet = "D") AND (lcRpSortBy = "P")
  IF (lcRpSumDet = 'S') OR (lcRpSortBy <> "P")
    lcRPNote = 'N'
  ENDIF  
ENDIF
= lfOGShowGet('lcRPNote')

*-- end of lfvSumDet.
*:*************************************************************
*: Name      : lfvVendor
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : validate vendor
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvVendor()
*:*************************************************************

FUNCTION lfvVendor

DO CASE
  CASE lcRpPoType = 'P'
    lcVenFld = VARREAD()
    lcVendor = EVAL(lcVenFld)
    SELECT APVENDOR
    SET ORDER TO TAG VenCode 
    IF !EMPTY(lcVendor) .AND. ('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
      =gfApVnBrow(@lcVendor)
    ENDIF
    &lcVenFld = lcVendor 

  CASE lcRpPoType = 'N'
    = lfvWareHo()
ENDCASE    

*:*************************************************************
*: Name      : lfvWareHo
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : Validate warehouse
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvWareHo()
*:*************************************************************

FUNCTION lfvWareHo

lcWareHo = VARREAD()

lcTag = ORDER('WAREHOUS')

SET ORDER TO WAREHOUS IN WAREHOUS

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcWareHo.,'WAREHOUS') 
    &lcWareHo = WAREHOUS.cWareCode
  ELSE
    &lcWareHo = gfBrowWare(.T.)
  ENDIF
ELSE
  &lcWareHo = ''
ENDIF

SET ORDER TO WAREHOUS IN WAREHOUS

*:*************************************************************
*: Name      : lfvPO
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : validate purchase order
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvPO()
*:*************************************************************

FUNCTION lfvPO

lcPOFld = VARREAD()

lcPONo = &lcPOFld

IF !EMPTY(lcPONo) .AND. ('?' $ lcPONo .OR. !SEEK(lcRpPoType+lcPONo , 'POSHDR'))
  DO POSBrow WITH lcPONo,"",lcRpPoType
ENDIF

&lcPOFld = lcPONo

*:***************************************************************************

FUNCTION lfType
PARAMETERS lcType

DO CASE 
  CASE lcType = '1'
    lcReturn = 'O'
  CASE lcType = '2'
    lcReturn = 'R'

  CASE lcType = lcShpTrnCd
    lcReturn = 'S'
  CASE lcType = '4'
    lcReturn = 'D'
  CASE lcType = '5'
    lcReturn = 'C'
ENDCASE  

RETURN lcReturn

*:*************************************************************
*: Name      : lfPrnPrice
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : print price
*:*************************************************************
*: Called from : ....
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : lfPrnPrice()
*:*************************************************************

FUNCTION lfPrnPrice
PRIVATE lnPrice,ldStart,ldEnd,lnDscPrcnt
*IF lcRpPoType = 'N'
  IF EMPTY(Style.cDiscCode)
    *C121153,1 ASH 01/11/2004 (Begin) Get price from Style.PriceA.
    lnPrice = Style.PriceA
    *C121153,1 ASH 01/11/2004 (End)
  ELSE
    STORE {} TO ldStart,ldEnd
    STORE 0 TO lnDscPrcnt
    DIMENSION laDiscRng[3,2]
    laDiscRng[1,1] = "START"
    laDiscRng[1,2] = "ldStart"
    laDiscRng[2,1] = "DENDATE"
    laDiscRng[2,2] = "ldEnd"
    laDiscRng[3,1] = "DISCPCNT"
    laDiscRng[3,2] = "lnDscPrcnt"
    *-- Fill the related GL information from the codes file.
    =gfRltFld(Style.cDiscCode, @laDiscRng, "CDISCCODE")
    IF BETWEEN(PoSHdr.Entered,ldStart,ldEnd)
      *C121153,1 ASH 01/11/2004 (Begin) Get price from Style.PriceA.
      *lnPrice = Style.nSugRetPri - (Style.nSugRetPri*lnDscPrcnt/100)
      lnPrice = Style.PriceA - (Style.PriceA*lnDscPrcnt/100)
      *C121153,1 ASH 01/11/2004 (End)
    ELSE
      *lnPrice = Style.nSugRetPri
      lnPrice = Style.PriceA
    ENDIF
  ENDIF
*ELSE
*  lnPrice = Style.nSugRetPri
*ENDIF
RETURN lnPrice

*:*************************************************************
*: Name      : lfCreatCur
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : Create Temporary cursor has open quantities
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None
*:*************************************************************
*: Example   : =lfCreatCur()
*:*************************************************************
*:
FUNCTION lfCreatCur
CREATE CURSOR (lcOpenLine) (PO C(6) , STYLE C(19), cLineNo C(6)         ,;
   OPN1 N (7) , OPN2 N (7) , OPN3 N (7), OPN4 N (7), OPN5 N (7)     ,;
   OPN6 N (7), OPN7 N (7), OPN8 N (7), TOTQTY N(9),PRICE N(9,4) , AMOUNT N(11,4))

SELECT (lcOpenLine)
ZAP
INDEX ON PO+STYLE+cLineNo TAG &lcOpenLine OF (gcWorkDir+lcOpenLine+'.CDX')

*-- end of lfCreatCur.
*:*************************************************************
*: Name      : lfChkAddLn
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : Add new line to temporary open quantites cursor.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None
*:*************************************************************
*: Example   : =lfChkAddLn()
*:*************************************************************
*:
FUNCTION lfChkAddLn

PRIVATE lnAlias
lnAlias = SELECT()
SELECT (WorkFile)

IF !SEEK(PO+STYLE+STR(lineNo,6),lcOpenLine)
  m.cLineNo = STR(lineNo,6)
  m.Po      = PO
  m.Style   = STYLE
  INSERT INTO (lcOpenLine) FROM MEMVAR
ENDIF

SELECT (lnAlias)

*-- end of lfChkAddLn.
*:*************************************************************
*: Name      : lfvCurDisp
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : Activate currency display screen to get user 
*:           : selection for currencies.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None
*:*************************************************************
*: Example   : =lfvCurDisp()
*:*************************************************************
*:

FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*-- end of lfvCurDisp.
*:*************************************************************
*: Name      : lfCurrDesc
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : Currency description if sort by currency.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : Currency description.
*:*************************************************************
*: Example   : =lfCurrDesc()
*:*************************************************************
*:
FUNCTION lfCurrDesc
PRIVATE lcCurrVal , lcCurDesc
lcCurDesc = ''
lcCurrVal  = ALLTRIM(cPriceCur)
lnCurVlPos = ASCAN(laCurrVal,lcCurrVal)
IF lnCurVlPos > 0
  lcCurDesc  = laCurrDesc[lnCurVlPos,1]
ENDIF  
RETURN PADR(ALLTRIM(lcCurDesc),18)
*-- end of lfCurrDesc.

*:*************************************************************
*: Name      : lfItmPos
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : Evaluate fixed filter position within array.
*:*************************************************************
*: Passed Parameters  : ...
*:*************************************************************
*: Returns            : Position
*:*************************************************************
*: Example   : = lfItmPos()
*:*************************************************************
*:
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- end of lfItmPos.
*:*************************************************************
*: Name      : lfFillVars
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : Fill most of report memory variables.
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfFillVars()
*:*************************************************************
*:
FUNCTION lfFillVars

DIMENSION laCost [5,1] , laSetUps[7,2]

laCost = SPACE(9)

IF !USED('SYCCOMP')
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF  

laSetUps[1,1]   = 'M_CITYPE1'
laSetUps[2,1]   = 'M_CITYPE2'
laSetUps[3,1]   = 'M_CITYPE3'
laSetUps[4,1]   = 'M_CITYPE4'
laSetUps[5,1]   = 'M_CITYPE5'

laSetUps[6,1]  = 'llMulCurr'

laSetUps[7,1]  = 'M_DYELOT'
= gfGetMemVar(@laSetups)

laCost[1,1]  = ALLTRIM(laSetUps[1,2])
laCost[2,1]  = ALLTRIM(laSetUps[2,2])
laCost[3,1]  = ALLTRIM(laSetUps[3,2])
laCost[4,1]  = ALLTRIM(laSetUps[4,2])
laCost[5,1]  = ALLTRIM(laSetUps[5,2])

llMultCurr = laSetUps[6,2]


lcUseDye = laSetUps[7,2]
IF llMultCurr
  *-- Open international file.
  IF !USED("SYCINT")
    USE (gcSysHome+"SYCINT.DBF") IN 0 
    llOpenInt = .T.
  ENDIF

  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (gcSysHome+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF  

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]
ENDIF
*-- end of lfFillVars.

*:*************************************************************
*: Name      : lfClearRep
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : Function that we call when Close the option grid.
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfClearRep()
*:*************************************************************
*:
FUNCTION lfClearRep
IF llOpenComp AND USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF

IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign

  IF llOpenInt AND USED("SYCINT")
    USE IN SYCINT 
  ENDIF

  IF llOpenCurr AND USED("SYCCURR")
    USE IN SYCCURR
  ENDIF

  IF llOpenExch AND USED("SYCEXCH")
    USE IN SYCEXCH
  ENDIF  
ENDIF
*-- end of lfClearRep.

*:*************************************************************
*: Name      : lfCalAmnts
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : Calculte amounts in base currency to be printed.
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCalAmnts()
*:*************************************************************
*:
FUNCTION lfCalAmnts
PRIVATE lnRetrnVal
lnRetrnVal = 0
*-- If Print in forign currency.
*C121153,1 ASH 01/11/2004 (Begin) Get the cost from style.TotCost.
*Comments the following lines to print style cost instead of PO cost
*IF lcRpCurr = "F"
*  lnRetrnVal = nCost1
*ELSE
  *-- If Print in original base currency.
*  IF lcRpCurr = "O"
*    lnRetrnVal = IIF(lcRpPrice='R',lfPrnPrice(),nECost1)
*  ELSE  && print By Date or calculated now.
*    IF (cCurrCode = gcBaseCurr) OR (nCost1 = 0)
*      lnRetrnVal = nCost1
*    ELSE
*      lnRetrnVal = gfAmntDisp(nCost1,lcRpCurr,ldRpExDate,lcRpTmpNam)
*    ENDIF  
*  ENDIF
*ENDIF

IF lcRpCurr = "F"
  lnRetrnVal = Style.TotCost
ELSE
  *-- If Print in original base currency.
  IF lcRpCurr = "O"
    lnRetrnVal = IIF(lcRpPrice='R',lfPrnPrice(),Style.TotCost)
  ELSE  && print By Date or calculated now.
    IF (cCurrCode = gcBaseCurr) OR (Style.TotCost = 0)
      lnRetrnVal = Style.TotCost
    ELSE
      lnRetrnVal = gfAmntDisp(Style.TotCost,lcRpCurr,ldRpExDate,lcRpTmpNam)
    ENDIF  
  ENDIF
ENDIF
*C121153,1 ASH 01/11/2004 (End)
*-- End If Print in forign currency.
RETURN lnRetrnVal

*--end of lfCalAmnts.
*:*************************************************************
*: Name      : lfvPrice
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/14/2003
*: Purpose   : Enable / Disable Currency display if Retail price.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None
*:*************************************************************
*: Example   : =lfChCurSm()
*:*************************************************************
*:
FUNCTION lfvPrice
lnPricePos = ASCAN(laOGObjType,'lnRepCurr')
IF lnPricePos # 0
  lnPricePos = ASUBSCRIPT(laOGObjType,lnPricePos,1)
  laOGObjCnt[lnPricePos] = (lcRpPrice = "C")
  *-- if Costing price.
  IF lcRpPrice = "C"
    lcRpCurr = "F"
  ELSE  && else if Retail price.
    lcRpCurr = "O"
  ENDIF
  = lfOGShowGet('lnRepCurr')
ENDIF

*-- end of lfvPrice.
*:*************************************************************