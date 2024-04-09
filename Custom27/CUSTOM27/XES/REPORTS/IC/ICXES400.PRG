************************************************************************
* PROG: ICXES400.PRG  (Converted from 26 to 27)
* DESC: CUSTOM CUT & SOLD (FOR XES600)
* AUTH: Adel Mohammed El Gazzar.
* DATE: 12/06/2000
* Refer to (102072)
*:**************************************************************************
*: Calls : 
*:         FUNCTION  : lfSayHdr()
*:                   : lfColData()
*:                   : lfSayBody()
*:                   : lfSayFoot()
******************************************************************************

lcDateExp = ' '
*--If the Activisty date is entered, Extract it.
lnDatePos = AT('BETWEEN(DTOS(INVLINE.INVDATE',lcRpExp)
*--If the user entered a date range.
IF lnDatePos > 0
  *--Get the next 'AND' OR '.AND.' to determine the end of the date range expression.
  lnNextAnd  = MIN(AT('AND',SUBSTR(lcRpExp,lnDatePos)),AT('.AND',SUBSTR(lcRpExp,lnDatePos)))
  *--If it's the last option in the filter, then its end will be the end of lcRpExp.
  lnNextAnd  = IIF(lnNextAnd<>0,lnNextAnd,LEN(lcRpExp))
  lcDateExp   = SUBSTR(lcRpExp,lnDatePos,lnNextAnd)
  *--Extract it as the filter will be set on the style file.
  lcRpExp    = STRTRAN(lcRpExp,lcDateExp,'.T. ')
ENDIF
lcDateExp = IIF(!EMPTY(lcDateExp),lcDateExp,'.T.')
*--Is there any record that meets the filter?
SELECt STYLE
*--Clear filter as it may be filtered from a previous run.
SET FILTER TO
LOCATE FOR &lcRpExp
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ELSE
  SET FILTER TO &lcRpExp
ENDIF

R_WIDTH    = 'W'
*---Global variable
ROW      = 5
PAGENO   = 0
ldLDate  = {}
ldHDate  = {}
*lcFilter = ''
lcStyle    = SPACE(12)
lcStyDec   = SPACE(20)
lcGroup    = SPACE(02)
lcGrpDesc  = SPACE(15)
lnInvoices = 0
lnOpnOrd   = 0
lnBooked   = 0
lnWip      = 0
lnAllWip   = 0
lnTWip     = 0
lnStock    = 0
lnRStock   = 0
lnIStock   = 0
lnStkAmt   = 0
lnCut      = 0
lnAvail    = 0
lnToSell   = 0
R_TITLE    = "CUT & SOLD"
DIMENSION laGrpTot[9]  &&DIMENSION laGrpTot[8]
DIMENSION laFabTot[9]  &&DIMENSION laFabTot[8]
laFabTot = 0
laGrpTot = 0
*STORE SPACE(30) TO XTITLE
XTITLE = lcRptitle
STORE SPACE(01) TO XSTATUS, TSEA, TDIV
STORE SPACE(12) TO LSTYLE, HSTYLE, XFABRIC

DIMENSION XSEASON(6), XDIVISION(6)
*STORE SPACE(30) TO XTITLE
STORE SPACE(12) TO LSTYLE, HSTYLE
STORE SPACE(7)  TO XFABRIC
STORE SPACE(2)  TO XSEASON, XDIVISION, XGROUP
STORE "A"       TO XSTATUS
STORE SPACE(10) TO XPATTERN
DIMENSION laGrand[9]  &&DIMENSION laGrand[8]
laGrand = 0
DIMENSION laTotStk(9),laQty(9),laPoQty(9),laTotOrd(9)
STORE 0 TO lnOver
STORE 0 To   lnPOpnQty , lnOpenQty
lcOrdFltr = ''
STORE {} TO ldLDate,ldHDate
lnMajLen = LEN(gfItemMask("PM"))

*--Get date Range
lnDatePos = ASCAN(laOGFxFlt,"INVLINE.INVDATE")
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  lnPPos    = AT("|",laOGFxFlt[lnDatePos,6])
  ldLDate   = IIF(lnPPos <>1,CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,10)),{})
  ldHDate   = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],lnPPos+1,10))
ENDIF
IF llOgFltCh
  *--Prepare files and relations
  =lfPrepFile()
  *-- Collect data  
  =lfColData()
ENDIF  
*--S T A R T   P R I N T I N G
SET DEVICE TO PRINT
= lfSayHdr()
IF lfSayBody()
  =lfSayGrand()
  DO ENDREPORT
ENDIF
SET DEVICE TO SCREEN
*---End Main

*:**************************************************************************
*: PROGRAM   : lfColData  (C#101281)
*: DESC.     : Function to collect data according to selection criteria.
*: MODULE    : Aria Apparel Series.
*: DATE      : 08/12/98
*: Developer : Ahmed Salah Shalaby
*:**************************************************************************
FUNCTION lfColData

SELECT STYLE
SET ORDER TO TAG STYLE
WAIT WINDOW 'Collecting Data. PLease wait..' NOWAIT
SCAN WHILE INKEY() <> 32
  STORE 0 TO lnWip,lnToSell,lnOver
  STORE 0 TO laTotStk,laQty,laPoQty,laTotOrd
  STORE 0 To  lnOpnOrd, lnPOpnQty , lnOpenQty
  lcStyle    = STYLE
  lcStyDec   = Desc
  lcGroup    = cStyGroup
  lcGrpDesc  = gfCodDes(lcGroup , 'CSTYGROUP') 
  lcFabDesc  = IIF(SEEK(FABRIC,"FABRIC"),FABRIC.Desc,"")
  *-- (1) Start calculate Invoices (Neglect voided invoice )
  SELECT INVLINE
  = SEEK(lcStyle)
  SUM REST TotQty TO lnInvoices WHILE style+invoice+STR(lineno,6)= lcStyle ;
  FOR &lcDateExp .AND. InvHdr.Status <> 'V'
  *-- (2) Start calculate Open order 
  SELECT OrdLine
  = SEEK(lcStyle)
  lcDateExp = STRTRAN(lcDateExp,'INVLINE.INVDATE','ORDHDR.ENTERED')
  *-- add the lcFilter To the new filter lcOrdFlt that hold the entered date. 
  lcOrdFltr = lcDateExp
  lcDateExp = STRTRAN(lcDateExp,'ORDHDR.ENTERED','ORDHDR.COMPLETE')
  SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = lcStyle
    WAIT WINDOW 'Calculating open orders for style : '+ALLTRIM(SUBSTR(lcStyle,1,lnMajLen))+' :Order#: '+Order NOWAIT
    *- Compute open to sell on Complete date.
    IF OrdHdr.Status $ 'OH' AND &lcDateExp
      FOR I = 1 To 8 
        lcCount = STR(I,1)
        laTotOrd[I] = Qty&lcCount
        *- Add Qty size to total qty.
        laTotOrd[9] = laTotOrd[9] + Qty&lcCount
      ENDFOR
      *- Add total qty To open orders.
      lnOpnOrd = lnOpnOrd + laTotOrd(9) 
    ENDIF  
    *- Compute open to sell on Entered date.
    IF &lcOrdFltr
      IF OrdHdr.Status $ 'OH'
        lnOpenQty = lnOpenQty + ToTQty
      ENDIF
      IF OrdHdr.Status <> 'X' AND !EMPTY(ldHDate) AND SEEK(lcStyle,'InvLine')
        SELECT INVLINE  
        lnPOpnQty = 0
        SUM REST TotQty WHILE style+invoice+STR(lineno,6) = lcStyle;
                        FOR InvHdr.Status<>'V' AND Order  = OrdLine.Order;
                        AND LineNo=OrdLine.LineNo;
                        AND Invdate > ldHDate;
                        TO lnPOpnQty
        lnOpenQty = lnOpenQty + lnPOpnQty
      ENDIF  
    ENDIF
  ENDSCAN
  WAIT CLEAR
  *-- (2) End Open order calculation
  *-- (3) Start calculate Booked
  lnBooked = lnInvoices + lnOpenQty
  *-- (3) Start Booked calculation
  *-- (4) Start calculate Wip
  SELECT PoSLn
  = SEEK(lcStyle+'P')
  lcDateExp = STRTRAN(lcDateExp,'ORDHDR.COMPLETE','POSHDR.ENTERED')
  DO WHILE STYLE = lcStyle AND cstytype = 'P'
    lcPo = PO
    STORE 0 TO laQty
    SUM REST IIF(TranCd='1',QTY1,-Qty1),IIF(TranCd='1',QTY2,-Qty2),; 
             IIF(TranCd='1',QTY3,-Qty3),IIF(TranCd='1',QTY4,-Qty4),; 
             IIF(TranCd='1',QTY5,-Qty5),IIF(TranCd='1',QTY6,-Qty6),; 
             IIF(TranCd='1',QTY7,-Qty7),IIF(TranCd='1',QTY8,-Qty8); 
        FOR &lcDateExp .AND. POSHDR.STATUS $ 'OH';
        WHILE style+cstytype+po+STR(lineno,6)+trancd = lcStyle+'P'+lcPo TO ARRAY laQty      
    FOR lnCount = 1 TO 8
      laPoQty(lnCount) = laPoQty(lnCount) + MAX(laQty(lnCount),0) 
    ENDFOR      
  ENDDO
  FOR lnCount = 1 TO 8
    lnWip = lnWip + laPoQty(lnCount)
  ENDFOR
  =SEEK(lcStyle)
  SUM REST IIF(TranCd='1',TotQty,IIF(TranCd = "2",0,-TotQty));
      FOR &lcDateExp .AND. POSHDR.STATUS $ 'OHC';
      WHILE style+cstytype+po+STR(lineno,6)+trancd = lcStyle TO lnCut
  *-- (4) End Wip calculation
  *-- (5) Start calculate Stock
  IF !EMPTY(ldHDate)
    SELECT STYINVJL
    =SEEK(lcStyle)
    SUM REST IIF(cIrType='R',NSTK1,IIF(cTrType $ "29",-NSTK1,-ABS(NSTK1))), IIF(cIrType='R',NSTK2,IIF(cTrType $ "29",-NSTK2,-ABS(NSTK2))),;
             IIF(cIrType='R',NSTK3,IIF(cTrType $ "29",-NSTK3,-ABS(NSTK3))), IIF(cIrType='R',NSTK4,IIF(cTrType $ "29",-NSTK4,-ABS(NSTK4))),;
             IIF(cIrType='R',NSTK5,IIF(cTrType $ "29",-NSTK5,-ABS(NSTK5))), IIF(cIrType='R',NSTK6,IIF(cTrType $ "29",-NSTK6,-ABS(NSTK6))),;
             IIF(cIrType='R',NSTK7,IIF(cTrType $ "29",-NSTK7,-ABS(NSTK7))), IIF(cIrType='R',NSTK8,IIF(cTrType $ "29",-NSTK8,-ABS(NSTK8))),;
             IIF(cIrType='R',NTOTSTK,IIF(cTrType $ "29",-NTOTSTK,-ABS(NTOTSTK)));
    TO ARRAY laTotStk ;
    WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = lcStyle;
    FOR DtrDate <= ldHDate
  ELSE
    IF !USED(lcStyTmp)
      =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH',@lcStyTmp,.T.)
    ENDIF
    SELECT (lcStyTmp)
    =SEEK(lcStyle)
    SUM REST STK1,STK2,STK3,STK4,STK5,STK6,STK7,STK8,TOTSTK ;
    TO ARRAY laTotStk ;
    WHILE Style = lcStyle 
  ENDIF
  lnStkAmt = laTotStk(9)
  *-- (7) Start calculate Available
  lnAvail = lnWip + lnStkAmt
  *-- (7) End Available calculation
  *-- (8) Start calculate ToSell
  IF lnAvail - lnOpnOrd < 0
    lnOver = lnStkAmt + lnWip - lnOpnOrd
  ELSE
    lnToSell = lnAvail - lnOpnOrd
  ENDIF
  *-- (8) End ToSell calculation
  *--Start replace calculated data in CURSOR (lcWorkTmp)
  IF lnCut <> 0 .OR. lnBooked <> 0 .OR. lnInvoices <> 0 .OR. lnOpnOrd <> 0 ;
     .OR. lnToSell <> 0 .OR. lnAvail <> 0 .OR. lnWip <> 0 .OR. lnStkAmt <> 0;
     .OR. lnOpenQty <> 0
    SELECT (lcWorkTmp)
    APPEND BLANK
    REPLACE Style      WITH SUBSTR(Style.Style,1,lnMajLen)  ;
            Color      WITH SUBSTR(STYLE.style,lnMajLen+2);
            StyDesc    WITH lcStyDec ;
            Fabric     WITH STYLE.Fabric;
            Group      WITH lcGroup  ;
            GrpDesc    WITH lcGrpDesc;
            FabDesc    WITH lcFabDesc;
            Cut        WITH lnCut;
            Booked     WITH lnBooked;
            Invoices   WITH lnInvoices;
            OpnOrd     WITH lnOpenQty;
            ToSell     WITH lnToSell;
            Over       WITH lnOver ;
            Avail      WITH lnAvail;
            Wip        WITH lnWip;
            Stock      WITH lnStkAmt
  ENDIF
ENDSCAN

IF lcRpForm = "S"
  *--Erase old one
  IF USED(lcCursor)
    USE IN (lcCursor)
  ENDIF  
  SELECT STYLE,COLOR,STYDESC,FABRIC,GROUP,GRPDESC,FABDESC,SUM(CUT) AS CUT,;
         SUM(BOOKED) AS BOOKED,SUM(INVOICES) AS INVOICES,SUM(OPNORD) AS OPNORD,;
         SUM(TOSELL) AS TOSELL,SUM(OVER) AS OVER ,SUM(AVAIL) AS AVAIL,;
         SUM(WIP) AS WIP , SUM(STOCK) AS STOCK;
  FROM (lcWorkTmp);
  INTO CURSOR (lcCursor);
  GROUP BY STYLE
  COPY TO (gcWorkDir+lcCursor)
  USE IN (lcCursor)
  =gfOpenFile(gcWorkDir+'&lcCursor','','EX')
*  INDEX ON STYLE TAG (lcCursor) OF (gcWorkDir+lcCursor)
  INDEX ON Fabric + Group + STYLE TAG (lcCursor) OF (gcWorkDir+lcCursor)
ENDIF

*:**************************************************************************
*: PROGRAM   : lfSayHdr  (C#101281)
*: DESC.     : Function to print report header.
*: MODULE    : Aria Apparel Series.
*: DATE      : 08/12/98
*: Developer : Ahmed Salah Shalaby
*:**************************************************************************
FUNCTION lfSayHdr

 *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*STYLE        COLOR   DESCRIPTION              CUT   BOOKED  SHIPPED  OPEN ORD.   TO SELL  OVER SOLD   AVAILABLE     WIP   STOCK 
*XXXXXXXXXXXX XXXXXX  XXXXXXXXXXXXXXXXXXXX  999999   999999   999999     999999    999999     999999      999999  999999  999999
*             ------  --------------------  ------   ------   ------     ------    ------     ------      ------  ------  ------
PAGENO = PAGENO + 1
DO RPT_HDR WITH  "ICXES400",XTITLE,R_WIDTH
ROW = 6
@ ROW,00 SAY IIF(ALIAS() = lcRecap,IIF(Code ="F","Fabric","Group"),"STYLE")
@ ROW,13 SAY IIF(lcRpForm = "D", lcColorTlt ,"")
@ ROW,21 SAY "DESCRIPTION"   
@ ROW,46 SAY "CUT"
@ ROW,52 SAY "BOOKED"
@ ROW,60 SAY "SHIPPED"
@ ROW,69 SAY "OPEN ORD."
@ ROW,81 SAY "TO SELL"
@ ROW,90 SAY "OVER SOLD"
*@ ROW,95 SAY "AVAILABLE"
@ ROW,102 SAY "AVAILABLE"
@ ROW,116 SAY "WIP"
@ ROW,122 SAY "STOCK"
ROW = ROW + 1
@ ROW,000 SAY "------------"
@ ROW,21 SAY "--------------------"
@ ROW,43 SAY "------"
@ ROW,52 SAY "------"
@ ROW,61 SAY "------"
@ ROW,72 SAY "------"
@ ROW,082 SAY "------"
@ ROW,93 SAY "------"
@ ROW,105 SAY "------"
@ ROW,113 SAY "------"
@ ROW,121 SAY "------"
ROW = ROW + 1

*:**************************************************************************
*: PROGRAM   : lfSayBody  (C#101281)
*: DESC.     : Function to print report body.
*: MODULE    : Aria Apparel Series.
*: DATE      : 08/12/98
*: Developer : Ahmed Salah Shalaby
*:**************************************************************************
FUNCTION lfSayBody

PRIVATE lcOldGrp
lcOldGrp = SPACE(02)
lcOldFab = SPACE(7)
SELECT IIF(lcRpForm = "S",(lcCursor),(lcWorkTmp))
GOTO TOP
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN .F.
ENDIF

 *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*STYLE        COLOR   DESCRIPTION              CUT   BOOKED  SHIPPED  OPEN ORD.   TO SELL  OVER SOLD   AVAILABLE     WIP   STOCK 
*XXXXXXXXXXXX XXXXXX  XXXXXXXXXXXXXXXXXXXX  999999   999999   999999     999999    999999     999999      999999  999999  999999
SCAN WHILE INKEY() <> 32
  WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
  IF lcOldGrp <> Group .AND. !EMPTY(lcOldGrp)
    IF ROW > 57
      =lfSayHdr()
    ENDIF
    =lfSayFoot("LAGRPTOT",lcOldGrp,lcOldDesc)
  ENDIF
  IF lcOldFab <> Fabric .AND. !EMPTY(lcOldFab)
    IF ROW > 57
      =lfSayHdr()
    ENDIF
    =lfSayFoot("LAFABTOT",lcOldFab,lcFabDesc)
  ENDIF
  lcOldGrp  = Group  
  lcOldDesc = GrpDesc
  lcOldFab  = Fabric
  lcFabDesc = FabDesc 
  *@ ROW,00 SAY IIF(lcRpForm = "D",STYLE,SUBSTR(STYLE,1,lnMajLen))
  @ ROW,00 SAY Style
  @ ROW,13 SAY IIF(lcRpForm = "D",Color,"")
  @ ROW,21 SAY StyDesc
  @ ROW,43 SAY CUT      PICTURE "999999"
  laGrpTot[1] = laGrpTot[1] + CUT
  laFabTot[1] = laFabTot[1] + CUT
  @ ROW,52 SAY Booked   PICTURE "999999"
  laGrpTot[2] = laGrpTot[2] + Booked
  laFabTot[2] = laFabTot[2] + Booked
  @ ROW,61 SAY Invoices PICTURE "999999"
  laGrpTot[3] = laGrpTot[3] + Invoices
  laFabTot[3] = laFabTot[3] + Invoices
  @ ROW,72 SAY OpnOrd   PICTURE "999999"
  laGrpTot[4] = laGrpTot[4] + OpnOrd
  laFabTot[4] = laFabTot[4] + OpnOrd
  @ ROW,082 SAY ToSell   PICTURE "999999"
  laGrpTot[5] = laGrpTot[5] + ToSell
  laFabTot[5] = laFabTot[5] + ToSell
  @ ROW,093 SAY Over PICTURE "999999"
  laGrpTot[9] = laGrpTot[9] + Over
  laFabTot[9] = laFabTot[9] + Over
  
  @ ROW,105 SAY Avail PICTURE "999999"
  laGrpTot[6] = laGrpTot[6] + Avail
  laFabTot[6] = laFabTot[6] + Avail
  @ ROW,113 SAY WIP PICTURE "999999" 
  laGrpTot[7] = laGrpTot[7] + WIP
  laFabTot[7] = laFabTot[7] + WIP
  @ ROW,121 SAY STOCK PICTURE "999999" 
  laGrpTot[8] = laGrpTot[8] + STOCK
  laFabTot[8] = laFabTot[8] + STOCK
  ROW = ROW + 1
  IF ROW > 60
    =lfSayHdr()
  ENDIF
ENDSCAN
SKIP -1
=lfSayFoot("LAGRPTOT",lcOldGrp,lcOldDesc)
=lfSayFoot("LAFABTOT",lcOldFab,lcFabDesc)
RETURN .T.

*:**************************************************************************
*: PROGRAM   : lfSayFoot  (C#101281)
*: DESC.     : Function to print report body.
*: MODULE    : Aria Apparel Series.
*: DATE      : 08/12/98
*: Developer : Ahmed Salah Shalaby
*:**************************************************************************
FUNCTION lfSayFoot

PARAMETERS laTot,lcOldGrp,lcOldDesc
@ ROW,0 SAY REPLICATE('-',132)
ROW = ROW + 1
@ ROW,000 SAY lcOldGrp
@ ROW,8 SAY lcOldDesc
@ ROW,026 SAY "TOTAL : " 

 *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*STYLE        COLOR   DESCRIPTION              CUT   BOOKED  SHIPPED  OPEN ORD.   TO SELL  OVER SOLD   AVAILABLE     WIP   STOCK 
*XXXXXXXXXXXX XXXXXX  XXXXXXXXXXXXXXXXXXXX  999999   999999   999999     999999    999999     999999      999999  999999  999999
*        								   9999999  9999999	 9999999    9999999	  9999999    9999999              9999999 9999999 9999999
@ ROW,42 SAY &laTot[1] PICTURE "9999999"
@ ROW,51 SAY &laTot[2] PICTURE "9999999"
@ ROW,60 SAY &laTot[3] PICTURE "9999999"
@ ROW,71 SAY &laTot[4] PICTURE "9999999"
@ ROW,081 SAY &laTot[5] PICTURE "9999999"
@ ROW,92 SAY &laTot[9] PICTURE "9999999"

@ ROW,104 SAY &laTot[6] PICTURE "9999999"
@ ROW,112 SAY &laTot[7] PICTURE "9999999"
@ ROW,120 SAY &laTot[8] PICTURE "9999999"
IF LaTot ="LAGRPTOT"
  FOR lnCount = 1 TO 9
    laGrand[lnCount] = laGrand[lnCount] + laGrpTot[lnCount]
  ENDFOR
ENDIF
IF IIF(lcRpRecap = "G",laTot = "LAGRPTOT",IIF(lcRpRecap = "F",laTot = "LAFABTOT",.T.))
  SELECT (lcRecap)
  IF !SEEK(IIF(laTot = "LAGRPTOT","G","F") + lcOldGrp)
    APPEND BLANK      
  ENDIF
  REPLACE Code     WITH IIF(laTot = "LAGRPTOT","G","F"),;
          Group    WITH lcOldGrp,;
          Desc     WITH lcOldDesc,;
          Cut      WITH Cut + &laTot(1) ,;
          Booked   WITH Booked + &laTot(2) ,;
          Invoices WITH Invoices + &laTot(3) ,;
          OpnOrd   WITH OpnOrd + &laTot(4),;
          ToSell   WITH ToSell + &laTot(5),;
          Over     WITH Over + &laTot(9) ,;
          Avail    WITH Avail + &laTot(6),;
          Wip      WITH Wip + &laTot(7),;
          Stock    WITH Stock + &laTot(8)
  SELECT IIF(lcRpForm = "S",(lcCursor),(lcWorkTmp))
ENDIF 
&laTot = 0
ROW = ROW + 1
@ ROW,0 SAY REPLICATE('-',132)
ROW = ROW + 1

*:**************************************************************************
*: PROGRAM   : lfSayGrand  (C#101318)
*: DESC.     : Function to print report Grand total.
*: MODULE    : Aria Apparel Series.
*: DATE      : 08/12/98
*: Developer : Ahmed Salah Shalaby
*:**************************************************************************
FUNCTION lfSayGrand

@ ROW,0 SAY REPLICATE('-',132)
ROW = ROW + 1
 *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*STYLE        COLOR   DESCRIPTION              CUT   BOOKED  SHIPPED  OPEN ORD.   TO SELL  OVER SOLD   AVAILABLE     WIP   STOCK 
*XXXXXXXXXXXX XXXXXX  XXXXXXXXXXXXXXXXXXXX  999999   999999   999999     999999    999999     999999      999999  999999  999999
*        								   9999999  9999999	 9999999    9999999	  9999999    9999999              9999999 9999999 9999999
@ ROW,000 SAY "GRAND TOTAL : "
@ ROW,42  SAY laGrand[1] PICTURE "9999999"
@ ROW,51  SAY laGrand[2] PICTURE "9999999"
@ ROW,60  SAY laGrand[3] PICTURE "9999999"
@ ROW,71  SAY laGrand[4] PICTURE "9999999"
@ ROW,81  SAY laGrand[5] PICTURE "9999999"
@ ROW,92  SAY laGrand[9] PICTURE "9999999"
@ ROW,104 SAY laGrand[6] PICTURE "9999999"
@ ROW,112 SAY laGrand[7] PICTURE "9999999"
@ ROW,120 SAY laGrand[8] PICTURE "9999999"

ROW = ROW + 1
@ ROW,0 SAY REPLICATE('-',132)
ROW = ROW + 1
STORE 0 TO laGrand
SELECT (lcRecap)
GO TOP
IF !EOF(LCRECAP)
  =lfSayHdr()
  lcCode = Code
  SCAN
    IF ROW > 60 
      =lfSayHdr()
    ENDIF
    IF Code <> lcCode
      @ ROW,0 SAY REPLICATE('-',132)
      ROW = ROW + 1
      @ ROW,000 SAY "GRAND TOTAL : "
      @ ROW,42 SAY laGrand[1] PICTURE "9999999"
      @ ROW,51 SAY laGrand[2] PICTURE "9999999"
      @ ROW,60 SAY laGrand[3] PICTURE "9999999"
      @ ROW,71 SAY laGrand[4] PICTURE "9999999"
      @ ROW,81 SAY laGrand[5] PICTURE "9999999"
      @ ROW,92 SAY laGrand[6] PICTURE "9999999"
      @ ROW,104 SAY laGrand[7] PICTURE "9999999"
      @ ROW,112 SAY laGrand[8] PICTURE "9999999"
      @ ROW,120 SAY laGrand[9] PICTURE "9999999"
      ROW = ROW +1
      @ ROW,0 SAY REPLICATE('-',132)
      STORE 0 TO laGrand
      lcCode = Code
      =lfSayHdr()
    ENDIF
    @ ROW,000 SAY Group
    @ ROW,21 SAY Desc
    @ ROW,42 SAY CUT      PICTURE "9999999"
    @ ROW,51 SAY Booked   PICTURE "9999999"
    @ ROW,60 SAY Invoices PICTURE "9999999"
    @ ROW,71 SAY OpnOrd   PICTURE "9999999"
    @ ROW,81 SAY ToSell   PICTURE "9999999"
    @ ROW,092 SAY Over PICTURE "9999999"
    @ ROW,104 SAY Avail PICTURE "9999999"
    @ ROW,112 SAY WIP PICTURE "9999999"  
    @ ROW,120 SAY STOCK PICTURE "9999999"
    ROW = ROW + 1
    laGrand(1) = laGrand(1) + CUT
    laGrand(2) = laGrand(2) + Booked
    laGrand(3) = laGrand(3) + Invoices
    laGrand(4) = laGrand(4) + OpnOrd
    laGrand(5) = laGrand(5) + ToSell
    laGrand(6) = laGrand(6) + Over
    laGrand(7) = laGrand(7) + Avail
    laGrand(8) = laGrand(8) + Wip
    laGrand(9) = laGrand(9) + Stock
  ENDSCAN
  @ ROW,0 SAY REPLICATE('-',132)
  ROW = ROW + 1
  @ ROW,000 SAY "GRAND TOTAL : "
  @ ROW,42 SAY laGrand[1] PICTURE "9999999"
  @ ROW,51 SAY laGrand[2] PICTURE "9999999"
  @ ROW,60 SAY laGrand[3] PICTURE "9999999"
  @ ROW,71 SAY laGrand[4] PICTURE "9999999"
  @ ROW,81 SAY laGrand[5] PICTURE "9999999"
  @ ROW,92 SAY laGrand[6] PICTURE "9999999"
  @ ROW,104 SAY laGrand[7] PICTURE "9999999"
  @ ROW,112 SAY laGrand[8] PICTURE "9999999"
  @ ROW,120 SAY laGrand[9] PICTURE "9999999"
  ROW = ROW +1
  @ ROW,0 SAY REPLICATE('-',132)
ENDIF

*:**************************************************************************
*: PROGRAM   : lfToSell  (C#101358)
*: DESC.     : Function to get the open to sell and the over sold.
*: MODULE    : Aria Apparel Series.
*: DATE      : 12/01/98
*: Developer : Sherif Attala Ishak
*:**************************************************************************
FUNCTION lfToSell
PRIVATE lnSell

FOR lnCount = 1 TO 8
  lnSell = laTotStk(lnCount) + laPoQty(lnCount) ;
      - laTotOrd(lnCount) 
  IF lnSell < 0
    lnOver = lnOver + lnSell
  ELSE
    lnToSell = lnToSell + lnSell
  ENDIF
ENDFOR


*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Adel Mohammed El Gazzar (ADEL) 
*! Date      : 12/06/2000
*! Purpose   : OG When Function
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Parameters  : None
*!**************************************************************************
*! Returns     : .T.
*!**************************************************************************
*! Example     : = lfwRepWhen()
*!**************************************************************************
FUNCTION lfwRepWhen

IF !USED('ORDHDR')
  =gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
ENDIF
IF !USED('INVHDR')
  =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
ENDIF
IF !USED('POSHDR')
  =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
ENDIF
IF !USED('POSLN')
  =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'Poslns','SH')
ENDIF
SELECT POSLN
IF EMPTY(SET('RELATION'))
  SET RELATION TO 'P'+PO INTO POSHDR ADDI
ENDIF
IF !USED('WAREHOUS')
  =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
ENDIF
IF !USED('StyInvJl')
  =gfOpenFile(gcDataDir+'StyInvJl',gcDataDir+'StyInvJl','SH')
ENDIF
IF !USED(lcStyTmp)
  =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH',@lcStyTmp,.T.)
ENDIF


*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Adel Mohammed El Gazzar (ADEL) 
*! Date      : 11/22/2000
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Parameters  : lcParm
*!**************************************************************************
*! Example     : =lfsrvSty()
*!**************************************************************************
*! Note        : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
* 
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    GO TOP IN STYLE
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    *GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- End of lfsrvSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Adel Mohammed El Gazzar (ADEL) 
*! Date      : 11/22/2000
*! Purpose   : sum a specific field for the current style in style file
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Parameters  : lcSty,lccomp,lnAddToVar
*!**************************************************************************
*! Returns     : Calculated field value.
*!**************************************************************************
*! Example     : = lfStySum()
*!**************************************************************************
* 
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  

  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- End of lfStySum.
*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/18/2000
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
FUNCTION lfEvalSegs

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcNonMajTl = ''
lcNonMajPi = ''
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = ALLTRIM(lcNonMajTlt)
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- end of lfEvalSegs.

****************************************************************************
* FUNC: lfwOldVal
* DESC: To get the old value.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* Date: 12/05/2000
****************************************************************************
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/05/2000
*! Purpose   : Validation function for validating Fabric Code
*!*************************************************************
*! Called from : Only this color [Option Grid]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

lnAlias   = SELECT(0)
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal  = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
SELECT Fabric
lcFabOrder = ORDER()
SET ORDER TO Fabric
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
SELECT Fabric
SET ORDER TO &lcFabOrder
SELECT(lnAlias)


FUNCTION lfPrepFile

IF !USED(lcWorkTmp)
  CREATE TABLE (gcWorkDir+lcWorkTmp) (Style C(12),Color C(6),StyDesc C(20),Fabric C(7),Group C(06),GrpDesc C(15),;
                           FabDesc C(15), Cut N(12,2),Booked N(12,2),Invoices N(12,2),OpnOrd N(12,2),;
                           ToSell N(12,2),Over N(12,2), Avail N(12,2),Wip N(12,2), Stock N(12,2))
  INDEX ON Fabric + Group + STYLE TAG (lcWorkTmp) OF (gcWorkDir+lcWorkTmp)
ELSE
  IF RECCOUNT(lcWorkTmp) >0
    SELECT (lcWorkTmp)
    ZAP
  ENDIF  
ENDIF                           
IF !USED(lcRecap)
  CREATE TABLE (gcWorkDir+lcRecap) (Code C(1),Group C(7),Desc C(15),Cut N(12,2),Booked N(12,2),Invoices N(12,2),;
                           OpnOrd N(12,2),ToSell N(12,2),Over N(12,2),Avail N(12,2),Wip N(12,2), Stock N(12,2))
  INDEX ON Code + Group TAG CODE  
ELSE
  IF RECCOUNT(lcRecap) >0
    SELECT (lcRecap)
    ZAP
  ENDIF  
ENDIF
*--Set needed relations
SELECT ORDLINE
IF EMPTY(SET('RELATION'))
  SET RELATION TO cordtype+order INTO ORDHDR ADDI
ENDIF  
SELECT INVLINE
SET ORDER TO TAG Invlines
IF EMPTY(SET('RELATION'))
  SET RELATION TO Invoice INTO INVHDR ADDI
ENDIF
