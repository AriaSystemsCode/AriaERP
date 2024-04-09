*:***************************************************************************
*: Program file  : ICROB520
*: Program desc. : PRINT STOCK ADJUSTMENTS JOURNAL REPORT
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC )
*: Developer     : Ashraf Sherif Mohammad (ASH)
*: Customer      : Robyn Merdith
*:***************************************************************************
*: Calls : lfvStyle,lfvFabric,lfEvalSegs,lfvDates,lfWoldVal,lfWOpGrid
*:***************************************************************************
*C101338,1 ASH 11/03/98
*Notes : This report is copied from ROB5200 in ver 2.6.
*:***************************************************************************
*:E301248 AHM 06/03/99 Change the program to deal with Tran. Type 'I' that is
*:E301248              added in styinvjl file for issunig style from PO/CT cost sheet.
*:***************************************************************************

**-------- Assiging 2.6 variables with its eq. in 2.7 OG  [begin ] --------
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYINVJL.DTRDATE'),1)
LDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,;
            ATC('|',laOGFxFlt[lnDatePos,6])-1))
HDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],;
            ATC('|',laOGFxFlt[lnDatePos,6])+1))

lnStylePos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR'),1)
LStyle     = SUBSTR(laOGFxFlt[lnStylePos,6],1,ATC('|',laOGFxFlt[lnStylePos,6])-1)
HStyle     = SUBSTR(laOGFxFlt[lnStylePos,6],ATC('|',laOGFxFlt[lnStylePos,6])+1)

lnSeaPos   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.SEASON'),1)
xSeason    = laOGFxFlt[lnSeaPos,6]

lnDivPos   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CDIVISION'),1)
xDivision  = laOGFxFlt[lnDivPos,6]

lnGroupPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYGROUP'),1)
xGroup     = laOGFxFlt[lnGroupPos,6]

lnSessPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYINVJL.CSESSION'),1)
lcLSessNo  = SUBSTR(laOGFxFlt[lnSessPos,6],1,ATC('|',laOGFxFlt[lnSessPos,6])-1)
lcHSessNo  = SUBSTR(laOGFxFlt[lnSessPos,6],ATC('|',laOGFxFlt[lnSessPos,6])+1)

lnLocPos   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYINVJL.CWARECODE'),1)
lcWareCode = laOGFxFlt[lnLocPos,6]

lnColorPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)'),1)
TCLR       = STRTRAN(laOGFxFlt[lnColorPos,6],'|','')

XTITLE     = 'FROM: '+DTOC(LDATE)+' THRU:'+ DTOC(HDATE)
XDETAILYN  = IIF(llRpShow,'Y','N')
XFORMAT    = lcRpFormat
lcRByS     = IIF(llRpSess,'Y','N')
lcFormAorB = lcRpRepF
lcType     = IIF(lcRpAdj='B','PA',lcRpAdj)
llCostAccs = gfUserPriv('IC','ICSTYLE','COSTING')

*-Get memo variables.
*llMultiWH  = gfGetMemVar('M_WareHouse') = 'Y'
XAVG_COST  = gfGetMemVar('M_COST_METH') = 'A'
XDYELOT_S  = gfGetMemVar('M_DYELOT')    = 'Y'

IF llMultiWH
  lnStrtCol = 7
ELSE
  lnStrtCol = 0
ENDIF

*-- Relations
SELECT StyInvJl
SET RELATION TO STYLE INTO STYLE

*-- Build the filter.
XFILTER = ' '
IF XDETAILYN='N' AND XFORMAT='L'
  XFILTER  = 'BETWEEN(DATE,LDATE,HDATE)' 
ELSE
  XFILTER  = 'BETWEEN(dTrDate,LDATE,HDATE)' 
ENDIF

IF !EMPTY(LSTYLE) AND !EMPTY(HSTYLE)
  XFILTER = XFILTER + ".AND. BETWEEN(STYLE,LSTYLE,HSTYLE)"
ENDIF
IF lcRByS = 'Y'
  xFilter=STRTRAN(xFilter,'BETWEEN(dTrDate,LDATE,HDATE)','BETWEEN(cSession,lcLSessNo,lcHSessNo)') 
ENDIF
IF llMultiWH .AND. !EMPTY (lcWareCode)
  IF XDETAILYN  = 'N' AND XFORMAT='L'
    xFilter = xFilter + '.AND. (cFromWare = lcWareCode .OR. cToWare = lcWareCode)'
  ELSE
    xFilter = xFilter + '.AND. (cWareCode = lcWareCode )'
  ENDIF
ENDIF
IF !EMPTY (lcType) AND XDETAILYN = 'N' AND XFORMAT='L'
    xFilter = xFilter + '.AND. Type $ lcType'
ENDIF
IF LEN(TRIM(XSEASON)) <>0
  XFILTER = XFILTER + '.AND.STYLE.SEASON = XSEASON'
ENDIF

IF LEN(TRIM(XDIVISION)) <>0
  XFILTER = XFILTER + '.AND.STYLE.CDIVISION = XDIVISION'
ENDIF

IF LEN(TRIM(XGROUP)) <>0
  XFILTER = XFILTER + '.AND.STYLE.cStyGROUP = XGROUP'
ENDIF
TCLR = TRIM(TCLR)
IF LEN(TCLR) >0
  XFILTER = XFILTER + ".AND. TRIM(SUBSTR(Style.Style,lnNonMajSt,lnColorLen)) $ TCLR"
ENDIF

IF XDETAILYN='N' AND XFORMAT='L'
  XFILE = 'INVTADJ'
ELSE
  XFILE = 'STYINVJL'
ENDIF

SELECT &XFILE
SET FILTER TO
LOCATE ALL FOR &XFILTER

IF EOF()
  **No records selected.
  =gfModalGen('TRM42153B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

R_TITLE = 'ADJUSTMENTS JOURNAL'
XREPORT = 'ICROB5200'

SET DEVICE TO PRINT

XSTYLE   = SUBSTR(Style,1,lnMajorLen)
ROW      = 99
XSTOTPCS = 0
XSTOTAMT = 0
XGTOTPCS = 0
XGTOTAMT = 0
PAGENO   = 0
XSTOTPhPCS = 0
XSTOTPhAMT = 0
XGTOTPhPCS = 0
XGTOTPhAMT = 0
DIMENSION XNEWQTY(8)                    && USED ONLY IN NEW LONG FORMAT
WORK = GFTEMPNAME()


DO CASE
  *-Show detail and long format form 'A'.
  CASE XDETAILYN = 'Y' AND XFORMAT='L' .AND. lcFormAorB='A'
    *--Build temp file.
    IF !lpBldTmp()
      RETURN
    ENDIF
    =lpLADETY()
   
  *-Show detail and long format form 'B'.  
  CASE XDETAILYN = 'Y' AND XFORMAT='L' .AND. lcFormAorB='B'
    *-Build temp file.
    IF !lpBldTmp()
      RETURN
    ENDIF
    =lpSty900LB()

  *-Short format
  CASE XFORMAT='S'
    llGoOut = .F.
    =lpFormSh()
    IF llGoOut
      RETURN
    ENDIF
  *- Show detail no and long format form 'A'.
  CASE XDETAILYN = 'N' AND XFORMAT='L' .AND. lcFormAorB='A'
    =lpLADETN()
  
  *- Show detail no and long format form 'B'.
  CASE XDETAILYN = 'N' AND XFORMAT='L' .AND. lcFormAorB='B'
    =lpLBDETN()
ENDCASE

*SET DEVICE TO PRINT
DO ENDREPORT
SET DEVICE TO SCREEN

IF USED (WORK)
  USE IN &WORK
  ERASE &Work..DBF
ENDIF
*-- END OF REPORT CODE.


*:************************************************************************
*: Program file  : lpSty900LB                     
*: Program desc. : A new form for the style stock adjustemnts journal report.
*:         Module: Aria Apparel Series.
*:      Developer: Ashraf Sherif Mohammad (ASH)
*: MODI          :
*:************************************************************************
PROCEDURE lpSty900LB

******* INVENTORY ADJUSTMENT JOURNAL THE LONG FOR THE NEW ONE ******
SELECT &WORK
GO TOP
XSTYLE   = SUBSTR(Style,1,lnMajorLen)

SCAN
  IF ROW > 55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH  XREPORT,XTITLE,R_WIDTH
    ROW = 5
    @ ROW,01 SAY 'STYLE        COLOR  DIV   TYP                     SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY  TOT. COST DATE'
    ROW = ROW + 1
  ENDIF

  IF SUBSTR(Style,1,lnMajorLen) <> XSTYLE
    @ ROW,00 SAY REPLICATE('.',130)
    ROW = ROW + 1
    @ ROW,000 SAY '*** ADJUSTMENTS  SUBTOTAL ***'+XSTYLE
    @ ROW,104 SAY XSTOTPCS PICTURE '9999999'
    IF llCostAccs
      @ ROW,112 SAY XSTOTAMT PICTURE '9999999.99'
    ENDIF

    ROW = ROW + 1
    @ ROW,000 SAY '***   PHYSICAL SUBTOTAL   ***'+XSTYLE
    @ ROW,104 SAY XSTOTPhPCS PICTURE '9999999'
    IF llCostAccs
      @ ROW,112 SAY XSTOTPhAMT PICTURE '9999999.99'
    ENDIF
    
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('.',130)
    ROW = ROW + 1
    XSTYLE   = SUBSTR(Style,1,lnMajorLen)
    XSTOTPCS  = 0
    XSTOTAMT  = 0
    XSTOTPhPCS  = 0
    XSTOTPhAMT  = 0
  ENDIF

  IF EOF() .OR. INKEY() = 32
    
    @ ROW,00 SAY REPLICATE('-',130)
    ROW = ROW + 1
    @ ROW,000 SAY '*** ADJUSTMENTS  GRAND TOTAL ***'

    @ ROW,104 SAY XGTOTPCS PICTURE '9999999'
    IF llCostAccs
      @ ROW,112 SAY XGTOTAMT PICTURE '9999999.99'
    ENDIF
    
    ROW = ROW + 1
    @ ROW,000 SAY '***   PHYSICAL GRAND TOTAL   ***'
    @ ROW,104 SAY xGTotPhPcs PICTURE '9999999'
    IF llCostAccs
      @ ROW,112 SAY xGTotPhAmt PICTURE '9999999.99'
    ENDIF
    ROW = ROW + 1
    
    @ ROW,00 SAY REPLICATE('-',130)
    ROW = ROW + 1
    EXIT
  ENDIF

*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
*STYLE   CLR DYELOT #   DV TYP                     SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY TOT. COST  DATE
*ADJUSTMENT REFERENCE
*1234567 123 XXXXXXXXXX 12 XXX EXISTING INVT:-> 123456 123456 123456 123456 123456 123456 123456 123456 1234567 1234567.99 99/99/99
*1234567890123456789012345     ADJUSTED INVT:->
*                              NEW INVENTORY:->

  IF XDETAILYN = 'Y'
    @ ROW,001 SAY SUBSTR(STYLE,1,lnMajorLen)
    @ ROW,014 SAY SUBSTR(Style,lnMajorLen+2,lnColorLen)
    @ ROW,021 SAY STYLE.CDIVISION
    *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment.
    *@ ROW,027 SAY IIF(cTrType='1','ADJ','PHY')
    @ ROW,027 SAY IIF(cTrType $ 'I1','ADJ','PHY')
    *E301248 AHM 06/03/99 (End))
    @ ROW,031 SAY 'EXISTING INVT:->'
    @ ROW,048 SAY OLDQTY1 PICTURE '@Z 999999'
    @ ROW,055 SAY OLDQTY2 PICTURE '@Z 999999'
    @ ROW,062 SAY OLDQTY3 PICTURE '@Z 999999'
    @ ROW,069 SAY OLDQTY4 PICTURE '@Z 999999'
    @ ROW,076 SAY OLDQTY5 PICTURE '@Z 999999'
    @ ROW,083 SAY OLDQTY6 PICTURE '@Z 999999'
    @ ROW,090 SAY OLDQTY7 PICTURE '@Z 999999'
    @ ROW,097 SAY OLDQTY8 PICTURE '@Z 999999'

    @ ROW,104 SAY TOTOLD PICTURE '@Z 9999999'
    IF llCostAccs
      @ ROW,112 SAY TotOld*IIF(XAVG_COST,nOLDCOST,STYLE->TOTCOST) ;
                    PICTURE '9999999.99'
    ENDIF
    ROW = ROW + 1
    @ ROW,01 SAY SUBSTR(REFERENCE,1,25)
    @ ROW,027 SAY cSession
    @ ROW,035 SAY 'ADJ. INVT:->'
    IF cTrType = '2'
      @ ROW,048 SAY nStk1  - OLDQTY1   PICTURE '@Z 999999'
      @ ROW,055 SAY nStk2  - OLDQTY2   PICTURE '@Z 999999'
      @ ROW,062 SAY nStk3  - OLDQTY3   PICTURE '@Z 999999'
      @ ROW,069 SAY nStk4  - OLDQTY4   PICTURE '@Z 999999'
      @ ROW,076 SAY nStk5  - OLDQTY5   PICTURE '@Z 999999'
      @ ROW,083 SAY nStk6  - OLDQTY6   PICTURE '@Z 999999'
      @ ROW,090 SAY nStk7  - OLDQTY7   PICTURE '@Z 999999'
      @ ROW,097 SAY nStk8  - OLDQTY8   PICTURE '@Z 999999'
      @ ROW,104 SAY nTOTStk- TOTOLD    PICTURE '@Z 9999999'
      IF llCostAccs
        @ ROW,112 SAY (nTOTStk-TOTOLD)*IIF(XAVG_COST,NCOST,STYLE->TOTCOST) ;
                      PICTURE '9999999.99'
      ENDIF
    ELSE
      @ ROW,048 SAY nStk1   PICTURE '@Z 999999'
      @ ROW,055 SAY nStk2   PICTURE '@Z 999999'
      @ ROW,062 SAY nStk3   PICTURE '@Z 999999'
      @ ROW,069 SAY nStk4   PICTURE '@Z 999999'
      @ ROW,076 SAY nStk5   PICTURE '@Z 999999'
      @ ROW,083 SAY nStk6   PICTURE '@Z 999999'
      @ ROW,090 SAY nStk7   PICTURE '@Z 999999'
      @ ROW,097 SAY nStk8   PICTURE '@Z 999999'
      @ ROW,104 SAY nTOTStk PICTURE '@Z 9999999'
      IF llCostAccs
        @ ROW,112 SAY nTOTStk*IIF(XAVG_COST,NCOST,STYLE->TOTCOST) ;
                      PICTURE '9999999.99'
      ENDIF
    ENDIF
    @ ROW,123 SAY dTrDATE
    ROW=ROW+1
     
    *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment. 
    *IF cTrType = '1'             && ADJUSTMENT
    IF cTrType $ 'I1'             && ADJUSTMENT
    *E301248 AHM 06/03/99 (End)
      XNEWQTY1 = OLDQTY1 + nStk1
      XNEWQTY2 = OLDQTY2 + nStk2
      XNEWQTY3 = OLDQTY3 + nStk3
      XNEWQTY4 = OLDQTY4 + nStk4
      XNEWQTY5 = OLDQTY5 + nStk5
      XNEWQTY6 = OLDQTY6 + nStk6
      XNEWQTY7 = OLDQTY7 + nStk7
      XNEWQTY8 = OLDQTY8 + nStk8
      XTOTNEW  = TOTOLD  + nTOTStk
    ELSE
      XNEWQTY1 = nStk1
      XNEWQTY2 = nStk2
      XNEWQTY3 = nStk3
      XNEWQTY4 = nStk4
      XNEWQTY5 = nStk5
      XNEWQTY6 = nStk6
      XNEWQTY7 = nStk7
      XNEWQTY8 = nStk8
      XTOTNEW  = nTOTStk
    ENDIF

    @ ROW,031 SAY 'NEW INVENTORY:->'
    @ ROW,048 SAY XNEWQTY1 PICTURE '@Z 999999'
    @ ROW,055 SAY XNEWQTY2 PICTURE '@Z 999999'
    @ ROW,062 SAY XNEWQTY3 PICTURE '@Z 999999'
    @ ROW,069 SAY XNEWQTY4 PICTURE '@Z 999999'
    @ ROW,076 SAY XNEWQTY5 PICTURE '@Z 999999'
    @ ROW,083 SAY XNEWQTY6 PICTURE '@Z 999999'
    @ ROW,090 SAY XNEWQTY7 PICTURE '@Z 999999'
    @ ROW,097 SAY XNEWQTY8 PICTURE '@Z 999999'
    @ ROW,104 SAY XTOTNEW PICTURE '@Z 9999999'
    IF llCostAccs
      *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment.
      *@ ROW,112 SAY ( XTotNew * IIF( XAvg_Cost, ;
                      IIF(cTrType='1',STYLE->Ave_Cost,nCost), ;
                      STYLE->TotCost) ) PICTURE '9999999.99'  
      @ ROW,112 SAY ( XTotNew * IIF( XAvg_Cost, ;
                      IIF(cTrType $ 'I1',STYLE->Ave_Cost,nCost), ;
                      STYLE->TotCost) ) PICTURE '9999999.99'         
      *E301248 AHM 06/03/99 (End)
    ENDIF
    ROW=ROW+1
  ENDIF

  *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment.
  *IF cTrType = '1'           && SUB TOTAL ON ADJUSTMENTS ONLY
  IF cTrType $ 'I1'           && SUB TOTAL ON ADJUSTMENTS ONLY
  *E301248 AHM 06/03/99 (End)
    XSTOTPCS  = XSTOTPCS + nTOTStk
    XSTOTAMT  = XSTOTAMT + (nTOTStk * IIF(XAVG_COST,NCOST,STYLE->TOTCOST))
    XGTOTPCS  = XGTOTPCS + nTOTStk
    XGTOTAMT  = XGTOTAMT + (nTOTStk*IIF(XAVG_COST,NCOST,STYLE->TOTCOST )) 
  ELSE
    xSTotPhPcs  = xSTotPhPcs + nTOTStk
    xSTotPhAmt  = xSTotPhAmt + nTOTStk * IIF(XAVG_COST,NCOST,STYLE->TOTCOST)
    xGTotPhPcs  = xGTotPhPcs + nTOTStk
    xGTotPhAmt  = xGTotPhAmt + nTOTStk * IIF(XAVG_COST,NCOST,STYLE->TOTCOST)
  ENDIF
ENDSCAN

@ ROW,00 SAY REPLICATE('.',130)
ROW = ROW + 1
@ ROW,000 SAY '*** ADJUSTMENTS  SUBTOTAL ***'+TRIM(XSTYLE)
@ ROW,104 SAY XSTOTPCS PICTURE '9999999'
IF llCostAccs
  @ ROW,112 SAY XSTOTAMT PICTURE '9999999.99'
ENDIF
ROW = ROW + 1
@ ROW,000 SAY '***   PHYSICAL SUBTOTAL   ***'+TRIM(XSTYLE)
@ ROW,104 SAY XSTOTPhPCS PICTURE '9999999'
IF llCostAccs
  @ ROW,112 SAY XSTOTPhAMT PICTURE '9999999.99'
ENDIF
    
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('.',130)
ROW = ROW + 1

@ ROW,00 SAY REPLICATE('-',130)
ROW = ROW + 1
@ ROW,000 SAY '*** ADJUSTMENTS  GRAND TOTAL ***'
@ ROW,104 SAY XGTOTPCS PICTURE '9999999'
IF llCostAccs
  @ ROW,112 SAY XGTOTAMT PICTURE '9999999.99'
ENDIF
    
ROW = ROW + 1
@ ROW,000 SAY '***   PHYSICAL GRAND TOTAL   ***'
@ ROW,104 SAY xGTotPhPcs PICTURE '9999999'
IF llCostAccs
  @ ROW,112 SAY xGTotPhAmt PICTURE '9999999.99'
ENDIF
ROW = ROW + 1
    
@ ROW,00 SAY REPLICATE('-',130)


*!*******************************************************************
*! PROCEDURE  : lpLADETY
*! DESC.      : Print the stock adjustment in case of long form, format 'A'
*!              and detail 'YES'
*! AUTH       : Ashraf Sherif Mohammad (ASH)
*! DATE       : 11/03/98
*!********************************************************************
PROCEDURE lpLADETY

SELECT (WORK)
GO TOP
XSTYLE = SUBSTR(Style,1,lnMajorLen)
SCAN
  IF ROW > 55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    ROW = 5
    @ ROW,00 SAY IIF(llMultiWH ,'WAREHS ' ,'')+'STYLE        COLOR  ' +;
                 SPACE(4)                                             +;
                 '   DIV    TY                  SZ1    SZ2    SZ3    SZ4' +;
                 '    SZ5    SZ6    SZ7    SZ8  TOTQTY  TOT. COST'

    ROW = ROW + 1
    @ ROW,00 SAY 'ADJUSTMENT REFERENCE    DATE     SESSION NO.'
    ROW = ROW + 1
  ENDIF

  IF SUBSTR(Style,1,lnMajorLen) <> XSTYLE
    @ ROW,00 SAY REPLICATE('.',132)
    ROW = ROW + 1
    @ ROW,000           SAY '*** ADJUSTMENTS SUBTOTAL *** ' + TRIM(XSTYLE)
    @ ROW,107+lnStrtCol SAY XSTOTPCS PICTURE '9999999'   

    IF llCostAccs
      @ ROW,115+lnStrtCol SAY XSTOTAMT PICTURE '9999999.99'
    ENDIF
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('.',132)
    XSTYLE   = SUBSTR(Style,1,lnMajorLen)
    XSTOTPCS  = 0
    XSTOTAMT  = 0
    ROW = ROW + 1
  ENDIF

  IF EOF() .OR. INKEY() = 32
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1

    @ ROW,000           SAY '*** ADJUSTMENTS GRAND TOTAL ***'
    @ ROW,107+lnStrtCol SAY XGTOTPCS PICTURE '9999999'       

    IF llCostAccs
      @ ROW,115+lnStrtCol SAY XGTOTAMT PICTURE '9999999.99'  
    ENDIF
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    EXIT
  ENDIF

*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*STYLE        COLOR  DYELOT #   DV TY                  SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY  TOT. COST
*ADJUSTMENT REFERENCE    DATE     SESSION NO
*123456789012 123456 XXXXXXXXXX 12 X EXISTING INVT: 123456 123456 123456 123456 123456 123456 123456 123456 1234567 1234567.99
*12345678901234567890123 99/99/99 XXXXXX ADJ. INVT:
*                                    NEW INVENTORY:


*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*WAREHS STYLE        COLOR  DYELOT #   DV TY                  SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY  TOT. COST
*ADJUSTMENT REFERENCE       DATE
*123456 123456789012 123456 XXXXXXXXXX 12 X EXISTING INVT: 123456 123456 123456 123456 123456 123456 123456 123456 1234567 1234567.99
*12345678901234567890123 99/99/99 XXXXXX        ADJ. INVT:
*                                           NEW INVENTORY:
*                                           FROM.. 123456:
*                                           TRANSFERED...:
*                                           TO.... 123456:


  IF XDETAILYN = 'Y'
    IF llMultiWH
      @ ROW,00 SAY IIF(EMPTY(lcWareCode), cWareCode, lcWareCode)
    ENDIF
    @ ROW,000+lnStrtCol SAY SUBSTR(STYLE,1,lnMajorLen)
    @ ROW,013+lnStrtCol SAY SUBSTR(Style,lnMajorLen+2,lnColorLen)
    @ ROW,027+lnStrtCol SAY STYLE.CDIVISION
    @ ROW,034+lnStrtCol SAY IIF(cTrType='2','P','A')
    @ ROW,036+lnStrtCol SAY 'EXISTING INVT:'
    @ ROW,051+lnStrtCol SAY OLDQTY1 PICTURE '@Z 999999'
    @ ROW,058+lnStrtCol SAY OLDQTY2 PICTURE '@Z 999999'
    @ ROW,065+lnStrtCol SAY OLDQTY3 PICTURE '@Z 999999'
    @ ROW,072+lnStrtCol SAY OLDQTY4 PICTURE '@Z 999999'
    @ ROW,079+lnStrtCol SAY OLDQTY5 PICTURE '@Z 999999'
    @ ROW,086+lnStrtCol SAY OLDQTY6 PICTURE '@Z 999999'
    @ ROW,093+lnStrtCol SAY OLDQTY7 PICTURE '@Z 999999'
    @ ROW,100+lnStrtCol SAY OLDQTY8 PICTURE '@Z 999999'
    @ ROW,107+lnStrtCol SAY TOTOLD  PICTURE '@Z 9999999'

    IF llCostAccs
      @ ROW,115+lnStrtCol SAY TOTOLD*IIF(XAVG_COST,nOLDCOST,STYLE.TOTCOST) ;
                          PICTURE '9999999.99'
    ENDIF
    ROW = ROW + 1
    @ ROW,000           SAY LEFT(REFERENCE,23)
    @ ROW,024           SAY dTrDATE
    @ ROW,033           SAY cSession
    @ ROW,040+lnStrtCol SAY 'ADJ. INVT:'
    @ ROW,051+lnStrtCol SAY nStk1   PICTURE '@Z 999999'
    @ ROW,058+lnStrtCol SAY nStk2   PICTURE '@Z 999999'
    @ ROW,065+lnStrtCol SAY nStk3   PICTURE '@Z 999999'
    @ ROW,072+lnStrtCol SAY nStk4   PICTURE '@Z 999999'
    @ ROW,079+lnStrtCol SAY nStk5   PICTURE '@Z 999999'
    @ ROW,086+lnStrtCol SAY nStk6   PICTURE '@Z 999999'
    @ ROW,093+lnStrtCol SAY nStk7   PICTURE '@Z 999999'
    @ ROW,100+lnStrtCol SAY nStk8   PICTURE '@Z 999999'
    @ ROW,107+lnStrtCol SAY nTOTStk PICTURE '@Z 9999999'
    IF llCostAccs
      @ ROW,115+lnStrtCol SAY nTOTStk*IIF(XAVG_COST,NCOST,STYLE->TOTCOST) ;
                          PICTURE '9999999.99'
    ENDIF
    ROW=ROW+1
    *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment.
    *IF cTrType='1'
    IF cTrType $ 'I1'              && ADJUSTMENT
    *E301248 AHM 06/03/99 (End)
      XNEWQTY1 = OLDQTY1 + nStk1
      XNEWQTY2 = OLDQTY2 + nStk2
      XNEWQTY3 = OLDQTY3 + nStk3
      XNEWQTY4 = OLDQTY4 + nStk4
      XNEWQTY5 = OLDQTY5 + nStk5
      XNEWQTY6 = OLDQTY6 + nStk6
      XNEWQTY7 = OLDQTY7 + nStk7
      XNEWQTY8 = OLDQTY8 + nStk8
      XTOTNEW  = TOTOLD  + nTOTStk
    ELSE
      XNEWQTY1 = nStk1
      XNEWQTY2 = nStk2
      XNEWQTY3 = nStk3
      XNEWQTY4 = nStk4
      XNEWQTY5 = nStk5
      XNEWQTY6 = nStk6
      XNEWQTY7 = nStk7
      XNEWQTY8 = nStk8
      XTOTNEW  = nTOTStk
    ENDIF
    @ ROW,036+lnStrtCol SAY 'NEW INVENTORY:'
    @ ROW,051+lnStrtCol SAY XNEWQTY1 PICTURE '@Z 999999'
    @ ROW,058+lnStrtCol SAY XNEWQTY2 PICTURE '@Z 999999'
    @ ROW,065+lnStrtCol SAY XNEWQTY3 PICTURE '@Z 999999'
    @ ROW,072+lnStrtCol SAY XNEWQTY4 PICTURE '@Z 999999'
    @ ROW,079+lnStrtCol SAY XNEWQTY5 PICTURE '@Z 999999'
    @ ROW,086+lnStrtCol SAY XNEWQTY6 PICTURE '@Z 999999'
    @ ROW,093+lnStrtCol SAY XNEWQTY7 PICTURE '@Z 999999'
    @ ROW,100+lnStrtCol SAY XNEWQTY8 PICTURE '@Z 999999'
    @ ROW,107+lnStrtCol SAY XTOTNEW  PICTURE '@Z 9999999'

    IF llCostAccs
      @ ROW,115+lnStrtCol SAY XTOTNEW*IIF(XAVG_COST,NCOST,STYLE->TOTCOST) ;
                          PICTURE '9999999.99'
    ENDIF
    ROW=ROW+1
  ENDIF

  *-- UPDATE THE SUBTOTALS
  *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment.
  *IF cTrType = '1'
  IF cTrType $ 'I1'                  && ADJUSTMENT
  *E301248 AHM 06/03/99 (End)
    XSTOTPCS  = XSTOTPCS + nTOTStk
    XSTOTAMT  = XSTOTAMT+(nTOTStk * IIF(XAVG_COST,NCOST,STYLE->TOTCOST))
    XGTOTPCS  = XGTOTPCS + nTOTStk
    XGTOTAMT  = XGTOTAMT+(nTOTStk*IIF(XAVG_COST,NCOST,STYLE->TOTCOST ))
  ENDIF
ENDSCAN

@ ROW,00 SAY REPLICATE('.',132)
ROW = ROW + 1
@ ROW,000           SAY '*** ADJUSTMENTS SUBTOTAL *** ' + TRIM(XSTYLE)
@ ROW,107+lnStrtCol SAY XSTOTPCS PICTURE '9999999'   
IF llCostAccs
  @ ROW,115+lnStrtCol SAY XSTOTAMT PICTURE '9999999.99'
ENDIF
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('.',132)
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',132)
ROW = ROW + 1
@ ROW,000           SAY '*** ADJUSTMENTS GRAND TOTAL ***'
@ ROW,107+lnStrtCol SAY XGTOTPCS PICTURE '9999999'       

IF llCostAccs
  @ ROW,115+lnStrtCol SAY XGTOTAMT PICTURE '9999999.99'  
ENDIF
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',132)

*!*******************************************************************
*! PROCEDURE  : lpBldTmp
*! DESC.      : Build the temporary file which calculate the existing 
*!              quantities and the average costs when the adjustment occurs
*! AUTH       : Ashraf Sherif Mohammad (ASH)
*! DATE       : 11/03/98
*!********************************************************************
PROCEDURE lpBldTmp

PRIVATE lnFileStru,lcBreak,C,I,lnAveCost,lcPA
DIMENSION laOldQty[9]
STORE 0 TO laOldQty,lnAveCost
lcBreak = SPACE(27)
lcPA = IIF(lcType='B','PA',lcType)

SELECT STYINVJL
SET ORDER TO STYINVJL 
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+10,4]
FOR C=1 TO 8
  I = STR(C,1)
  laFileStru [lnFileStru+C,1] = 'OLDQTY&I'
  laFileStru [lnFileStru+C,2] = 'N'
  laFileStru [lnFileStru+C,3] = 6
  laFileStru [lnFileStru+C,4] = 0
ENDFOR
laFileStru [lnFileStru+9,1] = 'TOTOLD'
laFileStru [lnFileStru+9,2] = 'N'
laFileStru [lnFileStru+9,3] = 7
laFileStru [lnFileStru+9,4] = 0
laFileStru [lnFileStru+10,1] = 'nOldCost'
laFileStru [lnFileStru+10,2] = 'N'
laFileStru [lnFileStru+10,3] = 9
laFileStru [lnFileStru+10,4] = 2

CREATE TABLE &WORK FROM ARRAY laFileStru
SELECT STYINVJL

SCAN FOR &XFILTER
  lnRecNo = RECNO()
  IF STYLE+cWareCode # lcBreak
    lcBreak = STYLE + cWareCode
    llPys = .T.
    IF cTrCode $ '29' AND cIRType='I'
      SKIP
      IF !EOF() AND STYLE+cWareCode = lcBreak AND cTrCode $ '29' AND cIRType='R'
        FOR C=1 TO 8
          I = STR(C,1)
          laOldQty[C] = nSTK&I
        ENDFOR
        laOldQty[9]   = nTotStk
        lnAveCost     = nCost
        llPys = .F.
        GOTO lnRecNo
      ELSE
        llPys = .T.
      ENDIF
    ENDIF  
    DO WHILE llPys
      IF cTrType $ '29' .AND. cIRType = 'R'
        FOR C=1 TO 8
          I = STR(C,1)
          laOldQty[C] = nSTK&I
        ENDFOR
        laOldQty[9]   = nTotStk
        lnAveCost     = nCost
        EXIT
      ELSE
        SKIP -1 
        IF BOF() OR STYLE+cWareCode # lcBreak
          STORE 0 TO laOldQty,lnAveCost
          GOTO lnRecNo
          EXIT
        ELSE
          LOOP
        ENDIF
      ENDIF
    ENDDO
    DO WHILE RECNO() # lnRecNo
      DO CASE
        *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment.
        *CASE cTrType = '1' 
        CASE cTrType $ 'I1' 
        *E301248 AHM 06/03/99 (End)
          FOR C=1 TO 8
            I = STR(C,1)
            laOldQty[C] = laOldQty[C] + NSTK&I
          ENDFOR
          laOldQty[9] = laOldQty[9] + nTotStk
          IF nTotStk > 0 AND laOldQty[9] < 0 .AND. (laOldQty[9]+nTotStk) > 0
            lnAveCost = nCost
          ENDIF
          IF nTotStk > 0 AND laOldQty[9] > 0 
            lnAveCost = (lnAveCost*(laOldQty[9]-nTotStk) + nCost*nTotStk) /laOldQty[9]
           ENDIF
        *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment.   
        *CASE !(cTrType $ '129') AND cIrType = 'R' 
        CASE !(cTrType $ 'I129') AND cIrType = 'R' 
        *E301248 AHM 06/03/99 (End)
          FOR C=1 TO 8
            I = STR(C,1)
            laOldQty[C] = laOldQty[C] + NSTK&I
          ENDFOR
          laOldQty[9] = laOldQty[9] + nTotStk
      
          IF nTotStk > 0 AND laOldQty[9] < 0 .AND. (laOldQty[9]+nTotStk) > 0
            lnAveCost = nCost
          ENDIF
          IF nTotStk > 0 AND laOldQty[9] > 0 
            lnAveCost = (lnAveCost*(laOldQty[9]-nTotStk) + nCost*nTotStk) /laOldQty[9]
          ENDIF
    
        *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment.
        *CASE !(cTrType $ '129') AND cIrType = 'I' 
        CASE !(cTrType $ 'I129') AND cIrType = 'I' 
        *E301248 AHM 06/03/99 (End)
          FOR C=1 TO 8
            I = STR(C,1)
            laOldQty[C] = laOldQty[C] - NSTK&I
          ENDFOR
          laOldQty[9] = laOldQty[9] - nTotStk
      ENDCASE
      SKIP
    ENDDO
  ENDIF   && STYLE+cWareCode # lcBreak
  SCATTER MEMVAR MEMO
  DO CASE
    *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment.
    *CASE cTrType = '1' 
    CASE cTrType $ 'I1' 
    *E301248 AHM 06/03/99 (End)
      FOR C=1 TO 8
        I = STR(C,1)
        m.OldQty&I  = laOldQty[C]
        laOldQty[C] = laOldQty[C] + NSTK&I
      ENDFOR
      m.TotOld    = laOldQty[9]
      laOldQty[9] = laOldQty[9] + nTotStk
      m.nOldCost  = lnAveCost
      IF nTotStk > 0 AND laOldQty[9] < 0 .AND. (laOldQty[9]+nTotStk) > 0
        lnAveCost = nCost
      ENDIF
      IF nTotStk > 0 AND laOldQty[9] > 0 
        lnAveCost = (lnAveCost*(laOldQty[9]-nTotStk) + nCost*nTotStk) /laOldQty[9]
      ENDIF
      IF 'A' $ lcPA
        INSERT INTO &WORK FROM MEMVAR
      ENDIF
    CASE cTrType $ '29' AND cIrType = 'R' 
      FOR C=1 TO 8
        I = STR(C,1)
        m.OldQty&I  = laOldQty[C]
        laOldQty[C] = NSTK&I
      ENDFOR
      m.TotOld    = laOldQty[9]
      laOldQty[9] = nTotStk
      m.nOldCost  = lnAveCost
      lnAveCost   = nCost

      IF 'P' $ lcPA AND cTrType='2'
        INSERT INTO &WORK FROM MEMVAR
      ENDIF
      
    *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment.
    *CASE !(cTrType $ '129') AND cIrType = 'R' 
    CASE !(cTrType $ 'I129') AND cIrType = 'R' 
    *E301248 AHM 06/03/99 (End)
      FOR C=1 TO 8
        I = STR(C,1)
        laOldQty[C] = laOldQty[C] + NSTK&I
      ENDFOR
      laOldQty[9] = laOldQty[9] + nTotStk
      IF nTotStk > 0 AND laOldQty[9] < 0 .AND. (laOldQty[9]+nTotStk) > 0
        lnAveCost = nCost
      ENDIF
      IF nTotStk > 0 AND laOldQty[9] > 0 
        lnAveCost = (lnAveCost*(laOldQty[9]-nTotStk) + nCost*nTotStk) /laOldQty[9]
      ENDIF

    *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment. 
    *CASE !(cTrType $ '129') AND cIrType = 'I' 
    CASE !(cTrType $ 'I129') AND cIrType = 'I' 
    *E301248 AHM 06/03/99 (End)
      FOR C=1 TO 8
        I = STR(C,1)
        laOldQty[C] = laOldQty[C] - NSTK&I
      ENDFOR
      laOldQty[9] = laOldQty[9] - nTotStk
  ENDCASE
ENDSCAN

SET RELATION TO
SELECT(WORK)
GO TOP
IF EOF()
  **No records selected.
  =gfModalGen('TRM42153B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN .F.
ELSE
  INDEX ON STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE TAG &WORK
  SET RELATION TO STYLE INTO STYLE ADDITIVE
ENDIF


*!*******************************************************************
*! PROCEDURE  : lpFormSh
*! DESC.      : Print the stock adjustment when FORM is short
*! AUTH       : Ashraf Sherif Mohammad (ASH)
*! DATE       : 11/03/98
*!********************************************************************
PROCEDURE lpFormSh

DO CASE
  CASE EMPTY(lcType) OR 'PA' $ lcType
    *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment.
    *XFilter = XFilter + " .AND. (cTrType='1' OR (cTrType = '2' AND cIRType='R'))"
    XFilter = XFilter + " .AND. (cTrType $ 'I1' OR (cTrType = '2' AND cIRType='R'))"
    *E301248 AHM 06/03/99 (End)
  CASE lcType='A'
    *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment.
    *XFilter = XFilter + " .AND. cTrType $ '1'"
    XFilter = XFilter + " .AND. cTrType $ 'I1'"
    *E301248 AHM 06/03/99 (End)
  CASE lcType='P'
    XFilter = XFilter + " .AND. (cTrType = '2' AND cIRType='R')"
ENDCASE
LOCATE FOR &xFilter

IF EOF()
  **No records selected.
  =gfModalGen('TRM42153B00000','DIALOG' )
  SET DEVICE TO SCREEN
  llGoOut = .T.
  RETURN
ENDIF

SCAN FOR &XFILTER        && SHORT FORM THE OLD ONE
  IF ROW > 55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH  XREPORT,XTITLE,R_WIDTH
    ROW = 5
    @ ROW,00 SAY IIF(llMultiWH ,'WAREHS ' ,'')+'STYLE        COLOR  '  +;
                 SPACE(4)                                              +;
                 '   REFERENCE    TY DIV      SZ1   SZ2   SZ3   SZ4'   +;
                 '   SZ5   SZ6   SZ7   SZ8  TOTQTY  TOT. COST DATE'
    ROW = ROW + 1
  ENDIF

  IF SUBSTR(Style,1,lnMajorLen) <> XSTYLE
    @ ROW,00 SAY REPLICATE('.',132)
    ROW = ROW + 1
    @ ROW,000           SAY '*** ADJUSTMENTS  SUBTOTAL *** ' + TRIM(XSTYLE)
    @ ROW,098+lnStrtCol SAY XSTOTPCS PICTURE '9999999'      
    IF llCostAccs
      @ ROW,106+lnStrtCol SAY XSTOTAMT PICTURE '9999999.99' 
    ENDIF
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('.',132)
    ROW = ROW + 1
    XSTYLE   = SUBSTR(Style,1,lnMajorLen)
    XSTOTPCS = 0
    XSTOTAMT = 0
  ENDIF

  IF EOF() .OR. INKEY() = 32
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    @ ROW,000 SAY '*** ADJUSTMENTS  GRAND TOTAL ***'
    @ ROW,098+lnStrtCol SAY XGTOTPCS PICTURE '9999999' 
    IF llCostAccs
      @ ROW,106+lnStrtCol SAY XGTOTAMT PICTURE '9999999.99' 
    ENDIF
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    EXIT
  ENDIF

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*WAREHS STYLE        COLOR  DYELOT #   REFERENCE    TY DV   SZ1   SZ2   SZ3   SZ4   SZ5   SZ6   SZ7   SZ8  TOTQTY  TOT. COST DATE
*123456 123456789012 123456 XXXXXXXXXX 1234567890123 A 12 12345 12345 12345 12345 12345 12345 12345 12345 1234567 1234567.99 99/99/99
*                                                    P

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*STYLE        COLOR  DYELOT #   REFERENCE    TY DV   SZ1   SZ2   SZ3   SZ4   SZ5   SZ6   SZ7   SZ8  TOTQTY  TOT. COST DATE
*123456789012 123456 XXXXXXXXXX 1234567890123 A 12 12345 12345 12345 12345 12345 12345 12345 12345 1234567 1234567.99 99/99/99
*                                                    P

  IF XDETAILYN = 'Y'
    IF llMultiWH
      @ ROW,00 SAY IIF(EMPTY(lcWareCode), cWareCode, lcWareCode)
    ENDIF
    @ ROW,000+lnStrtCol SAY SUBSTR(STYLE,1,lnMajorLen)
    @ ROW,013+lnStrtCol SAY SUBSTR(Style,lnMajorLen+2,lnColorLen)
    @ ROW,027+lnStrtCol SAY SUBSTR(REFERENCE,1,13)
    *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment.
    *@ ROW,041+lnStrtCol SAY IIF(cTrType = '2','P',IIF(cTrType='1','A',''))
    @ ROW,041+lnStrtCol SAY IIF(cTrType = '2','P',IIF(cTrType$'I1','A',''))
    *E301248 AHM 06/03/99 (End)
    
    @ ROW,043+lnStrtCol SAY STYLE.CDIVISION  
    @ ROW,050+lnStrtCol SAY nStk1   PICTURE '@Z 99999'
    @ ROW,056+lnStrtCol SAY nStk2   PICTURE '@Z 99999'
    @ ROW,062+lnStrtCol SAY nStk3   PICTURE '@Z 99999'
    @ ROW,068+lnStrtCol SAY nStk4   PICTURE '@Z 99999'
    @ ROW,074+lnStrtCol SAY nStk5   PICTURE '@Z 99999'
    @ ROW,080+lnStrtCol SAY nStk6   PICTURE '@Z 99999'
    @ ROW,086+lnStrtCol SAY nStk7   PICTURE '@Z 99999'
    @ ROW,092+lnStrtCol SAY nStk8   PICTURE '@Z 99999'
    @ ROW,098+lnStrtCol SAY nTOTStk PICTURE '@Z 9999999'
    IF llCostAccs
      @ ROW,106+lnStrtCol SAY nTOTStk*IIF(XAVG_COST,NCOST,STYLE->TOTCOST) ;
                          PICTURE '9999999.99'
    ENDIF
    @ ROW,117+lnStrtCol SAY dTrDATE    
    ROW = ROW + 1
  ENDIF

  *-- UPDATE THE SUBTOTALS
  *E301248 AHM 06/03/99 (Begin) Add tran. type 'I' as adjustment.
  *IF cTRType = '1'
  IF cTRType $ 'I1'
  *E301248 AHM 06/03/99 (End)
    XSTOTPCS = XSTOTPCS + nTOTSTK
    XSTOTAMT = XSTOTAMT + (nTOTStk*IIF(XAVG_COST,NCOST,STYLE->TOTCOST))
    XGTOTPCS = XGTOTPCS + nTOTSTK
    XGTOTAMT = XGTOTAMT + (nTOTSTK*IIF(XAVG_COST,NCOST,STYLE->TOTCOST))
  ENDIF
ENDSCAN
@ ROW,00 SAY REPLICATE('.',132)
ROW = ROW + 1
@ ROW,000           SAY '*** ADJUSTMENTS  SUBTOTAL *** ' + TRIM(XSTYLE)
@ ROW,098+lnStrtCol SAY XSTOTPCS PICTURE '9999999'      
IF llCostAccs
  @ ROW,106+lnStrtCol SAY XSTOTAMT PICTURE '9999999.99' 
ENDIF
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('.',132)
ROW = ROW + 1

@ ROW,00 SAY REPLICATE('-',132)
ROW = ROW + 1
@ ROW,000 SAY '*** ADJUSTMENTS  GRAND TOTAL ***'
@ ROW,098+lnStrtCol SAY XGTOTPCS PICTURE '9999999'     
IF llCostAccs
  @ ROW,106+lnStrtCol SAY XGTOTAMT PICTURE '9999999.99' 
ENDIF
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',132)

*!*******************************************************************
*! PROCEDURE  : lpLADETN
*! DESC.      : Print the stock adjustment when FORM is long, format 'A'
*!              and detail is 'NO'
*! AUTH       : Ashraf Sherif Mohammad. (ASH)
*! DATE       : 11/03/98
*!********************************************************************
PROCEDURE  lpLADETN

SELECT INVTADJ
SET RELATION TO STYLE INTO STYLE ADDITIVE
SCAN FOR &XFILTER
  IF !llMultiWH .AND. Type = 'T'
    SKIP
    LOOP
  ENDIF
  IF ROW > 55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    ROW = 5
    @ ROW,00 SAY IIF(llMultiWH ,'WAREHS ' ,'')+'STYLE        COLOR  ' +;
                 IIF(XDYELOT_S,'DYELOT #',SPACE(8))                   +;
                 '   DV TY                  SZ1    SZ2    SZ3    SZ4' +;
                 '    SZ5    SZ6    SZ7    SZ8  TOTQTY  TOT. COST'


    ROW = ROW + 1
    @ ROW,00 SAY 'ADJUSTMENT REFERENCE       DATE'
    ROW = ROW + 1
  ENDIF

  IF SUBSTR(Style,1,lnMajorLen) <> XSTYLE
    @ ROW,00 SAY REPLICATE('.',132)
    ROW = ROW + 1
    @ ROW,000           SAY '*** ADJUSTMENTS SUBTOTAL *** ' + TRIM(XSTYLE)
    @ ROW,107+lnStrtCol SAY XSTOTPCS PICTURE '9999999'   

    IF llCostAccs
      @ ROW,115+lnStrtCol SAY XSTOTAMT PICTURE '9999999.99'
    ENDIF
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('.',132)
    ROW = ROW + 1
    XSTYLE   = SUBSTR(Style,1,lnMajorLen)
    XSTOTPCS  = 0
    XSTOTAMT  = 0
  ENDIF

  IF EOF() .OR. INKEY() = 32
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    @ ROW,000           SAY '*** ADJUSTMENTS GRAND TOTAL ***'
    @ ROW,107+lnStrtCol SAY XGTOTPCS PICTURE '9999999'       
    IF llCostAccs
      @ ROW,115+lnStrtCol SAY XGTOTAMT PICTURE '9999999.99'  
    ENDIF
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    EXIT
  ENDIF

*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*STYLE        COLOR  DYELOT #   DV TY                  SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY  TOT. COST
*ADJUSTMENT REFERENCE       DATE
*123456789012 123456 XXXXXXXXXX 12 X EXISTING INVT: 123456 123456 123456 123456 123456 123456 123456 123456 1234567 1234567.99
*1234567890123456789012345  99/99/99 ADJUSTED INVT:
*                                    NEW INVENTORY:


*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*WAREHS STYLE        COLOR  DYELOT #   DV TY                  SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY  TOT. COST
*ADJUSTMENT REFERENCE       DATE
*123456 123456789012 123456 XXXXXXXXXX 12 X EXISTING INVT: 123456 123456 123456 123456 123456 123456 123456 123456 1234567 1234567.99
*1234567890123456789012345  99/99/99        ADJUSTED INVT:
*                                           NEW INVENTORY:
*                                           FROM.. 123456:
*                                           TRANSFERED...:
*                                           TO.... 123456:

  *-- UPDATE THE SUBTOTALS
  IF TYPE = 'A'                          && SUB TOTAL ON ADJUSTMENTS ONLY
    XSTOTPCS  = XSTOTPCS + TOTADJ
    XSTOTAMT  = XSTOTAMT+(TOTADJ * IIF(XAVG_COST,UNT_COST,STYLE->TOTCOST))
    XGTOTPCS  = XGTOTPCS + TOTADJ
    XGTOTAMT  = XGTOTAMT+(TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE->TOTCOST ))
  ENDIF
ENDSCAN

@ ROW,00 SAY REPLICATE('.',132)
ROW = ROW + 1
@ ROW,000           SAY '*** ADJUSTMENTS SUBTOTAL *** ' + TRIM(XSTYLE)
@ ROW,107+lnStrtCol SAY XSTOTPCS PICTURE '9999999'    
IF llCostAccs
  @ ROW,115+lnStrtCol SAY XSTOTAMT PICTURE '9999999.99'
ENDIF
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('.',132)
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',132)
ROW = ROW + 1
@ ROW,000           SAY '*** ADJUSTMENTS GRAND TOTAL ***'
@ ROW,107+lnStrtCol SAY XGTOTPCS PICTURE '9999999'       
IF llCostAccs
  @ ROW,115+lnStrtCol SAY XGTOTAMT PICTURE '9999999.99'  
ENDIF
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',132)

*!*******************************************************************
*! PROCEDURE  : lpLBDETN
*! DESC.      : Print the stock adjustment when FORM is long, format 'B'
*!              and detail is 'NO'
*! AUTH       : Ashraf Sherif Mohammad (ASH)
*! DATE       : 11/03/98
*!********************************************************************
PROCEDURE lpLBDETN

SELECT INVTADJ
SET RELATION TO STYLE INTO STYLE ADDITIVE
SCAN FOR &XFILTER
  IF !llMultiWH .AND. Type = 'T'
    SKIP
    LOOP
  ENDIF

  IF ROW > 55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH  XREPORT,XTITLE,R_WIDTH
    ROW = 5
    @ ROW,01 SAY 'STYLE        COLOR     DV TYP                     SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY TOT. COST  DATE'
    ROW = ROW + 1
  ENDIF

  IF SUBSTR(Style,1,lnMajorLen) <> XSTYLE
    @ ROW,00 SAY REPLICATE('.',130)
    ROW = ROW + 1
    @ ROW,000 SAY '*** ADJUSTMENTS  SUBTOTAL ***'+TRIM(XSTYLE)
    @ ROW,104 SAY XSTOTPCS PICTURE '9999999'
    IF llCostAccs
      @ ROW,112 SAY XSTOTAMT PICTURE '9999999.99'
    ENDIF

    ROW = ROW + 1
    @ ROW,000 SAY '***   PHYSICAL SUBTOTAL   ***'+TRIM(XSTYLE)
    @ ROW,104 SAY XSTOTPhPCS PICTURE '9999999'
    IF llCostAccs
      @ ROW,112 SAY XSTOTPhAMT PICTURE '9999999.99'
    ENDIF
    
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('.',130)
    ROW = ROW + 1
    XSTYLE   = SUBSTR(Style,1,lnMajorLen)
    XSTOTPCS  = 0
    XSTOTAMT  = 0
    XSTOTPhPCS  = 0
    XSTOTPhAMT  = 0
  ENDIF

  IF EOF() .OR. INKEY() = 32
    @ ROW,00 SAY REPLICATE('-',130)
    ROW = ROW + 1
    @ ROW,000 SAY '*** ADJUSTMENTS  GRAND TOTAL ***'

    @ ROW,104 SAY XGTOTPCS PICTURE '9999999'
    IF llCostAccs
      @ ROW,112 SAY XGTOTAMT PICTURE '9999999.99'
    ENDIF
    
    ROW = ROW + 1
    @ ROW,000 SAY '***   PHYSICAL GRAND TOTAL   ***'
    @ ROW,104 SAY xGTotPhPcs PICTURE '9999999'
    IF llCostAccs
      @ ROW,112 SAY xGTotPhAmt PICTURE '9999999.99'
    ENDIF
    ROW = ROW + 1
    
    @ ROW,00 SAY REPLICATE('-',130)
    ROW = ROW + 1
    EXIT
  ENDIF

*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
*STYLE   CLR DYELOT #   DV TYP                     SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY TOT. COST  DATE
*ADJUSTMENT REFERENCE
*1234567 123 XXXXXXXXXX 12 XXX EXISTING INVT:-> 123456 123456 123456 123456 123456 123456 123456 123456 1234567 1234567.99 99/99/99
*1234567890123456789012345     ADJUSTED INVT:->
*                              NEW INVENTORY:->
  IF XDETAILYN = 'Y'
    @ ROW,001 SAY SUBSTR(STYLE,1,lnMajorLen)
    @ ROW,014 SAY SUBSTR(Style,lnMajorLen+2,lnColorLen)
    @ ROW,024 SAY STYLE.CDIVISION
    @ ROW,027 SAY IIF(TYPE='A','ADJ','PHY')
    @ ROW,031 SAY 'EXISTING INVT:->'
    @ ROW,048 SAY OLDQTY1 PICTURE '@Z 999999'
    @ ROW,055 SAY OLDQTY2 PICTURE '@Z 999999'
    @ ROW,062 SAY OLDQTY3 PICTURE '@Z 999999'
    @ ROW,069 SAY OLDQTY4 PICTURE '@Z 999999'
    @ ROW,076 SAY OLDQTY5 PICTURE '@Z 999999'
    @ ROW,083 SAY OLDQTY6 PICTURE '@Z 999999'
    @ ROW,090 SAY OLDQTY7 PICTURE '@Z 999999'
    @ ROW,097 SAY OLDQTY8 PICTURE '@Z 999999'

    @ ROW,104 SAY TOTOLD PICTURE '@Z 9999999'
    IF llCostAccs
      @ ROW,112 SAY TOTOLD*IIF(XAVG_COST,OLD_COST,STYLE->TOTCOST) ;
                    PICTURE '9999999.99'
    ENDIF
    ROW = ROW + 1
    @ ROW,01 SAY REASON
    @ ROW,031 SAY 'ADJUSTED INVT:->'

    IF TYPE = 'P'
      @ ROW,048 SAY ADJ1-OLDQTY1   PICTURE '@Z 999999'
      @ ROW,055 SAY ADJ2-OLDQTY2   PICTURE '@Z 999999'
      @ ROW,062 SAY ADJ3-OLDQTY3   PICTURE '@Z 999999'
      @ ROW,069 SAY ADJ4-OLDQTY4   PICTURE '@Z 999999'
      @ ROW,076 SAY ADJ5-OLDQTY5   PICTURE '@Z 999999'
      @ ROW,083 SAY ADJ6-OLDQTY6   PICTURE '@Z 999999'
      @ ROW,090 SAY ADJ7-OLDQTY7   PICTURE '@Z 999999'
      @ ROW,097 SAY ADJ8-OLDQTY8   PICTURE '@Z 999999'
      @ ROW,104 SAY TOTADJ-TOTOLD PICTURE '@Z 9999999'
      IF llCostAccs
        @ ROW,112 SAY (TOTADJ-TOTOLD)*IIF(XAVG_COST,UNT_COST,STYLE->TOTCOST) ;
                      PICTURE '9999999.99'
      ENDIF
    ELSE
      @ ROW,048 SAY ADJ1   PICTURE '@Z 999999'
      @ ROW,055 SAY ADJ2   PICTURE '@Z 999999'
      @ ROW,062 SAY ADJ3   PICTURE '@Z 999999'
      @ ROW,069 SAY ADJ4   PICTURE '@Z 999999'
      @ ROW,076 SAY ADJ5   PICTURE '@Z 999999'
      @ ROW,083 SAY ADJ6   PICTURE '@Z 999999'
      @ ROW,090 SAY ADJ7   PICTURE '@Z 999999'
      @ ROW,097 SAY ADJ8    PICTURE '@Z 999999'
      @ ROW,104 SAY TOTADJ PICTURE '@Z 9999999'
      IF llCostAccs
        @ ROW,112 SAY TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE->TOTCOST) ;
                      PICTURE '9999999.99'
      ENDIF
    ENDIF
    @ ROW,123 SAY DATE
    ROW=ROW+1

    IF TYPE='A'        && ADJUSTMENT
      XNEWQTY1 = OLDQTY1 + ADJ1
      XNEWQTY2 = OLDQTY2 + ADJ2
      XNEWQTY3 = OLDQTY3 + ADJ3
      XNEWQTY4 = OLDQTY4 + ADJ4
      XNEWQTY5 = OLDQTY5 + ADJ5
      XNEWQTY6 = OLDQTY6 + ADJ6
      XNEWQTY7 = OLDQTY7 + ADJ7
      XNEWQTY8 = OLDQTY8 + ADJ8
      XTOTNEW  = TOTOLD  + TOTADJ
    ELSE
      XNEWQTY1 = ADJ1
      XNEWQTY2 = ADJ2
      XNEWQTY3 = ADJ3
      XNEWQTY4 = ADJ4
      XNEWQTY5 = ADJ5
      XNEWQTY6 = ADJ6
      XNEWQTY7 = ADJ7
      XNEWQTY8 = ADJ8
      XTOTNEW  = TOTADJ
    ENDIF

    @ ROW,031 SAY 'NEW INVENTORY:->'
    @ ROW,048 SAY XNEWQTY1 PICTURE '@Z 999999'
    @ ROW,055 SAY XNEWQTY2 PICTURE '@Z 999999'
    @ ROW,062 SAY XNEWQTY3 PICTURE '@Z 999999'
    @ ROW,069 SAY XNEWQTY4 PICTURE '@Z 999999'
    @ ROW,076 SAY XNEWQTY5 PICTURE '@Z 999999'
    @ ROW,083 SAY XNEWQTY6 PICTURE '@Z 999999'
    @ ROW,090 SAY XNEWQTY7 PICTURE '@Z 999999'
    @ ROW,097 SAY XNEWQTY8 PICTURE '@Z 999999'
    @ ROW,104 SAY XTOTNEW PICTURE '@Z 9999999'
    IF llCostAccs
      @ ROW,112 SAY ( XTotNew * IIF( XAvg_Cost, ;
                      IIF(Type='A',STYLE->Ave_Cost,Unt_Cost), ;
                      STYLE->TotCost) ) PICTURE '9999999.99'   
    ENDIF
    ROW=ROW+1
  ENDIF

  * UPDATE THE SUBTOTALS
  
  IF TYPE='A'           && SUB TOTAL ON ADJUSTMENTS ONLY
    XSTOTPCS  = XSTOTPCS + TOTADJ
    XSTOTAMT  = XSTOTAMT + (TOTADJ * IIF(XAVG_COST,UNT_COST,STYLE->TOTCOST))
    XGTOTPCS  = XGTOTPCS + TOTADJ
    XGTOTAMT  = XGTOTAMT + (TOTADJ*IIF(XAVG_COST,UNT_COST,STYLE->TOTCOST )) 
  ELSE
    xSTotPhPcs  = xSTotPhPcs + TOTADJ
    xSTotPhAmt  = xSTotPhAmt + TOTADJ * IIF(XAVG_COST,UNT_COST,STYLE->TOTCOST)
    xGTotPhPcs  = xGTotPhPcs + TOTADJ
    xGTotPhAmt  = xGTotPhAmt + TOTADJ * IIF(XAVG_COST,UNT_COST,STYLE->TOTCOST)
  ENDIF
ENDSCAN
@ ROW,00 SAY REPLICATE('.',130)
ROW = ROW + 1
@ ROW,000 SAY '*** ADJUSTMENTS  SUBTOTAL ***'+TRIM(XSTYLE)
@ ROW,104 SAY XSTOTPCS PICTURE '9999999'
IF llCostAccs
  @ ROW,112 SAY XSTOTAMT PICTURE '9999999.99'
ENDIF
ROW = ROW + 1
@ ROW,000 SAY '***   PHYSICAL SUBTOTAL   ***'+TRIM(XSTYLE)
@ ROW,104 SAY XSTOTPhPCS PICTURE '9999999'
IF llCostAccs
  @ ROW,112 SAY XSTOTPhAMT PICTURE '9999999.99'
ENDIF
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('.',130)
ROW = ROW + 1

@ ROW,00 SAY REPLICATE('-',130)
ROW = ROW + 1
@ ROW,000 SAY '*** ADJUSTMENTS  GRAND TOTAL ***'
@ ROW,104 SAY XGTOTPCS PICTURE '9999999'
IF llCostAccs
  @ ROW,112 SAY XGTOTAMT PICTURE '9999999.99'
ENDIF
ROW = ROW + 1
@ ROW,000 SAY '***   PHYSICAL GRAND TOTAL   ***'
@ ROW,104 SAY xGTotPhPcs PICTURE '9999999'
IF llCostAccs
  @ ROW,112 SAY xGTotPhAmt PICTURE '9999999.99'
ENDIF
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',130)


*!*************************************************************
*! Name      : lfWOpGrid
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 11/03/98
*! Purpose   : Function in the when of the OG.
*:*************************************************************
*! Example     : = lfWOpGrid()
*!*************************************************************

FUNCTION lfWOpGrid
R_WIDTH    = 'W'
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYINVJL.DTRDATE'),1)
IF EMPTY(laOGFxFlt[lnDatePos,6])
  laOGFxFlt[lnDatePos,6] = DTOC(DATE())+'|'+DTOC(DATE())
ENDIF


*!*************************************************************
*! Name      : lfvStyle
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 11/03/98
*! Purpose   : Valid function of the Style
*!*************************************************************
*! Called from : Option grid [Style Get field]
*!*************************************************************
*! Calls       : gfStyBrw()
*!*************************************************************
FUNCTION lfvStyle
PRIVATE lnAlias,lcStyOrder

lnAlias = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 

lcObjName = SYS(18)
lcObjVal  = EVALUATE(SYS(18))

IF !EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE')
  lcObjVal = gfStyBrw('M',"","",.F.)
  &lcObjName = lcObjVal
ENDIF

SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvLoc
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 11/03/98
*! Purpose   : Valid function of the Location.
*!*************************************************************
*! Called from : Option grid [Location Get field]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************

FUNCTION lfvLoc
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Warehouse he entered is not in the
*file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfWoldVal
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 11/03/98
*! Purpose   : To return the old value.
*:*************************************************************
*! Example     : = lfWoldVal()
*!*************************************************************

FUNCTION lfWOldVal
laOldVal = EVALUATE(SYS(18))


*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 11/03/98
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************

FUNCTION lfEvalSegs

lnMajSeg   = gfItemMask('SM')  && No. of major segments.
lcMajPict  = gfItemMask("PM")

*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.      
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style code Structure. [End]

lcStyGrp = lcStyMajor + ' Group' 

RETURN ''


*!*************************************************************
*! Name      : lfVRepF
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 11/03/98
*! Purpose   : Valid function of the report format.
*!*************************************************************
*! Called from : Option grid [Report format Get field]
*!*************************************************************
*! Calls       :
*!*************************************************************

FUNCTION lfVRepF
CLEAR READ

*!*************************************************************
*! Name      : lfVRSess
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 11/03/98
*! Purpose   : Valid function of the Use session number.
*!*************************************************************
*! Called from : Option grid [Use seesion number question]
*!*************************************************************
*! Calls       :
*!*************************************************************

FUNCTION lfVRSess
CLEAR READ
