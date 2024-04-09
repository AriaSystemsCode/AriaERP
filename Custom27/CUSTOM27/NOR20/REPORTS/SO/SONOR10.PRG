*:************************************************************************
*: Program file  : SONOR10.PRG
*: Program desc. : Custom order detail report for North Port.
*: Module: Aria Apparel Series.
*:Developer : Ahmed Abdel Naby (AAN)
*:Date : 03/22/2001
*: C102205 Refer to.
*:************************************************************************
*B804147,1 AAN 19/04/2001 Fix bug that prevents printing  order line with zero qty.
*:************************************************************************
*-- Rework C102205,4 Call a function lfEvalSegs from prg not from OG [Begin].
= lfEvalSegs()
*-- Rework C102205,4 Call a function lfEvalSegs from prg not from OG [Begin].
STORE "A" TO lcShip
DIMENSION laNotAvl(8)
STORE "" TO laNotAvl
lcTempLine = GFTEMPNAME()
lcRpExp = lcRpExp + ' AND ORDHDR.STATUS $ lcRpStatus ' + " .AND. !PICKED"
*--CREATE WORK FILE
SELECT ORDLINE
COPY STRUCTURE TO (gcWorkDir+'&lcTempLine')
=gfOpenFile(gcWorkDir+'&lcTempLine','','EX')

DIMENSION XTOTAL(3,10)
DIMENSION CLRTOT(10)
DIMENSION ORDTOT(10)

R_WIDTH  = 'W'          && STANDARD REPORT 'WIDE'

LFCR = CHR(10) + CHR(13)
SAVSORT   = ' '

*------------------------------------------------------
* BUILD TEMPORARY LINE ITEM FILE *** new
*------------------------------------------------------
SELECT ORDHDR
SET ORDER TO TAG ORDHDR

SELECT (lcTempLine)
SELECT ORDLINE
SET RELATION TO 'O'+ORDER INTO ORDHDR
SET RELATION TO STYLE INTO STYLE ADDITIVE
SCAN FOR &lcRpExp 
  WAIT WINDOW "Printing sales order : " + Order NOWAIT
  *--B804147,1 AAN Preventing print the order have zero qty  [Begin]
  *SCATTER MEMVAR MEMO
  *INSERT INTO (lcTempLine) FROM MEMVAR
  IF TotQty>0
    SCATTER MEMVAR MEMO
    INSERT INTO (lcTempLine) FROM MEMVAR
  ENDIF
  *--B804147,1 AAN Preventing print the order have zero qty  [End]
ENDSCAN
SELECT (lcTempLine)
SET RELATION TO 'O'+ORDER INTO ORDHDR
SET RELATION TO STYLE  INTO STYLE ADDITIVE
SELECT (lcTempLine)
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF

IF llRpChkAvb
  SELECT DISTINCT STYLE.STYLE, STYLE.Stk1, STYLE.Stk2, STYLE.Stk3, STYLE.Stk4,;
                  STYLE.Stk5, STYLE.Stk6, STYLE.Stk7, STYLE.Stk8, STYLE.ALO1,STYLE.ALO2,;
                  STYLE.ALO3,STYLE.ALO4,STYLE.ALO5,STYLE.ALO6,STYLE.ALO7,STYLE.ALO8;
  WHERE STYLE.STYLE = &lcTempLine..STYLE ;
  FROM  STYLE, &lcTempLine;
  INTO TABLE (gcWorkDir + 'lcStyAvl')
  SELECT lcStyAvl
  INDEX ON STYLE TAG lcStyAvl
ENDIF
*------------------ END BUILD TEMPORARY LINE ITEM FILE -------------------
*------ SET UP RELATIONS --------*
SELECT (lcTempLine)
IF LcRpSortBy='S' .AND. !(llRpSummMt)
  SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
ELSE
  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER  ADDITIVE
ENDIF
IF llRpChkAvb
  SET RELATION TO STYLE INTO lcStyAvl ADDITIVE
ENDIF
LOCATE

*-----------------------------------------------------------
* [BREAK] SET UP SORT FILEDS & CONTROL BREAKS
*-----------------------------------------------------------
XSORT = ' '
DO CASE
  * ACCOUNT NUMBER SEQUENCE
  CASE LcRpSortBy='A'
    XSORT  = 'ACCOUNT+ORDER+STYLE+STR(LINENO,6)'
    BREAK  = 'CUSTOMER.ACCOUNT + SPACE(3) + CUSTOMER.BTNAME + LFCR + CUSTOMER.PHONE1'
    CTLSEQ = 'ACCOUNT NUMBER'

  * ORDER NUMBER SEQUENCE
  CASE LcRpSortBy='O'
    XSORT  = 'ORDER+STYLE+STR(LINENO,6)'
    BREAK  = 'ORDER + SPACE(3) + ACCOUNT' && ACCOUNT IS INCLUDED HERE
                                          && ONLY TO DISPLAY ON SUBTOTAL.
                                          && IT IS NEEDED TO BREAK
    CTLSEQ = 'ORDER'

  * FABRIC GROUP
  CASE LcRpSortBy='F'
    XSORT  = 'STYLE.FABRIC+STYLE+ORDER+STR(LINENO,6)'
    BREAK  = 'STYLE.FABRIC'
    CTLSEQ = 'PRIMARY FABRIC'

  * STYLE/COLOR
  CASE LcRpSortBy = 'S'
    XSORT    = 'STYLE+DTOS(ORDHDR.COMPLETE)+ORDER+STR(LINENO,6)'
    BREAK    = ' '
    CTLSEQ   = 'STYLE/COLOR'

  CASE LcRpSortBy = 'D'
    XSORT  = 'DTOS(ORDHDR.COMPLETE)+ORDER+STR(LINENO,6)'
    BREAK  = 'DTOC(ORDHDR.COMPLETE)'
    CTLSEQ = 'COMPLETION DATE'

  CASE LcRpSortBy = 'R'
    XSORT  = 'ORDHDR.REP1+STYLE+ORDER+STR(LINENO,6)'
    BREAK  = 'ORDHDR.REP1'
    CTLSEQ = 'SALESREP'

  CASE LcRpSortBy = 'P'
    XSORT = 'ORDHDR.PRIORITY + ORDHDR.ORDER'
    BREAK = 'ORDHDR.PRIORITY'
    CTLSEQ = 'PRIORITY'
ENDCASE
*----------------------------------------------
* SORT TEMPORARY FILE
*----------------------------------------------

SELECT (lcTempLine)
IF XSORT<>' '
  IF XSORT <> SAVSORT
    SAVSORT = XSORT
    Z = LTRIM(STR(RECCOUNT(),7))
    WAIT WINDOW 'Sorting &Z selected records for report ...' NOWAIT
    INDEX ON &XSORT TAG &lcTempLine
  ENDIF
  SET ORDER TO TAG &lcTempLine
ENDIF
*--PRINT REPORT
SELECT (lcTempLine)
LOCATE
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*SONOR10                                 ORDER DETAIL REPORT - 123456789012345678901234567890                             PAGE: 1234
*MM/DD/YY                                                                                                                 TIME: HH:SS
*
*1234567 12345678901234567890 1234.99
*
*ACCT  ORDER    START  COMPLETE SP STYLE   CLR    (1)    (2)    (3)    (4)    (5)    (6)    (7)    (8)   PIECES   PRICE PIKTKT DIFF.
*12345 123456 MM/DD/YY MM/DD/YY XX 1234567 123 111111 222222 333333 444444 555555 666666 777777 888888 12345678 1234.99 123456 12345
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
IOXFILE = SELECT()
IF BREAK <> ' '
  HBREAK = &BREAK
ENDIF
CTLSTYLE   = STYLE
*CTLCOLOR   = STYLE+COLOR
CTLOR      = ACCOUNT+ORDER+IIF(ORDHDR.MULTIPO,'*MULTI-PO*',ORDHDR.CUSTPO)
XORDER     = ORDER
STYDESC    = STYLE.DESC
STYFAB     = STYLE.FABRIC
XSCALE     = STYLE.SCALE
NEWSTYLE   = .T.
XSUM_STORE = .F.
PAGENO = 0
ROW    = 99
XTIME  = SUBSTR(TIME(),1,5)
XPRV_KEY=SPACE(1)

*--------------------------------------------------------
*  (1) STYLE
*  (2) USER DEFINED CONTROL BREAK
*  (3) GRAND TOTALS
*--------------------------------------------------------
XTOTAL  = 0.00   &&-ARRAY(3,10)
CLRTOT  = 0.00   &&-ARRAY(10)
ORDTOT  = 0.00   &&-ARRAY(10)
*-----------------------------------------------------
* [REPORT] BEGIN REPORT LOOP
*-----------------------------------------------------
R_TITLE = " ORDER DETAIL REPORT "
CLEAR TYPEAHEAD
SET DEVICE TO SCREEN
WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
SET DEVICE TO PRINT

DO WHILE INKEY() <>32

  IF ROW >=54
    PAGENO = PAGENO +1

    DO RPT_HDR WITH 'SONOR10','(SUB TOTALS BY: ' + CTLSEQ +') '+ lcRpMsg1,R_WIDTH
    IF lcRpSortBy = 'S'
       @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP CUSTOMER               (1)    (2)    (3)    (4)    (5)    (6)    (7)    (8) A  PIECES   PRICE'        
    ELSE
       @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP STYLE        COLOR     (1)    (2)    (3)    (4)    (5)    (6)    (7)    (8) A  PIECES   PRICE'            
    ENDIF
    @ 06,000 SAY REPLICATE('-',132)
    ROW = 7
  ENDIF

  SET DEVICE TO PRINT

  SELECT (lcTempLine)
  LEVEL = 0
  IF &lcTempLine..STYLE <> CTLSTYLE
    LEVEL = 1
  ENDIF

  DOCLR = IIF(lcRpSortBy='S' .AND. STYLE<>CTLSTYLE, .T., .F.)
  DOORD = IIF(lcRpSortBy='A' .AND. ;
  ACCOUNT+ORDER+IIF(ORDHDR.MULTIPO,'*MULTI-PO*',ORDHDR.CUSTPO)<>CTLOR, .T., .F.)


  IF LEN(TRIM(BREAK)) <>0
    IF HBREAK <> &BREAK
      LEVEL = 2
    ENDIF
  ENDIF

  *----------------------------------------------------
  * [DOCLR] STYLE/CLR SUB TOTALS
  *----------------------------------------------------
  IF DOCLR
    IF (lcRpRepFrm = 'S' .AND. lcRpStyPrn = 'N')
      @ ROW,055 SAY CLRTOT(1,1)  PICTURE '@Z 99999'
      @ ROW,061 SAY CLRTOT(1,2)  PICTURE '@Z 99999'
      @ ROW,068 SAY CLRTOT(1,3)  PICTURE '@Z 99999'
      @ ROW,075 SAY CLRTOT(1,4)  PICTURE '@Z 99999'
      @ ROW,082 SAY CLRTOT(1,5)  PICTURE '@Z 99999'
      @ ROW,089 SAY CLRTOT(1,6)  PICTURE '@Z 99999'
      @ ROW,096 SAY CLRTOT(1,7)  PICTURE '@Z 99999'
      @ ROW,103 SAY CLRTOT(1,8)  PICTURE '@Z 99999'
      @ ROW,112 SAY CLRTOT(1,9)  PICTURE '9999999'
      @ ROW,120 SAY PADL('$'+ALLT(STR(CLRTOT(1,10),10,2)),10,' ')
    ELSE
      ROW = ROW+1
      @ ROW,030 SAY '***'
      @ ROW,034 SAY LEFT(CTLSTYLE,12)
      @ ROW,047 SAY RIGHT(CTLSTYLE,6)
      @ ROW,054 SAY CLRTOT(1,1)  PICTURE '@Z 99999'
      @ ROW,060 SAY CLRTOT(1,2)  PICTURE '@Z 99999'
      @ ROW,066 SAY CLRTOT(1,3)  PICTURE '@Z 99999'
      @ ROW,072 SAY CLRTOT(1,4)  PICTURE '@Z 99999'
      @ ROW,078 SAY CLRTOT(1,5)  PICTURE '@Z 99999'
      @ ROW,084 SAY CLRTOT(1,6)  PICTURE '@Z 99999'
      @ ROW,090 SAY CLRTOT(1,7)  PICTURE '@Z 99999'
      @ ROW,096 SAY CLRTOT(1,8)  PICTURE '@Z 99999'
      @ ROW,102 SAY CLRTOT(1,9)  PICTURE '9999999'
      @ ROW,120 SAY PADL('$'+ALLT(STR(CLRTOT(1,10),10,2)),10,' ')
    ENDIF
    ROW = ROW+2
    CLRTOT = 0.00
  ENDIF

  *----------------------------------------------------
  * [DOORD] ACCOUNT/ORDER SUB TOTALS
  *----------------------------------------------------
  IF DOORD
    @ ROW,000 SAY '***'
    @ ROW,006 SAY LEFT(CTLOR,5)
    @ ROW,012 SAY SUBSTR(CTLOR,6,6)
    @ ROW,019 SAY 'CUST. PO# :'
    @ ROW,031 SAY RIGHT(CTLOR,10)
    @ ROW,055 SAY ORDTOT(1,1)  PICTURE '@Z 99999'
    @ ROW,061 SAY ORDTOT(1,2)  PICTURE '@Z 99999'
    @ ROW,068 SAY ORDTOT(1,3)  PICTURE '@Z 99999'
    @ ROW,075 SAY ORDTOT(1,4)  PICTURE '@Z 99999'
    @ ROW,082 SAY ORDTOT(1,5)  PICTURE '@Z 99999'
    @ ROW,089 SAY ORDTOT(1,6)  PICTURE '@Z 99999'
    @ ROW,096 SAY ORDTOT(1,7)  PICTURE '@Z 99999'
    @ ROW,103 SAY ORDTOT(1,8)  PICTURE '@Z 99999'
    @ ROW,112 SAY ORDTOT(1,9)  PICTURE '9999999'
    @ ROW,120 SAY PADL('$'+ALLT(STR(ORDTOT(1,10),10,2)),10,' ')
    ROW = ROW+2
    CTLOR = ACCOUNT+ORDER+IIF(ORDHDR.MULTIPO,'*MULTI-PO*',ORDHDR.CUSTPO)
    ORDTOT = 0.00
    IF llRpOrdNot
      XPRV_ROW=ROW
      XREPORT='SONOR10'
      DO PRT_NOTE WITH 'B',XORDER
      ROW=IIF(ROW=XPRV_ROW,ROW,ROW+1)   && INCREMNET ROW ONLY IF NOTEPAD PRINTED
      XORDER=ORDER
    ENDIF
  ENDIF

  *----------------------------------------------------
  * [LEVEL 1] STYLE SUB TOTALS
  *----------------------------------------------------
  IF LEVEL >=1
    NEWSTYLE = .F.
    * PRINT STYLE SUBTOTAL ONLY FOR REPORTS S,F

    IF lcRpSortBy $ 'SF'
      @ ROW,000 SAY REPLICATE('-',132)
      ROW = ROW+1
      @ ROW,000 SAY SUBSTR(STYDESC,1,12)
      @ ROW,013 SAY '('+STYFAB+')'          
      @ ROW,026 SAY CTLSTYLE
      @ ROW,039 SAY '******'
      @ ROW,054 SAY XTOTAL(1,1)  PICTURE '@Z 999999'
      @ ROW,061 SAY XTOTAL(1,2)  PICTURE '@Z 999999'
      @ ROW,068 SAY XTOTAL(1,3)  PICTURE '@Z 999999'
      @ ROW,075 SAY XTOTAL(1,4)  PICTURE '@Z 999999'
      @ ROW,082 SAY XTOTAL(1,5)  PICTURE '@Z 999999'
      @ ROW,089 SAY XTOTAL(1,6)  PICTURE '@Z 999999'
      @ ROW,096 SAY XTOTAL(1,7)  PICTURE '@Z 999999'
      @ ROW,103 SAY XTOTAL(1,8)  PICTURE '@Z 999999'
      @ ROW,112 SAY XTOTAL(1,9)  PICTURE '9999999'
      @ ROW,120 SAY PADL('$'+ALLT(STR(XTOTAL(1,10),10,2)),10,' ')
      ROW = ROW+1
      @ ROW,000 SAY REPLICATE('-',132)
      ROW = ROW+2
      CTLSTYLE = STYLE
      STYDESC  = STYLE.DESC
      STYFAB   = STYLE.FABRIC
      XSCALE   = STYLE.SCALE
      NEWSTYLE = .T.
    ENDIF

    X = 1
    DO WHILE X<=10
      XTOTAL(2,X) = XTOTAL(2,X) + XTOTAL(1,X)
      XTOTAL(3,X) = XTOTAL(3,X) + XTOTAL(1,X)
      XTOTAL(1,X) =0.00
      X = X+1
    ENDDO
  ENDIF
  *------------------ END USER DEFINED SUB TOTALS -------------------
  *----------------------------------------------------
  * [LEVEL 2] USER DEFINED SUB TOTALS
*----------------------------------------------------
  IF BREAK<>' ' .AND. LEVEL >=2
  @ ROW,000 SAY REPLICATE('-',132)
  ROW = ROW+1
  IF lcRpSortBy = 'O'
    @ ROW,000 SAY SUBSTR(HBREAK,10,5)
    @ ROW,006 SAY SUBSTR(HBREAK,1,6)
  ELSE
    IF lcRpSortBy = 'R'
      SELECT SALESREP
      SEEK HBREAK
      XNAME=NAME
      SELECT (lcTempLine)
    ELSE
      XNAME=''
      ENDIF
      @ ROW,000 SAY HBREAK+' '+XNAME
    ENDIF
    IF LFCR $ HBREAK
      ROW = ROW+1
    ENDIF
    @ ROW,054 SAY XTOTAL(2,1)  PICTURE '@Z 999999'
    @ ROW,061 SAY XTOTAL(2,2)  PICTURE '@Z 999999'
    @ ROW,068 SAY XTOTAL(2,3)  PICTURE '@Z 999999'
    @ ROW,075 SAY XTOTAL(2,4)  PICTURE '@Z 999999'
    @ ROW,082 SAY XTOTAL(2,5)  PICTURE '@Z 999999'
    @ ROW,089 SAY XTOTAL(2,6)  PICTURE '@Z 999999'
    @ ROW,096 SAY XTOTAL(2,7)  PICTURE '@Z 999999'
    @ ROW,103 SAY XTOTAL(2,8)  PICTURE '@Z 999999'
    @ ROW,112 SAY XTOTAL(2,9)  PICTURE '9999999'
    @ ROW,120 SAY PADL('$'+ALLT(STR(XTOTAL(2,10),10,2)),10,' ')
    ROW = ROW+1
    @ ROW,000 SAY REPLICATE('-',132)
    ROW = ROW+2
    HBREAK = &BREAK
    X = 1
    DO WHILE X<=10
      XTOTAL(2,X) =0.00
      X = X+1
    ENDDO
  ENDIF
  *------------------ END USER DEFINED SUB TOTALS -------------------
  IF EOF(IOXFILE)
    EXIT
  ENDIF
  *------------------------------------------------
  * PROCESS DETAIL RECORD
  *------------------------------------------------
  IF NEWSTYLE
    IF ROW >=54
      PAGENO = PAGENO +1
      DO RPT_HDR WITH 'SONOR10','(SUB TOTALS BY: ' + CTLSEQ +') '+ lcRpMsg1,R_WIDTH
      IF lcRpSortBy = 'S'
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP CUSTOMER               (1)    (2)    (3)    (4)    (5)    (6)    (7)    (8) A  PIECES   PRICE'
      ELSE
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP STYLE        COLOR     (1)    (2)    (3)    (4)    (5)    (6)    (7)    (8) A  PIECES   PRICE'
      ENDIF
      @ 06,000 SAY REPLICATE('-',132)
      ROW = 7
    ENDIF
    NEWSTYLE = .F.
    SELECT SCALE
    SEEK 'S'+XSCALE
    IF ROW >=53
      PAGENO = PAGENO +1
      DO RPT_HDR WITH 'SONOR10','(SUB TOTALS BY: ' + CTLSEQ +') '+ lcRpMsg1,R_WIDTH
      IF lcRpSortBy = 'S'
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP CUSTOMER              (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)  A PIECES   PRICE'
      ELSE
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP STYLE        COLOR    (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)  A PIECES   PRICE'
      ENDIF
      @ 06,000 SAY REPLICATE('-',132)
      ROW = 7
    ENDIF
    @ ROW,054 SAY PADL(ALLT(SZ1),6)
    @ ROW,061 SAY PADL(ALLT(SZ2),6)
    @ ROW,068 SAY PADL(ALLT(SZ3),6)
    @ ROW,075 SAY PADL(ALLT(SZ4),6)
    @ ROW,082 SAY PADL(ALLT(SZ5),6)
    @ ROW,089 SAY PADL(ALLT(SZ6),6)
    @ ROW,096 SAY PADL(ALLT(SZ7),6)
    @ ROW,103 SAY PADL(ALLT(SZ8),6)
    ROW=ROW+1
  ENDIF
  SELECT (lcTempLine)

  IF lcRpSortBy='S' .AND. XPRV_KEY <> STYLE
  XPRV_KEY=STYLE
  SELECT STYLE
  SEEK XPRV_KEY
  IF FOUND()
    IF ROW >=54
      PAGENO = PAGENO +1
      DO RPT_HDR WITH 'SONOR10','(SUB TOTALS BY: ' + CTLSEQ +') '+ lcRpMsg1,R_WIDTH
      IF lcRpSortBy = 'S'
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP CUSTOMER              (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   A PIECES   PRICE'
      ELSE
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP STYLE        COLOR    (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   A PIECES   PRICE'
      ENDIF
      @ 06,000 SAY REPLICATE('-',132)
      ROW = 7
    ENDIF
    SELECT STYLE
    @ ROW,000 SAY SUBSTR(Style,1,LnMajorlen)
    @ ROW,013 SAY SUBSTR(Style,lnNonMajSt,Lncolorlen)
    @ ROW,020 SAY SUBSTR(gfCodDes(SUBSTR(&lcTempLine..Style,lnNonMajSt,Lncolorlen), 'COLOR     '),1,10)
    DO CASE
      CASE lcRpStyPrn = 'S'
        @ ROW,032 SAY 'STOCK :'
        @ ROW,054 SAY STK1  PICTURE '@Z 99999'
        @ ROW,060 SAY STK2  PICTURE '@Z 99999'
        @ ROW,066 SAY STK3  PICTURE '@Z 99999'
        @ ROW,072 SAY STK4  PICTURE '@Z 99999'
        @ ROW,078 SAY STK5  PICTURE '@Z 99999'
        @ ROW,084 SAY STK6  PICTURE '@Z 99999'
        @ ROW,090 SAY STK7  PICTURE '@Z 99999'
        @ ROW,096 SAY STK8  PICTURE '@Z 99999'
        @ ROW,102 SAY TOTSTK  PICTURE '9999999'
      CASE lcRpStyPrn = 'B'
        @ ROW,032 SAY 'WIP + STOCK :'
        @ ROW,054 SAY WIP1+STK1  PICTURE '@Z 999999'
        @ ROW,060 SAY WIP2+STK2  PICTURE '@Z 999999'
        @ ROW,066 SAY WIP3+STK3  PICTURE '@Z 999999'
        @ ROW,072 SAY WIP4+STK4  PICTURE '@Z 999999'
        @ ROW,078 SAY WIP5+STK5  PICTURE '@Z 999999'
        @ ROW,084 SAY WIP6+STK6  PICTURE '@Z 999999'
        @ ROW,090 SAY WIP7+STK7  PICTURE '@Z 999999'
        @ ROW,096 SAY WIP8+STK8  PICTURE '@Z 999999'
        @ ROW,102 SAY TOTWIP+TOTSTK  PICTURE '9999999'
      CASE lcRpStyPrn = 'W'
        @ ROW,032 SAY 'WIP :'
        @ ROW,054 SAY WIP1  PICTURE '@Z 99999'
        @ ROW,060 SAY WIP2  PICTURE '@Z 99999'
        @ ROW,066 SAY WIP3  PICTURE '@Z 99999'
        @ ROW,072 SAY WIP4  PICTURE '@Z 99999'
        @ ROW,078 SAY WIP5  PICTURE '@Z 99999'
        @ ROW,084 SAY WIP6  PICTURE '@Z 99999'
        @ ROW,090 SAY WIP7  PICTURE '@Z 99999'
        @ ROW,096 SAY WIP8  PICTURE '@Z 99999'
        @ ROW,102 SAY TOTWIP  PICTURE '9999999'
      ENDCASE
      IF .NOT. (lcRpRepFrm = 'S' .AND. lcRpStyPrn='N')
        ROW = ROW+1
      ENDIF
    ENDIF
    SELECT (lcTempLine)
  ENDIF
  *********************************************************************
  ****************** PRINT THE LINE ITEM DETAIL ***********************
  *********************************************************************
  SELECT (lcTempLine)
  IF lcRpRepFrm = 'D'
    @ ROW,000 SAY ACCOUNT
    @ ROW,006 SAY ORDER
    @ ROW,013 SAY ORDHDR.START
    @ ROW,022 SAY ORDHDR.COMPLETE
    @ ROW,031 SAY ORDHDR.STATUS
    @ ROW,032 SAY ALLTRIM(ORDHDR.PRIORITY)
    IF lcRpSortBy = 'S'
      @ ROW,034 SAY SUBSTR(CUSTOMER.STNAME,1,10)
    ELSE
      @ ROW,034 SAY SUBSTR(Style,1,LnMajorlen)      
      @ ROW,047 SAY SUBSTR(Style,lnNonMajSt,Lncolorlen)      
    ENDIF
    *--Check condition of printing
    XSUM_STORE=IIF(llRpSummMt .AND. ORDHDR.MULTI='Y',.T.,.F.)
    IF XSUM_STORE
      STORE 0 TO XQTY1,XQTY2,XQTY3,XQTY4,XQTY5,XQTY6,XQTY7,XQTY8,;
                 XTOTQTY,XAMT
      STORE 0 TO XPIK1,XPIK2,XPIK3,XPIK4,XPIK5,XPIK6,XPIK7,XPIK8
      XSTYLE=STYLE
      XCOMDT=DTOS(ORDHDR.COMPLETE)
      XORDER=ORDER
      SCAN WHILE STYLE+DTOS(ORDHDR.COMPLETE)+ORDER = XSTYLE+;
                 XCOMDT+XORDER
        FOR XMCOUNT = 1 TO 8
          Z=STR(XMCOUNT,1)
          XQTY&Z=XQTY&Z+QTY&Z
          XPIK&Z=XPIK&Z+PIK&Z  
        ENDFOR
        XTOTQTY=XTOTQTY+TOTQTY
        XAMT   =XAMT   +(TOTQTY*PRICE)
      ENDSCAN
      XTOTQTY=XQTY1+XQTY2+XQTY3+XQTY4+XQTY5+XQTY6+XQTY7+XQTY8
      IF llRpChkAvb
        =lfUpdAvl("XQTY", "XPIK")
      ENDIF
      @ ROW,054 SAY IIF(XQTY1 <> 0, PADL(STR(XQTY1,5)+laNotAvl(1),6),"") 
      @ ROW,061 SAY IIF(XQTY2 <> 0, PADL(STR(XQTY2,5)+laNotAvl(2),6),"") 
      @ ROW,068 SAY IIF(XQTY3 <> 0, PADL(STR(XQTY3,5)+laNotAvl(3),6),"") 
      @ ROW,075 SAY IIF(XQTY4 <> 0, PADL(STR(XQTY4,5)+laNotAvl(4),6),"") 
      @ ROW,082 SAY IIF(XQTY5 <> 0, PADL(STR(XQTY5,5)+laNotAvl(5),6),"") 
      @ ROW,089 SAY IIF(XQTY6 <> 0, PADL(STR(XQTY6,5)+laNotAvl(6),6),"") 
      @ ROW,096 SAY IIF(XQTY7 <> 0, PADL(STR(XQTY7,5)+laNotAvl(7),6),"") 
      @ ROW,103 SAY IIF(XQTY8 <> 0, PADL(STR(XQTY8,5)+laNotAvl(8),6),"") 
      @ ROW, 110 SAY lcShip
      @ ROW,112 SAY XTOTQTY  PICTURE '9999999'
    ELSE
      IF llRpChkAvb
         =lfUpdAvl("QTY", "PIK")
      ENDIF
      @ ROW,054 SAY IIF(QTY1 <> 0, PADL(STR(QTY1,5)+laNotAvl(1),6),"") 
      @ ROW,061 SAY IIF(QTY2 <> 0, PADL(STR(QTY2,5)+laNotAvl(2),6),"") 
      @ ROW,068 SAY IIF(QTY3 <> 0, PADL(STR(QTY3,5)+laNotAvl(3),6),"") 
      @ ROW,075 SAY IIF(QTY4 <> 0, PADL(STR(QTY4,5)+laNotAvl(4),6),"") 
      @ ROW,082 SAY IIF(QTY5 <> 0, PADL(STR(QTY5,5)+laNotAvl(5),6),"") 
      @ ROW,089 SAY IIF(QTY6 <> 0, PADL(STR(QTY6,5)+laNotAvl(6),6),"") 
      @ ROW,096 SAY IIF(QTY7 <> 0, PADL(STR(QTY7,5)+laNotAvl(7),6),"") 
      @ ROW,103 SAY IIF(QTY8 <> 0, PADL(STR(QTY8,5)+laNotAvl(8),6),"") 
      @ ROW, 110 SAY lcShip
      @ ROW,112 SAY TOTQTY  PICTURE '9999999'
      @ ROW,120 SAY PRICE   PICTURE '9999999.99'
    ENDIF
    ROW = ROW+1
  ENDIF

  SELECT (lcTempLine)
  IF llRpOrdLnt .AND. lcRpRepFrm = 'D' .AND. !llRpSummMt
    IF !EMPTY(NOTE_MEM)
      ROW = ROW + 1
      @ ROW , 00 SAY 'ORDER NOTES  :'
      FOR X = 1 TO MEMLINES(NOTE_MEM)
        @ ROW,15 SAY MLINE(NOTE_MEM,X)
        ROW = ROW + 1
      ENDFOR
    ENDIF
  ENDIF
  **********************************************************************
  ********************* END PRINT THE LINE DETAIL **********************
  **********************************************************************

  XTOTAL(1,1) = XTOTAL(1,1) +  IIF(XSUM_STORE,XQTY1,QTY1)
  XTOTAL(1,2) = XTOTAL(1,2) +  IIF(XSUM_STORE,XQTY2,QTY2)
  XTOTAL(1,3) = XTOTAL(1,3) +  IIF(XSUM_STORE,XQTY3,QTY3)
  XTOTAL(1,4) = XTOTAL(1,4) +  IIF(XSUM_STORE,XQTY4,QTY4)
  XTOTAL(1,5) = XTOTAL(1,5) +  IIF(XSUM_STORE,XQTY5,QTY5)
  XTOTAL(1,6) = XTOTAL(1,6) +  IIF(XSUM_STORE,XQTY6,QTY6)
  XTOTAL(1,7) = XTOTAL(1,7) +  IIF(XSUM_STORE,XQTY7,QTY7)
  XTOTAL(1,8) = XTOTAL(1,8) +  IIF(XSUM_STORE,XQTY8,QTY8)
  XTOTAL(1,9) = XTOTAL(1,9) +  IIF(XSUM_STORE,XTOTQTY,TOTQTY)
  XTOTAL(1,10) = XTOTAL(1,10) + IIF(XSUM_STORE,XAMT,TOTQTY*PRICE)
  IF lcRpSortBy = 'S'
    *-- Sum qty under condition XSUM_STORE
    CLRTOT(1) = CLRTOT(1) +  IIF(XSUM_STORE,XQTY1,QTY1)
    CLRTOT(2) = CLRTOT(2) +  IIF(XSUM_STORE,XQTY2,QTY2)
    CLRTOT(3) = CLRTOT(3) +  IIF(XSUM_STORE,XQTY3,QTY3)
    CLRTOT(4) = CLRTOT(4) +  IIF(XSUM_STORE,XQTY4,QTY4)
    CLRTOT(5) = CLRTOT(5) +  IIF(XSUM_STORE,XQTY5,QTY5)
    CLRTOT(6) = CLRTOT(6) +  IIF(XSUM_STORE,XQTY6,QTY6)
    CLRTOT(7) = CLRTOT(7) +  IIF(XSUM_STORE,XQTY7,QTY7)
    CLRTOT(8) = CLRTOT(8) +  IIF(XSUM_STORE,XQTY8,QTY8)
    CLRTOT(9) = CLRTOT(9) +  IIF(XSUM_STORE,XTOTQTY,TOTQTY)
    CLRTOT(10) = CLRTOT(10) + IIF(XSUM_STORE,XAMT,TOTQTY*PRICE)
  ENDIF
  IF lcRpSortBy = 'A'
    ORDTOT(1) = ORDTOT(1) +  QTY1
    ORDTOT(2) = ORDTOT(2) +  QTY2
    ORDTOT(3) = ORDTOT(3) +  QTY3
    ORDTOT(4) = ORDTOT(4) +  QTY4
    ORDTOT(5) = ORDTOT(5) +  QTY5
    ORDTOT(6) = ORDTOT(6) +  QTY6
    ORDTOT(7) = ORDTOT(7) +  QTY7
    ORDTOT(8) = ORDTOT(8) +  QTY8
    ORDTOT(9) = ORDTOT(9) +  TOTQTY
    ORDTOT(10) = ORDTOT(10) +  TOTQTY * PRICE
  ENDIF
  SELECT (lcTempLine)

  IF .NOT. XSUM_STORE
    SKIP
  ENDIF

ENDDO
*---------------------- END OF REPORT -------------------------
*----------------------------------------------------
* [LEVEL 3] GRAND TOTALS
*----------------------------------------------------
IF ROW >=60
  ROW = 5
ENDIF
@ ROW,000 SAY REPLICATE('=',132)
ROW = ROW+1

@ ROW,010 SAY 'GRAND TOTALS'
@ ROW,054 SAY XTOTAL(3,1)  PICTURE '@Z 999999'
@ ROW,061 SAY XTOTAL(3,2)  PICTURE '@Z 999999'
@ ROW,068 SAY XTOTAL(3,3)  PICTURE '@Z 999999'
@ ROW,075 SAY XTOTAL(3,4)  PICTURE '@Z 999999'
@ ROW,082 SAY XTOTAL(3,5)  PICTURE '@Z 999999'
@ ROW,089 SAY XTOTAL(3,6)  PICTURE '@Z 999999'
@ ROW,096 SAY XTOTAL(3,7)  PICTURE '@Z 999999'
@ ROW,103 SAY XTOTAL(3,8)  PICTURE '@Z 999999'
@ ROW,112 SAY XTOTAL(3,9)  PICTURE '9999999'
@ ROW,120 SAY PADL('$'+ALLT(STR(XTOTAL(3,10),10,2)),10,' ')
ROW = ROW+1

@ ROW,000 SAY REPLICATE('=',132)

S_CLOSEALL = .F.  && Not to close the opened files because it's possible
                  &&  to reprint the report again
DO ENDREPORT
RETURN
*---------------------------
*   END SONOR10.PRG
*---------------------------

*!**************************************************************************
*:Name      : lfUpdAvl
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*: Purpose  : Update the the style available qty
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : 
*!**************************************************************************
FUNCTION lfUpdAvl
PARAMETERS lcOrd, lcPik
PRIVATE lnCnt
lnCnt = 1
STORE "" TO laNotAvl
STORE "A" TO lcShip
IF !EOF('lcStyAvl')
  FOR lnCnt = 1 TO 8
    lcCnt = STR(lnCnt,1)
    IF (EVAL(lcOrd + lcCnt) - EVAL(lcPik + lcCnt)) <> 0 AND;
     (lcStyAvl.stk&lcCnt - lcStyAvl.ALO&lcCnt)  < (EVAL(lcOrd + lcCnt) - EVAL(lcPik + lcCnt))
      STORE "*" TO laNotAvl(lnCnt)
      STORE "" TO lcShip
    ELSE
      REPLACE lcStyAvl.Alo&lcCnt WITH lcStyAvl.Alo&lcCnt +;
      EVAL(lcOrd + lcCnt) - EVAL(lcPik + lcCnt)
    ENDIF
  ENDFOR
ELSE
  STORE "*" TO laNotAvl(lnCnt)
  STORE "" TO lcShip
ENDIF

*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 17/08/2000
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfEvalSegs()
*:***************************************************************************
*
FUNCTION lfEvalSegs
STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcMajPict  = gfItemMask("PM")
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

*-- end of lfEvalSegs.

*!*************************************************************
*:Name      : lfNMajType
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*: Purpose  : Mask NonMajor segments .
*!*************************************************************
*! Called from : lfEvalSegs.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNMajType()
*!*************************************************************
FUNCTION lfNMajType
PARAMETERS lcNMajType,lnMajSegs

*-- Loop Around Non Major elements.
FOR lnI = lnMajSegs + 1 TO ALEN(laMajSegs,1)

  IF laMajSegs[lnI,1] = lcNMajType

    lcFree_Clr = IIF(EMPTY(lcFree_Clr),laMajSegs[lnI,1],lcFree_Clr)
    lnNonMajSt = IIF(lnNonMajSt = 0,laMajSegs[lnI,4],lnNonMajSt)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSegs[lnI,3],;
                     lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])

    lcNonMajTl = IIF(EMPTY(lcNonMajTl),PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                     lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

RETURN !EMPTY(lcFree_Clr)
*-- end of lfNMajType. 

*!*************************************************************
*:Name      : lfArrDumy
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : Fill Sort and select arrays
*!*************************************************************
*! Example   : = lfArrDumy()
*!*************************************************************
*-- Function to fill select by and sort by arrays.
FUNCTION lfArrDumy
PRIVATE lnSelElms , lnSrtElms
lnSelElms = 3
lnSrtElms = 6
DIMENSION laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1],;
          laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1],;
          laSort2Des[2,1],laSort2Val[2,1]

*-- Sort 2 elementes [Begin]
laSort2Des[1,1] = "Line#"
laSort2Des[2,1] = lcStyMajor
laSort2Val[1,1] = "L"
laSort2Val[2,1] = "S"
*-- Sort 2 elementes [End  ]

*-- Sort 1 base elements [Begin]
laSortDesc[1,1] = 'Account'
laSortDesc[2,1] = 'Order'
laSortDesc[3,1] = lcStyMajor
laSortDesc[4,1] = 'Priority'
laSortDesc[5,1] = 'Primary Sales Representative'
laSortDesc[6,1] = 'Complete Date'

laSortVal[1,1]  = 'A'
laSortVal[2,1]  = 'O'
laSortVal[3,1]  = 'S'
laSortVal[4,1]  = 'P'
laSortVal[5,1]  = 'R'
laSortVal[6,1]  = 'D'
*-- Sort 1 base elements [End  ]

*-- Fill Select by array base elements. [Begin]
laSlctDesc[1,1] = 'Account'
laSlctDesc[2,1] = lcStyMajor
laSlctDesc[3,1] = 'All'

laSlctVal[1,1]  = 'A'
laSlctVal[2,1]  = 'S'
laSlctVal[3,1]  = 'L'
*-- Fill Select by array base elements. [End  ]

IF 'MA' $ gcCmpModules
  lnSelElms = lnSelElms + 1
  lnSrtElms = lnSrtElms + 1
  DIMENSION laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1],;
            laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1]

  =AINS(laSortDesc,5,1)
  =AINS(laSortVal,5,1)

  lnInsFabIn = ASCAN(laSlctDesc,'All',1)
  =AINS(laSlctDesc,lnInsFabIn,1)
  =AINS(laSlctVal,lnInsFabIn,1)
  STORE 'Fabric' TO laSortDesc[5,1],laSlctDesc[lnInsFabIn,1]
  STORE 'F' TO laSortVal[5,1],laSlctVal[lnInsFabIn,1]
ENDIF

*-- end of lfArrDumy.

*!*************************************************************
*:Name      : lfvCurr
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : set currency symbol
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvCurr()
*!*************************************************************
FUNCTION lfvCurr
*-- end of lfvCurr.

*!**************************************************************************
*:Name      : lfvEdiOrd
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
FUNCTION lfvEdiOrd
lcRpEdiFlt = ""
IF 'EB' $ gcCmpModules AND lcRpEdiPrn <> "B"
  lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
ENDIF
llClearOrd = .T.
*-- end of lfvEdiOrd.

*!*************************************************************
*:Name      : lfvKind
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : Enable/disable some objects due to report kind (Detail/Summary)
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvKind()
*!*************************************************************
FUNCTION lfvKind
CLEAR READ
*-- end of lfvKind.

*!*************************************************************
*:Name      : lfvSelcBy
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : Validate select by option in option grid.
*!           : [Simply it enable and disable selecting buttons]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfSelcObjs
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : In this function we want to know old value to disable
*!           : last object, and we transfer it to its corressponding 
*!           : character because option grid returns its item number in popup. 
*!*************************************************************
*! Example   : =lfvSelcBy()
*!*************************************************************
FUNCTION lfvSelcBy
lcDummy = "Y"
llChSelect = .T.
llClearAcc = (lcRpSelcBy # 'A')
llClearSty = (lcRpSelcBy # 'S')
llClearFab = (lcRpSelcBy # 'F')
CLEAR READ
*-- end of lfvSelcBy.

*!**************************************************************************
*:Name      : lfwOldSelc
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose  : To store the Old Select By value
*!**************************************************************************
*! Example   : = lfwOldSelc()
*!**************************************************************************
FUNCTION lfwOldSelc
lcOldSelc = lcRpSelcBy
*-- End of lfwOldSelc.

*!*************************************************************
*: Name      : lfvSortBy
*:Developer  : Ahmed Abdel Naby (AAN)
*:Date       : 03/22/2001
*: Purpose   : 1- Enable and disable some variavle objects due to sort case
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState,lfPreObjs
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : 1- In this function we want to know old value to enable/disable
*!           :    objects due to some sort cases, and we transfer it to 
*!           :    its corressponding character because option grid returns
*!           :    its item number in popup, the idea of enable/disable in
*!           :    this function is to full control printing and do not enable
*!           :    enabled button or disable disabled button.
*!           : 2- In some cases we rise summarization flag to Recollect data again.
*!*************************************************************
*! Example   : =lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy
laOldVal = IIF(laOldVal = 1 , 'A' , IIF(laOldVal = 2 , 'O',;
           IIF(laOldVal = 3 , 'S' , IIF(laOldVal = 4 , 'G',;
           IIF(laOldVal = 5 , 'F' , IIF(laOldVal = 6 , 'W',;
           IIF(laOldVal = 7 , 'R' , 'D')))))))

llRpScale  = IIF(lcRpSortBy $ 'SFG',.F.,.T.)

llRpSummMt = IIF(lcRpSortBy <> 'S' ,.F.,llRpSummMt)
lcRpStyPrn = IIF(lcRpSortBy <> 'S' ,'N',lcRpStyPrn)
llRpOrdNot = IIF(!(lcRpSortBy $ 'AO'),.F.,llRpOrdNot)
CLEAR READ

*-- Notes [begin]
*-- if In case of sort by (STYLE,FABRIC OR STYLE GROUP) and old value is
*-- another sort type and user want to print sizes, we must disable
*-- reprint scale when diff. because it is the normal print case here,
*-- and vice versa.
*-- Notes [end]

*-- If user want to print size scale

IF lcRpSortBy != laOldVal

  *-- Different sort by cases.
  DO CASE
    CASE lcRpSortBy = 'A'   && Sort by account 

      *-- Enable/disable variable objects due to sort case. [begin]
      *-- if report kind is detail
      *-- Enable/disable variable objects due to sort case. [end]

    CASE lcRpSortBy = 'O'    && Sort by order

      *-- Enable/disable variable objects due to sort case. [begin]
      *-- Enable/disable variable objects due to sort case. [begin]

    CASE lcRpSortBy = 'S'      && Sort by style
      *-- Enable/disable variable objects due to sort case. [begin]

    CASE lcRpSortBy = 'G'    && Sort by style group

      *-- Enable/disable variable objects due to sort case. [begin]
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() && Prepair objects.
      ENDIF  
      *-- Enable/disable variable objects due to sort case. [begin]

    CASE lcRpSortBy = 'F'    && Sort by fabric

      *-- Enable/disable variable objects due to sort case. [begin]
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
      *-- Enable/disable variable objects due to sort case. [begin]

    CASE lcRpSortBy = 'W'    && Sort by location

      *-- Enable/disable variable objects due to sort case. [begin]
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
      *-- Enable/disable variable objects due to sort case. [begin]

    CASE lcRpSortBy = 'R'    && Sort by sales representative

      *-- Enable/disable variable objects due to sort case. [begin]
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
      *-- Enable/disable variable objects due to sort case. [begin]

    CASE lcRpSortBy = 'D'    && Sort by complete date

      *-- Enable/disable variable objects due to sort case. [begin]
      IF INLIST(laOldVal,'A','O','S')
        = lfPreObjs() 
      ENDIF  
      *-- Enable/disable variable objects due to sort case. [begin]

  ENDCASE          && end Different sort by cases.
ENDIF
*-- end of lfvSortBy.

*!*************************************************************
*:Name      : lfwOldVal
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!**************************************************************************
*:Name      : lfvDisMesg
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : Diaplay message to inform user to ignore Complete,Cancel,Bid Orders
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : = lfvDisMesg()
*!**************************************************************************
FUNCTION lfvDisMesg
PARAMETERS lcLine
IF lcLine = "A"
  IF llRpPrtShp AND IIF('B' $ lcRpStatus OR 'C' $ lcRpStatus OR 'X' $ lcRpStatus,.T.,.F.)
    *-- Message < This option will be applied only on Hold and Open Orders, it will >
    *--         < ignore other orders status.                                       >
    *-- Buttons <                                OK                                 >
    = gfModalGen("INM32088B00000","Dialog")
  ENDIF
ELSE
  IF !llRpAloOrd AND IIF('B' $ lcRpStatus OR 'C' $ lcRpStatus OR 'X' $ lcRpStatus,.T.,.F.)
    *-- Message < This option will be applied only on Hold and Open Orders, it will >
    *--         < ignore other orders status.                                       >
    *-- Buttons <                                OK                                 >
    = gfModalGen("INM32088B00000","Dialog")
  ENDIF 
ENDIF  
*-- End of lfvDisMesg.

*!*************************************************************
*: Name      : lfvCoorGrp
*:Developer  : Ahmed Abdel Naby (AAN)
*:Date       : 03/22/2001
*: Purpose   : Rise change print coordinate groups flag.
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
*! Example   : =lfvCoorGrp()
*!*************************************************************
FUNCTION lfvCoorGrp
llChCoord = .T.
*-- end of lfvCoorGrp.

*!*************************************************************
*:Name      : lfvSizes
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : Control Form name, Enable/disable some objects.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvSizes()
*!*************************************************************
FUNCTION lfvSizes
lcRpForm = IIF(llRpSizes,'SORDDETA','SORDDETB')
= lfRepPltFr(lcRpForm)
llRpScale = IIF(llRpSizes,.T.,.F.)
CLEAR READ
*-- end of lfvSizes.

*!*************************************************************
*:Name      : lfvSumm
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : Enable/disable Order line notes object
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvSumm()
*!*************************************************************
FUNCTION lfvSumm
llRpOrdLnt = IIF(llRpSummMt ,.F.,llRpOrdLnt)
llChSumm   = .T.
CLEAR READ
*-- end of lfvSumm.

*!*************************************************************
*:Name      : lfvCurDisp
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : Activate currency display screen to get user 
*:          : selection for currencies.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************
*!E301272,1
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*-- end of lfvCurDisp.

*!*************************************************************
*:Name      : lfvOStatus
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : - Evaluate Status expression.
*!          : - Rise change status flag. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    *lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Bid','B',;
                              IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Complete','C',;                                                            
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))))
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))

  ENDFOR  && end Loop to make Status expression.
ENDIF

lcRpStatus = IIF(EMPTY(lcRpStatus),'OH',ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llOGFltCh = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llOGFltCh = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [end]
*-- end of lfvOStatus.

*!*************************************************************
*:Name      : lfsrAcc
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : Rise change account flag, in range browse screen.
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
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChAcc = .T.
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.

*!*************************************************************
*:Name     : lfSRVFab
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : control browsing primary fabric and validate 
*:          : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    llClearFab = .F.
  OTHERWISE      && Valid code
    lcAlias = ALIAS()
    SELECT STYLE
    LOCATE FOR STYLE.Fabric = Fabric.Fabric
    llHaveSty = FOUND()
    *-- If no styles found for this fabric
    IF !llHaveSty
      *-- the following message is
      *-- No styles in fabric group XXX .
      *--           <Ok>
      = gfModalGen("TRM32055B36000","Dialog",Fabric.Fabric)
    ENDIF
    SELECT (lcAlias)
    RETURN llHaveSty    && Record selected only if fabric found in style file.
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*:Name      : lfSROrder
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : Rise change order flag, in range browse screen.
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
*! Example   : =lfSROrder()
*!*************************************************************
*! Note      : S symbol is [S,Set- R,ReSet]
*!*************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm
llChOrder = .T.
DO CASE
  CASE lcParm = 'S'

    SELECT ORDHDR
    lcCustRel = IIF(llRpSummMt,['M' + Account],;
                [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])

    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.
    LOCATE 
  
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    llClearOrd = .F.

ENDCASE
*-- end of lfsChOrder.

*!*************************************************************
*:Name      : lfsrRep
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : Rise change sales rep. flag, in range browse screen.
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
*! Example   : =lfsrRep()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrRep
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChRep = .T.
    GO TOP IN SALESREP
  CASE lcParm = 'R'
    llClearRep = .F.
ENDCASE
*-- end of lfsrRep.

*!*************************************************************
*:Name      : lfsrvSty
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : Rise change style flag, in range browse screen.
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
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
  OTHERWISE      && Valid code
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*:Name      : lfsrLoc
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : Rise change Location flag, in range browse screen.
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
*! Example   : =lfsrLoc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrLoc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChLoc = .T.
    GO TOP IN WAREHOUS
  CASE lcParm = 'R'
    llClearLoc = .F.
ENDCASE
*-- end of lfsrLoc.

*!*************************************************************
*:Name      : lfwRepWhen
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
*-- if it's first time to run the report.
*-- using TYPE of variable instead of global llFirstTime, to control
*-- reset case which does not rise llFirsttime, but restore initial
*-- value for lnVarbEnd and advanced case which keep the variables same.
IF TYPE('lnVarbEnd') = 'C'
  lnVarbEnd = 0

  *DECLARE laRpSource[5]
  DECLARE laRpSource[2]
  
  IF TYPE('laRpTarget[1]') = 'C'
    IF EMPTY(laRpTarget[1])
      DECLARE laRpTarget[1]
    ELSE
      FOR lnI = 2 TO ALEN(laRpTarget)
        IF TYPE('laRpTarget[lnI]') = 'U'
          laRpTarget[lnI] = ""
        ENDIF
      ENDFOR    
    ENDIF
  ELSE
    DECLARE laRpTarget[1]
  ENDIF

  STORE 'Open'     TO laRpSource[1]
  STORE 'Hold'     TO laRpSource[2]
  lcRpStatus = 'OH'

  SET ORDER TO ORDHDR IN ORDHDR      && To use it to validate ORDER   # in option grid.
  SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate ACCOUNT # in option grid.
  SET ORDER TO STYLE IN STYLE        && To use it to validate STYLE   # in option grid.
  SET ORDER TO SALESREP IN SALESREP  && To use it to validate REP     # in option grid.
  SET ORDER TO WAREHOUS IN WAREHOUS  && To use it to validate LOCATION# in option grid.

  IF 'MA' $ gcCmpModules
    SET ORDER TO FABRIC IN FABRIC      && To use it to validate FABRIC  # in option grid.
  ENDIF  

  lnAccPos   = lfItmPos('CUSTOMER.ACCOUNT')
  lnStyPos   = lfItmPos('STYLE.STYLE')
  lnFabPos   = lfItmPos('FABRIC.FABRIC')
  lnLocPos   = lfItmPos('WAREHOUS.CWARECODE')
  lnOrdPos   = lfItmPos('ORDHDR.ORDER')
  lnStartPos = lfItmPos('ORDLINE.START')
  lnCompPos  = lfItmPos('ORDLINE.COMPLETE')
  lnGrpPos   = lfItmPos('STYLE.CSTYGROUP')
  lnSeaPos   = lfItmPos('STYLE.SEASON')
  lnDivPos   = lfItmPos('STYLE.CDIVISION')
  lnPriPos   = lfItmPos('ORDHDR.PRIORITY')
  
  lnCatPos   = lfItmPos('ORDHDR.CORDERCAT')
  
  lnClrSgPos = lfItmPos('SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)')
  *-- Convert all ceiling functions to use lfItmPos because [End..
  lnFreSgPos = lnClrSgPos + 1

  *-- Compute Start of variable filter to control its apperance [begin]
  *-- in option grid.
  
  *-- laRpFltVal : Array to hold D for disable and E for enable, to control
  *--            : INLIST appearance in option grid.
  *-- laRpVarNow : Array to hold .T. or .F., to control variables 
  *--            : appearance in option grid. 
ELSE
  FOR lnElm = 1 TO ALEN(laOgObjType,1) 
   IF laOgObjType[lnElm,1] == "lcRpSortBy"
     _CUROBJ= OBJNUM(&laOgObjType[lnElm,2].)+1
   ENDIF
   IF lcDummy = "Y" AND laOgObjType[lnElm,1] == "lcRpSelcBy"
     _CUROBJ= OBJNUM(&laOgObjType[lnElm,2].)+1
     lcDummy = "N"
   ENDIF
  ENDFOR
ENDIF  && END IF you first time enter when function.

*-- Disable/enable By account, style, fabric, location, sales representative. [begin]
*-- note that disable and enable is according to value of laRpFltVal.

IF lcRpSelcBy = "L" OR !(lcRpSelcBy == lcOldSelc)
  **STORE .T. TO llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep
  STORE .T. TO llClearAcc,llClearSty,llClearFab
ENDIF
lcOldSelc = lcRpSelcBy
*-- end of lfwRepWhen.

*!*************************************************************
*:Name      : lfItmPos
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.

*!**************************************************************************
*:Name      : lpChkStat
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : To check for Order status in Order to Enable/Disable the 2
*:            Option added to the Selection grid.
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : DO lpChkStat
*!**************************************************************************
PROCEDURE lpChkStat
PRIVATE llStatus
llStatus = IIF('O' $ lcRpStatus OR 'H' $ lcRpStatus,.T.,.F.)
IF !llStatus
  llRpPrtShp = .F.
  llRpAloOrd = .T.
ENDIF

*-- Disable (Partially shipped Orders & Include Allocated Order Lines) if Order Status
*-- contains only Complete or Cancelled
laOGObjCnt[lfVarPos('llRpPrtShp')] = llStatus
=lfOGShowGet('llRpPrtShp')

laOGObjCnt[lfVarPos('llRpAloOrd')] = llStatus
=lfOGShowGet('llRpAloOrd')
*-- End of lpChkStat.

*!**************************************************************************
*:Name      : lfVarPos
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : To get the position of the Variable in OG.
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : = lfVarPos()
*!**************************************************************************
FUNCTION lfVarPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(laOGObjType,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGObjType,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfVarPos.

*!*************************************************************
*:Name      : lfStySum
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*:Name      : lfFabSum
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
IF RECCOUNT() != 0
  lnFabRec = RECNO('FABRIC')

  SELECT Fabric_X
  SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
  SELECT Fabric
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfFabSum.

*:****************************************************************
*:Name      : lfVSelec
*:Developer : Ahmed Abdel Naby (AAN)
*:Date      : 03/22/2001
*:Purpose   : sum a specific field for the current style in style file
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfVSelec()
*:****************************************************************
*
FUNCTION lfVSelec
CLEAR READ
*-- End of lfVSelec.

*!*************************************************************
*! Name      : lfvFabric
*: DATE      : 03/22/2001
*: Developer : Ahmed Abdel Naby (AAN)
*! Purpose   : VALIDATE THE FABRIC
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
*!c102024,4 - Add this function for validate the fabric.
*!*************************************************************

FUNCTION lfvFabric

PRIVATE lcFabld,lcFabrc,lnSelFile,lcFaTag 

lcFabld   = VARREAD()
lcFabrc  = EVAL(lcFabld)

lnSelFile = SELECT(0)

SELECT FABRIC
lcFaTag   = ORDER('FABRIC')

SET ORDER TO TAG FABRIC IN FABRIC

IF !EMPTY(lcFabrc) .AND. ('?' $ lcFabrc .OR. !SEEK(lcFabrc , 'FABRIC'))
  =FABROW (@lcFabrc,'*')
ENDIF
 
&lcFabld = lcFabrc

SET ORDER TO lcFaTag

SELECT (lnSelFile)

*!*************************************************************
*: Name      : lfPreObjs
*: DATE      : 03/22/2001
*: Developer : Ahmed Abdel Naby (AAN)
*: Purpose   : Enable/Disable controled objects in 4 sort cases
*:           : - Style group, Fabric, Sales Rep., Complete date
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfObjState
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPreObjs()
*!*************************************************************
FUNCTION lfPreObjs

IF lcRpRepFrm = 'D'
  IF laOldVal = 'S'
    llChSumm = IIF(llRpSummMt,.T.,llChSumm)
  ENDIF
ENDIF
*-- end of lfPreObjs.

