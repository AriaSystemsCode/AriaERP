*!********************************************************************
*: Program file  : ICAND100.PRG   (Cnverted from 26 to 27 for AND100)
*: Program desc. : Custom special cut&sold report. Done for Andrew Mark
*:         Module: Aria Apparel Series.
*:         Date  : 01/17/2000
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*!********************************************************************
*! Refer to (C101756)
*!********************************************************************

*----Report variables.
*-- lcRpTitle   && Holds the reports optional title.
*-- llCutSolsz  && Cut & Sold By Size?
*-- lcRpWip     && Print WIP ((S)ummary or (D)etail)?
*-- lcRpSales   && Print Sales ((S)ummary or (D)etail)?
*-- lcRpWipSor  && Sort WIP By.
*-- lcRpSalSor  && Sort Sales By. 
*-- llRpOnHold  && Include orders on 'Hold' ?
*-- lcRpPrtQty  && Print quantities (Both,Allocated,Not allocated)
*-- lcStyMajor  && Major Title.
*-- lcRpMakBuy  && Domestic/Imported.
*-- lnColorLen  && Color length.
*---Prepare filter
lcRpExp  = lcRpExp + ' AND '+IIF(lcRpMakBuy = 'B','.T.',IIF(lcRpMakBuy = 'D','STYLE.MAKE','!STYLE.MAKE'))
lcRpExp  = lcRpExp + ' AND (TOTORD<>0 OR TOTWIP<> 0 OR TOTSTK <>0)'
STORE ' ' TO lcOrDtFlt,lcPrDtFlt,R_TITLE
*--Get the sales order completion dates filter  if any.Also extract it from lcRpExp
=lfGetDates('BETWEEN(DTOS(ORDLINE',@lcOrDtFlt)
lcOrDtFlt = IIF(!EMPTY(lcOrDtFlt),STRTRAN(lcOrDtFlt,'ORDLINE.',''),'.T.')
*--Get the sales order completion dates filter  if any.Also extract it from lcRpExp
=lfGetDates('BETWEEN(DTOS(CUTTKTH.COMP',@lcPrDtFlt)
lcPrDtFlt = IIF(!EMPTY(lcPrDtFlt),lcPrDtFlt,'.T.')
*--Prepare data
SELECt STYLE
*--Clear filter as it may be filtered from a previous run.
SET FILTER TO
LOCATE FOR &lcRpExp
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
STORE .F.      TO llWipRpt, llOrdRpt
llWipRpt = (lcRpWip='D')
llOrdRpt = (lcRpSales='D')
lcStat    = IIF(!llRpOnHold,'O','HO')
IF llOgFltCh
  *--Prepare files and relations
  =lfPrepFile()
  *-- Collect data  
  =lfCollect()
ENDIF  

PAGENO=0
ROW=99
XTIME=TIME()
R_WIDTH='W'
IF llWipRpt .OR. llOrdRpt
  DO CASE
    CASE lcRpPrtQty = 'L'
      R_TITLE = 'SPECIAL CUT & SOLD - ALLOCATED DETAIL REPORT'
    CASE lcRpPrtQty = 'N'
      R_TITLE = 'SPECIAL CUT & SOLD - NOT ALLOCATED DETAIL REPORT'
    CASE lcRpPrtQty = 'B'
      R_TITLE = 'SPECIAL CUT & SOLD - DETAIL REPORT'
  ENDCASE
ELSE
  DO CASE
    CASE lcRpPrtQty = 'L'
      R_TITLE = 'SPECIAL CUT & SOLD - ALLOCATED SUMMARY REPORT'
    CASE lcRpPrtQty = 'N'
      R_TITLE = 'SPECIAL CUT & SOLD - NOT ALLOCATED SUMMARY REPORT'
    CASE lcRpPrtQty = 'B'
      R_TITLE = 'SPECIAL CUT & SOLD - SUMMARY REPORT'
  ENDCASE
ENDIF
XDESC=SPACE(20)
SET DEVICE TO PRINT
IF !llCutSolsz
  DO STY961                                 && No Breakdown In Size
ELSE
  DO STY962                                 && Breakdown In Size
ENDIF
DO ENDREPORT
SET DEVICE TO SCREEN
RETURN


****************************************************************************
* PROG: STY961.PRG
* DATE: 02/20/92
* DESC: REPORT WITHOUT SIZE BREAKDOWN
****************************************************************************
PROCEDURE STY961
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*JNL330              123456789012345678901234567890 - SPECIAL CUT & SOLD REPORT          MM/DD/YY
*PAGE: 123                                                                               HH:MM:SS
*
*------------------------------------------------------------------------------------------------
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*XXXXXXX XXXXXXXXXXXXXXXXXXXX       XXX      XXX      XXX      XXX      XXX      XXX      XXX      XXX      XXX      XXX     TOTAL
*PO #   VENDOR COMPLETE
*XXXXXX XXXXX  99/99/99         9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*     P.O. Subtotals ........  99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 999999999
*
*CUTTKT CONTR1 COMPLETE
*XXXXXX XXXXX  99/99/99         9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*     Cuttkt Subtotals ......  99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 999999999
*
*ORDER  ACCT#  COMPLETE
*XXXXXX XXXXX  99/99/99         9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*     Orders Subtotals ......  99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 999999999
*
*            STOCK  .........   9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*            W.I.P  .........   9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*            ORDERS .........   9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*            =====================================================================================================================
*            O.T.S. .........  99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 999999999
*
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*                            ====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*===
*                            GRAND TOTALS:  STOCK: 12345678   W.I.P: 12345678   ORDERS: 12345678   O.T.S: 12345678
*                            ====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*===

*** Declare & Initialize Variables & Array
STORE SPACE(6)  TO XCUTTKT, XPO, XORDER
STORE SPACE(5)  TO XVENDOR, XACCOUNT, XCONTR1
STORE SPACE(6)  TO XCOLOR         &&TAK06/21/94
STORE CTOD('  /  /  ') TO XDATE
STORE 0 TO XCLRCNT, XCNT, XGRDSTK, XGRDWIP, XGRDORD
DIMENSION XCOLOR(10),XCLR(10),XSTK(10),XCUT(10),XSUBCUT(10),XPOS(10),;
          XSUBPOS(10),XORD(10),XSUBORD(10),XOTS(10)

*** Determine Which Index, Seek, and Scan To Use (WIP)
DO CASE
  CASE lcRpWipSor='D'                          && Sort by Completion Date
    XCUTSCAN = '(CUTTKTH.COMPLETE=XDATE .AND. CUTTKT=XCUTTKT) FOR SUBSTR(STYLE,1,LEN(lcStyPic))=XSTYLE'
    XPOSCAN  = '(POSHDR.COMPLETE=XDATE .AND. PO=XPO) FOR SUBSTR(STYLE,1,LEN(lcStyPic))=XSTYLE'
    XCUTHDR  = 'COMPLETE CUTTKT CONTR1'
    XPOHDR   = 'COMPLETE PO #   VENDOR'
  CASE lcRpWipSor='F'                          && Sort by Contractor
    XCUTSCAN='(CONTR1=XCONTR1 .AND. CUTTKT=XCUTTKT) FOR SUBSTR(STYLE,1,LEN(lcStyPic))=XSTYLE'
    XPOSCAN='(POSHDR.VENDOR=XVENDOR .AND. PO=XPO) FOR SUBSTR(STYLE,1,LEN(lcStyPic))=XSTYLE'
    XCUTHDR='CONTR1 CUTTKT COMPLETE'
    XPOHDR='VENDOR PO #   COMPLETE'
  OTHERWISE                                 && Sort by Cutting Ticket/PO
    XCUTSCAN='(CUTTKTH.CUTTKT=XCUTTKT) FOR SUBSTR(STYLE,1,LEN(lcStyPic))=XSTYLE'
    XPOSCAN='(PO=XPO) FOR SUBSTR(STYLE,1,LEN(lcStyPic))=XSTYLE'
    XCUTHDR='CUTTKT CONTR1 COMPLETE'
    XPOHDR='PO #   VENDOR COMPLETE'
ENDCASE

*** Determine Which Index, Seek, and Scan To Use (ORDERS)
DO CASE
  CASE lcRpSalSor='D'                         && Sort by Completion Date
    XORDSCAN='(COMPLETE=XDATE .AND. ORDER=XORDER) FOR SUBSTR(STYLE,1,LEN(lcStyPic))=XSTYLE'
    XORDHDR='COMPLETE ORDER   ACCT#'
  CASE lcRpSalSor='A'                         && Sort by Account
    XORDSCAN='(ACCOUNT=XACCOUNT .AND. ORDER=XORDER) FOR SUBSTR(STYLE,1,LEN(lcStyPic))=XSTYLE'
    XORDHDR='ACCT# ORDER   COMPLETE'
  OTHERWISE                                 && Sort by Orders
    XORDSCAN='(ORDER=XORDER) FOR SUBSTR(STYLE,1,LEN(lcStyPic))=XSTYLE'
    XORDHDR='ORDER  ACCT#  COMPLETE'
ENDCASE
SELE STYLE
SET FILTER TO &lcRpExp
LOCATE
DO WHILE INKEY() <> 32 
  WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
  XSTYLE=SUBSTR(STYLE,1,LEN(lcStyPic))
  XDESC=DESC
  STORE SPACE(5) TO XCLR
  STORE SPACE(6) TO XCOLOR         &&TAK06/21/94
  STORE 0 TO XSTK, XCUT, XSUBCUT, XTOTCUT, XPOS, XSUBPOS, XTOTPOS, XORD,;
             XSUBORD, XTOTORD, XOTS
  *** To Get Number of Color For Each Style
  COUNT ALL FOR (SUBSTR(STYLE,1,LEN(lcStyPic))=XSTYLE) TO XCLRCNT
  *** Load Array With Color and Stock
  =SEEK(XSTYLE)                               && To Place Pointer Back to 1st Match
  XCNT=IIF(XCLRCNT>10, 10, XCLRCNT)
  FOR I=1 TO XCNT
    XCLR(I) = SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)
    XCOLOR(I) = SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)
    XSTK(I) = TOTSTK
    SKIP
  ENDFOR
  IF XCLRCNT>10
    XCLR(10) = 'OTHER'
    FOR I=11 TO XCLRCNT
      XSTK(10)=XSTK(10)+TOTSTK
      SKIP
    ENDFOR
  ENDIF

  IF ROW > 54
    ROW=0
    PAGENO=PAGENO+1
    DO RPT_HDR WITH 'ICAND100',lcRpTitle,R_WIDTH
    ROW=5
  ENDIF
  *** Display Style, Description, And Range Of Colors
  @ ROW,01 SAY XSTYLE+' '+SUBSTR(XDESC,1,17)
  COL=34
  FOR I=1 TO XCNT
    IF LEN(TRIM(XCLR(I))) < 4
      XCLR(I)=IIF(LEN(TRIM(XCLR(I)))=1,'    '+TRIM(XCLR(I)),XCLR(I))
      XCLR(I)=IIF(LEN(TRIM(XCLR(I)))=2,'   '+TRIM(XCLR(I)),XCLR(I))
      XCLR(I)=IIF(LEN(TRIM(XCLR(I)))=3,'  '+TRIM(XCLR(I)),XCLR(I))
    ENDIF
    @ ROW,COL SAY XCLR(I)
    COL=COL+9
  ENDFOR
  IF ROW=5
    @ ROW,125 SAY 'TOTAL'
  ENDIF
  ROW=ROW+1
  @ ROW,01 SAY REPLICATE('Ä',129)

  *** Get WIP From Cutting Ticket Temporary File
  SELECT (lcCutTemp)
  GO TOP
  IF SEEK(XSTYLE)
    XFIRSTTIME=.T.
    DO WHILE INKEY()<>32
      IF ROW > 54 .AND. !EOF()
        ROW=0
        PAGENO=PAGENO+1
        DO RPT_HDR WITH 'ICAND100',lcRpTitle,R_WIDTH
        ROW=5
        *** Display Style, Description, And Range Of Colors
        @ ROW,01 SAY XSTYLE+' '+SUBSTR(XDESC,1,17)
        COL=34
        FOR I=1 TO XCNT
          @ ROW,COL SAY XCLR(I)
          COL=COL+9
        ENDFOR
        @ ROW,125 SAY 'TOTAL'
        ROW=ROW+1
        @ ROW,01 SAY REPLICATE('Ä',129)
        XFIRSTTIME=.T.
      ENDIF

      XCUTTKT=CUTTKT
      XDATE=CUTTKTH.COMPLETE
      XCONTR1=CONTR1

      SCAN WHILE &XCUTSCAN
        DO CASE
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(1)
            XCUT(1)=IIF(TRANCD='1',XCUT(1)+TOTQTY, XCUT(1)-TOTQTY)
            XCUT(1)=IIF(XCUT(1)>0, XCUT(1), 0)
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(2)
            XCUT(2)=IIF(TRANCD='1',XCUT(2)+TOTQTY, XCUT(2)-TOTQTY)
            XCUT(2)=IIF(XCUT(2)>0, XCUT(2), 0)
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(3)
            XCUT(3)=IIF(TRANCD='1',XCUT(3)+TOTQTY, XCUT(3)-TOTQTY)
            XCUT(3)=IIF(XCUT(3)>0, XCUT(3), 0)
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(4)
            XCUT(4)=IIF(TRANCD='1',XCUT(4)+TOTQTY, XCUT(4)-TOTQTY)
            XCUT(4)=IIF(XCUT(4)>0, XCUT(4), 0)
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(5)
            XCUT(5)=IIF(TRANCD='1',XCUT(5)+TOTQTY, XCUT(5)-TOTQTY)
            XCUT(5)=IIF(XCUT(5)>0, XCUT(5), 0)
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(6)
            XCUT(6)=IIF(TRANCD='1',XCUT(6)+TOTQTY, XCUT(6)-TOTQTY)
            XCUT(6)=IIF(XCUT(6)>0, XCUT(6), 0)
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(7)
            XCUT(7)=IIF(TRANCD='1',XCUT(7)+TOTQTY, XCUT(7)-TOTQTY)
            XCUT(7)=IIF(XCUT(7)>0, XCUT(7), 0)
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(8)
            XCUT(8)=IIF(TRANCD='1',XCUT(8)+TOTQTY, XCUT(8)-TOTQTY)
            XCUT(8)=IIF(XCUT(8)>0, XCUT(8), 0)
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(9)
            XCUT(9)=IIF(TRANCD='1',XCUT(9)+TOTQTY, XCUT(9)-TOTQTY)
            XCUT(9)=IIF(XCUT(9)>0, XCUT(9), 0)
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(10)
            XCUT(10)=IIF(TRANCD='1',XCUT(10)+TOTQTY, XCUT(10)-TOTQTY)
            XCUT(10)=IIF(XCUT(10)>0, XCUT(10), 0)
          OTHERWISE
            XCUT(10)=IIF(TRANCD='1',XCUT(10)+TOTQTY, XCUT(10)-TOTQTY)
            XCUT(10)=IIF(XCUT(10)>0, XCUT(10), 0)
        ENDCASE
      ENDSCAN

      FOR I=1 TO XCNT
        XTOTCUT=XTOTCUT+XCUT(I)
        XSUBCUT(I)=XSUBCUT(I)+XCUT(I)
      ENDFOR

      IF llWipRpt .AND. XTOTCUT>0
        *** Display WIP Heading
        IF XFIRSTTIME
          ROW=ROW+1
          @ ROW,01 SAY XCUTHDR
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('Ä',22)
          XFIRSTTIME=.F.
        ENDIF
        ROW=ROW+1
        DO CASE
          CASE lcRpWipSor='D'
            @ ROW,01 SAY XDATE
            @ ROW,10 SAY XCUTTKT
            @ ROW,17 SAY XCONTR1
          CASE lcRpWipSor='F'
            @ ROW,01 SAY XCONTR1
            @ ROW,08 SAY XCUTTKT
            @ ROW,15 SAY XDATE
          OTHERWISE
            @ ROW,01 SAY XCUTTKT
            @ ROW,08 SAY XCONTR1
            @ ROW,15 SAY XDATE
        ENDCASE

        COL=32
        *** Display WIP Total Line Item
        FOR I=1 TO XCNT
          @ ROW,COL SAY XCUT(I)  PICTURE '9999999'
          COL=COL+9
        ENDFOR
        @ ROW,122 SAY XTOTCUT  PICTURE '99999999'
      ENDIF

      IF EOF() .OR. SUBSTR(STYLE,1,LEN(lcStyPic))<>XSTYLE
        EXIT
      ENDIF
      STORE 0 TO XCUT, XTOTCUT
    ENDDO

    IF llWipRpt
      COL=31
      XSUBTOT=0
      ROW=ROW+1
      @ ROW,31 SAY REPLICATE('Ä',99)
      ROW=ROW+1
      @ ROW,06 SAY 'Cuttkt Subtotals ......'
      FOR I=1 TO XCNT
        @ ROW,COL SAY XSUBCUT(I)  PICTURE '99999999'
        XSUBTOT=XSUBTOT+XSUBCUT(I)
        COL=COL+9
      ENDFOR
      @ ROW,121 SAY XSUBTOT  PICTURE '999999999'
      ROW=ROW+1
    ENDIF
  ENDIF

  *** Get WIP From Purchase Order Temporary File
  SELECT (lcPoTemp)
  GO TOP
  IF SEEK(XSTYLE)
    XFIRSTTIME=.T.
    DO WHILE INKEY()<>32
      IF ROW > 54 .AND. !EOF()
        ROW=0
        PAGENO=PAGENO+1
        DO RPT_HDR WITH 'ICAND100',lcRpTitle,R_WIDTH
        ROW=5
        *** Display Style, Description, And Range Of Colors
        @ ROW,01 SAY XSTYLE+' '+SUBSTR(XDESC,1,17)
        COL=34
        FOR I=1 TO XCNT
          @ ROW,COL SAY XCLR(I)
          COL=COL+9
        ENDFOR
        @ ROW,125 SAY 'TOTAL'
        ROW=ROW+1
        @ ROW,01 SAY REPLICATE('Ä',129)
        XFIRSTTIME=.T.
      ENDIF
      XPO=PO
      XVENDOR=VENDOR
      XDATE=POSHDR.COMPLETE

      SCAN WHILE &XPOSCAN
        IF TRANCD<>'3'                      && Not In-Transit
          DO CASE
            CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(1)
              XPOS(1)=IIF(TRANCD='1',XPOS(1)+TOTQTY, XPOS(1)-TOTQTY)
              XPOS(1)=IIF(XPOS(1)>0, XPOS(1), 0)
            CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(2)
              XPOS(2)=IIF(TRANCD='1',XPOS(2)+TOTQTY, XPOS(2)-TOTQTY)
              XPOS(2)=IIF(XPOS(2)>0, XPOS(2), 0)
            CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(3)
              XPOS(3)=IIF(TRANCD='1',XPOS(3)+TOTQTY, XPOS(3)-TOTQTY)
              XPOS(3)=IIF(XPOS(3)>0, XPOS(3), 0)
            CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(4)
              XPOS(4)=IIF(TRANCD='1',XPOS(4)+TOTQTY, XPOS(4)-TOTQTY)
              XPOS(4)=IIF(XPOS(4)>0, XPOS(4), 0)
            CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(5)
              XPOS(5)=IIF(TRANCD='1',XPOS(5)+TOTQTY, XPOS(5)-TOTQTY)
              XPOS(5)=IIF(XPOS(5)>0, XPOS(5), 0)
            CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(6)
              XPOS(6)=IIF(TRANCD='1',XPOS(6)+TOTQTY, XPOS(6)-TOTQTY)
              XPOS(6)=IIF(XPOS(6)>0, XPOS(6), 0)
            CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(7)
              XPOS(7)=IIF(TRANCD='1',XPOS(7)+TOTQTY, XPOS(7)-TOTQTY)
              XPOS(7)=IIF(XPOS(7)>0, XPOS(7), 0)
            CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(8)
              XPOS(8)=IIF(TRANCD='1',XPOS(8)+TOTQTY, XPOS(8)-TOTQTY)
              XPOS(8)=IIF(XPOS(8)>0, XPOS(8), 0)
            CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(9)
              XPOS(9)=IIF(TRANCD='1',XPOS(9)+TOTQTY, XPOS(9)-TOTQTY)
              XPOS(9)=IIF(XPOS(9)>0, XPOS(9), 0)
            CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(10)
              XPOS(10)=IIF(TRANCD='1',XPOS(10)+TOTQTY, XPOS(10)-TOTQTY)
              XPOS(10)=IIF(XPOS(10)>0, XPOS(10), 0)
            OTHERWISE
              XPOS(10)=IIF(TRANCD='1',XPOS(10)+TOTQTY, XPOS(10)-TOTQTY)
              XPOS(10)=IIF(XPOS(10)>0, XPOS(10), 0)
          ENDCASE
        ENDIF
      ENDSCAN

      FOR I=1 TO XCNT
        XTOTPOS=XTOTPOS+XPOS(I)
        XSUBPOS(I)=XSUBPOS(I)+XPOS(I)
      ENDFOR

      IF llWipRpt .AND. XTOTPOS>0
        *** Display WIP Heading
        IF XFIRSTTIME
          ROW=ROW+1
          @ ROW,01 SAY XPOHDR
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('Ä',22)
          XFIRSTTIME=.F.
        ENDIF
        ROW=ROW+1
        DO CASE
          CASE lcRpWipSor='D'
            @ ROW,01 SAY XDATE
            @ ROW,10 SAY XPO
            @ ROW,17 SAY XVENDOR
          CASE lcRpWipSor='F'
            @ ROW,01 SAY XVENDOR
            @ ROW,08 SAY XPO
            @ ROW,15 SAY XDATE
          OTHERWISE
            @ ROW,01 SAY XPO
            @ ROW,08 SAY XVENDOR
            @ ROW,15 SAY XDATE
        ENDCASE

        COL=32
        *** Display WIP Total Line Item
        FOR I=1 TO XCNT
          @ ROW,COL SAY XPOS(I)  PICTURE '9999999'
          COL=COL+9
        ENDFOR
        @ ROW,122 SAY XTOTPOS  PICTURE '99999999'
      ENDIF

      IF EOF() .OR. SUBSTR(STYLE,1,LEN(lcStyPic))<>XSTYLE
        EXIT
      ENDIF
      STORE 0 TO XPOS, XTOTPOS
    ENDDO

    IF llWipRpt
      COL=31
      XSUBTOT=0
      ROW=ROW+1
      @ ROW,31 SAY REPLICATE('Ä',99)
      ROW=ROW+1
      @ ROW,06 SAY 'P.O. Subtotals ........'
      FOR I=1 TO XCNT
        @ ROW,COL SAY XSUBPOS(I)  PICTURE '99999999'
        XSUBTOT=XSUBTOT+XSUBPOS(I)
        COL=COL+9
      ENDFOR
      @ ROW,121 SAY XSUBTOT  PICTURE '999999999'
      ROW=ROW+1
    ENDIF
  ENDIF

  *** Get Sales From Orders Temporary File
  SELECT (lcOrdTemp)
  GO TOP
  IF SEEK(XSTYLE)
    XFIRSTTIME=.T.
    DO WHILE INKEY()<>32
      IF ROW > 54 .AND. !EOF()
        ROW=0
        PAGENO=PAGENO+1
        DO RPT_HDR WITH 'ICAND100',lcRpTitle,R_WIDTH
        ROW=5
        *** Display Style, Description, And Range Of Colors
        @ ROW,01 SAY XSTYLE+' '+SUBSTR(XDESC,1,17)
        COL=34
        FOR I=1 TO XCNT
          @ ROW,COL SAY XCLR(I)
          COL=COL+9
        ENDFOR
        @ ROW,125 SAY 'TOTAL'
        ROW=ROW+1
        @ ROW,01 SAY REPLICATE('Ä',129)
        XFIRSTTIME=.T.
      ENDIF

      XORDER=ORDHDR.ORDER
      XACCOUNT=ACCOUNT
      XDATE=COMPLETE

      SCAN WHILE &XORDSCAN
        DO CASE
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(1)
            XORD(1)=XORD(1)+TOTQTY
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(2)
            XORD(2)=XORD(2)+TOTQTY
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(3)
            XORD(3)=XORD(3)+TOTQTY
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(4)
            XORD(4)=XORD(4)+TOTQTY
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(5)
            XORD(5)=XORD(5)+TOTQTY
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(6)
            XORD(6)=XORD(6)+TOTQTY
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(7)
            XORD(7)=XORD(7)+TOTQTY
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(8)
            XORD(8)=XORD(8)+TOTQTY
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(9)
            XORD(9)=XORD(9)+TOTQTY
          CASE SUBSTR(STYLE,lnNonMajSt,lnColorLen)=XCOLOR(10)
            XORD(10)=XORD(10)+TOTQTY
          OTHERWISE
            XORD(10)=XORD(10)+TOTQTY
        ENDCASE
      ENDSCAN

      FOR I=1 TO XCNT
        XTOTORD=XTOTORD+XORD(I)
        XSUBORD(I)=XSUBORD(I)+XORD(I)
      ENDFOR

      IF llOrdRpt .AND. XTOTORD>0
        *** Display ORD Heading
        IF XFIRSTTIME
          ROW=ROW+1
          @ ROW,01 SAY XORDHDR
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('Ä',22)
          XFIRSTTIME=.F.
        ENDIF
        ROW=ROW+1
        DO CASE
          CASE lcRpSalSor='D'
            @ ROW,01 SAY XDATE
            @ ROW,10 SAY XORDER
            @ ROW,18 SAY XACCOUNT
          CASE lcRpSalSor='A'
            @ ROW,01 SAY XACCOUNT
            @ ROW,07 SAY XORDER
            @ ROW,15 SAY XDATE
          OTHERWISE
            @ ROW,01 SAY XORDER
            @ ROW,08 SAY XACCOUNT
            @ ROW,15 SAY XDATE
        ENDCASE

        COL=32
        *** Display ORD Total Line Item
        FOR I=1 TO XCNT
          @ ROW,COL SAY XORD(I) PICTURE '9999999'
          COL=COL+9
        ENDFOR
        @ ROW,122 SAY XTOTORD  PICTURE '99999999'
      ENDIF

      IF EOF() .OR. SUBSTR(STYLE,1,LEN(lcStyPic))<>XSTYLE
        EXIT
      ENDIF
      STORE 0 TO XORD, XTOTORD
    ENDDO

    IF llOrdRpt
      COL=31
      XSUBTOT=0
      ROW=ROW+1
      @ ROW,31 SAY REPLICATE('Ä',99)
      ROW=ROW+1
      @ ROW,06 SAY 'Orders Subtotals ......'
      FOR I=1 TO XCNT
        @ ROW,COL SAY XSUBORD(I)  PICTURE '99999999'
        XSUBTOT=XSUBTOT+XSUBORD(I)
        COL=COL+9
      ENDFOR
      @ ROW,121 SAY XSUBTOT  PICTURE '999999999'
      ROW=ROW+1
    ENDIF
  ENDIF

  *** Display Summary Information
  STORE 0 TO XTOTSTK, XTOTWIP, XTOTORD, XTOTOTS

  IF ROW > 49
    ROW=0
    PAGENO=PAGENO+1
    DO RPT_HDR WITH 'ICAND100',lcRpTitle,R_WIDTH
    ROW=5
    @ ROW,01 SAY XSTYLE+' '+SUBSTR(XDESC,1,17)
    COL=34
    FOR I=1 TO XCNT
      @ ROW,COL SAY XCLR(I)
      COL=COL+9
    ENDFOR
    @ ROW,125 SAY 'TOTAL'
    ROW=ROW+1
    @ ROW,01 SAY REPLICATE('Ä',129)
  ENDIF

  COL=32
  ROW=ROW+1
  @ ROW,13 SAY 'STOCK  .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XSTK(I)  PICTURE '9999999'
    XTOTSTK=XTOTSTK+XSTK(I)
    XOTS(I)=XOTS(I)+XSTK(I)
    COL=COL+9
  ENDFOR
  @ ROW,122 SAY XTOTSTK    PICTURE '99999999'
  XGRDSTK=XGRDSTK+XTOTSTK

  ROW=ROW+1
  COL=32
  @ ROW,13 SAY 'W.I.P  .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XSUBCUT(I)+XSUBPOS(I)  PICTURE '9999999'
    XTOTWIP=XTOTWIP+XSUBCUT(I)+XSUBPOS(I)
    XOTS(I)=XOTS(I)+XSUBCUT(I)+XSUBPOS(I)
    COL=COL+9
  ENDFOR
  @ ROW,122 SAY XTOTWIP       PICTURE '99999999'
  XGRDWIP=XGRDWIP+XTOTWIP

  ROW=ROW+1
  COL=32
  @ ROW,13 SAY 'ORDERS .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XSUBORD(I)  PICTURE '9999999'
    XTOTORD=XTOTORD+XSUBORD(I)
    XOTS(I)=XOTS(I)-XSUBORD(I)
    COL=COL+9
  ENDFOR
  @ ROW,122 SAY XTOTORD       PICTURE '99999999'
  XGRDORD=XGRDORD+XTOTORD

  ROW=ROW+1
  @ ROW,13 SAY REPLICATE('Í',117)
  ROW=ROW+1
  COL=31
  @ ROW,13 SAY 'O.T.S. .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XOTS(I)     PICTURE '99999999'
    XTOTOTS=XTOTOTS+XOTS(I)
    COL=COL+9
  ENDFOR
  @ ROW,121 SAY XTOTOTS       PICTURE '999999999'

  SELE STYLE
  SCAN WHILE SUBSTR(STYLE,1,LEN(lcStyPic))=XSTYLE
  ENDSCAN
  IF EOF()
    ROW=ROW+2
    IF ROW > 52
      ROW=0
      PAGENO=PAGENO+1
      DO RPT_HDR WITH 'ICAND100',lcRpTitle,R_WIDTH
    ENDIF
    @ ROW,29 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*==='
    ROW=ROW+1
    @ ROW,29 SAY 'GRAND TOTALS:  STOCK:'
    @ ROW,51 SAY XGRDSTK  PICTURE '99999999'
    @ ROW,62 SAY 'W.I.P:'
    @ ROW,69 SAY XGRDWIP  PICTURE '99999999'
    @ ROW,80 SAY 'ORDERS:'
    @ ROW,88 SAY XGRDORD  PICTURE '99999999'
    @ ROW,99 SAY 'O.T.S:'
    @ ROW,106 SAY XGRDSTK+XGRDWIP-XGRDORD  PICTURE '99999999'
    ROW=ROW+1
    @ ROW,29 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*==='
    EXIT
  ENDIF
  ROW=ROW+3
ENDDO
RETURN


****************************************************************************
* PROG : STY962.PRG
* DATE : 02/20/92
* DESC : REPORT WITH SIZE BREAKDOWN
* MODI : 01/11/94 YMA
****************************************************************************
PROCEDURE STY962
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*JNL330              123456789012345678901234567890 - SPECIAL CUT & SOLD REPORT          MM/DD/YY
*PAGE: 123                                                                               HH:MM:SS
*
*------------------------------------------------------------------------------------------------
*XXXXXXX XXX XXXXXXXXXXXXXXXXXXXX                 XXX    XXX    XXX    XXX    XXX    XXX    XXX    XXX   TOTAL   PRICE       EXT S P
*COMPLETE PO #  VENDOR NAME
*99/99/99 123456 12345 123456789012345678901234 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*VENDOR NAME                    PO #   COMPLETE
*12345  12345678901234567890123 123456 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*PO #  VENDOR NAME                     COMPLETE
*123456 12345 123456789012345678901234 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*                    P.O. Subtotals .......    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*
*COMPLETE CUTTKT CONTR NAME
*99/99/99 123456 12345 123456789012345678901234 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*CONTR NAME                     CUTTKT COMPLETE
*12345 123456789012345678901234 123456 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*CUTTKT CONTR NAME                     COMPLETE
*123456 12345 123456789012345678901234 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*                    Cuttkt Subtotals .....    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*
*START    COMPLETE ORDER  ACCT# NAME
*99/99/99 99/99/99 123456 12345 123456789012345 99999  99999  99999  99999  99999  99999  99999  99999  999999 1234.12 123456.12 X X
*ACCT# NAME            ORDER  START    COMPLETE
*12345 123456789012345 123456 99/99/99 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999 1234.12 123456.12 X X
*ORDER  ACCT# NAME            START    COMPLETE
*123456 12345 123456789012345 99/99/99 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999 1234.12 123456.12 X X
*                    Orders Subtotals .....    999999 999999 999999 999999 999999 999999 999999 999999 9999999        9999999.99
*
*                          STOCK  .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*                          W.I.P  .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*                          ORDERS .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*                          ===================================================================================
*                          O.T.S. .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*                          ====*====*====*====*====*====*====*====*====*====*====*====*====*====
*                          TOTAL PIECES ORDERED: 12345678     TOTAL $$$ ORDERED $ 999,999,999.99
*                          ====*====*====*====*====*====*====*====*====*====*====*====*====*====
*** Initialize Variables
STORE SPACE(30) TO XNAME
STORE SPACE(6)  TO XCUTTKT, XPO, XORDER
STORE SPACE(5)  TO XVENDOR, XACCOUNT, XCONTR1
STORE SPACE(6)  TO XCOLOR         &&TAK06/21/94
STORE SPACE(1)  TO lcStat, XPIKTKT
STORE CTOD('  /  /  ') TO XDATE, XSTART, XAVAIL
STORE 0 TO XCNT, XPRICE, XGRDORD, XGRDPRI
DIMENSION XSTK(8),XOTS(8),XSIZE(8),XCUT(8),XSUBCUT(8),XPOS(8),XSUBPOS(8),XORD(8),XSUBORD(8)

*** Determine Which Index, Seek, and Scan To Use (WIP)
DO CASE
  CASE lcRpWipSor='D'                          && Sort by Completion Date
    XCUTSCAN='(STYLE+DTOS(CUTTKTH.COMPLETE)+CUTTKT)=(lcStyle+DTOS(XDATE)+XCUTTKT)'
    XCUTHDR='COMPLETE CUTTKT CONTR NAME'
    XPOSCAN='(STYLE+DTOS(POSHDR.COMPLETE)+PO)=(lcStyle+DTOS(XDATE)+XPO)'
    XPOHDR='COMPLETE PO #  VENDOR NAME            AVAILABLE'
  CASE lcRpWipSor='F'                          && Sort by Contractor
    XCUTSCAN='(STYLE+CONTR1+CUTTKT)=(lcStyle+XCONTR1+XCUTTKT)'
    XCUTHDR='CONTR NAME'+SPACE(21)+'CUTTKT COMPLETE'
    XPOSCAN='(STYLE+VENDOR+PO)=(lcStyle+XVENDOR+XPO)'
    XPOHDR='VENDOR NAME'+SPACE(20)+'PO #   COMPLETE'
  OTHERWISE                                  && Sort by Cutting Ticket/PO
    XCUTSCAN='(STYLE+CUTTKT)=(lcStyle+XCUTTKT)'
    XCUTHDR='CUTTKT CONTR NAME'+SPACE(21)+'COMPLETE'
    XPOSCAN='(STYLE+PO)=(lcStyle+XPO)'
    XPOHDR='PO #  VENDOR NAME'+SPACE(21)+'COMPLETE'
ENDCASE

*** Determine Which Index, Seek, and Scan To Use (ORDERS)
DO CASE
  CASE lcRpSalSor='D'                         && Sort by Completion Date
    XORDHDR='START    COMPLETE ORDER  ACCT# NAME'
  CASE lcRpSalSor='A'                         && Sort by Account
    XORDHDR='ACCT# NAME'+SPACE(12)+'ORDER  START    COMPLETE'
  OTHERWISE                                  && Sort by Orders
    XORDHDR='ORDER  ACCT# NAME'+SPACE(12)+'START    COMPLETE'
ENDCASE

SELE STYLE
SET FILTER TO &lcRpExp
SET RELATION TO 'S'+SCALE INTO SCALE
LOCATE

DO WHILE INKEY() <> 32
  XSTYLE = STYLE
  XDESC  = DESC
  XSCALE = SCALE
  XCOLOR = SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)
  lcStyle = style
  STORE SPACE(3) TO XSIZE
  STORE 0 TO XSTK, XOTS, XCUT, XSUBCUT, XTOTCUT, XPOS, XSUBPOS, XTOTPOS, XORD, XSUBORD, XTOTORD, XSUBEXT

  *** To Get Number of Size Breakdown For Each Style
  XCNT = SCALE.CNT

  *** Load Array With Scale
  FOR I=1 TO XCNT
    Z=STR(I,1)
    XSIZE(I) = SCALE.SZ&Z
    XSTK(I) = STK&Z
  ENDFOR

  IF ROW > 54
    ROW=0
    PAGENO=PAGENO+1
    DO RPT_HDR WITH 'ICAND100',lcRpTitle,R_WIDTH
    ROW=5
  ENDIF

  *** Display Style, Description, And Scale Breakdown
  lcClrDesc = gfCodDes(SUBSTR(STYLE,LEN(lcStyPic)) , 'COLOR')
  @ ROW,01 SAY lcStyle+' '+ALLTRIM(lcClrDesc)+' '+SUBSTR(XDESC,1,13)
  SELECT STYLE

  COL=50
  FOR I=1 TO XCNT
    IF LEN(TRIM(XSIZE(I))) < 3
      XSIZE(I)=IIF(LEN(TRIM(XSIZE(I)))=1,'  '+TRIM(XSIZE(I)),XSIZE(I))
      XSIZE(I)=IIF(LEN(TRIM(XSIZE(I)))=2,' '+TRIM(XSIZE(I)),XSIZE(I))
    ENDIF
    @ ROW,COL SAY XSIZE(I)
    COL=COL+7
  ENDFOR
  IF ROW=5
    @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
  ENDIF

  ROW=ROW+1
  @ ROW,01 SAY REPLICATE('Ä',131)

  *** Get WIP From Cutting Ticket Temporary File
  SELECT (lcCutTemp)
  GO TOP
  IF SEEK(lcStyle)
    XFIRSTTIME=.T.
    DO WHILE INKEY()<>32
      IF ROW > 54 .AND. !EOF()
        ROW=0
        PAGENO=PAGENO+1
        DO RPT_HDR WITH 'ICAND100',lcRpTitle,R_WIDTH
        ROW=5
        *** Display Style, Description, And Scale Breakdown
        lcClrDesc = gfCodDes(SUBSTR(STYLE,LEN(lcStyPic)) , 'COLOR')
        @ ROW,01 SAY lcStyle+' '+ALLTRIM(lcClrDesc)+' '+SUBSTR(XDESC,1,13)
        SELECT (lcCutTemp)
        COL=50
        FOR I=1 TO XCNT
          @ ROW,COL SAY XSIZE(I)
          COL=COL+7
        ENDFOR
        @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
        ROW=ROW+1
        @ ROW,01 SAY REPLICATE('Ä',131)
        XFIRSTTIME=.T.
      ENDIF

      XCUTTKT=CUTTKT
      XDATE=CUTTKTH.COMPLETE
      XCONTR1=CONTR1
      lcStat=CUTTKTH.STATUS

      XNAME=IIF(SEEK(XCONTR1,'apvendor'),apvendor.CVENCOMP,SPACE(30))
      SELECT (lcCutTemp)
      SCAN WHILE &XCUTSCAN
        FOR I=1 TO XCNT
          Z=STR(I,1)
          XCUT(I) = IIF(TRANCD='1', XCUT(I)+QTY&Z, XCUT(I)-QTY&Z)
          XCUT(I) = IIF(XCUT(I)>0, XCUT(I), 0)
        ENDFOR
      ENDSCAN

      FOR I=1 TO XCNT
        XTOTCUT=XTOTCUT+XCUT(I)
        XSUBCUT(I)=XSUBCUT(I)+XCUT(I)
      ENDFOR

      IF llWipRpt .AND. XTOTCUT>0
        *** Display WIP Heading
        IF XFIRSTTIME
          ROW=ROW+1
          @ ROW,01 SAY XCUTHDR
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('Ä',46)
          XFIRSTTIME=.F.
        ENDIF
        ROW=ROW+1
        DO CASE
          CASE lcRpWipSor='D'
            @ ROW,01 SAY XDATE
            @ ROW,10 SAY XCUTTKT
            @ ROW,17 SAY XCONTR1
            @ ROW,23 SAY SUBSTR(XNAME,1,24)
          CASE lcRpWipSor='F'
            @ ROW,01 SAY XCONTR1
            @ ROW,07 SAY SUBSTR(XNAME,1,24)
            @ ROW,32 SAY XCUTTKT
            @ ROW,39 SAY XDATE
          OTHERWISE
            @ ROW,01 SAY XCUTTKT
            @ ROW,08 SAY XCONTR1
            @ ROW,14 SAY SUBSTR(XNAME,1,24)
            @ ROW,39 SAY XDATE
        ENDCASE

        COL=48
        *** Display WIP Total Line Item
        FOR I=1 TO XCNT
          @ ROW,COL SAY XCUT(I)  PICTURE '99999'
          COL=COL+7
        ENDFOR
        @ ROW,104 SAY XTOTCUT  PICTURE '999999'
        @ ROW,129 SAY lcStat
      ENDIF

      IF EOF() .OR. STYLE<>lcStyle
        EXIT
      ENDIF
      STORE 0 TO XCUT, XTOTCUT
    ENDDO

    IF llWipRpt
      COL=47
      XSUBTOT=0
      ROW=ROW+1
      @ ROW,47 SAY REPLICATE('Ä',85)
      ROW=ROW+1
      @ ROW,21 SAY 'Cuttkt Subtotals .....'
      FOR I=1 TO XCNT
        @ ROW,COL SAY XSUBCUT(I)  PICTURE '999999'
        XSUBTOT=XSUBTOT+XSUBCUT(I)
        COL=COL+7
      ENDFOR
      @ ROW,103 SAY XSUBTOT  PICTURE '9999999'
      ROW=ROW+1
    ENDIF
  ENDIF

  *** Get WIP From Purchase Order Temporary File
  SELECT (lcPoTemp)
  GO TOP
  
  IF SEEK(lcStyle)
    XFIRSTTIME=.T.
    DO WHILE INKEY()<>32
      IF ROW > 54 .AND. !EOF()
        ROW=0
        PAGENO=PAGENO+1
        DO RPT_HDR WITH 'ICAND100',lcRpTitle,R_WIDTH
        ROW=5
        *** Display Style, Description, And Scale Breakdown

        lcClrDesc = gfCodDes(SUBSTR(STYLE,LEN(lcStyPic)) , 'COLOR')        
        @ ROW,01 SAY lcStyle+' '+ALLTRIM(lcClrDesc)+' '+SUBSTR(XDESC,1,13)
        COL=50
        FOR I=1 TO XCNT
          @ ROW,COL SAY XSIZE(I)
          COL=COL+7
        ENDFOR
        @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
        ROW=ROW+1
        @ ROW,01 SAY REPLICATE('Ä',131)
        XFIRSTTIME=.T.
      ENDIF
      XPO=PO
      XVENDOR=VENDOR
      XDATE=POSHDR.COMPLETE
      lcStat=POSHDR.STATUS
      XAVAIL = POSHDR.Available
      XNAME=IIF(SEEK(XVENDOR,'apvendor'),apvendor.CVENCOMP,SPACE(30))      
      SCAN WHILE &XPOSCAN
        IF TRANCD<>'3'                    && trancd=3 -> In-transit
          FOR I=1 TO XCNT
            Z=STR(I,1)
            XPOS(I) = IIF(TRANCD='1', XPOS(I)+QTY&Z, XPOS(I)-QTY&Z)
            XPOS(I) = IIF(XPOS(I)>0, XPOS(I), 0)
          ENDFOR
        ENDIF
      ENDSCAN

      FOR I=1 TO XCNT
        XTOTPOS=XTOTPOS+XPOS(I)
        XSUBPOS(I)=XSUBPOS(I)+XPOS(I)
      ENDFOR

      IF llWipRpt .AND. XTOTPOS>0
        *** Display WIP Heading
        IF XFIRSTTIME
          ROW=ROW+1
          @ ROW,01 SAY XPOHDR
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('Ä',46)
          XFIRSTTIME=.F.
        ENDIF
        ROW=ROW+1
        DO CASE
          CASE lcRpWipSor='D'
            @ ROW,01 SAY XDATE
            @ ROW,10 SAY XPO
            @ ROW,17 SAY XVENDOR
            @ ROW,23 SAY SUBSTR(XNAME,1,14)
            @ ROW,38 SAY XAVAIL
          CASE lcRpWipSor='F'
            @ ROW,01 SAY XVENDOR
            @ ROW,08 SAY SUBSTR(XNAME,1,23)
            @ ROW,32 SAY XPO
            @ ROW,39 SAY XDATE
          OTHERWISE
            @ ROW,01 SAY XPO
            @ ROW,08 SAY XVENDOR
            @ ROW,14 SAY SUBSTR(XNAME,1,14)
            @ ROW,29 SAY XAVAIL
            @ ROW,39 SAY XDATE
        ENDCASE

        COL=48
        *** Display WIP Total Line Item
        FOR I=1 TO XCNT
          @ ROW,COL SAY XPOS(I)  PICTURE '99999'
          COL=COL+7
        ENDFOR
        @ ROW,104 SAY XTOTPOS  PICTURE '999999'
        @ ROW,129 SAY lcStat
      ENDIF

      IF EOF() .OR. STYLE<>lcStyle
        EXIT
      ENDIF
      STORE 0 TO XPOS, XTOTPOS
    ENDDO

    IF llWipRpt
      COL=47
      XSUBTOT=0
      ROW=ROW+1
      @ ROW,47 SAY REPLICATE('Ä',85)
      ROW=ROW+1
      @ ROW,21 SAY 'P.O. Subtotals .......'
      FOR I=1 TO XCNT
        @ ROW,COL SAY XSUBPOS(I)  PICTURE '999999'
        XSUBTOT=XSUBTOT+XSUBPOS(I)
        COL=COL+7
      ENDFOR
      @ ROW,103 SAY XSUBTOT  PICTURE '9999999'
      ROW=ROW+1
    ENDIF
  ENDIF

  *** Get Sales From Orders Temporary File
  SELECT (lcOrdTemp)
  GO TOP
  IF SEEK(lcStyle)
    XFIRSTTIME=.T.
    DO WHILE INKEY()<>32
      IF ROW > 54 .AND. !EOF()
        ROW=0
        PAGENO=PAGENO+1
        DO RPT_HDR WITH 'ICAND100',lcRpTitle,R_WIDTH
        ROW=5
        *** Display Style, Description, And Scale Breakdown

        lcClrDesc = gfCodDes(SUBSTR(STYLE,LEN(lcStyPic)) , 'COLOR')        
        @ ROW,01 SAY lcStyle+' '+ALLTRIM(lcClrDesc)+' '+SUBSTR(XDESC,1,13)
        COL=50
        FOR I=1 TO XCNT
          @ ROW,COL SAY XSIZE(I)
          COL=COL+7
        ENDFOR
        @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
        ROW=ROW+1
        @ ROW,01 SAY REPLICATE('Ä',131)
        XFIRSTTIME=.T.
      ENDIF

      XORDER=ORDER
      XACCOUNT=ACCOUNT
      XDATE=COMPLETE
      XSTART=START
      lcStat=IIF(ORDHDR.APPROVAL='DECLINE','D',ORDHDR.STATUS)
      XPIKTKT=IIF(PICKED=.T.,'Y',' ')
      XPRICE=PRICE

      XNAME=IIF(SEEK('M'+XACCOUNT,'CUSTOMER'),CUSTOMER.BTNAME,SPACE(30))

      FOR I=1 TO XCNT
        Z=STR(I,1)
        XORD(I)=IIF(QTY&Z>0,QTY&Z,0)
        XTOTORD=XTOTORD+XORD(I)
        XSUBORD(I)=XSUBORD(I)+XORD(I)
      ENDFOR
      XEXT=XTOTORD*XPRICE
      XSUBEXT=XSUBEXT+XEXT
      XGRDORD=XGRDORD+XTOTORD
      XGRDPRI=XGRDPRI+XEXT

      IF llOrdRpt .AND. XTOTORD>0
        *** Display ORD Heading
        IF XFIRSTTIME
          ROW=ROW+1
          @ ROW,01 SAY XORDHDR
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('Ä',46)
          XFIRSTTIME=.F.
        ENDIF

        ROW=ROW+1
        DO CASE
          CASE lcRpSalSor='D'
            @ ROW,01 SAY XSTART
            @ ROW,10 SAY XDATE
            @ ROW,19 SAY XORDER
            @ ROW,26 SAY XACCOUNT
            @ ROW,32 SAY SUBSTR(XNAME,1,15)
          CASE lcRpSalSor='A'
            @ ROW,01 SAY XACCOUNT
            @ ROW,07 SAY SUBSTR(XNAME,1,15)
            @ ROW,23 SAY XORDER
            @ ROW,30 SAY XSTART
            @ ROW,39 SAY XDATE
          OTHERWISE
            @ ROW,01 SAY XORDER
            @ ROW,08 SAY XACCOUNT
            @ ROW,14 SAY SUBSTR(XNAME,1,15)
            @ ROW,30 SAY XSTART
            @ ROW,39 SAY XDATE
        ENDCASE

        COL=48
        *** Display ORD Total Line Item
        FOR I=1 TO XCNT
          @ ROW,COL SAY XORD(I) PICTURE '99999'
          COL=COL+7
        ENDFOR
        @ ROW,104 SAY XTOTORD  PICTURE '999999'
        @ ROW,111 SAY XPRICE   PICTURE '9999.99'
        @ ROW,119 SAY XEXT     PICTURE '999999.99'
        @ ROW,129 SAY lcStat
        @ ROW,131 SAY XPIKTKT
      ENDIF
      SKIP
      IF EOF() .OR. STYLE<>lcStyle
        EXIT
      ENDIF
      STORE 0 TO XORD, XTOTORD, XEXT
    ENDDO

    IF llOrdRpt
      COL=47
      XSUBTOT=0
      ROW=ROW+1
      @ ROW,47 SAY REPLICATE('Ä',85)
      ROW=ROW+1
      @ ROW,21 SAY 'Orders Subtotals .....'
      FOR I=1 TO XCNT
        @ ROW,COL SAY XSUBORD(I)  PICTURE '999999'
        XSUBTOT=XSUBTOT+XSUBORD(I)
        COL=COL+7
      ENDFOR
      @ ROW,103 SAY XSUBTOT  PICTURE '9999999'
      @ ROW,118 SAY XSUBEXT  PICTURE '9999999.99'
      ROW=ROW+1
    ENDIF
  ENDIF

  *** Display Summary Information
  STORE 0 TO XTOTSTK, XTOTWIP, XTOTORD, XTOTOTS

  IF ROW > 49
    ROW=0
    PAGENO=PAGENO+1
    DO RPT_HDR WITH 'ICAND100',lcRpTitle,R_WIDTH
    ROW=5

    lcClrDesc = gfCodDes(SUBSTR(STYLE,LEN(lcStyPic)) , 'COLOR')        
    @ ROW,01 SAY lcStyle+' '+ALLTRIM(lcClrDesc)+' '+SUBSTR(XDESC,1,13)

    COL=50
    FOR I=1 TO XCNT
      @ ROW,COL SAY XSIZE(I)
      COL=COL+7
    ENDFOR
    @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
    ROW=ROW+1
    @ ROW,01 SAY REPLICATE('Ä',131)
  ENDIF

  COL=47
  ROW=ROW+1
  @ ROW,27 SAY 'STOCK  .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XSTK(I)  PICTURE '999999'
    XTOTSTK=XTOTSTK+XSTK(I)
    XOTS(I)=XOTS(I)+XSTK(I)
    COL=COL+7
  ENDFOR
  @ ROW,103 SAY XTOTSTK    PICTURE '9999999'

  ROW=ROW+1
  COL=47
  @ ROW,27 SAY 'W.I.P  .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XSUBCUT(I)+XSUBPOS(I)  PICTURE '999999'
    XTOTWIP=XTOTWIP+XSUBCUT(I)+XSUBPOS(I)
    XOTS(I)=XOTS(I)+XSUBCUT(I)+XSUBPOS(I)
    COL=COL+7
  ENDFOR
  @ ROW,103 SAY XTOTWIP       PICTURE '9999999'

  ROW=ROW+1
  COL=47
  @ ROW,27 SAY 'ORDERS .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XSUBORD(I)  PICTURE '999999'
    XTOTORD=XTOTORD+XSUBORD(I)
    XOTS(I)=XOTS(I)-XSUBORD(I)
    COL=COL+7
  ENDFOR
  @ ROW,103 SAY XTOTORD       PICTURE '9999999'

  ROW=ROW+1
  @ ROW,27 SAY REPLICATE('Í',105)
  ROW=ROW+1
  COL=47
  @ ROW,27 SAY 'O.T.S. .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XOTS(I)     PICTURE '999999'
    XTOTOTS=XTOTOTS+XOTS(I)
    COL=COL+7
  ENDFOR
  @ ROW,103 SAY XTOTOTS       PICTURE '9999999'

  SELE STYLE
  SKIP

  IF EOF()
    ROW=ROW+2
    IF ROW > 52
      ROW=0
      PAGENO=PAGENO+1
      DO RPT_HDR WITH 'ICAND100',lcRpTitle,R_WIDTH
    ENDIF
    @ ROW,27 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*===='
    ROW=ROW+1
    @ ROW,27 SAY 'TOTAL PIECES ORDERED:'
    @ ROW,49 SAY XGRDORD  PICTURE '99999999'
    @ ROW,62 SAY 'TOTAL $$$ ORDERED $'
    @ ROW,82 SAY XGRDPRI  PICTURE '999,999,999.99'
    ROW=ROW+1
    @ ROW,27 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*===='
    EXIT
  ENDIF
  ROW=ROW+3
ENDDO
RETURN

*!*************************************************************
*! Name      : lfStySum
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/18/2000
*! Purpose   : sum a specific field for the current style in style file
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
*-- end of lfStySum.

*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/18/2000
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : lcParm
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfsrvSty()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
* 
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
  CASE lcParm = 'R'  && Reset code
    SELECT STYLE
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/18/2000
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

****************************************************************************
* FUNC: lfwOldVal
* DESC: To get the old value.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* Date: 01/18/2000
****************************************************************************
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))



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
lcColorTt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- end of lfEvalSegs.

****************************************************************************
* FUNC: lfvWipSal
* DESC: To clear read.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* Date: 01/18/2000
****************************************************************************
FUNCTION lfvWipSal

CLEAR READ


FUNCTION lfGetDates
PARAMETERS lcDateExp,lcExp
*-- If the user enters dates for completion,Get the filter start position
lnStDPos = ATC(lcDateExp,lcRpExp)
IF lnStDPos > 0
  *--Get the end position for the expression.
  lnEndPos = ATC('AND',SUBSTR(lcRpExp,lnStDPos))
  *--Get the filter expression.
  lcExp    = SUBSTR(lcRpExp,lnStDPos,lnEndPos-1)
  *--Extract the filter expression from lcRpExp.
  lcRpExp  = STRTRAN(lcRpExp,SUBSTR(lcRpExp,lnStDPos,lnEndPos-1),'.T.')
ENDIF

****************************************************************************
* FUNC: lfCollect
* DESC: To collect data.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* Date: 01/18/2000
****************************************************************************
FUNCTION lfCollect

SELECT STYLE
SCAN WHILE INKEY()<>32 FOR &lcRpExp
  WAIT WINDOW 'Selecting Records - <Space Bar> To Abort '+lcStyMajor+'\'+SUBSTR(lcColorTt,11)+' ' +SUBSTR(STYLE,1,LEN(lcStyPic))+'\'+SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen) NOWAIT
  *--Cut lines
  lcStyle = Style
  IF SEEK(Style,'CUTTKTL')
    SELE CUTTKTL
    SCAN REST WHILE style+cuttkt+trancd =  lcStyle ;
              FOR CUTTKTH.STATUS<>'X' .AND. CUTTKTH.STATUS<>'C' .AND. TOTQTY<>0 AND &lcPrDtFlt
       SCATTER MEMVAR MEMO
       INSERT INTO (lcCutTemp) FROM MEMVAR
       IF SEEK('M'+CUTTKT,'mfgoprhd')
         REPLACE &lcCutTemp..CONTR1 WITH mfgoprhd.cContCode
       ENDIF
    ENDSCAN   
  ENDIF  
  *--PO Lines
  IF SEEK(lcStyle,'POSLN')
    XADD = ''
    DO CASE
      CASE lcRpPrtQty = 'L'
        XADD = ' .AND.  CutPick2.CtktNo = PosLn.Po'
      CASE lcRpPrtQty = 'N'
        XADD = ' .AND.  CutPick2.CtktNo <> PosLn.Po'
    ENDCASE
    SELECT POSLN
    IF !EMPTY(lcPrDtFlt)
      lcPrDtFlt = STRTRAN(lcPrDtFlt,'CUTTKTH.','POSHDR.')
    ENDIF
    SCAN REST WHILE style+cstytype+po+STR(lineno,6)+trancd = lcStyle;
              FOR POSHDR.STATUS<>'X' .AND. POSHDR.STATUS<>'C' ;
                  AND &lcPrDtFlt .AND. TOTQTY<>0 &XADD
       SCATTER MEMVAR MEMO
       INSERT INTO (lcPOTemp) FROM MEMVAR
    ENDSCAN   
  ENDIF
  *--Order lines
  IF SEEK (lcStyle,'ORDLINE')
    XADDORD = ''
    DO CASE
      CASE lcRpPrtQty = 'L'
        XADDORD = ' .AND.  (OrdLine.Order = CutPick.Order .OR. ' + ;
                  ' OrdLine.Order = CutPick3.Order) '
      CASE lcRpPrtQty = 'N'
        XADDORD = ' .AND.  (OrdLine.Order <>CutPick.Order .AND.' + ;
                  ' OrdLine.Order <> CutPick3.Order) '
    ENDCASE
    SELECT ORDLINE  
    SCAN WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = lcStyle;
         FOR  ORDHDR.STATUS $ lcStat .AND. TOTQTY<>0 AND &lcOrDtFlt &XADDORD
       SCATTER MEMVAR MEMO
       INSERT INTO (lcOrdTemp) FROM MEMVAR
    ENDSCAN            
  ENDIF
ENDSCAN
*--Close unneeded files.
USE IN CutPick2
USE IN CutPick3


****************************************************************************
* FUNC: lfwRepWhen
* DESC: To validate OG When.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* Date: 01/18/2000
****************************************************************************
FUNCTION lfwRepWhen

FUNCTION lfPrepFile
*--
SELECT CUTTKTL
SET RELATION TO CUTTKT INTO CUTTKTH
IF !USED('&lcCutTemp')
  =AFIELDS(laFileStru)
  lnArrLen = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnArrLen+1,4]
  *--Add the type field.
  laFileStru[lnArrLen+1,1] = 'CONTR1'
  laFileStru[lnArrLen+1,2] = 'C'
  laFileStru[lnArrLen+1,3] = '8'
  laFileStru[lnArrLen+1,4] = '0'
  CREATE DBF (gcWorkDir+lcCutTemp) FROM ARRAY laFileStru
  SET RELATION TO CUTTKT INTO CUTTKTH
  DO CASE
    CASE !llCutSolsz .AND. lcRpWip = 'S'
      INDEX ON SUBSTR(STYLE,1,LEN(lcStyPic))+CUTTKT+SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)+TRANCD TAG (lcCutTemp)
    CASE llCutSolsz .AND. lcRpWip='S'
      INDEX ON STYLE+CUTTKT+TRANCD TAG (lcCutTemp)
    CASE !llCutSolsz .AND. lcRpWipSor='D'
      INDEX ON SUBSTR(STYLE,1,LEN(lcStyPic))+DTOS(CUTTKTH.COMPLETE)+CUTTKT+SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)+TRANCD TAG (lcCutTemp)
    CASE !llCutSolsz .AND. lcRpWipSor='F'
      INDEX ON SUBSTR(STYLE,1,LEN(lcStyPic))+CUTTKTH.CONTR1+CUTTKT+SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)+TRANCD TAG (lcCutTemp)
    CASE !llCutSolsz .AND. lcRpWipSor='P'
      INDEX ON SUBSTR(STYLE,1,LEN(lcStyPic))+CUTTKT+SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)+TRANCD TAG (lcCutTemp)
    CASE llCutSolsz .AND. lcRpWipSor='D'
      INDEX ON STYLE+DTOS(CUTTKTH.COMPLETE)+CUTTKT+TRANCD+STR(RECNO(),7) TAG (lcCutTemp)
    CASE llCutSolsz .AND. lcRpWipSor='F'
      INDEX ON STYLE+CUTTKTH.CONTR1+CUTTKT+TRANCD+STR(RECNO(),7) TAG (lcCutTemp)
    CASE llCutSolsz .AND. lcRpWipSor='P'
      INDEX ON STYLE+CUTTKT+TRANCD+STR(RECNO(),7) TAG (lcCutTemp)
  ENDCASE
ELSE
  IF RECCOUNT('&lcCutTemp') >0
    SELECT (lcCutTemp)
    ZAP
  ENDIF  
ENDIF  
*--
SELECT POSLN
SET RELATION TO 'P'+PO INTO POSHDR
IF !USED('&lcPoTemp')
  COPY STRUCTURE TO (gcWorkDir+lcPoTemp)
  =gfOpenFile(gcWorkDir+lcPoTemp,'','EX')
  SET RELATION TO 'P'+PO INTO POSHDR
  *--
  DO CASE
    CASE !llCutSolsz .AND. lcRpWip='S'
      INDEX ON SUBSTR(STYLE,1,LEN(lcStyPic))+PO+SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen) TAG (lcPoTemp)
    CASE llCutSolsz .AND. lcRpWip='S'
      INDEX ON STYLE+PO+TRANCD TAG (lcPoTemp)
    CASE !llCutSolsz .AND. lcRpWipSor='D'
      INDEX ON SUBSTR(STYLE,1,LEN(lcStyPic))+DTOS(POSHDR.COMPLETE)+PO+SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)+TRANCD+STR(RECNO(),7) TAG (lcPoTemp)
    CASE !llCutSolsz .AND. lcRpWipSor='F'
      INDEX ON SUBSTR(STYLE,1,LEN(lcStyPic))+VENDOR+PO+SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)+TRANCD TAG (lcPoTemp)
    CASE !llCutSolsz .AND. lcRpWipSor='P'
      INDEX ON SUBSTR(STYLE,1,LEN(lcStyPic))+PO+SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)+TRANCD+STR(RECNO(),7) TAG (lcPoTemp)
    CASE llCutSolsz .AND. lcRpWipSor='D'
      INDEX ON STYLE+DTOS(POSHDR.COMPLETE)+PO+TRANCD+STR(RECNO(),7) TAG (lcPoTemp)
    CASE llCutSolsz .AND. lcRpWipSor='F'
      INDEX ON STYLE+VENDOR+PO+TRANCD+STR(RECNO(),7) TAG (lcPoTemp)
    CASE llCutSolsz .AND. lcRpWipSor='P'
     INDEX ON STYLE+PO+TRANCD+STR(RECNO(),7) TAG (lcPoTemp)
  ENDCASE
ELSE
  IF RECCOUNT('&lcPoTemp') >0
    SELECT (lcPoTemp)
    ZAP
  ENDIF  
ENDIF  

*--
SELECT ORDLINE
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDI
IF !USED('&lcOrdTemp')
  COPY STRUCTURE TO (gcWorkDir+lcOrdTemp)
  =gfOpenFile(gcWorkDir+lcOrdTemp,'','EX')
  SET RELATION TO 'O'+ORDER INTO ORDHDR ADDI
  *--
  DO CASE
    CASE !llCutSolsz .AND. lcRpSales='S'
      INDEX ON SUBSTR(STYLE,1,LEN(lcStyPic))+ORDER+SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)+DTOS(COMPLETE)+STORE TAG (lcOrdTemp)
    CASE llCutSolsz .AND. lcRpSales='S'
      INDEX ON STYLE+ORDER+DTOS(COMPLETE)+STORE TAG (lcOrdTemp)
    CASE !llCutSolsz .AND. lcRpSalSor='D'
      INDEX ON SUBSTR(STYLE,1,LEN(lcStyPic))+DTOS(COMPLETE)+ORDER+SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)+STORE TAG (lcOrdTemp)
    CASE !llCutSolsz .AND. lcRpSalSor='A'
      INDEX ON SUBSTR(STYLE,1,LEN(lcStyPic))+ACCOUNT+ORDER+SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)+DTOS(COMPLETE)+STORE TAG (lcOrdTemp)
    CASE !llCutSolsz .AND. lcRpSalSor='O'
      INDEX ON SUBSTR(STYLE,1,LEN(lcStyPic))+ORDER+SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)+DTOS(COMPLETE)+STORE TAG (lcOrdTemp)
    CASE llCutSolsz .AND. lcRpSalSor='D'
      INDEX ON STYLE+DTOS(COMPLETE)+ORDER+STORE TAG (lcOrdTemp)
    CASE llCutSolsz .AND. lcRpSalSor='A'
      INDEX ON STYLE+ACCOUNT+ORDER+DTOS(COMPLETE)+STORE TAG (lcOrdTemp)
    CASE llCutSolsz .AND. lcRpSalSor='O'
      INDEX ON STYLE+ORDER+DTOS(COMPLETE)+STORE TAG (lcOrdTemp)
  ENDCASE
ELSE
  IF RECCOUNT('&lcOrdTemp') >0
    SELECT (lcOrdTemp)
    ZAP
  ENDIF  
ENDIF  
*-- Openning the file CutPick with a relation to OrdLine according 
*-- to the CutOrd index.
SELECT OrdLine 
SET RELATION TO '2'+Order+STR(LineNo,6) INTO CutPick ADDITIVE
*-- Openning the file CutPick with a relation to PosLn according 
*-- to the CutPick index in another work area.
SELECT 0 
USE '&gcDataDir.CutPick' AGAIN ALIAS CutPick2
SET ORDER TO CutPick
SELECT PosLn 
SET RELATION TO '2'+PO+Style INTO CutPick2 ADDITIVE

*-- Openning the file CutPick with a relation to OrdLine according 
*-- to the CutOrd index in another work area to check if the order 
*-- was allocated by cuttkt not by PO.
SELECT 0 
USE '&gcDataDir.CutPick' AGAIN ALIAS CutPick3
SET ORDER TO CutOrd
SELECT OrdLine 
SET RELATION TO '1'+Order+STR(LineNo,6) INTO CutPick3 ADDITIVE

*--Open mfgophdr file to get the contractor.
=gfOpenFile(gcDataDir+'mfgoprhd',gcDataDir+'Mfgoprhd','SH')
*--Open apvendor file to get the contractor NAME.
=gfOpenFile(gcDataDir+'apvendor',gcDataDir+'VenCode','SH')
