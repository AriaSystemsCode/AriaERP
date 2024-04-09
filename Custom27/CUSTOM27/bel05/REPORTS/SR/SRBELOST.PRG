*:************************************************************************
*: Program file  : SRBELOST.Prg
*: Program desc. : CUSTOM SALESREP STATISTICS FORM BELLPOINT
*: System        : Aria Advantage Series VER. 2.7
*: Module        : SR
*: Developer     : Adel Mohammed El Gazzar
*: Date          : 10/07/02
*: Reference     : C102723,1
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO SRORDST
*:************************************************************************
*: Modifications:
*:B606641,1 Abd 12/17/2002 Fix bug that the 2 lay out from the report do not
*:B606641,1 Abd            match.
*:************************************************************************

XTITLE=SPACE(40)
STORE 0.00 TO GBACT,GBACTAMT,GBAVG,GBAVGAMT
STORE 0.00 TO GCACT,GCACTAMT,GCAVG,GCAVGAMT
IF llMultCurr
  PRIVATE lcCurr
  lcCurr = ""
  lcTmpCur = gfTempName()
  =lfCretTemp()
ENDIF

*B606641,1 ABD - [Begin]
lcTemp = gfTempName()
*B606641,1 ABD - [End]

ROW     = 99
PAGENO  = 0
R_WIDTH = 'W'
R_TITLE = 'Salesrep statistics'
H1='                         ............ B O O K E D ..............       ........... C A N C E L E D ........... '
H2='                              ACTUAL            AVERAGE        %             ACTUAL           AVERAGE       %  '
H3='REP ... NAME ...    CNT     PCS   AMOUNT      PCS   AMOUNT   TOTAL         PCS   AMOUNT     PCS   AMOUNT  TOTAL'
TEMP = GFTEMPNAME()

llRep_div = ALLTRIM(gfGetMemVar('M_REP_COMM' , gcAct_Comp)) = 'D'
IF llRep_div
  = gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH')
ENDIF
SELECT ORDHDR
LOCATE ALL FOR &lcRpExp
IF !FOUND()
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

*--If sort by 'Sales Rep'
IF lcRpSortBy = 'S' 
  DO lpPrntSls
ELSE
  *--If sort by 'Sales Rep + Style Group'
  DO lpPrntGrp
ENDIF

DO ENDREPORT         && END THE REPORT OR DISPLAY ON SCREEN
SET DEVICE TO SCREEN

IF USED(TEMP)
  USE IN (TEMP)
ENDIF
ERASE (gcWorkDir+Temp+'.DBF')
ERASE (gcWorkDir+Temp+'.CDX')

*!*****************************************************************
*! Name      : lpPrntGrp
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/07/02
*! Purpose   : Print when sorting by Rep+Sryle Group
*!*************************************************************
*! Refer to  : C102723
PROCEDURE lpPrntGrp

= gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH')
= gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH')
= gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
SELECT ORDHDR
=AFIELDS(laFileStru)
lnArrLen = ALEN(laFileStru,1)
DIMENSION laFileStru[lnArrLen+1,4]
*--Add the type field.
laFileStru[lnArrLen+1,1] = 'StyGrp'
laFileStru[lnArrLen+1,2] = 'C'
laFileStru[lnArrLen+1,3] = '6'
laFileStru[lnArrLen+1,4] = '0'
*--Create the temp file.
CREATE DBF (gcWorkDir+TEMP) FROM ARRAY laFileStru
IF llMultCurr AND lcRpCurr == "F"
  INDEX ON cCurrCode+REP1+StyGrp+ORDER TAG (TEMP)
ELSE
  INDEX ON REP1+StyGrp+ORDER TAG (TEMP)
ENDIF
m.StyGrp  = " "
SELECT ORDHDR
SCAN REST FOR &lcRpExp
  *B606641,1 Abd - Add new wait window to let the user now that the report is work. [Begin]
  WAIT WINDOW 'Computing order statistics ... Order # :' + Order NOWAIT
  *B606641,1 Abd - [End]
  IF SEEK(CORDTYPE+ORDER,'ORDLINE')
    SCAT MEMVAR MEMO
    SELECT ORDLINE
    SCAN REST WHILE cordtype+order+STR(lineno,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER
      =SEEK (ORDLINE.STYLE,'STYLE')
      *B606641,1 Abd - Add new group for deleted styles. [Begin]
      *m.StyGrp = Style.cStyGroup
      m.StyGrp = IIF(EMPTY(ALLTRIM(Style.cStyGroup)),'DELETE',Style.cStyGroup)
      *B606641,1 Abd - [End]
      lcKey = IIF(llMultCurr AND lcRpCurr == "F",OrdHdr.cCurrCode,"")+OrdHdr.REP1+m.StyGrp+m.Order
      IF !SEEK(lcKey,TEMP)
         STORE 0 TO m.BOOK,m.BOOKAMT,m,CANCEL,m.CANCELAMT 
         INSERT INTO (TEMP) FROM MEMVAR
      ENDIF
      STORE 0 TO lnTot, lnAmnt
      *--If any cancelled Lines
      IF SEEK(ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'ORDCANLN')
        SELECT ORDCANLN
        SUM REST WHILE cordtype+order+STR(lineno,6) = ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6);
            TOTQTY,TOTQTY*PRICE TO lnTot, lnAmnt
      ENDIF
      SELECT (TEMP)
      REPLACE BOOK      WITH BOOK    + OrdLine.TotBook,;
              BOOKAMT   WITH BOOKAMT + OrdLine.TotBook*OrdLine.Price,;
              CANCEL    WITH CANCEL    + lnTot,;
              CANCELAMT WITH CANCELAMT + lnAmnt
      IF ORDHDR.Status = 'X' .AND. Bulk <> 'Y'
         REPLACE CANCEL    WITH CANCEL    + OrdLine.TotQty,;
                 CANCELAMT WITH CANCELAMT + OrdLine.TotQty*OrdLine.Price
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN

*B606641,1 Abd - Select the temp file to get the correct count record.[Begin]
*SELECT (TEMP)
*B606641,1 Abd - [End]
STORE 0.00 TO XBOOK,XBOOKAMT,XCANCEL,XCANCELAMT,GCOUNT
WAIT WINDOW 'Computing order statistics ...' NOWAIT

SUM ALL BOOK,BOOKAMT,CANCEL,CANCELAMT TO XBOOK,XBOOKAMT,XCANCEL,XCANCELAMT
COUNT ALL TO GCOUNT FOR !(Status = 'X' .AND. Bulk = 'Y')


*B606641,1 ABD - Collect the data on the header file. [Begin]
= lfGetTot()
*B606641,1 ABD - [End]

WAIT WINDOW 'Sorting by primary salesrep ...' NOWAIT

*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+...9....+....0....+....1....+
*                         ............ B O O K E D ..............      ........... C A N C E L E D ...........
*                              ACTUAL            AVERAGE        %            ACTUAL           AVERAGE       %
*REP ... NAME ...    CNT     PCS   AMOUNT      PCS   AMOUNT   TOTAL        PCS   AMOUNT     PCS   AMOUNT  TOTAL
*123 12345678912345 0123  123456 12345678   123456 12345678  123.9      123456 12345678  123456 12345678  123.9
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+...9....+....0....+....1....+

SET DEVICE TO PRINT
PRIVATE lnRecNo
lnRecNo = 0



IF llMultCurr
  SELECT SycCurr
  SCAN
    lcCurr = cCurrCode
    lnRecNo = RECNO()
    SELECT (Temp)
    IF lcRpCurr == "F"
      SEEK(lcCurr)
    ELSE
      LOCATE FOR ALLTRIM(ccurrcode)==ALLTRIM(lcCurr)
    ENDIF
    IF !FOUND()
      LOOP
    ENDIF
    SELECT (lcTmpCur)
    SEEK(lcCurr)
    IF !FOUND()
      APPEND BLANK
      REPLACE CurrCode WITH lcCurr
      SELECT SALESREP
      LOCATE
      SCAN
        IF ROW>55
          PAGENO=PAGENO+1
          DO RPT_HDR WITH 'REP940',XTITLE,R_WIDTH
          @ 05,01  SAY H1
          @ 06,01  SAY H2
          @ 07,01  SAY H3
          ROW=8
        ENDIF
 
        XSALESREP = REPCODE
        XREPNAME  = SALESREP->NAME
   
        WAIT WINDOW 'Processing salesrep '+ XSALESREP +'...' NOWAIT
        STORE 0   TO XCOUNT
        STORE 0.0 TO BACT,BACTAMT,BAVG,BAVGAMT,BPCNT
        STORE 0.0 TO CACT,CACTAMT,CAVG,CAVGAMT,CPCNT
        SELECT (TEMP)
        *--If Sales rep based on division and this sales rep belongs to any division
        IF llRep_div AND SEEK(XSALESREP,'REP_DIV')
          *--Get the first record as to scan for all.
          LOCATE
          lcExp = ".T."
          *--Calculate all Booked for this Sales rep if it's Rep1 or Rep2 and for the same division
          *--assigned to it in REP_DIV
          IF lcRpCurr == "F"
            lcForCon = "FOR (SEEK(REP1+cDivision,'REP_DIV') AND cCurrCode+REP1 = lcCurr+XSALESREP) ;
                         OR (SEEK(REP2+cDivision,'REP_DIV') AND cCurrCode+REP2 = lcCurr+XSALESREP) "
          ELSE
            lcForCon = "FOR (SEEK(REP1+cDivision,'REP_DIV') AND REP1 = XSALESREP) ;
                         OR (SEEK(REP2+cDivision,'REP_DIV') AND REP2 = XSALESREP) "
          ENDIF               
        ELSE
          *--If Sales rep based on division and this sales rep DOES NOT belongs to any division,
          *--go normally.
          IF lcRpCurr == "F"
            SEEK lcCurr+XSALESREP
            lcExp = "cCurrCode+REP1 = lcCurr+XSALESREP"
          ELSE
            SEEK XSALESREP
            lcExp = "REP1 = XSALESREP"
          ENDIF
          lcForCon = ""
        ENDIF
        STORE 0   TO lnGrpCnt,lnCnt
        STORE 0.0 TO lnGrpBQt,lnGrpBAm,lnGrpBVQt,lnGrpBVAt,lnGrpBCnt
        STORE 0.0 TO lnGrpCQt,lnGrpCAm,lnGrpCVQt,lnGrpCVAt,lnGrpCCnt
        llNeedSep = .F.
        lcStyGrp = StyGrp

        SCAN WHILE &lcExp &lcForCon
          *--If the Style Group changes, Print its subtotal
          IF StyGrp <> lcStyGrp
            DO lpGrpTot
          ENDIF
          XCOUNT  = XCOUNT  + IIF( Status = 'X' .AND. Bulk = 'Y', 0, 1         )
          lnCnt   = lnCnt   + IIF( Status = 'X' .AND. Bulk = 'Y', 0, 1         )
          BACT    = BACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOK      )
          CACT    = CACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCEL    )
          lnGrpBQt= lnGrpBQt+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOK      )
          lnGrpCQt= lnGrpCQt+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCEL    )
          
          IF lcRpCurr == "F"
            BACTAMT = BACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOKAMT)
            lnGrpBAm= lnGrpBAm+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOKAMT)
            CACTAMT = CACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCELAMT)
            lnGrpCAm= lnGrpCAm+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCELAMT)
          ELSE
            BACTAMT = BACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, IIF(BOOKAMT<>0,gfAmntDisp(BOOKAMT,lcRpCurr,ldRpExDate,lcRpTmpNam),0))
            lnGrpBAm= lnGrpBAm+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, IIF(BOOKAMT<>0,gfAmntDisp(BOOKAMT,lcRpCurr,ldRpExDate,lcRpTmpNam),0))
            CACTAMT = CACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, IIF(CANCELAMT<>0,gfAmntDisp(CANCELAMT,lcRpCurr,ldRpExDate,lcRpTmpNam),0))
            lnGrpCAm= lnGrpCAm+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, IIF(CANCELAMT<>0,gfAmntDisp(CANCELAMT,lcRpCurr,ldRpExDate,lcRpTmpNam),0))
          ENDIF  
        ENDSCAN

        *--Print the last Style Group changes at the same time with the Rep1
        IF (StyGrp <> lcStyGrp) OR !&lcExp
          DO lpGrpTot
        ENDIF  
        
        *B606641,1 ABD - get the data on the header record. [Begin]
        STORE 0   TO XCOUNT
        STORE 0.0 TO BACT,BACTAMT,BAVG,BAVGAMT,BPCNT
        STORE 0.0 TO CACT,CACTAMT,CAVG,CAVGAMT,CPCNT
        = lfsubtot ()
        *B606641,1 ABD - [End]
        
        STORE 0 TO BAVG,BAVGAMT,CAVG,CAVGAMT
        IF XCOUNT<>0
          BAVG    = BACT/XCOUNT
          BAVGAMT = BACTAMT/XCOUNT
          CAVG    = CACT/XCOUNT
          CAVGAMT = CACTAMT/XCOUNT
        ENDIF

        BPCNT=0.0
        IF XBOOKAMT<>0
          BPCNT = ROUND((BACTAMT/XBOOKAMT)*100,1)
        ENDIF

        CPCNT=0.0
        IF XCANCELAMT<>0
          CPCNT = ROUND((CACTAMT/XCANCELAMT)*100,1)
        ENDIF
      
        IF llMultCurr AND lcRpCurr <> "F"
          SELECT SycCurr
          IF lcRpCurr == "F"
            GOTO lnRecNo
          ELSE
            GOTO BOTTOM
          ENDIF
          SELECT (Temp)
        ENDIF

        IF llNeedSep 
          @ ROW,01 SAY REPL('-',130)
          ROW = ROW +1
        ENDIF  
  
        @ ROW,01 SAY XSALESREP
        @ ROW,05 SAY SUBSTR(SALESREP->NAME,1,15)
        @ ROW,20 SAY XCOUNT    PICTURE '9999'
        @ ROW,26 SAY BACT      PICTURE '999999'
        @ ROW,33 SAY BACTAMT   PICTURE '99999999'
        @ ROW,44 SAY BAVG      PICTURE '999999'
        @ ROW,51 SAY BAVGAMT   PICTURE '99999999'
        @ ROW,61 SAY BPCNT     PICTURE '999.9'
        @ ROW,72  SAY CACT     PICTURE '999999'
        @ ROW,79  SAY CACTAMT  PICTURE '99999999'
        @ ROW,90  SAY CAVG     PICTURE '999999'
        @ ROW,97  SAY CAVGAMT  PICTURE '99999999'
        @ ROW,107 SAY CPCNT    PICTURE '999.9'
        
        *B606641,1 Abd - Print a seprate line after every Sales rep. [Begin]
        *IF llNeedSep 
        *ROW = ROW +1
        *@ ROW,01 SAY REPL('-',130)
        *ENDIF
        ROW = ROW +1
        @ ROW,01 SAY REPL('-',130)
        *B606641,1 Abd - [End]

        * GRANT TOTALS
        GBACT    = GBACT    + BACT
        GBACTAMT = GBACTAMT + BACTAMT
        GCACT    = GCACT    + CACT
        GCACTAMT = GCACTAMT + CACTAMT
        ROW=ROW+1
      ENDSCAN

      IF lcRpCurr == "F"
        * PRINT GRAND TOTALS
        GBAVG    = GBACT/GCOUNT
        GBAVGAMT = GBACTAMT/GCOUNT
        GCAVG    = GCACT/GCOUNT
        GCAVGAMT = GCACTAMT/GCOUNT
 
        ROW=ROW+2
        @ ROW,01 SAY 'TOTALS BY '
        @ ROW,11 SAY lcCurr+" :"
        
        @ ROW,20 SAY GCOUNT     PICTURE '9999'
        @ ROW,26 SAY GBACT      PICTURE '999999'
        @ ROW,33 SAY GBACTAMT   PICTURE '99999999'
        @ ROW,44 SAY GBAVG      PICTURE '999999'
        @ ROW,51 SAY GBAVGAMT   PICTURE '99999999'
 
        @ ROW,72 SAY GCACT      PICTURE '999999'
        @ ROW,79 SAY GCACTAMT   PICTURE '99999999'
        @ ROW,90 SAY GCAVG      PICTURE '999999'
        @ ROW,97 SAY GCAVGAMT   PICTURE '99999999'
        GBACT    = 0
        GBACTAMT = 0
        GCACT    = 0
        GCACTAMT = 0
        ROW=ROW+2
      ENDIF
    ENDIF    
  ENDSCAN
  IF lcRpCurr <> "F"
    * PRINT GRAND TOTALS
    GBAVG    = GBACT/GCOUNT
    GBAVGAMT = GBACTAMT/GCOUNT
    GCAVG    = GCACT/GCOUNT
    GCAVGAMT = GCACTAMT/GCOUNT
 
    ROW=ROW+2
    @ ROW,01 SAY 'TOTALS BY '
    @ ROW,11 SAY gcBaseCurr+" :"

    @ ROW,20 SAY GCOUNT     PICTURE '9999'
    @ ROW,26 SAY GBACT      PICTURE '999999'
    @ ROW,33 SAY GBACTAMT   PICTURE '99999999'
    @ ROW,44 SAY GBAVG      PICTURE '999999'
    @ ROW,51 SAY GBAVGAMT   PICTURE '99999999'
 
    @ ROW,72 SAY GCACT      PICTURE '999999'
    @ ROW,79 SAY GCACTAMT   PICTURE '99999999'
    @ ROW,90 SAY GCAVG      PICTURE '999999'
    @ ROW,97 SAY GCAVGAMT   PICTURE '99999999'
    GBACT    = 0
    GBACTAMT = 0
    GCACT    = 0
    GCACTAMT = 0
    ROW=ROW+2
  ENDIF
ELSE
  llNeedSep = .F.
  SELECT SALESREP
  SCAN
    IF ROW>55
       PAGENO=PAGENO+1
       DO RPT_HDR WITH 'REP940',XTITLE,R_WIDTH
       @ 05,01  SAY H1
       @ 06,01  SAY H2
       @ 07,01  SAY H3
       ROW=8
    ENDIF
 
    XSALESREP = REPCODE
    XREPNAME  = SALESREP->NAME
  
    WAIT WINDOW 'Processing salesrep '+ XSALESREP +'...' NOWAIT
    STORE 0   TO XCOUNT
    STORE 0.0 TO BACT,BACTAMT,BAVG,BAVGAMT,BPCNT
    STORE 0.0 TO CACT,CACTAMT,CAVG,CAVGAMT,CPCNT

    SELECT (TEMP)
    *--If Sales rep based on division and this sales rep belongs to any division
    IF llRep_div AND SEEK(XSALESREP,'REP_DIV')
      LOCATE
      lcExp = ".T."
      *--Calculate all Booked for this Sales rep if it's Rep1 or Rep2 and for the same division
      *--assigned to it in REP_DIV
      lcForCon = "FOR (SEEK(REP1+cDivision,'REP_DIV') AND REP1 = XSALESREP) ;
                   OR (SEEK(REP2+cDivision,'REP_DIV') AND REP2 = XSALESREP) "
    ELSE
      *--If Sales rep based on division and this sales rep DOES NOT belongs to any division,
      *--go normally.
      SEEK XSALESREP
      lcExp = "REP1 = XSALESREP"
      lcForCon = ""
    ENDIF
    STORE 0   TO lnGrpCnt,lnCnt
    STORE 0.0 TO lnGrpBQt,lnGrpBAm,lnGrpBVQt,lnGrpBVAt,lnGrpBCnt
    STORE 0.0 TO lnGrpCQt,lnGrpCAm,lnGrpCVQt,lnGrpCVAt,lnGrpCCnt
    llNeedSep = .F.
    lcStyGrp = StyGrp

    SCAN WHILE &lcExp &lcForCon
      *--If the Style Group changes, Print its subtotal
      IF StyGrp <> lcStyGrp
        DO lpGrpTot
      ENDIF
    
      XCOUNT  = XCOUNT  + IIF( Status = 'X' .AND. Bulk = 'Y', 0, 1         )
      lnCnt   = lnCnt   + IIF( Status = 'X' .AND. Bulk = 'Y', 0, 1         )
      BACT    = BACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOK      )
      lnGrpBQt= lnGrpBQt+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOK      )            
      BACTAMT = BACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOKAMT   )
      lnGrpBAm= lnGrpBAm+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOKAMT)      
      CACT    = CACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCEL    )
      CACTAMT = CACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCELAMT )
    ENDSCAN
    
    
    *--Print the last Style Group changes at the same time with the Rep1
    IF (StyGrp <> lcStyGrp) OR !&lcExp
      DO lpGrpTot
    ENDIF  

    *B606641,1 ABD - get the data on the header record. [Begin]
    STORE 0   TO XCOUNT
    STORE 0.0 TO BACT,BACTAMT,BAVG,BAVGAMT,BPCNT
    STORE 0.0 TO CACT,CACTAMT,CAVG,CAVGAMT,CPCNT
    = lfsubtot()
    *B606641,1 ABD - [End]

    STORE 0 TO BAVG,BAVGAMT,CAVG,CAVGAMT
    IF XCOUNT<>0
      BAVG    = BACT/XCOUNT
      BAVGAMT = BACTAMT/XCOUNT
      CAVG    = CACT/XCOUNT
      CAVGAMT = CACTAMT/XCOUNT
    ENDIF

    BPCNT=0.0
    IF XBOOKAMT<>0
      BPCNT = ROUND((BACTAMT/XBOOKAMT)*100,1)
    ENDIF

    CPCNT=0.0
    IF XCANCELAMT<>0
      CPCNT = ROUND((CACTAMT/XCANCELAMT)*100,1)
    ENDIF
    IF llNeedSep 
      @ ROW,01 SAY REPL('-',130)
      ROW = ROW +1
    ENDIF  

    @ ROW,01 SAY XSALESREP
    @ ROW,05 SAY SUBSTR(SALESREP->NAME,1,15)
    @ ROW,20 SAY XCOUNT    PICTURE '9999'
    @ ROW,26 SAY BACT      PICTURE '999999'
    @ ROW,33 SAY BACTAMT   PICTURE '99999999'
    @ ROW,44 SAY BAVG      PICTURE '999999'
    @ ROW,51 SAY BAVGAMT   PICTURE '99999999'
    @ ROW,61 SAY BPCNT     PICTURE '999.9'
    @ ROW,72  SAY CACT     PICTURE '999999'
    @ ROW,79  SAY CACTAMT  PICTURE '99999999'
    @ ROW,90  SAY CAVG     PICTURE '999999'
    @ ROW,97  SAY CAVGAMT  PICTURE '99999999'
    @ ROW,107 SAY CPCNT    PICTURE '999.9'
    IF llNeedSep 
      ROW = ROW +1
      @ ROW,01 SAY REPL('-',130)
    ENDIF  

    * GRANT TOTALS
    GBACT    = GBACT    + BACT
    GBACTAMT = GBACTAMT + BACTAMT
    GCACT    = GCACT    + CACT
    GCACTAMT = GCACTAMT + CACTAMT
    ROW=ROW+1
  ENDSCAN

  *PRINT GRAND TOTALS
  GBAVG    = GBACT/GCOUNT
  GBAVGAMT = GBACTAMT/GCOUNT
  GCAVG    = GCACT/GCOUNT
  GCAVGAMT = GCACTAMT/GCOUNT

  ROW=ROW+2
  @ ROW,01 SAY 'TOTALS:'
  @ ROW,20 SAY GCOUNT     PICTURE '9999'
  @ ROW,26 SAY GBACT      PICTURE '999999'
  @ ROW,33 SAY GBACTAMT   PICTURE '99999999'
  @ ROW,44 SAY GBAVG      PICTURE '999999'
  @ ROW,51 SAY GBAVGAMT   PICTURE '99999999'

  @ ROW,72 SAY GCACT      PICTURE '999999'
  @ ROW,79 SAY GCACTAMT   PICTURE '99999999'
  @ ROW,90 SAY GCAVG      PICTURE '999999'
  @ ROW,97 SAY GCAVGAMT   PICTURE '99999999'
ENDIF

*!*****************************************************************
*! Name      : lpPrntSls
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/07/02
*! Purpose   : Print when sorting by Rep
*!*************************************************************
PROCEDURE lpPrntSls


SELECT ORDHDR
COPY REST TO &gcWorkDir.&TEMP FOR &lcRpExp
STORE 0.00 TO XBOOK,XBOOKAMT,XCANCEL,XCANCELAMT,GCOUNT
WAIT WINDOW 'Computing order statistics ...' NOWAIT



SELECT 0
USE (gcWorkDir+TEMP) EXCLUSIVE
IF llMultCurr AND lcRpCurr == "F"
  INDEX ON cCurrCode+REP1 TAG (TEMP)
ELSE
  INDEX ON REP1 TAG (TEMP)
ENDIF

SUM ALL BOOK,BOOKAMT,CANCEL,CANCELAMT TO XBOOK,XBOOKAMT,XCANCEL,XCANCELAMT
COUNT ALL TO GCOUNT FOR !(Status = 'X' .AND. Bulk = 'Y')
WAIT WINDOW 'Sorting by primary salesrep ...' NOWAIT

*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+...9....+....0....+....1....+
*                         ............ B O O K E D ..............      ........... C A N C E L E D ...........
*                              ACTUAL            AVERAGE        %            ACTUAL           AVERAGE       %
*REP ... NAME ...    CNT     PCS   AMOUNT      PCS   AMOUNT   TOTAL        PCS   AMOUNT     PCS   AMOUNT  TOTAL
*123 12345678912345 0123  123456 12345678   123456 12345678  123.9      123456 12345678  123456 12345678  123.9
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+...9....+....0....+....1....+

SET DEVICE TO PRINT
PRIVATE lnRecNo
lnRecNo = 0

IF llMultCurr
  SELECT SycCurr
  SCAN
    lcCurr = cCurrCode
    lnRecNo = RECNO()
    SELECT (Temp)
    IF lcRpCurr == "F"
      SEEK(lcCurr)
    ELSE
      LOCATE FOR ALLTRIM(ccurrcode)==ALLTRIM(lcCurr)
    ENDIF
    IF !FOUND()
      LOOP
    ENDIF
    SELECT (lcTmpCur)
    SEEK(lcCurr)
    IF !FOUND()
      APPEND BLANK
      REPLACE CurrCode WITH lcCurr
      SELECT SALESREP
      LOCATE
      SCAN
        IF ROW>55
          PAGENO=PAGENO+1
          DO RPT_HDR WITH 'REP940',XTITLE,R_WIDTH
          @ 05,01  SAY H1
          @ 06,01  SAY H2
          @ 07,01  SAY H3
          ROW=8
        ENDIF
 
        XSALESREP = REPCODE
        XREPNAME  = SALESREP->NAME
   
        WAIT WINDOW 'Processing salesrep '+ XSALESREP +'...' NOWAIT
        STORE 0   TO XCOUNT
        STORE 0.0 TO BACT,BACTAMT,BAVG,BAVGAMT,BPCNT
        STORE 0.0 TO CACT,CACTAMT,CAVG,CAVGAMT,CPCNT
        SELECT (TEMP)
        *-- Adjust expression based on the basis of Salesrep Commession setup.
        *--If Sales rep based on division and this sales rep belongs to any division
        IF llRep_div AND SEEK(XSALESREP,'REP_DIV')
          *--Get the first record as to scan for all.
          LOCATE
          lcExp = ".T."
          *--Calculate all Booked for this Sales rep if it's Rep1 or Rep2 and for the same division
          *--assigned to it in REP_DIV
          IF lcRpCurr == "F"
            lcForCon = "FOR (SEEK(REP1+cDivision,'REP_DIV') AND cCurrCode+REP1 = lcCurr+XSALESREP) ;
                         OR (SEEK(REP2+cDivision,'REP_DIV') AND cCurrCode+REP2 = lcCurr+XSALESREP) "
          ELSE
            lcForCon = "FOR (SEEK(REP1+cDivision,'REP_DIV') AND REP1 = XSALESREP) ;
                         OR (SEEK(REP2+cDivision,'REP_DIV') AND REP2 = XSALESREP) "
          ENDIF               
        ELSE
          *--If Sales rep based on division and this sales rep DOES NOT belongs to any division,
          *--go normally.
          IF lcRpCurr == "F"
            SEEK lcCurr+XSALESREP
            lcExp = "cCurrCode+REP1 = lcCurr+XSALESREP"
          ELSE
            SEEK XSALESREP
            lcExp = "REP1 = XSALESREP"
          ENDIF
          lcForCon = ""
        ENDIF
        SCAN WHILE &lcExp &lcForCon
          XCOUNT  = XCOUNT  + IIF( Status = 'X' .AND. Bulk = 'Y', 0, 1         )
          BACT    = BACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOK      )
          CACT    = CACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCEL    )
          IF lcRpCurr == "F"
            BACTAMT = BACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOKAMT)
            CACTAMT = CACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCELAMT)
          ELSE
            BACTAMT = BACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, IIF(BOOKAMT<>0,gfAmntDisp(BOOKAMT,lcRpCurr,ldRpExDate,lcRpTmpNam),0))
            CACTAMT = CACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, IIF(CANCELAMT<>0,gfAmntDisp(CANCELAMT,lcRpCurr,ldRpExDate,lcRpTmpNam),0))
          ENDIF  
        ENDSCAN

        STORE 0 TO BAVG,BAVGAMT,CAVG,CAVGAMT
        IF XCOUNT<>0
          BAVG    = BACT/XCOUNT
          BAVGAMT = BACTAMT/XCOUNT
          CAVG    = CACT/XCOUNT
          CAVGAMT = CACTAMT/XCOUNT
        ENDIF

        BPCNT=0.0
        IF XBOOKAMT<>0
          BPCNT = ROUND((BACTAMT/XBOOKAMT)*100,1)
        ENDIF

        CPCNT=0.0
        IF XCANCELAMT<>0
          CPCNT = ROUND((CACTAMT/XCANCELAMT)*100,1)
        ENDIF
      
        IF llMultCurr AND lcRpCurr <> "F"
          SELECT SycCurr
          IF lcRpCurr == "F"
            GOTO lnRecNo
          ELSE
            GOTO BOTTOM
          ENDIF
          SELECT (Temp)
        ENDIF
        @ ROW,01 SAY XSALESREP
        @ ROW,05 SAY SUBSTR(SALESREP->NAME,1,15)
        @ ROW,20 SAY XCOUNT    PICTURE '9999'
        @ ROW,26 SAY BACT      PICTURE '999999'
        @ ROW,33 SAY BACTAMT   PICTURE '99999999'
        @ ROW,44 SAY BAVG      PICTURE '999999'
        @ ROW,51 SAY BAVGAMT   PICTURE '99999999'
        @ ROW,61 SAY BPCNT     PICTURE '999.9'
        @ ROW,72  SAY CACT     PICTURE '999999'
        @ ROW,79  SAY CACTAMT  PICTURE '99999999'
        @ ROW,90  SAY CAVG     PICTURE '999999'
        @ ROW,97  SAY CAVGAMT  PICTURE '99999999'
        @ ROW,107 SAY CPCNT    PICTURE '999.9'

        * GRANT TOTALS
        GBACT    = GBACT    + BACT
        GBACTAMT = GBACTAMT + BACTAMT
        GCACT    = GCACT    + CACT
        GCACTAMT = GCACTAMT + CACTAMT
        ROW=ROW+1
      ENDSCAN

      IF lcRpCurr == "F"
        * PRINT GRAND TOTALS
        GBAVG    = GBACT/GCOUNT
        GBAVGAMT = GBACTAMT/GCOUNT
        GCAVG    = GCACT/GCOUNT
        GCAVGAMT = GCACTAMT/GCOUNT
 
        ROW=ROW+2
        @ ROW,01 SAY 'TOTALS BY '
        @ ROW,11 SAY lcCurr+" :"

        @ ROW,20 SAY GCOUNT     PICTURE '9999'
        @ ROW,26 SAY GBACT      PICTURE '999999'
        @ ROW,33 SAY GBACTAMT   PICTURE '99999999'
        @ ROW,44 SAY GBAVG      PICTURE '999999'
        @ ROW,51 SAY GBAVGAMT   PICTURE '99999999'
 
        @ ROW,72 SAY GCACT      PICTURE '999999'
        @ ROW,79 SAY GCACTAMT   PICTURE '99999999'
        @ ROW,90 SAY GCAVG      PICTURE '999999'
        @ ROW,97 SAY GCAVGAMT   PICTURE '99999999'
        GBACT    = 0
        GBACTAMT = 0
        GCACT    = 0
        GCACTAMT = 0
        ROW=ROW+2
      ENDIF
    ENDIF    
  ENDSCAN
  IF lcRpCurr <> "F"
    * PRINT GRAND TOTALS
    GBAVG    = GBACT/GCOUNT
    GBAVGAMT = GBACTAMT/GCOUNT
    GCAVG    = GCACT/GCOUNT
    GCAVGAMT = GCACTAMT/GCOUNT
 
    ROW=ROW+2
    @ ROW,01 SAY 'TOTALS BY '
    @ ROW,11 SAY gcBaseCurr+" :"
    
    @ ROW,20 SAY GCOUNT     PICTURE '9999'
    @ ROW,26 SAY GBACT      PICTURE '999999'
    @ ROW,33 SAY GBACTAMT   PICTURE '99999999'
    @ ROW,44 SAY GBAVG      PICTURE '999999'
    @ ROW,51 SAY GBAVGAMT   PICTURE '99999999'
 
    @ ROW,72 SAY GCACT      PICTURE '999999'
    @ ROW,79 SAY GCACTAMT   PICTURE '99999999'
    @ ROW,90 SAY GCAVG      PICTURE '999999'
    @ ROW,97 SAY GCAVGAMT   PICTURE '99999999'
    GBACT    = 0
    GBACTAMT = 0
    GCACT    = 0
    GCACTAMT = 0
    ROW=ROW+2
  ENDIF
ELSE
  SELECT SALESREP
  SCAN
    IF ROW>55
       PAGENO=PAGENO+1
       DO RPT_HDR WITH 'REP940',XTITLE,R_WIDTH
       @ 05,01  SAY H1
       @ 06,01  SAY H2
       @ 07,01  SAY H3
       ROW=8
    ENDIF
 
    XSALESREP = REPCODE
    XREPNAME  = SALESREP->NAME
  
    WAIT WINDOW 'Processing salesrep '+ XSALESREP +'...' NOWAIT
    STORE 0   TO XCOUNT
    STORE 0.0 TO BACT,BACTAMT,BAVG,BAVGAMT,BPCNT
    STORE 0.0 TO CACT,CACTAMT,CAVG,CAVGAMT,CPCNT

    SELECT (TEMP)
    *-- Adjust expression based on the basis of Salesrep Commession setup.
    *--If Sales rep based on division and this sales rep belongs to any division
    IF llRep_div AND SEEK(XSALESREP,'REP_DIV')
      LOCATE
      lcExp = ".T."
      *--Calculate all Booked for this Sales rep if it's Rep1 or Rep2 and for the same division
      *--assigned to it in REP_DIV
      lcForCon = "FOR (SEEK(REP1+cDivision,'REP_DIV') AND REP1 = XSALESREP) ;
                   OR (SEEK(REP2+cDivision,'REP_DIV') AND REP2 = XSALESREP) "
    ELSE
      *--If Sales rep based on division and this sales rep DOES NOT belongs to any division,
      *--go normally.
      SEEK XSALESREP
      lcExp = "REP1 = XSALESREP"
      lcForCon = ""
    ENDIF
    SCAN WHILE &lcExp &lcForCon
      XCOUNT  = XCOUNT  + IIF( Status = 'X' .AND. Bulk = 'Y', 0, 1         )
      BACT    = BACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOK      )
      BACTAMT = BACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOKAMT   )
      CACT    = CACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCEL    )
      CACTAMT = CACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCELAMT )
    ENDSCAN

    STORE 0 TO BAVG,BAVGAMT,CAVG,CAVGAMT
    IF XCOUNT<>0
      BAVG    = BACT/XCOUNT
      BAVGAMT = BACTAMT/XCOUNT
      CAVG    = CACT/XCOUNT
      CAVGAMT = CACTAMT/XCOUNT
    ENDIF

    BPCNT=0.0
    IF XBOOKAMT<>0
      BPCNT = ROUND((BACTAMT/XBOOKAMT)*100,1)
    ENDIF

    CPCNT=0.0
    IF XCANCELAMT<>0
      CPCNT = ROUND((CACTAMT/XCANCELAMT)*100,1)
    ENDIF

    @ ROW,01 SAY XSALESREP
    @ ROW,05 SAY SUBSTR(SALESREP->NAME,1,15)
    @ ROW,20 SAY XCOUNT    PICTURE '9999'
    @ ROW,26 SAY BACT      PICTURE '999999'
    @ ROW,33 SAY BACTAMT   PICTURE '99999999'
    @ ROW,44 SAY BAVG      PICTURE '999999'
    @ ROW,51 SAY BAVGAMT   PICTURE '99999999'
    @ ROW,61 SAY BPCNT     PICTURE '999.9'
    @ ROW,72  SAY CACT     PICTURE '999999'
    @ ROW,79  SAY CACTAMT  PICTURE '99999999'
    @ ROW,90  SAY CAVG     PICTURE '999999'
    @ ROW,97  SAY CAVGAMT  PICTURE '99999999'
    @ ROW,107 SAY CPCNT    PICTURE '999.9'

    * GRANT TOTALS
    GBACT    = GBACT    + BACT
    GBACTAMT = GBACTAMT + BACTAMT
    GCACT    = GCACT    + CACT
    GCACTAMT = GCACTAMT + CACTAMT
    ROW=ROW+1
  ENDSCAN

  *PRINT GRAND TOTALS
  GBAVG    = GBACT/GCOUNT
  GBAVGAMT = GBACTAMT/GCOUNT
  GCAVG    = GCACT/GCOUNT
  GCAVGAMT = GCACTAMT/GCOUNT

  ROW=ROW+2
  @ ROW,01 SAY 'TOTALS:'
  @ ROW,20 SAY GCOUNT     PICTURE '9999'
  @ ROW,26 SAY GBACT      PICTURE '999999'
  @ ROW,33 SAY GBACTAMT   PICTURE '99999999'
  @ ROW,44 SAY GBAVG      PICTURE '999999'
  @ ROW,51 SAY GBAVGAMT   PICTURE '99999999'

  @ ROW,72 SAY GCACT      PICTURE '999999'
  @ ROW,79 SAY GCACTAMT   PICTURE '99999999'
  @ ROW,90 SAY GCAVG      PICTURE '999999'
  @ ROW,97 SAY GCAVGAMT   PICTURE '99999999'
ENDIF

*!*************************************************************
*! Name      : lfFillVars
*: Developer : Adel Mohammed El Gazzar
*! Date      : 08/28/2001
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
*B603547,1
FUNCTION lfFillVars

IF !USED('SYCCOMP')
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF  
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

*-- End Of lfFillVars.

*:***************************************************************************
*: Name        : lfCretTemp
*: Developer   : Adel Mohammed El Gazzar
*: Date        : 10/07/02
*: Purpose     : 
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfCretTemp()
*:***************************************************************************
*
FUNCTION lfCretTemp
DIMENSION laFStru[1,4]
laFStru[1,1] = 'CurrCode'
laFStru[1,2] = 'C'
laFStru[1,3] = 3
laFStru[1,4] = 0

CREATE DBF (gcWorkDir+lcTmpCur) FROM ARRAY laFStru
=gfOpenFile(gcWorkDir+'&lcTmpCur','','EX')

INDEX ON CurrCode TAG (lcTmpCur)
SET ORDER TO TAG (lcTmpCur)


*:***************************************************************************
*: Name        : lpGrpTot
*: Developer   : Adel Mohammed El Gazzar
*: Date        : 10/07/02
*: Purpose     : Print the Style Group Sub total
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfCretTemp()
*:***************************************************************************
PROCEDURE lpGrpTot

IF lnCnt=0
  RETURN
ENDIF
IF lnCnt<>0
  lnGrpBVQt = lnGrpBQt/lnCnt
  lnGrpBVAt = lnGrpBAm/lnCnt
  lnGrpCVQt = lnGrpCQt/lnCnt
  lnGrpCVAt = lnGrpCAm/lnCnt
ENDIF
lnGrpBCnt =0.0
IF XBOOKAMT<>0
  lnGrpBCnt = ROUND((lnGrpBAm/XBOOKAMT)*100,1)
ENDIF

lnGrpCCnt=0.0
IF XCANCELAMT<>0
   lnGrpCCnt = ROUND((lnGrpCVAt/XCANCELAMT)*100,1)
ENDIF

@ ROW,01 SAY '**Stye Group:' + ALLT(lcStyGrp)
@ ROW,20 SAY lnCnt     PICTURE '9999'
@ ROW,26 SAY lnGrpBQt  PICTURE '999999'
@ ROW,33 SAY lnGrpBAm  PICTURE '99999999'
@ ROW,44 SAY lnGrpBVQt PICTURE '999999'
@ ROW,51 SAY lnGrpBVAt PICTURE '99999999'
@ ROW,61 SAY lnGrpBCnt PICTURE '999.9'

@ ROW,72  SAY lnGrpCQt  PICTURE '999999'
@ ROW,79  SAY lnGrpCAm  PICTURE '99999999'
@ ROW,90  SAY lnGrpCVQt PICTURE '999999'
@ ROW,97  SAY lnGrpCVAt PICTURE '99999999'
@ ROW,107 SAY lnGrpCCnt PICTURE '999.9'
STORE 0   TO lnGrpCnt,lnCnt
STORE 0.0 TO lnGrpBQt,lnGrpBAm,lnGrpBVQt,lnGrpBVAt,lnGrpBCnt
STORE 0.0 TO lnGrpCQt,lnGrpCAm,lnGrpCVQt,lnGrpCVAt,lnGrpCCnt
lcStyGrp = StyGrp
ROW = ROW + 1
llNeedSep = .T.
*:*************************************************************
*: Name      : lfGetTot
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : Get a copy from the header record.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : = lfGetTot()
*:*************************************************************
*:B606641,1
FUNCTION lfGetTot
PRIVATE lnAlias

lnAlias = SELECT(0)

IF !USED(lcTemp)
  SELECT ORDHDR
  LOCATE
  COPY REST TO &gcWorkDir.&lcTemp FOR &lcRpExp

  SELECT 0  
  USE (gcWorkDir+lcTemp) EXCLUSIVE
  IF llMultCurr AND lcRpCurr == "F"
    INDEX ON cCurrCode+REP1 TAG (TEMP)
  ELSE
    INDEX ON REP1 TAG (TEMP)
  ENDIF

ENDIF



USE (gcWorkDir+lcTEMP) EXCLUSIVE
IF llMultCurr AND lcRpCurr == "F"
  INDEX ON cCurrCode+REP1 TAG (TEMP)
ELSE
  INDEX ON REP1 TAG (TEMP)
ENDIF


SELECT (lnAlias)
*-- End OF lfGetTot
*:*************************************************************
*: Name      : lfsubtot
*: Developer : Abdou Elgendy [Abd]
*: Date      : 12/22/2002
*: Purpose   : Get the sub total Qty.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : = lfsubtot()
*:*************************************************************
*:B606641,1

FUNCTION lfsubtot
PRIVATE lnAlias

lnAlias = SELECT(0)

SELECT(lcTemp)

IF llMultCurr
  =SEEK(lcCurr+xsalesrep)
  SCAN WHILE &lcExp &lcForCon
    XCOUNT  = XCOUNT  + IIF( Status = 'X' .AND. Bulk = 'Y', 0, 1         )
    lnCnt   = lnCnt   + IIF( Status = 'X' .AND. Bulk = 'Y', 0, 1         )
    BACT    = BACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOK      )
    CACT    = CACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCEL    )
    lnGrpBQt= lnGrpBQt+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOK      )
    lnGrpCQt= lnGrpCQt+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCEL    )
          
    IF lcRpCurr == "F"
      BACTAMT = BACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOKAMT)
      lnGrpBAm= lnGrpBAm+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOKAMT)
      CACTAMT = CACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCELAMT)
      lnGrpCAm= lnGrpCAm+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCELAMT)
    ELSE
      BACTAMT = BACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, IIF(BOOKAMT<>0,gfAmntDisp(BOOKAMT,lcRpCurr,ldRpExDate,lcRpTmpNam),0))
      lnGrpBAm= lnGrpBAm+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, IIF(BOOKAMT<>0,gfAmntDisp(BOOKAMT,lcRpCurr,ldRpExDate,lcRpTmpNam),0))
      CACTAMT = CACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, IIF(CANCELAMT<>0,gfAmntDisp(CANCELAMT,lcRpCurr,ldRpExDate,lcRpTmpNam),0))
      lnGrpCAm= lnGrpCAm+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, IIF(CANCELAMT<>0,gfAmntDisp(CANCELAMT,lcRpCurr,ldRpExDate,lcRpTmpNam),0))
    ENDIF  
  ENDSCAN
ELSE
  =SEEK(xsalesrep)
  SCAN WHILE &lcExp &lcForCon
    XCOUNT  = XCOUNT  + IIF( Status = 'X' .AND. Bulk = 'Y', 0, 1         )
    lnCnt   = lnCnt   + IIF( Status = 'X' .AND. Bulk = 'Y', 0, 1         )
    BACT    = BACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOK      )
    lnGrpBQt= lnGrpBQt+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOK      )            
    BACTAMT = BACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOKAMT   )
    lnGrpBAm= lnGrpBAm+ IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOKAMT)      
    CACT    = CACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCEL    )
    CACTAMT = CACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCELAMT )
  ENDSCAN
ENDIF
  
SELECT(lnAlias)
*-- End OF lfsubtot
*:*************************************************************