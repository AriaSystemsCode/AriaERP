*!********************************************************************
*: Program file  : SOMOR400.PRG (Converted from 26 to 27 for SIGNUM)
*: Program desc. : CUSTOMIZED ORDER DYELOT REPORT.
*:         System: ARIA APPAREL SERIES
*:         Module: SALES ORDER
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:           DATE: 09/28/1999
*:       Refer To: (C101668)
*!****************************************************************************
*: MODIFICATIONS:
*:B802784,1 ADEL 11/16/1999 1- Fix the bug for wrong subtotal when sortig by style or fabric.
*:B802784,1                 2- Fix the of 'No records selected' when selecting by fabric and select a color(s).
*:B603328,1      12/16/1999 3- Fix the bug of not printing the total yards for each color.
*:B804500,1 MHM 01/29/2002  1- Fix the bug ofThe report doesn't include all the colors of the primary fab
*:B605443,1 TMI 03/20/02    Sum Yards quantity instead of dollars amnt in case of print by Yield.
*!****************************************************************************
*--- Program variables
*---------------------------------------------------------------
*-(1) Variables initilized in the option grig
*-- lcRpReport     && Holds the reports form (Detail - Summary).
*-- llRpSummrz     && Summarize  multi-store orders?
*-- lcRpLinNot     && Print Order line notes?
*-- lcRpPrint      && Print By (Dyelot-Yield).
*-- lcRpSort       && Sort By. 
*-- lcRpStkWip     && Print Stock/Wip (All-Stock-Wip-None).
*-- lcOldVal       && Holds the old value.
*-- lcStyMajor     && Holds the style title.
*-- lnMajorLen     && Holds the major length.
*-- lcNonMajTl     && Holds the non major title.
*-- lcRpSelcBy     && Select By (Account-Style-Fabric-Everything).
*-- llRpCorGrp     && Line in coordinate group?
*-- lcRpOpTitl     && Holds the optional title.
*-- lcStyCurs      && Holds the styles cursor name.
*-- lcOrdLTmp      && Holds the orders cursor name.
*-(2) Variables initilized in this program.
llNotes   = (gfGetMemVar('M_LN_NOTE') = 'Y')
*B603328,1 (Begin) Remove lnTotYard
*STORE 0 TO lnTotQty,lnYield,lnTotYard
STORE 0 TO lnTotQty,lnYield
*B603328,1 (End)
*--Note : When the user selects a range of Accounts,Styles,Fabrics, the OG program
*--       puts this range in an one-field temp file. But if the user doesn't select any 
*--       arnge (i.e. he wants to print all Accounts or styles...etc) this temp file 
*--       doesnot exist so the following variables check this .
*-- llAccount      && Has the user selected Account(s)?
*-- llStyle        && Has the user selected Style(s)?
*-- llFabric       && Has the user selected Fabric(s)?
*-- lcRngTmp       && Holds the range temp file name if any.
*-- lcStyClrS      && Holds major and nonmajor titles.
STORE '' TO lcRngTmp
STORE .F. TO llAccount,llStyle,llFabric
lcStyClrS = UPPER(ALLTRIM(lcStyMajor+SPACE(8)+lcNonMajTl))
LFCR = CHR(10) + CHR(13)
DIMENSION XTOTAL(3,10)
DIMENSION CLRTOT(10)
DIMENSION ORDTOT(10)
*-- STANDARD REPORT 'WIDE'
R_WIDTH  = 'W'
*------------------------------
*-Start collecting range data-*
*------------------------------
*--Collect data only when the effective options in OG change.
IF llOGFltCh
  lcRngOrd = ''
  *---Has the user selected a range of Accounts,Styles,Fabrics? What is the temp file name then?
  =(lcRpSelcBy <> 'E' ) AND lfChkRnge()
  lnOrdPos = lfItmPos('ORDHDR.ORDER')
  IF lnOrdPos > 0
    lcRngOrd  = laOgFxFlt[lnOrdPos,6]
  ENDIF  
  *--Zap the temp files if any.
  SELECT (lcTempLin1)
  ZAP
  SELECT (lcTempLin2)
  ZAP
  SET RELATION TO
  DO CASE
    CASE lcRpSelcBy = 'A'
      *--Collect data by Account.
      =lfCollData('A')
    CASE lcRpSelcBy = 'S'
      *--Collect data by Style.
      =lfCollData('S')
    CASE lcRpSelcBy = 'F'
      *--Collect data by Fabric.
      =lfCollData('F')
    OTHERWISE
      *--Collect data by Everything.
      =lfCollData('E')
  ENDCASE
ENDIF
WAIT CLEAR
SELECT (lcOrdLTmp)
GO TOP
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
IF !USED(lcStyCurs)
  CREATE CURSOR (lcStyCurs) (cStyle C(12),cColor C(6), cFabric C(7),cFabClr C(6),;
                             nYield N(7,3),nTotYield N(8,3))
  INDEX ON cFabric+cFabClr+cStyle+cColor TAG (lcStyCurs) OF (lcStyCurs)
ENDIF  

SELECT (lcOrdLTmp)
GO TOP
*-----------------------------------------------------------
* [BREAK] SET UP SORT FILEDS & CONTROL BREAKS
*-----------------------------------------------------------
DO CASE
  *--ACCOUNT NUMBER SEQUENCE
  CASE lcRpSort ='A'
    BREAK = "CUSTOMER.ACCOUNT+SPACE(3)+CUSTOMER.BTNAME+SPACE(3)+;
            ' Rep1: '+OrdHdr.Rep1+'  Rep2: '+ OrdHdr.Rep2+;
            IIF(!EMPTY(gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE')),' Term Description: '+gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE'),'')"
    lcSubTotH = 'ACCOUNT NUMBER'
  *--ORDER NUMBER SEQUENCE
  CASE lcRpSort ='O'
    BREAK  = 'ORDER + SPACE(3) + ACCOUNT' && ACCOUNT IS INCLUDED HERE ONLY TO DISPLAY ON SUBTOTAL.IT IS NEEDED TO BREAK
    lcSubTotH = 'ORDER'
  *-- FABRIC GROUP
  CASE lcRpSort ='F'
    BREAK  = 'STYLE.FABRIC'
    lcSubTotH = 'PRIMARY FABRIC'
  *-- STYLE/COLOR
  CASE lcRpSort  = 'S'
    BREAK    = ' '
    lcSubTotH   = lcStyMajor+'\'+lcNonMajTl
  CASE lcRpSort  = 'D'
    BREAK  = 'DTOC(ORDHDR.COMPLETE)'
    lcSubTotH = 'COMPLETION DATE'
  CASE lcRpSort  = 'R'
    BREAK  = 'ORDHDR.REP1'
    lcSubTotH = 'SALESREP'
ENDCASE

* PRINT REPORT
SELECT (lcOrdLTmp)
GOTO TOP
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*ORD920                                  ORDER DETAIL REPORT - 123456789012345678901234567890                             PAGE: 1234
*MM/DD/YY                                                                                                                 TIME: HH:SS
*
*1234567 12345678901234567890 1234.99
*
*ACCT  ORDER    START  COMPLETE SP  G STYLE   CLR   (1)    (2)    (3)    (4)    (5)    (6)    (7)    (8)   PIECES   PRICE PIKTKT DIFF.

*12345 123456 MM/DD/YY MM/DD/YY XX 1234567 123 111111 222222 333333 444444 555555 666666 777777 888888 12345678 1234.99 123456 12345
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

IOXFILE = SELECT()
IF BREAK <> ' '
   HBREAK = &BREAK
ENDIF
lcStyOnly  = SUBSTR(Style,1,lnMajorLen) 
lcAllSty   = STYLE
CTLOR      = ACCOUNT+ORDER+IIF(ORDHDR.MULTIPO,'*MULTI-PO*',ORDHDR.CUSTPO)
lcOrder    = ORDER
lcStyDesc  = STYLE.DESC
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
*B603328,1 (Begin) Commenting the following line because we do not 
*B603328,1         need it any more in the calculation of total yards for each color.
*lnTotYard = 0
*B603328,1 (End)
CLRTOT  = 0.00   &&-ARRAY(10)
ORDTOT  = 0.00   &&-ARRAY(10)
*-----------------------------------------------------
* [REPORT] BEGIN REPORT LOOP
*-----------------------------------------------------
R_TITLE = " ORDER DETAIL REPORT "
CLEAR TYPEAHEAD
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
*-- Add a new variable to have 2 spaces to be added to the privious positions.
lnShift = IIF(lcRpSort  ='A',2,0)
DO WHILE INKEY() <>32
  WAIT WINDOW 'Report printing - <SPACE BAR> to abort. ACCOUNT: '+ACCOUNT+' ORDER: '+ORDER  NOWAIT
  =lfHeader()
  SELECT (lcOrdLTmp)
  LEVEL = 0
  IF SUBSTR(STYLE,1,lnMajorLen) <> lcStyOnly
    LEVEL = 1
  ENDIF
  DOCLR = (lcRpSort ='S' .AND. STYLE<>lcAllSty)
  DOORD = IIF(lcRpSort ='A' .AND. ;
          ACCOUNT+ORDER+IIF(ORDHDR.MULTIPO,'*MULTI-PO*',ORDHDR.CUSTPO)<>CTLOR, .T., .F.)
  IF !EMPTY(BREAK)
    IF HBREAK <> &BREAK
      LEVEL = 2
    ENDIF
  ENDIF
  *----------------------------------------------------
  * [DOCLR] STYLE/CLR SUB TOTALS
  *----------------------------------------------------
  IF DOCLR
    *-- Printing the total yield per color in case of sorting by style/color.
    IF lcRpPrint = "Y"
       lnCurrAlias = SELECT(0)
       SELECT (lcStyCurs)
       GOTO TOP          
       DO WHILE !EOF()
         lcFabric  = cFabric
         lcItemClr = cFabClr
         lnTotYield = 0
         SCAN REST WHILE cFabric+cFabClr+cStyle+cColor = lcFabric+lcItemClr
           lnTotYield = lnTotYield + nTotYield
         ENDSCAN
         *B603328,1 (Begin) Printing the lnTotYield instead of lnTotYard because
         *B603328,1         we calculate the total yards in lfGetYield
         *@ Row,00 SAY lcItemClr + "  =  " + ALLTRIM(STR(lnTotYard,8,3))
         *lnTotYard = 0
         @ Row,00 SAY lcItemClr + "  =  " + ALLTRIM(STR(lnTotYield,8,3))
         *B603328,1 (End)
         Row = Row + 1
         =lfHeader()
       ENDDO
       ZAP
       SELECT(lnCurrAlias)
         =lfHeader()
       ENDIF  
       IF (lcRpReport = 'S' .AND. lcRpStkWip = 'N')
         @ ROW,056 SAY CLRTOT(1,1)  PICTURE '@Z 99999'
         @ ROW,062 SAY CLRTOT(1,2)  PICTURE '@Z 99999'
         @ ROW,068 SAY CLRTOT(1,3)  PICTURE '@Z 99999'
         @ ROW,074 SAY CLRTOT(1,4)  PICTURE '@Z 99999'
         @ ROW,080 SAY CLRTOT(1,5)  PICTURE '@Z 99999'
         @ ROW,086 SAY CLRTOT(1,6)  PICTURE '@Z 99999'
         @ ROW,092 SAY CLRTOT(1,7)  PICTURE '@Z 99999'
         @ ROW,098 SAY CLRTOT(1,8)  PICTURE '@Z 99999'
         @ ROW,102 SAY CLRTOT(1,9)  PICTURE '9999999'
         *B605443 TMI [START] Remove Dollar sign if print by Yield.
         *@ ROW,118 SAY CLRTOT(1,10) PICTURE '$9999999.99'
         @ ROW,118 SAY CLRTOT(1,10) PICTURE IIF(lcRpPrint = "Y",'9','$')+"9999999.99"
         *B605443 TMI [END  ] Remove Dollar sign if print by Yield.
       ELSE
         ROW = ROW+1
         =lfHeader()
         @ ROW,030 SAY '***'
         @ ROW,034 SAY LEFT(lcAllSty,12)
         @ ROW,047 SAY RIGHT(lcAllSty,6)
         @ ROW,056 SAY CLRTOT(1,1)  PICTURE '@Z 99999'
         @ ROW,062 SAY CLRTOT(1,2)  PICTURE '@Z 99999'
         @ ROW,068 SAY CLRTOT(1,3)  PICTURE '@Z 99999'
         @ ROW,074 SAY CLRTOT(1,4)  PICTURE '@Z 99999'
         @ ROW,080 SAY CLRTOT(1,5)  PICTURE '@Z 99999'
         @ ROW,086 SAY CLRTOT(1,6)  PICTURE '@Z 99999'
         @ ROW,092 SAY CLRTOT(1,7)  PICTURE '@Z 99999'
         @ ROW,098 SAY CLRTOT(1,8)  PICTURE '@Z 99999'
         @ ROW,102 SAY CLRTOT(1,9)  PICTURE '9999999'
         *B605443 TMI [START] Remove Dollar sign if print by Yield.
         *@ ROW,118 SAY CLRTOT(1,10) PICTURE '$9999999.99'
         @ ROW,118 SAY CLRTOT(1,10) PICTURE IIF(lcRpPrint = "Y",'9','$')+"9999999.99"
         *B605443 TMI [END  ] Remove Dollar sign if print by Yield.
       ENDIF
       ROW = ROW+2
       =lfHeader()
       lcAllSty = STYLE
       CLRTOT = 0.00
     ENDIF
     *----------------------------------------------------
     * [DOORD] ACCOUNT/ORDER SUB TOTALS
     *----------------------------------------------------
     IF DOORD
       *--Printing the total yield of the primary fabric of style/color.
       IF lcRpPrint = "Y"
         lnCurrAlias = SELECT(0)
         SELECT (lcStyCurs)
         GOTO TOP
         DO WHILE !EOF()
           lcFabric  = cFabric
           lcItemClr = cFabClr
           lnTotYield = 0
           SCAN REST WHILE cFabric+cFabClr+cStyle+cColor = lcFabric+lcItemClr
             lnTotYield = lnTotYield + nTotYield
           ENDSCAN
            *B603328,1 (Begin) Printing the lnTotYield instead of lnTotYard
            *B603328,1         because we calculate the total yards in lfGetYield
            *@ Row,00 SAY ALLTRIM(lcFabric)+"/"+lcItemClr + "  =  " +;
                          ALLTRIM(STR(lnTotYard,8,3))
            *lnTotYard = 0
            @ Row,00 SAY ALLTRIM(lcFabric)+"/"+lcItemClr + "  =  " +;
                          ALLTRIM(STR(lnTotYield,8,3))
            *B603328,1 (End)
           Row = Row + 1
           =lfHeader()
         ENDDO
         ZAP
         SELECT(lnCurrAlias)
         =lfHeader()
       ENDIF  
       @ ROW,000 SAY '***'
       @ ROW,006 SAY LEFT(CTLOR,5)
       @ ROW,012 SAY SUBSTR(CTLOR,6,6)
       @ ROW,019 SAY 'CUST. PO# :'
       @ ROW,031 SAY RIGHT(CTLOR,10)
       @ ROW,056+lnShift SAY ORDTOT(1,1)  PICTURE '@Z 99999'
       @ ROW,062+lnShift SAY ORDTOT(1,2)  PICTURE '@Z 99999'
       @ ROW,068+lnShift SAY ORDTOT(1,3)  PICTURE '@Z 99999'
       @ ROW,074+lnShift SAY ORDTOT(1,4)  PICTURE '@Z 99999'
       @ ROW,080+lnShift SAY ORDTOT(1,5)  PICTURE '@Z 99999'
       @ ROW,086+lnShift SAY ORDTOT(1,6)  PICTURE '@Z 99999'
       @ ROW,092+lnShift SAY ORDTOT(1,7)  PICTURE '@Z 99999'
       @ ROW,098+lnShift SAY ORDTOT(1,8)  PICTURE '@Z 99999'
       @ ROW,102+lnShift SAY ORDTOT(1,9)  PICTURE '9999999'
       @ ROW,118+lnShift SAY ORDTOT(1,10) PICTURE '$9999999.99'
       ROW = ROW+2
       =lfHeader()
       CTLOR = ACCOUNT+ORDER+IIF(ORDHDR.MULTIPO,'*MULTI-PO*',ORDHDR.CUSTPO)
       ORDTOT = 0.00
       SELECT NOTEPAD                
       IF lcRpNotPd = 'Y'
         XPRV_ROW = ROW
         IF SEEK('B'+lcOrder)
           lnMemoWdth = SET('MEMOWIDTH')
           SET MEMOWIDTH TO 75
           lnMemoLnNo = 0      
           lnMemolins = 0
           Row = Row +1
           @ Row,02 SAY '*-- N O T E P A D --*'
           Row = Row +1
           lnMemolins = MEMLINES(mNotes)
           IF lnMemolins > 0
             DO WHILE lnMemoLnNo <> lnMemolins .AND. INKEY() <> 32
               IF ROW >=54
                 =lfHeader()
               ENDIF
               lnMemoLnNo = lnMemoLnNo + 1
               @ Row,02 SAY MLINE(mNotes,lnMemoLnNo)
               Row = Row + 1
             ENDDO
           ENDIF
           @ Row,02 SAY '*-- END OF NOTEPAD --*'
           lnMemoLnNo = 0      
           lnMemolins = 0
           SET MEMOWIDTH TO (lnMemoWdth)
         ENDIF
         ROW=IIF(ROW=XPRV_ROW,ROW,ROW+1)   && INCREMNET ROW ONLY IF NOTEPAD PRINTED                
       ENDIF
     ENDIF       
     SELECT (lcOrdLTmp)
     lcOrder=ORDER
     *----------------------------------------------------
     * [LEVEL 1] STYLE SUB TOTALS
     *----------------------------------------------------
     IF LEVEL >=1
       NEWSTYLE = .F.
       * PRINT STYLE SUBTOTAL ONLY FOR REPORTS S,F
       IF lcRpSort  $ 'SF'
         @ ROW,000 SAY REPLICATE('-',132)
         ROW = ROW+1
         =lfHeader()
         @ ROW,000 SAY SUBSTR(lcStyDesc,1,12)
         @ ROW,013 SAY '('+STYFAB+')'          
         @ ROW,026 SAY lcStyOnly
         @ ROW,039 SAY '******'
         @ ROW,056 SAY XTOTAL(1,1)  PICTURE '@Z 99999'
         @ ROW,062 SAY XTOTAL(1,2)  PICTURE '@Z 99999'
         @ ROW,068 SAY XTOTAL(1,3)  PICTURE '@Z 99999'
         @ ROW,074 SAY XTOTAL(1,4)  PICTURE '@Z 99999'
         @ ROW,080 SAY XTOTAL(1,5)  PICTURE '@Z 99999'
         @ ROW,086 SAY XTOTAL(1,6)  PICTURE '@Z 99999'
         @ ROW,092 SAY XTOTAL(1,7)  PICTURE '@Z 99999'
         @ ROW,098 SAY XTOTAL(1,8)  PICTURE '@Z 99999'
         @ ROW,102 SAY XTOTAL(1,9)  PICTURE '9999999'
         *B605443,1 TMI [START] Remove Dollar sign if print by Yield.
         *@ ROW,118 SAY XTOTAL(1,10) PICTURE '$9999999.99'
         @ ROW,118 SAY XTOTAL(1,10) PICTURE IIF(lcRpPrint = "Y",'9','$')+"9999999.99"
         *B605443,1 TMI [END  ] Remove Dollar sign if print by Yield.
         ROW = ROW+1
         =lfHeader()
         @ ROW,000 SAY REPLICATE('-',132)
         ROW = ROW+2
         =lfHeader()
 
         *B802784,1 (Begin) Remark the following line and get the style only.
         *lcStyOnly = STYLE
         lcStyOnly  = SUBSTR(Style,1,lnMajorLen)
         *B802784,1 (End)
 
         lcStyDesc  = STYLE.DESC
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
       *--Printing the total yield of the primary fabric of style/color in case of sorting by fabric or order.
       IF lcRpSort  $ 'FO' AND lcRpPrint = "Y"
         lnCurrAlias = SELECT(0)
         SELECT (lcStyCurs)
         GOTO TOP
         DO WHILE !EOF()
           lcFabric  = cFabric
           lcItemClr = cFabClr
           lnTotYield = 0
           SCAN REST WHILE cFabric+cFabClr+cStyle+cColor = lcFabric+lcItemClr
             lnTotYield = lnTotYield + nTotYield
           ENDSCAN
           IF lcRpSort  = 'O'
              *B603328,1 (Begin) Printing the lnTotYield instead of lnTotYard 
              *B603328,1         because we calculate the total yards in lfGetYield
              *@ Row,00 SAY ALLTRIM(lcFabric)+"/"+lcItemClr + "  =  " +;
                            ALLTRIM(STR(lnTotYard,8,3))
              *lnTotYard = 0
              @ Row,00 SAY ALLTRIM(lcFabric)+"/"+lcItemClr + "  =  " +;
                            ALLTRIM(STR(lnTotYield,8,3))
              *B603328,1 (End)
           ELSE
             *B603328,1 Printing the lnTotYield instead of lnTotYard 
             *B603328,1 because we calculate the total yards in lfGetYield
             *@ Row,00 SAY lcItemClr + "  =  " +;
                           ALLTRIM(STR(lnTotYard,8,3))            
             *lnTotYard = 0
             @ Row,00 SAY lcItemClr + "  =  " +;
                           ALLTRIM(STR(lnTotYield,8,3))
             *B603328,1 (End)
           ENDIF                
           Row = Row + 1
           =lfHeader()
         ENDDO
         ZAP
         SELECT (lnCurrAlias)
       ENDIF  
       @ ROW,000 SAY REPLICATE('-',132)
       ROW = ROW+1
       =lfHeader()
       IF lcRpSort ='O'
         @ ROW,000 SAY SUBSTR(HBREAK,10,5)
         @ ROW,006 SAY SUBSTR(HBREAK,1,6)
       ELSE
         IF lcRpSort ='R'
           SELE SALESREP
           SEEK HBREAK
           XNAME=NAME
           SELECT (lcOrdLTmp)
         ELSE
           XNAME=''
         ENDIF
         @ ROW,000 SAY HBREAK+' '+XNAME
      ENDIF
      *-- Adding the If command in order to don't print the totals in case of sorting by account.
      IF lcRpSort  <> 'A'
        IF LFCR $ HBREAK
          ROW = ROW+1
          =lfHeader()
        ENDIF
        @ ROW,056+lnShift SAY XTOTAL(2,1)  PICTURE '@Z 99999'
        @ ROW,062+lnShift SAY XTOTAL(2,2)  PICTURE '@Z 99999'
        @ ROW,068+lnShift SAY XTOTAL(2,3)  PICTURE '@Z 99999'
        @ ROW,074+lnShift SAY XTOTAL(2,4)  PICTURE '@Z 99999'
        @ ROW,080+lnShift SAY XTOTAL(2,5)  PICTURE '@Z 99999'
        @ ROW,086+lnShift SAY XTOTAL(2,6)  PICTURE '@Z 99999'
        @ ROW,092+lnShift SAY XTOTAL(2,7)  PICTURE '@Z 99999'
        @ ROW,098+lnShift SAY XTOTAL(2,8)  PICTURE '@Z 99999'
        @ ROW,102+lnShift SAY XTOTAL(2,9)  PICTURE '9999999'
        *B605443,1 TMI [START] Remove Dollar sign if print by Yield.
        *@ ROW,118+lnShift SAY XTOTAL(2,10)  PICTURE '$9999999.99'
        @ ROW,118+lnShift SAY XTOTAL(2,10) PICTURE IIF(lcRpPrint = "Y",'9','$')+"9999999.99"
        *B605443,1 TMI [END  ] Remove Dollar sign if print by Yield.

      ENDIF
      ROW = ROW+1
      =lfHeader()
      @ ROW,000 SAY REPLICATE('-',132)
      ROW = ROW+2
      =lfHeader()
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
        DO RPT_HDR WITH 'SOMOR400','(SUB TOTALS BY: ' + lcSubTotH +') '+ lcRpOpTitl ,R_WIDTH
        IF lcRpSort  = 'S'
          *-- Changing the layout by removing the price,piktkt, and diff columns and adding the dyelot and c/t
          *-- Printed the header for the group field which is the 'G' character after   the 'SP' header ('SP  G').
          IF lcRpReport = "D" AND lcRpPrint = "Y"
            @ 05,000 SAY 'ACCT  ORDER  STORE#   COMPLETE SP  G CUSTOMER             (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8) PIECES  Yield       Yards'
          ELSE
            @ 05,000 SAY 'ACCT  ORDER  STORE#   COMPLETE SP  G CUSTOMER             (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8) PIECES DEYLOT    C/T'
          ENDIF 
        ELSE
          IF lcRpSort  = 'A'
            IF lcRpReport = "D" AND lcRpPrint = "Y"
              @ 05,000 SAY 'ACCT  STORE#     START  COMPLETE SP  G '+lcStyClrS+'   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8) PIECES  Yield       Yards'
            ELSE
              @ 05,000 SAY 'ACCT  STORE#     START  COMPLETE SP  G '+lcStyClrS+'   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8) PIECES DEYLOT    C/T'
            ENDIF
          ELSE
            IF lcRpReport = "D" AND lcRpPrint = "Y"
              @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  G '+lcStyClrS+'   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8) PIECES  Yield       Yards'
            ELSE
              @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  G '+lcStyClrS+'   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8) PIECES DEYLOT    C/T'
            ENDIF
          ENDIF  
        ENDIF
        @ 06,000 SAY REPLICATE('-',132)
        ROW = 7
      ENDIF
      NEWSTYLE = .F.
      SELECT SCALE
      SEEK 'S'+XSCALE
      @ ROW,056+lnShift SAY PADL(ALLTRIM(SZ1),5)
      @ ROW,062+lnShift SAY PADL(ALLTRIM(SZ2),5)
      @ ROW,068+lnShift SAY PADL(ALLTRIM(SZ3),5)
      @ ROW,074+lnShift SAY PADL(ALLTRIM(SZ4),5)
      @ ROW,080+lnShift SAY PADL(ALLTRIM(SZ5),5)
      @ ROW,086+lnShift SAY PADL(ALLTRIM(SZ6),5)
      @ ROW,092+lnShift SAY PADL(ALLTRIM(SZ7),5)
      @ ROW,098+lnShift SAY PADL(ALLTRIM(SZ8),5)
      ROW=ROW+1
    ENDIF
    SELE (lcOrdLTmp)
    IF lcRpSort ='S' .AND. XPRV_KEY <> STYLE
      XPRV_KEY = STYLE
      SELE STYLE
      IF SEEK( XPRV_KEY)
        lcColDesc = SUBSTR(gfCodDes(SUBSTR(STYLE,lnNonMajSt,lnColorLen),'COLOR'),1,10)

        *B802784,1 (Begin) Say the style.
        *@ ROW,000 SAY lcStyOnly  = Style
        @ ROW,000 SAY lcStyOnly
        *B802784,1 (End)

        @ ROW,020 SAY lcColDesc 
        DO CASE
          CASE lcRpStkWip = 'S'
            @ ROW,032 SAY 'STOCK :'
            @ ROW,056 SAY STK1  PICTURE '@Z 99999'
            @ ROW,062 SAY STK2  PICTURE '@Z 99999'
            @ ROW,068 SAY STK3  PICTURE '@Z 99999'
            @ ROW,074 SAY STK4  PICTURE '@Z 99999'
            @ ROW,080 SAY STK5  PICTURE '@Z 99999'
            @ ROW,086 SAY STK6  PICTURE '@Z 99999'
            @ ROW,092 SAY STK7  PICTURE '@Z 99999'
            @ ROW,098 SAY STK8  PICTURE '@Z 99999'
            @ ROW,102 SAY TOTSTK  PICTURE '9999999'
          CASE lcRpStkWip = 'W'            &&TAK06/20/94
            @ ROW,IIF(lcRpSort  <> 'A',032,34) SAY 'WIP + STOCK :'
            @ ROW,032 SAY 'WIP + STOCK :'
            @ ROW,056 SAY WIP1+STK1  PICTURE '@Z 999999'
            @ ROW,062 SAY WIP2+STK2  PICTURE '@Z 999999'
            @ ROW,068 SAY WIP3+STK3  PICTURE '@Z 999999'
            @ ROW,074 SAY WIP4+STK4  PICTURE '@Z 999999'
            @ ROW,080 SAY WIP5+STK5  PICTURE '@Z 999999'
            @ ROW,086 SAY WIP6+STK6  PICTURE '@Z 999999'
            @ ROW,092 SAY WIP7+STK7  PICTURE '@Z 999999'
            @ ROW,098 SAY WIP8+STK8  PICTURE '@Z 999999'
            @ ROW,102 SAY TOTWIP+TOTSTK  PICTURE '9999999'
          CASE lcRpStkWip = 'I'             && TMI 11/21/94
            @ ROW,032 SAY 'WIP :'
            @ ROW,056 SAY WIP1  PICTURE '@Z 99999'
            @ ROW,062 SAY WIP2  PICTURE '@Z 99999'
            @ ROW,068 SAY WIP3  PICTURE '@Z 99999'
            @ ROW,074 SAY WIP4  PICTURE '@Z 99999'
            @ ROW,080 SAY WIP5  PICTURE '@Z 99999'
            @ ROW,086 SAY WIP6  PICTURE '@Z 99999'
            @ ROW,092 SAY WIP7  PICTURE '@Z 99999'
            @ ROW,098 SAY WIP8  PICTURE '@Z 99999'
            @ ROW,102 SAY TOTWIP  PICTURE '9999999'
        ENDCASE
        IF .NOT. (lcRpReport = 'S' .AND. lcRpStkWip='N')
          ROW = ROW+1
        ENDIF
      ENDIF
      SELE (lcOrdLTmp)
    ENDIF
    *********************************************************************
    ****************** PRINT THE LINE ITEM DETAIL ***********************
    *********************************************************************
    SELECT (lcOrdLTmp)
    IF lcRpReport = 'D'
      @ ROW,000 SAY ACCOUNT
      @ ROW,006 SAY IIF(lcRpSort  ='A',STORE,ORDER)
      @ ROW,013+lnShift SAY IIF(lcRpSort  ='S',STORE,ORDHDR.START)
      @ ROW,022+lnShift SAY ORDHDR.COMPLETE
      @ ROW,031+lnShift SAY ORDHDR.STATUS
      @ ROW,032+lnShift SAY ORDHDR.PRIORITY
      @ ROW,033+lnShift SAY Group
      lcStyleCod = SUBSTR(Style,1,lnMajorLen)
      lcColorCod = SUBSTR(STYLE,lnNonMajSt,lnColorLen)
      IF lcRpSort  = 'S'
        @ ROW,035+2+lnShift SAY SUBSTR(CUSTOMER.STNAME,1,10)
      ELSE
        @ ROW,035+2+lnShift SAY STYLE
      ENDIF
      XSUM_STORE=(llRpSummrz .AND. ORDHDR.MULTI='Y')
      IF XSUM_STORE
        STORE 0 TO XQTY1,XQTY2,XQTY3,XQTY4,XQTY5,XQTY6,XQTY7,XQTY8,lnTotQty,XAMT
        XSTYLE=STYLE
        XCOMDT=DTOS(ORDHDR.COMPLETE)
        lcOrder=ORDER
        SCAN WHILE STYLE+DTOS(ORDHDR.COMPLETE)+ORDER = XSTYLE+XCOMDT+lcOrder
          FOR XMCOUNT = 1 TO 8
            Z=STR(XMCOUNT,1)
            XQTY&Z=XQTY&Z+QTY&Z
          ENDFOR
          lnTotQty= lnTotQty+TOTQTY
          XAMT   = XAMT   +(TOTQTY*PRICE)
        ENDSCAN
        lnTotQty=XQTY1+XQTY2+XQTY3+XQTY4+XQTY5+XQTY6+XQTY7+XQTY8
        @ ROW,056+lnShift SAY XQTY1    PICTURE '@Z 99999'
        @ ROW,062+lnShift SAY XQTY2    PICTURE '@Z 99999'
        @ ROW,068+lnShift SAY XQTY3    PICTURE '@Z 99999'
        @ ROW,074+lnShift SAY XQTY4    PICTURE '@Z 99999'
        @ ROW,080+lnShift SAY XQTY5    PICTURE '@Z 99999'
        @ ROW,086+lnShift SAY XQTY6    PICTURE '@Z 99999'
        @ ROW,092+lnShift SAY XQTY7    PICTURE '@Z 99999'
        @ ROW,098+lnShift SAY XQTY8    PICTURE '@Z 99999'
        @ ROW,102+lnShift SAY lnTotQty  PICTURE '9999999'
        *--If the report is printed by yield then we will pass a lfGetYield
        *--function to get the yield of the primary fabric in the style.
        IF lcRpPrint = "Y" AND !EMPTY(Style.Fabric)
          =lfGetYield(lcStyleCod,lcColorCod)
          @ ROW,110+lnShift SAY lnYield PICTURE "@Z 999.999"
          @ ROW,120+lnShift SAY lnTotQty*lnYield PICTURE "@Z 99999999.99"
          *B603328,1 (Begin) commented out.
          *lnTotYard = lnTotYard + xTotQty*lnYield
          *B603328,1 (End)
        ELSE
          @ ROW,111+lnShift SAY Dyelot
          @ ROW,122+lnShift SAY CutTkt
        ENDIF  
      ELSE
        @ ROW,056+lnShift SAY QTY1    PICTURE '@Z 99999'
        @ ROW,062+lnShift SAY QTY2    PICTURE '@Z 99999'
        @ ROW,068+lnShift SAY QTY3    PICTURE '@Z 99999'
        @ ROW,074+lnShift SAY QTY4    PICTURE '@Z 99999'
        @ ROW,080+lnShift SAY QTY5    PICTURE '@Z 99999'
        @ ROW,086+lnShift SAY QTY6    PICTURE '@Z 99999'
        @ ROW,092+lnShift SAY QTY7    PICTURE '@Z 99999'
        @ ROW,098+lnShift SAY QTY8    PICTURE '@Z 99999'
        @ ROW,102+lnShift SAY TOTQTY  PICTURE '9999999'
        *-- Getting the last cuttkt done for the selected order.
        *-- Then printing the dyelot for the selected style+color of the order
        IF lcRpPrint = "Y" AND !EMPTY(Style.Fabric)
          =lfGetYield(lcStyleCod,lcColorCod)
          @ ROW,110+lnShift SAY lnYield PICTURE "@Z 999.999"
          @ ROW,118+lnShift SAY TotQty*lnYield PICTURE "@Z 99999999.99"
          *B603328,1 (Begin) Commented out.
          *lnTotYard = lnTotYard + TotQty*lnYield
          *B603328,1 (End)
        ELSE
          @ ROW,111+lnShift SAY Dyelot
          @ ROW,122+lnShift SAY CutTkt
        ENDIF  
      ENDIF
      ROW = ROW+1
    ENDIF
    SELECT (lcOrdLTmp)
    IF llNotes .AND. lcRpLinNot = 'Y' .AND. lcRpReport = 'D' .AND. !llRpSummrz
      IF !EMPTY(NOTE_MEM)
        ROW = ROW + 1
        =lfHeader()
        @ ROW , 00 SAY 'ORDER NOTES  :'
        FOR X = 1 TO MEMLINES(NOTE_MEM)
          @ ROW,15 SAY MLINE(NOTE_MEM,X)
          ROW = ROW + 1
          =lfHeader()
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
    XTOTAL(1,9) = XTOTAL(1,9) +  IIF(XSUM_STORE,lnTotQty,TOTQTY)
    *--B605443,1 TMI [START] Sum YARD quantity in case of PRINT by Yield.
    *XTOTAL(1,10) = XTOTAL(1,10) + IIF(XSUM_STORE,XAMT,TOTQTY*PRICE)
    XTOTAL(1,10) = XTOTAL(1,10) + IIF(XSUM_STORE,  XAMT ,;
                                  TOTQTY*IIF(lcRpPrint = "Y",&lcStyCurs..nYield,PRICE))
    *--B605443,1 TMI [END  ] Sum YARD quantity in case of PRINT by Yield.
    IF lcRpSort  = 'S'
       CLRTOT(1) = CLRTOT(1) +  IIF(XSUM_STORE,XQTY1,QTY1)
       CLRTOT(2) = CLRTOT(2) +  IIF(XSUM_STORE,XQTY2,QTY2)
       CLRTOT(3) = CLRTOT(3) +  IIF(XSUM_STORE,XQTY3,QTY3)
       CLRTOT(4) = CLRTOT(4) +  IIF(XSUM_STORE,XQTY4,QTY4)
       CLRTOT(5) = CLRTOT(5) +  IIF(XSUM_STORE,XQTY5,QTY5)
       CLRTOT(6) = CLRTOT(6) +  IIF(XSUM_STORE,XQTY6,QTY6)
       CLRTOT(7) = CLRTOT(7) +  IIF(XSUM_STORE,XQTY7,QTY7)
       CLRTOT(8) = CLRTOT(8) +  IIF(XSUM_STORE,XQTY8,QTY8)
       CLRTOT(9) = CLRTOT(9) +  IIF(XSUM_STORE,lnTotQty,TOTQTY)
       *--B605443,1 TMI [START] Sum YARD quantity in case of PRINT by Yield.
       *CLRTOT(10) = CLRTOT(10) + IIF(XSUM_STORE,XAMT,TOTQTY*PRICE)
       CLRTOT(10) = CLRTOT(10) + IIF(XSUM_STORE,  XAMT  ,;
                                 TOTQTY*IIF(lcRpPrint = "Y",&lcStyCurs..nYield,PRICE))
       *--B605443,1 TMI [END  ] Sum YARD quantity in case of PRINT by Yield.
    ENDIF
    IF lcRpSort  = 'A'
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
    SELE (lcOrdLTmp)
    IF !XSUM_STORE
      SKIP                && All cases except summurize case due to SCAN.
    ENDIF
ENDDO
*---------------------- END OF REPORT -------------------------
*----------------------------------------------------
* [LEVEL 3] GRAND TOTALS
*----------------------------------------------------
IF ROW >=60
  =lfHeader()
ENDIF
@ ROW,000 SAY REPLICATE('=',132)
ROW = ROW+1
=lfHeader()
@ ROW,010+lnShift SAY 'GRAND TOTALS'
@ ROW,056+lnShift SAY XTOTAL(3,1)  PICTURE '@Z 99999'
@ ROW,062+lnShift SAY XTOTAL(3,2)  PICTURE '@Z 99999'
@ ROW,068+lnShift SAY XTOTAL(3,3)  PICTURE '@Z 99999'
@ ROW,074+lnShift SAY XTOTAL(3,4)  PICTURE '@Z 99999'
@ ROW,080+lnShift SAY XTOTAL(3,5)  PICTURE '@Z 99999'
@ ROW,086+lnShift SAY XTOTAL(3,6)  PICTURE '@Z 99999'
@ ROW,092+lnShift SAY XTOTAL(3,7)  PICTURE '@Z 99999'
@ ROW,098+lnShift SAY XTOTAL(3,8)  PICTURE '@Z 99999'
@ ROW,102+lnShift SAY XTOTAL(3,9)  PICTURE '9999999'
*B605443,1 TMI [START] Remove the dollar sign in case of yield
*@ ROW,118+lnShift SAY XTOTAL(3,10)  PICTURE '$9999999.99'
@ ROW,118+lnShift SAY XTOTAL(3,10) PICTURE IIF(lcRpPrint = "Y",'9','$')+"9999999.99"
*B605443,1 TMI [END  ] Remove the dollar sign in case of yield


ROW = ROW+1
=lfHeader()
@ ROW,000 SAY REPLICATE('=',132)

DO ENDREPORT
SELECT (lcStyCurs)
ZAP
SET DEVICE TO SCREEN
RETURN

*!***************************************************************
*! Name      : lfGetYield
*! Developer : Adel Mohammed El Gazzar (ADEL)
*!       DATE: 09/28/1999
*! Purposes  : To get the yield of the primary fabric from the style cost sheet.
*!***************************************************************
FUNCTION lfGetYield
PARAMETERS lcStyKey,lcClrKey

PRIVATE lnAlias

lnAlias = SELECT(0)
STORE 0 TO lnYield

IF SEEK(Style.Fabric+lcClrKey+lcStyKey+lcClrKey,lcStyCurs)
  lnYield = &lcStyCurs..nYield
  *B603328,1 (Begin) Changing the replacement of nTotYield by replacing it
  *B603328,1         with lnYield*&OrdLinTemp which is the total yards.
  *REPLACE &lcStyCurs..nTotYield WITH &lcStyCurs..nTotYield + lnYield
  REPLACE &lcStyCurs..nTotYield WITH &lcStyCurs..nTotYield + (lnYield*&lcOrdLTmp..TotQty)
  *B603328,1 (End)
  RETURN
ENDIF

IF SEEK(lcStyKey,'BOM')
  SELECT Bom
  *:B804500,1 MHM 01/29/2002  loop for all colors [start]
  *LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = lcStyKey ;
  *     FOR cCatgTyp = "F" AND SUBSTR(Item,1,7) = Style.Fabric
  SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = lcStyKey ;
       FOR cCatgTyp = "F" AND SUBSTR(Item,1,7) = Style.Fabric
   *:B804500,1 MHM 01/29/2002  [End]
     
  lcFabClr = ""
  *:B804500,1 MHM 01/29/2002  no need for check found[start]
  *IF FOUND()
  *:B804500,1 MHM 01/29/2002  [end]
    DO CASE
      *-- Case of same as
      CASE SUBSTR(CITMMASK,lnNonMajSt,lnColorLen) = "******" AND IClr = "******"
        lcFabClr = lcClrKey 
      *-- Case of Constant  
      CASE SUBSTR(CITMMASK,lnNonMajSt,lnColorLen) = "******" AND IClr <> "******"
        lcFabClr = IClr
      *-- Case of different  
      CASE SUBSTR(CITMMASK,lnNonMajSt,lnColorLen) <> "******" AND IClr <> "******"
        LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = lcStyKey;
                    FOR cCatgTyp = "F" AND SUBSTR(Item,1,7) = Style.Fabric;
                        AND SUBSTR(CITMMASK,lnNonMajSt,lnColorLen) = lcClrKey
        *:B804500,1 MHM 01/29/2002  if color not foud loop [start]
        *lcFabClr = IClr
        IF FOUND()
          lcFabClr = IClr
        ELSE
          LOOP
        ENDIF  
        *:B804500,1 MHM 01/29/2002  [end]
    ENDCASE
    lnYield = nBomTotQty
    IF lnYield <> 0
      *B603328,1 (Begin) Changing the replacement of nTotYield by replacing it with  
      *B603328,1         lnYield*&OrdLinTemp which is the total yards.
      *INSERT INTO (lcStyCurs) (cStyle,cColor,cFabric,cFabClr,nYield,nTotYield) ;
                   VALUES (lcStyKey,lcClrKey,Style.Fabric,lcFabClr,lnYield,lnYield)
      INSERT INTO (lcStyCurs) (cStyle,cColor,cFabric,cFabClr,nYield,nTotYield) ;
                   VALUES (lcStyKey,lcClrKey,Style.Fabric,lcFabClr,lnYield,lnYield*&lcOrdLTmp..TotQty)
      *B603328,1 (End)
    ENDIF               
  *:B804500,1 MHM 01/29/2002  [start]
  *ENDIF
  ENDSCAN
  *:B804500,1 MHM 01/29/2002  [end]
  
ENDIF
SELECT(lnAlias )

*!***************************************************************
*! Name      : lfHeader
*! Developer : Adel Mohammed El Gazzar (ADEL)
*!       DATE: 09/28/1999
*! Purposes  : To get the yield of the primary fabric
*!             from the style cost sheet.
*!***************************************************************
*! Example   : = lfHeader()
*!***************************************************************
FUNCTION lfHeader

IF ROW >=54
  PAGENO = PAGENO +1
  DO RPT_HDR WITH 'SOMOR400','(SUB TOTALS BY: ' + lcSubTotH +') '+ lcRpOpTitl ,R_WIDTH
  IF lcRpSort  = 'S'
    IF lcRpReport = "D" AND lcRpPrint = "Y"
      @ 05,000 SAY 'ACCT  ORDER  STORE#   COMPLETE SP  G CUSTOMER             (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8) PIECES  Yield       Yards'
    ELSE
      @ 05,000 SAY 'ACCT  ORDER  STORE#   COMPLETE SP  G CUSTOMER             (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8) PIECES DEYLOT    C/T'
    ENDIF
  ELSE
    IF lcRpSort  = 'A'
      IF lcRpReport = "D" AND lcRpPrint = "Y"
        @ 05,000 SAY 'ACCT  STORE#     START  COMPLETE SP  G '+lcStyClrS+'   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8) PIECES  Yield       Yards'
      ELSE
        @ 05,000 SAY 'ACCT  STORE#     START  COMPLETE SP  G '+lcStyClrS+'   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8) PIECES DEYLOT    C/T'
      ENDIF
    ELSE
      IF lcRpReport = "D" AND lcRpPrint = "Y"
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  G '+lcStyClrS+'   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8) PIECES  Yield       Yards'
      ELSE
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  G '+lcStyClrS+'   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8) PIECES DEYLOT    C/T'
      ENDIF
    ENDIF  
  ENDIF
  @ 06,000 SAY REPLICATE('-',132)
  ROW = 7
ENDIF


****************************************************************************
* FUNC: lfwRepWhen
* DESC: To valid the OG WHEN function.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 09/28/1999
****************************************************************************
FUNCTION lfwRepWhen

STORE .T. TO llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep
IF !USED(lcOrdLTmp)
  SELECT ORDLINE
  DIMENSION laFileSt[1,4]
  =AFIELDS(laFileSt)
  DIMENSION laFileSt[ALEN(laFileSt,1)+1,4]
  laFileSt[ALEN(laFileSt,1),1] = "Fabric"
  laFileSt[ALEN(laFileSt,1),2] = "C"
  laFileSt[ALEN(laFileSt,1),3] = 7
  laFileSt[ALEN(laFileSt,1),4] = 0

  CREATE TABLE (gcWorkdir+lcTempLin1) FROM ARRAY laFileSt
  SELECT (lcTempLin1)  
  INDEX ON cordtype+order+STR(lineno,6) TAG Ordline OF (gcWorkdir+lcTempLin1+'.CDX')
  INDEX ON style+DTOS(complete)+cordtype+order+store+STR(lineno,6) TAG Ordlines OF (gcWorkdir+lcTempLin1+'.CDX')

  CREATE TABLE (gcWorkdir+lcTempLin2) FROM ARRAY laFileSt
  SELECT (lcTempLin2)  
  INDEX ON cordtype+order+STR(lineno,6) TAG Ordline OF (gcWorkdir+lcTempLin2+'.CDX')
  INDEX ON style+DTOS(complete)+cordtype+order+store+STR(lineno,6) TAG Ordlines OF (gcWorkdir+lcTempLin2+'.CDX')

ENDIF
*--Open NOTEPAD file
=gfOpenFile(gcDataDir+'NOTEPAD',gcDataDir+'NOTEPAD','SH')



****************************************************************************
* FUNC: lfCollData
* DESC: To collect data for the report .
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 09/28/1999
****************************************************************************
* Parameters : lcSelectBy  :   (A)  Selecting by Account (S) Selecting by Style.
*                          :   (F)  Selecting by Fabric  (E) Selecting by Everything.
****************************************************************************
FUNCTION lfCollData
PARAMETERS lcSelectBy

=lfNewCall()
=lfvSortBy()
RETURN
*-- end of lfCollect.
  
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/27/98
*! Purpose   : Rise change style flag, in range browse screen.
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
    *lcAlias = ALIAS()
    *SELECT STYLE
    *LOCATE FOR STYLE.Fabric = Fabric.Fabric
    *llHaveSty = FOUND()
    *-- If no styles found for this fabric
    *IF !llHaveSty
      *-- the following message is
      *-- No styles in fabric group XXX .
      *--           <Ok>
      *= gfModalGen("TRM32055B36000","Dialog",Fabric.Fabric)
    *ENDIF
    *SELECT (lcAlias)
    *RETURN llHaveSty    && Record selected only if fabric found in style file.
ENDCASE
*-- end of lfsrvSty.
*!*************************************************************
*! Name      : lfStySum
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/27/98
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

*MAB [Begin]
IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
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
*MAB [Begin]
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/27/98
*! Purpose   : Rise change account flag, in range browse screen.
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
*! Name      : lfFabSum
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/27/98
*! Purpose   : sum a specific field for the current fabric in fabric file
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
*MAB 05/11/1999 Avoiding do calculation for empty file [Begin]
*lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)
IF RECCOUNT() != 0
  lnFabRec = RECNO('FABRIC')

  SELECT Fabric_X
  SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
  SELECT Fabric
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
*MAB 05/11/1999 Avoiding do calculation for empty file [End  ]
RETURN INT(lnTotcomp)
*-- end of lfFabSum.

*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/27/98
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
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
*! Name      : lfSROrder
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/27/98
*! Purpose   : Rise change order flag, in range browse screen.
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
*-- Set Relation with customer file[Begin]
DO CASE
  CASE lcParm = 'S'

    SELECT ORDHDR
    lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.
    GO TOP
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    llClearOrd = .F.
ENDCASE
*-- end of lfsChOrder.

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/27/1998
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs

*-- Major length
lnMajorLen = LEN(ALLTRIM(gfItemMask('PM')))
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
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]


RETURN ''
*-- end of lfEvalSegs.
****************************************************************************
* FUNC: lfvForm
* DESC: To clear read.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 09/28/1999
****************************************************************************
FUNCTION lfvForm

CLEAR READ


****************************************************************************
* FUNC: lfChkRnge
* DESC: Has the user selected a range of Accounts,Styles,Fabrics?
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 09/28/1999
****************************************************************************
FUNCTION lfChkRnge
PRIVATE lcItmToGet , lnSelFile

lcRngTmp = ''
DO CASE
  CASE lcRpSelcBy = 'A'
    llAccount = .T.
    lcItmToGet = 'CUSTOMER.ACCOUNT'
  CASE lcRpSelcBy = 'S'
    llStyle = .T.
    lcItmToGet = 'STYLE.STYLE'
  CASE lcRpSelcBy = 'F'
    llFabric = .T.
    lcItmToGet = 'FABRIC.FABRIC'
ENDCASE
lnSelFile = lfItmPos(lcItmToGet)
IF lnSelFile > 0 AND USED(laOgFxFlt[lnSelFile,6])
  lcRngTmp  = laOgFxFlt[lnSelFile,6]
ENDIF  
IF EMPTY(lcRngTmp) OR !USED(lcRngTmp) OR RECCOUNT(lcRngTmp) = 0
  STORE .F. TO llAccount , llStyle , llFabric
ENDIF
*-- end of lfChkRnge.

****************************************************************************
* FUNC: lfvSelcBy
* DESC: Valid 'Select By' combo box.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 09/28/1999
****************************************************************************
FUNCTION lfvSelcBy

CLEAR READ


****************************************************************************
* FUNC: lfClearRep
* DESC: To Close the files na derase them.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 09/28/1999
****************************************************************************
FUNCTION lfClearRep

llOGFltCh = .T.
IF USED(lcTempLin1)
  USE IN (lcTempLin1)
  ERASE (gcWorkDir+lcTempLin1)+'.DBF'
  ERASE (gcWorkDir+lcTempLin1)+'.CDX'
ENDIF

IF USED(lcTempLin2)
  USE IN (lcTempLin2)
  ERASE (gcWorkDir+lcTempLin2)+'.DBF'
  ERASE (gcWorkDir+lcTempLin2)+'.CDX'
ENDIF

SELECT STYLE
CLOSE INDEXES
IF FILE(gcWorkDir+"MyFabTag.CDX")
  ERASE (gcWorkDir+"MyFabTag.CDX")
ENDIF

IF USED(lcStyCurs)
  USE IN (lcStyCurs)
ENDIF

****************************************************************************
* FUNC: lfvSortBy
* DESC: To sort the orders temp  files.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 09/28/1999
****************************************************************************
FUNCTION lfvSortBy
PRIVATE lcAlias

lcAlias = ALIAS()
DO CASE
  CASE lcRpSort ='A'
    lcIndxStr =  'ACCOUNT+ORDER+STYLE+STR(LINENO,6)'
  CASE lcRpSort ='O'
    lcIndxStr = 'ORDER+STYLE+STR(LINENO,6)'
  CASE lcRpSort ='F'
    lcIndxStr = 'STYLE.FABRIC+STYLE+ORDER+STR(LINENO,6)'
  CASE lcRpSort ='S'
    lcIndxStr = 'STYLE+DTOS(ORDHDR.COMPLETE)+ORDER+STR(LINENO,6)'  
  CASE lcRpSort ='D'
    lcIndxStr = 'DTOS(ORDHDR.COMPLETE)+STYLE+ORDER+STR(LINENO,6)'
  CASE lcRpSort ='R'
    lcIndxStr = 'ORDHDR.REP1+STYLE+ORDER+STR(LINENO,6)'
ENDCASE
IF USED(lcOrdLTmp)
  SELECT (lcOrdLTmp)
ELSE
  SELECT (lcTempLin1)  
ENDIF
INDEX ON &lcIndxStr TAG (lcOrdLTmp)
SELECT (lcAlias)

****************************************************************************
* FUNC: lfItmPos
* DESC: To get the row in the fixed filter.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 09/28/1999
****************************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lcItmInFlt , lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.

****************************************************************************
* FUNC: lfNewCall
* DESC: To collect data for the report .
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 09/28/1999
****************************************************************************
FUNCTION lfNewCall
PRIVATE llHaveRec

llHaveRec = .T.
IF !EMPTY(lcRngTmp)
  lnAndPos = AT(' AND ',lcRpExp)
  IF lnAndPos = 0
    IF ("STYLE.STYLE"$UPPER(lcRpExp)) OR ("CUSTOMER.ACCOUNT"$UPPER(lcRpExp)) OR ;
       ("FABRIC.FABRIC"$UPPER(lcRpExp))
      lcRpExp = ".T."
    ENDIF
  ELSE
    lcRpExp = SUBSTR(lcRpExp,lnAndPos+4)
  ENDIF  
ENDIF

*-- Order Range
IF !EMPTY(lcRngOrd) AND USED(lcRngOrd) AND RECCOUNT(lcRngOrd) > 0
  lnAndPos = AT(' AND ',lcRpExp)
  IF lnAndPos = 0
    IF "ORDHDR.ORDER"$UPPER(lcRpExp)
      lcRpExp = ".T."
    ENDIF
  ELSE
    lcRpExp = SUBSTR(lcRpExp,lnAndPos+4)
  ENDIF  

  SELECT ORDLINE
  SET ORDER TO ORDLINE
  SET RELATION TO "O" + ORDER INTO ORDHDR , STYLE INTO STYLE
  
  SELECT (lcRngOrd)
  SCAN
    IF SEEK("O"+ORDER,"ORDLINE")
      SELECT ORDLINE
      SCAN WHILE cordtype+order+STR(lineno,6) = 'O'+&lcRngOrd..ORDER
        SCATTER MEMVAR MEMO
        m.Fabric  = Style.Fabric
        m.Account = OrdHdr.Account
        INSERT INTO (lcTempLin1) FROM MEMVAR
      ENDSCAN  
    ENDIF
  ENDSCAN
  
  IF RECCOUNT(lcTempLin1) = 0
    llHaveRec = .F.
    lcOrdLTmp = lcTempLin1
  ENDIF  

ENDIF

IF llHaveRec
  lcScaned = IIF(RECCOUNT(lcTempLin1)>0,lcTempLin1,"ORDLINE")
  *-- Select By Ranges.
  IF !EMPTY(lcRngTmp) AND USED(lcRngTmp) AND RECCOUNT(lcRngTmp) > 0
    IF lcRpSelcBy = 'A'
      SET ORDER TO TAG ORDACCT IN ORDHDR
      SET ORDER TO TAG ORDLINE IN (lcScaned)
    ELSE
      IF lcRpSelcBy = 'F'
        IF lcScaned = "ORDLINE"
          SELECT STYLE 
          IF FILE(gcWorkDir+"MyFabTag.CDX")
            SET ORDER TO MyFabTag
          ELSE
            *-- Temporary Tag on Master Style File
            INDEX ON Fabric+Style TAG MyFabTag OF (gcWorkDir+"MyFabTag.CDX")
          ENDIF
        ELSE
          SELECT (lcScaned)
          INDEX ON Fabric+Style+DTOS(complete)+cordtype+order+store+STR(lineno,6) TAG Ordlines OF (gcWorkdir+lcTempLin1+'.CDX')
        ENDIF
      ENDIF
      SET ORDER TO TAG ORDLINES IN (lcScaned)
    ENDIF

    SELECT (lcRngTmp)
    SCAN
      DO CASE
        CASE llAccount
          lcActNo = Account
          IF SEEK(Account,"ORDHDR")
            SELECT ORDHDR
            SCAN REST WHILE account+cordtype+order = &lcRngTmp..Account
              lcOrder = ORDER
              WAIT WINDOW 'SELECTING ORDER: &lcOrder ACCT: &lcActNo ...' NOWAIT              
              IF SEEK("O"+OrdHdr.Order,lcScaned)
                SELECT (lcScaned)
                SCAN REST WHILE cordtype+order+STR(lineno,6) = "O"+OrdHdr.Order
                  SCATTER MEMVAR MEMO
                  INSERT INTO (lcTempLin2) FROM MEMVAR
                ENDSCAN
              ENDIF
            ENDSCAN
          ENDIF

        CASE llStyle
          IF SEEK(SUBSTR(cStyMajor,1,lnMajorLen),lcScaned)
            SELECT (lcScaned)
            SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) =;
                            SUBSTR(&lcRngTmp..cStyMajor,1,lnMajorLen)
              lcFStyle = SUBSTR(Style,1,lnMajorLen)
              WAIT WINDOW 'Selecting orders for styles: &lcFStyle  ...' NOWAIT
              SCATTER MEMVAR MEMO
              INSERT INTO (lcTempLin2) FROM MEMVAR
            ENDSCAN
          ENDIF

        CASE llFabric
          IF lcScaned = "ORDLINE"
          
            IF SEEK(Fabric,"STYLE")
              SELECT STYLE
              SCAN REST WHILE Fabric + Style = &lcRngTmp..Fabric AND SEEK(Style.Style,"ORDLINE")
                SELECT ORDLINE
                SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) =;
                            Style.Style
                  SCATTER MEMVAR MEMO
                  INSERT INTO (lcTempLin2) FROM MEMVAR
                ENDSCAN            
              ENDSCAN
            ENDIF
          
          ELSE
          
            IF SEEK(Fabric,lcScaned)
              SELECT (lcScaned)
              SCAN REST WHILE Fabric+Style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = ;
                              &lcRngTmp..Fabric
                SCATTER MEMVAR MEMO
                INSERT INTO (lcTempLin2) FROM MEMVAR
              ENDSCAN                            
            ENDIF
          ENDIF
      ENDCASE
    ENDSCAN

    IF RECCOUNT(lcTempLin2) = 0
      llHaveRec = .F.
      lcOrdLTmp = lcTempLin2
    ENDIF  

  ENDIF
ENDIF  

IF llHaveRec
  lcOrdLTmp = IIF(RECCOUNT(lcTempLin2) > 0,lcTempLin2,lcTempLin1)
  *--Set filter for the other OG's options
  lcRpExp = lcRpExp+IIF(llRpCorGrp,' AND !EMPTY(GROUP)','')
  lcRpExp = lcRpExp+[ AND TOTQTY > 0]
  lcRpExp = STRTRAN(lcRpExp,'ORDHDR.CORDTYPE','CORDTYPE')
  lcRpExp = STRTRAN(lcRpExp,'ORDHDR.ORDER','ORDER')

  IF (lcSelectBy = 'F') AND (EMPTY(lcRngTmp) OR !USED(lcRngTmp) OR RECCOUNT(lcRngTmp)=0)
    lcRpExp = lcRpExp + [ AND !EMPTY(Style.Fabric)]
  ENDIF  
  SET ORDER TO ORDHDR IN ORDHDR

  *B802784,1 (Begin) Set order to style fro the relation.
  SET ORDER TO TAG STYLE IN STYLE
  *B802784,1 (End)

  IF RECCOUNT(lcOrdLTmp) > 0
    SELECT (lcOrdLTmp)
    SET RELATION TO
    SET RELATION TO cordtype+ORDER INTO ORDHDR ADDITIVE
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    SET FILTER TO &lcRpExp
  ELSE
    SELECT (lcOrdLTmp)
    USE
    SELECT ORDLINE
    SET RELATION TO
    SET RELATION TO cordtype+ORDER INTO ORDHDR ADDITIVE
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    GO TOP
    SET TALK ON
    LOCATE FOR &lcRpExp
    COPY REST TO (gcWorkDir+lcOrdLTmp) FOR &lcRpExp
    SET TALK OFF
    = gfOpenFile (gcWorkdir+lcOrdLTmp,' ','EX')
    SET RELATION TO cordtype+ORDER INTO ORDHDR ADDITIVE
    SET RELATION TO STYLE INTO STYLE ADDITIVE
  ENDIF

  IF lcRpSort = 'S' .AND. !llRpSummrz
    SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
  ELSE
    SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER  ADDITIVE
  ENDIF
ENDIF  
SELECT (lcOrdLTmp)
*-- end of 
