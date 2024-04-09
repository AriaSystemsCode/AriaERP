************************************************************************
* PROG: SOXES600.PRG  (Converted from 26 to 27)
* DESC: CUSTOM ORDER DETAIL REPORT (FOR XES600)
* AUTH: Adel Mohammed El Gazzar.
* DATE: 12/01/2000
* Refer to (103073)
*:************************************************************************
*: Calls : 
*:         FUNCTION  :lfEvalSegs,lfNMajType,lfwRepWhen,lfSRVSty,
*:                    lfStySum,lfvFabric,lfvAccount,lfvSelcBy,lfvSortBy,
*:                    lfvOrder,lfsrAcc,lfUpdFltVar
*:************************************************************************

*=====Declare needed Variables
*---(1) OG Variavbles
*--lcRpStySt  && Holds style status.
*--llRpDropCl && Print Drooped styles only?
*--lcRpSelcBy && Select By.
*--lcRpSort   && Sort By.
*--lcRpNotPd  && Print Notepad?
*--lcRpLinNot && Print Order Line Notes?
*--lcRpOpTitl && Optional Title
*--lcRpPrnStk && Print Stock/Wip?
*--lcRpForm   && Report form?
*--lcRpSumm   && Summarize Multi-Store?
*---(2) Report Variavbles 
lnMajLen = LEN(gfItemMask("PM"))
NULDATE  = {}
TS       = ''
TD       = ''
STORE SPACE(6) TO LOR,HOR
STORE NULDATE  TO LCO,HCO,LST,HS
XGR       = SPACE(2)
STORE SPACE(6) TO CLR1,CLR2,CLR3,CLR4,CLR5,CLR6,CLR7,CLR8
STORE SPACE(1) TO lnAllPer,lcFilter,ZST,lcAccAls,lcStyAls
STORE 0 TO XQTY1,XQTY2,XQTY3,XQTY4,XQTY5,XQTY6,XQTY7,XQTY8,XTOTQTY,XAMT
llStyRange = .T.
llAccRange = .T.
llFabRange = .T.
lcNote     = .F.
lcSortCode = ' '
llSummarize= .F.
R_WIDTH = 'W'
xTitle     = SPACE(40)
lcCancel   = .F.
llNotes    = (gfGetMemVar('M_LN_NOTE',gcAct_Comp) = 'Y')
lcSele     = ''
TCLR       = ''
lcAccount = SPACE(5)
lcFabric  = SPACE(7)
STORE SPACE(12) TO lStyle,hStyle
STORE SPACE(6) TO LOR,HOR
LFCR = CHR(10) + CHR(13)

IF !lfUpdFltVar()
  DO CASE
    CASE lcRpSelcBy = 'S' AND !llStyRange
      WAIT WINDOW "Must select Style range...!"
      SET DEVICE TO SCREEN
     CASE lcRpSelcBy = 'A' AND !llAccRange
      WAIT WINDOW "Must select Account range...!"
      SET DEVICE TO SCREEN
    CASE lcRpSelcBy = 'F' AND !llFabRange
      WAIT WINDOW "Must select Fabric...!"
      SET DEVICE TO SCREEN
  ENDCASE
  RETURN
ENDIF

*------------------------
* CREATE WORK FILE
*------------------------
IF !USED(lcRpTmp)
  SELECT ORDLINE
  COPY STRUCTURE TO (gcWorkDir+lcRpTmp)
  =gfOpenFile(gcWorkDir+lcRpTmp,'','EX')
ENDIF  

ldCompDate = {}
DIMENSION XTOTAL(3,10)
DIMENSION CLRTOT(10)
DIMENSION ORDTOT(10)

STORE 0 TO lnPerCent,lnTotalQty,lnTotalPik

IF llOgFltCh
  IF USED(lcRpTmp) AND RECCOUNT(lcRpTmp) > 0
    SELECT (lcRpTmp)
    ZAP
  ENDI
  *-----------------------------------------------
  * [ACCOUNT] APPEND BY ACCOUNT#
  *-----------------------------------------------
  IF lcSele='A'
    SELECT OrdHdr
    SET ORDER TO TAG ORDACCT
    SELECT ORDLINE
    SET ORDER TO TAG ORDLINE
    SELECT (lcAccAls)
    SCAN
      lcAccount = Account
      SELECT OrdHdr
      =SEEK(lcAccount)
      SCAN REST WHILE account+cordtype+order = lcAccount+'O'
        lcOrder = ORDER
        WAIT WINDOW 'SELECTING ORDER: &lcOrder  ACCT: &lcAccount ...' NOWAIT
        SELECT ORDLINE
        =SEEK('O'+lcOrder)
        COPY REST TO &gcWorkDir.&lcRpWork ;
        WHILE cordtype+order+STR(lineno,6) = 'O'+lcOrder FOR TOTQTY > 0
        SELECT (lcRpTmp)
        APPEND FROM (gcWorkDir+lcRpWork)
      ENDSCAN
    ENDSCAN
  ENDIF
  *-------------------- END SELECTION BY ACCOUNT# -------------------
  *-------------------------------------------------------
  *  [STYLE] APPEND BY STYLE NUMBER
  *-------------------------------------------------------
  IF lcSele = 'S'
    SELECT ORDLINE
    SET ORDER TO TAG ORDLINES
    SELECT &lcStyAls
    SCAN
      WAIT WINDOW 'Selecting orders for styles: ' + ALLTRIM(&lcStyAls..cStyMajor)+' ...' NOWAIT
      SELECT ORDLINE
      =SEEK(ALLTRIM(&lcStyAls..cStyMajor))
      COPY REST TO &gcWorkDir.&lcRpWork ;
      WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) =;
            ALLTRIM(&lcStyAls..cStyMajor) FOR TOTQTY > 0
      SELECT (lcRpTmp)
      APPEND FROM (gcWorkDir+lcRpWork)
    ENDSCAN
  ENDIF
  *----------------- END SELECTION BY STYLE NUMBER -------------------
  *--------------------------------------------------
  * [FABRIC] APPEND BY FABRIC CODE
  *--------------------------------------------------
  IF lcSele = 'F'
     SELECT ORDLINE
     SET ORDER TO TAG ORDLINES
     SELECT STYLE
     STYFILTER = "FABRIC=lcFabric"
     LOCATE ALL FOR &STYFILTER
     IF EOF()
        WAIT WINDOW 'NO STYLES IN FABRIC GROUP &lcFabric' NOWAIT
        SET DEVICE TO SCREE
        RETURN
     ENDIF
     SCAN REST FOR FABRIC = lcFabric
       STYKEY = STYLE
       WAIT WINDOW 'SELECTING ORDERS FOR STYLE: &STYKEY ...' NOWAIT
       SELECT ORDLINE
       =SEEK(STYKEY)
       COPY REST TO &gcWorkDir.&lcRpWork ;
       WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) =;
       STYKEY FOR TOTQTY > 0
       SELECT (lcRpTmp)
       APPEND FROM (gcWorkDir+lcRpWork)
     ENDSCAN
     lcSele = ' '  &&-ONLY ONE FABRIC SELECTION PERMITTED
  ENDIF
  *----------------- END SELECTION BY FABRIC -------------------
ENDIF
*------------------ END BUILD TEMPORARY LINE ITEM FILE -------------------

**---Start printing
DO WHILE .T.
  WAIT WINDOW 'Selecting records. Please stand by' NOWAIT
  SELE OrdHdr
  SET ORDER TO TAG OrdHdr
  SELECT (lcRpTmp)
  IF llOgFltCh
    IF lcSele = 'E'
      USE
      SELECT ORDLINE
      IF EMPTY(SET('RELATION'))
        SET RELATION TO 'O'+ORDER INTO OrdHdr
        SET RELATION TO STYLE  INTO STYLE ADDITIVE
      ENDIF  
      GO TOP
      SET TALK ON
      LOCATE FOR &lcFilter
      COPY REST TO (gcWorkDir+lcRpTmp) FOR &lcFilter
      SET TALK OFF
      SELECT ORDLINE
      SET RELATION TO
      =gfOpenFile(gcWorkDir+lcRpTmp,'','EX')
      IF EMPTY(SET('RELATION'))
        SET RELATION TO 'O'+ORDER INTO OrdHdr
        SET RELATION TO STYLE  INTO STYLE ADDITIVE
      ENDIF  
    ELSE
      IF EMPTY(SET('RELATION')) 
        SET RELATION TO 'O'+ORDER INTO OrdHdr
        SET RELATION TO STYLE  INTO STYLE ADDITIVE
      ENDIF  
      SET FILTER TO &lcFilter
    ENDIF
  ENDIF  
  SELECT (lcRpTmp)
  GOTO TOP
  IF EOF()
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG')
      SET DEVICE TO SCREE
    RETURN
  ENDIF
  *------ SET UP RELATIONS --------*
  SELECT (lcRpTmp)
  IF lcSortCode ='S' .AND. !llSummarize
    IF EMPTY(SET('RELATION'))
      SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
    ENDIF  
  ELSE
    IF EMPTY(SET('RELATION'))
      SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER  ADDITIVE
    ENDIF  
  ENDIF
  GO TOP
  *-----------------------------------------------------------
  * [BREAK] SET UP SORT FILEDS & CONTROL BREAKS
  *-----------------------------------------------------------
  lcSort = ' '
  DO CASE
    * ACCOUNT NUMBER SEQUENCE
    CASE lcSortCode='A'
      lcSort = 'ACCOUNT+ORDER+STYLE+STR(LINENO,6)'
      BREAK  = 'CUSTOMER.ACCOUNT + SPACE(3) + CUSTOMER.BTNAME + LFCR + CUSTOMER.PHONE1'
      CTLSEQ = 'ACCOUNT NUMBER'
    * ORDER NUMBER SEQUENCE
    CASE lcSortCode='O'
      lcSort  = 'ORDER+STYLE+STR(LINENO,6)'
      BREAK  = 'ORDER + SPACE(3) + ACCOUNT' && ACCOUNT IS INCLUDED HERE
                                            && ONLY TO DISPLAY ON SUBTOTAL.
                                            && IT IS NEEDED TO BREAK
      CTLSEQ = 'ORDER'
    * FABRIC GROUP
    CASE lcSortCode='F'
      lcSort  = 'STYLE.FABRIC+STYLE+ORDER+STR(LINENO,6)'
      BREAK  = 'STYLE.FABRIC'
      CTLSEQ = 'PRIMARY FABRIC'
    * STYLE/COLOR
    CASE lcSortCode = 'S'
      lcSort    = 'STYLE+DTOS(OrdHdr.COMPLETE)+ORDER+STR(LINENO,6)'
      BREAK     = ' '
      CTLSEQ    = 'STYLE'
    CASE lcSortCode = 'D'
      lcSort  = 'DTOS(OrdHdr.COMPLETE)+STYLE+ORDER+STR(LINENO,6)'
      BREAK  = 'DTOC(OrdHdr.COMPLETE)'
      CTLSEQ = 'COMPLETION DATE'
    CASE lcSortCode = 'R'    
      lcSort  = 'OrdHdr.REP1+STYLE+ORDER+STR(LINENO,6)'
      BREAK  = 'OrdHdr.REP1'
      CTLSEQ = 'SALESREP'
  ENDCASE
  *----------------------------------------------
  * SORT TEMPORARY FILE
  *----------------------------------------------
  SELECT (lcRpTmp)
  Z = LTRIM(STR(RECCOUNT(),7))
  WAIT WINDOW 'Sorting &Z selected records for report ...' NOWAIT
  INDEX ON &lcSort TAG (lcRpTmp)
  SET ORDER TO TAG (lcRpTmp)
  
  * PRINT REPORT
  SELECT (lcRpTmp)
  GOTO TOP
  lnAlias = SELECT()
  IF BREAK <> ' '
    HBREAK = &BREAK
  ENDIF
  lcStyle   = SUBSTR(Style,1,lnMajLen)
  CTLCOLOR  = STYLE
  CTLOR     = ACCOUNT+ORDER+IIF(OrdHdr.MULTIPO,'*MULTI-PO*',OrdHdr.CUSTPO)
  lcOrder   = ORDER
  lcStyDesc = STYLE.DESC
  lcStyFab  = STYLE.FABRIC
  lcScale   = STYLE.SCALE
  ldCompDate = &lcRpTmp..Complete
  NEWSTYLE   = .T.
  XSUM_STORE = .F.      &&   Flag to multi order style sort.
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
  R_TITLE = " OPEN ORDER DETAIL REPORT "
  WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
  STORE 0 TO lnAllAdd1,lnAllAdd2,lnAllAdd3,lnAllAdd4
  STORE 0 TO lnFinTot, lnFinAmnt
  SET DEVICE TO PRINT
  DO WHILE INKEY() <>32
    IF ROW >=54
      PAGENO = PAGENO +1
      DO RPT_HDR WITH 'SOXES600','(SUB TOTALS BY: ' + CTLSEQ +') '+ xTitle,R_WIDTH
      IF lcSortCode = 'S'
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  CUSTOMER              (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
      ELSE
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  STYLE        COLOR    (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
      ENDIF
      @ 06,000 SAY REPLICATE('-',132)
      ROW = 7
    ENDIF
    SELECT (lcRpTmp)
    LEVEL = 0
    IF SUBSTR(&lcRpTmp..STYLE,1,lnMajLen) <> lcStyle
      LEVEL = 1
    ENDIF
    DOCLR = IIF(lcSortCode='S' .AND. STYLE<>CTLCOLOR, .T., .F.)
    DOORD = IIF(lcSortCode='A' .AND. ;
                ACCOUNT+ORDER+IIF(OrdHdr.MULTIPO,'*MULTI-PO*',OrdHdr.CUSTPO)<>CTLOR, .T., .F.)
    IF !EMPTY(BREAK)
      IF HBREAK <> &BREAK
        LEVEL = 2
      ENDIF
    ENDIF
    *----------------------------------------------------
    * [DOCLR] STYLE/CLR SUB TOTALS
    *----------------------------------------------------
    IF DOCLR
      IF (lcRpForm = 'S' .AND. lcRpPrnStk= 'N')
        @ ROW,055 SAY CLRTOT(1,1)  PICTURE '@Z 99999'
        @ ROW,061 SAY CLRTOT(1,2)  PICTURE '@Z 99999'
        @ ROW,067 SAY CLRTOT(1,3)  PICTURE '@Z 99999'
        @ ROW,073 SAY CLRTOT(1,4)  PICTURE '@Z 99999'
        @ ROW,079 SAY CLRTOT(1,5)  PICTURE '@Z 99999'
        @ ROW,085 SAY CLRTOT(1,6)  PICTURE '@Z 99999'
        @ ROW,091 SAY CLRTOT(1,7)  PICTURE '@Z 99999'
        @ ROW,097 SAY CLRTOT(1,8)  PICTURE '@Z 99999'
        @ ROW,103 SAY CLRTOT(1,9)  PICTURE '9999999'
        @ ROW,119 SAY CLRTOT(1,10)  PICTURE '$9999999.99'
      ELSE
        ROW = ROW+1
        @ ROW,030 SAY '***'
        @ ROW,034 SAY LEFT(CTLCOLOR,12)
        @ ROW,047 SAY RIGHT(CTLCOLOR,6)
        @ ROW,055 SAY CLRTOT(1,1)  PICTURE '@Z 99999'
        @ ROW,061 SAY CLRTOT(1,2)  PICTURE '@Z 99999'
        @ ROW,067 SAY CLRTOT(1,3)  PICTURE '@Z 99999'
        @ ROW,073 SAY CLRTOT(1,4)  PICTURE '@Z 99999'
        @ ROW,079 SAY CLRTOT(1,5)  PICTURE '@Z 99999'
        @ ROW,085 SAY CLRTOT(1,6)  PICTURE '@Z 99999'
        @ ROW,091 SAY CLRTOT(1,7)  PICTURE '@Z 99999'
        @ ROW,097 SAY CLRTOT(1,8)  PICTURE '@Z 99999'
        @ ROW,103 SAY CLRTOT(1,9)  PICTURE '9999999'
        @ ROW,119 SAY CLRTOT(1,10)  PICTURE '$9999999.99'
      ENDIF
      ROW = ROW+2
      CTLCOLOR = STYLE
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
      @ ROW,031 SAY SUBSTR(CTLOR,12)
      @ ROW,055 SAY ORDTOT(1,1)  PICTURE '@Z 99999'
      @ ROW,061 SAY ORDTOT(1,2)  PICTURE '@Z 99999'
      @ ROW,067 SAY ORDTOT(1,3)  PICTURE '@Z 99999'
      @ ROW,073 SAY ORDTOT(1,4)  PICTURE '@Z 99999'
      @ ROW,079 SAY ORDTOT(1,5)  PICTURE '@Z 99999'
      @ ROW,085 SAY ORDTOT(1,6)  PICTURE '@Z 99999'
      @ ROW,091 SAY ORDTOT(1,7)  PICTURE '@Z 99999'
      @ ROW,097 SAY ORDTOT(1,8)  PICTURE '@Z 99999'
      @ ROW,103 SAY ORDTOT(1,9)  PICTURE '9999999'
      @ ROW,119 SAY ORDTOT(1,10)  PICTURE '$9999999.99'
      ROW = ROW+2
      CTLOR = ACCOUNT+ORDER+IIF(OrdHdr.MULTIPO,'*MULTI-PO*',OrdHdr.CUSTPO)
      ORDTOT = 0.00
      IF lcRpNotPd = 'Y'
        XPRV_ROW=ROW
        XREPORT='SOXES600'
        =lfPrtNtPad()
        ROW=IIF(ROW=XPRV_ROW,ROW,ROW+1)   && INCREMNET ROW ONLY IF NOTEPAD PRINTED
        lcOrder=ORDER
      ENDIF
    ENDIF
    *----------------------------------------------------
    * [LEVEL 1] STYLE SUB TOTALS
    *----------------------------------------------------
    IF LEVEL >=1
      NEWSTYLE = .F.
      * PRINT STYLE SUBTOTAL ONLY FOR REPORTS S,F
      IF lcSortCode $ 'SF'
        @ ROW,000 SAY REPLICATE('-',132)
        ROW = ROW+1
        @ ROW,000 SAY SUBSTR(lcSTYDESC,1,12)
        *-- change the printing of the lcStyFab with the + sign
        *-- so if the lcStyFab has ' in it wont give error
        @ ROW,013 SAY '('+lcStyFab+')'          
        @ ROW,026 SAY lcStyle
        @ ROW,039 SAY '******'
        @ ROW,055 SAY XTOTAL(1,1)  PICTURE '@Z 99999'
        @ ROW,061 SAY XTOTAL(1,2)  PICTURE '@Z 99999'
        @ ROW,067 SAY XTOTAL(1,3)  PICTURE '@Z 99999'
        @ ROW,073 SAY XTOTAL(1,4)  PICTURE '@Z 99999'
        @ ROW,079 SAY XTOTAL(1,5)  PICTURE '@Z 99999'
        @ ROW,085 SAY XTOTAL(1,6)  PICTURE '@Z 99999'
        @ ROW,091 SAY XTOTAL(1,7)  PICTURE '@Z 99999'
        @ ROW,097 SAY XTOTAL(1,8)  PICTURE '@Z 99999'
        @ ROW,103 SAY XTOTAL(1,9)  PICTURE '9999999'
        @ ROW,119 SAY XTOTAL(1,10)  PICTURE '$9999999.99'
        ROW = ROW+1
        @ ROW,000 SAY REPLICATE('-',132)
        ROW = ROW+2
        lcStyle   = SUBSTR(Style,1,lnMajLen)
        STYDESC   = STYLE.DESC
        lcStyFab  = STYLE.FABRIC
        lcScale   = STYLE.SCALE
        NEWSTYLE  = .T.
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
      IF lcSortCode='O'
        @ ROW,000 SAY SUBSTR(HBREAK,10,5)
        @ ROW,006 SAY SUBSTR(HBREAK,1,6)
      ELSE
        IF lcSortCode='R'
          SELE SALESREP
          =SEEK(HBREAK)
          XNAME=NAME
          SELECT (lcRpTmp)
        ELSE
          XNAME=''
        ENDIF
        @ ROW,000 SAY HBREAK+' '+XNAME
      ENDIF
      IF LFCR $ HBREAK
        ROW = ROW+1
      ENDIF
      @ ROW,055 SAY XTOTAL(2,1)  PICTURE '@Z 99999'
      @ ROW,061 SAY XTOTAL(2,2)  PICTURE '@Z 99999'
      @ ROW,067 SAY XTOTAL(2,3)  PICTURE '@Z 99999'
      @ ROW,073 SAY XTOTAL(2,4)  PICTURE '@Z 99999'
      @ ROW,079 SAY XTOTAL(2,5)  PICTURE '@Z 99999'
      @ ROW,085 SAY XTOTAL(2,6)  PICTURE '@Z 99999'
      @ ROW,091 SAY XTOTAL(2,7)  PICTURE '@Z 99999'
      @ ROW,097 SAY XTOTAL(2,8)  PICTURE '@Z 99999'
      @ ROW,103 SAY XTOTAL(2,9)  PICTURE '9999999'
      @ ROW,119 SAY XTOTAL(2,10)  PICTURE '$9999999.99'
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
    IF EOF(lnAlias)
      EXIT
    ENDIF
    *------------------------------------------------
    * PROCESS DETAIL RECORD
    *------------------------------------------------
    IF NEWSTYLE
      IF ROW >=54
        PAGENO = PAGENO +1
        DO RPT_HDR WITH 'SOXES600','(SUB TOTALS BY: ' + CTLSEQ +') '+ XTITLE,R_WIDTH
        IF lcSortCode = 'S'
          @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  CUSTOMER              (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
        ELSE
          @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  STYLE        COLOR    (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
        ENDIF
        @ 06,000 SAY REPLICATE('-',132)
        ROW = 7
      ENDIF
      NEWSTYLE = .F.
      SELECT SCALE
      =SEEK('S'+lcScale)
      *-- start a new page to print size descriptions in top of page.
      IF ROW >=53
        PAGENO = PAGENO +1
        DO RPT_HDR WITH 'SOXES600','(SUB TOTALS BY: ' + CTLSEQ +') '+ XTITLE,R_WIDTH
        IF lcSortCode = 'S'
          @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  CUSTOMER              (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
        ELSE
          @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  STYLE        COLOR    (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
        ENDIF
        @ 06,000 SAY REPLICATE('-',132)
        ROW = 7
      ENDIF
      @ ROW,055 SAY SZ1
      @ ROW,061 SAY SZ2
      @ ROW,067 SAY SZ3
      @ ROW,073 SAY SZ4
      @ ROW,079 SAY SZ5
      @ ROW,085 SAY SZ6
      @ ROW,091 SAY SZ7
      @ ROW,097 SAY SZ8
      ROW=ROW+1
    ENDIF
    SELECT (lcRpTmp)
    IF lcSortCode='S' .AND. XPRV_KEY <> STYLE
      XPRV_KEY=STYLE
      SELECT STYLE
      IF SEEK(XPRV_KEY)
        *--  start a new page to get style and its quantities in one page.
        IF ROW >=54
          PAGENO = PAGENO +1
          DO RPT_HDR WITH 'SOXES600','(SUB TOTALS BY: ' + CTLSEQ +') '+ XTITLE,R_WIDTH
          IF lcSortCode = 'S'
            @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  CUSTOMER              (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
          ELSE
            @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  STYLE        COLOR    (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
          ENDIF
          @ 06,000 SAY REPLICATE('-',132)
          ROW = 7
        ENDIF
        lcColorDesc = gfCodDes(SUBSTR(STYLE,lnMajLen+2) , 'COLOR')
        @ ROW,000 SAY SUBSTR(Style,1,lnMajLen)
        @ ROW,013 SAY SUBSTR(STYLE,lnMajLen+2)
        @ ROW,020 SAY SUBSTR(lcColorDesc,1,10)
        DO CASE
          CASE lcRpPrnStk = 'S'
            @ ROW,032 SAY 'STOCK :'
            @ ROW,055 SAY STK1  PICTURE '@Z 99999'
            @ ROW,061 SAY STK2  PICTURE '@Z 99999'
            @ ROW,067 SAY STK3  PICTURE '@Z 99999'
            @ ROW,073 SAY STK4  PICTURE '@Z 99999'
            @ ROW,079 SAY STK5  PICTURE '@Z 99999'
            @ ROW,085 SAY STK6  PICTURE '@Z 99999'
            @ ROW,091 SAY STK7  PICTURE '@Z 99999'
            @ ROW,097 SAY STK8  PICTURE '@Z 99999'
            @ ROW,103 SAY TOTSTK  PICTURE '9999999'
          CASE lcRpPrnStk = 'W'            &&TAK06/20/94
            @ ROW,032 SAY 'WIP + STOCK :'
            @ ROW,055 SAY WIP1+STK1  PICTURE '@Z 999999'
            @ ROW,061 SAY WIP2+STK2  PICTURE '@Z 999999'
            @ ROW,067 SAY WIP3+STK3  PICTURE '@Z 999999'
            @ ROW,073 SAY WIP4+STK4  PICTURE '@Z 999999'
            @ ROW,079 SAY WIP5+STK5  PICTURE '@Z 999999'
            @ ROW,085 SAY WIP6+STK6  PICTURE '@Z 999999'
            @ ROW,091 SAY WIP7+STK7  PICTURE '@Z 999999'
            @ ROW,097 SAY WIP8+STK8  PICTURE '@Z 999999'
            @ ROW,103 SAY TOTWIP+TOTSTK  PICTURE '9999999'
          CASE lcRpPrnStk = 'I'             && TMI 11/21/94
            @ ROW,032 SAY 'WIP :'
            @ ROW,055 SAY WIP1  PICTURE '@Z 99999'
            @ ROW,061 SAY WIP2  PICTURE '@Z 99999'
            @ ROW,067 SAY WIP3  PICTURE '@Z 99999'
            @ ROW,073 SAY WIP4  PICTURE '@Z 99999'
            @ ROW,079 SAY WIP5  PICTURE '@Z 99999'
            @ ROW,085 SAY WIP6  PICTURE '@Z 99999'
            @ ROW,091 SAY WIP7  PICTURE '@Z 99999'
            @ ROW,097 SAY WIP8  PICTURE '@Z 99999'
            @ ROW,103 SAY TOTWIP  PICTURE '9999999'
        ENDCASE
        IF .NOT. (lcRpForm = 'S' .AND. lcRpPrnStk='N')
          ROW = ROW+1
        ENDIF
      ENDIF
      SELECT (lcRpTmp)
    ENDIF
    *********************************************************************
    ****************** PRINT THE LINE ITEM DETAIL ***********************
    *********************************************************************
    SELECT (lcRpTmp)
    IF lcRpForm = 'D'
      @ ROW,000 SAY ACCOUNT
      @ ROW,006 SAY ORDER
      @ ROW,013 SAY ORDHDR.START
      @ ROW,022 SAY ORDHDR.COMPLETE
      @ ROW,031 SAY ORDHDR.STATUS
      @ ROW,032 SAY trim(ORDHDR.PRIORITY)
      IF lcSortCode = 'S'
        @ ROW,035 SAY SUBSTR(CUSTOMER.STNAME,1,10)
      ELSE
        @ ROW,035 SAY SUBSTR(Style,1,lnMajLen)
        @ ROW,048 SAY SUBSTR(STYLE,lnMajLen+2)
      ENDIF
      *--Check condition of printing
      XSUM_STORE=(llSummarize .AND. ORDHDR.MULTI='Y')
      IF XSUM_STORE
        STORE 0 TO XQTY1,XQTY2,XQTY3,XQTY4,XQTY5,XQTY6,XQTY7,XQTY8,;
                   XTOTQTY,XAMT
        lcStyle = Style
        XCOMDT  = DTOS(ORDHDR.COMPLETE)
        XORDER=ORDER
        SCAN WHILE STYLE+DTOS(ORDHDR.COMPLETE)+ORDER = lcStyle+XCOMDT+XORDER
          FOR XMCOUNT = 1 TO 8
            Z=STR(XMCOUNT,1)
            XQTY&Z=XQTY&Z+QTY&Z
          ENDFOR
          XTOTQTY=XTOTQTY+TOTQTY
          XAMT   =XAMT   +(TOTQTY*PRICE)
        ENDSCAN
        XTOTQTY=XQTY1+XQTY2+XQTY3+XQTY4+XQTY5+XQTY6+XQTY7+XQTY8
        @ ROW,055 SAY XQTY1    PICTURE '@Z 99999'
        @ ROW,061 SAY XQTY2    PICTURE '@Z 99999'
        @ ROW,067 SAY XQTY3    PICTURE '@Z 99999'
        @ ROW,073 SAY XQTY4    PICTURE '@Z 99999'
        @ ROW,079 SAY XQTY5    PICTURE '@Z 99999'
        @ ROW,085 SAY XQTY6    PICTURE '@Z 99999'
        @ ROW,091 SAY XQTY7    PICTURE '@Z 99999'
        @ ROW,097 SAY XQTY8    PICTURE '@Z 99999'
        @ ROW,103 SAY XTOTQTY  PICTURE '9999999'
      ELSE
        @ ROW,055 SAY QTY1    PICTURE '@Z 99999'
        @ ROW,061 SAY QTY2    PICTURE '@Z 99999'
        @ ROW,067 SAY QTY3    PICTURE '@Z 99999'
        @ ROW,073 SAY QTY4    PICTURE '@Z 99999'
        @ ROW,079 SAY QTY5    PICTURE '@Z 99999'
        @ ROW,085 SAY QTY6    PICTURE '@Z 99999'
        @ ROW,091 SAY QTY7    PICTURE '@Z 99999'
        @ ROW,097 SAY QTY8    PICTURE '@Z 99999'
        @ ROW,103 SAY TOTQTY  PICTURE '9999999'
        @ ROW,112 SAY PRICE   PICTURE '9999.99'
        @ ROW,120 SAY PIKTKT
        IF .NOT. EMPTY(PIKTKT) .AND. ((TOTQTY - TOTPIK)>0)
          @ ROW,127 SAY TOTQTY - TOTPIK PICTURE '99999'
        ENDIF
      ENDIF
      ROW = ROW+1
    ENDIF
    SELECT (lcRpTmp)
    IF llNotes .AND. lcRpLinNot  = 'Y' .AND. lcRpForm = 'D' .AND. !llSummarize
      IF !EMPTY(NOTE_MEM)
        ROW = ROW + 1
        @ ROW , 00 SAY 'ORDER NOTES  :'
        FOR X = 1 TO MEMLINES(NOTE_MEM)
          @ ROW,15 SAY MLINE(NOTE_MEM,X)
          ROW = ROW + 1
          IF ROW >=53 
            PAGENO = PAGENO +1
            DO RPT_HDR WITH 'SOXES600','(SUB TOTALS BY: ' + CTLSEQ +') '+ XTITLE,R_WIDTH
            IF lcSortCode = 'S'
              @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  CUSTOMER              (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
            ELSE
              @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  STYLE        COLOR    (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
            ENDIF
            @ 06,000 SAY REPLICATE('-',132)
            ROW = 7
          ENDIF
        ENDFOR
      ENDIF
    ENDIF
    **********************************************************************
    ********************* END PRINT THE LINE DETAIL **********************
    **********************************************************************

    *** Added by TAK 04/18/94 sum qty under condition XSUM_STORE
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

    IF lcSortCode = 'S'
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
    IF lcSortCode = 'A'
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
    SELECT (lcRpTmp)
    IF .NOT. XSUM_STORE   &&-- Put SKIP under condition
      SKIP              && All cases except summurize case due to SCAN.
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
  @ ROW,055 SAY XTOTAL(3,1)  PICTURE '@Z 99999'
  @ ROW,061 SAY XTOTAL(3,2)  PICTURE '@Z 99999'
  @ ROW,067 SAY XTOTAL(3,3)  PICTURE '@Z 99999'
  @ ROW,073 SAY XTOTAL(3,4)  PICTURE '@Z 99999'
  @ ROW,079 SAY XTOTAL(3,5)  PICTURE '@Z 99999'
  @ ROW,085 SAY XTOTAL(3,6)  PICTURE '@Z 99999'
  @ ROW,091 SAY XTOTAL(3,7)  PICTURE '@Z 99999'
  @ ROW,097 SAY XTOTAL(3,8)  PICTURE '@Z 99999'
  @ ROW,103 SAY XTOTAL(3,9)  PICTURE '9999999'
  @ ROW,119 SAY XTOTAL(3,10)  PICTURE '$9999999.99'
  ROW = ROW+1
  @ ROW,000 SAY REPLICATE('=',132)
  DO ENDREPORT
  EXIT
ENDDO
SET DEVICE TO SCREEN
RETURN
*---------------------------
*   END SOXES600.PRG
*---------------------------

*!*************************************************************
*! Name      : lfvOrder  (C#101404)
*! Auth      : Adel Mohammed El Gazzar.
*! DATE      : 12/01/2000
*! Purpose   : Validation function for the Order field
*!*************************************************************
*! Called from : Order field [Option Grid]
*!*************************************************************
FUNCTION lfvOrder
PRIVATE lcVar , lcObj , laTemp,lcAlias

lcAlias = ALIAS()
lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
*--IF Statment to check if we are going to Browse
SELECT ORDHDR
SET ORDER TO TAG ORDHDR
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK ('O'+lcObj))
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value
  lcBrFields = "ORDER     :R :H= 'ORDER#' , "    +;
               "ACCOUNT   :R :H= 'Account' ,"    +;
               "STORE     :R :H= 'Store' ,"      +;
               "ENTERED   :R :H= 'Entered Date',"+;
               "SEASON    :R :H= 'Season' ,"     +;
               "cDIVISION :R :H= 'Division' ,"   +;
               "Terms=gfCodDes(CTERMCODE , 'CTERMCODE') :R :H= 'Terms' ,"  +;
               "ShipV=gfCodDes(ShipVia , 'SHIPVIA')   :R :H= 'ShipVia' ,"  +;
               "STATUS    :R :H= 'Status ' ,"    +; 
               "OPEN      :R :H= 'Open Amt. ',"  +; 
               "BULK      :R :H= 'Bulk' "
  lcFile_Ttl = "Orders..."
  lcBrowCond = [FOR STATUS != "X" AND OPEN >= 1 ]
  = gfBrows(lcBrowCond,'ORDER','laTemp')
  *--IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = ''
  ENDIF
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfsrvSty   (C#101404)
*! Auth      : Adel Mohammed El Gazzar.
*! DATE      : 12/01/2000
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
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum   (C#101404)
*! Auth      : Adel Mohammed El Gazzar.
*! DATE      : 12/01/2000
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
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)

*!*************************************************************
*! Name      : lfEvalSegs    (C#101404)
*! Auth      : Adel Mohammed El Gazzar.
*! DATE      : 12/01/2000
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
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
  
*-- if you does not find Non Major Type Color Code.
IF !lfNMajType('C',lnMajSeg)  
  = lfNMajType('F',lnMajSeg)  && Check for Non Major Type Free code.
ENDIF  && end if you does not find Non Major Type Color Code.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
lcRpDrop = 'Dropped ' +ALLTRIM(lcNonMajTlt) + 's only.'
lcRpStyS = ALLTRIM(lcStyMajor) + 's status'
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfvSelcBy  (C#101404)
*! Auth      : Adel Mohammed El Gazzar.
*! DATE      : 12/01/2000
*! Purpose   : Validate select by option in option grid.
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
*! Note      : 
*!           : 
*!           : 
*!*************************************************************
*! Example   : =lfvSelcBy()
*!*************************************************************
FUNCTION lfvSelcBy

STORE .T. TO llChSelect,llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep
CLEAR READ

*!*************************************************************
*! Name      : lfNMajType   (C#101404)
*! Auth      : Adel Mohammed El Gazzar.
*! DATE      : 12/01/2000
*! Purpose   : Mask NonMajor segments .
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
*! Name      : lfsrAcc
*! Auth      : Adel Mohammed El Gazzar.
*! DATE      : 12/01/2000
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
*! Name      : lfvFabric  (C#101404)
*! Auth      : Adel Mohammed El Gazzar.
*! DATE      : 12/01/2000
*! Purpose   : validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
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

*!*************************************************************
*! Name      : lfvSortBy  (C#101404)
*! Auth      : Adel Mohammed El Gazzar.
*! DATE      : 12/01/2000
*! Purpose   : Validate Sort by option in option grid.
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
*! Note      : 
*!           : 
*!           : 
*!*************************************************************
*! Example   : =lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy
llRpSupr = (lcRpSort <> 'A' OR lcRpForm <>'D')
CLEAR READ

FUNCTION lfvSumm

CLEAR READ


*!*************************************************************
*! Name      : lfUpdFltVar
*! Auth      : Adel Mohammed El Gazzar.
*! DATE      : 12/01/2000
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfUpdFltVar()
*!*************************************************************
FUNCTION lfUpdFltVar

*lcNotes     = lcRpLinNot 
lcSele      = lcRpSelcBy
lcNote      = (lcRpNotPd='Y')
lcSortCode  = lcRpSort
llSummarize = (lcRpSumm = 'Y')
xTitle      = lcRpOpTitl
*XCOORGRP    = lcRpCorGrp

FOR lnInd  = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
  
*--- Add Fabric# to the filter
*    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.FABRIC' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.FABRIC'
      lcFabric = laOgFxFlt[lnInd,6]
      IF lcRpSelcBy = 'F' AND EMPTY(lcFabric)
        llFabRange = .F.
        RETURN .F.
      ENDIF

*--- Add Style Temp File to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.STYLE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      IF !USED(laOgFxFlt[lnInd,6]))
        llStyRange = .F.
        RETURN .F.
      ENDIF
      lcStyAls = laOgFxFlt[lnInd,6]
      
*--- Add Order# to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.ORDER' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      LOR = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,6))
      HOR = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],8,20))
      
*--- Add Season list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.SEASON' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      TS = laOgFxFlt[lnInd,6]
      
*--- Add Order Status list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.STATUS' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      ZST = laOgFxFlt[lnInd,6]

*--- Add Division list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.CDIVISION' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      TD = laOgFxFlt[lnInd,6]
      
*--- Add Prioroty list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.PRIORITY' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lnAllPer = laOgFxFlt[lnInd,6]
      
*--- Add Color list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      TCLR = laOgFxFlt[lnInd,6]

*--- Add GROUP list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CSTYGROUP' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      XGR = laOgFxFlt[lnInd,6]

*--- Add GROUP list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDLINE.COMPLETE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      LCO = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      HCO = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
*--- Add GROUP list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDLINE.START' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      LST = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      HS  = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))

*--- Add GROUP list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.ACCOUNT' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      IF !USED(laOgFxFlt[lnInd,6]))
        llAccRange = .F.
        RETURN .F.
      ENDIF
      lcAccAls = laOgFxFlt[lnInd,6]
  ENDCASE
ENDFOR
lcFilter = 'TOTQTY>0'
IF !EMPTY(LOR)
  lcFilter = lcFilter + ".AND.BETWEEN(ORDER,LOR,HOR)"
ENDIF
IF !EMPTY(ZST)
  lcFilter = lcFilter + '.AND.OrdHdr.STATUS $ ZST'
ENDIF
IF !EMPTY(XGR)
  lcFilter = lcFilter + '.AND.STYLE.cStyGroup = XGR'
ENDIF
IF !EMPTY( TS )
  lcFilter = lcFilter + ".AND. ORDLINE.SEASON $ TS "
ENDIF
IF !EMPTY( TD )
  lcFilter = lcFilter + ".AND. OrdHdr.cDIVISION $ TD "
ENDIF
IF LCO <> NULDATE
  lcFilter = lcFilter + '.AND.BETWEEN(OrdHdr.COMPLETE,LCO,HCO)'
ENDIF
IF LST <> NULDATE
  lcFilter = lcFilter + '.AND.BETWEEN(OrdHdr.START,LST,HS)'
ENDIF
IF !EMPTY(TCLR)
  lcFilter = lcFilter + '.AND.SUBSTR(Style,14,6) $ TCLR'
ENDIF
IF lcRpCorGrp = 'Y'
  lcFilter  = lcFilter + ".AND. !EMPTY(GROUP)"
ENDIF
IF !EMPTY(lnAllPer)
  lcFilter = lcFilter + '.AND. OrdHdr.PRIORITY $ lnAllPer'
ENDIF
IF lcRpStySt <> 'L'
  lcFilter = lcFilter + '.AND. Style.Status = lcRpStySt'
ENDIF
IF llRpDropCl
  lcFilter = lcFilter + ' .AND. SUBSTR(STYLE.PATTERN,1,1) = "D"'
ENDIF

*!*************************************************************
*! Name      : lfPrtNtPad
*! Auth      : Adel Mohammed El Gazzar.
*! DATE      : 12/01/2000
*! Purpose   : TO print the Order note pad.
*!*************************************************************
FUNCTION lfPrtNtPad
PRIVATE lcAlias

lcAlias = ALIAS()
IF SEEK('B'+lcOrder,'NotePad')
  SELECT NotePad
  =SEEK('B'+lcOrder)
  lnMline = 1
  *---------------------------------------------------
  * [NOTEPAD] COMMENT LINES
  *---------------------------------------------------
  lnMemWidth = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  lnMTotLin = MEMLINES(NotePad.MNOTES)
  @ ROW,00 SAY REPLICATE('|',80 )
  ROW = ROW + 1
  DO WHILE .T.
    IF lnMline > lnMTotLin
      EXIT
    ENDIF
    IF ROW >=53
      PAGENO = PAGENO +1
      DO RPT_HDR WITH 'SOXES600','(SUB TOTALS BY: ' + CTLSEQ +') '+ XTITLE,R_WIDTH
      IF lcSortCode = 'S'
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  CUSTOMER              (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
      ELSE
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  STYLE        COLOR    (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
      ENDIF
      @ 06,000 SAY REPLICATE('-',132)
      ROW = 7
    ENDIF
    IF  '*' <> LEFT(MLINE(MNOTES,lnMline),1)  
      @ ROW,00 SAY '|'+MLINE(MNOTES,lnMline)
      ROW = ROW+1
    ENDIF
    lnMline = lnMline + 1
  ENDDO
  @ ROW,00 SAY REPLICATE('|',80 )
  ROW = ROW +1
  SET MEMOWIDTH TO lnMemWidth
ENDIF  
SELECT (lcAlias)
