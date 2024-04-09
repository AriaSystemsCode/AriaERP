*:***************************************************************************
*: Program file  : SONAP900   C102337
*: Program desc. : Custom Order Detail For NAP
*: For Report    : 
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Ahmed Mohamed_El_Anwar  (AME)
*: Date          : 05/31/2001
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : This program was NAP900 In ARIA 26 with C10117
*:***************************************************************************
*: Example : DO SONAP900
*:***************************************************************************
*

SELECT ORDLINE
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE

llNotes  = (gfGetMemVar('M_OR_NOTE') = 'Y')
LFCR = chr(10)+chr(13)

IF llFrTime   &&first time to run report
  DIMENSION laTempStru[1,4]
  laTempStru = ''
  lcTempLine = gfTempName()
  SELECT ORDLINE
  = AFIELDS(laTempStru)
  llFrTime = .F.  && After this time all of your variablrs have been defined, you not need to goto any llFrTime block again.  
ENDIF

IF EMPTY(lcTempLine) OR !USED(lcTempLine)
  DIMENSION laTempStru[1,4]
  laTempStru = ''
  SELECT ORDLINE
  = AFIELDS(laTempStru)
  CREATE TABLE (gcWorkDir+lcTempLine) ;
  FROM ARRAY laTempStru
ENDIF


IF llClearFn OR llOGFltCh
  llClearFn = .F.
  IF ('MA' $ gcCmpModules) AND (lcRpSelcBy = 'F')
    IF AT('FABRIC.FABRIC',lcRpExp) <> 0
      lcRpExp = STRTRAN(lcRpExp,'FABRIC.FABRIC','STYLE.FABRIC')
    ENDIF
  ENDIF

  IF RECCOUNT(lcTempLine) > 0
    SELECT (lcTempLine)
    ZAP
    SET RELATION TO
  ENDIF		&& end If you find any data.

  lnOrdPos   = lfItmPos('ORDHDR.ORDER')
  lcTOrdFile = laOGFxFlt[lnOrdPos,6]
  llWorkDeal = !EMPTY(lcTOrdFile) AND USED(lcTOrdFile) AND RECCOUNT(lcTOrdFile) > 0
  llSeLDeal = .F.
  DO CASE
   CASE lcRpSelcBy = 'S'
     lnSelPos   = lfItmPos('STYLE.CSTYMAJOR')
     lcTSelFile = laOGFxFlt[lnSelPos,6]
     llSeLDeal = !EMPTY(lcTSelFile) AND USED(lcTSelFile) AND RECCOUNT(lcTSelFile) > 0
   CASE lcRpSelcBy = 'A'
     lnSelPos   = lfItmPos('CUSTOMER.ACCOUNT')
     lcTSelFile = laOGFxFlt[lnSelPos,6]
     llSeLDeal = !EMPTY(lcTSelFile) AND USED(lcTSelFile) AND RECCOUNT(lcTSelFile) > 0
   CASE lcRpSelcBy = 'F'
     lnSelPos   = lfItmPos('FABRIC.FABRIC')
     lcTSelFile = laOGFxFlt[lnSelPos,6]
     llSeLDeal = !EMPTY(lcTSelFile) AND USED(lcTSelFile) AND RECCOUNT(lcTSelFile) > 0
  ENDCASE
  IF !llWorkDeal
    IF llSeLDeal
      DO CASE
       CASE lcRpSelcBy = 'S'
         SELECT ORDLINE
         SET ORDER TO ORDLINES
         SELECT &lcTSelFile
         SCAN
           SELECT ORDLINE
           *C102337,4 AME [Start] The temp. file holds the style major not the style
           *=SEEK(&lcTSelFile..STYLE)
           *SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6)  = &lcTSelFile..STYLE;
                     FOR &lcRpExp .AND. ORDHDR.STATUS $ lcRpStatus .AND.  IIF(llRpCorrGp,!EMPTY(GROUP),.T.) 
           =SEEK(LEFT(&lcTSelFile..CstyMajor,lnMajorLen))
           SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6)  = LEFT(&lcTSelFile..CstyMajor,lnMajorLen);
                     FOR &lcRpExp .AND. ORDHDR.STATUS $ lcRpStatus .AND.  IIF(llRpCorrGp,!EMPTY(GROUP),.T.) 
           *C102337,4 AME [End]
                       WAIT WINDOW 'Collecting data Order # '+ Order NOWAIT
             SCATTER MEMVAR MEMO
             INSERT INTO (lcTempLine) FROM MEMVAR
           ENDSCAN
         ENDSCAN
         SELECT ORDLINE
         SET ORDER TO ORDLINE
       CASE lcRpSelcBy = 'A'
         SELECT ORDLINE
         SET RELATION OFF INTO ORDHDR
         SELECT ORDHDR
         *SET RELATION TO 'O'+ORDER INTO ORDLINE ADDITIVE
         SET ORDER TO ORDACCT
         SELECT &lcTSelFile
         SCAN
           SELECT ORDHDR
           =SEEK(&lcTSelFile..ACCOUNT)
           *C102337,4 AME [Start] Cordtype not Cortype
           *SCAN REST WHILE ACCOUNT+CORTYPE+ORDER = &lcTSelFile..ACCOUNT
           SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = &lcTSelFile..ACCOUNT           
           *C102337,4 AME [End]
             SELECT ORDLINE
             =SEEK('O'+ORDHDR.ORDER)
             SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+ORDHDR.ORDER ;
                       FOR &lcRpExp .AND. ORDHDR.STATUS $ lcRpStatus .AND.  IIF(llRpCorrGp,!EMPTY(GROUP),.T.) 
               WAIT WINDOW 'Collecting data Order # '+ Order NOWAIT
               SCATTER MEMVAR MEMO
               INSERT INTO (lcTempLine) FROM MEMVAR
             ENDSCAN
           ENDSCAN
         ENDSCAN
         SELECT ORDHDR
         SET ORDER TO ORDHDR
         SET RELATION TO 
       CASE lcRpSelcBy = 'F'
         SELECT ORDLINE 
         SET RELATION OFF INTO STYLE
         SET ORDER TO ORDLINES
         SELECT STYLE 
         *SET RELATION TO STYLE INTO ORDLINE
         IF !FILE(gcWorkDir+lcStyIndex)
           INDEX ON Fabric TAG (lcStyIndex) OF (gcWorkDir+lcStyIndex+'.CDX')
         ENDIF
         SET ORDER TO TAG (lcStyIndex)
         SELECT &lcTSelFile
         SCAN
           SELECT STYLE
           =SEEK(&lcTSelFile..FABRIC)
           SCAN REST WHILE FABRIC = &lcTSelFile..FABRIC
             SELECT ORDLINE
             =SEEK(STYLE.STYLE)
             SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = STYLE.STYLE ;
                       FOR &lcRpExp .AND. ORDHDR.STATUS $ lcRpStatus .AND.  IIF(llRpCorrGp,!EMPTY(GROUP),.T.) 
               WAIT WINDOW 'Collecting data Order # '+ Order NOWAIT
               SCATTER MEMVAR MEMO
               INSERT INTO (lcTempLine) FROM MEMVAR
             ENDSCAN
           ENDSCAN
         ENDSCAN
         SELECT STYLE
         SET RELATION TO
         SET ORDER TO STYLE
      ENDCASE
    ELSE
      SELECT ORDLINE
      WAIT WINDOW 'Collecting data.... Please wait' NOWAIT
      SCAN FOR &lcRpExp .AND. ORDHDR.STATUS $ lcRpStatus .AND.  IIF(llRpCorrGp,!EMPTY(GROUP),.T.) 
        WAIT WINDOW 'Collecting data Order # '+ Order NOWAIT
        SCATTER MEMVAR MEMO
        INSERT INTO (lcTempLine) FROM MEMVAR
      ENDSCAN
    ENDIF
  ELSE
    SELECT (lcTOrdFile)
    SCAN
      WAIT WINDOW 'Collecting data Order # '+ Order NOWAIT            
      SELECT ORDLINE
      IF SEEK('O'+&lcTOrdFile..ORDER) AND;
         &lcRpExp  AND;
         ORDHDR.STATUS $ lcRpStatus AND; 
         IIF(llRpCorrGp,!EMPTY(GROUP),.T.)
         
        SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+&lcTOrdFile..Order
          IF &lcRpExp  AND;
             ORDHDR.STATUS $ lcRpStatus AND; 
             IIF(llRpCorrGp,!EMPTY(GROUP),.T.)
            
            
            SCATTER MEMVAR MEMO
            INSERT INTO (lcTempLine) FROM MEMVAR
          ENDIF
        ENDSCAN
      ENDIF
    ENDSCAN   
  ENDIF
ENDIF

*-----------------------------------------------------------
* [BREAK] SET UP SORT FILEDS & CONTROL BREAKS
*-----------------------------------------------------------
lcSort = SPACE(1)
lcSavSort  = SPACE(1)
DO CASE
* ACCOUNT NUMBER SEQUENCE
 CASE lcRpSortBy='A'
   lcSort  = 'ACCOUNT+ORDER+STYLE+STR(LINENO,6)'
   BREAK   = 'CUSTOMER.ACCOUNT + SPACE(3) + CUSTOMER.BTNAME + LFCR + CUSTOMER.PHONE1'
   CTLSEQ  = 'ACCOUNT NUMBER'
  
* ORDER NUMBER SEQUENCE
 CASE lcRpSortBy='O'
   lcSort = 'ORDER+STYLE+STR(LINENO,6)'
   BREAK  = 'ORDER + SPACE(3) + ACCOUNT' && ACCOUNT IS INCLUDED HERE
                                         && ONLY TO DISPLAY ON SUBTOTAL.
                                         && IT IS NEEDED TO BREAK
   CTLSEQ = 'ORDER'

* FABRIC GROUP
 CASE lcRpSortBy='F'
   lcSort = 'STYLE.FABRIC+STYLE+ORDER+STR(LINENO,6)'
   BREAK  = 'STYLE.FABRIC'
   CTLSEQ = 'PRIMARY FABRIC'

* STYLE/COLOR
 CASE lcRpSortBy = 'S'
   lcSort   = 'STYLE+DTOS(ORDHDR.COMPLETE)+ORDER+STR(LINENO,6)'
   BREAK    = ' '
   CTLSEQ   = 'STYLE/COLOR'
* Completion date
 CASE lcRpSortBy = 'D'
   lcSort = 'DTOS(ORDHDR.COMPLETE)+STYLE+ORDER+STR(LINENO,6)'
   BREAK  = 'DTOC(ORDHDR.COMPLETE)'
   CTLSEQ = 'COMPLETION DATE'
  
*Primary Sales Rep
 CASE lcRpSortBy = 'R'    
   lcSort = 'ORDHDR.REP1+STYLE+ORDER+STR(LINENO,6)'
   BREAK  = 'ORDHDR.REP1'
   CTLSEQ = 'SALESREP'
ENDCASE

SELECT &lcTempLine
IF lcSort<>SPACE(1)
  IF lcSort <> lcSavSort
    lcSavSort = lcSort
    Z = LTRIM(STR(RECCOUNT(),7))
    WAIT WINDOW 'Sorting &Z selected records for report ...' NOWAIT
    INDEX ON &lcSort TAG &lcTempLine
  ENDIF
  SET ORDER TO TAG &lcTempLine
ENDIF

SELECT ORDLINE
SET RELATION TO

SELECT (lcTempLine)
LOCATE

IF EOF()
  *----------There's No record to display------------*
  *----------            <OK>            ------------*
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

SELECT ORDHDR
SET RELATION TO REP1 INTO SALESREP ADDITIVE

SELECT (lcTempLine)
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
SET RELATION TO 'O'+ORDER+STR(LineNo,6) INTO ORDLINE ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO 'S'+SCALE INTO SCALE ADDITIVE
*C102337,4 AME [Start]
IF lcRpSortBy='S' .AND. !llRpSummMt
  SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
ELSE
  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER  ADDITIVE
ENDIF
*C102337,4 AME [End]

IF BREAK <> SPACE(1)
  HBREAK = &BREAK
ENDIF
lcCtlStyle = STYLE.CStyMajor
lcCtlColor = STYLE
*C102337,4 AME [Start] the multi po statment should be 15 chr
*CTLOR      = ACCOUNT+ORDER+IIF(ORDHDR.MULTIPO,'*MULTI-PO*',ORDHDR.CUSTPO)
CTLOR      = ACCOUNT+ORDER+IIF(ORDHDR.MULTIPO,'*MULTI-PO*     ',ORDHDR.CUSTPO)
*C102337,4 AME [End] 
lcStyle    = SPACE(19)
ldComDate  = SPACE(8)
lcOrder    = ORDER
lcStyDesc  = STYLE.DESC
lcStyFab   = STYLE.FABRIC
lcScale    = STYLE.SCALE
llNewStyle = .T.

DIMENSION        laTotal(3,2),laClrTot(2),laOrdTot(2)
STORE  0.00 TO   laTotal     ,laClrTot ,   laOrdTot  

R_WIDTH  = 'W'
SET DEVICE TO PRINT
PAGENO = 0
ROW    = 99
*C102337,4 AME [Start] Initial the xprv_key value
XPRV_KEY = SPACE(1)
*C102337,4 AME [End]
R_TITLE = " ORDER DETAIL REPORT "

WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
SCAN WHILE INKEY() <>32
  llSum_Stor=IIF(llRpSummMt .AND. ORDHDR.MULTI='Y',.T.,.F.)
  IF llSum_Stor .AND. STYLE+DTOS(ORDHDR.COMPLETE)+ORDER = lcStyle+ldComDate+lcOrder
    LOOP
  ENDIF
  IF ROW >=54
    PAGENO = PAGENO + 1

    DO RPT_HDR WITH 'SONAP900','(SUB TOTALS BY: ' + CTLSEQ +') '+ lcRpMsg1,R_WIDTH

    IF lcRpSortBy = 'S'
      @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  CUSTOMER              CUST.PO#   NAP P/O#   Onhand         TotOrd       PRICE      PIKTKT   DIFF.'
    ELSE
      @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  STYLE        COLOR    NAP P/O#              Onhand         TotOrd       PRICE      PIKTKT   DIFF.'
    ENDIF
    @ 06,000 SAY REPLICATE('-',132)
    ROW = 7
  ENDIF
  LEVEL = 0
  IF STYLE.CStyMajor <> lcCtlStyle
    LEVEL = 1
  ENDIF
  DOCLR = IIF(lcRpSortBy='S' .AND. STYLE<>lcCtlColor, .T., .F.)
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
    IF (lcRpKind = 'S' .AND. lcRpStyPrn = 'N')
      @ ROW,93 SAY laClrTot(1)  PICTURE '9999999'
      @ ROW,118 SAY laClrTot(2)  PICTURE '$9999999.99'
    ELSE
      ROW = ROW+1
      @ ROW,030 SAY '***'
      @ ROW,035 SAY LEFT(lcCtlColor,12)
      @ ROW,048 SAY RIGHT(lcCtlColor,6)
      @ ROW,93 SAY laClrTot(1)  PICTURE '9999999' 
      @ ROW,118 SAY laClrTot(2)  PICTURE '$9999999.99'
    ENDIF
    ROW = ROW+1
    lcCtlColor = STYLE
    laClrTot = 0.00
  ENDIF

  *----------------------------------------------------
  * [DOORD] ACCOUNT/ORDER SUB TOTALS
  *----------------------------------------------------
  IF DOORD
    @ ROW,000 SAY '***'
    @ ROW,006 SAY LEFT(CTLOR,5)
    @ ROW,012 SAY SUBSTR(CTLOR,6,6)
    @ ROW,019 SAY 'CUST. PO# :'
    *C102337,4 AME [start] Cust Po. in A27 is 15 chr length.
    *@ ROW,031 SAY RIGHT(CTLOR,10)
    @ ROW,031 SAY RIGHT(CTLOR,15)
    *C102337,4 AME [End]
    @ ROW,93 SAY laOrdTot(1,1)  PICTURE '9999999'
    @ ROW,118 SAY laOrdTot(1,2)  PICTURE '$9999999.99'
    ROW = ROW+2
    *C102337,4 AME [Start] the multi po statment should be 15 chr
    *CTLOR = ACCOUNT+ORDER+IIF(ORDHDR.MULTIPO,'*MULTI-PO*',ORDHDR.CUSTPO)
    CTLOR = ACCOUNT+ORDER+IIF(ORDHDR.MULTIPO,'*MULTI-PO*',ORDHDR.CUSTPO) 
    *C102337,4 AME [End]    
    laOrdTot = 0.00
    IF llRpOrdNot
      XPRV_ROW = ROW
      XREPORT='SONAP900'
      DO lfPrt_Note WITH 'B',lcOrder
      ROW = IIF(ROW=XPRV_ROW,ROW,ROW+1)   && INCREMNET ROW ONLY IF NOTEPAD PRINTED
      lcOrder=ORDER
    ENDIF
  ENDIF

  *----------------------------------------------------
  *-- [LEVEL 1] STYLE SUB TOTALS
  *----------------------------------------------------
  IF LEVEL >=1
    llNewStyle = .F.
  *-- PRINT STYLE SUBTOTAL ONLY FOR REPORTS S,F
    IF lcRpSortBy $ 'SF'
      @ ROW,000 SAY REPLICATE('-',132)
      ROW =ROW+1
      @ ROW,000 SAY SUBSTR(lcStyDesc,1,12)
      @ ROW,013 SAY '('+lcStyFab+')'
      @ ROW,026 SAY LEFT(lcCtlStyle,12)
      @ ROW,039 SAY '******'
      @ ROW,93 SAY laTotal(1,1)  PICTURE '9999999' 
      @ ROW,118 SAY laTotal(1,2)  PICTURE '$9999999.99'
      ROW = ROW+1
      @ ROW,000 SAY REPLICATE('-',132)
      ROW = ROW+2
      lcCtlStyle = STYLE.CStyMajor
      lcStyDesc  = STYLE.DESC
      lcStyFab   = STYLE.FABRIC
      lcScale    = STYLE.SCALE
      llNewStyle = .T.
    ENDIF

    FOR X = 1 TO 2
      laTotal(2,X) = laTotal(2,X) + laTotal(1,X)
      laTotal(3,X) = laTotal(3,X) + laTotal(1,X)
      laTotal(1,X) = 0.00
    ENDFOR
  ENDIF
  *----------------------------------------------------
  *-- [LEVEL 2] USER DEFINED SUB TOTALS
  *----------------------------------------------------
  IF BREAK<>SPACE(1) .AND. LEVEL >=2
    @ ROW,000 SAY REPLICATE('-',132)
    ROW = ROW + 1
    IF lcRpSortBy = 'O'
      @ ROW,000 SAY SUBSTR(HBREAK,10,5)
      @ ROW,006 SAY SUBSTR(HBREAK,1,6)
    ELSE
      IF lcRpSortBy='R'
        lcName=SalesRep.NAME
      ELSE
        lcName = SPACE(0)
      ENDIF
      @ ROW,000 SAY HBREAK+' '+lcName
    ENDIF
    IF LFCR $ HBREAK
      ROW = ROW+1
    ENDIF
    @ ROW,93 SAY laTotal(2,1)  PICTURE '9999999'
    @ ROW,118 SAY laTotal(2,2)  PICTURE '$9999999.99'
    ROW = ROW + 1
    @ROW,000 SAY REPLICATE('-',132)
    ROW = ROW + 2
    HBREAK = &BREAK
    FOR X = 1 TO 2
      laTotal(2,X) =0.00
    ENDFOR
  ENDIF
  *------------------------------------------------
  *-- PROCESS DETAIL RECORD
  *------------------------------------------------
  IF llNewStyle

    IF ROW >=54
      PAGENO = PAGENO +1

      DO RPT_HDR WITH 'SONAP900','(SUB TOTALS BY: ' + CTLSEQ +') '+ lcRpMsg1,R_WIDTH
      IF lcRpSortBy = 'S'
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  CUSTOMER              CUST.PO#   NAP P/O#   Onhand         TotOrd       PRICE      PIKTKT    DIFF.'
      ELSE
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  STYLE        COLOR    NAP P/O#              Onhand         TotOrd       PRICE      PIKTKT    DIFF.'
      ENDIF
      @ 06,000 SAY REPLICATE('-',132)
      ROW = 7
    ENDIF

    llNewStyle = .F.
    IF ROW >=53
      PAGENO = PAGENO +1
      DO RPT_HDR WITH 'SONAP900','(SUB TOTALS BY: ' + CTLSEQ +') '+ lcRpMsg1,R_WIDTH
      IF lcRpSortBy = 'S'
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  CUSTOMER              CUST.PO#   NAP P/O#   Onhand         TotOrd       PRICE      PIKTKT   DIFF.'
      ELSE
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  STYLE        COLOR    NAP P/O#              Onhand         TotOrd       PRICE      PIKTKT   DIFF.'
      ENDIF
      @ 06,000 SAY REPLICATE('-',132)
      ROW = 7
    ENDIF
  ENDIF
  
  IF lcRpSortBy='S' .AND. XPRV_KEY <> STYLE
    XPRV_KEY = STYLE
    IF SEEK(XPRV_KEY,'STYLE')
      IF ROW >=54
        PAGENO = PAGENO +1
        DO RPT_HDR WITH 'SONAP900','(SUB TOTALS BY: ' + CTLSEQ +') '+ lcRpMsg1,R_WIDTH
        IF lcRpSortBy = 'S'
          @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  CUSTOMER              CUST.PO#   NAP P/O#   Onhand         TotOrd       PRICE      PIKTKT   DIFF.'
        ELSE
          @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP  STYLE        COLOR    NAP P/O#              Onhand         TotOrd       PRICE      PIKTKT   DIFF.'
        ENDIF
        @ 06,000 SAY REPLICATE('-',132)
        ROW = 7
      ENDIF
      lcColcode = gfCodDes(SUBSTR(STYLE,lnNonMajSt,lnColorLen), 'COLOR')
      @ ROW,000 SAY STRTRAN(STYLE,SUBSTR(STYLE,lnNonMajSt-1,lnColorLen+1))  && add one the cutted str due to separator.
      @ ROW,013 SAY SUBSTR(STYLE,lnNonMajSt,lnColorLen)
      @ ROW,020 SAY SUBSTR(lcColcode,1,10)
      DO CASE
       CASE lcRpStyPrn = 'S'
         @ ROW,030 SAY 'STOCK:'
         *C102337,4 AME [Start] Stock field is in style file
         *@ ROW,043 SAY TOTSTK  PICTURE '9999999'
         @ ROW,043 SAY STYLE.TOTSTK  PICTURE '9999999'
       *CASE lcRpStyPrn = 'W'
       CASE lcRpStyPrn = 'P'
       *C102337,4 AME [End]       
         @ ROW,030 SAY 'WIP + STOCK:'
         *C102337,4 AME [Start] wip and Stock fields are in style file
         *@ ROW,043 SAY TOTWIP+TOTSTK  PICTURE '9999999'
         @ ROW,043 SAY STYLE.TOTWIP+STYLE.TOTSTK  PICTURE '9999999'
       *CASE lcRpStyPrn = 'I'
       CASE lcRpStyPrn = 'W'
       *C102337,4 AME [End]       
         @ ROW,030 SAY 'WIP:'
         *C102337,4 AME [Start] wip field is in style file
         *@ ROW,043 SAY TOTWIP  PICTURE '9999999'
         @ ROW,043 SAY STYLE.TOTWIP  PICTURE '9999999'
         *C102337,4 AME [End]
      ENDCASE
      IF .NOT. (lcRpKind = 'S' .AND. lcRpStyPrn='N')
        ROW = ROW+1
      ENDIF
    ENDIF
    IF !EOF()
      GO RECNO()
    ENDIF
  ENDIF

  *********************************************************************
  ****************** PRINT THE LINE ITEM DETAIL *****************
  *********************************************************************
  IF lcRpKind = 'D'
    @ ROW,000 SAY ACCOUNT
    @ ROW,006 SAY ORDER
    @ ROW,013 SAY ORDHDR.START
    @ ROW,022 SAY ORDHDR.COMPLETE
    @ ROW,031 SAY ORDHDR.STATUS
    @ ROW,032 SAY ORDHDR.PRIORITY && length is diff. xyz
    IF lcRpSortBy = 'S'
      @ ROW,035 SAY SUBSTR(CUSTOMER.STNAME,1,10)
    ELSE
      @ ROW,035 SAY STRTRAN(STYLE,SUBSTR(STYLE,lnNonMajSt-1,lnColorLen+1))  && add one the cutted str due to separator.
      @ ROW,048 SAY SUBSTR(STYLE,lnNonMajSt,lnColorLen)
    ENDIF
*--Check condition of printing
    llSum_Stor=IIF(llRpSummMt .AND. ORDHDR.MULTI='Y',.T.,.F.)
    IF llSum_Stor
      STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,;
                      lnTotQty,lnAmt
      lcStyle   = STYLE
      ldComDate = DTOS(ORDHDR.COMPLETE)
      lcOrder   = ORDER

      lcCustPo = IIF(!EOF('OrdHdr') .AND. OrdHdr.MultiPo ,&lcTempLine..CustPo ,ORDHDR.CustPo)
      IF !EOF('OrdLine')       
        SELECT OrdLine
        SCAN WHILE STYLE+DTOS(ORDHDR.COMPLETE)+ORDER = lcStyle+;
                   ldComDate+lcOrder
          FOR XMCOUNT = 1 TO 8
            Z=STR(XMCOUNT,1)
            lnQty&Z = lnQty&Z+QTY&Z
          ENDFOR
          lnTotQty = lnTotQty + TOTQTY
          lnAmt    = lnAmt + (TOTQTY*PRICE)
        ENDSCAN
      ENDIF     
      lnTotQty = lnQty1+lnQty2+lnQty3+lnQty4+lnQty5+lnQty6+lnQty7+lnQty8
      lnCusAlias = SELECT()
      SELECT &lcTempLine
      lcTempOrd  = Order
      lcLineNo   = LineNo

      IF SEEK ('2'+lcTempOrd+SUBSTR(STR(lcLineNo),5,6),'CutPick')
        lcNapPo = CutPick.CTktNo
      ELSE
        lcNapPo = SPACE(1)
      ENDIF
      *C102337,4 AME [Start] Take just 12 chr of custpo.
      *@ ROW,058 SAY IIF (lcRpSortBy = 'S',lcCustPo,lcNapPo)
      @ ROW,058 SAY IIF (lcRpSortBy = 'S',LEFT(lcCustPo,12),lcNapPo)
      *C102337,4 AME [End]     
      @ ROW,070 SAY IIF (lcRpSortBy = 'S',lcNapPo,SPACE(0))
      @ ROW,078 SAY STYLE.TotStk
      *C102337,4 AME [Start] shift left to align with totord
      *@ ROW,094 SAY lnTotQty  PICTURE '9999999'
      @ ROW,093 SAY lnTotQty  PICTURE '9999999'
      *C102337,4 AME [End]
    ELSE
      SELECT &lcTempLine
      lcTempOrd  = Order
      lcLineNo   = LineNo

      IF SEEK ('2'+lcTempOrd+SUBSTR(STR(lcLineNo),5,6),'CutPick')
        lcNapPo = CutPick.CTktNo
      ELSE
        lcNapPo = SPACE(1)
      ENDIF

      *lcCustPo = IIF(SEEK(lcTempOrd) .AND. MultiPo ,&lcTempLine..CustPo,ORDHDR.CustPo)
      lcCustPo = IIF(!EOF('OrdHdr') .AND. OrdHdr.MultiPo ,&lcTempLine..CustPo ,ORDHDR.CustPo)
      *C102337,4 AME [Start] Take just 12 chr of custpo
      *@ ROW,058 SAY IIF (lcRpSortBy = 'S',lcCustPo,lcNapPo)
      @ ROW,058 SAY IIF (lcRpSortBy = 'S',LEFT(lcCustPo,12),lcNapPo)
      *C102337,4 AME [End]
      @ ROW,070 SAY IIF (lcRpSortBy = 'S',lcNapPo,SPACE(0))
      @ ROW,078 SAY STYLE.TotStk
      *C102337,4 AME [Start] shift left to align with totord
      *@ ROW,094 SAY TOTQTY
      @ ROW,093 SAY TOTQTY
      *C102337,4 AME [End]
      @ ROW,105 SAY PRICE    PICTURE '9999.99'
      @ ROW,118 SAY PIKTKT
      IF .NOT. EMPTY(PIKTKT) .AND. ((TOTQTY - TOTPIK)>0)
        @ ROW,127 SAY TOTQTY - TOTPIK PICTURE '99999'
      ENDIF
    ENDIF
    ROW = ROW+1
  ENDIF  && END OF DETAIL

  SELECT &lcTempLine      
  IF llNotes .AND. llRpOrdLnt  .AND. lcRpKind = 'D' .AND. !llRpSummMt
    IF !EMPTY(NOTE_MEM)
      ROW =ROW + 1
      @ ROW , 00 SAY 'ORDER NOTES  :'
      FOR X = 1 TO MEMLINES(NOTE_MEM)
        @ ROW,15 SAY MLINE(NOTE_MEM,X)
        ROW =ROW + 1
      ENDFOR
    ENDIF
  ENDIF
  **********************************************************************
  ********************* END PRINT THE LINE DETAIL ****************
  **********************************************************************
  laTotal(1,1)  = laTotal(1,1) +  IIF(llSum_Stor,lnTotQty,TOTQTY)
  laTotal(1,2) = laTotal(1,2) + IIF(llSum_Stor,lnAmt,TOTQTY*PRICE)

  IF lcRpSortBy = 'S'
    laClrTot(1)  = laClrTot(1) +  IIF(llSum_Stor,lnTotQty,TOTQTY)
    laClrTot(2) = laClrTot(2) + IIF(llSum_Stor,lnAmt,TOTQTY*PRICE)
  ENDIF
  IF lcRpSortBy = 'A'
    laOrdTot(1) = laOrdTot(1) +  TOTQTY
    laOrdTot(2) = laOrdTot(2) +  TOTQTY * PRICE
  ENDIF
ENDSCAN



*----------------------------------------------------
* [DOCLR] STYLE/CLR SUB TOTALS
*----------------------------------------------------
IF lcRpSortBy='S' 
  IF (lcRpKind = 'S' .AND. lcRpStyPrn = 'N')
    @ ROW,93 SAY laClrTot(1)  PICTURE '9999999'
    @ ROW,118 SAY laClrTot(2)  PICTURE '$9999999.99'
  ELSE
    ROW = ROW+1
    @ ROW,030 SAY '***'
    @ ROW,035 SAY LEFT(lcCtlColor,12)
    @ ROW,048 SAY RIGHT(lcCtlColor,6)
    @ ROW,93 SAY laClrTot(1)  PICTURE '9999999' 
    @ ROW,118 SAY laClrTot(2)  PICTURE '$9999999.99'
  ENDIF
  ROW = ROW+1
  lcCtlColor = STYLE
  laClrTot = 0.00
ENDIF

*----------------------------------------------------
* [DOORD] ACCOUNT/ORDER SUB TOTALS
*----------------------------------------------------
IF lcRpSortBy='A' 

  @ ROW,000 SAY '***'
  @ ROW,006 SAY LEFT(CTLOR,5)
  @ ROW,012 SAY SUBSTR(CTLOR,6,6)
  @ ROW,019 SAY 'CUST. PO# :'
  *C102337,4 AME [start] Cust Po. in A27 is 15 chr length.
  *@ ROW,031 SAY RIGHT(CTLOR,10)
  @ ROW,031 SAY RIGHT(CTLOR,15)
  *C102337,4 AME [End]
  @ ROW,93 SAY laOrdTot(1)  PICTURE '9999999'
  @ ROW,118 SAY laOrdTot(2)  PICTURE '$9999999.99'
  ROW = ROW+2
  *C102337,4 AME [Start] No need for this stament since we are not going to use this var again.
  *CTLOR = ACCOUNT+ORDER+IIF(ORDHDR.MULTIPO,'*MULTI-PO*',ORDHDR.CUSTPO)
  *C102337,4 AME [End]
  laOrdTot = 0.00
  IF llRpOrdNot
    XPRV_ROW = ROW
    XREPORT='SONAP900'
    DO lfPrt_Note WITH 'B',lcOrder
    ROW = IIF(ROW=XPRV_ROW,ROW,ROW+1)   && INCREMNET ROW ONLY IF NOTEPAD PRINTED
    lcOrder=ORDER
  ENDIF
ENDIF


*----------------------------------------------------
*-- [LEVEL 1] STYLE SUB TOTALS
*----------------------------------------------------

llNewStyle = .F.
*-- PRINT STYLE SUBTOTAL ONLY FOR REPORTS S,F
IF lcRpSortBy $ 'SF'
  @ ROW,000 SAY REPLICATE('-',132)
  ROW =ROW+1
  @ ROW,000 SAY SUBSTR(lcStyDesc,1,12)
  @ ROW,013 SAY '('+lcStyFab+')'
  @ ROW,026 SAY LEFT(lcCtlStyle,12)
  @ ROW,039 SAY '******'
  @ ROW,93 SAY laTotal(1,1)  PICTURE '9999999' 
  @ ROW,118 SAY laTotal(1,2)  PICTURE '$9999999.99'
  ROW = ROW+1
  @ ROW,000 SAY REPLICATE('-',132)
  ROW = ROW+2
  lcCtlStyle = STYLE.CStyMajor
  lcStyDesc  = STYLE.DESC
  lcStyFab   = STYLE.FABRIC
  lcScale    = STYLE.SCALE
  llNewStyle = .T.
ENDIF

FOR X = 1 TO 2
  laTotal(2,X) = laTotal(2,X) + laTotal(1,X)
  laTotal(3,X) = laTotal(3,X) + laTotal(1,X)
  laTotal(1,X) = 0.00
ENDFOR


*----------------------------------------------------
*-- [LEVEL 2] USER DEFINED SUB TOTALS
*----------------------------------------------------
IF BREAK<>SPACE(1) 
  @ ROW,000 SAY REPLICATE('-',132)
  ROW = ROW + 1
  IF lcRpSortBy = 'O'
    @ ROW,000 SAY SUBSTR(HBREAK,10,5)
    @ ROW,006 SAY SUBSTR(HBREAK,1,6)
  ELSE
    IF lcRpSortBy='R'
      lcName=SalesRep.NAME
    ELSE
      lcName = SPACE(0)
    ENDIF
    @ ROW,000 SAY HBREAK+' '+lcName
  ENDIF
  IF LFCR $ HBREAK
    ROW = ROW+1
  ENDIF
  @ ROW,93 SAY laTotal(2,1)  PICTURE '9999999'
  @ ROW,118 SAY laTotal(2,2)  PICTURE '$9999999.99'
  ROW = ROW + 1
  @ROW,000 SAY REPLICATE('-',132)
  ROW = ROW + 2
  HBREAK = &BREAK
  FOR X = 1 TO 2
    laTotal(2,X) =0.00
  ENDFOR
ENDIF


*----------------------------------------------------
* [LEVEL 3] GRAND TOTALS
*----------------------------------------------------
IF ROW >=60
  ROW = 5
ENDIF
@ ROW,000 SAY REPLICATE('=',132)
ROW = ROW+1
@ ROW,010 SAY 'GRAND TOTALS'
@ ROW,93 SAY laTotal(3,1)  PICTURE '9999999'
@ ROW,118 SAY laTotal(3,2)  PICTURE '$9999999.99'

ROW = ROW+1

@ ROW,000 SAY REPLICATE('=',132)

WAIT CLEAR
DO ENDREPORT
SELECT &lcTempLine
SET  RELATION TO
SET DEVICE TO SCREEN 
RETURN

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AHMED Mohamed ELAnwer. (AME)
*! Date      : 04/26/2001
*! Purpose   : Option Grid When function
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

  DECLARE laRpSource[5]
  
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
  
  STORE 'Bid'      TO laRpSource[1]  
  STORE 'Open'     TO laRpSource[2]
  STORE 'Hold'     TO laRpSource[3]
  STORE 'Complete' TO laRpSource[4]
  STORE 'Canceled' TO laRpSource[5]
  lcRpStatus = 'BOHCX'



  SET ORDER TO ORDHDR IN ORDHDR      && To use it to validate ORDER   # in option grid.
  SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate ACCOUNT # in option grid.
  SET ORDER TO STYLE IN STYLE        && To use it to validate STYLE   # in option grid.

  IF 'MA' $ gcCmpModules
    SET ORDER TO FABRIC IN FABRIC      && To use it to validate FABRIC  # in option grid.
  ENDIF  
  
ENDIF

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Ahmed Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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
 
*C102337,4 AME [Start] Eval The length of Major_seg.
lnMajorLen = LEN(gfItemMask("PM"))   && Style major length.
*C102337,4 AME [End]
 
 
 
  
*-- if you does not find Non Major Type Color Code.
IF !lfNMajType('C',lnMajSeg)  
  = lfNMajType('F',lnMajSeg)  && Check for Non Major Type Free code.
ENDIF  && end if you does not find Non Major Type Color Code.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''
*-- end of lfEvalSegs.
*!*************************************************************
*! Name      : lfNMajType
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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
*! Name      : lfSROrder
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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
*! Name      : lfvOStatus
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
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
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Bid','B',;
                              IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Complete','C',;                                                            
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))))
  ENDFOR  && end Loop to make Status expression.
ENDIF


lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))

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
*! Name      : lfItmPos
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : Evaluate fixed filter position within array.
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

*!*************************************************************
*! Name      : lfvSelcBy
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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

*!**************************************************************************
*! Name      : lfwOldSelc
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : To store the Old Select By value
*!**************************************************************************
*! Example   : = lfwOldSelc()
*!**************************************************************************
FUNCTION lfwOldSelc
lcOldSelc = lcRpSelcBy
*-- End of lfwOldSelc.


*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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
*! Name      : lfStySum
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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
RETURN INT(lnTotcomp)
*-- end of lfStySum.


*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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
*! Name      : lfFabSum
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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
*! Name      : lfArrDumy
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : Fill Sort and select arrays
*!*************************************************************
*! Example   : = lfArrDumy()
*!*************************************************************
*B802418,1 Adjust array dimensions
*-- Function to fill select by and sort by arrays.
FUNCTION lfArrDumy
PRIVATE lnSelElms 
lnSelElms = 3

DIMENSION laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1]




*-- Fill Select by array base elements. [Begin]
laSlctDesc[1,1] = lcStyMajor 
laSlctDesc[2,1] = 'Account'
laSlctDesc[3,1] = 'All'

laSlctVal[1,1]  = 'S'
laSlctVal[2,1]  = 'A'
laSlctVal[3,1]  = 'L'
*-- Fill Select by array base elements. [End  ]


IF 'MA' $ gcCmpModules
  lnSelElms = lnSelElms + 1
  DIMENSION laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1]

  lnInsFabIn = ASCAN(laSlctDesc,'All',1)
  =AINS(laSlctDesc,lnInsFabIn,1)
  =AINS(laSlctVal,lnInsFabIn,1)
  STORE 'Fabric' TO laSlctDesc[lnInsFabIn,1]
  STORE 'F'      TO laSlctVal[lnInsFabIn,1]
ENDIF


*!*************************************************************
*! Name      : lfClearRep
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep
llClearFn = .T.
SELECT Style
CLOSE INDEXES

IF FILE(gcWorkDir+lcStyIndex+'.CDX')
  ERASE (gcWorkDir+lcStyIndex+'.CDX')
ENDIF

*-- Close temp. opended files, if its used.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : Evaluate fixed filter position within array.
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
*!*************************************************************
*! Name      : lfvKind
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : Enable/disable some objects due to report kind (Detail/Summary)
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
IF lcRpKind = 'S'
  STORE .F. TO llRpSummMt,llRpOrdLnt,llRpOrdNot
ENDIF
CLEAR READ

*!*************************************************************
*! Name      : lfvSumm
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : Enable/disable Order line notes object
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
CLEAR READ

*-- end of lfvSumm.


*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : 1- Enable and disable some variavle objects due to sort case
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

llRpSummMt = IIF(lcRpSortBy <> 'S' ,.F.,llRpSummMt)
llRpOrdNot = IIF(lcRpSortBy <> 'A' .OR. lcRpKind = 'S',.F.,llRpOrdNot)
lcRpStyPrn = IIF(lcRpSortBy <> 'S' ,.F.,lcRpStyPrn)
CLEAR READ
*-- end of lfvSortBy.
*!*************************************************************
*! Name      : lfArrDumy
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : Fill Sort and select arrays
*!*************************************************************
*! Example   : = lfArrDumy()
*!*************************************************************
*-- Function to fill select by and sort by arrays.
FUNCTION lfArrDumy
PRIVATE lnSelElms , lnSrtElms
lnSelElms = 3
lnSrtElms = 5
DIMENSION laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1],;
          laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1]



*-- Sort 1 base elements [Begin]
laSortDesc[1,1] = 'Account'
laSortDesc[2,1] = 'Order'
laSortDesc[3,1] = lcStyMajor
laSortDesc[4,1] = 'Complete Date'
laSortDesc[5,1] = 'Primary Sales Representative'


laSortVal[1,1]  = 'A'
laSortVal[2,1]  = 'O'
laSortVal[3,1]  = 'S'
laSortVal[4,1]  = 'D'
laSortVal[5,1]  = 'R'
*-- Sort 1 base elements [End  ]

*-- Fill Select by array base elements. [Begin]
laSlctDesc[1,1] = lcStyMajor
laSlctDesc[2,1] = 'Account'
laSlctDesc[3,1] = 'All'

laSlctVal[1,1]  = 'S'
laSlctVal[2,1]  = 'A'
laSlctVal[3,1]  = 'L'
*-- Fill Select by array base elements. [End  ]


IF 'MA' $ gcCmpModules
  lnSelElms = lnSelElms + 1
  lnSrtElms = lnSrtElms + 1
  DIMENSION laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1],;
            laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1]

  =AINS(laSortDesc,3,1)
  =AINS(laSortVal,3,1)

  lnInsFabIn = ASCAN(laSlctDesc,'All',1)
  =AINS(laSlctDesc,lnInsFabIn,1)
  =AINS(laSlctVal,lnInsFabIn,1)
  STORE 'Fabric' TO laSortDesc[3,1],laSlctDesc[lnInsFabIn,1]
  STORE 'F' TO laSortVal[3,1],laSlctVal[lnInsFabIn,1]
ENDIF

*-- end of lfArrDumy.

*!*************************************************************
*! Name      : lfPrt_Note
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : PRINT NOTEPADS
*!*************************************************************
*! Example   : = lfPrt_Note()
*!*************************************************************
PROCEDURE lfPrt_Note

PARAMETER XTYPE,XKEY
PRIVATE lnAlias, lnMemoWdth, lnMemoLnNo, lnMemolins
WAIT CLEAR
lnAlias = SELECT()
IF !USED('NOTEPAD')
  =gfOpenFile(GCDataDir+'NOTEPAD','NOTEPAD','SH')
ENDIF
SELECT NOTEPAD

IF SEEK (XTYPE+XKEY)
  lnMemoWdth = SET('MEMOWIDTH')
  SET MEMOWIDTH TO 75
  lnMemoLnNo = 0      
  lnMemolins = 0
  IF ROW > 55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH XREPORT,lcRpMsg1,R_WIDTH
    ROW = 5
  ENDIF

  lnMemolins = MEMLINES(mNotes)
  IF lnMemolins > 0
    @ ROW,00 SAY REPLICATE('|',80 )
    ROW = ROW + 1
    DO WHILE lnMemoLnNo <> lnMemolins .AND. INKEY() <> 32
      IF ROW > 55
        PAGENO = PAGENO + 1
        DO RPT_HDR WITH XREPORT,lcRpMsg1,R_WIDTH
        ROW = 5
      ENDIF
      lnMemoLnNo = lnMemoLnNo + 1
      IF SUBSTR(MLINE(mNotes,lnMemoLnNo),1,1) = '*'
        LOOP
      ENDIF
      @ ROW,00 SAY '|'+MLINE(mNotes,lnMemoLnNo)
      ROW = ROW + 1
    ENDDO
    @ ROW,00 SAY REPLICATE('|',80 )
    ROW = ROW + 1
  ENDIF
  SET MEMOWIDTH TO (lnMemoWdth)
ENDIF

SELECT (lnAlias)
RETURN

*-- end of lfPrt_Note.