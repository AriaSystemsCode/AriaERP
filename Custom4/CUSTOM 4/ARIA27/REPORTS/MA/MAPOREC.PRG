*:***************************************************************************
*: Program file  : MAPOREC
*: Program desc. : Material PURCHASE ORDER RECEIPT REPORT
*! Date          : 03/17/1999
*: System        : Aria Advantage Series.
*: Module        : MATERIALS (M)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : Enh. # 301165
*:***************************************************************************
*: Example : DO MAPOREC
*:***************************************************************************
*: Modifications:
*:B603148,1 HDM 09/19/1999  Costing variables should be read from
*:                           base Currency fields instead of forign
*:                           currency fields
*:                           Assign 0 to lnTOTqty to avoid lnTOTqty 
*:                           not found case no receivings lines for
*:                           the PO
*:C101767,1 ADEL 02/03/2000 - Add 'Print by Roll Y/N' in OG.
*:C101767,1                 - Print data collected by rolls. 
*:B603186,1 ADEL 08/30/2000 1- Make 'Division' option work
*:B603186,1 ADEL            2- Remove 'Bid' from status as no receivings available for 'Bid'.
*:B603186,1 ADEL            3- Change 'Fabric #' to "Fabric".
*:B603186,1 ADEL            4- Give the facility of 'Currency' to the report.
*:B603891,1 ABD 09/28/2000 Fix bug that the Price & Amount filed print *******
*:B603891,1 ABD            When company multi currency and print with equivalent 
*:B603891,1 ABD            Based on the Exchange rates defined on or will define now.
*:E301816,1 ABD 03/18/2002 Add an option to the reciept log report to print 
*:E301816,1 ABD            Rolls that have zero qty (Y/N)
*:B606632,1 ABD 12/12/2002 Fix bug that the report print the foreign currency value
*:B606632,1 ABD            in the actual cost instead of the vase currency.
*:***************************************************************************

*-- llPrint variable that is used to prevent printing if there is not
*--           any record matches the report criteria
PRIVATE llPrint
llPrint  = .F.  

*--B603148,1 HDM [Start]
STORE 0 TO lnTOTqty
*--B603148,1 HDM [End]

     *** LOOP FOR THE REPORT CRITERIA ***
XTITLE     = lcRpOpTi
*B603186,1 (Begin) Print Selcted PO type.
*XFILTER    = lcRpExp
XFILTER    = lcRpExp + IIF(lcRpPrntBy = 'P',"AND cmattype = 'P'",IIF(lcRpPrntBy = 'R',"AND cmattype = 'R'","AND .T."))
*B603186,1 (End)

lnChoice   = lcRpSort
lcCstDtl   = lcRpMcd 
XDYELOT_S  = (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')

*-- lcWorkFile  Variable that hold the temporary file name
lcWorkFile = gfTempName()

*-- R_WIDTH  Variable that hold the report width
*-- R_TITLE  Variable that hold the report title
*-- XREPORT  variable that hold the report name

R_WIDTH  = 'W'                         && Standard Report is Wide
R_TITLE  = 'MATERIAL PURCHASE ORDER RECEIPTS REPORT'
XREPORT  = 'MAPoRec'

*-- gcContCode golabel Variable that hold the name of country

llIsCanada = IIF(UPPER(ALLTRIM(gcContCode)) = 'CANADA', .T., .F.) 
llIsEngland= IIF(UPPER(ALLTRIM(gcContCode)) = 'ENGLAND', .T., .F.)


*-- llMultiWH  Variable that hold the if there is multiwa or not
llMultiWH  = (ALLTRIM(UPPER(gfGetMemVar('M_WareHouse'))) = 'Y')


*-- lcWareCode  Variable that hold the cotentes of the field warehous

lnDatePos = ASCAN(laOGFxFlt,"WAREHOUS.CWARECODE")
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    lcWareCode  = ""
  ELSE  
    lcWareCode  = laOGFxFlt[lnDatePos,6]
  ENDIF
ENDIF


*-- DIMENSIONS   that hold cost elements and totals.
DIMENSION laEst [4]
DIMENSION laLan [4]
DIMENSION laAct [4]
DIMENSION laSub[4,3]
DIMENSION laSEst [4]
DIMENSION laSLan [4]
DIMENSION laSAct [4]
DIMENSION laFinal[4,3]
*-- initialize them 
STORE 0 TO laSEst , laSAct , laSlan ,laEst , laAct , laLan ,lnLan 
STORE 0 TO lnAct_Lan , lnLan_Est , lnAct_Est
STORE 0 TO lnFinEst  , lnFinLan ,lnFinAct
STORE 0 TO laFinal ,laSub, lnEst ,lnACT , lnLand , lnX,lnY,lnZ

*---- SORT TO  lcWorkFile FILE	 
BREAK     = ' '
BREAKD    = ' '

DO CASE
     
*--SUMMARY BY ORDER#
  CASE lnChoice ='P'
    lcSortFld   = 'POMAT+TRANCD+FABRIC+COLOR+STR(RECNO(),7)'
    BREAK       = 'POMAT'
    BREAKD      = 'POMAT'

*--SUMMARY BY VENDOR ACCOUNT
  CASE lnChoice ='V'
    lcSortFld   = 'VENDOR+"P"+POMAT+TRANCD+FABRIC+COLOR+STR(RECNO(),7)'
    BREAK       = 'VENDOR'
    BREAKD      = 'VENDOR'

*--SUMMARY BY PRIOIRTY
  CASE lnChoice ='I'                                       
    lcSortFld   = 'FABRIC+TRANCD+COLOR+POMAT+STR(RECNO(),7)'
    BREAK       = 'FABRIC'
    BREAKD      = 'FABRIC'

*--SUMMARY BY COLOR
  CASE lnChoice ='C'
    lcSortFld   = 'FABRIC+COLOR+TRANCD+POMAT+STR(RECNO(),7)'
    BREAK       = 'FABRIC+COLOR'
    BREAKD      = 'COLOR'
ENDCASE
*B603186,1 (Begin) Make Subtotals work if the currency changes.
lcSortFld = IIF(llMultCurr AND lcRpCurr = "F" AND !llRpRoll,"cCurrCode+"+lcSortFld,lcSortFld)
BREAK = IIF(llMultCurr AND lcRpCurr = "F",'cCurrCode+"/"+'+BREAK,BREAK)
*B603186,1 (End)
*C101767,1 (Begin) When collecting data by roll Collect them only when the user changes selection criteria.
lnRollNo = 0
IF llRpRoll
  IF llOgFltCh
    *-- If no records selected return otherwise print the report by rolls.
    IF !lfRollData()
      =gfModalGen('TRM00052B00000','DIALOG')
      *-- If the temp file has any records from a previous preview zap it.
      IF USED(lcRollTmp) AND RECCOUNT(lcRollTmp) > 0
        SELECT (lcRollTmp)
        ZAP
      ENDIF
      RETURN
    ENDIF
  ELSE
    *-- Just reprint.
    =lfPrintRep()
  ENDIF
  *-- Quit program after printing by roll.
  RETURN
ENDIF
*C101767,1 (End)

*---THE MAIN PROGRAM    
DO lpCollData

*--- TO PRINT THE REPORT     
IF llPrint
  SET DEVICE TO PRINT
  DO  lpPrint
  DO  ENDREPORT
  SET DEVICE TO SCREEN     
ENDIF  

IF USED(lcWorkFile)
  SELECT (lcWorkFile)
  SET RELATION TO
  USE 
ENDIF

*-- ERASE THE lcWorkFile
ERASE (gcWorkDir+lcWorkFile+'.DBF')  
ERASE (gcWorkDir+lcWorkFile+'.CDX')  

*!*************************************************************
*! Name      : lpCollData
*! Developer : BASSEM RAFAAT 
*! Date      : 03/17/1999
*! Purpose   : COLLECT THE DATA 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : ......
*!*************************************************************

PROCEDURE lpCollData

SELECT POFLN
SET FILTER TO
SET RELATION TO

*B603186,1 (Begin) Take into consideration the returns also.
*SET RELATION TO "P"+POMAT INTO POFHDR
SET RELATION TO POFLN.CMATTYPE+POMAT INTO POFHDR
*B603186,1 (End)
SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE
SET RELATION TO CWARECODE INTO WAREHOUS ADDITIVE

*-- HDM B603148,1 [Start] Include trancd in the filter
*LOCATE ALL FOR &XFILTER
LOCATE ALL FOR &XFILTER AND POFLN.TRANCD = '2'
*-- HDM B603148,1 [End]
IF !FOUND()
  =gfModalGen('TRM00052B00000','DIALOG')
  llPrint = .F.
  RETURN
ELSE
  *B603186,1 (Begin) Add the currency filed in case of Foriegn Currency.
  *COPY REST TO &lcWorkFile FOR &XFILTER AND POFLN.TRANCD = '2' && FOR RECEIVING ONLY
  IF llMultCurr AND lcRpCurr = "F"
    =AFIELDS(laFileStru)
    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru+1,4]
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'cCurrCode'
    laFileStru[lnFileStru,2] = 'C'
    laFileStru[lnFileStru,3] = 3
    laFileStru[lnFileStru,4] = 0
    CREATE TABLE (gcWorkDir+lcWorkFile) FROM ARRAY laFileStru
    SELECT POFLN
    SCAN REST FOR &XFILTER AND POFLN.TRANCD = '2'
      SCATTER MEMVAR MEMO
      m.cCurrCode = IIF(SEEK(POFLN.CMATTYPE+POMAT,'POFHDR'),POFHDR.cpricecur,SPACE(3))
      INSERT INTO (lcWorkFile) FROM MEMVAR
    ENDSCAN
    SELECT (lcWorkFile)
    USE
  ELSE
    COPY REST TO (gcWorkDir+lcWorkFile) FOR &XFILTER AND POFLN.TRANCD = '2' && FOR RECEIVING ONLY
  ENDIF  
  *BB603186,1 (End)
  llPrint = .T.
  SELECT POFLN
  SET RELATION TO
  *B603186,1 (Begin) Open the temp file from the work directory.
  *=gfOpenFile('&lcWorkFile',' ','EX')
  =gfOpenFile((gcWorkDir+lcWorkFile),' ','EX')
  *B603186,1 (End)

  *--Sort to lcWorkFile Index
  SELECT (lcWorkFile)
  IF !EMPTY(lcSortFld)
    WAIT  'SORTING  RECORDS FOR FABRIC PURCHASE ORDER DETAIL REPORT ...' WINDOW NOWAIT
    INDEX ON &lcSortFld TAG &lcWorkFile
    *B603186,1 (Begin) Take into consideration the returns also.
    *SET RELATION TO "P"+POMAT INTO POFHDR
     SET RELATION TO &lcWorkFile..CMATTYPE+POMAT INTO POFHDR
    *B603186,1 (End)
    SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE
    SET RELATION TO CWARECODE INTO WAREHOUS ADDITIVE
    WAIT CLEAR
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lpPrint
*! Developer : BASSEM RAFAAT 
*! Date      : 03/17/1999
*! Purpose   : Print the report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************

PROCEDURE lpPrint

*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*MAT680                           MATERIAL P.O. RECEIPTS - 123456789012345678901234567890                           PAGE: 1234
*MM/DD/YY
*
*!    0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*!    PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        REFERENCE  PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE $PRICE        AMOUNT
*!    PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        DYELOT #   PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE $PRICE        AMOUNT
*!..  ±±±±±± ±±±±±±±± ±±/±±/±± ± ±±±±±±± ±±±±±± ±±±±±±±±±±±± ±±±±±±±±±± ±±±±±±±±±± ±±±±±± ±±±±±±±.±±± ±±± ±±±±±±± ±±± ±±±.±± ±±±±±±±±±±±±±
*!..  123456 12345678 12345678 1 1234567 123456 123456789012 1234567890 1234567890 123456 12345678901 123 1234567 123 123456 1234567890123
*
* *SUB TOTAL*     XXXXXXXXXXXX                                                        99999999.999    99999999            9999999.99
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..

****** Variables Initialization ******
DIMENSION XTOTAL(1,3),XTOTAL1(1,3)
XTOTAL = 0.00
XTOTAL1= 0.00
XTIME  = TIME()
PAGENO = 0
ROW    = 99
SELECT &lcWorkFile
GOTO TOP

IF LEN(TRIM(BREAK)) <>0
  HBREAK = &BREAK
ENDIF

IF lcCstDtl = 'Y'
  DO lpPrintCost
  RETURN
ENDIF      
    
SELE &lcWorkFile

R_Title = R_Title + IIF( llMultiWH .AND. !EMPTY( lcWareCode ),;
                        ' FOR WAREHOUSE: '+ lcWareCode , '')
*---------------------------------------------------------
* BEGIN [MAIN REPORT] LOOP
*---------------------------------------------------------
*B603186,1 (Begin) Initialize 3 new variables for the Currency Subtotal.
IF llMultCurr AND lcRpCurr = "F"
  STORE 0 TO lnCurrFab,lnFabConv,lnFabAmntC
  lcCrntCurr = cCurrCode
ENDIF  
*B603186,1 (End)
DO WHILE .T.

  IF ROW >=53
     PAGENO = PAGENO+1
     DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
     *B603186,1 (Begin) Currency signs will be removed in case of multi currency.
     IF llMultCurr
       DO lpMulCurrHdr
     ELSE  
     *B603186,1 (End)
       IF !XDYELOT_S
         IF llIsEngland
           @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        REFERENCE  PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE œPRICE      AMOUNT'
         ELSE
           IF llIsCanada
             @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        REFERENCE  PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE  PRICE      AMOUNT'
           ELSE
             @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        REFERENCE  PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE $PRICE      AMOUNT'
           ENDIF
         ENDIF
       ELSE
         IF llIsEngland
           @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        DYELOT #   PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE œPRICE      AMOUNT'
         ELSE
           IF llIsCanada
             @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        DYELOT #   PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE PRICE      AMOUNT'
           ELSE
             @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        DYELOT #   PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE $PRICE      AMOUNT'
           ENDIF
         ENDIF
       ENDIF
     *B603186,1 (Begin) Currency signs will be removed in case of multi currency.
     ENDIF
     *B603186,1 (End)
     @ 07,00 SAY REPLICATE('=',130)
     ROW = 08
  ENDIF

  ******---- Begin Subtotals Loop ----******
  DO WHILE LEN(TRIM(BREAK)) <>0
     IF &BREAK = HBREAK
        EXIT
     ENDIF
     ROW=ROW+1
     @ ROW,00 SAY REPLICATE('-',130)
     ROW = ROW+1
     @ ROW,000 SAY '* SUB TOTAL * '
     @ ROW,015 SAY HBREAK
     @ ROW,083 SAY XTOTAL(1,1)  PICTURE '99999999.999'     
     @ ROW,099 SAY XTOTAL(1,2)  PICTURE '99999999'
     @ ROW,118 SAY XTOTAL(1,3) PICTURE '99999999.999'
     ROW = ROW+1
     @ ROW,00 SAY REPLICATE('-',130)
     ROW = ROW+1
     X = 1

     DO WHILE (X <= 3)
        XTOTAL1(1,X) = XTOTAL1(1,X) + XTOTAL(1,X)
        XTOTAL(1,X) = 0.00
        X = X+1
     ENDDO

     HBREAK = &BREAK
     EXIT

  ENDDO
  *B603186,1 (Begin) Print currency subtotal
  IF (llMultCurr AND lcRpCurr = "F") AND  cCurrCode <> lcCrntCurr
     ROW=ROW+1
     @ ROW,00 SAY REPLICATE('=',130)
     ROW = ROW+1
     @ ROW,000 SAY '* SUB TOTAL * '
     IF SEEK(lcCrntCurr,'SYCCURR')
       lcCrntCurr = lcCrntCurr + ' - ' + ALLTRIM(SYCCURR.CCURRDESC)
     ENDIF
     @ ROW,015 SAY lcCrntCurr
     @ ROW,083 SAY lnCurrFab  PICTURE '99999999.999'     
     @ ROW,099 SAY lnFabConv  PICTURE '99999999'
     @ ROW,118 SAY lnFabAmntC PICTURE '99999999.999'
     ROW = ROW+1
     @ ROW,00 SAY REPLICATE('=',130)
     ROW = ROW+1
     STORE 0 TO lnCurrFab,lnFabConv,lnFabAmntC     
     lcCrntCurr = cCurrCode
  ENDIF
  *B603186,1 (End)
  ******---- End Subtotals Loop ----******

  IF EOF()
     EXIT
  ENDIF

  IF ROW >=53
     ROW = 99
     LOOP
  ENDIF

  SELECT &lcWorkFile

  ROW=ROW+1
  @ ROW,000 SAY POMAT
  @ ROW,007 SAY VENDOR
  @ ROW,016 SAY DATE
  @ ROW,025 SAY POFHDR.STATUS
  @ ROW,027 SAY FABRIC
  @ ROW,035 SAY COLOR
  @ ROW,042 SAY LEFT(FABRIC.DESC,12)
      
      
  IF XDYELOT_S     && IF DYELOT SYSTEM
    @ ROW,055 SAY DYELOT
  ELSE
    @ ROW,055 SAY LEFT(REFERENCE,11)
  ENDIF

  @ ROW,066 SAY PATTERN    
  @ ROW,077 SAY WIDTH
  @ ROW,084 SAY NFABTOTQTY PICTURE '9999999.999'
  @ ROW,096 SAY FABRIC->UOMBUY                      
  @ ROW,0100 SAY (NFABTOTQTY*FABRIC->CONV)  PICTURE '9999999'
  @ ROW,108 SAY FABRIC->UOMUSE
  *B603186,1 (Begin) Print in proper currency dipaly.
  *@ ROW,111 SAY nelancost1 PICTURE '999.999'
  *@ ROW,115 SAY (NFABTOTQTY*(nelancost1)) PICTURE '99999999.999'
  lnActPric = lfCalAmnts()
  @ ROW,111 SAY lnActPric PICTURE '999.999'
  @ ROW,115 SAY (NFABTOTQTY*(lnActPric)) PICTURE '99999999.999'
  *B603186,1 (End)

  IF llMultiWH .AND. EMPTY( lcWareCode )
    Row = Row + 1
    @ Row,000 SAY 'WAREHOUSE: '+ cWareCode
    @ Row,018 SAY IIF( SEEK( cWareCode , 'WareHous'),;
                           WareHous.cDesc, '' )
  ENDIF
  XTOTAL(1,1)=XTOTAL(1,1)+NFABTOTQTY
  XTOTAL(1,2)=XTOTAL(1,2)+(NFABTOTQTY*FABRIC->CONV)
  *B603186,1 (Begin) Take the proper price.
  *XTOTAL(1,3)=XTOTAL(1,3)+(NFABTOTQTY*(nelancost1)) 
  XTOTAL(1,3)=XTOTAL(1,3)+(NFABTOTQTY*(lnActPric)) 
  IF llMultCurr AND lcRpCurr = "F"
    lnCurrFab  = lnCurrFab+ NFABTOTQTY 
    lnFabConv  = lnFabConv+(NFABTOTQTY*FABRIC->CONV) 
    lnFabAmntC = lnFabAmntC + (NFABTOTQTY*(lnActPric))
  ENDIF  
  *B603186,1 (End)
  SELECT &lcWorkFile
  SKIP
ENDDO
*------------------ END [MAIN REPORT] LOOP --------------------
*B603170,1 (Begin) Print Grand Total in case of Single currenc or Muti and not in Foriegn Curr. 
IF !llMultCurr OR lcRpCurr <> "F"
*B603170,1 (End)
  X = 2
  IF LEN(TRIM(BREAK)) =0
    X =1
  ENDIF
  ROW=ROW+2
  @ ROW,00 SAY REPLICATE('=',130)
  ROW = ROW+1
  @ ROW,000 SAY '* GRAND TOTAL *'
  @ ROW,082 SAY XTOTAL1(1,1)  PICTURE '999999999.999'
  @ ROW,098 SAY XTOTAL1(1,2)  PICTURE '999999999' 
  @ ROW,117 SAY XTOTAL1(1,3)  PICTURE '999999999.999'
  ROW = ROW+1
  @ ROW,00 SAY REPLICATE('=',130)
*B603170,1 (Begin) Print Grand Total in case of Single currenc or Muti and not in Foriegn Curr. 
ENDIF
*B603170,1 (End)

*!*************************************************************
*! Name      : lpPrintCost
*! Developer : BASSEM RAFAAT 
*! Date      : 03/17/1999
*! Purpose   : print the report with material costing details.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrintCost
*!*************************************************************
*! Modifications:
*!     B603148,1 HDM REPLACE nCost* with necost*
*!                   REPLACE nlan_cost* with nelancost*
*!*************************************************************


PROCEDURE lpPrintCost

SELECT &lcWorkFile

R_Title = R_Title + IIF( llMultiWH .AND. !EMPTY( lcWareCode ),;
                        ' FOR WAREHOUSE: '+ lcWareCode , '')
*---------------------------------------------------------
* BEGIN [MAIN REPORT] LOOP
*---------------------------------------------------------
 DO WHILE .T.
  IF Row >=53
    PageNo = PageNo+1
    DO RPT_HDR WITH XReport,XTitle,R_WIDTH
    IF !XDYELOT_S
      IF llIsEngland
        @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        REFERENCE  PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE œPRICE      AMOUNT'
      ELSE
        IF llIsCanada
          @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        REFERENCE  PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE  PRICE      AMOUNT'
        ELSE
          @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        REFERENCE  PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE $PRICE      AMOUNT'
        ENDIF
      ENDIF
    ELSE
      IF llIsEngland
        @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        DYELOT #   PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE œPRICE      AMOUNT'
      ELSE
        IF llIsCanada
          @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        DYELOT #   PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE PRICE      AMOUNT'
        ELSE
          @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        DYELOT #   PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE $PRICE      AMOUNT'
        ENDIF
      ENDIF
    ENDIF
    @ 07,00 SAY REPLICATE('=',130)
    Row = 08
  ENDIF

  ******---- Begin Subtotals Loop ----******
  DO WHILE !EMPTY(Break)
    IF &Break = HBreak
      EXIT
    ENDIF
    Row=Row+1
    DO CASE
      CASE BREAKD ='POMAT'
        lnSort    = 'POMAT'
      CASE BREAKD ='VENDOR'
        lnSort    = 'VENDOR'
      CASE BREAKD ='FABRIC'
        lnSort    = 'FABRIC'
      CASE BREAKD ='COLOR'
        lnSort    = 'COLOR'
    ENDCASE
    @ ROW,00 SAY '************************************ TOTAL FOR &lnSort &HBREAK ***************************************************'
    ROW=ROW+1
    @ ROW,000 SAY '**SUB TOTAL**'
    ROW=ROW+1
  
    @Row,00 SAY "Price"
    @ROW ,013 SAY laSub[1,1]   PICTURE '9999999.999'
    laFinal[1,1] = laFinal[1,1] + laSub[1,1]

    @ROW ,024 SAY laSub[1,2]  PICTURE '9999999.999'
    laFinal[1,2] = laFinal[1,2] + laSub[1,2]
    @ROW ,35 SAY  laSub[1,3]  PICTURE '9999999.999'
    laFinal[1,3] = laFinal[1,3] +laSub[1,3]

    @ROW ,46 SAY (laSub[1,3] - laSub[1,2])                    PICTURE '9999999.999'
    @ROW ,57 SAY (laSub[1,2] - laSub[1,1])                    PICTURE '9999999.999'
    @ROW ,68 SAY (laSub[1,3] - laSub[1,1])                    PICTURE '9999999.999'
    @ROW ,81 SAY IIF(laSub[1,2]=0,'N/A',(laSub[1,3] / laSub[1,2])*100) PICTURE '9999.99'
    @ROW ,91 SAY IIF(laSub[1,1]=0,'N/A',(laSub[1,2] / laSub[1,1])*100) PICTURE '9999.99'
    @ROW ,100 SAY IIF(laSub[1,1]=0,'N/A',(laSub[1,3] / laSub[1,1])*100) PICTURE '9999.99'
    ROW=ROW+1

    @Row,00 SAY "Tax/Duty"
    @ROW ,013 SAY laSub[2,1]   PICTURE '9999999.999'
    laFinal[2,1] = laFinal[2,1] + laSub[2,1]
    @ROW ,024 SAY  laSub[2,2]  PICTURE '9999999.999'
    laFinal[2,2] = laFinal[2,2] + laSub[2,2]
    @ROW ,35 SAY  laSub[2,3]            PICTURE '9999999.999'
    laFinal[2,3] = laFinal[2,3] + laSub[2,3]

    @ROW ,46 SAY (laSub[2,3] - laSub[2,2])                    PICTURE '9999999.999'
    @ROW ,57 SAY (laSub[2,2] - laSub[2,1])                    PICTURE '9999999.999'
    @ROW ,68 SAY (laSub[2,3] - laSub[2,1])                    PICTURE '9999999.999'
    @ROW ,81 SAY IIF(laSub[2,2]=0,'N/A',(laSub[2,3] / laSub[2,2])*100) PICTURE '9999.99'
    @ROW ,91 SAY IIF(laSub[2,1]=0,'N/A',(laSub[2,2] / laSub[2,1])*100) PICTURE '9999.99'
    @ROW ,100 SAY IIF(laSub[2,1]=0,'N/A',(laSub[2,3] / laSub[2,1])*100) PICTURE '9999.99'
    ROW=ROW+1

    @Row,00 SAY "Freight"
    @ROW ,013 SAY laSub[3,1]            PICTURE '9999999.999'
    laFinal[3,1] = laFinal[3,1] + laSub[3,1]
    @ROW ,024 SAY laSub[3,2]  PICTURE '9999999.999'
    laFinal[3,2] = laFinal[3,2] + laSub[3,2]
    @ROW ,35 SAY laSub[3,3]   PICTURE '9999999.999'
    laFinal[3,3] = laFinal[3,3] + laSub[3,3]

    @ROW ,46 SAY (laSub[3,3] - laSub[3,2])                   PICTURE '9999999.999'
    @ROW ,57 SAY (laSub[3,2] - laSub[3,1])                   PICTURE '9999999.999'
    @ROW ,68 SAY (laSub[3,3] - laSub[3,1])                   PICTURE '9999999.999'
    @ROW ,81 SAY IIF(laSub[3,2]=0,'N/A',(laSub[3,3] / laSub[3,2])*100) PICTURE '9999.99'
    @ROW ,91 SAY IIF(laSub[3,1]=0,'N/A',(laSub[3,2] / laSub[3,1])*100) PICTURE '9999.99'
    @ROW ,100 SAY IIF(laSub[3,1]=0,'N/A',(laSub[3,3] / laSub[3,1])*100) PICTURE '9999.99'
    ROW=ROW+1

    @Row,00 SAY "Quota"
    @ROW ,013 SAY laSub[4,1]  PICTURE '9999999.999'
    laFinal[4,1] = laFinal[4,1] + laSub[4,1]
    @ROW ,024 SAY laSub[4,2]  PICTURE '9999999.999'
    laFinal[4,2] = laFinal[4,2] + laSub[4,2]
    @ROW ,35 SAY  laSub[4,3]  PICTURE '9999999.999'
    laFinal[4,3] = laFinal[4,3] + laSub[4,3]

    @ROW ,46 SAY (laSub[4,3] - laSub[4,2])                    PICTURE '9999999.999'
    @ROW ,57 SAY (laSub[4,2] - laSub[4,1])                    PICTURE '9999999.999'
    @ROW ,68 SAY (laSub[4,3] - laSub[4,1])                    PICTURE '9999999.999'
    @ROW ,81 SAY IIF(laSub[4,2]=0,'N/A',(laSub[4,3] / laSub[4,2])*100) PICTURE '9999.99'
    @ROW ,91 SAY IIF(laSub[4,1]=0,'N/A',(laSub[4,2] / laSub[4,1])*100) PICTURE '9999.99'
    @ROW ,100 SAY IIF(laSub[4,1]=0,'N/A',(laSub[4,3] / laSub[4,1])*100) PICTURE '9999.99'
    Row = Row+1
 
    @ ROW ,14 SAY '---------- ----------  --------- ----------  --------- ----------  -------   ------- --------'
    Row = Row+1
    @ ROW , 03 SAY 'Total Cost'
    @ ROW , 13 SAY laSub[1,1]+laSub[2,1]+laSub[3,1]+laSub[4,1]    PICTURE '9999999.999'
    lnX= laSub[1,1]+laSub[2,1]+laSub[3,1]+laSub[4,1]
    lnEst = lnEst + laSEST[1]
    @ ROW , 24 SAY laSub[1,2]+laSub[2,2]+laSub[3,2]+laSub[4,2]   PICTURE '9999999.999'
    lnY=laSub[1,2]+laSub[2,2]+laSub[3,2]+laSub[4,2]
    lnLan = lnLan+laSlan[1]

    @ ROW , 35 SAY laSub[1,3]+laSub[2,3]+laSub[3,3]+laSub[4,3]  PICTURE '9999999.999'
    lnZ=  laSub[1,3]+laSub[2,3]+laSub[3,3]+laSub[4,3]
    lnAcT  = lnAcT + laSact[1]

    @ ROW , 46 SAY lnZ- lnY                                                PICTURE '9999999.999'
    @ ROW , 57 SAY (lnY - lnX)                                                PICTURE '9999999.999'
    @ ROW , 68 SAY (lnZ - lnX)                                                PICTURE '9999999.999'
    @ ROW , 81 SAY IIF(lnY=0,'N/A',(lnZ / lnY)*100)                 PICTURE '9999.99'
    @ ROW , 91 SAY IIF(lnX=0,'N/A',(lnY / lnX)*100)                 PICTURE '9999.99'
    @ ROW , 100 SAY IIF(lnX=0,'N/A',(lnZ / lnX)*100)                PICTURE '9999.99'

    ROW = ROW+1
    @ ROW ,14 SAY '========== ========== ==========  ========= ==========  =========  =======  ======== ======== '
    STORE 0 TO laSub
    HBREAK = &BREAK
    EXIT
  ENDDO
  ******---- End Subtotals Loop ----******
  IF EOF()
    EXIT
  ENDIF

  IF Row >=53
    Row = 99
    LOOP
  ENDIF

  SELECT &lcWorkFile
  Row=Row+1
  @ Row,000 SAY POMAT
  @ Row,007 SAY VENDOR
  @ Row,016 SAY DATE
  @ Row,025 SAY POFHDR.STATUS
  @ Row,027 SAY Fabric
  @ Row,035 SAY COLOR
  @ Row,042 SAY LEFT(Fabric.DESC,12)           
  
  IF XDYELOT_S             && IF DYELOT SYSTEM
    @ Row,055 SAY DYELOT
  ELSE
    @ Row,055 SAY LEFT(REFERENCE,11)
  ENDIF

  @ Row,066 SAY PATTERN
  @ Row,077 SAY WIDTH                               
  @ Row,084 SAY NFABTOTQTY PICTURE '9999999.999'        
  @ Row,096 SAY Fabric->UOMBUY                      
  @ Row,0100 SAY (NFABTOTQTY*Fabric.CONV)  PICTURE '9999999'     
  @ Row,108 SAY Fabric->UOMUSE                               
  @ Row,111 SAY nelancost1              PICTURE '999.999'
  @ Row,118 SAY (NFABTOTQTY*nelancost1) PICTURE '99999999.999'
   
  IF llMultiWH .AND. EMPTY( lcWareCode )
    Row = Row + 1
    @ Row,000 SAY 'WAREHOUSE: '+ cWareCode
    @ Row,018 SAY IIF( SEEK( cWareCode , 'WareHous'),;
                       WareHous.cDesc, '' )
  ENDIF

  Row=Row  +1
  @ Row,014 SAY 'Estimated      Landed    Actual   Act - Lan  Lan - Est  Act - Est  Act/Lan   Lan/Est  Act/Est'
  Row=Row  +1
  @Row,00 SAY "Price"
  @ ROW , 13 SAY NeCOST1*NFABTOTQTY                      PICTURE '9999999.999'  
  laSub[1,1]=laSub[1,1]+(NeCOST1*NFABTOTQTY)
  @ ROW , 24 SAY nelancost1*NFABTOTQTY                    PICTURE '9999999.999'
  laSub[1,2]=laSub[1,2]+(nelancost1*NFABTOTQTY)

  *B606632,1 ABD - Remark the next line and print the actual cost instead of the
  *B606632,1 ABD - The foreign currency value [Begin]
  *@ ROW , 35 SAY NACT_COST1*NFABTOTQTY                    PICTURE '9999999.999'
  *laSub[1,3]=laSub[1,3]+(NACT_COST1*NFABTOTQTY)
  *@ ROW , 46 SAY (NACT_COST1 -  nelancost1)*NFABTOTQTY                    PICTURE '9999999.999'
  *lnAct_Lan= lnAct_Lan+(NACT_COST1 -  nelancost1)*NFABTOTQTY
  @ ROW , 35 SAY nEactCost1*NFABTOTQTY                    PICTURE '9999999.999'
  laSub[1,3]=laSub[1,3]+(nEactCost1*NFABTOTQTY)
  @ ROW , 46 SAY (nEactCost1 -  nelancost1)*NFABTOTQTY                    PICTURE '9999999.999'
  lnAct_Lan= lnAct_Lan+(nEactCost1 -  nelancost1)*NFABTOTQTY
  *B606632,1 ABD - [End]
  
  @ ROW , 57 SAY (nelancost1 - NeCOST1)*NFABTOTQTY        PICTURE '9999999.999'
  lnLan_Est=lnLan_Est+(nelancost1 - NeCOST1)*NFABTOTQTY

  *B606632,1 ABD - Remark the next line and print the actual cost instead of the
  *B606632,1 ABD - The foreign currency value [Begin]
  *@ ROW , 68 SAY (NACT_COST1 - NeCOST1)*NFABTOTQTY        PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(NACT_COST1 - NeCOST1)*NFABTOTQTY
  *@ ROW , 81 SAY IIF(nelancost1=0,'N/A',(NACT_COST1 / nelancost1)*100)  PICTURE '9999.99'  
  @ ROW , 68 SAY (nEactCost1 - NeCOST1)*NFABTOTQTY        PICTURE '9999999.999'
  lnAct_Est=lnAct_Est+(nEactCost1 - NeCOST1)*NFABTOTQTY
  @ ROW , 81 SAY IIF(nelancost1=0,'N/A',(nEactCost1 / nelancost1)*100)  PICTURE '9999.99'
  *B606632,1 ABD - [End]
  
  @ ROW , 91 SAY IIF(NeCOST1=0,'N/A',(nelancost1 / NeCOST1)*100)   PICTURE '9999.99'
  
  *B606632,1 ABD - Remark the next line and print the actual cost instead of the
  *B606632,1 ABD - The foreign currency value [Begin]
  *@ ROW , 100 SAY IIF(NeCOST1=0,'N/A',(NACT_COST1 /NeCOST1)*100)   PICTURE '9999.99'
  @ ROW , 100 SAY IIF(NeCOST1=0,'N/A',(nEactCost1 /NeCOST1)*100)   PICTURE '9999.99'
  *B606632,1 ABD - [End]
  
  ROW = ROW+1

  @Row,00 SAY "Tax/Duty"
  @ ROW , 13 SAY necost3 *NFABTOTQTY                          PICTURE '9999999.999'
  laSub[2,1]=laSub[2,1]+(necost3*NFABTOTQTY)
  @ ROW , 24 SAY nelancost3*NFABTOTQTY                       PICTURE '9999999.999'
  laSub[2,2]=laSub[2,2]+(nelancost3*NFABTOTQTY)
  
  *B606632,1 ABD - Remark the next line and print the actual cost instead of the
  *B606632,1 ABD - The foreign currency value [Begin]
  *@ ROW , 35 SAY NACT_COST3*NFABTOTQTY                       PICTURE '9999999.999'
  *laSub[2,3]=laSub[2,3]+(NACT_COST3*NFABTOTQTY)
  *@ ROW , 46 SAY (NACT_COST3 -  nelancost3)*NFABTOTQTY       PICTURE '9999999.999'
  *lnAct_Lan= lnAct_Lan+(NACT_COST1 -  nelancost3)*NFABTOTQTY
  
  @ ROW , 35 SAY nEactCost3*NFABTOTQTY                       PICTURE '9999999.999'
  laSub[2,3]=laSub[2,3]+(nEactCost3*NFABTOTQTY)
  @ ROW , 46 SAY (nEactCost3 -  nelancost3)*NFABTOTQTY       PICTURE '9999999.999'
  lnAct_Lan= lnAct_Lan+(nEactCost1 -  nelancost3)*NFABTOTQTY
  *B606632,1 ABD - [End]
  
  @ ROW , 57 SAY (nelancost3 - necost3)*NFABTOTQTY            PICTURE '9999999.999'
  lnLan_Est=lnLan_Est+(nelancost2 - necost2)*NFABTOTQTY
  
  *B606632,1 ABD - Remark the next line and print the actual cost instead of the
  *B606632,1 ABD - The foreign currency value [Begin]
  *@ ROW , 68 SAY (NACT_COST3 - necost3)*NFABTOTQTY            PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(NACT_COST3 - necost3)*NFABTOTQTY
  *@ ROW , 81 SAY IIF(nelancost2=0,'N/A',(NACT_COST3 / nelancost3)*100)  PICTURE '9999.99'
  
  @ ROW , 68 SAY (nEactCost3 - necost3)*NFABTOTQTY            PICTURE '9999999.999'
  lnAct_Est=lnAct_Est+(nEactCost3 - necost3)*NFABTOTQTY
  @ ROW , 81 SAY IIF(nelancost2=0,'N/A',(nEactCost3 / nelancost3)*100)  PICTURE '9999.99'
  *B606632,1 ABD - [End]
  
  
  @ ROW , 91 SAY IIF(necost2=0,'N/A',(nelancost3 / necost3)*100)          PICTURE '9999.99'
  
  *B606632,1 ABD - Remark the next line and print the actual cost instead of the
  *B606632,1 ABD - The foreign currency value [Begin]
  *@ ROW , 100 SAY IIF(necost2=0,'N/A',(NACT_COST3 / necost3)*100)         PICTURE '9999.99'
  @ ROW , 100 SAY IIF(necost2=0,'N/A',(nEactCost3 / necost3)*100)         PICTURE '9999.99'
  *B606632,1 ABD - [End]
  
  ROW = ROW+1

  @Row,00 SAY "Freight"
  @ ROW , 13 SAY necost2*NFABTOTQTY                    PICTURE '9999999.999'
  laSub[3,1]=laSub[3,1]+(necost2*NFABTOTQTY)
  @ ROW , 24 SAY nelancost2*NFABTOTQTY                PICTURE '9999999.999'
  laSub[3,2]=laSub[3,2]+(nelancost2*NFABTOTQTY)
  
  *B606632,1 ABD - Remark the next line and print the actual cost instead of the
  *B606632,1 ABD - The foreign currency value [Begin]
  *@ ROW , 35 SAY NACT_COST2*NFABTOTQTY                PICTURE '9999999.999'
  *laSub[3,3]=laSub[3,3]+(NACT_COST2*NFABTOTQTY)
  *@ ROW , 46 SAY (NACT_COST2 -  nelancost2)*NFABTOTQTY        PICTURE '9999999.999'
  *lnAct_Lan= lnAct_Lan+(NACT_COST1 -  nelancost2)*NFABTOTQTY

  @ ROW , 35 SAY nEactCost2*NFABTOTQTY                PICTURE '9999999.999'
  laSub[3,3]=laSub[3,3]+(nEactCost2*NFABTOTQTY)
  @ ROW , 46 SAY (nEactCost2 -  nelancost2)*NFABTOTQTY        PICTURE '9999999.999'
  lnAct_Lan= lnAct_Lan+(nEactCost1 -  nelancost2)*NFABTOTQTY
  *B606632,1 ABD - [End]
  
  @ ROW , 57 SAY (nelancost2 - necost2)*NFABTOTQTY             PICTURE '9999999.999'
  lnLan_Est=lnLan_Est+(nelancost2 - necost2)*NFABTOTQTY
  
  *B606632,1 ABD - Remark the next line and print the actual cost instead of the
  *B606632,1 ABD - The foreign currency value [Begin]
  *@ ROW , 68 SAY (NACT_COST2 - necost2)*NFABTOTQTY             PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(NACT_COST2 - necost2)*NFABTOTQTY
  *@ ROW , 81 SAY IIF(nelancost3=0,'N/A',(NACT_COST2 / nelancost2)*100)  PICTURE '9999.99'
  @ ROW , 68 SAY (nEactCost2 - necost2)*NFABTOTQTY             PICTURE '9999999.999'
  lnAct_Est=lnAct_Est+(nEactCost2 - necost2)*NFABTOTQTY
  @ ROW , 81 SAY IIF(nelancost3=0,'N/A',(nEactCost2 / nelancost2)*100)  PICTURE '9999.99'
  *B606632,1 ABD - [End]
  
  @ ROW , 91 SAY IIF(necost3=0,'N/A',(nelancost2 / necost2)*100)          PICTURE '9999.99'
  
  *B606632,1 ABD - Remark the next line and print the actual cost instead of the
  *B606632,1 ABD - The foreign currency value [Begin]
  *@ ROW , 100 SAY IIF(necost3=0,'N/A',(NACT_COST2 / necost2)*100)         PICTURE '9999.99'
  @ ROW , 100 SAY IIF(necost3=0,'N/A',(nEactCost2 / necost2)*100)         PICTURE '9999.99'
  *B606632,1 ABD - [End]
  
  ROW = ROW+1

  @Row,00 SAY "Quota"
  @ ROW , 13 SAY necost4*NFABTOTQTY                    PICTURE '9999999.999'
  laSub[4,1]=laSub[4,1]+(necost4*NFABTOTQTY)
  @ ROW , 24 SAY nelancost4*NFABTOTQTY                PICTURE '9999999.999'
  laSub[4,2]=laSub[4,2]+(nelancost4*NFABTOTQTY)

  *B606632,1 ABD - Remark the next line and print the actual cost instead of the
  *B606632,1 ABD - The foreign currency value [Begin]
  *@ ROW , 35 SAY NACT_COST4*NFABTOTQTY                PICTURE '9999999.999'
  *laSub[4,3]=laSub[4,3]+(NACT_COST4*NFABTOTQTY)
  *@ ROW , 46 SAY (NACT_COST4 -  nelancost4)*NFABTOTQTY               PICTURE '9999999.999'
  *lnAct_Lan= lnAct_Lan+(NACT_COST1 -  nelancost4)*NFABTOTQTY

  @ ROW , 35 SAY nEactCost4*NFABTOTQTY                PICTURE '9999999.999'
  laSub[4,3]=laSub[4,3]+(nEactCost4*NFABTOTQTY)
  @ ROW , 46 SAY (nEactCost4 -  nelancost4)*NFABTOTQTY               PICTURE '9999999.999'
  lnAct_Lan= lnAct_Lan+(nEactCost1 -  nelancost4)*NFABTOTQTY
  *B606632,1 ABD - [End]
    
  @ ROW , 57 SAY (nelancost4 - necost4)*NFABTOTQTY                    PICTURE '9999999.999'
  lnLan_Est=lnLan_Est+(nelancost4 - necost4)*NFABTOTQTY
  
  *B606632,1 ABD - Remark the next line and print the actual cost instead of the
  *B606632,1 ABD - The foreign currency value [Begin]
  *@ ROW , 68 SAY (NACT_COST4 - necost4)*NFABTOTQTY                    PICTURE '9999999.999'
  *lnAct_Est=lnAct_Est+(NACT_COST4 - necost4)*NFABTOTQTY
  *@ ROW , 81 SAY IIF(nelancost4=0,'N/A',(NACT_COST4 / nelancost4)*100)  PICTURE '9999.99'
  @ ROW , 68 SAY (nEactCost4 - necost4)*NFABTOTQTY                    PICTURE '9999999.999'
  lnAct_Est=lnAct_Est+(nEactCost4 - necost4)*NFABTOTQTY
  @ ROW , 81 SAY IIF(nelancost4=0,'N/A',(nEactCost4 / nelancost4)*100)  PICTURE '9999.99'
  *B606632,1 ABD - [End]
  
  @ ROW , 91 SAY IIF(necost4=0,'N/A',(nelancost4 / necost4)*100)          PICTURE '9999.99'

  *B606632,1 ABD - Remark the next line and print the actual cost instead of the
  *B606632,1 ABD - The foreign currency value [Begin]
  *@ ROW , 100 SAY IIF(necost4=0,'N/A',(NACT_COST4 / necost4)*100)         PICTURE '9999.99'
  @ ROW , 100 SAY IIF(necost4=0,'N/A',(nEactCost4 / necost4)*100)         PICTURE '9999.99'
  *B606632,1 ABD - [End]
  
  ROW = ROW + 1 

  @ Row,015 SAY '---------     -------    ------  ---------  ---------  ---------  -------   -------  -------'
  ROW = ROW +1
  @ ROW , 03 SAY 'Total Cost'
  @ ROW , 13 SAY (NeCOST1+necost2+necost3+necost4)*NFABTOTQTY             PICTURE '9999999.999'
  lnEst = (NeCOST1+necost2+necost3+necost4)*NFABTOTQTY
  @ ROW , 24 SAY (nelancost1+nelancost2+nelancost3+nelancost4)*NFABTOTQTY    PICTURE '9999999.999'
  lnLand = lnLand + (nelancost1+nelancost2+nelancost3+nelancost4)*NFABTOTQTY
  
  *B606632,1 ABD - Remark the next line and print the actual cost instead of the
  *B606632,1 ABD - The foreign currency value [Begin]
  *@ ROW , 35 SAY (NACT_COST1+NACT_COST2+NACT_COST3+NACT_COST4)*NFABTOTQTY                    PICTURE '9999999.999'
  *lnAcT  = lnAcT + (NACT_COST1+NACT_COST2+NACT_COST3+NACT_COST4)*NFABTOTQTY
  @ ROW , 35 SAY (nEactCost1+nEactCost2+nEactCost3+nEactCost4)*NFABTOTQTY                    PICTURE '9999999.999'
  lnAcT  = lnAcT + (nEactCost1+nEactCost2+nEactCost3+nEactCost4)*NFABTOTQTY
  *B606632,1 ABD - [End]  
  
  @ ROW , 46 SAY lnAct -lnLand                                                PICTURE '9999999.999'
  @ ROW , 57 SAY lnLand -lnEst                                                PICTURE '9999999.999'
  @ ROW , 68 SAY lnAct - lnEst                                                PICTURE '9999999.999'
  @ ROW , 81 SAY  IIF(lnLand=0,'N/A',(lnAcT / lnLand)*100)                 PICTURE '9999.99'
  @ ROW , 91 SAY  IIF(lnEst=0,'N/A',(lnLand / lnEst)*100)                  PICTURE '9999.99'
  @ ROW , 100 SAY IIF(lnEst=0,'N/A',(lnAcT / lnEst)*100)                  PICTURE '9999.99'
  ROW = ROW + 1

  @ Row,015 SAY '=========     =======    ======  =========  =========  =========  =======   =======  ======='

  STORE 0 TO lnAct_Lan , lnLan_Est , lnAct_Est ,lnAct ,lnLand ,lnEst

  XTotal(1,1)=XTotal(1,1)+NFABTOTQTY
  XTotal(1,2)=XTotal(1,2)+(NFABTOTQTY*Fabric->CONV)
  XTotal(1,3)=XTotal(1,3)+(NFABTOTQTY*nelancost1) 

  SELECT &lcWorkFile
  lnTOTqty = NFABTOTQTY
  SKIP
ENDDO
*------------------ END [MAIN REPORT] LOOP --------------------

Row=Row+2
IF ROW+8 >=53
  PAGENO = PAGENO+1
  DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
  IF !XDYELOT_S
    IF llIsEngland
      @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        REFERENCE  PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE œPRICE      AMOUNT'
    ELSE
      IF llIsCanada
        @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        REFERENCE  PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE  PRICE      AMOUNT'
      ELSE
        @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        REFERENCE  PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE $PRICE      AMOUNT'
      ENDIF
    ENDIF
  ELSE
    IF llIsEngland
      @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        DYELOT #   PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE œPRICE      AMOUNT'
    ELSE
      IF llIsCanada
        @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        DYELOT #   PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE PRICE      AMOUNT'
      ELSE
        @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        DYELOT #   PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE $PRICE      AMOUNT'
      ENDIF
    ENDIF
  ENDIF
  @ 07,00 SAY REPLICATE('=',130)
  ROW = 08
ENDIF
@ Row,00 SAY REPLICATE('=',130)
Row = Row+1
@ Row,000 SAY '* GRAND TOTAL *'
ROW = ROW+1
@ ROW , 03 SAY "Price"
@ ROW , 13 SAY laFinal[1,1]                              PICTURE '9999999.999'
@ ROW , 24 SAY laFinal[1,2]                              PICTURE '9999999.999'
@ ROW , 35 SAY laFinal[1,3]                              PICTURE '9999999.999'
@ ROW , 46 SAY laFinal[1,3]-laFinal[1,2]                 PICTURE '9999999.999'
lnAct_Lan= lnAct_Lan+(laAct[1] -  lalan[1])
@ ROW , 57 SAY laFinal[1,2] -laFinal[1,1]                PICTURE '9999999.999'
lnLan_Est=lnLan_Est+(lalan[1] - laEst[1])
@ ROW , 68 SAY laFinal[1,3] -laFinal[1,1]                PICTURE '9999999.999'
lnAct_Est=lnAct_Est+(laAct[1] - laEst[1])

@ ROW , 81 SAY IIF(laFinal[1,2]=0,'N/A',(laFinal[1,3] / laFinal[1,2])*100)     PICTURE '9999.99'
@ ROW , 91 SAY IIF(laFinal[1,1]=0,'N/A',(laFinal[1,2] / laFinal[1,1])*100)     PICTURE '9999.99'
@ ROW , 100 SAY IIF(laFinal[1,1]=0,'N/A',(laFinal[1,3] / laFinal[1,1])*100)    PICTURE '9999.99'

ROW = ROW + 1
@ ROW , 03 SAY "Tax/Duty"
@ ROW , 13 SAY laFinal[2,1]                              PICTURE '9999999.999'
@ ROW , 24 SAY laFinal[2,2]                              PICTURE '9999999.999'
@ ROW , 35 SAY laFinal[2,3]                              PICTURE '9999999.999'

@ ROW , 46 SAY laFinal[2,3]-laFinal[2,2]                 PICTURE '9999999.999'
lnAct_Lan= lnAct_Lan+(laAct[2] -  lalan[2])
@ ROW , 57 SAY laFinal[2,2] -laFinal[2,1]                PICTURE '9999999.999'
lnLan_Est=lnLan_Est+(lalan[2] - laEst[2])
@ ROW , 68 SAY laFinal[2,3] -laFinal[2,1]                PICTURE '9999999.999'
lnAct_Est=lnAct_Est+(laAct[2] - laEst[2])

@ ROW , 81 SAY IIF(laFinal[2,2]=0,'N/A',(laFinal[2,3] / laFinal[2,2])*100)      PICTURE '9999.99'
@ ROW , 91 SAY IIF(laFinal[2,1]=0,'N/A',(laFinal[2,2] / laFinal[2,1])*100)      PICTURE '9999.99'
@ ROW , 100 SAY IIF(laFinal[2,1]=0,'N/A',(laFinal[2,3] / laFinal[2,1])*100)     PICTURE '9999.99'

ROW= ROW+1
@ ROW , 03 SAY "Freight"
@ ROW , 13 SAY laFinal[3,1]                              PICTURE '9999999.999'
@ ROW , 24 SAY laFinal[3,2]                              PICTURE '9999999.999'
@ ROW , 35 SAY laFinal[3,3]                              PICTURE '9999999.999'

@ ROW , 46 SAY laFinal[3,3]-laFinal[3,2]                 PICTURE '9999999.999'
lnAct_Lan= lnAct_Lan+(laAct[3] -  lalan[3])
@ ROW , 57 SAY laFinal[3,2] -laFinal[3,1]                PICTURE '9999999.999'
lnLan_Est=lnLan_Est+(lalan[3] - laEst[3])
@ ROW , 68 SAY laFinal[3,3] -laFinal[3,1]                PICTURE '9999999.999'
lnAct_Est=lnAct_Est+(laAct[3] - laEst[3])

@ ROW , 81 SAY IIF(laFinal[3,2]=0,'N/A',(laFinal[3,3] / laFinal[3,2])*100)  PICTURE '9999.99'
@ ROW , 91 SAY IIF(laFinal[3,1]=0,'N/A',(laFinal[3,2] / laFinal[3,1])*100)  PICTURE '9999.99'
@ ROW , 100 SAY IIF(laFinal[3,1]=0,'N/A',(laFinal[3,3] / laFinal[3,1])*100) PICTURE '9999.99'

ROW= ROW+1
@ ROW , 03 SAY "Quota"
@ ROW , 13 SAY laFinal[4,1]                              PICTURE '9999999.999'
@ ROW , 24 SAY laFinal[4,2]                              PICTURE '9999999.999'
@ ROW , 35 SAY laFinal[4,3]                              PICTURE '9999999.999'
  
@ ROW , 46 SAY laFinal[4,3]-laFinal[4,2]                 PICTURE '9999999.999'
lnAct_Lan= lnAct_Lan+(laAct[4] -  lalan[4])
@ ROW , 57 SAY laFinal[4,2] -laFinal[4,1]                 PICTURE '9999999.999'
lnLan_Est=lnLan_Est+(lalan[4] - laEst[4])
@ ROW , 68 SAY laFinal[4,3] -laFinal[4,1]                 PICTURE '9999999.999'
lnAct_Est=lnAct_Est+(laAct[4] - laEst[4])
@ ROW , 81 SAY IIF(laFinal[4,2]=0,'N/A',(laFinal[4,3] / laFinal[4,2])*100)  PICTURE '9999.99'
@ ROW , 91 SAY IIF(laFinal[4,1]=0,'N/A',(laFinal[4,2] / laFinal[4,1])*100)  PICTURE '9999.99'
@ ROW , 100 SAY IIF(laFinal[4,1]=0,'N/A',(laFinal[4,3] / laFinal[4,1])*100) PICTURE '9999.99'
ROW = ROW + 1
@ ROW ,14 SAY '---------- ----------  --------- ----------  --------- ----------  -------   ------- --------'
ROW = ROW + 1

@ ROW , 03 SAY 'Total Cost'
@ ROW , 14 SAY laFinal[1,1]+laFinal[2,1]+laFinal[3,1]+laFinal[4,1]    PICTURE '999999.999'
lnFin1 =  laFinal[1,1]+laFinal[2,1]+laFinal[3,1]+laFinal[4,1]
 
lnEst = (NeCOST1+necost2+necost3+necost4)*lnTOTQTY
@ ROW , 25 SAY laFinal[1,2]+laFinal[2,2]+laFinal[3,2]+laFinal[4,2]    PICTURE '999999.999'
lnFin2 =  laFinal[1,2]+laFinal[2,2]+laFinal[3,2]+laFinal[4,2]

lnLand = (nelancost1+nelancost2+nelancost3+nelancost4)*lnTOTQTY
@ ROW , 36 SAY laFinal[1,3]+laFinal[2,3]+laFinal[3,3]+laFinal[4,3]    PICTURE '999999.999'
lnFin3 =laFinal[1,3]+laFinal[2,3]+laFinal[3,3]+laFinal[4,3]

*B606632,1 ABD - Remark the next line and print the actual cost instead of the
*B606632,1 ABD - The foreign currency value [Begin]
*lnAcT  = (NACT_COST1+NACT_COST2+NACT_COST3+NACT_COST4)*lnTOTQTY
lnAcT  = (nEactCost1+nEactCost2+nEactCost3+nEactCost4)*lnTOTQTY
*B606632,1 ABD - [End]

@ ROW , 46 SAY  lnFin3 - lnFin2                     PICTURE '9999999.999'
@ ROW , 57 SAY lnFin2 - lnFin1                      PICTURE '9999999.999'
@ ROW , 68 SAY lnFin3 - lnFin1                      PICTURE '9999999.999'

@ ROW , 81 SAY IIF(lnFin2=0,'N/A',(lnFin3 / lnFin2)*100)         PICTURE '9999.99'
@ ROW , 91 SAY IIF(lnFin1=0,'N/A',(lnFin2 / lnFin1)*100)         PICTURE '9999.99'
@ ROW , 100 SAY IIF(lnFin1=0,'N/A',(lnFin3 / lnFin1)*100)        PICTURE '9999.99'

ROW = ROW + 1
@ ROW ,14 SAY '========== ========== ==========  ========= ==========  =========  =======  ======== ======== '
Row = Row+1
@ Row,00 SAY REPLICATE('=',130)

*!*************************************************************
*! Name      : lfvVen
*! Developer : BASSEM RAFAAT 
*! Date      : 03/17/1999
*! Purpose   : Validate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVen()
*!*************************************************************

FUNCTION lfvVen
PRIVATE lcVenFld,lcVender,lcSelFile,lcApvTag

lcVenFld  = VARREAD()
lcVendor  = EVAL(lcVenFld)

lnSelFile =  SELECT(0)

SELECT APVENDOR

lcApvTag  = ORDER('APVENDOR')

SET ORDER TO TAG VENCODE IN APVENDOR

IF !EMPTY(lcVendor) .AND. ( '?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
  =gfApVnBrow(@lcVendor)
ENDIF

&lcVenFld = lcVendor

SET ORDER TO lcApvTag

SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfvPo
*! Developer : BASSEM RAFAAT 
*! Date      : 03/17/1999
*! Purpose   : Validate PURCHASE ORDER MATERIAL
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPo()
*!*************************************************************

FUNCTION lfvPo
PRIVATE lcPoFld, lcPomat,lnSelFile,lcPofTag

lcPoFld   = VARREAD()
lcPomat   = EVAL(lcPoFld)

lnSelFile = SELECT(0)

SELECT POFHDR

lcPofTag  = ORDER('POFHDR')

SET ORDER TO TAG POFHDR IN POFHDR

IF !EMPTY(lcPomat) .AND. ('?' $ lcPomat .OR. !SEEK('P'+lcPomat , 'POFHDR'))
  =POFBROW('P','',@lcPomat)
ENDIF

&lcPoFld  = lcPomat

SET ORDER TO lcPofTag

SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfvFab
*! Developer : BASSEM RAFAAT 
*! Date      : 03/17/1999
*! Purpose   : Validate the fabric item range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFab()
*!*************************************************************

FUNCTION lfvFab
PRIVATE lcFabFld,lcFabric,lnSelFile,lcFabTag

lcFabFld  = VARREAD()
lcFabric  = EVAL(lcFabFld)

lnSelFile = SELECT(0)

SELECT FABRIC

lcFabTag  = ORDER('FABRIC')

SET ORDER TO TAG FABRIC IN FABRIC

IF !EMPTY(lcFabric)  .AND. ('?' $  lcFabric .OR. !SEEK(lcFabric , 'FABRIC')) 
  =FABROW (@lcFabric,'*')
ENDIF

&lcFabFld  = lcFabric

SET ORDER TO lcFabTag

SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfvWare
*! Developer : BASSEM RAFAAT 
*! Date      : 03/17/1999
*! Purpose   : Validate the warehous
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvWare()
*!*************************************************************

FUNCTION lfvWare
PRIVATE lcWarFld,lcWarhous,lnSelFile,lcWareTag

lcWarFld   = VARREAD()
lcWareCode = EVAL(lcWarFld)

lnSelFile = SELECT(0)

SELECT WAREHOUS

lcWareTag = ORDER('WAREHOUS') 

SET ORDER TO TAG WAREHOUS IN WAREHOUS

IF !EMPTY(lcWareCode) .AND. ('?' $ lcWareCode .OR. !SEEK(lcWareCode , 'WAREHOUS'))
  lcWareCode =gfBrowWare(.T.)  
ENDIF

&lcWarFld = lcWareCode

SET ORDER TO lcWareTag

SELECT (lnSelFile)

*!*************************************************************
*! Name      : lfSRVPo
*! Developer : BASSEM RAFAAT (BWA) 
*! Date      : 03/09/1999
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
*! Example   : =lfSRVPo()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVPo
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO Vencode IN APVENDOR
    SELECT POFHDR
    SET ORDER TO TAG POFHDR
    *B603186,1 (Begin) Restrict browse to PO or Return as the user selects
    IF lcRpPrntBy <> 'A'
      SET KEY TO IIF(lcRpPrntBy='P','P','R'))
    ENDIF
    *B603186,1 (End)
    SET RELATION TO VENDOR INTO APVENDOR
    GO TOP IN POFHDR
  CASE lcParm = 'R'  && Reset code
    SELECT POFHDR
    SET RELATION TO 
    *B603186,1 (Begin) Release restrictions.
    SET KEY TO
    *B603186,1 (End)
ENDCASE
*-- end of lfSRVPo
*!*************************************************************
*! Name      : lfSRVFab
*! Developer : BASSEM RAFAAT (BWA) 
*! Date      : 03/09/1999
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
    *llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : BASSEM RAFAAT (BWA) 
*! Date      : 03/09/1999
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
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)
*-- end of lfFabSum.


*!*************************************************************
*! Name      : lfRollData
*! Developer : Adel Mohammed El Gazaar (ADEL)
*! Date      : 02/05/2000
*! Purpose   : Print report by roll.
*!Refer to   : C101767,1
*!*************************************************************
FUNCTION lfRollData

*-- Set needed realtions to locate for lcRpExp 
=lfSetRel('POFLN')
LOCATE ALL FOR &lcRpExp AND POFLN.TRANCD = '2'
IF !FOUND()
   RETURN .F.
ELSE
  IF !USED(lcRollTmp)
    *-- Create temp file and index it.
    =lfCreate()
  ENDIF
  IF USED(lcRollTmp) AND RECCOUNT(lcRollTmp) > 0
    SELECT (lcRollTmp)
    ZAP
  ENDIF
  *--Collect data.
  =lfCollData()
  *-- Start printing.
  =lfPrintRep()
  RETURN
ENDIF

*!*************************************************************
*! Name      : lfSetRel
*! Developer : Adel Mohammed El Gazaar (ADEL)
*! Date      : 02/05/2000
*! Purpose   : Set needed relations
*!Refer to   : C101767,1
*!*************************************************************
FUNCTION lfSetRel

PARAMETERS lcAlias

*-- If the etmp file does not exist as it enters printing without data.
IF !USED(lcAlias)
  RETURN .F.
ENDIF
SELECT (lcAlias)
SET FILTER TO
SET RELATION TO

SET RELATION TO "P"+POMAT INTO POFHDR
SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE
IF lcAlias = 'POFLN'
  SET RELATION TO CWARECODE INTO WAREHOUS ADDITIVE
ENDIF  


*!*************************************************************
*! Name      : lfCreate
*! Developer : Adel Mohammed El Gazaar (ADEL)
*! Date      : 02/05/2000
*! Purpose   : Create temp file.
*!Refer to   : C101767,1
*!*************************************************************
FUNCTION lfCreate

CREATE TABLE (gcWorkDir+lcRollTmp) (cType C(1), POMAT C(6),TranCd C(1), Vendor C(8), Fabric C(7), Color C(6), Desc C(20), Pattern C(10),;
                                    Width C(6), DyeLot C(10), RollID C(20),Qty N(12,3),Uom C(3))
IF !EMPTY(lcSortFld)
  INDEX ON &lcSortFld TAG &lcRollTmp
ENDIF

*!*************************************************************
*! Name      : lfCollData
*! Developer : Adel Mohammed El Gazaar (ADEL)
*! Date      : 02/05/2000
*! Purpose   : Collect data for rolls.
*!Refer to   : C101767,1
*!*************************************************************
FUNCTION lfCollData

*--Open Rolls file
=gfOpenFile(gcDataDir+'ROLLS',gcDataDir+'SESSION','SH')
SELECT POFLN
lcWidth = ''
*:E301816,1 ABD - Add an option to the reciept log report to print 
*:E301816,1 ABD - Rolls that have zero qty (Y/N). [Begin]
lcRolExp = IIF(llRpRolZer ,'.T.','Rolls.nQtyBal > 0')
*:E301816,1 ABD - [End]
SCAN FOR &lcRpExp
  *-- Get the width from  TRANCD = '1' as we cann't enter it when recieving.
  IF TRANCD = '1'
    lcWidth = WIDTH
  ENDIF
  IF TRANCD = '2' && FOR RECEIVING ONLY
    *-- If the fabric/color tracks rolls,get them.
    IF FABRIC.lTrkRolls AND SEEK(cRSESSION+Fabric+Color+cWareCode+Dyelot,'ROLLS')
      lcKey = cRSESSION+Fabric+Color+cWareCode+Dyelot
      SELECT ROLLS
      *:E301816,1 ABD - Add an option to the reciept log report to print 
      *:E301816,1 ABD - Rolls that have zero qty (Y/N). [Begin]
      *SCAN REST WHILE csession+crollitem+color+cwarecode+dyelot+crollid = lcKey
      SCAN REST WHILE csession+crollitem+color+cwarecode+dyelot+crollid = lcKey ;
        For &lcRolExp
        *:E301816,1 ABD - [End]
        
        INSERT INTO (lcRollTmp) (cType,POMAT,TranCd,Vendor,Fabric,Color,Desc,Pattern,Width,DyeLot,RollID,Qty,Uom);
                       VALUES   (POFLN.cMatType,POFLN.PoMat,POFLN.TranCd,POFLN.Vendor,POFLN.Fabric,;
                                 POFLN.Color,Fabric.Desc,POFLN.Pattern,lcWidth,POFLN.Dyelot,Rolls.cRollID,Rolls.nQtyBal,Fabric.UomUse)
      ENDSCAN
    ELSE
      INSERT INTO (lcRollTmp) (cType,POMAT,TranCd,Vendor,Fabric,Color,Desc,Pattern,Width,DyeLot,RollID,Qty,Uom);
                     VALUES   (POFLN.cMatType,POFLN.PoMat,POFLN.TranCd,POFLN.Vendor,POFLN.Fabric,;
                               POFLN.Color,Fabric.Desc,POFLN.Pattern,lcWidth,POFLN.Dyelot,SPACE(20),POFLN.nFabTotQty,Fabric.UomUse)
    ENDIF                   
  ENdIF  
ENDSCAN

*!*************************************************************
*! Name      : lfPrintRep
*! Developer : Adel Mohammed El Gazaar (ADEL)
*! Date      : 02/05/2000
*! Purpose   : Print report by roll.
*!Refer to   : C101767,1
*!*************************************************************
FUNCTION lfPrintRep

SELECT POFLN
SET RELATION TO
*--Get the company address
=gfOpenFile(gcSysHome+'syccomp',gcSysHome+'Ccomp_id','SH')
=SEEK(gcAct_Comp,'SycComp')
*--Set needed realtions
IF !lfSetRel(lcRollTmp)
  *--No records to display.
  =gfModalGen('TRM00052B00000','DIALOG')  
  RETURN
ENDIF
*--Start printing.
lcRepName = 'MAPOROLL'
DO gfDispRe WITH EVAL('lcRepName')
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lfRolCnt
*! Developer : Adel Mohammed El Gazaar (ADEL)
*! Date      : 02/05/2000
*! Purpose   : Count no of rolls.
*!Refer to   : C101767,1
*!*************************************************************
FUNCTION lfRolCnt

lnRollNo = lnRollNo +IIF(Fabric.lTrkRolls,1,0)
RETURN ''

*!*************************************************************
*! Name      : lfFillVars
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/23/2000
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfFillVars()
*!*************************************************************
*:B603186,1
*!*************************************************************
FUNCTION lfFillVars

DIMENSION laSetUps[7,2]
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


*!*************************************************************
*! Name      : lfvCurr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/23/2000
*! Purpose   : Validate currency browse in OG.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfcurrbrow()
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfvCurr()
*!*************************************************************
*:B603186,1
*!*************************************************************
FUNCTION lfvCurr
PRIVATE lcVar , lcObj , laTemp,lcCurTag

lcCurFld = VARREAD()

lcCurVal = &lcCurFld

lcCurTag = ORDER('SYCCURR')
SET ORDER TO Ccurrcode IN SYCCURR

IF !EMPTY(lcCurVal) AND ('?' $ lcCurVal OR !SEEK(lcCurVal , 'SYCCURR'))
  = gfcurrbrow(@lcCurVal)
ENDIF  
SET ORDER TO lcCurTag IN SYCCURR
&lcCurFld = lcCurVal

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/23/2000
*! Purpose   : Activate currency display screen to get user 
*!           : selection for currencies.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfRepCur()
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************
*:B603186,1
*!*************************************************************
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*-- end of lfvCurDisp.

*!*************************************************************
*! Name      : lpMulCurrHdr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/30/2000
*! Purpose   : Print the report header for  multi Currency.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : DO lpMulCurrHdr
*!*************************************************************
*:B603186,1
*!*************************************************************
PROCEDURE lpMulCurrHdr

IF !XDYELOT_S
  IF llIsEngland
    @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        REFERENCE  PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE  PRICE      AMOUNT'
  ELSE
    IF llIsCanada
      @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        REFERENCE  PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE  PRICE      AMOUNT'
    ELSE
      @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        REFERENCE  PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE  PRICE      AMOUNT'
    ENDIF
  ENDIF
ELSE
  IF llIsEngland
    @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        DYELOT #   PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE  PRICE      AMOUNT'
  ELSE
    IF llIsCanada
      @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        DYELOT #   PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE PRICE      AMOUNT'
    ELSE
      @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.        DYELOT #   PATTERN    WIDTH       TOTQTY BUY  TOTQTY USE  PRICE      AMOUNT'
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfCalAmnts
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/31/2000
*! Purpose   : Calculate amount in selected currencies. 
*!           : selection for currencies.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfAmntDisp()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfCalAmnts()
*!*************************************************************
*:B603186,1
*!*************************************************************
FUNCTION lfCalAmnts

PRIVATE lnRetrnVal
lnRetrnVal = 0
*-- If Print in forign currency.
IF lcRpCurr = "F"
  lnRetrnVal = nLan_Cost1
ELSE
  *-- If Print in original base currency.
  IF lcRpCurr = "O"
    lnRetrnVal = nelancost1
  ELSE  && print By Date or calculated now.
    IF (POfHDR.CPRICECUR= gcBaseCurr) OR (nLan_Cost1 = 0)
      lnRetrnVal = nLan_Cost1
    ELSE
      *B603891,1 ABD 09/28/2000 Add new parameter to detect which Field we 
      *B603891,1 ABD 09/28/2000 Get currency values from it. [Begin]      
      *lnRetrnVal = gfAmntDisp(nLan_Cost1,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"POFHDR")
      lnRetrnVal = gfAmntDisp(nLan_Cost1,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"POFHDR","CPRICECUR")
      *B603891,1 ABD [End]
    ENDIF  
  ENDIF
ENDIF
*-- End If Print in forign currency.
RETURN lnRetrnVal
*--end of lfCalAmnts.

*!*************************************************************
*! Name      : lfvDetCost
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/31/2000
*! Purpose   : Validate Detail Costing Option in OG
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfvDetCost()
*!*************************************************************
*:B603186,1
*!*************************************************************
FUNCTION lfvDetCost

CLEAR READ

