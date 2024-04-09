*:***************************************************************************
*: Program file  : SRREPCOM
*: Program desc. : Sales Representative commission statement Report
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Sales Representative (SR )
*: Developer     : Mohamed Atia Badran  (MAB)
*:***************************************************************************
*: Calls : 
*:    Procedures : ENDREPORT
*:    Functions  : gfModalGen,gfGetAdr,lfwRepWhen,lfwOldVal,lfvRepCode,lfvDate
*:               : lfFltState,lfAdrShift
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SRREPCOM
*:***************************************************************************
*: This Report Program is due to E300926 ...
*: Modifications:
*:E301424,1 ABD  05/31/2000 Report support multi currency format.
*:E301424,1 ABD             (Header,variables and this code)
*:B603707,1 BWA 08/23/2000 Fix the bug of the voided amount must get with -ve not +ve.
*:B603895,1 ADEL 09/14/2000 Fix the bug of printing blank pages.
*:B603914,1 ADEL 09/21/2000 Fix the bug of uniting some pages without making
*:B603914,1                 any footers or headers.
*:B603918,1 MHM 10/18/2000 Fix bug in date range. the report was not consider it before
*:B603713,8 MHM 11/09/2000 increase the length of the fields (amount)
*:B803818,1 MHM 11/15/2000 Fix bug in pay date range. 
*:B604494,1 MHM 05/21/2001 Fix bug of Payment Date for the Manual case was not printing in proper range
*:B607357,1 ABD 06/17/2003 Salesrep commission statement reporting incorrect Balance.
*:***************************************************************************
*
*E301424,1 ABD- Declaration variables. [Begin]
lcCurrtCur = ''
ROW        = 23
lcRepComm  = gfTempName() && variable to hold Temp. index on REPCOMM FILE if company multi curr.
BEGBAL     = 0
lcFilter = ".T."
*E301424,1 ABD- End Declaration variables.

*-------- Assiging 2.6 variables with its eq. in 2.7 OG  [begin ] --------
*-- Eval. From and To dates. [begin] 
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'REPCOMM.DATE'),1)
LDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,;
            ATC('|',laOGFxFlt[lnDatePos,6])-1))

HDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],;
            ATC('|',laOGFxFlt[lnDatePos,6])+1))
*-- Eval. From and To dates. [end] 

*-- Eval. From and To Pay dates. [begin] 
lnPayDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'REPCOMM.PAYDATE'),1)
XLPAYDATE  = CTOD(SUBSTR(laOGFxFlt[lnPayDatePos,6],1,;
                  ATC('|',laOGFxFlt[lnPayDatePos,6])-1))
XHPAYDATE  = CTOD(SUBSTR(laOGFxFlt[lnPayDatePos,6],;
                  ATC('|',laOGFxFlt[lnPayDatePos,6])+1))
*-- Eval. From and To Pay dates. [end] 

TODAY      = DATE()

*-------------------------------
* SETUP HEADINGS
*-------------------------------
HLINE1 = TRIM(laCompAdd[1])
HLINE2 = TRIM(laCompAdd[2])
HLINE3 = TRIM(laCompAdd[3])
HLINE4 = TRIM(laCompAdd[4])
HLINE5 = TRIM(laCompAdd[5])

DCA = lcRpBal

*-- Eval. sales rep. [begin] 
lnRepPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SALESREP.REPCODE'),1)
LSALESREP = ALLTRIM(SUBSTR(laOGFxFlt[lnRepPos,6],1,;
            ATC('|',laOGFxFlt[lnRepPos,6])-1))
HSALESREP = ALLTRIM(SUBSTR(laOGFxFlt[lnRepPos,6],;
            ATC('|',laOGFxFlt[lnRepPos,6])+1))

*-- if your salesrep between codes data like ([   ] - [XXX]).
IF EMPTY(LSALESREP) AND !EMPTY(HSALESREP)
  SELECT SALESREP
  GO TOP
  LSALESREP = ALLTRIM(REPCODE)
ENDIF  && end if your salesrep between codes data like ([   ] - [XXX]).

*-- if your salesrep between codes data like ([XXX] - [   ]).
IF !EMPTY(LSALESREP) AND EMPTY(HSALESREP)
  SELECT SALESREP
  GO BOTTOM
  HSALESREP = ALLTRIM(REPCODE)
ENDIF  && end if your salesrep between codes data like ([XXX] - [   ]).

GO TOP IN SALESREP
*-- Eval. sales rep. [end] 

*------ Collecting Salesrep data from salesrep file  [Begin ] --------
WAIT WINDOW 'Locating records in file....' TIMEOUT 1

XFILTER = ALLTRIM("REPCODE = '' " + IIF(EMPTY(LSALESREP),"" ,;
          "AND BETWEEN(REPCODE,LSALESREP,HSALESREP) ") +;
          IIF(DCA = 'A',"","AND BALANCE " + IIF(DCA = 'C',"< 0","> 0")))

*E301424,1 ABD- Support multi currency format and add currecy to filter. [begin]
IF llMultCurr 
  lcCurFiltr     = laOGFxFlt[lnCurrPos,6]
  IF lnCurrPos     > 0
    lnCurrPos1   = AT('AND' , SUBSTR(lcRpExp,lnCurrPos))
    IF lnCurrPos1  > 0
       lcFilter    =  IIF(EMPTY(lcCurFiltr),"" ,SUBSTR(lcRpExp ,lnCurrPos, lnCurrPos1-1))
    ELSE
      lnCurrPos1 = LEN(lcRpExp)
      lcFilter    = IIF(EMPTY(lcCurFiltr), "",SUBSTR(lcRpExp ,lnCurrPos, lnCurrPos1))
    ENDIF
  ENDIF  
ENDIF  
*E301424,1 ABD- [End]

*:B803818,1 MHM 11/15/2000 Fix bug in pay date range. {no need for this part}[start]
*:B603918,1 MHM 10/18/2000 [start] add filter in case of not multi currency
*lcExtract = SUBSTR(lcRpExp,AT('BETWEEN(DTOS(REPCOMM.DATE)' , LCRPEXP))
*-- If it is last expresion 
*IF EMPTY(SUBSTR(lcExtract,1,AT('AND' , lcExtract)-1))
*  lcDatFlt = lcExtract
*ELSE  
*  lcDatFlt = SUBSTR(lcExtract,1,AT('AND' , lcExtract)-1)
*ENDIF
*lcFilter  = lcFilter  + IIF(EMPTY(lcFilter) ,  "", " AND " )   + lcDatFlt 
*:B603918,1 MHM 10/18/2000 [end]
*:B803818,1 MHM 11/15/2000 Fix bug in pay date range. {no need for this part}[end]

SELECT SALESREP
LOCATE ALL FOR &XFILTER
IF EOF()
  =gfModalGen(.f.,.f.,.f.,.f.,'No Statements Selected...!')  
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SET FILTER TO &XFILTER
GO TOP
*------ Collecting Salesrep data from salesrep file  [End ] --------

*E301424,1 ABD - set filter on RepComm. [Begin]
SELECT REPCOMM

*E301424,1 ABD Make index on repCom File If Company Multi Currency .[Begin]
IF llMultCurr
  INDEX ON REPCODE+CCURRCODE+DTOS(DATE)+TRAN+TRANTYPE TAG (lcRepComm) OF;
                                           (gcWorkDir+lcRepComm+'.CDX')
ENDIF
SET FILTER To &lcFilter
*E301424,1 ABD - [End]


*-- Intializing print values [Begin]
NEXTREP  = .T.
THRUDATE = DTOC(LDATE-1)
BEGBAL   = 0
RUNBAL   = 0
llPrnt   =.F.
PAGENO   = 0

*-- Intializing print values [End]

* INITIALIZE PRINTER
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
*----------------
* PRINTING MAIN LOOP [Begin]
*----------------
DO WHILE .T.
  SELECT SALESREP
    IF EOF()
    EXIT
  ENDIF

  XSALESREP = REPCODE
  XNAME     = NAME
  XPAY_TYPE = PAY_TYPE     && 'M' FOR MANUAL  'R' FOR RUNNING BALANCE

  IF !EMPTY(HSALESREP) AND (REPCODE > HSALESREP)
    EXIT
  ENDIF

  WAIT WINDOW 'Collecting statements for sales representative ' + XSALESREP NOWAIT

  *---------------------------------------------------------
  * Get 1st record in financial history file and initialize
  * work totals.
  *---------------------------------------------------------

  *-- must add a function to accumlate the beginning balance for ever
  *-- sales rep - Curruncy.

  IF NEXTREP .AND. XPAY_TYPE <> 'M'
    SELECT REPCOMM
    SEEK XSALESREP
    IF EOF() .OR. DATE > HDATE
      SELECT SALESREP
      SKIP
      LOOP
    ENDIF
    PAGENO = 0
    BEGBAL = 0.00
 
    *E301424,1 ABD- Save the current currency. [Begin]
    lcCurrtCur = cCurrCode 
    *E301424,1 ABD- [End] 

    *E301424,1 ABD- Remark the next lines and sum only when not multi 
    *E301424,1 ABD- Currency and make it function to accumalate every
    *E301424,1 ABD- beginning amount for multi curency . [begin]
    *B603895,1 (Begin) Save the record to get it back.
    *=lfBgBlanc()      
    *:B803818,1 MHM 11/15/2000 Fix bug in pay date range.[start]
    *lcKey = EVAL(Key())
    =lfBgBlanc()
    *=SEEK(lcKey)
    *:B803818,1 MHM 11/15/2000 Fix bug in pay date range.[end]
    *B603895,1 (End)
    *SUM REST AMOUNT TO BEGBAL WHILE REPCODE=XSALESREP .AND. DATE<LDATE
    *E301424,1 ABD- [End]

    IF REPCODE <> XSALESREP
      SELECT SALESREP
      SKIP
      LOOP
    ENDIF
    RUNBAL = BEGBAL
  ENDIF

  IF NEXTREP .AND. XPAY_TYPE = 'M'
    SELECT REPCOMM
    SEEK XSALESREP
    IF EOF()
      SELECT SALESREP
      SKIP
      LOOP
    ENDIF
    PAGENO = 0
    RUNBAL = 0
    LOCATE REST FOR STATUS='P' .AND.  BETWEEN( PAYDATE , XLPAYDATE , XHPAYDATE )

    IF REPCODE<> XSALESREP
      SELECT SALESREP
      SKIP
      LOOP
    ENDIF
  ENDIF

  llPrnt=.T.    && Rise print flag
  NEXTREP = .F.

  *E301424,1 ABD- Remark the next lines and make it function to call it if company 
  *E301424,1 ABD- Multi currency to print new page if currency change. [Begin]
  = lfPageHder()
  *@ 00,01 SAY ' '
  *XCOL=CENTER(1,79,HLINE1)
  *@ 01,XCOL SAY HLINE1
  *XCOL=CENTER(1,79,HLINE2)
  *@ 02,XCOL SAY HLINE2
  *XCOL=CENTER(1,79,HLINE3)
  *@ 03,XCOL SAY HLINE3
  *XCOL=CENTER(1,79,HLINE4)
  *@ 04,XCOL SAY HLINE4
  *XCOL=CENTER(1,79,HLINE5)
  *@ 05,XCOL SAY HLINE5
  *SELECT SALESREP
  *-- Evaluate sales rep. address [begin]
  *laRepData[1] = REPCODE + '   ' + NAME
  *laRepData[2] = CADDRESS1
  *laRepData[3] = CADDRESS2
  *laRepData[4] = gfGetAdr('SALESREP' , '' , '' , '' , 4) + ' --- ' + ;
  *               gfGetAdr('SALESREP' , '' , '' , '' , 3)
  *laRepData[5] = IIF(EMPTY(Phone),'',TRANSFORM(Phone , gfPhoneTem()))
  *= lfAdrShift('laRepData')    && Shift SALESREP address if there is empty line.
  *-- Evaluate sales rep. address [end]
  *XTXT='C O M M I S S I O N S   S T A T E M E N T'
  *XCOL=CENTER(1,79,XTXT)
  *@ 07,XCOL SAY XTXT
  *IF XPAY_TYPE <> 'M'
  *  XTXT='PERIOD ENDING '+DTOC(HDATE)
  *  XCOL=CENTER(1,79,XTXT)
  *  @ 08,XCOL SAY XTXT
  *ENDIF
  *-- Print sales rep. address [begin]
  *@ 10,10 SAY laRepData[1]
  *@ 11,10 SAY laRepData[2]
  *@ 12,10 SAY laRepData[3]
  *@ 13,10 SAY laRepData[4]
  *@ 14,10 SAY laRepData[5]
  **-- Print sales rep. address [end]
  *PAGENO=PAGENO+1       &&Increment page number for printing new sales rep.
  *@ 17,50 SAY "PAGE: "
  *@ 17,57 SAY PAGENO     PICTURE '999'
  *0....+....1....+....2....+....3....+....4....+....5....+....6...+....7....+....
  *  DATE   TRAN#  .....  DESC .....    ORDER  REFERENCE      AMOUNT      BALANCE
  *MM/DD/YY 123456 12345678901234567890 123456 1234567890  1234567.99-  1234567.99-
  *0....+....1....+....2....+....3....+....4....+....5....+....6...+....7....+....
  *@ 20,00 SAY REPLICATE('-',80)
  *@ 21,02 SAY 'DATE     TRAN#  ..... DESC .....   ORDER  REFERENCE        AMOUNT     BALANCE'
  *@ 22,00 SAY REPLICATE('-',80)
  *SELECT REPCOMM
  * ROW = 23
  *E301424,1 ABD- [End]

  
  
  IF PAGENO=1 .AND. BEGBAL<>0 .AND. XPAY_TYPE<>'M'
    @ ROW,09 SAY 'ACCOUNT BALANCE AS OF &THRUDATE'
    *: B603713,8  MHM 11/09/2000 [START]
    *@ ROW,69 SAY BEGBAL   PICTURE '9999999.99'
    IF lcRpDeciml='Y'
      @ ROW,69 SAY BEGBAL   PICTURE '9999999.99'
    ELSE  
      @ ROW,69 SAY BEGBAL   PICTURE '9999999999'
    ENDIF
    *: B603713,8  MHM 11/09/2000 [START]
    ROW = ROW +1
  ENDIF

  ***
  ****** PRINT SALESREP ACTIVITY FOR RUNNING BALANCE SALESREPS
  ***
  SELE REPCOMM
  
  DO WHILE XPAY_TYPE<>'M'
    SELECT REPCOMM

    IF REPCODE<>XSALESREP .OR. DATE>HDATE
      NEXTREP=.T.
      EXIT
    ENDIF
    *E301424,1 ABD- Save the current currency. [Begin]
    lcCurrtCur = cCurrCode
    *E301424,1 ABD- [End]
    SCAN REST WHILE lcCurrtCur = cCurrCode .AND.  REPCODE = XSALESREP
      *:B803818,1 MHM 11/15/2000 Fix bug in pay date range. [start]
      IF DATE > HDATE
        EXIT
      ENDIF
      *:B803818,1 MHM 11/15/2000 Fix bug in pay date range. [end]
      *E301424,1 ABD- Remark next line accumulate beginning balance if not foriegn. [Begin]
      IF llMultCurr 
         lnAmount = IIF(nforamnt = 0 , AMOUNT , lfBaseAmt(nforamnt))

         *B603707,1 BWA 08/23/2000 Fix the bug of the voided amount must get with -ve not +ve.[START]
         *RUNBAL = RUNBAL + lnAmount
         RUNBAL = RUNBAL + IIF(AMOUNT > 0 , lnAmount , IIF(lnAmount < 0 , lnAmount , lnAmount*-1))
         *B603707,1 [END]

      ELSE
        RUNBAL = RUNBAL + AMOUNT
      ENDIF
      *E301424,1 ABD- [End]

      @ ROW,00 SAY DATE
      @ ROW,11 SAY TRAN
      @ ROW,18 SAY PADR(DESC,18)
      @ ROW,37 SAY ORDER
      @ ROW,44 SAY PADR(CUSTPO,13)
    
      *E301424,1 ABD- Remark next line and change print out amount [begin]
      *@ ROW,51 SAY AMOUNT             PICTURE '9999999.99'
      *@ ROW,69 SAY RUNBAL             PICTURE '9999999.99'
      IF llMultCurr 

        *B603707,1 BWA 08/23/2000 Fix the bug of the voided amount must get with -ve not +ve.[START]
        *@ ROW,51 SAY lnAmount           PICTURE '9999999.99'
         *: B603713,8  MHM 11/09/2000 [START]
         *@ ROW,51 SAY IIF(AMOUNT > 0 , lnAmount , IIF(lnAmount < 0 , lnAmount , lnAmount*-1))   PICTURE '9999999.99'
         IF lcRpDeciml='Y'
           @ ROW,51 SAY IIF(AMOUNT > 0 , lnAmount , IIF(lnAmount < 0 , lnAmount , lnAmount*-1))   PICTURE '9999999.99'
         ELSE
           @ ROW,51 SAY IIF(AMOUNT > 0 , lnAmount , IIF(lnAmount < 0 , lnAmount , lnAmount*-1))   PICTURE '9999999999'
         ENDIF
        *: B603713,8  MHM 11/09/2000 [END]
        *B603707,1 [END]
         *: B603713,8  MHM 11/09/2000 [START]
         *@ ROW,69 SAY RUNBAL             PICTURE '9999999.99'
         IF lcRpDeciml='Y'
           @ ROW,69 SAY RUNBAL             PICTURE '9999999.99'
         ELSE
           @ ROW,69 SAY RUNBAL             PICTURE '9999999999'
         ENDIF          
         *: B603713,8  MHM 11/09/2000 [END]
      ELSE
        *: B603713,8  MHM 11/09/2000 [START]
        *@ ROW,51 SAY AMOUNT             PICTURE '9999999.99'
        *@ ROW,69 SAY RUNBAL             PICTURE '9999999.99'
        IF lcRpDeciml='Y'
          @ ROW,51 SAY AMOUNT             PICTURE '9999999.99'
          @ ROW,69 SAY RUNBAL             PICTURE '9999999.99'
        ELSE
          @ ROW,51 SAY AMOUNT             PICTURE '9999999999'
          @ ROW,69 SAY RUNBAL             PICTURE '9999999999'
        ENDIF  
        *: B603713,8  MHM 11/09/2000 [END]
      ENDIF
      *E301424,1 ABD- [End]
    
      ROW = ROW+1
      IF !EMPTY(ACCOUNT) .AND. SEEK('M'+REPCOMM.ACCOUNT,'CUSTOMER')
        @ ROW,11 SAY CUSTOMER.BTNAME
        ROW = ROW+1
      ENDIF
      *E301424,1 ABD- remark the next line and scan will change the pointer. [Begin]
      *SELECT REPCOMM
      *SKIP
      *E301424,1 ABD- [End]

      *B800529,1 TAK 03/27/96 Added to check the date also.
      *E301424,1 ABD- remark the next line and print Page Foter if row >52 .[Begin]
      *IF REPCODE=XSALESREP .AND. DATE<=HDATE .AND. ROW >= 52
      *  EXIT
      *B603914,1 (Begin) If the date is out exit else check the row number.
      *IF DATE<=HDATE .AND. ROW >= 52
      IF DATE > HDATE
        EXIT
      ENDIF
      IF REPCODE=XSALESREP AND ROW >= 52
      *B603914,1 (End)
        = lfPageFotr()  && Function Print Page Foter .
        *B603895,1 (Begin) Print the header here (it also contains Row reset.)
        = lfPageHder()   && Function Print Page Header.
        lcCurrtCur = cCurrCode
        *B603895,1 (End)
        *E301424,1 ABD- [End]
      ENDIF
    ENDSCAN
    
    *E301424,1 ABD- Print new page if currency change & sales rep didn't change. [begin]
    IF !EOF() .AND. lcCurrtCur <> cCurrCode .AND. REPCODE=XSALESREP ; 
       .AND. lcRpCurr = "F"
      = lfPageFotr()    && Function Print Page Foter .
      = lfPageHder()    && Function Print Page Header.
      *-- Save the current currency.
      lcCurrtCur = cCurrCode
      *B603895,1 (Begin) Save the record to get it back.
      *=lfBgBlanc()      
      lcKey = EVAL(Key())
      =lfBgBlanc()
      =SEEK(lcKey)
      *B603895,1 (End)
      RUNBAL = BEGBAL
      IF BEGBAL<>0 
        @ ROW,09 SAY 'ACCOUNT BALANCE AS OF &THRUDATE'
        *: B603713,8  MHM 11/09/2000 [START]
        *@ ROW,69 SAY BEGBAL   PICTURE '9999999.99'
        IF lcRpDeciml='Y'
          @ ROW,69 SAY BEGBAL   PICTURE '9999999.99'
        ELSE
          @ ROW,69 SAY BEGBAL   PICTURE '9999999999'
        ENDIF
        *: B603713,8  MHM 11/09/2000 [START]
        ROW = ROW +1
      ENDIF
    ENDIF
    *E301424,1 ABD- [End]
  ENDDO
 
  *** END PRINT LOOP FOR RUNNING BALANCE SALESREPS
  ***
  ****** PRINT SALESREP ACTIVITY FOR MANUAL PAYMENT SALESREPS
  ***
  DO WHILE XPAY_TYPE = 'M'
  
    IF REPCODE<>XSALESREP
      NEXTREP=.T.
      EXIT
    ENDIF

    IF .NOT. ( STATUS='P' .AND. BETWEEN(PAYDATE,XLPAYDATE,XHPAYDATE))
      SKIP
      LOOP
    ENDIF

    SELECT REPCOMM

    *E301424,1 ABD- Save the current currency & Scan On the File. [Begin]
    lcCurrtCur = cCurrCode
    RUNBAL     = IIF(llMultCurr .AND. lcRpCurr = "F" ,0, RUNBAL )
    SCAN REST WHILE lcCurrtCur = cCurrCode .AND.  REPCODE = XSALESREP
      *B607357,1 ABD - Remark the next few lines that the program should loop on
      *B607357,1 ABD - Not paid record. [Begin]
      *:B604494,1 MHM 05/21/2001 [Start]
      *IF PAYDATE > XHPAYDATE OR PAYDATE < XLPAYDATE
      *  EXIT
      *ENDIF
      *:B604494,1 MHM 05/21/2001 [End]
      IF !( STATUS='P' .AND. BETWEEN(PAYDATE,XLPAYDATE,XHPAYDATE))
        LOOP
      ENDIF
      *B607357,1 ABD - [End]
  
      *E301424,1 ABD- [End]
      *E301424,1 ABD- Remark next line accumalte beginning balance if not foriegn. [Begin]
      *RUNBAL = RUNBAL + AMOUNT
      IF llMultCurr
        lnAmount = IIF(nforamnt = 0 , AMOUNT , lfBaseAmt(nforamnt))

        *B603707,1 BWA 08/23/2000 Fix the bug of the voided amount must get with -ve not +ve.[START]
        *RUNBAL = RUNBAL + lnAmount
        RUNBAL = RUNBAL + IIF(AMOUNT > 0 , lnAmount , IIF(lnAmount < 0 , lnAmount , lnAmount*-1))
        *B603707,1 [END]

      ELSE
        RUNBAL = RUNBAL + AMOUNT
      ENDIF
      *E301424,1 ABD- [End]

      @ ROW,00 SAY DATE
      @ ROW,11 SAY TRAN
      @ ROW,18 SAY PADR(DESC,18)
      @ ROW,37 SAY ORDER
      @ ROW,44 SAY PADR(CUSTPO,13)

      *E301424,1 ABD- Remark next line print beginning balance if not foriegn. [Begin]
      *@ ROW,51 SAY AMOUNT             PICTURE '9999999.99'
      *@ ROW,69 SAY RUNBAL             PICTURE '9999999.99'
      IF llMultCurr

        *B603707,1 BWA 08/23/2000 Fix the bug of the voided amount must get with -ve not +ve.[START]
        *@ ROW,51 SAY lnAmount           PICTURE '9999999.99'
        *: B603713,8  MHM 11/09/2000 [START]
         *@ ROW,51 SAY IIF(AMOUNT > 0 , lnAmount , IIF(lnAmount < 0 , lnAmount , lnAmount*-1)) PICTURE '9999999.99'
         IF lcRpDeciml='Y'
           @ ROW,51 SAY IIF(AMOUNT > 0 , lnAmount , IIF(lnAmount < 0 , lnAmount , lnAmount*-1)) PICTURE '9999999.99'
         ELSE
           @ ROW,51 SAY IIF(AMOUNT > 0 , lnAmount , IIF(lnAmount < 0 , lnAmount , lnAmount*-1)) PICTURE '9999999999'
         ENDIF
        *: B603713,8  MHM 11/09/2000 [END]
        *B603707,1 [END]
         *: B603713,8  MHM 11/09/2000 [START]
         *@ ROW,69 SAY RUNBAL             PICTURE '9999999.99'
         IF lcRpDeciml='Y'
           @ ROW,69 SAY RUNBAL             PICTURE '9999999.99'
         ELSE
           @ ROW,69 SAY RUNBAL             PICTURE '9999999999'
         ENDIF
         *: B603713,8  MHM 11/09/2000 [END]
      ELSE
        *--MHM
        *@ ROW,51 SAY AMOUNT             PICTURE '9999999.99'
        *@ ROW,69 SAY RUNBAL             PICTURE '9999999.99'
        IF lcRpDeciml='Y'
          @ ROW,51 SAY AMOUNT             PICTURE '9999999.99'
          @ ROW,69 SAY RUNBAL             PICTURE '9999999.99'
        ELSE
          @ ROW,51 SAY AMOUNT             PICTURE '9999999999'
          @ ROW,69 SAY RUNBAL             PICTURE '9999999999'
        ENDIF
        *--MHM
      ENDIF
      *E301424,1 ABD- [End]
      ROW = ROW+1
      IF !EMPTY(ACCOUNT) .AND. SEEK('M'+REPCOMM.ACCOUNT,'CUSTOMER')
        @ ROW,11 SAY CUSTOMER.BTNAME
        ROW = ROW+1
      ENDIF
      
      *E301424,1 ABD- remark the next line and scan will change the pointer. [Begin]
      *SELECT REPCOMM
      *SKIP
      *E301424,1 ABD- [End]
      
      *E301424,1 ABD- remark the next line and print Page Foter if row >52 .[Begin]
      *IF REPCODE=XSALESREP .AND. ROW >= 52
      *EXIT
      IF ROW >= 52
        = lfPageFotr()  && Function Print Page Foter .
        *B603895,1 (Begin) Print the header here (it also contains Row reset.)
        = lfPageHder()   && Function Print Page Header.
        lcCurrtCur = cCurrCode
        *B603895,1 (End)
      *E301424,1 ABD- [End]
      ENDIF
    ENDSCAN
    *E301424,1 ABD- Print new page if currency change & sales rep didn't change. [begin]
    IF !EOF() .AND. lcCurrtCur <> cCurrCode .AND. REPCODE=XSALESREP ; 
       .AND. lcRpCurr = "F"
      = lfPageFotr()   && Function Print Page Foter .
      = lfPageHder()   && Function Print Page Header.
      lcCurrtCur = cCurrCode
    ENDIF
    *E301424,1 ABD- [End]
  ENDDO
    
  *** END PRINT LOOP FOR RUNNING BALANCE SALESREPS
  *E301424,1 ABD-  Remark the next liens and make it in function . [Begin]
  = lfPageFotr() && Function Print Page Foter .
  *@ 53,00 SAY REPLICATE('-',80)
  *@ 54,02 SAY "PRINTED ON " + DTOC(TODAY)
  *E301424,1 ABD- Print Run Balance In case of not equal Zero [Begin]
  *@ 54,55 SAY 'BALANCE ->'
  *@ 54,55 SAY IIF(RUNBAL=0,'','BALANCE ->')
  *E301424,1 ABD- [End]
  *IF NEXTREP
  *  E301424,1 ABD- Print Run Balance In case of not equal Zero [Begin]
  *  @ 54,69 SAY RUNBAL PICTURE '9999999.99'
  *  @ 54,69 SAY IIF(RUNBAL=0,'',RUNBAL) PICTURE '9999999.99'
  *  E301424,1 ABD- [End]
  *ELSE
  *  @ 54,69 SAY 'CONTINUED'
  *ENDIF
  *@ 55,00 SAY REPLICATE('-',80)
  *IF NEXTREP
  *  SELECT SALESREP
  *  SKIP
  *ENDIF
  *B800529,1 TAK 03/27/96 Reset the row at the end.
  *@ 00,00 SAY ''   &&to eject the page.

ENDDO

IF llPrnt
  DO ENDREPORT
  *E301424,1 ABD - remove filter from RepComm. [Begin]
  SELECT REPCOMM
  SET FILTER To 
  *E301424,1 ABD - [End]
ELSE
  =gfModalGen(.f.,.f.,.f.,.f.,'There are no records to display...! ')  
ENDIF
SET DEVICE TO SCREEN

*E301424,1 ABD Close temp index that made on repcomm file. [begin]
IF llMultCurr
  SELECT REPCOMM
  CLOSE INDEXES
  ERASE(gcWorkDir+lcRepComm+'.CDX')
ENDIF
*E301424,1 ABD [End]

*----------------
* PRINTING MAIN LOOP [End]
*----------------
RETURN
*-- end of SRREPSH.PRG report code.


*------- Functions section ---------
*----------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/13/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfAdrShift
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
R_WIDTH    = 'W'
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'REPCOMM.DATE'),1)
IF EMPTY(laOGFxFlt[lnDatePos,6])
  laOGFxFlt[lnDatePos,6] = DTOC(DATE() - DAY(DATE()) - (DAY(DATE() - DAY(DATE()))-1))+;
                           '|'+DTOC(DATE() - DAY(DATE()))
ENDIF

lnPayDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'REPCOMM.PAYDATE'),1)

SELECT SALESREP
SET ORDER TO
LOCATE FOR REPCODE = '' AND PAY_TYPE = 'M'
IF FOUND()
  IF EMPTY(laOGFxFlt[lnPayDatePos,6])
    laOGFxFlt[lnPayDatePos,6] = DTOC(DATE()) + '|' + DTOC(DATE())
  ENDIF
  XANY_MANUAL = .T.
ELSE
  = lfFltState(lnPayDatePos,'D')  && Disable Pay Date.
  XANY_MANUAL = .F.
ENDIF  
SET ORDER TO SALESREP IN SALESREP  && To use it to validate REP# in option grid.

IF EMPTY(laCompAdd)
  PRIVATE lcAliasNow
  lcAliasNow = ALIAS()
  *-- Get company Address [begin].
  DECLARE laCompAdd[5],laRepData[5]

  STORE '' TO laCompAdd,laRepData

  *E301077,72 Open Syccomp File [Begin
  IF !USED('SYCCOMP')
    USE (gcSysHome+'SYCCOMP') ORDER TAG Ccomp_id IN 0 SHARED
  ENDIF
  *E301077,72 Open Syccomp File [End 
  
  SELECT SYCCOMP
  SEEK gcAct_Comp

  laCompAdd[1] = cCom_Name
  laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4) + ' --- ' + ;
                 gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[5] = IIF(EMPTY(cCom_Phon),"",TRANSFORM(cCom_Phon , gfPhoneTem()))
  
  = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  *-- Get company Address [end].
  SELECT (lcAliasNow)
ENDIF  

*E301424,1 ABD-  Evaluate Currency filter postion. [Begin]
IF llMultCurr
  lnCurrPos  = lfItmPos('REPCOMM.CCURRCODE')
ELSE
  lcRpCurr = "O"
ENDIF  
*E301424,1 ABD- Evaluate Currency filter postion. [End ]

*-- End of lfwRepWhen.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/13/1998
*! Purpose   : When function to get the Old value
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

*!*************************************************************
*! Name      : lfvRepCode
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/13/98
*! Purpose   : Validate Primary Sales Rep. in SALESREP file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Sales representative Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRepCode()
*!*************************************************************
FUNCTION lfvRepCode
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , 'SALESREP'))
  SELECT SALESREP
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "REPCODE   :R :H= 'Code' , "   +;
               "NAME      :R :H= 'Name' ,"    +;
               "cAddress6 :R :H= 'Country' ," +;
               "PHONE     :R :H= 'Phone' ,"   +;
               "BALANCE   :R :H= 'Balance' "
  
  lcFile_Ttl = "Sales Representative ..."
  = gfBrows('','REPCODE','laTemp')
    
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvRepCode.

*!*************************************************************
*! Name      : lfvDate
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/13/98
*! Purpose   : Validate Entered date [i.e. Not empty]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid [Period Option]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvDate()
*!*************************************************************
FUNCTION lfvDate
IF EMPTY(EVALUATE(SYS(18)))
  WAIT WINDOW 'You must fill period range...' NOWAIT
  lcCurrObj = SYS(18)
  &lcCurrObj = laOldVal
  SHOW GET lcCurrObj
  _CUROBJ = _CUROBJ
ENDIF
*-- end of lfvDate.

*!*************************************************************
*! Name      : lfFltState
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/15/98
*! Purpose   : Enable and disable selected objects.
*!*************************************************************
*! Called from : lfwRepWhen 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfOGShowGet
*!*************************************************************
*! Passed Parameters  : 1- Filter Object Number
*!                    : 2- 'E' for enable, 'D' for disable
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfFltState()
*!*************************************************************
FUNCTION lfFltState
PARAMETERS lnObjNum,lcObjState
PRIVATE lcFxFltPos,lnPos

IF lcObjState = 'D' AND !EMPTY(laOGFxFlt[lnObjNum,6])
  laOGFxFlt[lnObjNum,6] = ''
ENDIF  

lcFxFltPos = 'laOGFxFlt[' + ALLTRIM(STR(lnObjNum)) + ',6]'
lnPos = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,lcFxFltPos),1)

laOGObjCnt[lnPos] = (lcObjState = 'E')
= lfOGShowGet(lcFxFltPos)  && Enable / Disable Object .
*-- end of lfFltState.

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/15/1998
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : lfwRepWhen 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.


*!*************************************************************
*! Name      : lfClearRep
*: Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/2000
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
*
FUNCTION lfClearRep
*E301077,72 Close Syccomp File [Begin
IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF
*E301077,72 Close Syccomp File [End 


*E301424,1 ABD- Report support multi currency format close needed files.[begin]
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
*E301424,1 ABD -[End]


*-- End OF lfClearRep.
*!*************************************************************
*! Name      : lfFillVars
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/2000
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
*E301424,1
FUNCTION lfFillVars

*E301424,1 ABD- Report support multi currency format open needed files.[begin]
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
*!*************************************************************
*! Name      : lfItmPos
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*E301424,1
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End Of lfItmPos.
*!*************************************************************
*! Name      : lfBaseAmt
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/2000
*! Purpose   : Compute base amount
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfBaseAmt()
*!*************************************************************
*E301424,1
FUNCTION lfBaseAmt
PARAMETERS lnAmntCurr
PRIVATE lnBaseAmt

lnBaseAmt = lnAmntCurr
*-- if Multi currency and user want to print in base currency and 
*-- currency not the base currency.
IF llMultCurr AND lcRpCurr <> "F" lnBaseAmt <> 0
  lnBaseAmt = gfAmntDisp(lnBaseAmt,lcRpCurr,ldRpExDate,lcRpTmpNam)
ENDIF
RETURN lnBaseAmt

*-- End of lfBaseAmt.
*!*************************************************************
*! Name      : lfPageHder
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/2000
*! Purpose   : Print the page Header.
*!*************************************************************
*! Passed Parameters  : Non.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : = lfPageHder()
*!*************************************************************
*E301424,1
FUNCTION lfPageHder

@ 00,01 SAY ' '
XCOL=CENTER(1,79,HLINE1)
@ 01,XCOL SAY HLINE1
XCOL=CENTER(1,79,HLINE2)
@ 02,XCOL SAY HLINE2
XCOL=CENTER(1,79,HLINE3)
@ 03,XCOL SAY HLINE3
XCOL=CENTER(1,79,HLINE4)
@ 04,XCOL SAY HLINE4
XCOL=CENTER(1,79,HLINE5)
@ 05,XCOL SAY HLINE5

SELECT SALESREP
  
*-- Evaluate sales rep. address [begin]
laRepData[1] = REPCODE + '   ' + NAME
laRepData[2] = CADDRESS1
laRepData[3] = CADDRESS2
laRepData[4] = gfGetAdr('SALESREP' , '' , '' , '' , 4) + ' --- ' + ;
                gfGetAdr('SALESREP' , '' , '' , '' , 3)
laRepData[5] = IIF(EMPTY(Phone),'',TRANSFORM(Phone , gfPhoneTem()))

= lfAdrShift('laRepData')    && Shift SALESREP address if there is empty line.
*-- Evaluate sales rep. address [end]
XTXT='C O M M I S S I O N S   S T A T E M E N T'
XCOL=CENTER(1,79,XTXT)
@ 07,XCOL SAY XTXT
IF XPAY_TYPE <> 'M'
  XTXT='PERIOD ENDING '+DTOC(HDATE)
  XCOL=CENTER(1,79,XTXT)
  @ 08,XCOL SAY XTXT
ENDIF
*-- Print sales rep. address [begin]
@ 10,10 SAY laRepData[1]
@ 11,10 SAY laRepData[2]
@ 12,10 SAY laRepData[3]
@ 13,10 SAY laRepData[4]
@ 14,10 SAY laRepData[5]
*-- Print sales rep. address [end]

PAGENO=PAGENO+1       &&Increment page number for printing new sales rep.
@ 17,50 SAY "PAGE: "
@ 17,57 SAY PAGENO     PICTURE '999'

*0....+....1....+....2....+....3....+....4....+....5....+....6...+....7....+....
*  DATE   TRAN#  .....  DESC .....    ORDER  REFERENCE      AMOUNT      BALANCE
*MM/DD/YY 123456 12345678901234567890 123456 1234567890  1234567.99-  1234567.99-
*0....+....1....+....2....+....3....+....4....+....5....+....6...+....7....+....

@ 20,00 SAY REPLICATE('-',80)
@ 21,02 SAY 'DATE     TRAN#  ..... DESC .....   ORDER  REFERENCE        AMOUNT     BALANCE'
@ 22,00 SAY REPLICATE('-',80)

SELECT REPCOMM
ROW = 23

*-- End Of lfPageHder
*!*************************************************************
*! Name      : lfPageFotr
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/2000
*! Purpose   : Print the page Foter.
*!*************************************************************
*! Passed Parameters  : Non.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : = lfPageFotr()
*!*************************************************************
*E301424,1
FUNCTION lfPageFotr

@ 53,00 SAY REPLICATE('-',80)
@ 54,02 SAY "PRINTED ON " + DTOC(TODAY)

*E301424,1 ABD- Print Run Balance In case of not equal Zero [Begin]
*@ 54,55 SAY 'BALANCE ->'

IF llMultCurr
  @ 54,25 SAY 'Currency  '+IIF(lcRpCurr = "F"  ,lcCurrtCur,gcBaseCurr)
ENDIF

@ 54,55 SAY IIF(RUNBAL=0,'','BALANCE ->')
*E301424,1 ABD- [End]
IF ROW >= 52
  @ 54,69 SAY 'CONTINUED'
ELSE  
  *E301424,1 ABD- Print Run Balance In case of not equal Zero [Begin]
  *@ 54,69 SAY RUNBAL PICTURE '9999999.99'
  *--MHM
  *@ 54,69 SAY IIF(RUNBAL=0,'',RUNBAL) PICTURE '9999999.99'
  IF lcRpDeciml='Y'
    @ 54,69 SAY IIF(RUNBAL=0,'',RUNBAL) PICTURE '9999999.99'
  ELSE
    @ 54,69 SAY IIF(RUNBAL=0,'',RUNBAL) PICTURE '9999999999'
  ENDIF
  *--MHM
  *E301424,1 ABD- [End]
ENDIF
@ 55,00 SAY REPLICATE('-',80)
IF NEXTREP
  SELECT SALESREP
  SKIP
ENDIF
*B800529,1 TAK 03/27/96 Reset the row at the end.
@ 00,00 SAY ''   &&to eject the page.

*-- End Of lfPageFotr.
*!*************************************************************
*! Name      : lfBgBlanc
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 07/01/2000
*! Purpose   : Accumlate the Beginning Balane for evry sales rep
*!           : Deff. Currency.
*!*************************************************************
*! Passed Parameters  : Non.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : = lfBgBlanc()
*!*************************************************************
*E301424,1
FUNCTION lfBgBlanc

PAGENO = 0
BEGBAL = 0.00

*E301424,1 ABD- Save the current currency. [Begin]
lcCurrtCur = cCurrCode 
*E301424,1 ABD- [End]

*E301424,1 ABD- Remark the next lines and sum only when not multi 
*E301424,1 ABD- Currency and if multi & foriegn don't print beginning balance. [begin]
IF llMultCurr 
  IF lcRpCurr = "F"  
    SCAN REST WHILE REPCODE+cCurrCode=XSALESREP+lcCurrtCur .AND. DATE<LDATE
      BEGBAL = BEGBAL + IIF(nforamnt = 0 , AMOUNT , lfBaseAmt(nforamnt))
    ENDSCAN
  ELSE  
    SCAN REST WHILE REPCODE=XSALESREP .AND. DATE<LDATE
       BEGBAL = BEGBAL + IIF(nforamnt = 0 , AMOUNT , lfBaseAmt(nforamnt))
    ENDSCAN
  ENDIF
ELSE
  SUM REST AMOUNT TO BEGBAL WHILE REPCODE=XSALESREP .AND. DATE<LDATE
ENDIF

*-- End Of lfBgBlanc.
*!*************************************************************
