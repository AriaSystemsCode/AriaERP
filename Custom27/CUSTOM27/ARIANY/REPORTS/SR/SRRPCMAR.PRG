*:***************************************************************************
*: Program file  : SRRPCMAR
*: Program desc. : Sales Representative commission statement Report for aria NY
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Sales Representative (SR )
*: Developer     : Mohamed Shokry  (MHM)
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
*: This Report Program is due to C200176 ...
*: Modifications:
*:B604486,1  MHM 21/05/2001 Fix the bug of the pay date option not work correctly
*:B604486,1                 in case of manual payment
*:***************************************************************************
*
lcCurrtCur = ''
ROW        = 23
lcRepComm  = gfTempName() && variable to hold Temp. index on REPCOMM FILE if company multi curr.
BEGBAL     = 0
lcFilter   = ".T."
*--open custom file to get data in case of manual payment
=gfOpenFile(gcDataDir+'PAYTHIST',gcDataDir+'PAYTHIST','SH')
=gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
=gfOpenFile(gcDataDir+'DEBIT',gcDataDir+'DEBIT','SH')


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

SELECT REPCOMM

IF llMultCurr
  INDEX ON REPCODE+CCURRCODE+DTOS(DATE)+TRAN+TRANTYPE TAG (lcRepComm) OF;
                                           (gcWorkDir+lcRepComm+'.CDX')
ENDIF
SET FILTER To &lcFilter


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
 
    lcCurrtCur = cCurrCode 
    =lfBgBlanc()

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

  = lfPageHder()
  *-- Evaluate sales rep. address [begin]

  IF PAGENO=1 .AND. BEGBAL<>0 .AND. XPAY_TYPE<>'M'
    @ ROW,09 SAY 'ACCOUNT BALANCE AS OF &THRUDATE'
    IF lcRpDeciml='Y'
      @ ROW,69 SAY BEGBAL   PICTURE '9999999.99'
    ELSE  
      @ ROW,69 SAY BEGBAL   PICTURE '9999999999'
    ENDIF
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
    lcCurrtCur = cCurrCode
    SCAN REST WHILE lcCurrtCur = cCurrCode .AND.  REPCODE = XSALESREP
      IF DATE > HDATE
        EXIT
      ENDIF
      IF llMultCurr 
         lnAmount = IIF(nforamnt = 0 , AMOUNT , lfBaseAmt(nforamnt))
         RUNBAL = RUNBAL + IIF(AMOUNT > 0 , lnAmount , IIF(lnAmount < 0 , lnAmount , lnAmount*-1))
      ELSE
        RUNBAL = RUNBAL + AMOUNT
      ENDIF

      @ ROW,00 SAY DATE
      @ ROW,11 SAY TRAN
      @ ROW,18 SAY PADR(DESC,18)
      @ ROW,37 SAY ORDER
      @ ROW,44 SAY PADR(CUSTPO,13)
    
      IF llMultCurr 

         IF lcRpDeciml='Y'
           @ ROW,51 SAY IIF(AMOUNT > 0 , lnAmount , IIF(lnAmount < 0 , lnAmount , lnAmount*-1))   PICTURE '9999999.99'
         ELSE
           @ ROW,51 SAY IIF(AMOUNT > 0 , lnAmount , IIF(lnAmount < 0 , lnAmount , lnAmount*-1))   PICTURE '9999999999'
         ENDIF
         IF lcRpDeciml='Y'
           @ ROW,69 SAY RUNBAL             PICTURE '9999999.99'
         ELSE
           @ ROW,69 SAY RUNBAL             PICTURE '9999999999'
         ENDIF          
      ELSE
        IF lcRpDeciml='Y'
          @ ROW,51 SAY AMOUNT             PICTURE '9999999.99'
          @ ROW,69 SAY RUNBAL             PICTURE '9999999.99'
        ELSE
          @ ROW,51 SAY AMOUNT             PICTURE '9999999999'
          @ ROW,69 SAY RUNBAL             PICTURE '9999999999'
        ENDIF  
      ENDIF
    
      ROW = ROW+1
      IF !EMPTY(ACCOUNT) .AND. SEEK('M'+REPCOMM.ACCOUNT,'CUSTOMER')
        @ ROW,11 SAY CUSTOMER.BTNAME
        ROW = ROW+1
      ENDIF

      IF DATE > HDATE
        EXIT
      ENDIF
      IF REPCODE=XSALESREP AND ROW >= 52
        = lfPageFotr()  && Function Print Page Foter .
        = lfPageHder()   && Function Print Page Header.
        lcCurrtCur = cCurrCode
      ENDIF
    ENDSCAN
    
    IF !EOF() .AND. lcCurrtCur <> cCurrCode .AND. REPCODE=XSALESREP ; 
       .AND. lcRpCurr = "F"
      = lfPageFotr()    && Function Print Page Foter .
      = lfPageHder()    && Function Print Page Header.
      *-- Save the current currency.
      lcCurrtCur = cCurrCode
      lcKey = EVAL(Key())
      =lfBgBlanc()
      =SEEK(lcKey)
      RUNBAL = BEGBAL
      IF BEGBAL<>0 
        @ ROW,09 SAY 'ACCOUNT BALANCE AS OF &THRUDATE'
        IF lcRpDeciml='Y'
          @ ROW,69 SAY BEGBAL   PICTURE '9999999.99'
        ELSE
          @ ROW,69 SAY BEGBAL   PICTURE '9999999999'
        ENDIF
        ROW = ROW +1
      ENDIF
    ENDIF
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

    lcCurrtCur = cCurrCode
    RUNBAL     = IIF(llMultCurr .AND. lcRpCurr = "F" ,0, RUNBAL )
    SCAN REST WHILE lcCurrtCur = cCurrCode .AND.  REPCODE = XSALESREP  FOR TranType = '2'

      *:B604486,1 MHM 05/21/2001 [Start]
      IF PAYDATE > XHPAYDATE OR PAYDATE < XLPAYDATE
        EXIT
      ENDIF
      *:B604486,1 MHM 05/21/2001 [End]

      IF llMultCurr
        lnAmount = IIF(nforamnt = 0 , AMOUNT , lfBaseAmt(nforamnt))
        RUNBAL = RUNBAL + IIF(AMOUNT > 0 , lnAmount , IIF(lnAmount < 0 , lnAmount , lnAmount*-1))
      ELSE
        RUNBAL = RUNBAL + AMOUNT
      ENDIF

      @ ROW,00 SAY DATE
      @ ROW,11 SAY TRAN
      @ ROW,18 SAY PADR(DESC,18)
      @ ROW,37 SAY ORDER
      @ ROW,44 SAY PADR(CUSTPO,13)

      IF llMultCurr

         IF lcRpDeciml='Y'
           @ ROW,51 SAY IIF(AMOUNT > 0 , lnAmount , IIF(lnAmount < 0 , lnAmount , lnAmount*-1)) PICTURE '9999999.99'
         ELSE
           @ ROW,51 SAY IIF(AMOUNT > 0 , lnAmount , IIF(lnAmount < 0 , lnAmount , lnAmount*-1)) PICTURE '9999999999'
         ENDIF
         IF lcRpDeciml='Y'
           @ ROW,69 SAY RUNBAL             PICTURE '9999999.99'
         ELSE
           @ ROW,69 SAY RUNBAL             PICTURE '9999999999'
         ENDIF
      ELSE
        IF lcRpDeciml='Y'
          @ ROW,51 SAY AMOUNT             PICTURE '9999999.99'
          @ ROW,69 SAY RUNBAL             PICTURE '9999999.99'
        ELSE
          @ ROW,51 SAY AMOUNT             PICTURE '9999999999'
          @ ROW,69 SAY RUNBAL             PICTURE '9999999999'
        ENDIF
      ENDIF
      *--MHM200 here we will add detail band for partial payment from custom
      *--File PayThist

      IF TranType= '2'
        ROW = ROW+1
        lnTotPay   = 0
        IF ROW >= 52
          = lfPageFotr()  && Function Print Page Foter .
          = lfPageHder()   && Function Print Page Header.
          lcCurrtCur = cCurrCode
        ENDIF
        =lfParHdr()
        SELECT PAYTHIST

        =SEEK(REPCOMM.repcode+DTOS(REPCOMM.date)+REPCOMM.Tran)
        SCAN REST WHILE PAYTHIST.RepCode = XSALESREP  FOR PAYTHIST.Tran = REPCOMM.Tran 
          @ ROW,04 SAY invoice
          @ ROW,11 SAY ACCOUNT
          @ ROW,17 SAY PayDate

          IF lcRpDeciml='Y'
            @ ROW,28 SAY lfInvAmt(invoice)   PICTURE '999999999.99'
            @ ROW,41 SAY nTotComm   PICTURE '999999.99'
            @ ROW,51 SAY nTotPrvCom   PICTURE '999999.99'
            *:B604486,1 MHM 05/21/2001 [Start]
            *@ ROW,61 SAY lfGetBalce()   PICTURE '999999.99'
            @ ROW,61 SAY nCommBal   PICTURE '999999.99'
            *:B604486,1 MHM 05/21/2001 [End]
            @ ROW,71 SAY amount   PICTURE '999999.99'
            lnTotPay = lnTotPay + amount
          ELSE 
            @ ROW,28 SAY lfInvAmt(invoice)   PICTURE '99999999999'
            @ ROW,41 SAY nTotComm   PICTURE '999999999'
            @ ROW,51 SAY nTotPrvCom   PICTURE '999999999'
            *:B604486,1 MHM 05/21/2001 [Start]
            *@ ROW,61 SAY lfGetBalce()   PICTURE '999999999'
            @ ROW,61 SAY nCommBal PICTURE '999999999'
            *:B604486,1 MHM 05/21/2001 [Start]
            @ ROW,71 SAY amount   PICTURE '999999999'
            lnTotPay = lnTotPay + amount
          ENDIF
          ROW = ROW+1
          IF ROW >= 52
            = lfPageFotr()  && Function Print Page Foter .
            = lfPageHder()   && Function Print Page Header.
            lcCurrtCur = cCurrCode
          ENDIF
        ENDSCAN
        IF ROW >= 49
          = lfPageFotr()  && Function Print Page Foter .
          = lfPageHder()   && Function Print Page Header.
          lcCurrtCur = cCurrCode
        ENDIF
        @ ROW,71 SAY REPLICATE('=',9)
        ROW = ROW+1
        IF lcRpDeciml='Y'
          @ ROW,71 SAY lnTotPay   PICTURE '999999.99'
        ELSE
          @ ROW,71 SAY lnTotPay   PICTURE '999999999'
        ENDIF  
        ROW = ROW+1
        @ ROW,71 SAY REPLICATE('=',9)
        ROW = ROW+1
      ENDIF
      *----MHM200
      ROW = ROW+1
      IF !EMPTY(ACCOUNT) .AND. SEEK('M'+REPCOMM.ACCOUNT,'CUSTOMER')
        @ ROW,11 SAY CUSTOMER.BTNAME
        ROW = ROW+1
      ENDIF
      
      IF ROW >= 52
        = lfPageFotr()  && Function Print Page Foter .
        = lfPageHder()   && Function Print Page Header.
        lcCurrtCur = cCurrCode
      ENDIF
    ENDSCAN
    IF !EOF() .AND. lcCurrtCur <> cCurrCode .AND. REPCODE=XSALESREP ; 
       .AND. lcRpCurr = "F"
      = lfPageFotr()   && Function Print Page Foter .
      = lfPageHder()   && Function Print Page Header.
      lcCurrtCur = cCurrCode
    ENDIF
  ENDDO
    
  *** END PRINT LOOP FOR RUNNING BALANCE SALESREPS
  = lfPageFotr() && Function Print Page Foter .

ENDDO

IF llPrnt
  DO ENDREPORT
  SELECT REPCOMM
  SET FILTER To 
ELSE
  =gfModalGen(.f.,.f.,.f.,.f.,'There are no records to display...! ')  
ENDIF
SET DEVICE TO SCREEN

IF llMultCurr
  SELECT REPCOMM
  CLOSE INDEXES
  ERASE(gcWorkDir+lcRepComm+'.CDX')
ENDIF

*----------------
* PRINTING MAIN LOOP [End]
*----------------
RETURN
*-- end of SRREPSH.PRG report code.


*------- Functions section ---------
*----------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2001
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

  IF !USED('SYCCOMP')
    USE (gcSysHome+'SYCCOMP') ORDER TAG Ccomp_id IN 0 SHARED
  ENDIF
  
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

IF llMultCurr
  lnCurrPos  = lfItmPos('REPCOMM.CCURRCODE')
ELSE
  lcRpCurr = "O"
ENDIF  

*-- End of lfwRepWhen.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2001
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2001
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2001
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2001
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2001
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2001
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
IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF


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


*-- End OF lfClearRep.
*!*************************************************************
*! Name      : lfFillVars
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2001
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
*E
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
*!*************************************************************
*! Name      : lfItmPos
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2001
*! Purpose   : Evaluate fixed filter position within array.
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

*-- End Of lfItmPos.
*!*************************************************************
*! Name      : lfBaseAmt
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2001
*! Purpose   : Compute base amount
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfBaseAmt()
*!*************************************************************
*
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2001
*! Purpose   : Print the page Header.
*!*************************************************************
*! Passed Parameters  : Non.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : = lfPageHder()
*!*************************************************************
*
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2001
*! Purpose   : Print the page Foter.
*!*************************************************************
*! Passed Parameters  : Non.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : = lfPageFotr()
*!*************************************************************
FUNCTION lfPageFotr

@ 53,00 SAY REPLICATE('-',80)
@ 54,02 SAY "PRINTED ON " + DTOC(TODAY)


IF llMultCurr
  @ 54,25 SAY 'Currency  '+IIF(lcRpCurr = "F"  ,lcCurrtCur,gcBaseCurr)
ENDIF

@ 54,55 SAY IIF(RUNBAL=0,'','BALANCE ->')
IF ROW >= 52
  @ 54,69 SAY 'CONTINUED'
ELSE  
  IF lcRpDeciml='Y'
    @ 54,69 SAY IIF(RUNBAL=0,'',RUNBAL) PICTURE '9999999.99'
  ELSE
    @ 54,69 SAY IIF(RUNBAL=0,'',RUNBAL) PICTURE '9999999999'
  ENDIF
ENDIF
@ 55,00 SAY REPLICATE('-',80)
IF NEXTREP
  SELECT SALESREP
  SKIP
ENDIF
@ 00,00 SAY ''   &&to eject the page.

*-- End Of lfPageFotr.
*!*************************************************************
*! Name      : lfBgBlanc
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2001
*! Purpose   : Accumlate the Beginning Balane for evry sales rep
*!           : Deff. Currency.
*!*************************************************************
*! Passed Parameters  : Non.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : = lfBgBlanc()
*!*************************************************************
*
FUNCTION lfBgBlanc

PAGENO = 0
BEGBAL = 0.00

lcCurrtCur = cCurrCode 
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
*! Name      : lfParHdr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2001
*! Purpose   : Accumlate the Beginning Balane for evry sales rep
*!           : Deff. Currency.
*!*************************************************************
*! Passed Parameters  : Non.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : = lfParHdr()
*!*************************************************************
*
FUNCTION lfParHdr

*TRAN#  Acc.  Tran Date Invoice Amount Total Comm. Previous Comm. Comm. Bal.  Comm. Paid
*123456 12345 MM/DD/YY  12345678901234 12345678901 12345678901234 12345678901 12345678901
*0....+....1....+....2....+....3....+....4....+....5....+....6...+....7....+....

*@ ROW,04 SAY 'TRAN#  Acc.  Tran Date  Inv. Amount  Tot. Com. Prev.Com. Com. Bal. Com. Paid'
@ ROW,04 SAY 'Inv.#  Acc.  Tran Date  Inv. Amount  Tot. Com. Prev.Com. Com. Bal. Com. Paid'
ROW = ROW+1
@ ROW,04 SAY REPLICATE('=',6)
@ ROW,11 SAY REPLICATE('=',5)
@ ROW,17 SAY REPLICATE('=',10)
@ ROW,28 SAY REPLICATE('=',12)
@ ROW,41 SAY REPLICATE('=',9)
@ ROW,51 SAY REPLICATE('=',9)
@ ROW,61 SAY REPLICATE('=',9)
@ ROW,71 SAY REPLICATE('=',9)
ROW = ROW+1

*!*************************************************************
*! Name      : lfInvAmt
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2001
*! Purpose   : get invoice amount 
*!*************************************************************
*! Passed Parameters  : Non.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : = lfParHdr()
*!*************************************************************
*
FUNCTION lfInvAmt
PARAMETER lcInvNo
PRIVATE lnAlias 

lnAlias =SELECT(0)

IF SEEK(lcInvNo,'INVHDR')
  lnInvAmt = INVHDR.TOTALCHG
ELSE
  lnInvAmt = ''
ENDIF  
SELECT(lnAlias)
RETURN lnInvAmt

*!*************************************************************
*! Name      : lfGetBalce                   
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/03/2001
*! Purpose   : Get Balance 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfGetBalce()
*!*************************************************************
*!
FUNCTION lfGetBalce
PRIVATE lnInvAmt , lnDbitAmt , lnTotPaid ,lnAlias , lcSavOrd , lnBalance
STORE 0 TO lnDbitAmt,  lnInvAmt  , lnBalance
lnAlias =SELECT(0)
*--get invoice amount

IF TranType = '1'
  =SEEK(PAYTHIST.INVOICE,'INVHDR')
  *!B604494,1  05/21/2001 MHM [start]
  *lnInvAmt = INVHDR.TOTALCHG
  lnInvAmt = INVHDR.ShipAmt + INVHDR.Discount - (INVHDR.Trde_Disc/100)*INVHDR.ShipAmt
  *!B604494,1  05/21/2001 MHM [end]
  *--get debit amount from debit file
  SELECT DEBIT
  lcSavOrd = ORDER('DEBIT')
  SET ORDER TO 2
  =SEEK('1'+PAYTHIST.INVOICE)
  lnDbitAmt = DEBIT.AMOUNT
  *--get total invoice amount paid
  SET ORDER TO &lcSavOrd
  *!B604494,1  05/21/2001 MHM [start]
  *lnTotPaid = lnInvAmt - lnDbitAmt
  IF INVHDR.TOTALCHG > lnDbitAmt
    lnTotN = INVHDR.TOTALCHG - lnDbitAmt
    IF lnTotN <= lnInvAmt
      lnTotPaid = lnTotN
    ELSE 
      lnTotPaid = lnInvAmt
    ENDIF
  ELSE  
    lnTotPaid = 0
  ENDIF
  *!B604494,1  05/21/2001 MHM [end]
  SELECT REPCOMM
  lnRecNo = RECNO() 
  lcSavOrd1 = ORDER('REPCOMM')
  SET ORDER TO 1
  =SEEK(RepCode+DTOS(Paydate)+PAYTHIST.INVOICE+ '1')  
  lnBalance= ROUND((lnTotPaid*RepComm.commpcnt/100) - PAYTHIST.ntotprvcom,2)
  SET ORDER TO &lcSavOrd1
  GOTO lnRecNo
ELSE
  lnBalance = 0
ENDIF

SELECT (lnAlias)
RETURN lnBalance
