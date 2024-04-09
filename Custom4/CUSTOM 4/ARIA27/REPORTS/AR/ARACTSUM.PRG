*:************************************************************************
*: Program file  : ARACTSUM.Prg
*: Program desc. : Activity Summary Report
*: System        : Aria Advantage Series VER. 2.7
*: Module        : AR
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Date          : 12/27/98
*: Reference     : *E301104,1
*:************************************************************************
*: Calls : 
*:    Procedures : CALCULATE()
*:    Functions  : lfwOGWhen(), lfvDateRng(), lfvpbDateOk(), lfvSelBy()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO ARACTSUM
*:************************************************************************
*: Modifications:
*B802070,1 AMM 03/22/99   Validate "MONTH" setting, adjust printing the PERIOD title
*B603279,1 BWA 11/18/99   Fix the bug of the value of the sales column to calculate 
*                         from the (InvHdr.dbf) directly on condition of the factor
*                         and modify option in the grid to the factor.
*B803034,1 BWA 09/02/2000 Fix the bug of The column for the GRAND TOTAL needs more digits.
*                         It's printing * ,***, ***. **.
*B803167,1 BWA 04/17/2000 1- Modify in the report to get the sales figures calculated by
*B803167,1                   this report match with total sales shown in the sales journal
*B803167,1                   report (in case of selecting to print active and voided 
*B803167,1                   invoices in the journal report).
*B803167,1                   Modify an option in the report criteria to the void invoices.
*B803167,1                2- Fix the bug of get "lfvpbDateOk" does not exist upon click 
*B803167,1                   the ok button of the transaction date option.
*B603685,1 BWA 04/17/2000    Fix the bug of get "lfvPbOk" does not exist.
*B803225,1 BWA 06/26/2000 Fix the bug of the report give a wrong ending balance due to some
*B803225,1                transactions type doesn't belong to any of the types the program checks for.
*B603591,1 BWA 07/19/2000 Make Activity summary report consider the different types of Currencies.
*B603805,1 BWA 08/10/2000 Fix the bug of the columns for the report displayed "********". 
*B603805,1                by modifying a filter to ask the user if wants to print decimals or not.[wam]
*B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*B605369,1 SSE 01/15/2001 Fix bug of not calculating the Charge back. 
*B606513,1 ADEL 10/20/02 Don't take Charge back into account from ARHIST as it's already calculated
*B606513,1 ADEL          in the debit adjustment. 
*:************************************************************************************************
*  TRAN CODES             FILE-ID
*  0 = CREDIT MEMOS       CREDIT
*  1 = INVOICES           DEBIT
*  3 = CHARGEBACK IN      DEBIT
*  4 = PAYMENT            CREDIT
*  5 = CREDIT ADJUSTMENT  CREDIT
*  2 = DEBIT ADJUSTMENT   DEBIT
*  6 = CREDIT ON ACCOUNT  CREDIT
*  7 = ALLOWANCE          CREDIT
*  8 = CHR.BCK            ARHIST
*  9 = CREDIT ON ACCT     ARHIST
*-- If select by month and month value not entered, or select by date and 
*-- date values not entered, so there is no records to display.

IF (lcrpSelBy = 'M' .AND. (EMPTY(lcMonth) ) ) ;
   .OR. (lcrpSelBy = 'D' .AND. (EMPTY(lDate) .OR. EMPTY(hDate)) )
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

*-- If year value is empty , fill it with the current year.
IF lcrpSelBy = 'M' .AND. EMPTY(lcYear)
  lcYear=STR(YEAR(DATE()),4)
ENDIF
SET DEVICE TO PRINTER
****INIT. TOTALS***
STORE 0.00 TO XTOTOPEN,XTOTSALES,XTOTPAY,XTOTRET, XTOTFALL,XTOTDALL, ;
              XTOTMALL,XTOTBAL
PAGENO = 0
ROW    = 99

*-- Array to get the Division long name
DIMENSION laType[1,2]
lcType = SPACE(1)
laType[1,1] = 'ALLOW_TYPE'
laType[1,2] = 'lcType'

R_WIDTH= 'W'             && STANDARD REPORT 'WIDE'
XTIME  = SUBSTR(TIME(),1,5)
*B802070,1 AMM Move this part down
*XTITLE='PERIOD '+ DTOC(LDATE) + ' --- ' + DTOC(HDATE)
*B802070,1 AMM end
R_TITLE= "ACTIVITY SUMMARY REPORT"
llFound =.F.             && Indicate if there are records to print or not.

*-- If select by month, form the from&to dates as beginning and ending 
*-- of that month
IF lcrpSelBy = 'M' 
  LcX    = lcMONTH+'/'+'01'+'/'+PADL(lcYEAR,4,'0')
  LDATE  = CTOD(LcX)
  NDATE  = LDATE+32
  HDATE  = NDATE-(DAY(NDATE))
  ENDING = LDATE-(DAY(LDATE))
ELSE
  ENDING = LDATE - 1
ENDIF

*B802070,1 AMM Moved here because LDATE&HDATE now have their right values
XTITLE='PERIOD '+ DTOC(LDATE) + ' --- ' + DTOC(HDATE)
*B802070,1 AMM end

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*AR980                                                                                                                   MM/DD/YY
*TIME: HH:SS                                               ACTIVITY SUMMARY REPORT                                       PAGE: 1234
*
*Acct# Name                   Open. Bal         Sales      Payments       Returns Freight All.   Disc. All.   Misc. All.   Ending Bal.
*12345 123456789012345678 99,999,999.99 99,999,999.99 99,999,999.99 99,999,999.99 9,999,999.99 9,999,999.99 9,999,999.99 99,999,999.99
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

SELE CUSTOMER
=SEEK('M')
SCAN WHILE  TYPE='M' FOR &lcRpExp
  STORE 0 TO XOPENBAL,XSALES,XPAY,XRET,XFALL,XDALL,XMALL,XBAL,XOPENDB,XOPENCR
  IF ROW >=54
    *-- Print header of the report.
    PAGENO = PAGENO +1
    DO RPT_HDR WITH 'AR980',XTITLE,R_WIDTH
    
    *B803034,1 BWA 09/02/2000 Fix the bug of The column for the GRAND TOTAL needs more digits.
    *                         It's printing * ,***, ***. **. [START]
    *@ 05,000 SAY 'Acct# Name                  Open Bal.         Sales      Payments       Returns Freight All.   Disc. All.   Misc. All.   Ending Bal.'  && YMA 02/05/94
    
    *B603805,1 Fix the bug of the columns for the report displayed "********". 
    *B603805,1 by modifying a filter to ask the user if wants to print decimals or not.[START]
    *@ 05,000 SAY 'Acct# Name                  Open Bal.         Sales      Payments       Returns Freight All.  Disc. All.   Misc. All.    Ending Bal.'
    IF llRpDec
      @ 05,000 SAY 'Acct# Name                Open Bal.         Sales      Payments       Returns Freight All.  Disc. All.   Misc. All.    Ending Bal.'
    ELSE
      @ 05,000 SAY 'Acct# Name               Open Bal.          Sales      Payments       Returns Freight All.  Disc. All.   Misc. All.    Ending Bal.'
    ENDIF
    *B603805,1 [END]

    *B803034,1 BWA 09/02/2000 [END]
    
    @ 06,000 SAY REPLICATE('-',132)
    ROW = 7
  ENDIF
  XACCOUNT=ACCOUNT

  *-- PROCEDURE TO CALCULATE OPENBALANCE AND CALCULATE PAYMENT AND ALLOWANCE
  =CALCULATE()

  *-----------O P E N  B A L .-----------
  XOPENBAL = XOPENDB + XOPENCR
  *-------------S A L E S----------------
  SELE INVHDR
  =SEEK(XACCOUNT)
  *B603279,1 BWA 11/18/99 Fix the bug of the value of the sales column calculation[START]
  *SUM REST TOTALCHG TO XSALES WHILE (XACCOUNT=ACCOUNT) ;
      FOR BETWEEN(INVDATE,LDATE,HDATE) AND ;
         (SEEK(ACCOUNT+INVOICE,"DEBIT") OR SEEK(ACCOUNT+INVOICE,"ARHIST"))

  *B803167,1 Modify in the report to get the sales figures calculated by
  *          this report match with total sales shown in the sales journal
  *          report (like in case of selecting to print active and voided 
  *          invoices in the journal report).[START]
         
  *SUM REST TOTALCHG TO XSALES WHILE (XACCOUNT=ACCOUNT) ;
      FOR BETWEEN(INVDATE,LDATE,HDATE) AND ;
         IIF(lcRpFact = 'F' , !EMPTY(InvHdr.cFacCode) ,;
             IIF(lcRpFact = 'N' , EMPTY(InvHdr.cFacCode),.T.))

  IF llRpVodInv      && Variable if the user wanted to Include voided invoices or not.
    XSALES = 0

    *B603591,1 BWA 07/19/2000 Make Activity summary report consider the different types of Currencies.[START]
    *SCAN REST WHILE ACCOUNT=XACCOUNT ;
              FOR  IIF(lcRpFact ='F' , !EMPTY(InvHdr.cFacCode) ,;
                   IIF(lcRpFact ='N' , EMPTY(InvHdr.cFacCode),.T.))

    SCAN REST WHILE ACCOUNT=XACCOUNT ;
              FOR  IIF(lcRpFact ='F' , !EMPTY(InvHdr.cFacCode) ,;
                   IIF(lcRpFact ='N' , EMPTY(InvHdr.cFacCode),.T.));
              .AND.  INVHDR.CCURRCODE = ALLTRIM(lcCurrency)
              
     *B603591,1 BWA 07/19/2000 [End]
      DO CASE
        CASE STATUS <> 'V' AND BETWEEN(InvDate,LDATE,HDATE)
        *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
          *XSALES = XSALES + TotalChg
          XSALES = XSALES + IIF(llMultCurr,gfAmntDisp(TotalChg, lcRpCurr , ldRpExDate , lcRpTmpNam),TotalChg)
        *B603591,1 BWA 07/19/2000 [End]
        
        CASE STATUS = 'V'  AND BETWEEN(InvDate,LDATE,HDATE)  AND !BETWEEN(vDate,LDATE,HDATE)
          *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
          *XSALES = XSALES + vTotalChg
          XSALES = XSALES + IIF(llMultCurr,gfAmntDisp(vTotalChg, lcRpCurr , ldRpExDate , lcRpTmpNam),vTotalChg)
          *B603591,1 BWA 07/19/2000 [End]
          
        CASE STATUS = 'V'  AND !BETWEEN(InvDate,LDATE,HDATE) AND BETWEEN(vDate,LDATE,HDATE)
          *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
          *XSALES = XSALES - vTotalChg
          XSALES = XSALES - IIF(llMultCurr,gfAmntDisp(vTotalChg, lcRpCurr , ldRpExDate , lcRpTmpNam),vTotalChg)
          *B603591,1 BWA 07/19/2000 [End]
      ENDCASE
    ENDSCAN

  ELSE
    *B603591,1 BWA 07/19/2000 [Begin] Add the currency code filter.
    *SUM REST TOTALCHG TO XSALES WHILE (XACCOUNT=ACCOUNT) ;
    FOR BETWEEN(INVDATE,LDATE,HDATE) AND ;
       IIF(lcRpFact = 'F' , !EMPTY(InvHdr.cFacCode) ,;
           IIF(lcRpFact = 'N' , EMPTY(InvHdr.cFacCode),.T.))
           
    SUM REST TOTALCHG TO XSALES WHILE (XACCOUNT=ACCOUNT) ;
    FOR BETWEEN(INVDATE,LDATE,HDATE) AND ;
       IIF(lcRpFact = 'F' , !EMPTY(InvHdr.cFacCode) ,;
           IIF(lcRpFact = 'N' , EMPTY(InvHdr.cFacCode),.T.));
           .AND.  INVHDR.CCURRCODE = ALLTRIM(lcCurrency)
    *B603591,1 [END]
    
  ENDIF
  *B803167,1 [END]

  *B603279,1 BWA 11/18/99 [END]
  
  *--------R E T U R N S----------------
  *B802070,1 AMM check if the file opened, this file is installed with RM module.
  IF USED('RETHDR')
  *B802070,1 AMM end
    SELE RETHDR
    =SEEK(XACCOUNT)

    *B603591,1 BWA 07/19/2000 Add the cuurency code to the filter.[START]    
    *SUM REST TOTCREDIT TO XRET WHILE XACCOUNT=ACCOUNT ;
                             FOR   BETWEEN(CRDATE,LDATE,HDATE)

    SUM REST IIF(llMultCurr,gfAmntDisp(TOTCREDIT, lcRpCurr , ldRpExDate , lcRpTmpNam),TOTCREDIT);
             TO XRET WHILE XACCOUNT=ACCOUNT ;
             FOR   BETWEEN(CRDATE,LDATE,HDATE);
             .AND.  RETHDR.CCURRCODE = ALLTRIM(lcCurrency)
    *B603591,1 [END]

    XRET=(-1*XRET)
  *B802070,1 AMM end if 
  ENDIF
  *B802070,1 AMM end
  *--------B A L A N C E------------------
  XBAL=(XOPENBAL+XSALES+XPAY+XRET+XFALL+XDALL+XMALL)
  *----- T O T A L S -------
  XTOTOPEN = XTOTOPEN  + XOPENBAL
  XTOTSALES= XTOTSALES + XSALES
  XTOTPAY  = XTOTPAY   + XPAY
  XTOTRET  = XTOTRET   + XRET
  XTOTFALL = XTOTFALL  + XFALL
  XTOTDALL = XTOTDALL  + XDALL
  XTOTMALL = XTOTMALL  + XMALL
  XTOTBAL  = XTOTBAL   + XBAL
  *-- Prevent printing customers with no activities.
  lnAct=(ABS(XOPENBAL)+XSALES+ABS(XPAY)+ABS(XRET)+ABS(XFALL)+ABS(XDALL)+ABS(XMALL))
  IF (llRpNAct .OR. (!llRpNAct AND lnAct > 0))
    llFound = .T.
    @ ROW,000 SAY CUSTOMER.ACCOUNT
    
    *B803034,1 BWA 09/02/2000 Fix the bug of The column for the GRAND TOTAL needs more digits.
    *                         It's printing * ,***, ***. **. [START]
    *@ ROW,006 SAY SUBSTR(CUSTOMER.BTNAME,1,17)
    *@ ROW,024 SAY XOPENBAL PICTURE '99,999,999.99'
    @ ROW,006 SAY SUBSTR(CUSTOMER.BTNAME,1,16)

    *B603805,1 Fix the bug of the columns for the report displayed "********". 
    *B603805,1 by modifying a filter to ask the user if wants to print decimals or not.[START]
    *@ ROW,023 SAY XOPENBAL PICTURE '999,999,999.99'
    IF llRpDec
      @ ROW,021 SAY XOPENBAL PICTURE '999,999,999.99'
    ELSE
      @ ROW,020 SAY XOPENBAL PICTURE '99,999,999,999'
    ENDIF
    *B603805,1 [END]
    *B803034,1 BWA 09/02/2000 [END]
    
    *B603805,1 Fix the bug of the columns for the report displayed "********". 
    *B603805,1 by modifying a filter to ask the user if wants to print decimals or not.[START]
    *@ ROW,038 SAY XSALES   PICTURE '99,999,999.99'
    *@ ROW,052 SAY XPAY     PICTURE '99,999,999.99'
    *@ ROW,066 SAY XRET     PICTURE '99,999,999.99'
    *@ ROW,080 SAY XFALL    PICTURE '9,999,999.99'
    
    IF llRpDec
      @ ROW,036 SAY XSALES   PICTURE '99,999,999.99'
      @ ROW,050 SAY XPAY     PICTURE '99,999,999.99'
      @ ROW,064 SAY XRET     PICTURE '99,999,999.99'
      @ ROW,078 SAY XFALL    PICTURE '9,999,999.99'
    ELSE
      @ ROW,035 SAY XSALES   PICTURE '99,999,999,999'
      @ ROW,050 SAY XPAY     PICTURE '9,999,999,999'
      @ ROW,064 SAY XRET     PICTURE '9,999,999,999'
      @ ROW,078 SAY XFALL    PICTURE '9999,999,999'
    ENDIF
    *B603805,1 [END]

    *B803034,1 BWA 09/02/2000 Fix the bug of The column for the GRAND TOTAL needs more digits.
    *                         It's printing * ,***, ***. **. [START]
    *@ ROW,093 SAY XDALL    PICTURE '9,999,999.99'
    *@ ROW,106 SAY XMALL    PICTURE '9,999,999.99'
    *@ ROW,119 SAY XBAL     PICTURE '99,999,999.99'

    *B603805,1 Fix the bug of the columns for the report displayed "********". 
    *B603805,1 by modifying a filter to ask the user if wants to print decimals or not.[START]
    *@ ROW,092 SAY XDALL    PICTURE '9,999,999.99'
    *@ ROW,105 SAY XMALL    PICTURE '9,999,999.99'
    *@ ROW,118 SAY XBAL     PICTURE '999,999,999.99'

    IF llRpDec
      @ ROW,090 SAY XDALL    PICTURE '9,999,999.99'
      @ ROW,103 SAY XMALL    PICTURE '9,999,999.99'
      @ ROW,116 SAY XBAL     PICTURE '999,999,999.99'
    ELSE
      @ ROW,090 SAY XDALL    PICTURE '9999,999,999'
      @ ROW,103 SAY XMALL    PICTURE '9999,999,999'
      @ ROW,116 SAY XBAL     PICTURE '99,999,999,999'
    ENDIF
    *B603805,1 [END]

    *B803034,1 BWA 09/02/2000 [END]

    
    ROW=ROW+1
  ENDIF
ENDSCAN
IF !llFound
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ELSE
  *---------- PRINT GRAND TOTAL ---------------
  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE ('*',132)
  ROW = ROW + 1

  @ ROW,00  SAY '*** GRAND TOTAL ***'
  
  *B803034,1 BWA 09/02/2000 Fix the bug of The column for the GRAND TOTAL needs more digits.
  *                         It's printing * ,***, ***. **. [START]
  *@ ROW,024 SAY XTOTOPEN  PICTURE '99,999,999.99'
  *@ ROW,038 SAY XTOTSALES PICTURE '99,999,999.99'
  *@ ROW,052 SAY XTOTPAY   PICTURE '99,999,999.99'
  *@ ROW,066 SAY XTOTRET   PICTURE '99,999,999.99'
  *@ ROW,080 SAY XTOTFALL  PICTURE '9,999,999.99'
  *@ ROW,093 SAY XTOTDALL  PICTURE '9,999,999.99'
  *@ ROW,106 SAY XTOTMALL  PICTURE '9,999,999.99'
  *@ ROW,119 SAY XTOTBAL   PICTURE '99,999,999.99'

  *B603805,1 Fix the bug of the columns for the report displayed "********". 
  *B603805,1 by modifying a filter to ask the user if wants to print decimals or not.[START]
  *@ ROW,023 SAY XTOTOPEN  PICTURE '999,999,999.99'
  *@ ROW,037 SAY XTOTSALES PICTURE '999,999,999.99'
  *@ ROW,051 SAY XTOTPAY   PICTURE '999,999,999.99'
  *@ ROW,065 SAY XTOTRET   PICTURE '999,999,999.99'
  *@ ROW,080 SAY XTOTFALL  PICTURE '9,999,999.99'
  *@ ROW,092 SAY XTOTDALL  PICTURE '9,999,999.99'
  *@ ROW,105 SAY XTOTMALL  PICTURE '9,999,999.99'
  *@ ROW,118 SAY XTOTBAL   PICTURE '999,999,999.99'
  
  IF llRpDec
    @ ROW,021 SAY XTOTOPEN  PICTURE '999,999,999.99'
    @ ROW,035 SAY XTOTSALES PICTURE '999,999,999.99'
    @ ROW,049 SAY XTOTPAY   PICTURE '999,999,999.99'
    @ ROW,063 SAY XTOTRET   PICTURE '999,999,999.99'
    @ ROW,078 SAY XTOTFALL  PICTURE '9,999,999.99'
    @ ROW,090 SAY XTOTDALL  PICTURE '9,999,999.99'
    @ ROW,103 SAY XTOTMALL  PICTURE '9,999,999.99'
    @ ROW,116 SAY XTOTBAL   PICTURE '999,999,999.99'
  ELSE
    @ ROW,020 SAY XTOTOPEN  PICTURE '99,999,999,999'
    @ ROW,035 SAY XTOTSALES PICTURE '99,999,999,999'
    @ ROW,049 SAY XTOTPAY   PICTURE '99,999,999,999'
    @ ROW,063 SAY XTOTRET   PICTURE '99,999,999,999'
    @ ROW,078 SAY XTOTFALL  PICTURE '9999,999,999'
    @ ROW,090 SAY XTOTDALL  PICTURE '9999,999,999'
    @ ROW,103 SAY XTOTMALL  PICTURE '9999,999,999'
    @ ROW,116 SAY XTOTBAL   PICTURE '99,999,999,999'
  ENDIF
  *B603805,1 [END]

  *B803034,1 BWA 09/02/2000 [END]
  
  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE ('*',132)
ENDIF

DO ENDREPORT         && END THE REPORT OR DISPLAY ON SCREEN
SET DEVICE TO SCREEN


*!*************************************************************
*! Name      : Calculate
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/27/98
*! Purpose   : Calculate THE OPEN BALANCE, PAYMENT AND ALLOWANCE
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfRltFld()
*!*************************************************************
*! Called from : ARACTSUM.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =CALCULATE()
*!*************************************************************

PROCEDURE CALCULATE
*------------------------------- D E B I T ----------------------------------*
SELECT DEBIT
=SEEK(XACCOUNT)
*B603591,1 BWA 07/19/2000 Filter on the currency code.[START]
*SCAN WHILE ACCOUNT=XACCOUNT
SCAN WHILE ACCOUNT=XACCOUNT FOR DEBIT.CCURRCODE = ALLTRIM(lcCurrency)
*B603591,1 [END]

  *-- OPEN BALANCE --*
  XDFOR = IIF( TRANTYPE='3','TRANDATE<=ENDING .AND. CHGBK_DATE<=ENDING',;
                             'TRANDATE<=ENDING')
  IF &XDFOR
    *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
    *XOPENDB = XOPENDB+AMOUNT
    XOPENDB = XOPENDB + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)
    *B603591,1 [END]
  ENDIF

  *---- ADJUSTMENT ---*
  *B605369,1 Include charge back type. [Begin]
  *IF (TRANTYPE='2'.AND. BETWEEN(TRANDATE,LDATE,HDATE))
  IF (TRANTYPE = '2' AND BETWEEN(TRANDATE,LDATE,HDATE)) OR ;
    (TRANTYPE = '3' AND EMPTY(ChgBk_Date) AND BETWEEN(TRANDATE,LDATE,HDATE))
  *B605369,1 Include charge back type. [End]
  
    lcType = SPACE(1)
    =gfRltFld(DEBIT.TRANCODE , @laType , 'TRANCODE')
    DO CASE
      CASE lcType = 'F'
        *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
        *XFALL=XFALL+AMOUNT
        XFALL=XFALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)
        *B603591,1 BWA 07/19/2000 [End]
  
      CASE lcType = 'D'
       *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
        *XDALL=XDALL+AMOUNT
        XDALL=XDALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)
        *B603591,1 BWA 07/19/2000 [End]
      *B803225,1 BWA 06/26/2000 Fix the bug of the report give a wrong ending balance due to some
      *B803225,1                transaction type doesn't belong to any of the types the program checks for.[START]
      *CASE lcType = 'M'
      OTHERWISE
        *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
        *XMALL=XMALL+AMOUNT
        XMALL=XMALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)
        *B603591,1 [END]

      *B803225,1 [END]
    ENDCASE        
  ENDIF
ENDSCAN

*-------------------------------- C R E D I T -------------------------------*
SELECT CREDIT

SEEK XACCOUNT
*B603591,1 BWA 07/19/2000 Filter on the currency code.[START]
*SCAN WHILE ACCOUNT=XACCOUNT
SCAN WHILE ACCOUNT=XACCOUNT FOR CREDIT.CCURRCODE = ALLTRIM(lcCurrency)
*B603591,1 [END]

  *-- OPEN BALANCE --*
  XCFOR = IIF( TRANTYPE='6','TRANDATE<=ENDING .AND. CREDT_DATE<=ENDING',;
                            'TRANDATE<=ENDING')
  IF &XCFOR
    *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
    *XOPENCR=XOPENCR+AMOUNT
    XOPENCR=XOPENCR + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)
    *B603591,1 [END]
  ENDIF
  *---- PAYMENTS ----*
  IF (TRANTYPE='4' .AND. BETWEEN(TRANDATE,LDATE,HDATE))
    *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
    *XPAY=XPAY+AMOUNT
    XPAY=XPAY + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)
    *B603591,1 [END]
  ENDIF

  *---- ADJUSTMENT ---*
  *B605369,1 Include the credit on account type. [Begin]
  *IF (TRANTYPE='5'.AND. BETWEEN(TRANDATE,LDATE,HDATE))
  IF (TRANTYPE = '5' AND BETWEEN(TRANDATE,LDATE,HDATE)) OR ;
    (TRANTYPE = '6' AND EMPTY(Credt_Date) AND BETWEEN(TRANDATE,LDATE,HDATE))  
  *B605369,1 Include the credit on account type. [End]
  
    lcType = SPACE(1)
    =gfRltFld(CREDIT.cCreditCod , @laType , 'CCREDITCOD')
    DO CASE
      CASE lcType = 'F'
        *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
        *XFALL=XFALL+AMOUNT
        XFALL=XFALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)
        *B603591,1 BWA 07/19/2000 [End]
        
      CASE lcType = 'D'
        *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
        *XDALL=XDALL+AMOUNT
        XDALL=XDALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)
        *B603591,1 BWA 07/19/2000 [End]
        
      *B803225,1 BWA 06/26/2000 Fix the bug of the report give a wrong ending balance due to some
      *B803225,1                transaction type doesn't belong to any of the types the program checks for.[START]
      *CASE lcType = 'M'
      OTHERWISE
        *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
        *XMALL=XMALL+AMOUNT
        XMALL=XMALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)
        *B603591,1 [END]

      *B803225,1 [END]

    ENDCASE        
  ENDIF
ENDSCAN

*-------------------------------- A R H I S T -------------------------------*

SELE ARHIST
SEEK XACCOUNT
*B603591,1 BWA 07/19/2000 Filter on the currency code.[START]
*SCAN WHILE ACCOUNT=XACCOUNT
SCAN WHILE ACCOUNT=XACCOUNT FOR ARHIST.CCURRCODE = ALLTRIM(lcCurrency)
*B603591,1 [END]

  *-- PAYMENTS --*
  IF (TRANTYPE='4'.AND.BETWEEN(TRANDATE,LDATE,HDATE))
    *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
    *XPAY=XPAY+AMOUNT
    XPAY=XPAY + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)
    *B603591,1 [END]
  ENDIF

  *---- ADJUSTMENT ---*
  *B605369,1 Collect the charge back from Debit adjustment screen. [Begin]
  *we need to add more trantype like 3 , 6 , 8 , 9
  *IF ((TRANTYPE='2'.OR.TRANTYPE='5'.OR.TRANTYPE='7').AND. ;
      BETWEEN(TRANDATE,LDATE,HDATE))
  *B606513,1 (Begin) Remark the next line and restore the old condition not to including chargeback.
  *IF ((TRANTYPE = '2'.OR.TRANTYPE='5'.OR.TRANTYPE='7').AND. ;
      BETWEEN(TRANDATE,LDATE,HDATE)) OR (TranType = '3' AND BETWEEN(TranDate,LDATE,HDATE) AND ;
      BETWEEN(ChgBk_Date,LDATE,HDATE)) OR (TranType = '6' AND BETWEEN(TranDate,LDATE,HDATE) AND ;
      BETWEEN(Credt_Date,LDATE,HDATE))
  IF ((TRANTYPE='2'.OR.TRANTYPE='5'.OR.TRANTYPE='7').AND. ;
      BETWEEN(TRANDATE,LDATE,HDATE))
  *B606513,1 (End)
  *B605369,1 Collect the charge back from Debit adjustment screen. [End]

    lcType = SPACE(1)
    =gfRltFld(ARHIST.TRANCODE , @laType , IIF(TRANTYPE='2','TRANCODE','CCREDITCOD')  )
    DO CASE
      CASE lcType = 'F'
        *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
        *XFALL=XFALL+AMOUNT
        XFALL=XFALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)
        *B603591,1 BWA 07/19/2000 [End]
      
      CASE lcType = 'D'
        *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
        *XDALL=XDALL+AMOUNT
        XDALL=XDALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)  
        *B603591,1 BWA 07/19/2000 [End]
        
      *B803225,1 BWA 06/26/2000 Fix the bug of the report give a wrong ending balance due to some
      *B803225,1                transaction type doesn't belong to any of the types the program checks for.[START]
      *CASE lcType = 'M'
      OTHERWISE
        *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
        *XMALL=XMALL+AMOUNT
        XMALL=XMALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)  
        *B603591,1 [END]

      *B803225,1 [END]

    ENDCASE        
  ENDIF

  *-- OPEN BALANCE --*
  IF (HISTDATE > ENDING) .AND. (TRANDATE <= ENDING)

    *** THE PERIOD CLOSING DATE
    IF TRANTYPE = '3' .AND. CHGBK_DATE>ENDING
      LOOP
    ENDIF

    IF TRANTYPE = '6' .AND. CREDT_DATE>ENDING
      LOOP
    ENDIF

    IF INLIST(TRANTYPE,'1','2','3')
      *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
      *XOPENDB = XOPENDB + AMOUNT
      XOPENDB = XOPENDB + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)
      *B603591,1 [END]
    ENDIF
    IF INLIST(TRANTYPE,'0','4','5','6')
      *B603591,1 BWA 07/19/2000 [Begin] Compute the amount depending on the currency.
      *XOPENCR = XOPENCR + AMOUNT
      XOPENCR = XOPENCR + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)
      *B603591,1 [END]
    ENDIF
  ENDIF
ENDSCAN

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/27/98
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfTempName()
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

*-- If select by date disable the month item, else enable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCMONTH'),1)
laOGObjCnt[lnPos] = lcrpSelBy # 'D'
= lfOGShowGet('LCMONTH')

*-- If select by date disable the year item, else enable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCYEAR'),1)
laOGObjCnt[lnPos] = lcrpSelBy # 'D'
= lfOGShowGet('LCYEAR')

*-- If select by date Enable the date item, else disable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LNDUMDATE'),1)
laOGObjCnt[lnPos] = lcrpSelBy = 'D'
= lfOGShowGet('LNDUMDATE')

*B603591,1 BWA 07/19/2000 Initialize a variable to hold the cusrrency code.[START]
lcCurrency = gcBaseCurr
*B603591,1 [END]
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/27/1998
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.spr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************

FUNCTION lfvDateRng

PRIVATE ldFrom,ldTo

ldFrom = LDATE
LDTO   = HDATE

lcTitle = 'Date range'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]




*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/27/1998
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbDateOk()
*!*************************************************************

*B803167,1 2- Fix the bug of get "lfvpbDateOk" does not exist.[START]
*B603685,1 Fix the bug of get "lfvPbOk" does not exist.[START]
*FUNCTION lfvpbDateOk
FUNCTION lfvPbOk
*B603685,1 [END]
*B803167,1 [END]

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  LDate = ldFrom
  HDate = ldTo
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/27/1998
*! Purpose   : Validate The select by (date/month) setting in the 
*!             option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbDateOk()
*!*************************************************************
FUNCTION lfvSelBy

IF lcrpSelBy = 'D'   && Select by date
  *-- Enable Date & disable month and year
  STORE SPACE(0) TO lcmonth, lcYear  
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCMONTH'),1)
  laOGObjCnt[lnPos] = .F.
  = lfOGShowGet('LCMONTH')
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCYEAR'),1)
  laOGObjCnt[lnPos] = .F.
  = lfOGShowGet('LCYEAR')
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LNDUMDATE'),1)
  laOGObjCnt[lnPos] = .T.
  = lfOGShowGet('LNDUMDATE')
ELSE                 && lcRpSelBy = 'M' Select by month
  *-- Enable month and year & disable date
  STORE {} TO lDate, hDate
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCMONTH'),1)
  laOGObjCnt[lnPos] = .T.
  = lfOGShowGet('LCMONTH')
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCYEAR'),1)
  laOGObjCnt[lnPos] = .T.
  = lfOGShowGet('LCYEAR')
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LNDUMDATE'),1)
  laOGObjCnt[lnPos] = .F.
  = lfOGShowGet('LNDUMDATE')
ENDIF


*B603591,1 BWA 07/19/2000 Make Activity summary report consider the different types of Currencies.[START]
*!*************************************************************
*! Name      : lfvCurCode
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/19/2000
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
FUNCTION lfvCurCode
lnAlias=SELECT(0)

*-- Allow blank curreny in case user doesn't selected forign currency.
IF EMPTY(lcCurrency) .AND. lcRpCurr <> 'F' 
  RETURN
ENDIF

IF !SEEK(lcCurrency,'SYCCURR') .OR. ATC("?",lcCurrency) > 0
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    lcCurrency = lcOldCurr
  ELSE
    lcCurrency = laTemp[1]
  ENDIF
ENDIF

SHOW GET lcCurrency

SELECT(lnAlias)
*--End of lfvCurCode.
*!*************************************************************
*! Name      : lfwCurCode
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/19/2000
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
FUNCTION lfwCurCode

lcOldCurr = gcBaseCurr

*--End of lfwCurCode.
*B603591,1 [END]
