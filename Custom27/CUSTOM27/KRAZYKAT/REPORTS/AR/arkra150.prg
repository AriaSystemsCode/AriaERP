*:************************************************************************
*: Program file  : ARKRA150.Prg (Converted from 26 to 27)
*: Program desc. : Activity Summary Report
*: System        : Aria Advantage Series VER. 2.7
*: Module        : AR
*: Developer     : Adel Mohammed El Gazzar (ADEL)
*: Date          : 10/23/1999
*: Reference     : (C101637)
*: Note          : Copied from ARACTSUM "Standard" with some modifications.
*:************************************************************************
*: Calls : 
*:    Procedures : CALCULATE()
*:    Functions  : lfwOGWhen(), lfvDateRng(), lfvPbOk(), lfvSelBy()
*:               : lfChgback(), lfHistChg()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO ARACTSUM
*:************************************************************************
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
**************************************************************************
*: Modifications
*:B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*:B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*:B603997,1 BWA 10/31/2000 1) Fix the bug of month and year fields becomes empty when selecting month after date.
*:B603997,1                2) Modify the text of the wait window.
*:***************************************************************************
*--C101637 Date range restrictions.

IF (lcRpSelBy = 'D' .AND. EMPTY(hDate))
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'You must enter a date range .Please try again!')
  RETURN  
ENDIF

*--C101637 Month restrictions.
IF lcRpSelBy = 'M' .AND. EMPTY(lcMonth)
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Month must be greater than 0 .Please try again!')
  RETURN  
ENDIF  

*--C101637 Open ARHIST with another alias and order.
IF !USED('ARHIST_A')
  USE (gcDataDir+'ARHIST')  AGAIN ALIAS ARHIST_A ORDER TAG ARHISTT IN 0
ENDIF  

*-- If year value is empty , fill it with the current year.
IF lcRpSelBy = 'M' .AND. EMPTY(lcYear)
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
lctype = SPACE(1)
laType[1,1] = 'ALLOW_TYPE'      
laType[1,2] = 'lctype'
R_WIDTH= 'W'             && STANDARD REPORT 'WIDE'
XTIME  = SUBSTR(TIME(),1,5)
R_TITLE= "ACTIVITY SUMMARY REPORT"
llNotFound =.T.             && Indicate if there are records to print or not.
*-- If select by month, form the from&to dates as beginning and ending 
*-- of that month
IF lcRpSelBy = 'M' 
  LcX    = lcMONTH+'/'+'01'+'/'+PADL(lcYEAR,4,'0')
  LDATE  = CTOD(LcX)
  NDATE  = LDATE+32
  HDATE  = NDATE-(DAY(NDATE))
  ENDING = LDATE-(DAY(LDATE))
ELSE
  ENDING = LDATE - 1
ENDIF
XTITLE='PERIOD '+ DTOC(LDATE) + ' --- ' + DTOC(HDATE)

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*AR980                                                                                                                   MM/DD/YY
*TIME: HH:SS                                               ACTIVITY SUMMARY REPORT                                       PAGE: 1234
*
*Acct# Name                   Open. Bal         Sales      Payments       Returns Freight All.   Disc. All.   Misc. All.   Ending Bal.
*12345 123456789012345678 99,999,999.99 99,999,999.99 99,999,999.99 99,999,999.99 9,999,999.99 9,999,999.99 9,999,999.99 99,999,999.99
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

SELE CUSTOMER
=SEEK('M')
SCAN WHILE INKEY() <> 32 AND TYPE='M' FOR &lcRpExp
  *--C101637 Add wait window
  
  *B603997,1 BWA 10/31/2000 Modify the text of the wait window.[START]
  *WAIT WINDOW 'Printing Account: " + ACCOUNT + " - <SPACE BAR> to abort' NOWAIT
  WAIT WINDOW "Printing Account: " + ACCOUNT NOWAIT
  *B603997,1 [END]

  STORE 0 TO XOPENBAL,XSALES,XPAY,XRET,XFALL,XDALL,XMALL,XBAL,XOPENDB,XOPENCR
  IF ROW >=54
    *-- Print header of the report.
    PAGENO = PAGENO +1
    DO RPT_HDR WITH 'AR980',XTITLE,R_WIDTH
    @ 05,000 SAY 'Acct# Name                  Open Bal.         Sales      Payments       Returns Freight All.   Disc. All.   Misc. All.   Ending Bal.'  && YMA 02/05/94
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
  SUM REST TOTALCHG TO XSALES WHILE (XACCOUNT=ACCOUNT) ;
      FOR BETWEEN(INVDATE,LDATE,HDATE) AND ;
         (SEEK(ACCOUNT+INVOICE,"DEBIT") OR SEEK(ACCOUNT+INVOICE,"ARHIST"))

  *--------R E T U R N S----------------
  IF USED('RETHDR')
    SELE RETHDR
    =SEEK(XACCOUNT)
    SUM REST TOTCREDIT TO XRET WHILE XACCOUNT=ACCOUNT ;
                             FOR   BETWEEN(CRDATE,LDATE,HDATE)       
    XRET=(-1*XRET)
  ENDIF
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
    llNotFound = .F.
    @ ROW,000 SAY CUSTOMER.ACCOUNT
    @ ROW,006 SAY SUBSTR(CUSTOMER.BTNAME,1,17)
    @ ROW,024 SAY XOPENBAL PICTURE '99,999,999.99'
    @ ROW,038 SAY XSALES   PICTURE '99,999,999.99'     
    @ ROW,052 SAY XPAY     PICTURE '99,999,999.99'
    @ ROW,066 SAY XRET     PICTURE '99,999,999.99'
    @ ROW,080 SAY XFALL    PICTURE '9,999,999.99'
    @ ROW,093 SAY XDALL    PICTURE '9,999,999.99'
    @ ROW,106 SAY XMALL    PICTURE '9,999,999.99'
    @ ROW,119 SAY XBAL     PICTURE '99,999,999.99'
    ROW=ROW+1
  ENDIF
ENDSCAN
WAIT CLEAR

IF llNotFound
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
ELSE
  *---------- PRINT GRAND TOTAL ---------------
  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE ('*',132)
  ROW = ROW + 1
  @ ROW,00  SAY '*** GRAND TOTAL ***'
  @ ROW,024 SAY XTOTOPEN  PICTURE '99,999,999.99'
  @ ROW,038 SAY XTOTSALES PICTURE '99,999,999.99'
  @ ROW,052 SAY XTOTPAY   PICTURE '99,999,999.99'
  @ ROW,066 SAY XTOTRET   PICTURE '99,999,999.99'
  @ ROW,080 SAY XTOTFALL  PICTURE '9,999,999.99'
  @ ROW,093 SAY XTOTDALL  PICTURE '9,999,999.99'
  @ ROW,106 SAY XTOTMALL  PICTURE '9,999,999.99'
  @ ROW,119 SAY XTOTBAL   PICTURE '99,999,999.99'
  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE ('*',132)
  DO ENDREPORT         && END THE REPORT OR DISPLAY ON SCREEN
ENDIF
SET DEVICE TO SCREEN
*--

*!*************************************************************
*! Name      : Calculate
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/23/1999
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
SCAN WHILE ACCOUNT=XACCOUNT

  *-- OPEN BALANCE --*
  XDFOR = IIF( TRANTYPE='3','TRANDATE<=ENDING .AND. CHGBK_DATE<=ENDING',;
                             'TRANDATE<=ENDING')
  IF &XDFOR
    XOPENDB = XOPENDB+AMOUNT
  ENDIF

  *---- ADJUSTMENT ---*
  *-- C101637
  IF (TRANTYPE='2' OR lfChgback()) .AND. BETWEEN(TRANDATE,LDATE,HDATE))
    lctype = SPACE(1)
    =gfRltFld(DEBIT.TRANCODE , @laType , 'TRANCODE')
    DO CASE
      CASE lctype = 'F'
        XFALL=XFALL+AMOUNT
      CASE lctype = 'D'
        XDALL=XDALL+AMOUNT
      CASE lctype = 'M'
        XMALL=XMALL+AMOUNT
    ENDCASE        
  ENDIF
ENDSCAN

*-------------------------------- C R E D I T -------------------------------*
SELECT CREDIT

SEEK XACCOUNT
SCAN WHILE ACCOUNT=XACCOUNT
  *-- OPEN BALANCE --*
  XCFOR = IIF( TRANTYPE='6','TRANDATE<=ENDING .AND. CREDT_DATE<=ENDING',;
                            'TRANDATE<=ENDING')
  IF &XCFOR
    XOPENCR=XOPENCR+AMOUNT
  ENDIF
  *---- PAYMENTS ----*
  IF (TRANTYPE='4' .AND. BETWEEN(TRANDATE,LDATE,HDATE))
    XPAY=XPAY+AMOUNT
  ENDIF

  *---- ADJUSTMENT ---*
  IF (TRANTYPE='5'.AND. BETWEEN(TRANDATE,LDATE,HDATE))
    lctype = SPACE(1)
    =gfRltFld(CREDIT.cCreditCod , @laType , 'CCREDITCOD')
    DO CASE
      CASE lctype = 'F'
        XFALL=XFALL+AMOUNT
      CASE lctype = 'D'
        XDALL=XDALL+AMOUNT
      CASE lctype = 'M'
        XMALL=XMALL+AMOUNT
    ENDCASE        
  ENDIF
ENDSCAN

*-------------------------------- A R H I S T -------------------------------*

SELE ARHIST
SEEK XACCOUNT
SCAN WHILE ACCOUNT=XACCOUNT
  *-- PAYMENTS --*
  IF (TRANTYPE='4'.AND.BETWEEN(TRANDATE,LDATE,HDATE))
    XPAY=XPAY+AMOUNT
  ENDIF

  *---- ADJUSTMENT ---*
  *-- C101637
  IF (BETWEEN(TRANDATE,LDATE,HDATE) AND ;
     (TRANTYPE='2'.OR.TRANTYPE='5'.OR.TRANTYPE='7' OR lfHistChg()))
      
    lctype = SPACE(1)
    =gfRltFld(ARHIST.TRANCODE , @laType , IIF(TRANTYPE='2','TRANCODE','CCREDITCOD')  )
    DO CASE
      CASE lctype = 'F'
        XFALL=XFALL+AMOUNT
      CASE lctype = 'D'
        XDALL=XDALL+AMOUNT
      CASE lctype = 'M'
        XMALL=XMALL+AMOUNT
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
      XOPENDB = XOPENDB + AMOUNT
    ENDIF
    IF INLIST(TRANTYPE,'0','4','5','6')
      XOPENCR = XOPENCR + AMOUNT
    ENDIF
  ENDIF
ENDSCAN

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/23/1999
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
laOGObjCnt[lnPos] = lcRpSelBy # 'D'
= lfOGShowGet('LCMONTH')

*-- If select by date disable the year item, else enable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCYEAR'),1)
laOGObjCnt[lnPos] = lcRpSelBy # 'D'
= lfOGShowGet('LCYEAR')

*-- If select by date Enable the date item, else disable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LNDUMDATE'),1)
laOGObjCnt[lnPos] = lcRpSelBy = 'D'
= lfOGShowGet('LNDUMDATE')


*!*************************************************************
*! Name      : lfvDateRng
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/23/1999
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
*! Name      : lfvPbOk
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/23/1999
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
*! Example     : = lfvPbOk()
*!*************************************************************

FUNCTION lfvPbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  LDate = ldFrom
  HDate = ldTo
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfvSelBy
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/23/1999
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
FUNCTION lfvSelBy

IF lcRpSelBy = 'D'   && Select by date
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

  *B603997,1 BWA 10/31/2000 1) Fix the bug of month and year fields becomes empty when selecting month after date.[START]
  lcmonth = STR(MONTH(DATE())-1,2)
  lcYear  = STR(IIF(MONTH(DATE())-1<>12,YEAR(DATE()),(YEAR(DATE())-1)),4)
  *B603997,1 [END]

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

*!***************************************************************************
*! Name      : lfHistChg
*! Developer : Sherif Attala Ishak
*! Date      : 06/27/97
*! Purpose   : Pick the charge backs from ARHIST that were not created as a
*!             result of apply debit/credit .
*!***************************************************************************
*! Calls     : 
*!***************************************************************************
*! Passed Parameters  : None
*!***************************************************************************
*! Returns            : None 
*!***************************************************************************
*! Example            : lfHistChg()
*!***************************************************************************
FUNCTION lfHistChg
RETURN (TranType = "3" AND Seek(Account+Tran,"ARHIST_A") AND ARHIST_A.TranType = "3")
*--

*!***********************************************************************
*! Name      : lfChgback
*! Developer : Sherif Attala Ishak
*! Date      : 06/27/97
*! Purpose   : Pick the charge backs from DEBIT file that don't have 
*!			   entries in ARHIST file.	
*!***********************************************************************
*! Calls     : 
*!***********************************************************************
*! Passed Parameters  : None
*!***********************************************************************
*! Returns            : None 
*!***********************************************************************
*! Example            : lfChgback()
*!***********************************************************************
FUNCTION lfChgback
RETURN TranType = "3" AND !Seek(Account+Tran,"ARHIST_A")
*-- 