*:**************************************************************************
*: Program file        : ARCSCRSC.PRG
*: Program desc.       : CUSTOMIZED CUSTOMER CREDIT REPORT FOR SOPE CREEK.
*: Date                : 05/10/2001
*: System              : Aria Advantage Series.
*: Module              : ACCOUNT RECEIVABLE (AR)
*: Developer           : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number : C102242
*:**************************************************************************
*: Calls :
*:    Procedures : lpRpPrint
*:    Functions  : lfvAcct() , lfCollTime()
*:**************************************************************************
*: Example : DO ARCSCRSC
*:**************************************************************************
lcStTime   = TIME()     && Variable to collect the start time
STORE .F. TO llData     && Check if there is data to print.

*-- get the fixed filters in OG to make optimized expressions [Begin.]
*-- Account Filter.
IF EMPTY(laOGFxFlt[1,6])  && if account empty store full index expr.
  lcAcctOnly = [type+account+store = 'M']
ELSE    && not empty add type,store to account to make full index expr.
  lcLeftAcc  = 'M' + SUBSTR(laOGFxFlt[1,6],1,ATC('|',laOGFxFlt[1,6]) - 1)
  lcRightAcc = 'M' + SUBSTR(laOGFxFlt[1,6],ATC('|',laOGFxFlt[1,6]) + 1)
  lcAcctOnly = [BETWEEN(type+account+store,'&lcLeftAcc.','&lcRightAcc.')]
ENDIF

*-- Terms Filter
IF EMPTY(laOGFxFlt[2,6]) && if payment terms empty Store null in filter expr. 
  lcTermFilt = ""
ELSE                     && if not empty add it to filter expr.
  lcTermFilt  = laOGFxFlt[2,6]
  lcTermFilt  = [cTermCode='&lcTermFilt.']
ENDIF

*-- filter expr. is optimized when payment terms is ALL (no terms selected) , else it is 
*-- partially optimized.
lcFiltExpr = lcAcctOnly + IIF(EMPTY(lcTermFilt),'',[ AND ] + lcTermFilt)
*-- get the fixed filters in OG to make optimized expressions [End.]

SELECT CUSTOMER
SET FILTER TO &lcFiltExpr   && collect only data that match Customer criteria.
GO TOP

*-- if no data match Criteria .
IF EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN

ELSE      && else if we have data then start printing report code.

  *-- Start the report.
  ROW    = 99
  PAGENO = 0  
  R_WIDTH ='W'
  R_TITLE = 'Customized Customer Credit Report.'

  *-- XUNSHPM : Print unShipped amount.
  DIMENSION XTOT(3)  && Array that hold totals.
  STORE 0 TO XTOT , XUNSHPM

  *-- PRINT THE REPORT.
  CLEAR TYPEAHEAD
  SET DEVICE TO PRINT

  DO lpPrint   && main print report procedure.
  IF llData
    DO ENDREPORT
  ELSE
    = gfModalGen('TRM00052B00000','DIALOG' )
    SET DEVICE TO SCREEN
  ENDIF
  SET DEVICE TO SCREEN

ENDIF  && end if no data match Criteria .

                          *-- End of Report code.
*!**************************************************************************
*! Name      : lpPrint
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/10/2001
*! Purpose   : Printing loop for the main report.
*!**************************************************************************
*! Called from : Main Program (ARCSCRSC)
*!**************************************************************************
*! Calls       : gfCodDes()
*!**************************************************************************
*! Example     : Do lpPrint
*!**************************************************************************
PROCEDURE lpPrint
*--Printing loop

SELECT CUSTOMER
*-- loop filterd accounts
SCAN
  WAIT WINDOW NOWAIT "Collecting Data For Account: " +Account

  *-- if page over to next page
  IF ROW > 55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH 'ARCSCRSC','',R_WIDTH
    @ 05,01 SAY 'Acct# Name.............. City........... State. Zip....... Terms...................      Credit   Unshipped      Unpaid        Open'
    @ 06,01 SAY '                                                                                          Limit Merchandise    Invoices      Credit'
    @ 07,00 SAY REPLICATE('=',132)
    ROW = 8
  ENDIF  && end if page over to next page.

*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3...
*ACCT# NAME.............. CITY........... STATE. ZIP....... TERMS.....  CREDIT   UNSHIPPED      UNPAID        OPEN
*                                                                       LIMIT MERCHANDISE    INVOICES      CREDIT
*12345 123456789012345678 123456789012345 123456 1234567890 1234567890 1234567  1234567.99  1234567.99  1234567.99
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

  *-- Calculate unshipped amount.

  SELECT ORDHDR    && no active order for ORHDHR in OG and that for Rush-More
  *-- Use full index expression with for condition in sum command to activate Rush-More (Partial because of Status)

  =SEEK(Customer.Account)
  XUNSHPM=0
  SCAN WHILE Account+cOrdType+Order = Customer.Account FOR STATUS $ 'OH'
    IF cCurrCode = gcBaseCurr
      XUNSHPM = XUNSHPM + OpenAmt
    ELSE
      XUNSHPM = XUNSHPM + gfAmntDisp(OpenAmt,"O",Entered)
    ENDIF
  ENDSCAN

  *-- Calculate open credit line.
  SELECT CUSTOMER

  *-- Credit Line - Unshipped Merchandise - Open Invoices (NETBAL)
  XOPENCR = CRLIMIT - XUNSHPM - NETBAL

  *--Case the user select customer negative credit value get the (-) credit value OR all the values.
  IF (llRpNgtiv AND XOPENCR < 0 ) OR !llRpNgtiv
    llData = .T.
    *-- Say to printer.
    @ ROW,01  SAY ACCOUNT
    @ ROW,07  SAY SUBSTR(BTNAME,1,18)
    @ ROW,26  SAY PADR(CADDRESS32,15)
    @ ROW,42  SAY PADR(CADDRESS42,6)
    @ ROW,49  SAY PADR(CADDRESS52,10)
    @ ROW,60  SAY SUBSTR(gfCodDes(CTERMCODE,'CTERMCODE',.T.),1,24)
    @ ROW,85  SAY CRLIMIT  PICTURE '99999999999'
    IF llPrnDec
      @ ROW,97  SAY XUNSHPM  PICTURE '99999999.99'
      @ ROW,109  SAY NETBAL  PICTURE '99999999.99'
      @ ROW,121 SAY XOPENCR  PICTURE '99999999.99'
    ElSE
      @ ROW,97  SAY XUNSHPM  PICTURE '99999999999'
      @ ROW,109  SAY NETBAL  PICTURE '99999999999'
      @ ROW,121 SAY XOPENCR  PICTURE '99999999999'
    ENDIF
    ROW = ROW + 1

    *-- Calculate Grand totals.
    XTOT(1) = XTOT(1) + XUNSHPM
    XTOT(2) = XTOT(2) + NETBAL
    XTOT(3) = XTOT(3) + XOPENCR
  ENDIF
ENDSCAN  && end loop filterd accounts.

WAIT CLEAR

IF llData
  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE ('*',132)
  ROW = ROW + 1
  @ ROW,01 SAY '*** GRAND TOTAL ***'

  *-- Say Grand totals.
  IF llPrnDec
    @ ROW,97  SAY XTOT(1)     PICTURE '99999999.99'
    @ ROW,109 SAY XTOT(2)     PICTURE '99999999.99'
    @ ROW,121 SAY XTOT(3)     PICTURE '99999999.99'
  ELSE
    @ ROW,97  SAY XTOT(1)     PICTURE '99999999999'
    @ ROW,109 SAY XTOT(2)     PICTURE '99999999999'
    @ ROW,121 SAY XTOT(3)     PICTURE '99999999999'
  ENDIF
  ROW = ROW + 1

  @ ROW,00 SAY REPLICATE ('*',132)
  ROW = ROW + 1

  lcEdTime = TIME()  && Time in which we finish collect data.
  lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
  WAIT WINDOW NOWAIT 'Total time is ' +  ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...'
ENDIF
*-- End of lpPrint.
*!**************************************************************************
*! Name      : lfvAcct
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/10/2001
*! Purpose   : Validation function for the Customer Account field
*!**************************************************************************
*! Called from : Customer Account field [Option Grid]
*!**************************************************************************
*! Calls       : CusBrowM()
*!**************************************************************************
*! Example     : =lfvAcct()
*!**************************************************************************
FUNCTION lfvAcct
PRIVATE lcObjNam , lcObjVal

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Account he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK('M'+lcObjVal , 'CUSTOMER')
  llBrowse = .T.
  lcAccount = lcObjVal
  DO CUSBROWM WITH lcAccount
  lcObjVal = lcAccount
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal

*-- End of lfvAcct.
*!**************************************************************************
*! Name      : lfCollect
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/10/2001
*! Purpose   : Calcualte spent time in data collection.
*!**************************************************************************
*! Called from : Report code section.
*!**************************************************************************
*! Passed Parameters : Start collection date,End collection date
*!**************************************************************************
*! Returns           : Spent time.
*!**************************************************************************
*! Example   : =lfCollTime()
*!**************************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd

lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))

RETURN (lnEnd - lnStart)

*-- End of lfCollTime.