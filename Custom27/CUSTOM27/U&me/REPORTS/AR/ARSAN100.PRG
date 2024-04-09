*:***************************************************************************
*: Program file  : ARSAN100.PRG
*: Program desc. : CUSTOMIZED ASSIGNMENT SCHEDULE TO THE FACTOR FOR YOU&ME.
*! Date          : 06/02/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C102175
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARSAN100
*:***************************************************************************

*-- Section to check if there is FABRIC value selected or not.
*-- And the date is not empty.
IF EMPTY(ldRPPHDate)
  WAIT WINDOW "Date range is mandatory ! Please retry "
  SET DEVICE TO SCREEN
  RETURN
ENDIF

IF EMPTY(lcRpSpeFac)
  WAIT WINDOW "Factor code cannot be empty! try again. "
  SET DEVICE TO SCREEN
  RETURN
ENDIF

*--Clear relations
SELE INVHDR
SET RELA TO

DO lpSetFiltr         &&Set and build the filters due to grid var.

LOCATE
IF EOF()
  WAIT CLEAR
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

WAIT CLEAR
xTitle  = IIF(SEEK(lcRpSpeFac , 'SYCFACT') , SYCFACT.CFACCOMP , SPACE(0)) + IIF(lcRpForm = 'A' , ' '+' ASSIGNMENT NO.________' , ' '+' ASSIGNMENT NO. ' + lcAssSht)
R_TITLE = 'ASSIGNMENT SCHEDULE'
R_WIDTH = IIF(lcRpForm = 'A','N','W')
Row     = 99
PageNo  = 0

SET DEVICE TO PRINT
DO lpForm&lcRpForm
DO ENDREPORT
SET DEVICE TO SCREEN
                       *--End of the program --*
*!*************************************************************
*! Name      : lpSetFiltr
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/02/2001
*! Purpose   : reconstruct filter expression
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpSetFiltr
*!*************************************************************
PROCEDURE lpSetFiltr

SELECT InvHdr
SET RELATION TO 'M'+Account INTO CUSTOMER

lcFilter = IIF(EMPTY(lcRpexp) ," .T. ", lcRpexp)
lcFilter = lcFilter + " AND " + "Status <> 'V' .AND. cFaccode = lcRpSpeFac .AND. BETWEEN(INVHDR.InvDate,ldRPPLDate,ldRPPHDate)"

WAIT WINDOW 'Searching the file for invoices that meet the date range......' NOWAIT
SET FILTER TO &lcFilter

*--End of lpSetFiltr.
*!*************************************************************
*! Name      : lpFormA
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/02/2001
*! Purpose   : Report format A.
*!*************************************************************
*! Called from : ARSAN100.PRG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpFormA
*!*************************************************************
PROCEDURE lpFormA

*!********************************************************************************
*!* INVOICE| INVOICE|  NAME OF CUSTOMER / |TERMS    |AMOUNT      | CREDIT APPR.  *
*!* DATE   | NUMBER |  ADDRESS            |         |            | NUMBER        *
*!********************************************************************************

NUM     = 0
lnTotal = 0
DO WHILE !EOF()

  IF Row >55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH 'ARSAN100',XTITLE,R_WIDTH
    @ 06,00 SAY ' INVOICE| INVOICE|  NAME OF CUSTOMER / |TERMS      |AMOUNT      | CREDIT APPR.'
    @ 07,00 SAY ' DATE   | NUMBER |  ADDRESS            |           |            | NUMBER      '
    @ 08,00 SAY REPLICATE('*',80)
    Row = 10
  ENDIF

  WAIT WINDOW 'PRINTING INVOICE#  ' + Invoice NOWAIT

  * LINE 1
  @ Row,01 SAY INVDATE
  @ Row,11 SAY INVOICE
  @ Row,18 SAY SUBSTR(CUSTOMER.BTNAME,1,19)
  lcTerms   = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
  @ Row,40 SAY LEFT(lcTerms,10)
  @ Row,51 SAY TOTALCHG PICTURE '9999999999.99'
  @ Row,65 SAY APPROVAL
  Row = Row + 1

  * LINE 2
  @ Row,18 SAY CUSTOMER.Caddress12
  Row = Row + 1

  * LINE 3
  @ Row,18 SAY CUSTOMER.Caddress22
  Row=Row+1

  * LILNE 4
  @ Row,18 SAY LEFT(CUSTOMER.Caddress32,15) +' '+ LEFT(CUSTOMER.Caddress42,3) +' '+ LEFT(CUSTOMER.Caddress52,10)

  Row = Row + 1
  @ Row,00 SAY REPLICATE ('.',79)

  lnTotal = lnTotal + TOTALCHG

  SELE INVHDR
  SKIP
  Row = Row + 1
  NUM = NUM + 1
ENDDO

Row = Row + 1
@ Row,00 SAY REPLICATE('*',80)
Row = Row + 1
@ Row,00 SAY '*** GRAND TOTAL ***'
@ Row,49 SAY lnTotal PICTURE '$99999999999.99'
Row = Row + 1
@ Row,00 SAY REPLICATE('*',80)
Row=Row+1
@ Row,00 SAY ' '
RETURN

*--End of lpFormA.
*!*************************************************************
*! Name      : lpFormB
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/02/2001
*! Purpose   : Report format B.
*!*************************************************************
*! Called from : ARSAN100.PRG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpFormB
*!*************************************************************
PROCEDURE lpFormB

SELECT InvHdr
*--Print report sorted by account.
SET ORDER TO TAG InvHdrA
SUM TotalChg TO lnTotInv

LOCATE
STORE 0.00 TO lnSTotal1,lnSTotal2,lnSTotal3,lnSTotal4,lnSTotal5,lnSTotal6
STORE 0.00 TO lnGTotal1,lnGTotal2,lnGTotal3,lnGTotal4,lnGTotal5,lnGTotal6

lcAccount = Account
SCAN WHILE .T.
  IF Row >55
    PageNo = PageNo + 1
    DO RPT_HDR WITH 'ARSAN100',xTitle,R_Width
    @ 05,000 SAY ' ACCOUNT NAME.......................... INVOICE INV DATE     GROSS AMT TRADE DISC FACTOR FEE    RESERVE OTHER CHRG           NET'
    @ 06,000 SAY REPLICATE('*',132)
    Row = 07
  ENDIF

  WAIT WINDOW 'PRINTING INVOICE# ' + Invoice NOWAIT

  IF Account <> lcAccount
    DO lpSubTot
  ENDIF

  lnTradeDis = ( (Trde_Disc/100)*TotalChg )
  lnFactFee  = (TotalChg - lnTradeDis)*(lnFactCom/100)
  lnReserve  = (TotalChg - lnTradeDis)*(lnReserPs/100)
  lnOthers   = (TotalChg / lnTotInv ) * lnOterChg
  lnNet      = TotalChg - (lnTradeDis + lnFactFee + lnReserve + lnOthers)

  @ Row,001 SAY Account
  @ Row,009 SAY Customer.BtName
  @ Row,040 SAY Invoice
  @ Row,048 SAY InvDate
  @ Row,057 SAY TotalChg   PICTURE '9999999999.99'
  @ Row,071 SAY lnTradeDis PICTURE '9999999.99'
  @ Row,082 SAY lnFactFee  PICTURE '9999999.99'
  @ Row,093 SAY lnReserve  PICTURE '9999999.99'
  @ Row,104 SAY lnOthers   PICTURE '9999999.99'
  @ Row,115 SAY lnNet      PICTURE '9999999999.99'

  Row = Row + 1

  *--Acomulation for Sub total.
  lnSTotal1 = lnSTotal1 + TotalChg
  lnSTotal2 = lnSTotal2 + lnTradeDis
  lnSTotal3 = lnSTotal3 + lnFactFee
  lnSTotal4 = lnSTotal4 + lnReserve
  lnSTotal5 = lnSTotal5 + lnOthers
  lnSTotal6 = lnSTotal6 + lnNet

ENDSCAN

DO lpSubTot
DO lpGrndTot
RETURN

*--End of lpFormB.
*!*************************************************************
*! Name      : lpSubTot
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/02/2001
*! Purpose   : Procedure to calculate the sub total.
*!*************************************************************
*! Called from : ARSAN100.PRG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpSubTot
*!*************************************************************
PROCEDURE lpSubTot

*--Print sub total..
@ Row,000 SAY REPLICATE ('.',128)
Row = Row + 1
@ Row,000 SAY '*** SUBTOTAL : &lcAccount ***'
@ Row,056 SAY lnSTotal1 PICTURE '$9999999999.99'
@ Row,071 SAY lnSTotal2 PICTURE '9999999.99'
@ Row,082 SAY lnSTotal3 PICTURE '9999999.99'
@ Row,093 SAY lnSTotal4 PICTURE '9999999.99'
@ Row,104 SAY lnSTotal5 PICTURE '9999999.99'
@ Row,115 SAY lnSTotal6 PICTURE '9999999999.99'

Row = Row + 1
@ Row,000 SAY REPLICATE ('.',128)
Row = Row + 1
lcAccount = Account
*--Acomulation for grand total.
lnGTotal1 = lnGTotal1 + lnSTotal1
lnGTotal2 = lnGTotal2 + lnSTotal2
lnGTotal3 = lnGTotal3 + lnSTotal3
lnGTotal4 = lnGTotal4 + lnSTotal4
lnGTotal5 = lnGTotal5 + lnSTotal5
lnGTotal6 = lnGTotal6 + lnSTotal6

STORE 0.00 TO lnSTotal1,lnSTotal2,lnSTotal3,lnSTotal4,lnSTotal5,lnSTotal6
RETURN

*--End of lpSubTot.
*!*************************************************************
*! Name      : lpGrndTot
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/02/2001
*! Purpose   : Procedure to calculate the grand total.
*!*************************************************************
*! Called from : ARSAN100.PRG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpGrndTot
*!*************************************************************
PROCEDURE lpGrndTot

*--Print grand total..
Row = Row + 1
@ Row,000 SAY  REPLICATE('*',132)
Row = Row + 1
@ Row,000 SAY '*** GRAND TOTAL       ***'
@ Row,056 SAY lnGTotal1 PICTURE '$9999999999.99'
@ Row,071 SAY lnGTotal2 PICTURE '9999999.99'
@ Row,082 SAY lnGTotal3 PICTURE '9999999.99'
@ Row,093 SAY lnGTotal4 PICTURE '9999999.99'
@ Row,104 SAY lnGTotal5 PICTURE '9999999.99'
@ Row,115 SAY lnGTotal6 PICTURE '9999999999.99'

Row=Row+1

@ Row,000 SAY '*** ALLOWED ALLOWANCE ***'
@ Row,056 SAY (lnGTotal6 * (lnPrmRate+lnAbvPrmRt))/100 PICTURE '$9999999999.99'
Row=Row+1
@ Row,000 SAY  REPLICATE('*',132)
Row=Row+1

@ Row,00 SAY ' '
RETURN

*--End of lpGrndTot.
*!*************************************************************
*! Name      : lfAccount
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/02/2001
*! Purpose   : Account in range browse screen.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfAccount()
*!*************************************************************
*! Note      : S symbol is [S,Set] ,R symbol is [R ,Reset]
*!*************************************************************
FUNCTION lfAccount
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
ENDCASE

*--End of lfAccount.
*!*************************************************************
*! Name      : lfvFactor
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 06/02/2001
*! Purpose   : TO validate a specific factor.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvFactor()
*!*************************************************************
FUNCTION lfvFactor
PRIVATE lnAlias

lnAlias = SELECT()
SELECT CUSTOMER
IF !EMPTY(lcRPSpeFac) .AND. !SEEK ('F'+lcRPSpeFac)
  llNoThing = lfGetFac (lcRPSpeFac)
ENDIF
IF EMPTY(lcRPSpeFac)
  WAIT WINDOW 'Factor Code Can Not Be Empty! Try Again.'
  RETURN .F.
ENDIF

SELECT(lnAlias)

*--End of lfvFactor.
*!*************************************************************
*! Name      : lfwRunGrid
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 06/02/2001
*! Purpose   : valid function when run grid.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfwRunGrid()
*!*************************************************************
FUNCTION lfwRunGrid

lcRpSpeFac = SYCFACT.CFACCODE

*-- Check if SYCFACT has no records then quit report.
IF EMPTY(lcRpSpeFac)
  *-- No factors found
  *-- < Ok >
  = gfModalGen('TRM40100B00000')
  llOgTrmnat = .T.         && Globale variable in the option grid program.
  CLEAR READ
ENDIF

*--End of lfwRunGrid.
*!*************************************************************
*! Name      : lfClrRead
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 06/02/2001
*! Purpose   : To change option view to see some records or not.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfClrRead()
*!*************************************************************
FUNCTION lfClrRead

IF lcRpForm = "A"
  STORE .F. TO lcCmpAlwnc
ENDIF
CLEAR READ

*--End of lfClrRead.
*!*************************************************************
*! Name      : lfvGreater
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 06/02/2001
*! Purpose   : To validate the number of the comming fields.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfvGreater()
*!*************************************************************
FUNCTION lfvGreater
PARAMETERS LCVARSAN

IF &lcVARSAN < 0
  DO CASE
    CASE lcVARSAN = "lnFactCom"
      WAIT WINDOW "Factor commission % value could not be negative"

    CASE lcVARSAN = "lnReserPs"
      WAIT WINDOW "Reserve withheld % value could not be negative"

    CASE lcVARSAN = "lnOterChg"
      WAIT WINDOW "Other Charges value could not be negative"

    CASE lcVARSAN = "lnPrmRate"
      WAIT WINDOW "Prime rate value could not be negative"

    CASE lcVARSAN = "lnAbvPrmRt"
      WAIT WINDOW "Above Prime rate value could not be negative"

  ENDCASE
  &LCVARSAN = 0
ENDIF

*--End of lfvGreater.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/02/2001
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DATERNG.SPX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng
PRIVATE ldFrom,ldTo

ldFrom = ldRPPLDate
ldTo   = ldRPPHDate
lcTitle = 'Invoice Date'

DO DATERNG.SPX

ldRPPLDate = ldFrom
ldRPPHDate = ldTo

*--End of lfvDateRng.
*!*************************************************************
*! Name      : lfvPbOk
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/02/2001
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : DATERNG.SPX
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
  ldRPPLDate = ldFrom
  ldRPPHDate = ldTo
  CLEAR READ
ENDIF

*--End of lfvPbOk.