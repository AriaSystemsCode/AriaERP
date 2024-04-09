*:***************************************************************************
*: Program file  : ARCSPHIS.PRG
*: Program desc. : CUSTOMIZED CUSTOMER PAYMENT HISTORY REPORT.
*: 				   The program is a conversion from 2.6 ..DAN700.PRG Customer: DANA-UNDIES (REFERE TO C101075)
*! Date          : 05/23/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C101781
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARCSPHIS
*:***************************************************************************
              *--Variables in the hidden filter in report generator--*
*1) In hidden filter : Customer.status = "A"

              *--Types of the transaction type for the history file--*
*******************************************************************           
*!      Type  Meaning
*!      0     Returns (Credit Memo).
*!      1     Invoice, Direct Invoice.
*!      2     Debit Adjustment.
*!      3     Charge Back (Debit On Account).
*!      4     Payment.
*!      5     Credit Adjustment.
*!      6     Credit On Account.
*!      7     Allowance (Credit Adjustment in this program).
*!      8     Charge Back (Only in the ARHIST).
*!      9     Credit On Account (Only in the ARHIST).
*******************************************************************
              *--      End Types      --*

           *--Section of Initializing the variables --*
llAgeTyp = (ALLTRIM(GFGETMEMVAR('XAGINGTYPE')) = 'D')      && the aging type ("D" for date , "T" for Terms )
STORE 0 TO lnOrdNo,lnOrdAmt,lnInvNo,lnInvAmt,lnCrdtNo,lnCrdtAmt,lnChrgNo,;
                  lnChrgAmt,PAGENO,lnInvDys,lnChDys,lnCrdDys
DECLARE laBillTo[5,1]       && Declare a dimension to hold the address.
laBillTo = ''               && Array to hold the Sold To address
STORE GOMONTH(gdSysDate,-12) TO ldDate
SET DEVICE TO PRINTER

           *--Section of relations --*
SELECT CUSTOMER
SET RELATION TO SALESREP INTO SALESREP
=SEEK('M')
           *--Section of main pogram & printing --*
lcRpexp = IIF(EMPTY(lcRpexp) , ".T." , lcRpexp)
SCAN REST WHILE type+account+store = 'M' FOR &lcRpexp
  STORE 0 TO lnChrgNo,lnChrgAmt,lnCrdtNo,lnCrdtAmt,lnInvNo,lnInvAmt
  STORE 0 TO lnOrdNo,lnOrdAmt,lnInvDys,lnChDys,lnCrdDys

  lcAccount = customer.account
  *---"OPEN ORDERS,TOTAL,AVG"
  =lfOrder(lcAccount)
  *---"INVOICES,TOTAL,AVG"
  =lfInvoice(lcAccount)
  *---"CHARGEBACKS"
  =lfTotChrg(lcAccount)
  *---"CREDITS"
  =lfTotCrdt(lcAccount)
  *---"PAST PAYMENTS LAST 12 MONTHS"
  =lfAvgDays(lcAccount)

  SELECT CUSTOMER
  PAGENO=PAGENO+1
  @01,20 SAY "CUSTOMER PAYMENT HISTORY"
  @01,70 SAY "PAGE:"
  @01,75 SAY PAGENO PICTURE "9999"
  @02,38 SAY MDY(gdSysDate)
  @04,00 SAY ACCOUNT
  @04,32 SAY BTNAME
  @05,00 SAY Phone1
  =lfBiAdr()
  @05,32 SAY laBillTo[1]
  @06,00 SAY BUYER
  @06,32 SAY IIF (!EMPTY(laBillTo[2]),laBillTo[2],laBillTo[3])
  @07,00 SAY SALESREP.NAME
  @07,32 SAY IIF (!EMPTY(laBillTo[2]),laBillTo[3],' ' )
  @08,00 SAY gfCodDes(CUSTOMER.CTERMCODE,'CTERMCODE')
  @09,00 SAY REPLICATE('-',80)
  @10,00 SAY "AGING:    CURRENT        31-60       61-90       91-120     OVER 120  TOTAL OPEN"
  @11,00 SAY REPLICATE('-',80)
  IF llAgeTyp
    @12,06 SAY CURRENT PICTURE '99999999.99'
    @12,19 SAY AGE30 PICTURE '99999999.99'
    @12,31 SAY AGE60 PICTURE '99999999.99'
    @12,44 SAY AGE90 PICTURE '99999999.99'
    @12,57 SAY AGE120 PICTURE '99999999.99'
    @12,69 SAY TOTAGE PICTURE '99999999.99'
  ELSE
    @12,06 SAY TERCURRENT PICTURE '99999999.99'
    @12,19 SAY TERAGE30 PICTURE '99999999.99'
    @12,31 SAY TERAGE60 PICTURE '99999999.99'
    @12,44 SAY TERAGE90 PICTURE '99999999.99'
    @12,57 SAY TERAGE120 PICTURE '99999999.99'
    @12,69 SAY TOTAGE PICTURE '99999999.99'
  ENDIF
  @14,00 SAY REPLICATE('-',80)
  @15,00 SAY "OPEN ORDERS:"
  @15,12 SAY lnOrdNo  PICTURE '99999'
  @15,19 SAY "AVG. $ PER ORDER:$"
  @15,37 SAY IIF (lnOrdNo<>0,lnOrdAmt/lnOrdNo,'N/A')  PICTURE '99999999.99'
  @15,49 SAY "TOTAL ORDER AMOUNT:$"
  @15,69 SAY lnOrdAmt  PICTURE '99999999.99'
  @16,00 SAY REPLICATE('-',80)
  @17,00 SAY "CREDIT LIMIT:"
  @17,15 SAY "$"
  @17,17 SAY CRLIMIT  PICTURE '9999999'
  @17,54 SAY "GRAND TOTAL:"
  @17,67 SAY "$"
  @17,69 SAY TOTAGE+lnOrdAmt PICTURE '99999999.99'
  @18,00 SAY REPLICATE('-',80)
  @19,00 SAY "PAST PAYMENTS              INVOICES             CHARGEBACKS            CREDITS"
  @20,00 SAY "LAST 12 MONTHS          -----------             -----------        -----------"
  @21,00 SAY "--------   TOTAL $:"
  @21,24 SAY lnInvAmt   PICTURE '99999999.99'
  @21,48 SAY lnChrgAmt  PICTURE '99999999.99'
  @21,67 SAY lnCrdtAmt  PICTURE '99999999.99'
  @22,00 SAY "TOTAL TRANSACTIONS:"
  @22,24 SAY lnInvNo
  @22,49 SAY lnChrgNo
  @22,68 SAY lnCrdtNo
  @23,00 SAY "        AVERAGE  $:"
  @23,24 SAY IIF(lnInvNo<>0,lnInvAmt/lnInvNo,'N/A')    PICTURE '99999999.99'
  @23,48 SAY IIF(lnChrgNo<>0,lnChrgAmt/lnChrgNo,'N/A') PICTURE '99999999.99'
  @23,67 SAY IIF(lnCrdtNo<>0,lnCrdtAmt/lnCrdtNo,'N/A') PICTURE '99999999.99'
  @24,00 SAY "                        ===========             ===========        ==========="
  @26,00 SAY "AVERAGE DAYS PAID :"
  @26,31 SAY IIF(lnInvNo  <> 0,ROUND(lnInvDys/lnInvNo,0),'N/A')  PICTURE '9999'
  @26,55 SAY IIF(lnChrgNo <> 0,ROUND(lnChDys/lnChrgNo,0),'N/A')  PICTURE '9999'
  @26,74 SAY IIF(lnCrdtNo <> 0,ROUND(lnCrdDys/lnCrdtNo,0),'N/A') PICTURE '9999'
ENDSCAN
SET DEVICE TO SCREEN
DO ENDREPORT
*!**************************************************************************
*! Name      : lfOrder.
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/23/2000
*! Purpose   : Calculate the customer's orders no.,amount and average
*:           : through the last 12 months
*!**************************************************************************
*! Called from : ARCSPHIS.PRG
*!**************************************************************************
*! Calls       : 
*!**************************************************************************
*! Example     : =lfOrder()
*! Parameters  : lcAccount
*!**************************************************************************
FUNCTION lfOrder
PARAMETERS lcAccount

lnAlias = ALIAS()
SELECT ORDHDR
=SEEK(lcAccount+'O')                            && ACCOUNT + STATUS  = 'O' OPEN
SCAN REST WHILE ACCOUNT+STATUS+BULK+cordtype+ORDER = lcAccount+'O' FOR cOrdType = 'O'
  lnOrdNo   = lnOrdNo + 1
  lnOrdAmt  = lnOrdAmt + OpenAmt
ENDSCAN
=SEEK(lcAccount+'H')                             && ACCOUNT + STATUS  = 'H' HOLD
SCAN REST WHILE ACCOUNT+STATUS+BULK+cordtype+ORDER = lcAccount+'H' FOR cOrdType = 'O'
  lnOrdNo   = lnOrdNo + 1
  lnOrdAmt  = lnOrdAmt + OpenAmt
ENDSCAN

SELECT (lnAlias)
RETURN 

*!**************************************************************************
*! Name      : lfInvoice.
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/23/2000
*! Purpose   : Calculate the customer's invoics' no.,amount and average
*!           : through the last 12 months
*!**************************************************************************
*! Called from : ARCSPHIS.PRG
*!**************************************************************************
*! Calls       : 
*!**************************************************************************
*! Example     : =lfInvoice()
*! Parameters  : lcAccount
*!**************************************************************************
FUNCTION lfInvoice
PARAMETERS lcAccount

lnAlias = ALIAS()
SELECT INVHDR
=SEEK(lcAccount)
SCAN REST WHILE ACCOUNT+INVOICE = lcAccount ;
     FOR  STATUS <> 'V' .AND. BETWEEN(INVDATE,ldDate,gdSysDate) 
  lnInvNo  = lnInvNo + 1
  lnInvAmt = lnInvAmt + TotalChg
ENDSCAN
SELECT (lnAlias)
RETURN 

*!**************************************************************************
*! Name      : lfTotChrg.
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/23/2000
*! Purpose   : Calculate the customer's chargebacks' no.,amount and average
*!           : through the last 12 months.
*!**************************************************************************
*! Called from : ARCSPHIS.PRG
*!**************************************************************************
*! Calls       : 
*!**************************************************************************
*! Example     : =lfTotChrg()
*! Parameters  : lcAccount
*!**************************************************************************
FUNCTION lfTotChrg
PARAMETERS lcAccount

lnAlias = ALIAS()
SELECT DEBIT
=SEEK(lcAccount)
SCAN REST WHILE ACCOUNT+TRAN+cInstalNo+DTOS(TRANDATE) = lcAccount ;
     FOR  TRANTYPE = '3' .AND. BETWEEN(TRANDATE,ldDate,gdSysDate)  &&TRANTYPE = 3  Charge Back (Debit On Account).
  lnChrgNo  = lnChrgNo + 1
  lnChrgAmt = lnChrgAmt + Amount
ENDSCAN
SELECT (lnAlias)
RETURN 
*!**************************************************************************
*! Name      : lfTotCrdt.
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/23/2000
*! Purpose   : Calculate the customer's credits' no.,amount and average
*!           : through the last 12 months.
*!**************************************************************************
*! Called from : ARCSPHIS.PRG
*!**************************************************************************
*! Calls       : 
*!**************************************************************************
*! Example     : =lfTotCrdt()
*! Parameters  : lcAccount
*!**************************************************************************
FUNCTION lfTotCrdt
PARAMETERS lcAccount

lnAlias = ALIAS()
SELECT CREDIT
=SEEK(lcAccount)
SCAN REST WHILE ACCOUNT+TRAN+DTOS(TRANDATE) = lcAccount ;
     FOR   BETWEEN(TRANDATE,ldDate,gdSysDate) 
  lnCrdtNo   = lnCrdtNo + 1
  lnCrdtAmt = lnCrdtAmt + ABS(Amount)          && ABS() to get postive amount.
ENDSCAN
SELECT (lnAlias)
RETURN 

*!**************************************************************************
*! Name      : lfAvgDays.
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/23/2000
*! Purpose   : Calculate the average days that took the custmer to pay
*!           : an invoice,keyoff each chargeback and credit.
*!**************************************************************************
*! Called from : ARCSPHIS.PRG
*!**************************************************************************
*! Calls       : 
*!**************************************************************************
*! Example     : =lfAvgDays()
*! Parameters  : lcAccount
*!**************************************************************************
FUNCTION lfAvgDays
PARAMETERS lcAccount

lnAlias = ALIAS()
SELECT ArHist
=SEEK(lcAccount)
SCAN REST WHILE ACCOUNT+HISTORY = lcAccount ;
          FOR   BETWEEN(TRANDATE,ldDate,gdSysDate) 
  DO CASE
    CASE TranType = '1'                             &&Invoies
      lnInvDys = lnInvDys + (HistDate-TranDate)
    CASE TranType = '3'                             &&Chargebacks
      lnChrgNo  = lnChrgNo +1
      lnChrgAmt = lnChrgAmt + Amount
      lnChDys   = lnChDys + (HistDate-TranDate)
    CASE INLIST (TranType,'0','4','5','6')          &&Credits
      lnCrdtNo  = lnCrdtNo +1
      lnCrdtAmt = lnCrdtAmt + ABS(Amount)
      lnCrdDys  = lnCrdDys + (HistDate-TranDate)
  ENDCASE     
ENDSCAN
SELECT (lnAlias)
RETURN

*!**************************************************************************
*! Name      : lfBiAdr
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/23/2000
*! Purpose   : Function to Get the Bill to Address.
*!**************************************************************************
*! Called from : ARCSPHIS.PRG
*!**************************************************************************
*! Calls       : 
*!**************************************************************************
*! Example     : =lfBiAdr()
*!**************************************************************************
FUNCTION lfBiAdr
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT CUSTOMER

laBillTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laBillTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laBillTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laBillTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laBillTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

SELECT (lcAlias)
                     *-- END ARCSPHIS.PRG --*
