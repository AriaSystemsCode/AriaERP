*:***************************************************************************
*: Program file  : ARCSTHST.PRG
*: Program desc. : CUSTOMIZED AGING CUSTOMER HISTORY REPORT FOR F&G.
*! Date          : 11/05/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*: Tracking Job Number: C200133
*:
*:***************************************************************************
*: Calls :
*:    Functions  : lfCreatTmp , lfRelation , lfColctDat , lfAccount , lfsrRep
*:               : lfvDateRng , lfvpbok    , fwDays     , lfvDay    , lfUpdtAge
*:               : lFInsRecrd
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARCSTHST
*:***************************************************************************
*: Modification :
*: B604244,1 BWA 22/02/2001 1) Modifications in the report layout in the collecting
*:                             the balance column.[PRG & FRX]
*:                          2) The filter of the sorting of the report doesn't work.
*:****************************************************************************
*: The DEBIT and CREDIT Type.
*:      Type  Meaning
*:      0     Returns (Credit Memo).
*:      1     Invoice, Direct Invoice.
*:      2     Debit Adjustment.                              DEBIT
*:      3     Charge Back (Debit On Account).                DEBIT
*:      4     Payment.                                       CREDIT
*:      5     Credit Adjustment.                             CREDIT
*:      6     Credit On Account.                             CREDIT
*:      7     Allowance (Credit Adjustment in this program). 
*:      8     Charge Back (Only in the ARHIST).
*:      9     Credit On Account (Only in the ARHIST).
*************************************************************************
*--Get the full picture of the date.
lcCentury  = SET('CENTURY')
SET CENTURY ON

*-- Get the Variables its Initial value
STORE SPACE(0) TO lcFilter

*--The header of the aging.
lcAgeHd30  = 'Over ' + ALLTRIM(STR(lnRPDay1))
lcAgeHd60  = 'Over ' + ALLTRIM(STR(lnRPDay2))
lcAgeHd90  = 'Over ' + ALLTRIM(STR(lnRPDay3))
lcAgeHd120 = 'Over ' + ALLTRIM(STR(lnRPDay4))
lcAgeHd150 = 'Over ' + ALLTRIM(STR(lnRpDay5))
*--BAS
=lfCreatNdx()
*--BAS
=lfCreatTmp()
=lfRelation()
=lfColctDat()

*--Case the user select sad logo.
IF !llRpLogo
  SELECT (lcPicBmp)
  *--Bas Comment this line as per khaled.
  *APPEND GENERAL gPIC FROM ( gcBmpHome+'SadLogo.BMP')
  llChangLog = .F.
ELSE
  IF !llChangLog
    SELECT (lcPicBmp)
    APPEND GENERAL gPIC FROM ( gcBmpHome+'HapyLogo.BMP')
  ENDIF
ENDIF

SELECT (lclines)
SET RELATION TO &lclines..SALESREP + &lclines..ACCOUNT INTO (lcBalnc) ADDITIVE

*B604244,1 BWA 22/02/2001 The filter of the sorting of the report doesn't work.[START]
*SET ORDER TO TAG SALESREP
IF lcRpSort = "T"
  SET ORDER TO TAG SALESREP
  *--bas
  =lfSortBal()
  *--bas
ELSE
  IF lcRpKyOff = "B"
    INDEX ON SALESREP+ACCOUNT+DTOS(DATE) TAG DATE OF (gcWorkDir+lcLines)
  ELSE
    INDEX ON SALESREP+ACCOUNT+Type+DTOs(DATE) TAG DATE OF (gcWorkDir+lcLines)
  ENDIF
  =lfSortBal()
ENDIF
*B604244,1 BWA 22/02/2001 [END]

SET RELATION TO &lclines..SALESREP INTO SALESREP ADDITIVE
SET RELATION TO &lclines..SALESREP + &lclines..ACCOUNT INTO (lcTmpAges) ADDITIVE
LOCATE

DO gfDispRe WITH EVALUATE('lcRpForm')

*-- use this function to erase the tmp. files and its index in case the user
=lfClarAll()

WAIT CLEAR
SET CENTURY &lcCentury

*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 11/05/2000
*! Purpose   : Function to creat the temp. file hold the data.
*!*************************************************************
*! Called from : ARCSTHST.PRG
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

CREATE Table (lclines) (Salesrep C(3)   , Account C(5)  , Btname C(30) , TransNo C(6)   ,;
                        TransType C(25) , Date D(8)     , Qty N(15,2)  , InvRt N(15,2)  ,;
                        Cash N(15,2)    , Adjst N(15,2) , Type C(1)    , Balance N(15,2))
ZAP
INDEX ON SALESREP+Account+Type TAG SALESREP OF (gcWorkDir+lcLines)
INDEX ON Account               TAG Account  OF (gcWorkDir+lcLines)

CREATE Table (lcBalnc) (Salesrep C(3) , Account C(5) , Balance N(15,2))
ZAP
INDEX ON SALESREP + Account TAG Account  OF (gcWorkDir+lcBalnc)

CREATE Table (lcTmpAges) (Salesrep C(3) , Account C(5)  , Current N(12,2) , Age30 N(12,2) ,;
                          Age60 N(12,2) , Age90 N(12,2) , Age120 N(12,2)  , Age150 N(12,2))
ZAP
INDEX ON SALESREP + Account TAG Account  OF (gcWorkDir+lcTmpAges)

*-- End of lfCreatTmp.
*!*************************************************************
*! Name      : lfRelation
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 11/05/2000
*! Purpose   : Function to prepare the relations of the program.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRelation

SELECT CUSTOMER
SET RELATION TO Customer.Account INTO Credit ADDITIVE
SET RELATION TO Customer.Account INTO Debit  ADDITIVE
SET RELATION TO Customer.Account INTO Invhdr ADDITIVE
SET RELATION TO Customer.Account INTO Rethdr ADDITIVE
SET RELATION TO Customer.Account INTO Arhist ADDITIVE

*-- End of lfRelation.
*!*************************************************************
*! Name      : lfcolctDat
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 11/05/2000
*! Purpose   : Function to Collect the date from the files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfcolctDat
PRIVATE lcAlias,lcAccount

lcAlias  = ALIAS()
lcAccount= SPACE(0)

*B604244,1 BWA 22/02/2001 Variable hold the old balance.[START]
lcBalance = 0
*B604244,1 BWA 22/02/2001 [END]

*--Salesrep
lnDataPos = ASCAN(laOGFxFlt,'SALESREP.REPCODE')
IF lnDataPos > 0
  LCRPEXP = STRTRAN(LCRPEXP ,'SALESREP.REPCODE','CUSTOMER.SALESREP' )
ENDIF

SELECT CUSTOMER
*--The filter
lcFilter = IIF(!EMPTY(LCRPEXP),LCRPEXP,'.T.')

SCAN WHILE Type+Account+Store ='M' FOR &lcFilter
  lcAccount=ACCOUNT
  WAIT WINDOW 'Collect data for Customer ' + Account NOWAIT
  *--Get the data for the report to the selected customer.
  =lfFillTmp()
ENDSCAN


SELECT (lcAlias)
*-- End of lfcolctDat.
*!*************************************************************
*! Name      : lfFillTmp
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 11/05/2000
*! Purpose   : Function to Collect the first period balance.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*The Balance = Debit transaction - credit transaction
*The Amount balance from the first day till the day before the user select
*in the from transaction date.

FUNCTION lfFillTmp
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT (lcBalnc)
APPEND BLANK
REPLACE Salesrep WITH CUSTOMER.Salesrep ;
        Account  WITH lcAccount         ;
        Balance  WITH 0

SELECT (lcTmpAges)
APPEND BLANK

*--The invoice +ve value
SELECT INVHDR
*--Bas Comment this line as per khaled.
IF .F.
IF SEEK(lcAccount) AND lcRpKyOff $ 'NB'   && get the data in case key off Both,No
  
  *-- Flag to indecate to insert record in case there is no transaction,
  *-- match the from date and there is previous transaction.
  llrecflg = .T.

  SCAN REST WHILE ACCOUNT+INVOICE = lcAccount                            ;
            FOR IIF(ldFrom # {} , Invdate >= ldFrom , Invdate = Invdate) ;
            .AND. INVHDR.STATUS <> 'V' AND TOTALCHG <> 0

    *--Case for the debit aging.
    =lfUpdtAge('D',ldRpEndDat - INVDATE , '1')

    *-- Rise the flag with false to not insert any record in the other case.
    llrecflg = .F.

    *-- Insert the transaction record for the invoice from invhdr file.
    SELECT (lclines)

    *B604244,1 BWA 22/02/2001 Variable hold the old balance.[START]
    lcBalance = &lclines..Balance
    *B604244,1 BWA 22/02/2001 [END]

    APPEND BLANK

    *B604244,1 BWA 22/02/2001 Change the collection of the balance field.[START]
    *REPLACE Salesrep  WITH CUSTOMER.Salesrep ;
            Account   WITH lcAccount         ;
            Btname    WITH CUSTOMER.Btname   ;
            TransNo   WITH INVHDR.Invoice    ;
            TransType WITH "Invoice"         ;
            Date      WITH INVHDR.Invdate    ;
            Qty       WITH INVHDR.SHIP       ;
            InvRt     WITH INVHDR.TOTALCHG   ;
            Balance   WITH INVHDR.TOTALCHG

    REPLACE Salesrep  WITH CUSTOMER.Salesrep ;
            Account   WITH lcAccount         ;
            Btname    WITH CUSTOMER.Btname   ;
            TransNo   WITH INVHDR.Invoice    ;
            TransType WITH "Invoice"         ;
            Date      WITH INVHDR.Invdate    ;
            Qty       WITH INVHDR.SHIP       ;
            InvRt     WITH INVHDR.TOTALCHG   ;
            Balance   WITH lcBalance + INVHDR.TOTALCHG
    *B604244,1 BWA 22/02/2001 [END]
  ENDSCAN

  =SEEK(lcAccount)
  *-- Case there is no records match the transaction date,
  *-- and in the same time there is previous transaction before the from date.
  SCAN REST WHILE ACCOUNT+INVOICE = lcAccount             ;
            FOR IIF(ldFrom # {} , Invdate < ldFrom , .F.) ;
            .AND. INVHDR.STATUS <> 'V' AND TOTALCHG <> 0

    IF llrecflg
      SELECT (lclines)
      REPLACE Salesrep  WITH CUSTOMER.Salesrep ;
              Account   WITH lcAccount         ;
              Btname    WITH CUSTOMER.Btname

      llrecflg = .F.
    ENDIF
    *--Fill the Balance field with the total charge from the invhdr.[in the lcBalnc]
    SELECT (lcBalnc)
    REPLACE &lcBalnc..Balance WITH &lcBalnc..Balance + INVHDR.TOTALCHG

  ENDSCAN

*--Gets its initial value.
llrecflg = .T.
ENDIF
ENDIF

*--The credit memo -ve value
SELECT RETHDR
*--Bas Comment this line as per khaled.
IF .F.
IF SEEK(lcAccount) AND lcRpKyOff $ 'NB'   && get the data in case key off Both,No
  SCAN REST WHILE ACCOUNT+CRMEMO = lcAccount                         ;
            FOR IIF(ldFrom # {} , CRDATE >= ldFrom , CRDATE = CRDATE) ;
            .AND. RETHDR.STATUS <> 'V' AND TOTCREDIT <> 0

    *--Case for the credit aging.
    =lfUpdtAge('C',ldRpEndDat - CRDATE , '2')

    *-- Insert the transaction record for the credit memo from rethdr file.
    SELECT (lclines)

    *B604244,1 BWA 22/02/2001 Variable hold the old balance.[START]
    lcBalance = &lclines..Balance
    *B604244,1 BWA 22/02/2001 [END]

    APPEND BLANK

    *B604244,1 BWA 22/02/2001 Change the collection of the balance field.[START]
    *REPLACE Salesrep  WITH CUSTOMER.Salesrep ;
            Account   WITH lcAccount         ;
            Btname    WITH CUSTOMER.Btname   ;
            TransNo   WITH RETHDR.Crmemo     ;
            TransType WITH "Credit Memo"     ;
            Date      WITH RETHDR.CRDATE     ;
            Qty       WITH RETHDR.Pieces     ;
            InvRt     WITH RETHDR.TOTCREDIT  ;
            Balance   WITH RETHDR.TOTCREDIT

    REPLACE Salesrep  WITH CUSTOMER.Salesrep   ;
            Account   WITH lcAccount           ;
            Btname    WITH CUSTOMER.Btname     ;
            TransNo   WITH RETHDR.Crmemo       ;
            TransType WITH "Credit Memo"       ;
            Date      WITH RETHDR.CRDATE       ;
            Qty       WITH -(RETHDR.Pieces)    ;
            InvRt     WITH -(RETHDR.TOTCREDIT) ;
            Balance   WITH lcBalance - (RETHDR.TOTCREDIT)
    *B604244,1 BWA 22/02/2001 [END]

  ENDSCAN

  =SEEK(lcAccount)
  *-- Case there is no records match the transaction date,
  *-- and in the same time there is previous transaction before the from date.
  SCAN REST WHILE ACCOUNT+CRMEMO = lcAccount              ;
            FOR IIF(ldFrom # {} , CRDATE < ldFrom , .F.) ;
            .AND. RETHDR.STATUS <> 'V' AND TOTCREDIT <> 0

      *--Subtract the Balance field with the total charge from the Rethdr.[in the lcBalnc]
      SELECT (lcBalnc)
      REPLACE &lcBalnc..Balance WITH &lcBalnc..Balance - RETHDR.TOTCREDIT
  ENDSCAN
ENDIF
ENDIF

*--The credit transaction.[45]
SELECT CREDIT
IF SEEK(lcAccount) AND lcRpKyOff $ 'NB'   && get the data in case key off Both,No
*--bas
  *SCAN REST WHILE ACCOUNT+TRAN+DTOS(TranDate) = lcAccount                   ;
            FOR IIF(ldFrom # {} , TRANDATE >= ldFrom , TRANDATE = TRANDATE ) ;
           .AND. CREDIT.TranType $ '456' AND Amount <> 0
  SCAN REST WHILE ACCOUNT+TRAN+DTOS(TranDate) = lcAccount                   ;
            FOR IIF(ldFrom # {} , TRANDATE >= ldFrom , TRANDATE = TRANDATE ) ;
            .AND. CREDIT.TranType $ '0456' AND Amount <> 0
*--bas

    *--Case for the credit aging.
    =lfUpdtAge('C',gdSysDate - TRANDATE , '3')

    *-- Insert the transaction record for the credit types from credit file.
    SELECT (lclines)

    *B604244,1 BWA 22/02/2001 Variable hold the old balance.[START]
    lcBalance = &lclines..Balance
    *B604244,1 BWA 22/02/2001 [END]

    APPEND BLANK

    *B604244,1 BWA 22/02/2001 Change the collection of the balance field.[START]
    *REPLACE Salesrep  WITH CUSTOMER.Salesrep ;
            Account   WITH lcAccount         ;
            Btname    WITH CUSTOMER.Btname   ;
            TransNo   WITH IIF(CREDIT.TranType $ '45' , CREDIT.Batch , CREDIT.HISTORY) ;
            TransType WITH IIF(CREDIT.TranType = '4' , "Payment" , IIF(CREDIT.TranType = '6',"Credit On Account","Credit Adjustment"));
            Date      WITH CREDIT.TRANDATE   ;
            Cash      WITH IIF(CREDIT.TranType = '4' , CREDIT.AMOUNT , 0);
            Adjst     WITH IIF(CREDIT.TranType $ '56' , CREDIT.AMOUNT , 0);
            Balance   WITH CREDIT.AMOUNT
*--bas
    *REPLACE Salesrep  WITH CUSTOMER.Salesrep ;
            Account   WITH lcAccount         ;
            Btname    WITH CUSTOMER.Btname   ;
            TransNo   WITH IIF(CREDIT.TranType $ '45' , CREDIT.Batch , CREDIT.HISTORY) ;
            TransType WITH IIF(CREDIT.TranType = '4' , "Payment" , IIF(CREDIT.TranType = '6',"Credit On Account","Credit Adjustment"));
            Date      WITH CREDIT.TRANDATE   ;
            Cash      WITH IIF(CREDIT.TranType = '4' , CREDIT.AMOUNT , 0);
            Adjst     WITH IIF(CREDIT.TranType $ '56' , CREDIT.AMOUNT , 0);
            Balance   WITH lcBalance + CREDIT.AMOUNT

    REPLACE Salesrep  WITH CUSTOMER.Salesrep ;
            Account   WITH lcAccount         ;
            Btname    WITH CUSTOMER.Btname   ;
            TransNo   WITH IIF(CREDIT.TranType $ '45' , CREDIT.Batch , CREDIT.HISTORY) ;
            TransType WITH IIF(CREDIT.TranType = '4' , "Payment" , IIF(CREDIT.TranType = '6',"Credit On Account",IIF(CREDIT.TranType = '0',"Credit Memo","Credit Adjustment")));
            Date      WITH CREDIT.TRANDATE   ;
            Cash      WITH IIF(CREDIT.TranType $ '04' , CREDIT.AMOUNT , 0);
            Adjst     WITH IIF(CREDIT.TranType $ '56' , CREDIT.AMOUNT , 0);
            Balance   WITH lcBalance + CREDIT.AMOUNT
*--bas
    *B604244,1 BWA 22/02/2001 [END]

  ENDSCAN

  =SEEK(lcAccount)
  *-- Case there is no records match the transaction date,
  *-- and in the same time there is previous transaction before the from date.
*--bas
  *SCAN REST WHILE ACCOUNT+TRAN+DTOS(TranDate) = lcAccount    ;
            FOR IIF(ldFrom # {} , TRANDATE < ldFrom , .F. ) ;
           .AND. CREDIT.TranType $ '456' AND Amount <> 0
  SCAN REST WHILE ACCOUNT+TRAN+DTOS(TranDate) = lcAccount    ;
            FOR IIF(ldFrom # {} , TRANDATE < ldFrom , .F. ) ;
            AND Amount <> 0
*--bas
      SELECT (lcBalnc)
      REPLACE &lcBalnc..Balance WITH &lcBalnc..Balance + CREDIT.AMOUNT

  ENDSCAN
ENDIF

*--From Arhist file which hold the Key off transaction.[2457]
SELECT ARHIST

IF SEEK(lcAccount) AND lcRpKyOff $ 'YB'   && get the data in case key off Both,Yes
*--BAS
  *SCAN REST WHILE ACCOUNT+HISTORY+TranType+Tran+Cinstalno = lcAccount       ;
            FOR IIF(ldFrom # {} , TRANDATE >= ldFrom , TRANDATE = TRANDATE ) ;
           .AND. ARHIST.TranType $ '0123456789' AND Amount <> 0
  SCAN REST WHILE ACCOUNT+HISTORY+TRANTYPE+TRANCODE = lcAccount       ;
            FOR IIF(ldFrom # {} , TRANDATE >= ldFrom , TRANDATE = TRANDATE ) ;
            AND ARHIST.TranType $ '0123456' AND Amount <> 0
*--BAS

    *--Case in the arhist file.
*--bas
    *IF ARHIST.TranType $ '23'
    IF ARHIST.TranType $ '123'
*--bas
      *--Case for the debit aging.
*      =lfUpdtAge('D',ldRpEndDat  - TRANDATE , '3')
    ELSE
      *--Case for the credit aging.
*--bas
      *IF ARHIST.TranType $ '4579'
      IF ARHIST.TranType $ '0456'
*--bas
*        =lfUpdtAge('C',gdSysDate - TRANDATE , '3')
      ENDIF
    ENDIF

    *-- Insert the transaction record for the credit memo from rethdr file.
    SELECT (lclines)

    *B604244,1 BWA 22/02/2001 Variable hold the old balance.[START]
    lcBalance = &lclines..Balance
    *B604244,1 BWA 22/02/2001 [END]

    APPEND BLANK

    *B604244,1 BWA 22/02/2001 Change the collection of the balance field.[START]
    *REPLACE Salesrep  WITH CUSTOMER.Salesrep ;
            Account   WITH lcAccount         ;
            Btname    WITH CUSTOMER.Btname   ;
            TransNo   WITH ARHIST.Batch      ;
            Type      WITH 'K'               ;
            TransType WITH IIF(ARHIST.TranType = '2'  , "Debit Adjustment"  ,;
                           IIF(ARHIST.TranType = '4'  , "Payment"           ,;
                           IIF(ARHIST.TranType $ '57' , "Credit Adjustment" ,;
                           IIF(ARHIST.TranType = '3'  , "Debit On Account" , "Credit On Account"))));
            Date      WITH ARHIST.TRANDATE   ;
            Cash      WITH IIF( ARHIST.TranType = '4' , ARHIST.AMOUNT , 0);
            Adjst     WITH IIF( ARHIST.TranType $ '23579' , ARHIST.AMOUNT , 0);
            Balance   WITH IIF( ARHIST.TranType $ '234579' , ARHIST.AMOUNT , 0)
*--bas
    *REPLACE Salesrep  WITH CUSTOMER.Salesrep ;
            Account   WITH lcAccount         ;
            Btname    WITH CUSTOMER.Btname   ;
            TransNo   WITH ARHIST.Tran       ;
            Type      WITH 'K'               ;
            TransType WITH IIF(ARHIST.TranType = '2'  , "Debit Adjustment"  ,;
                           IIF(ARHIST.TranType = '4'  , "Payment"           ,;
                           IIF(ARHIST.TranType $ '57' , "Credit Adjustment" ,;
                           IIF(ARHIST.TranType $ '69'  , "Credit On Account" , "Debit On Account"))));
            TransType WITH  IIF(ARHIST.TranType = '0'  , "Return "  ,;
                            IIF(ARHIST.TranType = '1'  , "Invoice" ,;
                            IIF(ARHIST.TranType = '8'  , "Charge Back" , TransType)));
            Date      WITH ARHIST.TRANDATE   ;
            Cash      WITH IIF( ARHIST.TranType = '4' , ARHIST.AMOUNT , 0);
            Adjst     WITH IIF( ARHIST.TranType $ '23579' , ARHIST.AMOUNT , 0);
            Balance   WITH lcBalance + IIF( ARHIST.TranType $ '234579' , ARHIST.AMOUNT , 0)

    REPLACE Salesrep  WITH CUSTOMER.Salesrep ;
            Account   WITH lcAccount         ;
            Btname    WITH CUSTOMER.Btname   ;
            TransNo   WITH ARHIST.Tran       ;
            Type      WITH 'K'               ;
            TransType WITH lfTitlNam()       ;
            Date      WITH ARHIST.TRANDATE   ;
            Cash      WITH IIF( ARHIST.TranType = '4' , ARHIST.AMOUNT , 0);
            Adjst     WITH IIF( ARHIST.TranType $ '2356789' , ARHIST.AMOUNT , 0);
            InvRt     WITH IIF( ARHIST.TranType = '1' , ARHIST.AMOUNT , IIF( ARHIST.TranType = '0',-ARHIST.AMOUNT,0));
            Balance   WITH lcBalance + ARHIST.AMOUNT
*--bas
    *B604244,1 BWA 22/02/2001 [END]

  ENDSCAN

  =SEEK(lcAccount)
  *-- Case there is no records match the transaction date,
  *-- and in the same time there is previous transaction before the from date.

*--BAS
  *SCAN REST WHILE ACCOUNT+HISTORY+TranType+Tran+Cinstalno = lcAccount ;
            FOR IIF(ldFrom # {} , TRANDATE < ldFrom , .F. )          ;
           .AND. ARHIST.TranType $ '0123456789' AND Amount <> 0

  SCAN REST WHILE ACCOUNT+HISTORY+TRANTYPE+TRANCODE = lcAccount ;
            FOR IIF(ldFrom # {} , TRANDATE < ldFrom , .F. )          ;
            AND Amount <> 0
*--BAS

      *--Fill the Balance field with the total charge from the ARHIST.[in the lcBalnc]
      SELECT (lcBalnc)
      REPLACE &lcBalnc..Balance WITH &lcBalnc..Balance + ARHIST.AMOUNT

  ENDSCAN
ENDIF

*--The Debit transaction.[23]
SELECT DEBIT
IF SEEK(lcAccount) AND lcRpKyOff $ 'NB'   && get the data in case key off Both,No
*--bas
  *SCAN REST WHILE ACCOUNT+TRAN+Cinstalno+DTOS(TranDate) = lcAccount        ;
            FOR IIF(ldFrom # {} , TRANDATE >= ldFrom , TRANDATE = TRANDATE) ;
           .AND. DEBIT.TranType $ '23' AND Amount <> 0

  SCAN REST WHILE ACCOUNT+TRAN+Cinstalno+DTOS(TranDate) = lcAccount        ;
            FOR IIF(ldFrom # {} , TRANDATE >= ldFrom , TRANDATE = TRANDATE) ;
            .AND. DEBIT.TranType $ '123' AND Amount <> 0
*--bas
    *--Case for the debit aging.
    =lfUpdtAge('D',ldRpEndDat - TRANDATE , '3')

    *-- Insert the transaction record for the debit types from debit file.
    SELECT (lclines)

    *B604244,1 BWA 22/02/2001 Variable hold the old balance.[START]
    lcBalance = &lclines..Balance
    *B604244,1 BWA 22/02/2001 [END]

    APPEND BLANK

    *B604244,1 BWA 22/02/2001 Change the collection of the balance field.[START]
    *REPLACE Salesrep  WITH CUSTOMER.Salesrep ;
            Account   WITH lcAccount         ;
            Btname    WITH CUSTOMER.Btname   ;
            TransNo   WITH DEBIT.Batch       ;
            TransType WITH IIF(DEBIT.TranType ='2' ,"Debit Adjustment","Debit On Account");
            Date      WITH DEBIT.TRANDATE    ;
            Adjst     WITH DEBIT.AMOUNT      ;
            Balance   WITH DEBIT.AMOUNT
*--bas
    *REPLACE Salesrep  WITH CUSTOMER.Salesrep ;
            Account   WITH lcAccount         ;
            Btname    WITH CUSTOMER.Btname   ;
            TransNo   WITH DEBIT.Batch       ;
            TransType WITH IIF(DEBIT.TranType ='2' ,"Debit Adjustment","Debit On Account");
            Date      WITH DEBIT.TRANDATE    ;
            Adjst     WITH DEBIT.AMOUNT      ;
            Balance   WITH lcBalance + DEBIT.AMOUNT

    REPLACE Salesrep  WITH CUSTOMER.Salesrep ;
            Account   WITH lcAccount         ;
            Btname    WITH CUSTOMER.Btname   ;
            TransNo   WITH DEBIT.Batch       ;
            TransType WITH IIF(DEBIT.TranType ='2' ,"Debit Adjustment",IIF(DEBIT.TranType ='2',"DIRECT INVOICE","Debit On Account"));
            Date      WITH DEBIT.TRANDATE    ;
            Adjst     WITH DEBIT.AMOUNT      ;
            Balance   WITH lcBalance + DEBIT.AMOUNT
*--bas
    *B604244,1 BWA 22/02/2001 [END]

  ENDSCAN

  =SEEK(lcAccount)
  *-- Case there is no records match the transaction date,
  *-- and in the same time there is previous transaction before the from date.
*--bas
  *SCAN REST WHILE ACCOUNT+TRAN+Cinstalno+DTOS(TranDate) = lcAccount ;
            FOR IIF(ldFrom # {} , TRANDATE >= ldFrom , .F.)         ;
           .AND. DEBIT.TranType $ '23' AND Amount <> 0
  SCAN REST WHILE ACCOUNT+TRAN+Cinstalno+DTOS(TranDate) = lcAccount ;
            FOR IIF(ldFrom # {} , TRANDATE >= ldFrom , .F.)         ;
            AND Amount <> 0
*--bas
      *--Fill the Balance field with the total charge from the DEBIT.[in the lcBalnc]
      SELECT (lcBalnc)
      REPLACE &lcBalnc..Balance WITH &lcBalnc..Balance + DEBIT.AMOUNT

  ENDSCAN
ENDIF

SELECT (lcAlias)
*-- End of lfFillTmp.
*!*************************************************************
*! Name      : lfAccount
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 11/05/2000
*! Purpose   : Account in range browse screen .
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfAccount ()
*!*************************************************************
*! Note      : S symbol is [S,Set] ,R symbol is [R ,Reset]
*!*************************************************************
FUNCTION lfAccount
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
ENDCASE
*-- End of lfAccount.
*!*************************************************************
*! Name      : lfsrRep
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 11/05/2000
*! Purpose   : Sales rep. in range browse screen.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrRep()
*!*************************************************************
*! Note      : S symbol is [S,Set] ,R symbol is [R ,Reset]
*!*************************************************************
FUNCTION lfsrRep
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    GO TOP IN SALESREP
ENDCASE

*-- End of lfsrRep.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 11/05/2000
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng

lcTitle = 'Transaction Date'
DO DateRng.Spx

*-- End of lfvDateRng.
*!*************************************************************
*! Name      : lfvpbok
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 11/05/2000
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
*! Example     : =lfvpbok()
*!*************************************************************
FUNCTION lfvpbok

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
  WAIT CLEAR
ELSE
  CLEAR READ
ENDIF

*-- End of lfvpbok.
*!*************************************************************
*! Name      : lfwDays
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 11/05/2000
*! Purpose   : Get the old value of days buckets in a variable.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfwDays()
*!*************************************************************
FUNCTION lfwDays

lnOldDays = EVALUATE(SYS(18))

*-- End of lfwDays.
*!*************************************************************
*! Name      : lfvDay
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 11/05/2000
*! Purpose   : To validate the number of days in the buckets.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvDay()
*!*************************************************************
FUNCTION lfvDay
PARAMETERS lnBuckt

DO CASE 
  *-- For Buckets No1
  CASE lnBuckt = '1'
    IF lnRPDay1 <= 0
      =gfModalGen('TRM40132B40011','ALERT')
      lnRPDay1 = lnOldDays
    ENDIF
    IF lnRPDay1 >= lnRPDay2
      =gfModalGen('TRM40133B40011','ALERT','2nd|1st')
      lnRPDay1 = lnOldDays
    ENDIF

  *-- For Buckets No2
  CASE lnBuckt = '2'
    IF lnRPDay2 <= lnRPDay1
      =gfModalGen('TRM40133B40011','ALERT','2nd|1st')
      lnRPDay2 = lnOldDays
    ENDIF
    IF lnRPDay2 >= lnRPDay3
      =gfModalGen('TRM40133B40011','ALERT','3rd|2nd')
      lnRPDay2 = lnOldDays
    ENDIF

  *-- For Buckets No3
  CASE lnBuckt = '3'
    IF lnRPDay3 <= lnRPDay2
      =gfModalGen('TRM40133B40011','ALERT','3rd|2nd')
      lnRPDay3 = lnOldDays
    ENDIF
    IF lnRPDay3 >= lnRPDay4
      =gfModalGen('TRM40133B40011','ALERT','3rd|2nd')
      lnRPDay3 = lnOldDays
    ENDIF

  *-- For Buckets No4
  CASE lnBuckt = '4'
    IF lnRPDay4 <= lnRPDay3
      =gfModalGen('TRM40133B40011','ALERT','4th|3rd')
      lnRPDay4 = lnOldDays
    ENDIF
    IF lnRPDay4 >= lnRPDay5
      =gfModalGen('TRM40133B40011','ALERT','4th|3rd')
      lnRPDay4 = lnOldDays
    ENDIF

  *-- For Buckets No5
  CASE lnBuckt = '5'
    IF lnRPDay5 <= lnRPDay4
      =gfModalGen('TRM40133B40011','ALERT','5th|4th')
      lnRPDay5 = lnOldDays
    ENDIF
ENDCASE

*-- End of lfvDay.
*!*************************************************************
*! Name      : lfFirstTim
*! Developer : Bassem Rafaat (BWA)
*! Date      : 11/05/2000
*! Purpose   : Function to initial the variabls in the first time.
*!*************************************************************
*! Called from : SYREPUVR.DBF
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFirstTim()
*!*************************************************************
FUNCTION lfFirstTim

CREATE CURSOR (lcLogBmp) (gPic G)
APPEND BLANK
APPEND GENERAL gPIC FROM ( gcBmpHome+'FGLOGO.BMP')

CREATE CURSOR (lcPicBmp) (gPic G)
APPEND BLANK
APPEND GENERAL gPIC FROM ( gcBmpHome+'HapyLogo.BMP')

llChangLog = .T.

*-- End of lfFirstTim.
*!*************************************************************
*! Name      : lfUpdtAge
*! Developer : Bassem Rafaat (BWA)
*! Date      : 11/05/2000
*! Purpose   : Function to get the aging.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfUpdtAge()
*!*************************************************************
FUNCTION lfUpdtAge
PARAMETERS lcUpdtTyp , lnAgeDays , lnTypValu

*-- Intializing variables that calculates data from transaction files.
STORE 0.00 TO m.Current , m.Age30 , m.Age60 , m.Age90 , m.Age120 ,;
              m.Age150  , m.Age00
              
*-- lcAges : Varaible Hold Field description (Age00, 30 , 60 , 90 , 120 and 150)
*-- if Debit / History
IF lcUpdtTyp = 'D'
  lcAges = 'm.Age'                                                    +;
    IIF(lnAgeDays >= lnRpDay5,'150',IIF(lnAgeDays >= lnRpDay4,'120',;
    IIF(lnAgeDays >= lnRpDay3 ,'90',;
    IIF(lnAgeDays >= lnRpDay2 ,'60' ,IIF(lnAgeDays >= lnRpDay1 ,'30','00')))))
ELSE
  lcAges = 'm.Age'                                                      +;
    IIF(lnAgeDays >= 150,'150',IIF(lnAgeDays >= 120,'120',IIF(lnAgeDays >= 90 ,'90' ,;
    IIF(lnAgeDays >= 60 ,'60' ,IIF(lnAgeDays >= 30 ,'30','00')))))
ENDIF

DO CASE  && Fill Age variable
  CASE lnTypValu = '1'
    &lcAges   = TOTALCHG
  CASE lnTypValu = '2'
    &lcAges   = TOTCREDIT
  CASE lnTypValu = '3'
    &lcAges   = Amount
ENDCASE

m.Current = IIF(m.Age00 = 0 , 0 , m.Age00)  && because there is no Age00 field (it's Current)
=lFInsRecrd(lcUpdtTyp)

*-- End of lfUpdtAge.
*!*************************************************************
*! Name      : lFInsRecrd
*! Developer : Bassem Rafaat (BWA)
*! Date      : 11/05/2000
*! Purpose   : Insert ages value.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : lFInsRecrd
*!*************************************************************
FUNCTION lFInsRecrd
PARAMETERS lcUpdtTyp

PRIVATE lcAlias
lcAlias  = ALIAS()

SELECT (lcTmpAges)
REPLACE Salesrep WITH CUSTOMER.Salesrep                                         ;
        Account  WITH lcAccount                                                 ;
        Current  WITH Current +  m.Current ;
        Age30    WITH Age30   +  M.Age30   ;
        Age60    WITH Age60   +  M.Age60   ;
        Age90    WITH Age90   +  M.Age90   ;
        Age120   WITH Age120  +  M.Age120  ;
        Age150   WITH Age150  +  M.Age150

SELECT (lcAlias)
*-- End of lFInsRecrd.
*!*************************************************************
*! Name      : lfClarAll
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 11/05/2000
*! Purpose   : Function clear all the temp. files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : =lfClarAll()
*!*************************************************************
FUNCTION lfClarAll

IF USED (lcLines)
  SELECT (lcLines)
  USE
  ERASE (gcWorkDir + lcLines + '.DBF')
  CLOSE INDEX
  ERASE (gcWorkDir + lcLines + '.CDX')
ENDIF

IF USED (lcBalnc)
  SELECT (lcBalnc)
  USE
  ERASE (gcWorkDir + lcBalnc + '.DBF')
  CLOSE INDEX
  ERASE (gcWorkDir + lcBalnc + '.CDX')
ENDIF

IF USED (lcTmpAges)
  SELECT (lcTmpAges)
  USE
  ERASE (gcWorkDir + lcTmpAges + '.DBF')
  CLOSE INDEX
  ERASE (gcWorkDir + lcTmpAges + '.CDX')
ENDIF

*-- End of lfClarAll.
*!*************************************************************
*! Name      : lfSortBal
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/04/2001
*! Purpose   : Function to sort the balance in case the date.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : =lfSortBal()
*!*************************************************************
FUNCTION lfSortBal
PRIVATE lcAlias , lcSalesRp , lcSorBal , lcAccount

lcAlias = SELECT(0)
SELECT (lclines)
STORE SPACE(0) TO lcSalesRp , lcAccount
STORE 0 TO lcSorBal
SCAN
  IF lcSalesRp # &lclines..Salesrep OR lcAccount # &lclines..Account
    lcSorBal = &lcBalnc..Balance
  ENDIF
  REPLACE Balance WITH lcSorBal + InvRt + Cash + Adjst
  lcSorBal  = &lclines..Balance
  lcSalesRp = &lclines..Salesrep
  lcAccount = &lclines..Account
ENDSCAN

SELECT(lcAlias)

*-- End of lfSortBal.
*!*************************************************************
*! Name      : lfCreatNdx
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/08/2001
*! Purpose   : Create a custom index on the ARHIST file
*!*************************************************************
*! Called from : ARCSTHST.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCreatNdx()
*!*************************************************************
FUNCTION lfCreatNdx

SELECT ARHIST
*-- If index file is already created 
IF !FILE(gcWorkDir +lcArHist+ '.CDX') 
  INDEX ON ACCOUNT+HISTORY+TRANTYPE+TRANCODE TAG CAHTTTag OF (gcWorkDir +  lcArHist + '.CDX')
ELSE
  SET ORDER TO TAG CAHTTTag OF (gcWorkDir +  lcArHist + '.CDX')
ENDIF

*-- End of lfCreatNdx.
*!*************************************************************
*! Name      : lfClearRep
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/08/2001
*! Purpose   : Close files when closing the option grid.
*!*************************************************************
*! Called from : OPTION GRID.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

IF FILE(gcWorkDir +lcArHist+ '.CDX') 
  SELECT ARHIST
  CLOSE INDEX
  ERASE (gcWorkDir +lcArHist+ '.CDX')
ENDIF

*--End of lfClearRep.
FUNCTION lfTitlNam

STORE SPACE(0) TO lcTitle
DO CASE 
  CASE ARHIST.TranType = '0'
    lcTitle = "Return "
  CASE ARHIST.TranType = '1'
    lcTitle = "Invoice"
  CASE ARHIST.TranType = '2'
    lcTitle = "Debit Adjustment"
  CASE ARHIST.TranType = '3'
    lcTitle = "Charge Back"
  CASE ARHIST.TranType = '4'
    lcTitle = "Payment"
  CASE ARHIST.TranType = '5'
    lcTitle = "Credit Adjustment"
  CASE ARHIST.TranType = '6'
    lcTitle = "Credit On Account"
ENDCASE

RETURN lcTitle
*--End of lfTitlNam