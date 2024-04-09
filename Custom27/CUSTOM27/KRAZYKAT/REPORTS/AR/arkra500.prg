*:***************************************************************************
*: Program file  : ARKRA500.PRG
*: Program desc. : Custom Open ChargeBack Report.
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*: Customer      : Krazy Kat Ltd.
*:***************************************************************************
*: C101634,1 KHM 10/14/99
*:***************************************************************************
*: Modifications     : ........
*:B603997,1 BWA 10/31/2000 Fix the bug of variable factor not found.
*:B604026,1 BWA 11/20/2000 Make a Dos format for the report 500 & 501.
*:B606214,1 SSE 08/15/2002 Fix alignment in detail form ARKRA500.
*:B606214,1                All modifications is in FRX.
*:B606588,1 RAE 11/13/2002 Stop printing in line number 55 instead of 57 in the FRX.
*:***********************************************************************************
*-- Initializing the necessary variables.
lcRpSpeFac = IIF(lcRpFHB <> 'F',SPACE(6),lcRpSpeFac)
lcRpKeyOff = IIF(ldRpEDate >= gdSysDate,'N',lcRpKeyOff)
lcTempName = gfTempName()
lcAcc      = SPACE(5)
lnTotal    = 0

*-- Creating the temporary file.
CREATE TABLE (gcWorkDir+lcTempName) (Account C(5),Tran C(10),TranDate D,Typ C(1),;
                        Reference C(43) ,Amount N(10,2))
INDEX ON Account TAG (lcTempName) 

IF lcRpKeyOff = 'Y'
  lcArFilter = "HistDate >ldRpEDate AND TranDate <=ldRpEDate"
ENDIF

lcFilter = "TRANDATE <= ldRpEDate"

DO CASE
  CASE EMPTY(lcRpFAcc) AND EMPTY(lcRpTAcc) 
    lcFilter = lcFilter
  CASE EMPTY(lcRpFAcc) 
    lcFilter = lcFilter + " AND Account <= lcRpTAcc "
    IF lcRpKeyOff = 'Y'
      lcArFilter = lcArFilter +  " AND Account <= lcRpTAcc "
    ENDIF
  CASE EMPTY(lcRpTAcc)
    lcFilter = lcFilter + " AND Account >= lcRpFAcc "
    IF lcRpKeyOff = 'Y'
      lcArFilter = lcArFilter +  " AND Account >= lcRpFAcc "
    ENDIF
  OTHERWISE
    lcFilter = lcFilter + " AND BETWEEN(Account,lcRpFAcc,lcRpTAcc)"
    IF lcRpKeyOff = 'Y'
      lcArFilter = lcArFilter +  " AND BETWEEN(Account,lcRpFAcc,lcRpTAcc)"
    ENDIF
ENDCASE

*B603997,1 BWA 10/31/2000 Fix the bug of variable factor not found.[START]
*lcFilter1 = IIF(lcRpFHB = 'F',IIF(EMPTY(lcRpSpeFac),'!EMPTY(Factor)',;
            'Factor = lcRpSpeFac'),IIF(lcRpFHB = 'H','EMPTY(Factor)','.T.'))

lcFilter1 = IIF(lcRpFHB = 'F',IIF(EMPTY(lcRpSpeFac),'!EMPTY(cFacCode)',;
            'cFacCode = lcRpSpeFac'),IIF(lcRpFHB = 'H','EMPTY(cFacCode)','.T.'))
*B603997,1 [END]

*-- Selecting records for report 
WAIT WINDOW 'Collecting data. Please wait ...' NOWAIT

SELECT Debit
SET RELATION TO 'M'+Account INTO Customer

SCAN FOR &lcFilter AND TRANTYPE = '3' AND (!BETWEEN(TranCode,'42','50'))

  SELECT Customer
  IF !EVAL(lcFilter1)
    SELECT DEBIT
    LOOP
  ENDIF

  SELECT Debit
  INSERT INTO (lcTempName) ;
    (Account ,Tran ,TranDate ,Typ ,Reference,Amount);
    VALUES (Debit.Account,SUBSTR(Debit.Reference,1,9),Debit.TranDate,;
    'D', lfGetRef("DEBIT") , Debit.Amount)
ENDSCAN
SET RELATION TO 

SELECT Credit
SET RELATION TO 'M'+ACCOUNT INTO Customer

SCAN FOR &lcFilter AND TRANTYPE $ '056' AND (!BETWEEN(cCreditCod,'42','50'))
  IF Trantype $ '06'
    SELECT Customer
  ENDIF  
  IF !EVAL(lcFilter1)
    SELECT Credit
    LOOP
  ENDIF

  SELECT Credit
  INSERT INTO (lcTempName) ;
   (Account ,Tran ,TranDate ,Typ ,Reference,Amount);
   VALUES (Credit.Account,SUBSTR(Credit.Reference,1,9) ,Credit.TranDate,;
   IIF(Credit.TranType $ '05','C','P'), lfCrFields(),Credit.Amount)
ENDSCAN
SET RELATION TO

IF lcRpKeyOff = 'Y'
  SELECT ARHIST
  SET ORDER TO TAG ARHISTHT IN "ARHIST"
  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
  SCAN FOR &lcArFilter AND TRANTYPE $ '0356' AND (!BETWEEN(TranCode,'42','50'))
    SELECT CUSTOMER
    IF !EVAL(lcFilter1)
      SELECT ARHIST
      LOOP
    ENDIF
    IF lfKeyedOff()
      SELECT ARHIST
      LOOP
    ENDIF
    SELECT ARHIST
    INSERT INTO (lcTempName) ;
      (ACCOUNT ,TRAN,TRANDATE ,TYP ,REFERENCE,AMOUNT);
      VALUES (ARHIST.ACCOUNT,SUBSTR(ARHIST.REFERENCE,1,9),ARHIST.TRANDATE,;
      lfRetType(), lfgetRef("ARHIST") , ARHIST.AMOUNT)
  ENDSCAN
  SET RELATION TO
ENDIF
WAIT CLEAR

SELECT (lcTempName)
SET ORDER TO TAG (lcTempName)
SCAN
  lcAcc = Account
  lnAccTot = lfTotal()
  IF lnAccTot = 0
    lnRecNo = RECNO()
    =SEEK(lcAcc)
    REPLACE REST Typ WITH 'Z' WHILE Account = lcAcc 
    GOTO lnRecNo
  ENDIF
ENDSCAN
DELETE ALL FOR Typ = 'Z'

GO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ELSE
  SET RELATION TO 'M'+Account INTO CUSTOMER
  DO gfDispRe WITH EVAL('lcRpForm')
ENDIF
SET DEVICE TO SCREEN
USE IN (lcTempName)
ERASE (gcWorkDir+lcTempName)+'.DBF'
ERASE (gcWorkDir+lcTempName)+'.CDX'

*!*************************************************************
*! Name      : lfGetRef
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/14/99
*! Purpose   : To get the reference of the ChargeBack.
*:*************************************************************
*! Example     : = lfGetRef()
*!*************************************************************
FUNCTION lfGetRef
PARAMETERS lcAlias
PRIVATE lnAlias

lnAlias = SELECT()
lcExpr  = ALLTRIM(&lcAlias..DESC)+SUBSTR(&lcAlias..REFERENCE,10,11)
lcArOrd = ORDER('ARHIST')
lnRecNo = RECNO('ARHIST')

SET ORDER TO TAG ARHISTT IN "ARHIST"
=SEEK(&lcAlias..ACCOUNT+&lcAlias..TRAN,"ARHIST")
lcHistory = ARHIST.HISTORY
SET ORDER TO TAG ARHISTHT IN "ARHIST"

IF SEEK(&lcAlias..ACCOUNT+lcHistory+"4","ARHIST")
  lcExpr = ALLTRIM(ARHIST.STORE) + ' ' +lcExpr
ENDIF
SELECT ArHist
SET ORDER TO (lcArOrd) IN ARHIST
IF BETWEEN(lnRecNo,1,RecCount())
  GOTO  lnRecNo IN ARHIST
ENDIF
SELECT(lnAlias)
RETURN lcExpr


*!*************************************************************
*! Name      : lfPrtAcc
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/14/99
*! Purpose   : To determine if we 'll print the account name and 
*!           : code or not.
*:*************************************************************
*! Example     : = lfPrtAcc()
*!**************************************************************************
FUNCTION lfPrtAcc

IF lcAcc = Account
  RETURN
ENDIF
lcAcc   = Account
lnTotal = 0
RETURN .F.

*!*************************************************************
*! Name      : lfAcc
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/14/99
*! Purpose   : To assign the account code.
*:*************************************************************
*! Example     : = lfAcc()
*!*************************************************************
FUNCTION lfAcc
lcAcc = Account
RETURN " "

*!*************************************************************
*! Name      : lfCrFields
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/14/99
*! Purpose   : To get the reference filed
*:*************************************************************
*! Example     : = lfCrFields()
*!*************************************************************
FUNCTION lfCrFields

SET ORDER TO TAG ARHISTT IN "ARHIST"
=SEEK(Credit.Account+Credit.Tran,"ARHIST")
lcHistory = ArHist.History
SET ORDER TO TAG ARHISTHT IN "ARHIST"
IF SEEK(Credit.Account+lcHistory+"4","ARHIST")
  RETURN (ALLTRIM(ARHIST.STORE) +' '+ALLTRIM(Credit.Desc)+SUBSTR(Credit.Reference,10,11))
ELSE
  RETURN (ALLTRIM(Credit.Desc)+SUBSTR(Credit.Reference,10,11))
ENDIF

*!*************************************************************
*! Name      : lfKeyedOff
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/14/99
*! Purpose   : To get the reference filed
*:*************************************************************
*! Example     : = lfKeyedOff()
*!*************************************************************
FUNCTION lfKeyedOff
PRIVATE lnAlias

RETURN SEEK(ARHIST.Account+ARHIST.Tran+DTOS(ARHIST.TranDate),'DEBIT') .OR.;
       SEEK(ARHIST.Account+ARHIST.Tran+DTOS(ARHIST.TranDate),'CREDIT')

*!*************************************************************
*! Name      : lfRetType
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/14/99
*! Purpose   : To get the reference filed
*:*************************************************************
*! Example     : = lfRetType()
*!*************************************************************
FUNCTION lfRetType
PRIVATE lnAlias
lnAlias = SELECT()
DO CASE
 CASE ARHIST.TRANTYPE $ '05' 
  RETURN 'C'
 CASE ARHIST.TRANTYPE $ '3' 
  RETURN 'D'
 CASE ARHIST.TRANTYPE $ '6' 
  RETURN 'P'
ENDCASE
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfTotal
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/14/99
*! Purpose   : To return customer's total
*:*************************************************************
*! Example     : = lfTotal()
*!*************************************************************
FUNCTION lfTotal
PRIVATE lnAlias,lnRecNo

lnAlias = SELECT()
lnTotal = 0
SELECT (lcTempName)
=SEEK(lcAcc)
SCAN WHILE ACCOUNT = lcAcc
  lnTotal = lnTotal+AMOUNT
ENDSCAN
IF !BOF()
  SKIP-1
ENDIF
SELECT(lnAlias)
RETURN lnTotal

*!*************************************************************
*! Name      : lfvPEDate
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/14/99
*! Purpose   : To validate the period ending date.
*:*************************************************************
*! Example     : = lfvPEDate()
*!*************************************************************
FUNCTION lfvPEDate

IF EMPTY(ldRpEDate)
  ldRpEDate = gdSysDate
ENDIF

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Khalid Mohi El-Din Mohamed KHM
*! Date      : 10/14/1999
*! Purpose   : To validate the account range.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvAccount()
*!*************************************************************
FUNCTION lfvAccount

lcObjNam = SYS(18)
lcAccNo   = EVALUATE(SYS(18))

IF lcObjNam = "LCOGVALUEF" 
  IF '?' $ lcAccNo .OR. (!EMPTY(lcAccNo) .AND. !SEEK('M' + lcAccNo , 'Customer'))
    =CusBrowM(@lcAccNo , '' , 'M')
  ENDIF
  lcRpFAcc = lcAccNo
ELSE
  IF '?' $ lcAccNo .OR. (!EMPTY(lcAccNo) .AND. !SEEK('M' + lcAccNo , 'Customer'))
    =CusBrowM(@lcAccNo , '' , 'M')
  ENDIF
  lcRpTAcc = lcAccNo
ENDIF
&lcObjNam = lcAccNo


*!*************************************************************
*! Name      : lfClrRead
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/14/99
*! Purpose   : To referesh the Factored/House/Both option.
*:*************************************************************
*! Example     : = lfClrRead()
*!*************************************************************
FUNCTION lfClrRead
CLEAR READ

*!*************************************************************
*! Name      : lfvForm
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/14/99
*! Purpose   : To assign the selected form
*:*************************************************************
*! Example     : = lfvForm()
*!*************************************************************
FUNCTION lfvForm
IF lcRpRepFrm = 'D' 
  lcRpForm  = 'ARKRA500'
ELSE
  lcRpForm  = 'ARKRA501'
ENDIF

*!*************************************************************
*! Name      : lfOgWhen
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/14/99
*! Purpose   : Function to be executed in the when of the OG.
*:*************************************************************
*! Example     : = lfOgWhen()
*!*************************************************************
FUNCTION lfOgWhen

ldRpEDate = gdSysDate
R_WIDTH   = 'N'