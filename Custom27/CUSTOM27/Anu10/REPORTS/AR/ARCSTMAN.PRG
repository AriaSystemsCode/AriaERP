*:**************************************************************************
*: Program file  : ARCSTMAN
*: Program desc. : Custom Customer Statement for A'Nue
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 11/09/2000
*: Reference     : C102023 (Also refer to C100871 A26) 
*:**************************************************************************
*: Calls : 
*:    Procedures : lpPrint
*:    Functions  : lfItmPos , gfGetAdr , lfAdrShift
*:**************************************************************************
*: Example : DO ARCSTMAN
*: B122417,1 BWA 04/08/2004 Fix the bug of the account bill to name.
*:**************************************************************************
*

PRIVATE lcGroupKey , lcAccount , lnTotal , ldHighDate , ldLowDate , lnX
DIMENSION laBtAddres[5,1]
STORE '' TO lcGroupKey , laBtAddres
STORE {} TO ldLowDate , ldHighDate
llNewCust = .T.

lnTranDate = lfItmPos('DEBIT.TRANDATE')   && get Tran. Date Fixed filter Position
IF lnTranDate > 0
  ldLowDate  = CTOD(SUBSTR(laOGFxFlt[lnTranDate,6],1,10))
  ldHighDate = CTOD(SUBSTR(laOGFxFlt[lnTranDate,6],12))
ENDIF
ldLowDate  = IIF(EMPTY(ldLowDate),{01/01/01},ldLowDate)
ldHighDate = IIF(EMPTY(ldHighDate),DATE(),ldHighDate)

ROW = 1
SET DEVICE TO PRINT
DO lpPrint           && main printing procedure
SET DEVICE TO SCREEN
RETURN
*-- End of Report.

*!**************************************************************************
*! Name      : lpPrint
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/09/2000
*! Purpose   : To print all the @ Say
*!**************************************************************************
*! Example   : DO lpPrint
*!**************************************************************************
*
PROCEDURE lpPrint
*B122417,1 BWA 08/04/2004 Add variable hold the Btname value.[START]
PRIVATE lcCustName
STORE SPACE(0) TO lcCustName
*B122417,1 BWA 08/04/2004.[END]

DO WHILE INKEY() <> 32
  SELECT (lcTmpAcct)
  IF EOF()
    EXIT
  ENDIF

  SELECT (lcTmpTrans)
  IF llNewCust 
    lnTotal = 0           && reset Total for each new account
    lcGroupKey = cGroupKey
    lcAccount  = Account

    IF SEEK('M'+Account,'Customer')
      =gfGetAdr('Customer','','','',@laBtAddres,2)
      =lfAdrShift('laBtAddres')

      *B122417,1 BWA 08/04/2004 The variable hold the value from the customer file.[START]
      lcCustName = ALLTRIM(Customer.Btname)
      *B122417,1 BWA 08/04/2004.[END]

    ENDIF

    SEEK lcGroupKey
    *-- this count to move pointer in transaction file where there is no null date { / / }
    COUNT REST TO lnX WHILE cGroupKey = lcGroupKey AND TRANDATE <= ldLowDate  

    llNewCust = .F.
  ENDIF

  DO WHILE cGroupKey = lcGroupKey
    llPrint = .F.
   
    lnTotal = lnTotal + Amount
    @ ROW,42 SAY TranDate
    @ ROW,51 SAY Tran
    @ ROW,58 SAY MAX(ldHighDate-TranDate, 0) PICTURE '999'
    IF Amount > 0
      @ ROW,61 SAY Amount PICTURE '@Z 99999.99'
    ELSE
      @ ROW,69 SAY Amount PICTURE '@Z 999999.99'
    ENDIF
    SKIP
    ROW = ROW + 1
    IF ROW = 20 OR ROW = 52
      llPrint=.T.
      IF ROW = 52
        ROW = 53
      ENDIF
      @ ROW,04 SAY DATE()
      @ ROW,16 SAY lcAccount
      @ ROW,25 SAY lnTotal PICTURE '9999999.99'
      @ ROW,78 SAY lnTotal PICTURE '9999999.99'
      ROW = ROW + 2

      *B122417,1 BWA 08/04/2004 Print the right value.[START]
      *@ ROW,5 SAY &lcTmpAcct..BtName
      @ ROW,5 SAY lcCustName
      *B122417,1 BWA 08/04/2004.[END]

      ROW = ROW + 1
      @ ROW,5 SAY laBtAddres[1,1]
      ROW = ROW + 1
      @ ROW,5 SAY laBtAddres[2,1]
      ROW=ROW+1
      @ ROW,5 SAY laBtAddres[3,1]

      IF ROW > 52
        EXIT
      ELSE
        ROW = ROW + 9
      ENDIF
    ENDIF
  ENDDO
  
  IF !llPrint
    IF ROW < 23
      ROW = 20
    ELSE
      ROW = 53
    ENDIF
    @ ROW,04 SAY DATE()
    @ ROW,16 SAY lcAccount
    @ ROW,25 SAY lnTotal PICTURE '9999999.99'
    @ ROW,78 SAY lnTotal PICTURE '9999999.99'

    ROW = ROW + 4

    *B122417,1 BWA 08/04/2004 Print the right value.[START]
    *@ ROW,7 SAY &lcTmpAcct..BtName
    @ ROW,7 SAY lcCustName
    *B122417,1 BWA 08/04/2004.[END]

    ROW = ROW + 1
    @ ROW,7 SAY laBtAddres[1,1]
    ROW = ROW + 1
    @ ROW,7 SAY laBtAddres[2,1]
    ROW = ROW + 1
    @ ROW,7 SAY laBtAddres[3,1]
  ENDIF

  IF !(cGroupKey == lcGroupKey)
    SELECT (lcTmpAcct)
    SKIP
    llNewCust = .T.
  ENDIF

  IF ROW > 30
    ROW = 1
  ELSE
    ROW = 34
  ENDIF
ENDDO
*-- End of lpPrint