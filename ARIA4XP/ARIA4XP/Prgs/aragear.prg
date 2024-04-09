*:***************************************************************************
*: Program file  : ARAgeAr
*: Program desc. : Account Receiviable Aging
*: For screen    : ....
*:        System : Aria Advantage Series.
*:        Module : Account Receiviable (AR)
*:     Developer : Walid A. Wahab (WAB)
*:***************************************************************************
*: Calls :
*:     Procedures : lpAging,lpCalAge,lpReplCust
*:     Functions  : gfSetup,gfrange,gfThermo
*:***************************************************************************
*: Passed Parameters  : lcAccount
*:***************************************************************************
*: Example : DO ARAgeAr
*:***************************************************************************
*: Note that : If lcAccount is ommited it means that calling from menu.
*:           : else if it's character it means that calling from another program.
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*:B608267,1 WAM 09/16/07 Use system date Aria application property instead of function date()
*! B610313,1 HIA 04/18/2013 T20130410.0017 - Applied invoices to a check and saved error came up and stayed this way it doesn’t update
*:***************************************************************************

*
PARAMETERS lcAccount,lcDebitFile,lcCreditFile,lcArHistFile,lcDataSession
#INCLUDE R:\ARIA4XP\PRGS\aragear.H

lnoldDataSession = SET('DATASESSION')
IF TYPE('lcDataSession') = 'C'
  lnDataSession = VAL(lcDataSession)
  SET DATASESSION TO lnDataSession
ENDIF
lcDebitFile   = IIF(TYPE('lcDebitFile')='C',lcDebitFile,'DEBIT')
lcCreditFile  = IIF(TYPE('lcCreditFile')='C',lcCreditFile,'CREDIT')
lcArHistFile  = IIF(TYPE('lcArHistFile')='C',lcArHistFile,'ArHist')
STORE .F. TO llCustFile,llDebtFile,llCrdtFile,llArHsFile,llCurrFile,llContinue

STORE '' TO lcDebtOrd , lcCrdtOrd , lcArHstOrd
STORE 0  TO lnDebtRec , lnCrdtRec , lnArHstRec
loToolBarWindow = oAriaApplication.oToolBar.oWindParent
llFromPrg = TYPE('lcAccount') = 'C'
*ASM, Code copied from MAH to solve the problem of having a blank screen when voiding Invoice [Start]
IF llFromPrg AND TYPE('oAriaApplication.cHostFormName') = 'C' .AND. !EMPTY(oAriaApplication.cHostFormName)
  LOCAL lnIndex
  FOR lnIndex = 1 TO _SCREEN.FORMCOUNT
    IF _SCREEN.FORMS(lnIndex).NAME == oAriaApplication.cHostFormName
      _SCREEN.FORMS(lnIndex).RELEASE()
      EXIT
    ENDIF
  ENDFOR
ENDIF
*ASM, Code copied from MAH to solve the problem of having a blank screen when voiding Invoice [Start]
*-- Variables to deal with Multi currency [begin]
llMulCurr  = gfGetMemVar('llMulCurr') && if .T. , current company use multi currency.
lnCurrRate  = 1          && Variable to hold current rate
lnCurrUnit  = 1          && Variable to hold current unit
lcAUntSin   = '*'        && Variable to hold unit sign.
lcAExRSin   = '*'        && Variable to hold exchange rate sign.
*-- Variables to deal with Multi currency [end]

lcTempAcc = gfTempName()   && Name of file that hold temporary Account data.

*-- Open Customer File. [begin]
IF !USED('Customer')
  llCustFile = gfOpenFile(oAriaApplication.DataDir+'Customer',oAriaApplication.DataDir+'Customer','SH')
  llSuccess = CURSORSETPROP("Buffering", 5, "CUSTOMER")
ELSE
  lcCustOrd = ORDER('Customer')
  lnCustRec = RECNO('Customer')
ENDIF
*-- Open Customer File. [end  ]

SELECT Customer
GO TOP IN Customer

*-- if calling from another program, not empty and valid account.
IF TYPE('lcAccount') = 'C' AND !EMPTY(lcAccount) AND SEEK('M'+lcAccount)
  CREATE TABLE (oAriaApplication.WorkDir+lcTempAcc)  (Account C(5))
  INSERT INTO (lcTempAcc)                ;
    (Account) VALUES (lcAccount)
  llContinue = .T.
ELSE
  *--LangAccount      'Acct#'
  *--LangAccName      'Name'
  *--LangAccPhone		'Phone'
  *--LangAccCntry		'Country'
  *--LangAccBalance 	'Balance'

  lcBrowFlds = [account   :H = LangAccount  :10,  ;
    	 	 	  btName    :H = LangAccName  :35 , ;
				  Phone1    :H = LangAccPhone :16,  ;
			  	  cAddress6 :H = LangAccCntry :30,  ;
				  netBal    :H = LangAccBalance]

  *llContinue = gfrange(lcBrowFlds,lcTempAcc,"account",['M'])
  llContinue = gfBrowse(lcBrowFlds,"Accounts","CUSTOMER",['M'],.F.,.F.,.T.,.F.,.F.,.F.,;
    lcTempAcc,"Account",.F.,.F.,.F.,.F.,.F.,.F.,"CUSTOMER")
ENDIF

*-- If you call program from menu and user press < Ok >
IF llContinue
  DO lpAging  && Call procedure that evaluate account aging.
ENDIF

*-- Close open files [Begin]

USE IN (lcTempAcc)   && Erase cursor.
IF FILE(oAriaApplication.WorkDir+lcTempAcc+'.DBF')
  ERASE oAriaApplication.WorkDir+lcTempAcc+'.DBF'
ENDIF
=lfUpdateFiles()
SET DATASESSION TO lnoldDataSession
*!*************************************************************
*! Name      : lpAging
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/17/98
*! Purpose   : Initiall variables used in calculate aging and loop around
*!           : temporary cursor hold all accounts.
*!*************************************************************
*! Calls     :
*!             Procedures : lpCalAge
*!             Functions  : gfThermo
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpAging
*!*************************************************************
PROCEDURE lpAging
PRIVATE lcAccnCode,llDebit,llCredit,llArHist

*-- Start calculations.
SELECT (lcTempAcc)
COUNT TO lnAllAcct FOR !DELETED()  && All Accounts in Cursor file.
lnThermNo  = 0					&& Initiall Value for thermo Fn.

IF lnAllAcct > 0
  *-- Open Debit file first time call this code
  IF !USED(lcDebitFile)
    llDebtFile = gfOpenFile(oAriaApplication.DataDir+lcDebitFile,oAriaApplication.DataDir+lcDebitFile,'SH')
  ELSE

    IF !llDebtFile
      lcDebtOrd = ORDER(lcDebitFile)
      lnDebtRec = RECNO(lcDebitFile)

      *Set Default Index.
      SET ORDER TO Debit IN (lcDebitFile)
      GO TOP IN (lcDebitFile)
    ENDIF
  ENDIF
  IF !USED(lcCreditFile)
    llCrdtFile = gfOpenFile(oAriaApplication.DataDir+lcCreditFile,oAriaApplication.DataDir+lcCreditFile,'SH')
  ELSE
    IF !llCrdtFile
      lcCrdtOrd = ORDER(lcCreditFile)
      lnCrdtRec = RECNO(lcCreditFile)

      *Set Default Index.
      SET ORDER TO Credit IN (lcCreditFile)
      GO TOP IN (lcCreditFile)
    ENDIF
  ENDIF

  *-- Open ArHist file first time run this code
  IF !USED(lcArHistFile)  && if ArHist file not opened , open it by gfOpenFile [Begin.]
    llArHsFile = gfOpenFile(oAriaApplication.DataDir+lcArHistFile,oAriaApplication.DataDir+'Arhistht','SH')
  ELSE                && Else ArHist file is opened then save ORDER & RECNO

    IF !llArHsFile    && if ArHist file is opened by Module [Begin.]
      lcArHstOrd = ORDER(lcArHistFile)  && save current ORDER
      lnArHstRec = RECNO(lcArHistFile)  && save current RECNO

      *Set Default Index.
      SET ORDER TO Arhistht IN (lcArHistFile)
      GO TOP IN (lcArHistFile)
    ENDIF             && if ArHist file is opened by Module [End.]

  ENDIF              && if ArHist file not opened , open it by gfOpenFile [End.]
ENDIF

*--Progress bar object
*--Lang_PogrssCaption 	"Aging Account..."
oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
oProgress.TotalProgress = lnAllAcct
oProgress.lblFirstLabel.CAPTION = Lang_PogrssCaption
oProgress.SHOW()

*-- Scan all Accounts.
SELECT (lcTempAcc)
SCAN
  oProgress.lblFirstLabel.CAPTION = Lang_PogrssCaption + &lcTempAcc..Account
  oProgress.CurrentProgress(lnThermNo)
  lnThermNo = lnThermNo + 1

  = SEEK('M'+&lcTempAcc..Account,'CUSTOMER')
  lcAccnCode = &lcTempAcc..Account
  DO lpCalAge   && Calculate age for current account.
ENDSCAN         && end Scan all Accounts.
*-- End calculations.
oProgress=NULL
IF TYPE('oAriaApplication.oToolBar.oWindParent')<>'U'
  oAriaApplication.oToolBar.oWindParent = loToolBarWindow
ENDIF


*-- End of lpAging.

*!*************************************************************
*! Name      : lpCalAge
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/17/98
*! Purpose   : Calculate AR Aging.
*!*************************************************************
*! Calls     :
*!             Procedures : lpReplCust
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpCalAge
*!*************************************************************
PROCEDURE lpCalAge

*-- Define variables that calculates data in debit and credit files. [begin]
STORE 0.00 TO lnAge00   ,;
  lnAge30   ,;
  lnAge60   ,;
  lnAge90   ,;
  lnAge120

STORE 0.00 TO lnTerAge00   ,;
  lnTerAge30   ,;
  lnTerAge60   ,;
  lnTerAge90   ,;
  lnTerAge120

STORE 0.00 TO lnOpenAr   ,;
  lnOpenCr   ,;
  lnChgBack

*Define variables that calculates data from ArHist file.
STORE 0.00 TO lnPst12Avg

SELECT Customer
= RLOCK()
DO lpReplCust  && Clear data in customer file, Note that: all data equal zero.

llDebit  = SEEK(lcAccnCode,lcDebitFile)
llCredit = SEEK(lcAccnCode,lcCreditFile)

*returns True if seek in ArHist File is successful
llArHist = SEEK(lcAccnCode,lcArHistFile)

*-- If you find Transactions in debit file.
IF llDebit
  SELECT (lcDebitFile)
  *-- scan around this account in debit file.
  *! B610313,1 HIA 04/18/2013 T20130410.0017 - Applied invoices to a check and saved error came up and stayed this way it doesn’t update [Start]
  *Scan For account = lcAccnCode
  SCAN REST WHILE Account = lcAccnCode
  *! B610313,1 HIA 04/18/2013 T20130410.0017 - Applied invoices to a check and saved error came up and stayed this way it doesn’t update [End]
  
    *-- Computes age of a transaction based on the tran date
    *-- lnDays : Hold Number of days after transaction.
    *-- lcAges : Hold Variable to be changed (Age transaction variable).

    *Calculate equav. Base rate and unit from transaction file
    *-- if company use multi currency
    IF llMulCurr AND (CCURRCODE # oAriaApplication.BaseCurrency)
      lnAmount = lfGetCurr()   && Fill currency variabes.
    ELSE
      lnAmount = Amount
    ENDIF
    *B608267,1 WAM 09/16/07 Use system date Aria application property instead of function date()
    *lnDays    = DATE() - &lcDebitFile..TranDate
    lnDays    = oAriaApplication.SystemDate - &lcDebitFile..TranDate
    *B608267,1 WAM 09/16/07 (End)

    lcAges = 'lnAge'+ IIF(lnDays >= 120,'120',IIF(lnDays >= 90 ,'90'  ,;
      IIF(lnDays >= 60 ,'60' ,IIF(lnDays >= 30 ,'30','00'))))
    &lcAges = &lcAges + lnAmount       && Accomulate Age variable
    *-- Computes age of a transaction based on the tran date [end]

    *-- Computes age of a transaction based on the tran due date. [begin]
    *-- The due date is computed based on the data in the code file for terms.
    *-- lnDays : Hold Number of days after transaction is due date.
    *-- lcAges : Hold Variable to be changed (Age transaction due date variable).

    *B608267,1 WAM 09/16/07 Use system date Aria application property instead of function date()
    *lnDays = DATE() - IIF(EMPTY(&lcDebitFile..DueDate),            ;
    &lcDebitFile..TranDate+30, &lcDebitFile..DueDate)
    lnDays = oAriaApplication.SystemDate - IIF(EMPTY(&lcDebitFile..DueDate),            ;
      &lcDebitFile..TranDate+30, &lcDebitFile..DueDate)
    *B608267,1 WAM 09/16/07 (End)

    lcAges = 'lnTerAge'+ IIF(lnDays >= 91,'120',IIF(lnDays >= 61 ,'90'  ,;
      IIF(lnDays >= 31 ,'60' ,IIF(lnDays >= 1 ,'30','00'))))

    &lcAges = &lcAges + lnAmount   && Accomulate Age due date variable
    *-- Computes age of a transaction based on the tran due date. [end]

    lnChgBack = IIF(&lcDebitFile..TranType = '3',lnChgBack + lnAmount,lnChgBack)

  ENDSCAN   && end scan around this account in debit file.
ENDIF

*-- If you find Transactions in credit file.
IF llCredit

  *-- Summarize all unapplied Credits.
  SELECT (lcCreditFile)

  SCAN REST WHILE Account = lcAccnCode
    * Calculate equav. Base rate and unit from transaction file
    IF llMulCurr AND (CCURRCODE # oAriaApplication.BaseCurrency)
      lnCrAmt = lfGetCurr()
    ELSE
      lnCrAmt = Amount
    ENDIF
    lnOpenCr  = lnOpenCr + lnCrAmt
  ENDSCAN
ENDIF

*collect Last 12 months Avg. days past due [Begin.]
IF llArHist   && If you find Transactions in ArHist file. [Begin.]
  *-- lnDateDiff for Date Difference between History Date and Due Date
  *-- lnInvoice for number of Invoices payed by Direct Payments
  STORE 0  TO lnDateDiff , lnInvoice , lnDiffer
  STORE '' TO lcHistory

  *-- Open ARHIST file with another alias
  USE (oAriaApplication.DataDir+'ARHIST') AGAIN ALIAS ARHIST_X ORDER TAG Arhistht IN 0

  *-- Summarize all ArHist.
  SELECT (lcArHistFile)
  =SEEK(lcAccnCode)
  *B608267,1 WAM 09/16/07 Use system date Aria application property instead of function date()
  *SUM (HistDate-DueDate) WHILE Account+HISTORY+TranType+TRAN+Cinstalno = lcAccnCode ;
  FOR TranType= '1' .AND. BETWEEN(HistDate,GOMONTH(DATE(),-12),DATE()) ;
  .AND. lfVChekPay() TO lnDateDiff
  SUM (HistDate-DueDate) WHILE Account+HISTORY+TranType+TRAN+Cinstalno = lcAccnCode ;
    FOR TranType= '1' .AND. BETWEEN(HistDate,GOMONTH(oAriaApplication.SystemDate,-12),oAriaApplication.SystemDate) ;
    .AND. lfVChekPay() TO lnDateDiff
  *B608267,1 WAM 09/16/07 (End)

  *-- Close ARHIST_X file
  USE IN ARHIST_X

  *-- lnPst12Avg returns Average paid invoices days within one year starting from today
  IF lnInvoice <> 0
    lnPst12Avg = CEIL(lnDateDiff/lnInvoice)
  ENDIF

ENDIF   && If you find Transactions in ArHist file. [End.]



SELECT Customer
*-- if there is debit or credit or ArHist transactions, Post new customer balance.

*if there's transactions update the selected account
IF llDebit OR llCredit OR llArHist  && include also ArHist Flag in the if condition
  DO lpReplCust
ENDIF
UNLOCK  && Unlocking customer file.
*-- end of lpCalAge.

*!*************************************************************
*! Name      : lpReplCust
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/17/98
*! Purpose   : Transfer Aged data to customer file.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpReplCust
*!*************************************************************
PROCEDURE lpReplCust
*B608267,1 WAM 09/16/07 Use system date Aria application property instead of function date()
*REPLACE AgeDate    WITH DATE()                                           ,;
CURRENT    WITH lnAge00                                          ,;
Age30      WITH lnAge30                                          ,;
Age60      WITH lnAge60                                          ,;
Age90      WITH lnAge90                                          ,;
Age120     WITH lnAge120                                         ,;
TerCurrent WITH lnTerAge00                                       ,;
TerAge30   WITH lnTerAge30                                       ,;
TerAge60   WITH lnTerAge60                                       ,;
TerAge90   WITH lnTerAge90                                       ,;
TerAge120  WITH lnTerAge120                                      ,;
TotAge     WITH lnAge00 + lnAge30 + lnAge60 + lnAge90 + lnAge120 ,;
OpenCr     WITH lnOpenCr                                         ,;
ChgBack    WITH lnChgBack                                        ,;
NetBal     WITH TotAge + lnOpenCr                                ,;
nPast12Avg WITH lnPst12Avg

REPLACE AgeDate  WITH oAriaApplication.SystemDate                    ,;
  CURRENT    WITH lnAge00                                          ,;
  Age30      WITH lnAge30                                          ,;
  Age60      WITH lnAge60                                          ,;
  Age90      WITH lnAge90                                          ,;
  Age120     WITH lnAge120                                         ,;
  TerCurrent WITH lnTerAge00                                       ,;
  TerAge30   WITH lnTerAge30                                       ,;
  TerAge60   WITH lnTerAge60                                       ,;
  TerAge90   WITH lnTerAge90                                       ,;
  TerAge120  WITH lnTerAge120                                      ,;
  TotAge     WITH lnAge00 + lnAge30 + lnAge60 + lnAge90 + lnAge120 ,;
  OpenCr     WITH lnOpenCr                                         ,;
  ChgBack    WITH lnChgBack                                        ,;
  NetBal     WITH TotAge + lnOpenCr                                ,;
  nPast12Avg WITH lnPst12Avg
*B608267,1 WAM 09/16/07 (End)

*-- end of lpReplCust.

*!*************************************************************
*! Name      : lfGetCurr
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/17/98
*! Purpose   : Calculate Currency unit, rate, and signs.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfGetExSin,gfChkRate
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lfGetCurr
*!*************************************************************
FUNCTION lfGetCurr
PRIVATE lcCurrAlas , lnEqAmt
lnEqAmt = 0
lcCurrAlas = SELECT(0)

*--Because currency code file used per record in debit and [Begin]
*--credit files in scan loop, it's a good tech. to open
*--it first time call this function and close it in program

IF !llCurrFile AND !USED('SYCCURR')
  *Amin 01/13/2004 [Start]
  *llCurrFile = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  llCurrFile = gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
  *- Amin 01/13/2004 [End]
ENDIF

*Calculate equav. Base rate and unit from transaction file [Begin]
SELECT (lcCurrAlas)
lnEqAmt = gfAmntDisp(Amount,"O",TranDate)

RETURN lnEqAmt
*-- end of lfGetCurr.

*!**************************************************************************
*! Name      : lfvChekPay
*! Developer : Sameh (SSE)
*! Date      : 06/09/1999
*! Purpose   : Returns True if Invoice (TranType='1') has payment (TranType='4')
*!*************************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Returns            : True OR False
*!*************************************************************************
*! Example   : =lfvChekPay()
*!*************************************************************************
*E301246,1
FUNCTION lfVChekPay
IF SEEK(lcAccnCode + &lcArHistFile..HISTORY + '4','ARHIST_X')
  lnInvoice = lnInvoice + 1
  RETURN .T.
ELSE
  RETURN .F.
ENDIF
*-- End of lfvChekPay.


*!**************************************************************************
*! Name      : lfUpdateFiles
*! Developer : Walid A. Wahab (WAB)
*! Date      : 08/05/2003
*! Purpose   : update an close the files
*!*************************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Example   : =lfUpdateFiles()
*!*************************************************************************
FUNCTION lfUpdateFiles

IF !llFromPrg
  =lfTableUpdate()
  IF  llCustFile AND USED('CUSTOMER')
    USE IN 'CUSTOMER'
  ENDIF
  IF llDebtFile AND USED(lcDebitFile)
    USE IN (lcDebitFile)
  ENDIF
  IF llCrdtFile AND USED(lcCreditFile)
    USE IN (lcCreditFile)
  ENDIF
  IF llArHsFile AND USED(lcArHistFile)
    USE IN (lcArHistFile)
  ENDIF
  IF llCurrFile AND USED('SYCCURR')
    USE IN 'SYCCURR'
  ENDIF
ELSE
  IF !llCustFile AND USED('CUSTOMER')
    SELECT Customer
    SET ORDER TO &lcCustOrd
    IF lnCustRec > 0 AND lnCustRec <= RECCOUNT('Customer')
      GO lnCustRec
    ENDIF
  ENDIF
  IF !llDebtFile AND USED(lcDebitFile)
    SELECT (lcDebitFile)
    SET ORDER TO &lcDebtOrd
    IF lnDebtRec > 0 AND lnDebtRec <= RECCOUNT(lcDebitFile)
      GO lnDebtRec
    ENDIF
  ENDIF
  IF !llCrdtFile AND USED(lcCreditFile)
    SELECT (lcCreditFile)
    SET ORDER TO &lcCrdtOrd
    IF lnCrdtRec > 0 AND lnCrdtRec <= RECCOUNT(lcCreditFile)
      GO lnCrdtRec
    ENDIF
  ENDIF
  IF !llArHsFile AND USED(lcArHistFile)
    SELECT (lcArHistFile)
    SET ORDER TO &lcArHstOrd  && restore the old order
    IF lnArHstRec > 0 AND lnArHstRec <= RECCOUNT(lcArHistFile)
      GO lnArHstRec           && restore old Recno
    ENDIF
  ENDIF              && if file is opened already select it first [End.]
ENDIF
*-End of Function  code...

*!**************************************************************************
*! Name      : lfTableUpdate
*! Developer : Walid A. Wahab (WAB)
*! Date      : 08/05/2003
*! Purpose   : Table Update ( phisical update )
*!*************************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Example   : =lfTableUpdate()
*!*************************************************************************
FUNCTION lfTableUpdate
LOCAL lnAlias,llSuccess
lnAlias = SELECT()
DIMENSION laErrors[1]
SELECT Customer
lcErrorMessage=""
llSuccess = TABLEUPDATE(.T.,.T.)
UNLOCK ALL
SELECT (lnAlias)
RETURN llSuccess
