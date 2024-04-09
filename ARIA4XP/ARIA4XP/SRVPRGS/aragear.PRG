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
*:N000623,1 MMT 09/29/2008 Convert program to Aria5[T20080413.0001]
*:***************************************************************************
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*PARAMETERS lcRequestID, lcXMLFileName
PARAMETERS lcRequestID, lcXMLFileName, ClientID
*T20100512.0026 Hassan 2010 05 23 [BEGIN]

PRIVATE loAgent
loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

PRIVATE loProgress
loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

loProgress.Percent = 0
loProgress.DESCRIPTION = "Opening Data Files..."
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*loAgent.UpdateObjectProgress(lcRequestID, loProgress)
loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
*T20100512.0026 Hassan 2010 05 23 [END]

LOCAL loEnvironment
loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
loEnvironment.ClientID = ClientID
loEnvironment.ConnectionsRefresh()
*T20100512.0026 Hassan 2010 05 23 [END]

LOCAL lcCurrentProcedure
*!*  lcCurrentProcedure = "y:\aria4xp\SRVPRGS"
*!*  DO (lcCurrentProcedure + "\SY\ariamain.fxp") WITH '99'
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)

*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID)
DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID),ClientID
*T20100512.0026 Hassan 2010 05 23 [END]

STORE .F. TO llCustFile,llDebtFile,llCrdtFile,llArHsFile,llCurrFile,llContinue
oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)


oAriaEnvironment.activeModuleID = 'AR'
PUBLIC gcAct_Appl
gcAct_Appl = "AR"

STORE '' TO lcDebtOrd , lcCrdtOrd , lcArHstOrd
STORE 0  TO lnDebtRec , lnCrdtRec , lnArHstRec
*-- Variables to deal with Multi currency [begin]
llMulCurr  = oAriaEnvironment.setups.getSetting('LLMULCURR') && if .T. , current company use multi currency.
lnCurrRate  = 1          && Variable to hold current rate
lnCurrUnit  = 1          && Variable to hold current unit
lcAUntSin   = '*'        && Variable to hold unit sign.
lcAExRSin   = '*'        && Variable to hold exchange rate sign.
*-- Variables to deal with Multi currency [end]


lnPosAcc = ASCAN(laogFXflt,'CUSTOMER.ACCOUNT')
lcAccFile = ''
llAccSelect = .F.
IF lnPosAcc <> 0
  lnPosAcc = ASUBSCRIPT(laogFXflt,lnPosAcc,1)
  lcAccFile =  laogFXflt[lnPosAcc,6]
  IF !EMPTY(lcAccFile ) AND USED(lcAccFile )
    SELECT (lcAccFile )
    LOCATE
    IF !EOF()
      llAccSelect = .T.
      lcTempAcc  = lcAccFile
    ENDIF
  ENDIF
ENDIF

IF !llAccSelect
  lcTempAcc = oAriaEnvironment.CURSORS.GetCursorTempName()   && Name of file that hold temporary Account data.
  CREATE TABLE (oAriaApplication.WorkDir+lcTempAcc)  (Account C(5))
  =oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+'Customer',oAriaEnvironment.DataDir+'Customer','SH','Customers')
  SELECT 'Customers'
  =SEEK("M")
  SCAN REST WHILE TYPE+Account+STORE = 'M'
    INSERT INTO (lcTempAcc)                ;
      (Account) VALUES (Customers.Account)
  ENDSCAN
  llAccSelect = .T.
ENDIF

*lcTempAcc = oAriaEnvironment.Cursors.GetCursorTempName()   && Name of file that hold temporary Account data.

*-- Open Customer File. [begin]
IF !USED('Customer')
  llCustFile = oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+'Customer',oAriaEnvironment.DataDir+'Customer','SH')
  llSuccess = CURSORSETPROP("Buffering", 5, "CUSTOMER")
ELSE
  lcCustOrd = ORDER('Customer')
  lnCustRec = RECNO('Customer')
ENDIF
*-- Open Customer File. [end  ]

SELECT Customer

*-- if calling from another program, not empty and valid account.
llContinue = .T.

*-- If you call program from menu and user press < Ok >
IF llContinue
  DO lpAging  && Call procedure that evaluate account aging.
ENDIF

*-- Close open files [Begin]

USE IN (lcTempAcc)   && Erase cursor.
IF FILE(oAriaEnvironment.WorkDir+lcTempAcc+'.DBF')
  ERASE oAriaEnvironment.WorkDir+lcTempAcc+'.DBF'
ENDIF
=lfUpdateFiles()
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
  IF !USED('Debit')
    llDebtFile = oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+'Debit',oAriaEnvironment.DataDir+'Debit','SH')
  ENDIF
  IF !USED('Credit')
    llCrdtFile = oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+'Credit',oAriaEnvironment.DataDir+'Credit','SH')
  ENDIF

  *-- Open ArHist file first time run this code
  IF !USED('Arhist')  && if ArHist file not opened , open it by oAriaEnvironment.remotetableaccess.OpenTable [Begin.]
    llArHsFile = oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+'Arhist',oAriaEnvironment.DataDir+'Arhistht','SH')
  ENDIF              && if ArHist file not opened , open it by oAriaEnvironment.remotetableaccess.OpenTable [End.]
ENDIF


*-- Scan all Accounts.
SELECT (lcTempAcc)
SCAN
  lnPerCent = RECNO()/lnAllAcct
  IF MOD(RECNO(), CEILING(lnAllAcct / 10)) = 0
    loProgress.Percent = lnPerCent
    loProgress.DESCRIPTION = "Aging Account :"+&lcTempAcc..Account
    *T20100512.0026 Hassan 2010 05 23 [BEGIN]
    *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
    *T20100512.0026 Hassan 2010 05 23 [END]
  ENDIF
  = oAriaEnvironment.remotetableaccess.SeekRecord('M'+&lcTempAcc..Account,'CUSTOMER')
  lcAccnCode = &lcTempAcc..Account
  DO lpCalAge   && Calculate age for current account.
ENDSCAN         && end Scan all Accounts.
*-- End calculations.


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
DO lpReplCust  && Clear data in customer file, Note that: all data equal zero.

llDebit  = oAriaEnvironment.remotetableaccess.SeekRecord(lcAccnCode,'Debit')
llCredit = oAriaEnvironment.remotetableaccess.SeekRecord(lcAccnCode,'Credit')

llArHist = oAriaEnvironment.remotetableaccess.SeekRecord(lcAccnCode,'ARHIST')

*-- If you find Transactions in debit file.
IF llDebit
  SELECT Debit
  *-- scan around this account in debit file.
  SCAN REST WHILE Account+TRAN+CINSTALNO+DTOS(TRANDATE) =  lcAccnCode

    *-- Computes age of a transaction based on the tran date
    *-- lnDays : Hold Number of days after transaction.
    *-- lcAges : Hold Variable to be changed (Age transaction variable).

    *Calculate equav. Base rate and unit from transaction file
    *-- if company use multi currency
    IF llMulCurr AND (CCURRCODE # oAriaEnvironment.BaseCurrency)
      lnAmount = lfGetCurr()   && Fill currency variabes.
    ELSE
      lnAmount = Amount
    ENDIF

    lnDays    = oAriaEnvironment.SystemDate - Debit.TRANDATE


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
    lnDays = oAriaEnvironment.SystemDate - IIF(EMPTY(Debit.DueDate),            ;
      Debit.TRANDATE+30, Debit.DueDate)
    *B608267,1 WAM 09/16/07 (End)

    lcAges = 'lnTerAge'+ IIF(lnDays >= 91,'120',IIF(lnDays >= 61 ,'90'  ,;
      IIF(lnDays >= 31 ,'60' ,IIF(lnDays >= 1 ,'30','00'))))

    &lcAges = &lcAges + lnAmount   && Accomulate Age due date variable
    *-- Computes age of a transaction based on the tran due date. [end]

    lnChgBack = IIF(Debit.TranType = '3',lnChgBack + lnAmount,lnChgBack)

  ENDSCAN   && end scan around this account in debit file.
ENDIF

*-- If you find Transactions in credit file.
IF llCredit

  *-- Summarize all unapplied Credits.
  SELECT Credit

  SCAN REST WHILE Account+TRAN+DTOS(TRANDATE) = lcAccnCode
    * Calculate equav. Base rate and unit from transaction file
    IF llMulCurr AND (CCURRCODE # oAriaEnvironment.BaseCurrency)
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
  oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir+'ARHIST',oAriaEnvironment.DataDir+'Arhistht','SH','ARHIST_X')

  *-- Summarize all ArHist.
  SELECT ARHIST
  =oAriaEnvironment.remotetableaccess.SeekRecord(lcAccnCode)
  *B608267,1 WAM 09/16/07 Use system date Aria application property instead of function date()
  *SUM (HistDate-DueDate) WHILE Account+HISTORY+TranType+TRAN+Cinstalno = lcAccnCode ;
  FOR TranType= '1' .AND. BETWEEN(HistDate,GOMONTH(DATE(),-12),DATE()) ;
  .AND. lfVChekPay() TO lnDateDiff
  SUM (HistDate-DueDate) WHILE Account+HISTORY+TranType+TRAN+CINSTALNO = lcAccnCode ;
    FOR TranType= '1' .AND. BETWEEN(HistDate,GOMONTH(oAriaEnvironment.SystemDate,-12),oAriaEnvironment.SystemDate) ;
    .AND. lfVChekPay() TO lnDateDiff
  *B608267,1 WAM 09/16/07 (End)

  *-- Close ARHIST_X file
  oAriaEnvironment.remotetableaccess.CLOSETABLE('ARHIST_X')

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


=oAriaEnvironment.remotetableaccess.REPLACERECORD("AgeDate  WITH oAriaEnvironment.SystemDate ,"+;
  "CURRENT    WITH lnAge00 ,"+;
  "Age30      WITH lnAge30                                          ,"+;
  "Age60      WITH lnAge60                                          ,"+;
  "Age90      WITH lnAge90                                          ,"+;
  "Age120     WITH lnAge120                                         ,"+;
  "TerCurrent WITH lnTerAge00                                       ,"+;
  "TerAge30   WITH lnTerAge30                                       ,"+;
  "TerAge60   WITH lnTerAge60                                       ,"+;
  "TerAge90   WITH lnTerAge90                                       ,"+;
  "TerAge120  WITH lnTerAge120                                      ,"+;
  "TotAge     WITH lnAge00 + lnAge30 + lnAge60 + lnAge90 + lnAge120 ,"+;
  "OpenCr     WITH lnOpenCr                                         ,"+;
  "ChgBack    WITH lnChgBack                                        ,"+;
  "NetBal     WITH TotAge + lnOpenCr                                ,"+;
  "nPast12Avg WITH lnPst12Avg")
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
  llCurrFile = oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.SysTemFilesPath+'SYCCURR',oAriaEnvironment.SysTemFilesPath+'Ccurrcode','SH')
ENDIF

SELECT (lcCurrAlas)
lnEqAmt = gfAmntDisp(Amount,"O",TRANDATE)

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
IF oAriaEnvironment.remotetableaccess.SeekRecord(lcAccnCode + ARHIST.HISTORY + '4','ARHIST_X')
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

=lfTableUpdate()
IF  llCustFile AND USED('CUSTOMER')
  oAriaEnvironment.remotetableaccess.CLOSETABLE('CUSTOMER')
ENDIF
IF llDebtFile AND USED('Debit')
  oAriaEnvironment.remotetableaccess.CLOSETABLE('Debit')
ENDIF
IF llCrdtFile AND USED('CREDIT')
  oAriaEnvironment.remotetableaccess.CLOSETABLE('CREDIT')
ENDIF
IF llArHsFile AND USED('ArHist')
  oAriaEnvironment.remotetableaccess.CLOSETABLE('ArHist')
ENDIF
IF llCurrFile AND USED('SYCCURR')
  oAriaEnvironment.remotetableaccess.CLOSETABLE('SYCCURR')
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
llSuccess =  oAriaEnvironment.remotetableaccess.TABLEUPDATE()
UNLOCK ALL
SELECT (lnAlias)
RETURN llSuccess
