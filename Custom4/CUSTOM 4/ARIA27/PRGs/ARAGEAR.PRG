*:***************************************************************************
*: Program file  : ARAgeAr
*: Program desc. : Account Receiviable Aging
*: For screen    : ....
*:        System : Aria Advantage Series.
*:        Module : Account Receiviable (AR)
*:     Developer : Mohamed Atia Badran (MAB)
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
*: E301077,19 MAB 01/05/1999 Open and Close files using global functions
*: E301077,19     gfOpenFile and gfCloseFile. 
*: E301246,01 SSE 06/07/1999 Added a new field nPast12Avg and to age it.
*: B603053,1 MAB 07/05/1999 Calculate equav. Base rate and unit from transaction file.
*: B802456,1 MAB 07/29/1999 Set Default Index especially for history file.
*:                          Avoiding bug when calling from Key-Off Program.
*: B606901,1 KHM 04/22/2003 Call gfSetup function to save and restore variables in case
*: B606901,1                of multi sessions.
*: B607322,1 KHM 06/23/2003 Optimize the speed of the program.
*: B607455,1 AMH 07/27/2003 Fix the bug of position is out of screen.
*: B131508,1 EIH 03/23/2006 Fix bug that when saving in reverse key screen it show custmer browser after saving
*: B131508,1 EIH 			The reason is using same name of variable 'lcAcc'
*: B130045,1 NNA 11/23/2005 Fix bug that in some cases (Special England) when you make 
*: B130045,1 NNA            Apply Debit/Credit the Age fileds and creidt avaliable become Zero
*:***************************************************************************
*
PARAMETERS lcAccount
*E301246,1 Add llArHsFile var to detect whether ARHIST File is opened or not [Begin.]
*STORE .F. TO llCustFile,llDebtFile,llCrdtFile,llCurrFile,llContinue
STORE .F. TO llCustFile,llDebtFile,llCrdtFile,llArHsFile,llCurrFile,llContinue
*E301246,1 Add llArHsFile var to detect whether ARHIST File is opened or not [End.]

*E301246,1 Add llArHsFile var to detect whether ARHIST File is opened or not [Begin.]
*STORE '' TO lcDebtOrd,lcCrdtOrd
*STORE 0  TO lnDebtRec,lnCrdtRec
STORE '' TO lcDebtOrd , lcCrdtOrd , lcArHstOrd
STORE 0  TO lnDebtRec , lnCrdtRec , lnArHstRec
*E301246,1 Add llArHsFile var to detect whether ARHIST File is opened or not [End.]

*-- Variables to deal with Multi currency [begin]
llMulCurr  = gfGetMemVar('llMulCurr') && if .T. , current company use multi currency.
lnCurrRate  = 1          && Variable to hold current rate
lnCurrUnit  = 1          && Variable to hold current unit
lcAUntSin   = '*'        && Variable to hold unit sign.
lcAExRSin   = '*'        && Variable to hold exchange rate sign.
*-- Variables to deal with Multi currency [end]

lcTempAcc = gfTempName()   && Name of file that hold temporary Account data.

*B606901,1 KHM 04/22/2003 (Begin) Adding the gfSetup function to restore save and restore variables

*B607455,1 AMH 07/27/2003 (Begin) Saving the variable.
*B131508,1 EIH 03/23/2006 Using variable 'lcAccnt' instead of using variable 'lcAcc' [Begin]
*lcAcc = lcAccount
lcAccnt = lcAccount
*B131508,1 EIH 03/23/2006 [End]
*B607455,1 AMH 07/27/2003 (End) 

*B130045,1 NNA 11/23/2005 (Begin) Saving the variable.
IF TYPE('lcCurCode')<> 'U'
  lcCurr = lcCurCode
ENDIF
*B130045,1 NNA (End)

IF !gfSetup()
  RETURN
ENDIF

*B607455,1 AMH 07/27/2003 (Begin) Restore the variable.

*B131508,1 EIH 03/23/2006 Using variable 'lcAccnt' instead of using variable 'lcAcc' [Begin]
*lcAccount = lcAcc
lcAccount = lcAccnt
*B131508,1 EIH 03/23/2006 [End]

*B607455,1 AMH 07/27/2003 (End)

*B606901,1 KHM 04/22/2003 (End)

*B130045,1 NNA 11/23/2005 (Begin) Restore the variable.
IF TYPE('lcCurr')<> 'U'
  lcCurCode = lcCurr
ENDIF
*B130045,1 NNA (End)

*-- Open Customer File. [begin]
IF !USED('Customer')
  llCustFile = gfOpenFile(gcDataDir+'Customer',gcDataDir+'Customer','SH')
ELSE
  lcCustOrd = ORDER('Customer')
  lnCustRec = RECNO('Customer')  
ENDIF
*-- Open Customer File. [end  ]

SELECT Customer
GO TOP IN Customer

*-- if calling from another program, not empty and valid account.
IF TYPE('lcAccount') = 'C' AND !EMPTY(lcAccount) AND SEEK('M'+lcAccount)
  CREATE TABLE (gcWorkDir+lcTempAcc)  (Account C(5))
  INSERT INTO (lcTempAcc)                ;
              (Account) VALUES (lcAccount)     
  llContinue = .T.
ELSE
  lcBrowFlds = [account   :H= 'Acct#' :10,]    + ;
               [btName    :H= 'Name' :35 ,]    + ;
               [Phone1    :H = 'Phone' :16,]   + ;
               [cAddress6 :H = 'Country' :30,] + ;
               [netBal  :H = 'Balance' ]       

  llContinue = gfrange(lcBrowFlds,lcTempAcc,"account",['M'])
ENDIF

*-- If you call program from menu and user press < Ok >
IF llContinue
  DO lpAging  && Call procedure that evaluate account aging.
ENDIF  

*-- Close open files [Begin]

USE IN (lcTempAcc)   && Erase cursor.

IF llCustFile
  *E301077,19 Close Files.
  *USE IN Customer
  =gfCloseFile('CUSTOMER')
ELSE
  SELECT Customer
  SET ORDER TO &lcCustOrd
  IF lnCustRec > 0 AND lnCustRec <= RECCOUNT('Customer')
    GO lnCustRec
  ENDIF
ENDIF

IF llDebtFile   
  *E301077,19 Close Files.
  *USE IN Debit
  =gfCloseFile('DEBIT')

ELSE

  *E301077,19 Restore old data if this files opened by module only.
  IF USED('DEBIT')
    SELECT Debit
    SET ORDER TO &lcDebtOrd
    IF lnDebtRec > 0 AND lnDebtRec <= RECCOUNT('Debit')
      GO lnDebtRec
    ENDIF
  ENDIF  
ENDIF

IF llCrdtFile   
  *E301077,19 Close Files.
  *USE IN Credit
  =gfCloseFile('CREDIT')

ELSE

  *E301077,19 Restore old data if this files opened by module only.
  IF USED('CREDIT')
    SELECT Credit
    SET ORDER TO &lcCrdtOrd
    IF lnCrdtRec > 0 AND lnCrdtRec <= RECCOUNT('Credit')
      GO lnCrdtRec
    ENDIF
  ENDIF  

ENDIF  

*E301246,1 handle ArHist file whether opened by gfOpenFile or by Module [Begin.]
IF llArHsFile        && if opened by gfOpenFile
  =gfCloseFile('ARHIST')

ELSE                 && Else file is not opened by gfOpenFile

  IF USED('ARHIST')  && if file is opened already select it first [Begin.]
    SELECT ArHist
    SET ORDER TO &lcArHstOrd  && restore the old order 
    IF lnArHstRec > 0 AND lnArHstRec <= RECCOUNT('ArHist')
      GO lnArHstRec           && restore old Recno
    ENDIF
  ENDIF              && if file is opened already select it first [End.]

ENDIF  
*E301246,1 handle ArHist file whether opened by gfOpenFile or by Module [End.]

IF llCurrFile
  =gfCloseFile('SYCCURR')
ENDIF
*-- Close open files [End.]
*-End of program code...

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

*E301246,1 Logical Variable called llArHist to return true if there is 
*--        tranactions for this account
*PRIVATE lcAccnCode,llDebit,llCredit
PRIVATE lcAccnCode,llDebit,llCredit,llArHist

*-- Start calculations. 
SELECT (lcTempAcc)
COUNT TO lnAllAcct FOR !DELETED()  && All Accounts in Cursor file.
lnThermNo  = 0					&& Initiall Value for thermo Fn.

*E301077,19 Open transaction files. [Beg
IF lnAllAcct > 0
  *-- Open Debit file first time call this code [Begin]
  IF !USED('Debit')
    llDebtFile = gfOpenFile(gcDataDir+'Debit',gcDataDir+'Debit','SH')
  
  ELSE
    
    IF !llDebtFile
      lcDebtOrd = ORDER('Debit')
      lnDebtRec = RECNO('Debit')  

      *B802456,1 Set Default Index.
      SET ORDER TO Debit IN Debit
      GO TOP IN Debit
    ENDIF  
    
  ENDIF
  *-- Open Debit file first time call this code [End  ]

  *-- Open credit file first time run this code [Begin]
  IF !USED('Credit')
    llCrdtFile = gfOpenFile(gcDataDir+'Credit',gcDataDir+'Credit','SH')
  
  ELSE
    
    IF !llCrdtFile 
      lcCrdtOrd = ORDER('Credit')
      lnCrdtRec = RECNO('Credit')  

      *B802456,1 Set Default Index.
      SET ORDER TO Credit IN Credit
      GO TOP IN Credit
    ENDIF
      
  ENDIF
  *-- Open credit file first time run this code [end  ]
  
  *E301246,1 open ArHist file first time by gfOpenFile or if opened save its ORDER & RECNO [Begin.]
  *-- Open ArHist file first time run this code [Begin.]
  IF !USED('ArHist')  && if ArHist file not opened , open it by gfOpenFile [Begin.]
    llArHsFile = gfOpenFile(gcDataDir+'ArHist',gcDataDir+'Arhistht','SH')
  
  ELSE                && Else ArHist file is opened then save ORDER & RECNO
    
    IF !llArHsFile    && if ArHist file is opened by Module [Begin.]
      lcArHstOrd = ORDER('ArHist')  && save current ORDER
      lnArHstRec = RECNO('ArHist')  && save current RECNO

      *B802456,1 Set Default Index.
      SET ORDER TO Arhistht IN ArHist
      GO TOP IN ArHist
    ENDIF             && if ArHist file is opened by Module [End.]
      
  ENDIF              && if ArHist file not opened , open it by gfOpenFile [End.]
  *-- Open ArHist file first time run this code [End.]
  *E301246,1 open ArHist file first time by gfOpenFile or if opened save its ORDER & RECNO [End.]

ENDIF
*E301077,19 Open transaction files. [End

*-- Scan all Accounts.
SELECT (lcTempAcc)
SCAN
  lnThermNo = lnThermNo + 1
  = gfThermo(lnAllAcct,lnThermNo,"Aging Account...",&lcTempAcc..Account)
  = SEEK('M'+&lcTempAcc..Account,'CUSTOMER')
  lcAccnCode = &lcTempAcc..Account
  DO lpCalAge   && Calculate age for current account.
ENDSCAN         && end Scan all Accounts.
*-- End calculations. 

*-- Asure that Thermometer was closed.
IF (lnThermNo < lnAllAcct) AND (lnThermNo > 0)
  FOR lnCloseThm = lnThermNo TO lnAllAcct
    =gfThermo(lnAllAcct,lnCloseThm,"Finishe Aging Account..."," ")
  ENDFOR
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
*-- Define variables that calculates data in debit and credit files. [end]

*E301246,01 Define variables that calculates data from ArHist file. [Begin.]
STORE 0.00 TO lnPst12Avg
*E301246,01 Define variables that calculates data from ArHist file. [End.]

SELECT CUSTOMER
= RLOCK()
DO lpReplCust  && Clear data in customer file, Note that: all data equal zero.

llDebit  = SEEK(lcAccnCode,'DEBIT')
llCredit = SEEK(lcAccnCode,'CREDIT')

*E301246,1 returns True if seek in ArHist File is successful [Begin.]
llArHist = SEEK(lcAccnCode,'ArHist')
*E301246,1 returns True if seek in ArHist File is successful [End.]

*-- If you find Transactions in debit file.
IF llDebit

  SELECT DEBIT
  *-- scan around this account in debit file.

  *B607322,1 KHM 06/23/2003 (Begin) Optimize the SCAN command.
  *SCAN FOR account = lcAccnCode
  SCAN REST WHILE Account+Tran+cInstalNo+DTOS(TranDate) = lcAccnCode
  *B607322,1 KHM 06/23/2003 (End)
  
    *-- Computes age of a transaction based on the tran date [begin]
    *-- lnDays : Hold Number of days after transaction.
    *-- lcAges : Hold Variable to be changed (Age transaction variable).

    *B603053,1 Calculate equav. Base rate and unit from transaction file [Begin]
    *-- if company use multi currency
    IF llMulCurr AND (CCURRCODE # gcBaseCurr)
      lnAmount = lfGetCurr()   && Fill currency variabes.
    ELSE
      lnAmount = Amount
    ENDIF
    *B603053,1 Calculate equav. Base rate and unit from transaction file [Begin]

    lnDays    = DATE() - Debit.TranDate
    
    lcAges = 'lnAge'+ IIF(lnDays >= 120,'120',IIF(lnDays >= 90 ,'90'  ,;
                      IIF(lnDays >= 60 ,'60' ,IIF(lnDays >= 30 ,'30','00')))) 
    &lcAges = &lcAges + lnAmount       && Accomulate Age variable
    *-- Computes age of a transaction based on the tran date [end]

    *-- Computes age of a transaction based on the tran due date. [begin]
    *-- The due date is computed based on the data in the code file for terms.
    *-- lnDays : Hold Number of days after transaction is due date.
    *-- lcAges : Hold Variable to be changed (Age transaction due date variable).
    lnDays = DATE() - IIF(EMPTY(Debit.DueDate),            ;
                          Debit.TranDate+30, Debit.DueDate)
    
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
  SELECT CREDIT

  *B607322,1 KHM 06/23/2003 (Begin) Optimize the SCAN command.
  *SCAN REST WHILE Account = lcAccnCode
  SCAN REST WHILE Account+Tran+DTOS(TranDate) = lcAccnCode
  *B607322,1 KHM 06/23/2003 (End)
  
    *B603053,1 Calculate equav. Base rate and unit from transaction file [Begin]
    IF llMulCurr AND (CCURRCODE # gcBaseCurr)
      lnCrAmt = lfGetCurr()
    ELSE
      lnCrAmt = Amount
    ENDIF
    lnOpenCr  = lnOpenCr + lnCrAmt
    *B603053,1 Calculate equav. Base rate and unit from transaction file [End  ]
      
  ENDSCAN

ENDIF

*E301246,1 collect Last 12 months Avg. days past due [Begin.]
IF llArHist   && If you find Transactions in ArHist file. [Begin.]
  *-- lnDateDiff for Date Difference between History Date and Due Date
  *-- lnInvoice for number of Invoices payed by Direct Payments
  STORE 0  TO lnDateDiff , lnInvoice , lnDiffer
  STORE '' TO lcHistory
  
  *-- Open ARHIST file with another alias
  USE (gcDataDir+'ARHIST') AGAIN ALIAS ARHIST_X ORDER TAG Arhistht IN 0
  
  *-- Summarize all ArHist.
  SELECT ARHIST  
  =SEEK(lcAccnCode)
  SUM (HistDate-DueDate) WHILE Account+History+TranType+Tran+Cinstalno = lcAccnCode ;
                         FOR TranType= '1' .AND. BETWEEN(HistDate,GOMONTH(DATE(),-12),DATE()) ;
                         .AND. lfVChekPay() TO lnDateDiff

  *-- Close ARHIST_X file
  USE IN ARHIST_X
  
*-- lnPst12Avg returns Average paid invoices days within one year starting from today
  IF lnInvoice <> 0
    lnPst12Avg = CEIL(lnDateDiff/lnInvoice)
  ENDIF  

ENDIF   && If you find Transactions in ArHist file. [End.]
*E301246,1 collect Last 12 months Avg. days past due [End.]


SELECT CUSTOMER
*-- if there is debit or credit or ArHist transactions, Post new customer balance.

*E301246,1 if there's transactions update the selected account
*IF llDebit OR llCredit 
IF llDebit OR llCredit or llArHist  && include also ArHist Flag in the if condition
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
*REPLACE AgeDate    WITH DATE()                                           ,;
*        Current    WITH lnAge00                                          ,;
*        Age30      WITH lnAge30                                          ,;
*        Age60      WITH lnAge60                                          ,;
*        Age90      WITH lnAge90                                          ,;
*        Age120     WITH lnAge120                                         ,;
*        TerCurrent WITH lnTerAge00                                       ,;
*        TerAge30   WITH lnTerAge30                                       ,;
*        TerAge60   WITH lnTerAge60                                       ,;
*        TerAge90   WITH lnTerAge90                                       ,;
*        TerAge120  WITH lnTerAge120                                      ,;
*        TotAge     WITH lnAge00 + lnAge30 + lnAge60 + lnAge90 + lnAge120 ,;   
*        OpenCr     WITH lnOpenCr                                         ,;
*        ChgBack    WITH lnChgBack                                        ,;
*        NetBal     WITH TotAge + lnOpenCr                                

*E301246,01 Add new field nPast12Avg to the REPLACE command [Begin.]
REPLACE AgeDate    WITH DATE()                                           ,;
        Current    WITH lnAge00                                          ,;
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
*E301246,01 Add new field nPast12Avg to the REPLACE command [End.]     

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

*E301077,19 Because currency code file used per record in debit and [Begin]
*E301077,19 credit files in scan loop, it's a good tech. to open 
*E301077,19 it first time call this function and close it in program
*E301077,19 end (Fasten the process).
IF !llCurrFile AND !USED('SYCCURR')
  llCurrFile = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
ENDIF  
*E301077,19 Because currency code file used per record in debit and [end  ]

*B603053,1 Calculate equav. Base rate and unit from transaction file [Begin]
SELECT (lcCurrAlas)
lnEqAmt = gfAmntDisp(Amount,"O",TranDate)
*B603053,1 Calculate equav. Base rate and unit from transaction file [End  ]

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
FUNCTION lfvChekPay
IF SEEK(lcAccnCode + ARHIST.History + '4','ARHIST_X')
  lnInvoice = lnInvoice + 1
  RETURN .T.
ELSE
  RETURN .F.
ENDIF
*-- End of lfvChekPay.
