*:***************************************************************************
*: Program file  : SRAgeRp
*: Program desc. : Age Commission Payables.
*: For screen    : ....
*:        System : Aria Advantage Series.
*:        Module : Sales Repesintive (SR)
*:     Developer : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Calls : 
*:     Procedures : lpAging,lpCalAge,lpReplSRep
*:     Functions  : gfrange,gfThermo
*:***************************************************************************
*: Passed Parameters  : lcRepCode
*:***************************************************************************
*: Example : DO SRAgeRp
*:***************************************************************************
*: Note that : If lcRepCode is ommited it means that calling from menu.
*:           : else if it's character it means that calling from another program. 
*:***************************************************************************
*: Modifications :
*: *E301077,20 MAB 01/05/1999 Open and Close files using global functions
*: *E301077,20     gfOpenFile and gfCloseFile. 
PARAMETERS lcRepCode
STORE .F. TO llRepFile,llCommFile,llContinue

lcCommOrd = ''
lnCommRec = 0

lcTempRep = gfTempName() && Name of file that hold temporary Sales Rep. data.

*-- Open required files. [begin]
IF !USED('SalesRep')
  llRepFile = gfOpenFile(gcDataDir+'SalesRep',gcDataDir+'SalesRep','SH')
ELSE
  lcRepOrd = ORDER('SalesRep')
  lnRepRec = RECNO('SalesRep')  
ENDIF
*-- Open required files. [end]

SELECT SalesRep

*-- if calling from another program, not empty and valid Rep. Code .
IF TYPE('lcRepCode') = 'C' AND !EMPTY(lcRepCode) AND SEEK(lcRepCode)
  CREATE TABLE (gcWorkDir+lcTempRep)  (RepCode C(3))
  INSERT INTO (lcTempRep)                ;
              (RepCode) VALUES (lcRepCode)     
  llContinue = .T.
ELSE
  lcBrowFlds = [RepCode   :H= 'Rep#' :10,]    + ;
               [Name      :H= 'Name' :35 ,]    + ;
               [Phone     :H = 'Phone' :16,]   + ;
               [cAddress6 :H = 'Country' :30,] + ;
               [Balance   :H = 'Balance' ]       

  llContinue = gfrange(lcBrowFlds,lcTempRep,"RepCode")
ENDIF

*-- If you call program from menu and user press < Ok >
IF llContinue
  DO lpAging  && Call procedure that evaluate RepCode aging..
ENDIF  

*-- Close open files [Begin]
USE IN (lcTempRep)   && Erase cursor.

IF llRepFile
  *E301077,20 Close Files.
  *USE IN SalesRep
  =gfCloseFile('SALESREP')
ELSE
  SELECT SalesRep
  SET ORDER TO &lcRepOrd
  IF lnRepRec > 0 AND lnRepRec <= RECCOUNT('SalesRep')
    GO lnRepRec
  ENDIF
ENDIF

IF llCommFile   

  *E301077,20 Close Files.
  *USE IN RepComm
  =gfCloseFile('REPCOMM')

ELSE

  *E301077,20 Restore old data if this files opened by module only.
  IF USED('REPCOMM')
    SELECT RepComm
    SET ORDER TO &lcCommOrd
    IF lnCommRec > 0 AND lnCommRec <= RECCOUNT('RepComm')
      GO lnCommRec
    ENDIF
  ENDIF  

ENDIF
*-- Close open files [End.]
*-End of program code...

*!*************************************************************
*! Name      : lpAging
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/17/98
*! Purpose   : Initiall variables used in calculate aging and loop around 
*!           : temporary cursor hold all Rep Codes.
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
PRIVATE lcCommCode
*-- Start calculations. 
SELECT (lcTempRep)
COUNT TO lnAllReps FOR !DELETED()  && All Accounts in Cursor file.
lnThermNo  = 0					&& Initiall Value for thermo Fn.

*E301077,20 Open RepComm file if want to use it only. [Begin
IF lnAllReps > 0

  IF !USED('RepComm')

    llCommFile = gfOpenFile(gcDataDir+'RepComm',gcDataDir+'RepComm','SH')

  ELSE

    IF !llCommFile
      lcCommOrd = ORDER('RepComm')
      lnCommRec = RECNO('RepComm')
    ENDIF
      
  ENDIF

ENDIF  
*E301077,20 Open RepComm file if want to use it only. [End..

*E301077,20 Select Sales Rep. Temporary file.[Beg
SELECT (lcTempRep)
*E301077,20 Select Sales Rep. Temporary file.[End
*-- Scan all RepCodes.
SCAN
  lnThermNo = lnThermNo + 1
  = gfThermo(lnAllReps,lnThermNo,"Aging Rep# ... ",&lcTempRep..RepCode)
  = SEEK(&lcTempRep..RepCode,'SalesRep')
  lcCommCode = &lcTempRep..RepCode
  DO lpCalAge   && Calculate age for current RepCode.
ENDSCAN         && end Scan all RepCodes.
*-- End calculations. 

*-- Asure that Thermometer was closed.
IF (lnThermNo < lnAllReps) AND (lnThermNo > 0)
  FOR lnCloseThm = lnThermNo TO lnAllReps
    =gfThermo(lnAllReps,lnCloseThm,"Finishe Aging ..."," ")
  ENDFOR
ENDIF
*-- End of lpAging...

*!*************************************************************
*! Name      : lpCalAge
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/17/98
*! Purpose   : Calculate AR Aging.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpReplRep
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpCalAge
*!*************************************************************
PROCEDURE lpCalAge
*-- Define variables that calculates data in RepComm file. [begin]
STORE 0.00 TO lnCredit
STORE 0.00 TO lnAge00   ,;
              lnAge30   ,;
              lnAge60   ,; 
              lnAge90   ,; 
              lnAge120  ,;
              lnBalance
*-- Define variables that calculates data in RepComm file. [end]

SELECT SalesRep
= RLOCK()
DO lpReplRep  && Clear data in SalesRep file, Note that: all data equal zero.

*-- If you find Transactions in RepComm file. [begin]
IF SEEK(lcCommCode,'RepComm')
  lnHeader = 1
  SELECT RepComm
  *-- scan around this RepCode in RepComm file.
  SCAN FOR RepCode = lcCommCode
    *-- Outer if Block [begin]
    *-- if lnHeader = 1 this means that first transaction on RepComm which
    *-- must contain the beginning balance, thus we not delete this record 
    *-- if amount of it equal zero.
    *-- else if lnHeader > 1, it means that Process remaining transactions 
    *-- on RepComm (financial history file)
    IF lnHeader >= 2 AND RepComm.Amount = 0
      BLANK
      DELETE
    ELSE 
      *-- if amount is negative, it means that credit case.
      IF RepComm.Amount < 0
        lnCredit = lnCredit + RepComm.Amount
      ELSE  && Sales Rep. have a commission, thus calculate its age.
        *-- Computes age of a transaction based on commission date [begin]
        *-- lnDays : Hold Number of days after transaction.
        *-- lcAges : Hold Variable to be changed (Age transaction variable).
        lnDays    = DATE() - RepComm.Date
        lcAges = 'lnAge'+ IIF(lnDays >= 120,'120',IIF(lnDays >= 90 ,'90'  ,;
                          IIF(lnDays >= 60 ,'60' ,IIF(lnDays >= 30 ,'30','00')))) 
    
        &lcAges = &lcAges + RepComm.Amount  && Accomulate Age variable
        *-- Computes age of a transaction based on commission date [end]
      ENDIF
    ENDIF
    *-- Outer if Block [end]
    lnHeader = lnHeader + 1    && Counter to know what record we in.
  ENDSCAN   && end scan around this RepCode in RepComm file.

  lnBalance = lnAge00 + lnAge30 + lnAge60 + lnAge90 + lnAge120
  lnCredit  = ABS(lnCredit)

  IF lnCredit >= lnBalance
    STORE (lnBalance - lnCredit) TO lnAge00,lnBalance
    STORE 0.00 TO lnCredit
    STORE 0.00 TO lnAge30   ,;
                  lnAge60   ,; 
                  lnAge90   ,; 
                  lnAge120

  ENDIF
  
  IF lnCredit != 0
    FOR lnI = 120 TO 0 STEP -30
      lcAges   = 'lnAge'+IIF(lnI = 0 , '00' , ALLTRIM(STR(lnI)))

      IF lnCredit >= &lcAges
         lnCredit  = lnCredit - &lcAges
         &lcAges   = 0.00
      ELSE
         &lcAges   = &lcAges - lnCredit
         lnCredit  = 0.00
      ENDIF

    ENDFOR
    lnBalance = lnAge00 + lnAge30 + lnAge60 + lnAge90 + lnAge120
  ENDIF

  *-- Update SalesRep File.
  SELECT SalesRep
  DO lpReplRep
ENDIF
*-- If you find Transactions in RepComm file. [end]
UNLOCK  && Unlocking SalesRep file.
*-- end of lpCalAge.

*!*************************************************************
*! Name      : lpReplRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/17/98
*! Purpose   : Transfer Aged data to SalesRep file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpReplRep
*!*************************************************************
PROCEDURE lpReplRep
REPLACE AgeDate    WITH DATE()    ,;
        Current    WITH lnAge00   ,;
        Age30      WITH lnAge30   ,;
        Age60      WITH lnAge60   ,;
        Age90      WITH lnAge90   ,;
        Age120     WITH lnAge120  ,;
        Balance    WITH lnBalance
*-- end of lpReplRep.