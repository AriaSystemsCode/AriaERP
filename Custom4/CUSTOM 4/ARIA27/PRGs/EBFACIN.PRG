*!*************************************************************
*! Name      : EBFACIN
*! Developer : Wael Ali Mohamed
*! Date      : 30/12/1999
*! Purpose   : Send Invoice and Sales Orders to Rosenthal
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcTranType
*!                       'I' : Send Invoice
*!                       'O' : Send Sales Orders
*!*************************************************************
*!B804158,1 HS 05/09/2001 Fix the format of some fields.
*!B804547,1 HIA 12/07/2001 Adjust format of some fields.
*!C102757,1 HIA 10/24/2002 Adjust format of some fields.
*!B607461,1 SSE 09/01/2003 Add Invoice Date range to the range screen.
*!B119768,1 SSE 09/16/2003 Fix bug change some of the layout.
*!*************************************************************
PARAMETER lcTranType

DECLARE laTRltFld[4,2]
laTRltFld[1,1] = 'NTERDISCR'
laTRltFld[1,2] = 'lnDiscRate'
laTRltFld[2,1] = 'EOM'
laTRltFld[2,2] = 'lcTEOM'
laTRltFld[3,1] = 'NTERDUED'
laTRltFld[3,2] = 'lnDaysDue'
laTRltFld[4,1] = 'NTERDISCD '
laTRltFld[4,2] = 'lnDiscDays'
STORE 0   TO lnDiscRate,lnDaysDue,lnDiscDays
STORE ' ' TO lcTEOM,lcBrowseTl,lcOldValue
STORE .F. TO llBrowse
STORE .T. TO llInvoice,llOrders
*B605258,1 [START] Check lcTranType value
lcTranType = IIF(EMPTY(lcTranType),'I',lcTranType)
*B605258,1 [END  ] Check lcTranType value

lcWindTitl = IIF(lcTranType='I','Invoices Transmission','Send Sales Orders Transmission')

=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
=gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDRA','SH')
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDACCT','SH')
=gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')

*-- Initialize variables
lcFactor   = SPACE(6)
lcCustomer = SPACE(5)
lcCustName = SPACE(30)
lcCustFact = SPACE(10)
lnLastTran = 2

lcKeyBmp   = gcBmpHome + "ExtKey.BMP"
lcSelBmp   = gcBmpHome + "SEL.BMP"
lcProceed  = gcBmpHome + "proceed.bmp"
lcClose    = gcBmpHome + "Close2.bmp"

STORE '' TO lcClientNo
STORE  0 TO lnAssignNo

IF lcTranType = 'O'
  =gfOpenFile(gcDataDir+'CitTrnLn',gcDataDir+'CitTrnLn','SH')
  *-- Create CIT Send Orders Temp. File
  lcTmpCit = gfTempName()
  CREATE TABLE (gcWorkDir+lcTmpCit) (cFacCode C(6),dDate D,Account c(5),Order C(6))
ENDIF

*-- Create transaction temp. file
lcTempTran = gfTempName()
CREATE TABLE (gcWorkDir+lcTempTran) ;
	(Type C(1), Account C(5), TranNum C(6), cSelect C(1))
INDEX ON CsELECT+Type+Account+TranNum TAG 'SELECT'
INDEX ON Type+Account+TranNum TAG (lcTempTran)  ADDITIVE
*B605258,1 TMI [Start] Add RangeSelct index
INDEX ON Type + TranNum TAG RangeSelct
*B605258,1 TMI [End  ] Add RangeSelct index
SET RELATION TO 'M'+Account INTO CUSTOMER
IF lcTranType = 'I'
  SET RELATION TO Account+TranNum INTO INVHDR ADDITIVE
ELSE
  SET RELATION TO Account+'O'+TranNum INTO ORDHDR ADDITIVE
ENDIF  
*-- Number of selected transactions
STORE 0 TO lnNoTrans

*----- To get the month for the name of the output file
lcMonth = SUBSTR('ABCDEFGHIJKL',MONTH(gdSysDate),1)

*C102494 Hassan 10/29/2001 [Begin]
*lcOutFile = gcDataDir + 'CIT'+PADL(MONTH(gdSysDate),2,'0')+PADL(DAY(gdSysDate),2,'0')+'.NEW'
lcOutFile = " "
IF FILE(gcDataDir+"MEMO.MEM")
  RESTORE FROM gcDataDir+"MEMO" ADDITIVE
ENDIF  
*C102494 Hassan 10/29/2001 [End]

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*DO (gcScrDir+gcWinAppl+"\EBFACIN.SPX") WITH 'I'
DO (gcScrDir + IIF(RIGHT(gcScrDir , 1) = "\" , "" , "\") + "EBFACIN.SPX") WITH 'I'

IF lcTranType = 'O'
  SELECT (lcTmpCit)
  SCAN
    SCATTER MEMVAR
    IF !SEEK(m.cFacCode+m.Order,'CitTrnLn')
      INSER INTO CitTrnLn (cFacCode,Order) VALUES (m.cFacCode,m.Order)
    ENDIF
    SELECT CitTrnLn
    GATHER MEMVAR
  ENDSCAN
  USE IN (lcTmpCit)
  ERASE (gcWorkDir+lcTmpCit+'.*')
ENDIF

*!*************************************************************
*! Name      : lfvFactor
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
*! Purpose   : Validate Factors
*!*************************************************************
*! Calls     : ARIABROW,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfvFactor()
*!*************************************************************
FUNCTION lfvFactor
PRIVATE lnAlias

lnAlias = SELECT()
IF llBrowse OR (!EMPTY(lcFactor) .AND. !SEEK(lcFactor,'SycFact'))
  lcBrFields = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  SELECT SycFact
  lcFactor = IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,gnBrFSRow2, gnBrFSCol2,'','',;
                          'cFacCode','laBrowArr'),SycFact.cFacCode,SPACE(6))
ENDIF
IF lcOldValue <> lcFactor
  STORE .T. TO llInvoice,llOrders
ENDIF
IF EMPTY(lcFactor)
  SHOW GET pbTransact DISABLE
ELSE
  SHOW GET pbTransact ENABLE
ENDIF
=lfRefresh('EBFACIN')
SELECT (lnAlias)
llBrowse = .F.

*!*************************************************************
*! Name      : lfvCustomer
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
*! Purpose   : Validate Customer
*!*************************************************************
*! Calls     : CUSBROWM,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfvCustomer()
*!*************************************************************
FUNCTION lfvCustomer
PRIVATE xAccount

IF llBrowse .OR. (!EMPTY(lcCustomer) .AND. !SEEK('M'+lcCustomer,'CUSTOMER')) 
  xAccount = lcCustomer
  SELECT CUSTOMER
  DO CUSBROWM WITH xAccount
  lcCustomer = xAccount
ENDIF
IF lcOldValue <> lcCustomer
  STORE .T. TO llInvoice,llOrders
ENDIF  
lcCustFact = IIF(EMPTY(lcCustomer),'',IIF(EMPTY(Customer.FactAcct),PADL(lcCustomer,9,'0'),LEFT(Customer.FactAcct,9)))
lcCustName = IIF(EMPTY(lcCustomer),'',Customer.BtName)
=lfRefresh('EBFACIN')
llBrowse = .F.

*!*************************************************************
*! Name      : lfvTrans
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
*! Purpose   : Select Transactions
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfvTrans()
*!*************************************************************
FUNCTION lfvTrans

IF llInvoice AND lcTranType = 'I'
  SELECT (lcTempTran)
  =SEEK('I')
  DELETE REST WHILE Type = 'I'
  SELECT INVHDR
  =SEEK(ALLTRIM(lcCustomer))
  SCAN REST WHILE Account+Invoice = ALLTRIM(lcCustomer) FOR cFacCode = lcFactor AND Status <>'V'
    INSERT INTO (lcTempTran) (Type,Account,TranNum) VALUES ('I',INVHDR.Account,INVHDR.Invoice)
  ENDSCAN
  llInvoice = .F.
ENDIF
IF llOrders AND lcTranType = 'O'
  SELECT (lcTempTran)
  =SEEK('O')
  DELETE REST WHILE Type = 'O'
  SELECT ORDHDR
  IF EMPTY(lcCustomer)
    SET ORDER TO TAG ORDHDR
    =SEEK('O')
    SCAN REST WHILE cOrdType+Order = 'O' FOR cFacCode = lcFactor AND Status <>'X'
      INSERT INTO (lcTempTran) (Type,Account,TranNum) VALUES ('O',ORDHDR.Account,ORDHDR.Order)
    ENDSCAN
  ELSE
    =SEEK(lcCustomer+"O")
    SCAN REST WHILE Account+cOrdType+Order = lcCustomer+"O" FOR cFacCode = lcFactor AND Status <>'X'
      INSERT INTO (lcTempTran) (Type,Account,TranNum) VALUES ('O',ORDHDR.Account,ORDHDR.Order)
    ENDSCAN
  ENDIF
  SET ORDER TO TAG ORDACCT
  llOrders = .F.
ENDIF
lcTranBrow = IIF(lcTranType='O','Select Sales','Select Invoices')
lnTrans    = 0
lcTranMode = IIF(lcTranType='O','O','I')
*DO (gcScrDir+gcWinAppl+"\EBSNDTR.SPX") WITH IIF(lcTranType='O','O','I')
DO (gcScrDir+IIF(RIGHT(gcScrDir , 1) = "\" , "" , "\")+"EBSNDTR.SPX") WITH IIF(lcTranType='O','O','I')
SET ORDER TO TAG 'SELECT' IN (lcTempTran)
IF SEEK('»',lcTempTran)
  SHOW GET pbProceed ENABLE
ELSE
  SHOW GET pbProceed DISABLE
ENDIF
SET ORDER TO TAG (lcTempTran) IN (lcTempTran)

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
*! Purpose   : Call functions
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcType :
*!             'S' : Select/UnSelect
*!             'A' : Select All
*!             'V' : Invert
*!             'N' : Select None
*!             'O' : Switch
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvSelect('S')
*!*************************************************************
FUNCTION lfvSelect
PARAMETERS lcType
PRIVATE lcKey

SELECT (lcTempTran)

*B607461,1 SSE 09/01/2003 Get the correct key. [Begin]
*lcKey = Type+Account+TranNum
lcKey = Type+TranNum
*B607461,1 SSE 09/01/2003 Get the correct key. [End]

DO CASE
  CASE lcType = 'S'
    REPLACE cSelect WITH IIF(cSelect='»',' ','»')
  CASE lcType = 'A'
    =SEEK(lcTranMode)
    REPLACE REST cSelect WITH '»' WHILE Type+Account+TranNum = lcTranMode
  CASE lcType = 'N'
    =SEEK(lcTranMode)
    REPLACE REST cSelect WITH ' ' WHILE Type+Account+TranNum = lcTranMode
  CASE lcType = 'V'
    =SEEK(lcTranMode)
    REPLACE REST cSelect WITH IIF(cSelect='»',' ','»') WHILE Type+Account+TranNum = lcTranMode
ENDCASE
=SEEK(lcKey)
=lfwBrowTrn()

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
*! Purpose   : Screen Activate function
*!*************************************************************
*! Calls     : lfClearKey,gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*!*************************************************************
*! Name      : lfDeAct
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
*! Purpose   : Screen Deactivate function
*!*************************************************************
*! Calls     : lpAction, lpTab, lpBackTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDeAct()
*!*************************************************************
FUNCTION lfDeAct

IF WONTOP()=lcTranBrow
  ON KEY LABEL ALT+S DO lpAction WITH 'S'
  ON KEY LABEL ALT+A DO lpAction WITH 'A'
  ON KEY LABEL ALT+N DO lpAction WITH 'N'
  ON KEY LABEL ALT+V DO lpAction WITH 'V'

  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lpTab     WITH 'EBSNDTR2','pbSelect'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'EBSNDTR2','pbClose'
ELSE
  glFromBrow = .F.
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lpAction
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
*! Purpose   : Call functions
*!*************************************************************
*! Calls     : lfvSwitch,lfvSelect
*!*************************************************************
*! Parameters: lcType :
*!             'S' : Select/UnSelect
*!             'A' : Select All
*!             'V' : Invert
*!             'N' : Select None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpAction WITH 'S'
*!*************************************************************
PROCEDURE lpAction
PARAMETERS lcType
=lfvSelect(lcType)

*!*************************************************************
*! Name      : lpTab
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTab WITH 'EBSNDTR2','pbSelect'
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName

ON KEY LABEL TAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lpBackTab
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpBackTab WITH 'EBSNDTR2','pbSelect'
*!*************************************************************
PROCEDURE lpBackTab
PARAMETERS lcWindName, lcObjName

ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lfClearKey
*! Developer : WAM
*! Date      : 12/30/1999
*! Purpose   : Clear key
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearKey()
*!*************************************************************
FUNCTION lfClearKey

ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+S 
ON KEY LABEL ALT+A 
ON KEY LABEL ALT+N 
ON KEY LABEL ALT+V 
ON KEY LABEL ALT+O
ON KEY LABEL ALT+B

*!*************************************************************
*! Name      : lfBrowTran
*! Developer : WAM
*! Date      : 12/30/1999
*! Purpose   : Browse invoices and Sales Orders
*!*************************************************************
*! Calls     : lfwBrowTrn
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBrowTran()
*!*************************************************************
FUNCTION lfBrowTran

lcSelect = SELECT()
SELECT (lcTempTran)
lnTrans = RECNO()
lcFields = "cMarker=IIF(RECNO()=lnTrans,'>',' '):H=' ':R:1:W=.F.,cSelect :H=' ' :R,"
DO CASE
  CASE lcTranMode = 'I'
    lcFields = lcFields + "InvHdr.Invoice :R,InvHdr.InvDate :R :H='Date',InvHdr.Account :R,InvHdr.Store :R,InvHdr.Order :R,;
    cFactNo=IIF(EMPTY(Customer.FactAcct),Customer.Account,Customer.FactAcct) :R :H='Fact. Accnt.',InvHdr.Ship :R,InvHdr.TotalChg :R :H='Total Chg.'"
    lcTranBrow = 'Select Invoices'
    =SEEK('I')
  CASE lcTranMode = 'O'
    lcFields = lcFields + "OrdHdr.Order  :R,lcSes=gfCodDes(OrdHdr.Season,'SEASON') :R:H='Season' :P='XXXXXXXXXXXXXXXXXXXX',lcDiv=gfCodDes(OrdHdr.cDivision,'CDIVISION') :R:H='Division' :P='XXXXXXXXXXXXXXXXXXXX',"
    lcFields = lcFields + "OrdHdr.Status :R :H='ST',OrdHdr.Account :R ,cAccName=SUBSTR(Customer.StName,1,20) :R:H='Name',cStore=IIF(OrdHdr.Multi='Y','*MULTI*',OrdHdr.Store) :R :H='Store',"
    lcFields = lcFields + "OrdHdr.Start  :R,OrdHdr.Complete :R,OrdHdr.OpenAmt :R:P='9999999.99':H='Open Amnt.',OrdHdr.ApprAmt :R:P='9999999.99':H='App.Amnt.'"
    lcTranBrow = 'Select Sales Orders'
    =SEEK('O')
ENDCASE
BROWSE FIELDS &lcFields  ;
       WINDOW EBSNDTR1   ;
       IN WINDOW EBSNDTR ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       KEY lcTranMode    ;
       WHEN lfwBrowTrn() ;
       TITLE lcTranBrow
SELECT (lcSelect)

*!*************************************************************
*! Name      : lfwBrowTrn
*! Developer : WAM
*! Date      : 12/30/1999
*! Purpose   : Show invoices/Credit Memos and Sales Orders
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwBrowTrn()
*!*************************************************************
FUNCTION lfwBrowTrn

SELECT (lcTempTran)
lnTrans = RECNO()
SHOW WINDOW (lcTranBrow) REFRESH SAME
IF EMPTY(cSelect)
  SHOW GET pbSelect,1 ENABLE PROMPT  '\<Select'
ELSE
  SHOW GET pbSelect,1 ENABLE PROMPT  'Un\<Select'
ENDIF

*!*************************************************************
*! Name      : lfvProceed
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
*! Purpose   : Validate Proceed
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfvProceed()
*!*************************************************************
FUNCTION lfvProceed

*-- Client Information
lcClientNo = ALLTRIM(SycFact.cClientNo)
lnAssignNo = SycFact.AssignNo

IF EMPTY(lcClientNo)
  =gfModalGen('TRM00000B00000','ALERT','','','Factor information not complete. Cannot proceed.')
  RETURN
ENDIF

*B119768,1 SSE 09/28/2003 Commented out. [Begin]
*IF LEN(lcClientNo) <> 4 
*  =gfModalGen('TRM00000B00000','ALERT','','','Invalid Client Number. Cannot proceed.')
*  RETURN
*ENDIF
*B119768,1 SSE 09/28/2003 Commented out. [End]

*---- Name of output file
*C102494 Hassan 10/29/2001 [Begin]
*!*	lcOutFile = lcTranType + lcClientNo + lcMonth + PADL(DAY(gdSysDate),2,'0')+'.'
*!*	IF ADIR(laFiles,lcOutFile+'*') = 0
*!*	  lcOutFile = lcOutFile+'001'
*!*	ELSE
*!*	  lnDecimal = SET('DECIMAL')
*!*	  SET DECIMAL TO 0
*!*	  lcOutFile=lcOutFile+;
*!*	  PADL(VAL(SUBSTR(laFiles[ALEN(laFiles,1),1],AT('.',laFiles[ALEN(laFiles,1),1])+1,3))+1,3,'0')
*!*	  SET DECIMAL TO lnDecimal
*!*	ENDIF  
*C102494 Hassan 10/29/2001 [End]
*-- Check if the output file allready exists.

IF FILE(lcOutFile)
  IF gfModalGen('QRM00000B00006','ALERT','','','Output file '+lcOutFile+' already exist. Overwrite it?') = 2
    RETURN
  ENDIF
ENDIF

*-- Open the output file
lnOutFile = FCREATE(lcOutFile,0)
IF lnOutFile < 0
  =gfModalGen('TRM00000B00000','ALERT','','','Cannot open output file. Cannot proceed.')
  RETURN
ENDIF

=IIF(lcTranType='O',lfWrtOrder(),lfWrtInv())
*-- Increament the Assignment number in customer file
SELECT SYCFACT
= RLOCK()
REPLACE AssignNo WITH IIF(lnLastTran=1,0,lnAssignNo)
UNLOCK
=FCLOSE(lnOutFile)
CLEAR READ
*E300817,1 Message : 00370
*E300817,1 Output file has been created
*E300817,1 Button : 00000
*E300817,1 Ok
=gfModalGen('TRM00000B00000','ALERT','','','Output file '+lcOutFile+' has been created.')


*!*************************************************************
*! Name : lfWrtInv
*! Auth : Wael Aly MOhamed
*! Date : 12/30/99
*!**************************************************************
*! Synopsis : Write an output Invoices text file to be send to Rosenthal.
*!*************************************************************
*! Called from : 
*!         Procedures : EBFACIN
*!*************************************************************
*! Calls : 
*!         FUNCTIONS : lfAddInvRec
*!                     lfAddSubRec
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfWrtInv
PRIVATE lcCustomer

WAIT 'Creating outbound Invoice file...' WINDOW NOWAIT
*-- Initialize Invoice records and Invocie Dollars
STORE 0 TO lnInvRec,lnInvAmt

SELECT (lcTempTran)
SET ORDER TO TAG SELECT

*-- First write Invoice records
IF SEEK('»I')
  DO WHILE cSelect+Type+Account+TranNum = "»I"
    lcCustomer = Account
    =SEEK('M'+lcCustomer,'Customer') 
    *B804158,1 HS 05/09/2001 Fix the customer code format [Begin]
    *lcCustNum = IIF(EMPTY(Customer.FactAcct),PADL(lcCustomer,9,'0'),LEFT(Customer.FactAcct,9) )
    lcCustNum = IIF(EMPTY(Customer.FactAcct),PADR(lcCustomer,9),LEFT(Customer.FactAcct,9) )
    *B804158,1 HS 05/09/2001 Fix the customer code format [End]
    SCAN REST WHILE cSelect+Type+Account+TranNum = "»I"+lcCustomer
      *-- Add Invoice Record Type "D"
      =lfAddInvRec()
      *-- Increament number of invoice records and Invoice Dollars
      lnInvRec = lnInvRec + 1
      lnInvAmt = lnInvAmt + InvHdr.TotalChg
    ENDSCAN
  ENDDO
ENDIF  
*-- Add Sub Total Record Type "S"
=lfAddSubRec()

*!*************************************************************
*! Name : lfAddInvRec
*! Auth : Wael Aly MOhamed
*! Date : 12/30/99
*!*************************************************************
*! Synopsis : Write an invoice record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : EBFACIN
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddInvRec
PRIVATE lcSegLine, lcTmpDate, lcInvDate, lcFTermRate, lcFTermDays,lcSTermDays

*-- Terms Information
*B119768,1 SSE 09/16/2003 No discount it should be filled with Zeros, No Terms day it must be 10 days. [Begin]
*lcFTermRate = SPACE(2)
*lcFTermDays = SPACE(3)
lcFTermRate = '00'
lcFTermDays = '010'
*B119768,1 SSE 09/16/2003 No discount it should be filled with Zeros, No Terms day it must be 10 days. [End]

lcSTermDays = SPACE(3)
lcExtraDays = SPACE(3)
lcTypeTermDays = 'N'

=gfRltFld(InvHdr.cTermCode,@laTRltFld,'CTERMCODE')

IF lnDiscRate <> 0
  *-- First terms rate
  lcFTermRate = PADL(INT(lnDiscRate),2,'0')
ENDIF  
IF lnDiscDays <> 0
  *-- First Terms days
  lcFTermDays = PADL(INT(lnDiscDays),3,'0')
  IF lnDaysDue <> 0
    lcSTermDays = PADL(INT(lnDaysDue),3,'0')
  ENDIF
ELSE
  IF lnDaysDue <> 0
    lcFTermDays = PADL(INT(lnDaysDue),3,'0')
  ENDIF    
ENDIF    
IF lcTEOM='Y'
  lcTypeTermDays = 'E'
ENDIF

lcSegLine = ''
*--1 Type
lcSegLine = lcSegLine + 'A1'
*--2 Client Number

*B119768,1 SSE 09/16/2003 Make it left justified and space filled to the right. [Begin]
*lcSegLine = lcSegLine + PADL(lcClientNo,13,'0')
lcSegLine = lcSegLine + PADR(lcClientNo,13,SPACE(1))
*B119768,1 SSE 09/16/2003 Make it left justified and space filled to the right. [End]

*--3 Filer
lcSegLine = lcSegLine + SPACE(1)
*--4 Customer Number

*B119768,1 SSE 09/16/2003 Make it left justified and space filled to the right. [Begin]
*lcSegLine = lcSegLine + PADL(lcCustNum,10,'0')
lcSegLine = lcSegLine + PADR(lcCustNum,10,SPACE(1))
*B119768,1 SSE 09/16/2003 Make it left justified and space filled to the right. [End]

*--5 Invoice Number
lcInvNumb = &lcTempTran..TranNum
lcInvNumb = ALLTRIM(lcInvNumb)
lcSegLine = lcSegLine + PADL(lcInvNumb , 6,'0')
*--6 Invoice Amount
lcSegLine = lcSegLine + PADL(ALLTRIM(STR(InvHdr.TotalChg*100,10)),10,'0')
*--7 Invoice Date
lcInvDate = PADL(MONTH(InvHdr.InvDate),2,'0') + PADL(DAY(InvHdr.InvDate),2,'0')+RIGHT(STR(YEAR(InvHdr.InvDate),4),2)
lcSegLine = lcSegLine + lcInvDate

*--8 As Of Date

*B119768,1 SSE 09/16/2003 Make it Zero filled. [Begin]
*lcSegLine = lcSegLine + SPACE(6)
lcSegLine = lcSegLine + '000000'
*B119768,1 SSE 09/16/2003 Make it Zero filled. [End]


*--9 Order#
*B119768,1 SSE 09/16/2003 Make it left justified and space filled to the right. [Begin]
*lcSegLine = lcSegLine + PADL(ALLTRIM(InvHdr.Order),15,'0')
lcSegLine = lcSegLine + PADR(ALLTRIM(InvHdr.Order),15,SPACE(1))
*B119768,1 SSE 09/16/2003 Make it left justified and space filled to the right. [End]

*--10 Store Number
lcInvStore = ALLTRIM(InvHdr.Store)
lcSegLine = lcSegLine + PADL(lcInvStore , 4)

*--11 Filer
lcSegLine = lcSegLine + SPACE(2)

*--12 Approval #

*B119768,1 SSE 09/16/2003 Make it Zero filled. [Begin]
*lcSegLine = lcSegLine + PADL(ALLTRIM(SUBSTR(OrdHdr.Approval,1,6)),6,'0')
lcSegLine = lcSegLine + PADL(ALLTRIM(SUBSTR(InvHdr.Approval,1,6)),6,'0')
*B119768,1 SSE 09/16/2003 Make it Zero filled. [End]

*--13 Type Of Trems
lcSegLine = lcSegLine + lcTypeTermDays 
*--14 Discount Percent

lcSegLine = lcSegLine + lcFTermRate

*--15 Term Number Of Days
*B119768,1 SSE 09/16/2003 Add Number of Days. [Begin]
*lcSegLine = lcSegLine + SPACE(3)
lcSegLine = lcSegLine + lcFTermDays
*B119768,1 SSE 09/16/2003 Add Number of Days. [End]

*--16 Purchase Order
lcSegLine = lcSegLine + PADL(InvHdr.Custpo , 15)
*--17 Free form terms
lcSegLine = lcSegLine + SPACE(2)
*--18 Free form terms
lcSegLine = lcSegLine + iif(empty(alltrim(invhdr.cfaccode)),'A',SPACE(1))
*--19 Free form terms
lcSegLine = lcSegLine + SPACE(5)

= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name : lfAddSubRec
*! Auth : Wael Aly MOhamed
*! Date : 12/30/99
*!*************************************************************
*! Synopsis : Write the subtotal record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : lfWrtInv
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddSubRec
PRIVATE lcSegLine, lcInvAmt, lcRetAmt, lcTotAmt

*-- Increament the assignment number
lnAssignNo = lnAssignNo + 1

lcSegLine = ''
*--1 Filler
lcSegLine = lcSegLine + SPACE(2)
*--2 Client Number

lcSegLine = lcSegLine + 'EU'

*B119768,1 SSE 09/16/2003 Add the word 'TOTALS'. [Begin]
lcSegLine = lcSegLine + 'TOTALS'
*B119768,1 SSE 09/16/2003 Add the word 'TOTALS'. [End]

*--3 Totals
*B119768,1 SSE 09/16/2003 Commented out. [Begin]
*lcSegLine = lcSegLine + SPACE(6)
*B119768,1 SSE 09/16/2003 Commented out. [End]

*--4 Invocie Amount
lcSegLine = lcSegLine + PADL(ALLTRIM(STR(lnInvAmt*100,9)),10,'0')
*--5 Filler
lcSegLine = lcSegLine + SPACE(1)

*B119768,1 SSE 09/16/2003 Add extra space for position 22,23. [Begin]
lcSegLine = lcSegLine + SPACE(2)
*B119768,1 SSE 09/16/2003 Add extra space for position 22,23. [End]

*--6 Invoice Records
lcSegLine = lcSegLine + PADL(lnInvRec,4,'0')
*--7 Filler
lcSegLine = lcSegLine + SPACE(1)
*--8 Assignment Number - Numeric.
lcSegLine = lcSegLine + PADL(lnAssignNo,4,'0')
= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name      : lfWrtOrder
*! Developer : WAM
*! Date      : 12/30/1999
*! Purpose   : Write outbound Order
*!*************************************************************
*! Calls     : lfAddOrdRec(),lfAddOrdSub()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBrowTran()
*!*************************************************************
FUNCTION lfWrtOrder
PRIVATE lcCustomer

WAIT 'Creating outbound Orders file...' WINDOW NOWAIT

*-- Initialize subtotal number of Name/Address records, Order records
STORE 0 TO lnSOrdRec, lnSOrdAmt

SELECT (lcTempTran)
SET ORDER TO TAG SELECT
=SEEK('»O')
DO WHILE cSelect+Type+Account+TranNum = "»O"
  lcCustomer = Account
  *-- Get customer factor number
  =SEEK('M'+lcCustomer,'Customer') 
  lcCustNum = IIF(EMPTY(Customer.FactAcct),PADL(lcCustomer,9,'0'),LEFT(Customer.FactAcct,9) )
  *-- Add Order Record for this customer Type "D"    
  SCAN WHILE cSelect+Type+Account+TranNum = "»O"+lcCustomer
    =lfAddOrdRec()
    
    *-- Update the CitTrnLn Temp. File
    INSERT INTO (lcTmpCit) (cFacCode,dDate,Account,Order) VALUES ;
    (lcFactor,gdSysDate,lcCustomer,OrdHdr.Order)

    *-- Increament number of order records and Order amount
    lnSOrdRec = lnSOrdRec + 1
    lnSOrdAmt = lnSOrdAmt + ROUND(OrdHdr.BookAmt,0)
  ENDSCAN
ENDDO
*-- Add Order Trailer Record
=lfAddOrdSub()
 
WAIT CLEAR

*!*************************************************************
*! Name : lfAddOrdRec
*! Auth : Wael Aly MOhamed
*! Date : 12/30/1999
*!*************************************************************
*! Synopsis : Write the order record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : lfWrtOrder
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddOrdRec
PRIVATE lcSegLine, lcTmpDate, lcOrdDate, lcFTermDays

*-- Terms Information
lcFTermDays = '000'
lcExtraDays = SPACE(3)
=gfRltFld(OrdHdr.cTermCode,@laTRltFld,'CTERMCODE')
IF lcTEOM='Y'
  lcFTermDays = PADL(INT(lnDaysDue),2,'0')+'E'
ELSE  
  lcFTermDays = PADL(INT(lnDaysDue),3,'0')
ENDIF    

lcSegLine = ''
*01-- Client Number

*B119768,1 SSE 09/16/2003 Make it left justified and space filled to the right. [Begin]
*lcSegLine = lcSegLine + PADL(lcClientNo,13,'0')
lcSegLine = lcSegLine + PADR(lcClientNo,13,SPACE(1))
*B119768,1 SSE 09/16/2003 Make it left justified and space filled to the right. [End]

*02-- Customer Number

*B119768,1 SSE 09/16/2003 Make it left justified and space filled to the right. [Begin]
*lcSegLine = lcSegLine + PADL(lcCustNum,10,'0')
lcSegLine = lcSegLine + PADR(lcCustNum,10,SPACE(1))
*B119768,1 SSE 09/16/2003 Make it left justified and space filled to the right. [End]

*03-- Order #.
*B119768,1 SSE 09/16/2003 Make it left justified and space filled to the right. [Begin]
*lcSegLine = lcSegLine + PADL(ALLTRIM(OrdHdr.Order),15,'0')
lcSegLine = lcSegLine + PADR(ALLTRIM(OrdHdr.Order),15,SPACE(1))
*B119768,1 SSE 09/16/2003 Make it left justified and space filled to the right. [End]

*04-- Order Completion Date
lcOrdDate = RIGHT(STR(YEAR(OrdHdr.Complete),4),2)+PADL(MONTH(OrdHdr.Complete),2,'0')+PADL(DAY(OrdHdr.Complete),2,'0')
lcSegLine = lcSegLine + lcOrdDate
*05-- As Of Date

*B119768,1 SSE 09/16/2003 Make it Zero filled. [Begin]
*lcSegLine = lcSegLine + SPACE(6)
lcSegLine = lcSegLine + '000000'
*B119768,1 SSE 09/16/2003 Make it Zero filled. [End]

*06-- TERMS (DAYS)
lcSegLine = lcSegLine + SPACE(3)
*07-- Terms (EOF)
lcSegLine = lcSegLine + 'E'
*08-- Order Amount
lcSegLine = lcSegLine + PADL(ROUND(OrdHdr.BookAmt,0),11,'0')
*09-- Customer Name
lcSegLine = lcSegLine + PADL(ALLTRIM(Customer.stName),35,' ')
*10-- Customer ADDR
lcSegLine = lcSegLine + PADL(ALLTRIM(Customer.caddress2),30,' ')
*11-- Customer City
lcSegLine = lcSegLine + PADL(ALLTRIM(Customer.caddress3),20,' ')
*12-- Customer State
lcSegLine = lcSegLine + PADL(ALLTRIM(Customer.caddress4),2,' ')
*13-- Customer Zip
lcSegLine = lcSegLine + PADL(ALLTRIM(Customer.caddress5),10,' ')
*14-- Filer
lcSegLine = lcSegLine + SPACE(8)
= FPUTS(lnOutFile,lcSegLine)
*!*************************************************************
*! Name : lfAddOrdSub
*! Auth : Wael Aly MOhamed
*! Date : 12/30/1999
*!*************************************************************
*! Synopsis : Write the subtotal record in the output orders text file
*!* ************************************************************
*! Called from : 
*!         Procedures : lfWrtOrder
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddOrdSub
PRIVATE lcSegLine

lcSegLine = ''
*01-- Type
lcSegLine = lcSegLine + '9'
*02-- Client Number
lcSegLine = lcSegLine + PADL(lcClientNo,5,'0')
*03-- Order records
lcSegLine = lcSegLine + PADL(lnSOrdRec,5,'0')
*04-- Total Order amount
lcSegLine = lcSegLine + PADL(lnSOrdAmt,10,'0')
*05-- Invoice records
lcSegLine = lcSegLine + SPACE(5)
*06-- Total Invoice Dollars
lcSegLine = lcSegLine + SPACE(10)
*07-- Filer
lcSegLine = lcSegLine + SPACE(59)
= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name : lfGetFile
*! Auth : Hassan Ibrahim ALi
*! Date : 10/29/2001
*! Ref  : C102494,1
*!*************************************************************
*! Synopsis : Give ability to user to choose name and path of;
*!            the output file.
*!*************************************************************
*! Called from : None.
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfGetFile
PRIVATE ltOutFile , lcTalk

IF FILE(gcDataDir+"MEMO.MEM")
  RESTORE FROM gcDataDir+"MEMO" ADDITIVE
ELSE
  lcOutFile = ""
ENDIF  

ltOutFile = PUTFILE('' , lcOutFile , "NEW" )

IF EMPTY(ltOutFile) = .F.
  lcOutFile = ltOutFile 
  lcTalk = SET('TALK')
  SET TALK OFF
  SAVE TO gcDataDir + "MEMO" ALL LIKE lcOutFile
  SET TALK &lcTalk.
  =lfRefresh('EBFACIN')
ENDIF  
*!*************************************************************


*B605258,1 TMI [Start ] Range function
*!*************************************************************
*! Name      : lfvRange
*! Developer : Haytham El-Sheltawi
*! Copied here by : Tarek Mohamed Ibrahim
*! Date      : 8/23/2001
*! Purpose   : Valid function for push button "Range", the
*!             select range button of the select transactions
*!             screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvRange()
*!*************************************************************
*B604858,1 HS 
*!*************************************************************
FUNCTION lfvRange

PRIVATE lcRangeFr , lcRangeTo , lcScope , lcKey , lcOldOrder , lcOldNear
STORE "" TO lcRangeFr , lcRangeTo , lcScope

*B607461,1 SSE 09/01/2003 Define From and To date for Invoice Date. [Begin]
*DO (gcScrDir + "AR\Ebarange.SPX")
PRIVATE ldRangeFr , ldRangeTo
STORE {} TO ldRangeFr , ldRangeTo
DO (gcScrDir + IIF(RIGHT(gcScrDir , 1) = "\" , "" , "\") + "Ebarange.SPX")
*B607461,1 SSE 09/01/2003 Define From and To date for Invoice Date. [End]

IF !EMPTY(lcScope)
  SELECT (lcTempTran)
  *-- Save file environment
  lcOldNear  = SET("NEAR")
  lcOldOrder = ORDER()
  lcKey      = Type + TranNum
  
  SET ORDER TO RangeSelct
  SET NEAR ON
  
  *B607461,1 SSE 09/01/2003 Remove extra spaces from seek. [Begin]
  *SEEK lcTranMode + lcRangeFr
  SEEK lcTranMode + ALLTRIM(lcRangeFr)
  *B607461,1 SSE 09/01/2003 Remove extra spaces from seek. [End]
  
  DO CASE
    CASE lcScope = 'A'
      *B607461,1 SSE 09/01/2003 Add the date range condition. [Begin]
      *REPLACE REST cSelect WITH '»' WHILE Type + TranNum <= lcTranMode + lcRangeTo
      IF !EMPTY(ldRangeFr) OR !EMPTY(ldRangeTo)
        IF lcTranMode  = 'O'
          REPLACE REST cSelect WITH '»' WHILE Type + TranNum <= lcTranMode + lcRangeTo ;
                  FOR BETWEEN(OrdHdr.Complete,ldRangeFr,ldRangeTo)
        ELSE
          REPLACE REST cSelect WITH '»' WHILE Type + TranNum <= lcTranMode + lcRangeTo ;
                  FOR BETWEEN(InvHdr.InvDate,ldRangeFr,ldRangeTo)
        ENDIF          
      ELSE
        REPLACE REST cSelect WITH '»' WHILE Type + TranNum <= lcTranMode + lcRangeTo
      ENDIF  
      *B607461,1 SSE 09/01/2003 Add the date range condition. [End]
    CASE lcScope = 'N'      
      *B607461,1 SSE 09/01/2003 Add the date range condition. [Begin]
      *REPLACE REST cSelect WITH ' ' WHILE Type + TranNum <= lcTranMode + lcRangeTo
      IF !EMPTY(ldRangeFr) OR !EMPTY(ldRangeTo)
        IF lcTranMode  = 'O'
          REPLACE REST cSelect WITH ' ' WHILE Type + TranNum <= lcTranMode + lcRangeTo ;
                  FOR BETWEEN(OrdHdr.Complete,ldRangeFr,ldRangeTo)
        ELSE
          REPLACE REST cSelect WITH ' ' WHILE Type + TranNum <= lcTranMode + lcRangeTo ;
                  FOR BETWEEN(InvHdr.InvDate,ldRangeFr,ldRangeTo)
        ENDIF          
      ELSE
        REPLACE REST cSelect WITH ' ' WHILE Type + TranNum <= lcTranMode + lcRangeTo
      ENDIF
      *B607461,1 SSE 09/01/2003 Add the date range condition. [End]      
    CASE lcScope = 'I'      
      *B607461,1 SSE 09/01/2003 Add the date range condition. [Begin]
      *REPLACE REST cSelect WITH IIF(cSelect='»',' ','»') WHILE Type + TranNum <= lcTranMode + lcRangeTo
      IF !EMPTY(ldRangeFr) OR !EMPTY(ldRangeTo)
        IF lcTranMode  = 'O'
          REPLACE REST cSelect WITH IIF(cSelect='»',' ','»') WHILE Type + TranNum <= lcTranMode + lcRangeTo ;
                  FOR BETWEEN(OrdHdr.Complete,ldRangeFr,ldRangeTo)
        ELSE
          REPLACE REST cSelect WITH IIF(cSelect='»',' ','»') WHILE Type + TranNum <= lcTranMode + lcRangeTo ;
                  FOR BETWEEN(InvHdr.InvDate,ldRangeFr,ldRangeTo)
        ENDIF          
      ELSE
        REPLACE REST cSelect WITH IIF(cSelect='»',' ','»') WHILE Type + TranNum <= lcTranMode + lcRangeTo
      ENDIF
      *B607461,1 SSE 09/01/2003 Add the date range condition. [End]      
  ENDCASE
  
  *-- Restore file environment
  =SEEK(lcKey)
  IF EMPTY(lcOldOrder)
    SET ORDER TO
  ELSE
    SET ORDER TO &lcOldOrder
  ENDIF
  SET NEAR &lcOldNear
  =lfwBrowTrn()
ENDIF
*B605258,1 TMI [END  ] Range function
