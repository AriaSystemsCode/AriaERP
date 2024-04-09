*!*************************************************************
*! Name      : EBFACIN
*! Developer : Wael Ali Mohamed
*! Date      : 30/12/1999
*! Purpose   : Send Invoice and Sales Orders to Capital Factor
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcTranType
*!                       'I' : Send Invoice
*!                       'O' : Send Sales Orders
*!*************************************************************
*! Note      : This program and the screens it uses were copied
*!             from Rosental Send Invoices\Orders program, which
*!             was made for customer RAY00 with custom program
*!             number 102181.
*!*************************************************************
*!B804158,1 HS  05/09/2001 Fix the format of some fields.
*!B604858,1 HS  09/01/2001 add select range of invoices and orders
*!B804408,1 AMM Adjust order amount to be right justified
*!*************************************************************
PARAMETER lcTranType

DECLARE laTRltFld[2,2]
laTRltFld[1,1] = 'CCAPTRMCOD'
laTRltFld[1,2] = 'lcCapTerm'
laTRltFld[2,1] = 'CCAPTRMDES'
laTRltFld[2,2] = 'lcTermDesc'
STORE ' ' TO lcBrowseTl,lcOldValue
STORE .F. TO llBrowse
STORE .T. TO llInvoice,llOrders
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

*-- Create transaction temp. file
lcTempTran = gfTempName()
CREATE TABLE (gcWorkDir+lcTempTran) ;
	(Type C(1), Account C(5), TranNum C(6), cSelect C(1))
INDEX ON CsELECT+Type+Account+TranNum TAG 'SELECT'
*B604858,1 [Begin]
INDEX ON Type + TranNum TAG RangeSelct
*B604858,1 [End]
INDEX ON Type+Account+TranNum TAG (lcTempTran)  ADDITIVE
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
lcOutFile = gcDataDir + 'CIT'+PADL(MONTH(gdSysDate),2,'0')+PADL(DAY(gdSysDate),2,'0')+'.NEW'
IF FILE(gcDataDir+"MEMO.MEM")
  RESTORE FROM gcDataDir+"MEMO" additive
ENDIF  
*C102494 Hassan 10/29/2001 [End]

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

DO (gcScrDir + IIF(RIGHT(gcScrDir , 1) = "\" , "" , "\") + "EBFACIN.SPX") WITH 'I'

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
DO (gcScrDir + IIF(RIGHT(gcScrDir , 1) = "\" , "" , "\") + "EBSNDTR.SPX") WITH IIF(lcTranType='O','O','I')
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
lcKey = Type+Account+TranNum
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
IF LEN(lcClientNo) <> 4 
  =gfModalGen('TRM00000B00000','ALERT','','','Invalid Client Number. Cannot proceed.')
  RETURN
ENDIF
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
STORE 0 TO lnInvAmt

SELECT (lcTempTran)
SET ORDER TO TAG SELECT
lnInvAmt = lfCalcInvTot()

*-- First write Invoice records
IF SEEK('»I')
  DO WHILE cSelect+Type+Account+TranNum = "»I"
    lcCustomer = Account
    =SEEK('M'+lcCustomer,'Customer') 
    *B804158,1 HS 05/09/2001 Fix the customer code format [Begin]
    *lcCustNum = IIF(EMPTY(Customer.FactAcct),PADL(lcCustomer,9,'0'),LEFT(Customer.FactAcct,9) )
    lcCustNum = IIF(EMPTY(Customer.FactAcct),PADR(lcCustomer,9),LEFT(Customer.FactAcct,9) )
    *B804158,1 HS 05/09/2001 Fix the customer code format [End]
    SCAN REST WHILE cSelect+Type+Account+TranNum = "»I" + lcCustomer
      *-- Add Invoice Record Type "D"
      =lfAddInvRec()
    ENDSCAN
  ENDDO
ENDIF  

*!*************************************************************
*! Name : lfCalcInvTot
*! Auth : Imad
*! Date : 07/19/2001
*!*************************************************************
*! Synopsis : Calculate  the Total amount of the Selected Invoices
*!*************************************************************
*! Called from : 
*!         Procedures : lfWrtInv
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfCalcInvTot

PRIVATE lnInvTot
STORE 0 TO lnInvTot

IF SEEK('»I')
  SCAN REST WHILE cSelect + Type + Account + TranNum = "»I"
    lnInvTot = lnInvTot + InvHdr.TotalChg
  ENDSCAN
ENDIF

RETURN lnInvTot

*!*************************************************************
*! Name : lfAddInvRec
*! Auth : Imad
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
PRIVATE lcSegLine, lcTmpDate, lcCapTerm, lcTermDesc

*-- Move the record pointer to the coresponding record in order header file
=SEEK(InvHdr.Account + "O" + InvHdr.Order , "OrdHdr")
 
*-- Prepare to get "Bill To Information"
IF EMPTY(INVHDR.Store)
  =SEEK('M' + INVHDR.Account , 'Customer')
ELSE
  =SEEK('S' + INVHDR.Account + INVHDR.Store , 'Customer')
ENDIF

*-- Get the term code desc.
*=SEEK("N" + InvHdr.cTermCode + "N" + PADR("CTERMCODE" , 10) , "Codes")
*lcTermDesc = Codes.cDiscrep

*-- Terms Information
lcCapTerm   = SPACE(3)
lcTermDesc  = SPACE(40)
=gfRltFld(InvHdr.cTermCode,@laTRltFld,'CTERMCODE')

lcSegLine = ''
*-- Client Number
lcSegLine = lcSegLine + lcClientNo
*-- Factor Code Internal Always "IV"
lcSegLine = lcSegLine + 'IV'
*-- Invoice Date
lcSegLine = lcSegLine + DTOS(InvHdr.InvDate)
*-- Filler
lcSegLine = lcSegLine + SPACE(8)
*-- Invoice Number 
lcSegLine = lcSegLine + PADR(invHdr.invoice, 10)
*-- Invoice Amount
lcSegLine = lcSegLine + STR(InvHdr.TotalChg * 100 , 11 , 0)
*-- Order Date
lcSegLine = lcSegLine + DTOS(OrdHdr.Entered)
*-- Order Number
lcSegLine = lcSegLine + PADR(invHdr.order, 15)
*-- Invoice Terms
lcSegLine = lcSegLine + PADR(lcCapTerm , 3)
*-- Term Description
lcSegLine = lcSegLine + PADR(lcTermDesc, 40)
*-- Filler
lcSegLine = lcSegLine + SPACE(2)
*-- Filler
lcSegLine = lcSegLine + SPACE(9)
*-- Filler
lcSegLine = lcSegLine + SPACE(2)
*-- Filler
lcSegLine = lcSegLine + SPACE(9)
*-- Ship to Account ID
lcSegLine = lcSegLine + PADR(InvHdr.Account, 15)
*-- Invoice Date
lcSegLine = lcSegLine + DTOS(InvHdr.InvDate)
*-- Customer Ship to Name
lcSegLine = lcSegLine + IIF(ORDHDR.Alt_ShpTo , SPACE(30) , Customer.STName)
*-- Ship to's Address 1
lcSegLine = lcSegLine + IIF(ORDHDR.Alt_ShpTo , SPACE(25) , LEFT(Customer.cAddress1, 25))
*-- Ship to's Address 2
lcSegLine = lcSegLine + IIF(ORDHDR.Alt_ShpTo , SPACE(25) , LEFT(Customer.cAddress2, 25))
*-- Ship to's City
lcSegLine = lcSegLine + IIF(ORDHDR.Alt_ShpTo , SPACE(18) , LEFT(Customer.cAddress3, 18))
*-- Ship to's State
lcSegLine = lcSegLine + IIF(ORDHDR.Alt_ShpTo , SPACE(2) , LEFT(Customer.cAddress4, 2))
*-- Filler
lcSegLine = lcSegLine + SPACE(3)
*-- Filler
lcSegLine = lcSegLine + SPACE(3)
*-- Ship to's Zip
lcSegLine = lcSegLine + IIF(ORDHDR.Alt_ShpTo , SPACE(10) , LEFT(Customer.cAddress5, 10))
*-- Customer Bill to Account ID
lcSegLine = lcSegLine + PADR(InvHdr.Account, 15)
*-- Cutomer Bill to Name
lcSegLine = lcSegLine + Customer.BTName
*-- Bill to Name Address 1
lcSegLine = lcSegLine + LEFT(Customer.cAddress12, 25)
*-- Bill to Name Address 2
lcSegLine = lcSegLine + LEFT(Customer.cAddress22, 25)
*-- Bill to's City
lcSegLine = lcSegLine + LEFT(Customer.cAddress32, 18)
*-- Bill to's State
lcSegLine = lcSegLine + LEFT(Customer.cAddress42, 2)
*-- Filler
lcSegLine = lcSegLine + SPACE(3)
*-- Filler
lcSegLine = lcSegLine + SPACE(3)
*-- Bill to's Zip
lcSegLine = lcSegLine + LEFT(Customer.cAddress52, 10)
*-- Filler
lcSegLine = lcSegLine + SPACE(10)
*-- Filler
lcSegLine = lcSegLine + SPACE(10)
*-- Filler
lcSegLine = lcSegLine + SPACE(1)
*-- Filler
lcSegLine = lcSegLine + SPACE(10)
*-- Filler
lcSegLine = lcSegLine + SPACE(15)
*-- APRO ASSIGNED TRANSMISSION NUMBER
lcSegLine = lcSegLine + PADR(lcClientNo + DTOS(DATE()) , 15)
*-- Total amount of TRANSMISSION
lcSegLine = lcSegLine + STR(lnInvAmt * 100 , 11 , 0)
*-- Filler
lcSegLine = lcSegLine + SPACE(15)
*-- Filler
lcSegLine = lcSegLine + SPACE(20)

= FPUTS(lnOutFile,lcSegLine)
*-- End

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
  lcCustomer  = Account
  *-- Get customer factor number
  =SEEK('M'+lcCustomer,'Customer') 
  lcCustNum   = IIF(EMPTY(Customer.FactAcct),PADL(lcCustomer,9,'0'),LEFT(Customer.FactAcct,9) )
  *-- Add Order Record for this customer Type "D"    
  SCAN WHILE cSelect+Type+Account+TranNum = "»O"+lcCustomer
    =lfAddOrdRec()
    
  ENDSCAN
ENDDO
 
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
*! Modifications :
*! 	Modi No 1
*! 	By Hassan Ibrahim Aly 
*!   Date : 17/07/2001
*!*************************************************************
FUNCTION lfAddOrdRec
PRIVATE lcSegLine, lcTmpDate

*-- Prepare to get "Bill To Information"
IF EMPTY(ORDHDR.Store)
  =SEEK('M' + ORDHDR.Account , 'Customer')
ELSE
  =SEEK('S' + ORDHDR.Account + ORDHDR.Store , 'Customer')
ENDIF

*-- Terms Information
lcCapTerm   = SPACE(3)
lcTermDesc  = SPACE(40)
= gfRltFld(OrdHdr.cTermCode,@laTRltFld,'CTERMCODE')

lcSegLine = ''
*01-- Client Number
lcSegLine = lcSegLine + lcClientNo
*02-- Order Number
lcSegLine = lcSegLine + PADR(OrdHdr.ORDER, 10)
*03-- Customer Bill-To Account
lcSegLine = lcSegLine + PADR(OrdHdr.ACCOUNT, 15)
*04-- Customer Bill-To Name
lcSegLine = lcSegLine + Customer.BTName
*05-- Bill-To's Adderess1
lcSegLine = lcSegLine + SUBSTR(Customer.cAddress12, 1, 25)
*06-- Bill-To's Adderess2
lcSegLine = lcSegLine + SUBSTR(Customer.cAddress22, 1, 25)
*07- Bill-To's City
lcSegLine = lcSegLine + SUBSTR(Customer.cAddress32, 1, 18)
*08- Bill-To's State
lcSegLine = lcSegLine + SUBSTR(Customer.cAddress42, 1,  2)
*09- Bill-To's Zip
lcSegLine = lcSegLine + SUBSTR(Customer.cAddress52, 1, 10)
*10-- Xcntr (Alwas Blank) Not Avalible
lcSegLine = lcSegLine + SPACE(3)
*11-- Order Date
lcSegLine = lcSegLine + DTOS(OrdHdr.ENTERED)
*12-- Order's Terms 
lcSegLine = lcSegLine + PADR(lcCapTerm , 3)
*13-- Order's Terms (Discription)
lcSegLine = lcSegLine + PADR(lcTermDesc , 40)
*14-- Order Completion Date
lcSegLine = lcSegLine + DTOS(OrdHdr.Complete)
*15-- Order Amount
*B804408,1 AMM make it right alignment
*lcSegLine = lcSegLine + PADR(ROUND(OrdHdr.BookAmt,0), 9)
lcSegLine = lcSegLine + PADL(ROUND(OrdHdr.BookAmt,0), 9)
*B804408,1 AMM end
*16-- XLRQAM (Alwas Blank) Not Avalible
lcSegLine = lcSegLine + SPACE(9)
*17-- XCOMM1 (Alwas Blank) Not Avalible 
lcSegLine = lcSegLine + SPACE(25)
*18-- XCOMM2 (Alwas Blank) Not Avalible 
lcSegLine = lcSegLine + SPACE(25)
*19-- XCOMM3 (Alwas Blank) Not Avalible 
lcSegLine = lcSegLine + SPACE(25)
*20-- XPSMDT (Alwas Blank) Not Avalible 
lcSegLine = lcSegLine + SPACE(8)
*21-- Constant "A"
lcSegLine = lcSegLine + "A"

= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name      : lfvRange
*! Developer : Haytham El-Sheltawi
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
*B804408,1 AMM start
*DO (gcScrDir + "\Ebarange.SPX")
DO (gcScrDir + "Ebarange.SPX")
*B804408,1 AMM end

IF !EMPTY(lcScope)
  SELECT (lcTempTran)
  *-- Save file environment
  lcOldNear  = SET("NEAR")
  lcOldOrder = ORDER()
  lcKey      = Type + TranNum
  
  SET ORDER TO RangeSelct
  SET NEAR ON
  
  SEEK lcTranMode + lcRangeFr
  DO CASE
    CASE lcScope = 'A'
      REPLACE REST cSelect WITH '»' WHILE Type + TranNum <= lcTranMode + lcRangeTo
    CASE lcScope = 'N'
      REPLACE REST cSelect WITH ' ' WHILE Type + TranNum <= lcTranMode + lcRangeTo
    CASE lcScope = 'I'
      REPLACE REST cSelect WITH IIF(cSelect='»',' ','»') WHILE Type + TranNum <= lcTranMode + lcRangeTo
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

lcOutFile = gcDataDir + 'CIT'+PADL(MONTH(gdSysDate),2,'0')+PADL(DAY(gdSysDate),2,'0')+'.NEW'
IF FILE(gcDataDir+"MEMO.MEM")
  RESTORE FROM gcDataDir+"MEMO" additive
ENDIF  

ltOutFile = PUTFILE('' , lcOutFile , "NEW")

IF EMPTY(ltOutFile) = .F.
  lcOutFile = ltOutFile 
  lcTalk = SET('TALK')
  SET TALK OFF
  SAVE TO gcDataDir + "MEMO" ALL LIKE lcOutFile
  SET TALK &lcTalk.
  =lfRefresh('EBFACIN')
ENDIF  
*!*************************************************************