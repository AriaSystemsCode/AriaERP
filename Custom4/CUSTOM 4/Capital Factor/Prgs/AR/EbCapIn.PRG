*:*********************************************************************************
*: Program file  : EBFACIN.PRG
*: Program desc. : Send Invoice and Sales Orders to CAPITAL FACTOR C#102388
*: For screen    : Menu
*:        System : Aria4 XP.
*:        Module : Accounts Receivable (AR).
*:     Developer : Waleed Hamed (WLD)
*:     Entry     : C#102388 convert from aria27 to Aria4
*:*********************************************************************************
*! Passed Parameters  :  lcTranType
*!                       'I' : Send Invoices
*!                       'O' : Send Sales Orders
*:*********************************************************************************
*: Modifications :
*:*********************************************************************************
PARAMETER lcTranType
DO FORM oAriaApplication.ScreenHome+ "AR\EBCAPIN.SCX" WITH lcTranType

*!*************************************************************
*! Name : lfOpenFiles
*! Auth : Waleed Hamed (WLD)
*! Date : 09/17/2006
*!**************************************************************
*! Synopsis :
*!*************************************************************
*! Called from :
*!         Procedures : Init in EBFACIN.scx
*!*************************************************************
*! Calls :
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfOpenFiles
LPARAMETERS loFormSet

SET MULTILOCKS ON
=gfOpentable(oAriaApplication.SysPath+'SYCFACT',oAriaApplication.SysPath+'CFACCODE','SH')
=gfOpentable(oAriaApplication.DataDir+'INVHDR',oAriaApplication.DataDir+'INVHDRA','SH')
=gfOpentable(oAriaApplication.DataDir+'ORDHDR',oAriaApplication.DataDir+'ORDHDR','SH')
=gfOpentable(oAriaApplication.DataDir+'CUSTOMER',oAriaApplication.DataDir+'CUSTOMER','SH')
=gfOpentable(oAriaApplication.DataDir+'CitTrnLn',oAriaApplication.DataDir+'CitTrnLn','SH')
=gfOpentable(oAriaApplication.DataDir+'CODES',oAriaApplication.DataDir+'CODES','SH')

*!*************************************************************
*! Name : lfFormInit
*! Auth : Waleed Hamed (WLD)
*! Date : 09/17/2006
*!**************************************************************
*! Synopsis :
*!*************************************************************
*! Called from :
*!         Procedures : Init in EBFACIN.scx
*!*************************************************************
*! Calls :
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfFormInit
LPARAMETERS loFormSet

STORE '' TO loFormSet.lcClientID,loFormSet.lcClientNo,loFormSet.lcBatchId,loFormSet.lcPassWord
STORE  0 TO loFormSet.lnAssignNo,loFormSet.lnBatchNo
loFormSet.lnLastTran = 2

IF loFormSet.lcTranType = 'O'
  *-- Create CAPITAL FACTOR Send Orders Temp. File
  loFormSet.lcTmpCit = gfTempName()
  CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lcTmpCit) (cFacCode C(6),BatchNo N(2),dDate D,Account C(5),ORDER C(6))
ENDIF

*-- Create transaction temp. file
loFormSet.lcTempTran = gfTempName()
CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lcTempTran) ;
  (TYPE C(1), Account C(5), TranNum C(6), cSelect C(1),InvDate D(8),STORE C(8),ORDER C(6),FactAcct C(10),Ship N(7),TotalChg N(14,2),;
  CrDate D(8),RANO C(6),REFERENCE C(30),TotCredit N(14,2),Season C(20),Division C(20),STATUS C(1),StName C(20),cStore C(8),START D(8),COMPLETE D(8),;
  OpenAmt N(7,2),ApprAmt N(7,2),lSelect L(1))
INDEX ON cSelect+TYPE+Account+TranNum TAG 'SELECT'
INDEX ON TYPE + TranNum TAG RangeSelct
INDEX ON TYPE+Account+TranNum TAG (loFormSet.lcTempTran)  ADDITIVE

SET RELATION TO 'M'+Account INTO CUSTOMER
IF loFormSet.lcTranType = 'I'
  SET RELATION TO Account+TranNum INTO INVHDR ADDITIVE
ELSE
  SET RELATION TO Account+'O'+TranNum INTO ORDHDR ADDITIVE
ENDIF

loFormSet.lcOutFile = oAriaApplication.DataDir + 'CAP'+PADL(MONTH(oAriaApplication.SystemDate),2,'0')+PADL(DAY(oAriaApplication.SystemDate),2,'0')+'.NEW'
IF FILE(oAriaApplication.DataDir+"MEMO.MEM")
  RESTORE FROM oAriaApplication.DataDir+"MEMO" ADDITIVE
ENDIF

ENDFUNC

*!*************************************************************
*! Name : lfShow
*! Auth : Waleed Hamed (WLD)
*! Date : 09/17/2006
*!**************************************************************
*! Synopsis :
*!*************************************************************
*! Called from :
*!         Procedures : Init in EBFACIN.scx
*!*************************************************************
*! Calls :
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfShow
LPARAMETERS loFormSet
loFormSet.ariaForm1.ENABLED = .T.

DO CASE
CASE loFormSet.activemode = 'S'
  WITH loFormSet.ariaForm1
    STORE .T. TO .kbFactor.ENABLED , .kbFactor.keytextbox.ENABLED , .kbFactor.keyCmd.ENABLED
    STORE .F. TO .kBCustomer.ENABLED , .kBCustomer.keyCmd.ENABLED , .kBCustomer.keytextbox.ENABLED ,;
      .txtCustFact.ENABLED , .txtCustName.ENABLED , .txtFactName.ENABLED ,;
      .txtOutFile.ENABLED , .cmdGetFile.ENABLED , .cboLastTran.ENABLED ,;
      .cmdProceed.ENABLED , .cmdSelect.ENABLED
    STORE "" TO .kbFactor.keytextbox.VALUE ,.txtFactName.VALUE , .kBCustomer.keytextbox.VALUE , .txtCustFact.VALUE , .txtCustName.VALUE
    .cboLastTran.VALUE = '0'
    .txtOutFile.VALUE = loFormSet.lcOutFile
  ENDWITH
ENDCASE
*!*************************************************************
*! Name      : lfvFactor
*! Auth      : Waleed Hamed (WLD)
*! Date      : 09/17/2006
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
FUNCTION lfvFact
LPARAMETERS loFormSet, llBrowse
PRIVATE lcFactor, lnAlias
lnAlias = SELECT()

lcFactor   = loFormSet.AriaForm1.kbFactor.KeyTextBox.VALUE
lcOldValue = loFormSet.AriaForm1.kbFactor.KeyTextBox.OldValue
IF llBrowse OR (!EMPTY(lcFactor) .AND. !GFSEEK(lcFactor,'SycFact'))
  lcBrFields = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  SELECT SycFact
  lcFactor = IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,gnBrFSRow2, gnBrFSCol2,'','',;
    'cFacCode','laBrowArr',.F., .F., .F., 'SycFact'),SycFact.cFacCode,SPACE(6))
ENDIF

IF lcOldValue <> lcFactor
  STORE .T. TO loFormSet.llInvoice , loFormSet.llOrders
ENDIF

IF EMPTY(lcFactor)
  WITH loFormSet.ariaForm1
    STORE .F. TO .kBCustomer.ENABLED , .kBCustomer.keyCmd.ENABLED , .kBCustomer.keytextbox.ENABLED ,;
      .txtOutFile.ENABLED , .cmdGetFile.ENABLED , .cboLastTran.ENABLED ,;
      .cmdSelect.ENABLED
  ENDWITH
ELSE
  WITH loFormSet.ariaForm1
    STORE .T. TO .kBCustomer.ENABLED , .kBCustomer.keyCmd.ENABLED , .kBCustomer.keytextbox.ENABLED ,;
      .cmdGetFile.ENABLED , .cboLastTran.ENABLED ,;
      .cmdSelect.ENABLED
  ENDWITH
ENDIF
loFormSet.AriaForm1.txtFactName.VALUE = IIF(!EMPTY(lcFactor),SycFact.cFacComp,loFormSet.AriaForm1.txtFactName.VALUE)

SELECT (lnAlias)
llBrowse = .F.
RETURN !EMPTY(lcFactor)

*!*************************************************************
*! Name      : lfvCustomer
*! Developer : Waleed Hamed (WLD)
*! Date      : 09/17/2006
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
FUNCTION lfvCust
LPARAMETERS loFormSet, llBrowse
PRIVATE xAccount

lcCustomer = loFormSet.AriaForm1.kbCustomer.KeyTextBox.VALUE

IF llBrowse .OR. (!EMPTY(lcCustomer) .AND. !GFSEEK('M'+lcCustomer,'CUSTOMER'))
  xAccount = lcCustomer
  SELECT CUSTOMER
  DO CUSBROWM WITH xAccount
  lcCustomer = xAccount
ENDIF


lcOldValue = loFormSet.AriaForm1.kbCustomer.KeyTextBox.OldValue

IF lcOldValue <> lcCustomer
  STORE .T. TO loFormSet.llInvoice , loFormSet.llOrders
ENDIF

lcCustFact = IIF(EMPTY(CUSTOMER.FactAcct),'**NEW**',CUSTOMER.FactAcct)
lcCustName = CUSTOMER.BtName

loFormSet.AriaForm1.txtCustFact.VALUE = lcCustFact
loFormSet.AriaForm1.txtCustName.VALUE = lcCustName
llBrowse = .F.

*!*************************************************************
*! Name      : lfvSelTrans
*! Developer : Waleed Hamed (WLD)
*! Date      : 09/17/2006
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
FUNCTION lfvSelTrans
LPARAMETERS loFormSet

lcFactor   = ALLTRIM(loFormSet.AriaForm1.kbFactor.KeyTextBox.VALUE)
lcCustomer = ALLTRIM(loFormSet.AriaForm1.kbCustomer.KeyTextBox.VALUE)
SET ORDER TO (loFormSet.lcTempTran) IN (loFormSet.lcTempTran)
IF loFormSet.llInvoice AND loFormSet.lcTranType = 'I'
  SELECT (loFormSet.lcTempTran)
  =gfSEEK('I')
  DELETE REST WHILE TYPE = 'I'
  SELECT INVHDR
  =gfSetOrder('INVHDRA')
  =GFSEEK(ALLTRIM(lcCustomer))
  SCAN REST WHILE Account+Invoice = ALLTRIM(lcCustomer) FOR cFacCode = lcFactor AND STATUS <>'V'
    INSERT INTO (loFormSet.lcTempTran) (cSelect,TYPE,Account,TranNum,invdate,STORE,ORDER,ship,totalchg,lSelect);
      VALUES ('N','I',INVHDR.Account,INVHDR.Invoice,INVHDR.invdate,INVHDR.STORE,INVHDR.ORDER,INVHDR.ship,INVHDR.totalchg,.F.)
    SELECT CUSTOMER
    =GFSEEK(PADR(lcCustomer,5))
    SELECT (loFormSet.lcTempTran)
    REPLACE FactAcct WITH IIF(EMPTY(CUSTOMER.factacct),'**NEW**',CUSTOMER.factacct)
  ENDSCAN
  loFormSet.llInvoice = .F.
ENDIF

IF loFormSet.llOrders AND loFormSet.lcTranType = 'O'
  SELECT (loFormSet.lcTempTran)
  =gfSEEK('O')
  DELETE REST WHILE TYPE = 'O'
  SELECT ORDHDR
  IF EMPTY(lcCustomer)
    =gfSetOrder('ORDHDR')
    =GFSEEK('O')
    lcScanExpr = 'cOrdType+ORDER'
    lcExpr = 'O'
  ELSE
    =gfSetOrder('ORDACCT')
    =GFSEEK(lcCustomer+"O")
    lcScanExpr = 'Account+cOrdType+ORDER'
    lcExpr = lcCustomer+'O'
  ENDIF
  SCAN REST WHILE &lcScanExpr = lcExpr FOR cFacCode = lcFactor AND STATUS <>'X'
    INSERT INTO (loFormSet.lcTempTran) (cSelect,TYPE,Account,TranNum,Season,Division,STATUS,cStore,START,COMPLETE,OpenAmt,ApprAmt,lSelect);
      VALUES ('N','O',ORDHDR.Account,ORDHDR.ORDER,gfCodDes(OrdHdr.Season,'SEASON'),gfCodDes(OrdHdr.cDivision,'CDIVISION'),OrdHdr.STATUS,;
      IIF(OrdHdr.MULTI='Y','*MULTI*',OrdHdr.STORE),OrdHdr.START,OrdHdr.COMPLETE,OrdHdr.OpenAmt,OrdHdr.ApprAmt,.F.)
    SELECT CUSTOMER
    =GFSEEK(PADR(lcCustomer,5))
    SELECT (loFormSet.lcTempTran)
    REPLACE StName WITH SUBSTR(Customer.StName,1,20)
  ENDSCAN
  =gfSetOrder('ORDACCT')
  loFormSet.llOrders = .F.
ENDIF

SELECT (loFormSet.lcTempTran)
LOCATE
lnTrans    = 0
lcTranMode = IIF(loFormSet.lcTranType='O','O','I')

DO FORM oAriaApplication.ScreenHome+ "AR\EBCAPTR.SCX" WITH loFormSet,'EBCAPIN',loFormSet.lcTranType

IF loFormSet.lcTranType <> 'O'
  loFormSet.lcTranType = 'I'
ENDIF

SET ORDER TO TAG 'SELECT' IN (loFormSet.lcTempTran)
IF gfSEEK('Y',loFormSet.lcTempTran)
  loFormSet.Ariaform1.cmdProceed.ENABLED = .T.
ELSE
  loFormSet.Ariaform1.cmdProceed.ENABLED = .F.
ENDIF
SET ORDER TO TAG (loFormSet.lcTempTran) IN (loFormSet.lcTempTran)

*!*************************************************************
*! Name : lfGetFile
*! Developer : Waleed Hamed (WLD)
*! Date      : 09/17/2006
*! Ref  : B605078,1
*!*************************************************************
*! Synopsis : Give ability to user to choose name and path of;
*!            the output file.
*!*************************************************************
*! Called from : None.
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfGetFile
LPARAMETERS loFormSet

PRIVATE lcOutFile

lcOutFile = GETFILE('NEW','Select Output File')

IF !EMPTY(lcOutFile)
  loFormSet.Ariaform1.txtOutFile.VALUE = lcOutFile
  loFormSet.lcOutFile = lcOutFile
  SAVE TO oAriaApplication.DataDir+"MEMO" ALL LIKE lcOutFile
ENDIF


*!*************************************************************
*! Name      : lfvProc
*! Developer : Waleed Hamed (WLD)
*! Date      : 09/17/2006
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
FUNCTION lfvProc
LPARAMETERS loFormSet

*-- Client Information
loFormSet.lcClientID = SycFact.cClientID
loFormSet.lcClientNo = SycFact.cClientNo
loFormSet.lcBatchId  = 'C' +IIF(loFormSet.lcTranType='I','I','O') +loFormSet.lcClientNo

loFormSet.lcPassWord = SycFact.cPassWord
loFormSet.lnAssignNo = SycFact.AssignNo
loFormSet.lnBatchNo  = MAX(SycFact.BatchNo,1)

IF EMPTY(loFormSet.lcClientID) .OR. EMPTY(loFormSet.lcClientNo) .OR. EMPTY(loFormSet.lcPassWord)
  =gfModalGen('TRM00000B00000','ALERT','','','Factor information not complete. Cannot proceed.')
  RETURN
ENDIF
*-- Check if the output file allready exists.
IF FILE(loFormSet.lcOutFile)
  IF gfModalGen('QRM00000B00006','ALERT','','','Output file '+loFormSet.lcOutFile+' already exist. Overwrite it?') = 2
    RETURN
  ENDIF
ENDIF
*-- Open the output file
lnOutFile = FCREATE(loFormSet.lcOutFile,0)
IF lnOutFile < 0
  =gfModalGen('TRM00000B00000','ALERT','','','Cannot open output file. Cannot proceed.')
  RETURN
ENDIF
*-- Get Transmission date in the format MMDDYY
lcTranDate = PADL(MONTH(oAriaApplication.SystemDate),2,'0') + PADL(DAY(oAriaApplication.SystemDate),2,'0') +;
  RIGHT(STR(YEAR(oAriaApplication.SystemDate),4),2)

=IIF(loFormSet.lcTranType='O',lfWrtOrder(loFormSet),lfWrtInv(loFormSet))
*-- Increament the Assignment number in customer file
SELECT SYCFACT
= RLOCK()
lnLastTran = loFormSet.lnLastTran
lnBatchNo  = loFormSet.lnBatchNo
lnAssignNo = loFormSet.lnAssignNo
=gfREPLACE([AssignNo WITH IIF(lnLastTran=1,0,lnAssignNo) ,]+;
  [BatchNo  WITH IIF(lnBatchNo=99, 1,lnBatchNo+1)])
=gfTableUpdate('SYCFACT')
UNLOCK
=FCLOSE(lnOutFile)

IF loFormSet.lcTranType = 'O'
  SELECT (loFormSet.lcTmpCit)
  SCAN
    SCATTER MEMVAR MEMO
    SELECT CitTrnLn
    =gfAppend()
    GATHER MEMVAR MEMO
    =gfReplace('')
  ENDSCAN
  SELECT CitTrnLn
  =gfTableUpdate('CitTrnLn')
ENDIF

CLEAR READ
*E300817,1 Message : 00370
*E300817,1 Output file has been created
*E300817,1 Button : 00000
*E300817,1 Ok
=gfModalGen('TRM00000B00000','ALERT','','','Output file '+loFormSet.lcOutFile+' has been created.')

loFormSet.activemode = 'S'
=lfShow(loFormSet)

*!*************************************************************
*! Name : lfWrtInv
*! Auth : Waleed Hamed (WLD)
*! Date : 09/17/2006
*!**************************************************************
*! Synopsis : Write an output Invoices text file to be send to Capital Factor.
*!*************************************************************
*! Called from :
*!         Procedures : EBFACIN
*!*************************************************************
*! Calls :
*!         FUNCTIONS : lfCalcInvTot
*!                     lfAddInvRec
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfWrtInv
LPARAMETERS loFormSet

PRIVATE lcCustomer

WAIT 'Creating outbound Invoice file...' WINDOW NOWAIT

SELECT (loFormSet.lcTempTran)
SET ORDER TO TAG SELECT

*-- Initialize Invoice records and Invocie Dollars
STORE 0 TO lnInvAmt
lnInvAmt = lfCalcInvTot()

*-- First write Invoice records
IF gfSEEK('YI')
  DO WHILE cSelect+TYPE+Account+TranNum = "YI"
    lcCustomer = Account
    SELECT Customer
    IF gfSEEK('M'+lcCustomer)
      lcCustNum = IIF(EMPTY(Customer.FactAcct),PADR(lcCustomer,9),LEFT(Customer.FactAcct,9) )
    ENDIF
    SELECT (loFormSet.lcTempTran)
    SCAN REST WHILE cSelect+TYPE+Account+TranNum = "YI"+lcCustomer
      *-- Add Invoice Record Type "D"
      =lfAddInvRec(loFormSet)
    ENDSCAN
  ENDDO
ENDIF
SELECT (loFormSet.lcTempTran)
SET RELATION TO

*!*************************************************************
*! Name : lfAddInvRec
*! Auth : Waleed Hamed (WLD)
*! Date : 09/17/2006
*!*************************************************************
*! Synopsis : Write an invoice record in the output orders text file
*!*************************************************************
*! Called from :
*!         Procedures : EBFACIN
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddInvRec
LPARAMETERS loFormSet

PRIVATE lcSegLine, lcTmpDate, lcCapTerm, lcTermDesc
DECLARE laTRltFld[2,2]
laTRltFld[1,1] = 'CCAPTRMCOD'
laTRltFld[1,2] = 'lcCapTerm'
laTRltFld[2,1] = 'CCAPTRMDES'
laTRltFld[2,2] = 'lcTermDesc'

*-- Move the record pointer to the coresponding record in order header file
=gfSEEK(InvHdr.Account + "O" + InvHdr.ORDER , "OrdHdr")

*-- Prepare to get "Bill To Information"
IF EMPTY(INVHDR.STORE)
  =gfSEEK('M' + InvHdr.Account , 'Customer')
ELSE
  =gfSEEK('S' + InvHdr.Account + INVHDR.STORE , 'Customer')
ENDIF
*-- Terms Information
lcCapTerm   = SPACE(3)
lcTermDesc  = SPACE(40)
=gfRltFld(InvHdr.cTermCode,@laTRltFld,'CTERMCODE')

lcSegLine = ''
*-- Client Number
lcSegLine = lcSegLine + loFormSet.lcClientNo
*-- Factor Code Internal Always "IV"
lcSegLine = lcSegLine + 'IV'
*-- Invoice Date
lcSegLine = lcSegLine + DTOS(InvHdr.InvDate)
*-- Filler
lcSegLine = lcSegLine + SPACE(8)
*-- Invoice Number
lcSegLine = lcSegLine + PADR(EVALUATE(loFormSet.lcTempTran+'.TranNum'), 10)
*-- Invoice Amount
lcSegLine = lcSegLine + STR(InvHdr.TotalChg * 100 , 11 , 0)
*-- Order Date
lcSegLine = lcSegLine + DTOS(OrdHdr.Entered)
*-- Order Number
lcSegLine = lcSegLine + PADR(invHdr.ORDER, 15)
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
lcSegLine = lcSegLine + PADR(loFormSet.lcClientNo + DTOS(DATE()) , 15)
*-- Total amount of TRANSMISSION
lcSegLine = lcSegLine + STR(lnInvAmt * 100 , 11 , 0)
*-- Filler
lcSegLine = lcSegLine + SPACE(15)
*-- Filler
lcSegLine = lcSegLine + SPACE(20)

= FPUTS(lnOutFile,lcSegLine)
*!*************************************************************
*! Name      : lfWrtOrder
*! Developer : Waleed Hamed (WLD)
*! Date      : 09/17/2006
*! Purpose   : Write outbound Order
*!*************************************************************
*! Calls     : lfAddOrdRec()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBrowTran()
*!*************************************************************
FUNCTION lfWrtOrder
LPARAMETERS loFormSet

PRIVATE lcCustomer

lcFactor   = loFormSet.AriaForm1.kbFactor.KeyTextBox.VALUE

WAIT'Creating outbound Orders file...' WINDOW NOWAIT

SELECT (loFormSet.lcTempTran)
SET ORDER TO TAG SELECT
=gfSEEK('YO')
DO WHILE cSelect+TYPE+Account+TranNum = "YO"
  lcCustomer = Account
  SELECT CUSTOMER
  *-- Get customer factor number
  IF gfSEEK('M'+lcCustomer)
    lcCustNum   = IIF(EMPTY(Customer.FactAcct),PADL(lcCustomer,9,'0'),LEFT(Customer.FactAcct,9) )
  ENDIF
  SELECT (loFormSet.lcTmpCit)
  ZAP

  SELECT (loFormSet.lcTempTran)
  *-- Add Order Record for this customer Type "D"
  SCAN WHILE cSelect+TYPE+Account+TranNum = "YO"+lcCustomer
    =lfAddOrdRec(loFormSet)

    *-- Update the CitTrnLn Temp. File
    INSERT INTO (loFormSet.lcTmpCit) (cFacCode,BatchNo,dDate,Account,ORDER) VALUES ;
      (lcFactor,loFormSet.lnBatchNo,oAriaApplication.SystemDate,lcCustomer,OrdHdr.ORDER)
  ENDSCAN
ENDDO

WAIT CLEAR
*!*************************************************************
*! Name : lfAddOrdRec
*! Auth : Waleed Hamed (WLD)
*! Date : 08/27/2006
*! Ref  : E500439,1 AMM
*!*************************************************************
*! Synopsis : Write the order record in the output orders text file
*!*************************************************************
*! Called from :
*!         Procedures : lfWrtOrder
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddOrdRec
LPARAMETERS loFormSet
*********
PRIVATE lcSegLine, lcTmpDate
DECLARE laTRltFld[2,2]
laTRltFld[1,1] = 'CCAPTRMCOD'
laTRltFld[1,2] = 'lcCapTerm'
laTRltFld[2,1] = 'CCAPTRMDES'
laTRltFld[2,2] = 'lcTermDesc'

*-- Prepare to get "Bill To Information"
IF EMPTY(ORDHDR.STORE)
  =gfSEEK('M' + ORDHDR.Account , 'Customer')
ELSE
  =gfSEEK('S' + ORDHDR.Account + ORDHDR.STORE , 'Customer')
ENDIF

*-- Terms Information
lcCapTerm   = SPACE(3)
lcTermDesc  = SPACE(40)
= gfRltFld(OrdHdr.cTermCode,@laTRltFld,'CTERMCODE')

lcSegLine = ''
*01-- Client Number
lcSegLine = lcSegLine + loFormSet.lcClientNo
*02-- Order Number
lcSegLine = lcSegLine + PADR(EVALUATE(loFormSet.lcTempTran+'.TranNum'), 10)
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
lcSegLine = lcSegLine + DTOS(OrdHdr.COMPLETE)
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
*! Name : lfCalcInvTot
*! Auth : Waleed Hamed (WLD)
*! Date : 09/17/2006
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

IF gfSEEK('YI')
  SCAN REST WHILE cSelect + TYPE + Account + TranNum = "YI"
    lnInvTot = lnInvTot + InvHdr.TotalChg
  ENDSCAN
ENDIF

RETURN lnInvTot
