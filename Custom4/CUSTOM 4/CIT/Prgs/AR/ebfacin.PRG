*:*********************************************************************************
*: Program file  : EBFACIN.PRG
*: Program desc. : Send Invoice/Credit Memo and Sales Orders to CIT C#101915
*: For screen    : Menu
*:        System : Aria4 XP.
*:        Module : Accounts Receivable (AR).
*:     Developer : Hend Ghanem (HBG)
*:     Entry     : C#101915 convert from aria27 to Aria4
*:*********************************************************************************
*! Passed Parameters  :  lcTranType
*!                       'I' : Send Invoice/Credit Memos
*!                       'O' : Send Sales Orders
*:*********************************************************************************
*: Modifications :
*:*********************************************************************************
*! B608549,1 WAM 05/11/2008 Send the proper payment terms information
*! B610857,1 MMT 09/17/2014 modify Send Orders to CIT program to use 4 chars from client no[T20140908.0019]
*! B610857,2 MMT 09/14/2014 modify Send Orders to use the client forms not shared ones[T20140908.0019]
*:*********************************************************************************
PARAMETER lcTranType

*! B610857,2 MMT 09/14/2014 modify Send Orders to use the client forms not shared ones[T20140908.0019][Start]
*DO FORM oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleID+ "\EBFACIN.SCX" WITH lcTranType
DO FORM oAriaApplication.clientscreenhome+oAriaApplication.ActiveModuleID+ "\EBFACIN.SCX" WITH lcTranType
*! B610857,2 MMT 09/14/2014 modify Send Orders to use the client forms not shared ones[T20140908.0019][End]
*!*************************************************************
*! Name : lfOpenFiles
*! Auth : Hend Ghanem
*! Date : 08/21/2006
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
  =gfOpentable(oAriaApplication.DataDir+'RETHDR',oAriaApplication.DataDir+'RETHDRA','SH')
  =gfOpentable(oAriaApplication.DataDir+'ORDHDR',oAriaApplication.DataDir+'ORDHDR','SH')
  =gfOpentable(oAriaApplication.DataDir+'CUSTOMER',oAriaApplication.DataDir+'CUSTOMER','SH')
  =gfOpentable(oAriaApplication.DataDir+'CitTrnLn',oAriaApplication.DataDir+'CitTrnLn','SH')
  
*!*************************************************************
*! Name : lfFormInit
*! Auth : Hend Ghanem
*! Date : 08/21/2006
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
    *=gfOpentable(oAriaApplication.DataDir+'CitTrnLn',oAriaApplication.DataDir+'CitTrnLn','SH')
    *-- Create CIT Send Orders Temp. File
    loFormSet.lcTmpCit = gfTempName()
    CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lcTmpCit) (cFacCode C(6),BatchNo N(2),dDate D,Account C(5),ORDER C(6))
  ENDIF

  *-- Create transaction temp. file
  loFormSet.lcTempTran = gfTempName()
  CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lcTempTran) ;
    (TYPE C(1), Account C(5), TranNum C(6), cSelect C(1),InvDate D(8),Store C(8),Order C(6),FactAcct C(10),Ship N(7),TotalChg N(14,2),;
     CrDate D(8),RANO C(6),Reference C(30),TotCredit N(14,2),Season C(20),Division C(20),Status C(1),StName C(20),cStore C(8),Start D(8),Complete D(8),;
     OpenAmt N(7,2),ApprAmt N(7,2),lSelect L(1))
  INDEX ON cSelect+TYPE+Account+TranNum TAG 'SELECT'
  INDEX ON TYPE + TranNum TAG RangeSelct
  INDEX ON TYPE+Account+TranNum TAG (loFormSet.lcTempTran)  ADDITIVE
  
  SET RELATION TO 'M'+Account INTO CUSTOMER
  IF loFormSet.lcTranType = 'I'
    SET RELATION TO Account+TranNum INTO INVHDR ADDITIVE
    SET RELATION TO Account+TranNum INTO RETHDR ADDITIVE
  ELSE
    SET RELATION TO Account+'O'+TranNum INTO ORDHDR ADDITIVE
  ENDIF
  *-- Number of selected transactions
  *STORE 0 TO lnNoTrans

  loFormSet.lcOutFile = oAriaApplication.DataDir + 'CIT'+PADL(MONTH(oAriaApplication.SystemDate),2,'0')+PADL(DAY(oAriaApplication.SystemDate),2,'0')+'.NEW'
  IF FILE(oAriaApplication.DataDir+"MEMO.MEM")
    RESTORE FROM oAriaApplication.DataDir+"MEMO" ADDITIVE
  ENDIF

ENDFUNC

*!*************************************************************
*! Name : lfShow
*! Auth : Hend Ghanem
*! Date : 08/21/2006
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
  loFormSet.ariaForm1.Enabled = .T.

  DO CASE
    CASE loFormSet.activemode = 'S'
      WITH loFormSet.ariaForm1
        STORE .T. TO .kbFactor.enabled , .kbFactor.keytextbox.Enabled , .kbFactor.keyCmd.Enabled 
        STORE .F. TO .kBCustomer.enabled , .kBCustomer.keyCmd.Enabled , .kBCustomer.keytextbox.Enabled ,;
			         .txtCustFact.Enabled , .txtCustName.Enabled , .txtFactName.Enabled ,;
	  		         .txtOutFile.enabled , .cmdGetFile.enabled , .chkFTP.Enabled , .cboLastTran.Enabled ,;
			         .cmdProceed.Enabled , .cmdSelect.Enabled 
        STORE "" TO .kbFactor.keytextbox.value ,.txtFactName.value , .kBCustomer.keytextbox.value , .txtCustFact.Value , .txtCustName.Value
        .cboLastTran.value = '0'
        .chkFTP.value = .T.
        .txtOutFile.value = loFormSet.lcOutFile
      ENDWITH  

  ENDCASE 
  *!*************************************************************
  *! Name      : lfvFactor
  *! Auth      : Hend Ghanem
  *! Date      : 08/27/2006
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
    STORE .T. TO loFormSet.llInvoice , loFormSet.llCrMemo , loFormSet.llOrders
  ENDIF
  
  IF EMPTY(lcFactor)
    WITH loFormSet.ariaForm1
      STORE .F. TO .kBCustomer.enabled , .kBCustomer.keyCmd.Enabled , .kBCustomer.keytextbox.Enabled ,;
	  		       .txtOutFile.enabled , .cmdGetFile.enabled , .chkFTP.Enabled , .cboLastTran.Enabled ,;
			       .cmdSelect.Enabled 
    ENDWITH 
  ELSE
    WITH loFormSet.ariaForm1
      STORE .T. TO .kBCustomer.enabled , .kBCustomer.keyCmd.Enabled , .kBCustomer.keytextbox.Enabled ,;
	  		       .cmdGetFile.enabled , .chkFTP.Enabled , .cboLastTran.Enabled ,;
			       .cmdSelect.Enabled 
    ENDWITH 
  ENDIF
  loFormSet.AriaForm1.txtFactName.VALUE = IIF(!EMPTY(lcFactor),SycFact.cFacComp,loFormSet.AriaForm1.txtFactName.VALUE)
  
  SELECT (lnAlias)
  llBrowse = .F.
  RETURN !EMPTY(lcFactor)

  *!*************************************************************
  *! Name      : lfvCustomer
  *! Developer : Hend Ghanem
  *! Date      : 08/27/2006
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
    STORE .T. TO loFormSet.llInvoice , loFormSet.llCrMemo , loFormSet.llOrders
  ENDIF
  
  lcCustFact = IIF(EMPTY(CUSTOMER.FactAcct),'**NEW**',CUSTOMER.FactAcct)
  lcCustName = CUSTOMER.BtName

  loFormSet.AriaForm1.txtCustFact.VALUE = lcCustFact
  loFormSet.AriaForm1.txtCustName.VALUE = lcCustName
  llBrowse = .F.

  *!*************************************************************
  *! Name      : lfvSelTrans
  *! Developer : Hend Ghanem
  *! Date      : 08/27/2006
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
    =SEEK('I')
    DELETE REST WHILE TYPE = 'I'
    SELECT INVHDR
    =gfSetOrder('INVHDRA')
    =GFSEEK(ALLTRIM(lcCustomer))
    SCAN REST WHILE Account+Invoice = ALLTRIM(lcCustomer) FOR cFacCode = lcFactor AND STATUS <>'V'
      INSERT INTO (loFormSet.lcTempTran) (cSelect,TYPE,Account,TranNum,invdate,store,order,ship,totalchg,lSelect);
           VALUES ('N','I',INVHDR.Account,INVHDR.Invoice,INVHDR.invdate,INVHDR.Store,INVHDR.Order,INVHDR.ship,INVHDR.totalchg,.F.)
      SELECT CUSTOMER
      =GFSEEK(PADR(lcCustomer,5))  
      SELECT (loFormSet.lcTempTran)
      REPLACE FactAcct WITH IIF(empty(CUSTOMER.factacct),'**NEW**',CUSTOMER.factacct)
    ENDSCAN
    loFormSet.llInvoice = .F.
  ENDIF
  
  IF loFormSet.llCrMemo AND loFormSet.lcTranType = 'I'
    SELECT (loFormSet.lcTempTran)
    =SEEK('C')
    DELETE REST WHILE TYPE = 'C'
    SELECT RETHDR
    =gfSetOrder('RETHDRA')
    =GFSEEK(ALLTRIM(lcCustomer))
    SCAN REST WHILE Account+CrMemo = ALLTRIM(lcCustomer) FOR cFacCode = lcFactor AND STATUS <>'V'
      INSERT INTO (loFormSet.lcTempTran) (cSelect,TYPE,Account,TranNum,CrDate,RANO,Reference,TotCredit,lSelect);
           VALUES ('N','C',RETHDR.Account,RETHDR.CrMemo,RETHDR.CrDate,RETHDR.RANO,RETHDR.Reference,RETHDR.TotCredit,.F.)
      SELECT CUSTOMER
      =GFSEEK(PADR(lcCustomer,5))  
      SELECT (loFormSet.lcTempTran)
      REPLACE FactAcct WITH IIF(empty(CUSTOMER.factacct),'**NEW**',CUSTOMER.factacct)      
    ENDSCAN
    loFormSet.llCrMemo = .F.
  ENDIF
  
  IF loFormSet.llOrders AND loFormSet.lcTranType = 'O'
    SELECT (loFormSet.lcTempTran)
    =SEEK('O')
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
      INSERT INTO (loFormSet.lcTempTran) (cSelect,TYPE,Account,TranNum,Season,Division,Status,cStore,Start,Complete,OpenAmt,ApprAmt,lSelect);
             VALUES ('N','O',ORDHDR.Account,ORDHDR.ORDER,gfCodDes(OrdHdr.Season,'SEASON'),gfCodDes(OrdHdr.cDivision,'CDIVISION'),OrdHdr.Status,;
             IIF(OrdHdr.Multi='Y','*MULTI*',OrdHdr.Store),OrdHdr.Start,OrdHdr.Complete,OrdHdr.OpenAmt,OrdHdr.ApprAmt,.F.)
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
  *! B610857,2 MMT 09/14/2014 modify Send Orders to use the client forms not shared ones[T20140908.0019][Start]
  *DO FORM oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleID+ "\EBSNDTR.SCX" WITH loFormSet,'EBFACIN',loFormSet.lcTranType
  DO FORM oAriaApplication.clientscreenhome+oAriaApplication.ActiveModuleID+ "\EBSNDTR.SCX" WITH loFormSet,'EBFACIN',loFormSet.lcTranType
  *! B610857,2 MMT 09/14/2014 modify Send Orders to use the client forms not shared ones[T20140908.0019][End]
  IF loFormSet.lcTranType <> 'O'
    loFormSet.lcTranType = 'I'
  ENDIF  

  SET ORDER TO TAG 'SELECT' IN (loFormSet.lcTempTran)
  IF SEEK('Y',loFormSet.lcTempTran)
    loFormSet.Ariaform1.cmdProceed.ENABLED = .T.
  ELSE
    loFormSet.Ariaform1.cmdProceed.ENABLED = .F.
  ENDIF
  SET ORDER TO TAG (loFormSet.lcTempTran) IN (loFormSet.lcTempTran)

*!*************************************************************
*! Name : lfGetFile
*! Developer : Hend Ghanem
*! Date      : 08/27/2006
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
  loFormSet.Ariaform1.txtOutFile.value = lcOutFile
  loFormSet.lcOutFile = lcOutFile
  SAVE TO oAriaApplication.DataDir+"MEMO" ALL LIKE lcOutFile
ENDIF  


*!*************************************************************
*! Name      : lfvProc
*! Developer : Hend Ghanem
*! Date      : 08/27/2006
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
*B610857,1 MMT 09/17/2014 modify Send Orders to CIT program to use 4 chars from client no[T20140908.0019][Start]
*loFormSet.lcClientNo = SycFact.cClientNo
loFormSet.lcClientNo = SUBSTR(SycFact.cClientNo,1,4)
*B610857,1 MMT 09/17/2014 modify Send Orders to CIT program to use 4 chars from client no[T20140908.0019][End]
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
    SCATTER MEMVAR memo
    SELECT CitTrnLn
    =gfAppend()
    GATHER MEMVAR memo
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
*! Auth : Hend Ghanem
*! Date : 08/27/2006
*!**************************************************************
*! Synopsis : Write an output Invoices/Credits text file to be send to CIT.
*!*************************************************************
*! Called from : 
*!         Procedures : EBFACIN
*!*************************************************************
*! Calls : 
*!         FUNCTIONS : lfAddSecRec
*!                     lfAddCusRec
*!                     lfAddInvRec
*!                     lfAddRetRec
*!                     lfAddSubRec
*!                     lfAddTrnRec
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfWrtInv
LPARAMETERS loFormSet

PRIVATE lcCustomer

WAIT 'Creating outbound Invoice/Credit file...' WINDOW NOWAIT

*-- Initialize total number of Name/Address records, Invoice records and
*-- Credit records
STORE 0 TO lnTCusRec, lnTInvRec, lnTRetRec

*-- Initialize total invoice amount, total credit amount and total net amount
STORE 0 TO lnTInvAmt, lnTRetAmt, lnTTotAmt
SELECT (loFormSet.lcTempTran)
SET ORDER TO TAG SELECT

*-- Add Security record
IF !loFormSet.ariaform1.chkFTP.value
  = lfAddSecRec(loFormSet)
ENDIF
*-- First write Invoice records
IF SEEK('YI')
  *-- Initialize subtotal number of Name/Address records, Invoice records
  *-- and Credit records
  STORE 0 TO lnSCusRec, lnSInvRec, lnSRetRec
  
  *-- Initialize subtotal invoice amount, subtotal credit amount and 
  *-- subtotal net amount
  STORE 0 TO lnSInvAmt, lnSRetAmt, lnSTotAmt

  DO WHILE cSelect+Type+Account+TranNum = "YI"
    lcCustomer = Account
    SELECT Customer
    IF gfSEEK('M'+lcCustomer) 
      IF !Empty(Customer.FactAcct)
        lcCustNum = PADL(ALLTRIM(SUBSTR(Customer.FactAcct,1,7)),7,'0')
      ELSE
        lcCustNum = "9999999"
      ENDIF
    ENDIF  
    SELECT (loFormSet.lcTempTran)      
    
    *-- Add Name/Address Record Type "A"
    =lfAddCusRec(loFormSet)
    *-- Increament number of Name/Addres records
    lnSCusRec = lnSCusRec + 1
    
    SCAN REST WHILE cSelect+Type+Account+TranNum = "YI"+lcCustomer
      *-- Add Invoice Record Type "D"
      =lfAddInvRec(loFormSet)
      *-- Increament number of invoice records
      lnSInvRec = lnSInvRec + 1
    ENDSCAN
  ENDDO
  =lfAddSubRec(loFormSet)
  *!* *-- Add Sub Total Record Type "S"
   *-- Increament total number of records and total amount
  lnTCusRec = lnTCusRec + lnSCusRec
  lnTInvRec = lnTInvRec + lnSInvRec
  lnTRetRec = lnTRetRec + lnSRetRec
  lnTInvAmt = lnTInvAmt + lnSInvAmt
  lnTRetAmt = lnTRetAmt + lnSRetAmt
ENDIF  
SELECT (loFormSet.lcTempTran)
SET RELATION TO
*-- Second write Credits records
IF SEEK("YC")
  *-- Initialize subtotal number of Name/Address records, Invoice records 
  *-- and Credit records
  STORE 0 TO lnSCusRec, lnSInvRec, lnSRetRec
  
  *-- Initialize subtotal invoice amount, subtotal credit amount and 
  *-- subtotal net amount
  STORE 0 TO lnSInvAmt, lnSRetAmt, lnSTotAmt

  *-- Set relation to return header file
  SET RELATION TO Account+TranNum INTO RetHdr
  
  *-- Set relation to invoice header file
  SELECT RetHdr
  SET RELATION TO Account+Invoice INTO InvHdr
  SELECT (loFormSet.lcTempTran)
  DO WHILE cSelect+Type+Account+TranNum = "YC"
    lcCustomer = Account
    *-- Get customer factor number
    SELECT Customer
    IF gfSEEK('M'+lcCustomer) 
      IF !Empty(Customer.FactAcct)
        lcCustNum = PADL(ALLTRIM(SUBSTR(Customer.FactAcct,1,7)),7,'0')
      ELSE
        lcCustNum = "9999999"
      ENDIF
    ENDIF  
    SELECT (loFormSet.lcTempTran)
    *-- Add Name/Address Record Type "A"
    =lfAddCusRec(loFormSet)
    *-- Increament number of Name/Addres records
    lnSCusRec = lnSCusRec + 1

    SCAN REST WHILE cSelect+Type+Account+TranNum = "YC"+lcCustomer
      *-- Add Credit Record Type "C"
      =lfAddRetRec(loFormSet)
      *-- Increament number of invoice records
      lnSRetRec = lnSRetRec + 1
    ENDSCAN
  ENDDO
  *-- Add Sub Total Record Type "S"
  =lfAddSubRec(loFormSet)

  *-- Increament total number of records and total amount
  lnTCusRec = lnTCusRec + lnSCusRec
  lnTInvRec = lnTInvRec + lnSInvRec
  lnTRetRec = lnTRetRec + lnSRetRec
  lnTInvAmt = lnTInvAmt + lnSInvAmt
  lnTRetAmt = lnTRetAmt + lnSRetAmt
ENDIF  
lnTTotAmt = lnTInvAmt - lnTRetAmt
*-- Add Transmission total record Type "T"
=lfAddTrnRec(loFormSet)

*!*************************************************************
*! Name : lfAddSecRec
*! Auth : Hend Ghanem
*! Date : 08/27/2006
*!*************************************************************
*! Synopsis : Write the security record in the output orders text file
*!*************************************************************
*! Called from : 
*!         FUNCTION : lfWrtOrder(),lfWrtInv()
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddSecRec
LPARAMETERS loFormSet

PRIVATE lcSegLine

lcSegLine = '$$ADD'
*-- Client ID
lcSegLine = lcSegLine + SPACE(1) + 'ID=' + 'EP003F'
*-- BATCH ID
lcSegLine = lcSegLine + SPACE(1) + "BID='" + SUBSTR(loFormSet.lcBatchId,1,6)+"'"
*-- Password
lcSegLine = lcSegLine + SPACE(1) + "PASSWORD=" + SUBSTR(loFormSet.lcPassWord,1,4)
= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name : lfAddCusRec
*! Auth : Wael Aly MOhamed
*! Date : 12/30/99
*!*************************************************************
*! Synopsis : Write a customer record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : EBFACIN
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddCusRec
LPARAMETERS loFormSet

PRIVATE lcSegLine

lcSegLine = ''

*-- Client Number
lcSegLine = lcSegLine + loFormSet.lcClientNo
*-- Line Number
lcSegLine = lcSegLine + SPACE(2)
*-- Record Type
lcSegLine = lcSegLine + "A"
*-- Customer Number
lcSegLine = lcSegLine + PADR(lcCustomer,15," ")
*-- Customer Name
lcSegLine = lcSegLine + SUBSTR(Customer.BtName,1,30)
*-- Customer Address Line 1
lcSegLine = lcSegLine + SUBSTR(Customer.cAddress12,1,30)
*-- Customer Address Line 2
lcSegLine = lcSegLine + SUBSTR(Customer.cAddress22,1,30)
*-- Customer Address City
lcSegLine = lcSegLine + SUBSTR(Customer.cAddress32,1,17)
*-- Customer Address State
lcSegLine = lcSegLine + SUBSTR(Customer.cAddress42,1,2)
*-- Zip Code
lcSegLine = lcSegLine + SUBSTR(Customer.cAddress52,1,9)
*-- Country 
lcSegLine = lcSegLine + LEFT(Customer.cAddress62,17) + SPACE(3)
*- phone
lcSegLine = lcSegLine + LEFT(Customer.PHONE1,10)
*-- Filer
= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name : lfAddInvRec
*! Auth : Hend Ghanem
*! Date : 08/27/2006
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

PRIVATE lcSegLine, lcTmpDate, lcInvDate, lcFTermRate, lcFTermDays, lcEOM
lcSegLine = ''
*-- Client Number
lcSegLine = lcSegLine + loFormSet.lcClientNo
*-- Trade Style + Space(1) Blank
lcSegLine = lcSegLine + Space(2)
*-- Record Type
lcSegLine = lcSegLine + "D"
*-- Customer Number
lcSegLine = lcSegLine + Padr(lcCustomer,15," ")
*-- Invoice Number
lcSegLine = lcSegLine + PADL(EVALUATE(loFormSet.lcTempTran+'.TranNum'),8,'0')
*-- Filer
lcSegLine = lcSegLine + SPACE(7)
*-- Invoice Amount
lcAmount= STRTRAN(STR(InvHdr.TotalChg,11,2) ,' ','0')
lcAmount= STRTRAN(lcAmount ,'.','')
lcSegLine = lcSegLine + lcAmount

*- Increament invoice amount and total net amount
lnSInvAmt = lnSInvAmt + InvHdr.TotalChg
lnSTotAmt = lnSTotAmt + InvHdr.TotalChg

*-- Invoice Date
lcInvDate = PADL(MONTH(InvHdr.InvDate),2,'0') +;
            PADL(DAY  (InvHdr.InvDate),2,'0') +;
            RIGHT(STR(YEAR(InvHdr.InvDate),4),2)
lcSegLine = lcSegLine + lcInvDate

*-- As Of Date
lcSegLine = lcSegLine + SPACE(6)
*-- Client Terms Code
lcSegLine = lcSegLine + PADR(RIGHT(ALLTRIM(InvHdr.cTermcode) , 3) , 3 , " ")
*-- Merchandise Code
lcSegLine = lcSegLine + IIF(InvHdr.Discount = 0 , SPACE(1) , '1')
*-- Merchandise Amount
lcStr = ALLTRIM(STR(InvHdr.ShipAmt))
lcStr = STRTRAN(lcStr , "." , "")
lcStr = PADL(RIGHT(lcStr,10),10,"0")
lcSegLine = lcSegLine + IIF( InvHdr.Discount = 0 , Space(10) , lcStr)

*-- Store Number
lcSegLine = lcSegLine + PADL(ALLTRIM(InvHdr.Store) , 5  , ' ')
*-- Cust Purchase Order#
lcSegLine = lcSegLine + PADR(ALLTRIM(InvHdr.CustPo), 22 , ' ')
*-- Cust Dept {OPtional}
lcSegLine = lcSegLine + Space(6)
*-- Risk Code
lcSegLine = lcSegLine + SPACE(1)
*-- Extra Days
lcSegLine = lcSegLine + SPACE(3)
*-- Extra Days Code
lcSegLine = lcSegLine + SPACE(1)
*-- Discount Type Code
lcSegLine = lcSegLine + SPACE(1)
*-- Discount Amount (10)
lcSegLine = lcSegLine + PADL("" , 10 , "0")
*-- Credit Memo INvoice #
lcSegLine = lcSegLine + SPACE(8)
*-- Futur Use
lcSegLine = lcSegLine + SPACE(40)

= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name : lfAddRetRec
*! Auth : Wael Aly MOhamed
*! Date : 12/30/99
*!*************************************************************
*! Synopsis : Write a credit record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : EBFACIN
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddRetRec
LPARAMETERS loFormSet

PRIVATE lcSegLine, lcTmpDate, lcRetDate, lcFTermRate, lcFTermDays, lcEOM

lcSegLine = ''
*-- Client Number
lcSegLine = lcSegLine + loFormSet.lcClientNo
*-- Trade Style
lcSegLine = lcSegLine + Space(2)
*-- Record Type
lcSegLine = lcSegLine + "R"
*-- Client Customer Number
lcSegLine = lcSegLine + Padr(lcCustomer,15)
*-- Client Order Number
lcSegLine = lcSegLine + PADL(ALLTRIM(RetHdr.Order),22,'0')
*-- Credit Amount
lcAmount= STRTRAN(STR(RetHdr.TotCredit,9,2) ,' ','0')
lcAmount= STRTRAN(lcAmount,'.','')
lcSegLine = lcSegLine + lcAmount
*-- Bill & Hold Code ?
lcSegLine = lcSegLine + Space(1)
*-- Start Ship Date ?
lcSegLine = lcSegLine + Space(6)
*-- Filer
lcSegLine = lcSegLine + SPACE(7)
*-- Invoice Number
lcSegLine = lcSegLine + PADL(RetHdr.Invoice,7,'0')
*-- Entry Code
lcSegLine = lcSegLine + '22'
*-- Risk Code
lcSegLine = lcSegLine + SPACE(1)
*-- Filer
lcSegLine = lcSegLine + SPACE(2)
*-- Store Number
lcSegLine = lcSegLine + PADL(ALLTRIM(RetHdr.Store),4,'0')
*-- Credit Date
lcRetDate = PADL(MONTH(RetHdr.CrDate),2,'0') + PADL(DAY(RetHdr.CrDate),2,'0') +;
            RIGHT(STR(YEAR(RetHdr.CrDate),4),2)

lcSegLine = lcSegLine + lcRetDate
*-- Filer
lcSegLine = lcSegLine + '0'
*-- Credit Amount
lcAmount= STRTRAN(STR(RetHdr.TotCredit,9,2) ,' ','0')
lcAmount= SUBSTR(lcAmount,1,6) + SUBSTR(lcAmount,8,2)
lcSegLine = lcSegLine + lcAmount

*- Increament Credit amount and total net amount
lnSRetAmt = lnSRetAmt + RetHdr.TotCredit
lnSTotAmt = lnSTotAmt + RetHdr.TotCredit

*-- Cust Purchase Order#
lcSegLine = lcSegLine + PADR(ALLTRIM(RetHdr.CustPo),15,' ')

*-- Terms Information
lcFTermDays = SPACE(3)
lcSTermDays = SPACE(3)
lcEOM       = SPACE(1)
lcExtraDays = SPACE(3)


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
STORE ' ' TO lcTEOM
=gfRltFld(InvHdr.cTermCode,@laTRltFld,'CTERMCODE')

lcEOM = IIF(lcTEOM= 'Y', 'E', lcEOM)
*-- Extra Days
lcSegLine = lcSegLine + lcExtraDays
*-- As Of Date
lcSegLine = lcSegLine + SPACE(6)
*-- Filler
lcSegLine = lcSegLine + SPACE(1)
*-- Interest Code
lcSegLine = lcSegLine + IIF(.F.,'Z',SPACE(1))
*-- Filler
lcSegLine = lcSegLine + SPACE(12)
*-- Net Days
lcSegLine = lcSegLine + SPACE(3)
*-- Filler
lcSegLine = lcSegLine + SPACE(9)
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
LPARAMETERS loFormSet

PRIVATE lcSegLine, lcInvAmt, lcRetAmt, lcTotAmt

*-- Increament the assignment number
loFormSet.lnAssignNo = loFormSet.lnAssignNo + 1
lcSegLine = ''
*-- Client Number
lcSegLine = lcSegLine + loFormSet.lcClientNo
*-- Trade Style [Always = "99"]
lcSegLine = lcSegLine + "99"
*-- Record Type
lcSegLine = lcSegLine + "S"
*-- Client Customer Number
lcSegLine = lcSegLine + PADR("",15,"9")
*-- Total number of Name/Address records
lcSegLine = lcSegLine + PADL(lnSCusRec,6,'0')
*-- Total number of Invoice records
lcSegLine = lcSegLine + PADL(lnSInvRec,6,'0')
*-- Total number of Credit records
lcSegLine = lcSegLine + PADL(lnSRetRec,6,'0')

*-- Total invoice amount
lcInvAmt = STRTRAN(STR(lnSInvAmt , 13 , 2) , ' ' , '0')
lcInvAmt = STRTRAN(lcInvAmt , '.' , '')
lcSegLine = lcSegLine + lcInvAmt

*-- Total credits amount
lcRetAmt = STRTRAN(STR(lnSRetAmt , 13 , 2) , ' ' , '0')
lcRetAmt = STRTRAN(lcRetAmt , '.' , '')
lcSegLine = lcSegLine + lcRetAmt

*-- Assignment number
lcSegLine = lcSegLine + PADL(ALLTRIM(STR(loFormSet.lnAssignNo)),4,'0')

*-- Assignment Date
lcAssignDate = PADL(MONTH(Date()),2,'0') + PADL(DAY(Date()),2,'0') +;
               RIGHT(STR(YEAR(Date()),4),2)
lcSegLine = lcSegLine + lcAssignDate              

*-- Factoring Free Code
lcSegLine = lcSegLine + "0"
*-- Future USe
lcSegLine = lcSegLine + Space(95)
= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name : lfAddTrnRec
*! Auth : Hend Ghanem
*! Date : 08/27/2006
*!*************************************************************
*! Synopsis : Write the transmission total record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : EBFACIN
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddTrnRec
LPARAMETERS loFormSet

PRIVATE lcSegLine, lcInvAmt, lcRetAmt, lcTotAmt

lcSegLine = ''
*-- Client Number
lcSegLine = lcSegLine + "9999"
*-- Trade Style
lcSegLine = lcSegLine + "99"
*-- Record Type
lcSegLine = lcSegLine + "T"
*-- Client Customer Number
lcSegLine = lcSegLine + PADR("",15,"9")
*-- Total number of Name/Address records
lcSegLine = lcSegLine + PADL(lnTCusRec,6,'0')
*-- Total number of Invoice records
lcSegLine = lcSegLine + PADL(lnTInvRec,6,'0')
*-- Total number of Credit records
lcSegLine = lcSegLine + PADL(lnTRetRec,6,'0')
*-- Total (invoice/Credit Order) amount
lcInvAmt = STRTRAN(STR(lnTInvAmt,13,2) ,' ','0')
lcInvAmt = STRTRAN(lcInvAmt ,'.','')
lcSegLine = lcSegLine + lcInvAmt
*-- Total Credits Amount
lcRetAmt= STRTRAN(STR(lnTRetAmt,13,2) ,' ','0')
lcRetAmt= STRTRAN(lcRetAmt ,'.','')
lcSegLine = lcSegLine + lcRetAmt
*-- Transmision Date ??
*-- Invoice Date
lcTransDate = PADL(MONTH(Date()),2,'0') + PADL(DAY(Date()),2,'0') +;
              RIGHT(STR(YEAR(Date()),4),2)
lcSegLine = lcSegLine + lcTransDate
*-- Future Use
lcSegLine = lcSegLine + SPACE(100)
= FPUTS(lnOutFile,lcSegLine)


*!*************************************************************
*! Name      : lfWrtOrder
*! Developer : WAM
*! Date      : 12/30/1999
*! Purpose   : Write outbound Order
*!*************************************************************
*! Calls     : lfAddSecRec(),lfAddCusRec(),lfAddOrdRec(),lfAddOrdSub(),lfAddOrdTot()
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
*-- Add Security record
=lfAddSecRec(loFormSet)

*-- Initialize subtotal number of Name/Address records, Order records
STORE 0 TO lnSCusRec, lnSOrdRec, lnSOrdAmt
STORE 0 TO lnTCusRec, lnTOrdRec, lnTOrdAmt
STORE 0 TO lnNewCust

SELECT (loFormSet.lcTempTran)
SET ORDER TO TAG SELECT
=SEEK('YO')
DO WHILE cSelect+Type+Account+TranNum = "YO"
  lcCustomer = Account
  SELECT CUSTOMER
  *-- Get customer factor number
  IF gfSEEK('M'+lcCustomer) 
    IF Empty(Customer.FactAcct)
      lnNewCust = lnNewCust + 1
      lcCustNum = "999" + PADL(lnNewCust,4,'0')
    ELSE
      lcCustNum = PADL(ALLTRIM(SUBSTR(Customer.FactAcct,1,7)),7,'0')
    ENDIF
  ENDIF  
  SELECT (loFormSet.lcTmpCit)
  ZAP
  
  SELECT (loFormSet.lcTempTran)
  *-- Add Name/Address Record Type "A"
  =lfAddCusRec(loFormSet)
  *-- Increament number of Name/Addres records
  lnSCusRec = lnSCusRec + 1
  
  *-- Add Order Record for this customer Type "D"    
  SCAN WHILE cSelect+Type+Account+TranNum = "YO"+lcCustomer
    =lfAddOrdRec(loFormSet)
    
    *-- Update the CitTrnLn Temp. File
    INSERT INTO (loFormSet.lcTmpCit) (cFacCode,BatchNo,dDate,Account,Order) VALUES ;
    (lcFactor,loFormSet.lnBatchNo,oAriaApplication.SystemDate,lcCustomer,OrdHdr.Order)

    *-- Increament number of order records
    lnSOrdRec = lnSOrdRec + 1
  ENDSCAN
ENDDO

*-- Add Sub Total Record Type "S"
=lfAddOrdSub(loFormSet)
  
*-- Increament total number of records and total amount
lnTCusRec = lnTCusRec + lnSCusRec
lnTOrdRec = lnTOrdRec + lnSOrdRec
lnTOrdAmt = lnTOrdAmt + lnSOrdAmt

*-- Add Transmission total record Type "T"
=lfAddOrdTot(loFormSet)
WAIT CLEAR

*!*************************************************************
*! Name : lfAddOrdRec
*! Auth : Hend Ghanem
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

PRIVATE lcSegLine, lcTmpDate, lcOrdDate, lcFTermRate, lcFTermDays, lcEOM

lcSegLine = ''

*01-- Client Number
lcSegLine = lcSegLine + loFormSet.lcClientNo

*03-- Trade Style
lcSegLine = lcSegLine +SPACE(1)
lcSegLine = lcSegLine +SPACE(1)
*04-- Record Type
lcSegLine = lcSegLine +"R"

*05-- Customer Number
lcSegLine = lcSegLine + PADR(lcCustomer,15)

*06-- Client Control Key
lcSegLine = lcSegLine + PADR(EVALUATE(loFormSet.lcTempTran+'.TranNum'),22)

*07-- Order Amount
lcSegLine = lcSegLine + PADL(ROUND(OrdHdr.BookAmt,0),8,'0')

lnSOrdAmt = lnSOrdAmt + ROUND(OrdHdr.BookAmt,0)

*08-- Order Bill And Hold Indicator
lcSegLine = lcSegLine + SPACE(1)

*09-- Order Start Date
lcOrdDate = PADL(MONTH(OrdHdr.Start),2,'0') + PADL(DAY(OrdHdr.Start),2,'0') +;
             RIGHT(STR(YEAR(OrdHdr.Start),4),2)
lcSegLine = lcSegLine + lcOrdDate

*-- Terms Information
lcFTermDays = SPACE(3)

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
STORE ' ' TO lcTEOM
*B608549,1 WAM 05/11/2008 Send the proper payment terms information [T20080424.0010]
*=gfRltFld(InvHdr.cTermCode,@laTRltFld,'CTERMCODE')
=gfRltFld(OrdHdr.cTermCode,@laTRltFld,'CTERMCODE')
*B608549,1 WAM 05/11/2008 (End)

IF lnDiscDays <> 0
  *-- First Terms days
  lcFTermDays = PADL(INT(lnDiscDays),3,'0')
ELSE
  IF lnDaysDue <> 0
    lcFTermDays = PADL(INT(lnDaysDue),3,'0')
  ENDIF    
ENDIF    

*10 -- First Terms Days
lcSegLine = lcSegLine + lcFTermDays

*11-- Order Completion Date
lcOrdDate = PADL(MONTH(OrdHdr.Complete),2,'0') + PADL(DAY(OrdHdr.Complete),2,'0') +;
             RIGHT(STR(YEAR(OrdHdr.Complete),4),2)
lcSegLine = lcSegLine + lcOrdDate

lcSegLine = lcSegLine + SPACE(102)
= FPUTS(lnOutFile,lcSegLine)
*!*************************************************************
*! Name : lfAddOrdSub
*! Auth : Hend Ghanem
*! Date : 08/27/2006
*!*************************************************************
*! Synopsis : Write the subtotal record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : lfWrtOrder
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddOrdSub
LPARAMETERS loFormSet

PRIVATE lcSegLine

lcSegLine = ''

*01-- Client Number
lcSegLine = lcSegLine + loFormSet.lcClientNo

*02-- Trade Style
lcSegLine = lcSegLine + '99'

*03-- Record Type
lcSegLine = lcSegLine + "S"

*04-- Customer Number
lcSegLine = lcSegLine + REPLICATE('9',15)

*05-- Total number of Name/Address records
lcSegLine = lcSegLine + PADL(lnSCusRec,6,'0')

*06-- Total number of Order records
lcSegLine = lcSegLine + PADL(lnSOrdRec,6,'0')

*07-- Filler
lcSegLine = lcSegLine + SPACE(6)

*08-- Total Order amount
lcSegLine = lcSegLine + PADL(lnSOrdAmt,12,'0')

*09-- Filer
lcSegLine = lcSegLine + SPACE(118)

= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name : lfAddOrdTot
*! Auth : Hend Ghanem
*! Date : 08/27/2006
*!*************************************************************
*! Synopsis : Write the transmission total record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : EBFACIN
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddOrdTot
LPARAMETERS loFormSet

PRIVATE lcSegLine

lcSegLine = ''
*03-- Client Number
lcSegLine = lcSegLine + '9999'

*-- Trade style
lcSegLine = lcSegLine + '99'

*05-- Record Type
lcSegLine = lcSegLine + "T"

*04-- Customer Number
lcSegLine = lcSegLine + REPLICATE('9',15)

*06-- Total number of Name/Address records
lcSegLine = lcSegLine + PADL(lnTCusRec,6,'0')

*07-- Total number of Order records
lcSegLine = lcSegLine + PADL(lnTOrdRec,6,'0')

*08-- Filler
lcSegLine = lcSegLine + SPACE(6)

*09-- Total Order amount
lcSegLine = lcSegLine + PADL(lnTOrdAmt,12,'0')

*11-- Filer
lcSegLine = lcSegLine + SPACE(12)

*-- Transmission date
lcOrdDate = PADL(MONTH(DATE()),2,'0') + PADL(DAY(DATE()),2,'0') +;
             RIGHT(STR(YEAR(DATE()),4),2)
lcSegLine = lcSegLine + lcOrdDate

*-- Filler
lcSegLine = lcSegLine + SPACE(100)

= FPUTS(lnOutFile,lcSegLine)