*:*********************************************************************************
*: Program file  : EBFACID.PRG
*: Program desc. : Send Detail Invoice/Credit Memo and Sales Orders to CIT
*: For screen    : Menu
*:        System : Aria4 XP.
*:        Module : Accounts Receivable (AR).
*:     Developer : Waleed Hamed (WLD)
*:     Entry     : Convert from aria27 to Aria4
*:*********************************************************************************
*! Passed Parameters  :  lcTranType
*!                       'I' : Send Detail Invoice/Credit Memos
*!                       'O' : Send Sales Orders
*:*********************************************************************************
*: Modifications :
*! B608065,1 HIA 04/29/2007 review and fix the 'D' record 
*! B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [T20100526.0007]
*:*********************************************************************************
PARAMETER lcTranType

*!B609276,1 MMT 05/27/2010 don't Erase temp. file after printing report[Start]
IF oAriaApplication.MULTIINST 
   lcParmLst = "lcTranType"
   =gfCallForm('EBFACID','AR',lcParmLst)
ELSE 
*!B609276,1 MMT 05/27/2010 don't Erase temp. file after printing report[End]
  DO FORM oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleID+ "\EBFACID.SCX" WITH lcTranType
*! B609276,1 MMT 05/27/2010 don't Erase temp. file after printing report[Start]
ENDIF
*!B609276,1 MMT 05/27/2010 don't Erase temp. file after printing report[End]

*!*************************************************************
*! Name : lfOpenFiles
*! Auth : Waleed Hamed (WLD)
*! Date : 11/26/2006
*!**************************************************************
*! Synopsis :
*!*************************************************************
*! Called from :
*!         Procedures : Init in EBFACID.scx
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
  =gfOpentable(oAriaApplication.DataDir+'CODES',oAriaApplication.DataDir+'CODES','SH')
  =gfOpentable(oAriaApplication.DataDir+'INVLINE',oAriaApplication.DataDir+'INVLINE','SH')
  =gfOpentable(oAriaApplication.DataDir+'STYLEUPC',oAriaApplication.DataDir+'STYLEUPC','SH')
  =gfOpentable(oAriaApplication.DataDir+'SPCK_LIN',oAriaApplication.DataDir+'SPCKLINS','SH')
  =gfOpentable(oAriaApplication.DataDir+'SCALE',oAriaApplication.DataDir+'SCALE','SH')

  *!*************************************************************
  *! Name : lfFormInit
  *! Auth : Waleed Hamed (WLD)
  *! Date : 11/26/2009
  *!**************************************************************
  *! Synopsis :
  *!*************************************************************
  *! Called from :
  *!         Procedures : Init in EBFACID.scx
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
    (TYPE C(1), Account C(5), TranNum C(6), cSelect C(1),InvDate D(8),STORE C(8),ORDER C(6),FactAcct C(10),Ship N(7),TotalChg N(14,2),;
    CrDate D(8),RANO C(6),REFERENCE C(30),TotCredit N(14,2),Season C(20),Division C(20),STATUS C(1),StName C(20),cStore C(8),START D(8),COMPLETE D(8),;
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
    *!B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [Start]
    loFormSet.lcOutFile = ADDBS(lcOutFile)+ 'CIT'+PADL(MONTH(oAriaApplication.SystemDate),2,'0')+PADL(DAY(oAriaApplication.SystemDate),2,'0')+'.NEW'
    ELSE
      loFormSet.lcOutFile = 'CIT'+PADL(MONTH(oAriaApplication.SystemDate),2,'0')+PADL(DAY(oAriaApplication.SystemDate),2,'0')+'.NEW'
    *!B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [End]
  ENDIF

ENDFUNC

*!*************************************************************
*! Name : lfShow
*! Auth : Waleed Hamed (WLD)
*! Date : 11/26/2006
*!**************************************************************
*! Synopsis :
*!*************************************************************
*! Called from :
*!         Procedures : Init in EBFACID.scx
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
          .txtOutFile.ENABLED , .cmdGetFile.ENABLED , .chkFTP.ENABLED , .cboLastTran.ENABLED ,;
          .cmdProceed.ENABLED , .cmdSelect.ENABLED
        STORE "" TO .kbFactor.keytextbox.VALUE ,.txtFactName.VALUE , .kBCustomer.keytextbox.VALUE , .txtCustFact.VALUE , .txtCustName.VALUE
        .cboLastTran.VALUE = '0'
        .chkFTP.VALUE = .T.
        *! B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [Start]
        *.txtOutFile.VALUE = loFormSet.lcOutFile
        .txtFlName.Enabled =.F.
        IF TYPE('loFormSet.lcOutFile') = 'C'
          .txtOutFile.value = JUSTPATH(loFormSet.lcOutFile)
          .txtFlName.Value = JUSTFNAME(loFormSet.lcOutFile)
        ENDIF
        *! B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [End]
      ENDWITH

  ENDCASE
  *!*************************************************************
  *! Name      : lfvFactor
  *! Auth      : Waleed Hamed (WLD)
  *! Date      : 11/26/2006
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
      STORE .F. TO .kBCustomer.ENABLED , .kBCustomer.keyCmd.ENABLED , .kBCustomer.keytextbox.ENABLED ,;
        .txtOutFile.ENABLED , .cmdGetFile.ENABLED , .chkFTP.ENABLED , .cboLastTran.ENABLED ,;
        .cmdSelect.ENABLED
      *! B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [Start]
	  STORE .F. TO .txtFlName.Enabled 
      *! B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [End]
    ENDWITH
  ELSE
    WITH loFormSet.ariaForm1
      STORE .T. TO .kBCustomer.ENABLED , .kBCustomer.keyCmd.ENABLED , .kBCustomer.keytextbox.ENABLED ,;
        .cmdGetFile.ENABLED , .chkFTP.ENABLED , .cboLastTran.ENABLED ,;
        .cmdSelect.ENABLED
      *! B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [Start]
	  STORE .T. TO .txtFlName.Enabled 
      *! B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [End]

    ENDWITH
  ENDIF
  loFormSet.AriaForm1.txtFactName.VALUE = IIF(!EMPTY(lcFactor),SycFact.cFacComp,loFormSet.AriaForm1.txtFactName.VALUE)

  SELECT (lnAlias)
  llBrowse = .F.
  RETURN !EMPTY(lcFactor)

  *!*************************************************************
  *! Name      : lfvCustomer
  *! Developer : Waleed Hamed (WLD)
  *! Date      : 11/26/2006
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
  *! Developer : Waleed Hamed (WLD)
  *! Date      : 11/26/2006
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
  *! B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [Start]
  *LPARAMETERS loFormSet
  PARAMETERS loFormSet
  *! B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [End]

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
      INSERT INTO (loFormSet.lcTempTran) (cSelect,TYPE,Account,TranNum,invdate,STORE,ORDER,ship,totalchg,lSelect);
        VALUES ('N','I',INVHDR.Account,INVHDR.Invoice,INVHDR.invdate,INVHDR.STORE,INVHDR.ORDER,INVHDR.ship,INVHDR.totalchg,.F.)
      SELECT CUSTOMER
      =GFSEEK(PADR(lcCustomer,5))
      SELECT (loFormSet.lcTempTran)
      REPLACE FactAcct WITH IIF(EMPTY(CUSTOMER.factacct),'**NEW**',CUSTOMER.factacct)
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
      INSERT INTO (loFormSet.lcTempTran) (cSelect,TYPE,Account,TranNum,CrDate,RANO,REFERENCE,TotCredit,lSelect);
        VALUES ('N','C',RETHDR.Account,RETHDR.CrMemo,RETHDR.CrDate,RETHDR.RANO,RETHDR.REFERENCE,RETHDR.TotCredit,.F.)
      SELECT CUSTOMER
      =GFSEEK(PADR(lcCustomer,5))
      SELECT (loFormSet.lcTempTran)
      REPLACE FactAcct WITH IIF(EMPTY(CUSTOMER.factacct),'**NEW**',CUSTOMER.factacct)
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

  *! B609276,1 MMT 05/27/2010 don't Erase temp. file after printing report[Start]
  IF oAriaApplication.MULTIINST 
    lcParmLst = "loFormSet,'EBFACIN',loFormSet.lcTranType"
    =gfCallForm('EBSNDTR','AR',lcParmLst)
  ELSE 
  *! B609276,1 MMT 05/27/2010 don't Erase temp. file after printing report[End]

    DO FORM oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleID+ "\EBSNDTR.SCX" WITH loFormSet,'EBFACID',loFormSet.lcTranType
  *! B609276,1 MMT 05/27/2010 don't Erase temp. file after printing report[Start]
  ENDIF
  *! B609276,1 MMT 05/27/2010 don't Erase temp. file after printing report[End]

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
  *! Developer : Waleed Hamed (WLD)
  *! Date      : 11/26/2006
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
  *!B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [Start]
  *lcOutFile = GETFILE('NEW','Select Output File')
  lcOldDefPth = FULLPATH('')
  IF !EMPTY(loFormSet.lcOutFile)
    lcOutFile = JUSTPATH(loFormSet.lcOutFile)
    SET DEFAULT TO (lcOutFile)
  ENDIF 
  lcOutFile =  GETDIR('', 'Output File Directory', 'Select Folder',64)
  *!B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [End]

  IF !EMPTY(lcOutFile)
    loFormSet.Ariaform1.txtOutFile.VALUE = lcOutFile
    *!B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [Start]
*!*	    loFormSet.lcOutFile = lcOutFile
*!*	    SAVE TO oAriaApplication.DataDir+"MEMO" ALL LIKE lcOutFile
    loFormSet.lcOutFile = ADDBS(lcOutFile)+'CIT'+PADL(MONTH(oAriaApplication.SystemDate),2,'0')+PADL(DAY(oAriaApplication.SystemDate),2,'0')+'.NEW'
    SAVE TO oAriaApplication.DataDir+"MEMO.MEM" ALL LIKE lcOutFile
    *!B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [End]
  ENDIF
*! B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [Start]
SET DEFAULT TO (lcOldDefPth)
*! B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [End]


  *!*************************************************************
  *! Name      : lfvProc
  *! Developer : Waleed Hamed (WLD)
  *! Date      : 11/26/2006
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
  loFormSet.lcBatchId  = 'D' +IIF(loFormSet.lcTranType='I','I','O') +loFormSet.lcClientNo

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
  *! Date : 11/26/2006
  *!**************************************************************
  *! Synopsis : Write an output Invoices/Credits text file to be send to CIT.
  *!*************************************************************
  *! Called from :
  *!         Procedures : EBFACID
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
  **WLD
  ** Initiate Color Variables for the Styles
  lnMajorLen = LEN(gfItemMask("PM"))
  DECLARE laItemSeg[1]
  llColor = .F.
  STORE 0 TO lnClrLen,lnClrPos
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnCount,1]='C'
      llColor = .T.
      lnClrLen = LEN(laItemSeg[lnCount,3])
      lnClrPos = laItemSeg[lnCount,4]
      EXIT
    ENDIF
  ENDFOR
  **
  *-- Initialize total invoice amount, total credit amount and total net amount
  STORE 0 TO lnTInvAmt, lnTRetAmt, lnTTotAmt
  SELECT (loFormSet.lcTempTran)
  SET ORDER TO TAG SELECT

  *-- Add Security record
  IF !loFormSet.ariaform1.chkFTP.VALUE
    = lfAddSecRec(loFormSet)
  ENDIF
  *-- First write Invoice records
  IF SEEK('YI')
    *-- Initialize subtotal number of Name/Address records, Invoice records
    *-- and Credit records
    STORE 0 TO lnSCusRec, lnSInvRec, lnSRetRec,lnSInvSum

    *-- Initialize subtotal invoice amount, subtotal credit amount and
    *-- subtotal net amount
    STORE 0 TO lnSInvAmt, lnSRetAmt, lnSTotAmt

    DO WHILE cSelect+TYPE+Account+TranNum = "YI"
      lcCustomer = Account
      SELECT Customer
      IF gfSEEK('M'+lcCustomer)
        IF !EMPTY(Customer.FactAcct)
          lcCustNum = PADL(ALLTRIM(SUBSTR(Customer.FactAcct,1,7)),7,'0')
        ELSE
          lcCustNum = "9999999"
        ENDIF
      ENDIF
      SELECT (loFormSet.lcTempTran)
      SCAN REST WHILE cSelect+TYPE+Account+TranNum = "YI"+lcCustomer
        *-- Add Name/Address Record Type "N"
        =lfAddCusRec(loFormSet)
        *-- Increament number of Name/Addres records
        lnSCusRec = lnSCusRec + 1

        STORE 0 TO lnSInvRec
        SELECT InvLine
        =gfSEEK(EVALUATE(loFormSet.lcTempTran+'.TranNum'),'InvLine')
        SCAN REST WHILE INVOICE+STR(LINENO,6) = EVALUATE(loFormSet.lcTempTran+'.TranNum')
          STORE 0 TO lnCount
          FOR lnCount=1 TO 8
            STORE '' TO lcUpc,lcSize,lcColor, lcSKU, lcClrDesc
            lcStr=PADL(lnCount,1,' ')
            IF Qty&lcStr. != 0
              IF gfSEEK(STYLE+lcStr,'StyleUpc')
                lcUpc = ALLTRIM(StyleUpc.cupcnum1 + StyleUpc.cupcnum2 + StyleUpc.cupcnum3)
              ENDIF
              lcColor = IIF(llColor,SUBSTR(STYLE,lnClrPos,lnClrLen),'')
              IF !EMPTY(lcColor)
                =gfSEEK('N' + PADR(lcColor,6) + 'N' + PADR('COLOR',10) , 'CODES')
                lcClrDesc = ALLTRIM(CODES.cDiscRep)
              ENDIF
              IF gfSEEK('S'+SCALE,'SCALE')
                lcSize = SCALE.SZ&lcStr.
              ENDIF
              SELECT Spck_Lin
              =gfSEEK("S"+InvLine.Account+InvLine.STYLE)
              LOCATE REST WHILE TYPE+account+STYLE+pack_id = "S"+InvLine.Account+InvLine.STYLE FOR Qty&lcStr. = 1
              lcSKU =IIF(FOUND(),SPCK_LIN.pack_id," ")
              SELECT InvLine
              *-- Increament number of invoice records
              lnSInvRec = lnSInvRec + 1
              *-- Add Invoice Line Item (detail)Record Type "I"
              =lfAddInvRec(loFormSet)
            ENDIF
          ENDFOR
        ENDSCAN
        SELECT (loFormSet.lcTempTran)
        *-- Increament number of invoice Summary records
        lnSInvSum = lnSInvSum + 1
        *-- Add Invoice Summary Record Type "D" or "C"
        =lfAddInvSum(loFormSet)
        lnSInvAmt = lnSInvAmt + INVHDR.TotalChg
        *lnSRetAmt = lnSRetAmt + INVHDR.TotalChg
      ENDSCAN
    ENDDO
    *-- Add Sub Total Record Type "S"
    *-- Increament total number of records and total amount
    lnTCusRec = lnTCusRec + lnSCusRec
    lnTInvRec = lnTInvRec + lnSInvSum
    lnTRetRec = lnTRetRec + lnSRetRec
    lnTInvAmt = lnTInvAmt + lnSInvAmt
    lnTRetAmt = lnTRetAmt + lnSRetAmt
    =lfAddSubRec(loFormSet)
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
    DO WHILE cSelect+TYPE+Account+TranNum = "YC"
      lcCustomer = Account
      *-- Get customer factor number
      SELECT Customer
      IF gfSEEK('M'+lcCustomer)
        IF !EMPTY(Customer.FactAcct)
          lcCustNum = PADL(ALLTRIM(SUBSTR(Customer.FactAcct,1,7)),7,'0')
        ELSE
          lcCustNum = "9999999"
        ENDIF
      ENDIF
      SELECT (loFormSet.lcTempTran)

      SCAN REST WHILE cSelect+TYPE+Account+TranNum = "YC"+lcCustomer
        *-- Add Name/Address Record Type "N"
        =lfAddCusRec(loFormSet)
        *-- Increament number of Name/Addres records
        lnSCusRec = lnSCusRec + 1

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
  *! Auth : Waleed Hamed (WLD)
  *! Date : 11/26/2006
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
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

  *!*************************************************************
  *! Name : lfAddCusRec
  *! Auth : Wael Aly MOhamed
  *! Date : 12/30/99
  *!*************************************************************
  *! Synopsis : Write a customer record in the output orders text file
  *!*************************************************************
  *! Called from :
  *!         Procedures : EBFACID
  *!*************************************************************
  *! Modifications : None.
  *!*************************************************************
FUNCTION lfAddCusRec
  LPARAMETERS loFormSet

  PRIVATE lcSegLine

  lcSegLine = ''

  *-- Client Number
  lcSegLine = lcSegLine + loFormSet.lcClientNo
  *-- Trade Style
  lcSegLine = lcSegLine + SPACE(1)
  *-- Record Type
  lcSegLine = lcSegLine + "N"
  *-- Client Customer Number
  lcSegLine = lcSegLine + PADR(lcCustomer,15," ")
  *-- Invoice Number
  lcSegLine = lcSegLine + PADL(EVALUATE(loFormSet.lcTempTran+'.TranNum'),8,'0')
  *-- Filler
  lcSegLine = lcSegLine + SPACE(7)
  *-- Customer "Bill To" Name
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.BtName,1,30),30)
  *-- Customer Address Line 1
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.cAddress12,1,30),30,' ')
  *-- Customer Address Line 2
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.cAddress22,1,30),30,' ')
  *-- Customer Address City
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.cAddress32,1,17),17,' ')
  *-- Customer Address State
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.cAddress42,1,2),2,' ')
  *-- Zip Code
  lcSegLine = lcSegLine + LEFT(CUSTOMER.cAddress52,10)
  *-- Country
  lcSegLine = lcSegLine + LEFT(CUSTOMER.cAddress62,17)
  *- phone
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.PHONE1,1,15),15,' ')
  *- Fax
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.Fax,1,15),15,' ')
  *- Duns
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.Duns,1,9),9,' ')
  *- EMail
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.Usr_Dfnd1,1,40),40,' ')
  *-- Customer "Ship To" Name
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.StName,1,30),30,' ')
  *-- Customer Address Line 1
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.cAddress1,1,30),30,' ')
  *-- Customer Address Line 2
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.cAddress2,1,30),30,' ')
  *-- Customer Address City
  lcSegLine = lcSegLine + LEFT(CUSTOMER.cAddress3,17)
  *-- Customer Address State
  lcSegLine = lcSegLine + LEFT(CUSTOMER.cAddress4,2)
  *-- Zip Code
  lcSegLine = lcSegLine + LEFT(CUSTOMER.cAddress5,10)
  *-- Country
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.cAddress6,1,17),17,' ')
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

  *!*************************************************************
  *! Name : lfAddInvRec
  *! Auth : Waleed Hamed (WLD)
  *! Date : 11/26/2006
  *!*************************************************************
  *! Synopsis : Write an invoice record in the output orders text file
  *!*************************************************************
  *! Called from :
  *!         Procedures : EBFACID
  *!*************************************************************
  *! Modifications : None.
  *!*************************************************************
FUNCTION lfAddInvRec
  LPARAMETERS loFormSet

  PRIVATE lcSegLine, lcTmpDate, lcInvDate, lcFTermRate, lcFTermDays, lcEOM
  lcSegLine = ''
  *-- Client Number
  lcSegLine = lcSegLine + loFormSet.lcClientNo
  *-- Trade Style
  lcSegLine = lcSegLine + SPACE(1)
  *-- Record Type
  lcSegLine = lcSegLine + "I"
  *-- Client Customer Number
  lcSegLine = lcSegLine + PADR(lcCustomer,15," ")
  *lcSegLine = lcSegLine + lcClientCust
  *-- Invoice Number
  lcSegLine = lcSegLine + PADL(EVALUATE(loFormSet.lcTempTran+'.TranNum'),8,'0')
  *--  Filler
  lcSegLine = lcSegLine + SPACE(7)
  *-- Sequential Line No.
  lcSegLine = lcSegLine + PADL(lnSInvRec,5,'0')
  *-- Quantity Invoiced
  lcSegLine = lcSegLine + PADL(InvLine.Qty&lcStr,10,'0')
  *-- Unit or Basis for Measurement Code
  lcSegLine = lcSegLine + 'EA'
  *-- Unit Price
  lcSegLine = lcSegLine + PADL(InvLine.Price,17,'0')
  *-- Basis of Unit Price Code
  lcSegLine = lcSegLine + "PE"
  *-- UPC Number
  lcSegLine = lcSegLine + PADR(lcUpc,20,' ')
  *-- Buyer's Catalog Number
  lcSegLine = lcSegLine + PADR(SUBSTR(lcSKU,1,20),20,' ')
  *-- Vendor Style Number
  lcSegLine = lcSegLine + PADR(SUBSTR(InvLine.STYLE,1,lnMajorLen),20,' ')
  *-- European Article Number
  lcSegLine = lcSegLine + IIF(LEN(lcUpc)=12,SPACE(20),PADR(lcUpc,20,' '))
  *-- Item Discription Line 1
  lcSegLine = lcSegLine + PADR(SUBSTR(InvLine.Desc1,1,30),30,' ')
  *-- Item Discription Line 2
  lcSegLine = lcSegLine + SPACE(30)
  *-- Color Description
  lcSegLine = lcSegLine + PADR(lcClrDesc,20,' ')
  *-- Size Description
  lcSegLine = lcSegLine + PADR(ALLTRIM(lcSize),20,' ')

  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')


  *!*************************************************************
  *! Name : lfAddRetRec
  *! Auth : Wael Aly MOhamed
  *! Date : 12/30/99
  *!*************************************************************
  *! Synopsis : Write a credit record in the output orders text file
  *!*************************************************************
  *! Called from :
  *!         Procedures : EBFACID
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
  lcSegLine = lcSegLine + SPACE(1)
  *-- Record Type
  lcSegLine = lcSegLine + "R"
  *-- Client Customer Number
  lcSegLine = lcSegLine + PADR(lcCustomer,15)
  *-- Client Order Number
  lcSegLine = lcSegLine + PADL(ALLTRIM(RetHdr.ORDER),22,'0')
  *-- Credit Amount
  lcAmount= STRTRAN(STR(RetHdr.TotCredit,9,2) ,' ','0')
  lcAmount= STRTRAN(lcAmount,'.','')
  lcSegLine = lcSegLine + lcAmount
  *-- Bill & Hold Code ?
  lcSegLine = lcSegLine + SPACE(1)
  *-- Start Ship Date ?
  lcSegLine = lcSegLine + SPACE(6)
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
  lcSegLine = lcSegLine + PADL(ALLTRIM(RetHdr.STORE),4,'0')
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
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

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
  *B607932,1 WLD add space in the secone field 01/14/2007 [Begin]
  lcSegLine = lcSegLine + SPACE(1)
  *B607932,1 WLD add space in the secone field 01/14/2007 [End]
  *-- Record Type
  lcSegLine = lcSegLine + "S"
  *-- Total number of Name/Address records
  lcSegLine = lcSegLine + PADL(lnSCusRec,6,'0')
  *-- Total number of Invoice records
  *B607932,1 WLD Total number of Invoice records 01/14/2007 [Begin]
  *lcSegLine = lcSegLine + PADL(lnSInvRec,6,'0') 
  lcSegLine = lcSegLine + PADL(lnSInvSum,6,'0') 
  *B607932,1 WLD Total number of Invoice records 01/14/2007 [End]
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
  lcAssignDate = PADL(MONTH(DATE()),2,'0') + PADL(DAY(DATE()),2,'0') +;
    RIGHT(STR(YEAR(DATE()),4),2)
  lcSegLine = lcSegLine + lcAssignDate
  *-- Factoring Free Code
  lcSegLine = lcSegLine + "0"
  *-- Future USe
  lcSegLine = lcSegLine + SPACE(95)
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

  *!*************************************************************
  *! Name : lfAddTrnRec
  *! Auth : Waleed Hamed (WLD)
  *! Date : 11/26/2006
  *!*************************************************************
  *! Synopsis : Write the transmission total record in the output orders text file
  *!*************************************************************
  *! Called from :
  *!         Procedures : EBFACID
  *!*************************************************************
  *! Modifications : None.
  *!*************************************************************
FUNCTION lfAddTrnRec
  LPARAMETERS loFormSet

  PRIVATE lcSegLine, lcInvAmt, lcRetAmt, lcTotAmt

  lcSegLine = ''
  *-- Client Number
  lcSegLine = lcSegLine + "9999"
  *-- Filler
  lcSegLine = lcSegLine + SPACE(1)
  *-- Record Type
  lcSegLine = lcSegLine + "T"
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
  lcTransDate = PADL(MONTH(DATE()),2,'0') + PADL(DAY(DATE()),2,'0') +;
    RIGHT(STR(YEAR(DATE()),4),2)
  lcSegLine = lcSegLine + lcTransDate
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')


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
  DO WHILE cSelect+TYPE+Account+TranNum = "YO"
    lcCustomer = Account
    SELECT CUSTOMER
    *-- Get customer factor number
    IF gfSEEK('M'+lcCustomer)
      IF EMPTY(Customer.FactAcct)
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
    SCAN WHILE cSelect+TYPE+Account+TranNum = "YO"+lcCustomer
      =lfAddOrdRec(loFormSet)

      *-- Update the CitTrnLn Temp. File
      INSERT INTO (loFormSet.lcTmpCit) (cFacCode,BatchNo,dDate,Account,ORDER) VALUES ;
        (lcFactor,loFormSet.lnBatchNo,oAriaApplication.SystemDate,lcCustomer,OrdHdr.ORDER)

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
  *! Auth : Waleed Hamed (WLD)
  *! Date : 11/26/2006
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
  lcOrdDate = PADL(MONTH(OrdHdr.START),2,'0') + PADL(DAY(OrdHdr.START),2,'0') +;
    RIGHT(STR(YEAR(OrdHdr.START),4),2)
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
  =gfRltFld(InvHdr.cTermCode,@laTRltFld,'CTERMCODE')

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
  lcOrdDate = PADL(MONTH(OrdHdr.COMPLETE),2,'0') + PADL(DAY(OrdHdr.COMPLETE),2,'0') +;
    RIGHT(STR(YEAR(OrdHdr.COMPLETE),4),2)
  lcSegLine = lcSegLine + lcOrdDate

  lcSegLine = lcSegLine + SPACE(102)
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')
  *!*************************************************************
  *! Name : lfAddOrdSub
  *! Auth : Waleed Hamed (WLD)
  *! Date : 11/26/2006
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

  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

  *!*************************************************************
  *! Name : lfAddOrdTot
  *! Auth : Waleed Hamed (WLD)
  *! Date : 11/26/2006
  *!*************************************************************
  *! Synopsis : Write the transmission total record in the output orders text file
  *!*************************************************************
  *! Called from :
  *!         Procedures : EBFACID
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
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

  *!*************************************************************
  *! Name : lfAddInvSum
  *! Auth : Waleed Hamed
  *! Date : 01/13/2002
  *!*************************************************************
  *! Synopsis : Write an invoice summary in the output orders text file
  *!*************************************************************
  *! Called from :
  *!         Procedures : EBFACID
  *!*************************************************************
  *! Modifications : None.
  *!*************************************************************
FUNCTION lfAddInvSum
  LPARAMETERS loFormSet
  PRIVATE lcSegLine, lcTmpDate, lcInvDate, lcFTermRate, lcFTermDays, lcEOM

  lcSegLine = ''
  *-- Client Number 1 - 4
  lcSegLine = lcSegLine + loFormSet.lcClientNo
  *-- Trade Style 5 - 5
  lcSegLine = lcSegLine + SPACE(1)
  *-- Record Type 6 - 6
  lcSegLine = lcSegLine + IIF(loFormSet.lcTranType='I','D','C')
  *-- Client Customer Number 7 - 21
  lcSegLine = lcSegLine + PADR(lcCustomer,15," ")
  *-- Invoice Number 22 - 29
  lcSegLine = lcSegLine + PADL(EVALUATE(loFormSet.lcTempTran+'.TranNum'),8,'0')
  *-- Filler 30 - 36
  lcSegLine = lcSegLine + SPACE(7)
  *-- Invoice Item Count 37 - 41
  lcSegLine = lcSegLine + PADL(lnSInvRec,5,'0')
  *-- Invoice Amount 42 - 51
  lcSegLine = lcSegLine + STRTRAN(STR(INVHDR.TotalChg*100,10),' ','0')
  *-- Invoice Date 52 - 57
  lcInvDate = PADL(MONTH(INVHDR.InvDate),2,'0')+PADL(DAY(INVHDR.InvDate),2,'0')+RIGHT(STR(YEAR(INVHDR.InvDate),4),2)
  lcSegLine = lcSegLine + lcInvDate
  *-- As Of Date 58 - 63
  lcSegLine = lcSegLine + SPACE(6)
  *-- Date Shipped  64 - 69
  lcDateShp = PADL(MONTH(INVHDR.ShipDate),2,'0')+PADL(DAY(INVHDR.ShipDate),2,'0')+RIGHT(STR(YEAR(INVHDR.ShipDate),4),2)
  lcSegLine = lcSegLine + lcDateShp
  *-- Reserved 70 - 75
  lcSegLine = lcSegLine + SPACE(6)
  IF loFormSet.lcTranType='I'
    *-- Client Terms Code 76 - 78
    lcSegLine = lcSegLine + PADR(RIGHT(ALLTRIM(INVHDR.cTermCode) , 3) , 3 , " ")
    *-- Terms Information 79 - 108
    =SEEK('N' + INVHDR.cTermCode + 'N' + 'CTERMCODE' , 'CODES')
    lcSegLine = lcSegLine + PADR(ALLTRIM(CODES.cDiscRep),30 , ' ' )
  ELSE
    *-- Client Terms Code 76 - 78
    lcSegLine = lcSegLine + '000'
    *-- Terms Information 79 - 108
    lcSegLine = lcSegLine + SPACE(30)
  ENDIF

  *-- Merchandise Amount 109 - 118
  lcSegLine = lcSegLine + PADL(INT(INVHDR.ShipAmt*100),10,'0')

  *-- Store Number 119 - 123
  lcSegLine = lcSegLine + PADR(ALLTRIM(INVHDR.STORE) , 5  , ' ')
  *-- Cust Purchase Order# 124 - 145
  lcSegLine = lcSegLine + PADR(ALLTRIM(INVHDR.CustPo), 22 , ' ')
  *-- Cust Purchase Order Date 146 - 151
  =gfSEEK(lcCustomer+'O'+InvHdr.ORDER,'ORDHDR')
  lcEnter = PADL(MONTH(OrdHdr.Entered),2,'0')+PADL(DAY(OrdHdr.Entered),2,'0')+RIGHT(STR(YEAR(OrdHdr.Entered),4),2)
  lcSegLine = lcSegLine + lcEnter
  *-- Cust department# 152 - 157
  lcSegLine = lcSegLine + PADR(ALLTRIM(INVHDR.Dept), 6 , ' ')
  *-- Risk Code 158 - 158
  lcSegLine = lcSegLine + SPACE(1)
  
  *B608065 HIA review and fix the 'D' record [Begin]
  *-- Discount Type Code 159 - 159
  *lcSegLine = lcSegLine + IIF(ABS(INVHDR.DISCOUNT)>0,'1',SPACE(1))
  lcSegLine = lcSegLine + SPACE(1)
  *B608065 HIA review and fix the 'D' record [End]
  
  *B608065 HIA review and fix the 'D' record [Begin]
  *-- Discount Amount 160 - 169
  *lcSegLine = lcSegLine + PADL(INT(ABS(INVHDR.DISCOUNT)*100),10,'0')
  lcSegLine = lcSegLine + SPACE(10)
  *B608065 HIA review and fix the 'D' record [End]
    
  *-- Credit Memo Invoice # 170 - 177
  lcSegLine = lcSegLine + IIF(loFormSet.lcTranType='I',SPACE(8),LEFT(EVALUATE(loFormSet.lcTempTran+'.TranNum'),8))
  *-- Filler 178 - 184
  lcSegLine = lcSegLine + SPACE(7)
  *-- Freight Amount 185 - 194
  lcSegLine = lcSegLine + PADL(INT(INVHDR.FREIGHT*100),10,'0')
  *-- Sales Tax Amount 195 - 204
  lcSegLine = lcSegLine + PADL(INT((INVHDR.Tax_Amt+INVHDR.nPstAmt)*100),10,'0')
  *-- Other Charge Amount 205 - 214
  lcSegLine = lcSegLine + PADL(INT((INVHDR.COD+INVHDR.INSUR+INVHDR.nCharges)*100),10,'0')
  
  *B608065 HIA review and fix the 'D' record [Begin]
  *-- Allowance Amount 215 - 224
  *lcSegLine = lcSegLine + SPACE(10)
  lcSegLine = lcSegLine + PADL(INT(ABS(INVHDR.DISCOUNT)*100),10,'0')
  *B608065 HIA review and fix the 'D' record [End] 
   
  *-- Vendor ID 225 - 239
  lcSegLine = lcSegLine + PADR(ALLTRIM(CUSTOMER.ccusvend), 15 , ' ')
  *-- Freight Carrier 240 - 269
  =SEEK('N'+INVHDR.ShipVia+'N'+'SHIPVIA','CODES')
  lcSegLine = lcSegLine + PADR(ALLTRIM(CODES.cDiscRep),30,' ')
  *-- Shipment Payment Code 270 - 271
  lcSegLine = lcSegLine + "PP"
  *-- Number of Cartons 272 - 277
  lcSegLine = lcSegLine + PADL(INVHDR.cartons,6,'0')

  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

*! B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [Start]
FUNCTION lfvFlName
PARAMETERS loFormSet,lcFlNamValue
lcPath  = JUSTPATH(loFormSet.lcOutFile)
loFormSet.lcOutFile= ADDBS(lcPath)+lcFlNamValue
*! B609276,1 MMT 05/27/2010 change user file selection UI to 2 separate fields (directory and File) [End]