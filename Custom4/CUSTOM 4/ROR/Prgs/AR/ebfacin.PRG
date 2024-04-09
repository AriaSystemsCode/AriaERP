*:*********************************************************************************
*: Program file  : EBFACIN.PRG
*: Program desc. : Send Invoice and Sales Orders to Rosenthal & Rosenthal
*: For screen    : Menu
*:        System : Aria4 XP.
*:        Module : Accounts Receivable (AR).
*:     Developer : Waleed Hamed (WLD)
*:     Entry     : Convert from aria27 to Aria4
*:*********************************************************************************
*! Passed Parameters  :  lcTranType
*!                       'I' : Send Invoices
*!                       'O' : Send Sales Orders
*:*********************************************************************************
*: Modifications :
*:B608347,1 MMT 11/08/2007 Fix bug of Wrong Approval Num,Assign Number,Terms Code[T20071030.0007]
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[T20101130.0036]
*:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{T20120326.0023}
*:B609886,2 MMT 04/19/2012 Modify progrm to create file with the required format{T20120326.0023}
*:*********************************************************************************
PARAMETER lcTranType
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[Start]
IF oAriaApplication.MULTIINST 
  lcParmLst = "lcTranType"
  =gfCallForm('EBFACIN','AR',lcParmLst)
ELSE  
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[End]
  DO FORM oAriaApplication.ScreenHome+ "AR\EBFACIN.SCX" WITH lcTranType
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[Start]
ENDIF
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[End]

*!*************************************************************
*! Name : lfOpenFiles
*! Auth : Waleed Hamed (WLD)
*! Date : 11/15/2006
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
*! Date : 11/15/2006
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
loFormSet.lnSubInv = 2

IF loFormSet.lcTranType = 'O'
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
  *:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{Start}
  loFormSet.lcOutFile = oAriaApplication.DataDir + 'm'+PADL(MONTH(oAriaApplication.SystemDate),2,'0')+PADL(DAY(oAriaApplication.SystemDate),2,'0')+'.txt'  
  *:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{END}
ELSE
  SET RELATION TO Account+'O'+TranNum INTO ORDHDR ADDITIVE
  *:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{Start}
  loFormSet.lcOutFile = oAriaApplication.DataDir + 'cr'+PADL(MONTH(oAriaApplication.SystemDate),2,'0')+PADL(DAY(oAriaApplication.SystemDate),2,'0')+'.txt'  
  *:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{END}
ENDIF
*:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{Start}
*loFormSet.lcOutFile = oAriaApplication.DataDir + 'CIT'+PADL(MONTH(oAriaApplication.SystemDate),2,'0')+PADL(DAY(oAriaApplication.SystemDate),2,'0')+'.NEW'
*:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{End}
IF FILE(oAriaApplication.DataDir+"MEMO.MEM")
  RESTORE FROM oAriaApplication.DataDir+"MEMO" ADDITIVE
ENDIF

loFormSet.ariaForm1.cboSubInv.visible  = IIF(loFormSet.lcTranType = 'I',.T.,.F.) 
loFormSet.ariaForm1.AriaLabel6.visible = IIF(loFormSet.lcTranType = 'I',.T.,.F.) 

ENDFUNC

*!*************************************************************
*! Name : lfShow
*! Auth : Waleed Hamed (WLD)
*! Date : 11/15/2006
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
    .cboLastTran.VALUE  = '0'
    .cboSubInv.VALUE    = '0'
    .txtOutFile.VALUE   = loFormSet.lcOutFile
  ENDWITH
ENDCASE
*!*************************************************************
*! Name      : lfvFactor
*! Auth      : Waleed Hamed (WLD)
*! Date      : 11/15/2006
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
      .cmdSelect.ENABLED , .cboSubInv.ENABLED
  ENDWITH
ELSE
  WITH loFormSet.ariaForm1
    STORE .T. TO .kBCustomer.ENABLED , .kBCustomer.keyCmd.ENABLED , .kBCustomer.keytextbox.ENABLED ,;
      .cmdGetFile.ENABLED , .cboLastTran.ENABLED ,;
      .cmdSelect.ENABLED , .cboSubInv.ENABLED
  ENDWITH
ENDIF
loFormSet.AriaForm1.txtFactName.VALUE = IIF(!EMPTY(lcFactor),SycFact.cFacComp,loFormSet.AriaForm1.txtFactName.VALUE)
*:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{Start}
loFormSet.lcClientNo = SycFact.cClientNo
IF loFormSet.lcTranType = 'I'
  loFormSet.lcOutFile = oAriaApplication.DataDir + 'm'+ALLTRIM(loFormSet.lcClientNo)+PADL(MONTH(oAriaApplication.SystemDate),2,'0')+PADL(DAY(oAriaApplication.SystemDate),2,'0')+'.txt'  
ELSE
  loFormSet.lcOutFile = oAriaApplication.DataDir + 'cr'+ALLTRIM(loFormSet.lcClientNo)+PADL(MONTH(oAriaApplication.SystemDate),2,'0')+PADL(DAY(oAriaApplication.SystemDate),2,'0')+'.txt'  
ENDIF
loFormSet.ariaForm1.txtOutFile.VALUE   = loFormSet.lcOutFile
*:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{End}
SELECT (lnAlias)
llBrowse = .F.
RETURN !EMPTY(lcFactor)

*!*************************************************************
*! Name      : lfvCustomer
*! Developer : Waleed Hamed (WLD)
*! Date      : 11/15/2006
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
*! Date      : 11/15/2006
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
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[Start]
*LPARAMETERS loFormSet
PARAMETERS loFormSet
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[End]

lcFactor   = ALLTRIM(loFormSet.AriaForm1.kbFactor.KeyTextBox.VALUE)
lcCustomer = ALLTRIM(loFormSet.AriaForm1.kbCustomer.KeyTextBox.VALUE)
lnSubInv =  VAL(loFormSet.AriaForm1.cboSubInv.value)

*:B608347,1 MMT 11/08/2007 Fix bug of Wrong Approval Num,Assign Number,Terms Code[Start]
*SET ORDER TO (loFormSet.lcTempTran) IN (loFormSet.lcTempTran)
SET ORDER TO RangeSelct IN (loFormSet.lcTempTran)
*:B608347,1 MMT 11/08/2007 Fix bug of Wrong Approval Num,Assign Number,Terms Code[End]

IF loFormSet.lcTranType = 'I'
  SELECT (loFormSet.lcTempTran)
  =gfSEEK('I')
  DELETE REST WHILE TYPE = 'I'
  SELECT INVHDR
  =gfSetOrder('INVHDRA')
  =GFSEEK(ALLTRIM(lcCustomer))
  IF lnSubInv = 1       
    SCAN REST WHILE Account+Invoice = ALLTRIM(lcCustomer) FOR cFacCode = lcFactor AND STATUS <>'V'
      INSERT INTO (loFormSet.lcTempTran) (cSelect,TYPE,Account,TranNum,invdate,STORE,ORDER,ship,totalchg,lSelect);
        VALUES ('N','I',INVHDR.Account,INVHDR.Invoice,INVHDR.invdate,INVHDR.STORE,INVHDR.ORDER,INVHDR.ship,INVHDR.totalchg,.F.)
      SELECT CUSTOMER
      =GFSEEK(PADR(lcCustomer,5))
      SELECT (loFormSet.lcTempTran)
      REPLACE FactAcct WITH IIF(EMPTY(CUSTOMER.factacct),'**NEW**',CUSTOMER.factacct)
    ENDSCAN
  ELSE
    SCAN REST WHILE Account+Invoice = ALLTRIM(lcCustomer) FOR cFacCode = lcFactor AND ;
        STATUS <>'V' 
      INSERT INTO (loFormSet.lcTempTran) (cSelect,TYPE,Account,TranNum,invdate,STORE,ORDER,ship,totalchg,lSelect);
        VALUES ('N','I',INVHDR.Account,INVHDR.Invoice,INVHDR.invdate,INVHDR.STORE,INVHDR.ORDER,INVHDR.ship,INVHDR.totalchg,.F.)
      SELECT CUSTOMER
      =GFSEEK(PADR(lcCustomer,5))
      SELECT (loFormSet.lcTempTran)
      REPLACE FactAcct WITH IIF(EMPTY(CUSTOMER.factacct),'**NEW**',CUSTOMER.factacct)
    ENDSCAN
  ENDIF
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

*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[Start]
IF oAriaApplication.MULTIINST 
  lcParmLst = "loFormSet,'EBFACIN',loFormSet.lcTranType"
  =gfCallForm('EBSNDTR','AR',lcParmLst)
ELSE
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[End]
  DO FORM oAriaApplication.ScreenHome+ "AR\EBSNDTR.SCX" WITH loFormSet,'EBFACIN',loFormSet.lcTranType
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[Start]
ENDIF
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[End]

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
*! Date      : 11/15/2006
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
*! Date      : 11/15/2006
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
lnSubInv   = loFormSet.lnSubInv
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
*! Date : 11/15/2006
*!**************************************************************
*! Synopsis : Write an output Invoices text file to be send to Capital Factor.
*!*************************************************************
*! Called from :
*!         Procedures : EBFACIN
*!*************************************************************
*! Calls :
*!         FUNCTIONS : lfCalcInvTot
*!                     lfAddInvRec
*!                     lfAddSubRec
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
STORE 0 TO lnInvRec,lnInvAmt
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
      *-- Increament number of invoice records and Invoice Dollars
      lnInvRec = lnInvRec + 1      
    ENDSCAN
  ENDDO
ENDIF
*-- Add Sub Total Record Type "S"
*:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{Start}
*=lfAddSubRec(loFormSet)
*:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{End}
SELECT (loFormSet.lcTempTran)
SET RELATION TO

*!*************************************************************
*! Name : lfAddInvRec
*! Auth : Waleed Hamed (WLD)
*! Date : 11/15/2006
*!*************************************************************
*! Synopsis : Write an invoice record in the output orders text file
*!*************************************************************
*! Called from :
*!         Procedures : EBFACIN
*!*************************************************************
*! Modifications : None.
*!*************************************************************
*:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{Start}
*FUNCTION lfAddInvRec
FUNCTION _lfAddInvRec
*:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{END}
LPARAMETERS loFormSet

PRIVATE lcSegLine, lcTmpDate, lcInvDate, lcFTermRate, lcFTermDays,lcSTermDays
STORE 0   TO lnDiscRate,lnDaysDue,lnDiscDays
STORE ' ' TO lcTEOM

*:B608347,1 MMT 11/08/2007 Fix bug of Wrong Approval Num,Assign Number,Terms Code[Start]
=gfSEEK('O'+INVHDR.ORDER,'ORDHDR','ORDHDR')
*:B608347,1 MMT 11/08/2007 Fix bug of Wrong Approval Num,Assign Number,Terms Code[End]


DECLARE laTRltFld[4,2]
laTRltFld[1,1] = 'NTERDISCR'
laTRltFld[1,2] = 'lnDiscRate'
laTRltFld[2,1] = 'EOM'
laTRltFld[2,2] = 'lcTEOM'
laTRltFld[3,1] = 'NTERDUED'
laTRltFld[3,2] = 'lnDaysDue'
laTRltFld[4,1] = 'NTERDISCD '
laTRltFld[4,2] = 'lnDiscDays'

*-- Terms Information
*lcFTermRate = SPACE(4)
lcFTermRate = '0000'
lcFTermDays = SPACE(3)
lcSTermDays = SPACE(3)
lcExtraDays = SPACE(3)

=gfRltFld(InvHdr.cTermCode,@laTRltFld,'CTERMCODE')

IF lnDiscRate <> 0
  *-- First terms rate
  lcFTermRate = PADL(ALLTRIM(STR(lnDiscRate*100)),4,'0')
ENDIF  

*:B608347,1 MMT 11/08/2007 Fix bug of Wrong Approval Num,Assign Number,Terms Code[Start]
*!*	IF lnDiscDays <> 0
*!*	  *-- First Terms days
*!*	  lcFTermDays = PADL(INT(lnDiscDays),3,'0')
*!*	  IF lnDaysDue <> 0
*!*	    lcSTermDays = PADL(INT(lnDaysDue),3,'0')
*!*	  ENDIF
*!*	ELSE
*!*	  IF lnDaysDue <> 0
*!*	    lcFTermDays = PADL(INT(lnDaysDue),3,'0')
*!*	  ENDIF    
*!*	ENDIF    
*:B608347,1 MMT 11/08/2007 Fix bug of Wrong Approval Num,Assign Number,Terms Code[End]

IF lcTEOM='Y'
  lcSTermDays = 'EOM'
ENDIF

lcSegLine = ''
*01-- Type
lcSegLine = lcSegLine + 'I'
*02-- Client Number
lcSegLine = lcSegLine + PADL(loFormSet.lcClientNo,5,'0')
*03-- Customer Number
lcSegLine = lcSegLine + lcCustNum
*04-- Filler
lcSegLine = lcSegLine + SPACE(10)
*05-- Invoice Amount
lcSegLine = lcSegLine + PADL(ALLTRIM(STR(InvHdr.TotalChg*100,9)),9,'0')
*06-- Invoice Date
lcInvDate = RIGHT(STR(YEAR(InvHdr.InvDate),4),2)+PADL(MONTH(InvHdr.InvDate),2,'0') + PADL(DAY(InvHdr.InvDate),2,'0')
lcSegLine = lcSegLine + lcInvDate
*07-- Approval # 
lcSegLine = lcSegLine + SUBSTR(OrdHdr.Approval,1,4)
*08-- Filler (19)
lcSegLine = lcSegLine + Space(19)
*09-- Schedule Code
lcSegLine = lcSegLine + PADL(loFormSet.lnAssignNo,5,'0')
*10-- As Of Date
lcSegLine = lcSegLine + SPACE(6)
*11-- Credit Indicator
lcSegLine = lcSegLine + SPACE(1)
*12-- Invoice Number
lcInvNumb = EVALUATE(loFormSet.lcTempTran+'.TranNum')
lcSegLine = lcSegLine + PADL(ALLTRIM(lcInvNumb) , 10,'0')
*13-- Store Number
lcSegLine = lcSegLine + PADL(ALLTRIM(InvHdr.Store),5,' ')
*14-- Purchase Order
lcSegLine = lcSegLine + InvHdr.Custpo
*15-- Free Form Terms
*-- First Terms Rate

*:B608347,1 MMT 11/08/2007 Fix bug of Wrong Approval Num,Assign Number,Terms Code[Start]
*!*	lcSegLine = lcSegLine + lcFTermRate
*!*	lcSegLine = lcSegLine + lcFTermDays
*!*	*-- Second Terms Rate
*!*	lcSegLine = lcSegLine + SPACE(3)
*!*	*-- Second Terms Days
*!*	lcSegLine = lcSegLine + lcSTermDays
*!*	*-- THird Terms Days
*!*	lcSegLine = lcSegLine + SPACE(3)
*!*	*-- Extra Days
*!*	lcSegLine = lcSegLine + lcExtraDays 
lcHAlias = ALIAS()
SELECT Codes
lcHOrder = ORDER()
=gfSetOrder('Codes')

=gfSEEK('N' + InvHdr.CTERMCODE + 'N' + 'CTERMCODE' , 'CODES')
lcHTermDiscRep  = ALLTRIM(CODES.cDiscRep)
lcSegLine = lcSegLine + PADR(lcHTermDiscRep,31," ")

IF !EMPTY(lcHOrder)
  =gfSetOrder(lcHOrder)
ENDIF 
SELECT (lcHAlias)
*:B608347,1 MMT 11/08/2007 Fix bug of Wrong Approval Num,Assign Number,Terms Code[End]

= FPUTS(lnOutFile,lcSegLine)
*!*************************************************************
*! Name : lfAddSubRec
*! Auth : Waleed Hamed (WLD)
*! Date : 11/15/2006
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
PRIVATE lcSegLine

*-- Increament the assignment number

*:B608347,1 MMT 11/08/2007 Fix bug of Wrong Approval Num,Assign Number,Terms Code[Start]
*lnAssignNo = loFormSet.lnAssignNo + 1
loFormSet.lnAssignNo = loFormSet.lnAssignNo + 1
*:B608347,1 MMT 11/08/2007 Fix bug of Wrong Approval Num,Assign Number,Terms Code[End]

lcSegLine = ''
*01-- Type
lcSegLine = lcSegLine + '9'
*02-- Client Number
lcSegLine = lcSegLine + PADL(loFormSet.lcClientNo,5,'0')
*03-- Filler
lcSegLine = lcSegLine + SPACE(15)
*04-- Invoice Records
lcSegLine = lcSegLine + PADL(lnInvRec,5,'0')
*05-- Invocie Dollars
lcSegLine = lcSegLine + PADL(ALLTRIM(STR(lnInvAmt*100,9)),10,'0')
*06-- Filer
lcSegLine = lcSegLine + SPACE(100)
= FPUTS(lnOutFile,lcSegLine)
*!*************************************************************
*! Name      : lfWrtOrder
*! Developer : Waleed Hamed (WLD)
*! Date      : 11/15/2006
*! Purpose   : Write outbound Order
*!*************************************************************
*! Calls     : lfAddOrdRec(),lfAddOrdSub()
*!*************************************************************
*! Passed Parameters  :  loFormSet
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfWrtOrder()
*!*************************************************************
FUNCTION lfWrtOrder
LPARAMETERS loFormSet

PRIVATE lcCustomer

lcFactor   = loFormSet.AriaForm1.kbFactor.KeyTextBox.VALUE

WAIT 'Creating outbound Orders file...' WINDOW NOWAIT

*-- Initialize subtotal number of Name/Address records, Order records
STORE 0 TO lnSOrdRec, lnSOrdAmt

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
    *-- Increament number of order records and Order amount
    lnSOrdRec = lnSOrdRec + 1
    lnSOrdAmt = lnSOrdAmt + ROUND(OrdHdr.BookAmt,0)

  ENDSCAN
ENDDO
*-- Add Order Trailer Record
*:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{Start}
*=lfAddOrdSub(loFormSet)
*:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{End}
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
*:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{Start}
*FUNCTION lfAddOrdRec
FUNCTION _lfAddOrdRec
*:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{END}
LPARAMETERS loFormSet
PRIVATE lcSegLine, lcTmpDate, lcOrdDate, lcFTermDays
STORE 0   TO lnDiscRate,lnDaysDue,lnDiscDays
STORE ' ' TO lcTEOM

DECLARE laTRltFld[4,2]
laTRltFld[1,1] = 'NTERDISCR'
laTRltFld[1,2] = 'lnDiscRate'
laTRltFld[2,1] = 'EOM'
laTRltFld[2,2] = 'lcTEOM'
laTRltFld[3,1] = 'NTERDUED'
laTRltFld[3,2] = 'lnDaysDue'
laTRltFld[4,1] = 'NTERDISCD '
laTRltFld[4,2] = 'lnDiscDays'

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
*01-- Type
lcSegLine = lcSegLine + 'O'
*02-- Client Number
lcSegLine = lcSegLine + PADL(loFormSet.lcClientNo,5,'0')
*03-- Customer Number
lcSegLine = lcSegLine + lcCustNum
*04-- Terms (# of Days)
lcSegLine = lcSegLine + lcFTermDays
*05-- As Of Date
lcSegLine = lcSegLine + SPACE(6)
*06-- Order Amount
lcSegLine = lcSegLine + PADL(ROUND(OrdHdr.BookAmt,0),7,'0')
*07-- Order Start Date
lcOrdDate = RIGHT(STR(YEAR(OrdHdr.Start),4),2)+PADL(MONTH(OrdHdr.Start),2,'0')+PADL(DAY(OrdHdr.Start),2,'0')
lcSegLine = lcSegLine + lcOrdDate
*08-- Order Completion Date
lcOrdDate = RIGHT(STR(YEAR(OrdHdr.Complete),4),2)+PADL(MONTH(OrdHdr.Complete),2,'0')+PADL(DAY(OrdHdr.Complete),2,'0')
lcSegLine = lcSegLine + lcOrdDate
*09-- Reference#
lcSegLine = lcSegLine + PADR(OrdHdr.custpo,9)
*10-- Approval Number
lcSegLine = lcSegLine + SPACE(4)
*11-- Status Code
lcSegLine = lcSegLine + SPACE(1)
*12-- Reason Code
lcSegLine = lcSegLine + SPACE(1)
*13-- Amount Approved
lcSegLine = lcSegLine + SPACE(7)
*14-- Description
lcSegLine = lcSegLine + SPACE(23)
*15-- Terms (Extra Days)
lcSegLine = lcSegLine + lcExtraDays
*16-- Filer
lcSegLine = lcSegLine + SPACE(9)

= FPUTS(lnOutFile,lcSegLine)
*!*************************************************************
*! Name : lfAddOrdSub
*! Auth : Waleed Hamed (WLD)
*! Date : 11/15/2006
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
*01-- Type
lcSegLine = lcSegLine + '9'
*02-- Client Number
lcSegLine = lcSegLine + PADL(loFormSet.lcClientNo,5,'0')
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
*! Name : lfCalcInvTot
*! Auth : Waleed Hamed (WLD)
*! Date : 11/15/2006
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

*:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{Start}
*!*************************************************************
*! Name : lfAddOrdRec
*! Auth : Mariam Mazhar (MMT)
*! Date : 04/10/2012
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
PRIVATE lcSegLine, lcTmpDate, lcOrdDate, lcFTermDays
STORE 0   TO lnDiscRate,lnDaysDue,lnDiscDays
STORE ' ' TO lcTEOM

DECLARE laTRltFld[4,2]
laTRltFld[1,1] = 'NTERDISCR'
laTRltFld[1,2] = 'lnDiscRate'
laTRltFld[2,1] = 'EOM'
laTRltFld[2,2] = 'lcTEOM'
laTRltFld[3,1] = 'NTERDUED'
laTRltFld[3,2] = 'lnDaysDue'
laTRltFld[4,1] = 'NTERDISCD '
laTRltFld[4,2] = 'lnDiscDays'

*-- Terms Information
lcFTermDays = '000'
lcExtraDays = SPACE(3)
=gfRltFld(OrdHdr.cTermCode,@laTRltFld,'CTERMCODE')
IF lcTEOM='Y'
  lcFTermDays = PADL(INT(lnDaysDue),2,'0')+'E'
ELSE  
  lcFTermDays = PADL(INT(lnDaysDue),3,'0')
ENDIF    
lcHAlias = ALIAS()
SELECT Codes
lcHOrder = ORDER()
=gfSetOrder('Codes')

=gfSEEK('N' + OrdHdr.cTermCode + 'N' + 'CTERMCODE' , 'CODES')
lcHTermDiscRep  = ALLTRIM(CODES.cDiscRep)
*lcSegLine = lcSegLine + PADR(lcHTermDiscRep,15," ")

IF !EMPTY(lcHOrder)
  =gfSetOrder(lcHOrder)
ENDIF 
SELECT (lcHAlias)


lcSegLine = ''
*01-- Type
lcSegLine = lcSegLine + 'CO1'
*02-- Client Number
*:B609886,2 MMT 04/19/2012 Modify progrm to create file with the required format{Start}
*lcSegLine = lcSegLine + PADL(loFormSet.lcClientNo,8,'0')
lcSegLine = lcSegLine + PADR(loFormSet.lcClientID,8)
*:B609886,2 MMT 04/19/2012 Modify progrm to create file with the required format{END}
*03 TradeStyle
*lcSegLine = lcSegLine + SPACE(2)
lcSegLine = lcSegLine + SUBSTR(ALLTRIM(loFormSet.lcClientNo),1,2)
*04-- Customer Number
lcSegLine = lcSegLine + PADR(ALLTRIM(lcCustNum),20)
*05-Store
lcSegLine = lcSegLine + SPACE(10)
*06 Customer Order#
lcSegLine = lcSegLine + PADL(ALLTRIM(OrdHdr.custpo),15)
*07-- Order Amount
lcSegLine = lcSegLine + PADL(ROUND(OrdHdr.BookAmt,0),18,'0')
*08 - Filler
lcSegLine = lcSegLine + "00"
*09 - Entered Date
lcEntOrdDate = STR(YEAR(OrdHdr.Entered),4)+PADL(MONTH(OrdHdr.Entered),2,'0')+PADL(DAY(OrdHdr.Entered),2,'0')
lcSegLine = lcSegLine + lcEntOrdDate 
*010-- Order Start Date
lcOrdDate = STR(YEAR(OrdHdr.Start),4)+PADL(MONTH(OrdHdr.Start),2,'0')+PADL(DAY(OrdHdr.Start),2,'0')
lcSegLine = lcSegLine + lcOrdDate
*011-- Order Completion Date
lcOrdDate = STR(YEAR(OrdHdr.Complete),4)+PADL(MONTH(OrdHdr.Complete),2,'0')+PADL(DAY(OrdHdr.Complete),2,'0')
lcSegLine = lcSegLine + lcOrdDate
*012-- Terms (# of Days)
lcSegLine = lcSegLine + PADL(lcHTermDiscRep,15)
*013-- As Of Date
lcSegLine = lcSegLine + SPACE(8)
*014-- commnets
lcSegLine = lcSegLine + SPACE(30)
= FPUTS(lnOutFile,lcSegLine)
*!*************************************************************
*! Name : lfAddInvRec
*! Auth : Mariam Mazhar (MMT)
*! Date : 04/10/2012
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

PRIVATE lcSegLine, lcTmpDate, lcInvDate, lcFTermRate, lcFTermDays,lcSTermDays
STORE 0   TO lnDiscRate,lnDaysDue,lnDiscDays
STORE ' ' TO lcTEOM

*:B608347,1 MMT 11/08/2007 Fix bug of Wrong Approval Num,Assign Number,Terms Code[Start]
=gfSEEK('O'+INVHDR.ORDER,'ORDHDR','ORDHDR')
*:B608347,1 MMT 11/08/2007 Fix bug of Wrong Approval Num,Assign Number,Terms Code[End]


DECLARE laTRltFld[4,2]
laTRltFld[1,1] = 'NTERDISCR'
laTRltFld[1,2] = 'lnDiscRate'
laTRltFld[2,1] = 'EOM'
laTRltFld[2,2] = 'lcTEOM'
laTRltFld[3,1] = 'NTERDUED'
laTRltFld[3,2] = 'lnDaysDue'
laTRltFld[4,1] = 'NTERDISCD '
laTRltFld[4,2] = 'lnDiscDays'

*-- Terms Information
*lcFTermRate = SPACE(4)
lcFTermRate = '0000'
lcFTermDays = SPACE(3)
lcSTermDays = SPACE(3)
lcExtraDays = SPACE(3)

=gfRltFld(InvHdr.cTermCode,@laTRltFld,'CTERMCODE')

IF lnDiscRate <> 0
  *-- First terms rate
  lcFTermRate = PADL(ALLTRIM(STR(lnDiscRate*100)),4,'0')
ENDIF  

*:B608347,1 MMT 11/08/2007 Fix bug of Wrong Approval Num,Assign Number,Terms Code[Start]
*!*	IF lnDiscDays <> 0
*!*	  *-- First Terms days
*!*	  lcFTermDays = PADL(INT(lnDiscDays),3,'0')
*!*	  IF lnDaysDue <> 0
*!*	    lcSTermDays = PADL(INT(lnDaysDue),3,'0')
*!*	  ENDIF
*!*	ELSE
*!*	  IF lnDaysDue <> 0
*!*	    lcFTermDays = PADL(INT(lnDaysDue),3,'0')
*!*	  ENDIF    
*!*	ENDIF    
*:B608347,1 MMT 11/08/2007 Fix bug of Wrong Approval Num,Assign Number,Terms Code[End]

IF lcTEOM='Y'
  lcSTermDays = 'EOM'
ENDIF

lcSegLine = ''
*01-- Type
lcSegLine = lcSegLine + 'II1'
*02-- Client Number
*:B609886,2 MMT 04/19/2012 Modify progrm to create file with the required format{Start}
*lcSegLine = lcSegLine + PADR(loFormSet.lcClientNo,8)
lcSegLine = lcSegLine + PADR(loFormSet.lcClientID,8)
*:B609886,2 MMT 04/19/2012 Modify progrm to create file with the required format{END}
*03 TradeStyle
lcSegLine = lcSegLine + SUBSTR(ALLTRIM(loFormSet.lcClientNo),1,2)
*04-- Schedule Code
lcSegLine = lcSegLine + PADL(InvHdr.order,6,'0')
*05- Schedule date
lcShipDate = STR(YEAR(InvHdr.ShipDate),4)+PADL(MONTH(InvHdr.ShipDate),2,'0') + PADL(DAY(InvHdr.ShipDate),2,'0')
lcSegLine = lcSegLine + lcShipDate 
*lcSegLine = lcSegLine + SPACE(8)
*06-- Invoice Number
lcInvNumb = EVALUATE(loFormSet.lcTempTran+'.TranNum')
lcSegLine = lcSegLine + PADL(ALLTRIM(lcInvNumb) , 22,'0')
*07-- Invoice Date
lcInvDate = STR(YEAR(InvHdr.InvDate),4)+PADL(MONTH(InvHdr.InvDate),2,'0') + PADL(DAY(InvHdr.InvDate),2,'0')
lcSegLine = lcSegLine + lcInvDate
*-08 Sign
lcSegLine = lcSegLine + "+"
*09-- Invoice Amount
lcSegLine = lcSegLine + PADL(ALLTRIM(STR(InvHdr.TotalChg*100,11)),11,'0')
*010-- Customer Number
lcSegLine = lcSegLine + PADL(ALLTRIM(lcCustNum),20)
lcHAlias = ALIAS()
SELECT Codes
lcHOrder = ORDER()
=gfSetOrder('Codes')

=gfSEEK('N' + InvHdr.CTERMCODE + 'N' + 'CTERMCODE' , 'CODES')
lcHTermDiscRep  = ALLTRIM(CODES.cDiscRep)
*-011 Terms
lcSegLine = lcSegLine + PADR(ALLTRIM(lcHTermDiscRep),15," ")

IF !EMPTY(lcHOrder)
  =gfSetOrder(lcHOrder)
ENDIF 
SELECT (lcHAlias)
*12-- Store Number
lcSegLine = lcSegLine + PADL(ALLTRIM(InvHdr.Store),10,' ')
*13-- Purchase Order
lcSegLine = lcSegLine + PADL(ALLTRIM(InvHdr.Custpo),15)
*14-- Comments
lcSegLine = lcSegLine + SPACE(30)
*15-- Reference
lcSegLine = lcSegLine + SPACE(30)
*16-- As Of Date
lcSegLine = lcSegLine + SPACE(8)
*017 Non Merch
lcSegLine = lcSegLine + REPLICATE('0',11)
*018-- Filler
lcSegLine = lcSegLine + SPACE(1)
= FPUTS(lnOutFile,lcSegLine)
*:B609886,1 MMT 04/10/2012 Modify progrm to create file with the required format{END}