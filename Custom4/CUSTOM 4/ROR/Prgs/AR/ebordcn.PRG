*!*************************************************************
*! Name      : EBORDCN
*! Developer : Waleed Hamed
*! Date      : 11/21/2006
*! Purpose   : Print Order Confirmation Report
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[T20101130.0036]
*!*************************************************************
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[Start]
IF oAriaApplication.MULTIINST 
  =gfCallForm('EBORDCON',oAriaApplication.ActiveModuleID)
ELSE
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[End]
  DO FORM oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleID+ "\EBORDCON.SCX" 
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[Start]
ENDIF
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[End]

*!*************************************************************
*! Name      : lpShow
*! Developer : Waleed Hamed
*! Date      : 11/21/2006
*! Purpose   : To validate the modes
*!*************************************************************
*! Example   :  =lpShow()
*!*************************************************************
FUNCTION lpShow
LPARAMETERS loFormSet
loFormSet.ariaForm1.kbFactor.Enabled = .T.

*!*************************************************************
*! Name      : lfInit
*! Developer : Waleed Hamed
*! Date      : 11/21/2006
*! Purpose   : Initialization of the form
*! Example   :  =lfInit()
*!*************************************************************
FUNCTION lfInit
LPARAMETERS loFormSet

SET MULTILOCKS ON
=gfOpenTable(oAriaApplication.DataDir+'CUSTOMER',oAriaApplication.DataDir+'CUSTOMER','SH')
=gfOpenTable(oAriaApplication.DataDir+'ORDHDR',oAriaApplication.DataDir+'ORDHDR','SH')
=gfOpenTable(oAriaApplication.SysPath+'SYCFACT',oAriaApplication.SysPath+'CFACCODE','SH')
=gfOpenTable(oAriaApplication.DataDir+'CITTRNLN',oAriaApplication.DataDir+'CITTRNLN','SH')
loFormSet.dataenvironment.initialselectedalias = 'ORDHDR'
loFormSet.nWorkArea = 'ORDHDR'
loFormSet.lcTempTran = gfTempName()

*!*************************************************************
*! Name      : lfvFactor
*! Developer : Waleed Hamed
*! Date      : 11/21/2006
*! Purpose   : Validate Factors
*! Example   :  =lfvFactor()
*!*************************************************************
FUNCTION lfvFactor
LPARAMETERS loFormSet, llBrowse

PRIVATE lcFactor, lnAlias
lnAlias = SELECT()

lcFactor   = loFormSet.AriaForm1.kbFactor.KeyTextBox.VALUE
lcOldValue = loFormSet.AriaForm1.kbFactor.KeyTextBox.OldValue

IF EMPTY(lcFactor)
  loFormSet.ariaform1.txtFactName.Value  = ''
  loFormSet.ariaForm1.cmdSelect.Enabled  = .F.
  loFormSet.ariaForm1.cmdProceed.Enabled = .F.
ENDIF

IF llBrowse OR (!EMPTY(lcFactor) .AND. !GFSEEK(lcFactor,'SycFact'))
  lcBrFields = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  SELECT SycFact
  lcFactor = IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,gnBrFSRow2, gnBrFSCol2,'','',;
                          'cFacCode','laBrowArr',.F., .F., .F., 'SycFact'),SycFact.cFacCode,SPACE(6))
ENDIF
loFormSet.AriaForm1.kbFactor.KeyTextBox.VALUE = lcFactor
loFormSet.AriaForm1.cmdSelect.ENABLED = !EMPTY(lcFactor)
loFormSet.AriaForm1.txtFactName.VALUE = IIF(!EMPTY(lcFactor),SycFact.cFacComp,loFormSet.AriaForm1.txtFactName.VALUE)

=lfObjStatus(loFormSet, !EMPTY(lcFactor))

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvCustomer
*! Developer : Waleed Hamed
*! Date      : 11/21/2006
*! Purpose   : To validate the customer code
*! Example   :  =lfvCustomer()
*!*************************************************************
FUNCTION lfvCustomer
LPARAMETERS loFormSet, llBrowse
PRIVATE xAccount

lcCustomer = loFormSet.AriaForm1.kbCustomer.KeyTextBox.VALUE
IF EMPTY(lcCustomer)
  loFormSet.ariaform1.kbCustomer.KeyTextBox.VALUE = ''
  loFormSet.ariaform1.txtCustName.Value = ''
  loFormSet.ariaform1.txtCustFact.Value = ''
ENDIF
IF llBrowse .OR. (!EMPTY(lcCustomer) .AND. !GFSEEK('M'+lcCustomer,'CUSTOMER'))
  xAccount = lcCustomer
  SELECT CUSTOMER
  DO CUSBROWM WITH xAccount
  lcCustomer = xAccount
ENDIF
IF !EMPTY(lcCustomer)
  loFormSet.ariaform1.kbCustomer.KeyTextBox.VALUE = lcCustomer
  loFormSet.ariaform1.txtCustName.Value = CUSTOMER.BtName
  loFormSet.ariaform1.txtCustFact.Value = IIF(EMPTY(CUSTOMER.FactAcct),'**NEW**',CUSTOMER.FactAcct)
ENDIF  

*!*************************************************************
*! Name      : lfObjStatus
*! Developer : Waleed Hamed
*! Date      : 11/21/2006
*! Purpose   : To enable/disable objects
*! Example   :  =lfObjStatus()
*!*************************************************************
FUNCTION lfObjStatus
LPARAMETERS loFormSet, lStatus

loFormSet.ariaform1.kbCustomer.Enabled   = lStatus
loFormSet.ariaform1.dtPickerFrom.Enabled = lStatus
loFormSet.ariaform1.dtpickerTo.Enabled   = lStatus

*!*************************************************************
*! Name      : lfvTrans
*! Developer : Waleed Hamed
*! Date      : 11/21/2006
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
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[Start]
*LPARAMETERS loFormSet
PARAMETERS loFormSet
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[End]
lcCustomer = loFormSet.AriaForm1.kbCustomer.KeyTextBox.VALUE
lcFactor   = loFormSet.AriaForm1.kbFactor.KeyTextBox.VALUE
ldTLDate = loFormSet.ariaForm1.dtPickerFrom.value
ldTHDate = loFormSet.ariaForm1.DtpickerTo.value

IF !USED(loFormSet.lcTempTran)
  CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lcTempTran) (cFacCode C(6), BatchNo C(2), dDate D,;
                Account C(5), Order C(6), Status C(1), ApprAmt N(14,2), Approval C(10),;
                cOrdDisp C(4), cOrdHDRes C(30), cGrantName C(30), cComment C(30),;
                FactAcc C(6), btName C(30), cSELECT C(1), lSELECT L, Type C(1),;
                cStatus C(10), start D, Complete D, OpenAmt N(14,2),BookAmt N(14,2)) 
  INDEX ON cSelect+Type+cFacCode+Order+DTOS(dDate) TAG 'SELECT'
  INDEX ON Type+Order TAG RangeSelct ADDITIVE
  INDEX ON Type+cFacCode+Order+DTOS(dDate) TAG (loFormSet.lcTempTran) ADDITIVE
ELSE
  SELECT(loFormSet.lcTempTran)
  ZAP
ENDIF
*IF loFormSet.llOrders
  lcFilter = 'Account = ALLTRIM(lcCustomer)'
  DO CASE
    CASE !EMPTY(ldTLDate) AND !EMPTY(ldTHDate)
      lcFilter = lcFilter + 'AND BETWEEN(dDate,ldTLDate,ldTHDate)'
    CASE  EMPTY(ldTLDate) AND !EMPTY(ldTHDate)
      lcFilter = lcFilter + 'AND dDate <= ldTHDate'
    CASE !EMPTY(ldTLDate) AND  EMPTY(ldTHDate)
      lcFilter = lcFilter + 'AND dDate >= ldTLDate'
  ENDCASE
  SELECT CITTRNLN
  LOCATE
  IF gfSeek(lcFactor)
    SCAN REST WHILE cFacCode+Order+DTOS(dDate) = lcFactor FOR &lcFilter
      WAIT WINDOW 'Order #: ' + Order NOWAIT
      SCATTER MEMVAR memo
      SELECT (loFormSet.lcTempTran)
      APPEND BLANK
      GATHER MEMVAR MEMO
      *SELECT OrdHdr
      gfSeek('O'+m.Order,'OrdHdr')
      gfSeek('M'+OrdHdr.Account,'Customer')
      SELECT (loFormSet.lcTempTran)
      REPLACE TYPE     WITH 'N'               ,;
              Start    WITH OrdHDr.Start      ,;
              Complete WITH OrdHDr.Complete   ,;
              OpenAmt  WITH OrdHDr.OpenAmt    ,;
              BookAmt  WITH OrdHdr.BookAmt    ,;
              FactAcc  WITH Customer.FactAcct ,;
              btName   WITH Customer.btName

      
    ENDSCAN
  ENDIF 
  SELECT (loFormSet.lcTempTran)
  LOCATE
  *loFormSet.llOrders = .F.
*ENDIF  

*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[Start]
IF oAriaApplication.MULTIINST 
  lcParmLst = "loFormSet,'EBORDCON','N'"
  =gfCallForm('EBSNDTR',oAriaApplication.ActiveModuleID,lcParmLst)  
ELSE
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[End]
  DO FORM oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleID+ "\EBSNDTR.SCX" WITH loFormSet,'EBORDCON','N'
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[Start]
ENDIF
*:B609502,1 MMT 01/16/2011 Modify ROR Programs to work on SAAS[End]

SET ORDER TO TAG 'SELECT' IN (loFormSet.lcTempTran)
IF SEEK('Y',loFormSet.lcTempTran)
  loFormSet.Ariaform1.cmdProceed.ENABLED = .T.
ELSE
  loFormSet.Ariaform1.cmdProceed.ENABLED = .F.
ENDIF
SET ORDER TO TAG (loFormSet.lcTempTran) IN (loFormSet.lcTempTran)

*khalid

*!*************************************************************
*! Name      : lfvProceed
*! Developer : Waleed Hamed
*! Date      : 11/21/2006
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
LPARAMETERS loFormSet

=lfPrint(loFormSet)
loFormSet.ariaForm1.cmdProceed.Enabled = .F.

*!*************************************************************
*! Name      : lfPrint
*! Developer : Waleed Hamed
*! Date      : 11/21/2006
*! Purpose   : To print the report
*!*************************************************************
FUNCTION lfPrint
LPARAMETERS loFormSet

PRIVATE loOGScroll
SELECT (loFormSet.lcTempTran)
SET ORDER TO 'Select'
IF SEEK (' ')
  DELETE REST WHILE cSelect+Type+cFacCode+Order+DTOS(dDate) = ' '
ENDIF
IF SEEK ('N')
  DELETE REST WHILE cSelect+Type+cFacCode+Order+DTOS(dDate) = 'N'
ENDIF
  
*SET FILTER TO cSelect='»'
LOCATE

*-- Create a Dummy Object of the Option Grid...
LOCAL lnDataSess, lcDevice, lcClassDir, oOptionGrid
lnDataSess = SET("Datasession")
lcDevice   = oAriaApplication.gcDevice
oAriaApplication.gcDevice = 'PRINTER'

lcClassDir   = ADDBS(oAriaApplication.ClassDir)
oOptionGrid  = NEWOBJECT("optiongrid",lcClassDir+"optiongrid.vcx")
loOGScroll   = oOptionGrid.OptionGrid.oHost
lcOGPlatForm = ''
loOgScroll.lcOGPlatForm  = 	''
loOgScroll.lcOGLastForm  = 'EBORDCN'
loOGScroll.llPrintPDF = .F.
loogScroll.cCROrientation = 'P'
LoOGScroll.llCrystal = .T.

DIMENSION loOGScroll.laSelFile[1,3]
loOGScroll.laSelFile = ''

DIMENSION LOogsCROLL.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  loFormSet.lcTempTran + ".DBF"
SELECT (loFormSet.lcTempTran)
USE 
=gfDispRe('EBORDCN')

SET DATASESSION TO (lnDataSess)

oOptionGrid = .NULL.
LOogsCROLL =  .NULL.

IF FILE(oAriaApplication.WorkDir +  loFormSet.lcTempTran + ".DBF")
  ERASE(oAriaApplication.WorkDir +  loFormSet.lcTempTran + ".DBF")
ENDIF  
IF FILE(oAriaApplication.WorkDir +  loFormSet.lcTempTran + ".CDX")
  ERASE(oAriaApplication.WorkDir +  loFormSet.lcTempTran + ".CDX")
ENDIF  

*!*************************************************************
*! Name      : FileExist
*! Developer : Waleed Hamed
*! Date      : 11/21/2006
*! Purpose   : To return the existance of the file
*!*************************************************************
*! Parameters: cFileToCheckFor 
*!*************************************************************
FUNCTION FileExist
LPARAMETERS cFileToCheckFor

RETURN loOGScroll.FileExist(cFileToCheckFor)
