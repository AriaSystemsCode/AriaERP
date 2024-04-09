*!*************************************************************
*! Name      : EBORDCN
*! Developer : Wael Ali Mohamed
*! Date      : 30/12/1999
*! Purpose   : Print Order Confirmation Report
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************

STORE .F. TO llBrowse
STORE .T. TO llOrders 
lcWindTitl = 'Print Send Sales Orders Confirmation'

=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
=gfOpenFile(gcDataDir+'CITTRNLN',gcDataDir+'CITTRNLN','SH')
=gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')

*-- Initialize variables
lcFactor   = SPACE(6)
lcOldValue = SPACE(1)
lcCustomer = SPACE(5)
lcCustName = SPACE(30)
lcCustFact = SPACE(10)
STORE '' TO lcDispDesc,lcReason

lnTrans = 0
STORE {} TO ldTLDate,ldTHDate
lcKeyBmp   = gcBmpHome + "ExtKey.BMP"
lcSelBmp   = gcBmpHome + "SEL.BMP"
lcProceed  = gcBmpHome + "proceed.bmp"
lcClose    = gcBmpHome + "Close2.bmp"
lcTempTran = gfTempName()
SELECT CITTRNLN
SET RELATION TO 'M'+Account INTO CUSTOMER
SET RELATION TO 'O'+Order   INTO ORDHDR ADDITIVE
lcBrowseTl = ''
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

DO (gcScrDir+gcWinAppl+"\EBORDCON.SPX")

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
IF EMPTY(lcFactor)
  SHOW GET ebTransactions DISABLE
ELSE
  SHOW GET ebTransactions ENABLE
ENDIF
=lfRefresh('EBORDCON')
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
  IF xAccount<>lcCustomer
    llOrders=.T.
  ENDIF    
  lcCustomer = xAccount
ENDIF
lcCustFact = IIF(EMPTY(lcCustomer),'',IIF(EMPTY(Customer.FactAcct),'**NEW**',Customer.FactAcct))
lcCustName = IIF(EMPTY(lcCustomer),'',Customer.BtName)
=lfRefresh('EBORDCON')
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

IF llOrders
  lcFilter = 'Account = ALLTRIM(lcCustomer)'
  DO CASE
    CASE !EMPTY(ldTLDate) AND !EMPTY(ldTHDate)
      lcFilter = lcFilter + 'AND BETWEEN(dDate,ldTLDate,ldTHDate)'
    CASE  EMPTY(ldTLDate) AND !EMPTY(ldTHDate)
      lcFilter = lcFilter + 'AND dDate <= ldTHDate'
    CASE !EMPTY(ldTLDate) AND  EMPTY(ldTHDate)
      lcFilter = lcFilter + 'AND dDate >= ldTLDate'
  ENDCASE
 
  SELECT CITTRNLN.*, SPACE(1) AS cSELECT FROM CITTRNLN ;
  WHERE cFacCode+Order+STR(batchno,2) = ALLTRIM(lcFactor) AND &lcFilter ;
  INTO DBF (gcWorkDir+lcTempTran)
  INDEX ON cSelect+cFacCode+Order+DTOS(dDate) TAG 'SELECT'
  INDEX ON cFacCode+Order+DTOS(dDate) TAG (lcTempTran) ADDITIVE
  llOrders = .F.
ENDIF  
lcTranBrow = 'Select Sales Orders'
DO (gcScrDir+gcWinAppl+"\EBSNDTR.SPX") WITH 'O'
SET ORDER TO TAG 'SELECT' IN (lcTempTran)
IF SEEK('»',lcTempTran)
  SHOW GET pbProceed ENABLE
ELSE
  SHOW GET pbProceed DISABLE
ENDIF
SET ORDER TO TAG (lcTempTran) IN (lcTempTran)

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

SELECT (lcTempTran)
SET RELATION TO cFacCode+ORder INTO CITTRNLN
SET FILTER TO cSelect='»'
IF pSetup(.T.)
 DO gfDispRep WITH (gcWinAppl+'\EBORDCN.FRX')
ENDIF
SET FILTER TO 

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
lcKey = cFacCode+Order+DTOS(dDate)
DO CASE
  CASE lcType = 'S'
    REPLACE cSelect WITH IIF(cSelect='»',' ','»')
  CASE lcType = 'A'
    REPLACE ALL cSelect WITH '»' 
  CASE lcType = 'N'
    REPLACE ALL cSelect WITH ' '
  CASE lcType = 'V'
    REPLACE ALL cSelect WITH IIF(cSelect='»',' ','»') 
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
ON KEY LABEL ALT+B

*!*************************************************************
*! Name      : lfBrowTran
*! Developer : WAM
*! Date      : 12/30/1999
*! Purpose   : Browse invoices/Credit Memos and Sales Orders
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
lcFields = lcFields + "Order  :R,Approval :R :H='Approval#',cSt=IIF(Status='H','Hold',IIF(Status='A','Approved',IIF(Status='D','Declined',''))) :R :H='Status',"
lcFields = lcFields + "dDate :R :H='Batch Date',Account :R,OrdHdr.Start:R,OrdHdr.Complete,OrdHdr.OpenAmt :P='9999999.99':H='Open Amnt.'"
lcTranBrow = 'Select Sales Orders'
BROWSE FIELDS &lcFields  ;
       WINDOW EBSNDTR1   ;
       IN WINDOW EBSNDTR ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE             ;
       NOCLEAR           ;
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
*! Name      : lfvTDate
*! Developer : WAM
*! Date      : 12/30/1999
*! Purpose   : Validate Date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvTDate()
*!*************************************************************
FUNCTION lfvTDate
PARAMETER ldDate

IF ldDate<> lcOldValue
  llOrders = .T.
ENDIF


*!*************************************************************
*! Name      : lfStatus
*! Developer : WAM
*! Date      : 12/30/1999
*! Purpose   : Get order status and Disposition Description
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfStatus()
*!*************************************************************
FUNCTION lfStatus
PRIVATE lcStatus
STORE '' TO lcStatus,lcDispDesc,lcReason
DO CASE
  CASE CitTrnLn.Status = 'A'
    lcStatus = 'APPROVED'
  CASE CitTrnLn.Status = 'H'
    lcStatus = 'HOLD'
  CASE CitTrnLn.Status = 'D'
    lcStatus = 'DECLINED'
ENDCASE
lcDispDesc = CitTrnLn.cOrdHDRes
RETURN (lcStatus)
