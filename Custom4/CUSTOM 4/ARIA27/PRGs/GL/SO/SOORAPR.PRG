************************************************************************
*: Program file  : SoOrApr.PRG
*: Program desc. : Approve/Decline Orders
*:
*:         System: Aria Apparel System
*:      Developer: WAM - Wael Aly Mohamed
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Modifications      :
*B602179,1 AMM 11/25/1998 Erase temporary files when quitting the program
*B602581,1 WAM 02/24/1999 Speed up program
*B802099,1 Reham On 04/07/99 Add an incremental search in the customer browse.
*C200089,1 Reham On 07/22/1999
*C200089,1 1- When Order header status changed to "Open", Call process Id to
*C200089,1    execute special function.
*C200089,1 2- When Order header status changed to "Hold", Call process Id to
*C200089,1    execute special function.
*B802389,1 ALB 08/22/1999 Fixed a Bug which occured when selectin an order
*B802389,1     or an account then running Sales Order or Customers Screen .
*B802389,1     Returning to prg ,the selected Order or Account is the first in browse.
*B802391,1 ALB 08/24/1999 Fixed a Bug of Orders Locator ,which accepted only
*B802391,1     5 digits (should be 6).And of course didn't locate the order,if exists. 
*B603201,1 RAMY 10/13/1999 Change the Customer browse to browse the net of 
*B603201,1                 (NetBal - ChgBack) insted of browseing the TotAge amount
*E#301307,1 SSH Increase Approval amount and qty fields length.
*B603485,1 RAMY 03/05/2000	When the customer orders exceeds his cridet 
*B603485,1                  limit should not complete approving
*B603506,1 ABD 06/12/2000 Increase amount sizes in Browse fields to fit currencies.
*B603506,1 ABD            [BookAmt,ShipAmt,CancelAmt,Openamt] modif. in screen soorapr6.scx
*B603830,1 MHM 08/27/2000 The order approval program allows the user to change the status 
*B603830,1 MHM            of the orders from hold to open while the account status is Hold.
*B803797,1 ABD 11/08/2000 Add Wait window when collecting data for selected account. 
*:*************************************************************
DECLARE lafolders[2,2],lafoldwinds[2,2],laWndObj[2,3]
DECLARE laCodes[1,10],laReason[1,2]

STORE ''  TO laCodes,laReason,lcCustomer,lcSelect,lcApproval,lcFolder,lcExpToSeek,;
             lcBrowseTl,lcAccount,lcOrderNo
*B802389,1
 STORE '' TO lcOrdNo,lcAcct
 STORE .F. TO llOrdFlag
*B802389,1(end) 
STORE ''  TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinCh3,lcWinCh4,lcWinCh5,lcWinCh6
STORE 0   TO lnReason,lnApproved,lnStatus,lnAMarker,lnOMarker,lnAvailable
STORE 1   TO lnActFolder

*B602581,1 Initialiaze variables
llSelAll  = .F.
STORE 0 TO lnOpenAmt,lnHoldAmt,lnOldCrLimit
*B602581,1 (End)


*B603485,1 RAMY (START)
lcTmpCdx = gfTempName()
*B603485,1 RAMY (end)
IF !gfSetup()
  RETURN
ENDIF
STORE .T. TO llBrowOrd
lcAccTitl = 'Accounts'  && Accounts browse title.
lcOrdTitl = 'Orders'    && Orders browse title.

lafolders[1,1] = 'Accounts'
lafolders[1,2] = 1
lafolders[2,1] = 'Orders'
lafolders[2,2] = 2
lnnofld = 2
lcwfoldchng = '=lfActFolder()'
lnfoldercend = 102.00
lnfolderrend = 2.000
=gfOpenFile(gcSysHome+'SycInt',gcSysHome+'cContCode','SH')
SELECT ORDHDR
SET ORDER TO TAG Ordacct
SELECT CUSTOMER
IF !WEXIST(gcBaseWind)
  
  lcCustomer = gfTempName() 
  lcSelect   = gfTempName()
  lcWinCh0   = gfTempName()
  lcWinCh1   = gfTempName()
  lcWinCh2   = gfTempName()
  lcWinCh3   = gfTempName()
  lcWinCh4   = gfTempName()
  lcWinCh5   = gfTempName()
  lcWinCh6   = gfTempName()
  lcfolder   = gfTempName()   && Folder Window Name
  lcfoldprnt = gcBaseWind     && window parent name for the folder
  lnactfolder = 1             && active folder
  lafoldwinds[1,1] = 'Accounts'
  lafoldwinds[1,2] = lcWinCh1
  lafoldwinds[2,1] = 'Orders'
  lafoldwinds[2,2] = lcWinCh2
  
  *B602581,1 Use Customer master file
  CREATE TABLE (gcWorkDir+lcCustomer) (Account C(5),cSelect C(1))
  INDEX ON Account TAG (lcCustomer)

  *B602581,1 Commented out
  *SELECT CUSTOMER
  *=AFIELDS(laFileStru)
  *lnFileStru = ALEN(laFileStru,1)
  *DIMENSION laFileStru[lnFileStru+3,4]
  *laFileStru[lnFileStru+1,1] = 'CSELECT'
  *laFileStru[lnFileStru+1,2] = 'C'
  *laFileStru[lnFileStru+1,3] = 1
  *laFileStru[lnFileStru+1,4] = 0
  *laFileStru[lnFileStru+2,1] = 'nOpenOrd'
  *laFileStru[lnFileStru+2,2] = 'N'
  *laFileStru[lnFileStru+2,3] = 11
  *laFileStru[lnFileStru+2,4] = 2
  *laFileStru[lnFileStru+3,1] = 'nHoldOrd'
  *laFileStru[lnFileStru+3,2] = 'N'
  *laFileStru[lnFileStru+3,3] = 11
  *laFileStru[lnFileStru+3,4] = 2
  *CREATE TABLE (gcWorkDir+lcCustomer) FROM ARRAY laFileStru
  *INDEX ON cSelect+Account TAG (lcSelect)
  *INDEX ON Account TAG (lcCustomer) ADDITIVE
  *SELECT CUSTOMER
  *=SEEK('M') 
  *SCAN REST WHILE Type+Account+store='M' FOR STATUS <> 'X'
  *  SCATTER MEMVAR
  *  INSERT INTO (lcCustomer) FROM MEMVAR
  *  SELECT ORDHDR
  *  =SEEK(Customer.Account+'O')
  *  SUM REST OpenAmt TO lnOpenAmt ;
  *  WHILE Account+Status+Bulk+cOrdType+Order=Customer.Account+'O'
  *  =SEEK(Customer.Account+'H')
  *  SUM REST OpenAmt TO lnHoldAmt ;
  *  WHILE Account+Status+Bulk+cOrdType+Order=Customer.Account+'H'
  *  SELECT (lcCustomer)
  *  REPLACE nOpenOrd WITH lnOpenAmt ,;
  *          nHoldOrd WITH lnHoldAmt 
  *ENDSCAN
  *SET ORDER TO TAG Ordacct IN ORDHDR  
  *B602581,1 (End)
  
  laCodes[1,1]  = 'DECL_CODE'
  laCodes[1,2]  = 'laReason'
  laCodes[1,3]  = 'lnReason'
  laCodes[1,4]  = ''
  laCodes[1,5]  = .F.
  laCodes[1,6]  = .F.
  laCodes[1,7]  = 'ORDHDR'
  laCodes[1,8]  = 'ORDACCT'
  laCodes[1,9]  = 'ACCOUNT+CORDTYPE+ORDER'
  laCodes[1,10] = 'DECL_CODE'
ENDIF

laWndObj[1,1] = lcWinCH1
laWndObj[1,2] = 'ibAccount'
laWndObj[1,3] = 'lnAvailable'
laWndObj[2,1] = 'GWCCONTRL1'
laWndObj[2,2] = 'pbcptask'
laWndObj[2,3] = 'PBCLS'

DECLARE laPanelObj[2,3]
STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbCustoer'
laPanelObj[1,2] = gcBmpHome+"ExtKey.bmp"
laPanelObj[1,3] = [VALID lfvCustomer() MESSAGE 'Customer Inquire']
laPanelObj[2,1] = 'pbCusOrd'
laPanelObj[2,2] = gcBmpHome+"orders.bmp"
laPanelObj[2,3] = [VALID lfOrders() MESSAGE 'Customer Orders']

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)
lcBaseFile=''

*B602581,1 Browse from Customer master file
*SELECT (lcCustomer)
*SET RELATION TO Type+Account+Store INTO Customer
*SET RELATION TO Account+'O' INTO ORDHDR ADDITIVE
*B602581,1 (End)

DO (gcScrDir+gcWinAppl+"\SOORAPR.SPX")
=lfClearKey()
=gfCloseFille('SycInt')
RELEASE WINDOW (lcAccTitl)
RELEASE WINDOW (lcOrdTitl)
IF glQuitting
  USE IN (lcCustomer)
  *B602179,1 AMM Erase temporary files when quitting the program start
  ERASE (gcWorkDir+lcCustomer+'.DBF')
  ERASE (gcWorkDir+lcCustomer+'.CDX')
  *B602179,1 AMM  end
  *B603485,1 RAMY (START)
  SELECT ORDHDR
  CLOSE INDEXES
  ERASE (gcWorkDir+lcTmpCdx+'.CDX')
  *B603485,1 RAMY (END)
ENDIF



*!*************************************************************
*! Name      : lfActFolder
*! Developer : Wael Aly Mohamed
*! Date      : 08/01/1997
*! Purpose   : Change Folder
*!*************************************************************
*! Calls     : lfBrowse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfActFolder()
*!*************************************************************
FUNCTION lfActFolder
DO CASE
  CASE lnActFolder=1
    laWndObj[1,1] = lcWinCH1
    laWndObj[1,2] = 'ibAccount'
    laWndObj[1,3] = 'lnAvailable'
    SHOW GETS WINDOW (lcWinCh6) DISABLE ONLY
    =lfvSelect('')
  CASE lnActFolder=2
    *B602581,1 Update total order open and hold amount
    SELECT ORDHDR
    *B803797,1 ABD - Add Wait window when collecting data for selected account. [Begin]
    WAIT WINDOW "Collecting data for account " + Customer.Account + " ...!" NOWAIT
    *B803797,1 ABD - [End]
    SET ORDER TO TAG Ordbulk
    =SEEK(Customer.Account+'O')
    SUM REST OpenAmt TO lnOpenAmt ;
    WHILE Account+Status+Bulk+cOrdType+Order=Customer.Account+'O' FOR cOrdType='O'
    =SEEK(Customer.Account+'H')
    SUM REST OpenAmt TO lnHoldAmt ;
    WHILE Account+Status+Bulk+cOrdType+Order=Customer.Account+'H' FOR cOrdType='O'
    SET ORDER TO TAG Ordacct
    SET KEY TO lcAccount+'O'
    =SEEK(lcAccount+'O')
    LOCATE REST WHILE Account+cOrdType+Order=lcAccount+'O' FOR INLIST(Status,'O','H');
    =lfRefresh(lcWinCh6)
    *B602581,1 (End)
    SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY
    =IIF(llBrowOrd,lfBrowse(),.T.)
    =lfwOrdBrow()
    laWndObj[1,1] = lcWinCH2
    laWndObj[1,2] = 'ibOrder'
    laWndObj[1,3] = IIF(ORDHDR.STATUS='O','lnApproved','lnReason')
    *B803797,1 ABD - Clear wait for wait window. [Begin]
    WAIT CLEAR
    *B803797,1 ABD - [End]
ENDCASE

*!*************************************************************
*! Name      : lfwScreen
*! Developer : Wael Aly Mohamed
*! Date      : 08/01/1997
*! Purpose   : Main screen When function
*!*************************************************************
*! Calls     : lfChngFolder,lfvCrLimit,lfwAccBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfwScreen()
*!*************************************************************
FUNCTION lfwScreen

SHOW GET pbBrws DISABLE
laCtrStat[10]= 'DISABLE'

*B602581,1 Browse from master customer file
*SELECT (lcCustomer)
*lnAMarker = RECNO()
*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnAMarker,'>',' ')  :H=' ':R:1:W=.F.,;
       cSelect  :H= ' ' :R :W=.F.,;
       Account  :H= 'Account' :R ,;
       btName   :H= 'Name' :R :27,;
       nHoldOrd :H= 'Hold Ord.' :R ,;
       nOpenOrd :H= 'Open Ord.' :R ,;
       TotAge   :H= 'Bal. Due'  :P='99999999.99' :R ,;
       CrLimit  :H= 'Credit Limit' :P= '9999999' :V=lfvCrLimit() ,;
       CrAvail  :H= 'Available' :P='99999999.99' :R ;
       WINDOW (lcWinCh3)  ;
       IN WINDOW (lcWinCh1) ;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       WHEN lfwAccBrow();
       VALID :F lfvAccBrow();
       TITLE lcAccTitl
SELECT Customer
lnAMarker = RECNO()

*B603201,1 RAMY  Change the net value in the browse [start]

*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnAMarker,'>',' ')  :H=' ':R:1:W=.F.,;
       cSelect =IIF(SEEK(Account,lcCustomer),&lcCustomer..cSelect,IIF(llSelAll,"»",'')) :H= ' ' :R :W=.F.,;
       Account  :H= 'Account'  :R,;
       btName   :H= 'Name'     :R,;
       TotAge   :H= 'Bal. Due'     :P='99999999.99' :R ,;
       CrLimit  :H= 'Credit Limit' :P= '9999999' :W=lfwCrLimit() :V=lfvCrLimit() ,;
       CrAvail  :H= 'Available'    :P='99999999.99' :R ,;
       cFacCode :H= 'Factor'   :R,;
       FactAcct :H='Fact Acnt' :R;
       WINDOW (lcWinCh3)  ;
       IN WINDOW (lcWinCh1) ;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       KEY 'M'          ;
       WHEN lfwAccBrow();
       TITLE lcAccTitl
*E#301307,1 SSH Increase Approval amount and qty fields length.
*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnAMarker,'>',' ')  :H=' ':R:1:W=.F.,;
       cSelect =IIF(SEEK(Account,lcCustomer),&lcCustomer..cSelect,IIF(llSelAll,"»",'')) :H= ' ' :R :W=.F.,;
       Account  :H= 'Account'  :R,;
       btName   :H= 'Name'     :R,;
       NetBal   :H= 'Bal. Due'     :P='99999999.99' :R ,;
       CrLimit  :H= 'Credit Limit' :P= '9999999' :W=lfwCrLimit() :V=lfvCrLimit() ,;
       CrAvail  :H= 'Available'    :P='99999999.99' :R ,;
       cFacCode :H= 'Factor'   :R,;
       FactAcct :H='Fact Acnt' :R;
       WINDOW (lcWinCh3)  ;
       IN WINDOW (lcWinCh1) ;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       KEY 'M'          ;
       WHEN lfwAccBrow();
       TITLE lcAccTitl
BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnAMarker,'>',' ')  :H=' ':R:1:W=.F.,;
       cSelect =IIF(SEEK(Account,lcCustomer),&lcCustomer..cSelect,IIF(llSelAll,"»",'')) :H= ' ' :R :W=.F.,;
       Account  :H= 'Account'  :R,;
       btName   :H= 'Name'     :R,;
       NetBal   :H= 'Bal. Due'     :P='9999999999.99' :R ,;
       CrLimit  :H= 'Credit Limit' :P= '999999999' :W=lfwCrLimit() :V=lfvCrLimit() ,;
       CrAvail  :H= 'Available'    :P='9999999999.99' :R ,;
       cFacCode :H= 'Factor'   :R,;
       FactAcct :H='Fact Acnt' :R;
       WINDOW (lcWinCh3)  ;
       IN WINDOW (lcWinCh1) ;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       KEY 'M'          ;
       WHEN lfwAccBrow();
       TITLE lcAccTitl
*E#301307,1 SSH (End)
*B603201,1 RAMY [end]

*B602581,1 (End)
=lfChngFolder(lnActFolder)

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Wael Aly Mohamed
*! Date      : 08/01/1997
*! Purpose   : READ Activate function of main screen
*!*************************************************************
*! Calls     : lfClearKey
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*!*************************************************************
*! Name      : lfDOrdApr
*! Developer : Wael Aly Mohamed
*! Date      : 08/01/1997
*! Purpose   : Deactivate function of main screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDOrdApr()
*!*************************************************************
FUNCTION lfDOrdApr

IF WONTOP()=lcAccTitl .OR. WONTOP()=lcOrdTitl
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  DO CASE 
    CASE WONTOP()=lcAccTitl
      ON KEY LABEL TAB     DO lpTab WITH lcWinCh4,'pbSelect'
      ON KEY LABEL BACKTAB DO lpTab WITH lcWinCh4,'lnAvailable'
      ON KEY LABEL ALT+S DO lpTab WITH lcWinCh4,'pbSelect',.T.
      ON KEY LABEL ALT+A DO lpTab WITH lcWinCh4,'pbSelAll',.T.
      ON KEY LABEL ALT+N DO lpTab WITH lcWinCh4,'pbSelNone',.T.
      ON KEY LABEL ALT+I DO lpTab WITH lcWinCh4,'pbInvert',.T.
      ON KEY LABEL ALT+P DO lpTab WITH lcWinCh4,'pbApprove',.T.
      ON KEY LABEL ALT+V DO lpTab WITH lcWinCh4,'lnAvailable',.T.
    CASE WONTOP()=lcOrdTitl
      ON KEY LABEL TAB     DO lpTab WITH lcWinCh6,'lnStatus'
      ON KEY LABEL BACKTAB DO lpTab WITH lcWinCh6,'lnApproved'
  ENDCASE
  *B802099,1 Reham On 04/08/99   *** Begin ***
  *B802099,1 Trap the incremental search in the browse.
  FOR lnChrToTrap = 32 TO 126
    ON KEY LABEL (CHR(lnChrToTrap)) DO lfIncSearch
  ENDFOR
  *B802099,1 Reham On 04/08/99   *** End   ***
ELSE
  glFromBrow = .F.
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfBrowse
*! Developer : WAM 
*! Date      : 08/01/1996
*! Purpose   : Browse Customers and Orders
*!*************************************************************
*! Calls     : lfwOrdBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfBrowse()
*!*************************************************************
FUNCTION lfBrowse
SELECT ('ORDHDR')
lnOMarker = RECNO()
RELEASE WINDOW (lcOrdTitl)
*B602581,1 Browse customers from master customer file
*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnOMarker,'>',' ')  :H=' ':R:1:W=.F.,;
       Order  :H = 'Order#'  :R ,;
       lcStatus=IIF(Status='O','Open','Hold') :H = 'Status' :R ,;
       lcSesDesc=gfCodDes(SEASON,'SEASON'):H="Season" :31,;
       cFacCode :H= 'Factor'     :R ,;
       Complete :H= 'Complete'   :R ,;
       OpenAmt  :H= 'Open Amnt.' :R ,;
       Approval :H= 'Aprvl-No'   :R ,;
       ApprAmt  :H= 'Approved'   :R ;
       WINDOW (lcWinCh5) ;
       IN WINDOW (lcWinCh2);
       KEY &lcCustomer..Account ;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       VALID :F lfvAccBrow();       
       WHEN lfwOrdBrow();
       TITLE lcOrdTitl
BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnOMarker,'>',' ')  :H=' ':R:1:W=.F.,;
       Order  :H = 'Order#'  :R ,;
       lcStatus=IIF(Status='O','Open','Hold') :H = 'Status' :R ,;
       lcSesDesc=gfCodDes(SEASON,'SEASON'):H="Season" :31,;
       cFacCode :H= 'Factor'     :R ,;
       Complete :H= 'Complete'   :R ,;
       OpenAmt  :H= 'Open Amnt.' :R ,;
       Approval :H= 'Aprvl-No'   :R ,;
       ApprAmt  :H= 'Approved'   :R ;
       WINDOW (lcWinCh5) ;
       IN WINDOW (lcWinCh2);
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       FOR INLIST(Status,'O','H');
       WHEN lfwOrdBrow();
       TITLE lcOrdTitl
*B602581,1 (End)
llBrowOrd=.F.

*!*************************************************************
*! Name      : lfwAccBrow
*! Developer : WAM 
*! Date      : 08/01/1996
*! Purpose   : Refresh Customer Browse
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwAccBrow()
*!*************************************************************
FUNCTION lfwAccBrow

*B602581,1 Browse customers from master customer file
*lnAMarker = RECNO(lcCustomer)
*SHOW WINDOW (lcAccTitl) REFRESH SAME
*=lfRefresh(lcWinCh0)
*IF cSelect = SPACE(1)
*  SHOW GET pbSelect,1 PROMPT '\<Select'
*ELSE
*  SHOW GET pbSelect,1 PROMPT 'Un\<Select'
*ENDIF
*lcAccount = &lcCustomer..ACCOUNT


lnAMarker = RECNO('CUSTOMER')
SHOW WINDOW (lcAccTitl) REFRESH SAME
=SEEK(Customer.cCont_Cod2,'SycInt')
=lfRefresh(lcWinCh0)
lcSelect=IIF(SEEK(Account,lcCustomer),&lcCustomer..cSelect,IIF(llSelAll,"»",' '))
IF lcSelect = SPACE(1)
  SHOW GET pbSelect,1 PROMPT '\<Select'
ELSE
  SHOW GET pbSelect,1 PROMPT 'Un\<Select'
ENDIF
*B602581,1 (End)
*B802389,1
=lfLocateRec('A')
*B802389,1 (End)

*!*************************************************************
*! Name      : lfwOrdBrow
*! Developer : WAM 
*! Date      : 08/01/1996
*! Purpose   : Refresh Order Browse
*!*************************************************************
*! Calls     : gfwCodePop
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwOrdBrow()
*!*************************************************************
FUNCTION lfwOrdBrow

lnOMarker = RECNO('ORDHDR')
SHOW WINDOW (lcOrdTitl) REFRESH SAME
lcApproval = OrdHdr.Approval
lnApproved = OrdHdr.ApprAmt
lnStatus   = IIF(OrdHdr.Status='O',1,2)
SHOW GET ibOrder  ENABLE
IF OrdHdr.Status = 'O'
  SHOW GET lnStatus   ENABLE
  SHOW GET lcApproval ENABLE
  SHOW GET lnApproved ENABLE
  =gfwCodePop(@laCodes,'DECL_CODE','N')
  SHOW GET lnReason DISABLE
ENDIF
IF OrdHdr.Status = 'H'
  SHOW GET lnStatus   ENABLE
  SHOW GET lcApproval DISABLE
  SHOW GET lnApproved DISABLE
  lnReason=ASCAN(laReason,DECL_CODE)/2
  =gfwCodePop(@laCodes,'DECL_CODE','T')
  SHOW GET lnReason ENABLE
ENDIF
*B802389,1
=lfLocateRec('O')
*B802389,1 (End)

*!*************************************************************
*! Name      : lfvStatus
*! Developer : WAM 
*! Date      : 08/01/1996
*! Purpose   : Approve/Decline order
*!*************************************************************
*! Calls     : gfGetExSin,gfwCodePop
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvStatus()
*!*************************************************************
FUNCTION lfvStatus
*B603830,1 MHM [Begin]
*PRIVATE lcUntSin, lcExRSin
PRIVATE lcUntSin, lcExRSin,lcStatus 
*B603830,1 MHM [End]

IF (lnStatus = 1 .AND. ORDHDR.STATUS='O') .OR. (lnStatus = 2 .AND. ORDHDR.STATUS='H')
  RETURN
ENDIF

*B603830,1 MHM [Begin]
IF (lnStatus = 1 .AND. ORDHDR.STATUS='H').AND. (CUSTOMER.STATUS $'HX')
  lnStatus = 2
  lcStatus=IIF (CUSTOMER.STATUS = 'H','Hold','Cancelled')
  = gfModalGen("TRM32083",'DIALOG', ORDHDR.ACCOUNT+"|"+lcStatus)
  SHOW GET lnStatus   ENABLE
  RETURN
ENDIF
*B603830,1 MHM [End]

lcUntSin = ' '
lcExRSin = gfGetExSin(@lcUntSin,OrdHdr.cCurrCode)
IF lnStatus = 1
  lnOpen = CEILING(ORDHDR.OPENAMT) &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
  SELECT Customer
  REPLACE CrAvail WITH CrAvail - lnOpen
  SELECT ORDHDR
  REPLACE STATUS     WITH 'O'           ,;
          DECL_CODE  WITH SPACE(2)      ,;
          APPRAMT    WITH CEILING(OPENAMT)
  STORE APPRAMT TO lnApproved
  =gfwCodePop(@laCodes,'DECL_CODE','N')
  SHOW GET lcApproval ENABLE
  SHOW GET lnApproved ENABLE
  SHOW GET lnReason   DISABLE
  
  *C200089,1 Reham On 07/22/1999  *** Begin ***
  *C200089,1 When Order header status changed to "Open", Call process Id to
  *C200089,1 execute special function.
  IF ASCAN(laEvntTrig , PADR('APR_OPN',10)) <> 0
    llReturn = gfDoTriger('SOORAPR',PADR('APR_OPN',10))
  ENDIF
  *C200089,1 Reham On 07/22/1999  *** End   ***
  
  _CUROBJ = OBJNUM(lcApproval)
  
  *B602581,1 Update total order open and hold amount
  *SELECT (lcCustomer)
  *REPLACE nOpenOrd WITH nOpenOrd + lnOpen ,;
          nHoldOrd WITH nHoldOrd - lnOpen
  lnOpenAmt = lnOpenAmt + lnOpen
  lnHoldAmt = lnHoldAmt - lnOpen
  =lfRefresh(lcWinCh6)
  *B602581,1 (End)
ELSE
  SELECT Customer
  lnOpen = OrdHdr.ApprAmt &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
  REPLACE CrAvail WITH CrAvail + lnOpen
  SELECT ORDHDR
  REPLACE STATUS     WITH 'H'       ,;
          APPRAMT    WITH 0         ,;
          APPROVAL   WITH SPACE(10)
          
  STORE '' TO lcApproval
  STORE 0  TO lnApproved
  SHOW GET lcApproval DISABLE
  SHOW GET lnApproved DISABLE
  SHOW GET lnReason   ENABLE
  =gfwCodePop(@laCodes,'DECL_CODE','D')  
  _CUROBJ = OBJNUM(lnReason)  

  *C200089,1 Reham On 07/28/1999  *** Begin ***
  *C200089,1 Update the hold reason before calling the trigger.
  =lfvReason()
  *C200089,1 When Order header status changed to "Open", Call process Id to
  *C200089,1 execute special function.
  IF ASCAN(laEvntTrig , PADR('APR_HLD',10)) <> 0
    llReturn = gfDoTriger('SOORAPR',PADR('APR_HLD',10))
  ENDIF
  *C200089,1 Reham On 07/28/1999  *** End   ***
  
  *B602581,1 Update total order open and hold amount
  *SELECT (lcCustomer)
  *REPLACE nOpenOrd WITH nOpenOrd - lnOpen ,;
          nHoldOrd WITH nHoldOrd + lnOpen
  lnOpenAmt = lnOpenAmt - lnOpen
  lnHoldAmt = lnHoldAmt + lnOpen
  =lfRefresh(lcWinCh6)
  *B602581,1 (End)

ENDIF  
*B602581,1 Commented out
*SELECT (lcCustomer)
*REPLACE CrAvail  WITH Customer.CrAvail
*B602581,1 (End)

SELECT ORDHDR

*!*************************************************************
*! Name      : lfvReason
*! Developer : WAM 
*! Date      : 08/01/1996
*! Purpose   : Update Order hold reason
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvReason()
*!*************************************************************
FUNCTION lfvReason

SELECT ORDHDR
=RLOCK()
REPLACE DECL_CODE WITH SUBSTR(laReason[lnReason,2],1,6)
UNLOCK

*!*************************************************************
*! Name      : lfvApproval
*! Developer : WAM 
*! Date      : 08/01/1996
*! Purpose   : Update Order Approval number
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvApproval()
*!*************************************************************
FUNCTION lfvApproval

SELECT ORDHDR
=RLOCK()
REPLACE Approval WITH lcApproval
UNLOCK

*!*************************************************************
*! Name      : lfvApprAmnt
*! Developer : WAM 
*! Date      : 08/01/1996
*! Purpose   : Update Order Approved Amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvApprAmnt()
*!*************************************************************
FUNCTION lfvApprAmnt

SELECT ORDHDR
=RLOCK()
REPLACE ApprAmt WITH lnApproved
UNLOCK

*!*************************************************************
*! Name      : lfvSelect
*! Developer : WAM 
*! Date      : 08/01/1996
*! Purpose   : Select, Unselect, Select all, Select None and Invert accounts
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcType      'S'  : Select/Unselect
*!                         'A'  : Select All  
*!                         'N'  : Select None
*!                         'V'  : Invert
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvSelect('S')
*!*************************************************************
FUNCTION lfvSelect
PARAMETERS lcType

*B602581,1 Commented out
*PRIVATE lcKey,lcDataSel,lcDataUSel
*SELECT (lcCustomer)
*lcKey = Account
*DO CASE 
*  CASE lcType = 'S'
*    REPLACE cSelect WITH IIF(cSelect=SPACE(1),"»",SPACE(1))
*    SKIP
*    IF EOF()
*      SKIP -1
*    ENDIF
*   lcKey = Account
*  CASE lcType = 'A'
*    REPLACE ALL cSelect WITH "»"
*  CASE lcType = 'N'
*    REPLACE ALL cSelect WITH SPACE(1)
*  CASE lcType = 'V'
*    REPLACE ALL cSelect WITH IIF(cSelect=SPACE(1),"»",SPACE(1))
*  OTHERWISE
*ENDCASE
*SET ORDER TO TAG (lcSelect)
*lcDataSel  = IIF(SEEK("»"),'ENABLE','DISABLE')
*lcDataUSel = IIF(SEEK(' '),'ENABLE','DISABLE')
*SET ORDER TO TAG (lcCustomer)
*=SEEK(lcKey)
*SHOW GET pbSelAll    &lcDataUSel
*SHOW GET pbApprove   &lcDataSel
*SHOW GET lnAvailable &lcDataSel
*SHOW GET pbSelNone   &lcDataSel
*=lfwAccBrow()
*SHOW GET ibAccount ENABLE
*SHOW GET pbInvert  ENABLE

*B602581,1 Browse customers from master customer file
PRIVATE lcDataSel,lcDataUSel
DO CASE 
  CASE lcType = 'S'
    IF SEEK(CUSTOMER.ACCOUNT,lcCustomer)
      SELECT (lcCustomer)
      REPLACE cSelect WITH IIF(cSelect="»",'','»')
    ELSE
      INSERT INTO (lcCustomer) (Account,cSelect) VALUES (Customer.Account,IIF(llSelAll,'','»'))
    ENDIF
    SELECT CUSTOMER
    SKIP
    IF EOF() OR TYPE <> 'M'
      =SEEK('M'+lcAccount)
    ENDIF
  CASE lcType = 'A'
    llSelAll  = .T.
     SELECT (lcCustomer)
     REPLACE cSelect WITH "»"
  CASE lcType = 'N'
    llSelAll  = .F.
    SELECT (lcCustomer)
    REPLACE ALL cSelect WITH " "
  CASE lcType = 'V'
    llSelAll  = !llSelAll
    SELECT (lcCustomer)
     DO CASE
       CASE cSelect="»"
         REPLACE ALL cSelect WITH ' '
       CASE cSelect=' '
         REPLACE ALL cSelect WITH IIF(cSelect="»",'','»')
     ENDCASE    
  OTHERWISE
ENDCASE
SELECT (lcCustomer)
LOCATE FOR cSelect="»"
lcDataSel = IIF(llSelAll OR FOUND(),'ENABLE','DISABLE')
LOCATE FOR cSelect=" "
lcDataUSel= IIF(!llSelAll OR FOUND(),'ENABLE','DISABLE')
SELECT CUSTOMER
SHOW GET pbSelAll    &lcDataUSel
SHOW GET pbApprove   &lcDataSel
SHOW GET lnAvailable &lcDataSel
SHOW GET pbSelNone   &lcDataSel
=lfwAccBrow()
*B602581,1 (End)

*!*************************************************************
*! Name      : lfvApprove
*! Developer : WAM 
*! Date      : 08/01/1997
*! Purpose   : Approve by credit line for a group of customers
*!*************************************************************
*! Calls     : gfGetExSin,gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvApprove()
*!*************************************************************
FUNCTION lfvApprove
PRIVATE lcUntSin,lcCurRec,lcStatus
*B602581,1 Commented out
*lcUntSin = ' '
*SELECT (lcCustomer)
*lcCurRec = Account
*SET ORDER TO TAG (lcSelect)
*IF SEEK('»')
*  WAIT 'Approving orders...' WINDOW NOWAIT
*  SCAN REST WHILE cSelect = '»'
*    STORE IIF(lnAvailable=1,Customer.CrLimit-Customer.TotAge,;
*              Customer.CrAvail) TO lnCrAvail
*    SELECT ORDHDR
*    SCAN REST WHILE Account+cOrdType = &lcCustomer..Account+'O'
*      lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)
*      IF lnAvailable=1
*        lnCrAvail = lnCrAvail - ApprAmt &lcExRSin nExRate &lcUntSin nCurrUnit
*      ELSE
*        IF APPRAMT = 0 .AND. ;
*           CEILING(OpenAmt) &lcExRSin nExRate &lcUntSin nCurrUnit <= lnCrAvail
*          REPLACE ApprAmt    WITH CEILING(OpenAmt) ,;
*                  Status     WITH 'O'       ,;
*                  DECL_CODE  WITH SPACE(2)  ,;
*                  MOD_DATE   WITH gdSysDate ,;
*                  MOD_TIME   WITH TIME()    ,;
*                  MOD_USERID WITH gcUser_id ,;
*                  MOD_ACTION WITH 'ORDER APPROVED'
*          lnCrAvail = lnCrAvail - ;
*                      ApprAmt &lcExRSin nExRate &lcUntSin nCurrUnit
*        ENDIF
*      ENDIF
*    ENDSCAN
*    SELECT Customer
*    REPLACE CrAvail WITH lnCrAvail
*    SELECT (lcCustomer)
*    REPLACE CrAvail WITH lnCrAvail
*  ENDSCAN
*ELSE  
*  *E300408,1 Message : 32038
*  *E300408,1 No customers selected
*  *E300408,1 Button : 00000
*  *E300408,1 Ok
*  =gfModalGen('TRM32038B00000','ALERT','customers')
*ENDIF
*SET ORDER TO TAG (lcCustomer)
*=SEEK(lcCurRec)
*WAIT CLEAR

*B603485,1 RAMY (START) release window not to cause conflict with lcfile
RELEASE WINDOW (lcAccTitl)
lnAlias = SELECT(0)
SELECT ORDHDR
lcOldOrd = ORDER()
*B603485,1 Ramy Create a temporary index to include complete date in it
IF !FILE(gcWorkDir + lcTmpCdx + '.CDX')
  INDEX ON ACCOUNT + cORDTYPE + DTOS(COMPLETE)+ ORDER TAG (lcTmpCdx) OF (gcWorkDir +  lcTmpCdx + '.CDX')
ELSE
  SET ORDER TO TAG (lcTmpCdx) OF (gcWorkDir +  lcTmpCdx + '.CDX')
ENDIF
SELECT (lnAlias)
*B603485,1 RAMY (END)
*B602581,1 Use customer master file
lcUntSin = ' '
SELECT (lcCustomer)
LOCATE FOR cSelect="»"
llFound = FOUND()
SELECT CUSTOMER
lcCurRec = Account
IF llSelAll OR llFound
  WAIT 'Approving orders...' WINDOW NOWAIT
  IF llSelAll
    lcFile = 'CUSTOMER'
    =SEEK('M','CUSTOMER')
    lcWhileCod = "Type+Account+Store = 'M'"
    lcForCod   = "IIF(SEEK(ACCOUNT,lcCustomer),&lcCustomer..cSelect='»',.T.)"
  ELSE
    lcFile = lcCustomer
    GO TOP IN (lcCustomer)
    lcWhileCod = ".T."
    lcForCod   = "cSelect='»'"
  ENDIF
  SELECT (lcFile)

  *B603830,1 MHM [Begin]
  IF ALIAS()<> 'CUSTOMER'
    SET RELATION TO 'M'+ Account INTO CUSTOMER
  ENDIF
  *B603830,1 MHM [End]
  SCAN REST WHILE EVAL(lcWhileCod) FOR EVAL(lcForCod)
    *B603830,1 MHM [Begin]
    IF CUSTOMER.STATUS $'HX'
       lcStatus=IIF (CUSTOMER.STATUS = 'H','Hold','Cancelled')
       IF gfModalGen("QRM32084B3200",'DIALOG', ACCOUNT+"|"+lcStatus+"|") = 2
         EXIT
       ELSE     
         LOOP
       ENDIF       
    ELSE
    *B603830,1 MHM [End]

      =IIF(llSelAll,.T.,SEEK('M'+Account,'CUSTOMER'))
    
      *B603201,1 RAMY  Change this line to calclate the net value of  
      *B603201,1 RAMY  NetBal - ChgBack to update it in the CUSTOMER file [start]
 
      *STORE IIF(lnAvailable=1,Customer.CrLimit-Customer.TotAge,;
                Customer.CrAvail) TO lnCrAvail
      STORE IIF(lnAvailable=1 , Customer.CrLimit - customer.NetBal ,;
              Customer.CrAvail) TO lnCrAvail
      *B603201,1 RAMY  [end]
   
      SELECT ORDHDR
      *B603485,1 RAMY
      *=SEEK(CUSTOMER.ACCOUNT+'O')
      =SEEK(&lcFile..ACCOUNT+'O')
      *B603485,1 RAMY end
      *B603485,1 RAMY The index has changed, so remove order from expression
      *SCAN REST WHILE Account+cOrdType+Order = Customer.Account+'O' ;
                 FOR  INLIST(Status,'O','H')
      SCAN REST WHILE Ordhdr.Account+OrdHdr.cOrdType = &lcFile..Account+'O' ;
                 FOR  INLIST(ORDHDR.Status,'O','H')
        WAIT WINDOW 'Approving orders for account : ' + ordhdr.account +" , order# " + ordhdr.order NOWAIT
      *B603485,1 RAMY end
        lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)
        IF lnAvailable=1
          lnCrAvail = lnCrAvail - ApprAmt &lcExRSin nExRate &lcUntSin nCurrUnit
        ELSE
          IF APPRAMT = 0 .AND. ;
            CEILING(OpenAmt) &lcExRSin nExRate &lcUntSin nCurrUnit <= lnCrAvail
            REPLACE ApprAmt    WITH CEILING(OpenAmt) ,;
                    Status     WITH 'O'       ,;
                    DECL_CODE  WITH SPACE(2)
            lnCrAvail = lnCrAvail - ;
                        ApprAmt &lcExRSin nExRate &lcUntSin nCurrUnit
            *C200089,1 Reham On 07/22/1999  *** Begin ***
            *C200089,1 When Order header status changed to "Open", Call process Id to
            *C200089,1 execute special function.
            IF ASCAN(laEvntTrig , PADR('APR_OPN',10)) <> 0
              llReturn = gfDoTriger('SOORAPR',PADR('APR_OPN',10))
            ENDIF
          *C200089,1 Reham On 07/22/1999  *** End   ***
          *B603485,1 RAMY Add the else condition to display a warning message if 
          *B603485,1      the order aproval exceeds the customer cridet limit [START]
          ELSE
            IF ORDHDR.STATUS = 'H' .AND. gfModalGen("QRM32076B3200",'DIALOG', ORDHDR.ACCOUNT+"|"+OrdHdr.ORDER) = 2
              EXIT
            ENDIF
          *B603485,1 RAMY [end]
          ENDIF
        ENDIF
        *B603485,1 RAMY Start, select ORDHDR file
        SELECT ORDHDR
        *B603485,1 RAMY end
      ENDSCAN
      SELECT Customer
      REPLACE CrAvail WITH lnCrAvail
      *B603485,1 RAMY Start
      SELECT (lcFile)
     *B603485,1 RAMY end
    *B603830,1 MHM [Begin]
    ENDIF
    *B603830,1 MHM [End]
  ENDSCAN
ELSE  
  *E300408,1 Message : 32038
  *E300408,1 No customers selected
  *E300408,1 Button : 00000
  *E300408,1 Ok
  =gfModalGen('TRM32038B00000','ALERT','customers')
ENDIF
SELECT Customer
*B603830,1 MHM [Begin]
SELECT (lcFile)
SET RELATION OFF INTO Customer
SELECT Customer
*B603830,1 MHM [End]
=SEEK('M'+lcCurRec)
*ahmed
=lfWScreen()
*ahmed end

SELECT ORDHDR
SET ORDER TO (lcOldOrd)
WAIT CLEAR
*B602581,1 (End)

*!*************************************************************
*! Name      : lfwCrLimit
*! Developer : WAM 
*! Date      : 08/01/1997
*! Purpose   : Validate customer credit limit
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwCrLimit()
*!*************************************************************
FUNCTION lfwCrLimit
lnOldCrLimit = Customer.CrLimit

*!*************************************************************
*! Name      : lfvCrLimit
*! Developer : WAM 
*! Date      : 08/01/1997
*! Purpose   : Validate customer credit limit
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvCrLimit()
*!*************************************************************
FUNCTION lfvCrLimit

SELECT CUSTOMER

*B602581,1 Browse customers from master customer file
*REPLACE CrAvail WITH CrAvail - CrLimit + &lcCustomer..CrLimit ,;
        CrLimit WITH &lcCustomer..CrLimit
*SELECT (lcCustomer)
*REPLACE CrAvail WITH Customer.CrAvail
REPLACE CrAvail WITH CrAvail - lnOldCrLimit + CrLimit 
*B602581,1 (End)


*!*************************************************************
*! Name      : lfOrders
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Inquire Customer Orders
*!*************************************************************
*! Calls     : gfOpenFile,ORDBROWA,gpDoProg
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfOrders()
*!*************************************************************
FUNCTION lfOrders
*B802389,1 Raises Orders Button Flag & Keeps the value of the Selected Order in variable
llOrdFlag=.T.
lcAcct=lcAccount
lcOrdNo=lcOrderNo
*B802389,1
IF !EOF('ORDHDR')
  lcOrder = "'"+ORDHDR.cOrdType+"','"+ORDHDR.ORDER+"'"
  DO gpDoProg WITH 'AWRSOORD',.F.,'SO',lcOrder
ENDIF  

*!*************************************************************
*! Name      : lfvCustomer
*! Developer : WAM 
*! Date      : 08/01/1997
*! Purpose   : Customer Inquire
*!*************************************************************
*! Calls     : gpDoProg
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvCustomer()
*!*************************************************************
FUNCTION lfvCustomer
*B802389,1 Raises Orders Button Flag & Keeps the value of the Selected Account in variable
llOrdFlag =.T.
lcAcct=lcAccount
lcOrdNo=lcOrderNo
*B802389,1
lcCust = "'"+CUSTOMER.ACCOUNT+"',''"
DO gpDoProg WITH 'AWRARCUST',.F.,'AR',lcCust

*!*************************************************************
*! Name      : lpTab
*! Developer : Wael Aly Mohamed
*! Date      : 08/01/1997
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTab WITH 'lcWinCh32', 'm.Store',.T.
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName,llToCheck

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lfClearKey
*! Developer : WAM
*! Date      : 07/01/1996
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

ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+S
ON KEY LABEL ALT+A
ON KEY LABEL ALT+N
ON KEY LABEL ALT+I
ON KEY LABEL ALT+P
ON KEY LABEL ALT+V
*B802099,1 Reham On 04/08/99   *** Begin ***
*B802099,1 Trap the incremental search in the browse.
FOR lnChrToTrap = 32 TO 126
  ON KEY LABEL (CHR(lnChrToTrap))
ENDFOR
*B802099,1 Reham On 04/08/99   *** End   ***

*!*************************************************************
*! Name      : lfIncSearch
*! Developer : Reham Al-Allamy
*! Date      : 04/07/1999
*! Purpose   : Incremental search from the browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfIncSearch()
*!*************************************************************
*B802099,1 Trap the incremental search in the browse.
*!*************************************************************
*
FUNCTION lfIncSearch
PRIVATE lnBrRecNO , lcOrgTag , lcExpToSeek

PUSH KEY CLEAR
ON KEY
ON KEY LABEL F1 llDumi = .T.
CLEAR TYPEAHEAD

*-- Charcter to be searched.
lcExpToSeek = UPPER(CHR(LASTKEY()))

IF INLIST(LASTKEY(),27,13,9,15)
  CLEAR TYPEAHEAD
  POP KEY
  RETURN
ENDIF

*-- Define the window of the incremental search function.
IF _DOS .OR. _UNIX
  DEFINE WINDOW lwIncSrch ;
    FROM 20, 5 ;
      TO 22,74 ;
   FLOAT NOCLOSE SHADOW NOMINIMIZE NONE COLOR SCHEME 5
  ACTIVATE WINDOW lwIncSrch NOSHOW
ELSE
  DEFINE WINDOW lwIncSrch ;
    AT 21.000, 10.000 ;
    SIZE 4.167,55.444 ;
    FONT "FoxFont", 9 ;
    STYLE "B" TITLE "Aria Apparel System" ;
    FLOAT NOCLOSE SHADOW NOMINIMIZE NONE COLOR RGB(,,,192,192,192)
  ACTIVATE WINDOW lwIncSrch NOSHOW
ENDIF

PUSH KEY CLEAR
CLEAR TYPEAHEAD
KEYBOARD "{END}"
*-- Call function to activate the search screen.
DO lfActInSr
POP KEY
*-- Refresh the customer browse.
=lfwAccBrow()

*!*************************************************************
*! Name      : lfActInSr
*! Developer : Reham Al-Allamy
*! Date      : 04/07/1999
*! Purpose   : the second part of the incremental search function
*!           : to get the value of searching.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfActInSr()
*!*************************************************************
*B802099,1 Trap the incremental search in the browse.
*!*************************************************************
*
FUNCTION lfActInSr

*B802391,1 ALB 08/24/1999 Commented out the DOS case because we do not 
*B802391,1                have it in version 2.7
*IF _DOS OR _UNIX
*  @ 0,0 TO 2,69
*  @ 1,6  SAY "Locate         :" SIZE 1,16, 0
*  @ 1,23 GET lcExpToSeek SIZE 1,41 PICTURE '@! XXXXX' MESSAGE "Press 'ENTER' To Locate"
*ELSE
  *B802391,1 ALB 08/24/1999 Fixed Bug of accepting only 5 though should
  *B802391,1                be 6 'XXXXXX' digits
  *@ 1.500,14.333 GET lcExpToSeek ;
    SIZE 1.167,41.000 PICTURE '@! XXXXX' FONT "FoxFont", 9 MESSAGE "Press  'ENTER'  To  Locate"

  @ 1.500,14.333 GET lcExpToSeek ;
    SIZE 1.167,41.000 PICTURE '@! XXXXXX' FONT "FoxFont", 9 MESSAGE "Press  'ENTER'  To  Locate"
  *B802391,1 (End)	
  @ 1.334,14.111 TO 1.334,51.115 PEN 2, 8 STYLE "1" ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 2.667,14.222 TO 2.667,51.115 PEN 2, 8 STYLE "1" ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 1.417,14.111 TO 2.750,14.111 PEN 2, 8 ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 1.417,50.893 TO 2.750,50.893 PEN 2, 8 ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 1.417,4.333 SAY "Locate         :"  FONT "MS Sans Serif", 8 ;
    STYLE "B"
	
  @ 0.000,0.000 TO 0.000,55.444 PEN 1, 8 STYLE "1" ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.333,0.444 TO 0.333,55.000 PEN 1, 8 STYLE "1" ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 4.083,0.000 TO 4.083,55.444 PEN 1, 8 STYLE "1" ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 3.750,0.444 TO 3.750,55.000 PEN 1, 8 STYLE "1" ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.000,0.000 TO 4.167,0.000 PEN 1, 8 ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.333,0.444 TO 3.833,0.444 PEN 1, 8 ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 0.333,54.889 TO 3.833,54.889 PEN 1, 8 ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.000,55.333 TO 4.167,55.333 PEN 1, 8 ;
    COLOR RGB(128,128,128,128,128,128)
*ENDIF

ACTIVATE WINDOW lwIncSrch TOP

READ MODAL
RELEASE WINDOW lwIncSrch

*-- Expression to be searched.
lcExpToSeek = ALLTRIM(lcExpToSeek)
*-- If press enter, search for the typed expression.
IF LASTKEY() = 13
  *B802391,1 ALB 08/24/1999 (Begin) Execute the locate if the user is in 
  *B802391,1                the second folder
  IF lnActFolder = 2
    *-- Save the record pointer of the ORDHDR file.
    lnOrdRecNO = IIF(RECNO("ORDHDR") > RECCOUNT("ORDHDR") , 0 , RECNO("ORDHDR"))
    IF SEEK(ORDHDR.ACCOUNT+'O'+lcExpToSeek  )
      CLEAR TYPEAHEAD
      POP KEY
      RETURN
    ELSE
      *-- restore the record pointer if the typed expression was not found.
      IF lnOrdRecNO > 0 .AND. lnOrdRecNO <= RECCOUNT("ORDHDR")
        GO lnOrdRecNO IN ORDHDR
      ENDIF
    ENDIF
    *B802391,1 ALB 08/24/1999 (End)	    
  *--First Folder 
  ELSE    
    *-- Save the record pointer of the customer screen.
    lnBrRecNO = IIF(RECNO("CUSTOMER") > RECCOUNT("CUSTOMER") , 0 , RECNO("CUSTOMER"))

    *-- Serach for the expression typed.
    IF SEEK("M"+lcExpToSeek , "CUSTOMER")
      CLEAR TYPEAHEAD
      POP KEY
      RETURN
    ELSE
      *-- restore the record pointer if the typed expression was not found.
      IF lnBrRecNO > 0 .AND. lnBrRecNO <= RECCOUNT("CUSTOMER")
        GO lnBrRecNO IN CUSTOMER
      ENDIF
    ENDIF
  ENDIF 
ELSE
  lcExpToSeek = ''
ENDIF

ON KEY
CLEAR TYPEAHEAD
POP KEY
RETURN

*!*************************************************************
*! Name      : lfLocateRec
*! Developer : Gehan George 
*! Date      : 08/22/1999
*! Purpose   : Raises a Flag when pressing pbCustoer or pbCusOrd buttons
*!           : with true .The Order number & Account are kept in two variables 
*!           : to Seek & Select them when returning to screen & browsing any .
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcBrowTyp 'O': Orders
*!                                 'A': Account
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfLocateRec('O')
*!*************************************************************
*B802389,1 Locate the selected record inn browse & make it the current one.
*!*************************************************************
FUNCTION lfLocateRec
PARAMETERS lcBrowTyp
DO CASE 
  CASE lcBrowTyp ='O'
    IF llOrdFlag 
      *--To Seek the correct order
      =SEEK(lcAcct+'O'+lcOrdNo)
      *--To Seek the correct Account
      =SEEK('M'+lcAcct,'CUSTOMER')
      SHOW WINDOW (lcOrdTitl) REFRESH SAME
      llOrdFlag = .F.
    ELSE
      lcOrderNo = OrdHdr.Order
    ENDIF
  CASE lcBrowTyp ='A'
    IF llOrdFlag 
      =SEEK('M'+lcAcct)
      SHOW WINDOW (lcAccTitl) REFRESH SAME
      llOrdFlag = .F.
    ELSE
      lcAccount = Customer.Account
    ENDIF
ENDCASE
