*:************************************************************************
*: Program File        : ARRVKEY.PRG
*: Program Description : Reverse payments/key off.
*: For Screen          : ARRVKEY.SPR
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Accounts Receivable - (AR)
*: Developer Name      : Sherif A. Ishak
*: Tracking Job Number : 
*:************************************************************************
*: Called From         : System Menu.
*:************************************************************************
*: Example             : DO ARRVKEY
*:************************************************************************
*: Modifications
*:B603770,1 ASH 07/24/2000 Optimize the time of opening the screen.
*:B603720,1 ASH 08/27/2000 Fix the bug of not saving the cAdjAcct in the credit file while reversing keyoff.
*:B803658,1 NAD 09/13/2000 Fix bug alias 'XXXXXXXXXX' not found. 
*:B803714,1 KAM 10/08/2000 Prevent Void Invoice Selection. (Add new message 40174)
*:B603960,1 BWA 10/19/2000 1) Fix the bug of alias not found in case the user click the global browse.
*:                         2) Fix the bug of the name of the folders in visible when we come from any screen.[START]
*:B605142,1 SSE 11/20/2001 Fix bug of Reversing credit transactions after checking if credit line exists in Credit file or not.
*:B606627,1 RAE 11/21/2002 The DEBIT file does't be updated in case of returning 
*:                         "Invoice Material Sales Order"
*:B038431,1 TMI 08/23/2004 Fix some problems for the reverse key screen
*:B124082,1 NNA 10/27/2004 Fix bug that when you Reverse key Off the CarpType Filed Return Empty So when
*:B124082,1 NNA            you try to apply a Payment again it Save in the Arhist File with a blank Carptype
*:************************************************************************
*  TRAN CODES             FILE-ID
*  0 = CREDIT MEMOS       CREDIT
*  1 = INVOICES           DEBIT
*  3 = CHARGEBACK IN      DEBIT
*  4 = PAYMENT            CREDIT
*  5 = CREDIT ADJUSTMENT  CREDIT
*  2 = DEBIT ADJUSTMENT   DEBIT
*  6 = CREDIT ON ACCOUNT  CREDIT
*  7 = ALLOWANCE          CREDIT
*  8 = CHR.BCK            ARHIST
*  9 = CREDIT ON ACCT     ARHIST

EXTERNAL ARRAY laKeyField
STORE '' TO lcRvWin1, lcRvWin2, lcRvWin21, lcRvWin3, lcRvWin31, lcRvWin32,;
            lcRvWin4, lcTmpGLDIS, lcTmpArhist, lcTmpSelRec 

DIMENSION laScrMode[4]                 && Screen modes array.
DIMENSION laFoldWinds[2,2]             && Folder screens array.
DIMENSION laKeyField[1,4]


laDefProc[9]  = .F.     && Save procedure(Scr)
laDefProc[10] = .F.     && close procedure(lpClsScr)

STORE 1   TO lnActFolder
lcBrowseTl=''
lnMarker = 0
lnMarkH  = 0
lnMarkD = 0
lcAcc = SPACE(5)
lcAccName = SPACE(30)
lcAccCity = SPACE(30)
lnHistCnt  = 0
lnPayCnt   = 0  
LcFilter1 = "Account = LcAcc AND !EMPTY(Account) AND Trantype ='4'"
llFound = .F.
llBrowse = .F.
LnRec = 0
LcCheck = SPACE(8)

*B603960,1 BWA 10/19/2000 Fix the bug of the name of the folders in visible when we come from any screen.[START]
lcFolder = ''
lcfoldprnt = ''
lnFolderCEnd = 102            && Folder Column end.
lnFolderREnd =   2.00            && Folder Row end.
lnNoFld      =   2               && Number of folders.
lcwfoldchng  = '=lfActFolder()'  && function to control shows after change the folder

lcPayBrTit = 'Open cash payments'
lckeyBrTit = "Key off entries"
lcHstBrTit = "History"
*B603960,1 [END]

IF !gfSetup()
  RETURN
ENDIF

llGLLink   = (UPPER(ALLTRIM(gfGetMemVar("M_LINK_GL"  ))) = "Y")    && GL Link status.
llMltCur   = gfGetMemVar("llMulCurr")                              && Multi currency status.
llRepCBk   = (OCCURS("SR", gcCmpModules)#0) AND ;
             (UPPER(ALLTRIM(gfGetMemVar("M_REPCB")))="Y")          && Charge back sales rep setup.
llApLink  = "AP" $ gcCmpModules

*B603960,1 BWA 10/19/2000 Fix the bug of the name of the folders in visible when we come from any screen.[START]
*lnFolderCEnd = 102            && Folder Column end.
*lnFolderREnd =   2.00            && Folder Row end.
*lnNoFld      =   2               && Number of folders.
*lcwfoldchng  = '=lfActFolder()'  && function to control shows after change the folder
*B603960,1 [END]

lcPayBrTit = 'Open cash payments'
lckeyBrTit = "Key off entries"
lcHstBrTit = "History"

lcCancel   = gcBmpHome + "Can.bmp"
lcFind   = gcBmpHome + "Find.bmp"

lcGlFYear  =SPACE(4)          && Fiscal Year
lcGlPeriod =SPACE(2)          && Period

IF !WEXIST(gcBaseWind)
  lcRvWin1 = gfTempName()   
  lcRvWin2 = gfTempName()     &&Payment window
  lcRvWin21 = gfTempName()
  lcRvWin3 = gfTempName()     &&History window  
  lcRvWin31 = gfTempName()    &&History header window 
  lcRvWin32 = gfTempName()    &&History detail window  
  lcRvWin4 = gfTempName()
  lcTmpGLDIS  = gfTempName()  
  lcTmpArhist = gfTempName()
  lcTmpSelRec = gfTempName()
  lcfolder  = gfTempName() && Folder Window Name
  lcfoldprnt  = gcBaseWind          && window parent name for the folder
  
  STORE 1   TO lnActFolder
  CREATE CURSOR (lcTmpSelRec) (cType C(1),Key C(6))
  INDEX ON CtYPE+KEY TAG KEY
  
  IF !CHECKPRD(gdSysDate,'lcGlFYear','lcGlPeriod','KO')
    RETURN
  ENDIF
  laFoldWinds[1,1] = "Open Cash Payments"
  laFoldWinds[1,2] = lcRvWin2
  laFoldWinds[2,1] = "History"
  laFoldWinds[2,2] = lcRvWin3

  laKeyField[1,1] = 'lcAcc'
  laKeyField[1,2] = .T.
  laKeyField[1,3] = 'CREDIT'
  laKeyField[1,4] = 1
ENDIF

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

PUSH KEY
ON KEY

lcSysMen = SET('SYSMENU')
SET SYSMENU ON
DEFINE PAD _BROWSE1 OF _MSYSMENU PROMPT "" KEY CTRL+B
DEFINE PAD _BROWSE2 OF _MSYSMENU PROMPT "" KEY CTRL+C
ON SELECTION PAD _BROWSE1 OF _MSYSMENU ACTIVATE WINDOW (lcPayBrTit)
ON SELECTION PAD _BROWSE2 OF _MSYSMENU ACTIVATE WINDOW (lcHstBrTit)
=lfTmpHist()

*B603960,1 BWA 10/19/2000 Fix the bug of alias not found in case the user click the global browse.[Start]
laCtrStat[10] = 'DISABLE'
*B603960,1 [END]

DO (gcScrDir+gcWinAppl+"\ARRVKEY.SPX")
RELEASE PAD _BROWSE1 OF _MSYSMENU
RELEASE PAD _BROWSE2 OF _MSYSMENU
*B606627,1 RAE[START] BY MAN
RELEASE WINDOW (lcPayBrTit)
RELEASE WINDOW (lcRvWin21) 
*B606627,1 RAE[END] BY MAN
SET SYSMENU &lcSysMen
POP KEY
IF glQuitting
  USE IN (lcTmpSelRec)
ENDIF
*--End of ArRvKey

*!*************************************************************
*! Name      : lfBrowPay
*! Developer : Sherif Ishak
*! Date      : 10/25/1999
*! Purpose   : To browse the cash payments transactions.
*:*************************************************************
*! Example     : =lfBrowPay()
*!*************************************************************
Function lfBrowPay

SELECT CREDIT
GO TOP

lnMarker = RECNO()
BROWSE FIELDS ; 
       Marker = IIF(LnMarker=RECNO(),'>',' ')  :H=' ':R:1:W=.F. ,;
       Select = IIF(!EOF(lcTmpSelRec),'»',' ') :H=' ':R:1:W=.F. ,;
       Tran     :H='Transaction No' :R ,;
       Trandate :H= 'Date' :R ,;  
       Desc     :H= 'Description' :R ,;
       Amount   :R ,;
       Store    :H='Check #' :R ,;
       Batch    :H='Batch #' :R ;
       NOMENU           ;
       NOAPPEND         ;
       NODELETE         ;
       NOWAIT           ;
       NOCLEAR          ;
       SAVE             ;
       FOR EVAL(LcFilter1);
       WHEN lfwBrow()   ;
       WINDOW (lcRvWin21)   ;
       IN WINDOW (lcRvWin2) ;
       TITLE (lcPayBrTit)

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
ON KEY LABEL ALT+Z
ON KEY LABEL ALT+A
ON KEY LABEL ALT+C

*!*************************************************************
*! Name      : lfReadAct
*! Developer : SHERIF ATTALA ISHAK
*! Date      : 12/01/1996
*! Purpose   : READ Activate function of INV700
*!*************************************************************
*! Calls     : gfClearKey.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

=gfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*!*************************************************************
*! Name      : lfdArRv
*! Developer : SHERIF ATTALA ISHAK
*! Date      : 12/01/1996
*! Purpose   : READ Deactivate function 
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfdArRv()
*!*************************************************************
FUNCTION lfdArRv

IF WONTOP() = lcPayBrTit .OR. WONTOP() = lckeyBrTit .OR. WONTOP() = lcHstBrTit
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ESC  DO lpTab WITH 'gwcContrl1', 'pbCls'
  IF WONTOP() = lcPayBrTit
    ON KEY LABEL TAB     DO lpTab WITH lcRvWin4,'pbSelect'
    ON KEY LABEL BACKTAB DO lpTab WITH lcRvWin4,'PbInvert'
  ENDIF  
  IF WONTOP() = lckeyBrTit
    ON KEY LABEL TAB     DO lpTab WITH lcRvWin4,'pbSelect'
    ON KEY LABEL BACKTAB DO lpTab WITH lcRvWin1,'ibKra200a'
  ENDIF  
  IF WONTOP() = lcHstBrTit
    ON KEY LABEL TAB     DO lpTab WITH lcRvWin1,'ibKra200b'
    ON KEY LABEL BACKTAB DO lpTab WITH lcRvWin1,'PbSearch'
  ENDIF  
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lpShow
*! Developer : Sherif Attala Ishak
*! Date      : 
*! Purpose   : 
*!*************************************************************
*! Calls     : lfwBrow,lfTmpHist,lfBrowHist,lfBrowLine,
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : lpShow()
*!*************************************************************
FUNCTION lpShow

*B603960,1 BWA 10/19/2000 Fix the bug of alias not found in case the user click the global browse.[Start]
SHOW GET pbBrws DISABLE
*B603960,1 [END]

IF laScrMode[1]
  lcAcc = SPACE(5)
  lcAccName = SPACE(30)
  lcAccCity = SPACE(30)
  SELECT (lcTmpSelRec)
  ZAP
  SELECT (lcTmpArhist)  
  DELETE ALL
  =lfwBrow()
  *B038431,1 TMI [Start] Disable the "select" button in case of Select Mode
  SHOW GET PbSelect DISABLE
  *B038431,1 TMI [End  ]   
  _CUROBJ = OBJNUM(lcAcc)
ELSE
  =lfAdjtBut()
ENDIF

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : When function for browse of cash payments or key off. 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfwBrow()
*!*************************************************************
FUNCTION lfwBrow

IF lnActfolder = 1
   lnMarker = RECNO("CREDIT")
   SHOW WINDOW (lcPayBrTit) SAME REFRESH
ELSE
  lnMarkH = RECNO(lcTmpArhist)
  SHOW WINDOW (lckeyBrtit) SAME REFRESH
  lnMarkD = RECNO("ARHIST")
  SHOW WINDOW (lcHstBrTit) SAME REFRESH
ENDIF
IF EOF(lcTmpSelRec)
  IF EOF()
    SHOW GET PbSelect,1 PROMPT '\<Select' DISABLE
  ELSE
    SHOW GET PbSelect,1 PROMPT '\<Select' ENABLE
  ENDIF  
ELSE
  SHOW GET PbSelect,1 PROMPT '\<UnSelect'
ENDIF

*!*************************************************************
*! Name      : lfBrowKey
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : Browse key off sessions.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : lfBrowHist()
*!*************************************************************
FUNCTION lfBrowKey

SELECT (lcTmpArhist)
GO TOP
lnMarkH = RECNO()
BROWSE FIELDS ; 
       Marker = IIF(LnMarkH=RECNO(),'>',' ')  :H=' ':R:1:W=.F. ,;
       Select = IIF(!EOF(lcTmpSelRec),'»',' ') :H=' ':R:1:W=.F. ,;
       History  :R :H='History',;
       Histdate :R :H='Key-off Date' ,;
       Totdb    :R :H='Total Debits' ,;
       Totcr    :R :H='Total Credits' ,;
       Openamt  :R :H='Open Amount' ;    
       NOMENU           ;
       NOAPPEND         ;
       NODELETE         ;
       NOEDIT           ;
       NOWAIT           ;
       NOCLEAR          ;
       SAVE             ;
       WHEN lfwBrow()   ;
       WINDOW (lcRvWin31)   ;
       IN WINDOW (lcRvWin3) ;
       TITLE (lcHstBrTit) 

*!*************************************************************
*! Name      : lfBrowline
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : Browse key off entries for the key off being 
*!             pointed in the browse of key off sessions. 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : lfBrowline()
*!*************************************************************
FUNCTION lfBrowline
SELECT ARHIST
lnMarkD = RECNO()
BROWSE FIELDS ; 
       Marker = IIF(lnMarkD=RECNO(),'>',' ')  :H=' ':R:1:W=.F. ,;
       Tran  :R :H='Transaction No',;
       Store :R :H='Check #' ,;
       Trandate :R :H='Trans. Date' ,;
       Duedate :R :H='Due Date' ,;
       Batch :R :H='Batch No.' ,;
       Amount :R ,;
       Desc :R :H='Description' ;
       NOMENU           ;
       NOAPPEND         ;
       NODELETE         ;
       NOEDIT           ;
       NOWAIT           ;
       NOCLEAR          ;
       SAVE             ;
       KEY !EMPTY(ACCOUNT);
       WHEN lfwBroLine();
       WINDOW (lcRvWin32)   ;
       IN WINDOW (lcRvWin3) ;
       TITLE (lckeyBrTit) 

*!*************************************************************
*! Name      : lfwBroLine
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : When function for the browse of key off entries.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : lfwBroLine()
*!*************************************************************
FUNCTION lfwBroLine
lnMarkD = RECNO("ARHIST")
SHOW WINDOW (lckeyBrtit) SAME REFRESH

*!*************************************************************
*! Name      : lfvAcc
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : The validation function for the Account
*!*************************************************************
*! Calls     : lfTmpHist,lfBrowHist,lfBrowLine,lfTmpHist.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : lfvAcc()
*!*************************************************************
FUNCTION lfvAcc

IF EMPTY(lcAcc) AND !llBrowse 
  RETURN
ENDIF
IF !SEEK('M'+lcAcc,'CUSTOMER') OR llBrowse 
  IF !CUSBROWM(lcAcc,'','M')
    lcAcc =space(5)
    llBrowse = .F.
    _CUROBJ = OBJNUM(lcAcc)
    RETURN
  ELSE
    lcAcc = Account
  ENDIF
ENDIF  
lcAccName  = Customer.BtName
lcAccCity  = Customer.cAddress32
lcAcc = ALLT(lcAcc)
lnPayCnt = 0
lnHistCnt = 0

SELECT CREDIT
=SEEK(lcAcc)
COUNT TO lnPayCnt while Account = lcAcc FOR TranType = "4" 

=lfTmpHist()
=lfBrowKey()
STORE .F. TO laScrMode
STORE .T. TO laScrMode[4]  &&Switch to the New mode
SHOW GETS
llCUpDate = .T.
llBrowse = .F.

*!*************************************************************
*! Name      : lfvActBrow
*! Developer : MALAK - Malak Hanna
*! Date      : 05/02/1995
*! Purpose   : Function to activate browse function.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  Object Name
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  gfvActBrow(m.Scale)
*!*************************************************************
FUNCTION lfvActBrow 
PARAMETERS lcObjName

llBrowse = .T.
_CUROBJ  = OBJNUM(&lcObjName)
KEYBOARD "{ENTER}"


*!*************************************************************
*! Name      : lfTmpHist
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : Function to create temporary file contain key off sessions.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : lfTmpHist()
*!*************************************************************
FUNCTION lfTmpHist

*--Create temporary file for key off 
*B603770,1 ASH 07/24/2000 (Begin) Use the full index to optimize the speed.
*SELECT DISTINCT Account,History , Histdate , Totdb , Totcr ,Openamt ;
FROM ARHIST ;
WHERE ACCOUNT+HISTORY+TRANTYPE+TRAN = LcAcc ;
AND (!EMPTY(Totdb) OR !EMPTY(Totcr));
INTO TABLE (gcWorkDir+lcTmpArhist)

SELECT DISTINCT Account,History , Histdate , Totdb , Totcr ,Openamt ;
FROM ARHIST ;
WHERE ACCOUNT+HISTORY+TRANTYPE+TRAN+CINSTALNO = LcAcc ;
AND (!EMPTY(Totdb) OR !EMPTY(Totcr));
INTO TABLE (gcWorkDir+lcTmpArhist)
*B603770,1 ASH 07/24/2000 (End)
lnHistCnt = _TALLY
INDEX ON Account+History+DTOS(Histdate) TAG (lcTmpArhist)
SET RELATION TO LcAcc + History INTO ARHIST ADDITIVE
GO TOP

*!*************************************************************
*! Name      : lfvSelAll
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : select all key off sessions or payments to reverse.
*!*************************************************************
*! Calls     : lfwBrow.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : lfvSelAll()
*!*************************************************************
FUNCTION lfvSelAll

PRIVATE llFound
llFound = .F.

SELECT (lcTmpSelRec)
IF lnActFolder = 1
  =SEEK('P')
  DELETE REST WHILE cType+Key ='P'
  SELECT CREDIT
  LnRec = RECNO()
  =SEEK(LcAcc)
  SCAN REST WHILE account+tran+DTOS(trandate) = LcAcc FOR TranType = '4'
    IF Credit.TranDate <= gdSysDate
      INSERT INTO (lcTmpSelRec) (cType,Key) VALUES ('P',CREDIT.Tran)
    ELSE
      llFound = .T.
    ENDIF
  ENDSCAN
  IF llFound
   =gfModalGen('INM00000B00000','ALERT','','',;
   'One or more transactions that fall after the reverse date will be ignored.')
  ENDIF
  GO LnRec
  SHOW WINDOW (lcPayBrTit) SAME REFRESH
  IF SEEK('P',lcTmpSelRec) 
    SHOW GET PbSelNone ENABLE
    SHOW GET PbSelAll  DISABLE
  ENDIF
ELSE
  =SEEK('H')
  DELETE REST WHILE cType+Key ='H'
  SELECT (lcTmpArhist)
  LnRec = RECNO()
  SCAN
  *B803714,1 KAM 10/08/2000 To prevent void invoice selection[start]
   IF !lfSelAcc()
    =gfModalGen('INM40174B00000','ALERT',&lcTmpArhist..HISTORY)
    LOOP
   ENDIF
  *B803714,1 KAM [end]

   IF &lcTmpArhist..Histdate <= gdSysDate
     INSERT INTO (lcTmpSelRec) (cType,Key) VALUES ('H',&lcTmpArhist..History)
   ELSE
     llFound = .T.
   ENDIF  
  ENDSCAN
    
  IF llFound
     =gfModalGen('INM00000B00000','ALERT','','',;
     'One or more transactions that fall after the reverse date will be ignored.')
  ENDIF
  GO LnRec
  SHOW WINDOW (lcHstBrTit) SAME REFRESH
  IF SEEK('H',lcTmpSelRec) 
    SHOW GET PbSelNone ENABLE
    SHOW GET PbSelAll  DISABLE
  ENDIF
ENDIF
=lfwBrow()

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : function to select key off or cash payment to reverse.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : lfvSelect()
*!*************************************************************
FUNCTION lfvSelect

IF EOF(lcTmpSelRec)
  IF lnActFolder = 1
    IF Credit.TranDate > gdSysDate
      =gfModalGen('INM00000B00000','ALERT','','',;
      'The transaction date should not be greater than the reverse date. Cannot select.')
      RETURN
    ENDIF
    INSERT INTO (lcTmpSelRec) (cType,Key) VALUES ('P',CREDIT.Tran)
    SELECT (lcTmpSelRec)
    =SEEK("P")
    COUNT TO lnCnt while cType = "P"
    IF lnCnt = lnPayCnt
      SHOW GET PbSelAll DISABLE
    ELSE
      SHOW GET PbSelAll ENABLE
    ENDIF
  ELSE
    IF &lcTmpArhist..Histdate > gdSysDate
      =gfModalGen('INM00000B00000','ALERT','','',;
     'The transaction date should not be greater than the reverse date. Cannot select.')
      RETURN
    ENDIF
    *B803714,1 KAM to prevent void invoice selection[start]
    IF !lfSelAcc()
     =gfModalGen('INM40174B00000','ALERT',&lcTmpArhist..HISTORY)
     RETURN       
    ENDIF
    *B803714,1 KAM[end]
    INSERT INTO (lcTmpSelRec) (cType,Key) VALUES ('H',&lcTmpArhist..History)
    SELECT (lcTmpSelRec)
    =SEEK("H")
    COUNT TO lnCnt while cType = "H"
    IF lnCnt = lnHistCnt
      SHOW GET PbSelAll DISABLE
    ELSE
      SHOW GET PbSelAll ENABLE
    ENDIF
  ENDIF
  =lfwBrow()  
  SHOW GET PbSelNone ENABLE
ELSE
  SELECT (lcTmpSelRec)
  DELETE
  SHOW GET PbSelAll ENABLE
  IF !SEEK(IIF(lnActFolder=1,'P','H'),lcTmpSelRec)
    SHOW GET PbSelNone DISABLE
    SHOW GET PbProceed DISABLE
  ELSE  
    SHOW GET PbSelNone ENABLE
  ENDIF
  =lfwBrow()  
ENDIF

*!*************************************************************
*! Name      : lfvSelNone
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : Cancel the selelction for any of key off or payments.
*!*************************************************************
*! Calls     : lfwBrow.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : lfvSelNone()
*!*************************************************************
FUNCTION lfvSelNone

SHOW GET PbSelAll ENABLE
SHOW GET PbSelNone DISABLE
SELECT (lcTmpSelRec)
ZAP
=lfwBrow()  

*!*************************************************************
*! Name      : lfvInvert
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : Invert the selected key offs or payments.
*!*************************************************************
*! Calls     : lfwBrow.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : lfvInvert()
*!*************************************************************
FUNCTION lfvInvert

PRIVATE llFOUND
llFound = .F.

SELECT (lcTmpSelRec)
GO TOP 
IF EOF()
  SHOW GET PbSelAll DISABLE
ELSE
  SHOW GET PbSelAll ENABLE
ENDIF  
IF lnActFolder = 1
  SELECT CREDIT
  LnRec = RECNO()
  =SEEK(lcAcc)  
  SCAN WHILE Account = LcAcc FOR TranType = '4'
    IF EOF(lcTmpSelRec)
      IF Credit.TranDate <= gdSysDate
        INSERT INTO (lcTmpSelRec) (cType,Key) VALUES ('P',CREDIT.Tran)
      ELSE
        llFound = .T.
      ENDIF  
    ELSE
      SELECT (lcTmpSelRec)  
      DELETE
    ENDIF
  ENDSCAN
  IF llFound
      =gfModalGen('INM00000B00000','ALERT','','',;
      'One or more transactions that fall after the reverse date will be ignored.')
  ENDIF
  GO LnRec
ELSE
  SELECT (lcTmpArhist)
  LnRec = RECNO()
  GO TOP
  SCAN WHILE !EOF()
    IF EOF(lcTmpSelRec)
      IF &lcTmpArhist..HistDate <= gdSysDate
        INSERT INTO (lcTmpSelRec) (cType,Key) VALUES ('H',&lcTmpArhist..History)
      ELSE
        llFound = .T.
      ENDIF  
    ELSE
      SELECT (lcTmpSelRec)  
      DELETE
    ENDIF
  ENDSCAN
  IF llFound
    =gfModalGen('INM00000B00000','ALERT','','',;
     'One or more transactions that fall after the reverse date will be ignored.')
  ENDIF
  GO LnRec
ENDIF
IF !SEEK(IIF(lnActFolder=1,'P','H'),lcTmpSelRec)
  SHOW GET PbSelNone DISABLE
ELSE  
  SHOW GET PbSelNone ENABLE
ENDIF
=lfwBrow()  

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : The validation function for proceed button that 
*!             reverse selected key offs or payments.
*!*************************************************************
*! Calls     : lfAccRev,lfUpdSale,lfUpdateD,lfUpdateC,
*!             lfDelhc,lfwBrow.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : lpSavScr()
*!*************************************************************
FUNCTION lpSavScr

*B038431,1  TMI [Start] Check that selected records are more than 0
*IF EOF(lcTmpSelRec) 
PRIVATE lnRecCnt
lnRecCnt = 0
SELECT (lcTmpSelRec)
GO TOP
COUNT TO lnRecCnt FOR !DELETED()
IF lnRecCnt = 0
*B038431,1  TMI [End  ] 
   =gfModalGen('INM00000B00000','ALERT','','','No transactions have been selected. Cannot proceed.')
  llcSave = .F.
  RETURN
ENDIF
IF llRepCBk
  =gfOpenFile(gcDataDir+'REPCOMM','REPCOMM','SH')
  =gfOpenFile(gcDataDir+'SalesRep','SalesRep','SH')
ENDIF
IF llGlLink
  = gfOpenFile(gcDataDir+"GLDIST", "GLDISTNO", "SH")
  IF !USED(lcTmpGLDIS)
    =AFIELDS(laFileStru)
    CREATE TABLE (gcWorkDir+lcTmpGLDIS) FROM ARRAY laFileStru
  ENDIF
ENDIF
IF llApLink
  =gfOpenFile(gcDataDir+'ApPayMnt','TypMethDoc','SH')
ENDIF  
llUpdate = .F.  && So far no update has been done

IF lnactfolder = 1
  SELECT CREDIT
  SET RELATION OFF INTO (lcTmpSelRec)
ELSE
  SELECT (lcTmpArhist)
  SET RELATION OFF INTO (lcTmpSelRec)
ENDIF

SELECT (lcTmpSelRec)
=SEEK('P')
SCAN REST WHILE cType+Key = 'P'

  *E000000,1 Message : 40169
  *E000000,1 Are you sure you want to reverse payment # 99999
  *E000000,1 Button : 40003
  *E000000,1 Proceed Cancel

  IF SEEK(lcAcc+Key,"CREDIT")  AND ;
     gfModalGen('TRM40169B40003','ALERT','reverse payment# '+CREDIT.Tran+'?') = 1

    WAIT 'Reverse Payment # ' + CREDIT.Tran WINDOW NOWAIT
    IF llGlLink  
      WAIT 'Update general ledger.'  WINDOW NOWAIT
      *-- 1) TOTAL GROSS MERCHANDISE     <DEBIT>   "CASH RECEIPT"
      *--    CATEGORY KEY FOR "Cash Receipts"..........=> '002'
      DO GLDIST WITH Credit.Link_Code,'002',CREDIT.Amount,'CR',;
                     Credit.Tran,gdSysDate,lcGlFYear,lcGlPeriod,lcTmpGLDIS,Credit.cAdjAcct,;
                     Credit.cCurrCode,Credit.nCurrUnit,Credit.nExRate

      *-- 2) MERCHANDISE DISCOUNT        <CREDIT>  "ACCOUNT RECEIVABLE"
      *--    CATEGORY KEY FOR "Accounts Receivable"....=> '001'
      DO GLDIST WITH Credit.Link_Code,'001',-(CREDIT.Amount),'CR',;
                     Credit.Tran,gdSysDate,lcGlFYear,lcGlPeriod,'&lcTmpGLDIS',Credit.cArGlAcc,;
                     Credit.cCurrCode,Credit.nCurrUnit,Credit.nExRate
        
    ENDIF
    IF llApLink
      SELECT ApPayMnt
      =SEEK('A'+IIF(Credit.lNonAr,'N','A')+Credit.Store)
      LOCATE REST WHILE cPayType+cPayMeth+cPayDocNo+cBnkCode+cChkAcct = ;
                        'A'+IIF(Credit.lNonAr,'N','A')+Credit.Store ;
                  FOR   dPayDate = Credit.TranDate .AND. cPayClNo=lcAcc ;
                 .AND.  nPayAmnt = Credit.Amount
      IF FOUND()
        DELETE
      ENDIF
    ENDIF
    SELECT CUSTOMER
    =SEEK("M" + lcAcc)
    =RLOCK()
    REPLACE OPENCR WITH OPENCR - CREDIT.AMOUNT,;
            NETBAL WITH NETBAL - CREDIT.AMOUNT    
    UNLOCK
    SELECT CREDIT
    DELETE
    llupdate =.T.   &&To indicate that there is update has been done
  ENDIF
ENDSCAN
IF llGlLink AND llupdate
  SELECT (lcTmpGLDIS)
  lcGlSess = gfSequence('GLSESSION')
  REPLACE ALL Glsession WITH lcGlSess
  USE
  *--Update GLDIST File
  SELECT GLDIST
  APPEND FROM (gcWorkDir +lcTmpGLDIS)
  =gfOpenFile(gcWorkDir+lcTmpGLDIS,'','EX')
  ZAP
ENDIF

SELECT (lcTmpSelRec)
=SEEK('H')
SCAN REST WHILE cType+Key='H'

  *E000000,1 Message : 40169
  *E000000,1 Are you sure you want to reverse key off session# 99999
  *E000000,1 Button : 40003
  *E000000,1 Proceed Cancel
  IF SEEK(lcAcc+Key,"ARHIST")  AND ;
     gfModalGen('TRM40169B40003','ALERT','reverse key off session# '+ARHIST.History +'?') = 1
    SELECT ARHIST
    WAIT 'Reverse Key off # ' + History WINDOW NOWAIT
    IF !lfAccRev()
      =gfModalGen('INM00000B00000','ALERT','','',;
      'There are C/B and/or Credit on account entries generated in key off # '+ history + ;
      ' have already been keyeed off, Therefore unable to reverse this key off.')
      *B038431,1  TMI [Start] Remove the related line from the temp file "lcTmpSelRec"
      SELECT &lcTmpSelRec
      DELETE
      =lfwBrow()
      *B038431,1  TMI [End  ] 
      LOOP
    ENDIF
    STORE 0 TO lnExchDif
    lcUnitSign = "/"
    =SEEK(lcAcc+&lcTmpSelRec..Key)
    lcLnk = Link_Code
    SCAN REST WHILE ACCOUNT +History +Trantype+Tran = LcAcc + &lcTmpSelRec..Key
      lcAdjAcc  = cAdjAcct
      lcArGlAcc = cArGlAcc

      IF llGlLink AND llMltCur
        =lfUExchDif()
      ENDIF

      IF Trantype ="7" OR (Trantype ="2" AND Deb_adj = "Y")
        IF llGlLink
          *-- Credit Adjustment
         
          IF Trantype ="7"
              
            DO GLDIST WITH ArHist.Link_Code,'009',ArHist.Amount,'CA',ArHist.Tran, ;
                           gdSysDate,lcGlFYear,lcGlPeriod,'&lcTmpGLDIS',lcAdjAcc,;
                           ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate

            DO GLDIST WITH ArHist.Link_Code,'001',-(ArHist.Amount),'CA', ;
                           ArHist.Tran,gdSysDate,lcGlFYear,lcGlPeriod,'&lcTmpGLDIS',;
                           lcArGlAcc,ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate
          ELSE

             DO GLDIST WITH ArHist.Link_Code,'010',ArHist.Amount,'DA',ArHist.Tran, ;
                            gdSysDate,lcGlFYear,lcGlPeriod,'&lcTmpGLDIS',lcAdjAcc,;
                            ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate

             DO GLDIST WITH ArHist.Link_Code,'001',-(ArHist.Amount),'DA', ;
                            ArHist.Tran,gdSysDate,lcGlFYear,lcGlPeriod,'&lcTmpGLDIS',;
                            lcArGlAcc,ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate
          ENDIF
        ENDIF
        IF llApLink
          SELECT ApPayMnt
          =SEEK('AA')
          LOCATE REST WHILE cPayType+cPayMeth+cPayDocNo+cBnkCode+cChkAcct = 'AA' ;
           FOR ((CPAYDOCNO = &lcTmpSelRec..Key) .OR. EMPTY(CPAYDOCNO)) ;
               .AND.  dPayDate = ArHist.TranDate .AND. cPayClNo=lcAcc ;
               .AND.  ABS(nPayAmnt) = ABS(ArHist.Amount)
          IF FOUND()
            DELETE
          ENDIF
        ENDIF
        
        SELECT ARHIST
        IF llRepCBk .AND. Trantype ="7"  
          =lfUpdSale(TranDate,Tran)
        ENDIF
      ELSE
        DO CASE
        
          *B606627,1 RAE Add TranType = "M" in case of Invoice Material Sales Order.[start]
          *CASE INLIST(Trantype,"1","2","3")
          CASE INLIST(Trantype,"1","2","3","M")
          *B606627,1 RAE[end]
        
            =lfUpdateD()
          CASE INLIST(Trantype,"0","4","5","6")
            =lfUpdateC()
          OTHERWISE
            =lfDelhc()
        ENDCASE
        SELECT ARHIST
      ENDIF

      *-- Register a reverse GL transaction..
      *B803658,1 NAD 09/13/2000 (Start) Check for GL link before posting to Gl.
      IF llGlLink
      *B803658,1 NAD  (End)
        IF Trantype  $ "89"
          DO GLDIST WITH ArHist.Link_Code  , "001"   , -(ARHIST.Amount) , "KO",;
                         ArHist.Tran , gdSysDate     , lcGlFYear , lcGlPeriod  ,;
                         lcTmpGLDIS, lcArGlAcc, ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate
        ELSE 
          DO GLDIST WITH ArHist.Link_Code  , "001"  , ARHIST.Amount , "KO",;
                         ArHist.Tran , gdSysDate    , lcGlFYear , lcGlPeriod   ,;
                         lcTmpGLDIS, lcArGlAcc , ARHIST.cCurrCode, ARHIST.nCurrUnit,ARHIST.nExRate
        ENDIF
      *B803658,1 NAD 09/13/2000 (Start) Added the ENDIF.
      ENDIF
      *B803658,1 NAD 09/13/2000 (End)
    ENDSCAN

    =IIF(lnExchDif <> 0, lfUpdDiff(), .T.)
    
    =IIF(llGlLink,lfUpdGL(),.T.)
    SELECT ARHIST
    =SEEK(lcAcc+&lcTmpSelRec..Key)
    DELETE REST WHILE ACCOUNT+History+Trantype+Tran = LcAcc + &lcTmpSelRec..Key    
    llupdate =.T.   &&To indicate that there is update has been done
  ENDIF
ENDSCAN
SELECT (lcTmpArhist)  

*B038431,1  TMI [Start] 
IF llupdate   && Open the screen only if there are some lines will be reversed
*- Save alias orders to restore them after calling ARAGEAR.PRG program
  PRIVATE laEnvrnmt,lnLn
  lnLn = 0
  FOR lnI = 1 TO 225
    SELECT (lnI)
    IF !EMPTY(ALIAS())
      lnLn = lnLn + 1
      DIMENSION laEnvrnmt[lnLn,2]
      laEnvrnmt[lnLn,1] = ALIAS()
      laEnvrnmt[lnLn,2] = ORDER()
    ENDIF
  ENDFOR
  SAVE TO (gcWorkDir+'TMPMEMVR.MEM') ALL LIKE l*
  *B038431,1  TMI [End  ] 

  *-- Age the customer
  SELECT ArHist
  DO (gcAppHome+"ARAGEAR.PRG") WITH lcAcc  

  *B038431,1  TMI [Start] if called from another program , restore environment
  RESTORE FROM (gcWorkDir+'TMPMEMVR.MEM') ADDITIVE
  FOR lnI = 1 TO ALEN(laEnvrnmt,1)
    SET ORDER TO (laEnvrnmt[lnI,2]) IN (laEnvrnmt[lnI,1])
  ENDFOR
  *=IIF(lnActFolder = 1 .AND. !WEXIST(lcPayBrTit) , lfBrowPay()  , .T. )
  *=IIF(lnActFolder = 2 .AND. !WEXIST(lckeyBrtit) , lfBrowline() , .T. )
  *=IIF(lnActFolder = 2 .AND. !WEXIST(lcHstBrTit) , lfBrowKey()  , .T. )
  =lfBrowPay()
  =lfBrowline()
  =lfBrowKey()
ENDIF  
*B038431,1  TMI [End  ] 

SELECT (lcTmpArhist)

IF lnactfolder = 1  
  SELECT CREDIT
  SET RELATION TO 'P'+Tran INTO (lcTmpSelRec) 
ELSE
  SELECT (lcTmpArhist)
  SET RELATION TO 'H'+HISTORY INTO (lcTmpSelRec) ADDITIVE
ENDIF

IF !llupdate 
  llcSave = .F.
  RETURN
ENDIF
WAIT CLEAR

*!*************************************************************
*! Name      : lfUpdSale
*! Developer : Samah Wilson
*! Date      : 12/01/1996
*! Purpose   : Updating SalesRep file
*!*************************************************************
*! Passed Parameters  : ldDate  = Transaction date
*!                      lcTrano = Tran number in ArHist file      
*!*************************************************************
FUNCTION lfUpdSale
PARAMETERS ldDate,lcTrano
PRIVATE lnAlias

lnAlias = SELECT()
=SEEK('M'+lcAcc,"CUSTOMER")
IF SEEK(Customer.Salesrep+DTOS(ldDate)+lcTrano,"RepComm") AND (RepComm.Status ="O")
  IF SEEK(Customer.Salesrep,'SalesRep')
    SELECT SalesRep
    =RLOCK()
    REPLACE Current WITH Current - RepComm.Amount;
            Balance WITH Balance - RepComm.Amount
    UNLOCK
    SELECT REPCOMM
    DELETE
  ENDIF
ENDIF
IF SEEK(Customer.Rep2+DTOS(ldDate)+lcTrano,"RepComm") AND (RepComm.Status ="O")
  IF SEEK(Customer.Rep2,'SalesRep')
    SELECT SalesRep
    =RLOCK()
    REPLACE Current WITH Current - RepComm.Amount;
            Balance WITH Balance - RepComm.Amount
    UNLOCK
    SELECT REPCOMM
    DELETE
  ENDIF
ENDIF  
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfAccRev
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : function to determine if there are charge back or
*!             credit on account already keyeed off found in the 
*!             key off selected to revese.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : lfAccRev()
*!*************************************************************
FUNCTION lfAccRev

SCAN REST WHILE ACCOUNT +History +Trantype+Tran = LcAcc + &lcTmpSelRec..Key 
  IF (Trantype = "8" AND !SEEK(LcAcc +Tran,"DEBIT")) OR;
     (Trantype = "9" AND !SEEK(LcAcc +Tran,"CREDIT")) 
    RETURN .F.
  ENDIF   
ENDSCAN

*!*************************************************************
*! Name      : lfUpdateD
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : Function that udpate debit file.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : lfUpdateD()
*!*************************************************************
FUNCTION lfUpdateD

IF !SEEK(ArHist.Account+ArHist.Tran,'Debit')

  *B124082,1 NNA 10/27/2004 (Begin) insert Also [CarglAcc,cAdjAcct] from the Arhist.Dbf
  *INSERT INTO DEBIT;
   (Account,Store,Trantype,Trancode,Tran,Trandate,Chgbk_date,Desc,Reference,;
    Amount,Batch,Duedate,cFacCode,Dsc_amt,dadd_date,cadd_time,;
    cadd_user,Glflag,Mon_flg,Link_code,cCurrCode,nCurrUnit,nExRate,;
    dPostDate) ;
   VALUES (ARHIST.Account,ARHIST.Store,ARHIST.TranType,ARHIST.Trancode,;
           ARHIST.Tran,ARHIST.Trandate,ARHIST.Chgbk_date,ARHIST.Desc,;
           ARHIST.Reference,ARHIST.Amount,ARHIST.Batch,ARHIST.Duedate,;
           ARHIST.cFacCode,ARHIST.Dsc_amt,gdSysDate,TIME(),gcuser_id,;
           ARHIST.Glflag,ARHIST.Mon_flg,ARHIST.Link_code,ArHist.cCurrCode,;
           ArHist.nCurrUnit,ArHist.nExRate,ArHist.dPostDate)
  INSERT INTO DEBIT;
   (Account,Store,Trantype,Trancode,Tran,Trandate,Chgbk_date,Desc,Reference,;
    Amount,Batch,Duedate,cFacCode,Dsc_amt,dadd_date,cadd_time,;
    cadd_user,Glflag,Mon_flg,Link_code,cCurrCode,nCurrUnit,nExRate,;
    dPostDate,cAdjAcct,CarglAcc) ;
   VALUES (ARHIST.Account,ARHIST.Store,ARHIST.TranType,ARHIST.Trancode,;
           ARHIST.Tran,ARHIST.Trandate,ARHIST.Chgbk_date,ARHIST.Desc,;
           ARHIST.Reference,ARHIST.Amount,ARHIST.Batch,ARHIST.Duedate,;
           ARHIST.cFacCode,ARHIST.Dsc_amt,gdSysDate,TIME(),gcuser_id,;
           ARHIST.Glflag,ARHIST.Mon_flg,ARHIST.Link_code,ArHist.cCurrCode,;
           ArHist.nCurrUnit,ArHist.nExRate,ArHist.dPostDate,ArHist.cAdjAcct,;
           ArHist.CarglAcc)
   *B124082,1 NNA (End)

ELSE
  SELECT DEBIT
  =RLOCK()
  REPLACE Amount WITH Amount + ARHIST.Amount
  UNLOCK
ENDIF

*!*************************************************************
*! Name      : lfUpdateC
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : Function that update credit file.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : lfUpdateC()
*!*************************************************************
FUNCTION lfUpdateC
*:B603720,1 ASH 08/27/2000 (Begin) Fix the bug of not saving the cAdjAcct 
*:B603720,1                in the credit file while reversing keyoff.

*B605142,1 Check if credit file has a record for this transaction. [Begin]
*INSERT INTO CREDIT;
* (Account,Store,Trantype,cCreditCod,Tran,Trandate,Credt_date,Desc,Reference,;
*  Amount,Batch,History,cFacCode,Dsc_amt,dadd_date,cadd_time,;
*  cadd_user,Glflag,Mon_flg,Link_code,cCurrCode,nCurrUnit,nExRate,;
*  dPostDate,cAdjAcct) ;
* VALUES (ARHIST.Account,ARHIST.Store,ARHIST.TranType,ARHIST.Trancode,;
*         ARHIST.Tran,ARHIST.Trandate,ARHIST.Credt_date,ARHIST.Desc,;
*         ARHIST.Reference,ARHIST.Amount,ARHIST.Batch,ARHIST.History,;
*         ARHIST.cFacCode,ARHIST.Dsc_amt,gdSysDate,TIME(),gcuser_id,;
*         ARHIST.Glflag,ARHIST.Mon_flg,ARHIST.Link_code,ArHist.cCurrCode,;
*         ArHist.nCurrUnit,ArHist.nExRate,ArHist.dPostDate,ArHist.cAdjAcct)
PRIVATE lcAlias
lcAlias = ALIAS()
IF !SEEK(ArHist.Account+ArHist.Tran,'Credit')

  *B124082,1 NNA 10/27/2004 (Begin) insert Also [CarglAcc,CBnkCode,CChkAcct,CArpType] 
  *B124082,1 NNA            from the Arhist.Dbf
  *INSERT INTO CREDIT;
   (Account,Store,Trantype,cCreditCod,Tran,Trandate,Credt_date,Desc,Reference,;
    Amount,Batch,History,cFacCode,Dsc_amt,dadd_date,cadd_time,;
    cadd_user,Glflag,Mon_flg,Link_code,cCurrCode,nCurrUnit,nExRate,;
    dPostDate,cAdjAcct) ;
   VALUES (ARHIST.Account,ARHIST.Store,ARHIST.TranType,ARHIST.Trancode,;
           ARHIST.Tran,ARHIST.Trandate,ARHIST.Credt_date,ARHIST.Desc,;
           ARHIST.Reference,ARHIST.Amount,ARHIST.Batch,ARHIST.History,;
           ARHIST.cFacCode,ARHIST.Dsc_amt,gdSysDate,TIME(),gcuser_id,;
           ARHIST.Glflag,ARHIST.Mon_flg,ARHIST.Link_code,ArHist.cCurrCode,;
           ArHist.nCurrUnit,ArHist.nExRate,ArHist.dPostDate,ArHist.cAdjAcct)
  INSERT INTO CREDIT;
   (Account,Store,Trantype,cCreditCod,Tran,Trandate,Credt_date,Desc,Reference,;
    Amount,Batch,History,cFacCode,Dsc_amt,dadd_date,cadd_time,;
    cadd_user,Glflag,Mon_flg,Link_code,cCurrCode,nCurrUnit,nExRate,;
    dPostDate,cAdjAcct,CarglAcc,CBnkCode,CChkAcct,Carptype) ;
   VALUES (ARHIST.Account,ARHIST.Store,ARHIST.TranType,ARHIST.Trancode,;
           ARHIST.Tran,ARHIST.Trandate,ARHIST.Credt_date,ARHIST.Desc,;
           ARHIST.Reference,ARHIST.Amount,ARHIST.Batch,ARHIST.History,;
           ARHIST.cFacCode,ARHIST.Dsc_amt,gdSysDate,TIME(),gcuser_id,;
           ARHIST.Glflag,ARHIST.Mon_flg,ARHIST.Link_code,ArHist.cCurrCode,;
           ArHist.nCurrUnit,ArHist.nExRate,ArHist.dPostDate,ArHist.cAdjAcct,;
           ArHist.CarglAcc,ArHist.CBnkCode,ArHist.CChkAcct,ArHist.Carptype)
   *B124082,1 NNA (End)

ELSE
  SELECT Credit
  =RLOCK()
  REPLACE Amount WITH Amount + ARHIST.Amount
  UNLOCK
ENDIF
SELECT (lcAlias)
*B605142,1 Check if credit file has a record for this transaction. [End]

*:B603720,1 ASH 08/27/2000 (End)         

*!*************************************************************
*! Name      : lfDelhc
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : Function that delete credit on account or charge back.
*!             when reversing the key off.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
FUNCTION lfDelhc

IF Trantype = "8"
  IF SEEK(LcAcc+Tran,"DEBIT")
    SELECT DEBIT
    DELETE
  ENDIF
ELSE
  IF SEEK(LcAcc+Tran,"CREDIT")
    SELECT CREDIT
    DELETE
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lpTab
*! Developer : SHERIF ATTALA ISHAK
*! Date      : 12/01/1996
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTab WITH 'INV700', 'pbGlobal'
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)



IF lcWindNAme = 'gwcContrl1'  AND _CUROBJ = 3
  KEYBOARD "{ENTER}"
ENDIF

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Sherif Attala Ishak
*! Date      : 11/13/1996
*! Purpose   : The validation function for the radio buttons.
*!*************************************************************
*! Calls     : lfwBrow,lfTmpHist,lfBrowHist,lfBrowLine,
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : lfActFolder()
*!*************************************************************
FUNCTION lfActFolder

IF lnactfolder = 1
  SELECT (lcTmpArhist)
  SET RELATION OFF INTO (lcTmpSelRec)
  SELECT CREDIT
  SET RELATION TO 'P'+Tran INTO (lcTmpSelRec) 

  =lfwBrow()  
  IF !EMPTY(lcAcc)
    KEYBOARD "{CTRL+B}" 
  ENDIF
ELSE
  SELECT CREDIT
  SET RELATION OFF INTO (lcTmpSelRec)
  SELECT (lcTmpArhist)
  SET RELATION TO 'H'+HISTORY INTO (lcTmpSelRec) ADDITIVE
  =lfwBrow()  
  IF !EMPTY(lcAcc)
    KEYBOARD "{CTRL+C}" 
  ENDIF
ENDIF  
SHOW GET IBKRA200A ENABLE
SHOW GET IBKRA200B ENABLE
=IIF(laScrMode[1],.T.,lfAdjtBut())


FUNCTION lpClsScr

*!*************************************************************
*! Name      : lfvFind
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : Function to search for check # of payments
*:*************************************************************
*! Example     : =lfvFind()
*!*************************************************************
 FUNCTION lfvFind

llFind =.F.
SELECT (lcTmpArhist)
lcCurrKey = lcAcc+History
SET RELATION OFF INTO ARHIST
SELECT ARHIST
=SEEK(lcAcc)
SCAN REST WHILE Account = lcAcc FOR Trantype ="4"
  IF STORE = lcCheck
    lcCurrKey = lcAcc+ History
    llFind = .T.
    EXIT
  ENDIF
ENDSCAN
SET ORDER TO TAG ARHISTHT IN ARHIST
SELECT (lcTmpArhist)
SET RELATION TO lcAcc + History INTO ARHIST ADDITIVE
=SEEK(lcCurrKey)
IF !llFind
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
  "No Key off session has payments with check # " + ALLTRIM(LcCheck))
  _CUROBJ = OBJNUM(LcCheck)
ELSE
  CLEAR READ
ENDIF
SHOW WINDOW (lcHstBrTit) SAME REFRESH
=lfwBrow()  

*!*************************************************************
*! Name      : lfvCancel
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : The validation function for cancel button.
*:*************************************************************
*! Example     : =lfvCancel()
*!*************************************************************
FUNCTION lfvCancel
CLEA READ


*!*************************************************************
*! Name      : lfvSearch
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : The validation function for search button.
*:*************************************************************
*! Example     : =lfvSearch()
*!*************************************************************
FUNCTION lfvSearch

lcCheck = SPACE(8)
PUSH KEY
ON KEY
DO (gcScrDir+gcWinAppl+"\ARRVKEY1.SPX")
POP KEY
KEYBOARD "{CTRL+C}"    


*!*************************************************************
*! Name      : lfvCheck
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : The validation function for Check #.
*:*************************************************************
*! Example     : =lfvCheck()
*!*************************************************************
FUNCTION lfvCheck

IF EMPTY(LcCheck)
  SHOW GET PbFind DISABLE
  RETURN
ENDIF
SHOW GET PbFind ENABLE


*!*************************************************************
*! Name      : lfUExchDif
*! Developer : Sherif Ishak
*! Date      : 05/15/2000
*! Purpose   : Calculate the diff. in the exchange rate.
*:*************************************************************
*! Example     : =lfUExchDif()
*!*************************************************************
FUNCTION lfUExchDif
lcRateSign = gfGetExSin(@lcUnitSign,cCurrCode)  
lnNewVal   = (Amount * IIF(Trantype  $ "89", 1, -1) &lcRateSign nExRate &lcUnitSign nCurrUnit)
lnExchDif  = lnExchDif + lnNewVal


*!*************************************************************
*! Name      : lfUpdDiff
*! Developer : Sherif Ishak
*! Date      : 05/15/2000
*! Purpose   : Create GL entries due to difference in ex. rate
*:*************************************************************
*! Example     : =lfUpdDiff()
*!*************************************************************
FUNCTION lfUpdDiff
PRIVATE lnAlias

lnAlias  = SELECT(0)
lcUseAcc = SPACE(0)

= gfOpenFile(gcDataDir+"GLSETUP", "", "SH")
= gfOpenFile(gcDataDir+"GLACCHAR","","SH")
lcUseAcc = ALLTRIM(GLSETUP.cSetExMj)
= gfOpenFile(gcDataDir+"ACCOD", "Accsegno", "SH")
  
GO TOP
lnAcsSegSz = nAcsSegSz 
lnAcsNoSeg = nAcsNoSeg
lcAcsMask  = "X" + SUBSTR(cAcsMask,2)
lcAcsMask  = ALLTRIM(STRTRAN(lcAcsMask,'#','9'))
*-- Prepare account code according to the account code 
*-- mask (lcAcsMask) as well as the field content
lcUseAcc = lcUseAcc +;
STRTRAN(SUBSTR(ALLTRIM(lcAcsMask),LEN(lcUseAcc)+1),'9','0') +;
REPLICATE(' ',FSIZE('cAcctCode','GLACCHAR')-lnAcsSegSz)
*B803658,1 NAD 09/13/2000 (Start) Check for GL link before posting to Gl.
IF llGlLink
*B803658,1 NAD (End)
  DO GLDIST WITH lcLnk, "009", lnExchDif, "EX", "" ,;
                 gdSysDate, lcGlFYear, lcGlPeriod, lcTmpGLDIS,lcUseAcc
*B803658,1 NAD 09/13/2000 (Start) Added the ENDIF.
ENDIF 
*B803658,1 NAD (End)                  

SELECT (lnAlias)
*!*************************************************************
*! Name      : lfUpdGL
*! Developer : Sherif Ishak
*! Date      : 05/15/2000
*! Purpose   : Update GL distribution file.
*:*************************************************************
*! Example     : =lfUpdGL()
*!*************************************************************
FUNCTION lfUpdGL
PRIVATE lnAlias

lnAlias  = SELECT(0)

USE (gcWorkDir+lcTmpGLDIS) IN 0 SHARED AGAIN ALIAS UseToSum
DIMENSION laAccounts[1,2]
laAccounts = SPACE(0)
SELECT (lcTmpGLDIS)
lcGlSess = gfSequence('GLSESSION')
SCAN
  lcGLAcc = GLAccount + Tran_Type
  IF Tran_Type # 'KO'
    lnLen = ALEN(laAccounts, 1)
    lnLen = lnLen + IIF(lnLen = 1 AND EMPTY(laAccounts[1,1]), 0, 1)
    DIMENSION laAccounts[lnLen,2]
    laAccounts[lnLen, 1] = lcGLAcc 
    laAccounts[lnLen, 2] = .T.
  ELSE
    IF ASCAN(laAccounts, lcGLAcc) = 0
      SELECT UseToSum
      SUM ALL nEqvAmnt FOR GLAccount+Tran_Type = lcGLAcc  TO lnGLAmount

      lnLen = ALEN(laAccounts, 1)
      lnLen = lnLen + IIF(lnLen = 1 AND EMPTY(laAccounts[1,1]), 0, 1)
      DIMENSION laAccounts[lnLen,2]
      laAccounts[lnLen, 1] = lcGLAcc
      laAccounts[lnLen, 2] = lnGLAmount # 0
    ENDIF
  ENDIF
  SELECT (lcTmpGLDIS)
  REPLACE GLSESSION WITH lcGlSess
ENDSCAN

USE IN UseToSum

SELECT (lcTmpGLDIS)
SCAN FOR laAccounts[ASUBSCRIPT(laAccounts,ASCAN(laAccounts,GLAccount+Tran_Type),1),2]
  SCATTER MEMVAR MEMO
  INSERT INTO GLDIST FROM MEMVAR
ENDSCAN
SELECT (lcTmpGLDIS)
ZAP
SELECT (lnAlias)


*!*************************************************************
*! Name      : lfAdjtBut
*! Developer : Sherif Ishak
*! Date      : 05/15/2000
*! Purpose   : 
*:*************************************************************
*! Example     : =lfAdjtBut()
*!*************************************************************
FUNCTION lfAdjtBut

IF lnActFolder = 1
  SELECT CREDIT
  lnRecNo = RECNO()
  =SEEK(lcAcc)  
  LOCATE REST WHILE Account = LcAcc FOR TranType = '4'
  llRecords = FOUND()
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF  
  SHOW GET PbSearch DISABLE	
ELSE
  lnRecNo = RECNO(lcTmpArhist)
  GO TOP IN (lcTmpArhist)
  llRecords = !EOF(lcTmpArhist)
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO lnRecNo IN (lcTmpArhist)
  ENDIF  
  IF llRecords
    SHOW GET PbSearch ENABLE	
  ENDIF 
ENDIF
llSelect = SEEK(IIF(lnActFolder=1,'P','H'),lcTmpSelRec)
IF llRecords AND laScrMode[4]
  SHOW GET pbSelect ENABLE 
  SHOW GET PbInvert ENABLE
  SHOW GET PbSelAll ENABLE
ELSE
  SHOW GET pbSelect DISABLE
  SHOW GET PbInvert DISABLE
  SHOW GET PbSelAll  DISABLE
ENDIF
IF llSelect
  SHOW GET PbSelNone ENABLE 
ELSE
  SHOW GET PbSelNone DISABLE
ENDIF


*!*************************************************************
*! Name      : lfSelAcc            *:B803714,1 KAM 
*! Developer : Abd Elrahim Koura
*! Date      : 10/08/2000
*! Purpose   : Function to prevent void invoice selection.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : .F. if void invoice otherwise return .T. 
*!*************************************************************
*! Example            : lfSelAcc()
*!**********************************************************
FUNCTION lfSelAcc
PRIVATE lnArea,lnRecNo,llRetVal
lnArea=SELECT()
lnRecNo=RECNO(lcTmpArhist)
llRetVal = .T.
IF !USED("INVHDR")
  =gfOpenFile(gcDataDir+"INVHDR", "INVHDR", "SH")
ENDIF
SELECT( "ARHIST" )
IF SEEK(&lcTmpArhist..ACCOUNT+&lcTmpArhist..HISTORY)
  SCAN WHILE ACCOUNT+HISTORY = &lcTmpArhist..ACCOUNT+&lcTmpArhist..HISTORY  FOR  TRANTYPE='1'
    IF SEEK(TRAN,"INVHDR")
      IF INVHDR.STATUS="V"
        llRetVal = .F.
        EXIT
      ENDIF   
    ENDIF  
  ENDSCAN
ENDIF  
SELECT(lnArea)
GOTO lnRecNo
RETURN llRetVal
*:B803714,1 KAM [end] 
*:***************************************************************************

