*:************************************************************************
*: Program file  : AROLS11.PRG (Converted from 26 to 27)
*: Program desc. : Program to give the user the abilty to interchange
*:                 between factor and non-factor items.
*: For screen    : OLS1100.Screen
*: Customer      : OLSEN
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:               : Refer to (C101585)
*:************************************************************************

*--- Initilize needed variables.
*-- Program Variables Decleration.
llBrowse   = .F.          && Flag to browse if you click by mouse on ib.
lcTmpFile  = gfTempName() && Variable to hold the program main lines file.
gcHost     = 'OLS1100'    && The host screen.
lnMainRec  = 0            && Main browse pointer record#.
lcScrMode  = 'S'          && Screen mode is select.
lcAccount  = SPACE(5)     && Account Code.
lcFactor   = SPACE(5)     && Factor  Code.
lcDesc     = ''           && Account Name.
llUpdated  = .F.          && No modifications has been done.
lcBrowHdr  = 'Open Credit and Debit Transactions'  && Browse title.
lcBrowBmp  = (gcBmpHome + "ExtKey.BMP")       && External key icon location.
*-- Open files required for processing.
IF !gfSetup()
  RETURN
ENDIF  
=gfOpenFile(gcDataDir+'Customer', gcDataDir+'Customer', 'SH')
=gfOpenFile(gcDataDir+'Debit'   , gcDataDir+'Debit'   , 'SH')
=gfOpenFile(gcDataDir+'Credit'  , gcDataDir+'Credit'  , 'SH')
=gfOpenFile(gcDataDir+'InvHdr'  , gcDataDir+'InvHdr'  , 'SH')
=gfOpenFile(gcDataDir+'ConsInvH', gcDataDir+'ConsInvH', 'SH')
=gfOpenFile(gcDataDir+'OrdHdr'  , gcDataDir+'OrdHdr'  , 'SH')
=gfOpenFile(gcDataDir+'RetHdr'  , gcDataDir+'RetHdr'  , 'SH')
*-- C101585 Open SyscFact to assure the existing of the factor
=gfOpenFile(gcSysHome+'SycFact'  , gcSysHome+'cFacCode'  , 'SH')
*---- Create the temp file holding styles in criteria to select styles from to delete.
CREATE TABLE (gcWorkDir+lcTmpFile) ;
	   (cStore   C(08)  ,;
	    cTran    C(06)  ,;
	    cDesc    C(15)  ,;
	    dDate    D      ,;
	    nAmount  N(10,2),;
	    cFactor  C(06)  ,;
	    lUpdated L      )

*-- Because we are using a MDI window type under WINDOWS which has some harmful
*-- effect on the system menu we have to PUSH the menu and POP it when going out
PUSH KEY 
*-- Call the main screen from the right path.
DO (gcScrDir+gcWinAppl+"\AROLS11.SPX")
POP KEY


*:************************************************************************
*: Program file  : lfShow
*: Program desc. : The programm main show function.
*:         Module: Aria Apparel Series.
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:************************************************************************
FUNCTION lfShow

DO CASE
  CASE lcScrMode = 'S'
    lcAccount = SPACE(05)
    lcFactor  = SPACE(05)
    lcDesc    = ''
    llUpdated = .F.
    SELECT (lcTmpFile)
    ZAP
    *-- Show window (lcBrowHdr)
    RELEASE WINDOW (lcBrowHdr)
    =lfBrowse()
    SHOW GETS                                       DISABLE
    SHOW GET pbFactor,1 PROMPT 'Change To \<Factor' DISABLE
    SHOW GET pbClose,1  PROMPT '\!\?\<Close'        ENABLE
    SHOW GET ibKey                                  ENABLE
    SHOW GET lcAccount                              ENABLE
     _CUROBJ = OBJNUM(lcAccount)
  CASE lcScrMode = 'M'
    WAIT WINDOW 'Collecting Data. Please Wait' NOWAIT
    SHOW GETS                                 DISABLE
    IF SEEK(lcAccount,'Debit')
      SELECT Debit
      SCAN REST WHILE ACCOUNT+TRAN+DTOS(TRANDATE) = lcAccount
        INSERT INTO (lcTmpFile);
               (cStore, cTran, cDesc, dDate   , nAmount, cFactor, lUpdated);
        VALUES (Debit.Store,Debit.Tran,Debit.Desc,Debit.TranDate,Debit.Amount,Debit.cFacCode, .f.)
      ENDSCAN
    ENDIF
    IF SEEK(lcAccount,'Credit')
      SELECT Credit
      SCAN REST WHILE ACCOUNT+TRAN+DTOS(TRANDATE) = lcAccount
        INSERT INTO (lcTmpFile);
               (cStore, cTran, cDesc, dDate   , nAmount, cFactor, lUpdated);
        VALUES (Credit.Store,Credit.Tran,Credit.Desc,Credit.TranDate,Credit.Amount,Credit.cFacCode, .f.)
      ENDSCAN
    ENDIF
     SELECT (lcTmpFile)
    IF RECCOUNT() <> 0
      IF EMPTY(cFactor)
        IF !EMPTY(lcFactor)
          SHOW GET pbFactor,1 PROMPT 'Change To \<Factor'     ENABLE
        ELSE
          SHOW GET pbFactor,1 PROMPT 'Change To \<Factor'     DISABLE
        ENDIF
      ELSE
        SHOW GET pbFactor,1 PROMPT 'Change To Non \<Factor'   ENABLE
      ENDIF
      SHOW GET pbClose,1  PROMPT '\!\?\<Cancel' ENABLE
      SHOW GET ibBkTAB                          ENABLE
      SHOW GET ibTAB                            ENABLE
    ELSE
      SHOW GET pbClose,1  PROMPT '\!\?\<Close' ENABLE
    ENDIF
    *-- Show window (lcBrowHdr)
    RELEASE WINDOW (lcBrowHdr)
    =lfBrowse()
    WAIT CLEAR
ENDCASE

*:************************************************************************
*: Program file  : lfvAccount
*: Program desc. : The validation function to the Account field.
*:         Module: Aria Apparel Series.
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:************************************************************************
FUNCTION lfvAccount

IF (!EMPTY(lcAccount) .AND. LASTKEY() <> 27 ) .OR. llBrowse
  IF llBrowse .OR. !SEEK('M'+lcAccount,'Customer')
    SELECT Customer
    DO CusBrowM WITH lcAccount
  ENDIF

  IF !EMPTY(lcAccount)
    lcDesc = Customer.btName
    SHOW GET ibKey      DISABLE
    SHOW GET lcAccount  DISABLE
    SHOW GET lcDesc     DISABLE
    *-- C101585 Seek the factor in sycFact not in customer file.
    IF SEEK(Customer.cFacCode,'SycFact')
      lcFactor = SycFact.cFacCode
    ELSE
      lcFactor = ''
    ENDIF
    lcScrMode = 'M'
    =lfShow()
  ENDIF
ENDIF  
llBrowse = .F.

*:************************************************************************
*: Program file  : lfBrowse
*: Program desc. : The function to browse Debit and credit lines for the
*:                 selected account.
*:         Module: Aria Apparel Series.
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:************************************************************************
FUNCTION lfBrowse

SELECT (lcTmpFile)
GO TOP
lnMainRec = RECNO()

*lcBrowFlds = "lcPoint=IIF(RECNO() = lnMainRec,'>',' '):H=' ',"+;
               "cTran   :H= 'Tran. ':6,"+;
               "cDesc   :H= 'Description':13,"+;
               "cStore  :H='Store':8,"+;
               "lcFactor=IIF(EMPTY(cFactor),'Non Factored',cFactor):H='Factor':12,"+;
               "dDate   :H= 'Date',"+;
               "nAmount :H= 'Amount'"
lcBrowFlds = "lcPoint=IIF(RECNO() = lnMainRec,'>',' '):H=' ',"+;
               "cTran   :H= 'Tran. ':10,"+;
               "cDesc   :H= 'Description':30,"+;
               "cStore  :H='Store':10,"+;
               "lcFactor=IIF(EMPTY(cFactor),'Non Factored',cFactor):H='Factor':18,"+;
               "dDate   :H= 'Date',"+;
               "nAmount :H= 'Amount'"               
*BROWSE FIELDS &lcBrowFlds;
       NOAPPEND NODELETE NOWAIT NOCLEAR  SAVE ;
       TITLE lcBrowHdr WHEN lfBrowWhen();
       WINDOW OLS11001 IN WINDOW (gcHost)
       
       
BROWSE FIELDS &lcBrowFlds;
       NOAPPEND NODELETE NOWAIT NOCLEAR NOEDIT SAVE ;
       TITLE lcBrowHdr WHEN lfBrowWhen();
       WINDOW OLS11001 IN WINDOW (gcHost)


*:************************************************************************
*: Program file  : lfBrowWhen
*: Program desc. : The When function for the main browse.
*:         Module: Aria Apparel Series.
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:************************************************************************
FUNCTION lfBrowWhen

SELECT (lcTmpFile)
lnMainRec = RECNO()
SHOW WINDOW (lcBrowHdr) REFRESH
*-- Refresh the push bottones.
IF EMPTY(cFactor)
  IF !EMPTY(lcFactor)
    SHOW GET pbFactor,1 PROMPT 'Change To \<Factor'     ENABLE
  ELSE
    SHOW GET pbFactor,1 PROMPT 'Change To \<Factor'     DISABLE
  ENDIF
ELSE
  SHOW GET pbFactor,1 PROMPT 'Change To Non \<Factor'   ENABLE
ENDIF

*:************************************************************************
*: Program file  : lfvFactor
*: Program desc. : The validation function of the Factor pb.
*:         Module: Aria Apparel Series.
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:************************************************************************
FUNCTION lfvFactor

SELECT (lcTmpFile)
IF EMPTY(cFactor)
  IF !EMPTY(lcFactor)
    REPLACE cFactor  WITH lcFactor,;
            lUpdated WITH .T.
    llUpdated = .T.
  ENDIF
ELSE
  REPLACE cFactor  WITH '',;
          lUpdated WITH .T.
  llUpdated = .T.
ENDIF

IF llUpdated = .T.
  SHOW GET pbSave ENABLE
ENDIF
=lfBrowWhen()

*:************************************************************************
*: Program file  : lfvSave
*: Program desc. : The validation function of the Save pb.
*:         Module: Aria Apparel Series.
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:************************************************************************
FUNCTION lfvSave

SELECT (lcTmpFile)
SCAN FOR lUpdated
  IF SEEK(lcAccount+cTran,'Debit')
    REPLACE Debit.cFacCode WITH cFactor
  ENDIF
  
  IF SEEK(lcAccount+cTran,'Credit')
    REPLACE Credit.cFacCode WITH cFactor
  ENDIF
  
  IF SEEK(cTran,'InvHdr')
    REPLACE InvHdr.cFacCode WITH cFactor
    IF SEEK(InvHdr.Order,'OrdHdr')
      REPLACE OrdHdr.cFacCode WITH cFactor
    ENDIF
  ENDIF
  
  IF SEEK(cTran,'ConsInvH')
    REPLACE ConsInvH.cFacCode WITH cFactor
    IF SEEK(ConsInvH.Order,'OrdHdr')
      REPLACE OrdHdr.cFacCode WITH cFactor
    ENDIF
  ENDIF
  
  IF SEEK(cTran,'RetHdr')
    REPLACE RetHdr.cFacCode WITH cFactor
  ENDIF
ENDSCAN
lcScrMode = 'S'
=lfShow()

*:************************************************************************
*: Program file  : lfvClose
*: Program desc. : The validation function of the close pb.
*:         Module: Aria Apparel Series.
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:************************************************************************
FUNCTION lfvClose

IF EOF(lcTmpFile)
  CLEAR READ
ELSE
  IF llUpdated .AND. gfDialog('!','Are you sure? You will lose all changes.','\!\<Yes;\?\<No') = 2
    RETURN
  ENDIF
  lcScrMode = 'S'
  =lfShow()
ENDIF

*:************************************************************************
*: Program file  : lfReadAct
*: Program desc. : When activating the read.
*:         Module: Aria Apparel Series.
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:************************************************************************
FUNCTION lfReadAct
=gfClearKey()
ON KEY LABEL ALT+O SHOW WINDOW (lcBrowHdr)

*:************************************************************************
*: Program file  : lfReadDeact
*: Program desc. : When deactivating the read.
*:         Module: Aria Apparel Series.
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:************************************************************************
FUNCTION lfReadDeact
ON KEY
IF UPPER(WONTOP()) = UPPER(lcBrowHdr)
  ON KEY LABEL TAB        DO lpTab     WITH 'OLS11002', 'pbFactor'
  ON KEY LABEL BACKTAB    DO lpTab     WITH 'OLS11002', 'pbClose'
  ON KEY LABEL CTRL+ENTER DO lpTrapKey WITH 'OLS11002', 'pbClose'
  ON KEY LABEL ESC        DO lpTrapKey WITH 'OLS11002', 'pbClose'
  ON KEY LABEL ALT+C      DO lpTrapKey WITH 'OLS11002', 'pbClose'
  ON KEY LABEL ALT+S      DO lpTrapKey WITH 'OLS11002', 'pbSave'
  ON KEY LABEL ALT+F      DO lfvFactor
  ON KEY LABEL ENTER      DO lfvFactor
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL CTRL+HOME  GO TOP
  ON KEY LABEL CTRL+END   GO BOTTOM
ENDIF

RETURN .F.

*:************************************************************************
*: Program file  : lpTab
*: Program desc. : Trap the tab key.
*:         Module: Aria Apparel Series.
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:************************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName
ACTIVATE WINDOW (lcWindNAme) TOP
_CUROBJ = OBJNUM(&lcObjName)

*:************************************************************************
*: Program file  : lpTrapKey
*: Program desc. : Activate a window and current object = the passed object.
*:         Module: Aria Apparel Series.
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:************************************************************************
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName
ON KEY
ACTIVATE WINDOW (lcWindName) TOP
_CUROBJ = OBJNUM(&lcObjName)

KEYBOARD CHR(13) CLEAR



*=========================================================================

