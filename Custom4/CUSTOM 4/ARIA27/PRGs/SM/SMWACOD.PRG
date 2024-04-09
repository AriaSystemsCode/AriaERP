*:************************************************************************
*:
*: Procedure file: SMWACCOD.PRG 
*:                 (Account code structure)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Ahmed Salah SHalaby - (SSH)
*:      Copyright (c) 
*:************************************************************************
*: Modifications:
*:B#804025,1 SSH 25/03/2001 Fix some bugs in screen layout.
*:************************************************************************
PRIVATE laAllChart

DECLARE laAllChart[1,2]
laAllChart = SPACE(1)
lcAccName  = ""
lnCharNo   = 1
lcWindTitl = "Chart of accounts"
lcMtBrowTt = "Chart of accounts"
IF !USED("SYWACHRT")
   = gfOpenFile(gcSysHome+"SYWACHRT","Cidentify","SH")
ENDIF
IF !USED("sywaccod")
  = gfOpenFile(gcSysHome+"sywaccod","","SH")
ENDIF
lcTmpQuery = gfTempName()
SELECT SYWACHRT
SELECT ALLTRIM(cSelctn), cIdentify;
FROM SYWACHRT WHERE .T. INTO ARRAY laAllChart ;
ORDER BY cIdentify
SELECT SYWACHRT
LOCATE FOR lSelect
IF FOUND()
  lnCharNo = VAL(cIdentify)
ENDIF
=lfGetChart()
=lfvChar(.T.)
DO (gcScrDir + gcWinAppl + '\SMwacod.SPR')
POP KEY
RELEASE WINDOW (lcMtBrowTt)
RELEASE PAD _Option OF _MSYSMENU
RELEASE BAR 099 OF P01PU01
RELEASE BAR 100 OF P01PU01
*:B#804025,1 SSH 25/03/2001 Release the filter.
SELECT sywaccod
SET FILTER TO
*:B#804025,1 SSH 25/03/2001 [END]


*!*************************************************************
*! Name      : lfvChar
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for chart of account popup ..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfvChar()
*!*************************************************************
*!
FUNCTION lfvChar
PARAMETER llRefr
SELECT SYWACHRT
REPLACE ALL lSelect WITH .F.
IF SEEK(laAllChart[lnCharNo,2])
  REPLACE lSelect WITH .T.
ENDIF
SELECT sywaccod
SET FILTER TO
SET FILTER TO cidentify = STR(lnCharNo,1)
LOCATE FOR !EMPTY(cAcSegDes)
lcAccName = cAcSegDes
=lfGetHelp(laAllChart[lnCharNo,2])
llDummy = !llRefr .AND. lfActsBrow()


*!*************************************************************
*! Name      : lfWChar
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Get help function ..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfWChar()
*!*************************************************************
*!
FUNCTION lfWChar
=lfGetHelp(laAllChart[lnCharNo,2])


*!*************************************************************
*! Name      : lfActsBrow
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Activate the browse.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfActsBrow()
*!*************************************************************
FUNCTION lfActsBrow

SELECT &lcTmpQuery
GO TOP
lcBrfield1 = [cMark     :02 :H = ''                :W=.F. ,] +;
             [cAcctCode :15 :H = lcAccName       :W=.F. ,] +;
             [cAccnlDes :63 :H = 'Account description':W=.F.]

BROWSE FIELDS &lcBrField1;
       WHEN lfwBrows();
       FOR cidentify = STR(lnCharNo,1);
       VALID :F lfvBrowse() ;
       WINDOW SMWACODB IN WINDOW (gcBaseWind);
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       TITLE ALLTRIM(lcMtBrowTt)


*!*************************************************************
*! Name      : lfwBrows
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : When Browse Temp. sequence file fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrows()
*!*************************************************************
*!
FUNCTION lfwBrows

SELECT (lcTmpQuery)
lnNewRec = RECNO()
REPLACE ALL cMark WITH ' '
GO lnNewRec
REPLACE cMark WITH '>'
SHOW WINDOW (lcMtBrowTt) REFRESH
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfvBrowse
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid Browse Temp. sequence file fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrowse()
*!*************************************************************
*!
FUNCTION lfvBrowse

IF ALLTRIM(WONTOP()) # ALLTRIM((lcMtBrowTt))
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- end of lfvBrowse.


*!*************************************************************
*! Name      : lfGetChart
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Get Chart of account ..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfGetChart()
*!*************************************************************
*!
FUNCTION lfGetChart

PRIVATE lnOldAls
lnOldAls = SELECT(0)
IF !USED("sywglchr")
   = gfOpenFile(gcSysHome+"sywglchr","","SH")
ENDIF
SELECT sywglchr
SELECT " " AS cMark ,cAcctCode, cAccnlDes ,cIdentify FROM sywglchr INTO DBF gcWorkDir+(lcTmpQuery)
SELECT sywaccod
SET FILTER TO
SET FILTER TO cidentify = STR(lnCharNo,1)
LOCATE FOR !EMPTY(cAcSegDes)
lcAccName = cAcSegDes
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfUnTrap
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : Activate screen function....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
FUNCTION lfUnTrap

* -- Clear Trap
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If TOP window is not one of the browses window                                        
IF ALLTRIM(WONTOP()) # ALLTRIM((lcMtBrowTt))
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL CTRL+TAB
  ON KEY LABEL CTRL+ENTER
  ON KEY LABEL CTRL+HOME
  ON KEY LABEL CTRL+END
  ON KEY LABEL CTRL+W
  ON KEY LABEL ENTER
ENDIF
*-- end of lfUnTrap.

*!*************************************************************
*! Name      : lfKey
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Screen ICINVLK When Function.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfKey()
*!*************************************************************
*!
FUNCTION lfKey

PUSH KEY
DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 100 OF P01PU01 PROMPT lcMtBrowTt KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcMtBrowTt)

*!*************************************************************
*! Name      : lfTrap
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trap function for Browse....
*!           : Deactivate screen function.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpTab,lpBackTab
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
FUNCTION lfTrap

*-- if TOP window is one of the browse window
IF ALLTRIM(WONTOP()) = ALLTRIM((lcMtBrowTt))
  glFromBrow = .T.
  ON KEY LABEL CTRL+ENTER lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+HOME  lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+W     lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+END   lnDummy = 1		&&Do nothing
  ON KEY LABEL TAB           DO lpTab
  ON KEY LABEL CTRL+TAB      DO lpTab
  ON KEY LABEL BACKTAB       DO lpBackTab
  =lfGetHelp("CODBROW")
ENDIF
*-- end of lfTrap.


*!*************************************************************
*! Name      : lpTab
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trapping TAB order for browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpTab
*!*************************************************************
PROCEDURE lpTab

ON KEY LABEL TAB
ON KEY LABEL CTRL+TAB
*-- Go to next window
IF ALLTRIM(WONTOP()) = ALLTRIM((lcMtBrowTt))
   ACTIVATE WINDOW SMWACOD2
  _CUROBJ = OBJNUM(pbResume)
ELSE
   ACTIVATE WINDOW (lcMtBrowTt)
  _CUROBJ = OBJNUM(ibBBrow)
ENDIF
*-- end of lpTab.

*!*************************************************************
*! Name      : lpBackTab
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trapping BACKTAB order for browse window
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpBackTab
*!*************************************************************
PROCEDURE lpBackTab

ON KEY LABEL BACKTAB
IF ALLTRIM(WONTOP()) = ALLTRIM((lcMtBrowTt))
  *-- Go to previous window
  ACTIVATE WINDOW SMWACOD1		&&Activate popup windows
  _CUROBJ = OBJNUM(lnCharNo)
ELSE
   ACTIVATE WINDOW (lcMtBrowTt)
  _CUROBJ = OBJNUM(ibBBrow) && Activate control panel window
ENDIF  
*-- end of lpBackTab.

*!*************************************************************
*! Name      : lfvaNext
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Next button valid function ..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfvaNext()
*!*************************************************************
*!
FUNCTION lfvaNext

SELECT SYWACHRT
REPLACE ALL lSelect WITH .F.
IF SEEK(laAllChart[lnCharNo,2])
  REPLACE lSelect WITH .T.
ENDIF
SELECT sywaccod
REPLACE ALL lSelect WITH .T. FOR cIdentify = STR(lnCharNo,1)
LOCATE FOR lSelect
lcNextActn = 'N'

*!*************************************************************
*! Name      : lfSetFilt
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Funcion to set filter for the selected chart of account
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfSetFilt()
*!*************************************************************
*!
FUNCTION lfSetFilt

IF !USED("SYWACBAL")
  =gfOpenFile(gcSysHome+"SYWACBAL","","SH")
ENDIF
SELECT SYWACBAL
SET FILTER TO cIdentify = STR(lnCharNo,1)

IF !USED("SYWACCOD")
  =gfOpenFile(gcSysHome+"SYWACCOD","","SH")
ENDIF
SELECT SYWACCOD
SET FILTER TO cIdentify = STR(lnCharNo,1)

IF !USED("SYWGLCHR")
  =gfOpenFile(gcSysHome+"SYWGLCHR","","SH")
ENDIF
SELECT SYWGLCHR
SET FILTER TO cIdentify = STR(lnCharNo,1)

IF !USED("SYWGLSGV")
  =gfOpenFile(gcSysHome+"SYWGLSGV","","SH")
ENDIF
SELECT SYWGLSGV
SET FILTER TO cIdentify = STR(lnCharNo,1)

IF !USED("SYWGLTYP")
  =gfOpenFile(gcSysHome+"SYWGLTYP","","SH")
ENDIF
SELECT SYWGLTYP
SET FILTER TO cIdentify = STR(lnCharNo,1)
*:B#804025,1 SSH 25/03/2001 Add default like code to created company.
IF !USED("SYWLINK")
  =gfOpenFile(gcSysHome+"SYWLINK","","SH")
ENDIF
SELECT SYWLINK
SET FILTER TO cIdentify = STR(lnCharNo,1)
SELECT SYWACHRT
SET FILTER TO
*:B#804025,1 SSH [END]