*:********************************************************************
*: Procedure file: SMWSCALE.PRG   N#000185
*:                 (Scale screen)
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: SYSTEM MANAGER
*:         Author: Ahmed Salah Shalaby - (SSH)
*:      Copyright (c) 
*:********************************************************************
*: Modification :
*:********************************************************************
PRIVATE llExtSzScl
llExtSzScl = gfGetMemVar('M_USEEXSSC')

lcPrompt   = "S\<elect"    && Hold the word "S\<elect"
lcNPrompt  = "\<Next >"
=gfOpenFile(gcSysHome+"SYWSCAL","","SH")
lcTmpQuery = gfTempName()
lcMtBrowTt = "Default  Scale"
lcWinTitl  = "Scale Screen"
IF llExtSzScl
  lcTmpDet = gfTempName()
  =gfOpenFile(gcSysHome+"SYWSCALH","","SH")
  lcMDBrowTt = "Dimention Scale"
ENDIF
=lfGetScal()
IF llExtSzScl
  DO (gcScrDir + gcWinAppl + '\SMWESCAL.SPR')
ELSE
  DO (gcScrDir + gcWinAppl + '\SMWSCAL.SPR')
ENDIF
IF USED("SYWSCAL")
  =lfExit()
ENDIF

*!*************************************************************
*! Name      : lfGetScal
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function to get scales.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetScal()
*!*************************************************************
*!
FUNCTION lfGetScal

IF llExtSzScl
  SELECT " " AS cMark , IIF(SYWSCALH.lSelect,"»"," ") As cSelect ,cExtScale,cScaleDes;
      FROM SYWSCALH INTO DBF gcWorkDir+(lcTmpQuery)
  SELECT (lcTmpQuery)
  INDEX ON cExtScale TAG (lcTmpQuery)
ELSE
  SELECT " " AS cMark , IIF(SYWSCAL.lSelect,"»"," ") As cSelect ,SCALE,cSCL_DESC,SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8;
      FROM SYWSCAL WHERE cType="N" INTO DBF gcWorkDir+(lcTmpQuery)
  SELECT (lcTmpQuery)
  INDEX ON scale TAG (lcTmpQuery)
ENDIF
IF llExtSzScl
  SELECT SCALE,cSCL_DESC,SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8;
      FROM SYWSCAL WHERE cType="E" INTO DBF gcWorkDir+(lcTmpDet)
  SELECT (lcTmpDet)
  INDEX ON scale TAG (lcTmpDet)
ENDIF

*!*************************************************************
*! Name      : lfEnter
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trab enter key functoin.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfEnter()
*!*************************************************************
*!
FUNCTION lfEnter

SELECT (lcTmpQuery)
REPLACE cSelect WITH IIF(cSelect="»",' ',"»")
lcPrompt = IIF(cSelect="»","\<Unselect","S\<elect")

*!*************************************************************
*! Name      : lfvSel
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Select button.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSel()
*!*************************************************************
*!
FUNCTION lfvSel

SELECT (lcTmpQuery)
REPLACE cSelect WITH IIF(cSelect="»",' ',"»")
lcPrompt = IIF(cSelect="»","\<Unselect","S\<elect")
SHOW GET pbSel,1 PROMPT lcPrompt
SHOW WINDOW (lcMtBrowTt) REFRESH

*!*************************************************************
*! Name      : lfvAll
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Select all button.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvAll()
*!*************************************************************
*!
FUNCTION lfvAll

SELECT (lcTmpQuery)
REPLACE ALL cSelect WITH "»"
lcPrompt = "\<Unselect"
SHOW GET pbSel,1 PROMPT lcPrompt
GO TOP
SHOW WINDOW (lcMtBrowTt) REFRESH

*!*************************************************************
*! Name      : lfvNon
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Select non button.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNon()
*!*************************************************************
*!
FUNCTION lfvNon

SELECT (lcTmpQuery)
REPLACE ALL cSelect WITH " "
lcPrompt = "S\<elect"
SHOW GET pbSel,1 PROMPT lcPrompt
GO TOP
SHOW WINDOW (lcMtBrowTt) REFRESH

*!*************************************************************
*! Name      : lfvInv
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : invert Selection button.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvInv()
*!*************************************************************
*!
FUNCTION lfvInv

SELECT (lcTmpQuery)
lnNewRec = RECNO()
REPLACE ALL cSelect WITH IIF(cSelect="»",' ',"»")
GO lnNewRec
lcPrompt = IIF(cSelect="»","\<Unselect","S\<elect")
SHOW GET pbSel,1 PROMPT lcPrompt
SHOW WINDOW (lcMtBrowTt) REFRESH


*!*************************************************************
*! Name      : lfvNext
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid funciotn for next button.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNext()
*!*************************************************************
*!
FUNCTION lfvNext
lcNextActn = "N"
=lfUpdate()
=lfExit()
CLEAR READ


*!*************************************************************
*! Name      : lfvPre
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid funciotn for previous button.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPre()
*!*************************************************************
*!
FUNCTION lfvPre
lcNextActn = "B"
=lfUpdate()
=lfExit()
CLEAR READ



*!*************************************************************
*! Name      : lfvResu
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid funciotn for resume button.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvResu()
*!*************************************************************
*!
FUNCTION lfvResu

lcNextActn = "U"
=lfUpdate()
=lfExit()
CLEAR READ

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
IF llExtSzScl
  DEFINE BAR 101 OF P01PU01 PROMPT "\-" SKIP FOR .T.
  DEFINE BAR 102 OF P01PU01 PROMPT lcMDBrowTt KEY ALT+D
  ON SELECTION BAR 102 OF P01PU01 ACTIVATE WINDOW (lcMDBrowTt)
ENDIF
DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 100 OF P01PU01 PROMPT lcMtBrowTt KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcMtBrowTt)
*KEYBOARD"{ALT+B}"

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
IF (ALLTRIM(WONTOP()) = ALLTRIM((lcMtBrowTt))) .OR. (llExtSzScl .AND. ALLTRIM(WONTOP()) = ALLTRIM((lcMDBrowTt)))
  glFromBrow = .T.
  ON KEY LABEL CTRL+ENTER lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+HOME  lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+W     lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+END   lnDummy = 1		&&Do nothing
  ON KEY LABEL TAB           DO lpTab
  ON KEY LABEL CTRL+TAB      DO lpTab
  ON KEY LABEL BACKTAB       DO lpBackTab
  ON KEY LABEL ENTER         DO lfEnter
  =lfGetHelp("SCALBR")
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
IF ALLTRIM(WONTOP()) = ALLTRIM((lcMtBrowTt)) .OR. (llExtSzScl .AND. ALLTRIM(WONTOP()) = ALLTRIM((lcMDBrowTt)))
   ACTIVATE WINDOW SMWSCL3
  _CUROBJ = OBJNUM(pbSel)
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

IF ALLTRIM(WONTOP()) = ALLTRIM((lcMtBrowTt)) .OR. (llExtSzScl .AND. ALLTRIM(WONTOP()) = ALLTRIM((lcMDBrowTt)))
  *-- Go to previous window
  ACTIVATE WINDOW SMWSCL2		&&Activate popup windows
  _CUROBJ = OBJNUM(pbResume)
ELSE
   ACTIVATE WINDOW (lcMtBrowTt)
  _CUROBJ = OBJNUM(ibBBrow) && Activate control panel window
ENDIF  
*-- end of lpBackTab.

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

IF llExtSzScl
  lcBrfield1 = [cMark  :02 :H = ''                 :W=.F. ,] +;
               [cSelect  :02 :H = ''               :W=.F. ,] +;
               [lc=SUBSTR(cExtScale,1) :15 :H = 'Scale code'         :W=.F. ,] +;
               [cScaleDes :15 :H = 'Scale desc.'       :W=.F.]
  BROWSE FIELDS &lcBrField1;
         WHEN lfwBrows();
         VALID :F lfvBrowse() ;
         WINDOW SMWesclB IN WINDOW (gcBaseWind);
         LOCK 0;
         NOAPPEND;
         NOCLEAR;
         NODELETE;
         NOMENU;
         NOWAIT;
         SAVE;
         TITLE ALLTRIM(lcMtBrowTt)
ELSE
  lcBrfield1 = [cMark  :02 :H = ''                 :W=.F. ,] +;
               [cSelect  :02 :H = ''               :W=.F. ,] +;
               [Scale :15 :H = 'Scale code'         :W=.F. ,] +;
               [cSCL_DESC :15 :H = 'Scale desc.'       :W=.F. ,] +;
               [SZ1 :7 :H = 'Size1'       :W=.F. ,] +;
               [SZ2 :7 :H = 'Size2'       :W=.F. ,] +;
               [SZ3 :7 :H = 'Size3'       :W=.F. ,] +;
               [SZ4 :7 :H = 'Size4'       :W=.F. ,] +;
               [SZ5 :7 :H = 'Size5'       :W=.F. ,] +;
               [SZ6 :7 :H = 'Size6'       :W=.F. ,] +;
               [SZ7 :7 :H = 'Size7'       :W=.F. ,] +;
               [SZ8 :7 :H = 'Size8':W=.F.]
  BROWSE FIELDS &lcBrField1;
         WHEN lfwBrows();
         VALID :F lfvBrowse() ;
         WINDOW SMWSCLB IN WINDOW (gcBaseWind);
         LOCK 0;
         NOAPPEND;
         NOCLEAR;
         NODELETE;
         NOMENU;
         NOWAIT;
         SAVE;
         TITLE ALLTRIM(lcMtBrowTt)
ENDIF

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
lcPrompt = IIF(cSelect="»","\<Unselect","S\<elect")
SHOW GET pbSel,1 PROMPT lcPrompt
IF llExtSzScl
  SELECT (lcTmpDet)
  SET FILTER TO SUBSTR(&lcTmpDet..Scale,1,1)=SUBSTR(&lcTmpQuery..cExtScale,1,1)
  SHOW WINDOW (lcMDBrowTt) REFRESH
ENDIF
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

IF (ALLTRIM(WONTOP()) # ALLTRIM((lcMtBrowTt))) .OR. (llExtSzScl .AND. (ALLTRIM(WONTOP()) # ALLTRIM(lcMDBrowTt)))
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- end of lfvBrowse.


*!*************************************************************
*! Name      : lfUpdate
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function to updat the selected scales.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfUpdate()
*!*************************************************************
*!
FUNCTION lfUpdate
PRIVATE lnOldAls

lnOldAls = SELECT(0)
IF llExtSzScl
  SELECT SYWSCALH
  SCAN
    IF SEEK(cExtScale,lcTmpQuery)
      SELECT SYWSCALH
      REPLACE lSelect WITH &lcTmpQuery..cselect = "»"
    ENDIF
  ENDSCAN
  SELECT SYWSCAL
  SCAN
    IF SEEK(SUBSTR(SCALE,1,1),lcTmpQuery) .AND. cTYPE = "E"
      REPLACE lSelect WITH &lcTmpQuery..cselect = "»"
    ELSE
      REPLACE lSelect WITH .F.
    ENDIF
  ENDSCAN
ELSE
  SELECT SYWSCAL
  SCAN
    IF SEEK(SCALE,lcTmpQuery) .AND. cTYPE = "N"
      REPLACE lSelect WITH &lcTmpQuery..cselect = "»"
    ELSE
      REPLACE lSelect WITH .F.
    ENDIF
  ENDSCAN
ENDIF
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfDActsBrow
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function to updat deail browse.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfDActsBrow()
*!*************************************************************
*!
FUNCTION lfDActsBrow

SELECT &lcTmpDet
GO TOP
lcBrfield2 = [lc=SUBSTR(Scale,2) :15 :H = 'Scale Dimentions'         :W=.F. ,] +;
             [cSCL_DESC :15 :H = 'Dimentions desc.'       :W=.F. ,] +;
             [SZ1 :7 :H = 'Size1'       :W=.F. ,] +;
             [SZ2 :7 :H = 'Size2'       :W=.F. ,] +;
             [SZ3 :7 :H = 'Size3'       :W=.F. ,] +;
             [SZ4 :7 :H = 'Size4'       :W=.F. ,] +;
             [SZ5 :7 :H = 'Size5'       :W=.F. ,] +;
             [SZ6 :7 :H = 'Size6'       :W=.F. ,] +;
             [SZ7 :7 :H = 'Size7'       :W=.F. ,] +;
             [SZ8 :7 :H = 'Size8':W=.F.]

BROWSE FIELDS &lcBrField2;
       WHEN lfwBrows();
       VALID :F lfvBrowse() ;
       WINDOW SMWESCLD IN WINDOW (gcBaseWind);
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       TITLE ALLTRIM(lcMDBrowTt)


*!*************************************************************
*! Name      : lfExit
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function to exit session.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfExit()
*!*************************************************************
*!
FUNCTION lfExit

POP KEY
RELEASE WINDOW (lcMtBrowTt)
RELEASE PAD _Option OF _MSYSMENU
RELEASE BAR 099 OF P01PU01
RELEASE BAR 100 OF P01PU01
=lfEraTemp(lcTmpQuery)
USE IN SYWSCAL
IF llExtSzScl
  RELEASE WINDOW (lcMDBrowTt)
  RELEASE PAD _Option OF _MSYSMENU
  RELEASE BAR 100 OF P01PU01
  RELEASE BAR 101 OF P01PU01
  =lfEraTemp(lcTmpDet)
  USE IN SYWSCALH
ENDIF