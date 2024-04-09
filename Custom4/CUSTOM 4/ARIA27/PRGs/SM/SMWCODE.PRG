*:********************************************************************
*: Procedure file: SMWCODES.PRG   N#000185
*:                 (Codes)
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: SYSTEM MANAGER
*:         Author: Ahmed Salah Shalaby - (SSH)
*:      Copyright (c) 
*:********************************************************************
*: Modification :
*:B#804025,1 SSH 25/03/2001 Do not display the automatic generated code.
*:B#804025,1 SSH 25/03/2001 in the browse.
*:************************************************************************
PRIVATE laCodName , laCodes
DECLARE laCodes[1,2] , laCodName[1,2] , laLiStarr[1,2]
laCodes    = SPACE(1)
laCodName  = SPACE(1)
laLiStarr  = SPACE(1)
lcPrompt   = "S\<elect"    && Hold the word "S\<elect"
lcNPrompt  = "\<Next >"
=gfOpenFile(gcSysHome+'SYDFIELD')
=gfOpenFile(gcSysHome+"SYWCODE","","SH")
lcTmpQuery= gfTempName()
PRIVATE lnCodes
lcMtBrowTt = "Default  Codes"
lnAllCodes = 0
lcWinTitl  = "Codes Screen"
=lfWRun()
lnCodes = IIF(lcNextActn="B",lnAllCodes,1)
=lfGetCodes()
DO (gcScrDir + gcWinAppl + '\SMWCODE.SPR')
=lfEraTemp(lcTmpQuery)
POP KEY
RELEASE WINDOW (lcMtBrowTt)
RELEASE PAD _Option OF _MSYSMENU
RELEASE BAR 099 OF P01PU01
RELEASE BAR 100 OF P01PU01

*!*************************************************************
*! Name      : lfWRun
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : When enter screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfWRun()
*!*************************************************************
*!
FUNCTION lfWRun

SELECT SYWCODE
SET ORDER TO Ccode_no

SELECT CFLD_HEAD, CFLD_NAME;
FROM SYDFIELD WHERE LVLDENTRY = .T. .AND. SEEK('N'+CFLD_NAME,'SYWCODE') INTO ARRAY laCodes ;
ORDER BY CFLD_HEAD

lnAllCodes = _TALLY
SELECT SYWCODE
*:B#804025,1 SSH 25/03/2001 Initiate the browse tittla.
lcMtBrowTt = "Default  "+ALLTRIM(laCodes[1,1])
*:B#804025,1 SSH [END]

LOCATE FOR lStart
IF FOUND()
  REPLACE lStart WITH .F.
  lnElemNo = ASCAN(laCodes,PADR(SYWCODE.cFld_Name,10))
  lnCodes = ASUBSCRIPT(laCodes,lnElemNo,1)
  lcMtBrowTt = "Default  "+ALLTRIM(laCodes[lnCodes,1])
ENDIF

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
IF ALLTRIM(WONTOP()) = ALLTRIM((lcMtBrowTt))
  glFromBrow = .T.
  ON KEY LABEL CTRL+ENTER lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+HOME  lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+W     lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+END   lnDummy = 1		&&Do nothing
  ON KEY LABEL TAB           DO lpTab
  ON KEY LABEL CTRL+TAB      DO lpTab
  ON KEY LABEL BACKTAB       DO lpBackTab
  ON KEY LABEL ENTER         DO lfEnter
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
   ACTIVATE WINDOW SMWCODE3
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

IF ALLTRIM(WONTOP()) = ALLTRIM((lcMtBrowTt))
  *-- Go to previous window
  ACTIVATE WINDOW SMWCODE2		&&Activate popup windows
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
PRIVATE llIsEdit
SELECT &lcTmpQuery
GO TOP
*:B#804025,1 SSH 25/03/2001 Do not display the automatic generated code.
*:B#804025,1 SSH 25/03/2001 in the browse.
llIsEdit = .F.
SET ORDER TO Cfld_name IN SYDFIELD
IF SEEK(ALLTRIM(laCodes[lnCodes,2]),"SYDFIELD")
  llIsEdit = (ALLTRIM(SYDFIELD.mCodeInfo) = "EDITABLE")
ENDIF
IF !llIsEdit
  lcBrfield1 = [cMark    :02 :H = ''                :W=.F. ,] +;
               [cSelect  :02 :H = ''                :W=.F. ,] +;
               [cDiscRep :63 :H = 'Code description':W=.F.]
ELSE
  lcBrfield1 = [cMark    :02 :H = ''                :W=.F. ,] +;
               [cSelect  :02 :H = ''                :W=.F. ,] +;
               [cCode_No :15 :H = 'Code name'       :W=.F. ,] +;
               [cDiscRep :63 :H = 'Code description':W=.F.]
ENDIF
*:B#804025,1 SSH {END}
BROWSE FIELDS &lcBrField1;
       WHEN lfwBrows();
       FOR cFld_Name + cCode_No = laCodes[lnCodes,2];
       VALID :F lfvBrowse() ;
       WINDOW SMWCODEB IN WINDOW (gcBaseWind);
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
lcPrompt = IIF(cSelect="»","\<Unselect","S\<elect")
SHOW GET pbSel,1 PROMPT lcPrompt
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
*! Name      : lfGetCodes
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Get al default code.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetCodes()
*!*************************************************************
*!
FUNCTION lfGetCodes

SELECT " " AS cMark , IIF(SYWCODE.lSelect,"»"," ") As cSelect ,cCode_No,cDiscRep,cFld_Name;
    FROM SYWCODE ;
    WHERE cFld_Name=laCodes[lnCodes,2];
     .AND. CRLTFIELD='N' .AND. cDefCode = 'N'  INTO DBF gcWorkDir+(lcTmpQuery)

SELECT (lcTmpQuery)
INDEX ON cFld_Name + cCode_No TAG (lcTmpQuery)


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
REPLACE ALL cSelect WITH "»" FOR cFld_Name + cCode_No = laCodes[lnCodes,2]
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
REPLACE ALL cSelect WITH " " FOR cFld_Name + cCode_No = laCodes[lnCodes,2]
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
REPLACE ALL cSelect WITH IIF(cSelect="»",' ',"»") FOR cFld_Name + cCode_No = laCodes[lnCodes,2]
GO lnNewRec
lcPrompt = IIF(cSelect="»","\<Unselect","S\<elect")
SHOW GET pbSel,1 PROMPT lcPrompt
SHOW WINDOW (lcMtBrowTt) REFRESH

*!*************************************************************
*! Name      : lfvNvigatr
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid navigator button.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNvigatr()
*!*************************************************************
*!
FUNCTION lfvNvigatr
PARAMETER lcAction

=lfUpdacode()
IF (lcAction = 'N' .AND. lnCodes = lnAllCodes);
   .OR. (lcAction = 'B' .AND. lnCodes = 1)
  lcNextActn = lcAction
  IF lcAction = 'N'
    IF  lcNPrompt  = "\<Next >"
      lcNPrompt  = "\<Finish"
      SHOW GET pbNxt,1 PROMPT lcNPrompt
      RETURN
    ENDIF
    SHOW GET pbNxt,1 PROMPT lcNPrompt
    lcNextActn = "F"
    IF gfModalGen("QRM00375B00006","DIALOG") = 1
      CLEAR READ
    ENDIF
  ELSE
    CLEAR READ
  ENDIF
ELSE
  lnCodes = lnCodes + IIF(lcAction = 'N',+1,-1)
  *--- cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam
  IF (lcAction = 'N' .AND. lnCodes > lnAllCodes);
   .OR. (lcAction = 'B' .AND. lnCodes < 1)
     IF lcAction = 'N'
       lcNextActn = lcAction
       IF  lcNPrompt  = "\<Next >"
         lcNPrompt  = "\<Finish"
         SHOW GET pbNxt,1 PROMPT lcNPrompt
         RETURN
       ENDIF
       SHOW GET pbNxt,1 PROMPT lcNPrompt
       lcNextActn = "F"
       IF gfModalGen("QRM00375B00006","DIALOG") = 1
         CLEAR READ
       ENDIF
     ELSE
       CLEAR READ
     ENDIF
  ELSE
    SHOW GET lnCodes DISABLE
    =lfGetDef()
  ENDIF
ENDIF
SELECT (lcTmpQuery)
=SEEK(laCodes[lnCodes,2])
DEACTIVATE WINDOW (lcMtBrowTt)
RELEASE WINDOW (lcMtBrowTt)
lcMtBrowTt = "Default  "+ALLTRIM(laCodes[lnCodes,1])
=lfActsBrow()
KEYBOARD"{ALT+B}"

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

=lfvNvigatr("N")

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

lcNPrompt  = "\<Next >"
SHOW GET pbNxt,1 PROMPT lcNPrompt
=lfvNvigatr("B")

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
=lfUpdacode()
SELECT SYWCODE
SET ORDER TO Ccode_no
=SEEK('N'+laCodes[lnCodes,2])
REPLACE lStart WITH .T.
CLEAR READ

*!*************************************************************
*! Name      : lfGetDef
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : funciotn to get default codes.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetDef()
*!*************************************************************
*!
FUNCTION lfGetDef

SELECT (lcTmpQuery)
IF !SEEK(laCodes[lnCodes,2])
  SELECT SYWCODE
  SET ORDER TO Ccode_no
  *--- cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam
  =SEEK('N'+laCodes[lnCodes,2])
  SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam=;
                  'N'+laCodes[lnCodes,2];
            FOR SYWCODE.CRLTFIELD='N'
    SCATTER MEMVAR MEMO
    INSERT INTO (lcTmpQuery) FROM MEMVAR
    SELECT (lcTmpQuery)
    REPLACE cSelect WITH IIF(SYWCODE.lSelect,"»"," ")
  ENDSCAN
ENDIF
SELECT (lcTmpQuery)

*!*************************************************************
*! Name      : lfUpdacode
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : funciotn to update codes.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfUpdacode()
*!*************************************************************
*!
FUNCTION lfUpdacode

SELECT SYWCODE
SET ORDER TO Ccode_no

SELECT (lcTmpQuery)
GO TOP
*--- cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam
=SEEK(laCodes[lnCodes,2])
SCAN REST WHILE cFld_Name + cCode_No = laCodes[lnCodes,2]
  lcKey = 'N'+laCodes[lnCodes,2]
  SELECT SYWCODE
  IF SEEK(lcKey+&lcTmpQuery..cCode_No)
    SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam=;
                    lcKey+&lcTmpQuery..cCode_No &&FOR   cCode_No==&lcTmpQuery..cCode_No
      WAIT WINDOW "Update codes file with "+ALLTRIM(laCodes[lnCodes,2])+" : "+ ccode_no NOWAIT
      REPLACE lSelect WITH IIF(&lcTmpQuery..cSelect="»",.T.,.F.)
    ENDSCAN
  ENDIF
ENDSCAN
WAIT CLEAR