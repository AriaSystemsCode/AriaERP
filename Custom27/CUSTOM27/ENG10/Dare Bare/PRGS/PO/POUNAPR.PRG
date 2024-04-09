*:***************************************************************************
*: Program file  : POUNAPR
*: Program desc. : View unapproved operation by user
*: For screen    : POUNAPR.SCX
*:        System : Aria Advantage Series.
*: For Module    : Style Purchase Order - (PO)
*:        Date   : 02/19/2003
*:     Developer : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Example : DO POUNAPR
*:***************************************************************************
*: Due to C200471
*: Modification :
*: C037345,1 ABD Some Modification to the program.
*:***************************************************************************
EXTERNAL ARRAY laDefProc,laCtrStat,laScrMode

laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
laDefProc[10] = .F.              && Disable the control panel close proc. (lpClsScr)

STORE SPACE(0) TO lcUserName,lcDays,lcRpShwOpr
lcDays = DTOC(gdSysDate)
STORE 0 TO lnRpDays

*-- lnBrRecNo  : Browse Record Number
STORE 0 TO lnBrRecNo 

DECLARE laPanelObj[1,3]
STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbScope'
laPanelObj[1,2] = gcBmpHome+'SCOPE.BMP'
laPanelObj[1,3] = [VALID lfvScope()]
llFirst = .T.
lcMajorPic = gfItemMask('PM')
lnMajorLen = LEN(lcMajorPic)

IF !gfSetup()
  RETURN
ENDIF  

lcBaseFile = ''
laCtrStat[7]  = "ENABLE"  &&Edit
laCtrStat[10] = "DISABLE" &&Browse

*C037345,1 ABD - Define the Source array and target array that will hold the operations. [Begin]
DIMENSION  laRpSorOpr [1,1] , laRpTarOpr[1,1]
STORE '' TO lcRpOprExp , laRpSorOpr , laRpTarOpr , lcRpVewOpr
*C037345,1 ABD - [End]

lcTmpOpert = gfTempName()     && File to hold selected operations.
lcOperBrow = 'Operations'
llBrowse = .F.
*-- Define Screens Variables and Temporary file [End  ]

*-- Create Temp. File [Begin]
*C037345,1 ABD - Add user id into the temp file to approve all operation related to this user. [Begin]
*CREATE TABLE (gcWorkDir+lcTmpOpert) (PO C(6),STYLE C(19),VENDOR C(8),CCRPTHOPR C(40),;
                                     DREQUIRED D,DAPPROVED D,CCOMMENTS C(30),NOTE_MEM  M,;
                                     CSTYTYPE C(1),NLINENO N(4,0),CCRPTMPCOD C(6))
CREATE TABLE (gcWorkDir+lcTmpOpert) (PO C(6),STYLE C(19),VENDOR C(8),CCRPTHOPR C(40),;
                                     DREQUIRED D,DAPPROVED D,CCOMMENTS C(30),NOTE_MEM  M,;
                                     CSTYTYPE C(1),NLINENO N(4,0),CCRPTMPCOD C(6) , cuser_id C(10))

*C037345,1 ABD - [End]
INDEX ON PO+STYLE TAG (lcTmpOpert) OF (gcWorkDir+lcTmpOpert)
*-- Create Temp. File [End  ]

=lfSetKey()

DO (gcScrDir+gcWinAppl+'\POUNAPR.SPX')

*-- Screen Cleanup Code.
glQuitting = .T.  && Rise quit flag because it's modal screen.

POP KEY
RELEASE WINDOW (lcOperBrow)

IF USED(lcTmpOpert)
  USE IN (lcTmpOpert)
  ERASE &gcWorkdir.&lcTmpOpert..DBF          && Erase the Temp file.
ENDIF

IF FILE(gcWorkDir+lcTmpOpert+'.CDX')
  ERASE &gcWorkdir.&lcTmpOpert..CDX          && Erase the Temp file.
ENDIF

IF FILE(gcWorkDir+lcTmpOpert+'.FBT')
  ERASE &gcWorkdir.&lcTmpOpert..FBT          && Erase the Temp file.
ENDIF

*-- end of program code.

**********************************************************************
**************** Control Browse and trapping Functions ***************
**********************************************************************

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/20/2003
*! Purpose   : Function to create the Browse
*!*************************************************************
*
FUNCTION lfDispBrow

SELECT (lcTmpOpert)
LOCATE

lnBrRecNo  = RECNO()

*C037345,1 ABD - Change the browse to be not scrolled. [Begin]
*lcBrowFild = "lcMarker=IIF(RECNO()=lnBrRecNo , '>' , ' ') :1 :H= ' ' :W= .F. ,"+;
             "PO        :R :H = 'Order No'  :10,"+;
             "VENDOR    :R :H = 'Account'   :10,"+;
             "STYLE     :R :H = 'Style'     :19,"+;
             "CCRPTHOPR :R :H = 'Operation' :40,"+;
             "DREQUIRED :R :H = 'Required'  :10,"+;
             "DAPPROVED    :H = 'Approved'  :10"+IIF(!laScrMode[3],":R","")
lcBrowFild = "lcMarker=IIF(RECNO()=lnBrRecNo , '>' , ' ') :1 :H= ' ' :W= .F. ,"+;
             "PO        :R :H = 'Order No'  :10,"+;
             "VENDOR    :R :H = 'Account'   :10,"+;
             "STYLE     :R :H = 'Style'     :17,"+;
             "CCRPTHOPR :R :H = 'Operation' :20,"+;
             "DREQUIRED :R :H = 'Required'  :10,"+;
             "DAPPROVED :R :H = 'Approved'  :10 "
*C037345,1 ABD - [End]

BROWSE FIELDS &lcBrowFild     ;
       WINDOW POUNAPR2        ;
       WHEN lfwBrows()        ;
       IN WINDOW POUNAPR      ;
       LOCK 0                 ;
       NOAPPEND               ;
       NOCLEAR                ;
       NODELETE               ;
       NOWAIT                 ;
       NOMENU                 ;
       SAVE                   ;
       TITLE lcOperBrow
*-- end of lfDispBrow.

*!*************************************************************
*! Name      : lfwBrows
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/26/2003
*! Purpose   : When Browse Function.
*!*************************************************************
*
FUNCTION lfwBrows
lnBrRecNo  = RECNO(lcTmpOpert)
SHOW WINDOW (lcOperBrow) REFRESH
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfwIbBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/20/2003
*! Purpose   : When function of the invisible button IbBrow
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*
FUNCTION lfwIbBrow
*-- IF The left mouse button is not pressed
IF !MDOWN()
  KEYBOARD "{ALT+B}" CLEAR 
  RETURN .T.
ENDIF    && End of IF
RETURN .F.
*-- end of lfwIbBrow.

*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/20/2003
*! Purpose   : Function to trap the keys for the Browse , 
*!             and save the changes if the current record was edited
*!*************************************************************
*
FUNCTION lfBrowTrap
*-- IF The window on top is the Browse
IF WONTOP(lcOperBrow)
  glFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH "TAB"
  ON KEY LABEL BACKTAB DO lfTraps WITH "BACKTAB"
  ON KEY LABEL ALT+B 

ENDIF    && End of IF
*-- end of lfBrowTrap.

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/20/2003
*! Purpose   : Function to untrap the keys for the Browse
*!*************************************************************
*
FUNCTION lfBrwUnTrp

*-- IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcOperBrow) .AND. glFromBrow
  = gfStopBrow()
  glFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcOperBrow)
ENDIF    && End of IF
*-- end of lfBrwUnTrp.

*!*************************************************************
*! Name      : lfTraps
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/20/2003
*! Purpose   : Trap (Tab / Backtab) Keys
*!*************************************************************
*
FUNCTION lfTraps
PARAMETERS lcTrap
*-- Tab Case
IF lcTrap = "TAB"
  ACTI WINDOW POUNAPR3 TOP
  _CUROBJ = OBJNUM(pbCom)

ELSE  && Backtab Case.
  ACTI WINDOW POUNAPR1 TOP
  _CUROBJ = OBJNUM(lcUserName)
ENDIF
*-- end of lfTraps.

**********************************************************************
********************** Control Screen Functions **********************
**********************************************************************

*!*************************************************************
*! Name      : lfSetKey
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/20/2003
*! Purpose   : Set Active Keys
*!*************************************************************
*!
FUNCTION lfSetKey
PUSH KEY

ON KEY LABEL ALT+B ACTIVATE WINDOW (lcOperBrow)
ON KEY LABEL ESC   DO gfCPClose
*-- end of lfSetKey.

*!*************************************************************
*! Name      : lfClearKey
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/11/2003
*! Purpose   : Clear Active Keys
*!*************************************************************
*!
FUNCTION lfClearKey
ON KEY LABEL ESC
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+B
*-- end of lfClearKey.

**********************************************************************
************************ Business Rules Section **********************
**********************************************************************

*!*************************************************************
*! Name      : lpSavScr
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/20/2003
*! Purpose   : To Save care label in Style file.
*!*************************************************************
*!
PROCEDURE lpSavScr

SELECT (lcTmpOpert)
SCAN
  SCATTER FIELDS DAPPROVED,CCOMMENTS,NOTE_MEM MEMO MEMVAR
  *C037345,1 ABD - Change the current login user with user at the temp file to let any one
  *C037345,1 ABD - approve any operation. [Begin]
  *IF SEEK(gcUser_Id+style+cstytype+po+ccrptmpcod+STR(nlineno,4),'STYCRPLN')
  IF SEEK(IIF(lcRpVewOpr = "O",&lcTmpOpert..cuser_id,gcUser_Id)+style+cstytype+po+ccrptmpcod+STR(nlineno,4),'STYCRPLN')
    *C037345,1 ABD - [End]
    SELECT STYCRPLN
    GATHER FIELDS DAPPROVED,CCOMMENTS,NOTE_MEM MEMO MEMVAR
    *C037345,1 ABD - Update the new field that will hold the user name that approved the order. [Begin]
    IF !EMPTY(DAPPROVED)
      REPLACE USr_Apr WITH gcUser_Id
    ENDIF
    *C037345,1 ABD - [End]
  ENDIF
ENDSCAN
ZAP
STORE .F. TO laScrMode
laScrMode[1] = .T.
*-- end of lpSavScr.

*!*************************************************************
*! Name      : lpClsScr
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/20/2003
*! Purpose   : To Cancel all changes.
*!*************************************************************
*!
PROCEDURE lpClsScr

=lfGetInfo()
*-- end of lpClsScr.

*!*************************************************************
*! Name      : lpShow
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/20/2003
*! Purpose   : Handle controfl show.
*!*************************************************************
*!
PROCEDURE lpShow

DO CASE
  CASE laScrMode[1]                       && Select mode.
    =lfDispBrow()
    SHOW GET pbCom   DISABLE
    SHOW GET pbNotes DISABLE
    SHOW GET pbScope ENABLE
    SHOW GET pbEdt   DISABLE
    IF llFirst
      llFirst = .F.
      =lfvScope()
    ENDIF
  CASE laScrMode[2]                       && View mode 
    =lfDispBrow()
    SHOW GET pbScope ENABLE
    IF RECCOUNT(lcTmpOpert) = 0
      SHOW GET pbCom   DISABLE
      SHOW GET pbNotes DISABLE
      SHOW GET pbEdt   DISABLE
    ELSE
      SHOW GET pbCom   ENABLE
      SHOW GET pbNotes ENABLE
      SHOW GET pbEdt   ENABLE
    ENDIF
    SHOW GET pbDlt  DISABLE
    SHOW GET pbSlct DISABLE
  CASE laScrMode[3]                       && Edit mode 
    =lfDispBrow()
    SHOW GET pbScope DISABLE
    llCUpDate = .T.
ENDCASE
*-- end of lpShow.

*!*************************************************************
*! Name      : lfvScope
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/20/2003
*! Purpose   : validate the scope botton
*!*************************************************************
*! Example     : = lfvScope()
*!*************************************************************
FUNCTION lfvScope

PUSH KEY CLEAR
llExpr = gfOpGrid('POUNAPR' , .T.)  && Run selection grid.
POP KEY
glQuitting = .T.  && Rise quit flag because it's modal screen.

PRIVATE llOpenUser
llOpenUser = .F.
IF !USED('SYUUSER')
  llOpenUser = gfOpenFile(gcSysHome+'SYUUSER',gcSysHome+'CUSER_ID','SH')
ENDIF
IF SEEK(gcUser_Id,'SYUUSER')
  lcUserName = SYUUSER.CUSR_NAME
ENDIF
IF TYPE('llExpr') <> 'L'
  DO CASE
    CASE lcRpShwOpr = 'A'
      lcDays = DTOC(gdSysDate)
    CASE lcRpShwOpr = 'D'
      lcDays = DTOC(gdSysDate) + ' + ' + ALLTRIM(STR(SYUUSER.NPROMDAYS,3,0)) + ' days'
    CASE lcRpShwOpr = 'U'
      lcDays = DTOC(gdSysDate) + ' + ' + ALLTRIM(STR(lnRpDays,3,0)) + ' days'
  ENDCASE
  =lfGetInfo()
  IF RECCOUNT(lcTmpOpert) > 0
    STORE .F. TO laScrMode
    laScrMode[2] = .T.
  ENDIF
ENDIF
IF llOpenUser
  USE IN SYUUSER
ENDIF
SHOW GET lcUserName
SHOW GET lcDays
SHOW GETS
*-- end of lfvScope.

*!*************************************************************
*! Name      : lfvShwOpr
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/20/2003
*! Purpose   : validate the Show operations option
*!*************************************************************
*! Example     : = lfvShwOpr()
*!*************************************************************
FUNCTION lfvShwOpr

CLEAR READ
*-- end of lfvShwOpr.

*!*************************************************************
*! Name      : lfvNote
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/23/2003
*! Purpose   : valid function of notes button.
*!*************************************************************
*
FUNCTION lfvNote

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpOpert)
DO (gcScrDir+"ARLNOTES.SPX")
SELECT(lnalias)
ACTIVATE WINDOW (lcOperBrow)
RETURN
*-- end of lfvNote.

*!*************************************************************
*! Name      : lfvCom
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/23/2003
*! Purpose   : valid function of Comments button.
*!*************************************************************
*
FUNCTION lfvCom

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpOpert)
SCATTER FIELDS CCOMMENTS MEMVAR
DO (gcScrDir+gcWinAppl+'\POCOMSCR.SPX')
GATHER FIELDS CCOMMENTS MEMVAR
SELECT(lnalias)
ACTIVATE WINDOW (lcOperBrow)
RETURN
*-- end of lfvCom.

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/23/2003
*! Purpose   : Collecting the selected records
*!*************************************************************
*
FUNCTION lfGetInfo

SELECT (lcTmpOpert)
ZAP

*C037345,1 ABD - Check before collect the data if the user will select by account or by operation. [Begin]
IF lcRpVewOpr = 'U'
  *C037345,1 ABD - [End]
  
  IF SEEK(gcUser_Id,'STYCRPLN')
    SELECT STYCRPLN
    SCAN REST WHILE cuser_id+style+cstytype+po+ccrptmpcod+STR(nlineno,4) = gcUser_Id;
                FOR EMPTY(DAPPROVED)
      IF lcRpShwOpr # 'A' .AND. DREQUIRED > gdSysDate + IIF(lcRpShwOpr='D',SYUUSER.NPROMDAYS,lnRpDays)
        LOOP
      ENDIF
      SCATTER MEMO MEMVAR
      m.Vendor = ''
      IF CSTYTYPE = 'P'
        IF SEEK(CSTYTYPE+PO,'POSHDR')
          m.Vendor = POSHDR.VENDOR
        ENDIF
      ELSE
        IF SEEK('O'+PO,'ORDHDR')
          m.Vendor = ORDHDR.ACCOUNT
        ENDIF
      ENDIF
      INSERT INTO (lcTmpOpert) FROM MEMVAR
    ENDSCAN
  ENDIF

  *C037345,1 ABD - Else for if statment. [Begin]
ELSE
  SELECT STYCRPLN
  SCAN FOR EMPTY(DAPPROVED) .AND. ALLTRIM(cCrpthOpr) $ ALLTRIM(lcRpOprExp)
    IF lcRpShwOpr # 'A' .AND. DREQUIRED > gdSysDate + IIF(lcRpShwOpr='D',SYUUSER.NPROMDAYS,lnRpDays)
      LOOP
    ENDIF
    
    SCATTER MEMO MEMVAR
    m.Vendor = ''
    IF CSTYTYPE = 'P'
      IF SEEK(CSTYTYPE+PO,'POSHDR')
        m.Vendor = POSHDR.VENDOR
      ENDIF
    ELSE
      IF SEEK('O'+PO,'ORDHDR')
        m.Vendor = ORDHDR.ACCOUNT
      ENDIF
    ENDIF
    INSERT INTO (lcTmpOpert) FROM MEMVAR
  ENDSCAN

ENDIF
*C037345,1 ABD - [End]

IF RECCOUNT(lcTmpOpert) > 0
  SELECT (lcTmpOpert)
  LOCATE
ENDIF

*C037345,1 ABD - [Begin]
*!*************************************************************
*! Name      : lfvOpraton
*! Developer : Abdou Elgendy [ABD]
*! Date      : 01/20/2004
*! Purpose   : Call mover function then Evaluate Operation Expration.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover,lfCmpExpr
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvOpraton()
*!*************************************************************
*!
FUNCTION lfvOpraton

IF EMPTY(laRpTarOpr)
  DIMENSION  laRpTarOpr[1,1]
ENDIF
SELECT DISTINCT cCrpthOpr FROM stycrpln INTO ARRAY laRpSorOpr
= gfMover(@laRpSorOpr,@laRpTarOpr,'Operation',.T.,'')  && call mover function.

= lfCmpExpr()

*-- end of lfvOpraton.
*!*************************************************************
*! Name      : lfCmpExpr
*! Developer : Abdou Elgendy [ABD]
*! Date      : 01/20/2004
*! Purpose   : Evaluate Operation expression.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : lfvOpraton.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfCmpExpr()
*!*************************************************************
*!
FUNCTION lfCmpExpr
PRIVATE laTarget

IF EMPTY(laRpTarOpr)
  = ACOPY(laRpSorOpr,laTarget)
ELSE
  = ACOPY(laRpTarOpr,laTarget)
ENDIF

= ASORT(laTarget)
lcRpOprExp = ''

FOR lnI = 1 TO ALEN(laTarget,1)
  lcRpOprExp = IIF(EMPTY(lcRpOprExp),laTarget[lnI],lcRpOprExp + ','+laTarget[lnI])
ENDFOR

*-- end of lfCmpExpr.

*!*************************************************************
*! Name      : lfvApprv
*! Developer : Abdou Elgendy [ABD]
*! Date      : 01/20/2004
*! Purpose   : Evaluate Operation expression.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : lfvOpraton.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvApprv()
*!*************************************************************
*!
FUNCTION lfvApprv
PRIVATE lnPrv_Als 

lnPrv_Als  = SELECT(0)

SELECT (lcTmpOpert)
IF EMPTY(DAPPROVED)
  REPLACE DAPPROVED WITH gdSysDate
ELSE
  REPLACE DAPPROVED WITH {}
ENDIF

IF !EOF()
  SKIP
  IF EOF()
    SKIP - 1
  ENDIF
ENDIF
=lfwBrows()

SELECT (lnPrv_Als)
*-- End OF lfvApprv
*C037345,1 ABD - [End]
*!*************************************************************