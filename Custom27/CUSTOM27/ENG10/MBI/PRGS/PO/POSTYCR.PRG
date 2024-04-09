*:***************************************************************************
*: Program file  : POSTYCR
*: Program desc. : Style Critical Path
*: For screen    : POSTYCRP.SCX,POSTYCR1.SCX,POSTYCR2.SCX,POSTYCR3.SCX
*:        System : Aria Advantage Series.
*:        Module : Style Purchase Order (PO)
*:        Date   : 07/24/2002
*:     Developer : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Example : DO POCRTMP
*:***************************************************************************
*: Due to C200376,1
*:***************************************************************************
*: Modifications :
*: C200413,1 AMH 09/24/2002 Made some modifications.
*: C037345,1 ABD 01/20/2004 Made some modifications.
*: B129041,1 NNA 09/21/2005 Fix bug (Alias not Found) by Re attach the Sydobjct's Record
*:***************************************************************************
*

EXTERNAL ARRAY laDefProc,laData,laKeyField

laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
DECLARE laKeyField [4,4]

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'STYCRPHD'
laKeyField[1,4] = 1
laKeyField[2,1] = 'LEFT(laData[4],1)'
laKeyField[2,2] =.F.
laKeyField[2,3] = 'STYCRPHD'
laKeyField[2,4] = 2
laKeyField[3,1] = 'laData[5]'
laKeyField[3,2] =.F.
laKeyField[3,3] = 'STYCRPHD'
laKeyField[3,4] = 3
laKeyField[4,1] = 'laData[2]'
laKeyField[4,2] =.T.
laKeyField[4,3] = 'STYCRPHD'
laKeyField[4,4] = 4

STORE ' ' TO lcMjrPct
STORE 0   TO lnstylewid
STORE .T. TO llAlowNew
STORE ' ' TO laCodInfo
STORE ' ' TO laGroup,laLink
STORE 1   TO lnGroup,lnLink
STORE ''  TO lcOldVal,lcTranNo
llNoShow = .F.            && Flag to make the screen call the PROCEDURE lpShow evry time it run
glFromBrow = .T.
lcTmpCrLn  = gfTempName()     && File to hold StyCrPLn records and to brows from it.
lcTmpCrLn1 = gfTempName()     && Temp. index for the temp file of StyCrPLn.
lcCrLnBrow = 'Operations'
llBrowse = .F.
llLinkTo = .F.

IF !gfSetup()
  RETURN
ENDIF

lcMjrPct   = gfItemMask('PM')
lnstylewid = LEN(lcMjrPct)

*-- Create Group Array [Begin]
DIMENSION laCodInfo [1,10]
laCodInfo[1,01] = "CCRPTHGRP"
laCodInfo[1,02] = "laGroup"
laCodInfo[1,03] = "lnGroup"
laCodInfo[1,04] = ""
laCodInfo[1,05] = .F.
laCodInfo[1,06] = .F.
laCodInfo[1,07] = "CRPTMPLN"
laCodInfo[1,08] = "CRPTMPLN"
laCodInfo[1,09] = ""
laCodInfo[1,10] = "cCrPthGrp"

*C037345,1 ABD - Add new element to the array for the Production Samples received. [Begin]
*DIMENSION laGroup[1,2], laLink[5,2]
DIMENSION laGroup[1,2], laLink[6,2]
*C037345,1 ABD - [End]

STORE "" TO laGroup,laLink

=gfwCodePop ( @laCodInfo, "CCRPTHGRP" ,"L" )
laLink[1,1] = 'N/A'
laLink[2,1] = 'Style PO created'
laLink[3,1] = 'Style PO received'
laLink[4,1] = 'Material PO created'
laLink[5,1] = 'Material PO received'

*C037345,1 ABD - Add new element to the array for the Production Samples received. [Begin]
laLink[6,1] = 'Production Sample Received'
*C037345,1 ABD - [End]

laLink[1,2] = ' '
laLink[2,2] = '1'
laLink[3,2] = '2'
laLink[4,2] = '3'
laLink[5,2] = '4'

*C037345,1 ABD - Add new element to the array for the Production Samples received. [Begin]
laLink[6,2] = '5'
*C037345,1 ABD - [End]

llLinkTo = gfGetMemVar('LLINKTOCRP',gcAct_Comp)

*--Object link button.
DECLARE laPanelObj[1,3]
STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbObjlnk'
laPanelObj[1,2] = gcBmpHome+'RELATE.BMP'
laPanelObj[1,3] = [VALID lfvObjLnk() MESSAGE 'Object link' DISABLE ]

IF !WEXIST(gcBaseWind)
  *-- Screen fields variable, to be used to build the laData array.
  lcScFields = "Style,cCrPTmpCod,nLineNo,cStyType,Po,ShipDate,cStyCrpKey"
  SCATTER FIELDS &lcScFields. TO laData BLANK
  =lfCreatTmp()
ENDIF

DO (gcScrDir+gcWinAppl+"\POSTYCRP.SPX")
ON KEY LABEL ALT+B 
*!**************************************************************************
*! Name      : lpShow
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : procedure to refresh screen objects.
*!**************************************************************************
*
PROCEDURE lpShow
 
EXTERNAL ARRAY laScrMode

DO CASE
  CASE laScrMode[2] .OR. laScrMode[1] && VIEW & SELECT Mode
    IF laScrMode[2]
      laData[4] = IIF(laData[4]='S','Sales Order','Purchase Order')
    ELSE
      laData[4] = 'Sales Order'
    ENDIF
    =lfGetInfo()
    =lfwBrows()
    IF laScrMode[1]
      SHOW GET pbObjlnk DISABLE
      SHOW GET pbNotes  DISABLE
    ELSE
      SHOW GET pbObjlnk ENABLE
      SHOW GET pbNotes  ENABLE
    ENDIF
  CASE laScrMode[3]                   && EDIT Mode
    laData[4] = IIF(laData[4]='S','Sales Order','Purchase Order')
    IF EOF(lcTmpCrLn)
      SHOW GETS WINDOW POSTYCR3 DISABLE ONLY
      SHOW GET pbAdd     ENABLE
      _CUROBJ = OBJNUM(pbAdd)
    ENDIF
    =lfwBrows()
    SHOW GET pbObjlnk ENABLE
  CASE laScrMode[4]                   && ADD  Mode
    =lfGetInfo()
    =lfwBrows()
    laData[4] = 'Sales Order'
    SHOW GET pbObjlnk DISABLE
    SHOW GETS WINDOW POSTYCR1 DISABLE ONLY
    SHOW GETS WINDOW POSTYCR3 DISABLE ONLY
    SHOW GET pbSav DISABLE
    SHOW GET laData[1] Enable
    SHOW GET laData[2] Enable
    SHOW GET laData[4] Enable
    SHOW GET laData[5] Enable
    SHOW GET ibStyle   Enable
    SHOW GET ibCode    Enable
    SHOW GET ibOrder   Enable
    ACTIVATE WINDOW POSTYCR1 TOP
    _CUROBJ = OBJNUM(laData[1])
ENDCASE
SHOW GET pbUsrFields DISABLE
*-- end of lpShow.

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Function to untrap the keys for the Browse
*!*************************************************************
*
FUNCTION lfBrwUnTrp

*-- IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcCrLnBrow) .AND. glFromBrow
  = gfStopBrow()
  glFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+B DO lfDispBrow
ENDIF    && End of IF
*-- end of lfBrwUnTrp.

*!*************************************************************
*! Name      : lfTraps
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Trap (Tab / Backtab) Keys
*!*************************************************************
*
FUNCTION lfTraps
PARAMETERS lcTrap
*-- Tab Case
IF lcTrap = "TAB"
  ACTI WINDOW POSTYCR3 TOP
  _CUROBJ = OBJNUM(m.cCrPthOpr)
ELSE  && Backtab Case.
  ACTI WINDOW POSTYCR1 TOP
  _CUROBJ = OBJNUM(laData[6])
ENDIF
*-- end of lfTraps.

*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Function to trap the keys for the Browse , 
*!             and save the changes if the current record was edited
*!*************************************************************
*
FUNCTION lfBrowTrap
*-- IF The window on top is the Browse
IF WONTOP(lcCrLnBrow)
  glFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH "TAB"
  ON KEY LABEL BACKTAB DO lfTraps WITH "BACKTAB"
  ON KEY LABEL ALT+B 

ENDIF    && End of IF
*-- end of lfBrowTrap.

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Function to create the Browse
*!*************************************************************
*
FUNCTION lfDispBrow

SELECT (lcTmpCrLn)
lcBrowFild = "nLineNo                                     :H = 'Line No.'        :6  :R,"+;
             "cCrPthOpr                                   :H = 'Operation'       :60 :R,"+;
             "dRequired                                   :H = 'Required'        :15 :R,"+;
             "dApproved                                   :H = 'Approved'        :15 :R,"+;
             "nLeadTime                                   :H = 'Lead Time(Days)' :6  :R,"+;
             "cComments                                   :H = 'Comments'        :45 :R,"+;
             "lcGroup = gfCodDes(CCRPTHGRP , 'CCRPTHGRP') :H = 'Group'           :45 :R,"+;
             "cUser_Id                                    :H = 'User'            :15 :R,"+;
             "llAutRecalc = IIF(lAutRecalc,'Y','N')       :H = 'Auto Recalc.'    :2  :R"
BROWSE FIELDS &lcBrowFild     ;
       WINDOW POSTYCR2        ;
       WHEN lfwBrows()        ;
       IN WINDOW (gcBaseWind) ;
       LOCK 0                 ;
       NOAPPEND               ;
       NOCLEAR                ;
       NODELETE               ;
       NOWAIT                 ;
       NOEDIT                 ;
       NOMENU                 ;
       SAVE                   ;
       TITLE lcCrLnBrow
*-- end of lfDispBrow.

*!*************************************************************
*! Name      : lfwBrows
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : When Browse Function.
*!*************************************************************
*
FUNCTION lfwBrows

PRIVATE lnAlias,lcStatus
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
SCATTER MEMVAR
IF ASCAN(laGroup,m.cCrPthGrp) > 0
  lnGroup = ASUBSCRIPT(laGroup,ASCAN(laGroup,m.cCrPthGrp),1)
ENDIF
IF ASCAN(laLink,m.cLinkTo) > 0
  lnLink = ASUBSCRIPT(laLink,ASCAN(laLink,m.cLinkTo),1)
ENDIF
lcStatus = IIF(EMPTY(m.dApproved) .AND. (laScrMode[3] .OR. laScrMode[4]);
               .AND. !EMPTY(m.nLineNo),'ENABLE','DISABLE')
*C037345,1 ABD - Add new element to the array for the Production Samples received. [Begin]
*lcTranNo = IIF(lnLink>3,'Material PO',IIF(lnLink>1,'Style PO',''))
lcTranNo = IIF(lnLink>3,IIF(lnLink=6,'Style PO','Material PO'),IIF(lnLink>1,'Style PO',''))
*C037345,1 ABD - [End]

SHOW GET m.nLineNo
SHOW GET m.cCrPthOpr
SHOW GET m.dRequired
SHOW GET m.dApproved
SHOW GET m.nLeadTime
SHOW GET m.cComments
SHOW GET lnGroup
SHOW GET lnLink
SHOW GET m.cUser_Id
SHOW GET m.lAutRecalc
SHOW GET pbRemove &lcStatus.
SHOW GET lcTranNo ENABLE
IF laScrMode[3] .OR. laScrMode[4]
  IF lnLink = 1
    SHOW GET m.cPo DISABLE
    SHOW GET ibcPo DISABLE
  ELSE
    SHOW GET m.cPo ENABLE
    SHOW GET ibcPo ENABLE
  ENDIF
ELSE
  SHOW GET m.cPo
ENDIF
SHOW WINDOW (lcCrLnBrow) REFRESH SAME
SELECT (lnAlias)
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Function to create tmp file of STYCRPLN.
*!*************************************************************
*
FUNCTION lfCreatTmp

SELECT STYCRPLN
=AFIELDS(laFStru)

DIMENSION laFStru[ALEN(laFStru,1)+2,4]

laFStru[ALEN(laFStru,1),1] = 'nOldLineNo'
laFStru[ALEN(laFStru,1),2] = 'N'
laFStru[ALEN(laFStru,1),3] = 4
laFStru[ALEN(laFStru,1),4] = 0

laFStru[ALEN(laFStru,1)-1,1] = 'cStatus'
laFStru[ALEN(laFStru,1)-1,2] = 'C'
laFStru[ALEN(laFStru,1)-1,3] = 1
laFStru[ALEN(laFStru,1)-1,4] = 0

CREATE CURSOR (lcTmpCrLn) FROM ARRAY laFStru
SELECT (lcTmpCrLn)
INDEX ON Style+cStyType+Po+cCrPTmpCod+STR(nLineNo,4)+cStatus TAG (lcTmpCrLn) OF (lcTmpCrLn)
INDEX ON Style+cStyType+Po+cCrPTmpCod+cStatus+STR(nLineNo,4) TAG (lcTmpCrLn1) OF (lcTmpCrLn)
SET ORDER TO TAG (lcTmpCrLn)
*-- end of lfCreatTmp.

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Function to collect data from STYCRPLN to tmp file.
*!*************************************************************
*
FUNCTION lfGetInfo

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
ZAP
IF SEEK(PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2],'STYCRPLN')
  SELECT STYCRPLN
  SCAN REST WHILE STYLE+CSTYTYPE+PO+CCRPTMPCOD+STR(NLINENO,4) = PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2]
    SCATTER MEMVAR MEMO
    m.nOldLineNo = m.nLineNo
    m.cStatus    = 'O'
    SELECT (lcTmpCrLn)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
ENDIF
SELECT (lcTmpCrLn)
LOCATE
=lfDispBrow()
SELECT (lnAlias)
*-- end of lfGetInfo.

*!*************************************************************
*! Name      : lfvStyle
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of Style field.
*!*************************************************************
*
FUNCTION lfvStyle

PRIVATE lnAlias
IF llBrowse .OR. LASTKEY()=13 .OR. LASTKEY()=9
  *--Ok start validate.
ELSE
  RETURN
ENDIF

lnAlias = SELECT(0)
IF llbrowse OR (!EMPTY(laData[1]) .AND. !SEEK(laData[1],'STYLE'))
  IF llbrowse OR ATC('?',laData[1]) <> 0
    llbrowse = .F.
    laData[1] = gfStyBrw('M',"","",.F.)
  ENDIF
  IF !SEEK(laData[1],'STYLE') .AND. !EMPTY(laData[1])
    laData[1] = gfStyBrw('M',"","",.F.)
  ENDIF
ENDIF
SELECT (lnAlias)
*-- end of lfvStyle.

*!*************************************************************
*! Name      : lfvCode
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of code field.
*!*************************************************************
*
FUNCTION lfvCode
PRIVATE lnAlias
IF llBrowse .OR. LASTKEY()=13 .OR. LASTKEY()=9
  *--Ok start validate.
ELSE
  RETURN
ENDIF

lnAlias = SELECT(0)
IF laScrMode[4]
  IF !EMPTY(laData[2]) .AND. SEEK(PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2],'STYCRPHD')
      =gfModalGen('INM00000B34000','','',.F.,;
                  'This style critical path already exists. Please enter another one.')
      STORE SPACE(0) TO laData[1] , laData[2], laData[4], laData[5]
      laData[4] = 'Sales Order'
      _CUROBJ = OBJNUM(laData[1])
      SHOW GET laData[1] ENABLE
      SHOW GET laData[2] ENABLE
      SHOW GET laData[4] ENABLE
      SHOW GET laData[5] ENABLE
  ELSE
    IF llbrowse .OR. !SEEK(laData[2],'CRPTMPHD')
      SELECT CRPTMPHD
      llbrowse = .F.
      DIMENSION laTempData[1]
      STORE '' TO laTempData
      lcKeyVal = "[]"
      lcFile_Ttl = 'Critical Path Template Code'
      lcBrFields = "cCrPTmpCod  :H='Code',"+;
                   "cCrPTmpDes  :H='Description'"
      =AriaBrow(&lcKeyVal,lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'cCrPTmpCod','laTempData')
      laData[2] = laTempData[1]
    ENDIF
    IF !EMPTY(laData[2])
      IF SEEK(PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2],'STYCRPHD')
        =gfModalGen('INM00000B34000','','',.F.,;
                    'This style critical path already exists. Please enter another one.')
        STORE SPACE(0) TO laData[1] , laData[2], laData[4], laData[5]
        laData[4] = 'Sales Order'
        _CUROBJ = OBJNUM(laData[1])
        SHOW GET laData[1] ENABLE
        SHOW GET laData[2] ENABLE
        SHOW GET laData[4] ENABLE
        SHOW GET laData[5] ENABLE
      ELSE
        IF EMPTY(laData[1])
          =gfModalGen('INM00000B34000','','',.F.,;
                      'Style cannot be empty.')
          _CUROBJ = OBJNUM(laData[1])
          SHOW GET laData[2] ENABLE
          SELECT (lnAlias)
          RETURN
        ENDIF
        IF EMPTY(laData[5])
          =gfModalGen('INM00000B34000','','',.F.,;
                      'Order cannot be empty.')
          _CUROBJ = OBJNUM(laData[5])
          SHOW GET laData[2] ENABLE
          SELECT (lnAlias)
          RETURN
        ENDIF
        SHOW GETS WINDOW POSTYCR1 DISABLE ONLY
        SHOW GET laData[6] ENABLE
        SHOW GET pbAdd     ENABLE
        SHOW GET pbNotes   ENABLE
        SHOW GET pbSav     ENABLE
        SHOW GET ibOrder   ENABLE
        laData[3] = CRPTMPHD.NLINENO
        =lfGetNew()
        GO BOTTOM IN (lcTmpCrLn)
        =lfAutRecal(laData[6],-1)
        =lfGetLink()
        =lfwBrows()
        IF RECCOUNT(lcTmpCrLn) > 0
          SHOW GETS WINDOW POSTYCR3 ENABLE ONLY
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ELSE
  IF llbrowse OR (!EMPTY(laData[2]) .AND. !SEEK(PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2],'STYCRPHD'))
    SELECT STYCRPHD
    llbrowse = .F.
    DIMENSION laTempData[4]
    STORE '' TO laTempData
    lcKeyVal = "[]"
    lcFile_Ttl = 'Style Critical Path'
    lcBrFields = "Style       :H='Style',"+;
                 "cCrPTmpCod  :H='Initial Template',"+;
                 "cStytype    :H='Type',"+;
                 "Po          :H='Order',"+;
                 "ShipDate    :H='Shipping Date'"
    =AriaBrow(&lcKeyVal,lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'Style,cStyType,Po,cCrPTmpCod','laTempData')
    laData[1] = laTempData[1]
    laData[4] = laTempData[2]
    laData[4] = IIF(laData[4]='S','Sales Order','Purchase Order')
    laData[5] = laTempData[3]
    laData[2] = laTempData[4]
  ENDIF
  IF !EMPTY(laData[2])
    laScrMode    = .F.
    laScrMode[2] = .T.
    =gfSeekRec()
    =lfGetInfo()
  ENDIF
ENDIF
SELECT (lnAlias)
*-- end of lfvCode.

*!*************************************************************
*! Name      : lfwIbBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
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
*! Name      : lfvAdd
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of Add button.
*!*************************************************************
*
FUNCTION lfvAdd

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
APPEND BLANK
laData[3] = laData[3] + 1
REPLACE NLINENO    WITH laData[3],;
        NOLDLINENO WITH NLINENO,;
        STYLE      WITH laData[1],;
        CSTYTYPE   WITH LEFT(laData[4],1),;
        PO         WITH laData[5],;
        CCRPTMPCOD WITH laData[2],;
        CSTATUS    WITH 'N',;
        CCRPTHGRP  WITH laGroup[1,2],;
        LAUTRECALC WITH .T.,;
        CADUSRSCHD WITH 'N'

=lfwBrows()
SHOW GETS WINDOW POSTYCR3 ENABLE ONLY
_CUROBJ = OBJNUM(m.cCrPthOpr)
SELECT (lnAlias)
*-- end of lfvAdd.

*!*************************************************************
*! Name      : lfvGroup
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of Group field.
*!*************************************************************
*
FUNCTION lfvGroup

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
m.cCrPthGrp = laGroup[lnGroup,2]
=lfChnStats('CCRPTHGRP')
REPLACE CCRPTHGRP  WITH m.cCrPthGrp
=lfwBrows()
SELECT (lnAlias)
*-- end of lfvGroup.

*!*************************************************************
*! Name      : lfvLink
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of Link to field.
*!*************************************************************
*
FUNCTION lfvLink

PRIVATE lnAlias,lnCurLink,lnOldLink
lnAlias = SELECT(0)

SELECT (lcTmpCrLn)
m.cLinkTo = laLink[lnLink,2]
=lfChnStats('CLINKTO')
IF m.cLinkTo<>CLINKTO .AND. !EMPTY(CLINKTO)
  lnCurLink = VAL(CLINKTO)
  lnOldLink = VAL(m.cLinkTo)
  *C037345,1 ABD - Add new element to the array for the Production Samples received. [Begin]
  *IF (lnCurLink>2 .AND. lnOldLink<3) .OR. (lnCurLink<3 .AND. lnOldLink>2)
  IF (lnCurLink>2 .AND. lnOldLink<3) .OR. (lnCurLink<3 .AND. lnOldLink>2) ;
    .OR. (lnCurLink=6 .AND. (lnOldLink>2 .AND. lnCurLink# 6))
    *C037345,1 ABD - [End]
    m.cPo = ''
  ENDIF
ENDIF

*C037345,1 ABD - Add new element to the array for the Production Samples received. [Begin]
*IF LEFT(laData[4],1) = 'S' .AND. m.cLinkTo $ '12' .AND. EMPTY(m.cPo)
*  IF SEEK('2'+laData[5],'CUTPICK') .AND. SEEK('P'+CUTPICK.CTKTNO,'POSHDR')
IF LEFT(laData[4],1) = 'S' .AND. m.cLinkTo $ '125' .AND. EMPTY(m.cPo)
  IF SEEK('2'+laData[5],'CUTPICK') .AND. SEEK('P'+CUTPICK.CTKTNO,'POSHDR')
  *C037345,1 ABD - [End]
    m.cPo = CUTPICK.CTKTNO
  ENDIF
ENDIF
IF EMPTY(m.cLinkTo)
  m.cPo = ''
ENDIF
REPLACE CLINKTO WITH m.cLinkTo ,;
        CPO     WITH m.cPo
=lfwBrows()
SELECT (lnAlias)
*-- end of lfvLink.

*!*************************************************************
*! Name      : lfvOpr
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of Operation field.
*!*************************************************************
*
FUNCTION lfvOpr

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
=lfChnStats('CCRPTHOPR')
REPLACE CCRPTHOPR  WITH m.cCrPthOpr
=lfwBrows()
SELECT (lnAlias)
*-- end of lfvOpr.

*!*************************************************************
*! Name      : lfvLdTime
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of Lead Time field.
*!*************************************************************
*
FUNCTION lfvLdTime

PRIVATE lnAlias, ldRequired
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
=lfChnStats('NLEADTIME')
REPLACE NLEADTIME WITH m.nLeadTime
=lfwBrows()
IF lcOldVal # m.nLeadTime .AND. LAUTRECALC .AND. EMPTY(m.dApproved) .AND.;
   IIF(laScrMode[3] .AND. CSTATUS#'N',m.nLineNo#laData[3],.T.)
  IF gfModalGen('INM00000B34001','','',.F.,;
                'Do you want to recalculate the Required Dates?')=1
    IF laScrMode[3] .AND. CSTATUS # 'N'
      ldRequired = DREQUIRED + NLEADTIME
      SKIP
      =lfAutRecal(ldRequired,1)
    ELSE
      LOCATE REST FOR LAUTRECALC .AND. EMPTY(DAPPROVED) .AND. NLINENO # m.nLineNo
      IF FOUND()
        ldRequired = DREQUIRED
        SKIP -1
      ELSE
        ldRequired = laData[6]
        GO BOTTOM IN (lcTmpCrLn)
      ENDIF
      =lfAutRecal(ldRequired,-1)
    ENDIF
    SEEK PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2]+STR(m.nLineNo,4)
    =lfwBrows()
  ENDIF
ENDIF
SELECT (lnAlias)
*-- end of lfvLdTime.

*!*************************************************************
*! Name      : lfvReq
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of Required field.
*!*************************************************************
*
FUNCTION lfvReq

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
=lfChnStats('DREQUIRED')
REPLACE DREQUIRED WITH m.dRequired
=lfwBrows()
IF IIF(laScrMode[3] .AND. CSTATUS#'N',NLINENO#laData[3],NLINENO#1) .AND. lcOldVal # m.dRequired .AND. LAUTRECALC
  IF gfModalGen('INM00000B34001','','',.F.,;
                'Do you want to recalculate the Required Dates?')=1
    lnSign = IIF(laScrMode[3] .AND. CSTATUS # 'N',1,-1)
    SKIP lnSign
    =lfAutRecal(m.dRequired+IIF(lnSign=1,m.NLEADTIME,0),lnSign)
    SKIP -lnSign
  ENDIF
ENDIF
SELECT (lnAlias)
*-- end of lfvReq.

*!*************************************************************
*! Name      : lfvApp
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of Approved field.
*!*************************************************************
*
FUNCTION lfvApp

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
=lfChnStats('DAPPROVED')
REPLACE DAPPROVED WITH m.dApproved
=lfwBrows()
SELECT (lnAlias)
*-- end of lfvApp.

*!*************************************************************
*! Name      : lfvCom
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of Comments field.
*!*************************************************************
*
FUNCTION lfvCom

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
=lfChnStats('CCOMMENTS')
REPLACE CCOMMENTS WITH m.cComments
=lfwBrows()
SELECT (lnAlias)
*-- end of lfvCom.

*!*************************************************************
*! Name      : lfvUser
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of User field.
*!*************************************************************
*
FUNCTION lfvUser

PRIVATE lnAlias
IF llBrowse .OR. LASTKEY()=13 .OR. LASTKEY()=9
  *--Ok start validate.
ELSE
  RETURN
ENDIF

lnAlias = SELECT(0)
IF llbrowse OR (!EMPTY(m.cUser_Id) .AND. !SEEK(m.cUser_Id,'SYUUSER'))
  SELECT SYUUSER
  llbrowse = .F.
  DIMENSION laTempData[1]
  STORE '' TO laTempData
  lcKeyVal = "[]"
  lcFile_Ttl = 'Users'
  lcBrFields = "cUser_Id  :H='User Id',"+;
               "cUsr_Name :H='User Name',"+;
               "cUsr_Loca :H='User Location',"+;
               "cUsr_Phon :H='Phone No.',"+;
               "cUsr_Levl :H='User Level'"
  =AriaBrow(&lcKeyVal,lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'cUser_Id','laTempData')
  m.cUser_Id = laTempData[1]
ENDIF
IF !EMPTY(m.cUser_Id)
  SELECT (lcTmpCrLn)
  =lfChnStats('CUSER_ID')
  REPLACE CUSER_ID WITH m.cUser_Id
  =lfwBrows()
ENDIF
SELECT (lnAlias)
*-- end of lfvUser.

*!*************************************************************
*! Name      : lfvAutRecl
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of Auto Recalc. field.
*!*************************************************************
*
FUNCTION lfvAutRecl

PRIVATE lnAlias, ldRequired
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
=lfChnStats('LAUTRECALC')
REPLACE LAUTRECALC WITH m.lAutRecalc
=lfwBrows()
IF lcOldVal # m.lAutRecalc .AND. EMPTY(m.dApproved) .AND. IIF(m.lAutRecalc,.T.,NLINENO#1)
  IF gfModalGen('INM00000B34001','','',.F.,;
                'Do you want to recalculate the Required Dates?')=1
    LOCATE REST FOR LAUTRECALC .AND. EMPTY(DAPPROVED) .AND. NLINENO # m.nLineNo
    IF FOUND()
      ldRequired = DREQUIRED
      SKIP -1
    ELSE
      ldRequired = laData[6]
      GO BOTTOM IN (lcTmpCrLn)
    ENDIF
    SEEK PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2]+STR(m.nLineNo,4)
    IF m.lAutRecalc
      =lfAutRecal(ldRequired,-1)
      =lfwBrows()
    ELSE
      SKIP -1
      =lfAutRecal(ldRequired,-1)
      SKIP
      =lfwBrows()
    ENDIF
  ENDIF
ENDIF

*C037345,1 ABD - In Case not check the Aut Calculate Add the system date to 
*C037345,1 ABD - Required date.  [Begin]
IF !m.lAutRecalc
    REPLACE dRequired WITH gdSysDate
    M.dRequired = gdSysDate
    SHOW GET M.dRequired
ENDIF
*C037345,1 ABD - [End]

SELECT (lnAlias)
*-- end of lfvAutRecl.

*!*************************************************************
*! Name      : lfChnStats
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : function to change status
*!*************************************************************
*
FUNCTION lfChnStats
PARAMETERS lcField

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
IF EVALUATE(lcField) # EVALUATE('m.'+lcField) .AND. CSTATUS = 'O'
  REPLACE CSTATUS WITH 'M'
ENDIF
SELECT (lnAlias)
*-- end of lfChnStats.

*!*************************************************************
*! Name      : lfvInsert
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of Insert button.
*!*************************************************************
*
FUNCTION lfvInsert

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
laData[3] = laData[3] + 1
=lfReNumber(1)
APPEND BLANK
REPLACE NLINENO    WITH m.nLineNo+1,;
        NOLDLINENO WITH NLINENO,;
        STYLE      WITH laData[1],;
        CSTYTYPE   WITH LEFT(laData[4],1),;
        PO         WITH laData[5],;
        CCRPTMPCOD WITH laData[2],;
        CSTATUS    WITH 'N',;
        CCRPTHGRP  WITH laGroup[1,2],;
        LAUTRECALC WITH .T.,;
        CADUSRSCHD WITH 'N'

=lfwBrows()
_CUROBJ = OBJNUM(m.cCrPthOpr)
SELECT (lnAlias)
*-- end of lfvInsert.

*!*************************************************************
*! Name      : lfvRemove
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of Remove button.
*!*************************************************************
*
FUNCTION lfvRemove

PRIVATE lnAlias,ldRequired
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
laData[3] = laData[3] - 1
REPLACE CSTATUS    WITH 'D'
IF NLINENO # 1 .AND. lAutRecalc .AND. EMPTY(m.dApproved)
  IF gfModalGen('INM00000B34001','','',.F.,;
                'Do you want to recalculate the Required Dates?')=1
    LOCATE REST FOR LAUTRECALC .AND. EMPTY(DAPPROVED) .AND. NLINENO # m.nLineNo
    IF FOUND()
      ldRequired = DREQUIRED
    ELSE
      ldRequired = laData[6]
    ENDIF
    SEEK PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2]+STR(m.nLineNo,4)
    SKIP -1
    =lfAutRecal(ldRequired,-1)
    SKIP
  ENDIF
ENDIF
DELETE
=lfReNumber(-1)
LOCATE
=lfwBrows()
_CUROBJ = OBJNUM(m.nLineNo)
IF EOF(lcTmpCrLn)
  SHOW GETS WINDOW POSTYCR3 DISABLE ONLY
  SHOW GET pbAdd     ENABLE
  _CUROBJ = OBJNUM(pbAdd)
ENDIF
SELECT (lnAlias)
*-- end of lfvRemove.

*!*************************************************************
*! Name      : lpSavScr
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : To Save record in StyCrPHd file & related records in StyCrPLn file.
*!*************************************************************
*!
PROCEDURE lpSavScr

PRIVATE lnAlias, lcDelSet
lnAlias = SELECT(0)

*-- Updating the header file.
SELECT STYCRPHD
IF laScrMode[4]
  APPEND BLANK
  laData[7] = lfCrNewKey()
ENDIF
GATHER FIELDS &lcScFields. FROM laData
=lfUpdUsrFl('CCRPTMPCOD',laData[2])
=lfUpdUsrFl('CSTYCRPKEY',laData[7])

*-- Updating the lines file.
SELECT (lcTmpCrLn)
SET ORDER TO TAG (lcTmpCrLn1)

*-- Updating deleted records.
lcDelSet = SET('DELETE')
SET DELETE OFF
=lfScan('D')
SET DELETE &lcDelSet.

*-- Updating modified records.
=lfScan('M')

*-- Updating new records.
=lfScan('N')

SELECT (lcTmpCrLn)
SET ORDER TO TAG (lcTmpCrLn)
SELECT (lnAlias)
*--end of lpSavScr.

*!*************************************************************
*! Name      : lfScan
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : function to update StyCrPLn file
*!*************************************************************
*
FUNCTION lfScan
PARAMETERS lcType

SEEK PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2]+lcType
SCAN REST WHILE STYLE+CSTYTYPE+PO+CCRPTMPCOD+CSTATUS+STR(NLINENO,4) = PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2]+lcType
  DO CASE
    CASE lcType = 'N'
      SCATTER MEMVAR MEMO
      SELECT STYCRPLN
      APPEND BLANK
      GATHER MEMVAR MEMO
      
      *C037345,1 ABD - In Case not check the Aut Calculate Add the system date to 
      *C037345,1 ABD - Required date.  [Begin]
      IF !M.LAUTRECALC
        REPLACE dRequired WITH gdSysDate
      ENDIF
      *C037345,1 ABD - [End]
      
    OTHERWISE
      IF SEEK(PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2]+STR(nOldLineNo,4),'STYCRPLN')
        IF lcType = 'M'
          SCATTER MEMVAR MEMO
          SELECT STYCRPLN
          GATHER MEMVAR MEMO

          *C037345,1 ABD - In Case not check the Aut Calculate Add the system date to 
          *C037345,1 ABD - Required date.  [Begin]
          IF !M.LAUTRECALC
            REPLACE dRequired WITH gdSysDate
          ENDIF
          *C037345,1 ABD - [End]
          
        ELSE
          SELECT STYCRPLN
          LOCATE REST WHILE STYLE+CSTYTYPE+PO+CCRPTMPCOD+STR(NLINENO,4) = ;
                            PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2]+STR(EVALUATE(lcTmpCrLn+'.nOldLineNo'),4);
                      FOR !DELETED()
          IF FOUND()
            DELETE
          ENDIF
        ENDIF
      ENDIF
  ENDCASE
ENDSCAN
*-- end of lfScan.

*!*************************************************************
*! Name      : lfRenumber
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : function to recalculate nlineno field when insert or remove line
*!*************************************************************
*
FUNCTION lfRenumber
PARAMETERS lnSign

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
IF lnSign = 1
  SET ORDER TO TAG (lcTmpCrLn) DESCENDING
  LOCATE
ELSE
  SEEK PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2]+STR(m.nlineno+1,4)
ENDIF
SCAN REST WHILE STYLE+CSTYTYPE+PO+CCRPTMPCOD+STR(NLINENO,4)+CSTATUS =;
                PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2];
          FOR IIF(lnSign=1,NLINENO>m.nLineNo,.T.)
  REPLACE NLINENO WITH NLINENO + lnSign
  IF CSTATUS = 'O'
    REPLACE CSTATUS WITH 'M'
  ENDIF
ENDSCAN
IF lnSign = 1
  SET ORDER TO TAG (lcTmpCrLn) ASCENDING
ENDIF
*-- end of lfRenumber.

*!**************************************************************************
*! Name      : lfUpdUsrFl
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/24/2002
*! Purpose   : Updating User defined fields.
*!**************************************************************************
*! Example   : =lfUpdUsrFl()
*!**************************************************************************
*
FUNCTION lfUpdUsrFl
PARAMETERS lcFldName,lcFldValue

lnFldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,lcFldName),1)
laUsrFields[lnFldPos,6] = lcFldValue
*-- end of lfUpdUsrFl.

*!*************************************************************
*! Name      : lpDelScr
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : To Delete record in StyCrPHd file and related records in StyCrPLn file.
*!*************************************************************
*!
PROCEDURE lpDelScr

PRIVATE lnAlias
lnAlias = SELECT(0)

*-- Delete this record from the header file.
IF SEEK(PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2],'STYCRPHD')
  SELECT STYCRPHD
  =gfObj_lock(.F.)
  DELETE
  laScrMode    = .F.
  laScrMode[1] = .T.
ENDIF

*-- Delete all lines.
SELECT (lcTmpCrLn)
REPLACE ALL CSTATUS WITH 'D'
SET ORDER TO TAG (lcTmpCrLn1)
=lfScan('D')
SELECT (lcTmpCrLn)
SET ORDER TO TAG (lcTmpCrLn)
SELECT (lnAlias)
*--end of lpDelScr.

*!*************************************************************
*! Name      : lfGetNew
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Function to collect new data from CRPTMPLN to tmp file.
*!*************************************************************
*
FUNCTION lfGetNew

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
ZAP
IF SEEK(laData[2],'CRPTMPLN')
  SELECT CRPTMPLN
  SCAN REST WHILE CCRPTMPCOD+STR(NLINENO,4) = laData[2]
    SCATTER MEMVAR MEMO
    m.Style      = laData[1]
    m.cStyType   = LEFT(laData[4],1)
    m.Po         = laData[5]
    m.nOldLineNo = m.nLineNo
    m.cStatus    = 'N'
    m.cAdUsrSchd = 'N'
    m.cPo = ''
    m.dApproved = {}
    SELECT (lcTmpCrLn)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
ENDIF
SELECT (lcTmpCrLn)
LOCATE
=lfDispBrow()
SELECT (lnAlias)
*-- end of lfGetNew.

*!*************************************************************
*! Name      : lfvOrder
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of Order field.
*!*************************************************************
*
FUNCTION lfvOrder
PRIVATE lnAlias,lcAType,lcFile,lcOrder
lnAlias = SELECT(0)
lcAType = LEFT(laData[4],1)
lcAType = IIF(lcAType='S','O',lcAType)
lcFile = IIF(lcAType='O','ORDHDR','POSHDR')
IF llBrowse OR ( !EMPTY(laData[5]) AND !SEEK(lcAType+laData[5],lcFile) )
  lcOrder=laData[5]
  SELECT (lcFile)
  =lfOrdBrow(@lcOrder,'','','')
  laData[5]=lcOrder
  llBrowse = .F.
ENDIF
IF !EMPTY(laData[5])
  IF lcAType = 'P'
    laData[6] = POSHDR.COMPLETE
  ELSE
    laData[6] = ORDHDR.START
  ENDIF
  SHOW GET laData[6]
ENDIF
SELECT (lnAlias)
*-- end of lfvOrder.

*!*************************************************************
*! Name      : lfvcPo
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of cPo field.
*!*************************************************************
*
FUNCTION lfvcPo

IF lnLink = 1
  RETURN
ENDIF
PRIVATE lnAlias,lcAType,lcFile,lcOrder
lnAlias = SELECT(0)

*C037345,1 ABD - browse from the POSHDR not from he POFhdr.[Begin]
*lcAType = IIF(lnLink>3,'F','P')
lcAType = IIF(lnLink>3,IIF(lnLink=6,'P','F'),'P')
*C037345,1 ABD - [End]

lcFile = IIF(lcAType='F','POFHDR','POSHDR')
IF llBrowse OR ( !EMPTY(m.cPo) AND !SEEK('P'+m.cPo,lcFile) )
  lcOrder=m.cPo
  SELECT (lcFile)
  =lfOrdBrow(@lcOrder,'','','')
  m.cPo=lcOrder
  llBrowse = .F.
ENDIF
=lfChnStats('CPO')
SELECT (lcTmpCrLn)
REPLACE CPO WITH m.cPo

*C200413,1 AMH Update the approved date for create MPO [Start]
IF !EMPTY(m.cPo) .AND. lnLink = 4
  REPLACE DAPPROVED WITH gdSysDate
ENDIF
*C200413,1 AMH [End]

=lfwBrows()
SELECT (lnAlias)
*-- end of lfvcPo.

*!*************************************************************
*! Name      : lfOrdBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Browse sales order
*!*************************************************************
FUNCTION lfOrdBrow
PARAMETERS lcOrder,lcAccount,lcStore,lcCustPo

DO CASE
  CASE lcAType = 'P'
    SELECT POSHDR
    SET RELATION TO VENDOR INTO APVENDOR ADDITIVE
    SELECT POSLN
    SET RELATION TO 'P'+PO INTO POSHDR ADDITIVE
    
    lcBrFields = [POSHDR.PO         :R :H='PO #':8,]+;
                 [POSHDR.Status     :R :H='S':2,]+;
                 [POSHDR.Vendor     :R :H='Vendor':11,]+;
                 [ApVendor.cVenComp :R :H='Name':18,]+;
                 [POSHDR.Complete   :R :H='Complete':8,]+;
                 [POSHDR.Entered    :R :H='Entered':8,]+;
                 [POSHDR.nStyOrder  :R :H='Tot.Qty.':7,]+;
                 [POSHDR.POTotal    :R :H='Amount':10,]+;
                 [POSHDR.Receive    :R :H='Receive':7,]+;
                 [POSHDR.Open       :R :H='Open':7]
    lcOrder = IIF(ARIABROW("PADR(laData[1],lnstylewid) FOR !EOF('POSHDR')","Po Orders",gnBrFSRow1,;
                           gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','PO','laBrowArr'),POSHDR.PO,SPACE(6))
    SET RELATION OFF INTO POSHDR
    SELECT POSHDR
    SET RELATION OFF INTO APVENDOR
  CASE lcAType = 'F'
    SELECT POFHDR
    SET RELATION TO VENDOR INTO APVENDOR ADDITIVE
    
    *C200413,1 AMH Browse all MPOs [Start]
    *IF LEFT(laData[4],1) = 'S'
    *  SELECT MAPOALO
    *  SET RELATION TO 'P'+POMAT INTO POFHDR ADDITIVE
    *ENDIF
    *C200413,1 AMH [End]
    
    lcBrFields = [POFHDR.POMAT      :R :H='PO #':8,]+;
                 [POFHDR.Status     :R :H='S':2,]+;
                 [POFHDR.Vendor     :R :H='Vendor':11,]+;
                 [ApVendor.cVenComp :R :H='Name':18,]+;
                 [POFHDR.Complete   :R :H='Complete':8,]+;
                 [POFHDR.Entered    :R :H='Entered':8,]+;
                 [POFHDR.nFabOrder  :R :H='Tot.Qty.':7,]+;
                 [POFHDR.POTotal    :R :H='Amount':10,]+;
                 [POFHDR.NFBReceive :R :H='Receive':7,]+;
                 [POFHDR.NPO_Open   :R :H='Open':7]
    
    *C200413,1 AMH Browse all MPOs [Start]
    *lcOrder = IIF(ARIABROW(IIF(LEFT(laData[4],1)='S',"laData[5] FOR !EOF('POFHDR')",;
                               "FOR POFHDR.STATUS='O'"),"Material Po Orders",gnBrFSRow1,;
                           gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','PO','laBrowArr'),POFHDR.POMAT,SPACE(6))
    *IF LEFT(laData[4],1) = 'S'
    *  SET RELATION OFF INTO POFHDR
    *ENDIF
    lcOrder = IIF(ARIABROW("FOR POFHDR.STATUS='O'","Material Po Orders",gnBrFSRow1,;
                           gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','PO','laBrowArr'),POFHDR.POMAT,SPACE(6))
    *C200413,1 AMH [End]
    
    SELECT POFHDR
    SET RELATION OFF INTO APVENDOR
  CASE lcAType = 'O'
    SELECT ORDHDR
    SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER ADDITIVE
    SELECT ORDLINE
    SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
    
    lcBrFields = [ORDHDR.Order:H="Order#",ORDHDR.Status:1:H="Status",ORDHDR.Account:H="Acct",]+;
                 [Store=IIF(ORDHDR.MULTI='Y','*Multi*',ORDHDR.STORE):H="Store",]+;
                 [Customer.stname:30:H="Name",]
    lcBrFields = lcBrFields + [ORDHDR.Open:H="Open.Qty.",ORDHDR.OpenAmt:H="Open.Amt.",]+;
                 [ORDHDR.Ship:H="Ship.Qty.",ORDHDR.ShipAmt:H="Ship.Amt.",]+;
                 [ORDHDR.Book:H="Book.Qty.",ORDHDR.BookAmt:H="Book.Amt.",]+;
                 [ORDHDR.Cancel:H="Cancel.Qty.",ORDHDR.CancelAmt:H="Cancel.Amt."]
    lcBrFields = lcBrFields + [,ORDHDR.Entered:H="Entered",ORDHDR.Start:H="Start",ORDHDR.Complete:H="Complete",]+;
                 [ORDHDR.Cancelled:H="Cancelled",]+;
                 [ORDHDR.cWareCode:H="Warehouse",]+;
                 [lcSesDesc=gfCodDes(ORDHDR.Season,'SEASON'):H="Season",]+;
                 [lcDivDesc=gfCodDes(ORDHDR.cDivision,'CDIVISION'):H="Division",]+;
                 [lcShipVia=gfCodDes(ORDHDR.ShipVia,'SHIPVIA'):H="ShipVia"]
    lcOrder = IIF(ARIABROW("PADR(laData[1],lnstylewid) FOR !EOF('ORDHDR')","Orders",gnBrFSRow1,;
                           gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),OrdHdr.Order,SPACE(6))
    SET RELATION OFF INTO ORDHDR
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER
ENDCASE
*-- end of lfOrdBrow.

*!*************************************************************
*! Name      : lfvShip
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Valid function of Shipdate field.
*!*************************************************************
*
FUNCTION lfvShip

IF lcOldVal # laData[6] .AND. gfModalGen('INM00000B34001','','',.F.,;
              'Do you want to recalculate the Required Dates?')=1
  GO BOTTOM IN (lcTmpCrLn)
  =lfAutRecal(laData[6],-1)
ENDIF
*-- end of lfvShip.

*!*************************************************************
*! Name      : lfAutRecal
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Auto recalculate the required date.
*!*************************************************************
*
FUNCTION lfAutRecal
PARAMETERS ldReqDate,lnSign

PRIVATE lnAlias, lnLineNo
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
lnLineNo = NLINENO
IF lnSign = -1
  SET ORDER TO TAG (lcTmpCrLn) DESCENDING
ENDIF
SCAN REST
  IF LAUTRECALC
    IF EMPTY(DAPPROVED) .AND. lnSign = 1
      REPLACE DREQUIRED WITH ldReqDate
      IF NLINENO = laData[3] .AND. DREQUIRED > laData[6]
        =gfModalGen('INM00000B34000','','',.F.,'Please be aware that this amendment has made '+;
                    'the last operation greater than shipping date.')
      ENDIF
    ENDIF
    ldReqDate = ldReqDate + (nLeadTime * lnSign)
    IF EMPTY(DAPPROVED) .AND. lnSign = -1
      REPLACE DREQUIRED WITH ldReqDate
    ENDIF
  ENDIF
ENDSCAN
SET ORDER TO TAG (lcTmpCrLn) ASCENDING
SEEK PADR(laData[1],19)+LEFT(laData[4],1)+laData[5]+laData[2]+STR(lnLineNo,4)
SELECT (lnAlias)
*-- end of lfAutRecal.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : Save the current value of the current field.
*!*************************************************************
*
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))
*-- end of lfwOldVal.

*!*************************************************************
*!  Function : lfvObjLnk
*! Developer : AHMED MAHER
*! Date      : 07/24/2002
*! Purpose   : Object link.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvObjLnk

PRIVATE lnAlias
lnAlias = SELECT(0)
DO GetObj WITH 'K',laData[7]
SELECT(lnalias)
RETURN
*-- end of lfvObjLnk.

*!*************************************************************
*! Name      : lfvNotes
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : valid function of notes button.
*!*************************************************************
*
FUNCTION lfvNotes

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
DO (gcScrDir+"ARLNOTES.SPX")
IF CSTATUS = 'O'
  REPLACE CSTATUS WITH 'M'
ENDIF
SELECT(lnalias)
RETURN
*-- end of lfvNotes.

*!*************************************************************
*! Name      : lfCrNewKey
*! Developer : AMH (AHMED MAHER)
*! Date      : 07/24/2002
*! Purpose   : function to create new key.
*!*************************************************************
*
FUNCTION lfCrNewKey

PRIVATE lnAlias,lcOrder,lcNewKey
lcNewKey = REPLICATE('0',20)
lnAlias = SELECT(0)
SELECT STYCRPHD
lcOrder = SET('ORDER')
SET ORDER TO TAG CSTYCRPKEY
LOCATE
lcNewKey = IIF(EMPTY(CSTYCRPKEY),lcNewKey,CSTYCRPKEY)
lcNewKey = PADL(ALLTRIM(STR(VAL(lcNewKey)+1,20,0)),20,'0')
SET ORDER TO &lcOrder.
LOCATE
SELECT(lnalias)
RETURN lcNewKey
*-- end of lfCrNewKey.

*!*************************************************************
*! Name      : lfGetLink
*! Developer : AMH (AHMED MAHER)
*! Date      : 08/14/2002
*! Purpose   : Function to update the link to and approved date.
*!*************************************************************
*
FUNCTION lfGetLink
PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCrLn)
SCAN
  *C037345,1 ABD - [Begin]
  *IF cStyType = 'S' .AND. cLinkTo $ '12' .AND. SEEK('2'+laData[5],'CUTPICK');
  *   .AND. SEEK('P'+CUTPICK.CTKTNO,'POSHDR')
  *  REPLACE cPo       WITH CUTPICK.CTKTNO,;
            dApproved WITH gdSysDate

  IF cStyType = 'S' .AND. cLinkTo $ '125' .AND. SEEK('2'+laData[5],'CUTPICK');
     .AND. SEEK('P'+CUTPICK.CTKTNO,'POSHDR')
    REPLACE cPo       WITH CUTPICK.CTKTNO
    IF cLinkTo $ '1'
      REPLACE dApproved WITH gdSysDate
    ENDIF
    *C037345,1 ABD - [End]            
  ENDIF
ENDSCAN
SELECT (lnAlias)
*-- end of lfGetLink.