***************************************************************************************
*: Procedure file: ICHLDLSTL.PRG (Descriptive list entry screen for Cathy Danyiels)
*:         System: ARIA 2.7
*:         Module: Inventory Control
*:         Author: Adel Mohammed El Gazzar (ADEL)
*:  Date         : 04/19/2001
*: Refer to      : (C#102271)
*:****************************************************************************************
*:Modifications :
*:B804218,1 ABD 05/22/2001 Fix bug that When the user enters a new style in the detail 
*:B804218,1 ABD            Screen the program should pick the sales description from 
*:B804218,1 ABD            The style file and default it in the
*:B804218,1 ABD            Description field.
*:B804248,1 WAB 06/12/2001 Fix the bug of when enter the style manually ( without selecting 
*:B804248,1 WAB            from the browse), the screen doesn't default the sales description
*:B804248,1 WAB            from the style file.
*:B604828,1 TMI 08/26/2001 Showing a message when no styles matching the selected season and group instead of browsing an empty browse.
*:B605157,1 BWA 11/26/2001 Fix the bug of the "New" button is disabled in case the user add the style manually
*:****************************************************************************************
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DIMENSION laKeyField[1,4],laFileStru[1],laRecNo[1,17],laDelRec[1,17],;
          laTmpRec[1,4],laWndObj[3,3]

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'CDLHLSID'
laKeyField[1,4] = 1

laDefProc[7] = .F.          && Using my own save function
laDefProc[9] = .F.          && Using my own delete function

STORE SPACE(1)  TO puTmpLine,lcDtlTmp,lcDetStat,laRecNo,laDelRec,laTmpRec,laTmpRec[1,4]
STORE SPACE(3)  TO lcOldSea,lcOldClr,laTmpRec[1,2]
STORE SPACE(2)  TO lcOldGrp
STORE SPACE(7)  TO lcOldSty,laTmpRec[1,1],laTmpRec[1,1]
STORE .F.       TO llBrowse,llNewRec,llDeleted,laRecNo[1,14],laDelRec[1,14],;
                   laRecNo[1,14],laDelRec[1,14]
STORE .T.       TO llFrstRec
STORE {}        TO laRecNo[1,9],laRecNo[1,11],laDelRec[1,9],laDelRec[1,11]
STORE 0         TO laRecNo[1,6],laRecNo[1,15],laDelRec[1,6],laDelRec[1,15],;
                   laTmpRec[1,3],lnDtlCont,lsLine,lnFldCont,lnCurrRec
STORE 'Descriptive list details' TO lcChldTtl
STORE '' TO lcWinHdr,lcWinDtl,lcWinKey,lcWinBrow,lcMjrTtl,lcNMjrTl,lcWinBut,lcNMjrPt
STORE SPACE(15) TO lcGrpDesc
lcBrowTtl = "Descriptive list details"
*-- lcfolder   variable that hold the temp name of the folder window
*-- lnLastFold  variable that hold the last activated folder befor changing
*-- lafoldwinds array that hold the count of used folder and it's titles
DIMENSION lafoldwinds[2,2]
STORE SPACE(0) TO lcfolder,lcMajTtl,lcNonMajT,lcMjrPct
STORE 0 TO lnLastFold,lnStyleLen,lnStyStPos,lnClrPo,lnColorLen,lnSeasLen,lnactfolder
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'Ccode_no','SH')
=gfOpenFile(gcDataDir+'CATHCODE',gcDataDir+'Crltfield','SH')
=lfEvalSegs()
llEmptyRec = .F.
lcMjrTtl  = gfItemMask('HM')
lcNMjrTl  = gfItemMask('HN')
lcMjrPct  = gfItemMask('PM')
lcNMjrPt  = gfItemMask('PN')
lcIMjrPt  = gfItemMask('PI')
lnstylewid=LEN(lcMjrPct)
lncolorwid=LEN(lcNMjrPt)
lcSepart  =SUBSTR(lcIMjrPt,lnstylewid+1,1)
laWndObj[1,1] = gcBaseWind
laWndObj[1,2] = "LADATA[1]"
laWndObj[1,3] = "PBDETAIL"

laWndObj[2,1] = "GWCCONTRL1"
laWndObj[2,2] = "PBTOP"
laWndObj[2,3] = "PBCLS"

laWndObj[3,1] = "lcWinHdr"
laWndObj[3,2] = "LSLINE"
laWndObj[3,3] = "PBCLOSE"
lcScFields = " "

IF !gfSetup()
  RETURN
ENDIF
lnFolderCEnd = 102.40
lnFolderREnd = 1.9
lnNoFld      = 2
lcwfoldchng  = '=lfActFolder()'  && function to control shows after change the folder
llNoShow = .F.

IF !WEXIST(gcBaseWind)
  lcScFields = "CDLHLSTID,DDLHDLVDT,DDLHRVSDT,SEASON,CDLHSSDSC,CDLHSLDSC,CDLHGROUP,;
                MDLHNOTES,CDLHS2DSC"
  lnFldCont = FCOUNT('HLDSLSDT')
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  lcDtlTmp = gfTempName()
  SELECT HLDSLSDT
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+7,4]

  laFileStru[lnFileStru+1,1] = 'NRECNO'
  laFileStru[lnFileStru+1,2] = 'N'
  laFileStru[lnFileStru+1,3] = 7
  laFileStru[lnFileStru+1,4] = 0

  laFileStru[lnFileStru+2,1] = 'CSTATUS'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 1
  laFileStru[lnFileStru+2,4] = 0

  laFileStru[lnFileStru+3,1] = 'SEASON'
  laFileStru[lnFileStru+3,2] = 'C'
  laFileStru[lnFileStru+3,3] = 3
  laFileStru[lnFileStru+3,4] = 0

  laFileStru[lnFileStru+4,1] = 'CSTYLE'
  laFileStru[lnFileStru+4,2] = 'C'
  laFileStru[lnFileStru+4,3] = 12
  laFileStru[lnFileStru+4,4] = 0

  laFileStru[lnFileStru+5,1] = 'COLOR'
  laFileStru[lnFileStru+5,2] = 'C'
  laFileStru[lnFileStru+5,3] = 6
  laFileStru[lnFileStru+5,4] = 0

  laFileStru[lnFileStru+6,1] = 'CSTYDESC'
  laFileStru[lnFileStru+6,2] = 'C'
  laFileStru[lnFileStru+6,3] = 20
  laFileStru[lnFileStru+6,4] = 0
  
  laFileStru[lnFileStru+7,1] = 'cArr'
  laFileStru[lnFileStru+7,2] = 'C'
  laFileStru[lnFileStru+7,3] = 3
  laFileStru[lnFileStru+7,4] = 0

  *--Generate temp names for screens
  lcWinHdr  =  gfTempName()
  lcWinDtl  =  gfTempName()
  lcWinKey  =  gfTempName()
  lcWinBrow =  gfTempName()
  lcWinBut  =  gfTempName()
  lafoldwinds[1,1] = 'Header'
  lafoldwinds[1,2] = lcWinHdr
  lafoldwinds[2,1] = 'Details'
  lafoldwinds[2,2] = lcWinDtl
  lcfolder    = gfTempName()        && Folder Window Name
  lcfoldprnt  = gcBaseWind          && window parent name for the folder
  STORE 1 TO lnActFolder,lnLastFold && active folder
  CREATE TABLE (gcWorkDir+lcDtlTmp) FROM ARRAY laFileStru
  INDEX ON cdldlstid+cArr TAG (lcDtlTmp)
  DEFINE POPUP puTmpLine MOVER scroll
ENDIF
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowTtl)
lcDetStat = IIF((laScrMode[2] .OR. laScrMode[3]) .OR. ;
                (laScrMode[4] .AND. !EMPTY(laData[4]);
                              .AND. !EMPTY(laData[7])),"ENABLE","DISABLE")
SELECT HLDSLSHD
DO (gcScrDir+gcWinAppl+"\HLDSLST.SPX")

IF glQuitting
  RELEASE POPUP puTmpLine
  
  IF USED(lcDtlTmp)
    USE IN (lcDtlTmp)
  ENDIF
  
  ERASE (gcWorkDir+lcDtlTmp+'.DBF')
  ERASE (gcWorkDir+lcDtlTmp+'.CDX')
  ERASE (gcWorkDir+lcDtlTmp+'.FPT')
ENDIF
 
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

DO CASE
  CASE laScrMode[1]      && SELECT
    SELECT(lcDtlTmp)
    ZAP
    STORE SPACE(01) TO laTmpRec[1,4]
    STORE SPACE(03) TO laTmpRec[1,2]
    STORE SPACE(07) TO laTmpRec[1,1]
    STORE SPACE(15) TO lcGrpDesc
    STORE .T.       TO llFrstRec
    STORE 0         TO laTmpRec[1,3]
    SHOW GET ibFolder[2] DISABLE
    SHOW GET laData[1] ENABLE
    SHOW GET ibDescId ENABLE
    _CUROBJ = OBJNUM(laData[1])
    IF lnactfolder <> 1
      lnlastfold = lnactfolder
      lnactfolder=1
      =lfchngfolder(lnactfolder)
    ENDIF
    SELECT HLDSLSHD    
  CASE laScrMode[2] .OR. laScrMode[3]     && VIEW or EDIT
    SELECT(lcDtlTmp)
    ZAP
    IF SEEK(laData[1],'HLDSLSDT')
      SELECT HLDSLSDT
      SCAN REST WHILE cdldlstid = laData[1]
        SCATTER MEMVAR MEMO
        SELECT (lcDtlTmp)
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE nRecNo   WITH RECNO('HLDSLSDT'),;
                cStatus  WITH 'S',;
                cStyle   WITH SUBSTR(HLDSLSDT.Style,1,lnSeasLen+lnStyleLen),;
                Color    WITH SUBSTR(HLDSLSDT.Style,lnClrPo,lnColorLen),;
                cstydesc WITH IIF(SEEK(STYLE,'STYLE'),STYLE.DESC,SPACE(20)),;
                Season   WITH SUBSTR(HLDSLSDT.Style,1,lnSeasLen) 
      ENDSCAN
    ENDIF
    IF laScrMode[3]
      IF lnActFolder = 2
        SHOW GET laTmpRec[1,1] ENABLE
        SHOW GET laTmpRec[1,2] ENABLE
        SHOW GET laTmpRec[1,3] ENABLE
        SHOW GET laTmpRec[1,4] ENABLE
        SHOW GET pbNew         ENABLE
        SHOW GET pbRemove      ENABLE
        SHOW GET pbZom         ENABLE
      ELSE
        SHOW GET laTmpRec[1,1] DISABLE
        SHOW GET laTmpRec[1,2] DISABLE
        SHOW GET laTmpRec[1,3] DISABLE
        SHOW GET laTmpRec[1,4] DISABLE
        SHOW GET pbNew         DISABLE
        SHOW GET pbRemove      DISABLE
        SHOW GET pbZom         DISABLE
      ENDIF  
    ELSE
      laTmpRec[1,1] = laRecNo[1,4]
      laTmpRec[1,2] = laRecNo[1,5]
      laTmpRec[1,3] = laRecNo[1,6]
      laTmpRec[1,4] = laRecNo[1,7]
      SHOW GET laTmpRec[1,1] DISABLE
      SHOW GET laTmpRec[1,2] DISABLE
      SHOW GET laTmpRec[1,3] DISABLE
      SHOW GET laTmpRec[1,4] DISABLE
      SHOW GET pbNew         DISABLE
      SHOW GET pbRemove      DISABLE
      SHOW GET pbZom         DISABLE
    ENDIF
    SHOW GET ibSeason  DISABLE
    SHOW GET ibGroup   DISABLE
    SHOW GET laData[4] DISABLE
    SHOW GET laData[7] DISABLE
    SHOW GET laData[1] DISABLE    
    SHOW GET ibDescId  DISABLE
    SHOW GET ibFolder[2] ENABLE
    =lfChngFolder(lnActFolder)
    SELECT HLDSLSHD
  CASE laScrMode[4]      && ADD
    SELECT(lcDtlTmp)
    ZAP
    STORE 0  TO lsLine
    SHOW GET pbRemove DISABLE
    _CUROBJ = OBJNUM(laData[2])
    SHOW GET laData[1] DISABLE
    SHOW GET ibDescId  DISABLE    
    SHOW GET pbZom DISABLE
ENDCASE
SHOW GET pbUsrField DISABLE
SHOW GET ibLastObj  DISABLE
SELECT HLDSLSHD

*!*************************************************************
*! Name      : lfvData_1
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Vlaidate Descriptive id.
*!*************************************************************
*! Calls     : lfBrowDet
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvData_1()
*!*************************************************************
FUNCTION lfvData_1

SELECT HLDSLSHD
IF !EMPTY(laData[1])
  =gfSeekRec()
  lcGrpDesc = IIF(SEEK('N'+'CSTYGROUP '+laData[7],'CODES'),Codes.CdiscRep,'')
  =lfRefresh()
ENDIF
IF laScrMode[4]
  SHOW GET ladata[1] DISABLE
  SHOW GET ibDescId  DISABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfwActBrow
*!
*!**************************************************************************
*
FUNCTION lfwActBrow

IF !MDOWN()
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvActBrow
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Vlaidate Descriptive id.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvActBrow()
*!*************************************************************
FUNCTION lfvActBrow
PARAMETER lcObjName

_CUROBJ  = OBJNUM(lcObjName)
llBrowse = .T.
KEYBOARD "{ENTER}"

*!*************************************************************
*! Name      : lfSaveOld
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Save old values.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfSaveOld()
*!*************************************************************
FUNCTION lfSaveOld
PARAMETER lcType,lcOldObj

IF lcType $ 'TC' .AND. EMPTY(lcOldObj)
  KEYBOARD " "
  KEYBOARD "{BACKSPACE}"
ENDIF

DO CASE
  CASE lcType='S'
    lcOldSea = lcOldObj
  CASE lcType='G'
    lcOldGrp = lcOldObj
  CASE lcType='T'
    lcOldSty = lcOldObj
  CASE lcType='C'
    lcOldClr = lcOldObj
ENDCASE

*!*************************************************************
*! Name      : lfvSeaGrp
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Season/Group validation.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvSeaGrp()
*!*************************************************************
*
FUNCTION lfvSeaGrp
PARAMETER lcType,lnObjNum

lcOldAlias = ALIAS()
SELECT Codes
IF lcType  = 'Season'
  lcKey    = 'N'+'SEASON    '
  lnStrLen = 3
ELSE
  lcKey    = 'N'+'CSTYGROUP '
  lnStrLen = 2
ENDIF

IF llBrowse .OR. !SEEK(lcKey+laData[lnObjNum])
  llBrowse = .F.
  lcOldBrFld = lcBrFields
  lcOldTitle = lcFile_Ttl
  lcBrFields = "cCode_No :H= '&lcType Code',;
                cDiscRep :H= '&lcType Description'"
  lcFile_Ttl = lcType + 's'
  SET ORDER TO TAG Codes IN Codes
  =SEEK('N')
  lcRetrn = IIF(ARIABROW(['N' FOR crltfield+cfld_name+ccode_no+crltd_nam = lcKey],;
                  IIF(lcType='Season','Season','Groups'),gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','cCode_No','laBrowArr'),;
                  CODES.cCode_No,SPACE(6))
  SET ORDER TO TAG cCode_No IN Codes
  lcBrFields = lcOldBrFld
  lcFile_Ttl = lcOldTitle
  IF !EMPTY(lcRetrn)
    laData[lnObjNum] = lcRetrn
    IF lcType = 'Season'
      laData[5] = Codes.CdiscRep
      *--Get the 2 related long names.
      IF SEEK('Y'+'SEASON    '+laData[lnObjNum],'CATHCODE')
        SELECT CATHCODE
        laData[6] = CATHCODE.CRLTD_VLU
        LOCATE REST WHILE crltfield+cfld_name+ccode_no+crltd_nam = 'Y'+'SEASON    '+laData[lnObjNum]
        laData[9] = CATHCODE.CRLTD_VLU
      ENDIF
      SHOW GET laData[6] ENABLE
      SHOW GET laData[9] ENABLE
      SHOW GET laData[5] ENABLE
      _CUROBJ = OBJNUM(laData[5])
    ELSE
      lcGrpDesc = Codes.CdiscRep
      _CUROBJ = OBJNUM(laData[8])
    ENDIF
  ELSE
    laData[lnObjNum] = IIF(lcType = 'Season',lcOldSea,lcOldGrp)
    _CUROBJ = _CUROBJ
  ENDIF
ELSE
  IF !llBrowse 
    IF lcType = 'Season'
      LOCATE REST WHILE crltfield+cfld_name+ccode_no+crltd_nam = 'Y'+'SEASON    '+laData[lnObjNum];
                  FOR !EMPTY(Codes.CdiscRep)
    ENDIF              
    laData[lnObjNum] = CODES.cCode_No
    IF lcType = 'Season'
      laData[5] = Codes.CdiscRep
      IF SEEK('Y'+'SEASON    '+laData[lnObjNum],'CATHCODE')
        SELECT CATHCODE
        laData[6] = CATHCODE.CRLTD_VLU
        LOCATE REST WHILE crltfield+cfld_name+ccode_no+crltd_nam = 'Y'+'SEASON    '+laData[lnObjNum]
        laData[9] = CATHCODE.CRLTD_VLU
      ENDIF
      SHOW GET laData[6] ENABLE
      SHOW GET laData[9] ENABLE
      SHOW GET ibSeason  DISABLE
      SHOW GET laData[4] DISABLE
    ELSE
      lcGrpDesc = Codes.CdiscRep
      SHOW GET lcGrpDesc DISABLE
      SHOW GET ibGroup   DISABLE
      SHOW GET laData[7] DISABLE
    ENDIF
  ENDIF
ENDIF

IF laScrMode[4] AND (EMPTY(laData[4]) OR EMPTY(laData[7]))
  SHOW GET ibFolder[2] DISABLE
ELSE
  SHOW GET ibFolder[2] ENABLE
ENDIF
=lfRefresh()
IF !EMPTY(lcOldAlias)
  SELECT(lcOldAlias)
ENDIF 

*!**************************************************************************
*!
*!      Function: lfwData_5
*!
*!**************************************************************************
*
FUNCTION lfwData_5

SHOW GET ibSeason  DISABLE
SHOW GET laData[4] DISABLE

*!**************************************************************************
*!
*!      Function: lfwData_8
*!
*!**************************************************************************
*
FUNCTION lfwData_8
IF !EMPTY(laData[7])
  SHOW GET ibGroup   DISABLE
  SHOW GET laData[7] DISABLE
ENDIF  

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Save Descriptive id.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpSavScr
*!*************************************************************
** This procedure will be used in this screen instead of the standard
*  SAVE button.
*
PROCEDURE lpSavScr

SELECT(lcDtlTmp)
LOCATE FOR (!EMPTY(STYLE) AND ndldtlunt <> 0)
llAnyRec = FOUND()
LOCATE
IF EOF() OR !llAnyRec 
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"You cannot save without editing the details.")
  llcSave = .F.
  RETURN
ENDIF
llEmptyRec = .F.
DELETE ALL FOR cStatus = 'D'
lnCount = 1
SCAN 
  IF nRecNo = 0 .AND. cStatus = 'S'
    LOOP
  ENDIF
  REPLACE cdldLinNo WITH ALLTRIM(STR(lnCount)),;
          cStatus   WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)
          IF ndldtlunt = 0
            REPLACE cStatus WITH 'S'
          ENDIF
  lnCount = lnCount + 1
ENDSCAN
=gfTmp2Mast('HLDSLSDT',lcDtlTmp,'Saving descriptive list lines. Please wait.')
SELECT HLDSLSHD
IF laScrMode[4]
  APPEND BLANK
ENDIF
GATHER FROM laData FIELDS &lcScFields MEMO
=gfAdd_Info()
IF WVISIBLE('lcWinHdr')
  =gfChClose('lcWinHdr')
ENDIF
*--Avoid eracing the updated fileds in HLDSLSHD from LFSVUSRFILEDS function in ARIA27.PRG  
STORE '' TO laUsrFields
RETURN

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Delete Descriptive id.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpDelScr
*!*************************************************************
** This procedure will be used in this screen instead of the standard
*  SAVE button.
*
PROCEDURE lpDelScr

SELECT(lcDtlTmp)
REPLACE ALL cStatus WITH IIF(cStatus<>'A','D','A')
=gfTmp2Mast('HLDSLSDT',lcDtlTmp,'Deleting descriptive list lines. Please wait.')
SELECT HLDSLSHD
SCATTER MEMVAR MEMO BLANK
GATHER MEMVAR MEMO
DELETE
laScrMode    = .F.
laScrMode[1] = .T.
SHOW GETS ONLY
IF WVISIBLE('lcWinHdr')
  =gfChClose('lcWinHdr')
ENDIF
RETURN


*!*************************************************************
*! Name      : lfvStyClr
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Validate sctyle\color.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyClr()
*!*************************************************************
*
FUNCTION lfvStyClr
PARAMETER lcStyle,lcClr

lcOldAlias = ALIAS()
SELECT Style
IF EMPTY(lcStyle) .AND. EMPTY(lcClr)
  STORE lcOldSty TO laTmpRec[1,1]
  STORE lcOldClr TO laTmpRec[1,2]
  _CUROBJ = OBJNUM(laTmpRec[1,1])
ELSE
  llRefresh = .T.
  llRecFnd = .F.

  *B605157,1 BWA 11/26/2001 Fix the bug of the "New" button is disabled in case the user add the style manually.[START]
  *IF SEEK(lcStyle+lcSepart+lcClr)
  IF SEEK(lcStyle+lcSepart+lcClr) AND LEFT(lcStyle,3) = ALLTRIM(laData[4])
  *B605157,1 BWA 11/26/2001.[END]

    SCAN WHILE Style = lcStyle+lcSepart+lcClr FOR cStyGroup = laData[7]
      llRecFnd = .T.

      *B605157,1 BWA 11/26/2001 Fix the bug of the "New" button is disabled in case the user add the style manually.[START]
      STORE SPACE(0) TO lcStyDesc
      lcStyDesc = Style.Desc
      *B605157,1 BWA 11/26/2001.[END]

      EXIT
    ENDSCAN
  ENDIF
  IF !llRecFnd
    DIMENSION laTemp[2]
    STORE SPACE(1) TO laTemp
    lcOldBrFld = lcBrFields
    lcOldTitle = lcFile_Ttl
    lcBrFields = "Style :H= lcMjrTtl+lcSepart+lcNMjrTl, Desc :H= 'Style Description', Season :H= 'Season Code',;
                  cDivision :H= 'Division Code', Pricea :H= 'Style Price'"
    lcFile_Ttl = 'Styles'
    lcSeason = laData[4]
    lcGroup  = laData[7]
    *B604828,1 TMI [Start] Search in style file for the selected season and style group
    LOCATE FOR Season = lcSeason .AND. cStyGroup = lcGroup
    IF FOUND()
    *B604828,1 TMI [End  ]    
      =gfBrows([FOR Season = lcSeason .AND. cStyGroup = lcGroup],'Style',;
                'laTemp')
    *B604828,1 TMI [Start] if no style match the criteria inform the user with this message.
    ELSE
      =gfModalGen('INM00000B42000','DIALOG',.F.,.F.,;
                   'No styles matching the specified season and style group.')
    ENDIF
    *B604828,1 TMI [End  ]
    lcBrFields = lcOldBrFld
    lcFile_Ttl = lcOldTitle
    IF !EMPTY(laTemp[1])
      IF llNewRec
        llNewRec = .F.
      ENDIF
      
      STORE SUBSTR(laTemp[1],1,lnSeasLen+lnStyleLen) TO laTmpRec[1,1]
      STORE SUBSTR(laTemp[1],lnClrPo,lnColorLen)     TO laTmpRec[1,2]
      SELECT(lcDtlTmp) 
      REPLACE STYLE   WITH STYLE.STYLE,;
              cStyle  WITH laTmpRec[1,1],;
              Color   WITH laTmpRec[1,2]
              
      lcStyDesc = IIF(SEEK(STYLE,'Style'),Style.Desc,SPACE(20))
      REPLACE CSTYDESC WITH lcStyDesc 
      
      *B804218,1 ABD Fix bug that When the user enters a new style in the detail 
      *B804218,1 ABD Screen the program should pick the sales description from 
      *B804218,1 ABD The style file and default it in the
      *B804218,1 ABD Description field. [Begin]
      REPLACE Mdlddesc  With Style.mSalesDesc
      *B804218,1 ABD [End]

      SHOW GET laTmpRec[1,3] ENABLE
      SHOW GET laTmpRec[1,4] ENABLE
      =lfwDscBrs()
    ELSE
      llRefresh = .F.
      STORE lcOldSty TO laTmpRec[1,1]
      STORE lcOldClr TO laTmpRec[1,2]
      _CUROBJ = OBJNUM(laTmpRec[1,1])
    ENDIF
  ELSE

    *B605157,1 BWA 11/26/2001 Fix the bug of the "New" button is disabled in case the user add the style manually.[START]
    PRIVATE lcSasonMan
    lcSasonMan = ALLTRIM(laData[4])
    IF LEFT(lcStyle,3) # lcSasonMan
      =gfModalGen('TRM32020B00000','ALERT','season '+ALLTRIM(lcSasonMan))
      STORE SPACE(0) TO laTmpRec[1,1]
      STORE SPACE(0) TO laTmpRec[1,2]    
      SHOW GETS WINDOW (lcWinBUT) ONLY
      _CUROBJ = OBJNUM(laTmpRec[1,1])
      RETURN
    ENDIF
    *B605157,1 BWA 11/26/2001.[END]

    IF llNewRec
      llNewRec = .F.
    ENDIF
    STORE SUBSTR(Style.Style,1,lnSeasLen+lnStyleLen) TO laTmpRec[1,1]
    STORE SUBSTR(Style.Style,lnClrPo,lnColorLen)     TO laTmpRec[1,2]
    SHOW GET laTmpRec[1,3] ENABLE
    SHOW GET laTmpRec[1,4] ENABLE

    *B605157,1 BWA 11/26/2001 Fix the bug of the "New" button is disabled in case the user add the style manually.[START]
    SELECT(lcDtlTmp)
    REPLACE STYLE    WITH STYLE.STYLE ,;
            CSTYDESC WITH lcStyDesc

    STORE SPACE(0) TO lcStyDesc
    *B605157,1 BWA 11/26/2001.[END]

  ENDIF
  IF llRefresh
    SELECT(lcDtlTmp) 
    lcKeys = EVAL(KEY())
    LOCATE FOR EMPTY(STYLE)
    IF !FOUND()
      llEmptyRec = .F.
    ELSE
      llEmptyRec = .T.  
    ENDIF  
    =SEEK(lcKeys)  
    SHOW GET pbRemove  ENABLE
  ENDIF
  SHOW GET laTmpRec[1,1] ENABLE
  SHOW GET laTmpRec[1,2] ENABLE
  SELECT(lcDtlTmp) 
  REPLACE STYLE   WITH STYLE.STYLE,;
          cStyle  WITH laTmpRec[1,1],;
          Color   WITH laTmpRec[1,2],;
          cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)
  *B804248,1 WAB (start) 
  REPLACE Mdlddesc  With Style.mSalesDesc
  *B804248,1 WAB (END)
  =lfwDscBrs()          
ENDIF
=lfRefresh()
IF !EMPTY(lcOldAlias)
  SELECT(lcOldAlias)
ENDIF 

*!*************************************************************
*! Name      : lfvNew
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Validate new sctyle\color.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvNew()
*!*************************************************************
*
FUNCTION lfvNew

SELECT(lcDtlTmp)
STORE SPACE(12) TO laTmpRec[1,1]
STORE SPACE(6)  TO laTmpRec[1,2]
STORE 0         TO laTmpRec[1,3]
STORE SPACE(1)  TO laTmpRec[1,4]
*--Get the line no
SET ORDER TO TAG (lcDtlTmp) DESC
lccArr = '  1'
IF SEEK (ladata[1])
  lccArr = STR(VAL(cArr)+1,3)
ENDIF
SET ORDER TO TAG (lcDtlTmp) ASCE
APPEND BLANK
REPLACE cdldlstid WITH laData[1],;
        cArr      WITH lccArr ,;
        Season    WITH laData[4],;
        CSTATUS   WITH 'A'
llEmptyRec = .T.
SHOW GET laTmpRec[1,1] ENABLE
SHOW GET laTmpRec[1,2] ENABLE
SHOW GET laTmpRec[1,3] DISABLE
SHOW GET laTmpRec[1,4] DISABLE
SHOW GET pbNew         DISABLE
SHOW GET pbRemove      ENABLE
_CUROBJ = OBJNUM(laTmpRec[1,1])
=lfwDscBrs()
RETURN

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Validate Remove sctyle\color.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvRemove()
*!*************************************************************
*
FUNCTION lfvRemove

PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lcDtlTmp)
REPLACE cStatus WITH IIF(cStatus<>'A','D','A')
DELETE
LOCATE
IF SEEK(laData[1])
  SHOW GET pbRemove      ENABLE
  SHOW GET laTmpRec[1,3] ENABLE
  SHOW GET laTmpRec[1,4] ENABLE
ELSE
  STORE SPACE(12) TO laTmpRec[1,1]
  STORE SPACE(6) TO laTmpRec[1,2]
  STORE 0        TO laTmpRec[1,3]
  STORE SPACE(1) TO laTmpRec[1,4]
  SHOW GET pbRemove      DISABLE
  SHOW GET laTmpRec[1,1] DISABLE
  SHOW GET laTmpRec[1,2] DISABLE
  SHOW GET laTmpRec[1,3] DISABLE
  SHOW GET laTmpRec[1,4] DISABLE
ENDIF
_CUROBJ = OBJNUM(pbNew)
lcKey = EVAL(KEY())
LOCATE
LOCATE REST FOR EMPTY(STYLE)
IF FOUND()
  llEmptyRec = .T.
ELSE
  llEmptyRec = .F.  
ENDIF  
=SEEK(lcKey)  
=lfwDscBrs()
SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfvUnits
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Validate Units.
*!*************************************************************
*! Parameters: lnUnits
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvRemove()
*!*************************************************************
*
FUNCTION lfvUnits
PARAMETER lnUnits

IF lnUnits < 0
  =gfModalGen('INM00000B00000','DIALOG',.F.,.F.,'The total units must be greater than zero.')
  _CUROBJ = _CUROBJ
ELSE
  REPLACE &lcDtlTmp..ndldtlunt WITH lnUnits
  =lfwDscBrs()
ENDIF
FUNCTION lfwNotes


*!*************************************************************
*! Name      : lfvNotes
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Validate Units.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvNotes()
*!*************************************************************
*
FUNCTION lfvNotes

PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lcDtlTmp)
REPLACE mdlddesc WITH laTmpRec[1,4]
SELECT (lnAlias)

*!**************************************************************************
*!
*!      FUNCTION: lfvZoom()
*!
*!**************************************************************************
*
FUNCTION lfvZoom

=gfZoom('laTmpRec[1,4]')
_CUROBJ = OBJNUM(laTmpRec[1,4])

*!**************************************************************************
*!
*!      FUNCTION: lfArangPop()
*!
*!**************************************************************************
*
FUNCTION lfArangPop



*!*************************************************************
*! Name      : lfChkUnComS
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Check for uncompleted invoice sessions
*!*************************************************************
*! Calls     : gfUnCompSession,lfCratTemp,lfBrowOrd,lfBrowDet
*!*************************************************************
*! Passed Parameters  :  llFrmSetup : .T. Called from Screen setup
*!                                    .F. Called from Screen Show function
*!*************************************************************
*! Returns            :  llContinue : .T. Found an uncompleted session
*!                                        No uncompleted sessions Found
*!*************************************************************
*! Example            :  =lfChkUnComS(.F.)
*!*************************************************************
FUNCTION lfChkUnComS
PARAMETERS llFrmSetup
PRIVATE lcObject

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Timour A. K.
*! Date      : 04/10/97
*! Purpose   : READ Activate function of INV700
*!*************************************************************
*
FUNCTION lfReadAct

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowTtl)

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Change folders.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfActFolder()
*!*************************************************************
*
FUNCTION lfActFolder

lcStatus = IIF(laScrMode[3] .OR. laScrMode[4],'ENABLE','DISABLE')
DO CASE
  CASE lnActFolder = 1
    ACTIVATE WINDOW (lcWinHdr)
    SHOW GETS WINDOW (lcWinHdr)  &lcStatus  ONLY
    SHOW GETS WINDOW (lcWinBUT)  DISABLE ONLY
    IF laScrMode[1]
      SHOW GET ibFolder[2] DISABLE
    ELSE
      SHOW GET ibFolder[2] ENABLE
      _CUROBJ = OBJNUM(laData[2])
    ENDIF
    IF laScrMode[4] AND !EMPTY(laData[4]) AND !EMPTY(laData[7]) ;
       AND RECCOUNT(lcDtlTmp) > 0
      SHOW GET laData[4] DISABLE
      SHOW GET laData[7] DISABLE
      SHOW GET ibSeason  DISABLE
      SHOW GET ibGroup   DISABLE
    ENDIF
    SHOW GET pbZoom ENABLE
    IF  laScrMode[3]
      SHOW GET laData[4] DISABLE
      SHOW GET laData[7] DISABLE
      SHOW GET ibSeason  DISABLE
      SHOW GET ibGroup   DISABLE
    ENDIF
    SHOW GET pbZom         DISABLE
  CASE lnActFolder = 2
    SHOW GETS WINDOW (lcWinHdr) DISABLE ONLY
    SHOW GETS WINDOW (lcWinBUT)  &lcStatus   ONLY
    lcNewStat = IIF(!llEmptyRec AND (laScrMode[3] OR laScrMode[4]) ,'ENABLE','DISABLE')
    =lfBrowLin()
    =lfwDscBrs()
    SHOW GET pbNew  &lcNewStat
    IF laScrMode[4] AND EMPTY(&lcDtlTmp..Style)
      _CUROBJ = OBJNUM(laTmpRec[1,1])
    ENDIF
    IF laScrMode[4] AND RECCOUNT(lcDtlTmp) = 0
      SHOW GET laTmpRec[1,1] DISABLE
      SHOW GET laTmpRec[1,2] DISABLE
      SHOW GET laTmpRec[1,3] DISABLE
      SHOW GET laTmpRec[1,4] DISABLE
      SHOW GET pbNew  &lcNewStat
      SHOW GET pbZom  ENABLE
    ENDIF
    SHOW GET pbZom         ENABLE
    SELECT HLDSLSHD
ENDCASE
RETURN

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs

*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.

*-- Compute Free/Color Items in Style Structure. [Begin]
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

*-- Loop Around Non Major elements.
FOR lnI = 1 TO ALEN(laMajSeg,1)
  DO CASE
    CASE laMajSeg[lnI,1] = "F"            && Free
      lcMajTtl   = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      lnStyleLen = LEN(laMajSeg[lnI,3])
      lnStyStPos = laMajSeg[lnI,4]
    CASE laMajSeg[lnI,1] = "C"            && Color 
      lnClrPo    = laMajSeg[lnI,4]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      lnColorLen = LEN(laMajSeg[lnI,3])
    CASE laMajSeg[lnI,1] = "Z"            && Season
      lnSeasLen = LEN(laMajSeg[lnI,3])      
  ENDCASE
ENDFOR    
RETURN ''
*-- End of lfEvalSegs.

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfEvalSegs()
*!*************************************************************
FUNCTION lpClsScr

SELECT HLDSLSHD
IF laScrMode[3]
  SCATTER FIELDS &lcScFields TO laData
ENDIF

*!*************************************************************
*! Name      : lfBrowLin
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Browse lines.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfBrowLin()
*!*************************************************************
FUNCTION lfBrowLin

SELECT(lcDtlTmp)
GO TOP
lnCurrRec = RECNO()
lcOldBrFld = lcBrFields
lcBrfields = [cMarker =IIF(RECNO()=lnCurrRec,'>',' '):1:R:H=' ':W=.F.,]+;
             [SEASON        :H= 'Season'  :10 :R,] +;
             [CSTYLE        :H= ' ' + lcMjrTtl + SPACE(40) : 30 :R,] +; 
             [COLOR         :H= ' ' + lcNMjrTl+' ' :15 :R,]+;
             [CSTYDESC      :H= 'Style description  '+SPACE(40) : 30 :R,] + ;
             [ndldtlunt     :H= 'Total units ' :15:R]
BROWSE FIELDS &lcBrfields  ;
      WHEN lfwDscBrs()    ;
      NOAPPEND NODELETE    ;
      NOMENU               ;
      NOWAIT NOCLEAR  SAVE ;
      TITLE lcBrowTtl      ;
      WINDOW (lcWinBrow) IN WINDOW (lcWinDtl)
ACTIVATE WINDOW (lcBrowTtl)
lcBrFields = lcOldBrFld

*!*************************************************************
*! Name      : lfwDscBrs
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : When Browse lines.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfwDscBrs()
*!*************************************************************
FUNCTION lfwDscBrs

SELECT(lcDtlTmp)
lnCurrRec = RECNO()
SHOW WINDOW (lcBrowTtl) REFRESH SAME
lcStatus   = IIF(laScrMode[1] OR laScrMode[2] OR EOF(),'DISABLE','ENABLE')
laTmpRec[1,1] = cstyle
laTmpRec[1,2] = Color
laTmpRec[1,3] = ndldtlunt
laTmpRec[1,4] = mdlddesc

SHOW GET laTmpRec[1,1] &lcStatus
SHOW GET laTmpRec[1,2] &lcStatus
SHOW GET laTmpRec[1,3] &lcStatus
SHOW GET laTmpRec[1,4] &lcStatus
SHOW GET ibStyle   &lcStatus
IF llEmptyRec
  SHOW GET pbNew DISABLE
ELSE
  IF laScrMode[3] .OR. laScrMode[4]
    SHOW GET pbNew ENABLE  
  ENDIF  
ENDIF
 
*!*************************************************************
*! Name      : lfwDscBrs
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Trap of keys
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lfwDscBrs
*!*************************************************************
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
 
*!*************************************************************
*! Name      : lfDeactivate
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : READ Deactivate function of screen SOORD
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfDeactivate()
*!*************************************************************
FUNCTION lfDeactivate

IF WONTOP()=lcBrowTtl
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  ON KEY LABEL TAB DO lpTrapKey WITH (lcWinBut), 'pbNew'
  *ON KEY LABEL BACKTAB DO lpBackTab WITH
ELSE
  glFromBrow = .F.
  ON KEY LABEL TAB
ENDIF

*!*************************************************************
*! Name      : lfClearKey
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
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
*! Name      : lfvDescBr
*! Developer : Adel Mohhamed El Gazzar
*! Date      : 05/02/2001
*! Purpose   : Browse Descriptive id.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDescBr()
*!*************************************************************
FUNCTION lfvDescBr

_CUROBJ = OBJNUM(laData[1])
laData[1] = "?"
KEYBOARD '{ENTER}'