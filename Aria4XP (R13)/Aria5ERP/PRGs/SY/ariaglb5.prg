*:************************************************************************
*: Program file  : ARIAGLB5.PRG
*: Program desc. : Aria5 Globals Prorgam
*:         System: Aria advantage series Ver. 5.0
*:         Module: Main system
*:           Date: 07/09/2017
*:      Developer: Mariam Mazhar[MMT]
*:************************************************************************
*: Modifications :
*: E303923,1 MMT 01/30/2018 Aria5 Messaging function[T20180124.0005]
*: B611523,1 MMT 02/05/2018 Fix Start page issues in different resolutions[Aria5 issues]
*: E612162,1 MMT 06/22/2020 Add Process Helper screen to Aria5 Menu
*!*************************************************************
*! Name      : lfTileRightClick
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/07/2017
*! Purpose   : Tile right click function
*!*************************************************************
FUNCTION lfTileRightClick
LPARAMETERS lcprogramname,lcModule,lcprogorwin,lcUser_ID,lcTitle,lcParamter,lcExpKey

DO CASE 
  CASE BAR() = 1
    lfRemoveFromFavorites(lcprogramname,lcModule,lcprogorwin,lcUser_ID,lcTitle,lcParamter)
  CASE BAR() = 2  
    lfRenameTile(lcprogramname,lcModule,lcprogorwin,lcUser_ID,lcTitle,lcParamter)
  CASE BAR() = 3
    lfMoveToFolder(lcprogramname,lcModule,lcprogorwin,lcUser_ID,lcTitle,lcParamter,lcExpKey)
ENDCASE
oAriaApplication.Context = 5
*!*************************************************************
*! Name      : lfRenameTile
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/07/2017
*! Purpose   : Rename Tile/Folder
*!*************************************************************
FUNCTION lfRenameTile
LPARAMETERS lcprogramname,lcModule,lcprogorwin,lcUser_ID,lcTitle,lcParamter 

IF lcModule = 'FL' && Renaming folder
  lcOldName = lcTitle
  lcEnteredName = lcTitle
  llCancelled = .F.
  HIDE POPUP popTileOptions

  oAriaApplication.oMainForm.imgTrans.Visible = .T.
  oAriaApplication.oMainForm.imgTrans.Top = 0
  oAriaApplication.oMainForm.imgTrans.left = 0
  oAriaApplication.oMainForm.imgTrans.Width = oAriaApplication.oMainForm.Width 
  oAriaApplication.oMainForm.imgTrans.Height = oAriaApplication.oMainForm.Height 

  DO FORM oAriaApplication.ScreenHome+ "sy\syfldnam.scx" WITH lcTitle TO lcNamFld
  oAriaApplication.oMainForm.imgTrans.Visible = .F.
  lcTitle = lcEnteredName 
  IF llCancelled
    DEACTIVATE POPUP  popTileOptions
    oAriaApplication.oMainForm.llNoRebuildMenu = .T.
    RETURN 
  ELSE
    lcFavoritesCursor = oAriaApplication.oMainForm.cFavoritesCursor
    SELECT(lcFavoritesCursor)
    LOCATE FOR cType = 'F' AND cName = lcOldName AND cSec = lcParamter 
    IF FOUND()
      REPLACE cName WITH lcTitle 
    ENDIF
    IF USED(lcFavoritesCursor)
      LOCAL lnOldSession, lnOldSelect
      lnOldSession = SET("Datasession")
      lnOldSelect  = SELECT()
      SET DATASESSION TO 1
      SELECT(lcFavoritesCursor)
      LOCAL loXMLParase, lcXML
      loXMLParase = NEWOBJECT('wwxml')
      lcXML = loXMLParase.CursorToXML()
      LOCAL lcPreferenceDir
      lcPreferenceDir = ADDBS(ALLTRIM(oAriaApplication.ResourceHome)) + ADDBS(ALLTRIM(oAriaApplication.User_ID))
      LOCAL lcPreferenceName
      lcPreferenceName = lcPreferenceDir + 'Favorites.xml'
      SET SAFETY OFF
      STRTOFILE(lcXML, lcPreferenceName ) 
      SELECT(lnOldSelect)
      SET DATASESSION TO lnOldSession
    ENDIF 
    oAriaApplication.oMainForm.DisplayFavorites(.T.,oAriaApplication.oMainForm.lcCurrSecVal)
    DEACTIVATE POPUP  popTileOptions
  ENDIF
ELSE  && Renaming program
  lcOldName = lcTitle
  lcEnteredName = lcTitle
  llCancelled = .F.
  HIDE POPUP popTileOptions
  oAriaApplication.oMainForm.imgTrans.Visible = .T.
  oAriaApplication.oMainForm.imgTrans.Top = 0
  oAriaApplication.oMainForm.imgTrans.left = 0
  oAriaApplication.oMainForm.imgTrans.Width = oAriaApplication.oMainForm.Width 
  oAriaApplication.oMainForm.imgTrans.Height = oAriaApplication.oMainForm.Height 
  DO FORM oAriaApplication.ScreenHome+ "sy\syfldnam.scx" WITH lcTitle,'R'
  oAriaApplication.oMainForm.imgTrans.Visible = .F.
  lcTitle = lcEnteredName 
  IF llCancelled
    DEACTIVATE POPUP  popTileOptions
    oAriaApplication.oMainForm.llNoRebuildMenu = .T.
    RETURN 
  ELSE
    lcFavoritesCursor = oAriaApplication.oMainForm.cFavoritesCursor
    SELECT(lcFavoritesCursor)
    LOCATE FOR cType = 'L' AND cExpKey = lcModule+ALLTRIM(SUBSTR(lcprogramname,4))+lcprogorwin AND cName = lcOldName &&AND cSec = lcParamter 
    IF FOUND()
      REPLACE cName WITH lcTitle 
    ENDIF
    IF USED(lcFavoritesCursor)
      LOCAL lnOldSession, lnOldSelect
      lnOldSession = SET("Datasession")
      lnOldSelect  = SELECT()
      SET DATASESSION TO 1
      SELECT(lcFavoritesCursor)
      LOCAL loXMLParase, lcXML
      loXMLParase = NEWOBJECT('wwxml')
      lcXML = loXMLParase.CursorToXML()
      LOCAL lcPreferenceDir
      lcPreferenceDir = ADDBS(ALLTRIM(oAriaApplication.ResourceHome)) + ADDBS(ALLTRIM(oAriaApplication.User_ID))
      LOCAL lcPreferenceName
      lcPreferenceName = lcPreferenceDir + 'Favorites.xml'
      SET SAFETY OFF
      STRTOFILE(lcXML, lcPreferenceName ) 
      SELECT(lnOldSelect)
      SET DATASESSION TO lnOldSession
    ENDIF  
    oAriaApplication.oMainForm.DisplayFavorites(.T.,oAriaApplication.oMainForm.lcCurrSecVal)
    DEACTIVATE POPUP  popTileOptions
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfMoveToFolder
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/07/2017
*! Purpose   : Move Tile/Folder to folder
*!*************************************************************
FUNCTION lfMoveToFolder
LPARAMETERS lcprogramname,lcModule,lcprogorwin,lcUser_ID,lcTitle,lcParamter,lcExpKey 


HIDE POPUP popTileOptions
 oAriaApplication.oMainForm.imgTrans.Visible = .T.
  oAriaApplication.oMainForm.imgTrans.Top = 0
  oAriaApplication.oMainForm.imgTrans.left = 0
  oAriaApplication.oMainForm.imgTrans.Width = oAriaApplication.oMainForm.Width 
  oAriaApplication.oMainForm.imgTrans.Height = oAriaApplication.oMainForm.Height 
DO FORM oAriaApplication.ScreenHome+ "sy\SYmvfld.scx" WITH oAriaApplication.oMainForm.cFavoritesCursor TO lcFolderSec
oAriaApplication.oMainForm.imgTrans.Visible = .F.

IF !EMPTY(lcFolderSec)
  LOCAL lnOldSession, lnOldSelect
  lnOldSession = SET("Datasession")
  lnOldSelect  = SELECT()
  SET DATASESSION TO 1
  SELECT(oAriaApplication.oMainForm.cFavoritesCursor)
  
  *-- Check if the same location.
  LOCAL llContinue
  llContinue = .T.
  lcCurrSecValue = IIF(TYPE('oAriaApplication.oMainForm.lcCurrSecVal')='L','FAVORITES',oAriaApplication.oMainForm.lcCurrSecVal)
  * cName = lcOldName AND cSec = lcParamter 
  IF IIF(lcModule = "FL" ,UPPER(ALLTRIM(lcFolderSec)) == UPPER(ALLTRIM(lcParamter)),;
     UPPER(ALLTRIM(lcFolderSec)) ==UPPER(ALLTRIM(lcCurrSecValue)))
    *: B611523,1 MMT 02/05/2018 Fix Start page issues in different resolutions[Start] 
    *MESSAGEBOX('Cannot move ' + ALLTRIM(lcTitle) + ': The destination folder is the same as the source folder.', 16, 'Move Folder')
    =GFMODALGEN5('TRM00000B00000',.F.,.F.,.F.,'Cannot move ' + ALLTRIM(lcTitle) + ': The destination folder is the same as the source folder.')
    *: B611523,1 MMT 02/05/2018 Fix Start page issues in different resolutions[End] 
    llContinue = .F.
  ENDIF

  *-- Check if the same parent.
  IF llContinue AND lcModule = "FL" 
    LOCATE FOR UPPER(ALLTRIM(cSec)) == UPPER(ALLTRIM(lcParamter))
    IF UPPER(ALLTRIM(lcFolderSec)) == UPPER(ALLTRIM(cParent))
      *: B611523,1 MMT 02/05/2018 Fix Start page issues in different resolutions[Start] 
      *MESSAGEBOX('Cannot move ' + ALLTRIM(lcTitle) + ': The destination folder is the same as the source folder.', 16, 'Move Folder')
      =GFMODALGEN5('TRM00000B00000',.F.,.F.,.F.,'Cannot move ' + ALLTRIM(lcTitle) + ': The destination folder is the same as the source folder.')
      *: B611523,1 MMT 02/05/2018 Fix Start page issues in different resolutions[End] 
      llContinue = .F.
    ENDIF
  ENDIF
  
  *-- Check if the user move a folder to sub folder.
  IF llContinue .AND. !(UPPER(ALLTRIM(lcFolderSec)) == 'FAVORITES') AND lcModule = "FL" 
    LOCATE FOR UPPER(ALLTRIM(cSec)) == UPPER(ALLTRIM(lcFolderSec))
    
    LOCAL lcParent
    lcParent = cParent
      
    DO WHILE !(UPPER(ALLTRIM(lcParent)) == 'FAVORITES')
      IF UPPER(ALLTRIM(lcParent)) == UPPER(ALLTRIM(lcParamter))
        *: B611523,1 MMT 02/05/2018 Fix Start page issues in different resolutions[Start] 
        *MESSAGEBOX('Cannot move ' + ALLTRIM(lcTitle) + ': The destination folder is subfolder of the source folder.', 16,'Move Folder')
        =GFMODALGEN5('TRM00000B00000',.F.,.F.,.F.,'Cannot move ' + ALLTRIM(lcTitle) + ': The destination folder is subfolder of the source folder.')
        *: B611523,1 MMT 02/05/2018 Fix Start page issues in different resolutions[End] 
        llContinue = .F.
        
        EXIT
      ENDIF
      
      LOCATE FOR UPPER(ALLTRIM(cSec)) == UPPER(ALLTRIM(lcParent))
      lcParent = cParent
    ENDDO
  ENDIF
  *-- Assign the new parent 
  IF llContinue
 
    LOCATE FOR IIF(lcModule = "FL" ,UPPER(ALLTRIM(cSec)) == UPPER(ALLTRIM(lcParamter)) AND CTYPE ='F',;
               CTYPE ='L' AND UPPER(ALLTRIM(cParent)) == UPPER(ALLTRIM(lcCurrSecValue)) AND  UPPER(ALLTRIM(cExpKey)) = UPPER(ALLTRIM(lcExpKey)))
    REPLACE cParent WITH lcFolderSec
    LOCAL lcSec
    lcSec = UPPER(ALLTRIM(cSec))
    lcFavoritesCursor = oAriaApplication.oMainForm.cFavoritesCursor
    IF USED(lcFavoritesCursor)
      LOCAL lnOldSession, lnOldSelect
      lnOldSession = SET("Datasession")
      lnOldSelect  = SELECT()
      SET DATASESSION TO 1
      SELECT(lcFavoritesCursor)
      LOCAL loXMLParase, lcXML
      loXMLParase = NEWOBJECT('wwxml')
      lcXML = loXMLParase.CursorToXML()
      LOCAL lcPreferenceDir
      lcPreferenceDir = ADDBS(ALLTRIM(oAriaApplication.ResourceHome)) + ADDBS(ALLTRIM(oAriaApplication.User_ID))
      LOCAL lcPreferenceName
      lcPreferenceName = lcPreferenceDir + 'Favorites.xml'
      SET SAFETY OFF
      STRTOFILE(lcXML, lcPreferenceName ) 
      SELECT(lnOldSelect)
      SET DATASESSION TO lnOldSession
    ENDIF  
    oAriaApplication.oMainForm.DisplayFavorites(.T.,oAriaApplication.oMainForm.lcCurrSecVal)
    
  ENDIF
  SELECT(lnOldSelect)
  SET DATASESSION TO lnOldSession
ENDIF
DEACTIVATE POPUP  popTileOptions
*Y
*!*************************************************************
*! Name      : lfRemoveFromFavorites
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/09/2017
*! Purpose   : Remove item from Favorites
*!*************************************************************
FUNCTION lfRemoveFromFavorites
LPARAMETERS lcprogramname,lcModule,lcprogorwin,lcUser_ID,lcTitle,lcParamter
lcFavoritesCursor = oAriaApplication.oMainForm.cFavoritesCursor
lcSetDel = SET("Deleted") 
SET DELETED OFF 

IF lcmodule = "FL"
  *: B611523,1 MMT 02/05/2018 Fix Start page issues in different resolutions[Start] 
  *IF MESSAGEBOX("Are you sure you want to remove the folder '"+lcTitle+"' and all of its contents?", 4+32, 'Delete Folder') = 7
  IF GFMODALGEN5('QRM00000B00006',.F.,.F.,.F.,"Are you sure you want to remove the folder '"+lcTitle+"' and all of its contents?") = 2
  *: B611523,1 MMT 02/05/2018 Fix Start page issues in different resolutions[End] 
    SET DELETED &lcSetDel.
    DEACTIVATE POPUP  popTileOptions
    RETURN 
  ENDIF
  lfRemoveFolder(ALLTRIM(lcParamter))
  SET DELETED &lcSetDel
  IF USED(lcFavoritesCursor)
    LOCAL lnOldSession, lnOldSelect
    lnOldSession = SET("Datasession")
    lnOldSelect  = SELECT()
    SET DATASESSION TO 1
    SELECT(lcFavoritesCursor)
    LOCAL loXMLParase, lcXML
    loXMLParase = NEWOBJECT('wwxml')
    lcXML = loXMLParase.CursorToXML()
    LOCAL lcPreferenceDir
    lcPreferenceDir = ADDBS(ALLTRIM(oAriaApplication.ResourceHome)) + ADDBS(ALLTRIM(oAriaApplication.User_ID))
    LOCAL lcPreferenceName
    lcPreferenceName = lcPreferenceDir + 'Favorites.xml'
    SET SAFETY OFF
    STRTOFILE(lcXML, lcPreferenceName ) 
    SELECT(lnOldSelect)
    SET DATASESSION TO lnOldSession
  ENDIF 
  oAriaApplication.oMainForm.DisplayFavorites(.T.,oAriaApplication.oMainForm.lcCurrSecVal)
  DEACTIVATE POPUP  popTileOptions
  RETURN 
ENDIF

SELECT(lcFavoritesCursor)
LOCATE FOR cExpKey = lcModule+ALLTRIM(SUBSTR(lcprogramname,4))+lcprogorwin AND !DELETED()
IF FOUND()
  DELETE
ENDIF
SET DELETED &lcSetDel
IF USED(lcFavoritesCursor)
  LOCAL lnOldSession, lnOldSelect
  lnOldSession = SET("Datasession")
  lnOldSelect  = SELECT()
  SET DATASESSION TO 1
  SELECT(lcFavoritesCursor)
  LOCAL loXMLParase, lcXML
  loXMLParase = NEWOBJECT('wwxml')
  lcXML = loXMLParase.CursorToXML()
  LOCAL lcPreferenceDir
  lcPreferenceDir = ADDBS(ALLTRIM(oAriaApplication.ResourceHome)) + ADDBS(ALLTRIM(oAriaApplication.User_ID))
  LOCAL lcPreferenceName
  lcPreferenceName = lcPreferenceDir + 'Favorites.xml'
  SET SAFETY OFF
  STRTOFILE(lcXML, lcPreferenceName ) 
  SELECT(lnOldSelect)
  SET DATASESSION TO lnOldSession
ENDIF 
oAriaApplication.oMainForm.DisplayFavorites(.T.,oAriaApplication.oMainForm.lcCurrSecVal)
DEACTIVATE POPUP  popTileOptions
oAriaApplication.Context = 5
*!*************************************************************
*! Name      : lfAddToFavorites
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/09/2017
*! Purpose   : Add item to Favorites
*! Entry     : E303736,1
*!*************************************************************
FUNCTION lfAddToFavorites
LPARAMETERS lcprogramname,lcModule,lcprogorwin,lcParam,lcName,loFormMenu
lcEnteredName  = lcName
llCancelled = .F.
DO FORM oAriaApplication.ScreenHome+ "sy\syfldnam.scx" WITH lcName,'N' TO lcFolderSec
IF llCancelled 
  oAriaApplication.Context = 5
  RETURN .F.
ENDIF
oAriaApplication.Context = 5
IF EMPTY(lcFolderSec)
  lcFolderSec = 'FAVORITES'
ENDIF


lcFavoritesCursor = oAriaApplication.oMainForm.cFavoritesCursor
SELECT(lcFavoritesCursor)

IF USED(lcFavoritesCursor)
  LOCAL lnOldSession, lnOldSelect
  lnOldSession = SET("Datasession")
  lnOldSelect  = SELECT()
  SET DATASESSION TO 1
  SELECT(lcFavoritesCursor)
  *-- Get Last no
  LOCAL lcSec
  CALCULATE MAX(VAL(cSec)) TO lcSec
  lcSec = lcSec + 1
  lcSec = PADR(STR(lcSec, 10), 10)
  
  *-- Append new record
  APPEND BLANK 
  REPLACE cType WITH 'L', ;
        cSec WITH lcSec, ;
        cName WITH lcEnteredName  , ;
        cParent WITH lcFolderSec , ;
        cEXPKey WITH UPPER(ALLTRIM(lcModule)) + UPPER(ALLTRIM(lcprogramname)) + UPPER(ALLTRIM(lcprogorwin)) + UPPER(ALLTRIM(lcParam))
  LOCAL loXMLParase, lcXML
  loXMLParase = NEWOBJECT('wwxml')
  lcXML = loXMLParase.CursorToXML()
  LOCAL lcPreferenceDir
  lcPreferenceDir = ADDBS(ALLTRIM(oAriaApplication.ResourceHome)) + ADDBS(ALLTRIM(oAriaApplication.User_ID))
  LOCAL lcPreferenceName
  lcPreferenceName = lcPreferenceDir + 'Favorites.xml'
  SET SAFETY OFF
  STRTOFILE(lcXML, lcPreferenceName ) 
  SELECT(lnOldSelect)
  SET DATASESSION TO lnOldSession
ENDIF 
oAriaApplication.oMainForm.DisplayFavorites(.T.,oAriaApplication.oMainForm.lcCurrSecVal)
RETURN .T.
*!*************************************************************
*! Name      : lfRemovdFolder
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/07/2017
*! Purpose   : Remove Folder 
*!*************************************************************
FUNCTION lfRemoveFolder
LPARAMETERS lcFldSec
LOCAL lcFavoritesCursor,lnPosRec
lcFavoritesCursor = oAriaApplication.oMainForm.cFavoritesCursor
IF USED(lcFavoritesCursor)
  SELECT (lcFavoritesCursor)
  SCAN FOR ALLTRIM(cParent) = ALLTRIM(lcFldSec) AND !DELETED()
    IF cType = 'F'
      lnPosRec = RECNO(lcFavoritesCursor)
      *X
      *lfRemovdFolder(ALLTRIM(cSec))
      lfRemoveFolder(ALLTRIM(cSec))
      *X
      IF BETWEEN(lnPosRec ,1,RECCOUNT(lcFavoritesCursor))
        GO RECORD lnPosRec IN (lcFavoritesCursor)
      ENDIF
    ELSE
      DELETE  
    ENDIF
  ENDSCAN
  LOCATE FOR ALLTRIM(cSec) = ALLTRIM(lcFldSec) AND !DELETED()
  IF FOUND()
    DELETE
  ENDIF
ENDIF
*: E303923,1 MMT 01/30/2018 Aria5 Messaging function[T20180124.0005][Start]
*!*************************************************************
*! Name      : GFMODALGEN5
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/30/2018
*! Purpose   : Show Message Box
*! Parameters: *!                1-lcDlgID   (Dialog ID)
*!                          1st 2 characters are TR for Terminat icon
*!                                               QR for Quiry    icon
*!                                               IN for Inform   icon
*!                          2nd 4 characters are the messag ID
*!                          3rd 4 characters are the button ID
*!                2-lcDlgTyp  (Dialog type)
*!                          'D' --> Dialog colors
*!                          'A' --> Alert  colors
*!                3-lcVarsStr  (variable(s) to be replased in the messag
*!                4-lcDlgValid (Validation function name to be used in
*!                              the valid of the dialog buttons)
*!                5-lcDlgMessg if you want to display a specific message
*!                            send the message string to this parameter
*!*************************************************************
FUNCTION GFMODALGEN5
LPARAMETER LCDLGID,LCDLGTYP,LCVARSSTR,LCDLGVALID,LCDLGMESSG
*RETURN oAriaApplication.MessageBox(lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg)

* B037981,1 MAH Disable toolbar before show message
IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(OARIAAPPLICATION.OTOOLBAR)
  OARIAAPPLICATION.OTOOLBAR.ENABLED = .F.
ENDIF
* B037981,1 MAH End

LOCAL OMESSAGEBOX, LLACTIVEFORMLOCKED, LLFORMEXIST
OMESSAGEBOX = NEWOBJECT("Aria5MessageBox",ADDBS(OARIAAPPLICATION.CLASSDIR)+"Utility5.vcx")
IF VARTYPE(OMESSAGEBOX) != "O"
  * N119813,FW1 MAH Enable toolbar before return
  IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(OARIAAPPLICATION.OTOOLBAR)
    OARIAAPPLICATION.OTOOLBAR.ENABLED = .T.
  ENDIF
  * N119813,FW1 MAH End

  RETURN 0
ENDIF

*-- Get the dialog and buttons from the dictionary.
IF !OMESSAGEBOX.GETMESSAGE(LCDLGID,LCDLGTYP,LCVARSSTR,LCDLGVALID,LCDLGMESSG)
  * N119813,FW1 MAH Enable toolbar before return
  IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(OARIAAPPLICATION.OTOOLBAR)
    OARIAAPPLICATION.OTOOLBAR.ENABLED = .T.
  ENDIF
  * N119813,FW1 MAH End

  RETURN 0
ENDIF

IF (TYPE("_SCREEN.ActiveForm.LockScreen") = "L") AND !EMPTY(TYPE("_SCREEN.ActiveForm.LockScreen")) AND TYPE('_SCREEN.ActiveForm') = 'O' AND !ISNULL(_SCREEN.ACTIVEFORM)
  LLFORMEXIST = .T.
  LLACTIVEFORMLOCKED = _SCREEN.ACTIVEFORM.LOCKSCREEN
  _SCREEN.ACTIVEFORM.LOCKSCREEN = .F.
ENDIF

PRIVATE LNMESSAGECHOICE
LNMESSAGECHOICE = 1
OMESSAGEBOX.SETMESSAGEBOX()    && Set message parameters.

PUSH KEY
ON KEY
OMESSAGEBOX.SHOW()  && Show the message.
POP KEY

OMESSAGEBOX = .NULL.
RELEASE OMESSAGEBOX

IF LLFORMEXIST AND TYPE('_SCREEN.ActiveForm') = 'O' AND !ISNULL(_SCREEN.ACTIVEFORM)
  _SCREEN.ACTIVEFORM.LOCKSCREEN = LLACTIVEFORMLOCKED
ENDIF

* B037981,1 MAH Enable toolbar before show message
IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(OARIAAPPLICATION.OTOOLBAR)
  OARIAAPPLICATION.OTOOLBAR.ENABLED = .T.
ENDIF
* B037981,1 MAH End

RETURN LNMESSAGECHOICE  && Return the response.
ENDFUNC
*: E303923,1 MMT 01/30/2018 Aria5 Messaging function[T20180124.0005][End]

*: E612162,1 MMT 06/22/2020 Add Process Helper screen to Aria5 Menu[Start]
*!*************************************************************
*! Name      : lfOpenProgOrReport
*! Developer : Mariam Mazhar[MMT]
*! Date      : 06/22/2020 
*! Purpose   : Open Report or Program
*!*************************************************************
FUNCTION lfOpenProgOrReport
LPARAMETERS lcModule,lcProgramName,lcProgOrWin,lcParameter 
SET STEP ON 
IF oAriaApplication.ActiveModuleID <> lcModule 
  oAriaApplication.ChangeModule(lcModule)
ENDIF  

IF EMPTY(oAriaApplication.ActiveCompanyID)
  RETURN
ENDIF
*XXX
oAriaApplication.processid = lcProgramName
IF lcProgOrWin <> 'R'
  oAriaApplication.DoProgram('AWR'+lcProgramName,lcParameter  ,lcProgOrWin ,.f.)
ELSE  
  oAriaApplication.ReportPrint(lcProgramName,lcParameter ,lcModule)
ENDIF
*: E612162,1 MMT 06/22/2020 Add Process Helper screen to Aria5 Menu[End]
FUNCTION lfSwitchView
LPARAMETERS loFormSet
lnBar = BAR()
llGridView = (lnBar = 1)
loFormSet.SwitchView(llGridView)
DEACTIVATE POPUP popViewOptions
*XXXX