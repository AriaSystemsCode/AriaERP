*!*****************************************************************************************
*! Name      : GfEndPrn
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/31/2002 06:01:39 PM
*! Purpose   : 
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
PROCEDURE GfEndPrn
  IF lcOGPlatForm != 'DOS'
    RETURN .T.
  ENDIF 
  SET PRINTER TO
  DO CASE
    CASE oAriaApplication.gcDevice = "PRINTER"
    CASE oAriaApplication.gcDevice = "FILE"
    CASE oAriaApplication.gcDevice = "SCREEN"
      LOCAL lcPreviewTitle
      lcPreviewTitle = IIF(TYPE('lcOGWinTitl')='C',lcOGWinTitl,'Report') + " Preview"
      =PreviewFile(oAriaApplication.gcOutFile,lcPreviewTitle)
      ERASE (oAriaApplication.gcOutFile)
  ENDCASE  
ENDPROC 

*!*****************************************************************************************
*! Name      : PreviewFile
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/03/2002 01:13:48 PM
*! Purpose   : 
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: 
*!****************************************************************************************
*! Returns   : 
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*!
FUNCTION PreviewFile
  LPARAMETERS cFileName, cWindTitle

  IF (VARTYPE(cFileName) != "C") OR EMPTY(cFileName) OR !FileExist(cFileName) OR;
     (VARTYPE(cWindTitle) != "C") OR EMPTY(cWindTitle)
    RETURN .F.
  ENDIF 

  LOCAL lcSetReSource, lcSetResourceTo, lnSetMemowidth, lnTypeAhead
  LOCAL lnScaleMode, lcWindName, lcIconFile, lcResourceFile
  *-- , lcTmpResource

  *-- Set Resource to the associated resource file... BEGIN
  *lcResourceFile  = ADDBS(oAriaApplication.ReportHome) + "ViewText.DBF" 
  lcResourceFile  = "ViewText.DBF" 
  *IF FILE(lcResourceFile)
  lcSetReSource   = SET("Resource")
  lcSetResourceTo = SET("Resource",1)

  *-- Save the resource to a temporary file... begin
  *lcResourceFile = gfTempName()
  *USE ViewText IN 0 SHARED
  *COPY TO (oAriaApplication.WorkDir+lcResourceFile)
  *USE IN ViewText
  
  SET RESOURCE OFF 
  SET RESOURCE TO (lcResourceFile)
  SET RESOURCE ON 
  *ENDIF 
  *-- Set Resource to the associated resource file... END

  *-- Set Memo Width
  lnSetMemowidth = SET("Memowidth")
  SET MEMOWIDTH TO 230
  SET DEVICE TO SCREEN

  lnTypeAhead = SET('TYPEAHEAD')
  CLEAR TYPEAHEAD 
  *SET TYPEAHEAD TO 128

  *-- Create the toolbar
  PRIVATE loToolBar
  LOCAL lcClassDir, loDummy as Form
  lcClassDir = ADDBS(oAriaApplication.ClassDir)
  loToolBar = NEWOBJECT("previewbar",lcClassDir+"optiongrid.vcx")
  loToolBar.cPreviewWind = cWindTitle
  loToolBar.Dock(0)
  loToolBar.VISIBLE = .T.

  lnScaleMode = _Screen.ScaleMode

  _Screen.ScaleMode = 0
  lcWindName = gfTempName()
  lcIconFile = _Screen.Icon

  DEFINE WINDOW (lcWindName) FROM 0,0 TO _Screen.Height, _Screen.Width ;
   FONT "FOXFONT",9 TITLE (cWindTitle);
    SYSTEM ICON FILE (lcIconFile) CLOSE

  _screen.ScaleMode = lnScaleMode

  ACTIVATE WINDOW (lcWindName) TOP NOSHOW 
*  MODIFY FILE (cFileName) WINDOW (lcWindName) NOEDIT NOMENU SAVE SAME

*! E037885,2 MAH 05/11/2005 Disable closing main screen [BEGIN]
LOCAL lcAltF4
lcAltF4 = ON("KEY", "ALT+F4")
ON KEY LABEL ALT+F4 llTempVar = .F.
oAriaApplication.oMainForm.Closable = .F.
*! E037885,2 MAH 05/11/2005 [END]

  MODIFY FILE (cFileName) WINDOW (lcWindName) IN WINDOW (oAriaApplication.oMainForm.Name) NOEDIT NOMENU SAVE SAME 

*! E037885,2 MAH 05/11/2005 Enable closing main screen [BEGIN]
oAriaApplication.oMainForm.Closable = .T.
ON KEY LABEL ALT+F1 &lcAltF4.
*! E037885,2 MAH 05/11/2005 [END]


  RELEASE WINDOWS (lcWindName)
  CLEAR TYPEAHEAD

  SET TYPEAHEAD TO lnTypeAhead
  SET MEMOWIDTH TO lnSetMemowidth

  *IF FILE(lcResourceFile)
  SET RESOURCE OFF 
  SET RESOURCE TO (lcSetResourceTo)
  SET RESOURCE &lcSetReSource.
  *ENDIF  
  KEYBOARD '{TAB}' + '{BACKTAB}' CLEAR PLAIN 
  
ENDFUNC 
*-- end of PreviewFile.



