***********************************************************************
*: Program file  : ICSRCTR.PRG
*: Program desc. : Serialized Inventory Lookup Screen
*: System        : Aria 4 XP
*: Developer     : Wael M. ABo-Shawareb (WSH)
*: Date          : 06/19/2006
*: Reference     : C131527,1
*:************************************************************************
*: Modifications :
*:*************************************************************************
LPARAMETERS loStyForm

DO FORM (oAriaApplication.ScreenHome + "ICSRLKUP") WITH loStyForm

*!*************************************************************
*! Name      : lfInitForm
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : FormSet Init Event
*!*************************************************************
*! Example   : =lfInitForm()
*!*************************************************************
FUNCTION lfInitForm
LPARAMETERS loFormSet, loParent

LOCAL lnAlias
lnAlias = SELECT(0)

loFormSet.loParent   = loParent
loFormSet.ActiveMode = loParent.ActiveMode

WITH loFormSet.AriaForm1
  .chkMaintain.Enabled = loFormSet.ActiveMode $ 'AE' AND loParent.llAllColors
  .txtStyle.Enabled    = .F.
  .txtColor.Enabled    = .F.
  .txtDesc.Enabled     = .F.
  .cmdOk.Enabled       = loFormSet.ActiveMode $ 'AE' AND loParent.llAllColors
  
  .lblStyle.Caption  = loParent.lcMjrTtl
  .lblColor.Caption  = loParent.lcNmjrTl + " :"
  .cmdCancel.Caption = IIF(loFormSet.ActiveMode $ 'AE' AND loParent.llAllColors, "\<Cancel", "\<Close")
  
  *-- Hide Location Combo Box if the system does not use Multi-Locations
  IF !loParent.llMultiWH
    .cboLocation.Visible = .F.
    .lblLocation.Visible = .F.
    .lblLocDots.Visible  = .F.
    
    .shpStyle.Height  = .shpStyle.Height  - .cboLocation.Height
    .grdSerial.Top    = .grdSerial.Top    - .cboLocation.Height
    .grdSerial.Height = .grdSerial.Height + .cboLocation.Height
  ENDIF
  
  DIMENSION .cboLocation.aSourceArray[1]
  =ACOPY(loParent.AriaForm1.pgfStyleInfo.Page2.cboLocation.aSourceArray, .cboLocation.aSourceArray)
  
  .chkMaintain.Value = loParent.llSRMaintain
  .cboLocation.Value = "******"
  .txtStyle.Value    = loParent.lcStyMajor
  .txtColor.Value    = IIF(loParent.llAllColors, STRTRAN(loParent.lcNMjrPt, "X", "*"), loParent.AriaForm1.kbNonMajor.KeyTextBox.Value)
  .txtDesc.Value     = loParent.AriaForm1.txtLongDesc.Value
ENDWITH

=lfCreateTemp(loFormSet)
=lfGetData(loFormSet)
=lfAdjustGrid(loFormSet)
=lfFilterGrid(loFormSet)
=lfRefreshButtons(loFormSet)

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfGetData
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Collect Data
*!*************************************************************
*! Example   : =lfGetData()
*!*************************************************************
FUNCTION lfGetData
LPARAMETERS loFormSet

LOCAL lnAlias, lcStyle, lcStyKey
lnAlias = SELECT(0)
llFound = .F.
lcStyle = IIF(loFormSet.loParent.llAllColors, SUBSTR(loFormSet.loParent.lcStyMajor, 1, loFormSet.loParent.lnStyleWid), loFormSet.loParent.lcStyleKey)

=gfOpenTable(oAriaApplication.DataDir+'INVSERNO',oAriaApplication.DataDir+'STYLE','SH')

SELECT (loFormSet.lcTmpSer)
ZAP

loFormSet.llLockMaintain = .F.

SELECT INVSERNO
IF gfSeek(lcStyle)
  DO WHILE !EOF() AND Style = lcStyle
    lcSerNum   = cSer_Num
    lcSeekKey  = Style+Sz1+cSer_Num
    lnTotQty   = 0
    
    SCAN REST WHILE Style+Sz1+cSer_Num+cWareCode+cIRType+STR(LINENO,6) = lcSeekKey
      lnTotQty    = lnTotQty + IIF(cIRType = 'I', -TotQty, TotQty)
      m.cWareCode = cWareCode
    ENDSCAN
    
    IF !EMPTY(lcSerNum) AND lnTotQty > 0
      m.lPrint    = .F.
      m.Style     = SUBSTR(lcSeekKey, 1, 19)
      m.cColor    = SUBSTR(m.Style, loFormSet.loParent.lnStyleWid + 2)
      m.cSize     = SUBSTR(lcSeekKey, 20, 5)
      m.cSerialNo = lcSerNum
      
      SELECT (loFormSet.lcTmpSer)
      APPEND BLANK
      GATHER MEMVAR
    ENDIF
    SELECT INVSERNO
  ENDDO
  
  loFormSet.llLockMaintain = .T.
ENDIF

*-- Revert changes done by Delete Command...
SELECT INVSERNO
=TABLEREVERT(.T.)

=gfCloseTable("INVSERNO")

SELECT (loFormSet.lcTmpSer)
LOCATE

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfFilterGrid
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Filter Grid on Selected Location
*!*************************************************************
*! Example   : =lfFilterGrid()
*!*************************************************************
FUNCTION lfFilterGrid
LPARAMETERS loFormSet

LOCAL lnAlias
lnAlias = SELECT(0)

SELECT (loFormSet.lcTmpSer)
SET KEY TO
REPLACE ALL lPrint WITH .F.

IF loFormSet.AriaForm1.cboLocation.Value # "******"
  SET KEY TO loFormSet.AriaForm1.cboLocation.Value
ENDIF
LOCATE

loFormSet.AriaForm1.grdSerial.Refresh()
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfRefreshButtons
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Refresh Grid Selection Buttons
*!*************************************************************
*! Example   : =lfRefreshButtons()
*!*************************************************************
FUNCTION lfRefreshButtons
LPARAMETERS loFormSet

*-- Refresh Grid Selection Buttons..
LOCAL lnAlias, lnRecNo, llPrint
lnAlias = SELECT(0)

IF EOF(loFormSet.lcTmpSer)
  loFormSet.AriaForm1.cmdSelect.Enabled = .F.
  loFormSet.AriaForm1.cmdAll.Enabled    = .F.
  loFormSet.AriaForm1.cmdNone.Enabled   = .F.
  loFormSet.AriaForm1.cmdInvert.Enabled = .F.
  loFormSet.AriaForm1.cmdPrint.Enabled  = .F.
  RETURN
ENDIF

SELECT (loFormSet.lcTmpSer)
lnRecNo = RECNO()

loFormSet.AriaForm1.cmdSelect.Caption = IIF(EVALUATE(loFormSet.lcTmpSer + '.lPrint'), "Un\<Select", "\<Select")
loFormSet.AriaForm1.cmdSelect.Enabled = .T.
loFormSet.AriaForm1.cmdInvert.Enabled = .T.

LOCATE FOR !lPrint
loFormSet.AriaForm1.cmdAll.Enabled  = FOUND()

LOCATE FOR lPrint
loFormSet.AriaForm1.cmdNone.Enabled  = FOUND()
loFormSet.AriaForm1.cmdPrint.Enabled = FOUND()

GOTO (lnRecNo)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfPrintLabel
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Print Label for Selected Style Pieces
*!*************************************************************
*! Example   : =lfPrintLabel()
*!*************************************************************
FUNCTION lfPrintLabel
LPARAMETERS loFormSet

LOCAL lnAlias, lcColor, lnRecNo
lnAlias = SELECT(0)

WAIT WINDOW "Collecting data to print..." NOWAIT

=gfOpenTable(oAriaApplication.DataDir+'StyleUPC',oAriaApplication.DataDir+'StyleUPC','SH')

*-- Create temp file used by OG in printing
lcTmpLbl = gfTempName()

DIMENSION laFileStru[7,18]
laFileStru[1,1] = 'cStyle'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 12
laFileStru[1,4] = 0
laFileStru[2,1] = 'cColor'
laFileStru[2,2] = 'C'
laFileStru[2,3] = '20'
laFileStru[2,4] = 0
laFileStru[3,1] = 'cSize'
laFileStru[3,2] = 'C'
laFileStru[3,3] = '5'
laFileStru[3,4] = 0
laFileStru[4,1] = 'cUPC'
laFileStru[4,2] = 'C'
laFileStru[4,3] = '13'
laFileStru[4,4] = 0
laFileStru[5,1] = 'cSerNo'
laFileStru[5,2] = 'C'
laFileStru[5,3] = '8'
laFileStru[5,4] = 0
laFileStru[6,1] = 'cFabNam'
laFileStru[6,2] = 'C'
laFileStru[6,3] = '40'
laFileStru[6,4] = 0
laFileStru[7,1] = 'cFurOrg'
laFileStru[7,2] = 'C'
laFileStru[7,3] = '20'
laFileStru[7,4] = 0

lnCount = 0
FOR lnCount = 1 TO 7
  STORE '' TO laFileStru[lnCount,7],laFileStru[lnCount,8],laFileStru[lnCount,9],;
              laFileStru[lnCount,10],laFileStru[lnCount,11],laFileStru[lnCount,12],;
              laFileStru[lnCount,13],laFileStru[lnCount,14],laFileStru[lnCount,15],;
              laFileStru[lnCount,16]
  STORE 0 TO  laFileStru[lnCount,17],laFileStru[lnCount,18]
ENDFOR
=gfCrtTmp(lcTmpLbl,@laFileStru)

SELECT (loFormSet.lcTmpSer)
lnRecNo = IIF(EOF(), 0, RECNO())

*-- Collect data for printing
SCAN FOR lPrint
  =gfSeek(Style + cSize, 'StyleUPC')
  
  *-- If Color Segment exists in Style Code Structure
  IF loFormSet.loParent.llDspClrDs
    lcColor = SUBSTR(Style, loFormSet.loParent.laSegInfo[loFormSet.loParent.lnColorSegPos,4], loFormSet.loParent.laSegInfo[loFormSet.loParent.lnColorSegPos,5])
    lcColor = gfCodDes(lcColor, "COLOR")
  ELSE
    lcColor = SUBSTR(Style, loFormSet.loParent.laSegInfo[loFormSet.loParent.lnColorSegPos,4], loFormSet.loParent.laSegInfo[loFormSet.loParent.lnColorSegPos,5])
  ENDIF
  
  SELECT (lcTmpLbl)
  INSERT INTO (lcTmpLbl) (cStyle, cColor, cSize, cUPC, cSerNo, cFabNam, cFurOrg) ;
               VALUES    (EVALUATE(loFormSet.lcTmpSer + '.Style'),;
                          lcColor,;
                          EVALUATE(loFormSet.lcTmpSer + '.cSize'),;
                          StyleUpc.cUpcNum1 + StyleUpc.cUpcNum2 + StyleUpc.cUpcNum3,;
                          EVALUATE(loFormSet.lcTmpSer + '.cSerialNo'),;
                          Style.cFabNam,;
                          Style.cFurOrg)
ENDSCAN

IF !EMPTY(lnRecNo)
  GOTO (lnRecNo)
ENDIF

SELECT (lcTmpLbl)
LOCATE

*-- Create a Dummy Object of the Option Grid...
PRIVATE loOGScroll

LOCAL lnDataSess, lcDevice, lcClassDir, oOptionGrid
lnDataSess = SET("Datasession")
lcDevice   = oAriaApplication.gcDevice
oAriaApplication.gcDevice = 'PRINTER'

lcClassDir   = ADDBS(oAriaApplication.ClassDir)
oOptionGrid  = NEWOBJECT("optiongrid",lcClassDir+"optiongrid.vcx")
loOGScroll   = oOptionGrid.OptionGrid.oHost
lcOGPlatForm = ''
loOgScroll.lcOGPlatForm  = ''
loOgScroll.lcOGLastForm  = 'ICSRLBL'
loOGScroll.llPrintPDF = .F.

DIMENSION loOGScroll.laSelFile[1,3]
loOGScroll.laSelFile = ''

SELECT (lcTmpLbl)
LOCATE

=gfDispRe('ICSRLBL')

SET DATASESSION TO (lnDataSess)

oOptionGrid = .NULL.

USE IN (lcTmpLbl)
=gfCloseTable("STYLEUPC")

WAIT CLEAR

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfAdjustGrid
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Adjust Grid Control Sources
*!*************************************************************
*! Exapmle   : =lfAdjustGrid()
*!*************************************************************
FUNCTION lfAdjustGrid
LPARAMETERS loFormSet

LOCAL lnAlias
lnAlias = SELECT(0)

SELECT (loFormSet.lcTmpSer)
WITH loFormSet.AriaForm1.grdSerial
  .RecordSource = ''
  .RecordSource = loFormSet.lcTmpSer
  
  .Column1.ControlSource = loFormSet.lcTmpSer + '.lPrint'
  .Column2.ControlSource = loFormSet.lcTmpSer + '.cColor'
  .Column3.ControlSource = loFormSet.lcTmpSer + '.cSize'
  .Column4.ControlSource = loFormSet.lcTmpSer + '.cSerialNo'
  
  .Column1.ReadOnly = .T.
  .Column2.ReadOnly = .T.
  .Column3.ReadOnly = .T.
  .Column4.ReadOnly = .T.
  
  .Column2.Header1.Caption  = loFormSet.loParent.lcNmjrTl
ENDWITH

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Valid Function for Select Button
*!*************************************************************
*! Example   : =lfvSelect()
*!*************************************************************
FUNCTION lfvSelect
LPARAMETERS loFormSet

LOCAL lnAlias
lnAlias = SELECT(0)

SELECT (loFormSet.lcTmpSer)
REPLACE lPrint WITH !lPrint

=lfRefreshButtons(loFormSet)
loFormSet.AriaForm1.grdSerial.Refresh()

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvSelectAll
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Valid Function for Select All Button
*!*************************************************************
*! Example   : =lfvSelectAll()
*!*************************************************************
FUNCTION lfvSelectAll
LPARAMETERS loFormSet

LOCAL lnRecNo, lnAlias
lnAlias = SELECT(0)

SELECT (loFormSet.lcTmpSer)
lnRecNo = IIF(EOF(), 0, RECNO())

REPLACE ALL lPrint WITH .T.

IF !EMPTY(lnRecNo)
  GOTO (lnRecNo)
ENDIF

=lfRefreshButtons(loFormSet)
loFormSet.AriaForm1.grdSerial.Refresh()

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvSelectNone
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Valid Function for Select None Button
*!*************************************************************
*! Example   : =lfvSelectNone()
*!*************************************************************
FUNCTION lfvSelectNone
LPARAMETERS loFormSet

LOCAL lnRecNo, lnAlias
lnAlias = SELECT(0)

SELECT (loFormSet.lcTmpSer)
lnRecNo = IIF(EOF(), 0, RECNO())

REPLACE ALL lPrint WITH .F.

IF !EMPTY(lnRecNo)
  GOTO (lnRecNo)
ENDIF

=lfRefreshButtons(loFormSet)
loFormSet.AriaForm1.grdSerial.Refresh()

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvInvert
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Valid Function for Invert Button
*!*************************************************************
*! Example   : =lfvInvert()
*!*************************************************************
FUNCTION lfvInvert
LPARAMETERS loFormSet

LOCAL lnRecNo, lnAlias
lnAlias = SELECT(0)

SELECT (loFormSet.lcTmpSer)
lnRecNo = IIF(EOF(), 0, RECNO())

REPLACE ALL lPrint WITH !lPrint

IF !EMPTY(lnRecNo)
  GOTO (lnRecNo)
ENDIF

=lfRefreshButtons(loFormSet)
loFormSet.AriaForm1.grdSerial.Refresh()

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvRefresh
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Valid Function for Refresh Button
*!*************************************************************
*! Example   : =lfvInvert()
*!*************************************************************
FUNCTION lfvRefresh
LPARAMETERS loFormSet

WAIT WINDOW "Recollecting Data..." NOWAIT

loFormSet.AriaForm1.LockScreen = .T.
loFormSet.AriaForm1.cboLocation.Value = "******"
loFormSet.AriaForm1.grdSerial.RecordSource = ''

=lfGetData(loFormSet)
=lfAdjustGrid(loFormSet)
=lfFilterGrid(loFormSet)
=lfRefreshButtons(loFormSet)

loFormSet.AriaForm1.LockScreen = .F.

WAIT CLEAR

*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Create Temp Cursor for the Grid
*!*************************************************************
*! Exapmle   : =lfCreateTemp()
*!*************************************************************
FUNCTION lfCreateTemp
LPARAMETERS loFormSet

LOCAL lnAlias, lnI, lnJ
lnAlias = SELECT(0)

loFormSet.lcTmpSer = gfTempName()

DIMENSION lafilestru[6,18]
lafilestru[1,1] = 'lPrint'
lafilestru[1,2] = 'L'
lafilestru[1,3] = 1
lafilestru[1,4] = 0
lafilestru[2,1] = 'Style'
lafilestru[2,2] = 'C'
lafilestru[2,3] = 19
lafilestru[2,4] = 0
lafilestru[3,1] = 'cWareCode'
lafilestru[3,2] = 'C'
lafilestru[3,3] = 6
lafilestru[3,4] = 0
lafilestru[4,1] = 'cColor'
lafilestru[4,2] = 'C'
lafilestru[4,3] = 19
lafilestru[4,4] = 0
lafilestru[5,1] = 'cSize'
lafilestru[5,2] = 'C'
lafilestru[5,3] = 5
lafilestru[5,4] = 0
lafilestru[6,1] = 'cSerialNo'
lafilestru[6,2] = 'C'
lafilestru[6,3] = 8
lafilestru[6,4] = 0

FOR lnI = 1 TO 6
  STORE 0 TO laFilestru[lnI,17], laFilestru[lnI,18]
  FOR lnJ = 7 TO 16
    STORE "" TO lafilestru[lnI,lnJ]
  ENDFOR
ENDFOR
=gfCrtTmp(loFormSet.lcTmpSer, @laFileStru, "cWareCode+cColor+cSize+cSerialNo", loFormSet.lcTmpSer)

SELECT (lnAlias)

*!*************************************************************
*! Name      : FileExist
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : To return the existance of the file "Used only by OG"
*!*************************************************************
*! Parameters: cFileToCheckFor 
*!*************************************************************
FUNCTION FileExist
LPARAMETERS cFileToCheckFor

RETURN loOGScroll.FileExist(cFileToCheckFor)
