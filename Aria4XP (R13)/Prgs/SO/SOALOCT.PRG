*:************************************************************************
*: Program file  : SOALOCT.PRG
*: Program desc. : 1- Cutting Ticket Allocation
*:               : 2- Style Purchase Orders Allocation
*: For screen    : SOALOCT.SCX (0,1,2,3)
*: System        : Aria 4.0 XP
*: Module        : Shared between Purchase orders & Manufactering Modules
*: Developer     : Wael M. Abo-Shawareb (WSH)
*: Issue NO.     : 037584,1                                                 :*
*:************************************************************************
*: Passed Parameters  : lcAllocate : 'U' Cutting Ticket Allocation
*:                                   'P' Style Purchase Orders Allocation
*:*************************************************************
*: Example            :
*: =oAriaApplication.DoProgram('AWRSOALOCT',.F.,'MF','U')
*:*************************************************************
*:Modification:
*! B609356,1 SMA 07/21/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*! B610485,1 HIA 08/22/13 T20130808.0001 - PO - error in Style PO Allocation screen
*! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002]
*! B611009,2 MMT 05/31/2015 PO allocation program crashes if PO styles conatins ?[T20150527.0002]
*:*************************************************************
*: B607585,1 AMH Add cwarecode field to POSLN index.
LPARAMETERS lcAllocate

#INCLUDE R:\ARIA4XP\PRGS\SO\SOALOCT.H

*-- Call the screen.
DO FORM (oAriaApplication.ScreenHome + "SO\SOALOCT") WITH lcAllocate

RETURN

*!*************************************************************
*! Name      : lfAllocFrmInit
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/29/2004
*! Purpose   : Main Form Init Event
*!           : main screen
*!*************************************************************
FUNCTION lfAllocFrmInit
LPARAMETERS loFormSet

LOCAL lcCount, lnCount

WITH loFormSet
  .lcDyeConfig = IIF(ALLTRIM(gfGetMemVar('M_DYELOT')) = 'Y', IIF(ALLTRIM(gfGetMemVar('M_STYCNFG')) = 'Y', 'C', 'D'), 'N')
  .llGenOrNum  = ALLTRIM(gfGetMemVar('M_GenStOrN')) = 'Y'
  .llExtSizSc  = gfGetMemVar('M_USEEXSSC')

  *-- "llDispPric" hold setting variable to display selling price & gross margin
  .llDispPric  = IIF(.lcAllocate = 'P', gfGetMemVar('M_PoDspPrc'), gfGetMemVar('M_MfDspPrc'))
  .llStyMark   = gfGetMemVar('M_stymark') = 'T'
  .lcKeyCode   = IIF(.lcAllocate = "U", '1', '2')
  .lcMajHead   = gfItemMask('HM', '', loFormSet.lcInvType)

  .lcOrder = ''
  .lcStyle = ''

  *-- None of the screen's objects has been updated.
  .llCUpDate  = .F.
  .llAloMulSt = .F.      && Variabel to get the user choice , if he/She want to allocate all stores for this line or not
  .llOrder    = .F.      && Variabel to check if we change the allocation Qty from Order button
  .llMultiStr = .F.
  .llRepDye   = .F.      && Flag to know if replace cyurrent order line with the current CT/PO line.

  .AriaForm1.kbPONo.KeyTextBox.Value = SPACE(6)
  .AriaForm1.Caption = IIF(.lcAllocate = "P" , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_POHEAD,loFormSet.GetHeaderText("LANG_SOALOCT_POHEAD",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_CTHEAD,loFormSet.GetHeaderText("LANG_SOALOCT_CTHEAD",loFormSet.HeaderAlias))) +;
                       IIF(TYPE('gnProgCopy') = 'N' AND gnProgCopy > 1, ' /' + ALLTRIM(STR(gnProgCopy)), '')

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.AriaForm1.lblOrderNo.Caption = IIF(.lcAllocate = "P" , LANG_SOALOCT_LBLSPO, LANG_SOALOCT_LBLCTO)
  .AriaForm1.lblOrderNo.Caption = IIF(.lcAllocate = "P" , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_LBLSPO,loFormSet.GetHeaderText("LANG_SOALOCT_LBLSPO",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_LBLCTO,loFormSet.GetHeaderText("LANG_SOALOCT_LBLCTO",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]


  LOCAL lcBrwQty
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrwQty = IIF(.lcAllocate = "U" , IIF(.llExtSizSc, LANG_SOALOCT_CUT, LANG_SOALOCT_SZ), LANG_SOALOCT_PO)
lcBrwQty = IIF(.lcAllocate = "U" , IIF(.llExtSizSc, IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_CUT,loFormSet.GetHeaderText("LANG_SOALOCT_CUT",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SZ,loFormSet.GetHeaderText("LANG_SOALOCT_SZ",loFormSet.HeaderAlias))), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_PO,loFormSet.GetHeaderText("LANG_SOALOCT_PO",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]


  FOR lnCount = 1 TO 8
    lcCount = STR(lnCount,1)
    .laBrwQty[lnCount] = lcBrwQty + lcCount
  ENDFOR

  *-- Define the needed arrays in this file.
  DECLARE .laSize[9], .laOrd[9] , .laOldLaOrd[9]
  STORE "" TO .laSize
  .laSize[9] = 0

  DIMENSION .laStrQty[9]      && Array to hold the Qty of all lines of the multi stor order
  DIMENSION .laStrCut[9]      && Array to hold the Cut of all lines of the multi stor order
  DIMENSION .laLinQty[9]      && Array to hold the Qty for each line of the multi stor order
  DIMENSION .laStrlnCut[9]    && Array to hold the Cut for each line of the multi stor order which will be store
  STORE 0 TO .laStrQty, .laStrCut, .laLinQty, .laStrlnCut, .laOrd

  .lcCurKey   = ""     && Hold the main keys to prevent double collecting the data.
  .lcKeyFld   = ""     && Hold the P/O # or C/T #.
  .lcCurOrder = ""     && Hold the current order no.

  *--Get the style header.
  .lcStyHdr   = gfItemMask("HI")

  *--Define the base file temp name.
  .lcAlocFile = gfTempName()

  *--Define temp grids and work cursors
  .lcCutPick  = gfTempName()
  .lcOrderLin = gfTempName()
  .lcMainLin  = gfTempName()
  .lcOrdStLin = gfTempName()
  .lcVendor   = gfTempName()

  *--Adjust Main Browse Fields
  IF .lcAllocate = 'P'
    .lcBrFields = "PO        :H = '" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_PONO,loFormSet.GetHeaderText("LANG_SOALOCT_PONO",loFormSet.HeaderAlias))   + "',"  +;
                  "Status    :H = '" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_STATUS,loFormSet.GetHeaderText("LANG_SOALOCT_STATUS",loFormSet.HeaderAlias)) + "'," +;
                  "Vendor    :H = '" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_VENDOR,loFormSet.GetHeaderText("LANG_SOALOCT_VENDOR",loFormSet.HeaderAlias)) + "'," +;
                  "lcVnName = " + .lcVendor + ".cVenComp :H = '" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_VENNAME,loFormSet.GetHeaderText("LANG_SOALOCT_VENNAME",loFormSet.HeaderAlias))   + "',"  +;
                  "Entered   :H = '" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_ENTERED,loFormSet.GetHeaderText("LANG_SOALOCT_ENTERED",loFormSet.HeaderAlias))  + "'," +;
                  "Complete  :H = '" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_COMPLETE,loFormSet.GetHeaderText("LANG_SOALOCT_COMPLETE",loFormSet.HeaderAlias)) + "'," +;
                  "nStyOrder :H = '" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_TOTQTY,loFormSet.GetHeaderText("LANG_SOALOCT_TOTQTY",loFormSet.HeaderAlias))   + "'," +;
                  "POTotal   :H = '" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_AMOUNT,loFormSet.GetHeaderText("LANG_SOALOCT_AMOUNT",loFormSet.HeaderAlias))   + "'," +;
                  "Receive   :H = '" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_RECEIVE,loFormSet.GetHeaderText("LANG_SOALOCT_RECEIVE",loFormSet.HeaderAlias))  + "'," +;
                  "Open      :H = '" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_OPEN,loFormSet.GetHeaderText("LANG_SOALOCT_OPEN",loFormSet.HeaderAlias))     + "'"

    IF !lfSQLStatement('SELECT * FROM APVENDOR', .lcVendor, 'CVENDCODE|', .lcVendor + '|', .T.)
      RETURN .F.
    ENDIF

    SET ORDER TO (.lcVendor) IN (.lcVendor)
  ELSE
    .lcBrFields = "PO       :H ='" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_TKTNO,loFormSet.GetHeaderText("LANG_SOALOCT_TKTNO",loFormSet.HeaderAlias))    + "',"+;
                  "Style    :H ='" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_STYLE,loFormSet.GetHeaderText("LANG_SOALOCT_STYLE",loFormSet.HeaderAlias))    + "',"+;
                  "Status   :H ='" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_STATUS,loFormSet.GetHeaderText("LANG_SOALOCT_STATUS",loFormSet.HeaderAlias))   + "',"+;
                  "Entered  :H ='" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_ISSUE,loFormSet.GetHeaderText("LANG_SOALOCT_ISSUE",loFormSet.HeaderAlias))    + "',"+;
                  "Complete :H ='" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_COMPLETE,loFormSet.GetHeaderText("LANG_SOALOCT_COMPLETE",loFormSet.HeaderAlias)) + "',"+;
                  "Season   :H ='" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SEASON,loFormSet.GetHeaderText("LANG_SOALOCT_SEASON",loFormSet.HeaderAlias))   + "',"+;
                  "cDivision:H ='" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_DIVISION,loFormSet.GetHeaderText("LANG_SOALOCT_DIVISION",loFormSet.HeaderAlias)) + "',"+;
                  "nStyOrder:H ='" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_BUDGET,loFormSet.GetHeaderText("LANG_SOALOCT_BUDGET",loFormSet.HeaderAlias))   + "':P='999999',"+;
                  "Receive  :H ='" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_RECEIVE,loFormSet.GetHeaderText("LANG_SOALOCT_RECEIVE",loFormSet.HeaderAlias))  + "':P='999999',"+;
                  "Damage   :H ='" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_DAMAGE,loFormSet.GetHeaderText("LANG_SOALOCT_DAMAGE",loFormSet.HeaderAlias))   + "':P='999999',"+;
                  "Open     :H= '" + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_OPEN,loFormSet.GetHeaderText("LANG_SOALOCT_OPEN",loFormSet.HeaderAlias))     + "':P='999999' "

  ENDIF

  .AriaBrFields.edtBrowseFields.Value = .lcBrFields

  *--Prepare the Main Screen file and assign SQL Browse Properties for the Formset
  LOCAL llNoRecordFound, llServerError

  .cBrowseTableDBEngine   = 'SQL'
  .cBrowseAliasName       = .lcAlocFile
  .cBrowseTableName       = 'POSHDR'
  .cBrowseIndexName       = 'POSHDR'
  .cBrowseIndexExpression = 'cBusDocu+cStyType+PO'
  .cBrowseIndexFields     = 'cBusDocu,cStyType,PO'
  .cBrowseKey             = 'P' + .lcAllocate

  *B128956,1 KHM 07/13/2005 Change it to single quotation [Begin]
  *.cBrowseFilter          = 'Status <> "X"'
  .cBrowseFilter          = "Status <> 'X'"
  *B128956,1 KHM 07/13/2005 [End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.BrowseTitle            = IIF(.lcAllocate = 'P', LANG_SOALOCT_BROWTTLSPO, LANG_SOALOCT_BROWTTLCTO)
.BrowseTitle            = IIF(.lcAllocate = 'P', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_BROWTTLSPO,loFormSet.GetHeaderText("LANG_SOALOCT_BROWTTLSPO",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_BROWTTLCTO,loFormSet.GetHeaderText("LANG_SOALOCT_BROWTTLCTO",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]


  .oRemoteCursor.mGetCursor(.cBrowseAliasName,.cBrowseAliasName,.DataSessionID,;
                            'STRUCTURE',.F.,.cBrowseTableName,'*','',.cBrowseIndexName,;
                            .cBrowseIndexExpression,.F.,.cBrowseIndexFields,0,;
                            .cBrowseFilter,.cBrowseKey,@llNoRecordFound,@llServerError)

  *-- Set Relation to get vendor name in main browse
  IF .lcAllocate = 'P'
    SELECT (.lcAlocFile)
    SET RELATION TO Vendor INTO (.lcVendor)
  ENDIF

  *-- To pass the required information for the Work Order Key.
  WITH .Ariaform1.kbPoNo
    .cbusinessdocumenttype    = 'P'
    .cworkordertype           = loFormSet.lcAllocate
    .obrowsecursor            = loFormSet.lcAlocFile
    .cbrowsetitle             = loFormSet.BrowseTitle
    .cbrowsefields            = loFormSet.AriaBrFields.edtBrowseFields.Value
    .lcBrowRetFld             = 'PO'
    .Keytextbox.InputMask     = lfPicture(loFormSet)
    .Keytextbox.Format        = lfPicture(loFormSet)
    .Keytextbox.ControlSource = loFormSet.lcAlocFile + '.PO'
  ENDWITH

  *--Set value for FormSet Master File and Data Environment Initial Selected Cursor
  .DataEnvironment.InitialSelectedAlias = .lcAlocFile

  *--Create main file Indecies
  =lfCreateIndecies(.lcAlocFile, 'PO|', 'POSHDR|')

  IF !lfCreateTmpFiles(loFormSet)
    RETURN .F.
  ENDIF

  *--Buil Form Grids
  =lfBuildPOGrid(loFormSet)
  =lfBuildSOGrid(loFormSet)
ENDWITH

*!*************************************************************
*! Name      : lfAllocFrmAct
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/29/2004
*! Purpose   : Main Form Activate Event
*!           : main screen
*!*************************************************************
FUNCTION lfAllocFrmAct
LPARAMETERS loFormSet

=lfActPad(loFormSet)

*!*************************************************************
*! Name      : lfAllocFrmDeact
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/29/2004
*! Purpose   : Main Form Deactivate Event
*!           : main screen
*!*************************************************************
FUNCTION lfAllocFrmDeact

*-- Release the Inquire C/T or P/O pad from the system menu.
RELEASE PAD _INQUIRE OF _MSYSMENU

*!*************************************************************
*! Name      : lfAllocFrmDest
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/29/2004
*! Purpose   : Main Form Destroy Event
*!           : main screen
*!*************************************************************
FUNCTION lfAllocFrmDest
LPARAMETERS loFormSet

loFormSet.loOrdHdr = .NULL.
=lfAllocFrmDeact()

*!*************************************************************
*! Name      : lfActPad
*! Developer : Wael M. Abo-Shawareb
*! Date      : 11/29/2004
*! Purpose   : Bulid a new menu pad [Option] in the when of the
*!           : main screen
*!*************************************************************
FUNCTION lfActPad
LPARAMETERS loFormSet

*-- Define a pad called options have 2 bars :
*-- 1- Inquire the current C/T or P/O.
*-- 2- Inquire the current order no.

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*DEFINE PAD _INQUIRE OF (loFormSet.cHostFormName) PROMPT 'O\<ptions' KEY ALT+P, ' '
DEFINE PAD _INQUIRE OF (loFormSet.cHostFormName) PROMPT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_OPTIONS,loFormSet.GetHeaderText("LANG_SOALOCT_OPTIONS",loFormSet.HeaderAlias)) KEY ALT+P, ' '
*N000682,1 11/20/2012 MMT Globlization changes[END]
SET SKIP OF PAD _INQUIRE OF (loFormSet.cHostFormName) TYPE('_SCREEN.ActiveForm.Parent.ActiveMode') = 'C' AND _Screen.ActiveForm.Parent.ActiveMode = 'S'
ON PAD _INQUIRE OF (loFormSet.cHostFormName) ACTIVATE POPUP _INQUIREPOP

DEFINE POPUP _INQUIREPOP MARGIN
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*DEFINE BAR 1 OF _INQUIREPOP PROMPT IIF(loFormSet.lcAllocate = "U", LANG_SOALOCT_CTINQUIRE, LANG_SOALOCT_POINQUIRE) SKIP FOR TYPE('_SCREEN.ActiveForm.Parent.ActiveMode') = 'C' AND _Screen.ActiveForm.Pa
DEFINE BAR 1 OF _INQUIREPOP PROMPT IIF(loFormSet.lcAllocate = "U",;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_CTINQUIRE,loFormSet.GetHeaderText("LANG_SOALOCT_CTINQUIRE",loFormSet.HeaderAlias)),;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_POINQUIRE,loFormSet.GetHeaderText("LANG_SOALOCT_POINQUIRE",loFormSet.HeaderAlias)))  SKIP FOR TYPE('_SCREEN.ActiveForm.Parent.ActiveMode') = 'C' AND _Screen.ActiveForm.Parent.ActiveMode = 'S'
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*DEFINE BAR 2 OF _INQUIREPOP PROMPT LANG_SOALOCT_SOINQUIRE SKIP FOR (TYPE('_SCREEN.ActiveForm.Parent.ActiveMode') = 'C' AND _Screen.ActiveForm.Parent.ActiveMode = 'S') .OR. TYPE("_Screen.ActiveForm.Par
DEFINE BAR 2 OF _INQUIREPOP PROMPT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
   LANG_SOALOCT_SOINQUIRE,loFormSet.GetHeaderText("LANG_SOALOCT_SOINQUIRE",loFormSet.HeaderAlias));
   SKIP FOR (TYPE('_SCREEN.ActiveForm.Parent.ActiveMode') = 'C' AND _Screen.ActiveForm.Parent.ActiveMode = 'S') .OR. ;
   TYPE("_Screen.ActiveForm.Parent.lcCurOrder") # 'C' .OR. EMPTY(_Screen.ActiveForm.Parent.lcCurOrder)
*N000682,1 11/20/2012 MMT Globlization changes[End]


ON SELECTION POPUP _INQUIREPOP llDumy = lpInquire(_Screen.ActiveForm.Parent)

*!*************************************************************
*! Name      : lpInquire
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/29/2004
*! Purpose   : Do Inquire
*!*************************************************************
FUNCTION lpInquire
LPARAMETERS loFormSet

IF TYPE("loFormSet") # 'O'
  RETURN
ENDIF

LOCAL lcPOInq, lcSOInq
lcPOInq   = ''
lcSOInq   = ''

DO CASE
  *-- C/T or P/O screen.
  CASE BAR() = 1
    IF loFormSet.lcAllocate = "U"
      lcPOInq = "'" + loFormSet.lcKeyFld + "'"
      oAriaApplication.DoProgram('AWRMFCUTKT', lcPOInq, .F., 'MF')
    ELSE
      lcPOInq = "'P','P','" + loFormSet.lcKeyFld + "'"
      oAriaApplication.DoProgram('AWRPOSTY', lcPOInq, .F., 'PO')
    ENDIF
  *-- Sales order screen.
  CASE BAR() = 2
    lcSOInq = "'O','" + loFormSet.lcCurOrder + "'"
    oAriaApplication.DoProgram('AWRSOORD', lcSOInq, .F., 'SO')
ENDCASE

*!*************************************************************
*! Name      : lpShow
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/29/2004
*! Purpose   : Show function for the whole screen.
*!*************************************************************
FUNCTION lpShow
LPARAMETERS loFormSet

LOCAL lnAlias
lnAlias = SELECT(0)

WITH loFormSet
  SELECT (.lcAlocFile)

  DO CASE
    *-- If select mode, zap the temp. files & refresh the browse.
    CASE .ActiveMode = 'S'
      .lcCurKey   = ""

      *-- Blank all the arrays.
      .laSize    = ''
      .laSize[9] = 0
      .laOrd     = 0

      .lcCurOrder  = ""         && Hold the current order no.
      .lcKeyFld    = SPACE(6)   && Hold the C/T# or P/O#.

      *-- Zap all the temp. files in the current program.
      =lfClearData(loFormSet)

      .AriaForm1.kbPONo.KeyTextBox.Value   = SPACE(6)
      .AriaForm1.kbPONo.KeyTextBox.Enabled = .T.
      .AriaForm1.cboShowOrders.Enabled     = .F.
      .AriaForm1.cboShowOrders.Value       = ' '
      .AriaForm1.grdPOLines.ReadOnly       = .T.
      .AriaForm1.grdSOLines.ReadOnly       = .T.

      *--To prevent calling setfocus if the ChangeMode function is called from
      *--  the Valid method of the PO# Text Box
      IF TYPE(".AriaForm1.ActiveControl") = 'O' AND (UPPER(.AriaForm1.ActiveControl.BaseClass) # 'TEXTBOX')
        .AriaForm1.kbPONo.KeyTextBox.SetFocus()
      ENDIF

    CASE .ActiveMode = 'V'
      *-- Flag to know if the screen's objects has been updated or not.
      .llCUpDate  = .F.

      .AriaForm1.LockScreen = .T.

      .AriaForm1.kbPONo.KeyTextBox.Enabled = .F.
      .AriaForm1.grdPOLines.ReadOnly       = .T.
      .AriaForm1.grdSOLines.ReadOnly       = .T.

      *--Clear Grids Control Sources
      =lfRestoreGrid(.AriaForm1.grdPOLines)
      =lfRestoreGrid(.AriaForm1.grdSOLines)

      *-- Call function "lfGetData" to get the available data from the following files :
      *-- 1- Cutting Ticket Lines File or Style Purchase Orders Lines.
      *-- 2- Sales Orders Lines File.
      *-- 3- Cutting Picking File.
      IF !lfGetData(loFormSet)
        *--Go to Select Mode
        .ChangeMode('S')
      ELSE
        *--Update Cursors' Data to Detect changes when Saving...
        .AriaForm1.cboShowOrders.Enabled = .T.
        .AriaForm1.cboShowOrders.Value   = 'A'
      ENDIF

      *--Restore Grids' Properties
      =lfBuildPOGrid(loFormSet)
      =lfBuildSOGrid(loFormSet)

      SELECT (.lcMainLin)
      LOCATE
      IF !EOF()
        .AriaForm1.grdPOLines.ActivateCell(1,1)
      ENDIF

      .AriaForm1.LockScreen = .F.

      =lfPOLineChange(loFormSet)

    CASE .ActiveMode = 'E'
      .AriaForm1.kbPONo.KeyTextBox.Enabled = .F.
      .AriaForm1.grdPOLines.ReadOnly       = .T.
      .AriaForm1.grdSOLines.ReadOnly       = .T.

      *--Clear Grids Control Sources
      =lfRestoreGrid(.AriaForm1.grdPOLines)

      *-- Call function "lfGetData" to get the available data from the following files :
      *-- 1- Cutting Ticket Lines File or Style Purchase Orders Lines.
      *-- 2- Sales Orders Lines File.
      *-- 3- Cutting Picking File.
      IF !lfGetData(loFormSet)
        *--Go to Select Mode
        .ChangeMode('S')
      ELSE
        *--Update Cursors' Data to Detect changes when Saving...
        .AriaForm1.cboShowOrders.Enabled = .T.
        .AriaForm1.cboShowOrders.Value   = 'A'
      ENDIF

      *--Restore Grids' Properties
      =lfBuildPOGrid(loFormSet)

      SELECT (.lcMainLin)
      LOCATE
      .AriaForm1.grdPOLines.SetFocus()

      =lfPOLineChange(loFormSet)

      .AriaForm1.LockScreen = .F.

      =lfRestoreGrid(.AriaForm1.grdSOLines)
      =lfBuildSOGrid(loFormSet)
  ENDCASE
ENDWITH

*-- Disable the option pad in the select mode.
SET SKIP OF PAD _INQUIRE OF (loFormSet.cHostFormName) TYPE('_SCREEN.ActiveForm.parent.ActiveMode') = 'C' AND _Screen.ActiveForm.Parent.ActiveMode = 'S'

*-- Disable the sales order bar in the option pad if there is no order#.
SET SKIP OF BAR 2 OF _INQUIRE (TYPE('_SCREEN.ActiveForm.Parent.lcCurOrder') = 'C' AND EMPTY(_Screen.ActiveForm.Parent.lcCurOrder))

*-- Select the main file that the navigation works on.
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfGetData
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/29/2004
*! Purpose   : Function to get the available data.
*!*************************************************************
FUNCTION lfGetData
LPARAMETERS loFormSet

LOCAL lcPONo, lcSQLStat, lnCount, lcCount, lnAlias
lcPONo  = loFormSet.AriaForm1.kbPONo.KeyTextBox.Value
lnAlias = SELECT(0)

WITH loFormSet
  *-- If the key change, get the data for the current key.
  *IF .lcCurKey <> .lcAllocate + lcPONo

    *-- Zap all the temp. files to recollect data...
    =lfClearData(loFormSet)

    .lcKeyFld = lcPONo   && Fill with the P/O #.
    .lcCurKey = .lcAllocate + lcPONo


    *--Get PO/CT Lines of the Selecteed PO/CT #
    lcSQLStat = "SELECT * FROM POSLN (INDEX = POSLN)" +;
                " WHERE cBusDocu = 'P' AND " +;
                "       cStyType = '" + .lcAllocate + "' AND " +;
                "       PO = '" + lcPONo + "' AND" +;
                "       TranCd = '1'"
    IF !lfSQLStatement(lcSQLStat, .lcMainLin, 'Style+Dyelot|', 'POSLN|', .F., 5)
      SELECT (lnAlias)
      RETURN .F.
    ENDIF

    *--Collect Cutpick data
    lcSQLStat = "SELECT * FROM CUTPICK (INDEX = CUTPICK)" +;
                " WHERE TranCd = '" + .lcKeyCode + "' AND " +;
                "       cTktNo = '" + .lcKeyFld + "'"
    IF !lfSQLStatement(lcSQLStat, .lcCutPick, 'TRANCD+CTKTNO+cTktLineNo+ORDER+STYLE+CORDLINE|', loFormSet.lcCutPick + '|', .F., 5)
      SELECT (lnAlias)
      RETURN .F.
    ENDIF

    *-- Loop in the main file to get the order lines for each style.
    LOCAL lcOldKey
    lcOldKey = ''

    SELECT (.lcMainLin)
    SCAN
      *--If the Order lines for the same Style/Dyelot have been previously scanned, loop
      IF lcOldKey = Style + Dyelot
        LOOP
      ENDIF
      lcOldKey = Style + Dyelot

      *-- Prepare the order lines temp. file.
      *! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][Start]
*!*	      lcSQLStat = "SELECT * " +;
*!*	                  "  FROM ORDLINE " +;
*!*	                  " WHERE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = '" + EVALUATE(.lcMainLin + '.Style') + "' AND" +;
*!*	                  "       (EMPTY(Dyelot) OR Dyelot = '" + EVALUATE(.lcMainLin + '.Dyelot') + "') AND" +;
*!*	                  "       TotQty <> 0"
      *! B611009,2 MMT 05/31/2015 PO allocation program crashes if PO styles conatins ?[T20150527.0002][Start]
*!*	      lcSQLStat = "SELECT * " +;
*!*	                  "  FROM ORDLINE " +;
*!*	                  " WHERE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = [" + EVALUATE(.lcMainLin + '.Style') + "] AND" +;
*!*	                  "       (EMPTY(Dyelot) OR Dyelot = [" + EVALUATE(.lcMainLin + '.Dyelot') + "]) AND" +;
*!*	                  "       TotQty <> 0"
      lcStyleValue = EVALUATE(.lcMainLin + '.Style') 
      lcSQLStat = "SELECT * " +;
                  "  FROM ORDLINE " +;
                  " WHERE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = ?lcStyleValue  AND" +;
                  "       (EMPTY(Dyelot) OR Dyelot = [" + EVALUATE(.lcMainLin + '.Dyelot') + "]) AND" +;
                  "       TotQty <> 0"
      *! B611009,2 MMT 05/31/2015 PO allocation program crashes if PO styles conatins ?[T20150527.0002][End]                  
      *! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][End]                  
      IF !lfSQLStatement(lcSQLStat, 'TmpOrdLine', 'Style|', 'ORDLINES|', .T.)
        SELECT (lnAlias)
        RETURN .F.
      ENDIF

      SELECT TmpOrdLine
      SET ORDER TO ORDLINES

      *-- Scan all the order lines that contain the current style.
      SCAN
        *-- If the order header status is not "C" or "X" for the current
        *-- order line, include this order line in the order temp file.
        IF .loOrdHdr.Seek('O' + TmpOrdLine.Order, 'OrdHdr') AND !(EVALUATE(.lcOrdHdr + '.Status') $ 'CX')
          IF !SEEK(TmpOrdLine.cOrdType+TmpOrdLine.Order+STR(TmpOrdLine.LineNo,6), .lcOrderLin, .lcOrderLin)
            *-- Insert the current order line in the order temp file.
            SCATTER MEMVAR MEMO
            INSERT INTO (.lcOrderLin) FROM MEMVAR

            SELECT (.lcOrderLin)
            IF !EVALUATE(.lcOrdHdr + '.MultiPo')
              REPLACE CustPo WITH EVALUATE(.lcOrdHdr + '.CustPO')
            ENDIF
          ENDIF
        ENDIF
      ENDSCAN
    ENDSCAN

    *--Clear Temp File
    IF USED('TmpOrdLine')
      USE IN TmpOrdLine
    ENDIF

    *--Get Scale for the first Style in lcMainLin Cursor
    IF .lcAllocate = "U" .AND. !.llExtSizSc
      SELECT (.lcMainLin)
      LOCATE
      *! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][Start]                  
*!*	      lcSQLStat = "SELECT sz1, sz2, sz3, sz4, sz5, sz6, sz7, sz8" +;
*!*	                  "  FROM Scale " +;
*!*	                  " WHERE Type  = 'S' AND " +;
*!*	                  "       Scale = '" + EVALUATE(.lcMainLin + '.Scale') + "'"
      *! B611009,2 MMT 05/31/2015 PO allocation program crashes if PO styles conatins ?[T20150527.0002][Start]
*!*	      lcSQLStat = "SELECT sz1, sz2, sz3, sz4, sz5, sz6, sz7, sz8" +;
*!*	                  "  FROM Scale " +;
*!*	                  " WHERE Type  = 'S' AND " +;
*!*	                  "       Scale = [" + EVALUATE(.lcMainLin + '.Scale') + "]"
      lcScaleValue = EVALUATE(.lcMainLin + '.Scale') 
      lcSQLStat = "SELECT sz1, sz2, sz3, sz4, sz5, sz6, sz7, sz8" +;
                  "  FROM Scale " +;
                  " WHERE Type  = 'S' AND " +;
                  "       Scale = ?lcScaleValue "
      *! B611009,2 MMT 05/31/2015 PO allocation program crashes if PO styles conatins ?[T20150527.0002][End]                  
      *! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][End]                                    
      IF !lfSQLStatement(lcSQLStat, 'TmpStyScale', '', '', .T.)
        SELECT (lnAlias)
        RETURN .F.
      ENDIF

      SELECT TmpStyScale
      LOCATE

      IF !EOF()
        FOR lnCount = 1 TO 8
          lcCount = STR(lnCount,1)
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laBrwQty[lnCount] = IIF(!EMPTY(Sz&lcCount) ,Sz&lcCount, LANG_SOALOCT_SZ + lcCount)
.laBrwQty[lnCount] = IIF(!EMPTY(Sz&lcCount) ,Sz&lcCount, IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SZ,loFormSet.GetHeaderText("LANG_SOALOCT_SZ",loFormSet.HeaderAlias)) + lcCount)
*N000682,1 11/20/2012 MMT Globlization changes[End]

        ENDFOR
      ELSE
        FOR lnCount = 1 TO 8
          lcCount = STR(lnCount,1)
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.laBrwQty[lnCount] = LANG_SOALOCT_SZ + lcCount
.laBrwQty[lnCount] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SZ,loFormSet.GetHeaderText("LANG_SOALOCT_SZ",loFormSet.HeaderAlias)) + lcCount
*N000682,1 11/20/2012 MMT Globlization changes[End]

        ENDFOR
      ENDIF

      *--Clear Temp File
      USE IN TmpStyScale
    ENDIF

    SELECT(.lcAlocFile)
    =TABLEUPDATE(.T.)

    SELECT (.lcOrderLin)
    =TABLEUPDATE(.T.)

    SELECT (.lcCutPick)
    =TABLEUPDATE(.T.)

    .loOrdHdr.TableUpdate()

    SELECT (.lcMainLin)
    =TABLEUPDATE(.T.)
    LOCATE
  *ENDIF
ENDWITH

RETURN .T.

*!*************************************************************
*! Name      : lfPOLineChange
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/05/2004
*! Purpose   : Valid function for the PO browse.
*!*************************************************************
*! Example   : =lfPOLineChange()
*!*************************************************************
FUNCTION lfPOLineChange
LPARAMETERS loFormSet, llNotRefreshSO

LOCAL lnCount, lcCount, lcSQLStat, lnRemResult, lnAlias
lnAlias = SELECT(0)

SELECT (loFormSet.lcMainLin)
*! B610485,1 HIA 08/22/13 T20130808.0001 - PO - error in Style PO Allocation screen [Begin]
*!*	lcSQLStat = "SELECT sz1, sz2, sz3, sz4, sz5, sz6, sz7, sz8, Cnt" +;
*!*	            "  FROM Scale " +;
*!*	            " WHERE Type  = 'S' AND " +;
*!*	            "       Scale = '" + Scale + "'"
*! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][Start]                  
*!*	lcSQLStat = "SELECT sz1, sz2, sz3, sz4, sz5, sz6, sz7, sz8, Cnt" +;
*!*	            "  FROM Scale " +;
*!*	            " WHERE Type  = 'S' AND " +;
*!*	            "       Scale = '" + STRTRAN(Scale, "'", "''") + "'"
*! B611009,2 MMT 05/31/2015 PO allocation program crashes if PO styles conatins ?[T20150527.0002][Start]
*!*	lcSQLStat = "SELECT sz1, sz2, sz3, sz4, sz5, sz6, sz7, sz8, Cnt" +;
*!*	            "  FROM Scale " +;
*!*	            " WHERE Type  = 'S' AND " +;
*!*	            "       Scale = [" + Scale+ "]"
lcScaleValue = Scale
lcSQLStat = "SELECT sz1, sz2, sz3, sz4, sz5, sz6, sz7, sz8, Cnt" +;
            "  FROM Scale " +;
            " WHERE Type  = 'S' AND " +;
            "       Scale = ?lcScaleValue "
*! B611009,2 MMT 05/31/2015 PO allocation program crashes if PO styles conatins ?[T20150527.0002][End]            
*! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][End]                              
*! B610485,1 HIA 08/22/13 T20130808.0001 - PO - error in Style PO Allocation screen [End]
            
IF !lfSQLStatement(lcSQLStat, 'TmpStyScale', '', '', .T.)
  SELECT (lnAlias)
  RETURN .F.
ENDIF

SELECT TmpStyScale
IF !EOF()
  FOR lnCount = 1 TO 8
    lcCount = ALLTRIM(STR(lnCount))
    loFormSet.laSize[lnCount] = Sz&lcCount
  ENDFOR

  *--loFormSet.laSize[9] is the Size Count
  loFormSet.laSize[9] = cnt
ELSE
  STORE "" TO laSize

  *--loFormSet.laSize[9] is the Size Count
  loFormSet.laSize[9] = 0
ENDIF

*--Clear Temp file
USE IN TmpStyScale

IF !llNotRefreshSO
  *--Activate First row in the SO Grid
  SELECT (loFormSet.lcOrderLin)
  LOCATE FOR Style = EVALUATE(loFormSet.lcMainLin + '.Style') AND (EMPTY(Dyelot) OR Dyelot = EVALUATE(loFormSet.lcMainLin + '.Dyelot'))
ENDIF

=lfRefreshButtons(loFormSet)

IF !llNotRefreshSO
  *--Activate First row in the SO Grid
  SELECT (loFormSet.lcOrderLin)
  LOCATE FOR Style = EVALUATE(loFormSet.lcMainLin + '.Style') AND (EMPTY(Dyelot) OR Dyelot = EVALUATE(loFormSet.lcMainLin + '.Dyelot'))

  IF FOUND()
    loFormSet.AriaForm1.grdSOLines.ActivateCell(1,1)
  ENDIF
ENDIF

SELECT(lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfSOLineChange
*! Developer : Wael M. ABO-Shawareb (WSH)
*! Date      : 12/05/2004
*! Purpose   : Valid function for the order lines browse.
*!*************************************************************
*! Example   : =lfSOLineChange()
*!*************************************************************
FUNCTION lfSOLineChange
LPARAMETERS loFormSet

LOCAL lnAlias
lnAlias = SELECT(0)

=lfRefreshButtons(loFormSet)

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfRefreshButtons
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/05/2004
*! Purpose   : Refresh Form Buttons
*!*************************************************************
*! Example   : =lfRefreshButtons()
*!*************************************************************
FUNCTION lfRefreshButtons
LPARAMETERS loFormSet

LOCAL lnCount, lcCount, lnAlias
lnAlias = SELECT(0)

WITH loFormSet
  .AriaForm1.LockScreen = .T.

  .lcCurOrder = EVALUATE(loFormSet.lcOrderLin + '.Order')

  *--Assign Current Order Text Box Value
  .AriaForm1.txtOrder.Value = .lcCurOrder

  *--Restore Ordered and Available field values
  .AriaForm1.cntOrdered.mEmptyAll()
  .AriaForm1.cntAvailable.mEmptyAll()

  *--Display Sizes Labels
  FOR lnCount = 1 TO 8
    lcCount = ALLTRIM(STR(lnCount))
    .AriaForm1.cntOrdered.txtSizeLbl&lcCount..Value = .laSize[lnCount]
  ENDFOR

  *-- Display the available qty. from the order lines.
  IF !EMPTY(EVALUATE(.lcOrderLin + '.Order'))
    FOR lnCount = 1 TO 8
      lcCount = ALLTRIM(STR(lnCount))
      .AriaForm1.cntAvailable.txtQty&lcCount..Value = EVALUATE(.lcOrderLin + '.Qty' + lcCount) - EVALUATE(.lcOrderLin + '.Cut' + lcCount)
    ENDFOR
  ELSE
    FOR lnCount = 1 TO 8
      lcCount = ALLTRIM(STR(lnCount))
      .AriaForm1.cntAvailable.txtQty&lcCount..Value = 0
    ENDFOR
  ENDIF
  .AriaForm1.cntAvailable.mGetTotal()

  *-- Enable the release all button if the total order qty. greater than zero.
  LOCAL lnTotCut, lnOldRec
  lnOldRec = RECNO(.lcOrderLin)

  SELECT (.lcCutPick)
  =SEEK(.lcKeyCode + .lcKeyFld + STR(EVALUATE(.lcMainLin + '.LineNo'),6))
  SUM TotQty REST ;
      WHILE TRANCD+CTKTNO+cTktLineNo+ORDER+STYLE+CORDLINE = .lcKeyCode + .lcKeyFld + STR(EVALUATE(.lcMainLin + '.LineNo'),6) ;
      FOR SEEK('O' + Order + CORDLINE, .lcOrderLin, .lcOrderLin) ;
      TO lnTotCut

  IF lnOldRec <> 0 AND lnOldRec <= RECCOUNT(.lcOrderLin)
    GOTO lnOldRec IN .lcOrderLin
  ENDIF

  IF lnTotCut <> 0
    .AriaForm1.cmdReleaseAll.Enabled = (.ActiveMode = 'E')
  ELSE
    .AriaForm1.cmdReleaseAll.Enabled = .F.
  ENDIF

  *-- Enable the release button & the order button to modify the qty.
  *-- if the total order qty. greater than zero & record exist in the
  *-- temp. cutpick file.
  LOCAL llFoundCut

  SELECT (.lcCutPick)
  llFoundCut = SEEK(.lcKeyCode + .lcKeyFld + STR(EVALUATE(.lcMainLin + '.LineNo'),6) + EVALUATE(.lcOrderLin + '.Order') + EVALUATE(.lcOrderLin + '.Style') + STR(EVALUATE(.lcOrderLin + '.LineNo'),6), .lcCutPick)

  SELECT (.lcMainLin)

  STORE 0 TO .laOrd
  IF TotOrd <> 0 AND llFoundCut
    IF EVALUATE(.lcOrderLin + '.TotCut') = 0
      .AriaForm1.cmdRelease.Enabled = .F.
      .AriaForm1.cmdOrder.Enabled   = .F.
    ELSE
      .AriaForm1.cmdRelease.Enabled = (.ActiveMode = 'E')
      .AriaForm1.cmdOrder.Enabled   = (.ActiveMode = 'E')

      SELECT (.lcCutPick)
      FOR lnCount = 1 TO 8
        lcCount = ALLTRIM(STR(lnCount))
        .laOrd[lnCount] = Qty&lcCount
        .AriaForm1.cntOrdered.txtQty&lcCount..Value = .laOrd[lnCount]
        .laOrd[9] = .laOrd[9] + .laOrd[lnCount]
      ENDFOR
    ENDIF
  ELSE
    .AriaForm1.cmdRelease.Enabled = .F.
    .AriaForm1.cmdOrder.Enabled   = .F.
  ENDIF
  .AriaForm1.cntOrdered.mGetTotal()

  SELECT (.lcMainLin)

  *-- Check if there is available qty. to be allocated, enable the link buuton.
  IF .ActiveMode = 'E'
    LOCAL llEnable
    llEnable = .F.

    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      llEnable = (EVALUATE(.lcMainLin + '.Qty' + lcCount) - EVALUATE(.lcMainLin + '.Ord' + lcCount) <> 0) AND;
                 (EVALUATE(.lcOrderLin + '.Qty' + lcCount) - EVALUATE(.lcOrderLin + '.Cut' + lcCount) <> 0)
      IF llEnable
        EXIT
      ENDIF
    ENDFOR

    .AriaForm1.cmdLink.Enabled = llEnable
  ELSE
    .AriaForm1.cmdLink.Enabled = .F.
  ENDIF

  *-- Disable the sales order bar if the there is no order.
  SET SKIP OF BAR 2 OF _INQUIRE (TYPE('_SCREEN.ActiveForm.Parent.lcCurOrder') = 'C' AND EMPTY(_Screen.ActiveForm.Parent.lcCurOrder))

  .AriaForm1.LockScreen = .F.
  .Refresh()
ENDWITH  && loFormSet

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfvRel
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/05/2004
*! Purpose   : Valid function for the push button <RELEASE>
*!*************************************************************
*! Example   : =lfvRel()
*!*************************************************************
FUNCTION lfvRel
LPARAMETERS loFormSet

*** Remove relation between the current   ***
*** {IIF(lcAllocate="C" , "C/T" , "P/O")} ***
*** line and the current order line?      ***
*** < Yes > - < No > ***
LOCAL lcSQLStat, lnRemResult, llRelMulSt, lnAlias
lnAlias = SELECT(0)

loFormSet.llMultiStr = .F.

lcSQLStat = "SELECT Multi" +;
            "  FROM ORDHDR " +;
            " WHERE CORDTYPE+ORDER = 'O" + EVALUATE(loFormSet.lcOrderLin + '.Order') + "'"
IF !lfSQLStatement(lcSQLStat, 'TmpOrdHdr', '', '', .T.)
  SELECT (lnAlias)
  RETURN .F.
ENDIF

SELECT TmpOrdHdr
LOCATE

IF !EOF()
  IF ALLTRIM(TmpOrdHdr.Multi) = 'Y'

    *-- Check if this Order include same style in Multi stores
    =lfChkMultS(loFormSet)
    IF loFormSet.llMultiStr
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*llRelMulSt = (gfModalGen("INM34176B34001", "DIALOG", LANG_SOALOCT_RELLINK) = 1)
llRelMulSt = (gfModalGen("INM34176B34001", "DIALOG", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_RELLINK,loFormSet.GetHeaderText("LANG_SOALOCT_RELLINK",loFormSet.HeaderAlias))) = 1)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    ENDIF
  ENDIF
ENDIF
USE IN TmpOrdHdr

*-- IF user want to release all stores
IF llRelMulSt
  loFormSet.lcCurOrder = EVALUATE(loFormSet.lcOrderLin + '.Order')

  SELECT (loFormSet.lcCutPick)
  LOCATE

  *-- Loop in all the cutpick file lines to release the relation between
  *-- them & the related order lines.
  SCAN FOR Style = EVALUATE(loFormSet.lcMainLin + '.Style') AND Order = loFormSet.lcCurOrder
    IF SEEK("O" + Order + cOrdLine, loFormSet.lcOrderLin, loFormSet.lcOrderLin)

      *-- Call function to release the relation for the current C/T or
      *-- P/O line and the current order line.
      =lfRelLin(loFormSet)
    ENDIF
  ENDSCAN

  *-- Refresh PO Grid to refresh the the objects
  *-- in the screen after changing.

  =lfPOLineChange(loFormSet)
  loFormSet.AriaForm1.grdSOLines.SetFocus()
  loFormSet.AriaForm1.grdSOLinesToolBar.RefreshGrids = .T.
ELSE
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF gfModalGen("INM32050B00006" , "DIALOG", IIF(loFormSet.lcAllocate = "U", LANG_SOALOCT_SHORTCT, LANG_SOALOCT_SHORTPO)) = 1
IF gfModalGen("INM32050B00006" , "DIALOG", IIF(loFormSet.lcAllocate = "U", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SHORTCT,loFormSet.GetHeaderText("LANG_SOALOCT_SHORTCT",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SHORTPO,loFormSet.GetHeaderText("LANG_SOALOCT_SHORTPO",loFormSet.HeaderAlias)))) = 1
*N000682,1 11/20/2012 MMT Globlization changes[End]

    SELECT (loFormSet.lcOrderLin)

    *-- Call function to release the relation for the current C/T or
    *-- P/O line and the current order line.
    =SEEK(loFormSet.lcKeyCode + loFormSet.lcKeyFld + STR(EVALUATE(loFormSet.lcMainLin + '.LineNo'), 6) + EVALUATE(loFormSet.lcOrderLin + '.Order')+EVALUATE(loFormSet.lcOrderLin + '.Style') + STR(EVALUATE(loFormSet.lcOrderLin + '.LineNo'), 6), loFormSet.lcCutPick)
    =lfRelLin(loFormSet)

    *-- Refresh SO Grid to refresh the the objects
    *-- in the screen after changing.
    =lfPOLineChange(loFormSet, .T.)

    loFormSet.AriaForm1.grdSOLines.SetFocus()
    loFormSet.AriaForm1.grdSOLinesToolBar.RefreshGrids = .T.
  ENDIF
ENDIF

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfvRelAll
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/05/2004
*! Purpose   : Valid function for the push button <RELEASE ALL>
*!*************************************************************
*! Example   : =lfvRelAll()
*!*************************************************************
FUNCTION lfvRelAll
LPARAMETERS loFormSet

LOCAL lnAlias
lnAlias = SELECT(0)

*** Remove relation between the current   ***
*** {IIF(lcAllocate="C" , "C/T" , "P/O")} ***
*** line and all order lines? ***
*** < Yes > - < No > ***
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF gfModalGen("QRM32051B00006", "DIALOG", IIF(loFormSet.lcAllocate = "U", LANG_SOALOCT_SHORTCT, LANG_SOALOCT_SHORTPO)) = 1
IF gfModalGen("QRM32051B00006", "DIALOG", IIF(loFormSet.lcAllocate = "U", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SHORTCT,loFormSet.GetHeaderText("LANG_SOALOCT_SHORTCT",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SHORTPO,loFormSet.GetHeaderText("LANG_SOALOCT_SHORTPO",loFormSet.HeaderAlias)))) = 1
*N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.AriaForm1.LockScreen = .T.

  SELECT (loFormSet.lcOrderLin)

  LOCAL lcOldFilter
  lcOldFilter = FILTER()

  SET FILTER TO

  SELECT (loFormSet.lcCutPick)
  LOCATE

  *-- Loop in all the cutpick file lines to release the relation between
  *-- them & the related order lines.
  SCAN FOR Style = EVALUATE(loFormSet.lcMainLin + '.Style')
    IF SEEK("O" + Order + cOrdLine, loFormSet.lcOrderLin, loFormSet.lcOrderLin)
      *-- Call function to release the relation for the current C/T or
      *-- P/O line and the current order line.
      =lfRelLin(loFormSet)
    ENDIF
  ENDSCAN

  SELECT (loFormSet.lcOrderLin)
  SET FILTER TO &lcOldFilter

  loFormSet.AriaForm1.LockScreen = .F.

  *-- Call the main browse when function to refresh the the objects
  *-- in the screen after changing.
  =lfPOLineChange(loFormSet)
  loFormSet.AriaForm1.grdSOLines.SetFocus()
  loFormSet.AriaForm1.grdSOLinesToolBar.RefreshGrids = .T.
ENDIF

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfRelLin
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/05/2004
*! Purpose   : Function to release the current c/t or p/o line
*!           : and the current order line.
*! Example   : =lfRelLin()
*!*************************************************************
FUNCTION lfRelLin
LPARAMETERS loFormSet, lnSelPrice

LOCAL lnI, lcI, lcSqlStat, lnRemResult, lnTotCost, lnTotQty, lnoldqty, lnGrosMrgn, lnRotSub
lnTotQty = 0

*-- The screen's objects has been updated.
loFormSet.llCUpDate  = .T.

IF loFormSet.llDispPric
  lnOldQty = 0
  For lnI = 1 To 8
    lcI = ALLTRIM(STR(lnI))
    lnOldQty = lnOldqty + EVALUATE(loFormSet.lcMainLin + '.Ord' + lcI)
  ENDFOR
  lnSelPrice = EVALUATE(loFormSet.lcMainLin + '.nSelPrice')  && selling price before release relation
ENDIF

*-- Update the C/T or P/O lines with the new qty.
SELECT (loFormSet.lcMainLin)
REPLACE Ord1    WITH MAX(Ord1   - EVALUATE(loFormSet.lcCutPick + '.Qty1'), 0),;
        Ord2    WITH MAX(Ord2   - EVALUATE(loFormSet.lcCutPick + '.Qty2'), 0),;
        Ord3    WITH MAX(Ord3   - EVALUATE(loFormSet.lcCutPick + '.Qty3'), 0),;
        Ord4    WITH MAX(Ord4   - EVALUATE(loFormSet.lcCutPick + '.Qty4'), 0),;
        Ord5    WITH MAX(Ord5   - EVALUATE(loFormSet.lcCutPick + '.Qty5'), 0),;
        Ord6    WITH MAX(Ord6   - EVALUATE(loFormSet.lcCutPick + '.Qty6'), 0),;
        Ord7    WITH MAX(Ord7   - EVALUATE(loFormSet.lcCutPick + '.Qty7'), 0),;
        Ord8    WITH MAX(Ord8   - EVALUATE(loFormSet.lcCutPick + '.Qty8'), 0),;
        TotOrd  WITH MAX(TotOrd - EVALUATE(loFormSet.lcCutPick + '.TotQty'), 0)

IF loFormSet.llDispPric
  STORE 0 TO lnRotSub, lnGrosMrgn

  For lnI = 1 To 8
    lcI = ALLTRIM(STR(lnI))
    lnTotQty = lnTotQty + EVALUATE(loFormSet.lcMainLin + '.Ord'  + lcI)
  ENDFOR

  lnTotQty   = lnOldQty - lnTotQty
  lnSelPrice = IIF(lnTotQty = 0, 0, ((lnOldQty * lnSelPrice) - (lnTotQty * EVALUATE(loFormSet.lcOrderLin + '.Price'))) / lnTotQty)
  *! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][Start]                              
*!*	  lcSQLStat = "SELECT PriceA, TotCost" +;
*!*	              "  FROM Style " +;
*!*	              " WHERE Style = '" + EVALUATE(loFormSet.lcMainLin + '.Style') + "'"
  *! B611009,2 MMT 05/31/2015 PO allocation program crashes if PO styles conatins ?[T20150527.0002][Start]            
*!*	  lcSQLStat = "SELECT PriceA, TotCost" +;
*!*	              "  FROM Style " +;
*!*	              " WHERE Style = [" + EVALUATE(loFormSet.lcMainLin + '.Style') + "]"
  lcStyleValue = EVALUATE(loFormSet.lcMainLin + '.Style') 
  lcSQLStat = "SELECT PriceA, TotCost" +;
              "  FROM Style " +;
              " WHERE Style = ?lcStyleValue "
  *! B611009,2 MMT 05/31/2015 PO allocation program crashes if PO styles conatins ?[T20150527.0002][End]                          
  *! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][End]                                            
  IF !lfSQLStatement(lcSQLStat, 'TmpStyle', '', '', .T.)
    RETURN .F.
  ENDIF

  SELECT TmpStyle
  LOCATE

  IF lnSelPrice = 0
    lnSelPrice = TmpStyle.PriceA
  ENDIF

  SELECT (loFormSet.lcMainLin)
  IF loFormSet.lcAllocate = 'P'
    lnTotCost = nICost1 + nICost2 + nICost3 + nICost4 + nICost5
  ELSE
    lnTotCost = TmpStyle.TOTCOST
  ENDIF

  lnRotSub   = IIF(loFormSet.llStyMark, lnTotCost, lnSelPrice)
  lnGrosMrgn = IIF(lnRotSub = 0, 0,((lnSelPrice - lnTotCost) / lnRotSub) * 100)

  REPLACE nSelPrice WITH lnSelPrice,;
          nGrosMrgn WITH lnGrosMrgn

  USE IN TmpStyle
ENDIF
=gfAdd_info(loFormSet.lcMainLin)

*-- Subtract the cutpick qty. from the order line.
SELECT (loFormSet.lcOrderLin)
REPLACE Cut1   WITH MAX(Cut1   - EVALUATE(loFormSet.lcCutPick + '.Qty1'), 0),;
        Cut2   WITH MAX(Cut2   - EVALUATE(loFormSet.lcCutPick + '.Qty2'), 0),;
        Cut3   WITH MAX(Cut3   - EVALUATE(loFormSet.lcCutPick + '.Qty3'), 0),;
        Cut4   WITH MAX(Cut4   - EVALUATE(loFormSet.lcCutPick + '.Qty4'), 0),;
        Cut5   WITH MAX(Cut5   - EVALUATE(loFormSet.lcCutPick + '.Qty5'), 0),;
        Cut6   WITH MAX(Cut6   - EVALUATE(loFormSet.lcCutPick + '.Qty6'), 0),;
        Cut7   WITH MAX(Cut7   - EVALUATE(loFormSet.lcCutPick + '.Qty7'), 0),;
        Cut8   WITH MAX(Cut8   - EVALUATE(loFormSet.lcCutPick + '.Qty8'), 0),;
        TotCut WITH MAX(TotCut - EVALUATE(loFormSet.lcCutPick + '.TotQty'), 0)

=gfAdd_info(loFormSet.lcOrderLin)

*-- Blank the qty. in the cutpick file & delete the record.
SELECT (loFormSet.lcCutPick)
REPLACE QTY1   WITH 0,;
        QTY2   WITH 0,;
        QTY3   WITH 0,;
        QTY4   WITH 0,;
        QTY5   WITH 0,;
        QTY6   WITH 0,;
        QTY7   WITH 0,;
        QTY8   WITH 0,;
        TOTQTY WITH 0
DELETE

*!*************************************************************
*! Name      : lfvLink
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/06/2004
*! Purpose   : Valid function for the push button <LINK>
*!*************************************************************
*! Example   : =lfvLink()
*!*************************************************************
FUNCTION lfvLink
LPARAMETERS loFormSet

LOCAL llStyDye, lnI, lcI, lnAlias
lnAlias  = SELECT(0)

loFormSet.llRepDye = .F.    && Flag to know if replace cyurrent order line with the current CT/PO line.

*-- If system is set to use dyelot.
IF loFormSet.lcDyeConfig # 'N'

  *-- Seek to know if the current style is dyelot yes or not.
  llStyDye = .F.
  *! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][Start]                              
*!*	  lcSQLStat = "SELECT cdye_flg" +;
*!*	              "  FROM Style " +;
*!*	              " WHERE Style = '" + EVALUATE(loFormSet.lcMainLin + '.Style') + "'"
  *! B611009,2 MMT 05/31/2015 PO allocation program crashes if PO styles conatins ?[T20150527.0002][Start]            
*!*	  lcSQLStat = "SELECT cdye_flg" +;
*!*	              "  FROM Style " +;
*!*	              " WHERE Style = [" + EVALUATE(loFormSet.lcMainLin + '.Style') + "]"
  lcStyleValue =EVALUATE(loFormSet.lcMainLin + '.Style') 
  lcSQLStat = "SELECT cdye_flg" +;
              "  FROM Style " +;
              " WHERE Style = ?lcStyleValue "
  *! B611009,2 MMT 05/31/2015 PO allocation program crashes if PO styles conatins ?[T20150527.0002][End]                          
  *! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][End]                                            
  IF !lfSQLStatement(lcSQLStat, 'TmpStyle', '', '', .T.)
    SELECT (lnAlias)
    RETURN .F.
  ENDIF

  SELECT TmpStyle
  LOCATE

  IF !EOF()
    llStyDye = (TmpStyle.cdye_flg = "Y")
  ENDIF

  *-- If the current style use dyelots.
  IF llStyDye
    DO CASE

      *-- If there is dyelot in CT/PO line & there is no dyelot in the order line,
      *-- Allocate & replace the CT/PO line dyelot in the order line dyelot.
      CASE !EMPTY(EVALUATE(loFormSet.lcMainLin + '.Dyelot')) .AND. EMPTY(EVALUATE(loFormSet.lcOrderLin + '.Dyelot'))
        loFormSet.llRepDye = .T.

      *-- If there is dyelot in both CT/PO line & order line.
      CASE !EMPTY(EVALUATE(loFormSet.lcMainLin + '.Dyelot')) .AND. !EMPTY(EVALUATE(loFormSet.lcOrderLin + '.Dyelot'))

        *-- WSH... They will never be different as long as we filter
        *--        order lines with the same or empty dyelot as PO/CT line.

        *-- If both CT/PO line & order line dyelots are different, Cannot allocate.
        IF EVALUATE(loFormSet.lcMainLin + '.Dyelot') <> EVALUATE(loFormSet.lcOrderLin + '.Dyelot')

          *** {IIF(lcAllocate="C" , "C/T" , "P/O")} line and the current ***
          *** order line have different dyelots. Cannot allocate!        ***
          *** <  OK  > ***
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("INM32064B00000", "DIALOG", IIF(loFormSet.lcAllocate = "U", LANG_SOALOCT_SHORTCT, LANG_SOALOCT_SHORTPO))
=gfModalGen("INM32064B00000", "DIALOG", IIF(loFormSet.lcAllocate = "U", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SHORTCT,loFormSet.GetHeaderText("LANG_SOALOCT_SHORTCT",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SHORTPO,loFormSet.GetHeaderText("LANG_SOALOCT_SHORTPO",loFormSet.HeaderAlias))))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          SELECT (lnAlias)
          RETURN .F.
        ELSE

          *-- If both CT/PO line & order line dyelots are the same, check for any
          *-- other previously allocated CT/PO line.
          *! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][Start]                              
*!*	          lcSQLStat = "SELECT CUTPICK.cTktNo, POSLN.Dyelot" +;
*!*	                      "  FROM CUTPICK (INDEX = CUTORD)" +;
*!*	                      " INNER JOIN POSLN (INDEX = POSLN)" +;
*!*	                      "    ON POSLN.cBusDocu = 'P' AND" +;
*!*	                      "       POSLN.cStyType = '" + loFormSet.lcAllocate + "' AND" +;
*!*	                      "       POSLN.PO = CUTPICK.cTktNo" +;
*!*	                      " WHERE CUTPICK.[Order]  = '" + EVALUATE(loFormSet.lcOrderLin + '.Order') + "' AND " +;
*!*	                      "       CUTPICK.cOrdLine = "  + STR(EVALUATE(loFormSet.lcOrderLin + '.LineNo')) + " AND" +;
*!*	                      "       CUTPICK.Style    = '" + EVALUATE(loFormSet.lcOrderLin + '.Style') + "' AND" +;
*!*	                      "       CUTPICK.ctktno  <> '" + loFormSet.lcKeyFld + "'"
          lcSQLStat = "SELECT CUTPICK.cTktNo, POSLN.Dyelot" +;
                      "  FROM CUTPICK (INDEX = CUTORD)" +;
                      " INNER JOIN POSLN (INDEX = POSLN)" +;
                      "    ON POSLN.cBusDocu = 'P' AND" +;
                      "       POSLN.cStyType = '" + loFormSet.lcAllocate + "' AND" +;
                      "       POSLN.PO = CUTPICK.cTktNo" +;
                      " WHERE CUTPICK.[Order]  = '" + EVALUATE(loFormSet.lcOrderLin + '.Order') + "' AND " +;
                      "       CUTPICK.cOrdLine = "  + STR(EVALUATE(loFormSet.lcOrderLin + '.LineNo')) + " AND" +;
                      "       CUTPICK.Style    = '" + STRTRAN(EVALUATE(loFormSet.lcOrderLin + '.Style'),"'","''") + "' AND" +;
                      "       CUTPICK.ctktno  <> '" + loFormSet.lcKeyFld + "'"
          *! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][End]                                                    
          IF !lfSQLStatement(lcSQLStat, 'TmpCutPick', '', '')
            SELECT (lnAlias)
            RETURN .F.
          ENDIF

          SELECT TmpCutPick

          *-- If the current order line has been allocated befor to another CT/PO
          *-- line has different line, do not allocate.
          LOCATE FOR TmpCutPick.Dyelot <> EVALUATE(loFormSet.lcMainLin + '.Dyelot')

          IF FOUND()
            *** Current order line have been allocated befor to another        ***
            *** IIF(lcAllocate="C" , "C/T" , "P/O") that has different dyelot. ***
            *** Cannot Allocate! ***
            *** < Ok > ***
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("INM32065B00000", "DIALOG", IIF(loFormSet.lcAllocate = "U" , LANG_SOALOCT_SHORTCT, LANG_SOALOCT_SHORTPO))
=gfModalGen("INM32065B00000", "DIALOG", IIF(loFormSet.lcAllocate = "U" , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SHORTCT,loFormSet.GetHeaderText("LANG_SOALOCT_SHORTCT",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SHORTPO,loFormSet.GetHeaderText("LANG_SOALOCT_SHORTPO",loFormSet.HeaderAlias))))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            SELECT (lnAlias)
            RETURN .F.
          ENDIF

          *-- If current order line has been allocated befor to another CT/PO line
          *-- has the same dyelot, warn the user to allocate or cancel.
          LOCATE FOR TmpCutPick.Dyelot == EVALUATE(loFormSet.lcMainLin + '.Dyelot')

          IF FOUND()
            *** Current order line have been allocated befor to another       ***
            *** IIF(lcAllocate="C" , "C/T" , "P/O") that has the same dyelot. ***
            *** < Allocate > - < Cancel > ***
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF gfModalGen("INM32066B32007", "DIALOG", IIF(loFormSet.lcAllocate = "U" , LANG_SOALOCT_SHORTCT, LANG_SOALOCT_SHORTPO)) = 2
IF gfModalGen("INM32066B32007", "DIALOG", IIF(loFormSet.lcAllocate = "U" , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SHORTCT,loFormSet.GetHeaderText("LANG_SOALOCT_SHORTCT",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SHORTPO,loFormSet.GetHeaderText("LANG_SOALOCT_SHORTPO",loFormSet.HeaderAlias)))) = 2
*N000682,1 11/20/2012 MMT Globlization changes[End]

              SELECT (lnAlias)
              RETURN .F.
            ENDIF
          ENDIF

          USE IN TmpCutPick
        ENDIF
    ENDCASE
  ENDIF
  SELECT (loFormSet.lcAlocFile)
ENDIF

*-- The screen's objects has been updated.
loFormSet.llCUpDate  = .T.

*-- Seek for the current line in the temp cutpick file.
SELECT (loFormSet.lcCutPick)
=SEEK(loFormSet.lcKeyCode + loFormSet.lcKeyFld + STR(EVALUATE(loFormSet.lcMainLin + '.LineNo'), 6) + EVALUATE(loFormSet.lcOrderLin + '.Order')+EVALUATE(loFormSet.lcOrderLin + '.Style') + STR(EVALUATE(loFormSet.lcOrderLin + '.LineNo'), 6), loFormSet.lcCutPick)
=ACOPY(loFormSet.laOrd, loFormSet.laOldLaOrd)

loFormSet.llMultiStr =  .F.

STORE 0 TO loFormSet.laOrd

lcSQLStat = "SELECT Multi" +;
            "  FROM ORDHDR " +;
            " WHERE CORDTYPE+ORDER = 'O" + EVALUATE(loFormSet.lcOrderLin + '.Order') + "'"
IF !lfSQLStatement(lcSQLStat, 'TmpOrdHdr', '', '', .T.)
  SELECT (lnAlias)
  RETURN .F.
ENDIF

SELECT TmpOrdHdr
LOCATE

IF !EOF()
  IF ALLTRIM(TmpOrdHdr.Multi) = 'Y'
    *-- Check if this Order include same style in Multi stores
    = lfChkMultS(loFormSet)
    IF loFormSet.llMultiStr
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.llAloMulSt = IIF(gfModalGen("INM34176B34001", "DIALOG", LANG_SOALOCT_MAKEALO) = 1, .T., .F.)
loFormSet.llAloMulSt = IIF(gfModalGen("INM34176B34001", "DIALOG", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_MAKEALO,loFormSet.GetHeaderText("LANG_SOALOCT_MAKEALO",loFormSet.HeaderAlias))) = 1, .T., .F.)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    ENDIF
  ENDIF
ENDIF

*--Clear temp file
USE IN TmpOrdHdr

*-- If user want to allocate all stores , Collect the Qty and the Cut of all lines for this order
IF loFormSet.llAloMulSt
  LOCAL lnOldRec, lcOrder, lcStyle, lnOldOrder

  SELECT (loFormSet.lcOrderLin)
  lnOldRec   = RECNO()
  lnOldOrder = ORDER()
  lcOrder    = Order
  lcStyle    = Style

  SET ORDER TO (loFormSet.lcOrderLin)
  =SEEK('O'+lcOrder)
  SCAN REST WHILE cOrdType + order + STR(LineNo,6) = 'O'+ lcOrder;
            FOR Style = lcStyle
    *-- If this line line is completely allocated , ignor it
    IF TotCut = TotQty
      LOOP
    ENDIF  && End If this line line is completely allocated , ignor it

    FOR lnI = 1 TO 8
      lcI = ALLTRIM(STR(lnI))
      IF Cut&lcI = 0 OR Qty&lcI - Cut&lcI <> 0
        loFormSet.laStrQty[lnI] = loFormSet.laStrQty[lnI] + IIF(Cut&lcI = 0, Qty&lcI, (Qty&lcI - Cut&lcI))
        loFormSet.laStrCut[lnI] = loFormSet.laStrCut[lnI] + Cut&lcI
      ENDIF
    ENDFOR
    IF TotCut = 0 OR TotQty - TotCut <> 0
      loFormSet.laStrQty[9] = loFormSet.laStrQty[9] + IIF(TotCut = 0, TotQty, (TotQty - TotCut))
      loFormSet.laStrCut[9] = loFormSet.laStrCut[9] + TotCut
    ENDIF
  ENDSCAN

  *--Restore Old Order Tag
  SELECT (loFormSet.lcOrderLin)
  SET ORDER TO (lnOldOrder)

  IF BETWEEN(lnOldRec, 1 , RECCOUNT())
    GOTO lnOldRec
  ENDIF
ENDIF

*-- Update the cutpick file with the modified qty.
FOR lnI = 1 TO 8
  lcI = ALLTRIM(STR(lnI))
  loFormSet.laOrd[lnI] = loFormSet.laOrd[lnI] + MIN(IIF(loFormSet.llAloMulSt, (loFormSet.laStrQty[lnI]),;
                             (EVALUATE(loFormSet.lcOrderLin + '.Qty' + lcI) - EVALUATE(loFormSet.lcOrderLin + '.Cut' + lcI))),;
                             (EVALUATE(loFormSet.lcMainLin + '.Qty' + lcI) - EVALUATE(loFormSet.lcMainLin + '.Ord' + lcI)))
  loFormSet.laOrd[9]   = loFormSet.laOrd[9] + loFormSet.laOrd[lnI]
ENDFOR

LOCAL lcModTitle
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcModTitle = LANG_SOALOCT_CSTEDTTTL1 + loFormSet.lcStyHdr + ' :' + EVALUATE(loFormSet.lcOrderLin + '.Style') + LANG_SOALOCT_CSTEDTTTL2 + loFormSet.lcCurOrder
lcModTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_CSTEDTTTL1,loFormSet.GetHeaderText("LANG_SOALOCT_CSTEDTTTL1",loFormSet.HeaderAlias)) + loFormSet.lcStyHdr + ' :' + EVALUATE(loFormSet.lcOrderLin + '.Style') + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_CSTEDTTTL2,loFormSet.GetHeaderText("LANG_SOALOCT_CSTEDTTTL2",loFormSet.HeaderAlias)) + loFormSet.lcCurOrder
*N000682,1 11/20/2012 MMT Globlization changes[End]


=ACOPY(loFormSet.laOrd, loFormSet.laEdtQty1)

*-- Call the Edit Cost screen for the current style.
DO FORM (oAriaApplication.ScreenHome + "SO\SOALOMD") WITH loFormSet, lcModTitle

=lfPOLineChange(loFormSet, .T.)
loFormSet.AriaForm1.grdSOLines.SetFocus()
loFormSet.AriaForm1.grdSOLinesToolBar.RefreshGrids = .T.

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfvFltrOrd
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/06/2004
*! Purpose   : Valid function for the popup filter.
*!*************************************************************
*! Example   : =lfvFltrOrd()
*!*************************************************************
FUNCTION lfvFltrOrd
LPARAMETERS loFormSet, lcValue

LOCAL lnAlias, lcFltExpr
lnAlias   = SELECT(0)
lcFltExpr = ''

*--Lock Screen Events Read
loFormSet.AriaForm1.LockScreen = .T.

SELECT (loFormSet.lcOrderLin)
GO TOP

DO CASE
  *-- Show linked order lines only.
  CASE lcValue = 'L'
    lcFltExpr = "TotCut <> 0 .AND. " +;
                "SEEK('" + loFormSet.lcKeyCode + loFormSet.lcKeyFld + "'" +;
                " + STR(" + loFormSet.lcMainLin + ".LineNo,6)" +;
                " + ORDER + Style + STR(LineNo,6), '" + loFormSet.lcCutPick + "')"

  *-- Show order lines with available qty.
  CASE lcValue = 'V'
    lcFltExpr = "(TotQty - TotCut) <> 0"

  *-- Show both linked order lines & order lines with available qty.
  CASE lcValue = 'W'
    lcFltExpr = "((TotQty - TotCut) <> 0) .OR. " +;
                " (TotCut <> 0 .AND. " +;
                "  SEEK('" + loFormSet.lcKeyCode + loFormSet.lcKeyFld + "'" +;
                "  + STR(" + loFormSet.lcMainLin + ".LineNo,6)" +;
                "  + ORDER + Style + STR(LineNo,6), '" + loFormSet.lcCutPick + "'))"
ENDCASE

SET FILTER TO &lcFltExpr

=lfPOLineChange(loFormSet)
loFormSet.AriaForm1.grdSOLines.SetFocus()
loFormSet.AriaForm1.grdSOLinesToolBar.RefreshGrids = .T.

*--Restore Screen Events Read
loFormSet.AriaForm1.LockScreen = .F.

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/06/2004
*! Purpose   : Local function save called from the gcpSave function.
*!*************************************************************
*! Example   : =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr
LPARAMETERS loFormSet

LOCAL lnAlias
lnAlias = SELECT(0)

*-- Clear the filter in the order lines temp file till finish saving.
SELECT (loFormSet.lcOrderLin)
SET FILTER TO
LOCATE

*-- Update the total cut qty. field in the order header file.
LOCAL lcOrder, lnTotQty, lnOldQty
lnTotQty = 0                    && Var. hold the total qty. for the first order.
lcOrder  = EVALUATE(loFormSet.lcOrderLin + '.Order')   && Var. hold the first order.
lnOldQty = 0

SELECT (loFormSet.lcOrderLin)
SCAN
  *-- If the order no. changed.
  IF lcOrder <> EVALUATE(loFormSet.lcOrderLin + '.Order')
    *-- Seek for the order no. in the order header file to update
    *-- the total order qty. field.
    IF loFormSet.loOrdHdr.Seek("O" + lcOrder, "OrdHdr")
      loFormSet.loOrdHdr.REPLACE("TotCut WITH TotCut + " + STR(lnTotQty - lnOldQty))
    ENDIF

    lnOldQty = OLDVAL('TotCut', loFormSet.lcOrderLin)

    *-- Acumelate the total order qty. for the current order.
    lnTotQty = EVALUATE(loFormSet.lcOrderLin + '.TotCut')

    *-- Change the order variable with the new order no.
    lcOrder  = EVALUATE(loFormSet.lcOrderLin + '.Order')
  ELSE
    lnOldQty = lnOldQty + OLDVAL('TotCut', loFormSet.lcOrderLin)

    *-- Acumelate the total order qty. for the current order.
    lnTotQty = lnTotQty + EVALUATE(loFormSet.lcOrderLin + '.TotCut')
  ENDIF
ENDSCAN

*-- Seek for the order no. in the order header file to update
*-- the total order qty. field.
IF loFormSet.loOrdHdr.SEEK("O" + lcOrder , "OrdHdr")
  loFormSet.loOrdHdr.REPLACE('TotCut WITH TotCut + ' + STR(lnTotQty - lnOldQty))
ENDIF

*-- Update the total ordered qty. field in the C/T or P/O header file.
lnTotQty = 0
SELECT (loFormSet.lcMainLin)
SUM TotOrd TO lnTotQty

SELECT (loFormSet.lcAlocFile)
REPLACE TotOrd WITH lnTotQty

*--Update All Files
LOCAL llRetVal, lcCursors, lcPriKeys, lcTables, lcIndecies
llRetVal = .T.

*--Update All SQL Tables in one Transaction

*--Updated Cursors separated by '|'
lcCursors  = loFormSet.lcAlocFile + '|' +;
             loFormSet.lcMainLin  + '|' +;
             loFormSet.lcCutPick  + '|'

*--SQL Tables Primary Key Lists separated by '|'

*B607585,1 AMH Add cwarecode field to POSLN index [Start]
*lcPriKeys  = "cBusDocu,cStyType,PO|" +;
             "cBusDocu,cStyType,PO,cRSession,ShipNo,cInvType,Style,LineNo,TranCd,cStyGrade|" +;
             "Trancd,cTktNo,cTktLineNo,Order,Style,cOrdLine|"
lcPriKeys  = "cBusDocu,cStyType,PO|" +;
             "cBusDocu,cStyType,PO,cRSession,ShipNo,cInvType,Style,LineNo,TranCd,cStyGrade,cWareCode|" +;
             "Trancd,cTktNo,cTktLineNo,Order,Style,cOrdLine|"
*B607585,1 AMH [End]

lcTables   = "POSHDR|POSLN|CUTPICK|"

lcIndecies = "POSHDR|POSREC|CUTPKORD"

*--Do Update
llRetVal = lfTableUpdate(lcCursors, lcPriKeys, lcTables, lcIndecies, .F.)

*--Update All FOX Tables in one Transaction
IF llRetVal
  *--Updated Cursors separated by '|'
  lcCursors  = loFormSet.lcOrdHdr + '|' +;
               loFormSet.lcOrderLin  + '|'

  *--FOX Tables Primary Key Lists separated by '|'
  lcPriKeys  = "CORDTYPE+ORDER|" +;
               "CORDTYPE+ORDER+STR(LINENO,6)|"

  lcTables   = "ORDHDR|ORDLINE|"

  lcIndecies = "ORDHDR|ORDLINE|"

  *--Do Update
  llRetVal = lfTableUpdate(lcCursors, lcPriKeys, lcTables, lcIndecies, .T.)
ENDIF

SELECT (loFormSet.lcMainLin)
LOCATE

loFormSet.AriaForm1.grdPOLines.ActivateCell(1,1)
=lfPOLineChange(loFormSet)

SELECT (lnAlias)
RETURN llRetVal

*!*************************************************************
*! Name      : lpClsScr
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/08/2004
*! Purpose   : Local cancel function called if cancel the current modification.
*! Example   : =lpClsScr()
*!*************************************************************
FUNCTION lpClsScr
LPARAMETERS loFormSet

*-- Blank the variable to force collecting the data again.
loFormSet.lcCurKey = " "

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/08/2004
*! Purpose   : Valid function for the push button <Order>
*! Example   : =lfvOrder()
*!*************************************************************
FUNCTION lfvOrder
LPARAMETERS loFormSet

LOCAL lnAlias, lcModTitle
lnAlias = SELECT(0)

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcModTitle = LANG_SOALOCT_CSTEDTTTL1 + loFormSet.lcStyHdr + ' :' + EVALUATE(loFormSet.lcOrderLin + '.Style') + LANG_SOALOCT_CSTEDTTTL2 + loFormSet.lcCurOrder
lcModTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_CSTEDTTTL1,loFormSet.GetHeaderText("LANG_SOALOCT_CSTEDTTTL1",loFormSet.HeaderAlias)) + loFormSet.lcStyHdr + ' :' + EVALUATE(loFormSet.lcOrderLin + '.Style') + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_CSTEDTTTL2,loFormSet.GetHeaderText("LANG_SOALOCT_CSTEDTTTL2",loFormSet.HeaderAlias)) + loFormSet.lcCurOrder
*N000682,1 11/20/2012 MMT Globlization changes[End]


=ACOPY(loFormSet.laOrd, loFormSet.laOldLaOrd)
loFormSet.llOrder = .T.

=ACOPY(loFormSet.laOrd, loFormSet.laEdtQty1)

*-- Call the Edit Cost screen for the current style.
DO FORM (oAriaApplication.ScreenHome + "SO\SOALOMD") WITH loFormSet, lcModTitle

=lfPOLineChange(loFormSet, .T.)
loFormSet.AriaForm1.grdSOLines.SetFocus()
loFormSet.AriaForm1.grdSOLinesToolBar.RefreshGrids = .T.

SELECT (lnAlias)
RETURN

*!***************************************************************
*****              Functions for SOALOMD Screen             *****
***** Screen to modify the allocated qty. in the C/T or P/O *****
*!***************************************************************

*!*************************************************************
*! Name      : lfAloMDInit
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/12/2004
*! Purpose   : Init Function for the SOALOMD Screen.
*!*************************************************************
*! Example   : =lfAloMDInit(ThisFormSet, loParent, lcTitle)
*!*************************************************************
FUNCTION lfAloMDInit
LPARAMETERS loFormSet, loParent, lcTitle

WITH loFormSet
  .loParent = loParent
  .ChangeMode(loParent.ActiveMode)

  WITH .AriaForm1
    .Caption = lcTitle

    FOR lnCount = 1 TO 8
      *--Display Sizes Labels
      lcCount = ALLTRIM(STR(lnCount))

      *--Display PO Qty
      .cntPOQty.txtSizeLbl&lcCount..Value = loParent.laSize[lnCount]
      .cntPOQty.txtQty&lcCount..Value     = EVALUATE(loParent.lcMainLin + '.Qty' + lcCount)

      *--Display Allocated Qty
      .cntAllocated.txtQty&lcCount..Value = EVALUATE(loParent.lcMainLin + '.Ord' + lcCount)

      *--Display UnAllocated Qty
      .cntUnAllocated.txtQty&lcCount..Value = EVALUATE(loParent.lcMainLin + '.Qty' + lcCount) - EVALUATE(loParent.lcMainLin + '.Ord' + lcCount)

      *--Display Order Qty
      .cntOrderQty.txtQty&lcCount..Value = IIF(loParent.llAloMulSt, loParent.laStrQty[lnCount], EVALUATE(loParent.lcOrderLin + '.Qty' + lcCount))

      *--Display Order Generated Qty
      .cntGenerated.txtQty&lcCount..Value   = loParent.laEdtQty1[lnCount]
      .cntGenerated.txtQty&lcCount..Enabled = !(lnCount > loParent.laSize[9])

      *--Display Order Available Qty
      .cntAvailable.txtQty&lcCount..Value     = IIF(loParent.llAloMulSt, loParent.laStrQty[lnCount], EVALUATE(loParent.lcOrderLin + '.Qty' + lcCount)) -;
                                                IIF(loParent.llAloMulSt, 0, EVALUATE(loParent.lcOrderLin + '.Cut' + lcCount)) -;
                                                loParent.laEdtQty1[lnCount] +;
                                                IIF(loParent.llOrder, loParent.laOrd[lnCount], 0)
      .cntAvailable.txtQty&lcCount..AriaColor = IIF((EVALUATE(loParent.lcOrderLin + '.Qty' + lcCount) - EVALUATE(loParent.lcOrderLin + '.Cut' + lcCount) + loParent.laOrd[lnCount] - loParent.laEdtQty1[lnCount]) = 0, 'G', 'R')
    ENDFOR

    *--Adjust Total PO Qty
    .cntPOQty.mGetTotal()

    *--Adjust Total Allocated Qty
    .cntAllocated.mGetTotal()

    *--Adjust Total UnAllocated Qty
    .cntUnAllocated.mGetTotal()

    *--Adjust Total Order Qty
    .cntOrderQty.mGetTotal()

    *--Adjust Total Generated Qty
    .cntGenerated.mGetTotal()

    *--Adjust Total Available Qty and Backcolor
    .cntAvailable.mGetTotal()
    .cntAvailable.txtTotQty.AriaColor = IIF((EVALUATE(loParent.lcOrderLin + '.TotQty') - EVALUATE(loParent.lcOrderLin + '.TotCut') + loParent.laOrd[9] - loParent.laEdtQty1[9]) = 0, 'G', 'R')

    .cntGenerated.txtQty1.SetFocus()
  ENDWITH
ENDWITH

RETURN

*!*************************************************************
*! Name      : lfvEdtQty
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/12/2004
*! Purpose   : Valid function for the generated qty. in the
*!           : modify screen.
*!*************************************************************
*! Calls     : lfModRef
*!*************************************************************
*! Example            :  =lfvEdtQty()
*!*************************************************************
FUNCTION lfvEdtQty
LPARAMETERS loFormSet, lnNo

LOCAL loParent, lnCurrVal, lnMax, lcNo
lcNo      = ALLTRIM(STR(lnNo))
loParent  = loFormSet.loParent
lnCurrVal = loFormSet.AriaForm1.cntGenerated.txtQty&lcNo..Value

*-- The screen's objects has been updated.
loParent.llCUpDate  = .T.

*-- Get the maximum limit for entering the current qty.
IF loParent.llAloMulSt
  lnMax =  MIN((EVALUATE(loParent.lcMainLin + '.Qty' + lcNo) - EVALUATE(loParent.lcMainLin + '.Ord' + lcNo)), loParent.laStrQty[lnNo])

  IF lnCurrVal < 0 .OR. lnCurrVal > lnMax .OR. lnCurrVal > loParent.laStrQty[lnNo]
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *WAIT WINDOW 'RANGE: 0 to '+ ALLTRIM(STR(MIN(lnMax, loParent.laStrQty[lnNo]))) NOWAIT
    WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_WAITRANGE,loParent.GetHeaderText("LANG_SOALOCT_WAITRANGE",loParent.HeaderAlias))+" "+ ALLTRIM(STR(MIN(lnMax, loParent.laStrQty[lnNo]))) NOWAIT
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    RETURN .F.
  ENDIF
ELSE
  lnMax = MIN((EVALUATE(loParent.lcMainLin + '.Qty' + lcNo) - EVALUATE(loParent.lcMainLin + '.Ord' + lcNo)),;
             (EVALUATE(loParent.lcOrderLin + '.Qty' + lcNo) - EVALUATE(loParent.lcOrderLin + '.Cut' + lcNo)))
  lnMax = IIF(loParent.llOrder, lnMax + loParent.laOrd[lnNo], lnMax)

  IF lnCurrVal < 0 .OR. lnCurrVal > lnMax .OR. lnCurrVal > EVALUATE(loParent.lcOrderLin + '.Qty' + lcNo)
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *WAIT WINDOW 'RANGE: 0 to '+ ALLTRIM(STR(MIN(lnMax, EVALUATE(loParent.lcOrderLin + '.Qty' + lcNo)))) NOWAIT    
    WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_WAITRANGE,loParent.GetHeaderText("LANG_SOALOCT_WAITRANGE",loParent.HeaderAlias))+" "+ ALLTRIM(STR(MIN(lnMax, EVALUATE(loParent.lcOrderLin + '.Qty' + lcNo)))) NOWAIT
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    RETURN .F.
  ENDIF
ENDIF

*--Update the Ayyay Value
loParent.laEdtQty1[lnNo] = lnCurrVal

*-- Calculate the total qty.
loFormSet.AriaForm1.cntGenerated.mGetTotal()

*--Refresh Order Available Qty
WITH loFormSet.AriaForm1.cntAvailable
  .txtQty&lcNo..Enabled   = .F.
  .txtQty&lcNo..Value     = IIF(loParent.llAloMulSt, loParent.laStrQty[lnNo], EVALUATE(loParent.lcOrderLin + '.Qty' + lcNo)) -;
                            IIF(loParent.llAloMulSt, 0, EVALUATE(loParent.lcOrderLin + '.Cut' + lcNo)) -;
                            lnCurrVal +;
                            IIF(loParent.llOrder, loParent.laOrd[lnNo], 0)
  .txtQty&lcNo..AriaColor = IIF((EVALUATE(loParent.lcOrderLin + '.Qty' + lcNo) - EVALUATE(loParent.lcOrderLin + '.Cut' + lcNo) + loParent.laOrd[lnNo] - loParent.laEdtQty1[lnNo]) = 0, 'G', 'R')
ENDWITH

*--Refresh Total Available Qty and Backcolor
loFormSet.AriaForm1.cntAvailable.mGetTotal()
loParent.laEdtQty1[9] = loFormSet.AriaForm1.cntGenerated.txtTotQty.Value
loFormSet.AriaForm1.cntAvailable.txtTotQty.AriaColor = IIF((EVALUATE(loParent.lcOrderLin + '.TotQty') - EVALUATE(loParent.lcOrderLin + '.TotCut') + loParent.laOrd[9] - loParent.laEdtQty1[9]) = 0, 'G', 'R')

RETURN .T.

*!*************************************************************
*! Name      : lfvModOK
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/13/2004
*! Purpose   : Valid function for the ok button.
*!*************************************************************
*! Calls     : lfRelLin
*!*************************************************************
*! Example            :  =lfvModOK()
*!*************************************************************
FUNCTION lfvModOK
LPARAMETERS loFormSet

LOCAL loParent, lnAlias, lnTotCost, lnTotQty, lnOldQty, lnGrosMrgn, lnRotSub
loParent = loFormSet.loParent
lnAlias  = SELECT(0)

*-- The screen's objects has been updated.
loParent.llCUpDate  = .T.
=ACOPY(loParent.laEdtQty1, loParent.laEdtQty)

IF loParent.laEdtQty[9] = 0
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("QRM34178B000006", "DIALOG", IIF(loParent.lcAllocate = "U" , LANG_SOALOCT_SHORTCT, LANG_SOALOCT_SHORTPO))
=gfModalGen("QRM34178B000006", "DIALOG", IIF(loParent.lcAllocate = "U" , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SHORTCT,loFormSet.GetHeaderText("LANG_SOALOCT_SHORTCT",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SHORTPO,loFormSet.GetHeaderText("LANG_SOALOCT_SHORTPO",loFormSet.HeaderAlias))))
*N000682,1 11/20/2012 MMT Globlization changes[End]


  SELECT (lnAlias)
  RETURN .F.
ELSE
  IF loParent.llAloMulSt AND !loParent.llOrder

    *-- IF Generated Qty less Than Total Order Qty
    IF loParent.laEdtQty[9] < loParent.laStrQty[9]

      *-- Generated Qty less Than Total Order Qty. please Confirm allocation
      *-- <CONFIRM> <NO>
      IF gfModalGen("INM34177B34017", "DIALOG") = 1
        =lfAloMulSt(loFormSet)
      ELSE
        RETURN
      ENDIF
    ELSE
      =lfAloMulSt(loFormSet)
    ENDIF
  ELSE

    *--Get Old Qty before Allocation
    IF loParent.llDispPric
      lnOldQty = 0
      For lnI = 1 To 8
        lcI = ALLTRIM(STR(lnI))
        lnOldQty = lnOldqty + EVALUATE(loParent.lcMainLin + '.Ord' + lcI)
      ENDFOR
    ENDIF

    *-- Update the cutpick file with the modified qty.
    SELECT (loParent.lcCutPick)

    IF !SEEK(loParent.lcKeyCode+loParent.lcKeyFld+STR(EVALUATE(loParent.lcMainLin + '.LineNo'),6)+EVALUATE(loParent.lcOrderLin + '.Order')+EVALUATE(loParent.lcOrderLin + '.Style')+STR(EVALUATE(loParent.lcOrderLin + '.LineNo'),6), loParent.lcCutPick)
      *-- Add new record to the cutpick file if does not exist in the cutpick file.
      APPEND BLANK
    ENDIF

    REPLACE cTktNo     WITH loParent.lcKeyFld  ,;
            TranCd     WITH loParent.lcKeyCode ,;
            cTktLineNo WITH STR(EVALUATE(loParent.lcMainLin + '.LineNo'),6) ,;
            Order      WITH EVALUATE(loParent.lcOrderLin + '.Order') ,;
            cOrdLine   WITH STR(EVALUATE(loParent.lcOrderLin + '.LineNo'),6) ,;
            Style      WITH EVALUATE(loParent.lcOrderLin + '.Style')

    REPLACE Qty1   WITH IIF(loParent.llOrder,0,Qty1)   + loParent.laEdtQty[1] ,;
            Qty2   WITH IIF(loParent.llOrder,0,Qty2)   + loParent.laEdtQty[2] ,;
            Qty3   WITH IIF(loParent.llOrder,0,Qty3)   + loParent.laEdtQty[3] ,;
            Qty4   WITH IIF(loParent.llOrder,0,Qty4)   + loParent.laEdtQty[4] ,;
            Qty5   WITH IIF(loParent.llOrder,0,Qty5)   + loParent.laEdtQty[5] ,;
            Qty6   WITH IIF(loParent.llOrder,0,Qty6)   + loParent.laEdtQty[6] ,;
            Qty7   WITH IIF(loParent.llOrder,0,Qty7)   + loParent.laEdtQty[7] ,;
            Qty8   WITH IIF(loParent.llOrder,0,Qty8)   + loParent.laEdtQty[8] ,;
            TotQty WITH IIF(loParent.llOrder,0,TotQty) + loParent.laEdtQty[9]

    =gfAdd_info(loParent.lcCutPick)

    IF loParent.llDispPric
      lnTotQty = 0
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        lnTotQty = lnTotQty + loParent.laEdtQty[lnI]
      ENDFOR
      lnSelPrice = lnTotQty * EVALUATE(loParent.lcOrderLin + '.Price')
    ENDIF

    *-- Update the temp P/O or C/T lines with the modified qty.
    SELECT (loParent.lcMainLin)

    IF loParent.llDispPric
      lnSelPrice = (lnSelPrice + lnOldQty * nSelPrice) / (lnTotQty + lnOldQty)
      IF loParent.lcAllocate = 'P'
        lnTotCost = nICost1 + nICost2 + nICost3 + nICost4 + nICost5
      ELSE
        *! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][Start]                              
*!*	        lcSQLStat = "SELECT TotCost" +;
*!*	                    "  FROM Style " +;
*!*	                    " WHERE Style = '" + Style + "'"
        *! B611009,2 MMT 05/31/2015 PO allocation program crashes if PO styles conatins ?[T20150527.0002][Start]                          
*!*	        lcSQLStat = "SELECT TotCost" +;
*!*	                    "  FROM Style " +;
*!*	                    " WHERE Style = [" + Style + "]"
        lcStyleValue = Style
        lcSQLStat = "SELECT TotCost" +;
                    "  FROM Style " +;
                    " WHERE Style = ?lcStyleValue "
        *! B611009,2 MMT 05/31/2015 PO allocation program crashes if PO styles conatins ?[T20150527.0002][End]                                              
        *! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][End]                                                  
        IF !lfSQLStatement(lcSQLStat, 'TmpStyle', '', '', .T.)
          RETURN .F.
        ENDIF

        SELECT TmpStyle
        LOCATE
        lnTotCost = TmpStyle.TOTCOST

        USE IN TmpStyle
      ENDIF

      SELECT (loParent.lcMainLin)

      lnRotSub   = IIF(loParent.llStyMark, lnTotCost, lnSelPrice)
      lnGrosMrgn = IIF(lnRotSub = 0, 0, ((lnSElPrice - lnTotCost) / lnRotSub) * 100)
      REPLACE nSelPrice WITH lnSelPrice,;
              nGrosMrgn WITH lnGrosMrgn
    ENDIF

    REPLACE Ord1    WITH IIF(loParent.llOrder, Ord1 - loParent.laOrd[1], Ord1) + loParent.laEdtQty[1],;
            Ord2    WITH IIF(loParent.llOrder, Ord2 - loParent.laOrd[2], Ord2) + loParent.laEdtQty[2],;
            Ord3    WITH IIF(loParent.llOrder, Ord3 - loParent.laOrd[3], Ord3) + loParent.laEdtQty[3],;
            Ord4    WITH IIF(loParent.llOrder, Ord4 - loParent.laOrd[4], Ord4) + loParent.laEdtQty[4],;
            Ord5    WITH IIF(loParent.llOrder, Ord5 - loParent.laOrd[5], Ord5) + loParent.laEdtQty[5],;
            Ord6    WITH IIF(loParent.llOrder, Ord6 - loParent.laOrd[6], Ord6) + loParent.laEdtQty[6],;
            Ord7    WITH IIF(loParent.llOrder, Ord7 - loParent.laOrd[7], Ord7) + loParent.laEdtQty[7],;
            Ord8    WITH IIF(loParent.llOrder, Ord8 - loParent.laOrd[8], Ord8) + loParent.laEdtQty[8],;
            TotOrd  WITH IIF(loParent.llOrder, TotOrd  - loParent.laOrd[9], TotOrd) + loParent.laEdtQty[9]

    =gfAdd_info(loParent.lcMainLin)

    *-- Update the temp. order lines file with the modified qty.
    SELECT (loParent.lcOrderLin)
    REPLACE Cut1       WITH IIF(loParent.llOrder, 0, Cut1) + loParent.laEdtQty[1],;
            Cut2       WITH IIF(loParent.llOrder, 0, Cut2) + loParent.laEdtQty[2],;
            Cut3       WITH IIF(loParent.llOrder, 0, Cut3) + loParent.laEdtQty[3],;
            Cut4       WITH IIF(loParent.llOrder, 0, Cut4) + loParent.laEdtQty[4],;
            Cut5       WITH IIF(loParent.llOrder, 0, Cut5) + loParent.laEdtQty[5],;
            Cut6       WITH IIF(loParent.llOrder, 0, Cut6) + loParent.laEdtQty[6],;
            Cut7       WITH IIF(loParent.llOrder, 0, Cut7) + loParent.laEdtQty[7],;
            Cut8       WITH IIF(loParent.llOrder, 0, Cut8) + loParent.laEdtQty[8],;
            TotCut     WITH IIF(loParent.llOrder, 0, TotCut) + loParent.laEdtQty[9]

    *-- Replace the CT/PO line dyelot in the order line dyelot.
    IF loParent.llRepDye
      REPLACE Dyelot WITH EVALUATE(loParent.lcMainLin + '.Dyelot')
    ENDIF

    =gfAdd_info(loParent.lcOrderLin)
  ENDIF
ENDIF

STORE .F. TO loParent.llOrder, loParent.llAloMulSt

*-- Select the main file "POSHDR" if P/O allocation or "CUTTKTH" if C/T allocation.
SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfAloMulSt
*! Developer : Wael M. Abo-Shawareb
*! Date      : 12/22/2004
*! Purpose   : Allocate in case of multi store
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAloMulSt()
*!*************************************************************
FUNCTION lfAloMulSt
LPARAMETERS loFormSet

LOCAL loParent, lcOrder, lnTotQty, lcStyle, lnI, lcI, lnOldQty, lnSelPrice, lnGrosMrgn, lnRotSub
loParent = loFormSet.loParent
lnTotQty = 0
lcOrder  = EVALUATE(loParent.lcOrderLin + '.Order')
lcStyle  = EVALUATE(loParent.lcOrderLin + '.Style')

*--Get Old Qty before Allocation
IF loParent.llDispPric
  lnOldQty = 0
  For lnI = 1 To 8
    lcI = ALLTRIM(STR(lnI))
    lnOldQty = lnOldqty + EVALUATE(loParent.lcMainLin + '.Ord' + lcI)
  ENDFOR
  lnSelPrice = 0
ENDIF

SELECT (loParent.lcOrderLin)
LOCAL lcOldOrd, lnOldRec
lcOldOrd = ORDER()
lnOldRec = RECNO()

SET ORDER TO (loParent.lcOrderLin)
=SEEK('O'+lcOrder)
SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O' + lcOrder;
          FOR Style = lcStyle

  IF TotCut = TotQty
    LOOP
  ENDIF

  FOR lnI = 1 TO 8
    lcI = ALLTRIM(STR(lnI))
    loParent.laLinQty[lnI] = IIF(Cut&lcI = 0, Qty&lcI, Qty&lcI - Cut&lcI)
  ENDFOR
  loParent.laLinQty[9] = TotQty

  STORE 0 TO loParent.laStrlnCut
  FOR lnI = 1 TO 8
    lcI = ALLTRIM(STR(lnI))
    IF loParent.laEdtQty[lnI] > loParent.laLinQty[lnI]
      loParent.laStrlnCut[lnI] = loParent.laLinQty[lnI]
      loParent.laEdtQty[lnI]   = loParent.laEdtQty[lnI] - loParent.laLinQty[lnI]
    ELSE
      loParent.laStrlnCut[lnI] = loParent.laEdtQty[lnI]
      loParent.laEdtQty[lnI]   = 0
    ENDIF
    loParent.laStrlnCut[9] = loParent.laStrlnCut[9] + loParent.laStrlnCut[lnI]
  ENDFOR

  SELECT (loParent.lcOrderLin)
  REPLACE Cut1    WITH Cut1 + loParent.laStrlnCut[1] ,;
          Cut2    WITH Cut2 + loParent.laStrlnCut[2] ,;
          Cut3    WITH Cut3 + loParent.laStrlnCut[3] ,;
          Cut4    WITH Cut4 + loParent.laStrlnCut[4] ,;
          Cut5    WITH Cut5 + loParent.laStrlnCut[5] ,;
          Cut6    WITH Cut6 + loParent.laStrlnCut[6] ,;
          Cut7    WITH Cut7 + loParent.laStrlnCut[7] ,;
          Cut8    WITH Cut8 + loParent.laStrlnCut[8] ,;
          TotCut  WITH TotCut + loParent.laStrlnCut[9]

  =gfAdd_info(loParent.lcOrderLin)

  SELECT (loParent.lcCutPick)
  IF !SEEK(loParent.lcKeyCode+loParent.lcKeyFld+STR(EVALUATE(loParent.lcMainLin + '.LineNo'),6)+EVALUATE(loParent.lcOrderLin + '.Order')+EVALUATE(loParent.lcOrderLin + '.Style')+STR(EVALUATE(loParent.lcOrderLin + '.LineNo'),6), loParent.lcCutPick)
    *-- Add new record to the cutpick file if does not exist in the cutpick file.
    APPEND BLANK
  ENDIF

  REPLACE cTktNo     WITH loParent.lcKeyFld  ,;
          TranCd     WITH loParent.lcKeyCode ,;
          cTktLineNo WITH STR(EVALUATE(loParent.lcMainLin + '.LineNo'),6) ,;
          Order      WITH EVALUATE(loParent.lcOrderLin + '.Order') ,;
          cOrdLine   WITH STR(EVALUATE(loParent.lcOrderLin + '.LineNo'),6) ,;
          Style      WITH EVALUATE(loParent.lcOrderLin + '.Style')

  REPLACE Qty1   WITH Qty1 + loParent.laStrlnCut[1] ,;
          Qty2   WITH Qty2 + loParent.laStrlnCut[2] ,;
          Qty3   WITH Qty3 + loParent.laStrlnCut[3] ,;
          Qty4   WITH Qty4 + loParent.laStrlnCut[4] ,;
          Qty5   WITH Qty5 + loParent.laStrlnCut[5] ,;
          Qty6   WITH Qty6 + loParent.laStrlnCut[6] ,;
          Qty7   WITH Qty7 + loParent.laStrlnCut[7] ,;
          Qty8   WITH Qty8 + loParent.laStrlnCut[8] ,;
          TotQty WITH TotQty + loParent.laStrlnCut[9]

  =gfAdd_info(loParent.lcCutPick)

  IF loParent.llDispPric
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      lnTotQty = lnTotQty + loParent.laStrlnCut[lnI]
    ENDFOR
    lnSelPrice = lnSelPrice + (lnTotQty * EVALUATE(loParent.lcOrderLin + '.Price'))
  ENDIF
ENDSCAN

SELECT (loParent.lcOrderLin)
SET ORDER TO (lcOldOrd)

IF BETWEEN(lnOldRec, 1 , RECCOUNT())
  GOTO lnOldRec
ENDIF

*-- Update the temp P/O or C/T lines with the modified qty.
SELECT (loParent.lcMainLin)
IF loParent.llDispPric
  lnSelPrice = (lnSelPrice + lnOldQty * nSelPrice) / (lnTotQty + lnOldQty)
  *! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][Start]                              
*!*	  lcSQLStat = "SELECT PriceA, nICost1, nICost2, nICost3, nICost4, nICost5, TotCost" +;
*!*	              "  FROM Style " +;
*!*	              " WHERE Style = '" + EVALUATE(loParent.lcMainLin + '.Style') + "'"
  *! B611009,2 MMT 05/31/2015 PO allocation program crashes if PO styles conatins ?[T20150527.0002][Start]
*!*	  lcSQLStat = "SELECT PriceA, nICost1, nICost2, nICost3, nICost4, nICost5, TotCost" +;
*!*	              "  FROM Style " +;
*!*	              " WHERE Style = [" + EVALUATE(loParent.lcMainLin + '.Style') + "]"
  lcStyleValue = EVALUATE(loParent.lcMainLin + '.Style') 
  lcSQLStat = "SELECT PriceA, nICost1, nICost2, nICost3, nICost4, nICost5, TotCost" +;
              "  FROM Style " +;
              " WHERE Style = ?lcStyleValue "
  *! B611009,2 MMT 05/31/2015 PO allocation program crashes if PO styles conatins ?[T20150527.0002][END]                                        
  *! B611009,1 MMT 05/27/2015 PO allocation program crashes if PO styles conatins '[T20150527.0002][End]                                            
  IF !lfSQLStatement(lcSQLStat, 'TmpStyle', '', '', .T.)
    RETURN .F.
  ENDIF

  SELECT TmpStyle
  LOCATE

  IF loParent.lcAllocate = 'P'
    lnTotCost = nICost1 + nICost2 + nICost3 + nICost4 + nICost5
  ELSE
    lnTotCost = TOTCOST
  ENDIF

  lnRotSub   = IIF(loParent.llStyMark, lnTotCost, lnSelPrice)
  lnGrosMrgn = IIF(lnRotSub = 0, 0, ((lnSelPrice - lnTotCost) / lnRotSub) * 100)

  SELECT (loParent.lcMainLin)
  REPLACE nSelPrice WITH lnSelPrice,;
          nGrosMrgn WITH lnGrosMrgn
ENDIF

REPLACE Ord1    WITH Ord1 + loParent.laEdtQty1[1] ,;
        Ord2    WITH Ord2 + loParent.laEdtQty1[2] ,;
        Ord3    WITH Ord3 + loParent.laEdtQty1[3] ,;
        Ord4    WITH Ord4 + loParent.laEdtQty1[4] ,;
        Ord5    WITH Ord5 + loParent.laEdtQty1[5] ,;
        Ord6    WITH Ord6 + loParent.laEdtQty1[6] ,;
        Ord7    WITH Ord7 + loParent.laEdtQty1[7] ,;
        Ord8    WITH Ord8 + loParent.laEdtQty1[8] ,;
        TotOrd  WITH TotOrd + loParent.laEdtQty1[9]

=gfAdd_info(loParent.lcMainLin)

STORE 0 TO loParent.laStrQty, loParent.laStrCut, loParent.laLinQty, loParent.laStrlnCut

*-- End of lfAloMulSt

*!*************************************************************
*! Name      : lfvCancel
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/13/2004
*! Purpose   : Valid function for cancel button
*!*************************************************************
*! Example   :  =lfvCancel()
*!*************************************************************
FUNCTION lfvCancel
LPARAMETERS loFormSet

LOCAL loParent
loParent = loFormSet.loParent

STORE .F. TO loParent.llOrder, loParent.llAloMulSt

STORE 0 TO loParent.laStrQty, loParent.laStrCut, loParent.laLinQty, loParent.laStrlnCut

=ACOPY(loParent.laOldLaOrd, loParent.laOrd)
*--End of lfvCancel

*!*************************************************************
*! Name      : lfChkMultS
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/05/2004
*! Purpose   : Check If this is style is in more than store
*!             in case of Multi Store
*!*************************************************************
*! Example   :  =lfChkMultS()
*!*************************************************************
FUNCTION lfChkMultS
LPARAMETERS loFormSet

LOCAL lcThisOrd, lcThisSty, lcStore, lnStores
lcThisOrd  = EVALUATE(loFormSet.lcOrderLin + '.Order')
lcThisSty  = EVALUATE(loFormSet.lcOrderLin + '.Style')
lnStores   = 0
loFormSet.llMultiStr = .F.

SELECT Store FROM (loFormSet.lcOrderLin) ;
 WHERE cOrdType+Order+STR(LineNo,6) = 'O' + lcThisOrd AND ;
       Style = lcThisSty ;
  INTO CURSOR lcTmpStore

SELECT lcTmpStore
LOCATE
lcStore = Store

SCAN
  IF Store <> lcStore
    loFormSet.llMultiStr = .T.
    EXIT
  ENDIF
ENDSCAN

USE IN lcTmpStore

RETURN loFormSet.llMultiStr
*--End of lfChkMultS

*!**************************************************************************
*! Name      : lfPicture
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/30/2004
*! Purpose   : Change Picture of PO.
*!**************************************************************************
FUNCTION lfPicture
LPARAMETERS loFormSet

IF loFormSet.llGenOrNum
  RETURN "!!!!!!"
ELSE
  RETURN "!99999"
ENDIF

*!*************************************************************
*! Name      : lfCreateTmpFiles
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/29/2004
*! Purpose   : Create Formset's temp cursors
*!*************************************************************
*! Example   : =lfCreateTmpFiles()
*!*************************************************************
FUNCTION lfCreateTmpFiles
LPARAMETERS loFormSet

LOCAL lSQLCommand, lnRemResult

*-- Prepare the Order Header temp. file.
loFormSet.loOrdHdr = CREATEOBJECT("RemoteTable", 'OrdHdr', 'OrdHdr', loFormSet.lcOrdHdr, SET("Datasession"))
loFormSet.lcOrdHdr = loFormSet.loOrdHdr.lcCursorView
loFormSet.loOrdHdr.SetOrder('OrdHdr')

*-- Prepare the order lines temp. file.
lSQLCommand = "SELECT * FROM ORDLINE WHERE 1 = 2" && WHERE .F. "(1 = 2) To Work for Fox and SQL"
IF !lfSQLStatement(lSQLCommand, 'TmpOrdLine', '', '', .T.)
  RETURN .F.
ENDIF

SELECT TmpOrdLine
=AFIELDS(laFileStru)
USE IN TmpOrdLine
=gfCrtTmp(loFormSet.lcOrderLin, @laFileStru, "cOrdType+Order+STR(LineNo,6)", loFormSet.lcOrderLin)

SELECT (loFormSet.lcOrderLin)
INDEX ON Style+Dyelot TAG (loFormSet.lcOrdStLin)
=CURSORSETPROP("Buffering", 5, loFormSet.lcOrderLin)
SET ORDER TO (loFormSet.lcOrderLin)

*-- Prepare the order lines temp. file.
lSQLCommand = "SELECT TOP 1 * FROM CUTPICK"
IF !lfSQLStatement(lSQLCommand, loFormSet.lcCutPick, 'TRANCD+CTKTNO+cTktLineNo+ORDER+STYLE+CORDLINE|', loFormSet.lcCutPick + '|', .F., 5)
  RETURN .F.
ENDIF

*-- Prepare the style purchase order lines temp. file if P/O allocation.
lSQLCommand = "SELECT TOP 1 * FROM POSLN (INDEX = POSLN)"
IF !lfSQLStatement(lSQLCommand, loFormSet.lcMainLin, 'Style+Dyelot|', 'POSLN|', .F., 5)
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfSQLStatement
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/29/2004
*! Purpose   : Runs a SQL Server Query Statement
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfSQLStatement()
*!*************************************************************
FUNCTION lfSQLStatement
LPARAMETERS lcSQLStatment, lcCursor, lcIndex, lcTages, llNative, lnBuffering

LOCAL lnConnectionHandlar, lcConnString

*--Get Connection String Type
IF llNative
  lcConnString = oAriaApplication.cAriaNativeDataFilesConStr
ELSE
  lcConnString = oAriaApplication.ActiveCompanyConStr
ENDIF

*--Loop Until Successfully Connect or Cancel by the user
DO WHILE .T.
  *--Run the Satatement
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.SQLRun(lcSqlStatment, lcCursor, '',;
                                  lcConnString, 3, 'SAVE', SET("Datasession"))

  IF lnConnectionHandlar = 1
    *--If Query Successfully executed, Create Indexes if needed for the result cursor
    lnBuffering = IIF(TYPE('lnBuffering') = 'N', lnBuffering, CURSORGETPROP("Buffering", lcCursor))

    =lfCreateIndecies(lcCursor, lcIndex, lcTages)
    RETURN .T.
  ELSE
    *--Query Execution Error
    =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnConnectionHandlar, .F.)

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF MESSAGEBOX(LANG_CONNERRMSG, 5+16, LANG_CONNERRTTL) = 2
IF MESSAGEBOX(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONNERRMSG,loFormSet.GetHeaderText("LANG_CONNERRMSG",loFormSet.HeaderAlias)), 5+16, IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONNERRTTL,loFormSet.GetHeaderText("LANG_CONNERRTTL",loFormSet.HeaderAlias))) = 2
*N000682,1 11/20/2012 MMT Globlization changes[End]

      RETURN .F.
    ENDIF
  ENDIF
ENDDO

*!*************************************************************
*! Name      : lfTableUpdate
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/29/2004
*! Purpose   : Update a Set of SQL Tables
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfTableUpdate()
*!*************************************************************
FUNCTION lfTableUpdate
LPARAMETERS lcAllCursors, lcAllPrimaryKeyLists, lcAllSQLTables, lcAllSQLIndecies, llNative

LOCAL lnConnHandler, lcConnString, lcTranCode, lcCursor, lcPrimaryKeyList, lcSQLTable, lcSQLIndex

*--Get Connection String Type
IF llNative
  lcConnString = oAriaApplication.cAriaNativeDataFilesConStr
ELSE
  lcConnString = oAriaApplication.ActiveCompanyConStr
ENDIF

*-- Begin Updating Transaction
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(lcConnString, 3, '', llNative)

*-- Check Resule for Begin Transaction
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran", lcTranCode, .T.)
  RETURN .F.
ENDIF

DO WHILE AT('|', lcAllCursors) <> 0
  *-- Get the next cursor to update
  lcCursor     = SUBSTR(lcAllCursors, 1, AT("|", lcAllCursors, 1) - 1)
  lcAllCursors = STRTRAN(lcAllCursors, lcCursor + "|", "", 1, 1)

  *-- Get the Primary Key List for the Table
  lcPrimaryKeyList     = SUBSTR(lcAllPrimaryKeyLists, 1, AT("|", lcAllPrimaryKeyLists, 1) - 1)
  lcAllPrimaryKeyLists = STRTRAN(lcAllPrimaryKeyLists, lcPrimaryKeyList + "|", "", 1, 1)
  *lcPrimaryKeyList     = IIF(llNative, STRTRAN(lcPrimaryKeyList, ',', '+'), lcPrimaryKeyList)

  *-- Get the SQL Table Name.
  lcSQLTable           = SUBSTR(lcAllSQLTables, 1, AT("|", lcAllSQLTables, 1) - 1)
  lcAllSQLTables       = STRTRAN(lcAllSQLTables, lcSQLTable + "|", "", 1, 1)

  *-- Get the SQL Table Unique Index Name.
  lcSQLIndex       = SUBSTR(lcAllSQLIndecies, 1, AT("|", lcAllSQLIndecies, 1) - 1)
  lcAllSQLIndecies = STRTRAN(lcAllSQLIndecies, lcSQLIndex + "|", "", 1, 1)

  *-- Start update the Table with the deleted records
  lnConnHandler = oAriaApplication.RemoteCompanyData.SQLUpdate(lcCursor, lcTranCode, SET("Datasession"), lcPrimaryKeyList, lcSQLTable, lcSQLIndex)

  *-- Check Result for Update Process
  IF lnConnHandler <> 1 AND lnConnHandler <> 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUpdate", lnConnHandler, .T.)
    lnConnHandler = oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode, llNative)
    IF lnConnHandler <> 1
      =oAriaApplication.RemoteCompanyData.CheckRetResult("RollBackTran", lnConnHandler, .T.)
    ENDIF
    RETURN .F.
  ENDIF
ENDDO

*-- Commit Changes and Check Result
lnConnHandler = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode, llNative)
IF lnConnHandler <> 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran", lnConnHandler, .T.)
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  Return .F.
ENDIF

RETURN .T.

*!*************************************************************
*! Name      : lfCreateIndecies
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/29/2004
*! Purpose   : Create Indecies for a cursor
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
FUNCTION lfCreateIndecies
LPARAMETERS lcCursor, lcIndex, lcTages

LOCAL lnOldBuffMode, lcIndex1, lcTages1, lcIndExp

*--If Query Successfully executed, Create Indexes if needed for the result cursor
lnOldBuffMode = CURSORGETPROP("Buffering", lcCursor)
=CURSORSETPROP("Buffering", 3, lcCursor)

lcTages1 = lcTages
lcIndex1 = lcIndex
SELECT (lcCursor)
DO WHILE AT("|", lcIndex1,1) <> 0
  lcIndex  = SUBSTR(lcIndex1, 1, AT("|", lcIndex1, 1) - 1)
  lcIndex1 = STRTRAN(lcIndex1, lcIndex + "|", "", 1, 1)
  lcTages  = SUBSTR(lcTages1, 1, AT("|", lcTages1, 1) - 1)
  lcTages1 = STRTRAN(lcTages1, lcTages + "|", "", 1, 1)
  *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
  *INDEX ON &lcIndex. TAG (lcTages) OF (lcCursor)
  INDEX ON &lcIndex. TAG (lcTages)
  *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
ENDDO
=CURSORSETPROP("Buffering", IIF(TYPE("lnBuffMode") = 'N', lnBuffMode, lnOldBuffMode), lcCursor)

RETURN .T.
*--End of lfCreateIndecies()

*!*************************************************************
*! Name      : lfBuildPOGrid
*! Developer : Wael M. Abo-Shawareb
*! Date      : 12/01/2004
*! Purpose   : Build PO Lines Grid Properties
*!*************************************************************
*! Example   : lfBuildPOGrid()
*!*************************************************************
FUNCTION lfBuildPOGrid
LPARAMETERS loFormSet

WITH loFormSet.AriaForm1.grdPOLines
  LOCAL lnI, lnJ, lnAlias
  lnAlias = SELECT(0)
  lnI     = 1

  SELECT (loFormSet.lcMainLin)

  .RecordSource = loFormSet.lcMainLin
  .ChildOrder   = 'POSLN'

  .Columns(lnI).ControlSource   = loFOrmSet.lcMainLin + '.Style'
  .Columns(lnI).Header1.Caption = loFormSet.lcMajHead
  lnI = lnI + 1

  .Columns(lnI).ControlSource   = loFOrmSet.lcMainLin + '.TotQty'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = IIF(loFormSet.lcAllocate == 'P', LANG_SOALOCT_TOTPO, LANG_SOALOCT_TOTCUT)
.Columns(lnI).Header1.Caption = IIF(loFormSet.lcAllocate == 'P', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_TOTPO,loFormSet.GetHeaderText("LANG_SOALOCT_TOTPO",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_TOTCUT,loFormSet.GetHeaderText("LANG_SOALOCT_TOTCUT",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lnI = lnI + 1

  IF loFOrmSet.lcDyeConfig # 'N'
    .Columns(lnI).ControlSource   = loFOrmSet.lcMainLin + '.Dyelot'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = IIF(loFormSet.lcDyeConfig == 'D', LANG_SOALOCT_DYELOT, LANG_SOALOCT_CONFIG)
.Columns(lnI).Header1.Caption = IIF(loFormSet.lcDyeConfig == 'D', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_DYELOT,loFormSet.GetHeaderText("LANG_SOALOCT_DYELOT",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_CONFIG,loFormSet.GetHeaderText("LANG_SOALOCT_CONFIG",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  ELSE
    .Columns(lnI).Visible = .F.
    .Columns(lnI).Width   = 0
  ENDIF
  lnI = lnI + 1

  FOR lnJ = 1 TO 8
    .Columns(lnI).ControlSource   = loFOrmSet.lcMainLin + '.Qty' + STR(lnJ, 1)
    .Columns(lnI).Header1.Caption = loFOrmSet.laBrwQty[lnJ]
    lnI = lnI + 1
  ENDFOR

  .Columns(lnI).ControlSource   = loFOrmSet.lcMainLin + '.TotOrd'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = LANG_SOALOCT_TOTORD
.Columns(lnI).Header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_TOTORD,loFormSet.GetHeaderText("LANG_SOALOCT_TOTORD",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lnI = lnI + 1

  FOR lnJ = 1 TO 8
    .Columns(lnI).ControlSource   = loFOrmSet.lcMainLin + '.Ord' + STR(lnJ, 1)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = LANG_SOALOCT_ORD + STR(lnJ, 1)
.Columns(lnI).Header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_ORD,loFormSet.GetHeaderText("LANG_SOALOCT_ORD",loFormSet.HeaderAlias)) + STR(lnJ, 1)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    lnI = lnI + 1
  ENDFOR

  IF loFOrmSet.llDispPric
    .Columns(lnI).ControlSource   = loFOrmSet.lcMainLin + '.nSelPrice'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = LANG_SOALOCT_SELPRC
.Columns(lnI).Header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_SELPRC,loFormSet.GetHeaderText("LANG_SOALOCT_SELPRC",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    lnI = lnI + 1

    .Columns(lnI).ControlSource   = loFOrmSet.lcMainLin + '.nGrosMrgn'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = LANG_SOALOCT_GRSMRG
.Columns(lnI).Header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_GRSMRG,loFormSet.GetHeaderText("LANG_SOALOCT_GRSMRG",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    lnI = lnI + 1
  ELSE
    .Columns(lnI).Visible = .F.
    .Columns(lnI).Width   = 0
    .Columns(lnI+1).Visible = .F.
    .Columns(lnI+1).Width   = 0
    lnI = lnI + 2
  ENDIF

  .SetAll("FontName", "Tahoma", "Column")
  .SetAll("Alignment", 2, "Header")

  SELECT (lnAlias)
ENDWITH

*!*************************************************************
*! Name      : lfBuildSOGrid
*! Developer : Wael M. Abo-Shawareb
*! Date      : 12/01/2004
*! Purpose   : Build SO Lines Grid Properties
*!*************************************************************
*! Example   : lfBuildSOGrid()
*!*************************************************************
FUNCTION lfBuildSOGrid
LPARAMETERS loFormSet

WITH loFormSet.AriaForm1.grdSOLines
  LOCAL lnI, lnJ, lcCurs, lnAlias
  lnI     = 1
  lcCurs  = loFOrmSet.lcOrderLin
  lnAlias = SELECT(0)

  SELECT (loFOrmSet.lcOrderLin)

  .RecordSource   = lcCurs
  .ChildOrder     = loFormSet.lcOrdStLin
  .LinkMaster     = loFormSet.lcMainLin
  .RelationalExpr = "Style"

  LOCAL lcAllocated
  *B128956,1 KHM 07/13/2005 Change the special character because it cause problems when
  *B128956,1                the PC has other languages like chines.
  *lcAllocated = "IIF(" + lcCurs + ".TotCut <> 0," + ;
                    "IIF(SEEK('" + loFormSet.lcKeyCode + loFormSet.lcKeyFld + "' + STR(" + loFormSet.lcMainLin + ".LineNo, 6) + " + lcCurs + ".Order + " + lcCurs + ".Style + STR(" + lcCurs + ".LineNo,;
                        "IIF(" + lcCurs + ".TotCut = " + loFormSet.lcCutPick + ".TotQty," +;
                            "'*'," +;
                            "'»*')," +;
                        "'»')," +;
                    "'')"
  lcAllocated = "IIF(" + lcCurs + ".TotCut <> 0," + ;
                    "IIF(SEEK('" + loFormSet.lcKeyCode + loFormSet.lcKeyFld + "' + STR(" + loFormSet.lcMainLin + ".LineNo, 6) + " + lcCurs + ".Order + " + lcCurs + ".Style + STR(" + lcCurs + ".LineNo, 6),'" + loFormSet.lcCutPick + "')," + ;
                        "IIF(" + lcCurs + ".TotCut = " + loFormSet.lcCutPick + ".TotQty," +;
                            "'*'," +;
                            "'>>*')," +;
                        "'>>')," +;
                    "'')"
  *B128956,1 KHM 07/13/2005 [End]

  .Columns(lnI).ControlSource   = lcAllocated
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = LANG_SOALOCT_ALLOCT
.Columns(lnI).Header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_ALLOCT,loFormSet.GetHeaderText("LANG_SOALOCT_ALLOCT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lnI = lnI + 1

  .Columns(lnI).ControlSource   = lcCurs + '.Order'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = LANG_SOALOCT_ORDER
.Columns(lnI).Header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_ORDER,loFormSet.GetHeaderText("LANG_SOALOCT_ORDER",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lnI = lnI + 1

  .Columns(lnI).ControlSource   = lcCurs + '.Complete'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = LANG_SOALOCT_COMPLETE
.Columns(lnI).Header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_COMPLETE,loFormSet.GetHeaderText("LANG_SOALOCT_COMPLETE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lnI = lnI + 1

  .Columns(lnI).ControlSource   = lcCurs + '.Account'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = LANG_SOALOCT_ACCOUNT
.Columns(lnI).Header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_ACCOUNT,loFormSet.GetHeaderText("LANG_SOALOCT_ACCOUNT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lnI = lnI + 1

  .Columns(lnI).ControlSource   = lcCurs + '.Store'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = LANG_SOALOCT_STORE
.Columns(lnI).Header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_STORE,loFormSet.GetHeaderText("LANG_SOALOCT_STORE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lnI = lnI + 1

  .Columns(lnI).ControlSource   = lcCurs + '.Style'
  .Columns(lnI).Header1.Caption = loFormSet.lcMajHead
  lnI = lnI + 1

  IF loFOrmSet.lcDyeConfig # 'N'
    .Columns(lnI).ControlSource   = lcCurs + '.Dyelot'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = IIF(loFormSet.lcDyeConfig == 'D', LANG_SOALOCT_DYELOT, LANG_SOALOCT_CONFIG)
.Columns(lnI).Header1.Caption = IIF(loFormSet.lcDyeConfig == 'D', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_DYELOT,loFormSet.GetHeaderText("LANG_SOALOCT_DYELOT",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_CONFIG,loFormSet.GetHeaderText("LANG_SOALOCT_CONFIG",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  ELSE
    .Columns(lnI).Visible = .F.
    .Columns(lnI).Width   = 0
  ENDIF
  lnI = lnI + 1

  .Columns(lnI).ControlSource   = lcCurs + '.TotQty'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = LANG_SOALOCT_TOTORD
.Columns(lnI).Header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_TOTORD,loFormSet.GetHeaderText("LANG_SOALOCT_TOTORD",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lnI = lnI + 1

  FOR lnJ = 1 TO 8
    .Columns(lnI).ControlSource   = lcCurs + '.Qty' + STR(lnJ, 1)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = LANG_SOALOCT_ORD + STR(lnJ, 1)
.Columns(lnI).Header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_ORD,loFormSet.GetHeaderText("LANG_SOALOCT_ORD",loFormSet.HeaderAlias)) + STR(lnJ, 1)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    lnI = lnI + 1
  ENDFOR

  .Columns(lnI).ControlSource   = lcCurs + '.CustPO'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = LANG_SOALOCT_CUSTPO
.Columns(lnI).Header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_CUSTPO,loFormSet.GetHeaderText("LANG_SOALOCT_CUSTPO",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lnI = lnI + 1

  .Columns(lnI).ControlSource   = lcCurs + '.Price'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Columns(lnI).Header1.Caption = LANG_SOALOCT_PRICE
.Columns(lnI).Header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCT_PRICE,loFormSet.GetHeaderText("LANG_SOALOCT_PRICE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lnI = lnI + 1

  .SetAll("FontName", "Tahoma", "Column")
  .SetAll("FontName", "Tahoma", "Header")
  .SetAll("Alignment", 2, "Header")

  *--Set dyenamic colors.
  *LOCAL lcDyClr

  *lcDyClr = "IIF(" + lcCurs + ".cAllocated = '*>>'," +;
                 "16769996," +;
                 "IIF(" + lcCurs + ".cAllocated = '*'," +;
                      "16769806," +;
                      "IIF(" + lcCurs + ".cAllocated = '>>'," +;
                           "12320767," +;
                           "16777215)))"

  *.SetAll("Dynamicbackcolor", "", "Column")
  *.SetAll("Dynamicbackcolor", lcDyClr, "Column")

  SELECT (lnAlias)
ENDWITH

*!*************************************************************
*! Name      : lfRestoreGrid
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/08/2004
*! Purpose   : Remove Grid Control Sources
*!*************************************************************
*! Call      : lfRestoreGrid()
*!*************************************************************
FUNCTION lfRestoreGrid
LPARAMETERS logrdContrrol

LOCAL lnI

WITH logrdContrrol
  .RecordSource = ''
  .LinkMaster   = ''
  .ChildOrder   = ''

  FOR lnI = 1 TO .ColumnCount
    .Columns(lnI).ControlSource  = ''
  ENDFOR
ENDWITH

RETURN

*!*************************************************************
*! Name      : lfClearData
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/08/2004
*! Purpose   : Zap all the temp. files to recollect data...
*!*************************************************************
*! Call      : lfClearData()
*!*************************************************************
FUNCTION lfClearData
LPARAMETERS loFormSet

WITH loFormSet
  *-- Zap all the temp. files to recollect data...
  SELECT (.lcMainLin)
  =TABLEREVERT(.T.)
  =CURSORSETPROP("Buffering", 3)
  ZAP
  =CURSORSETPROP("Buffering", 5)

  SELECT (.lcOrderLin)
  SET FILTER TO
  =TABLEREVERT(.T.)
  =CURSORSETPROP("Buffering", 3)
  ZAP
  =CURSORSETPROP("Buffering", 5)

  SELECT (.lcCutPick)
  =TABLEREVERT(.T.)
  =CURSORSETPROP("Buffering", 3)
  ZAP
  =CURSORSETPROP("Buffering", 5)

  .loOrdHdr.TableRevert()
ENDWITH

RETURN

*!*************************************************************
*! Name      : lfMultiSession
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/08/2004
*! Purpose   : Check if there is another session of the screen in eddit mode
*!*************************************************************
*! Call      : lfMultiSession()
*!*************************************************************
FUNCTION lfMultiSession
LPARAMETERS loFormSet

LOCAL lnI

FOR lnI = 1 TO _Screen.FormCount
  IF TYPE("_Screen.Forms(lnI).Parent.lcCallProg") = 'C' AND ;
     _Screen.Forms(lnI).Parent.lcCallProg = 'SO\SOALOCT.FXP' AND ;
     _Screen.Forms(lnI).Parent.lcKeyFld = loFormSet.lcKeyFld AND ;
     _Screen.Forms(lnI).Parent.ActiveMode = 'E'

    IF gfModalGen("INM00029B00015","ALERT") = 1
      RETURN lfMultiSession(loFormSet)
    ELSE
      RETURN .T.
    ENDIF
  ENDIF
ENDFOR

RETURN .F.
