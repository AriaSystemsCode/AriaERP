*:*************************************************************
*: File      : POBARREC.PRG                                  :*
*: System    : ARIA 4.0 XP                                   :*
*: Modules   : PO                                            :*
*: Program   : Custom Receiving by Bar Code.                 :*
*: Developer : Wael M. Abo-Shawareb  (WSH)                   :*
*: Date      : 08/11/2005                                    :*
*: Issue NO. : 124096,1  ,200621.120 (127096.EXE) 											 :*
*:*************************************************************

DO FORM (oAriaApplication.ScreenHome + 'PO\POBARREC')

*!*************************************************************
*! Name      : lfFormInit
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/11/2005
*! Purpose   : Screen Init Event
*!*************************************************************
*! Example   : =lfFormInit()
*!*************************************************************
FUNCTION lfFormInit
LPARAMETERS loFormSet

LOCAL lnAlias, llLinkToGL, lnShiftTop, lnI
lnAlias    = SELECT(0)
llLinkToGL = (gfGetMemVar('M_Link_GL') = 'Y')

WITH loFormSet
  .lcInvType   = '0001'
  .llLinkToGl  = llLinkToGL
  .llUseConfig = gfGetMemVar('M_STYCNFG') = 'Y'
  .lcDefConfig = gfGetMemVar('M_CONFDEF')
  
  *-- Get the default discount code
  LOCAL lcCodes, loCodes
  lcCodes = gfTempName()
  loCodes = CREATEOBJECT("RemoteTable", "CODES", "CCODE_NO", lcCodes, SET("Datasession"))
  
  .lcDefDiscCode = ''
  IF loCodes.SEEK("DCDISCCODE")
    .lcDefDiscCode = EVALUATE(lcCodes + '.cCode_No')
  ENDIF
  loCodes = .NULL.
  
  *--Open needed table objects
  .lcRecLines = gfTempName()
  .lcSpck_Lin = gfTempName()
  .lcPOSHDR   = gfTempName()
  .lcPOSLN    = gfTempName()
  .lcStyle    = gfTempName()
  .lcStyDye   = gfTempName()
  .lcScale    = gfTempName()
  .lcWareHous = gfTempName()
  .lcTmpPoHdr = gfTempName()
  .loSpck_Lin = CREATEOBJECT("RemoteTable", "Spck_Lin", "SPCK_LIN", .lcSpck_Lin, SET("Datasession"))
  .loPOSHDR   = CREATEOBJECT("RemoteTable", "POSHDR", "POSHDR", .lcPOSHDR, SET("Datasession"))
  .loPOSLN    = CREATEOBJECT("RemoteTable", "POSLN", "POSLNS", .lcPOSLN, SET("Datasession"))
  .loStyle    = CREATEOBJECT("RemoteTable", "Style", "Style", .lcStyle, SET("Datasession"))
  .loStyDye   = CREATEOBJECT("RemoteTable", "StyDye", "StyDye", .lcStyDye, SET("Datasession"))
  .loScale    = CREATEOBJECT("RemoteTable", "Scale", "Scale", .lcScale, SET("Datasession"))
  .loWareHous = CREATEOBJECT("RemoteTable", "WareHous", "WareHous", .lcWareHous, SET("Datasession"))
  
  .Dataenvironment.InitialSelectedAlias = .lcPosHdr
  .cBrowseAliasName = .lcPosHdr
  
  *-- Create temp PO file used to browse open POs for a scanned item
  LOCAL ARRAY laFStru(1)
  LOCAL lnFlds

  =AFIELDS(laFStru, .lcPOSHdr)
  lnFlds = ALEN(laFStru, 1)
  
  DIMENSION laFStru[lnFlds + 2, 18]
  laFStru[lnFlds + 1, 1] = 'nHdrLine'
  laFStru[lnFlds + 1, 2] = 'N'
  laFStru[lnFlds + 1, 3] = 9
  laFStru[lnFlds + 1, 4] = 0
  
  laFStru[lnFlds + 2, 1] = 'nLineNo'
  laFStru[lnFlds + 2, 2] = 'N'
  laFStru[lnFlds + 2, 3] = 9
  laFStru[lnFlds + 2, 4] = 0

  STORE 0 TO laFStru[lnFlds+1,17], laFStru[lnFlds+1,18],;
             laFStru[lnFlds+2,17], laFStru[lnFlds+2,18]
  
  FOR lnI = 7 TO 16
    STORE "" TO laFStru[lnFlds+1,lnI], laFStru[lnFlds+2,lnI]
  ENDFOR
  
  =gfCrtTmp(.lcTmpPoHdr, @laFStru, "STR(nHdrLine,6)+PO", .lcTmpPoHdr)
  
  *-- If the System not linked to GL, hide Postng Date and move controls top
  .AriaForm1.lblPostDate.Visible = llLinkToGL
  .AriaForm1.lblPostDots.Visible = llLinkToGL
  .AriaForm1.txtPostDate.Visible = llLinkToGL
  
  IF !llLinkToGL
    WITH .AriaForm1
      lnShiftTop       = .txtRecDate.Top  - .txtPostDate.Top
      .txtRecDate.Top  = .txtRecDate.Top  - lnShiftTop
      .lblRecDate.Top  = .lblRecDate.Top  - lnShiftTop
      .lblRecDots.Top  = .lblRecDots.Top  - lnShiftTop
      .cmdStart.Top    = .cmdStart.Top    - lnShiftTop
      .gdrLines.Top    = .gdrLines.Top    - lnShiftTop
      .gdrLines.Height = .gdrLines.Height - lnShiftTop
    ENDWITH
  ENDIF
  
  .loWAREHOUS.Seek('')
  WITH .AriaForm1.cboLocation
    lnI = 1
    
    SELECT (loFormSet.lcWAREHOUS)
    SCAN FOR lStyInv
      DIMENSION .asourcearray[lnI,2]
      .asourcearray[lnI,1] = cDesc
      .asourcearray[lnI,2] = cWareCode
      lnI = lnI + 1
    ENDSCAN
    
    .Requery()
  ENDWITH
  
  *--Prepare Browse Fields
  .lcBrowFlds = "PO        :H='PO #',"+;
                "Status    :H='Status', "+;
                "Vendor    :H='Vendor',"+;
                "Entered   :H='Entered',"+;
                "nStyOrder :H='Total Qty.',"+;
                "POTotal   :H='Amount',"+;
                "Receive   :H='Receive',"+;
                "Open      :H='Open'"
  
  *-- Create Temp Lines Cursor
  =lfCrtTmpLines(loFormSet)
  
  *-- Assign field control sources
  .AriaForm1.kbPO.KeyTextBox.ControlSource = .lcRecLines + '.PO'
  .AriaForm1.txtQty.ControlSource          = .lcRecLines + '.TotQty'
  .AriaForm1.cboLocation.ControlSource     = .lcRecLines + '.cWareCode'
  .AriaForm1.txtStyDesc.ControlSource      = .lcRecLines + '.cDesc'
  .AriaForm1.txtReference.ControlSource    = .lcRecLines + '.Reference'
ENDWITH

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfChangeMode
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/11/2005
*! Purpose   : Change Mode Event
*!*************************************************************
*! Example   : =lfChangeMode()
*!*************************************************************
FUNCTION lfChangeMode
LPARAMETERS loFormSet

WITH loFormSet.AriaForm1
  DO CASE
    CASE loFormSet.ActiveMode = 'S'  && Select mode.
      .txtRecDate.Enabled   = .T.
      .txtPostDate.Enabled  = .T.
      .cmdStart.Enabled     = .T.
      .grdLines.Enabled     = .F.
      .txtUPC.Enabled       = .F.
      .txtQty.Enabled       = .F.
      .txtReference.Enabled = .F.
      .kbPO.Enabled         = .F.
      .txtStyDesc.Enabled   = .F.
      .cboLocation.Enabled  = .F.
      .cmdNew.Enabled       = .F.
      .cmdRemove.Enabled    = .F.
      
      .txtRecDate.Value  = oAriaApplication.SystemDate
      .txtPostDate.Value = oAriaApplication.SystemDate
      
    CASE loFormSet.ActiveMode = 'A'  && Add mode.
      .txtRecDate.Enabled   = .F.
      .txtPostDate.Enabled  = .F.
      .txtStyDesc.Enabled   = .F.
      .cmdStart.Enabled     = .F.
      .kbPO.Enabled         = .T.
      .grdLines.Enabled     = .T.
      .txtUPC.Enabled       = .T.
      .txtQty.Enabled       = .T.
      .cboLocation.Enabled  = .T.
      .txtReference.Enabled = .F.
      .cmdNew.Enabled       = .F.
      .cmdRemove.Enabled    = !EOF(loFormSet.lcRecLines)
      
      .txtUPC.SetFocus()
  ENDCASE
  
  *-- Clear records in the Tenp Lines Cursor and rebuild Grid
  SELECT (loFormSet.lcRecLines)
  =TABLEREVERT(.T.)
  =CURSORSETPROP("Buffering", 3)
  ZAP
  =CURSORSETPROP("Buffering", 5)
  
  SELECT (loFormSet.lcTmpPoHdr)
  ZAP
  
  loFormSet.nLines = 0
  
  .grdLines.RecordSource = ''
  =lfBuildGrdLines(loFormSet)
ENDWITH

*!*************************************************************
*! Name      : lfCrtTmpLines
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/11/2005
*! Purpose   : Create temp Lines Cursor
*!*************************************************************
*! Example   : =lfCrtTmpLines()
*!*************************************************************
FUNCTION lfCrtTmpLines
LPARAMETERS loFormSet

LOCAL lnFlds, lnI
DIMENSION laStru[1,18]
=AFIELDS(laStru, loFormSet.lcPOSLN)
lnFlds = ALEN(laStru,1)

DIMENSION laStru[lnFlds + 9,18]

laStru[lnFlds+1,1] = 'UPC'
laStru[lnFlds+1,2] = 'C'
laStru[lnFlds+1,3] = 20
laStru[lnFlds+1,4] = 0

laStru[lnFlds+2,1] = 'cDesc'
laStru[lnFlds+2,2] = 'C'
laStru[lnFlds+2,3] = 20
laStru[lnFlds+2,4] = 0

laStru[lnFlds+3,1] = 'cSize'
laStru[lnFlds+3,2] = 'C'
laStru[lnFlds+3,3] = 10
laStru[lnFlds+3,4] = 0

laStru[lnFlds+4,1] = 'lNewLn'
laStru[lnFlds+4,2] = 'L'
laStru[lnFlds+4,3] = 1
laStru[lnFlds+4,4] = 0

laStru[lnFlds+5,1] = 'lDetCost'
laStru[lnFlds+5,2] = 'L'
laStru[lnFlds+5,3] = 1
laStru[lnFlds+5,4] = 0

laStru[lnFlds+6,1] = 'cDye_Flg'
laStru[lnFlds+6,2] = 'C'
laStru[lnFlds+6,3] = 1
laStru[lnFlds+6,4] = 0

laStru[lnFlds+7,1] = 'lNewLUpd'
laStru[lnFlds+7,2] = 'L'
laStru[lnFlds+7,3] = 1
laStru[lnFlds+7,4] = 0

laStru[lnFlds+8,1] = 'cLastOpr'
laStru[lnFlds+8,2] = 'C'
laStru[lnFlds+8,3] = 6
laStru[lnFlds+8,4] = 0

laStru[lnFlds+9,1] = 'nHdrLine'
laStru[lnFlds+9,2] = 'N'
laStru[lnFlds+9,3] = 9
laStru[lnFlds+9,4] = 0

STORE 0 TO laStru[lnFlds+1,17], laStru[lnFlds+1,18], laStru[lnFlds+2,17], laStru[lnFlds+2,18],;
           laStru[lnFlds+3,17], laStru[lnFlds+3,18], laStru[lnFlds+4,17], laStru[lnFlds+4,18],;
           laStru[lnFlds+5,17], laStru[lnFlds+5,18], laStru[lnFlds+6,17], laStru[lnFlds+6,18],;
           laStru[lnFlds+7,17], laStru[lnFlds+7,18], laStru[lnFlds+8,17], laStru[lnFlds+8,18],;
           laStru[lnFlds+9,17], laStru[lnFlds+9,18]

FOR lnI = 7 TO 16
  STORE "" TO laStru[lnFlds+1,lnI], laStru[lnFlds+2,lnI], laStru[lnFlds+3,lnI],;
              laStru[lnFlds+4,lnI], laStru[lnFlds+5,lnI], laStru[lnFlds+6,lnI],;
              laStru[lnFlds+7,lnI], laStru[lnFlds+8,lnI], laStru[lnFlds+9,lnI]
ENDFOR

=gfCrtTmp(loFormSet.lcRecLines, @laStru, "cInvType+Style+cBusDocu+cStyType+PO+STR(LineNo,6)+TranCd", loFormSet.lcRecLines)
SELECT (loFormSet.lcRecLines)
INDEX ON UPC TAG UPC ADDITIVE
INDEX ON PO+Style+Dyelot+cWareCode+STR(LineNo,6)+TranCd TAG TmpLine3 ADDITIVE
SET ORDER TO (loFormSet.lcRecLines)
=CURSORSETPROP("Buffering", 5)

*!*************************************************************
*! Name      : lfBuildGrdLines
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/11/2005
*! Purpose   : Assign Grid Properties and Control sources
*!*************************************************************
*! Example   : =lfBuildGrdLines()
*!*************************************************************
FUNCTION lfBuildGrdLines
LPARAMETERS loFormSet

WITH loFormSet.AriaForm1.grdLines
  .RecordSource = loFOrmSet.lcRecLines
  
  .Column1.ControlSource = loFOrmSet.lcRecLines + '.PO'
  .Column2.ControlSource = loFOrmSet.lcRecLines + '.Style'
  .Column3.ControlSource = loFOrmSet.lcRecLines + '.cSize'
  .Column4.ControlSource = loFOrmSet.lcRecLines + '.cDesc'
  .Column5.ControlSource = loFOrmSet.lcRecLines + '.cWareCode'
  .Column6.ControlSource = loFOrmSet.lcRecLines + '.Dyelot'
  *.Column7.ControlSource = loFOrmSet.lcRecLines + '.cStatus'
  .Column8.ControlSource = loFOrmSet.lcRecLines + '.TotQty'
ENDWITH

*!*************************************************************
*! Name      : lfvUPC
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/11/2005
*! Purpose   : Valid function for the UPC field
*!*************************************************************
*! Example   : =lfvUPC()
*!*************************************************************
FUNCTION lfvUPC
LPARAMETERS loFormSet

*****************************************************************************************
**** WSH Note: !!!!                                                                  ****
****       As per customer, we will use the scanned number to search for             ****
****       its existance in the Spck_Lin file "Pack_Id field" . We will assume that  ****
****       the SKU number can't be repeated for two customers ...!!!                 ****
*****************************************************************************************

PRIVATE lcBrFields
lcBrFields = loFormSet.lcBrowFlds

LOCAL lnAlias, llBrowse, lnOpenPos, lcValue, lcPackId, lnI, lcStyle, lnSize, lcSize, lnHdrLine
lnAlias   = SELECT(0)
lnOpenPos = 0
lcValue   = loFormSet.AriaForm1.txtUPC.Value
lcPackId  = PADR(lcValue,7)

IF loFormSet.loSpck_Lin.Seek('S')
  SELECT (loFormSet.lcSpck_Lin)
  LOCATE REST WHILE TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT = 'S' FOR lcPackId $ PACK_ID
  IF !FOUND()
    =gfModalGen("TRM34214B00000", "Dialog", lcPackId)
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
  
  lnHdrLine = loFormSet.nLines + 1
  lcStyle   = Style
  lnSize    = 1
  
  FOR lnI = 1 TO 8
    IF EVALUATE('Qty' + STR(lnI,1)) > 0
      lnSize = lnI
      EXIT
    ENDIF
  ENDFOR
  lcSize = STR(lnSize,1)
  
  IF loFormSet.loPOSLN.Seek(loFormSet.lcInvType + lcStyle)
    SELECT (loFormSet.lcPOSLN)
    SCAN REST WHILE cInvType+Style+cBusDocu+cStyType+PO+STR(LineNo,6)+TranCd = loFormSet.lcInvType + lcStyle FOR TranCd = '1'
      IF !loFormSet.loPOSHDR.Seek(cBusDocu + cStyType + PO) OR !(EVALUATE(loFormSet.lcPOSHDR + '.Status') = 'O') OR SEEK(STR(lnHdrLine,6)+PO, loFormSet.lcTmpPoHdr)
        LOOP
      ENDIF
      lnOpenPos = lnOpenPos + 1
      
      SELECT (loFormSet.lcPOSHDR)
      SCATTER MEMVAR
      SELECT (loFormSet.lcTmpPoHdr)
      APPEND BLANK
      GATHER MEMVAR
      REPLACE cWareCode WITH EVALUATE(loFormSet.lcPOSLN + '.cWareCode'),;
              nLineNo   WITH EVALUATE(loFormSet.lcPOSLN + '.LineNo'),;
              nHdrLine  WITH lnHdrLine
    ENDSCAN
  ENDIF
  
  SELECT (loFormSet.lcTmpPoHdr)
  IF lnOpenPos = 0
    =gfModalGen("TRM34215B00000", "Dialog", lcStyle)
    SELECT (lnAlias)
    RETURN .F.
  ELSE
    IF lnOpenPos > 1
      DIMENSION laTemp[1]
      LOCATE
      llBrowse = AriaBrow("'" + STR(lnHdrLine,6) + "' FOR nHdrLine = " + STR(lnHdrLine,6), "Open Purchase Orders", .F., .F., .F., .F., '', '', 'PO', 'laTemp')
      
      IF !llBrowse OR EMPTY(laTemp[1])
        *=gfModalGen("TRM00000B00000", "Dialog", .F., .F., "No Purchase Order Selected. Cannot proceed.")
        SELECT (lnAlias)
        RETURN .F.
      ENDIF
    ENDIF
  ENDIF
  
  SELECT (loFormSet.lcTmpPoHdr)
  loFormSet.loPOSLN.Seek(loFormSet.lcInvType + lcStyle + cBusDocu + cStyType + PO + STR(nLineNo,6) + '1')
  
  SELECT (loFormSet.lcPOSLN)
  SCATTER MEMVAR
  
  SELECT (loFOrmSet.lcRecLines)
  IF !loFormSet.llNew
    APPEND BLANK
  ENDIF
  
  REPLACE UPC         WITH lcValue,;
          cBusDocu    WITH m.cBusDocu,;
          cStyType    WITH m.cStyType,;
          PO          WITH m.PO,;
          cInvType    WITH m.cInvType,;
          Style       WITH m.Style,;
          LineNo      WITH m.LineNo,;
          TranCD      WITH '2',;
          cWareCode   WITH EVALUATE(loFormSet.lcTmpPoHdr + '.cWareCode'),;
          Dyelot      WITH EVALUATE(loFormSet.lcSpck_Lin + '.Dyelot'),;
          Pack_Id     WITH lcPackId,;
          Qty&lcSize. WITH 1,;
          TotQty      WITH 1,;
          nFCost1     WITH m.nFCost1,;
          nFCost2     WITH m.nFCost2,;
          nFCost3     WITH m.nFCost3,;
          nFCost4     WITH m.nFCost4,;
          nFCost5     WITH m.nFCost5,;
          nFCost6     WITH m.nFCost6,;
          nFCost7     WITH m.nFCost7,;
          nICost1     WITH m.nICost1,;
          nICost2     WITH m.nICost2,;
          nICost3     WITH m.nICost3,;
          nICost4     WITH m.nICost4,;
          nICost5     WITH m.nICost5,;
          nICost6     WITH m.nICost6,;
          nICost7     WITH m.nICost7,;
          cCstSht_Id  WITH m.cCstSht_Id,;
          Vendor      WITH m.Vendor,;
          ShipVia     WITH m.ShipVia,;
          Scale       WITH m.Scale,;
          cUOMCode    WITH m.cUOMCode,;
          cStyGrade   WITH m.cStyGrade,;
          cPackType   WITH m.cPackType,;
          Gros_Price  WITH m.Gros_Price,;
          nLanPrRat   WITH EVALUATE(loFormSet.lcTmpPoHdr + '.nPriceRat'),;
          nLanDuRat   WITH EVALUATE(loFormSet.lcTmpPoHdr + '.nDutyRat'),;
          cLotNo      WITH '01'
  
  =loFormSet.loStyle.Seek(lcStyle)
  =loFormSet.loScale.Seek('S'+EVALUATE(loFormSet.lcStyle + '.Scale'))
  
  REPLACE nHdrLine    WITH lnHdrLine,;
          cSize       WITH EVALUATE(loFormSet.lcScale + '.Sz' + lcSize),;
          cDesc       WITH EVALUATE(loFormSet.lcStyle + '.Desc1'),;
          lDetCost    WITH EVALUATE(loFormSet.lcStyle + '.lDetCost'),;
          cDye_Flg    WITH EVALUATE(loFormSet.lcStyle + '.cDye_Flg'),;
          lNewLn      WITH .F.,;
          lNewLUpd    WITH .F.,;
          cLastOpr    WITH EVALUATE(loFormSet.lcTmpPoHdr + '.cLastOpr')
  
  =gfAdd_Info(loFOrmSet.lcRecLines, loFormSet)
  
  loFormSet.llNew  = .F.
  loFormSet.nLines = lnHdrLine
  
  WITH loFormSet.AriaForm1
    .txtUPC.Value        = ''
    .kbPO.Enabled        = lnOpenPos > 1
    .cmdRemove.Enabled   = !EOF(loFormSet.lcRecLines)
    .cmdNew.Enabled      = .F.
    .txtUPC.Enabled      = .T.
    .cmdNew.Enabled      = .F.
    .txtQty.Enabled      = .T.
    .cboLocation.Enabled = .T.
    
    .Refresh()
  ENDWITH
ELSE
  =gfModalGen("TRM34214B00000", "Dialog", lcPackId)
ENDIF

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfLineChanged
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/11/2005
*! Purpose   : Valid function for Line Change in Data Grid
*!*************************************************************
*! Example   : =lfLineChanged()
*!*************************************************************
FUNCTION lfLineChanged
LPARAMETERS loFormSet

WITH loFormSet.AriaForm1
  .txtUPC.Value   = EVALUATE(loFOrmSet.lcRecLines + '.UPC')
  .txtUPC.Enabled = .F.
  .cmdNew.Enabled = .T.
  
  .Refresh()
ENDWITH

*!*************************************************************
*! Name      : lfvNew
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/11/2005
*! Purpose   : Valid function for New Button
*!*************************************************************
*! Example   : =lfvNew()
*!*************************************************************
FUNCTION lfvNew
LPARAMETERS loFormSet

LOCAL lnAlias
lnAlias = SELECT(0)

WITH loFormSet.AriaForm1
  SELECT (loFOrmSet.lcRecLines)
  APPEND BLANK
  
  .txtUPC.Value        = ''
  .txtUPC.Enabled      = .T.
  .cmdNew.Enabled      = .F.
  .txtQty.Enabled      = .F.
  .kbPO.Enabled        = .F.
  .cboLocation.Enabled = .F.
  
  .txtUPC.SetFocus()
  loFormSet.llNew = .T.
  
  .Refresh()
ENDWITH

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/11/2005
*! Purpose   : Valid function for Remove Button
*!*************************************************************
*! Example   : =lfvRemove()
*!*************************************************************
FUNCTION lfvRemove
LPARAMETERS loFormSet

LOCAL lnAlias
lnAlias = SELECT(0)

WITH loFormSet.AriaForm1
  SELECT (loFOrmSet.lcRecLines)
  DELETE
  LOCATE
  
  .cmdRemove.Enabled = !EOF(loFormSet.lcRecLines)
  
  =lfLineChanged(loFormSet)
  .Refresh()
ENDWITH

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfvPO
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/11/2005
*! Purpose   : Valid function for PO Field
*!*************************************************************
*! Example   : =lfvPO()
*!*************************************************************
FUNCTION lfvPO
LPARAMETERS loFormSet, lcValue

LOCAL lnAlias, lnHdrLine, lcOldVal
lnAlias   = SELECT(0)
lnHdrLine = EVALUATE(loFormSet.lcRecLines + '.nHdrLine')
lcOldVal  = loFormSet.AriaForm1.kbPO.KeyTextBox.OldValue
lcStyle   = EVALUATE(loFormSet.lcRecLines + '.Style')

PRIVATE lcBrFields
lcBrFields = loFormSet.lcBrowFlds

SELECT (loFormSet.lcTmpPoHdr)
IF EMPTY(lcValue) OR !SEEK(STR(lnHdrLine,6)+lcValue)
  IF EMPTY(lcValue)
    LOCATE
  ENDIF
  
  DIMENSION laTemp[1]
  llBrowse = AriaBrow("'" + STR(lnHdrLine,6) + "' FOR nHdrLine = " + STR(lnHdrLine,6), "Open Purchase Orders", .F., .F., .F., .F., '', '', 'PO', 'laTemp')
  
  IF !llBrowse OR EMPTY(laTemp[1])
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
  
  IF !(ALLTRIM(laTemp[1]) == ALLTRIM(lcOldVal))
    loFormSet.loPOSLN.SEEK(loFormSet.lcInvType + lcStyle + cBusDocu + cStyType + PO + STR(nLineNo,6) + '1')
    
    SELECT (loFormSet.lcPOSLN)
    SCATTER MEMVAR
    
    SELECT (loFormSet.lcRecLines)
    REPLACE PO          WITH m.PO,;
            LineNo      WITH m.LineNo,;
            cWareCode   WITH EVALUATE(loFormSet.lcTmpPoHdr + '.cWareCode'),;
            nFCost1     WITH m.nFCost1,;
            nFCost2     WITH m.nFCost1,;
            nFCost3     WITH m.nFCost1,;
            nFCost4     WITH m.nFCost1,;
            nFCost5     WITH m.nFCost1,;
            nFCost6     WITH m.nFCost1,;
            nFCost7     WITH m.nFCost1,;
            nICost1     WITH m.nICost1,;
            nICost2     WITH m.nICost2,;
            nICost3     WITH m.nICost3,;
            nICost4     WITH m.nICost4,;
            nICost5     WITH m.nICost5,;
            nICost6     WITH m.nICost6,;
            nICost7     WITH m.nICost7,;
            cCstSht_Id  WITH m.cCstSht_Id,;
            cLastOpr    WITH EVALUATE(loFormSet.lcTmpPoHdr + '.cLastOpr'),;
            Vendor      WITH m.Vendor,;
            ShipVia     WITH m.ShipVia,;
            Scale       WITH m.Scale,;
            cUOMCode    WITH m.cUOMCode,;
            cStyGrade   WITH m.cStyGrade,;
            cPackType   WITH m.cPackType,;
            Gros_Price  WITH m.Gros_Price,;
            nLanPrRat   WITH EVALUATE(loFormSet.lcTmpPoHdr + '.nPriceRat'),;
            nLanDuRat   WITH EVALUATE(loFormSet.lcTmpPoHdr + '.nDutyRat'),;
            cLotNo      WITH '01'
  ENDIF
  
  loFormSet.AriaForm1.kbPO.KeyTextBox.Value = laTemp[1]
  loFormSet.AriaForm1.Refresh()
ENDIF

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfvQty
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/11/2005
*! Purpose   : Valid function for Quantity Field...
*!*************************************************************
*! Example   : =lfvQty()
*!*************************************************************
FUNCTION lfvQty
LPARAMETERS loFormSet

LOCAL lnI, lnValue, lnSizeNo
lnValue = loFormSet.AriaForm1.txtQty.Value

IF lnValue <= 0
  =gfModalGen('TRM34216B00000','DIALOG')
  RETURN .F.
ENDIF

FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  IF Qty&lcI. > 0
    REPLACE Qty&lcI. WITH lnValue IN (loFormSet.lcRecLines)
    EXIT
  ENDIF
ENDFOR

loFormSet.AriaForm1.Refresh()
RETURN .T.

*:*************************************************************
*! Name      : lfvPostDat
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/11/2005
*! Purpose   : Valid the Posting date.
*:*************************************************************
*! Called    : From valid of dtPickerPostingDate in the formset
*!*************************************************************
FUNCTION lfvPostDat
LPARAMETERS loFormSet

PRIVATE lcGLFYear,lcGLPeriod

*-- Fiscal yeare and period
STORE '' TO lcGLFYear,lcGLPeriod

WITH loFormSet.AriaForm1
  IF loFormSet.llLinkToGl
    IF !CHECKPRD(.txtPostDate.Value, 'lcGLFYear', 'lcGLPeriod', 'PO')
      .txtPostDate.Value = .txtPostDate.OldValue
      RETURN .F.
    ENDIF
    loFormSet.lcGLFYear  = lcGLFYear
    loFormSet.lcGLPeriod = lcGLPeriod
  ELSE
    .txtRecDate.Value = .txtPostDate.Value
    =lfvRecvDat(loFormSet)
  ENDIF
ENDWITH

RETURN .T.

*:*************************************************************
*! Name      : lfvRecvDat
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/11/2005
*! Purpose   : Valid the Receive date.
*:*************************************************************
*! Called    : From valid of dtPickerReceivingDate in the formset
*!*************************************************************
FUNCTION lfvRecvDat
LPARAMETERS loFormSet

WITH loFormSet.AriaForm1
  IF loFormSet.llLinkToGl AND .txtRecDate.Value > .txtPostDate.Value
    = gfModalGen('TRM42106B42000','DIALOG','receiving')
    RETURN .F.
  ELSE
    PRIVATE lcGLFYear,lcGLPeriod
    STORE '' TO lcGLFYear,lcGLPeriod
    
    IF !lfCHECKPRD(.txtRecDate.Value,'lcGLFYear','lcGLPeriod',loFormSet)
      RETURN .F.
    ENDIF
    
    loFormSet.lcGLFYear  = lcGLFYear
    loFormSet.lcGLPeriod = lcGLPeriod
  ENDIF
ENDWITH

RETURN .T.

*!**************************************************************************
*! Name      : lfCheckprd
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : Function To validate the receiving date in case of GL not installed
*!**************************************************************************
*! Calls     : gfModalGen,gfOpenFile
*!**************************************************************************
*! Parameters: ldDate   : Transaction date to be check
*!       : lcPeriod : Transaction Period
*!           : lcFYear  : Transaction Fiscal Year 
*!**************************************************************************
*! Example   :  =lfCheckprd()
*!**************************************************************************
FUNCTION lfCheckprd
PARAMETERS ldDate,lcFYear,lcPeriod, loFormSet

PRIVATE llContinue,lcErrorM1,lnAlias, lcSqlStatement
lnAlias = SELECT()
llContinue = .T.

lcSqlStatement = "SELECT Cfisfyear, Cfspprdid, Dfsppbgdt, Dfsppendt "+;
                 " FROM FSPRD "+;
                 " WHERE BETWEEN ('"+DTOS(ldDate)+"' ,DTOS(Dfsppbgdt),DTOS(Dfsppendt))"

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatement,"FSPRD","FSPRD",;
                      oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',loFormSet.DataSessionId)

IF lnConnectionHandlar <> 1
  llContinue = .F.
ENDIF

LOCATE
IF FOUND()
  LOCATE REST FOR BETWEEN(ldDate,Dfsppbgdt,Dfsppendt) WHILE (ldDate >= Dfsppbgdt)
ENDIF
IF !FOUND()                  && No period match checked date
  llContinue = .F.
ELSE
  &lcFYear  = Cfisfyear      && Transaction date year
  &lcPeriod = Cfspprdid      && Transaction date period     
ENDIF  
IF !llContinue             && There is an error.
  lcErrorM1  = 'This receiving date '
  lcErrorM1 =  lcErrorM1 + Dtoc(ldDate) + ' does not fall within any period. '
  lcErrorM1 = lcErrorM1 + 'Would you like to continue any way ?'
  IF gfModalGen('INM00274B34001','ALERT',lcErrorM1)=1
    SELECT (lnAlias)
    RETURN(.T.)
  ELSE
    SELECT (lnAlias)
    RETURN(.F.)
  ENDIF
ENDIF
SELECT (lnAlias)
RETURN(.T.)

*!*************************************************************
*! Name      : lfvWareCode
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/11/2005
*! Purpose   : Valid function for ware house field
*!*************************************************************
*! Example   : =lfvWareCode()
*!*************************************************************
FUNCTION lfvWareCode
LPARAMETERS loFormSet

LOCAL lnAlias, lnChoice, lcStyle, lcWareCode, lcTrnCode, lnConnHandler
lnAlias    = SELECT(0)
lcStyle    = EVALUATE(loFormSet.lcRecLines + '.Style')
lcWareCode = loFormSet.AriaForm1.cboLocation.Value

*-- Check the existance of the Style Warehouse in StyDye file.
IF !loFormSet.loStyDye.Seek(lcStyle + lcWareCode)
  *-Style: xxx is not assigned to warehouse: xxx. "\<Add;\<Reenter"
  lnChoice = gfModalGen('TRM42025B42006','DIALOG',lcStyle + '|' + lcWareCode)
  
  IF lnChoice = 1
    loFormSet.loStyle.Seek(lcStyle)    
    
    loFormSet.loStyDye.Append()
    SELECT (loFormSet.loStyDye.lcCursorUpdate)
    REPLACE STYLE     WITH lcStyle,;
            Dyelot    WITH SPACE(10),;
            cWareCode WITH lcWareCode,;
            Desc      WITH EVALUATE(loFormSet.lcStyle + '.Desc'),;
            Gl_Link   WITH 'DEFDEF'  ,;
            Ave_Cost  WITH EVALUATE(loFormSet.lcStyle + '.Ave_cost'),;
            cDiscCode WITH loFormSet.lcDefDiscCode
    =gfAdd_Info(loFormSet.loStyDye.lcCursorUpdate, loFormSet)
    SCATTER MEMVAR MEMO
    
    IF loFormSet.llUseConfig AND EVALUATE(loFormSet.lcStyle + '.cDye_Flg') = 'Y' AND !EMPTY(ThisFormSet.lcDefConfCode)
       APPEND BLANK
       GATHER MEMVAR MEMO
       REPLACE Dyelot WITH loFormSet.lcDefConfig
       =gfAdd_Info(loFormSet.loStyDye.lcCursorUpdate, loFormSet)
    ENDIF
    
    *-- Begin Updating Transaction
    lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr, 3, '')
    
    *-- Check Resule for Begin Transaction
    IF TYPE('lcTranCode') = 'N'
      =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran", lcTranCode, .T.)
      RETURN .F.
    ENDIF
    
    loFormSet.loStyDye.TableUpdate(lcTrnCode)
    
    *-- Commit Changes and Check Result
    lnConnHandler = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
    IF lnConnHandler <> 1
      =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran", lnConnHandler, .T.)
      =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
      SELECT (lnAlias)
      Return .F.
    ENDIF
  ELSE
    RETURN .F.
  ENDIF
ENDIF

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfSavePO
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/11/2005
*! Purpose   : Save function
*!*************************************************************
*! Example   : =lfSavePO()
*!*************************************************************
FUNCTION lfSavePO
LPARAMETERS loFormSet

DO lfSaveRec WITH loFormSet.lcInvType ,'I',loFormSet.lcRecLines,;
                  loFormSet.ariaForm1.txtPostDate.value,;
                  loFormSet.ariaForm1.txtRecDate.value,'',.F.,.F.,.F.,.F.,0,0,0 ;
             IN (oAriaApplication.ProgramHome+'POSTREC')

RETURN .T.
