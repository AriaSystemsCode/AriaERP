*:************************************************************************
*: File      : POACFRV.PRG                                              :*
*: System    : ARIA 4.0 XP                                              :*
*: Modules   : MF ,PO ,MA                                               :*
*: Program   : Adjust costs for Receiving.                              :*
*: Developer : Wael M. Abo-Shawareb                                     :*
*: Issue NO. : 037551,1                                                 :*
*:************************************************************************
*: Program Types :  lcPType =>'I' Style P.O. Adjust Costs               :* 
*:                                Shipment Adjust Costs -> llByShp=.T.  :* 
*:                            'M' Cutting Ticket Adjust Costs           :* 
*:                            'T' MFG Order Adjust Costs                :* 
*:************************************************************************
*B607585,1 AMH Add nLineNo field to BomLine table.
*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[T20061128.0001]
*B608510,1 MMT 04/02/2008 Convert Shipment Cost sheet program to ARIA4[T20061128.0001]
*B608510,2 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[T20061128.0001]
*B608510,3 MMT 05/29/2008 Convert Shipment Cost sheet program to ARIA4[T20061128.0001]
*B608667,1 MMT 08/27/2008 Fix bug of wrong unt cost in case of foreign currency [T20061128.0001]
*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[t20080924.0005]
*B609356,1 SMA 07/21/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*B610211,1 HIA 01/22/2013 Aria4xp - PO - Adjust Costs for Receiving by Shipment[T20130121.0001]
*:************************************************************************
#INCLUDE R:\ARIA4XP\PRGS\POACFRV.H

LOCAL lcModUsedIn, lcPType

lcModUsedIn = 'PO-I,MF-M,MA-T'
lcPType     = SUBSTR(lcModUsedIn, ATC(oAriaApplication.ActiveModuleID, lcModUsedIn) + 3, 1)



*--System has not been setup to use detailed costing,
*--Therefore the adjust costs for receivings is not available.
IF lcPType = 'I' AND !gfGetMemVar('M_LImpCost')
  =gfModalGen('TRM34085B00000','DIALOG')
  RETURN
ENDIF

DO FORM (oAriaApplication.ScreenHome + "POACFRV") WITH lcPType

*--End [POACFRV.PRG]...


*!*************************************************************
*! Name      : lfFormInit
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/25/2004
*! Purpose   : Initialize Screen Properties.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : oFormSet --> FormSet Object Reference
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfFormInit()
*!*************************************************************
FUNCTION lfFormInit
LPARAMETERS oFormSet

*--Screen Variables.
WITH oFormSet
  STORE .F. TO .llRefresh, .llGetInfo, .llGenerate
  STORE ' ' to .lcBldAct

  .lcStyFbGrd    = ''
  .llGridNav     = .T.
  .lnGridLastRow = 0

  .AriaForm1.kbPO.KeyTextBox.Value = SPACE(6)
  .AriaForm1.txtBudget.Value   = 0
  .AriaForm1.txtEntered.Value  = {}
  .AriaForm1.txtComplete.Value = {}

  DIMENSION .laPanelObj[2,6]
  .laPanelObj[1,1] = 'cmdRefrs'
  .laPanelObj[1,2] = oAriaApplication.BitmapHome + "REFRESH.BMP"
  .laPanelObj[1,3] = "mvRefresh"
  .laPanelObj[1,4] = LANG_POACFRV_REFRESH
  .laPanelObj[1,5] = LANG_POACFRV_REFRESH
  .laPanelObj[1,6] = "E"
  
  .laPanelObj[2,1] = 'cmdCostF'
  .laPanelObj[2,2] = oAriaApplication.BitmapHome + "COSTFORC.BMP"
  .laPanelObj[2,3] = "mvCstFore"
  .laPanelObj[2,4] = LANG_POACFRV_FORECAST
  .laPanelObj[2,5] = LANG_POACFRV_FORECAST
  .laPanelObj[2,6] = "VAE"

  *--Flag to control if Adjust cost by Shipment.
  
   .llByShp = (.lcPType = 'I' AND (gfGetMemVar('M_cCostImp') = 'S'))
  

   IF .llByShp
   
     
     =gfOpenTable('SHPCSTHD','SHPCSTHD','SH')
     gfSeek("",'SHPCSTHD')
     *SELECT  ' '+CTMPLTDSC,CTEMPLATE FROM SHPCSTHD INTO ARRAY .laTemplate ORDER BY  ldefault,ctemplate
     SELECT  ' '+CTMPLTDSC,CTEMPLATE FROM SHPCSTHD INTO ARRAY oFormSet.laTemplate ORDER BY  ldefault DESC 
     IF EMPTY(.laTemplate[1,2])
       =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_POACFRV_No_TEMPLATE )
	  RETURN
     ENDIF 
   ENDIF   
  
  .lcTitle = IIF(.lcPType = 'M', LANG_POACFRV_CUTTKT,;
                 IIF(.lcPType = 'T', LANG_POACFRV_MFGORD,;
                     IIF(.llByShp, LANG_POACFRV_SHPMNT, LANG_POACFRV_POORD)))

  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
  IF !(.lcPType = 'I')           
    WITH .AriaForm1
      STORE .F. TO .grdShpCost.Visible,.cmdApprove.Visible ,.cboSelect.Visible ,.shpSelect.Visible ,;
                   .shpHeaderData.Visible ,.lblShpNo.Visible,.lblTemplate.Visible,;
                   .cboTemplates.Visible,.lblEnterShp.Visible, .txtShpEntered.Visible,.lblCopmShp.Visible,;
                   .txtCompShp.Visible,.lblContainer.Visible,.txtContainer.Visible ,.lblrefer.Visible ,;
                   .txtReference.Visible,.lblNotes.Visible,.txtNotes.Visible,.lblVessel.Visible,;
                   .txtAirVessel.Visible,.cboCostItems.Visible ,.shpCostItems.Visible,.lblAmnt.Visible,;
                   .txtAmnt.Visible,.lblPercentage.Visible,.txtPercent.Visible, .cmdDistribute.Visible,;
                   .shpUpdate.Visible,.chkUpdateAll.Visible,.cmdApprove.Visible,.shpDetails.Visible,;
                   .lblStydesc.Visible,.lblClrDesc.Visible ,.lblScale.Visible,.txtStyDesc.Visible ,;
                   .txtClrDesc.Visible, .txtScaleDesc.Visible,.sbQuantites.Visible,.lblCostItems.Visible,.chkFreight.Visible

      .shpDatesBud.TOP = .shpDatesBud.TOP - 31
      .lblTktNo.TOP  =.lblTktNo.TOP - 31
      .shpTktNo.TOP  =.shpTktNo.TOP - 31
      .grdCostInfo.TOP  = .grdCostInfo.TOP - 31 
      
      .kbPO.TOP =.kbPO.TOP  - 31
      .lblEntered.TOP  =.lblEntered.TOP  - 31
      .txtEntered.TOP  =.txtEntered.TOP - 31
      .lblComplete.TOP  =.lblComplete.TOP - 31
      .txtComplete.TOP =.txtComplete.TOP  - 31
      .lblBudget.TOP  =.lblBudget.TOP - 31
      .txtBudget.TOP  =.txtBudget.TOP  - 31
      .lblTktNoDots.TOP  =.lblTktNoDots.TOP - 31
      
    ENDWITH 
  ENDIF 
  
  .llShpApprove	= (.lcPType = 'I' AND  gfGetMemVar('M_APRVSHIP'))
  .llShowMrg    = (.lcPType = 'I' AND .llShpApprove AND  gfUserPriv('PO','POACFRV','SHOWMRG'))
  .llShpCsUsr   = (.lcPType = 'I' AND .llShpApprove AND  gfUserPriv('PO','POACFRV','APRVSHPC'))
  
  IF !.llShpApprove
    .AriaForm1.cmdApprove.Visible = .F.
  ELSE
    IF .llShpCsUsr 
       .AriaForm1.cmdApprove.Visible = .T.
    ENDIF 
  ENDIF 
  IF .lcPType = 'I'
    .AriaForm1.cboSelect.Enabled= .T.
    .AriaForm1.txtPercent.Value =0
    .AriaForm1.txtAmnt.Value=0
  ENDIF 
  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]

  *--Assign Caption for TktNo and Pieces Labels
  .AriaForm1.lblTktNo.Caption = IIF(.lcPType = 'M', LANG_POACFRV_CUTTKTNO,;
                                    IIF(.lcPType = 'T', LANG_POACFRV_MFGORDNO,;
                                        IIF(.llByShp, LANG_POACFRV_SHPMNTNO, LANG_POACFRV_POORDNO)))
  .AriaForm1.lblBudget.Caption = IIF(.lcPType $ 'MT', LANG_POACFRV_PCSBDG, LANG_POACFRV_ORDPCS)
  
  *--Remove First Column if Display Screen By Shipment is Flase
  

  
   
*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
*!*	  IF !.llByShp
*!*	    .AriaForm1.grdCostInfo.RemoveObject("Column1")
*!*	  ENDIF
*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]

  *--Create Temp Cursors and Indecies Names
  .lcPOSHDR     = gfTempName()
  .lcTmpLine    = gfTempName()
  .lcTmpLine1   = gfTempName()
  .lcHistCurs   = gfTempName()
  .lcShpTotCurs = gfTempName()
  
  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
  .lcTmpSpLn    = gfTempName()
  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
  

  *--Get Costing Elements Types and labels
  LOCAL laSetups[21,2], lnCosts, lnI, lnX
  lnX = 1
  lnCosts = IIF(.lcPType # 'T', 7, 4)

  FOR lnI = 1 TO 7
    lcI = STR(lnI,1)
    laSetups[lnX,1]   = IIF(.lcPType = 'T' AND lnI > lnCosts, '', 'M_C' + .lcPType + 'TYPE' + lcI)
    laSetups[lnX+1,1] = IIF(.lcPType = 'T' AND lnI > lnCosts, '', 'M_C' + .lcPType + 'SLBL' + lcI)
    laSetups[lnX+2,1] = IIF(.lcPType = 'T' AND lnI > lnCosts, '', 'M_C' + .lcPType + 'COST' + lcI)
    lnX = lnX + 3
  ENDFOR

  =gfGetMemVar(@laSetups)
  
  lnX = 1
  FOR lnI = 1 TO 7
    lcI=STR(lnI,1)
    .laCost[lnI,1] = laSetups[lnX,2]
    .laCost[lnI,2] = laSetups[lnX+1,2]
    .laCost[lnI,3] = laSetups[lnX+2,2]
    lnX = lnX + 3
  ENDFOR

  *--Open needed files.
  LOCAL lSQLCommand
  
  IF .lcPType = 'T'
    .lcInvType = '0002'

    LOCAL lcWareHouse
    lcWareHouse = gfGetMemVar('M_WareHouse')

    .lcBrowFlds = "po       :H='"+LANG_POACFRV_MFGORDNO+"',"+;
                  "Style    :H='"+LANG_POACFRV_FABRIC  +"',"+;
                  IIF(lcWareHouse = 'Y', "cWareCode:H='"+LANG_POACFRV_WARECODE+"',", "")+;
                  "Status   :H='"+LANG_POACFRV_STATUS  +"',"+;
                  "Entered  :H='"+LANG_POACFRV_ENTERED +"',"+;
                  "Complete :H='"+LANG_POACFRV_COMPLETE+"'"
  ELSE
    .lcInvType = '0001'
    IF .lcPType = 'I'
      IF .llByShp
        .lcBrowFlds = "ShipNo   :H='"+LANG_POACFRV_SHPMNTNO +"',"+;
                      "Status   :H='"+LANG_POACFRV_STATUS   +"',"+;
                      "Entered  :H='"+LANG_POACFRV_ENTERED  +"',"+;
                      "Cartons  :H='"+LANG_POACFRV_CARTONS  +"',"+;
                      "AirWayB  :H='"+LANG_POACFRV_AIRWAYB +"',"+;
                      "ETA      :H='"+LANG_POACFRV_ETA      +"',"+;
                      "TotQtyHDR:H='"+LANG_POACFRV_INTRANSIT+"',"+;
                      "Recv_Stk :H='"+LANG_POACFRV_RECEIVED +"',"+;
                      "Recv_Dam :H='"+LANG_POACFRV_DAMAGE   +"',"+;
                      "Recv_Can :H='"+LANG_POACFRV_CANCELED +"',"+;
                      "cVessel  :H='"+LANG_POACFRV_CVESSEL +"',"+;
                      "Reference:H='"+LANG_POACFRV_REFERENCE  +"'"
      ELSE
        .lcBrowFlds = "PO        :H='"+LANG_POACFRV_CUTTKTNO+"',"+;
                      "Status    :H='"+LANG_POACFRV_STATUS  +"', "+;
                      "Vendor    :H='"+LANG_POACFRV_VENDOR  +"',"+;
                      "lcVnName = ApVendor.cVenComp :H='"+LANG_POACFRV_VENCOMP+"',"+;
                      "Entered   :H='"+LANG_POACFRV_ENTERED +"',"+;
                      "Complete  :H='"+LANG_POACFRV_COMPLETE+"',"+;
                      "nStyOrder :H='"+LANG_POACFRV_TOTQTY  +"',"+;
                      "POTotal   :H='"+LANG_POACFRV_AMOUNT  +"',"+;
                      "Receive   :H='"+LANG_POACFRV_REC     +"',"+;
                      "Open      :H='"+LANG_POACFRV_OPEN    +"'"

        lSQLCommand = "SELECT * FROM APVENDOR"
        lnRemResult = oAriaApplication.RemoteSystemData.Execute(lSQLCommand, '', 'APVENDOR', '', ;
                              oAriaApplication.cAriaNativeDataFilesConStr, 3, ;
                              '', .DataSessionId)
        IF lnRemResult < 0
          RETURN .F.
        ENDIF
        
        LOCAL lnOldBuffMode
        lnOldBuffMode = CURSORGETPROP("Buffering", "APVENDOR")
        
        =CURSORSETPROP("Buffering", 3, "APVENDOR")

        SELECT APVENDOR
        INDEX ON CVENDCODE TAG VENCODE
        
        =CURSORSETPROP("Buffering", lnOldBuffMode, "APVENDOR")
      ENDIF
    ELSE
      .lcBrowFlds = "po       :H='"+LANG_POACFRV_CUTTKTNO+"',"+;
                    "Style    :H='"+LANG_POACFRV_STYLE   +"',"+;
                    "Status   :H='"+LANG_POACFRV_STATUS  +"',"+;
                    "Entered  :H='"+LANG_POACFRV_ISSUE   +"',"+;
                    "Complete :H='"+LANG_POACFRV_COMPLETE+"',"+;
                    "Season   :H='"+LANG_POACFRV_SEASON  +"',"+;
                    "cDivision:H='"+LANG_POACFRV_DIVISION+"',"+;
                    "nStyOrder:H='"+LANG_POACFRV_BUDGET  +"':P='999999',"+;
                    "Receive  :H='"+LANG_POACFRV_RECEIVED+"':P='999999',"+;
                    "Damage   :H='"+LANG_POACFRV_DAMAGE  +"':P='999999',"+;
                    "Open     :H='"+LANG_POACFRV_OPEN    +"':P='999999' "
    ENDIF
  ENDIF

  *-- To pass the required information for the work order key #. 
  WITH .Ariaform1.kbPo
    .cbusinessdocumenttype = 'P'
    .cworkordertype        = IIF(oFormSet.lcPType = 'I', 'P', IIF(oFormSet.lcPType = 'M', 'U', 'F'))
    .obrowsecursor         = oFormSet.lcPosHdr
    .cbrowsetitle          = oFormSet.lcTitle
    .cbrowsefields         = oFormSet.lcBrowFlds
    .lcBrowRetFld          = IIF(oFormSet.lcPType = 'I' AND oFormSet.llByShp, 'ShipNo', 'PO')
    .Keytextbox.InputMask  = lfPicture(oFormSet.lcPType, oFormSet.llByShp)
    .Keytextbox.Format     = lfPicture(oFormSet.lcPType, oFormSet.llByShp)
  ENDWITH

  *--Assign SQL Browse Properties
  LOCAL llNoRecordFound, llServerError
  
  .cBrowseTableDBEngine   = 'SQL'
  .cBrowseAliasName       = .lcPOSHDR
  .cBrowseTableName       = IIF(.llByShp, 'SHPMTHDR', 'POSHDR')
  .cBrowseIndexName       = IIF(.llByShp, 'SHPMTHDR', 'POSHDR')
  .cBrowseIndexExpression = IIF(.llByShp, 'cBusDocu+cShpType+ShipNo', 'cBusDocu+cStyType+PO')
  .cBrowseIndexFields     = IIF(.llByShp, 'cBusDocu,cShpType,ShipNo', 'cBusDocu,cStyType,PO')
  .cBrowseKey             = IIF(.lcPType = 'I', 'PP', IIF(.lcPType = 'M', 'PU', 'PF'))
  .BrowseTitle            = IIF(.llByShp, LANG_POACFRV_BROETTLSHP,;
                                IIF(.lcPType = 'I', LANG_POACFRV_BROETTLSPO,;
                                    IIF(.lcPType = 'M', LANG_POACFRV_BROETTLSCC,;
                                        LANG_POACFRV_BROETTLMMF)))

  .oRemoteCursor.mGetCursor(.cBrowseAliasName,.cBrowseAliasName,.DataSessionID,;
                            'STRUCTURE',.F.,.cBrowseTableName,'*','',.cBrowseIndexName,;
                            .cBrowseIndexExpression,.F.,.cBrowseIndexFields,0,;
                            .cBrowseFilter,.cBrowseKey,@llNoRecordFound,@llServerError)

  *--Set value for FormSet Master File and Data Environment Initial Selected Cursor
  .DataEnvironment.InitialSelectedAlias = .lcPOSHDR

  *--Create Index for POSHDR Cursor
  LOCAL lnOldBuffMode, lcHdrIdx
  lnOldBuffMode = CURSORGETPROP("Buffering", .lcPOSHDR)
  lcHdrIdx      = IIF(.llByShp, 'ShipNo', 'PO')

  =CURSORSETPROP("Buffering", 3, .lcPOSHDR)

  SELECT (.lcPOSHDR)
  INDEX ON &lcHdrIdx TAG POSHDR

  =CURSORSETPROP("Buffering", lnOldBuffMode)

  IF .lcPType = 'I' AND !.llByShp
    SET RELATION TO Vendor INTO ApVendor
  ENDIF

  *--Create the Temporary BOMLine file
  =lfCrtTempFl(oFormSet)

  *--Create the Temporary BOMLine file
  =lfAdjustGrid(oFormSet)
  
  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
  IF !.llByShp
    .AriaForm1.grdCostInfo.RemoveObject("Column1")
    IF .lcPType = 'I' 
      .AriaForm1.cboSelect.Value = '2'
      lfVSelect(2,oFormSet)
    ENDIF  
  ELSE
    .AriaForm1.cboSelect.Value = '1'
     lfVSelect(1,oFormSet)
  ENDIF
  lfGtClrDat(oFormSet)
  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
    
ENDWITH

*!*************************************************************
*! Name      : lfFormAct
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/25/2004
*! Purpose   : Activate Method for Screen.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : oFormSet --> FormSet Object Reference
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfFormAct(This)
*!*************************************************************
FUNCTION lfFormAct
LPARAMETERS oFormSet

*--Activate Options pad.
=lfActPad(oFormSet)

*!*************************************************************
*! Name      : lfActPad
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/25/2004
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : oFormSet --> FormSet Object Reference
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfActPad(oFOrmSet)
*!*************************************************************
FUNCTION lfActPad
LPARAMETERS oFormSet

*--WSH Con
LOCAL lcHostFormName
lcHostFormName = '[' + oFormSet.cHostFormName + ']'

*DEFINE PAD _Option OF _MSYSMENU PROMPT LANG_POACFRV_OPTIONS KEY ALT+P , ' '
*ON PAD _Option OF _MSYSMENU ACTIVATE POPUP _OPTIONPOP

DEFINE PAD _Option OF (oFormSet.cHostFormName) PROMPT LANG_POACFRV_OPTIONS KEY ALT+P , ' '
ON PAD _Option OF (oFormSet.cHostFormName) ACTIVATE POPUP _OPTIONPOP

DEFINE POPUP _OPTIONPOP MARGIN SHADOW

*DEFINE BAR 1 OF _OPTIONPOP PROMPT LANG_POACFRV_REFRESH SKIP FOR _Screen.ActiveForm.Parent.ActiveMode # 'E' OR _Screen.ActiveForm.Parent.llRefresh
*DEFINE BAR 2 OF _OPTIONPOP PROMPT LANG_POACFRV_FORECAST SKIP FOR _Screen.ActiveForm.Parent.ActiveMode == 'S'
DEFINE BAR 1 OF _OPTIONPOP PROMPT LANG_POACFRV_REFRESH SKIP FOR gfFormIsActive(&lcHostFormName) .AND. (_Screen.ActiveForm.Parent.ActiveMode # 'E' OR _Screen.ActiveForm.Parent.llRefresh)
DEFINE BAR 2 OF _OPTIONPOP PROMPT LANG_POACFRV_FORECAST SKIP FOR gfFormIsActive(&lcHostFormName) .AND. _Screen.ActiveForm.Parent.ActiveMode == 'S'
*--WSH Con

IF oFormSet.llByShp

  *--WSH Con
  *DEFINE BAR 3 OF _OPTIONPOP PROMPT LANG_POACFRV_SHPTOTCST SKIP FOR _Screen.ActiveForm.Parent.ActiveMode == 'S'
  DEFINE BAR 3 OF _OPTIONPOP PROMPT LANG_POACFRV_SHPTOTCST SKIP FOR gfFormIsActive(&lcHostFormName) .AND. _Screen.ActiveForm.Parent.ActiveMode == 'S'
  *--WSH Con

  ON SELECTION BAR 3 OF _OPTIONPOP llDumy = lpDispScr(_Screen.ActiveForm.Parent)
ENDIF

ON SELECTION BAR 1 OF _OPTIONPOP llDumy = lfvRefrsh(_Screen.ActiveForm.Parent)
ON SELECTION BAR 2 OF _OPTIONPOP llDumy = lfvCstFore(_Screen.ActiveForm.Parent)

RETURN

*!*************************************************************
*! Name      : lfFormDeact
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/25/2004
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : oFormSet --> FormSet Object Reference
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfFormDeact(This)
*!*************************************************************
FUNCTION lfFormDeact
LPARAMETERS oFormSet

RELEASE PAD _Option OF _MSYSMENU

*!*************************************************************
*! Name      : lpShow
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/25/2004
*! Purpose   : Show Screen Mode Changes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfUnCompSession(),
*!                          gfCPSave(),()
*!*************************************************************
*! Passed Parameters  : oFormSet --> FormSet Object Reference.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lpShow(This)
*!*************************************************************
FUNCTION lpShow
LPARAMETERS oFormSet

DO CASE
  CASE oFormSet.ActiveMode = 'S'  && Select mode.

    oFormSet.AriaForm1.kbPO.KeyTextBox.Value = SPACE(6)
    oFormSet.AriaForm1.txtBudget.Value   = 0
    oFormSet.AriaForm1.txtComplete.Value = {}
    oFormSet.AriaForm1.txtEntered.Value  = {}

		

    STORE .F. TO oFormSet.llRefresh, oFormSet.llGetInfo

    oFormSet.AriaForm1.kbPo.Enabled = .T.
    
    
    *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
    IF oFormSet.lcPType = 'I'
      oFormSet.AriaForm1.cboSelect.Visible= .T.
      oFormSet.AriaForm1.cboSelect.Enabled= .T.
      oFormSet.AriaForm1.txtAmnt.Enabled = .F.
      oFormSet.AriaForm1.txtPercent.Enabled = .F.
      oFormSet.AriaForm1.cmdDistribute.Enabled = .F.
      oFormSet.AriaForm1.chkFreight.Enabled = .F.
      oFormSet.AriaForm1.chkFreight.Value = .F.
      oFormset.AriaForm1.txtPercent.Value =0
      oFormset.AriaForm1.txtAmnt.Value=0
      oFormSet.AriaForm1.sbQuantites.Enabled = .F.


      oFormSet.llByShp = (gfGetMemVar('M_cCostImp') = 'S')
      IF !oFormSet.llByShp 
        oFormSet.AriaForm1.cboSelect.Value = '2'
        lfVSelect(2,oFormSet)
        oFormSet.AriaForm1.cboCostItems.eNABLED = .F.             
      ELSE
        oFormSet.AriaForm1.cboSelect.Value = '1'
      	lfVSelect(1,oFormSet)
      	oFormSet.AriaForm1.cboTemplates.Enabled = .T.
        oFormSet.AriaForm1.cboTemplates.requery 
      	oFormSet.AriaForm1.cboTemplates.REFRESH	 
        lfvTemplate(oFormSet.AriaForm1.cboTemplates.Value,oFormSet)
      ENDIF  
      IF !EMPTY(oFormSet.lcTmpSpLn) AND USED(oFormSet.lcTmpSpLn)
        Sele(oFormSet.lcTmpSpLn)
        DELETE ALL 
      ENDIF 
    ELSE
      oFormSet.AriaForm1.cboSelect.Visible= .F.
    ENDIF 
	  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
    
    *B128912,1 KHM 07/10/2005 Commented out because it gives error in some cases [Begin]
    *IF !oFormSet.llFromKBPO
    *  oFormSet.AriaForm1.kbPo.SetFocus()
    *ENDIF
    *B128912,1 KHM 07/10/2005 [End]
    
    *--Refresh the temporary file and the browse in case of selecting a new transaction.
    IF USED (oFormSet.lcTmpLine)
      SELECT (oFormSet.lcTmpLine)
      BLANK  ALL
      DELETE ALL
    ENDIF

  CASE oFormSet.ActiveMode = 'V'  && View mode. 
    IF oFormSet.llGetInfo
      =lfCreate(oFormSet, .F.)
    ENDIF
    oFormSet.llGetInfo = .T.
    oFormSet.AriaForm1.kbPo.Enabled = .F.
    
    *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
    IF oFormSet.lcPType = 'I'
      oFormSet.AriaForm1.cboSelect.Enabled= .F.
      oFormSet.AriaForm1.cboCostItems.eNABLED = .T.
      oFormSet.AriaForm1.sbQuantites.Enabled = .F.             
    ENDIF  
    *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
    
  CASE oFormSet.ActiveMode = 'E'  && Edit mode. 
    oFormSet.llRefresh = .F.
    
    *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
    oFormSet.AriaForm1.cboSelect.Enabled= .F.
    IF oFormSet.lcPType = 'I' AND oFormSet.llByShp
      oFormSet.AriaForm1.sbQuantites.Enabled = .F.
      oFormSet.AriaForm1.cboTemplates.eNABLED = .F.
                   
      IF ALEN(oFormSet.laCostItem,1)-1 = 1
        oFormSet.AriaForm1.txtAmnt.Enabled = .F.
        oFormSet.AriaForm1.txtPercent.Enabled = .F.
        oFormSet.AriaForm1.cmdDistribute.Enabled = .F.
        oFormSet.AriaForm1.chkFreight.Enabled = .F.
      ELSE
       IF ALEN(oFormSet.laCostItem,1)-1 > 1
         oFormSet.AriaForm1.txtAmnt.Enabled = .T.
         oFormSet.AriaForm1.txtPercent.Enabled = .T.
         oFormSet.AriaForm1.chkFreight.Enabled = .T.
       ENDIF 
        oFormSet.AriaForm1.cmdDistribute.Enabled = .F.
      ENDIF 
      IF USED('SHPMTH') AND gfSeek('PP'+PADR(oFormSet.AriaForm1.kbPO.KeyTextBox.Value,6),'SHPMTH') AND SHPMTH.Status = 'H'
  	    oFormSet.AriaForm1.cmdApprove.Enabled = .T.	
  	  ENDIF   
  	  lfGrdShipCntSrc(oFormSet)
      oFormSet.AriaForm1.txtAmnt.Enabled = .F.
      oFormSet.AriaForm1.txtPercent.Enabled = .F.
      oFormSet.AriaForm1.cmdDistribute.Enabled = .F.
      oFormSet.AriaForm1.chkFreight.Enabled = .F.
       lfvCostItm(oFormSet.AriaForm1.cboCostItems.ListIndex,oFormSet)
    ENDIF 
    *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
    

    oFormSet.AriaForm1.kbPo.Enabled = .F.

  CASE oFormSet.ActiveMode = 'A'  && Add  mode.     
    oFormSet.AriaForm1.kbPo.Enabled = .F.
    
    *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
    oFormSet.AriaForm1.cboSelect.Enabled= .F.
    IF oFormSet.lcPType = 'I' AND oFormSet.llByShp
      oFormSet.AriaForm1.sbQuantites.Enabled = .F.
      oFormSet.AriaForm1.cboSelect.Enabled= .F.
      oFormSet.AriaForm1.cboTemplates.eNABLED = .F.             
      IF ALEN(oFormSet.laCostItem,1)-1 = 1
        oFormSet.AriaForm1.txtAmnt.Enabled = .F.
        oFormSet.AriaForm1.txtPercent.Enabled = .F.
        oFormSet.AriaForm1.cmdDistribute.Enabled = .F.
        oFormSet.AriaForm1.chkFreight.Enabled = .F.
      ELSE
       IF ALEN(oFormSet.laCostItem,1)-1 > 1
         oFormSet.AriaForm1.txtAmnt.Enabled = .T.
         oFormSet.AriaForm1.txtPercent.Enabled = .T.
         oFormSet.AriaForm1.chkFreight.Enabled = .T.
       ENDIF 
        oFormSet.AriaForm1.cmdDistribute.Enabled = .F.
        oFormSet.AriaForm1.chkFreight.Enabled = .F.
      ENDIF 
      lfGrdShipCntSrc(oFormSet)
      oFormSet.AriaForm1.txtAmnt.Enabled = .F.
      oFormSet.AriaForm1.txtPercent.Enabled = .F.
      oFormSet.AriaForm1.cmdDistribute.Enabled = .F.
      oFormSet.AriaForm1.chkFreight.Enabled = .F.
    ENDIF 
    *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]

ENDCASE

*--Refresh the Refresh toolbar button.
LOCAL lnCount

*--WSH Con
*FOR lnCount = 1 TO oAriaApplication.oToolBar.ControlCount 
FOR lnCount = 1 TO oFormSet.oToolBar.ControlCount
*--WSH Con

  *--WSH Con
  *IF UPPER(oAriaApplication.oToolBar.Controls(lnCount).Class) = 'TOOLBARCUSTOMBUTTON' AND ;
     UPPER(oAriaApplication.oToolBar.Controls(lnCount).CustomName) = UPPER('cmdRefrs')

    *oAriaApplication.oToolBar.Controls(lnCount).Enabled = (oFormSet.ActiveMode = 'E')

  IF UPPER(oFormSet.oToolBar.Controls(lnCount).Class) = 'TOOLBARCUSTOMBUTTON' AND ;
     UPPER(oFormSet.oToolBar.Controls(lnCount).CustomName) = UPPER('cmdRefrs')

    oFormSet.oToolBar.Controls(lnCount).Enabled = (oFormSet.ActiveMode = 'E')
  *--WSH Con

    EXIT
  ENDIF
ENDFOR

*--Adjust Grid Columns Read Only Properties.
WITH oFormSet.AriaForm1.grdCostInfo

  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
  *!*    IF oFormSet.llByShp
  *!*      .Column1.ReadOnly = .T.
  *!*    ENDIF
  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
  .Column2.ReadOnly = .T.
  .Column3.ReadOnly = .T.
  .Column4.ReadOnly = .T.
  .Column5.ReadOnly = oFOrmSet.ActiveMode $ 'SV'
  .Column6.ReadOnly = oFOrmSet.ActiveMode $ 'SV'
  
  .ActivateCell(1,1)
ENDWITH

*B128912,1 KHM 07/10/2005 Case its not a valid code and select mode return 0 
*B128912,1                to the setfoucs into PO or shipment
*RETURN
RETURN IIF(oFormSet.ActiveMode = 'S' AND !oFormSet.llFromKBPO, 0, .T.)
*B128912,1 KHM 07/10/2005 [End]


*:*************************************************************
*! Name      : lfvTkt
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/26/2004
*! Purpose   : Validate P/o,C/t Shipment and Mfg Order.
*:*************************************************************
*! Calls     : 
*!             Procedures : POSBrow 
*!             Functions  : CutBrow,gfMFGOrdBr,gfBrows
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvTkt()
*!*************************************************************
FUNCTION lfvTkt
LPARAMETERS oFormSet



LOCAL lnAlias, lcHdrFile
lnAlias   = SELECT()
lcHdrFile = oFormSet.lcPOSHDR

SELECT(lcHdrFile)
LOCATE

oFormSet.lcTktNo = EVALUATE(oFormSet.lcPOSHDR + IIF(oFormSet.llByShp, '.ShipNo', '.PO'))
oFormSet.AriaForm1.kbPO.KeyTextBox.Value = oFormSet.lcTktNo

IF EMPTY(oFormSet.AriaForm1.kbPO.KeyTextBox.Value) OR EOF(oFormSet.lcPOSHDR)
  SELECT(lnAlias)
  RETURN .F.
ENDIF

oFormSet.llFromKBPO = .T.

IF oFormSet.lcPType = 'M'
  IF !(EVALUATE(oFormSet.lcPOSHDR + '.Status') $ 'OA')
    *--Only XXX can be accepted, Cannot proceed.
    =gfModalGen('TRM34086B00000','DIALOG','open or actualizied cut ticket')
    oFormSet.AriaForm1.kbPO.KeyTextBox.Value = SPACE(6)
  ENDIF
ELSE
  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
  *IF !(EVALUATE(oFormSet.lcPOSHDR + '.Status') $ 'OA')
  
  *B608510,2 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[Start]
  *IF IIF(oFormSet.lcPType = 'I' AND oFormSet.llByShp ,!(EVALUATE(oFormSet.lcPOSHDR + '.Status') $ 'OH'),!(EVALUATE(oFormSet.lcPOSHDR + '.Status') $ 'OA'))
  IF IIF(oFormSet.lcPType = 'I' AND oFormSet.llByShp ,!(EVALUATE(oFormSet.lcPOSHDR + '.Status') $ 'OHC'),!(EVALUATE(oFormSet.lcPOSHDR + '.Status') $ 'OA'))
  *B608510,2 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[End]
  
  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
    *--Only XXX can be accepted, Cannot proceed.
    
   *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
   * =gfModalGen('TRM34086B00000','DIALOG','the status open')
    =gfModalGen('TRM34086B00000','DIALOG',IIF(oFormSet.lcPType = 'I' AND oFormSet.llByShp,LANG_POACFRV_STATUSOH,LANG_POACFRV_STATUSOPEN))
   *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
    
    oFormSet.AriaForm1.kbPO.KeyTextBox.Value = SPACE(6)
  ENDIF
ENDIF

IF EMPTY(oFormSet.AriaForm1.kbPO.KeyTextBox.Value)
  SELECT(lnAlias)
  oFormSet.llFromKBPO = .F.
  RETURN .F.
ENDIF

oFormSet.lcStyFbGrd = ''

LOCAL lnPieses, lcPOChk

IF oFormSet.lcPType = 'I' AND oFormSet.llByShp
  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
  *IF Status = 'O'
  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
  
    LOCAL lcStatement, lcTempCurs, lcTempPOSHDR
    lcTempCurs   = gfTempName()
    lcTempPOSHDR = gfTempName()
    
    lcStatement = "SELECT po, shipno, cStyGrade FROM POSLN (INDEX = POSLNSH)" +;
                  "       WHERE ShipNo = '" + oFormSet.lcTktNo + "' AND" +;
                  "             cBusDocu = 'P' AND cStyType = 'P'"
    IF !lfSQLStatement(lcStatement, lcTempCurs, '', '')
      SELECT(lnAlias)
      RETURN .F.
    ENDIF
    
    SELECT (lcTempCurs)
    IF EOF()
      *--The shipment lines have not been found! unable to proceed.
      = gfModalGen('TRM34077B42000','DIALOG')
      SELECT (lnAlias)
      RETURN .F.
    ELSE
      lnPieses = 0
      lcPOChk = SPACE(6)
      SCAN
        IF PO <> lcPOChk 

          lcStatement = "SELECT nStyOrder FROM POSHDR (INDEX = POSHDR)" +;
                        "       WHERE cBusDocu = 'P' AND cStyType = 'P' AND" +;
                        "             PO = '" + EVALUATE(lcTempCurs + '.PO') + "'"
                        
          IF !lfSQLStatement(lcStatement, lcTempPOSHDR, '', '')
            SELECT(lnAlias)
            RETURN .F.
          ENDIF

          SELECT (lcTempCurs)
          lnPieses = lnPieses + IIF(!EOF(lcTempPOSHDR), EVALUATE(lcTempPOSHDR + '.nStyOrder'), 0)
          lcPOChk  = Po
        ENDIF
      ENDSCAN
    ENDIF
    
    USE IN (lcTempCurs)
    IF USED(lcTempPOSHDR)
      USE IN (lcTempPOSHDR)
    ENDIF
    
 *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
 * ENDIF
 *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
 
  SELECT (lcHdrFile)
  
  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
  *oFormSet.AriaForm1.txtBudget.Value = lnPieses
  oFormSet.AriaForm1.txtAirVessel.Value  = cvessel
  oFormSet.AriaForm1.txtShpEntered.Value = entered
  oFormSet.AriaForm1.txtContainer.Value  = airwayb
  oFormSet.AriaForm1.txtNotes.Value  = Note
  oFormSet.AriaForm1.txtReference.Value  = reference
  oFormSet.AriaForm1.txtCompShp.value = ETA
  oFormSet.AriaForm1.cboCostItems.eNABLED = .T.             
  lfCreatShpTemp(oFormSet)
  lfGrdShipCntSrc(oFormSet)
  oFormSet.AriaForm1.grdShpCost.afterrowcolchange()
  oFormSet.AriaForm1.grdShpCost.refresh
  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
  
ELSE
  oFormSet.AriaForm1.txtBudget.Value = &lcHdrFile..nStyOrder
ENDIF

oFormSet.AriaForm1.txtEntered.Value  = &lcHdrFile..Entered
oFormSet.AriaForm1.txtComplete.Value = IIF(oFormSet.llByShp, &lcHdrFile..ETA, &lcHdrFile..Complete)

LOCAL lcStatement, lcItemCurs, lnRemResult
lcItemCurs = gfTempName()

DO CASE
  CASE oFormSet.lcPType = 'T'
    lcStatement = "SELECT cStyGrade FROM ITEM (INDEX = STYLE)" +;
                  "       WHERE cInvType = '" + oFormSet.lcInvType + "' AND" +;
                  "             Style LIKE '" + RTRIM(EVALUATE(lcHdrFile + '.Style')) + "%'"

    IF lfSQLStatement(lcStatement, lcItemCurs, '', '')
      oFormSet.lcStyFbGrd = EVALUATE(lcItemCurs + '.cStyGrade')
      USE IN (lcItemCurs)
    ENDIF
  CASE oFormSet.lcPType = 'M'
    lcStatement = "SELECT cStyGrade FROM STYLE WHERE Style = '" + RTRIM(EVALUATE(lcHdrFile + '.Style')) + "'"
    lnRemResult = oAriaApplication.RemoteSystemData.Execute(lcStatement, '', lcItemCurs, '', ;
                          oAriaApplication.cAriaNativeDataFilesConStr, 3, ;
                          '', SET("Datasession"))

    IF !(lnRemResult < 0)
      oFormSet.lcStyFbGrd = EVALUATE(lcItemCurs + '.cStyGrade')
      USE IN (lcItemCurs)
    ENDIF
  CASE oFormSet.lcPType = 'I' AND !oFormSet.llByShp
    lcStatement = "SELECT cStyGrade FROM POSLN (INDEX = POSLN)" +;
                  "       WHERE cBusDocu = 'P' AND cStyType = 'P' AND" +;
                  "             PO = '" + oFormSet.lcTktNo + "'"

    IF lfSQLStatement(lcStatement, lcItemCurs, '', '')
      oFormSet.lcStyFbGrd = EVALUATE(lcItemCurs + '.cStyGrade')
      USE IN (lcItemCurs)
    ENDIF
ENDCASE

oFormSet.llGetInfo  = .F.
oFormSet.llGenerate = .F.
oFormSet.lcBldAct   = ' '

SELECT (oFormSet.lcPOSHDR)
LOCATE

IF !lfCreate(oFormSet, .F.)
  oFormSet.llFromKBPO = .F.

  SELECT(lnAlias)
  RETURN .F.
ELSE
  oFormSet.ChangeMode(IIF(oFormSet.llGenerate, 'A', 'V'))
ENDIF

oFormSet.llFromKBPO = .F.
RETURN .T.

*:*************************************************************
*! Name      : lfCreate
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/26/2004
*! Purpose   : To create the temproray bom line file.
*:*************************************************************
*! Calls     : 
*!             Procedures : lfActCst
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : oFormSet -> FormSet ObjectControl.
*!                      llToVew  -> Not ask to create yes/no.
*!*************************************************************
*! Returns            : llLoop -> Not Valid selections.
*!*************************************************************
*! Example   : =lfCreate()
*!*************************************************************
FUNCTION lfCreate
LPARAMETERS oFormSet, llToVew

*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
*LOCAL lcStatement, lcBOMLine, lcPOSLN, lnGenerate, llFstTme, lcPO, lcLnNo, lcStyClr
LOCAL lcStatement, lcBOMLine, lcPOSLN, lnGenerate, lcPO, lcLnNo, lcStyClr
*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]

lcBOMLine = gfTempName()
lcPOSLN   = gfTempName()
lcPO      = ''
lcLnNo    = ''
lcStyClr  = ''

oFormSet.AriaForm1.LockScreen = .T.
=lfRestoreGrid(oFormSet.AriaForm1.grdCostInfo)

SELECT (oFormSet.lcTmpLine)
BLANK ALL
DELETE ALL



IF oFormSet.llByShp
  lcStatement = "SELECT * FROM BOMLINE (INDEX = BOMLNSHP)" +;
                "       WHERE cImTyp = 'I' AND cType = '2' AND" +;
                "             ShipNo = '" + oFormSet.lcTktNo + "'"
  
  IF !lfSQLStatement(lcStatement, lcBOMLine, '', '')
    oFormSet.AriaForm1.LockScreen = .F.
    RETURN .F.
  ENDIF

  llFstTme = .T.  && flag for first time only.
  
  SELECT (lcBOMLine)
  IF !EOF()
    SCAN FOR EMPTY(cRSession)
      SCATTER MEMVAR
      m.cStatus = 'S'
      INSERT INTO (oFormSet.lcTmpLine) FROM MEMVAR
    ENDSCAN
  ENDIF

  SELECT (oFormSet.lcTmpLine)
  LOCATE
  
  IF EOF()
    lcStatement = "SELECT POSLN.*, POSHDR.Status AS cStatus" +;
                  "      FROM  POSLN (INDEX = POSLNSH) LEFT OUTER JOIN POSHDR (INDEX = POSHDR)" +;
                  "            ON POSLN.cBusDocu = POSHDR.cBusDocu AND" +;
                  "               POSLN.cStyType = POSHDR.cStyType AND" +;
                  "               POSLN.PO = POSHDR.PO" +;
                  "      WHERE POSLN.ShipNo   = '" + oFormSet.lcTktNo + "' AND" +;
                  "            POSLN.cBusDocu = '" + EVALUATE(oFormSet.lcPOSHDR + '.cBusDocu') + "' AND" +;
                  "            POSLN.cStyType = '" + EVALUATE(oFormSet.lcPOSHDR + '.cShpType') + "' AND" +;
                  "            POSLN.TranCd   = '3'"

    IF !lfSQLStatement(lcStatement, lcPOSLN, '', '')
      oFormSet.AriaForm1.LockScreen = .F.
      RETURN .F.
    ENDIF
    
    SELECT (lcPOSLN)
    LOCATE
    
    SCAN
      IF EVALUATE(lcPOSLN + '.cStatus') <> 'O'
        LOOP
      ENDIF
      
      IF !(PO == lcPO)
        lcStatement = "SELECT * FROM BOMLINE (INDEX = BOMLINE)" +;
                      "       WHERE cImTyp = 'I' AND" +;
                      "             cType IN ('1', '2') AND" +;
                      "             cTktNo = '" + PO + "'"
        
        IF !lfSQLStatement(lcStatement, lcBOMLine,;
                           'cIMTyp+cType+ShipNo+cTktNo+STR(LINENO,6)|',;
                           'BOMLNSHP|')
          oFormSet.AriaForm1.LockScreen = .F.
          RETURN .F.
        ENDIF
      ENDIF
          
      SELECT (lcPOSLN)
      lcPO     = PO
      lcLnNo   = STR(LineNo, 6)
      lcStyClr = Style
      
      SELECT (lcBOMLine)
      SET ORDER TO BOMLNSHP
      
      IF !SEEK('I2'+oFormSet.lcTktNo+lcPO+lcLnNo)
        IF llFstTme AND !llToVew
          llFstTme = .F.
          
          *--No previous cost records found! Generate new records?,\<Yes,\<No
          lnGenrate = gfModalGen('QRM34087B00006','DIALOG')
          IF lnGenrate = 1
            oFormSet.llGenerate = .T.    &&To create a new records.
          ELSE
            oFormSet.AriaForm1.LockScreen = .F.
            RETURN .F.
          ENDIF
        ENDIF
        IF SEEK('I2'+SPACE(6)+lcPO+lcLnNo)
          SCAN WHILE cIMTyp+cType+ShipNo+cTktNo+STR(LINENO,6)='I2'+SPACE(6)+lcPO+lcLnNo ;
                 FOR Style=lcStyClr
            SCATTER TO laFields
            SELECT (oFormSet.lcTmpLine)
            APPEND BLANK
            GATHER FROM laFields
            REPLACE ShipNo  WITH oFormSet.lcTktNo,;
                    cStatus WITH 'S'
          ENDSCAN
        ELSE
          IF SEEK('I1'+SPACE(6)+lcPO+lcLnNo)
            SCAN WHILE cIMTyp+cType+ShipNo+cTktNo+STR(LINENO,6)='I1'+SPACE(6)+lcPO+lcLnNo ;
                   FOR Style=lcStyClr
              SCATTER TO laFields
              SELECT (oFormSet.lcTmpLine)
              APPEND BLANK
              GATHER FROM laFields
              REPLACE ShipNo  WITH oFormSet.lcTktNo,;
                      cType   WITH '2',;
                      cStatus WITH 'A'
            ENDSCAN
          ELSE
            *--No cost sheet details found for XXX.
            =gfModalGen('TRM34088B00000','DIALOG','P/O :'+lcPo+' of this Shipment')
          ENDIF 
        ENDIF
      ELSE
        LOCATE REST WHILE cimtyp+ctype+shipno+ctktno+STR(lineno,6)=;
                          'I'+'2'+oFormSet.lcTktNo+lcPO+lcLnNo FOR EMPTY(cRSession)
        IF !FOUND()                  
          IF llFstTme AND !llToVew
            llFstTme=.F.
            *--No previous cost records found! Generate new records?,\<Yes,\<No
            lnGenrate = gfModalGen('QRM34087B00006','DIALOG')
            IF lnGenrate = 1
              oFormSet.llGenerate = .T.    &&To create a new records.
            ELSE
              oFormSet.AriaForm1.LockScreen = .F.
              RETURN .F.
            ENDIF
          ENDIF  
          
          IF oFormSet.llGenerate
            IF SEEK('I1'+SPACE(6)+lcPO+lcLnNo)
              SCAN WHILE cIMTyp+cType+ShipNo+cTktNo+STR(LINENO,6)='I1'+SPACE(6)+lcPO+lcLnNo ;
                   FOR Style=lcStyClr
                 SCATTER TO laFields
                 SELECT (oFormSet.lcTmpLine)
                 APPEND BLANK
                 GATHER FROM laFields
                 REPLACE ShipNo  WITH oFormSet.lcTktNo,;
                         cType   WITH '2',;
                         cStatus WITH 'A'
               ENDSCAN
            ENDIF
          ENDIF  
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  
  
  
  SELECT (oFormSet.lcTmpLine)
  GO TOP
  IF EOF()
    *--No cost sheet details found for XXX.
    =gfModalGen('TRM34088B00000','DIALOG','P/Os of this Shipment')
    oFormSet.AriaForm1.LockScreen = .F.
    RETURN .F.
  ENDIF

  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
  IF oFormSet.lcPType  = 'I' .AND. oFormSet.llByShp
     =lfGetShipData(oFormSet)
  ENDIF    
  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]



  USE IN (lcBOMLine)
  IF USED(lcPOSLN)
    USE IN (lcPOSLN)
  ENDIF

ELSE     && Not by shipment.

  lcStatement = "SELECT * FROM BOMLINE (INDEX = BOMLINE)" +;
                "       WHERE cImTyp = '" + oFOrmSet.lcPType + "' AND " +;
                "             cType IN ('1', '2') AND" +;
                "             cTktNo = '" + oFormSet.lcTktNo + "'"
  
  IF !lfSQLStatement(lcStatement, lcBOMLine, 'cType+cTktNo|', 'BOMLINE|')
    oFormSet.AriaForm1.LockScreen = .F.
    RETURN .F.
  ENDIF

  SELECT (lcBOMLine)
  SET ORDER TO BOMLINE
  
  IF !SEEK('1'+oFormSet.lcTktNo)
    *--No cost sheet details found for XXX.
    =gfModalGen('TRM34088B00000','DIALOG','this ' + oFormSet.lcTitle)
    oFormSet.AriaForm1.LockScreen = .F.
    RETURN .F.
  ELSE 
    IF !SEEK('2'+oFormSet.lcTktNo)
      IF !llToVew
        *--No previous cost records found! Generate new records?,\<Yes,\<No
        lnGenrate = gfModalGen('QRM34087B00006', 'DIALOG')
      ELSE
        lnGenrate = 1
      ENDIF
      IF lnGenrate = 1
        oFormSet.llGenerate = .T.    && To create a new records.
        SEEK '1'+oFormSet.lcTktNo
        lcCond1 = "cType+cTktNo = '1'+oFormSet.lcTktNo"
      ELSE
        oFormSet.AriaForm1.LockScreen = .F.
        RETURN .F.
      ENDIF
    ELSE
      LOCATE REST WHILE cType+cTktNo='2'+oFormSet.lcTktNo FOR EMPTY(cRSession)
      IF FOUND() 
        lcCond1 = "cType+cTktNo='2'+oFormSet.lcTktNo"
      ELSE
        IF !llToVew
          *--No previous cost records found! Generate new records?,\<Yes,\<No
          lnGenrate = gfModalGen('QRM34087B00006','DIALOG')
        ELSE
          lnGenrate = 1
        ENDIF
        IF lnGenrate = 1
          oFormSet.llGenerate = .T.    &&To create a new records.
          SEEK '1'+oFormSet.lcTktNo
          lcCond1="cType+cTktNo='1'+oFormSet.lcTktNo"
        ELSE
          oFormSet.AriaForm1.LockScreen = .F.
          RETURN .F.
        ENDIF
      ENDIF 
    ENDIF
    =SEEK(IIF(oFormSet.llGenerate,'1','2')+oFormSet.lcTktNo)

    SCAN REST WHILE cType+cTktNo=IIF(oFormSet.llGenerate,'1','2')+oFormSet.lcTktNo;
              FOR EMPTY(cRSession) AND cStyGrade = oFormSet.lcStyFbGrd
    
      SCATTER MEMVAR
      m.cStatus = IIF(oFormSet.llGenerate, 'A', 'S')
      INSERT INTO (oFormSet.lcTmpLine) FROM MEMVAR
    ENDSCAN
  ENDIF
  
  USE IN (lcBOMLine)
ENDIF

SELECT (oFormSet.lcTmpLine)
SET ORDER TO TAG (oFormSet.lcTmpLine)

IF IIF(oFormSet.lcPType $ 'MI', EVALUATE(oFormSet.lcPOSHDR + '.Status') = 'A', .F.)

  *--Cutting ticket has been actualized. 
  *--Do you wish to default the new cost items to the actual cost or the estimated cost?
  IF EMPTY(oFormSet.lcBldAct)
    lnChse = gfModalGen('QRM38127B38017','DIALOG')
    IF lnChse = 1
      SET ORDER TO TAG (oFormSet.lcTmpLine1)
      GO TOP
      DO WHILE !EOF()
        =lfActCst(oFormSet)
      ENDDO
    ENDIF
    oFormSet.lcBldAct = IIF(lnChse = 1, 'A', 'E')
  ELSE
    IF oFormSet.lcBldAct = 'A'
      SET ORDER TO TAG (oFormSet.lcTmpLine1)
      DO WHILE !EOF()
        =lfActCst(oFormSet)
      ENDDO
    ENDIF  
  ENDIF
ENDIF

SELECT (oFormSet.lcTmpLine)
SET ORDER TO TAG (oFormSet.lcTmpLine)
SET FILTER TO !lVoid
LOCATE

IF EOF()
  *--All items have allready been received for this lcTitle.
  = gfModalGen('TRM34089B00000', 'DIALOG', oFormSet.lcTitle)
  oFormSet.AriaForm1.LockScreen = .F.
  RETURN .F.
ENDIF

*--Create title record.
LOCAL lcPrvType, lcPrvTp, lcPrvSp, lcPrvTk, lcCstTttl, lcLftTttl, lcRgtTttl
lcPrvType = ' '

SCAN FOR !EMPTY(cType)
  lcPrvTp = cType
  lcPrvSp = ShipNo
  lcPrvTk = cTktNo
  IF lcPrvType <> cBomTyp
    lcPrvType = cBomTyp
    lnSavRc = RECNO()
    lcCstTttl = '< ' + SUBSTR(ALLTRIM(oFormSet.laCost[VAL(lcPrvType),3]), 1, 15) + ' >'
    lcLftTttl = REPLICATE('<', (19 - LEN(lcCstTttl)) / 2)
    lcRgtTttl = REPLICATE('>', (19 - LEN(lcCstTttl)) / 2)

    APPEND BLANK
    REPLACE cType   WITH lcPrvTp,;
            ShipNo  WITH lcPrvSp,;
            cTktNo  WITH lcPrvTk,;
            cBomTyp WITH lcPrvType,;
            Style   WITH '',;
            Item    WITH lcLftTttl+lcCstTttl+lcRgtTttl,;
            MfgCode WITH ''
    GOTO lnSavRc
  ENDIF
ENDSCAN
GO TOP

SELECT (oFormSet.lcPOSHDR)
LOCATE

*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
IF NOT (oFormSet.lcPType = 'I' AND oFormSet.llByShp)
*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]

  =lfAdjustGrid(oFormSet)
  
*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
ENDIF
*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
oFormSet.AriaForm1.LockScreen = .F.
RETURN .T.

*:*************************************************************
*! Name      : lfvRefrsh
*! Developer : Wael M. Abo-Shawareb
*! Date      : 09/19/2004
*! Purpose   : To Refresh the screen with any added items to
*!             the cost sheet.
*:*************************************************************
*! Calls     : 
*!             Procedures : lfActCst
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvRefrsh()
*!*************************************************************
FUNCTION lfvRefrsh
LPARAMETERS oFormSet

IF oFormSet.llRefresh
  RETURN
ENDIF  

LOCAL lcPOSLN, lcBOMLine, lcPO, lcLnNo, lcStyClr
lcPOSLN   = gfTempName()
lcBOMLine = gfTempName()
lcPO      = ''
lcLnNo    = ''
lcStyClr  = ''

IF oFormSet.llByShp
  lcStatement = "SELECT POSLN.*, POSHDR.Status AS cStatus" +;
                "      FROM  POSLN (INDEX = POSLNSH) LEFT OUTER JOIN POSHDR (INDEX = POSHDR)" +;
                "            ON POSLN.cBusDocu = POSHDR.cBusDocu AND" +;
                "               POSLN.cStyType = POSHDR.cStyType AND" +;
                "               POSLN.PO = POSHDR.PO" +;
                "      WHERE POSLN.ShipNo   = '" + oFormSet.lcTktNo + "' AND" +;
                "            POSLN.cBusDocu = '" + EVALUATE(oFormSet.lcPOSHDR + '.cBusDocu') + "' AND" +;
                "            POSLN.cStyType = '" + EVALUATE(oFormSet.lcPOSHDR + '.cShpType') + "' AND" +;
                "            POSLN.TranCd   = '3'"

  IF !lfSQLStatement(lcStatement, lcPOSLN, '', '')
    RETURN .F.
  ENDIF
  
  SELECT (lcPOSLN)
  LOCATE
  
  SCAN FOR EMPTY(cRSession)
    IF EVALUATE(lcPOSLN + '.cStatus') <> 'O'
      LOOP
    ENDIF

    IF !(PO == lcPO)
      lcStatement = "SELECT * FROM BOMLINE (INDEX = BOMLINE)" +;
                    "       WHERE cImTyp = 'I' AND" +;
                    "             cType = '1' AND" +;
                    "             cTktNo = '" + PO + "'"
      
      IF !lfSQLStatement(lcStatement, lcBOMLine,;
                         'cIMTyp+cType+ShipNo+cTktNo+STR(LINENO,6)|',;
                         'BOMLNSHP|')
        oFormSet.AriaForm1.LockScreen = .F.
        RETURN .F.
      ENDIF
    ENDIF
    
    SELECT (lcPOSLN)
    lcPO     = PO
    lcLnNo   = STR(LineNo, 6)
    lcStyClr = Style

    SELECT (lcBOMLine)
    SET ORDER TO BOMLNSHP
    LOCATE

    IF SEEK('I1'+SPACE(6)+lcPO+lcLnNo)
      SCAN WHILE cIMTyp+cType+ShipNo+cTktNo+STR(LineNo,6)='I1'+SPACE(6)+lcPO+lcLnNo ;
           FOR Style = lcStyClr
        *--If this line is not added to the adjustment , add it to the temp file.
        *--and remark it (lNew=True).
        IF !SEEK('2'+oFormSet.lcTktNo+CTKTNO+CBOMTYP+STR(LINENO,6)+STYLE+ITEM+MFGCODE, oFormSet.lcTmpLine, oFormSet.lcTmpLine)
          SCATTER MEMVAR
          m.ShipNo  = oFormSet.lcTktNo
          m.cType   = '2'
          m.lNew    = .T.
          m.cStatus = 'A'
          INSERT INTO (oFormSet.lcTmpLine) FROM MEMVAR
        ENDIF
      ENDSCAN
    ENDIF
  ENDSCAN
  IF USED(lcBOMLine)
    USE IN (lcBOMLine)
  ENDIF
  USE IN (lcPOSLN)
ELSE
  LOCAL llAct
  
  lcStatement = "SELECT * FROM BOMLINE (INDEX = BOMLINE)" +;
                "       WHERE cImTyp = '" + oFOrmSet.lcPType + "' AND " +;
                "             cType = '1' AND" +;
                "             cTktNo = '" + oFormSet.lcTktNo + "'"
  
  IF !lfSQLStatement(lcStatement, lcBOMLine, 'cType+cTktNo|', 'BOMLINE|')
    oFormSet.AriaForm1.LockScreen = .F.
    RETURN .F.
  ENDIF

  SELECT (lcBOMLine)
  SET ORDER TO BOMLINE
  
  llAct = .F.

  SCAN
    *--For each record cType=1 and locate for its couple of cType=2
    *--there is cType=1 without cType=2 (i.e. this line added 
    *--and wasn't exist the first time this adjustment done) 
    IF !SEEK('2'+CTKTNO+CBOMTYP+STR(LINENO,6)+STYLE+ITEM+MFGCODE, oFormSet.lcTmpLine, oFormSet.lcTmpLine)
      SCATTER MEMVAR
      m.cType   = '2'
      m.lnew    = .T.
      m.cStatus = 'A'
      INSERT INTO (oFormSet.lcTmpLine) FROM MEMVAR            
      llAct = .T.
    ENDIF
  ENDSCAN

  SELECT (oFormSet.lcTmpLine)
  *--Cutting ticket has been actualized. 
  *--Do you wish to default the new cost items to the actual cost or the estimated cost?
  IF llAct AND oFormSet.lcPType = 'M' AND EVALUATE(oFormSet.lcPOSHDR + '.Status') = 'A' AND ;
     gfModalGen('QRM38127B38017','DIALOG') = 1

    lnSetOrder = VAL(SYS(21))
    SET ORDER TO TAG (oFormSet.lcTmpLine1)
    DO WHILE !EOF()
      IF !lNew
        SKIP
        LOOP
      ENDIF  
      =lfActCst(oFormSet)
    ENDDO
    SET ORDER TO lnSetOrder          
  ENDIF
ENDIF

IF USED(lcPOSLN)
  USE IN (lcPOSLN)
ENDIF
IF USED(lcBOMLine)
  USE IN (lcBOMLine)
ENDIF

SELECT (oFormSet.lcTmpLine)
LOCATE

*--Create title record.
LOCAL lcPrvType, lcPrvTp, lcPrvSp, lcPrvTk, lcCstTttl, lcLftTttl, lcRgtTttl
lcPrvType = ' '

SCAN FOR !EMPTY(cType)
  lcPrvTp = cType
  lcPrvSp = ShipNo
  lcPrvTk = cTktNo
  lnSavRc = RECNO()
  IF lcPrvType <> cBomTyp
    lcPrvType = cBomTyp

    LOCATE FOR cBomTyp == lcPrvType AND EMPTY(cCatgtyp)
    IF !FOUND()
      lcCstTttl = '< ' + SUBSTR(ALLTRIM(oFormSet.laCost[VAL(lcPrvType),3]), 1, 15) + ' >'
      lcLftTttl = REPLICATE('<', (19 - LEN(lcCstTttl)) / 2)
      lcRgtTttl = REPLICATE('>', (19 - LEN(lcCstTttl)) / 2)

      APPEND BLANK
      REPLACE cType   WITH lcPrvTp,;
              ShipNo  WITH lcPrvSp,;
              cTktNo  WITH lcPrvTk,;
              cBomTyp WITH lcPrvType,;
              Style   WITH '',;
              Item    WITH lcLftTttl+lcCstTttl+lcRgtTttl,;
              MfgCode WITH ''
    ENDIF
  ENDIF
  GOTO lnSavRc
ENDSCAN
LOCATE

oFormSet.AriaForm1.grdCostInfo.Refresh()
oFormSet.llRefresh = .T.

*--Refresh the Refresh toolbar button.
LOCAL lnCount

*--WSH Con
*FOR lnCount = 1 TO oAriaApplication.oToolBar.ControlCount 
  *IF UPPER(oAriaApplication.oToolBar.Controls(lnCount).Class) = 'TOOLBARCUSTOMBUTTON' AND ;
     UPPER(oAriaApplication.oToolBar.Controls(lnCount).CustomName) = UPPER('cmdRefrs')

    *oAriaApplication.oToolBar.Controls(lnCount).Enabled = .F.

FOR lnCount = 1 TO oFormSet.oToolBar.ControlCount 
  IF UPPER(oFormSet.oToolBar.Controls(lnCount).Class) = 'TOOLBARCUSTOMBUTTON' AND ;
     UPPER(oFormSet.oToolBar.Controls(lnCount).CustomName) = UPPER('cmdRefrs')

    oFormSet.oToolBar.Controls(lnCount).Enabled = .F.
*--WSH Con

    EXIT
  ENDIF
ENDFOR

RETURN

*:*************************************************************
*! Name      : lfActCst
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/29/2004
*! Purpose   : Default the new cost items to the actual cost.
*:*************************************************************
PROCEDURE lfActCst
LPARAMETERS oFOrmSet

LOCAL lcKey, lcStyClr, lcTempCurs, lcStatement, lcTktNo
lcTktNo    = oFormSet.AriaForm1.kbPO.KeyTextBox.Value
lcTempCurs = gfTempName()
lcKey      = cBomTyp + MfgCode + Item

LOCAL lnUsed, lnRecQty, lnRecAmt, lnUnitCost, lnOpen, lnLstRecYld, lnLstRecCst
STORE 0 TO lnUsed, lnRecQty, lnRecAmt, lnUnitCost, lnOpen, lnLstRecYld, lnLstRecCst

SELECT (oFormSet.lcTmpLine)
=SEEK(lcKey)

*--Get open quantity for all styles that use this item
SCAN REST WHILE cBomTyp+MfgCode+Item = lcKey
  lcStyClr = Style
  
  lcStatement = "SELECT TranCD, TotQty FROM POSLN (INDEX = POSLN)" +;
                "       WHERE cBusDocu = '" + EVALUATE(oFormSet.lcPOSHDR + '.cBusDocu') + "' AND" +;
                "             cStyType = '" + EVALUATE(oFormSet.lcPOSHDR + IIF(oFormSet.llByShp, '.cShpType', '.cStyType')) + "' AND" +;
                "             PO = '" + lcTktNo + "' AND" +;
                "             cInvType = '" + oFOrmSet.lcInvType + "' AND" +;
                "             Style = '" + lcStyClr + "'"
  
  IF !lfSQLStatement(lcStatement, lcTempCurs, '', '')
    RETURN .F.
  ENDIF

  SELECT (lcTempCurs)
  IF !EOF()
    SUM ALL IIF(TranCd = '1', TotQty, -TotQty) TO lnClrOpen
    lnOpen = lnOpen + MAX(lnClrOpen, 0)
  ENDIF
  
  USE IN (lcTempCurs)
ENDSCAN

SELECT (oFormSet.lcTmpLine)
=SEEK(lcKey)

*--Get used quantity from this item component.
lcStatement = "SELECT Used_Qty FROM CTKTBOM (INDEX = CTKTBOM)" +;
              "       WHERE cImTyp = '" + cImTyp + "' AND" +;
              "             CutTkt = '" + cTktNo + "' AND" +;
              "             Typ = '" + cBomTyp + "' AND" +;
              "             cInvType = '" + oFOrmSet.lcInvType + "' AND" +;
              "             Item = '" + Item + "' AND" +;
              "             MfgCode = '" + MfgCode + "'"

IF !lfSQLStatement(lcStatement, lcTempCurs, '', '')
  RETURN .F.
ENDIF

IF !EOF(lcTempCurs)
  lnUsed = EVALUATE(lcTempCurs + '.Used_Qty')
ENDIF

SELECT (oFormSet.lcTmpLine)

*--Get received quantity, received amnout, last received yield
*--and last received cost for this item component.
lcStatement = "SELECT * FROM BOMLINE (INDEX = BOMLINE)" +;
              "       WHERE cImTyp = '" + cIMTyp + "' AND" +;
              "             cType = '2' AND" +;
              "             cTktNo = '" + cTktNo + "' AND" +;
              "             [LineNo] = " + STR(LineNo,6) + " AND" +;
              "             cBOMTyp = '" + cBomTyp + "'"

IF !lfSQLStatement(lcStatement, lcTempCurs, '', '')
  RETURN .F.
ENDIF

SELECT (lcTempCurs)
IF !EOF()
  SCAN FOR cBomTyp+MfgCode+Item = lcKey AND !EMPTY(cRSession)
    lnRecQty = lnRecQty + ItemQty
    lnRecAmt = lnRecAmt + ItemAmt
    lnLstRecYld = UnitQty
    lnLstRecCst = UnitCost
  ENDSCAN
ENDIF

SELECT (oFormSet.lcTmpLine)
IF lnOpen = 0
  lnUnitQty  = lnLstRecYld
  lnUnitCost = lnLstRecCst
ELSE
  lnUnitQty = MAX(ROUND((lnUsed-lnRecQty)/lnOpen,3),0)

  *--Get actual unit cost for this item component.
  lcStatement = "SELECT nTotACst FROM BOMCOST (INDEX = BOMCSTKT)" +;
                "       WHERE cBOMType = '" + cBOMTyp + "' AND" +;
                "             cImTyp = '" + cIMTyp + "' AND "+;
                "             cTktNo = '" + cTktNo + "' AND" +;
                "             Item = '" + Item + "' AND" +;
                "             mfgCode = '" + mfgCode + "'"

  IF !lfSQLStatement(lcStatement, lcTempCurs, '', '')
    RETURN .F.
  ENDIF

  SELECT (lcTempCurs)
  SUM nTotACst ALL TO lnActAmt

  IF lnUnitQty <> 0
    lnUnitCost = MAX(ROUND(((lnActAmt - lnRecAmt) / lnOpen) / lnUnitQty, 7), 0)
  ELSE
    lnUnitCost = MAX(ROUND(((lnActAmt - lnRecAmt) / lnOpen), 7), 0)
  ENDIF
ENDIF

SELECT (oFormSet.lcTmpLine)
REPLACE REST  UnitQty  WITH lnUnitQty ,;
              UnitCost WITH lnUnitCost ,;
              ItemQty  WITH StyQty*UnitQty,;
              ItemAmt  WITH ItemQty*UnitCost WHILE cBomTyp+MfgCode+Item = lcKey

USE IN (lcTempCurs)
SELECT (oFormSet.lcTmpLine)
RETURN


*:*************************************************************
*! Name      : lfCPDelete
*! Developer : Wael M. Abo-Shawareb
*! Date      : 09/15/2004
*! Purpose   : To clear all adjusted items and return to default.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
FUNCTION lfCPDelete
LPARAMETERS oFormSet

LOCAL llSuccess
llSuccess = .F.

*--The current adjustment will be ignored. Accept,Cancel
IF gfModalGen('TRM34090B34007','DIALOG') = 1
  SELECT (oFormSet.lcBOMLine)
  DELETE ALL

  SELECT (oFormSet.lcTmpLine)
  SCAN FOR cStatus = 'S'
    SCATTER MEMVAR MEMO
    SELECT (oFormSet.lcBOMLine)
    APPEND BLANK
    GATHER MEMO MEMVAR
  ENDSCAN
  SELECT (oFormSet.lcBOMLine)
  =TABLEUPDATE(.T.)

  IF oFormSet.llByShp
  
    DELETE FOR CIMTYP+CTYPE+SHIPNO='I2'+oFormSet.lcTktNo AND EMPTY(cRSession)
    
    *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
    IF gfSEEK(oFormSet.lcTktNo,'SHPRLFLD')
      SELECT SHPRLFLD
      SCAN REST WHILE SHIPNO+PO+STR(LINENO,6) = oFormSet.lcTktNo
        gfDelete()
      ENDSCAN 
      gfTableUpdate()
    ENDIF
    *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
    
    
    IF !USED('SHPMFGDT')
      =gfOpenTable('SHPMFGDT','SHPMFGDT','SH','SHPMFGDT')
    ENDIF 
    IF gfSeek(oFormSet.lcTktNo,'SHPMFGDT')
      SELECT SHPMFGDT
      SCAN REST WHILE shipno+mfgcode = oFormSet.lcTktNo
        gfDelete()
      ENDSCAN 
      gfTableUpdate()
    ENDIF 
  ELSE
    IF SEEK(oFormSet.lcPType+'2'+oFormSet.lcTktNo)
      DELETE REST WHILE CIMTYP+CTYPE+CTKTNO=oFormSet.lcPType+'2'+oFormSet.lcTktNo FOR EMPTY(cRSession)
    ENDIF
  ENDIF
  
  *B607585,1 AMH Add nLineNo field to BomLine table [Start]
  *llSuccess = lfSQLUpdate(oFormSet.lcBOMLine + "|",;
                          "cimtyp,ctype,ctktno,shipno,lineno,cbomtyp,cinvtype,style,cinvtypc,item,mfgcode,crsession,cStyGrade" + "|",;
                          "BOMLINE|",;
                          "BOMLINEU|")
  llSuccess = lfSQLUpdate(oFormSet.lcBOMLine + "|",;
                          "cimtyp,ctype,ctktno,shipno,lineno,cbomtyp,cinvtype,style,cinvtypc,item,mfgcode,crsession,cStyGrade,nLineNo" + "|",;
                          "BOMLINE|",;
                          "BOMLINEU|")
  *B607585,1 AMH [End]
	
ENDIF

RETURN llSuccess
*:*************************************************************
*! Name      : lfSavScr
*! Developer : Wael M. Abo-Shawareb
*! Date      : 09/15/2004
*! Purpose   : Save / Update.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
PROCEDURE lfSavScr
LPARAMETERS oFormSet



*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
IF oFormSet.lcPType = 'I' AND oFormSet.llByShp
  PRIVATE lcExSign,lcUntSin
  STORE '/' TO lcExSign,lcUntSin
  lcTmpLine = oFormSet.lcTmpLine
  lcTmpSpLn = oFormSet.lcTmpSpLn
  lcSHIPNO = PADR(oFormSet.AriaForm1.kbPO.KeyTextBox.Value ,6)
  SELECT &lcTmpLine
  SCAN FOR LINENO>0
    IF SEEK(' '+cTktNo+STYLE+STR(LINENO,6),lcTmpSpLn)
      DO CASE
        CASE CCATGTYP = 'P' 
          REPLACE UNITCOST WITH &lcTmpSpLn..NFCOST1 
          
          *B608510,3 MMT 05/29/2008 Convert Shipment Cost sheet program to ARIA4[Start]
          REPLACE NEXRATE  WITH &lcTmpSpLn..NPRICERAT
          *B608510,3 MMT 05/29/2008 Convert Shipment Cost sheet program to ARIA4[End]
          
          *,;
				  cStatus  WITH  'S'
				  
        CASE CCATGTYP $ 'DM'
          lnPos = ASCAN(oFormSet.laCostItem,'*'+&lcTmpLine..MFGCODE+'*')
          IF lnPos > 0
            lnPos = ASUBSCRIPT(oFormSet.laCostItem,lnPos,1)
            lcI = ALLTRIM(STR(lnPos-1))
            
      
            *B608667,1 MMT 08/27/2008 Fix bug of wrong unt cost in case of foreign currency [Start]
            IF &lcTmpLine..Ccurrcode <> oAriaApplication.BaseCurrency 
               lcExSign = gfGetExSin(@lcUntSin,&lcTmpLine..Ccurrcode)
               IF lcExSign = '/'
                 lcExSign = '*'
               ELSE
                 IF lcExSign = '*'
                   lcExSign = '/'
                 ENDIF 
               ENDIF 
                 
               REPLACE UNITCOST WITH IIF(&lcTmpSpLn..TOTQTY>0, (&lcTmpSpLn..COST&lcI &lcExSign &lcTmpLine..nExRate  &lcUntSin &lcTmpLine..nCurrUnit)/(&lcTmpSpLn..TOTQTY) , UNITCOST)
            ELSE
            *B608667,1 MMT 08/27/2008 Fix bug of wrong unt cost in case of foreign currency [End]
            
              REPLACE UNITCOST WITH IIF(&lcTmpSpLn..TOTQTY>0, &lcTmpSpLn..COST&lcI/&lcTmpSpLn..TOTQTY , UNITCOST)
            
            *B608667,1 MMT 08/27/2008 Fix bug of wrong unt cost in case of foreign currency [Start]
            ENDIF 
            *B608667,1 MMT 08/27/2008 Fix bug of wrong unt cost in case of foreign currency [End]
            
            *C127341,12 TMI [Start] if duty currency is forign , then change the unit cost to forigen currency
            =gfSEEK('PP'+&lcTmpSpLn..PO,'POSHD')
            IF  POSHD.CDUTYCUR <> oAriaApplication.BaseCurrency                     
              lnDutyUnit  = IIF(POSHD.nDCurUnit=0,1,POSHD.nDCurUnit)
              lnDutyRate  = IIF(POSHD.nDutyRat=0,1,POSHD.nDutyRat)
              lcExSign = gfGetExSin(@lcUntSin,POSHD.CDUTYCUR)
              lcUntSin = IIF(lcUntSin='/','*','/')
              lcExSign = IIF(lcExSign='/','*','/')
              REPLACE UNITCOST WITH UNITCOST &lcExSign lnDutyRate &lcUntSin lnDutyUnit  
            ENDIF                        
*            REPLACE  cStatus  WITH  'S'
          ENDIF
          
      ENDCASE
    ENDIF 
  ENDSCAN
  
  =gfSeek('PP'+lcSHIPNO,'SHPMTH') 
  SELECT SHPMTH
  gfReplace("CTEMPLATE WITH '"+ oFormSet.AriaForm1.cboTemplates.Value +"'")
  IF SHPMTH.STATUS = 'H'
    IF oFormSet.llApprove
      SELECT SHPMTH
      gfReplace("STATUS    WITH 'O'")
      oFormSet.llApprove = .F.
    ENDIF
  ENDIF
  SELECT SHPMTH
  =gfAdd_Info('SHPMTH') 
  gfReplace()
  
  *B608510,2 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[Start]
  lnSHPTable = gfGetRemoteTable(SET("Datasession"),'SHPRLFLD')
  lcCurSorSHP = IIF(lnSHPTable <>0,oAriaApplication.laRemoteTable[lnSHPTable].lcCursorUpdate,'SHPRLFLD')
  *B608510,2 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[End]
  
  SELECT &lcTmpSpLn
  LOCATE   
  SCAN FOR KEY = ' '
    *B608510,2 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[Start]
    *IF !gfSEEK(SHIPNO+PO+STR(LINENO,6),'SHPRLFLD')
    IF (!gfSEEK(SHIPNO+PO+STR(LINENO,6),'SHPRLFLD') AND !SEEK(SHIPNO+PO+STR(LINENO,6),lcCurSorSHP ))
    *B608510,2 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[End]
      m.SHIPNO     = &lcTmpSpLn..SHIPNO 
      m.PO         = &lcTmpSpLn..PO 
      m.LINENO     = &lcTmpSpLn..LINENO
      m.NPRICERAT  = &lcTmpSpLn..NPRICERAT  
      m.NTRDISC    = &lcTmpSpLn..NTRDISC    
      m.STLMNTDISC = &lcTmpSpLn..STLMNTDISC 
      m.PRICEA     = &lcTmpSpLn..PRICEA     
      m.NROYALTY   = &lcTmpSpLn..NROYALTY
      m.NDUTYRAT   = &lcTmpSpLn..NDUTYRAT
      SELECT SHPRLFLD
      gfAppend('IN SHPRLFLD',.T.)
      =gfAdd_Info('SHPRLFLD') 
       gfReplace()             
    ELSE
      SELECT SHPRLFLD
      gfReplace("NPRICERAT  WITH &lcTmpSpLn..NPRICERAT ,"+;
                "NTRDISC    WITH  &lcTmpSpLn..NTRDISC ,"+;
                "STLMNTDISC WITH   &lcTmpSpLn..STLMNTDISC, "+;
                "PRICEA     WITH   &lcTmpSpLn..PRICEA,     "+;
                "NROYALTY   WITH   &lcTmpSpLn..NROYALTY,"+;
                "NDUTYRAT   WITH  &lcTmpSpLn..NDUTYRAT")
      SELECT SHPRLFLD          
      =gfAdd_Info('SHPRLFLD')  
       gfReplace()                       
    ENDIF 
  ENDSCAN 
  
  IF !USED('SHPMFGDT')
    =gfOpenTable('SHPMFGDT','SHPMFGDT','SH','SHPMFGDT')
  ENDIF 
  
  SELECT &lcTmpSpLn
  LOCATE  
  
  FOR lnInc = 2 TO ALEN(oFormSet.laCostItem,1)
    IF !gfSeek(&lcTmpSpLn..SHIPNO +oFormSet.laCostItem[lnInc,1] ,'SHPMFGDT')
      SELECT SHPMFGDT
      m.SHIPNO     = &lcTmpSpLn..SHIPNO 
      m.MFGCODE    = oFormSet.laCostItem[lnInc,1]
      m.LLDUTY     = oFormSet.laCostItem[lnInc,6]
      SELECT SHPMFGDT
      gfAppend('IN SHPMFGDT',.T.)
      =gfAdd_Info('SHPMFGDT')     
      gfReplace()           
    ELSE
      SELECT SHPMFGDT
      IF LLDUTY <> oFormSet.laCostItem[lnInc,6]
        llDutyValue = oFormSet.laCostItem[lnInc,6]
        gfReplace ("LLDUTY with llDutyValue")  
        =gfAdd_Info('SHPMFGDT') 
        gfReplace()              
      ENDIF   
    ENDIF   
  ENDFOR 
  
  SELECT SHPMFGDT
  gfTableUpdate()
  SELECT SHPRLFLD
  gfTableUpdate()
  SELECT SHPMTH
  gfTableUpdate()
ENDIF 
*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]

WAIT WINDOW 'Start Updating...' NOWAIT

SELECT (oFormSet.lcTmpLine)
GO TOP

SELECT (oFormSet.lcBOMLine)
DELETE ALL


SELECT (oFormSet.lcTmpLine)
SCAN FOR cStatus = 'S'
  SCATTER MEMVAR MEMO
  SELECT (oFormSet.lcBOMLine)
  APPEND BLANK
  GATHER MEMO MEMVAR
ENDSCAN
SELECT (oFormSet.lcBOMLine)




*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
*IF !oFormSet.llGenerate  
*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]

  =TABLEUPDATE(.T.)
  
*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]  
*ENDIF   
*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]

LOCAL lcStatement, lcTmpItem, lnRemResult, lcStyGrad
lcTmpItem = gfTempName()
lcStyGrad = ''

SELECT (oFormSet.lcTmpLine)
SCAN FOR !lVoid AND !EMPTY(Style) AND nSteps <> 1
  IF oFormSet.lcPType $ 'FT'
    lcStatement = "SELECT cStyGrade FROM ITEM (INDEX = STYLE) WHERE cInvType = '0002' AND Style = '" + Style + "'"

    IF lfSQLStatement(lcStatement, lcTmpItem, '', '')
      lcStyGrad = EVALUATE(lcTmpItem + '.cStyGrade')
      USE IN (lcTmpItem)
    ENDIF
  ELSE
    lcStatement = "SELECT cStyGrade FROM STYLE WHERE Style = '" + RTRIM(Style) + "'"
    lnRemResult = oAriaApplication.RemoteSystemData.Execute(lcStatement, '', lcTmpItem, '', ;
                          oAriaApplication.cAriaNativeDataFilesConStr, 3, ;
                          '', oFormSet.DataSessionId)

    IF !(lnRemResult < 0)
      lcStyGrad = EVALUATE(lcTmpItem + '.cStyGrade')
      USE IN (lcTmpItem)
    ENDIF
  ENDIF

  SELECT (oFormSet.lcTmpLine)
  SCATTER TO laFields

  IF EVALUATE(oFormSet.lcTmpLine + '.cStatus') = 'A' OR ;
              !SEEK(cimtyp+ctype+ctktno+shipno+STR(lineno,6)+cbomtyp+cinvtype+style+cinvtypc+item+mfgcode+crsession, oFormSet.lcBOMLine)
    SELECT (oFormSet.lcBOMLine)
    APPEND BLANK
  ENDIF
  SELECT (oFormSet.lcBOMLine)
  GATHER FROM laFields 
  REPLACE cIMTyp    WITH oFormSet.lcPType,;
          cType     WITH '2',;
          StyQty    WITH 0,;
          ItemQty   WITH 0,;
          ItemAmt   WITH 0,;
          cStyGrade WITH lcStyGrad

  SELECT (oFormSet.lcTmpLine)
  REPLACE nSteps  WITH 1
ENDSCAN

LOCAL llSuccess

*B607585,1 AMH Add nLineNo field to BomLine table [Start]
*llSuccess = lfSQLUpdate(oFormSet.lcBOMLine + "|",;
                        "cimtyp,ctype,ctktno,shipno,lineno,cbomtyp,cinvtype,style,cinvtypc,item,mfgcode,crsession,cStyGrade" + "|",;
                        "BOMLINE|",;
                        "BOMLINEU|")
llSuccess = lfSQLUpdate(oFormSet.lcBOMLine + "|",;
                        "cimtyp,ctype,ctktno,shipno,lineno,cbomtyp,cinvtype,style,cinvtypc,item,mfgcode,crsession,cStyGrade,nLineNo" + "|",;
                        "BOMLINE|",;
                        "BOMLINEU|")
*B607585,1 AMH [End]

SELECT (oFormSet.lcBOMLine)
llSuccess = llSuccess AND TABLEUPDATE(.T.)
IF llSuccess
  SELECT (oFormSet.lcTmpLine)
  REPLACE ALL cStatus WITH 'S'
ENDIF

WAIT CLEAR
RETURN llSuccess

*:*************************************************************
*! Name      : lfvCstFore
*! Developer : Wael M. Abo-Shawareb
*! Date      : 09/20/2004
*! Purpose   : See Cost Forcasting.
*!             Show estimated cost, landed cost for all previous
*!              receivings and next receiving cost for this tktk.
*:*************************************************************
*! Calls     : 
*!             Procedures : POACFO.SPX
*!             Functions  : lfCrtCstHist(),lfFOTrap()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvCstFore()
*!*************************************************************
FUNCTION lfvCstFore
LPARAMETERS oFormSet

LOCAL lnStyles, lnSavRc
lnStyles = 0

DIMENSION oFormSet.laStyles(1,2)


SELECT (oFormSet.lcTmpLine)
lnSavRc = RECNO()
GO TOP

SCAN FOR !EMPTY(Style)
  IF ASCAN(oFormSet.laStyles, Style) = 0
    lnStyles = lnStyles + 1

    DIMENSION oFormSet.laStyles(lnStyles,2)
    oFormSet.laStyles[lnStyles,1] = Style
    oFormSet.laStyles[lnStyles,2] = lnStyles
  ENDIF
ENDSCAN
=ASORT(oFormSet.laStyles)

DO FORM (oAriaApplication.ScreenHome + "POACFO") WITH oFormSet, oFormSet.lcHistCurs, oFormSet.ActiveMode

SELECT (oFormSet.lcTmpLine)
GOTO lnSavRc
RETURN

*!*************************************************************
*! Name      : lfCrtCstHist
*! Developer : Wael M. Abo-Shawareb
*! Date      : 09/20/2004
*! Purpose   : Build the costing temp file for specific style.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfCrtCstHist()
*!*************************************************************
FUNCTION lfCrtCstHist
LPARAMETERS oFormSet, lcCStyle, lcCursor

LOCAL lcBaseStruc, lcAddStruc, lcBomType, lcCutTag, lcPosTag, lcBomTag
LOCAL lnAlias, lcSession, lcMfgTag, lcBOMLine, lcTempCurs, lcTmpLine
lcTmpLine  = oFormSet.lcTmpLine
lnAlias    = SELECT()
lcTempCurs = gfTempName()
lcBOMLine  = gfTempName()

WAIT 'Collecting costing information. Please standby....' WINDOW NOWAIT

lcBaseStruc = "cTktNo C(6),cMarker C(1),cSort C(1),cBomTyp C(1),Item C(19),LineNo N(6),cCatgTyp C(1)," +;
              "Mfg C(6),Uom C(3),Desc C(20),EstItmQty N(7,3),EstUntCst N(7,3),EstItmAmt N(9,3)," +;
              "CurItmQty N(7,3),CurUntCst N(7,3),CurItmAmt N(9,3)"

lcAddStruc = ''

DIMENSION oFormSet.laSessions(1,2)
oFormSet.laSessions = ''
oFormSet.lnSessions = 0

lcdate = ''

lcStatement = "SELECT * FROM BOMLINE (INDEX = " + IIF(oFormSet.llByShp, "BOMLNSHP", "BOMLINE") + ")" +;
              "       WHERE cImTyp = '" + oFOrmSet.lcPType + "' AND" +;
              "             cType IN ('1', '2') AND" +;
                            IIF(oFormSet.llByShp,;
                                " ShipNo = '" + oFormSet.lcTktNo + "'",;
                                " cTktNo = '" + oFormSet.lcTktNo + "'")

IF !lfSQLStatement(lcStatement, lcBOMLine,;
                   'cIMTyp+cType+ShipNo+cTktNo+STR(LINENO,6)+cBomTyp+Style+Item+MfgCode|',;
                   'BOMLNSHP|')
  oFormSet.AriaForm1.LockScreen = .F.
  RETURN .F.
ENDIF

SELECT (lcBOMLine)
SET ORDER TO BOMLNSHP DESCENDING

IF SEEK(oFormSet.lcPType + '2')
  SCAN REST WHILE CIMTYP+CTYPE = oFormSet.lcPType + '2' FOR Style = lcCStyle AND !EMPTY(cRSession)
    IF ASCAN(oFormSet.laSessions, cRSession, 1) = 0
      lcData = ' '

      lcStatement = "SELECT [Date] FROM POSLN (INDEX = POSHREC)" +;
                    "       WHERE " + IIF(oFormSet.llByShp, "ShipNo = '" + oFormSet.lcTktNo + "' AND", "") +;
                    "             cRSession = '" + cRSession + "' AND" +;
                    "             cBusDocu = '" + EVALUATE(oFormSet.lcPOSHDR + '.cBusDocu') + "' AND" +;
                    "             cStyType = '" + IIF(oFormSet.llByShp, 'P', EVALUATE(oFormSet.lcPOSHDR + '.cStyType')) + "' AND" +;
                    "             PO = '" + cTktNo + "' AND" +;
                    "             cInvType = '" + IIF(oFormSet.lcPType $ 'MI', '0001', '0002') + "' AND" +;
                    "             Style = '" + lcCStyle + "' AND" +;
                    "             [LineNo] = " + STR(LineNo) + " AND" +;
                    "             TranCD = '2'"
                    
      IF !lfSQLStatement(lcStatement, lcTempCurs, '', '')
        SELECT(lnAlias)
        RETURN .F.
      ENDIF

      lcdate = '('+PADL(MONTH(EVALUATE(lcTempCurs + '.Date')),2,'0')+'-'+PADL(DAY(EVALUATE(lcTempCurs + '.Date')),2,'0')+')'
      
      SELECT (lcBOMLine)
      IF !EMPTY(lcdate) 
        oFormSet.lnSessions = oFormSet.lnSessions + 1

        DIMENSION oFormSet.laSessions(oFormSet.lnSessions,2)
        oFormSet.laSessions[oFormSet.lnSessions,1] = cRSession
        oFormSet.laSessions[oFormSet.lnSessions,2] = lcdate

        lcAddStruc = lcAddStruc + ',ItemQtyR'+STR(oFormSet.lnSessions,1)+' N(7,3), UntCstR'+STR(oFormSet.lnSessions,1)+' N(7,3),ItmAmtR'+STR(oFormSet.lnSessions,1)+' N(9,3)'
      ENDIF
      IF oFormSet.lnSessions = 8
        EXIT
      ENDIF

    ENDIF
  ENDSCAN
  =ASORT(oFormSet.laSessions, 1)
ENDIF

IF USED(lcCursor)
  USE IN (lcCursor)
ENDIF

CREATE CURSOR (lcCursor) (&lcBaseStruc &lcAddStruc)
*B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*INDEX ON cTktNo+cBomTyp+cSort+Item+Mfg TAG (lcCursor) OF (lcCursor)
INDEX ON cTktNo+cBomTyp+cSort+Item+Mfg TAG (lcCursor) 
*B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
SELECT (oFormSet.lcTmpLine)
SET FILTER TO Style = lcCStyle
GO TOP

LOCAL lcTicket, lcBomType, lcTmpTkt
lcTmpTkt = gfTempName()

DO WHILE !EOF()
  
  lcTicket = cTktNo
  INSERT INTO (lcCursor) (cTktNo,cSort,cBomTyp,Desc) VALUE (lcTicket,'G','G','Grand total :')

  lcStatement = "SELECT * FROM BOMLINE (INDEX = BOMLINE)" +;
                "       WHERE cImTyp = '" + oFOrmSet.lcPType + "' AND" +;
                "             cType IN ('1', '2') AND" +;
                "             cTktNo = '" + lcTicket + "'"

  IF !lfSQLStatement(lcStatement, lcBOMLine,;
                     'cType+STR(LINENO,6)+cBomTyp+Style+Item+MfgCode|',;
                     'BOMLNSHP|')
    oFormSet.AriaForm1.LockScreen = .F.
    RETURN .F.
  ENDIF

  SELECT (oFormSet.lcTmpLine)
  DO WHILE cType + cTktNo = IIF(oFormSet.llByShp OR !oFormSet.llGenerate,'2','1') + lcTicket

    lcBomType = cBomTyp
    INSERT INTO (lcCursor) (cTktNo,Item,Mfg,cSort,cBomTyp,Desc) VALUE ;
                           (lcTicket,'*******************','******','0',lcBomType,PROPER(oFormSet.laCost[VAL(lcBomType),2]))
    INSERT INTO (lcCursor) (cTktNo,cSort,cBomTyp,Desc) VALUE (lcTicket,'2',lcBomType,'Sub Total :')

    SCAN REST WHILE cType + cTktNo + cBomTyp = ;
                    IIF(oFOrmSet.llByShp OR !oFormSet.llGenerate,'2','1')+lcTicket+lcBomType ;
              FOR Style = lcCStyle

      =SEEK('1'+STR(LineNo,6)+lcBomType+Style+Item+MfgCode, lcBOMLine, 'BOMLNSHP')
      
      lcStatement = "SELECT [Desc], cUOMCode FROM CTKTBOM (INDEX = CTKTBOM)" +;
                    "       WHERE cImTyp = '" + oFOrmSet.lcPType + "' AND" +;
                    "             CutTkt = '" + lcTicket + "' AND" +;
                    "             typ = '" + lcBomType + "' AND" +;
                    "             cInvType = '" + cInvTypC + "' AND" +;
                    "             Item = '" + Item + "' AND" +;
                    "             mfgCode = '" + MfgCode + "'"

      IF !lfSQLStatement(lcStatement, lcTmpTkt, '', '')
        oFormSet.AriaForm1.LockScreen = .F.
        RETURN .F.
      ENDIF
      
      LOCAL lcUOMUse
      lcUOMUse = ''
      =gfGetUOMData(EVALUATE(lcTmpTkt + '.cUOMCode'), '', @lcUOMUse, 1)

      INSERT INTO (lcCursor)(cTktNo,cSort,cBomTyp,Item,Mfg,Desc,EstItmQty,EstUntCst,EstItmAmt,Uom,LineNo,cCatgTyp) VALUE ;
                            (lcTicket,'1',lcBomType,&lcBOMLine..Item,&lcBOMLine..MfgCode,EVALUATE(lcTmpTkt + '.Desc'),;
                             &lcBOMLine..UnitQty,&lcBOMLine..UnitCost,&lcBOMLine..UnitQty*&lcBOMLine..UnitCost,lcUOMUse,&lcBOMLine..LineNo,&lcBOMLine..cCatgTyp)

      REPLACE &lcCursor..CurItmQty WITH &lcTmpLine..UnitQty  ,;
              &lcCursor..CurUntCst WITH &lcTmpLine..UnitCost  ,;
              &lcCursor..CurItmAmt WITH &lcTmpLine..UnitQty*&lcTmpLine..UnitCost

      IF SEEK(lcTicket+lcBomType+'2',lcCursor)
         REPLACE &lcCursor..CurItmAmt WITH &lcCursor..CurItmAmt + &lcTmpLine..UnitQty*&lcTmpLine..UnitCost ,;
                 &lcCursor..EstItmAmt WITH &lcCursor..EstItmAmt + &lcBOMLine..UnitQty*&lcBOMLine..UnitCost
      ENDIF
      IF SEEK(lcTicket+'GG',lcCursor)
         REPLACE &lcCursor..CurItmAmt WITH &lcCursor..CurItmAmt + &lcTmpLine..UnitQty*&lcTmpLine..UnitCost ,;
                 &lcCursor..EstItmAmt WITH &lcCursor..EstItmAmt + &lcBOMLine..UnitQty*&lcBOMLine..UnitCost
      ENDIF

      lcRcvdKey = '2'+STR(&lcTmpLine..LineNo,6)+lcBomType+lcCStyle+;
                  &lcTmpLine..Item+&lcTmpLine..MfgCode
      
      *--Index Expresion : cimtyp+ctype+shipno+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode
      IF SEEK(lcRcvdKey, lcBOMLine)
        SELECT (lcBOMLine)
        SCAN REST WHILE ctype+shipno+ctktno+STR(lineno,6)+cbomtyp+style+item+mfgcode =;
                        '2'+IIF(oFormSet.llByShp,oFormSet.lcTktNo+lcTicket,SPACE(6)+lcTicket)+;
                        STR(&lcTmpLine..LineNo,6)+lcBomType+lcCStyle+&lcTmpLine..Item+&lcTmpLine..Mfgcode ;
                  FOR !EMPTY(cRSession)
          lnElemNo = ASCAN(oFormSet.laSessions, cRSession, 1)          
          IF lnElemNo > 0 AND BETWEEN(STR(ASUBSCRIPT(oFormSet.laSessions, lnElemNo, 1), 1), '1', '8')
            lcSession = STR(ASUBSCRIPT(oFormSet.laSessions, lnElemNo, 1), 1)

            IF SEEK(lcTicket+lcBomType+'1'+Item+MfgCode, lcCursor)
              REPLACE &lcCursor..ItemQtyR&lcSession WITH &lcBOMLine..UnitQty  ,;
                      &lcCursor..UntCstR&lcSession  WITH &lcBOMLine..UnitCost ,;
                      &lcCursor..ItmAmtR&lcSession  WITH &lcBOMLine..UnitQty * &lcBOMLine..UnitCost
            ENDIF
            IF SEEK(lcTicket+lcBomType+'2', lcCursor)
              REPLACE &lcCursor..ItmAmtR&lcSession WITH &lcCursor..ItmAmtR&lcSession + &lcBOMLine..UnitQty * &lcBOMLine..UnitCost
            ENDIF
            IF SEEK(lcTicket+'GG', lcCursor)
              REPLACE &lcCursor..ItmAmtR&lcSession WITH &lcCursor..ItmAmtR&lcSession + &lcBOMLine..UnitQty * &lcBOMLine..UnitCost
            ENDIF
          ENDIF  
        ENDSCAN
      ENDIF
    ENDSCAN
    INSERT INTO (lcCursor) (cTktNo,cSort,cBomTyp) VALUE (lcTicket,'3',lcBomType)
  ENDDO
ENDDO

SELECT (lcTmpLine)
SET FILTER TO
GO TOP IN (lcCursor)

SELECT (lnAlias)
WAIT CLEAR 
RETURN

*!*************************************************************
*! Name      : lfvStyles
*! Developer : Wael M. Abo-Shawareb
*! Date      : 09/25/2004
*! Purpose   : Select one of the styles and rebuild the costing file
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyles()
*!*************************************************************
FUNCTION lfvStyles
LPARAMETERS oFormSet

LOCAL lcOldSty, lcCStyle, lcStyDesc
lcStyDesc = ''

lcOldSty = oFormSet.AriaForm1.cboStyle.OldValue
lcCStyle = oFormSet.AriaForm1.cboStyle.Value

IF TYPE("lcOldSty") # 'C' OR (lcCStyle # lcOldSty)
  LOCAL lcStatement, lcItemCurs
  lcItemCurs = gfTempName()

  IF oFormSet.loParent.lcPType = 'T'
    lcStatement = "SELECT Desc1 FROM ITEM (INDEX = STYLE)" +;
                  "       WHERE cInvType = '" + oFormSet.loParent.lcInvType + "' AND" +;
                  "             Style = '" + lcCStyle + "'"

    IF lfSQLStatement(lcStatement, lcItemCurs, '', '')
      lcStyDesc = EVALUATE(lcItemCurs + '.DESC1')
    ENDIF
  ELSE
    lcStatement = "SELECT Desc1 FROM STYLE WHERE Style = '" + RTRIM(lcCStyle) + "'"
    lnRemResult = oAriaApplication.RemoteSystemData.Execute(lcStatement, '', lcItemCurs, '', ;
                          oAriaApplication.cAriaNativeDataFilesConStr, 3, ;
                          '', oFormSet.DataSessionId)
    
    IF !(lnRemResult < 0)
      lcStyDesc = EVALUATE(lcItemCurs + '.Desc1')
    ENDIF
  ENDIF

  IF USED(lcItemCurs)
    USE IN (lcItemCurs)
  ENDIF

  oFormSet.AriaForm1.txtStyDesc.Value = lcStyDesc

  oFormSet.AriaForm1.LockScreen = .T.
  =lfRestoreGrid(oFormSet.AriaForm1.grdCostForeInfo)
  =lfCrtCstHist(oFormSet.loParent, lcCStyle, oFormSet.lcCursor)
  =lfRefreshForCast(oFormSet)
  oFormSet.AriaForm1.LockScreen = .F.
ENDIF

RETURN

*!*************************************************************
*! Name      : lfvItemQty        
*! Developer : Wael M. Abo-Shawareb
*! Date      : 09/25/2004
*! Purpose   : Valid function for cost forecasting unit quantity
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvItemQty()
*!*************************************************************
FUNCTION lfvItemQty
LPARAMETERS oFormSet

LOCAL lnAlias, loParent
loParent = oFormSet.loParent
lnAlias  = SELECT()

SELECT (oFormSet.lcCursor)

IF CurItmQty = oFormSet.lnOldQty
  RETURN
ENDIF

IF CurItmQty < 0
  REPLACE CurUntCst WITH oFormSet.lnOldQty
  =gfModalGen('TRM42000B42001','DIALOG')
  RETURN
ENDIF

LOCAL lcCurrRec, lcTicket, lnAdjAmt, lcTmpLine, lcCStyle
lcCurrRec = cTktNo+cBomTyp+cSort+Item+Mfg
lcTicket  = cTktNo
lnAdjAmt  = (CurItmQty - oFormSet.lnOldQty) * CurUntCst
lcTmpLine = oFormSet.loParent.lcTmpLine
lcCStyle  = oFormSet.AriaForm1.cboStyle.Value

REPLACE CurItmAmt WITH CurItmAmt + lnAdjAmt

IF SEEK(lcTicket+cBomTyp+'2')
  REPLACE CurItmAmt WITH CurItmAmt + lnAdjAmt
ENDIF

IF SEEK(lcTicket+'GG')
  REPLACE CurItmAmt WITH CurItmAmt + lnAdjAmt
ENDIF

=SEEK(lcCurrRec)

IF SEEK(IIF(loParent.llByShp OR !loParent.llGenerate,'2','1')+IIF(loParent.llByShp,loParent.lcTktNo,'')+lcTicket+cBomTyp+STR(LineNo,6)+lcCStyle+Item+Mfg, lcTmpLine, lcTmpLine)
  REPLACE &lcTmpLine..UnitQty  WITH CurItmQty ,;
          &lcTmpLine..ItemQty  WITH &lcTmpLine..StyQty*CurItmQty ,;
          &lcTmpLine..ItemAmt  WITH &lcTmpLine..StyQty*CurItmQty*&lcTmpLine..UnitCost
ENDIF

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfvItemCst       
*! Developer : Wael M. Abo-Shawareb
*! Date      : 09/25/2004
*! Purpose   : Valid function for cost forecasting unit cost
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvItemCst()
*!*************************************************************
FUNCTION lfvItemCst
LPARAMETERS oFormSet

LOCAL lnAlias, loParent
loParent = oFormSet.loParent
lnAlias  = SELECT()

SELECT (oFormSet.lcCursor)

IF CurUntCst = oFormSet.lnOldCst
  RETURN
ENDIF

IF CurUntCst < 0
  REPLACE CurUntCst WITH oFormSet.lnOldCst
  =gfModalGen('TRM42000B42001','DIALOG')
  RETURN 0
ENDIF

LOCAL lcCurrRec, lcTicket, lnAdjAmt, lcTmpLine, lcCStyle
lcCurrRec = cTktNo+cBomTyp+cSort+Item+Mfg
lcTicket  = cTktNo
lnAdjAmt  = (CurUntCst - oFormSet.lnOldCst) * CurItmQty
lcTmpLine = loParent.lcTmpLine
lcCStyle  = oFormSet.AriaForm1.cboStyle.Value

REPLACE CurItmAmt WITH CurItmAmt + lnAdjAmt

IF SEEK(lcTicket+cBomTyp+'2')
  REPLACE CurItmAmt WITH CurItmAmt + lnAdjAmt
ENDIF

IF SEEK(lcTicket+'GG')
  REPLACE CurItmAmt WITH CurItmAmt + lnAdjAmt
ENDIF

=SEEK(lcCurrRec)

IF SEEK(IIF(loParent.llByShp OR !loParent.llGenerate,'2','1')+IIF(loParent.llByShp,loParent.lcTktNo,'')+lcTicket+cBomTyp+STR(LineNo,6)+lcCStyle+Item+Mfg, lcTmpLine, lcTmpLine)
  REPLACE &lcTmpLine..UnitCost WITH CurUntCst ,;
          &lcTmpLine..ItemQty  WITH &lcTmpLine..StyQty*&lcTmpLine..UnitQty ,;
          &lcTmpLine..ItemAmt  WITH &lcTmpLine..StyQty*&lcTmpLine..UnitQty*CurUntCst
ENDIF

SELECT (lnAlias)
RETURN

*!**************************************************************************
*! Name      : lfPicture  
*! Developer : Wael M. ABo-Shawareb
*! Date      : 08/26/2004
*! Purpose   : Change Picture of PO .
*!**************************************************************************
FUNCTION lfPicture
LPARAMETERS lcTranType, llByShp

IF llByShp OR (lcTranType = 'I' AND (gfGetMemVar('M_GenStOrN') = 'Y'))
  RETURN "!!!!!!" 
ELSE
  RETURN "!99999"
ENDIF  

*!*************************************************************
*! Name      : lfCrtTempFl
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/29/2004             
*! Purpose   : To create BOMLine temprory file
*!             that is needed by this program.
*!*************************************************************
*! Call      : gfCrtTmp()
*!*************************************************************
FUNCTION lfCrtTempFl
LPARAMETERS oFormSet

*--Temp file.
LOCAL lcTempCurs, lcStatement
lcTempCurs  = gfTempName()
lcStatement = "SELECT TOP 1 * FROM BOMLINE (INDEX = BOMLINE)"

IF !lfSQLStatement(lcStatement,;
                   lcTempCurs,;
                   'cimtyp+ctype+ctktno+shipno+STR(lineno,6)+cbomtyp+cinvtype+style+cinvtypc+item+mfgcode+crsession|',;
                   'BOMLINE|',;
                   'SAVE',;
                   5,;
                   'BOMLINE')
  RETURN .F.
ENDIF

oFormSet.lcBOMLine = lcTempCurs

SELECT (lcTempCurs)
=AFIELDS(laFileStru)

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+3,18]

laFileStru[lnFileStru+1,1] = 'lNew'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'nSteps'
laFileStru[lnFileStru+2,2] = 'N'
laFileStru[lnFileStru+2,3] = 2
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'cStatus'
laFileStru[lnFileStru+3,2] = 'C'
laFileStru[lnFileStru+3,3] = 1
laFileStru[lnFileStru+3,4] = 0

STORE .F. TO lafilestru[lnFileStru+1,5], lafilestru[lnFileStru+1,6],;
             lafilestru[lnFileStru+2,5], lafilestru[lnFileStru+2,6],;
             lafilestru[lnFileStru+3,5], lafilestru[lnFileStru+3,6]
STORE 0 TO laFilestru[lnFileStru+1,17], laFilestru[lnFileStru+1,18],;
           laFilestru[lnFileStru+2,17], laFilestru[lnFileStru+2,18],;
           laFilestru[lnFileStru+3,17], laFilestru[lnFileStru+3,18]
 
FOR lnI = 7 TO 16
  STORE "" TO lafilestru[lnFileStru+1,lnI]
  STORE "" TO lafilestru[lnFileStru+2,lnI]
  STORE "" TO lafilestru[lnFileStru+3,lnI]
ENDFOR

*--Index tags array.
DIMENSION laTags[2,2]
IF oFormSet.llByShp
  laTags[1,1] = 'cType+ShipNo+cTktNo+cBomTyp+STR(LineNo,6)+Style+Item+MFGCode'
ELSE
  laTags[1,1] = 'cType+cTktNo+cBomTyp+STR(LineNo,6)+Style+Item+MFGCode'
ENDIF
laTags[1,2] = oFormSet.lcTmpLine

laTags[2,1] = 'cBomTyp+MfgCode+Item+Style'
laTags[2,2] = oFormSet.lcTmpLine1
=gfCrtTmp(oFormSet.lcTmpLine,@laFileStru,@laTags)

SELECT (oFormSet.lcTmpLine)
SET ORDER TO

RETURN

*!*************************************************************
*! Name      : lfSQLStatement
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/26/2004
*! Purpose   : Runs a SQL Server Query Statement
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfSQLStatement()
*!*************************************************************
FUNCTION lfSQLStatement
LPARAMETERS lcSQLStatment, lcCursor, lcIndex, lcTages, lcScheme, lnBuffMode, lcTable

LOCAL lnConnectionHandlar, lnOldBuffMode, lcIndex1, lcTages1, lnTempIndex, lcIndExp

*--Scheme to Send for SQLRun Method 'SAVE' or 'BROWSE'
lcScheme = IIF(TYPE("lcScheme") = 'C', lcScheme, 'SAVE')

*--Table Name
lcTable = IIF(TYPE("lcTable") # 'C', '', lcTable)

*--Loop Until Successfully Connect or Cancel by the user
DO WHILE .T.
  *--Run the Satatement
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.SQLRun(lcSqlStatment, lcCursor, lcTable,;
                                  oAriaApplication.ActiveCompanyConStr,;
                                  3, lcScheme, SET("Datasession"))

  IF lnConnectionHandlar = 1
    *--If Query Successfully executed, Create Indexes if needed for the result cursor
    lnOldBuffMode = CURSORGETPROP("Buffering", lcCursor)
    =CURSORSETPROP("Buffering", 3, lcCursor)

    lcTages1 = lcTages
    lcIndex1 = lcIndex
    lnTempIndex = 1
    SELECT (lcCursor)
    DO WHILE AT("|", lcIndex1,1) <> 0
      lcIndExp = SUBSTR(lcIndex1, 1, AT("|", lcIndex1, 1) - 1)
      lcIndex1 = STRTRAN(lcIndex1, lcIndExp + "|", "", 1, 1)
      lcTages  = SUBSTR(lcTages1, 1, AT("|", lcTages1, 1) - 1)
      lcTages1 = STRTRAN(lcTages1, lcTages + "|", "", 1, 1)
      *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
      *INDEX ON &lcIndExp. TAG (lcTages) OF (lcCursor)
      INDEX ON &lcIndExp. TAG (lcTages)
      *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
    ENDDO
    =CURSORSETPROP("Buffering", IIF(TYPE("lnBuffMode") = 'N', lnBuffMode, lnOldBuffMode), lcCursor)
    RETURN .T.
  ELSE
    *--Query Execution Error
    =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnConnectionHandlar, .F.)

    IF MESSAGEBOX(LANG_CONNERRMSG, 5+16, LANG_CONNERRTTL) = 2
      RETURN .F.
    ENDIF
  ENDIF
ENDDO

*!*************************************************************
*! Name      : lfSQLUpdate
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 09/26/2004
*! Purpose   : Update a Set of SQL Tables
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfSQLUpdate()
*!*************************************************************
FUNCTION lfSQLUpdate
LPARAMETERS lcAllCursors, lcAllPrimaryKeyLists, lcAllSQLTables, lcAllSQLIndecies

LOCAL lnConnHandler, lcTranCode, lcCursor, lcPrimaryKeyList, lcSQLTable, lcSQLIndex

*-- Begin Updating Transaction
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr, 3, '')

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
    lnConnHandler = oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
    IF lnConnHandler <> 1
      =oAriaApplication.RemoteCompanyData.CheckRetResult("RollBackTran", lnConnHandler, .T.)
    ENDIF
    RETURN .F.
  ENDIF
ENDDO

*-- Commit Changes and Check Result
lnConnHandler = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnHandler <> 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran", lnConnHandler, .T.)
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  Return .F.
ENDIF

RETURN .T.

*!*************************************************************
*! Name      : lfAdjustGrid
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/29/2004             
*! Purpose   : Assign Control Sources and Properties for the Grid
*!*************************************************************
*! Call      : lfAdjustGrid()
*!*************************************************************
FUNCTION lfAdjustGrid
LPARAMETERS oFOrmSet

WITH oFormSet.AriaForm1.grdCostInfo
  .RecordSource = oFOrmSet.lcTmpLine
  

  IF oFormSet.llByShp
    .Column1.ControlSource = oFOrmSet.lcTmpLine + '.cTktNo'
    .Column1.DynamicInputMask = "IIF(EMPTY("+oFormSet.lcTmpLine+".cCatgTyp), '', 'XXXXXX')"
  ENDIF

  .Column2.ControlSource    = oFOrmSet.lcTmpLine + '.Style'
  .Column2.Header1.Caption  = gfItemMask('HM', '', oFormSet.lcInvType)
  .Column3.ControlSource    = oFOrmSet.lcTmpLine + '.Item'
  .Column4.ControlSource    = "IIF(" + oFOrmSet.lcTmpLine + ".cCatgTyp = 'M', gfCodDes(" + oFOrmSet.lcTmpLine + ".MfgCode, 'MfgCode'), " + oFOrmSet.lcTmpLine + ".MfgCode)"
  .Column5.ControlSource    = oFOrmSet.lcTmpLine + '.UnitQty'
  .Column5.DynamicInputMask = "IIF(EMPTY("+oFormSet.lcTmpLine+".cCatgTyp), '', '9999.999')"
  .Column6.ControlSource    = oFOrmSet.lcTmpLine + '.UnitCost'
  .Column6.DynamicInputMask = "IIF(EMPTY("+oFormSet.lcTmpLine+".cCatgTyp), '', '99999999.999')"

  *--Set dyenamic colors.
  .SetAll("Dynamicbackcolor", "", "Column")
  .SetAll("Dynamicbackcolor","IIF(EMPTY("+oFormSet.lcTmpLine+".cCatgTyp),16769996,16777215)", "Column")
  .Column3.DynamicBackColor = "IIF(EMPTY("+oFormSet.lcTmpLine+".cCatgTyp),12320767,16777215)"
ENDWITH

*!*************************************************************
*! Name      : lfGridNav
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/29/2004             
*! Purpose   : Navigate in the grid columns
*!*************************************************************
*! Call      : lfGridNav()
*!*************************************************************
FUNCTION lfGridNav
LPARAMETERS oFormSet, lnStep, lcGridCurs

LOCAL lnAlias, lnI
lnAlias = SELECT()

oFormSet.AriaForm1.LockScreen = .T.

SELECT (lcGridCurs)

lnI = 0
DO WHILE EMPTY(cCatgTyp)
  IF EOF() OR BOF()
    EXIT
  ENDIF
  lnI = lnI + lnStep
  SKIP lnStep
ENDDO
IF EOF() OR BOF()
  SKIP -lnI
ENDIF

oFormSet.AriaForm1.LockScreen = .F.
SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfInitForeCast
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/29/2004
*! Purpose   : Navigate in the grid columns
*!*************************************************************
*! Call      : lfInitForeCast()
*!*************************************************************
FUNCTION lfInitForeCast
LPARAMETERS oFormSet, oParent, lcCursName, lcActiveMode

WITH oFormSet
  .lcCursor   = lcCursName
  .loParent   = oParent
  .ActiveMode = lcActiveMode

  IF !oFormSet.loParent.llByShp
    .AriaForm1.grdCostForeInfo.RemoveObject("Column1")
  ENDIF
  
  .AriaForm1.lblStyle.Caption = ALLTRIM(gfItemMask('HM', '', oParent.lcInvType)) + ' :'
  .AriaForm1.cboStyle.Value   = oParent.laStyles[1]
  .AriaForm1.cboStyle.Valid()

  .AriaForm1.txtStyDesc.Enabled = .F.
  
  =lfRefreshForCast(oFormSet)
ENDWITH

*!*************************************************************
*! Name      : lfRefreshForCast
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/29/2004
*! Purpose   : Assign Grid Properties in Forecast Screen
*!*************************************************************
*! Call      : lfRefreshForCast()
*!*************************************************************
FUNCTION lfRefreshForCast
LPARAMETERS oFormSet

WITH oFormSet.AriaForm1.grdCostForeInfo
  LOCAL lnI, lcI, lnJ, lnTemp, lnColumns
  lnTemp = oFormSet.loParent.lnSessions

  .RecordSource = oFOrmSet.lcCursor
  
  IF oFormSet.loParent.llByShp
    .Columns(1).ControlSource   = oFOrmSet.lcCursor + '.cTktNo'
    .Columns(1).ReadOnly        = .T.
    .Columns(1).Header1.Caption = LANG_POACFRV_PON
    .Column1.DynamicInputMask   = "IIF(EMPTY("+oFormSet.lcCursor+".cCatgTyp), '', 'XXXXXX')"
    lnI = 2
  ELSE
    lnI = 1
  ENDIF

  .Columns(lnI).ControlSource = oFOrmSet.lcCursor + '.Item'
  .Columns(lnI).ReadOnly = .T.
  .Columns(lnI).Header1.Caption = LANG_POACFRV_ITM
  .Columns(lnI+1).ControlSource = oFOrmSet.lcCursor + '.Mfg'
  .Columns(lnI+1).ReadOnly = .T.
  .Columns(lnI+1).Header1.Caption = LANG_POACFRV_MFG
  .Columns(lnI+2).ControlSource = oFOrmSet.lcCursor + '.UOM'
  .Columns(lnI+2).ReadOnly = .T.
  .Columns(lnI+2).Header1.Caption = LANG_POACFRV_UOM
  .Columns(lnI+3).ControlSource = oFOrmSet.lcCursor + '.Desc'
  .Columns(lnI+3).ReadOnly = .T.
  .Columns(lnI+3).Header1.Caption = LANG_POACFRV_DSC
  .Columns(lnI+4).ControlSource = oFOrmSet.lcCursor + '.EstItmQty'
  .Columns(lnI+4).ReadOnly = .T.
  .Columns(lnI+4).Header1.Caption = LANG_POACFRV_ESTQTY
  .Columns(lnI+5).ControlSource = oFOrmSet.lcCursor + '.EstUntCst'
  .Columns(lnI+5).ReadOnly = .T.
  .Columns(lnI+5).Header1.Caption = LANG_POACFRV_ESTCST
  .Columns(lnI+6).ControlSource = oFOrmSet.lcCursor + '.EstItmAmt'
  .Columns(lnI+6).ReadOnly = .T.
  .Columns(lnI+6).Header1.Caption = LANG_POACFRV_EXT
  
  lnI = lnI + 7
  
  *--Adjust Receives Columns
  FOR lnJ = 1 TO lnTemp
    .Columns(lnI).ControlSource   = oFOrmSet.lcCursor + '.ItemQtyR' + STR(lnJ, 1)
    .Columns(lnI).ReadOnly        = .T.
    .Columns(lnI).Header1.Caption = LANG_POACFRV_QTY + oFormSet.loParent.laSessions[lnJ,2]

    .Columns(lnI+1).ControlSource   = oFOrmSet.lcCursor + '.UntCstR'  + STR(lnJ, 1)
    .Columns(lnI+1).ReadOnly        = .T.
    .Columns(lnI+1).Header1.Caption = LANG_POACFRV_CST + oFormSet.loParent.laSessions[lnJ,2]

    .Columns(lnI+2).ControlSource   = oFOrmSet.lcCursor + '.ItmAmtR'  + STR(lnJ, 1)
    .Columns(lnI+2).ReadOnly        = .T.
    .Columns(lnI+2).Header1.Caption = LANG_POACFRV_EXT + oFormSet.loParent.laSessions[lnJ,2]

    lnI = lnI + 3
  ENDFOR
  
  .Columns(lnI).ControlSource    = oFOrmSet.lcCursor + '.CurItmQty'
  .Columns(lnI).ReadOnly         = (oFormSet.ActiveMode = 'V')
  .Columns(lnI).Header1.Caption  = LANG_POACFRV_CURQTY
  .Columns(lnI).DynamicInputMask = "IIF(EMPTY("+oFormSet.lcCursor+".cCatgTyp), '', '9999.999')"

  lnI = lnI + 1
  .Columns(lnI).ControlSource    = oFOrmSet.lcCursor + '.CurUntCst'
  .Columns(lnI).ReadOnly         = (oFormSet.ActiveMode = 'V')
  .Columns(lnI).Header1.Caption  = LANG_POACFRV_CURCST
  .Columns(lnI).DynamicInputMask = "IIF(EMPTY("+oFormSet.lcCursor+".cCatgTyp), '', '99999999.999')"

  lnI = lnI + 1
  .Columns(lnI).ControlSource   = oFOrmSet.lcCursor + '.CurItmAmt'
  .Columns(lnI).ReadOnly        = .T.
  .Columns(lnI).Header1.Caption = LANG_POACFRV_EXT

  *--Hided not needed Columns
  FOR lnJ = 1 TO .ColumnCount
    IF lnJ > lnI
      .Columns(lnJ).Visible = .F.
    ELSE
      .Columns(lnJ).Visible = .T.
    ENDIF
  ENDFOR
  
  *--Set dyenamic colors.
  .SetAll("Dynamicbackcolor", "", "Column")
  .SetAll("FontName", "Tahoma", "Column")
  .SetAll("Alignment", 2, "Header")
  
  .SetAll("Dynamicbackcolor","IIF("+oFormSet.lcCursor+".cSort='0',16769996,IIF("+oFormSet.lcCursor+;
                             ".cSort $ '2G',16763806,16777215))", "Column")
  .columns(IIF(oFormSet.loParent.llByShp, 5, 4)).DynamicBackColor="IIF("+oFormSet.lcCursor+".cSort='0',12320767,IIF("+oFormSet.lcCursor+;
                            ".cSort $ '2G',16763806,16777215))"
ENDWITH

*!*************************************************************
*! Name      : lfRestoreGrid
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/29/2004
*! Purpose   : Remove Grid Control Sources
*!*************************************************************
*! Call      : lfRestoreGrid()
*!*************************************************************
FUNCTION lfRestoreGrid
LPARAMETERS grdContrrol

LOCAL lnI

WITH grdContrrol
  .RecordSource = ''
  
  FOR lnI = 1 TO .ColumnCount
    .Columns(lnI).ControlSource  = ''
  ENDFOR
ENDWITH

RETURN

*!**************************************************************************
*! Name      : lpDispScr
*! Developer : Wael M. Abo-Shawareb
*! Date      : 09/27/2004
*! Purpose   : Display Screen to show the total costs for the shipment.
*!*************************************************************
*! Example   : =lpDispScr
*!*************************************************************
PROCEDURE lpDispScr
LPARAMETERS oFormSet

LOCAL lcTmpFil, lcBrTrttl
lcTmpFil  = oFormSet.lcShpTotCurs
lcBrTrttl = LANG_POACFRV_SHPMNT + ' ' + LANG_POACFRV_TOTCST

CREATE CURSOR (lcTmpFil) (cItem C(19),cCost C(13))

IF !lfColctDat(oFormSet, lcTmpFil)
  RETURN
ENDIF

DO FORM (oAriaApplication.ScreenHome + "POSHCST") WITH lcTmpFil, lcBrTrttl

*!*************************************************************
*! Name      : lfColctDat
*! Developer : Wael M. Abo-Shawareb
*! Date      : 09/27/2004
*! Purpose   : Collect needed Data 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfColctDat()
*!*************************************************************
FUNCTION lfColctDat
LPARAMETERS oFormSet, lcTmpFil

LOCAL lnTotAmt, lcOldTag, lnAlias, lnRecNum, lnCounter, lnAlias, lcCounter, lcStatement, lcTmpPOSLN
LOCAL lnTotAmt1, lnTotAmt2, lnTotAmt3, lnTotAmt4, lnTotAmt5, lnTotAmt6, lnTotAmt7, lnTotAmt

STORE 0 TO lnTotAmt1, lnTotAmt2, lnTotAmt3, lnTotAmt4, lnTotAmt5, lnTotAmt6, lnTotAmt7, lnTotAmt
lnAlias    = SELECT()
lcTmpPOSLN = gfTempName()

SELECT (oFormSet.lcTmpLine)
lnRecNum = RECNO()
lcOldTag = SET('ORDER')

SET ORDER TO TAG (oFormSet.lcTmpLine1)

FOR lnCounter = 1 TO 7
  lcCounter = STR(lnCounter,1)
  SELECT (oFormSet.lcTmpLine)
  IF SEEK(lcCounter)
    SCAN REST WHILE cBomTyp+MfgCode+Item+Style = lcCounter
      lcStatement = "SELECT TotQty FROM POSLN (INDEX = POSLNSH)" +;
                    "       WHERE ShipNo = '" + ShipNo + "' AND" +;
                    "             cBusDocu = 'P' AND" +;
                    "             cStyType = 'P' AND" +;
                    "             PO = '" + cTktNo + "' AND" +;
                    "             cInvType = '" + cInvType + "' AND" +;
                    "             Style = '" + Style + "' AND" +;
                    "             [LineNo] = '" + STR(LineNo) + "' AND" +;
                    "             TranCD = '3'"
      
      IF !lfSQLStatement(lcStatement, lcTmpPOSLN, '', '')
        SELECT(lnAlias)
        SELECT (oFormSet.lcTmpLine)
        SET ORDER TO &lcOldTag
        RETURN .F.
      ENDIF

      IF !EOF(lcTmpPOSLN)
        SELECT (oFormSet.lcTmpLine)
        lnTotAmt&lcCounter = lnTotAmt&lcCounter + (unitcost * EVALUATE(lcTmpPOSLN + '.TotQty'))
      ENDIF
    ENDSCAN

    IF lnTotAmt&lcCounter <> 0
      SELECT (lcTmpFil)
      APPEND BLANK
      REPLACE cCost WITH STR(lnTotAmt&lcCounter,13,3),;
              cItem WITH ALLTRIM(oFormSet.laCost[lnCounter,3])
      lnTotAmt = lnTotAmt + lnTotAmt&lcCounter
    ENDIF
  ENDIF  
ENDFOR

IF USED(lcTmpPOSLN)
  USE IN (lcTmpPOSLN)
ENDIF
    
SELECT (lcTmpFil)
APPEND BLANK
APPEND BLANK

REPLACE cCost WITH STR(lnTotAmt,13,3) ,;
        cItem WITH "Total Shipment"
LOCATE

SELECT (oFormSet.lcTmpLine)
SET ORDER TO &lcOldTag

IF BETWEEN(lnRecNum,1,RECCOUNT())
  GOTO lnRecNum
ENDIF  

SELECT(lnAlias)
RETURN

*!*************************************************************
*! Name      : lfHeaderSeek
*! Developer : Wael M. Abo-Shawareb
*! Date      : 09/27/2004
*! Purpose   : Seek in the POSHDR or SHOMTHDR files
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfHeaderSeek()
*!*************************************************************
FUNCTION lfHeaderSeek
LPARAMETERS oFormSet, lcKeyVal

LOCAL lnRet

lcKeyVal = oFormSet.cBrowseKey + lcKeyVal
lnRet    = oFormSet.SeekRecord()

RETURN IIF(lnRet = 0, .F., .T.)
*-- End...
*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
*:**************************************************************************
*:* Name        : lfVSelect
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Validate Type of receive by
*:***************************************************************************
*E302483,1 
FUNCTION lfVSelect
PARAMETERS lnValue,loFormSet

IF lnValue = 1
  
  loFormSet.llByShp = .T.
  DIMENSION loFormSet.laTemplate[1,2]
  loFormSet.laTemplate = ''
  =gfOpenTable('SHPCSTHD','SHPCSTHD','SH')
  gfSeek("",'SHPCSTHD')
  *SELECT  ' '+CTMPLTDSC,CTEMPLATE FROM SHPCSTHD INTO ARRAY loFormSet.laTemplate ORDER BY  ldefault,ctemplate
  SELECT  ' '+CTMPLTDSC,CTEMPLATE FROM SHPCSTHD INTO ARRAY loFormSet.laTemplate ORDER BY  ldefault DESC 
  IF EMPTY(loFormSet.laTemplate[1,2])
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_POACFRV_No_TEMPLATE )
 	loFormSet.changemode("S")
 	RETURN
  ELSE
    loFormSet.AriaForm1.cboTemplates.Value = loFormSet.laTemplate [1,2]
   	loFormSet.AriaForm1.cboTemplates.Enabled = .T.
   	loFormSet.AriaForm1.cboTemplates.requery 
    loFormSet.AriaForm1.cboTemplates.REFRESH	 
    lfvTemplate(loFormSet.AriaForm1.cboTemplates.Value,loFormSet)
  ENDIF 

  
  
  WITH loFormSet.AriaForm1
    STORE .F. TO .shpDatesBud.Visible,.lblTktNo.Visible ,.shpTktNo.Visible ,;
                 .lblEntered.Visible ,.txtEntered.Visible ,.lblComplete.Visible,.grdCostInfo.Visible,;
                 .txtComplete.Visible,.lblBudget.Visible,.txtBudget.Visible,.lblTktNoDots.Visible                          
                 
   
    STORE .T. TO .cmdApprove.Visible,.cboSelect.Visible,.shpSelect.Visible,.shpHeaderData.Visible,;
                 .lblShpNo.Visible,.lblTemplate.Visible ,.cboTemplates.Visible,.kbPO.Visible,;
                 .lblEnterShp.Visible ,.txtShpEntered.Visible,.lblCopmShp.Visible,.txtCompShp.Visible ,;
                 .lblContainer.Visible, .txtContainer.Visible, .lblrefer.Visible ,  .txtReference.Visible ,;
                 .lblNotes.Visible,.txtNotes.Visible , .lblVessel.Visible , .txtAirVessel.Visible,;
                 .cboCostItems.Visible ,.shpCostItems.Visible, .lblAmnt.Visible,.txtAmnt.Visible ,;
                 .lblPercentage.Visible,.txtPercent.Visible, .cmdDistribute.Visible,  .shpUpdate.Visible,;
                 .chkUpdateAll.Visible,.cmdApprove.Visible ,.shpDetails.Visible , .lblStydesc.Visible ,;
                 .lblClrDesc.Visible ,.lblScale.Visible, .txtStyDesc.Visible , .txtClrDesc.Visible ,;
                 .txtScaleDesc.Visible ,   .sbQuantites.Visible , .lblCostItems.Visible ,.grdShpCost.Visible,.chkFreight.Visible           
    .cboCostItems.eNABLED = .F.             
  ENDWITH 
  
  IF !loFormSet.llShpCsUsr
    STORE .F. TO loFormSet.AriaForm1.cmdApprove.Visible
  ENDIF 
ELSE
  loFormSet.llByShp = .F.
  WITH loFormSet.AriaForm1
  
    STORE .T. TO .shpDatesBud.Visible,.lblTktNo.Visible ,.shpTktNo.Visible  ,;
                 .lblEntered.Visible,.txtEntered.Visible,.lblComplete.Visible ,.grdCostInfo.Visible,;
                 .txtComplete.Visible ,.lblBudget.Visible ,.txtBudget.Visible ,.lblTktNoDots.Visible 
                 
                 
    STORE .F. TO .shpHeaderData.Visible ,.lblShpNo.Visible ,.lblTemplate.Visible ,;
                 .cboTemplates.Visible  ,.lblEnterShp.Visible,.txtShpEntered.Visible,.lblCopmShp.Visible,;
                 .txtCompShp.Visible , .lblContainer.Visible, .txtContainer.Visible,.lblrefer.Visible,;
                 .txtReference.Visible , .lblNotes.Visible,  .txtNotes.Visible, .lblVessel.Visible ,;
                 .txtAirVessel.Visible,.cboCostItems.Visible,.shpCostItems.Visible,.lblAmnt.Visible,;
                 .txtAmnt.Visible ,.lblPercentage.Visible,.txtPercent.Visible,.cmdDistribute.Visible,;
                 .shpUpdate.Visible,.chkUpdateAll.Visible ,.cmdApprove.Visible,.shpDetails.Visible ,;
                 .lblStydesc.Visible ,.lblClrDesc.Visible,.lblScale.Visible ,.txtStyDesc.Visible,.grdShpCost.Visible,;
                 .txtClrDesc.Visible,.txtScaleDesc.Visible,.sbQuantites.Visible,.lblCostItems.Visible,.chkFreight.Visible             
                
  ENDWITH 
ENDIF
lfRefreshVar(loFormSet)

loFormSet.nWorkArea = loFormSet.GetMasterFile()


IF TYPE("loFormSet.cBrowseFileName") = 'C' .AND. !EMPTY(loFormSet.cBrowseFileName) .AND. ;
   !ISNULL(loFormSet.cBrowseFileName)
  loFormSet.AriaBrFields.edtBrowseFields.Value = loFormSet.Getbrowsefields(loFormSet.cBrowseFileName)
ELSE
  loFormSet.AriaBrFields.edtBrowseFields.Value = loFormSet.Getbrowsefields(loFormSet.nWorkArea)
ENDIF


loFormSet.nWorkArea = IIF(TYPE('loFormSet.nWorkArea')#'C','',loFormSet.nWorkArea)
IF !EMPTY(loFormSet.nWorkArea)
  SELECT (loFormSet.nWorkArea)
  loFormSet.Hasnotes = TYPE("lHasNotes")<>"U"
  
  lcKeyExp = loFormSet.getKeyExpr()
  IF ATC('(',lcKeyExp)>0 OR ATC('+',lcKeyExp)>0
    lcKeyExp = '('+lcKeyExp+')'
  ENDIF
ENDIF
loFormSet.Key = lcKeyExp


IF TYPE("loFormSet.oToolBar") = 'O'
  loFormSet.oToolBar.buttonrefresh()
endif   


IF !USED('SHPMTHDR')
  =gfOpenTable('SHPMTHDR','SHPMTHDR','SH')
ENDIF 

IF !USED('CODES')
  =gfOpenTable('CODES','CODES','SH')
ENDIF   
*E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]

*!*************************************************************
*! Name      : lfRefreshVar
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/25/2004
*! Purpose   : Initialize Screen Properties.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : oFormSet --> FormSet Object Reference
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfFormInit()
*!*************************************************************
FUNCTION lfRefreshVar
LPARAMETERS oFormSet

*--Screen Variables.
WITH oFormSet
 
  .lcTitle = IIF(.llByShp, LANG_POACFRV_SHPMNT, LANG_POACFRV_POORD)

  *--Create Temp Cursors and Indecies Names
  IF EMPTY(.lcPOSHDR)
    .lcPOSHDR     = gfTempName()
  ENDIF 
    
  

  *--Get Costing Elements Types and labels
  
  *--Open needed files.
  LOCAL lSQLCommand
  
  
    .lcInvType = '0001'
    
      IF .llByShp
        .lcBrowFlds = "ShipNo   :H='"+LANG_POACFRV_SHPMNTNO +"',"+;
                      "Status   :H='"+LANG_POACFRV_STATUS   +"',"+;
                      "Entered  :H='"+LANG_POACFRV_ENTERED  +"',"+;
                      "Cartons  :H='"+LANG_POACFRV_CARTONS  +"',"+;
                      "AirWayB  :H='"+LANG_POACFRV_AIRWAYB +"',"+;
                      "ETA      :H='"+LANG_POACFRV_ETA      +"',"+;
                      "TotQtyHDR:H='"+LANG_POACFRV_INTRANSIT+"',"+;
                      "Recv_Stk :H='"+LANG_POACFRV_RECEIVED +"',"+;
                      "Recv_Dam :H='"+LANG_POACFRV_DAMAGE   +"',"+;
                      "Recv_Can :H='"+LANG_POACFRV_CANCELED +"',"+;
                      "cVessel  :H='"+LANG_POACFRV_CVESSEL +"',"+;
                      "Reference:H='"+LANG_POACFRV_REFERENCE  +"'"
      ELSE
        .lcBrowFlds = "PO        :H='"+LANG_POACFRV_CUTTKTNO+"',"+;
                      "Status    :H='"+LANG_POACFRV_STATUS  +"', "+;
                      "Vendor    :H='"+LANG_POACFRV_VENDOR  +"',"+;
                      "lcVnName = ApVendor.cVenComp :H='"+LANG_POACFRV_VENCOMP+"',"+;
                      "Entered   :H='"+LANG_POACFRV_ENTERED +"',"+;
                      "Complete  :H='"+LANG_POACFRV_COMPLETE+"',"+;
                      "nStyOrder :H='"+LANG_POACFRV_TOTQTY  +"',"+;
                      "POTotal   :H='"+LANG_POACFRV_AMOUNT  +"',"+;
                      "Receive   :H='"+LANG_POACFRV_REC     +"',"+;
                      "Open      :H='"+LANG_POACFRV_OPEN    +"'"

        lSQLCommand = "SELECT * FROM APVENDOR"
        lnRemResult = oAriaApplication.RemoteSystemData.Execute(lSQLCommand, '', 'APVENDOR', '', ;
                              oAriaApplication.cAriaNativeDataFilesConStr, 3, ;
                              '', .DataSessionId)
        IF lnRemResult < 0
          RETURN .F.
        ENDIF
        
        LOCAL lnOldBuffMode
        lnOldBuffMode = CURSORGETPROP("Buffering", "APVENDOR")
        
        =CURSORSETPROP("Buffering", 3, "APVENDOR")

        SELECT APVENDOR
        INDEX ON CVENDCODE TAG VENCODE
        
        =CURSORSETPROP("Buffering", lnOldBuffMode, "APVENDOR")
      ENDIF

  *-- To pass the required information for the work order key #. 
  WITH .Ariaform1.kbPo
    .cbusinessdocumenttype = 'P'
    .cworkordertype        = IIF(oFormSet.lcPType = 'I', 'P', IIF(oFormSet.lcPType = 'M', 'U', 'F'))
    .obrowsecursor         = oFormSet.lcPosHdr
    .cbrowsetitle          = oFormSet.lcTitle
    .cbrowsefields         = oFormSet.lcBrowFlds
    .lcBrowRetFld          = IIF(oFormSet.lcPType = 'I' AND oFormSet.llByShp, 'ShipNo', 'PO')
    .Keytextbox.InputMask  = lfPicture(oFormSet.lcPType, oFormSet.llByShp)
    .Keytextbox.Format     = lfPicture(oFormSet.lcPType, oFormSet.llByShp)
  ENDWITH

  *--Assign SQL Browse Properties
  LOCAL llNoRecordFound, llServerError
  
  .cBrowseTableDBEngine   = 'SQL'
  .cBrowseAliasName       = .lcPOSHDR
  .cBrowseTableName       = IIF(.llByShp, 'SHPMTHDR', 'POSHDR')
  .cBrowseIndexName       = IIF(.llByShp, 'SHPMTHDR', 'POSHDR')
  
  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
  .cBrowseFileName        = IIF(.llByShp, 'SHPMTHDR', 'POSHDR')
  *E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
  
  .cBrowseIndexExpression = IIF(.llByShp, 'cBusDocu+cShpType+ShipNo', 'cBusDocu+cStyType+PO')
  .cBrowseIndexFields     = IIF(.llByShp, 'cBusDocu,cShpType,ShipNo', 'cBusDocu,cStyType,PO')
  .cBrowseKey             = IIF(.lcPType = 'I', 'PP', IIF(.lcPType = 'M', 'PU', 'PF'))
  .BrowseTitle            = IIF(.llByShp, LANG_POACFRV_BROETTLSHP,;
                                IIF(.lcPType = 'I', LANG_POACFRV_BROETTLSPO,;
                                    IIF(.lcPType = 'M', LANG_POACFRV_BROETTLSCC,;
                                        LANG_POACFRV_BROETTLMMF)))

  .oRemoteCursor.mGetCursor(.cBrowseAliasName,.cBrowseAliasName,.DataSessionID,;
                            'STRUCTURE',.F.,.cBrowseTableName,'*','',.cBrowseIndexName,;
                            .cBrowseIndexExpression,.F.,.cBrowseIndexFields,0,;
                            .cBrowseFilter,.cBrowseKey,@llNoRecordFound,@llServerError)

  *--Set value for FormSet Master File and Data Environment Initial Selected Cursor
  .DataEnvironment.InitialSelectedAlias = .lcPOSHDR

  *--Create Index for POSHDR Cursor
  LOCAL lnOldBuffMode, lcHdrIdx
  lnOldBuffMode = CURSORGETPROP("Buffering", .lcPOSHDR)
  lcHdrIdx      = IIF(.llByShp, 'ShipNo', 'PO')

  =CURSORSETPROP("Buffering", 3, .lcPOSHDR)

  SELECT (.lcPOSHDR)
  INDEX ON &lcHdrIdx TAG POSHDR

  =CURSORSETPROP("Buffering", lnOldBuffMode)

  IF .lcPType = 'I' AND !.llByShp
    SET RELATION TO Vendor INTO ApVendor
  ENDIF
  *--Create the Temporary BOMLine file
ENDWITH
oFormSet.AriaBrFields.edtBrowseFields.Value = oFormSet.lcBrowFlds


*:**************************************************************************
*:* Name        : lfvTemplate
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Validate Template Selected 
*:***************************************************************************
*E302483,1 
FUNCTION lfvTemplate
PARAMETERS lcTempValue,loFormSet



IF !USED("SHPCSTLN")
  =gfOpenTable('SHPCSTLN','SHPCSTLN','SH')
ENDIF 

DIMENSION loFormSet.laCostItem[1,3]
loFormSet.laCostItem = ''
IF gfSeek(PADR(lcTempValue,6),"SHPCSTLN","SHPCSTLN")
  SELECT CCODE_NO+'-'+CDESCRIP, '*'+CCODE_NO+'*',0,CCATGTYP,CBOMTYP,.F.;
       FROM SHPCSTLN ;
       WHERE CTEMPLATE = PADR(lcTempValue,6);
       ORDER BY LINENO ;
       INTO ARRAY loFormSet.laCostItem

  
  IF !EMPTY(loFormSet.laCostItem)
    DIMENSION loFormSet.laCostItem[ALEN(loFormSet.laCostItem,1)+1 , ALEN(loFormSet.laCostItem,2)]
    
    =AINS(loFormSet.laCostItem,1)
    loFormSet.laCostItem[1,1] = LANG_POACFRV_ALL_COST  
    
    FOR lnI = 2 TO ALEN(loFormSet.laCostItem,1)
      IF loFormSet.laCostItem[lnI,4] = 'D'
        loFormSet.laCostItem[lnI,2] = '*'+'######'+'*'
      ENDIF
    ENDFOR
  ELSE
    loFormSet.laCostItem[1,1] = LANG_POACFRV_NO_COST_ITEM
  ENDIF  
ENDIF 
loFormSet.AriaForm1.cboCostItems.DISPLAYvALUE = loFormSet.laCostItem[1,1] 
loFormSet.AriaForm1.cboCostItems.Refresh()  
loFormSet.AriaForm1.cboCostItems.ReQUERY  

*:**************************************************************************
*:* Name        : lfvAmount
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Validate Amount Entered 
*:***************************************************************************
*E302483,1 
FUNCTION lfvAmount
PARAMETERS lcType,loFormset

IF lcType  = 'P'
  lnAmount = loFormset.AriaForm1.txtPercent.Value
ELSE
  lnAmount = loFormset.AriaForm1.txtAmnt.Value
ENDIF 


DO CASE
  CASE lcType = 'A'
    IF lnAmount > 0
      loFormset.AriaForm1.txtPercent.Value = 0
      loFormset.AriaForm1.txtPercent.Enabled = .F.
    ENDIF

  CASE lcType = 'P'

    IF lnAmount > 0
      loFormset.AriaForm1.txtAmnt.Value = 0
      loFormset.AriaForm1.txtAmnt.Enabled = .F.
    ENDIF
ENDCASE

loFormSet.AriaForm1.cmdDistribute.Enabled = .T.

*-- end of lfvAmount.
*:**************************************************************************
*:* Name        : lfCreatShpTemp
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Collect Shipment information
*:***************************************************************************
*E302483,1 
FUNCTION lfCreatShpTemp
PARAMETERS loFormSet

IF !USED('POSLN')
  =gfOpenTable('POSLN','POSLN','SH')
ENDIF 
SELECT POSLN
DIMENSION laFileStru[1,18]
AFIELDS(laFileStru)
lnLn = ALEN(laFileStru,1)
lnOldLen = lnLn 
lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'KEY'
laFileStru[lnLn,2] = 'C'
laFileStru[lnLn,3] = 1
laFileStru[lnLn,4] = 0



FOR lnI = 2 TO ALEN(loFormSet.laCostItem,1)
  lcI = ALLTRIM(STR(lnI-1))
  lnLn = lnLn + 1
  DIMENSION laFileStru[lnLn,18]
  laFileStru[lnLn,1] = 'COST' + lcI
  laFileStru[lnLn,2] = 'N'
  laFileStru[lnLn,3] = 10
  laFileStru[lnLn,4] = 4

  lnLn = lnLn + 1
  DIMENSION laFileStru[lnLn,18]
  laFileStru[lnLn,1] = 'COST' + lcI + 'COD'
  laFileStru[lnLn,2] = 'C'
  laFileStru[lnLn,3] = 6
  laFileStru[lnLn,4] = 0

  lnLn = lnLn + 1
  DIMENSION laFileStru[lnLn,18]
  laFileStru[lnLn,1] = 'COST' + lcI + 'PER'
  laFileStru[lnLn,2] = 'N' 
  laFileStru[lnLn,3] = 6
  laFileStru[lnLn,4] = 2
    
  lnLn = lnLn + 1
  DIMENSION laFileStru[lnLn,18]
  laFileStru[lnLn,1] = 'COSTSTAT' + lcI
  laFileStru[lnLn,2] = 'C'
  laFileStru[lnLn,3] = 1
  laFileStru[lnLn,4] = 0

  lnLn = lnLn + 1
  DIMENSION laFileStru[lnLn,18]
  laFileStru[lnLn,1] = 'PERCNTOF' + lcI
  laFileStru[lnLn,2] = 'M'
  laFileStru[lnLn,3] = 10
  laFileStru[lnLn,4] = 0
    
  lnLn = lnLn + 1
  DIMENSION laFileStru[lnLn,18]
  laFileStru[lnLn,1] = 'UNTCST' + lcI
  laFileStru[lnLn,2] = 'N'
  laFileStru[lnLn,3] = 10
  laFileStru[lnLn,4] = 4
ENDFOR

lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'COSTSTAT'     &&->>>2
laFileStru[lnLn,2] = 'C'
laFileStru[lnLn,3] = 1
laFileStru[lnLn,4] = 0

lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'ROYLPERCNT'
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 6
laFileStru[lnLn,4] = 2

lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'DUTYPERCNT'
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 6
laFileStru[lnLn,4] = 2
  
lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'NDUTYRAT'
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 9
laFileStru[lnLn,4] = 4

lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'TOTCOST'
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 11
laFileStru[lnLn,4] = 4
 
lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'CCURMETH'
laFileStru[lnLn,2] = 'C'
laFileStru[lnLn,3] = 1
laFileStru[lnLn,4] = 0
  
lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'CUNTMETH'
laFileStru[lnLn,2] = 'C'
laFileStru[lnLn,3] = 1
laFileStru[lnLn,4] = 0

lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'CDUTCURMTD'
laFileStru[lnLn,2] = 'C'
laFileStru[lnLn,3] = 1
laFileStru[lnLn,4] = 0
  
lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'CDUTUNTMTD'
laFileStru[lnLn,2] = 'C'
laFileStru[lnLn,3] = 1
laFileStru[lnLn,4] = 0

lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'NCURRUNIT'
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 4
laFileStru[lnLn,4] = 0
  
lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'NDCURUNIT'
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 4
laFileStru[lnLn,4] = 0
  
lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'BPRICE'
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 12
laFileStru[lnLn,4] = 4
  
lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'TOTSALEPRI'
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 15
laFileStru[lnLn,4] = 3
  
lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'nTrDsVl'   && Trade Disc value: 
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 13
laFileStru[lnLn,4] = 3

lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'nStlmVl'   && Settlement Disc Value
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 13
laFileStru[lnLn,4] = 3

  

lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'NPRICERAT'   && Price Rate
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 9
laFileStru[lnLn,4] = 2

lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'NTRDISC'   && Trade Disc %
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 6
laFileStru[lnLn,4] = 2

lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'STLMNTDISC'   && Settlement Disc %
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 6
laFileStru[lnLn,4] = 2

lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'PRICEA'   && PriceA
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 12
laFileStru[lnLn,4] = 2

lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'NROYALTY'   && Royalty
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 10
laFileStru[lnLn,4] = 3


lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'TUCost'
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 10
laFileStru[lnLn,4] = 4

lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'Mrgn'
laFileStru[lnLn,2] = 'N'
laFileStru[lnLn,3] = 10
laFileStru[lnLn,4] = 4


lnLn = lnLn + 1
DIMENSION laFileStru[lnLn,18]
laFileStru[lnLn,1] = 'CURR'
laFileStru[lnLn,2] = 'C'
laFileStru[lnLn,3] = 3
laFileStru[lnLn,4] = 0


FOR lnCount = lnOldLen  TO ALEN(laFileStru,1)
  STORE '' TO laFileStru[lnCount,7],laFileStru[lnCount,8],laFileStru[lnCount,9],;
    laFileStru[lnCount,10],laFileStru[lnCount,11],laFileStru[lnCount,12],;
    laFileStru[lnCount,13],laFileStru[lnCount,14],laFileStru[lnCount,15],;
    laFileStru[lnCount,16]
    
  STORE 0 TO  laFileStru[lnCount,17],laFileStru[lnCount,18]
ENDFOR



DIMENSION laTags[1,2]
laTags[1,1] = 'KEY+PO+STYLE+STR(LINENO,6)'
laTags[1,2] = loFormSet.lcTmpSpLn



=gfCrtTmp(loFormSet.lcTmpSpLn,@laFileStru,@laTags)

*:**************************************************************************
*:* Name        : lfGetShipData
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Collect Shipment information
*:***************************************************************************
*E302483,1 
FUNCTION lfGetShipData
PARAMETERS loFormSet

IF USED(loFormSet.lcTmpSpLn)
  Sele(loFormSet.lcTmpSpLn)
  DELETE ALL 
ENDIF 

lcSHIPNO = PADR(loFormSet.AriaForm1.kbPO.KeyTextBox.Value ,6)
=gfOpenTable('POSLN','poslnsh','SH','POSLNSHP')
=gfOpenTable('POSHDR','POSHDR','SH','POSHD')
=gfOpenTable('SHPMTHDR','SHPMTHDR','SH','SHPMTH')
=gfOpenTable('SHPRLFLD','SHPRLFLD','SH','SHPRLFLD')
=gfOpenTable('STYLE','STYLE','SH','STYLE_A')
=gfOpenTable('CUTPICK','CUTPICK','SH','CUTPICK')
=gfOpenTable('ORDHDR','ORDHDR','SH','ORDHDR')
=gfOpenTable('ORDLINE','ORDLINE','SH','ORDLINE')
=gfOpenTable('SHPMFGDT','SHPMFGDT','SH','SHPMFGDT')


gfSeek('PP'+lcSHIPNO,'SHPMTH')

IF !EMPTY(SHPMTH.CTEMPLATE)
  lcOldAlias = SELECT()
  
  loFormSet.AriaForm1.cboTemplates.Value = SHPMTH.CTEMPLATE
  
  lfvTemplate(loFormSet.AriaForm1.cboTemplates.Value,loFormSet)
  
  gfSeek(SHPMTH.SHIPNO,'SHPMFGDT','SHPMFGDT')
  SELECT SHPMFGDT
  SCAN 
    lnPos = ASCAN(loFormSet.laCostItem,SHPMFGDT.mfgcode)
    lnPos = ASUBSCRIPT(loFormSet.laCostItem,lnPos,1)
    loFormSet.laCostItem[lnPos,6] = llduty
  ENDSCAN  
  
  SELECT(lcOldAlias)
ENDIF 



lfCreatShpTemp(loFormSet)
lfGrdShipCntSrc(loFormSet)


*B608510,2 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[Start]
*IF SHPMTH.Status = 'O' 

*B610211,1 HIA 01/22/2013 Aria4xp - PO - Adjust Costs for Receiving by Shipment[T20130121.0001][Start]
*IF SHPMTH.Status $ 'OC' 
IF (SHPMTH.Status $ 'OC') AND not(UPPER(ALLTRIM(SHPMTH.Status)) == 'O' AND not(loFormSet.llShpApprove))
*B610211,1 HIA 01/22/2013 Aria4xp - PO - Adjust Costs for Receiving by Shipment[T20130121.0001][End]

*B608510,2 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[End]
  loFormSet.llenablebutt = .F.
ELSE

  loFormSet.llenablebutt = .T.  
ENDIF

SELECT POSLNSHP

=gfSEEK(lcSHIPNO,'POSLNSHP')

SCAN REST WHILE shipno+ cbusdocu+ cstytype+po+ cinvtype+ style+ STR(lineno,6)+ trancd  =lcSHIPNO +"PP";
     FOR TRANCD $ IIF(SHPMTH.STATUS $ 'OH' , '23' , '2' )
   =gfSeek('PP'+PO,'POSHD')     
   SCATTER MEMVAR
   =gfSEEK(POSLNSHP.SHIPNO+POSLNSHP.PO+STR(POSLNSHP.LINENO,6),'SHPRLFLD')
   =gfSEEK(POSLNSHP.Style,'STYLE_A')
   
   m.PRICEA = IIF(!llFstTme,STYLE_A.PRICEA,SHPRLFLD.PRICEA)
   
   m.CURR = POSHD.CPRICECUR
   m.NCURRUNIT = POSHD.NCURRUNIT
   M.NDCURUNIT = POSHD.NDCURUNIT 
   lcPUnMeth = ''
   lcPMethod = gfGetExSin(@lcPUnMeth,POSHD.CPRICECUR)
   lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
   lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
   M.CCURMETH = lcPMethod
   M.CUNTMETH = lcPUnMeth      
   m.NPRICERAT = IIF(SHPRLFLD.NPRICERAT = 0 , POSHD.NPRICERAT , SHPRLFLD.NPRICERAT)      
   *m.NPRICERAT = POSHD.NPRICERAT      
   
   m.BPRICE = POSLNSHP.NFCOST1 &lcPMethod m.NPRICERAT &lcPUnMeth m.NCURRUNIT  
   lcDPUnMeth = ''
   lcDPMethod  = gfGetExSin(@lcDPUnMeth,POSHD.CDUTYCUR)
   lcDPMethod  = IIF(EMPTY(lcDPMethod ),'*',lcDPMethod )
   lcDPUnMeth = IIF(EMPTY(lcDPUnMeth),'/',lcDPUnMeth)
   M.CDUTUNTMTD = lcDPMethod 
   M.CDUTCURMTD = lcDPUnMeth
   
   M.NDUTYRAT   = IIF(SHPRLFLD.NDUTYRAT = 0 , POSHD.NDUTYRAT , SHPRLFLD.NDUTYRAT ) 
   
   *use the same exchnge rate signs for Paurchase price to the duty for GPS[Start]
   IF ASCAN(loFormSet.laEvntTrig,PADR('GPSDUTY',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
     loFormSet.mDoTrigger(PADR('GPSDUTY',10)) 
   ENDIF    
   *[End] 

   m.NTRDISC    = SHPRLFLD.NTRDISC
   m.STLMNTDISC = SHPRLFLD.STLMNTDISC
   M.NROYALTY   = SHPRLFLD.NROYALTY
   
   M.ACCOUNT = ''
   IF gfSEEK('2'+POSLNSHP.PO+POSLNSHP.STYLE,'CUTPICK')
     =gfSEEK('O'+CUTPICK.ORDER,'ORDHDR')
 
      m.NTRDISC = IIF(m.NTRDISC=0,ORDHDR.DISC,m.NTRDISC)
*!*       
*!*       *MMT
      lnSTDisc = 0
      DIMENSION laSTDisc[1,2]
      laSTDisc[1,1] = 'NTERDISCR'
      laSTDisc[1,2] = 'lnSTDisc'
      =gfRltFld(ORDHDR.ctermcode,@laSTDisc,'CTERMCODE ')
      m.STLMNTDISC =IIF(m.STLMNTDISC =0,lnSTDisc,m.STLMNTDISC)
*!*       *MMT
     IF gfSeek('O'+CUTPICK.ORDER+m.Style,'ORDLINE','ORDBLKST')
       *B610211,1 HIA 01/22/2013 Aria4xp - PO - Adjust Costs for Receiving by Shipment[T20130121.0001][Start]
       *m.PRICEA = IIF(m.PRICEA = 0,ORLINE.Price,m.PRICEA )
       m.PRICEA = IIF(m.PRICEA = 0,Ordline.Price,m.PRICEA )
       *B610211,1 HIA 01/22/2013 Aria4xp - PO - Adjust Costs for Receiving by Shipment[T20130121.0001][End]
     ENDIF 
         
     M.ACCOUNT = ORDHDR.ACCOUNT
     SELECT CUTPICK
     SCAN REST WHILE TRANCD+CTKTNO+STYLE = '2'+POSLNSHP.PO+POSLNSHP.STYLE
       =gfSEEK('O'+CUTPICK.ORDER,'ORDHDR')
       M.ACCOUNT = IIF(ORDHDR.ACCOUNT == M.ACCOUNT , M.ACCOUNT , '*****' )
       IF M.ACCOUNT = '*****'
         EXIT
       ENDIF
     ENDSCAN
   ENDIF
   

   
   IF !SEEK(' '+POSLNSHP.PO+POSLNSHP.STYLE+STR(POSLNSHP.LINENO,6) , loFormSet.lcTmpSpLn)
        INSERT INTO (loFormSet.lcTmpSpLn ) FROM MEMVAR
   ELSE
     SELECT (loFormSet.lcTmpSpLn )
     REPLACE QTY1 WITH QTY1 + POSLNSHP.QTY1 ;
             QTY2 WITH QTY2 + POSLNSHP.QTY2 ;
             QTY3 WITH QTY3 + POSLNSHP.QTY3 ;
             QTY4 WITH QTY4 + POSLNSHP.QTY4 ;
             QTY5 WITH QTY5 + POSLNSHP.QTY5 ;
             QTY6 WITH QTY6 + POSLNSHP.QTY6 ;
             QTY7 WITH QTY7 + POSLNSHP.QTY7 ;
             QTY8 WITH QTY8 + POSLNSHP.QTY8 ;
             TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
   
   ENDIF
   SELECT(loFormSet.lcTmpSpLn )
    IF !llFstTme
      PRIVATE laRoyalty,lnRylrate
      DECLARE laRoyalty[1,2]
      lnRylrate = 0
      laRoyalty[1,1] = 'NRYLRATE'
      laRoyalty[1,2] = 'lnRylrate'
      =gfRltFld(STYLE_A.ROYALTY,@laRoyalty,'ROYALTY   ')
      REPLACE ROYLPERCNT WITH lnRylrate ;
              NROYALTY   WITH PRICEA*lnRylrate*TOTQTY/100  
    ELSE
      REPLACE ROYLPERCNT WITH IIF(PRICEA=0,0, NROYALTY*100/( PRICEA*TOTQTY*(1 - NTRDISC/100) ) )
    ENDIF
     REPLACE nTrDsVl WITH PRICEA*TOTQTY*NTRDISC/100 ;
             nStlmVl WITH PRICEA*TOTQTY*(1-NTRDISC/100)*STLMNTDISC/100 
ENDSCAN 



SELECT(loFormSet.lcTmpSpLn)
APPEND BLANK
REPLACE KEY WITH CHR(255) 
PRIVATE lcExSign,lcUntSin
STORE '/' TO lcExSign,lcUntSin
lcTmpSpLn = loFormSet.lcTmpSpLn 
lcTmpLine = loFormSet.lcTmpLine
SELECT(loFormSet.lcTmpLine)
SCAN FOR LINENO <> 0
IF SEEK(' '+cTktNo+STYLE+STR(LINENO,6),lcTmpSpLn)          &&->>>1
  SELECT(loFormSet.lcTmpSpLn)
  DO CASE
    CASE &lcTmpLine..CCATGTYP = 'P' 
      lcPMethod = &lcTmpSpLn..CCURMETH
      lcPUnMeth = &lcTmpSpLn..CUNTMETH
      REPLACE NFCOST1   WITH &lcTmpLine..UNITCOST ;
              COSTSTAT WITH &lcTmpLine..CCOSTSTAT ;
              BPRICE   WITH NFCOST1 &lcPMethod NPRICERAT &lcPUnMeth NCURRUNIT  

	  
	 	

     CASE &lcTmpLine..CCATGTYP $ 'DM'
       lnPos = ASCAN(loFormSet.laCostItem,'*'+&lcTmpLine..MFGCODE+'*')
       IF lnPos > 0
         lnPos = ASUBSCRIPT(loFormSet.laCostItem,lnPos,1)
         IF &lcTmpLine..MFGCODE = '######'            && Check the case of more than one duty is entered
           llCheck = .F.
           lnFr = lnPos
           FOR lnI = lnFr TO ALEN(loFormSet.laCostItem,1)
             IF &lcTmpLine..MFGCODE = '######' .AND. &lcTmpLine..CBOMTYP = loFormSet.laCostItem[lnI,5]
                  lnPos = lnI
                  EXIT
              ENDIF 
           ENDFOR
         ENDIF
         
         SELECT(loFormSet.lcTmpSpLn)
         lcI = ALLTRIM(STR(lnPos-1))
        
         *B608667,1 MMT 08/27/2008 Fix bug of wrong unt cost in case of foreign currency [Start]
         IF &lcTmpLine..Ccurrcode <> oAriaApplication.BaseCurrency 
           lcExSign = gfGetExSin(@lcUntSin,&lcTmpLine..Ccurrcode)
           REPLACE COST&lcI.Cod WITH &lcTmpLine..MFGCODE  ;
                   COST&lcI     WITH &lcTmpLine..UNITCOST &lcExSign &lcTmpLine..nExRate  &lcUntSin &lcTmpLine..nCurrUnit * (&lcTmpLine..UNITQTY * &lcTmpSpLn..TOTQTY) ;
                   COSTSTAT&lcI WITH &lcTmpLine..CCOSTSTAT
           
         ELSE
  		 *B608667,1 MMT 08/27/2008 Fix bug of wrong unt cost in case of foreign currency [End]
         
         REPLACE COST&lcI.Cod WITH &lcTmpLine..MFGCODE  ;
                 COST&lcI     WITH &lcTmpLine..UNITCOST*(&lcTmpLine..UNITQTY * &lcTmpSpLn..TOTQTY) ;
                 COSTSTAT&lcI WITH &lcTmpLine..CCOSTSTAT

		*B608667,1 MMT 08/27/2008 Fix bug of wrong unt cost in case of foreign currency [Start]
		ENDIF 
		*B608667,1 MMT 08/27/2008 Fix bug of wrong unt cost in case of foreign currency [End]

         *[Start] Update royalty cost item for GPS00 from the Standard Royalty
         IF ASCAN(loFormSet.laEvntTrig,PADR('GPSROYLT',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
            loFormSet.mDoTrigger(PADR('GPSROYLT',10)) 
         ENDIF            
         *[End] 

          
            
         IF POSHD.CDUTYCUR <> oAriaApplication.BaseCurrency                     
           =gfSEEK('PP'+&lcTmpSpLn..PO,'POSHD')
           lnDutyUnit  = IIF(POSHD.nDCurUnit=0,1,POSHD.nDCurUnit)
           lnDutyRate  = IIF(POSHD.nDutyRat=0,1,POSHD.nDutyRat)
           lcExSign = gfGetExSin(@lcUntSin,POSHD.CDUTYCUR)
           REPLACE COST&lcI WITH COST&lcI &lcExSign lnDutyRate &lcUntSin lnDutyUnit  
         ENDIF
       ENDIF
    ENDCASE
  ENDIF        
ENDSCAN
    
    *- Update the laCostItem Array qty's to distrubute right
    FOR lnI = 1 TO ALEN(loFormSet.laCostItem,1)
      loFormSet.laCostItem[lnI,3] = 0 
    ENDFOR
     
    SELECT &lcTmpSpLn
    GO TOP
    SCAN FOR KEY=' '
      FOR lnI = 2 TO ALEN(loFormSet.laCostItem,1)
        lcI = ALLTRIM(STR(lnI-1))
        loFormSet.laCostItem[lnI,3] = loFormSet.laCostItem[lnI,3] + IIF(EMPTY(COST&lcI.COD),0,TOTQTY)
        
        IF '######' $ COST&lcI.COD .AND. BPRICE*TOTQTY>0
          PRIVATE lcDPMethod,lcDPUnMeth
          lcDPMethod = &lcTmpSpLn..CDUTUNTMTD  
          lcDPUnMeth = &lcTmpSpLn..CDUTCURMTD
          
          *use the same exchnge rate signs for Paurchase price to the duty for GPS[Start]
          IF ASCAN(loFormSet.laEvntTrig,PADR('DUTYSGNS',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
            loFormSet.mDoTrigger(PADR('DUTYSGNS',10)) 
          ENDIF    
          *[End] 

          
          
          lnDutable = (NFCOST1 &lcDPMethod NDUTYRAT &lcDPUnMeth NDCURUNIT)*TOTQTY 
          

          FOR lnJ = 2 TO ALEN(loFormSet.laCostItem,1)
            lcJ = ALLTRIM(STR(lnJ-1))
            IF COSTSTAT&lcJ = '1'
            
              *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[START]
              IF IIF(loFormSet.laCostItem[lnJ ,2] <> '*######*',loFormSet.laCostItem[lnJ ,6],.T.)
              *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[END]
                lnDutable = lnDutable + COST&lcJ
              *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[START]
              ENDIF 
              *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[End]
              
            ENDIF
          ENDFOR
          IF lnDutable>0
            REPLACE DUTYPERCNT WITH COST&lcI*100/lnDutable
          ENDIF
        ENDIF
      ENDFOR      
   ENDSCAN
    GO TOP

   =lfSumDist(loFormSet)
   loFormSet.AriaForm1.grdShpCost.afterrowcolchange()
   loFormSet.AriaForm1.grdShpCost.refresh
   
*:**************************************************************************
*:* Name        : lfSumDist
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Sum distributed lines
*:***************************************************************************
*E302483,1 
FUNCTION lfSumDist
PARAMETERS loFormSet
PRIVATE lnRecno,lcTemp,lnSlct,lnI,lcI,lnBPrice,lcPMethod,lcPUnMeth,lnSum





lnSlct = SELECT()
lcTmpSpLn = loFormSet.lcTmpSpLn 

SELECT &lcTmpSpLn
lnRecno = RECNO(lcTmpSpLn)
SET FILT TO 
GOTO TOP
SCAN FOR KEY = ' '
  *- Recalculate dutable items if any
  =lfRecalcDuty(loFormSet)
  
  lnSum = BPRICE*TOTQTY  
  FOR lnI = 2 TO ALEN(loFormSet.laCostItem,1)
    lcI = ALLTRIM(STR(lnI-1))
    
    *B608510,2 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[Start]
    *IF COST&lcI > 0
    IF COST&lcI >= 0
    *B608510,2 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[End]
       
      *[Start] Update royalty cost item for GPS00 from the Standard Royalty
      IF ASCAN(loFormSet.laEvntTrig,PADR('GPSROYLT',10)) <> 0 AND '*ROYALT*' $ loFormSet.laCostItem[lnI,2]
        LOOP  
      ENDIF            
      *[End]  
    
      lnSum = lnSum + COST&lcI      
      REPLACE UNTCST&lcI WITH IIF(TotQty>0,COST&lcI/TotQty,0)
    ENDIF
  ENDFOR
  
  REPLACE TOTCOST    WITH lnSum ;
          TOTSALEPRI WITH PRICEA*TOTQTY*(1 - NTRDISC/100)*(1 - STLMNTDISC/100)

  

  REPLACE TOTCOST    WITH TOTCOST + NROYALTY

  

  REPLACE TUCost WITH TOTCOST/IIF(TOTQTY>0,TOTQTY,1) ;
          Mrgn   WITH IIF(TOTSALEPRI>0,100 - (TOTCOST/(TOTSALEPRI))*100 ,0)
  
ENDSCAN

GO BOTTOM
BLANK
REPLACE KEY WITH CHR(255) 

GO TOP
lcTemp = 'Tot'+SUBSTR(lcTmpSpLn,4)
TOTAL ON '' TO (oAriaApplication.WorkDir+lcTemp)
SELE 0 
USE (oAriaApplication.WorkDir+lcTemp)
SCATTER MEMVAR
USE IN &lcTemp
ERASE (oAriaApplication.WorkDir+lcTemp+'.DBF')

SELECT &lcTmpSpLn
GO BOTTOM
FOR lnI = 2 TO ALEN(loFormSet.laCostItem,1)
  lcI = ALLTRIM(STR(lnI-1))
  IF M.COST&lcI > 0
    lcI = ALLTRIM(STR(lnI-1))
    REPLACE COST&lcI WITH M.COST&lcI 
  ENDIF
ENDFOR
REPLACE TOTCOST    WITH M.TOTCOST ;
        TOTQTY     WITH M.TOTQTY  ;
        TOTSALEPRI WITH M.TOTSALEPRI

REPLACE nTrDsVl WITH m.nTrDsVl ;
        nStlmVl WITH m.nStlmVl

REPLACE NROYALTY WITH M.NROYALTY


IF BETWEEN(lnRecno,1,RECCOUNT(lcTmpSpLn))
  GOTO (lnRecno) IN (lcTmpSpLn)
ENDIF

SELECT (lnSlct)
*-- end of lfSumDist.
*:**************************************************************************
*:* Name        : lfRecalcDuty
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : If this item is duatble then recalculate the duty if exists
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfRecalcDuty()
*:***************************************************************************
*E302483,1 
FUNCTION lfRecalcDuty
PARAMETERS loFormSet



PRIVATE lnD,lcD,lnI,lcK,lnDutable
lcTmpSpLn = loFormSet.lcTmpSpLn 

lnD = ASCAN(loFormSet.laCostItem,'*######*')
IF lnD > 0

  PRIVATE lcDPMethod,lcDPUnMeth
  lcDPMethod = &lcTmpSpLn..CDUTUNTMTD  
  lcDPUnMeth = &lcTmpSpLn..CDUTCURMTD
  
  *use the same exchnge rate signs for Paurchase price to the duty for GPS[Start]
  IF ASCAN(loFormSet.laEvntTrig,PADR('DUTYSGNS',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    loFormSet.mDoTrigger(PADR('DUTYSGNS',10)) 
  ENDIF    
  *[End] 
  
  lnDutable = (NFCOST1 &lcDPMethod NDUTYRAT &lcDPUnMeth NDCURUNIT)*TOTQTY 


  FOR lnI = 2 TO ALEN(loFormSet.laCostItem,1)
    lcK = ALLTRIM(STR(lnI-1))
    
    IF COSTSTAT&lcK = '1'
      IF IIF(loFormSet.laCostItem[lnI,2] <> '*######*',loFormSet.laCostItem[lnI,6],.T.)
        lnDutable = lnDutable + COST&lcK
      ENDIF 
    ENDIF
  ENDFOR 
  lnD = ASUBSCRIPT(loFormSet.laCostItem,lnD,1)
  lcD = ALLTRIM(STR(lnD-1))
  REPLACE Cost&lcD WITH DUTYPERCNT*lnDutable/100
ENDIF

*-- end of lfRecalcDuty.
*:**************************************************************************
*:* Name        : lfGrdShipCntSrc
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Add grid control source
*:***************************************************************************
*E302483,1 
FUNCTION lfGrdShipCntSrc



PARAMETERS loFormSet
  WITH loFormSet.AriaForm1.grdShpCost
  	.RecordSource = ''
  	.Columncount = 0
  	.READONLY = .T.
    .RecordSource = loFormSet.lcTmpSpLn
    .SETALL('READONLY',.T.,'COLUMN')
    
   	lcColNo = ALLTRIM(STR(.Columncount+1))
   	.Columncount = .Columncount + 1
  	.Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.Style'
   	.Column&lcColNo..Header1.caption = LANG_POACFRV_STYLE
   	.Column&lcColNo..Readonly = .T.
   	.Column&lcColNo..width = 148

   	lcColNo = ALLTRIM(STR(.Columncount+1))
   	.Columncount = .Columncount + 1
    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.Account'
    .Column&lcColNo..Header1.caption = LANG_POACFRV_CUSTOMER 
    .Column&lcColNo..Readonly = .T.
 
  	lcColNo = ALLTRIM(STR(.Columncount+1))
    .Columncount = .Columncount + 1
    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.PO'
    .Column&lcColNo..Header1.caption = LANG_POACFRV_PO
    .Column&lcColNo..Readonly = .T.

   	lcColNo = ALLTRIM(STR(.Columncount+1))
  	.Columncount = .Columncount + 1
    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.TOTQTY'
    .Column&lcColNo..Header1.caption =  LANG_POACFRV_QTYSHP      
    .Column&lcColNo..text1.InputMask = '99999'
    .Column&lcColNo..text1.Format = '99999'
    *.Column&lcColNo..Format = '99999'
    .Column&lcColNo..InputMask = '99999'
    .Column&lcColNo..Readonly = .T.
     
   	lcColNo = ALLTRIM(STR(.Columncount+1))
   	.Columncount = .Columncount + 1
    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.NFCOST1'
    .Column&lcColNo..Header1.caption = LANG_POACFRV_POPRICE
    IF loFormSet.ActiveMode $ 'AE'
      .Column&lcColNo..Readonly = .F.
      BINDEVENT(.Column&lcColNo..text1,"GotFocus",loFormSet,"lfED")
      BINDEVENT(.Column&lcColNo..text1,"LostFocus",loFormSet,"lfv1")
    ELSE
      .Column&lcColNo..Readonly = .T.  
    ENDIF   
    
     

   	lcColNo = ALLTRIM(STR(.Columncount+1))
   	.Columncount = .Columncount + 1
    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.curr'
    .Column&lcColNo..Header1.caption = LANG_POACFRV_CURR
    .Column&lcColNo..Readonly = .T.
     
    lcColNo = ALLTRIM(STR(.Columncount+1))
  	.Columncount = .Columncount + 1
    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.NPRICERAT'
    .Column&lcColNo..Header1.caption = LANG_POACFRV_EXRATE

    IF loFormSet.ActiveMode $ 'AE'
      .Column&lcColNo..Readonly = .F.
      BINDEVENT(.Column&lcColNo..text1,"GotFocus",loFormSet,"lfED")
      BINDEVENT(.Column&lcColNo..text1,"LostFocus",loFormSet,"lfv2")
    ELSE
      .Column&lcColNo..Readonly = .T.  
    ENDIF   

              
   	lcColNo = ALLTRIM(STR(.Columncount+1))
   	.Columncount = .Columncount + 1
    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.BPrice'
    .Column&lcColNo..Header1.caption = LANG_POACFRV_BPRICE         
    .Column&lcColNo..Readonly = .T.
    
    
  	lcColNo = ALLTRIM(STR(.Columncount+1))
  	.Columncount = .Columncount + 1
    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.PRICEA'
    .Column&lcColNo..Header1.caption = LANG_POACFRV_SPRICE
    IF loFormSet.ActiveMode $ 'AE'
      .Column&lcColNo..Readonly = .F.
      BINDEVENT(.Column&lcColNo..text1,"GotFocus",loFormSet,"lfED")
      BINDEVENT(.Column&lcColNo..text1,"LostFocus",loFormSet,"lfv3")
    ELSE
      .Column&lcColNo..Readonly = .T.  
    ENDIF   

    
    
    
     
  	lcColNo = ALLTRIM(STR(.Columncount+1))
  	.Columncount = .Columncount + 1
    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.ROYLPERCNT'
    .Column&lcColNo..Header1.caption = LANG_POACFRV_ROYALPERC
    IF loFormSet.ActiveMode $ 'AE'
      .Column&lcColNo..Readonly = .F.
      BINDEVENT(.Column&lcColNo..text1,"GotFocus",loFormSet,"lfED2")
      BINDEVENT(.Column&lcColNo..text1,"LostFocus",loFormSet,"lfv4")
    ELSE
      .Column&lcColNo..Readonly = .T.  
    ENDIF   

     
          
  	lcColNo = ALLTRIM(STR(.Columncount+1))
  	.Columncount = .Columncount + 1
    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.NROYALTY'
    .Column&lcColNo..Header1.caption = LANG_POACFRV_ROYALATY
    .Column&lcColNo..Readonly = .T.
    
     
   	lcColNo = ALLTRIM(STR(.Columncount+1))
    .Columncount = .Columncount + 1
    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.NTRDISC'
    .Column&lcColNo..Header1.caption = LANG_POACFRV_TRDDISC
    IF loFormSet.ActiveMode $ 'AE'
      .Column&lcColNo..Readonly = .F.
      BINDEVENT(.Column&lcColNo..text1,"GotFocus",loFormSet,"lfED")
      BINDEVENT(.Column&lcColNo..text1,"LostFocus",loFormSet,"lfvDsc1")
    ELSE
      .Column&lcColNo..Readonly = .T.  
    ENDIF   

    
     
   	lcColNo = ALLTRIM(STR(.Columncount+1))
    .Columncount = .Columncount + 1
    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.nTrDsVl'
    .Column&lcColNo..Header1.caption = LANG_POACFRV_TRADE
    .Column&lcColNo..Readonly = .T.
          
   	lcColNo = ALLTRIM(STR(.Columncount+1))
  	.Columncount = .Columncount + 1
    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.STLMNTDISC'
    .Column&lcColNo..Header1.caption = LANG_POACFRV_STLDISC
    IF loFormSet.ActiveMode $ 'AE'
      .Column&lcColNo..Readonly = .F.
      BINDEVENT(.Column&lcColNo..text1,"GotFocus",loFormSet,"lfED")
      BINDEVENT(.Column&lcColNo..text1,"LostFocus",loFormSet,"lfvDsc2")
    ELSE
      .Column&lcColNo..Readonly = .T.  
    ENDIF   

    
    
     
    lcColNo = ALLTRIM(STR(.Columncount+1))
    .Columncount = .Columncount + 1
    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.nStlmVl'
    .Column&lcColNo..Header1.caption = LANG_POACFRV_STLAMNT
    .Column&lcColNo..Readonly = .T.
     
    lnCostItem = loFormSet.AriaForm1.cboCostItems.ListIndex
    lnLn = ALEN(loFormSet.laCostItem,1)
   	lnFr = IIF( lnCostItem=1 , 2 , lnCostItem)
    lnTo = IIF( lnCostItem=1 , lnLn, lnCostItem)
     
    FOR lnI = lnFr TO lnTo
      lcI = ALLTRIM(STR(lnI-1))
      lnDStrt = AT('-',loFormSet.laCostItem[lnI,1])+1
      lcDesc = ALLTRIM(SUBSTR(loFormSet.laCostItem[lnI,1],lnDStrt))
 	    DO CASE
       
	      CASE '######' $ loFormSet.laCostItem[lnI,2]
	        lcPrc = ALLTRIM(PADR(lcDesc,10)) + '%'
          lcColNo = ALLTRIM(STR(.Columncount+1))
  	 	    .Columncount = .Columncount + 1
    	    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.DUTYPERCNT'
          .Column&lcColNo..Header1.caption = lcPrc    
          IF loFormSet.ActiveMode $ 'AE'
            .Column&lcColNo..Readonly = .F.
            BINDEVENT(.Column&lcColNo..text1,"GotFocus",loFormSet,"lfEd3")
            BINDEVENT(.Column&lcColNo..text1,"LostFocus",loFormSet,"lfvDuty")
          ELSE
            .Column&lcColNo..Readonly = .T.  
          ENDIF   

	
  
          lcColNo = ALLTRIM(STR(.Columncount+1))
          .Columncount = .Columncount + 1
          .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.COST'+lcI
          .Column&lcColNo..Header1.caption = lcDesc 
          .Column&lcColNo..Readonly = .T.
           
          lcColNo = ALLTRIM(STR(.Columncount+1))
          .Columncount = .Columncount + 1
          .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.NDUTYRAT'
          .Column&lcColNo..Header1.caption = LANG_POACFRV_DUTYEXRATE
          .Column&lcColNo..text1.fORMAT = '999.99'
          .Column&lcColNo..InputMask = '999.99'
          IF loFormSet.ActiveMode $ 'AE'
            .Column&lcColNo..Readonly = .F.
            BINDEVENT(.Column&lcColNo..text1,"GotFocus",loFormSet,"lfEd")
            BINDEVENT(.Column&lcColNo..text1,"LostFocus",loFormSet,"lfv5")
          ELSE
            .Column&lcColNo..Readonly = .T.  
          ENDIF   


           
        OTHERWISE
          *[Start] loop if this is GPS and this cost is the Royalty 
          IF !(ASCAN(loFormSet.laEvntTrig, PADR('GPSROYLT',10)) <> 0 .AND. '*ROYALT*' $ loFormSet.laCostItem[lnI,2])
    		    lcColNo = ALLTRIM(STR(.Columncount+1))
    		    .Columncount = .Columncount + 1
  	  	    .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.COST'+lcI
    	      .Column&lcColNo..Header1.caption = lcDesc 
    	      IF loFormSet.ActiveMode $ 'AE'
       	      .Column&lcColNo..Readonly = .F.
	            BINDEVENT(.Column&lcColNo..text1,"GotFocus",loFormSet,"lfwCs")
    	        BINDEVENT(.Column&lcColNo..text1,"LostFocus",loFormSet,"lfCs")
    	      ELSE
      	     .Column&lcColNo..Readonly = .T.   
    	     ENDIF    
         ENDIF   
	   ENDCASE 	
	   
	   *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[START]
	   IF !(ASCAN(loFormSet.laEvntTrig, PADR('GPSROYLT',10)) <> 0 .AND. '*ROYALT*' $ loFormSet.laCostItem[lnI,2])
	   *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[End]
	   
	 	lcColNo = ALLTRIM(STR(.Columncount+1))
 		.Columncount = .Columncount + 1
 		.Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.UNTCST'+lcI
		.Column&lcColNo..Header1.caption = LANG_POACFRV_UNITCOST
 		.Column&lcColNo..Readonly = .T.
 		
       *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[START]
       ENDIF 
       *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[End]
       		
  ENDFOR 
 	 lcColNo = ALLTRIM(STR(.Columncount+1))
 	 .Columncount = .Columncount + 1
 	 .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.TOTCOST'
	 .Column&lcColNo..Header1.caption = LANG_POACFRV_TOTALCOST
	 .Column&lcColNo..Readonly = .T.

   	 lcColNo = ALLTRIM(STR(.Columncount+1))
 	 .Columncount = .Columncount + 1
 	 .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.TUCost'
	 .Column&lcColNo..Header1.caption = LANG_POACFRV_TOTUCOST
	 .Column&lcColNo..Readonly = .T.
 	 
 	 
 	IF  loFormSet.llShpCsUsr .OR. loFormSet.llShowMrg   
    lcColNo = ALLTRIM(STR(.Columncount+1))
  	.Columncount = .Columncount + 1
 	  .Column&lcColNo..ControlSource = loFormSet.lcTmpSpLn + '.Mrgn'
    .Column&lcColNo..text1.fORMAT = '999.99'
    .Column&lcColNo..InputMask = '999.99'
 	  .Column&lcColNo..Header1.caption = LANG_POACFRV_MRGIN
 	  .Column&lcColNo..Readonly = .T.
 	ENDIF 
ENDWITH 



*:**************************************************************************
*:* Name        : lfvCostItm
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Validate cost item selected 
*:***************************************************************************
*E302483,1 
FUNCTION lfvCostItm
PARAMETERS lcCSValue ,loFormSet

 
lcTmpSpLn = loFormSet.lcTmpSpLn

SELECT &lcTmpSpLn

SET FILTER TO 

loFormset.AriaForm1.txtPercent.Value =0
loFormset.AriaForm1.txtAmnt.Value=0

loFormSet.AriaForm1.txtAmnt.Enabled = .F.
loFormSet.AriaForm1.txtPercent.Enabled = .F.
loFormSet.AriaForm1.cmdDistribute.Enabled = .F.
loFormSet.AriaForm1.chkFreight.Enabled = .F.
lnCostItem = loFormSet.AriaForm1.cboCostItems.ListIndex



IF lnCostItem = 1
  loFormSet.AriaForm1.txtAmnt.Enabled = .F.
  loFormSet.AriaForm1.txtPercent.Enabled = .F.
  loFormSet.AriaForm1.cmdDistribute.Enabled = .F.
  loFormSet.AriaForm1.chkFreight.Enabled = .F.
  loFormSet.AriaForm1.chkFreight.Value = .F.
ELSE
  IF loFormSet.ActiveMode = 'A' .OR. loFormSet.ActiveMode = 'E'
    loFormSet.AriaForm1.txtAmnt.Enabled = .T.
    loFormSet.AriaForm1.txtPercent.Enabled = .T.
    loFormSet.AriaForm1.cmdDistribute.Enabled = .T.
    loFormSet.AriaForm1.chkFreight.Enabled = .T.
    IF '######' $ loFormSet.laCostItem[lnCostItem,2]
       loFormSet.AriaForm1.txtAmnt.Enabled = .F.
       loFormSet.AriaForm1.chkFreight.Enabled = .F.
    ENDIF
  ENDIF
  loFormSet.AriaForm1.chkFreight.Value = loFormSet.laCostItem[lnCostItem,6]
  lcI = ALLTRIM(STR(lnCostItem-1))
ENDIF  

GO TOP IN &lcTmpSpLn


lfGrdShipCntSrc(loFormSet)
loFormSet.AriaForm1.grdShpCost.afterrowcolchange()
loFormSet.AriaForm1.grdShpCost.refresh
*-- end of lfvCostItm.
*:**************************************************************************
*:* Name        : lfvDist
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Distribute entered amount
*:***************************************************************************
*E302483,1 
FUNCTION lfvDist
PARAMETERS loFormSet


PRIVATE lnRecno,lnQtySum,lnUnitCost,lnI,lcI,lcK,lcPrcntOf,lnDutable
lcTmpSpLn = loFormSet.lcTmpSpLn

lnRecno = RECNO(lcTmpSpLn)
lnCostItem = loFormSet.AriaForm1.cboCostItems.ListIndex
lnAmount = loFormset.AriaForm1.txtAmnt.Value
lnPercnt = loFormset.AriaForm1.txtPercent.Value 
lcI = ALLTRIM(STR(lnCostItem-1))

SELECT &lcTmpSpLn
DO CASE
CASE  lnAmount > 0
  IF loFormSet.laCostItem[lnCostItem,3] > 0
    lnUnitCost = lnAmount/loFormSet.laCostItem[lnCostItem,3]  
    
    SCAN FOR KEY = ' ' .AND. !EMPTY(COST&lcI.COD)
      REPLACE  Cost&lcI WITH lnUnitCost*TOTQTY
    ENDSCAN
  
    *- Calcualte total cost per line
    =lfSumDist(loFormSet)
  ELSE
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_POACFRV_NO_AMNT_DIST)
  ENDIF



CASE  lnPercnt > 0
  
  llDist = .F.
  DO CASE
    CASE '######' $ loFormSet.laCostItem[lnCostItem,2] 
      SCAN FOR KEY = ' ' .AND. !EMPTY(COST&lcI.COD)
        PRIVATE lcDPMethod,lcDPUnMeth
        lcDPMethod = &lcTmpSpLn..CDUTUNTMTD  
        lcDPUnMeth = &lcTmpSpLn..CDUTCURMTD
        
        *use the same exchnge rate signs for Paurchase price to the duty for GPS[Start]
        IF ASCAN(loFormSet.laEvntTrig,PADR('DUTYSGNS',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
          loFormSet.mDoTrigger(PADR('DUTYSGNS',10)) 
        ENDIF    
        *[End] 

        
        
        lnDutable = (NFCOST1 &lcDPMethod NDUTYRAT &lcDPUnMeth NDCURUNIT)*TOTQTY 

        FOR lnI = 2 TO ALEN(loFormSet.laCostItem,1)
          lcK = ALLTRIM(STR(lnI-1))
          IF COSTSTAT&lcK = '1'
            lnDutable = lnDutable + COST&lcK
          ENDIF
        ENDFOR 
        REPLACE DUTYPERCNT WITH lnPercnt ;
                Cost&lcI   WITH lnPercnt*lnDutable/100
        llDist = .T.
      ENDSCAN  
      *- Calcualte total cost per line
       =lfSumDist(loFormSet)
    OTHERWISE
      lcPrcntOf = ''
      IF loFormSet.laCostItem[lnCostItem,3]>0 .AND. lfMovrSlct(@lcPrcntOf,loFormSet) 
        SELECT &lcTmpSpLn
        GO TOP
        
        SCAN FOR KEY = ' ' .AND. !EMPTY(COST&lcI.COD)
          lnPercntOf = EVAL(lcPrcntOf)
          REPLACE Cost&lcI.PER WITH lnPercnt ;      
                  Cost&lcI     WITH (lnPercnt/100)*lnPercntOf ;
                  PERCNTOF&lcI WITH lcPrcntOf
          llDist = .T.
          
          *- If this item is duatble then recalculate the duty if exists
          =lfRecalcDuty(loFormSet)
          
        ENDSCAN
        *- Calcualte total cost per line
         =lfSumDist(loFormSet)
        
        loFormset.AriaForm1.txtPercent.Value =0
		loFormset.AriaForm1.txtAmnt.Value=0
	    loFormSet.AriaForm1.txtAmnt.Enabled = .T.
	    loFormSet.AriaForm1.txtPercent.Enabled = .T.
      ENDIF
  
    ENDCASE
  IF !llDist
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_POACFRV_NO_AMNT_DIST)
  ENDIF
  
OTHERWISE
  IF loFormset.laCostItem[lnCostItem,3] > 0  .AND. gfModalGen('INM00000B00006',.F.,.F.,.F.,LANG_POACFRV_REMOVE_DIST ) = 1
    REPLACE  Cost&lcI WITH 0 ALL
  ENDIF
ENDCASE

loFormset.AriaForm1.txtPercent.Value =0
loFormset.AriaForm1.txtAmnt.Value=0

loFormSet.AriaForm1.cmdDistribute.Enabled = .F.

IF BETWEEN(lnRecno,1,RECCOUNT(lcTmpSpLn))
  GOTO (lnRecno) IN (lcTmpSpLn)
ENDIF
*-- end of lfvDist.
*:**************************************************************************
*:* Name        : lfvAprvShp
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Approve button valid function
*:***************************************************************************
*E302483,1 
FUNCTION lfvAprvShp
PARAMETERS loFormSet
PRIVATE lnResp,lnSlct



lnSlct = SELECT()
IF USED('SHPMTH') AND gfSeek('PP'+PADR(loFormSet.AriaForm1.kbPO.KeyTextBox.Value,6),'SHPMTH') AND SHPMTH.Status = 'H'
  ** Message 34212 : 
  **   No further costs can be applied to this shipment following approval.  
  **   Are you sure you wish to proceed with approval of this shipment cost sheet?
  lnResp = gfModalGen('QRM34214B00006','DIALOG')
  IF lnResp = 1
    loFormSet.AriaForm1.cmdApprove.Enabled = .F.
    loFormSet.llApprove = .T.
    loFormSet.SaveFiles(.F.)
    RETURN 
  ELSE
    RETURN 
  ENDIF
ENDIF

SELECT (lnSlct)

*:**************************************************************************
*:* Name        : lfAfterRowColChange
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : After Row Col. chanaged of grid
*:***************************************************************************
*E302483,1 
FUNCTION lfAfterRowColChange
PARAMETERS loFormSet



lcTmpSpLn = loFormSet.lcTmpSpLn

IF !USED('SCALE')
  =gfOpenTable('SCALE','SCALE','SH')
ENDIF 
IF !USED('STYLE_A')
  =gfOpenTable('STYLE','STYLE','SH','STYLE_A')
ENDIF 

=gfSeek(&lcTmpSpLn..STYLE,'STYLE_A')
=gfSeek('S'+&lcTmpSpLn..SCALE,'SCale')

WITH loFormSet.AriaForm1
  .txtStyDesc.VAlue = STYLE_A.DESC
  .txtClrDesc.VAlue = gfCodDes(SUBSTR(&lcTmpSpLn..STYLE,loFormSet.lnClrPos,loFormSet.lnClrLen),'COLOR')
  .txtScaleDesc.VAlue = SCALE.CSCL_DESc
  WITH .sbQuantites
    .scale = &lcTmpSpLn..SCALE
    .txtQty1.Value = &lcTmpSpLn..QTY1
    .txtQty2.Value = &lcTmpSpLn..QTY2
    .txtQty3.Value = &lcTmpSpLn..QTY3
    .txtQty4.Value = &lcTmpSpLn..QTY4
    .txtQty5.Value = &lcTmpSpLn..QTY5
    .txtQty6.Value = &lcTmpSpLn..QTY6
    .txtQty7.Value = &lcTmpSpLn..QTY7
    .txtQty8.Value = &lcTmpSpLn..QTY8
    .Refresh
  ENDWITH 
ENDWITH 

*:**************************************************************************
*:* Name        : lfGtClrDat
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Get Color data and scale width
*:***************************************************************************
*E302483,1 
FUNCTION lfGtClrDat
PARAMETERS loFormSet
DECLARE laItemSeg[1]
PRIVATE lnCount 
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    loFormSet.lnClrLen = LEN(laItemSeg[lnCount,3])
    loFormSet.lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])
    EXIT
  ENDIF
ENDFOR

*:**************************************************************************
*:* Name        : lfED
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : When function of Cost Entered
*:***************************************************************************
*E302483,1 
FUNCTION lfED
PARAMETERS loFormSet
lcTmpSpLn = loFormSet.lcTmpSpLn
IF &lcTmpSpLn..KEY = CHR(255)
  KEYBOARD '{TAB}' 
  RETURN .F.	
ENDIF 
 
IF (loFormSet.ActiveMode = 'A'  .OR. loFormSet.ActiveMode = 'E')
  RETURN .T.
ELSE
  KEYBOARD '{TAB}' 
  RETURN .F.  
ENDIF 


*:**************************************************************************
*:* Name        : lfv1
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Validate Cost Entered
*:***************************************************************************
*E302483,1 
FUNCTION lfv1
PARAMETERS loFormSet,llFrmCod

*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[Start]
IF !(loFormSet.ActiveMode $ 'EA')
  RETURN 
ENDIF 
*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[End]


PRIVATE lcPMethod,lcPUnMeth,lnK,lcK,lcPrcntOf
lcTmpSpLn = loFormSet.lcTmpSpLn
IF &lcTmpSpLn..KEY =CHR(255)
  BLANK FIELD NFCOST1 IN (lcTmpSpLn)
  RETURN
ENDIF


lcPMethod = &lcTmpSpLn..CCURMETH
lcPUnMeth = &lcTmpSpLn..CUNTMETH
REPLACE BPRICE WITH NFCOST1 &lcPMethod NPRICERAT &lcPUnMeth NCURRUNIT  

*- Recalculate cost items that are percentage of other cost items
FOR lnK = 2 TO ALEN(loFormSet.laCostItem,1)
  lcK = ALLTRIM(STR(lnK-1))
  lcPrcntOf = PERCNTOF&lcK
  IF !EMPTY(lcPrcntOf)
    lnPercntOf = EVAL(lcPrcntOf)
    REPLACE Cost&lcK WITH (Cost&lcK.PER/100)*lnPercntOf
  ENDIF
ENDFOR

=lfRecalcDuty(loFormSet)

=IIF(llFrmCod,'',lfSumDist(loFormSet))
RETURN .T.


*:**************************************************************************
*:* Name        : lfv2
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Validate price ex. rate
*:***************************************************************************
*E302483,1 
FUNCTION lfv2
PARAMETERS loFormSet

*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[Start]
IF !(loFormSet.ActiveMode $ 'EA')
  RETURN 
ENDIF 
*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[End]



lcTmpSpLn = loFormSet.lcTmpSpLn

PRIVATE lcPMethod,lcPUnMeth,lnRecno,lnResp,lcFor
SELECT &lcTmpSpLn

IF NPRICERAT<=0 
  REPLACE NPRICERAT WITH 1
ENDIF
IF &lcTmpSpLn..KEY = CHR(255)
  BLANK FIELDS NPRICERAT
  RETURN
ENDIF

IF &lcTmpSpLn..CURR  = oAriaApplication.BaseCurrency                     
  REPLACE NPRICERAT WITH 1
  RETURN
ENDIF 

SELECT &lcTmpSpLn

IF loFormSet.AriaForm1.chkUpdateAll.Value 
  lnRecno = RECNO()
  lnNewRate = NPRICERAT
  lcCurr = &lcTmpSpLn..CURR  
  GO TOP
  SCAN FOR KEY = ' ' .AND. &lcTmpSpLn..CURR = lcCurr
    lcPMethod = &lcTmpSpLn..CCURMETH
    lcPUnMeth = &lcTmpSpLn..CUNTMETH
    REPLACE NPRICERAT WITH lnNewRate ;
            BPRICE WITH NFCOST1 &lcPMethod NPRICERAT &lcPUnMeth NCURRUNIT
    =lfv1(loFormSet,.T.)
  ENDSCAN
  IF BETWEEN(lnRecno,1,RECCOUNT(lcTmpSpLn))
    GOTO (lnRecno)
  ENDIF
ELSE
  lcPMethod = &lcTmpSpLn..CCURMETH
  lcPUnMeth = &lcTmpSpLn..CUNTMETH
  REPLACE BPRICE WITH NFCOST1 &lcPMethod NPRICERAT &lcPUnMeth NCURRUNIT
  =lfv1(loFormSet,.T.)
ENDIF  
 
=lfSumDist(loFormSet)
*:**************************************************************************
*:* Name        : lfv3
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Validate price 
*:***************************************************************************
*E302483,1 
FUNCTION lfv3
PARAMETERS loFormSet,llNoCalc

*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[Start]
IF !(loFormSet.ActiveMode $ 'EA')
  RETURN 
ENDIF 
*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[End]


lcTmpSpLn = loFormSet.lcTmpSpLn
PRIVATE lnPos,lcI
IF &lcTmpSpLn..KEY = CHR(255)
  BLANK FIELDS PRICEA
  RETURN
ENDIF

SELECT &lcTmpSpLn
REPLACE NROYALTY WITH PRICEA*TOTQTY*(1 - NTRDISC/100)*ROYLPERCNT/100
REPLACE nTrDsVl WITH PRICEA*TOTQTY*NTRDISC/100 ;
        nStlmVl WITH PRICEA*TOTQTY*(1-NTRDISC/100)*STLMNTDISC/100 


*[Start] Update royalty cost item for GPS00 from the Standard Royalty
IF ASCAN(loFormSet.laEvntTrig,PADR('GPSROYLT',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  loFormSet.mDoTrigger(PADR('GPSROYLT',10)) 
ENDIF           
*[End]

IF  loFormSet.AriaForm1.chkUpdateAll.Value .AND. !llNoCalc
  lnRecno = RECNO()
  lnNewPriA = PRICEA
  GO TOP
  SCAN FOR KEY = ' '
    REPLACE PRICEA   WITH lnNewPriA ;
            NROYALTY WITH PRICEA*TOTQTY*(1 - NTRDISC/100)*ROYLPERCNT/100 ;
            nTrDsVl  WITH PRICEA*TOTQTY*NTRDISC/100                       ;
            nStlmVl  WITH PRICEA*TOTQTY*(1-NTRDISC/100)*STLMNTDISC/100 
 
    *[Start] Update royalty cost item for GPS00 from the Standard Royalty
    IF  ASCAN(loFormSet.laEvntTrig,PADR('GPSROYLT',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
       loFormSet.mDoTrigger(PADR('GPSROYLT',10)) 
    ENDIF           
    *[End  ]

  ENDSCAN
  IF BETWEEN(lnRecno,1,RECCOUNT(lcTmpSpLn))
    GOTO (lnRecno)
  ENDIF
ENDIF
 
=IIF(llNoCalc , .T. ,lfSumDist(loFormSet))
*:**************************************************************************
*:* Name        : lfEd2
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : When func. of Royalty percent
*:***************************************************************************
*E302483,1 
FUNCTION lfEd2
PARAMETERS loFormSet
lcTmpSpLn = loFormSet.lcTmpSpLn
IF lfEd(loFormSet) .AND. &lcTmpSpLn..PRICEA * &lcTmpSpLn..TOTQTY > 0
  RETURN .T.
ELSE
  KEYBOARD '{TAB}' 
  RETURN .F.
ENDIF 

*:**************************************************************************
*:* Name        : lfv4
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : VAlidate Royalty percent
*:***************************************************************************
*E302483,1 
FUNCTION lfv4
PARAMETERS loFormSet

*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[Start]
IF !(loFormSet.ActiveMode $ 'EA')
  RETURN 
ENDIF 
*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[End]


lcTmpSpLn = loFormSet.lcTmpSpLn

REPLACE NROYALTY WITH &lcTmpSpLn..PRICEA * &lcTmpSpLn..TOTQTY*(1 - &lcTmpSpLn..NTRDISC/100)* &lcTmpSpLn..ROYLPERCNT/100
*[Start] Update royalty cost item for GPS00 from the Standard Royalty
IF ASCAN(loFormSet.laEvntTrig,PADR('GPSROYLT',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  loFormSet.mDoTrigger(PADR('GPSROYLT',10)) 
ENDIF            
*[End] 

IF loFormSet.AriaForm1.chkUpdateAll.Value 
  lnRecno = RECNO()
  lnNewVal = &lcTmpSpLn..ROYLPERCNT
  GO TOP
  SCAN FOR KEY = ' '
    REPLACE ROYLPERCNT WITH lnNewVal 
    REPLACE NROYALTY WITH &lcTmpSpLn..PRICEA * &lcTmpSpLn..TOTQTY*(1 - &lcTmpSpLn..NTRDISC/100)* &lcTmpSpLn..ROYLPERCNT/100    
   
    *[Start] Update royalty cost item for GPS00 from the Standard Royalty
    IF ASCAN(loFormSet.laEvntTrig,PADR('GPSROYLT',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
      loFormSet.mDoTrigger(PADR('GPSROYLT',10)) 
    ENDIF            
    *[End] 
    
  ENDSCAN
  IF BETWEEN(lnRecno,1,RECCOUNT(lcTmpSpLn))
    GOTO (lnRecno)
  ENDIF
ENDIF  
=lfSumDist(loFormSet)

*:**************************************************************************
*:* Name        : lfvDsc1
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : VAlidate function of trade disc.
*:***************************************************************************
*E302483,1 
FUNCTION  lfvDsc1
PARAMETERS loFormSet


*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[Start]
IF !(loFormSet.ActiveMode $ 'EA')
  RETURN 
ENDIF 
*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[End]


lcTmpSpLn = loFormSet.lcTmpSpLn
PRIVATE lnRecno,lnNewVal,lcFld
IF &lcTmpSpLn..KEY = CHR(255)
  BLANK FIELDS NTRDISC IN (lcTmpSpLn)
  RETURN 
ENDIF 
SELECT &lcTmpSpLn
IF NTRDISC >= 100
  REPLACE NTRDISC WITH 99
ENDIF
REPLACE nTrDsVl WITH PRICEA*TOTQTY*NTRDISC/100 
IF NTRDISC < 0
  REPLACE  NTRDISC WITH 0
ENDIF 

=lfv3(loFormSet,.T.)

SELECT &lcTmpSpLn
IF loFormSet.AriaForm1.chkUpdateAll.Value 
  lnRecno = RECNO()
  lnNewVal = NTRDISC
  GO TOP
  SCAN FOR KEY = ' '
    REPLACE NTRDISC WITH lnNewVal 
    REPLACE nTrDsVl WITH PRICEA*TOTQTY*NTRDISC/100 
	=lfv3(loFormSet,.T.)
  ENDSCAN
  IF BETWEEN(lnRecno,1,RECCOUNT(lcTmpSpLn))
    GOTO (lnRecno)
  ENDIF
ENDIF  

*- Update summation 
lfSumDist(loFormSet)

*:**************************************************************************
*:* Name        : lfvDsc2
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : VAlidate function of stl. disc.
*:***************************************************************************
*E302483,1 
FUNCTION  lfvDsc2
PARAMETERS loFormSet


*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[Start]
IF !(loFormSet.ActiveMode $ 'EA')
  RETURN 
ENDIF 
*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[End]


lcTmpSpLn = loFormSet.lcTmpSpLn
PRIVATE lnRecno,lnNewVal,lcFld
IF &lcTmpSpLn..KEY = CHR(255)
  BLANK FIELDS STLMNTDISC IN (lcTmpSpLn)
  RETURN 
ENDIF 
SELECT &lcTmpSpLn

REPLACE nStlmVl WITH PRICEA*TOTQTY*(1-NTRDISC/100)*STLMNTDISC/100 

IF STLMNTDISC < 0
  REPLACE  STLMNTDISC WITH 0
ENDIF 

SELECT &lcTmpSpLn
IF loFormSet.AriaForm1.chkUpdateAll.Value 
  lnRecno = RECNO()
  lnNewVal = STLMNTDISC 
  GO TOP
  SCAN FOR KEY = ' '
    REPLACE STLMNTDISC  WITH lnNewVal 
    REPLACE nStlmVl  WITH PRICEA*TOTQTY*(1-NTRDISC/100)*STLMNTDISC/100 
	=lfv3(loFormSet,.T.)
  ENDSCAN
  IF BETWEEN(lnRecno,1,RECCOUNT(lcTmpSpLn))
    GOTO (lnRecno)
  ENDIF
ENDIF  

*- Update summation 
lfSumDist(loFormSet)

*:**************************************************************************
*:* Name        : lfEd3
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : the When Function of duty percent
*:***************************************************************************
*E302483,1 
FUNCTION lfEd3
PARAMETERS loFormSet


lcTmpSpLn = loFormSet.lcTmpSpLn
PRIVATE lnD
lnD = ASCAN(loFormSet.laCostItem,'*######*')
IF lnD > 0
  lnD = ASUBSCRIPT(loFormSet.laCostItem,lnD,1)
  lcD = ALLTRIM(STR(lnD-1))
  RETURN lfEd(loFormSet) .AND. !EMPTY(COST&lcD.Cod)  
ELSE
  RETURN .F.
ENDIF

*:**************************************************************************
*:* Name        : lfvDuty
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Validate duty 
*:***************************************************************************
*E302483,1 
FUNCTION lfvDuty
PARAMETERS loFormSet,lnCurrCol,llDutyRat


*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[Start]
IF !(loFormSet.ActiveMode $ 'EA')
  RETURN 
ENDIF 
*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[End]


lcTmpSpLn = loFormSet.lcTmpSpLn

IF &lcTmpSpLn..KEY =CHR(255)
  RETURN
ENDIF

lcCurrValue = ALLTRIM(STR(lnCurrCol))
lcCurrFld = loFormSet.AriaForm1.grdShpCost.Column&lcCurrValue..ControlSource

PRIVATE lnI,lcI,lnDuAmnt,lnBPrice,lcPMethod,lcPUnMeth
SELECT &lcTmpSpLn

PRIVATE lcDPMethod,lcDPUnMeth
lcDPMethod = &lcTmpSpLn..CDUTUNTMTD  
lcDPUnMeth = &lcTmpSpLn..CDUTCURMTD

*use the same exchnge rate signs for Paurchase price to the duty for GPS[Start]
IF ASCAN(loFormSet.laEvntTrig,PADR('DUTYSGNS',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  loFormSet.mDoTrigger(PADR('DUTYSGNS',10)) 
ENDIF    
*[End  ] 


lnDuAmnt = (&lcTmpSpLn..NFCOST1 &lcDPMethod &lcTmpSpLn..NDUTYRAT &lcDPUnMeth &lcTmpSpLn..NDCURUNIT)*&lcTmpSpLn..TOTQTY 

FOR lnI = 2 TO ALEN(loFormSet.laCostItem,1)
  lcI = ALLTRIM(STR(lnI-1))
  IF &lcTmpSpLn..COSTSTAT&lcI = '1'
    lnDuAmnt = lnDuAmnt + COST&lcI
  ENDIF
ENDFOR

lcPMethod = &lcTmpSpLn..CCURMETH
lcPUnMeth = &lcTmpSpLn..CUNTMETH
lnBPrice  = &lcTmpSpLn..NFCOST1 &lcPMethod &lcTmpSpLn..NPRICERAT &lcPUnMeth &lcTmpSpLn..NCURRUNIT 

REPLACE &lcCurrFld WITH DUTYPERCNT*lnDuAmnt/100 IN (lcTmpSpLn)

SELECT &lcTmpSpLn
IF loFormSet.AriaForm1.chkUpdateAll.Value .AND. !llDutyRat
  lnRecno = RECNO()
  lnNewVal = &lcTmpSpLn..DUTYPERCNT 
  GO TOP
  SCAN FOR KEY = ' '

    lcDPMethod = &lcTmpSpLn..CDUTUNTMTD  
    lcDPUnMeth = &lcTmpSpLn..CDUTCURMTD
    
    *use the same exchnge rate signs for Paurchase price to the duty for GPS[Start]
    IF ASCAN(loFormSet.laEvntTrig,PADR('DUTYSGNS',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
      loFormSet.mDoTrigger(PADR('DUTYSGNS',10)) 
    ENDIF    
    *[End  ] 

    lnDuAmnt = (NFCOST1 &lcDPMethod NDUTYRAT &lcDPUnMeth NDCURUNIT)*TOTQTY 
    
    FOR lnI = 2 TO ALEN(loFormSet.laCostItem,1)
      lcI = ALLTRIM(STR(lnI-1))
      IF COSTSTAT&lcI = '1'
        lnDuAmnt = lnDuAmnt + COST&lcI
      ENDIF
    ENDFOR
  
    REPLACE DUTYPERCNT WITH lnNewVal ;
            &lcCurrFld WITH DUTYPERCNT*lnDuAmnt/100
  ENDSCAN
  IF BETWEEN(lnRecno,1,RECCOUNT(lcTmpSpLn))
    GOTO (lnRecno)
  ENDIF
ENDIF  
=IIF(!llDutyRat,lfSumDist(loFormSet),'')
*-- end of lfvDuty.
*:**************************************************************************
*:* Name        : lfv5
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Validate duty rate
*:***************************************************************************
*E302483,1 
FUNCTION lfv5
PARAMETERS loFormSet


*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[Start]
IF !(loFormSet.ActiveMode $ 'EA')
  RETURN 
ENDIF 
*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[End]



lcTmpSpLn = loFormSet.lcTmpSpLn

PRIVATE lcPMethod,lcPUnMeth,lnRecno,lnResp,lcFor
SELECT &lcTmpSpLn

IF NDUTYRAT<=0 
  REPLACE NDUTYRAT WITH 1
ENDIF
IF &lcTmpSpLn..KEY = CHR(255)
  BLANK FIELDS NDUTYRAT
  RETURN
ENDIF

IF POSHD.CDUTYCUR  = oAriaApplication.BaseCurrency                     
  IF ASCAN(loFormSet.laEvntTrig, PADR('DUTYSGNS',10)) = 0
    REPLACE NDUTYRAT WITH 1
    RETURN
  ENDIF
ENDIF 

=lfvDuty(loFormSet,loFormSet.lnGrdActCol+1,.T.)

SELECT &lcTmpSpLn
IF loFormSet.AriaForm1.chkUpdateAll.Value
  lnRecno = RECNO()
  lnNewRate = NDUTYRAT  
  =gfSEEK('PP'+&lcTmpSpLn..PO,'POSHD')
  lcCurr = POSHD.CDUTYCUR
  GO TOP
  SCAN FOR KEY = ' '
    =gfSEEK('PP'+&lcTmpSpLn..PO,'POSHD')
    IF POSHD.CDUTYCUR = lcCurr
      REPLACE NDUTYRAT WITH lnNewRate
    ENDIF
    =lfvDuty(loFormSet,loFormSet.lnGrdActCol+1,.T.)
  ENDSCAN
  IF BETWEEN(lnRecno,1,RECCOUNT(lcTmpSpLn))
    GOTO (lnRecno)
  ENDIF
ENDIF  
 
=lfSumDist(loFormSet)
*-- end of lfvDutyRat.
*:**************************************************************************
*:* Name        : lfwCs
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : When user enter cost
*:***************************************************************************
*E302483,1 
FUNCTION lfwCs
PARAMETERS loFormSet
lcTmpSpLn = loFormSet.lcTmpSpLn
lcCurrValue = ALLTRIM(STR(loFormSet.lnGrdActCol))
lcCurrFld = loFormSet.AriaForm1.grdShpCost.Column&lcCurrValue..ControlSource+"COD"


IF (loFormSet.ActiveMode ="A" .OR. loFormSet.ActiveMode ="E" ) .AND. !EMPTY(EVALUATE(lcCurrFld))
  RETURN .T.
ELSE 
  KEYBOARD '{TAB}' 
  RETURN .F.  
ENDIF 

*:**************************************************************************
*:* Name        : lfCs
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : Validate cost
*:***************************************************************************
*E302483,1 
FUNCTION lfCs
PARAMETERS loFormSet

*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[Start]
IF !(loFormSet.ActiveMode $ 'EA')
  RETURN 
ENDIF 
*B608667,2 MMT 09/30/2008 Fix bug of Validation in View Mode[End]

=lfSumDist(loFormSet)

*:**************************************************************************
*:* Name        : lfvisDutble
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/10/2007
*:* Purpose     : When press on Is Dutiable button
*:***************************************************************************
*E302483,1 
FUNCTION lfvisDutble
PARAMETERS loFormSet
lcTmpSpLn = loFormSet.lcTmpSpLn
lnCostItem = loFormSet.AriaForm1.cboCostItems.ListIndex
loFormSet.laCostItem[lnCostItem,6] = loFormSet.AriaForm1.chkFreight.Value

SELECT (lcTmpSpLn)
lnRecno = RECNO()
GO TOP
SCAN FOR KEY = ' '
  lfSumDist(loFormSet)
ENDSCAN
IF BETWEEN(lnRecno,1,RECCOUNT(lcTmpSpLn))
  GOTO (lnRecno)
ENDIF


*:**************************************************************************
*:* Name        : lfMovrSlct
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 04/02/2008
*:* Purpose     : Select items such that the selected item in the laCostItem popup is a percent of
*:***************************************************************************
*B608510,1 
FUNCTION lfMovrSlct
PARAMETERS lcPrcntOf,oFormSet
PRIVATE lnI,lnPos,lcPos,lcCst
DIMENSION laSource[1],laTarget[1]
STORE '' TO M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5,M_CITYPE6,M_CITYPE7,;
            laTarget , laSource
            
IF !USED('SHPCSTLN')
  = gfOpenTable('SHPCSTLN','SHPCSTLN')
  = gfSeek('')
ENDIF
SELECT PADR(cCode_No,6,' ') + '-' + PADR(cDescrip,30) + CCATGTYP+CBOMTYP FROM SHPCSTLN INTO ARRAY laSource
DIMENSION laSource[ALEN(laSource)+1]
=AINS(laSource,1)
lnCostItem = loFormSet.AriaForm1.cboCostItems.ListIndex
laSource[1] = 'PO Price'
=gfMover(@laSource,@laTarget,oFormSet.laCostItem[lnCostItem,1]+' is % of :',.T.,'')

IF !EMPTY(laTarget)
  lcPrcntOf = '0'
  FOR lnI = 1 TO ALEN(laTarget)
    IF 'PO Price' $ laTarget[lnI]
      lcPrcntOf = lcPrcntOf + '+(BPRICE*TOTQTY)'
    ELSE
    
      lcCst = '*' + LEFT(laTarget[lnI],6) + '*'
      lnPos = ASCAN(oFormSet.laCostItem,lcCst)
      IF lnPos>0
        lnPos = ASUBSCRIPT(oFormSet.laCostItem,lnPos,1) - 1
        lcPos = ALLTRIM(STR(lnPos))
        lcPrcntOf = lcPrcntOf + ' + COST&lcPos '
      ENDIF
    ENDIF
  ENDFOR
  
ELSE
  RETURN .F.  
ENDIF


