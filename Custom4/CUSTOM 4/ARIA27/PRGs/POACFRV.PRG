*:************************************************************************
*:        File: POACFRV.PRG                                             :*
*:      System: ARIA APPAREL SYSTEM 2.7                                 :*
*:     Modules: MF ,PO ,MA                                              :*
*:     Program: Adjust costs for Receiving.                             :*
*:      Author: Timour Abdalla Khalil                                   :*
*:************************************************************************
*: Program Types :  lcPType =>'I' Style P.O. Adjust Costs               :* 
*:                                Shipment Adjust Costs -> llByShp=.T.  :* 
*:                            'M' Cutting Ticket Adjust Costs           :* 
*:                            'T' MFG Order Adjust Costs                :* 
*:************************************************************************
*E301077,8 TAK 01/05/1999 Case the files used in this program with global function.
*E301077,8 MAB 03/20/1999 Opening and closing files.
*B603024,1 HDM 06/29/1999 Release "ALT+B" key trapping
*B802716,1 TAK 11/04/99 Fixed the bug of not getting the right Qty/Unt for
*B802716,1              each category based on what is in the C/T cost 
*B802716,1              sheet. By changing the index of the bomcost file 
*B802716,1              and the index of the tmporary file (lcTmpline1).
*B802716,1              Adding the cBomTyp in the For clause of the Scan
*B802716,1              command in the bomline file. And changing the way of
*B802716,1              calculating the unitcost by changing the formula to 
*B802716,1              be divided by the lnUnitQty. Also displaying only 8
*B802716,1              receiving lines in the forcasting option.
*E301347,1 HDM 11/16/1999 Make this program work with material Manfacturing orders
*B603348,1 ABD 03/20/2000 Update the picture of filed unitCost upon width of the filed.
*B802630,1 SSH 10/04/00   Fix the bug of not use actual cost in case of PO.
*! C102276,1 MHM 10/04/2001 Add a new option to the adjust cost for receiving screen
*! C102276,1 MHM            to get the total cost for all POs in case of shipment 
*! B604403,1 MHM 04/12/2001 Refresh the temp. file and browse in case of
*! B604403,1                selecting a new transaction.
*B604742,1 AMH 07/30/2001 Fix the bug of cannot adjust cost for receiving by shipment
*B604742,1                if received partially.
*B606124,1 KHM 06/25/2002 Fix the bug of duplicating styles.
*:*******************************************************************************
lcModUsedIn = 'PO-I,MF-M,MA-T'
lcPType     = SUBSTR(lcModUsedIn,ATC(gcWinAppl,lcModUsedIn)+3,1)

*E301077,8 MAB 03/20/1999 Comment out the following Block and transfer it after [Begin]
*E301077,8 MAB 03/20/1999 gfSetup block.
*--System has not been setup to use detailed costing,
*--Therefore the adjust costs for receivings is not available.
*IF lcPType ='I' AND ! gfGetMemVar('M_LImpCost')
*  =gfModalGen('TRM34085B00000','DIALOG')
*  RETURN
*ENDIF
*E301077,8 MAB 03/20/1999 Comment out the following Block and transfer it after [End  ]

*--Program variables.
STORE ''  TO lcWinCh0,lcWinCh1,lcWinCh2,lcTmpLine,lcTitle,lcCursor,;
             lcOldValue,lcBlFields,lcBrFields,lcScFields,lcCostImp
STORE .F. TO llBrowse,llCUpdate,llUpdated,llRefresh,;
             llLotMCst,llViewMod
STORE ' ' TO lcMCost1,lcMCost2,lcMCost3,lcMCost4,lcMCost5,;
             lcICost1,lcICost2,lcICost3,lcICost4,lcICost5,;
             lcTCost1,lcTCost2,lcTCost3,lcTCost4,lcTCost5
STORE ' ' TO lcMSLbl1,lcMSLbl2,lcMSLbl3,lcMSLbl4,lcMSLbl5,;
             lcISLbl1,lcISLbl2,lcISLbl3,lcISLbl4,lcISLbl5,;
             lcTSLbl1,lcTSLbl2,lcTSLbl3,lcTSLbl4,lcTSLbl5
STORE .F. TO llAlowNew,llNoShow,llGetInfo,llGenerate
STORE ' ' to lcBldAct,lcTmpLine1

*B606124,1 KHM 06/25/2002 (Begin) Initialized to hold the style grade.
lcStyFbGrd = ''
*B606124,1 KHM 06/25/2002 

*--Uncomplte session variables(UNCSS).
DECLARE laVariables[7]
lcAProgm   = 'POACFRV'
lcUnCmPgId = PADR('ADJCFRCV-'+lcPType,10)
lnSessNo   = gnProgCopy
lcUSessNo  = ' '
laVariables= ' '
llContinue = .F.
llGoAndChk = .T.
*-- Variable to tell if are we going to check the existance of
*-- any incomplete session in the screen's setup or not.
llChkUnCom = .T.
*--Uncomplete session variables.
laVariables[1]  = 'lcTktNo'
laVariables[2]  = 'laScrMode[3]'
laVariables[3]  = 'laScrMode[4]'
laVariables[4]  = 'ldEnterd'
laVariables[5]  = 'ldComplt'
laVariables[6]  = 'lnPieses'
laVariables[7]  = 'llGenerate'


*--Screen variables.
lcTktNo = SPACE(6)
STORE 0 TO lnPieses,lnMarker
STORE {} TO ldEnterd,ldComplt

IF !gfSetup()
  RETURN
ENDIF

*E301077,8 MAB 03/20/1999 This block transfered here to add files opened to array [Begin]
*E301077,8 MAB 03/20/1999 then close files in gfCleanUp calling .
*--System has not been setup to use detailed costing,
*--Therefore the adjust costs for receivings is not available.
IF lcPType ='I' AND ! gfGetMemVar('M_LImpCost')
  =gfModalGen('TRM34085B00000','DIALOG')
  RETURN
ENDIF
*E301077,8 MAB 03/20/1999 This block transfered here to add files opened to array [End  ]

*--Toolbar control pannel additional elements.
DECLARE laPanelObj[2,3]
laPanelObj[1,1] = 'pbRefrs'
laPanelObj[1,2] = gcBmpHome + "REFRESH.BMP"
laPanelObj[1,3] = [VALID lfvRefrsh() MESSAGE 'Refresh.' DISABLE]
laPanelObj[2,1] = 'pbCostF'
laPanelObj[2,2] = gcBmpHome + "COSTFORC.BMP"
laPanelObj[2,3] = [VALID lfvCstFore() MESSAGE 'Cost Forecasting.' DISABLE]
EXTERNAL ARRAY laDefProc
laDefProc[7]  = .F.     && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.     && Disable the control panel save proc.  (lpSavScr)
laDefProc[10] = .F.     && Disable the control panel close proc. (lpClsScr)

IF !WEXIST(gcBaseWind)

  lcCostImp  = gfGetMemVar('M_cCostImp')
  llLotMCst = ( gfGetMemVar('M_MatCstMt') $ 'LFI' )
  FOR I=1 TO IIF(lcPType = 'T',4,5)
    Z=STR(I,1)
    lc&lcPType.Cost&Z=gfGetMemVar('M_C'+lcPType+'Cost'+Z)
    lc&lcPType.SLbl&Z=gfGetMemVar('M_C'+lcPType+'SLbl'+Z)
  ENDFOR
  
  *--Program screen windows.
  lcWinCh0 = gfTempName()
  lcWinCh1 = gfTempName()
  lcWinCh2 = gfTempName()

  lcTmpLine  = gfTempName()
  lcTmpLine1 = gfTempName()
  lcCursor   = gfTempName()

  *--Get a unique session number, to be used to update the uncomplete 
  *--session file(UNCSS).
  lcUSessNo  = gfsequence('cSession')
  
  llNoShow = .F.
ENDIF


*--Flag to control if Adjust cost by Shipment.
llByShp = (lcPType ='I' AND lcCostImp = 'S' )

*--Open needed files.
DIME laOpnFls[5]
laOpnFls = .F.
IF lcPType = 'T'
  laOpnFls[1] = gfOpenFile(gcDataDir+'MMFGORDH','MMFGORDD','SH')
  laOpnFls[2] = gfOpenFile(gcDataDir+'MMFGORDD','MMFGORDD','SH')
  *E301347,1[START] Add Fabric file to the opened files
  laOpnFls[3] = gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')
  *E301347,1[END]
ELSE
  IF lcPType = 'I'
    laOpnFls[1] = gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
    laOpnFls[2] = gfOpenFile(gcDataDir+'POSLN','POSLN','SH')
    laOpnFls[3] = gfOpenFile(gcDataDir+'APVENDOR','VENCODE','SH')    
    laOpnFls[4] = gfOpenFile(gcDataDir+'SHPMTHDR','SHPMTHDR','SH')    
  ELSE
    laOpnFls[1] = gfOpenFile(gcDataDir+'CUTTKTH','CUTTKTH','SH')
    laOpnFls[2] = gfOpenFile(gcDataDir+'CUTTKTL','CUTTKTL','SH')
  ENDIF
  laOpnFls[5] = gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
ENDIF

IF llByShp
  SELECT POSLN
  SET ORDER TO TAG PosLnSh
  SELECT BOMLINE
  SET ORDER TO TAG BomLnShp
ENDIF


lcBrowTtl = 'Costing Lines'
lcTitle   = IIF(lcPType='M','cutting ticket',IIF(lcPType='T','MFG order',;
            IIF(llByShp,'shipment','purchase order')))
lcBaseFile=''

*--Activate Options pad.
=lfActPad()


DO (gcScrDir+"POACFRV.SPX")


RELEASE WINDOW (lcBrowTtl)
RELEASE PAD _Option OF _MSYSMENU

*-- HDM B603024,1 [Start] Release "ALT+B" key trapping
ON KEY LABEL ALT+B
*-- HDM B603024,1 [End]
*--Normal exit.
IF glQuitting
  *--Close Opened files.
  *E301077,8 Close using gfCloseFile.
  IF lcPType = 'T'
    IF laOpnFls[1] AND USED('MMFGORDH')
      *USE IN MMFGORDH
      =gfCloseFile('MMFGORDH')
    ENDIF
    IF laOpnFls[2] AND USED('MMFGORDD')
      *USE IN MMFGORDD
      =gfCloseFile('MMFGORDD')
    ENDIF
    *E301347,1[START] Close the fabric file
    IF laOpnFls[3] AND USED('FABRIC')
      =gfCloseFile('FABRIC')
    ENDIF
    *E301347,1[END]


  ELSE
    IF lcPType = 'I'
      IF laOpnFls[1] AND USED('POSHDR')
        *USE IN POSHDR
        =gfCloseFile('POSHDR')
      ENDIF
      IF laOpnFls[2] AND USED('POSLN')
        *USE IN POSLN
        =gfCloseFile('POSLN')
      ENDIF  
      IF laOpnFls[3] AND USED('APVENDOR')
        *USE IN APVENDOR
        =gfCloseFile('APVENDOR')
      ENDIF
      IF laOpnFls[4] AND USED('SHPMTHDR')
        *USE IN SHPMTHDR
        =gfCloseFile('SHPMTHDR')
      ENDIF  
    ELSE
      IF laOpnFls[1] AND USED('CUTTKTH')
        *USE IN CUTTKTH
        =gfCloseFile('CUTTKTH')
      ENDIF
      IF laOpnFls[2] AND USED('CUTTKTL')
        *USE IN CUTTKTL
        =gfCloseFile('CUTTKTL')
      ENDIF  
    ENDIF
    IF laOpnFls[5] AND USED('STYLE')
      *USE IN STYLE
      =gfCloseFile('STYLE')
    ENDIF
  ENDIF

  SELECT (lcTmpLine)
  USE
  ERASE (gcWorkDir+lcTmpLine+'.DBF')
  ERASE (gcWorkDir+lcTmpLine+'.CDX')
ENDIF
RETURN
*--End [POACFRV.PRG]...



*!*************************************************************
*! Name      : lpShow
*! Developer : Timour A. K.
*! Date      : 12/20/97
*! Purpose   : Show Screen Mode Changes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfUnCompSession(),lfActBrow(),
*!                          gfCPSave(),lfCreate()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lpShow()
*!*************************************************************
FUNCTION lpShow

DO CASE
  *-- S E L E C T  M O D E.
  CASE laScrMode[1]  && Select mode. 
    *--Check uncomplete session (UNCSS).
    IF llGoAndChk AND lfChkUnComS()
      RETURN
    ENDIF

    lcTktNo = SPACE(6)
    STORE 0 TO lnPieses,lnMarker
    STORE {} TO ldEnterd,ldComplt
    STORE .F. TO llCUpdate,llUpdated,llRefresh,llGetInfo
    llViewMod = .T.
    SHOW GET pbBrws  DISABLE
    SHOW GET pbTkt   ENABLE
    SHOW GET lcTktNo ENABLE
    SHOW GET pbRefrs DISABLE
    SHOW GET pbCostF DISABLE
    SHOW GET pbKey   DISABLE
    _CUROBJ = OBJNUM(lcTktNo)
    
    *B604403,1 MHM 04/12/2001 (Begin) Refresh the temporary file and the browse
    *B604403,1                in case of selecting a new transaction.
    IF USED (lcTmpLine)
      SELECT (lcTmpLine)
      BLANK  ALL
      DELETE ALL
    ENDIF
    *B604403,1 MHM 04/12/2001 (End)

  CASE laScrMode[2]  && View mode. 
    IF llGetInfo
      *--Uncsessvariable(UNCSS).
      llGoAndChk = .T.
      =lfCrtTempFl()
      =lfCreate(.F.)
    ENDIF
    llGetInfo = .T.
    llViewMod = .T.
    SHOW GET pbTkt   DISABLE
    SHOW GET lcTktNo DISABLE
    SHOW GET pbRefrs DISABLE
    SHOW GET pbCostF ENABLE
    SHOW GET pbKey   ENABLE
    
  CASE laScrMode[3]  && Edit mode. 
    STORE .T. TO llCUpdate,llUpdated
    *--Uncsessvariable(UNCSS).
    llGoAndChk = .T.
    llViewMod = .F.
    SHOW GET pbTkt   DISABLE
    SHOW GET lcTktNo DISABLE
    SHOW GET pbRefrs ENABLE
    SHOW GET pbCostF ENABLE
    SHOW GET pbKey   ENABLE
    *--Create uncmsess record.
    IF !llContinue
      =lfCrUnSess('O') 
    ENDIF

  CASE laScrMode[4]  && Add  mode.     
    STORE .T. TO llCUpdate,llUpdated
    llViewMod = .F.
    *--Uncsessvariable(UNCSS).
    llGoAndChk = .T.
    SHOW GET pbTkt   DISABLE
    SHOW GET lcTktNo DISABLE
    SHOW GET pbRefrs DISABLE
    SHOW GET pbCostF ENABLE
    SHOW GET pbKey   ENABLE
    *--Create uncmsess record.
    IF !llContinue
      =lfCrUnSess('O') 
    ENDIF
        
ENDCASE
*--Activate browse.
=lfActBrow()
RETURN


*!*************************************************************
*! Name      : lfActPad
*! Developer : Timour A. K.
*! Date      : 12/20/97
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfActPad()
*!*************************************************************
FUNCTION lfActPad

ON KEY LABEL ALT+P ACTIVATE POPUP _OPTIONPOP
DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Refresh' SKIP FOR !laScrMode[3] OR llRefresh
DEFINE BAR 2 OF _OPTIONPOP PROMPT 'Cost Forecasting' SKIP FOR laScrMode[1]
ON SELECTION BAR 1 OF _OPTIONPOP llDumy = lfvRefrsh()
ON SELECTION BAR 2 OF _OPTIONPOP llDumy = lfvCstFore()

*C102276,1 MHM 10/04/2001 [start] call Triger in case of shipment (llByShp)
IF ASCAN(laEvntTrig , PADR('ADDOPTN',10)) <> 0 AND llByShp
  =gfDoTriger('POACFRV',PADR('ADDOPTN',10))
ENDIF     
*C102276,1 MHM 10/04/2001 [end]

ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowTtl)
RETURN


*:*************************************************************
*! Name    : lfTrapKy
*! Purpose : Trap key.
*:*************************************************************
FUNCTION lfTrapKy
IF WONTOP() = lcBrowTtl
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lpTab
  ON KEY LABEL BACKTAB DO lpShiftTab
ENDIF  
RETURN


*!*************************************************************
*! Name    : lfReadAct
*! Purpose : Read Activate clear trap.
*!*************************************************************
FUNCTION lfReadAct
*--Clear Trap.
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
RETURN


*!*************************************************************
*! Name      : lpTab
*! Purpose   : Tab key trapping procedure.
*!*************************************************************
PROCEDURE lptab

IF WONTOP()=lcBrowTtl
  *ACTIVATE WINDOW (gwcContrl1)
  ACTIVATE WINDOW (lcWinCh0)
  IF laScrMode[1]
    _CUROBJ=OBJNUM(pbcptask)
  ELSE
    IF laScrMode[2]
      _CUROBJ=OBJNUM(pbSlct)
    ELSE
      _CUROBJ=OBJNUM(pbSav)
    ENDIF
  ENDIF
ELSE
  ACTIVATE WINDOW (lcBrowTtl)
ENDIF  
RETURN


*!*************************************************************
*! Name      : lpShiftTab
*! Purpose   : Shift Tab key trapping procedure.
*!*************************************************************
PROCEDURE lpshifttab

IF WONTOP()=lcBrowTtl
  ACTIVATE WINDOW (lcWinCh0)
  IF laScrMode[1]
    _CUROBJ=OBJNUM(lcTktNo)
  ELSE
    _CUROBJ=OBJNUM(pbCls)
  ENDIF
ELSE
  ACTIVATE WINDOW (lcBrowTtl)
ENDIF
RETURN


*:*************************************************************
*! Name      : lfActBrow
*! Developer : Timour A. K.
*! Date      : 12/20/97
*! Purpose   : Activate screen browse.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfwBrow(),lfvBrow()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfActBrow()
*!*************************************************************
FUNCTION lfActBrow

SELECT (lcTmpLine)
llViewMod = (laScrMode[1] OR laScrMode[2])
lnMarker  = RECNO()

*lcBrCfld= [Marker =IIF(RECNO()=lnMarker ,'>>','  '):H='  ':R:2:W=.F.,]+;
          IIF(llByShp,[cTktNo :8 :H='P/O #' :R:W=.F.,],'')+;
          [Style   :27 :H='Style'  :R:W=.F.,]+;
          [Item    :27 :H='Item'   :R:W=.F.,]+;
          [IClr    :9  :H='Itm. Clr' :R:W=.F.,]+;
          [lcMFCd=IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),MfgCode) :16 :H='MFG Code' :R:W=.F.,]+;
          [UnitQty :H='Qty / Unit' :P='@z 999.999' :V=(UnitQty>=0) :W=!(EMPTY(Style) OR llViewMod OR cCatgTyp $ 'PD'),]+;
          [UnitCost :H='Unit Cost' :P='@z 999.9999999' :V=(UnitCost>=0) :W=!(EMPTY(Style) OR llViewMod)]

*B603348,1 ABD Update the picture of filed unitCost upon width of the filed. [Begin]
*lcBrCfld= [Marker =IIF(RECNO()=lnMarker ,'>>','  '):H='  ':R:2:W=.F.,]+;
          IIF(llByShp,[cTktNo :8 :H='P/O #' :R:W=.F.,],'')+;
          [Style   :27 :H=IIF(lcPType='T','Fabric','Style')  :R:W=.F.,]+;
          [sClr:H = 'Color',]+;
          [Item    :27 :H='Item'   :R:W=.F.,]+;
          [IClr    :9  :H='Itm. Clr' :R:W=.F.,]+;
          [lcMFCd=IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),MfgCode) :16 :H='MFG Code' :R:W=.F.,]+;
          [UnitQty :H='Qty / Unit' :P='@z 999.999' :V=(UnitQty>=0) :W=!(EMPTY(Style) OR llViewMod OR cCatgTyp $ 'PD'),]+;
          [UnitCost :H='Unit Cost' :P='@z 999.9999999' :V=(UnitCost>=0) :W=!(EMPTY(Style) OR llViewMod)]

lcBrCfld= [Marker =IIF(RECNO()=lnMarker ,'>>','  '):H='  ':R:2:W=.F.,]+;
          IIF(llByShp,[cTktNo :8 :H='P/O #' :R:W=.F.,],'')+;
          [Style   :27 :H=IIF(lcPType='T','Fabric','Style')  :R:W=.F.,]+;
          [sClr:H = 'Color',]+;
          [Item    :27 :H='Item'   :R:W=.F.,]+;
          [IClr    :9  :H='Itm. Clr' :R:W=.F.,]+;
          [lcMFCd=IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),MfgCode) :16 :H='MFG Code' :R:W=.F.,]+;
          [UnitQty :H='Qty / Unit' :P='@z 999.999' :V=(UnitQty>=0) :W=!(EMPTY(Style) OR llViewMod OR cCatgTyp $ 'PD'),]+;
          [UnitCost :H='Unit Cost' :P='@z 9999999.99999' :V=(UnitCost>=0) :W=!(EMPTY(Style) OR llViewMod)]
          
*B603348,1 ABD [End]
BROWSE FIELDS &lcBrCfld ;
       NOAPPEND ;
	   NOCLEAR  ;
  	   NODELETE ;
       NOMENU   ;
  	   NOWAIT   ;
	   SAVE     REST;
       WHEN lfwBrow();
       VALID :F lfvBrow() ;
       TITLE (lcBrowTtl) ;
       WINDOW (lcWinCh1) ;
       IN WINDOW (gcBaseWind)         
RETURN


*:*************************************************************
*! Name      : lfwBrow
*! Developer : Timour A. K.
*! Date      : 12/20/97
*! Purpose   : Browse When function.
*!*************************************************************
FUNCTION lfwBrow
lnSkip = IIF(INLIST(LASTKEY(),18,5) ,-1,1)
lnSkip = IIF(lnSkip = -1 AND BOF(),1,lnSkip)
IF EMPTY(Style)
  SKIP lnSkip
ENDIF
CLEAR TYPEAHEAD
lnSkip = IIF(EMPTY(Style),lfwBrow(),lnSkip)
lnMarker  = RECNO()
SHOW WINDOW (lcBrowTtl) REFRESH
RETURN 


*:*************************************************************
*! Name      : lfvBrow
*! Developer : Timour A. K.
*! Date      : 12/20/97
*! Purpose   : Browse Valid function.
*!*************************************************************
FUNCTION lfvBrow

IF WONTOP() # (lcBrowTtl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
RETURN


*:*************************************************************
*! Name      : lfvTkt
*! Developer : Timour A. K.
*! Date      : 12/20/97
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

PRIVATE lnAlias, lcBrFields 
IF !llBrowse AND EMPTY(lcTktNo)
  RETURN
ENDIF
llBrowse =.F.
lnAlias  =SELECT()
lcHdrFile=IIF(lcPType ='M','CUTTKTH',IIF(lcPType ='T','MMfgOrdH',IIF(llByShp,'SHPMTHDR','POSHDR')))
SELECT(lcHdrFile)
DO CASE 

  CASE lcPType ='I' AND ! llByShp
    IF !SEEK('P'+lcTktNo)
      DO POSBrow WITH lcTktNo,"",'P'
    ENDIF 

  CASE lcPType ='I' AND llByShp
    IF !SEEK(lcTktNo)
      lcBrFields = [ShipNo :8:H='Shipment #',]+;
                   [Status :1:H="S",]+;
                   [Entered :8:H='Entered',]+;
  	               [Cartons :5:H='Cartons',]+;
	               [AirWayB :15:H='Air-Way Bill #',]+;
        	       [ETA     :8:H='E.T.A.',]+;
        	       [TotQty  :7:H='In-Transit',]+;
    	           [Recv_Stk :6:H='Received',]+;
    	           [Recv_Dam :6:H='Damaged',]+;
        	       [Recv_Can :6:H='Canceled',]+;
        	       [cVessel :30:H='Airline/Vessel',]+;
    	           [Reference :H='Reference']
      DIME laTempData[1]
      STORE '' TO laTempData
      =gfBrows(.F.,'ShipNo','laTempData','Shipments')
      lcTktNo=laTempData[1]
    ENDIF 
    IF ShpmtHdr.Status = 'O'
      SELECT POSLN
      IF ! SEEK(lcTktNo)
        *--The shipment lines have not been found! unable to proceed.
        = gfModalGen('TRM34077B42000','DIALOG')
        lcTktNo = SPACE(6)
      ELSE
        lnPieses = 0
        lcPOChk=SPACE(6)
        SCAN WHILE ShipNo=lcTktNo
          IF PO<>lcPOChk 
            lnPieses = lnPieses+IIF(SEEK('P'+PosLn.Po,'POSHDR'),POSHDR.nStyOrder,0) 
            lcPOChk = Po
          ENDIF
        ENDSCAN
      ENDIF  
    ENDIF
    SELECT ShpmtHdr

  CASE lcPType ='M'
    IF !SEEK(lcTktNo)
      xCuttkt=lcTktNo
      = CutBrow(lcTktNo,' ')
      lcTktNo=xCuttkt
    ENDIF

  CASE lcPType ='T'
    *E301347,1 [Start] Seek in thE MMFGORDH FILE useing tag Mmfgordh

    PRIVATE lcMFGTag
    lcMFGTag = ORDER('Mmfgordh')
    SET ORDER TO Mmfgordh
    IF !SEEK(lcTktNo)
    *E301347,1 [End]
      *E301347,1 [Start] Use the Global MFG Order Browse
      *lcBrFields   = [cMFGOrdNo:H='Order#',]+;
                     [cFabric  :H='Fabric',]+;
                     [cWareCode:H='Warehouse',]+;
                     [Status   :H='S',]+;
                     [Entered  :H='Entered',]+;
                     [Complete :H='Complete']
      *DIME laTempData[1]
      *STORE '' TO laTempData
      *=gfBrows(.F.,'cMFGOrdNo','laTempData',"Material Manufacturing Orders")
      *lcTktNo=laTempData[1]
      IF gfMFGOrdBr(@lcTktNo)
        *Now the lcTktNo has the order #
        lcTktNo = MMFGORDH.CMFGORDNO
        *SHOW GETS
      ELSE
        lcTktNo     = ''
        _CUROBJ     = _CUROBJ
      ENDIF
      *E301347,1 [End]
    ENDIF
    *E301347,1 [Start] Restore the old tag
    SET ORDER TO lcMFGTag IN Mmfgordh
    *E301347,1 [End]
ENDCASE
SHOW GET lcTktNo

IF EMPTY(lcTktNo) OR EOF(lcHdrFile)
  SELECT(lnAlias)
  _CUROBJ=OBJNUM(lcTktNo)
  RETURN
ENDIF

IF lcPType ='M'
  IF !(Status $ 'OA')
    *--Only XXX can be accepted, Cannot proceed.
    =gfModalGen('TRM34086B00000','DIALOG','open or actualizied cut ticket')
    lcTktNo=SPACE(6)
  ENDIF
ELSE
  *B802630,1 SSH 10/04/00 *** Begin ***  Use actual cost in case of PO.
  *IF Status <> 'O'
  IF !(Status $ 'OA')
    *B802630,1 SSH 10/04/00 *** End ***
    *--Only XXX can be accepted, Cannot proceed.
    =gfModalGen('TRM34086B00000','DIALOG','the status open')
    lcTktNo=SPACE(6)
  ENDIF
ENDIF

IF EMPTY(lcTktNo)
  SELECT(lnAlias)
  _CUROBJ=OBJNUM(lcTktNo)
  RETURN
ENDIF

ldEnterd = &lcHdrFile..Entered
ldComplt = IIF(llByShp,&lcHdrFile..ETA,&lcHdrFile..Complete)

*B606124,1 KHM 06/25/2002 (Begin) Initialized to hold the style grade.
lcStyFbGrd = ''
*B606124,1 KHM 06/25/2002 (End)

DO CASE
  CASE lcPType ='M'
    lnPieses = &lcHdrFile..Pcs_Bud

    *B606124,1 KHM 06/25/2002 (Begin) Get the style grade for the cutting ticket.
    lcStyFbGrd = IIF(SEEK(ALLTRIM(&lcHdrFile..Style),'Style'),Style.cStyGrade,'')
    *B606124,1 KHM 06/25/2002 (End)
    
  CASE lcPType ='I' AND !llByShp
    lnPieses = &lcHdrFile..nStyOrder
    
    *B606124,1 KHM 06/25/2002 (Begin) Get the style grade for the PO.
    lcStyFbGrd = IIF(SEEK('P'+lcTktNo,'PosLn'),PosLn.cStyGrade,'')
    *B606124,1 KHM 06/25/2002 (End)
        
  CASE lcPType ='T'
    lnPieses = &lcHdrFile..nMMGBudget

    *B606124,1 KHM 06/25/2002 (Begin) Get the style grade for the Manuf. Orders.
    lcStyFbGrd = IIF(SEEK(&lcHdrFile..cFabric,'Fabric'),Fabric.cFabGrade,'')
    *B606124,1 KHM 06/25/2002 (End)
    
ENDCASE
SHOW GET lcTktNo
llGetInfo = .F.
llGenerate= .F.
lcBldAct  = ' '
IF !lfCreate(.F.)
  lcTktNo = SPACE(6)
  STORE 0 TO lnPieses,lnMarker
  STORE {} TO ldEnterd,ldComplt
  SELECT(lnAlias)
  _CUROBJ=OBJNUM(lcTktNo)
  RETURN
ELSE
  laScrMode = .F.
  lnMode = IIF(llGenerate,4,2)
  laScrMode[lnMode] = .T.
ENDIF
SHOW GETS
=lfRefresh(lcWinCh0)
SHOW WINDOW (lcBrowTtl) REFRESH
RETURN



*:*************************************************************
*! Name      : lfCreate
*! Developer : Timour A. K.
*! Date      : 12/20/97
*! Purpose   : To create the temproray bom line file.
*:*************************************************************
*! Calls     : 
*!             Procedures : lpActCst
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : llToVew -> Not ask to create yes/no.
*!*************************************************************
*! Returns            : llLoop -> Not Valid selections.
*!*************************************************************
*! Example   : =lfCreate()
*!*************************************************************
FUNCTION lfCreate
PARA llToVew

SELECT BomLine
IF llByShp
  llFstTme=.T.  && flag for first time only.
  IF SEEK('I'+'2'+lcTktNo)
    SCAN REST WHILE cIMTyp+cType+ShipNo='I'+'2'+lcTktNo FOR EMPTY(cRSession)
      SCATTER MEMVAR
      m.nRec = RECNO()
      INSERT INTO (lcTmpLine) FROM MEMVAR
    ENDSCAN
  ENDIF
  SELECT POSLN
  SEEK lcTktNo

  *B604742,1 AMH Scan for shipment only [Start]
  *SCAN WHILE ShipNo=lcTktNo FOR EMPTY(cRSession)
  SCAN REST WHILE Shipno+cStytype+Po+Style+STR(LineNo,6)+TranCd=lcTktNo FOR TranCd='3'
  *B604742,1 AMH [End]

    IF SEEK('P'+PosLn.Po,'POSHDR') AND POSHDR.Status <> 'O'
      LOOP
    ENDIF
    lcPO=PO  
    lcLnNo=STR(LineNo,6)
    lcStyClr=Style
    SELECT BomLine

    *B604742,1 AMH if this line has prevuse adjust cost or recieved partially [Start]
    *IF !SEEK('I2'+lcTktNo+lcPO+lcLnNo)
    *B604742,4 AMH if this line not has prevuse adjust cost [Start]
    *IF !SEEK('I2'+lcTktNo+lcPO+lcLnNo) .OR. !EMPTY(cRSession)
    IF !SEEK('I2'+lcTktNo+lcPO+lcLnNo)
    *B604742,4 AMH [End]
    *B604742,1 AMH [End]

      IF llFstTme AND !llToVew
        llFstTme=.F.
        *--No previous cost records found! Generate new records?,\<Yes,\<No
        lnGenrate = gfModalGen('QRM34087B00006','DIALOG')
        IF lnGenrate=1
          llGenerate=.T.    &&To create a new records.
        ELSE
          RETURN .F.
        ENDIF
      ENDIF
      IF SEEK('I2'+SPACE(6)+lcPO+lcLnNo)
        SCAN WHILE cIMTyp+cType+ShipNo+cTktNo+STR(LINENO,6)='I2'+SPACE(6)+lcPO+lcLnNo ;
               FOR Style+SClr=lcStyClr
          SCATTER TO laFields        
          SELECT &lcTmpLine
          APPE BLANK
          GATHER FROM laFields
          REPLACE ShipNo WITH lcTktNo
        ENDSCAN
      ELSE
        IF SEEK('I1'+SPACE(6)+lcPO+lcLnNo)
          SCAN WHILE cIMTyp+cType+ShipNo+cTktNo+STR(LINENO,6)='I1'+SPACE(6)+lcPO+lcLnNo ;
                 FOR Style+SClr=lcStyClr
            SCATTER TO laFields
            SELECT &lcTmpLine
            APPE BLANK
            GATHER FROM laFields
            REPLACE ShipNo WITH lcTktNo,;
                    cType  WITH '2'
          ENDSCAN
        ELSE
          *--No cost sheet details found for XXX.
          =gfModalGen('TRM34088B00000','DIALOG','P/O :'+lcPo+' of this Shipment')
        ENDIF 
      ENDIF
   
    *B604742,4 AMH if this line has prevuse adjust cost [Start]
    ELSE
      LOCATE REST WHILE cimtyp+ctype+shipno+ctktno+STR(lineno,6)+;
                        cbomtyp+style+sclr+item+iclr+mfgcode =;
                        'I'+'2'+lcTktNo+lcPO+lcLnNo FOR EMPTY(cRSession)
      IF !FOUND()                  
        IF llFstTme AND !llToVew
          llFstTme=.F.
          *--No previous cost records found! Generate new records?,\<Yes,\<No
          lnGenrate = gfModalGen('QRM34087B00006','DIALOG')
          IF lnGenrate=1
            llGenerate=.T.    &&To create a new records.
          ELSE
            RETURN .F.
          ENDIF
        ENDIF  
        
        IF llGenerate
          IF SEEK('I1'+SPACE(6)+lcPO+lcLnNo)
            SCAN WHILE cIMTyp+cType+ShipNo+cTktNo+STR(LINENO,6)='I1'+SPACE(6)+lcPO+lcLnNo ;
                 FOR Style+SClr=lcStyClr
               SCATTER TO laFields
               SELECT &lcTmpLine
               APPE BLANK
               GATHER FROM laFields
               REPLACE ShipNo WITH lcTktNo,;
                       cType  WITH '2'
             ENDSCAN
          ENDIF
        ENDIF  
      ENDIF
    *B604742,4 AMH [End]

    ENDIF
  ENDSCAN

  SELE (lcTmpLine)
  GO TOP
  IF EOF()
    *--No cost sheet details found for XXX.
    =gfModalGen('TRM34088B00000','DIALOG','P/Os of this Shipment')
    RETURN .F.
  ENDIF

ELSE     && Not by shipment.

  IF ! SEEK(lcPType+'1'+lcTktNo)
    *--No cost sheet details found for XXX.
    =gfModalGen('TRM34088B00000','DIALOG','this '+lcTitle)
    RETURN .F.
  ELSE 
    IF ! SEEK(lcPType+'2'+lcTktNo)
      IF !llToVew
        *--No previous cost records found! Generate new records?,\<Yes,\<No
        lnGenrate = gfModalGen('QRM34087B00006','DIALOG')
      ELSE
        lnGenrate=1
      ENDIF
      IF lnGenrate=1
        llGenerate=.T.    &&To create a new records.
        SEEK lcPType+'1'+lcTktNo
        lcCond1 = "cIMTyp+cType+cTktNo = lcPType+'1'+lcTktNo"
      ELSE
        RETURN .F.
      ENDIF
    ELSE
      LOCATE REST WHILE cIMTyp+cType+cTktNo=lcPType+'2'+lcTktNo FOR EMPTY(cRSession)
      IF FOUND() 
        lcCond1="cIMTyp+cType+cTktNo=lcPType+'2'+lcTktNo"
      ELSE
        IF !llToVew
          *--No previous cost records found! Generate new records?,\<Yes,\<No
          lnGenrate = gfModalGen('QRM34087B00006','DIALOG')
        ELSE
          lnGenrate=1
        ENDIF
        IF lnGenrate=1
          llGenerate=.T.    &&To create a new records.
          SEEK lcPType+'1'+lcTktNo
          lcCond1="cIMTyp+cType+cTktNo=lcPType+'1'+lcTktNo"
        ELSE
          RETURN .F.
        ENDIF
      ENDIF 
    ENDIF
    =SEEK(lcPType+IIF(llGenerate,'1','2')+lcTktNo)

    *B606124,1 KHM 06/25/2002 (Begin) Adding the check of style grade.
    *SCAN REST WHILE cIMTyp+cType+cTktNo=lcPType+IIF(llGenerate,'1','2')+lcTktNo;
              FOR EMPTY(cRSession)
    SCAN REST WHILE cIMTyp+cType+cTktNo=lcPType+IIF(llGenerate,'1','2')+lcTktNo;
              FOR EMPTY(cRSession) AND cStyGrade = lcStyFbGrd
    *B606124,1 KHM 06/25/2002 (End)
    
      SCATTER MEMVAR
      m.nRec = RECNO()
      INSERT INTO (lcTmpLine) FROM MEMVAR
    ENDSCAN
  ENDIF
ENDIF

SELECT (lcTmpLine)
SET ORDER TO TAG &lcTmpLine

*B802630,1 SSH 10/04/00 *** Begin ***  Use actual cost in case of PO.
*IF  lcPType='M' .AND. CutTktH.Status='A'
IF  IIF(lcPType = 'I',POSHDR.Status='A',;
                      IIF(lcPType = 'M',CutTktH.Status='A',.F.))
*B802630,1 SSH 10/04/00  *** End ***
  *--Cutting ticket has been actualized. 
  *--Do you wish to default the new cost items to the actual cost or the estimated cost?
  IF EMPTY(lcBldAct)
    lnChse = gfModalGen('QRM38127B38017','DIALOG')
    IF lnChse = 1
      SET ORDER TO TAG &lcTmpLine1
      *B802716,1 Added go top.
      GO TOP
      *B802716,1 End.
      DO WHILE !EOF()
        DO lpActCst
      ENDDO
    ENDIF
    lcBldAct = IIF(lnChse=1,'A','E')
  ELSE
    IF lcBldAct = 'A'
      SET ORDER TO TAG &lcTmpLine1
      DO WHILE !EOF()
        DO lpActCst
      ENDDO
    ENDIF  
  ENDIF
ENDIF

SELECT (lcTmpLine)
SET ORDER TO TAG (lcTmpLine)
SET FILTER TO !lVoid
GO TOP
IF EOF()
  *--All items have allready been received for this lcTitle.
  = gfModalGen('TRM34089B00000','DIALOG',lcTitle)
  RETURN .F.
ENDIF

*--Create title record.
lcPrvType=' '
SCAN FOR !EMPTY(cType)
  lcPrvTp  =cType
  lcPrvSp  =ShipNo
  lcPrvTk  =cTktNo
  IF lcPrvType <> cBomTyp
    lcPrvType=cBomTyp
    lnSavRc=RECNO()
    lcCstTttl='< '+SUBSTR(ALLTRIM(lc&lcPType.Cost&lcPrvType),1,15)+' >'
    lcLftTttl=REPLICATE('<',(19-len(lcCstTttl))/2)
    lcRgtTttl=REPLICATE('>',(19-len(lcCstTttl))/2)
    APPEND BLANK
    REPLACE cType   WITH lcPrvTp,;
            ShipNo  WITH lcPrvSp,;
            cTktNo  WITH lcPrvTk,;
            cBomTyp WITH lcPrvType,;
            Style   WITH '',;
            Item    WITH lcLftTttl+lcCstTttl+lcRgtTttl,;
            IClr    WITH '',;
            MfgCode WITH '' 
    GOTO lnSavRc
  ENDIF
ENDSCAN
GO TOP
RETURN



*:*************************************************************
*! Name      : lfvRefrsh
*! Developer : Timour A. K.
*! Date      : 12/20/97
*! Purpose   : To Refresh the screen with any added items to
*!             the cost sheet.
*:*************************************************************
*! Calls     : 
*!             Procedures : lpActCst
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvRefrsh()
*!*************************************************************
FUNCTION lfvRefrsh

SHOW GET pbRefrs DISABLE
IF llRefresh
  RETURN
ENDIF  
SELECT BOMLINE
IF llByShp
  SET RELATION TO '2'+lcTktNo+CTKTNO+CBOMTYP+STR(LINENO,6)+STYLE+ITEM+ICLR+MFGCODE  INTO &lcTmpLine
  SELECT POSLN
  SEEK lcTktNo
  SCAN WHILE ShipNo=lcTktNo FOR EMPTY(cRSession)
    IF SEEK('P'+PosLn.Po,'POSHDR') AND POSHDR.Status <> 'O'
      LOOP
    ENDIF
    lcPO=PO    
    lcLnNo=STR(LineNo,6)
    lcStyClr=Style
    SELECT BOMLINE
    IF SEEK('I1'+SPACE(6)+lcPO+lcLnNo)
      SCAN WHILE cIMTyp+cType+ShipNo+cTktNo+STR(LineNo,6)='I1'+SPACE(6)+lcPO+lcLnNo ;
              FOR Style=lcStyClr
        *--If this line is not added to the adjustment , add it to the temp file.
        *--and remark it (lNew=True).
        IF EOF(lcTmpLine)
          SCATTER MEMVAR
          m.ShipNo= lcTktNo
          m.cType= '2'
          m.lNew = .T.
          INSERT INTO (lcTmpLine) FROM MEMVAR
        ENDIF
      ENDSCAN
    ENDIF
  ENDSCAN
ELSE
  SET RELATION TO '2'+CTKTNO+CBOMTYP+STR(LINENO,6)+STYLE+ITEM+ICLR+MFGCODE INTO &lcTmpLine
  llAct=.F.
  SELECT BOMLINE
  =SEEK(lcPType+'1'+lcTktNo)
  SCAN REST WHILE cIMTyp+cType+cTktNo=lcPType+'1'+lcTktNo
    *--For each record cType=1 and locate for its couple of cType=2
    *--there is cType=1 without cType=2 (i.e. this line added 
    *--and wasn't exist the first time this adjustment done) 
    IF EOF(lcTmpLine)
      SCATTER MEMVAR
      m.nRec = RECNO()
      m.cType='2'
      m.lnew=.T.
      INSERT INTO (lcTmpLine) FROM MEMVAR            
      llAct = .T.
    ENDIF
  ENDSCAN
  SELECT (lcTmpLine)
  *--Cutting ticket has been actualized. 
  *--Do you wish to default the new cost items to the actual cost or the estimated cost?
  IF llAct AND lcPType='M' AND CutTktH.Status='A' AND ;
     gfModalGen('QRM38127B38017','DIALOG') = 1
    lnSetOrder = VAL(SYS(21))
    SET ORDER TO TAG &lcTmpLine1
    DO WHILE !EOF()
      IF !lNew
        SKIP
        LOOP
      ENDIF  
      DO lpActCst
    ENDDO
    SET ORDER TO lnSetOrder          
  ENDIF
ENDIF
SELECT BOMLINE
SET RELATION TO
SELECT (lcTmpLine)
GO TOP
=lfActBrow()
llRefresh = .T.
RETURN


*:*************************************************************
*! Name      : lpActCst
*! Developer : Timour A. K.
*! Date      : 12/20/97
*! Purpose   : default the new cost items to the actual cost.
*:*************************************************************
PROCEDURE lpActCst
*B802716,1 Added type to expresion.
*lcKey = MfgCode+Item+IClr
lcKey = cBomTyp+MfgCode+Item+IClr
*B802716,1 End.
=SEEK(lcKey)
STORE 0 TO lnUsed,lnRecQty,lnRecAmt,lnUnitCost,lnOpen,lnLstRecYld,lnLstRecCst
*--Get open quantity for all styles that use this item
*B802716,1 Added type to expresion.
*SCAN REST WHILE MfgCode+Item+IClr = lcKey
SCAN REST WHILE cBomTyp+MfgCode+Item+IClr = lcKey
*B802716,1 End.
  lcStyClr= Style
  *B802630,1 SSH 10/04/00 *** Begin ***  Use actual cost in case of PO.
  IF lcPType = 'I'
    IF SEEK('P'+lcTktNo+lcStyClr,'POSLN')
      SELECT POSLN
      SUM REST IIF(TranCd='1',TotQty,-TotQty) TO lnClrOpen ;
          WHILE cstytype+po+style+STR(lineno,6)+trancd=;
                'P'+lcTktNo+lcStyClr
      lnOpen = lnOpen + MAX(lnClrOpen,0)
    ENDIF
  ELSE
  *B802630,1 SSH 10/04/00 *** End.. ***
    IF SEEK(lcTktNo+lcStyClr,'CutTktL')
      SELECT CutTktL
      SUM REST IIF(TranCd='1',TotQty,-TotQty) TO lnClrOpen ;
          WHILE CutTkt+Style=lcTktNo+lcStyClr
      lnOpen = lnOpen + MAX(lnClrOpen,0)
    ENDIF
  *B802630,1 SSH 10/04/00 *** Begin ***  Use actual cost in case of PO.
  ENDIF
  *B802630,1 SSH 10/04/00  *** End.. ***
ENDSCAN
=SEEK(lcKey)
*--Get used quantity from this item component.
*B802716,1 Changed the seek expresion.
*IF SEEK(cIMTyp+cTktNo+cBomTyp+IIF(!(cCatgTyp $ "PMD"),Item,PADR(MfgCode,12))+IClr,'cTktBom')
IF SEEK(cIMTyp+cTktNo+cBomTyp+Item+IClr+MfgCode,'cTktBom')
*B802716,1 End.
  lnUsed = cTktBom.Used_Qty
ENDIF
*--Get received quantity, received amnout, last received yield
*--and last received cost for this item component.
IF SEEK(cIMTyp+'2'+cTktNo+STR(LineNo,6)+cBomTyp,'BomLine')
  SELECT BomLine
*B802716,1 Added type to expresion.
*  SCAN REST WHILE cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp ='M2'+lcTktNo+STR(&lcTmpLine..LineNo,6)+&lcTmpLine..cBomTyp ;
        FOR MfgCode+Item+IClr = lcKey .AND. !EMPTY(cRSession)
  SCAN REST WHILE cIMTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp ='M2'+lcTktNo+STR(&lcTmpLine..LineNo,6)+&lcTmpLine..cBomTyp ;
        FOR cBomTyp+MfgCode+Item+IClr = lcKey .AND. !EMPTY(cRSession)
*B802716,1 End.

    lnRecQty = lnRecQty + ItemQty
    lnRecAmt = lnRecAmt + ItemAmt
    lnLstRecYld = UnitQty
    lnLstRecCst = UnitCost
  ENDSCAN
ENDIF
IF lnOpen = 0
  lnUnitQty  = lnLstRecYld
  lnUnitCost = lnLstRecCst
ELSE
  lnUnitQty = MAX(ROUND((lnUsed-lnRecQty)/lnOpen,3),0)
  *--Get actual unit cost for this item component.
  *B802716,1 fixed of wrong cost calculation.
  SELECT BOMCOST
  SET ORDER TO TAG BomCsTkt
  *cbomtype+cimtyp+ctktno+item+iclr+mfgcode
  *IF SEEK(&lcTmpLine..cTktNo+IIF(!(&lcTmpLine..cCatgTyp $ "PMD"),;
          'N'+&lcTmpLine..Item,'Y'+&lcTmpLine..MfgCode)+&lcTmpLine..IClr,'BomCost')
    *IF llLotMCst
   *   SELECT BomCost
   *   SUM REST nTotACst TO lnActAmt WHILE ;
   *   cTktNo+IIF(EMPTY(Item),"Y","N")+IIF(cCostType $ "MDP",MfgCode,Item)+IClr =;
   *   &lcTmpLine..cTktNo+IIF(!(&lcTmpLine..cCatgTyp $ "PMD"),'N'+;
   *   &lcTmpLine..Item,'Y'+&lcTmpLine..MfgCode)+&lcTmpLine..IClr
   *   lnUnitCost = MAX(ROUND((lnActAmt-lnRecAmt)/lnOpen,7),0)
    *ELSE
    *  lnUnitCost = BomCost.nUnitACst
    *ENDIF
  *ENDIF
  lcSekBCost = &lcTmpLine..cbomtyp+&lcTmpLine..cimtyp+&lcTmpLine..ctktno+&lcTmpLine..item+&lcTmpLine..iclr+&lcTmpLine..mfgcode
  lnActAmt   = 0
  IF SEEK(lcSekBCost,'BomCost')
    SUM REST nTotACst TO lnActAmt WHILE ;
    cbomtype+cimtyp+ctktno+item+iclr+mfgcode =lcSekBCost  
  ENDIF
  IF lnUnitQty<>0
    lnUnitCost = MAX(ROUND(((lnActAmt-lnRecAmt)/lnOpen)/lnUnitQty,7),0)
  ELSE
    lnUnitCost = MAX(ROUND(((lnActAmt-lnRecAmt)/lnOpen),7),0)  
  ENDIF
  *B802716,1 End.
ENDIF
SELECT (lcTmpLine)
*B802716,1 Added type to expresion.
*ItemAmt  WITH ItemQty*UnitCost WHILE cBomTyp+MfgCode+Item+IClr = lcKey
REPLACE REST  UnitQty  WITH lnUnitQty ,;
              UnitCost WITH lnUnitCost ,;
              ItemQty  WITH StyQty*UnitQty,;
              ItemAmt  WITH ItemQty*UnitCost WHILE cBomTyp+MfgCode+Item+IClr = lcKey
RETURN



*:*************************************************************
*! Name      : gfCPDelete
*! Developer : Timour A. K.
*! Date      : 12/20/97
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
FUNCTION gfCPDelete
PRIVATE lnSetOrder

*--The current adjustment will be ignored. Accept,Cancel
IF gfModalGen('TRM34090B34007','DIALOG') = 1
  SELECT BOMLINE
  IF llByShp
    IF SEEK('I'+'2'+lcTktNo)
      DELETE REST WHILE CIMTYP+CTYPE+SHIPNO='I2'+lcTktNo FOR EMPTY(cRSession)
    ENDIF 
  ELSE
    IF SEEK(lcPType+'2'+lcTktNo)
      DELETE REST WHILE CIMTYP+CTYPE+CTKTNO=lcPType+'2'+lcTktNo FOR EMPTY(cRSession)
    ENDIF
  ENDIF
  laScrMode=.F.
  laScrMode[1] = .T.
  SHOW GETS
ENDIF
RETURN


*!*************************************************************
*! Name    : lpClsScr
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Cancel/Close entery.
*!*************************************************************
FUNCTION lpClsScr

*--Cancel unCmSess record.
IF SEEK('O'+lcUnCmPgId+PADR(gcUser_id,10)+lcUSessNo,'UNCMSESS')
  =lfCrUnSess('I') 
ENDIF
RETURN


*:*************************************************************
*! Name      : lpSavScr
*! Developer : Timour A. K.
*! Date      : 12/20/97
*! Purpose   : Save/Update.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
PROCEDURE lpSavScr

IF ! llContinue
  =lfCrUnSess('pbSav') 
ENDIF

WAIT WINDOW 'Start Updating...' NOWAIT
SELECT (lcTmpLine)
GO TOP
SCAN FOR !lVoid AND !EMPTY(Style) AND nSteps <> 1
  *E301347,1 [START] Read the fabric grade from fabric file
  *lcStyGrad=IIF(SEEK(Style,'STYLE'),STYLE.cStyGrade,'1')
  IF lcPType ='T'
    IF SEEK(PADR(Style,7)+SCLR,'FABRIC')
      lcStyGrad = '1'
    ENDIF
  ELSE
    IF SEEK(Style,'STYLE')
      lcStyGrad = STYLE.cStyGrade
    ENDIF
  ENDIF
  *E301347,1[END]
  IF llGenerate
    SCATTER TO laFields
    SELECT BomLine
    IF ! EMPTY(&lcTmpLine..nRec) AND llByShp
      GOTO &lcTmpLine..nRec      
    ELSE
      APPEND BLANK
    ENDIF 
    =RLOCK()
    GATHER FROM laFields 
    REPLACE cIMTyp  WITH lcPType,;
            cType   WITH '2',;
            StyQty  WITH 0,;
            ItemQty WITH 0,;
            ItemAmt WITH 0,;  
            cStyGrade WITH lcStyGrad
    UNLOCK 
  ELSE
    *--If I did a refresh to the screen, there are some records 
    *-of has to added to the BOMLINE file
    IF llRefresh AND lNew
      SCATTER TO laFields
      SELECT BOMLINE
      IF ! EMPTY(&lcTmpLine..nRec) AND llByShp
        GOTO &lcTmpLine..nRec      
      ELSE
        APPEND BLANK
      ENDIF 
      =RLOCK()
      GATHER FROM laFields 
      REPLACE cIMTyp  WITH lcPType,;
              cType   WITH '2',;
              StyQty  WITH 0,;
              ItemQty WITH 0,;
              ItemAmt WITH 0,;  
              cStyGrade WITH lcStyGrad
      UNLOCK 
    ELSE    && i.e. cType = '2' or  && i.e. llRefresh = .F.
      SELECT BomLine
      GOTO &lcTmpLine..nRec      
      =RLOCK()
      REPLACE UnitQty  WITH &lcTmpLine..UnitQty,;
              UnitCost WITH &lcTmpLine..UnitCost 
      UNLOCK 
    ENDIF
  ENDIF
  SELECT (lcTmpLine)
  REPLACE nSteps WITH 1
  FLUSH

ENDSCAN
WAIT CLEAR

*--Complete unCmSess record ,Session completed.
=lfCrUnSess('C') 
RETURN


*:*************************************************************
*! Name      : lfvCstFore
*! Developer : Timour A. K.
*! Date      : 12/20/97
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

STORE 0 TO lnStyles,lnOldQty,lnOldCst,lnMarker
STORE '' TO lcCStyle,lcBrowseTl
DIMENSION laStyles(1,2)

SELECT (lcTmpLine)
lnSavRc = RECNO()
GO TOP
SCAN FOR !EMPTY(Style)
  *IF ASCAN(laStyles,Style) = 0
  IF ASCAN(laStyles, IIF(lcPType = 'T' , PADR(Style,13) + sClr , Style)) = 0
    lnStyles = lnStyles + 1
    DIMENSION laStyles(lnStyles,2)

    *E301347,1[start]
    *laStyles[lnStyles,1] = Style
    laStyles[lnStyles,1] = IIF(lcPType = 'T' , PADR(Style,13) + sClr , Style)
    *E301347,1[end]

    laStyles[lnStyles,2] = lnStyles
  ENDIF
ENDSCAN  
*E301347,1[START]
*lcCStyle = laStyles[1,1]
lcCStyle  = PADR(laStyles[1,1],19)
*E301347,1[END]

*lnSPopLen = IIF(lnStyles>7, 9,lnStyles+2)

*E301347,1[Start] Re-write this part to act according to program type
*=SEEK(lcCStyle,'Style')
*lcStyDesc = Style.Desc1

IF lcPType = 'T'
  =SEEK(PADR(lcCStyle,7)+SUBSTR(lcCStyle, LEN(lcCStyle)-6 , LEN(lcCStyle)))
  lcStyDesc = FABRIC.DESC
ELSE
  =SEEK(lcCStyle ,'STYLE')
  lcStyDesc = Style.Desc1
ENDIF

*E301347,1[End]

DIMENSION laSessions(1,2)
lnSessions = 0
=lfCrtCstHist()
PUSH KEY
=lfFOTrap()
ON KEY LABEL ALT+R ACTIVATE WINDOW (lcBrowseTl)
SHOW GET pbCostF DISABLE
DO (gcScrDir+"POACFO.SPX")
SHOW GET pbCostF ENABLE
POP KEY
SELECT (lcTmpLine)
GOTO lnSavRc
RETURN


*!*************************************************************
*! Name      : lfCrtCstHist  
*! Developer : Wael Aly Mohamed
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
*E301347,1[START]
*PRIVATE lcBaseStruc,lcAddStruc,lcBomType,lcCutTag,lcPosTag,lcBomTag,lnAlias,lcSession
PRIVATE lcBaseStruc,lcAddStruc,lcBomType,lcCutTag,lcPosTag,lcBomTag,lnAlias,lcSession,lcMfgTag
*E301347,1[END]
WAIT 'Collecting costing information. Please standby....' WINDOW NOWAIT
lnAlias = SELECT()
lcBaseStruc = 'cTktNo C(6),cMarker C(1),cSort C(1),cBomTyp C(1),Item C(19),LineNo N(6),cCatgTyp C(1),;
IClr C(6),Mfg C(6),Uom C(3),Desc C(20),EstItmQty N(7,3),EstUntCst N(7,3),EstItmAmt N(9,3),CurItmQty N(7,3),CurUntCst N(7,3),CurItmAmt N(9,3)'
lcAddStruc  = ''
IF lcPType='M'
  lcCutTag = ORDER('CutTktL')
  SET ORDER TO TAG CutRec IN CutTktL
ELSE
  *E301347,1[START]
  IF lcPType='I'
  *E301347,1[End]
    lcPosTag = ORDER('PosLn')
    SET ORDER TO TAG PoShRec IN PosLn
    *E301347,1[START]
  ELSE
    lcMfgTag = ORDER('MMFGORDD')
    SET ORDER TO TAG MFGREC IN MMFGORDD
  ENDIF
  *E301347,1[End]
ENDIF
lcBomTag = ORDER('BomLine')
*B802716,1 make it desending.
*SET ORDER TO BomLnShp IN BomLine
SET ORDER TO BomLnShp IN BomLine DESCENDING
*B802716,1 End.
SET ORDER TO TAG cTktBom IN cTktBom

DIMENSION laSessions(1,2)
laSessions = ''
lnSessions = 0

*E301347,1[START]
lcdate=''
*E301347,1[END]

SELECT BomLine

IF SEEK(lcPType+'2'+IIF(llByShp,lcTktNo,SPACE(6)+lcTktNo))
  SCAN REST WHILE CIMTYP+CTYPE+SHIPNO+CTKTNO = lcPType+'2'+IIF(llByShp,lcTktNo,SPACE(6)+lcTktNo) ;
              FOR Style = IIF(lcPType='T',SUBSTR(lcCStyle,1,7),lcCStyle) .AND. !EMPTY(cRSession)
    IF ASCAN(laSessions,cRSession,1) = 0
      lcData = ' '
      IF lcPType='M'
        IF SEEK(cRSession+lcTktNo+lcCStyle+'2','CutTktL')
          lcdate = '('+PADL(MONTH(CutTktL.Date),2,'0')+'-'+PADL(DAY(CutTktL.Date),2,'0')+')'
        ENDIF
      ELSE  && lcPType='I'
        *E301347,1[Start] Include the Material MFG Order
        IF lcPType='I'
          IF SEEK(IIF(llByShp,lcTktNo,SPACE(6))+cRSession+'P'+cTktNo+lcCStyle+STR(LineNo,6)+'2','PosLn')
            lcdate = '('+PADL(MONTH(PosLn.Date),2,'0')+'-'+PADL(DAY(PosLn.Date),2,'0')+')'
          ENDIF
        ELSE  &&lcPType='T'
          IF SEEK(cRSession + lcTktNo + PADR(lcCStyle,7) + SCLR + '2' , 'MMFGORDD')
            lcdate = '('+PADL(MONTH(MMFGORDD.DrecvDate),2,'0')+'-'+PADL(DAY(MMFGORDD.DrecvDate),2,'0')+')'
          ENDIF
        ENDIF
      ENDIF
      IF !EMPTY(lcdate) 
        lnSessions = lnSessions + 1
        DIMENSION laSessions(lnSessions,2)
        laSessions[lnSessions,1] = cRSession
        laSessions[lnSessions,2] = lcdate
        lcAddStruc = lcAddStruc + ',ItemQtyR'+STR(lnSessions,1)+' N(7,3), UntCstR'+STR(lnSessions,1)+' N(7,3),ItmAmtR'+STR(lnSessions,1)+' N(9,3)'
      ENDIF
      *B802716,1
      IF lnSessions = 8
        EXIT
      ENDIF

    ENDIF
  ENDSCAN
  =ASORT(laSessions,1)
ENDIF
CREATE CURSOR (lcCursor) (&lcBaseStruc &lcAddStruc)
INDEX ON cTktNo+cBomTyp+cSort+Item+IClr+Mfg TAG (lcCursor) OF (lcCursor)


SELECT (lcTmpLine)
SET FILTER TO Style = IIF(lcPType = 'T',SUBSTR(lcCStyle,1,7),lcCStyle)
GO TOP
DO WHILE !EOF()
  
  lcTicket = cTktNo
  INSERT INTO (lcCursor) (cTktNo,cSort,cBomTyp,Desc) VALUE (lcTicket,'G','G','Grand total :')

  DO WHILE cType + cTktNo = IIF(llByShp OR !llGenerate,'2','1')+lcTicket

    lcBomType = cBomTyp
    INSERT INTO (lcCursor) (cTktNo,Item,IClr,Mfg,cSort,cBomTyp,Desc) VALUE ;
    (lcTicket,'*******************','******','******','0',lcBomType,PROPER(EVAL('lc'+lcPType+'SLbl'+lcBomType)))
    INSERT INTO (lcCursor) (cTktNo,cSort,cBomTyp,Desc) VALUE (lcTicket,'2',lcBomType,'Sub total :')

    SCAN REST WHILE cType + cTktNo + cBomTyp = ;
              IIF(llByShp OR !llGenerate,'2','1')+lcTicket+lcBomType ;
              FOR IIF(lcPType = 'T', PADR(Style,13)+sClr,Style) = lcCStyle

      =SEEK(lcPType+'1'+SPACE(6)+lcTicket+STR(LineNo,6)+lcBomType+Style+SClr+Item+IClr+MfgCode,'BomLine')
      =SEEK(lcPType+lcTicket+lcBomType+Item+IClr+MfgCode,'cTktBom') 
      
      INSERT INTO (lcCursor)(cTktNo,cSort,cBomTyp,Item,IClr,Mfg,Desc,EstItmQty,EstUntCst,EstItmAmt,Uom,LineNo,cCatgTyp) VALUE ;
      (lcTicket,'1',lcBomType,Bomline.Item,Bomline.IClr,Bomline.MfgCode,cTktBom.Desc,BomLine.UnitQty,BomLine.UnitCost,BomLine.UnitQty*BomLine.UnitCost,cTktBom.Uom,BomLine.LineNo,BomLine.cCatgTyp)

      REPLACE &lcCursor..CurItmQty WITH &lcTmpLine..UnitQty  ,;
              &lcCursor..CurUntCst WITH &lcTmpLine..UnitCost  ,;
              &lcCursor..CurItmAmt WITH &lcTmpLine..UnitQty*&lcTmpLine..UnitCost

      IF SEEK(lcTicket+lcBomType+'2',lcCursor)
         REPLACE &lcCursor..CurItmAmt WITH &lcCursor..CurItmAmt + &lcTmpLine..UnitQty*&lcTmpLine..UnitCost ,;
                 &lcCursor..EstItmAmt WITH &lcCursor..EstItmAmt + BomLine.UnitQty*BomLine.UnitCost
      ENDIF
      IF SEEK(lcTicket+'GG',lcCursor)
         REPLACE &lcCursor..CurItmAmt WITH &lcCursor..CurItmAmt + &lcTmpLine..UnitQty*&lcTmpLine..UnitCost ,;
                 &lcCursor..EstItmAmt WITH &lcCursor..EstItmAmt + BomLine.UnitQty*BomLine.UnitCost
      ENDIF
      *E301347,1[start]
      *lcRcvdKey = lcPType+'2'+IIF(llByShp,lcTktNo+lcTicket,SPACE(6)+lcTicket)+;
                  STR(&lcTmpLine..LineNo,6)+lcBomType+lcCStyle+SPACE(6)+;
                  &lcTmpLine..Item+&lcTmpLine..IClr+&lcTmpLine..MfgCode

      lcRcvdKey = lcPType+'2'+IIF(llByShp,lcTktNo+lcTicket,SPACE(6)+lcTicket)+;
                  STR(&lcTmpLine..LineNo,6)+lcBomType+lcCStyle+IIF(lcPType='T',RIGHT(laStyles[1,1],6),SPACE(6))+;
                  &lcTmpLine..Item+&lcTmpLine..IClr+&lcTmpLine..MfgCode
      *E301347,1[end]
      
      *--Index Expresion : cimtyp+ctype+shipno+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode
      IF SEEK(lcRcvdKey,'BomLine')
        SELECT BomLine
        SCAN REST WHILE cimtyp+ctype+shipno+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode =;
                        lcPType+'2'+IIF(llByShp,lcTktNo+lcTicket,SPACE(6)+lcTicket)+;
        STR(&lcTmpLine..LineNo,6)+lcBomType+lcCStyle+&lcTmpLine..SClr+&lcTmpLine..Item+&lcTmpLine..IClr+&lcTmpLine..Mfgcode ;
        FOR !EMPTY(cRSession)
          *B802716,1
          *lcSession = STR(ASUBSCRIPT(laSessions,ASCAN(laSessions,BomLine.cRSession,1),1),1)
          lnElemNo = ASCAN(laSessions,BomLine.cRSession,1)          
          IF lnElemNo > 0 AND BETWEEN(STR(ASUBSCRIPT(laSessions,lnElemNo,1),1),'1','8')
            lcSession = STR(ASUBSCRIPT(laSessions,lnElemNo,1),1)

            IF SEEK(lcTicket+lcBomType+'1'+Item+IClr+MfgCode,lcCursor)
              REPLACE &lcCursor..ItemQtyR&lcSession WITH BomLine.UnitQty  ,;
                      &lcCursor..UntCstR&lcSession  WITH BomLine.UnitCost ,;
                      &lcCursor..ItmAmtR&lcSession  WITH BomLine.UnitQty*BomLine.UnitCost
            ENDIF
            IF SEEK(lcTicket+lcBomType+'2',lcCursor)
              REPLACE &lcCursor..ItmAmtR&lcSession WITH &lcCursor..ItmAmtR&lcSession+BomLine.UnitQty*BomLine.UnitCost
            ENDIF
            IF SEEK(lcTicket+'GG',lcCursor)
              REPLACE &lcCursor..ItmAmtR&lcSession  WITH &lcCursor..ItmAmtR&lcSession+BomLine.UnitQty*BomLine.UnitCost
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
IF lcPType='M'
  SET ORDER TO TAG (lcCutTag) IN CutTktL
ELSE
  *E301347,1[START]
  IF lcPType = 'I'
  *E301347,1[END]
    SET ORDER TO TAG (lcPosTag) IN PosLn  
  *E301347,1[START]
  ELSE
    SET ORDER TO TAG (lcMFGTag) IN MMFGORDD
  ENDIF
  *E301347,1[END]
ENDIF  
SET ORDER TO TAG (lcBomTag) IN BomLine  
SELECT (lnAlias)
WAIT CLEAR 
RETURN


*!*************************************************************
*! Name      : lfwForcst.
*! Developer : Wael Aly Mohamed
*! Purpose   : Whem Browse forecasting function.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwForcst()
*!*************************************************************
FUNCTION lfwForcst

SELECT (lcCursor)
llViewMod = (laScrMode[1] OR laScrMode[2])
lnMarker  = RECNO()
lcFields= [cMarker=IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F.,]+;
          IIF(llByShp,[cTktNo=IIF(cSort='1',cTktNo,SPACE(6)):H='PO#':8 :R,],'')+;
          [Item:H='Item':19:R,]+;
          [IClr:H='Color':7:R,]+;
          [Mfg:H='Mfg':7:R,]+;
          [Uom:H='UOM':6:R,]+;
          [Desc:H='Description':20:R,]+;
          [EstItmQty:H='Est.Qty.':9:R,]+;
          [EstUntCst:H='Est.Cost.':9:R,]+;
          [EstItmAmt:H='Ext.':9:R]
FOR lnCount = 1 TO lnSessions
  lcCount = STR(lnCount,1)
  lcFields= lcFields + ;
  ',ItemQtyR'+lcCount+':H='+"'"+'Qty. '+laSessions(lnCount,2)+"'"+':12:R,'+;
    'UntCstR'+lcCount+':H='+"'"+'Cost '+laSessions(lnCount,2)+"'"+':12:R,'+;
    'ItmAmtR'+lcCount+':H='+"'"+'Ext. '+laSessions(lnCount,2)+"'"+':12:R'
ENDFOR          
lcFields= lcFields + ;
          [,CurItmQty:H='Cur.Qty.':9:W=(!llViewMod AND lfwItemQty()):V=lfvItemQty(),]+;
          [CurUntCst :H='Cur.Cost':9:W=(!llViewMod AND lfwItemCst()):V=lfvItemCst(),]+;
          [CurItmAmt:H='Ext.':9:R]
BROWSE FIELDS &lcFields ;
       WINDOW    POACFO1  ;
       IN WINDOW POACFO ;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
       NOCLEAR          ;
       WHEN lfwFOBrow() ;
       TITLE lcBrowseTl 
RETURN

*!*************************************************************
*! Name      : lfFOTrap
*! Developer : Timour A. K.
*! Date      : 12/20/97
*! Purpose   : READ Deactivate function of screen MFG750
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfFOTrap()
*!*************************************************************
FUNCTION lfFOTrap

ON KEY LABEL CTRL+Q    lnDummy = 1
ON KEY LABEL CTRL+W    lnDummy = 1
ON KEY LABEL CTRL+HOME GO TOP
ON KEY LABEL CTRL+END  GO BOTTOM
ON KEY LABEL ESC Clear Read
ON KEY LABEL TAB DO lpTab1
ON KEY LABEL BACKTAB DO lpBTab1
RETURN

*!*************************************************************
*! Name      : lpTab1
*! Developer : Timour A.K.
*! Date      : 01/13/1998
*! Purpose   : Trap of tab key.
*!*************************************************************
PROCEDURE lpTab1
IF WONTOP()=lcBrowseTl
  ACTIVATE WINDOW POACFO2
  _CUROBJ=OBJNUM(ibStyles)
ELSE
  _CUROBJ=_CUROBJ+1
ENDIF  
RETURN

*!*************************************************************
*! Name      : lpBTab1 
*! Developer : Timour A.K.
*! Date      : 01/13/1998
*! Purpose   : Trap of back tab key.
*!*************************************************************
PROCEDURE lpBTab1
IF WONTOP()=lcBrowseTl
  ACTIVATE WINDOW POACFO2
  _CUROBJ=OBJNUM(pbClose)
ELSE
  IF _CUROBJ=OBJNUM(pbClose)
    _CUROBJ=OBJNUM(ibStyles)
  ELSE
    _CUROBJ=_CUROBJ-1
  ENDIF
ENDIF  
RETURN

*!*************************************************************
*! Name      : lfwFOBrow
*! Developer : Timour A. K.
*! Date      : 12/20/97
*! Purpose   : Show function for FO.
*!*************************************************************
FUNCTION lfwFOBrow

lnMarker = RECNO(lcCursor)
SHOW WINDOW (lcBrowseTl) REFRESH SAME


*!*************************************************************
*! Name      : lfvStyles           
*! Developer : Timour A. K.
*! Date      : 12/20/97
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
PRIVATE lcOldSty

lcOldSty = lcCStyle
lcCStyle = laStyles[ibStyles,1]

IF lcCStyle <> lcOldSty
  SHOW GET lcCStyle
  *=SEEK(lcCStyle,'Style')
  *lcStyDesc = Style.Desc1
  IF lcPType = 'T'
    =SEEK(PADR(lcCStyle,7)+SUBSTR(lcCStyle, LEN(lcCStyle)-6 , LEN(lcCStyle)))
    lcStyDesc = FABRIC.DESC
  ELSE
    =SEEK(lcCStyle ,'STYLE')
    lcStyDesc = Style.Desc1
  ENDIF
  SHOW GET lcStyDesc
  =lfCrtCstHist()
  =lfwForcst()
ENDIF
RETURN


*!*************************************************************
*! Name      : lfwItemQty      
*! Developer : Timour A. K.
*! Date      : 12/20/97
*! Purpose   : When function for cost forecasting unit quantity
*!*************************************************************
FUNCTION lfwItemQty

IF cCatGTyp $ 'DP'
  RETURN(.F.)
ENDIF
lnOldQty = CurItmQty

*!*************************************************************
*! Name      : lfwItemCst      
*! Developer : Timour A. K.
*! Date      : 12/20/97
*! Purpose   : When function for cost forecasting unit cost
*!*************************************************************
FUNCTION lfwItemCst

lnOldCst = CurUntCst

*!*************************************************************
*! Name      : lfvItemQty        
*! Developer : Timour A. K.
*! Date      : 12/20/97
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

lcCurrRec = cTktNo+cBomTyp+cSort+Item+IClr+Mfg
lcTicket = cTktNo
lnAdjAmt = (CurItmQty-lnOldQty)*CurUntCst
REPLACE CurItmAmt WITH CurItmAmt + lnAdjAmt
IF SEEK(lcTicket+cBomTyp+'2')
  REPLACE CurItmAmt WITH CurItmAmt + lnAdjAmt
ENDIF
IF SEEK(lcTicket+'GG')
  REPLACE CurItmAmt WITH CurItmAmt + lnAdjAmt
ENDIF
=SEEK(lcCurrRec)
IF SEEK(IIF(llByShp OR !llGenerate,'2','1')+IIF(llByShp,lcTktNo,'')+lcTicket+cBomTyp+STR(LineNo,6)+lcCStyle+Item+IClr+Mfg,lcTmpLine)
  REPLACE &lcTmpLine..UnitQty  WITH CurItmQty ,; 
          &lcTmpLine..ItemQty  WITH &lcTmpLine..StyQty*CurItmQty ,;
          &lcTmpLine..ItemAmt  WITH &lcTmpLine..StyQty*CurItmQty*&lcTmpLine..UnitCost
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvItemCst       
*! Developer : Timour A. K.
*! Date      : 12/20/97
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

lcCurrRec = cTktNo+cBomTyp+cSort+Item+IClr+Mfg
lcTicket = cTktNo
lnAdjAmt = (CurUntCst-lnOldCst)*CurItmQty
REPLACE CurItmAmt WITH CurItmAmt + lnAdjAmt
IF SEEK(lcTicket+cBomTyp+'2')
  REPLACE CurItmAmt WITH CurItmAmt + lnAdjAmt
ENDIF
IF SEEK(lcTicket+'GG')
  REPLACE CurItmAmt WITH CurItmAmt + lnAdjAmt
ENDIF
=SEEK(lcCurrRec)
IF SEEK(IIF(llByShp OR !llGenerate,'2','1')+IIF(llByShp,lcTktNo,'')+lcTicket+cBomTyp+STR(LineNo,6)+lcCStyle+Item+IClr+Mfg,lcTmpLine)
  REPLACE &lcTmpLine..UnitCost WITH CurUntCst ,;
          &lcTmpLine..ItemQty  WITH &lcTmpLine..StyQty*&lcTmpLine..UnitQty ,;
          &lcTmpLine..ItemAmt  WITH &lcTmpLine..StyQty*&lcTmpLine..UnitQty*CurUntCst          
ENDIF
RETURN


*--UNCOMPLETE SESSION FUNCTIONS(UNCSS).
*!*************************************************************
*! Name    : lfChkUnComS
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Check if there an uncomplete session.
*!*************************************************************
FUNCTION lfChkUnComS
PARAMETERS llFrmSetup
PRIVATE    llFondSess

llFondSess = .F.
llGoAndChk = IIF(llFrmSetup, .F., llGoAndChk)
IF (llFrmSetup AND llChkUnCom) OR !llFrmSetup
  llChkUnCom = .F.
  laScrMode  = .F.
  IF gfUnCompSession(lcUnCmPgId, lnSessNo, "adjust costs for receiving")
    llFondSess   = .T.
    llContinue   = .T.
    llFirst      = .F.
    lcUSessNo    = UNCMSESS.cSession
    lcCurObj     = ALLTRIM(UnCmSess.cCurrObj)
    =lfActBrow()
    IF !EMPTY(lcCurObj) AND lcCurObj = 'pbSav'
       llViewMode = laScrMode[3]
       llContinue = .T.
       =gfCPSave()
       llGoAndChk   = .T.
       laScrMode    = .F.
       IF llViewMode
         laScrMode[2] = .T.
       ELSE
         laScrMode[1] = .T.
       ENDIF
    ENDIF
    SHOW GETS
    =lfRefresh()
    llFirst=.F.
  ELSE
    laScrMode[1] = .T.
    =lfCrtTempFl()
  ENDIF
ENDIF
=lfActBrow()
RETURN (llFondSess)


*:*************************************************************
*! Name      : lfCrUnSess
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Create/Update uncomplete session record.
*:*************************************************************
*! Call      : lfUpdVars()
*:*************************************************************
FUNCTION lfCrUnSess
PARA lcUnSStat

lnAlias = SELECT()
SELECT UNCMSESS
IF !SEEK('O'+lcUnCmPgId+PADR(gcUser_id,10)+lcUSessNo)
  IF !SEEK('I'+lcUnCmPgId+PADR(gcUser_id,10)) 
    INSERT INTO UNCMSESS (Status,cUTranType,cUserId,cSession,cProgram,;
       cCurrScr,dTranDate,cTranTime,ccurrobj) VALUES ;
       ('O',lcUnCmPgId,gcUser_id,lcUSessNo,lcAProgm,'',gdSysDate,TIME(),' ')
  ELSE
    REPLACE Status    WITH 'O',;
            cSession  WITH lcUSessNo,;
            dTranDate WITH gdSysDate,;
            cTranTime WITH TIME(),;
            ccurrobj  WITH lcUnSStat
  ENDIF
  =lfUpdVars()
  =RLOCK('UNCMSESS')
ELSE
  *--Update uncomplete session.
  SELECT UNCMSESS
  IF lcUnSStat='pbSav'
    REPLACE cCurrObj  WITH 'pbSav'
  ELSE
    IF lcUnSStat='C'
      lcUSessNo = gfsequence('cSession')
    ENDIF
    REPLACE Status WITH lcUnSStat
    UNLOCK
    llContinue = .F.
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN


*!*************************************************************
*! Name      : lfUpdVars
*! Developer : Timour A. K. 
*! Date      : 10/10/97               
*! Purpose   : Function to create a string to be saved in the field
*!             UNCMSESS.mComent and UNCMSESS.mTmpFiles
*!*************************************************************
*! Call      : gfSavSess()
*!*************************************************************
FUNCTION lfUpdVars

lcFiles = "lcTmpLine," + lcTmpLine + "," + ORDER(lcTmpLine) + ";"
= gfSavSess(lcUnCmPgId, lcFiles, @laVariables,lcUSessNo)
RETURN


*!*************************************************************
*! Name      : lfCrtTempFl
*! Developer : Timour A. K. 
*! Date      : 10/10/97               
*! Purpose   : To create all the uncompete session temprory files
*!             that is needed by this program.
*!*************************************************************
*! Call      : gfCrtTmp()
*!*************************************************************
FUNCTION lfCrtTempFl

*--Temp file.
SELECT BomLine
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+3,4]
laFileStru[lnFileStru+1,1] = 'nRec'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 10
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'lNew'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'nSteps'
laFileStru[lnFileStru+3,2] = 'N'
laFileStru[lnFileStru+3,3] = 2
laFileStru[lnFileStru+3,4] = 0

*--Index tags array.
DIMENSION laTags[2,2]
IF llByShp
  laTags[1,1]='cType+ShipNo+cTktNo+cBomTyp+STR(LineNo,6)+Style+Item+IClr+MFGCode'
ELSE
  laTags[1,1]='cType+cTktNo+cBomTyp+STR(LineNo,6)+Style+Item+IClr+MFGCode'
ENDIF
laTags[1,2]=lcTmpLine
*B802716,1 Added type to index expresion.
*laTags[2,1]='MfgCode+Item+IClr+Style'
laTags[2,1]='cBomTyp+MfgCode+Item+IClr+Style'
laTags[2,2]=lcTmpLine1
*B802716,1 End.
=gfCrtTmp(lcTmpLine,@laFileStru,@laTags)

SELECT (lcTmpLine)
SET ORDER TO
RETURN
*--End...

