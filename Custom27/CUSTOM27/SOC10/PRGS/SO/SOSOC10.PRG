*!*******************************************************************************
*! Program file  : SoSoc10.PRG
*! Program desc. : Custom Sales Orders Screen for Soccer Select
*! Screen        : SoSoc10.SPR
*! System        : Aria Apparel System
*! Module        : Sales Order (SO)
*! Developer     : Sameh Saiid Ezzat (SSE)
*! Date          : 02/05/2001
*! Reference     : C102118
*!*******************************************************************************
*! Passed Parameters  : lcOrdType  : Order type
*!                             'O' : Order 
*!                             'C' : Contract
*!                             'T' : EDI temporary Order 
*!                      lcOrderNo : Order#
*!*******************************************************************************
*! Example        : DO SoSoc10
*!*******************************************************************************
*! N.B. This custom screen is a typical copy from Standard Sales Order Screen with 
*!      some modifications in the Standard Code (SoOrd.Prg)
*!*******************************************************************************
*B606908,1 SSE 01/23/2003 Fix some bugs in Custom Sales Order screen.
*B606923,1 SSE 01/29/2003 Fix object order in SOSOC322 screen.
*!*******************************************************************************
PARAMETERS lcOrdType,lcOrderNo
EXTERNAL ARRAY laData , laKeyField , laWobjects

DECLARE laFolders[3,2], laAddress[6,3], laQtyPaste[1,8], laFoldWinds[3,2], ;
        laKeyField[2,4], laSetups[14,2], laCodes[7,10], laTerms[1,2], laSeasons[1,2],;
        laDivision[1,2], laShipVia[1,2], laSpcInst[1,2], laWareHouses[1,2], ;
        laCanReason[1,2], laOCtgCd[1,2], laDRltFld[2,2], laVariables[6], ;
        laOrdStatus[5], laBrowArr[1], laDisRltFld[1,2]

*-- Variable llMultiSt is first initialized with True instead of False 
*-- because Soccer Select always places Multi Store Order.
*-- Also they don't have Bulk Orders so llFromBulk is first Initialized 
*-- with False instead of True
*-- Variables llZoom, llClearPck, llConsAcc, llGoAndChk, llChkUnCom not used anymore.
STORE .T. TO llESTYPSO, llClearPck, llFromPack, llReBrowse, llDetails, ;
             llUpdBook, llConsAcc, llCntrExst, llGoAndChk, llChkUnCom, llMultiSt
STORE .F. TO laSetups, llRebuild, llIgnorAll, llDomestic, m.lContract, llMove, ;
             llCDPerL, llBomVarnt, llOpenOrd , llBrowse, llZoom, cbSetAsDef, ;
             llMulCurr, llEditExRt, llBulk, llInsur, llNewLine, llContinue, ;
             llQtyPSize, llAddLine, llReorder, llFromBulk

*C102118,1 Define new variables [Begin]
*-- lcOrdLin2 is used only while saving.
STORE .F. TO llScope, llNewTeam, llCtrlSKey
STORE '' TO lcSocTemp, lcOrdLin2, m.cStyGroup 
*C102118,1 Define new variables [End]
             
STORE '' TO laCodes, laTerms, laShipVia, laSpcInst, laSeasons, laDivision, laCanReason,;
            laVariables, lcOrderBr, laWareHouses, lcTranBr, lcTranBr1, lcTranBr2,;
            lcOldCustPo, laOCtgCd, lcOldValue, lcWinCh1, lcWinCh2, lcWinCh3, lcWinCh30,;
            lcWinCh31, lcWinCh32, lcWinCh321, lcWinCh322, lcWinCh33, lcWinCh34, lcWinCh4,;
            lcOrderFile, lcTranFile, lcStyHdr, lcEmptySty, lcT_BomVar, lcBaseFild, ;
            lcBaseTit, lcGlSales, lcLastStyle, lcFiles, lclastOrder, lcLastLine, ;
            lcDShpName , lcDShpAdd1, lcDShpAdd2, lcDShpAdd3, lcDShpAdd4, lcDShpAdd5,;
            lcShipName, lcShipAdd1, lcShipAdd2, lcShipAdd3, lcShipAdd4, lcShipAdd5,;
            lcBillName, lcBillAdd1, lcBillAdd2, lcBillAdd3, lcBillAdd4, lcBillAdd5,;
            lcDBllName, lcDBllAdd1, lcDBllAdd2, lcDBllAdd3, lcDBllAdd4, lcDBllAdd5

STORE ' ' TO lcOrdBrow, lcBrTtl1, lcBrTtl2, lcBrowseTl, lcBrTtlM, lcGlYear, lcGlPeriod, ;
             lcODefSes, lcODefDiv, lcPriceLvl, llAlowNew, lcFolder, lcSession, lcScFields, ;
             lcScrMode, laBrowArr, lcCurrOrd, lcOrdLine, lcOrdHdr, lcPackHdr, lcTempline, ;
             lcSelLines, lcOrdCanLn, lcStores, lcStores1, lcOrdSS, lcOrdTS, lcShipSS, ;
             lcShipTS, lcBrTtlT, lcBooked, lcBookSS, lcBookTS, lcDepleted, lcBulkOrd, ;
             lcAlocated, lcODefWare

STORE 0   TO lnMarker, lnTopMark, lnBotMark, lnTMarker, lnStyLngth, lnDisc_Pcnt,;
             lnBookAmt, lnBalance, lnCartons, lnLines, lnTLines, lnSouComm1, ;
             lnSouComm2, lnActFolder, lnLastMode

*C102118,1 Change Sort by Variable to be always 3 (Sort by Store). [Begin]
*STORE 1  TO lnOrdStatus, lnSortBy, lnOrdTrans, lnShipAddr, lnWareHouse, lnMajorLen, ;
*            lnTerms,lnShipVia,lnSpcInst,lnSeason,lnDivision,lnCanReason,lnOCtgCd 
STORE 1  TO lnOrdStatus, lnOrdTrans, lnShipAddr, lnWareHouse, lnMajorLen, ;
            lnTerms,lnShipVia,lnSpcInst,lnSeason,lnDivision,lnCanReason,lnOCtgCd
lnSortBy = 3             
*C102118,1 Change Sort by Variable to be always 3 (Sort by Store). [Begin]

*C102118,1 Define the Variables used in the Global screen. [Begin]
DECLARE laYes_No[2,2] , laInk[2,2]
STORE ''  TO laYes_No , laInkCol , lcGTypeSeq , lcGLogoName , lcInkColor ,;
             lcGLogo , lcSpInst , lcDiv , lcTeam , lcWind_tl

*B606908,1 Define this variable with 40 char. [Begin]             
lcGLogoName = SPACE(40)
*B606908,1 Define this variable with 40 char. [End]

STORE 0 TO lnPieces , lnPkAmount , lnYouthQty , lnYouthPr , lnAdultQty , lnAdultPr           
STORE 1 TO ibGSponsor , ibGInkColor
*C102118,1 Define the Variables used in the Global screen. [End]

lcLastStore = SPACE(8)
lcForKey    = '.T.'
lcDelMesag  = 'cancel'

*C102118,1 SSE Commented out because it is Removed from screen [Begin]
*lcConfig    = "\<Configure"
*C102118,1 SSE Commented out because it is Removed from screen [End]

*C102118,1 Define these variables here instead of lfvStyle [Begin]
lcMaj      = gfItemMask('PM')             && Get the major of the style
lnMajSize  = LEN(lcMaj)                   && Length of the major
lnItemSz   = gfItemMask('PI')
llExtSS    = gfGetMemVar('M_USEEXSSC')
*C102118,1 Define these variables here instead of lfvStyle [End]

lcBackOrd   = "No "

*C102118,1 Always Browse lcOrdLine file in all screen modes. [Begin]
*lcOrderFile = 'ORDLINE'
lcOrderFile = lcOrdLine
*C102118,1 Always Browse lcOrdLine file in all screen modes. [End]

lnSessNo    = gnProgCopy
ldDefOrdDat = {}

laOrdStatus[1] = 'Bid      '
laOrdStatus[2] = 'Open     '
laOrdStatus[3] = 'Hold     '
laOrdStatus[4] = 'Cancelled'
laOrdStatus[5] = 'Complete '

laKeyField[1,1] = 'lcOrdType'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'ORDHDR'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[1]'
laKeyField[2,2] = .T.
laKeyField[2,3] = 'ORDHDR'
laKeyField[2,4] = 2

laDefProc[7]  = .F.     && Cancel/Uncancel procedure(lpDelScr)
laDefProc[9]  = .F.     && Save procedure(lpSavScr)
laDefProc[10] = .F.     && close procedure(lpClsScr)

IF !gfSetup()
  RETURN
ENDIF

IF TYPE('lcOrderNo') = 'C' .AND. SEEK(lcOrdType+lcOrderNo,'ORDHDR')
  SELECT OrdHdr
  lcOldOrd = ORDER()
  SET ORDER TO Ordacct
  PRIVATE lcTempFlt
  lcTempFlt = OrdHdr.Account
  SET FILTER TO account+cordtype+order = lcTempFlt+lcOrdType
  SET ORDER TO &lcOldOrd
  LOCATE
ENDIF
lcProgID   = 'SOSOC10'
llReBrowse = .T.
lcOrdBrow  = IIF(lcOrdType='C','Contract Lines','Order Lines')
lcBrTtlZ   = IIF(lcOrdType='C','Zoom Contract Lines','Zoom Order Lines')
lcBrTtlB   = 'Booked Quantities'
lcBrTtlS   = 'Shipped Quantities'
lcBrTtlP   = 'Production Quantities'
lcBrTtlD   = 'Depleted Quantities'
lcBrTtlK   = 'Bulk order details'
lcBrTtlW   = "Work Order"
lcBrTtlC   = 'Canceled Lines'
lcSize1    = 'Size1'
lcSize2    = 'Size2'
lcSize3    = 'Size3'
lcSize4    = 'Size4'
lcSize5    = 'Size5'
lcSize6    = 'Size6'
lcSize7    = 'Size7'
lcSize8    = 'Size8'

laFolders[1,1] = 'Header'
laFolders[1,2] = 1
laFolders[2,1] = 'Details'
laFolders[2,2] = 2
laFolders[3,1] = 'Summary'
laFolders[3,2] = 3
lnnofld = 3

lcwfoldchng = '=lfActFolder()'  && function to control shows after change the folder
lnfoldercend = 102.00
lnfolderrend = 2
llNoShow = .F.

llPODsPrc = .F.   				
llMFDsPrc = .F.					
llUpdAlo  = .F.
IF 'PO' $ gcCmpModules     && if module PO is installed
  llPODsPrc = gfGetMemVar('M_PoDspPrc')  && setting in Po Module
ENDIF
IF 'MF' $ gcCmpModules	   && if module MF is installed
  llMFDsPrc = gfGetMemVar('M_MFDspPrc')  
ENDIF
llStyMark  = gfGetMemVar('M_stymark')   ='T'	&& setting in MF Module

IF !WEXIST(gcBaseWind)

  =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
  SELECT cDesc,cWareCode FROM WAREHOUS INTO ARRAY laWareHouses
  =gfCloseFile('WAREHOUS')

  STORE 0 TO lnLastMode
  lcBaseFild = lcBrFields
  lcBaseTit  = lcFile_Ttl
  lcEmptySty = PADR(STRTRAN(GFITEMMASK("PI"),'X',' '),19)
  lcStyHdr   = gfItemMask("HI")
  lnStyLngth = LEN(lcStyHdr)        && Var. hold the length of the style.

  lcScFields = 'ORDER,ACCOUNT,STORE,CUSTPO,STATUS,MULTI,MULTIPO,ENTERED,START,'+;
               'COMPLETE,cTermCode,SHIPVIA,SPCINST,SEASON,cDivision,DISC,DEPT,'+;
               'NOTE1,NOTE2,BUYER,PHONE,CINSUR,BULK,CREORDER,PRIORITY,CFACCODE,'+;
               'REP1,COMM1,REP2,COMM2,CWARECODE,LINK_CODE,CCURRCODE,NEXRATE,BOOK,BOOKAMT,'+;
               'SHIP,SHIPAMT,CANCEL,CANCELAMT,OPEN,OPENAMT,CFROMORDER,'+;
               'CANCELLED,DECL_DATE,DECL_CODE,CCANCRESON,APPROVAL,APPRAMT,'+;
               'NCURRUNIT,Alt_ShpTo,CORDERCAT,GL_SALES,INT_VEND,EVENT_COD,'+;
               'BILLNO,MERC_TYPE,BLANK_ORD,DISTRB_NO,CCLASS'

  SELECT OrdHdr
  SCATTER FIELDS &lcScFields TO laData BLANK
  lcSession = gfsequence('CSESSION')

  llAlowNew = lcOrdType <> 'T'
  
  laSetups[1,1]  = 'M_PACK'           && System has been steup to use packs
  laSetups[2,1]  = 'M_STY_COM'        && Edit sales reps commissions at style level
  laSetups[3,1]  = 'M_OR_NOTE'        && Edit order lines notepad
  laSetups[4,1]  = 'M_LINK_GL'        && System has been linked to GL
  laSetups[5,1]  = 'M_WareHouse'      && System has been steup to use multiple warehouses
  laSetups[6,1]  = 'M_GenOrNum'       && Generate order number manually
  laSetups[7,1]  = 'M_HOLD_ORD'       && Default new order status to hold
  laSetups[8,1]  = 'M_CANAFTER'       && Number of days need to calculate order completion date
  laSetups[9,1]  = 'M_COMPDATE'       && Number of days added to order entered date to get the
                                      && date after which decrease order quqntity considered 
                                      && as cancellation 
  laSetups[10,1] = 'M_DIV_LINK'       && Link code at division level
  laSetups[11,1] = 'M_REP_COMM'       && Sales rep commision at division level
  laSetups[12,1] = 'M_CRDT_LMT'       && Warn if customer is above credit lemit 
  laSetups[13,1] = 'M_lImpCost'       && Use detailed costing for imported styles
  laSetups[14,1] = 'M_DYELOT'         && System has been setup to use dyelots
  
  =gfGetMemVar(@laSetups,gcAct_Comp)
  llMulCurr   = gfGetMemVar('llMulCurr',gcAct_Comp)
  llEditExRt  = gfGetMemVar('LLEDITEXRA',gcAct_Comp)
  llBomVarnt = gfGetMemVar('M_BOMVAR' , gcAct_Comp)
  lnMajorLen = LEN(gfItemMask("PM"))

  lcWinCh1    = gfTempName()
  lcWinCh2    = gfTempName()
  lcWinCh3    = gfTempName()
  lcWinCh30   = gfTempName()
  lcWinCh31   = gfTempName()
  lcWinCh32   = gfTempName()
  lcWinCh321  = gfTempName()
  lcWinCh322  = gfTempName()
  lcWinCh33   = gfTempName()
  lcWinCh34   = gfTempName()
  lcWinCh4    = gfTempName()
  lcFolder    = gfTempName()      && Folder Window Name
  lcfoldprnt  = gcBaseWind        && window parent name for the folder
  lnactfolder = 1                 && active folder
  
  laFoldWinds[1,1] = laFolders[1,1]
  laFoldWinds[1,2] = lcWinCh2
  laFoldWinds[2,1] = laFolders[2,1]
  laFoldWinds[2,2] = lcWinCh3
  laFoldWinds[3,1] = laFolders[3,1]
  laFoldWinds[3,2] = lcWinCh4

  lcOrdHdr   = gfTempName()
  lcOrdLine  = gfTempName()
  lcTempline = gfTempName()
  lcPackHdr  = gfTempName()
  lcSelLines = gfTempName()
  lcStores   = gfTempName()
  lcStores1  = gfTempName()
  lcOrdSS    = gfTempName()
  lcOrdTS    = gfTempName()
  lcShipSS   = gfTempName()
  lcShipTS   = gfTempName()
  lcDepleted = gfTempName()
  lcBulkOrd  = gfTempName()
  lcBooked   = gfTempName()
  lcBookSS   = gfTempName()
  lcBookTS   = gfTempName()
  lcAlocated = gfTempName()
  lcT_BomVar = gfTempName()
  lcOrdCanLn = gfTempName()

  *C102118,1 Add Soccer, lcOrdLin2 Temp files. [Begin]
  lcSocTemp  = gfTempName()
  lcOrdLin2  = gfTempName()  
  *C102118,1 Add Soccer, lcOrdLin2 Temp files. [Begin]
  
  SELECT OrdLine
  SCATTER MEMVAR BLANK MEMO
  IF TYPE('lcOrderNo') = 'C' .AND. SEEK(lcOrdType+lcOrderNo,'ORDHDR')
    SELECT ORDHDR
    STORE .F. TO laScrMode
    STORE .T. TO laScrMode[2]
  ENDIF
  laVariables[1] = 'ldDefOrdDat'
  laVariables[2] = 'lcODefSes'  
  laVariables[3] = 'lcODefDiv'
  laVariables[4] = 'lcODefWare'
  laVariables[5] = 'lcScrMode'
  laVariables[6] = 'lcCurrOrd'
    
  laCodes[1,1] = 'CTERMCODE'
  laCodes[1,2] = 'laTerms'
  laCodes[1,3] = 'lnTerms'
  laCodes[1,4] = ''
  laCodes[1,5] = .F.
  laCodes[1,6] = .F.
  laCodes[1,10] = 'cTermCode'

  laCodes[2,1] = 'SHIPVIA'
  laCodes[2,2] = 'laShipVia'
  laCodes[2,3] = 'lnShipVia'
  laCodes[2,4] = ''
  laCodes[2,5] = .F.
  laCodes[2,6] = .F.
  laCodes[2,10] = 'SHIPVIA'

  laCodes[3,1] = 'SPCINST'
  laCodes[3,2] = 'laSpcInst'
  laCodes[3,3] = 'lnSpcInst'
  laCodes[3,4] = ''
  laCodes[3,5] = .F.
  laCodes[3,6] = .F.
  laCodes[3,10] = 'SPCINST'

  laCodes[4,1] = 'SEASON'
  laCodes[4,2] = 'laSeasons'
  laCodes[4,3] = 'lnSeason'
  laCodes[4,4] = ''
  laCodes[4,5] = .T.
  laCodes[4,6] = .F.
  laCodes[4,10] = 'SEASON'

  laCodes[5,1] = 'CDIVISION'
  laCodes[5,2] = 'laDivision'
  laCodes[5,3] = 'lnDivision'
  laCodes[5,4] = ''
  laCodes[5,5] = .F.
  laCodes[5,6] = .F.
  laCodes[5,10] = 'cDivision'

  laDRltFld[1,1] = PADR('LINK_CODE',10)
  laDRltFld[1,2] = 'laData[32]'
  laDRltFld[2,1] = 'CSLSGLLINK'
  laDRltFld[2,2] = 'laData[53]' 
   
  laDisRltFld[1,1] = 'DISCPCNT'
  laDisRltFld[1,2] = 'lnDisc_Pcnt'

  laCodes[6,1] = 'CCANCRESON'
  laCodes[6,2] = 'laCanReason'
  laCodes[6,3] = 'lnCanReason'
  laCodes[6,4] = ''
  laCodes[6,5] = .F.
  laCodes[6,6] = .F.
  laCodes[6,10] = 'CCANCRESON'

  laCodes[7,1] = 'CORDERCAT'
  laCodes[7,2] = 'laOCtgCd'
  laCodes[7,3] = 'lnOCtgCd'
  laCodes[7,4] = ''
  laCodes[7,5] = .F.
  laCodes[7,6] = .F.
  laCodes[7,10] = 'CORDERCAT'
  
  STORE 'lcOrdType+laData[1]' TO ;
  laCodes[1,9],laCodes[2,9],laCodes[3,9],laCodes[4,9],laCodes[5,9],laCodes[7,9]
  llCDPerL = gfGetmEMvAR('M_CMPDOLN')
  llESTYPSO = gfGetMemVar('M_ESTYPSO') 
ELSE
  IF !laScrMode[1]
    SELECT STYLE
    SET RELATION TO 'S'+SCALE INTO SCALE
  ENDIF
  SELECT IIF((laScrMode[3] .OR. laScrMode[4]),lcOrdLine,'OrdLine')
  SCATTER MEMVAR BLANK MEMO
ENDIF

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

lcKeyBmp   = gcBmpHome + "ExtKey.BMP"
lcNew      = gcBmpHome + "new1.bmp"
lctemplate = gcBmpHome + "Template.BMP"
lcPacks    = gcBmpHome + "packs.bmp"
lcRemove   = gcBmpHome + "rem1.bmp"
lcNote     = gcBmpHome + "Notes1.bmp"
lcClose    = gcBmpHome + "Close2.bmp"
lcCopy     = gcBmpHome + "copy.bmp"
lcPaste    = gcBmpHome + "past.bmp"
lcCancel   = gcBmpHome + "Trash.bmp"
lcUnCancel = gcBmpHome + "unTrash.bmp"
lcCan      = gcBmpHome + "Can.bmp"
lcOk       = gcBmpHome + "Ok.bmp"
lcBPopBmp  = gcBmpHome + "dn3.bmp"

*C102118,4 Add 2 new Bmp for Scope, Update Push buttons. [Begin]
lcScope    = gcBmpHome + "Scope.bmp"
lcUpdate   = gcBmpHome + "Generat.bmp"
*C102118,4 Add 2 new Bmp for Scope, Update Push buttons. [End]

DECLARE laPanelObj[1,3]
STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbObjLink'
laPanelObj[1,2] = gcBmpHome+"Relate.bmp"
laPanelObj[1,3] = [VALID lfvObjLink() MESSAGE 'Object Link' DISABLE]

SELECT ORDHDR
IF TYPE('lcOrderNo') <> 'C'
  SET FILTER TO cOrdType+Order=lcOrdType
ENDIF
DEFINE BAR 9  OF P03PU03 PROMPT '\<Edit' SKIP FOR !(laScrMode[2] .AND. INLIST(laData[5],'O','H'))

*C102118,5 Disable CTRL + S key [Begin]
ON KEY LABEL CTRL+S lnDummy = 1
*C102118,5 Disable CTRL + S key [End]

DO (gcScrDir+gcWinAppl+"\SOSOC10.SPX")
=lfClearKey()
DEFINE BAR 9  OF P03PU03 PROMPT '\<Edit' SKIP FOR .T.
DEFINE BAR 10 OF P03PU03 PROMPT '\<Delete' SKIP FOR .T.

IF WEXIST(lcOrdBrow)
  HIDE WINDOW (lcOrdBrow) SAME
  RELEASE WINDOW (lcOrdBrow)
ENDIF
IF WEXIST(lcBrTtlZ)
  HIDE WINDOW (lcBrTtlZ) SAME
  RELEASE WINDOW (lcBrTtlZ)
ENDIF
IF WEXIST(lcBrTtlT)
  HIDE WINDOW (lcBrTtlT) SAME
  RELEASE WINDOW (lcBrTtlT)
ENDIF
IF glQuitting
  IF USED(lcOrdHdr)
    USE IN (lcOrdHdr)
  ENDIF
  ERASE (gcWorkDir+lcOrdHdr+".DBF")
  ERASE (gcWorkDir+lcOrdHdr+".CDX")
  IF USED(lcOrdLine)
    USE IN (lcOrdLine)
  ENDIF
  ERASE (gcWorkDir+lcOrdLine+".DBF")
  ERASE (gcWorkDir+lcOrdLine+".CDX")
  ERASE (gcWorkDir+lcOrdLine+".FPT")
  IF USED(lcTempline)
    USE IN (lcTempline)
  ENDIF
  ERASE (gcWorkDir+lcTempline+".DBF")
  ERASE (gcWorkDir+lcTempline+".CDX")
  ERASE (gcWorkDir+lcTempline+".FPT")
  IF USED(lcSelLines)
    USE IN (lcSelLines)
  ENDIF
  ERASE (gcWorkDir+lcSelLines+".DBF")
  ERASE (gcWorkDir+lcSelLines+".CDX")
  IF USED(lcStores)
    USE IN (lcStores)
  ENDIF
  ERASE (gcWorkDir+lcStores+".DBF")
  ERASE (gcWorkDir+lcStores+".CDX")
  IF USED(lcOrdSS)
    USE IN (lcOrdSS)
  ENDIF
  ERASE (gcWorkDir+lcOrdSS+".DBF")
  ERASE (gcWorkDir+lcOrdSS+".CDX")
  IF USED(lcOrdTS)
    USE IN (lcOrdTS)
  ENDIF
  ERASE (gcWorkDir+lcOrdTS+".DBF")
  ERASE (gcWorkDir+lcOrdTS+".CDX")
  IF USED(lcShipSS)
    USE IN (lcShipSS)
  ENDIF
  ERASE (gcWorkDir+lcShipSS+".DBF")
  ERASE (gcWorkDir+lcShipSS+".CDX")
  IF USED(lcShipTS)
    USE IN (lcShipTS)
  ENDIF
  ERASE (gcWorkDir+lcShipTS+".DBF")
  ERASE (gcWorkDir+lcShipTS+".CDX")
  IF USED(lcDepleted)
    USE IN (lcDepleted)
  ENDIF
  ERASE (gcWorkDir+lcDepleted+".DBF")
  ERASE (gcWorkDir+lcDepleted+".CDX")
  IF USED(lcBulkOrd)
    USE IN (lcBulkOrd)
  ENDIF
  ERASE (gcWorkDir+lcBulkOrd+".DBF")
  ERASE (gcWorkDir+lcBulkOrd+".CDX")
  IF USED(lcBooked)
    USE IN (lcBooked)
  ENDIF
  ERASE (gcWorkDir+lcBooked+".DBF")
  ERASE (gcWorkDir+lcBooked+".CDX")
  IF USED(lcBookSS)
    USE IN (lcBookSS)
  ENDIF
  ERASE (gcWorkDir+lcBookSS+".DBF")
  ERASE (gcWorkDir+lcBookSS+".CDX")
  IF USED(lcBookTS)
    USE IN (lcBookTS)
  ENDIF
  ERASE (gcWorkDir+lcBookTS+".DBF")
  ERASE (gcWorkDir+lcBookTS+".CDX")
  IF USED(lcAlocated)
    USE IN (lcAlocated)
  ENDIF
  ERASE (gcWorkDir+lcAlocated+".DBF")
  ERASE (gcWorkDir+lcAlocated+".CDX")
  IF USED(lcOrdCanLn)
    USE IN (lcOrdCanLn)
  ENDIF
  ERASE (gcWorkDir+lcOrdCanLn+".DBF")
  ERASE (gcWorkDir+lcOrdCanLn+".CDX")  
  IF USED(lcT_BomVar)
    USE IN (lcT_BomVar)
  ENDIF
  ERASE (gcWorkDir + lcT_BomVar + ".DBF")
  ERASE (gcWorkDir + lcT_BomVar + ".CDX")
  ERASE (gcWorkDir + lcT_BomVar + ".FPT")
  
  *C102118,1 Close Soccer, lcOrdLin2 Temp files. [Begin]
  IF USED(lcSocTemp)
    USE IN (lcSocTemp)
  ENDIF
  ERASE (gcWorkDir+lcSocTemp+".DBF")
  ERASE (gcWorkDir+lcSocTemp+".CDX")
  IF USED(lcOrdLin2)
    USE IN (lcOrdLin2)
  ENDIF
  ERASE (gcWorkDir+lcOrdLin2+".DBF")
  ERASE (gcWorkDir+lcOrdLin2+".CDX")
  ERASE (gcWorkDir+lcOrdLin2+".FPT")
  *C102118,1 Close Soccer, lcOrdLin2 Temp files. [End]
ENDIF

SELECT OrdLine
SET RELATION TO
SELECT ORDHDR
SET RELATION TO
SET FILTER TO
RELEASE PAD _INQUIRY OF _MSYSMENU

*!*******************************************************************************
*! Name      : lfActPad
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Bulid a new menu pad [Options]
*!*******************************************************************************
*! Calls     : lpvInquiry
*!*******************************************************************************
*! Passed Parameters  : None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfActPad()
*!*******************************************************************************
*
FUNCTION lfActPad

DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
SET SKIP OF PAD _INQUIRY OF _MSYSMENU (laScrMode[1])
ON PAD _INQUIRY OF _msysmenu ACTIVATE POPUP _INQURYPOP

DEFINE POPUP _INQURYPOP MARGIN SHADOW
DEFINE BAR 1  OF _INQURYPOP PROMPT '\<Order Notes'      SKIP FOR laScrMode[1] .OR. laScrMode[4]
DEFINE BAR 2  OF _INQURYPOP PROMPT 'Ob\<ject Link'      SKIP FOR laScrMode[1] .OR. laScrMode[4]
DEFINE BAR 3  OF _INQURYPOP PROMPT 'Customer \<Inquire' SKIP FOR EMPTY(laData[2])
DEFINE BAR 4  OF _INQURYPOP PROMPT 'Customer \<Notepad' SKIP FOR EMPTY(laData[2])
DEFINE BAR 5  OF _INQURYPOP PROMPT '\<Aging Summary'    SKIP FOR EMPTY(laData[2])

*C102118,1 SSE Commented out all the rest of Menu Bar are not used anymore. [Begin]
*DEFINE BAR 6  OF _INQURYPOP PROMPT '\<Style Inquire'    SKIP FOR (lnActFolder<>2) .OR. EMPTY(Style) .OR. (lnSortBy=3 .AND. !llDetails)
*DEFINE BAR 7  OF _INQURYPOP PROMPT 'Sort By Lin\<e#'    SKIP FOR (lnActFolder<>2) .OR. !llMultiSt
*DEFINE BAR 8  OF _INQURYPOP PROMPT 'Sort BY St\<yle'    SKIP FOR (lnActFolder<>2) .OR. !llMultiSt
*DEFINE BAR 9  OF _INQURYPOP PROMPT 'Sort By Sto\<re'    SKIP FOR (lnActFolder<>2) .OR. !llMultiSt 
*DEFINE BAR 10 OF _INQURYPOP PROMPT 'Show \<Quantity per Size' SKIP FOR (lnActFolder<>2)
*DEFINE BAR 11 OF _INQURYPOP PROMPT 'Show Su\<mmary'     SKIP FOR (lnActFolder<>2) .OR. !llMultiSt .OR. (lnSortBy=1) OR INLIST(lnOrdTrans,4,5,6,7)
*DEFINE BAR 12 OF _INQURYPOP PROMPT 'Order \<Transactions' SKIP FOR (lnActFolder<>2) .OR. llZoom .OR. lcOrdType<>'O'
*DEFINE BAR 13 OF _INQURYPOP PROMPT '\<Line Notepad'       SKIP FOR (lnActFolder<>2) .OR. laSetups[3,2]<>'Y' .OR. EMPTY(m.Style) .OR. !llDetails
*DEFINE BAR 14 OF _INQURYPOP PROMPT '\<Copy Line'          SKIP FOR (lnActFolder<>2) .OR. (laScrMode[2]) .OR. EMPTY(m.Style) .OR. llZoom
*DEFINE BAR 15 OF _INQURYPOP PROMPT '\<Paste Line'         SKIP FOR (lnActFolder<>2) .OR. (laScrMode[2]) .OR. EMPTY(m.Style) .OR. llZoom
*DEFINE BAR 16 OF _INQURYPOP PROMPT '\<Zoom Order Lines'   SKIP FOR (lnActFolder<>2)
*SET MARK OF BAR 10 OF _INQURYPOP TO llQtyPSize
*SET MARK OF BAR 16 OF _INQURYPOP TO llZoom
*SET MARK OF BAR lnSortBy+6 OF _INQURYPOP TO .T.
*C102118,1 SSE Commented out all the rest of Menu Bar are not used anymore. [End]

ON SELECTION POPUP _INQURYPOP DO lpvInquiry

*C102118,1 SSE Commented out all the rest of Menu Bar are not used anymore. [Begin]
*ON BAR 12 OF _INQURYPOP ACTIVATE POPUP _ORDTRANS
*DEFINE POPUP _ORDTRANS MARGIN SHADOW
*DEFINE BAR 1 OF _ORDTRANS PROMPT '\<Open' 
*DEFINE BAR 2 OF _ORDTRANS PROMPT '\<Booked'     SKIP FOR (!laScrMode[2])
*DEFINE BAR 3 OF _ORDTRANS PROMPT '\<Shipped'    SKIP FOR (!laScrMode[2]) OR !('AR' $ gcCmpModules)
*DEFINE BAR 4 OF _ORDTRANS PROMPT '\<Production' SKIP FOR !(laScrMode[2]  AND llDetails AND ('PO' $ gcCmpModules .OR. 'MF' $ gcCmpModules)) 
*DEFINE BAR 5 OF _ORDTRANS PROMPT '\<Depletion'  SKIP FOR !(laScrMode[2]  AND llDetails AND laData[23]='Y')
*DEFINE BAR 6 OF _ORDTRANS PROMPT 'Bul\<k order' SKIP FOR !(laScrMode[4]  AND llDetails AND !EMPTY(laData[43]))
*DEFINE BAR 7 OF _ORDTRANS PROMPT '\<Canceled'   SKIP FOR !(laScrMode[2]  AND llDetails)
*DEFINE BAR 8 OF _ORDTRANS PROMPT '\<Work Order' SKIP FOR !(laScrMode[2]  AND llDetails)
*SET MARK OF BAR lnOrdTrans OF _ORDTRANS TO .T.
*ON SELECTION POPUP _ORDTRANS DO lpvOrdTrans
*C102118,1 SSE Commented out all the rest of Menu Bar are not used anymore. [End]
*-- End of lfActPad.

*!*******************************************************************************
*! Name      : lpvInquiry
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Bulid a new menu pad [Options]
*!*******************************************************************************
*! Calls     : gpDoProg,NOTEPAD,gfActWind,lfRefresh,lfBrowDet,lfvLineNote
*!             lfvCopyQty,lfvPasteQty,lfvBrowZoom
*!*******************************************************************************
*! Passed Parameters  : None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lpvInquiry()
*!*******************************************************************************
*
FUNCTION lpvInquiry
ACTIVATE WINDOW (lcFolder)
DO CASE
  CASE BAR() = 1      && Order Notes
  	=lfvOrdNote()
  CASE BAR() = 2      && Object Link
    =lfvObjLink()
  CASE BAR() = 3      && Customer Inquire
  	lcParameter = "'"+laData[2]+"'"+IIF(EMPTY(laData[3]),'',",'"+laData[3]+"'")
    DO gpDoProg WITH 'AWRARCUST',.F.,'AR',lcParameter
  CASE BAR() = 4      && Customer Notepad
    =NOTEPAD('A',laData[2])  
  CASE BAR() = 5      && Customer Credit
    =SEEK('M'+ladata[2],'Customer')
    DO (gcScrDir+gcWinAppl+"\SOAGING.SPX")

*C102118,1 SSE Commented out all these bars are not used anymore. [Begin]  
*  CASE BAR() = 6      && Style Inquire
*  	lcParameter = "'"+m.Style+"',''"
*    DO gpDoProg WITH 'AWRICSTYLE',.F.,'IC',lcParameter
*  CASE BAR() = 7      && Sort by line#
*    lnSortBy = 1
*    llDetails =.T.
*    SET MARK OF BAR 7  OF _INQURYPOP TO .T.  
*    SET MARK OF BAR 8  OF _INQURYPOP TO .F.
*    SET MARK OF BAR 9  OF _INQURYPOP TO .F.
*    SET MARK OF BAR 11 OF _INQURYPOP TO .F.
*    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
*  CASE BAR() = 8      && Sort by Style
*    lnSortBy = 2
*    SET MARK OF BAR 7 OF _INQURYPOP TO .F.  
*    SET MARK OF BAR 8 OF _INQURYPOP TO .T.
*    SET MARK OF BAR 9 OF _INQURYPOP TO .F.
*    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
*  CASE BAR() = 9      && Sort by Store
*    lnSortBy = 3
*    SET MARK OF BAR 7 OF _INQURYPOP TO .F.
*    SET MARK OF BAR 8 OF _INQURYPOP TO .F.
*    SET MARK OF BAR 9 OF _INQURYPOP TO .T.
*    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
*  CASE BAR() = 10      && Show Quantity per Size
*    llQtyPSize = !llQtyPSize
*    SET MARK OF BAR 10 OF _INQURYPOP TO llQtyPSize
*    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
*  CASE BAR() = 11      && Show Summary
*    llDetails = !llDetails
*    =lfActFolder()
*    SET MARK OF BAR 11 OF _INQURYPOP TO !llDetails
*    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
*  CASE BAR() = 13      &&  Order Line Notepad
*    =lfvLineNote()
*  CASE BAR() = 14      &&  Copy Order Line
*   =lfvCopyQty()
*  CASE BAR() = 15      &&  Paste Order LIne
*   =lfvPasteQty()
*  CASE BAR() = 16     &&   Zoom Order Browse
*    llZoom = !llZoom
*    =lfvBrowZoom()
*    SET MARK OF BAR 16 OF _INQURYPOP TO llZoom
*  CASE BAR() = 17
*C102118,1 SSE Commented out all these bars are not used anymore. [End]

ENDCASE
*-- End of lpvInquiry.

*!*******************************************************************************
*! Name      : lfvBrowZoom
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Zoom Invoice lines browse
*!*******************************************************************************
*! Calls     : lfBrowDet
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvBrowZoom()
*!*******************************************************************************
*C102118,1 This function is not used anymore.
FUNCTION lfvBrowZoom

IF llZoom 
  =lfBrowDet(.F.,'O')
ELSE
  llReBrowse = .T.
  =lfActFolder()
ENDIF
*-- End of lfvBrowZoom.

*!*******************************************************************************
*! Name      : lpvOrdTrans
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Order transactions
*!*******************************************************************************
*! Calls     : lfvOrdTrans
*!*******************************************************************************
*! Passed Parameters  : None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lpvOrdTrans()
*!*******************************************************************************
*C102118,1 This function is not used anymore.
FUNCTION lpvOrdTrans

IF BAR() = lnOrdTrans
  RETURN
ENDIF  
IF WEXIST(lcBrTtlT)
  HIDE WINDOW (lcBrTtlT) SAME
  RELEASE WINDOW (lcBrTtlT)
ENDIF

DO CASE
  CASE lnOrdTrans= 3
    =gfCloseFile('InvLine')
    =gfCloseFile('consinvl')
    =gfCloseFile('INVHDR')
  CASE lnOrdTrans= 4
    IF 'PO' $ gcCmpModules
      =gfCloseFile('POSHDR')
      =gfCloseFile('POSLN')
    ENDIF
    IF 'MF' $ gcCmpModules
      =gfCloseFile('CUTTKTH')
      =gfCloseFile('CUTTKTL')
    ENDIF
    =gfCloseFile('CUTPICK')
  CASE lnOrdTrans= 7
    =gfCloseFile('ORDCANLN')
ENDCASE

SET MARK OF BAR lnOrdTrans OF _ORDTRANS TO .F.
lnOrdTrans = BAR()
SET MARK OF BAR lnOrdTrans OF _ORDTRANS TO .T.
SET SKIP OF BAR 6 OF _INQURYPOP (lnOrdTrans =4)
SET SKIP OF BAR 7 OF _INQURYPOP (lnOrdTrans =4)
=lfvOrdTrans()
*-- End of lpvOrdTrans.

*!*******************************************************************************
*! Name      : lfvOrdTrans
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Browse transactions
*!*******************************************************************************
*! Calls     : lfBrowDet
*!*******************************************************************************
*! Passed Parameters  : None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvOrdTrans()
*!*******************************************************************************
*C102118,1 This function is not used anymore.
FUNCTION lfvOrdTrans

IF lnOrdTrans = 1
  =lfPrepBrow() 
  
  SHOW WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322)  TOP
  =lfActFolder()
ELSE
  IF lnOrdTrans = 8
    =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
  ENDIF
  ACTIVATE WINDOW (lcFolder)
  HIDE WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322)  SAME
  SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
  =lfBrowDet(.T.,'T')
ENDIF
*-- End of lfvOrdTrans.

*!*******************************************************************************
*! Name      : lfClearKey
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Clear key
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfClearKey()
*!*******************************************************************************
*
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
FOR lnChrToTrap = 32 TO 126
  ON KEY LABEL (CHR(lnChrToTrap))
ENDFOR
*-- End of lfClearKey.

*!*******************************************************************************
*! Name      : lfReadAct
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : READ Activate function of SOSOC10
*!*******************************************************************************
*! Calls     : lfClearKey.
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfReadAct()
*!*******************************************************************************
*
FUNCTION lfReadAct

IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)
IF WONTOP() <> lcWinCh1 .AND. WONTOP() <> lcFolder .AND. WONTOP() <> 'GWCCONTRL1' .AND. ;
   laScrMode[4] .AND. EMPTY(laData[2])
  _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF
*-- End of lfReadAct.

*!*******************************************************************************
*! Name      : lfBrowDet
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Order lines
*!*******************************************************************************
*! Calls     : lfPrepBrow,lfwBrow
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None
*!*******************************************************************************
*! Example   :  =lfBrowDet()
*!*******************************************************************************
*
FUNCTION lfBrowDet
PARAMETERS llRebuild, lcBrow

=IIF(llRebuild,lfPrepBrow(),.T.)
IF WEXIST(lcBrTtlZ)
  HIDE WINDOW (lcBrTtlZ) SAME
  RELEASE WINDOW (lcBrTtlZ)
ENDIF
IF WEXIST(lcBrTtlT)
  HIDE WINDOW (lcBrTtlT) SAME
  RELEASE WINDOW (lcBrTtlT)
ENDIF
lcKey=lcOrdType+laData[1]
IF lcBrow $ 'OB'
  SELECT (lcOrderFile)
  =SEEK(lcOrdType+laData[1])
  lnMarker = RECNO()
  IF WEXIST(lcOrdBrow)
    HIDE WINDOW (lcOrdBrow) SAME
    RELEASE WINDOW (lcOrdBrow)
  ENDIF
  
  *C102118,1 SSE Commented out llZoom not used anymore. [Begin]
  *IF llZoom
  *  HIDE WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322) SAME
  *  SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
  *  SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
  *  SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
  *  BROWSE FIELDS ;
  *         cMarker =IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F., &lcOrderBr;
  *         WINDOW (lcWinCh33)  ;
  *         IN WINDOW (lcWinCh3);
  *         NOMENU             ;         
  *         NOAPPEND           ;
  *         NODELETE           ;         
  *         NOWAIT             ;
  *         SAVE               ;
  *         NOCLEAR            ;
  *         KEY lcKey          ;
  *         WHEN lfwBrow()     ;
  *         VALID :F lfvBrows();
  *         TITLE lcBrTtlZ 
  *ELSE
  *  IF lnOrdTrans = 1
  *    SHOW WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322) TOP
  *  ENDIF
  *  BROWSE FIELDS ;
  *         cMarker =IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F., &lcOrderBr;
  *         WINDOW (lcWinCh31)  ;
  *         IN WINDOW (lcWinCh3);
  *         NOMENU            ;         
  *         NOAPPEND          ;
  *         NODELETE          ;         
  *         NOWAIT            ;
  *         SAVE              ;
  *	        NOCLEAR           ;
  *	        KEY lcKey         ;
  *         WHEN lfwBrow()    ;
  *         VALID :F lfvBrows()  ;
  *         TITLE lcOrdBrow
  *ENDIF
  SHOW WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322) TOP
  BROWSE FIELDS ;
         cMarker =IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F., &lcOrderBr;
         WINDOW (lcWinCh31)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
	     NOCLEAR           ;
         KEY lcKey ;
         WHEN lfwBrow()    ;
         VALID :F lfvBrows()  ;
         FOR Store = IIF(EMPTY(lcDiv),SPACE(4),lcDiv) + IIF(EMPTY(lcTeam),SPACE(4),lcTeam);
         TITLE lcOrdBrow
  *C102118,1 SSE Commented out llZoom not used anymore. [End]

  =lfwBrow()
ENDIF  

*C102118,1 SSE Commented out lcBrow always 'O'. [Begin]
*IF lcBrow $ 'TB'
*  IF lnOrdTrans = 8
*    SELECT (lcTranFile)
*    lnTMarker = RECNO(lcTranFile)
*    BROWSE FIELDS ;
*           cMarker =IIF(RECNO()=lnTMarker,'>',' '):H=' ':R:1:W=.F., &lcTranBr;
*           WINDOW (lcWinCh34)  ;
*           IN WINDOW (lcWinCh3);
*           NOMENU           ;         
*           NOAPPEND         ;
*           NODELETE         ;         
*           NOWAIT           ;
*           SAVE             ;
*           NOCLEAR          ;
*           KEY "1"+&lcOrderFile..Order+STR(&lcOrderFile..LineNo,6) , "3"+&lcOrderFile..Order+STR(&lcOrderFile..LineNo,6) ;
*           FOR Order = &lcOrderFile..Order .AND. cOrdLine = STR(&lcOrderFile..LineNo,6) ;
*           WHEN lfwBrowT()  ;
*           VALID :F lfvBrows()  ;
*           TITLE lcBrTtlT
*  ELSE
*    lcKey    = EVAL(RELATION(1,lcOrderFile))
*    SELECT (lcTranFile)
*    =SEEK(lcKey)
*    lnTMarker = RECNO(lcTranFile)
*    BROWSE FIELDS ;
*           cMarker =IIF(RECNO()=lnTMarker,'>',' '):H=' ':R:1:W=.F., &lcTranBr;
*           WINDOW (lcWinCh34)  ;
*           IN WINDOW (lcWinCh3);
*           NOMENU           ;         
*           NOAPPEND         ;
*           NODELETE         ;         
*           NOWAIT           ;
*           SAVE             ;
*           NOCLEAR          ;
*           KEY lcKey        ;
*           FOR EVAL(lcForKey);
*           WHEN lfwBrowT()  ;
*           VALID :F lfvBrows()  ;
*           TITLE lcBrTtlT
*  ENDIF
*ENDIF
*C102118,1 SSE Commented out lnOrdTrans always equal to 1. [End]
*-- End of lfBrowDet.

*!*******************************************************************************
*! Name      : lfwBrow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show line details
*!*******************************************************************************
*! Calls     : lfBrowDet
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None
*!*******************************************************************************
*! Example   :  =lfwBrow()
*!*******************************************************************************
*
FUNCTION lfwBrow

SELECT (lcOrderFile)

*C102118,1 SSE Commented out because it is Removed from screen [Begin]
*IF llBomVarnt
*  lcConfig = IIF(SEEK("SO" + &lcOrderFile..Order + STR(&lcOrderFile..LineNo,6) , lcT_BomVar) , "\<Adornment" , "\<Configure")
*ENDIF
*C102118,1 SSE Commented out because it is Removed from screen [End]

lnMarker = RECNO()
*C102118,1 SSE Commented out llZoom not used anymore. [Begin]
*IF lnOrdTrans = 1
*  IF llZoom
*    SHOW WINDOW (lcBrTtlZ) REFRESH SAME
*  ELSE  
*    SHOW WINDOW (lcOrdBrow) REFRESH SAME
*    SELECT (lcOrderFile)
*    SCATTER MEMVAR MEMO
*    SHOW GETS WINDOW (lcWinCh32)  ONLY
*    SHOW GETS WINDOW (lcWinCh321) ONLY
*    SHOW GETS WINDOW (lcWinCh322) ONLY
*    =lfRefresh(lcWinCh32)
*    =lfRefresh(lcWinCh321)
*  ENDIF
*ELSE
*  IF lnOrdTrans= 4 .AND. Style.Make <> llDomestic
*    llDomestic = Style.Make
*    lcTranBr   = IIF(Style.Make,lcTranBr1,lcTranBr2)
*    =lfBrowDet(.F.,'T')
*    ACTIVATE WINDOW (lcOrdBrow)
*  ENDIF 
*  IF llZoom
*    SHOW WINDOW (lcBrTtlZ) REFRESH SAME
*  ELSE  
*    SHOW WINDOW (lcOrdBrow) REFRESH SAME
*  ENDIF
*ENDIF
*IF lnOrdTrans = 8
*  =lfBrowDet(.F.,'T')
*  ACTIVATE WINDOW (lcOrdBrow)
*ENDIF
*C102118,1 SSE Commented out llZoom not used anymore. [End]

*C102118,4 Enable lcWinCh32 screen if either Div, Team is empty. [Begin]
SHOW WINDOW (lcOrdBrow) REFRESH SAME
SELECT (lcOrderFile)
SCATTER MEMVAR MEMO
SHOW GETS WINDOW (lcWinCh32)  ONLY
IF EMPTY(lcDiv) OR EMPTY(lcTeam)
  SHOW GETS WINDOW (lcWinCh32) ONLY
ELSE
  SHOW GET lcDiv  DISABLE
  SHOW GET lcTeam DISABLE  
ENDIF
*C102118,4 Enable lcWinCh32 screen if either Div, Team is empty. [End]

SHOW GETS WINDOW (lcWinCh321) ONLY
SHOW GETS WINDOW (lcWinCh322) ONLY

*C102118,4 Enable Scope push button in View/Edit mode. [Begin]
IF laScrMode[2] OR laScrMode[3]
  SHOW GET PbScope ENABLE
ELSE
  SHOW GET PbScope DISABLE
ENDIF

IF RECCOUNT(lcOrdLine) > 0 
  IF &lcOrdLine..cStyGroup = "JY" OR &lcOrdLine..cStyGroup = "SH" OR &lcOrdLine..cStyGroup = "SO"
    SHOW GET m.Price DISABLE
  ELSE
    SHOW GET m.Price ENABLE
  ENDIF
ELSE
  SHOW GET m.Price DISABLE
ENDIF
*C102118,4 Enable Scope push button in View/Edit mode. [End]

=lfRefresh(lcWinCh32)
=lfRefresh(lcWinCh321)

*C102118,1 SSE Commented out because it is Removed from screen [Begin]
*SHOW GET pbConfig,1 PROMPT lcConfig 
*C102118,1 SSE Commented out because it is Removed from screen [End]
*-- End of lfwBrow.

*!*******************************************************************************
*! Name      : lfwBrowT
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show line details
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None
*!*******************************************************************************
*! Example   :  =lfwBrowT()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfwBrowT
lnTMarker = RECNO(lcTranFile)
SHOW WINDOW (lcBrTtlT) REFRESH SAME
*-- End of lfwBrowT

*!*******************************************************************************
*! Name      : lfDOrdScr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : READ Deactivate function of screen SOSOC10
*!*******************************************************************************
*! Calls     : lpTab
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  .f.
*!*******************************************************************************
*! Example   :  =lfDOrdScr()
*!*******************************************************************************
*
FUNCTION lfDOrdScr

IF (laScrMode[3] OR laScrMode[4]) AND WLAST()=(lcWinCh321) AND !lfChkTotQty(.F.)
  ACTIVATE WINDOW (lcWinCh321)
  RETURN
ENDIF

SET SKIP OF PAD _INQUIRY OF _MSYSMENU laScrMode[1]
IF WONTOP()=lcOrdBrow .OR. WONTOP()=lcBrTtlB .OR. WONTOP()=lcBrTtlS .OR. ;
   WONTOP()=lcBrTtlD .OR. WONTOP()=lcBrTtlP .OR. WONTOP()=lcBrTtlZ .OR. ;
   WONTOP()=lcBrTtlK OR WONTOP()=lcBrTtlC
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME LOCATE
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  ON KEY LABEL TAB DO lpTab WITH ;
  IIF(WONTOP()=lcOrdBrow,IIF(lnOrdTrans=1,lcWinCh32,lcWinCh30),IIF(WONTOP()=lcBrTtlZ,'gwcContrl1','gwcContrl1')),;
  IIF(WONTOP()=lcOrdBrow,IIF(lnOrdTrans=1,'m.Store','ibBrowTrn'),IIF(WONTOP()=lcBrTtlZ,'pbTop','pbTop'))

  ON KEY LABEL BACKTAB DO lpBackTab WITH ;
  IIF(WONTOP()=lcOrdBrow .OR. WONTOP()=lcBrTtlZ,lcFolder,lcWinCh30),;
  IIF(WONTOP()=lcOrdBrow .OR. WONTOP()=lcBrTtlZ,'ibFolder[2]','ibBrowOrd')
ELSE
  glFromBrow = .F.
ENDIF
IF (laScrMode[3] .OR. laScrMode[4]) .AND. llNewLine AND ;
   !(WONTOP()=(lcWinCh32) OR WONTOP()=(lcWinCh321) OR WONTOP()=(lcWinCh322))
  SELECT (lcOrdLine)
  SCATTER MEMVAR MEMO
  STORE .F.       TO llAddLine,llNewLine
  STORE lcEmptySty TO lcLastStyle
  STORE SPACE(08)  TO lcLastStore
  IF EMPTY(m.Style)
    SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
    SHOW GET pbNew   ENABLE

    *C102118,1 SSE Commented out because they are Removed from screen [Begin]
    *SHOW GET pbPacks ENABLE
    *IF llMultiSt
    *  SHOW GET pbTemplate ENABLE
    *ENDIF
    *C102118,1 SSE Commented out because they are Removed from screen [End]
  ELSE
    SHOW GETS WINDOW (lcWinCh32)  ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) ENABLE ONLY
    
    *C102118,1 Enable Scope in View , Edit mode. [Begin]
    IF laScrMode[2] OR laScrMode[3]
      SHOW GET PbScope ENABLE
    ELSE
      SHOW GET PbScope DISABLE
    ENDIF
    *C102118,1 Enable Scope in View , Edit mode. [End]

    *C102118,1 SSE Commented out because they are Removed from screen [Begin]
    *IF !llMultiSt
    *  SHOW GET pbTemplate DISABLE
    *  SHOW GET ibLStore   DISABLE
    *  SHOW GET m.Store    DISABLE      
    *ENDIF
    *C102118,1 SSE Commented out because they are Removed from screen [End]      
    
    IF lContract
      SHOW GET m.Price      DISABLE
  
      *C102118,1 SSE Commented out because they are Removed from screen [Begin]
      *SHOW GET m.Gros_Price DISABLE
      *SHOW GET m.Disc_Pcnt  DISABLE
      *C102118,1 SSE Commented out because they are Removed from screen [End]
    ENDIF
    
    *C102118,1 SSE Enable/Disable Printing, Tracking, Invoice Push buttons. [Begin]
    DO CASE
      CASE laScrMode[2]
        SHOW GET PbInvoice ENABLE
        SHOW GET PbPrint   ENABLE
        SHOW GET PbDTrack  ENABLE
      CASE laScrMode[3]
        SHOW GET PbInvoice DISABLE
        SHOW GET PbPrint   ENABLE
        SHOW GET PbDTrack  ENABLE
      CASE laScrMode[4]
        SHOW GET PbInvoice DISABLE
        SHOW GET PbDTrack  DISABLE
        IF !EMPTY(lcDiv) AND !EMPTY(lcTeam)
          SHOW GET PbPrint ENABLE
        ELSE
          SHOW GET PbPrint DISABLE
        ENDIF  
    ENDCASE
    *C102118,1 SSE Enable/Disable Printing, Tracking, Invoice Push buttons. [End]
    
  ENDIF
ENDIF
*-- End of lfDOrdScr.

*!*******************************************************************************
*! Name      : lpTab
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Trap of tab key.
*!*******************************************************************************
*! Calls     : None.
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  DO lpTab WITH 'lcWinCh32', 'm.Store',.T.
*!*******************************************************************************
*
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL TAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF
*-- End of lpTab.

*!*******************************************************************************
*! Name      : lpBackTab
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Trap of tab key.
*!*******************************************************************************
*! Calls     : None.
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  DO lpBackTab WITH 'lcWinCh32', 'm.Store',.T.
*!*******************************************************************************
*
PROCEDURE lpBackTab
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF
*-- End of lpBackTab.

*!*******************************************************************************
*! Name      : lfBrowMulti
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Browse function for order template screen (SOMULTI)
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfBrowMulti()
*!*******************************************************************************
*This function is not ued anymore
FUNCTION lfBrowMulti
PRIVATE lcFields

lcFields = [cMarker =IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F.,]+;
           [Flag :H=' ' :R,Style :H=lcStyHdr :R,Style.Desc :20 :H='Description':R,]+;
           [Group :H='Group' :R,Gros_Price :H='Gross Price' :R,Disc_Pcnt :H='Discount' :R,]+;
           [Price :H='Net Price' :R,TotQty :H='Pieces' :R,]+;
           [Amount  =Price*TotQty :P= '99999999999.99' :H='Amount']
           
*C102118,1 SSE Commented out because they don't have Bulk Orders [Begin]
*IF laScrMode[4] .AND. !EMPTY(laData[43])
*  lcFields = lcFields + [,&lcBulkOrd..TotBook :H= 'Booked',&lcBulkOrd..TotOpen :6:H= 'Open' ,]+;
*                        [&lcBulkOrd..TotUsed :6:H= 'Used',nBalance = MAX(&lcBulkOrd..TotOpen-&lcBulkOrd..TotUsed,0) :6:H= 'Balance']
*ENDIF
*C102118,1 SSE Commented out because they don't have Bulk Orders [End]

SELECT (lcTempline)
LOCATE
lnMarker = RECNO()
BROWSE FIELDS &lcFields ;
       WINDOW SOMULTI1  ;
       IN WINDOW SOMULTI;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       WHEN lfLineDet() ;
       TITLE lcBrTtlM 
*-- End of lfBrowMulti.

*!*******************************************************************************
*! Name      : lfDMulti
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : Deactivate function for Multi stores screens
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfDMulti()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfDMulti

IF (laScrMode[3] OR laScrMode[4]) AND WLAST()='SOMULTI4' AND !lfChkTotQty(.T.)
  ACTIVATE WINDOW 'SOMULTI4'
  _CUROBJ = OBJNUM(m.Qty1)
  RETURN .F.
ENDIF
IF WONTOP()=lcBrTtlM 
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME LOCATE
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB     DO lpTab WITH 'SOMULTI2','pbSelect'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'SOMULTI5','pbClose'
ENDIF
IF llNewLine AND  !(WONTOP()='SOMULTI3' OR WONTOP()='SOMULTI4' OR WONTOP()='SOMULTI5')
  SELECT (lcTempLine)
  SCATTER MEMVAR MEMO
  STORE .F.        TO llAddLine,llNewLine
  STORE lcEmptySty TO lcLastStyle
  IF EMPTY(m.Style)
    SHOW GETS WINDOW 'SOMULTI2' DISABLE ONLY
    SHOW GETS WINDOW 'SOMULTI3' DISABLE ONLY
    SHOW GETS WINDOW 'SOMULTI4' DISABLE ONLY
    SHOW GETS WINDOW 'SOMULTI5' DISABLE ONLY
    SHOW GET pbNew     ENABLE

    *C102118,1 SSE Commented out because it is Removed from screen [Begin]
    *SHOW GET pbPacks   ENABLE
    *C102118,1 SSE Commented out because it is Removed from screen [End]
    
    SHOW GET pbClose   ENABLE
    SHOW GET ibInv200E ENABLE
  ELSE
    SHOW GETS WINDOW 'SOMULTI2' ENABLE ONLY
    SHOW GETS WINDOW 'SOMULTI3' ENABLE ONLY
    SHOW GETS WINDOW 'SOMULTI4' ENABLE ONLY
    SHOW GETS WINDOW 'SOMULTI5' ENABLE ONLY
    IF lContract
      SHOW GET m.Price      DISABLE

      *C102118,1 SSE Commented out because they are Removed from screen [Begin]
      *SHOW GET m.Gros_Price DISABLE
      *SHOW GET m.Disc_Pcnt  DISABLE
      *C102118,1 SSE Commented out because they are Removed from screen [End]
    ENDIF
    =lfAdjButt('')
  ENDIF
ENDIF
RETURN .F.
*-- End of lfDMulti.

*!*******************************************************************************
*! Name      : lfLineDet
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show template line details
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None
*!*******************************************************************************
*! Example   :  =lfLineDet()
*!*******************************************************************************
*This function is not used anymore
FUNCTION lfLineDet

lnMarker = RECNO(lcTempLine)
SHOW WINDOW (lcBrTtlM ) REFRESH SAME
SCATTER MEMVAR MEMO
SHOW GETS WINDOW 'SOMULTI2' ONLY
SHOW GETS WINDOW 'SOMULTI3' ONLY
SHOW GETS WINDOW 'SOMULTI4' ONLY
SHOW GETS WINDOW 'SOMULTI5' ONLY
=lfRefresh('SOMULTI2')
=lfRefresh('SOMULTI3')
=lfRefresh('SOMULTI4')
IF Flag = SPACE(1)
  SHOW GET pbSelect,1 PROMPT '\<Select'
ELSE
  SHOW GET pbSelect,1 PROMPT 'Un\<Select'
ENDIF
IF !lContract
  SHOW GET m.Price      ENABLE

  *C102118,1 SSE Commented out because they are Removed from screen [Begin]
  *SHOW GET m.Gros_Price ENABLE
  *SHOW GET m.Disc_Pcnt  ENABLE
  *C102118,1 SSE Commented out because they are Removed from screen [End]
ELSE
  SHOW GET m.Price      DISABLE

  *C102118,1 SSE Commented out because they are Removed from screen [Begin]
  *SHOW GET m.Gros_Price DISABLE
  *SHOW GET m.Disc_Pcnt  DISABLE
  *C102118,1 SSE Commented out because they are Removed from screen [End]
ENDIF

*C102118,1 SSE Commented out because it is Removed from screen [Begin]
*IF !EMPTY(m.Prepak)
*  SHOW GET m.PPQty ENABLE
*ELSE
*  SHOW GET m.PPQty DISABLE
*ENDIF
*C102118,1 SSE Commented out because it is Removed from screen [End]
*-- End of lfLineDet.

*!*******************************************************************************
*! Name      : lpShow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : Show function
*!*******************************************************************************
*! Calls     : lfReStart,gfwCodePop,lfGetInfo
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lpShow()
*!*******************************************************************************
*
FUNCTION lpShow

lcBrFields = lcBaseFild
lcFile_Ttl = lcBaseTit
lcCanBar = "IIF(laData[5]='X','\<Uncancel','\<Cancel')"
DEFINE BAR 10 OF P03PU03 PROMPT &lcCanBar SKIP FOR !laScrMode[2]
IF llContinue .OR. (TYPE('lcOrderNo') = 'C' .AND. EMPTY(laData[1]) .AND. ;
                  SEEK(lcOrdType+lcOrderNo,'ORDHDR') AND laScrMode[2])
  SELECT IIF(laScrMode[2],'ORDHDR',lcOrdHdr)
  SCATTER FIELDS &lcScFields TO laData
ENDIF
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'Ccode_no','SH')

IF (laScrMode[2] AND lnLastMode <> 2) OR (laScrMode[4] AND lnLastMode <> 4)
  =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
  =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
  =gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH') 
  =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
  =gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')
  =IIF(laSetups[4,2]='Y',gfOpenFile(gcDataDir+'Gl_Link',gcDataDir+'Gl_Link1','SH'),.T.)
  SELECT STYLE
  SET RELATION TO 'S'+SCALE INTO SCALE
ENDIF
=IIF(llContinue,lfGetInfo(),.T.)

DO CASE
  CASE laScrMode[1]  .AND. lnLastMode <> 1
    lnLastMode = 1
    =lfClsFiles()
    =lfReStart()
    SHOW GET pbOrdNote DISABLE
    SHOW GET pbObjLink DISABLE
    SHOW GET pbDlt,1 DISABLE PROMPT lcCancel

    *C102118,1 SSE Commented out because it is Removed from screen [Begin]
    *-- We don't check for uncomplete session.
    *IF llGoAndChk AND lfChkUnComS()
    *  RETURN
    *ENDIF    
    *lcConfig = "\<Configure"
    *C102118,1 SSE Commented out because it is Removed from screen [End]
    
    lcBackOrd = "No "    
  CASE laScrMode[2]
    llGoAndChk = .T.
    lnLastMode = 2
    SHOW GET pbOrdNote ENABLE
    SHOW GET pbObjLink ENABLE

    IF INLIST(laData[5],'C','X')
      SHOW GET pbEdt DISABLE
    ELSE
      SHOW GET pbEdt ENABLE
    ENDIF
    STORE 'ORDHDR' TO laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],;
                      laCodes[3,7],laCodes[3,8],laCodes[4,7],laCodes[4,8],;
                      laCodes[5,7],laCodes[5,8],laCodes[7,7],laCodes[7,8]

    lcBackOrd = IIF(laData[5] = "O" .AND. laData[37] <> 0 , "Yes" , "No ")
    IF llBomVarnt
      SELECT * , "S" AS cStatus , RECNO() AS nRecNo ;
        FROM (gcDataDir + "BomVar") ;
       WHERE cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] ;
        INTO DBF (gcWorkDir + lcT_BomVar)
      INDEX ON cIdType+cCost_Id+STR(LineNo,6) TAG (lcT_BomVar)
    ENDIF
    
    =lfClsFiles()
    =lfGetInfo()

    *C102118,1 Change file to be lcOrdLine [Begin]    
    *SELECT OrdLine
    =lfCratTemp()
    SELECT (lcOrdLine)
    *C102118,1 Change file to be lcOrdLine [End]
    
    SET RELATION TO STYLE+laData[31]+DYELOT INTO STYDYE
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    =SEEK(lcOrdType+laData[1])
    laData[53] = IIF(!EMPTY(laData[53]),SUBSTR(laData[53],1,3),'DEF')

    *C102118,1 Fill lcOrdLine from Ordline file [Begin]
    DO lpGetOrdL
    *C102118,1 Fill lcOrdLine from Ordline file [End]

    SELECT ORDHDR
    SET RELATION TO 'M'+Account INTO CUSTOMER    
  CASE laScrMode[3]  .AND. lnLastMode <> 3  
    llGoAndChk = .T.
    lnLastMode = 3
    llCUpdate = .T.
    STORE 0 TO laQtyPaste
    lnLines = OrdHdr.LastLine

    *C102118,1 SSE Commented out all files are created in View Mode. [Begin]
    *=lfCratTemp()
    *C102118,1 SSE Commented out all files are created in View Mode. [End]
    
    lcBackOrd = IIF(laData[5] = "O" .AND. laData[37] <> 0 , "Yes" , "No ")
    IF llBomVarnt
      SELECT * , "S" AS cStatus , RECNO() AS nRecNo ;
        FROM (gcDataDir + "BomVar") ;
       WHERE cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] ;
        INTO DBF (gcWorkDir + lcT_BomVar)
      INDEX ON cIdType+cCost_Id+STR(LineNo,6) TAG (lcT_BomVar)
    ENDIF
    
    SELECT ORDHDR
    SCATTER MEMVAR
    INSERT INTO (lcOrdHdr) FROM MEMVAR
    SELECT ORDLINE
    =SEEK(lcOrdType+laData[1])

    *C102118,1 Change file to be lcOrdLine to fill lcOrdLine from Ordline file [Begin]    
    *SCAN REST WHILE cordtype+order+STR(lineno,6) = lcOrdType+laData[1]
    *  SCATTER MEMVAR MEMO
    *  INSERT INTO (lcOrdLine) FROM MEMVAR
    *ENDSCAN
    *C102118,1 Change file to be lcOrdLine to fill lcOrdLine from Ordline file [End]

    SELECT (lcOrdLine)
    SET RELATION TO STYLE+laData[31]+DYELOT INTO STYDYE
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    LOCATE
    SCATTER MEMVAR MEMO
    lcScrMode = 'E'
    
    *C102118,1 SSE Commented out we don't use Uncomplete session. [Begin]
    *SELECT 'UNCMSESS'
    *IF SEEK('I'+PADR("SOSOC10",10)+PADR(gcUser_id,10)) AND RLOCK()
    *  BLANK
    *ELSE
    *  APPEND BLANK
    *ENDIF
    *REPLACE Status     WITH 'O'       ,;
    *        cUTranType WITH lcProgID  ,;
    *        cUserId    WITH gcUser_id ,;
    *        cSession   WITH lcSession ,;
    *        cProgram   WITH 'SOSOC10'   ,;
    *        cCurrScr   WITH 'SOSOC10'   ,;
    *        dTranDate  WITH gdSysDate ,;
    *        cTranTime  WITH TIME()
    *=RLOCK()
    *lcFiles = 'lcOrdHdr,'+lcOrdHdr+','+lcOrdHdr+';'+;
    *          'lcOrdLine,'+lcOrdLine+',ORDLINE;'
    *lcFiles = lcFiles + 'lcOrdCanLn,'+lcOrdCanLn+','+lcOrdCanLn+';'
    *C102118,1 SSE Commented out we don't use Uncomplete session. [End]

    IF ORDHDR.TOTCUT > 0 
      IF !USED(lcAlocated)
        =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
        SELECT CutPick
        =AFIELDS(laFileStru)
        =gfCloseFile('CUTPICK')
        lnFileStru = ALEN(laFileStru,1)
        DIMENSION laFileStru[lnFileStru+2,4]
        laFileStru[lnFileStru+1,1] = 'nSteps'
        laFileStru[lnFileStru+1,2] = 'N'
        laFileStru[lnFileStru+1,3] = 2
        laFileStru[lnFileStru+1,4] = 0
        laFileStru[lnFileStru+2,1] = 'cUpdSizes'
        laFileStru[lnFileStru+2,2] = 'C'
        laFileStru[lnFileStru+2,3] = 8
        laFileStru[lnFileStru+2,4] = 0
        DECLARE laIndex[2,2]
        laIndex[1,1] = 'TRANCD+ORDER+CORDLINE'
        laIndex[1,2] = 'CUTORD'
        laIndex[2,1] = 'TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE'
        laIndex[2,2] = 'CUTPKORD'
        =gfCrtTmp(lcAlocated,@laFileStru,@laIndex)
      ENDIF
      lcFiles = lcFiles + 'lcAlocated,'+lcAlocated+',CUTPKORD;'
    ENDIF
    lcCurrOrd = laData[1]
    =gfSavSess(lcProgID, lcFiles, @laVariables,lcSession)
    STORE lcOrdHdr TO laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],;
                      laCodes[3,7],laCodes[3,8],laCodes[4,7],laCodes[4,8],;
                      laCodes[5,7],laCodes[5,8],laCodes[7,7],laCodes[7,8]
    IF lnOrdStatus=1
      DECLARE laOrdStatus[3]
      laOrdStatus[1] = 'Bid      '
      laOrdStatus[2] = 'Open     '
      laOrdStatus[3] = 'Hold     '
    ENDIF  
    llUpdBook = laData[5]='B' .OR. (gdSysDate <= (laData[8] + laSetups[8,2]))
    llReBrowse=.T.
  CASE laScrMode[4] 
    IF lnLastMode <> 4
      llGoAndChk = .T.
      STORE 0 TO laQtyPaste
      IF EMPTY(ldDefOrdDat)
        STORE '' TO lcODefSes,lcODefDiv
        STORE 0  TO lnSeason,lnDivision,lnWareHouse
        IF FILE("ORDDEF.MEM")
          RESTORE ADDITIVE FROM ("ORDDEF.MEM")
          STORE 1 TO lnSeason,lnDivision
          =gfwCodePop(@laCodes,'SEASON','L')
          =gfwCodePop(@laCodes,'CDIVISION','L')
          lnSeason   = ASCAN('laSeasons',lcODefSes)
          lnDivision = ASCAN('laDivision',lcODefDiv)
          lnWareHouse= ASCAN(laWareHouses,lcODefWare)
          lnSeason   = IIF(lnSeason=0,1,ASUBSCRIPT(laSeasons,lnSeason,1))
          lnDivision = IIF(lnDivision=0,1,ASUBSCRIPT(laDivision,lnDivision,1))
          lnWareHouse= IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
        ENDIF
        IF lnSeason = 0
          lnSeason = 1
          =gfwCodePop(@laCodes,'SEASON','D')
        ENDIF
        IF lnDivision = 0
          lnDivision = 1
          =gfwCodePop(@laCodes,'CDIVISION','D')
        ENDIF
        lnWareHouse = MAX(lnWareHouse,1)
        ldDefOrdDat = gdSysDate + laSetups[9,2]
        DO (gcScrDir+gcWinAppl+"\SoOrdDef.SPX")
      ENDIF
      STORE lcOrdHdr TO laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],;
                        laCodes[3,7],laCodes[3,8],laCodes[4,7],laCodes[4,8],;
                        laCodes[5,7],laCodes[5,8],laCodes[7,7],laCodes[7,8]
      =IIF(llContinue,.T.,lfCratTemp() AND lfReStart())
      DECLARE laOrdStatus[3]
      laOrdStatus[1] = 'Bid      '
      laOrdStatus[2] = 'Open     '
      laOrdStatus[3] = 'Hold     '
    ENDIF
    lnOCtgCd = 1
    =gfwCodePop(@laCodes,'CORDERCAT','D')
    SHOW GET pbOrdNote DISABLE
    SHOW GET pbObjLink DISABLE
    
    STORE 4 TO lnLastMode
    SELECT (lcOrdLine)
    SET RELATION TO STYLE+laData[31]+DYELOT INTO STYDYE
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    SCATTER MEMVAR MEMO
    SELECT (lcOrdHdr)
    SHOW GET lnOrdStatus
    IF !llContinue AND EMPTY(laData[2])
      =lfRefresh(lcWinCh2)
      SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh1)   DISABLE ONLY

      *C102118,1 Enable Department (League). [Begin]
      SHOW GET ibDepart ENABLE
      SHOW GET laData[17] ENABLE
      *C102118,1 Enable Department (League). [End]

      SHOW GET ibAccount ENABLE
      SHOW GET laData[2] ENABLE
      _CUROBJ = OBJNUM(laData[2])
    ENDIF
    
    *C102118,1 Initialize the Variables used in the Global screen. [Begin]    
    STORE ''  TO laYes_No , laInkCol , lcGTypeSeq , lcGLogoName , lcInkColor ,;
                 lcGLogo , lcSpInst , lcDiv , lcTeam

    *B606908,1 Define this variable with 40 char. [Begin]             
    lcGLogoName = SPACE(40)
    *B606908,1 Define this variable with 40 char. [End]
                 
    STORE 0 TO lnPieces , lnPkAmount , lnYouthQty , lnYouthPr , lnAdultQty ,;
               lnAdultPr
    STORE 1 TO ibGSponsor , ibGInkColor
    *C102118,1 Initialize the Variables used in the Global screen. [End]
ENDCASE 
llCntrExst = .T.

*C102118,1 SSE Commented out we don't use Uncomplete session. [Begin]
*=IIF(laScrMode[3] OR laScrMode[4],;
*     SEEK('O'+PADR(lcProgID,10)+gcUser_id+lcSession,'UNCMSESS'),.T.)
*C102118,1 SSE Commented out we don't use Uncomplete session. [End]
     
=lfActFolder()
*-- End of lpShow.

*!*******************************************************************************
*! Name      : lfActFolder
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Change folders
*!*******************************************************************************
*! Calls     : lfBrowDet
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  lfActFolder()
*!*******************************************************************************
*
FUNCTION lfActFolder

DO CASE
  CASE lnActFolder = 1
    ACTIVATE WINDOW (lcWinCh2)
    lcStatus = IIF(laScrMode[3] .OR. laScrMode[4],'ENABLE','DISABLE')
    SHOW GETS WINDOW (lcWinCh30)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh4)   DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh2) &lcStatus ONLY
    lcLines = IIF( (laScrMode[3] .OR. laScrMode[4]) .AND. EOF(lcOrdLine),'ENABLE','DISABLE')

    *-- Variable for Season Enable/Disable [Begin]
    *-- Enable the Season if the Season code is ALL and the detail line is empty.
    *-- Enable the Season if the Season code is not All regardless of the detail lines.

    *C102118,1 Adding 2 variable to Handle Tracking , Global Push Buttons mode [Begin]
    *PRIVATE lcSeasonEn
    PRIVATE lcSeasonEn , lcTrckEnbl , lcGlobEnbl
    lcTrckEnbl = IIF((laScrMode[2] .OR. laScrMode[3]),'ENABLE','DISABLE')
    lcGlobEnbl = IIF(laScrMode[4],IIF(EMPTY(laData[2]),'DISABLE','ENABLE'),'DISABLE')
    SHOW GET pbTracking &lcTrckEnbl
    SHOW GET pbGlobal &lcGlobEnbl
    *C102118,1 Adding 2 variable to Handle Tracking , Global Push Buttons mode [End]

    IF laScrMode[3] OR laScrMode[4]
      lcSeasonEn = IIF(laData[14] = "*",IIF(EOF(lcOrdLine),'ENABLE','DISABLE'),'ENABLE')
    ELSE
      lcSeasonEn = "DISABLE"
    ENDIF
    
    IF llMultiSt
      SHOW GET lnShipAddr DISABLE
    ENDIF
    IF lnShipAddr = 1
      SHOW GET lcShipName DISABLE
      SHOW GET lcShipAdd1 DISABLE
      SHOW GET lcShipAdd2 DISABLE
      SHOW GET lcShipAdd3 DISABLE
      SHOW GET lcShipAdd4 DISABLE
      SHOW GET lcShipAdd5 DISABLE
    ENDIF
    
    SHOW GET lnSeason &lcSeasonEn
    SHOW GET lnDivision  &lcLines
    SHOW GET lnWareHouse &lcLines
    SHOW GET ibCurrency  &lcLines
    SHOW GET laData[33]  &lcLines
    IF !llEditExRt OR laData[33]=gcBaseCurr
      SHOW GET laData[34] DISABLE
    ELSE
      SHOW GET laData[34] &lcLines
    ENDIF
    IF laScrMode[1]
      SHOW GET ibFolder[2] DISABLE
      SHOW GET ibFolder[3] DISABLE
      SHOW GET ibFolder[4] DISABLE
    ELSE
      SHOW GET ibFolder[2] ENABLE
      SHOW GET ibFolder[3] ENABLE
      SHOW GET ibFolder[4] ENABLE
    ENDIF  
  CASE lnActFolder = 2
    SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh30) ENABLE ONLY
    SELECT IIF(laScrMode[3] .OR. laScrMode[4],lcOrdLine,'OrdLine')

    =IIF(llReBrowse,lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B')),.T.)
    SHOW GET ibBrowOrd ENABLE
    SHOW GET ibBrowTrn ENABLE
    llReBrowse = .F.
    
    *C102118,1 SSE Commented out because they don't have Bulk Orders [Begin]
    *C102118,1     Also initialize llNewTeam with False
    llNewTeam = .F.
    *IF laScrMode[4] .AND. !EMPTY(laData[43]) .AND. !USED(lcBulkOrd)
    *  WAIT 'Copy bulk order lines... Please standby' WINDOW NOWAIT
    *  CREATE TABLE (gcWorkDir+lcBulkOrd) (Style C(19),LineNo N(6),;
    *          Book1 N(5),Book2 N(5),Book3 N(5),Book4 N(5),Book5 N(5),;
    *          Book6 N(5),Book7 N(5),Book8 N(5),TotBook N(6),;
    *          Open1 N(5),Open2 N(5),Open3 N(5),Open4 N(5),Open5 N(5),;
    *          Open6 N(5),Open7 N(5),Open8 N(5),TotOpen N(6),;
    *          Used1 N(5),Used2 N(5),Used3 N(5),Used4 N(5),Used5 N(5),;
    *          Used6 N(5),Used7 N(5),Used8 N(5),TotUsed N(6))
    *  =AFIELDS(laFileStru)
    *  =gfCrtTmp(lcBulkOrd,@laFileStru,[STR(LINENO,6)],lcBulkOrd)
    *  lcCopyToFile = IIF(llMultiSt,lcTempline,lcOrdLine)
    *  IF llMultiSt .AND. EOF(lcOrdLine)
    *    STORE 0 TO laData[35],laData[36],laData[41],laData[42]
    *  ENDIF      
    *  SELECT ORDLINE
    *  SET ORDER TO TAG ORDLINE IN ORDLINE
    *  IF SEEK(lcOrdType+laData[43])
    *    SCAN REST WHILE cOrdType+ORDER = lcOrdType+laData[43] FOR TOTQTY > 0
    *      SCATTER FIELDS EXCEPT INV*,PIK*,POALO*,CUT*,TOTPIK,TOT_POALO,;
    *                            TOTCUT,NPCK*,nPWght MEMVAR MEMO
    *      INSERT INTO (lcBulkOrd) ;
    *      (Style,LineNo,Book1,Book2,Book3,Book4,Book5,Book6,Book7,Book8,TotBook,;
    *       Open1,Open2,Open3,Open4,Open5,Open6,Open7,Open8,TotOpen) VALUES ;
    *      (m.Style,m.LineNo,m.Book1,m.Book2,m.Book3,m.Book4,m.Book5,;
    *       m.Book6,m.Book7,m.Book8,m.TotBook,m.Qty1,m.Qty2,m.Qty3,m.Qty4,;
    *       m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty)
    *      m.cFromOrder = m.Order
    *      m.BulkLineNo = m.LineNo
    *      m.Order      = SPACE(6)
    *      IF llMultiSt
    *        STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,;
    *                   m.Qty8,m.TotQty,m.Book1,m.Book2,m.Book3,m.Book4,;
    *                   m.Book5,m.Book6,m.Book7,m.Book8,m.TotBook
    *      ELSE
    *        m.Book1   = m.Qty1
    *        m.Book2   = m.Qty2
    *        m.Book3   = m.Qty3
    *        m.Book4   = m.Qty4
    *        m.Book5   = m.Qty5
    *        m.Book6   = m.Qty6
    *        m.Book7   = m.Qty7
    *        m.Book8   = m.Qty8
    *        m.TotBook = m.TotQty
    *        m.Flag    = 'N'
    *        m.Store   = laData[3]
    *        SELECT (lcBulkOrd)
    *        =RLOCK()
    *        REPLACE USED1 WITH m.Qty1 ,;
    *                USED2 WITH m.Qty2 ,;
    *                USED3 WITH m.Qty3 ,;
    *                USED4 WITH m.Qty4 ,;
    *                USED5 WITH m.Qty5 ,;
    *                USED6 WITH m.Qty6 ,;
    *                USED7 WITH m.Qty7 ,;
    *                USED8 WITH m.Qty8 ,;
    *                TOTUSED WITH m.TotQty
    *        UNLOCK
    *      ENDIF
    *      INSERT INTO (lcCopyToFile) FROM MEMVAR
    *    ENDSCAN
    *  ENDIF
    *  GO TOP IN (lcCopyToFile)
    *  WAIT CLEAR 
    *ENDIF
    *C102118,1 SSE Commented out because they don't have Bulk Orders [End]
  
    IF !llZoom
      SCATTER MEMVAR
      SHOW WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322) REFRESH SAME
      lcStatus = IIF((laScrMode[3] OR laScrMode[4]) AND !EOF(lcOrdLine) AND;
                      llDetails,'ENABLE','DISABLE')
      SHOW GETS WINDOW (lcWinCh32)  &lcStatus ONLY
      SHOW GETS WINDOW (lcWinCh321) &lcStatus ONLY
      SHOW GETS WINDOW (lcWinCh322) &lcStatus ONLY
      
      *C102118,1 Enable Scope in View , Edit mode. [Begin]
      IF laScrMode[2] OR laScrMode[3]
        SHOW GET PbScope ENABLE
      ELSE
        SHOW GET PbScope DISABLE
      ENDIF
      *C102118,1 Enable Scope in View , Edit mode. [End]
      
      IF (laScrMode[3] .OR. laScrMode[4]) AND llDetails
        SHOW GET pbNew   ENABLE 

        *C102118,1 SSE Commented out because it is Removed from screen [Begin]
        *SHOW GET pbPacks ENABLE
        *C102118,1 SSE Commented out because it is Removed from screen [End]
      ELSE
        SHOW GET pbNew   DISABLE
        
        *C102118,1 SSE Commented out because it is Removed from screen [Begin]
        *SHOW GET pbPacks DISABLE
        *C102118,1 SSE Commented out because it is Removed from screen [End]
        
      ENDIF
      
      *C102118,1 SSE Enable/Disable Printing, Tracking, Invoice Push buttons. [Begin]
      DO CASE
        CASE laScrMode[2]
          SHOW GET PbScope   ENABLE
          SHOW GET PbInvoice ENABLE
          SHOW GET PbPrint   ENABLE
          SHOW GET PbDTrack  ENABLE
        CASE laScrMode[3]
          SHOW GET PbScope   ENABLE
          SHOW GET PbInvoice DISABLE
          SHOW GET PbPrint   ENABLE
          SHOW GET PbDTrack  ENABLE
        CASE laScrMode[4]
          SHOW GET PbScope   DISABLE
          SHOW GET PbInvoice DISABLE
          SHOW GET PbDTrack  DISABLE
          IF !EMPTY(lcDiv) AND !EMPTY(lcTeam)
            SHOW GET PbPrint ENABLE
          ELSE
            SHOW GET PbPrint DISABLE
          ENDIF  
      ENDCASE
      *C102118,1 SSE Enable/Disable Printing, Tracking, Invoice Push buttons. [End]

      *C102118,1 SSE Commented out because they are Removed from screen [Begin]
      *IF (laScrMode[3] .OR. laScrMode[4]) .AND. llMultiSt AND llDetails
      *  SHOW GET pbTemplate ENABLE 
      *ELSE
      *  SHOW GET pbTemplate DISABLE
      *ENDIF      
      *IF (laScrMode[3] .OR. laScrMode[4]) .AND. llMultiSt .AND. ;
      *   !EOF(lcOrdLine) AND llDetails
      *  SHOW GET ibLStore ENABLE
      *  SHOW GET m.Store  ENABLE
      *ELSE
      *  SHOW GET ibLStore DISABLE
      *  SHOW GET m.Store  DISABLE
      *ENDIF
      *C102118,1 SSE Commented out because they are Removed from screen [End]
      
      IF (laScrMode[3] .OR. laScrMode[4]) .AND. !EMPTY(Style) AND llDetails
        SHOW GET pbRemove ENABLE
      ELSE
        SHOW GET pbRemove DISABLE
      ENDIF
    ENDIF
    IF (laScrMode[3] .OR. laScrMode[4]) .AND. !llESTYPSO
      *C102118,1 SSE Commented out because they are Removed from screen [Begin]
      *SHOW GET m.Gros_Price DISABLE
      *SHOW GET m.Disc_Pcnt DISABLE
      *C102118,1 SSE Commented out because they are Removed from screen [End]
      
      SHOW GET m.Price DISABLE
    ENDIF
    
    SELECT IIF(laScrMode[3] .OR. laScrMode[4],lcOrdLine,'OrdLine')
    =lfwBrow()
    _CUROBJ = IIF(!EMPTY(STYLE) .OR. llZoom,OBJNUM(ibBrowOrd),OBJNUM(pbNew))
  CASE lnActFolder = 3
    lcStatus = IIF(laScrMode[3] .OR. laScrMode[4],'ENABLE','DISABLE')
    SHOW GETS WINDOW (lcWinCh2)   DISABLE  ONLY 
    SHOW GETS WINDOW (lcWinCh30)  DISABLE  ONLY
    SHOW GETS WINDOW (lcWinCh32)  DISABLE  ONLY
    SHOW GETS WINDOW (lcWinCh321) DISABLE  ONLY
    SHOW GETS WINDOW (lcWinCh322) DISABLE  ONLY
    SHOW GETS WINDOW (lcWinCh4)  &lcStatus ONLY

    *C102118,1 SSE Commented out because they don't have Bulk Orders [Begin]
    *IF llMultiSt .OR. lcOrdType='C'
    *  SHOW GET llBulk DISABLE
    *ENDIF
    *C102118,1 SSE Commented out because they don't have Bulk Orders [End]

    lnSavAlias = SELECT (0)
    SELECT IIF(laScrMode[3] .OR. laScrMode[4],lcOrdhdr,'ORDHDR')
    IF !EMPTY(cfacCode)
      SHOW GET ibGlLink   DISABLE
      SHOW GET laData[32] DISABLE
    ELSE
      *AMH 09/06/2000 ENABLE ibGlLink in View or Select Mode [Start]
      *SHOW GET ibGlLink   ENABLE
      *SHOW GET laData[32] ENABLE
      IF laScrMode[3] .OR. laScrMode[4]
        SHOW GET ibGlLink   ENABLE
        SHOW GET laData[32] ENABLE
      ENDIF
      *AMH 09/06/2000 ENABLE ibGlLink in View or Select Mode [End  ]
    ENDIF
    SELECT(lnSavAlias)
    =lfRefresh(lcWinCh4)
ENDCASE
IF (laScrMode[4] AND !EMPTY(laData[2])).OR. (laScrMode[3] .AND. ORDHDR.STATUS='B')
  SHOW GET lnOrdStatus ENABLE
ELSE
  SHOW GET lnOrdStatus DISABLE
ENDIF  

*C102118,1 Commented out Soccer Select always Multi Store [Begin] 
*IF (laScrMode[3] .OR. laScrMode[4]) .AND. EOF(lcOrdLine) AND !EMPTY(laData[2])
*  SHOW GET llMultiSt ENABLE
*ELSE
*  SHOW GET llMultiSt DISABLE
*ENDIF
*C102118,1 Commented out Soccer Select always Multi Store [End] 

IF (laScrMode[3] .OR. laScrMode[4]) .AND. !llMultiSt AND !EMPTY(laData[2])
  SHOW GET ibStore   ENABLE
  SHOW GET laData[3] ENABLE
ELSE
  SHOW GET ibStore   DISABLE
  SHOW GET laData[3] DISABLE
ENDIF
IF (laScrMode[3] .OR. laScrMode[4]) .AND. EOF(lcOrdLine) .AND. llMultiSt AND !EMPTY(laData[2])
  SHOW GET laData[7] ENABLE
ELSE
  SHOW GET laData[7] DISABLE
ENDIF
IF laScrMode[1] OR ((laScrMode[3] .OR. laScrMode[4]) .AND. !laData[7] AND !EMPTY(laData[2]))
  SHOW GET laData[4] ENABLE
ELSE
  SHOW GET laData[4] DISABLE
ENDIF
RETURN
*-- End of lfActFolder.

*!*******************************************************************************
*! Name      : lfvDefaults
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate session defaults
*!*******************************************************************************
*! Calls     : CODECHK,gfBrowWare
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvDefaults()
*!*******************************************************************************
*
FUNCTION lfvDefaults

lcODefSes  = ALLTRIM(laSeasons[lnSeason,2])
lcODefDiv  = ALLTRIM(laDivision[lnDivision,2])
lcODefWare = laWareHouses[lnWareHouse,2]
IF EMPTY(ldDefOrdDat)
  *-- Message : 32022
  *-- Order copletion date cannot be empty.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('TRM32022B00000','ALERT',IIF(lcOrdType='C','Contract','Order'))
  _CUROBJ = OBJNUM(ldDefOrdDat)
  RETURN
ENDIF
IF gdSysDate > ldDefOrdDat
  *-- Message : 32013
  *-- The starting date cannot be greater than the completion date.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('TRM32013B00000','ALERT')
  ldDefOrdDat = gdSysDate + laSetups[9,2]
  _CUROBJ = OBJNUM(ldDefOrdDat)
  RETURN
ENDIF
IF  gcContCode # 'EGYPT'
  IF CDOW(ldDefOrdDat)='Saturday' .OR. CDOW(ldDefOrdDat)='Sunday'
    *-- Message : 32014
    *-- The completion date is a xxxxx.
    *-- Button : 00000
    *-- Ok
    =gfModalGen('INM32014B00000','ALERT',CDOW(ldDefOrdDat))
  ENDIF
ENDIF

IF cbSetAsDef
  SAVE ALL LIKE lcODef* TO ("ORDDEF.MEM")
ENDIF
CLEAR READ
*-- End of lfvDefaults.

*!*******************************************************************************
*! Name      : lpClsScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Cancel new Order
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lpClsScr()
*!*******************************************************************************
*
FUNCTION lpClsScr

*C102118,1 SSE Commented out we don't use Uncomplete session. [Begin]
*SELECT unCmSess
*REPLACE STATUS WITH 'I'
*UNLOCK
*C102118,1 SSE Commented out we don't use Uncomplete session. [End]

llContinue = .F.
llReBrowse =.T.
SELECT ORDHDR
IF laScrMode[3]
  SCATTER FIELDS &lcScFields TO laData
ENDIF
*-- End of lpClsScr.

*!*******************************************************************************
*! Name      : lfGetInfo
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Restore invoice information
*!*******************************************************************************
*! Calls     : gfGetAdr
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfGetInfo()
*!*******************************************************************************
*
FUNCTION lfGetInfo

DO lfGet_Info IN (gcAppHome + 'SO\SOSOCUPD.FXP')
*-- End of lfGetInfo.

*!*******************************************************************************
*! Name      : lfReStart
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : ReInitialize variables
*!*******************************************************************************
*! Calls     : gfwCodePop,lfchngfolder
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfReStart()
*!*******************************************************************************
*
FUNCTION lfReStart

*C102118,1 Always Browse lcOrdLine file in all screen modes. [Begin]
*lcOrderFile = 'ORDLINE'
lcOrderFile = lcOrdLine
*C102118,1 Always Browse lcOrdLine file in all screen modes. [End]

=gfwCodePop(@laCodes,'CTERMCODE','N')
=gfwCodePop(@laCodes,'SHIPVIA','N')
IF llMultiSt
  =lfChMSHV()
ENDIF

=gfwCodePop(@laCodes,'SPCINST','N')
=gfwCodePop(@laCodes,'SEASON','N')
=gfwCodePop(@laCodes,'CDIVISION','N')
=gfwCodePop(@laCodes,'CORDERCAT','N')
    
STORE 0 TO lnLines,lnTLines,lnSouComm1,lnSouComm2
STORE 1 TO lnOrdStatus,lnShipAddr
STORE lcEmptySty TO lcLastStyle
STORE SPACE(08)  TO lcLastStore
STORE '' TO  lcDShpName,lcDShpAdd1,lcDShpAdd2,lcDShpAdd3,lcDShpAdd4,lcDShpAdd5,;
             lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
             lcDBllName,lcDBllAdd1,lcDBllAdd2,lcDBllAdd3,lcDBllAdd4,lcDBllAdd5
STORE '' TO lcScrMode,lcFiles,lcCurrOrd

*C102118,1 Always Multi Store True and don't use the Zoom option [Begin] 
*STORE .F. TO llZoom,llMultiSt,llAddLine,llNewLine,llIgnorAll,llDomestic,llBulk,llReorder
STORE .F. TO llAddLine,llNewLine,llIgnorAll,llDomestic,llBulk,llReorder
*C102118,1 Always Multi Store True and don't use the Zoom option [End]

STORE .T. TO llReBrowse,llUpdBook
lcSize1   = 'Size1'
lcSize2   = 'Size2'
lcSize3   = 'Size3'
lcSize4   = 'Size4'
lcSize5   = 'Size5'
lcSize6   = 'Size6'
lcSize7   = 'Size7'
lcSize8   = 'Size8'

DECLARE laOrdStatus[5]
laOrdStatus[1] = 'Bid      '
laOrdStatus[2] = 'Open     '
laOrdStatus[3] = 'Hold     '
laOrdStatus[4] = 'Cancelled'
laOrdStatus[5] = 'Complete '
IF lnactfolder <> 1
  lnlastfold = lnactfolder
  lnactfolder=1
  =lfchngfolder(lnactfolder)
ENDIF
*-- End of lfReStart.

*!*******************************************************************************
*! Name      : lfClsFiles
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : Close opened temporary files
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfClsFiles()
*!*******************************************************************************
*
FUNCTION lfClsFiles

IF USED(lcBooked)
  USE IN (lcBooked)
ENDIF
IF USED(lcDepleted)
  USE IN (lcDepleted)
ENDIF
IF USED(lcOrdSS)
  USE IN (lcOrdSS)
ENDIF
IF USED(lcBookSS)
  USE IN (lcBookSS)
ENDIF
IF USED(lcShipSS)
  USE IN (lcShipSS)
ENDIF
IF USED(lcOrdTS)
  USE IN (lcOrdTS)
ENDIF
IF USED(lcBookTS)
  USE IN (lcBookTS)
ENDIF
IF USED(lcShipTS)
  USE IN (lcShipTS)
ENDIF
IF USED(lcAlocated)
  USE IN (lcAlocated)
ENDIF
IF USED(lcBulkOrd)
  USE IN (lcBulkOrd)
ENDIF
SELECT ORDHDR
*-- End of lfClsFiles.

*!*******************************************************************************
*! Name      : lfvOrdStatus
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Order status
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  .f.
*!*******************************************************************************
*! Example   :  =lfvOrdStatus()
*!*******************************************************************************
*
FUNCTION lfvOrdStatus

STORE SUBSTR('BOH',lnOrdStatus,1) TO laData[5]
SELECT (lcOrdHdr)
=RLOCK()
REPLACE STATUS WITH laData[5]
UNLOCK
llCUpDate = .T.
*-- End of lfvOrdStatus.

*!*******************************************************************************
*! Name      : lfvOrder
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Order number
*!*******************************************************************************
*! Calls     : lfOrdBrow,gfSeekRec
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  .f.
*!*******************************************************************************
*! Example   :  =lfvOrder()
*!*******************************************************************************
*
FUNCTION lfvOrder
PRIVATE lcOrder
PRIVATE lnAlias,lcOldOrdFlt
lnAlias = SELECT()
SELECT ORDHDR
lcOldOrdFlt = FILTER()
SET FILTER TO

IF llBrowse .OR. (!EMPTY(laData[1]) .AND. !SEEK(lcOrdType+laData[1],'ORDHDR'))
  lcOrder = laData[1]
  =lfOrdBrow(@lcOrder,laData[2],laData[3],laData[4])
  laData[1] = lcOrder 
  llBrowse = .F.
ENDIF

IF !EMPTY(laData[1])
  SELECT ORDHDR
  
  SET FILTER TO cOrdType+Order  = lcOrdType+Order
  =gfSeekRec()
ENDIF  
SELECT ORDHDR
SET FILTER TO &lcOldOrdFlt
SELECT (lnAlias)
*-- End of lfvOrder.

*!*******************************************************************************
*! Name      : lfOrdBrow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Invoice order
*!*******************************************************************************
*! Calls     : ARIABROW
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  .f.
*!*******************************************************************************
*! Example   :  =lfOrdBrow()
*!*******************************************************************************
*
FUNCTION lfOrdBrow
PARAMETERS lcOrder,lcAccount,lcStore,lcCustPo

SELECT ORDHDR
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER

*-- Adding the Browse in 2 lines because the compiler can't compile a line which is too long
*-- Note : No more fields can be added to the BROWSE or the browse will display an error message
*-- "a line is too long to be compiled"

DO lpDefnFlds               && Define lcBrFields

DO CASE
  CASE !EMPTY(lcOrder) .AND. SEEK(lcOrdType+lcOrder,'OrdHdr')
    RETURN
  CASE !EMPTY(lcAccount) .AND. EMPTY(lcCustPo)
    SET ORDER TO TAG 'OrdAcct' IN OrdHdr
    lcOrder = IIF(ARIABROW("lcAccount+lcOrdType"+' FOR STORE = IIF(EMPTY(lcStore),"",lcStore)',;
                  "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
                  OrdHdr.Order,SPACE(6))
    SET ORDER TO TAG ORDHDR IN OrdHdr

  CASE !EMPTY(lcAccount) .AND. !EMPTY(lcCustPo) .AND. EMPTY(lcStore)
    SET ORDER TO TAG 'OrdCust' IN OrdHdr
    PRIVATE lcCstPoExp
    lcCstPoExp = IIF(SEEK(lcAccount+UPPER(lcCustPo)+lcOrdType,'OrdHdr'),;
                     'Account + UPPER(CustPO) = lcAccount+UPPER(lcCustPo)','.T.')
    lcOrder = IIF((SEEK(lcAccount+UPPER(lcCustPo)+lcOrdType,'OrdHdr') AND lfGetNxtPo(OrdHdr.Order)) .OR. ;
              ARIABROW("lcAccount"+' FOR cOrdType=lcOrdType .AND. STORE = IIF(EMPTY(lcStore),"",lcStore);
              .AND. &lcCstPoExp',;
              "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
              OrdHdr.Order,SPACE(6))
    SET ORDER TO TAG ORDHDR IN OrdHdr

  CASE !EMPTY(lcAccount) .AND. !EMPTY(lcCustPo) .AND. !EMPTY(lcStore)
    SET ORDER TO TAG 'OrdCust' IN OrdHdr
    IF SEEK(lcAccount+UPPER(lcCustPo)+lcOrdType,'OrdHdr') .AND. Store = lcStore

      lcorder =IIF(lfGetNxtPo(OrdHdr.Order) .OR. ARIABROW("lcAccount"+' FOR cOrdType=lcOrdType .AND. STORE = lcStore .AND.Account + UPPER(CustPO) = lcAccount+UPPER(lcCustPo)',;
                   "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
                    OrdHdr.Order,SPACE(6))
    ELSE
      *-- Message : 30238
      *-- No orders matching the selected. 
      *-- Button  : 00000
      *-- Ok
      = gfModalGen('TRM32038B00000','DIALOG','orders matching the')
    ENDIF  
    SET ORDER TO TAG ORDHDR IN OrdHdr

  OTHERWISE
    SET ORDER TO TAG ORDHDR IN OrdHdr
    PRIVATE lcOldFilt
    lcOldFilt = FILTER()
    SET FILTER TO
    lcOrder = IIF(ARIABROW('lcOrdType',"Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
                  OrdHdr.Order,SPACE(6))
    SET FILTER TO &lcOldFilt
ENDCASE
SET RELATION OFF INTO CUSTOMER
*-- End of lfOrdBrow.

*!*******************************************************************************
*! Name      : lfGetNxtPo
*! Developer : Abdou ElGendi
*! Date      : 02/29/2000
*! Purpose   : Cheak IF The Account Have anthor Order With Same 
*!           : CustPo Number.
*!*******************************************************************************
*! Calls     : 
*!*******************************************************************************
*! Passed Parameters  : lcPreOrd
*!*******************************************************************************
*! Returns            : True .OR. False
*!*******************************************************************************
*
FUNCTION lfGetNxtPo
PARAMETER lcPreOrd
PRIVATE llToRet

LOCATE REST WHILE account+UPPER(custpo)+cordtype+order =;
                    lcAccount+UPPER(lcCustPo)+lcOrdType ;
              FOR ORDER <> lcPreOrd .AND. STORE = IIF(EMPTY(lcStore),"",lcStore) 
IF FOUND()
  llToRet = .F.
ELSE
  llToRet = .T.
  =SEEK(lcAccount+UPPER(lcCustPo)+lcOrdType)
ENDIF
RETURN llToRet
*-- End OF lfGetNxtPo.

*!*******************************************************************************
*! Name      : lfvDiscount
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate order discount
*!*******************************************************************************
*! Calls     : lfvCommision
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvDiscount()
*!*******************************************************************************
*
FUNCTION lfvDiscount
PARAMETERS llPercent

IF &lcOrdHdr..Disc = laData[16]
  RETURN
ENDIF
llCUpdate = .T.
SELECT (lcOrdHdr)
=RLOCK()
REPLACE Disc WITH laData[16]
UNLOCK
lnOldComm1 = laData[28]
lnOldComm2 = laData[30]
=lfvCommision()
*-- Message : 32047
*-- Would you like to overwrite order lines commissions with new commissions?
*-- Button : 32000
*-- Yes No
IF (lnOldComm1<>laData[28] .OR. lnOldComm2<>laData[30]) .AND. ;
   SEEK(lcOrdType+laData[1],lcOrdLine) .AND. ;
   (laSetups[2,2] <> 'Y' .OR. gfModalGen('QRM32047B32000','ALERT',;
   IIF(lcOrdType='C','contract','order'))=1)
  SELECT (lcOrdLine) 
  SCAN
    =RLOCK()
    REPLACE Comm1 WITH laData[28],;
            Comm2 WITH laData[30]
    UNLOCK
  ENDSCAN
  LOCATE 
  SELECT (lcOrdHdr)
ENDIF
*-- End of lfvDiscount

*!*******************************************************************************
*! Name      : lfvLinkCode
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate sales g.l. link code
*!*******************************************************************************
*! Calls     : gfGLBrowse
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvLinkCode()
*!*******************************************************************************
*
FUNCTION lfvLinkCode
PRIVATE lcLinkCode
IF llBrowse .OR. !SEEK('01'+laData[32],'Gl_Link')
  lcLinkCode = laData[32]
  =gfGLBrowse('01',@lcLinkCode)
  *laData[32] = lcLinkCode
  IF EMPTY(lcLinkCode) .AND. !SEEK('01'+laData[32],'Gl_Link')
    laData[32] = Link_Code
  ELSE
    IF !EMPTY(lcLinkCode)
      laData[32] = lcLinkCode
    ENDIF
  ENDIF
ENDIF
SELECT (lcOrdHdr)
=RLOCK()
REPLACE Link_Code WITH laData[32]
UNLOCK
=lfRefresh(lcWinCh4)
llBrowse = .F.
*-- End of lfvLinkCode.

*!*******************************************************************************
*! Name      : lfvSlsLink
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Order Sales link code
*!*******************************************************************************
*! Calls     : gfGLBrowse
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvSlsLink()
*!*******************************************************************************
*
FUNCTION lfvSlsLink
PRIVATE lcLinkCode

IF llBrowse .OR. !SEEK('02'+SUBSTR(laData[53],1,3),'Gl_Link')
  lcLinkCode = SUBSTR(laData[53],1,3)
  =gfGLBrowse('02',@lcLinkCode,'',1)
  laData[53] = lcLinkCode
ENDIF
=lfRefresh(lcWinCh4)
llBrowse = .F.
*-- End of lfvSlsLink.

*!*******************************************************************************
*! Name      : lfvFactor
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate factor
*!*******************************************************************************
*! Calls     : FacChk
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvFactor()
*!*******************************************************************************
*
FUNCTION lfvFactor
PRIVATE lcFacCode
lcFacCode = &lcOrdHdr..cFacCode

IF llBrowse .OR. (!EMPTY(laData[26]) .AND. !SEEK(laData[26],'SycFact'))
  lcBrFields  = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  SELECT SycFact
  laData[26]= IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,;
                  gnBrFSRow2, gnBrFSCol2,'','','cFacCode','laBrowArr'),;
                  SycFact.cFacCode,SPACE(5))
ENDIF
SELECT (lcOrdHdr)
=RLOCK()
REPLACE cFacCode WITH laData[26]
UNLOCK
*-- Read the GL link code of the factor while creating the sales order. 
*-- If there is a factor assgined to the current customer.
*-- we should read the GL link code of this factor insated of the 
*-- gl link code of this customer.  
*-- If the factor link code is empty we should not change the current sitation. [Begin]
IF !EMPTY(cFacCode)
  =gfOpenFile(gcDataDir+'Factor',gcDataDir+'Factor','SH')
  IF SEEK(&lcOrdHdr..cFacCode,'FACTOR') .AND. !EMPTY(Factor.Link_Code)
    laData[32] = Factor.Link_Code
  ELSE
    IF laSetups[10,2]='Y'
      =gfRltFld(laData[15],@laDRltFld,'CDIVISION')
      laData[53] = ALLTRIM(laData[53])
      laData[32] = ALLTRIM(laData[32])
    ELSE
      STORE Customer.cSlsGlLink TO laData[53]
    ENDIF
    IF EMPTY(laData[32])
      IF SEEK('M'+&lcOrdHdr..ACCOUNT,'CUSTOMER') .AND. !EMPTY(Customer.Link_Code)
        laData[32] = Customer.Link_Code
      ELSE
        laData[32] = 'DEFDEF'
      ENDIF
    ENDIF
  ENDIF
  =gfCloseFile('FACTOR')
 SELECT (lcOrdHdr)
  SHOW GET ibGlLink   DISABLE
  SHOW GET laData[32] DISABLE
ELSE
  IF laScrMode[3] .OR. laScrMode[4]
    SHOW GET ibGlLink   ENABLE
    SHOW GET laData[32] ENABLE
  ENDIF
  IF lcFacCode <> laData[26]
    IF laSetups[10,2]='Y'
      =gfRltFld(laData[15],@laDRltFld,'CDIVISION')
      laData[53] = ALLTRIM(laData[53])
      laData[32] = ALLTRIM(laData[32])
    ELSE
      STORE Customer.cSlsGlLink TO laData[53]
    ENDIF
    IF EMPTY(laData[32])
      IF SEEK('M'+&lcOrdHdr..ACCOUNT,'CUSTOMER') .AND. !EMPTY(Customer.Link_Code)
        laData[32] = Customer.Link_Code
      ELSE
        laData[32] = 'DEFDEF'
      ENDIF
    ENDIF
  ENDIF
ENDIF  
=RLOCK()
REPLACE Link_Code WITH laData[32]
UNLOCK

=lfRefresh(lcWinCh4)
llBrowse = .F.
*-- End of lfvFactor.

*!*******************************************************************************
*! Name      : lfvAccount
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Order Account
*!*******************************************************************************
*! Calls     : CUSBROWM,gfGetAdr,gfChkRate,gfModalGen,gfActWind,lfRefresh,ARIABROW
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  .f.
*!*******************************************************************************
*! Example   :  =lfvAccount()
*!*******************************************************************************
*
FUNCTION lfvAccount
PRIVATE xAccount

IF llBrowse .OR. (!EMPTY(laData[2]) .AND. !SEEK('M'+laData[2],'CUSTOMER'))
  xAccount = laData[2]
  SELECT CUSTOMER
  DO CUSBROWM WITH xAccount
  laData[2] = xAccount
ENDIF

=lfRefresh(lcWinCh1)
llBrowse = .F.
IF laScrMode[1]
  IF EMPTY(laData[2])
    SHOW GET ibStore   DISABLE
    SHOW GET laData[3] DISABLE
  ELSE
    SHOW GET ibStore   ENABLE
    SHOW GET laData[3] ENABLE
  ENDIF
ENDIF

*C102118,4 Enable Global Info Push button when user enters Account code. [Begin]
IF laScrMode[4]
  IF EMPTY(laData[2])
    SHOW GET pbGlobal DISABLE
  ELSE
    SHOW GET pbGlobal ENABLE
  ENDIF
ENDIF
*C102118,4 Enable Global Info Push button when user enters Account code. [End]

IF laScrMode[4] .AND. !EMPTY(laData[2])
  llFromBulk = .F.
  IF Customer.Status <> 'A'
    IF Customer.Status <> 'P'
      *-- Message : 32023
      *-- This a non-active xxxxx. Order entry is not allowed!
      *-- Button : 00000
      *-- Ok
      =gfModalGen('TRM32023B00000','ALERT','account'+'|'+IIF(lcOrdType='C','Contract','Order'))
      laData[2] = ''
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF
  ENDIF  
  
  *C102118,1 SSE Commented out Because they don't use bulk orders [Begin]
  *-- Message : 32011
  *-- There are open bulk orders for account xxxxx. 
  *-- Do you wish to use any of them in creating this order?
  *-- Button : 32000
  *-- Yes/No
  *IF lcOrdType='O' 
  *  SET ORDER TO TAG Ordbulk IN ORDHDR
  *  llFromBulk = (SEEK(laData[2]+'OYO','ORDHDR') .OR. SEEK(laData[2]+'HYO','ORDHDR')) .AND. ;
  *                gfModalGen('QRM32011B32000','ALERT',laData[2])=1
  *  SET ORDER TO TAG ORDHDR IN ORDHDR
  *  IF llFromBulk
  *    SELECT * FROM ORDHDR ;
  *    WHERE (account+status+bulk+cordtype+order=laData[2]+'OYO') .OR. ;
  *          (account+status+bulk+cordtype+order=laData[2]+'HYO') ;
  *    INTO CURSOR 'BulkOrd'
  *    lcBrFields = [Order:H="Order#",status:1:H="S",Season:H="SE",cDivision:H="DI",]+;
  *                 [CustPo :H="Cust. P.O#",]+;
  *                 [Open:H="Open.Qty.",OpenAmt:H="Open.Amt.",Ship:H="Ship.Qty.",Shipamt:H="Ship.Amt.",]+;
  *                 [start:H="Start",Complete:H="Complete",Note1:6:H="Notes"]
  *    llFromBulk = ARIABROW("","Open bulk orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr')
  *    =IIF(llFromBulk,SEEK('O'+BulkOrd.Order,'OrdHdr'),.T.)
  *    USE IN 'BulkOrd'
  *  ENDIF
  *ENDIF
  *C102118,1 SSE Commented out Because they don't use bulk orders [End]
  
  =SEEK('M'+laData[2],'CUSTOMER')
  =IIF(laSetups[11,2]='D',gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH'),.T.)

  *C102118,1 SSE Commented out Because they don't use bulk orders [Begin]
  *IF llFromBulk .AND. llBomVarnt
  *  lcCurBlkOr = OrdHdr.Order
  *  SELECT * , "A" AS cStatus , 0 AS nRecNo ;
  *    FROM (gcDataDir + "BomVar") ;
  *   WHERE cIdType+cCost_Id+STR(LineNo,6) = "SO" + lcCurBlkOr ;
  *    INTO DBF (gcWorkDir + lcT_BomVar)
  *  INDEX ON cIdType+cCost_Id+STR(LineNo,6) TAG (lcT_BomVar)
  *  REPLACE ALL cCost_Id WITH SPACE(6)
  *ENDIF
  *C102118,1 SSE Commented out Because they don't use bulk orders [End]

  lcPriceLvl = IIF(!EMPTY(Customer.PriceLvl),Customer.PriceLvl,'A')
  STORE Customer.BtName TO lcBillName,lcDBllName
  =gfGetAdr('Customer','','','',1,'2')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcDBllAdd&lcCount = lcDBllAdd&lcCount + IIF(EMPTY(lcDBllAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    lcBillAdd&lcCount = lcDBllAdd&lcCount
  ENDFOR  
  IF !EMPTY(Customer.Dist_Ctr)
    llMove = .T.
    lcESeek = Customer.Store
    =SEEK("S"+Customer.Account+Customer.Dist_ctr,"Customer")
  ENDIF
  STORE IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba) TO lcDShpName,lcShipName
  =gfGetAdr('CUSTOMER','','','',1,'')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcDShpAdd&lcCount = lcDShpAdd&lcCount + IIF(EMPTY(lcDShpAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    lcShipAdd&lcCount = lcDShpAdd&lcCount
  ENDFOR  
  IF llMove
    llMove = .F.
    =SEEK("S"+Customer.Account+lcESeek,"Customer")
  ENDIF
  STORE IIF(llFromBulk,OrdHdr.CustPo,'') TO laData[4]
  STORE IIF(llFromBulk,OrdHdr.Approval,'') TO laData[48]
 
  STORE gdSysDate TO laData[8],laData[9]
  STORE IIF(llFromBulk,OrdHdr.cFacCode,Customer.cFacCode) TO laData[26]
  STORE IIF(llFromBulk,OrdHdr.ShipVia,;
        IIF(EMPTY(Customer.ShipVia),lfGetDefCode('SHIPVIA'),;
        Customer.ShipVia)) TO laData[12]
  STORE IIF(llFromBulk,OrdHdr.cTermCode,;
        IIF(EMPTY(Customer.cTermCode),lfGetDefCode('CTERMCODE'),;
        Customer.cTermCode))   TO laData[11]
  STORE IIF(llFromBulk,OrdHdr.SpcInst,;
        IIF(EMPTY(Customer.SpcInst),lfGetDefCode('SPCINST'),;
        Customer.SpcInst)) TO laData[13]
        
  STORE IIF(llFromBulk,OrdHdr.Disc,Customer.Disc)     TO laData[16]
  STORE IIF(llFromBulk,OrdHdr.Rep1,Customer.SalesRep) TO laData[27]
  STORE IIF(llFromBulk,OrdHdr.Rep2,Customer.Rep2)     TO laData[29]
  STORE IIF(llFromBulk,OrdHdr.Note1,Customer.Note)    TO laData[18]
  STORE IIF(llFromBulk,OrdHdr.Note2,'')               TO laData[19]
  STORE IIF(llFromBulk,OrdHdr.Buyer,Customer.Buyer)   TO laData[20]
  STORE IIF(llFromBulk,OrdHdr.Phone,Customer.Phone1)  TO laData[21]
  
  *C102118,4 Always laData[6] is 'Y' --> Multi Store [Begin]
  *STORE 'N' TO laData[6],laData[23],laData[24]
  STORE 'N' TO laData[23],laData[24]
  STORE 'Y' TO laData[6]
  *C102118,4 Always laData[6] is 'Y' --> Multi Store [End]

  *-- Read the GL link code of the factor while creating the sales order. 
  *-- If there is a factor assgined to the current customer.
  *-- we should read the GL link code of this factor insated of the 
  *-- gl link code of this customer.  
  *-- If the factor link code is empty we should not change the current sitation. [Begin]
  STORE IIF(llFromBulk,OrdHdr.Link_Code,Customer.Link_Code) TO laData[32]  
  IF EMPTY(Customer.Link_Code)
    lcLink_Cod = 'DEFDEF'
  ELSE
    lcLink_Cod = Customer.Link_Code
  ENDIF
  STORE IIF(llFromBulk,OrdHdr.Link_Code,lcLink_Cod) TO laData[32]
  
  STORE IIF(llFromBulk,OrdHdr.cDivision,lcODefDiv) TO laData[15]
  IF laSetups[10,2]='Y'
    =gfRltFld(laData[15],@laDRltFld,'CDIVISION')
    laData[53] = ALLTRIM(laData[53])
    laData[32] = ALLTRIM(laData[32])
  ELSE
    STORE Customer.cSlsGlLink TO laData[53]
  ENDIF
  IF !EMPTY(Customer.cFacCode) 
    =gfOpenFile(gcDataDir+'Factor',gcDataDir+'Factor','SH')
    IF SEEK(Customer.cFacCode,'FACTOR') .AND. !EMPTY(Factor.Link_Code)
      laData[32] = Factor.Link_Code
    ENDIF
    =gfCloseFile('FACTOR')    
  ENDIF
  IF EMPTY(laData[32])
    STORE IIF(llFromBulk,OrdHdr.Link_Code,lcLink_Cod) TO laData[32]
  ENDIF
  laData[53]= IIF(EMPTY(laData[53]),'DEF',laData[53])
  laData[52] = SUBSTR(laOCtgCd[lnOCtgCd,2],1,6)  
  llInsur = (CUSTOMER.cInsur='Y')
  laData[22]=IIF(llInsur,'Y','N')

  STORE IIF(llFromBulk,OrdHdr.Comm1,;
        IIF(laSetups[11,2]='D' .AND. SEEK(laData[27]+laData[15],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm)) TO laData[28],lnSouComm1
  STORE IIF(llFromBulk,OrdHdr.Comm2,;
        IIF(laSetups[11,2]='D' .AND. SEEK(laData[29]+laData[15],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm2)) TO laData[30],lnSouComm2
  STORE IIF(llFromBulk,OrdHdr.cCurrCode,Customer.cCurrCode) TO laData[33]
  STORE IIF(llFromBulk,OrdHdr.Open,0)    TO laData[35],laData[41]
  STORE IIF(llFromBulk,OrdHdr.OpenAmt,0) TO laData[36],laData[42]
  
  STORE ldDefOrdDat TO laData[10]
  STORE IIF(llFromBulk,OrdHdr.Season,lcODefSes)     TO laData[14]
  STORE IIF(llFromBulk,OrdHdr.cWareCode,lcODefWare) TO laData[31]
  STORE IIF(llFromBulk,OrdHdr.Order,'') TO laData[43]
  
  STORE IIF( !EMPTY(laData[26]) OR IIF(llFromBulk,lfOrdStats(),laSetups[7,2]='Y') ,'H','O') TO laData[5]
  STORE IIF(laData[5]='O',2,3) TO lnOrdStatus
  
  STORE IIF(llFromBulk,Ordhdr.Priority,Customer.Priority) TO laData[25]
  SHOW GET lnOrdStatus ENABLE
  laData[33] = IIF(EMPTY(laData[33]),gcBaseCurr,laData[33])
  IF laData[33] = gcBaseCurr
    STORE 1 TO laData[34], laData[50]
  ELSE
    laData[34]=gfChkRate('laData[50]',laData[33],gdSysDate,.T.,.F.,.F.,llEditExRt)
    IF laData[34] = 0
      IF llEditExRt
        *-- Message : 00262
        *-- A valid xxx to xxx exchange rate could not be found on xxx.
        *-- Button : 00000
        *-- Ok
        =gfModalGen('INM00262B00000','ALERT',ALLTRIM(laData[33])+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(gdSysDate))
      ELSE
        laData[33] = gcBaseCurr
        STORE 1 TO laData[34], laData[50]
      ENDIF
    ENDIF
  ENDIF
  =IIF(laSetups[11,2]='D',gfCloseFile('REP_DIV'),.T.)

  SELECT (lcOrdHdr)
  APPEND BLANK
  =RLOCK()
  GATHER FROM laData FIELDS &lcScFields
  REPLACE cOrdType WITH lcOrdType
  UNLOCK
  =lfvRep1() .AND. lfvRep2()
  SHOW GET ibAccount  DISABLE
  SHOW GET laData[2]  DISABLE
  SHOW GETS WINDOW (lcWinCh2) ENABLE ONLY
  =lfRefresh(lcWinCh2)
  
  *C102118,1 Disable the Tracking Push Button in Add mode [Begin]
  IF laScrMode[4]
    SHOW GET pbTracking DISABLE
  ENDIF
  *C102118,1 Disable the Tracking Push Button in Add mode [End]

  SHOW GET lcShipName DISABLE
  SHOW GET lcShipAdd1 DISABLE
  SHOW GET lcShipAdd2 DISABLE
  SHOW GET lcShipAdd3 DISABLE
  SHOW GET lcShipAdd4 DISABLE
  SHOW GET lcShipAdd5 DISABLE
  =gfwCodePop(@laCodes,'CTERMCODE','T')
  =gfwCodePop(@laCodes,'SHIPVIA','T')
  IF llMultiSt
    =lfChMSHV()
  ENDIF
  =gfwCodePop(@laCodes,'SPCINST','T')
  =gfwCodePop(@laCodes,'SEASON','T')
  =gfwCodePop(@laCodes,'CDIVISION','T')

  lcScrMode = 'N'
  =IIF(llFromBulk,.T.,lfvCommision())

  *C102118,1 SSE Commented out we don't use Uncomplete session. [Begin]
  *SELECT 'UNCMSESS'
  *IF SEEK('I'+PADR("SOSOC10",10)+PADR(gcUser_id,10)) AND RLOCK()
  *  BLANK
  *ELSE
  *  APPEND BLANK
  *ENDIF
  *REPLACE Status     WITH 'O'       ,;
  *        cUTranType WITH lcProgID  ,;
  *        cUserId    WITH gcUser_id ,;
  *        cSession   WITH lcSession ,;
  *        cProgram   WITH 'SOSOC10'   ,;
  *        cCurrScr   WITH 'SOSOC10'   ,;
  *        dTranDate  WITH gdSysDate ,;
  *        cTranTime  WITH TIME()
  *=RLOCK()
  *lcFiles = 'lcOrdHdr,'+lcOrdHdr+','+lcOrdHdr+';'+;
  *          'lcOrdLine,'+lcOrdLine+',ORDLINE;'+IIF(llFromBulk,;
  *          'lcBulkOrd'+','+lcBulkOrd+','+lcBulkOrd+';','')
  *=gfSavSess(lcProgID, lcFiles, @laVariables,lcSession)
  *C102118,1 SSE Commented out we don't use Uncomplete session. [End]
  
  SELECT (lcOrdHdr)
  SHOW GET lnOrdStatus ENABLE  
  SHOW GET ibStore     ENABLE
  SHOW GET laData[3]   ENABLE
  SHOW GET laData[4]   ENABLE  

  *C102118,1 Multi Store Check Box is removed from Screen [Begin] 
  *SHOW GET llMultiSt   ENABLE  
  *C102118,1 Multi Store Check Box is removed from Screen [End] 

  llCUpDate = .T.

  *C102118,1 SSE Commented out Because they don't use bulk orders [Begin]
  *IF llFromBulk
  *  SHOW GET lnSeason    DISABLE
  *  SHOW GET lnDivision  DISABLE
  *  SHOW GET lnWareHouse DISABLE
  *  SHOW GET laData[33]  DISABLE
  *  SHOW GET laData[34]  DISABLE
  *ENDIF
  *C102118,1 SSE Commented out Because they don't use bulk orders [End]
  
  IF !llEditExRt OR laData[33]=gcBaseCurr
    SHOW GET laData[34] DISABLE
  ELSE
    SHOW GET laData[34] ENABLE
  ENDIF
  IF (Customer.Age60 <> 0 .OR. Customer.Age90 <> 0;
    .OR. Customer.Age120 <> 0 ) .AND. laSetups[12,2]='Y'
    ?? CHR(7)
    ?? CHR(7)
    ?? CHR(7)
    DO (gcScrDir+gcWinAppl+"\SOAGING.SPX")
  ENDIF
ENDIF
*-- End of lfvAccount.

*!*******************************************************************************
*! Name      : lfvMultiSt
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate multi stores check box
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvMultiSt()
*!*******************************************************************************     
*This function is not used anymore since Soccer Select places 
*Multi Store Orders so we removed the Multi Store Check Box 
*from Screen 
FUNCTION lfvMultiSt

IF llMultiSt
  IF !SEEK('S'+laData[2],'Customer')
    *-- Message : 32048
    *-- No stores found for account xxxxx
    *-- Button : 00000 
    *-- Ok
    =gfModalGen('TRM32048B00000','ALERT',laData[2])
  
    *C102118,1 Multi Store Check Box is removed from Screen [Begin] 
    *STORE .F. TO llMultiSt
    *SHOW GET llMultiSt
    *C102118,1 Multi Store Check Box is removed from Screen [End] 
    RETURN 
    
  ENDIF
  STORE .F. TO llBulk
  STORE ''  TO lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5
  STORE ''  TO lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5
  STORE SPACE(8) TO laData[3]
  STORE '*' TO laData[12]
  laCodes[2,5] = .T.
  STORE 'At Store Level' TO lcShipName,lcBillName
  SHOW GET ibStore    DISABLE
  SHOW GET laData[3]  DISABLE

  *C102118,1 SSE Commented out because they don't have Bulk Orders [Begin]
  *SHOW GET llBulk     DISABLE
  *C102118,1 SSE Commented out because they don't have Bulk Orders [End]

  SHOW GET lnShipAddr DISABLE
  SHOW GET laData[7]  ENABLE

  *C102118,1 SSE Commented out because it is Removed from screen [Begin]  
  *IF lnActFolder = 2
  *  SHOW GET pbTemplate ENABLE
  *ENDIF
  *C102118,1 SSE Commented out because it is Removed from screen [End]  
ELSE
  laData[12] = Customer.ShipVia
  laCodes[2,5] = .F.
  lcShipName = lcDShpName
  lcShipAdd1 = lcDShpAdd1
  lcShipAdd2 = lcDShpAdd2
  lcShipAdd3 = lcDShpAdd3
  lcShipAdd4 = lcDShpAdd4
  lcShipAdd5 = lcDShpAdd5
  lcBillName = lcDBllName
  lcBillAdd1 = lcDBllAdd1
  lcBillAdd2 = lcDBllAdd2 
  lcBillAdd3 = lcDBllAdd3
  lcBillAdd4 = lcDBllAdd4
  lcBillAdd5 = lcDBllAdd5
  laData[7]  = .F.
  laData[4]  = SPACE(15)
  SHOW GET ibStore    ENABLE
  SHOW GET laData[3]  ENABLE

  *C102118,1 SSE Commented out because they don't have Bulk Orders [Begin]
  *IF lcOrdType <> 'C'
  *  SHOW GET llBulk ENABLE
  *ENDIF
  *C102118,1 SSE Commented out because they don't have Bulk Orders [End]
  
  SHOW GET lnShipAddr ENABLE
  SHOW GET laData[7]  DISABLE
  
  *C102118,1 SSE Commented out because it is Removed from screen [Begin]
  *IF lnActFolder = 2
  *  SHOW GET pbTemplate DISABLE
  *ENDIF
  *C102118,1 SSE Commented out because it is Removed from screen [End]  
ENDIF
laData[6] = IIF(llMultiSt,'Y','N')
SHOW GET laData[4] ENABLE
lnShipAddr = 1
SHOW GET lnShipAddr
SHOW GET lcShipName DISABLE
SHOW GET lcShipAdd1 DISABLE
SHOW GET lcShipAdd2 DISABLE
SHOW GET lcShipAdd3 DISABLE
SHOW GET lcShipAdd4 DISABLE
SHOW GET lcShipAdd5 DISABLE
=lfRefresh(lcWinCh2)
SHOW GETS WINDOW (lcWinCh2) ONLY
SELECT (lcOrdHdr)
=RLOCK()
REPLACE Multi     WITH IIF(llMultiSt,'Y','N') ,;
        MultiPo   WITH laData[7] ,;
        CustPo    WITH laData[4] ,;
        Store     WITH laData[3] ,;
        Bulk      WITH IIF(llBulk,'Y','N') ,;
        Alt_ShpTo WITH laData[51],;
        StName    WITH SPACE(30) ,;
        cAddress1 WITH SPACE(30) ,;
        cAddress2 WITH SPACE(30) ,;
        cAddress3 WITH SPACE(30) ,;
        cAddress4 WITH SPACE(30) ,;
        cAddress5 WITH SPACE(30) ,;
        ShipVia   WITH laData[12]
UNLOCK
IF llMultiSt
  =gfwCodePop(@laCodes,'SHIPVIA','A')
  IF llMultiSt
    =lfChMSHV()
  ENDIF
ELSE
  =gfwCodePop(@laCodes,'SHIPVIA','T')
ENDIF
*-- End of lfvMultiSt.

*!*******************************************************************************
*! Name      : lfxvStore
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Invoice store
*!*******************************************************************************
*! Calls     : CUSBROWS,gfGetAdr,gfChkRate,gfModalGen
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  .f.
*!*******************************************************************************
*! Example   :  =lfxvStore()
*!*******************************************************************************
*C102118,1 This function is not used anymore It's replaced with a new one with
*C102118,1 the same name so this function is renamed to be lfvxStore.
*
*FUNCTION lfvStore
FUNCTION lfvxStore
PRIVATE xStore,lnAlias
lnAlias = SELECT()
IF llBrowse .OR. (!EMPTY(laData[3]) .AND. ;
                  !SEEK('S'+laData[2]+laData[3],'CUSTOMER'))
  xStore = laData[3]
  SELECT CUSTOMER
  IF !CUSBROWS(laData[2],.T.)
    llBrowse = .F.
    IF laScrMode[1]
      STORE lcOldValue TO laData[3]
    ELSE
      SELECT (lcOrdHdr)
      STORE Store TO laData[3]
    ENDIF
    SELECT (lnAlias)
    RETURN
  ENDIF
  laData[3] = xStore
ENDIF  
llBrowse = .F.
IF !laScrMode[1] .AND. laData[3] = &lcOrdHdr..Store
  SELECT (lnAlias)
  RETURN
ENDIF

IF !EMPTY(laData[3]) .AND. Customer.Status <> 'A' 
  IF Customer.Status <> 'P'
    *-- Message : 32023
    *-- This a non-active xxxxx. Order entry is not allowed!
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32023B00000','ALERT','store'+'|'+IIF(lcOrdType='C','Contract','Order'))
    SELECT (lcOrdHdr)
    STORE Store TO laData[3]
    SELECT (lnAlias)
    RETURN
  ENDIF
ENDIF
DO CASE
  CASE laScrMode[4]
    DO CASE 
      CASE EMPTY(laData[3]) .AND. ;
        gfModalGen('QRM32024B32000','ALERT',IIF(lcOrdType='C','contract','order')+'|'+'customer')=1
        *-- Message : 32024
        *-- Overwrite order header with xxxxx defaults?
        *-- Button : 32000
        *-- Yes;No
        =SEEK('M'+laData[2],'CUSTOMER')
        lcBillName = lcDBllName
        lcBillAdd1 = lcDBllAdd1
        lcBillAdd2 = lcDBllAdd2
        lcBillAdd3 = lcDBllAdd3
        lcBillAdd4 = lcDBllAdd4
        lcBillAdd5 = lcDBllAdd5
        lcShipName = lcDShpName
        lcShipAdd1 = lcDShpAdd1
        lcShipAdd2 = lcDShpAdd2
        lcShipAdd3 = lcDShpAdd3
        lcShipAdd4 = lcDShpAdd4
        lcShipAdd5 = lcDShpAdd5
        lnShipAddr = 1
        SHOW GETS WINDOW (lcWinCh2) ONLY
        
        STORE Customer.SalesRep  TO laData[27]
        STORE Customer.Rep2      TO laData[29] 
        =lfvRep1() .AND. lfvRep2()
        =IIF(laSetups[11,2]='D',gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH'),.T.)
        STORE IIF(laSetups[11,2]='D' .AND. SEEK(laData[27]+laData[15],'REP_DIV'),;
              REP_DIV.Comm_Rate,Customer.Comm) TO laData[28],lnSouComm1
        STORE IIF(laSetups[11,2]='D' .AND. SEEK(laData[29]+laData[15],'REP_DIV'),;
              REP_DIV.Comm_Rate,Customer.Comm2) TO laData[30],lnSouComm2
        lnShipAddr = 1
        =lfRefresh(lcWinCh2)
        STORE Customer.ShipVia  TO laData[12]
        SHOW GETS WINDOW (lcWinCh2) ONLY
        =IIF(laSetups[11,2]='D',gfCloseFile('REP_DIV'),.T.)

        SELECT (lcOrdHdr)
        =RLOCK()
        REPLACE Rep1    WITH laData[27] ,;
                Rep2    WITH laData[29] ,;
                Comm1   WITH laData[28] ,;
                Comm2   WITH laData[30] ,;
                ShipVia WITH laData[12]
        UNLOCK
        =gfwCodePop(@laCodes,'SHIPVIA','T')
        IF llMultiSt
          =lfChMSHV()
        ENDIF

        =lfvCommision()
      CASE !EMPTY(laData[3]) .AND. ;
        gfModalGen('QRM32024B32000','ALERT',IIF(lcOrdType='C','contract','order')+'|'+'store')=1
        *-- Message : 32024
        *-- Overwrite order header with xxx defaults?
        *-- Button : 32000
        *-- Yes;No
        STORE '' TO lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,lcBillName,;
                    lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,lcShipName
        lcBilLName = Customer.BtName
        =gfGetAdr('Customer','','','',1,'2')
        FOR lnCount = 1 TO ALEN(laAddress,1)
          lcCount = STR(laAddress[lnCount,1],1)
          lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
          SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
        ENDFOR  
        IF !EMPTY(Customer.Dist_Ctr)
          llMove = .T.
          lcESeek = Customer.Store
          =SEEK("S"+Customer.Account+Customer.Dist_ctr,"Customer")
        ENDIF
        lcShipName = IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
        =gfGetAdr('CUSTOMER','','','',1,'')
        FOR lnCount = 1 TO ALEN(laAddress,1)
          lcCount = STR(laAddress[lnCount,1],1)
          lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
          SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
        ENDFOR  
        IF llMove
          llMove = .F.
          =SEEK("S"+Customer.Account+lcESeek,"Customer")
        ENDIF

        STORE Customer.SalesRep TO laData[27]
        STORE Customer.Rep2     TO laData[29] 
        STORE Customer.ShipVia  TO laData[12]
        =lfvRep1() .AND. lfvRep2()
        =IIF(laSetups[11,2]='D',gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH'),.T.)
        STORE IIF(laSetups[11,2]='D' .AND. SEEK(laData[27]+laData[15],'REP_DIV'),;
              REP_DIV.Comm_Rate,Customer.Comm) TO laData[28],lnSouComm1
        STORE IIF(laSetups[11,2]='D' .AND. SEEK(laData[29]+laData[15],'REP_DIV'),;
              REP_DIV.Comm_Rate,Customer.Comm2) TO laData[30],lnSouComm2
        
        lnShipAddr = 1
        =lfRefresh(lcWinCh2)
        SHOW GETS WINDOW (lcWinCh2) ONLY
        =IIF(laSetups[11,2]='D',gfCloseFile('REP_DIV'),.T.)
        SELECT (lcOrdHdr)
        =RLOCK()
        REPLACE Rep1    WITH laData[27] ,;
                Rep2    WITH laData[29] ,;        
                Comm1   WITH laData[28] ,;
                Comm2   WITH laData[30] ,;
                ShipVia WITH laData[12]
        UNLOCK
        =gfwCodePop(@laCodes,'SHIPVIA','T')
        IF llMultiSt
          =lfChMSHV()
        ENDIF

        =lfvCommision()
    ENDCASE
  *-- Message : 32012
  *-- Change alternate shipto to new store shipto?
  *-- Button : 32000
  *-- Yes/No
  CASE laScrMode[3] .AND. lnShipAddr=2 .AND. !EMPTY(laData[3]) .AND. ;
    gfModalGen('QRM32012B32000','ALERT')=1
    
    STORE '' TO lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,lcShipName
    IF !EMPTY(Customer.Dist_Ctr)
      llMove = .T.
      lcESeek = Customer.Store
      =SEEK("S"+Customer.Account+Customer.Dist_ctr,"Customer")
    ENDIF
    
    lcShipName = IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
    =gfGetAdr('CUSTOMER','','','',1,'')
    FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount = STR(laAddress[lnCount,1],1)
      lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    ENDFOR  
    IF llMove
      llMove = .F.
      =SEEK("S"+Customer.Account+lcESeek,"Customer")
    ENDIF

    lnShipAddr=1
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE Alt_ShpTo WITH .F. ,;
            StName    WITH SPACE(30) ,;
            cAddress1 WITH SPACE(30) ,;
            cAddress2 WITH SPACE(30) ,;
            cAddress3 WITH SPACE(30) ,;
            cAddress4 WITH SPACE(30) ,;
            cAddress5 WITH SPACE(30)
    UNLOCK
    SHOW GET lnShipAddr
    SHOW GET lcShipName DISABLE
    SHOW GET lcShipAdd1 DISABLE
    SHOW GET lcShipAdd2 DISABLE
    SHOW GET lcShipAdd3 DISABLE
    SHOW GET lcShipAdd4 DISABLE
    SHOW GET lcShipAdd5 DISABLE
ENDCASE
IF !laScrMode[1]
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Store WITH laData[3]
  UNLOCK
  IF SEEK(lcOrdType+laData[1],lcOrdLine)
    WAIT 'Update order lines...' WINDOW NOWAIT
    SELECT (lcOrdLine)
    lcCurrRec = EVAL(SYS(14,VAL(SYS(21))))
    SCAN REST WHILE cOrdType+Order = lcOrdType+laData[1]
      =RLOCK()
      REPLACE Store WITH laData[3] ,;
              FLAG  WITH IIF(FLAG <> 'N','M',FLAG)
      UNLOCK
    ENDSCAN
    =SEEK(lcCurrRec)
    WAIT CLEAR 
  ENDIF
ENDIF
SELECT (lnAlias)
*-- End of lfvxStore.

*!*******************************************************************************
*! Name      : lfvMultiPo
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate multi customer po number check box
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvMultiPo()
*!*******************************************************************************
*
FUNCTION lfvMultiPo

IF laData[7]
  STORE SPACE(10) TO laData[4]
  SHOW GET laData[4] DISABLE
ELSE
  SHOW GET laData[4] ENABLE
ENDIF  
SELECT (lcOrdHdr)
=RLOCK()
REPLACE MultiPo WITH laData[7] ,;
        CustPo  WITH laData[4]
UNLOCK
IF lnActFolder = 2
  =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
ELSE
  llReBrowse = .T.
ENDIF
*-- End of lfvMultiPo.

*!*******************************************************************************
*! Name      : lfvCustPo
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate customer po number
*!*******************************************************************************
*! Calls     : gfModalGen
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvCustPo()
*!*******************************************************************************
*
FUNCTION lfvCustPo
PRIVATE llCustPo 

DO CASE
  CASE laScrMode[1] .AND. (!EMPTY(laData[2]) .OR. !EMPTY(laData[4]))
    llBrowse = .T.
    _CUROBJ = OBJNUM(laData[1])
    KEYBOARD "{ENTER}"
    RETURN
  CASE laScrMode[4] .AND. laData[4] <> &lcOrdHdr..CustPo
    IF !EMPTY(laData[4])
      SET ORDER TO TAG ORDCUST IN ORDHDR
      llCustPo =SEEK(laData[2]+UPPER(laData[4])+lcOrdType,'OrdHdr')
      SET ORDER TO TAG OrdHdr IN OrdHdr
      *-- Message : 32025
      *-- Cust. PO# xxx has been entered before.
      *-- Button : 32004
      *-- Proceed;Reenter
      IF llCustPo .AND. gfModalGen('QRM32025B32004','ALERT',ALLTRIM(laData[4]))=2
        _CUROBJ = _CUROBJ
        RETURN
      ENDIF
      SELECT (lcOrdHdr)
      =RLOCK()
      REPLACE CustPo WITH laData[4]
      UNLOCK
      STORE .F. TO laData[7]
      SHOW GET laData[7] DISABLE
    ELSE
      IF llMultiSt
        SHOW GET laData[7] ENABLE
      ENDIF
    ENDIF
ENDCASE
*-- End of lfvCustPo.

*!*******************************************************************************
*! Name      : lfvEntered
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid function for order entered date
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvEntered()
*!*******************************************************************************
*
FUNCTION lfvEntered

IF laData[8] <> &lcOrdHdr..Entered
  llCUpdate = .T.
  IF laData[33] <> gcBaseCurr
    laData[34] = gfChkRate('laData[50]',laData[33],laData[8],.T.,gcAct_Comp,.F.,llEditExRt)
    IF laData[34] = 0 
      IF llEditExRt
        *-- Message : 00262
        *-- A valid xxx to xxx exchange rate could not be found on xxx.
        *-- Button : 00000
        *-- Ok
        =gfModalGen('INM00262B00000','ALERT',ALLTRIM(laData[33])+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(laData[8]))
      ELSE
        laData[33] = gcBaseCurr
        STORE 1 TO laData[34], laData[50]
      ENDIF  
    ENDIF
  ENDIF
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE cCurrCode WITH laData[33] ,;
          Entered   WITH laData[8]  ,;
          nExRate   WITH laData[34] ,;
          nCurrUnit WITH laData[50]
  UNLOCK
  SHOW GETS WINDOW (lcWinCh2) ONLY
  llUpdBook = laData[5]='B' .OR. (gdSysDate <= (laData[8] + laSetups[8,2]))
ENDIF
*-- End of lfvEntered.

*!*******************************************************************************
*! Name      : lfvStart
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid function for order start date
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvStart()
*!*******************************************************************************
*
FUNCTION lfvStart

IF laData[9] <> &lcOrdHdr..Start
  IF laData[9] > laData[10]
    *-- Message : 32013
    *-- The starting date cannot be greater than the completion date.
    *-- Button : 00000
    *-- Ok
    IF gfModalGen('TRM32013B32004','ALERT',' It will default to Start date.') = 1
      laData[10] = laData[9]
      SHOW GET laData[10]
    ELSE
      laData[9] = &lcOrdHdr..Start
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF
  ENDIF
  llCUpdate = .T.
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Start    WITH laData[9],;
          Complete WITH laData[10]
  UNLOCK
ENDIF
*-- End of lfvStart.

*!*******************************************************************************
*! Name      : lfvComplete
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid function for order completion date
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvComplete()
*!*******************************************************************************
*
FUNCTION lfvComplete

IF laData[10] <> &lcOrdHdr..Complete
  IF laData[9] > laData[10]
    *-- Message : 32013
    *-- The starting date cannot be greater than the completion date.
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32013B00000','ALERT')
    laData[10] = &lcOrdHdr..Complete
    RETURN
  ENDIF
  IF  gcContCode # 'EGYPT'
    IF CDOW(laData[10])='Saturday' .OR. CDOW(laData[10])='Sunday'
      *-- Message : 32014
      *-- The completion date is a xxxxx.
      *-- Button : 00000
      *-- Ok
      =gfModalGen('INM32014B00000','ALERT',CDOW(laData[10]))
    ENDIF
  ENDIF
  llCUpdate = .T.
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Complete WITH laData[10]
  UNLOCK
ENDIF
*-- End of lfvComplete.

*!*******************************************************************************
*! Name      : lfvShipAddr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Switch between default and alternative shipping adresses
*!*******************************************************************************
*! Calls     : gfActPop
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvShipAddr()
*!*******************************************************************************
*
FUNCTION lfvShipAddr
PRIVATE lcShipState

laData[51] = (lnShipAddr = 2)
IF &lcOrdHdr..Alt_ShpTo <> laData[51]
  llCUpdate = .T.
  lcShipState = IIF(laData[51],'ENABLE','DISABLE')
  lcShipName  = IIF(laData[51],lcShipName,lcDShpName)
  lcShipAdd1  = IIF(laData[51],lcShipAdd1,lcDShpAdd1)
  lcShipAdd2  = IIF(laData[51],lcShipAdd2,lcDShpAdd2)
  lcShipAdd3  = IIF(laData[51],lcShipAdd3,lcDShpAdd3)
  lcShipAdd4  = IIF(laData[51],lcShipAdd4,lcDShpAdd4)
  lcShipAdd5  = IIF(laData[51],lcShipAdd5,lcDShpAdd5)
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Alt_ShpTo WITH laData[51] ,;
          StName    WITH IIF(laData[51],lcShipName,SPACE(30)) ,;
          cAddress1 WITH IIF(laData[51],lcShipAdd1,SPACE(30)) ,;
          cAddress2 WITH IIF(laData[51],lcShipAdd2,SPACE(30)) ,;
          cAddress3 WITH IIF(laData[51],lcShipAdd3,SPACE(30)) ,;
          cAddress4 WITH IIF(laData[51],lcShipAdd4,SPACE(30)) ,;
          cAddress5 WITH IIF(laData[51],lcShipAdd5,SPACE(30))
  UNLOCK
  SHOW GET lcShipName &lcShipState
  SHOW GET lcShipAdd1 &lcShipState
  SHOW GET lcShipAdd2 &lcShipState
  SHOW GET lcShipAdd3 &lcShipState
  SHOW GET lcShipAdd4 &lcShipState
  SHOW GET lcShipAdd5 &lcShipState
ENDIF
RETURN
*-- End of lfvShipAddr.

*!*******************************************************************************
*! Name      : lfvShipping
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid alternative shipping address information
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  lcAddressNo : Alternative address line #
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvShipping()
*!*******************************************************************************
*
FUNCTION lfvShipping
PARAMETERS lcAddressNo

SELECT (lcOrdHdr)
IF lcAddressNo = '0' .AND. StName <> lcShipName
  =RLOCK()
  REPLACE StName WITH lcShipName
  UNLOCK
  llCUpdate = .T.
ENDIF  
IF lcAddressNo <> '0' .AND. cAddress&lcAddressNo <> lcShipAdd&lcAddressNo
  =RLOCK()
  REPLACE cAddress&lcAddressNo WITH lcShipAdd&lcAddressNo
  UNLOCK
  llCUpdate = .T.
ENDIF
*-- End of lfvShipping.

*!*******************************************************************************
*! Name      : lfvTerms
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid order terms code
*!*******************************************************************************
*! Calls     : CODECHK
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvTerms()
*!*******************************************************************************
*
FUNCTION lfvTerms

IF laData[11] <> ALLTRIM(laTerms[lnTerms,2])
  laData[11] = ALLTRIM(laTerms[lnTerms,2])
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE cTermCode WITH laData[11]
  UNLOCK
  llCUpdate = .T.
ENDIF
*-- End of lfvTerms.

*!*******************************************************************************
*! Name      : lfvShipVia
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate order shipvia
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvShipVia()
*!*******************************************************************************
*
PROCEDURE lfvShipVia

IF laData[12] <> ALLTRIM(laShipVia[lnShipVia,2])
  laData[12] = ALLTRIM(laShipVia[lnShipVia,2])
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE ShipVia WITH laData[12]
  UNLOCK
  llCUpdate = .T.
ENDIF
*-- End of lfvShipVia.

*!*******************************************************************************
*! Name      : lfvSpcInst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid order special instruction code
*!*******************************************************************************
*! Calls     : CODECHK
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvSpcInst()
*!*******************************************************************************
*
FUNCTION lfvSpcInst

IF laData[13] <> ALLTRIM(laSpcInst[lnSpcInst,2])
  laData[13] = ALLTRIM(laSpcInst[lnSpcInst,2])
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE SpcInst WITH laData[13]
  UNLOCK
  llCUpdate = .T.
ENDIF
*-- End of lfvSpcInst.

*!*******************************************************************************
*! Name      : lfvSeason
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid order season code
*!*******************************************************************************
*! Calls     : CODECHK
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvSeason()
*!*******************************************************************************
*
FUNCTION lfvSeason

IF laData[14] <> SUBSTR(laSeasons[lnSeason,2],1,6)
  laData[14] = SUBSTR(laSeasons[lnSeason,2],1,6)
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Season WITH laData[14]
  UNLOCK
  llCUpdate = .T.
ENDIF
*-- End of lfvSeason.

*!*******************************************************************************
*! Name      : lfvDivision
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid order division code
*!*******************************************************************************
*! Calls     : CODECHK,lfvCommision
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvDivision()
*!*******************************************************************************
*
FUNCTION lfvDivision

IF laData[15] <> SUBSTR(laDivision[lnDivision,2],1,6)
  laData[15] = SUBSTR(laDivision[lnDivision,2],1,6)
  *-- Check if link codes at division level
  PRIVATE lcLink_cod
  lcLink_cod = laData[32]
  IF laSetups[10,2]='Y'
    =gfRltFld(laData[15],@laDRltFld,'CDIVISION')
    laData[53] = ALLTRIM(laData[53])
    laData[32] = ALLTRIM(laData[32])
  ENDIF
  IF EMPTY(laData[32])
    laData[32] = lcLink_cod
  ENDIF
  IF !EMPTY(Customer.cFacCode) 
    =gfOpenFile(gcDataDir+'Factor',gcDataDir+'Factor','SH')
    IF SEEK(Customer.cFacCode,'FACTOR') .AND. !EMPTY(Factor.Link_Code)
      laData[32] = Factor.Link_Code
    ENDIF
    =gfCloseFile('FACTOR')    
  ENDIF
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE cDivision  WITH laData[15] ,;
          Link_Code WITH laData[32]
  UNLOCK
  llCUpdate = .T.
  *-- Get sales rep commission for this division
  IF laSetups[11,2]='D'
    =gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH')
    IF SEEK(laData[27]+laData[15],'REP_DIV')
      STORE REP_DIV.Comm_Rate TO laData[28],lnSouComm1
    ENDIF
    IF SEEK(laData[29]+laData[15],'REP_DIV')
      STORE REP_DIV.Comm_Rate TO laData[30],lnSouComm2
    ENDIF
    =gfCloseFile('REP_DIV')

    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE Comm1 WITH laData[28] ,;
            Comm2 WITH laData[30]
    UNLOCK
    =lfvCommision()
  ENDIF
ENDIF
*-- End of lfvDivision.

*!*******************************************************************************
*! Name      : lfvDept
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate department
*!*******************************************************************************
*! Calls     : gfModalGen,ARIABROW,lfvCommision
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvDept()
*!*******************************************************************************
*
FUNCTION lfvDept

IF !llBrowse .AND. laData[17] = &lcOrdHdr..Dept
  RETURN
ENDIF
=gfOpenFile(gcDataDir+'CustDept',gcDataDir+'CustDept','SH')
IF llBrowse .OR. (!EMPTY(laData[17]) .AND. !SEEK(laData[2]+laData[17],'CustDept'))
  IF !SEEK(laData[2],'CustDept')
  
    *B606908,1 Reset the browse flag and comment out IF llBrowse. [Begin]
    *IF llBrowse
    llBrowse = .F.
    *B606908,1 Reset the browse flag and comment out IF llBrowse. [End]

      *-- Message : 32026
      *-- No departments assigend to account xxx
      *-- Button : 00000
      *-- OK
      =gfModalGen('TRM32026B00000','ALERT',laData[2])
      STORE SPACE(5) TO laData[17]
      RETURN

    *B606908,1 Commented out. [Begin]
    *ENDIF
    *B606908,1 Commented out. [End]
    
    llBrowse = .F.
  ENDIF
  SELECT CustDept
  
  *B606908,1 Modify Department heading. [Begin]
  *lcBrFields = [Dept:H="Department",cDeptDesc :H="Description",Rep1,Comm1,Rep2,Comm2,cTermCode]
  lcBrFields = [Dept:H="League",cDeptDesc :H="Description",Rep1,Comm1,Rep2,Comm2,cTermCode]
  *B606908,1 Modify Department heading. [End]

  laData[17] = IIF(ARIABROW('laData[2]',"League",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Dept','laBrowArr'),;
                  CustDept.Dept,laData[17])
ENDIF
llBrowse = .F.

*C102118,1 Add a checking for the new selected Department. [Begin]
*IF laData[17] <> &lcOrdHdr..Dept
IF laData[17] <> &lcOrdHdr..Dept AND SEEK(laData[2]+laData[17],'CustDept')
*C102118,1 Add a checking for the new selected Department. [End]

  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Dept  WITH laData[17]
  UNLOCK
  *-- If valid epartment, Get new sales rep, commission and payment terms
  IF SEEK(laData[2]+laData[17],'CustDept')
    *-- Message : 32027
    *-- Customer defaults will be overwritten by the department defaults.
    *-- Button : 00000
    *-- OK
    *C102118,5 Change the condition to be on Edit and Add mode. [Begin]
    *IF laScrMode[4] .AND. gfModalGen('INM32027B00000','ALERT')=1
    IF gfModalGen('INM32027B00000','ALERT')=1
    *C102118,5 Change the condition to be on Edit and Add mode. [End]

      =RLOCK()
      REPLACE Rep1  WITH CustDept.Rep1 ,;
              Rep2  WITH CustDept.Rep1 ,;
              cTermCode WITH CustDept.cTermCode
      UNLOCK
      =gfwCodePop(@laCodes,'CTERMCODE','T')
      STORE CustDept.Rep1  TO laData[27]
      STORE CustDept.Rep2  TO laData[29]
      =lfvRep1() .AND. lfvRep2()
      STORE CustDept.Comm1 TO laData[28],lnSouComm1
      STORE CustDept.Comm2 TO laData[30],lnSouComm2
      SHOW GET laData[27]
      SHOW GET laData[28]
      SHOW GET laData[29]
      SHOW GET laData[30]
      =lfvCommision()
    ENDIF
  ENDIF
  llCUpdate = .T.

*C102118,1 Clear laData[17]. [Begin]
ELSE
  laData[17] = ''
*C102118,1 Clear laData[17]. [End]

ENDIF
=gfCloseFile('CustDept')
*-- End of lfvDept.

*!*******************************************************************************
*! Name      : lfUpdate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Update order header other information
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfUpdate()
*!*******************************************************************************
*
FUNCTION lfUpdate

IF UPDATE()
  SELECT (lcOrdHdr)
  =RLOCK()  
  GATHER FROM laData FIELDS &lcScFields
  UNLOCK
  llCUpdate = .T.
ENDIF
*-- End of lfUpdate.

*!*******************************************************************************
*! Name      : lfvRep1
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate first salesrep
*!*******************************************************************************
*! Calls     : REPCHK
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvRep1()
*!*******************************************************************************
*
FUNCTION lfvRep1

IF llBrowse .OR. (!EMPTY(laData[27]) AND !SEEK(laData[27],'SalesRep'))
  SELECT SALESREP
  laData[27] = IIF(llBrowse,SPACE(3),laData[27])
  lcRep = laData[27]
  DO REPCHK WITH lcRep
  laData[27] = lcRep
ENDIF  
llBrowse = .F.
SELECT (lcOrdHdr)
IF Rep1 <> laData[27]
  IF laSetups[11,2]='D'
    =IIF(laSetups[11,2]='D',gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH'),.T.)    
    IF SEEK(laData[27]+laData[15],'REP_DIV')
      STORE REP_DIV.Comm_Rate TO laData[28],lnSouComm1
    ELSE
      laData[28] = IIF(EMPTY(laData[27]),0,SalesRep.Comm)
    ENDIF
  ELSE
    laData[28] = IIF(EMPTY(laData[27]),0,IIF(Customer.salesrep # laData[27],SalesRep.Comm,CUSTOMER.Comm))
  ENDIF
  SELECT (lcOrdLine)
  lnRecno = RECNO()
  IF SEEK(&lcOrdHdr..CordType+&lcOrdHdr..Order)
    SCAN WHILE CordType+Order+STR(LineNo,6) = &lcOrdHdr..CordType+&lcOrdHdr..Order
      M.Comm1 = laData[28]
      =lfVComm('1')
    ENDSCAN  
  ENDIF  
  SELECT (LCOrdLine)
  LOCATE
  IF !EOF(lcOrdLine)
    GO lnRecNo
  ENDIF
  =IIF(laSetups[11,2]='D',gfCloseFile('REP_DIV'),.T.)  
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Rep1  WITH laData[27] ,;
          Comm1 WITH laData[28]
  UNLOCK
  llCUpdate = .T.
  SHOW GET laData[28]
  =lfRefresh(lcWinCh2)
ENDIF
*-- End of lfvRep1.

*!*******************************************************************************
*! Name      : lfvRep2
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate second salesrep
*!*******************************************************************************
*! Calls     : REPCHK
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvRep2()
*!*******************************************************************************
*
FUNCTION lfvRep2
IF llBrowse .OR. (!EMPTY(laData[29]) .AND. !SEEK(laData[29],'SalesRep'))
  laData[29] = IIF(llBrowse,SPACE(3),laData[29])
  lcRep = laData[29]
  DO REPCHK WITH lcRep
  laData[29] = lcRep
ENDIF  
llBrowse = .F.
IF &lcOrdHdr..Rep2 <> laData[29]
  IF laSetups[11,2]='D'
    =IIF(laSetups[11,2]='D',gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH'),.T.)    
    IF SEEK(laData[29]+laData[15],'REP_DIV')
      STORE REP_DIV.Comm_Rate TO laData[30],lnSouComm2
    ELSE
      laData[30] = IIF(EMPTY(laData[29]),0,SalesRep.Comm)
    ENDIF
  ELSE
    laData[30] = IIF(EMPTY(laData[29]),0,IIF(Customer.salesrep # laData[29],SalesRep.Comm,CUSTOMER.Comm2))
  ENDIF
  SELECT (lcOrdLine)
  lnRecno = RECNO()
  IF SEEK(&lcOrdHdr..CordType+&lcOrdHdr..Order)
    SCAN WHILE CordType+Order+STR(LineNo,6) = &lcOrdHdr..CordType+&lcOrdHdr..Order
      M.Comm2 = laData[30]
      =lfVComm('2')
  	ENDSCAN  
  ENDIF  
  SELECT (lcOrdLine)
  LOCATE
  IF !EOF(lcOrdLine)
    GO lnRecNo
  ENDIF
  =IIF(laSetups[11,2]='D',gfCloseFile('REP_DIV'),.T.)  
  SELECT (lcOrdHdr)
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Rep2  WITH laData[29] ,;
          Comm2 WITH laData[30]
  UNLOCK
  llCUpdate = .T.
  SHOW GET laData[30]
  =lfRefresh(lcWinCh2)
ENDIF
*-- End of lfvRep2.

*!*******************************************************************************
*! Name      : lfvWareCode
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate order warehouse code
*!*******************************************************************************
*! Calls     : gfBrowWare
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvWareCode()
*!*******************************************************************************
*
FUNCTION lfvWareCode

laData[31] = laWareHouses[lnWareHouse,2]
IF &lcOrdHdr..cWareCode <> laData[31]
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE cWareCode WITH laData[31]
  UNLOCK
  llCUpdate = .T.
ENDIF
*-- End of lfvWareCode.

*!*******************************************************************************
*! Name      : lfvCurrency
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid function for currency fields
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvCurrency()
*!*******************************************************************************
*
FUNCTION lfvCurrency
PRIVATE lcCurrency

IF llBrowse .OR.  laData[33] <> &lcOrdHdr..cCurrCode
  lcCurrency = laData[33]
  =gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'cCurrCode','SH')

  IF !gfCurrBrow(@lcCurrency)
    lcCurrency  = &lcOrdHdr..cCurrCode
  ENDIF
  laData[33] = lcCurrency
  IF laData[33] <> &lcOrdHdr..cCurrCode
    IF laData[33] = gcBaseCurr
      STORE 1 TO laData[50], laData[34]
    ELSE
      laData[34] = gfChkRate('laData[50]',laData[33],laData[8],.T.,gcAct_Comp,.F.,.T.)
      IF laData[34] = 0
        *-- Message : 00262
        *-- A valid xxx to xxx exchange rate could not be found on xxx.
        *-- Button : 00000
        *-- Ok
        =gfModalGen('INM00262B00000','ALERT',ALLTRIM(laData[33])+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(laData[8]))
        laData[33] = &lcOrdHdr..cCurrCode
        laData[34] = &lcOrdHdr..nExRate
      ENDIF  
    ENDIF
    llUpdated = .T.
    SELECT (lcOrdHdr)
    =RLOCK()    
    REPLACE cCurrCode WITH laData[33] ,;
            nExRate   WITH laData[34] ,;
            nCurrUnit WITH laData[50]
    UNLOCK
    llCUpdate = .T.
  ENDIF  
  =gfCloseFile('SycCurr')
  IF !llEditExRt OR laData[33]=gcBaseCurr
    SHOW GET laData[34] DISABLE
  ELSE
    SHOW GET laData[34] ENABLE
  ENDIF
ENDIF    
llBrowse = .F.
*-- End of lfvCurrency.

*!*******************************************************************************
*! Name      : lfvExRate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid function for currency exchange rate.
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvExRate()
*!*******************************************************************************
*
FUNCTION lfvExRate

IF laData[34] <=0
  *-- Message : 32028
  *-- The exchange rate must be greater than zero
  *-- Button : 00000 
  *-- Ok
  =gfModalGen('TRM32028B00000','ALERT')
  laData[34] = &lcOrdHdr..nExRate
ENDIF
IF &lcOrdHdr..nExRate <> laData[34]
  SELECT (lcOrdHdr)
  =RLOCK()  
  REPLACE nExRate WITH laData[34]
  UNLOCK
  llCUpdate = .T.
ENDIF
*-- End of lfvExRate.

*!*******************************************************************************
*! Name      : lfvNew
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : New Order line
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvNew()
*!*******************************************************************************
*
FUNCTION lfvNew
PARAMETERS llStores

IF SEEK("SO" + m.Order + STR(m.LineNo,6),lcT_BomVar)  
  lclastOrder= m.Order 
  lcLastLine = STR(m.LineNo,6)
ENDIF
STORE .T. TO llNewline,llAddLine
SCATTER MEMVAR BLANK MEMO
m.Style = lcLastStyle
m.Store = laData[3]
=SEEK(m.Style,'STYLE')
=SEEK(m.Style+laData[31],'STYDYE')

*C102118,1 SSE Commented out because all orders are Multi Store [Begin]
*IF llStores
*  SHOW GETS WINDOW 'SOMULTI2' DISABLE ONLY
*  SHOW GETS WINDOW 'SOMULTI3' DISABLE ONLY
*  SHOW GETS WINDOW 'SOMULTI4' DISABLE ONLY
*  SHOW GETS WINDOW 'SOMULTI5' DISABLE ONLY
*  =lfRefresh('SOMULTI2')
*  =lfRefresh('SOMULTI3')
*  =lfRefresh('SOMULTI4')
*ELSE
*  SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
*  SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
*  SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
*  =lfRefresh(lcWinCh32)
*  =lfRefresh(lcWinCh321)
*ENDIF
*IF llMultiSt .AND. !llStores
*  m.Store = lcLastStore
*  SHOW GET ibLStore ENABLE
*  SHOW GET m.Store  ENABLE
*  _CUROBJ = OBJNUM(m.Store)
*ELSE
*  SHOW GET ibStyle ENABLE
*  SHOW GET m.Style ENABLE
*  _CUROBJ = OBJNUM(m.Style)
*ENDIF
*C102118,1 SSE Commented out because all orders are Multi Store [End]

*C102118,1 Show Divsion and Team if Empty [Begin]
*SHOW GETS WINDOW (lcWinCh32)  ENABLE ONLY
m.Style = lcEmptySty
IF !EMPTY(lcDiv) AND !EMPTY(lcDiv)
  SHOW GETS WINDOW (lcWinCh32)  ENABLE ONLY
ELSE
  SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
ENDIF
*C102118,1 Show Divsion and Team if Empty [End]

SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
=lfRefresh(lcWinCh32)
=lfRefresh(lcWinCh321)

*C102118,1 Initialize lcWinCh32, lcWinCh322 screen variables. [Begin]
PRIVATE lnCurrRec
lnCurrRec = 0
IF !EOF(lcOrdLine)
  lnCurrRec = RECNO(lcOrdLine)
ENDIF
IF &lcOrdLine..cStyGroup = 'JY'
  =lfAdlYo()
ENDIF

SHOW WINDOW (lcOrdBrow) REFRESH SAME
IF !EMPTY(lcDiv) AND !EMPTY(lcTeam)
  llNewTeam = .F.

  SHOW GET PbNew ENABLE  
  SHOW GET lcDiv     DISABLE
  SHOW GET lcTeam    DISABLE

  *B606923,1 Enable update push button. [Begin]
  *SHOW GET PbUpDate  DISABLE
  SHOW GET PbUpdate ENABLE
  *B606923,1 Enable update push button. [End]

  SHOW GET PbRemove  DISABLE

  IF laScrMode[3] OR laScrMode[4]
    SHOW GET ibStyle ENABLE
    SHOW GET m.Style ENABLE
    _CUROBJ = OBJNUM(m.Style)
  ELSE
    SHOW GET ibStyle DISABLE
    SHOW GET m.Style DISABLE
  ENDIF  

  IF laScrMode[4]
    SHOW GET PbScope DISABLE
  ELSE
    SHOW GET PbScope ENABLE
  ENDIF
ELSE
  STORE '' TO lcDiv, lcTeam
  SHOW GET lcDiv   ENABLE
  SHOW GET lcTeam  ENABLE
  _CUROBJ = OBJNUM(lcDiv)
ENDIF  
*C102118,1 Initialize lcWinCh32, lcWinCh322 screen variables. [End]

*-- End of lfvNew.

*!*******************************************************************************
*! Name      : lfwCustPo
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : When function for order lines CustPOs
*!*******************************************************************************
*! Calls     : gfModalGen
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfwCustPo()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfwCustPo
lcOldCustPo = CustPo
*-- End of lfwCustPo.

*!*******************************************************************************
*! Name      : lfvLCustPo
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate order lines CustPOs
*!*******************************************************************************
*! Calls     : gfModalGen
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvLCustPo()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfvLCustPo
PRIVATE lnCurrTag,lcCurrExp,lcStore,lcCustPo

IF CustPo <> lcOldCustPo
  llCUpDate = .T.
  *-- Message : 32046
  *-- Change Custpo# for all lines in store xxxxx.
  *-- Button : 32000
  *-- Yes;No
  IF gfModalGen('QRM32046B32000','ALERT',store)=1
    lnCurrTag = VAL(SYS(21))
    lcCurrExp = EVAL(SYS(14,lnCurrTag))
    lcStore  = Store
    lcCustPo = CustPo
    SET ORDER TO TAG ORDLINST
    =SEEK(lcOrdType+laData[1]+lcStore)
    SCAN REST WHILE cOrdType+ORDER+STORE = lcOrdType+laData[1]+lcStore
      =RLOCK()
      REPLACE CustPo WITH lcCustPo ,;
              FLAG   WITH IIF(FLAG <> 'N','M',FLAG)
      UNLOCK
    ENDSCAN
    SET ORDER TO TAG lnCurrTag
    =SEEK(lcCurrExp)
  ELSE
    =RLOCK()
    REPLACE FLAG WITH IIF(FLAG <> 'N','M',FLAG)
    UNLOCK
  ENDIF
ENDIF
RETURN
*-- End of lfvLCustPo.

*!*******************************************************************************
*! Name      : lfvLStore
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Validate order line store
*!*******************************************************************************
*! Calls     : CUSBROW
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfvLStore()
*!*******************************************************************************
*C102118,1 This function in Detail Folder is not used anymore.
*
FUNCTION lfvLStore
IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF
IF llBrowse .OR. ;
   ( (!llAddLine .OR. !EMPTY(m.Store)) .AND. !SEEK('S'+laData[2]+m.Store,'Customer'))
  xStore = m.Store
  SELECT CUSTOMER

  IF !CUSBROWS(laData[2],.T.)
    STORE IIF(llAddLine,SPACE(8),Store) TO m.Store
  ELSE
    m.Store = xStore
  ENDIF
ENDIF
SELECT (lcOrdLine)
llBrowse = .F.
IF !EMPTY(m.Store) .AND. Customer.Status <> 'A'
  IF Customer.Status <> 'P'
    *-- Message : 32023
    *-- This a non-active xxxxx. Order entry is not allowed!
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32023B00000','ALERT','store'+'|'+IIF(lcOrdType='C','Contract','Order'))
    STORE IIF(llAddLine,SPACE(8),Store) TO m.Store
  ENDIF   
ENDIF
IF !llAddLine .AND. m.Store=Store
  RETURN
ENDIF
IF EMPTY(m.Store)
  SCATTER MEMVAR MEMO
  STORE .F.       TO llAddLine,llNewLine
  STORE SPACE(8) TO lcLastStore
  IF EMPTY(m.Store)
    SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
    SHOW GET pbNew      ENABLE

    *C102118,1 SSE Commented out because they are Removed from screen [Begin]
    *SHOW GET pbPacks    ENABLE
    *SHOW GET pbTemplate ENABLE
    *C102118,1 SSE Commented out because they are Removed from screen [End]
           
    _CUROBJ = OBJNUM(pbNew)
  ELSE
    SHOW GETS WINDOW (lcWinCh32)  ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) ENABLE ONLY
    _CUROBJ = OBJNUM(ibBrowOrd)

    *C102118,1 Enable Scope in View , Edit mode. [Begin]
    IF laScrMode[2] OR laScrMode[3]
      SHOW GET PbScope ENABLE
    ELSE
      SHOW GET PbScope DISABLE
    ENDIF
    *C102118,1 Enable Scope in View , Edit mode. [End]    
  ENDIF
  RETURN
ELSE
  IF llAddLine 
    SHOW GET ibStyle ENABLE
    SHOW GET m.Style ENABLE
    _CUROBJ = OBJNUM(m.Style)
  ELSE
    llCUpDate = .T.
    =RLOCK()
    REPLACE Store WITH m.Store ,;
            FLAG WITH IIF(FLAG <> 'N','M',FLAG)
    =lfAdrLine()
    UNLOCK
  ENDIF
ENDIF
*-- End of lfvLStore.

*!*******************************************************************************
*! Name      : lfvStyle
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate entered Styles
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvStyle()
*!*******************************************************************************
*
FUNCTION lfvStyle
PARAMETERS llStores

IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF
SELECT IIF(llStores,lcTempline,lcOrdLine)

IF (!lladdline .AND. llBrowse) .OR. laScrMode [3] .AND. !lladdline .AND. Style <> m.Style 
  DO CASE
    CASE PICKED 
     *-- Message : 32079
     *-- Order line already XXXX ; Cannot modify style.
     *-- Button  : 00000
     *-- Ok
     =gfModalGen('INM32079B00000','ALERT','piked')
     STORE Style TO m.Style
     _CUROBJ = OBJNUM(m.Style)
     llBrowse = .F.  
     RETURN

    CASE TOtQty <> Totbook
      =gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH')
      SELECT IIF(llStores,lcTempline,lcOrdLine)
      IF !SEEK('O'+Order+STR(lineno,6),'Ordcanln') .AND. Totbook <> Ordcanln.TotQty
        *-- Message : 32079
        *-- Order line already XXXX ; Cannot modify style.
        *-- Button  : 00000
        *-- Ok
        =gfModalGen('INM32079B00000','ALERT','shiped')
        =gfCloseFile('ORDCANLN')
        STORE Style TO m.Style
        _CUROBJ = OBJNUM(m.Style)
        llBrowse = .F.  
        RETURN
     ENDIF
  ENDCASE  
ENDIF

*C102118,1 SSE Commented out Because these variables must be defined in the begining [Begin]
*lcMaj      = gfItemMask('PM')             && Get the major of the style
*lnMajSize  = LEN(lcMaj)                   && Length of the major
*lnItemSz   = gfItemMask('PI')
*llExtSS    = gfGetMemVar('M_USEEXSSC')
*C102118,1 SSE Commented out Because these variables must be defined in the begining [End]

llCallEx   = .F.
IF llBrowse .OR. ;
  (!llAddLine .OR. m.Style <> lcEmptySty) 
  IF !SEEK(ALLTRIM(m.Style),'Style'))
    m.Style = PADR(gfStyBrw("I" , m.Style , "" , .F.),19)
    IF EMPTY(m.Style)
      STORE IIF(llAddLine,lcEmptySty,Style) TO m.Style
      _CUROBJ = OBJNUM(m.Style)
    ELSE
      llCallEx = llExtSS .AND. !llStores
      IF llCallEx .AND. llExtSS
        m.Style = LEFT(m.STYLE,LEN(m.Style)-3)
      ENDIF
    ENDIF
    llBrowse = .F.
  ELSE
    llOneCol = .F.
    IF m.Style # Style.Style .AND. LEFT(m.STYLE,lnMajSize) = LEFT(Style.Style ,lnMajSize)
      SKIP 1 IN STYLE
      IF LEFT(m.STYLE,lnMajSize) # LEFT(Style.Style ,lnMajSize)
        llOneCol = .T.
      ENDIF
      SKIP -1 IN STYLE
      m.Style  = IIF(llOneCol,STYLE.STYLE,m.Style)
    ENDIF
     
    IF !llOneCol
      IF !llStores .AND. m.Style # Style.Style .AND. llExtSS .AND. LEN(ALLTRIM(m.STYLE)) = LEN(lnItemSz)-3
        llCallEx = .T.
      ELSE
        m.Style = PADR(gfStyBrw("I" , m.Style , "" , .F.),19)
        IF EMPTY(m.Style)
          STORE IIF(llAddLine,lcEmptySty,Style) TO m.Style
          _CUROBJ = OBJNUM(m.Style)
        ENDIF
        llBrowse = .F.
      ENDIF
    ENDIF
  ENDIF
  
  IF llCallEx
    lnBrR    = 0
    DIMENSION laQtyArr[10,3]
    laQtyArr[1,1]  = 'QTY1'
    laQtyArr[1,2]  = 6
    laQtyArr[1,3]  = 0
    laQtyArr[2,1]  = 'QTY2'
    laQtyArr[2,2]  = 6
    laQtyArr[2,3]  = 0
    laQtyArr[3,1]  = 'QTY3'
    laQtyArr[3,2]  = 6
    laQtyArr[3,3]  = 0
    laQtyArr[4,1]  = 'QTY4'
    laQtyArr[4,2]  = 6
    laQtyArr[4,3]  = 0
    laQtyArr[5,1]  = 'QTY5'
    laQtyArr[5,2]  = 6
    laQtyArr[5,3]  = 0
    laQtyArr[6,1]  = 'QTY6'
    laQtyArr[6,2]  = 6
    laQtyArr[6,3]  = 0
    laQtyArr[7,1]  = 'QTY7'
    laQtyArr[7,2]  = 6
    laQtyArr[7,3]  = 0
    laQtyArr[8,1]  = 'QTY8'
    laQtyArr[8,2]  = 6
    laQtyArr[8,3]  = 0
    laQtyArr[9,1]  = 'TOTQTY'
    laQtyArr[9,2]  = 7
    laQtyArr[9,3]  = 0
    laQtyArr[10,1]  = 'PPQTY'
    laQtyArr[10,2]  = 4
    laQtyArr[10,3]  = 0
    
    DIMENSION laTitArr[6,2]
    laTitArr[1,1] = 'STYLE'
    laTitArr[1,2] = lcStyHdr
    laTitArr[2,1] = 'SCALE'
    laTitArr[2,2] = 'S'
    laTitArr[3,1] = 'CPrepak'
    laTitArr[3,2] = 'P'
    laTitArr[4,1] = 'CPPQTY'
    laTitArr[4,2] = 'P. Qty.'
    laTitArr[5,1] = 'PACK_ID'
    laTitArr[5,2] = 'Pack'
    laTitArr[6,1] = 'Group'
    laTitArr[6,2] = 'Group'
    
    lcQtyPic = '999999'
    lcBrowStr = "cMk=IIF(RECNO()=lnBrR,'',' '):1:H=' ':W=.F.,Scale :H='Scl':R:W=.F.,Prepak:H='P':W=lfwOV():V=lfvPrePack(.F.,.T.):P='!'," +;
      "cPpQty:H='P. Qty.':W=lfwOV():V=lfCtoN().AND.lfvPPakQty(.F.,.T.):P='9999'," +;
      "cTotQty:H='TOT QTY':W=lfwOV():P='9999999':V=lfCtoN().AND.lfvPieces(.F.,.T.)," 
    
    lcBrowStr = lcBrowStr +;
      "cQty1:H='Qty1':V=lfCtoN():P=lcQtyPic, cQty2:H='Qty2':W=laScl[2]:V=lfCtoN():P=lcQtyPic,cQty3:H='Qty3':W=laScl[3]:V=lfCtoN():P=lcQtyPic,cQty4:H='Qty4':W=laScl[4]:V=lfCtoN():P=lcQtyPic,"
    lcBrowStr = lcBrowStr +;
      "cQty5:H='Qty5':W=laScl[5]:V=lfCtoN():P=lcQtyPic,cQty6:H='Qty6':W=laScl[6]:V=lfCtoN():P=lcQtyPic,cQty7:H='Qty7':W=laScl[7]:V=lfCtoN():P=lcQtyPic,cQty8:H='Qty8':W=laScl[8]:V=lfCtoN():P=lcQtyPic," +;
      "Gros_Price:H='GPrice':W=lfwOV().AND.llESTYPSO:V=lfvF(),"  +;
      "DISC_PCNT:H='Disc':W=lfwOV().AND.llESTYPSO:V=lfvF() ,Price:H='Price':W=lfwOV().AND.llESTYPSO:V=lfvF()" +;
      IIF(EMPTY(laData[27]), "" , ",Comm1:H='Rep':W=lfwOV():V=lfvF()" ) +;
      IIF(EMPTY(laData[29]),"",",Comm2:H='Rep2':W=lfwOV():V=lfvF()")+;
      IIF(laSetups[1,2]='Y',",Pack_id:H='Pack':W=lfwOV():V=lfvStyPack(.F.,.T.)",'')+;
      ",Group :P='!' :V=lfvf():F"

    SET ORDER TO TAG ORDLINST IN (lcOrdLine)
      
    DO (gcAppHome+'SOESSQTY.FXP') WITH ALLTRIM(m.Style),m.store,;
     IIF(llStores,lcTempline,lcOrdLine), lcBrowStr,lcOrdType+laData[1]+m.Store,laQtyArr,laTitArr,'SO'
     SELECT IIF(llStores,lcTempline,lcOrdLine)
    llReBrowse = .T.
    =lfActFolder()
    RETURN
  ENDIF
ENDIF
IF !llAddLine .AND. m.Style=Style
  RETURN
ENDIF
IF m.Style=lcEmptySty
  STORE m.Style TO lcLastStyle
  SCATTER MEMVAR MEMO
  STORE .F.       TO llAddLine,llNewLine
  STORE SPACE(08) TO lcLastStore
  IF EMPTY(m.Style)
    IF llStores
      SHOW GETS WINDOW 'SOMULTI2' DISABLE ONLY
      SHOW GETS WINDOW 'SOMULTI3' DISABLE ONLY
      SHOW GETS WINDOW 'SOMULTI4' DISABLE ONLY
      SHOW GETS WINDOW 'SOMULTI5' DISABLE ONLY
      SHOW GET ibInv200E ENABLE
      SHOW GET pbClose   ENABLE
    ELSE
      SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY

      *C102118,1 SSE Commented out because it is Removed from screen [Begin]
      *IF llMultiSt 
      *  SHOW GET pbTemplate ENABLE 
      *ENDIF
      *C102118,1 SSE Commented out because it is Removed from screen [End]      
      
    ENDIF
    SHOW GET pbNew   ENABLE
    
    *C102118,1 SSE Commented out because it is Removed from screen [Begin]
    *SHOW GET pbPacks ENABLE
    *C102118,1 SSE Commented out because it is Removed from screen [End]
    
    _CUROBJ = OBJNUM(pbNew)
  ELSE
    IF llStores
      SHOW GETS WINDOW 'SOMULTI2' ENABLE ONLY
      SHOW GETS WINDOW 'SOMULTI3' ENABLE ONLY
      SHOW GETS WINDOW 'SOMULTI4' ENABLE ONLY
      SHOW GETS WINDOW 'SOMULTI5' ENABLE ONLY
      =lfAdjButt('')
    ELSE
      SHOW GETS WINDOW (lcWinCh32)  ENABLE ONLY
      SHOW GETS WINDOW (lcWinCh321) ENABLE ONLY
      SHOW GETS WINDOW (lcWinCh322) ENABLE ONLY

      *C102118,1 Enable Scope in View , Edit mode. [Begin]
      IF laScrMode[2] OR laScrMode[3]
        SHOW GET PbScope ENABLE
      ELSE
        SHOW GET PbScope DISABLE
      ENDIF
      *C102118,1 Enable Scope in View , Edit mode. [End]
      
      *C102118,1 SSE Commented out because they are Removed from screen [Begin]
      *IF !llMultiSt
      *  SHOW GET pbTemplate DISABLE
      *  SHOW GET ibLStore   DISABLE
      *  SHOW GET m.Store    DISABLE
      *ENDIF
      *C102118,1 SSE Commented out because they are Removed from screen [End]
              
    ENDIF
    IF lContract
      SHOW GET m.Price      DISABLE
  
      *C102118,1 SSE Commented out because they are Removed from screen [Begin]
      *SHOW GET m.Gros_Price DISABLE
      *SHOW GET m.Disc_Pcnt  DISABLE
      *C102118,1 SSE Commented out because they are Removed from screen [End]      
    ENDIF
    _CUROBJ = IIF(llStores,OBJNUM(ibInv200E),OBJNUM(ibBrowOrd))
  ENDIF
  RETURN
ENDIF

IF SEEK(m.Style,'Style')
  *-- Message : 32017
  *-- Style scale not found in the scale file. Cannot accept.
  *-- Button : 00000 
  *-- Ok

  *-- Message : 32018
  *-- This is a canceled style. Cannot accept.
  *-- Button : 00000 
  *-- Ok

  *-- Message : 32019
  *-- This style/color is on hold. 
  *-- Button : 32003
  *-- Accept Reenter

  *-- Message : 32020
  *-- Styles restricted to XXX!
  *-- Button : 00000
  *-- Ok

  *-- Message : 32021
  *-- Style XXX date is 
  *-- Button : 32003
  *-- Accept Reenter

  SET RELATION OFF INTO STYLE
  IF (!SEEK('S'+Style.Scale,'Scale') .AND. ;
      gfModalGen('TRM32017B00000','ALERT')=1) ;
   .OR. (Style.Status='X' .AND. ;
        gfModalGen('TRM32018B00000','ALERT')=1) ;
   .OR. (Style.Status='H' .AND. ;
        gfModalGen('QRM32019B32003','ALERT')=2) ;
   .OR. (Style.cDivision <> laData[15] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;
   .OR. (ALLTRIM(laData[14])<>'*' AND TRIM(Style.Season)<>'Y' AND Style.Season<>laData[14] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','season '+ALLTRIM(laSeasons[lnSeason,1]))=1) ;
   .OR. (!EMPTY(Style.Start) .AND. Style.Start > laData[9] .AND. ;
        gfModalGen('QRM32021B32003','ALERT','start|'+DTOC(Style.Start))=2) ;
   .OR. (!EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[9] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2)
    STORE IIF(llAddLine,lcEmptySty,Style) TO m.Style

    SET RELATION TO STYLE INTO STYLE ADDITIVE
    _CUROBJ = OBJNUM(m.Style)
    RETURN
  ENDIF
  m.lContract = lcOrdType<>'C' .AND. lfvContPri(m.Style,'m.Gros_Price','m.Price','m.Disc_Pcnt')
  IF !m.lContract
    
    *C102118,1 Display Style Price screen if Style Group isn't Jerseys, Shorts, Socks [Begin]
    *m.Gros_Price = lfGetprice(m.Style,lcPriceLvl,m.TotQty)
    IF Style.cStyGroup <> "JY" AND Style.cStyGroup <> "SH" AND Style.cStyGroup <> "SO"
      m.Gros_Price = lfGetprice(m.Style,lcPriceLvl,m.TotQty)
    ENDIF  
    *C102118,1 Display Style Price screen if Style Group isn't Jerseys, Shorts, Socks [End]
    
  ENDIF
  
  SELECT IIF(llStores,lcTempline,lcOrdLine)
  IF m.Gros_Price < 0
    STORE IIF(llAddLine,lcEmptySty,Style) TO m.Style
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    _CUROBJ = OBJNUM(m.Style)
    RETURN
  ENDIF
  IF laSetups[5,2]='Y' .AND. !SEEK(m.Style+laData[31]+SPACE(10),'StyDye')
    *-- Message : 40012
    *-- Style/color xxx is not assigned to warehouse xxx
    *-- Button : 40002
    *-- Add Reenter
    IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.Style)+'|'+TRIM(laData[31]))=1
      DO gpAdStyWar WITH m.Style,SPACE(10),laData[31]
    ELSE
      STORE IIF(llAddLine,lcEmptySty,&lcOrdLine..Style) TO m.Style
      SET RELATION TO STYLE INTO STYLE ADDITIVE
      _CUROBJ = OBJNUM(m.Style)
      RETURN
    ENDIF
  ENDIF
  m.Desc1    = Style.Desc1
  m.Scale    = Style.Scale
  m.PrePak   = Style.PrePak
  m.Comm1    = IIF(Style.Commission,laData[28],0)
  m.Comm2    = IIF(Style.Commission,laData[30],0) 
  m.Season   = Style.Season
  m.Gl_Sales = ALLTRIM(laData[53])+Style.cSlsGlLink
  
  m.Gl_Sales = IIF(laSetups[4,2]='Y' AND SEEK('02'+m.Gl_Sales,'Gl_Link'),m.Gl_Sales,'DEFDEF')
  m.Flag     = IIF(llStores,'',IIF(llAddLine OR m.Flag='N','N','M'))

  *C102118,1 Getting values of the 4 added fields to lcOrdLine file. [Begin]
  m.Youth   = IIF(m.cStyGroup='JY',m.Qty1+m.Qty2+m.Qty3,0)
  m.Adult   = IIF(m.cStyGroup='JY',m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,0)
  m.YouthPr = IIF(m.cStyGroup='JY',lnYouthPr,0)
  m.AdultPr = IIF(m.cStyGroup='JY',lnAdultPr,0)
  m.cStyGroup  = Style.cStyGroup
  m.Store      = lcDiv + lcTeam
  *C102118,1 Getting values of the 4 added fields to lcOrdLine file. [End]
  
  IF !m.lContract
    *-- get the cDiscCode From stydye in every case
    lcDiscCode  = IIF(SEEK(m.Style+laData[31]+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
    m.Disc_Pcnt = 0 
    IF !EMPTY(ALLTRIM(lcDiscCode))
      *-- Get the disecound related filed to now which 
      *-- type whole Sale Or Retail sale Or Both.
      DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
      STORE '' To lcDisType , ldstartDte ,ldEndDate
      *-- Array to get the Discount affect for DecCode.
      laDisType[1,1]  = 'CCOSTAFECT'
      laDisType[1,2]  = 'lcDisType'
      *-- Array to get the start date For DescCode.
      lastartDte[1,1] = 'START'
      lastartDte[1,2] = 'ldstartDte'
      *-- Array to get the end date For DescCode.
      laEndDate[1,1]  = 'DENDATE'
      laEndDate[1,2]  = 'ldEndDate'
      = gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
      = gfRltFld(lcDiscCode, @lastartDte, 'CDISCCODE')
      = gfRltFld(lcDiscCode , @laEndDate, 'CDISCCODE')
      lnDisc_Pcnt = 0
      IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[8],ldstartDte,ldEndDate)
        lnDisc_Pcnt = m.Disc_Pcnt
        =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
        m.Disc_Pcnt = lnDisc_Pcnt
      ENDIF
    ENDIF  
    m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100 
  ENDIF
  SKIP IN STYLE
  lcLastStyle=STYLE.Style
  SELECT IIF(llStores,lcTempline,lcOrdLine)
  SET ORDER TO TAG ORDLINST
  IF SEEK(lcOrdType+laData[1]+m.Store+m.Style)
    *-- Message : 32029
    *-- This style/color had been entered on this order
    *-- Button : 00000
    *-- Ok
    =gfModalGen('INM32029B00000','ALERT',IIF(lcOrdType='C','contract','order'))
  ENDIF
  SET ORDER TO TAG ORDLINE
  =SEEK(lcOrdType+laData[1]+STR(m.LineNo,6))
  IF llAddLine
    lnLines     = lnLines + 1
    m.LineNo    = lnLines
    m.Account   = laData[2]
    m.Order     = laData[1]
    m.cOrdType  = lcOrdType
    m.cWareCode = laData[31]
    m.Complete  = laData[10]
    
    *C102118,1 SSE Commented out Because they don't use bulk orders [Begin]    
    *IF llMultiSt .AND. llFromBulk
    *  lcAlias1 = SELECT()
    *  SELECT (lcTempline)
    *  lcCurOrd = ORDER()
    *  lcCurRec = RECNO()
    *  SET ORDER TO TAG ordlinst
    *  IF SEEK(m.cOrdType + Order + Store + m.Style)
    *    m.cFromOrder = &lcTempline..cFromOrder
    *    m.BulkLineNo = &lcTempline..BulkLineNo
    *  ENDIF
    *  SET ORDER TO &lcCurOrd 
    *  IF BETWEEN(lcCurRec ,1,RECCOUNT()) 
    *  	GOTO lcCurRec 
    *  ENDIF
    *  SELECT (lcAlias1)
    *ENDIF
    *C102118,1 SSE Commented out Because they don't use bulk orders [End]

    *C102118,1 Multi Store Check Box is removed from Screen [Begin]     
    *SHOW GET llMultiSt DISABLE
    *C102118,1 Multi Store Check Box is removed from Screen [End] 
   
    APPEND BLANK
  ELSE
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      STORE IIF(lnCount > Scale.cnt,0,Qty&lcCount)  TO m.Qty&lcCount
      STORE IIF(lnCount > Scale.cnt,0,Book&lcCount) TO m.Book&lcCount
    ENDFOR
    m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
    m.TotBook= m.Book1+m.Book2+m.Book3+m.Book4+m.Book5+m.Book6+m.Book7+m.Book8
    IF !EMPTY(m.Prepak) AND SEEK('P'+m.Scale+m.Prepak,'Scale') AND ;
       MOD(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,Scale.PPTot) <> 0 
      m.Prepak=''
      m.PpQty = 0 
    ENDIF
    =SEEK('S'+m.Scale,'Scale')    
    =lfvNPrice(llStores) AND lfChkTotQty(llStores)
  ENDIF
  =lfAdrLine()

  *C102118,1 Adjust the record before replacing. [Begin]
  STORE 0 TO m.Size1, m.Size2, m.Size3, m.Size4, m.Size5, m.Size6, m.Size7, m.Size8, m.TotSize
  IF !llAddLine 
    SET ORDER TO TAG ORDLINE
    =SEEK(lcOrdType+laData[1]+STR(m.LineNo,6))
  ENDIF  
  *C102118,1 Adjust the record before replacing. [End] 

  =RLOCK()
  GATHER MEMVAR
  
  UNLOCK
  
  *C102118,1 Check for JY Group [Begin]
  *-- If Group is JY add another extra line number to Order Line.
  IF &lcOrdLine..cStyGroup = "JY"
    lnLines = lnLines + 1
  ENDIF
  *C102118,1 Check for JY Group [End]
  
  llAddLine  = .F.
  llCUpdate  = .T.
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  IF llStores
    SHOW WINDOW (lcBrTtlM) REFRESH SAME
    SHOW GETS WINDOW 'SOMULTI2' ENABLE ONLY
    SHOW GETS WINDOW 'SOMULTI3' ENABLE ONLY
    SHOW GETS WINDOW 'SOMULTI4' ENABLE ONLY
    SHOW GETS WINDOW 'SOMULTI5' ENABLE ONLY
    =lfAdjButt('')
    =lfRefresh('SOMULTI2')
    =lfRefresh('SOMULTI3')
    =lfRefresh('SOMULTI4')
  ELSE
    SHOW WINDOW (lcOrdBrow) REFRESH SAME
    SHOW GETS WINDOW (lcWinCh32)  ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) ENABLE ONLY

    *C102118,1 Enable Scope in View , Edit mode. [Begin]
    IF laScrMode[2] OR laScrMode[3]
      SHOW GET PbScope ENABLE
    ELSE
      SHOW GET PbScope DISABLE
    ENDIF
    *C102118,1 Enable Scope in View , Edit mode. [End]
    
    =lfRefresh(lcWinCh32)
    =lfRefresh(lcWinCh321)
  ENDIF

  *C102118,1 SSE Commented out because they are Removed from screen [Begin]    
  *IF !llMultiSt
  *  SHOW GET pbTemplate DISABLE
  *  SHOW GET ibLStore   DISABLE
  *  SHOW GET m.Store    DISABLE
  *ENDIF
  *C102118,1 SSE Commented out because they are Removed from screen [End]        

  IF lContract .OR. !llESTYPSO
    SHOW GET m.Price      DISABLE

    *C102118,1 SSE Commented out because they are Removed from screen [Begin]
    *SHOW GET m.Gros_Price DISABLE
    *SHOW GET m.Disc_Pcnt  DISABLE
    *C102118,1 SSE Commented out because they are Removed from screen [End]    
  ENDIF

  *C102118,1 Show Scale and Group from Style file [Begin]
  IF m.cStyGroup = 'JY'
    lcInkColor = Style.Pattern
  ENDIF

  IF m.cStyGroup = "JY" OR m.cStyGroup = "SH" OR m.cStyGroup = "SO"
    m.Price = 0 
    SHOW GET m.Price DISABLE
    REPLACE Price WITH 0 , Gros_Price WITH 0
  ELSE
    m.Price  = Style.PriceA
    SHOW GET m.Price ENABLE
  ENDIF
  SHOW GET m.cStyGroup DISABLE
  SHOW GET m.Scale DISABLE
  *C102118,1 Show Scale and Group from Style file [End]
  
ENDIF

*C102118,1 SSE Enable/Disable Printing, Tracking, Invoice Push buttons. [Begin]
DO CASE
  CASE laScrMode[2]
    SHOW GET PbInvoice ENABLE
    SHOW GET PbPrint   ENABLE
    SHOW GET PbDTrack  ENABLE
  CASE laScrMode[3]
    SHOW GET PbInvoice DISABLE
    SHOW GET PbPrint   ENABLE
    SHOW GET PbDTrack  ENABLE
  CASE laScrMode[4]
    SHOW GET PbInvoice DISABLE
    SHOW GET PbDTrack  DISABLE
    IF !EMPTY(lcDiv) AND !EMPTY(lcTeam)
      SHOW GET PbPrint ENABLE
    ELSE
      SHOW GET PbPrint DISABLE
    ENDIF  
ENDCASE

IF !EMPTY(lcDiv) AND !EMPTY(lcTeam)
  SHOW GET lcDiv  DISABLE
  SHOW GET lcTeam DISABLE
ELSE
  SHOW GET lcDiv  ENABLE
  SHOW GET lcTeam ENABLE
ENDIF
=lfAdlYo()
*C102118,1 SSE Enable/Disable Printing, Tracking, Invoice Push buttons. [End]

*-- End of lfvStyle.

*!*******************************************************************************
*! Name      : lfvStyGrp
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style group
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvStyGrp()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfvStyGrp
PARAMETERS llStores

IF Group <> m.Group
  llCUpdate = .T.
  =RLOCK()
  REPLACE Group WITH m.Group
  UNLOCK
  IF llStores
    SHOW WINDOW (lcBrTtlM) REFRESH SAME
  ELSE
    =RLOCK()
    REPLACE Flag WITH IIF(Flag='N','N','M')
    UNLOCK
    SHOW WINDOW (lcOrdBrow) REFRESH SAME
  ENDIF
ENDIF
*-- End of lfvStyGrp.

*!*******************************************************************************
*! Name      : lfvPrePack
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style prepak
*!*******************************************************************************
*! Calls     : gfPrePBrow
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvPrePack()
*!*******************************************************************************
*C102118,1 This function in Detail Folder is not used anymore.
*
FUNCTION lfvPrePack
PARAMETERS llStores,llFrmGBrw
PRIVATE lcBrFields,lcFile_Ttl
IF llFrmGBrw
  m.Prepak = EVAL(VARREAD())
ENDIF

IF llBrowse .OR. (!EMPTY(m.Prepak) .AND. !SEEK('P'+m.Scale+m.Prepak,'Scale'))
  =gfPrePBrow(Scale,@m.Prepak)
ENDIF
llBrowse = .F.
=SEEK('S'+m.Scale,'Scale')
IF (!llFrmGBrw .AND. Prepak <> m.Prepak) .OR. (llFrmGBrw .AND. Prepak <> lcOldVal)
  llCUpdate = .T.
  =RLOCK()
  REPLACE Prepak WITH m.Prepak

  IF llFrmGBrw
    REPLACE cStatus WITH IIF(cStatus='N',cStatus,'M')
  ENDIF

  UNLOCK
  IF llStores
    SHOW GETS WINDOW 'SOMULTI2' ONLY
    SHOW GETS WINDOW 'SOMULTI3' ONLY
    SHOW GETS WINDOW 'SOMULTI4' ONLY
    SHOW GETS WINDOW 'SOMULTI5' ONLY
  ELSE
    =RLOCK()
    REPLACE Flag WITH IIF(Flag='N','N','M')
    UNLOCK
    SHOW GETS WINDOW (lcWinCh32)  ONLY
    SHOW GETS WINDOW (lcWinCh321) ONLY
    SHOW GETS WINDOW (lcWinCh322) ONLY
    
    *C102118,1 Enable Scope in View , Edit mode. [Begin]
    IF laScrMode[2] OR laScrMode[3]
      SHOW GET PbScope ENABLE
    ELSE
      SHOW GET PbScope DISABLE
    ENDIF
    *C102118,1 Enable Scope in View , Edit mode. [End]
    
  ENDIF
ENDIF
IF EMPTY(Prepak)
  =RLOCK()
  REPLACE PPQty WITH 0

  IF llFrmGBrw
    REPLACE cPPqty WITH '0',;
            cStatus WITH IIF(cStatus='N',cStatus,'M')
  ENDIF
  UNLOCK
  m.PPQTY = 0

*C102118,1 SSE Commented out because it is Removed from screen [Begin]
  *SHOW GET m.PPQty DISABLE
*ELSE
*  SHOW GET m.PPQty ENABLE
*C102118,1 SSE Commented out because it is Removed from screen [End]
ENDIF
*-- End of lfvPrePack.

*!*******************************************************************************
*! Name      : lfvPPakQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style prepak quantity
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvPPakQty()
*!*******************************************************************************
*C102118,1 This function in Detail Folder is not used anymore.
*
FUNCTION lfvPPakQty
PARAMETERS llStores,llFrmGBrw
IF (!llFrmGBrw .AND. m.PPQty = PPQty) .OR. (llFrmGBrw .AND. m.PPQTY = VAL(lcOldVal) )
  RETURN
ENDIF
IF m.PPQty <> 0
  =SEEK('P'+m.Scale+m.Prepak,'Scale')
  IF !llStores .AND. Picked .AND. ;
     (m.PPQty*Scale.PP1 < Pik1 .OR. ;
      m.PPQty*Scale.PP2 < Pik2 .OR. ;
      m.PPQty*Scale.PP3 < Pik3 .OR. ;
      m.PPQty*Scale.PP4 < Pik4 .OR. ;
      m.PPQty*Scale.PP5 < Pik5 .OR. ;
      m.PPQty*Scale.PP6 < Pik6 .OR. ;
      m.PPQty*Scale.PP7 < Pik7 .OR. ;
      m.PPQty*Scale.PP8 < Pik8)
    *-- Message : 32015
    *-- Quantity cannot be below than what has been picked for this style/color
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32015B00000','ALERT')
    STORE PPQty TO m.PPQty
    =SEEK('S'+m.Scale,'Scale')
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  
  lnPPQ1 = Scale.PP1
  lnPPQ2 = Scale.PP2
  lnPPQ3 = Scale.PP3
  lnPPQ4 = Scale.PP4
  lnPPQ5 = Scale.PP5
  lnPPQ6 = Scale.PP6
  lnPPQ7 = Scale.PP7
  lnPPQ8 = Scale.PP8
  
  =SEEK('S'+m.Scale,'Scale')
  FOR lnI = 1 TO 8
    lcCnt = STR(lnI,1)
    m.Qty&lcCnt = m.PPQty * lnPPQ&lcCnt
    =lfvSizeQty(lcCnt,llStores)
  ENDFOR
  
  m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8

  *C102118,1 SSE Commented out because it is Removed from screen [Begin]
  *SHOW GET m.TotQty DISABLE
  *C102118,1 SSE Commented out because it is Removed from screen [End]
  
  =SEEK('S'+m.Scale,'Scale')
ELSE
  IF !Picked
    STORE 0 TO m.TotQty
    
    *C102118,1 SSE Commented out because it is Removed from screen [Begin]
    *SHOW GET m.TotQty ENABLE
    *C102118,1 SSE Commented out because it is Removed from screen [End]
  ENDIF
ENDIF
IF llFrmGBrw
  SELECT (lcTmpSize)
  =lfChkTQty()
ELSE
  SELECT IIF(llStores,lcTempLine,lcOrdLine)
  =lfChkTotQty(llStores)
ENDIF

=RLOCK()
REPLACE PPQty WITH m.PPQty ,;
        FLAG  WITH IIF(llStores,Flag,IIF(Flag='N','N','M'))
UNLOCK  
IF llFrmGBrw
  FOR lnC = 1 TO ALEN(laQtyArr,1)
    m.c&laQtyArr[lnC,1] = STR(m.&laQtyArr[lnC,1],laQtyArr[lnC,2]+IIF(laQtyArr[lnC,3]>0,1,0),laQtyArr[lnC,3])
  ENDFOR
  m.cStatus = IIF(&lcTmpSize..cStatus='N','N','M')
  SELECT (lcTmpSize)
  GATHER MEMVAR
ELSE
  IF llStores
    SHOW GETS WINDOW 'SOMULTI2' ONLY
    SHOW GETS WINDOW 'SOMULTI3' ONLY
    SHOW GETS WINDOW 'SOMULTI4' ONLY
    SHOW GETS WINDOW 'SOMULTI5' ONLY
    =lfRefresh('SOMULTI2')
    =lfRefresh('SOMULTI3')
    =lfRefresh('SOMULTI4')
  ELSE
    SHOW GETS WINDOW (lcWinCh32)  ONLY
    SHOW GETS WINDOW (lcWinCh321) ONLY
    SHOW GETS WINDOW (lcWinCh322) ONLY

    *C102118,1 Enable Scope in View , Edit mode. [Begin]
    IF laScrMode[2] OR laScrMode[3]
      SHOW GET PbScope ENABLE
    ELSE
      SHOW GET PbScope DISABLE
    ENDIF
    *C102118,1 Enable Scope in View , Edit mode. [End]
    
    =lfRefresh(lcWinCh321)
  ENDIF
ENDIF
*-- End of lfvPPakQty.

*!*******************************************************************************
*! Name      : lfvPieces
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style pieces
*!*******************************************************************************
*! Calls     : gfModalGen
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvPieces()
*!*******************************************************************************
*C102118,1 This function in Detail Folder is not used anymore.
*
FUNCTION lfvPieces
PARAMETERS llStores,llFrmGBrw

IF m.TotQty < 0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.TotQty = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF EMPTY(m.Prepak) 
  IF  !llFrmGBrw .AND. Scale.Cnt > 1 
    RETURN
  ELSE
    IF llFrmGBrw .AND. EVAL(lcTmpScl+'.Cnt') > 1
        RETURN
    ENDIF
  ENDIF
ENDIF
IF !EMPTY(m.Prepak) .AND. SEEK('P'+m.Scale+m.Prepak,'Scale')
  *-- Message : 32030
  *-- Total quantity is not evenly divisible by the prepak quantity xxx
  *-- Button : 00000 
  *-- Ok
  IF MOD(m.TotQty,Scale.PPTot) <> 0 .AND. ;
    gfModalGen('TRM32030B00000','ALERT',STR(Scale.PPTot,4))=1
    IF llFrmGBrw
      REPLACE cTotQty WITH lcOldVal,;
              TotQty  WITH VAL(lcOldVal)
    ENDIF
    _CUROBJ = _CUROBJ
    =SEEK('S'+m.Scale,'Scale')
    RETURN
  ENDIF
  m.Qty1 = m.TotQty*Scale.PP1/Scale.PPTot
  m.Qty2 = m.TotQty*Scale.PP2/Scale.PPTot
  m.Qty3 = m.TotQty*Scale.PP3/Scale.PPTot
  m.Qty4 = m.TotQty*Scale.PP4/Scale.PPTot
  m.Qty5 = m.TotQty*Scale.PP5/Scale.PPTot
  m.Qty6 = m.TotQty*Scale.PP6/Scale.PPTot
  m.Qty7 = m.TotQty*Scale.PP7/Scale.PPTot
  m.Qty8 = m.TotQty*Scale.PP8/Scale.PPTot
  IF Picked .AND.  ;
     (m.Qty1 < Pik1 .OR. m.Qty2 < Pik2 .OR. m.Qty3 < Pik3 .OR. m.Qty4 < Pik4 .OR. ;
      m.Qty5 < Pik5 .OR. m.Qty6 < Pik6 .OR. m.Qty7 < Pik7 .OR. m.Qty8 < Pik8)
    *-- Message : 32015
    *-- Quantity cannot be below than what has been picked for this style/color
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32015B00000','ALERT')
    SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
    _CUROBJ = _CUROBJ
    =SEEK('S'+m.Scale,'Scale')
    RETURN
  ENDIF
ELSE
  IF Scale.Cnt = 1
    m.Qty1 = m.TotQty
  ENDIF
ENDIF
=SEEK('S'+m.Scale,'Scale')
IF m.Qty1=Qty1 AND m.Qty2=Qty2 AND m.Qty3=Qty3 AND m.Qty4=Qty4 AND ;
   m.Qty5=Qty5 AND m.Qty6=Qty6 AND m.Qty7=Qty7 AND m.Qty8=Qty8 AND m.TotQty=TotQty
  RETURN
ENDIF
IF llFrmGBrw
  =lfChkTQty()
ELSE
  =lfChkTotQty(llStores)
ENDIF

IF llFrmGBrw
  m.cQty1 = STR(m.Qty1,6)
  m.cQty2 = STR(m.Qty2,6)
  m.cQty3 = STR(m.Qty3,6)
  m.cQty4 = STR(m.Qty4,6)
  m.cQty5 = STR(m.Qty5,6)
  m.cQty6 = STR(m.Qty6,6)
  m.cQty7 = STR(m.Qty7,6)
  m.cQty8 = STR(m.Qty8,6)
  m.cStatus = IIF(&lcTmpSize..cStatus='N','N','M')
  SELECT (lcTmpSize)
  GATHER MEMVAR
ENDIF
IF llStores
  SHOW GETS WINDOW 'SOMULTI2' ONLY
  SHOW GETS WINDOW 'SOMULTI3' ONLY
  SHOW GETS WINDOW 'SOMULTI4' ONLY
  SHOW GETS WINDOW 'SOMULTI5' ONLY
  =lfRefresh('SOMULTI2')
  =lfRefresh('SOMULTI3')
  =lfRefresh('SOMULTI4')
ELSE
  SHOW GETS WINDOW (lcWinCh32)  ONLY
  SHOW GETS WINDOW (lcWinCh321) ONLY
  SHOW GETS WINDOW (lcWinCh322) ONLY

  *C102118,1 Enable Scope in View , Edit mode. [Begin]
  IF laScrMode[2] OR laScrMode[3]
    SHOW GET PbScope ENABLE
  ELSE
    SHOW GET PbScope DISABLE
  ENDIF
  *C102118,1 Enable Scope in View , Edit mode. [End]
  
  =lfRefresh(lcWinCh32)
  =lfRefresh(lcWinCh321)
ENDIF
*-- End of lfvPieces

*!*******************************************************************************
*! Name      : lfvSizeQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style size quantity
*!*******************************************************************************
*! Calls     : gfModalGen
*!*******************************************************************************
*! Parameters: lnSize   : Size 
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvSizeQty()
*!*******************************************************************************
*
FUNCTION lfvSizeQty
PARAMETERS lcSize,llStores
PRIVATE lnChoice, lcAlias, lcCurrTag, lnCurrRec 

IF m.Qty&lcSize < 0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Qty&lcSize = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF m.Qty&lcSize <> Qty&lcSize AND !llStores
  IF m.Picked AND !EMPTY(m.PikTKt) AND m.Qty&lcSize < m.Pik&lcSize
    *-- Message : 32015
    *-- Quantity cannot be below than what has been picked for this style/color
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32015B00000','ALERT')
    m.Qty&lcSize = &lcOrdLine..Qty&lcSize
    RETURN
  ENDIF
  IF !EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[9] .AND. ;
       gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2
    m.Qty&lcSize = &lcOrdLine..Qty&lcSize
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  IF m.Qty&lcSize < Cut&lcSize
    IF !gfUserPriv('MF','MFCUTKT')
      llIgnorAll = .T.  
      *-- Message: This order has an allocated quantity, you may need to edit the 
      *-- quantities on the cutting ticket, but the access to do this is denied.'
      =gfModalGen('INM32075B00000','DIALOG')
    ENDIF
    
    lnChoice = IIF(llIgnorAll,3,;
    gfModalGen('QRM32016B32002','ALERT',ALLTRIM(SCALE.Sz&lcSize)+'|'+ALLTRIM(STR(Cut&lcSize,5))))
    llIgnorAll = (lnChoice=3)
    IF lnChoice =4
      STORE Qty&lcSize TO m.Qty&lcSize
      RETURN
    ENDIF
    lnAlias = SELECT()
    =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
    SET ORDER TO TAG 'CUTORD' IN (lcAlocated)
    IF !SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),lcAlocated) .AND. ;
       SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),'CutPick')
      SELECT CutPick
      SCAN REST WHILE TranCd+Order+cOrdLine = ;
        IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6)
        SCATTER TO laCutPick
        INSERT INTO (lcAlocated) FROM ARRAY laCutPick
      ENDSCAN
    ENDIF
    =gfCloseFile('CUTPICK')
    DO CASE
      CASE lnChoice =1
        IF !lfEditAlo(lcSize)
          m.Qty&lcSize = &lcOrdLine..Qty&lcSize
        ENDIF    
      OTHERWISE
        SELECT (lcAlocated)
        =SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6))
        SCAN REST WHILE TranCd+Order+cOrdLine = ;
            IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6)
          SELECT (lcOrdHdr)
          =RLOCK()
          REPLACE TotCut WITH TotCut - &lcAlocated..Qty&lcSize
          UNLOCK
          SELECT (lcOrdLine)
          =RLOCK()
          REPLACE Cut&lcSize WITH Cut&lcSize - &lcAlocated..Qty&lcSize ,;
                  TotCut     WITH TotCut     - &lcAlocated..Qty&lcSize
          UNLOCK
          SELECT (lcAlocated)
          =RLOCK()
          REPLACE TotQty     WITH TotQty - Qty&lcSize ,;
                  Qty&lcSize WITH 0
          UNLOCK
        ENDSCAN
    ENDCASE
    SELECT (lnAlias)
  ENDIF
ENDIF

*C102118,1 Update Total Qtys with all Qtys. [Begin]
lcAlias = SELECT()
SELECT (lcOrdLine)
lcCurrTag = TAG()
lnCurrRec = RECNO()
SET ORDER TO TAG OrdLinSt

IF SEEK(lcOrdType + laData[1] + lcDiv + lcTeam + m.Style + STR(m.LineNo,6))
  m.Youth   = IIF(m.cStyGroup='JY',m.Qty1+m.Qty2+m.Qty3,0)
  m.Adult   = IIF(m.cStyGroup='JY',m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,0)
  m.YouthPr = lnYouthPr
  m.AdultPr = lnAdultPr
  m.TotQty  = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
  REPLACE Youth   WITH m.Youth   ,;
          Adult   WITH m.Adult   ,;
          YouthPr WITH lnYouthPr ,;
          AdultPr WITH lnAdultPr ,;
          Size1   WITH m.Qty1    ,;
          Size2   WITH m.Qty2    ,;
          Size3   WITH m.Qty3    ,;
          Size4   WITH m.Qty4    ,;
          Size5   WITH m.Qty5    ,;
          Size6   WITH m.Qty6    ,;
          Size7   WITH m.Qty7    ,;
          Size8   WITH m.Qty8    ,;
          TotSize WITH m.TotQty
ENDIF
SET ORDER TO TAG &lcCurrTag
IF lnCurrRec <> 0
  GOTO lnCurrRec
ENDIF  
SELECT (lcAlias)
=lfAdlYo()
=lfRefresh(lcWinCh321)
SHOW WINDOW (lcOrdBrow) REFRESH SAME
*C102118,1 Update Total Qtys with all Qtys. [End]
*-- End of lfvSizeQty.

*!*******************************************************************************
*! Name      : lfChkTotQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Check total pieces againest total quantity entered
*!*******************************************************************************
*! Calls     : gfModalGen(),lfvNPrice()
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfChkTotQty()
*!*******************************************************************************
*
FUNCTION lfChkTotQty
PARAMETERS llStores

IF m.Style==lcEmptySty OR !(SEEK(m.Style,'Style'))
  RETURN
ENDIF

*C102118,1 Define some variables. [Begin]
PRIVATE lcAlias, lcCurrTag, lcCurrVal
lcAlias = ALIAS()
*C102118,1 Define some variables. [End]

*C102118,1 SSE Commented out we don't need this checking. [Begin]
*-- Message : 32031
*-- Quantity out of balance! xxx pieces ok
*-- Button : 32000 
*-- Yes NO
*IF m.TotQty <> m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8 AND ;
*  gfModalGen('QRM32031B32000','ALERT',STR(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,5))=2
*  RETURN(.F.)
*ENDIF
*C102118,1 SSE Commented out we don't need this checking. [End]

*C102118,1 Change Order to Ordline in lcOrdLine. [Begin]
SELECT (lcOrdLine)
lcCurrTag = TAG()
lcCurrVal = EVALUATE(KEY())
SET ORDER TO TAG OrdLinSt

*C102118,5 If Edit mode and user changed style. [Begin] 
IF laScrMode[3] AND &lcOrdLine..Style <> m.Style AND !llAddLine
  IF &lcOrdLine..cStyGroup = "JY"
    *lnLines  = lnLines + 1
    *m.LineNo = lnLines
    *m.Note_Mem = &lcOrdLine..Note_Mem
    *SCATTER TO laOrdLArry
    *DELETE
    *APPEND BLANK 
    *GATHER FROM laOrdLArry
    IF m.cStyGroup <> "JY"       
      *STORE 0 TO m.Price , m.Gros_Price
      PRIVATE lnCurPrice
      lnCurPrice = 0
      lnCurPrice = IIF(m.cStyGroup<>"SO" OR m.cStyGroup<>"SH",m.Price,0) 
      STORE lnCurPrice TO m.Price , m.Gros_Price
      REPLACE Price WITH lnCurPrice , Gros_Price WITH lnCurPrice
    ENDIF
  ENDIF  
  REPLACE Style WITH m.Style , Scale WITH m.Scale , Desc1 WITH m.Desc1 ,  ;
          PrePak WITH m.PrePak , Comm1 WITH m.Comm1 , Comm2 WITH m.Comm2 ,;
          Season WITH m.Season , Gl_Sales WITH m.Gl_Sales , ;
          Youth WITH IIF(m.cStyGroup='JY',m.Qty1+m.Qty2+m.Qty3,0) ,;
          Adult WITH IIF(m.cStyGroup='JY',m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,0) ,;
          YouthPr WITH IIF(m.cStyGroup='JY',lnYouthPr,0) , cStyGroup WITH m.cStyGroup ,;
          AdultPr WITH IIF(m.cStyGroup='JY',lnAdultPr,0) , Note_Mem WITH m.Note_Mem  ,;
          LineNo WITH m.LineNo , lRemoveLin WITH IIF(m.cStyGroup='JY',.T.,.F.)
  =gfAdd_Info(lcOrdLine)
          
ENDIF
*C102118,5 If Edit mode and user changed style. [End] 

IF !(m.Style==lcEmptySty) AND llAddLine 
  =SEEK(lcOrdType + laData[1] + lcDiv + lcteam + m.Style + STR(m.LineNo,6))
ENDIF  
SET ORDER TO TAG &lcCurrTag
*C102118,1 Change Order to Ordline in lcOrdLine. [End]

IF Qty1 <> m.Qty1 OR Qty2 <> m.Qty2 OR Qty3 <> m.Qty3 OR Qty4 <> m.Qty4 OR ;
   Qty5 <> m.Qty5 OR Qty6 <> m.Qty6 OR Qty7 <> m.Qty7 OR Qty8 <> m.Qty8
  
  STORE .T. TO llCUpdate
  m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
  IF !m.lContract AND lcPriceLvl='Q'
    m.Gros_Price = lfGetprice(m.Style,lcPriceLvl,m.TotQty)
    m.Price = m.Gros_Price*(100-m.Disc_Pcnt)/100
  ENDIF

  IF llStores
    lnTotPcs = lnTotPcs - TotQty+m.TotQty
    lnTotAmt = lnTotAmt - TotQty*Price + m.TotQty*m.Price
  ELSE    
    IF laScrMode[4] AND !EMPTY(cFromOrder) .AND. SEEK(STR(BulkLineNo,6),lcBulkOrd)
      SELECT (lcBulkOrd)
      =RLOCK()
      REPLACE USED1   WITH USED1 - &lcOrdLine..Qty1 + m.Qty1 ,;
              USED2   WITH USED2 - &lcOrdLine..Qty2 + m.Qty2 ,;
              USED3   WITH USED3 - &lcOrdLine..Qty3 + m.Qty3 ,;
              USED4   WITH USED4 - &lcOrdLine..Qty4 + m.Qty4 ,;
              USED5   WITH USED5 - &lcOrdLine..Qty5 + m.Qty5 ,;
              USED6   WITH USED6 - &lcOrdLine..Qty6 + m.Qty6 ,;
              USED7   WITH USED7 - &lcOrdLine..Qty7 + m.Qty7 ,;
              USED8   WITH USED8 - &lcOrdLine..Qty8 + m.Qty8 ,;
              TOTUSED WITH Used1+Used2+Used3+Used4+Used5+Used6+Used7+Used8
      UNLOCK
      IF TYPE('LLFRMGBRW')='L' .AND. llFrmGBrw
        SELECT(lcTmpSize)
      ELSE
        SELECT (lcOrdLine)
      ENDIF
    ENDIF
    
    *C102118,1 Change Order to Ordline in lcOrdLine. [Begin]
    SELECT (lcOrdLine)
    lcCurrTag = TAG()
    SET ORDER TO TAG OrdLine IN (lcOrdLine)
    *C102118,1 Change Order to Ordline in lcOrdLine. [End]
    
    =SEEK(cOrdType+Order+STR(LineNo,6),'OrdLine')
    laData[35] = laData[35] - TotBook
    
    *C102118,5 SSE Commented out [Begin]
    *laData[36] = laData[36] - TotBook*Price    
    *C102118,5 SSE Commented out [End]
    
    =RLOCK()

    *C102118,5 Updating the Book Qtys [Begin]
    *REPLACE Book1   WITH MAX(OrdLine.Book1-OrdLine.Qty1+m.Qty1,IIF(llUpdBook,0,OrdLine.Book1))  ,;
    *        Book2   WITH MAX(OrdLine.Book2-OrdLine.Qty2+m.Qty2,IIF(llUpdBook,0,OrdLine.Book2))  ,;
    *        Book3   WITH MAX(OrdLine.Book3-OrdLine.Qty3+m.Qty3,IIF(llUpdBook,0,OrdLine.Book3))  ,;
    *        Book4   WITH MAX(OrdLine.Book4-OrdLine.Qty4+m.Qty4,IIF(llUpdBook,0,OrdLine.Book4))  ,;
    *        Book5   WITH MAX(OrdLine.Book5-OrdLine.Qty5+m.Qty5,IIF(llUpdBook,0,OrdLine.Book5))  ,;
    *        Book6   WITH MAX(OrdLine.Book6-OrdLine.Qty6+m.Qty6,IIF(llUpdBook,0,OrdLine.Book6))  ,;
    *        Book7   WITH MAX(OrdLine.Book7-OrdLine.Qty7+m.Qty7,IIF(llUpdBook,0,OrdLine.Book7))  ,;
    *        Book8   WITH MAX(OrdLine.Book8-OrdLine.Qty8+m.Qty8,IIF(llUpdBook,0,OrdLine.Book8))  ,;
    *        TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8 ,;
    *        FLAG WITH IIF(FLAG='N','N','M')
    
    IF laScrMode[4]
      REPLACE Book1   WITH MAX(OrdLine.Book1-OrdLine.Qty1+m.Qty1,IIF(llUpdBook,0,OrdLine.Book1))  ,;
              Book2   WITH MAX(OrdLine.Book2-OrdLine.Qty2+m.Qty2,IIF(llUpdBook,0,OrdLine.Book2))  ,;
              Book3   WITH MAX(OrdLine.Book3-OrdLine.Qty3+m.Qty3,IIF(llUpdBook,0,OrdLine.Book3))  ,;
              Book4   WITH MAX(OrdLine.Book4-OrdLine.Qty4+m.Qty4,IIF(llUpdBook,0,OrdLine.Book4))  ,;
              Book5   WITH MAX(OrdLine.Book5-OrdLine.Qty5+m.Qty5,IIF(llUpdBook,0,OrdLine.Book5))  ,;
              Book6   WITH MAX(OrdLine.Book6-OrdLine.Qty6+m.Qty6,IIF(llUpdBook,0,OrdLine.Book6))  ,;
              Book7   WITH MAX(OrdLine.Book7-OrdLine.Qty7+m.Qty7,IIF(llUpdBook,0,OrdLine.Book7))  ,;
              Book8   WITH MAX(OrdLine.Book8-OrdLine.Qty8+m.Qty8,IIF(llUpdBook,0,OrdLine.Book8))  ,;
              TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8 ,;
              FLAG WITH IIF(FLAG='N','N','M')
    ELSE    
      IF Style.cStyGroup = "JY"
        IF OrdLine.Book1 + OrdLine.Book2 + OrdLine.Book3 > 0
          REPLACE Book1   WITH MAX(OrdLine.Book1-OrdLine.Qty1+m.Qty1,IIF(llUpdBook,0,OrdLine.Book1))  ,;
                  Book2   WITH MAX(OrdLine.Book2-OrdLine.Qty2+m.Qty2,IIF(llUpdBook,0,OrdLine.Book2))  ,;
                  Book3   WITH MAX(OrdLine.Book3-OrdLine.Qty3+m.Qty3,IIF(llUpdBook,0,OrdLine.Book3))  ,;
                  TotBook WITH Book1+Book2+Book3 ,;
                  FLAG WITH IIF(FLAG='N','N','M')      
        ENDIF
        IF !EOF('OrdLine')
          SKIP 1 IN OrdLine
          IF !EOF('OrdLine') AND Style.cStyGroup = "JY"
            REPLACE Book4   WITH MAX(OrdLine.Book4-OrdLine.Qty4+m.Qty4,IIF(llUpdBook,0,OrdLine.Book4))  ,;                  
                    Book5   WITH MAX(OrdLine.Book5-OrdLine.Qty5+m.Qty5,IIF(llUpdBook,0,OrdLine.Book5))  ,;
                    Book6   WITH MAX(OrdLine.Book6-OrdLine.Qty6+m.Qty6,IIF(llUpdBook,0,OrdLine.Book6))  ,;
                    Book7   WITH MAX(OrdLine.Book7-OrdLine.Qty7+m.Qty7,IIF(llUpdBook,0,OrdLine.Book7))  ,;
                    Book8   WITH MAX(OrdLine.Book8-OrdLine.Qty8+m.Qty8,IIF(llUpdBook,0,OrdLine.Book8))  ,;
                    TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8 ,;
                    FLAG WITH IIF(FLAG='N','N','M')
     
          ENDIF
          SKIP -1 IN OrdLine
        ENDIF  
      ELSE
        REPLACE Book1   WITH MAX(OrdLine.Book1-OrdLine.Qty1+m.Qty1,IIF(llUpdBook,0,OrdLine.Book1))  ,;
                Book2   WITH MAX(OrdLine.Book2-OrdLine.Qty2+m.Qty2,IIF(llUpdBook,0,OrdLine.Book2))  ,;
                Book3   WITH MAX(OrdLine.Book3-OrdLine.Qty3+m.Qty3,IIF(llUpdBook,0,OrdLine.Book3))  ,;
                Book4   WITH MAX(OrdLine.Book4-OrdLine.Qty4+m.Qty4,IIF(llUpdBook,0,OrdLine.Book4))  ,;
                Book5   WITH MAX(OrdLine.Book5-OrdLine.Qty5+m.Qty5,IIF(llUpdBook,0,OrdLine.Book5))  ,;
                Book6   WITH MAX(OrdLine.Book6-OrdLine.Qty6+m.Qty6,IIF(llUpdBook,0,OrdLine.Book6))  ,;
                Book7   WITH MAX(OrdLine.Book7-OrdLine.Qty7+m.Qty7,IIF(llUpdBook,0,OrdLine.Book7))  ,;
                Book8   WITH MAX(OrdLine.Book8-OrdLine.Qty8+m.Qty8,IIF(llUpdBook,0,OrdLine.Book8))  ,;
                TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8 ,;
                FLAG WITH IIF(FLAG='N','N','M')
      ENDIF
    ENDIF
    *C102118,5 Updating the Book Qtys [End]
    
    UNLOCK
    
    IF Flag <> 'N' AND !llUpdBook

      *C102118,5 check for Jersey to compare Ordline line file 2 lines with Qtys [Begin]
      PRIVATE llQtysChng
      llQtysChng = .F.
      IF Style.cStyGroup = "JY"
        IF !EOF('OrdLine')
          SKIP 1 IN OrdLine
          IF Style.cStyGroup = "JY" AND (OrdLine.Qty4 > m.Qty4 OR OrdLine.Qty5 > m.Qty5 OR ;
            OrdLine.Qty6 > m.Qty6 OR OrdLine.Qty7 > m.Qty7 OR OrdLine.Qty8 > m.Qty8)
            llQtysChng = .T.          
          ENDIF
          SKIP -1 IN OrdLine
        ENDIF
      ENDIF
      *C102118,5 check for Jersey to compare Ordline line file 2 lines with Qtys [End]
    
      *C102118,5 Add the Jersey checking to the Qty's [Begin]
      *IF OrdLine.Qty1 > m.Qty1 OR OrdLine.Qty2 > m.Qty2 OR OrdLine.Qty3 > m.Qty3 OR ;
      *   OrdLine.Qty4 > m.Qty4 OR OrdLine.Qty5 > m.Qty5 OR OrdLine.Qty6 > m.Qty6 OR  ;
      *   OrdLine.Qty7 > m.Qty7 OR OrdLine.Qty8 > m.Qty8
      IF OrdLine.Qty1 > m.Qty1 OR OrdLine.Qty2 > m.Qty2 OR OrdLine.Qty3 > m.Qty3 OR ;
         OrdLine.Qty4 > m.Qty4 OR OrdLine.Qty5 > m.Qty5 OR OrdLine.Qty6 > m.Qty6 OR  ;
         OrdLine.Qty7 > m.Qty7 OR OrdLine.Qty8 > m.Qty8 OR llQtysChng
      *C102118,5 Add the Jersey checking to the Qty's [End]

        =gfwCodePop(@laCodes,'CCANCRESON','D')        
        INSERT INTO (lcOrdCanLn) ;
        (cOrdType,Order,LineNo,Cancelled,cCancReson) VALUES ;
        (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2])
        =gfAdd_Info(lcOrdCanLn)
      ENDIF
      IF SEEK(lcOrdType+laData[1]+STR(m.LineNo,6),lcOrdCanLn)
        SELECT (lcOrdCanLn)
        REPLACE Qty1 WITH MAX(OrdLine.Qty1- m.Qty1,0) ,;
                Qty2 WITH MAX(OrdLine.Qty2- m.Qty2,0) ,;
                Qty3 WITH MAX(OrdLine.Qty3- m.Qty3,0) ,;
                Qty4 WITH MAX(OrdLine.Qty4- m.Qty4,0) ,;
                Qty5 WITH MAX(OrdLine.Qty5- m.Qty5,0) ,;
                Qty6 WITH MAX(OrdLine.Qty6- m.Qty6,0) ,;
                Qty7 WITH MAX(OrdLine.Qty7- m.Qty7,0) ,;
                Qty8 WITH MAX(OrdLine.Qty8- m.Qty8,0) ,;
                TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        REPLACE ACCOUNT WITH ORDLINE.ACCOUNT,;
                STYLE   WITH ORDLINE.STYLE,;
                STORE   WITH ORDLINE.STORE,;
                DYELOT  WITH ORDLINE.DYELOT

        *C102118,5 Display Cancellation screen in case of Jersey Style [Begin]
        *IF TotQty > 0
        *  STORE lcOrdCanLn TO laCodes[6,7],laCodes[6,8]
        *  STORE 'lcOrdType+laData[1]+STR(m.LineNo,6)' TO laCodes[6,9]
        *  =gfwCodePop(@laCodes,'CCANCRESON','T')
        *  STORE Cancelled TO m.Cancelled
        *  DO (gcScrDir+"SOORDCLN.SPX")
        *  STORE '' TO laCodes[6,7],laCodes[6,8],laCodes[6,9]
        *ELSE
        *  DELETE
        *ENDIF
        *-- this code can be written on one IF..ELSE..ENDIF but for readability
          
        IF TotQty > 0 OR llQtysChng
          STORE lcOrdCanLn TO laCodes[6,7],laCodes[6,8]
          STORE 'lcOrdType+laData[1]+STR(m.LineNo,6)' TO laCodes[6,9]
          =gfwCodePop(@laCodes,'CCANCRESON','T')
          STORE Cancelled TO m.Cancelled
          DO (gcScrDir+"SOORDCLN.SPX")
          STORE '' TO laCodes[6,7],laCodes[6,8],laCodes[6,9]
        ELSE
          DELETE
        ENDIF
        
        IF TYPE('LLFRMGBRW')='L' .AND. llFrmGBrw
          SELECT(lcTmpSize)
        ELSE
          SELECT (lcOrdLine)
        ENDIF
      ENDIF
      
      *C102118,5 Update the cancelled Qtys into 2 lines if it's Jersey [Begin]
      PRIVATE lcAlias , lcCurrTag , lcCurrVal , lnOrdLinNo , lcJersySty
      lcAlias = ALIAS()
      SELECT OrdLine      
      lcCurrTag  = TAG()
      lcCurrVal  = EVALUATE(KEY())
      lnOrdLinNo = LineNo + 1
      SET ORDER TO TAG OrdLinSt 
      lcJersySty = cOrdType + Order + Store + Style

      IF Style.cStyGroup = "JY" AND SEEK(lcJersySty+STR(lnOrdLinNo,6))
        INSERT INTO (lcOrdCanLn) ;
        (cOrdType,Order,LineNo,Cancelled,cCancReson) VALUES ;
        (lcOrdType,laData[1],lnOrdLinNo,gdSysDate,laCanReason[lnCanReason,2])        
        =gfAdd_Info(lcOrdCanLn)
      ENDIF
      
      IF SEEK(lcOrdType+laData[1]+STR(lnOrdLinNo,6),lcOrdCanLn)
        SELECT (lcOrdCanLn)
        REPLACE Qty1 WITH MAX(OrdLine.Qty1- m.Qty1,0) ,;
                Qty2 WITH MAX(OrdLine.Qty2- m.Qty2,0) ,;
                Qty3 WITH MAX(OrdLine.Qty3- m.Qty3,0) ,;
                Qty4 WITH MAX(OrdLine.Qty4- m.Qty4,0) ,;
                Qty5 WITH MAX(OrdLine.Qty5- m.Qty5,0) ,;
                Qty6 WITH MAX(OrdLine.Qty6- m.Qty6,0) ,;
                Qty7 WITH MAX(OrdLine.Qty7- m.Qty7,0) ,;
                Qty8 WITH MAX(OrdLine.Qty8- m.Qty8,0) ,;
                TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        REPLACE ACCOUNT WITH ORDLINE.ACCOUNT,;
                STYLE   WITH ORDLINE.STYLE,;
                STORE   WITH ORDLINE.STORE,;
                DYELOT  WITH ORDLINE.DYELOT
        IF TotQty = 0
          DELETE
        ENDIF
        IF TYPE('LLFRMGBRW')='L' .AND. llFrmGBrw
          SELECT(lcTmpSize)
        ELSE
          SELECT (lcOrdLine)
        ENDIF
      ENDIF

      SELECT OrdLine
      SET ORDER TO TAG (lcCurrTag)
      =SEEK(lcCurrVal)
      SELECT (lcAlias)
      *C102118,5 Update the cancelled Qtys into 2 lines if it's Jersey [End]

    ENDIF
    laData[35] = laData[35] + TotBook

    *C102118,1 Updating the Booked Amount [Begin]
    *laData[36] = laData[36] + TotBook*m.Price
    PRIVATE lnCurrVal
    SELECT (lcOrdline)
    lnCurrVal = EVALUATE(KEY())    && Save current record
    LOCATE

    STORE 0 TO laData[36] , laData[42]
    SCAN
      IF LineNo = m.LineNo
        IF m.cStyGroup = "JY"
          *-- Booked
          laData[36] = laData[36] + (((m.Book1+m.Book2+m.Book3)*YouthPr) + ((m.Book4+m.Book5+m.Book6+m.Book7+m.Book8)*AdultPr))
      
          *-- Opened
          laData[42] = laData[42] + ((m.Qty1+m.Qty2+m.Qty3)*YouthPr) + ((m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8)*AdultPr)
        
        ELSE
          *-- Booked
          laData[36] = laData[36] + m.TotBook*m.Price
      
          *-- Opened
          laData[42] = laData[42] + m.TotQty*m.Price 
        ENDIF
        LOOP
      ENDIF
      
      IF cStyGroup = 'JY'
        *-- Booked
        laData[36] = laData[36] + (((Book1+Book2+Book3)*YouthPr) + ((Book4+Book5+Book6+Book7+Book8)*AdultPr))
      
        *-- Opened
        laData[42] = laData[42] + ((Qty1+Qty2+Qty3)*YouthPr) + ((Qty4+Qty5+Qty6+Qty7+Qty8)*AdultPr)    
      ELSE
        *-- Booked
        laData[36] = laData[36] + TotBook*Price
      
        *-- Opened
        laData[42] = laData[42] + TotQty*Price 
      ENDIF
    ENDSCAN  

    SELECT (lcOrdLine)
    =SEEK(lnCurrVal)
    *C102118,5 Update Price if user in case user changed Style from Jersey to anything [End]
    *C102118,1 Updating the Booked Amount [End]
    
    IF FLAG <> 'N' AND !llUpdBook
      lnTotCanQty = 0
      For I = 1 To Scale.Cnt
        lcScalSize = STR(I,1)
        lnTotCanQty = lnTotCanQty + MAX(Qty&lcScalSize - m.Qty&lcScalSize,0)
      ENDFOR
      
      *C102118,5 Calculate the Cancelled Qtys in case of Jersey [Begin]
      IF cStyGroup = "JY"
        PRIVATE lnfrst3Qty
        lnfrst3Qty = 0
        For I = 1 To IIF(Scale.Cnt<=3,Scale.Cnt,3)
          lcScalSize = STR(I,1)
          lnfrst3Qty = lnfrst3Qty + MAX(Qty&lcScalSize - m.Qty&lcScalSize,0)
        ENDFOR      
      ENDIF
      *C102118,5 Calculate the Cancelled Qtys in case of Jersey [End]

      laData[39] = MAX(laData[39] + lnTotCanQty,0)
      
      *C102218,5 Update the Cancelled Qtys in case of Jersey [Begin]
      *laData[40] = MAX(laData[40] + lnTotCanQty*Price,0)      
      IF cStyGroup = "JY"         
        laData[40] = MAX(laData[40] + (lnfrst3Qty * YouthPr) + IIF(lnTotCanQty-lnfrst3Qty>0,((lnTotCanQty-lnfrst3Qty)*AdultPr),0),0)
      ELSE
        laData[40] = MAX(laData[40] + lnTotCanQty*Price,0)
      ENDIF  
      *C102218,5 Update the Cancelled Qtys in case of Jersey [End]
      
    ENDIF  

    laData[41] = laData[41] - TotQty+m.TOtQty
    laData[42] = laData[42] - TotQty*Price+ m.TOtQty*m.Price
        
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE BOOK      WITH laData[35] ,;
            BOOKAMT   WITH laData[36] ,;
            CANCEL    WITH laData[39] ,;
            CANCELAMT WITH laData[40] ,;
            OPEN      WITH laData[41] ,;
            OPENAMT   WITH laData[42]
    UNLOCK
    IF TYPE('LLFRMGBRW')='L' .AND. llFrmGBrw
      SELECT(lcTmpSize)
    ELSE
      SELECT (lcOrdLine)
    ENDIF
  ENDIF
  =IIF(llStores,lfRefresh('SOMULTI4'),lfRefresh(lcWinCh321))
  =RLOCK()

  GATHER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
                       Gros_Price,Price
  UNLOCK
  SCATTER MEMVAR FIELDS Book1,Book2,Book3,Book4,Book5,Book6,Book7,Book8,TotBook

  *C102118,1 Change Order to Ordline in lcOrdLine. [Begin]  
  SET ORDER TO TAG &lcCurrTag IN (lcOrdLine)
  =SEEK(lcCurrVal)
  *C102118,1 Change Order to Ordline in lcOrdLine. [End]
  
ENDIF

SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty

*C102118,4 Function added to calculate BookAmt,OpenAmt [Begin]
=lfCalcAmnt()
*C102118,4 Function added to calculate BookAmt,OpenAmt [End]

*C102118,1 Restore old file. [Begin]
SELECT (lcAlias)
*C102118,1 Restore old file. [End]
*-- End of lfChkTotQty.

*!*******************************************************************************
*! Name      : lfvOrdCanLn
*! Developer : Wael Aly Mohamed
*! Date      : 08/16/1998
*! Purpose   : Update Order line canceled reason and date
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvOrdCanLn()
*!*******************************************************************************
*
FUNCTION lfvOrdCanLn
CLEAR READ
SELECT (lcOrdCanLn)
=SEEK(lcOrdType+laData[1]+STR(m.LineNo,6))
REPLACE cCancReson WITH laCanReason[lnCanReason,2],;
        Cancelled  WITH m.Cancelled
SELECT (lcOrdLine)
*-- End of lfvOrdCanLn.

*!*******************************************************************************
*! Name      : lfvStyDesc
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style description
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvStyDesc()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfvStyDesc
PARAMETERS llStores

IF Desc1 <> m.Desc1
  llCUpdate = .T.
  =RLOCK()  
  REPLACE Desc1 WITH m.Desc1 ,;
          Flag  WITH IIF(Flag='N' .OR. llStores,Flag,'M')
  UNLOCK
ENDIF
*-- End of lfvStyDesc.

*!*******************************************************************************
*! Name      : lfvComm
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style salesrep commissions
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: lnSalesRep   : Salesrep number
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvComm()
*!*******************************************************************************
*C102118,1 This function in Detail Folder is not used anymore.
*
FUNCTION lfvComm
PARAMETERS lcSalesRep,llStores,llFrmGBrw
IF (!llFrmGBrw .AND. Comm&lcSalesRep <> m.Comm&lcSalesRep) .OR. (llFrmGBrw .AND. m.Comm&lcSalesRep # lcOldVal)
  llCUpdate = .T.
  =RLOCK()  
  REPLACE Comm&lcSalesRep WITH m.Comm&lcSalesRep ,;
          Flag  WITH IIF(Flag='N' .OR. llStores,Flag,'M')
  IF llFrmGBrw
    REPLACE cStatus WITH IIF(cStatus='N','N','M')
  ENDIF
  
  UNLOCK
ENDIF
*-- End of lfvComm.

*!*******************************************************************************
*! Name      : lfwGPrice
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : When function for Style Gross price
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfwGPrice()
*!*******************************************************************************
*C102118,1 This function in Detail Folder is not used anymore.
*
FUNCTION lfwGPrice
PARAMETERS llStores
PRIVATE lcPriceCatg,lnPrice,lcOrdAlias

IF m.lContract 
  RETURN(.F.)
ENDIF

IF llBomVarnt
  lcOrdAlias = ALIAS()
  SELECT (lcT_BomVar)
  SUM nPriceAdd FOR cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(m.LineNo,6) TO lnAddPrice
  SELECT (lcOrdAlias)
  IF lnAddPrice > 0
    RETURN
  ENDIF
ENDIF

*-- Message : 32032
*-- Style price xxx does not match order price.  Default to style price?
*-- Button : 32000
*-- Yes No
lcPriceCatg = IIF(lcPriceLvl='Q',;
              IIF(Style.nAtQtyC>0 AND m.TotQty >= Style.nAtQtyC,'C',;
              IIF(Style.nAtQtyB>0 AND m.TotQty >= Style.nAtQtyB,'B','A')),;
              IIF(INLIST(lcPriceLvl,'A','B','C'),lcPriceLvl,'A'))
lnPrice = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.Price&lcPriceCatg,;
                       gfStyPrice(m.Style,lcPriceCatg,laData[33]))

IF lnPrice>0 .AND. m.Gros_Price <> lnPrice .AND. ;
  gfModalGen('QRM32032B32000','ALERT',ALLTRIM(STR(lnPrice,12,2))+;
  '|'+IIF(lcOrdType='C','contract','order'))=1

  m.Gros_Price = lnPrice
  m.Price = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
  =lfvNPrice(llStores)
ENDIF
*-- End of lfwGPrice.

*!*******************************************************************************
*! Name      : lfvGPrice
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style Gross price
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvGPrice()
*!*******************************************************************************
*C102118,1 This function in Detail Folder is not used anymore.
*
FUNCTION lfvGPrice
PARAMETERS llStores,llFrmGBrw
PRIVATE lcOrdAlias

IF m.Gros_Price < 0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Gros_Price = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF (!llFrmGBrw .AND. m.Gros_Price <> Gros_Price) .OR. (llFrmGBrw .AND. m.Gros_Price <> lcOldVal )
  IF llBomVarnt .AND. (!llFrmGBrw .AND. m.Gros_Price < Gros_Price) .OR. (llFrmGBrw .AND. m.Gros_Price < lcOldVal )
    lcOrdAlias = ALIAS()
    SELECT (lcT_BomVar)
    SUM nPriceAdd FOR cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(m.LineNo,6) TO lnAddPrice
    SELECT (lcOrdAlias)
    IF m.Gros_Price < lnAddPrice
      *** There is style positions for this line, you cannot ***
      *** change the price to be less than the price adder.  ***
      *** <  Ok  > ***
      =gfModalGen("INM32073B00000" , "DIALOG")
      m.Gros_Price = IIF(llFrmGBrw,lcOldVal,Gros_Price)
      
      *C102118,1 SSE Commented out because it is Removed from screen [Begin]
      *SHOW GET m.Gros_Price
      *C102118,1 SSE Commented out because it is Removed from screen [End]
      
      RETURN
    ENDIF
  ENDIF
  m.Price = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
  =lfvNPrice(llStores,.F.,llFrmGBrw)
ENDIF
*-- End of lfvGPrice.

*!*******************************************************************************
*! Name      : lfvPrcDisc
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style price discount
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvPrcDisc()
*!*******************************************************************************
*C102118,1 This function in Detail Folder is not used anymore.
*
FUNCTION lfvPrcDisc
PARAMETERS llStores,llFrmGBrw

IF (!llFrmGBrw .AND. m.Disc_Pcnt <> Disc_Pcnt) .OR. (llFrmGBrw .AND. m.Disc_Pcnt# lcOldVal )
  m.Price = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
  =lfvNPrice(llStores,.T.,llFrmGBrw)
ENDIF
*-- End of lfvPrcDisc.

*!*******************************************************************************
*! Name      : lfvNPrice
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style Net price
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvNPrice()
*!*******************************************************************************
*
FUNCTION lfvNPrice
PARAMETERS llStores,llPercent,llFrmGBrw

IF m.Price < 0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Price = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF llFrmGBrw .AND. UPPER(VARREAD()) = 'PRICE'
  REPLACE Price WITH lcoldVal
ENDIF

*C102118,5 SSE Commented out , we don't have Gross Price to comapre it with Price [Begin]
*m.Price = IIF(m.Price>m.Gros_Price,Price,m.Price)
*C102118,5 SSE Commented out , we don't have Gross Price to comapre it with Price [End]

IF m.Price <> Price
  STORE .T. TO llCUpdate
  SELECT IIF(llFrmGBrw,lcTmpSize,IIF(llStores,lcTempLine,lcOrdLine))
  IF llStores
    lnTotAmt = lnTotAmt + TotQty*(m.Price - Price)
  ELSE
    *C102118,5 Update Price if user in case user changed Style from Jersey to anything [Begin]
    *laData[36] = laData[36] + TotBook*(m.Price - Price)
    *laData[42] = laData[42] + TotQty*(m.Price - Price)
    
    PRIVATE lcAlias, lnCurrVal
    lcAlias = ALIAS()              && Save current alias.
    SELECT (lcOrdline)
    lnCurrVal = EVALUATE(KEY())    && Save current record
    LOCATE

    STORE 0 TO laData[36] , laData[42]
    SCAN
      IF LineNo = m.LineNo
        IF m.cStyGroup = "JY"
          *-- Booked
          laData[36] = laData[36] + (((m.Book1+m.Book2+m.Book3)*m.YouthPr) + ((m.Book4+m.Book5+m.Book6+m.Book7+m.Book8)*m.AdultPr))
      
          *-- Opened
          laData[42] = laData[42] + ((m.Qty1+m.Qty2+m.Qty3)*m.YouthPr) + ((m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8)*m.AdultPr)    
        
        ELSE
          *-- Booked
          laData[36] = laData[36] + m.TotBook*m.Price
      
          *-- Opened
          laData[42] = laData[42] + m.TotQty*m.Price 
        ENDIF
        LOOP
      ENDIF
      
      IF cStyGroup = 'JY'
        *-- Booked
        laData[36] = laData[36] + (((Book1+Book2+Book3)*YouthPr) + ((Book4+Book5+Book6+Book7+Book8)*AdultPr))
      
        *-- Opened
        laData[42] = laData[42] + ((Qty1+Qty2+Qty3)*YouthPr) + ((Qty4+Qty5+Qty6+Qty7+Qty8)*AdultPr)    
      ELSE
        *-- Booked
        laData[36] = laData[36] + TotBook*Price
      
        *-- Opened
        laData[42] = laData[42] + TotQty*Price 
      ENDIF
    ENDSCAN  

    SELECT (lcOrdLine)
    =SEEK(lnCurrVal)
    SELECT (lcAlias)               && Restore old alias.
    *C102118,5 Update Price if user in case user changed Style from Jersey to anything [End]
      
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE BOOKAMT WITH laData[36] ,;
            OPENAMT WITH laData[42]
    UNLOCK
    SELECT (lcOrdLine)
  ENDIF
  m.Disc_Pcnt = IIF(llPercent,m.Disc_Pcnt,IIF(m.Gros_Price=0,0,100-m.Price*100/m.Gros_Price))
  IF llFrmGBrw
    SELECT(lcTmpSize)
    REPLACE cStatus WITH IIF(cStatus = 'N',cStatus,'M')
  ENDIF
  =RLOCK()
  REPLACE Gros_Price WITH m.Gros_Price ,;
          Disc_Pcnt  WITH m.Disc_Pcnt  ,;
          Price      WITH m.Price      ,;
          Flag       WITH IIF(Flag='N' .OR. llStores,Flag,'M')
  UNLOCK

  *C102118,1 SSE Commented out because they are Removed from screen [Begin]
  *SHOW GET m.Gros_Price
  *SHOW GET m.Disc_Pcnt
  *C102118,1 SSE Commented out because they are Removed from screen [End]
  
  SHOW GET m.Price
  IF llStores
    =lfrefresh('SOMULTI2')
    SHOW WINDOW (lcBrTtlM) REFRESH SAME
  ELSE
    SHOW WINDOW (lcOrdBrow) REFRESH SAME
  ENDIF
ENDIF
*-- End of lfvNPrice.

*!*******************************************************************************
*! Name      : lfvStyPack
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate order lines Style packs
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvStyPack()
*!*******************************************************************************
*C102118,1 This function in Detail Folder is not used anymore.
*
FUNCTION lfvStyPack
PARAMETERS llStores,llFrmGBrw
IF llFrmGBrw
  m.Pack_id = EVAL(VARREAD())
ENDIF
PRIVATE lcStyClr

IF llBrowse .OR. !EMPTY(m.Pack_Id)
  =gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPck_Hdr','SH')
  =gfOpenFile(gcDataDir+'SPck_Lin',gcDataDir+'SPCKLNST','SH')

  IF llBrowse OR ;
    (!SEEK('P'+m.Style+laData[2]+m.Pack_Id,'SPck_Lin') .AND. ;
     !SEEK('P'+m.Style+'*****'+m.Pack_Id,'SPck_Lin'))

    IF !SEEK('P'+m.Style+laData[2],'SPck_Lin') .AND. ;
       !SEEK('P'+m.Style+'*****','SPck_Lin')
      *-- Message : 32042
      *-- No packs found for style/color xxxx
      *-- Button : 00000 
      *-- Ok
      =gfModalGen('TRM32042B00000','DIALOG',ALLTRIM(m.Style))
      STORE SPACE(16) TO m.Pack_Id
    ELSE
      lcStyClr = m.Style
      SELECT SPck_Lin
      SET RELATION TO Type+Account+Pack_Id INTO SPck_Hdr
      lcBrFields = [Account :H="Account",Pack_Id :H="Pack Id",SPck_Hdr.Desc :H='Description',TotQty :H='Quantity']
      m.Pack_Id = IIF(ARIABROW("'P'+lcStyClr"+" FOR ACCOUNT=laData[2] OR ACCOUNT='*****'","Style packs",;
                  gnBrFSRow1, gnBrFSCol1,gnBrFSRow2, gnBrFSCol2,'','','Pack_id','laBrowArr'),SPck_Lin.Pack_Id,&lcOrdLine..Pack_Id)
    ENDIF  
  ENDIF
  IF !EMPTY(m.Pack_Id) AND m.TotQty = 0
    m.Qty1 = Spck_lin.Qty1
    m.Qty2 = Spck_lin.Qty2
    m.Qty3 = Spck_lin.Qty3
    m.Qty4 = Spck_lin.Qty4
    m.Qty5 = Spck_lin.Qty5
    m.Qty6 = Spck_lin.Qty6
    m.Qty7 = Spck_lin.Qty7
    m.Qty8 = Spck_lin.Qty8
    m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
    IF llFrmGBrw
      =lfChkTQty()
    ELSE
      =lfChkTotQty(llStores)
    ENDIF
    IF llStores
      SHOW WINDOW (lcBrTtlM) REFRESH SAME
      SHOW GETS WINDOW 'SOMULTI3' ONLY
      SHOW GETS WINDOW 'SOMULTI4' ONLY
      =lfRefresh('SOMULTI2')
      =lfRefresh('SOMULTI4')
    ELSE
      SHOW WINDOW (lcOrdBrow) REFRESH SAME
      SHOW GETS WINDOW (lcWinCh32)  ONLY
      SHOW GETS WINDOW (lcWinCh321) ONLY
      =lfRefresh(lcWinCh321)
    ENDIF
  ENDIF
  
  =gfCloseFile('SPck_Hdr')
  =gfCloseFile('SPck_Lin')
ENDIF
llBrowse = .F.
SELECT IIF(llFrmGBrw,lcTmpSize,IIF(llStores,lcTempline,lcOrdLine))

IF Pack_Id <> m.Pack_Id
  llCUpdate = .T.
  =RLOCK()  
  REPLACE Pack_Id WITH m.Pack_Id ,;
          Flag  WITH IIF(Flag='N' .OR. llStores,Flag,'M')
  IF llFrmGBrw
    REPLACE cStatus WITH IIF(cStatus = 'N',cStatus,'M')
  ENDIF
  
  UNLOCK
ENDIF
*-- End of lfvStyPack.

*!*******************************************************************************
*! Name      : lfvCopyQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Copy line quantities
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvCopyQty()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfvCopyQty
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laQtyPaste
*-- End of lfvCopyQty.

*!*******************************************************************************
*! Name      : lfvPasteQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Paste line quantities
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvPasteQty()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfvPasteQty
PARAMETERS llStores

IF laQtyPaste[1]+laQtyPaste[2]+laQtyPaste[3]+laQtyPaste[4]+;
   laQtyPaste[5]+laQtyPaste[6]+laQtyPaste[7]+laQtyPaste[8]=0
  *-- Message : 32033
  *-- No line quantities copied to paste!
  *-- Button : 00000 
  *-- ok
  =gfModalGen('TRM32033B00000','ALERT')
  RETURN
ENDIF
SELECT IIF(llStores,lcTempLine,lcOrdLine)
STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty
FOR lnCount = 1 TO Scale.Cnt
  lcCount = STR(lnCount,1)
  m.Qty&lcCount = laQtyPaste[lnCount]
  m.TotQty = m.TotQty + m.Qty&lcCount
ENDFOR
=lfChkTotQty(llStores)
SCATTER MEMVAR
IF llStores
  SHOW GETS WINDOW 'SOMULTI2' ONLY
  SHOW GETS WINDOW 'SOMULTI3' ONLY
  SHOW GETS WINDOW 'SOMULTI4' ONLY
  SHOW GETS WINDOW 'SOMULTI5' ONLY
  =lfRefresh('SOMULTI2')
  =lfRefresh('SOMULTI3')
  =lfRefresh('SOMULTI4')
  SHOW WINDOW (lcBrTtlM) REFRESH SAME
ELSE
  SHOW GETS WINDOW (lcWinCh32)  ONLY
  SHOW GETS WINDOW (lcWinCh321) ONLY
  SHOW GETS WINDOW (lcWinCh322) ONLY

  *C102118,1 Enable Scope in View , Edit mode. [Begin]
  IF laScrMode[2] OR laScrMode[3]
    SHOW GET PbScope ENABLE
  ELSE
    SHOW GET PbScope DISABLE
  ENDIF
  *C102118,1 Enable Scope in View , Edit mode. [End]
  
  =lfRefresh(lcWinCh321)
  SHOW WINDOW (lcOrdBrow) REFRESH SAME
ENDIF
_CUROBJ=OBJNUM(m.Qty1)
*-- End of lfvPasteQty.

*!*******************************************************************************
*! Name      : lfvLineNote
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Order line notepad
*!*******************************************************************************
*! Calls     : ARLNOTES.SPX
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfvLineNote()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfvLineNote
PARAMETERS llStores

DO (gcScrDir+"ARLNOTES.SPX")

IF llStores AND USED(lcTempline) OR USED(lcOrdLine)
  SELECT IIF(llStores,lcTempline,lcOrdLine)
  =RLOCK()
  REPLACE Flag WITH IIF(Flag='N' .OR. llStores,Flag,'M')
  UNLOCK
ENDIF
*-- End of lfvLineNote.

*!*******************************************************************************
*! Name      : lfvOrdNote
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Order notepad
*!*******************************************************************************
*! Calls     : NOTEPAD
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvOrdNote()
*!*******************************************************************************
*
FUNCTION lfvOrdNote
=NOTEPAD('B',IIF(lcOrdType='T','T','')+laData[1])
*-- End of lfvOrdNote.

*!*******************************************************************************
*! Name      : lfvRemove
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Remove Order line
*!*******************************************************************************
*! Calls     : gfModalGen
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvMRemove()
*!*******************************************************************************
*
FUNCTION lfvRemove
PARAMETERS llStores

PRIVATE lcOrdLnTag

IF m.Picked AND !EMPTY(m.PikTkt)
  *-- Message : 32056
  *-- This XXXXX has been picked. To remove first release the pick ticket.
  *-- Button : 00000 
  *-- Ok
  =gfModalGen('TRM32056B00000','ALERT',lcStyHdr)
  RETURN
ENDIF

*-- Message : 32035
*-- Are you sure you want to remove this order line?
*-- Button : 32000
*-- Yes No
IF gfModalGen('QRM32035B32000','ALERT',IIF(lcOrdType='C','contract','order'))= 1

  IF TotCut > 0 AND gfModalGen('QRM32062B32005','ALERT') = 2
    RETURN
  ENDIF

  IF llStores
    lnTotPcs = lnTotPcs - m.TotQty
    lnTotAmt = lnTotAmt - m.TotQty*m.Price
  ELSE
    SELECT (lcOrdLine)
    =RLOCK()    
    REPLACE FLAG WITH IIF(FLAG='N',FLAG,'M')
    UNLOCK

    IF TotCut > 0 
      =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
      =SEEK(IIF(Style.Make,'1','2')+m.Order+STR(m.LineNo,6),'CutPick')
      SET ORDER TO TAG 'CUTPKORD' IN (lcAlocated)
      SELECT CutPick
      SCAN REST WHILE TranCd+Order+cOrdLine = ;
                      IIF(Style.Make,'1','2')+m.Order+STR(m.LineNo,6)
        IF !SEEK(CutPick.TranCd+CutPick.cTktNo+CutPick.cTktLineNo+;
                 CutPick.Order+CutPick.cOrdLine,lcAlocated)
          INSERT INTO (lcAlocated) ;
          (cTktno,TranCd,cTktLineNo,Order,cOrdLine,Style) VALUES ;
          (CutPick.cTktNo,CutPick.Trancd,CutPick.cTktLineNo,CutPick.Order,;
           CutPick.cOrdLine,CutPick.Style)
        ENDIF
        SELECT (lcAlocated)
        =RLOCK()
        REPLACE Qty1   WITH 0 ,;
                Qty2   WITH 0 ,;
                Qty3   WITH 0 ,;
                Qty4   WITH 0 ,;
                Qty5   WITH 0 ,;
                Qty6   WITH 0 ,;
                Qty7   WITH 0 ,;
                Qty8   WITH 0 ,;
                TotQty WITH 0
        UNLOCK
        SELECT (lcOrdHdr)
        =RLOCK()
        REPLACE TotCut WITH TotCut - CutPick.TotQty
        UNLOCK
      ENDSCAN
      =gfCloseFile('CUTPICK')
      SELECT (lcOrdLine)
    ENDIF
    
    *C102118,1 SSE Commented out because they don't have Bulk Orders [Begin]
    *IF laScrMode[4] .AND. !EMPTY(laData[43]) .AND. ;
    *   SEEK(STR(BulkLineNo,6),lcBulkOrd) 
    *  SELECT (lcBulkOrd)
    *  =RLOCK()
    *  REPLACE USED1 WITH USED1 - m.Qty1 ,;
    *          USED2 WITH USED2 - m.Qty2 ,;
    *          USED3 WITH USED3 - m.Qty3 ,;
    *          USED4 WITH USED4 - m.Qty4 ,;
    *          USED5 WITH USED5 - m.Qty5 ,;
    *          USED6 WITH USED6 - m.Qty6 ,;
    *          USED7 WITH USED7 - m.Qty7 ,;
    *          USED8 WITH USED8 - m.Qty8 ,;
    *          TOTUSED WITH TOTUSED - m.TotQty
    *  UNLOCK
    *ENDIF
    *C102118,1 SSE Commented out because they don't have Bulk Orders [End]

    laData[35] = laData[35] - IIF(FLAG='N' OR llUpdBook,m.TotQty,0)
    laData[39] = laData[39] + IIF(FLAG='N' OR llUpdBook,0,m.TotQTy)

    *C102118,5 Update Cancelled Amount [Begin]
    *laData[40] = laData[40] + IIF(FLAG='N' OR llUpdBook,0,m.TotQTy)*m.Price
    IF &lcOrdLine..cStyGroup = "JY"
      laData[36] = laData[36] - IIF(FLAG='N' OR llUpdBook,((m.Qty1+m.Qty2+m.Qty3)*YouthPr)+((Qty4+Qty5+Qty6+Qty7+Qty8)*AdultPr),0)
      laData[40] = laData[40] + IIF(FLAG='N' OR llUpdBook,0,((m.Qty1+m.Qty2+m.Qty3)*YouthPr)+((Qty4+Qty5+Qty6+Qty7+Qty8)*AdultPr))
    ELSE
      laData[36] = laData[36] - IIF(FLAG='N' OR llUpdBook,m.TotQty*m.Price,0)
      laData[40] = laData[40] + IIF(FLAG='N' OR llUpdBook,0,m.TotQTy)*m.Price
    ENDIF  
    *C102118,5 Update Cancelled Amount [End]

    laData[41] = laData[41] - m.TotQty
    laData[42] = laData[42] - m.TotQty*m.Price
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE BOOK      WITH laData[35] ,;
            BOOKAMT   WITH laData[36] ,;
            CANCEL    WITH laData[39] ,;
            CANCELAMT WITH laData[40] ,;
            OPEN      WITH laData[41] ,;
            OPENAMT   WITH laData[42]
    UNLOCK
    IF laScrMode[3] AND !llUpdBook
      =gfwCodePop(@laCodes,'CCANCRESON','D')
      
      *C102118,5 Add 2 cancelled lines in case of Jersey [Begin]
      *INSERT INTO (lcOrdCanLn) ;
      * (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
      *  Style,Account,Store,Dyelot) VALUES ;
      * (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2],m.Qty1,m.Qty2,m.Qty3,;
      *  m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,;
      *  m.Style,m.Account,m.Store,m.Dyelot)
      SELECT OrdLine
      lcOrdLnTag = TAG()
      IF SEEK(&lcOrdLine..Style,'Style') AND Style.cStyGroup = "JY"
        SET ORDER TO TAG OrdLinSt
        =SEEK(lcOrdType+&lcOrdLine..Order+&lcOrdLine..Store+&lcOrdLine..Style)
        IF OrdLine.Qty1 + OrdLine.Qty2 + OrdLine.Qty3 > 0
          INSERT INTO (lcOrdCanLn) ;
           (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty1,Qty2,Qty3,TotQty,Style,Account,Store,Dyelot) VALUES ;
           (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2],m.Qty1,m.Qty2,m.Qty3,;
            m.Qty1+m.Qty2+m.Qty3,m.Style,m.Account,m.Store,m.Dyelot)
          =gfAdd_Info(lcOrdCanLn)
          IF !EOF('OrdLine')  
            SKIP 1 IN Ordline
            IF SEEK(OrdLine.Style,'Style') AND Style.cStyGroup = "JY"
              INSERT INTO (lcOrdCanLn) ;
             (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,Style,Account,Store,Dyelot) VALUES ;
             (lcOrdType,laData[1],m.LineNo+1,gdSysDate,laCanReason[lnCanReason,2],m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,;
              m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,m.Style,m.Account,m.Store,m.Dyelot)
              =gfAdd_Info(lcOrdCanLn)
            ENDIF
            SKIP -1 IN OrdLine
          ENDIF
        ELSE
          INSERT INTO (lcOrdCanLn) ;
          (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,Style,Account,Store,Dyelot) VALUES ;
          (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2],m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,;
          m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,m.Style,m.Account,m.Store,m.Dyelot)
          =gfAdd_Info(lcOrdCanLn)
        ENDIF  
      ELSE
        INSERT INTO (lcOrdCanLn) ;
         (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
          Style,Account,Store,Dyelot) VALUES ;
         (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2],m.Qty1,m.Qty2,m.Qty3,;
          m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,;
          m.Style,m.Account,m.Store,m.Dyelot)
        =gfAdd_Info(lcOrdCanLn)          
      ENDIF    
      SELECT OrdLine
      SET ORDER TO TAG (lcOrdLnTag)
      *C102118,5 Add 2 cancelled lines in case of Jersey [End]
   
      SELECT (lcOrdCanLn)
      STORE lcOrdCanLn TO laCodes[6,7],laCodes[6,8]
      STORE 'lcOrdType+laData[1]+STR(m.LineNo,6)' TO laCodes[6,9]
      =gfwCodePop(@laCodes,'CCANCRESON','T')
      STORE gdSysDate TO m.Cancelled
      DO (gcScrDir+"SOORDCLN.SPX")
      STORE '' TO laCodes[6,7],laCodes[6,8],laCodes[6,9]
      =gfAdd_Info(lcOrdCanLn)
    ENDIF 
  ENDIF
  
  IF llBomVarnt
    SELECT (lcT_BomVar)
    =SEEK("SO"+laData[1]+STR(&lcOrdLine..LineNo,6))
    SCAN REST WHILE cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(&lcOrdLine..LineNo,6)
      REPLACE cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)
    ENDSCAN
    DELETE ALL FOR cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(&lcOrdLine..LineNo,6)
  ENDIF
  
  SELECT IIF(llStores,lcTempLine,lcOrdLine)
  DELETE
  SKIP -1
  IF BOF()
    LOCATE
  ENDIF
  lcDispDet = IIF(EOF(),'DISABLE','')
  SCATTER MEMVAR
  IF llStores
    SHOW WINDOW (lcBrTtlM) REFRESH SAME
    SHOW GETS WINDOW 'SOMULTI2' &lcDispDet ONLY
    SHOW GETS WINDOW 'SOMULTI3' &lcDispDet ONLY
    SHOW GETS WINDOW 'SOMULTI4' &lcDispDet ONLY
    SHOW GETS WINDOW 'SOMULTI5' &lcDispDet ONLY
    =lfRefresh('SOMULTI2')
    =lfRefresh('SOMULTI3')
    =lfRefresh('SOMULTI4')
    SHOW GET pbClose ENABLE
    _CUROBJ = OBJNUM(ibInv200E)
  ELSE
    SHOW WINDOW (lcOrdBrow) REFRESH SAME
    SHOW GETS WINDOW (lcWinCh32)  &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh321) &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh322) &lcDispDet ONLY
    
    *C102118,1 Enable Scope in View , Edit mode. [Begin]
    IF laScrMode[2] OR laScrMode[3]
      SHOW GET PbScope ENABLE
    ELSE
      SHOW GET PbScope DISABLE
    ENDIF
    *C102118,1 Enable Scope in View , Edit mode. [End]
    
    =lfRefresh(lcWinCh321)
    _CUROBJ = OBJNUM(ibBrowOrd)
  ENDIF
  IF EOF()
    *C102118,1 Multi Store Check Box is removed from Screen [Begin] 
    *SHOW GET llMultiSt ENABLE
    *C102118,1 Multi Store Check Box is removed from Screen [End] 
    
    SHOW GET pbNew     ENABLE 

    *C102118,1 SSE Commented out because they are Removed from screen [Begin]
    *SHOW GET pbPacks   ENABLE
    *IF llMultiSt
    *  SHOW GET pbTemplate ENABLE
    *ENDIF
    *C102118,1 SSE Commented out because they are Removed from screen [End]
    
   _CUROBJ = OBJNUM(pbNew)
  ENDIF
  
  *C102118,1 Add the rest of code of A26 to Remove Push button. [Begin] 
  LOCATE
  STORE 0 TO lnPieces , lnPkAmount
  LOCATE FOR Store = lcDiv + lcTeam
  IF FOUND()
    =SEEK(lcOrdType + laData[1] + lcDiv + lcTeam)
    SCAN REST WHILE Store = lcDiv + lcTeam
      lnPieces = lnPieces + TotQty
      lnPkAmount = lnPkAmount + (TotQty*Price) + (Adult*AdultPr) + (Youth*YouthPr)
    ENDSCAN
  ELSE
    SHOW GET PbRemove  DISABLE
    SHOW GET PbUpdate  DISABLE
  ENDIF
  =lfAdlYo()
  =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
  *C102118,1 Add the rest of code of A26 to Remove Push button. [End]

  *C102118,4 Calculate book Qty. [Begin]
  =lfCalcAmnt()
  *C102118,4 Calculate book Qty. [End]
  
  STORE .T. TO llCUpdate
ENDIF
*-- End of lfvRemove.

*!*******************************************************************************
*! Name      : lfvTemplate
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Template order lines
*!*******************************************************************************
*! Calls     : SOMILTI.SPX
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvTemplate()
*!*******************************************************************************
*C102118,1 This function in Detail Folder is not used anymore.
*
FUNCTION lfvTemplate
PRIVATE lnOrdRecNo

SELECT (lcOrdline)
lnOrdRecNo = RECNO()
SET RELATION TO
SELECT (lcTempline)
SUM TotQty,TotQty*Price TO lnTotPcs,lnTotAmt
SET RELATION TO Style INTO Style
SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE

IF laScrMode[4] AND !EMPTY(laData[43])
  SET RELATION TO STR(BulkLineNo,6) INTO (lcBulkOrd) ADDITIVE
ENDIF
SET ORDER TO TAG ORDLINE
LOCATE
SCATTER MEMVAR
DO (gcScrDir+gcWinAppl+"\SOMULTI.SPX")
SELECT (lcTempline)
IF SEEK(lcOrdType+laData[1])
  *C102118,1 Multi Store Check Box is removed from Screen [Begin] 
  *SHOW GET llMultiSt DISABLE
  *C102118,1 Multi Store Check Box is removed from Screen [End] 
  
  =gfSavSess(lcProgID,lcFiles+'lcTempline,'+lcTempline+',ORDLINE;',@laVariables,lcSession)
ELSE
  =gfSavSess(lcProgID,lcFiles,@laVariables,lcSession)
ENDIF
SET RELATION TO
SELECT (lcOrdline)
SET RELATION TO Style INTO Style
SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE
IF BETWEEN(lnOrdRecNo,1,RECCOUNT())
  GO lnOrdRecNo
ELSE
 LOCATE
ENDIF
SCATTER MEMVAR
=lfActFolder()
*-- End of lfvTemplate.

*!*******************************************************************************
*! Name      : lfvShowBulk
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Show bulk order booked, open, used and balance quantities
*!*******************************************************************************
*! Calls     : ORD100B.SPX
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvShowBulk()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfvShowBulk
PRIVATE lnAlias
lnAlias  = SELECT()
SELECT (lcBulkOrd)
DO (gcScrDir+gcWinAppl+"\SoBulk.SPX")
SELECT (lnAlias)
*-- End of lfvShowBulk.

*!*******************************************************************************
*! Name      : lfvCommision
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Recalculate order commissions
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvCommision()
*!*******************************************************************************
*
FUNCTION lfvCommision

IF SEEK(laData[27],'SalesRep')
  DIMENSION laDiscnt[4,2]
  laDiscnt[1,1] = SalesRep.Discnt1
  laDiscnt[1,2] = SalesRep.Redcom1
  laDiscnt[2,1] = SalesRep.Discnt2
  laDiscnt[2,2] = SalesRep.Redcom2
  laDiscnt[3,1] = SalesRep.Discnt3
  laDiscnt[3,2] = SalesRep.Redcom3
  laDiscnt[4,1] = SalesRep.Discnt4
  laDiscnt[4,2] = SalesRep.Redcom4
  
  =ASORT(laDiscnt)
  DO CASE
    CASE laData[16] >= laDiscnt[4,1]
      laData[28] = laData[28] * (1-laDiscnt[4,2]/100)
    CASE laData[16] >= laDiscnt[3,1]
      laData[28] = laData[28] * (1-laDiscnt[3,2]/100)
    CASE laData[16] >= laDiscnt[2,1]
      laData[28] = laData[28] * (1-laDiscnt[2,2]/100)
    CASE laData[16] >= laDiscnt[1,1]
      laData[28] = laData[28] * (1-laDiscnt[1,2]/100)
  ENDCASE
  
  IF laData[28] <> &lcOrdHdr..Comm1
    *-- Message : 32036
    *-- xxx commission decreased to xxx%
    *-- Button : 00000
    *-- Ok
    =gfModalGen('INM32036B00000','ALERT',laData[27]+'|'+STR(laData[28],5,2))
  ENDIF
ENDIF
IF SEEK(laData[29],'SalesRep')
  DIMENSION laDiscnt[4,2]
  laDiscnt[1,1] = SalesRep.Discnt1
  laDiscnt[1,2] = SalesRep.Redcom1
  laDiscnt[2,1] = SalesRep.Discnt2
  laDiscnt[2,2] = SalesRep.Redcom2
  laDiscnt[3,1] = SalesRep.Discnt3
  laDiscnt[3,2] = SalesRep.Redcom3
  laDiscnt[4,1] = SalesRep.Discnt4
  laDiscnt[4,2] = SalesRep.Redcom4
  =ASORT(laDiscnt)
  DO CASE
    CASE laData[16] >= laDiscnt[4,1]
      laData[30] = laData[30] * (1-laDiscnt[4,2]/100)
    CASE laData[16] >= laDiscnt[3,1]
      laData[30] = laData[30] * (1-laDiscnt[3,2]/100)
    CASE laData[16] >= laDiscnt[2,1]
      laData[30] = laData[30] * (1-laDiscnt[2,2]/100)
    CASE laData[16] >= laDiscnt[1,1]
      laData[30] = laData[30] * (1-laDiscnt[1,2]/100)
  ENDCASE
  
  IF laData[30] <> &lcOrdHdr..Comm2
    *-- Message : 32036
    *-- xxx commission decreased to xxx%
    *-- Button : 00000
    *-- Ok
    =gfModalGen('INM32036B00000','ALERT',laData[29]+'|'+STR(laData[30],5,2))
  ENDIF
ENDIF
SELECT (lcOrdHdr)
=RLOCK()
REPLACE Comm1 WITH laData[28] ,;
        Comm2 WITH laData[30]
UNLOCK
SHOW GET laData[28]
SHOW GET laData[30]
*-- End of lfvCommision.

*!*******************************************************************************
*! Name      : lfvApp2St
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Apply template order lines to stores
*!*******************************************************************************
*! Calls     : SOSTORE.SPX
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfvApp2St()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfvApp2St

STORE 0 TO lnTopMark,lnBotMark,lnSTotPcs,lnSTotAmt
DIMENSION laKeyExp[1]
laKeyExp[1] = 'ACCOUNT'
SELECT *,000000 AS nSesUsed FROM (lcTempline) WHERE Flag = "�" INTO DBF (gcWorkDir+lcSelLines)
SUM TotQty,TotQty*Price TO lnSelPcs,lnSelAmt
SET RELATION TO Style INTO Style
LOCATE
=gfOpenFile(gcSysHome+'SycInt',gcSysHome+'Ccontcode','SH')
=SEEK(gcContCode,'SycInt')

SELECT Account,Store,StName,cAddress3,cAddress4,cAddress5,000000 AS Pieces,;
00000000000.00 AS AMOUNT, SPACE(15) AS CustPo,SPACE(1) AS cSelect, 000 AS Count;
FROM Customer WHERE Type+Account+Store='S'+laData[2] .AND. Status$'AP';
INTO DBF(gcWorkDir+lcStores)

INDEX ON cSelect+Account+Store TAG (lcStores1)
INDEX ON Account+Store TAG (lcStores)

DO (gcScrDir+gcWinAppl+"\SOSTORE.SPX")
=gfCloseFile('SycInt')
USE IN (lcSelLines)
=lfBrowMulti()
*-- End of lfvApp2St.

*!*******************************************************************************
*! Name      : lfWStores
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Browse function for stores screen (SOSTORE)
*!*******************************************************************************
*! Calls     : lfShowStore
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfWStores()
*!*******************************************************************************
*This function is not used anymore
FUNCTION lfWStores
PRIVATE lcFields 

lcFields = [cMarker =IIF(RECNO()=lnTopMark,'>',' '):H=' ':R:1:W=.F.,]+;
           [Style  :H=lcStyHdr :R :19,]

IF laScrMode[4] .AND. !EMPTY(laData[43])
  lcFields = lcFields + [TotQty :H='Pieces' :R :6,]+;
                        [Amount =Price*TotQty :P= '99999999999.99' :H='Amount',]+;
                        [&lcBulkOrd..TotBook :H= 'Booked',]+;
                        [&lcBulkOrd..TotOpen :H= 'Open' ,]+;
                        [X= &lcBulkOrd..TotUsed+nSesUsed :H= 'Used',]+;
                        [nBalance = MAX(&lcBulkOrd..TotOpen-&lcBulkOrd..TotUsed-nSesUsed,0) :H= 'Balance',]+;
                        [Style.Desc :H='Description':R :20,]+;
                        [Group :H='Grp' :R,]+;
                        [Price :H='Price' :R :9]
ELSE
  lcFields = lcFields + [Style.Desc :H='Description':R :20,]+;
                        [Group :H='Group' :R,Gros_Price :H='Gross Price' :R,]+;
                        [Disc_Pcnt :H='Discount' :R,Price :H='Net Price' :R :9,]+;
                        [TotQty :H='Pieces' :R :6,]+;
                        [Amount  =Price*TotQty :P= '99999999999.99' :H='Amount']
ENDIF

SELECT (lcSelLines)
lnTopMark = RECNO()
BROWSE FIELDS &lcFields ;
       WINDOW SOSTORE1  ;
       IN WINDOW SOSTORE;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       WHEN lfShowStore();
       TITLE lcBrTtS1    ;
       VALID :F lfVBrTtS1()

SELECT (lcStores)
lnBotMark = RECNO()
lcBrFields = "cMarker =IIF(RECNO()=lnBotMark,'>',' '):H=' ':R:1:W=.F.,;
              cSelect :H=' ' :R, Store :H='Store' :8:R, StName :H='Name':R :30,"
lcBrFields = lcBrFields + IIF(laData[7],"CustPo :H='Cust. PO.' :15 :W= cSelect='�',",'')
lcBrFields = lcBrFields + "n=Pieces*Count :H='Pieces' :P='999999' :R,;
             n1=Amount*Count :H='Amount' :P='99999999999.99' :R,"
lcBrFields =  lcBrFields + "cAddress3 :H=SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),;
              cAddress4 :H=SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),;
              cAddress5 :H=SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len)"

BROWSE FIELDS &lcBrFields ;
       WINDOW SOSTORE3  ;
       IN WINDOW SOSTORE;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       WHEN lfShowStore();
       TITLE lcBrTtS2    ;
       VALID :F lfVBrTtS2()
*-- End of lfWStores.

*!*******************************************************************************
*! Name      : lfShowStore
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Show function for stores screen (soStore)
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfShowStore()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfShowStore

lnTopMark = RECNO(lcSelLines)
SHOW WINDOW (lcBrTtS1) REFRESH SAME
lnBotMark = RECNO(lcStores)
SHOW WINDOW (lcBrTtS2) REFRESH SAME
IF &lcStores..cSelect = SPACE(1)
  SHOW GET pbSSelect,1 PROMPT '\<Select'
  SHOW GET pbIncrease DISABLE
  SHOW GET pbDecrease DISABLE
ELSE
  SHOW GET pbSSelect,1 PROMPT 'Un\<Select'
  SHOW GET pbIncrease ENABLE
  SHOW GET pbDecrease ENABLE
ENDIF

lnAlias = SELECT(0)
SELECT (lcStores)
LOCATE
IF EOF()
  SHOW GET pbSSelect  DISABLE
  SHOW GET pbSSelAll  DISABLE
  SHOW GET pbSSelNone DISABLE
  SHOW GET pbSInvert  DISABLE
  SHOW GET pbIncrease DISABLE
  SHOW GET pbDecrease DISABLE
  SHOW GET pbSApply   DISABLE
  
ELSE
  GO lnBotMark
  SHOW WINDOW (lcBrTtS2) REFRESH SAME

ENDIF
SELECT (lnAlias)
*-- End of lfShowStore.

*!*******************************************************************************
*! Name      : lfDStores
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Deactivate function for stores screen (soStore)
*!*******************************************************************************
*! Calls     : lpTab
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfDStores()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfDStores

IF WONTOP()=lcBrTtS1 .OR. WONTOP()=lcBrTtS2 
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME LOCATE
  ON KEY LABEL CTRL+END  GO BOTTOM
  DO CASE 
    CASE WONTOP() = lcBrTtS1 
      ON KEY LABEL TAB     DO lpTab WITH 'SOSTORE4','ibOrd100S2'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'SOSTORE4','pbClose'
    CASE WONTOP() = lcBrTtS2 
      ON KEY LABEL TAB     DO lpTab WITH 'SOSTORE4','pbSSelect'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'SOSTORE2','ibOrd100S1'
      lcExpToSeek=""
      lcOrdExpr=""
      lnStartTrap = 32
      lnEndTrap   = 126
      FOR lnChrToTrap = lnStartTrap TO lnEndTrap
        ON KEY LABEL (CHR(lnChrToTrap)) DO lfIncSearch &&lfGetRange
      ENDFOR
  ENDCASE 
ENDIF

RETURN .F.
*-- End of lfDStores.

*!*******************************************************************************
*! Name      : lfVBrTtS1
*! Developer : Ahmed Ibrahim
*! Date      : 09/22/1999
*! Purpose   : Valid function Of the first browse in MULTI store screen
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfVBrTtS1
IF WONTOP() = lcBrTtS2
  lcBrowseTl=lcBrTtS2
  =lfReadAct()
  =lfDStores()
ENDIF
*-- End of lfVBrTtS1.

*!*******************************************************************************
*! Name      : lfVBrTtS2
*! Developer : Ahmed Ibrahim
*! Date      : 09/22/1999
*! Purpose   : Valid function Of the second browse in MULTI store screen
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfVBrTtS2
IF WONTOP() = lcBrTtS1
  lcBrowseTl=lcBrTtS1
  =lfReadAct()
  =lfDStores()  
ENDIF
*-- End of lfVBrTtS2.

*!*******************************************************************************
*! Name      : lfAdjSButt
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Select, Unselect, Select all, Select None and Invert stores
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfAdjSButt()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfAdjSButt
PARAMETERS lcType
PRIVATE lcKey,lcDataSel,lcDataUSel,lnRecNo
SELECT (lcStores)
lcKey = Account+Store
DO CASE 
  CASE lcType = 'S'
    REPLACE cSelect  WITH IIF(cSelect=SPACE(1),"�",SPACE(1)) ,;
            CustPo   WITH IIF(cSelect=SPACE(1),SPACE(15),CustPo) ,;
            Pieces   WITH IIF(cSelect=SPACE(1),0,lnSelPcs) ,;
            Amount   WITH IIF(cSelect=SPACE(1),0,lnSelAmt) ,;
            Count    WITH IIF(cSelect=SPACE(1),0,1)
    SKIP
    IF EOF()
      SKIP -1
    ENDIF
    lcKey = Account+Store
  CASE lcType = 'A'
    REPLACE ALL cSelect WITH "�",;
                Pieces  WITH lnSelPcs ,;
                Amount  WITH lnSelAmt,;
                Count   WITH 1
  CASE lcType = 'N'
    REPLACE ALL cSelect WITH SPACE(1) ,;
                CustPo  WITH SPACE(15) ,;
                Pieces  WITH 0 ,;
                Amount  WITH 0 ,;
                Count   WITH 0
  CASE lcType = 'V'
    REPLACE ALL cSelect WITH IIF(cSelect=SPACE(1),"�",SPACE(1)) ,;
                CustPo  WITH IIF(cSelect=SPACE(1),SPACE(15),CustPo) ,;
                Pieces  WITH IIF(cSelect=SPACE(1),0,lnSelPcs) ,;
                Amount  WITH IIF(cSelect=SPACE(1),0,lnSelAmt) ,;
                Count   WITH IIF(cSelect=SPACE(1),0,1)
  CASE lcType = 'I'
    REPLACE Count WITH Count + 1
  CASE lcType = 'D'
    REPLACE Count WITH Count - 1 ,;
            cSelect WITH IIF(Count=0,SPACE(1),cSelect)
  OTHERWISE
ENDCASE
SET ORDER TO TAG (lcStores1)
lcDataSel  = IIF(SEEK("�"),'ENABLE','DISABLE')
SUM REST Pieces*Count,Amount*Count,Count TO lnSTotPcs,lnSTotAmt,lnStCount ;
    WHILE cSelect = "�"
lcDataUSel = IIF(SEEK(' '),'ENABLE','DISABLE')
SET ORDER TO TAG (lcStores)
=SEEK(lcKey)
lcData = IIF(EOF(),'DISABLE','ENABLE')
SHOW GET pbSSelAll  &lcDataUSel
SHOW GET pbSSelNone &lcDataSel
SHOW GET pbSApply   &lcDataSel
IF cSelect = SPACE(1)
  SHOW GET pbSSelect,1 PROMPT '\<Select'
  SHOW GET pbIncrease DISABLE
  SHOW GET pbDecrease DISABLE
ELSE
  SHOW GET pbSSelect,1 PROMPT 'Un\<Select'
  SHOW GET pbIncrease ENABLE
  SHOW GET pbDecrease ENABLE
ENDIF
SELECT (lcSelLines)
lnRecNo = RECNO()
REPLACE ALL nSesUsed WITH TotQty*lnStCount
GO lnRecNo
SELECT (lcStores)
SHOW GETS WINDOW 'SoStore2' OFF
*-- End of lfAdjSButt.

*!*******************************************************************************
*! Name      : lfvSApply
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Append selected stores to the order line temp file
*!*******************************************************************************
*! Calls     : lfAdjSButt
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfvSApply()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfvSApply

SET ORDER TO TAG (lcTempline) IN (lcTempline)
SELECT (lcStores)
SET ORDER TO TAG (lcStores1)
IF SEEK("�")
  llCUpdate = .T.
  SCAN REST WHILE cSelect = "�"
    lnCount=Count
    SELECT (lcTempline)
    =SEEK("�")
    SCAN REST WHILE Flag = "�"
      SCATTER MEMVAR MEMO
      WAIT 'Adding store/style : '+&lcStores..Store+'/'+m.Style WINDOW NOWAIT
      lnLines = lnLines + 1
      IF laScrMode[4] .AND. !EMPTY(cFromOrder) .AND. ;
        SEEK(STR(BulkLineNo,6),lcBulkOrd)
        SELECT (lcBulkOrd)
        =RLOCK()
        REPLACE Used1 WITH Used1 + m.Qty1*lnCount ,;
                Used2 WITH Used2 + m.Qty2*lnCount ,;
                Used3 WITH Used3 + m.Qty3*lnCount ,;
                Used4 WITH Used4 + m.Qty4*lnCount ,;
                Used5 WITH Used5 + m.Qty5*lnCount ,;
                Used6 WITH Used6 + m.Qty6*lnCount ,;
                Used7 WITH Used7 + m.Qty7*lnCount ,;
                Used8 WITH Used8 + m.Qty8*lnCount ,;
                TotUsed WITH TotUsed + m.TotQty*lnCount
        UNLOCK
      ENDIF
      INSERT INTO (lcOrdLine) ;
      (cOrdType,Order,Account,LineNo,Store,Style,Desc1,Season,Scale,Group,PrePak,PPQty,Price,;
       Gros_Price,Disc_Pcnt,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
       Book1,Book2,Book3,Book4,Book5,Book6,Book7,Book8,TotBook,Note_Mem,;
       Comm1,Comm2,Pack_Id,cFromOrder,BulkLineNo,CustPo,Flag,cWareCode,Gl_Sales,Complete) VALUES ;
      (lcOrdType,laData[1],laData[2],lnLines,&lcStores..Store,m.Style,m.Desc1,;
       m.Season,m.Scale,m.Group,m.PrePak,m.PPQty,m.Price,m.Gros_Price,m.Disc_Pcnt,;
       m.Qty1*lnCount,m.Qty2*lnCount,m.Qty3*lnCount,m.Qty4*lnCount,m.Qty5*lnCount,;
       m.Qty6*lnCount,m.Qty7*lnCount,m.Qty8*lnCount,m.TotQty*lnCount,m.Qty1*lnCount,;
       m.Qty2*lnCount,m.Qty3*lnCount,m.Qty4*lnCount,m.Qty5*lnCount,m.Qty6*lnCount,;
       m.Qty7*lnCount,m.Qty8*lnCount,m.TotQty*lnCount,m.Note_Mem,m.Comm1,;
       m.Comm2,m.Pack_Id,m.cFromOrder,m.BulkLineNo,&lcStores..CustPo,'N',;
       laData[31],m.Gl_Sales,m.Complete)
      laData[35] = laData[35] + m.TotQty*lnCount
      laData[36] = laData[36] + m.TotQty*lnCount*m.Price
      laData[41] = laData[41] + m.TotQty*lnCount
      laData[42] = laData[42] + m.TotQty*lnCount*m.Price
    ENDSCAN
    LOCATE
  ENDSCAN
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE BOOK    WITH laData[35] ,;
          BOOKAMT WITH laData[36] ,;
          OPEN    WITH laData[41] ,;
          OPENAMT WITH laData[42]
  UNLOCK
  SELECT (lcSelLines)
  REPLACE ALL nSesUsed WITH 0         
  LOCATE  
  SELECT (lcStores)
  REPLACE ALL cSelect WITH SPACE(1) ,;
              CustPo  WITH SPACE(15),;
              Pieces  WITH 0 ,;
              Amount  WITH 0 ,;
              Count   WITH 0
  LOCATE
  WAIT CLEAR
ENDIF
SET ORDER TO TAG (lcStores) IN (lcStores)
SET ORDER TO TAG ORDLINE IN (lcTempline)
=lfAdjSButt('')
*-- End of lfvSApply.

*!*******************************************************************************
*! Name      : lfAdjButt
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Adjust buttons for order template screen
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfAdjButt()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfAdjButt
PARAMETERS lcType
PRIVATE lcKey,lcDataSel,lcDataUSel,lcData

SELECT (lcTempLine)
lcKey = cOrdType+Order+STR(LineNo,6)
DO CASE
  CASE lcType = 'S'
    IF EOF()
      LOCATE
    ENDIF  
    REPLACE Flag WITH IIF(Flag=SPACE(1),"�",SPACE(1))
    SKIP
    IF EOF()
      SKIP -1
    ENDIF
    lcKey = cOrdType+Order+STR(LineNo,6)
  CASE lcType = 'A'
    REPLACE ALL Flag WITH "�"
  CASE lcType = 'N'
    REPLACE ALL Flag WITH SPACE(1)
  CASE lcType = 'I'
    REPLACE ALL Flag WITH IIF(Flag=SPACE(1),"�",SPACE(1))
  CASE lcType = 'L'
    *-- Message : 32043
    *-- Are you sure you want to clear template order lines?
    *-- Button : 32000 
    *-- Yes/No
    IF gfModalGen('QRM32043B32000','DIALOG',IIF(lcOrdType='C','contract','order'))=2
      RETURN
    ENDIF
    SELECT (lcTempLine)
    DELETE ALL
    SCATTER MEMVAR BLANK MEMO
    STORE 0 TO lnTotPcs,lnTotAmt
    SHOW WINDOW (lcBrTtlM) REFRESH SAME
    SHOW GETS WINDOW 'SOMULTI2' DISABLE ONLY
    SHOW GETS WINDOW 'SOMULTI3' DISABLE ONLY
    SHOW GETS WINDOW 'SOMULTI4' DISABLE ONLY
    SHOW GETS WINDOW 'SOMULTI5' DISABLE ONLY
    =lfRefresh('SOMULTI2')
    =lfRefresh('SOMULTI3')
    =lfRefresh('SOMULTI42')
    SHOW GET pbNew   ENABLE

    *C102118,1 SSE Commented out because it is Removed from screen [Begin]
    *SHOW GET pbPacks ENABLE
    *C102118,1 SSE Commented out because it is Removed from screen [End]
    
    SHOW GET pbClose ENABLE
    _CUROBJ = OBJNUM(pbNew)
    RETURN
  OTHERWISE
ENDCASE  
SET ORDER TO TAG (lcTempLine)
lcDataSel   = IIF(SEEK("�"),'ENABLE','DISABLE')
lcDataUSel  = IIF(SEEK(' '),'ENABLE','DISABLE')
SET ORDER TO TAG OrdLine
=SEEK(lcKey)
lcData  = IIF(EOF(),'DISABLE','ENABLE')
SHOW GET pbSelAll  &lcDataUSel
SHOW GET pbSelNone &lcDataSel
SHOW GET pbStores  &lcDataSel
SHOW GET pbClear   &lcData
SHOW GET pbInvert  &lcData
IF Flag = SPACE(1)
  SHOW GET pbSelect,1 PROMPT '\<Select' &lcData
ELSE
  SHOW GET pbSelect,1 PROMPT 'Un\<Select' &lcData
ENDIF
*-- End of lfAdjButt.

*!*******************************************************************************
*! Name      : lfvPacks
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Calls packs screen
*!*******************************************************************************
*! Calls     : lfAccPacks,gfModalGen,ORD100P.SPX,lfAdjMButt
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvPacks()
*!*******************************************************************************
*C102118,1 This function in Detail Folder is not used anymore.
*
FUNCTION lfvPacks
PARAMETERS llStores
PRIVATE lnAlias,lcDataSel

lnAlias = SELECT()

=gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPck_Hdr','SH')
=gfOpenFile(gcDataDir+'SPck_Lin',gcDataDir+'SPck_Lin','SH')
SELECT SPck_Lin
SET RELATION TO Style INTO Style

IF !lfAccPacks()
  *-- Message : 32037
  *-- There are neither generic packs nor packs for account xxx.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('TRM32037B00000','ALERT',laData[2])
  SELECT (lnAlias)
  RETURN
ENDIF
lcAccount = laData[2]
SELECT (lcPackHdr)
SET RELATION TO Type+Account+Pack_Id INTO SPck_Lin
DIMENSION laKeyExp[1]
laKeyExp[1] = 'ACCOUNT'
DO (gcScrDir+"arPacks.SPX") WITH llStores
SELECT (lcPackHdr)
USE
ERASE (gcWorkDir+lcPackHdr+".DBF")
ERASE (gcWorkDir+lcPackHdr+".CDX")

=gfCloseFile('SPck_Hdr')
=gfCloseFile('SPck_Lin')

SELECT (LcTempLine)
lcoldtag = ORDER()
SET ORDER TO TAG (lcTempLine)
lcDataSel   = IIF(SEEK("�"),'ENABLE','DISABLE')
SET ORDER TO &lcoldtag

SELECT (lnAlias)
IF !llStores .AND. llMultiSt .AND. SEEK(lcOrdType+laData[1],lcTempline)
  SHOW GETS WINDOW 'SOMULTI2' ENABLE ONLY
  SHOW GET pbStores  &lcDataSel
  SHOW GETS WINDOW 'SOMULTI3' ENABLE ONLY
  SHOW GETS WINDOW 'SOMULTI4' ENABLE ONLY
  SHOW GETS WINDOW 'SOMULTI5' ENABLE ONLY
  _CUROBJ = OBJNUM(ibInv200E)
ENDIF
IF (llStores .OR. !llMultiSt ) .AND. SEEK(lcOrdType+laData[1],lcOrdline)
  =lfActFolder()
ENDIF
*-- End of lfvPacks.

*!*******************************************************************************
*! Name      : lfAccPacks
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Create temporary file holding account packs and generic packs
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfAccPacks()
*!*******************************************************************************
*C102118,1 This function in Detail Folder is not used anymore.
*
FUNCTION lfAccPacks
PRIVATE lnPacks

CREATE DBF (gcWorkDir+lcPackHdr) ;
        (Type C(1),Account C(5),Pack_Id C(16),Desc C(20),Season C(6),;
         cDivision C(6),TotWip N(6),TotStk N(6),TotOrd N(6),TotQty N(6),PackQty N(6))
INDEX ON Account+Pack_Id TAG (lcPackHdr)
SELECT Spck_Lin
SET RELATION TO Type+Account+Pack_Id INTO SPck_Hdr ADDITIVE

lnPacks = 0

WAIT WINDOW 'Collecting data. Please wait.....' NOWAIT
IF SEEK('P'+laData[2],'SPck_Lin')
  *-- Get the first record to be updated under needed condition (if any) so that
  *-- the following variables will belong to it not to unproper record.
  LOCATE REST WHILE Type+Account='P'+laData[2] ;
              FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]
  lnPacks = IIF(FOUND(),1,0)
  SELECT SPck_Lin
  lcPack_Id = SPck_Lin.Pack_Id
  m.Desc = SPck_Hdr.Desc
  m.Season = SPck_Hdr.Season
  m.cDivision = SPck_Hdr.cDivision
  STORE 0 TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty  
  SCAN REST WHILE Type+Account='P'+laData[2] ;
            FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]
    WAIT WINDOW lcStyHdr+Style+"\"+Pack_id NOWAIT
    IF lcPack_Id # SPck_Lin.Pack_Id
      INSERT INTO (lcPackHdr) ;
      (Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
       VALUES ('P',laData[2],lcPack_Id,m.Desc,m.Season,;
            m.cDivision,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
      lcPack_Id = SPck_Lin.Pack_Id
      m.Desc = SPck_Hdr.Desc
      m.Season = SPck_Hdr.Season
      m.cDivision = SPck_Hdr.cDivision
      lnPacks = lnPacks + 1
      STORE 0 TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
    ENDIF
    lnPackQty = lnPackQty+Spck_Lin.TotQty
    lnPackWip = lnPackWip+Style.TotWip
    lnPackOrd = lnPackOrd+Style.TotOrd
    lnPackStk = lnPackStk+Style.TotStk
  ENDSCAN
  IF lnPacks > 0
    INSERT INTO (lcPackHdr) ;
                (Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
          VALUES ('P',laData[2],lcPack_Id,m.Desc,m.Season,;
                 m.cDivision,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
  ENDIF
ENDIF
WAIT WINDOW 'Collecting data. Please wait.....' NOWAIT
IF SEEK('P*****','SPck_Lin')
  SELECT SPck_Lin
  *-- Get the first record to be updated under needed condition (if any) so that
  *-- the following variables will belong to it not to unproper record.
  LOCATE REST WHILE Type+Account='P*****';
              FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]
  lnPacks = IIF(FOUND(),1,0)              
  lcPack_Id = SPck_Lin.Pack_Id
  m.Desc = SPck_Hdr.Desc
  m.Season = SPck_Hdr.Season
  m.cDivision = SPck_Hdr.cDivision

  STORE 0 TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
  SCAN REST WHILE Type+Account='P*****' ;
     FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]
     WAIT WINDOW lcStyHdr+Style+"\"+Pack_id NOWAIT
    IF lcPack_Id # SPck_Lin.Pack_Id
      INSERT INTO (lcPackHdr) ;
      (Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
       VALUES ('P','*****',lcPack_Id,m.Desc,m.Season,;
            m.cDivision,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)

      lcPack_Id = SPck_Lin.Pack_Id
      m.Desc = SPck_Hdr.Desc
      m.Season = SPck_Hdr.Season
      m.cDivision = SPck_Hdr.cDivision
      lnPacks = lnPacks + 1
      STORE 0 TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
    ENDIF
    lnPackQty = lnPackQty+Spck_Lin.TotQty
    lnPackWip = lnPackWip+Style.TotWip
    lnPackOrd = lnPackOrd+Style.TotOrd
    lnPackStk = lnPackStk+Style.TotStk
  ENDSCAN
  IF lnPacks > 0
    INSERT INTO (lcPackHdr) ;
                (Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
           VALUES ('P','*****',lcPack_Id,m.Desc,m.Season,;
                   m.cDivision,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
  ENDIF
ENDIF
RETURN(RECCOUNT(lcPackHdr)>0)
*-- End of lfAccPacks.

*!*******************************************************************************
*! Name      : lfwAccPacks
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : READ WHEN function for packs screen (ORD100P)
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfwAccPacks()
*!*******************************************************************************
*C102118,1 This function is not used anymore.
FUNCTION lfwAccPacks

SELECT (lcPackHdr)
LOCATE
lnTopMark = RECNO()
BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnTopMark,'>',' '):H=' ':R:1:W=.F.,;
       Account  :H='Account' :R :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       Pack_Id  :H='Pack Id' :R :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       Desc     :H='Description':R :20 :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       PackQty  :H='Pack' :W =lfTrapKeys(.F.) :V= PackQty >= 0 AND lfTrapKeys(.T.),;
       TotWip   :H='Wip':R :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       TotStk   :H='Stock':R :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       TotOrd   :H='Ordered' :R :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       TotQty   :H='Qty.' :R :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       lcSesDesc=gfCodDes(Season,'SEASON') :H= 'Season' :20 :R :W=lfVPacksWin() :V=lfVPacksWin() :F,;
       lcDivDesc=gfCodDes(cDivision,'CDIVISION') :H= 'Division' :20 :R :W=lfVPacksWin() :V=lfVPacksWin() :F;
       WINDOW arpacks1  ;
       IN WINDOW arpacks;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       WHEN lfSAccPack() ;
       TITLE lcBrTtl1 ;
       VALID :F lfVPacksWin()

SELECT Spck_Lin
lnBotMark = RECNO()
BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnBotMark,'>',' '):H=' ':R:1:W=.F.,;
       Style :H = lcStyHdr :R :W=lfTrapKeys(.F.) :V=lfTrapKeys(.F.) :F,;
       Style.TotWip :H= 'WIP' :R :W=lfTrapKeys(.F.) :V=lfTrapKeys(.F.) :F,;
       Style.TotStk :H= 'Stock' :R :W=lfTrapKeys(.F.) :V=lfTrapKeys(.F.) :F,;
       Style.TotOrd :H= 'Ordered' :R :W=lfTrapKeys(.F.) :V=lfTrapKeys(.F.) :F,;
       TotQty :H='Qty.' :R :W=lfTrapKeys(.F.) :V=lfTrapKeys(.F.) :F;
       WINDOW arpacks2   ;
       IN WINDOW arpacks ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WHEN lfPackDet() ;
       TITLE lcBrTtl2 ;
       VALID :F lfTrapKeys(.F.)
*-- End of lfwAccPacks.

*!*******************************************************************************
*! Name      : lfPackDet
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : When function for packs details browse
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfPackDet()
*!*******************************************************************************
*C102118,1 This function is not used anymore.
FUNCTION lfPackDet

IF llClearPck
  llClearPck = .F.
  FOR lnChrToTrap = 32 TO 126
    ON KEY LABEL (CHR(lnChrToTrap))
  ENDFOR
ENDIF
*-- End of lfPackDet.

*!*******************************************************************************
*! Name      : lfSAccPack
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Show function for packs screen (ORD100P)
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfSAccPack()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfSAccPack
lnTopMark = RECNO(lcPackHdr)
SHOW WINDOW (lcBrTtl1) REFRESH SAME
lnBotMark = RECNO('Spck_Lin')
SHOW WINDOW (lcBrTtl2) REFRESH SAME
llClearPck = .T.
*-- End of lfSAccPack.

*!*******************************************************************************
*! Name      : lfDAccPack
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Deactivate function for packs screen
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfDAccPack()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfDAccPack

IF WONTOP() = lcBrTtl1 OR WONTOP() = lcBrTtl2
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME LOCATE
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ALT+Z DO lpTab WITH 'arPacks3','pbZoom'
  ON KEY LABEL ALT+A DO lpTab WITH 'arPacks3','pbApply'
  ON KEY LABEL ALT+C DO lpTab WITH 'arPacks3','pbCancel'
  DO CASE
    CASE WONTOP() = lcBrTtl1
      ON KEY LABEL TAB     DO lpTab WITH 'arPacks3','ibPacks2'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'arPacks3','pbCancel'
    CASE WONTOP() = lcBrTtl2
      ON KEY LABEL TAB     DO lpTab WITH 'arPacks3','pbZoom'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'arPacks3','ibPacks1'
  ENDCASE
ENDIF  
RETURN .F.
*-- End of lfDAccPack.

*!*******************************************************************************
*! Name      : lfvZoom
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Show pack line details
*!*******************************************************************************
*! Calls     : arpacksz.SPX
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfvZoom()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfvZoom
DO (gcScrDir+"arpacksz.SPX")
*-- End of lfvZoom

*!*******************************************************************************
*! Name      : lfvPckApply
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Apply selected packs to order lines
*!*******************************************************************************
*! Calls     : gfModalGen
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfvPckApply()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfvPckApply
PARAMETERS llStores,lcStore

IF llStores .AND. EMPTY(lcStore)
  RETURN
ENDIF
SELECT (lcPackHdr)
LOCATE FOR PackQty <> 0
IF !FOUND()
  *-- Message : 32038
  *-- No packs selected
  *-- Button : 00000
  *-- Ok
  =gfModalGen('TRM32038B00000','ALERT','packs')
  LOCATE
  RETURN
ENDIF
STORE .T. TO llCUpdate
SCAN FOR PackQty <> 0
  IF !lfValidPack()
    LOOP
  ENDIF
  SELECT Spck_Lin
  SCAN REST WHILE Type+Account+Pack_Id=&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id
    WAIT 'Adding '+ALLTRIM(Pack_ID)+'/'+ALLTRIM(Style) WINDOW NOWAIT
    =SEEK(Style,'STYLE')

    IF laSetups[5,2]='Y' .AND. !SEEK(Style+laData[31]+SPACE(10),'StyDye')
      DO gpAdStyWar WITH Style,SPACE(10),laData[31]
    ENDIF
    STORE 0 TO m.Gros_Price,m.Price,m.Disc_Pcnt
    m.lContract = lcOrdType<>'C' .AND. lfvContPri(Style,'m.Gros_Price','m.Price','m.Disc_Pcnt')

    IF !m.lContract
      m.Gros_Price= lfGetprice(Style,lcPriceLvl,TotQty*&lcPackHdr..PackQty)
      lcDiscCode  = IIF(SEEK(Style+laData[31]+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
      m.Disc_Pcnt = 0
      IF !EMPTY(ALLTRIM(lcDiscCode))
        *-- Get the disecound related filed to now which 
        *-- type whole Sale Or Retail sale Or Both.
        DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
        STORE '' To lcDisType , ldstartDte ,ldEndDate
        *-- Array to get the Discount affect for DecCode.
        laDisType[1,1]  = 'CCOSTAFECT'
        laDisType[1,2]  = 'lcDisType'
        *-- Array to get the start date For DescCode.
        lastartDte[1,1] = 'START'
        lastartDte[1,2] = 'ldstartDte'
        *-- Array to get the end date For DescCode.
        laEndDate[1,1]  = 'DENDATE'
        laEndDate[1,2]  = 'ldEndDate'
        = gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
        = gfRltFld(lcDiscCode, @lastartDte, 'CDISCCODE')
        = gfRltFld(lcDiscCode , @laEndDate, 'CDISCCODE')
  
        IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[8],ldstartDte,ldEndDate)
          lnDisc_Pcnt = 0
          =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
          m.Disc_Pcnt = lnDisc_Pcnt
        ENDIF
      ENDIF 
      m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100
    ENDIF
    m.Gl_Sales = ALLTRIM(laData[53])+Style.cSlsGlLink
    
    IF laSetups[4,2]='Y' AND !SEEK('02'+m.Gl_Sales,'Gl_Link')
      m.Gl_Sales = 'DEFDEF'
    ENDIF
    IF !llStores .AND. llMultiSt
      SELECT (lcTempLine)
      lnTLines = lnTLines + 1
      lnLineNo = lnTLines
    ELSE
      lnLines  = lnLines + 1
      lnLineNo = lnLines
      SELECT (lcOrdLine)
    ENDIF
    
    *C102118,1 SSE Commented out Because they don't use bulk orders [Begin]
    *IF llMultiSt .AND. llFromBulk
    *  lcAlias1 = SELECT()
    *  SELECT (lcTempline)
    *  lcCurOrd = ORDER()
    *  lcCurRec = RECNO()
    *  SET ORDER TO 
    *  lcPackSty = "Spck_Lin.Style"
    *  lcUpdKey = "&lcTempline..Style = " + lcPackSty 
    *  LOCATE FOR &lcUpdKey 
    *  IF FOUND()
    *    m.cFromOrder = &lcTempline..cFromOrder
    *    m.BulkLineNo = &lcTempline..BulkLineNo
    *  ENDIF 
    *  SET ORDER TO &lcCurOrd
    *  IF BETWEEN(lcCurRec ,1,RECCOUNT()) 
    *  	GOTO lcCurRec 
    *  ENDIF
    *  SELECT (lcAlias1)
    *ENDIF  
    *C102118,1 SSE Commented out Because they don't use bulk orders [End]

    APPEND BLANK
    =RLOCK()
    REPLACE cOrdType   WITH lcOrdType ,;
            Order      WITH laData[1] ,; 
            Account    WITH laData[2] ,;
            Store      WITH IIF(llStores,lcStore,laData[3]) ,;
            LineNo     WITH lnLineNo  ,;
            Style      WITH Spck_Lin.Style ,;
            Desc1      WITH Style.Desc1 ,;
            Scale      WITH Style.Scale ,;
            Season     WITH Style.Season ,;
            Gros_Price WITH m.Gros_Price ,;
            Price      WITH m.Price ,;
            Disc_Pcnt  WITH m.Disc_Pcnt ,;
            cFromOrder WITH m.cFromOrder ,; 
            BulkLineNo WITH m.BulkLineNo
    REPLACE Qty1       WITH Spck_Lin.Qty1*&lcPackHdr..PackQty,;
            Qty2       WITH Spck_Lin.Qty2*&lcPackHdr..PackQty,;
            Qty3       WITH Spck_Lin.Qty3*&lcPackHdr..PackQty,;
            Qty4       WITH Spck_Lin.Qty4*&lcPackHdr..PackQty,;
            Qty5       WITH Spck_Lin.Qty5*&lcPackHdr..PackQty,;
            Qty6       WITH Spck_Lin.Qty6*&lcPackHdr..PackQty,;
            Qty7       WITH Spck_Lin.Qty7*&lcPackHdr..PackQty,;
            Qty8       WITH Spck_Lin.Qty8*&lcPackHdr..PackQty,;
            TotQty     WITH Spck_Lin.TotQty*&lcPackHdr..PackQty ,;
            Comm1      WITH laData[28] ,;
            Comm2      WITH laData[30] ,;
            cWareCode  WITH laData[31] ,;
            Gl_Sales   WITH m.Gl_Sales ,;
            Pack_Id    WITH &lcPackHdr..Pack_Id ,;
            lContract  WITH m.lContract
    IF !llStores .AND. llMultiSt
      lnTotPcs = lnTotPcs + TotQty
      lnTotAmt = lnTotAmt + TotQty*Price
    ELSE
      REPLACE Flag    WITH 'N' ,;
              Book1   WITH Spck_Lin.Qty1*&lcPackHdr..PackQty,;
              Book2   WITH Spck_Lin.Qty2*&lcPackHdr..PackQty,;
              Book3   WITH Spck_Lin.Qty3*&lcPackHdr..PackQty,;
              Book4   WITH Spck_Lin.Qty4*&lcPackHdr..PackQty,;
              Book5   WITH Spck_Lin.Qty5*&lcPackHdr..PackQty,;
              Book6   WITH Spck_Lin.Qty6*&lcPackHdr..PackQty,;
              Book7   WITH Spck_Lin.Qty7*&lcPackHdr..PackQty,;
              Book8   WITH Spck_Lin.Qty8*&lcPackHdr..PackQty,;
              TotBook WITH Spck_Lin.TotQty*&lcPackHdr..PackQty
      laData[35] = laData[35] + TotQty
      laData[36] = laData[36] + TotQty*Price
      laData[41] = laData[41] + TotQty
      laData[42] = laData[42] + TotQty*Price
    ENDIF
    UNLOCK
  ENDSCAN
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE BOOK    WITH laData[35] ,;
          BOOKAMT WITH laData[36] ,;
          OPEN    WITH laData[41] ,;
          OPENAMT WITH laData[42]
  UNLOCK
ENDSCAN
WAIT CLEAR
CLEAR READ
*-- End of lfvPckApply.

*!*******************************************************************************
*! Name      : lfValidPack
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Validate packs before add to the order lines
*!*******************************************************************************
*! Calls     : gfModalGen
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfValidPack()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfValidPack
PRIVATE llValidPack,llOnHoldAll,llAssignAll

llValidPack = .T.
STORE .F. TO llOnHoldAll,llAssignAll
SELECT Spck_Lin
SCAN REST WHILE Type+Account+Pack_Id=&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id

  IF !SEEK('S'+Style.Scale,'Scale')
    *-- Message : 32039
    *-- Pack xxx contains one or more xxx. xxx
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32039B00000','ALERT',&lcPackHdr..Pack_Id+'|'+'style with non existing scale'+'|'+'Pack will be ignored.')
    llValidPack = .F.
    EXIT
  ENDIF  
  IF Style.Status = 'X'
    *-- Message : 32039
    *-- Pack xxx contains one or more xxx. xxx
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32039B00000','ALERT',&lcPackHdr..Pack_Id+'|'+'canceled style'+'|'+'Pack will be ignored.')
    llValidPack = .F.
    EXIT
  ENDIF
  IF !llOnHoldAll .AND. Style.Status = 'H'
    *-- Message : 32040
    *-- Pack xxx contains one or more on-hold style/color.
    *-- Button : 32005
    *-- Proceed Cancel
    IF gfModalGen('QRM32040B32005','ALERT',&lcPackHdr..Pack_Id)=1
      llOnHoldAll = .T.
    ELSE
      llValidPack= .F.
      EXIT
    ENDIF
  ENDIF
  IF laSetups[5,2]='Y' .AND. !llAssignAll .AND. ;
     !SEEK(Style+laData[31]+SPACE(10),'StyDye')
    *-- Message : 32041
    *-- Pack xxx contains one or more style/color that is not assign
    *-- to warehouse xxx. Assign all styles/colors to this warehouse?
    *-- Button : 32000
    *-- Yes No
    IF gfModalGen('QRM32041B32000','ALERT',&lcPackHdr..Pack_Id+'|'+laData[31])=1
      llAssignAll = .T.
    ELSE
      llValidPack = .F.
      EXIT
    ENDIF
  ENDIF   
ENDSCAN
=SEEK(&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id)
RETURN(llValidPack)
*-- End of lfValidPack.

*!*******************************************************************************
*! Name      : lfvPStore
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Order store
*!*******************************************************************************
*! Calls     : CUSBROW,gfModalGen
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  .f.
*!*******************************************************************************
*! Example   :  =lfvPStore()
*!*******************************************************************************
*
FUNCTION lfvPStore
PARAMETERS xStore

IF llBrowse .OR. (!EMPTY(xStore) .AND. ;
                  !SEEK('S'+laData[2]+xStore,'CUSTOMER'))
  IF !CUSBROWS(laData[2],.T.)
    llBrowse = .F.
    STORE SPACE(8) TO xStore
    RETURN
  ENDIF
ENDIF  
llBrowse = .F.
IF !EMPTY(xStore) .AND. Customer.Status <> 'A' 
  *-- Message : 32023
  *-- This a non-active xxxxx. Order entry is not allowed!
  *-- Button : 00000
  *-- Ok
  =gfModalGen('TRM32023B00000','ALERT','store'+'|'+IIF(lcOrdType='C','Contract','Order'))
  STORE SPACE(8) TO xStore
  RETURN
ENDIF
*-- End of lfvPStore.

*!*******************************************************************************
*! Name      : lpSavScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Save new or modified order
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lpSavScr()
*!*******************************************************************************
*
FUNCTION lpSavScr
*C102118,5 not releasing screen when presssing Ctrl+S [Begin]
*llCtrlSKey = .F.
IF LASTKEY() = 19
  =lfChkTotQty(.F.)
*  llCtrlSKey = .T.
ENDIF
*C102118,5 not releasing screen when presssing Ctrl+S [End]

DO lfSavScr IN (gcAppHome + 'SO\SOSOCUPD.FXP')
*-- End of lpSavScr.

*!*******************************************************************************
*! Name      : lfGtOrder
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Read order number manually
*!*******************************************************************************
*! Calls     : ARORDER.SPX
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  Order#
*!*******************************************************************************
*! Example            :  =lfGtOrder()
*!*******************************************************************************
*
FUNCTION lfGtOrder
PRIVATE lcOrderNo

STORE SPACE(6) TO lcOrderNo
DO (gcScrDir+"ARORDER.SPX") WITH 'O','lcOrderNo',lcOrdType

RETURN(lcOrderNo)
*-- End of lfGtOrder.

*!*******************************************************************************
*! Name      : lfPrepBrow
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Prepare files and relations according to current selected 
*!             Transaction, sort, total or per size qty and details or summary 
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfPrepBrow()
*!*******************************************************************************
*
FUNCTION lfPrepBrow
WAIT 'Colecting data...Please standby.' WINDOW NOWAIT

*C102118,1 SSE Commented out all these Cases not used anymore. [Begin]
lcForKey = '.T.'
*DO CASE
*  *-- Show Shipped Transactions
*  CASE lnOrdTrans= 3
*    =gfOpenFile(gcDataDir+'InvHdr',gcDataDir+'InvHdr','SH')
*    lcForKey = "INVHDR.STATUS <> 'V'"
*    =gfOpenFile(gcDataDir+'InvLine',gcDataDir+'InvLine','SH')
*    =gfOpenFile(gcDataDir+'consinvl',gcDataDir+'CINVLINE','SH')
*  *-- Show Production Transactions
*  CASE lnOrdTrans= 4
*    IF 'PO' $ gcCmpModules
*      =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
*      =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
*      SET RELATION TO CSTYTYPE+PO INTO PosHdr
*    ENDIF
*    IF 'MF' $ gcCmpModules
*      =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
*      =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTL','SH')
*      SET RELATION TO CutTkt INTO CutTktH
*    ENDIF
*    =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
*    IF 'PO' $ gcCmpModules
*      SET RELATION TO 'P'+cTktNo+Style INTO PoSLn
*    ENDIF
*    IF 'MF' $ gcCmpModules
*      SET RELATION TO cTktNo+Style INTO CutTktL ADDITIVE
*    ENDIF
*  *-- Show Cancelled Transactions
*  CASE lnOrdTrans= 7
*    =gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH')
*  *-- Show Work Order
*  CASE lnOrdTrans= 8
*    =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
*ENDCASE
*DO CASE
*  *-- Sort by line# - Total quantity
*  CASE lnSortBy = 1 .AND. !llQtyPSize
*    lcOrderFile = IIF(laScrMode[2] .OR. laScrMode[1],'ORDLINE',lcOrdLine)
*    SET ORDER TO TAG 'ORDLINE' IN (lcOrderFile)
*    lcOrderBr = [STYLE :R :H=lcStyHdr,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
*                IIF(llMultiSt,[STORE :R ,],'') +;
*                IIF(laData[7],IIF(laScrMode[2],[CustPo  :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
*                [GROUP :H='Group' :R,Gros_Price :H='Gross Price' :R,] +;
*                [Disc_Pcnt :H='Discount' :R,PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):R:P='999999999.99' :R,] +;
*                [TOTQTY :H= 'Qty.' :P='999999':R ,] +;
*                [lnAmount=PRICE*TOTQTY :H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99',] +;
*                IIF(ORDHDR.cOrdType='T' .AND. ORDHDR.MON_FLG='G' ,;
*                "Status=lfEDIStat() :H='Status' :R :18",;
*                [PIKTKT :H='Piktkt' :R ,] +;
*                [TOTPIK :H='Picked' :R ,] +;
*                [PIKDATE :H='Date' :R])                
*    DO CASE
*      *-- Open quantity
*      CASE lnOrdTrans= 1
*        SELECT (lcOrderFile)
*        IF !laScrmode[1]
*          SET RELATION TO Style INTO Style
*          SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE
*        ENDIF
*      *-- Booked quantity
*      CASE lnOrdTrans= 2
*        IF !USED(lcBooked)
*          =gfOpenFile(gcDataDir+'ORDLINE','','SH',@lcBooked,.T.)
*        ENDIF
*        lcBrTtlT   = lcBrTtlB
*        lcTranFile = lcBooked
*        SET ORDER TO TAG 'ORDLINE' IN (lcBooked)
*        SELECT (lcOrderFile)
*        SET RELATION TO cOrdType+ORDER+STR(LINENO,6) INTO (lcBooked)
*        lcTranBr = [STYLE :R ,STORE :R ,] +;
*                   [GROUP :H='Grp' :R ,PRICE :R ,TOTBOOK :H='Qty.' :R ,] +;
*                   [lnAmount=PRICE*TOTBOOK :H='Amount':R:P='99999999999.99']
*      *-- Shipped quantity
*      CASE lnOrdTrans= 3
*        lcBrTtlT = lcBrTtlS
*        SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
*        IF SEEK(laData[1],'CONSINVL')
*          lcTranFile = 'CONSINVL'
*        ELSE
*          lcTranFile = 'INVLINE'
*          SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
*        ENDIF
*        SELECT (lcTranFile)
*        SET RELATION TO INVOICE INTO INVHDR
*        SELECT (lcOrderFile)
*        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcTranFile)
*        lcTranBr = [INVOICE:H='Invoice#' :R ,INVDATE:H='Date' :R,] +;
*                   [STORE :H='Store' :R,TOTQTY :H='Quantity' :R,PRICE :H='Price' :R,] +;
*                   [lnAmount=PRICE*TOTQTY :H='Amount':R:P='99999999999.99']
*      *-- Production quantity
*      CASE lnOrdTrans= 4
*        SELECT (lcOrderFile)
*        SET RELATION TO Style INTO STYLE
*        SET RELATION TO IIF(Style.Make,"1","2")+Order+STR(LineNo,6) INTO CutPick ADDITIVE
*        =SEEK(lcOrdType+laData[1])
*        lcBrTtlT  = lcBrTtlP
*        lcTranFile = 'CutPick'
*        lcTranBr1 = [CutPick.cTktNo :H='Tran #' :R ,] +;
*                    [Type1='C/T' :H='Type' :R ,] +;
*                    [CutTktH.ENTERED :R,CutTktH.Complete :R,] +;
*                    [CutPick.TotQty :H='Qty. Alo.' :R ,] +;
*                    [CutTktL.TotQty :H='Qty. Ord.' :R ,] +;
*                    [lnTotQty=CutTktL.TotQty-CutTktL.TotOrd :H='Qty. Opn.':R] 
*        lcTranBr2 = [CutPick.cTktNo :H='Tran #' :R ,] +;
*                    [Type2='P/O' :H='Type' :R ,] +;
*                    [PosHdr.ENTERED :R,PosHdr.Complete :R,] +;
*                    [PosHdr.Vendor  :H='Vendor' :R,] +;
*                    [CutPick.TotQty :H='Qty. Alo.' :9:R,] +;
*                    [PosLn.TotQty   :H='Qty. Ord.' :9:R ,] +;
*                    [lnTotQty=PosLn.TotQty-PosLn.TotOrd   :H='Qty. Opn.' :9:R]
*        lcTranBr=IIF(Style.Make,lcTranBr1,lcTranBr2)
*        llDomestic = Style.Make
*      *-- Depleted orders
*      CASE lnOrdTrans= 5
*        IF !USED(lcDepleted) .OR. !SEEK(laData[1],lcDepleted)
*          SET ORDER TO TAG ORDACCT IN ORDHDR
*          SET ORDER TO TAG ORDLINE IN ORDLINE
*          SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
*          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
*          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
*          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
*          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
*          FROM ORDHDR,ORDLINE;
*          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=laData[2]+lcOrdType AND ;
*          ORDLINE.cFromOrder=laData[1] AND ;
*          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
*          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)
*          INDEX ON CFROMORDER+STR(BULKLINENO,6) TAG (lcDepleted)
*          SET ORDER TO TAG ORDHDR IN ORDHDR          
*        ENDIF
*        lcBrTtlT   = lcBrTtlD
*        lcTranFile = lcDepleted
*        SELECT (lcOrderFile)
*        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcDepleted)
*        lcTranBr = [ORDER :R ,STYLE :R ,STORE :R ,] +;
*                   [GROUP :H='Grp' :R ,PRICE :R ,TOTBOOK :H= 'Qty.' :R ,] +;
*                   [lnAmount=PRICE*TOTBOOK :10:H='Amount':R:P='99999999999.99',] +;
*                   [PIKTKT :H='Piktkt' :6:R ,PIKDATE :H='Date' :8:R]
*      *-- Bulk order details
*      CASE lnOrdTrans= 6
*        lcBrTtlT   = lcBrTtlK
*        lcTranFile = lcBulkOrd
*        SELECT (lcOrderFile)
*        SET RELATION TO STR(BulkLineNo,6) INTO (lcBulkOrd)
*        lcTranBr = [STYLE :R ,TotBook :H= 'Tot. Book' :10 :R ,] +;
*                   [TotOpen :H= 'Tot. Open' :10 :R ,TotUsed :H= 'Tot. Used' :10 :R ,] +;
*                   [Balance = MAX(TotOpen-TotUsed,0) :H= 'Balance' :10:R]
*      CASE lnOrdTrans= 7
*        lcBrTtlT   = lcBrTtlC
*        lcTranFile = IIF(laScrMode[2],'ORDCANLN',lcOrdCanLn)
*        SELECT (lcOrderFile)
*        SET RELATION TO cOrdType+Order+STR(LineNo,6) INTO (lcTranFile)
*        lcTranBr = [Cancelled :R :H='Date',TotQty :H= 'Total Canceld' :R,]+;
*                   [cDesc=gfCodDes(cCancReson,'CCANCRESON') :R :H='Cancelation Reason']
*      CASE lnOrdTrans = 8
*        lcBrTtlT   = lcBrTtlW
*        lcTranFile = "CutPick"
*        lcTranBr = [lnDummy=IIF(TranCd='1','CT #',IIF(TranCd='2','PO #','Adornment')) :R:10:H='Tran. Type',]+;
*                   [cTktNo:R:H='Tran. #',Order :R:H='Order #', Style:R:20, Qty1:R, Qty2:R,]+;
*                   [Qty3:R, Qty4:R, Qty5:R, Qty6:R, Qty7:R, Qty8:R, TotQty:R]
*    ENDCASE
*  *-- Sort by line# - Quantity per size
*  CASE lnSortBy = 1 .AND. llQtyPSize
*    lcOrderFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
*    SET ORDER TO TAG 'ORDLINE' IN (lcOrderFile)
*    lcOrderBr = [STYLE :H=lcStyHdr :R,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +IIF(llMultiSt,[STORE :8 :R,],'') +;
*                IIF(laData[7],IIF(lcScrMode='V',[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
*                [TOTQTY :P='999999' :R, lnAmount=PRICE*TOTQTY :10:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='9999999999.99',TOTPIK :H='Tot.Pik.' :R ,]+;
*                [Qty1 :P='99999':R,Qty2 :P='99999':R,Qty3 :P='99999':R,Qty4 :P='99999':R,]+;
*                [Qty5 :P='99999':R,Qty6 :P='99999':R,Qty7 :P='99999':R,Qty8 :P='99999':R,]+;
*                [PIK1 :P='99999':R,PIK2 :P='99999':R,PIK3 :P='99999':R,PIK4 :P='99999':R,]+;
*                [PIK5 :P='99999':R,PIK6 :P='99999':R,PIK7 :P='99999':R,PIK8 :P='99999':R,TOTPIK :H='Tot.Pik.']+;
*                IIF(laSetups[2,2]='Y',",n=laData[27] :H='Rep 1':R ,COMM1 :H='Comm 1' :R,n1=laData[29] :H='Rep 2':R ,COMM2 :H='Comm 2' :R",'')+;
*                IIF(laData[7],",CUSTPO :H='Cust. PO#':R",'')
*    DO CASE
*      *-- Open quantity
*      CASE lnOrdTrans= 1
*        SELECT (lcOrderFile)
*        SET RELATION TO Style INTO Style
*        SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE
*      *-- Booked quantity
*      CASE lnOrdTrans= 2
*        IF !USED(lcBooked)
*          =gfOpenFile(gcDataDir+'ORDLINE','','SH',@lcBooked,.T.)
*        ENDIF
*        lcBrTtlT   = lcBrTtlB
*        lcTranFile = lcBooked
*        SET ORDER TO TAG 'ORDLINE' IN (lcBooked)
*        SELECT (lcOrderFile)
*        SET RELATION TO cOrdType+ORDER+STR(LINENO,6) INTO (lcBooked)
*        lcTranBr = [STYLE :R,STORE :R,BOOK1 :R,BOOK2 :R,BOOK3 :R,] +;
*                   [BOOK4 :R,BOOK5 :R,BOOK6 :R,BOOK7 :R,BOOK8 :R,TOTBOOK :6:R ,] +;
*                   [lnAmount=PRICE*TOTBOOK :9:H='Amount':R:P='99999999999.99']+;
*                   IIF(laSetups[2,2]='Y', ;
*                   ",n=laData[27] :H='Rep 1':5:R ,COMM1 :H='Comm 1' :6:R ,"+;
*                   "n1=laData[29]:H='Rep 2':5:R ,COMM2 :H='Comm 2' :6:R",'')+;
*                   IIF(laData[7],[,CUSTPO :15:H='Cust P/O':R],'')
*      *-- Shipped quantity
*      CASE lnOrdTrans= 3
*        lcBrTtlT   = lcBrTtlS
*        SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
*        IF SEEK(laData[1],'CONSINVL')
*          lcTranFile = 'CONSINVL'
*        ELSE
*          lcTranFile = 'INVLINE'
*          SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
*        ENDIF
*        SELECT (lcOrderFile)
*        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcTranFile)
*        lcTranBr = [INVOICE:H='Invoice#' :R ,INVDATE:H='Date' :R,STORE :R ,] +;
*                   [TOTQTY :H='Qty.' :P='9999999':R ,PRICE :R ,] +;
*                   [lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='99999999999.99',]+; 
*                   [Qty1 :6 :R,Qty2 :6 :R,Qty3 :6 :R,Qty4 :6 :R,] +;
*                   [Qty5 :6 :R,Qty6 :6 :R,Qty7 :6 :R,Qty8 :6 :R]
*      *-- Production quantity
*      CASE lnOrdTrans= 4
*        SELECT (lcOrderFile)
*        SET RELATION TO Style INTO STYLE
*        SET RELATION TO IIF(Style.Make,"1","2")+Order+STR(LineNo,6) INTO CutPick ADDITIVE
*        =SEEK(lcOrdType+laData[1])
*        lcBrTtlT   = lcBrTtlP
*        lcTranFile = 'CutPick'
*        lcTranBr1= [CutPick.cTktNo :H='Tran #' :R ,] +;
*                   [Type1='C/T' :H='Type' :R ,] +;
*                   [CutTktH.ENTERED  :R,CutTktH.Complete :R,] +;
*                   [CutPick.Qty1 :H='Size1':6:R ,] +;
*                   [CutPick.Qty2 :H='Size2':6:R ,] +;
*                   [CutPick.Qty3 :H='Size3':6:R ,] +;
*                   [CutPick.Qty4 :H='Size4':6:R ,] +;
*                   [CutPick.Qty5 :H='Size5':6:R ,] +;
*                   [CutPick.Qty6 :H='Size6':6:R ,] +;
*                   [CutPick.Qty7 :H='Size7':6:R ,] +;
*                   [CutPick.Qty8 :H='Size8':6:R ,]
*        lcTranBr1=lcTranBr1+;
*                   [CutPick.TotQty :H='Qty. Alo.' :9:R ,] +;
*                   [CutTktL.Qty1 :H='Size1':6:R ,] +;
*                   [CutTktL.Qty2 :H='Size2':6:R ,] +;
*                   [CutTktL.Qty3 :H='Size3':6:R ,] +;
*                   [CutTktL.Qty4 :H='Size4':6:R ,] +;
*                   [CutTktL.Qty5 :H='Size5':6:R ,] +;
*                   [CutTktL.Qty6 :H='Size6':6:R ,] +;
*                   [CutTktL.Qty7 :H='Size7':6:R ,] +;
*                   [CutTktL.Qty8 :H='Size8':6:R ,] +;
*                   [CutTktL.TotQty   :H='Qty. Ord.' :9:R ,] +;
*                   [lnTotQty=CutTktL.TotQty-CutTktL.TotOrd :9:H='Qty. Opn.':R] 
*        lcTranBr2= [CutPick.cTktNo :H='Tran #' :R ,] +;
*                   [Type2='P/O' :H='Type' :R ,] +;
*                   [PosHdr.ENTERED :R,PosHdr.Complete :R,] +;
*                   [PosHdr.Vendor :H='Vendor ' :R ,] +;
*                   [CutPick.Qty1 :H='Size1':6:R ,] +;
*                   [CutPick.Qty2 :H='Size2':6:R ,] +;
*                   [CutPick.Qty3 :H='Size3':6:R ,] +;
*                   [CutPick.Qty4 :H='Size4':6:R ,] +;
*                   [CutPick.Qty5 :H='Size5':6:R ,] +;
*                   [CutPick.Qty6 :H='Size6':6:R ,] +;
*                   [CutPick.Qty7 :H='Size7':6:R ,] +;
*                   [CutPick.Qty8 :H='Size8':6:R ,]
*        lcTranBr2=lcTranBr2+;
*                   [CutPick.TotQty  :H='Qty. Alo.' :9:R,] +;
*                   [PosLn.Qty1 :H='Size1':6:R ,] +;
*                   [PosLn.Qty2 :H='Size2':6:R ,] +;
*                   [PosLn.Qty3 :H='Size3':6:R ,] +;
*                   [PosLn.Qty4 :H='Size4':6:R ,] +;
*                   [PosLn.Qty5 :H='Size5':6:R ,] +;
*                   [PosLn.Qty6 :H='Size6':6:R ,] +;
*                   [PosLn.Qty7 :H='Size7':6:R ,] +;
*                   [PosLn.Qty8 :H='Size8':6:R ,] +;
*                   [PosLn.TotQty :H='Qty. Ord.' :9:R ,] +;
*                   [lnTotQty=PosLn.TotQty-PosLn.TotOrd :H='Qty. Opn.' :9:R]
*        lcTranBr=IIF(Style.Make,lcTranBr1,lcTranBr2)
*        llDomestic = Style.Make      
*      *-- Depleted orders
*      CASE lnOrdTrans= 5
*        IF !USED(lcDepleted) .OR. !SEEK(laData[1],lcDepleted)
*          SET ORDER TO TAG ORDACCT IN ORDHDR
*          SET ORDER TO TAG ORDLINE IN ORDLINE
*          SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
*          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
*          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
*          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
*          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
*          FROM ORDHDR,ORDLINE;
*          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=lcOrdType+laData[2] AND ;
*          ORDLINE.cFromOrder=laData[1] AND ;
*          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
*          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)
*          INDEX ON CFROMORDER+STR(BULKLINENO,6) TAG (lcDepleted)
*          SET ORDER TO TAG ORDHDR IN ORDHDR
*        ENDIF
*        lcBrTtlT   = lcBrTtlD
*        lcTranFile = lcDepleted
*        SELECT (lcOrderFile)
*        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcDepleted)
*        lcTranBr = [ORDER :R ,] +;
*                   [STYLE ,STORE :R,BOOK1 :R,BOOK2 :R,BOOK3 :R,] +;
*                   [BOOK4 :R,BOOK5 :R,BOOK6 :R,BOOK7 :R,BOOK8 :R,TOTBOOK :R ,] +;
*                   [lnAmount=PRICE*TOTBOOK :10:H='Amount':R:P='99999999999.99',] +;
*                   IIF(laSetups[2,2]='Y', ;
*                   "n=laData[27] :H='Rep 1' :R ,COMM1 :H='Comm 1' :R ," +;
*                   "n1=laData[29] :H='Rep 2':R ,COMM2 :H='Comm 2' :R ",'') +;
*                   IIF(laData[7],[,CUSTPO :H='Cust P/O':R],'')
*      *-- Bulk order details
*      CASE lnOrdTrans= 6
*        lcBrTtlT   = lcBrTtlK
*        lcTranFile = lcBulkOrd
*        SELECT (lcOrderFile)
*        SET RELATION TO STR(BulkLineNo,6) INTO (lcBulkOrd)
*        lcTranBr = [STYLE :19:R,] +;
*                   [Book1 :5 :R,Book2 :5 :R,Book3 :5 :R,Book4 :5 :R,] +;
*                   [Book5 :5 :R,Book6 :5 :R,Book7 :5 :R,Book8 :5 :R ,] +;
*                   [TotBook :H= 'Tot. Book' :10 :R ,] +;
*                   [Open1 :5 :R,Open2 :5 :R,Open3 :5 :R,Open4 :5 :R,] +;
*                   [Open5 :5 :R,Open6 :5 :R,Open7 :5 :R,Open8 :5 :R ,] +;
*                   [TotOpen :H= 'Tot. Open' :10 :R ,] +;
*                   [Used1 :5 :R,Used2 :5 :R,Used3 :5 :R,Used4 :5 :R,] +;
*                   [Book5 :5 :R,Used6 :5 :R,Used7 :5 :R,Used8 :5 :R ,] +;
*                   [TotUsed :H= 'Tot. Used' :10 :R ,] +;
*                   [Bal1=MAX(Open1-Used1,0) :H='Bal1' :5:R,]+;
*                   [Bal2=MAX(Open2-Used2,0) :H='Bal2' :5:R,]+;
*                   [Bal3=MAX(Open3-Used3,0) :H='Bal3' :5:R,]+;
*                   [Bal4=MAX(Open4-Used4,0) :H='Bal4' :5:R,]+;
*                   [Bal5=MAX(Open5-Used5,0) :H='Bal5' :5:R,]+;
*                   [Bal6=MAX(Open6-Used6,0) :H='Bal6' :5:R,]+;
*                   [Bal7=MAX(Open7-Used7,0) :H='Bal7' :5:R,]+;
*                   [Bal8=MAX(Open8-Used8,0) :H='Bal8' :5:R,]+;
*                   [Balance = MAX(TotOpen-TotUsed,0) :H= 'Balance' :10:R]
*      CASE lnOrdTrans= 7
*        lcBrTtlT   = lcBrTtlC
*        lcTranFile = IIF(laScrMode[2],'ORDCANLN',lcOrdCanLn)
*        SELECT (lcOrderFile)
*        SET RELATION TO cOrdType+Order+STR(LineNo,6) INTO (lcTranFile)
*        lcTranBr = [Cancelled :R :H='Date',TotQty :H= 'Tot. Can.' :R,]+;
*                   [cDesc=gfCodDes(cCancReson,'CCANCRESON') :R :H='Cancelation Reason',]+;
*                   [Qty1 :R :H='Can1',Qty2 :R :H='Can2',Qty3 :R :H='Can3',Qty4 :R :H='Can4',]+;
*                   [Qty5 :R :H='Can5',Qty6 :R :H='Can6',Qty7 :R :H='Can7',Qty8 :R :H='Can8']
*      CASE lnOrdTrans = 8
*        lcBrTtlT   = lcBrTtlW
*        lcTranFile = "CutPick"
*        lcTranBr   = [lnDummy=IIF(TranCd='1','CT #',IIF(TranCd='2','PO #','Adornment')) :R:10:H='Tran. Type',]+;
*                     [cTktNo:R:H='Tran. #',Order :R:H='Order #', Style:R:20, Qty1:R, Qty2:R,]+;
*                     [Qty3:R, Qty4:R, Qty5:R, Qty6:R, Qty7:R, Qty8:R, TotQty:R]
*    ENDCASE
*  *-- Sort by style/color - Total quantity - Details
*  CASE lnSortBy = 2 .AND. !llQtyPSize .AND. llDetails
*    =IIF(!USED(lcOrdLine),lfCratTemp(),.T.)
*    IF !SEEK(lcOrdType+laData[1],lcOrdLine)
*      SELECT (lcOrdLine)
*      DELETE ALL
*      SELECT ORDLINE
*      SET ORDER TO TAG ORDLINE
*      =SEEK(lcOrdType+laData[1])
*      SCAN REST WHILE cordtype+order+STR(lineno,6) = lcOrdType+laData[1]
*        SCATTER MEMVAR MEMO
*        INSERT INTO (lcOrdLine) FROM MEMVAR
*      ENDSCAN
*    ENDIF  
*    lcOrderFile = (lcOrdLine)
*    SET ORDER TO TAG ORDLINES IN (lcOrdLine)
*    lcOrderBr = [STYLE :H=lcStyHdr :R,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
*                [STORE :H='Store' :R,]+;
*                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
*                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'),TOTQTY :H='Quantity' :R,] +;
*                [lnAmount=PRICE*TOTQTY :10:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99',]+;
*                [TOTPIK :H='Tot.Pik.' :R]
*    DO CASE
*      *-- Open quantity
*      CASE lnOrdTrans= 1
*        SELECT (lcOrderFile)
*        SET RELATION TO Style INTO Style
*        SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE
*      *-- Booked quantity
*      CASE lnOrdTrans= 2
*        IF !USED(lcBooked)
*          =gfOpenFile(gcDataDir+'ORDLINE','','SH',@lcBooked,.T.)
*        ENDIF
*        lcBrTtlT   = lcBrTtlB
*        lcTranFile = (lcBooked)
*        SET ORDER TO TAG 'ORDLINE' IN (lcBooked)
*        SELECT (lcOrderFile)
*        SET RELATION TO cOrdType+ORDER+STR(LINENO,6) INTO (lcBooked)
*        lcTranBr = [STYLE :H=lcStyHdr :12:R ,] +;
*                   [STORE :H='Store' :8:R ,PRICE :H='Price' :9:R ,] +;
*                   [TotBook :H='Quantity' :10:R  ,] +;
*                   [lnAmount=PRICE*TotBook :10:H='Amount':R:P='99999999999.99']
*      *-- Shipped quantity
*      CASE lnOrdTrans= 3
*        lcBrTtlT = lcBrTtlS
*        SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
*        IF SEEK(laData[1],'CONSINVL')
*          lcTranFile = 'CONSINVL'
*        ELSE
*          lcTranFile = 'INVLINE'
*          SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
*        ENDIF
*        SELECT (lcOrderFile)
*        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcTranFile)
*        lcTranBr = [STYLE :H=lcStyHdr :R,STORE :H='Store' :R,PRICE :H='Price' :R,] +;
*                   [TOTQTY :H='Quantity' :R,] +;
*                   [lnAmount=PRICE*TOTQTY :10:H='Amount':R:P='99999999999.99',]+;
*                   [INVOICE:H='Invoice' :R ,INVDATE:H='Date' :R]
*      *-- Production quantity
*      CASE lnOrdTrans= 4
*        SELECT (lcOrderFile)
*        SET RELATION TO Style INTO STYLE
*        SET RELATION TO IIF(Style.Make,"1","2")+Order+STR(LineNo,6) INTO CutPick ADDITIVE
*        =SEEK(lcOrdType+laData[1])
*        lcBrTtlT = lcBrTtlP
*        lcTranFile = 'CutPick'
*        lcTranBr1 = [CutPick.cTktNo :H='Tran #' :R ,] +;
*                    [Type1='C/T' :H='Type' :R ,] +;
*                    [CutTktH.ENTERED :R ,CutTktH.Complete :R,] +;
*                    [CutPick.TotQty :H='Qty. Alo.' :9:R ,] +;
*                    [CutTktL.TotQty :H='Qty. Ord.' :9:R ,] +;
*                    [lnTotQty=CutTktL.TotQty-CutTktL.TotOrd :9:H='Qty. Opn.':R] 
*         lcTranBr2= [CutPick.cTktNo :H='Tran #' :R ,] +;
*                    [Type2='P/O' :H='Type' :R ,] +;
*                    [PosHdr.ENTERED :R,PosHdr.Complete :R,] +;
*                    [PosHdr.Vendor :H='Vendor ' :R ,] +;
*                    [CutPick.TotQty  :H='Qty. Alo.' :9:R,] +;
*                    [PosLn.TotQty   :H='Qty. Ord.' :9:R ,] +;
*                    [lnTotQty=PosLn.TotQty-PosLn.TotOrd   :H='Qty. Opn.' :9:R]
*        lcTranBr=IIF(Style.Make,lcTranBr1,lcTranBr2)
*        llDomestic = Style.Make
*      *-- Depleted orders
*      CASE lnOrdTrans= 5
*        IF !USED(lcDepleted) .OR. !SEEK(laData[1],lcDepleted)
*          SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
*          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
*          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
*          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
*          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
*          FROM ORDHDR,ORDLINE;
*          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=laData[2]+lcOrdType AND ;
*          ORDLINE.cFromOrder=laData[1] AND ;
*          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
*          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)
*          INDEX ON CFROMORDER+STR(BULKLINENO,6) TAG (lcDepleted)
*        ENDIF
*        lcBrTtlT   = lcBrTtlD
*        lcTranFile = lcDepleted
*        SELECT (lcOrderFile)
*        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcDepleted)
*        lcTranBr = [ORDER :R ,STYLE :R ,STORE :R ,] +;
*                   [GROUP :H='Grp' :R ,PRICE :R ,TOTBOOK :H= 'Qty.' :R ,] +;
*                   [lnAmount=PRICE*TOTBOOK :10:H='Amount':R:P='99999999999.99',] +;
*                   [PIKTKT :H='Piktkt' :6:R ,PIKDATE :H='Date' :8:R]
*      CASE lnOrdTrans= 7
*        lcBrTtlT   = lcBrTtlC
*        lcTranFile = IIF(laScrMode[2],'ORDCANLN',lcOrdCanLn)
*        SELECT (lcOrderFile)
*        SET RELATION TO cOrdType+Order+STR(LineNo,6) INTO (lcTranFile)
*        lcTranBr = [Cancelled :R :H='Date',TotQty :H= 'Total Canceld' :R,]+;
*                   [cDesc=gfCodDes(cCancReson,'CCANCRESON') :R :H='Cancelation Reason']
*      CASE lnOrdTrans = 8
*        lcBrTtlT   = lcBrTtlW
*        lcTranFile = "CutPick"
*        lcTranBr   = [lnDummy=IIF(TranCd='1','CT #',IIF(TranCd='2','PO #','Adornment')) :R:10:H='Tran. Type',]+;
*                     [cTktNo:R:H='Tran. #',Order :R:H='Order #', Style:R:20, Qty1:R, Qty2:R,]+;
*                     [Qty3:R, Qty4:R, Qty5:R, Qty6:R, Qty7:R, Qty8:R, TotQty:R]
*    ENDCASE
*  *-- Sort by style - Total quantity - Summary
*  CASE lnSortBy = 2 .AND. !llQtyPSize .AND. !llDetails
*    IF !USED(lcOrdSS) .OR. !SEEK(lcOrdType+laData[1],lcOrdSS)
*      lcFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
*      SET ORDER TO TAG ORDLINE IN (lcFile)
*      SELECT cOrdType,&lcFile..Order,Style,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
*      SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
*      SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount, ;
*      SUM(PIK1) AS nPIK1,SUM(PIK2) AS nPIK2,SUM(PIK3) AS nPIK3,SUM(PIK4) AS nPIK4,;
*      SUM(PIK5) AS nPIK5,SUM(PIK6) AS nPIK6,SUM(PIK7) AS nPIK7,SUM(PIK8) AS nPIK8,;
*      SUM(TOTPIK) AS nTotPIK;
*      FROM (lcFile) WHERE cOrdType+&lcFile..ORDER+STR(LINENO,6)=;
*      lcOrdType+laData[1] GROUP BY &lcFile..Order,Style INTO CURSOR (lcOrdSS)
*      INDEX ON cOrdType+ORDER+STYLE TAG (lcOrdSS)
*    ENDIF
*    SELECT (lcOrdSS)
*    SET RELATION TO STYLE INTO STYLE
*    lcOrderFile = lcOrdSS
*    lcOrderBr = [STYLE  :H=lcStyHdr :R ,] +;
*                [Style.Desc :H= 'Description', nTOTQTY :H='Quantity' :R:P='9999999999',]+;
*                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):14:R:P='99999999999.99',nTOTPIK :H='Tot.Pik.']
*    DO CASE
*      *-- Booked quantity
*      CASE lnOrdTrans= 2
*        IF !USED(lcBookSS) .OR. !SEEK(laData[1],lcBookSS)
*          SET ORDER TO TAG ORDLINE IN IIF(laScrMode[2],'ORDLINE',lcOrdLine)
*          SELECT Style,SUM(Book1) AS nQty1,SUM(Book2) AS nQty2,;
*          SUM(Book3) AS nQty3,SUM(Book4) AS nQty4,SUM(Book5) AS nQty5,SUM(Book6) AS nQty6,;
*          SUM(Book7) AS nQty7,SUM(Book8) AS nQty8,SUM(TOTBook) AS nTotQTy,SUM(TOTBook*PRICE) AS nAmount ;
*          FROM IIF(laScrMode[2],'ORDLINE',lcOrdLine) WHERE ;
*          cOrdType+ORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
*          GROUP BY Order,Style INTO CURSOR (lcBookSS)
*          INDEX ON ORDER+STYLE TAG (lcBookSS)
*        ENDIF
*        SELECT (lcOrderFile)
*        SET RELATION TO ORDER+STYLE INTO (lcBookSS) ADDITIVE
*        lcBrTtlT = lcBrTtlB
*        lcTranFile = lcBookSS
*        lcTranBr   = [STYLE :H=lcStyHdr :R,nTOTQTY :H='Quantity' :R:P='9999999999',]+;
*                     [nAMOUNT :H='Amount' :14:R:P='99999999999.99']
*      *-- Shipped quantity
*      CASE lnOrdTrans= 3
*        IF !USED(lcShipSS) .OR. !SEEK(laData[1],lcShipSS)
*          SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
*          IF SEEK(laData[1],'CONSINVL')
*            lcFile = 'CONSINVL'
*          ELSE
*            lcFile = 'INVLINE'
*            SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
*          ENDIF
*          SELECT &lcFile..Order,Style,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
*          SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
*          SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount ;
*          FROM (lcFile) WHERE ORDER+STR(LINENO,6)=laData[1] ;
*          GROUP BY Order,Style INTO CURSOR (lcShipSS)
*          INDEX ON ORDER+STYLE TAG (lcShipSS)
*        ENDIF
*        SELECT (lcOrderFile)
*        SET RELATION TO ORDER+STYLE INTO (lcShipSS)  ADDITIVE
*        lcBrTtlT = lcBrTtlS
*        lcTranFile = lcShipSS
*        lcTranBr   = [STYLE :H=lcStyHdr :R,nTOTQTY :H='Quantity' :R:P='9999999999',]+;
*                     [nAMOUNT :H='Amount' :R:P='99999999999.99']
*    ENDCASE
*  *-- Sort by style - Quantity per size- Details
*  CASE lnSortBy = 2 .AND. llQtyPSize .AND. llDetails
*    =IIF(!USED(lcOrdLine),lfCratTemp(),.T.)
*    IF !SEEK(lcOrdType+laData[1],lcOrdLine)
*      SELECT (lcOrdLine)
*      DELETE ALL
*      SELECT ORDLINE
*      SET ORDER TO TAG ORDLINE
*      =SEEK(lcOrdType+laData[1])
*      SCAN REST WHILE cordtype+order+STR(lineno,6) = lcOrdType+laData[1]
*        SCATTER MEMVAR MEMO
*        INSERT INTO (lcOrdLine) FROM MEMVAR
*      ENDSCAN
*    ENDIF  
*    lcOrderFile = lcOrdLine
*    SET ORDER TO TAG ORDLINES IN (lcOrdLine)
*    lcOrderBr = [STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
*                [STORE :H='Store' :R ,] +;
*                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
*                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):R,TOTQTY :H='Quantity' :R,] +;
*                [lnAmount=PRICE*TOTQTY :H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='9999999999.99',]+;
*                [QTY1 :H='Size1' :R:P='99999',] +;
*                [QTY2 :H='Size2' :R:P='99999',] +;
*                [QTY3 :H='Size3' :R:P='99999',] +;
*                [QTY4 :H='Size4' :R:P='99999',] +;
*                [QTY5 :H='Size5' :R:P='99999',] +;
*                [QTY6 :H='Size6' :R:P='99999',] +;
*                [QTY7 :H='Size7' :R:P='99999',] +;
*                [QTY8 :H='Size8' :R:P='99999',] +;
*                [PIK1 :P='99999':R,PIK2 :P='99999':R,PIK3 :P='99999':R,PIK4 :P='99999':R,]+;
*                [PIK5 :P='99999':R,PIK6 :P='99999':R,PIK7 :P='99999':R,PIK8 :P='99999':R,TOTPIK :H='Tot.Pik.']
*    DO CASE
*      *-- Open quantity
*      CASE lnOrdTrans= 1
*        SELECT (lcOrderFile)
*        SET RELATION TO Style INTO Style
*        SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE
*      *-- Booked quantity
*      CASE lnOrdTrans= 2
*        IF !USED(lcBooked)
*          =gfOpenFile(gcDataDir+'ORDLINE','','SH',@lcBooked,.T.)
*        ENDIF
*        lcBrTtlT   = lcBrTtlB
*        lcTranFile = (lcBooked)
*        SET ORDER TO TAG 'ORDLINE' IN (lcBooked)
*        SELECT ORDLINE
*        SET RELATION TO cOrdType+ORDER+STR(LINENO,6) INTO (lcBooked)
*        lcTranBr = [STYLE :H=lcStyHdr :19:R ,] +;
*                   [STORE :H='Store' :8:R ,] +;
*                   [Book1 :H='Size1' :5:R:P='99999' ,] +;
*                   [Book2 :H='Size2' :5:R:P='99999' ,] +;
*                   [Book3 :H='Size3' :5:R:P='99999' ,] +;
*                   [Book4 :H='Size4' :5:R:P='99999' ,] +;
*                   [Book5 :H='Size5' :5:R:P='99999' ,] +;
*                   [Book6 :H='Size6' :5:R:P='99999' ,] +;
*                   [Book7 :H='Size7' :5:R:P='99999' ,] +;
*                   [Book8 :H='Size8' :5:R:P='99999' ,] +;
*                   [PRICE :H='Price' :9:R ,] +;
*                   [TOTBook :H='Quantity' :10:R  ,] +;
*                   [lnAmount=PRICE*TOTBook :14:H='Amount':R:P='99999999999.99']
*      *-- Shipped quantity
*      CASE lnOrdTrans= 3
*        lcBrTtlT = lcBrTtlS
*        SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
*        IF SEEK(laData[1],'CONSINVL')
*          lcTranFile = 'CONSINVL'
*        ELSE
*          lcTranFile = 'INVLINE'
*          SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
*        ENDIF
*        SELECT (lcOrderFile)
*        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcTranFile)
*        lcTranBr = [STYLE :H=lcStyHdr :R ,STORE :H='Store' :R ,] +;
*                   [Qty1 :5:R ,Qty2 :5:R ,Qty3 :5:R ,Qty4 :5:R,] +;
*                   [Qty5 :5:R ,Qty6 :5:R ,Qty7 :5:R ,Qty8 :5:R,] +;
*                   [PRICE :H='Price' :8:R ,TOTQTY :H='Qty.' :P='999999':R,] +;
*                   [lnAmount=PRICE*TOTQTY :14:H='Amount':R:P='99999999999.99',]+;
*                   [INVOICE:H='Invoice' :6:R ,INVDATE:H='Date' :8:R ]
*      *-- Production quantity
*      CASE lnOrdTrans= 4
*        SELECT (lcOrderFile)
*        SET RELATION TO Style INTO STYLE
*        SET RELATION TO IIF(Style.Make,"1","2")+Order+STR(LineNo,6) INTO CutPick ADDITIVE
*        =SEEK(lcOrdType+laData[1])
*        lcBrTtlT = lcBrTtlP
*        lcTranFile = 'CutPick'
*        lcTranBr1= [CutPick.cTktNo :H='Tran #' :R ,] +;
*                   [Type1='C/T' :H='Type' :R ,] +;
*                   [CutTktH.ENTERED  :R ,CutTktH.Complete :R,] +;
*                   [CutPick.Qty1 :H='Size1':5:R ,] +;
*                   [CutPick.Qty2 :H='Size2':5:R ,] +;
*                   [CutPick.Qty3 :H='Size3':5:R ,] +;
*                   [CutPick.Qty4 :H='Size4':5:R ,] +;
*                   [CutPick.Qty5 :H='Size5':5:R ,] +;
*                   [CutPick.Qty6 :H='Size6':5:R ,] +;
*                   [CutPick.Qty7 :H='Size7':5:R ,] +;
*                   [CutPick.Qty8 :H='Size8':5:R ,]
*        lcTranBr1=lcTranBr1+;
*                   [CutPick.TotQty :H='Qty. Alo.' :9:R ,] +;
*                   [CutTktL.Qty1 :H='Size1':6:R ,] +;
*                   [CutTktL.Qty2 :H='Size2':6:R ,] +;
*                   [CutTktL.Qty3 :H='Size3':6:R ,] +;
*                   [CutTktL.Qty4 :H='Size4':6:R ,] +;
*                   [CutTktL.Qty5 :H='Size5':6:R ,] +;
*                   [CutTktL.Qty6 :H='Size6':6:R ,] +;
*                   [CutTktL.Qty7 :H='Size7':6:R ,] +;
*                   [CutTktL.Qty8 :H='Size8':6:R ,] +;
*                   [CutTktL.TotQty :H='Qty. Ord.' :9:R ,] +;
*                   [lnTotQty=CutTktL.TotQty-CutTktL.TotOrd :9:H='Qty. Opn.':R] 
*        lcTranBr2= [CutPick.cTktNo :H='Tran #' :R ,] +;
*                   [Type2='P/O' :H='Type' :R ,] +;
*                   [PosHdr.ENTERED  :R ,PosHdr.Complete :R,] +;
*                   [PosHdr.Vendor :H='Vendor ' :R ,] +;
*                   [CutPick.Qty1 :H='Size1':5:R ,] +;
*                   [CutPick.Qty2 :H='Size2':5:R ,] +;
*                   [CutPick.Qty3 :H='Size3':5:R ,] +;
*                   [CutPick.Qty4 :H='Size4':5:R ,] +;
*                   [CutPick.Qty5 :H='Size5':5:R ,] +;
*                   [CutPick.Qty6 :H='Size6':5:R ,] +;
*                   [CutPick.Qty7 :H='Size7':5:R ,] +;
*                   [CutPick.Qty8 :H='Size8':5:R ,]
*        lcTranBr2=lcTranBr2+;
*                   [CutPick.TotQty :H='Qty. Alo.' :9:R,] +;
*                   [PosLn.Qty1 :H='Size1':6:R ,] +;
*                   [PosLn.Qty2 :H='Size2':6:R ,] +;
*                   [PosLn.Qty3 :H='Size3':6:R ,] +;
*                   [PosLn.Qty4 :H='Size4':6:R ,] +;
*                   [PosLn.Qty5 :H='Size5':6:R ,] +;
*                   [PosLn.Qty6 :H='Size6':6:R ,] +;
*                   [PosLn.Qty7 :H='Size7':6:R ,] +;
*                   [PosLn.Qty8 :H='Size8':6:R ,] +;
*                   [PosLn.TotQty :H='Qty. Ord.' :9:R ,] +;
*                   [lnTotQty=PosLn.TotQty-PosLn.TotOrd :H='Qty. Opn.' :9:R]
*        lcTranBr=IIF(Style.Make,lcTranBr1,lcTranBr2)
*        llDomestic = Style.Make
*      *-- Depleted orders
*      CASE lnOrdTrans= 5
*        IF !USED(lcDepleted) .OR. !SEEK(laData[1],lcDepleted)
*          SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
*          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
*          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
*          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
*          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
*          FROM ORDHDR,ORDLINE;
*          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=lcOrdType+laData[2] AND ;
*          ORDLINE.cFromOrder=laData[1] AND ;
*          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
*          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)
*          INDEX ON CFROMORDER+STR(BULKLINENO,6) TAG (lcDepleted)
*        ENDIF
*        lcBrTtlT   = lcBrTtlD
*        lcTranFile = lcDepleted
*        SELECT (lcOrderFile)
*        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcDepleted)
*        lcTranBr = [ORDER :R ,] +;
*                   [STYLE ,STORE :R,BOOK1 :R,BOOK2 :R,BOOK3 :R,] +;
*                   [BOOK4 :R,BOOK5 :R,BOOK6 :R,BOOK7 :R,BOOK8 :R,TOTBOOK :R ,] +;
*                   [lnAmount=PRICE*TOTBOOK :14:H='Amount':R:P='99999999999.99',] +;
*                   IIF(laSetups[2,2]='Y', ;
*                   "n=laData[27] :H='Rep 1' :R ,COMM1 :H='Comm 1' :R ," +;
*                   "n1=laData[29] :H='Rep 2':R ,COMM2 :H='Comm 2' :R ",'') +;
*                   IIF(laData[7],[,CUSTPO :H='Cust P/O':R],'')
*      CASE lnOrdTrans= 7
*        lcBrTtlT   = lcBrTtlC
*        lcTranFile = IIF(laScrMode[2],'ORDCANLN',lcOrdCanLn)
*        SELECT (lcOrderFile)
*        SET RELATION TO cOrdType+Order+STR(LineNo,6) INTO (lcTranFile)
*        lcTranBr = [Cancelled :R :H='Date',TotQty :H= 'Tot. Can.' :R,]+;
*                   [cDesc=gfCodDes(cCancReson,'CCANCRESON') :R :H='Cancelation Reason',]+;
*                   [Qty1 :R :H='Can1',Qty2 :R :H='Can2',Qty3 :R :H='Can3',Qty4 :R :H='Can4',]+;
*                   [Qty5 :R :H='Can5',Qty6 :R :H='Can6',Qty7 :R :H='Can7',Qty8 :R :H='Can8']
*      CASE lnOrdTrans = 8
*        lcBrTtlT   = lcBrTtlW
*        lcTranFile = "CutPick"
*        lcTranBr   = [lnDummy=IIF(TranCd='1','CT #',IIF(TranCd='2','PO #','Adornment')) :R:10:H='Tran. Type',]+;
*                     [cTktNo:R:H='Tran. #',Order :R:H='Order #', Style:R:20, Qty1:R, Qty2:R,]+;
*                     [Qty3:R, Qty4:R, Qty5:R, Qty6:R, Qty7:R, Qty8:R, TotQty:R]
*    ENDCASE
*  *-- Sort by style - Quantity per size- Summary
*  CASE lnSortBy = 2 .AND. llQtyPSize .AND. !llDetails
*    IF !USED(lcOrdSS) .OR. !SEEK(lcOrdType+laData[1],lcOrdSS)
*      lcFile=IIF(laScrMode[2],'ORDLINE',lcOrdLine)
*      SET ORDER TO TAG ORDLINE IN (lcFile)
*      SELECT cOrdType,&lcFile..Order,Style,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
*      SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
*      SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount ,;
*      SUM(PIK1) AS nPIK1,SUM(PIK2) AS nPIK2,SUM(PIK3) AS nPIK3,SUM(PIK4) AS nPIK4,;
*      SUM(PIK5) AS nPIK5,SUM(PIK6) AS nPIK6,SUM(PIK7) AS nPIK7,SUM(PIK8) AS nPIK8,;
*      SUM(TOTPIK) AS nTotPIK;
*      FROM (lcFile) ;
*      WHERE cOrdType+&lcFile..ORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
*      GROUP BY &lcFile..Order,Style INTO CURSOR (lcOrdSS)
*      INDEX ON cOrdType+ORDER+STYLE TAG (lcOrdSS)
*    ENDIF
*    lcOrderFile = lcOrdSS
*    lcOrderBr = [STYLE :H=lcStyHdr :R ,] +;
*                [nQTY1 :H='Qty1' :6:R:P='999999',]+;
*                [nQTY2 :H='Qty2' :6:R:P='999999',]+;
*                [nQTY3 :H='Qty3' :6:R:P='999999',]+;
*                [nQTY4 :H='Qty4' :6:R:P='999999',]+;
*                [nQTY5 :H='Qty5' :6:R:P='999999',]+;
*                [nQTY6 :H='Qty6' :6:R:P='999999',]+;
*                [nQTY7 :H='Qty7' :6:R:P='999999',]+;
*                [nQTY8 :H='Qty8' :6:R:P='999999',]+;
*                [nTOTQTY :H='Quantity' :10:R:P='9999999999',] +;
*                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Wholesale Price',;
*                'Amount'):14:R:P='99999999999.99',] +;
*                [nPIK1 :H='Pik1' :P='99999':R,]+;
*                [nPIK2 :H='Pik2' :P='99999':R,]+;
*                [nPIK3 :H='Pik3' :P='99999':R,]+;
*                [nPIK4 :H='Pik4' :P='99999':R,]+;
*                [nPIK5 :H='Pik5' :P='99999':R,]+;
*                [nPIK6 :H='Pik6' :P='99999':R,]+;
*                [nPIK7 :H='Pik7' :P='99999':R,]+;
*                [nPIK8 :H='Pik8' :P='99999':R,]+;
*                [nTotPik :H='Tot.Pik' :R]
*    DO CASE
*      *-- Booked quantity    
*      CASE lnOrdTrans= 2
*        IF !USED(lcBookSS) .OR. !SEEK(laData[1],lcBookSS)
*          SET ORDER TO TAG ORDLINE IN ORDLINE
*          SELECT OrdLine.ORDER,Style,SUM(Book1) AS nQty1,SUM(Book2) AS nQty2,;
*          SUM(Book3) AS nQty3,SUM(Book4) AS nQty4,SUM(Book5) AS nQty5,SUM(Book6) AS nQty6,;
*          SUM(Book7) AS nQty7,SUM(Book8) AS nQty8,SUM(TOTBook) AS nTotQTy,SUM(TOTBook*PRICE) AS nAmount ;
*          FROM ORDLINE WHERE cOrdType+ORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
*          GROUP BY Order,Style INTO CURSOR (lcBookSS)
*          INDEX ON ORDER+STYLE TAG (lcBookSS)
*        ENDIF
*        SELECT (lcOrderFile)
*        SET RELATION TO ORDER+STYLE INTO (lcBookSS)
*        lcBrTtlT   = lcBrTtlB
*        lcTranFile = lcBookSS
*        lcTranBr  = [STYLE :H=lcStyHdr :R ,] +;
*                    [nQTY1 :H='Qty1' :6:R:P='999999',]+;
*                    [nQTY2 :H='Qty2' :6:R:P='999999',]+;
*                    [nQTY3 :H='Qty3' :6:R:P='999999',]+;
*                    [nQTY4 :H='Qty4' :6:R:P='999999',]+;
*                    [nQTY5 :H='Qty5' :6:R:P='999999',]+;
*                    [nQTY6 :H='Qty6' :6:R:P='999999',]+;
*                    [nQTY7 :H='Qty7' :6:R:P='999999',]+;
*                    [nQTY8 :H='Qty8' :6:R:P='999999',]+;
*                    [nTOTQTY :H='Quantity' :10:R:P='9999999999' ,] +;
*                    [nAMOUNT :H='Amount' :14:R:P='99999999999.99']
*      *-- Shipped quantity
*      CASE lnOrdTrans= 3
*        IF !USED(lcShipSS) .OR. !SEEK(laData[1],lcShipSS)
*          SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
*          IF SEEK(laData[1],'CONSINVL')
*            lcFile = 'CONSINVL'
*          ELSE
*            lcFile = 'INVLINE'
*            SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
*          ENDIF
*          SELECT &lcFile..Order,Style,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
*          SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
*          SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount ;
*          FROM (lcFile) WHERE ORDER+STR(LINENO,6)=laData[1] ;
*          GROUP BY Order,Style INTO CURSOR (lcShipSS)
*          INDEX ON Order+STYLE TAG (lcShipSS)
*        ENDIF
*        SELECT (lcOrderFile)
*        SET RELATION TO Order+STYLE INTO (lcShipSS)
*        lcBrTtlT = lcBrTtlS
*        lcTranFile = lcShipSS
*        lcTranBr  = [STYLE :H=lcStyHdr :R ,] +;
*                    [nQTY1 :H='Qty1' :6:R:P='999999',]+;
*                    [nQTY2 :H='Qty2' :6:R:P='999999',]+;
*                    [nQTY3 :H='Qty3' :6:R:P='999999',]+;
*                    [nQTY4 :H='Qty4' :6:R:P='999999',]+;
*                    [nQTY5 :H='Qty5' :6:R:P='999999',]+;
*                    [nQTY6 :H='Qty6' :6:R:P='999999',]+;
*                    [nQTY7 :H='Qty7' :6:R:P='999999',]+;
*                    [nQTY8 :H='Qty8' :6:R:P='999999',]+;
*                    [nTOTQTY :H='Quantity' :10:R:P='9999999999' ,] +;
*                    [nAMOUNT :H='Amount' :14:R:P='99999999999.99']
*    ENDCASE
*  *-- Sort by store# - Total quantity - Details
*  CASE lnSortBy = 3 .AND. !llQtyPSize .AND. llDetails
*    lcOrderFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
*    SET ORDER TO TAG 'ORDLINST' IN (lcOrderFile)
*    lcOrderBr = [STORE :H='Store' :R,STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
*                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
*                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):9:R ,TOTQTY :H='Quantity' :10:R,] +;
*                [lnAmount=PRICE*TOTQTY :10:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99',TOTPIK :H='Tot.Pik']
*    DO CASE
*      *-- Open quantity
*      CASE lnOrdTrans= 1
*        SELECT (lcOrderFile)
*        SET RELATION TO Style INTO Style
*        SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE
*      *-- Booked quantity
*      CASE lnOrdTrans= 2
*        IF !USED(lcBooked)
*          =gfOpenFile(gcDataDir+'ORDLINE','','SH',@lcBooked,.T.)
*        ENDIF
*        lcBrTtlT   = lcBrTtlB
*        lcTranFile = (lcBooked)
*        SET ORDER TO TAG 'ORDLINE' IN (lcBooked)
*        SELECT ORDLINE
*        SET RELATION TO cOrdType+ORDER+STR(LINENO,6) INTO (lcBooked)
*        lcTranBr = [STORE :H='Store' :R ,STYLE :H=lcStyHdr :R,] +;
*                   [PRICE :H='Price' :9:R,TotBook :H='Quantity' :10:R,] +;
*                   [lnAmount=PRICE*TotBook :14:H='Amount':R:P='9999999999.99']
*      *-- Shipped quantity
*      CASE lnOrdTrans= 3
*        lcBrTtlT   = lcBrTtlS
*        SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
*        IF SEEK(laData[1],'CONSINVL')
*          lcTranFile = 'CONSINVL'
*        ELSE
*          lcTranFile = 'INVLINE'
*          SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
*        ENDIF
*        SELECT ORDLINE
*        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcTranFile)
*        lcTranBr   = [STYLE :H=lcStyHdr :R ,STORE :H='Store' :R ,]+;
*                     [PRICE :H='Price' :9:R,TOTQTY :H='Quantity' :10:R,] +;
*                     [lnAmount=PRICE*TOTQTY :14:H='Amount':R:P='99999999999.99',]+;
*                     [INVOICE:H='Invoice' :R ,INVDATE:H='Date' :R ]
*      *-- Production quantity
*      CASE lnOrdTrans= 4
*        SELECT ORDLINE
*        SET RELATION TO Style INTO STYLE
*        SET RELATION TO IIF(Style.Make,"1","2")+Order+STR(LineNo,6) INTO CutPick ADDITIVE
*        =SEEK(lcOrdType+laData[1])
*        lcBrTtlT = lcBrTtlP
*        lcTranFile = 'CutPick'
*        lcTranBr1 = [CutPick.cTktNo :H='Tran #' :R ,] +;
*                    [Type1='C/T' :H='Type' :R ,] +;
*                    [CutTktH.ENTERED  :R ,CutTktH.Complete :R,] +;
*                    [CutPick.TotQty :H='Qty. Alo.' :9:R ,] +;
*                    [CutTktL.TotQty :H='Qty. Ord.' :9:R ,] +;
*                    [lnTotQty=CutTktL.TotQty-CutTktL.TotOrd :9:H='Qty. Opn.':R] 
*         lcTranBr2= [CutPick.cTktNo :H='Tran #' :R ,] +;
*                    [Type2='P/O' :H='Type' :R ,] +;
*                    [PosHdr.ENTERED  :R ,PosHdr.Complete :R,] +;
*                    [PosHdr.Vendor :H='Vendor ' :R ,] +;
*                    [CutPick.TotQty  :H='Qty. Alo.' :9:R,] +;
*                    [PosLn.TotQty   :H='Qty. Ord.' :9:R ,] +;
*                    [lnTotQty=PosLn.TotQty-PosLn.TotOrd   :H='Qty. Opn.' :9:R]
*        lcTranBr=IIF(Style.Make,lcTranBr1,lcTranBr2)
*        llDomestic = Style.Make
*      *-- Depleted orders
*      CASE lnOrdTrans= 5
*        IF !USED(lcDepleted) .OR. !SEEK(laData[1],lcDepleted)
*          SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
*          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
*          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
*          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
*          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
*          FROM ORDHDR,ORDLINE;
*          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=laData[2]+lcOrdType AND ;
*          ORDLINE.cFromOrder=laData[1] AND ;
*          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
*          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)
*          INDEX ON CFROMORDER+STR(BULKLINENO,6) TAG (lcDepleted)
*        ENDIF
*        lcBrTtlT   = lcBrTtlD
*        lcTranFile = lcDepleted
*        SELECT (lcOrderFile)
*        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcDepleted)
*        lcTranBr = [ORDER :R ,STYLE :R ,STORE :R ,] +;
*                   [GROUP :H='Grp' :R ,PRICE :R ,TOTBOOK :H= 'Qty.' :R ,] +;
*                   [lnAmount=PRICE*TOTBOOK :14:H='Amount':R:P='99999999999.99',] +;
*                   [PIKTKT :H='Piktkt' :6:R ,PIKDATE :H='Date' :8:R]
*      CASE lnOrdTrans= 7
*        lcBrTtlT   = lcBrTtlC
*        lcTranFile = IIF(laScrMode[2],'ORDCANLN',lcOrdCanLn)
*        SELECT (lcOrderFile)
*        SET RELATION TO cOrdType+Order+STR(LineNo,6) INTO (lcTranFile)
*        lcTranBr = [Cancelled :R :H='Date',TotQty :H= 'Total Canceld' :R,]+;
*                   [cDesc=gfCodDes(cCancReson,'CCANCRESON') :R :H='Cancelation Reason']
*      CASE lnOrdTrans = 8
*        lcBrTtlT   = lcBrTtlW
*        lcTranFile = "CutPick"
*        lcTranBr   = [lnDummy=IIF(TranCd='1','CT #',IIF(TranCd='2','PO #','Adornment')) :R:10:H='Tran. Type',]+;
*                     [cTktNo:R:H='Tran. #',Order :R:H='Order #', Style:R:20, Qty1:R, Qty2:R,]+;
*                     [Qty3:R, Qty4:R, Qty5:R, Qty6:R, Qty7:R, Qty8:R, TotQty:R]
*    ENDCASE
*  *-- Sort by store# - Total quantity- Summary
*  CASE lnSortBy = 3 .AND. !llQtyPSize .AND. !llDetails
*    IF !USED(lcOrdTS) .OR. !SEEK(lcOrdType+laData[1],lcOrdTS)
*      lcFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
*      SET ORDER TO TAG ORDLINE IN (lcFile)
*      SELECT cOrdType,&lcFile..Order,Store,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
*      SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
*      SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQty) AS nTotQTy,SUM(TOTQty*PRICE) AS nAmount ,;
*      SUM(PIK1) AS nPIK1,SUM(PIK2) AS nPIK2,SUM(PIK3) AS nPIK3,SUM(PIK4) AS nPIK4,;
*      SUM(PIK5) AS nPIK5,SUM(PIK6) AS nPIK6,SUM(PIK7) AS nPIK7,SUM(PIK8) AS nPIK8,;
*      SUM(TOTPIK) AS nTotPIK;
*      FROM (lcFile) WHERE cOrdType+&lcFile..ORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
*      GROUP BY &lcFile..Order,Store INTO CURSOR (lcOrdTS)
*
*      INDEX ON cOrdType+ORDER+STORE TAG (lcOrdTS)
*    ENDIF
*    SELECT (lcOrdTS)
*    SET RELATION TO 'S'+laData[2]+Store INTO Customer
*    lcOrderFile = lcOrdTS
*    lcOrderBr = [STORE :H='Store' :R ,] +;
*                [Customer.StName :H='Name' :30 ,] +;
*                [nTOTQTY :H='Quantity' :10:R:P='9999999999'  ,] +;
*                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Wholesale Price',;
*                'Amount'):14:R:P='99999999999.99',nTotPIK :H = 'Picked']
*    DO CASE
*      *-- Booked quantity
*      CASE lnOrdTrans= 2
*        IF !USED(lcBookTS) .OR. !SEEK(laData[1],lcBookTS)
*          lcFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
*          SET ORDER TO TAG ORDLINE IN (lcFile)
*          SELECT &lcFile..Order,Store,SUM(Book1) AS nQty1,SUM(Book2) AS nQty2,;
*          SUM(Book3) AS nQty3,SUM(Book4) AS nQty4,SUM(Book5) AS nQty5,SUM(Book6) AS nQty6,;
*          SUM(Book7) AS nQty7,SUM(Book8) AS nQty8,SUM(TOTBook) AS nTotQTy,SUM(TOTBook*PRICE) AS nAmount ;
*          FROM (lcFile) WHERE &lcFile..cOrdType+&lcFile..ORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
*          GROUP BY &lcFile..Order,Store INTO CURSOR (lcBookTS)
*          INDEX ON Order+STORE TAG (lcBookTS)
*        ENDIF
*        SELECT (lcOrderFile)
*        SET RELATION TO Order+STORE INTO (lcBookTS) ADDITIVE
*        lcTranFile = lcBookTS
*        lcBrTtlT   = lcBrTtlB
*        lcTranBr   = [STORE :H='Store' :R ,] +;
*                     [nTOTQTY :H='Quantity' :10:R:P='9999999999' ,] +;
*                     [nAMOUNT :H='Amount' :14:R:P='99999999999.99']
*      *-- Shipped quantity
*      CASE lnOrdTrans= 3
*        IF !USED(lcShipTS) .OR. !SEEK(laData[1],lcShipTS)
*          SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
*          IF SEEK(laData[1],'CONSINVL')
*            lcFile = 'CONSINVL'
*          ELSE
*            lcFile = 'INVLINE'
*            SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
*          ENDIF
*          SELECT &lcFile..Order,Store,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
*          SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
*          SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount ;
*          FROM (lcFile) WHERE ORDER+STR(LINENO,6)=laData[1] ;
*          GROUP BY Order,Store INTO CURSOR (lcShipTS)
*          INDEX ON Order+STORE TAG (lcShipTS)
*        ENDIF
*        SELECT (lcOrderFile)
*        SET RELATION TO Order+STORE INTO (lcShipTS) ADDITIVE
*        lcBrTtlT   = lcBrTtlS
*        lcTranFile = lcShipTS
*        lcTranBr   = [STORE :H='Store' :12:R ,] +;
*                     [nTOTQTY :H='Quantity' :10:R:P='9999999999' ,] +;
*                     [nAMOUNT :H='Amount' :14:R:P='99999999999.99']
*    ENDCASE
*  *-- Sort by store# - Quantity per size- Details
*  CASE lnSortBy = 3 .AND. llQtyPSize .AND. llDetails
*    lcOrderFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
*    SET ORDER TO TAG 'ORDLINST' IN (lcOrderFile)
*    lcOrderBr = [STORE :H='Store' :R ,STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'')+;
*                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(), ]),'') +;
*                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):9:R,TOTQTY :H='TotQty' :P='999999':R,]+;
*                [lnAmount=PRICE*TOTQTY :14:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99',]+;
*                [QTY1 :H='Size1' :R:P='99999',QTY2 :H='Size2' :R:P='99999',] +;
*                [QTY3 :H='Size3' :R:P='99999',QTY4 :H='Size4' :R:P='99999',] +;
*                [QTY5 :H='Size5' :R:P='99999',QTY6 :H='Size6' :R:P='99999',] +;
*                [QTY7 :H='Size7' :R:P='99999',QTY8 :H='Size8' :R:P='99999',]  +;
*                [PIK1 :P='99999':R,PIK2 :P='99999':R,PIK3 :P='99999':R,PIK4 :P='99999':R,]+;
*                [PIK5 :P='99999':R,PIK6 :P='99999':R,PIK7 :P='99999':R,PIK8 :P='99999':R,TOTPIK:H='Tot.Pik.']
*    DO CASE
*      *-- Open quantity
*      CASE lnOrdTrans= 1
*        SELECT (lcOrderFile)
*        SET RELATION TO Style INTO Style
*        SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE
*      *-- Booked quantity
*      CASE lnOrdTrans= 2
*        IF !USED(lcBooked)
*          =gfOpenFile(gcDataDir+'ORDLINE','','SH',@lcBooked,.T.)
*        ENDIF
*        lcBrTtlT   = lcBrTtlB
*        lcTranFile = lcBooked
*        SET ORDER TO TAG 'ORDLINE' IN (lcBooked)
*        SELECT ORDLINE
*        SET RELATION TO cOrdType+ORDER+STR(LINENO,6) INTO (lcBooked)
*        lcTranBr = [STORE :H='Store' :R ,STYLE :H=lcStyHdr :R ,] +;
*                   [Book1 :H='Size1' :5:R:P='99999',]+;
*                   [Book2 :H='Size2' :5:R:P='99999',]+;
*                   [Book3 :H='Size3' :5:R:P='99999',]+;
*                   [Book4 :H='Size4' :5:R:P='99999',]+;
*                   [Book5 :H='Size5' :5:R:P='99999',]+;
*                   [Book6 :H='Size6' :5:R:P='99999',]+;
*                   [Book7 :H='Size7' :5:R:P='99999',]+;
*                   [Book8 :H='Size8' :5:R:P='99999',]+;
*                   [PRICE :H='Price' :9:R ,] +;
*                   [TOTBook :H='Qty.' :6:R ,] +;
*                   [lnAmount=PRICE*TOTBook :14:H='Amount':R:P='99999999999.99']
*      *-- Shipped quantity
*      CASE lnOrdTrans= 3
*        lcBrTtlT = lcBrTtlS
*        SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
*        IF SEEK(laData[1],'CONSINVL')
*          lcTranFile = 'CONSINVL'
*        ELSE
*          lcTranFile = 'INVLINE'
*          SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
*        ENDIF
*        SELECT ORDLINE
*        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcTranFile)
*        lcTranBr   = [STYLE :H=lcStyHdr :R,STORE :H='Store' :R ,] +;
*                     [Qty1 :5:R ,Qty2 :5:R ,Qty3 :5:R ,Qty4 :5:R ,] +;
*                     [Qty5 :5:R ,Qty6 :5:R ,Qty7 :5:R ,Qty8 :5:R ,] +;
*                     [PRICE :H='Price' :9:R ,TOTQTY :H='Qty.' :P='999999':R ,] +;
*                     [lnAmount=PRICE*TOTQTY :14:H='Amount':R:P='99999999999.99',]+; 
*                     [INVOICE:H='Invoice' :R ,INVDATE:H='Date' :R ]
*      *-- Production quantity
*      CASE lnOrdTrans= 4
*        SELECT OrdLine
*        SET RELATION TO Style INTO STYLE
*        SET RELATION TO IIF(Style.Make,"1","2")+Order+STR(LineNo,6) INTO CutPick ADDITIVE
*        =SEEK(lcOrdType+laData[1])
*        lcBrTtlT = lcBrTtlP
*        lcTranFile = 'CutPick'
*        lcTranBr1= [CutPick.cTktNo :H='Tran #' :R ,] +;
*                   [Type1='C/T' :H='Type' :R ,] +;
*                   [CutTktH.ENTERED  :R ,CutTktH.Complete :R,] +;
*                   [CutPick.Qty1 :H='Size1':6:R ,] +;
*                   [CutPick.Qty2 :H='Size2':6:R ,] +;
*                   [CutPick.Qty3 :H='Size3':6:R ,] +;
*                   [CutPick.Qty4 :H='Size4':6:R ,] +;
*                   [CutPick.Qty5 :H='Size5':6:R ,] +;
*                   [CutPick.Qty6 :H='Size6':6:R ,] +;
*                   [CutPick.Qty7 :H='Size7':6:R ,] +;
*                   [CutPick.Qty8 :H='Size8':6:R ,]
*        lcTranBr1=lcTranBr1+;
*                   [CutPick.TotQty :H='Qty. Alo.' :9:R ,] +;
*                   [CutTktL.Qty1 :H='Size1':6:R ,] +;
*                   [CutTktL.Qty2 :H='Size2':6:R ,] +;
*                   [CutTktL.Qty3 :H='Size3':6:R ,] +;
*                   [CutTktL.Qty4 :H='Size4':6:R ,] +;
*                   [CutTktL.Qty5 :H='Size5':6:R ,] +;
*                   [CutTktL.Qty6 :H='Size6':6:R ,] +;
*                   [CutTktL.Qty7 :H='Size7':6:R ,] +;
*                   [CutTktL.Qty8 :H='Size8':6:R ,] +;
*                   [CutTktL.TotQty :H='Qty. Ord.' :9:R ,] +;
*                   [lnTotQty=CutTktL.TotQty-CutTktL.TotOrd :9:H='Qty. Opn.':R] 
*        lcTranBr2= [CutPick.cTktNo :H='Tran #' :R ,] +;
*                   [Type2='P/O' :H='Type' :R ,] +;
*                   [PosHdr.ENTERED  :R ,PosHdr.Complete :R,] +;
*                   [PosHdr.Vendor :H='Vendor ' :R ,] +;
*                   [CutPick.Qty1 :H='Size1':6:R ,] +;
*                   [CutPick.Qty2 :H='Size2':6:R ,] +;
*                   [CutPick.Qty3 :H='Size3':6:R ,] +;
*                   [CutPick.Qty4 :H='Size4':6:R ,] +;
*                   [CutPick.Qty5 :H='Size5':6:R ,] +;
*                   [CutPick.Qty6 :H='Size6':6:R ,] +;
*                   [CutPick.Qty7 :H='Size7':6:R ,] +;
*                   [CutPick.Qty8 :H='Size8':6:R ,]
*        lcTranBr2=lcTranBr2+;
*                   [CutPick.TotQty :H='Qty. Alo.' :9:R,] +;
*                   [PosLn.Qty1 :H='Size1':6:R ,] +;
*                   [PosLn.Qty2 :H='Size2':6:R ,] +;
*                   [PosLn.Qty3 :H='Size3':6:R ,] +;
*                   [PosLn.Qty4 :H='Size4':6:R ,] +;
*                   [PosLn.Qty5 :H='Size5':6:R ,] +;
*                   [PosLn.Qty6 :H='Size6':6:R ,] +;
*                   [PosLn.Qty7 :H='Size7':6:R ,] +;
*                   [PosLn.Qty8 :H='Size8':6:R ,] +;
*                   [PosLn.TotQty :H='Qty. Ord.' :9:R ,] +;
*                   [lnTotQty=PosLn.TotQty-PosLn.TotOrd   :H='Qty. Opn.' :9:R]
*        lcTranBr=IIF(Style.Make,lcTranBr1,lcTranBr2)
*        llDomestic = Style.Make
*      *-- Depleted orders
*      CASE lnOrdTrans= 5
*        IF !USED(lcDepleted) .OR. !SEEK(laData[1],lcDepleted)
*          SELECT ORDHDR.ORDER,ORDLINE.STYLE,ORDLINE.PRICE,;
*          ORDLINE.STORE,ORDLINE.BOOK1,ORDLINE.BOOK2,ORDLINE.BOOK3,ORDLINE.BOOK4,;
*          ORDLINE.BOOK5,ORDLINE.BOOK6,ORDLINE.BOOK7,ORDLINE.BOOK8,ORDLINE.TOTBOOK,;
*          ORDLINE.CFROMORDER,ORDLINE.BULKLINENO,ORDLINE.COMM1,ORDLINE.COMM2,;
*          ORDLINE.CUSTPO,ORDLINE.GROUP,ORDLINE.PIKTKT,ORDLINE.PIKDATE;
*          FROM ORDHDR,ORDLINE;
*          WHERE ORDHDR.ACCOUNT+ORDHDR.cOrdType+ORDHDR.ORDER=lcOrdType+laData[2] AND ;
*          ORDLINE.cFromOrder=laData[1] AND ;
*          ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)=;
*          lcOrdType+ORDHDR.ORDER INTO CURSOR (lcDepleted)
*          INDEX ON CFROMORDER+STR(BULKLINENO,6) TAG (lcDepleted)
*        ENDIF
*        lcBrTtlT   = lcBrTtlD
*        lcTranFile = lcDepleted
*        SELECT (lcOrderFile)
*        SET RELATION TO ORDER+STR(LINENO,6) INTO (lcDepleted)
*        lcTranBr = [ORDER :R ,] +;
*                   [STYLE ,STORE :R,BOOK1 :R,BOOK2 :R,BOOK3 :R,] +;
*                   [BOOK4 :R,BOOK5 :R,BOOK6 :R,BOOK7 :R,BOOK8 :R,TOTBOOK :R ,] +;
*                   [lnAmount=PRICE*TOTBOOK :14:H='Amount':R:P='99999999999.99',] +;
*                   IIF(laSetups[2,2]='Y', ;
*                   "n=laData[27] :H='Rep 1' :R ,COMM1 :H='Comm 1' :R ," +;
*                   "n1=laData[29] :H='Rep 2':R ,COMM2 :H='Comm 2' :R ",'') +;
*                   IIF(laData[7],[,CUSTPO :H='Cust P/O':R],'')
*      CASE lnOrdTrans= 7
*        lcBrTtlT   = lcBrTtlC
*        lcTranFile = IIF(laScrMode[2],'ORDCANLN',lcOrdCanLn)
*        SELECT (lcOrderFile)
*        SET RELATION TO cOrdType+Order+STR(LineNo,6) INTO (lcTranFile)
*        lcTranBr = [Cancelled :R :H='Date',TotQty :H= 'Tot. Can.' :R,]+;
*                   [cDesc=gfCodDes(cCancReson,'CCANCRESON') :R :H='Cancelation Reason',]+;
*                   [Qty1 :R :H='Can1',Qty2 :R :H='Can2',Qty3 :R :H='Can3',Qty4 :R :H='Can4',]+;
*                   [Qty5 :R :H='Can5',Qty6 :R :H='Can6',Qty7 :R :H='Can7',Qty8 :R :H='Can8']
*      CASE lnOrdTrans = 8
*        lcBrTtlT   = lcBrTtlW
*        lcTranFile = "CutPick"
*        lcTranBr   = [lnDummy=IIF(TranCd='1','CT #',IIF(TranCd='2','PO #','Adornment')) :R:10:H='Tran. Type',]+;
*                     [cTktNo:R:H='Tran. #',Order :R:H='Order #', Style:R:20, Qty1:R, Qty2:R,]+;
*                     [Qty3:R, Qty4:R, Qty5:R, Qty6:R, Qty7:R, Qty8:R, TotQty:R]
*    ENDCASE
*  *-- Sort by store# - Quantity per size- Summary
*  CASE lnSortBy = 3 .AND. llQtyPSize .AND. !llDetails
*    IF !USED(lcOrdTS) .OR. !SEEK(lcOrdType+laData[1],lcOrdTS)
*      lcFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
*      SET ORDER TO TAG ORDLINE IN (lcFile)
*      SELECT cOrdType,&lcFile..Order,Store,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
*      SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
*      SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount, ;
*      SUM(PIK1) AS nPIK1,SUM(PIK2) AS nPIK2,SUM(PIK3) AS nPIK3,SUM(PIK4) AS nPIK4,;
*      SUM(PIK5) AS nPIK5,SUM(PIK6) AS nPIK6,SUM(PIK7) AS nPIK7,SUM(PIK8) AS nPIK8,;
*      SUM(TOTPIK) AS nTotPIK;
*      FROM (lcFile) WHERE cOrdType+&lcFile..Order+STR(LINENO,6)=lcOrdType+laData[1] ;
*      GROUP BY &lcFile..Order,Store INTO CURSOR (lcOrdTS)
*      INDEX ON cOrdType+ORDER+STORE TAG (lcOrdTS)
*    ENDIF
*    lcOrderFile = lcOrdTS
*    lcOrderBr = [STORE :H='Store' :8:R ,] +;
*                [nQTY1 :H='Qty1' :6:R:P='999999',]+;
*                [nQTY2 :H='Qty2' :6:R:P='999999',]+;
*                [nQTY3 :H='Qty3' :6:R:P='999999',]+;
*                [nQTY4 :H='Qty4' :6:R:P='999999',]+;
*                [nQTY5 :H='Qty5' :6:R:P='999999',]+;
*                [nQTY6 :H='Qty6' :6:R:P='999999',]+;
*                [nQTY7 :H='Qty7' :6:R:P='999999',]+;
*                [nQTY8 :H='Qty8' :6:R:P='999999',]+;
*                [nTOTQTY :H='Tot. Qty.' :10:R:P='9999999999' ,] +;
*                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Transmitted Price',;
*                'Price') :14:R:P='99999999999.99',] +;
*                [nPIK1 :H='Pik1' :P='99999':R,]+;
*                [nPIK2 :H='Pik2' :P='99999':R,]+;
*                [nPIK3 :H='Pik3' :P='99999':R,]+;
*                [nPIK4 :H='Pik4' :P='99999':R,]+;
*                [nPIK5 :H='Pik5' :P='99999':R,]+;
*                [nPIK6 :H='Pik6' :P='99999':R,]+;
*                [nPIK7 :H='Pik7' :P='99999':R,]+;
*                [nPIK8 :H='Pik8' :P='99999':R,]+;
*                [nTotPik :H='Tot.Pik' :P='99999':R]
*    DO CASE
*      *-- Booked quantity
*      CASE lnOrdTrans= 2
*        IF !USED(lcBookTS) .OR. !SEEK(laData[1],lcBookTS)
*          lcFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
*          SET ORDER TO TAG ORDLINE IN (lcFile)
*          SELECT &lcFile..Order,Store,SUM(BOOK1) AS nQty1,SUM(BOOK2) AS nQty2,;
*          SUM(BOOK3) AS nQty3,SUM(BOOK4) AS nQty4,SUM(BOOK5) AS nQty5,SUM(BOOK6) AS nQty6,;
*          SUM(BOOK7) AS nQty7,SUM(BOOK8) AS nQty8,SUM(TOTBOOK) AS nTotQTy,SUM(TOTBOOK*PRICE) AS nAmount ;
*          FROM (lcFile) WHERE &lcFile..cOrdType+&lcFile..ORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
*          GROUP BY &lcFile..Order,Store INTO CURSOR (lcBookTS)
*          INDEX ON Order+STORE TAG (lcBookTS)
*        ENDIF
*        SELECT (lcOrderFile)
*        SET RELATION TO Order+STORE INTO (lcBookTS)
*        lcTranFile = lcBookTS
*        lcBrTtlT   = lcBrTtlB
*        lcTranBr   = [STORE :H='Store' :8:R ,] +;
*                     [nQTY1 :H='Qty1' :6:R:P='999999',]+;
*                     [nQTY2 :H='Qty2' :6:R:P='999999',]+;
*                     [nQTY3 :H='Qty3' :6:R:P='999999',]+;
*                     [nQTY4 :H='Qty4' :6:R:P='999999',]+;
*                     [nQTY5 :H='Qty5' :6:R:P='999999',]+;
*                     [nQTY6 :H='Qty6' :6:R:P='999999',]+;
*                     [nQTY7 :H='Qty7' :6:R:P='999999',]+;
*                     [nQTy8 :H='Qty8' :6:R:P='999999',]+;
*                     [nTOTQTY :H='Tot. Qty.' :10:R:P='9999999999' ,] +;
*                     [nAMOUNT :H='Amount'   :14:R:P='99999999999.99']
*      *-- Shipped quantity
*      CASE lnOrdTrans= 3
*        IF !USED(lcShipTS) .OR. !SEEK(laData[1],lcShipTS)
*          SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
*          IF SEEK(laData[1],'CONSINVL')
*            lcFile = 'CONSINVL'
*          ELSE
*            lcFile = 'INVLINE'
*            SET ORDER TO TAG 'INVLINEO' IN 'INVLINE'
*          ENDIF
*          SELECT &lcFile..Order,Store,SUM(Qty1) AS nQty1,SUM(Qty2) AS nQty2,;
*          SUM(Qty3) AS nQty3,SUM(Qty4) AS nQty4,SUM(Qty5) AS nQty5,SUM(Qty6) AS nQty6,;
*          SUM(Qty7) AS nQty7,SUM(Qty8) AS nQty8,SUM(TOTQTY) AS nTotQTy,SUM(TOTQTY*PRICE) AS nAmount ;
*          FROM (lcFile) WHERE ORDER+STR(LINENO,6)=laData[1] ;
*          GROUP BY Order,Store INTO CURSOR (lcShipTS)
*          INDEX ON Order+STORE TAG (lcShipTS)
*        ENDIF
*        SELECT (lcOrderFile)
*        SET RELATION TO Order+STORE INTO (lcShipTS)
*        lcTranFile = lcShipTS
*        lcBrTtlT   = lcBrTtlS
*        lcTranBr   = [STORE :H='Store' :8:R ,] +;
*                     [nQTY1 :H='Qty1' :6:R:P='999999' ,] +;
*                     [nQTY2 :H='Qty2' :6:R:P='999999' ,] +;
*                     [nQTY3 :H='Qty3' :6:R:P='999999' ,] +;
*                     [nQTY4 :H='Qty4' :6:R:P='999999' ,] +;
*                     [nQTY5 :H='Qty5' :6:R:P='999999' ,] +;
*                     [nQTY6 :H='Qty6' :6:R:P='999999' ,] +;
*                     [nQTY7 :H='Qty7' :6:R:P='999999' ,] +;
*                     [nQTy8 :H='Qty8' :6:R:P='999999' ,] +;
*                     [nTOTQTY :H='Tot. Qty.' :10:R:P='9999999999' ,] +;
*                     [nAMOUNT :H='Amount'   :14:R:P='99999999999.99']
*    ENDCASE
*ENDCASE

*C102118,1 Always Browse lcOrdLine file in all screen modes. [Begin]
*lcOrderFile = IIF(laScrMode[2],'ORDLINE',lcOrdLine)
lcOrderFile = lcOrdLine
*C102118,1 Always Browse lcOrdLine file in all screen modes. [End]

SET ORDER TO TAG 'ORDLINST' IN (lcOrderFile)
lcOrderBr = [Style  :R :H=lcStyHdr,]+;
            [cStyGroup :H= 'Group' :R,] +;
            [Scale     :H= 'Scale' :R,]+;
            [Size1 :H= 'Size1' :P='999999':R ,] +;
            [Size2 :H= 'Size2' :P='999999':R ,] +;
            [Size3 :H= 'Size3' :P='999999':R ,] +;
            [Size4 :H= 'Size4' :P='999999':R ,] +;
            [Size5 :H= 'Size5' :P='999999':R ,] +;
            [Size6 :H= 'Size6' :P='999999':R ,] +;
            [Size7 :H= 'Size7' :P='999999':R ,] +;
            [Size8 :H= 'Size8' :P='999999':R ,] +;
            [TotSize :H= 'Total' :P='9999999':R ,] +;
            [Price     :H='Price' :R]
SELECT (lcOrderFile)
SET RELATION TO Style INTO Style
SET RELATION TO Style+laData[31]+SPACE(10) INTO StyDye ADDITIVE
*C102118,1 SSE Commented out all these Cases not used anymore. [End]

WAIT CLEAR
*-- End of lfPrepBrow.

*!*******************************************************************************
*! Name      : lfCanReason
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Read order cancellation reason
*!*******************************************************************************
*! Calls     : ARORDER.SPX
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfCanReason()
*!*******************************************************************************
*
FUNCTION lfCanReason
PRIVATE lnCanReason

STORE 1  TO lnCanReason
=gfwCodePop(@laCodes,'CCANCRESON','L')

DO (gcScrDir+"ARORDER.SPX") WITH 'X','lnCanReason'
RETURN(ALLTRIM(laCanReason[lnCanReason,2]))
*-- End of lfCanReason.

*!*******************************************************************************
*! Name      : lpDelScr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Cancel/Uncancel order
*!*******************************************************************************
*! Calls     : gfModalGen,lfGetInfo
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lpDelScr()
*!*******************************************************************************
*
FUNCTION lpDelScr
DO lfDelScr IN (gcAppHome + 'SO\SOSOCUPD.FXP')
*-- End of lpDelScr.

*!*******************************************************************************
*! Name      : lfEditAlo
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Edit alocated quantities for a specific size
*!*******************************************************************************
*! Calls     : SOALO.SPX
*!*******************************************************************************
*! Passed Parameters  : lcEditSize   : Size
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfEditAlo('1')
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfEditAlo
PARAMETERS lcEditSize
PRIVATE lnAlias,llUpdCTkt

lnAlias    = SELECT()
lcTempCurs = gfTempName()
SELECT (lcAlocated)
=AFIELDS(laFileStru)
CREATE CURSOR (lcTempCurs) FROM ARRAY laFileStru
SELECT (lcAlocated)
SCAN FOR CORDLINE = STR(&lcOrdLine..LineNo,6)
  SCATTER TO laAlocated
  INSERT INTO (lcTempCurs) FROM ARRAY laAlocated
ENDSCAN
SELECT (lcTempCurs)
LOCATE 
llUpdAlo = .F.
SET ORDER TO TAG CUTPKORD IN (lcAlocated)
DO (gcScrDir+gcWinAppl+"\SOALO.SPX")
IF llUpdAlo
  *-- Message : 32006
  *-- Cuttkt# xxxxxx status is HOLD. Do you wish to update the generated 
  *-- cuttkt quantities accordingly?
  *-- Button : 32000 
  *-- Yes/No

  *-- Message : 32007
  *-- Purchase order# xxxxxx status is HOLD. Do you wish to update the generated 
  *-- Purchase order quantities accordingly?
  *-- Button : 32000 
  *-- Yes/No
  
  *-- Message : 32008
  *-- Do you wish to update Purchase order# xxxxxx quantities accordingly?
  *-- Button : 32000 
  *-- Yes/No

  IF 'PO' $ gcCmpModules
    =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
  ENDIF
  IF 'MF' $ gcCmpModules
    =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
  ENDIF

  SELECT (lcTempCurs)
  SCAN
    =SEEK(TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE,lcAlocated)
    llUpdCTkt = (TranCd='1' .AND. SEEK(CTKTNO,'CUTTKTH') .AND. ;
                 CUTTKTH.Status = 'H' .AND. ;
                 gfModalGen('QRM32006B32000','ALERT',CTKTNO)=1) .OR. ;
                (TranCd='2' .AND. SEEK('P'+CTKTNO,'POSHDR') .AND. ;
                 POSHDR.Status='H' .AND. ;
                 gfModalGen('QRM32007B32000','ALERT',CTKTNO)=1)
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE TotCut WITH TotCut - &lcAlocated..Qty&lcEditSize + &lcTempCurs..Qty&lcEditSize
    UNLOCK
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE Cut&lcEditSize WITH Cut&lcEditSize - ;
            &lcAlocated..Qty&lcEditSize + &lcTempCurs..Qty&lcEditSize ,;
            TotCut WITH TotCut - &lcAlocated..Qty&lcEditSize + &lcTempCurs..Qty&lcEditSize
    UNLOCK
    SELECT (lcAlocated)
    =RLOCK()
    REPLACE Qty&lcEditSize WITH &lcTempCurs..Qty&lcEditSize ,;
            TotQty         WITH &lcTempCurs..TotQty 
    IF llUpdCTkt .AND. !(lcEditSize $ cUpdSizes)
      REPLACE cUpdSizes WITH ALLTRIM(cUpdSizes)+lcEditSize
    ENDIF
    IF !llUpdCTkt .AND. (lcEditSize $ cUpdSizes)
      REPLACE cUpdSizes WITH STRTRAN(cUpdSizes,lcEditSize,'')
    ENDIF
    UNLOCK
  ENDSCAN
  IF 'PO' $ gcCmpModules
    =gfCloseFile('POSHDR')
  ENDIF
  IF 'MF' $ gcCmpModules
    =gfCloseFile('CUTTKTH')
  ENDIF
ENDIF
SET ORDER TO TAG CUTORD IN (lcAlocated)
SELECT (lnAlias)
RETURN(llUpdAlo)
*-- End of lfEditAlo.

*!*******************************************************************************
*! Name      : lfDAlocated
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Deactivate function for edit allocation screen
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  : None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfDAlocated()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfDAlocated
IF WONTOP() = lcBrTtlL
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME LOCATE
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ALT+C   DO lpTap WITH 'SOALO1','pbCancel',.T.
  ON KEY LABEL TAB     DO lpTab WITH 'SOALO1','pbOk'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'SOALO1','pbCancel'
ENDIF  
RETURN .F.
*-- End of lfDAlocated.

*!*******************************************************************************
*! Name      : lfwAlocated
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Browse function for edit allocation screen
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  : None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfwAlocated()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfwAlocated
PRIVATE lcFields,lnCount,lcCount

lcFields = "cTktNo  :H='Ticket#' :R," 
=SEEK('S'+&lcOrdLine..Scale,'Scale')
FOR lnCount = 1 TO 8
  lcCount = STR(lnCount,1)
  IF lcCount = lcEditSize
    lcFields=lcFields+"Qty"+lcCount+" :H='"+Scale.Sz&lcCount+"' :V=lfvGetFld()," 
  ELSE
    lcFields=lcFields+"Qty"+lcCount+" :H='"+Scale.Sz&lcCount+"' :R,"
  ENDIF
ENDFOR
lcFields=lcFields+"TotQty :H='TotQty' :R" 
BROWSE FIELDS &lcFields ;
       WINDOW SOALO0    ;
       IN WINDOW SOALO  ;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       TITLE lcBrTtlL 
*-- End of lfwAlocated.

*!*******************************************************************************
*! Name      : lfvGetFld
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validation function for allocated quantity
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  : None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvGetFld()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfvGetFld

IF SEEK(TRANCD+CTKTNO+ORDER+STYLE+CORDLINE,lcAlocated) .AND. ;
   Qty&lcEditSize > &lcAlocated..Qty&lcEditSize
  *-- Message : 32009
  *-- The allocated quantity cannot be greater than xxxx .
  *-- Button : 00000 
  *-- Ok
  =gfModalGen('TRM32009B00000','ALERT',ALLTRIM(STR(&lcAlocated..Qty&lcEditSize)))

  REPLACE Qty&lcEditSize WITH &lcAlocated..Qty&lcEditSize
ENDIF
REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
*-- End of lfvGetFld.

*!*******************************************************************************
*! Name      : lfvUpdAlo
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validation function for Edit allocated quantity screen
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  : None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvUpdAlo()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfvUpdAlo
PRIVATE lnAloQty

SELECT (lcTempCurs)
SUM Qty&lcEditSize TO lnAloQty
LOCATE
*-- Message : 32010
*-- The allocated quantity is not equal to the ordered quantity for 
*-- size xxxxx.
*-- Button : 32001 
*-- Accept/Modify

IF lnAloQty <> m.Qty&lcEditSize .AND. ;
  gfModalGen('QRM32010B32001','ALERT',ALLTRIM(Scale.Sz&lcEditSize))=2
  RETURN
ENDIF
llUpdAlo = .T.
SELECT (lcOrdLine)
m.Qty&lcEditSize = lnAloQty
CLEAR READ
*-- End of lfvUpdAlo.

*!*******************************************************************************
*! Name      : lfvObjLink
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Call the object link screen
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvObjLink()
*!*******************************************************************************
*
FUNCTION lfvObjLink
PRIVATE lnAlais
lnAlais = SELECT()
DO GetObj WITH 'O',laData[1]
SELECT (lnAlais)
*-- End of lfvObjLink.

*!*******************************************************************************
*! Name      : lfGetDefCode
*! Developer : Wael ALy MOhamed
*! Date      : 05/14/1997
*! Purpose   : Get defaut code
*!*******************************************************************************
*! Calls       : None
*!*******************************************************************************
*! Passed Parameters : lcCodeType : Code Type
*!*******************************************************************************
*! Return      : Default code
*!*******************************************************************************
*
FUNCTION lfGetDefCode
PARAMETERS lcCodeType
PRIVATE lcCurrTag

lcCurrTag = ORDER('CODES')
SET ORDER TO TAG 'CCODE_NO' IN 'CODES'
=SEEK('D'+PADR(UPPER(lcCodeType),10),'CODES')
SET ORDER TO TAG (lcCurrTag) IN 'CODES'
RETURN(CODES.CCODE_NO)
*-- End of lfGetDefCode.

*!*******************************************************************************
*! Name      : lfvContPri
*! Developer : Wael ALy MOhamed
*! Date      : 05/14/1997
*! Purpose   : Get style contract price
*!*******************************************************************************
*! Calls       : None
*!*******************************************************************************
*! Passed Parameters : lcStyle    : Style code
*!                     lcGPrice   : Gross Price
*!                     lcNPrice   : Net Price
*!                     lcDiscount : Discount Percent
*!*******************************************************************************
*! Return      : Contract price
*!*******************************************************************************
*
FUNCTION lfvContPri
PARAMETERS lcStyle,lcGPrice,lcNPrice,lcDiscount
PRIVATE lnAlias,lnGPrice,lnNPrice,lnDiscount,llContract
lnAlias = SELECT()

lnGPrice   = &lcGPrice
lnNPrice   = &lcNPrice
lnDiscount = &lcDiscount
llContract = .F.
IF llCntrExst
  llCntrExst = .F.

  SELECT ORDHDR
  IF TYPE('lcOrderNo') <> 'C'
    SET FILTER TO 
  ENDIF
  SET ORDER TO TAG ORDACCT DESCENDING
  lcOOLnTg = ORDER('ORDLINE')
  
  SET ORDER TO TAG Ordlines IN ORDLINE
  IF SEEK(laData[2]+'C')
    SCAN REST WHILE Account+cOrdType=laData[2]+'C' ;
       FOR  !INLIST(Status,'X','B') .AND. BETWEEN(laData[8],START,COMPLETE) .AND. CCURRCODE=laData[33]
      llCntrExst = .T.
      IF SEEK(lcStyle+DTOS(OrdHdr.Complete)+'C'+OrdHdr.Order+laData[3],'OrdLine') .AND. OrdLine.Price <> 0
        lnGPrice   = OrdLine.Gros_Price
        lnNPrice   = OrdLine.Price
        lnDiscount = OrdLine.Disc_Pcnt
        llContract = .T.
        EXIT
      ENDIF
    ENDSCAN
  ENDIF
  SET ORDER TO TAG (lcOOLnTg) IN ORDLINE
  SET ORDER TO TAG ORDHDR
  IF TYPE('lcOrderNo') <> 'C'
    SET FILTER TO cOrdType+Order=lcOrdType
  ENDIF
  =SEEK(lcOrdType+laData[1])
ENDIF

SELECT (lnAlias)
&lcGPrice   = lnGPrice
&lcNPrice   = lnNPrice
&lcDiscount = lnDiscount
RETURN(llContract)
*-- End of lfvContPri.

*!*******************************************************************************
*! Name      : lfChkUnComS
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Check for uncompleted invoice sessions
*!*******************************************************************************
*! Calls     : gfUnCompSession,lfCratTemp,lfBrowOrd
*!*******************************************************************************
*! Passed Parameters  :  llFrmSetup : .T. Called from Screen setup
*!                                    .F. Called from Screen Show function
*!*******************************************************************************
*! Returns            :  llContinue : .T. Found an uncompleted session
*!                                        No uncompleted sessions Found
*!*******************************************************************************
*! Example            :  =lfChkUnComS(.F.)
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfChkUnComS
PARAMETERS llFrmSetup
PRIVATE lcObject

IF (llFrmSetup AND llChkUnCom) OR !llFrmSetup
  llGoAndChk = IIF(llFrmSetup, .F., llGoAndChk)
  llChkUnCom = .F.
  IF gfUnCompSession(lcProgID, lnSessNo, "SOSOC10")
    STORE .F. TO laScrMode
    DO CASE
      CASE lcScrMode = 'V'
        STORE .T. TO laScrMode[2]
      CASE lcScrMode = 'E'
        STORE .T. TO laScrMode[3]
      CASE lcScrMode = 'N'
        STORE .T. TO laScrMode[4]
    ENDCASE
    STORE .T. TO llContinue,llCUpDate

    SELECT IIF(lcScrMode='V','ORDLINE',lcOrdLine)
    =SEEK(lcOrdType+lcCurrOrd)

    SELECT IIF(lcScrMode='V','ORDHDR',lcOrdHdr)
    =SEEK(lcOrdType+lcCurrOrd)
    STORE IIF(laScrMode[2],'ORDHDR',lcOrdHdr) TO ;
    laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],laCodes[3,7],;
    laCodes[3,8],laCodes[4,7],laCodes[4,8],laCodes[5,7],laCodes[5,8],;
    laCodes[7,7],laCodes[7,8]
    
    lcSession = UNCMSESS.cSession

    lcObject=ALLTRIM(UnCmSess.cCurrObj)
    IF !EMPTY(lcObject)
      IF llFrmSetup AND (UPPER(lcObject)="PBSAV" .OR. UPPER(lcObject)="PBDLT")
        =IIF(UPPER(lcObject)="PBSAV",lpSavScr(),lpDelScr())
        laScrMode    = .F.
        laScrMode[1] = .T.
      ELSE
        _CUROBJ = OBJNUM(&lcObject)
        KEYBOARD "{ENTER}" PLAIN CLEAR
      ENDIF
      SHOW GETS
    ENDIF
  ENDIF
ENDIF
RETURN (llContinue)
*-- End of lfChkUnComS.

*!*******************************************************************************
*! Name      : lfCratTemp
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Create temp files
*!*******************************************************************************
*! Calls     : gfCrtTmp
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfCratTemp()
*!*******************************************************************************
*
FUNCTION lfCratTemp
PRIVATE laFileStru,lnFileStru,laIndex

*C102118,1 SSE commented out because we create it in View mode. [Begin]
*IF !laScrMode[2]
*C102118,1 SSE commented out because we create it in View mode. [End]

  SELECT ORDHDR
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]
  laFileStru[lnFileStru+1,1] = 'nSteps'
  laFileStru[lnFileStru+1,2] = 'N'
  laFileStru[lnFileStru+1,3] = 2
  laFileStru[lnFileStru+1,4] = 0
  =gfCrtTmp(lcOrdHdr,@laFileStru,[cOrdType+ORDER],lcOrdHdr)

*C102118,1 SSE commented out because we create it in View mode. [Begin]
*ENDIF
*C102118,1 SSE commented out because we create it in View mode. [End]

*C102118,1 Create Soccer Temp file [Begin]
SELECT Soccer
=AFIELDS(laFileStru)
DECLARE laIndex[1,2]
laIndex[1,1] = 'Div+Team'
laIndex[1,2] = 'Soccer'
=gfCrtTmp(lcSocTemp,@laFileStru,@laIndex)
SELECT (lcSocTemp)
USE (gcWorkDir+lcSocTemp) EXCLUSIVE
SET ORDER TO TAG 'Soccer' IN (lcSocTemp)
*C102118,1 Create Soccer Temp file [End]

SELECT ORDLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)

*C102118,1 Increase the dimension of the array [Begin]
*DIMENSION laFileStru[lnFileStru+2,4]
DIMENSION laFileStru[lnFileStru+17,4]
*C102118,1 Increase the dimension of the array [End]

laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'lContract'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0

*C102118,1 Add additional 4 fields to lcOrdLine file. [Begin]
laFileStru[lnFileStru+3,1] = 'Youth'
laFileStru[lnFileStru+3,2] = 'N'
laFileStru[lnFileStru+3,3] = 6
laFileStru[lnFileStru+3,4] = 0

laFileStru[lnFileStru+4,1] = 'Adult'
laFileStru[lnFileStru+4,2] = 'N'
laFileStru[lnFileStru+4,3] = 6
laFileStru[lnFileStru+4,4] = 0

laFileStru[lnFileStru+5,1] = 'YouthPr'
laFileStru[lnFileStru+5,2] = 'N'
laFileStru[lnFileStru+5,3] = 12
laFileStru[lnFileStru+5,4] = 2

laFileStru[lnFileStru+6,1] = 'AdultPr'
laFileStru[lnFileStru+6,2] = 'N'
laFileStru[lnFileStru+6,3] = 12
laFileStru[lnFileStru+6,4] = 2

laFileStru[lnFileStru+7,1] = 'cStyGroup'
laFileStru[lnFileStru+7,2] = 'C'
laFileStru[lnFileStru+7,3] = 6
laFileStru[lnFileStru+7,4] = 0

laFileStru[lnFileStru+8,1] = 'Size1'
laFileStru[lnFileStru+8,2] = 'N'
laFileStru[lnFileStru+8,3] = 6
laFileStru[lnFileStru+8,4] = 0

laFileStru[lnFileStru+9,1] = 'Size2'
laFileStru[lnFileStru+9,2] = 'N'
laFileStru[lnFileStru+9,3] = 6
laFileStru[lnFileStru+9,4] = 0

laFileStru[lnFileStru+10,1] = 'Size3'
laFileStru[lnFileStru+10,2] = 'N'
laFileStru[lnFileStru+10,3] = 6
laFileStru[lnFileStru+10,4] = 0

laFileStru[lnFileStru+11,1] = 'Size4'
laFileStru[lnFileStru+11,2] = 'N'
laFileStru[lnFileStru+11,3] = 6
laFileStru[lnFileStru+11,4] = 0

laFileStru[lnFileStru+12,1] = 'Size5'
laFileStru[lnFileStru+12,2] = 'N'
laFileStru[lnFileStru+12,3] = 6
laFileStru[lnFileStru+12,4] = 0

laFileStru[lnFileStru+13,1] = 'Size6'
laFileStru[lnFileStru+13,2] = 'N'
laFileStru[lnFileStru+13,3] = 6
laFileStru[lnFileStru+13,4] = 0

laFileStru[lnFileStru+14,1] = 'Size7'
laFileStru[lnFileStru+14,2] = 'N'
laFileStru[lnFileStru+14,3] = 6
laFileStru[lnFileStru+14,4] = 0

laFileStru[lnFileStru+15,1] = 'Size8'
laFileStru[lnFileStru+15,2] = 'N'
laFileStru[lnFileStru+15,3] = 6
laFileStru[lnFileStru+15,4] = 0

laFileStru[lnFileStru+16,1] = 'TotSize'
laFileStru[lnFileStru+16,2] = 'N'
laFileStru[lnFileStru+16,3] = 7
laFileStru[lnFileStru+16,4] = 0

laFileStru[lnFileStru+17,1] = 'lRemoveLin'
laFileStru[lnFileStru+17,2] = 'L'
laFileStru[lnFileStru+17,3] = 1
laFileStru[lnFileStru+17,4] = 0
*C102118,1 Add additional 4 fields to lcOrdLine file. [End]

DECLARE laIndex[3,2]
laIndex[1,1] = 'cOrdType+ORDER+STORE+STYLE+STR(LINENO,6)'
laIndex[1,2] = 'ORDLINST'
laIndex[2,1] = 'cOrdType+ORDER+STYLE+STORE+STR(LINENO,6)'
laIndex[2,2] = 'ORDLINES'
laIndex[3,1] = 'cOrdType+ORDER+STR(LINENO,6)'
laIndex[3,2] = 'ORDLINE'
=gfCrtTmp(lcOrdLine,@laFileStru,@laIndex)

*C102118,1 Open File exclusive to allow Zapping. [Begin]
SELECT (lcOrdLine)
USE (gcWorkDir+lcOrdLine) EXCLUSIVE
*C102118,1 Open File exclusive to allow Zapping. [End]

SET ORDER TO TAG 'ORDLINE' IN (lcOrdLine)
SCATTER MEMVAR BLANK MEMO

*C102118,1 Create lcOrdLin2 Temp file [Begin]
=gfCrtTmp(lcOrdLin2,@laFileStru,@laIndex)
SELECT (lcOrdLin2)
USE (gcWorkDir+lcOrdLin2) EXCLUSIVE
SET ORDER TO TAG 'ORDLINE' IN (lcOrdLin2)
*C102118,1 Create lcOrdLin2 Temp file [End]

SELECT OrdLine
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'lContract'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
DECLARE laIndex[3,2]
laIndex[1,1] = 'Flag+ORDER+STR(LINENO,6)'
laIndex[1,2] = lcTempline
laIndex[2,1] = 'cOrdType+ORDER+STORE+STYLE+STR(LINENO,6)'
laIndex[2,2] = 'ORDLINST'
laIndex[3,1] = 'cOrdType+ORDER+STR(LINENO,6)'
laIndex[3,2] = 'ORDLINE'
=gfCrtTmp(lcTempline,@laFileStru,@laIndex)

=gfOpenFile(gcDataDir+'BomVar',gcDataDir+'BomVar','SH')
SELECT BomVar
=AFIELDS(laBomVarSt)
lnBomVarSt = ALEN(laBomVarSt,1)
DIMENSION laBomVarSt[lnBomVarSt + 2 , 4]
laBomVarSt[lnBomVarSt+1,1] = 'nRecno'
laBomVarSt[lnBomVarSt+1,2] = 'N'
laBomVarSt[lnBomVarSt+1,3] = 10
laBomVarSt[lnBomVarSt+1,4] = 0
laBomVarSt[lnBomVarSt+2,1] = 'cStatus'
laBomVarSt[lnBomVarSt+2,2] = 'C'
laBomVarSt[lnBomVarSt+2,3] = 1
laBomVarSt[lnBomVarSt+2,4] = 0
=gfCrtTmp(lcT_BomVar,@laBomVarSt,[cIdType+cCost_Id+STR(LineNo,6)],lcT_BomVar)
IF USED(lcT_BomVar)
  USE IN (lcT_BomVar)
ENDIF
=gfOpenFile(gcWorkDir+lcT_BomVar,gcWorkDir+lcT_BomVar,'EX')

*C102118,1 SSE commented out because we create it in View mode. [Begin]
*IF laScrMode[3]
*C102118,1 SSE commented out because we create it in View mode. [End]

  =gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH')
  =AFIELDS(laFileStru)
  =gfCrtTmp(lcOrdCanLn,@laFileStru,[CORDTYPE+ORDER+STR(LINENO,6)],lcOrdCanLn)
  =gfCloseFile('ORDCANLN')

*C102118,1 SSE commented out because we create it in View mode. [Begin]
*ENDIF
*C102118,1 SSE commented out because we create it in View mode. [End]

*-- End of lfCratTemp.

*!*******************************************************************************
*! Name      : lfGetprice
*! Developer : WAM
*! Date      : 02/22/1999
*! Purpose   : Get Style Price
*!*******************************************************************************
*! Calls     : lfCheckPri()
*!*******************************************************************************
*! Passed Parameters  :  lcStyle : Style
*!                       lcLevel : Price level
*!                       lnQuantity : invoice Quantity
*!*******************************************************************************
*! Returns            :  Alternative price
*!*******************************************************************************
*! Example            :  =lfGetprice(m.Style,'A',100)
*!*******************************************************************************
*
FUNCTION lfGetprice
PARAMETERS lcStyle,lcLevel,lnQuantity
PRIVATE lnPrice

=SEEK(lcStyle,'Style')
IF lcLevel = 'Q'
  DO CASE
    CASE Style.nAtQtyC > 0 AND lnQuantity > Style.nAtQtyC
      lcLevel = 'C'
    CASE Style.nAtQtyB > 0 AND lnQuantity >Style.nAtQtyB
      lcLevel = 'B'
    OTHERWISE
      lcLevel = 'A'
  ENDCASE
ELSE
  lcLevel=IIF(INLIST(lcLevel,'A','B','C'),lcLevel,'A')
ENDIF
lnPrice = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.Price&lcLevel,;
                      gfStyPrice(lcStyle,lcLevel,laData[33]))
RETURN(IIF(lnPrice=0,lfCheckPri(lcStyle,lcLevel),lnPrice))
*-- End of lfGetprice.

*!*******************************************************************************
*! Name      : lfCheckPri
*! Developer : WAM
*! Date      : 02/22/1999
*! Purpose   : Select alternative price level
*!*******************************************************************************
*! Calls     : gfStyPrice(),SOSTYPRI.SPX
*!*******************************************************************************
*! Passed Parameters  :  lcStyle : Style
*!                       lcLevel : Price level
*!*******************************************************************************
*! Returns            :  Alternative price
*!*******************************************************************************
*! Example            :  =lfCheckPri('A')
*!*******************************************************************************
*
FUNCTION lfCheckPri
PARAMETERS lcStyle,lcLevel
PRIVATE lcTitle,lcPrompt,lcPrompt1,lcPrompt2,lcPrompt3,lnPrice,lnprice1,;
        lnPrice2,lnPrice3

=SEEK(lcStyle,'Style')
lcTitle  = PROPER(lcStyHdr)+': '+lcStyle
lnPrice1 = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.PriceA,;
                       gfStyPrice(lcStyle,'A',laData[33]))
lnPrice2 = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.PriceB,;
                       gfStyPrice(lcStyle,'B',laData[33]))
lnPrice3 = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.PriceC,;
                       gfStyPrice(lcStyle,'C',laData[33]))
lnPrice   = IIF(lnPrice1 >0,1,IIF(lnPrice2 >0,2,AT(lcLevel,'ABC')))
lcPrompt  = "Price level '"+lcLevel+"' is zero. Proceed with"
lcPrompt1 = 'Price level A, '+ALLTRIM(STR(lnPrice1,12,2))
lcPrompt2 = 'Price level B, '+ALLTRIM(STR(lnPrice2,12,2))
lcPrompt3 = 'Price level C, '+ALLTRIM(STR(lnPrice3,12,2))

DO (gcScrDir+"SOSTYPRI.SPX")
RETURN(EVAL('lnPrice'+STR(lnPrice,1)))
*-- End of lfCheckPri.

*!*******************************************************************************
*! Name      : lfvalSty
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Purpose   : Validate the entered style 
*!*******************************************************************************
*! Calls     : ...
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   : None 
*!*******************************************************************************
*! Example   : =lfvalSty()
*!*******************************************************************************
*
FUNCTION lfvalSty

m.Style = &lcTmpSty..Style
IF (&lcTmpSty..cDivision <> laData[15] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;
   .OR. (ALLTRIM(laData[14])<>'*' AND TRIM(&lcTmpSty..Season)<>'Y' AND &lcTmpSty..Season<>laData[14] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','season '+ALLTRIM(laSeasons[lnSeason,1]))=1) ;
   .OR. (!EMPTY(&lcTmpSty..Start) .AND. &lcTmpSty..Start > laData[9] .AND. ;
        gfModalGen('QRM32021B32003','ALERT','start|'+DTOC(Style.Start))=2) ;
   .OR. (!EMPTY(&lcTmpSty..SoldOut) .AND. &lcTmpSty..SoldOut < laData[9] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2)
  RETURN .F.
ENDIF
m.lContract = lcOrdType<>'C' .AND. lfvContPri(m.Style,'m.Gros_Price','m.Price','m.Disc_Pcnt')
IF !m.lContract
  m.Gros_Price = lfGetprice(m.Style,lcPriceLvl,m.TotQty)
ENDIF
  
IF m.Gros_Price < 0
  RETURN .F.
ENDIF
IF laSetups[5,2]='Y' .AND. !SEEK(m.Style+laData[31]+SPACE(10),'StyDye')
  *-- Message : 40012
  *-- Style/color xxx is not assigned to warehouse xxx
  *-- Button : 40002
  *-- Add Reenter
  IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.Style)+'|'+TRIM(laData[31]))=1
    DO gpAdStyWar WITH m.Style,SPACE(10),laData[31]
  ELSE
    RETURN .F.
  ENDIF
ENDIF
m.Desc1    = &lcTmpSty..Desc1
m.Scale    = &lcTmpSty..Scale
m.PrePak   = &lcTmpSty..PrePak
m.Comm1    = IIF(&lcTmpSty..Commission,laData[28],0)
m.Comm2    = IIF(&lcTmpSty..Commission,laData[30],0) 
m.Season   = &lcTmpSty..Season
m.Gl_Sales = ALLTRIM(laData[53])+&lcTmpSty..cSlsGlLink
m.Gl_Sales = IIF(laSetups[4,2]='Y' AND SEEK('02'+m.Gl_Sales,'Gl_Link'),m.Gl_Sales,'DEFDEF')

IF !m.lContract
  *-- Add new validation on the discount code up on 
  *-- add new option about whole sale or retail sale
  *-- get the cDiscCode From stydye in every case
  lcDiscCode = IIF(SEEK(m.Style+laData[31]+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
  m.Disc_Pcnt = 0
  IF !EMPTY(ALLTRIM(lcDiscCode))
    *-- Get the disecound related filed to now which 
    *-- type whole Sale Or Retail sale Or Both.
    DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
    STORE '' To lcDisType , ldstartDte ,ldEndDate
    *-- Array to get the Discount affect for DecCode.
    laDisType[1,1]  = 'CCOSTAFECT'
    laDisType[1,2]  = 'lcDisType'
    *-- Array to get the start date For DescCode.
    lastartDte[1,1] = 'START'
    lastartDte[1,2] = 'ldstartDte'
    *-- Array to get the end date For DescCode.
    laEndDate[1,1]  = 'DENDATE'
    laEndDate[1,2]  = 'ldEndDate'
    = gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
    = gfRltFld(lcDiscCode, @lastartDte, 'CDISCCODE')
    = gfRltFld(lcDiscCode , @laEndDate, 'CDISCCODE')
    IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[8],ldstartDte,ldEndDate)
      lnDisc_Pcnt = m.Disc_Pcnt
      =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
      m.Disc_Pcnt = lnDisc_Pcnt
    ENDIF
  ENDIF  
  m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100      
ENDIF
m.Account   = laData[2]
m.Order     = laData[1]
m.cOrdType  = lcOrdType
m.cWareCode = laData[31]
*-- End of lfvalSty.

*!*******************************************************************************
*! Name      : lfChkTQty
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 05/04/1999
*! Purpose   : Check total pieces againest total quantity entered
*!*******************************************************************************
*! Calls     : gfModalGen(),lfvNPrice()
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfChkTQty()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfChkTQty
PARAMETERS llStores

*-- Message : 32031
*-- Quantity out of balance! xxx pieces ok
*-- Button : 32000 
*-- Yes NO
IF m.TotQty <> m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8 AND ;
  gfModalGen('QRM32031B32000','ALERT',STR(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,5))=2
  RETURN(.F.)
ENDIF
IF M.QTY1 # laOldQty[1] .OR. M.QTY2 # laOldQty[2] .OR. M.QTY3 # laOldQty[3] ;
 .OR. M.QTY4 # laOldQty[4] .OR. M.QTY5 # laOldQty[5] .OR. M.QTY6 # laOldQty[6] ;
 .OR. M.QTY7 # laOldQty[7] .OR. M.QTY8 # laOldQty[8] .OR. M.TOTQTY # laOldQty[9]
  
  STORE .T. TO llCUpdate
  m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
  *-- Get style price according to ordered quantity
  IF !m.lContract AND lcPriceLvl='Q'
    *-- Select alternative price level
    m.Gros_Price = lfGetprice(m.Style,lcPriceLvl,m.TotQty)
    m.Price = m.Gros_Price*(100-m.Disc_Pcnt)/100
  ENDIF

  IF llStores
    lnTotPcs = lnTotPcs - laOldQty[9]+m.TotQty
    lnTotAmt = lnTotAmt - laOldQty[9]*Price + m.TotQty*m.Price
  ELSE
    IF laScrMode[4] AND !EMPTY(cFromOrder) .AND. SEEK(STR(BulkLineNo,6),lcBulkOrd)
      SELECT (lcBulkOrd)
      =RLOCK()
      REPLACE USED1   WITH USED1 - &lcOrdLine..Qty1 + m.Qty1 ,;
              USED2   WITH USED2 - &lcOrdLine..Qty2 + m.Qty2 ,;
              USED3   WITH USED3 - &lcOrdLine..Qty3 + m.Qty3 ,;
              USED4   WITH USED4 - &lcOrdLine..Qty4 + m.Qty4 ,;
              USED5   WITH USED5 - &lcOrdLine..Qty5 + m.Qty5 ,;
              USED6   WITH USED6 - &lcOrdLine..Qty6 + m.Qty6 ,;
              USED7   WITH USED7 - &lcOrdLine..Qty7 + m.Qty7 ,;
              USED8   WITH USED8 - &lcOrdLine..Qty8 + m.Qty8 ,;
              TOTUSED WITH Used1+Used2+Used3+Used4+Used5+Used6+Used7+Used8
      UNLOCK
      SELECT(lcTmpSize)
    ENDIF
    =SEEK(cOrdType+Order+STR(LineNo,6),'OrdLine')
    laData[35] = laData[35] - TotBook
    laData[36] = laData[36] - TotBook*Price
    =RLOCK()
    REPLACE Book1   WITH MAX(OrdLine.Book1-OrdLine.Qty1+m.Qty1,IIF(llUpdBook,0,OrdLine.Book1))  ,;
            Book2   WITH MAX(OrdLine.Book2-OrdLine.Qty2+m.Qty2,IIF(llUpdBook,0,OrdLine.Book2))  ,;
            Book3   WITH MAX(OrdLine.Book3-OrdLine.Qty3+m.Qty3,IIF(llUpdBook,0,OrdLine.Book3))  ,;
            Book4   WITH MAX(OrdLine.Book4-OrdLine.Qty4+m.Qty4,IIF(llUpdBook,0,OrdLine.Book4))  ,;
            Book5   WITH MAX(OrdLine.Book5-OrdLine.Qty5+m.Qty5,IIF(llUpdBook,0,OrdLine.Book5))  ,;
            Book6   WITH MAX(OrdLine.Book6-OrdLine.Qty6+m.Qty6,IIF(llUpdBook,0,OrdLine.Book6))  ,;
            Book7   WITH MAX(OrdLine.Book7-OrdLine.Qty7+m.Qty7,IIF(llUpdBook,0,OrdLine.Book7))  ,;
            Book8   WITH MAX(OrdLine.Book8-OrdLine.Qty8+m.Qty8,IIF(llUpdBook,0,OrdLine.Book8))  ,;
            TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8 ,;
            FLAG WITH IIF(FLAG='N','N','M')
    UNLOCK
    
    *-- Store order line calceled quantity
    IF Flag <> 'N' AND !llUpdBook
      IF OrdLine.Qty1 > m.Qty1 OR OrdLine.Qty2 > m.Qty2 OR OrdLine.Qty3 > m.Qty3 OR ;
         OrdLine.Qty4 > m.Qty4 OR OrdLine.Qty5 > m.Qty5 OR OrdLine.Qty6 > m.Qty6 OR  ;
         OrdLine.Qty7 > m.Qty7 OR OrdLine.Qty8 > m.Qty8
        =gfwCodePop(@laCodes,'CCANCRESON','D')
        INSERT INTO (lcOrdCanLn) ;
        (cOrdType,Order,LineNo,Cancelled,cCancReson) VALUES ;
        (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2])
      ENDIF
      IF SEEK(lcOrdType+laData[1]+STR(m.LineNo,6),lcOrdCanLn)
        SELECT (lcOrdCanLn)
        REPLACE Qty1 WITH MAX(OrdLine.Qty1- m.Qty1,0) ,;
                Qty2 WITH MAX(OrdLine.Qty2- m.Qty2,0) ,;
                Qty3 WITH MAX(OrdLine.Qty3- m.Qty3,0) ,;
                Qty4 WITH MAX(OrdLine.Qty4- m.Qty4,0) ,;
                Qty5 WITH MAX(OrdLine.Qty5- m.Qty5,0) ,;
                Qty6 WITH MAX(OrdLine.Qty6- m.Qty6,0) ,;
                Qty7 WITH MAX(OrdLine.Qty7- m.Qty7,0) ,;
                Qty8 WITH MAX(OrdLine.Qty8- m.Qty8,0) ,;
                TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        REPLACE ACCOUNT WITH ORDLINE.ACCOUNT,;
                STYLE   WITH ORDLINE.STYLE,;
                STORE   WITH ORDLINE.STORE,;
                DYELOT  WITH ORDLINE.DYELOT
        IF TotQty > 0
          STORE lcOrdCanLn TO laCodes[6,7],laCodes[6,8]
          STORE 'lcOrdType+laData[1]+STR(m.LineNo,6)' TO laCodes[6,9]
          =gfwCodePop(@laCodes,'CCANCRESON','T')
          STORE Cancelled TO m.Cancelled
          DO (gcScrDir+"SOORDCLN.SPX")
          STORE '' TO laCodes[6,7],laCodes[6,8],laCodes[6,9]
        ELSE
          DELETE
        ENDIF
        SELECT(lcTmpSize)
      ENDIF
    ENDIF
        
    laData[35] = laData[35] + TotBook
    laData[36] = laData[36] + TotBook*m.Price

    IF FLAG <> 'N' AND !llUpdBook
      laData[39] = MAX(laData[39] + MIN(laOldQty[9],OrdLine.TotQty)-m.TotQty,0)
      laData[40] = MAX(laData[40] + (MIN(laOldQty[9],OrdLine.TotQty)-m.TotQty)*Price,0)
    ENDIF  
    laData[41] = laData[41] - laOldQty[9]+m.TOtQty
    laData[42] = laData[42] - laOldQty[9]*Price+ m.TOtQty*m.Price

    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE BOOK      WITH laData[35] ,;
            BOOKAMT   WITH laData[36] ,;
            CANCEL    WITH laData[39] ,;
            CANCELAMT WITH laData[40] ,;
            OPEN      WITH laData[41] ,;
            OPENAMT   WITH laData[42]
    UNLOCK
    SELECT(lcTmpSize)
  ENDIF

  =RLOCK()
  GATHER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
                       Gros_Price,Price
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOldQty
  UNLOCK
  SCATTER MEMVAR FIELDS Book1,Book2,Book3,Book4,Book5,Book6,Book7,Book8,TotBook
ENDIF
SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
*-- End of lfChkTQty.

*!*******************************************************************************
*! Name      : lfvOrdCatg
*! Developer : TAK
*! Date      : 06/08/1999
*! Purpose   : Valid order category code.
*!*******************************************************************************
*! Calls     : CODECHK
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvOrdCatg()
*!*******************************************************************************
*
FUNCTION lfvOrdCatg

IF laData[52] <> SUBSTR(laOCtgCd[lnOCtgCd,2],1,6)
  laData[52] = SUBSTR(laOCtgCd[lnOCtgCd,2],1,6)
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE cOrderCat WITH laData[52]
  UNLOCK
  llCUpdate = .T.
ENDIF
RETURN
*-- End of lfvOrdCatg.

*!*******************************************************************************
*! Name      : lfvBuyer
*! Developer : TAK
*! Date      : 06/10/1999
*! Purpose   : Validate or view buyer(s).
*!*******************************************************************************
*! Passed Parameters  : llByView-> .T. View , .F. Select/Add
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvBuyer()
*!*******************************************************************************
*
FUNCTION lfvBuyer
PARA llByView

lcContKey = 'C'+PADR(laData[2],8)+PADR(laData[3],8)
IF llByView
  lcOnKyEnr=ON('KEY','ENTER')
  =gfOpenFile(gcDataDir+'CONTACT','CONTACT','SH')
  SET KEY TO lcContKey
  LOCATE
  IF !EOF() 
    ON KEY LABEL ENTER DO lpSelBuyer
    DEFINE POPUP lcByrView FROM 3,10.625 TO 12.75,41.3 IN WINDOW (lcWinCh4) ;
	       PROMPT FIELD CONTACT SCROLL
    ACTIVATE POPUP lcByrView
    ON KEY LABEL ENTER &lcOnKyEnr
  ELSE
    _CUROBJ = OBJNUM(laData[20])
  ENDIF
  SET KEY TO 
  =lfUpdate()
ELSE
  IF !EMPTY(laData[20])
    =gfOpenFile(gcDataDir+'CONTACT','CONTACT','SH')
    IF SEEK(lcContKey)
      LOCATE REST WHILE cContType+cCont_id+Store = lcContKey FOR ALLT(UPPER(contact))= ALLT(UPPER(laData[20]))
      llNotExt = !FOUND()
    ELSE
      llNotExt = .T.
    ENDIF
    IF llNotExt AND gfModalGen('QRM32068B32000','ALERT',ALLTRIM(laData[20]))=1
      *--M32068 -- Buyer : xxxxxxxxx does not exist in this customer contracts,
      *--          Do you wish to Add this contract?
      *--B32000 -- Yes / No.
       APPEND BLANK
       REPLACE CCONTTYPE WITH 'C',;
               CCONT_ID  WITH laData[2],;
               STORE     WITH laData[3],;
               CONTACT   WITH laData[20],;
               CCONTTTL  WITH laData[20]
       =gfTraceKey('CONTACT',cContType+cCont_Id+Store+Contact,'M')
    ENDIF
    =lfUpdate()  
  ENDIF
ENDIF
RETURN
*-- End of lfvBuyer.

*!*******************************************************************************
*! Name      : lpSelBuyer
*! Developer : TAK
*! Date      : 06/10/1999
*! Purpose   : Select buyer(s).
*!*******************************************************************************
*
FUNCTION lpSelBuyer

LADATA[20] = CONTACT
SHOW GET LADATA[20]
KEYBOARD '{ESC}'
RETURN
*-- End of lpSelBuyer.

*!*******************************************************************************
*! Name      : lfvConfig
*! Developer : Reham Al-Allamy
*! Date      : 07/04/1999
*! Purpose   : Valid function of the <Configure> push button.
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvConfig()
*!*******************************************************************************
*C102118,1 This function in Detail Folder is not used anymore.
*
FUNCTION lfvConfig
PRIVATE lcCrOrdAlias

IF !EMPTY(m.Style)
  *-- Save current alias.
  lcCrOrdAlias = ALIAS()
  
  *-- Clear the trapped keys.
  PUSH KEY
  ON KEY
  
  *-- Variable hold the order price.
  IF llMulCurr AND laData[34]<>1 AND laData[34]<>0
    lcUnMeth   ='/'
    lcMethod   = gfGetExSin(@lcUnMeth,laData[33])
    lcOpsPMet = IIF(lcMethod='/','*','/')
    lcOpsPUMt = IIF(lcUnMeth='*','/','*')
  ENDIF
  lnRetPrice = m.Gros_Price
  lnGrossPrice = lnRetPrice
 
  *-- Call a dialog screen to display the styles positions & the variant cost sheet.
  PUSH MENU _MSYSMENU
  RELEASE PAD _INQUIRY OF _MSYSMENU
  DO  (gcAppHome+'SODS_ND.FXP') WITH "SO" , laData[1] , lcT_BomVar , m.Style , m.LineNo , lnGrossPrice , laData[2] , m.Store , lnRetPrice
  POP MENU _MSYSMENU
  
  *-- Restore the trapped keys.
  POP KEY
  *-- Restore current alias.
  SELECT (lcCrOrdAlias)
  
  *C102118,1 SSE Commented out because it is Removed from screen [Begin]
  *lcConfig = IIF(SEEK("SO" + &lcOrderFile..Order + STR(&lcOrderFile..LineNo,6) , lcT_BomVar) , "\<Adornment" , "\<Configure")
  *SHOW GET pbConfig,1 PROMPT lcConfig ENABLE
  *C102118,1 SSE Commented out because it is Removed from screen [End]

  *-- If there is any added price has added to the sales order line.
  IF m.Gros_Price <> lnRetPrice
    *-- Replace the order line price with the new returned price.
    m.Gros_Price = lnRetPrice
    m.Price      = m.Gros_Price*(100-m.Disc_Pcnt)/100

    *C102118,1 SSE Commented out because it is Removed from screen [Begin]
    *SHOW GET m.Gros_Price
    *C102118,1 SSE Commented out because it is Removed from screen [End]
    
    SHOW GET m.Price
    
    *-- Call valid function for the gross price.
    =lfvGPrice()

    *C102118,1 SSE Commented out because they are Removed from screen [Begin]    
    *SHOW GET m.Gros_Price
    *C102118,1 SSE Commented out because they are Removed from screen [End]    
  ENDIF
ENDIF
*-- End of lfvConfig.

*!*******************************************************************************
*! Name      : lfChMSHV
*! Developer : Ahmed Ibrahim
*! Date      : 08/18/1999
*! Purpose   : Function to change the title "ALL" of the ship via popup 
*!             to be "At Store Level"
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfChMSHV()
*!*******************************************************************************
*
FUNCTION lfChMSHV
FOR lnI = 1 TO ALEN(laShipVia,1)
  IF laShipVia[lnI,1] = PADR('ALL',30)
    laShipVia[lnI,1] = 'At Store Level'
    EXIT
  ENDIF
ENDFOR
SHOW GET lnShipVia
*-- End of lfChMSHV.

*!*******************************************************************************
*! Name      : lfvCompln
*! Developer : Ahmed Ibrahim
*! Date      : 08/18/1999
*! Purpose   : Valid function of complete date in order line
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvCompln()
*!*******************************************************************************
*C102118,1 This function in Detail Folder is not used anymore.
*
FUNCTION lfvCompln
PARAMETERS llStores
IF m.Complete <> EVAL(IIF(llStores,lcTempline,lcOrdLine)+'.Complete')
  IF laData[9] > m.Complete
    *-- Message : 32013
    *-- The starting date cannot be greater than the completion date.
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32013B00000','ALERT')
    m.Complete = EVAL(IIF(llStores,lcTempline,lcOrdLine)+'.Complete')
    RETURN
  ENDIF
  IF  gcContCode # 'EGYPT'
    IF CDOW(m.Complete)='Saturday' .OR. CDOW(m.Complete)='Sunday'
      *-- Message : 32014
      *-- The completion date is a xxxxx.
      *-- Button : 00000
      *-- Ok
      =gfModalGen('INM32014B00000','ALERT',CDOW(m.Complete))
    ENDIF
  ENDIF
  llCUpdate = .T.
  SELECT (IIF(llStores,lcTempline,lcOrdLine))
  =RLOCK()
  REPLACE Complete WITH m.Complete,;
          FLAG     WITH IIF(FLAG <> 'N','M',FLAG)
  UNLOCK
ENDIF
*-- End of lfvCompln.

*!*******************************************************************************
*! Name      : lfvBrows
*! Developer : Ahmed Ibrahim
*! Date      : 09/09/1999
*! Purpose   : Valid Browse function.
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfvBrows()
*!*******************************************************************************
*
FUNCTION lfvBrows
IF WONTOP() # (lcOrdBrow) AND WONTOP() # (lcBrTtlZ) .AND. WONTOP() # (lcBrTtlT)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- End of lfvBrows.

*!*******************************************************************************
*! Name      : lfIncSearch
*! Developer : Ahmed Ibrahim
*! Date      : 09/22/1999
*! Purpose   : Incremental search function
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfIncSearch()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfIncSearch

*-- clear trapping
PUSH KEY
ON KEY
ON KEY LABEL F1 llDumi = .T.
PRIVATE lnBrRecNO,lcOrgTag

CLEAR TYPEAHEAD
*-- get the key was pressed
lcExpToSeek = UPPER(CHR(LASTKEY()))

*-- if the key was pressed is one of "esc,enter,tab,shift+tab" do nothing
*-- else run the incremental search screen
IF !INLIST(LASTKEY(),27,13,9,15)
    DEFINE WINDOW lwIncSrch ;
      AT 21.000, 10.000 ;
      SIZE 4.167,55.444 ;
      FONT "FoxFont", 9 ;
      STYLE "B" TITLE "Aria Apparel System" ;
      FLOAT NOCLOSE SHADOW NOMINIMIZE NONE COLOR RGB(,,,192,192,192)

    ACTIVATE WINDOW lwIncSrch NOSHOW

  CLEAR TYPEAHEAD
  KEYBOARD "{END}"  && go to the end of the edit field
  DO lfActInSr
ENDIF
POP KEY
*-- reinitialize the trapping
=lfDStores()
*-- End of lfIncSearch.

*!*******************************************************************************
*! Name      : lfActInSr
*! Developer : Ahmed Ibrahim
*! Date      : 09/22/1999
*! Purpose   : function to get the value of the inc. search and seek in file
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfActInSr()
*!*******************************************************************************
*This function is not used anymore
FUNCTION lfActInSr
* initialize the get field
@ 1.500,14.333 GET lcExpToSeek ;
  SIZE 1.167,41.000 PICTURE '@!' FONT "FoxFont", 9 MESSAGE "Press  'ENTER'  To  Locate"
	
@ 1.334,14.111 TO 1.334,51.115 PEN 2, 8 STYLE "1" ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 2.667,14.222 TO 2.667,51.115 PEN 2, 8 STYLE "1" ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 1.417,14.111 TO 2.750,14.111 PEN 2, 8 ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 1.417,50.893 TO 2.750,50.893 PEN 2, 8 ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 1.417,4.333 SAY "Locate         :"  FONT "MS Sans Serif", 8 ;
  STYLE "B"
	
@ 0.000,0.000 TO 0.000,55.444 PEN 1, 8 STYLE "1" ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.333,0.444 TO 0.333,55.000 PEN 1, 8 STYLE "1" ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 4.083,0.000 TO 4.083,55.444 PEN 1, 8 STYLE "1" ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 3.750,0.444 TO 3.750,55.000 PEN 1, 8 STYLE "1" ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.000,0.000 TO 4.167,0.000 PEN 1, 8 ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.333,0.444 TO 3.833,0.444 PEN 1, 8 ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 0.333,54.889 TO 3.833,54.889 PEN 1, 8 ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.000,55.333 TO 4.167,55.333 PEN 1, 8 ;
  COLOR RGB(128,128,128,128,128,128)

ACTIVATE WINDOW lwIncSrch TOP
READ MODAL
RELEASE WINDOW lwIncSrch
IF llFromPack
  lcExactExp = lcExpToSeek
ENDIF
lcExpToSeek = ALLTRIM(lcExpToSeek)

*-- if the user exit the window by using the enter
IF LASTKEY() = 13
  *-- get the current record #
  lnBrRecNO = IIF(RECNO()>RECCOUNT(),0,RECNO())
  *-- if the used tag is not the tag the programer want to use in
  *-- the inc. search function then change the active order to use
  *-- the programer's tag
  IF TYPE("lcSeekTag")='C' AND !EMPTY(lcSeekTag)
    lcOrgTag = SYS(22)
    lcOrgExp = SYS(14,VAL(SYS(21)))
    SET ORDER TO TAG (lcSeekTag)
  ENDIF
  
  *-- If the index expression is a compound index then seek
  *-- One by one of the value the user want to find until the end of 
  *-- the expression or value not found in the file  
   FOR lnCount = 1 TO ALEN(laKeyExp,1)
    *-- If the value found in the file then go to the record that have this value
    *-- If called from Packs screen seek the generic and entered accounts.
    IF llFromPack
      lcExpToSeek = RTRIM(lcExactExp)
      lcGenAcct = '*****'
      lcActKey  = IIF(PACK_ID = PADR(lcExpToSeek,16) ,IIF(ACCOUNT = '*****',laData[2],'*****'),laData[2])
      laKeyExp[1] = IIF(PACK_ID = PADR(lcExpToSeek,16) ,IIF(ACCOUNT = '*****','laData[2]','lcGenAcct'),'lcGenAcct')
    ENDIF
    IF SEEK(&lakeyExp[lnCount]+lcExpToSeek)  OR ;
       (llFromPack AND SEEK(lcActKey+lcExpToSeek))
      IF TYPE("lcOrgTag")='C'
        lcOrgKey = &lcOrgExp
        SET ORDER TO TAG lcOrgTag
        
        IF !EMPTY(lcOrgTag)
          SEEK lcOrgKey
        ENDIF  
      ENDIF
      
      CLEAR TYPEAHEAD
      RETURN
    *-- else if the value does not exist in the file then go back to the previous record  
    ELSE
      IF TYPE("lcOrgTag")='C'
        SET ORDER TO TAG lcOrgTag
      ENDIF
      IF lnBrRecNO>0
        GO lnBrRecNO
      ENDIF  
    ENDIF
  ENDFOR
ELSE
  lcExpToSeek = ''
ENDIF
RETURN
*-- End of lfActInSr.

*!*******************************************************************************
*! Name      : lfAdrLine
*! Developer : Walid Abd Elwahab
*! Date      : 09/22/1999
*! Purpose   :add line or more  for new order line in case there are 
*!           : an style position for order.style 
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  None
*!*******************************************************************************
*! Example            :  =lfAdrLine()
*!*******************************************************************************
*
FUNCTION lfAdrLine
IF EMPTY(m.STYLE) 
  RETURN
ENDIF
IF SEEK("SO" + lclastOrder + lcLastLine , lcT_BomVar)  AND !EMPTY(lcLastLine)
  *-- there are adornment data for that order line
  lnDelPrice = 0
  SELECT (lcT_BOMVAR)  
  SUM nPriceAdd FOR cIdType+cCost_ID+STR(LineNo,6)= "SO"+lclastOrder+STR(m.LineNo,6) TO lnDelPrice 
  m.Gros_Price = m.Gros_Price - lnDelPrice
  m.Price      = m.Gros_Price*(100-m.Disc_Pcnt)/100
  DELETE FOR cIdType+cCost_ID + STR(LineNo,6) =  "SO" + lclastOrder + STR(m.LineNo,6)
  SCAN FOR cIdType+cCost_ID + STR(LineNo,6) = "SO" + lclastOrder + lcLastLine 
    IF SEEK(m.Style + cStylePos ,'ICStyPos')
      *-- the style have an style position 
      lcCost_Id = cCost_Id 
      lnLineNo  = LineNo
      lcStylePos= cStylePos
      lcNDrpID  = cNDrpID 
      lcDsgnCode= cDsgnCode
      lnPriceAdd= nPriceAdd
      lnUntCost = UntCost
      lnBomTQty = nBomTotQty
      lnTotCost = TotCost
      DECLARE laDsgnTmp[1],laDsgnCod[1]
      laDsgnTmp = ''
      laDsgnCod = ''
      llChkNmDrp = .F.
      IF SEEK(m.Account+m.Store+lcNDrpID,'IcNAmDrp')
        lnUntCost = ICNamDrp.nNDrpCost
        lnTotCost = lnUntCost
        lnPriceAdd= ICNamDrp.nNDrpPric 
        IF llMulCurr AND laData[34]<>1 AND laData[34]<>0
          lnPriceAdd = lnPriceAdd  &lcOpsPMet laData[34] &lcOpsPUMt laData[50]
        ENDIF
        llChkNmDrp = .T.
      ELSE
        lnUntCost = 0
        lnTotCost = 0
        lnPriceAdd= 0 
        lcNDrpID  = ''
      ENDIF
      IF !EMPTY(lcNDrpID)  
        IF !EMPTY(IcNamDrp.cndrptt) .AND. !EMPTY(IcNamDrp.cndrptb)
          *-- Get the design sizes of the 2 text name-drop memo field.
          =gfSubStr(ALLTRIM(Icstypos.m2nd_dsgn) , @laDsgnTmp , ',')
        ELSE
          *-- If the name-drop is 1 text lines.
          *-- Get the design sizes of the 1 text name-drop memo field.
          =gfSubStr(ALLTRIM(Icstypos.m1nd_dsgn) , @laDsgnTmp , ',')
        ENDIF
      ELSE
        *-- Get the design codes of the "No Name-Drop" memo field into array.
        =gfSubStr(ALLTRIM(Icstypos.m0nd_dsgn) , @laDsgnTmp , ',')
        *-- Select all the design codes concatinating with its names.
      ENDIF
      SELECT cdsgnCode FROM (gcDataDir+"IcDesign") ;
               WHERE ASCAN(laDsgnTmp , Icdesign.cDsgnSize) > 0 ;
               INTO ARRAY laDsgnCod ORDER BY 1
      *--if the design code is assign to the style position get the cost price
      IF ASCAN(laDsgnCod , lcDsgnCode) > 0
        lnUntCost = lnUntCost + IIF(SEEK(lcDsgnCode,"ICDesign") ;
                      ,Icdesign.ndsgncst , 0)
        lnTotCost = lnUntCost
        lnDesgnPrc = ICDesign.nDsgnPrc 
        IF llMulCurr AND laData[34]<>1 AND laData[34]<>0
          lnPriceAdd =lnPriceAdd +( lnDesgnPrc &lcOpsPMet laData[34] &lcOpsPUMt laData[50])
        ELSE
          lnPriceAdd =lnPriceAdd + lnDesgnPrc
        ENDIF
      ELSE
        lcDsgnCode = ''
      ENDIF
      lnRecNo = RECNO()
      *-lineno
      lnLineNo = m.LineNo
      APPEND BLANK
      REPLACE cIdType    WITH 'SO'       ,;
              cCost_Id   WITH lcCost_Id  ,;
              LineNo     WITH lnLineNo   ,;
              cStylePos  WITH lcStylePos ,;
              cNDrpID    WITH lcNDrpID   ,;
              cDsgnCode  WITH lcDsgnCode ,;
              nPriceAdd  WITH lnPriceAdd ,;
              UntCost    WITH lnUntCost  ,;
              nBomTotQty WITH lnBomTQty  ,;
              TotCost    WITH lnTotCost  ,;
              cStatus    WITH 'A'        ,;
              nRecNo     WITH 0
      m.Gros_price = m.Gros_price + &lcT_BOMVAR..nPriceAdd
      m.Price      = m.Gros_Price*(100-m.Disc_Pcnt)/100
      IF BETWEEN(lnRecNo, 1, RECCOUNT())
        GO lnRecNo
      ENDIF
    ENDIF
  ENDSCAN

  *C102118,1 SSE Commented out because they are Removed from screen [Begin]
  *SHOW GET m.Gros_Price ENABLE
  *C102118,1 SSE Commented out because they are Removed from screen [End]
  
  SHOW GET m.Price      DISABLE
  SELECT (lcOrdLine)
  REPLACE Gros_Price  WITH m.Gros_Price ,;
          Price       WITH m.Price 
  SHOW WINDOW (lcOrdBrow) REFRESH SAME          
ENDIF
*-- End of lfAdrLine.

*!*******************************************************************************
*! Name      : lfEDIStat
*! Developer : Ahmed Ibrahim
*! Date      : 12/07/1999
*! Purpose   : Get the modified EDI order status
*!*******************************************************************************
*! Calls     : None
*!*******************************************************************************
*! Passed Parameters  :  None
*!*******************************************************************************
*! Returns            :  EDI Modification status
*!*******************************************************************************
*! Example            :  =lfEDIStat()
*!*******************************************************************************
*This function is not used anymore.
FUNCTION lfEDIStat
lcRetStat = SPACE(0)
DO CASE
  CASE CLINESTAT = 'AI'
    lcRetStat = 'New Item'
  CASE CLINESTAT = 'DI'
    lcRetStat = 'Deleted Item'
  CASE CLINESTAT = 'QD'  
    lcRetStat = 'Quantity Decreased'
  CASE CLINESTAT = 'QI'
    lcRetStat = 'Quantity Increased'
  CASE CLINESTAT = 'PC'  
    lcRetStat = 'Price Changed'
  CASE CLINESTAT = 'PQ'  
    lcRetStat = 'Quantity Changed'
ENDCASE
RETURN lcRetStat
*-- End of lfEDIStat.

*!*******************************************************************************
*! Name      : lfOldValue
*! Developer : Abdou ElGendi
*! Date      : 02/17/2000
*! Purpose   : When function of size quantities , total and net price
*!           : to store old value of the current filed.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*******************************************************************************
*! Passed Parameters  : ............
*!*******************************************************************************
*! Returns            : ............
*!*******************************************************************************
*
FUNCTION lfOldvalue
lcOldValue = EVALUATE('m.' + SYS(18))
*-- End Of lfOldValue

*!*******************************************************************************
*! Name      : lfWOldVale
*! Developer : Abdou ElGendi
*! Date      : 02/17/2000
*! Purpose   : To store the old value value
*!*******************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*******************************************************************************
*! Passed Parameters  : ............
*!*******************************************************************************
*! Returns            : ............
*!*******************************************************************************
*
FUNCTION lfWOldVale
lcOldValue = EVALUATE(SYS(18))
*-- End Of lfWOldVale.

*!*******************************************************************************
*! Name      : lfOrdStats
*! Developer : Abdou ElGendi
*! Date      : 02/17/2000
*! Purpose   : To store the old value value
*!*******************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*******************************************************************************
*! Passed Parameters  : ............
*!*******************************************************************************
*! Returns            : ............
*!*******************************************************************************
*
FUNCTION lfOrdStats
*-- If the system setuped to default order status to hold while the bulk status is open
IF  laSetups[7,2]='Y' .AND. ORDHDR.Status = 'O'
  IF Ordhdr.ApprAmt <> 0
    *-- Message        : Bulk order was approve,
    *-- Message        : Do you want to approve the new order.
    *-- Message NO.    : 32078
    *-- Button Message :  YES -- NO
    *-- Button         : 32000    
    IF gfModalGen('QRM32078B32000','DIALOG') = 1
      RETURN .F.
    ENDIF
  ELSE
    *-- Message        : Bulk Order status is 'Open', while order default status 
    *-- Message        : is set to 'Hold'. Would you like this new order to be 
    *-- Message        : Open?"
    *-- Button         : 32000    
    *-- Button Message : YES -- NO
    *-- Button         : 32000
    IF gfModalGen('QRM32080B32000','DIALOG') = 1
      RETURN .F.
    ENDIF
  ENDIF
ENDIF
*--End Of lfOrdStats.

****************************************************************************************
* FUNC: lfVPacksWin
* DESC: Trap keys for Packs window when Packs is notn the current get field in browse..
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 10/08/2000
****************************************************************************************
*This function is not used anymore.
FUNCTION lfVPacksWin
IF WONTOP() = lcBrTtl1 AND VARREAD()<> "PackQty"
  lcExpToSeek=""
  lcOrdExpr=""
  lnStartTrap = 32
  lnEndTrap   = 126
  FOR lnChrToTrap = lnStartTrap TO lnEndTrap
    ON KEY LABEL (CHR(lnChrToTrap)) DO lfPckIncSrch
  ENDFOR
ENDIF
*-- End of lfVPacksWin.

****************************************************************************
* FUNC: lfPckIncSrch
* DESC: Inc search for Packs.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 10/08/2000
****************************************************************************
*This function is not used anymore.
FUNCTION lfPckIncSrch
*-- clear trapping
PUSH KEY
ON KEY
ON KEY LABEL F1 llDumi = .T.
PRIVATE lnBrRecNO,lcOrgTag
CLEAR TYPEAHEAD
*-- get the key was pressed
lcExpToSeek = UPPER(CHR(LASTKEY()))
*-- if the key was pressed is one of "esc,enter,tab,shift+tab" do nothing
*-- else run the incremental search screen
IF !INLIST(LASTKEY(),27,13,9,15)
    DEFINE WINDOW lwIncSrch ;
      AT 21.000, 10.000 ;
      SIZE 4.167,55.444 ;
      FONT "FoxFont", 9 ;
      STYLE "B" TITLE "Aria Apparel System" ;
      FLOAT NOCLOSE SHADOW NOMINIMIZE NONE COLOR RGB(,,,192,192,192)
    ACTIVATE WINDOW lwIncSrch NOSHOW
  CLEAR TYPEAHEAD
  KEYBOARD "{END}"  && go to the end of the edit field
  llFromPack = .T.
  DO lfActInSr
ENDIF
POP KEY
*-- End of lfPckIncSrch.

****************************************************************************
* FUNC: lfTrapKeys
* DESC: Trap Or Clear keys for Inc Search for Packs.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 10/08/2000
****************************************************************************
*This function is not used anymore.
FUNCTION lfTrapKeys
PARAMETERS llSetKeys
FOR lnChrToTrap = 32 TO 126
  IF llSetKeys
    ON KEY LABEL (CHR(lnChrToTrap)) DO lfPckIncSrch
  ELSE
    ON KEY LABEL (CHR(lnChrToTrap))  
  ENDIF
ENDFOR
*-- End of lfTrapKeys.

*!**************************************************************************
*! Name      : lfwSeason
*! Developer : SSE
*! Date      : 01/15/2001
*! Purpose   : Season Code When function 
*!**************************************************************************
*! Calls     : gfwCodePop
*!**************************************************************************
*! Example   :  =lfwSeason()
*!**************************************************************************
*
FUNCTION lfwSeason
IF laScrMode[3] OR laScrMode[4]
  IF EOF(lcOrdLine)
    PRIVATE lnSeasonPo
    lnSeasonPo = ASCAN(laCodes,"SEASON")
    IF lnSeasonPo > 0
      lnSeasonPo = ASUBSCRIPT(laCodes,lnSeasonPo,1)
      laCodes[lnSeasonPo,4] = ""
      =gfwCodePop(@laCodes,'SEASON','L')
    ENDIF          
  ELSE
    =gfwCodePop(@laCodes,'SEASON','L')
    DIMENSION laTmpSeasn[1]
    =ACOPY(laSeasons,laTmpSeasn)
    IF lnSeason > 1
      DIMENSION laSeasons[2,2]
      laSeasons[1,1] = laTmpSeasn[1,1]
      laSeasons[1,2] = laTmpSeasn[1,2]
      laSeasons[2,1] = laTmpSeasn[lnSeason,1]
      laSeasons[2,2] = laTmpSeasn[lnSeason,2]
      lnSeason = 2
    ENDIF
    SHOW GET lnSeason ENABLE
  ENDIF
ENDIF
*-- End of lfwSeason. 

*!*******************************************************************************
*! Name      : gfCPBrows
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/17/20011
*! Purpose   : Browse the Order Header file.
*!*******************************************************************************
*! Reference : B803368,1
*!*******************************************************************************
*! Calls     : lfvOrder()
*!*******************************************************************************
*! Example   : =gfCPBrows()
*!*******************************************************************************
*
FUNCTION gfCPBrows
PRIVATE lcOrder , lnAlias , lcOldOrder , lcAccount , lcStore
lnAlias = SELECT()
SELECT ORDHDR
lcOldOrder = laData[1]
lcAccount = IIF(TYPE('lcOrderNo')="C",laData[2],"")
lcStore = IIF(TYPE('lcOrderNo')="C",laData[3],"")

DO lpDefnFlds               && Define lcBrFields

IF !EMPTY(lcOldOrder) .AND. SEEK(lcOrdType+lcOldOrder,'OrdHdr')
  lcOrder = IIF(ARIABROW("lcOrdType"+' FOR STORE = IIF(EMPTY(lcStore),"",lcStore)',;
                  "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
                  OrdHdr.Order,SPACE(6))
ELSE
  lcOrder =     "?"      
  =lfOrdBrow(@lcOrder,lcAccount,lcStore,"")
ENDIF

*-- If coming from Customer screen lcAccount holds Account Code so as to browse 
*-- only order for this Account.
laData[1] = IIF("?" $ lcOrder OR EMPTY(lcOrder),lcOldOrder,lcOrder) 

IF !EMPTY(laData[1])
  SELECT ORDHDR
  IF TYPE('lcOrderNo') <> 'C'
    lcSveFilt = FILTER()
    SET FILTER TO cOrdType + Order = lcOrdType
  ENDIF
  
  IF SEEK(lcOrdType+laData[1],"ORDHDR")
    laScrMode   = .F.
    laScrMode[2]= .T.
    SCATTER FIELDS &lcScFields MEMO TO laData
    SHOW GETS      
  ENDIF
  
  SELECT ORDHDR
  IF TYPE('lcOrderNo') <> 'C'
    SET FILTER TO &lcSveFilt
  ENDIF
ENDIF
SELECT (lnAlias)
*-- End of gfCPBrows.

*!*******************************************************************************
*! Name      : lpDefnFlds
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/17/20011
*! Purpose   : Define the lcBrFields variable.
*!*******************************************************************************
*! Reference : B803368,1
*!*******************************************************************************
*! Example   : lpDefnFlds
*!*******************************************************************************
*
PROCEDURE lpDefnFlds

*B606908,1 Modify the department heading. [Begin]
*lcBrFields = [Order:H="Order#",Status:1:H="Status",Account:H="Acct",]+;
*             [Store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
*             [Customer.stname:30:H="Name",]+;
*             [Dept:H="Department",MultiPo=IIF(MultiPo,'Yes','No'):H="Multi PO",]+;
*             [CustPo=IIF(MultiPO,'*Multi_PO*',CustPO):H="Cust. P.O#",]+;
*             [Bulk:H="Bulk",Multi:H="Multi",cFacCode:H="Factor Code",]+;
*             [Rep1:H="Sales Rep1",Comm1:H="Commission 1",Rep2:H="Sales Rep2",]+;
*             [Comm2:H="Commission 2",Open:H="Open.Qty.",OpenAmt:H="Open.Amt.",]+;
*             [Ship:H="Ship.Qty.",ShipAmt:H="Ship.Amt.",]+;
*             [Book:H="Book.Qty.",BookAmt:H="Book.Amt.",]+;
*             [Cancel:H="Cancel.Qty.",CancelAmt:H="Cancel.Amt."]
lcBrFields = [Order:H="Order#",Status:1:H="Status",Account:H="Acct",]+;
             [Store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
             [Customer.stname:30:H="Name",]+;
             [Dept:H="League",MultiPo=IIF(MultiPo,'Yes','No'):H="Multi PO",]+;
             [CustPo=IIF(MultiPO,'*Multi_PO*',CustPO):H="Cust. P.O#",]+;
             [Bulk:H="Bulk",Multi:H="Multi",cFacCode:H="Factor Code",]+;
             [Rep1:H="Sales Rep1",Comm1:H="Commission 1",Rep2:H="Sales Rep2",]+;
             [Comm2:H="Commission 2",Open:H="Open.Qty.",OpenAmt:H="Open.Amt.",]+;
             [Ship:H="Ship.Qty.",ShipAmt:H="Ship.Amt.",]+;
             [Book:H="Book.Qty.",BookAmt:H="Book.Amt.",]+;
             [Cancel:H="Cancel.Qty.",CancelAmt:H="Cancel.Amt."]
*B606908,1 Modify the department heading. [End]
             
lcBrFields = lcBrFields + [,Entered:H="Entered",Start:H="Start",Complete:H="Complete",]+;
             [Cancelled:H="Cancelled",Disc:H="Disc.",]+;
             [cWareCode:H="Warehouse",cAdd_User:H="User",dAdd_Date:H="Date",]+;
             [lcSesDesc=gfCodDes(Season,'SEASON'):H="Season",]+;
             [lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="Division",]+;
             [lcShipVia=gfCodDes(ShipVia,'SHIPVIA'):H="ShipVia",]+;
             [Note1:6:H="Notes",Note2:6:H="Notes 2"]
IF llMulCurr
  lcBrFields = lcBrFields + [,cCurrCode:H="Currency",nExRate:H="Ex. Rate",nCurrUnit:H="Unit"]
ENDIF             
*-- End of lpDefnFlds

*!*******************************************************************************
*! Name      : lfVOTrack
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/05/2001
*! Purpose   : Order tracking station.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfVOTrack()
*!*******************************************************************************
*
FUNCTION lfVOTrack
DO (gcAppHome+gcWinAppl+"\SOSOC20.FXP") WITH lcOrdType , laData[1] , "SALESORDER"
*-- End of lfVOTrack.

*!*******************************************************************************
*! Name      : lfVGlobal
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/05/2001
*! Purpose   : Valid function for global information Push Button
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfvGlobal()
*!*******************************************************************************
*
FUNCTION lfVGlobal
lcOrder = laData[1]
lcAccount = laData[2]
DO (gcScrDir+gcWinAppl+"\SOSOCGL.SPX")
*-- End of lfVGlobal.

*!*******************************************************************************
*! Name      : lfvLogo
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/05/2001
*! Purpose   : Valid function for logo button in printing info. screen.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfvLogo()
*!*******************************************************************************
*
FUNCTION lfvLogo
lcLogo = GETFILE('BMP', 'Select A BMP file:', 'Select',1)
SHOW GET lcLogo
*-- End of lfvLogo

*!*******************************************************************************
*! Name      : lfvGLogo
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/05/2001
*! Purpose   : Valid function for logo button in global info. screen.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfvGLogo()
*!*******************************************************************************
*
FUNCTION lfvGLogo
lcGLogo = GETFILE('BMP', 'Select A BMP file:', 'Select',1)
SHOW GET lcGLogo
*-- End of lfvGLogo

*!*******************************************************************************
*! Name      : lpGetPInfo
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/05/2001
*! Purpose   : Get printing information.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : DO lpGetPInfo.
*!*******************************************************************************
*
PROCEDURE lpGetPInfo
PRIVATE lcAlias, lcSDiv, lcSTeam
lcAlias = ALIAS()
SELECT (lcSocTemp)
IF laScrMode[2]
  ZAP
ENDIF

SELECT Soccer

IF SEEK(lcOrdType + laData[1] + lcDiv + lcTeam)
  *---Use as default for global information.
  lcGTypeSeq  =  Soccer.TeamsNo
  lcGLogoName =  Soccer.LogoName
  lcGLogo     =  Soccer.Logo

  *---Start looping to load soccer data.
  SCAN WHILE Order = laData[1]
     SCATTER TO MEMVAR
     lcSDiv = Div
     lcSTeam = Team
     SELECT (lcSocTemp)
     IF !SEEK(lcSDiv+lcSTeam)
       APPEND BLANK
       GATHER FROM MEMVAR
       REPLACE SpInst WITH Soccer.SpInst
     ENDIF
  ENDSCAN
ENDIF
  
SELECT (lcAlias)
*-- End of lpGetPInfo 

*!*******************************************************************************
*! Name      : lfAdlYo
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Compute youth and adult quantities.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfAdlYo()
*!*******************************************************************************
*
FUNCTION lfAdlYo
PRIVATE lcAlias, lcCurrTag, lcKeyValue
lcAlias = ALIAS()
SELECT (lcOrdLine)
lcKeyValue = EVALUATE(KEY())
lcCurrTag = TAG()
SET ORDER TO TAG OrdLinSt

*IF laScrMode[3] OR laScrMode[4]
*  laData[36] = laData[36] - lnPkAmount
*  laData[42] = laData[42] - lnPkAmount
*ENDIF  

=SEEK(lcOrdType + laData[1] + lcDiv + lcTeam)
SUM REST Youth, Adult, (Youth*YouthPr) + (Adult*AdultPr) + (Price*TotSize) ;
    TO lnYouthQty, lnAdultQty, lnPkAmount WHILE Store = lcDiv + lcTeam
=SEEK(lcOrdType + laData[1] + lcDiv + lcTeam)
SUM REST TotSize TO lnPieces WHILE Store = lcDiv + lcTeam 

*IF laScrMode[3] OR laScrMode[4]
*  laData[36] = laData[36] + lnPkAmount
*  laData[42] = laData[42] + lnPkAmount
*ENDIF  

SELECT (lcOrdLine)
SET ORDER TO TAG &lcCurrTag
=SEEK(lcKeyValue)
SELECT (lcAlias)
=lfRefresh(lcWinCh32)
*-- End of lfAdlYo.

*!*******************************************************************************
*! Name      : lfvPrice
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Valid function for youth , adult price.
*!*******************************************************************************
*! Calls     : lfRefresh()
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfvPrice()
*!*******************************************************************************
*
FUNCTION lfvPrice
PRIVATE lcAlias, lcCurrTag, lcCurrVal, lnOldYthPr , lnOldAdlPr
lcAlias = ALIAS()
SELECT (lcOrdLine)
lcCurrVal = EVALUATE(KEY())
lcCurrTag = TAG()
SET ORDER TO TAG OrdLinSt

STORE 0 TO lnOldYthPr , lnOldAdlPr

IF laScrMode[3] OR laScrMode[4]
  =SEEK(lcOrdType + laData[1] + lcDiv + lcTeam)
  SCAN REST WHILE Store = lcDiv + lcTeam
    IF cStyGroup = "JY"
      lnOldYthPr = YouthPr 
      lnOldAdlPr = AdultPr
    ENDIF
  ENDSCAN    
ENDIF  

IF !laScrMode[2]
  =SEEK(lcOrdType + laData[1] + lcDiv + lcTeam)
  REPLACE REST YouthPr WITH lnYouthPr , AdultPr WITH lnAdultPr , Flag ;
  WITH IIF(Flag='N',Flag,'M');
  WHILE Store = lcDiv + lcTeam FOR cStyGroup = 'JY'
ENDIF

IF laScrMode[3] OR laScrMode[4]
  =SEEK(lcOrdType + laData[1] + lcDiv + lcTeam)
  SCAN REST WHILE Store = lcDiv + lcTeam
    IF cStyGroup = "JY"
      laData[36] = laData[36] + ((Book1+Book2+Book3)*(lnYouthPr-lnOldYthPr)) + ((Book4+Book5+Book6+Book7+Book8)*(lnAdultPr-lnOldAdlPr))
      laData[42] = laData[42] + ((Qty1+Qty2+Qty3)*(lnYouthPr-lnOldYthPr)) + ((Qty4+Qty5+Qty6+Qty7+Qty8)*(lnAdultPr-lnOldAdlPr))
    ENDIF
  ENDSCAN    
ENDIF  

SELECT (lcOrdLine) 
SET ORDER TO TAG &lcCurrTag
=SEEK(lcCurrVal)
=lfAdlYo()

SELECT (lcAlias)
*-- End of lfvPrice.

*!*******************************************************************************
*! Name      : lpAddPrInfo
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Add printing information for each team from global info.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : DO lpAddPrInfo
*!*******************************************************************************
*
FUNCTION lpAddPrInfo
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT (lcSocTemp)
IF !SEEK(lcDiv + lcTeam)
  APPEND BLANK
  REPLACE Div      WITH lcDiv    , Team     WITH lcTeam   ,;
          Sponsor  WITH IIF(ibGSponsor  = 1,.T.,.F.),;
          InkColor WITH IIF(ibGInkColor = 1,.T.,.F.),;
          TeamsNo  WITH lcGTypeSeq, LogoName WITH lcGLogoName,;
          Logo     WITH lcGLogo,;
          OStation WITH 'N',;                 
          TStation WITH 'K'          && Added recently with the previous line
          
ENDIF          
SELECT (lcAlias)
*-- End of lpAddPrInfo.

*!*******************************************************************************
*! Name      : lfvStore
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Valid function for div+team.
*!*******************************************************************************
*! Calls     : lfAdlYo().
*!           : lfBrowDet(). 
*!           : lfRefresh().
*!           : lpGetPInfo.
*!           : lpAddPrInfo.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfvStore()
*!*******************************************************************************
*
FUNCTION lfvStore
PRIVATE lcAlias , lcCurrTag , lcStat , llLines
lcAlias = ALIAS()
IF !EMPTY(lcDiv) AND !EMPTY(lcTeam)
  SELECT (lcOrdLine)
  lcCurrTag = TAG()
  SET ORDER TO TAG OrdLinSt
  IF !SEEK(lcOrdType + laData[1] + lcDiv + lcTeam)
    IF laScrMode[2]
      =gfModalGen("INM00000B00000","DIALOG",'','','Invalid Division/Team. Retry!.')
      IF llScope
        SELECT (lcOrdLine)
        LOCATE
        lcDiv  = SUBSTR(Store,1,4)
        lcTeam = SUBSTR(Store,5,8)
        SHOW GET lcDiv  ENABLE
        SHOW GET lcTeam ENABLE
        LOCATE FOR Store = lcDiv + lcTeam AND Style.cStyGroup = 'JY'
        IF FOUND()
          lnYouthPr  = YouthPr
          lnAdultPr  = AdultPr
        ELSE
          lnYouthPr  = 0
          lnAdultPr  = 0
        ENDIF
        LOCATE
        LOCATE FOR Store = lcDiv + lcTeam
        llLines = IIF(FOUND(),.T.,.F.)
        lcStat  = IIF(laScrMode[2] OR !llLines ,'DISABLE','ENABLE')

        SHOW GET PbNew DISABLE        
        SHOW GET lnYouthPr &lcStat
        SHOW GET lnAdultPr &lcStat

        *C102118,1 SSE Enable/Disable Printing, Tracking, Invoice Push buttons. [Begin]
        DO CASE
          CASE laScrMode[3]
            SHOW GET PbInvoice DISABLE
            SHOW GET PbPrint   ENABLE
            SHOW GET PbDTrack  ENABLE
          CASE laScrMode[4]
            SHOW GET PbInvoice DISABLE
            IF !EMPTY(lcDiv) AND !EMPTY(lcTeam)
              SHOW GET PbPrint ENABLE
            ELSE
              SHOW GET PbPrint DISABLE
            ENDIF  
            SHOW GET PbDTrack  DISABLE
        ENDCASE
        *C102118,1 SSE Enable/Disable Printing, Tracking, Invoice Push buttons. [End]

        DO lpGetPInfo

        =lfAdlYo()
        =lfBrowDet(.T.,'O')
        _CUROBJ = OBJNUM(PbScope)
        llScope = .F.
      ELSE
        IF _CUROBJ = OBJNUM(lcDiv)
          _CUROBJ = OBJNUM(lcTeam)
        ELSE
          _CUROBJ = OBJNUM(lcDiv)
        ENDIF  
      ENDIF
      RETURN
    ENDIF
    IF SEEK('S' + laData[2] + lcDiv + lcTeam,'Customer')

      *Added recently
      IF laScrMode[3] OR laScrMode[4]
        llAddLine = .T.
      ENDIF  
      *Added recently      

    ELSE
      IF gfModalGen("QRM00000B00006","ALERT",'','','Division and Team codes are not found in Customer file, Do you want to Add ?') = 1
        
        *Added recently
        llAddLine = .T.
        *Added recently
        
        llNewTeam = .F.
        
        IF SEEK('M' + laData[2],'Customer')
          SELECT Customer
          SCATTER TO MEMVAR
          APPEND BLANK
          GATHER FROM MEMVAR
          REPLACE TYPE WITH 'S' , Store WITH lcDiv + lcTeam
        ENDIF
      ELSE
        STORE SPACE(4) TO lcDiv , lcTeam
        SHOW GET lcDiv  ENABLE
        SHOW GET lcTeam ENABLE
        _CUROBJ = OBJNUM(lcDiv)
        RETURN
      ENDIF
    ENDIF    
    llNewTeam = .F.

    SHOW GET PbNew ENABLE

    IF laScrMode[3] OR laScrMode[4]
      SHOW GET ibStyle ENABLE
      SHOW GET m.Style ENABLE
      _CUROBJ = OBJNUM(m.Style)
    ELSE
      SHOW GET ibStyle DISABLE
      SHOW GET m.Style DISABLE
    ENDIF  

    IF laScrMode[4]
      SHOW GET PbScope DISABLE
    ELSE
      SHOW GET PbScope ENABLE
    ENDIF

    SHOW GET lcDiv     DISABLE
    SHOW GET lcTeam    DISABLE
    SELECT (lcOrdLine)
    STORE 0 TO lnPieces, lnPkAmount, lnYouthQty, lnAdultQty, lnYouthPr, lnAdultPr

    *B606908,1 Enable the update push button. [Begin]
    *SHOW GET PbUpDate  DISABLE
    SHOW GET pbUpdate ENABLE
    *B606908,1 Enable the update push button. [End]

    SHOW GET PbRemove  DISABLE
    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
    DO lpAddPrInfo
  ELSE
    IF (laScrMode[3] OR laScrMode[4]) AND gfModalGen("QRM00000B00006","DIALOG",'','',[You have entered this division and team before. Do you want to modify its styles ?]) = 2
      STORE SPACE(4) TO lcDiv , lcTeam
      SHOW GET lcDiv  ENABLE
      SHOW GET lcTeam ENABLE
      STORE 0 TO lnPieces , lnPkAmount , lnYouthQty , lnYouthPr , lnAdultQty , lnAdultPr ,;
                 m.Price , m.Qty1 , m.Qty2 , m.Qty3 , m.Qty4 , m.Qty5 , m.Qty6 , m.Qty7 ,;
                 m.Qty8 , m.TotQty
      STORE '' TO m.cStyGroup , m.Scale
      m.Style = lcEmptySty
      SHOW GET lnYouthPr ENABLE
      SHOW GET lnAdultPr ENABLE      
      SHOW GET m.Style ENABLE
      SHOW GET m.Price DISABLE
      SHOW GET m.Qty1 ENABLE
      SHOW GET m.Qty2 ENABLE
      SHOW GET m.Qty3 ENABLE
      SHOW GET m.Qty4 ENABLE
      SHOW GET m.Qty5 ENABLE
      SHOW GET m.Qty6 ENABLE
      SHOW GET m.Qty7 ENABLE
      SHOW GET m.Qty8 ENABLE
      
      =lfRefresh(lcWinCh32)
      =lfRefresh(lcWinCh321)
      _CUROBJ = OBJNUM(lcDiv)
      RETURN
    ENDIF
    llNewTeam = .F.
    
    LOCATE
    LOCATE FOR Store = lcDiv + lcTeam AND Style.cStyGroup = 'JY'
    IF FOUND()
      lnYouthPr  = YouthPr
      lnAdultPr  = AdultPr
    ELSE
      lnYouthPr  = 0
      lnAdultPr  = 0
    ENDIF
    LOCATE
    LOCATE FOR Store = lcDiv + lcTeam
    llLines = IIF(FOUND(),.T.,.F.)
    lcStat = IIF(laScrMode[2] OR !llLines ,'DISABLE','ENABLE')

    IF laScrMode[2]
      SHOW GET PbNew DISABLE
    ELSE
      SHOW GET PbNew ENABLE
      SHOW GET ibStyle ENABLE
      SHOW GET m.Style ENABLE
      _CUROBJ = OBJNUM(m.Style)      
    ENDIF
    
    IF laScrMode[4]
      SHOW GET PbDTrack DISABLE
      SHOW GET ibStyle ENABLE
      SHOW GET m.Style ENABLE
      _CUROBJ = OBJNUM(m.Style)      
    ELSE
      SHOW GET PbDTrack ENABLE
      DO lpGetPInfo
    ENDIF
        
    SHOW GET PbUpDate  &lcStat
    SHOW GET PbRemove  &lcStat
    SHOW GET lnYouthPr &lcStat
    SHOW GET lnAdultPr &lcStat
    =lfAdlYo()
    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
  ENDIF
  SHOW GET PbPrint ENABLE
  SELECT (lcOrdLine)
  SET ORDER TO TAG (lcCurrTag)  
ENDIF
SHOW WINDOW (lcOrdBrow) REFRESH SAME
SELECT (lcAlias)
*-- End of lfvStore.

*!*******************************************************************************
*! Name      : lfCalcAmnt
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Function to calculate BookAmt,OpenAmt
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfCalcAmnt()
*!*******************************************************************************
*
FUNCTION lfCalcAmnt
PRIVATE lcAlias, lnCurrVal
lcAlias = ALIAS()              && Save current alias.
SELECT (lcOrdline)
lnCurrVal = EVALUATE(KEY())    && Save current record
LOCATE

STORE 0 TO laData[36] , laData[42]
IF laScrMode[4]
  laData[36] = 0
  SCAN
    IF cStyGroup = 'JY'
      *-- Booked
      laData[36] = laData[36] + (((Book1+Book2+Book3)*YouthPr) + ((Book4+Book5+Book6+Book7+Book8)*AdultPr))
      
      *-- Opened
      laData[42] = laData[42] + ((Qty1+Qty2+Qty3)*YouthPr) + ((Qty4+Qty5+Qty6+Qty7+Qty8)*AdultPr)    
    ELSE
      *-- Booked
      laData[36] = laData[36] + TotBook*Price
      
      *-- Opened
      laData[42] = laData[42] + TotQty*Price 
    ENDIF
  ENDSCAN  
ELSE
  laData[41] = 0
  SCAN
    laData[41] = laData[41] + TotQty
    IF LineNo = m.LineNo
      IF m.cStyGroup = "JY"
        *-- Booked
        laData[36] = laData[36] + (((m.Book1+m.Book2+m.Book3)*YouthPr) + ((m.Book4+m.Book5+m.Book6+m.Book7+m.Book8)*AdultPr))
      
        *-- Opened
        laData[42] = laData[42] + ((m.Qty1+m.Qty2+m.Qty3)*YouthPr) + ((m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8)*AdultPr)
        
      ELSE
        *-- Booked
        laData[36] = laData[36] + TotBook*Price
      
        *-- Opened
        laData[42] = laData[42] + TotQty*Price 
      ENDIF
      LOOP
    ENDIF
      
    IF cStyGroup = 'JY'
      *-- Booked
      laData[36] = laData[36] + (((Book1+Book2+Book3)*YouthPr) + ((Book4+Book5+Book6+Book7+Book8)*AdultPr))
      
      *-- Opened
      laData[42] = laData[42] + ((Qty1+Qty2+Qty3)*YouthPr) + ((Qty4+Qty5+Qty6+Qty7+Qty8)*AdultPr)    
    ELSE
      *-- Booked
      laData[36] = laData[36] + TotBook*Price
      
      *-- Opened
      laData[42] = laData[42] + TotQty*Price 
    ENDIF
  ENDSCAN  

ENDIF  

SELECT (lcOrdHdr)
=RLOCK()
REPLACE BOOK      WITH laData[35] ,;
        BOOKAMT   WITH laData[36] ,;
        OPEN      WITH laData[41] ,;
        OPENAMT   WITH laData[42]
UNLOCK

SELECT (lcOrdLine)
=SEEK(lnCurrVal)
SELECT (lcAlias)               && Restore old alias.
*-- End of lfCalcAmnt

*!*******************************************************************************
*! Name      : lfvPrintInfo
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Valid function for printing info. button in detail screen.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfvPrintInfo()
*!*******************************************************************************
*
FUNCTION lfvPrintInfo
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT (lcSocTemp)
SET ORDER TO TAG Soccer
IF SEEK(lcDiv + lcTeam)
  ibSponsor  = IIF(Sponsor,1,2)
  ibInkColor = IIF(InkColor,1,2)
  lcSpName   = SpName
  lcPrintIns = PrintInst
  lcTeamNo   = TeamsNo
  lcLogoName = LogoName
  lcLogo     = Logo
  lcInkColor = IIF(EMPTY(lcInkColor),InkColorD,lcInkColor)
  lcSpInst   = SpInst
ELSE
  ibSponsor  = ibGSponsor
  ibInkColor = ibGInkColor
  lcSpName   = SPACE(40)
  lcPrintIns = SPACE(40)
  lcTeamNo   = lcGTypeSeq
  lcLogoName = lcGLogoName
  lcLogo     = lcGLogo
  lcInkColor = IIF(EMPTY(lcInkColor),SPACE(10),lcInkColor)
ENDIF
DO (gcScrDir+gcWinAppl+"\SOSOCPR.SPX")
SELECT (lcAlias)
*-- End of lfvPrintInfo

*!*******************************************************************************
*! Name      : lfPrWhen
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Printing information when function.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfPrWhen()
*!*******************************************************************************
*
FUNCTION lfPrWhen
IF laScrMode[2]
  SHOW GETS DISABLE
  SHOW GET PbOK      ENABLE
  SHOW GET pbSpInst  ENABLE
ELSE
  SHOW GETS ENABLE
ENDIF    
*-- End of lfPrWhen.

*!*******************************************************************************
*! Name      : lfvPOK
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Valid function for ok button in printing info. screen.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfvPOK()
*!*******************************************************************************
*
FUNCTION lfvPOK
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT (lcSocTemp)
IF !SEEK(lcDiv + lcTeam)
  APPEND BLANK
ENDIF
REPLACE Div       WITH lcDiv      ,;
        Team      WITH lcTeam     ,;
        Sponsor   WITH IIF(IbSponsor=1,.T.,.F.) ,;
        InkColor  WITH IIF(ibInkColor=1,.T.,.F.) ,;
        SpName    WITH lcSpName   ,;
        PrintInst WITH lcPrintIns ,;
        TeamsNo   WITH lcTeamNo   ,;
        InkColorD WITH lcInkColor ,;
        LogoName  WITH lcLogoName ,;
        Logo      WITH lcLogo     ,;
        YouthPkg  WITH lnYouthQty ,;
        AdultPkg  WITH lnAdultQty ,;
        SpInst    WITH lcSpInst
SELECT (lcAlias)
*-- End of lfvPOK.

*!*******************************************************************************
*! Name      : lfvSpInst
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Valid function for special instruction button in printing info. screen.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfvSpInst()
*!*******************************************************************************
*
FUNCTION lfvSpInst
DO (gcScrDir+gcWinAppl+"\SOSOCSP.SPX")
*-- End of lfvSpInst.

*!*******************************************************************************
*! Name      : lfvDTrack
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Order/Division/Team tracking station.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfVDTrack()
*!*******************************************************************************
*
FUNCTION lfvDTrack
PRIVATE lcOldOrd , lcOldDiv , lcOldTeam , lcOldAcct
lcOldOrd  = laData[1]
lcOldDiv  = lcDiv
lcOldTeam = lcTeam
lcOldAcct = laData[2]
ON KEY LABEL TAB 
DO (gcAppHome+gcWinAppl+"\SOSOC20.FXP") WITH lcOrdType,laData[1],"SALESORDER",lcdiv,lcTeam
_CUROBJ=OBJNUM(PbDTrack)
laData[1] = lcOldOrd 
lcDiv     = lcOldDiv
lcTeam    = lcOldTeam
laData[2] = lcOldAcct
*-- End of lfvDTrack.

*!*******************************************************************************
*! Name      : lfvScope
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Valid function for Scope on team button in detail screen.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfvScope()
*!*******************************************************************************
*
FUNCTION lfvScope
STORE SPACE(4) TO lcDiv,lcTeam
llScope = .T.
SHOW GET lcDiv  ENABLE
SHOW GET lcTeam ENABLE
IF !laScrMode[2]
  STORE 0 TO lnPieces,lnPkAmount,lnYouthQty,lnYouthPr,lnAdultQty,lnAdultPr
  SHOW GET lnYouthPr
  SHOW GET lnAdultPr
ENDIF
_CUROBJ = OBJNUM(lcDiv)
*-- End of lfvScope

*!*******************************************************************************
*! Name      : lfUpdTeam
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Valid function for update team/new team button.
*!*******************************************************************************
*! Calls     : lfCheck().
*!           : lfBrowDet().
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfUpdTeam()
*!*******************************************************************************
*
FUNCTION lfUpdTeam
PRIVATE lnCheck

*B606923,1 SSE Commented out. [Begin]
*-- If Price is zero don't proceed
*IF lnYouthQty > 0 AND lnYouthPr <= 0
*  SHOW GET lnYouthPr ENABLE
*  _CUROBJ = OBJNUM(lnYouthPr)    
*  RETURN
*ENDIF  

*-- If Price is zero don't proceed
*IF lnAdultQty > 0 AND lnAdultPr <= 0
*  SHOW GET lnAdultPr ENABLE
*  _CUROBJ = OBJNUM(lnAdultPr)  
*  RETURN
*ENDIF  
*B606923,1 SSE Commented out. [End]
  
lnCheck = 1
lnCheck = lfCheck()

DO CASE
  CASE lnCheck = 1
    STORE '' TO m.Style, m.cStyGroup, m.Scale, m.Price
    STORE 0 TO lnPieces, lnPkAmount, lnYouthQty, lnAdultQty, lnYouthPr, lnAdultPr,;
               m.Qty1, m.Qty2, m.Qty3, m.Qty4, m.Qty5, m.Qty6, m.Qty7, m.Qty8
    STORE SPACE(4) TO lcDiv,lcTeam
    SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
    =lfRefresh(lcWinCh32)
    =lfRefresh(lcWinCh321)
    =lfRefresh(lcWinCh322)
    SHOW GET lcDiv   ENABLE
    SHOW GET lcTeam  ENABLE
    =lfBrowDet(.T.,'O')
    _CUROBJ = OBJNUM(lcDiv)
    STORE .T. TO llNewline,llAddLine          && to act as if we entered a new line
  CASE lnCheck = 2
    _CUROBJ = OBJNUM(m.Style)
    RETURN
  CASE lnCheck = 3
    SELECT (lcOrdLine)
    DELETE ALL WHILE Store = lcDiv + lcTeam
    IF laScrMode[4] 
      PACK
    ENDIF  
    STORE '' TO m.Style, m.cStyGroup, m.Scale, m.Price
    STORE 0 TO lnPieces, lnPkAmount, lnYouthQty, lnAdultQty, lnYouthPr, lnAdultPr,;
               m.Qty1, m.Qty2, m.Qty3, m.Qty4, m.Qty5, m.Qty6, m.Qty7, m.Qty8
    STORE SPACE(4) TO lcDiv,lcTeam
    SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
    =lfRefresh(lcWinCh32)
    =lfRefresh(lcWinCh321)
    =lfRefresh(lcWinCh322)
    SHOW GET lcDiv   ENABLE
    SHOW GET lcTeam  ENABLE
    =lfBrowDet(.T.,'O')
    _CUROBJ = OBJNUM(lcDiv)    
    STORE .T. TO llNewline,llAddLine          && to act as if we entered a new line
ENDCASE
*-- End of lfUpdTeam.

*!*******************************************************************************
*! Name      : lfCheck
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Function to check that total number of styles with group
*!           : 'JY' = styles with group 'SH' and 'SO'.
*!*******************************************************************************
*! Returns   : lnChoice
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfCheck()
*!*******************************************************************************
*
FUNCTION lfCheck
PRIVATE lnJY, lnSH, lnSO, lnChoice, lcAlias, lcCurrTag, lcCurValue
STORE 0 TO lnJY ,lnSH,lnSO
lcAlias = ALIAS()
SELECT (lcOrdLine)
lcCurrTag = TAG()
lcCurValue = EVALUATE(KEY())
SET ORDER TO TAG OrdLinSt
=SEEK(lcOrdType + laData[1] + lcDiv + lcTeam)

SCAN REST WHILE Store = lcDiv + lcTeam
  DO CASE
    CASE cStyGroup = 'JY'
      lnJY = lnJY + TotQty
    CASE cStyGroup = 'SO'
      lnSO = lnSO + TotQty
    CASE cStyGroup = 'SH'
      lnSH = lnSH + TotQty
  ENDCASE
ENDSCAN

lnChoice = 1
IF (lnJY <> lnSO) OR (lnJY <> lnSH)
  lnChoice =  gfModalGen("INM00000B32010","DIALOG",'','','Total number of jerseys does not match with total number of shorts or socks.')
ENDIF

SELECT (lcOrdLine)
SET ORDER TO TAG (lcCurrTag)
=SEEK(lcCurValue)
SELECT (lcAlias)
RETURN lnChoice
*-- End of lfCheck.

*!*******************************************************************************
*! Name      : lpGetOrdL
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Procedure to get information from ordline file.
*!*******************************************************************************
*! Calls     : lfCopyTemp().
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : DO lpGetOrdL
*!*******************************************************************************
*
PROCEDURE lpGetOrdL
PRIVATE llYOrA, lcCurrTag1, lcCurrTag2, lcCurStore, lcCurStyle, lnCurLine, lcStyGrp
llYOrA = .F.
SELECT (lcOrdLine)
lcCurrTag1 = TAG()
SET ORDER TO TAG OrdLinSt
ZAP

SELECT OrdLine
lcCurrTag2 = TAG()
SET ORDER TO TAG OrdLinSt
SET RELATION TO Style INTO Style

= SEEK(lcOrdType + laData[1])
SCAN REST WHILE cOrdType + Order = lcOrdType + laData[1]
  lcStyGrp = Style.cStyGroup
  IF Style.cStyGroup = 'JY'
    lcCurStore = Store
    lcCurStyle = Style
    lnCurLine  = LineNo
    IF SEEK(lcOrdType+laData[1]+lcCurStore+lcCurStyle+STR(lnCurLine-1,6),(lcOrdLine))
      SELECT (lcOrdLine)
      REPLACE Qty4 WITH OrdLine.Qty4, Qty5 WITH OrdLine.Qty5, Qty6 WITH OrdLine.Qty6 , ;
              Qty7 WITH OrdLine.Qty7, Qty8 WITH OrdLine.Qty8, Book4 WITH OrdLine.Book4, ;
              Book5 WITH OrdLine.Book5, Book6 WITH OrdLine.Book6, Book7 WITH OrdLine.Book7, ;
              Book8 WITH OrdLine.Book8, Pik4 WITH OrdLine.Pik4 , Pik5 WITH OrdLine.Pik5, ;
              Pik6 WITH OrdLine.Pik6, Pik7 WITH OrdLine.Pik7 , Pik8 WITH OrdLine.Pik8, ;
              Cut4 WITH OrdLine.Cut4, Cut5 WITH OrdLine.Cut5 , Cut6 WITH OrdLine.Cut6, ;
              Cut7 WITH OrdLine.Cut7, Cut8 WITH OrdLine.Cut8 , PoAlo4 WITH OrdLine.PoAlo4, ;
              PoAlo5 WITH OrdLine.PoAlo5, PoAlo6 WITH OrdLine.PoAlo6, PoAlo7 WITH OrdLine.PoAlo7 , ;
              PoAlo8 WITH OrdLine.PoAlo8, nPck4 WITH OrdLine.nPck4 , nPck5 WITH OrdLine.nPck5, ;
              nPck6 WITH OrdLine.nPck6 , nPck7 WITH OrdLine.nPck7, nPck8 WITH OrdLine.nPck8 ,;
              TotQty WITH TotQty + OrdLine.Qty4+OrdLine.Qty5+OrdLine.Qty6+OrdLine.Qty7+OrdLine.Qty8
      REPLACE TotBook WITH TotBook + OrdLine.Book4 + OrdLine.Book5 + OrdLine.Book6 +;
              OrdLine.Book7 + OrdLine.Book8, TotPik WITH TotPik + OrdLine.Pik4 + OrdLine.Pik5 + ;
              OrdLine.Pik6 + OrdLine.Pik7 + OrdLine.Pik8, TotCut WITH TotCut + OrdLine.Cut4 + ;
              OrdLine.Cut5 + OrdLine.Cut6 + OrdLine.Cut7 + OrdLine.Cut8, ;
              Tot_PoAlo WITH Tot_PoAlo + OrdLine.PoAlo4 + OrdLine.PoAlo5 + OrdLine.PoAlo6 + ;
              OrdLine.PoAlo7 + OrdLine.PoAlo8, ;
              Youth WITH Youth + IIF(OrdLine.Qty4+OrdLine.Qty5+OrdLine.Qty6+OrdLine.Qty7+;
              OrdLine.Qty8=0,OrdLine.Qty1+OrdLine.Qty2+OrdLine.Qty3,0), ;
              YouthPr WITH YouthPr + IIF(OrdLine.Qty4+OrdLine.Qty5+OrdLine.Qty6+OrdLine.Qty7+;
              OrdLine.Qty8=0,OrdLine.Price,0) , ;
              Adult WITH Adult + IIF(OrdLine.Qty1+OrdLine.Qty2+OrdLine.Qty3=0,OrdLine.Qty4+ ;
              OrdLine.Qty5+OrdLine.Qty6+OrdLine.Qty7+OrdLine.Qty8,0), ;
              AdultPr WITH AdultPr + IIF(OrdLine.Qty1+OrdLine.Qty2+OrdLine.Qty3=0,OrdLine.Price,0) , ;
              Size4 WITH Size4 + Qty4, Size5 WITH Size5 + Qty5, Size6 WITH Size6 + Qty6, ;
              Size7 WITH Size7 + Qty7, Size8 WITH Size8 + Qty8, ;
              TotSize WITH TotQty, cStyGroup WITH lcStyGrp, Price WITH 0
    ELSE
      SELECT OrdLine
      SCATTER MEMVAR MEMO
      SELECT (lcOrdLine)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE Qty1 WITH OrdLine.Qty1, Qty2 WITH OrdLine.Qty2,;
              Qty3 WITH OrdLine.Qty3, Qty4 WITH OrdLine.Qty4,; 
              Qty5 WITH OrdLine.Qty5, Qty6 WITH OrdLine.Qty6 , ;
              Qty7 WITH OrdLine.Qty7, Qty8 WITH OrdLine.Qty8,  ;
              Book1 WITH OrdLine.Book1, Book2 WITH OrdLine.Book2, ;
              Book3 WITH OrdLine.Book3, Book4 WITH OrdLine.Book4, ;
              Book5 WITH OrdLine.Book5, Book6 WITH OrdLine.Book6, ;
              Book7 WITH OrdLine.Book7, Book8 WITH OrdLine.Book8, ;
              Pik1 WITH OrdLine.Pik1, Pik2 WITH OrdLine.Pik2, ;
              Pik3 WITH OrdLine.Pik3, Pik4 WITH OrdLine.Pik4 , ;
              Pik5 WITH OrdLine.Pik5, Pik6 WITH OrdLine.Pik6, ;
              Pik7 WITH OrdLine.Pik7 , Pik8 WITH OrdLine.Pik8, ;
              Cut1 WITH OrdLine.Cut1, Cut2 WITH OrdLine.Cut2, ;
              Cut3 WITH OrdLine.Cut3, Cut4 WITH OrdLine.Cut4, ;
              Cut5 WITH OrdLine.Cut5 , Cut6 WITH OrdLine.Cut6, ;
              Cut7 WITH OrdLine.Cut7, Cut8 WITH OrdLine.Cut8 , ;
              PoAlo1 WITH OrdLine.PoAlo1, PoAlo2 WITH OrdLine.PoAlo2 , ;
              PoAlo3 WITH OrdLine.PoAlo3, PoAlo4 WITH OrdLine.PoAlo4, ;
              PoAlo5 WITH OrdLine.PoAlo5, PoAlo6 WITH OrdLine.PoAlo6, ;
              PoAlo7 WITH OrdLine.PoAlo7 , PoAlo8 WITH OrdLine.PoAlo8, ;
              nPck1 WITH OrdLine.nPck1 , nPck2 WITH OrdLine.nPck2 , ;
              nPck3 WITH OrdLine.nPck3, nPck4 WITH OrdLine.nPck4 , ;
              nPck5 WITH OrdLine.nPck5, nPck6 WITH OrdLine.nPck6 ,;
              nPck7 WITH OrdLine.nPck7, nPck8 WITH OrdLine.nPck8 ,;
              TotQty WITH OrdLine.Qty1+OrdLine.Qty2+OrdLine.Qty3+;
              OrdLine.Qty4+OrdLine.Qty5+OrdLine.Qty6+OrdLine.Qty7+OrdLine.Qty8
      REPLACE TotBook WITH OrdLine.Book1+OrdLine.Book2+OrdLine.Book3+;
              OrdLine.Book4+OrdLine.Book5+OrdLine.Book6+OrdLine.Book7+OrdLine.Book8, ;
              TotPik WITH OrdLine.Pik1+OrdLine.Pik2+OrdLine.Pik3+OrdLine.Pik4+ ;
              OrdLine.Pik5+OrdLine.Pik6+OrdLine.Pik7+OrdLine.Pik8, ;
              TotCut WITH OrdLine.Cut1+OrdLine.Cut2+OrdLine.Cut3+OrdLine.Cut4+;
              OrdLine.Cut5+OrdLine.Cut6+OrdLine.Cut7+OrdLine.Cut8, ;
              Tot_PoAlo WITH OrdLine.PoAlo1+OrdLine.PoAlo2+OrdLine.PoAlo3+;
              OrdLine.PoAlo4+OrdLine.PoAlo5+OrdLine.PoAlo6+OrdLine.PoAlo7+OrdLine.PoAlo8, ;
              Youth WITH IIF(OrdLine.Qty4+OrdLine.Qty5+OrdLine.Qty6+OrdLine.Qty7+;
              OrdLine.Qty8=0,OrdLine.Qty1+OrdLine.Qty2+OrdLine.Qty3,0), ;
              YouthPr WITH IIF(OrdLine.Qty4+OrdLine.Qty5+OrdLine.Qty6+OrdLine.Qty7+;
              OrdLine.Qty8=0,OrdLine.Price,0) , ;
              Adult WITH IIF(OrdLine.Qty1+OrdLine.Qty2+OrdLine.Qty3=0,OrdLine.Qty4+ ;
              OrdLine.Qty5+OrdLine.Qty6+OrdLine.Qty7+OrdLine.Qty8,0), ;
              AdultPr WITH IIF(OrdLine.Qty1+OrdLine.Qty2+OrdLine.Qty3=0,OrdLine.Price,0) , ;
              Size1 WITH Qty1, Size2 WITH Qty2, Size3 WITH Qty3, Size4 WITH Qty4, ;
              Size5 WITH Qty5, Size6 WITH Qty6, Size7 WITH Qty7, Size8 WITH Qty8, ;
              TotSize WITH TotQty, cStyGroup WITH lcStyGrp, Price WITH 0

    ENDIF
  ELSE
    SCATTER MEMVAR MEMO
    m.cStyGroup = lcStyGrp
    m.Size1 = m.Qty1
    m.Size2 = m.Qty2
    m.Size3 = m.Qty3
    m.Size4 = m.Qty4
    m.Size5 = m.Qty5
    m.Size6 = m.Qty6
    m.Size7 = m.Qty7
    m.Size8 = m.Qty8
    m.TotSize = m.TotQty
    INSERT INTO (lcOrdLine) FROM MEMVAR      
  ENDIF       
ENDSCAN

SELECT OrdLine
SET ORDER TO TAG &lcCurrTag2

STORE ''  TO laYes_No , laInkCol , lcGTypeSeq , lcGLogoName , lcInkColor ,;
             lcGLogo , lcSpInst , lcDiv , lcTeam

*B606908,1 Define this variable with 40 char. [Begin]             
lcGLogoName = SPACE(40)
*B606908,1 Define this variable with 40 char. [End]
             
STORE 0 TO lnPieces , lnPkAmount , lnYouthQty , lnYouthPr , lnAdultQty , lnAdultPr           
STORE 1 TO ibGSponsor , ibGInkColor

SELECT (lcOrdLine)
SET ORDER TO TAG &lcCurrTag1
LOCATE
IF !EOF()
  lcDiv  = SUBSTR(&lcOrdLine..Store,1,4)
  lcTeam = SUBSTR(&lcOrdLine..Store,5,8)
ENDIF
LOCATE FOR Store = lcDiv + lcTeam AND Style.cStyGroup = 'JY'
IF FOUND()
  lnYouthPr  = YouthPr
  lnAdultPr  = AdultPr
ELSE
  lnYouthPr  = 0
  lnAdultPr  = 0
ENDIF
LOCATE

DO lpGetPInfo
=lfAdlYo()
=lfBrowDet(.T.,'O')
_CUROBJ = OBJNUM(PbScope)
DO lpAddPrInfo
*-- End of lpGetOrdL

*!*******************************************************************************
*! Name      : lfvInvoice
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Valid function for Invoices button in detail screen.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfvInvoice()
*!*******************************************************************************
*
FUNCTION lfvInvoice
PRIVATE lcAlias
lcAlias = ALIAS()

=gfOpenFile(gcDataDir+'InvLine',gcDataDir+'InvLineO','SH')
SELECT InvLine
= SEEK(laData[1])
LOCATE REST WHILE Order = laData[1] FOR (Style = &lcOrdLine..Style ;
            AND IIF(!EMPTY(Store),Store = lcDiv + lcTeam,.T.))
IF FOUND()
  DO (gcScrDir+gcWinAppl+"\SOSOCIN.SPX")
ELSE
  =gfModalGen("INM00000B00000","DIALOG",'','','No invoices for this line.')
ENDIF
SELECT (lcAlias)
*-- End of lfvInvoice.

*!*******************************************************************************
*! Name      : lfInvBrow
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Browse invoice lines for an order line.
*!*******************************************************************************
*! Reference : C102118,1
*!*******************************************************************************
*! Example   : =lfInvBrow()
*!*******************************************************************************
*
FUNCTION lfInvBrow
PRIVATE lcInFields
lcWind_tl = 'Invoice Lines'
lcInFields = [STYLE :H='Style' :19:R ,] +;
             [STORE :H='Store' :8:R ,] +;
             [PRICE :H='Price' :8:R ,] +;
             [TOTQTY :H='Quantity' :6:R  ,] +;
             [lnPkAmount=PRICE*TOTQTY :9:H='Amount':R:P='999999.99',]+;
             [INVOICE:H='Invoice' :6:R ,] +;
             [INVDATE:H='Date' :10:R ]

BROWSE FIELDS &lcInfields FOR Order+STR(LineNo,6)+Invoice=laData[1] AND Style = &lcOrdLine..Style ;
                          AND IIF(!EMPTY(Store),Store = lcDiv + lcTeam,.T.);
       LOCK 0 NOAPPEND NODELETE NOMENU NOWAIT NOEDIT ;
       SAVE TITLE (lcWind_tl) WINDOW SOSOCI2 ;
       IN WINDOW SOSOCIN
*-- End of lfInvBrow.

