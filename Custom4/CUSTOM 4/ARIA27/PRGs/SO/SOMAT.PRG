*:************************************************************************
*: Program file  : SOMAT.PRG  (N000385)
*: Program desc. : Material Sales Orders
*:       System  : Aria Apparel System
*:    Developer  : TMI-Tarek Mohamed Ibraheem
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:*************************************************************
*: Passed Parameters  : lcOrdType : CMORDER type
*:                             'O' : CMORDER 
*:                             'C' : Contract
*:                             'T' : EDI temporary CMORDER 
*:                      lcOrderNo : CMORDER#
*:*************************************************************
*: Modifications      :
*:B606634,1 TMI 01/20/2003 Enable to add order # manually
*:B607083,1 AMH 03/23/2003 Fix bug of variable lcAgeType not found.
*:E126684,1 TMI 03/01/2005 change the decimals for NSELLPRICE,NSELLNETPR fields to 5 and make the ROUNDING UP
*:N125956,1 TMI 03/10/2005 Enable to use contracts for Fabrics
*:E126324,1 MHM 03/27/2005 handle printing from material sales order screen
*:C126361,1 TMI 05/19/2005 if this material sales order has a release order  then do not cnacel
*:B607537,1 HBG 12/04/2005 Update AltFabric & AltColor in MASOLINE if fabrci changed
*:E131400,1 NNA 04/12/2006 get the default warehous as saved in the file that if there is no default warehouse assigned 
*:E131400,1 NNA            to the material sales order screen
*:**********************************************************************
PARAMETERS lcOrdType,lcOrderNo

IF !('MA' $ gcCmpModules AND 'MA' $ gcComp_Mdl)
  *--XXX module is not installed, Cannot proceed.
  =gfModalGen('QRM42083B42001','DIALOG','Material')
  RETURN          
ENDIF

EXTERNAL ARRAY laData,laKeyField , laWobjects

DECLARE lafolders[3,2],laAddress[6,3],laQtyPaste[1],lafoldwinds[3,2],;
        laKeyField[2,4],laSetups[14,2] 

*E126684,1  TMI [Start] Set decimals to 5
lnSvDeci = SET('DECIMAL')
SET DECIMAL TO 5
*E126684,1  TMI [End  ] 

*B607083,1 Fix bug of variable lcAgeType not found [Start]
PRIVATE lcAgeType
lcAgeType = 'D'
*B607083,1 [End]

PRIVATE lcOldDept
lcOldDept = ''
PRIVATE lcOldDept,lcOldFab,lcOldClr,lcClrDesc,lcIDesc,lcUOB,m.Pattern,m.Width,m.UomUse
STORE '' TO lcOldDept,lcOldFab,lcOldClr,lcClrDesc,lcIDesc,lcUOB,  m.ClrDesc,m.IDesc,m.Pattern,m.Width,m.UomUse
DECLARE laCodes[7,10],laTerms[1,2],laSeasons[1,2],laDivision[1,2],;
        laShipVia[1,2],laSpcInst[1,2],laWareHouses[1,2],laCanReason[1,2],;
        laOCtgCd[1,2]        
DECLARE laDRltFld[2,2],laVariables[6],laOrdStatus[5],laBrowArr[1]
DECLARE laDisRltFld[1,2]
laOrdStatus[1] = 'Bid      '
laOrdStatus[2] = 'Open     '
laOrdStatus[3] = 'Hold     '
laOrdStatus[4] = 'Cancelled'
laOrdStatus[5] = 'Complete '
llCDPerL = .F.

llClearPck = .T.

STORE '' TO laCodes,laTerms,laShipVia,laSpcInst,laSeasons,laDivision,laCanReason,;
            laVariables,lcOrderBr,laWareHouses,lcTranBr,lcTranBr1,lcTranBr2,;
            lcOldCustPo,laOCtgCd,lcOldValue
STORE .F. to llMove 
laKeyField[1,1] = 'lcOrdType'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'MASOHDR'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[1]'
laKeyField[2,2] = .T.
laKeyField[2,3] = 'MASOHDR'
laKeyField[2,4] = 2

STORE .F. TO laSetups,llRebuild,llIgnorAll,llDomestic,m.lContract
lcDelMesag    = 'cancel'
laDefProc[7]  = .F.     && Cancel/Uncancel procedure(lpDelScr)
laDefProc[9]  = .F.     && Save procedure(lpSavScr)
laDefProc[10] = .F.     && close procedure(lpClsScr)
STORE 0   TO lnDisc_Pcnt,m.nSellConv

STORE 0   TO lnMarker,lnTopMark,lnBotMark,lnTMarker,lnStyLngth
STORE ' ' TO lcOrdBrow,lcBrTtl1,lcBrTtl2,lcBrowseTl,lcBrTtlM,lcGlYear,;
             lcGlPeriod,lcODefSes,lcODefDiv,lcPriceLvl,llAlowNew,;
             lcFolder,lcSession,lcScFields,lcScrMode,laBrowArr,lcCurrOrd
STORE ''  TO lcWinCh1,lcWinCh2,lcWinCh3,lcWinCh30,lcWinCh31,lcWinCh32,;
             lcWinCh321,lcWinCh322,lcWinCh33,lcWinCh34,lcWinCh4,;
             lcOrderFile,lcTranFile
lnMajorLen = 1
llESTYPSO = .T.
STORE 0 TO lnBookAmt,lnBalance
lcPayTmp = gfTempName()
llOpenOrd = .F.
STORE ''  TO   lcBaseFild,lcBaseTit,lcGlSales
lcorderfile = 'MASOLIN'
STORE {} TO ldDefOrdDate
STORE 0  TO lnCartons,lnLines,lnTLines,lnActFolder
STORE 1  TO lnOrdStatus,lnSortBy,lnOrdTrans,lnShipAddr,lnWareHouse
STORE '' TO lcFiles
STORE '' to lclastOrder , lcLastLine
*-- Define 3 variables for opening these 3 files.
PRIVATE llIcStyPos , llIcDesign , llIcNamDrp
STORE .F. TO llIcStyPos , llIcDesign , llIcNamDrp
*:E126324,1 MHM 03/27/2005 Define needed Array and temp. file[Start]

DIMENSION  laNewMpos[1]
laNewMpos  = ""
lcReportID = " "
lcTypPrint = ''
lcNewMPos = gfTempName()

CREATE CURSOR (lcNewMPos) (CMORDER C(6))
INDEX ON CMORDER TAG (lcNewMPos) OF (lcNewMPos)
APPEND BLANK
*:E126324,1 MHM [End]


llIcStyPos = gfOpenFile(gcDataDir+'ICSTYPOS',gcDataDir+'ICSTYPOS','SH')
llIcDesign = gfOpenFile(gcDataDir+'Icdesign',gcDataDir+'Icdesign','SH')

llIcNamDrp = gfOpenFile(gcDataDir+'IcNamDrp',gcDataDir+'IcNamDrp','SH')
STORE SPACE(8)  TO lcLastStore
STORE '' TO  lcDShpName,lcDShpAdd1,lcDShpAdd2,lcDShpAdd3,lcDShpAdd4,lcDShpAdd5,;
             lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
             lcDBllName,lcDBllAdd1,lcDBllAdd2,lcDBllAdd3,lcDBllAdd4,lcDBllAdd5
             
STORE .F.  TO llBrowse,llZoom,cbSetAsDef,llMulCurr,llEditExRt,llBulk,llInsur,;
              llNewLine,llContinue,llMultiSt,llAddLine,llReorder

PRIVATE lcOldStats

lcOldStats = ""
STORE .T.  TO llReBrowse,llDetails,llUpdBook,llConsAcc,llCntrExst
STORE " "  TO lcOrdLine,lcOrdHdr,lcPackHdr,lcTempline,lcSelLines,lcOrdCanLn,;
              lcStores,lcStores1,lcOrdSS,lcOrdTS,lcShipSS,lcShipTS,lcBrTtlT,;
              lcBooked,lcBookSS,lcBookTS,lcDepleted,lcBulkOrd,lcAlocated,lcODefWare
lnSessNo = gnProgCopy
STORE 1 TO lnTerms,lnShipVia,lnSpcInst,lnSeason,lnDivision,lnCanReason,lnOCtgCd
STORE 0 TO lnLastMode
STORE .T. TO llGoAndChk,llChkUnCom
lcForKey = '.T.'
IF !gfSetup()
  RETURN
ENDIF

IF TYPE('lcOrderNo') = 'C' .AND. SEEK(lcOrdType+lcOrderNo,'MASOHDR')
  SELECT MASOHDR
  lcOldOrd = ORDER()
  SET ORDER TO Ordacct
  PRIVATE lcTempFlt
  lcTempFlt = MASOHDR.Account
  SET FILTER TO account+cordtype+CMORDER = lcTempFlt+lcOrdType
  SET ORDER TO &lcOldOrd
  GOTO TOP
ENDIF
lcProgID   = 'SOMAT'
llReBrowse = .T.
lcOrdBrow  = IIF(lcOrdType='C','Contract Lines','Material Order Lines')
lcBrTtlZ   = IIF(lcOrdType='C','Zoom Contract Lines','Zoom Order Lines')
lcBrTtlB   = 'Booked Quantities'
lcBrTtlS   = 'Shipped Quantities'
lcBrTtlP   = 'Production Quantities'
lcBrTtlD   = 'Depleted Quantities'
lcBrTtlK   = 'Bulk order details'
lcBrTtlW  = "Work Order"
lcBrTtlC  = 'Canceled Lines'

lafolders[1,1] = 'Header'
lafolders[1,2] = 1
lafolders[2,1] = 'Details'
lafolders[2,2] = 2
lafolders[3,1] = 'Summary'
lafolders[3,2] = 3
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
  *:E126324,1 MHM 03/27/2005 Initialize Array[Start]
  laNewMpos  = ""
  *:E126324,1 MHM [End]
  =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
  SELECT cDesc,cWareCode FROM WAREHOUS INTO ARRAY laWareHouses

  *E131400,1 NNA 04/12/2006 (Begin) Open Warehous file to get the default warehous as saved at ldefware field then
  *E131400,1 NNA             close file again
  SELECT WAREHOUS
  LOCATE FOR ldefware
  IF FOUND()
    lnWareHouse = ASCAN(laWareHouses,WAREHOUS.CWARECODE)
    lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
  ENDIF  
  *E131400,1 NNA (End)

  =gfCloseFile('WAREHOUS')
  STORE 0 TO lnLastMode
  lcBaseFild = lcBrFields
  lcBaseTit  = lcFile_Ttl
  
  lcScFields = 'CMORDER,ACCOUNT,STORE,CUSTPO,STATUS,MULTI,MULTIPO,ENTERED,START,'+;
               'COMPLETE,cTermCode,SHIPVIA,SPCINST,SEASON,cDivision,DISC,DEPT,'+;
               'NOTE1,NOTE2,BUYER,PHONE,CINSUR,BULK,CREORDER,PRIORITY,CFACCODE,'+;
               'REP1,COMM1,REP2,COMM2,CWARECODE,LINK_CODE,CCURRCODE,NEXRATE,FABBOOK,FABBOOKAMT,'+;
               'FABSHIP,FABSHIPAMT,FABCANCEL,FABCANAMT,FABOPEN,FABOPENAMT,CFROMORDER,'+;
               'CANCELLED,DECL_DATE,DECL_CODE,CCANCRESON,APPROVAL,APPRAMT,'+;
               'NCURRUNIT,Alt_ShpTo,CORDERCAT,GL_SALES,INT_VEND,EVENT_COD,'+;
               'BILLNO,MERC_TYPE,BLANK_ORD,DISTRB_NO,CCLASS'
  SELECT MASOHDR
  SCATTER FIELDS &lcScFields TO laData BLANK
  lcSession = gfsequence('CSESSION')
  llAlowNew = lcOrdType <> 'T'
  laSetups[1,1]  = 'M_PACK'           && System has been steup to use packs
  laSetups[2,1]  = 'M_STY_COM'        && Edit sales reps commissions at styl level
  laSetups[3,1]  = 'M_OR_NOTE'        && Edit CMORDER lines notepad
  laSetups[4,1]  = 'M_LINK_GL'        && System has been linked to GL
  laSetups[5,1]  = 'M_WareHouse'      && System has been steup to use multiple warehouses
  laSetups[6,1]  = 'M_GenOrNum'       && Generate CMORDER number manually
  laSetups[7,1]  = 'M_HOLD_ORD'       && Default new CMORDER status to hold
  laSetups[8,1]  = 'M_CANAFTER'       && Number of days need to calculate CMORDER completion date
  laSetups[9,1]  = 'M_COMPDATE'       && Number of days added to CMORDER entered date to get the
                                      && date after which decrease CMORDER quqntity considered 
                                      && as cancellation 
  laSetups[10,1] = 'M_DIV_LINK'       && Link code at division level
  laSetups[11,1] = 'M_REP_COMM'       && Sales rep commision at division level
  laSetups[12,1] = 'M_CRDT_LMT'       && Warn if customer is above credit lemit 
  laSetups[13,1] = 'M_lImpCost'       && Use detailed costing for imported styl
  laSetups[14,1] = 'M_DYELOT'         && System has been setup to use dyelots
  =gfGetMemVar(@laSetups,gcAct_Comp)

  laSetups[10,2] = 'N'  && No division In case of materials
  
  *B607083,1 Get aging type. [Start]
  lcAgeType = gfGetMemVar('XAGINGTYPE',gcAct_Comp)    && Aging ar by date / terms
  *B607083,1 [End]

  llMulCurr   = gfGetMemVar('llMulCurr',gcAct_Comp)
  llEditExRt  = gfGetMemVar('LLEDITEXRA',gcAct_Comp)
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
  lcfolder    = gfTempName()      && Folder Window Name
  lcfoldprnt  = gcBaseWind        && window parent name for the folder
  lnactfolder = 1                 && active folder

  lafoldwinds[1,1] = lafolders[1,1]
  lafoldwinds[1,2] = lcWinCh2
  lafoldwinds[2,1] = lafolders[2,1]
  lafoldwinds[2,2] = lcWinCh3
  lafoldwinds[3,1] = lafolders[3,1]
  lafoldwinds[3,2] = lcWinCh4

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
  
  lcOrdCanLn = gfTempName()
  SELECT MASOLIN
  SCATTER MEMVAR BLANK MEMO
  IF TYPE('lcOrderNo') = 'C' .AND. SEEK(lcOrdType+lcOrderNo,'MASOHDR')
    SELECT MASOHDR
    STORE .F. TO laScrMode
    STORE .T. TO laScrMode[2]
  ENDIF
  laVariables[1] = 'ldDefOrdDate'
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
  
ELSE
  SELECT IIF((laScrMode[3] .OR. laScrMode[4]),lcOrdLine,'MASOLIN')
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
*--check triger if found
DECLARE laPanelObj[1,3]

STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbObjLink'
laPanelObj[1,2] = gcBmpHome+"Relate.bmp"
laPanelObj[1,3] = [VALID lfvObjLink() MESSAGE 'Object Link' DISABLE]
SELECT MASOHDR
IF TYPE('lcOrderNo') <> 'C'
  SET FILTER TO cOrdType+CMORDER=lcOrdType
ENDIF
DEFINE BAR 9  OF P03PU03 PROMPT '\<Edit' SKIP FOR !(laScrMode[2] .AND. INLIST(laData[5],'O','H'))
DO (gcScrDir+gcWinAppl+"\SOmat.SPX")
=lfClearKey()
DEFINE BAR 9  OF P03PU03 PROMPT '\<Edit' SKIP FOR .T.
DEFINE BAR 10 OF P03PU03 PROMPT '\<Delete' SKIP FOR .T.
IF llIcStyPos
  USE IN IcStyPos
ENDIF

IF llIcDesign
  USE IN IcDesign
ENDIF 

IF llIcNamDrp
  USE IN IcNamDrp
ENDIF
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
  *:E126324,1 MHM 03/27/2005 Check to print material sala order [Start]
  IF !EMPTY(laNewMpos)
    lcSOMNo = IIF(ALEN(laNewMpos,1)>1,ALLTRIM(STR(ALEN(laNewMpos,1),6)),'')
    *--If the user entered at least 1 MSO, ask him.
    * Do you want to print the (1-2..etc) purchase orders you have just entered?
    *     Yes    No
    DO CASE
      CASE lcOrdType = 'O'
        lcMessage = IIF(ALEN(laNewMpos,1)>1,lcSOMNo+ ' Material Sales orders','Material Sales order')
        lcTypPrint = 'O'
      CASE lcOrdType = 'C'
        lcMessage = IIF(ALEN(laNewMpos,1)>1,lcSOMNo+ ' contracts','contract')
        lcTypPrint = 'C'
    ENDCASE
    IF gfModalGen('INM36179B36001','DIALOG',lcMessage) = 1
      SELECT MASOHDR
      SELECT (lcNewMPos)
      ZAP
      FOR lnI = 1 TO ALEN(laNewMPos,1)
        INSERT INTO (lcNewMPos) (CMORDER) VALUES (SUBSTR(laNewMPos[lnI],2))
      ENDFOR
      
      KEYBOARD  CHR(13)
      WAIT WINDOW ''
      =gfCPPrint()
    ENDIF
  ENDIF
  *:E126324,1 MHM [End]
  
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
  
  *E126684,1  TMI [Start] restore to old decimal setting
  SET DECIMAL TO &lnSvDeci
  *E126684,1  TMI [End  ] 
ENDIF
SELECT MASOLIN
SET RELATION TO
SELECT MASOHDR
SET RELATION TO
SET FILTER TO
RELEASE PAD _INQUIRY OF _MSYSMENU

*!*************************************************************
*! Name      : lfActPad
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : lpvInquiry
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfActPad()
*!*************************************************************
FUNCTION lfActPad

DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
SET SKIP OF PAD _INQUIRY OF _MSYSMENU (laScrMode[1])
ON PAD _INQUIRY OF _msysmenu ACTIVATE POPUP _INQURYPOP

DEFINE POPUP _INQURYPOP MARGIN SHADOW
DEFINE BAR 1  OF _INQURYPOP PROMPT '\<Material Order Notes'  SKIP FOR laScrMode[1] .OR. laScrMode[4]
DEFINE BAR 2  OF _INQURYPOP PROMPT 'Ob\<ject Link'      SKIP FOR laScrMode[1] .OR. laScrMode[4]
DEFINE BAR 3  OF _INQURYPOP PROMPT 'Customer \<Inquire' SKIP FOR EMPTY(laData[2])
DEFINE BAR 4  OF _INQURYPOP PROMPT 'Customer \<Notepad' SKIP FOR EMPTY(laData[2])
DEFINE BAR 5  OF _INQURYPOP PROMPT '\<Aging Summary'    SKIP FOR EMPTY(laData[2])
DEFINE BAR 6  OF _INQURYPOP PROMPT '\<Fabric Inquire'    SKIP FOR (lnActFolder<>2) .OR. EMPTY(Fabric) .OR. (lnSortBy=3 .AND. !llDetails)
DEFINE BAR 7  OF _INQURYPOP PROMPT 'Sort By Lin\<e#'    SKIP FOR (lnActFolder<>2) .OR. !llMultiSt
DEFINE BAR 8  OF _INQURYPOP PROMPT 'Sort BY Fa\<bric'    SKIP FOR (lnActFolder<>2) .OR. !llMultiSt
DEFINE BAR 9  OF _INQURYPOP PROMPT 'Sort By Sto\<re'    SKIP FOR (lnActFolder<>2) .OR. !llMultiSt 
DEFINE BAR 10 OF _INQURYPOP PROMPT 'Show Su\<mmary'     SKIP FOR (lnActFolder<>2) .OR. !llMultiSt .OR. (lnSortBy=1) OR INLIST(lnOrdTrans,4,5,6,7)
DEFINE BAR 11 OF _INQURYPOP PROMPT 'Order \<Transactions' SKIP FOR (lnActFolder<>2) .OR. llZoom .OR. lcOrdType<>'O'
DEFINE BAR 12 OF _INQURYPOP PROMPT '\<Line Notepad'       SKIP FOR (lnActFolder<>2) .OR. laSetups[3,2]<>'Y' .OR. EMPTY(m.FABRIC) .OR. !llDetails
DEFINE BAR 13 OF _INQURYPOP PROMPT '\<Copy Line'          SKIP FOR (lnActFolder<>2) .OR. (laScrMode[2]) .OR. EMPTY(m.FABRIC) .OR. llZoom
DEFINE BAR 14 OF _INQURYPOP PROMPT '\<Paste Line'         SKIP FOR (lnActFolder<>2) .OR. (laScrMode[2]) .OR. EMPTY(m.FABRIC) .OR. llZoom
DEFINE BAR 15 OF _INQURYPOP PROMPT '\<Zoom Order Lines'   SKIP FOR (lnActFolder<>2)

SET MARK OF BAR 15 OF _INQURYPOP TO llZoom
SET MARK OF BAR lnSortBy+6 OF _INQURYPOP TO .T.
ON SELECTION POPUP _INQURYPOP DO lpvInquiry
ON BAR 11 OF _INQURYPOP ACTIVATE POPUP _ORDTRANS
DEFINE POPUP _ORDTRANS MARGIN SHADOW
DEFINE BAR 1 OF _ORDTRANS PROMPT '\<Open' 
DEFINE BAR 2 OF _ORDTRANS PROMPT '\<Booked'     SKIP FOR (!laScrMode[2])
DEFINE BAR 3 OF _ORDTRANS PROMPT '\<Shipped'    SKIP FOR (!laScrMode[2]) OR !('AR' $ gcCmpModules)
DEFINE BAR 4 OF _ORDTRANS PROMPT '\<Canceled'   SKIP FOR !(laScrMode[2]  AND llDetails)
SET MARK OF BAR lnOrdTrans OF _ORDTRANS TO .T.
ON SELECTION POPUP _ORDTRANS DO lpvOrdTrans

*!*************************************************************
*! Name      : lpvInquiry
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : gpDoProg,NOTEPAD,gfActWind,lfRefresh,lfBrowDet,lfvLineNote
*!             lfvCopyQty,lfvPasteQty,lfvBrowZoom
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpvInquiry()
*!*************************************************************
FUNCTION lpvInquiry
ACTIVATE WINDOW (lcFolder)
DO CASE
  CASE BAR() = 1      && ORDER Notes
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
  CASE BAR() = 6      && FABRIC Inquire    
    lcFabInq = IIF(EMPTY(m.FABRIC),&lcOrderFile..FABRIC,m.FABRIC)
    lcClrInq = IIF(EMPTY(m.COLOR ),&lcOrderFile..COLOR ,m.COLOR )    
    IF SEEK(lcFabInq+lcClrInq,'FABRIC')
      lcParameter = [']+lcFabInq+[',']+lcClrInq+[']
      DO gpDoProg WITH 'AWRMAMATRL',.t.,'MA',lcParameter
    ENDIF
  CASE BAR() = 7      && Sort by line#
    lnSortBy = 1
    llDetails =.T.
    SET MARK OF BAR 7  OF _INQURYPOP TO .T.  
    SET MARK OF BAR 8  OF _INQURYPOP TO .F.
    SET MARK OF BAR 9  OF _INQURYPOP TO .F.
    SET MARK OF BAR 11 OF _INQURYPOP TO .F.
    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
  CASE BAR() = 8      && Sort by fabric
    lnSortBy = 2
    SET MARK OF BAR 7 OF _INQURYPOP TO .F.  
    SET MARK OF BAR 8 OF _INQURYPOP TO .T.
    SET MARK OF BAR 9 OF _INQURYPOP TO .F.
    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
  CASE BAR() = 9      && Sort by Store
    lnSortBy = 3
    SET MARK OF BAR 7 OF _INQURYPOP TO .F.
    SET MARK OF BAR 8 OF _INQURYPOP TO .F.
    SET MARK OF BAR 9 OF _INQURYPOP TO .T.
    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
  CASE BAR() = 10      && Show Summary
    llDetails = !llDetails
    =lfActFolder()
    SET MARK OF BAR 10 OF _INQURYPOP TO !llDetails
    =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
  CASE BAR() = 12      &&  CMORDER Line Notepad
    =lfvLineNote()
  CASE BAR() = 13      &&  Copy CMORDER Line
   =lfvCopyQty()
  CASE BAR() = 14      &&  Paste CMORDER LIne
   =lfvPasteQty()
  CASE BAR() = 15     &&   Zoom CMORDER Browse
    llZoom = !llZoom
    =lfvBrowZoom()
    SET MARK OF BAR 15 OF _INQURYPOP TO llZoom
ENDCASE

*!*************************************************************
*! Name      : lfvBrowZoom
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Zoom Invoice lines browse
*!*************************************************************
*! Calls     : lfBrowDet
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvBrowZoom()
*!*************************************************************
FUNCTION lfvBrowZoom

IF llZoom 
  =lfBrowDet(.F.,'O')
ELSE
  llReBrowse = .T.
  =lfActFolder()
ENDIF

*!*************************************************************
*! Name      : lpvOrdTrans
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate CMORDER transactions
*!*************************************************************
*! Calls     : lfvOrdTrans
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpvOrdTrans()
*!*************************************************************
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
    =gfCloseFile('MINVLINE')  
    =gfCloseFile('consinvl')
    =gfCloseFile('MINVHDR')  
  CASE lnOrdTrans= 4
    =gfCloseFile('MORDCNLN')
ENDCASE
SET MARK OF BAR lnOrdTrans OF _ORDTRANS TO .F.
lnOrdTrans = BAR()
SET MARK OF BAR lnOrdTrans OF _ORDTRANS TO .T.
=lfvOrdTrans()

*!*************************************************************
*! Name      : lfvOrdTrans
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Browse transactions
*!*************************************************************
*! Calls     : lfBrowDet
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOrdTrans()
*!*************************************************************
FUNCTION lfvOrdTrans

IF lnOrdTrans = 1
  =lfPrepBrow() 
  SHOW WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322)  TOP
  =lfActFolder()
ELSE
  ACTIVATE WINDOW (LCFOLDER)
  HIDE WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322)  SAME
  SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
  =lfBrowDet(.T.,'T')
ENDIF

*!*************************************************************
*! Name      : lfClearKey
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
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
FOR lnChrToTrap = 32 TO 126
  ON KEY LABEL (CHR(lnChrToTrap))
ENDFOR
            
*!*************************************************************
*! Name      : lfReadAct
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : READ Activate function of SoOrd
*!*************************************************************
*! Calls     : lfClearKey.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfBrowDet
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Browse ORDER lines
*!*************************************************************
*! Calls     : lfPrepBrow,lfwBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfBrowDet()
*!*************************************************************
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
  IF llZoom
    HIDE WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322) SAME
    SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
    BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F., &lcOrderBr;
           WINDOW (lcWinCh33)  ;
           IN WINDOW (lcWinCh3);
           NOMENU             ;         
           NOAPPEND           ;
           NODELETE           ;         
           NOWAIT             ;
           SAVE               ;
	       NOCLEAR            ;
	       KEY lcKey          ;
           WHEN lfwBrow()     ;
           VALID :F lfvBrows();
           TITLE lcBrTtlZ 
  ELSE
    IF lnOrdTrans = 1
      SHOW WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322) TOP
    ENDIF
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
           TITLE lcOrdBrow 
  ENDIF
  =lfwBrow()
ENDIF  
IF lcBrow $ 'TB'
  IF lnOrdTrans = 8
    SELECT (lcTranFile)
    lnTMarker = RECNO(lcTranFile)
    BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnTMarker,'>',' '):H=' ':R:1:W=.F., &lcTranBr;
           WINDOW (lcWinCh34)  ;
           IN WINDOW (lcWinCh3);
           NOMENU           ;         
           NOAPPEND         ;
           NODELETE         ;         
           NOWAIT           ;
           SAVE             ;
           NOCLEAR          ;
           KEY "1"+&lcOrderFile..CMORDER+STR(&lcOrderFile..LineNo,6) , "3"+&lcOrderFile..CMORDER+STR(&lcOrderFile..LineNo,6) ;
           FOR CMORDER = &lcOrderFile..CMORDER .AND. cOrdLine = STR(&lcOrderFile..LineNo,6) ;
           WHEN lfwBrowT()  ;
           VALID :F lfvBrows()  ;
           TITLE lcBrTtlT
  ELSE
    lcKey    = EVAL(RELATION(1,lcOrderFile))
    SELECT (lcTranFile)
    =SEEK(lcKey)
    lnTMarker = RECNO(lcTranFile)
    BROWSE FIELDS ;
           cMarker =IIF(RECNO()=lnTMarker,'>',' '):H=' ':R:1:W=.F., &lcTranBr;
           WINDOW (lcWinCh34)  ;
           IN WINDOW (lcWinCh3);
           NOMENU           ;         
           NOAPPEND         ;
           NODELETE         ;         
           NOWAIT           ;
           SAVE             ;
           NOCLEAR          ;
           KEY lcKey        ;
           FOR EVAL(lcForKey);
           WHEN lfwBrowT()  ;
           VALID :F lfvBrows()  ;
           TITLE lcBrTtlT
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfwBrow
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Show line details
*!*************************************************************
*! Calls     : lfBrowDet
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfwBrow()
*!*************************************************************
FUNCTION lfwBrow

SELECT (lcOrderFile)

lnMarker = RECNO()

m.IDesc     = FABRIC.DESC
*N125956,1 TMI 03/10/2005 [Start] comment this line
*m.ClrDesc   = gfCodDes(&lcOrderFile..Color, 'COLOR')
*N125956,1 TMI [End] 
m.UomUse    = FABRIC.UOMUSE    
SHOW GET m.IDesc
SHOW GET m.ClrDesc
SHOW GET m.UomUse
IF lnOrdTrans = 1
  IF llZoom
    SHOW WINDOW (lcBrTtlZ) REFRESH SAME
  ELSE  
    SHOW WINDOW (lcOrdBrow) REFRESH SAME
    SELECT (lcOrderFile)
    SCATTER MEMVAR MEMO
    SHOW GETS WINDOW (lcWinCh32)  ONLY
    SHOW GETS WINDOW (lcWinCh321) ONLY
    SHOW GETS WINDOW (lcWinCh322) ONLY
    =lfRefresh(lcWinCh32)
    =lfRefresh(lcWinCh321)
  ENDIF
ELSE
  IF lnOrdTrans= 4 .AND. FABRIC.Make <> llDomestic
    llDomestic = FABRIC.Make
    =lfBrowDet(.F.,'T')
    ACTIVATE WINDOW (lcOrdBrow)
  ENDIF 
  IF llZoom
    SHOW WINDOW (lcBrTtlZ) REFRESH SAME
  ELSE  
    SHOW WINDOW (lcOrdBrow) REFRESH SAME
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfwBrowT
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Show line details
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfwBrowT()
*!*************************************************************
FUNCTION lfwBrowT
lnTMarker = RECNO(lcTranFile)
SHOW WINDOW (lcBrTtlT) REFRESH SAME

*!*************************************************************
*! Name      : lfDOrdScr
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : READ Deactivate function of screen SOORD
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfDOrdScr()
*!*************************************************************
FUNCTION lfDOrdScr

IF (laScrMode[3] OR laScrMode[4]) AND WLAST()=(lcWinCh321) AND !lfChkTotQty(.F.)
  ACTIVATE WINDOW (lcWinCh321)
  _CUROBJ = OBJNUM(m.FABQty)
  RETURN
ENDIF
SET SKIP OF PAD _INQUIRY OF _MSYSMENU laScrMode[1]

IF WONTOP()=lcOrdBrow .OR. WONTOP()=lcBrTtlB .OR. WONTOP()=lcBrTtlS .OR. ;
   WONTOP()=lcBrTtlD .OR. WONTOP()=lcBrTtlP .OR. WONTOP()=lcBrTtlZ .OR. ;
   WONTOP()=lcBrTtlK OR WONTOP()=lcBrTtlC
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
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

  STORE SPACE(08)  TO lcLastStore
  IF EMPTY(m.Fabric)
    SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
    SHOW GET pbNew   ENABLE
  ELSE
    SHOW GETS WINDOW (lcWinCh32)  ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) ENABLE ONLY
    IF !llMultiSt
      SHOW GET ibLStore   DISABLE
      SHOW GET m.Store    DISABLE
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lpTab
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTab WITH 'lcWinCh32', 'm.Store',.T.
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL TAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lpBackTab
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpBackTab WITH 'lcWinCh32', 'm.Store',.T.
*!*************************************************************
PROCEDURE lpBackTab
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF


*!*************************************************************
*! Name      : lpShow
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Show function
*!*************************************************************
*! Calls     : lfReStart,gfwCodePop,lfGetInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpShow()
*!*************************************************************
FUNCTION lpShow

lcBrFields = lcBaseFild
lcFile_Ttl = lcBaseTit
lcCanBar = "IIF(laData[5]='X','\<Uncancel','\<Cancel')"
DEFINE BAR 10 OF P03PU03 PROMPT &lcCanBar SKIP FOR !laScrMode[2]
IF llContinue .OR. (TYPE('lcOrderNo') = 'C' .AND. EMPTY(laData[1]) .AND. ;
                  SEEK(lcOrdType+lcOrderNo,'MASOHDR') AND laScrMode[2])
  SELECT IIF(laScrMode[2],'MASOHDR',lcOrdHdr)
  SCATTER FIELDS &lcScFields TO laData
ENDIF
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'Ccode_no','SH')
IF (laScrMode[2] AND lnLastMode <> 2) OR (laScrMode[4] AND lnLastMode <> 4)
  =gfOpenFile(gcDataDir+'FABRIC',gcDataDir+'FABRIC','SH') 
  =gfOpenFile(gcDataDir+'FABDYE',gcDataDir+'FABDYE','SH')
  =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
  =gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')
  =IIF(laSetups[4,2]='Y',gfOpenFile(gcDataDir+'Gl_Link',gcDataDir+'Gl_Link1','SH'),.T.)
  SELECT FABRIC
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
    IF llGoAndChk AND lfChkUnComS()
      RETURN
    ENDIF
    
    
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
    STORE 'MASOHDR' TO laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],;
                      laCodes[3,7],laCodes[3,8],laCodes[4,7],laCodes[4,8],;
                      laCodes[5,7],laCodes[5,8],laCodes[7,7],laCodes[7,8]

    
    =lfClsFiles()
    =lfGetInfo()
    SELECT MASOLIN
    SET RELATION TO FABRIC+COLOR+laData[31]+DYELOT INTO FABDYE
    SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE
    =SEEK(lcOrdType+laData[1])
    laData[53] = IIF(!EMPTY(laData[53]),SUBSTR(laData[53],1,3),'DEF')
    SELECT MASOHDR
    SET RELATION TO 'M'+Account INTO CUSTOMER
    *:E126324,1 MHM 03/27/2005 update CMORDER field[Start]
    REPLACE (lcNewMPos+'.CMORDER') WITH laData[1]
    *:E126324,1 MHM [End]
    
  CASE laScrMode[3]  .AND. lnLastMode <> 3

    llGoAndChk = .T.
    lnLastMode = 3
    llCUpdate = .T.
    STORE 0 TO laQtyPaste
    lnLines = MASOHDR.LastLine
    =lfCratTemp()
    
    SELECT MASOHDR
    SCATTER MEMVAR
    INSERT INTO (lcOrdHdr) FROM MEMVAR
    SELECT MASOLIN
    =SEEK(lcOrdType+laData[1])
    SCAN REST WHILE cordtype+CMORDER+STR(lineno,6) = lcOrdType+laData[1]
      SCATTER MEMVAR MEMO
      INSERT INTO (lcOrdLine) FROM MEMVAR
    ENDSCAN
    SELECT (lcOrdLine)
    SET RELATION TO FABRIC+COLOR+laData[31]+DYELOT INTO FABDYE
    SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE
    GO TOP
    SCATTER MEMVAR MEMO
    lcScrMode = 'E'
    SELECT 'UNCMSESS'
    IF SEEK('I'+PADR("SOMAT",10)+PADR(gcUser_id,10)) AND RLOCK()
      BLANK
    ELSE
      APPEND BLANK
    ENDIF
    REPLACE Status     WITH 'O'       ,;
            cUTranType WITH lcProgID  ,;
            cUserId    WITH gcUser_id ,;
            cSession   WITH lcSession ,;
            cProgram   WITH 'SOMAT'   ,;
            cCurrScr   WITH 'SOMAT'   ,;
            dTranDate  WITH gdSysDate ,;
            cTranTime  WITH TIME()
    =RLOCK()
    lcFiles = 'lcOrdHdr,'+lcOrdHdr+','+lcOrdHdr+';'+;
              'lcOrdLine,'+lcOrdLine+',MASOLIN;'
    lcFiles = lcFiles + 'lcOrdCanLn,'+lcOrdCanLn+','+lcOrdCanLn+';'
    lcCurrOrd = laData[1]
    =gfSavSess(lcProgID, lcFiles, @laVariables,lcSession)
    STORE lcOrdHdr TO laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],;
                      laCodes[3,7],laCodes[3,8],laCodes[4,7],laCodes[4,8],;
                      laCodes[5,7],laCodes[5,8],laCodes[7,7],laCodes[7,8]

    *IF lnOrdStatus=1
      DECLARE laOrdStatus[3]
      laOrdStatus[1] = 'Bid      '
      laOrdStatus[2] = 'Open     '
      laOrdStatus[3] = 'Hold     '
    *ENDIF  
    llUpdBook = laData[5]='B' .OR. (gdSysDate <= (laData[8] + laSetups[8,2]))
    llReBrowse=.T.
  CASE laScrMode[4] 
    IF lnLastMode <> 4
      llGoAndChk = .T.
      STORE 0 TO laQtyPaste
      IF EMPTY(ldDefOrdDate)
        STORE '' TO lcODefSes,lcODefDiv
        STORE 0  TO lnSeason,lnDivision,lnWareHouse

        *E131400,1 NNA 04/12/2006 (Begin) Open Warehous file to get the default warehous as saved at ldefware field then
        *E131400,1 NNA             close file again
        lnOldAlias = SELECT(0)
        IF !USED('WAREHOUS')
          =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
        ENDIF
        SELECT WAREHOUS
        LOCATE FOR ldefware
        IF FOUND()
          lnWareHouse = ASCAN(laWareHouses,WAREHOUS.CWARECODE)
          lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
        ENDIF  
        =gfCloseFile('WAREHOUS')
        SELECT(lnOldAlias)
        *E131400,1 NNA (End)

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
        ldDefOrdDate = gdSysDate + laSetups[9,2]
        DO (gcScrDir+gcWinAppl+"\SOMOrdDf.SPX")
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
    SET RELATION TO FABRIC+COLOR+laData[31]+DYELOT INTO FABDYE
    SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE
    SCATTER MEMVAR MEMO
    SELECT (lcOrdHdr)
    SHOW GET lnOrdStatus
    IF !llContinue AND EMPTY(laData[2])
      =lfRefresh(lcWinCh2)
      SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh1)   DISABLE ONLY
      SHOW GET ibAccount ENABLE
      SHOW GET laData[2] ENABLE
      _CUROBJ = OBJNUM(laData[2])
    ENDIF
ENDCASE 
llCntrExst = .T.
=IIF(laScrMode[3] OR laScrMode[4],;
     SEEK('O'+PADR(lcProgID,10)+gcUser_id+lcSession,'UNCMSESS'),.T.)
=lfActFolder()

*!*************************************************************
*! Name      : lfActFolder
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Change folders
*!*************************************************************
*! Calls     : lfBrowDet
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfActFolder()
*!*************************************************************
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
    PRIVATE lcSeasonEn
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
    lcShoWh = lfChangeWh()
    SHOW GET lnWareHouse &lcShoWh
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
    SELECT IIF(laScrMode[3] .OR. laScrMode[4],lcOrdLine,'MASOLIN')
    =IIF(llReBrowse,lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B')),.T.)
    SHOW GET ibBrowOrd ENABLE
    SHOW GET ibBrowTrn ENABLE
    llReBrowse = .F.
    IF !llZoom
      SCATTER MEMVAR
      SHOW WINDOW (lcWinCh32),(lcWinCh321),(lcWinCh322) REFRESH SAME
      lcStatus = IIF((laScrMode[3] OR laScrMode[4]) AND !EOF(lcOrdLine) AND;
                      llDetails,'ENABLE','DISABLE')
      SHOW GETS WINDOW (lcWinCh32)  &lcStatus ONLY
      SHOW GETS WINDOW (lcWinCh321) &lcStatus ONLY
      SHOW GETS WINDOW (lcWinCh322) &lcStatus ONLY
      IF (laScrMode[3] .OR. laScrMode[4]) AND llDetails
        SHOW GET pbNew   ENABLE 
      ELSE
        SHOW GET pbNew   DISABLE
      ENDIF
      IF (laScrMode[3] .OR. laScrMode[4]) .AND. llMultiSt .AND. ;
         !EOF(lcOrdLine) AND llDetails
        SHOW GET ibLStore ENABLE
        SHOW GET m.Store  ENABLE
      ELSE
        SHOW GET ibLStore DISABLE
        SHOW GET m.Store  DISABLE
      ENDIF
      IF (laScrMode[3] .OR. laScrMode[4]) .AND. !EMPTY(FABRIC) AND llDetails
        SHOW GET pbRemove ENABLE
      ELSE
        SHOW GET pbRemove DISABLE
      ENDIF
    ENDIF
    SELECT IIF(laScrMode[3] .OR. laScrMode[4],lcOrdLine,'MASOLIN')
    =lfwBrow()
    _CUROBJ = IIF(!EMPTY(FABRIC) .OR. llZoom,OBJNUM(ibBrowOrd),OBJNUM(pbNew))
  CASE lnActFolder = 3
    lcStatus = IIF(laScrMode[3] .OR. laScrMode[4],'ENABLE','DISABLE')
    SHOW GETS WINDOW (lcWinCh2)   DISABLE  ONLY 
    SHOW GETS WINDOW (lcWinCh30)  DISABLE  ONLY
    SHOW GETS WINDOW (lcWinCh32)  DISABLE  ONLY
    SHOW GETS WINDOW (lcWinCh321) DISABLE  ONLY
    SHOW GETS WINDOW (lcWinCh322) DISABLE  ONLY
    SHOW GETS WINDOW (lcWinCh4)  &lcStatus ONLY
    IF llMultiSt .OR. lcOrdType='C'
      SHOW GET llBulk DISABLE
    ENDIF
    lnSavAlias = SELECT (0)
    SELECT IIF(laScrMode[3] .OR. laScrMode[4],lcOrdhdr,'MASOHDR')
    IF !EMPTY(cfacCode)
      SHOW GET ibGlLink   DISABLE
      SHOW GET laData[32] DISABLE
    ELSE
      IF laScrMode[3] .OR. laScrMode[4]
        SHOW GET ibGlLink   ENABLE
        SHOW GET laData[32] ENABLE
      ENDIF
    ENDIF
    SELECT(lnSavAlias)
    =lfRefresh(lcWinCh4)
ENDCASE
IF (laScrMode[4] AND !EMPTY(laData[2])).OR. laScrMode[3]
  SHOW GET lnOrdStatus ENABLE
ELSE
  SHOW GET lnOrdStatus DISABLE
ENDIF  
IF (laScrMode[3] .OR. laScrMode[4]) .AND. EOF(lcOrdLine) AND !EMPTY(laData[2])
  SHOW GET llMultiSt ENABLE
ELSE
  SHOW GET llMultiSt DISABLE
ENDIF
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

*!*************************************************************
*! Name      : lfvDefaults
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate session defaults
*!*************************************************************
*! Calls     : CODECHK,gfBrowWare
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDefaults()
*!*************************************************************
FUNCTION lfvDefaults

lcODefSes  = ALLTRIM(laSeasons[lnSeason,2])
lcODefDiv  = ALLTRIM(laDivision[lnDivision,2])
lcODefWare = laWareHouses[lnWareHouse,2]
IF EMPTY(ldDefOrdDate)
  *E300420,1 Message : 32022
  *E300420,1 CMORDER copletion date cannot be empty.
  *E300420,1 Button : 00000
  *E300420,1 Ok
  =gfModalGen('TRM32022B00000','ALERT',IIF(lcOrdType='C','Contract','CMORDER'))
  _CUROBJ = OBJNUM(ldDefOrdDate)
  RETURN
ENDIF
IF gdSysDate > ldDefOrdDate
  *E300420,1 Message : 32013
  *E300420,1 The starting date cannot be greater than the completion date.
  *E300420,1 Button : 00000
  *E300420,1 Ok
  =gfModalGen('TRM32013B00000','ALERT')
  ldDefOrdDate = gdSysDate + laSetups[9,2]
  _CUROBJ = OBJNUM(ldDefOrdDate)
  RETURN
ENDIF
IF  gcContCode # 'EGYPT'
  IF CDOW(ldDefOrdDate)='Saturday' .OR. CDOW(ldDefOrdDate)='Sunday'
    *E300420,1 Message : 32014
    *E300420,1 The completion date is a xxxxx.
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('INM32014B00000','ALERT',CDOW(ldDefOrdDate))
  ENDIF
ENDIF

IF cbSetAsDef
  SAVE ALL LIKE lcODef* TO ("ORDDEF.MEM")
ENDIF
CLEAR READ

*!*************************************************************
*! Name      : lpClsScr
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Cancel new CMORDER
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpClsScr()
*!*************************************************************
FUNCTION lpClsScr

SELECT unCmSess
REPLACE STATUS WITH 'I'
UNLOCK
llContinue = .F.
llReBrowse =.T.
SELECT MASOHDR
IF laScrMode[3]
  SCATTER FIELDS &lcScFields TO laData
ENDIF

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Restore invoice information
*!*************************************************************
*! Calls     : gfGetAdr
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGetInfo()
*!*************************************************************
FUNCTION lfGetInfo

DO lfGet_Info IN (gcAppHome + 'SO\SOMUPDAT.FXP')

*!*************************************************************
*! Name      : lfReStart
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : ReInitialize variables
*!*************************************************************
*! Calls     : gfwCodePop,lfchngfolder
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReStart()
*!*************************************************************
FUNCTION lfReStart

lcorderfile = 'MASOLIN'
=gfwCodePop(@laCodes,'CTERMCODE','N')
=gfwCodePop(@laCodes,'SHIPVIA','N')
IF llMultiSt
  =lfChMSHV()
ENDIF
=gfwCodePop(@laCodes,'SPCINST','N')
=gfwCodePop(@laCodes,'SEASON','N')
=gfwCodePop(@laCodes,'CDIVISION','N')
=gfwCodePop(@laCodes,'CORDERCAT','N')
STORE 0 TO lnLines,lnTLines
STORE 1 TO lnOrdStatus,lnShipAddr

STORE SPACE(08)  TO lcLastStore
STORE '' TO  lcDShpName,lcDShpAdd1,lcDShpAdd2,lcDShpAdd3,lcDShpAdd4,lcDShpAdd5,;
             lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
             lcDBllName,lcDBllAdd1,lcDBllAdd2,lcDBllAdd3,lcDBllAdd4,lcDBllAdd5
STORE '' TO lcScrMode,lcFiles,lcCurrOrd
STORE .F. TO llZoom,llMultiSt,llAddLine,llNewLine,llIgnorAll,llDomestic,llBulk,llReorder
STORE .T. TO llReBrowse,llUpdBook

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

*!*************************************************************
*! Name      : lfClsFiles
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Close opened temporary files
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfClsFiles()
*!*************************************************************
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
SELECT MASOHDR

*!*************************************************************
*! Name      : lfvOrdStatus
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate CMORDER status
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvOrdStatus()
*!*************************************************************
FUNCTION lfvOrdStatus

STORE SUBSTR('BOH',lnOrdStatus,1) TO laData[5]
SELECT (lcOrdHdr)
=RLOCK()
REPLACE STATUS WITH laData[5]
lcOldStats = ''

UNLOCK
llCUpDate = .T.

*!*************************************************************
*! Name      : lfvOrder
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate CMORDER number
*!*************************************************************
*! Calls     : lfOrdBrow,gfSeekRec
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvOrder()
*!*************************************************************
FUNCTION lfvOrder
PRIVATE lcOrder,lnAlias,lcOldOrdFlt
lnAlias = SELECT()
SELECT MASOHDR
lcOldOrdFlt = FILTER()
SET FILTER TO
IF llBrowse .OR. (!EMPTY(laData[1]) .AND. !SEEK(lcOrdType+laData[1],'MASOHDR'))
  lcOrder = laData[1]
  =lfOrdBrow(@lcOrder,laData[2],laData[3],laData[4])
  laData[1] = lcOrder 
  llBrowse = .F.
ENDIF

IF !EMPTY(laData[1])
  SELECT MASOHDR
  SET FILTER TO cOrdType+CMORDER  = lcOrdType+CMORDER
  =gfSeekRec()
ENDIF  
SELECT MASOHDR
SET FILTER TO &lcOldOrdFlt
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfOrdBrow
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate Invoice order
*!*************************************************************
*! Calls     : ARIABROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfOrdBrow()
*!*************************************************************
FUNCTION lfOrdBrow
PARAMETERS lcOrder,lcAccount,lcStore,lcCustPo

SELECT MASOHDR
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
*-- Adding the Browse in 2 lines because the compiler can't compile a line which is too long
*-- Note : No more fields can be added to the BROWSE or the browse will display an error message
*-- "a line is too long to be compiled"
DO lpDefnFlds               && Define lcBrFields
DO CASE
  CASE !EMPTY(lcOrder) .AND. SEEK(lcOrdType+lcOrder,'MASOHDR')
    RETURN
  CASE !EMPTY(lcAccount) .AND. EMPTY(lcCustPo)
    SET ORDER TO TAG 'OrdAcct' IN MASOHDR
    lcOrder = IIF(ARIABROW("lcAccount+lcOrdType"+' FOR STORE = IIF(EMPTY(lcStore),"",lcStore)',;
                  "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','CMORDER','laBrowArr'),;
                  MASOHDR.CMORDER,SPACE(6))
    SET ORDER TO TAG MASOHDR IN MASOHDR
  CASE !EMPTY(lcAccount) .AND. !EMPTY(lcCustPo) .AND. EMPTY(lcStore)
    SET ORDER TO TAG 'OrdCust' IN MASOHDR
    PRIVATE lcCstPoExp
    lcCstPoExp = IIF(SEEK(lcAccount+UPPER(lcCustPo)+lcOrdType,'MASOHDR'),;
                     'Account + UPPER(CustPO) = lcAccount+UPPER(lcCustPo)','.T.')
    lcOrder = IIF((SEEK(lcAccount+UPPER(lcCustPo)+lcOrdType,'MASOHDR') AND lfGetNxtPo(MASOHDR.CMORDER)) .OR. ;
              ARIABROW("lcAccount"+' FOR cOrdType=lcOrdType .AND. STORE = IIF(EMPTY(lcStore),"",lcStore);
              .AND. &lcCstPoExp',;
              "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','CMORDER','laBrowArr'),;
              MASOHDR.CMORDER,SPACE(6))
    SET ORDER TO TAG MASOHDR IN MASOHDR
  CASE !EMPTY(lcAccount) .AND. !EMPTY(lcCustPo) .AND. !EMPTY(lcStore)
    SET ORDER TO TAG 'OrdCust' IN MASOHDR
    IF SEEK(lcAccount+UPPER(lcCustPo)+lcOrdType,'MASOHDR') .AND. Store = lcStore

      lcorder =IIF(lfGetNxtPo(MASOHDR.CMORDER) .OR. ARIABROW("lcAccount"+' FOR cOrdType=lcOrdType .AND. STORE = lcStore .AND.Account + UPPER(CustPO) = lcAccount+UPPER(lcCustPo)',;
                   "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','CMORDER','laBrowArr'),;
                    MASOHDR.CMORDER,SPACE(6))
    ELSE
      *B603478,1 Message : 30238
      *B603478,1 No orders matching the selected. 
      *B603478,1 Button  : 00000
      *B603478,1 Ok
      = gfModalGen('TRM32038B00000','DIALOG','orders matching the')
    ENDIF  
    SET ORDER TO TAG MASOHDR IN MASOHDR
  OTHERWISE
    SET ORDER TO TAG MASOHDR IN MASOHDR
    PRIVATE lcOldFilt
    lcOldFilt = FILTER()
    SET FILTER TO
    lcOrder = IIF(ARIABROW('lcOrdType',"Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','CMORDER','laBrowArr'),;
                  MASOHDR.CMORDER,SPACE(6))
    SET FILTER TO &lcOldFilt
ENDCASE
SET RELATION OFF INTO CUSTOMER
*!*************************************************************
*! Name      : lfGetNxtPo
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Cheak IF The Account Have anthor CMORDER With Same 
*!           : CustPo Number.
*! Reference : B603478,1
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : lcPreOrd
*!*************************************************************
*! Returns            : True .OR. False
*!*************************************************************
*B603478,1
FUNCTION lfGetNxtPo
PARAMETER lcPreOrd
PRIVATE llToRet

LOCATE REST WHILE account+UPPER(custpo)+cordtype+CMORDER =;
                    lcAccount+UPPER(lcCustPo)+lcOrdType ;
              FOR CMORDER <> lcPreOrd .AND. STORE = IIF(EMPTY(lcStore),"",lcStore) 
IF FOUND()
  llToRet = .F.
ELSE
  llToRet = .T.
  =SEEK(lcAccount+UPPER(lcCustPo)+lcOrdType)
ENDIF
RETURN llToRet
*B603478,1(End)
*-- End OF lfGetNxtPo.
*!*************************************************************
*! Name      : lfvDiscount
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate CMORDER discount
*!*************************************************************
*! Calls     : lfvCommision
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDiscount()
*!*************************************************************
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
*E300408,1 Message : 32047
*E300408,1 Would you like to overwrite Order lines commissions with new commissions?
*E300408,1 Button : 32000
*E300408,1 Yes No
IF (lnOldComm1<>laData[28] .OR. lnOldComm2<>laData[30]) .AND. ;
   SEEK(lcOrdType+laData[1],lcOrdLine) .AND. ;
   (laSetups[2,2] <> 'Y' .OR. gfModalGen('QRM32047B32000','ALERT',;
   IIF(lcOrdType='C','contract','Order'))=1)
  SELECT (lcOrdLine) 
  SCAN
    =RLOCK()
    REPLACE Comm1 WITH laData[28],;
            Comm2 WITH laData[30]
    UNLOCK
  ENDSCAN
  GO TOP 
  SELECT (lcOrdHdr)
ENDIF

*!*************************************************************
*! Name      : lfvLinkCode
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate sales g.l. link code
*!*************************************************************
*! Calls     : gfGLBrowse
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvLinkCode()
*!*************************************************************
FUNCTION lfvLinkCode
PRIVATE lcLinkCode
IF llBrowse .OR. !SEEK('01'+laData[32],'Gl_Link')
  lcLinkCode = laData[32]
  =gfGLBrowse('01',@lcLinkCode)
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

*!*************************************************************
*! Name      : lfvSlsLink
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate CMORDER Sales link code
*!*************************************************************
*! Calls     : gfGLBrowse
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSlsLink()
*!*************************************************************
FUNCTION lfvSlsLink
PRIVATE lcLinkCode

IF llBrowse .OR. !SEEK('02'+SUBSTR(laData[53],1,3),'Gl_Link')
  lcLinkCode = SUBSTR(laData[53],1,3)
  =gfGLBrowse('02',@lcLinkCode,'',1)
  laData[53] = lcLinkCode
ENDIF
=lfRefresh(lcWinCh4)
llBrowse = .F.

*!*************************************************************
*! Name      : lfvFactor
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate factor
*!*************************************************************
*! Calls     : FacChk
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFactor()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfvAccount
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate CMORDER Account
*!*************************************************************
*! Calls     : CUSBROWM,gfGetAdr,gfChkRate,gfModalGen,gfActWind,lfRefresh,ARIABROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvAccount()
*!*************************************************************
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
IF laScrMode[4] .AND. !EMPTY(laData[2])
  IF Customer.Status <> 'A'
    IF Customer.Status <> 'P'
      *E300420,1 Message : 32023
      *E300420,1 This a non-active xxxxx. CMORDER entry is not allowed!
      *E300420,1 Button : 00000
      *E300420,1 Ok
      =gfModalGen('TRM32023B00000','ALERT','account'+'|'+IIF(lcOrdType='C','Contract','CMORDER'))
      laData[2] = ''
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF
  ENDIF  
  *E300420,1 Message : 32011
  *E300420,1 There are open bulk orders for account xxxxx. 
  *E300420,1 Do you wish to use any of them in creating this CMORDER?
  *E300420,1 Button : 32000
  *E300420,1 Yes/No
  IF lcOrdType='O' 
    SET ORDER TO TAG MASOHDR IN MASOHDR
  ENDIF
  =SEEK('M'+laData[2],'CUSTOMER')
  =IIF(laSetups[11,2]='D',gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH'),.T.)

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
  STORE '' TO laData[4],laData[48]
  STORE gdSysDate TO laData[8],laData[9]
  STORE Customer.cFacCode TO laData[26]
  STORE IIF(EMPTY(Customer.ShipVia),lfGetDefCode('SHIPVIA'),;
        Customer.ShipVia) TO laData[12]
  STORE IIF(EMPTY(Customer.cTermCode),lfGetDefCode('CTERMCODE'),;
        Customer.cTermCode)   TO laData[11]
  STORE IIF(EMPTY(Customer.SpcInst),lfGetDefCode('SPCINST'),;
        Customer.SpcInst) TO laData[13]
        
  STORE Customer.Disc     TO laData[16]
  STORE Customer.SalesRep TO laData[27]
  STORE Customer.Rep2     TO laData[29]
  STORE Customer.Note     TO laData[18]
  STORE ''                TO laData[19]
  STORE Customer.Buyer    TO laData[20]
  STORE Customer.Phone1   TO laData[21]
  STORE 'N' TO laData[6],laData[23],laData[24]
  STORE Customer.Link_Code TO laData[32]  
  IF EMPTY(Customer.Link_Code)
    lcLink_Cod = 'DEFDEF'
  ELSE
    lcLink_Cod = Customer.Link_Code
  ENDIF
  STORE lcLink_Cod TO laData[32]
  STORE lcODefDiv TO laData[15]

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
    STORE lcLink_Cod TO laData[32]
  ENDIF
  laData[53]= IIF(EMPTY(laData[53]),'DEF',laData[53])
  laData[52] = SUBSTR(laOCtgCd[lnOCtgCd,2],1,6)  
  llInsur = (CUSTOMER.cInsur='Y')
  laData[22]=IIF(llInsur,'Y','N')
  STORE IIF(laSetups[11,2]='D' .AND. SEEK(laData[27]+laData[15],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm) TO laData[28]
  STORE IIF(laSetups[11,2]='D' .AND. SEEK(laData[29]+laData[15],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm2) TO laData[30]
  STORE Customer.cCurrCode TO laData[33]
  STORE 0    TO laData[35],laData[41],laData[36],laData[42]
  STORE ldDefOrdDate TO laData[10]
  STORE lcODefSes     TO laData[14]
  STORE lcODefWare TO laData[31]
  IF !EMPTY(laData[26])
    STORE IIF(laSetups[7,2] $ 'YF' ,'H','O') TO laData[5]
  ELSE
    STORE IIF(laSetups[7,2]='Y' ,'H','O') TO laData[5]
  ENDIF
  STORE Customer.Priority TO laData[25]
  SHOW GET lnOrdStatus ENABLE
  laData[33] = IIF(EMPTY(laData[33]),gcBaseCurr,laData[33])
  IF laData[33] = gcBaseCurr
    STORE 1 TO laData[34], laData[50]
  ELSE
    laData[34]=gfChkRate('laData[50]',laData[33],gdSysDate,.T.,.F.,.F.,llEditExRt)
    IF laData[34] = 0
      IF llEditExRt
        *E300420,1 Message : 00262
        *E300420,1 A valid xxx to xxx exchange rate could not be found on xxx.
        *E300420,1 Button : 00000
        *E300420,1 Ok
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
  =lfvCommision()
  SELECT 'UNCMSESS'
  IF SEEK('I'+PADR("SOMAT",10)+PADR(gcUser_id,10)) AND RLOCK()
    BLANK
  ELSE
    APPEND BLANK
  ENDIF
  REPLACE Status     WITH 'O'       ,;
          cUTranType WITH lcProgID  ,;
          cUserId    WITH gcUser_id ,;
          cSession   WITH lcSession ,;
          cProgram   WITH 'SOMAT'   ,;
          cCurrScr   WITH 'SOMAT'   ,;
          dTranDate  WITH gdSysDate ,;
          cTranTime  WITH TIME()
  =RLOCK()
  lcFiles = 'lcOrdHdr,'+lcOrdHdr+','+lcOrdHdr+';'+;
            'lcOrdLine,'+lcOrdLine+',MASOLIN;'
  =gfSavSess(lcProgID, lcFiles, @laVariables,lcSession)
  SELECT (lcOrdHdr)
  SHOW GET lnOrdStatus ENABLE  
  SHOW GET ibStore     ENABLE
  SHOW GET laData[3]   ENABLE
  SHOW GET laData[4]   ENABLE  
  SHOW GET llMultiSt   ENABLE  
  llCUpDate = .T.
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

*!*************************************************************
*! Name      : lfvMultiSt
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate multi stores check box
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvMultiSt()
*!*************************************************************     
FUNCTION lfvMultiSt

IF llMultiSt
  IF !SEEK('S'+laData[2],'Customer')
    *E300420,1 Message : 32048
    *E300420,1 No stores found for account xxxxx
    *E300420,1 Button : 00000 
    *E300420,1 Ok
    =gfModalGen('TRM32048B00000','ALERT',laData[2])
  
    STORE .F. TO llMultiSt
    SHOW GET llMultiSt
    RETURN
  ENDIF
  STORE ''  TO lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5
  STORE ''  TO lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5
  STORE SPACE(8) TO laData[3]
  STORE '*' TO laData[12]
  laCodes[2,5] = .T.
  STORE 'At Store Level' TO lcShipName,lcBillName
  SHOW GET ibStore    DISABLE
  SHOW GET laData[3]  DISABLE
  SHOW GET lnShipAddr DISABLE
  SHOW GET laData[7]  ENABLE
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
  SHOW GET lnShipAddr ENABLE
  SHOW GET laData[7]  DISABLE
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

*!*************************************************************
*! Name      : lfvStore
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate Invoice store
*!*************************************************************
*! Calls     : CUSBROWS,gfGetAdr,gfChkRate,gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvStore()
*!*************************************************************
FUNCTION lfvStore
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
    *B803068,1 ABD [ End ]
    *E300420,1 Message : 32023
    *E300420,1 This a non-active xxxxx. Order entry is not allowed!
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('TRM32023B00000','ALERT','store'+'|'+IIF(lcOrdType='C','Contract','CMORDER'))
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
        gfModalGen('QRM32024B32000','ALERT',IIF(lcOrdType='C','contract','Order')+'|'+'customer')=1
        *E300420,1 Message : 32024
        *E300420,1 Overwrite Order header with xxxxx defaults?
        *E300420,1 Button : 32000
        *E300420,1 Yes;No
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
              REP_DIV.Comm_Rate,Customer.Comm) TO laData[28]
        STORE IIF(laSetups[11,2]='D' .AND. SEEK(laData[29]+laData[15],'REP_DIV'),;
              REP_DIV.Comm_Rate,Customer.Comm2) TO laData[30]
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
        gfModalGen('QRM32024B32000','ALERT',IIF(lcOrdType='C','contract','Order')+'|'+'store')=1
        *E300420,1 Message : 32024
        *E300420,1 Overwrite CMORDER header with xxx defaults?
        *E300420,1 Button : 32000
        *E300420,1 Yes;No
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
              REP_DIV.Comm_Rate,Customer.Comm) TO laData[28]
        STORE IIF(laSetups[11,2]='D' .AND. SEEK(laData[29]+laData[15],'REP_DIV'),;
              REP_DIV.Comm_Rate,Customer.Comm2) TO laData[30]
        
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
  *E300420,1 Message : 32012
  *E300420,1 Change alternate shipto to new store shipto?
  *E300420,1 Button : 32000
  *E300420,1 Yes/No
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
    WAIT 'Update CMORDER lines...' WINDOW NOWAIT
    SELECT (lcOrdLine)
    lcCurrRec = EVAL(SYS(14,VAL(SYS(21))))
    SCAN REST WHILE cOrdType+CMORDER = lcOrdType+laData[1]
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

*!*************************************************************
*! Name      : lfvMultiPo
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate multi customer po number check box
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvMultiPo()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfvCustPo
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate customer po number
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvCustPo()
*!*************************************************************
FUNCTION lfvCustPo
PRIVATE llCustPo,lcAlias
lcAlias = ALIAS()

DO CASE
  CASE laScrMode[1] .AND. (!EMPTY(laData[2]) .OR. !EMPTY(laData[4]))
    llBrowse = .T.
    _CUROBJ = OBJNUM(laData[1])
    KEYBOARD "{ENTER}"
    RETURN
    
  CASE (laScrMode[3] OR laScrMode[4]) .AND. laData[4] <> &lcOrdHdr..CustPo

    IF !EMPTY(laData[4])
      SET ORDER TO TAG ORDCUST IN MASOHDR
      llCustPo =SEEK(laData[2]+UPPER(laData[4])+lcOrdType,'MASOHDR')
      SET ORDER TO TAG MASOHDR IN MASOHDR
      *E300420,1 Message : 32025
      *E300420,1 Cust. PO# xxx has been entered before.
      *E300420,1 Button : 32004
      *E300420,1 Proceed;Reenter
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
    
    IF !laData[7]
      PRIVATE lcKeyValue
      SELECT (lcOrdLine)
      lcKeyValue = EVALUATE(KEY())
      REPLACE ALL CustPo WITH laData[4]
      =SEEK(lcKeyValue)
    ENDIF  
        
ENDCASE

SELECT (lcAlias)

*!*************************************************************
*! Name      : lfvEntered
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Valid function for CMORDER entered date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvEntered()
*!*************************************************************
FUNCTION lfvEntered

IF laData[8] <> &lcOrdHdr..Entered
  llCUpdate = .T.
  IF laData[33] <> gcBaseCurr
    laData[34] = gfChkRate('laData[50]',laData[33],laData[8],.T.,gcAct_Comp,.F.,llEditExRt)
    IF laData[34] = 0 
      IF llEditExRt
        *E300420,1 Message : 00262
        *E300420,1 A valid xxx to xxx exchange rate could not be found on xxx.
        *E300420,1 Button : 00000
        *E300420,1 Ok
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

*!*************************************************************
*! Name      : lfvStart
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Valid function for CMORDER start date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvStart()
*!*************************************************************
FUNCTION lfvStart

IF laData[9] <> &lcOrdHdr..Start
  IF laData[9] > laData[10]
    * --  Completion date must follow Start date. It will dedfault to Start date.
    * --  Change the button to be "Proceed - Reenter", 
    * --  change the complete date if the user chose Proceed
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

*!*************************************************************
*! Name      : lfvComplete
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Valid function for CMORDER completion date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvComplete()
*!*************************************************************
FUNCTION lfvComplete

IF laData[10] <> &lcOrdHdr..Complete
  IF laData[9] > laData[10]
    *Message : 32013
    *          The starting date cannot be greater than the completion date.
    * Button : 00000
    * Ok
    =gfModalGen('TRM32013B00000','ALERT')
    laData[10] = &lcOrdHdr..Complete
    RETURN
  ENDIF
  IF  gcContCode # 'EGYPT'
    IF CDOW(laData[10])='Saturday' .OR. CDOW(laData[10])='Sunday'
      * Message : 32014
      * The completion date is a xxxxx.
      * Button : 00000
      * Ok
      =gfModalGen('INM32014B00000','ALERT',CDOW(laData[10]))
    ENDIF
  ENDIF
  llCUpdate = .T.
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Complete WITH laData[10]
  UNLOCK
ENDIF

*!*************************************************************
*! Name      : lfvShipAddr
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Switch between default and alternative shipping adresses
*!*************************************************************
*! Calls     : gfActPop
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvShipAddr()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfvShipping
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Valid alternative shipping address information
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcAddressNo : Alternative address line #
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvShipping()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfvTerms
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Valid CMORDER terms code
*!*************************************************************
*! Calls     : CODECHK
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvTerms()
*!*************************************************************
FUNCTION lfvTerms

IF laData[11] <> ALLTRIM(laTerms[lnTerms,2])
  laData[11] = ALLTRIM(laTerms[lnTerms,2])
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE cTermCode WITH laData[11]
  UNLOCK
  llCUpdate = .T.
ENDIF

*!*************************************************************
*! Name      : lfvShipVia
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate CMORDER shipvia
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvShipVia()
*!*************************************************************
PROCEDURE lfvShipVia
IF !( ALLTRIM(laData[12]) == ALLTRIM(laShipVia[lnShipVia,2]) ) 
  laData[12] = ALLTRIM(laShipVia[lnShipVia,2])
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE ShipVia WITH laData[12]
  UNLOCK
  llCUpdate = .T.
ENDIF

*!*************************************************************
*! Name      : lfvSpcInst
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Valid CMORDER special instruction code
*!*************************************************************
*! Calls     : CODECHK
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSpcInst()
*!*************************************************************
FUNCTION lfvSpcInst

IF laData[13] <> ALLTRIM(laSpcInst[lnSpcInst,2])
  laData[13] = ALLTRIM(laSpcInst[lnSpcInst,2])
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE SpcInst WITH laData[13]
  UNLOCK
  llCUpdate = .T.
ENDIF

*!*************************************************************
*! Name      : lfvSeason
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Valid CMORDER season code
*!*************************************************************
*! Calls     : CODECHK
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSeason()
*!*************************************************************
FUNCTION lfvSeason

IF laData[14] <> SUBSTR(laSeasons[lnSeason,2],1,6)
  laData[14] = SUBSTR(laSeasons[lnSeason,2],1,6)
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Season WITH laData[14]
  UNLOCK
  llCUpdate = .T.
ENDIF

*!*************************************************************
*! Name      : lfvDivision
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Valid CMORDER division code
*!*************************************************************
*! Calls     : CODECHK,lfvCommision
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDivision()
*!*************************************************************
FUNCTION lfvDivision

IF laData[15] <> SUBSTR(laDivision[lnDivision,2],1,6)
  laData[15] = SUBSTR(laDivision[lnDivision,2],1,6)
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
    
    STORE IIF(SEEK(laData[27]+laData[15],'REP_DIV'),REP_DIV.Comm_Rate,Customer.Comm) ;
          TO laData[28]
    STORE IIF(SEEK(laData[29]+laData[15],'REP_DIV'),REP_DIV.Comm_Rate,Customer.Comm2) ;
          TO laData[30]
    =gfCloseFile('REP_DIV')

    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE Comm1 WITH laData[28] ,;
            Comm2 WITH laData[30]
    UNLOCK
    =lfvCommision()
  ENDIF
ENDIF

*!*******************************************************************************
*! Name      : lfwDept
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : When function for Department field.
*!*******************************************************************************
*! Reference : B604262,1
*!*******************************************************************************
*! Example   : =lfwDept()
*!*******************************************************************************
*
FUNCTION lfwDept
lcOldDept = laData[17]
*-- End of lfwDept.

*!*************************************************************
*! Name      : lfvDept
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate department
*!*************************************************************
*! Calls     : gfModalGen,ARIABROW,lfvCommision
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDept()
*!*************************************************************
*
FUNCTION lfvDept

IF !llBrowse .AND. laData[17] = &lcOrdHdr..Dept
  RETURN
ENDIF
=gfOpenFile(gcDataDir+'CustDept',gcDataDir+'CustDept','SH')
IF llBrowse .OR. (!EMPTY(laData[17]) .AND. !SEEK(laData[2]+laData[17],'CustDept'))
  IF !SEEK(laData[2],'CustDept')
    IF llBrowse
      *--Message : 32026
      *--No departments assigend to account xxx
      *--Button : 00000
      *--ok
      =gfModalGen('TRM32026B00000','ALERT',laData[2])
      STORE SPACE(5) TO laData[17]
    ENDIF
    llBrowse = .F.
    RETURN
  ENDIF
  SELECT CustDept
  lcBrFields = [Dept:H="Department",cDeptDesc :H="Description",Rep1,Comm1,Rep2,Comm2,cTermCode]
  laData[17] = IIF(ARIABROW('laData[2]',"Departments",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Dept','laBrowArr'),;
                  CustDept.Dept,laData[17])
ENDIF
llBrowse = .F.

IF laData[17] <> &lcOrdHdr..Dept AND SEEK(laData[2]+laData[17],'CustDept')

  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE Dept  WITH laData[17]
  UNLOCK
  *-- If valid epartment, Get new sales rep, commission and payment terms
  IF SEEK(laData[2]+laData[17],'CustDept')
    *--Message : 32027
    *--Customer defaults will be overwritten by the department defaults.
    *--Button : 00000
    *--ok
    IF gfModalGen('INM32027B00000','ALERT')=1
      =RLOCK()
      REPLACE Rep1  WITH CustDept.Rep1 ,;
              Rep2  WITH CustDept.Rep1 ,;
              cTermCode WITH CustDept.cTermCode
      UNLOCK
      =gfwCodePop(@laCodes,'CTERMCODE','T')
      STORE CustDept.Rep1  TO laData[27]
      STORE CustDept.Rep2  TO laData[29]
      =lfvRep1() .AND. lfvRep2()
      STORE CustDept.Comm1 TO laData[28]
      STORE CustDept.Comm2 TO laData[30]
      SHOW GET laData[27]
      SHOW GET laData[28]
      SHOW GET laData[29]
      SHOW GET laData[30]
      =lfvCommision()
    ENDIF
  ENDIF
  llCUpdate = .T.

ELSE
  
  laData[17] = IIF(!EMPTY(laData[17]),lcOldDept,'')

ENDIF
=gfCloseFile('CustDept')

*!*************************************************************
*! Name      : lfUpdate
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Update CMORDER header other information
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdate()
*!*************************************************************
FUNCTION lfUpdate

IF UPDATE()
  SELECT (lcOrdHdr)
  =RLOCK()  
  GATHER FROM laData FIELDS &lcScFields
  UNLOCK
  llCUpdate = .T.
ENDIF

*!*************************************************************
*! Name      : lfvRep1
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate first salesrep
*!*************************************************************
*! Calls     : REPCHK
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRep1()
*!*************************************************************
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
      STORE REP_DIV.Comm_Rate TO laData[28]
    ELSE
      laData[28] = IIF(EMPTY(laData[27]),0,SalesRep.Comm)
    ENDIF
  ELSE
    laData[28] = IIF(EMPTY(laData[27]),0,IIF(Customer.salesrep # laData[27],SalesRep.Comm,CUSTOMER.Comm))
  ENDIF
  SELECT (lcOrdLine)
  lnRecno = RECNO()
  IF SEEK(&lcOrdHdr..CordType+&lcOrdHdr..CMORDER)
    SCAN WHILE CordType+CMORDER+STR(LineNo,6) = &lcOrdHdr..CordType+&lcOrdHdr..CMORDER
      M.Comm1 = laData[28]
      =lfVComm('1')
    ENDSCAN  
  ENDIF  
  SELECT (LCOrdLine)
  GO TOP
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

*!*************************************************************
*! Name      : lfvRep2
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate second salesrep
*!*************************************************************
*! Calls     : REPCHK
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRep2()
*!*************************************************************
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
      STORE REP_DIV.Comm_Rate TO laData[30]
    ELSE
      laData[30] = IIF(EMPTY(laData[29]),0,SalesRep.Comm)
    ENDIF
  ELSE
    laData[30] = IIF(EMPTY(laData[29]),0,IIF(Customer.salesrep # laData[29],SalesRep.Comm,CUSTOMER.Comm2))
  ENDIF
  SELECT (lcOrdLine)
  lnRecno = RECNO()
  IF SEEK(&lcOrdHdr..CordType+&lcOrdHdr..CMORDER)
    SCAN WHILE CordType+CMORDER+STR(LineNo,6) = &lcOrdHdr..CordType+&lcOrdHdr..CMORDER
      M.Comm2 = laData[30]
      =lfVComm('2')
  	ENDSCAN  
  ENDIF  
  SELECT (lcOrdLine)
  GO TOP
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

*!*************************************************************
*! Name      : lfvWareCode
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate CMORDER warehouse code
*!*************************************************************
*! Calls     : gfBrowWare
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvWareCode()
*!*************************************************************
FUNCTION lfvWareCode

laData[31] = laWareHouses[lnWareHouse,2]
IF &lcOrdHdr..cWareCode <> laData[31]
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE cWareCode WITH laData[31]
  UNLOCK
  llCUpdate = .T.
ENDIF

*!*************************************************************
*! Name      : lfvCurrency
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Valid function for currency fields
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCurrency()
*!*************************************************************
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
        *--Message : 00262
        *--A valid xxx to xxx exchange rate could not be found on xxx.
        *--Button : 00000
        *--Ok
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

*!*************************************************************
*! Name      : lfvExRate
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Valid function for currency exchange rate.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvExRate()
*!*************************************************************
FUNCTION lfvExRate

IF laData[34] <=0
  *--Message : 32028
  *--The exchange rate must be greater than zero
  *--Button : 00000 
  *--Ok
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

*!*************************************************************
*! Name      : lfvNew
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : New CMORDER line
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvNew()
*!*************************************************************
FUNCTION lfvNew
STORE .T. TO llNewline,llAddLine
SCATTER MEMVAR BLANK MEMO

STORE ' ' TO m.Fabric,m.Color,m.Pattern,m.Width,m.ClrDesc,m.IDesc
SHOW GET ClrDesc
SHOW GET IDesc
m.Store = laData[3]
=SEEK(m.Fabric+m.Color,'FABRIC')
=SEEK(m.Fabric+m.Color+laData[31],'FABDYE')
SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY
SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY
SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
=lfRefresh(lcWinCh32)
=lfRefresh(lcWinCh321)

IF llMultiSt
  m.Store = lcLastStore
  SHOW GET ibLStore ENABLE
  SHOW GET m.Store  ENABLE
  _CUROBJ = OBJNUM(m.Store)
ELSE
  SHOW GET ibFabric ENABLE
  SHOW GET m.Fabric ENABLE
  SHOW GET ibColor  ENABLE
  SHOW GET m.Color  ENABLE
  _CUROBJ = OBJNUM(m.Fabric)
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfwCustPo
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : When function for CMORDER lines CustPOs
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfwCustPo()
*!*************************************************************
FUNCTION lfwCustPo
lcOldCustPo = CustPo

*!*************************************************************
*! Name      : FUNCTION lfvLCustPo
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate Order lines CustPOs
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvLCustPo()
*!*************************************************************
FUNCTION lfvLCustPo
PRIVATE lnCurrTag,lcCurrExp,lcStore,lcCustPo

IF CustPo <> lcOldCustPo
  llCUpDate = .T.
  *--Message : 32046
  *--Change Custpo# for all lines in store xxxxx.
  *--Button : 32000
  *--Yes;No
  IF gfModalGen('QRM32046B32000','ALERT',store)=1
    lnCurrTag = VAL(SYS(21))
    lcCurrExp = EVAL(SYS(14,lnCurrTag))
    lcStore  = Store
    lcCustPo = CustPo
    SET ORDER TO TAG MASOLINST
    =SEEK(lcOrdType+laData[1]+lcStore)
    SCAN REST WHILE cOrdType+CMORDER+STORE = lcOrdType+laData[1]+lcStore
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

*!*************************************************************
*! Name      : lfvLStore
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate CMORDER line store
*!*************************************************************
*! Calls     : CUSBROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvLStore()
*!*************************************************************
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
    *--Message : 32023
    *--This a non-active xxxxx. ORDER entry is not allowed!
    *--Button : 00000
    *--Ok
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
    _CUROBJ = OBJNUM(pbNew)
  ELSE
    SHOW GETS WINDOW (lcWinCh32)  ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh321) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh322) ENABLE ONLY
    _CUROBJ = OBJNUM(ibBrowOrd)
  ENDIF
  RETURN
ELSE
  IF llAddLine 
    SHOW GET ibFabric ENABLE
    SHOW GET m.Fabric ENABLE
    SHOW GET ibColor  ENABLE
    SHOW GET m.Color  ENABLE
    _CUROBJ = OBJNUM(m.Fabric)
  ELSE
    llCUpDate = .T.
    =RLOCK()
    REPLACE Store WITH m.Store ,;
            FLAG WITH IIF(FLAG <> 'N','M',FLAG)
    UNLOCK
  ENDIF
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvPieces
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate Fabric pieces
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPieces()
*!*************************************************************
FUNCTION lfvPieces
PARAMETERS llStores

IF m.FabQty < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.FabQty = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
    
IF m.FabQty=FabQty
  RETURN
ENDIF

=lfChkTotQty()

SHOW GETS WINDOW (lcWinCh32)  ONLY
SHOW GETS WINDOW (lcWinCh321) ONLY
SHOW GETS WINDOW (lcWinCh322) ONLY
=lfRefresh(lcWinCh32)
=lfRefresh(lcWinCh321)

*!*************************************************************
*! Name      : FUNCTION lfvOrdCanLn
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Update CMORDER line canceled reason and date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvOrdCanLn()
*!*************************************************************
FUNCTION lfvOrdCanLn
CLEAR READ
SELECT (lcOrdCanLn)
=SEEK(lcOrdType+laData[1]+STR(m.LineNo,6))
REPLACE cCancReson WITH laCanReason[lnCanReason,2],;
        Cancelled  WITH m.Cancelled
SELECT (lcOrdLine)

*!*************************************************************
*! Name      : FUNCTION lfvStyDesc
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate Styl description
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyDesc()
*!*************************************************************
FUNCTION lfvStyDesc
PARAMETERS llStores

IF Desc1 <> m.Desc1
  llCUpdate = .T.
  =RLOCK()  
  REPLACE Desc1 WITH m.Desc1 ,;
          Flag  WITH IIF(Flag='N' .OR. llStores,Flag,'M')
  UNLOCK
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvComm
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate Styl salesrep commissions
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lnSalesRep   : Salesrep number
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvComm()
*!*************************************************************
FUNCTION lfvComm
PARAMETERS lcSalesRep,llStores
IF Comm&lcSalesRep <> m.Comm&lcSalesRep
  llCUpdate = .T.
  =RLOCK()  
  REPLACE Comm&lcSalesRep WITH m.Comm&lcSalesRep ,;
          Flag  WITH IIF(Flag='N' .OR. llStores,Flag,'M')
  UNLOCK
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfwGPrice 
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : When function for Styl Gross price
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfwGPrice()
*!*************************************************************
FUNCTION lfwGPrice
*N125956,1 TMI 03/10/2005 [Start] If this is a contract ( in Add mode ) , do not update entered price
IF lContract
  RETURN .F.
ENDIF  
*N125956,1 TMI [End] 

*!*************************************************************
*! Name      : FUNCTION lfvGPrice
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate fabric Gross price
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvGPrice()
*!*************************************************************
FUNCTION lfvGPrice
PRIVATE lcOrdAlias

IF m.nSellPrice < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.nSellPrice = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF m.nSellPrice <> nSellPrice
  *E126684,1  TMI [Start] get net sell price from sell price ( round up to 5)
  *m.nSellNetPr = ROUND(m.nSellPrice*(100-m.Disc_Pcnt)/100,2)
  m.nSellNetPr = CEILING(100000 * m.nSellPrice*(100-m.Disc_Pcnt)/100) / 100000
  *E126684,1  TMI [End  ] 
  =lfvNPrice()
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvPrcDisc
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate fabric price discount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPrcDisc()
*!*************************************************************
FUNCTION lfvPrcDisc
 
IF m.Disc_Pcnt <> Disc_Pcnt
  *E126684,1  TMI [Start] get net sell price from sell price ( round up to 5)
  *m.nSellNetPr = ROUND(m.nSellPrice*(100-m.Disc_Pcnt)/100,2)
  m.nSellNetPr = CEILING(100000 * m.nSellPrice*(100-m.Disc_Pcnt)/100 ) / 100000
  *E126684,1  TMI [End  ] 
  =lfvNPrice(.F.,.T.)
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvNPrice
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate Styl Net price
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvNPrice()
*!*************************************************************
FUNCTION lfvNPrice
PARAMETERS llStores,llPercent

IF m.nSellNetPr < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.nSellNetPr = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

m.nSellNetPr = IIF(m.nSellNetPr>m.nSellPrice,nSellNetPr,m.nSellNetPr)
IF m.nSellNetPr <> nSellNetPr
  STORE .T. TO llCUpdate
  SELECT (lcOrdLine)
  
  *E126684,1  TMI [Start] accomulate booked and open amnt
  *laData[36] = laData[36] + ROUND(FABBook*(m.nSellNetPr - nSellNetPr),2)
  *laData[42] = laData[42] + ROUND(FabQty*(m.nSellNetPr - nSellNetPr),2)
  laData[36] = laData[36] + CEILING(FABBook*m.nSellNetPr*100)/100 - CEILING(FABBook*nSellNetPr*100)/100
  laData[42] = laData[42] + CEILING(FabQty*m.nSellNetPr*100)/100 - CEILING(FabQty*nSellNetPr*100)/100
  *E126684,1  TMI [End  ] 

  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE FABBOOKAMT WITH laData[36] ,;
          FABOPENAMT WITH laData[42]
  UNLOCK
  SELECT (lcOrdLine)
  *m.Disc_Pcnt = IIF(llPercent,m.Disc_Pcnt,IIF(m.nSellPrice=0,0,100-m.nSellNetPr*100/m.nSellPrice))
  =RLOCK()

  REPLACE nSellPrice WITH m.nSellPrice ,;
          Disc_Pcnt  WITH m.Disc_Pcnt  ,;
          nSellNetPr WITH m.nSellNetPr ,;
          Flag       WITH IIF(Flag='N',Flag,'M')  

  UNLOCK
  SHOW GET m.nSellPrice
  SHOW GET m.Disc_Pcnt
  SHOW GET m.nSellNetPr
  SHOW WINDOW (lcOrdBrow) REFRESH SAME

ENDIF

*:**************************************************************************
*:* Name        : lfvSellCnv
*:* Developer : TMI  -  Tarek Mohamed Ibraheem
*:* Date        : 05/30/2002
*:* Purpose     : Valid function for nSellConv field
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvSellCnv()
*:***************************************************************************
FUNCTION lfvSellCnv
IF m.nSellConv <=0
  =gfModalGen('TRM36014B36000','ALERT','Conversion factor')
  m.nSellConv = lcOldValue  
  _CUROBJ = OBJNUM(m.nSellConv)
  RETURN
ENDIF
IF m.nSellConv <> nSellConv
  REPLACE nSellConv WITH m.nSellConv
ENDIF
*-- end of lfvSellCnv.

*!*************************************************************
*! Name      : FUNCTION lfvCopyQty
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Copy line quantities
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvCopyQty()
*!*************************************************************
FUNCTION lfvCopyQty
SCATTER FIELDS FABQty TO laQtyPaste

*!*************************************************************
*! Name      : FUNCTION lfvPasteQty
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Paste line quantities
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPasteQty()
*!*************************************************************
FUNCTION lfvPasteQty
PARAMETERS llStores

IF laQtyPaste[1]=0
  *E300408,1 Message : 32033
  *E300408,1 No line quantities copied to paste!
  *E300408,1 Button : 00000 
  *E300408,1 ok
  =gfModalGen('TRM32033B00000','ALERT')
  RETURN
ENDIF
SELECT (lcOrdLine)
STORE 0 TO m.FABQTY
m.FABQty = laQtyPaste[1]
=lfChkTotQty(llStores)
SCATTER MEMVAR
SHOW GETS WINDOW (lcWinCh32)  ONLY
SHOW GETS WINDOW (lcWinCh321) ONLY
SHOW GETS WINDOW (lcWinCh322) ONLY
=lfRefresh(lcWinCh321)
SHOW WINDOW (lcOrdBrow) REFRESH SAME
_CUROBJ=OBJNUM(m.FABQty)

*!*************************************************************
*! Name      : lfvLineNote
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : CMORDER line notepad
*!*************************************************************
*! Calls     : ARLNOTES.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvLineNote()
*!*************************************************************
FUNCTION lfvLineNote
PARAMETERS llStores

DO (gcScrDir+"ARLNOTES.SPX")

IF llStores AND USED(lcTempline) OR USED(lcOrdLine)
  SELECT IIF(llStores,lcTempline,lcOrdLine)
  =RLOCK()
  REPLACE Flag WITH IIF(Flag='N' .OR. llStores,Flag,'M')
  UNLOCK
ENDIF
*!*************************************************************
*! Name      : lfvOrdNote
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : CMORDER notepad
*!*************************************************************
*! Calls     : NOTEPAD
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvOrdNote()
*!*************************************************************
FUNCTION lfvOrdNote
=NOTEPAD('V',IIF(lcOrdType='T','T','')+laData[1])
*!*************************************************************
*! Name      : lfvRemove
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Remove CMORDER line
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvMRemove()
*!*************************************************************
FUNCTION lfvRemove
PARAMETERS llStores

*E300408,1 Message : 32035
*E300408,1 Are you sure you want to remove this order line?
*E300408,1 Button : 32000
*E300408,1 Yes No
IF gfModalGen('QRM32035B32000','ALERT',IIF(lcOrdType='C','contract','order'))= 1

  IF llStores
    lnTotPcs = lnTotPcs - m.FabQty
    lnTotAmt = lnTotAmt - m.FabQty*m.nSellNetPr
  ELSE
    SELECT (lcOrdLine)
    =RLOCK()    
    REPLACE FLAG WITH IIF(FLAG='N',FLAG,'M')
    UNLOCK

    laData[35] = laData[35] - IIF(FLAG='N' OR llUpdBook,m.FABQty,0)
    *E126684,1  TMI [Start] use CEILING instead of ROUND
    *laData[36] = laData[36] - IIF(FLAG='N' OR llUpdBook,ROUND(m.FABQty*m.nSellNetPr,2),0)
    laData[36] = laData[36] - IIF(FLAG='N' OR llUpdBook,CEILING(m.FABQty*m.nSellNetPr*100)/100,0)
    *E126684,1  TMI [End  ] 
    laData[39] = laData[39] + IIF(FLAG='N' OR llUpdBook,0,m.FABQTy)
    *E126684,1  TMI [Start] use CEILING instead of ROUND
    *laData[40] = laData[40] + IIF(FLAG='N' OR llUpdBook,0,ROUND(m.FABQTy*m.nSellNetPr,2))
    laData[40] = laData[40] + IIF(FLAG='N' OR llUpdBook,0,CEILING(m.FABQTy*m.nSellNetPr*100)/100 )
    *E126684,1  TMI [End  ] 
    laData[41] = laData[41] - m.FABQty
    *E126684,1  TMI [Start] use CEILING instead of ROUND
    *laData[42] = laData[42] - ROUND(m.FABQty*m.nSellNetPr,2)
    laData[42] = laData[42] - CEILING(m.FABQty*m.nSellNetPr*100)/100
    *E126684,1  TMI [End  ] 
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE FABBOOK      WITH laData[35] ,;
            FABBOOKAMT   WITH laData[36] ,;
            FABCANCEL    WITH laData[39] ,;
            FABCANAMT    WITH laData[40] ,;
            FABOPEN      WITH laData[41] ,;
            FABOPENAMT   WITH laData[42]
    UNLOCK
    IF laScrMode[3] AND !llUpdBook
      =gfwCodePop(@laCodes,'CCANCRESON','D')
      INSERT INTO (lcOrdCanLn) ;
       (cOrdType,CMORDER,LineNo,Cancelled,cCancReson,FabQty,;
        Fabric,Color,Account,Store,Dyelot,nSellNetPr) VALUES ;
       (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2],m.FabQty,;
        m.Fabric,m.Color,m.Account,m.Store,m.Dyelot,m.nSellNetPr)      
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
  SELECT IIF(llStores,lcTempLine,lcOrdLine)
  DELETE
  SKIP -1
  IF BOF()
    GO TOP
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
    =lfRefresh(lcWinCh321)
    _CUROBJ = OBJNUM(ibBrowOrd)
  ENDIF
  IF EOF()
    SHOW GET llMultiSt ENABLE
    SHOW GET pbNew     ENABLE 
   _CUROBJ = OBJNUM(pbNew)
  ENDIF
  STORE .T. TO llCUpdate
ENDIF

*!*************************************************************
*! Name      : lfvCommision
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Recalculate CMORDER commissions
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCommision()
*!*************************************************************
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
  *B802650,1 AMM end
  
  
  IF laData[28] <> &lcOrdHdr..Comm1
    *E300420,1 Message : 32036
    *E300420,1 xxx commission decreased to xxx%
    *E300420,1 Button : 00000
    *E300420,1 Ok
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
    *E300420,1 Message : 32036
    *E300420,1 xxx commission decreased to xxx%
    *E300420,1 Button : 00000
    *E300420,1 Ok
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

*!*************************************************************
*! Name      : lfvApp2St
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Apply template order lines to stores
*!*************************************************************
*! Calls     : SOSTORE.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvApp2St()
*!*************************************************************
FUNCTION lfvApp2St

STORE 0 TO lnTopMark,lnBotMark,lnSTotPcs,lnSTotAmt
DIMENSION laKeyExp[1]
laKeyExp[1] = 'ACCOUNT'
SELECT *,000000 AS nSesUsed FROM (lcTempline) WHERE Flag = "�" INTO DBF (gcWorkDir+lcSelLines)
SUM FabQty,FabQty*nSellNetPr TO lnSelPcs,lnSelAmt
SET RELATION TO FABRIC+COLOR INTO FABRIC
GO TOP
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

*!*************************************************************
*! Name      : lfWStores
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Browse function for stores screen (SOSTORE)
*!*************************************************************
*! Calls     : lfShowStore
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfWStores()
*!*************************************************************
FUNCTION lfWStores
PRIVATE lcFields 

lcFields = [cMarker =IIF(RECNO()=lnTopMark,'>',' '):H=' ':R:1:W=.F.,]+;
           [Fabric  :H=' Fabric ' :R :7,]
*E126684,1  TMI [Start] use CEILING instead of ROUND
*lcFields = lcFields + [Fabric.Desc :H='Description':R :20,]+;
                      [nSellPrice :H='Gross Price' :R,]+;
                      [Disc_Pcnt :H='Discount' :R,nSellNetPr :H='Net Price' :R :9,]+;
                      [FabQty :H='Pieces' :R :6,]+;
                      [Amount  =ROUND(nSellNetPr*FabQty,2) :P= '99999999999.99' :H='Amount']
lcFields = lcFields + [Fabric.Desc :H='Description':R :20,]+;
                      [nSellPrice :H='Gross Price' :R,]+;
                      [Disc_Pcnt :H='Discount' :R,nSellNetPr :H='Net Price' :R :9,]+;
                      [FabQty :H='Pieces' :R :6,]+;
                      [Amount  =CEILING(nSellNetPr*FabQty*100)/100 :P= '99999999999.99' :H='Amount']
*E126684,1  TMI [End  ] 
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
lcBrFields = lcBrFields + "n=Pieces*Count :H='Pieces' :P='999999.999' :R,;
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

*!*************************************************************
*! Name      : lfShowStore
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Show function for stores screen (soStore)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfShowStore()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfDStores
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Deactivate function for stores screen (soStore)
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfDStores()
*!*************************************************************
FUNCTION lfDStores

IF WONTOP()=lcBrTtS1 .OR. WONTOP()=lcBrTtS2 
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
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


*!*************************************************************
*! Name      : lfVBrTtS1
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Valid function Of the first browse in MULTI store screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  
*!*************************************************************

FUNCTION lfVBrTtS1
IF WONTOP() = lcBrTtS2
  lcBrowseTl=lcBrTtS2
  =lfReadAct()
  =lfDStores()
ENDIF

*!*************************************************************
*! Name      : lfVBrTtS2
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Valid function Of the second browse in MULTI store screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  
*!*************************************************************
FUNCTION lfVBrTtS2
IF WONTOP() = lcBrTtS1
  lcBrowseTl=lcBrTtS1
  =lfReadAct()
  =lfDStores()  
ENDIF

*!*************************************************************
*! Name      : lfAdjSButt
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Select, Unselect, Select all, Select None and Invert stores
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfAdjSButt()
*!*************************************************************
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
REPLACE ALL nSesUsed WITH FabQty*lnStCount
GO lnRecNo
SELECT (lcStores)
SHOW GETS WINDOW 'SoStore2' OFF

*!*************************************************************
*! Name      : lfvSApply
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Append selected stores to the ORDER line temp file
*!*************************************************************
*! Calls     : lfAdjSButt
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvSApply()
*!*************************************************************
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
      WAIT 'Adding store/Fabric : '+&lcStores..Store+'/'+m.Fabric WINDOW NOWAIT
      lnLines = lnLines + 1
      
      INSERT INTO (lcOrdLine) ;
      (cOrdType,CMORDER,Account,LineNo,Store,Fabric,Color,Desc1,Season,nSellNetPr,;
       nSellPrice,Disc_Pcnt,FabQty,;
       FabBook,Note_Mem,;
       Comm1,Comm2,Pack_Id,cFromOrder,BulkLineNo,CustPo,Flag,cWareCode,Gl_Sales,Complete) VALUES ;
      (lcOrdType,laData[1],laData[2],lnLines,&lcStores..Store,m.Fabric,m.Color,m.Desc1,;
       m.Season,m.nSellNetPr,m.nSellPrice,m.Disc_Pcnt,;
       m.FabQty*lnCount,m.FabQty*lnCount,m.Note_Mem,m.Comm1,;
       m.Comm2,m.Pack_Id,m.cFromOrder,m.BulkLineNo,&lcStores..CustPo,'N',;
       laData[31],m.Gl_Sales,m.Complete)
      IF ASCAN(laEvntTrig , PADR('UPDDYELT',10)) <> 0
        PRIVATE lnOldAlis
        lnOldAlis  =  SELECT(0)
        SELECT (lcOrdLine)
        REPLACE Dyelot WITH m.Dyelot
        SELECT (lnOldAlis)
      ENDIF
      laData[35] = laData[35] + m.FabQty*lnCount
      *E126684,1  TMI [Start] use CEILING instead of ROUND
      *laData[36] = laData[36] + ROUND(m.FabQty*lnCount*m.nSellNetPr,2)
      laData[36] = laData[36] + CEILING(m.FabQty*lnCount*m.nSellNetPr*100)/100
      *E126684,1  TMI [End  ] 
*>>>
      laData[41] = laData[41] + m.FABQty*lnCount
      *E126684,1  TMI [Start] use CEILING instead of ROUND
      *laData[42] = laData[42] + ROUND(m.FABQty*lnCount*m.nSellNetPr,2)
      laData[42] = laData[42] + CEILING(m.FABQty*lnCount*m.nSellNetPr*100)/100
      *E126684,1  TMI [End  ] 
    ENDSCAN
    GO TOP
  ENDSCAN
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE FABBOOK    WITH laData[35] ,;
          FABBOOKAMT WITH laData[36] ,;
          FABOPEN    WITH laData[41] ,;
          FABOPENAMT WITH laData[42]
  UNLOCK
  SELECT (lcSelLines)
  REPLACE ALL nSesUsed WITH 0         
  GO TOP  
  SELECT (lcStores)
  REPLACE ALL cSelect WITH SPACE(1) ,;
              CustPo  WITH SPACE(15),;
              Pieces  WITH 0 ,;
              Amount  WITH 0 ,;
              Count   WITH 0
  GO TOP
  WAIT CLEAR
ENDIF
SET ORDER TO TAG (lcStores) IN (lcStores)
SET ORDER TO TAG MASOLIN IN (lcTempline)
=lfAdjSButt('')

*!*************************************************************
*! Name      : lfvPStore
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate order store
*!*************************************************************
*! Calls     : CUSBROW,gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvPStore()
*!*************************************************************
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
  *E300420,1 Message : 32023
  *E300420,1 This a non-active xxxxx. CMORDER entry is not allowed!
  *E300420,1 Button : 00000
  *E300420,1 Ok
  =gfModalGen('TRM32023B00000','ALERT','store'+'|'+IIF(lcOrdType='C','Contract','CMORDER'))
  STORE SPACE(8) TO xStore
  RETURN
ENDIF

*!*************************************************************
*! Name      : lpSavScr
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Save new or modified CMORDER
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr

DO lfSavScr IN (gcAppHome + 'SO\SOMUPDAT.FXP')

*!*************************************************************
*! Name      : lfGtOrder
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Read order number manually
*!*************************************************************
*! Calls     : ARORDER.SPX
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  CMORDER#
*!*************************************************************
*! Example            :  =lfGtOrder()
*!*************************************************************
FUNCTION lfGtOrder
PRIVATE lcOrderNo

STORE SPACE(6) TO lcOrderNo
*B606634,1 TMI [Start] Call the screen somatord.scx instead 
*DO (gcScrDir+"ARORDER.SPX") WITH 'O','lcOrderNo',lcOrdType
DO (gcScrDir+"SO\SOMATORD.SPX") WITH 'O','lcOrderNo',lcOrdType
*B606634,1 TMI [End  ] 
RETURN(lcOrderNo)


*:**************************************************************************
*:* Name        : lfvOkMat
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/20/2003
*:* Purpose     : Valid Fn. for Ok of the screen somatord.scx
*:***************************************************************************
*:* Called from : SOMATORD.SCX
*:***************************************************************************
*B606634,1
FUNCTION lfvOkMat
IF lcType = 'O' .AND. LEN(ALLTRIM(lcRetValue)) < 6
  *E300408,1 Message : 40000
  *E300408,1 Order# must be six digits.
  *E300408,1 Button : 00000
  *E300408,1 Ok
  =gfModalGen('INM40000B00000','ALERT')
  _CUROBJ = OBJNUM(lcRetValue)
  RETURN
ENDIF
IF lcType = 'O' .AND. SEEK(lcOrdType+lcRetValue,'MASOHDR')
  *E300408,1 Message : 40001
  *E300408,1 Order# xxx already exists in the order file.
  *E300408,1 Button : 00000
  *E300408,1 Ok
  =gfModalGen('INM40001B00000','ALERT',lcRetValue)
  _CUROBJ = OBJNUM(lcRetValue)
  RETURN
ENDIF
&lcValue=lcRetValue
CLEAR READ
*-- end of lfvOk.

*!*************************************************************
*! Name      : lfPrepBrow
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Prepare files and relations according to current selected 
*!             Transaction, sort, total or per size qty and details or summary 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfPrepBrow()
*!*************************************************************
FUNCTION lfPrepBrow
WAIT 'Colecting data...Please standby.' WINDOW NOWAIT
lcForKey = '.T.'
DO CASE
  *-- Show Shipped Transactions
  CASE lnOrdTrans= 3
    =gfOpenFile(gcDataDir+'MINVHDR',gcDataDir+'MINVHDR','SH')
    lcForKey = "MINVHDR.STATUS <> 'V'"
    =gfOpenFile(gcDataDir+'MINVLINE',gcDataDir+'MINVLINE','SH')
    =gfOpenFile(gcDataDir+'consinvl',gcDataDir+'CINVLINE','SH')
  *-- Show Production Transactions
  CASE lnOrdTrans= 4
    =gfOpenFile(gcDataDir+'MORDCNLN',gcDataDir+'MORDCNLN','SH')
    lcTranFile = IIF(laScrMode[2],'MORDCNLN',lcOrdCanLn)
    lcForKey = "&lcOrderFile..Fabric+&lcOrderFile..Color = &lcTranFile..Fabric+&lcTranFile..Color"
  *-- Show Work order
ENDCASE
DO CASE
  *-- Sort by line# - Total quantity
  CASE lnSortBy = 1 
    lcOrderFile = IIF(laScrMode[2] .OR. laScrMode[1],'MASOLIN',lcOrdLine)
    SET ORDER TO TAG 'MASOLIN' IN (lcOrderFile)
    *E126684,1  TMI [Start] use CEILING instead of ROUND
    *lcOrderBr = [Fabric  :R:H=' Fabric ' :7, Color :R:H=' Color ':6,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(llMultiSt,[STORE :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo  :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [nSellPrice :H='Gross Price' :R,] +;
                [Disc_Pcnt :H='Discount' :R,nSellNetPr :H='Price':R:P='999999999.99' :R,] +;
                [FABQTY :H= 'Qty.' :P='999999.999':R ,] +;
                [lnAmount=ROUND(nSellNetPr*FABQTY,2) :H=IIF(MASOHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99']
    lcOrderBr = [Fabric  :R:H=' Fabric ' :7, Color :R:H=' Color ':6,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(llMultiSt,[STORE :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo  :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [nSellPrice :H='Gross Price' :R,] +;
                [Disc_Pcnt :H='Discount' :R,nSellNetPr :H='Price':R:P='999999999.99999' :R,] +;
                [FABQTY :H= 'Qty.' :P='999999.999':R ,] +;
                [lnAmount=CEILING(nSellNetPr*FABQTY*100)/100 :H=IIF(MASOHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99']
    *E126684,1  TMI [End  ] 
    
    DO CASE
      *-- Open quantity
      CASE lnOrdTrans= 1
        SELECT (lcOrderFile)
        IF !laScrmode[1]
          SET RELATION TO FABRIC+COLOR INTO FABRIC
          SET RELATION TO FABRIC+COLOR+laData[31]+SPACE(10) INTO FabDye ADDITIVE
        ENDIF
      *-- Booked quantity
      CASE lnOrdTrans= 2
        IF !USED(lcBooked)
          =gfOpenFile(gcDataDir+'MASOLIN','','SH',@lcBooked,.T.)
        ENDIF
        lcBrTtlT   = lcBrTtlB
        lcTranFile = lcBooked
        SET ORDER TO TAG 'MASOLIN' IN (lcBooked)
        SELECT (lcOrderFile)
        SET RELATION TO cOrdType+CMORDER+STR(LINENO,6) INTO (lcBooked)
        *E126684,1  TMI [Start] use CEILING instead of ROUND
        *lcTranBr = [Fabric :R ,Color :R ,STORE :R ,] +;
                   [nSellNetPr :R :H='Price',FabBook :H='Qty.' :R ,] +;
                   [lnAmount=ROUND(nSellNetPr*FabBook,2) :H='Amount':R:P='99999999999.99']
        lcTranBr = [Fabric :R ,Color :R ,STORE :R ,] +;
                   [nSellNetPr :R :H='Price',FabBook :H='Qty.' :R ,] +;
                   [lnAmount=CEILING(nSellNetPr*FabBook*100)/100 :H='Amount':R:P='99999999999.99']
        *E126684,1  TMI [End  ]                    
      
      *-- Shipped quantity
      CASE lnOrdTrans= 3
        lcBrTtlT = lcBrTtlS
        SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
        IF SEEK(laData[1],'CONSINVL')
          lcTranFile = 'CONSINVL'
        ELSE
          lcTranFile = 'MINVLINE'
          SET ORDER TO TAG 'MINVLINO' IN 'MINVLINE'
        ENDIF
        SELECT (lcTranFile)
        SET RELATION TO CMATINV INTO MINVHDR
        SELECT (lcOrderFile)
        SET RELATION TO CMORDER+STR(LINENO,6) INTO (lcTranFile)
        *E126684,1  TMI [Start] use CEILING instead of ROUND
        *lcTranBr = [CMATINV:H='Invoice#' :R ,INVDATE:H='Date' :R,] +;
                   [STORE :H='Store' :R,FabQty :H='Quantity' :R,nSellNetPr :H='Price' :R,] +;
                   [lnAmount=ROUND(nSellNetPr*FABQTY,2) :H='Amount':R:P='99999999999.99']
        lcTranBr = [CMATINV:H='Invoice#' :R ,INVDATE:H='Date' :R,] +;
                   [STORE :H='Store' :R,FabQty :H='Quantity' :R,nSellNetPr :H='Price' :R,] +;
                   [lnAmount=CEILING(nSellNetPr*FABQTY*100)/100 :H='Amount':R:P='99999999999.99']
        *E126684,1  TMI [End  ]       
        
      CASE lnOrdTrans= 4
        lcBrTtlT   = lcBrTtlC
        lcTranFile = IIF(laScrMode[2],'MORDCNLN',lcOrdCanLn)
        SELECT (lcOrderFile)
        SET RELATION TO cOrdType+CMORDER+STR(LineNo,6) INTO (lcTranFile)
        lcTranBr = [Cancelled :R :H='Date',FabQty :H= 'Total Canceld' :R,]+;
                   [cDesc=gfCodDes(cCancReson,'CCANCRESON') :R :H='Cancelation Reason']

    ENDCASE
  *-- Sort by line# - Quantity per size
  *-- Sort by fabric/color - Total quantity - Details
  CASE lnSortBy = 2 .AND. llDetails
    =IIF(!USED(lcOrdLine),lfCratTemp(),.T.)

    IF !SEEK(lcOrdType+laData[1],lcOrdLine)
      SELECT (lcOrdLine)
      DELETE ALL
      SELECT MASOLIN
      SET ORDER TO TAG MASOLIN
      =SEEK(lcOrdType+laData[1])
      SCAN REST WHILE cordtype+CMORDER+STR(lineno,6) = lcOrdType+laData[1]
        SCATTER MEMVAR MEMO
        INSERT INTO (lcOrdLine) FROM MEMVAR
      ENDSCAN
    ENDIF  
    lcOrderFile = (lcOrdLine)
    SET ORDER TO TAG MASOLINF IN (lcOrdLine)
    *E126684,1  TMI [Start] use CEILING instead of ROUND
    *lcOrderBr = [FABRIC :H=' Fabric ' :R,COLOR :H=' Color ' :R,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                [STORE :H='Store' :R,]+;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [nSellNetPr :H=IIF(MASOHDR.cOrdType='T','Transmitted Price','Price'),FABQTY :H='Quantity' :R,] +;
                [lnAmount=ROUND(nSellNetPr*FABQTY,2) :10:H=IIF(MASOHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99']
    lcOrderBr = [FABRIC :H=' Fabric ' :R,COLOR :H=' Color ' :R,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                [STORE :H='Store' :R,]+;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [nSellNetPr :H=IIF(MASOHDR.cOrdType='T','Transmitted Price','Price'),FABQTY :H='Quantity' :R,] +;
                [lnAmount=CEILING(nSellNetPr*FABQTY*100)/100 :10:H=IIF(MASOHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99']
    *E126684,1  TMI [End  ] 
    DO CASE
      *-- Open quantity
      CASE lnOrdTrans= 1
        SELECT (lcOrderFile)
        SET RELATION TO FABRIC+COLOR INTO FABRIC
        SET RELATION TO FABRIC+COLOR+laData[31]+SPACE(10) INTO FABDYE ADDITIVE
      *-- Booked quantity
      CASE lnOrdTrans= 2
        IF !USED(lcBooked)
          =gfOpenFile(gcDataDir+'MASOLIN','','SH',@lcBooked,.T.)
        ENDIF
        lcBrTtlT   = lcBrTtlB
        lcTranFile = (lcBooked)
        SET ORDER TO TAG 'MASOLIN' IN (lcBooked)
        SELECT (lcOrderFile)
        SET RELATION TO cOrdType+CMORDER+STR(LINENO,6) INTO (lcBooked)
        *E126684,1  TMI [Start] use CEILING instead of ROUND
        *lcTranBr = [FABRIC :H=' Fabric ' :R,COLOR :H=' Color ' :R ,] +;
                   [STORE :H='Store' :8:R ,nSellNetPr :H='Price' :9:R ,] +;
                   [FabBook :H='Quantity' :10:R  ,] +;
                   [lnAmount=ROUND(nSellNetPr*FabBook,2) :10:H='Amount':R:P='99999999999.99']
        lcTranBr = [FABRIC :H=' Fabric ' :R,COLOR :H=' Color ' :R ,] +;
                   [STORE :H='Store' :8:R ,nSellNetPr :H='Price' :9:R ,] +;
                   [FabBook :H='Quantity' :10:R  ,] +;
                   [lnAmount=CEILING(nSellNetPr*FabBook*100)/100 :10:H='Amount':R:P='99999999999.99']
        *E126684,1  TMI [End  ] 
        
      *-- Shipped quantity
      CASE lnOrdTrans= 3
        lcBrTtlT = lcBrTtlS
        SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
        IF SEEK(laData[1],'CONSINVL')
          lcTranFile = 'CONSINVL'
        ELSE
          lcTranFile = 'MINVLINE'
          SET ORDER TO TAG 'MINVLINO' IN 'MINVLINE'
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO CMORDER+STR(LINENO,6) INTO (lcTranFile)
        *E126684,1  TMI [Start] use CEILING instead of ROUND
        *lcTranBr = [FABRIC :H=' Fabric ' :R,COLOR :H=' Color ' :R,STORE :H='Store' :R,nSellNetPr :H='Price' :R,] +;
                   [FabQty :H='Quantity' :R,] +;
                   [lnAmount=ROUND(nSellNetPr*FabQty,2) :10:H='Amount':R:P='99999999999.99',]+;
                   [CMATINV:H='Invoice' :R ,INVDATE:H='Date' :R]
        lcTranBr = [FABRIC :H=' Fabric ' :R,COLOR :H=' Color ' :R,STORE :H='Store' :R,nSellNetPr :H='Price' :R,] +;
                   [FabQty :H='Quantity' :R,] +;
                   [lnAmount=CEILING(nSellNetPr*FabQty*100)/100 :10:H='Amount':R:P='99999999999.99',]+;
                   [CMATINV:H='Invoice' :R ,INVDATE:H='Date' :R]
        *E126684,1  TMI [End  ] 
      CASE lnOrdTrans= 4
        lcBrTtlT   = lcBrTtlC
        lcTranFile = IIF(laScrMode[2],'MORDCNLN',lcOrdCanLn)
        SELECT (lcOrderFile)
        SET RELATION TO cOrdType+CMORDER+STR(LineNo,6) INTO (lcTranFile)
        lcTranBr = [Cancelled :R :H='Date',FabQty :H= 'Total Canceld' :R,]+;
                   [cDesc=gfCodDes(cCancReson,'CCANCRESON') :R :H='Cancelation Reason']

    ENDCASE
  *-- Sort by fabric - Total quantity - Summary
  CASE lnSortBy = 2 .AND. !llDetails
    IF !USED(lcOrdSS) .OR. !SEEK(lcOrdType+laData[1],lcOrdSS)
      lcFile = IIF(laScrMode[2],'MASOLIN',lcOrdLine)
      SET ORDER TO TAG MASOLIN IN (lcFile)

      SELECT cOrdType,&lcFile..CMORDER,&lcFile..LineNo,Fabric,Color,SUM(FabQty) AS nTotQTy,SUM(FabQty*nSellNetPr) AS nAmount;
        FROM (lcFile);
        WHERE cOrdType+&lcFile..CMORDER+STR(LINENO,6)=lcOrdType+laData[1];
        GROUP BY &lcFile..CMORDER,Fabric,Color ;
        INTO CURSOR (lcOrdSS)
      INDEX ON cOrdType+CMORDER+Fabric+Color TAG (lcOrdSS)
    ENDIF
    SELECT (lcOrdSS)
    SET RELATION TO FABRIC+COLOR INTO FABRIC
    lcOrderFile = lcOrdSS

    lcOrderBr = [Fabric  :H="Fabric" :R ,] +;
                [Color   :H="Color" :R ,] +;
                [Fabric.Desc :H= 'Description', nTotQTy :H='Quantity' :R:P='9999999999.999',]+;
                [nAMOUNT :H=IIF(MASOHDR.cOrdType='T','Wholesale Price','Amount'):14:R:P='99999999999.99']
    DO CASE
      *-- Booked quantity
      CASE lnOrdTrans= 2
        IF !USED(lcBookSS) .OR. !SEEK(laData[1],lcBookSS)
          SET ORDER TO TAG MASOLIN IN IIF(laScrMode[2],'MASOLIN',lcOrdLine)
          SELECT FabRic,Color,FabBook AS nTotQTy,SUM(FabBook*nSellNetPr) AS nAmount ;
          FROM IIF(laScrMode[2],'MASOLIN',lcOrdLine);
          WHERE cOrdType+CMORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
          GROUP BY CMORDER,Fabric,Color INTO CURSOR (lcBookSS)
          INDEX ON CMORDER+Fabric+Color TAG (lcBookSS)
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO CMORDER+Fabric+Color INTO (lcBookSS) ADDITIVE
        lcBrTtlT = lcBrTtlB
        lcTranFile = lcBookSS
        lcTranBr   = [FABRIC :H=' Fabric ' :R,COLOR :H=' Color ' :R,nTOTQTY :H='Quantity' :R:P='9999999999.999',]+;
                     [nAMOUNT :H='Amount' :14:R:P='99999999999.99']
      *-- Shipped quantity
      CASE lnOrdTrans= 3
        IF !USED(lcShipSS) .OR. !SEEK(laData[1],lcShipSS)
          SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
          IF SEEK(laData[1],'CONSINVL')
            lcFile = 'CONSINVL'
          ELSE
            lcFile = 'MINVLINE'
            SET ORDER TO TAG 'MINVLINO' IN 'MINVLINE'
          ENDIF
          SELECT &lcFile..CMORDER,Fabric,Color,SUM(FabQty) AS nTotQTy,SUM(FabQty*nSellNetPr) AS nAmount ;
            FROM (lcFile) ;
            WHERE CMORDER+STR(LINENO,6)=laData[1] ;
            GROUP BY CMORDER,Fabric,Color INTO CURSOR (lcShipSS)
          INDEX ON CMORDER+Fabric+Color TAG (lcShipSS)
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO CMORDER+Fabric+Color INTO (lcShipSS)  ADDITIVE
        lcBrTtlT = lcBrTtlS
        lcTranFile = lcShipSS
        lcTranBr   = [FABRIC :H=' Fabric ' :R,COLOR :H=' Color ' :R,nTOTQTY :H='Quantity' :R:P='9999999999.999',]+;
                     [nAMOUNT :H='Amount' :R:P='99999999999.99']
    ENDCASE
  *-- Sort by styl - Quantity per size- Details
  *-- Sort by styl - Quantity per size- Summary
  *-- Sort by store# - Total quantity - Details
  CASE lnSortBy = 3 .AND. llDetails
    lcOrderFile = IIF(laScrMode[2],'MASOLIN',lcOrdLine)
    SET ORDER TO TAG 'MASOLINST' IN (lcOrderFile)
    *E126684,1  TMI [Start]     use CEILING instead of ROUND
    *lcOrderBr = [STORE :H='Store' :R,FABRIC :H=' Fabric ' :R,COLOR :H=' Color ' :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [nSellNetPr :H=IIF(MASOHDR.cOrdType='T','Transmitted Price','Price'):9:R ,FabQty :H='Quantity' :10:R,] +;
                [lnAmount=ROUND(nSellNetPr*FabQty,2) :10:H=IIF(MASOHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99']
    lcOrderBr = [STORE :H='Store' :R,FABRIC :H=' Fabric ' :R,COLOR :H=' Color ' :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [nSellNetPr :H=IIF(MASOHDR.cOrdType='T','Transmitted Price','Price'):9:R ,FabQty :H='Quantity' :10:R,] +;
                [lnAmount=CEILING(nSellNetPr*FabQty*100)/100 :10:H=IIF(MASOHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99']
    *E126684,1  TMI [End  ] 
    DO CASE
      *-- Open quantity
      CASE lnOrdTrans= 1
        SELECT (lcOrderFile)
        SET RELATION TO FABRIC+COLOR INTO FABRIC
        SET RELATION TO FABRIC+COLOR+laData[31]+SPACE(10) INTO FABDYE ADDITIVE
      *-- Booked quantity
      CASE lnOrdTrans= 2
        IF !USED(lcBooked)
          =gfOpenFile(gcDataDir+'MASOLIN','','SH',@lcBooked,.T.)
        ENDIF
        lcBrTtlT   = lcBrTtlB
        lcTranFile = (lcBooked)
        SET ORDER TO TAG 'MASOLIN' IN (lcBooked)
        SELECT MASOLIN
        SET RELATION TO cOrdType+CMORDER+STR(LINENO,6) INTO (lcBooked)
        *E126684,1  TMI [Start] use CEILING instead of ROUND
        *lcTranBr = [STORE :H='Store' :R ,FABRIC :H=' Fabric ' :R,COLOR :H=' Color ' :R,] +;
                   [nSellNetPr :H='Price' :9:R,FabBook :H='Quantity' :10:R,] +;
                   [lnAmount=ROUND(nSellNetPr*FabBook,2) :14:H='Amount':R:P='9999999999.99']
        lcTranBr = [STORE :H='Store' :R ,FABRIC :H=' Fabric ' :R,COLOR :H=' Color ' :R,] +;
                   [nSellNetPr :H='Price' :9:R,FabBook :H='Quantity' :10:R,] +;
                   [lnAmount=CEILING(nSellNetPr*FabBook*100)/100 :14:H='Amount':R:P='9999999999.99']
        *E126684,1  TMI [End  ] 
        
      *-- Shipped quantity
      CASE lnOrdTrans= 3
        lcBrTtlT   = lcBrTtlS
        SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
        IF SEEK(laData[1],'CONSINVL')
          lcTranFile = 'CONSINVL'
        ELSE
          lcTranFile = 'MINVLINE'
          SET ORDER TO TAG 'MINVLINO' IN 'MINVLINE'
        ENDIF
        SELECT MASOLIN
        SET RELATION TO CMORDER+STR(LINENO,6) INTO (lcTranFile)
        *E126684,1  TMI [Start] use CEILING instead of ROUND
        *lcTranBr   = [FABRIC :H=' Fabric ' :R,COLOR :H=' Color ' :R ,STORE :H='Store' :R ,]+;
                     [nSellNetPr :H='Price' :9:R,FabQty :H='Quantity' :10:R,] +;
                     [lnAmount=ROUND(nSellNetPr*FabQty,2) :14:H='Amount':R:P='99999999999.99',]+;
                     [CMATINV:H='Invoice' :R ,INVDATE:H='Date' :R ]
        lcTranBr   = [FABRIC :H=' Fabric ' :R,COLOR :H=' Color ' :R ,STORE :H='Store' :R ,]+;
                     [nSellNetPr :H='Price' :9:R,FabQty :H='Quantity' :10:R,] +;
                     [lnAmount=CEILING(nSellNetPr*FabQty*100)/100 :14:H='Amount':R:P='99999999999.99',]+;
                     [CMATINV:H='Invoice' :R ,INVDATE:H='Date' :R ]
        *E126684,1  TMI [End  ]       
        
      CASE lnOrdTrans= 4
        lcBrTtlT   = lcBrTtlC
        lcTranFile = IIF(laScrMode[2],'MORDCNLN',lcOrdCanLn)
        SELECT (lcOrderFile)
        SET RELATION TO cOrdType+CMORDER+STR(LineNo,6) INTO (lcTranFile)
        lcTranBr = [Cancelled :R :H='Date',FabQty :H= 'Total Canceld' :R,]+;
                   [cDesc=gfCodDes(cCancReson,'CCANCRESON') :R :H='Cancelation Reason']
    ENDCASE
  *-- Sort by store# - Total quantity- Summary
  CASE lnSortBy = 3 .AND. !llDetails
    IF !USED(lcOrdTS) .OR. !SEEK(lcOrdType+laData[1],lcOrdTS)
      lcFile = IIF(laScrMode[2],'MASOLIN',lcOrdLine)
      SET ORDER TO TAG MASOLIN IN (lcFile)

      *E126684,1  TMI [Start] use CEILING for amount summation
      *SELECT cOrdType,&lcFile..CMORDER,&lcFile..LineNo,Store,SUM(FabQty) AS nTotQTy,SUM(FabQty*nSellNetPr) AS nAmount;
        FROM (lcFile);
        WHERE cOrdType+&lcFile..CMORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
        GROUP BY &lcFile..CMORDER,Store INTO CURSOR (lcOrdTS)
      SELECT cOrdType,&lcFile..CMORDER,&lcFile..LineNo,Store,SUM(FabQty) AS nTotQTy,SUM(CEILING(FabQty*nSellNetPr*100)/100) AS nAmount;
        FROM (lcFile);
        WHERE cOrdType+&lcFile..CMORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
        GROUP BY &lcFile..CMORDER,Store INTO CURSOR (lcOrdTS)
      *E126684,1  TMI [End  ]  
      
      INDEX ON cOrdType+CMORDER+STORE TAG (lcOrdTS)
    ENDIF
    SELECT (lcOrdTS)
    SET RELATION TO 'S'+laData[2]+Store INTO Customer
    lcOrderFile = lcOrdTS

    lcOrderBr = [STORE :H='Store' :R ,] +;
                [Customer.StName :H='Name' :30 ,] +;
                [nTOTQTY :H='Quantity' :10:R:P='9999999999.999'  ,] +;
                [nAMOUNT :H=IIF(MASOHDR.cOrdType='T','Wholesale Price',;
                'Amount'):14:R:P='99999999999.99']
    DO CASE
      *-- Booked quantity
      CASE lnOrdTrans= 2
        IF !USED(lcBookTS) .OR. !SEEK(laData[1],lcBookTS)
          lcFile = IIF(laScrMode[2],'MASOLIN',lcOrdLine)
          SET ORDER TO TAG MASOLIN IN (lcFile)
          SELECT &lcFile..CMORDER,Store,SUM(FabBook) AS nTotQTy,SUM(FabBook*nSellNetPr) AS nAmount ;
            FROM (lcFile);
            WHERE &lcFile..cOrdType+&lcFile..CMORDER+STR(LINENO,6)=lcOrdType+laData[1] ;
            GROUP BY &lcFile..CMORDER,Store INTO CURSOR (lcBookTS)
          INDEX ON CMORDER+STORE TAG (lcBookTS)
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO CMORDER+STORE INTO (lcBookTS) ADDITIVE
        lcTranFile = lcBookTS
        lcBrTtlT   = lcBrTtlB
        lcTranBr   = [STORE :H='Store' :R ,] +;
                     [FabQty :H='Quantity' :10:R:P='9999999999.999' ,] +;
                     [nAMOUNT :H='Amount' :14:R:P='99999999999.99']

      *-- Shipped quantity
      CASE lnOrdTrans= 3
        IF !USED(lcShipTS) .OR. !SEEK(laData[1],lcShipTS)
          SET ORDER TO TAG 'CINVLINE' IN 'CONSINVL'
          IF SEEK(laData[1],'CONSINVL')
            lcFile = 'CONSINVL'
          ELSE
            lcFile = 'MINVLINE'
            SET ORDER TO TAG 'MINVLINO' IN 'MINVLINE'
          ENDIF
          SELECT &lcFile..CMORDER,Store,SUM(FabQty) AS nTotQTy,SUM(FabQty*nSellNetPr) AS nAmount ;
            FROM (lcFile);
            WHERE CMORDER+STR(LINENO,6)=laData[1];
            GROUP BY CMORDER,Store INTO CURSOR (lcShipTS)
          INDEX ON CMORDER+STORE TAG (lcShipTS)
        ENDIF
        SELECT (lcOrderFile)
        SET RELATION TO CMORDER+STORE INTO (lcShipTS) ADDITIVE
        lcBrTtlT   = lcBrTtlS
        lcTranFile = lcShipTS
        lcTranBr   = [STORE :H='Store' :12:R ,] +;
                     [FabQty :H='Quantity' :10:R:P='9999999999.999' ,] +;
                     [nAMOUNT :H='Amount' :14:R:P='99999999999.99']
    ENDCASE
ENDCASE
WAIT CLEAR

*!*************************************************************
*! Name      : lfCanReason
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Read CMORDER cancellation reason
*!*************************************************************
*! Calls     : ARORDER.SPX
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCanReason()
*!*************************************************************
FUNCTION lfCanReason
PRIVATE lnCanReason

STORE 1  TO lnCanReason
=gfwCodePop(@laCodes,'CCANCRESON','L')

DO (gcScrDir+"ARORDER.SPX") WITH 'X','lnCanReason'
RETURN(ALLTRIM(laCanReason[lnCanReason,2]))

*!*************************************************************
*! Name      : lpDelScr
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Cancel/Uncancel Order
*!*************************************************************
*! Calls     : gfModalGen,lfGetInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpDelScr()
*!*************************************************************
FUNCTION lpDelScr

*C126361,4  TMI [Start] if this material sales order has a release order  then do not cnacel
IF ASCAN(laEvntTrig , PADR('REMRLORD',10)) <> 0
  IF !gfDoTriger('SOMAT',PADR('REMRLORD',10))
    RETURN .F.
  ENDIF
ENDIF
*C126361,4  TMI [End  ] 
DO lfDelScr IN (gcAppHome + 'SO\SOMUPDAT.FXP')

*!*************************************************************
*! Name      : lfvObjLink
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Call the object link screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvObjLink()
*!*************************************************************
FUNCTION lfvObjLink
PRIVATE lnAlais
lnAlais = SELECT()
DO GetObj WITH 'O',laData[1]
SELECT (lnAlais)

*!*************************************************************
*! Name      : lfGetDefCode
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Get defaut code
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcCodeType : Code Type
*!*************************************************************
*! Return      : Default code
*!*************************************************************
FUNCTION lfGetDefCode
PARAMETERS lcCodeType
PRIVATE lcCurrTag

lcCurrTag = ORDER('CODES')
SET ORDER TO TAG 'CCODE_NO' IN 'CODES'
=SEEK('D'+PADR(UPPER(lcCodeType),10),'CODES')
SET ORDER TO TAG (lcCurrTag) IN 'CODES'
RETURN(CODES.CCODE_NO)

*!*************************************************************
*! Name      : lfvContPri
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Get styl contract price
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcStyl    : Styl code
*!                     lcGPrice   : Gross Price
*!                     lcNPrice   : Net Price
*!                     lcDiscount : Discount Percent
*!*************************************************************
*! Return      : Contract price
*!*************************************************************
FUNCTION lfvContPri
*N125956,1 TMI 03/10/2005 [Start] rename lcStyle to "lcFabClr"
*PARAMETERS lcStyle,lcGPrice,lcNPrice,lcDiscount
PARAMETERS lcFabClr,lcGPrice,lcNPrice,lcDiscount
*N125956,1 TMI [End] 
PRIVATE lnAlias,lnGPrice,lnNPrice,lnDiscount,llContract
lnAlias = SELECT()

*(TMI) GET IT FOR FABRIC COLOR
lnGPrice   = &lcGPrice
lnNPrice   = &lcNPrice
lnDiscount = &lcDiscount
llContract = .F.
IF llCntrExst
  llCntrExst = .F.

  SELECT MASOHDR
  IF TYPE('lcOrderNo') <> 'C'
    SET FILTER TO 
  ENDIF
  SET ORDER TO TAG ORDACCT DESCENDING
  lcOOLnTg = ORDER('MASOLIN')
  
  SET ORDER TO TAG MASOLINF IN MASOLIN
  IF SEEK(laData[2]+'C')
    SCAN REST WHILE Account+cOrdType=laData[2]+'C' ;
       FOR  !INLIST(Status,'X','B') .AND. BETWEEN(laData[8],START,COMPLETE) .AND. CCURRCODE=laData[33]
      llCntrExst = .T.
      *N125956,1 TMI 03/10/2005 [Start]  rename lcStyle to "lcFabClr"
      *IF SEEK(lcStyle+DTOS(MASOHDR.Complete)+'C'+MASOHDR.CMORDER+laData[3],'MASOLIN') .AND. MASOLIN.nSellNetPr <> 0
      IF SEEK(lcFabClr+DTOS(MASOHDR.Complete)+'C'+MASOHDR.CMORDER+laData[3],'MASOLIN') .AND. MASOLIN.nSellNetPr <> 0
        *N125956,1 TMI [End] 
        lnGPrice   = MASOLIN.nSellPrice
        lnNPrice   = MASOLIN.nSellNetPr
        lnDiscount = MASOLIN.Disc_Pcnt
        llContract = .T.
        EXIT
      ENDIF
    ENDSCAN
  ENDIF
  SET ORDER TO TAG (lcOOLnTg) IN MASOLIN
  SET ORDER TO TAG MASOHDR
  IF TYPE('lcOrderNo') <> 'C'
    SET FILTER TO cOrdType+CMORDER=lcOrdType
  ENDIF
  *B802859,1 SSH(End)
  =SEEK(lcOrdType+laData[1])
ENDIF

SELECT (lnAlias)
&lcGPrice   = lnGPrice
&lcNPrice   = lnNPrice
&lcDiscount = lnDiscount
RETURN(llContract)

*!*************************************************************
*! Name      : lfChkUnComS
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
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

IF (llFrmSetup AND llChkUnCom) OR !llFrmSetup
  llGoAndChk = IIF(llFrmSetup, .F., llGoAndChk)
  llChkUnCom = .F.
  IF gfUnCompSession(lcProgID, lnSessNo, "SOMAT")
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

    SELECT IIF(lcScrMode='V','MASOLIN',lcOrdLine)
    =SEEK(lcOrdType+lcCurrOrd)
    SELECT IIF(lcScrMode='V','MASOHDR',lcOrdHdr)
    =SEEK(lcOrdType+lcCurrOrd)
    STORE IIF(laScrMode[2],'MASOHDR',lcOrdHdr) TO ;
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

*!*************************************************************
*! Name      : lfCratTemp
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Create temp files
*!*************************************************************
*! Calls     : gfCrtTmp
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCratTemp()
*!*************************************************************
FUNCTION lfCratTemp
PRIVATE laFileStru,lnFileStru,laIndex

IF !laScrMode[2]
  SELECT MASOHDR
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]
  laFileStru[lnFileStru+1,1] = 'nSteps'
  laFileStru[lnFileStru+1,2] = 'N'
  laFileStru[lnFileStru+1,3] = 2
  laFileStru[lnFileStru+1,4] = 0

  =gfCrtTmp(lcOrdHdr,@laFileStru,[cOrdType+CMORDER],lcOrdHdr)
ENDIF
SELECT MASOLIN
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'lContract'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

DECLARE laIndex[3,2]
laIndex[1,1] = 'cOrdType+CMORDER+STORE+FABRIC+COLOR+STR(LINENO,6)'
laIndex[1,2] = 'MASOLINST'
laIndex[2,1] = 'cOrdType+CMORDER+FABRIC+COLOR+STORE+STR(LINENO,6)'
laIndex[2,2] = 'MASOLINF'
laIndex[3,1] = 'cOrdType+CMORDER+STR(LINENO,6)'
laIndex[3,2] = 'MASOLIN'
=gfCrtTmp(lcOrdLine,@laFileStru,@laIndex)
SET ORDER TO TAG 'MASOLIN' IN (lcOrdLine)
SCATTER MEMVAR BLANK MEMO

SELECT MASOLIN
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'lContract'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
DECLARE laIndex[3,2]
laIndex[1,1] = 'Flag+CMORDER+STR(LINENO,6)'
laIndex[1,2] = lcTempline
laIndex[2,1] = 'cOrdType+CMORDER+STORE+FABRIC+COLOR+STR(LINENO,6)'
laIndex[2,2] = 'MASOLINST'
laIndex[3,1] = 'cOrdType+CMORDER+STR(LINENO,6)'
laIndex[3,2] = 'MASOLIN'
=gfCrtTmp(lcTempline,@laFileStru,@laIndex)

IF laScrMode[3]
  =gfOpenFile(gcDataDir+'MORDCNLN',gcDataDir+'MORDCNLN','SH')
  =AFIELDS(laFileStru)
  =gfCrtTmp(lcOrdCanLn,@laFileStru,[CORDTYPE+CMORDER+STR(LINENO,6)],lcOrdCanLn)
  =gfCloseFile('MORDCNLN')
ENDIF


*!*************************************************************
*! Name      : lfvOrdCatg                           
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Valid order category code.
*!*************************************************************
*! Calls     : CODECHK
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOrdCatg()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfvBuyer                            *E301253,1
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Validate or view buyer(s).
*!*************************************************************
*! Passed Parameters  : llByView-> .T. View , .F. Select/Add
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvBuyer()
*!*************************************************************
FUNCTION lfvBuyer
PARA llByView
lcContKey = 'C'+PADR(laData[2],8)+PADR(laData[3],8)
IF llByView
  lcOnKyEnr=ON('KEY','ENTER')
  =gfOpenFile(gcDataDir+'CONTACT','CONTACT','SH')
  SET KEY TO lcContKey
  GO TOP
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

*!*************************************************************
*! Name      : lpSelBuyer                            *E301253,1
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Select buyer(s).
*!*************************************************************
FUNCTION lpSelBuyer

LADATA[20] = CONTACT
SHOW GET LADATA[20]
KEYBOARD '{ESC}'
RETURN

*!*************************************************************
*! Name      : lfwStart                              *C200081,1 
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : When function for CMORDER start date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwStart()
*!*************************************************************
*
FUNCTION lfwStart


*!*************************************************************
*! Name      : lfwComplete                           
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : When function for order completion date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwComplete()
*!*************************************************************
FUNCTION lfwComplete


*!*************************************************************
*! Name      : lfChMSHV
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Function to change the title "ALL" of the ship via popup 
*!             to be "At Store Level"
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfChMSHV()
*!*************************************************************
FUNCTION lfChMSHV

FOR lnI = 1 TO ALEN(laShipVia,1)
  IF laShipVia[lnI,1] = PADR('ALL',30)
    laShipVia[lnI,1] = 'At Store Level'
    EXIT
  ENDIF
ENDFOR
SHOW GET lnShipVia

*!*************************************************************
*! Name      : lfvCompln
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Valid function of complete date in order line
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCompln()
*!*************************************************************
FUNCTION lfvCompln

PARAMETERS llStores
IF m.Complete <> EVAL(IIF(llStores,lcTempline,lcOrdLine)+'.Complete')
  IF laData[9] > m.Complete
    *E300420,1 Message : 32013
    *E300420,1 The starting date cannot be greater than the completion date.
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('TRM32013B00000','ALERT')
    m.Complete = EVAL(IIF(llStores,lcTempline,lcOrdLine)+'.Complete')
    RETURN
  ENDIF
  IF  gcContCode # 'EGYPT'
    IF CDOW(m.Complete)='Saturday' .OR. CDOW(m.Complete)='Sunday'
      *E300420,1 Message : 32014
      *E300420,1 The completion date is a xxxxx.
      *E300420,1 Button : 00000
      *E300420,1 Ok
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


*!*************************************************************
*! Name      : lfvBrows
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Valid Browse function.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvBrows()
*!*************************************************************
FUNCTION lfvBrows

IF WONTOP() # (lcOrdBrow) AND WONTOP() # (lcBrTtlZ) .AND. WONTOP() # (lcBrTtlT)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfIncSearch
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Incremental search function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfIncSearch()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfActInSr
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : function to get the value of the inc. search and seek in file
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfActInSr()
*!*************************************************************
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
lcExpToSeek = ALLTRIM(lcExpToSeek)
IF LASTKEY() = 13
  lnBrRecNO = IIF(RECNO()>RECCOUNT(),0,RECNO())
  IF TYPE("lcSeekTag")='C' AND !EMPTY(lcSeekTag)
    lcOrgTag = SYS(22)
    lcOrgExp = SYS(14,VAL(SYS(21)))
    SET ORDER TO TAG (lcSeekTag)
  ENDIF

  
  * if the index expression is a compound index then seek
  * one by one of the value the user want to find until the
  * end of the expression or value not found in the file  
   FOR lnCount = 1 TO ALEN(laKeyExp,1)
    * if the value found in the file then go to the record that have
    * this value
    
    IF SEEK(&lakeyExp[lnCount]+lcExpToSeek)  
       
      IF TYPE("lcOrgTag")='C'
        lcOrgKey = &lcOrgExp
        SET ORDER TO TAG lcOrgTag
        
        IF !EMPTY(lcOrgTag)
          SEEK lcOrgKey
        ENDIF  
      ENDIF
      
      CLEAR TYPEAHEAD
      RETURN
    * else if the value does not exist in the file then go back to the
    * previous record  
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

*!*************************************************************
*! Name      : lfOldValue
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : When function of size quantities , total and net price
*!           : to store old value of the current filed.
*! Reference : B603449,1
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
FUNCTION lfOldvalue
lcOldValue = EVALUATE('m.' + SYS(18))
*-- End Of lfoldvalue

*!*************************************************************
*! Name      : lfWOldVale
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : To store the old value value
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
FUNCTION lfWOldVale
lcOldValue = EVALUATE(SYS(18))

*-- End Of lfoldvalue
*!*************************************************************
*! Name      : lfOrdStats
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : To store the old value value
*! Reference : B803049,1
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************

FUNCTION lfOrdStats
*-- If the system setuped to default CMORDER status to hold while the bulk status is open
IF  laSetups[7,2]='Y' .AND. MASOHDR.Status = 'O'
  IF MASOHDR.ApprAmt <> 0
    *B802614,1 Message        : Bulk CMORDER was approve,
    *B802614,1 Message        : Do you want to approve the new CMORDER.
    *B802614,1 Message NO.    : 32078
    *B802614,1 Button Message :  YES -- NO
    *B802614,1 Button         : 32000    
    IF gfModalGen('QRM32078B32000','DIALOG') = 1
      RETURN .F.
    ENDIF
  ELSE
    *B802614,1 Message        : Bulk CMORDER status is 'Open', while CMORDER default status 
    *B802614,1 Message        : is set to 'Hold'. Would you like this new CMORDER to be 
    *B802614,1 Message        : Open?"
    *B802614,1 Button         : 32000    
    *B802614,1 Button Message : YES -- NO
    *B802614,1 Button         : 32000
    IF gfModalGen('QRM32080B32000','DIALOG') = 1
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

*--End Of lfOrdStats.


*!**************************************************************************
*! Name      : lfwSeason
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Season Code When function 
*!**************************************************************************
*! Reference : C102080,1
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

*!**************************************************************************
*! Name      : gfCPBrows
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Browse the order Header file.
*!**************************************************************************
*! Reference : B803368,1
*!**************************************************************************
*! Calls     : lfvOrder()
*!**************************************************************************
*! Example   : =gfCPBrows()
*!**************************************************************************
FUNCTION gfCPBrows
PRIVATE lcOrder , lnAlias , lcOldOrder , lcAccount , lcStore
lnAlias = SELECT()
SELECT MASOHDR
lcOldOrder = laData[1]
lcAccount = IIF(TYPE('lcOrderNo')="C",laData[2],"")
lcStore = IIF(TYPE('lcOrderNo')="C",laData[3],"")

DO lpDefnFlds               && Define lcBrFields

IF !EMPTY(lcOldOrder) .AND. SEEK(lcOrdType+lcOldOrder,'MASOHDR')
  lcOrder = IIF(ARIABROW("lcOrdType"+' FOR STORE = IIF(EMPTY(lcStore),"",lcStore)',;
                  "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','CMORDER','laBrowArr'),;
                  MASOHDR.CMORDER,SPACE(6))
ELSE
  lcOrder =     "?"      
  =lfOrdBrow(@lcOrder,lcAccount,lcStore,"")
ENDIF

*-- If coming from Customer screen lcAccount holds Account Code so as to browse 
*-- only CMORDER for this Account.
laData[1] = IIF("?" $ lcOrder OR EMPTY(lcOrder),lcOldOrder,lcOrder) 

IF !EMPTY(laData[1])
  SELECT MASOHDR
  IF TYPE('lcOrderNo') <> 'C'
    lcSveFilt = FILTER()
    SET FILTER TO cOrdType + CMORDER = lcOrdType
  ENDIF
  
  IF SEEK(lcOrdType+laData[1],"MASOHDR")
    laScrMode   = .F.
    laScrMode[2]= .T.
    SCATTER FIELDS &lcScFields MEMO TO laData
    SHOW GETS      
  ENDIF
  
  SELECT MASOHDR
  IF TYPE('lcOrderNo') <> 'C'
    SET FILTER TO &lcSveFilt
  ENDIF
ENDIF
SELECT (lnAlias)
*-- End of gfCPBrows.

*!**************************************************************************
*! Name      : lpDefnFlds
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Define the lcBrFields variable.
*!**************************************************************************
*! Reference : B803368,1
*!**************************************************************************
*! Example   : lpDefnFlds
*!**************************************************************************
PROCEDURE lpDefnFlds
lcBrFields = [CMORDER:H="Order#",Status:1:H="Status",Account:H="Acct",]+;
             [Store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
             [Customer.stname:30:H="Name",]+;
             [Dept:H="Department",MultiPo=IIF(MultiPo,'Yes','No'):H="Multi PO",]+;
             [CustPo=IIF(MultiPO,'*Multi_PO*',CustPO):H="Cust. P.O#",]+;
             [Bulk:H="Bulk",Multi:H="Multi",cFacCode:H="Factor Code",]+;
             [Rep1:H="Sales Rep1",Comm1:H="Commission 1",Rep2:H="Sales Rep2",]+;
             [Comm2:H="Commission 2",FABOpen:H="Open.Qty.",FABOpenAmt:H="Open.Amt.",]+;
             [FABShip:H="Ship.Qty.",FABShipAmt:H="Ship.Amt.",]+;
             [FABBook:H="Book.Qty.",FABBookAmt:H="Book.Amt.",]+;
             [FABCancel:H="Cancel.Qty.",FABCanAmt:H="Cancel.Amt."]
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

*!*************************************************************
*! Name      : gfvOrdNote
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Function To call Notepad Program
*!*************************************************************
*! Calls     : NotePad.PRG
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : Control Pannel NotePad Button
*!*************************************************************
*! Example            : = gfvOrdNote()
*!*************************************************************
FUNCTION gfvOrdNote
=lfvOrdNote()

*!*************************************************************
*! Name      : lfChangeWh
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date      : 05/30/2002
*! Purpose   : Function To control disable and enable of WareHuose
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : lfActFolder
*!*************************************************************
*! Example            : =lfChangeWh()
*!*************************************************************
FUNCTION lfChangeWh
PRIVATE lnAlias , lnRecNo

lnAlias = SELECT()

*--if in Add mode
IF laScrMode[4] 
 RETURN 'ENABLE'
ENDIF

*--if in Edit mode
*Make warehouse editable in case of
*  1-No pick tickt created for the Sales Order
*  2-No Cut tickt Or PO created for the Sales Order
*  3-No Partialy Shipment for the Sales Order
IF laScrMode[3] 
  SELECT (lcOrdLine)
  lnRecNo = RECNO()
  LOCATE FOR &lcOrdHdr..FABSHIP >0
  IF BETWEEN(lnRecNo ,1,RECCOUNT()) 
   GOTO lnRecNo
  ENDIF
  SELECT (lnAlias)  
  RETURN IIF( FOUND('&lcOrdLine') , 'DISABLE' , 'ENABLE' ) 
ELSE
  SELECT (lnAlias)
  RETURN 'DISABLE'  
ENDIF  
*--End lfChangeWh


*!*************************************************************
*! Name        : lfwFabric
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date        : 05/30/2002
*! Purpose     : take the old fabric field in a variable
*! Calls       : 
*!*************************************************************
FUNCTION lfwFabric
lcOldFab = m.Fabric


*!*************************************************************
*! Name        : lfvKFabric
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date        : 05/30/2002
*! Purpose     : validate the fabric field from the key field
*! Calls       : 
*!*************************************************************
FUNCTION lfvKFabric

m.Fabric = PADR('?',7)
=lfvFabric()
_CUROBJ = OBJNUM(m.Color)

*!*************************************************************
*! Name        : lfwColor
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date        : 05/30/2002
*! Purpose     : take the old color fieldin a variable
*! Calls       : 
*!*************************************************************
FUNCTION lfwColor

lcOldClr = m.Color

*!*************************************************************
*! Name        : lfvKColor
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date        : 05/30/2002
*! Purpose     : validate the color field from the key field
*! Calls       : 
*!*************************************************************
FUNCTION lfvKColor

m.Color = PADR('?',6)
=lfvColor()


*!*************************************************************
*! Name        : lfvFabric
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date        : 05/30/2002
*! Purpose     : validate the fabric field
*! Calls       : 
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

IF MDOWN()
  RETURN
ENDIF
lnAlias = SELECT(0)
SELECT Fabric
IF !EMPTY(m.Fabric) AND !SEEK(m.Fabric)
    llBrowse = .F.
    DO FaBrow WITH m.Fabric,'*'
    IF EMPTY(m.Fabric)
      m.Fabric = lcOldFab
      m.Color = lcOldClr
      _CUROBJ = OBJNUM(m.Fabric)
    ENDIF
ENDIF
lcIDesc = FABRIC.DESC
m.IDesc = lcIDesc
SHOW GET m.Fabric ENABLE
=lfRefresh(lcWinCh32)
SELECT(lnAlias)

*!*************************************************************
*! Name        : lfvColor
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date        : 05/30/2002
*! Purpose     : validate the color field
*! Calls       : 
*!*************************************************************
FUNCTION lfvColor
PRIVATE lnAlias

lnAlias = ALIAS()
IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF
SELECT (lcOrdLine)
IF EMPTY(m.Fabric) AND EMPTY(m.Color)
   IF LASTKEY() <> 15
    =gfModalGen('INM36013B36000','DIALOG')
    _CUROBJ = IIF(EMPTY(m.Fabric),OBJNUM(m.Fabric),OBJNUM(m.Color))    
    RETURN
  ELSE  
    RETURN
  ENDIF  
ENDIF

IF (!lladdline .AND. llBrowse) .OR. laScrMode[3] .AND. !lladdline .AND. Color <> m.Color
   IF FABQty <> FABBook
     =gfOpenFile(gcDataDir+'MORDCNLN',gcDataDir+'MORDCNLN','SH')
     SELECT lcOrdLine
     IF !SEEK('O'+CMORDER+STR(lineno,6),'MOrdcnln') .AND. FABbook <> MOrdcnln.FABQty
       *B603606,1 Message : 32106
       *B603606,1 order line already XXXX ; Cannot modify fabric.
       *B603606,1 Button  : 00000
       *B603606,1 Ok
       =gfModalGen('INM32106B00000','ALERT','shiped')
       =gfCloseFile('MORDCNLN')
       STORE IIF(llAddLine,SPACE(7),&lcOrdLine..FABRIC) TO m.FABRIC
       STORE IIF(llAddLine,SPACE(6),&lcOrdLine..COLOR) TO m.COLOR
       _CUROBJ = OBJNUM(m.fabric)
       llBrowse = .F.  
       RETURN
     ENDIF
   ENDIF
ENDIF

IF llBrowse .OR. !EMPTY(m.Color)
  IF !SEEK(m.Fabric+m.Color,'Fabric'))
    m.Color  = IIF(EMPTY(m.Fabric),PADR(m.Color,6),CHR(240))
    lcOldF = m.Fabric
    DO FaBrow WITH m.Fabric,m.Color
    IF EMPTY(m.Color)
      m.Fabric = lcOldF
      STORE IIF(llAddLine,SPACE(6),&lcOrdLine..COLOR) TO m.COLOR
      _CUROBJ = OBJNUM(m.Color)
      SELECT(lnAlias)
      RETURN
    ENDIF
    llBrowse = .F.
  ENDIF
ENDIF

IF !llAddLine .AND. m.Fabric = &lcOrdLine..Fabric .AND. m.Color = &lcOrdLine..Color
  RETURN
ENDIF

IF SEEK(m.Fabric+m.Color,'Fabric')
  *E300408,1 Message : 32110
  *E300408,1 FABRIC restricted to XXX!
  *E300408,1 Button : 00000
  *E300408,1 Ok

  *E300408,1 Message : 32109
  *E300408,1 FABRIC XXX date is 
  *E300408,1 Button : 32003
  *E300408,1 Accept Reenter

  SET RELATION OFF INTO FABRIC
  IF (!EMPTY(FABRIC.INTRODUCED) .AND. FABRIC.INTRODUCED > laData[9] .AND. ;   
        gfModalGen('QRM32109B32003','ALERT','start|'+DTOC(FABRIC.INTRODUCED))=2)         
    STORE IIF(llAddLine,SPACE(7),&lcOrdLine..FABRIC) TO m.FABRIC
    STORE IIF(llAddLine,SPACE(6),&lcOrdLine..COLOR) TO m.COLOR
    SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE
    _CUROBJ = OBJNUM(m.FABRIC)
    RETURN
  ENDIF
  
  m.nSellPrice = FABRIC.nSellPrice

  *N125956,1 TMI 03/10/2005 [Start] Get contract price (if any)
  m.lContract = lcOrdType<>'C' .AND. lfvContPri(m.Fabric+m.Color,'m.nSellPrice','m.nSellNetPr','m.Disc_Pcnt')
  *N125956,1 TMI [End] 

  SELECT (lcOrdLine)
  IF m.nSellPrice < 0
    STORE IIF(llAddLine,SPACE(7),&lcOrdLine..FABRIC) TO m.FABRIC
    STORE IIF(llAddLine,SPACE(6),&lcOrdLine..COLOR) TO m.COLOR
    SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE
    _CUROBJ = OBJNUM(m.Fabric)
    RETURN
  ENDIF
  
  IF !EMPTY(m.Fabric) AND !EMPTY(m.Color) .AND. laSetups[5,2]='Y' .AND. !SEEK(m.FABRIC+m.COLOR+laData[31]+SPACE(10),'FABDYE')
    *E300725,1 Message : 38029
    *E300725,1 Item/Color xxxxx/xxxx is not assigned to warehouse xxxx
    *E300725,1 Button : 38001
    *E300725,1 Add Cancel
    IF gfModalGen('QRM38029B38001','ALERT','Item/Color: '+ALLTRIM(m.Fabric)+'/'+ALLTRIM(m.Color)+'|'+laData[31]) = 1
      DO gpAdFabWar WITH SUBSTR(m.FABRIC,1,7),m.COLOR,SPACE(10),laData[31]
      *N000385,4 TMI [Start] Add link code and sell link code to fabdye from fabric 
      lnAlias = ALIAS()    
      SELECT FABDYE
      IF !EMPTY(FABRIC.LINK_CODE)
        REPLACE GL_LINK WITH FABRIC.LINK_CODE
      ENDIF
      IF !EMPTY(FABRIC.CSLSGLLINK)
        REPLACE CSLSGLLINK WITH FABRIC.CSLSGLLINK
      ENDIF
      SELECT (lnAlias)
      *N000385,4 TMI [End  ] Add link code and sell link code to fabdye from fabric  
    ELSE
      STORE IIF(llAddLine,SPACE(7),&lcOrdLine..FABRIC) TO m.FABRIC
      STORE IIF(llAddLine,SPACE(6),&lcOrdLine..COLOR) TO m.COLOR
      SET RELATION OFF INTO FABRIC
      SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE
      _CUROBJ = OBJNUM(m.FABRIC)
      RETURN
    ENDIF   
  ENDIF
  
  *N000385,4 TMI [Start] No fabric-color is repeated 
  lcOldOrd = ORDER(lcOrdLine)
  SET ORDER TO  MASOLINST IN (lcOrdLine)    
  *---Key :  cOrdType+CMORDER+STORE+FABRIC+COLOR+STR(LINENO,6)  
  IF SEEK('O'+laData[1]+m.Store+m.Fabric+m.Color,(lcOrdLine))
    =gfModalGen('INM32112B00000','DIALOG')
    SET ORDER TO &lcOldOrd IN (lcOrdLine)
    RETURN
  ENDIF
  SET ORDER TO &lcOldOrd IN (lcOrdLine)
  *N000385,4 TMI [End  ] No fabric-color is repeated   

  lcClrDesc   = gfCodDes(m.Color, 'COLOR')
  m.ClrDesc   = lcClrDesc
  SHOW GET m.IDesc
  SHOW GET m.ClrDesc
  IF !laData[7]
    m.CustPo   = &lcOrdHdr..CustPo
  ENDIF  
  
  =SEEK(m.FABRIC+m.COLOR+laData[31]+SPACE(10),'FABDYE')
  m.Gl_Sales = ALLTRIM(laData[53])+FABDYE.CSLSGLLINK
  m.Gl_Sales = IIF(laSetups[4,2]='Y' AND SEEK('02'+m.Gl_Sales,'Gl_Link'),m.Gl_Sales,'DEFDEF')

  m.Flag       = IIF(llAddLine OR m.Flag='N','N','M')
  *E126684,1  TMI [Start] get net sell price from sell price round up to 5
  *m.nSellNetPr = ROUND(m.nSellPrice*(100-m.Disc_Pcnt)/100,2)
  m.nSellNetPr = CEILING(100000 * m.nSellPrice*(100-m.Disc_Pcnt)/100 ) / 100000
  *E126684,1  TMI [End  ] 
  
  *N000385,4 TMI [Start] 

  *m.Desc1   = FABRIC.DESC + ' - ' + lcClrDesc
  m.Desc1   = lcIDesc + ' - ' + lcClrDesc
  
  *N000385,4 TMI [End  ] 
  lcOldFab = m.Fabric 
  lcOldClr = m.Color
  SELECT (lcOrdLine)
  SET ORDER TO TAG MASOLINST
  SET ORDER TO TAG MASOLIN
  =SEEK(lcOrdType+laData[1]+STR(m.LineNo,6))
  =SEEK(m.Fabric+m.Color,'FABRIC')
  IF llAddLine 
    lnLines     = lnLines + 1
    m.LineNo    = lnLines
    m.Account   = laData[2]
    m.CMORDER   = laData[1]
    m.cOrdType  = lcOrdType
    m.cWareCode = laData[31]
    m.Complete  = laData[10]
    m.Pattern   = FABRIC.Pattern
    m.Width     = FABRIC.Width
    m.cSellUOM  = FABRIC.CSELLUOM  
    m.UomUse    = FABRIC.UOMUSE    
    m.NSELLCONV = FABRIC.NSELLCONV 
    m.ClrDesc   = lcClrDesc
    m.IDesc     = lcIDesc
    m.Comm1     = laData[28]
    m.Comm2     = laData[30]
    SHOW GET m.IDesc
    SHOW GET m.ClrDesc
    SHOW GET llMultiSt DISABLE
    APPEND BLANK
  ELSE
    *B607537,1 HBG 12/04/2005 Update AltFabric & AltColor in MASOLINE if fabrci changed [Begin]
    m.AltFabric = EVAL(lcOrdLine+'.Fabric')
    m.AltColor  = EVAL(lcOrdLine+'.Color')
    *B607537,1 [End]
    lcCount = STR(lnCount,1)
    STORE FABQty TO m.FABQty
    STORE FABBook TO m.FABBook
    =lfvNPrice() AND lfChkTotQty()
  ENDIF
  =RLOCK()
  GATHER MEMVAR
  UNLOCK
  llAddLine  = .F.
  llCUpdate  = .T.

  SHOW WINDOW (lcOrdBrow) REFRESH SAME
  SHOW GETS WINDOW (lcWinCh32)  ENABLE ONLY
  SHOW GETS WINDOW (lcWinCh321) ENABLE ONLY
  SHOW GETS WINDOW (lcWinCh322) ENABLE ONLY

  =lfRefresh(lcWinCh32)
  =lfRefresh(lcWinCh321)
  
  *N125956,1 TMI 03/10/2005 [Start] Update the LCONTRACT field to prevent updating sell price in add mode  
  SELECT (lcOrdLine)
  REPLACE lContract WITH m.lContract
  *N125956,1 TMI [End] 
  
  IF !llMultiSt
    SHOW GET ibLStore   DISABLE
    SHOW GET m.Store    DISABLE
  ENDIF
  _CUROBJ = OBJNUM(m.Pattern)
ELSE
  _CUROBJ = OBJNUM(m.Color)
ENDIF



*!*************************************************************
*! Name        : lfvPattern
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date        : 05/30/2002
*! Purpose     : validate the pattern field
*! Calls       : 
*!*************************************************************
FUNCTION lfvPattern
REPLACE PATTERN WITH M.PATTERN
*-- end of lfvPattern.
*!*************************************************************
*! Name        : lfvWidth
*! Developer : TMI  -  Tarek Mohamed Ibraheem
*! Date        : 05/30/2002
*! Purpose     : validate the width field
*! Calls       : 
*!*************************************************************
FUNCTION lfvWidth
REPLACE WIDTH WITH M.WIDTH
*-- end of lfvWidth.
*!*************************************************************
*! Name      : FUNCTION lfChkTotQty
*! Developer : TMI  -  Tarek Mohamed Ibrahim
*! Date      : 05/30/2002
*! Purpose   : Check total pieces againest total quantity entered
*!*************************************************************
*! Calls     : gfModalGen(),lfvNPrice()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfChkTotQty()
*!*************************************************************
FUNCTION lfChkTotQty
PARAMETERS llStores

SELECT (lcOrdLine)
IF FABQty <> m.FABQty
  STORE .T. TO llCUpdate

  m.nSellPrice = IIF(llAddLine,FABRIC.nSellPrice,m.nSellPrice)
  *E126684,1  TMI [Start] get net sell price from sell price round up to 5
  *m.nSellNetPr = ROUND(m.nSellPrice*(100-m.Disc_Pcnt)/100,2)
  m.nSellNetPr = CEILING(100000 * m.nSellPrice*(100-m.Disc_Pcnt)/100 ) / 100000
  *E126684,1  TMI [End  ] 

  =SEEK(cOrdType+CMOrder+STR(LineNo,6),'MASOLIN')
  laData[35] = laData[35] - FABBook
  *E126684,1  TMI [Start] 
  *laData[36] = laData[36] - ROUND(FABBook*nSellNetPr,2)
  laData[36] = laData[36] - CEILING(FABBook*nSellNetPr*100)/100
  *E126684,1  TMI [End  ] 
  
  =RLOCK()
  REPLACE FABBook   WITH MAX(MASOLIN.FABBook-MASOLIN.FABQty+m.FABQty,IIF(llUpdBook,0,MASOLIN.FABBook)) ,;
          FLAG WITH IIF(FLAG='N','N','M')
  UNLOCK

  PRIVATE lnOldCanLn
  lnOldCanLn = 0
    
  IF Flag <> 'N' AND !llUpdBook
    IF MASOLIN.FABQTY > m.FABQTY     
      IF !SEEK(lcOrdType+laData[1]+STR(m.LineNo,6),lcOrdCanLn)  
        =gfwCodePop(@laCodes,'CCANCRESON','D')
        INSERT INTO (lcOrdCanLn) ;
        (cOrdType,Order,LineNo,Cancelled,cCancReson,nSellNetPr) VALUES ;
        (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2],m.nSellNetPr)
      ENDIF           
    ENDIF
    IF SEEK(lcOrdType+laData[1]+STR(m.LineNo,6),lcOrdCanLn)
      SELECT (lcOrdCanLn)        
      lnOldCanLn = FABQTY        
      REPLACE FABQTY WITH MAX(MASOLIN.FABQTY- m.FABQTY,0) ,;
              ACCOUNT WITH MASOLIN.ACCOUNT,;
              FABRIC  WITH MASOLIN.FABRIC,;
              COLOR   WITH MASOLIN.COLOR,;
              STORE   WITH MASOLIN.STORE,;
              DYELOT  WITH MASOLIN.DYELOT
      IF FABQTY > 0
        STORE lcOrdCanLn TO laCodes[6,7],laCodes[6,8]
        STORE 'lcOrdType+laData[1]+STR(m.LineNo,6)' TO laCodes[6,9]
        =gfwCodePop(@laCodes,'CCANCRESON','T')
        STORE Cancelled TO m.Cancelled
        DO (gcScrDir+"SOORDCLN.SPX")
        STORE '' TO laCodes[6,7],laCodes[6,8],laCodes[6,9]
      ELSE
        DELETE
      ENDIF
      SELECT (lcOrdLine)
    ENDIF
  ENDIF
  laData[35] = laData[35] + FABBOOK
  *E126684,1  TMI [Start] accomulate laData[36] useing CEILING instead of ROUND
  *laData[36] = laData[36] + ROUND(FABBOOK*m.nSellNetPr,2)
  laData[36] = laData[36] + CEILING(FABBOOK*m.nSellNetPr*100)/100
  *E126684,1  TMI [End  ] 
  IF FLAG <> 'N' AND !llUpdBook
    lnTotCanQty = 0
    lnTotCanQty = &lcOrdCanLn..FABQTY       
    laData[39] = MAX(laData[39] - lnOldCanLn + lnTotCanQty,0)
    laData[40] = MAX(laData[40] - (lnOldCanLn*nSellNetPr) + (lnTotCanQty*nSellNetPr),0)
  ENDIF  

  laData[41] = laData[41] - FABQTY+m.FABQTY
  *E126684,1  TMI [Start] use CEILING instead of ROUND
  *laData[42] = laData[42] - ROUND(FABQTY*nSellNetPr,2) + ROUND(m.FABQTY*m.nSellNetPr,2)
  laData[42] = laData[42] - CEILING(FABQTY*nSellNetPr*100)/100 + CEILING(m.FABQTY*m.nSellNetPr*100)/100
  *E126684,1  TMI [End  ] 
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE FABBOOK      WITH laData[35] ,;
          FABBOOKAMT   WITH laData[36] ,;
          FABCANCEL    WITH laData[39] ,;
          FABCANAMT    WITH laData[40] ,;
          FABOPEN      WITH laData[41] ,;
          FABOPENAMT   WITH laData[42]
  UNLOCK
  SELECT (lcOrdLine)
  =lfRefresh(lcWinCh321)
  =RLOCK()
  GATHER MEMVAR FIELDS FABQTY,nSellPrice,nSellNetPr
  UNLOCK
  SCATTER MEMVAR FIELDS FABBook
ENDIF
SCATTER MEMVAR FIELDS FABQTY


