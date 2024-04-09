*:**************************************************************************
*: Program file  : ArMInv
*: Program desc. : Issue Material Sales Order Invoice
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 05/19/2002
*: Reference     : N000388,1

*-
*- Sorry for the following, due to some problems this program was not worked with my full effeciency
*- you may face many code not related to the core task of the progrma , the best one to 
*- fix problems that may appear is TMI - tarek mohammed ibrahim
*-
*:**************************************************************************
* Note : This program is an exact copy of the file ARMINV.PRG with some changes 
*        regardign release orderes for RES03
-*

IF !('MA' $ gcCmpModules)
  *-- Message : 42083
  *-- Material module is not installed, Cannot proceed.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('INM42083B00000','ALERT','Material')
  RETURN
ENDIF

*E126684,1  TMI [Start] Set decimals to 5
lcSvDeci = SET('DECIMAL')
SET DECIMAL TO 5
*E126684,1  TMI [End  ] 

DECLARE lafolders[4,2],lafoldwinds[4,2],laAddress[6,3],laQtyPaste[1,1],;
        laSetups[27,2],laBrowArr[1],laShipName[1,2]
llConsInv = .F.        
DECLARE laCodes[6,10],laTerms[1,2],laSeasons[1,2],laDivision[1,2],laVRltFld[1,2],;
        laShipVia[1,2],laSpcInst[1,2],laTRltFld[6,2],laVars[11],laWareHouses[1,2]
DECLARE laCanReason[1,2]

*C126361,3  TMI [Start] 
lcRelOrd = '      '
*C126361,3  TMI [End  ] 

STORE 0 TO lnRep1Comm ,lnRep2Comm
STORE '' TO laCanReason,lcBackOrd
STORE 1  TO lnCanReason
DECLARE laDisRltFld[1,2]
STORE 0   TO lnDisc_Pcnt
STORE '' TO laCodes,laTerms,laShipVia,laSpcInst,laSeasons,laDivision,;
            laVars,lcGlSession,laBrowArr,laShipName,lcTaxName
*C126361,4  TMI [Start] define local navigation actions
laDefProc[1] = .F.  && lpTopScr
laDefProc[2] = .F.  && lpBtmScr
laDefProc[3] = .F.  && lpNxtScr
laDefProc[4] = .F.  && lpPrvScr
*C126361,4  TMI [End  ]             
laDefProc[7]  = .F.     && Delete proceude && *C126361,3 
laDefProc[9]  = .F.     && Save procedure(lpSavScr)
laDefProc[10] = .F.     && close procedure(lpClsScr)

STORE 0   TO lnOrdMark,lnInvMark,lnTotTax,laQtyPaste
STORE ' ' TO lcOrdBrow,lcInvBrow,lcBrowseTl,lcGlYear,lcGlPeriod,;
              lcSysYear,lcSysPeriod,lcIDefSes,lcIDefDiv,lcPriceLvl,lcIDefWare

STORE 0 TO lnOldChrg
STORE ' ' TO lcWinCh1,lcWinCh2,lcWinCh21,lcWinCh22,lcWinCh3,lcWinCh4,lcWinCh40,;
             lcWinCh41,lcWinCh42,lcWinCh43,lcWinCh44,lcWinCh45,lcWinCh46,;
             lcWinCh51,lcWinCh52,lcWinCh53
              
STORE {}  TO ldDefInvDate,ldDefPstDate      && Invoice Date Posting Date
STORE 0   TO lnCartons,lnSouComm1,lnSouComm2,lnActFolder
STORE 1   TO lnShipAddr,lnPstRule,lnWareHouse,lnShipName
STORE ''  TO lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
             lcFolder,lcTaxTitle,lcTaxBreak,lcSession,lcUpsType,lcOrdStat
STORE .F. TO llBrowse,llZoom,llIsCanada,llIsEngland,llMulCurr,llEditExRt,;
             llCod,llContinue,llSysDate,llNoShow,laSetups,llInstTerm,;
             llAddLine,llUpCnsLine,llOpnCrdt
*C126361,1  TMI [Start] Allow to post variable
llPost = .F.
llPst2Inv = .F.
*C126361,1  TMI [End  ] 
STORE ""  TO lcInvLine,lcInvHdr,lcPackHdr,lcUpsBox,lcEngChrg,lcFlFields,;
             lcScFields,lcWinChrg,laWareHouses,;
             lcInstLin,lcInstHdr,lcAppCrdt,lcConsInvH,lcConsInvD

*N000388,1 Define 3 Temp files to be used by gfMatCrl function. [Begin]
PRIVATE llTrkRolls , lnLineNo , llMtInvScr , llRolls , llMatInvJl
PRIVATE lcTrnTemp , lcTempOrdr , lcRollTmp , llFirstTime
STORE "" TO lcTrnTemp , lcTempOrdr , lcRollTmp
STORE "" TO lcTmpRoll , lcTmpJour , lcFullRoll
STORE .T. TO llFirstTime , llMtInvScr 
STORE .F. TO llTrkRolls , llRolls , llMatInvJl
lcProg  = 'ARMINV'
*N000388,1 Define 3 Temp files to be used by gfMatCrl function. [End]

STORE ""  TO lcPackLine 
STORE ""  TO lcOrdCanLn
STORE ''  TO lcTEOM,lcTCod,lcOldValue,lcUpsTrack
STORE 0   TO lnTDaysDue,lnTerDiscR,lnLastMode
STORE 20 TO lnEomDay
STORE .F. TO m.lUpsIns,m.LNEWLINE,m.LPACKED,m.LBACKORD,llCnRemain
STORE 0   TO m.nTaxRate,m.nChrgTax,m.nMerchTax

lnSessNo = gnProgCopy
STORE 1 TO lnTerms,lnShipVia,lnSpcInst,lnSeason,lnDivision
lcorder = space(6)

lcStatColor = gcObjColor
STORE .T. TO llGoAndChk,llChkUnCom
STORE ''  TO lcAdTrnSeq,lcAcTrnSeq,lcHisSeq,lcGlSess,lcRepBat,lnUnCmSeRc
STORE .T. TO llUpdGlDif,llUpdMstGL

*DECLARE laEngStyTax[1,2]
STORE 0 TO lnTaxRate  && Tax Rate at Style level  
*STORE '' TO laEngStyTax

*B607226,1 ABD - Define the tax array that hold the vat tax. [Begin]
DECLARE laEngStyTax[1,2]
STORE 0 TO lnTaxRate  && Tax Rate at Style level  
STORE '' TO laEngStyTax
*B607226,1 ABD - [End]

IF !gfSetup()
  RETURN
ENDIF

lcProgID  = 'MRELSEORD'
lcOrdBrow = 'List'
lcInvBrow = 'Material Release Order Lines'
lcBrTtlZ  = 'Zoom Release Order Lines'
lcSize1   = 'Units'

lafolders[1,1] = 'List'
lafolders[1,2] = 1
lafolders[2,1] = 'Header'
lafolders[2,2] = 2
lafolders[3,1] = 'Details'
lafolders[3,2] = 3
lafolders[4,1] = 'Charges'
lafolders[4,2] = 4
lnnofld = 4

lcwfoldchng = '=lfActFolder()'  && function to control shows after change the folder
lnFoldercend = 102.000
lnFolderrend = 2.000
STORE 'ENABLE' TO lcScopeStat
*C126361,1  TMI [Start] 
*STORE 'DISABLE' TO lcPostStat
*C126361,1  TMI [End  ] 

PRIVATE lcUpdPik
STORE SPACE(0) TO lcUpdPik  && Modify Picking upon invoice

lcUpdPik=gfGetMemVar('M_MPKTKIN' ,gcAct_Comp) 

IF !WEXIST(gcBaseWind)

  lnLastMode = 0
  SELECT MaSoLin
  SCATTER MEMVAR MEMO BLANK  
  SELECT MInvHdr  
  SCATTER MEMVAR BLANK  
  lcScFields = 'cMOrder,CustPo,PikTkt,Account,Store,InvDate,cWareCode,Season,;
                Comm1,Comm2,cMatInv,Flag,cDivision,CCURRCODE'
  lcFlFields = 'SHIPDATE,DUEDATE,APPROVAL,cTermCode,SHIPVIA,SPCINST,NCHRGTAX,'+;
               'DEPT,NOTE1,NOTE2,REP1,REP2,lUpsIns,Cod_Flag,NEXRATE,cFacCode,'+;
               'CARTONS,WEIGHT,CODTAG,COD_AMT,TRDE_DISC,FABSHIPAMT,DISCPCNT,DISCOUNT,'+;
               'FREIGHT,INSUR,COD,TAX_AMT,TOTALCHG,NCHARGES,NCURRUNIT,FABSHIP,'+;
               'nPstAmt,cTaxRule,TAX_RATE,nPstRate,nHstRate,nHstAmt,BOL_NO,APPRAMT,DPOSTDATE,CCODTRCKNO,STATUS'
  lcSession   = gfsequence('CSESSION')
  lcGlSession = gfsequence('GLSESSION')
  laSetups[1,1]  = 'M_PACK'        && Use Style packs/Sku
  laSetups[2,1]  = 'M_STY_COM'     && Commision at style level
  laSetups[3,1]  = 'M_LN_NOTE'     && Add note to the invoice line  
  laSetups[4,1]  = 'M_LINK_GL'     && Check for gl link
  laSetups[5,1]  = 'M_WareHouse'   && Use maltiple locations
  laSetups[6,1]  = 'M_GenOrNum'    && Enter order # manually
  laSetups[7,1]  = 'M_MATCSTMT'    && Cost method for Material
  laSetups[8,1]  = 'M_MATDYE  '    && Use dylot  
  laSetups[9,1]  = 'M_TAX'         && Use taxes
  laSetups[10,1] = 'M_TAX_RATE'    && Tax 
  laSetups[11,1] = 'M_TAX_METH'    && Tax Method 
  laSetups[12,1] = 'M_UPC_USE'     && use upc numbers 
  laSetups[13,1] = 'M_DIV_LINK'    && gl link codes as devision level 
  laSetups[14,1] = 'M_REP_COMM'    && Sales rep commission  
  laSetups[15,1] = 'M_UPSBOX'      && track boxes for ups manifacturing
  laSetups[16,1] = 'XAGINGTYPE'    && Aging ar by date / terms   
  laSetups[17,1] = 'XPOSTFINV'     && post factored invoice to customer
  laSetups[18,1] = 'XUPSFROM'      && UPS Shipper Code         
  laSetups[19,1] = 'M_CRDT_LMT'    && warn above credit limit
  laSetups[20,1] = 'M_EDTPRICE'    && edit style price Y/N 
  laSetups[21,1] = 'M_INVBULK'     && invoice bulk orders
  laSetups[22,1] = 'M_INVHOLD'     && invoice hold order 
  laSetups[23,1] = 'M_BACKORD'     && Back orders 
  laSetups[24,1] = 'M_TAX_DESC'    && Tax Name 
  laSetups[26,1] = 'M_HST_RATE'    && HST Tax Rate
  laSetups[27,1] = 'M_TrkRolls'    && Keep track of rolls.
  =gfGetMemVar(@laSetups,gcAct_Comp)
  
  lcTaxName  = IIF(EMPTY(laSetups[24,2]),'G.S.T. Tax',laSetups[24,2])
  
  *N000388,1 Define temp files names to be used in gfMatCrl function. [Begin]
  lcTmpJour = gfTempName()
  llTrkRolls = ALLTRIM(laSetups[27,2]) = "Y"
  
  *B606163,1 ABD - this 2 files are copy from the lctmp jour file.[Begin]
  lcTrnTemp  = gfTempName()
  lcTempOrdr = gfTempName()
  *-- This File For Detail rolls order .
  lcRollTmp  = gfTempName()
  llFirstTime = .T.
  *B606163,1 ABD - [End]

  *N000388,1 Define temp files names to be used in gfMatCrl function. [End]
  
  *-- Define temp files names
  lcInvHdr  = gfTempName()
  lcInvLine = gfTempName()
  lcConsInvH= gfTempName()
  lcConsInvD= gfTempName()
  lcUpsBox  = gfTempName()
  lcPackHdr = gfTempName()
  lcInstHdr = gfTempName()
  lcInstLin = gfTempName()
  lcAppCrdt = gfTempName()
  
  lcPackLine= gfTempName()
  lcOrdCanLn= gfTempName()
  llIsCanada = IIF(UPPER(ALLTRIM(gcContCode))='CANADA',.T.,.F.)
  IF UPPER(ALLTRIM(gcContCode))='ENG'
    *-- array to hold English taxes
    *DECLARE laEngStyTax[1,2]
    *laEngStyTax[1,1] = 'NTAXRATE'
    *laEngStyTax[1,2] = 'lnTaxRate'
    *-- variables to hold the tax break and the tax name
    *STORE "" TO lcTaxTitle,lcTaxBreak
    
    *B607226,1 ABD - Define the tax array that hold the vat tax. [Begin]
    DECLARE laEngStyTax[1,2]
    laEngStyTax[1,1] = 'NTAXRATE'
    laEngStyTax[1,2] = 'lnTaxRate'
    *-- variables to hold the tax break and the tax name
    STORE "" TO lcTaxTitle,lcTaxBreak
    *B607226,1 ABD - [End]
    
    llIsEngland = .T.
    *-- english charges temp name
    lcEngChrg = gfTempName()
    laSetups[15,2]='N'        && not to track boxes in case of england
  ELSE
    llIsEngland = .F.
  ENDIF
  llMulCurr   = gfGetMemVar('llMulCurr',gcAct_Comp)    && Use multi currency
  llEditExRt  = gfGetMemVar('LLEDITEXRA',gcAct_Comp)   && Edit exchange rate
  lcWinCh1    = gfTempName()
  lcWinCh2    = gfTempName()
  lcWinCh21   = gfTempName()
  lcWinCh22   = gfTempName()
  lcWinCh3    = gfTempName()
  lcWinCh4    = gfTempName()
  lcWinCh40   = gfTempName()
  lcWinCh41   = gfTempName()
  lcWinCh42   = gfTempName()
  lcWinCh43   = gfTempName()
  lcWinCh44   = gfTempName()
  lcWinCh45   = gfTempName()
  lcWinCh46   = gfTempName()
  lcWinCh51   = gfTempName()
  lcWinCh52   = gfTempName()
  lcWinCh53   = gfTempName()  
  lcfolder    = gfTempName()      && Folder Window Name
  lcfoldprnt  = gcBaseWind        && window parent name for the folder
  lnactfolder = 1                 && active folder

  lcWinChrg = IIF(llIsEngland,lcWinCh51,IIF(llIsCanada,lcWinCh52,lcWinCh53))
  lafoldwinds[1,1] = lafolders[1,1]
  lafoldwinds[1,2] = lcWinCh2
  lafoldwinds[2,1] = lafolders[2,1]
  lafoldwinds[2,2] = lcWinCh3
  lafoldwinds[3,1] = lafolders[3,1]
  lafoldwinds[3,2] = lcWinCh4
  lafoldwinds[4,1] = lafolders[4,1]
  lafoldwinds[4,2] = lcWinChrg
  llNoShow = .F.
  *-- Saving variables for the uncomplete session
  laVars[1]  = 'ldDefInvDate'
  laVars[2]  = 'lcGlSession'
  laVars[3]  = 'ldDefPstDate'
  laVars[4]  = 'lnUnCmSeRc'
  laVars[5]  = 'lcAdTrnSeq'
  laVars[6]  = 'lcAcTrnSeq'
  laVars[7]  = 'lcHisSeq'
  laVars[8]  = 'lcGlSess'
  laVars[9]  = 'lcRepBat'
  laVars[10] = 'llUpdGlDif'
  laVars[11] = 'llUpdMstGL'
  * --Payment Terms Code
  laCodes[1,1]  = 'CTERMCODE'
  laCodes[1,2]  = 'laTerms'
  laCodes[1,3]  = 'lnTerms'
  laCodes[1,4]  = ''
  laCodes[1,5]  = .F.
  laCodes[1,6]  = .F.
  laCodes[1,7]  = lcInvHdr
  laCodes[1,8]  = lcInvHdr
  laCodes[1,9]  = 'laData[4]+laData[1]+laData[5]+laData[3]'
  laCodes[1,10] = 'cTermCode'
  *-- ship via related fields
  laVRltFld[1,1] = 'CUPS'
  laVRltFld[1,2] = 'lcUpsType'

  laDisRltFld[1,1] = 'DISCPCNT'
  laDisRltFld[1,2] = 'lnDisc_Pcnt'
   *-- Payment Terms Related Fields
  laTRltFld[1,1] = 'NTERDISCR'
  laTRltFld[1,2] = 'lnTerDiscR'
  laTRltFld[2,1] = 'EOM'
  laTRltFld[2,2] = 'lcTEOM'
  laTRltFld[3,1] = 'NTERDUED'
  laTRltFld[3,2] = 'lnTDaysDue'
  laTRltFld[4,1] = 'CODYN'
  laTRltFld[4,2] = 'lcTCod'
  laTRltFld[5,1] = 'LINSTALLM'
  laTRltFld[5,2] = 'llInstTerm'
  laTRltFld[6,1] = 'EOMDAY'
  laTRltFld[6,2] = 'lnEomDay'
  
  laCodes[2,1]  = 'SHIPVIA'
  laCodes[2,2]  = 'laShipVia'
  laCodes[2,3]  = 'lnShipVia'
  laCodes[2,4]  = ''
  laCodes[2,5]  = .T.
  laCodes[2,6]  = .F.
  laCodes[2,7]  = lcInvHdr
  laCodes[2,8]  = lcInvHdr
  laCodes[2,9]  = 'laData[4]+laData[1]+laData[5]+laData[3]+cdivision'
  laCodes[2,10] = 'SHIPVIA'
  *--Special instruction Codes
  laCodes[3,1]  = 'SPCINST'
  laCodes[3,2]  = 'laSpcInst'
  laCodes[3,3]  = 'lnSpcInst'
  laCodes[3,4]  = ''
  laCodes[3,5]  = .F.
  laCodes[3,6]  = .F.
  laCodes[3,7]  = lcInvHdr
  laCodes[3,8]  = lcInvHdr
  laCodes[3,9]  = 'laData[4]+laData[1]+laData[5]+laData[3]+cdivision'
  laCodes[3,10] = 'SPCINST'
  *-- Season codes
  laCodes[4,1]  = 'SEASON'
  laCodes[4,2]  = 'laSeasons'
  laCodes[4,3]  = 'lnSeason'
  laCodes[4,4]  = ''
  laCodes[4,5]  = .T.
  laCodes[4,6]  = .F.
  laCodes[4,7]  = lcInvHdr
  laCodes[4,8]  = lcInvHdr
  laCodes[4,9]  = 'laData[4]+laData[1]+laData[5]+laData[3]+cdivision'
  laCodes[4,10] = 'SEASON'
  *-- Devision  codes
  laCodes[5,1]  = 'CDIVISION'
  laCodes[5,2]  = 'laDivision'
  laCodes[5,3]  = 'lnDivision'
  laCodes[5,4]  = ''
  laCodes[5,5]  = .F.
  laCodes[5,6]  = .F.
  laCodes[5,7]  = lcInvHdr
  laCodes[5,8]  = lcInvHdr
  laCodes[5,9]  = 'laData[4]+laData[1]+laData[5]+laData[3]+cdivision'
  laCodes[5,10] = 'cDivision'
  
  laCodes[6,1]  = 'CCANCRESON'
  laCodes[6,2]  = 'laCanReason'
  laCodes[6,3]  = 'lnCanReason'
  laCodes[6,4]  = ''
  laCodes[6,5]  = .F.
  laCodes[6,6]  = .F.
  laCodes[6,7]  = lcOrdCanLn
  laCodes[6,8]  = lcOrdCanLn
  laCodes[6,9]  = "'O'+m.cMOrder+STR(m.LineNo,6)"
  laCodes[6,10] = 'CCANCRESON'
  =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
  SELECT LEFT(cDesc,20),cWareCode FROM WAREHOUS INTO ARRAY laWareHouses
  =gfCloseFile('WAREHOUS')
ELSE 
  STORE IIF(laScrMode[1],'ENABLE','DISABLE') TO lcScopeStat
  
  SELECT (lcInvLine)
  =SEEK(laData[4]+laData[1]+laData[5]+laData[3])
  SCATTER MEMVAR MEMO
  SELECT (lcInvHdr)
  =SEEK(laData[4]+laData[1]+laData[5]+laData[3])
  SCATTER MEMVAR FIELDS &lcFlFields
  SCATTER FIELDS &lcScFields TO laData
ENDIF

llInvChrg = llIsEngland .AND. gfOpenFile(gcDataDir+'MInvChrg',gcDataDir+'MInvChrg','SH')
IF 'AL' $ gcCmpModules
  llPikTkt  = gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'Ordpik','SH')
  SELECT PIKTKT
  SET FILTER TO STATUS <> 'X' AND STATUS <> 'C'
ENDIF
*-- use customer file with a new alias to get the ship name of the store
USE (gcDataDir+'Customer') AGAIN IN 0 ALIAS 'DistCntr' ORDER TAG CUSTOMER
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

lcKeyBmp  = gcBmpHome + "ExtKey.BMP"
lcNew     = gcBmpHome + "new1.bmp"
lcNotes   = gcBmpHome + "NOTES1.BMP"
lcPacks   = gcBmpHome + "packs.bmp"
lcRemove  = gcBmpHome + "rem1.bmp"
lcClose   = gcBmpHome + "Close2.bmp"

*N000388,1 Added for the Rolls screen. [Begin]
lcSelecP = gcBmpHome + "SEL1.BMP"
lcUnSelP = gcBmpHome + "UNSEL.BMP"
*N000388,1 Added for the Rolls screen. [End]

DECLARE laPanelObj[1,3]
laPanelObj[1,1] = 'pbScope'
laPanelObj[1,2] = gcBmpHome + 'POST.BMP'
laPanelObj[1,3] = [WHEN lfwPost() VALID lfPost() MESSAGE 'Post Release Order' &lcScopeStat ]

llOpnFiles=.T.

PUSH MENU _MSYSMENU
lnBrowse = lfGtSavBar('GFCPBROW')
lnFilter = lfGtSavBar('GFSETFILTR')
*C126361,1  TMI [Start] 
*DO (gcScrDir+gcWinAppl+"\ARMInv.SPX")
PUSH KEY
ON KEY
DO (gcScrDir+gcWinAppl+"\ARMREL.SPX")
POP KEY
*C126361,1  TMI [End  ] 
POP MENU _MSYSMENU

=lfClearKey()
SELECT (lcInvLine)
SET RELATION TO
IF WEXIST(lcOrdBrow)
  RELEASE WINDOW (lcOrdBrow)
ENDIF
IF WEXIST(lcInvBrow)
  RELEASE WINDOW (lcInvBrow)
ENDIF
IF WEXIST(lcBrTtlZ)
  RELEASE WINDOW (lcBrTtlZ)
ENDIF
USE IN 'DistCntr'
IF glQuitting
  IF USED(lcInvHdr)
    USE IN (lcInvHdr)
  ENDIF
  ERASE (gcWorkDir+lcInvHdr+".DBF")
  ERASE (gcWorkDir+lcInvHdr+".CDX")
  IF USED(lcInvLine)
    USE IN (lcInvLine)
  ENDIF
  ERASE (gcWorkDir+lcInvLine+".DBF")
  ERASE (gcWorkDir+lcInvLine+".CDX")
  ERASE (gcWorkDir+lcInvLine+".FPT")
  IF USED(lcUpsBox)
    USE IN (lcUpsBox)
  ENDIF
  ERASE (gcWorkDir+lcUpsBox+".DBF")
  ERASE (gcWorkDir+lcUpsBox+".CDX")
  IF USED(lcInstHdr)
    USE IN (lcInstHdr)
  ENDIF
  ERASE (gcWorkDir+lcInstHdr+".DBF")
  ERASE (gcWorkDir+lcInstHdr+".CDX")
  IF USED(lcInstLin)
    USE IN (lcInstLin)
  ENDIF
  ERASE (gcWorkDir+lcInstLin+".DBF")
  ERASE (gcWorkDir+lcInstLin+".CDX")
  IF USED(lcAppCrdt)
    USE IN (lcAppCrdt)
  ENDIF
  ERASE (gcWorkDir+lcAppCrdt+".DBF")
  ERASE (gcWorkDir+lcAppCrdt+".CDX")
  IF llIsEngland AND USED(lcEngChrg)
    USE IN (lcEngChrg)
  ENDIF
  ERASE (gcWorkDir+lcEngChrg+".DBF")
  ERASE (gcWorkDir+lcEngChrg+".CDX")
  IF USED(lcOrdCanLn)
    USE IN (lcOrdCanLn)
  ENDIF
  ERASE (gcWorkDir+lcOrdCanLn+".DBF")
  ERASE (gcWorkDir+lcOrdCanLn+".CDX")
  
  IF USED(lcConsInvH)
    USE IN (lcConsInvH)
    ERASE (gcWorkDir+lcConsInvH+".DBF")
    ERASE (gcWorkDir+lcConsInvH+".CDX")
  ENDIF

  IF USED(lcConsInvD)
    USE IN (lcConsInvD)
    ERASE (gcWorkDir+lcConsInvD+".DBF")
    ERASE (gcWorkDir+lcConsInvD+".CDX")
    ERASE (gcWorkDir+lcConsInvD+".FPT")
  ENDIF
  
  *N000388,1 Close the temp files which was used in gfMatCrl. [Begin]
  IF llRolls AND USED('Rolls')    
    USE IN Rolls
  ENDIF
  
  IF llMatInvJl AND USED('MatInvJl')
    USE IN MatInvJl
  ENDIF 
  
  IF USED(lcTmpRoll)
    USE IN (lcTmpRoll)
    ERASE (gcWorkDir+lcTmpRoll+".DBF")
    ERASE (gcWorkDir+lcTmpRoll+'.CDX') 
    ERASE (gcWorkDir+lcTmpRoll+'.FPT')   
  ENDIF

  IF USED(lcTmpJour)
    USE IN (lcTmpJour)
    ERASE (gcWorkDir+lcTmpJour+".DBF")
    ERASE (gcWorkDir+lcTmpJour+'.CDX') 
    ERASE (gcWorkDir+lcTmpJour+'.FPT')   
  ENDIF

  IF USED(lcFullRoll)
    USE IN (lcFullRoll)
    ERASE (gcWorkDir+lcFullRoll+".DBF")
    ERASE (gcWorkDir+lcFullRoll+'.CDX') 
    ERASE (gcWorkDir+lcFullRoll+'.FPT')   
  ENDIF
  *N000388,1 Close the temp files which was used in gfMatCrl. [End]

  *B606163,1 ABD - Close the temp files which was used in gfMatCrl. [Begin]
  IF USED(lcTrnTemp)
    USE IN (lcTrnTemp)
    ERASE (gcWorkDir+lcTrnTemp+".DBF")
    ERASE (gcWorkDir+lcTrnTemp+'.CDX') 
    ERASE (gcWorkDir+lcTrnTemp+'.FPT')   
  ENDIF

  IF USED(lcTempOrdr)
    USE IN (lcTempOrdr)
    ERASE (gcWorkDir+lcTempOrdr+".DBF")
    ERASE (gcWorkDir+lcTempOrdr+'.CDX') 
    ERASE (gcWorkDir+lcTempOrdr+'.FPT')   
  ENDIF

  IF USED(lcRollTmp)
    USE IN (lcRollTmp)
    ERASE (gcWorkDir+lcRollTmp+".DBF")
    ERASE (gcWorkDir+lcRollTmp+'.CDX') 
    ERASE (gcWorkDir+lcRollTmp+'.FPT')   
  ENDIF
  *B606163,1 ABD - [End]

  *E126684,1  TMI [Start] restore decimal setting
  SET DECIMAL TO &lcSvDeci
  *E126684,1  TMI [End  ] 
  
ENDIF

IF llInvChrg 
  USE IN MInvChrg
ENDIF
IF 'AL' $ gcCmpModules
  IF llPikTkt
    USE IN PIKTKT
  ELSE
    SELECT PIKTKT
    SET FILTER TO
  ENDIF
ENDIF  
RELEASE PAD _INQUIRY OF _MSYSMENU

*!*************************************************************
*! Name      : lfBrowOrd
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Orders
*!*************************************************************
*! Calls     : lfwOrdBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfBrowOrd()
*!*************************************************************
FUNCTION lfBrowOrd
PRIVATE lcFields

lcSelect = SELECT()
SELECT (lcInvHdr)
lnOrdMark = RECNO(lcInvHdr)

lcFields = "cMarker=IIF(RECNO()=lnOrdMark ,'>',' '):H=' ':R:1:W=.F.,;
cSelect :H=' ' :R,cAcc=IIF(Flag$'AO','*****',Account) :H='Acnt.' :R,"
  
lcFields = lcFields + "cOrd=IIF(Flag='O','******',cMOrder) :H='Order#' :R,Custpo :H='Cust. P.O.' :R,cstore=IIF(Consol='Y',cConStore,Store) :H='Store' :R,cMatInv :H='Inv.#' :R,"
lcFields = lcFields + "Ordered :H='Order' :R, FabShip :H= 'Ship' :R, FabShipAmt :H='Ship Amnt.' :13 :R,Cartons :H='Cartons' :R,Weight :H='Weight' :R"

BROWSE FIELDS &lcFields ;
       WINDOW (lcWinCh21)  ;
       IN WINDOW (lcWinCh2);
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfwOrdBrow() ;
       TITLE lcOrdBrow
SELECT (lcSelect)

*!*************************************************************
*! Name      : lfwOrdBrow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show Scope Orders
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfwOrdBrow()
*!*************************************************************
FUNCTION lfwOrdBrow

SELECT (lcInvHdr)
lnOrdMark = RECNO()
SHOW WINDOW (lcOrdBrow) REFRESH SAME
*-- screen fields
SCATTER FIELDS &lcScFields TO laData
*-- data fields
*C126361,4  TMI [Start] 
SET KEY TO 
GO TOP
*C126361,4  TMI [End  ] 
SCATTER MEMVAR FIELDS &lcFlFields
IF !EOF() AND (laSetups[2,2] = 'Y')
  STORE 0 TO lnRep1Comm , lnRep2Comm
 *-- Refresh Sales Rep. header Commession For invoice
  =lfRefComm()
ENDIF
*-- seek account+cMorder+store+PickTkt
=SEEK(laData[4]+laData[1]+laData[5]+laData[3],lcInvLine)
=SEEK('M'+laData[4],'Customer')
*-- back order
lcBackOrd   = IIF(EMPTY(Customer.cBackOrd),laSetups[23,2],Customer.cBackOrd)
*-- get customer price level
lcPriceLvl  = IIF(!EMPTY(Customer.PriceLvl),Customer.PriceLvl,'A')
llUpCnsLine = Flag $ 'OA'
SHOW GETS WINDOW (lcWinCh1) ONLY
*-- invoice Status
DO CASE
  CASE Status = 'H'
    lcOrdStat = 'Hold'    
    lcStatColor = "RGB(255,255,255,255,0,0)"
  CASE Status = 'O'
    lcOrdStat = 'Open'
    lcStatColor = "RGB(255,255,255,0,128,0)"
ENDCASE
=lfRefresh(lcWinCh1)

lcStatus=IIF(laScrMode[4],'ENABLE','DISABLE')

IF cSelect = SPACE(1)
  SHOW GET pbSelect,1 &lcStatus PROMPT '\<Select'
  SHOW GET ibFolder[2] DISABLE
  SHOW GET ibFolder[3] DISABLE
  SHOW GET ibFolder[4] DISABLE
ELSE
  SHOW GET pbSelect,1 &lcStatus PROMPT 'Un\<Select'
  SHOW GET ibFolder[2] ENABLE
  SHOW GET ibFolder[3] ENABLE
  SHOW GET ibFolder[4] ENABLE
ENDIF
lcStatus=IIF(laScrMode[4] AND cSelect <> SPACE(1) ,'ENABLE','DISABLE')

IF FabShip = 0
  SHOW GET pbShipInv,1 &lcStatus PROMPT 'S\<hip'
ELSE
  SHOW GET pbShipInv,1 &lcStatus PROMPT 'Uns\<hip'
ENDIF

IF !EMPTY(laData[3]) AND lcUpdPik $ 'NA' 
  SHOW GET pbShipInv,1 DISABLE PROMPT 'Uns\<hip'
ENDIF

lnCartons = nCartons
IF Consol='Y'

  DIMENSION laShipN1[1,2],laShipN2[1,2]
  STORE '' TO laShipN1,laShipN2
  
  SELECT DIST IIF(EMPTY(DistCntr.Dba),DistCntr.StName,DistCntr.Dba),DistCntr.Store ;
  FROM DistCntr,Customer WHERE ;
  DistCntr.Type+DistCntr.Account+DistCntr.Store='S'+laData[4]+customer.Dist_Ctr ;
  INTO ARRAY laShipN1

  SELECT DIST IIF(EMPTY(DistCntr.Dba),DistCntr.StName,DistCntr.Dba),DistCntr.Store ;
  FROM DistCntr,Customer WHERE ;
  DistCntr.Type+DistCntr.Account+DistCntr.Store='M'+laData[4] ;
  INTO ARRAY laShipN2

  IF !EMPTY(laShipN1)
    DECLARE laShipName[1,1]
    =ACOPY(laShipN1,laShipName)
  ENDIF  
  IF !EMPTY(laShipN1) OR !EMPTY(laShipN2)
    DIMENSION laShipName[IIF(!EMPTY(laShipN1),ALEN(laShipN1,1),0)+;
                         IIF(!EMPTY(laShipN2),ALEN(laShipN2,1),0),2]
    IF !EMPTY(laShipN2)
      *-- add the ship name of the account to the ship name array
      *-- after the ship name of the store
      =ACOPY(laShipN2,laShipName,1,ALEN(laShipN2),IIF(!EMPTY(laShipN1),ALEN(laShipN1),0)+1)
    ENDIF
  ENDIF
  DIMENSION laShipName[ALEN(laShipName,1)+1,2]
  =AINS(laShipName,1)
  *-- Add at store level for the array
  laShipName[1,1]='At Store Level'
  laShipName[1,2]='********'
ENDIF

*!*************************************************************
*! Name      : lfBrowDet
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Invoice lines
*!*************************************************************
*! Calls     : lfwBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfBrowDet()
*!*************************************************************
FUNCTION lfBrowDet
PRIVATE lcFields

lcSelect = SELECT()
SELECT (lcInvLine)
lnInvMark = RECNO()

lcFields = "cMarker=IIF(RECNO()=lnInvMark ,'>',' '):H=' ':R:1:W=.F.,Fabric :H='Fabric' :R,Color :H='Color' :R,"
lcFields = lcFields + IIF(laSetups[8,2]='Y',"Dyelot :R,",'')
lcFields = lcFields +IIF('AL' $ gcCmpModules,"","Group :H='Group' :R,")+;
           "FabBook :H='Ordered' :R,"+;
           "FabQty :H='Release' :R,"+;
           "nSellPrice :H='Gross Price' :P='999999999.99999' :R,"+;
           "Disc_Pcnt :H='Disc. Pcnt.' :P='999.99' :R,nSellNetPr :H='Net Price' :P='999999999.99999' :R,"+;
           "lnAmount=CEILING(nSellNetPr*FabQty*100)/100 :H='Ship Amount':R:P='99999999999.99'"
*C126361,1  TMI [End  ] 
*E126684,1  TMI [End  ] 
lcFields = lcFields + IIF('AL' $ gcCmpModules,",Packed=IIF(lPacked,'Y','N') :H='PS':R",'')
IF WEXIST(lcBrTtlZ)
  HIDE WINDOW (lcBrTtlZ)
  RELEASE WINDOW (lcBrTtlZ)
ENDIF
IF WEXIST(lcInvBrow)
  HIDE WINDOW (lcInvBrow)
  RELEASE WINDOW (lcInvBrow)
ENDIF
IF llZoom    && Zoom invoice line option
  HIDE WINDOW (lcWinCh42),(lcWinCh44),(lcWinCh45),(lcWinCh46) SAME
  SHOW GETS WINDOW (lcWinCh42) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh44) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh45) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh46) DISABLE ONLY
  BROWSE FIELDS &lcFields ;
         FOR Consol = &lcInvHdr..Consol ;
         WINDOW (lcWinCh43)  ;
         IN WINDOW (lcWinCh4);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
	     NOCLEAR           ;
         WHEN lfwBrow()    ;
         TITLE lcBrTtlZ 
ELSE
  SHOW WINDOW (lcWinCh42),(lcWinCh44),(lcWinCh45),(lcWinCh46) TOP
  BROWSE FIELDS &lcFields ;
         FOR Consol = &lcInvHdr..Consol ;
         WINDOW (lcWinCh41)  ;
         IN WINDOW (lcWinCh4);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
	     NOCLEAR           ;
         WHEN lfwBrow()    ;
         TITLE lcInvBrow         
ENDIF
=lfwBrow()
SELECT (lcSelect)

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Show line details
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfwBrow()
*!*************************************************************
FUNCTION lfwBrow
PRIVATE lcLineStat
SELECT (lcInvLine)

lnInvMark = RECNO(lcInvLine)
IF llZoom
  SHOW WINDOW (lcBrTtlZ) REFRESH SAME
ELSE
  SHOW WINDOW (lcInvBrow) REFRESH SAME
ENDIF
SCATTER MEMVAR MEMO

lcStatus=IIF(&lcInvHdr..Consol <> 'Y' AND laScrMode[4],'ENABLE','DISABLE')
SHOW GETS WINDOW (lcWinCh42) &lcStatus ONLY
SHOW GETS WINDOW (lcWinCh44) &lcStatus ONLY

*N000388,1 Disable Sell Conversion. [Begin]
DO lpChkRolBt
SHOW GET m.nSellConv DISABLE
*N000388,1 Disable Sell Conversion. [End]

SHOW GETS WINDOW (lcWinCh45) &lcStatus ONLY
SHOW GETS WINDOW (lcWinCh46) &lcStatus ONLY

*lcLineStat = IIF(llZoom .OR. &lcInvHdr..Consol='Y' OR lNewLine OR laScrMode[1],'DISABLE','ENABLE')
lcLineStat = IIF(llZoom .OR. &lcInvHdr..Consol='Y' OR laScrMode[1],'DISABLE','ENABLE')

*C126361,3  TMI [Start] 
IF laScrMode[4]
  *C126361,3  TMI [End  ] 

  *N000388,1 Show prompt Rolls , Lots. [Begin]
  IF USED('Fabric') AND Fabric.lTrkRolls
    
    *B606702,1 Fix bug of showing Rolls prompt when llTrkRolls is false. [Begin]
    *SHOW GET pbRolls,1 &lcLineStat PROMPT '\<Rolls'  
    IF llTrkRolls
      SHOW GET pbRolls,1 &lcLineStat PROMPT '\<Rolls'  
    ELSE
      SHOW GET pbRolls,1 &lcLineStat PROMPT '\<Lots'  
    ENDIF
    *B606702,1 Fix bug of showing Rolls prompt when llTrkRolls is false. [End]
    
  ELSE
    SHOW GET pbRolls,1 &lcLineStat PROMPT '\<Lots'
  ENDIF
  *N000388,1 Show prompt Rolls , Lots. [End]
  
  IF m.FabQty = 0
    SHOW GET pbShipLine,1 &lcLineStat PROMPT '\<Ship'
  ELSE
    SHOW GET pbShipLine,1 &lcLineStat PROMPT '\<Clear'
  ENDIF
  
  llCnRemain = !m.LBACKORD AND !m.lNewLine
  IF lcBackOrd = 'I' AND ;
    (m.FabBook > m.FabQty)
    SHOW GET llCnRemain &lcLineStat
  ELSE
    SHOW GET llCnRemain DISABLE
  ENDIF
  IF SEEK('O'+cMOrder+STR(LineNo,6),lcOrdCanLn)
    SHOW GET pbCnRemain &lcLineStat
  ELSE
    SHOW GET pbCnRemain DISABLE
  ENDIF
  lcLineStat = IIF(llZoom .OR. &lcInvHdr..Consol='Y' ,'DISABLE','ENABLE')
  
  *C126361,3  TMI [Start] 
ENDIF
*C126361,3  TMI [End  ] 

SHOW GET pbRemove &lcLineStat
=lfRefresh(lcWinCh42)
=lfRefresh(lcWinCh44)
=lfRefresh(lcWinCh46)

IF !EMPTY(laData[3])
*-- Not empty PikTkt
  DO CASE    && Modify Picking upon invoice Cases
    CASE lcUpdPik = 'A'   && Add
      SHOW GET pbNew ENABLE
      IF !&lcInvLine..lNewLine
        SHOW GET m.FabQty DISABLE
      ENDIF
      
    CASE lcUpdPik = 'N'     && No
      SHOW GET pbNew DISABLE
      SHOW GET pbShipLine,1 DISABLE PROMPT '\<Ship'
      SHOW GET m.FabQty DISABLE  
  
    CASE lcUpdPik = 'M'    && Modify 
      SHOW GET pbNew DISABLE
  ENDCASE
ELSE
  *-- Empty piktikt
  IF lcUpdPik $ 'AY'    && Yes
    IF &lcInvHdr..Consol = 'N' && new line
      SHOW GET pbNew ENABLE
    ELSE
      SHOW GET pbNew DISABLE  
    ENDIF
  ELSE
    SHOW GET pbNew DISABLE
  ENDIF
ENDIF

*N000388,1 Check to disable Rolls button. [Begin]
IF USED('Fabric')
  IF ((llTrkRolls AND Fabric.lTrkRolls) OR laSetups[7,2] $ 'L') AND &lcInvHdr..Consol <> "Y"
    IF Fabric.cDye_Flg='Y' 
      IF EMPTY(&lcInvLine..Dyelot)
        SHOW GET pbRolls DISABLE
      ELSE
        SHOW GET pbRolls ENABLE      
      ENDIF  
    ELSE
      SHOW GET pbRolls ENABLE
    ENDIF
  ELSE
    SHOW GET pbRolls DISABLE
  ENDIF
ENDIF  
*N000388,1 Check to disable Rolls button. [End]

IF laScrMode[2]
  SHOW GET pbRolls  DISABLE
  SHOW GET pbNew    DISABLE
  SHOW GET pbRemove DISABLE
ENDIF

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Change folders
*!*************************************************************
*! Calls     : gfwCodePop,lfGetAddInfo,gfRltFld,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfActFolder()
*!*************************************************************
FUNCTION lfActFolder

SELECT (lcInvLine)

*N000388,1 Refresh the relation. [Begin]
IF EMPTY(SET('RELATION'))
  IF USED('Fabric')
    SET RELATION TO Fabric + Color INTO Fabric ADDITIVE
  ENDIF  
  IF USED('FabDye')
    SET RELATION TO Fabric + Color + cWareCode + Dyelot INTO FabDye ADDITIVE
  ENDIF  
ENDIF  
*N000388,1 Refresh the relation. [End]

IF lnActFolder = 3
  
  *N000388,1 Always force not to be end  of file. [Begin]
  IF EOF()
    LOCATE
  ENDIF  
  *N000388,1 Always force not to be end  of file. [End]
  
  SET KEY TO laData[4]+laData[1]+laData[5]+laData[3]
ELSE
  SET KEY TO
ENDIF

DO CASE
  CASE lnActFolder = 1
    =lfReClcShp(.T.)
    lcStatus = IIF(laScrMode[4],'ENABLE','DISABLE')
    SHOW GETS WINDOW (lcWinCh3)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh40) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh42) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh44) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh45) DISABLE ONLY
    SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh22) &lcStatus ONLY
    
    =lfwOrdBrow()
    
    *--To handle Select, Unselect, Select all, Select None and Invert invoices
    *C126361,4  TMI [Start] 
    *=lfAdjSButt('')
    =IIF(laScrMode[4],lfAdjSButt(''),'')
    *C126361,4  TMI [End  ] 
    IF !EMPTY(laData[3]) AND lcUpdPik $ 'NA'
      SHOW GET pbShipInv,1 DISABLE PROMPT 'Uns\<hip'
    ENDIF

    SHOW GET ibInvoice ENABLE
  CASE lnActFolder = 2
    SELECT (lcInvHdr)

    SHOW GETS WINDOW (lcWinCh22) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh40) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh42) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh44) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh45) DISABLE ONLY
    SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY
    lcStatus = IIF(laScrMode[4] AND (Consol='Y' OR EMPTY(Flag)),'ENABLE','DISABLE')
    =gfwCodePop(@laCodes,'CTERMCODE','T')
    =gfwCodePop(@laCodes,'SHIPVIA','T')
    =gfwCodePop(@laCodes,'SPCINST','T')
    
    *N000388,1 Commented out. [Begin]
    *=gfwCodePop(@laCodes,'SEASON','T')
    *=gfwCodePop(@laCodes,'CDIVISION','T')
    *N000388,1 Commented out. [End]
    
    SHOW GETS WINDOW (lcWinCh3) &lcStatus ONLY
    =lfGetAddInfo()
    
    *N000388,1 Commented out. [Begin]
    *SHOW GET lnSeason    DISABLE
    *SHOW GET lnDivision  DISABLE
    *N000388,1 Commented out. [End]
    
    SHOW GET lnWareHouse DISABLE
    IF Consol = 'Y'
      laCodes[2,5]  = .T.
    ELSE 
      laCodes[2,5]  = .F.
    ENDIF
    =gfwCodePop(@laCodes,'SHIPVIA','L')
    *-- change elemnt (All) in shipvia popup to be (Multi)
    =lfModShpAr()
    IF laScrMode[4] AND (Consol = 'Y' OR lfChekCons())
      SHOW GET lnShipVia ENABLE
    ELSE  
      SHOW GET lnShipVia DISABLE
    ENDIF
    IF laScrMode[4]
      SHOW GET m.Note1 ENABLE
      SHOW GET m.Note2 ENABLE
    ENDIF
    IF !llEditExRt OR laData[14] = gcBaseCurr
      SHOW GET m.NEXRATE DISABLE
    ELSE
      SHOW GET m.NEXRATE &lcStatus
    ENDIF
    
    PRIVATE lcMainAcct , lnCustRecn
    lnCustRecn = RECNO('Customer')
    lcMainAcct = IIF(SEEK('M'+Account,'Customer'),Customer.Consol,'')
    
    *N000388,1 Remove consolidated invoice option. [Begin]
    lcMainAcct = 'N'
    *N000388,1 Remove consolidated invoice option. [End]

    lcSalesStat = IIF(laScrMode[4] AND laSetups[2,2]<>'Y' AND ;
                  IIF(lcMainAcct='Y',CONSOL='Y',.T.),'ENABLE','DISABLE')
    IF BETWEEN(lnCustRecn,1,RECCOUNT('Customer'))
      GOTO lnCustRecn IN Customer
    ENDIF
    
    SHOW GET ibSaleRep1 &lcSalesStat
    SHOW GET m.Rep1     &lcSalesStat
    SHOW GET laData[9]  &lcSalesStat
    SHOW GET ibSaleRep2 &lcSalesStat
    SHOW GET m.Rep2     &lcSalesStat
    SHOW GET laData[10] &lcSalesStat
  CASE lnActFolder = 3
 
    SHOW GETS WINDOW (lcWinCh22) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh3)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh40) ENABLE  ONLY
    SELECT (lcInvLine)
    IF !llZoom .AND. &lcInvHdr..Consol <> 'Y'
    
     
      lcStat = IIF(laScrMode[4],'ENABLE','DISABLE')
      SHOW GETS WINDOW (lcWinCh42) &lcStat ONLY
      SHOW GETS WINDOW (lcWinCh44) &lcStat ONLY
      
      *N000388,1 Disable Sell Conversion. [Begin]
      DO lpChkRolBt
      SHOW GET m.nSellConv DISABLE
      *N000388,1 Disable Sell Conversion. [End]
        
      *C126361,4  TMI [Start] 
      *SHOW GETS WINDOW (lcWinCh45) ENABLED ONLY
      *SHOW GETS WINDOW (lcWinCh46) ENABLED ONLY
      lcStat = IIF(laScrMode[4],'ENABLED','DISABLED')
      SHOW GETS WINDOW (lcWinCh45) &lcStat ONLY
      SHOW GETS WINDOW (lcWinCh46) &lcStat ONLY
      *C126361,4  TMI [End  ] 
      IF laScrMode[4]
        SHOW GET pbNew ENABLE 
      ELSE
        SHOW GET pbNew DISABLE
      ENDIF
      IF laScrMode[4] .AND. !EMPTY(Fabric+Color)
        SHOW GET pbRemove ENABLE
      ELSE
        SHOW GET pbRemove DISABLE
      ENDIF
      IF laSetups[3,2]='Y' .AND. !EMPTY(Fabric+Color)
        SHOW GET pbLineNote ENABLE
      ELSE
        SHOW GET pbLineNote DISABLE
      ENDIF
      =lfwBrow()
      _CUROBJ = OBJNUM(ibInv200E)

    ENDIF
    
  CASE lnActFolder = 4
    =lfReClcShp(.T.)

    SELECT (lcInvHdr)
    llCod = (m.Cod_Flag='Y')
    IF laScrMode[4]
     =IIF(!llIsEngland .AND. lCompUps,lfvUpsChrg(),.T.)
    ENDIF
    =gfRltFld(&lcInvHdr..ShipVia,@laVRltFld,'SHIPVIA')
    lcUpsTrack  = IIF(SUBSTR(lcUpsType,1,5)='USUPS','UPS COD Tracking#','COD Tracking#')
    lcUpsPrompt = IIF(SUBSTR(lcUpsType,1,5)='USUPS','UPS Insur.','Insurance ')
    llOpnCrdt = .F.
    IF laScrMode[4] AND llCod AND m.TotalChg > 0 AND (Consol='Y' OR EMPTY(Flag))
      DO gfAcOpCrdt IN (gcapphome+gcwinappl+'\ARMtInv.PRG') WITH laData[4],laData[14],'llOpnCrdt'
      SHOW GET m.Cod_Amt ENABLE
    ELSE
      SHOW GET m.Cod_Amt DISABLE
    ENDIF

    =lfRefresh(lcWinChrg)
    lcStatus = IIF(laScrMode[4] AND m.FabShipAmt > 0 AND Consol <> 'Y','ENABLE','DISABLE')
    SHOW GET m.Cartons &lcStatus
    SHOW GET m.Weight  &lcStatus
    SHOW GET m.Freight &lcStatus
    SHOW GET m.Insur   &lcStatus
    SHOW GET m.Cod     &lcStatus
    lcUpsSt= IIF(laScrMode[4] AND m.FabShipAmt > 0 AND Consol <> 'Y' AND SUBSTR(lcUpsType,1,5)= "USUPS",'ENABLE','DISABLE')
    SHOW GET pbUpsBox  &lcUpsSt
    SHOW GET pbCharges &lcStatus
    SHOW GET m.DISCPCNT   &lcStatus
    SHOW GET m.DISCOUNT   &lcStatus
    SHOW GET lnpstrule    &lcStatus
    SHOW GET m.nPstRate   &lcStatus
    SHOW GET m.nPstAmt    &lcStatus
    SHOW GET m.Tax_Rate   &lcStatus
    SHOW GET m.nHstRate    &lcStatus
    SHOW GET m.nHstAmt     &lcStatus

    SHOW GET m.TAX_AMT &lcStatus
    lcStatus = IIF(laScrMode[4] AND m.FabShipAmt > 0 AND (Consol='Y' OR EMPTY(Flag)),'ENABLE','DISABLE')

    *N000388,1 Commented out. [Begin]
    *SHOW GET m.BOL_NO     &lcStatus
    *SHOW GET m.CODTAG     &lcStatus
    *SHOW GET m.CCODTRCKNO &lcStatus
    SHOW GET m.FabShip &lcStatus
    SHOW GET m.FabShipAmt &lcStatus
    *N000388,1 Commented out. [End]

    SHOW GET llCod        &lcStatus
    SHOW GET m.lUpsIns,1  &lcStatus PROMPT lcUpsPrompt 
    SHOW GET m.TRDE_DISC  &lcStatus

    *B606702,1 Enable/Disable Factor code. [Begin]
    SHOW GET ibFactor    &lcStatus
    SHOW GET m.cFacCode  &lcStatus
    *B606702,1 Enable/Disable Factor code. [End]

    =gfRltFld(m.cTermCode,@laTRltFld,'CTERMCODE')
    

    IF llOpnCrdt
      SHOW GET pbCredits ENABLE
    ELSE
      SHOW GET pbCredits DISABLE
    ENDI
    IF laScrMode[4] AND m.TotalChg > 0 AND llInstTerm AND (Consol='Y' OR EMPTY(Flag))
      SHOW GET pbInvInst ENABLE
    ELSE
      SHOW GET pbInvInst DISABLE
    ENDIF
    SHOW GETS WINDOW (lcWinCh22) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh3)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh40) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh42) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh44) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh45) DISABLE ONLY
ENDCASE

*!*************************************************************
*! Name      : lfvShipName
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Ship to DC
*!*************************************************************
*! Calls     : lfGetAddInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfvShipName()
*!*************************************************************
FUNCTION lfvShipName
*--Store
laData[5] = laShipName[lnShipName*2]
SELECT (lcInvHdr)
=RLOCK()
PRIVATE lcField
lcField = IIF(Consol = 'Y','cConStore','Store')
REPLACE &lcField WITH laData[5]
UNLOCK
SELECT (lcInvLine)
SET ORDER TO TAG 'CONSOL'
=SEEK('Y')
REPLACE REST cConStore WITH laData[5] WHILE ;
             Consol+Account+cDivision+cCurrCode+Fabric+Color = 'Y'             
SET ORDER TO TAG (lcInvLine)
SELECT (lcInvHdr)
SHOW GET laData[5] DISABLE
=lfGetAddInfo()

*!*************************************************************
*! Name      : lfGetAddInfo
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Get Shipto and Billto Addresses
*!*************************************************************
*! Calls     : gfGetAdr,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfGetAddInfo()
*!*************************************************************
FUNCTION lfGetAddInfo

STORE '' TO lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
            lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5

IF !SEEK('S'+laData[4]+laData[5],'Customer')
  =SEEK('M'+laData[4],'Customer')
ENDIF
lcBillName = Customer.BtName
=gfGetAdr('Customer','','','',1,'2')
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
ENDFOR
*-- Store
IF laData[5] <> '********'
  *-- Seek O + Order
  =SEEK('O'+laData[1],'MaSoHdr')
  IF MaSoHdr.Alt_ShpTo  
    *-- Alternate Ship to Address
    lcShipName = MaSoHdr.StName
    lcShipAdd1 = MaSoHdr.cAddress1
    lcShipAdd2 = MaSoHdr.cAddress2
    lcShipAdd3 = MaSoHdr.cAddress3
    lcShipAdd4 = MaSoHdr.cAddress4
    lcShipAdd5 = MaSoHdr.cAddress5
  ELSE
    *-- Customers ship to name or customer do business as name
    lcShipName =  IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
    lnCusRecNo = RECNO('Customer')    
    IF !EMPTY(Customer.Dist_Ctr)
      lcDistCntr = Customer.Dist_Ctr   && Customer distribution center
      llNoThing = SEEK('S'+laData[4]+lcDistCntr,'CUSTOMER')
    ENDIF
    IF !llNoThing AND RECCOUNT('Customer') >= lnCusRecNo
      GO lnCusRecNo IN Customer
    ENDIF
    =gfGetAdr('CUSTOMER','','','',1,'')
    FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount = STR(laAddress[lnCount,1],1)
      lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    ENDFOR  
    IF RECCOUNT('Customer') >= lnCusRecNo
      GO lnCusRecNo IN Customer
    ENDIF
  ENDIF
ENDIF  
lnWareHouse = ASCAN(laWareHouses,laData[7])    && ladata[7]=Ware house code
lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
IF Consol='Y'
  lnShipName = ASCAN(laShipName,laData[5])
  lnShipName = IIF(lnShipName=0,1,ASUBSCRIPT(laShipName,lnShipName,1))
  SHOW GET lnShipName ENABLE
ELSE
  DIMENSION laShipName[1,2]
  laShipName[1,1] = lcShipName
  laShipName[1,2] = laData[5]
  lnShipName = 1
  SHOW GET lnShipName DISABLE
ENDIF
=lfRefresh(lcWinCh3)

*!*************************************************************
*! Name      : lpClsScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Cancel new Invoice
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

SELECT UnCmSess

IF SEEK('O'+PADR('MRELSEORD',10)+PADR(gcUser_id,10)+lcSession)
  REPLACE STATUS WITH 'I'
ENDIF
llContinue = .F.
UNLOCK

SELECT (lcBaseFile)
=gfObj_Lock(.F.)

IF laScrMode[4]
  =lfvOrder(lcRelOrd)
ENDIF

SELECT (lcInvHdr)

*-- end of lpClsScr.
*!*************************************************************
*! Name      : lfvDefaults
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice Session Defaults
*!*************************************************************
*! Calls     : gfModalGen,CHECKPRD
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDefaults()
*!*************************************************************
FUNCTION lfvDefaults

IF EMPTY(ldDefInvDate)
  *-- Message : 40032
  *-- Must enter a default invoice date.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('INM40032B00000','ALERT','invoice')
  _CUROBJ = OBJNUM(ldDefInvDate)
  RETURN
ENDIF
IF laSetups[4,2] = 'Y' .AND. EMPTY(ldDefPstDate)
  *-- Message : 40032
  *-- Must enter a default posting date.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('INM40032B00000','ALERT','posting')
  _CUROBJ = OBJNUM(ldDefPstDate) 
  RETURN
ENDIF
IF laSetups[4,2] = 'Y' .AND. ldDefInvDate > ldDefPstDate
  *-- Message : 40086
  *-- invoice date cannot be greater than posting date
  *-- Button : 00000 
  *-- Ok
  =gfModalGen('TRM40086B00000','ALERT','')
  _CUROBJ = OBJNUM(ldDefPstDate) 
  RETURN
ENDIF
IF !CHECKPRD(IIF(laSetups[4,2]='Y',ldDefPstDate,ldDefInvDate),'lcGlYear','lcGlPeriod','IN')
  _CUROBJ = OBJNUM(ldDefPstDate) 
  RETURN
ENDIF
CLEAR READ

*!*************************************************************
*! Name      : lpShow
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Screen Show Function
*!*************************************************************
*! Calls     : ARINVSES.SPX,lfwOrdBrow()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpShow()
*!*************************************************************
FUNCTION lpShow
PRIVATE llConsol
DEFINE BAR lnBrowse OF P03PU03 PROMPT 'Fil\<ter' SKIP FOR .T.
DEFINE BAR lnFilter OF P03PU03 PROMPT '\<Browse' SKIP FOR .T.

SHOW GET pbDlt  DISABLE
SHOW GET pbBrws DISABLE


DO CASE
  CASE laScrMode[1]
    
    *C126361,3  TMI [Start] 
    lcRelOrd =  '      '
    *C126361,3  TMI [End  ]
    
    *C126361,1  TMI [Start] set post variable to fales
    llPost = .F.
    SHOW GET pbScope DISABLE
    *C126361,1  TMI [End  ] 

    *N000388,1 Close the temp files which was used in gfMatCrl. [Begin]
    IF USED(lcTmpRoll)
      USE IN (lcTmpRoll)
      ERASE (gcWorkDir+lcTmpRoll+".DBF")
      ERASE (gcWorkDir+lcTmpRoll+'.CDX') 
      ERASE (gcWorkDir+lcTmpRoll+'.FPT')   
    ENDIF

    IF USED(lcTmpJour)
      USE IN (lcTmpJour)
      ERASE (gcWorkDir+lcTmpJour+".DBF")
      ERASE (gcWorkDir+lcTmpJour+'.CDX') 
      ERASE (gcWorkDir+lcTmpJour+'.FPT')   
    ENDIF

    IF USED(lcFullRoll)
      USE IN (lcFullRoll)
      ERASE (gcWorkDir+lcFullRoll+".DBF")
      ERASE (gcWorkDir+lcFullRoll+'.CDX') 
      ERASE (gcWorkDir+lcFullRoll+'.FPT')   
    ENDIF
    *N000388,1 Close the temp files which was used in gfMatCrl. [End]

    *B606163,1 ABD - Close the temp files which was used in gfMatCrl. [Begin]
    IF USED(lcTrnTemp)
      USE IN (lcTrnTemp)
      ERASE (gcWorkDir+lcTrnTemp+".DBF")
      ERASE (gcWorkDir+lcTrnTemp+'.CDX') 
      ERASE (gcWorkDir+lcTrnTemp+'.FPT')   
    ENDIF

    IF USED(lcTempOrdr)
      USE IN (lcTempOrdr)
      ERASE (gcWorkDir+lcTempOrdr+".DBF")
      ERASE (gcWorkDir+lcTempOrdr+'.CDX') 
      ERASE (gcWorkDir+lcTempOrdr+'.FPT')   
    ENDIF
    *B606163,1 ABD - [End]

  
    DO lpUpdNotes
    
    lnLastMode = 1
    STORE 0 TO laQtyPaste
    *C126361,1  TMI [Start] 
    *SHOW GET pbScope ENABLE
    *C126361,1  TMI [End  ] 
    lcStatColor = gcObjColor
    STORE '' TO lcOrdStat
    llCancel = .F.
    IF EMPTY(ldDefInvDate) .OR. EMPTY(ldDefPstDate)
      STORE gdSysDate TO ldDefInvDate,ldDefPstDate
      *C126361,1  TMI [Start] no need for this screen in release order
      *DO (gcScrDir+gcWinAppl+"\ARINVSES.SPX")      
      *C126361,1  TMI [End  ] 
    ENDIF
    IF llCancel
      glQuitting=.T.
      CLEAR READ
      RETURN
    ENDIF
    IF llGoAndChk AND lfChkUnComS()
      RETURN
    ENDIF
    SELECT (lcInvHdr)
    SET RELATION TO 
    IF lnActFolder <> 1
      lnlastfold = lnactfolder
      lnActFolder = 1
      =lfChngFolder(lnActFolder)
    ENDIF  
    
  
  CASE laScrMode[2]   && View Mode mode
  
    SHOW GETS WINDOW (lcWinCh1)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh42) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh44) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh45) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh46) DISABLE ONLY
  
    =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
    =gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')
    
    SHOW GET pbDlt ENABLE

  CASE laScrMode[4]   && Add mode
    
    IF lnLastMode <> 4
      IF llOpnfiles  
        =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
        =gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')
      ENDIF

      lnLastMode = 4      
      SELECT 'UNCMSESS'
      IF SEEK('I')
        BLANK
      ELSE
        APPEND BLANK     && Add record for the uncomplete session
      ENDIF
      *C126361,3  TMI [Start] 
      REPLACE Status     WITH 'O' ,;
              cUTranType WITH lcProgID ,;
              cUserId    WITH gcUser_id ,;
              cSession   WITH lcSession ,;
              cProgram   WITH 'ARMREL'  ,;
              cCurrScr   WITH 'ARMREL'  ,;
              dTranDate  WITH gdSysDate ,;
              cTranTime  WITH TIME()
      *C126361,1  TMI [End  ] 
      UNLOCK
      lcFiles = 'lcInvHdr,'+lcInvHdr+','+lcInvHdr+';'+;
                'lcInvLine,'+lcInvLine+','+lcInvLine+';'+;
                'lcInstHdr,'+lcInstHdr+','+lcInstHdr+';'+;
                'lcInstLin,'+lcInstLin+','+lcInstLin+';'+;
                'lcAppCrdt,'+lcAppCrdt+','+lcAppCrdt+';'+;
                IIF(laSetups[15,2]='Y','lcUpsBox,'+lcUpsBox+','+lcUpsBox+';','')+;
                IIF(llIsEngland,'lcEngChrg,'+lcEngChrg+','+lcEngChrg+';','')
             
      lcFiles = lcFiles + 'lcOrdCanLn,'+lcOrdCanLn+','+lcOrdCanLn+';'
      =gfSavSess(lcProgID, lcFiles, @laVars,lcSession)
      SELECT (lcInvHdr)
      SET ORDER TO TAG CONSOL
      GO TOP
      DO WHILE !EOF()
        lcAccount = Account
        lcKey = Consol+Account+cDivision+cCurrCode
        lnInvoices = 0
        lcOrderNo  = cMOrder
        lcTermCode = cTermCode
        lcShipvia = IIF(MaSoHdr.shipvia='*','*',Shipvia)        

        *N000388,1 Remove consolidated invoice option. [Begin]
        *IF SEEK('M'+lcAccount,'Customer') AND Customer.CONSOL='Y'
        IF SEEK('M'+lcAccount,'Customer') AND Customer.CONSOL='Y' AND .F.
        *N000388,1 Remove consolidated invoice option. [End]
    
          SCAN REST WHILE Consol+Account+cDivision+cCurrCode = lcKey
            lnInvoices = lnInvoices + 1
            lcOrderNo = IIF(cMOrder=lcOrderNo,lcOrderNo,SPACE(6))
            lcTermCode =IIF(cTermCode=lcTermCode,lcTermCode,SPACE(6))
            lcShipvia = IIF(Shipvia=lcShipVia,lcShipVia,'*')
          ENDSCAN
        ENDIF
        llConsInv = SEEK('M'+lcAccount,'Customer') AND Customer.CONSOL='Y' AND lnInvoices > 1
        
        *N000388,1 Remove consolidated invoice option. [Begin]
        llConsInv = .F.
        *N000388,1 Remove consolidated invoice option. [End]
        
        IF lnInvoices > 1 AND EMPTY(lcTermCode)
        ENDIF
        =SEEK(lcKey) 
        SCAN REST WHILE Consol+Account+cDivision+cCurrCode = lcKey FOR lnInvoices > 1
          SCATTER MEMVAR
          m.ShipVia = lcShipVia
          SELECT (lcConsInvH)
          IF !SEEK('Y'+m.Account+m.cDivision+m.cCurrCode)
            
            APPEND BLANK
            REPLACE cSelect   WITH '»'         ,;
                    Consol    WITH 'Y'         ,;
                    Account   WITH m.Account   ,;
                    cCurrCode WITH m.cCurrCode ,;
                    nExRate   WITH m.nExRate   ,;
                    nCurrUnit WITH m.nCurrUnit ,;
                    cMOrder   WITH lcOrderNo   ,;
                    cTermCode WITH m.cTermCode ,;
                    SpcInst   WITH m.SpcInst   ,;
                    ShipVia   WITH m.Shipvia   ,;
                    lUpsIns   WITH m.lUpsIns   ,;
                    InvDate   WITH m.InvDate   ,;
                    ShipDate  WITH m.ShipDate  ,;
                    dPostDate WITH m.dPostDate ,;
                    UpsZone   WITH m.UpsZone   ,;
                    Phone     WITH m.Phone     ,;
                    Tax_Rate  WITH m.Tax_Rate  ,;
                    nPstRate  WITH m.nPstRate  ,;
                    cTaxRule  WITH m.cTaxRule  ,;
                    Status    WITH m.Status    ,;
                    Note1     WITH m.Note1     ,;
                    Note2     WITH m.Note2     ,;
                    Rep1      WITH m.Rep1      ,;
                    Comm1     WITH 0           ,;
                    Rep2      WITH m.Rep2      ,;
                    Comm2     WITH 0           ,;
                    Dept      WITH m.Dept      ,; 
                    cFacCode  WITH m.cFacCode  ,;
                    cWareCode WITH m.cWareCode ,;
                    DueDate   WITH m.DueDate   ,;
                    Trde_Disc WITH m.Trde_Disc ,;
                    Approval  WITH m.Approval  ,;
                    Season    WITH m.Season    ,;
                    Cod_Flag  WITH m.Cod_Flag  ,;
                    lCompUps  WITH .T.         ,;
                    dAdd_Date WITH gdSysDate   ,;
                    cAdd_Time WITH TIME()      ,;
                    cAdd_User WITH gcUser_id   ,;      
                    CustPo    WITH m.CustPo          
             REPLACE nHstRate   WITH IIF(laSetups[9,2] <> 'Y' OR !llIsCanada,0,laSetups[26,2])
          ELSE  && if the CustPo Changed then blank the CustPo (MultiPo).
            REPLACE CustPo    WITH IIF(m.CustPo <> CustPo,'',m.CustPO)          
          ENDIF
          REPLACE Appramt   WITH Appramt+m.ApprAmt ,;
                  Season    WITH IIF(Season=m.Season,Season,'*') ,;
                  FabShip   WITH FabShip + m.FabShip ,;
                  Ordered   WITH Ordered + m.Ordered ,;
                  FabShipAmt WITH FabShipAmt + m.FabShipAmt ,;
                  Picked    WITH Picked  + m.Picked  ,;
                  Discount  WITH Discount - m.FabShipAmt*m.DiscPcnt/100,;
                  DiscPcnt  WITH IIF(FabShipAmt=0,0,ABS(Discount)*100/FabShipAmt)  ,;
                  Weight    WITH Weight  + m.Weight  ,;
                  Cartons   WITH Cartons + m.Cartons ,;
                  nMerchTax WITH nMerchTax + m.nMerchTax ,;
                  Tax_Amt   WITH Tax_Amt  + m.Tax_Amt  ,;
                  Cod_Amt   WITH Cod_Amt  + m.Cod_Amt  ,;
                  TotalChg  WITH TotalChg + m.TotalChg ,;
                  nTaxDue   WITH nTaxDue  + m.nTaxDue

          SELECT (lcInvLine)
          =SEEK(m.Account+m.cMOrder+m.Store+m.PikTkt)
          SCAN REST WHILE Account+cMOrder+Store+PikTkt+STR(LineNo,6) = ;
                          m.Account+m.cMOrder+m.Store+m.PikTkt
            SCATTER MEMVAR
            SELECT (lcConsInvD)
            IF !SEEK('Y'+Account+cDivision+cCurrCode+m.cWareCode+m.Fabric + m.Color)
              APPEND BLANK
              REPLACE Consol    WITH 'Y'         ,;
                      Account   WITH m.Account   ,;
                      cCurrCode WITH m.cCurrCode ,;
                      Fabric    WITH m.Fabric    ,;
                      Color     WITH m.Color     ,;
                      cMOrder   WITH lcOrderNo   ,;
                      Dyelot    WITH m.Dyelot    ,;
                      Season    WITH m.Season    ,;
                      Scale     WITH m.Scale     ,;
                      cWareCode WITH m.cWareCode ,;
                      lTaxable  WITH m.lTaxable
              
              *N000388,1 Update the conversion factor. [Begin]
              REPLACE nSellConv WITH m.nSellConv 
              *N000388,1 Update the conversion factor. [End]
              
            ENDIF
            =RLOCK()
            *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
            *REPLACE FabBook WITH FabBook+ m.FabBook ,;
                    FabQty WITH FabQty + m.FabQty,;
                    nNetAmnt   WITH nNetAmnt + ROUND(m.FabQty * m.nSellNetPr,2) ,;
                    nGrosAmnt  WITH nGrosAmnt+ ROUND(m.FabQty * m.nSellPrice,2) ,;
                    nSellNetPr WITH IIF(FabQty=0,0,nNetAmnt/FabQty)  ,;
                    nSellPrice WITH IIF(FabQty=0,0,nGrosAmnt/FabQty) ,;
                    Disc_Pcnt  WITH IIF(nGrosAmnt=0,0,(nGrosAmnt-nNetAmnt)*100/nGrosAmnt)
            REPLACE FabBook WITH FabBook+ m.FabBook ,;
                    FabQty WITH FabQty + m.FabQty,;
                    nNetAmnt   WITH nNetAmnt + CEILING(m.FabQty * m.nSellNetPr*100)/100 ,;
                    nGrosAmnt  WITH nGrosAmnt+ CEILING(m.FabQty * m.nSellPrice*100)/100 ,;
                    nSellNetPr WITH IIF(FabQty=0,0,nNetAmnt/FabQty)  ,;
                    nSellPrice WITH IIF(FabQty=0,0,nGrosAmnt/FabQty) ,;
                    Disc_Pcnt  WITH IIF(nGrosAmnt=0,0,(nGrosAmnt-nNetAmnt)*100/nGrosAmnt)
            *E126684,1  TMI [End  ] 
            UNLOCK
          ENDSCAN
        ENDSCAN
      ENDDO
      SELECT (lcConsInvH)
      SCAN
        =SEEK(Consol+Account+cDivision+cCurrCode,lcConsInvD)
        SELECT (lcConsInvD)
        lnInvLinNo = 0
        SCAN REST WHILE Consol+Account+cDivision+cCurrCode+Fabric+Color = ;
                        &lcConsInvH..Consol+&lcConsInvH..Account+;
                        &lcConsInvH..cDivision+&lcConsInvH..cCurrCode
          SCATTER MEMVAR
          lnInvLinNo = lnInvLinNo + 1
          m.LineNo = lnInvLinNo
          INSERT INTO (lcInvLine) FROM MEMVAR
        ENDSCAN
        SELECT (lcConsInvH)
        SCATTER MEMVAR
        m.LastLine = lnInvLinNo
        SELECT (lcInvHdr)
        =SEEK('N'+m.Account+m.cDivision+m.cCurrCode)
        REPLACE REST FLAG      WITH IIF(EMPTY(m.cMOrder),'A','O') ,;
                     cTermCode WITH m.cTermCode ,;
                     Trde_Disc WITH m.Trde_Disc ,; 
                     Cod_Flag  WITH m.Cod_Flag  ,;
                     DueDate   WITH m.DueDate   ,;
                     ShipDate  WITH m.ShipDate  ;
        WHILE Consol+Account+cDivision+cCurrCode='N'+m.Account+m.cDivision+m.cCurrCode
        
        INSERT INTO (lcInvHdr) FROM MEMVAR

      ENDSCAN
      SET ORDER TO TAG (lcInvHdr) IN (lcInvHdr)
    ENDIF
    =SEEK('O'+PADR(lcProgID,10)+gcUser_id+lcSession,'UNCMSESS')

    llCUpdate  = .T.
    llGoAndChk = .T.
    SELECT (lcInvHdr)
    *C126361,1  TMI [Start] 
    *SHOW GET pbScope   DISABLE
    *C126361,1  TMI [End  ] 
    SHOW GET ibStore   DISABLE
    SHOW GET laData[5] DISABLE    
    =lfActFolder()
    =lfwOrdBrow()
    
ENDCASE


IF lnActFolder = 1
  IF laScrMode[4]
    _CUROBJ = OBJNUM(ibInvoice)
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Invoice order
*!*************************************************************
*! Calls     : ORDBROWO,ORDBROWA
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvOrder()
*!*************************************************************
FUNCTION lfvOrder
PARAMETERS lcNavOrd,llAdd
PRIVATE XOrder,XAccount,XSTORE
PRIVATE lcBrFields
lcbrfields = "CMATINV :12:H='Release Order#',"+;
             "Account :8:H='Account',"+;
             "Store :13 :H='Store',"+;
             "cmOrder :8  :H='Order#',"+;
             "Custpo:19 :H='Cust. P.O#'"

*C126361,4  TMI [Start] 
IF EMPTY(lcNavOrd)
  *C126361,4  TMI [End  ] 

  laData[1] = lcRelOrd
  
  SET ORDER TO MRELHDR IN MRELHDR
  IF llBrowse .OR. (!EMPTY(laData[1]) .AND. !SEEK(laData[1],'MRELHDR'))
    XAccount = laData[4]
    XORDER   = laData[1]
    XSTORE   = laData[5]
    llBrowse = .T.
    lcRelOrd = IIF(!EMPTY(laData[4]),'MRELHDRA','MRELHDR')
    lcKey = IIF(!EMPTY(laData[4]),laData[4],'') + IIF(!EMPTY(laData[5]),'FOR STORE = XSTORE','')
    SET ORDER TO &lcRelOrd IN MRELHDR
    SELECT MRELHDR
    lcRelOrd = IIF(ARIABROW([lcKey],"Material Release Orders",gnBrFSRow1, gnBrFSCol1,;
                    gnBrFSRow2, gnBrFSCol2,'','','CMATINV','laBrowArr'),;  
                    MRELHDR.CMATINV,SPACE(6))
  ENDIF
   
  llBrowse = .F.
*C126361,4  TMI [Start] 
ELSE
  =gfCPSelect()  && go to select mode first
  lcRelOrd = lcNavOrd
ENDIF  
*C126361,4  TMI [End  ] 

IF !EMPTY(lcRelOrd)

  laData[1] = MRELHDR.CMORDER  
  laData[4] = MRELHDR.Account
  laData[5] = MRELHDR.STORE
  
  =SEEK('O'+laData[1] ,'MASOHDR')
  laData[2] = MASOHDR.CUSTPO
  
  lnPstRule = IIF((SEEK('M'+laData[4],'Customer') AND !EMPTY(CTAXRULE)),EVAL(Customer.cTaxRule),1) 

  SHOW GET ibAccount DISABLE
  SHOW GET laData[4] DISABLE
  SHOW GET ibStore   ENABLE
  SHOW GET laData[5] ENABLE
  
  *IF MaSoHdr.Multi <> 'Y'
    
    *-- not multi store and no pick tickets
    IF llAdd .OR. lfGetOrder(laData[1],MaSoHdr.Store,.F.,'','')
      
      IF !llAdd
        *-- Add mode
        STORE .F. TO laScrMode
        STORE .T. TO laScrMode[2]
      ENDIF
      GO TOP IN (lcInvHdr)
      
      *C126361,1  TMI [Start] if the mat. sales order has a release order then update the 
      *-                      lcInvLine and lcInvHdr temp files from there
      
      lcSvOrd = ORDER('MRELLINE')
      SELECT MRELLINE
      SET ORDER TO MRELLINE
      lnSvRec = RECNO() 
      LOCATE
      
      IF SEEK(lcRelOrd,'MRELLINE')
      
        SELECT &lcInvHdr
        LOCATE
        REPLACE CMATINV WITH MRELLINE.CMATINV  
        
        SELECT &lcInvLine
        LOCATE
        REPLACE CMATINV WITH MRELLINE.CMATINV  ALL
        
        llPost = .T.  && Allow to post
        SHOW GET pbScope ENABLE
        
      ENDIF
      
      SELECT MRELLINE
      SET ORDER TO &lcSvOrd
      IF BETWEEN(lnSvRec,1,RECCOUNT())
        GOTO (lnSvRec)
      ENDIF
      *C126361,1  TMI [End  ] 
      
      SHOW GETS
      
      =lfvStore('',llAdd)
      
      *C126361,4  TMI [Start] 
      llCUpdate  = .T.
      llGoAndChk = .T.
      SELECT (lcInvHdr)
      SHOW GET ibStore   DISABLE
      SHOW GET laData[5] DISABLE    
      =lfActFolder()  
      =lfwOrdBrow()
      *C126361,4  TMI [End  ] 

    ELSE
      *-- clear the screen 
      =lfInitSel()
    ENDIF
  *ENDIF
ENDIF

*-- end of lfvOrder.

*:**************************************************************************
*:* Name        : lfInitSel
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/14/2005
*:* Purpose     : lfInitSel
*:***************************************************************************
FUNCTION lfInitSel
STORE SPACE(6)  TO laData[1],laData[3]
STORE SPACE(15) TO laData[2]
STORE SPACE(5)  TO laData[4]
STORE SPACE(8)  TO laData[5]
SHOW GET ibOrder   ENABLE
SHOW GET laData[1] ENABLE
SHOW GET laData[2] ENABLE

*N000388,1 Commented out. [Begin]
*SHOW GET ibPikTkt  ENABLE
*SHOW GET laData[3] ENABLE
*N000388,1 Commented out. [End]

SHOW GET ibAccount ENABLE
SHOW GET laData[4] ENABLE
SHOW GET ibStore   DISABLE
SHOW GET laData[5] DISABLE
_CUROBJ = OBJNUM(laData[1])


*!*************************************************************
*! Name      : lfvAccount
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Invoice Account
*!*************************************************************
*! Calls     : CUSBROWM,gfGetAdr,gfChkRate,gfModalGen,gfActWind,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvAccount()
*!*************************************************************
FUNCTION lfvAccount
PRIVATE xAccount

IF llBrowse .OR. (!EMPTY(laData[4]) .AND. !SEEK('M'+laData[4],'CUSTOMER')) 
  xAccount = laData[4]
  SELECT CUSTOMER
  DO CUSBROWM WITH xAccount
  laData[4] = xAccount
ENDIF
llBrowse = .F.
IF EMPTY(laData[4])
  STORE SPACE(8) TO laData[5]
  SHOW GET ibStore DISABLE
  SHOW GET laData[5] DISABLE
ELSE
  lnPstRule = Customer.cTaxRule
  SHOW GET ibStore ENABLE
  SHOW GET laData[5] ENABLE
ENDIF

*!*************************************************************
*! Name      : lfvCustPo
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Customer PO#
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvCustPo()
*!*************************************************************
FUNCTION lfvCustPo

*!*************************************************************
*! Name      : lfvStore
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Invoice store
*!*************************************************************
*! Calls     : CUSBROW,gfGetAdr,gfChkRate,gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvStore()
*!*************************************************************
FUNCTION lfvStore
PARAMETERS lcDummy,llAdd
PRIVATE xStore,xAccount

IF !llBrowse AND MDOWN()
  RETURN
ENDIF

IF llBrowse .OR. !EMPTY(laData[1]) .OR. ;
   (!EMPTY(laData[5]) .AND. !SEEK('S'+laData[4]+laData[5],'CUSTOMER'))

  IF EMPTY(laData[1])   
    xStore = laData[5]
    SELECT CUSTOMER
    =CUSBROWS(laData[4],.T.)
    laData[5] = xStore
  ELSE
    SET ORDER TO TAG MaSoLinSt IN MaSoLin
    IF llBrowse OR (!EMPTY(laData[5]) .AND. !SEEK('O'+laData[1]+laData[5],'MaSoLin'))
      =gfOpenFile(gcSysHome+'SycInt',gcSysHome+'Ccontcode','SH')
      =SEEK(gcContCode,'SycInt')
      lcBrFields = "STORE :h='Store',stName:23:h='Name',"+;
                   "cAddress1 :H='ST '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
                   "cAddress2 :H='ST '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
                   "cAddress3 :H='ST '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
                   "cAddress4 :H='ST '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
                   "cAddress5 :H='ST '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
                   "cAddress12 :H='BT '+SycInt.cPart1Lab :R :P=REPLICATE('X',SycInt.nPart1Len),"+;
                   "cAddress22 :H='BT '+SycInt.cPart2Lab :R :P=REPLICATE('X',SycInt.nPart2Len),"+;
                   "cAddress32 :H='BT '+SycInt.cPart3Lab :R :P=REPLICATE('X',SycInt.nPart3Len),"+;
                   "cAddress42 :H='BT '+SycInt.cPart4Lab :R :P=REPLICATE('X',SycInt.nPart4Len),"+;
                   "cAddress52 :H='BT '+SycInt.cPart5Lab :R :P=REPLICATE('X',SycInt.nPart5Len),"+;
                   "Phone1 :P= GFPHONETEM() :H='Phone #...',Buyer :H='Buyer',salesrep :H='Rep'"
      SELECT CUSTOMER
      SET RELATION TO 'O'+laData[1]+Store INTO MaSoLin
      laData[5]=IIF(ARIABROW("'S'+laData[4] FOR !EOF('MaSoLin')",;
                "Locations for Customer : "+laData[4],gnBrFSRow1, gnBrFSCol1,;
                 gnBrFSRow2, gnBrFSCol2,'','','STORE',"laBrowArr",.F.),Customer.Store,SPACE(8))
      SET RELATION OFF INTO MaSoLin
      =gfCloseFile('SycInt')
    ENDIF
    IF lfGetOrder(laData[1],laData[5],.F.,'','')
      
      IF !llAdd
        STORE .F. TO laScrMode
        STORE .T. TO laScrMode[2]
      ENDIF
      
      GO TOP IN (lcInvHdr)
      

      *:***************************************************************************
      
      *C126361,1  TMI [Start] if the mat. sales order has a release order then update the 
      *-                      lcInvLine and lcInvHdr temp files from there

      lcSvOrd = ORDER('MRELLINE')
      SELECT MRELLINE
      SET ORDER TO MRELLINE
      lnSvRec = RECNO() 
      LOCATE
      
      IF SEEK(lcRelOrd,'MRELLINE')
        SELECT &lcInvHdr
        LOCATE
        REPLACE CMATINV WITH MRELLINE.CMATINV  
        
        SELECT &lcInvLine
        LOCATE
        REPLACE CMATINV WITH MRELLINE.CMATINV  ALL
        
        llPost = .T.  && Allow to post
        SHOW GET pbScope ENABLE
        SHOW GET pbDlt   ENABLE
      ENDIF
      
      SELECT MRELLINE
      SET ORDER TO &lcSvOrd
      IF BETWEEN(lnSvRec,1,RECCOUNT())
        GOTO (lnSvRec)
      ENDIF
      *C126361,1  TMI [End  ] 
      
      *:***************************************************************************
      
         
      SHOW GETS
    ELSE
      =lfInitSel()
    ENDIF
  ENDIF
  llBrowse = .F.
ENDIF  

*!*************************************************************
*! Name      : lfClearKey
*! Developer : WAM
*! Date      : 07/01/1996
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

*126361,3 TMI
*-* ON KEY LABEL CTRL+Q  
*-* ON KEY LABEL CTRL+W
*-* ON KEY LABEL CTRL+HOME
*-* ON KEY LABEL CTRL+END
*-* ON KEY LABEL TAB
*-* ON KEY LABEL BACKTAB
*-* ON KEY LABEL ALT+S 
*-* ON KEY LABEL ALT+B
*-* ON KEY LABEL ALT+A 
*-* ON KEY LABEL ALT+N 
*-* ON KEY LABEL ALT+V 
*-* ON KEY LABEL ALT+H 

ON KEY LABEL CTRL+Q    llDummy = .T.
ON KEY LABEL CTRL+W    llDummy = .T.
ON KEY LABEL CTRL+HOME llDummy = .T.
ON KEY LABEL CTRL+END  llDummy = .T.
ON KEY LABEL TAB       
ON KEY LABEL BACKTAB   
ON KEY LABEL ALT+S     llDummy = .T.
ON KEY LABEL ALT+B     
ON KEY LABEL ALT+A     llDummy = .T.
ON KEY LABEL ALT+N     llDummy = .T.
ON KEY LABEL ALT+V     llDummy = .T.
ON KEY LABEL ALT+H     llDummy = .T.
ON KEY LABEL ESC       llDummy = .T.
*126361,3
*!*************************************************************
*! Name      : lfReadAct
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Screen Activate function
*!*************************************************************
*! Calls     : lfClearKey,gfStopBrow
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

*!*************************************************************
*! Name      : lfDARIINV
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Screen Deactivate function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDARIINV()
*!*************************************************************
FUNCTION lfDARIINV

IF !laScrMode[1] AND WLAST()=(lcWinCh44) AND !lfChkTotQty()
  ACTIVATE WINDOW (lcWinCh44)
  _CUROBJ = OBJNUM(m.FabQty)
  RETURN .F.
ENDIF
IF WONTOP()=lcOrdBrow .OR. WONTOP()=lcInvBrow .OR. WONTOP()=lcBrTtlZ
  IF WONTOP()=lcOrdBrow 
    ON KEY LABEL ALT+S DO lpAction WITH 'S'
    ON KEY LABEL ALT+A DO lpAction WITH 'A'
    ON KEY LABEL ALT+N DO lpAction WITH 'N'
    ON KEY LABEL ALT+V DO lpAction WITH 'V'
    ON KEY LABEL ALT+H DO lpAction WITH 'H'
  ENDIF
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  ON KEY LABEL TAB DO lpTab WITH ;
  IIF(WONTOP()=lcOrdBrow,lcWinCh22,IIF(WONTOP()=lcInvBrow,lcWinCh42,'gwcContrl1')),;
  IIF(WONTOP()=lcOrdBrow,'pbSelect',IIF(WONTOP()=lcInvBrow,'m.Fabric','pbTop'))
  ON KEY LABEL BACKTAB DO lpBackTab WITH lcFolder,IIF(WONTOP()=lcOrdBrow,'ibFolder[1]','ibFolder[3]')
ELSE
  glFromBrow = .F.
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lpAction
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Hot keys for order browse
*!*************************************************************
*! Calls     : lfvShipInv,lfAdjSButt
*!*************************************************************
*! Parameters: lcAction  : 'S' : Select/UnSelect
*!                         'A' : Select All
*!                         'N' : Select None
*!                         'V' : Invert
*!                         'H' : Ship/UnShip invoice
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpAction WITH 'S'
*!*************************************************************
PROCEDURE lpAction
PARAMETERS lcAction

IF lcAction = 'H'
  =IIF(cSelect=' ',.T.,lfvShipInv())
ELSE
  =lfAdjSButt(lcAction)
ENDIF

*!*************************************************************
*! Name      : lpTab
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTab WITH 'ARDINV0', 'pbClose'
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName

ON KEY LABEL TAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lpBackTab
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpBackTab WITH 'ARDINV0', 'pbClose'
*!*************************************************************
PROCEDURE lpBackTab
PARAMETERS lcWindName, lcObjName

ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lfAdjSButt
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Select, Unselect, Select all, Select None and Invert invoices
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcType  : 'S' : Select/UnSelect
*!                       'A' : Select All
*!                       'N' : Select None
*!                       'V' : Invert
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfAdjSButt()
*!*************************************************************
FUNCTION lfAdjSButt
PARAMETERS lcType
PRIVATE lcKey,lcDataSel,lcDataUSel
SELECT (lcInvHdr)

lcKey = Account+cMOrder+Store+PikTkt

DO CASE 

  CASE lcType = 'S'   &&  Select/UnSelect
    REPLACE cSelect WITH IIF(cSelect=SPACE(1),"»",SPACE(1))
    IF IIF(EMPTY(cSelect),FabShip>0,FabShip=0)
      SCATTER FIELDS &lcScFields TO laData
      SCATTER MEMVAR FIELDS &lcFlFields
      llUpCnsLine = Consol <> 'Y' and Flag $ 'OA'
      =lfvShipInv(.T.)
      =lfwOrdBrow()
    ENDIF
  CASE INLIST(lcType,'A','N','V')
    SCAN FOR cSelect = IIF(lcType='A',' ',IIF(lcType='N',"»",''))
      REPLACE cSelect WITH IIF(lcType='A',"»",IIF(lcType='N',"",;
                           IIF(EMPTY(cSelect),"»",'')))
      IF Consol <> 'Y'
        SCATTER FIELDS &lcScFields TO laData
        SCATTER MEMVAR FIELDS &lcFlFields
        llUpCnsLine = Flag $ 'OA'
        =lfvShipInv(.F.,lcType)
      ENDIF
    ENDSCAN
  OTHERWISE
ENDCASE

SET ORDER TO TAG 'SELECT'
=SEEK("»")
LOCATE REST WHILE cSelect="»" FOR Consol <> 'Y'
lcDataSel  = IIF(FOUND(),'ENABLE','DISABLE')
=SEEK(' ')
LOCATE REST WHILE cSelect=" " FOR Consol <> 'Y'
lcDataUSel = IIF(FOUND(),'ENABLE','DISABLE')
SET ORDER TO TAG (lcInvHdr)

=SEEK(lcKey)
SHOW GET pbSelAll  &lcDataUSel
SHOW GET pbSelNone &lcDataSel
IF cSelect = SPACE(1)
  SHOW GET pbSelect,1 PROMPT '\<Select'
  SHOW GET ibFolder[2] DISABLE
  SHOW GET ibFolder[3] DISABLE
  SHOW GET ibFolder[4] DISABLE
  SHOW GET pbShipInv   DISABLE
ELSE
  SHOW GET pbSelect,1 PROMPT 'Un\<Select'
  SHOW GET ibFolder[2] ENABLE
  SHOW GET ibFolder[3] ENABLE
  SHOW GET ibFolder[4] ENABLE
  SHOW GET pbShipInv   ENABLE
ENDIF

*!*************************************************************
*! Name      : lfSetScope
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1998
*! Purpose   : Set filter to Orders
*!*************************************************************
*! Calls     : ARMATSCP.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfSetFilter()
*!*************************************************************
FUNCTION lfSetScope

PRIVATE lcFromOrd,lcToOrd,lcPriority,lnSeason,lnDivision,;
        ldFromStart,ldToStart,ldFromComp,ldToComp,;
        cbFabrics,cbColors,cbCustomers,rbStatus,llColors,;
        laFabSourc,laClrSourc,laCusSourc,laFabTarge,laClrTarge,laCusTarge

DIMENSION laScope[2,10],laFabSourc[1],laClrSourc[1],laCusSourc[1],laFabTarge[1],;
          laClrTarge[1],laCusTarge[1],laItemSeg[1],laSeasons[1,2],laDivision[1,2]
STORE ''  TO laScope,laFabSourc,laClrSourc,laCusSourc,laFabTarge,laClrTarge,;
             laCusTarge,laSeasons,laDivision

STORE ' ' TO lcFromOrd,lcToOrd,lcFromPick,lcToPick
STORE '   ' TO lcPriority  &&  Priority Field in the scope screen
STORE 1   TO lnSeason,lnDivision
STORE {}  TO ldFromStart,ldToStart,ldFromComp,ldToComp,ldFromPick,ldToPick
STORE .F. TO cbFabrics,cbColors,cbCustomers,llColors
STORE IIF(laSetups[22,2]='Y',3,1) TO rbStatus
STORE .F. TO llColors
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    llColors = .T.
    EXIT
  ENDIF
ENDFOR
laScope[1,1] = 'SEASON'
laScope[1,2] = 'laSeasons'
laScope[1,3] = 'lnSeason'
laScope[1,5] = .T.
laScope[1,6] = .F.

laScope[2,1] = 'CDIVISION'
laScope[2,2] = 'laDivision'
laScope[2,3] = 'lnDivision'
laScope[2,5] = .T.
laScope[2,6] = .F.
=gfwCodePop(@laScope,'SEASON','A')
=gfwCodePop(@laScope,'CDIVISION','A')

DO (gcScrDir+gcWinAppl+"\ARMATSCP.SPX")
GO TOP IN (lcInvHdr)
IF EOF(lcInvHdr)
  *-- Message : 40128
  *-- No orders match the selected scope.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('TRM40128B00000','ALERT')
ELSE
  STORE .F. TO laScrMode
  STORE .T. TO laScrMode[4]
  SHOW GETS
ENDIF

*!*************************************************************
*! Name      : lfvOrdScp
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Invoice order
*!*************************************************************
*! Calls     : ORDBROWO,ORDBROWA
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvOrdScp()
*!*************************************************************
FUNCTION lfvOrdScp
PARAMETERS lcOrderNo
PRIVATE XOrder,XAccount,XSTORE

IF llBrowse .OR. (!EMPTY(&lcOrderNo) .AND. !SEEK('O'+&lcOrderNo,'MaSoHdr'))
  XORDER     = &lcOrderNo
  llBrowse   = .T.
  &lcOrderNo = IIF(MaOrdBrowO(@XORDER,.T.,'O'),XORDER,'')
ENDIF
llBrowse = .F.

*!**************************************************************************
*! Name      : lfFabClrBr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/16/2002
*! Purpose   : Validation that both Fabric + Color are not empty.
*!**************************************************************************
*! Example   : =lfFabClrBr()
*!**************************************************************************
*N000388,1 
FUNCTION lfFabClrBr
IF lfNavigate()
  _CUROBJ = _CUROBJ
  RETURN
ENDIF  

IF MDOWN() AND !llBrowse
  RETURN
ENDIF

IF SYS(18) = "FABRIC"
  IF EMPTY(m.Fabric)
    _CUROBJ = OBJNUM(m.Fabric)
    IF llBrowse
      DO FABROW WITH m.Fabric,"*"
      m.Fabric = IIF(EMPTY(m.Fabric),IIF(llAddLine,SPACE(7),&lcInvLine..Fabric),m.Fabric)  
    ENDIF
    IF !EMPTY(m.Fabric)
      =lfObsColor()
    ENDIF
    llBrowse = .F.         
  ELSE
    IF llBrowse OR ((!llAddLine OR m.Fabric <> SPACE(7)) AND !SEEK(m.Fabric,'Fabric'))
      DO FABROW WITH m.Fabric,"*"
      m.Fabric = IIF(EMPTY(m.Fabric),IIF(llAddLine,SPACE(7),&lcInvLine..Fabric),m.Fabric)  
    ENDIF
    IF !EMPTY(m.Fabric)
      =lfObsColor()
    ENDIF
    llBrowse = .F.         
  ENDIF
ELSE   && Color
  IF llBrowse OR ((!llAddLine OR m.Color <> SPACE(6)) AND !SEEK(m.Fabric+m.Color,'Fabric'))
    IF SEEK(m.Fabric,'Fabric')
      m.Color = CHR(240)
      DO FABROW WITH m.Fabric,m.Color      
    ELSE 
      *N000388,1 write message
      m.Color = ''
      _CUROBJ = OBJNUM(m.Fabric)
      RETURN
    ENDIF  
    m.Color = IIF(EMPTY(m.Color),IIF(llAddLine,SPACE(6),&lcInvLine..Color),m.Color)  
  ENDIF
  llBrowse = .F.         
ENDIF
IF SEEK(m.Fabric+m.Color,'Fabric')
  DO lpCheckFab
ENDIF
*-- End of lfFabClrBr.

*!**************************************************************************
*! Name      : lpCheckFab
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/16/2002
*! Purpose   : Check for some rules to be applied against Fabric+Color.
*!**************************************************************************
*! Example   : DO lpCheckFab
*!**************************************************************************
*N000388,1
PROCEDURE lpCheckFab
PRIVATE llAddFabrc

llAddFabrc = .T.
IF (llAddLine AND (m.Fabric = SPACE(7) OR m.Color = SPACE(6))) OR ;
   (!llAddLine AND m.Fabric = &lcInvLine..Fabric AND m.Color = &lcInvLine..Color)
  llAddFabrc = .F.
ENDIF

SELECT (lcInvLine) 
SET RELATION OFF INTO Fabric
SET RELATION OFF INTO FabDye

llAddFabrc = llAddFabrc AND SEEK(m.Fabric+m.Color,'Fabric')

*N000388,1 Add validation for enabling PbRolls button. [Begin]
DO lpChkRolBt
*N000388,1 Add validation for enabling PbRolls button. [End]

IF llAddFabrc
  *-- Message : 40085
  *-- No dyelots found for Fabric xxxx in warehouse xxxxx
  *-- Button : 00000
  *-- Ok
  IF (laSetups[8,2]='Y' AND Fabric.cDye_Flg='Y' AND !lfFabDye() AND ;
        gfModalGen('TRM40085B00000','ALERT','Fabric-Color'+': ' + m.Fabric + "-" + m.Color + IIF(laSetups[5,2]='Y',' in warehouse '+laData[7],''))=1)
    llAddFabrc = .F.
  ENDIF
ENDIF
IF llAddFabrc
  m.nSellPrice  = IIF(SEEK(m.Fabric+m.Color,'Fabric'),Fabric.nSellPrice,0)

  *-- get the cDiscCode From Fabdye in every case
  STORE 0 TO m.Disc_Pcnt
  m.nSellNetPr = m.nSellPrice*(100-m.Disc_Pcnt)/100  
  llAddFabrc = m.nSellPrice >= 0
ENDIF
IF llAddFabrc
  IF laSetups[5,2]='Y' AND !SEEK(m.Fabric+m.Color+laData[7]+SPACE(10),'FabDye')
    *-- Message : 40012
    *-- Fabric xxx is not assigned to warehouse xxx
    *-- Button : 40002
    *-- Add Reenter
    IF gfModalGen('QRM40012B40002','ALERT',m.Fabric+'|'+TRIM(laData[7]))=1
      DO gpAdFabWar WITH m.Fabric,m.Color,SPACE(10),laData[7]
    ELSE
      llAddFabrc = .F.
    ENDIF
  ENDIF
ENDIF

*N000388,1 Check for Fabric+Color. [Begin]
PRIVATE lcInvLnTag
lcInvLnTag = ORDER()
SET ORDER TO TAG 'FabColor' IN (lcInvLine)
IF SEEK(laData[4]+laData[1]+laData[5]+laData[3]+m.Fabric+m.Color+IIF(Fabric.cDye_Flg='Y',m.DyeLot,'')) AND llAddLine
  *-- Message : 40013
  *-- Warning! This Fabric/Color had been entered on this invoice
  *-- Button : 00000
  *-- Ok
  =gfModalGen('INM42107B00000','ALERT','Fabric/Color'+'|'+'Invoice')
  llAddFabrc = .F.
  SET ORDER TO TAG (lcInvLnTag) IN (lcInvLine)
ENDIF
SET ORDER TO TAG (lcInvLnTag) IN (lcInvLine)
*N000388,1 Check for Fabric+Color. [End]

SELECT (lcInvLine) 
SET RELATION TO Fabric + Color INTO Fabric ADDITIVE
SET RELATION TO Fabric + Color + cWareCode + Dyelot INTO FabDye ADDITIVE
IF llAddFabrc  
  lnTaxRate = 0
  SET ORDER TO TAG FabColor

  *N000388,1 SSE Commented out , move these lines of codes upward. [Begin]
  *IF SEEK(laData[4]+laData[1]+laData[5]+laData[3]+m.Fabric+m.Color+IIF(Fabric.cDye_Flg='Y',m.DyeLot,''))
  *  *-- Message : 40013
  *  *-- Warning! This Fabric/Color had been entered on this invoice
  *  *-- Button : 00000
  *  *-- Ok
  *  =gfModalGen('INM42107B00000','ALERT','Fabric/Color'+'|'+'Invoice')
  *ENDIF
  *N000388,1 SSE Commented out , move these lines of codes upward. [End]

  SET ORDER TO TAG (lcInvLine)
  IF llAddLine
    m.cmOrder = laData[1]
    *m.PikTkt  = laData[3]
    m.Account = laData[4]
    m.Store   = laData[5]
    m.InvDate = laData[6]
    *m.cDivision= laData[13]
    m.cCurrCode=laData[14]
    m.cWarecode= laData[7]
    *C126361,4  TMI [Start] 
    m.CMATINV = lcRelOrd
    *C126361,4  TMI [End  ] 
    m.lNewLine=.T.
    m.Consol=&lcInvHdr..Consol
    SELECT (lcInvHdr)
    =RLOCK()
    REPLACE LastLine WITH LastLine + 1
    UNLOCK
    SELECT (lcInvLine)
    m.LineNo  = &lcInvHdr..LastLine
    INSERT INTO (lcInvLine) FROM MEMVAR
    *C126361,4  TMI [Start] 
    *=SEEK(laData[4]+laData[1]+laData[5]+laData[3]+STR(m.LineNo,6))
    *C126361,4  TMI [End  ] 
  ELSE
    =SEEK(laData[4]+laData[1]+laData[5]+laData[3]+STR(m.LineNo,6))  
    REPLACE AltFabric WITH IIF(EMPTY(AltFabric),Fabric,AltFabric) ,;
            AltColor WITH IIF(EMPTY(AltColor),Color,AltColor) ,; 
            Fabric    WITH m.Fabric  ,;
            Color     WITH m.Color
  ENDIF
  lnTaxRate =0
  
  *N000388,1 Commented out. [Begin]
  *IF llIsEngland .AND. !Customer.lVatExem  .AND. Style.nTaxBreak <> 0
  *  =gfRltFld(Style.cTaxCode,@laEngStyTax,'CTAXCODE')  
  *ENDIF
  *N000388,1 Commented out. [End]
  
  
  *B607226,1 ABD - get the tax code and tax % from fabric file. [Begin]
  IF llIsEngland .AND. !Customer.lVatExem
    =gfRltFld(Fabric.cTaxCode,@laEngStyTax,'CTAXCODE')
    m.ntaxrate = lnTaxRate
  ENDIF
  *B607226,1 ABD - [End]
  
  m.Desc1  = Fabric.Desc
  m.nSellconv = Fabric.nSellconv
  m.cSellUOM = Fabric.cSellUOM
  *m.Comm1  = IIF(Style.commission,laData[9],0)
  *m.Comm2  = IIF(Style.commission,laData[10],0)
  *m.lTaxable = STYLE.lTAXABLE
  m.Comm1  = 0
  m.Comm2  = 0
  m.lTaxable = .T.
  REPLACE Desc1     WITH m.Desc1 ,;
          nTaxRate  WITH lnTaxRate ,;
          Comm1     WITH m.Comm1 ,;
          Comm2     WITH m.Comm2 ,;
          lTaxable  WITH m.lTaxable ,;
          nSellConv WITH m.nSellConv ,;
          cSellUOM  WITH m.cSellUOM
  STORE .T. TO llCUpdate
  =IIF(llUpCnsLine,lfUpCnsLine(),.T.)
ELSE
  SCATTER MEMVAR MEMO
ENDIF
STORE .F. TO llAddLine

*N000388,1 Always force not to be end  of file. [Begin]
IF EOF(lcInvLine)
  GO TOP IN (lcInvLine)
ENDIF  
*N000388,1 Always force not to be end  of file. [End]

SET KEY TO laData[4]+laData[1]+laData[5]+laData[3]
SHOW WINDOW (lcInvBrow) REFRESH SAME
SHOW GETS WINDOW (lcWinCh42) ENABLE ONLY
SHOW GETS WINDOW (lcWinCh44) ENABLE ONLY

*N000388,1 Disable Sell Conversion. [Begin]
DO lpChkRolBt
SHOW GET m.nSellConv DISABLE
*N000388,1 Disable Sell Conversion. [End]

SHOW GETS WINDOW (lcWinCh45) ENABLE ONLY
=lfRefresh(lcWinCh42)
=lfRefresh(lcWinCh44)
=lfRefresh(lcWinCh46)
=lfvNPrice()
*-- End of lpCheckFab.

*!*************************************************************
*! Name      : lfvFabrics
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1998
*! Purpose   : Set styles scope
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvFabrics()
*!*************************************************************
FUNCTION lfvFabrics
PRIVATE lnAlias

IF EMPTY(laFabSourc)
  lnAlias = SELECT()
  =gfOpenFile(gcDataDir+'Fabric',gcDataDir+'Fabric','SH') 
  
  SELECT Fabric
  SET ORDER TO TAG cFabric
  SCAN
    laFabSourc[ALEN(laFabSourc)] = Fabric
    DIMENSION laFabSourc[ALEN(laFabSourc)+1]
  ENDSCAN
  DIMENSION laFabSourc[ALEN(laFabSourc)-1]
  SET ORDER TO TAG Fabric
  SELECT (lnAlias)
ENDIF  
=gfMover(@laFabSourc,@laFabTarge,"Only these Fabrics : ",.T.)
cbFabrics = !EMPTY(laFabTarge)
SHOW GET cbFabrics ENABLE
RETURN

*!*************************************************************
*! Name      : lfvColors
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1998
*! Purpose   : Set Colors scope
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvColors()
*!*************************************************************
FUNCTION lfvColors
IF EMPTY(laClrSourc)
  SELECT cCode_No+SPACE(1)+cDiscrep FROM CODES ;
  WHERE cDefCode+cRltField+cFld_Name = 'N'+'N'+'COLOR' ;
  INTO ARRAY laClrSourc
ENDIF  
=gfMover(@laClrSourc,@laClrTarge,"Only these Colors : ",.T.)
cbColors = !EMPTY(laClrTarge)
SHOW GET cbColors ENABLE
RETURN

*!*************************************************************
*! Name      : lfvCustomers
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1998
*! Purpose   : Set Customers scope
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvCustomers()
*!*************************************************************
FUNCTION lfvCustomers

IF EMPTY(laCusSourc)
  SELECT Account+SPACE(1)+btName FROM Customer INTO ARRAY laCusSourc ;
  WHERE Type+Account+Store = 'M'
ENDIF  
=gfMover(@laCusSourc,@laCusTarge,"Only these Customers : ",.T.)
cbCustomers = !EMPTY(laCusTarge)
SHOW GET cbCustomers ENABLE
RETURN

*!*************************************************************
*! Name      : lfvInvScp
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1998
*! Purpose   : Validate invoices scope
*!*************************************************************
*! Calls     : lfGetOrder,gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvInvScp()
*!*************************************************************
FUNCTION lfvInvScp
PRIVATE lcCondition,lcStatus,lcExp,lnScpCount
PRIVATE lcPkTCond

*-- Message : 40106
*-- Order number range error!
*-- Button : 00000
*-- Ok

*-- Message : 40107
*-- Order start date range error!
*-- Button : 00000
*-- Ok

*-- Message : 40108
*-- Order completion date range error!
*-- Button : 00000
*-- Ok

IF !EMPTY(lcFromOrd) .AND. !EMPTY(lcToOrd) .AND. lcToOrd < lcFromOrd ;
  .AND. gfModalGen('TRM40106B00000','ALERT')=1
  _CUROBJ = OBJNUM(lcFromOrd)
  RETURN
ENDIF  
IF !EMPTY(ldFromStart) .AND. !EMPTY(ldToStart) .AND. ldToStart < ldFromStart ;
  .AND. gfModalGen('TRM40107B00000','ALERT')=1
  _CUROBJ = OBJNUM(ldFromStart)
  RETURN
ENDIF  
IF !EMPTY(ldFromComp) .AND. !EMPTY(ldToComp) .AND. ldToComp < ldFromComp ;
  .AND. gfModalGen('TRM40108B00000','ALERT')=1
  _CUROBJ = OBJNUM(ldFromComp)
  RETURN
ENDIF  
CLEAR READ
lcCondition='.T.'

*-- add season,division,Priority to the filter
*lcCondition=lcCondition+IIF(laSeasons[lnSeason,2]='*','',' AND Season=laSeasons[lnSeason,2]')
*lcCondition=lcCondition+IIF(laDivision[lnDivision,2]='*','',' AND cDivision=laDivision[lnDivision,2]')
lcCondition=lcCondition+IIF(EMPTY(lcPriority),'',' AND Priority=lcPriority')

DO CASE
  *-- Date Filter
  CASE EMPTY(ldFromStart)  .AND. !EMPTY(ldToStart)
    lcCondition=lcCondition+' AND Start<=ldToStart'
  CASE !EMPTY(ldFromStart) .AND. EMPTY(ldToStart)
    lcCondition=lcCondition+' AND Start>=ldFromStart'
  CASE !EMPTY(ldFromStart) .AND. !EMPTY(ldToStart)
    lcCondition=lcCondition+' AND BETWEEN(Start,ldFromStart,ldToStart)'
ENDCASE  
DO CASE
  *-- Complitation Date Filter
  CASE EMPTY(ldFromComp)  .AND. !EMPTY(ldToComp)
    lcCondition=lcCondition+' AND Complete<=ldToComp'
  CASE !EMPTY(ldFromComp) .AND. EMPTY(ldToComp)
    lcCondition=lcCondition+' AND Complete>=ldFromComp'
  CASE !EMPTY(ldFromComp) .AND. !EMPTY(ldToComp)
    lcCondition=lcCondition+' AND BETWEEN(Complete,ldFromComp,ldToComp)'
ENDCASE  
IF llOpnFiles
  =gfOpenFile(gcDataDir+'Fabric',gcDataDir+'Fabric','SH') 
  =gfOpenFile(gcDataDir+'FabDye',gcDataDir+'FabDye','SH')
ENDIF 
USE gcDataDir+'Customer' IN 0 AGAIN ALIAS CUST ORDER Customer
SELECT (lcInvLine)
SET RELATION TO Fabric + Color INTO Fabric
SET RELATION TO Fabric + Color +cWareCode + Dyelot INTO FabDye ADDITIVE

SELECT MaSoHdr

DO CASE
  CASE !EMPTY(laCusTarge)
    SET ORDER TO TAG OrdAcct
    lcCondition=lcCondition+' AND '+;
                IIF(rbStatus=1,"STATUS='O'",IIF(rbStatus=2,"STATUS='H'",;
                    "INLIST(STATUS,'O','H')" ))
    FOR lnScpCount = 1 TO ALEN(laCusTarge)
      lcExp = SUBSTR(laCusTarge[lnScpCount],1,5) + 'O'
      =SEEK(lcExp)
      SCAN REST FOR EVAL(lcCondition) WHILE Account+cOrdType+cMOrder=lcExp
        =lfGetOrdPkT()
      ENDSCAN
    ENDFOR
  CASE !EMPTY(lcFromOrd)
    lcCondition=lcCondition+' AND '+;
                IIF(rbStatus=1,"STATUS='O'",IIF(rbStatus=2,"STATUS='H'",;
                    "INLIST(STATUS,'O','H')" ))
                    
    SET ORDER TO TAG MaSoHdr
    =SEEK('O'+lcFromOrd)
    SCAN REST FOR   EVAL(lcCondition);
              WHILE cOrdType+cMOrder='O' AND IIF(EMPTY(lcToOrd),.T.,cMOrder<=lcToOrd)
      =lfGetOrdPkT()
    ENDSCAN
  OTHERWISE
    SET ORDER TO TAG OrdAcct IN MaSoHdr
    SELECT CUST
    =SEEK('M')
    SCAN REST FOR SEEK(Account,'MaSoHdr') WHILE Type+Account+Store = 'M'
      lcAccount = Account
      lcExp = lcAccount + 'O'
      SELECT MaSoHdr
      =SEEK(lcExp)
      SCAN REST FOR EVAL(lcCondition);
                WHILE Account+cOrdType+cMOrder=lcExp AND IIF(EMPTY(lcToOrd),.T.,cMOrder<=lcToOrd)
        =lfGetOrdPkT()            
      ENDSCAN
    ENDSCAN
ENDCASE
SET ORDER TO TAG MaSoHdr IN MaSoHdr
IF USED ('CUST') 
  USE IN CUST
ENDIF

*!*************************************************************
*! Name      : lfGetOrder
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1998
*! Purpose   : Get order information
*!*************************************************************
*! Calls     : gfRltFld,gfChkRate,gfModalGen
*!*************************************************************
*! Parameters: lcOrderNo  : Order Number
*!             lcStoreNo  : Store
*!             llPicked   : Picked lines only
*!             lcPikFrom  : Pick Ticket lower range
*!             lcPikTo    : Pick Ticket Higher range
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfGetOrder()
*!*************************************************************
FUNCTION lfGetOrder
PARAMETERS lcOrderNo,lcStoreNo,llPicked,lcPikFrom,lcPikTo,llScope
PRIVATE ldDueDate,lcStore,lcCustLink,lcCustSales,lcSaleRep1,lcSaleRep2,;
        lcCurrCode,lnExRate,lnCurrUnit,llPacked,lcLinesCond,lnTaxQty,;
        lnTaxRate,llBackOrd


*C126361,4  TMI [Start] if lfvStore in calllist , exit
PRIVATE lnI
lnI = 1

DO WHILE !EMPTY(PROGRAM(lnI))
  IF 'LFVSTORE' $ PROGRAM(lnI) 
    RETURN
  ENDIF
  lnI = lnI +  1
ENDDO
*C126361,4  TMI [End  ] 

*N000388,1 always send llPicked parameter .F.
llPicked = .F.

PRIVATE lnAllCartn
lnAllCartn = 0

IF !llScope

  *-- if the function was not called from the scope screen
  IF llOpnFiles
    =gfOpenFile(gcDataDir+'Fabric',gcDataDir+'Fabric','SH')
    =gfOpenFile(gcDataDir+'FabDye',gcDataDir+'FabDye','SH')
  ENDIF  
  
  SELECT (lcInvLine)
  SET RELATION TO Fabric + Color INTO Fabric
  SET RELATION TO Fabric + Color + cWareCode + Dyelot INTO FabDye ADDITIVE
  
ENDIF

IF llPicked
  *-- Pick ticket filter   
  lcLinesCond = "Picked .AND. TotPik > 0"
  DO CASE
    CASE !EMPTY(lcPikFrom) AND EMPTY(lcPikTo)
      lcLinesCond = lcLinesCond + "AND PikTkt >= lcPikFrom"
    CASE EMPTY(lcPikFrom) AND !EMPTY(lcPikTo)
      lcLinesCond = lcLinesCond + "AND PikTkt <= lcPikTo"
    CASE !EMPTY(lcPikFrom) AND !EMPTY(lcPikTo)
      lcLinesCond = lcLinesCond + "AND BETWEEN(PikTkt,lcPikFrom,lcPikTo)"
  ENDCASE
  DO CASE
    CASE llScope AND !EMPTY(ldFromPick) AND EMPTY(ldToPick)
      lcLinesCond = lcLinesCond + "AND pikdate >= ldFromPick"
    CASE llScope AND EMPTY(ldFromPick) AND !EMPTY(ldToPick)
      lcLinesCond = lcLinesCond + "AND pikdate <= ldToPick"
    CASE llScope AND !EMPTY(ldFromPick) AND !EMPTY(ldToPick)
      lcLinesCond = lcLinesCond + "AND BETWEEN(pikdate,ldFromPick,ldToPick)"
  ENDCASE
ELSE
  
  *N000388,1 Commented out since we don't use piktkt uptil now. [Begin]
  *lcLinesCond= "!Picked AND TotQty >0"
  lcLinesCond= "FabQty >0"
  *N000388,1 Commented out since we don't use piktkt uptil now. [End]

ENDIF

IF llScope AND !EMPTY(laFabTarge)
  *--Style major Filter
  lcLinesCond=lcLinesCond+" AND ASCAN(laFabTarge,Fabric)>0"
ENDIF
*-- get the Term code related fields               
=gfRltFld(MaSoHdr.cTermCode,@laTRltFld,'CTERMCODE')

lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)

IF llIsEngland
  ldDueDate = IIF(lcTEOM <> 'Y',ldDefInvDate+lnTDaysDue,;
                  CTOD('01'+SUBSTR(DTOC(GOMONTH(ldDefInvDate,1)),3))-1+lnTDaysDue)
ELSE                
  ldDueDate = IIF(lcTEOM <> 'Y',ldDefInvDate+lnTDaysDue,;
                  GOMONTH(CTOD(SUBSTR(DTOC(ldDefInvDate),1,3)+'10'+;
                  SUBSTR(DTOC(ldDefInvDate),6,5)),IIF(DAY(ldDefInvDate) > lnEOMDay,2,1))+lnTDaysDue)
ENDIF
lcCurrCode= IIF(EMPTY(MaSoHdr.cCurrCode),gcBaseCurr,MaSoHdr.cCurrCode)
STORE 1 TO lnExRate, lnCurrUnit
IF lcCurrCode <> gcBaseCurr
  lnExRate=gfChkRate('lnCurrUnit',lcCurrCode,ldDefInvDate,.T.,.F.,.F.,llEditExRt)
  IF lnExRate = 0
    IF llEditExRt
      *-- Message : 00262
      *-- A valid xxx to xxx exchange rate could not be found on xxx.
      *-- Button : 00000
      *-- Ok
      =gfModalGen('INM00262B00000','ALERT',ALLTRIM(lcCurrCode)+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(ldDefInvDate))
    ELSE
      lcCurrCode = gcBaseCurr
      STORE 1 TO lnExRate, lnCurrUnit
    ENDIF
  ENDIF
ENDIF
=SEEK('M'+MaSoHdr.Account,'Customer')
IF EMPTY(MaSoHdr.Store)  
  lcPhone=Customer.Phone1
ENDIF  

llBackOrd = INLIST(IIF(EMPTY(Customer.cBackOrd),laSetups[23,2],Customer.cBackOrd),'A','I')

SELECT MaSoLin
SET ORDER TO TAG Masolinst
=SEEK('O'+lcOrderNo+ALLTRIM(lcStoreNo))

lcStore = Store
=IIF(EMPTY(Store),SEEK('M'+Account,'Customer'),SEEK('S'+Account+Store,'Customer'))

*C126361,4  TMI [Start] 
SET ORDER TO MRELHDR IN MRELHDR
SET ORDER TO MRELLINE IN MRELLINE
=SEEK(lcRelOrd,'MRELHDR')
=SEEK(lcRelOrd,'MRELLINE')
*C126361,4  TMI [End  ] 

lcSaleRep1 = MRELHDR.Rep1
lnComm1    = MRELHDR.Comm1
lcSaleRep2 = MRELHDR.Rep2
lnComm2    = MRELHDR.Comm2

SELECT MRELLINE
SCAN REST WHILE CMATINV+STR(LINENO,6) = lcRelOrd

  llPacked = .F.
    
  SCATTER MEMVAR MEMO
  m.nSellPrice = IIF(m.nSellPrice=0,m.nSellNetPr,m.nSellPrice)
  =SEEK(m.Fabric + m.Color,'Fabric')
  STORE 0 TO lnTaxQty,lnTaxRate
  *B607226,1 ABD - get the tax code and tax % from fabric file. [Begin]
  IF llIsEngland .AND. laSetups[9,2]='Y' .AND. !Customer.lVatExem
    =gfRltFld(Fabric.cTaxCode,@laEngStyTax,'CTAXCODE') 
    lnTaxQty = lnTaxQty + M.FabQty
  ENDIF
  *B607226,1 ABD - [End]
    
  *N000388,1 we have removed the PikTkt , Pik1..8 , TotPik field from lcInvLine file because it was removed from
  *          Material Order Line file where we have taken its structure to make the lcInvLine file
  m.lTaxable = .T.
  *I've added this above line in order to handle the INSERT INTO statement until we reach to a solution.
  
  IF !SEEK(m.Account+m.cMOrder+m.Store+m.PikTkt+STR(m.LineNo,6),lcInvLine)
  
    INSERT INTO (lcInvLine);
    (cMOrder,Account,LineNo,Store,Fabric,Color,Dyelot,Note_Mem,Comm1,Comm2,;
     FabBook,Flag,FabQty,;
     nSellNetPr,nSellPrice,Disc_Pcnt,lPacked,lBackOrd,nTaxRate,Group,PrePak,;
     Desc1,Season,PPQty,Scale,cWareCode,Consol,cDivision,cCurrCode,lTaxable,cDyeFlag,cwarecode,nSellConv,cSellUOM);
     VALUES (m.cMOrder,m.Account,m.LineNo,m.Store,m.Fabric,m.Color,m.Dyelot,;
     m.Note_Mem,m.Comm1,m.Comm2,;
     m.FabQty,IIF(llPicked,'B',' '),IIF(llPicked,m.FabPik,m.FabQty),;
     m.nSellNetPr,m.nSellPrice,m.Disc_Pcnt,llPacked,llBackOrd,lnTaxRate,m.Group,m.PrePak,;
     MaSoLin.Desc1,m.Season,m.PPQty,m.Scale,m.cWareCode,'N',MaSoHdr.cDivision,MaSoHdr.cCurrCode,m.lTaxable,Fabric.cDye_Flg,m.cwarecode,MaSoLin.nSellConv,MaSoLin.cSellUOM)
  
  ENDIF
  
  SELECT (lcInvLine)

  SCATTER MEMVAR FIELDS FabQty
  SELECT (lcInvHdr)

  llInvPck = .F.       && this line is instead of the next one which is commented

  IF !SEEK(m.Account + m.cMOrder + m.Store + m.PikTkt)
     
    APPEND BLANK
    REPLACE cSelect    WITH '»' ,;
            cMOrder    WITH m.cMOrder ,;
            Store      WITH m.Store ,;
            PikTkt     WITH m.PikTkt ,;
            CustPo     WITH IIF(EMPTY(m.CustPo),MaSoHdr.CustPo,m.CustPo),;
            Dist_Ctr   WITH Customer.Dist_Ctr ,;
            Account    WITH MaSoHdr.Account ,;
            cTermCode  WITH MaSoHdr.cTermCode  ,;
            SpcInst    WITH MaSoHdr.SpcInst ,;
            lUpsIns    WITH (MaSoHdr.CINSUR='Y') ,;
            Rep1       WITH lcSaleRep1 ,;
            Comm1      WITH lnComm1    ,;
            Rep2       WITH lcSaleRep2 ,;
            Comm2      WITH lnComm2    ,;
            Note1      WITH MaSoHdr.Note1 ,;
            Note2      WITH MaSoHdr.Note2 ,;
            lCompUps   WITH .T. ,;
            Consol     WITH 'N' 
    
    REPLACE DiscPcnt   WITH MaSoHdr.Disc      ,;
            InvDate    WITH ldDefInvDate     ,;
            ShipDate   WITH ldDefInvDate     ,;
            dPostDate  WITH ldDefPstDate     ,;
            DueDate    WITH ldDueDate        ,;
            Dept       WITH MaSoHdr.Dept      ,; 
            cFacCode   WITH MaSoHdr.cFacCode  ,;
            Approval   WITH MaSoHdr.Approval  ,;
            Appramt    WITH MaSoHdr.ApprAmt   ,;
            UpsZone    WITH Customer.UpsZone ,;
            Phone      WITH IIF(EMPTY(MaSoHdr.Store),lcPhone,Customer.Phone1)  ,;
            cWareCode  WITH IIF(llPicked .AND. SEEK(m.cMOrder+m.PikTkt,'PikTkt'),;
                                PikTkt.cWareCode,MaSoHdr.cWareCode) ,;
            Trde_Disc  WITH lnTerDiscR ,;
            Tax_Rate   WITH IIF(laSetups[9,2] <> 'Y',0,;
                            IIF(llIsCanada,laSetups[10,2],Customer.nTaxRate)) ,;
            nPstRate   WITH IIF(laSetups[9,2]='Y' AND llIsCanada,;
                             Customer.nTaxRate,0) ,;
            cTaxRule   WITH IIF(laSetups[9,2]='Y' AND llIsCanada,;
                            Customer.cTaxRule,'') ,;
            Cod_Flag   WITH IIF(lcTCod='Y','Y','N'),;
            Status     WITH MaSoHdr.Status ,;
            cCurrCode  WITH lcCurrCode ,;
            nExRate    WITH lnExRate   ,;
            nCurrUnit  WITH lnCurrUnit ,;
            dAdd_Date  WITH gdSysDate  ,;
            cAdd_Time  WITH TIME()     ,;
            cAdd_User  WITH gcUser_id
      
    REPLACE nHstRate   WITH IIF(laSetups[9,2] <> 'Y' OR !llIsCanada,0,laSetups[26,2])
      
  ENDIF
  
  REPLACE LastLine   WITH M.LINENO
    
  lnAllCartn = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
  REPLACE Ordered   WITH Ordered  + &lcInvLine..FabBook ,;
          FabShip   WITH FabShip     + m.FabQty ,;
          FabShipAmt WITH FabShipAmt + CEILING(m.FabQty*m.nSellNetPr*100)/100 ,;
          Discount  WITH -FabShipAmt * DiscPcnt/100,;
          nCartons  WITH IIF(llInvPck,nCartons,lnCartons) ,;
          Cartons   WITH IIF(llInvPck,Cartons,lnAllCartn) ,;
          ShipVia   WITH IIF(Customer.nBrkWeight <> 0 AND Weight > Customer.nBrkWeight,Customer.cAltShpVia,;
                         IIF(ALLTRIM(MaSoHdr.ShipVia)='*',Customer.ShipVia,MaSoHdr.ShipVia)),;
          nMerchTax WITH nMerchTax + lnTaxQty * m.nSellNetPr * lnTaxRate/100 ,;
          Tax_Amt   WITH nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100  ,;
          Cod_Amt   WITH IIF(Cod_Flag='Y' AND llIsEngland,FabShipAmt+Tax_Amt+Discount,0) ,; 
          TotalChg  WITH IIF(llIsEngland,FabShipAmt+Tax_Amt+Discount,0) ,;
          nTaxDue   WITH nTaxDue + IIF(m.lTaxable,CEILING(m.FabQty*m.nSellNetPr*100)/100,0)

ENDSCAN

*-* *C126361,3  TMI [Start] Add lines of release order from MRELLINE file that are not found in the MASOLIN
*-* SELECT &lcInvHdr
*-* GO TOP
*-* REPLACE CMATINV    WITH lcRelOrd ,;
*-*         Ordered    WITH 0 ,;
*-*         FabShip    WITH 0 ,;
*-*         FabShipAmt WITH 0 ,;
*-*         Discount   WITH 0 ,;
*-*         nCartons   WITH 0 ,;
*-*         Cartons    WITH 0 ,;
*-*         ShipVia    WITH '',;
*-*         nMerchTax  WITH 0 ,;
*-*         Tax_Amt    WITH 0 ,;
*-*         Cod_Amt    WITH 0 ,; 
*-*         TotalChg   WITH 0 ,;
*-*         nTaxDue    WITH 0
*-* 
*-* SELECT &lcInvLine
*-* *-* DELETE ALL
*-* 
*-* SELECT MRELLINE
*-* SET ORDER TO MRELLINE
*-* =SEEK(lcRelOrd,'MRELLINE')
*-* SCAN REST WHILE CMATINV+STR(LINENO,6) = lcRelOrd
*-*   SCATTER MEMVAR MEMO
*-*   INSERT INTO &lcInvLine FROM MEMVAR
*-*    
*-*   *- Update the lcInvHdr file
*-*   SELECT &lcInvHdr 
*-*   lnAllCartn = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
*-*   REPLACE Ordered   WITH Ordered  + &lcInvLine..FabBook ,;
*-*           FabShip   WITH FabShip     + m.FabQty ,;
*-*           FabShipAmt WITH FabShipAmt + CEILING(m.FabQty*m.nSellNetPr*100)/100 ,;
*-*           Discount  WITH -FabShipAmt * DiscPcnt/100,;
*-*           nCartons  WITH IIF(llInvPck,nCartons,lnCartons) ,;
*-*           Cartons   WITH IIF(llInvPck,Cartons,lnAllCartn) ,;
*-*           ShipVia   WITH IIF(Customer.nBrkWeight <> 0 AND Weight > Customer.nBrkWeight,Customer.cAltShpVia,;
*-*                          IIF(ALLTRIM(MaSoHdr.ShipVia)='*',Customer.ShipVia,MaSoHdr.ShipVia)),;
*-*           nMerchTax WITH nMerchTax + lnTaxQty * m.nSellNetPr * lnTaxRate/100 ,;
*-*           Tax_Amt   WITH nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100  ,;
*-*           Cod_Amt   WITH IIF(Cod_Flag='Y' AND llIsEngland,FabShipAmt+Tax_Amt+Discount,0) ,; 
*-*           TotalChg  WITH IIF(llIsEngland,FabShipAmt+Tax_Amt+Discount,0) ,;
*-*           nTaxDue   WITH nTaxDue + IIF(m.lTaxable,CEILING(m.FabQty*m.nSellNetPr*100)/100,0)
*-*           
*-* ENDSCAN
*-* 

GO TOP IN &lcInvHdr
GO TOP IN &lcInvLine

*C126361,3  TMI [End  ] 

IF !llScope AND EOF(lcInvHdr)
  *-- Message : 40128
  *-- No orders match the selected scope.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('TRM40128B00000','ALERT')
  RETURN(.F.)
ENDIF

RETURN
*-- end of lfGetOrder.

******header functions
*!*************************************************************
*! Name      : lfvInvDate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice date
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInvDate()
*!*************************************************************
FUNCTION lfvInvDate

IF &lcInvHdr..InvDate <> laData[6]
  IF laData[6] > m.DPOSTDATE
    *-- Message : 40086
    *-- invoice date cannot be greater than posting date
    *-- Button : 00000 
    *-- Ok
    =gfModalGen('TRM40086B00000','ALERT','')
    STORE &lcInvHdr..InvDate TO laData[6]
    RETURN
  ENDIF
  STORE .T. TO llCUpdate
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE InvDate WITH laData[6]
  UNLOCK
  IF CONSOL='Y'
    SET ORDER TO TAG CONSOL 
    =SEEK('N'+laData[4]+laData[13]+laData[14])
    REPLACE REST InvDate WITH laData[6] ;
    WHILE Consol+Account+cDivision+cCurrCode = 'N'+laData[4]+laData[13]+laData[14]
    =SEEK('Y'+laData[4]+laData[13]+laData[14])
    SET ORDER TO TAG (lcInvHdr)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvShipDate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice date
*!*************************************************************
*! Calls     : lfGetDueDate
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvShipDate()
*!*************************************************************
FUNCTION lfvShipDate

IF &lcInvHdr..ShipDate <> m.ShipDate
  STORE .T. TO llCUpdate
  =lfGetDueDate()
ENDIF

*!*************************************************************
*! Name      : lfvDueDate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice Due date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDueDate()
*!*************************************************************
FUNCTION lfvDueDate

IF &lcInvHdr..DueDate <> m.DueDate
  STORE .T. TO llCUpdate
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE DueDate WITH m.DueDate
  UNLOCK
  IF CONSOL='Y'
    SET ORDER TO TAG CONSOL 
    =SEEK('N'+laData[4]+laData[13]+laData[14])
    REPLACE REST DueDate WITH m.DueDate ;
    WHILE Consol+Account+cDivision+cCurrCode = 'N'+laData[4]+laData[13]+laData[14]
    =SEEK('Y'+laData[4]+laData[13]+laData[14])
    SET ORDER TO TAG (lcInvHdr)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvTerms
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice terms code
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

IF m.cTermCode <> ALLTRIM(laTerms[lnTerms,2])
  m.cTermCode = ALLTRIM(laTerms[lnTerms,2])
  =lfGetDueDate()
  llCod  = (ALLTRIM(lcTCod)='Y')
  STORE .T. TO llCUpdate
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE lCompUps  WITH .T.     ,;
          cTermCode WITH m.cTermCode ,;
          Cod_Flag  WITH IIF(llCod,'Y','N') ,;
          Trde_Disc WITH lnTerDiscR
  UNLOCK
  IF CONSOL='Y'
    SET ORDER TO TAG CONSOL 
    =SEEK('N'+laData[4]+laData[13]+laData[14])
    REPLACE REST lCompUps  WITH .T. ,;
                 cTermCode WITH m.cTermCode ,;
                 Cod_Flag  WITH IIF(llCod,'Y','N') ,;
                 Trde_Disc WITH lnTerDiscR  ;
    WHILE Consol+Account+cDivision+cCurrCode = 'N'+laData[4]+laData[13]+laData[14]
    =SEEK('Y'+laData[4]+laData[13]+laData[14])
    SET ORDER TO TAG (lcInvHdr)
  ENDIF
  m.Trde_Disc = Trde_Disc
ENDIF

*!*************************************************************
*! Name      : lfGetDueDate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Compute invoice Due Date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetDueDate()
*!*************************************************************
FUNCTION lfGetDueDate

=gfRltFld(m.cTermCode,@laTRltFld,'CTERMCODE')
lcTEOM = ALLTRIM(lcTEOM)
lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)

IF llIsEngland
  m.DueDate = IIF(lcTEOM <> 'Y',m.ShipDate+lnTDaysDue,;
                  CTOD('01'+SUBSTR(DTOC(GOMONTH(m.ShipDate,1)),3))-1+lnTDaysDue)
ELSE                
  m.DueDate = IIF(lcTEOM <> 'Y',m.ShipDate+lnTDaysDue,;
                  GOMONTH(CTOD(SUBSTR(DTOC(m.ShipDate),1,3)+'10'+;
                  SUBSTR(DTOC(m.ShipDate),6,5)),IIF(DAY(m.ShipDate) > lnEOMDay,2,1))+lnTDaysDue)
ENDIF
SELECT (lcInvHdr)
=RLOCK()
REPLACE DueDate  WITH m.DueDate ,;
        ShipDate WITH m.ShipDate
UNLOCK
IF CONSOL='Y'
  SET ORDER TO TAG CONSOL 
  =SEEK('N'+laData[4]+laData[13]+laData[14])
  REPLACE REST DueDate  WITH m.DueDate ,;
               ShipDate WITH m.ShipDate ;
  WHILE Consol+Account+cDivision+cCurrCode = 'N'+laData[4]+laData[13]+laData[14]
  =SEEK('Y'+laData[4]+laData[13]+laData[14])
  SET ORDER TO TAG (lcInvHdr)
ENDIF
SHOW GET m.DueDate

*!*************************************************************
*! Name      : lfvHdrComm
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice Commission
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvHdrComm()
*!*************************************************************
FUNCTION lfvHdrComm
SELECT (lcInvHdr)
=RLOCK()
REPLACE Comm1 WITH laData[9] ,;
        Comm2 WITH laData[10]
UNLOCK

IF 'LADATA(10)' $ OBJVAR(_CUROBJ)
  _CUROBJ = OBJNUM(M.NOTE1)
ENDIF

llCUpdate = .T.

*!*************************************************************
*! Name      : lfvNote
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice Notes
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvNote()
*!*************************************************************
FUNCTION lfvNote
SELECT (lcInvHdr)
=RLOCK()
REPLACE Note1 WITH m.Note1 ,;
        Note2 WITH m.Note2
UNLOCK
llCUpdate = .T.

*!*************************************************************
*! Name      : lfvPostDate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Posting date
*!*************************************************************
*! Calls     : CODECHK
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPostDate()
*!*************************************************************
FUNCTION lfvPostDate

IF &lcInvHdr..dPostDate <> m.dPostDate
  *-- Message : 40086
  *-- invoice date cannot be greater than posting date
  *-- Button : 00000 
  *-- Ok
  IF (laData[6] > m.dPostDate .AND. gfModalGen('TRM40086B00000','ALERT','')=1) .OR. ;
    !CHECKPRD(m.dPostDate,'lcGlYear','lcGlPeriod','IN')
    STORE &lcInvHdr..dPostDate TO m.dPostDate
    RETURN
  ENDIF  
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE dPostDate WITH m.dPostDate
  UNLOCK
  llCUpdate = .T.
  IF CONSOL='Y'
    SET ORDER TO TAG CONSOL 
    =SEEK('N'+laData[4]+laData[13]+laData[14])
    REPLACE REST dPostDate WITH m.dPostDate ;
    WHILE Consol+Account+cDivision+cCurrCode = 'N'+laData[4]+laData[13]+laData[14]
    =SEEK('Y'+laData[4]+laData[13]+laData[14])
    SET ORDER TO TAG (lcInvHdr)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvShipVia
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice shipvia code
*!*************************************************************
*! Calls     : CODECHK
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvShipVia()
*!*************************************************************
FUNCTION lfvShipVia

IF m.ShipVia <> ALLTRIM(laShipVia[lnShipVia,2])
  IF ALLTRIM(laShipVia[lnShipVia,2]) = '*' AND Consol = 'N'
    lnShipVia = ASUBSCRIPT(laShipVia,ASCAN(laShipVia,ALLTRIM(m.Shipvia)),1)
    RETURN 
  ENDIF
  m.ShipVia = ALLTRIM(laShipVia[lnShipVia,2])
  STORE .T. TO llCUpdate
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE ShipVia  WITH m.ShipVia ,;
          lCompUps WITH .T.
  UNLOCK
  IF CONSOL='Y'  
    SET ORDER TO TAG CONSOL 
    SCAN FOR Consol+Account+cDivision+cCurrCode = ;
             'N'+laData[4]+laData[13]+laData[14]
      REPLACE lCompUps WITH .T. ,;
              ShipVia  WITH IIF(m.ShipVia <> '*',m.ShipVia,;
                 IIF(!SEEK('S'+Account+STORE,'CUSTOMER'),' ',;
                 IIF(Customer.nBrkWeight <> 0 AND Weight > Customer.nBrkWeight,;
                 Customer.cAltShpVia,Customer.ShipVia)))
    ENDSCAN
    =SEEK('Y'+laData[4]+laData[13]+laData[14])    
    SET ORDER TO TAG (lcInvHdr)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvSpcInst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice special instruction code
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
IF m.SpcInst <> ALLTRIM(laSpcInst[lnSpcInst,2])
  m.SpcInst = ALLTRIM(laSpcInst[lnSpcInst,2])
  STORE .T. TO llCUpdate
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE SpcInst WITH m.SpcInst
  UNLOCK
  IF CONSOL='Y'
    SET ORDER TO TAG CONSOL 
    =SEEK('N'+laData[4]+laData[13]+laData[14])
    REPLACE REST SpcInst WITH m.SpcInst ;
    WHILE Consol+Account+cDivision+cCurrCode = 'N'+laData[4]+laData[13]+laData[14]
    =SEEK('Y'+laData[4]+laData[13]+laData[14])
    SET ORDER TO TAG (lcInvHdr)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvRep1
*! Developer : WAM
*! Date      : 07/01/1996
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

IF llBrowse .OR. (!EMPTY(m.Rep1) .AND. !SEEK(m.Rep1,'SalesRep'))
  m.Rep1 = IIF(llBrowse,SPACE(3),m.Rep1)
  DO REPCHK WITH m.Rep1
  STORE .T. TO llCUpdate
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Rep1 WITH m.Rep1
  UNLOCK
ENDIF  
llBrowse = .F.
=lfRefresh(lcWinCh3)

*!*************************************************************
*! Name      : lfvRep2
*! Developer : WAM
*! Date      : 07/01/1996
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

IF llBrowse .OR. (!EMPTY(m.Rep2) .AND. !SEEK(m.Rep2,'SalesRep'))
  m.Rep2 = IIF(llBrowse,SPACE(3),m.Rep2)
  DO REPCHK WITH m.Rep2
  STORE .T. TO llCUpdate
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Rep2 WITH m.Rep2
  UNLOCK
ENDIF  
llBrowse = .F.
=lfRefresh(lcWinCh3)

*!*************************************************************
*! Name      : lfvExRate
*! Developer : WAM
*! Date      : 07/01/1996
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

IF m.nExRate <> &lcInvHdr..nExRate
  IF m.nExRate <=0
    *-- Message : 40037
    *-- The exchange rate must be greater than zero
    *-- Button : 00000 
    *-- Ok
    =gfModalGen('INM40037B00000','ALERT')
    m.nExRate = &lcInvHdr..nExRate
  ENDIF
  STORE .T. TO llCUpdate
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE nExRate WITH m.nExRate
  UNLOCK
ENDIF

*!*************************************************************
*! Name      : lfvCodTag
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice COD Tag
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCodTag()
*!*************************************************************
FUNCTION lfvCodTag
SELECT (lcInvHdr)
=RLOCK()
REPLACE CODTAG WITH m.CODTAG
UNLOCK

*!*************************************************************
*! Name      : lfvCodTrK
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice UPS COD Tracking#
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCodTrK()
*!*************************************************************
FUNCTION lfvCodTrK
SELECT (lcInvHdr)
=RLOCK()
REPLACE CCODTRCKNO WITH m.CCODTRCKNO
UNLOCK

*!*************************************************************
*! Name      : lfvInvDisc
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice Merchandise Discount 
*!*************************************************************
*! Calls     : lfvInvChrg
*!*************************************************************
*! Passed Parameters  :  llPercent : .T. : Discount Percent
*!                                   .F. : Discount Amount
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInvDisc(.T.)
*!*************************************************************
FUNCTION lfvInvDisc
PARAMETERS llPercent

m.DiscPcnt = IIF(llPercent,m.DiscPcnt,ABS(m.Discount)*100/m.FabShipAmt)
m.Discount = IIF(llPercent,-1*m.FabShipAmt*m.DiscPcnt/100,-1*ABS(m.Discount))
=lfvInvChrg()

*!*************************************************************
*! Name      : lfvGstTax
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice GST Tax
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Passed Parameters  :  llRate : .T. : GST Tax Rate Percent
*!                                .F. : GST Tax Rate Amount
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvGstTax(.T.)
*!*************************************************************
FUNCTION lfvGstTax
PARAMETERS llRate,llCalled
PRIVATE lnOTaxAmnt,lnTaxAmount
IF lfNegChrg()

  SELECT (lcInvHdr)  
  lnTaxAmount = IIF(UPPER(ALLTRIM(gcContCode))='USA',nTaxDue,FabShipAmt)
  m.Tax_Amt = IIF(llRate,m.Tax_Rate*(IIF(laSetups[11,2]='M',lnTaxAmount,;
                  lnTaxAmount+Freight+Insur+COD)-DiscPcnt*lnTaxAmount/100)/100,m.Tax_Amt)
  IF m.Tax_Amt > IIF(laSetups[11,2]='M',FabShipAmt,;
                 FabShipAmt+Freight+Insur+COD)+Discount
    m.Tax_Amt = &lcInvHdr..Tax_Amt
    RETURN
  ENDIF

  m.Tax_Rate = IIF(!llRate,;
                   IIF(m.Tax_Amt = 0 , 0 ,;
                   IIF(laSetups[11,2]='M' AND lnTaxAmount = 0 , 0 ,;
                   IIF(laSetups[11,2]#'M' AND lnTaxAmount+Freight+Insur+COD-DiscPcnt*lnTaxAmount/100 = 0 , 0 ,;
                   m.Tax_Amt*100/(IIF(laSetups[11,2]='M',lnTaxAmount,lnTaxAmount+Freight+Insur+COD)-DiscPcnt*lnTaxAmount/100))));
                  ,m.Tax_Rate)

  m.nPstAmt  = IIF(llIsCanada .AND. VAL(cTaxRule)=2,;
               (FabShipAmt+Freight+Insur+COD+m.Tax_Amt+Discount)*nPstRate/100,nPstAmt)
  IF Cod_Flag='Y'
  IF EMPTY(lcUPSType) OR lcUPSType='OTHER'
  
    m.Cod_Amt = FabShipAmt-FabShipAmt*m.DiscPcnt/100 +m.Tax_Amt+m.nPstAmt
  ELSE
    m.Cod_Amt = FabShipAmt+Freight+Insur+COD+Discount+m.Tax_Amt+m.nPstAmt
  ENDIF
   
  IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. ;
      Cartons=1 AND SEEK(cMOrder+Store+PikTkt,lcUpsBox)  
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH m.Cod_Amt
      UNLOCK
    ENDIF
  ENDIF
  SELECT (lcInvHdr)  
  lnOTaxAmnt=Tax_Amt
  m.TotalChg = FabShipAmt+Freight+Insur+COD+Discount+m.Tax_Amt+m.nPstAmt+IIF(lnPstRule=2,m.nHstAmt,0) 
  
  =RLOCK()  
  REPLACE Tax_Rate  WITH m.Tax_Rate  ,;
          Tax_Amt   WITH m.Tax_Amt   ,;
          nPstAmt   WITH m.nPstAmt   ,;
          Cod_Amt   WITH IIF(Cod_Flag='Y',m.Cod_Amt,0) ,;
          TotalChg  WITH m.TotalChg
  UNLOCK
  IF llCalled
    RETURN
  ENDIF
  lnRecNo = RECNO()
  SET ORDER TO TAG 'CONSOL'
  IF SEEK('Y'+Account+cDivision+cCurrCode)
    m.Tax_Amt = Tax_Amt-lnOTaxAmnt+m.Tax_Amt
    =lfvGstTax(.F.,.T.)
  ENDIF
  SET ORDER TO TAG (lcInvHdr)
  GO lnRecNo
  SCATTER MEMVAR FIELDS Tax_Rate,Tax_Amt,nPstAmt,Cod_Amt,TotalChg
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
  RETURN
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvPstTax
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Compute PST Tax Rate and Amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  llRate : .T. : PST Tax Rate Percent
*!                                .F. : PST Tax Rate Amount
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPstTax(.T.)
*!*************************************************************
FUNCTION lfvPstTax
PARAMETERS llRate,llCalled
PRIVATE lnOTaxAmnt

IF lfNegChrg()

  SELECT (lcInvHdr)  
  m.cTaxRule = STR(lnPstRule,2)
  m.nPstAmt  = IIF(llRate,(FabShipAmt+Freight+Insur+COD+;
               Discount+IIF(VAL(m.cTaxRule)=1,0,Tax_Amt))*;
               m.nPstRate/100,m.nPstAmt)
  m.nPstRate = IIF(!llRate,m.nPstAmt*100/(FabShipAmt+Freight+Insur+COD+Discount+;
               IIF(VAL(m.cTaxRule)=1,0,Tax_Amt)),m.nPstRate)
  IF Cod_Flag='Y'
    IF EMPTY(lcUPSType) OR lcUPSType='OTHER'
      m.Cod_Amt = FabShipAmt-FabShipAmt*m.DiscPcnt/100 +m.Tax_Amt+m.nPstAmt
    ELSE
   m.Cod_Amt = FabShipAmt+Freight+Insur+COD+Discount+Tax_Amt+m.nPstAmt
  
  ENDIF
  IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' ;
     AND (EMPTY(laData[3]) OR ASCAN(laEvntTrig , PADR('CRTCHRG',10)) = 0) AND ;
     Cartons=1 AND SEEK(cMOrder+Store+PikTkt,lcUpsBox)
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH m.Cod_Amt
      UNLOCK
    ENDIF
  ENDIF
  SELECT (lcInvHdr)  
  lnOTaxAmnt = nPstAmt
  m.nHstAmt  = IIF(llRate,(FabShipAmt+Freight+Insur+COD+;
               Discount+IIF(VAL(m.cTaxRule)=1,0,Tax_Amt))*;
               m.nHstRate/100,m.nHstAmt)
  m.TotalChg = FabShipAmt+Freight+Insur+COD+Discount+Tax_Amt+m.nPstAmt+IIF(lnPstRule=2,m.nHstAmt,0)
  
  =RLOCK()
  REPLACE cTaxRule WITH m.cTaxRule ,; 
          nPstRate WITH m.nPstRate ,;
          nPstAmt  WITH m.nPstAmt  ,;
          Cod_Amt  WITH IIF(Cod_Flag='Y',m.Cod_Amt,0) ,;
          TotalChg WITH m.TotalChg
  UNLOCK
  IF llCalled
    RETURN
  ENDIF
  lnRecNo = RECNO()
  SET ORDER TO TAG 'CONSOL'
  IF SEEK('Y'+Account+cDivision+cCurrCode)
    m.nPstAmt = nPstAmt-lnOTaxAmnt+m.nPstAmt
    =lfvPstTax(.F.,.T.)
  ENDIF
  SET ORDER TO TAG (lcInvHdr)
  GO lnRecNo
  SCATTER MEMVAR FIELDS nPstRate,nPstAmt,Cod_Amt,TotalChg
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
ENDIF

*!*************************************************************
*! Name      : lfvWeight
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice Weight
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvWeight()
*!*************************************************************
*N000388,1 This function is not used anymore since we have changed
*N000388,1 the weight to be with no validation
FUNCTION lfvWeight

IF m.Weight <> &lcInvHdr..Weight 
  IF llIsEngland
    SELECT (lcInvHdr)  
    =RLOCK()
    REPLACE Weight WITH m.Weight
    UNLOCK
  ELSE
    =lfvUpsChrg()
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvCartons
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice Cartons
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCartons()
*!*************************************************************
*N000388,1 This function is not used anymore since we have changed
*N000388,1 the weight to be with no validation
FUNCTION lfvCartons

IF m.Cartons <> &lcInvHdr..Cartons
  IF llIsEngland
    SELECT (lcInvHdr)  
    =RLOCK()
    REPLACE Cartons WITH m.Cartons
    UNLOCK
  ELSE
    =lfvUpsChrg()
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvInvChrg
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Calculate Invoices Total Chrage
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInvChrg()
*!*************************************************************
FUNCTION lfvInvChrg
PARAMETERS llCalled

IF lfNegChrg()

  SELECT (lcInvHdr)
  IF laSetups[9,2]='Y'     && Compute Tax
    IF llIsEngland
      SELECT (lcEngChrg)
      =SEEK(laData[1]+laData[5]+laData[3])
      SUM REST nChrgAmnt*(1-m.Trde_Disc/100)*(100-m.DiscPcnt)/100*nTaxRate/100 ;
      TO m.nChrgTax  ;
      WHILE cMOrder+Store+PikTkt+cchrgcode = laData[1]+laData[5]+laData[3]
      SELECT (lcInvHdr)
      m.Tax_Amt = m.nChrgTax+nMerchTax*(100-m.DiscPcnt)/100*(100-m.Trde_Disc)/100
    ELSE  
      lnTaxAmount = IIF(UPPER(ALLTRIM(gcContCode))='USA',nTaxDue,FabShipAmt)
      m.Tax_Amt = Tax_Rate*(IIF(laSetups[11,2]='M',lnTaxAmount,lnTaxAmount+m.COD+;
                  m.Insur+m.Freight)-m.DiscPcnt*lnTaxAmount/100)/100
      m.nPstAmt = IIF(llIsCanada,(FabShipAmt+m.COD+m.Insur+m.Freight-;
                  FabShipAmt*m.DiscPcnt/100+;
                  IIF(VAL(cTaxRule)=1,0,m.Tax_Amt))*nPstRate/100,0)
      m.nHstAmt  = IIF(llIsCanada,(FabShipAmt+m.COD+m.Insur+m.Freight-;
                  FabShipAmt*m.DiscPcnt/100+IIF(VAL(cTaxRule)=1,0,m.Tax_Amt))*nHstRate/100,0)
    ENDIF
  ENDIF
  IF Cod_Flag='Y'

    IF EMPTY(lcUPSType) OR lcUPSType='OTHER'
      m.Cod_Amt = FabShipAmt-FabShipAmt*m.DiscPcnt/100 +m.Tax_Amt+m.nPstAmt
    ELSE
      m.Cod_Amt = FabShipAmt+m.Freight+m.Insur+m.COD+nCharges-;
                FabShipAmt*m.DiscPcnt/100+m.Tax_Amt+m.nPstAmt
    ENDIF

  IF !llIsEngland .AND. laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. ;
       Cartons=1 .AND. SEEK(cMOrder+Store+Piktkt,lcUpsBox)
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH m.Cod_Amt
      UNLOCK
    ENDIF
  ENDIF

  m.TotalChg = FabShipAmt+m.Freight+m.Insur+m.COD+nCharges-;
               ROUND(FabShipAmt*m.DiscPcnt/100,2)+m.Tax_Amt+m.nPstAmt+IIF(lnPstRule=2,m.nHstAmt,0)
  SELECT (lcInvHdr)  
  lnOFreight = Freight
  lnOInsur   = Insur
  lnOCod     = COD
  lnODisc    = Discount
  =RLOCK()

  REPLACE DiscPcnt  WITH m.DiscPcnt  ,;
          Discount  WITH m.Discount  ,;
          Trde_Disc WITH m.Trde_Disc ,;
          Freight   WITH m.Freight   ,;
          Insur     WITH m.Insur     ,;
          Cod       WITH m.COD       ,;
          nChrgTax  WITH m.nChrgTax  ,;
          Tax_Amt   WITH m.Tax_Amt   ,;
          nPstAmt   WITH m.nPstAmt   ,;
          Cod_Amt   WITH IIF(Cod_Flag='Y',m.Cod_Amt,0) ,;
          TotalChg  WITH m.TotalChg
  REPLACE nHstAmt   WITH m.nHstAmt
  UNLOCK
  IF llCalled
    RETURN
  ENDIF
  lnRecNo = RECNO()
  SET ORDER TO TAG 'CONSOL'
  IF Consol = 'Y'
    =SEEK('N'+laDAta[4]+laData[13]+laData[14])
    SCAN REST WHILE Consol+Account+cDivision+cCurrCode=;
                    'N'+laDAta[4]+laData[13]+laData[14]
      SCATTER MEMVAR FIELDS DiscPcnt,COD,Insur,Freight
      =lfvInvChrg(.T.)
    ENDSCAN
  ELSE
    IF SEEK('Y'+Account+cDivision+cCurrCode)
      m.Freight  = Freight-lnOFreight+m.Freight
      m.Insur    = Insur -lnOInsur+m.Insur
      m.COD      = Cod -lnOCod+m.COD
      m.Discount = Discount-lnODisc+m.Discount
      m.DiscPcnt = ABS(m.Discount)*100/FabShipAmt
      =gfRltFld(ShipVia,@laVRltFld,'SHIPVIA')
      =lfvInvChrg(.T.)
    ENDIF
  ENDIF
  SET ORDER TO TAG (lcInvHdr)
  GO lnRecNo
  SCATTER MEMVAR FIELDS COD,Insur,Freight,nPstAmt,nHstAmt,Tax_Amt,Cod_Amt,TotalChg,DiscPcnt,Discount
  =gfRltFld(ShipVia,@laVRltFld,'SHIPVIA')
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
ENDIF

*!*************************************************************
*! Name      : lfvCodAmnt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice COD Amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCodAmnt()
*!*************************************************************
FUNCTION lfvCodAmnt

IF lfNegChrg()
    IF !llIsEngland .AND. laSetups[15,2]='Y' .AND. m.Cartons=1 .AND. ;
     SEEK(laData[1]+laData[5]+laData[3],lcUpsBox)
    IF SUBSTR(lcUpsType,1,5)='USUPS'
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH m.Cod_Amt
      UNLOCK
    ENDIF
  ENDIF
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Cod_Amt WITH m.Cod_Amt
  UNLOCK
ENDIF

*!*************************************************************
*! Name      : lfvUpsIns
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice UPS Charge
*!*************************************************************
*! Calls     : lfvUpsChrg()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvUpsIns()
*!*************************************************************
FUNCTION lfvUpsIns
SELECT (lcInvHdr)
=RLOCK()
REPLACE lUpsIns WITH m.lUpsIns
UNLOCK
=lfvUpsChrg(.F.,'I')

*!*************************************************************
*! Name      : lfvUseCod
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice COD Insurance
*!*************************************************************
*! Calls     : lfvUpsChrg()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvUseCod()
*!*************************************************************
FUNCTION lfvUseCod

m.Cod_Flag   = IIF(llCod,'Y','N')
m.CODTAG     = IIF(llCod,m.CODTAG,'')
m.CCODTRCKNO = IIF(llCod,m.CCODTRCKNO,'')


IF llIsEngland
  m.Cod_Amt = IIF(m.Cod_Flag='Y',FabShipAmt+nCharges+Tax_Amt+Discount,0)
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Cod_Flag   WITH m.Cod_Flag  ,;
          Cod_Amt    WITH m.Cod_Amt   ,;
          CODTAG     WITH m.CODTAG    ,;
          CCODTRCKNO WITH m.CCODTRCKNO,;
          lKeyOff    WITH IIF(llCod,lKeyOff,.F.)
  UNLOCK
  IF CONSOL='Y'
    SET ORDER TO TAG CONSOL 
    =SEEK('N'+laData[4]+laData[13]+laData[14])
    REPLACE REST Cod_Flag WITH m.Cod_Flag ,;
                 Cod_Amt  WITH IIF(llCod,FabShipAmt+nCharges+Tax_Amt+Discount,0)  ;
    WHILE Consol+Account+cDivision+cCurrCode = 'N'+laData[4]+laData[13]+laData[14]
    =SEEK('Y'+laData[4]+laData[13]+laData[14])
    SET ORDER TO TAG (lcInvHdr)
  ENDIF
  SHOW GET m.Cod_Amt
  
  *N000388,1 Commented out. [Begin]
  *SHOW GET m.CODTAG
  *SHOW GET m.CCODTRCKNO
  *N000388,1 Commented out. [End]
  
ELSE
  =lfvUpsChrg(.F.,'C')
ENDIF

*N000388,1 Zero Cod amount if C.O.D charge not checked. [Begin]
IF !llCod
  m.Cod_Amt = 0
ENDIF
*N000388,1 Zero Cod amount if C.O.D charge not checked. [End]

llOpnCrdt = .F.
IF llCod AND m.TotalChg > 0
  DO gfAcOpCrdt IN (gcapphome+gcwinappl+'\ARMtInv.PRG') WITH laData[4],laData[14],'llOpnCrdt'
  SHOW GET m.Cod_Amt ENABLE
ELSE
  SHOW GET m.Cod_Amt DISABLE
ENDIF
=lfRefresh(lcWinChrg)
IF llOpnCrdt  
  SHOW GET pbCredits ENABLE
ELSE
  SHOW GET pbCredits DISABLE
ENDI

*!*************************************************************
*! Name      : lfvCredits
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Select Account Credits
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCredits()
*!*************************************************************
FUNCTION lfvCredits
PRIVATE lnApplied,llKeyOff,lnKeyOffAmnt

lnKeyOffAmnt = IIF(SEEK(laData[1]+laData[5]+laData[3],lcInstHdr),;
               IIF(&lcInstHdr..nInstIAmnt>0,&lcInstHdr..nInstIAmnt,0),m.TotalChg)
IF lnKeyOffAmnt = 0
  RETURN
ENDIF
IF !USED(lcAppCrdt)
  =gfOpenFile(gcDataDir+'ARHIST', gcDataDir+'ARHIST','SH')
  SELECT ARHIST
  =AFIELDS(laFileStru)
  =gfCloseFile('ARHIST')
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+6,4]
  laFileStru[lnFileStru+1,1] = 'CMOrder'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'PikTkt'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 6
  laFileStru[lnFileStru+2,4] = 0
  laFileStru[lnFileStru+3,1] = 'cStore'
  laFileStru[lnFileStru+3,2] = 'C'
  laFileStru[lnFileStru+3,3] = 8
  laFileStru[lnFileStru+3,4] = 0
  laFileStru[lnFileStru+4,1] = 'nTrnNewAmn'
  laFileStru[lnFileStru+4,2] = 'N'
  laFileStru[lnFileStru+4,3] = 11
  laFileStru[lnFileStru+4,4] = 2
  laFileStru[lnFileStru+5,1] = 'cShToOpn'
  laFileStru[lnFileStru+5,2] = 'C'
  laFileStru[lnFileStru+5,3] = 1
  laFileStru[lnFileStru+5,4] = 0
  laFileStru[lnFileStru+6,1] = 'nStep'
  laFileStru[lnFileStru+6,2] = 'N'
  laFileStru[lnFileStru+6,3] = 2
  laFileStru[lnFileStru+6,4] = 0
  =gfCrtTmp(lcAppCrdt,@laFileStru,[Account+cMOrder+cStore+PikTkt+Tran],lcAppCrdt)
ENDIF

llKeyOff = &lcInvHdr..lKeyOff
DO gpOpnCredits IN (gcapphome+gcwinappl+'\ARMtInv.PRG') WITH ;
laData[4],laData[1],laData[5],laData[3],lnKeyOffAmnt,'llKeyOff',lcAppCrdt,laData[14]
SELECT (lcAppCrdt)
=SEEK(laData[4]+laData[1]+laData[5]+laData[3])
SUM REST Amount TO lnApplied WHILE ;
Account+cMOrder+cStore+PikTkt=laData[4]+laData[1]+laData[5]+laData[3]


m.Cod_Amt = lnKeyOffAmnt - ABS(lnApplied)
SELECT (lcInvHdr)
=RLOCK()
REPLACE COD_AMT WITH m.Cod_Amt ,;
        lKeyOff WITH llKeyOff
UNLOCK
SHOW GET m.Cod_Amt
=lfRefresh(lcWinChrg)

*!*************************************************************
*! Name      : lfvUpsChrg
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Compute USA/Canadian UPS Charges
*!*************************************************************
*! Calls     : gpUpsChrg,lfRefresh
*!*************************************************************
*! Passed Parameters  : llCalled   : Called for recursion
*!                      lcChrgType : Charge Type: Freight, Insurance, Cod, All
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvUpsChrg()
*!*************************************************************
FUNCTION lfvUpsChrg

PARAMETERS llCalled,lcChrgType
lcChrgType=IIF(TYPE('lcChrgType')='C',lcChrgType,'A')

WAIT 'Computing Invoice Charges...' WINDOW NOWAIT
SELECT (lcInvHdr)
SET ORDER TO TAG CONSOL
lcKey = Account+cDivision+cCurrCode
IF Consol='Y'
  =SEEK('N'+lcKey)
  SCAN REST WHILE Consol+Account+cDivision+cCurrCode='N'+lcKey
    SCATTER FIELDS &lcScFields TO laData
    SCATTER MEMVAR FIELDS FABSHIPAMT,DISCPCNT,TAX_RATE,cTaxRule,nPstRate,nHstRate,;
    ShipVia,COD_AMT,FREIGHT,INSUR,COD,CARTONS,WEIGHT,TAX_AMT,nPstAmt,nHstAmt,TOTALCHG
    =lfvUpsChrg(.T.,lcChrgType)
  ENDSCAN
  =SEEK('Y'+lcKey)
  =RLOCK()
  lnTaxAmount = IIF(UPPER(ALLTRIM(gcContCode))='USA',nTaxDue,FabShipAmt)

  lnGstChrg = IIF(laSetups[11,2]='M',lnTaxAmount,;
                  lnTaxAmount+Freight+Insur+COD)-DiscPcnt*lnTaxAmount/100
  lnPstChrg = FabShipAmt+Freight+Insur+COD+Discount+IIF(VAL(m.cTaxRule)=1,0,Tax_Amt)
  REPLACE lCompUps WITH .F.        ,;
          Cod_Flag WITH m.Cod_Flag ,;
          lUpsIns  WITH m.lUpsIns  ,;
          Tax_Rate WITH IIF(lnGstChrg=0,0,Tax_Amt*100/lnGstChrg) ,;
          nPstRate WITH IIF(lnPstChrg=0,0,nPstAmt*100/lnPstChrg)
  REPLACE nHstRate WITH IIF(lnPstChrg=0,0,nHstAmt*100/lnPstChrg)
  UNLOCK
  SCATTER MEMVAR FIELDS &lcFlFields
  SCATTER FIELDS &lcScFields TO laData
ELSE
  DO gpUpsChrg IN (gcapphome+gcwinappl+'\ARMtInv.PRG') WITH ;
  laData[4],laData[5],laData[1],ladata[3],laData[7],m.FabSHIPAMT,m.Cod_Flag='Y',;
  m.lUpsIns,m.DISCPCNT,m.TAX_RATE,m.cTaxRule,m.nPstRate,lcUpsBox,'m.ShipVia',;
  'm.COD_AMT','m.FREIGHT','m.INSUR','m.COD','m.CARTONS','m.WEIGHT','m.TAX_AMT',;
  'm.nPstAmt',IIF(UPPER(ALLTRIM(gcContCode))='USA',&lcInvHdr..nTaxDue,;
  m.FabSHIPAMT),lcChrgType
  m.nHstAmt  = IIF(llIsCanada,(FabShipAmt+m.COD+m.Insur+m.Freight-;
                   M.FabShipAmt*m.DiscPcnt/100+IIF(VAL(cTaxRule)=1,0,m.Tax_Amt))*nHstRate/100,0)

  m.TotalChg = m.FabShipAmt+m.Freight+m.Insur+m.Cod+m.Discount+m.Tax_Amt+m.nPstAmt+m.nHstAmt
  m.TotalChg = m.FabShipAmt+m.Freight+m.Insur+m.Cod+m.Discount+m.Tax_Amt+m.nPstAmt+IIF(lnPstRule=2,m.nHstAmt,0) 

  SELECT (lcInvHdr)
  lnFreight  = Freight
  lnInsur    = INSUR
  lnCod      = COD
  lnOldCart  = CARTONS
  lnWeight   = WEIGHT
  lnTaxAmnt  = TAX_AMT
  lnPstAmt   = nPstAmt
  lnHstAmt   = nHstAmt
  lnCodAmt   = Cod_amt
  lnTotalChg = TotalChg
  lnRecNo = RECNO()
  IF SEEK('Y'+lcKey)
    =RLOCK()

    REPLACE Freight  WITH Freight - lnFreight + m.Freight ,;
            INSUR    WITH INSUR   - lnInsur   + m.INSUR   ,;
            COD      WITH COD     - lnCod     + m.COD     ,;
            CARTONS  WITH CARTONS - lnOldCart + m.CARTONS ,;
            WEIGHT   WITH WEIGHT  - lnWeight  + m.WEIGHT  ,;
            TAX_AMT  WITH TAX_AMT - lnTaxAmnt + m.TAX_AMT ,;
            nPstAmt  WITH nPstAmt - lnPstAmt  + m.nPstAmt ,;
            Cod_amt  WITH Cod_Amt - lnCodAmt  + m.Cod_amt ,;
            TotalChg WITH TotalChg- lnTotalChg+ m.TotalChg
    REPLACE nHstAmt WITH nHstAmt - lnHstAmt  + m.nHstAmt
    UNLOCK
  ENDIF
  *C126361,4  TMI [Start] 
  *GO lnRecNo
  GO TOP IN &lcInvHdr
  *C126361,4  TMI [End  ] 
  m.lCompUps = .F.
  =RLOCK()
  GATHER MEMVAR FIELDS FABSHIPAMT,Cod_Flag,lUpsIns,DISCPCNT,TAX_RATE,cTaxRule,;
  nPstRate,ShipVia,COD_AMT,FREIGHT,INSUR,COD,CARTONS,WEIGHT,TAX_AMT,;
  nPstAmt,nHstAmt,TOTALCHG,lCompUps
  UNLOCK
ENDIF

IF !llCalled
  SET ORDER TO TAG (lcInvHdr)
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
  WAIT CLEAR
ENDIF

*!*************************************************************
*! Name      : lfvUpsBox
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Edit USA/Canadian UPS Box
*!*************************************************************
*! Calls     : gpUpsBox,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvUpsBox()
*!*************************************************************
FUNCTION lfvUpsBox

*DO gpUpsBox IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
*lcUpsBox,laData[11],laData[4],laData[1],laData[5],laData[3],m.lUpsIns,;
*(m.Cod_Flag='Y'),m.FABSHIPAMT,m.ShipVia,laData[7],'m.Cartons','m.Weight',;
*'m.Freight','m.Insur','m.Cod','m.COD_AMT'
DO gpUpsBox IN (gcapphome+gcwinappl+'\ARMtInv.PRG') WITH ;
lcUpsBox,laData[11],laData[4],laData[1],laData[5],laData[3],m.lUpsIns,;
(m.Cod_Flag='Y'),m.FABSHIPAMT,m.ShipVia,laData[7],'m.Cartons','m.Weight',;
'm.Freight','m.Insur','m.Cod','m.COD_AMT'

SELECT (lcInvHdr)
=RLOCK()
GATHER MEMVAR FIELDS &lcFlFields
UNLOCK
SHOW GETS WINDOW (lcWinChrg) ONLY
=lfRefresh(lcWinChrg)

*!*************************************************************
*! Name      : lfvCharges
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Edit Englnad Charges
*!*************************************************************
*! Calls     : gpCharges,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCharges()
*!*************************************************************
FUNCTION lfvCharges
PRIVATE lnOCharges,lnOChrgTax

lnOCharges = m.nCharges
lnOChrgTax = m.nChrgTax

DO gpCharges IN (gcapphome+gcwinappl+'\ARMtInv.PRG') WITH ;
lcEngChrg,SPACE(6),laData[4],laData[1],laData[5],laData[3],m.TRDE_DISC,;
'm.nCharges','m.nChrgTax'
SELECT (lcInvHdr)
lnRecNo = RECNO()
SET ORDER TO TAG 'CONSOL'
IF SEEK('Y'+Account+cDivision+cCurrCode)
  =RLOCK()
  REPLACE nCharges  WITH nCharges-lnOCharges+m.nCharges ,;
          nChrgTax  WITH nChrgTax-lnOChrgTax+m.nChrgTax ,;
          Tax_Amt   WITH nChrgTax+nMerchTax*(100-m.DiscPcnt)/100*(100-m.Trde_Disc)/100 ,;
          Cod_Amt   WITH IIF(Cod_Flag='Y',FabShipAmt+nCharges+Tax_Amt+Discount,0) ,; 
          TotalChg  WITH FabShipAmt+nCharges+Tax_Amt+Discount
  UNLOCK
ENDIF
SET ORDER TO TAG (lcInvHdr)
GO lnRecNo
=RLOCK()
REPLACE nCharges  WITH m.nCharges ,;
        nChrgTax  WITH m.nChrgTax ,;
        Tax_Amt   WITH nChrgTax+nMerchTax*(100-m.DiscPcnt)/100*(100-m.Trde_Disc)/100 ,;
        Cod_Amt   WITH IIF(Cod_Flag='Y',FabShipAmt+nCharges+Tax_Amt+Discount,0) ,; 
        TotalChg  WITH FabShipAmt+nCharges+Tax_Amt+Discount
UNLOCK
m.Tax_Amt  = Tax_Amt
m.Cod_Amt  = Cod_Amt
m.TotalChg = TotalChg
=lfRefresh(lcWinChrg)
SHOW GET m.Cod_Amt

*****options
*!*************************************************************
*! Name      : lfActPad
*! Developer : WAM
*! Date      : 07/01/1996
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
ON PAD _INQUIRY OF _MSYSMENU ACTIVATE POPUP _INQURYPOP

DEFINE POPUP _INQURYPOP MARGIN SHADOW
DEFINE BAR 1 OF _INQURYPOP  PROMPT 'Customer \<Inquire' SKIP FOR laScrMode[1]
DEFINE BAR 2 OF _INQURYPOP  PROMPT 'Customer \<Notepad' SKIP FOR laScrMode[1]
DEFINE BAR 3 OF _INQURYPOP  PROMPT '\<Aging Summary'    SKIP FOR laScrMode[1]
DEFINE BAR 4 OF _INQURYPOP  PROMPT '\<Fabric Inquire'    SKIP FOR (lnactfolder<>3) .OR. (EMPTY(m.Fabric) AND EMPTY(m.Color))
DEFINE BAR 5 OF _INQURYPOP  PROMPT '\<Line Notepad'     SKIP FOR (lnactfolder<>3) .OR. laSetups[3,2]<>'Y' .OR. (EMPTY(m.Fabric) AND EMPTY(m.Color))
DEFINE BAR 6 OF _INQURYPOP  PROMPT '\<Copy Line'        SKIP FOR (lnactfolder<>3) .OR. (EMPTY(m.Fabric) AND EMPTY(m.Color)) .OR. llZoom
DEFINE BAR 7 OF _INQURYPOP  PROMPT '\<Paste Line'       SKIP FOR (lnactfolder<>3) .OR. (EMPTY(m.Fabric) AND EMPTY(m.Color)) .OR. llZoom
DEFINE BAR 8 OF _INQURYPOP  PROMPT '\<Zoom Release Order Lines' SKIP FOR (lnactfolder<>3)
DEFINE BAR 9  OF _INQURYPOP PROMPT 'Session \<Defaults' SKIP FOR !llPost .OR. !laScrMode[1]
DEFINE BAR 10 OF _INQURYPOP PROMPT '\<Batch Summary'    SKIP FOR !llPost .OR. laScrMode[1] 
DEFINE BAR 11 OF _INQURYPOP PROMPT 'Release \<Order Notes'      SKIP FOR laScrMode[1]
DEFINE BAR 12 OF _INQURYPOP PROMPT 'PO\<ST'                     SKIP FOR laScrMode[1] .OR. lnActFolder<>1
DEFINE BAR 13 OF _INQURYPOP PROMPT 'Remove the Release Order'   SKIP FOR laScrMode[1]
DEFINE BAR 14 OF _INQURYPOP PROMPT '\-'
DEFINE BAR 15 OF _INQURYPOP PROMPT 'Edit Special instructions.' SKIP FOR laScrMode[1]

*C126361,1  TMI [End  ] 
SET MARK OF BAR 9 OF _INQURYPOP TO llZoom
ON SELECTION POPUP _INQURYPOP DO lpvInquiry

*!*************************************************************
*! Name      : lpvInquiry
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice options
*!*************************************************************
*! Calls     : gpDoProg,NOTEPAD,ARAGING.SPX,lfvLineNote,lfvCopyQty,;
               lfvPasteQty,lfvBrowZoom,ARINVSES.SPX,lfSetScope
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
  CASE BAR() = 1      && Customer Inquire
  	lcParameter = "'"+laData[4]+"'"+IIF(EMPTY(laData[5]),'',",'"+laData[5]+"'")
    DO gpDoProg WITH 'AWRARCUST',.F.,'AR',lcParameter
  CASE BAR() = 2      && Customer Notepad
    =NOTEPAD('A',laData[4])
  CASE BAR() = 3      && Customer Credit
    =SEEK('M'+ladata[4],'Customer')
    DO (gcScrDir+gcWinAppl+"\ARAGING.SPX") WITH &lcInvHdr..APPROVAL
  CASE BAR() = 4      && Style Inquire
  	lcParameter = "'"+m.Fabric+"','" + m.Color+ "'"
    DO gpDoProg WITH 'AWRMAMATRL',.F.,'MA',lcParameter
  CASE BAR() = 5      &&  Invoice Line Notepad
    =lfvLineNote()
  CASE BAR() = 6      &&  Copy Invoice Line
   =lfvCopyQty()
  CASE BAR() = 7      &&  Paste Invoice LIne
   =lfvPasteQty()
  CASE BAR() = 8      &&   Zoom Invoice Browse
    llZoom = !llZoom
    =lfvBrowZoom()
    SET MARK OF BAR 8 OF _INQURYPOP TO llZoom
  CASE BAR() = 9      &&   Session Defaults
    llCancel=.F.
    ldOldInvDate = ldDefInvDate 
    ldOldPstDate = ldDefPstDate
    
    DO (gcScrDir+gcWinAppl+"\ARINVSES.SPX")
    IF llCancel
      ldDefInvDate = ldOldInvDate
      ldDefPstDate = ldOldPstDate
    ENDIF
    
  CASE BAR() = 10     &&   Batch Summary
    =lfBatchSum(.F.)

  CASE BAR() = 11     &&   Scope Orders
    =NOTEPAD('C',&lcInvHdr..CMATINV)


   CASE BAR() = 12     && Refresh sales rep. header commession befor saving
     =lfPost()

   CASE BAR() = 13     && Remove the release order
     =gfCPDelete()    && Rewritten locally 
     
   CASE BAR() = 15     && Special instruction screen
     =lfSpInst()    
     
ENDCASE

*!*************************************************************
*! Name      : lfvBrowZoom
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Zoom In/Zoom Out Invoice lines
*!*************************************************************
*! Calls     : lfBrowDet,lfActFolder
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvBrowZoom()
*!*************************************************************
FUNCTION lfvBrowZoom

=lfBrowDet()
=lfActFolder()

*!*************************************************************
*! Name      : lfBatchSum
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Show Invoices Batch Summary
*!*************************************************************
*! Calls     : ARIINVS.SPX
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBatchSum()
*!*************************************************************
FUNCTION lfBatchSum
PARAMETERS llSave
PRIVATE lnAlias,lcOrdKey,lnAllShip,lnAllAmnt,lnAllCtns,lnAllWght,lnAllInvs,llProceed

lnAlias = SELECT()
SELECT (lcInvHdr)
lcOrdKey = Account+cMOrder+Store+PikTkt
SET ORDER TO TAG 'SELECT'
=SEEK("»")

SUM REST FabShip,TotalChg,Cartons,Weight  ;
         TO lnAllShip,lnAllAmnt,lnAllCtns,lnAllWght ;
    WHILE cSelect+Account+cMOrder+Store+PikTkt = "»" ;
    FOR   (Consol='Y' OR EMPTY(Flag))  && AND FabShip > 0

lnAllInvs = _TALLY
llProceed = .F.

IF lnAllInvs > 0
  DO (gcScrDir+gcWinAppl+"\ARMINVS.SPX") WITH llSave
ENDIF
SET ORDER TO TAG (lcInvHdr) IN (lcInvHdr)

=SEEK(lcOrdKey,lcInvHdr)
SELECT (lnAlias)
RETURN(llProceed)

******detailed functions
*!*************************************************************
*! Name      : lfvNew
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : New Invoice line
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

SCATTER MEMVAR BLANK MEMO
STORE .T. TO llAddLine, m.lNewLine
SHOW GETS WINDOW (lcWinCh42) DISABLE ONLY
SHOW GETS WINDOW (lcWinCh44) DISABLE ONLY
SHOW GETS WINDOW (lcWinCh45) DISABLE ONLY
SHOW GETS WINDOW (lcWinCh46) DISABLE ONLY
SHOW GET ibFabric ENABLE
SHOW GET m.Fabric ENABLE
SHOW GET ibColor ENABLE
SHOW GET m.Color ENABLE
=lfRefresh(lcWinCh42)
=lfRefresh(lcWinCh44)
=lfRefresh(lcWinCh46)
_CUROBJ = OBJNUM(m.Fabric)

*N000388,1 Disable the Rolls button in case of LOT Method. [Begin]
IF laSetups[7,2] = 'L'
  SHOW GET PbRolls DISABLE
ENDIF  
*N000388,1 Disable the Rolls button in case of LOT Method. [End]

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Remove Invoice line
*!*************************************************************
*! Calls     : gfModalGen(),lfUpCnsLine()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvRemove()
*!*************************************************************
FUNCTION lfvRemove
*-- Message : 40011
*-- Are you sure you want to remove this xxx?
*-- Button : 40000
*-- Yes No
IF gfModalGen('QRM40011B40000','ALERT','invoice line')= 1

  *sse we need to ask
  *lnCartons  = &lcInvHdr..nCartons - IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0)
  *m.Cartons  = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
  *m.Weight   = m.Weight - m.FabQty * Style.nStyWeight
  *sse we need to ask

  *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
  *m.FabShipAmt  = m.FabShipAmt - ROUND(m.FabQty * m.nSellNetPr,2)
  m.FabShipAmt  = m.FabShipAmt - CEILING(m.FabQty * m.nSellNetPr * 100)/100
  *E126684,1  TMI [End  ] 
  m.FabShip     = m.FabShip - m.FabQty 
  m.Discount = -1*m.DiscPcnt*m.FabShipAmt/100
  lnQuantity = 0
  
  *N000388,1 Commented out. [Begin]
  *IF laSetups[9,2]='Y' .AND. llIsEngland .AND. Style.nTaxBreak <> 0
  *  FOR lnCount = Style.nTaxBreak TO 8
  *    lnQuantity = lnQuantity - EVAL('m.Qty'+STR(lnCount,1))
  *  ENDFOR
  *ENDIF
  *N000388,1 Commented out. [End]
  
  *B607226,1 ABD - Assign the remove Qty to remove the assign tax to this line. [Begin]
  IF laSetups[9,2]='Y' .AND. llIsEngland
    lnQuantity = lnQuantity - m.FabQty
  ENDIF
  *B607226,1 ABD - [End]
  
  *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
  *m.nTaxDue = &lcInvHdr..nTaxDue - IIF(lTaxable,ROUND(m.FabQty*m.nSellNetPr,2),0)
  m.nTaxDue = &lcInvHdr..nTaxDue - IIF(lTaxable,CEILING(m.FabQty*m.nSellNetPr*100)/100,0)
  *E126684,1  TMI [End  ] 
  m.FabQty = 0
  =IIF(llUpCnsLine,lfUpCnsLine() AND lfUpCnsCtn(&lcInvHdr..Cartons,m.Cartons),.T.)
  SELECT (lcInvHdr)  
  =RLOCK()  
  *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
  *REPLACE CARTONS   WITH m.Cartons ,;
          nCartons  WITH lnCartons ,;
          WEIGHT    WITH m.Weight  ,;
          FabSHIPAMT WITH m.FabShipAmt ,;
          FabSHIP      WITH m.FabShip    ,;
          Discount  WITH m.Discount,;
          lCompUps  WITH !llIsEngland ,;
          nMerchTax WITH nMerchTax +  ROUND(lnQuantity * nSellNetPr,2) * nTaxRate/100 ,;
          Tax_Amt   WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
          Cod_Amt   WITH IIF(Cod_Flag='Y',IIF(llIsEngland,FabShipAmt+nCharges+;
                             Tax_Amt+Discount,Cod_Amt),0) ,; 
          TotalChg  WITH IIF(llIsEngland,FabShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
          nTaxDue   WITH m.nTaxDue
  REPLACE CARTONS   WITH m.Cartons ,;
          nCartons  WITH lnCartons ,;
          WEIGHT    WITH m.Weight  ,;
          FabSHIPAMT WITH m.FabShipAmt ,;
          FabSHIP      WITH m.FabShip    ,;
          Discount  WITH m.Discount,;
          lCompUps  WITH !llIsEngland ,;
          nMerchTax WITH nMerchTax +  (CEILING(lnQuantity * nSellNetPr*100)/100) * nTaxRate/100 ,;
          Tax_Amt   WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
          Cod_Amt   WITH IIF(Cod_Flag='Y',IIF(llIsEngland,FabShipAmt+nCharges+;
                             Tax_Amt+Discount,Cod_Amt),0) ,; 
          TotalChg  WITH IIF(llIsEngland,FabShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
          nTaxDue   WITH m.nTaxDue
  *E126684,1  TMI [End  ] 
  UNLOCK
  
  *B607226,1 ABD - Scatter the header field after remove the fabric line to get correct value to memeo fields. [Begin]
  SCATTER MEMVAR FIELDS &lcFlFields
  SCATTER FIELDS &lcScFields TO laData
  *B607226,1 ABD - [End]
  
  SELECT (lcInvLine)
  DELETE
  GO TOP
  SCATTER MEMVAR
  SHOW WINDOW (lcInvBrow) REFRESH SAME
  SHOW GETS WINDOW (lcWinCh42) ONLY
  SHOW GETS WINDOW (lcWinCh44) ONLY

  *N000388,1 Disable Sell Conversion. [Begin]
  DO lpChkRolBt
  SHOW GET m.nSellConv DISABLE
  *N000388,1 Disable Sell Conversion. [End]
  
  SHOW GETS WINDOW (lcWinCh45) ONLY
  STORE .T. TO llCUpdate

ENDIF

*!*************************************************************
*! Name      : lfUpCnsLine
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Update Consolidated invoice line with store invoice line changes
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfUpCnsLine()
*!*************************************************************
FUNCTION lfUpCnsLine
PARAMETERS llSelect
PRIVATE laLineInfo,lcFabCol,lcLineNo,lcTag
PRIVATE lnMerchTax
lcTag = ORDER(lcInvHdr)
SET ORDER TO TAG 'CONSOL' IN (lcInvHdr)
=SEEK('Y'+laData[4]+laData[13]+laData[14],lcInvHdr)
SELECT (lcInvLine)
SCATTER FIELDS FabQty,nSellNetPr,nSellPrice TO laLineInfo
lnMerchTax = 0

*N000388,1 Commented out. [Begin]
*IF laSetups[9,2]='Y' AND llIsEngland
*  FOR lnSize = 1 TO Scale.Cnt
*    lnMerchTax = lnMerchTax + IIF(BETWEEN(lnSize,Style.nTaxBreak,8),;
*    EVAL('m.QTY'+STR(lnSize,1))*m.Price-laLineInfo[lnSize]*laLineInfo[10],0)
*  ENDFOR
*ENDIF
*N000388,1 Commented out. [End]

*B607226,1 ABD - Accumulate merch tax . [Begin]
IF laSetups[9,2]='Y' AND llIsEngland
  lnMerchTax = lnMerchTax + laLineInfo[1] * laLineInfo[2] - m.FabQty * m.Price
ENDIF
*B607226,1 ABD - [End]

*E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
*lnTaxDue = &lcInvHdr..nTaxDue + IIF(lTaxable,ROUND(m.FabQty*m.nSellNetPr,2) - ROUND(laLineInfo[1]*laLineInfo[2],2),0)
lnTaxDue = &lcInvHdr..nTaxDue + IIF(lTaxable,CEILING(m.FabQty*m.nSellNetPr*100)/100-CEILING(laLineInfo[1]*laLineInfo[2]*100)/100,0)
*E126684,1  TMI [End  ] 
lcLineNo = STR(LineNo,6)
lcFabCol = Fabric + Color
SET ORDER TO TAG 'CONSOL'
IF !SEEK('Y'+laData[4]+laData[13]+laData[14]+lcFabCol)
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE LastLine WITH LastLine + 1
  UNLOCK
  INSERT INTO (lcInvLine) ;
  (Account,cDivision,cCurrCode,cMOrder,Fabric,Color,Season,Consol,cWareCode,LineNo,lNewLine) VALUES ;
  (laData[4],laData[13],laData[14],&lcInvHdr..CMOrder,SUBSTR(lcFabCol,1,7),SUBSTR(lcFabCol,8),m.Season,'Y',;
   laData[7],&lcInvHdr..LastLine,.T.)
ELSE
  *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
  *REPLACE FabQty     WITH FabQty - laLineInfo[1] + m.FabQty ,;
          nNetAmnt   WITH nNetAmnt - laLineInfo[1]*laLineInfo[2]+ROUND(m.FabQty*m.nSellNetPr,2),;
          nGrosAmnt  WITH nGrosAmnt- laLineInfo[1]*laLineInfo[3]+ROUND(m.FabQty*m.nSellPrice,2),;
          nSellNetPr WITH IIF(FabQty=0,0,nNetAmnt/FabQty) ,;
          nSellPrice WITH IIF(FabQty=0,0,nGrosAmnt/FabQty),;
          Disc_Pcnt  WITH IIF(nGrosAmnt=0,0,(nGrosAmnt-nNetAmnt)/nGrosAmnt)
  REPLACE FabQty     WITH FabQty - laLineInfo[1] + m.FabQty ,;
          nNetAmnt   WITH nNetAmnt - CEILING(laLineInfo[1]*laLineInfo[2]*100)/100+CEILING(m.FabQty*m.nSellNetPr*100)/100,;
          nGrosAmnt  WITH nGrosAmnt- CEILING(laLineInfo[1]*laLineInfo[3]*100)/100+CEILING(m.FabQty*m.nSellPrice*100)/100,;
          nSellNetPr WITH IIF(FabQty=0,0,nNetAmnt/FabQty) ,;
          nSellPrice WITH IIF(FabQty=0,0,nGrosAmnt/FabQty),;
          Disc_Pcnt  WITH IIF(nGrosAmnt=0,0,(nGrosAmnt-nNetAmnt)/nGrosAmnt)
  *E126684,1  TMI [End  ] 
  IF FabQty = 0 AND lNewLine
    DELETE
  ENDIF
ENDIF  
SELECT (lcInvLine)
SET ORDER TO TAG (lcInvLine)

IF lnActFolder = 3
  
  *N000388,1 Always force not to be end  of file. [Begin]
  IF EOF()
    LOCATE
  ENDIF  
  *N000388,1 Always force not to be end  of file. [End]
  
  SET KEY TO laData[4]+laData[1]+laData[5]+laData[3]
ELSE
  SET KEY TO
ENDIF

=SEEK(laData[4]+laData[1]+laData[5]+laData[3]+lcLineNo)
SELECT (lcInvHdr)
=RLOCK()

*N000388,1 we need to do it later
*REPLACE WEIGHT    WITH Weight+(m.FabQty-laLineInfo[1])*Style.nStyWeight
REPLACE WEIGHT WITH 0     && this line is dummy we have to remove it.
*N000388,1 we need to do it later

*E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
*REPLACE FabSHIPAMT WITH FabShipAmt+(ROUND(m.FabQty*m.nSellNetPr,2)-ROUND(laLineInfo[1]*laLineInfo[2],2)) ,;
        FabSHIP    WITH FabShip + (m.FabQty-laLineInfo[1]) ,;
        Discount  WITH -1*DiscPcnt*FabShipAmt/100 ,;
        lCompUps  WITH !llIsEngland ,; 
        nMerchTax WITH nMerchTax + lnMerchTax*m.nTaxRate/100 ,;
        Tax_Amt   WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
        Cod_Amt   WITH IIF(Cod_Flag='Y',IIF(llIsEngland,FabShipAmt+nCharges+;
                           Tax_Amt+Discount,Cod_Amt),0) ,; 
        TotalChg  WITH IIF(llIsEngland,FabShipAmt+nCharges+Tax_Amt+Discount,TotalChg),;
        nTaxDue   WITH lnTaxDue ,;
        cSelect   WITH IIF(llSelect,IIF(FabSHIP=0,'','»'),cSelect)
REPLACE FabSHIPAMT WITH FabShipAmt+CEILING(m.FabQty*m.nSellNetPr*100)/100 - CEILING(laLineInfo[1]*laLineInfo[2]*100)/100 ,;
        FabSHIP    WITH FabShip + (m.FabQty-laLineInfo[1]) ,;
        Discount  WITH -1*DiscPcnt*FabShipAmt/100 ,;
        lCompUps  WITH !llIsEngland ,; 
        nMerchTax WITH nMerchTax + lnMerchTax*m.nTaxRate/100 ,;
        Tax_Amt   WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
        Cod_Amt   WITH IIF(Cod_Flag='Y',IIF(llIsEngland,FabShipAmt+nCharges+;
                           Tax_Amt+Discount,Cod_Amt),0) ,; 
        TotalChg  WITH IIF(llIsEngland,FabShipAmt+nCharges+Tax_Amt+Discount,TotalChg),;
        nTaxDue   WITH lnTaxDue ,;
        cSelect   WITH IIF(llSelect,IIF(FabSHIP=0,'','»'),cSelect)
*E126684,1  TMI [End  ] 
UNLOCK
SET ORDER TO TAG (lcInvHdr)
=SEEK(laData[4]+laData[1]+laData[5]+laData[3])
SET ORDER TO TAG (lcTag)
SELECT (lcInvLine)

*!*************************************************************
*! Name      : lfUpCnsCtn
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Update Consolidated invoice Cartons
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lnOldCrtn : Invoice Old Cartons
*!             lnNewCrtn : Invoice New Cartons
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfUpCnsCtn()
*!*************************************************************
FUNCTION lfUpCnsCtn
PARAMETERS lnOldCrtn,lnNewCrtn
PRIVATE lnAlias,lctag

lnAlias = SELECT()
SELECT (lcInvHdr)
lcTag = ORDER()
SET ORDER TO TAG 'CONSOL'
=SEEK('Y'+laData[4]+laData[13]+laData[14])
=RLOCK()
REPLACE CARTONS WITH CARTONS - lnOldCrtn + lnNewCrtn
UNLOCK
SET ORDER TO TAG (lcInvHdr)
=SEEK(laData[4]+laData[1]+laData[5]+laData[3])
SET ORDER TO TAG (lcTag)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfFabDye
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Check if invoice style has dyelots in the selected warehouse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfFabDye()
*!*************************************************************
FUNCTION lfFabDye
PRIVATE laDyelots

IF SEEK (m.Fabric+m.Color+laData[7]+SPACE(10),'FabDye')
  SKIP IN FabDye
  RETURN (FabDye.Fabric+FabDye.Color+FabDye.cWareCode = m.Fabric+m.Color+laData[7] AND !EMPTY(FabDye.Dyelot))
ELSE
  RETURN .F.
ENDIF
*-- End of lfFabDye.

*!*************************************************************
*! Name      : lfvDyelot
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate entered dyelot
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvDyelot()
*!*************************************************************
FUNCTION lfvDyelot
PRIVATE lcDyelot

lcDyelot = m.Dyelot
IF (llBrowse .OR. EMPTY(lcDyelot) .OR. !SEEK(m.Fabric+m.Color+laData[7]+lcDyelot,'FabDye')) AND ;
  !FDYEBROW(m.Fabric,m.Color,@lcDyelot,.T.,IIF(laSetups[5,2]='Y',laData[7],''))
  lcDyelot = Dyelot  
ENDIF
m.Dyelot = lcDyelot
llBrowse = .F.
=gfStatic()
IF Dyelot <> m.Dyelot
  IF !EMPTY(m.Dyelot)
    =RLOCK()
    REPLACE Dyelot WITH m.Dyelot
    UNLOCK
    
    *-* IF INLIST(laSetups[7,2],'F','I','L')
    *-*   IF m.FabQty > 0 AND m.FabQty*m.nSellConv > FabDye.OnHand
    *-*     *-- Message : 04025
    *-*     *-- Shipped Quantity cannot be greater than On Hand Quantity.
    *-*     *-- Quantity (99999).
    *-*     *-- Button : 00000 
    *-*     *-- OK        
    *-*     =gfModalGen('TRM04025B00000','ALERT','Shipped Quantity'+'|'+'On Hand Quantity.')        
    *-*     *STORE MAX(EVAL('StyDye.Stk'+STR(lnCount,1)),0) TO ('m.Qty'+STR(lnCount,1))
    *-*     STORE MAX(FabDye.OnHand,0) / IIF(m.nSellConv=0,1,m.nSellConv) TO m.FabQty
    *-*     SHOW GET m.FabQty
    *-*   ENDIF
    *-* ENDIF
    *-* =lfChkTotQty()
  ENDIF
  llCUpdate = .T.
  SHOW WINDOW (lcInvBrow) REFRESH SAME
  
  *N000388,1 Refresh the Rolls button. [Begin]
  =lfwBrow()
  *N000388,1 Refresh the Rolls button. [End]
  
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvStyGrp
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style/color group
*!*************************************************************
*! Calls     : lfNavigate
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyGrp()
*!*************************************************************
FUNCTION lfvStyGrp

IF lfNavigate()
  _CUROBJ = _CUROBJ
  RETURN
ENDIF  
IF Group <> m.Group
  llCUpdate = .T.
  =RLOCK()
  REPLACE Group WITH m.Group
  UNLOCK
  SHOW WINDOW (lcInvBrow) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvLineNote
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Invoice line notepad
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
DO (gcScrDir+"ARLNOTES.SPX")

*!*************************************************************
*! Name      : lfvShipInv
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Ship/Unship INvoice
*!*************************************************************
*! Calls     : lfUpCnsLine
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvShipInv()
*!*************************************************************
FUNCTION lfvShipInv

PARAMETERS llSelect ,lcAct
PRIVATE llShip,lcSelect,lcAct
IF Consol = 'Y'
  SET ORDER TO TAG 'CONSOL'
  lcConsKey  = Account+cDivision+cCurrCode
  llConsShip = (m.FabShip=0)
  lcConsSel  = cSelect
  =SEEK('N'+lcConsKey)
  SCAN REST WHILE Consol+Account+cDivision+cCurrCode= 'N'+lcConsKey
    REPLACE cSelect WITH IIF(llSelect,lcConsSel,cSelect)
    IF IIF(llConsShip,FabSHIP=0,FabSHIP>0)
      SCATTER FIELDS &lcScFields TO laData
      SCATTER MEMVAR FIELDS &lcFlFields
      llUpCnsLine = .T.
      =lfvShipInv()
    ENDIF  
  ENDSCAN
  =SEEK('Y'+lcConsKey)
  SET ORDER TO TAG (lcInvHdr)
  RETURN
ENDIF

llShip = (m.FabShip=0)

IF !(TYPE('lcAct')$'UL')
  DO CASE 
    CASE lcAct $ 'AN'
      llShip=IIF(lcAct='A',.T.,.F.)
    CASE lcAct ='V'
      llShip=!EMPTY(&lcInvHdr..cSelect)
  ENDCASE
ENDIF      
SELECT (lcInvLine)
=SEEK(laData[4]+laData[1]+laData[5]+laData[3])
STORE 0 TO m.nMerchTax,lnCartons,m.Cartons,m.Weight,m.FabShipAmt,m.FabShip,m.Discount,m.nTaxdue
SCAN REST WHILE Account+CMOrder+Store+PikTkt = laData[4]+laData[1]+laData[5]+laData[3]

  IF SEEK('O'+CMOrder+STR(LineNO,6),lcOrdCanLn)
    SELECT (lcOrdCanLn)
    BLANK
    DELETE
    SELECT (lcInvLine)
    m.lBackOrd = .T.
  ENDIF  
  STORE IIF(llShip,FabBook,0) TO m.FabQty
  
  IF INLIST(laSetups[7,2],'F','I','L') AND llShip
    STORE MAX(MIN(m.FabQty*m.nSellConv,FabDye.OnHand),0) / IIF(m.nSellConv=0,1,m.nSellConv) TO m.FabQty
  ENDIF
  SCATTER MEMVAR FIELDS nSellNetPr,nSellPrice,nTaxRate,Season,lBackOrd
  =IIF(llUpCnsLine,lfUpCnsLine(llSelect),.T.)
  IF llShip
    
    *N000388,1 Commented out. [Begin]
    *IF laSetups[9,2]='Y' AND llIsEngland
    *  lnTaxQty = 0
    *  FOR lnSize = 1 TO Scale.Cnt
    *    lnTaxQty = lnTaxQty + IIF(BETWEEN(lnSize,Style.nTaxBreak,8),;
    *                              EVAL('m.Qty'+STR(lnSize,1)),0)
    *  ENDFOR
    *  m.nMerchTax = m.nMerchTax + lnTaxQty*nSellNetPr*nTaxRate/100
    *ENDIF
    *N000388,1 Commented out. [End]
    
    *B607226,1 ABD - Accumulate merch tax . [Begin]
    IF llIsEngland .AND. laSetups[9,2]='Y'
      lnTaxQty = 0
      lnTaxQty = lnTaxQty + M.FabQty 
      m.nMerchTax = m.nMerchTax + lnTaxQty*nSellNetPr*nTaxRate/100
    ENDIF
    *B607226,1 ABD - [End]
    
    
    *N000388,1 we need to do it later
    lnCartons = 1         && this record is dummy we have to remove it 
    m.Weight  = 0         && this record is dummy we have to remove it 
    *lnCartons = lnCartons + IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0)
    *m.Weight  = m.Weight  + m.TotQty*Style.nStyWeight
    *N000388,1 we need to do it later
    
    *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
    *m.FabShipAmt = m.FabShipAmt + ROUND(m.FabQty * nSellNetPr,2)
    m.FabShipAmt = m.FabShipAmt + CEILING(m.FabQty * nSellNetPr*100)/100
    *E126684,1  TMI [End  ] 
    m.FabShip    = m.FabShip    + m.FabQty
    *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
    *m.nTaxdue = m.nTaxdue + IIF(lTaxable,ROUND(m.FabQty*nSellNetPr,2),0)
    m.nTaxdue = m.nTaxdue + IIF(lTaxable,CEILING(m.FabQty*nSellNetPr*100)/100,0)
    *E126684,1  TMI [End  ] 
  ENDIF
  =RLOCK()
  GATHER MEMVAR FIELDS FabQty,lBackOrd
  UNLOCK
ENDSCAN
=SEEK(laData[4]+laData[1]+laData[5]+laData[3])
lnCartons = IIF(llShip AND !EMPTY(m.PikTkt) AND SEEK(m.cMOrder+m.Store+m.PikTkt,'Pack_Hdr'),Pack_Hdr.Tot_Cart,lnCartons)

*N000388,1 SSE Commented out. [Begin]
*IF llShip
*  m.Cartons = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
*ELSE
*  m.Cartons = CEILING(lnCartons)
*ENDIF  
*N000388,1 SSE Commented out. [End]

=IIF(llUpCnsLine,lfUpCnsCtn(&lcInvHdr..Cartons,m.Cartons),.T.)
SELECT (lcInvHdr)
=RLOCK()
REPLACE CARTONS   WITH m.Cartons    ,;
        nCartons  WITH lnCartons    ,;
        WEIGHT    WITH m.Weight     ,;
        FabSHIPAMT   WITH m.FabShipAmt    ,;
        FabSHIP      WITH m.FabShip       ,;
        Discount  WITH -FabShipAmt * DiscPcnt/100,;
        lCompUps  WITH !llIsEngland ,;
        nMerchTax WITH m.nMerchTax  ,;
        Tax_Amt   WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
        Cod_Amt   WITH IIF(Cod_Flag='Y',IIF(llIsEngland,FabShipAmt+nCharges+;
                           Tax_Amt+Discount,Cod_Amt),0) ,; 
        TotalChg  WITH IIF(llIsEngland,FabShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
        nTaxdue   WITH m.nTaxdue
UNLOCK
SCATTER MEMVAR FIELDS &lcFlFields
IF m.FabShip = 0
  SHOW GET pbShipInv,1 ENABLE PROMPT 'S\<hip'
ELSE
  SHOW GET pbShipInv,1 ENABLE PROMPT 'Uns\<hip'
ENDIF

*!*************************************************************
*! Name      : lfvShipLine
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Ship/UnShip Invoice Line
*!*************************************************************
*! Calls     : lfRefresh(),lfUpCnsLine()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvShipLine()
*!*************************************************************
FUNCTION lfvShipLine
PRIVATE lnTaxQty,lcInvKey

SELECT (lcInvLine)
lcInvKey=Account+cMOrder+Store+PikTkt+STR(LineNo,6)

SET KEY TO

IF SEEK('O'+cMOrder+STR(LineNO,6),lcOrdCanLn)
  SELECT (lcOrdCanLn)
  BLANK
  DELETE
  SELECT (lcInvLine)
  m.lBackOrd = .T.
  llCnRemain = .F.
  SHOW GET llCnRemain DISABLE
  SHOW GET pbCnRemain DISABLE
ENDIF

SELECT (lcInvLine)
m.FabQty = IIF(FabQty=0,FabBook,0)
IF INLIST(laSetups[7,2],'F','I','L') AND FabQty=0
  STORE MAX(MIN(m.FabQty*m.nSellConv,FabDye.onHand),0) / IIF(m.nSellConv=0,1,m.nSellConv) TO m.FabQty
ENDIF
STORE nSellNetPr TO m.nSellNetPr
STORE lBackOrd TO m.lBackOrd

*N000388,1 we need to ask
lnCartons  = &lcInvHdr..nCartons + 0      && this line is dummy
m.Weight   = m.Weight  + 0
*lnCartons  = &lcInvHdr..nCartons + IIF(Style.Qty_Ctn>0,(m.TotQty-TotQty)/Style.Qty_Ctn,0)
*m.Weight   = m.Weight  + (m.TotQty-TotQty)*Style.nStyWeight

IF m.FabQty=0
  m.Cartons = CEILING(lnCartons)
ELSE
  m.Cartons = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
ENDIF    

*E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
*m.FabShipAmt  = m.FabShipAmt + ROUND((m.FabQty - FabQty)*nSellNetPr,2)
m.FabShipAmt  = m.FabShipAmt + CEILING(m.FabQty*nSellNetPr*100)/100 - CEILING(FabQty*nSellNetPr*100)/100
*E126684,1  TMI [End  ] 
m.FabShip     = m.FabShip    + (m.FabQty - FabQty)
m.Discount = -m.FabShipAmt * m.DiscPcnt/100
=IIF(llUpCnsLine,lfUpCnsLine() AND lfUpCnsCtn(&lcInvHdr..Cartons,m.Cartons),.T.)
lnTaxQty = 0

*N000388,1 Commented out. [Begin]
*IF laSetups[9,2]='Y' AND llIsEngland
*  FOR lnSize = 1 TO Scale.Cnt
*    lnTaxQty = lnTaxQty + IIF(BETWEEN(lnSize,Style.nTaxBreak,8),;
*    IIF(TotQty=0,EVAL('m.Qty'+STR(lnSize,1)),EVAL('-Qty'+STR(lnSize,1))),0)
*  ENDFOR
*ENDIF
*N000388,1 Commented out. [End]

*B607226,1 ABD - Accumulate total tax qty. [Begin]
IF llIsEngland .AND. laSetups[9,2]='Y'
  lnTaxQty = lnTaxQty + (m.FabQty - FabQty)
ENDIF
*B607226,1 ABD - [End]


*E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
*m.nTaxdue = &lcInvHdr..nTaxDue + IIF(lTaxable,ROUND((m.FabQty-FabQty)*nSellNetPr,2),0)
m.nTaxdue = &lcInvHdr..nTaxDue + IIF(lTaxable,CEILING(m.FabQty*nSellNetPr*100)/100 - CEILING(FabQty*nSellNetPr*100)/100,0)
*E126684,1  TMI [End  ] 
=RLOCK()
GATHER MEMVAR FIELDS FabQty,lBackOrd
UNLOCK
STORE .T. TO llCUpdate
SELECT (lcInvHdr)
=RLOCK()
*E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
*REPLACE CARTONS    WITH m.Cartons    ,;
        nCartons   WITH lnCartons    ,;
        WEIGHT     WITH m.Weight     ,;
        FabSHIPAMT WITH m.FabShipAmt    ,;
        FabSHIP    WITH m.FabShip       ,;
        Discount   WITH m.Discount   ,;
        lCompUps   WITH !llIsEngland ,;
        nMerchTax  WITH nMerchTax +   ;
        IIF(laSetups[9,2]='Y' AND llIsEngland,ROUND(lnTaxQty*m.nSellNetPr*m.nTaxRate,2)/100,0) ,;
        Tax_Amt    WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
        Cod_Amt    WITH IIF(Cod_Flag='Y',IIF(llIsEngland,FabShipAmt+nCharges+;
                           Tax_Amt+Discount,Cod_Amt),0) ,; 
        TotalChg   WITH IIF(llIsEngland,FabShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
        nTaxdue    WITH m.nTaxdue
REPLACE CARTONS    WITH m.Cartons    ,;
        nCartons   WITH lnCartons    ,;
        WEIGHT     WITH m.Weight     ,;
        FabSHIPAMT WITH m.FabShipAmt    ,;
        FabSHIP    WITH m.FabShip       ,;
        Discount   WITH m.Discount   ,;
        lCompUps   WITH !llIsEngland ,;
        nMerchTax  WITH nMerchTax +   ;
        IIF(laSetups[9,2]='Y' AND llIsEngland,ROUND( (CEILING(lnTaxQty*m.nSellNetPr*100)/100) *m.nTaxRate,2)/100,0) ,;
        Tax_Amt    WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
        Cod_Amt    WITH IIF(Cod_Flag='Y',IIF(llIsEngland,FabShipAmt+nCharges+;
                           Tax_Amt+Discount,Cod_Amt),0) ,; 
        TotalChg   WITH IIF(llIsEngland,FabShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
        nTaxdue    WITH m.nTaxdue
*E126684,1  TMI [End  ] 
UNLOCK
SCATTER MEMVAR FIELDS Tax_Amt,Cod_Amt,TotalChg
SELECT (lcInvLine)

*N000388,1 Always force not to be end  of file. [Begin]
IF EOF()
  LOCATE
ENDIF  
*N000388,1 Always force not to be end  of file. [End]

SET KEY TO laData[4]+laData[1]+laData[5]+laData[3]
=SEEK(lcInvKey)
SHOW GETS WINDOW (lcWinCh42) ONLY
SHOW GETS WINDOW (lcWinCh44) ONLY

*N000388,1 Disable Sell Conversion. [Begin]
DO lpChkRolBt
SHOW GET m.nSellConv DISABLE
*N000388,1 Disable Sell Conversion. [End]

SHOW GETS WINDOW (lcWinCh45) ONLY
=lfRefresh(lcWinCh44)
IF m.FabQty = 0
  SHOW GET pbShipLine,1 ENABLE PROMPT '\<Ship'
ELSE
  SHOW GET pbShipLine,1 ENABLE PROMPT '\<Clear'
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvCopyQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Copy Invoice line Quantity
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
SCATTER FIELDS FabQty TO laQtyPaste

*!*************************************************************
*! Name      : FUNCTION lfvPasteQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Paste Invoice Line Quantity
*!*************************************************************
*! Calls     : gfModalGen(),lfUpCnsLine(),lfRefresh()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPasteQty()
*!*************************************************************
FUNCTION lfvPasteQty

IF laQtyPaste[1] = 0
  *-- Message : 40016
  *--  No line quantities copied to paste!
  *--  Button : 00000 
  *--  ok
  =gfModalGen('INM40016B00000','ALERT')
  RETURN
ENDIF
STORE 0 TO m.FabQty
m.FabQty = laQtyPaste[1]
IF !lfChkTotQty()
  SCATTER MEMVAR FIELDS FabQty
  RETURN
ENDIF
SHOW GETS WINDOW (lcWinCh44) ONLY
=lfRefresh(lcWinCh44)

*N000388,1 Disable Sell Conversion. [Begin]
DO lpChkRolBt
SHOW GET m.nSellConv DISABLE
*N000388,1 Disable Sell Conversion. [End]

SHOW WINDOW (lcInvBrow) REFRESH SAME
_CUROBJ=OBJNUM(m.FabQty)

*!*************************************************************
*! Name      : FUNCTION lfNavigate
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Navigate invoice lines while editing fields.
*!*************************************************************
*! Calls     : lfwBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfNavigate()
*!*************************************************************
FUNCTION lfNavigate
SELECT (lcInvLine)
lnLastKey = LASTKEY()
DO CASE
  CASE MDOWN()
  CASE lnLastKey=1
    =SEEK(laData[4]+laData[1]+laData[5]+laData[3])
    =lfwBrow()
    RETURN
  CASE lnLastKey=6
    GO bottom
    =lfwBrow()
    RETURN
  CASE lnLastKey=5
    SKIP -1
    IF Account+cMOrder+Store+PikTkt <> laData[4]+laData[1]+laData[5]+laData[3] OR BOF()
      =SEEK(laData[4]+laData[1]+laData[5]+laData[3])
    ELSE 
      =lfwBrow()
    ENDIF
    RETURN
  CASE lnLastKey=24
    SKIP
    IF Account+cMOrder+Store+PikTkt <> laData[4]+laData[1]+laData[5]+laData[3] OR EOF()
      SKIP -1
    ELSE 
      =lfwBrow()
    ENDIF
    RETURN
ENDCASE
RETURN(.F.)

*!*************************************************************
*! Name      : lfvSizeQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Size Shipped Quantity
*!*************************************************************
*! Calls     : lfNavigate(),lfRefresh()
*!*************************************************************
*! Parameters: lnSize   : Size 
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvSizeQty()
*!*************************************************************
FUNCTION lfvSizeQty
PARAMETERS lcSize

IF m.FabQty < 0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Buttfon  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.FabQty = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF lfNavigate()
  _CUROBJ = _CUROBJ 
  RETURN
ENDIF
=lfRefresh(lcWinCh44)

*N000388,1 Refresh the browse. [Begin]
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT (lcInvLine)
*B606163,1 ABD - [Begin]
*-- this function to update the lotas in case this fabric is 
*-- lots and cancel the remming Qty after apply the lots qty.
IF m.FabQty = 0
  = lfUpdLots ()
ENDIF  
*B606163,1 ABD - [End]

=RLOCK()
REPLACE FabQty WITH m.FabQty
UNLOCK
=lfwBrow()
SELECT (lcAlias)
*N000388,1 Refresh the browse. [End]

*!*************************************************************
*! Name      : FUNCTION lfChkTotQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Check total pieces againest total quantity entered
*!*************************************************************
*! Calls     : gfModalGen(),gfwCodePop(),lfUpCnsLine(),lfRefresh()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfChkTotQty()
*!*************************************************************
FUNCTION lfChkTotQty

IF FabQty <> m.FabQty 
  *-* IF INLIST(laSetups[7,2],'F','I','L')
  *-*   IF m.FabQty*m.nSellConv > FabDye.OnHand
  *-*     *-- Message : 04025
  *-*     *-- Shipped Quantity cannot be greater than On Hand Quantity.
  *-*     *-- Quantity (99999).
  *-*     *-- Button : 00000 
  *-*     *-- OK
  *-*     =gfModalGen('TRM04025B00000','ALERT','Shipped Quantity'+'|'+'On Hand Quantity.')        
  *-*     *STORE MAX(EVAL('StyDye.Stk'+STR(lnCount,1)),0) TO ('m.Qty'+STR(lnCount,1))
  *-*     STORE MAX(FabDye.OnHand,0) / IIF(m.nSellConv=0,1,m.nSellConv) TO m.FabQty
  *-* 
  *-*     RETURN(.F.)
  *-*   ENDIF
  *-* ENDIF
  
  IF !m.lNewLine
    SELECT (lcOrdCanLn)
    m.lBackOrd =.T.

    *-- lcChkBkOrd : Empty variable overrided by "I" or "N" according to 
    *--            : Order Header Allow back order flag. 
    lcChkBkOrd = " "
    IF m.FabBook > m.FabQty
        
      IF   (lcChkBkOrd="N" OR (EMPTY(lcChkBkOrd) AND lcBackOrd='N')) OR  ;
           ((lcChkBkOrd="I" OR (EMPTY(lcChkBkOrd) AND lcBackOrd='I')) AND ;           
           gfModalGen('QRM40154B40000','ALERT')=1)
        m.lBackOrd =.F.    
      ENDIF
    ENDIF        

    IF !m.lBackOrd
      IF !SEEK('O'+&lcInvLine..cMOrder+STR(&lcInvLine..LineNO,6))
        =gfwCodePop(@laCodes,'CCANCRESON','D')
        INSERT INTO (lcOrdCanLn) (cOrdType,cMOrder,LineNo,cCancReson,Cancelled,nSellNetPr,nSellPrice,Fabric,Color,Dyelot,Store) VALUES ;
        ('O',m.cMOrder,m.LineNo,laCanReason[lnCanReason,2],gdSysDate,lfUpdPrice(m.cMOrder,m.LineNo),lfUpdGrPrc(m.cMOrder,m.LineNo),m.Fabric,m.Color,m.Dyelot,m.Store)
      ENDIF
      =RLOCK()
      REPLACE FabQty WITH MAX(m.FabBook - m.FabQty , 0)
      UNLOCK
      m.lBackOrd = .F.
      SHOW GET pbCnRemain ENABLE
    ELSE
      IF SEEK('O'+&lcInvLine..cMOrder+STR(&lcInvLine..LineNO,6))
        DELETE
      ENDIF
      m.lBackOrd = .T.
      SHOW GET pbCnRemain DISABLE
    ENDIF
    llCnRemain = !m.lBackOrd

    IF (lcChkBkOrd="I" OR (EMPTY(lcChkBkOrd) AND lcBackOrd='I')) AND m.FabBook > m.FabQty
      SHOW GET llCnRemain ENABLE
    ELSE
      SHOW GET llCnRemain DISABLE
    ENDIF    
    SELECT (lcInvLine)
  ENDIF
  IF m.lNewLine AND lcPriceLvl='Q'
    *m.nSellPrice  = lfGetprice(m.Style,lcPriceLvl,m.FabQty)
    m.nSellNetPr = m.nSellPrice*(100-m.Disc_Pcnt)/100
  ENDIF

  STORE .T. TO llCUpdate
  SELECT (lcInvLine)

  *N000388,1 we need to ask  
  lnCartons  = &lcInvHdr..nCartons  - 0      && this lines are dummy
  m.Weight   = m.Weight  - 0                 && this lines are dummy
  *lnCartons  = &lcInvHdr..nCartons  - IIF(Style.Qty_Ctn>0,(TotQty-m.TotQty)/Style.Qty_Ctn,0)
  *m.Weight   = m.Weight  - (TotQty-m.TotQty)*Style.nStyWeight
  *N000388,1 we need to ask    

  IF m.FabQty =0
    m.Cartons = CEILING(lnCartons)
  ELSE
    m.Cartons = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
  ENDIF    

  *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
  *m.FabShipAmt  = m.FabShipAmt - ROUND(FabQty * nSellNetPr,2) + ROUND(m.FabQty*m.nSellNetPr,2)
  m.FabShipAmt  = m.FabShipAmt - CEILING(FabQty * nSellNetPr *100)/100 + CEILING(m.FabQty*m.nSellNetPr*100)/100
  *E126684,1  TMI [End  ] 
  m.FabShip     = m.FabShip - FabQty+m.FabQty
  m.Discount =-1*m.DiscPcnt*m.FabShipAmt/100  
  lnMerchTax = &lcInvHdr..nMerchTax  
  
  *N000388,1 Commented out. [Begin]
  *IF laSetups[9,2]='Y' AND llIsEngland
  *  FOR lnSize = 1 TO SCALE.CNT
  *    IF BETWEEN(lnSize,Style.nTaxBreak,8)
  *      lcSize = STR(lnSize,1)
  *      lnMerchTax = lnMerchTax + (ROUND(m.Qty&lcSize*m.nSellNetPr,2)-ROUND(Qty&lcSize*nSellNetPr,2))*m.nTaxRate/100
  *    ENDIF
  *  ENDFOR
  *ENDIF
  *N000388,1 Commented out. [End]
  
  *B607226,1 ABD - Accumulate merch tax . [Begin]
  IF laSetups[9,2]='Y' AND llIsEngland
    *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
    *lnMerchTax = lnMerchTax + (ROUND(M.FabQty*m.nSellNetPr,2)-ROUND(FabQty*nSellNetPr,2))*m.nTaxRate/100
    lnMerchTax = lnMerchTax + ( CEILING(M.FabQty*m.nSellNetPr*100)/100 - CEILING(FabQty*nSellNetPr*100)/100 ) * m.nTaxRate/100
    *E126684,1  TMI [End  ] 
  ENDIF
  *B607226,1 ABD - [End]
  
  *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
  *m.nTaxDue = &lcInvHdr..nTaxDue + ;
              IIF(lTaxable,ROUND(m.FabQty*m.nSellNetPr,2)-ROUND(FabQty*nSellNetPr,2),0)
  m.nTaxDue = &lcInvHdr..nTaxDue + ;
              IIF(lTaxable,CEILING(m.FabQty*m.nSellNetPr*100)/100-CEILING(FabQty*nSellNetPr*100)/100,0)
  *E126684,1  TMI [End  ] 
  =IIF(llUpCnsLine,lfUpCnsLine() AND lfUpCnsCtn(&lcInvHdr..Cartons,m.Cartons),.T.)
  SELECT (lcInvHdr)  
  =RLOCK()
  REPLACE CARTONS   WITH m.Cartons    ,;
          nCartons  WITH lnCartons    ,;
          WEIGHT    WITH m.Weight     ,;
          FabSHIPAMT   WITH m.FabShipAmt    ,;
          FabSHIP      WITH m.FabShip       ,;
          Discount  WITH m.Discount   ,;
          lCompUps  WITH !llIsEngland ,;
          nMerchTax WITH lnMerchTax   ,;
          Tax_Amt   WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
          Cod_Amt   WITH IIF(Cod_Flag='Y',IIF(llIsEngland,FabShipAmt+nCharges+;
                             Tax_Amt+Discount,Cod_Amt),0) ,; 
          TotalChg  WITH IIF(llIsEngland,FabShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
          nTaxDue   WITH m.nTaxDue   
  UNLOCK
  SCATTER MEMVAR FIELDS Tax_Amt,Cod_Amt,TotalChg
  SELECT (lcInvLine)
  =RLOCK()
  GATHER MEMVAR FIELDS FabQty,nSellPrice,nSellNetPr,lBackOrd
  UNLOCK
  =lfRefresh(lcWinCh44)
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvGPrice
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice line Gross price
*!*************************************************************
*! Calls     : lfvNPrice
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvGPrice()
*!*************************************************************
FUNCTION lfvGPrice

IF m.nSellPrice < 0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.nSellPrice = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF m.nSellPrice <> &lcInvLine..nSellPrice
  *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
  *m.nSellNetPr = ROUND(m.nSellPrice*(100-m.Disc_Pcnt)/100,2)
  SET DECIMALS TO 5
  m.nSellNetPr = CEILING( 100000 * (m.nSellPrice*(100-m.Disc_Pcnt)/100) ) / 100000
  *E126684,1  TMI [End  ] 
  =lfvNPrice()
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvPrcDisc
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice line price discount
*!*************************************************************
*! Calls     : lfvNPrice
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPrcDisc()
*!*************************************************************
FUNCTION lfvPrcDisc

IF m.Disc_Pcnt <> &lcInvLine..Disc_Pcnt
  *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
  *m.nSellNetPr = ROUND(m.nSellPrice*(100-m.Disc_Pcnt)/100,2) 
  SET DECIMALS TO 5
  m.nSellNetPr = CEILING( 100000 * (m.nSellPrice*(100-m.Disc_Pcnt)/100) ) / 100000
  *E126684,1  TMI [End  ] 
  =lfvNPrice(.T.)
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvNPrice
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice line net price
*!*************************************************************
*! Calls     : lfNavigate,lfUpCnsLine
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvNPrice()
*!*************************************************************
FUNCTION lfvNPrice
PARAMETERS llPercent

PRIVATE lnTaxQty

IF m.nSellNetPr < 0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.nSellNetPr = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF lfNavigate()
  _CUROBJ = _CUROBJ
  RETURN
ENDIF  
SELECT (lcInvLine)
IF nSellNetPr <> m.nSellNetPr
  STORE .T. TO llCUpdate
  *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
  *m.FabShipAmt = m.FabShipAmt - ROUND(m.FabQty*(nSellNetPr-m.nSellNetPr),2)
  m.FabShipAmt = m.FabShipAmt - CEILING(m.FabQty*nSellNetPr*100)/100 + CEILING(m.FabQty*m.nSellNetPr*100)/100
  *E126684,1  TMI [End  ] 
      
  lnTaxQty = 0
  
  *N000388,1 Commented out. [Begin]
  *IF laSetups[9,2]='Y' AND llIsEngland
  *  FOR lnSize = 1 TO Scale.Cnt
  *    lnTaxQty = lnTaxQty + IIF(BETWEEN(lnSize,Style.nTaxBreak,8),;
  *    EVAL('Qty'+STR(lnSize,1)),0)
  *  ENDFOR
  *ENDIF
  *N000388,1 Commented out. [End]

  *B607226,1 ABD - Accumulate merch total tax qty. [Begin]
  IF laSetups[9,2]='Y' AND llIsEngland
    lnTaxQty = lnTaxQty + m.FabQty
  ENDIF
  *B607226,1 ABD - [End]
  
  =IIF(llUpCnsLine,lfUpCnsLine() AND lfUpCnsCtn(&lcInvHdr..Cartons,m.Cartons),.T.)
  SELECT (lcInvHdr)  
  =RLOCK()
  
  *N000388,1 Replace the new discount. [Begin]
  REPLACE Discount WITH -1*m.FabShipAmt*m.DiscPcnt/100
  *N000388,1 Replace the new discount. [End]

  *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
  *REPLACE FabShipAmt   WITH m.FabShipAmt    ,;
          lCompUps  WITH !llIsEngland ,;
          nMerchTax WITH nMerchTax + ROUND(lnTaxQty*(m.nSellNetPr-&lcInvLine..nSellNetPr)*m.nTaxRate,2)/100 ,;
          Tax_Amt   WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
          Cod_Amt   WITH IIF(Cod_Flag='Y',IIF(llIsEngland,FabShipAmt+nCharges+;
                             Tax_Amt+Discount,Cod_Amt),0) ,; 
          TotalChg  WITH IIF(llIsEngland,FabShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
          nTaxDue   WITH nTaxDue + IIF(&lcInvLine..lTaxable,ROUND(m.FabQty*(m.nSellNetPr-&lcInvLine..nSellNetPr),2),0)
  REPLACE FabShipAmt   WITH m.FabShipAmt    ,;
          lCompUps  WITH !llIsEngland ,;
          nMerchTax WITH nMerchTax + ROUND((CEILING(lnTaxQty*m.nSellNetPr*100)/100-CEILING(lnTaxQty*&lcInvLine..nSellNetPr*100)/100)*m.nTaxRate,2)/100 ,;
          Tax_Amt   WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
          Cod_Amt   WITH IIF(Cod_Flag='Y',IIF(llIsEngland,FabShipAmt+nCharges+;
                             Tax_Amt+Discount,Cod_Amt),0) ,; 
          TotalChg  WITH IIF(llIsEngland,FabShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
          nTaxDue   WITH nTaxDue + IIF(&lcInvLine..lTaxable,CEILING(m.FabQty*m.nSellNetPr*100)/100-CEILING(m.FabQty*&lcInvLine..nSellNetPr*100)/100,0)
  *E126684,1  TMI [End  ] 
  UNLOCK
  
  m.Tax_Amt = Tax_Amt
  m.Discount = -1*m.FabShipAmt*m.DiscPcnt/100
  m.TotalChg = FabShipAmt+Freight+Insur+COD+Discount+Tax_Amt+nPstAmt+IIF(lnPstRule=2,nHstAmt,0)
  m.Disc_Pcnt = IIF(llPercent,m.Disc_Pcnt,IIF(m.nSellPrice=0,0,100-m.nSellNetPr*100/m.nSellPrice))

  SELECT (lcInvLine)
  =RLOCK()
  REPLACE nSellPrice WITH m.nSellPrice ,;
          Disc_Pcnt  WITH m.Disc_Pcnt ,;
          nSellNetPr WITH m.nSellNetPr
  UNLOCK
  SHOW GET m.Disc_Pcnt
  SHOW GET m.nSellNetPr
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvComm
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style salesrep commissions
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
PARAMETERS lnSalesRep
PRIVATE lcSalesRep

SELECT (lcInvLine)
lcSalesRep = STR(lnSalesRep,1)
IF Comm&lcSalesRep <> m.Comm&lcSalesRep
  llCUpdate = .T.
  REPLACE Comm&lcSalesRep WITH m.Comm&lcSalesRep
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvBackOrd
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate BackOrder field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvBackOrd()
*!*************************************************************
FUNCTION lfvBackOrd

IF !m.lNewLine
  SELECT (lcOrdCanLn)
  IF llCnRemain
    IF !SEEK('O'+&lcInvLine..cMOrder+STR(&lcInvLine..LineNO,6))
      =gfwCodePop(@laCodes,'CCANCRESON','D')
      
      INSERT INTO (lcOrdCanLn) (cOrdType,cMOrder,LineNo,cCancReson,Cancelled,nSellNetPr,nSellPrice,Fabric,Color,Dyelot,Store) VALUES ;
      ('O',&lcInvLine..cMOrder,&lcInvLine..LineNo,laCanReason[lnCanReason,2],gdSysDate,lfUpdPrice(&lcInvLine..cMOrder,&lcInvLine..LineNo),lfUpdGrPrc(&lcInvLine..cMOrder,&lcInvLine..LineNo),m.Fabric,m.Color,m.Dyelot,m.Store)
    
    ENDIF
    =RLOCK()
    REPLACE FabQty WITH MAX(m.FabBook-m.FabQty,0)
    UNLOCK
    SHOW GET pbCnRemain ENABLE
  ELSE
    IF SEEK('O'+&lcInvLine..cMOrder+STR(&lcInvLine..LineNO,6))
      DELETE
    ENDIF
    SHOW GET pbCnRemain DISABLE
  ENDIF
  SELECT (lcInvLine)
ENDIF
=RLOCK()
REPLACE lBackOrd WITH !llCnRemain
UNLOCK

*!*************************************************************
*! Name      : FUNCTION lfvCnRemain
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Enter Order line calcellation reason and date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvCnRemain()
*!*************************************************************
FUNCTION lfvCnRemain

=gfwCodePop(@laCodes,'CCANCRESON','T')
STORE &lcOrdCanLn..Cancelled TO m.Cancelled
DO (gcScrDir+"SOORDCLN.SPX")
      
*!*************************************************************
*! Name      : FUNCTION lfvOrdCanLn
*! Developer : Wael Aly Mohamed
*! Date      : 08/16/1998
*! Purpose   : Update Order line canceled reason and date
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

=SEEK('O'+laData[1]+STR(m.LineNo,6))
REPLACE cCancReson WITH laCanReason[lnCanReason,2],;
        Cancelled  WITH m.Cancelled
SELECT (lcInvLine)
      
*!*************************************************************
*! Name      : FUNCTION lfvStyDesc
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Invline line description
*!*************************************************************
*! Calls     : lfNavigate
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyDesc()
*!*************************************************************
FUNCTION lfvStyDesc

IF lfNavigate()
  _CUROBJ = _CUROBJ
  RETURN
ENDIF  
SELECT (lcInvLine)
=RLOCK()
REPLACE Desc1 WITH m.Desc1
UNLOCK

*!*************************************************************
*! Name      : lpSavScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Save new Invoice
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
PRIVATE lcOrd
*C126361,1  TMI [Start] normal save of the release order 
IF !llPst2Inv
  lcOrd = lcRelOrd
  =lfSave()

  SELECT (lcBaseFile)
  =gfObj_Lock(.F.)
  
  laScrMode    = .F.
  laScrMode[2] = .T.
  =lfvOrder(lcOrd)

  RETURN
ENDIF  

*C126361,1  TMI [End  ] 

SELECT UNCMSESS
REPLACE cCurrObj WITH 'pbSav'
USE IN UNCMSESS && Close the file in order not to corrupt then open it  
=gfOpenFile(gcDataDir+'UNCMSESS','TRANS','SH')

*N000388,1 Check the lots and rolls before saving (Cost method - LOT). [Begin]
DO CASE
  CASE laSetups[7,2] = "L"
    PRIVATE lcTmpLine
    lcTmpLine = gfTempName()
    llRolls = gfOpenFile(gcDataDir+'Rolls', gcDataDir+'RolApl','SH')
  CASE laSetups[7,2] $ "FI"
    PRIVATE lcTmpMtIv
    lcTmpMtIv = gfTempName()
    *= lfCrTemp()
    SELECT (lcInvLine)
    LOCATE
    SCAN FOR Consol <> 'Y'
      IF !lfOldLots()
        *--No [Receiving Lines] was done, Cannot update.
        *= gfModalGen('INM34061B42000','DIALOG','Receiving Lines')
        *llCSave = .F.
        *EXIT
      ENDIF
    ENDSCAN
ENDCASE
*N000388,1 Check the lots and rolls before saving (Cost method - LOT). [Begin]

SET ORDER TO TAG 'DYELOT' IN (lcInvLine)
SELECT (lcInvHdr)

*-- scan only for the selected orders
PRIVATE lnPos
lnPos=RECNO(lcInvHdr)

SCAN FOR cSelect='»' 
  *N000388,1 Check Fifo , Lifo , Lot method. [Begin]
  IF Consol <> 'Y' AND SEEK(Account+cMOrder+Store+PikTkt,lcInvLine)
    
    *C126361,1  TMI [Start] check that on hand qty >= shipped qty
    SELECT &lcInvLine
    LOCATE
    LOCATE REST FOR &lcInvLine..FABQTY > FABDYE.ONHAND
    IF FOUND()
      =gfModalGen('TRM04025B00000','ALERT','Shipped Quantity'+'|'+'On Hand Quantity.') 
      llCSave = .F.
      EXIT    
    ENDIF
    LOCATE
    *C126361,1  TMI [End  ] 
    
    SELECT (lcInvline)
    IF laSetups[7,2] = 'L' .AND. !lfChkRolls()
      = SEEK(ladata[4]+ladata[1]+laData[5],(lcInvHdr))
      llCSave = .F.
      EXIT
    ENDIF
    SELECT (lcInvHdr)
  ENDIF
  *N000388,1 Check Fifo , Lifo , Lot method. [End]
  
  IF Consol <> 'Y' AND SEEK(Account+cMOrder+Store+PikTkt+'Y'+SPACE(10),lcInvLine)
    SELECT (lcInvline)

    *N000388,1 Correct the locate statement. [Begin]
    *LOCATE REST FOR Account+cMOrder+Store+PikTkt+'Y'+SPACE(10) = &lcInvHdr..Account+;
    *       &lcInvHdr..cMOrder+&lcInvHdr..Store+&lcInvHdr..PikTkt+'Y'+SPACE(10) ;
    *       AND FabQty <> 0
    LOCATE REST FOR Account+cMOrder+Store+PikTkt+cDyeFlag+Dyelot = &lcInvHdr..Account+;
           &lcInvHdr..cMOrder+&lcInvHdr..Store+&lcInvHdr..PikTkt+'Y'+SPACE(10) ;
           AND FabQty <> 0
    *N000388,1 Correct the locate statement. [End]
    
    IF FOUND()
      *-- Message : 40153
      *-- shipped Fabric has not been assigned dyelots.
      *-- Button : 00000
      *-- Ok
      =gfModalGen('TRM40176B00000','ALERT')
      llCSave = .F.
      EXIT
    ENDIF
    SELECT (lcInvHdr)
  ENDIF

  IF Consol='Y' OR EMPTY(Flag)
    llSaveInv=.F.
            
    *-- check the invoice before saving    
    DO gfChkSavInv IN (gcapphome+gcwinappl+'\ARMtInv.PRG') WITH ;
    Account,cMOrder,Store,PikTKt,lcInvHdr,IIF(USED(lcInstHdr),lcInstHdr,''),;
    IIF(USED(lcAppCrdt),lcAppCrdt,''),IIF(USED(lcUpsBox),lcUpsBox,''),'llSaveInv'
    IF !llSaveInv
      llCSave = .F.
      EXIT
    ENDIF
  ENDIF
ENDSCAN  

IF BETWEEN (lnPos,1,RECCOUNT(lcInvHdr))
  GOTO lnPos IN (lcInvHDr)
ENDIF

SET ORDER TO TAG (lcInvLine) IN (lcInvLine)
IF !llCSave
  IF lnActFolder = 3
    SELECT (lcInvLine)
    
    *N000388,1 Always force not to be end  of file. [Begin]
    IF EOF()
      LOCATE
    ENDIF  
    *N000388,1 Always force not to be end  of file. [End]
    
    SET KEY TO laData[4]+laData[1]+laData[5]+laData[3]
  ENDIF
  llPst2Inv = .T.
  RETURN
ENDIF

IF !llContinue .AND. !lfBatchSum(.T.)
  llCSave = .F.
  llPst2Inv = .T.
  RETURN
ENDIF

DELETE ALL FOR cSelect+Account+cMOrder+Store+PikTkt = SPACE(1)
SET KEY TO 
*-- Array to hold invoice number
DECLARE laInv[1]
STORE '' TO laInv

*-- save the invoice
=lfReClcShp()
DO gpSaveInv IN (gcapphome+gcwinappl+'\ARMtInv.PRG') WITH ;
lcInvHdr,lcInvLine,IIF(USED(lcUpsBox),lcUpsBox,''),IIF(USED(lcEngChrg),lcEngChrg,''),;
IIF(USED(lcInstHdr),lcInstHdr,''),IIF(USED(lcInstLin),lcInstLin,''),;
IIF(USED(lcOrdCanLn),lcOrdCanLn,''),lcGlSession,'laInv'   
SELECT unCmSess

IF SEEK('O'+PADR('MRELSEORD',10)+PADR(gcUser_id,10)+lcSession)  
  REPLACE STATUS WITH 'C'
  llContinue = .F.
  UNLOCK
ENDIF

lnInvCount = ALEN(laInv)
DO CASE
  CASE EMPTY(laInv)
    llCSave = .F.
  CASE lnInvCount=1
    *-- Message : 40005
    *-- Invoice has been saves as.
    *-- Button : 00000 
    *-- Ok
    =gfModalGen('INM40005B00000','ALERT',laInv[1])
  OTHERWISE
    *-- Message : 40118
    *-- Invoices 999999 Through 999999.
    *-- Button : 00000 
    *-- Ok
    =gfModalGen('INM40118B00000','ALERT',laInv[1]+'|'+laInv[lnInvCount])
ENDCASE

*C126361,4  TMI [Start] 
SELECT (lcBaseFile)
=gfObj_Lock()
*C126361,4  TMI [End  ] 

SELECT (lcInvHdr)
RETURN

*!*************************************************************
*! Name      : lfSave
*! Developer : TMI
*! Date      : 04/12/2005
*! Purpose   : Save new Invoice
*!*************************************************************
FUNCTION lfSave

SELECT UNCMSESS
REPLACE cCurrObj WITH 'pbSav'
USE IN UNCMSESS && Close the file in order not to corrupt then open it  
=gfOpenFile(gcDataDir+'UNCMSESS','TRANS','SH')

SET ORDER TO TAG 'DYELOT' IN (lcInvLine)
SELECT (lcInvHdr)

*-- scan only for the selected orders
PRIVATE lnPos
lnPos=RECNO(lcInvHdr)

IF BETWEEN (lnPos,1,RECCOUNT(lcInvHdr))
  GOTO lnPos IN (lcInvHDr)
ENDIF

SET ORDER TO TAG (lcInvLine) IN (lcInvLine)
IF !llCSave
  IF lnActFolder = 3
    SELECT (lcInvLine)
    IF EOF()
      LOCATE
    ENDIF  
    SET KEY TO laData[4]+laData[1]+laData[5]+laData[3]
  ENDIF
  RETURN
ENDIF

DELETE ALL FOR cSelect+Account+cMOrder+Store+PikTkt = SPACE(1)
SET KEY TO 

*-- Array to hold invoice number
DECLARE laInv[1]
STORE '' TO laInv

*-- save the invoice
=lfReClcShp()

DO lfSavRel WITH ;
    lcInvHdr,lcInvLine,IIF(USED(lcUpsBox),lcUpsBox,''),IIF(USED(lcEngChrg),lcEngChrg,''),;
    IIF(USED(lcInstHdr),lcInstHdr,''),IIF(USED(lcInstLin),lcInstLin,''),;
    IIF(USED(lcOrdCanLn),lcOrdCanLn,''),lcGlSession,'laInv'   

SELECT UnCmSess
IF SEEK('O'+PADR('MRELSEORD',10)+PADR(gcUser_id,10)+lcSession)
  REPLACE STATUS WITH 'C'
  llContinue = .F.
  UNLOCK
ENDIF

lnInvCount = ALEN(laInv)
*-* DO CASE
*-*   CASE EMPTY(laInv)
*-*     llCSave = .F.
*-*   CASE lnInvCount=1
*-*     =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Material Release Sales Order has been saved as '+ laInv[1])
*-*   OTHERWISE
*-*     =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Material Release Sales Orders has been saved as '+ laInv[1] + ' TO ' + laInv[lnInvCount])
*-* ENDCASE
SELECT (lcInvHdr)

llCSave = .F.

_CUROBJ = OBJNUM(laData[1])


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
PRIVATE lcCurObj

IF (llFrmSetup AND llChkUnCom) OR !llFrmSetup
  llGoAndChk = IIF(llFrmSetup, .F., llGoAndChk)
  llChkUnCom = .F.
  IF gfUnCompSession(lcProgID, lnSessNo,"Material Invoice Sales Orders")
  
    IF llFrmSetup
      =gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
      =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
      =gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')  
      =gfOpenFile(gcDataDir+'PACK_LIN',gcDataDir+'PACK_LIN','SH')
      =gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH')
      =gfOpenFile(gcDataDir+'Fabric',gcDataDir+'Fabric','SH') 
      =gfOpenFile(gcDataDir+'FabDye',gcDataDir+'FabDye','SH')
      llOpnFiles=.F.      
    ENDIF 

    laScrMode    = .F.
    laScrMode[4] = .T.
    llContinue   = .T.
    lcSession    = UnCmSess.cSession
    lcCurObj     = ALLTRIM(UnCmSess.cCurrObj)
    lnLastMode   = 4
    SELECT (lcInvHdr)
    GO TOP
    SCATTER MEMVAR FIELDS &lcFlFields
    SCATTER FIELDS &lcScFields TO laData
    STORE lcInvHdr TO laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],;
                      laCodes[3,7],laCodes[3,8],laCodes[4,7],laCodes[4,8],;
                      laCodes[5,7],laCodes[5,8]
            
    IF !EMPTY(lcCurObj)
      IF llFrmSetup AND UPPER(lcCurObj) = "PBSAV"
        =lpSavScr()
        =lfCratTemp()
        laScrMode    = .F.
        laScrMode[1] = .T.
      ELSE
        _CUROBJ = OBJNUM(&lcCurObj)
        KEYBOARD "{ENTER}" PLAIN CLEAR
      ENDIF
    ENDIF
  ELSE
    =lfCratTemp()
  ENDIF
ENDIF
=lfBrowOrd() .AND. lfBrowDet()
RETURN (llContinue)

*!*************************************************************
*! Name      : lfCratTemp
*! Developer : WAM
*! Date      : 07/01/1996
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

IF WEXIST(lcOrdBrow)
  HIDE WINDOW (lcOrdBrow)
  RELEASE WINDOW (lcOrdBrow)
ENDIF

SELECT MInvHdr

=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+14,4]
laFileStru[lnFileStru+1,1] = 'cSelect'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'Dist_Ctr'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 8
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'Picked'
laFileStru[lnFileStru+3,2] = 'N'
laFileStru[lnFileStru+3,3] = 7
laFileStru[lnFileStru+3,4] = 0
laFileStru[lnFileStru+4,1] = 'lUpsIns'
laFileStru[lnFileStru+4,2] = 'L'
laFileStru[lnFileStru+4,3] = 1
laFileStru[lnFileStru+4,4] = 0
laFileStru[lnFileStru+5,1] = 'nSteps'
laFileStru[lnFileStru+5,2] = 'N'
laFileStru[lnFileStru+5,3] = 2
laFileStru[lnFileStru+5,4] = 0
laFileStru[lnFileStru+6,1] = 'nChrgTax'
laFileStru[lnFileStru+6,2] = 'N'
laFileStru[lnFileStru+6,3] = 13
laFileStru[lnFileStru+6,4] = 2
laFileStru[lnFileStru+7,1] = 'nMerchTax'
laFileStru[lnFileStru+7,2] = 'N'
laFileStru[lnFileStru+7,3] = 13
laFileStru[lnFileStru+7,4] = 4
laFileStru[lnFileStru+8,1] = 'lCompUps'
laFileStru[lnFileStru+8,2] = 'L'
laFileStru[lnFileStru+8,3] = 1
laFileStru[lnFileStru+8,4] = 0
laFileStru[lnFileStru+9,1] = 'LastLine'
laFileStru[lnFileStru+9,2] = 'N'
laFileStru[lnFileStru+9,3] = 6
laFileStru[lnFileStru+9,4] = 0
laFileStru[lnFileStru+10,1] = 'LKEYOFF'
laFileStru[lnFileStru+10,2] = 'L'
laFileStru[lnFileStru+10,3] = 0
laFileStru[lnFileStru+10,4] = 0
laFileStru[lnFileStru+11,1] = 'NTAXDUE'
laFileStru[lnFileStru+11,2] = 'N'
laFileStru[lnFileStru+11,3] = 17
laFileStru[lnFileStru+11,4] = 6
laFileStru[lnFileStru+12,1] = 'NCARTONS'
laFileStru[lnFileStru+12,2] = 'N'
laFileStru[lnFileStru+12,3] = 11
laFileStru[lnFileStru+12,4] = 5
laFileStru[lnFileStru+13,1] = 'Ordered'
laFileStru[lnFileStru+13,2] = 'N'
laFileStru[lnFileStru+13,3] = 11
laFileStru[lnFileStru+13,4] = 3
laFileStru[lnFileStru+14,1] = 'cConStore'
laFileStru[lnFileStru+14,2] = 'C'
laFileStru[lnFileStru+14,3] = 8
laFileStru[lnFileStru+14,4] = 0

DECLARE laIndex[3,2]
laIndex[1,1] = 'Account+cMOrder+Store+PikTkt+CDIVISION'
laIndex[1,2] = lcInvHdr
laIndex[2,1] = 'cSelect+Account+cMOrder+Store+PikTkt+CDIVISION'
laIndex[2,2] = 'Select'
laIndex[3,1] = 'Consol+Account+cDivision+cCurrCode+CDIVISION'
laIndex[3,2] = 'Consol'
=gfCrtTmp(lcInvHdr,@laFileStru,@laIndex)
=gfCrtTmp(lcConsInvH,@laFileStru,[Consol+Account+cDivision+cCurrCode],lcConsInvH)
SELECT (lcInvHdr)
SET ORDER TO TAG (lcInvHdr)
SCATTER MEMVAR FIELDS &lcFlFields BLANK
SCATTER FIELDS &lcScFields TO laData BLANK

IF WEXIST(lcBrTtlZ)
  HIDE WINDOW (lcBrTtlZ)
  RELEASE WINDOW (lcBrTtlZ)
ENDIF
IF WEXIST(lcInvBrow)
  HIDE WINDOW (lcInvBrow)
  RELEASE WINDOW (lcInvBrow)
ENDIF
SELECT MaSoLin
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)

*N000388,1 Add the PikTkt which was removed from MaSoLin structure
*DIMENSION laFileStru[lnFileStru+13,4]
DIMENSION laFileStru[lnFileStru+15,4]
*N000388,1 Add the PikTkt which was removed from MaSoLin structure

laFileStru[lnFileStru+1,1] = 'LNEWLINE'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 0
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'LPACKED'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 0
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'LBACKORD'
laFileStru[lnFileStru+3,2] = 'L'
laFileStru[lnFileStru+3,3] = 0
laFileStru[lnFileStru+3,4] = 0
laFileStru[lnFileStru+4,1] = 'nSteps'
laFileStru[lnFileStru+4,2] = 'N'
laFileStru[lnFileStru+4,3] = 2
laFileStru[lnFileStru+4,4] = 0
laFileStru[lnFileStru+5,1] = 'nTaxRate'
laFileStru[lnFileStru+5,2] = 'N'
laFileStru[lnFileStru+5,3] = 10
laFileStru[lnFileStru+5,4] = 2
laFileStru[lnFileStru+6,1] = 'cCurrCode'
laFileStru[lnFileStru+6,2] = 'C'
laFileStru[lnFileStru+6,3] = 3
laFileStru[lnFileStru+6,4] = 0
laFileStru[lnFileStru+7,1] = 'cDivision'
laFileStru[lnFileStru+7,2] = 'C'
laFileStru[lnFileStru+7,3] = 6
laFileStru[lnFileStru+7,4] = 0
laFileStru[lnFileStru+8,1] = 'Consol'
laFileStru[lnFileStru+8,2] = 'C'
laFileStru[lnFileStru+8,3] = 1
laFileStru[lnFileStru+8,4] = 0
laFileStru[lnFileStru+9,1] = 'nNetAmnt'
laFileStru[lnFileStru+9,2] = 'N'
laFileStru[lnFileStru+9,3] = 18
laFileStru[lnFileStru+9,4] = 10
laFileStru[lnFileStru+10,1] = 'nGrosAmnt'
laFileStru[lnFileStru+10,2] = 'N'
laFileStru[lnFileStru+10,3] = 18
laFileStru[lnFileStru+10,4] = 10
laFileStru[lnFileStru+11,1] = 'LTAXABLE'
laFileStru[lnFileStru+11,2] = 'L'
laFileStru[lnFileStru+11,3] = 0
laFileStru[lnFileStru+11,4] = 0
laFileStru[lnFileStru+12,1] = 'cDyeFlag'
laFileStru[lnFileStru+12,2] = 'C'
laFileStru[lnFileStru+12,3] = 1
laFileStru[lnFileStru+12,4] = 0
laFileStru[lnFileStru+13,1] = 'cConStore'
laFileStru[lnFileStru+13,2] = 'C'
laFileStru[lnFileStru+13,3] = 8
laFileStru[lnFileStru+13,4] = 0

*N000388,1 Add the PikTkt which was removed from MaSoLin structure
laFileStru[lnFileStru+14,1] = 'PikTkt'
laFileStru[lnFileStru+14,2] = 'C'
laFileStru[lnFileStru+14,3] = 6
laFileStru[lnFileStru+14,4] = 0

*B606163,1 ABD - [Begin]
laFileStru[lnFileStru+15,1] = 'Rolls_Old'
laFileStru[lnFileStru+15,2] = 'C'
laFileStru[lnFileStru+15,3] = 3
laFileStru[lnFileStru+15,4] = 0
*B606163,1 ABD - [End]

*N000388,1 Add the PikTkt which was removed from MaSoLin structure

*N000388,1 Add new index type to be used in Lot costing method. [Begin]
*DECLARE laIndex[4,2]
DECLARE laIndex[5,2]
*N000388,1 Add new index type to be used in Lot costing method. [End]

laIndex[1,1] = 'Account+cMOrder+Store+PikTkt+STR(LineNo,6)'
laIndex[1,2] = lcInvLine
laIndex[2,1] = 'Account+cMOrder+Store+PikTkt+Fabric+Color+DyeLot'
laIndex[2,2] = 'FabColor'
laIndex[3,1] = 'Consol+Account+cDivision+cCurrCode+Fabric+Color'
laIndex[3,2] = 'Consol'
laIndex[4,1] = 'Account+cMOrder+Store+PikTkt+cDyeFlag+Dyelot'
laIndex[4,2] = 'Dyelot'

*N000388,1 Add new index type to be used in Lot costing method. [Begin]
laIndex[5,1] = 'Fabric+Color+Dyelot+cWareCode'
laIndex[5,2] = 'Lot_Roll'
*N000388,1 Add new index type to be used in Lot costing method. [End]

=gfCrtTmp(lcInvLine,@laFileStru,@laIndex)
=gfCrtTmp(lcConsInvD,@laFileStru,[Consol+Account+cDivision+cCurrCode+cWareCode+Fabric+Color],lcConsInvD)
SELECT (lcInvLine)
SET ORDER TO TAG (lcInvLine)
SCATTER MEMVAR BLANK MEMO

IF laSetups[15,2] = 'Y'
  =gfOpenFile(gcDataDir+'MUPSBOX',gcDataDir+'MUPSBOX','SH')
  =AFIELDS(laFileStru)
  =gfCloseFile('MUPSBOX')
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'cMOrder'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'PikTkt'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 6
  laFileStru[lnFileStru+2,4] = 0
  =gfCrtTmp(lcUpsBox,@laFileStru,[cMOrder+Store+PikTkt+STR(CARTONS,5)],lcUpsBox)
ENDIF
IF llIsEngland
  *--MAN If called from Invoice Sales Order the file is not opened
  IF !USED("MInvChrg")
    =gfOpenFile(gcDataDir+'MInvChrg',gcDataDir+'MInvChrg','SH')
  ENDIF  
  
  SELECT MInvChrg 
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'cMOrder'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'PikTkt'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 6
  laFileStru[lnFileStru+2,4] = 0
  =gfCrtTmp(lcEngChrg,@laFileStru,[cMOrder+cStore+PikTkt+cchrgcode],lcEngChrg)
ENDIF
=gfCrtTmp(lcInstHdr,[(cMOrder C(6),Store C(8),PikTkt C(6),;
          cInstmType C(1), nInstmFreq N(3),nInstIAmnt N(11,2),dInstmStDt D,;
          nNoInstm N(3), cInstmRef C(30),nInstIPcnt N(6,2),nInstmAmnt N(10,2))],;
          [cMOrder+Store+PikTkt],lcInstHdr)
=gfCrtTmp(lcInstLin,[(cMOrder C(6),Store C(8),PikTkt C(6),;
            cInstalNo C(3),nInstmAmnt N(10,2),DueDate D,nInstmPcnt N(6,2),cInstmNote C(30))],;
            [cMOrder+Store+PikTkt+cInstalNo],lcInstLin)

=gfCrtTmp(lcOrdCanLn,[(cOrdType C(1),cMOrder C(6),LineNo N(6),;
            FabQty N(7),Cancelled D,cCancReson C(6),nSellNetPr N(12,2),nSellPrice N(12,2),Dyelot C(10),Store C(8),Fabric C(7),Color C(6))],;
            [CORDTYPE+cMOrder+STR(LINENO,6)],lcOrdCanLn)

=gfCrtTmp(lcPackLine,[(Pack_No C(6),cMOrder C(6),nOrdLineNo N(6),FabQty N(7))],;
            [PACK_NO+cMOrder+STR(nOrdLineNo,6)],lcPackLine)

*N000388,1 Define TmpJour structrue to be used in gfMatCrl. [Begin]
*PRIVATE lnNewFld
*=gfOpenFile(gcDataDir+'MatInvJl',gcDataDir+'MatInvJl','SH')
*SELECT MATINVJL
*= AFields(laFileStru)
*lnNewFld = ALEN(laFileStru,1)+1
*DIMENSION laFileStru[lnNewFld,4]
*laFileStru[lnNewFld,1] = 'cMarker'
*laFileStru[lnNewFld,2] = 'C'
*laFileStru[lnNewFld,3] = 1
*laFileStru[lnNewFld,4] = 0

*lnNewFld = ALEN(laFileStru,1)+1
*DIMENSION laFileStru[lnNewFld,4]
*laFileStru[lnNewFld,1] = 'cSele'
*laFileStru[lnNewFld,2] = 'C'
*laFileStru[lnNewFld,3] = 1
*laFileStru[lnNewFld,4] = 0

*lnNewFld = ALEN(laFileStru,1)+1
*DIMENSION laFileStru[lnNewFld,4]
*laFileStru[lnNewFld,1] = 'nApply'
*laFileStru[lnNewFld,2] = 'N'
*laFileStru[lnNewFld,3] = 8
*laFileStru[lnNewFld,4] = 0

*lnNewFld = ALEN(laFileStru,1)+1
*DIMENSION laFileStru[lnNewFld,4]
*laFileStru[lnNewFld,1] = 'nBalance'
*laFileStru[lnNewFld,2] = 'N'
*laFileStru[lnNewFld,3] = 8
*laFileStru[lnNewFld,4] = 0

*lnNewFld = ALEN(laFileStru,1)+1
*DIMENSION laFileStru[lnNewFld,4]
*laFileStru[lnNewFld,1] = 'roltrancd'
*laFileStru[lnNewFld,2] = 'C'
*laFileStru[lnNewFld,3] = 1
*laFileStru[lnNewFld,4] = 0

*lnNewFld = ALEN(laFileStru,1)+1
*DIMENSION laFileStru[lnNewFld,4]
*laFileStru[lnNewFld,1] = 'lstatus'
*laFileStru[lnNewFld,2] = 'C'
*laFileStru[lnNewFld,3] = 1
*laFileStru[lnNewFld,4] = 0

*lnNewFld = ALEN(laFileStru,1)+1
*DIMENSION laFileStru[lnNewFld,4]
*laFileStru[lnNewFld,1] = 'lneeded'
*laFileStru[lnNewFld,2] = 'L'
*laFileStru[lnNewFld,3] = 0
*laFileStru[lnNewFld,4] = 0

*=gfCrtTmp(lcTmpJour,@laFileStru,[cFabric+cColor+cWareCode+cDyelot+cRsession+cIsession],lcTmpJour)
*=gfCloseFile('MatInvJl')
*N000388,1 Define TmpJour structrue to be used in gfMatCrl. [End]

*!*************************************************************
*! Name      : lfvInvInst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice Installments
*!*************************************************************
*! Calls     : gpInvInstm
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInvInst()
*!*************************************************************
FUNCTION lfvInvInst
DO gpInvInstm IN (gcapphome+gcwinappl+'\ARMtInv.PRG') WITH ;
   laData[11],laData[1],laData[5],laData[3],ROUND(m.TotalChg,2),laData[6],;
   lcInstHdr,lcInstLin
FUNCTION lfGtSavBar
PARAMETERS lcBar
USE (gcSysHome+"SYCMenu") IN 0 ORDER Pross_ID AGAIN ALIAS MenuFile
lnRetVal = IIF(SEEK(lcBar, "MenuFile"), VAL(MenuFile.cBar_Pos),0)
USE IN MenuFile
RETURN (lnRetVal)

*!**************************************************************************
*! Name      : lfModShpAr
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/22/1999
*! Purpose   : change elemnt (All) in shipvia popup to be (Multi)
*!*************************************************************************
*! Calls     : 
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Returns            : None
*!*************************************************************************
*! Example   : =lfModShpAr()
*!************************************************************************
FUNCTION lfModShpAr
*--Change frist elemnt ( all ) to be ( Multi ) in ship via popup
  laShipVia[1,1] = IIF(ALLTRIM(laShipVia[1,2])='*','Multi',laShipVia[1,1])

*!**************************************************************************
*! Name      : lfChekCons
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/23/1999
*! Purpose   : check if the headr of consolidate invoice is multi or specific 
*!           : ship via code
*!*************************************************************************
*! Calls     : 
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Returns            : None
*!*************************************************************************
*! Example   : =lfChekCons()
*!************************************************************************
FUNCTION lfChekCons
PRIVATE lnRecNo,lcAccount,lcOrder,lcStore,lcPikTkt,lcConslFlag,llConAll

llConAll = .T.

lnRecNo = RECNO()
SET ORDER TO TAG 'CONSOL'
IF SEEK('Y'+Account+cDivision+cCurrCode)
  llConAll = (ShipVia = '*')
ENDIF

SET ORDER TO TAG (lcInvHdr)
GO TOP
RETURN llConAll

*!*************************************************************
*! Name      : lfRefComm
*! Developer : Sameh Al-Desouki
*! Date      : 06/02/2000
*! Purpose   : Refresh Sales Rep. header Commession For invoice
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfRefComm()
*!*************************************************************
FUNCTION lfRefComm
PRIVATE lnLnComm1 ,lnLnComm2 ,lnNetShipAmnt,lnNetAmnt, lnAlias , lcHdrKey , lcLineKey
STORE 0 TO lnLnComm1 ,lnLnComm2

*-- Save defaults
lnAlias = SELECT(0)

SELECT (lcInvLine)
lcLineKey = EVALUATE(KEY())

SELECT (lcInvHdr)
lcHdrKey  = EVALUATE(KEY())
WAIT 'Computing Sales Rep. commission...' WINDOW NOWAIT
*-- seek to Account in Invoice Header [Start]
IF SEEK(laData[4])
  *-- for account only in displaying data for the first time (when user complete select store)
  *-- and for acount ,division and currency code if refresh after change line commissions 
  SCAN REST WHILE (Account = laData[4]) FOR ((cDivision = laData[13]) AND (cCurrCode = laData[14]))
    SELECT (lcInvLine)
    = SEEK (&lcInvHdr..Account+&lcInvHdr..cMOrder+&lcInvHdr..Store+&lcInvHdr..PikTkt)
    *-- Accumulate sales Rep. commission
    SCAN REST WHILE Account+cMOrder+Store+PikTkt = ;
                  &lcInvHdr..Account+&lcInvHdr..cMOrder+&lcInvHdr..Store+&lcInvHdr..PikTkt
      lnNetAmnt = &lcInvLine..FabQty*&lcInvLine..nSellNetPr*(1-&lcInvHdr..DiscPcnt/100)*(1-&lcInvHdr..Trde_Disc/100)
      lnLnComm1   = lnLnComm1 + (lnNetAmnt * &lcInvLine..Comm1 / 100)
      lnLnComm2   = lnLnComm2 + (lnNetAmnt * &lcInvLine..Comm2 / 100)
    ENDSCAN

    *-- Calculate net shipment amount
    lnNetShipAmnt = &lcInvHdr..FabShipAmt*(1-&lcInvHdr..DiscPcnt/100)*(1-&lcInvHdr..Trde_Disc/100)
    *-- for lnNetShipAmnt <> 0 update header commission [Start]
    IF lnNetShipAmnt <> 0
      SELECT (lcInvHdr)
      *-- header commission for Sales Rep 1 
      lnRep1Comm = ROUND( lnLnComm1/lnNetShipAmnt*100 , 2)
      REPLACE &lcInvHdr..comm1 WITH lnRep1Comm
      laData[9] = lnRep1Comm
      *-- header commission for Sales Rep 2 
      lnRep2Comm = ROUND( lnLnComm2/lnNetShipAmnt*100 , 2)
      REPLACE &lcInvHdr..comm2 WITH lnRep2Comm
      laData[10] = lnRep2Comm
    ENDIF && for lnNetShipAmnt <> 0 update header commission [End]
  ENDSCAN
  
ENDIF && seek to Account in Invoice Header [End]

*-- display commissions 
SHOW GET laData[9] DISABLE && header commission for Sales Rep 1 
SHOW GET laData[10] DISABLE && header commission for Sales Rep 2

*-- Restore defaults
=SEEK(lcHdrKey , lcInvHdr)
=SEEK(lcLineKey, lcInvLine)
SELECT (lnAlias)
WAIT CLEAR
*-- end of lfRefComm.

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Abdou ElGendi
*! Date      : 06/25/2000
*! Purpose   : When function of size quantities , total and net price
*!           : to store old value of the current filed.
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
*:*************************************************************
*! Name      : lfwTaxAmt
*! Developer : Nader Anis (NAD)
*! Date      : 04/03/2000
*! Purpose   : To disable editing the Tax amount if the pecentage >0
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : .T. OR .F.
*!*************************************************************

FUNCTION lfwTaxAmt

IF laScrMode[4] AND Consol <> 'Y' AND m.TAX_RATE = 0
  IF UPPER(ALLTRIM(gcContCode))='USA'
    RETURN &lcInvHdr..nTaxDue > 0 
  ELSE
    RETURN m.FabShipAmt > 0 
  ENDIF
ENDIF
RETURN .F.

*:*************************************************************
*! Name      : lfReClcShp
*! Developer : Nader Anis (NAD)
*! Date      : 
*! Purpose   : Recalculate Ship Amount 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*! 
*!*************************************************************
*! Passed Parameters  : llFrmFlder
*    This parameter is true if called from lfActFolder function.   
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfReClcShp
PARAMETERS llFrmFlder
** laData[4] : Account
** laData[1] : Order
** laData[5] : Store
** laData[3] : PikTkt
lnOAlias= ALIAS()
PRIVATE lnShp,lnShpAmt,lnRec
STORE 0 TO lnShp,lnShpAmt,lnRec
IF llFrmFlder
  lnRec=RECNO(lcInvLine)
  IF SEEK(laData[4]+laData[1]+laData[5]+laData[3],lcInvLine)
    SELECT (lcInvline)
    *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
    *SUM REST FabQty,ROUND(FabQty * nSellNetPr,2) FOR Consol = &lcInvHdr..Consol TO lnShp,lnShpAmt WHILE  Account+cMOrder+Store+PikTkt = laData[4]+laData[1]+laData[5]+laData[3]
    SUM REST FabQty,CEILING(FabQty * nSellNetPr*100)/100 FOR Consol = &lcInvHdr..Consol TO lnShp,lnShpAmt WHILE  Account+cMOrder+Store+PikTkt = laData[4]+laData[1]+laData[5]+laData[3]
    *E126684,1  TMI [End  ] 
    SELECT (lcInvHdr)
    REPLACE FabShip    WITH lnShp 
    REPLACE FabShipAmt WITH lnShpAmt
    
    *N000388,1 Update the FabShip and FabShipAmt. [Begin]
    m.FabShip = FabShip
    m.FabShipAmt = FabShipAmt
    *N000388,1 Update the FabShip and FabShipAmt. [End]
    
  ENDIF
  SELECT (lcInvline)
  IF BETWEEN(lnRec ,1,RECCOUNT())
    GOTO lnRec
  ENDIF   
ELSE
  SELECT (lcInvHdr) 
  SCAN
    STORE 0 TO lnShp,lnShpAmt
    IF SEEK(Account+cMOrder+Store+PikTkt,lcInvLine)
      SELECT (lcInvline)
      *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
      *SUM REST FabQty,ROUND(FabQty * nSellNetPr,2) FOR Consol = &lcInvHdr..Consol TO lnShp,lnShpAmt WHILE  Account+cMOrder+Store+PikTkt = &lcInvHdr..Account+&lcInvHdr..cMOrder+&lcInvHdr..Store+&lcInvHdr..PikTkt
      SUM REST FabQty,CEILING(FabQty * nSellNetPr*100)/100 FOR Consol = &lcInvHdr..Consol TO lnShp,lnShpAmt WHILE  Account+cMOrder+Store+PikTkt = &lcInvHdr..Account+&lcInvHdr..cMOrder+&lcInvHdr..Store+&lcInvHdr..PikTkt
      *E126684,1  TMI [End  ] 
      SELECT (lcInvHdr) 
      REPLACE FabShip    WITH lnShp
      REPLACE FabShipAmt WITH lnShpAmt
    ENDIF
  ENDSCAN
ENDIF 
SELECT (lnOAlias)

*:********************************************************************************
*! Name      : lfGetOrdPkT
*! Developer : Nader Anis (NAD)
*! Date      : 09/05/2000
*! Purpose   : Get the orders or the orders with pick ticket in the scope screen. 
*!*********************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfGetOrdPkT
*=lfGetOrder(Order,'',cbPickedOrd,lcFromPick,lcToPick,.T.)          
=lfGetOrder(cMOrder,'',.F.,lcFromPick,lcToPick,.T.)

*:********************************************************************************
*! Name      : lfwCharges
*! Developer : Nader Anis (NAD)
*! Date      : 10/24/2000
*! Purpose   : When function for the charges screen fields. 
*! Reference : B803571,1
*!*********************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfwCharges

lnOldChrg=EVAL('m.'+SYS(18))

RETURN .T.

*:********************************************************************************
*! Name      : lfNegChrg
*! Developer : Nader Anis (NAD)
*! Date      : 10/24/2000
*! Purpose   : When function for the charges screen fields. 
*! Reference : B803571,1
*!*********************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfNegChrg
PRIVATE lcObject,llReturn

llReturn=.T.
lcObject='m.'+SYS(18)
IF &lcObject <0 AND UPPER(SYS(18)) # 'DISCOUNT' 
  &lcObject = lnOldChrg
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Buttfon  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  _CUROBJ = _CUROBJ
  llReturn=.F.
ENDIF

RETURN llReturn

*!*************************************************************
*! Name      : FUNCTION lfvHstTax
*! Developer : Adel Mohammed El Gazzar
*! Date      : 03/15/2001
*! Purpose   : Compute HST Tax Rate and Amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  llRate : .T. : HST Tax Rate Percent
*!                                .F. : HST Tax Rate Amount
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvHstTax(.T.)
*!*************************************************************
FUNCTION lfvHstTax
PARAMETERS llRate,llCalled
PRIVATE lnOTaxAmnt
IF lfNegChrg()
  SELECT (lcInvHdr)  
  m.cTaxRule = STR(lnPstRule,2)
  m.nHstAmt  = IIF(llRate,(FabShipAmt+Freight+Insur+COD+;
               Discount+IIF(VAL(m.cTaxRule)=1,0,Tax_Amt))*;
               m.nHstRate/100,m.nHstAmt)
  m.nHstRate = IIF(!llRate,m.nHstAmt*100/(FabShipAmt+Freight+Insur+COD+Discount+;
               IIF(VAL(m.cTaxRule)=1,0,Tax_Amt)),m.nHstRate)
  IF Cod_Flag='Y'
    IF EMPTY(lcUPSType) OR lcUPSType='OTHER'
      m.Cod_Amt = FabShipAmt-FabShipAmt*m.DiscPcnt/100 +m.Tax_Amt+IIF(lnPstRule=2,m.nHstAmt,0)
    ELSE
      m.Cod_Amt = FabShipAmt+Freight+Insur+COD+Discount+Tax_Amt+IIF(lnPstRule=2,m.nHstAmt,0)
    ENDIF
  IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. ;
      Cartons=1 .AND. SEEK(cMOrder+Store+PikTkt,lcUpsBox)
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH m.Cod_Amt
      UNLOCK
    ENDIF
  ENDIF
  SELECT (lcInvHdr)  
  lnOTaxAmnt = nHstAmt
  
  m.TotalChg = FabShipAmt+Freight+Insur+COD+Discount+Tax_Amt+IIF(lnPstRule=2,m.nHstAmt,0)+m.nPstAmt
  
  =RLOCK()
  REPLACE cTaxRule WITH m.cTaxRule ,; 
          nHstRate WITH m.nHstRate ,;
          nHstAmt  WITH m.nHstAmt  ,;
          Cod_Amt  WITH IIF(Cod_Flag='Y',m.Cod_Amt,0) ,;
          TotalChg WITH m.TotalChg
  UNLOCK
  IF llCalled
    RETURN
  ENDIF
  lnRecNo = RECNO()
  SET ORDER TO TAG 'CONSOL'
  IF SEEK('Y'+Account+cDivision+cCurrCode)
    m.nHstAmt = nHstAmt-lnOTaxAmnt+m.nHstAmt
    =lfvHstTax(.F.,.T.)
  ENDIF
  SET ORDER TO TAG (lcInvHdr)
  GO lnRecNo
  SCATTER MEMVAR FIELDS nHstRate,nHstAmt,Cod_Amt,TotalChg
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
ENDIF

*!***************************************************************************
*! Name      : lpUpdNotes
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/26/2001
*! Purpose   : Update the Notepad for all invoices.
*!***************************************************************************
*! Example   : DO lpUpdNotes
*!***************************************************************************
*
PROCEDURE lpUpdNotes
PRIVATE lcAlias
lcAlias = ALIAS()

IF RECCOUNT(lcInvHdr) > 0
  SELECT (lcInvHdr)
  LOCATE
  IF !EMPTY(&lcInvHdr..cMatInv)
    GO BOTTOM
    PRIVATE llNotePad
    llNotePad = gfOpenFile(gcDataDir+'NotePad',gcDataDir+'NotePad','SH')
    
    IF SEEK('C'+&lcInvHdr..cMatInv,'NotePad')
      SCATTER MEMVAR MEMO
      SELECT (lcInvHdr)
      SCAN
        IF !SEEK('C'+&lcInvHdr..cMatInv,'NotePad')
          m.Key = cMatInv
          m.cDesc = 'Notes For Invoice Number : ' + cMatInv
          WAIT WINDOW 'Updating notes for Invoice : ' + cMatInv NOWAIT
          INSERT INTO NotePad FROM MEMVAR
          
          IF SEEK(&lcInvHdr..cMatInv,'InvHdr')
            SELECT MInvHdr
            =RLOCK()
            REPLACE lHasNotes WITH .T.
            UNLOCK    
          ENDIF

        ENDIF
      ENDSCAN
    ENDIF
    
    IF llNotePad
      USE IN NotePad
    ENDIF
  ENDIF
ENDIF

SELECT (lcAlias)
*-- End of lpUpdNotes.

*!***************************************************************************
*! Name      : lfUpdPrice
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/29/2001
*! Purpose   : Get the price from Order Line file.
*!***************************************************************************
*! Example   : lfUpdPrice()
*!***************************************************************************
*
FUNCTION lfUpdPrice
PARAMETER lcOrder , lnInvLinNo
PRIVATE lnOrdLnPrc , lnOrdRecNo , lcOrdLnTag
lnOrdLnPrc = 0
lnOrdRecNo = RECNO('MaSoLin')
lcOrdLnTag = ORDER('MaSoLin')
SET ORDER TO TAG MaSoLin IN MaSoLin

IF SEEK('O' + lcOrder + STR(lnInvLinNo,6),'MaSoLin')
  lnOrdLnPrc = MaSoLin.nSellNetPr
ENDIF

SET ORDER TO (lcOrdLnTag) IN MaSoLin
IF BETWEEN(lnOrdRecNo,1,RECCOUNT('MaSoLin'))
  GOTO lnOrdRecNo IN MaSoLin
ENDIF
RETURN lnOrdLnPrc
*-- End of lfUpdPrice.

*!***************************************************************************
*! Name      : lfUpdGrPrc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/29/2001
*! Purpose   : Get the price from Order Line file.
*!***************************************************************************
*! Example   : lfUpdGrPrc()
*!***************************************************************************
*N000388,1 
FUNCTION lfUpdGrPrc
PARAMETER lcOrder , lnInvLinNo
PRIVATE lnOrdLnPrc , lnOrdRecNo , lcOrdLnTag
lnOrdLnPrc = 0
lnOrdRecNo = RECNO('MaSoLin')
lcOrdLnTag = ORDER('MaSoLin')
SET ORDER TO TAG MaSoLin IN MaSoLin

IF SEEK('O' + lcOrder + STR(lnInvLinNo,6),'MaSoLin')
  lnOrdLnPrc = MaSoLin.nSellPrice
ENDIF

SET ORDER TO (lcOrdLnTag) IN MaSoLin
IF BETWEEN(lnOrdRecNo,1,RECCOUNT('MaSoLin'))
  GOTO lnOrdRecNo IN MaSoLin
ENDIF
RETURN lnOrdLnPrc
*-- End of lfUpdGrPrc.

*!*************************************************************
*! Name      : lfGetGmSz
*! Developer : HEnd Ghanem (HBG)
*! Date      : 12/10/2001
*! Purpose   : Function To Size discreption
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfGetGmSz()
*!*************************************************************
*!
FUNCTION lfGetGmSz
PARAMETER lcPackSize

IF !EMPTY(lcPackSize) AND SEEK('S'+LEFT(lcPackSize,1),'SCALE')
  lcSz = RIGHT(lcPackSize,1)
  lcSize = SCALE.Sz&lcSz
ELSE
  lcSize = '*****'    
ENDIF  

RETURN lcSize 

*!**************************************************************************
*! Name      : lfObsColor
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/16/2002
*! Purpose   : Function to Check for the copyied colors if
*!             Status is obsolete for one or more color.
*!**************************************************************************
*! Example   : =lfObsColor()
*!**************************************************************************
*N000388,1 
FUNCTION lfObsColor

Private lnAlias , laColorObs , lcColorObs , llColorObs , llAllClrOs , llOneClrOs , llNewClor

DECLARE laColorObs[1,2] ,laObsColor[1]
STORE '' TO lcColorObs
laColorObs[1,1] = 'LLOBSOLETE'
laColorObs[1,2] = 'llColorObs'
STORE .T. TO  llAllClrOs 
STORE .F. TO  llOneClrOs,laObsColor,llNewClor

lnAlias = SELECT(0)
SELECT Fabric
= SEEK(m.Fabric)
SCAN REST WHILE Fabric + Color  = m.Fabric
  *-- Store Emty to the next Variable.
  STORE .F. TO llColorObs
  *-- Get the color from the style file.
  lcColorObs = Fabric.Color
  = gfRltFld(lcColorObs , @laColorObs, 'COLOR'))
  *-- check for this color if Obsolete.
  llAllClrOs = IIF(!llAllClrOs,llAllClrOs,llColorObs)
  llOneClrOs = IIF(llOneClrOs,llOneClrOs,llColorObs)
  IF llColorObs
    DECLARE laObsColor [ALEN(laObsColor)+IIF(!llNewClor,0,1)]
    laObsColor[ALEN(laObsColor)] = lcColorObs
    llNewClor = .T.
  ENDIF
ENDSCAN

DO CASE
  *-- Check if all color are obsolete yes , don't let the user to
  *-- Copy this style.
  CASE llAllClrOs
    *- Message Text   :- All Style colors are Obsolete, Can not copy this style.
    *- Message Number :- 42224.
    *- button message :- OK.
    *- button Number  :- 42001
    = gfModalGen('INM42224B42001','DIALOG','item'+'|'+'copy this item.')
    m.Fabric = ''
    _CUROBJ  = OBJNUM(m.Fabric)

  *-- Check if at least one color was obsolete , let the user know that
  *-- By a message and don't copy this color for the new style.
  CASE llOneClrOs
    *- Message Text   :- One or more of the style colors will not be 
    *- Message Text   :- copied because their statuses are obsolete.
    *- Message Number :- 42225
    *- button message :- OK.
    *- button Number  :- 000000
    = gfModalGen('INM42225B000000','DIALOG','item')
ENDCASE
SELECT (lnAlias)
RETURN
*-- End of lfObsColor.

*!**************************************************************************
*! Name      : lfCarWeigh
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/16/2002
*! Purpose   : The new validation for cartons and weight.
*!**************************************************************************
*! Example   : =lfCarWeigh()
*!**************************************************************************
*N000388,1 This function is not used anymore
FUNCTION lfCarWeigh
IF m.Cartons <> &lcInvHdr..Cartons OR m.Weight <> &lcInvHdr..Weight
  PRIVATE lcAlias
  lcAlias = ALIAS()
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Cartons WITH m.Cartons , Weight WITH m.Weight  
  UNLOCK
  SELECT (lcAlias)
ENDIF
*-- End of lfCarWeigh.

*!**************************************************************************
*! Name      : lfSellConv
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/16/2002
*! Purpose   : The new validation for Sell conversion factor.
*!**************************************************************************
*! Example   : =lfSellConv()
*!**************************************************************************
*N000388,1 This function is not used anymore as the field is disabled on screen.
FUNCTION lfSellConv
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT (lcInvLine)

IF m.nSellConv <= 0
  m.nSellConv = lcOldValue
  SHOW GET m.nSellConv
  _CUROBJ = OBJNUM(m.nSellConv)
  RETURN
ENDIF

IF m.nSellConv <> &lcInvLine..nSellConv
  IF INLIST(laSetups[7,2],'F','I','L')
    IF m.FabQty*m.nSellConv > FabDye.OnHand
      *-- Message : 04025
      *-- Shipped Quantity cannot be greater than On Hand Quantity.
      *-- Quantity (99999).
      *-- Button : 00000 
      *-- OK
      =gfModalGen('TRM04025B00000','ALERT','Shipped Quantity'+'|'+'On Hand Quantity.')        
      m.nSellConv = lcOldValue
      SHOW GET m.nSellConv
      _CUROBJ = OBJNUM(m.nSellConv)
      RETURN
    ENDIF
  ENDIF
ENDIF

SELECT (lcInvLine)
=RLOCK()
REPLACE nSellConv WITH m.nSellConv
UNLOCK  

SELECT (lcAlias)
*-- End of lfSellConv.

*!**************************************************************************
*! Name      : lfvRoll
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : called from the roll button to call the roll screen
*!**************************************************************************
*N000388,1
FUNCTION lfvRoll

PRIVATE lcKey , lnAdjStk , lcFDyelot , lcTrType , lnAlias , ldTrDate ,;
        ldPostDate , lcTrCode , lcRollNP , lcRollRP , lcFabric ,;
        lcColor , lcLineKey , lnOldQty


*B606163,1 ABD - Save the fabric conv. & the Program Name to use it in the gfmatcrl. [Begin]
PRIVATE lnUseConv , lcCalProg
lcCalProg = 'ARMINV'
*B606163,1 ABD - [End]

*N000388,1 Always force not to be end  of file. [Begin]
IF EOF(lcInvLine)
  GO TOP IN (lcInvLine)
ENDIF  
SET KEY TO laData[4]+laData[1]+laData[5]+laData[3]
*N000388,1 Always force not to be end  of file. [End]
  
lnQtyBuy = m.FabQty
lnType = 1  
lcKeyType = 'R'
lcType = 'Return #'
lcFabric = m.Fabric
lcColor  = m.Color
lcLineKey = laData[4]+laData[1]+laData[5]
lnAlias = SELECT(0)
lnOldQty = 0

*B606163,1 ABD - Save the fabric conv. to use it in the gfmatcrl. [Begin]
lnUseConv = IIF(Fabric.Conv = 0 , 1, Fabric.Conv)
*B606163,1 ABD - [End]

=gfOpenFile(gcDataDir+'MatInvJl',gcDataDir+'MatInvJl','SH')
=gfOpenFile(gcDataDir+'Rolls',gcDataDir+'RolApl','SH')

IF lnQtyBuy = 0 AND !lfvQty()
  RETURN
ENDIF

lcTrType  = '5'
lcFDyelot = m.Dyelot
llDyeLvl  = laSetups[8,1] = "Y"
lcRollNP  = lcSelecP
lcRollRP  = lcUnSelP
ldTrDate  = ldDefInvDate
ldPostDate = IIF(laSetups[4,2]='Y',ldDefPstDate,ldTrDate) &&-- Ask MAN
lcTrCode   = &lcInvLine..cMOrder

*B606163,1 ABD - Save the Line No. [Begin]
*lnLineNo   = 0
lnLineNo = &lcInvLine..LinenO
*B606163,1 ABD - [End]

lcFJlSess  = lcSession

*- Abdou
*lcWareCode = laWareHouses[lnWareHouse,2]
lcWareCode = &lcInvLine..cWareCode
*- Abdou

llWareHous = laSetups[5,2]='Y'
lcToWare   = ''

lcKey      = m.Fabric + m.Color
lnAdjStk   = m.FabQty

*B606163,1 ABD - return to old value. [Begin]
*lnAdjstk1 = lnAdjStk
*lnAdjstk  = lfGtAdjStk()
*lnAdjStk2 = lnAdjstk
*lnAdjStk = lnAdjStk * -1
*lnOldAdjStk = lnAdjstk1
lnOldAdjStk = lnAdjStk
lnAdjStk = lnAdjStk * -1

*-- Check for the cost meth.
IF laSetups[7,2] $ 'L'
  lnOldAls = SELECT (0)
  IF USED(lcTrnTemp) .AND. USED(lcTmpJour)
    = lfUpdetfle ()
    SELECT (lcTrnTemp)
    DELE ALL
    APPEND FROM (gcWorkDir+lcTmpJour+'.DBF')
  ELSE  
    *- Add new field call - niSsue
    SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,;
           cTran,cTranType,dTranDate,dPostDate,nUnitCost,nUntCstBuy     ,;
           SUM(nReceived-nIssued) AS 'nBalance'                         ,;
           SUM(nReceived) AS nReceived                                  ,;
           SUM(nIssued)   AS nIssued                                    ,;
           00000000.00 As 'nApply','' AS RolTranCd ,00000000.00 As 'niSsue',;
           "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nMPrvSQty,nPrvSVal  ;
    FROM   MATINVJL                                                      ;
    WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession =        ;
           lcFabric+lcColor+lcWareCode+lcFDyelot+MatInvJl.cRSession      ;
    GROUP BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
             MatInvJl.cDyelot,MatInvJl.cRSession                         ;
    HAVING nBalance <> 0                                                 ;
    ORDER BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
             MatInvJl.cDyelot,MatInvJl.cRSession                         ;
    INTO DBF (gcWorkDir+lcTrnTemp)

    INDEX ON  cFabric+cColor+cWareCode+cDyelot+cRSession TAG (lcTrnTemp) OF (gcWorkDir+lcTrnTemp)
  ENDIF
  
  *-- Check if this fabric is roll yes.
  SELECT (lnOldAls)
ENDIF
*B606163,1 ABD - [End]


lcCostMeth = laSetups[7,2]



*B606163,1 ABD - Call the Screen from the root directry. [Begin]
*DO lfJrRoData IN (gcAppHome+gcWinAppl+'\ARMtInv.PRG') WITH .T. , lcLineKey
DO lfJrRoData.PRG IN gfMatCrl.prg WITH .T.
*B606163,1 ABD - [End]

*B606163,1 ABD - remark the next line. [Begin]
*lnAdjStk  = ABS(lnAdjStk) + lnAdjstk1 - (lnAdjstk2)
*lnAdjStk = lnAdjStk * -1

*-- Function To Create 2 Temp Files and temp file for Every SO#
IF laSetups[7,2] $ 'L'
  = lfCreatTmp ()
  llFirstTime = .F.
ENDIF

*B606163,1 ABD - [End]

IF lnOldAdjStk <> ABS(lnAdjStk) AND lcKeyType  # 'P'
  
  *-- Adjust the Qty
  lnQtyBuy  = ABS(lnAdjStk)

  *-- Save the old Qty if you issue return.
  lnOldQty = lnOldAdjStk
  =lfvQty()
ELSE
  lnQtyBuy  = ABS(lnAdjStk)
ENDIF
lcTmpRolls = lcTmpRoll

=gfCloseFile('MatInvJl')
=gfCloseFile('Rolls')
SELECT(lnAlias)
*-- End of lfvRolls.

*!**************************************************************************
*! Name      : lfvQty
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the quantity field
*!**************************************************************************
*N000388,1
FUNCTION lfvQty
PRIVATE lnAlias,lcKey

IF lnQtyBuy <= 0
  =gfModalGen('INM36014B36000','DIALOG','Quantity')  
  lnQtyBuy = lnOldQty
  _CUROBJ  = OBJNUM(m.FabQty)
  RETURN .F.
ENDIF
lnAlias = SELECT()
m.FabQty = lnQtyBuy

SELECT (lcInvLine)
*REPLACE FabQty WITH lnQtyBuy
_CUROBJ = OBJNUM(m.FabQty)
KEYBOARD "{ENTER}" PLAIN CLEAR
*=lfvSizeQty()
*=lfwBrow()

*-- check if the quantity changed and the user has already entered rolls give a warning message
*lcKey = lcFabric+lcColor
*=SEEK(lcKey,'Fabric')
*IF lnQtyBuy <> lnOldQty AND (!llTrkRolls OR !Fabric.lTrkRolls) AND laSetups[7,2] $ 'LFI' AND;
*  SEEK(lcKey+laData[7]+m.Dyelot,lcTmpJour)
*  =gfModalGen('INM36092B00000','ALERT','lots')
*ENDIF

SELECT(lnAlias)
*-- End of lfvQty.

*:****************************************************************
*: Name      : lfGtAdjStk
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/25/2002
*: Purpose   : Create the temp files.
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfGtAdjStk()
*:****************************************************************
*B606163,1 ABD - [Begin]
*Note :- Now we didn't use this function.
*B606163,1 ABD - [End]
FUNCTION lfGtAdjStk
PRIVATE lnAlias , lnAdjQty , lnLastRec , lcOldOrder
lnAdjQty  = 0
lnAlias   = SELECT(0)

SELECT(lcInvLine)
*-- Save the Recored No.
lnLastRec   = RECNO()
lcOldOrder  = ORDER()
SET ORDER TO lot_Roll
LOCATE

LOCATE REST WHILE  Fabric+Color+Dyelot+cWareCode =  M.Fabric+ M.Color+  m.Dyelot + ladata[7]  ;
   FOR Rolls_Old = 'OLD'
IF BETWEEN(lnLastRec,1,RECCOUNT())
  GOTO  lnLastRec
ENDIF
REPLACE Rolls_Old WITH 'OLD'

IF FOUND ()
  = SEEK(M.Fabric+ M.Color+  m.Dyelot + ladata[7])
  SCAN REST WHILE Fabric+Color+Dyelot+cWareCode =  M.Fabric+ M.Color+  m.Dyelot + ladata[7];
    FOR Rolls_Old = 'OLD'
    lnAdjQty = lnAdjQty + Fabqty
  ENDSCAN
ELSE
  lnAdjQty = lnAdjQty + Fabqty
ENDIF

IF BETWEEN(lnLastRec,1,RECCOUNT())
  GOTO  lnLastRec
ENDIF
SET ORDER TO &lcOldOrder

*N000388,1 Always force not to be end  of file. [Begin]
IF EOF(lcInvLine)
  GO TOP IN (lcInvLine)
ENDIF  
SET KEY TO laData[4]+laData[1]+laData[5]+laData[3]
*N000388,1 Always force not to be end  of file. [End]  

SELECT (lnAlias)
RETURN lnAdjQty

*-- End OF lfGtAdjStk
*:****************************************************************
*: Name      : lfUpdLots
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/25/2002
*: Purpose   : Create the temp files.
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfUpdLots ()
*:****************************************************************
*B606163,1 ABD - [Begin]
FUNCTION lfUpdLots
Private lnAlias , lcSeekExp
lcSeekExp = ''

lnAlias  = SELECT (0)

lcSeekExp = &lcInvLine..Fabric + &lcInvLine..Color + &lcInvLine..cWarecode;
            + &lcInvLine..Dyelot
*-- Check if fabric keep Track lots.
IF USED(lcTmpJour)
  SELECT (lcTmpJour)
  *-- Seek into lcTmpJour & try to know If we add thosse recored or not.
  *-- cFabric+cColor+cWareCode+cDyelot+cRsession+cIsession
  
  IF SEEK(lcSeekExp)
  
    SCAN REST WHILE cFabric+cColor+cWareCode+cDyelot+cRsession+cIsession;
                  = lcSeekExp
     DELETE
    ENDSCAN
    LOCATE
  ENDIF
  
  *-- If fabric Keep trake Rolls.
  
  IF Fabric.ltrkrolls
    *-- Delete the rolls Qty
    IF USED(lcFullRoll)
      *-- Save Old Order For the FullRoll File.
      SELECT (lcFullRoll)
      lcOldRlOdr = ORDER()
      SET ORDER TO lcFullRoll
      IF SEEK(lcSeekExp)
         SCAN REST WHILE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) ;
                       = lcSeekExp FOR lineNo = &lcInvLine..LineNo
           DELETE
         ENDSCAN
      ENDIF
      SET ORDER TO &lcOldRlOdr
      LOCATE
    ENDIF
    
    *-- Delete the rolls Qty
    IF USED(lcTmpRoll)
      *-- Save Old Order For the FullRoll File.
      SELECT (lcTmpRoll)
      lcOldRlOdr = ORDER()
      SET ORDER TO lcTmpRoll2
      *-- Scan And Delete the Rolls
      IF SEEK(lcSeekExp)
         SCAN REST WHILE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) ;
                       = lcSeekExp FOR lineNo = &lcInvLine..LineNo
           DELETE
         ENDSCAN
      ENDIF
      SET ORDER TO &lcOldRlOdr
      LOCATE
    ENDIF
  ENDIF
ENDIF

SELECT (lnAlias)

*-- End OF lfUpdLots
*B606163,1 ABD - [End]

*:*************************************************************
*: Name       : lfCreatTmp
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 06/25/2002
*: Purpose    : Create temp file lcTmpJour.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfCreatTmp()
*:*************************************************************
*B606163,1 ABD - [Begin]
FUNCTION lfCreatTmp
PRIVATE lnAlias ,lcTrnExp , lcSeekExp

lnAlias = SELECT (0)
STORE '' To lcTrnExp , lcSeekExp 

*-- Function To Create Temp File For current So record
IF !USED(lcTempOrdr)
  = lfCrtTmp ()
ENDIF

IF llFirstTime
  SELECT (lcTempOrdr)
  APPEND FROM (gcWorkDir+lcTmpJour+'.DBF')
  REPLACE ALL cMorder With lcTrCode
  REPLACE ALL LineNo  With &lcinvLine..LineNo
ELSE
  IF USED(lcTmpJour)
    SELECT (lcTmpJour)
    SCAN
      lcSeekExp = lcTrCode+cFabric+cColor+cWareCode+cDyelot+cRsession+cIsession
      lcTrnExp  = cFabric+cColor+cWareCode+cDyelot+cRsession
      IF SEEK(lcSeekExp,lcTempOrdr) .AND. &lcTempOrdr..LineNo   = &lcInvLine..LinenO
        IF SEEK(lcTrnExp,lcTrnTemp) .AND. &lcTmpJour..nApply - &lcTrnTemp..nApply # 0
          SELECT (lcTempOrdr)
          REPLACE nApply WITH MAX(nApply + &lcTmpJour..nApply - &lcTrnTemp..nApply,0)
          SELECT (lcTmpJour)
        ENDIF  
     ELSE
        IF !SEEK(lcTrnExp,lcTrnTemp) .OR. &lcTmpJour..nApply - &lcTrnTemp..nApply # 0
            SCATTER MEMVAR MEMO
            SELECT (lcTempOrdr)
            APPEND BLANK
            GATHER MEMVAR MEMO
            REPLACE nApply WITH MAX(&lcTmpJour..nApply - &lcTrnTemp..nApply,0),;
                        cMorder WITH lcTrCode                                 ,;
                        LineNo  With &lcinvLine..LineNo
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ENDIF

*-- Update the Rolls file
IF Fabric.ltrkrolls .AND. USED(lcTmpRoll)
  *-- Function To Create Temp File For current So record
  IF !USED(lcRollTmp)
    = lfCrtRlTmp ()
  ENDIF

  
  IF llFirstTime
    SELECT (lcRollTmp)
    APPEND FROM (gcWorkDir+lcTmpRoll+'.DBF')
    REPLACE ALL LineNo  With &lcinvLine..LineNo
  ELSE
    *-- Create Detail Rolls File.
     SELECT (lcTmpRoll)
     SCAN
       lcSeekExp = lcTrCode+cRollID+cFabric+cColor+cWareCode+cDyelot+STR(&lcInvLine..lineNo,6)+cRsession
       IF SEEK(lcSeekExp,lcRollTmp)
         SELECT (lcRollTmp)
         REPLACE nApply WITH &lcTmpRoll..nApply
       ELSE
         SCATTER MEMVAR MEMO
         SELECT (lcRollTmp)
         APPEND BLANK
         GATHER MEMVAR MEMO
         REPLACE cMorder WITH lcTrCode          ,;         
                 LineNo  With &lcinvLine..LineNo
         SELECT (lcTmpRoll)
       ENDIF
   
     ENDSCAN
   
  ENDIF 
ENDIF

SELECT (lnAlias)

*-- End OF lfCreatTmp.
*B606163,1 ABD - [End]

*:*************************************************************
*: Name       : lfCrtTmp
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 05/25/2002
*: Purpose    : Create temp file lcTmpJour.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfCrtTmp()
*:*************************************************************
*:
FUNCTION lfCrtTmp
PRIVATE lnAlias

lnAlias = SELECT(0)

IF !llMatInvJl
  llMatInvJl=gfOpenFile(gcDataDir+'MATINVJL', 'MATINVJL', 'SH')
ENDIF

SELECT MATINVJL
= AFields(laFileStru)
lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'cMarker'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'cSele'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'nApply'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 8
laFileStru[lnNewFld,4] = 2

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'nBalance'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 8
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'roltrancd'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'lstatus'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'lneeded'
laFileStru[lnNewFld,2] = 'L'
laFileStru[lnNewFld,3] = 0
laFileStru[lnNewFld,4] = 0


lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'cMOrder'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 6
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'niSsue'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 8
laFileStru[lnNewFld,4] = 2


DIMENSION laTagArr[2,2]
laTagArr[1,1] = 'cMOrder+cFabric+cColor+cWareCode+cDyelot+cRsession+cIsession'
laTagArr[1,2] = lcTempOrdr

*N000388,1 Change the index. [Begin]
*laTagArr[2,1] = 'cFabric+cColor+cWareCode+cDyelot+cRsession'
laTagArr[2,1] = 'cFabric+cColor+cWareCode+cDyelot+cMOrder+STR(LineNo,6)'
*N000388,1 Change the index. [End]

laTagArr[2,2] = 'lcTempOrdr'

= gfCrtTmp(lcTempOrdr,@laFileStru,@laTagArr,lcTempOrdr)

SELECT (lcTempOrdr)
lnAlias = SELECT(0)

*-- End OF lfCrtTmp
*B606163,1 ABD - [End]

*:*************************************************************
*: Name       : lfUpdetfle
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 06/25/2002
*: Purpose    : update the temp file lcTmpJour.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfUpdetfle()
*:*************************************************************
*B606163,1 ABD - [Begin]
FUNCTION lfUpdetfle
PRIVATE lnAlias , lcSeekExp , lcSeekRoll

lnAlias = SELECT (0)
 
lcSeekExp  = &lcInvLine..cMorder + &lcInvLine..Fabric + &lcInvLine..Color + &lcInvLine..cWareCode + ;
             &lcInvLine..dyelot
lcSeekRoll = &lcInvLine..Fabric + &lcInvLine..Color + &lcInvLine..cWareCode + &lcInvLine..dyelot

SELECT (lcTempOrdr)
IF SEEK(lcSeekExp)
  LOCATE REST WHILE cMorder + Fabric + Color + cWareCode +Dyelot = lcSeekExp ;
    FOR STR(LineNo,6) = STR(&lcInvLine..lineNo,6)
  IF FOUND()
    SCAN REST WHILE cMorder + cFabric+cColor+cWareCode+cDyelot+cRsession+cIsession = lcTrCode;
                FOR LineNo = &lcInvLine..lineNo
      *-- Update the lctmpJour With last napply Qty.
      IF SEEK(cFabric+cColor+cWareCode+cDyelot+cRsession,lcTmpjour)
        SELECT (lcTmpJour)
        REPLACE nApply With &lcTempOrdr..napply
        SELECT (lcTempOrdr)
      ENDIF
    ENDSCAN
  ELSE
    SELECT (lcTmpJour)
    REPLACE ALL napply With 0
  ENDIF  
ELSE
  SELECT (lcTmpJour)
  REPLACE ALL napply With 0
ENDIF

*-- For the Rolls File.
IF Fabric.ltrkrolls .AND.  USED(lcTmpRoll)
  SELECT (lcTmpRoll)
  lcOldOrder = ORDER()
  SET ORDER TO lcTmpRoll2
  LOCATE
  *-- Check for the order.
  lcSeekExp = [cRollID+cFabric+cColor+cWareCode+cDyelot+STR(&lcInvLine..lineNo,6)+cRsession]
  IF SEEK(lcTrCode+&lcSeekExp,lcRollTmp)
    SELECT(lcTmpRoll)
    = SEEK(lcSeekRoll)
    SCAN REST WHILE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) =  lcSeekRoll
      IF SEEK(lcTrCode+crollId+lcSeekRoll+STR(&lcInvLine..lineNo,6),lcRollTmp)
        REPLACE nApply WITH &lcRollTmp..nApply
      ENDIF  
    ENDSCAN
  ELSE
    REPLACE ALL nApply WITH 0
    IF USED(lcFullRoll)
      SELECT (lcFullRoll)
      REPLACE ALL nApply WITH 0
    ENDIF
  ENDIF
  SELECT (lcTmpRoll)  
  SET ORDER TO &lcOldOrder
  
ENDIF

SELECT (lnAlias)

*-- End Of lfUpdetfle
*B606163,1 ABD - [End]
*:*************************************************************
*: Name       : lfCrtRlTmp
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 06/25/2002
*: Purpose    : Function to Create Temp roll order.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfCrtRlTmp()
*:*************************************************************
*B606163,1 ABD - [Begin]
FUNCTION lfCrtRlTmp
PRIVATE lnAlias , lcSeekExp , lcSeekRoll

lnAlias = SELECT (0)
SELECT (lcTmpRoll)
=AFIELDS(laFileStru)

DIMENSION laTagArr[4,2]
laTagArr[1,1] = 'cMorder+cRollID+cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6)+cRsession'
laTagArr[1,2] = lcRollTmp
laTagArr[2,1] = 'cFabric+cColor+cWareCode+cDyelot+cRsession'
laTagArr[2,2] = 'lcRollTmp2'
laTagArr[3,1] = 'cMorder+cFabric+cColor+cWareCode+cDyelot+cRsession'
laTagArr[3,2] = 'lcRollTmp3'
laTagArr[4,1] = 'cFabric+cColor+cWareCode+cDyelot+cMorder+STR(LineNo,6)'
laTagArr[4,2] = 'lcRollTmp4'

=gfCrtTmp(lcRollTmp,@laFileStru,@laTagArr)
SELECT (lnAlias)

*-- End OF lfCrtRlTmp.
*B606163,1 ABD - [End]

*:*************************************************************
*: Name       : lfChkRolls
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 06/25/2002
*: Purpose    : get needed record from the material Inv Journal.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfChkRolls()
*:*************************************************************
*B606163,1 [Begin]
FUNCTION lfChkRolls
PRIVATE lnAlias , llReturn , lcOldOrder , lnOldRecR , lnOldRecJ ,;
        lnOldInvHd , lnOldInvLn

lnAlias = SELECT (0)
llReturn = .T.

lnOldInvHd = RECNO(lcInvHdr)
lnOldInvLn = RECNO(lcInvLine)

IF USED(lcTmpRoll)
  lnOldRecR = RECNO(lcTmpRoll)
  lcOldOrder = ORDER (lcTmpRoll)
  SET ORDER TO lcTmpRoll2 IN (lcTmpRoll)
ENDIF

IF !USED(lcTmpJour)
  =gfModalGen('TRM40177B00000','ALERT','the Lots and/or Rolls quantities for all the lines')
  SELECT(lnAlias)
  RETURN .F.
ELSE    
  lnOldRecJ = RECNO(lcTmpJour)
  SELECT (lcTempOrdr)
  lcTmpOrder = ORDER()
  SET ORDER TO lcTempOrdr
  IF USED(lcRollTmp)
    SELECT (lcRollTmp)
    lcRolTmp = ORDER()
    SET ORDER TO lcRollTmp4
  ENDIF      

  SELECT (lcInvLine)
  
  *N000388,1 No need to collect same fabric , color
  *= lfSumLines()
  *SELECT (lcTmpLine)
  
  SCAN REST WHILE Account+cMOrder+Store+PikTkt+'Y'+SPACE(10) = &lcInvHdr..Account+; 
                  &lcInvHdr..cMOrder+&lcInvHdr..Store+&lcInvHdr..PikTkt+'Y'+SPACE(10) ; 
                  AND FabQty <> 0
    =SEEK(Fabric+Color,'Fabric')
    lcSeekExp = Fabric + Color + cWareCode + PADR(Dyelot,10) + cMOrder + STR(LineNo,6)
    lnApplyQty = 0 
    *- Check if the user Enetr the Qty

    *B606702,1 Fix bug variable 'X10MGKQD' not found. [Begin]
    *IF Fabric.lTrkRolls
    IF Fabric.lTrkRolls AND USED(lcTmpRoll)
    *B606702,1 Fix bug variable 'X10MGKQD' not found. [End]
    
      SELECT (lcRollTmp)
      = SEEK (lcSeekExp)
      SCAN REST WHILE cFabric+cColor+cWareCode+cDyelot+cMOrder+STR(LineNo,6) = lcSeekExp FOR nApply # 0
        lnApplyQty =   lnApplyQty + nApply
      ENDSCAN 
    ELSE
      = SEEK(Fabric+Color+cWareCode+Dyelot+cMOrder+STR(LineNo,6),lcTempOrdr)
      SELECT (lcTempOrdr)
      = SEEK(lcSeekExp)
      *-- Scan For Issue Qty in the temp file.
      SCAN REST WHILE cFabric+cColor+cWareCode+cDyelot+cMOrder+STR(LineNo,6) = lcSeekExp FOR  nApply # 0
        *-- Check if the cuurent Qty.
        lnApplyQty =   lnApplyQty + napply
      ENDSCAN
    ENDIF      
    IF lnApplyQty # &lcInvLine..FabQty
      llReturn = .F.
      =gfModalGen('TRM40177B00000','ALERT','the ' + IIF(Fabric.lTrkRolls,'Rolls','Lots') + ' quantities for Fabric : ' + ALLTRIM(&lcInvLine..Fabric) + ', Color : ' + ALLTRIM(&lcInvLine..Color) + ' in Order # ' + &lcInvLine..cMOrder + IIF(EMPTY(&lcInvLine..Store),'',', Store : ' + &lcInvLine..Store))
      EXIT
    ENDIF
  ENDSCAN
ENDIF

IF USED(lcTmpJour)
  IF BETWEEN(lnOldRecJ ,1,RECCOUNT(lcTmpJour)) 
    GOTO lnOldRecJ IN (lcTmpJour)
  ENDIF  
ENDIF

IF USED(lcTmpRoll)
  SET ORDER TO &lcOldOrder IN (lcTmpRoll)
  IF BETWEEN(lnOldRecR ,1,RECCOUNT(lcTmpRoll)) 
    GOTO lnOldRecR IN (lcTmpRoll)
  ENDIF  
ENDIF
 
SELECT (lcTempOrdr)
SET ORDER TO &lcTmpOrder

IF USED(lcRollTmp)
  SELECT (lcRollTmp)
  SET ORDER TO &lcRolTmp
ENDIF

IF USED(lcInvHdr)
  IF BETWEEN(lnOldInvHd,1,RECCOUNT(lcInvHdr)) 
    GOTO lnOldInvHd IN (lcInvHdr)
  ENDIF  
ENDIF

IF USED(lcInvLine)
  IF BETWEEN(lnOldInvLn,1,RECCOUNT(lcInvLine)) 
    GOTO lnOldInvLn IN (lcInvLine)
  ENDIF
ENDIF

SELECT(lnAlias)
RETURN llReturn
*-- End Of lfChkRolls.
*B606163,1 [End]

*:*************************************************************
*: Name       : lfSumLines
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 04/26/2002
*: Purpose    : Accumulate the lines for the same fabric color.
*:*************************************************************
*: Example    : = lfSumLines()
*:*************************************************************
*B606163,1 No need for this function 
FUNCTION lfSumLines
PRIVATE lnAlias , lnOldRecNo
lnOldRecNo = 1

lnAlias = SELECT (0)
SELECT (lcInvLine)
lnOldRecNo = RECNO()

CREATE TABLE (lcTmpLine) (Store C(8), cMOrder C(6), Fabric C(7), Color C(6), Dyelot C(10), cWareCode C(6) , FabQty N(10,3))
INDEX ON Fabric + Color + Dyelot + cWareCode + CMOrder TAG (lcTmpLine)

IF TYPE('llMaInvScr') = "U" OR llMaInvScr        && If coming from Material Sales Order invoice screen.
ELSE                                           && coming from Material Direct Invoice screen. 
ENDIF

SELECT (lcInvLine)
SCAN FOR Consol <> 'Y'
  SCATTER MEMVAR MEMO
  IF SEEK(Fabric+Color+Dyelot+cWareCode+cMOrder,lcTmpLine)
    REPLACE &lcTmpLine..FabQty WITH &lcTmpLine..FabQty + &lcInvLine..FabQty 
  ELSE  
    INSERT INTO (lcTmpLine) FROM MEMVAR
  ENDIF
ENDSCAN

SELECT (lcInvLine)
IF BETWEEN(lnOldRecNo ,1,RECCOUNT()) 
  GOTO lnOldRecNo
ENDIF
SELECT (lnAlias)
*-- End Of lfSumLines.
*B606163,1 [Begin]

*:*************************************************************
*: Name       : lfCrTemp
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 06/25/2002
*: Purpose    : Create temp file lcTmpJour.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfCrTemp()
*:*************************************************************
*B606163,1 [Begin]
FUNCTION lfCrTemp
PRIVATE lnAlias

lnAlias = SELECT(0)

IF !llMatInvJl
  llMatInvJl=gfOpenFile(gcDataDir+'MATINVJL', 'MATINVJL', 'SH')
ENDIF

SELECT MATINVJL
= AFields(laFileStru)
lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'cMarker'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'cSele'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'nApply'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 8
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'nBalance'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 8
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'roltrancd'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'lstatus'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'lneeded'
laFileStru[lnNewFld,2] = 'L'
laFileStru[lnNewFld,3] = 0
laFileStru[lnNewFld,4] = 0

=gfCrtTmp(lcTmpJour,@laFileStru,[cFabric+cColor+cWareCode+cDyelot+cRsession+cIsession],lcTmpJour)

SELECT (lcTmpJour)
lnAlias = SELECT(0)

*-- End OF lfCrTemp
*B606163,1 [Begin]

*:*************************************************************
*: Name       : lfOldLots
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 06/26/2002
*: Purpose    : get needed record from the material Inv Journal.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfOldLots()
*:*************************************************************
*B606163,1 [Begin]
FUNCTION lfOldLots
PRIVATE llDone,lnAlias,lnLotQty,lnActQty,lcKey

lnAlias = SELECT()

*-- Function to Create the lcTmpJour File.
= lfCrTemp()

llDone  = .T.
lcExp   = &lcInvLine..fabric+&lcInvLine..COLOR+&lcInvLine..cwarecode+PADR(&lcInvLine..Dyelot,10)

SELECT   cTrn_Seq,cFabric, cColor, cWareCode, cDyelot, dTranDate, cTranType     ,;
         lfGetTran('cTran',cfabric+ccolor+cwarecode+cdyelot+crsession+SPACE(6));
         AS cTran                                                      ,;
         cRSession, cISession              ,;
         lfGetTran('nUnitCost',cfabric+ccolor+cwarecode+cdyelot+crsession+SPACE(6));
         AS nUnitCost                 ,;
         lfGetTran('nUntCstBuy',cfabric+ccolor+cwarecode+cdyelot+crsession+SPACE(6));
         AS nUntCstBuy                ,;
         SUM(nReceived) AS nReceived       ,;
         SUM(nIssued)   AS nIssued         ,;
         SUM(nReceived-nIssued) AS 'nBalance' ,;
         00000000 As 'nApply',' 'AS cMarker ,"S" AS lStatus                 ;
FROM     &gcDataDir.MatInvJl                                                      ;
WHERE    cFabric+cColor+cWareCode+cDyelot+cRSession+cISession =             ;
         lcExp  ;
GROUP BY cFabric,cColor,cWareCode,cDyelot,cRSession                         ;
INTO DBF (gcWorkDir+lcTmpMtIv)
INDEX ON cFabric+cColor+cWareCode+cDyelot+cRSession+cISession               ;
TAG      lcTmpMtIv
SELECT (lcTmpMtIv)
DELETE ALL FOR nBalance <= 0
GOTO TOP
IF EOF()
  SELECT(lnAlias)
  RETURN .F.
ENDIF
SCAN
  IF !SEEK(cFabric+cColor+cWareCode+cDyelot+cRsession+cIsession,lcTmpJour)
    SCATTER MEMVAR
    SELECT(lcTmpJour)
    APPEND BLANK
    GATHER MEMVAR
  ENDIF  
ENDSCAN
SELECT(lcTmpJour)
DO CASE
  CASE laSetups[3,2] $ 'LF' 
    SET ORDER TO TAG lcTmpJour ASCENDING
  CASE laSetups[3,2] = 'I'
    SET ORDER TO TAG lcTmpJour DESCENDING
ENDCASE
IF laSetups[3,2] $ 'FI' 
  lnLotQty = &lcInvLine..fabQty
  SELECT(lcTmpJour)
  =SEEK(lcExp)
  SCAN WHILE cFabric+cColor+cWareCode+cDyelot = lcExp
    IF lnLotQty > 0
      IF (nReceived-nIssued) <= lnLotQty
        REPLACE nApply   WITH nReceived-nIssued,;
                nBalance WITH 0
        lnLotQty = lnLotQty - nReceived + nIssued
      ELSE  
        REPLACE nApply   WITH lnLotQty ,;
                nBalance WITH nReceived - (nIssued+lnLotQty)
        lnLotQty = 0
      ENDIF
    ELSE
      EXIT  
    ENDIF
  ENDSCAN
ENDIF
SELECT(lnAlias)
RETURN .T.
*-- End Of lfoldLots.
*B606163,1 [Begin]

*:************************************************************
*: Name       : lfGetTran
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 06/26/2002
*: Purpose    : 
*:*************************************************************
*: Example    : = lfGetTran()
*:*************************************************************
*B606163,1 [Begin]
FUNCTION lfGetTran
PARAMETER lcField, lcExpr
RETURN LOOKUP(MATINVJL.&lcField,lcExpr,MATINVJL.cfabric,'MATINVJL')
*-- End Of lfGetTran
*B606163,1 [Begin]p

*!**************************************************************************
*! Name      : lpChkRolBt
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/16/2002
*! Purpose   : Enable or Disable Rolls button.
*!**************************************************************************
*! Example   : DO lpChkRolBt
*!**************************************************************************
*N000388,1
PROCEDURE lpChkRolBt
PRIVATE lcAlias
lcAlias = ALIAS()

IF USED('Fabric') AND SEEK(m.Fabric+m.Color,'Fabric') AND laSetups[7,2] = 'L'
  IF Fabric.cDye_Flg = 'Y'
    IF EMPTY(m.DyeLot)
      SHOW GET PbRolls DISABLE
    ELSE
      SHOW GET PbRolls ENABLE
    ENDIF
  ELSE
    SHOW GET PbRolls ENABLE
  ENDIF 
ENDIF

SELECT (lcAlias)
*-- End of lpChkRolBt.

*!**************************************************************************
*! Name      : lfvFactor
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/12/2003
*! Purpose   : Valid function of Factor Code.
*!**************************************************************************
*! Calls     : ARIABROW()
*!**************************************************************************
*! Example   :  =lfvFactor()
*!**************************************************************************
*B606702
FUNCTION lfvFactor
PRIVATE lnAlias
lnAlias = SELECT()

IF !USED('SYCFACT')
  =gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')
ENDIF

IF llBrowse OR (!EMPTY(m.cFacCode) AND !SEEK(m.cFacCode,'SycFact'))

  lcBrFields  = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  SELECT SYCFACT
  m.cFacCode = IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,;
                  gnBrFSRow2, gnBrFSCol2,'','','cFacCode','laBrowArr'),;
                  SycFact.cFacCode,SPACE(6))
ENDIF
SELECT (lcInvHdr)
SCAN REST WHILE Account+cmOrder+Store+PikTkt+CDIVISION = laData[4]
  =RLOCK()
  REPLACE cFacCode WITH m.cFacCode
  UNLOCK
ENDSCAN
=lfRefresh(lcWinChrg)
llBrowse = .F.
SELECT (lnAlias)
*-- End of lfvFactor.

*:**************************************************************************
*:* Name        : lfSavRel
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/06/2005
*:* Purpose     : Save Material release order
*:***************************************************************************
*:* Called from : lpSavScr
*:***************************************************************************
*C126361,1 
FUNCTION lfSavRel
PARAMETERS lcHdrFile,lcDetFile,lcUpsFile,lcEngChrg,lcInstHdr,;
           lcInstLine,lcOrdCanLn,lcGlSession,laInvoices,llFromEdi

PRIVATE laInvSetup,laDRltFld,lcCustLink,lcCustSales,lcUntSin,lnInvCount,;
        lcExRSin,lcGlYear,lcGlPeriod,lnNetAmnt,lnComm1,lnComm2,lnNetShipAmnt,;
        lnCOGSAmt,lnHdrComm1,lnHdrComm2,lnAlias,;
        lcInvNo,lcOrdNo,llOrdExist,laAdjStk,lnUseQty

PRIVATE laOtherPar , lnRecCount , lnCurrRec 
lnRecCount = RECCOUNT(lcDetFile)
lnCurrRec  = 0
lnLineNo = 0

DIMENSION laOtherPar[4,2]
laOtherPar[1,1] = 'lnLineNo'
laOtherPar[1,2] = &lcDetFile..LINENO
laOtherPar[2,1] = ''
laOtherPar[2,2] = ''
laOtherPar[3,1] = 'llLastRec'
laOtherPar[3,2] = (lnRecCount = lnCurrRec)
laOtherPar[4,1] = ''
laOtherPar[4,2] = ""

lcSysType = gfGetMemVar('M_SYSTYPE',gcAct_Comp)

lnAlias = SELECT()
DECLARE laInvSetup[11,2]
laInvSetup[1,1] = 'M_LINK_GL'          &&  Check for Gl link
laInvSetup[2,1] = 'M_WareHouse'        &&  use multiple locations Y Or N  Ic Setup
laInvSetup[3,1] = 'M_MATCSTMT'         &&  Get the Fabric cost method          
laInvSetup[4,1] = 'M_DYELOT'           &&  Use Dylot Y Or N      
laInvSetup[5,1] = 'M_DIV_LINK'         &&  GL link codes at Division level
laInvSetup[6,1] = 'M_STY_COM'          &&  Commision at Style level  
laInvSetup[7,1] = 'M_TAX'              &&  use Taxes Y or N
laInvSetup[8,1] = 'XPOSTFINV'          &&  Post Factored invoice to customer
laInvSetup[9,1] = 'XAGINGTYPE'         &&  Aging AR by Date\Terms
laInvSetup[10,1]= 'M_GenOrNum'         &&  Enter order # Manually Y Or N
laInvSetup[11,1]= 'M_TrkRolls'         &&  Keep track of rolls.

=gfGetMemVar(@laInvSetup,gcAct_Comp)

IF laInvSetup[5,2]='Y'
  DECLARE laDRltFld[2,2]               && array to hold the division related fields
  laDRltFld[1,1] = 'LINK_CODE'           
  laDRltFld[1,2] = 'lcCustLink'        && Customer link   
  laDRltFld[2,1] = 'CSLSGLLINK'        
  laDRltFld[2,2] = 'lcCustSales'       && Custmer sales link code 
ENDIF

=gfOpenFile(gcDataDir+'REPCOMM', gcDataDir+'REPCOMM','SH')
=gfOpenFile(gcDataDir+'SALESREP', gcDataDir+'SALESREP','SH')

=gfOpenFile(gcDataDir+'MUPSBOX', gcDataDir+'MUPSBOX','SH')

*-- Temp name for the pick line temp file
SET ORDER TO TAG MaSoLin IN MaSoLin
lnInvCount = 1    && Variable to hold the number of invoices       

SELECT (lcHdrFile)
SCAN
  DIMENSION &laInvoices[lnInvCount] 

  *-- Get The order number manualy or using gfsequence depend on the invoice setup
  lcOrdNo = IIF(EMPTY(&lcHdrFile..cMOrder) .AND. DIRECT_INV,;
            IIF(laInvSetup[10,2]='Y',lfGtOrder(),;
            gfSequence('CMORDER','','',&lcHdrFile..cDivision)),&lcHdrFile..cMOrder)
   
  IF EMPTY(&lcHdrFile..cMatInv)
    *-- Get The Invoice number on the factor sequence if the invoice was factored 
    lcInvNo=IIF(EMPTY(&lcHdrFile..cFacCode),;
            gfSequence('CMATINV','','',&lcHdrFile..cDivision),;
            gfSequence('CFMATINV' ,'','',&lcHdrFile..cDivision))
    
    lcSAlias=ALIAS()
    lnSRecno=RECNO('MINVHDR')
    
    DO WHILE SEEK(lcInvNo,'MInvHdr')
      lcInvNo=IIF(EMPTY(&lcHdrFile..cFacCode),;
           gfSequence('CMATINV','','',&lcHdrFile..cDivision),;
           gfSequence('CFMATINV' ,'','',&lcHdrFile..cDivision))             
    ENDDO 
    SELECT MRELHDR
    APPEND BLANK
    =RLOCK()
    REPLACE cMatInv with lcInvNo
    UNLOCK
    IF RECCOUNT('MInvHdr') >= lnSRecno
       GO lnSRecno IN MInvHdr
    ENDIF
    SELECT (lcSAlias)

    &laInvoices[lnInvCount] = lcInvNo
    lnInvCount = lnInvCount + 1
  ELSE
    IF TYPE("lcInvNo") # "C"
      lcInvNo=&lcHdrFile..cMatInv 
    ELSE
      lcInvNo =IIF(&lcHdrFile..cMatInv < lcInvNo,lcInvNo,&lcHdrFile..cMatInv)
    ENDIF  
    &laInvoices[lnInvCount] = lcInvNo
  ENDIF
  
  SELECT (lcHdrFile)
  
  *-- If the invoice was consolidated invoice give the lines the same invoice number
  IF Consol='Y'
    lcAccount = Account
    lcKeyVal  = EVAL(KEY())
    REPLACE REST cMatInv WITH lcInvNo WHILE Account+cMOrder+Store+PikTkt=lcAccount
    =SEEK(lcKeyVal)
    
    LOCATE REST WHILE EVAL(KEY()) = lcKeyVal
  ENDIF  
  =RLOCK()
  REPLACE nExRate   WITH IIF(nExRate=0,1,nExRate) ,;
          nCurrUnit WITH IIF(nCurrUnit=0,1,nCurrUnit) ,;
          LastLine  WITH IIF(SEEK('O'+lcOrdNo,'MASOLIN'),LastLine,0),;
          cMatInv   WITH lcInvNo ,;
          cMOrder   WITH lcOrdNo
  UNLOCK

  STORE 0  TO lnNetAmnt,lnComm1,lnComm2,lnNetShipAmnt,lnCOGSAmt
  STORE '' TO lcGlYear,lcGlPeriod
  STORE '' TO lcCustLink,lcCustSales
  =SEEK('M'+&lcHdrFile..Account,'Customer')
  
  llConsAcc = &lcHdrFile..Direct_Inv OR &lcHdrFile..Consol='Y' OR ;
              EMPTY(&lcHdrFile..Flag)

  PRIVATE lcCDefLnk,lcSDefLnk     
  IF laInvSetup[5,2]='Y' 
  *-- the GL link was at the division level get the link code of the division     
    =gfRltFld(&lcHdrFile..cDivision,@laDRltFld,'CDIVISION')
    lcCDefLnk=IIF (EMPTY(Customer.Link_Code),'DEFDEF',Customer.Link_Code)
    lcSDefLnk=IIF (EMPTY(Customer.cSlsGlLink),'DEF',Customer.cSlsGlLink)
    lcCustLink  = IIF(EMPTY(lcCustLink),lcCDefLnk,PADR(lcCustLink,6))
    lcCustSales = IIF(EMPTY(lcCustSales),lcSDefLnk,PADR(lcCustSales,3))
  ELSE   
    lcCustLink  = IIF(EMPTY(Customer.Link_Code),'DEFDEF',Customer.Link_Code)
    lcCustSales = IIF(EMPTY(Customer.cSlsGlLink),'DEF',Customer.cSlsGlLink)
  ENDIF
  
  IF !EMPTY(&lcHdrFile..cFacCode) 
    =gfOpenFile(gcDataDir+'Factor',gcDataDir+'Factor','SH')
    IF SEEK(&lcHdrFile..cFacCode,'Factor')      
      lcCustLink =IIF(!EMPTY(Factor.Link_Code),Factor.Link_Code,lcCustLink)
    ENDIF  
  ENDIF   
  
  lcUntSin   = ''  
  *-- lcExRSin  Exchange Rate sign "/" Or "*"  
  lcExRSin   = gfGetExSin(@lcUntSin,&lcHdrFile..cCurrCode)
  llOrdExist = SEEK('O'+lcOrdNo,'MaSoHdr') && In case of sales order invoice 
  *-- get the customer link code from the order header in case of sales order invoice
  lcCustLink = IIF(EMPTY(MaSoHdr.Link_Code),lcCustLink,MaSoHdr.Link_Code) 
  
  SELECT (lcDetFile)
  =SEEK(&lcHdrFile..Account+IIF(llOrdExist,&lcHdrFile..cMOrder,SPACE(6))+;
            &lcHdrFile..Store+&lcHdrFile..PikTkt)
  WAIT 'Updating Material Release Lines...' WINDOW NOWAIT
  
  SET DELETED OFF  
  GO TOP

  SCAN REST WHILE Account+cMOrder+Store+PikTkt = ;
            &lcHdrFile..Account+IIF(llOrdExist,&lcHdrFile..cMOrder,SPACE(6))+;
            &lcHdrFile..Store+&lcHdrFile..PikTkt
            
    IF &lchdrfile..consol ='Y'
      LOCATE REST WHILE Account+cMOrder+Store+PikTkt = ;
            &lcHdrFile..Account+IIF(llOrdExist,&lcHdrFile..cMOrder,SPACE(6))+;
            &lcHdrFile..Store+&lcHdrFile..PikTkt
    ENDIF
    
    SCATTER MEMVAR MEMO
    =SEEK(m.Fabric+m.Color,'Fabric')
    =SEEK(m.Fabric+m.Color+m.cWareCode+SPACE(10),'FabDye')
    
    *-- the stock current value to use it in calculating the Fabric cost
    lnStkVal = Fabric.nStkVal

    m.CostUse   = IIF(laInvSetup[3,2]='A',;
                 IIF(laInvSetup[2,2]='Y',FabDye.nFAve_Cost,Fabric.nFAve_Cost),Fabric.CostUse)

    *-- Get the GL Cost ,GL Sales link code 
    IF laInvSetup[1,2]='Y' .AND. llConsAcc
      *-- Get Sales and Cost link codes
      =SEEK('O'+m.cMOrder+STR(m.LineNo,6),'MASOLIN')
      
      m.Gl_Cost  = IIF(!EMPTY(Fabric.Link_Code),Fabric.Link_Code,'DEFDEF')
      m.Gl_Cost  = IIF(laInvSetup[2,2]='Y' .AND. !EMPTY(FabDye.Gl_Link),FabDye.Gl_Link,m.Gl_Cost)
      *-- Concatinate customer sales link code and Fabric sales like code
      m.Gl_Sales = IIF(EMPTY(MaSoLin.Gl_Sales),lcCustSales+Fabric.cSlsGlLink,MaSoLin.Gl_Sales)

    ENDIF  
    *-- Accumulate Salesreps Commission Amount.
    IF laInvSetup[6,2]='Y'
      lnNetAmnt = ROUND(m.FabQty*m.nSellNetPr,2)*(1-&lcHdrFile..DiscPcnt/100)*(1-&lcHdrFile..Trde_Disc/100)
      lnComm1   = lnComm1 + (lnNetAmnt * m.Comm1 / 100)
      lnComm2   = lnComm2 + (lnNetAmnt * m.Comm2 / 100)
    ENDIF

    *-- Add new ivoice line
    m.Invoice  = &lcHdrFile..cMatInv
    m.cMatInv  = &lcHdrFile..cMatInv
    m.Order    = &lcHdrFile..cMOrder
    m.cMOrder  = &lcHdrFile..cMOrder
    
    *-- Calculate the equivlant amount by the company currency code
    m.nEqvAmnt = ROUND(m.nSellNetPr * m.FabQty &lcExRSin &lcHdrFile..nExRate &lcUntSin &lcHdrFile..nCurrUnit,2)
    m.InvDate  = &lcHdrFile..InvDate
    
    SELECT (lcDetFile)
    SCATTER FIELDS FabQty TO lnUseQty
    lnUseQty = lnUseQty * -1 /IIF(nSellconv=0,1,nSellconv)
    
    PRIVATE lcRefer
    IF EMPTY(&lcHdrFile..cMOrder) AND &lcHdrFile..DIRECT_INV
      lcRefer = 'CUST# '+ &lcHdrFile..Account + "-" + Customer.BTName
    ELSE
      lcRefer = 'CUST# '+ &lcHdrFile..Account + " Mat S.Order " + &lcHdrFile..cMOrder
    ENDIF

    laOtherPar[1,2] = &lcDetFile..LINENO
    lnCurrRec  = lnCurrRec + 1

    *-- Get difference between the old stock value and the new stock value
    lnStkVal  = lnStkVal - Fabric.nStkVal
    
    *-- calculate the new cost
    IF laInvSetup[3,2]='S'
      m.CostUse = Fabric.CostUse
    ELSE
      *C126361,1  TMI [Start] set costuse to 0 in case of release order
      *m.CostUSe = IIF(m.FabQty=0,0,(lnStkVal/IIF(laInvSetup[2,2]='Y',FabDye.OnHand,Fabric.onHand))/IIF(m.nSellConv=0,1,m.nSellConv))
      m.CostUSe = 0
      *C126361,1  TMI [End  ] 
    ENDIF
    
    *-- Cost of goods sold amount
    lnCOGSAmt = lnCOGSAmt + (m.FabQty * m.CostUse)
    
    lcSvOLn = ORDER('MRELLINE')
    SET ORDER TO MRELLINE IN MRELLINE
    m.Store = IIF(m.Consol='Y',m.cConStore,m.Store)
    IF llConsAcc .AND. m.FabQty > 0 
      IF !SEEK(m.cMatInv+STR(m.LineNo,6),'MRELLINE')
        
        SELECT MRELLINE
        IF DELETED(lcDetFile)
          DELETE
        ELSE
          INSERT INTO MRELLINE FROM MEMVAR
        ENDIF
        
      ELSE
        
        SELECT MRELLINE
        IF DELETED(lcDetFile)
          DELETE
        ELSE
          GATHER MEMVAR MEMO
        ENDIF
        
      ENDIF
    ENDIF
    SET ORDER TO &lcSvOLn IN MRELLINE
        
  ENDSCAN
  
  SET DELETED ON 
  
  =gfTraceKey('MRELLINE',lcInvNo,'A')   
  lcStoreFld = IIF(&lcHdrFile..Consol='Y',&lcHdrFile..cConStore,&lcHdrFile..Store)       
    
  WAIT 'Updating Material Release Order Header...' WINDOW NOWAIT
  *-- shipped amount - discount - Trade discount
  lnNetShipAmnt = &lcHdrFile..FabShipAmt*(1-&lcHdrFile..DiscPcnt/100)*(1-&lcHdrFile..Trde_Disc/100)
  *-- sales rep commission 
  lnHdrComm1 = &lcHdrFile..Comm1 
  lnHdrComm2 = &lcHdrFile..Comm2
  IF laInvSetup[6,2]='Y' .AND. lnNetShipAmnt <> 0
  *-- Commission  is at style level
    lnHdrComm1 = lnComm1/lnNetShipAmnt*100
    lnHdrComm2 = lnComm2/lnNetShipAmnt*100
  ENDIF

  SELECT (lcHdrFile)
  SCATTER MEMVAR
  m.Store = IIF(m.Consol='Y',m.cConStore,m.Store)
  m.Comm1     = lnHdrComm1
  m.Comm2     = lnHdrComm2
  *-- Sales rep Commission at Style level or at invoice level   
  m.COMMAMT1  = IIF(laInvSetup[6,2]='Y',lnComm1,lnHdrComm1*lnNetShipAmnt/100)
  m.COMMAMT2  = IIF(laInvSetup[6,2]='Y',lnComm2,lnHdrComm2*lnNetShipAmnt/100)
  m.Link_Code = lcCustLink
  
  m.Invoice   = lcInvNo
  m.cMatInv   = lcInvNo
  
  IF llConsAcc .AND. SEEK(m.cMatInv,'MRELHDR')   &&.AND. EMPTY(MRELHdr.Account)
    lcSVAlias = Alias()
  
    *-- update the invoice header  
    m.Status = 'C'    
    SELECT MRELHdr
    GATHER MEMVAR MEMO
    SELECT (lcSVAlias)    
    =gfTraceKey('MINVHDR',m.cMatInv,'A')
  ENDIF
  
  IF !llConsAcc AND SEEK(m.cMatInv,'MRELHDR')
    IF !EMPTY(MaSoHdr.CustPO)
      PRIVATE lnCurAlias
      lnCurAlias = SELECT(0)
      SELECT MrelHdr
      REPLACE CustPO WITH m.CustPO
      SELECT (lnCurAlias)
    ENDIF
  ENDIF
  
ENDSCAN

WAIT CLEAR
SELECT (lnAlias)
RETURN

*-- end of lfSavRel.

*:**************************************************************************
*:* Name        : lfwPost
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/19/2005
*:* Purpose     : When function for post button
*:***************************************************************************
FUNCTION lfwPost

IF lnActFolder<>1
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Select the List folder to Post')
  RETURN .F.
ENDIF

*-- end of lfwPost.
*:**************************************************************************
*:* Name        : lfPost
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/12/2005
*:* Purpose     : Post a previously created release order
*:***************************************************************************
*C126361,1
FUNCTION lfPost
PRIVATE lcOrder

IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure to Post the Current release order?')<>1
  RETURN
ENDIF  

SELECT (lcInvline)
LOCATE
IF laSetups[7,2] = 'L' .AND. !lfChkRolls()
  =SEEK(ladata[4]+ladata[1]+laData[5],(lcInvHdr))
  RETURN
ENDIF


STORE .F. TO llCancel
STORE gdSysDate TO ldDefInvDate,ldDefPstDate
DO (gcScrDir+gcWinAppl+"\ARINVSES.SPX")
IF llCancel
  RETURN
ENDIF 

SELECT &lcInvHdr
REPLACE InvDate   WITH ldDefInvDate ;
        ShipDate  WITH m.ShipDate   ;
        dPostDate WITH ldDefPstDate

PRIVATE lcMtInv 
lcMtInv = lcRelOrd

llPst2Inv = .T.  && Post a release order to an invoice with the same sequence #
IF gfCPSave()

  *- Release order posted so remove its lines from MRELHDR, MRELLINE files
  SELECT MRELHDR
  GO TOP
  LOCATE FOR CMATINV = lcMtInv 
  DELETE 
  
  
  SELECT MRELLINE
  SET ORDER TO MRELLINE
  =SEEK(lcMtInv,'MRELLINE')
  lcSvDel = SET('DELETED')
  SET DELETED OFF
  DELETE REST WHILE CMATINV = lcMtInv
  SET DELETED &lcSvDel
  
  llPost = .F.    
  SHOW GET pbScope DISABLE
  
  laScrMode = .F.
  laScrMode[1] = .T.
  SHOW GETS
  _CUROBJ = OBJNUM(laData[1])

ENDIF

llPst2Inv = .F.
     
*-- end of lfPost.


*:**************************************************************************
*:* Name        : lfwRelOrd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/14/2005
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwRelOrd()
*:***************************************************************************
FUNCTION lfwRelOrd

*-- end of lfwRelOrd.

*:**************************************************************************
*:* Name        : gfCPDelete
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/15/2005
*:* Purpose     : Cancel the release order
*:***************************************************************************
FUNCTION gfCPDelete

IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure you want to remove the release order# ' + lcRelOrd) = 1

  *- Release order posted so remove its lines from MRELHDR, MRELLINE files
  SELECT MRELHDR
  GO TOP
  LOCATE FOR CMATINV = lcRelOrd 
  DELETE 
  
  SELECT MRELLINE
  SET ORDER TO MRELLINE
  =SEEK(lcRelOrd,'MRELLINE')
  lcSvDel = SET('DELETED')
  SET DELETED OFF
  DELETE REST WHILE CMATINV = lcRelOrd
  SET DELETED &lcSvDel

  SELECT UnCmSess
  IF SEEK('O'+PADR('MRELSEORD',10)+PADR(gcUser_id,10)+lcSession)
    REPLACE STATUS WITH 'X'
    llContinue = .F.
    UNLOCK
  ENDIF
  
  laScrMode    = .F.
  laScrMode[1] = .T.
  SHOW GETS

ENDIF
*-- end of gfCPDelete.


*:**************************************************************************
*:* Name        : lfSpInst
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/15/2005
*:* Purpose     : Add special instruction screen
*:***************************************************************************
FUNCTION lfSpInst
PRIVATE lnSlct , M.MINVNOTE , llOk , lcInvNote
lnSlct = SELECT()

SELECT MRELHDR
M.MINVNOTE = MRELHDR.MINVNOTE  

llOk = .T.
lcInvNt  = gfTempName()
lcSpInst = gfTempName()
DO (gcScrDir+gcWinAppl+"\Arspinst.SPX")

IF llOk
  REPLACE MINVNOTE  WITH M.MINVNOTE
ENDIF

SELECT (lnSlct)
*-- end of lfSpInst.


*:**************************************************************************
*:* Name        : lpPrvScr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/17/2005
*:* Purpose     : define go_top local function 
*:***************************************************************************
FUNCTION lpTopScr
=lfvOrder(MRELHDR.CMATINV)

*-- end of lpTopScr.
*:**************************************************************************
*:* Name        : lpPrvScr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/17/2005
*:* Purpose     : define go_bottom local function 
*:***************************************************************************
FUNCTION lpBtmScr
=lfvOrder(MRELHDR.CMATINV)

*-- end of lpBtmScr.
*:**************************************************************************
*:* Name        : lpPrvScr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/17/2005
*:* Purpose     : define next local function 
*:***************************************************************************
FUNCTION lpNxtScr
=lfvOrder(MRELHDR.CMATINV)

*-- end of lpNxtScr.
*:**************************************************************************
*:* Name        : lpPrvScr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/17/2005
*:* Purpose     : define prev local function 
*:***************************************************************************
FUNCTION lpPrvScr
=lfvOrder(MRELHDR.CMATINV)

*-- end of lpPrvScr.


*!*************************************************************
*! Name      : gfCPEdit
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : Validation of edit button in the pannel
*!*************************************************************
*! Calls     : 
*!          Calls: GFMODALGEN()             (function  in ARIA3.PRG)
*!          Calls: GFOBJ_LOCK()             (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* The valid function for the control panel Top push Edit
*:->
FUNCTION gfCPEdit
EXTERNAL ARRAY laData,laDefProc,laScrMode

ACTIVATE WINDOW GWCCONTRL1 TOP

IF TYPE('lcBaseFile') = "C" 
  IF !EMPTY(lcBaseFile)
    SELECT (lcBaseFile)
  ENDIF
ENDIF


llUpDate=.F.
IF !llEditRec OR IIF(TYPE('COWNER')#'C',.F.,IIF(EMPTY(COWNER),.F.,COWNER<>gcCurSite))
  IF !llEditRec 
    =gfModalGen("TRM00088B00000","ALERT",'Editing') 
  ELSE
    =gfModalGen("TRM00299B00000","ALERT",'Modified')
  ENDIF  
ELSE
  IF EMPTY(lcBaseFile) OR gfObj_Lock(.T.)
    IF !EMPTY(lcBaseFile)
      SCATTER FIELDS &lcScFields MEMO TO laData
      lcStamp       =  cAdd_User+IIF(EMPTY(dAdd_Date),'',DTOC(dAdd_Date))+;
                     cAdd_Time
    ENDIF                  

    laScrMode    = .F.                  && Reset screen mode
    laScrMode[4] = .T.                  && Go to edit mode
    
    =lfvOrder('',.T.)
    
  ENDIF  
ENDIF

SELECT &lcInvHdr
SET KEY TO 
GO TOP 

SELECT &lcInvLine
SET KEY TO 
GO TOP

SELECT &lcInvHdr
IF lnActFolder = 3
  SELECT &lcInvLine
ENDIF

*-- end of gfCPEdit.

*!*************************************************************
*! Name      : gfCPClose
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : Validation of close button in the pannel
*!*************************************************************
*! Calls     : 
*!          Calls: GFMODALGEN()             (function  in ARIA3.PRG)
*!          Calls: LPCLSSCR.PRG             
*!          Calls: GFOBJ_LOCK()             (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* The valid function for the control panel Top push Close
*:->
FUNCTION gfCPClose
EXTERNAL ARRAY laData,laDefProc,laScrMode
EXTERNAL PROCEDURE lpClsScr
IF WEXIST('GWCCONTRL1') AND WREAD('GWCCONTRL1')
  ACTIVATE WINDOW GWCCONTRL1 TOP
ENDIF  

*E300704,1 Hesham El-Sheltawi (Start)
*E300704,1 check if the program have a base file or not 
*E300704,1 before selecting it
*SELECT (lcBaseFile)
IF !EMPTY(lcBaseFile)
  SELECT (lcBaseFile)
ENDIF  
*E300704,1 Hesham El-Sheltawi (END)
IF laScrMode[1] .OR. laScrMode[2]         
  *** Select or view mode
  glQuitting = .T.
  CLEAR READ
  RETURN
ELSE
  IF llCUpDate                       && If any object is updated 
    glChildTop = (SUBSTR(WLAST(),1,2)="CW")
    IF gfModalGen('QRM00031B00006','Alert') = 2
      RETURN
    ELSE
      IF laDefProc[10]
       *E300704,1 Hesham El-Sheltawi (Start)
       *E300704,1 check if the program have a base file or not 
       *E300704,1 before SCATTERING THE DATA
*      SCATTER FIELDS &lcScFields MEMO TO laData       
       IF !EMPTY(lcBaseFile)
         SCATTER FIELDS &lcScFields MEMO TO laData
       ENDIF  
       *E300704,1 Hesham El-Sheltawi (END)
      ELSE
        DO lpClsScr
      ENDIF  && DefProc
      llUpDate=.F.          
    ENDIF  &&Message
  ENDIF  &&llUpDate

  IF !EMPTY(lcBaseFile)
    =gfObj_Lock(.F.)                       && Unlock the record
  ENDIF  
  IF !laScrMode[2]                   && Go To select mode
    laScrMode    = .F.                   && Reset screen mode
    laScrMode[1] = .T.                   && Go to select mode
  ENDIF  
  llCUpdate =.F.
  SHOW GETS
ENDIF



