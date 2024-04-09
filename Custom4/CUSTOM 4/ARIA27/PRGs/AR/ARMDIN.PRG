*:**************************************************************************
*: Program file  : ArMDIn
*: Program desc. : Material Direct Invoice screen.
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 06/16/2002
*: Reference     : N000391
*:**************************************************************************
*: Notes         : This code is an exact copy of ARDINV.PRG
*:**************************************************************************
*:N000391,1 SSE 05/16/2002 New standard Material Invoice screen.
*:B606163,1 ABD 05/16/2002 Modifications in Rolls screen.
*:B606702,1 SSE 01/12/2003 Fix bug variable 'LACONV' not found.
*:B607432,1 ALB 09/09/2003 Disable new buttom in the multisession screens
*:E126684,1 TMI 03/01/2005 Use CEILING function to round amount up to 2 decimals
*:**************************************************************************
*
PARAMETERS lcInvoiceNo
EXTERNAL ARRAY laData,laKeyField

IF !('MA' $ gcCmpModules)
  *-- Message : 42083
  *-- Material module is not installed, Cannot proceed.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('INM42083B00000','ALERT','Material')
  RETURN
ENDIF

DECLARE lafolders[3,2],laAddress[6,3],laQtyPaste[1,8],lafoldwinds[3,2],;
        laKeyField[1,4],laSetups[20,2],laVars[20],laBrowArr[1]
DECLARE laSetups[22,2]

DECLARE laCodes[6,10],laTerms[1,2],laSeasons[1,2],laDivision[1,2],;
        laShipVia[1,2],laSpcInst[1,2],laTRltFld[6,2],laDRltFld[2,2],;
        laVRltFld[1,2],laWareHouses[1,2]

DECLARE laEngStyTax[1,2]
STORE 0 TO lnTaxRate  && Tax Rate at Style level  
STORE '' TO laEngStyTax

DECLARE laDisRltFld[1,2]
STORE 0   TO lnDisc_Pcnt
STORE '' TO laCodes,laTerms,laShipVia,laSpcInst,laSeasons,laDivision,;
            laVars,lcGlSession,laBrowArr,laWareHouses,lcTaxName,lcOldValue
lcOldStore=SPACE(8)
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .T.
laKeyField[1,3] = 'MINVHDR'
laKeyField[1,4] = 1
STORE .F. TO laSetups,llRebuild
lcDelMesag    = 'void'
laDefProc[7]  = .F.     && Void procedure(lpDelScr)
laDefProc[9]  = .F.     && Save procedure(lpSavScr)
laDefProc[10] = .F.     && close procedure(lpClsScr)

STORE 0    TO lnMarker,lnTopMark,lnBotMark,lnMerchTax,lnChrgtax,laQtyPaste
STORE ' '  TO lcInvBrow ,lcBrTtl2,lcBrowseTl,lcInvStat,lcGlYear,lcGlPeriod,;
              lcSysYear,lcSysPeriod,lcIDefSes,lcIDefDiv,lcPriceLvl,lcWinCh1,;
              lcWinCh2,lcWinCh3,lcWinCh30,lcWinCh31,lcWinCh32,lcWinCh33,;
              lcWinCh34,lcWinCh35,lcWinCh4,lcWinCh5,lcWinCh6,llAlowNew,lcIDefWare,lcCurrInv
              
STORE {} TO ldDefInvDate,ldDefPstDate
STORE 0 TO lnCartons,lnLines ,lnSouComm1,lnSouComm2,lnActFolder,lnStyLngth

*lnCartons = 1

STORE 1 TO lnShipAddr,lnPstRule,lnWareHouse
STORE '' TO lcLastStyle,lcFiles
STORE '' TO  lcDShpName,lcDShpAdd1,lcDShpAdd2,lcDShpAdd3,lcDShpAdd4,lcDShpAdd5,;
             lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
             lcDBllName,lcDBllAdd1,lcDBllAdd2,lcDBllAdd3,lcDBllAdd4,lcDBllAdd5,;
             lcFolder,lcTaxTitle,lcTaxBreak,lcSession,lcUpsType
STORE .F. TO llBrowse,llZoom,lcIDefPrnt,cbSetAsDef,llIsCanada,llOpnCrdt,;
             llIsEngland,llMulCurr,llEditExRt,llupsinsur,llCod,llNewLine,;
             llContinue,llSysDate,llInstTerm,llActCrdt
STORE .T. TO llReBrowse,llAddLine,llReMerTax

PRIVATE lnOldChrg
STORE 0 TO lnOldChrg
STORE ""  TO lcInvLine,lcInvHdr,lcGlDist,lcPackHdr,lcUpsBox,lcEngChrg,;
             lcVScFields,lcScFields,lcWinChrg,lcStyHdr,;
             lcInstHdr,lcInstLin,lcEmptySty,lcAppCrdt,lcUpsTrack,lcTmpAR

STORE ''  TO lcTEOM,lcTCod,lcBaseFild,lcBaseTit,lcScrMode
STORE 20 TO lnEomDay   && End of month day default value
STORE 0   TO lnTDaysDue,lnTaxDue
lcStatColor = gcObjColor
lnSessNo = gnProgCopy
STORE 1   TO lnTerms,lnShipVia,lnSpcInst,lnSeason,lnDivision,lnCanReason
STORE 0   TO lnLastMode
STORE .T. TO llGoAndChk,llChkUnCom

STORE ''  TO lcAdTrnSeq,lcAcTrnSeq,lcHisSeq,lcGlSess,lcRepBat
lnUnCmSeRc = 0

STORE .T. TO llUpdGlDif,llUpdMstGL

*N000391,1 Define 3 Temp files to be used by gfMatCrl function. [Begin]
PRIVATE lnLineNo , llMtInvScr , llTrkRolls
STORE "" TO lcTmpRoll , lcTmpJour , lcFullRoll
STORE .F. TO llTrkRolls , llMtInvScr
lcProg  = 'ARMDIN'
*N000391,1 Define 3 Temp files to be used by gfMatCrl function. [End]

*-- initialize all the global variables and opens the files needed by the program. 
IF !gfSetup()
  RETURN
ENDIF
*B607432,1 ALB Disaple new buttom in the multisession screens [BEGIN]
STORE 0   TO lnLastMode
*B607432,1 ALB Disaple new buttom in the multisession screens [END]
IF TYPE('lcInvoiceNo') = 'C' .AND. SEEK(lcInvoiceNo,'MINVHDR')
  SELECT MINVHDR
  lnOldOrd = ORDER()
  SET ORDER TO MInvhdra
  PRIVATE lcTempFlt
  lcTempFlt = MINVHDR.Account
  SET FILTER TO Account+cMatInv = lcTempFlt
  SET ORDER TO &lnOldOrd
  GOTO TOP
ENDIF

lcProgID  ='MDINVOICE'
llReBrowse = .T.
lcInvBrow = 'Invoice Lines'
lcBrTtlZ  = 'Zoom Invoice Lines'
lcSize1   = 'Units'

lafolders[1,1] = 'Header'
lafolders[1,2] = 1
lafolders[2,1] = 'Details'
lafolders[2,2] = 2
lafolders[3,1] = 'Charges'
lafolders[3,2] = 3
lnnofld = 3

lcwfoldchng = '=lfActFolder()'  && function to control shows after change the folder
lnfoldercend = 102.000
lnfolderrend = 2.000
STORE 'DISABLE' TO lcpbNoteStat,lcpbObjStat,lcPbSendStat
llNoShow = .F.

IF !WEXIST(gcBaseWind)
  STORE 0 TO lnLastMode
  lcEmptySty = PADR(STRTRAN(GFITEMMASK("PI"),'X',' '),19)
  lcStyHdr   = gfItemMask("HI")
  lnStyLngth = LEN(lcStyHdr) 
  lcScFields = 'CMATINV,ACCOUNT,CMORDER,CUSTPO,STORE,SHIPDATE,DUEDATE,APPROVAL,'+;
               'cTermCode,SHIPVIA,SPCINST,SEASON,cDivision,DEPT,NOTE1,NOTE2,REP1,'+;
               'COMM1,REP2,COMM2,CWARECODE,CCURRCODE,NEXRATE,CFACCODE,CARTONS,'+;
               'WEIGHT,CODTAG,COD_AMT,TRDE_DISC,PIKTKT,FabSHIPAMT,DISCPCNT,'+;
               'DISCOUNT,FREIGHT,INSUR,COD,TAX_AMT,TOTALCHG,NCHARGES,'+;
               'NCURRUNIT,INVDATE,FabSHIP,nPstAmt,cTaxRule,nPstRate,TAX_RATE,'+;
               'BOL_NO,APPRAMT,DPOSTDATE,CCODTRCKNO'
  lcVScFields= 'CMATINV,ACCOUNT,CMORDER,CUSTPO,STORE,SHIPDATE,DUEDATE,APPROVAL,'+;
               'cTermCode,SHIPVIA,SPCINST,SEASON,cDivision,DEPT,NOTE1,NOTE2,REP1,'+;
               'COMM1,REP2,COMM2,CWARECODE,CCURRCODE,NEXRATE,CFACCODE,CARTONS,'+;
               'WEIGHT,CODTAG,VCOD_AMT,TRDE_DISC,PIKTKT,VFabSHPAMT,DISCPCNT,'+;
               'VDISCOUNT,VFREIGHT,VINSUR,VCOD,VTAX_AMT,VTOTALCHG,NvCHARGES,'+;
               'NCURRUNIT,INVDATE,VFabSHIP,nVPstAmt,cTaxRule,nPstRate,TAX_RATE,'+;
               'BOL_NO,APPRAMT,DPOSTDATE,CCODTRCKNO'
  lcScFields  = lcScFields  + ',nHstRate,nHstAmt'
  lcVScFields = lcVScFields + ',nHstRate,nvHstAmt'
               
  SELECT MInvHdr
  SCATTER FIELDS &lcScFields TO laData BLANK
  lcSession   = gfsequence('CSESSION')
  lcGlSession = gfsequence('GLSESSION')
  llAlowNew = .T.
  laSetups[1,1]  = 'M_PACK'         &&  Use style packs / SKU    
  laSetups[2,1]  = 'M_STY_COM'      &&  Commision at Style level   
  laSetups[3,1]  = 'M_LN_NOTE'      &&  Add Note to the invoice line
  laSetups[4,1]  = 'M_LINK_GL'      &&  Check for Gl link
  laSetups[5,1]  = 'M_WareHouse'    &&  use maltiple locations Y Or N  Ic Setup 
  laSetups[6,1]  = 'M_GenOrNum'     &&  Enter order # Manually Y Or N
  laSetups[7,1]  = 'M_MATCSTMT'     &&  Cost Method
  laSetups[8,1]  = 'M_MATDYE  '     &&  Use Dylot Y Or N      
  laSetups[9,1]  = 'M_TAX'          &&  use Taxes Y or N
  laSetups[10,1] = 'M_TAX_RATE'     &&  Tax Rate
  laSetups[11,1] = 'M_TAX_METH'     &&  Tax Method
  laSetups[12,1] = 'M_UPC_USE'      &&  Use U.P.C. Numbers        
  laSetups[13,1] = 'M_DIV_LINK'     &&  GL link codes at Division level
  laSetups[14,1] = 'M_REP_COMM'     &&  Salesrep commission base 
  laSetups[15,1] = 'M_UPSBOX'       &&  Track Boxes for Ups Manifacuring   
  laSetups[16,1] = 'XAGINGTYPE'     &&  Aging AR by Date\Terms 
  laSetups[17,1] = 'XPOSTFINV'      &&  Post Factored invoice to customer
  laSetups[18,1] = 'M_CRDT_LMT'     &&  Warn above Credit limit 
  laSetups[19,1] = 'M_EDTPRICE'     &&  Edit style price Y/N 
  laSetups[20,1] = 'M_TAX_DESC'     &&  Tax Name      
  laSetups[21,1] = 'M_HST_RATE'     && HST Tax Rate
  laSetups[22,1] = 'M_TrkRolls'     && Keep track of rolls.
  =gfGetMemVar(@laSetups,gcAct_Comp)
 
  lcTaxName  = IIF(EMPTY(laSetups[20,2]),'G.S.T. Tax',laSetups[20,2])
  llIsCanada = IIF(UPPER(ALLTRIM(gcContCode))='CANADA',.T.,.F.)

  IF UPPER(ALLTRIM(gcContCode))='ENG'   && in case of England
    *-- Arrat to hold Tax Rate at Style level 
    DECLARE laEngStyTax[1,2]    
    laEngStyTax[1,1] = 'NTAXRATE'
    laEngStyTax[1,2] = 'lnTaxRate'  && Tax Rate at Style level  

    STORE "" TO lcTaxTitle,lcTaxBreak
    llIsEngland = .T.
    *-- variable to hold the temp Name for the english charges file
    lcEngChrg = gfTempName() 
    *-- in case of england Track Boxes for Ups Manifacuring must be No
    laSetups[15,2]='N'
  ELSE
    llIsEngland = .F.
  ENDIF
  llMulCurr   = gfGetMemVar('llMulCurr',gcAct_Comp)   && Use Multi Currency       
  llEditExRt  = gfGetMemVar('LLEDITEXRA',gcAct_Comp)  && Change exch. rates       

  lcBaseFild = "cMatInv :H='Invoice',INVDATE :H='Date',ACCOUNT :H='Account',"+;
               "STORE :H='Store',CMORDER :H='Order',CUSTOMER.STNAME :H='Name':20 ,"+;
               "FabSHIP :H='Pieces',TOTALCHG :H='Amount',"+;
               IIF(laSetups[5,2]='Y',"cWareCode :H='Warehouse',",'')+;
               IIF(llMulCurr,"cCurrCode :H='Currency',NexRate :H='Curr.Rate',",'')+;
               "REP1 :H='Rep1', COMM1 :H='Comm',REP2 :H='Rep2', COMM2 :H='Comm',"+;
               IIF(gfIsEdtble('CTERMCODE'),"cTermCode :H='Terms'",;
               "lcDesc= gfCodDes(cTermCode,'CTERMCODE') :H='Terms'")
  lcBaseTit  = 'Material Invoices'

  lcWinCh1    = gfTempName()
  lcWinCh2    = gfTempName()
  lcWinCh3    = gfTempName()
  lcWinCh30   = gfTempName()
  lcWinCh31   = gfTempName()
  lcWinCh32   = gfTempName()
  lcWinCh33   = gfTempName()
  lcWinCh34   = gfTempName()
  lcWinCh35   = gfTempName()
  lcWinCh4    = gfTempName()
  lcWinCh5    = gfTempName()
  lcWinCh6    = gfTempName()
  lcfolder    = gfTempName()      && Folder Window Name
  lcfoldprnt  = gcBaseWind        && window parent name for the folder
  lnactfolder = 1                 && active folder

  lcWinChrg = IIF(llIsEngland,lcWinCh4,IIF(llIsCanada,lcWinCh5,lcWinCh6))
  lafoldwinds[1,1] = lafolders[1,1]
  lafoldwinds[1,2] = lcWinCh2
  lafoldwinds[2,1] = lafolders[2,1]
  lafoldwinds[2,2] = lcWinCh3
  lafoldwinds[3,1] = lafolders[3,1]
  lafoldwinds[3,2] = lcWinChrg
  *-- get temp names for the files
  lcInvLine = gfTempName()
  lcInvHdr  = gfTempName()
  lcGlDist  = gfTempName()
  lcPackHdr = gfTempName()
  lcInstHdr = gfTempName()
  lcInstLin = gfTempName()
  lcAppCrdt = gfTempName()
  lcTmpAR   = gfTempName()

  lcUpsBox  = IIF(laSetups[15,2]='Y',gfTempName(),'')

  *N000391,1 Define temp files names to be used in gfMatCrl function. [Begin]
  llTrkRolls = ALLTRIM(laSetups[22,2]) = "Y"
  *lcTmpJour = gfTempName()
  *N000391,1 Define temp files names to be used in gfMatCrl function. [End]
  
  SELECT MInvLine
  SCATTER MEMVAR BLANK MEMO
  llNoShow = .F.
  IF TYPE('lcInvoiceNo') = 'C' .AND. SEEK(lcInvoiceNo,'MINVHDR')
    SELECT MINVHDR
    STORE .F. TO laScrMode
    STORE .T. TO laScrMode[2]
  ENDIF
  *-- variable used to update the uncomplete session file
  laVars[1]  = 'ldDefInvDate'
  laVars[2]  = 'lcIDefSes'  
  laVars[3]  = 'lcIDefDiv'
  laVars[4]  = 'lcIDefWare'
  laVars[5]  = 'llRebuild'
  laVars[6]  = 'lcGlSession'
  laVars[7]  = 'ldDefPstDate'  
  laVars[8]  = 'llupsinsur'
  laVars[9]  = 'lcScrMode'
  laVars[10] = 'lcCurrInv'
  laVars[11] = 'lnUnCmSeRc'    && Used by Key off program
  laVars[12] = 'lcAdTrnSeq'    && Used by Key off program
  laVars[13] = 'lcAcTrnSeq'    && Used by Key off program
  laVars[14] = 'lcHisSeq'      && Used by Key off program
  laVars[15] = 'lcGlSess'      && Used by Key off program
  laVars[16] = 'lcRepBat'      && Used by Key off program
  laVars[17] = 'llUpdGlDif'    && Used by Key off program
  laVars[18] = 'llUpdMstGL'    && Used by Key off program
  laVars[19] = 'lnShipAddr'
  laVars[20] = 'llActCrdt'
  *--Payment Terms Code
  laCodes[1,1] = 'CTERMCODE'
  laCodes[1,2] = 'laTerms'
  laCodes[1,3] = 'lnTerms'
  laCodes[1,4] = ''
  laCodes[1,5] = .F.
  laCodes[1,6] = .F.
  laCodes[1,10] = 'cTermCode'
  *-- Payment Terms Related Fields
  laTRltFld[1,1] = 'NTERDISCR'   
  laTRltFld[1,2] = 'laData[29]'  && Trade Discount
  laTRltFld[2,1] = 'EOM'
  laTRltFld[2,2] = 'lcTEOM'      && End of Month Y/N
  laTRltFld[3,1] = 'NTERDUED'
  laTRltFld[3,2] = 'lnTDaysDue'  && Net Due Days
  laTRltFld[4,1] = 'CODYN'       
  laTRltFld[4,2] = 'lcTCod'      && Cash on delivery Y/N
  laTRltFld[5,1] = 'LINSTALLM'
  laTRltFld[5,2] = 'llInstTerm'  && Use Instalment 
  laTRltFld[6,1] = 'EOMDAY'
  laTRltFld[6,2] = 'lnEomDay'    && End Of Month Day
  *-- Ship via  Codes
  laCodes[2,1] = 'SHIPVIA'
  laCodes[2,2] = 'laShipVia'
  laCodes[2,3] = 'lnShipVia'
  laCodes[2,4] = ''
  laCodes[2,5] = .F.
  laCodes[2,6] = .F.
  laCodes[2,10] = 'SHIPVIA'
  *--Special instruction Codes
  laCodes[3,1] = 'SPCINST'
  laCodes[3,2] = 'laSpcInst'
  laCodes[3,3] = 'lnSpcInst'
  laCodes[3,4] = ''
  laCodes[3,5] = .F.
  laCodes[3,6] = .F.
  laCodes[3,10] = 'SPCINST'
  *-- Season codes
  laCodes[4,1] = 'SEASON'
  laCodes[4,2] = 'laSeasons'
  laCodes[4,3] = 'lnSeason'
  laCodes[4,4] = ''
  laCodes[4,5] = .T.
  laCodes[4,6] = .F.
  laCodes[4,10] = 'SEASON'
  *-- Devision  codes
  laCodes[5,1] = 'CDIVISION'
  laCodes[5,2] = 'laDivision'
  laCodes[5,3] = 'lnDivision'
  laCodes[5,4] = ''
  laCodes[5,5] = .F.
  laCodes[5,6] = .F.
  laCodes[5,10] = 'cDivision'
  *-- Devision Related Fields 
  laDRltFld[1,1] = 'LINK_CODE'
  laDRltFld[2,1] = 'CSLSGLLINK'

  laDisRltFld[1,1] = 'DISCPCNT'
  laDisRltFld[1,2] = 'lnDisc_Pcnt'

  laVRltFld[1,1] = 'CUPS'
  laVRltFld[1,2] = 'lcUpsType'
  *-- order cancelation reason codes
  laCodes[6,1] = 'CCANCRESON'
  laCodes[6,2] = 'laCanReason'
  laCodes[6,3] = 'lnCanReason'
  laCodes[6,4] = ''
  laCodes[6,5] = .F.
  laCodes[6,6] = .F.
  laCodes[6,10] = 'CCANCRESON'
ELSE
  DO CASE
    CASE laScrMode[1] 
      STORE 'DISABLE' TO lcpbNoteStat,lcpbObjStat,lcPbSendStat
    CASE laScrMode[2]
      STORE 'ENABLE' TO lcpbNoteStat,lcpbObjStat,lcPbSendStat    
    CASE laScrMode[4]
      STORE 'DISABLE' TO lcpbNoteStat,lcpbObjStat,lcPbSendStat
  ENDCASE  
  SELECT IIF(laScrMode[2],'MInvLine',lcInvLine)
  SCATTER MEMVAR BLANK MEMO
ENDIF
llUpsBox  = laSetups[15,2]='Y' .AND. gfOpenFile(gcDataDir+'MUPSBOX',gcDataDir+'MUPSBOX','SH')
llRep_div = laSetups[14,2]='D' .AND. gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH')
=gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')

SELECT LEFT(cDesc,20),cWareCode FROM WAREHOUS INTO ARRAY laWareHouses

llPOSSetup = ('CM' $ gcComp_Mdl) AND (gfGetMemVar('M_SYSTYPE') = 'P')
*-- if the communication module was installed and system type = Point of sale
IF llPOSSetup
  *-- get the ware house for the current Point of sale
  SELECT WAREHOUS
  LOCATE FOR cSiteId = gcCurSite
  lcPOSWrHs = WAREHOUS.cWareCode
  lnPOSWeHs = ASUBSCRIPT(laWareHouses,ASCAN(laWareHouses,lcPOSWrHs),1)
  lnWareHouse = lnPOSWeHs
ENDIF

=gfCloseFile('WAREHOUS')

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

lcKeyBmp  = gcBmpHome + "ExtKey.BMP"
lcNew     = gcBmpHome + "new1.bmp"
lcNotes   = gcBmpHome + "NOTES1.BMP"
lcPacks   = gcBmpHome + "packs.bmp"
lcRemove  = gcBmpHome + "rem1.bmp"
lcClose   = gcBmpHome + "Close2.bmp"

*N000391,1 Added for the Rolls screen. [Begin]
lcRoll   = gcBmpHome+ "Rolls.BMP"
lcSelecP = gcBmpHome + "SEL1.BMP"
lcUnSelP = gcBmpHome + "UNSEL.BMP"
*N000391,1 Added for the Rolls screen. [End]

DECLARE laPanelObj[2,3]

STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbObjLink'
laPanelObj[1,2] = gcBmpHome+"Relate.bmp"
laPanelObj[1,3] = [VALID lfvObjLink() MESSAGE 'Object Link' &lcpbObjStat ]
laPanelObj[2,1] = 'pbSend'
laPanelObj[2,2] = gcBmpHome+"send2.bmp"
laPanelObj[2,3] = [VALID lfvSend() MESSAGE 'Send EDI Invoice File' &lcPbSendStat]

*N000391,1 Disable New , Delete , Edit menu. [Begin]
DEFINE BAR 07 OF P03PU03 PROMPT 'Ne\<w'  SKIP FOR .T.
DEFINE BAR 09 OF P03PU03 PROMPT '\<Edit' SKIP FOR .T.
DEFINE BAR 10 OF P03PU03 PROMPT '\<Void' SKIP FOR !(laScrMode[2] .AND. MINVHDR.STATUS<>'V')
*N000391,1 Disable New , Delete , Edit menu. [End]

llOpnFiles=.T.

SELECT MINVHDR
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER

lcEdtFld='SHIPDATE,DUEDATE,DEPT,ibDepart,NOTE1,NOTE2,lnSpcinst,SPCINST'

DO (gcScrDir+gcWinAppl+"\ARMDIN.SPX")

=lfClearKey()

DEFINE BAR 10 OF P03PU03 PROMPT '\<Delete' SKIP FOR .T.
IF WEXIST(lcBrTtlZ)
  HIDE WINDOW (lcBrTtlZ)
  RELEASE WINDOW (lcBrTtlZ)
ENDIF
IF WEXIST(lcInvBrow)
  HIDE WINDOW (lcInvBrow)
  RELEASE WINDOW (lcInvBrow)
ENDIF
IF glQuitting
  IF USED(lcInvLine)
    USE IN (lcInvLine)
  ENDIF
  ERASE (gcWorkDir+lcInvLine+".DBF")
  ERASE (gcWorkDir+lcInvLine+".CDX")
  ERASE (gcWorkDir+lcInvLine+".FPT")
  IF USED(lcInvHdr)
    USE IN (lcInvHdr)
  ENDIF
  ERASE (gcWorkDir+lcInvHdr+".DBF")
  ERASE (gcWorkDir+lcInvHdr+".CDX")
  IF USED(lcGlDist)
    USE IN (lcGlDist)
  ENDIF
  ERASE (gcWorkDir+lcGlDist+".DBF")
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
  IF USED(lcUpsBox)
    USE IN (lcUpsBox)
  ENDIF
  ERASE (gcWorkDir+lcUpsBox+".DBF")
  ERASE (gcWorkDir+lcUpsBox+".CDX")

  IF USED(lcTmpAR)
    USE IN (lcTmpAR)
    ERASE (gcWorkDir+lcTmpAR+'.DBF')
    ERASE (gcWorkDir+lcTmpAR+'.CDX')
  ENDIF
  
  *N000391,1 Close the temp files which was used in gfMatCrl. [Begin]
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
  *N000391,1 Close the temp files which was used in gfMatCrl. [End]
ENDIF


SELECT MINVHDR
SET RELATION TO
SELECT MInvLine
SET RELATION TO

IF llUpsBox
  USE IN MUPSBOX
ENDIF
IF llRep_div
  USE IN REP_DIV
ENDIF
RELEASE PAD _INQUIRY OF _MSYSMENU
  
*!*************************************************************
*! Name      : lfActFolder
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Change folders
*!*************************************************************
*! Calls     : None.
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
    SHOW GETS WINDOW (lcWinCh30) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh34) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh35) DISABLE ONLY
    SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY
    lcStatus = IIF(laScrMode[4],'ENABLE','DISABLE')
    SHOW GETS WINDOW (lcWinCh2) &lcStatus ONLY
    
    lcLines = IIF(laScrMode[4] .AND. EOF(lcInvLine),'ENABLE','DISABLE')
    IF lnShipAddr = 1
      SHOW GET lcShipName DISABLE
      SHOW GET lcShipAdd1 DISABLE
      SHOW GET lcShipAdd2 DISABLE
      SHOW GET lcShipAdd3 DISABLE
      SHOW GET lcShipAdd4 DISABLE
      SHOW GET lcShipAdd5 DISABLE
    ENDIF
    SHOW GET lnSeason   &lcLines
    SHOW GET lnDivision &lcLines
    SHOW GET lnWareHouse &lcLines
    SHOW GET ibCurrency &lcLines
    SHOW GET laData[22] &lcLines
    *-- control the status of the currency in the screen due to edit currency or not
    IF !llEditExRt OR laData[22]=gcBaseCurr
      SHOW GET laData[23] DISABLE
    ELSE
      SHOW GET laData[23] &lcLines
    ENDIF
    lcSalesStat = IIF(laScrMode[4] AND laSetups[2,2]<>'Y','ENABLE','DISABLE')
    SHOW GET ibSaleRep1 &lcSalesStat
    SHOW GET laData[17] &lcSalesStat  && sales rep1
    SHOW GET laData[18] &lcSalesStat  && sales rep1 Percent
    SHOW GET ibSaleRep2 &lcSalesStat  
    SHOW GET laData[19] &lcSalesStat  && sales rep2
    SHOW GET laData[20] &lcSalesStat  && sales rep2 Percent
    IF  laScrMode[3]      
      SHOW GET laData[5] DISABLE
      SHOW GET ibStore DISABLE 
      =lfEdtFld(lcEdtFld) 
     
      SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY   
    ENDIF
    
  CASE lnActFolder = 2
    SHOW GETS WINDOW (lcWinCh2)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh30) ENABLE  ONLY
    SELECT IIF(laScrMode[2] OR laScrMode[3],'MInvLine',lcInvLine)
    =IIF(llReBrowse,lfBrowDet(),.T.)
    IF !llZoom
      lcStatus = IIF(laScrMode[4] .AND. !EMPTY(Fabric) AND !EMPTY(Color),'ENABLE','DISABLE')
      SHOW GETS WINDOW (lcWinCh32) &lcStatus ONLY
      SHOW GETS WINDOW (lcWinCh34) &lcStatus ONLY
      SHOW GETS WINDOW (lcWinCh35) &lcStatus ONLY
      IF laScrMode[4]
        SHOW GET pbNew   ENABLE 
        *SHOW GET pbPacks ENABLE
      ELSE
        SHOW GET pbNew   DISABLE
        *SHOW GET pbPacks DISABLE
      ENDIF
      IF !EMPTY(Fabric) AND !EMPTY(Color)
        SHOW GET pbLineNote ENABLE

        *N000391,1 Enable Rolls button. [Begin]
        IF laScrMode[2]
          IF mInvHdr.Status = "V"
            SHOW GET pbRolls DISABLE
          ELSE
            SHOW GET pbRolls ENABLE
          ENDIF  
        ELSE
          SHOW GET pbRolls ENABLE
        ENDIF  
        *N000391,1 Enable Rolls button. [End]

        SHOW GET pbLineNote ENABLE
      ELSE
        SHOW GET pbLineNote DISABLE

        *N000391,1 Disable Rolls button. [Begin]
        SHOW GET pbRolls DISABLE
        *N000391,1 Disable Rolls button. [End]

      ENDIF
      IF laScrMode[4] .AND. laSetups[2,2]='Y' AND !EMPTY(laData[17])
        SHOW GET m.Comm1 ENABLE
      ELSE
        SHOW GET m.Comm1 DISABLE
      ENDIF
      IF laScrMode[4] .AND. laSetups[2,2]='Y' AND !EMPTY(laData[19])
        SHOW GET m.Comm2 ENABLE
      ELSE
        SHOW GET m.Comm2 DISABLE
      ENDIF
      _CUROBJ = IIF(EMPTY(Fabric) AND EMPTY(Color),OBJNUM(pbNew),OBJNUM(ibInv200E))
    ENDIF
    
  CASE lnActFolder = 3
    =gfRltFld(laData[10],@laVRltFld,'SHIPVIA') 
    =IIF(laScrMode[4] AND !llIsEngland AND &lcInvHDr..lCompUps ,lfvUpsChrg(),.T.)
    =IIF(laScrMode[4] AND llIsEngland AND llReMerTax,lfMechTax(),.T.)
    lcUpsTrack = IIF(SUBSTR(lcUpsType,1,5)='USUPS','UPS COD Tracking#','COD Tracking#')
    lcLStatus  = IIF(laScrMode[4] .AND. laData[31]>0,'ENABLE','DISABLE')
    lcStatus   = IIF(laScrMode[4],'ENABLE','DISABLE')
    IF SUBSTR(lcUpsType,1,5)='USUPS'
      SHOW GET llUpsInsur,1 PROMPT 'UPS Insur.'
    ELSE
      SHOW GET llUpsInsur,1 PROMPT 'Insurance '
    ENDIF
    SHOW GETS WINDOW (lcWinChrg) &lcStatus ONLY
    SHOW GETS WINDOW (lcWinCh2)  DISABLE   ONLY
    SHOW GETS WINDOW (lcWinCh30) DISABLE   ONLY
    SHOW GETS WINDOW (lcWinCh32) DISABLE   ONLY
    SHOW GETS WINDOW (lcWinCh34) DISABLE   ONLY
    SHOW GETS WINDOW (lcWinCh35) DISABLE   ONLY
    SHOW GET laData[25] &lcLStatus
    SHOW GET laData[26] &lcLStatus
    SHOW GET laData[34] &lcLStatus
    SHOW GET laData[35] &lcLStatus
    SHOW GET laData[36] &lcLStatus
    SHOW GET laData[28] &lcLStatus
    SHOW GET laData[45] &lcLStatus
    SHOW GET laData[46] &lcLStatus
    SHOW GET laData[51] &lcLStatus
    SHOW GET laData[52] &lcLStatus
    IF (laScrMode[2] OR laScrMode[3]) .AND. llIsEngland
      SHOW GET pbCharges ENABLE
    ENDIF
    IF !laScrMode[1] .AND. !llIsEngland .AND. laData[25] > 1 AND SUBSTR(lcUpsType,1,5) = "USUPS"
      SHOW GET pbUpsBox ENABLE
    ELSE
      SHOW GET pbUpsBox DISABLE
    ENDIF
    IF laSetups[9,2] <> 'Y'
      SHOW GET lnPstRule  DISABLE
      SHOW GET laData[45] DISABLE
      SHOW GET laData[46] DISABLE
      SHOW GET laData[51] DISABLE
    ENDIF
    
    llOpnCrdt = .F.
    IF laScrMode[4] AND llCod AND laData[38] > 0 
      SHOW GET laData[27] ENABLE
      SHOW GET laData[28] ENABLE
      llOpnCrdt = llActCrdt
    ELSE
      STORE SPACE(6) TO laData[27]
      SHOW GET laData[27] DISABLE
      SHOW GET laData[28] DISABLE
    ENDIF
        
    IF llOpnCrdt
      SHOW GET pbCredits ENABLE
    ELSE
      SHOW GET pbCredits DISABLE
    ENDI
    IF !laScrMode[1] AND laData[38] > 0 AND llInstTerm
      SHOW GET pbInvInst ENABLE
    ELSE
      SHOW GET pbInvInst DISABLE
    ENDIF
    IF  laScrMode[3]      
      SHOW GET laData[5] DISABLE
      SHOW GET ibStore DISABLE 
      =lfEdtFld(lcEdtFld) 
       SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY    
    ENDIF
    =lfRefresh(lcWinChrg)
ENDCASE
RETURN

*!*************************************************************
*! Name      : lfBrowDet
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Invoice lines
*!*************************************************************
*! Calls     : lfBrowDet
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
SELECT IIF(laScrMode[2] OR laScrMode[3],'MInvLine',lcInvLine)
=SEEK(IIF(laScrMode[2] OR laScrMode[3] ,laData[1],laData[2]+laData[3]+laData[5]))
lnMarker = RECNO()
lcFields = "cMarker=IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,Fabric :10 :H='Fabric' :R,Color :8 :H='Color' :R,"
lcFields = lcFields + IIF(laSetups[8,2]='Y',"Dyelot :R,",'')
*E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
*lcFields = lcFields +;
           "nSellPrice :H='Gross Price' :R,"+;
           "Disc_Pcnt :H='Disc. Pcnt.' :R,nSellNetPr :H='Net Price' :R,FabQty :H='Quantity' :R,"+;
           "lnAmount=nSellNetPr*FabQty :12 :P= '99999999999.99' :H='Ship Amount':R"
lcFields = lcFields +;
           "nSellPrice :H='Gross Price' :R,"+;
           "Disc_Pcnt :H='Disc. Pcnt.' :R,nSellNetPr :H='Net Price' :R,FabQty :H='Quantity' :R,"+;
           "lnAmount=CEILING(nSellNetPr*FabQty*100)/100 :12 :P= '99999999999.99' :H='Ship Amount':R"
*E126684,1  TMI [End  ] 
IF WEXIST(lcBrTtlZ)
  HIDE WINDOW (lcBrTtlZ)
  RELEASE WINDOW (lcBrTtlZ)
ENDIF
IF WEXIST(lcInvBrow)
  HIDE WINDOW (lcInvBrow)
  RELEASE WINDOW (lcInvBrow)
ENDIF
IF llZoom
  HIDE WINDOW (lcWinCh32),(lcWinCh34),(lcWinCh35) SAME
  SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh34) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh35) DISABLE ONLY
  BROWSE FIELDS &lcFields ;
         WINDOW (lcWinCh33)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
	     NOCLEAR           ;
	     KEY IIF(laScrMode[2] OR laScrMode[3],laData[1],'')  ;
	     WHEN lfwBrow()    ;
         TITLE lcBrTtlZ 
ELSE
  SHOW WINDOW (lcWinCh32),(lcWinCh34),(lcWinCh35) TOP
  BROWSE FIELDS &lcFields ;
         WINDOW (lcWinCh31)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
	     NOCLEAR           ;
  	     KEY IIF(laScrMode[2] OR laScrMode[3],laData[1],'')  ;
  	     WHEN lfwBrow()    ;
         TITLE lcInvBrow   
ENDIF
=lfwBrow()
llReBrowse = .F.
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

lnMarker = IIF(laScrMode[2],RECNO('MInvLine'),RECNO(lcInvLine))
IF llZoom
  SHOW WINDOW (lcBrTtlZ) REFRESH SAME
ELSE  
  SHOW WINDOW (lcInvBrow) REFRESH SAME  
  SCATTER MEMVAR MEMO
  SHOW GETS WINDOW (lcWinCh32) ONLY
  SHOW GETS WINDOW (lcWinCh34) ONLY
  SHOW GETS WINDOW (lcWinCh35) ONLY
  =lfRefresh(lcWinCh32)
  =lfRefresh(lcWinCh34)
  IF laScrMode[4] .AND. laSetups[2,2]='Y' AND !EMPTY(laData[17])
    SHOW GET m.Comm1 ENABLE
  ELSE
    SHOW GET m.Comm1 DISABLE
  ENDIF
  IF laScrMode[4] .AND. laSetups[2,2]='Y' AND !EMPTY(laData[19])
    SHOW GET m.Comm2 ENABLE
  ELSE
    SHOW GET m.Comm2 DISABLE
  ENDIF
  
  IF laScrMode[4]
    =lfPpStat()
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfDARDINV
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : READ Deactivate function of screen ARDINV
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfDARDINV()
*!*************************************************************
FUNCTION lfDARDINV

IF laScrMode[4] AND WLAST()=(lcWinCh34) AND !lfChkTotQty()
  ACTIVATE WINDOW (lcWinCh34)
  _CUROBJ = OBJNUM(m.Qty1)
  RETURN .F.
ENDIF

SET SKIP OF PAD _INQUIRY OF _MSYSMENU laScrMode[1]
IF WONTOP()=lcInvBrow .OR. WONTOP()=lcBrTtlZ
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  ON KEY LABEL TAB DO lpTab WITH ;
  IIF(WONTOP()=lcInvBrow,lcWinCh32,'gwcContrl1'),;
  IIF(WONTOP()=lcInvBrow,'m.Fabric','pbTop')
  ON KEY LABEL BACKTAB DO lpBackTab WITH lcFolder,'ibFolder[2]'
ELSE
  glFromBrow = .F.
ENDIF

IF laScrMode[4]  .AND. llNewLine .AND. ;
  !(WONTOP()=(lcWinCh32) OR WONTOP()=(lcWinCh34) OR WONTOP()=(lcWinCh35))
  SELECT (lcInvLine)
  SCATTER MEMVAR MEMO
  STORE .F. TO llAddLine,llNewLine
  STORE lcEmptySty TO lcLastStyle
  
  IF EMPTY(m.Fabric)
    SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh34) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh35) DISABLE ONLY
    SHOW GET pbNew     ENABLE
    SHOW GET ibInv200E ENABLE
    SHOW GET pbPacks ENABLE
  ELSE
    SHOW GETS WINDOW (lcWinCh32) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh34) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh35) ENABLE ONLY
  ENDIF
ENDIF
RETURN .F.

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
*! Name      : lfvInvNote
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : iNVOICE notepad
*!*************************************************************
*! Calls     : NOTEPAD
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvInvNote()
*!*************************************************************
FUNCTION lfvInvNote
=NOTEPAD('C',laData[1])

*!*************************************************************
*! Name      : lfvObjLink
*! Developer : WAM
*! Date      : 07/01/1996
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
DO GetObj WITH 'I',laData[1]
SELECT (lnAlais)

*!*************************************************************
*! Name      : lfCanReason
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Read order cancellation reason
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
PRIVATE laCanReason,lnCanReason

DECLARE laCanReason[1,2]
STORE '' TO laCanReason
STORE 1  TO lnCanReason
=gfwCodePop(@laCodes,'CCANCRESON','L')
DO (gcScrDir+"ARORDER.SPX") WITH 'X','lnCanReason'
RETURN(ALLTRIM(laCanReason[lnCanReason,2]))

*!*************************************************************
*! Name      : lfGtOrder
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Read order number manually
*!*************************************************************
*! Calls     : ARORDER.SPX
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  Order#
*!*************************************************************
*! Example            :  =lfGtOrder()
*!*************************************************************
FUNCTION lfGtOrder
PRIVATE lcOrderNo
STORE SPACE(6) TO lcOrderNo
DO (gcScrDir+"ARORDER.SPX") WITH 'O','lcOrderNo'
RETURN(lcOrderNo)

*!*************************************************************
*! Name      : lpDelScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Void Invoice
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvVoid()
*!*************************************************************
FUNCTION lpDelScr

PRIVATE lnAlias , llReturn , lcOldOrder , lnOldRecR , lnOldInvLn

*B606163,1 ABD - Remark the Next following Lines. [Begin]
*lnAlias = SELECT (0)
*llReturn = .T.
*lnOldInvLn = RECNO(lcInvLine)
*IF USED(lcTmpRoll)
*  lnOldRecR = RECNO(lcTmpRoll)
*  lcOldOrder = ORDER (lcTmpRoll)
*  SET ORDER TO lcTmpRoll2 IN (lcTmpRoll)
*ELSE
*  =gfModalGen('TRM40177B00000','ALERT','the Rolls quantities for all the lines')
*  SELECT(lnAlias)
*  RETURN .F.  
*ENDIF

*SELECT IIF(laScrMode[2],MInvLine,(lcInvLine))
*SCAN
*  =SEEK(Fabric+Color,'Fabric')
*  lcSeekExp = Fabric + Color + cWareCode + PADR(Dyelot,10) + STR(LineNo,6)
*  lnApplyQty = 0 
*  *- Check if the user Enter the Qty
*  IF Fabric.lTrkRolls
*    SELECT (lcTmpRoll)
*    = SEEK (lcSeekExp)
*    SCAN REST WHILE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = lcSeekExp FOR nApply # 0
*      lnApplyQty =   lnApplyQty + nApply
*    ENDSCAN 
*  ENDIF      
*  IF lnApplyQty # &lcInvLine..FabQty
*    llReturn = .F.
*    =gfModalGen('TRM40177B00000','ALERT','the Rolls quantities for Fabric : ' + ALLTRIM(&lcInvLine..Fabric) + ', Color : ' *+ ALLTRIM(&lcInvLine..Color) + ' in Order # ' + &lcInvLine..cMOrder + IIF(EMPTY(&lcInvLine..Store),'',', Store : ' + *&lcInvLine..Store))
*    EXIT
*  ENDIF
*ENDSCAN

*IF USED(lcTmpRoll)
*  SET ORDER TO &lcOldOrder IN (lcTmpRoll)
*  IF BETWEEN(lnOldRecR ,1,RECCOUNT(lcTmpRoll)) 
*    GOTO lnOldRecR IN (lcTmpRoll)
*  ENDIF  
*ENDIF
 
*IF USED(lcInvLine)
*  IF BETWEEN(lnOldInvLn,1,RECCOUNT(lcInvLine)) 
*    GOTO lnOldInvLn IN (lcInvLine)
*  ENDIF
*ENDIF

*SELECT(lnAlias)
*IF !llReturn
*  RETURN
*ENDIF

*-- this Variable don't Show the rolls screen.
llExtCall = .T.
*-- This Variable To generate new rolls sequence Number.
lcKeyType  = 'P'
lcMatInvJl = gfTempName()
lcMatRolls = gfTempName()
*B606163,1 ABD - [End]

PRIVATE lcInvYear,lcInvPeriod,lcInvLines

*N000391,1 ABD Define variable used in Lot costing method. [Begin]
PRIVATE lnUseQty , laOtherPar , lnRecCount , lnCurrRec
= gfOpenFile(gcDataDir+'Rolls',gcDataDir+'RolApl','SH')
lnRecCount = RECCOUNT(lcInvLine)
lnCurrRec  = 0
lnLineNo = 0

DIMENSION laOtherPar[4,2]
laOtherPar[1,1] = 'lnLineNo'
laOtherPar[1,2] = &lcInvLine..LINENO
laOtherPar[2,1] = ''
laOtherPar[2,2] = ''
laOtherPar[3,1] = 'llLastRec'
laOtherPar[3,2] = (lnRecCount = lnCurrRec)
laOtherPar[4,1] = ''
laOtherPar[4,2] = ""
*N000391,1 ABD Define variable used in Lot costing method. [End]

*N000391,1 No Material history file and customer history file. [Begin]
*=gfOpenFile(gcDataDir+'icStyHst', gcDataDir+'Styhst','SH')
*=gfOpenFile(gcDataDir+'arCusHst', gcDataDir+'Acthst','SH')
*N000391,1 No Material history file and customer history file. [End]

=gfOpenFile(gcDataDir+'DEBIT', gcDataDir+'DEBIT','SH')
=gfOpenFile(gcDataDir+'REPCOMM', gcDataDir+'REPCOMM','SH')
IF MInvHdr.CONSOL = 'Y' 
  =gfOpenFile(gcDataDir+'MConsInH', gcDataDir+'MConsInH','SH')
  =gfOpenFile(gcDataDir+'MConsInL', gcDataDir+'MConsInL','SH')
ENDIF
=gfOpenFile(gcDataDir+'ARHIST', gcDataDir+'ARHISTT','SH')

IF EMPTY(laData[24]) OR laSetups[17,2]='Y'
  *-- if there is no factor or post factored invoice to customer = Y
  =gfOpenFile(gcDataDir+'CREDIT', gcDataDir+'CREDIT','SH')
ENDIF

*B606702,1 Make program checks for payment anyway. [Begin]
*IF laSetups[17,2]='Y' AND EMPTY(laData[24]) 
*B606702,1 Make program checks for payment anyway. [End]

  SELECT ARHIST
  =SEEK(laData[2]+laData[1])
  *--  Check if the invoice was paid or not   
  LOCATE REST WHILE Account+Tran+cInstalNo = laData[2]+laData[1] FOR TranType='M'
  llPaid = FOUND()
  
  SELECT MINVHDR
  IF llPaid
    *-- Message : 40130
    *-- This invoice has already been paid. Cannot void.
    *-- Button : 00000 
    *-- Ok
    =gfModalGen('TRM40130B00000','ALERT','')
    RETURN
  ENDIF  

*B606702,1 Make program checks for payment anyway. [Begin]
*ENDIF
*B606702,1 Make program checks for payment anyway. [End]

*N000391,1 Uptil now no Material Return is done. [Begin]
*IF 'RM' $ gcCmpModules
**-- if the return mechendise module installed 
*  llRetLine = gfOpenFile(gcDataDir+'RETLINE',gcDataDir+'RETLINEI','SH')
*  llRetHdr  = gfOpenFile(gcDataDir+'RETHDR',gcDataDir+'RETHDR','SH')
*  *-- seek for the invoice  in the return merchandise files 
*  llRetInv  = SEEK(laData[2]+laData[1],'RETLINE') AND ;
*              SEEK(RETLINE.CrMemo,'RETHDR') AND RETHDR.Status <> 'V'
*  IF llRetHdr
*    USE IN RETHDR
*  ENDIF
*  IF llRetLine
*    USE IN RETLINE
*  ENDIF
*  SELECT MINVHDR
*  IF llRetInv
*    *-- Message : 40134
*    *-- Return merchandise invoice. Cannot void
*    *-- Button : 00000 
*    *-- Ok
*    =gfModalGen('TRM40134B00000','ALERT','')
*    RETURN
*  ENDIF
*ENDIF
*N000391,1 Uptil now no Material Return is done. [End]

PRIVATE lcVDateWin,ldVDate,ldOldDate,llVoid
lcVDateWin = gfTempName()
STORE gdSysDate TO ldVDate,ldOldDate
llVoid = .F.
*-- get the void date from the user
DO (gcScrDir+gcWinAppl+"\ARVDATE.SPR")
IF !llVoid
  RETURN
ENDIF  

*-- check if the void data is valid or not
IF !CHECKPRD(ldVDate,'lcSysYear','lcSysPeriod','VI1')
  RETURN
ENDIF

STORE '' TO lcInvYear,lcInvPeriod
IF !CHECKPRD(MInvHdr.dPostDate,'lcInvYear','lcInvPeriod','VI2')
  RETURN
ENDIF
IF !SEEK(MInvHdr.cMatInv,'MInvLine')
  *-- Message : 40002
  *-- invoice lines are not found
  *-- Button : 00000 
  *-- Ok
  =gfModalGen('INM40002B00000','ALERT','')
  RETURN
ENDIF
IF MInvHdr.CONSOL = 'Y' .AND. (!SEEK(MInvHdr.cMatInv,'MConsInH') OR ;
   !SEEK(MInvHdr.cMatInv,'MConsInL') )
  *-- Message : 40002
  *-- invoice lines are not found
  *-- Button : 00000 
  *-- Ok
  =gfModalGen('INM40002B00000','ALERT','Consolidated')
  RETURN
ENDIF
IF !llContinue
  SELECT (lcInvLine)
  DELETE ALL
  SELECT (lcInvHdr)
  DELETE ALL
  llRebuild = .T.
  *-- Message : 40003
  *-- This is a direct invoice. Would you like to rebuild the order
  *-- Button : 00001 
  *-- Yes No
  IF MInvHdr.Direct_Inv .AND. gfModalGen('INM40003B40000','ALERT') = 2
    llRebuild = .F.
  ENDIF
  SELECT 'UNCMSESS'
  IF SEEK('I')
    BLANK
  ELSE
    APPEND BLANK
  ENDIF
  REPLACE Status     WITH 'O'        ,;
          cUTranType WITH lcProgID   ,;
          cUserId    WITH gcUser_id  ,;
          cSession   WITH lcSession  ,;
          cCurrObj   WITH 'PBDLT'    ,;
          cProgram   WITH 'ARMDIN'   ,;
          cCurrScr   WITH 'ARMDIN'   ,;
          dTranDate  WITH ldVDate    ,;
          cTranTime  WITH TIME()
  =RLOCK()
  lcCurrInv = laData[1]

  =lfCreatHst()
  lcFiles = 'lcInvHdr,'+lcInvHdr+',;lcInvLine,'+lcInvLine+',MInvLine;'
  lcFiles = lcFiles + "lcTmpAR,"  + lcTmpAR+","  + ORDER(lcTmpAR)+";"
  =gfSavSess(lcProgID, lcFiles, @laVars,lcSession)
ENDIF
DECLARE laGlArray[2,13]
STORE '' TO laGlArray
IF laSetups[4,2]='Y'
  =gfOpenFile(gcDataDir+'GLDIST', gcDataDir+'GLDISTNO', 'SH')

  SELECT GLDIST
  COPY STRUCTURE TO (gcWorkDir+lcGlDist)
  =gfOpenFile(gcWorkDir+lcGlDist, '', 'EX')  
  STORE 'VM'        TO laGlArray[1,4],laGlArray[2,4]
  STORE laData[1]   TO laGlArray[1,5],laGlArray[2,5]   && Invoice no
  STORE ldVDate     TO laGlArray[1,6],laGlArray[2,6]   && Void Date
  STORE lcSysYear   TO laGlArray[1,7],laGlArray[2,7]   && Year
  STORE lcSysPeriod TO laGlArray[1,8],laGlArray[2,8]   && Period
  STORE lcGlDist    TO laGlArray[1,9],laGlArray[2,9]

  *B606163,1 ABD - Remark the Next following Lines. [Begin]
  *STORE ''          TO laGlArray[1,11],laGlArray[2,11]
  *STORE ''          TO laGlArray[1,12],laGlArray[2,12]
  *STORE ''          TO laGlArray[1,13],laGlArray[2,13]
  lcInvHFile = IIF(MInvHdr.CONSOL='Y','MConsInH','MINVHDR')
  STORE ''                    TO laGlArray[1,10],laGlArray[2,10] && 
  STORE &lcInvHFile..cCurrCode TO laGlArray[1,11],laGlArray[2,11] && Currency 
  STORE &lcInvHFile..nCurrUnit TO laGlArray[1,12],laGlArray[2,12] && Currency unit
  STORE &lcInvHFile..nExRate   TO laGlArray[1,13],laGlArray[2,13] && Exchange rate 
  *B606163,1 ABD - [End]
  
ENDIF
SET ORDER TO TAG DRTRAN IN DEBIT
lcUntSin = ''
lcExRSin = gfGetExSin(@lcUntSin, MINVHDR.cCurrCode)

lcInvHFile = IIF(MInvHdr.CONSOL='Y','MConsInH','MINVHDR')
SELECT (lcInvHFile)
SCAN REST WHILE cMatInv = laData[1]
  IF !SEEK(Account+CMORDER+Store+PikTKt,lcInvHdr)
    INSERT INTO (lcInvHdr) (Account,CMORDER,Store,PikTKt) VALUES ;
    (&lcInvHFile..Account,&lcInvHFile..CMORDER,&lcInvHFile..Store,&lcInvHFile..PikTkt)
  ENDIF
  IF SEEK('O'+CMORDER,'MaSoHdr')
    WAIT 'Rebuilding order# '+CMORDER+' ...' WINDOW NOWAIT
    lnDifValue = 0
    IF &lcInvHdr..nSteps < 1
      *-- the invoice line file
      lcInvLines = IIF(MInvHdr.CONSOL='Y','MConsInL','MInvLine')
      SELECT (lcInvLines)
      =SEEK(MInvHdr.cMatInv+IIF(MInvHdr.CONSOL='Y',&lcInvHFile..Store,''))
      SCAN REST WHILE cMatInv+IIF(MInvHdr.CONSOL='Y',Store,'') = ;
           &lcInvHFile..cMatInv+IIF(MInvHdr.CONSOL='Y',&lcInvHFile..Store,'') ;
           FOR CMORDER = &lcInvHFile..CMORDER
        IF !SEEK(Account+CMORDER+Store+SPACE(6)+STR(LineNo,6),lcInvLine)
          =SEEK('O'+CMORDER+STR(LineNo,6),'MaSoLin')
          SELECT MaSoLin
          =RLOCK()
          *-- update the order line quantity
          REPLACE FabQty WITH FabQty + &lcInvLines..FabQty
          UNLOCK
          *E126684,1  TMI [Start] round amount up
          *lnDifValue = lnDifValue + ( (&lcInvLines..FabQty * &lcInvLines..nSellNetPr) - ;
                                      (&lcInvLines..FabQty * nSellNetPr) )
          lnDifValue = lnDifValue + ( CEILING(&lcInvLines..FabQty * &lcInvLines..nSellNetPr * 100)/100 - ;
                                      CEILING(&lcInvLines..FabQty * nSellNetPr * 100)/100 )
          *E126684,1  TMI [End  ] 
          =gfTraceKey('MaSoLin','O'+MaSoLin.CMORDER+STR(MaSoLin.LineNo,6),'M')
          INSERT INTO (lcInvLine) (Account,CMORDER,Store,LineNo) VALUES ;
          (&lcInvLines..Account,&lcInvLines..CMORDER,&lcInvLines..Store,&lcInvLines..LineNo)
        ENDIF
      ENDSCAN
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF  
    IF &lcInvHdr..nSteps < 2
      SELECT MaSoHdr
      =RLOCK()
      IF llRebuild
        *-- update the order header
        *-- open amount =open amount + shipped amount- difference between invine price 
        *-- and order line price
        REPLACE STATUS     WITH 'O'  ,;
                FabSHIP    WITH FabSHIP - &lcInvHFile..FabSHIP,;
                FabSHIPAMT WITH FabSHIPAMT - &lcInvHFile..FabSHIPAMT ,;
                FabOpen    WITH FabOpen + &lcInvHFile..FabSHIP,;
                FabOpenAmt WITH FabOpenAmt + &lcInvHFile..FabSHIPAMT - lnDifValue,;
                APPRAMT    WITH IIF((MInvHdr.Consol='Y' OR (&lcInvHFile..Store <> Store)),APPRAMT+ROUND(&lcInvHFile..FabSHIPAMT,0),;
                                                      ROUND(&lcInvHFile..FabSHIPAMT,0))
      ELSE
        REPLACE FabCancel  WITH FabSHIP    ,;
                FabCanAmt  WITH FabSHIPAMT ,;
                FabSHIPAMT WITH 0          ,;
                FabSHIP    WITH 0          ,;
                CANCELLED  WITH ldVDate    ,;
                CCANCRESON WITH lfCanReason() ,;
                STATUS     WITH 'X'
      ENDIF
      UNLOCK
      =gfTraceKey('MaSoHdr','O'+MaSoHdr.CMORDER,'M')
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 2
      UNLOCK
    ENDIF
  ENDIF

  IF (!EMPTY(&lcInvHFile..Rep1) .AND. &lcInvHFile..CommAmt1 > 0) .AND. ;
    SEEK(&lcInvHFile..Rep1,'SALESREP')
    WAIT 'Charging back salesrep commissions ...' WINDOW NOWAIT
    *-- Calculate the equivlant amount
    lnEqvAmnt = -1* &lcInvHFile..CommAmt1 &lcExRSin MInvHdr.nExRate ;
                    &lcUntSin MInvHdr.nCurrUnit
    IF &lcInvHdr..nSteps < 3
      SELECT REPCOMM
      APPEND BLANK
      REPLACE STATUS     WITH 'O',;
              REPCODE    WITH &lcInvHFile..REP1   ,;
              ORDER      WITH &lcInvHFile..CMORDER  ,;
              TRAN       WITH &lcInvHFile..cMatInv,;
              DATE       WITH ldVdate,;
              TRANTYPE   WITH '6',;
              DESC       WITH 'CHARGE BACK - VOID',;
              CUSTPO     WITH &lcInvHFile..CustPo  ,;
              ACCOUNT    WITH &lcInvHFile..Account ,;
              STORE      WITH &lcInvHFile..Store   ,;
              AMOUNT     WITH lnEqvAmnt      ,;
              BALANCE    WITH SALESREP.BALANCE + lnEqvAmnt,;
              nForAmnt   WITH -1 * &lcInvHFile..COMMAMT1  ,;
              cCurrCode  WITH MInvHdr.cCurrCode,;
              nCurrUnit  WITH MInvHdr.nCurrUnit,;
              nExRate    WITH MInvHdr.nExRate  ,;
              dAdd_Date  WITH gdSysDate,;
              cAdd_Time  WITH TIME(),;
              cAdd_User  WITH gcUser_id
      =gfTraceKey('REPCOMM',REPCOMM.REPCODE+DTOS(REPCOMM.DATE)+REPCOMM.TRAN+REPCOMM.TRANTYPE,'A')
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 3
      UNLOCK
    ENDIF
    IF &lcInvHdr..nSteps < 4
      SELECT SALESREP
      =RLOCK()
      REPLACE CURRENT  WITH CURRENT + lnEqvAmnt,;
              BALANCE  WITH CURRENT + AGE30+AGE60+AGE90+AGE120
      UNLOCK
      =gfTraceKey('SALESREP',SALESREP.REPCODE,'M')
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 4
      UNLOCK
    ENDIF
  ENDIF
  IF (!EMPTY(&lcInvHFile..Rep2) .AND. &lcInvHFile..CommAmt2 > 0) .AND. ;
     SEEK(&lcInvHFile..Rep2,'SALESREP')
    WAIT 'Charging back salesrep commissions ...' WINDOW NOWAIT
    lnEqvAmnt = -1 * &lcInvHFile..CommAmt2 &lcExRSin MInvHdr.nExRate ;
                   &lcUntSin MInvHdr.nCurrUnit
    IF &lcInvHdr..nSteps < 5
      SELECT REPCOMM
      APPEND BLANK
      =RLOCK()
      REPLACE STATUS     WITH 'O'           ,;
              REPCODE    WITH &lcInvHFile..REP2   ,;
              ORDER      WITH &lcInvHFile..CMORDER  ,;
              TRAN       WITH &lcInvHFile..cMatInv,;
              DATE       WITH ldVDate     ,;
              TRANTYPE   WITH '6'           ,;
              DESC       WITH 'CHARGE BACK - VOID',;
              CUSTPO     WITH &lcInvHFile..CustPo ,;
              ACCOUNT    WITH &lcInvHFile..Account,;
              STORE      WITH &lcInvHFile..Store  ,;
              AMOUNT     WITH lnEqvAmnt     ,;
              BALANCE    WITH SALESREP.BALANCE + lnEqvAmnt,;
              nForAmnt   WITH -1 *  &lcInvHFile..COMMAMT2 ,;
              cCurrCode  WITH MInvHdr.cCurrCode,;
              nCurrUnit  WITH MInvHdr.nCurrUnit,;
              nExRate    WITH MInvHdr.nExRate  ,;
              dAdd_Date  WITH gdSysDate  ,;
              cAdd_Time  WITH TIME()  ,;
              cAdd_User  WITH gcUser_id
      UNLOCK
      =gfTraceKey('REPCOMM',REPCOMM.REPCODE+DTOS(REPCOMM.DATE)+REPCOMM.TRAN+REPCOMM.TRANTYPE,'A')
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 5
      UNLOCK
    ENDIF
    IF &lcInvHdr..nSteps < 6
      SELECT SALESREP
      =RLOCK()
      REPLACE CURRENT  WITH CURRENT + lnEqvAmnt,;
              BALANCE  WITH CURRENT + AGE30+AGE60+AGE90+AGE120
      UNLOCK
      =gfTraceKey('SALESREP',SALESREP.REPCODE,'M')
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 6
      UNLOCK
    ENDIF
  ENDIF
ENDSCAN
=SEEK(laData[1],'MInvHdr')
=SEEK(laData[1],'MInvLine')
WAIT 'Updating Fabric Cut & Sold ...' WINDOW NOWAIT
IF !SEEK(MInvHdr.Account+MInvHdr.CMORDER+MInvHdr.Store+MInvHdr.PikTKt,lcInvHdr)
  INSERT INTO (lcInvHdr) (Account,CMORDER,Store,PikTKt) VALUES ;
  (MInvHdr.Account,MInvHdr.CMORDER,MInvHdr.Store,MInvHdr.PikTkt)
ENDIF
lnCOGSAmt = 0
SELECT MInvLine
=SEEK(MInvHdr.cMatInv)
SCAN REST WHILE cMatInv = MInvHdr.cMatInv
  IF !SEEK(Fabric+Color,'Fabric')
    LOOP
  ENDIF
  IF !SEEK(Account+CMORDER+Store+SPACE(6)+STR(LineNo,6),lcInvLine)
    INSERT INTO (lcInvLine) (Account,CMORDER,Store,LineNo) VALUES ;
    (MInvLine.Account,MInvLine.CMORDER,MInvLine.Store,MInvLine.LineNo)
  ENDIF
 
  =SEEK('O'+MInvLine.CMORDER+STR(LineNo,6),'MaSoLin')
  
  IF laSetups[8,2]='Y' .AND. Fabric.cDye_Flg = 'Y' .AND. ;   
    SEEK(MInvLine.Fabric+MInvLine.Color+IIF(EMPTY(MInvLine.cWareCode),MaSoLin.cWareCode,MInvLine.cWareCode)+MInvLine.Dyelot,'FabDye')
    IF llRebuild .AND. &lcMInvLine..nSteps < 1
      SELECT FabDye 
      =RLOCK()
      REPLACE nSellOrder WITH nSellOrder + (mInvLine.FabQty * IIF(m.nSellconv=0,1,m.nSellconv))
      UNLOC
      SELECT (lcInvLine)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF
    IF &lcInvLine..nSteps < 2
      SELECT FabDye
      =RLOCK()
      REPLACE nSellShip WITH nSellShip - (mInvLine.FabQty * IIF(m.nSellconv=0,1,m.nSellconv))
      UNLOCK
      SELECT (lcInvLine)
      =RLOCK()
      REPLACE nSteps WITH 2
      UNLOCK
    ENDIF
    =gfTraceKey('FabDye',FabDye.Fabric+FabDye.Color+FabDye.CWARECODE+FabDye.DYELOT,'M')
  ENDIF
  IF SEEK(MInvLine.Fabric+MInvLine.Color+IIF(EMPTY(MInvLine.cWareCode),MaSoLin.cWareCode,MInvLine.cWareCode)+SPACE(10),'FabDye')
    IF llRebuild .AND. &lcInvLine..nSteps < 3
      SELECT FabDye
      =RLOCK()
      REPLACE nSellOrder WITH nSellOrder + (mInvLine.FabQty * IIF(m.nSellconv=0,1,m.nSellconv))
      UNLOCK
      SELECT (lcInvLine)
      =RLOCK()
      REPLACE nSteps WITH 3
      UNLOCK
    ENDIF
    IF &lcInvLine..nSteps < 4
      SELECT FabDye
      =RLOCK()
      REPLACE nSellShip WITH nSellShip - (MInvLine.FabQty * IIF(m.nSellconv=0,1,m.nSellconv))
      UNLOCK
      SELECT (lcInvLine)
      =RLOCK()
      REPLACE nSteps WITH 4
      UNLOCK
    ENDIF
    =gfTraceKey('FabDye',FabDye.Fabric+FabDye.Color+FabDye.CWARECODE+FabDye.DYELOT,'M')
  ENDIF

  SELECT Fabric
  =SEEK(MInvLine.Fabric+MInvLine.Color)
  =RLOCK()
  IF llRebuild .AND. &lcInvLine..nSteps < 5
    REPLACE nSellOrder WITH nSellOrder + (mInvLine.FabQty * IIF(m.nSellconv=0,1,m.nSellconv))
    SELECT (lcInvLine)
    =RLOCK()
    REPLACE nSteps WITH 5
    UNLOCK
    SELECT Fabric
  ENDIF
  IF &lcInvLine..nSteps < 6
      REPLACE nSellShip WITH nSellShip - (mInvLine.FabQty * IIF(m.nSellconv=0,1,m.nSellconv))
    SELECT (lcInvLine)
    =RLOCK()
    REPLACE nSteps WITH 6
    UNLOCK
  ENDIF
  SELECT Fabric
  UNLOCK
  =gfTraceKey('Fabric',Fabric.Fabric+Fabric.Color,'M')

  lnCOGSAmt = lnCOGSAmt + MInvLine.FabQty*MInvLine.CostUse
  IF laSetups[4,2]='Y'
    *E126684,1  TMI [Start] round amount up
    *DO GLDIST WITH MInvLine.GL_Sales,'003',(MInvLine.FabQty *MInvLine.nSellNetPr),'VM',MInvLine.cMatInv,;
                   ldVDate,lcSysYear,lcSysPeriod,lcGlDist,MInvLine.cSalesAcnt,;
                   MInvHdr.cCurrCode,MInvHdr.nCurrUnit,MInvHdr.nExRate
    DO GLDIST WITH MInvLine.GL_Sales,'003',CEILING(MInvLine.FabQty * MInvLine.nSellNetPr * 100)/100,'VM',MInvLine.cMatInv,;
                   ldVDate,lcSysYear,lcSysPeriod,lcGlDist,MInvLine.cSalesAcnt,;
                   MInvHdr.cCurrCode,MInvHdr.nCurrUnit,MInvHdr.nExRate
    *E126684,1  TMI [End  ]     
    
    IF MInvHdr.FabShipAmt <> 0
      *E126684,1  TMI [Start] round amount up to get discount
      *DO GLDIST WITH MInvLine.GL_Sales,'005',-(MInvLine.FabQty*MInvLine.nSellNetPr*ABS(MInvHDr.Discount)/MInvHDr.FabShipAmt),;
                     'VM',MInvLine.cMatInv,ldVDate,lcSysYear,lcSysPeriod,;
                     lcGlDist,MInvLine.cDiscAcnt,MInvHDr.cCurrCode,MInvHDr.nCurrUnit,;
                     MInvHDr.nExRate
      DO GLDIST WITH MInvLine.GL_Sales,'005',-((CEILING(MInvLine.FabQty*MInvLine.nSellNetPr*100)/100 )*ABS(MInvHDr.Discount)/MInvHDr.FabShipAmt),;
                     'VM',MInvLine.cMatInv,ldVDate,lcSysYear,lcSysPeriod,;
                     lcGlDist,MInvLine.cDiscAcnt,MInvHDr.cCurrCode,MInvHDr.nCurrUnit,;
                     MInvHDr.nExRate
      *E126684,1  TMI [End  ]                      
    ENDIF 
  ENDIF
  SELECT MInvLine
  =SEEK('O'+MInvLine.CMORDER+STR(LINENO,6),'MaSoLin')

  *N000391,1 We don't have Material history file. [Begin]
  *SELECT icStyHst
  *IF SEEK(MInvLine.Style+lcInvYear) .AND. &lcInvLine..nSteps < 7
  *  lnShipAmt = ROUND(MInvLine.FabQty*MInvLine.nSellNetPr &lcExRSin MInvHdr.nExRate ;
  *                  &lcUntSin MInvHdr.nCurrUnit, 2)
  *  lnDiscount = ROUND(MInvLine.FabQty*MInvLine.nSellNetPr*laData[32]/100 &lcExRSin MInvHdr.nExRate ;
  *                  &lcUntSin MInvHdr.nCurrUnit, 2)
  *  =RLOCK()
  *  REPLACE nSlsQty&lcInvPeriod  WITH nSlsQty&lcInvPeriod  - MInvLine.FabQty ,;
  *          nSlsQty              WITH nSlsQty              - MInvLine.FabQty ,;
  *          nSlsAmt&lcInvPeriod  WITH nSlsAmt&lcInvPeriod  - lnShipAmt  ,;
  *          nSlsAmt              WITH nSlsAmt              - lnShipAmt  ,;
  *          nDisAmt&lcInvPeriod  WITH nDisAmt&lcInvPeriod  - lnDiscount ,;
  *          nDisAmt              WITH nDisAmt              - lnDiscount ,;
  *          nCOGSAmt&lcInvPeriod WITH nCOGSAmt&lcInvPeriod - MInvLine.FabQty*MInvLine.CostUse  ,;
  *          nCOGSAmt             WITH nCOGSAmt             - MInvLine.FabQty*MInvLine.CostUse
  *  UNLOCK
  *  =gfTraceKey('icStyHst',MInvLine.Style+lcInvYear,'M')
  *  SELECT (lcInvLine)
  *  =RLOCK()
  *  REPLACE nSteps WITH 7
  *  UNLOCK
  *ENDIF
  *N000391,1 We don't have Material history file. [End]

  SELECT MInvLine
  SCATTER FIELDS FabQty TO lnUseQty
  lnUseQty = lnUseQty/IIF(nSellconv=0,1,nSellconv)  
  IF laSetups[4,2]='Y'
    laGlArray[1,1]  = GL_Sales
    laGlArray[1,2]  = '008'
    laGlArray[1,3]  = -1
    laGlArray[1,10] = cCOGSAcnt

    laGlArray[2,1]  = GL_COST
    laGlArray[2,2]  = '015'
    laGlArray[2,3]  = 1
    laGlArray[2,10] = cICAcnt
  ENDIF
  
  PRIVATE lcRefer
  IF MInvHdr.DIRECT_INV
    lcRefer = 'CUST# '+ Customer.Account + "-" + Customer.BTName
  ELSE
    lcRefer = 'CUST# '+ Customer.Account + " Sales order " + MInvHdr.CMORDER
  ENDIF

  *B606163,1 ABD - Remark the Next following Lines. [Begin]
  *=gfStyCrl('4',Style,IIF(EMPTY(MInvLine.cWareCode),MaSoLin.cWareCode,MInvLine.cWareCode),Dyelot,ldVDate,laData[1],;
  *          @laInvQty,Cost,lcRefer,lcGlSession,'',8,lcInvLine,'nSteps',@laGlArray,MInvLine.LineNo)
  *=gfMatCrl('5',m.Fabric,m.Color,m.cWareCode,m.Dyelot,&lcInvHdr..InvDate,&lcInvHdr..dPostDate,;
  *          laData[1],lnUseQty,CostUse,'','',0,'','',@laGlArray,lcSession,'','','','','','','',@laOtherPar)
  *- Close this Function
  *=gfMatCrl('5',mInvLine.Fabric,mInvLine.Color,mInvLine.cWareCode,mInvLine.Dyelot,;
  *          mInvHdr.InvDate,mInvHdr.dPostDate,laData[1],;
  *          lnUseQty,CostUse,'','',0,'','',@laGlArray,lcSession,'','','','','','','',@laOtherPar)
  *- Update Rolls  Matinvjl
  IF &lcInvLine..nSteps < 7
    
    = lfFilsUpd ()
    
    SELECT (lcInvLine)
    =RLOCK()
    REPLACE nSteps WITH 7
    UNLOCK
  ENDIF
  *B606163,1 ABD - [End]
  
ENDSCAN

*N000391,1 This code commented out it will never be applied. [Begin]
*IF 'AL' $ gcCmpModules
*  PRIVATE lcCurFile , lcInvFile
*  lcCurFile = ALIAS()
*  =SEEK(laData[1],'MInvHdr')
*  lcInvFile = IIF(MInvHdr.Consol='Y','MConsInH','MInvHdr')
*  SELECT (lcInvFile)
*  =SEEK(laData[1],lcInvFile)
*  SCAN REST WHILE cMatInv = laData[1]
*    IF SEEK('O'+CMORDER,'MaSoLin')
*      PRIVATE llPikLine , llPack_Lin , llPikTkt                    
*      llPikTkt = gfOpenFile(gcDataDir+'PikTkt',gcDataDir+'OrdPik','SH')
*      llPack_Lin = gfOpenFile(gcDataDir+'Pack_Lin',gcDataDir+'Pack_Lin','SH')
*      llPikLine = gfOpenFile(gcDataDir+'PikLine',gcDataDir+'PikLine','SH')
*      IF SEEK(MaSoLin.CMORDER+&lcInvFile..PikTkt,'PikTkt')
*        SELECT PikTkt
*        =RLOCK()
*        REPLACE Status WITH "O"
*        UNLOCK
*      ENDIF          
*      IF SEEK(&lcInvFile..PikTkt+MaSoLin.CMORDER,'PikLine')
*        SELECT PikLine
*        SCAN REST WHILE PikTkt + CMORDER = &lcInvFile..PikTkt+MaSoLin.CMORDER
*          IF SEEK('O'+CMORDER+STR(LineNo,6),'MaSoLin')
*            SELECT MaSoLin
*            =RLOCK()
*            REPLACE Pik1 WITH Pik1 + PikLine.Pik1 ,;
*                    Pik2 WITH Pik2 + PikLine.Pik2 ,;
*                    Pik3 WITH Pik3 + PikLine.Pik3 ,;
*                    Pik4 WITH Pik4 + PikLine.Pik4 ,;
*                    Pik5 WITH Pik5 + PikLine.Pik5 ,;
*                    Pik6 WITH Pik6 + PikLine.Pik6 ,;
*                    Pik7 WITH Pik7 + PikLine.Pik7 ,;
*                    Pik8 WITH Pik8 + PikLine.Pik8 ,;
*                    TotPik WITH TotPik + PikLine.TotPik ,;
*                    PikTkt WITH PikLine.PikTkt ,;
*                    PikDate WITH PikLine.PikDate ,;
*                    Picked WITH .T.
*            REPLACE Dyelot WITH  PikLine.Dyelot                    
*            IF SEEK(MaSoLin.PikTkt,'Pack_Lin')
*              SELECT Pack_Lin
*              LOCATE REST WHILE Pack_No = MaSoLin.PikTkt FOR MaSoLin.LineNo = nOrdLineNo
*              IF FOUND()
*                SELECT MaSoLin
*                REPLACE nPck1 WITH nPck1 + Pack_Lin.Qty1 ,;
*                        nPck2 WITH nPck2 + Pack_Lin.Qty2 ,;
*                        nPck3 WITH nPck3 + Pack_Lin.Qty3 ,;
*                        nPck4 WITH nPck4 + Pack_Lin.Qty4 ,;
*                        nPck5 WITH nPck5 + Pack_Lin.Qty5 ,;
*                        nPck6 WITH nPck6 + Pack_Lin.Qty6 ,;
*                        nPck7 WITH nPck7 + Pack_Lin.Qty7 ,;
*                        nPck8 WITH nPck8 + Pack_Lin.Qty8
*              ENDIF                      
*            ENDIF
*            UNLOCK            
*            IF laSetups[8,2]='Y' AND SEEK(Fabric+Color,'Fabric') AND Fabric.cDye_Flg = 'Y' AND ;   
*              SEEK(Fabric+Color+cWareCode+Dyelot,'FabDye')          
*              SELECT FabDye
*              =RLOCK() 
*              REPLACE Alo1 WITH Alo1 + MaSoLin.Pik1 ,;
*                      Alo2 WITH Alo2 + MaSoLin.Pik2 ,;
*                      Alo3 WITH Alo3 + MaSoLin.Pik3 ,;
*                      Alo4 WITH Alo4 + MaSoLin.Pik4 ,;
*                      Alo5 WITH Alo5 + MaSoLin.Pik5 ,;
*                      Alo6 WITH Alo6 + MaSoLin.Pik6 ,;
*                      Alo7 WITH Alo7 + MaSoLin.Pik7 ,;
*                      Alo8 WITH Alo8 + MaSoLin.Pik8 ,;
*                      TotAlo WITH TotAlo + MaSoLin.TotPik
*              UNLOCK        
*            ENDIF  
*            IF SEEK(Fabric+Color+cWareCode+SPACE(10),'FabDye')
*              SELECT FabDye
*              =RLOCK() 
*              REPLACE Alo1 WITH Alo1 + MaSoLin.Pik1 ,;
*                      Alo2 WITH Alo2 + MaSoLin.Pik2 ,;
*                      Alo3 WITH Alo3 + MaSoLin.Pik3 ,;
*                      Alo4 WITH Alo4 + MaSoLin.Pik4 ,;
*                      Alo5 WITH Alo5 + MaSoLin.Pik5 ,;
*                      Alo6 WITH Alo6 + MaSoLin.Pik6 ,;
*                      Alo7 WITH Alo7 + MaSoLin.Pik7 ,;
*                      Alo8 WITH Alo8 + MaSoLin.Pik8 ,;
*                      TotAlo WITH TotAlo + MaSoLin.TotPik
*              UNLOCK                
*            ENDIF        
*            IF SEEK(Fabric+Color,'Fabric')
*              SELECT Fabric
*              =RLOCK() 
*              REPLACE Alo1 WITH Alo1 + MaSoLin.Pik1 ,;
*                      Alo2 WITH Alo2 + MaSoLin.Pik2 ,;
*                      Alo3 WITH Alo3 + MaSoLin.Pik3 ,;
*                      Alo4 WITH Alo4 + MaSoLin.Pik4 ,;
*                      Alo5 WITH Alo5 + MaSoLin.Pik5 ,;
*                      Alo6 WITH Alo6 + MaSoLin.Pik6 ,;
*                      Alo7 WITH Alo7 + MaSoLin.Pik7 ,;
*                      Alo8 WITH Alo8 + MaSoLin.Pik8 ,;
*                      TotAlo WITH TotAlo + MaSoLin.TotPik
*              UNLOCK                
*            ENDIF            
*          ENDIF
*          SELECT PikLine
*          DELETE
*        ENDSCAN  
*      ENDIF
*      IF llPikTkt
*        USE IN PikTkt
*      ENDIF
*      IF llPack_Lin
*        USE IN Pack_Lin
*      ENDIF
*      IF llPikLine
*        USE IN PikLine
*      ENDIF  
*    ENDIF
*  ENDSCAN  
*  =SEEK(laData[1],'MInvHdr')
*  =SEEK(laData[1],lcInvFile)
*  SELECT (lcCurFile)
*ENDIF 

*N000391,1 Uptil now we don't update ArCusHst. [Begin]
*-- update the customer history file
*SELECT arCusHst
*IF SEEK(laData[2]+lcInvYear) .AND. &lcInvHdr..nSteps < 7
*  lnShipAmt = ROUND(MInvHdr.FabSHIPAMT &lcExRSin MInvHdr.nExRate ;
*                  &lcUntSin MInvHdr.nCurrUnit, 2)
*  lnDiscount = ROUND(ABS(MInvHdr.DISCOUNT) &lcExRSin MInvHdr.nExRate ;
*                  &lcUntSin MInvHdr.nCurrUnit, 2)
*  =RLOCK()
*  REPLACE nSlsQty&lcInvPeriod  WITH nSlsQty&lcInvPeriod  - MInvHdr.FabSHIP ,;
*          nSlsQty              WITH nSlsQty              - MInvHdr.FabSHIP ,;
*          nSlsAmt&lcInvPeriod  WITH nSlsAmt&lcInvPeriod  - lnShipAmt   ,;
*          nSlsAmt              WITH nSlsAmt              - lnShipAmt   ,;
*          nDisAmt&lcInvPeriod  WITH nDisAmt&lcInvPeriod  - lnDiscount  ,;
*          nDisAmt              WITH nDisAmt              - lnDiscount  ,;
*          nCOGSAmt&lcInvPeriod WITH nCOGSAmt&lcInvPeriod - lnCOGSAmt   ,;
*          nCOGSAmt             WITH nCOGSAmt             - lnCOGSAmt
*  UNLOCK
*  =gfTraceKey('arCusHst',laData[2]+lcInvYear,'M')
*  SELECT (lcInvHdr)
*  =RLOCK()
*  REPLACE nSteps WITH 7
*  UNLOCK
*ENDIF
*N000391,1 Uptil now we don't update ArCusHst. [End]

SELECT MINVHDR
IF laSetups[4,2]='Y'
  IF llIsEngland
    =gfOpenFile(gcDataDir+'MInvChrg',gcDataDir+'MInvChrg','SH')
    IF SEEK(MInvHdr.cMatInv,'MInvChrg')
      SELECT MInvChrg
      SCAN REST WHILE cMatInv= MInvHdr.cMatInv
        DO GLDIST WITH MInvHdr.LINK_CODE,'004',MInvChrg.nChrgAmnt,'VM',;
                  MInvHdr.cMatInv,ldVDate,lcSysYear,lcSysPeriod,lcGlDist,;
                  MInvChrg.cFrgtAcnt,MInvHdr.cCurrCode,MInvHdr.nCurrUnit,MInvHdr.nExRate
      ENDSCAN
    ENDIF
    =gfCloseFile('MInvChrg')
  ELSE
    DO GLDIST WITH MInvHdr.LINK_CODE,'004',(MInvHdr.Freight+MInvHdr.Insur+MInvHdr.Cod),;
              'VM',MInvHdr.cMatInv,ldVDate,lcSysYear,lcSysPeriod,lcGlDist,;
              MInvHdr.cFrgtAcnt,MInvHdr.cCurrCode,MInvHdr.nCurrUnit,MInvHdr.nExRate
  ENDIF                 
  IF laSetups[9,2]='Y'
    DO GLDIST WITH MInvHdr.LINK_CODE,'014',MInvHdr.TAX_AMT+MInvHdr.nPstAmt+MInvHdr.nHstAmt,;
                   'VM',MInvHdr.cMatInv,ldVDate,lcSysYear,lcSysPeriod,;
                   lcGlDist,MInvHdr.cTaxAcnt,MInvHdr.cCurrCode,MInvHdr.nCurrUnit,;
                   MInvHdr.nExRate
  ENDIF
  DO GLDIST WITH MInvHdr.LINK_CODE,'001',-(MInvHdr.TOTALCHG),'VM', ;
                 MInvHdr.cMatInv,ldVDate,lcSysYear,lcSysPeriod,lcGlDist,;
                 MInvHdr.cArAcnt,MInvHdr.cCurrCode,MInvHdr.nCurrUnit,;
                 MInvHdr.nExRate  
ENDIF
SELECT MINVHDR
=RLOCK()
REPLACE STATUS     WITH 'V',;
        CUSTPO     WITH '*VOID*',;
        VDATE      WITH ldVDate
REPLACE VCOMMAMT1  WITH IIF(VCOMMAMT1=0,COMMAMT1,VCOMMAMT1),;
        VCOMMAMT2  WITH IIF(VCOMMAMT2=0,COMMAMT2,VCOMMAMT2),;
        VFabSHIP   WITH IIF(VFabSHIP=0,FabSHIP,VFabSHIP),;
        VFabSHPAMT WITH IIF(VFabSHPAMT=0,FabSHIPAMT,VFabSHPAMT),;
        VCOD       WITH IIF(VCOD=0,COD,VCOD),;
        VCOD_AMT   WITH IIF(VCOD_AMT=0,COD_AMT,VCOD_AMT),;
        VINSUR     WITH IIF(VINSUR=0,INSUR,VINSUR),;
        VFREIGHT   WITH IIF(VFREIGHT=0,FREIGHT,VFREIGHT),;
        VDISCOUNT  WITH IIF(VDISCOUNT=0,DISCOUNT,VDISCOUNT),;
        VTOTALCHG  WITH IIF(VTOTALCHG=0,TOTALCHG,VTOTALCHG),;
        COMMAMT1   WITH 0,;
        COMMAMT2   WITH 0,;
        FabSHIP    WITH 0,;
        FabSHIPAMT WITH 0,;
        COD        WITH 0,;
        COD_AMT    WITH 0,;
        INSUR      WITH 0,;
        FREIGHT    WITH 0,;
        DISCOUNT   WITH 0,;
        TOTALCHG   WITH 0 ,;
        nVPstAmt   WITH IIF(nVPstAmt=0,nPstAmt,nVPstAmt)  ,;
        nPstAmt    WITH 0        ,;
        VTAX_AMT   WITH IIF(VTAX_AMT=0,TAX_AMT,VTAX_AMT)  ,;
        TAX_AMT    WITH 0        ,;
        nvCharges  WITH IIF(nvCharges=0,nCharges,nvCharges) ,;
        nCharges   WITH 0
REPLACE nVHstAmt   WITH IIF(nVHstAmt=0,nHstAmt,nVHstAmt)  ,;
        nHstAmt    WITH 0
UNLOCK
=gfTraceKey('MINVHDR',laData[1],'M')

IF MInvHdr.CONSOL='Y'
  SELECT MConsInH
  =SEEK(MInvHdr.cMatInv)
  SCAN REST WHILE cMatInv = MInvHdr.cMatInv
    REPLACE REST STATUS WITH 'V',;
                 CUSTPO WITH '*VOID*' 
    =gfTraceKey('MConsInH',cMatInv+STORE+CMORDER,'M')
  ENDSCAN
ENDIF
IF laSetups[4,2]='Y' AND !SEEK(laData[1]+'VM'+lcGlSession,'GLDIST')
  WAIT 'Updating the general ledger distribution file ...' WINDOW NOWAIT
  SELECT (lcGlDist)

  SCAN
    REPLACE GLSESSION WITH lcGlSession
    =gfAdd_Info(lcGlDist)  
  ENDSCAN
  
  USE
  SELECT GLDIST
  APPEND FROM (gcWorkDir+lcGlDist)
  =gfTraceKey('GLDIST',laData[1]+'VM'+lcGlSession,'A')
ENDIF

IF EMPTY(MInvHdr.cFacCode) .OR. laSetups[17,2]='Y'

  SELECT DEBIT
  =SEEK('M'+laData[1])
  lnCreAmt = 0 
  
  *-- Scan debit records.
  SCAN REST WHILE TRANTYPE+TRAN+CINSTALNO='M'+laData[1]
    SCATTER MEMVAR MEMO
    m.cShToOpn  = 'Y'
    
    INSERT INTO (lcTmpAr) FROM MEMVAR
    lnCreAmt = lnCreAmt - m.Amount
  ENDSCAN

  *-- Add Credit Data to master credit file[Begin]
  m.Batch     = gfsequence('BATCH')
  m.Tran = MInvHdr.cMatInv
  m.TranType  = 'I' 
  m.Desc      = 'CREDIT ADJ./VOID INV'
  m.Reference = 'Void Mat. Invoice'
  m.Amount    = lnCreAmt
  m.cInstalNo = ''
  m.TranDate=ldVDate
  SELECT CREDIT
  =RLOCK()
  INSERT INTO ('CREDIT') FROM MEMVAR
  =gfAdd_Info('CREDIT')
  =gfTraceKey('CREDIT','I'+Tran,'A')

  UNLOCK
  *-- Add Credit Data to master credit file[End  ]

  *-- Add Credit Data to temporary history file[Begin]
  =RLOCK(lcTmpAr)
  INSERT INTO (lcTmpAr) FROM MEMVAR
  UNLOCK  
  *-- Add Credit Data to temporary history file[End  ]
  
  *-- Call key off program.
  DO lfKeyOff IN (gcapphome+'ARKEYOF.PRG') WITH ;
           MINVHDR.ACCOUNT,ldVDate,ABS(lnCreAmt),lnCreAmt,lcTmpAR

  SELECT CUSTOMER
  IF SEEK('M'+laData[2]) .AND. &lcInvHdr..nSteps < 8
    lnEqvAmnt = ABS(MInvHdr.VTotalChg) &lcExRSin MINVHDR.nExRate;
                &lcUntSin MInvHdr.nCurrUnit * -1
    =lfHgWUpdat() 
    =gfTraceKey('CUSTOMER','M'+laData[2],'M')
    SELECT (lcInvHdr)
    =RLOCK()
    REPLACE nSteps WITH 8
    UNLOCK
  ENDIF

ENDIF

*N000391,1 SSE Commented out. [Begin]
*IF 'EB' $ gcCmpModules
*  llEdiTrans = gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')
*  SELECT EDITRANS
*  IF SEEK('810'+PADR(MInvHdr.cMatInv,20)+'A'+MInvHdr.Account)
*    DELETE
*  ENDIF
*  IF SEEK('810'+PADR(MInvHdr.cMatInv,20)+'F'+MInvHdr.cFacCode)
*    DELETE
*  ENDIF
*  IF llEdiTrans
*    USE IN 'EDITRANS'
*  ENDIF
*ENDIF
*N000391,1 SSE Commented out. [End]

SELECT unCmSess
REPLACE STATUS WITH 'C'
llContinue = .F.
UNLOCK

*N000391,1 We didn't use those 2 files. [Begin]
*=gfCloseFile('icStyHst')
*=gfCloseFile('arCusHst')
*N000391,1 We didn't use those 2 files. [End]

=gfCloseFile('DEBIT')
=gfCloseFile('REPCOMM')
IF MInvHdr.CONSOL = 'Y' 
  =gfCloseFile('MConsInH')
  =gfCloseFile('MConsInL')
ENDIF
SELECT MINVHDR
WAIT CLEAR
*-- Message : 40004
*-- Invoice#  has been Voided.
*-- Button : 00000 
*-- Ok
=gfModalGen('INM40004B00000','ALERT',laData[1])

*N000391,1 Close the rolls file. [Begin]
=gfCloseFile('Rolls')
*N000391,1 Close the rolls file. [End]

IF EMPTY(laData[24]) OR laSetups[17,2]='Y'
  =gfCloseFile('ARHIST')
  =gfCloseFile('CREDIT')
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvSend
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Send Invoice EDI File
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSend()
*!*************************************************************
FUNCTION lfvSend

*!*************************************************************
*! Name      : lfvDefaults
*! Developer : WAM
*! Date      : 07/01/1996
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
lcIDefSes = ALLTRIM(laSeasons[lnSeason,2])
lcIDefDiv = ALLTRIM(laDivision[lnDivision,2])
IF EMPTY(ldDefInvDate)
  *-- Message : 40032
  *-- Must enter a default invoice date.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('INM40032B00000','ALERT','invoice')
  _CUROBJ = OBJNUM(ldDefInvDate)
  RETURN
ENDIF
*-- if the gl module is installed then a default posting date must be entered
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
ELSE
  lcIDefWare = laWareHouses[lnWareHouse,2]
  *-- save the new defaults
  IF cbSetAsDef
    SAVE ALL LIKE lcIDef* TO ("INVDEF.MEM")
  ENDIF
ENDIF
CLEAR READ

*!*************************************************************
*! Name      : lfvInvDate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice date
*!*************************************************************
*! Calls     : CODECHK,lfvTerms,lfUpdate
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInvDate()
*!*************************************************************
FUNCTION lfvInvDate
IF &lcInvHdr..InvDate <> laData[41]
  IF laData[41] > laData[49]
    *-- Message : 40086
    *-- invoice date cannot be greater than posting date
    *-- Button : 00000 
    *-- Ok
    =gfModalGen('TRM40086B00000','ALERT','')
    STORE &lcInvHdr..InvDate TO laData[41]
    RETURN
  ENDIF
  =lfvTerms() .AND. lfUpdate()
ENDIF

*!*************************************************************
*! Name      : lfvPostDate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Posting date
*!*************************************************************
*! Calls     : CODECHK,lfvTerms,lfUpdate
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPostDate()
*!*************************************************************
FUNCTION lfvPostDate

IF &lcInvHdr..dPostDate <> laData[49]
  *-- Message : 40086
  *-- invoice date cannot be greater than posting date
  *-- Button : 00000 
  *-- Ok
  *-- ladata[41]= invoice date
  IF (laData[41] > laData[49] .AND. gfModalGen('TRM40086B00000','ALERT','')=1) .OR. ;
    !CHECKPRD(laData[49],'lcGlYear','lcGlPeriod','IN')
    STORE &lcInvHdr..dPostDate TO laData[49]
    RETURN
  ENDIF  
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE dPostDate WITH laData[49]
  UNLOCK
  llCUpdate = .T.
ENDIF

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

IF laSetups[4,2]='Y' .AND. USED(lcGlDist)
  USE IN (lcGlDist)
ENDIF
IF !laScrMode[3]  
  SELECT unCmSess  
  IF SEEK('O'+PADR('MDINVOICE',10)+PADR(gcUser_id,10)+lcSession)
    REPLACE STATUS WITH 'I'
    llContinue = .F.
    UNLOCK
  ENDIF
ENDIF           

SELECT MInvHdr
IF laScrMode[4]
  SCATTER FIELDS &lcScFields TO laData BLANK
ELSE
  SCATTER FIELDS &lcScFields TO laData 
ENDIF

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
PRIVATE llSaveInv

PRIVATE lcCInvOrd,lnCAlSel  
IF laScrMode[3] 
  SELECT MINVHDR
  =lfEdtInv(lcEdtFld)
  =gfOpenFile(gcDataDir+'DEBIT',gcDataDir+'DEBIT','SH')
  SELECT DEBIT
  IF SEEK(laData[2]+laData[1]+SPACE[3]+DTOS(laData[41]))
    =RLOCK()
    REPLACE DUEDATE WITH laData[7] 
    UNLOCK
  ELSE
    =gfOpenFile(gcDataDir+'ARHIST',gcDataDir+'ARHISTT','SH')
    SELECT ARHIST
    IF SEEK(laData[2]+laData[1]+SPACE[3])
      =RLOCK()
      REPLACE DUEDATE WITH laData[7]
      UNLOCK
    ENDIF  
  ENDIF
  
  SELECT MINVHDR
  RETURN
ENDIF

lnCAlSel = SELECT(0)
SELECT (lcInvHdr)
IF !llIsEngland .AND. lCompUps
    lcCInvOrd = ORDER()
    DO gpUpsChrg IN (gcapphome+gcwinappl+'\ARMTINV.PRG') WITH ;
      laData[2],laData[5],laData[3],ladata[30],laData[21],laData[31],llCod,;
      llupsinsur,laData[32],laData[46],laData[44],laData[45],lcUpsBox,'laData[10]',;
      'laData[28]','laData[34]','laData[35]','laData[36]','laData[25]','laData[26]',;
      'laData[37]','laData[43]',IIF(UPPER(ALLTRIM(gcContCode))='USA',;
      lnTaxDue,laData[31]),"A"
    SELECT (lcInvHdr)
    SET ORDER TO TAG (lcCInvOrd)  
ENDIF  

*-- Balance to pay
laData[38] = laData[31]+laData[34]+laData[35]+laData[36] - ;
              ROUND(ABS(laData[31]*laData[32]/100),2)+laData[37]+laData[43]+laData[39]
laData[38] = laData[38] + laData[52]

*-- Merch discount
laData[33] = ROUND((-1*laData[31]*laData[32]/100),2)

SELECT (lcInvHdr)
laData[25] = IIF(laData[25]=0,1,laData[25])
=RLOCK()
REPLACE FabShip    WITH laData[42] ,;
        FabShipAmt WITH laData[31] ,;
        Discount   WITH laData[33] ,;
        Freight    WITH laData[34] ,;
        Insur    WITH laData[35] ,;
        Cod      WITH laData[36] ,;
        Cod_Amt  WITH laData[28] ,; 
        Weight   WITH laData[26] ,;
        Tax_Amt  WITH laData[37] ,;
        nPstAmt  WITH laData[43] ,;
        Cartons  WITH laData[25] ,;
        TotalChg WITH laData[38] ,;
        Cod_Flag WITH IIF(llCod,'Y','N') ,;
        lKeyOff  WITH IIF(llCod,lKeyOff,.F.) ,;
        lCompUps WITH .F.

REPLACE nHstAmt WITH laData[52]

SELECT (lnCAlSel)
*-- Calculate merchendise tax
=IIF(llIsEngland AND llReMerTax ,lfMechTax(),.T.)
llSaveInv = .T.
*-- check if the invoice data is valid or not
DO gfChkSavInv IN (gcapphome+gcwinappl+'\ARMTINV.PRG') WITH ;
laData[2],laData[3],laData[5],laData[30],lcInvHdr,IIF(USED(lcInstHdr),lcInstHdr,''),;
IIF(USED(lcAppCrdt),lcAppCrdt,''),IIF(USED(lcUpsBox),lcUpsBox,''),'llSaveInv'
IF !llSaveInv
*-- cannot save the invoice
  llCSave = .F.
  SELECT (IIF(lnActFolder=2,lcInvLine,lcInvHdr))
  RETURN
ENDIF
*--start saving
SELECT UNCMSESS
REPLACE cCurrObj WITH 'pbSav'
USE IN  UNCMSESS
=gfOpenFile(gcDataDir+'UNCMSESS','TRANS','SH')
=SEEK ('O'+PADR('MDINVOICE',10)+PADR(gcUser_id,10)+lcSession,'UNCMSESS')
DECLARE laInv[1]
STORE '' TO laInv
DO gpSaveInv IN (gcapphome+gcwinappl+'\ARMTINV.PRG') WITH ;
lcInvHdr,lcInvLine,IIF(USED(lcUpsBox),lcUpsBox,''),IIF(USED(lcEngChrg),lcEngChrg,''),;
IIF(USED(lcInstHdr),lcInstHdr,''),IIF(USED(lcInstLin),lcInstLin,''),'',lcGlSession,'laInv'
SELECT unCmSess
IF SEEK('O'+PADR('MDINVOICE',10)+PADR(gcUser_id,10)+lcSession)
  REPLACE STATUS WITH 'C'
  llContinue = .F.
  UNLOCK
ENDIF
*-- Message : 40005
*-- Invoice has been saves as.
*-- Button : 00000 
*-- Ok
=gfModalGen('INM40005B00000','ALERT',laInv[1])

*-- invoice number
laData[1] = laInv[1]
SELECT MINVHDR
RETURN
*-- end of lpsavscr.

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Restore invoice information
*!*************************************************************
*! Calls     : gfGetAdr,lfBrowDet
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGetInfo()
*!*************************************************************
FUNCTION lfGetInfo
PRIVATE lcHdrFile

lcHdrFile = IIF(laScrMode[2],'MINVHDR',lcInvHdr)
=SEEK(IIF(laScrMode[2],laData[1],laData[2]+laData[3]+laData[5]+laData[30]),lcHdrFile)
=SEEK(IIF(EMPTY(laData[5]),'M'+laData[2],'S'+laData[2]+laData[5]),'Customer')

PRIVATE lnCusRecNo,lcDistCtr
lnCusRecNo = RECNO('Customer')
*-- get the store which is defined as distribution center for the customer
IF !EMPTY(Customer.Dist_Ctr)
  lcDistCtr = Customer.Dist_Ctr
  llNoThing = SEEK('S'+laData[2]+lcDistCtr,'Customer')
ENDIF

=SEEK('O'+laData[3],'MaSoHdr')

DECLARE laInsRltd[1,2]
laInsRltd[1,1]='LINSTALLM'
laInsRltd[1,2] = 'llInstTerm'
=gfRltFld(laData[9],@laInsRltd,'CTERMCODE')

*-- get the ship address or the alternate ship address
lnShipAddr = IIF(laScrMode[4],lnShipAddr,IIF(MaSoHdr.Alt_ShpTo,2,1))
STORE '' TO  lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5
lcBillName = Customer.BtName
=gfGetAdr('Customer','','','',1,'2')
*-- get the bill address
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
ENDFOR  
IF lnShipAddr = 2
*-- Alternate ship address
  lcShipName = IIF(laScrMode[2],MaSoHdr.StName,&lcInvHdr..StName)
  lcShipAdd1 = IIF(laScrMode[2],MaSoHdr.cAddress1,&lcInvHdr..cAddress1)
  lcShipAdd2 = IIF(laScrMode[2],MaSoHdr.cAddress2,&lcInvHdr..cAddress2)
  lcShipAdd3 = IIF(laScrMode[2],MaSoHdr.cAddress3,&lcInvHdr..cAddress2)
  lcShipAdd4 = IIF(laScrMode[2],MaSoHdr.cAddress4,&lcInvHdr..cAddress4)
  lcShipAdd5 = IIF(laScrMode[2],MaSoHdr.cAddress5,&lcInvHdr..cAddress5)
ELSE
  lcShipName =  IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
  =gfGetAdr('CUSTOMER','','','',1,'')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  ENDFOR  
ENDIF  

IF RECCOUNT('Customer') >= lnCusRecNo
  GO lnCusRecNo IN Customer
ENDIF

SELECT (lcHdrFile)
*-- Restore invoice status
DO CASE
  CASE &lcHdrFile..STATUS = 'V'
    lcInvStat = 'Void'    
    lcStatColor = "RGB(255,255,255,255,0,0)"
    SCATTER FIELDS &lcVScFields TO laData
  CASE &lcHdrFile..STATUS = 'O'
    lcInvStat = 'Open'
    lcStatColor = "RGB(0,0,0,255,255,0)"
  CASE &lcHdrFile..STATUS = 'C'
    lcInvStat = 'Complete'
    lcStatColor = "RGB(255,255,255,0,128,0)"
  OTHERWISE
    lcInvStat = ''
    lcStatColor = gcObjColor
ENDCASE
*-- Restore cash on delivery
llCod = &lcHdrFile..Cod_Flag='Y'
lnPstRule = VAL(laData[44])
=gfwCodePop(@laCodes,'CTERMCODE','T')
=gfwCodePop(@laCodes,'SHIPVIA','T')
=gfwCodePop(@laCodes,'SPCINST','T')
=gfwCodePop(@laCodes,'SEASON','T')
=gfwCodePop(@laCodes,'CDIVISION','T')
lnWareHouse = ASCAN(laWareHouses,laData[21])
lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
lnWareHouse = IIF(llPOSSetup,lnPOSWeHs,lnWareHouse)
llReBrowse = .T.
=lfActFolder()

*!*************************************************************
*! Name      : lfvLineNote
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Invoice line notepad
*!*************************************************************
*! Calls     : INV200LN.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvLineNote()
*!*************************************************************
FUNCTION lfvLineNote
laScrMode[2]=IIF(laScrMode[3],.T.,laScrMode[2])

DO (gcScrDir+"ARLNOTES.SPX")

laScrMode[2]=IIF(laScrMode[3],.F.,laScrMode[2])

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
SET SKIP OF PAD _INQUIRY OF _MSYSMENU laScrMode[1]
ON PAD _INQUIRY OF _msysmenu ACTIVATE POPUP _INQURYPOP

DEFINE POPUP _INQURYPOP MARGIN SHADOW
DEFINE BAR 1  OF _INQURYPOP PROMPT 'In\<voice Notes'         SKIP FOR !laScrMode[2]
DEFINE BAR 2  OF _INQURYPOP PROMPT '\<Object Link'           SKIP FOR !laScrMode[2]
DEFINE BAR 3  OF _INQURYPOP PROMPT 'Send \<EDI Invoice File' SKIP FOR !laScrMode[2]
DEFINE BAR 4  OF _INQURYPOP PROMPT 'Customer \<Inquire'   SKIP FOR EMPTY(laData[2])
DEFINE BAR 5  OF _INQURYPOP PROMPT 'Customer \<Notepad'   SKIP FOR EMPTY(laData[2])
DEFINE BAR 6  OF _INQURYPOP PROMPT '\<Aging Summary'      SKIP FOR EMPTY(laData[2])
DEFINE BAR 7  OF _INQURYPOP PROMPT '\<Fabric Inquire'     SKIP FOR (lnactfolder<>2) .OR. (EMPTY(m.Fabric) AND EMPTY(m.Color))
DEFINE BAR 8  OF _INQURYPOP PROMPT '\<Line Notepad'       SKIP FOR (lnactfolder<>2) .OR. laSetups[3,2]<>'Y' .OR. (EMPTY(m.Fabric) AND EMPTY(m.Color))
DEFINE BAR 9  OF _INQURYPOP PROMPT '\<Copy Line'          SKIP FOR (lnactfolder<>2) .OR. laScrMode[2] .OR. (EMPTY(m.Fabric) AND EMPTY(m.Color)) .OR. llZoom
DEFINE BAR 10 OF _INQURYPOP PROMPT '\<Paste Line'         SKIP FOR (lnactfolder<>2) .OR. laScrMode[2] .OR. (EMPTY(m.Fabric) AND EMPTY(m.Color)) .OR. llZoom
DEFINE BAR 11 OF _INQURYPOP PROMPT '\<Zoom Invoice Lines' SKIP FOR (lnactfolder<>2)
DEFINE BAR 12 OF _INQURYPOP PROMPT '\<Refresh Invoice Sales Rep. Commission' SKIP FOR laSetups[2,2] <> 'Y' OR !laScrMode[4]
SET MARK OF BAR 11 OF _INQURYPOP TO llZoom
ON SELECTION POPUP _INQURYPOP DO lpvInquiry

*!*************************************************************
*! Name      : lpvInquiry
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : None
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
  CASE BAR() = 1      && Invoice Notes
  	=lfvInvNote()
  CASE BAR() = 2      && Object Link
    =lfvObjLink()
  CASE BAR() = 3      && Send EDI Invoice File
    =lfvSend()
  CASE BAR() = 4      && Customer Inquire
  	lcParameter = "'"+laData[2]+"'"+IIF(EMPTY(laData[5]),'',",'"+laData[5]+"'")
    DO gpDoProg WITH 'AWRARCUST',.F.,'AR',lcParameter
  CASE BAR() = 5      && Customer Notepad
    =NOTEPAD('A',laData[2])  
  CASE BAR() = 6      && Customer Credit
    =SEEK('M'+ladata[2],'Customer')
    DO (gcScrDir+gcWinAppl+"\ARAGING.SPX") WITH laData[8]
  CASE BAR() = 7      && Style Inquire
  	lcParameter = "'"+m.Fabric+"','" + m.Color+ "'"
    DO gpDoProg WITH 'AWRMAMATRL',.F.,'MA',lcParameter
  CASE BAR() = 8      &&  Invoice Line Notepad
    =lfvLineNote()
  CASE BAR() = 9      &&  Copy Invoice Line
   =lfvCopyQty()
  CASE BAR() = 10      &&  Paste Invoice LIne
   =lfvPasteQty()
  CASE BAR() = 11     &&   Zoom Invoice Browse
    llZoom = !llZoom
    =lfvBrowZoom()
    SET MARK OF BAR 11 OF _INQURYPOP TO llZoom
  CASE BAR() = 12     && Refresh sales rep. header commession befor saving
    =lfRefComm()  
ENDCASE

*!*************************************************************
*! Name      : lfvBrowZoom
*! Developer : WAM
*! Date      : 07/01/1996
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
  =lfBrowDet()
ELSE
  llReBrowse=.T.
  =lfActFolder()
ENDIF

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

STORE .T. TO llNewline,llAddLine
SCATTER MEMVAR BLANK MEMO
m.Style = lcLastStyle
SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
SHOW GETS WINDOW (lcWinCh34) DISABLE ONLY
SHOW GETS WINDOW (lcWinCh35) DISABLE ONLY
=lfRefresh(lcWinCh32)
=lfRefresh(lcWinCh34)
SHOW GET ibStyle ENABLE
SHOW GET m.Style ENABLE
_CUROBJ = OBJNUM(m.Style)

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Remove Invoice line
*!*************************************************************
*! Calls     : gfModalGen
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
  *--decrese the quantity per carton with the style quantity
  lnCartons  = lnCartons  - IIF(Style.Qty_Ctn>0,m.FabQty/Style.Qty_Ctn,0)
  laData[25] = CEILING(lnCartons)
  laData[25] = IIF(laData[25]=0,1,laData[25])

  *-- decrease the total wieght with style wieght
  laData[26] = laData[26] - m.FabQty*Style.nStyWeight
  *-- decrease the shipped amount 
  *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
  *laData[31] = laData[31] - m.FabQty*m.nSellNetPr
  laData[31] = laData[31] - CEILING(m.FabQty*m.nSellNetPr*100)/100
  *E126684,1  TMI [End  ] 
  
  *-- merchandise discount
  laData[33] = -ROUND(laData[31]*laData[32]/100,2)
  *-- approval amount =Shipped amount
  laData[48] = laData[31]
  *-- decrease shipped quantyties
  laData[42] = laData[42] - m.FabQty
  *-- if this style was taxable then decrease the tax amount
  lnTaxDue   = IIF(lTaxable,lnTaxDue-m.FabQty*m.nSellNetPr,lnTaxDue)
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE CARTONS    WITH laData[25] ,;
          WEIGHT     WITH laData[26] ,;
          FabSHIPAMT WITH laData[31] ,;
          Discount   WITH laData[33] ,;
          FabSHIP    WITH laData[42] ,;
          APPRAMT    WITH laData[48] ,;
          nTaxDue    WITH lnTaxDue   ,;
          lCompUps   WITH .T.    
  UNLOCK
  SELECT (lcInvLine)
  
  IF laSetups[9,2]='Y' .AND. llIsEngland .AND. Style.nTaxBreak <> 0
    lnQuantity = 0
    FOR lnCount = Style.nTaxBreak TO 8
    *-- Sum the quantity for only the sizes that is Greater than the tax break weight
      lnQuantity = lnQuantity+EVAL('m.Qty'+STR(lnCount,1))
    ENDFOR
    *-- Decrease Merchandise tax
    lnMerchTax = lnMerchTax - lnQuantity * m.nSellNetPr * nTaxRate/100
  ENDIF
  SELECT (lcInvLine)
  DELETE
  GO TOP
  *-- change  the status  for the screen fields
  lcDispDet = IIF(EOF(),'DISABLE','')
  SCATTER MEMVAR
  SHOW WINDOW (lcInvBrow) REFRESH SAME
  SHOW GETS WINDOW (lcWinCh32) &lcDispDet ONLY
  SHOW GETS WINDOW (lcWinCh34) &lcDispDet ONLY
  SHOW GETS WINDOW (lcWinCh35) &lcDispDet ONLY
  =lfRefresh(lcWinCh32)
  =lfRefresh(lcWinCh34)
  IF EOF()
    SHOW GET pbNew ENABLE 
    SHOW GET pbPacks ENABLE
  ENDIF
  STORE .T. TO llReMerTax,llCUpdate
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

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : READ Activate function of invoice screen
*!*************************************************************
*! Calls     : gfStopBrow,lfClearKey.
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
IF WONTOP() <> lcWinCh1 .AND. WONTOP() <> lcFolder .AND. WONTOP() <> 'GWCCONTRL1' .AND. ;
   laScrMode[4] .AND. EMPTY(laData[2])
  _CUROBJ = OBJNUM(pbCls)
  RETURN
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate entered style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF
 
IF llBrowse .OR. ;
   ( (!llAddLine .OR. m.Style <> lcEmptySty) .AND. !SEEK(m.Style,'Style'))
   m.Style = IIF(llBrowse,'',m.Style)
  m.Style = PADR(gfStyBrw("I" , m.Style , "" , .F.),19)
  IF EMPTY(m.Style)
    STORE IIF(llAddLine,lcEmptySty,&lcInvLine..Style) TO m.Style
    _CUROBJ = OBJNUM(m.Style)
  ENDIF
  llBrowse = .F.
ENDIF

IF !llAddLine .AND. m.Style=&lcInvLine..Style
  RETURN
ENDIF
IF m.Style=lcEmptySty
  STORE m.Style TO lcLastStyle
  SCATTER MEMVAR MEMO
  STORE .F. TO llAddLine,llNewLine
  IF EMPTY(m.Style)
    SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh34) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh35) DISABLE ONLY
    SHOW GET pbNew     ENABLE
    SHOW GET ibInv200E ENABLE
    SHOW GET pbPacks   ENABLE
    _CUROBJ = OBJNUM(pbNew)
  ELSE
    SHOW GETS WINDOW (lcWinCh32) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh34) ENABLE ONLY
    SHOW GETS WINDOW (lcWinCh35) ENABLE ONLY
    _CUROBJ = OBJNUM(ibInv200E)
  ENDIF
  RETURN
ENDIF
SELECT (lcInvLine) 
SET RELATION OFF INTO STYLE
IF SEEK(m.Style,'Style')
  *-- Message : 40006
  *-- Style scale not found in the scale file. Cannot accept.
  *-- Button : 00000 
  *-- Ok

  *-- Message : 40007
  *-- This is a canceled style. Not allowed to invoice.
  *-- Button : 00000 
  *-- Ok

  *-- Message : 40008
  *-- This style/color is on hold. 
  *-- Button : 40001
  *-- Accept Reenter

  *-- Message : 40009
  *-- Styles restricted to XXX!
  *-- Button : 00000
  *-- Ok

  *-- Message : 40010
  *-- Style XXX date is 
  *-- Button : 40001
  *-- Accept Reenter

  *-- Message : 40085
  *-- No dyelots found for style xxxx in warehouse xxxxx
  *-- Button : 00000
  *-- Ok

  IF (!SEEK('S'+Style.Scale,'Scale') .AND. ;
      gfModalGen('TRM40006B00000','ALERT')=1) ;
   .OR. (Style.Status='X' .AND. ;
        gfModalGen('TRM40007B00000','ALERT')=1) ;
   .OR. (laSetups[8,2]='Y' .AND. Style.cDye_Flg='Y' .AND. !lfStyDye() .AND.;
        gfModalGen('TRM40085B00000','ALERT',lcStyHdr+': '+m.Style+IIF(laSetups[5,2]='Y',' in warehouse '+laData[21],''))=1) ;
   .OR. (Style.cDivision <> laData[13] .AND. ;
        gfModalGen('TRM40009B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;        
   .OR. IIF(ASCAN(laEvntTrig , PADR('STYLEVALID',10)) <> 0 AND laSetups[22,2] = .F.,.F.,(ALLTRIM(laData[12])<>'*' AND TRIM(Style.Season)<>'Y' AND Style.Season<>laData[12] .AND. ;
        gfModalGen('TRM40009B00000','ALERT','season '+ALLTRIM(laSeasons[lnSeason,1]))=1)) ;  
   .OR. (Style.Status='H' .AND. ;
        gfModalGen('QRM40008B40001','ALERT')=2) ;
   .OR. (!EMPTY(Style.Start) .AND. Style.Start > laData[41] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','start|'+DTOC(Style.Start))=2) ;
   .OR. (!EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[41] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2)
    STORE IIF(llAddLine,lcEmptySty,&lcInvLine..Style) TO m.Style

    SET RELATION TO STYLE INTO STYLE ADDITIVE
    _CUROBJ = OBJNUM(m.Style)
    RETURN
  ENDIF
  m.nSellPrice = lfGetprice(m.Style,lcPriceLvl,m.FabQty)
  
  SELECT (lcInvLine)
  IF m.nSellPrice < 0
    STORE IIF(llAddLine,lcEmptySty,&lcInvLine..Style) TO m.Style
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    _CUROBJ = OBJNUM(m.Style)
    RETURN
  ENDIF
  IF laSetups[5,2]='Y' .AND. !SEEK(m.Style+laData[21]+SPACE(10),'FabDye')
    *-- Message : 40012
    *-- Style xxx is not assigned to warehouse xxx
    *-- Button : 40002
    *-- Add Reenter
    IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.Style)+'|'+TRIM(laData[21]))=1
       *-- Add a new record to the FabDye file.
       DO gpAdStyWar WITH m.Style,SPACE(10),laData[21]
    ELSE
      STORE IIF(llAddLine,lcEmptySty,&lcInvLine..Style) TO m.Style
      SET RELATION TO STYLE INTO STYLE ADDITIVE
      _CUROBJ = OBJNUM(m.Style)
      RETURN
    ENDIF
  ENDIF
  lnTaxRate = 0
  SET ORDER TO TAG MInvLins
  IF SEEK(laData[2]+laData[3]+laData[5]+m.Fabric+m.Color)
    *-- Message : 40013
    *-- Warning! This style had been entered on this invoice
    *-- Button : 00000
    *-- Ok
    =gfModalGen('INM40013B00000','ALERT')
  ENDIF
  IF llAddLine
    lnLines   = lnLines + 1
    m.LineNo  = lnLines
    m.Account = laData[2]
    m.Store   = laData[5]
    m.InvDate = laData[41]
    m.cWareCode = laData[21]
    INSERT INTO (lcInvLine) FROM MEMVAR
  ELSE
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      IF lnCount > Scale.cnt
        m.Qty&lcCount = 0
      ENDIF
    ENDFOR
    m.FabQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
    *-- if the style quantity not divisible by Prepack
    IF !EMPTY(m.Prepak) AND SEEK('P'+m.Scale+m.Prepak,'Scale') AND ;
       MOD(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,Scale.PPTot) <> 0 
      m.Prepak=''
      m.PpQty = 0 
    ENDIF
  ENDIF
  SELECT STYLE
  SKIP
  lcLastStyle = Style 
  =SEEK(m.Style) 
  SELECT (lcInvLine)
  SET ORDER TO TAG MInvLine
  =SEEK(laData[2]+laData[3]+laData[5]+STR(m.LineNo,6))

  *-- get the cDiscCode From FabDye in every case
  lcDiscCode = IIF(SEEK(Fabric+Color+&lcInvHDr..cWareCode+SPACE(10),'FabDye'),StyDye.cDiscCode,'')
  IF !EMPTY(ALLTRIM(lcDiscCode))
    *-- Get the discount related field to now which 
    *-- type whole Sale Or Retail sale Or Both.
    DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
    STORE '' To lcDisType , ldStartDte ,ldEndDate
    *-- Array to get the Discount affect for DecCode.
    laDisType[1,1]  = 'CCOSTAFECT'
    laDisType[1,2]  = 'lcDisType'
    *-- Array to get the start date For DescCode.
    lastartDte[1,1] = 'START'
    lastartDte[1,2] = 'ldStartDte'
    *-- Array to get the end date For DescCode.
    laEndDate[1,1]  = 'DENDATE'
    laEndDate[1,2]  = 'ldEndDate'
    = gfRltFld(lcDiscCode, @laDisType, 'CDISCCODE')
    = gfRltFld(lcDiscCode, @laStartDte,'CDISCCODE')
    = gfRltFld(lcDiscCode, @laEndDate, 'CDISCCODE')
    STORE 0 To m.Disc_Pcnt , lnDisc_Pcnt
    IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[41],ldStartDte,ldEndDate)
      lnDisc_Pcnt = m.Disc_Pcnt
      m.Disc_Pcnt = lnDisc_Pcnt
      m.nSellNetPr = m.nSellPrice*(100-m.Disc_Pcnt)/100
      m.Disc_Pcnt = lnDisc_Pcnt
    ENDIF
  ENDIF  
  m.nSellNetPr = m.nSellPrice*(100-m.Disc_Pcnt)/100  
  =IIF(llAddLine .OR. FabQty = 0,.T.,lfvNPrice() AND lfChkTotQty())
  IF llIsEngland .AND. !Customer.lVatExem  .AND. Style.nTaxBreak <> 0
  *-- get the england style tax related field if it is not smaller the the tax Break
    =gfRltFld(Style.cTaxCode,@laEngStyTax,'CTAXCODE')  
  ENDIF
  llAddLine  = .F.
  llCUpdate  = .T.
  m.Desc1    = Style.Desc1
  m.Scale    = Style.Scale
  m.PrePak   = Style.PrePak
  m.Comm1    = IIF(Style.commission,laData[18],0)
  m.Comm2    = IIF(Style.commission,laData[20],0)
  m.lTaxAble = STYLE.lTaxAble
  m.Season   = Style.Season
  m.nTaxRate = lnTaxRate
  =RLOCK()
  GATHER MEMVAR MEMO
  UNLOCK
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  SHOW WINDOW (lcInvBrow) REFRESH SAME
  SHOW GETS WINDOW (lcWinCh32) ENABLE ONLY
  SHOW GETS WINDOW (lcWinCh34) ENABLE ONLY
  SHOW GETS WINDOW (lcWinCh35) ENABLE ONLY
  IF laSetups[2,2] <> 'Y' OR EMPTY(laData[17])
    SHOW GET m.Comm1 DISABLE
  ENDIF
  IF laSetups[2,2] <> 'Y' OR EMPTY(laData[19])
    SHOW GET m.Comm2 DISABLE
  ENDIF
  =lfRefresh(lcWinCh32)
  =lfRefresh(lcWinCh34)
  =lfPpStat()   
ENDIF  
  
*-- end of lfvStyle.

*!*************************************************************
*! Name      : lfStyDye
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
*! Example   :  =lfStyDye()
*!*************************************************************
FUNCTION lfStyDye
PRIVATE laDyelots

IF SEEK (m.Fabric+m.Color+laData[21]+SPACE(10),'FabDye')
  SKIP IN FabDye
  RETURN (FabDye.Fabric+FabDye.Color+FabDye.cwarecode= m.Fabric+m.Color+laData[21] AND !EMPTY(FabDye.Dyelot) )
ELSE 
  RETURN .F.
ENDIF

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
IF (llBrowse .OR. EMPTY(lcDyelot) .OR. !SEEK(m.Fabric+m.Color+laData[21]+lcDyelot,'FabDye'))
  lcAlias=SELECT()
  SELECT FabDye
  IF !SDYEBROW(m.Style,@lcDyelot,.T.,IIF(laSetups[5,2]='Y',laData[21],''))
    lcDyelot = Dyelot
  ENDIF  
  SELECT (lcAlias)
ENDIF
m.Dyelot = lcDyelot
llBrowse = .F.
IF Dyelot <> m.Dyelot
  llCUpdate = .T.
  =RLOCK()
  REPLACE Dyelot WITH m.Dyelot
  UNLOCK
  SHOW WINDOW (lcInvBrow) REFRESH SAME
ENDIF
IF EMPTY(m.Dyelot)
  _CUROBJ = _CUROBJ
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvStyGrp
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style/color group
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyGrp()
*!*************************************************************
FUNCTION lfvStyGrp

IF Group <> m.Group
  llCUpdate = .T.
  =RLOCK()
  REPLACE Group WITH m.Group
  UNLOCK
  SHOW WINDOW (lcInvBrow) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvPieces
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style pieces
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

IF m.FabQty < 0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.FabQty = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF EMPTY(m.Prepak) AND Scale.Cnt > 1
  RETURN
ENDIF

IF !EMPTY(m.Prepak) .AND. SEEK('P'+m.Scale+m.Prepak,'Scale')
  *-- Message : 40014
  *-- Total quantity is not evenly divisible by the prepak quantity xxx
  *-- Button : 00000 
  *-- Ok
  IF MOD(m.FabQty,Scale.PPTot) <> 0 .AND. ;
    gfModalGen('INM40014B00000','ALERT',STR(Scale.PPTot,4))=1
    _CUROBJ = _CUROBJ
    =SEEK('S'+m.Scale,'Scale')
    RETURN
  ENDIF
  m.Qty1 = IIF(Scale.PPTot>0,m.FabQty*Scale.PP1/Scale.PPTot,0)
  m.Qty2 = IIF(Scale.PPTot>0,m.FabQty*Scale.PP2/Scale.PPTot,0)
  m.Qty3 = IIF(Scale.PPTot>0,m.FabQty*Scale.PP3/Scale.PPTot,0)
  m.Qty4 = IIF(Scale.PPTot>0,m.FabQty*Scale.PP4/Scale.PPTot,0)
  m.Qty5 = IIF(Scale.PPTot>0,m.FabQty*Scale.PP5/Scale.PPTot,0)
  m.Qty6 = IIF(Scale.PPTot>0,m.FabQty*Scale.PP6/Scale.PPTot,0)
  m.Qty7 = IIF(Scale.PPTot>0,m.FabQty*Scale.PP7/Scale.PPTot,0)
  m.Qty8 = IIF(Scale.PPTot>0,m.FabQty*Scale.PP8/Scale.PPTot,0)
ENDIF
=SEEK('S'+m.Scale,'Scale')
IF Scale.Cnt = 1
  m.Qty1 = m.FabQty
ENDIF
IF m.Qty1=Qty1 AND m.Qty2=Qty2 AND m.Qty3=Qty3 AND m.Qty4=Qty4 AND ;
   m.Qty5=Qty5 AND m.Qty6=Qty6 AND m.Qty7=Qty7 AND m.Qty8=Qty8
  RETURN
ENDIF
*-- Check total pieces againest total quantity entered
IF lfChkTotQty()
  SHOW GETS WINDOW (lcWinCh32) ONLY
  SHOW GETS WINDOW (lcWinCh34) ONLY
  =lfRefresh(lcWinCh34)
ELSE
  SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,FabQty
ENDIF
*!*************************************************************
*! Name      : lfvSizeQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style size quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvSizeQty()
*!*************************************************************
FUNCTION lfvSizeQty
PARAMETERS lcQtyNo

IF m.Qty&lcQtyNo < 0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Buttfon  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Qty&lcQtyNo = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
=lfRefresh(lcWinCh34)
*-- End OF lfvSizeQty.

*!*************************************************************
*! Name      : lfChkTotQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
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

*-- Message : 40015
*-- Quantity out of balance! xxx pieces ok
*-- Button : 40000 
*-- Yes NO
IF m.FabQty <> m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8 AND ;
  gfModalGen('QRM40015B40000','ALERT',STR(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,5))=2
  RETURN(.F.)
ENDIF

IF Qty1 <> m.Qty1 OR Qty2 <> m.Qty2 OR Qty3 <> m.Qty3 OR Qty4 <> m.Qty4 OR ;
   Qty5 <> m.Qty5 OR Qty6 <> m.Qty6 OR Qty7 <> m.Qty7 OR Qty8 <> m.Qty8
  
  IF INLIST(laSetups[7,2],'F','L')
  *-- if the cost method is FIFO or LIFO then replace only with the On Hand Quantitys 
    FOR lnCount = 1 TO Scale.Cnt
      IF EVAL('m.Qty'+STR(lnCount,1)) > EVAL('StyDye.Stk'+STR(lnCount,1))
        *-- Message : 40137
        *-- Shipped Quantity For Size XXXXX Cannot Exceed On Hand
        *-- Quantity (99999).
        *-- Button : 00000 
        *-- OK
        =gfModalGen('TRM40137B00000','ALERT',ALLTRIM(EVAL('Scale.Sz'+STR(lnCount,1)))+'|('+ALLTRIM(STR(EVAL('StyDye.Stk'+STR(lnCount,1)),5))+')')
        STORE MAX(EVAL('StyDye.Stk'+STR(lnCount,1)),0) TO ('m.Qty'+STR(lnCount,1))
        RETURN(.F.)
      ENDIF
    ENDFOR
  ENDIF
  m.FabQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
  IF lcPriceLvl='Q'
    m.nSellPrice = lfGetprice(m.Style,lcPriceLvl,m.FabQty)
    m.nSellNetPr = m.nSellPrice*(100-m.Disc_Pcnt)/100
  ENDIF
  SELECT (lcInvLine)
  lnCartons  = lnCartons  - IIF(Style.Qty_Ctn>0,(FabQty-m.FabQty)/Style.Qty_Ctn,0)
  laData[25] = CEILING(lnCartons)
  laData[25] = IIF(laData[25]=0,1,laData[25])

  *-- Total Wieght 
  laData[26] = laData[26] - (FabQty-m.FabQty)*Style.nStyWeight
  *-- Shipped Amount
  *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
  *laData[31] = laData[31] - FabQty*nSellNetPr+m.FabQty*m.nSellNetPr
  laData[31] = laData[31] - CEILING(FabQty*nSellNetPr*100)/100+CEILING(m.FabQty*m.nSellNetPr*100)/100
  *E126684,1  TMI [End  ] 
  *-- Shippd Quantity
  laData[42] = laData[42] - FabQty+m.FabQty
  *-- Approval Amount
  laData[48] = laData[31]
  laData[33] = -ROUND(laData[31]*laData[32]/100,2)
  
  lnTaxDue   = lnTaxDue + IIF(lTaxable,m.FabQty*m.nSellNetPr-FabQty*nSellNetPr,0)
  SELECT (lcInvHdr)  
  =RLOCK()
  REPLACE CARTONS    WITH laData[25] ,;
          WEIGHT     WITH laData[26] ,;
          FabSHIPAMT WITH laData[31] ,;
          FabSHIP    WITH laData[42] ,;
          Discount WITH laData[33] ,;
          APPRAMT  WITH laData[48] ,;
          nTaxDue  WITH lnTaxDue   ,;
          lCompUps WITH .T.
  UNLOCK
  SELECT (lcInvLine)
  IF laSetups[9,2]='Y' .AND. llIsEngland
    FOR lnCount = 1 TO Scale.Cnt
      IF BETWEEN(lnCount,Style.nTaxBreak,8)
        lcCount = STR(lnCount,1)
        lnMerchTax = lnMerchTax + (m.Qty&lcCount*m.nSellNetPr-Qty&lcCount*nSellNetPr)*nTaxRate/100
      ENDIF
    ENDFOR  
  ENDIF
  =RLOCK()
  GATHER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,FabQty,;
                       nSellPrice,nSellNetPr,PPQty
  UNLOCK
  STORE .T. TO llReMerTax,llCUpdate
ENDIF
IF FabQty = 0
  *-- Message : 40017
  *-- Total Pieces 0! This line will be deleted
  *-- Button : 00000 
  *-- OK
  =gfModalGen('INM40017B00000','ALERT')
  BLANK
  DELETE
  IF !llNewLine
    GO TOP
    lcDispDet = IIF(EOF(),'DISABLE','')
    SCATTER MEMVAR
    SHOW GETS WINDOW (lcWinCh32) &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh34) &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh35) &lcDispDet ONLY
    =lfRefresh(lcWinCh32)
    =lfRefresh(lcWinCh34)
    IF EOF()
      SHOW GET pbNew ENABLE 
      SHOW GET pbPacks ENABLE
    ENDIF
  ENDIF
  ACTIVATE WINDOW (lcInvBrow)
  SHOW WINDOW (lcInvBrow) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvStyDesc
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style description
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
IF Desc1 <> m.Desc1
  llCUpdate = .T.
  =RLOCK()
  REPLACE Desc1 WITH m.Desc1
  UNLOCK
ENDIF

*!*************************************************************
*! Name      : lfvComm
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

lcSalesRep = STR(lnSalesRep,1)
IF Comm&lcSalesRep <> m.Comm&lcSalesRep
  llCUpdate = .T.
  =RLOCK()
  REPLACE Comm&lcSalesRep WITH m.Comm&lcSalesRep
  UNLOCK
ENDIF
*!*************************************************************
*! Name      : lfwPrice
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : When function for Style price
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfwPrice()
*!*************************************************************
FUNCTION lfwPrice

IF !(laSetups[19,2] $ 'DB')
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvGPrice
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style Gross price
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

IF m.nSellPrice < 0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.nSellPrice = lcOldValue
  _CUROBJ      = _CUROBJ
  RETURN
ENDIF
*E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
*m.nSellNetPr = ROUND(m.nSellPrice*(100-m.Disc_Pcnt)/100,2)
m.nSellNetPr = CEILING( 100000 * (m.nSellPrice*(100-m.Disc_Pcnt)/100) ) / 100000
*E126684,1  TMI [End  ] 
=lfvNPrice()

*!*************************************************************
*! Name      : lfvPrcDisc
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style price discount
*!*************************************************************
*! Example   :  =lfvPrcDisc()
*!*************************************************************
FUNCTION lfvPrcDisc

*E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
*m.nSellNetPr = ROUND(m.nSellPrice*(100-m.Disc_Pcnt)/100,2)
m.nSellNetPr = CEILING( 100000 * (m.nSellPrice*(100-m.Disc_Pcnt)/100) ) / 100000
*E126684,1  TMI [End  ] 
=lfvNPrice(.T.)

*!*************************************************************
*! Name      : lfvNPrice
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style Net price
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
PARAMETERS llPcnt

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

IF m.nSellNetPr <> nSellNetPr
  *--recalculate merchandise tax
  STORE .T. TO llReMerTax,llCUpdate
  SELECT (lcInvLine)
  *-- Shipped
  *E126684,1  TMI [Start] Use CEILING function to round amount up to 2 decimals
  *laData[31] = laData[31] + FabQty*(m.nSellNetPr - nSellNetPr)
  laData[31] = laData[31] + CEILING(FabQty*m.nSellNetPr*100)/100 - CEILING(FabQty*nSellNetPr*100)/100
  *E126684,1  TMI [End  ] 
  *-- approval amount
  laData[48] = laData[31]
  
  *-- Merchendise discount
  laData[33] = -ROUND(laData[31]*laData[32]/100,2)
  lnTaxDue   = lnTaxDue + IIF(lTaxable,FabQty*m.nSellNetPr-FabQty*nSellNetPr,0)
  SELECT (lcInvHdr)  
  =RLOCK()
  REPLACE FabSHIPAMT WITH laData[31] ,;
          APPRAMT  WITH laData[48] ,;
          Discount WITH laData[33] ,;
          nTaxDue  WITH lnTaxDue   ,;
          lCompUps WITH .T.
  UNLOCK
  SELECT (lcInvLine)  
  IF laSetups[9,2]='Y' .AND. llIsEngland .AND. Style.nTaxBreak <> 0
    lnQuantity = 0
    *-- Sum the quantity for only the sizes that is Greater than the tax break weight
    FOR lnCount = Style.nTaxBreak TO 8
      lnQuantity = lnQuantity+EVAL('m.Qty'+STR(lnCount,1))
    ENDFOR
    lnMerchTax = lnMerchTax + lnQuantity * (m.nSellNetPr-nSellNetPr) * nTaxRate/100
  ENDIF
  m.Disc_Pcnt  = IIF(llPcnt,m.Disc_Pcnt,IIF(m.nSellPrice=0,0,MAX(100-m.nSellNetPr*100/m.nSellPrice,0)))
  m.nSellPrice = IIF(m.Disc_Pcnt=0,m.nSellNetPr,m.nSellPrice)
  =RLOCK()
  REPLACE nSellNetPr WITH m.nSellNetPr ,;
          nSellPrice WITH m.nSellPrice ,;
          Disc_Pcnt  WITH m.Disc_Pcnt
  UNLOCK
  SHOW GET m.nSellPrice
  SHOW GET m.Disc_Pcnt
  SHOW GET m.nSellNetPr
  SHOW WINDOW (lcInvBrow) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvCopyQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
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
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laQtyPaste

*!*************************************************************
*! Name      : lfvPasteQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
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

IF laQtyPaste[1]+laQtyPaste[2]+laQtyPaste[3]+laQtyPaste[4]+;
   laQtyPaste[5]+laQtyPaste[6]+laQtyPaste[7]+laQtyPaste[8]=0
  *-- Message : 40016
  *-- No line quantities copied to paste!
  *-- Button : 00000 
  *-- ok
  =gfModalGen('INM40016B00000','ALERT')
  RETURN
ENDIF
SELECT(lcInvLine)
STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.FabQty
FOR lnCount = 1 TO Scale.Cnt
  lcCount = STR(lnCount,1)
  m.Qty&lcCount = laQtyPaste[lnCount]
  m.FabQty = m.FabQty + m.Qty&lcCount
ENDFOR
IF !lfChkTotQty()
  SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,FabQty
ENDIF
SHOW GETS WINDOW (lcWinCh32) ONLY
SHOW GETS WINDOW (lcWinCh34) ONLY
SHOW WINDOW (lcInvBrow) REFRESH SAME
_CUROBJ=OBJNUM(m.Qty1)

*!*************************************************************
*! Name      : lfvGstTax
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Compute GST Tax Percent and amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: Percent 
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvGstTax(.T.)
*!*************************************************************
FUNCTION lfvGstTax
PARAMETERS llPercent

IF lfNegChrg()
  IF !UPDATE()
    RETURN
  ENDIF  

  lnTaxAmnt = IIF(UPPER(ALLTRIM(gcContCode))='USA',lnTaxDue,laData[31])
  laData[37] = ROUND(IIF(llPercent,laData[46]*(IIF(laSetups[11,2]='M',lnTaxAmnt,;
                   lnTaxAmnt+laData[34]+laData[35]+laData[36])-;
                   lnTaxAmnt*laData[32]/100)/100,laData[37]),2)    
  laData[46] = IIF(!llPercent,laData[37]*100/;
                  (IIF(laSetups[11,2]='M',lnTaxAmnt,lnTaxAmnt+laData[34]+;
                  laData[35]+laData[36])-ABS(lnTaxAmnt*laData[32]/100)),laData[46])
  laData[43] = IIF(llIsCanada .AND. VAL(laData[44])=2,;
               (laData[31]+laData[34]+laData[35]+laData[36]+laData[37]-;
               laData[31]*laData[32]/100)*laData[45]/100,laData[43])
  IF llCod
    laData[28]= laData[28]-&lcInvHdr..Tax_Amt-&lcInvHdr..nPstAmt+laData[37]+laData[43]
    IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. laData[25]=1 ;
      .AND. SEEK(laData[3]+laData[5]+laData[30],lcUpsBox)
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH laData[28]
      UNLOCK
    ENDIF
  ENDIF
  llCUpdate = .T.
  laData[38] = laData[38]-&lcInvHdr..Tax_Amt-&lcInvHdr..nPstAmt+laData[37]+laData[43]
  laData[38] = laData[38] - &lcInvHdr..nHstAmt + laData[52]
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Cod_Amt  WITH laData[28] ,; 
          Tax_Rate WITH laData[46] ,;
          Tax_Amt  WITH laData[37] ,;
          nPstAmt  WITH laData[43] ,;
          TotalChg WITH laData[38]
  UNLOCK

  laData[46] = &lcInvHdr..Tax_Rate
  laData[37] = &lcInvHdr..Tax_Amt
  laData[43] = &lcInvHdr..nPstAmt
  laData[28] = &lcInvHdr..Cod_Amt
  laData[38] = &lcInvHdr..TotalChg

  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
ENDIF

*!*************************************************************
*! Name      : lfvPstTax
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Compute GST Pst Percent and amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: Percent 
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPstTax(.T.)
*!*************************************************************
FUNCTION lfvPstTax
PARAMETERS llPercent

IF lfNegChrg()
  IF !UPDATE()
    RETURN
  ENDIF
  laData[44] = STR(lnPstRule,2)
  laData[43] = IIF(llPercent,(laData[31]+laData[34]+laData[35]+laData[36]-;
               laData[31]*laData[32]/100+IIF(VAL(laData[44])=1,0,laData[37]));
               *laData[45]/100,laData[43])
  laData[45] = IIF(!llPercent,laData[43]*100/(laData[31]+laData[34]+;
               laData[35]+laData[36]-laData[31]*laData[32]/100+;
               IIF(VAL(laData[44])=1,0,laData[37])),laData[45])
               
  IF llCod
    laData[28]= laData[28]-&lcInvHdr..nPstAmt+laData[43]
    IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. laData[25]=1 ;
      .AND. SEEK(laData[3]+laData[5]+laData[30],lcUpsBox)
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH laData[28]
      UNLOCK
    ENDIF
  ENDIF
  llCUpdate = .T.
  laData[38] = laData[38] - &lcInvHdr..nPstAmt+laData[43]
  laData[52] = (laData[31]+laData[34]+laData[35]+laData[36]-;
                ABS(laData[31]*laData[32]/100)+;
                IIF(VAL(laData[44])=1,0,laData[37]))*laData[51]/100
  laData[38] = laData[31]+laData[34]+laData[35]+laData[36] - ;
               ROUND(ABS(laData[31]*laData[32]/100),2)+laData[37]+laData[43]+laData[52]
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Cod_Amt  WITH laData[28] ,; 
          cTaxRule WITH laData[44] ,; 
          nPstRate WITH laData[45] ,;
          nPstAmt  WITH laData[43] ,;
          TotalChg WITH laData[38]
  UNLOCK
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
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

IF laScrMode[3] OR laData[9] <> ALLTRIM(laTerms[lnTerms,2]) .OR. &lcInvHdr..SHIPDATE <> laData[6]
  laData[9] = ALLTRIM(laTerms[lnTerms,2])  && Get the new term code
  IF laScrMode[3]
    lnOTrdDis=laData[29]
  ENDIF
  =gfRltFld(laData[9],@laTRltFld,'CTERMCODE')
  IF laScrMode[3]
    laData[29]=lnOTrdDis
  ENDIF
  lcTEOM = ALLTRIM(lcTEOM)
  lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)
  lcTCod = ALLTRIM(lcTCod)
  llCod  = (lcTCod='Y')
  IF llIsEngland
    *-- get the new due date
    laData[7] = IIF(lcTEOM <> 'Y',laData[6]+lnTDaysDue,;
                    CTOD('01'+SUBSTR(DTOC(GOMONTH(laData[6],1)),3))-1+lnTDaysDue)
   ELSE 
     *-- EOM 	End of Month 		m/10/yy
     *-- if  day of invoice > End of Month date 		
     *-- m/10/yy + 2 monthes+ net due date	
     *-- if day of invoice < End of Month date 		
     *-- m/10/yy + 1 monthes+ net due date                  
     laData[7] = IIF(lcTEOM <> 'Y',laData[6] + lnTDaysDue,;
                GOMONTH(CTOD(SUBSTR(DTOC(laData[6]),1,3) +'10'+;
                SUBSTR(DTOC(laData[6]),6,5)),IIF(DAY(laData[6])>lnEOMDay,2,1))+lnTDaysDue)
  ENDIF
  SHOW GET laData[7]
  IF !laScrMode[3]  
    SELECT (lcInvHdr)
    =RLOCK()
    REPLACE lCompUps WITH .T.
    UNLOCK
    STORE .T. TO llReMerTax
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

IF laData[10] <> ALLTRIM(laShipVia[lnShipVia,2])
  laData[10] = ALLTRIM(laShipVia[lnShipVia,2])
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE lCompUps WITH .T.
  UNLOCK
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

IF laData[11] <> ALLTRIM(laSpcInst[lnSpcInst,2])
  laData[11] = ALLTRIM(laSpcInst[lnSpcInst,2])
ENDIF

*!*************************************************************
*! Name      : lfvSeason
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid Invoice season code
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

IF laData[12] <> SUBSTR(laSeasons[lnSeason,2],1,6)
  laData[12] = SUBSTR(laSeasons[lnSeason,2],1,6)
ENDIF

*!*************************************************************
*! Name      : lfvDivision
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid order division code
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

IF laData[13] <> SUBSTR(laDivision[lnDivision,2],1,6)
  laData[13] = SUBSTR(laDivision[lnDivision,2],1,6)
  IF laSetups[14,2]='D'
   
    STORE IIF(SEEK(laData[17]+laData[13],'REP_DIV'),REP_DIV.Comm_Rate,Customer.Comm) ;
          TO laData[18],lnSouComm1
    STORE IIF(SEEK(laData[19]+laData[13],'REP_DIV'),REP_DIV.Comm_Rate,Customer.Comm2) ;
          TO laData[20],lnSouComm2

    *-- Recalculate invoice commission
    =lfvCommision()
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvDept
*! Developer : WAM
*! Date      : 07/01/1996
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
FUNCTION lfvDept

IF !llBrowse .AND. laData[14] = IIF(laScrMode[3],MInvHdr.Dept,&lcInvHdr..Dept)
  RETURN
ENDIF

=gfOpenFile(gcDataDir+'CustDept',gcDataDir+'CustDept','SH')

IF llBrowse .OR. (!EMPTY(laData[14]) .AND. !SEEK(laData[2]+laData[14],'CustDept'))
  IF !SEEK(laData[2],'CustDept')
    IF llBrowse
      *-- Message : 40018
      *-- No departments assigend to account xxx
      *-- Button : 00000
      *-- ok
      =gfModalGen('INM40018B00000','ALERT',laData[2])
      STORE SPACE(5) TO laData[14]
    ENDIF
    llBrowse = .F.
    RETURN
  ENDIF
  SELECT CustDept
  lcBrFields = [Dept:H="Department",cDeptDesc :H="Description",Rep1,Comm1,Rep2,Comm2,cTermCode]
  laData[14] = IIF(ARIABROW('laData[2]',"Departments",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Dept','laBrowArr'),;
                  CustDept.Dept,laData[14])
ENDIF
llBrowse = .F.
IF !laScrMode[3]
  IF laData[14] <> &lcInvHdr..Dept  
    IF SEEK(laData[2]+laData[14],'CustDept')
      *-- Message : 40019
      *-- Customer defaults will be overwritten by the department defaults.
      *-- Button : 00000
      *-- ok
      =gfModalGen('INM40019B00000','ALERT')
      STORE CustDept.Rep1  TO laData[17]   && sales rep at department level   
      STORE CustDept.Rep2  TO laData[19]
      =lfvRep1() .AND. lfvRep2()
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE Rep1  WITH CustDept.Rep1 ,;
              Rep2  WITH CustDept.Rep1 ,;
              cTermCode WITH CustDept.cTermCode
      UNLOCK
      =gfwCodePop(@laCodes,'CTERMCODE','T')
      STORE CustDept.Comm1 TO laData[18],lnSouComm1  && commission at division level
      STORE CustDept.Comm2 TO laData[20],lnSouComm2
      SHOW GET laData[17]
      SHOW GET laData[18]
      SHOW GET laData[19]
      SHOW GET laData[20]
      *-- recalculate terms
      =lfvTerms()
      *-- Recalculate commission 
      =lfvCommision()
    ENDIF
  ENDIF
ENDIF
=gfCloseFile('CustDept')

*!*************************************************************
*! Name      : lfvFactor
*! Developer : WAM
*! Date      : 07/01/1996
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
PRIVATE lnAlias
lnAlias = SELECT()
IF llBrowse OR (!EMPTY(laData[24]) .AND. !SEEK(laData[24],'SycFact'))
  lcBrFields = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  SELECT SycFact
  laData[24] = IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,;
                  gnBrFSRow2, gnBrFSCol2,'','','cFacCode','laBrowArr'),;
                  SycFact.cFacCode,SPACE(6))
ENDIF
=lfRefresh(lcWinChrg)
SELECT (lnAlias)
llBrowse = .F.

*!*************************************************************
*! Name      : lfvCurrency
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid function for currency fields
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcCurrVar : Variable holds currency
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCurrency()
*!*************************************************************
FUNCTION lfvCurrency
PRIVATE lcCurrency

IF llBrowse .OR.  laData[22] <> &lcInvHdr..cCurrCode
  =gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'cCurrCode','SH')

  lcCurrency = laData[22]
  IF !gfCurrBrow(@lcCurrency)
    lcCurrency = &lcInvHdr..cCurrCode
  ENDIF
  laData[22] = lcCurrency
  IF laData[22] <> &lcInvHdr..cCurrCode
    IF laData[22] = gcBaseCurr
      STORE 1 TO laData[40], laData[23]  &&,currency unit ,currency rate
    ELSE
      laData[23] = gfChkRate('laData[40]',laData[22],laData[41],.T.,gcAct_Comp,.F.,.T.)
      IF laData[23] = 0
        *-- Message : 00262
        *-- A valid xxx to xxx exchange rate could not be found on xxx.
        *-- Button : 00000
        *-- Ok
        =gfModalGen('INM00262B00000','ALERT',ALLTRIM(laData[22])+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(laData[41]))
        laData[22] = &lcInvHdr..cCurrCode
        laData[23] = &lcInvHdr..nExRate
      ENDIF  
    ENDIF
  ENDIF  
  IF !llEditExRt OR laData[22]=gcBaseCurr
    SHOW GET laData[23] DISABLE
  ELSE
    SHOW GET laData[23] ENABLE
  ENDIF
  =gfCloseFile('SycCurr')
  DO gfAcOpCrdt IN (gcapphome+gcwinappl+'\ARMTINV.PRG') WITH ;
                   laData[2],laData[22],'llActCrdt'
  SELECT (lcInvHdr)  
ENDIF    
llBrowse = .F.

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

IF laData[23] <=0
  *-- Message : 40037
  *-- The exchange rate must be greater than zero
  *-- Button : 00000 
  *-- Ok
  =gfModalGen('INM40037B00000','ALERT')
  laData[23] = &lcInvHdr..nExRate
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

IF llBrowse .OR. (!EMPTY(laData[17]) AND !SEEK(laData[17],'SalesRep'))
  laData[17] = IIF(llBrowse,SPACE(3),laData[17])
  lcRep = laData[17]
  DO REPCHK WITH lcRep
  laData[17] = lcRep
ENDIF
llBrowse = .F.

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
IF llBrowse .OR. (!EMPTY(laData[19]) .AND. !SEEK(laData[19],'SalesRep'))
  laData[19] = IIF(llBrowse,SPACE(3),laData[19])
  lcRep = laData[19]
  DO REPCHK WITH lcRep
  laData[19] = lcRep
ENDIF  
llBrowse = .F.

*!*************************************************************
*! Name      : lfUpdate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Update Invoice header other information
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
  IF laScrMode[4]
    SELECT (lcInvHdr)
    =RLOCK()
    GATHER FROM laData FIELDS &lcScFields
    UNLOCK
  ENDIF
  =lfRefresh(lcWinCh2)
  llCUpdate = .T.
ENDIF
*!*************************************************************
*! Name      : lfvWareCode
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice warehouse code
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
laData[21] = laWareHouses[lnWareHouse,2]

*!*************************************************************
*! Name      : lfvTrdeDisc
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice trade discount
*!*************************************************************
*! Calls     : lfMechTax,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvTrdeDisc()
*!*************************************************************
FUNCTION lfvTrdeDisc

SELECT (lcInvHdr)
=RLOCK()
REPLACE Trde_Disc WITH laData[29]
UNLOCK
*-- Compute England merchandise tax
=IIF(llIsEngland,lfMechTax(),.T.)
SHOW GETS WINDOW (lcWinChrg) ONLY
=lfRefresh(lcWinChrg)

*!*************************************************************
*! Name      : lfvDiscount
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate order discount
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

IF !UPDATE()
  RETURN
ENDIF

laData[33] = IIF(llPercent,-1*laData[31]*laData[32]/100,-1*ABS(laData[33]))
laData[33] = ROUND(laData[33],2)

laData[32] = IIF(!llPercent,100*ABS(laData[33])/laData[31],laData[32])
IF laSetups[9,2] = 'Y' 
  lnTaxAmnt = IIF(UPPER(ALLTRIM(gcContCode))='USA',lnTaxDue,laData[31])
  laData[37] = ROUND(laData[46]*(IIF(laSetups[11,2]='M',lnTaxAmnt,;
               lnTaxAmnt+laData[34]+laData[35]+laData[36])-;
               lnTaxAmnt*laData[32]/100 )/100,2)
  IF llIsCanada
    laData[43] = (laData[31]+laData[34]+laData[35]+laData[36]-;
                  ABS(laData[31]*laData[32]/100)+;
                  IIF(VAL(laData[44])=1,0,laData[37]))*laData[45]/100
    laData[52] = (laData[31]+laData[34]+laData[35]+laData[36]-;
                  ABS(laData[31]*laData[32]/100)+;
                  IIF(VAL(laData[44])=1,0,laData[37]))*laData[51]/100
  ENDIF
ENDIF
*-- Balance to pay
laData[38] = laData[31]+laData[34]+laData[35]+laData[36]+laData[33]+;
             laData[37]+laData[43]
laData[38] = laData[38] + laData[52]
IF llCod
*-- Cash on delivery amount
  IF EMPTY(lcUpsType) OR lcUPSType='OTHER'
    laData[28] = laData[31]+laData[33]+laData[37]+laData[43]
  ELSE
    laData[28] = laData[31]+laData[34]+laData[35]+laData[36]+laData[33]+;
               laData[37]+laData[43]
  ENDIF
ENDIF
llCUpdate = .T.
SELECT (lcInvHdr)
=RLOCK()
REPLACE DiscPcnt WITH laData[32] ,;
        Discount WITH laData[33] ,;
        Tax_Amt  WITH laData[37] ,;
        nPstAmt  WITH laData[43] ,;
        Cod_Amt  WITH laData[28] ,;
        TotalChg WITH laData[38]
REPLACE nHstAmt  WITH laData[52]
UNLOCK
lnOldComm1 = laData[18]
lnOldComm2 = laData[20]
=lfvCommision()
*-- Message : 40038
*-- Would you like to overwrite invoice lines commissions with new commissions?
*-- Button : 40000
*-- Yes No
IF (lnOldComm1<>laData[18] .OR. lnOldComm2<>laData[20]) .AND. ; 
   SEEK(laData[2]+laData[3]+laData[5],lcInvLine) .AND. ;
   (laSetups[2,2] <> 'Y' .OR. gfModalGen('QRM40038B40000','ALERT')=1 )
  SELECT (lcInvLine) 
  SCAN
    =RLOCK()
    REPLACE Comm1 WITH laData[18], Comm2 WITH laData[20]
    UNLOCK
  ENDSCAN
  GO TOP 
  SELECT (lcInvHdr)
ENDIF
SHOW GETS WINDOW (lcWinChrg) ONLY
=lfRefresh(lcWinChrg)

*!*************************************************************
*! Name      : lfvCommision
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Recalculate order commissions
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

IF SEEK(laData[17],'SalesRep')
  *-- get the discount percent from the sales rep file
  DO CASE
    CASE laData[32] >= SalesRep.Discnt4
      laData[18] = lnSouComm1 * (1-SalesRep.Redcom4/100)
    CASE laData[32] >= SalesRep.Discnt3
      laData[18] = lnSouComm1 * (1-SalesRep.Redcom3/100)
    CASE laData[32] >= SalesRep.Discnt2
      laData[18] = lnSouComm1 * (1-SalesRep.Redcom2/100)
    CASE laData[32] >= SalesRep.Discnt1
      laData[18] = lnSouComm1 * (1-SalesRep.Redcom1/100)
  ENDCASE
  IF laData[18] <> &lcInvHdr..Comm1
    *-- Message : 40023
    *-- xxx commission decreased to xxx%
    *-- Button : 00000
    *-- Ok
    =gfModalGen('INM40023B00000','ALERT',laData[17]+'|'+STR(laData[18],5,2))
  ENDIF
ENDIF
IF SEEK(laData[19],'SalesRep')
  DO CASE
    CASE laData[32] >= SalesRep.Discnt4
      laData[20] = lnSouComm2 * (1-SalesRep.Redcom4/100)
    CASE laData[32] >= SalesRep.Discnt3
      laData[20] = lnSouComm2 * (1-SalesRep.Redcom3/100)
    CASE laData[32] >= SalesRep.Discnt2
      laData[20] = lnSouComm2 * (1-SalesRep.Redcom2/100)
    CASE laData[32] >= SalesRep.Discnt1
      laData[20] = lnSouComm2 * (1-SalesRep.Redcom1/100)
  ENDCASE
  IF laData[20] <> &lcInvHdr..Comm2
    *-- Message : 40023
    *-- xxx commission decreased to xxx%
    *-- Button : 00000
    *-- Ok
    =gfModalGen('INM40023B00000','ALERT',laData[19]+'|'+STR(laData[20],5,2))
  ENDIF
ENDIF
SELECT (lcInvHdr)
=RLOCK()
REPLACE Comm1 WITH laData[18] ,;
        Comm2 WITH laData[20]
UNLOCK
SHOW GET laData[18]
SHOW GET laData[20]

*!*************************************************************
*! Name      : lfvInvoice
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Invoice number
*!*************************************************************
*! Calls     : lfInvBrow,gfSeekRec
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvInvoice()
*!*************************************************************
FUNCTION lfvInvoice
PRIVATE lcInvoice

IF llBrowse .OR. (!EMPTY(laData[1]) .AND. !SEEK(laData[1],'MINVHDR'))
  lcInvoice = laData[1]
  =lfInvBrow(@lcInvoice,laData[2],laData[3],laData[4],laData[5])
  laData[1] = lcInvoice
  llBrowse = .F.
ENDIF
IF !EMPTY(laData[1])
 *-- Seek for a record with the main key fields values in the base file of the
 *-- Program 
  =gfSeekRec()
ENDIF  

*!*************************************************************
*! Name      : lfInvBrow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Browse Invoices
*!*************************************************************
*! Calls     : AriaBrow,gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfInvBrow()
*!*************************************************************
FUNCTION lfInvBrow
PARAMETERS lcInvoice,lcAccount,lcInvOrd,lcCustPo,lcStore
PRIVATE lnAlias,lnCurrTag

GO TOP IN 'MINVHDR'
IF EOF('MINVHDR')
  *-- Message : 40036
  *-- The invoice file is empty.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('INM40036B00000','ALERT')
  STORE SPACE(6) TO lcInvoice
  RETURN
ENDIF  
lnAlias = SELECT()
SELECT MINVHDR
GO TOP
lnCurrTag = TAG()
SET ORDER TO TAG MINVHDR
IF !EMPTY(lcAccount)
  SET ORDER TO TAG MINVHDRA
  =SEEK(lcAccount)
ENDIF
LOCATE REST WHILE Account=IIF(EMPTY(lcAccount),'',lcAccount) ;
             FOR  CMORDER =IIF(EMPTY(lcInvOrd),"",lcInvOrd) .AND. ;
                  CUSTPO =IIF(EMPTY(lcCustpo),"",lcCustpo) .AND. ;
                  STORE  =IIF(EMPTY(lcStore),"",lcStore)
IF !FOUND()
  *-- Message : 40084
  *-- No invoices found
  *-- Button : 00000 
  *-- Ok
  =gfModalGen('INM40084B00000','ALERT',IIF(!EMPTY(lcAccount),IIF(EMPTY(lcStore),'for account: '+lcAccount,'for account/store: '+lcAccount+'/'+lcStore),''))
  STORE SPACE(6) TO lcInvoice
  SET ORDER TO TAG lnCurrTag
  SELECT (lnAlias)
  RETURN
ENDIF
GO TOP
lcInvoice = ;
IIF(AriaBrow(IIF(!EMPTY(lcAccount),[lcAccount],'')+;
             ' FOR (CMORDER=IIF(EMPTY(lcInvOrd),"",lcInvOrd)) .AND. ;
                   (CUSTPO=IIF(EMPTY(lcCustpo),"",lcCustpo)) .AND. ;
                   (STORE=IIF(EMPTY(lcStore),"",lcStore))',;
lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','cMatInv','laBrowArr'),MInvHdr.cMatInv,SPACE(6)) 
SET ORDER TO TAG lnCurrTag
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate Invoice order
*!*************************************************************
*! Calls     : MAORDBROWO,MAORDBROWA
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvOrder()
*!*************************************************************
FUNCTION lfvOrder
PRIVATE XOrder,XAccount,XSTORE

*-- get the ivoice of that order
IF llBrowse .OR. (!EMPTY(laData[3]) .AND. !SEEK('O'+laData[3],'MaSoHdr'))
  XAccount = laData[2]
  XORDER   = laData[3]
  XSTORE   = laData[5]
  llBrowse = .T.
  laData[3] = IIF(IIF(EMPTY(XAccount),MAORDBROWO(@XORDER, .T., 'O'),MAORDBROWA(XAccount,.T.,'O')),XORDER,'')
ENDIF
llBrowse = .F.
IF !EMPTY(laData[3])
  laData[2] = MaSoHdr.Account
  llBrowse = .T.
  _CUROBJ = OBJNUM(laData[1])
  KEYBOARD "{ENTER}"
ENDIF
  
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

IF !(llBrowse .OR. (!EMPTY(laData[2]) AND LASTKEY()=13))
  RETURN
ENDIF
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
    STORE SPACE(8) TO laData[5]
    SHOW GET ibStore   DISABLE
    SHOW GET laData[5] DISABLE
  ELSE
    SHOW GET ibStore   ENABLE
    SHOW GET laData[5] ENABLE
  ENDIF
ENDIF
IF laScrMode[4] .AND. !EMPTY(laData[2])
  IF Customer.Status <> 'A'
    *-- Message : 40022
    *-- This a non-active account. Invoice is not allowed!
    *-- Button : 00000
    *-- Ok
    IF Customer.Status <> 'P'
      *-- Message : 40022
      *-- This a ð account. Invoice is not allowed!
      *-- Button : 00000
      *-- Ok
      lcMessage = IIF(Customer.Status = 'H','Hold','Canceled') + ' account'
      =gfModalGen('INM40022B00000','ALERT',lcMessage)
      _CUROBJ = _CUROBJ
      RETURN
    ELSE
      *-- Message : 40173
      *-- Tetxt   : The ð you have selected is Potential.
      *-- Tetxt   : Would you like to continue and make it Active ?
      *-- Button  : 40000
      *--         : Yes ; No
      IF gfModalGen("QRM40173B40000","DIALOG",'account')=2
        STORE &lcInvHdr..Account TO laData[2]
        _CUROBJ = _CUROBJ
        RETURN
      ELSE
        lnAlias = SELECT(0)
        IF SEEK('M'+laData[2],'Customer') .AND. Customer.Status = 'P' 
          SELECT Customer
          IF gfObj_Lock(.T.)
            REPLACE Customer.Status WITH "A"
            =gfObj_Lock(.F.)
          ELSE
            STORE &lcInvHdr..Account TO laData[2]
            _CUROBJ = _CUROBJ
            RETURN
          ENDIF
        ENDIF  
        SELECT (lnAlias)
      ENDIF
    ENDIF
  ENDIF  
  lcPriceLvl = IIF(!EMPTY(Customer.PriceLvl),Customer.PriceLvl,'A')
  *-- Customer Bill To name
  STORE Customer.BtName TO lcBillName,lcDBllName
  =gfGetAdr('Customer','','','',1,'2')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcDBllAdd&lcCount = lcDBllAdd&lcCount + IIF(EMPTY(lcDBllAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    lcBillAdd&lcCount = lcDBllAdd&lcCount
  ENDFOR  
  *-- Dba =Do business as 
  *-- get the shipname  
  STORE IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba) TO lcDShpName,lcShipName
  =gfGetAdr('CUSTOMER','','','',1,'')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcDShpAdd&lcCount = lcDShpAdd&lcCount + IIF(EMPTY(lcDShpAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    lcShipAdd&lcCount = lcDShpAdd&lcCount
  ENDFOR 
  llupsinsur = (Customer.cInsur='Y')
  *-- get the tax rate from the customer in case of canada
  STORE IIF(laSetups[9,2] <> 'Y',0,;
            IIF(llIsCanada,laSetups[10,2],Customer.nTaxRate)) TO laData[46]
  *-- PST Rate for canada only
  STORE IIF(laSetups[9,2]='Y' .AND. llIsCanada,Customer.nTaxRate,0) TO laData[45]
  STORE IIF(laSetups[9,2]='Y' .AND. llIsCanada,laSetups[21,2],0) TO laData[51]
  *-- tax Rule
  STORE IIF(laSetups[9,2]='Y' .AND. llIsCanada,Customer.cTaxRule,SPACE(2)) TO laData[44]
  STORE IIF(EMPTY(laData[44]),' 1',laData[44]) TO laData[44]
  STORE IIF(llIsCanada,VAL(laData[44]),1) TO lnPstRule
  *--factor code
  STORE Customer.cFacCode TO laData[24]  
  *-- the default codes
  STORE IIF(EMPTY(Customer.ShipVia),lfGetDefCode('SHIPVIA'),;
            Customer.ShipVia) TO laData[10]
  STORE IIF(EMPTY(Customer.cTermCode),lfGetDefCode('CTERMCODE'),;
            Customer.cTermCode)   TO laData[09]
  STORE IIF(EMPTY(Customer.SpcInst),lfGetDefCode('SPCINST'),;
            Customer.SpcInst) TO laData[11]
  STORE Customer.Disc      TO laData[32]
  STORE Customer.SalesRep  TO laData[17]
  STORE Customer.Rep2      TO laData[19]
  STORE Customer.Note      TO laData[15]
  STORE Customer.cCurrCode TO laData[22]
  STORE ldDefInvDate       TO laData[6],laData[41]
  STORE ldDefPstDate       TO laData[49]
  STORE lcIDefDiv          TO laData[13]
  STORE lcIDefSes          TO laData[12]
  STORE lcIDefWare         TO laData[21]
  =lfvRep1() .AND. lfvRep2()
  *-- sales rep commission on devisio or on customer
  STORE IIF(laSetups[14,2]='D' .AND. SEEK(laData[17]+laData[13],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm) TO laData[18],lnSouComm1
  STORE IIF(laSetups[14,2]='D' .AND. SEEK(laData[19]+laData[13],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm2) TO laData[20],lnSouComm2

  =gfRltFld(laData[9],@laTRltFld,'CTERMCODE')
  lcTEOM = ALLTRIM(lcTEOM)
  lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)
  lcTCod = ALLTRIM(lcTCod)
  llCod  = (lcTCod='Y')
  IF llIsEngland
  *-- due date 
    laData[7] = IIF(lcTEOM <> 'Y',laData[41]+lnTDaysDue,;
                    CTOD('01'+SUBSTR(DTOC(GOMONTH(laData[41],1)),3))-1+lnTDaysDue)
   ELSE  
   
     *-- EOM 	End of Month 		m/10/yy
     *-- if  day of invoice > End of Month date 		
     *-- m/10/yy + 2 monthes+ net due date	
     *-- if day of invoice < End of Month date 		
     *-- m/10/yy + 1 monthes+ net due date                 
     laData[7] = IIF(lcTEOM <> 'Y',laData[41] + lnTDaysDue,;
                 GOMONTH(CTOD(SUBSTR(DTOC(laData[41]),1,3) +'10'+;
                 SUBSTR(DTOC(laData[41]),6,5)),IIF(DAY(laData[41])>lnEOMDay,2,1))+lnTDaysDue)
  ENDIF
  *--  currency 
  laData[22] = IIF(EMPTY(laData[22]),gcBaseCurr,laData[22])
  IF laData[22] = gcBaseCurr
    STORE 1 TO laData[23], laData[40]
  ELSE
   *-- Exchange Rate
    laData[23] = gfChkRate('laData[40]', laData[22], laData[41] ,;
                              .T., .F., .F., llEditExRt)
    IF laData[23] = 0
      IF llEditExRt
        *-- Message : 00262
        *-- A valid xxx to xxx exchange rate could not be found on xxx.
        *-- Button : 00000
        *-- Ok
        =gfModalGen('INM00262B00000','ALERT',ALLTRIM(laData[22])+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(laData[41]))
      ELSE
        laData[22] = gcBaseCurr
        STORE 1 TO laData[23], laData[40]
      ENDIF
    ENDIF
  ENDIF
  DO gfAcOpCrdt IN (gcapphome+gcwinappl+'\ARMTINV.PRG') WITH ;
                   laData[2],laData[22],'llActCrdt'
  SELECT (lcInvHdr)
  APPEND BLANK
  =RLOCK() 
  GATHER FROM laData FIELDS &lcScFields
 
  REPLACE Status     WITH '' ,;
          DIRECT_INV WITH .T. ,;
          Cod_Flag   WITH IIF(llCod,'Y','N'),;
          Phone      WITH Customer.Phone1          
      
  UNLOCK
  SHOW GET ibAccount  DISABLE
  SHOW GET laData[2]  DISABLE
  SHOW GET laData[4]  ENABLE
  SHOW GET ibStore    ENABLE
  SHOW GET laData[5]  ENABLE
  =lfRefresh(lcWinCh2)
  =gfwCodePop(@laCodes,'CTERMCODE','T')
  =gfwCodePop(@laCodes,'SHIPVIA','T')
  =gfwCodePop(@laCodes,'SPCINST','T')
  =gfwCodePop(@laCodes,'SEASON','T')
  =gfwCodePop(@laCodes,'CDIVISION','T')

  =lfvCommision()
  SELECT 'UNCMSESS'
  IF SEEK('I') && if found repalce it 
    BLANK
  ELSE
    APPEND BLANK
  ENDIF
  REPLACE Status     WITH 'O'       ,;
          cUTranType WITH lcProgID  ,;
          cUserId    WITH gcUser_id ,;
          cSession   WITH lcSession ,;
          cProgram   WITH 'ARDINV'  ,;
          cCurrScr   WITH 'ARDINV'  ,;
          dTranDate  WITH gdSysDate ,;
          cTranTime  WITH TIME()
  =RLOCK()
  lcFiles = 'lcInvHdr,' +lcInvHdr +','+lcInvHdr+';'+;
            'lcInvLine,'+lcInvLine+',MInvLine;'+IIF(USED(lcupsbox),'lcUpsBox,'+lcUpsBox+','+ lcUpsBox+';','')
  =gfSavSess(lcProgID, lcFiles, @laVars,lcSession)
  llCUpDate = .T.
  =lfActFolder()
  
  IF (Customer.Age60 <> 0 .OR. Customer.Age90 <> 0;
    .OR. Customer.Age120 <> 0 ) .AND. laSetups[18,2]='Y'
    ?? CHR(7)
    ?? CHR(7)
    ?? CHR(7)
    DO (gcScrDir+gcWinAppl+"\ARAGING.SPX") WITH laData[8]
  ENDIF
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
PRIVATE llCustPo,lnAlias

IF laScrMode[4]
  IF &lcInvHdr..CustPo <> laData[4]
    IF !EMPTY(laData[4]))
      SET ORDER TO TAG OrdCust IN MaSoHdr
      llCustPo =SEEK(laData[2]+UPPER(laData[4])+'O','MaSoHdr')
      SET ORDER TO TAG MaSoHdr IN MaSoHdr
      *-- Message : 40021
      *-- Cust. PO# xxx has been entered before.
      *-- Button : 40004
      *-- Proceed;Reenter
      IF llCustPo .AND. gfModalGen('QRM40021B40004','ALERT',ALLTRIM(laData[4]))=2
        _CUROBJ = _CUROBJ
        RETURN
      ENDIF
    ENDIF
    lnAlias = SELECT()
    SELECT (lcInvHdr)
    =RLOCK()
    REPLACE CustPo WITH laData[4]
    UNLOCK
    SELECT (lnAlias)
  ENDIF
ENDIF

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
PRIVATE xStore


IF llBrowse .OR. (!EMPTY(laData[5]) .AND. ;
                  !SEEK('S'+laData[2]+laData[5],'CUSTOMER'))
  xStore = laData[5]
  SELECT CUSTOMER
  IF !CUSBROWS(laData[2],.T.)
    llBrowse = .F.
    IF laScrMode[1]
      STORE lcOldStore TO laData[5]
    ELSE
      SELECT (lcInvHdr)
      STORE Store TO laData[5]
    ENDIF
    RETURN
  ENDIF
  laData[5] = xStore
ENDIF  
llBrowse = .F.
IF !EMPTY(laData[5]) .AND. Customer.Status <> 'A' 
  *-- Message : 40022
  *-- This a non-active account. Invoice is not allowed!
  *-- Button : 00000
  *-- Ok
  *=gfModalGen('INM40022B00000','ALERT','store')
  IF Customer.Status <> 'P'
    *-- Message : 40022
    *-- This a ð account. Invoice is not allowed!
    *-- Button : 00000
    *-- Ok
    lcMessage = IIF(Customer.Status = 'H','Hold','Canceled') + ' store'
    =gfModalGen('INM40022B00000','ALERT',lcMessage)
    STORE &lcInvHdr..Store TO laData[5]
    RETURN
  ELSE
    *-- Message : 40173
    *-- Text    : You are about to save the invoice while the ð is Potential.
    *-- Text    : Continuing with change to Active ?
    *-- Button  : 40000
    *--         : Yes ; No
    IF gfModalGen("QRM40173B40000","DIALOG",'store')=2
      STORE &lcInvHdr..Store TO laData[5]
      _CUROBJ = _CUROBJ
      RETURN
    ELSE
      lnAlias = SELECT(0)
      IF SEEK('S'+laData[2]+laData[5],'Customer') .AND. Customer.Status = 'P' 
        SELECT Customer
        IF gfObj_Lock(.T.)
          REPLACE Customer.Status WITH "A"
          =gfObj_Lock(.F.)
        ELSE
          STORE &lcInvHdr..Store TO laData[5]
          _CUROBJ = _CUROBJ
          RETURN
        ENDIF
      ENDIF  
      SELECT (lnAlias)
    ENDIF
  ENDIF
ENDIF
IF laScrMode[1]
  IF !EMPTY(laData[5]) .OR. !EMPTY(laData[4])
    llBrowse = .T.
    _CUROBJ = OBJNUM(laData[1])
    KEYBOARD "{ENTER}"
  ENDIF  
  RETURN
ENDIF
IF laScrMode[4]  
  IF laData[5] <> &lcInvHdr..Store
    *-- Message : 40024
    *-- Overwrite invoice header with customer defaults?
    *-- Button : 40000
    *-- Yes;No
    IF EMPTY(laData[5]) .AND. ;
      gfModalGen('QRM40024B40000','ALERT','customer')=1
      =SEEK('M'+laData[2],'CUSTOMER')
      *-- Overwrite invoice header with customer defaults
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
    
      STORE Customer.SalesRep  TO laData[17]
      STORE Customer.Rep2      TO laData[19] 
      =lfvRep1() .AND. lfvRep2()
      *-- sales rep commission on customer or on devision
      STORE IIF(laSetups[14,2]='D' .AND. SEEK(laData[17]+laData[13],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm) TO laData[18],lnSouComm1
      STORE IIF(laSetups[14,2]='D' .AND. SEEK(laData[19]+laData[13],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm2) TO laData[20],lnSouComm2
      STORE IIF(laSetups[9,2] <> 'Y',0,;
                IIF(llIsCanada,laSetups[10,2],Customer.nTaxRate)) TO laData[46]
      STORE IIF(laSetups[9,2]='Y' .AND. llIsCanada,Customer.nTaxRate,0) TO laData[45]
      STORE IIF(laSetups[9,2]='Y' .AND. llIsCanada,Customer.cTaxRule,SPACE(2)) TO laData[44]
      STORE IIF(EMPTY(laData[44]),' 1',laData[44]) TO laData[44]
      STORE IIF(llIsCanada,VAL(laData[44]),1) TO lnPstRule
      STORE .T. TO llReMerTax
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE Rep1  WITH laData[17] ,;
              Comm1 WITH laData[18] ,;
              Rep2  WITH laData[19] ,;            
              Comm2 WITH laData[20] ,;
              lCompUps WITH .T.,;       
              Phone WITH Customer.Phone1
      UNLOCK
      lnShipAddr = 1
      =lfRefresh(lcWinCh2)
      SHOW GETS WINDOW (lcWinCh2) ONLY
      =lfvCommision()
    ENDIF  
  
    *-- Message : 40024
    *-- Overwrite invoice header with xxx defaults?
    *-- Button : 40000
    *-- Yes;No
    IF !EMPTY(laData[5]) .AND. ;
      gfModalGen('QRM40024B40000','ALERT','store')=1
      =SEEK('S'+laData[2]+laData[5],'CUSTOMER')
      STORE '' TO lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,lcBillName,;
                  lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,lcShipName
      lcBilLName = Customer.BtName
      =gfGetAdr('Customer','','','',1,'2')
      FOR lnCount = 1 TO ALEN(laAddress,1)
        lcCount = STR(laAddress[lnCount,1],1)
        lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
        SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
      ENDFOR  
    
      STORE Customer.SalesRep  TO laData[17]
      STORE Customer.Rep2      TO laData[19] 
      =lfvRep1() .AND. lfvRep2()
      STORE IIF(laSetups[14,2]='D' .AND. SEEK(laData[17]+laData[13],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm) TO laData[18],lnSouComm1
      STORE IIF(laSetups[14,2]='D' .AND. SEEK(laData[19]+laData[13],'REP_DIV'),;
            REP_DIV.Comm_Rate,Customer.Comm2) TO laData[20],lnSouComm2
      STORE IIF(laSetups[9,2] <> 'Y',0,;
                IIF(llIsCanada,laSetups[10,2],Customer.nTaxRate)) TO laData[46]
      STORE IIF(laSetups[9,2]='Y' .AND. llIsCanada,Customer.nTaxRate,0) TO laData[45]
      STORE IIF(laSetups[9,2]='Y' .AND. llIsCanada,Customer.cTaxRule,SPACE(2)) TO laData[44]
      STORE IIF(EMPTY(laData[44]),' 1',laData[44]) TO laData[44]
      STORE IIF(llIsCanada,VAL(laData[44]),1) TO lnPstRule
      STORE .T. TO llReMerTax
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE Rep1  WITH laData[17] ,;
              Comm1 WITH laData[18] ,;
              Rep2  WITH laData[19] ,;            
              Comm2 WITH laData[20] ,;
              lCompUps WITH .T.,;       
              Phone WITH Customer.Phone1
      UNLOCK
      lnShipAddr = 1
      =lfRefresh(lcWinCh2)
      SHOW GETS WINDOW (lcWinCh2) ONLY
      =lfvCommision()
    ENDIF
    IF !EMPTY(laData[5])
      PRIVATE lcDistCntr,lnCusRecNo
      STORE '' TO lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,lcShipName
      =SEEK('S'+laData[2]+laData[5],'CUSTOMER')
      lnCusRecNo = RECNO('Customer')    
      IF !EMPTY(Customer.Dist_Ctr)
        lcDistCntr = Customer.Dist_Ctr
        =SEEK('S'+laData[2]+lcDistCntr,'CUSTOMER')
      ENDIF
      lcShipName = IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
      =gfGetAdr('CUSTOMER','','','',1,'')
      FOR lnCount = 1 TO ALEN(laAddress,1)
        lcCount = STR(laAddress[lnCount,1],1)
        lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
        SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
      ENDFOR
      =lfRefresh(lcWinCh2)
      SHOW GETS WINDOW (lcWinCh2) ONLY
      IF RECCOUNT('Customer') >= lnCusRecNo
        GO lnCusRecNo IN Customer
      ENDIF
    ENDIF

    SELECT (lcInvHdr)
    =RLOCK()
    REPLACE Store WITH laData[5]
    UNLOCK
    PRIVATE lcOrder
    IF SEEK(laData[2]+laData[3]+lcOldStore,lcInvLine)
      WAIT 'Update invoice lines...' WINDOW NOWAIT
      SELECT (lcInvLine)
      lnCurrRec =RECNO(lcInvLine)
      lcOrder=ORDER()
      SET ORDER TO
      SCAN REST WHILE Account+CMORDER+Store+Piktkt = laData[2]
        =RLOCK()
        REPLACE Store WITH laData[5] 
        UNLOCK      
      ENDSCAN
      SET ORDER TO &lcOrder
      IF BETWEEN(lnCurrRec,1,RECCOUNT(lcInvLine))
        GOTO lnCurrRec
      ENDIF
      WAIT CLEAR 
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvShipAddr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate shipping address
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvShipAddr()
*!*************************************************************
FUNCTION lfvShipAddr

IF lnShipAddr = 1
  IF EMPTY(laData[5])
    lcShipName = lcDShpName
    lcShipAdd1 = lcDShpAdd1
    lcShipAdd2 = lcDShpAdd2
    lcShipAdd3 = lcDShpAdd3
    lcShipAdd4 = lcDShpAdd4
    lcShipAdd5 = lcDShpAdd5
  ELSE
    =SEEK('S'+laData[2]+laData[5],'Customer')
    STORE '' TO lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5
    lcShipName = IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
    =gfGetAdr('CUSTOMER','','','',1,'')
    FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount = STR(laAddress[lnCount,1],1)
      lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    ENDFOR  
  ENDIF
ENDIF
lcStatus = IIF(lnShipAddr=1,'DISABLE','ENABLE')
SHOW GET lcShipName &lcStatus
SHOW GET lcShipAdd1 &lcStatus
SHOW GET lcShipAdd2 &lcStatus
SHOW GET lcShipAdd3 &lcStatus
SHOW GET lcShipAdd4 &lcStatus
SHOW GET lcShipAdd5 &lcStatus
=gfSavSess(lcProgID, lcFiles, @laVars,lcSession)

*!*************************************************************
*! Name      : lfvAShpAdr
*! Developer : Wael Aly Mohamed
*! Date      : 01/17/1999
*! Purpose   : Store Alternative shipping address
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvAShpAdr()
*!*************************************************************
FUNCTION lfvAShpAdr

SELECT (lcInvHdr)
=RLOCK()
REPLACE StName    WITH lcShipName ,;
        cAddress1 WITH lcShipAdd1 ,;
        cAddress2 WITH lcShipAdd2 ,;
        cAddress3 WITH lcShipAdd3 ,;
        cAddress4 WITH lcShipAdd4 ,;
        cAddress5 WITH lcShipAdd5
UNLOCK

*!*************************************************************
*! Name      : lfvCharges
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : England Charges Screen
*!*************************************************************
*! Calls     : ARChrg.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvCharges()
*!*************************************************************
FUNCTION lfvCharges
PRIVATE lnAlias

lnAlias = SELECT()
IF laScrMode[4] AND !USED(lcEngChrg)
  =gfOpenFile(gcDataDir+'MInvChrg',gcDataDir+'MInvChrg','SH')
  SELECT MInvChrg
  =AFIELDS(laFileStru)
  =gfCloseFile('MInvChrg')
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'CMORDER'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'PikTkt'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 6
  laFileStru[lnFileStru+2,4] = 0
  *--  Create temp file for the english Charges
  =gfCrtTmp(lcEngChrg,@laFileStru,[CMORDER+cStore+PikTkt+cchrgcode],lcEngChrg)
ENDIF

DO gpCharges IN (gcapphome+gcwinappl+'\ARMTINV.PRG') WITH ;
IIF(laScrMode[4],lcEngChrg,'MInvChrg'),laData[1],laData[2],laData[3],laData[5],;
laData[30],laData[29],'laData[39]','lnChrgtax'
IF laScrMode[4]
  =lfMechTax()
ENDIF
=IIF(laScrMode[4],lfSavTmpFiles(),.T.)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvUpsBox
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Invoice Cartons Details (UPS Box)
*!*************************************************************
*! Calls     : ARUps.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvUpsBox()
*!*************************************************************
FUNCTION lfvUpsBox
PRIVATE lnAlias
lnAlias = SELECT()

DO gpUpsBox IN (gcapphome+gcwinappl+'\ARMTINV.PRG') WITH ;
IIF(laScrMode[4],lcUpsBox,'MUPSBOX'),laData[1],laData[2],laData[3],laData[5],;
laData[30],llupsinsur,llCod,laData[31],laData[10],laData[21],'laData[25]',;
'laData[26]','laData[34]','laData[35]','laData[36]','laData[28]'
IF laScrMode[4]
  SELECT (lcInvHdr)
  =RLOCK()
  GATHER FROM laData FIELDS &lcScFields
  UNLOCK
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvFreight
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Validate Freight,Cod and Insurance
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvFreight()
*!*************************************************************
FUNCTION lfvFreight

IF lfNegChrg()
  IF laSetups[9,2]='Y'
    lnTaxAmnt = IIF(UPPER(ALLTRIM(gcContCode))='USA',lnTaxDue,laData[31])
    laData[37] = ROUND(laData[46]*(IIF(laSetups[11,2]='M',lnTaxAmnt,lnTaxAmnt+laData[36]+;
               laData[35]+laData[34])-lnTaxAmnt*laData[32]/100)/100,2)         
    laData[43] = IIF(llIsCanada,(laData[31]+laData[36]+laData[35]+laData[34]-;
                     laData[31]*laData[32]/100+;
                IIF(VAL(laData[44])=1,0,laData[37]))*laData[45]/100,0)
    laData[52] = IIF(llIsCanada,(laData[31]+laData[36]+laData[35]+laData[34]-;
                     laData[31]*laData[32]/100+;
                 IIF(VAL(laData[44])=1,0,laData[37]))*laData[51]/100,0)
  ENDIF
  IF llCod
    *-- calculate Cash on delivery amount
    IF EMPTY(lcUPSType) OR lcUPSType='OTHER'
      laData[28] = laData[31]+laData[33]+laData[37]+laData[43]
    ELSE
      laData[28] = laData[31]+laData[34]+laData[35]+laData[36]-;
                   laData[31]*laData[32]/100+laData[37]+laData[43]
    ENDIF
    
    IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. laData[25]=1 ;
      .AND. SEEK(laData[3]+laData[5]+laData[30],lcUpsBox)
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH laData[28]
      UNLOCK
    ENDIF
  ENDIF

  laData[38] = laData[31]+laData[34]+laData[35]+laData[36]-;
               ROUND(laData[31]*laData[32]/100,2)+laData[37]+laData[43]
  laData[38] = laData[38] + laData[52]

  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Freight  WITH laData[34] ,;
          Insur    WITH laData[35] ,;
          Cod      WITH laData[36] ,;
          Cod_Amt  WITH laData[28] ,; 
          Tax_Amt  WITH laData[37] ,;
          nPstAmt  WITH laData[43] ,;
          TotalChg WITH laData[38]

  REPLACE nHstAmt   WITH laData[52]
  UNLOCK
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
  RETURN
ENDIF

*!*************************************************************
*! Name      : lfvCodAmnt
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Validate COD amount
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvCodAmnt()
*!*************************************************************
FUNCTION lfvCodAmnt
IF lfNegChrg()  
  IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. laData[25]=1 ;
    .AND. SEEK(laData[3]+laData[5]+laData[30],lcUpsBox)
    SELECT (lcUpsBox)
    =RLOCK()
    REPLACE &lcUpsBox..TotalCod WITH laData[28]
    UNLOCK
  ENDIF
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Cod_Amt  WITH laData[28]
  UNLOCK
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
  RETURN
ENDIF

*!*************************************************************
*! Name      : lfvUpsChrg
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : Caculate invoice charges
*!*************************************************************
*! Calls     : gpUpsChrg
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvUpsChrg()
*!*************************************************************
FUNCTION lfvUpsChrg

IF !laScrMode[4]
  RETURN
ENDIF
lcChrgType = IIF(SYS(18)='LLUPSINSUR','I',IIF(SYS(18)='LLCOD','C','A'))
DO gpUpsChrg IN (gcapphome+gcwinappl+'\ARMTINV.PRG') WITH ;
   laData[2],laData[5],laData[3],ladata[30],laData[21],laData[31],llCod,;
   llupsinsur,laData[32],laData[46],laData[44],laData[45],lcUpsBox,'laData[10]',;
   'laData[28]','laData[34]','laData[35]','laData[36]','laData[25]','laData[26]',;
   'laData[37]','laData[43]',IIF(UPPER(ALLTRIM(gcContCode))='USA',;
   lnTaxDue,laData[31]),lcChrgType

laData[52] = (laData[31]+laData[34]+laData[35]+laData[36]-;
              ABS(laData[31]*laData[32]/100)+;
              IIF(VAL(laData[44])=1,0,laData[37]))*laData[51]/100
laData[38] = laData[31]+laData[34]+laData[35]+laData[36] - ;
             ROUND(ABS(laData[31]*laData[32]/100),2)+laData[37]+laData[43]+laData[52]

*-- update the changes to the uncomplete session file
=gfSavSess(lcProgID, lcFiles, @laVars,lcSession)

*-- merchendise discount
laData[33] = -1 * ROUND(laData[31]*laData[32]/100,2)

SELECT (lcInvHdr)
laData[25] = IIF(laData[25]=0,1,laData[25])

=RLOCK()
REPLACE FabShip    WITH laData[42] ,;
        FabShipAmt WITH laData[31] ,;
        Discount WITH laData[33] ,;
        Freight  WITH laData[34] ,;
        Insur    WITH laData[35] ,;
        Cod      WITH laData[36] ,;
        Cod_Amt  WITH laData[28] ,; 
        Weight   WITH laData[26] ,;
        Tax_Amt  WITH laData[37] ,;
        nPstAmt  WITH laData[43] ,;
        Cartons  WITH laData[25] ,;
        TotalChg WITH laData[38] ,;
        Cod_Flag WITH IIF(llCod,'Y','N') ,;
        lKeyOff  WITH IIF(llCod,lKeyOff,.F.) ,;
        lCompUps WITH .F.
REPLACE nHstAmt  WITH laData[52]

UNLOCK
IF llCod
  SHOW GET laData[27] ENABLE
  SHOW GET laData[28] ENABLE
ELSE
  IF USED(lcAppCrdt) AND SEEK(laData[2],lcAppCrdt)
    SELECT (lcAppCrdt)
    DELETE ALL
  ENDIF
  STORE SPACE(6) TO laData[27]
  SHOW GET laData[27] DISABLE
  SHOW GET laData[28] DISABLE
ENDIF

IF llCod AND laData[38] > 0 AND llActCrdt
  llOpnCrdt = .T.
  SHOW GET pbCredits ENABLE
ELSE
  llOpnCrdt = .F.
  SHOW GET pbCredits DISABLE
ENDI

IF laSetups[15,2] <> 'Y' .OR. laData[25]=1 OR SUBSTR(lcUpsType,1,5) <> "USUPS"
  SHOW GET pbUpsBox DISABLE
ELSE
  SHOW GET pbUpsBox ENABLE
ENDIF
SHOW GETS WINDOW (lcWinChrg) ONLY
=lfRefresh(lcWinChrg)

*!*************************************************************
*! Name      : lpShow
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
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

IF laScrMode[1] AND lnLastMode = 4 AND lcIDefPrnt
  GO TOP IN (lcInvHdr)
  IF !EMPTY(&lcInvHdr..cMatInv) AND SEEK(&lcInvHdr..cMatInv,'MINVHDR')
    SELECT MINVHDR
    SCATTER FIELDS &lcScFields TO laData
    =gfCPPrint()
  ENDIF
ENDIF

laCtrStat[7] = 'DISABLE'
lcBrFields   = lcBaseFild
lcFile_Ttl   = lcBaseTit

IF TYPE('lcInvoiceNo') = 'C' .AND. EMPTY(laData[1]) AND SEEK(lcInvoiceNo,'MINVHDR')
  SELECT MINVHDR
  SCATTER FIELDS &lcScFields TO laData
ENDIF

*IF (laScrMode[2] AND lnLastMode <> 2) OR (laScrMode[4] AND lnLastMode <> 4)
*  SELECT STYLE
*  SET RELATION TO 'S'+SCALE INTO SCALE
*ENDIF

DO CASE
  CASE laScrMode[1] .AND. lnLastMode <> 1
  *-- check for uncomplete session
    IF llGoAndChk AND lfChkUnComS()
      RETURN
    ENDIF
    lnLastMode = 1
    *-- ReInitialize variables
    =lfReStart()
    SHOW GET pbInvNote DISABLE
    SHOW GET pbObjLink DISABLE
    SHOW GET pbSend    DISABLE
    
    *N000391,1 Disable the New push button on the toolbar. [Begin]
    SHOW GET pbcpNew DISABLE
    *N000391,1 Disable the New push button on the toolbar. [End]
  CASE laScrMode[2]
    llGoAndChk = .T.  && for the uncomplete session
    lnLastMode = 2
    lcScrMode = 'V'   && Void 
    SHOW GET pbInvNote ENABLE
    SHOW GET pbObjLink ENABLE
    SHOW GET pbSend    ENABLE

    IF MInvHdr.Status='V'
      SHOW GET pbDlt DISABLE
      SHOW GET pbEdt DISABLE
    ELSE    
      SHOW GET pbDlt ENABLE

      *N000391,1 Disable Edit push button on the toolbar. [Begin]
      SHOW GET pbEdt DISABLE
      *N000391,1 Disable Edit push button on the toolbar. [End]
    ENDIF
    
    *N000391,1 Disable New push button on the toolbar. [Begin]
    SHOW GET pbcpNew DISABLE
    *N000391,1 Disable New push button on the toolbar. [End]
    
    SELECT MInvLine
    SET RELATION TO Fabric+Color+laData[21]+DYELOT INTO FabDye
    SET RELATION TO Fabric + Color INTO Fabric ADDITIVE
    
    STORE 'MINVHDR' TO laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],;
                      laCodes[3,7],laCodes[3,8],laCodes[4,7],laCodes[4,8],;
                      laCodes[5,7],laCodes[5,8]
    STORE 'laData[1]' TO laCodes[1,9],laCodes[2,9],laCodes[3,9],laCodes[4,9],;
                         laCodes[5,9]
    *-- Restore invoice information
    =lfGetInfo()
  CASE laScrMode[4] .AND. lnLastMode <> 4
   
    lnLastMode = 4
    llGoAndChk = .T.
    IF EMPTY(ldDefInvDate)
    *-- Restore invoice defaults
      STORE '' TO lcIDefSes,lcIDefDiv
      STORE 0 TO lnSeason,lnDivision,lnWareHouse
      IF FILE("INVDEF.MEM")
        RESTORE ADDITIVE FROM ("INVDEF.MEM")
        STORE 1 TO lnSeason,lnDivision
        =gfwCodePop(@laCodes,'CDIVISION','L')
        =gfwCodePop(@laCodes,'SEASON','L')
        lnSeason    = ASCAN(laSeasons,lcIDefSes)
        lnDivision  = ASCAN(laDivision,lcIDefDiv)
        lnWareHouse = ASCAN(laWareHouses,lcIDefWare)
        lnSeason    = IIF(lnSeason=0,1,ASUBSCRIPT(laSeasons,lnSeason,1))
        lnDivision  = IIF(lnDivision=0,1,ASUBSCRIPT(laDivision,lnDivision,1))
        lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
      ENDIF
      lnWareHouse = IIF(llPOSSetup,lnPOSWeHs,lnWareHouse)
      IF lnSeason = 0
        lnSeason = 1
        =gfwCodePop(@laCodes,'SEASON','D')
      ENDIF
      IF lnDivision = 0
        lnDivision = 1
        =gfwCodePop(@laCodes,'CDIVISION','D')
      ENDIF
      lnWareHouse = MAX(lnWareHouse,1)
      lnWareHouse = IIF(llPOSSetup,lnPOSWeHs,lnWareHouse)

      STORE gdSysDate TO ldDefInvDate,ldDefPstDate
      llSessDef = .T.
      *-- the session default screen
      DO (gcScrDir+gcWinAppl+"\ARInvDef.SPX")
      IF !llSessDef
        STORE {}  TO ldDefInvDate,ldDefPstDate
        STORE .F. TO laScrMode
        STORE .T. TO laScrMode[1]
        SHOW GETS
        RETURN
      ENDIF
      SHOW GET lnWareHouse DISABLE
    ENDIF
    SELECT (lcInvLine)
    SET RELATION TO Fabric+Color+laData[21]+DYELOT INTO FabDye
    SET RELATION TO Fabric+Color INTO Fabric ADDITIVE
    
    STORE lcInvHdr TO laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],;
                      laCodes[3,7],laCodes[3,8],laCodes[4,7],laCodes[4,8],;
                      laCodes[5,7],laCodes[5,8]
    STORE 'laData[2]+laData[3]+laData[5]+laData[30]' TO ;
          laCodes[1,9],laCodes[2,9],laCodes[3,9],laCodes[4,9],laCodes[5,9]
    IF llContinue
      RETURN
    ENDIF
    =lfReStart()
    lcScrMode = 'N'
    =lfRefresh(lcWinCh2)    
    SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh34) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh35) DISABLE ONLY
    SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY
    SHOW GET pbInvNote DISABLE
    SHOW GET pbObjLink DISABLE
    SHOW GET pbSend    DISABLE
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    SHOW GET ibAccount ENABLE
    SHOW GET laData[2] ENABLE
    _CUROBJ = OBJNUM(laData[2])    
  CASE  laScrMode[3].AND. lnLastMode <> 3
    lnLastMode = 3
    SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh34) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh35) DISABLE ONLY     
    SHOW GETS WINDOW (lcWinCh30) DISABLE ONLY
    SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh2)  DISABLE ONLY
    SHOW GET laData[5] DISABLE
    SHOW GET ibStore DISABLE
    DO CASE
      CASE lnActFolder = 1
        =lfEdtFld(lcEdtFld)  
         SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY
      CASE lnActFolder = 2
        SHOW GET pbLineNote ENABLE

        *N000391,1 Enable Rolls button. [Begin]
        IF laScrMode[2]
          IF mInvHdr.Status = "V"
            SHOW GET pbRolls DISABLE
          ELSE
            SHOW GET pbRolls ENABLE
          ENDIF  
        ELSE
          SHOW GET pbRolls ENABLE
        ENDIF  
        *N000391,1 Enable Rolls button. [End]
        
      CASE lnActFolder = 3
       IF !llIsEngland .AND. laData[25] > 1 AND lcUpsType = "USUPS"
         SHOW GET pbUpsBox ENABLE
       ELSE
         SHOW GET pbUpsBox DISABLE
       ENDIF
       IF llIsEngland
         SHOW GET pbCharges ENABLE
       ENDIF
       IF laData[38] > 0 AND llInstTerm
         SHOW GET pbInvInst ENABLE
       ELSE
         SHOW GET pbInvInst DISABLE
       ENDIF
       =lfEdtFld(lcEdtFld)
       SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY 
    ENDCASE
ENDCASE  
=IIF(laScrMode[4],SEEK('O'+PADR(lcProgID,10)+gcUser_id+lcSession,'UNCMSESS'),.T.)

*!*************************************************************
*! Name      : lfMechTax
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : Compute England merchandise tax
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfMechTax()
*!*************************************************************
FUNCTION lfMechTax

=SEEK(IIF(EMPTY(laData[5]),'M'+laData[2],'S'+laData[2]+laData[5]),'CUSTOMER')
lnAlias = SELECT()
lnChrgtax  = 0
IF USED(lcEngChrg)
  SELECT (lcEngChrg)
  =SEEK(laData[3]+laData[5]+laData[30])
  SUM REST nChrgAmnt*(1-laData[29]/100)*nTaxRate/100 TO lnChrgtax  ;
  WHILE CMORDER+Store+PikTkt+cchrgcode = laData[3]+laData[5]+laData[30]
ENDIF

laData[33] = -1 * ROUND(laData[32]*laData[31]/100,2)
laData[37] = ROUND(lnChrgtax + IIF(Customer.lVatExem,0,;
             lnMerchTax * (100-laData[29])/100 * (100-laData[32])/100),2)            
laData[28] = IIF(llCod,laData[31]+laData[39]+laData[37]+laData[33],0)
laData[38] = laData[31]+laData[39]+laData[37]+laData[33]
SELECT (lcInvHdr)
laData[25] = IIF(laData[25]=0,1,laData[25])
=RLOCK()
REPLACE FabShip   WITH laData[42] ,;
        FabShipAmt WITH laData[31] ,;
        DiscPcnt  WITH laData[32] ,;
        Discount  WITH laData[33] ,;
        Trde_Disc WITH laData[29] ,;
        Cod_Amt   WITH laData[28] ,; 
        Weight    WITH laData[26] ,;
        Tax_Amt   WITH laData[37] ,;
        Cartons   WITH laData[25] ,;
        TotalChg  WITH laData[38] ,;
        nCharges  WITH laData[39] ,;
        Cod_Flag  WITH IIF(llCod,'Y','N') ,;
        lKeyOff  WITH IIF(llCod,lKeyOff,.F.)
UNLOCK
STORE .T. TO llCUpdate
STORE .F. TO llReMerTax
SELECT (lnAlias)
=lfActFolder()

*!*************************************************************
*! Name      : lfReStart
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
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

=gfwCodePop(@laCodes,'CTERMCODE','N')
=gfwCodePop(@laCodes,'SHIPVIA','N')
=gfwCodePop(@laCodes,'SPCINST','N')
=gfwCodePop(@laCodes,'SEASON','N')
=gfwCodePop(@laCodes,'CDIVISION','N')
STORE ''  TO lcInvStat,lcFiles,lcScrMode,lcCurrInv,lcUpsType
STORE 0 TO lnCartons,lnLines ,lnSouComm1,lnSouComm2,laQtyPaste,lnTaxDue
STORE 1 TO lnShipAddr,lnPstRule
STORE lcEmptySty TO lcLastStyle
STORE '' TO  lcDShpName,lcDShpAdd1,lcDShpAdd2,lcDShpAdd3,lcDShpAdd4,lcDShpAdd5,;
             lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
             lcDBllName,lcDBllAdd1,lcDBllAdd2,lcDBllAdd3,lcDBllAdd4,lcDBllAdd5
STORE .F. TO llZoom,llupsinsur,llCod
STORE .T. TO llReBrowse,llAddLine,llNewLine
lcStatColor = gcObjColor
lcCrdtColor = gcObjColor
lcUpsTrack = 'COD Tracking#'
lcSize1   = 'Units'
IF lnactfolder <> 1
  lnlastfold = lnactfolder
  lnactfolder=1
  =lfchngfolder(lnactfolder)
ENDIF
IF USED(lcAppCrdt)
  SELECT (lcAppCrdt)
  DELETE ALL
ENDIF
IF USED(lcInstHdr)
  SELECT (lcInstHdr)
  DELETE ALL
ENDIF
IF USED(lcInstLin)
  SELECT (lcInstLin)
  DELETE ALL
ENDIF
IF USED(lcEngChrg)
  SELECT (lcEngChrg)
  DELETE ALL
ENDIF

SELECT MINVHDR
SCATTER FIELDS &lcScFields TO laData BLANK
lnMerchTax = 0

*!*************************************************************
*! Name      : lfGetDefCode
*! Developer : Wael ALy MOhamed
*! Date      : 05/14/1997
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
PRIVATE lcObject

IF llOpnFiles=.T.
  =gfOpenFile(gcDataDir+'Fabric',gcDataDir+'Fabric','SH') 
  =gfOpenFile(gcDataDir+'FabDye',gcDataDir+'FabDye','SH')
  =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
  =gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')
  llOpnFiles=.F.
ENDIF 

IF (llFrmSetup AND llChkUnCom) OR !llFrmSetup
  llGoAndChk = IIF(llFrmSetup, .F., llGoAndChk)
  llChkUnCom = .F.
  
  IF gfUnCompSession(lcProgID, lnSessNo, "Material Direct Invoice")
    STORE .F. TO laScrMode
    DO CASE
      CASE lcScrMode = 'V'       && case Void 
        STORE .T. TO laScrMode[2]
      CASE lcScrMode = 'N'
        STORE .T. TO laScrMode[4]  && case New
    ENDCASE
   
    SELECT IIF(lcScrMode='V','MINVHDR',lcInvHdr)
    IF lcScrMode='V'
      =SEEK(lcCurrInv)
    ELSE
      GO TOP
    ENDIF
    SCATTER FIELDS &lcScFields TO laData
    SELECT IIF(lcScrMode='V','MInvLine',lcInvLine)
    SET ORDER TO TAG MInvLine
    SET RELATION TO Fabric+Color+laData[21]+DYELOT INTO FabDye
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    
    IF lcScrMode='V'
      =SEEK(lcCurrInv)
    ELSE
      GO TOP
    ENDIF
    SCATTER MEMVAR MEMO
    STORE .T. TO llContinue,llCUpDate
    STORE IIF(lcScrMode='V','MINVHDR',lcInvHdr) TO ;
          laCodes[1,7],laCodes[1,8],laCodes[2,7],laCodes[2,8],laCodes[3,7],;
          laCodes[3,8],laCodes[4,7],laCodes[4,8],laCodes[5,7],laCodes[5,8]
    STORE IIF(lcScrMode='V','laData[1]','laData[2]+laData[3]+laData[5]+laData[30]') TO ;
          laCodes[1,9],laCodes[2,9],laCodes[3,9],laCodes[4,9],laCodes[5,9]
    lcSession = UNCMSESS.cSession
    lcFiles = ALLTRIM(UnCmSess.Mtmpfiles)

    =lfGetInfo()
    lcObject=ALLTRIM(UnCmSess.cCurrObj)
    IF !EMPTY(lcObject)
      IF llFrmSetup AND (UPPER(lcObject)="PBSAV" .OR. UPPER(lcObject)="PBDLT")
        =IIF(UPPER(lcObject)="PBSAV",lpSavScr(),lpDelScr())
        =lfCratTemp()
        laScrMode    = .F.
        laScrMode[1] = .T.
      ELSE
        _CUROBJ = OBJNUM(&lcObject)
        KEYBOARD "{ENTER}" PLAIN CLEAR
      ENDIF
      SHOW GETS
    ENDIF
  ELSE
    =lfCratTemp()
  ENDIF
ENDIF

=lfBrowDet()

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

SELECT MINVHDR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+12,4]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'lUpsIns'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'lCompUps'
laFileStru[lnFileStru+3,2] = 'L'
laFileStru[lnFileStru+3,3] = 1
laFileStru[lnFileStru+3,4] = 0
laFileStru[lnFileStru+4,1] = 'LastLine'
laFileStru[lnFileStru+4,2] = 'N'
laFileStru[lnFileStru+4,3] = 6
laFileStru[lnFileStru+4,4] = 0
laFileStru[lnFileStru+5,1] = 'STNAME'
laFileStru[lnFileStru+5,2] = 'C'
laFileStru[lnFileStru+5,3] = 30
laFileStru[lnFileStru+5,4] = 0
laFileStru[lnFileStru+6,1] = 'CADDRESS1'
laFileStru[lnFileStru+6,2] = 'C'
laFileStru[lnFileStru+6,3] = 30
laFileStru[lnFileStru+6,4] = 0
laFileStru[lnFileStru+7,1] = 'CADDRESS2'
laFileStru[lnFileStru+7,2] = 'C'
laFileStru[lnFileStru+7,3] = 30
laFileStru[lnFileStru+7,4] = 0
laFileStru[lnFileStru+8,1] = 'CADDRESS3'
laFileStru[lnFileStru+8,2] = 'C'
laFileStru[lnFileStru+8,3] = 30
laFileStru[lnFileStru+8,4] = 0
laFileStru[lnFileStru+9,1] = 'CADDRESS4'
laFileStru[lnFileStru+9,2] = 'C'
laFileStru[lnFileStru+9,3] = 30
laFileStru[lnFileStru+9,4] = 0
laFileStru[lnFileStru+10,1] = 'CADDRESS5'
laFileStru[lnFileStru+10,2] = 'C'
laFileStru[lnFileStru+10,3] = 30
laFileStru[lnFileStru+10,4] = 0
laFileStru[lnFileStru+11,1] = 'LKEYOFF'
laFileStru[lnFileStru+11,2] = 'L'
laFileStru[lnFileStru+11,3] = 0
laFileStru[lnFileStru+11,4] = 0
laFileStru[lnFileStru+12,1] = 'NTAXDUE'
laFileStru[lnFileStru+12,2] = 'N'
laFileStru[lnFileStru+12,3] = 13
laFileStru[lnFileStru+12,4] = 2
=gfCrtTmp(lcInvHdr,@laFileStru,[ACCOUNT+CMORDER+STORE+PIKTKT],lcInvHdr)

SELECT MaSoLin
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+5,4]
laFileStru[lnFileStru+1,1] = 'nTaxRate'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 10
laFileStru[lnFileStru+1,4] = 2
laFileStru[lnFileStru+2,1] = 'nSteps'
laFileStru[lnFileStru+2,2] = 'N'
laFileStru[lnFileStru+2,3] = 2
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'LBACKORD'
laFileStru[lnFileStru+3,2] = 'L'
laFileStru[lnFileStru+3,3] = 0
laFileStru[lnFileStru+3,4] = 0
laFileStru[lnFileStru+4,1] = 'CONSOL'
laFileStru[lnFileStru+4,2] = 'C'
laFileStru[lnFileStru+4,3] = 1
laFileStru[lnFileStru+4,4] = 0
laFileStru[lnFileStru+5,1] = 'LTAXABLE'
laFileStru[lnFileStru+5,2] = 'L'
laFileStru[lnFileStru+5,3] = 0
laFileStru[lnFileStru+5,4] = 0
DECLARE laIndex[2,2]
laIndex[1,1] = 'Account+CMORDER+Store+Fabric+Color'
laIndex[1,2] = 'MInvLins'
laIndex[2,1] = 'Account+CMORDER+Store+STR(LineNo,6)'
laIndex[2,2] = 'MInvLine'
=gfCrtTmp(lcInvLine,@laFileStru,@laIndex)

SELECT (lcInvLine)
SCATTER MEMVAR MEMO

IF laSetups[15,2]='Y'
  SELECT MUPSBOX
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'CMORDER'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'PikTkt'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 6
  laFileStru[lnFileStru+2,4] = 0
  =gfCrtTmp(lcUpsBox,@laFileStru,[CMORDER+Store+PikTkt+STR(CARTONS,5)],lcUpsBox)
ENDIF

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
PRIVATE lnAlias
lnAlias = SELECT()

IF laScrMode[4] AND !USED(lcInstHdr)
  *-- create the instalment header temp file 
  =gfCrtTmp(lcInstHdr,[(CMORDER C(6),Store C(8),PikTkt C(6),;
            cInstmType C(1), nInstmFreq N(3),nInstIAmnt N(11,2),dInstmStDt D,;
            nNoInstm N(3), cInstmRef C(30),nInstIPcnt N(6,2),nInstmAmnt N(10,2))],;
            [CMORDER+Store+PikTkt],lcInstHdr)
  *-- create the instalment line temp file            
  =gfCrtTmp(lcInstLin,[(CMORDER C(6),Store C(8),PikTkt C(6),;
              cInstalNo C(3),nInstmAmnt N(10,2),DueDate D,nInstmPcnt N(6,2),cInstmNote C(30))],;
              [CMORDER+Store+PikTkt+cInstalNo],lcInstLin)
ENDIF

DO gpInvInstm IN (gcapphome+gcwinappl+'\ARMTINV.PRG') WITH ;
   laData[1],laData[3],laData[5],laData[30],laData[38],laData[41],;
   IIF(laScrMode[2] OR laScrMode[3] ,'ArMInstH',lcInstHdr),IIF(laScrMode[2] OR laScrMode[3],'ArMInstD',lcInstLin)
=IIF(laScrMode[4],lfSavTmpFiles(),.T.)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvCredits
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Customer Credits
*!*************************************************************
*! Calls     : gpOpnCredits
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCredits()
*!*************************************************************
FUNCTION lfvCredits
PRIVATE lnApplied,llKeyOff,lnKeyOffAmnt
*-- key off amount =initial instalment amount or balance to pay                                         
lnKeyOffAmnt = IIF(USED(lcInstHdr) AND SEEK(laData[3]+laData[5]+laData[30],lcInstHdr),;
               &lcInstHdr..nInstIAmnt,laData[38])
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
  laFileStru[lnFileStru+1,1] = 'CMORDER'
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
  =gfCrtTmp(lcAppCrdt,@laFileStru,[Account+CMORDER+cStore+PikTkt+Tran],lcAppCrdt)
ENDIF

llKeyOff = &lcInvHdr..lKeyOff
*-- Apply Invoice to customer credits
DO gpOpnCredits IN (gcapphome+gcwinappl+'\ARMTINV.PRG') WITH ;
laData[2],laData[3],laData[5],laData[30],lnKeyOffAmnt,'llKeyOff',lcAppCrdt,laData[22]
SELECT (lcAppCrdt)
*-- Seek amount + order + store + pick ticket 
=SEEK(laData[2]+laData[3]+laData[5]+laData[30])
SUM REST Amount TO lnApplied WHILE ;
Account+CMORDER+cStore+PikTkt=laData[2]+laData[3]+laData[5]+laData[30]
*-- cash on delivery amount
laData[28] = lnKeyOffAmnt - ABS(lnApplied)

=IIF(laScrMode[4],lfSavTmpFiles(),.T.)

SELECT (lcInvHdr)
=RLOCK()
REPLACE COD_AMT WITH laData[28] ,;
        lKeyOff WITH llKeyOff
UNLOCK
SHOW GET laData[28]
=lfRefresh(lcWinChrg)

*!*************************************************************
*! Name      : lfSavTmpFiles
*! Developer : WAM
*! Date      : 02/27/1999
*! Purpose   : Save temporary files in the uncmsess file
*!*************************************************************
*! Calls     : gfSavSess
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfSavTmpFiles()
*!*************************************************************
FUNCTION lfSavTmpFiles
PRIVATE lcFiles,lnAlias

lnAlias = SELECT()
lcFiles = 'lcInvHdr,' +lcInvHdr +','+lcInvHdr+';'+;
          'lcInvLine,'+lcInvLine+',MInvLine;'
IF USED(lcInstHdr) AND SEEK(laData[3]+laData[5]+laData[30],lcInstHdr)
  lcFiles = lcFiles + 'lcInstHdr,' +lcInstHdr+','+lcInstHdr+';'+;
                      'lcInstLin,' +lcInstLin+','+lcInstLin+';'
ENDIF
IF USED(lcAppCrdt) AND SEEK(laData[2]+laData[3]+laData[5]+laData[30],lcAppCrdt)
  lcFiles = lcFiles + 'lcAppCrdt,' +lcAppCrdt+','+lcAppCrdt+';'
ENDIF
IF llIsEngland AND USED(lcEngChrg) AND SEEK(laData[3]+laData[5]+laData[30],lcEngChrg)
  lcFiles = lcFiles + 'lcEngChrg,'+lcEngChrg+','+lcEngChrg+';'
ENDIF
IF laSetups[15,2]='Y' AND USED(lcUpsBox) AND SEEK(laData[3]+laData[5]+laData[30],lcUpsBox)
  lcFiles = lcFiles + 'lcUpsBox,'+lcUpsBox+','+lcUpsBox+';'
ENDIF
=gfSavSess(lcProgID, lcFiles, @laVars,lcSession)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfGetprice
*! Developer : WAM
*! Date      : 02/22/1999
*! Purpose   : Get Style Price
*!*************************************************************
*! Calls     : lfCheckPri()
*!*************************************************************
*! Passed Parameters  :  lcStyle : Style
*!                       lcLevel : Price level
*!                       lnQuantity : invoice Quantity
*!*************************************************************
*! Returns            :  Alternative price
*!*************************************************************
*! Example            :  =lfGetprice(m.Style,'A',100)
*!*************************************************************
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
lnPrice = IIF(!llMulCurr OR laData[22]=gcBaseCurr,Style.Price&lcLevel,;
                      gfStyPrice(lcStyle,lcLevel,laData[22]))
*-- if price= 0 then get the price from the user                      
RETURN(IIF(lnPrice=0,lfCheckPri(lcStyle,lcLevel),lnPrice))

*!*************************************************************
*! Name      : lfCheckPri
*! Developer : WAM
*! Date      : 02/22/1999
*! Purpose   : Select alternative price level
*!*************************************************************
*! Calls     : gfStyPrice(),SOSTYPRI.SPX
*!*************************************************************
*! Passed Parameters  :  lcStyle : Style
*!                       lcLevel : Price level
*!*************************************************************
*! Returns            :  Alternative price
*!*************************************************************
*! Example            :  =lfCheckPri(m.Style,'A')
*!*************************************************************
FUNCTION lfCheckPri
PARAMETERS lcStyle,lcLevel
PRIVATE lcTitle,lcPrompt,lcPrompt1,lcPrompt2,lcPrompt3,lnPrice,lnprice1,;
        lnPrice2,lnPrice3

=SEEK(lcStyle,'Style')
lcTitle  = PROPER(lcStyHdr)+': '+lcStyle
lnPrice1 = IIF(!llMulCurr OR laData[22]=gcBaseCurr,Style.PriceA,;
                       gfStyPrice(lcStyle,'A',laData[22]))
lnPrice2 = IIF(!llMulCurr OR laData[22]=gcBaseCurr,Style.PriceB,;
                       gfStyPrice(lcStyle,'B',laData[22]))
lnPrice3 = IIF(!llMulCurr OR laData[22]=gcBaseCurr,Style.PriceC,;
                       gfStyPrice(lcStyle,'C',laData[22]))
lnPrice   = IIF(lnPrice1 >0,1,IIF(lnPrice2 >0,2,AT(lcLevel,'ABC')))
lcPrompt  = "Price level '"+lcLevel+"' is zero. Proceed with"
lcPrompt1 = 'Price level A, '+ALLTRIM(STR(lnPrice1,12,2))
lcPrompt2 = 'Price level B, '+ALLTRIM(STR(lnPrice2,12,2))
lcPrompt3 = 'Price level C, '+ALLTRIM(STR(lnPrice3,12,2))

DO (gcScrDir+"SOSTYPRI.SPX")
RETURN(EVAL('lnPrice'+STR(lnPrice,1)))
             
*!*************************************************************
*! Name      : lfCreatHst
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 04/22/1999
*! Purpose   : Create Temporary file.
*!*************************************************************
*! Calls     : gfCrtTmp
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  =lfCreatHst()
*!*************************************************************
*
FUNCTION lfCreatHst
SELECT ARHIST
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+5,4]
laFileStru[lnFileStru+1,1] = "nStep"
laFileStru[lnFileStru+1,2] = "N"
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = "cYear"
laFileStru[lnFileStru+2,2] = "C"
laFileStru[lnFileStru+2,3] = 4
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = "cPrd"
laFileStru[lnFileStru+3,2] = "C"
laFileStru[lnFileStru+3,3] = 2
laFileStru[lnFileStru+3,4] = 0
laFileStru[lnFileStru+4,1] = "cShToOpn"
laFileStru[lnFileStru+4,2] = "C"
laFileStru[lnFileStru+4,3] = 1
laFileStru[lnFileStru+4,4] = 0
laFileStru[lnFileStru+5,1] = "nTrnNewAmn"
laFileStru[lnFileStru+5,2] = "N"
laFileStru[lnFileStru+5,3] = 11
laFileStru[lnFileStru+5,4] = 2

*--Index tags array.
DIMENSION laTags[3,2]
laTags[1,1] = 'Account+TranType+Tran+cInstalNo'
laTags[1,2] = lcTmpAR
laTags[2,1] = 'Account+Tran+cInstalNo'
laTags[2,2] = 'Tran'
laTags[3,1] = 'cShToOpn'
laTags[3,2] = 'Show'

*--Call Create temp file.
=gfCrtTmp(lcTmpAR,@laFileStru,@laTags)
SELECT (lcTmpAR)
SET ORDER TO TAG (lcTmpAR)
*-- end of lfCreatHst.


*!**************************************************************************
*! Name      : lfHgWUpdat
*! Developer : Sameh (SSE)
*! Date      : 06/09/1999
*! Purpose   : Update nHgWtrMark field (Customer) with NETBAL, if NETBAL is greater
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Returns            : None
*!*************************************************************************
*! Example   : =lfHgWUpdat()
*!*************************************************************************
*
FUNCTION lfHgWUpdat
=RLOCK()
REPLACE nHgWtrMark WITH IIF(NETBAL>nHgWtrMark,NETBAL,nHgWtrMark)
UNLOCK
*-- End of lfHgWUpdat.

*!*************************************************************
*
FUNCTION lfvOk
IF laSetups[4,2]='Y' AND ldVDate < MInvHdr.dPostDate
  llVoid = .F.
  =gfModalGen("INM40163B00000" , "DIALOG")
  ldVDate   = ldOldDate
  _CurObj   = OBJNUM(ldVDate)
ELSE
  llVoid = .T.
  CLEAR READ
ENDIF
  
*!*************************************************************
*
FUNCTION lfvCan

llVoid = .F.

*!*************************************************************
*
FUNCTION lfwVDate

ldOldDate = ldVDate

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
PRIVATE lnComm1 ,lnComm2 ,lnNetShipAmnt,lnNetAmnt, lnAlias

STORE 0 TO lnComm1 ,lnComm2
*-- Restore  current alias
lnAlias = SELECT(0)

WAIT 'Computing Sales Rep. commission...' WINDOW NOWAIT
*-- seek to Account+Order+Store+PikTkt in Invoice Header [Start]
IF SEEK(laData[2]+laData[3]+laData[5]+laData[30] ,lcInvHdr)
  = SEEK(laData[2]+laData[3]+laData[5] ,lcInvLine)
  SELECT (lcInvLine)
  *-- Accumulate sales Rep. commission
  SCAN REST WHILE Account+CMORDER+Store+PikTkt = ;
                  &lcInvHdr..Account+&lcInvHdr..CMORDER+&lcInvHdr..Store+&lcInvHdr..PikTkt
    lnNetAmnt = &lcInvLine..FabQty*&lcInvLine..nSellNetPr*(1-&lcInvHdr..DiscPcnt/100)*(1-&lcInvHdr..Trde_Disc/100)
    lnComm1   = lnComm1 + (lnNetAmnt * &lcInvLine..Comm1 / 100)
    lnComm2   = lnComm2 + (lnNetAmnt * &lcInvLine..Comm2 / 100)
  ENDSCAN
  *-- calculate net shipment amount
  lnNetShipAmnt = &lcInvHdr..FabShipAmt*(1-&lcInvHdr..DiscPcnt/100)*(1-&lcInvHdr..Trde_Disc/100)
  *-- for lnNetShipAmnt <> 0 update header commission [Start]
  IF lnNetShipAmnt <> 0
    *-- header commission for Sales Rep 1 
    laData[18] = ROUND( lnComm1/lnNetShipAmnt*100 , 2)
    *-- header commission for Sales Rep 2 
    laData[20] = ROUND( lnComm2/lnNetShipAmnt*100 , 2)
  ENDIF && for lnNetShipAmnt <> 0 update header commission [End]
ENDIF && seek to Account+CMORDER+Store+PikTkt in Invoice Header [End]

SHOW GET laData[18] DISABLE && header commission for Sales Rep 1 
SHOW GET laData[20] DISABLE && header commission for Sales Rep 2 
SELECT (lnAlias)
WAIT CLEAR

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Abdou ElGendi
*! Date      : 02/17/2000
*! Purpose   : Function to store old value of the current filed.
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
*! Name      : lfEdtFld
*! Developer : Nader Anis (NAD)
*! Date      : 03/28/2000
*! Purpose   : To handle the status of the screen fields in the edit mode
*!             in the invoice header  
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
FUNCTION lfEdtFld
PARAMETERS lcFlds

lcFlds=lcFlds+','

FOR lnloop = 1 TO OCCURS(',',lcFlds)
  lcFld=SUBSTR(lcFlds,1,AT(',',lcFlds)-1)
  lnItem=AT(lcFld,lcScFields)
  IF lnItem >0 
    lnItem=OCCURS(',',LEFT(lcScFields,LnItem))+1   
    lcScFld='laData['+ STR(lnItem,2) +']' 
    SHOW GET &lcScFld ENABLE
  ELSE
    SHOW GET &lcFld ENABLE
  ENDIF   
  lcFlds=SUBSTR(lcflds,LEN(lcFld)+2)
ENDFOR

*:*************************************************************
*! Name      : lfEdtInv
*! Developer : Nader Anis (NAD)
*! Date      : 03/28/2000
*! Purpose   : Save function for the edit mode of the invoice
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************

FUNCTION lfEdtInv
PARAMETERS lcSavFlds

lcSavFlds=lcSavFlds+',' 
=RLOCK()
FOR lnloop = 1 TO OCCURS(',',lcSavFlds)
  lcFld=SUBSTR(lcSavFlds,1,AT(',',lcSavFlds)-1)
  lnItem=AT(lcFld,lcScFields)
  IF lnItem >0 
    lnitem=OCCURS(',',LEFT(lcScFields,Lnitem))+1   
    lcScFld='laData['+ Str(lnItem,2) +']'
    REPLACE &lcFld WITH  &lcScFld
  ENDIF   
  lcSavFlds=SUBSTR(lcSavFlds,len(lcFld)+2)
ENDFOR
UNLOCK

*:*************************************************************
*! Name      : lfwStore
*! Developer : Nader Anis (NAD)
*! Date      : 09/17/2000
*! Purpose   : To save the old store for later use.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************

FUNCTION lfwStore
lcOldStore=laData[5]

*:********************************************************************************
*! Name      : lfwCharges
*! Developer : Nader Anis (NAD)
*! Date      : 10/24/2000
*! Purpose   : When function for the charges screen fields. 
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
lnOldChrg=EVAL(SYS(18))
RETURN .T.

*:********************************************************************************
*! Name      : lfNegChrg
*! Developer : Nader Anis (NAD)
*! Date      : 10/24/2000
*! Purpose   : When function for the charges screen fields. 
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
lcObject=SYS(18)
IF &lcObject <0 
  &lcObject=lnOldChrg
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
*! Name      : lfvHstTax
*! Developer : Adel Mohammed El Gazzar
*! Date      : 03/19/2001
*! Purpose   : Compute HST Tax Rate and Amount
*!*************************************************************
*! Passed Parameters  :  llRate : .T. : HST Tax Rate Percent
*!                                .F. : HST Tax Rate Amount
*!*************************************************************
*! Example   :  =lfvHstTax(.T.)
*!*************************************************************
*
FUNCTION lfvHstTax
PARAMETERS llPercent

IF lfNegChrg()
  IF !UPDATE()
    RETURN
  ENDIF
  *--Tax Rule.
  laData[44] = STR(lnPstRule,2)
  *--Hst Amount
  laData[52] = IIF(llPercent,(laData[31]+laData[34]+laData[35]+laData[36]-;
               laData[31]*laData[32]/100+IIF(VAL(laData[44])=1,0,laData[37]));
               *laData[51]/100,laData[52])
  laData[51] = IIF(!llPercent,laData[52]*100/(laData[31]+laData[34]+;
               laData[35]+laData[36]-laData[31]*laData[32]/100+;
               IIF(VAL(laData[44])=1,0,laData[37])),laData[51])
  IF llCod
    laData[28]= laData[28]-&lcInvHdr..nHstAmt+laData[52]
    IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. laData[25]=1 ;
      .AND. SEEK(laData[3]+laData[5]+laData[30],lcUpsBox)
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH laData[28]
      UNLOCK
    ENDIF
  ENDIF
  llCUpdate = .T.
  laData[38] = laData[38] - &lcInvHdr..nHstAmt+laData[52]
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE Cod_Amt  WITH laData[28] ,; 
          cTaxRule WITH laData[44] ,; 
          nHstRate WITH laData[51] ,;
          nHstAmt  WITH laData[52] ,;
          TotalChg WITH laData[38]
  
  UNLOCK
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
ENDIF

*!**************************************************************************
*! Name      : lfvRoll
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/03/2002
*! Purpose   : To assign a new Rolls when voiding in Lot cost method.
*!**************************************************************************
*
FUNCTION lfvRoll

*-- Define needed variables for gfMatCrl used in calling rolls screen
PRIVATE lcKey , lnAdjStk , lcFDyelot , lcTrType , lnAlias , ldTrDate ,;
        ldPostDate , lcTrCode , lcRollNP , lcRollRP , lcFabric ,;
        lcColor , lcLineKey , lnOldQty

lnAlias = SELECT(0)

*-- ABD - Save the fabric conv. & the Program Name to use it in the gfmatcrl. [Begin]
*PRIVATE lnUseConv , lcCalProg
*lcCalProg = 'ARMDIN'

lnQtyBuy = IIF(laScrMode[2],mInvLine.FabQty,&lcInvLine..FabQty)
lnType = 1
lcKeyType = 'R'
lcType = 'Return #'
lcFabric = IIF(laScrMode[2],mInvLine.Fabric,&lcInvLine..Fabric)
lcColor  = IIF(laScrMode[2],mInvLine.Color,&lcInvLine..Color)
lnAlias = SELECT(0)
lnOldQty = 0

*B606163,1 ABD - Save the fabric conv. to use it in the gfmatcrl. [Begin]
*lnUseConv = IIF(Fabric.Conv = 0 , 1, Fabric.Conv)
*B606163,1 ABD - [End]

=gfOpenFile(gcDataDir+'MatInvJl',gcDataDir+'MatInvJl','SH')
=gfOpenFile(gcDataDir+'Rolls',gcDataDir+'RolApl','SH')

lcTrType  = '5'
lcFDyelot = IIF(laScrMode[2],mInvLine.Dyelot,&lcInvLine..Dyelot)
llDyeLvl  = laSetups[8,1] = "Y"
lcRollNP  = lcSelecP
lcRollRP  = lcUnSelP
ldTrDate  = ldDefInvDate
ldPostDate = IIF(laSetups[4,2]='Y',ldDefPstDate,ldTrDate) &&-- Ask MAN
lcTrCode   = IIF(laScrMode[2],mInvLine.cMOrder,&lcInvLine..cMOrder)
lnLineNo = IIF(laScrMode[2],mInvLine.LineNO,&lcInvLine..LinenO)
lcFJlSess  = lcSession

lcWareCode = IIF(laScrMode[2],mInvLine.cWareCode,&lcInvLine..cWareCode)
llWareHous = laSetups[5,2]='Y'
lcToWare   = ''

lcKey    = IIF(laScrMode[2],mInvLine.Fabric+mInvLine.Color,&lcInvLine..Fabric + &lcInvLine..Color)
lnAdjStk = IIF(laScrMode[2],mInvLine.FabQty,&lcInvLine..FabQty)

lnOldAdjStk = lnAdjStk
*lnAdjStk = lnAdjStk * -1
lcCostMeth = laSetups[7,2]

DO lfJrRoData.PRG IN gfMatCrl.prg WITH .T.

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
*-- End of lfvRoll.

*!**************************************************************************
*! Name      : lfvQty
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the quantity field
*!**************************************************************************
*
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
_CUROBJ = OBJNUM(m.FabQty)
KEYBOARD "{ENTER}" PLAIN CLEAR
SELECT(lnAlias)
*-- End of lfvQty.

*:*************************************************************
*: Name       : lfFilsUpd
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 07/09/2002
*: Purpose    : Update 
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfFilsUpd()
*:*************************************************************
*B606163,1 ABD - [Begin]
FUNCTION lfFilsUpd
PRIVATE lnAlias , lcReSesion , llMatInvJl , llRolls , lcInvNo ,;
        lcLineNo, lnCostUse , lcFabric , lcColor , lcwarecode, lcdyelot
STORE '' TO lcReSesion , lcFabric , lcColor , lcwarecode, lcdyelot
STORE .F. TO llMatInvJl , llRolls  , lcInvNo , lcLineNo
STORE 0 TO lnCostUse , lnLineNo
lnAlias = SELECT (0)


IF !USED('MATINVJL')
  llMatInvJl=gfOpenFile(gcDataDir+'MATINVJL', 'MATINVJL', 'SH')
ENDIF

IF laSetups[7,2] = 'L' .AND. !USED('Rolls')
  llRolls = gfOpenFile(gcDataDir+'Rolls', gcDataDir+'RolApl','SH')
ENDIF

IF laSetups[4,2]='Y'
  laGlArray[1,1]  = GL_Sales
  laGlArray[1,2]  = '008'
  laGlArray[1,3]  = -1
  laGlArray[1,10] = cCOGSAcnt
  
  laGlArray[2,1]  = GL_COST
  laGlArray[2,2]  = '015'
  laGlArray[2,3]  = 1
  laGlArray[2,10] = cICAcnt
ENDIF


*-- In case of
DO CASE
  *-- Case Cost Methold LOT
  CASE laSetups[7,2] $ 'L'
    *-- Hold Line No.
    laOtherPar[1,2] =  MInvLine.LineNo
    *-- Hold Invoice Number.
    lcInvNo   = MInvLine.cMatinv
    *-- Hold The line no.
    lnLineNo  = MInvLine.LineNo
    SCATTER FIELDS nSellconv to laConv
    
    SELECT * FROM MATINVJL ;
    WHERE cTran+cOprCode+cLotNo+cTranType+cFabric+cColor+cWarecode = lcInvNo ;
    AND ctrantype = '5' AND MATINVJL.lineNo = lnLineNo INTO CURSOR (lcMatInvJl) ;
    ORDER BY cTran,cOprCode,cLotNo,cTranType,cFabric,cColor,cWarecode,cDyelot,cRSession,cISession
    LOCATE
  
    SCAN
      lcFabric   = mInvLine.Fabric
      lcColor    = mInvLine.Color
      lcwarecode = mInvLine.cWareCode
      lcdyelot   = mInvLine.Dyelot    
    
      *-- Issue Return Qty.
      lnUseQty = nIssued /IIF(MInvLine.nSellconv=0,1,MInvLine.nSellconv)  
      *-- Issue Session
      lcRSession = cRsession
      lcISession = cisession
      lnCostUse  = nUnitCost
  
      *-- This Function to Update the Rolls File.
      IF SEEK(mInvLine.Fabric+mInvLine.Color,'Fabric') .AND. Fabric.lTrkRolls
        SELECT ROLLS
        lcOldOrder = ORDER()
        SET ORDER TO Rolapl
        *-- Get Need record
        SELECT * FROM ROLLS  ;
        WHERE crsession+cisession+crollitem+color+cwarecode+dyelot+crollid =;
         lcRSession+lcISession+lcFabric+lcColor+lcwarecode+lcdyelot;
        AND Trancd = '2'  INTO CURSOR (lcMatRolls) ;
        ORDER BY crsession,cisession,crollitem,color,cwarecode,dyelot,crollid
     
        SELECT ROLLS
        SET ORDER TO Rollitem
        SELECT (lcMatRolls)
        LOCATE
        SCAN
          *-- Update the Header Record
          *-- crollitem+color+cwarecode+dyelot+crollid+trancd+crsession
          SELECT ROLLS
          =SEEK(lcFabric+lcColor+lcwarecode+lcdyelot+&lcMatRolls..cRollid+"1"+&lcMatRolls..cRSession)
          REPLACE nqtybal WITH nqtybal + &lcMatRolls..nQty
        
          *-- Update the issue Line & add it as recived line.
          SELECT (lcMatRolls)
          SCATTER MEMVAR
          m.nQty    =  &lcMatRolls..nQty * -1
          m.nQtyBal = 0
          INSERT INTO ROLLS FROM MEMVAR
        ENDSCAN
        SELECT ROLLS 
        SET ORDER TO &lcOldOrder
      ENDIF
      STORE MInvLine.nSellconv TO m.nSellconv
      = gfMatCrl('5',mInvLine.Fabric,mInvLine.Color,mInvLine.cWareCode,mInvLine.Dyelot,;
                     mInvHdr.InvDate,mInvHdr.dPostDate,laData[1],lnUseQty,lnCostUse,'','',0,'',;
                    '',@laGlArray,'','',&lcInvLine..cMatinv,lcRSession,lcISession,'','','',@laOtherPar)

      SELECT (lcMatInvJl)
    ENDSCAN
  CASE laSetups[7,2] $ 'AS'
    
    *B606702,1 Fix bug variable 'LACONV' not found. [Begin]
    SCATTER FIELDS nSellconv to laConv
    *B606702,1 Fix bug variable 'LACONV' not found. [End]
    
    =gfMatCrl('5',mInvLine.Fabric,mInvLine.Color,mInvLine.cWareCode,mInvLine.Dyelot,;
              mInvHdr.InvDate,mInvHdr.dPostDate,laData[1],;
              lnUseQty,CostUse,'','',0,'','',@laGlArray,lcSession,'','','','','','','',@laOtherPar)
  CASE laSetups[7,2] $ 'FI'

ENDCASE


*-- Close opened files.
IF llMatInvJl AND USED('MatInvJl')
  USE IN MatInvJl
ENDIF 

IF llRolls AND USED('Rolls')
  USE IN Rolls
ENDIF


SELECT (lnAlias)
*-- End Of lfFilsUpd.
*B606163,1 ABD - [End]
*:*************************************************************
