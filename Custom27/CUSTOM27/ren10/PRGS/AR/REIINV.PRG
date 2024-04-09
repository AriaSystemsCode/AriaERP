*************************************************************************
*: Program file  : REIInv.PRG
*: Program desc. : Issue Invoice
*:
*:         System: Aria Apparel System
*:      Developer: NAD - Nader Anis Mitry
*:
*:     C102761,1 Pre-Billing Selection Screen
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Modifications      :
*: B605016,1 BWA 10/14/2001 Fix the bug of updating the dates.
*: C200245,1 SSE 11/08/2001 Add 3 columns to In Range screen
*: C200245,1                Also allow user to filter by Date , Bill of lading and customer
*: B605193,1 SSE 12/04/2001 Fix the bug of adding wrong discount percent in last invoice.
*: B605864,1 ADEL 10/14/02 Enable editing lines its lines are removed from packing list.
*: B606941,1 NAD  02/06/2003 File Pack_Hdr Not Found 
*: B119562,1 SSE 09/08/2003 Fix bug of wrong summation when getting 2 consol. invoices for same account.
*:**************************************************************************************


DECLARE lafolders[4,2],lafoldwinds[4,2],laAddress[6,3],laQtyPaste[1,8],;
        laSetups[26,2],laBrowArr[1],laShipName[1,2]
llConsInv = .F.        

DECLARE laCodes[6,10],laTerms[1,2],laSeasons[1,2],laDivision[1,2],laVRltFld[1,2],;
        laShipVia[1,2],laSpcInst[1,2],laTRltFld[6,2],laVars[11],laWareHouses[1,2]

lcCrtTmp  = ""
llCalld   = .F.
lcChrgTyp = ""



DECLARE laCanReason[1,2]


STORE 0 TO lnRep1Comm ,lnRep2Comm
STORE '' TO laCanReason,lcBackOrd
STORE 1  TO lnCanReason
DECLARE laDisRltFld[1,2]

STORE 0   TO lnDisc_Pcnt
STORE '' TO laCodes,laTerms,laShipVia,laSpcInst,laSeasons,laDivision,;
            laVars,lcGlSession,laBrowArr,laShipName,lcTaxName
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
STORE 0   TO lnCartons,lnSouComm1,lnSouComm2,lnActFolder,lnStyLngth
STORE 1   TO lnShipAddr,lnPstRule,lnWareHouse,lnShipName
STORE ''  TO lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
             lcFolder,lcTaxTitle,lcTaxBreak,lcSession,lcUpsType,lcOrdStat
STORE .F. TO llBrowse,llZoom,llIsCanada,llIsEngland,llMulCurr,llEditExRt,;
             llCod,llContinue,llSysDate,llNoShow,laSetups,llInstTerm,;
             llAddLine,llUpCnsLine,llOpnCrdt
STORE ""  TO lcInvLine,lcInvHdr,lcPackHdr,lcUpsBox,lcEngChrg,lcFlFields,;
             lcScFields,lcWinChrg,laWareHouses,lcStyHdr,lcEmptySty,;
             lcInstLin,lcInstHdr,lcAppCrdt,lcMjrMsk,lcStyMjr,lcConsInvH,lcConsInvD


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



DECLARE laEngStyTax[1,2]
STORE 0 TO lnTaxRate  && Tax Rate at Style level  
STORE '' TO laEngStyTax

lcSadInvH = ""   
lcSadInvL = "" 
lcSadUpsB = "" 
lcSadInsH = ""   
lcSadInsL = "" 
lcSadOrdC = ""
lcFltFile  = ""
lcBrwFile  = ""

IF !gfSetup()
  RETURN
ENDIF

*C200245,1 Display an option grid to allow user filter pre billed invoice. [Begin]
PRIVATE lcInvExp , lcBolName , lcAcctName
STORE '' TO lcBolName , lcAcctName

PUSH KEY CLEAR
lcInvExp = gfOpGrid('ARPRBIL', .T.)
POP KEY

*B605864,1 (Begin) Get these lines here for optimizing.
llOgioTmp = ASCAN(laEvntTrig , PADR('TMPUPS',10)) <> 0
=gfOpenFile(gcDataDir+'INVHDRS',gcDataDir+'INVHDRS','SH')
LOCATE
IF EOF()
  =gfModalGen('INM00000B00000','ALERT','','', 'No Records to display.')
  glQuitting=.T.
  CLEAR READ
  RETURN
ENDIF
*B605864,1 (End)


IF USED(lcBolName)
  USE IN (lcBolName)
ENDIF

IF USED(lcAcctName)
  USE IN (lcAcctName)
ENDIF

*-- Check if user pressed the close button in option grid.
IF TYPE('lcInvExp') = 'L'
  RETURN
ENDIF
*C200245,1 Display an option grid to allow user filter pre billed invoice. [End]

lcSvKey=lcSydKey

lcProgID  = 'IINVOICE'
lcOrdBrow = 'List'
lcInvBrow = 'Invoice Lines'
lcBrTtlZ  = 'Zoom Invoice Lines'
lcSize1   = 'Size1'
lcSize2   = 'Size2'
lcSize3   = 'Size3'
lcSize4   = 'Size4'
lcSize5   = 'Size5'
lcSize6   = 'Size6'
lcSize7   = 'Size7'
lcSize8   = 'Size8'

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
lnfoldercend = 102.000
lnfolderrend = 2.000
STORE 'ENABLE' TO lcScopeStat


  PRIVATE lcUpdPik
  STORE SPACE(0) TO lcUpdPik  && Modify Picking upon invoice


lcUpdPik=gfGetMemVar('M_MPKTKIN' ,gcAct_Comp) 



IF !WEXIST(gcBaseWind)

  lnLastMode = 0
  SELECT ORDLINE
  SCATTER MEMVAR MEMO BLANK
  SELECT INVHDR
  SCATTER MEMVAR BLANK
  lcMjrMsk   = gfItemMask("PM")  
  lcEmptySty = PADR(STRTRAN(GFITEMMASK("PI"),'X',' '),19)
  lcStyHdr   = gfItemMask("HI")
  lcStyMjr   = gfItemMask("HM")
  lnStyLngth = LEN(lcStyHdr)        && Var. hold the length of the style.
  lcScFields = 'Order,CustPo,PikTkt,Account,Store,InvDate,cWareCode,Season,;
                Comm1,Comm2,Invoice,Flag,cDivision,CCURRCODE'

  lcFlFields = 'SHIPDATE,DUEDATE,APPROVAL,cTermCode,SHIPVIA,SPCINST,NCHRGTAX,'+;
               'DEPT,NOTE1,NOTE2,REP1,REP2,lUpsIns,Cod_Flag,NEXRATE,cFacCode,'+;
               'CARTONS,WEIGHT,CODTAG,COD_AMT,TRDE_DISC,SHIPAMT,DISCPCNT,DISCOUNT,'+;
               'FREIGHT,INSUR,COD,TAX_AMT,TOTALCHG,NCHARGES,NCURRUNIT,SHIP,'+;
               'nPstAmt,cTaxRule,TAX_RATE,nPstRate,nHstRate,nHstAmt,BOL_NO,APPRAMT,DPOSTDATE,CCODTRCKNO,STATUS'

  lcSession   = gfsequence('CSESSION')
  lcGlSession = gfsequence('GLSESSION')
  laSetups[1,1]  = 'M_PACK'        && Use Style packs/Sku
  laSetups[2,1]  = 'M_STY_COM'     && Commision at style level
  laSetups[3,1]  = 'M_LN_NOTE'     && Add note to the invoice line  
  laSetups[4,1]  = 'M_LINK_GL'     && Check for gl link
  laSetups[5,1]  = 'M_WareHouse'   && Use maltiple locations
  laSetups[6,1]  = 'M_GenOrNum'    && Enter order # manually
  laSetups[7,1]  = 'M_COST_METH'   && Cost method
  laSetups[8,1]  = 'M_DYELOT'      && Use dylot  
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
  
  laSetups[26,1] = 'M_HST_RATE' && HST Tax Rate
  
   
  =gfGetMemVar(@laSetups,gcAct_Comp)
  
 
  lcTaxName  = IIF(EMPTY(laSetups[24,2]),'G.S.T. Tax',laSetups[24,2])
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
  
  lcCrtTmp = gfTempName()
  
  llIsCanada = IIF(UPPER(ALLTRIM(gcContCode))='CANADA',.T.,.F.)
  IF UPPER(ALLTRIM(gcContCode))='ENG'
   *-- array to hold English taxes
    DECLARE laEngStyTax[1,2]
    laEngStyTax[1,1] = 'NTAXRATE'
    laEngStyTax[1,2] = 'lnTaxRate'
    *-- variables to hold the tax break and the tax name
    STORE "" TO lcTaxTitle,lcTaxBreak
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

  *E300834,1 Declare discount code related fields array
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
  laCodes[2,9]  = 'laData[4]+laData[1]+laData[5]+laData[3]'
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
  laCodes[3,9]  = 'laData[4]+laData[1]+laData[5]+laData[3]'
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
  laCodes[4,9]  = 'laData[4]+laData[1]+laData[5]+laData[3]'
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
  laCodes[5,9]  = 'laData[4]+laData[1]+laData[5]+laData[3]'
  laCodes[5,10] = 'cDivision'
  
  *E300956,1 Keep history of order line calceled quantity.
  laCodes[6,1]  = 'CCANCRESON'
  laCodes[6,2]  = 'laCanReason'
  laCodes[6,3]  = 'lnCanReason'
  laCodes[6,4]  = ''
  laCodes[6,5]  = .F.
  laCodes[6,6]  = .F.
  laCodes[6,7]  = lcOrdCanLn
  laCodes[6,8]  = lcOrdCanLn
  laCodes[6,9]  = "'O'+m.Order+STR(m.LineNo,6)"
  laCodes[6,10] = 'CCANCRESON'
  
  =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
  
  SELECT LEFT(cDesc,20),cWareCode FROM WAREHOUS INTO ARRAY laWareHouses
  
  =gfCloseFile('WAREHOUS')
    
  lcFltFile  = gfTempName()
  lcBrwFile  = gfTempName()
  =lfCratTemp()
  SELECT (lcInvHdr)
  SET ORDER TO TAG ('Select')
  =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
  =gfOpenFile(gcDataDir+'INVHDRS',gcDataDir+'INVHDRS','SH')
  =gfOpenFile(gcDataDir+'INVLINES',gcDataDir+'INVLINES','SH')
  =gfOpenFile(gcDataDir+'Scale',gcDataDir+'Scale','SH')
  =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
  =gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')
  =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
  
  *B606941,1 NAD (Start)
  * =gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH') 
  * =gfOpenFile(gcDataDir+'PACK_LIN',gcDataDir+'PACK_LIN','SH')
  IF 'AL' $ gcCmpModules
    =gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH') 
    =gfOpenFile(gcDataDir+'PACK_LIN',gcDataDir+'PACK_LIN','SH')
  ENDIF
  *B606941,1 NAD (END)
  =gfOpenFile(gcDataDir+'UPSBOXS',gcDataDir+'UPSBOXS','SH')
    =gfOpenFile(gcDataDir+'ORDCANSD',gcDataDir+'ORDCANSD','SH')
  =gfOpenFile(gcDataDir+'SDINSTMH',gcDataDir+'SDINSTMH','SH')
  =gfOpenFile(gcDataDir+'SDINSTMD',gcDataDir+'SDINSTMD','SH')
  
  SELECT STYLE
  SET RELATION TO 'S'+SCALE INTO SCALE
  SELECT (lcInvLine)
  SET RELATION TO STYLE INTO STYLE
  SET RELATION TO Style+cWareCode+Dyelot INTO STYDYE ADDITIVE
  
  SELECT INVHDRS
  IF EOF()
    =gfModalGen('INM00000B00000','ALERT','','', 'No Records to display.')
  
      glQuitting=.T.
      CLEAR READ
      RETURN
  ENDIF
  =AFIELDS(laFileStru)
  =gfCrtTmp(lcBrwFile,@laFileStru,[cSadInv],lcBrwFile)
  =gfCrtTmp(lcFltFile,@laFileStru,[cSadInv],lcBrwFile)  
  SELECT (lcBrwFile)
  
  *C200245,1 Add records to temp file according to lcRpExp. [Begin]  
  *APPEND from (gcDataDir+'INVHDRS.dbf') for EMPTY (FLAG)
  IF lcInvExp = ".T."
    APPEND FROM (gcDataDir+'INVHDRS.DBF') FOR EMPTY (FLAG)
  ELSE
    SELECT InvHdrs
    SCAN FOR EMPTY(FLAG) AND &lcInvExp.
      SCATTER MEMVAR MEMO
      INSERT INTO (lcBrwFile) FROM MEMVAR
    ENDSCAN
    SELECT (lcBrwFile)
  ENDIF  
  *C200245,1 Add records to temp file according to lcRpExp. [End]

  GO TOP
  
  *C200245,1 Check for records to display. [Begin] 
  IF EOF()
    =gfModalGen('INM00000B00000','ALERT','','', 'No Records to display.')        
    glQuitting = .T.
    CLEAR READ
    RETURN  
  ENDIF
  *C200245,1 Check for records to display. [Begin] 

  llContin = .T. 
     
  *C200245,1 Add 3 columns for Pick Ticket # , date and bill of lading. [Begin]
  *lcBrowFlds = "CsadInv    :H= 'Invoic#' :16, account   :H= 'Acct#' :10,STORE   :H= 'Store' :10,Order :H = 'Order' :16,Ship :H= 'Pieces' :10,ShipAmt:H= 'Amount' :20"   
  lcBrowFlds = "CsadInv    :H= 'Invoice#' :16, Piktkt : H='Pick Ticket#' :16, InvDate :H='Date' :16 ,account   :H= 'Acct#' :10,STORE   :H= 'Store' :10,Order :H = 'Order' :16,Ship :H= 'Pieces' :10,ShipAmt:H= 'Amount' :20,Bol_No :H='Bill Lading#' :16"   
  *C200245,1 Add 3 columns for Pick Ticket # , date and bill of lading. [End]
                      
  PUSH KEY                   
  llContin = gfrange(lcBrowFlds,lcFltFile,"cSadInv")
  POP KEY

  *B119562,1 Create Temp File for Account Message. [Begin]
  PRIVATE lcTmpAccnt , lcOldInvNo
  lcOldInvNo = ''
  lcTmpAccnt = gfTempName()
  CREATE CURSOR (lcTmpAccnt) (Account C(5))
  INDEX ON Account TAG (lcTmpAccnt)
  *B119562,1 Create Temp File for Account Message. [End]

  IF llContin  
    SELECT (lcFltFile)
    IF !EOF(lcFltFile)
    PRIVATE lcConsol
    lcConsol=""
    
    SCAN           
   
      WAIT WINDOW 'Updating Invoice # '+ CsadInv NOWAIT  
      IF SEEK (cSadInv,'INVHDRS')
       
        IF !INVHDRS.lLok_Stat
          IF !INVHDRS.account $ lcConsol      

            *B119562,1 Always consider all customers as consolidated. [Begin]
            *IF SEEK ('M'+INVHDRS.account,'CUSTOMER') AND CUSTOMER.CONSOL='Y'          
            *  lcConsol=lcConsol+ INVHDRS.account+'|'
            IF SEEK ('M'+INVHDRS.account,'CUSTOMER')
              lcOldInvNo = INVHDRS.cSadInv 
              IF !EOF('InvHdrs')
                SKIP IN InvHdrs
                IF InvHdrs.cSadInv == lcOldInvNo
                  lcConsol = lcConsol + INVHDRS.account+'|'  
                ENDIF
                SKIP -1 IN InvHdrs
              ENDIF              
            *B119562,1 Always consider all customers as consolidated. [End]
          
            ENDIF  
        
            SELECT INVHDRS
         
              SCAN REST WHILE cSadInv=&lcFltFile..cSadInv
                SCATTER MEMVAR MEMO
                m.Picked=M.NPICKED
                REPLACE lLok_Stat WITH .T.
                INSERT INTO (lcInVHdr) FROM MEMVAR      
              ENDSCAN            

            *B605864,1 (Begin) Initialize needed variables
            lnCartonNo = 0
            llFirst = .T.
            *B605864,1 (End)
              IF SEEK (&lcFltFile..cSadInv,'INVLINES')  
                SELECT INVLINES 
                SCAN REST WHILE cSadInv=&lcFltFile..cSadInv
                  SCATTER MEMVAR MEMO
                  *B605864,1 (Begin) Check if the line is still on the pack.
                 *B606941,1 NAD
                 *m.lPacked = SEEK(Piktkt+STR(LineNo,6)+Style,'Pack_Lin')
                 IF 'AL' $ gcCmpModules 
                    m.lPacked = SEEK(Piktkt+STR(LineNo,6)+Style,'Pack_Lin')
                  ELSE
                    m.lPacked =.F.
                  ENDIF
                  *B606941,1 NAD
                  *--Case !packed, Recompute the carton charges.
                  IF !m.lPacked
                    IF SEEK("�"+Account+Order+Store,lcInVHdr)
                      IF llFirst
                        REPLACE &lcInVHdr..Cartons  WITH IIF(!lPacked,&lcInVHdr..Cartons,0),;
                                &lcInVHdr..nCartons WITH IIF(!lPacked,&lcInVHdr..Cartons,0),;
                                &lcInVHdr..lCompUps WITH IIF(!lPacked,.F.,.T.)
                        llFirst  = .F.
                      ENDIF
                      =SEEK(STYLE,'STYLE')
                      IF lPacked
                        lnCartonNo = IIF(Style.Qty_Ctn>0,TotQty/Style.Qty_Ctn,0)
                        REPLACE &lcInVHdr..Cartons WITH &lcInVHdr..Cartons + lnCartonNo,;
                                &lcInVHdr..nCartons WITH &lcInVHdr..nCartons + lnCartonNo
                      ENDIF  
                    ENDIF
                  ENDIF
                  *B605864,1 (End)
                  
                  *B119562,1 Save the Shipped Quantities in Temp shipped Fields. [Begin]
                  m.TmpShp1 = m.Qty1
                  m.TmpShp2 = m.Qty2
                  m.TmpShp3 = m.Qty3
                  m.TmpShp4 = m.Qty4
                  m.TmpShp5 = m.Qty5
                  m.TmpShp6 = m.Qty6
                  m.TmpShp7 = m.Qty7
                  m.TmpShp8 = m.Qty8                  
                  *B119562,1 Save the Shipped Quantities in Temp shipped Fields. [End]

                  INSERT INTO (lcInvLine) FROM MEMVAR

                ENDSCAN 
              ENDIF
           
              IF SEEK (&lcFltFile..cSadInv,'UPSBOXS') 
                SELECT UPSBOXS
                SCAN REST WHILE cSadInv=&lcFltFile..cSadInv
                  SCATTER MEMVAR MEMO
                  INSERT INTO (lcUpsBox) FROM MEMVAR
                ENDSCAN 
              ENDIF
         
              IF SEEK (&lcFltFile..cSadInv,'ORDCANSD') 
                SELECT ORDCANSD
                SCAN REST WHILE cSadInv=&lcFltFile..cSadInv
                  SCATTER MEMVAR MEMO
                  INSERT INTO (lcOrdCanLn) FROM MEMVAR
                ENDSCAN 
              ENDIF 
      
              IF SEEK (&lcFltFile..cSadInv,'SDINSTMD') 
                SELECT SDINSTMD
                SCAN REST WHILE cSadInv=&lcFltFile..cSadInv
                  SCATTER MEMVAR MEMO
                  INSERT INTO (lcInstLin) FROM MEMVAR
                ENDSCAN 
              ENDIF 
      
              IF SEEK (&lcFltFile..cSadInv,'SDINSTMH') 
                SELECT SDINSTMH
                SCAN REST WHILE cSadInv=&lcFltFile..cSadInv
                  SCATTER MEMVAR MEMO
                  INSERT INTO (lcInstHdr) FROM MEMVAR
                ENDSCAN 
              ENDIF 
              
              *++nad
              
              lcAls=Alias()
              lnrec =recno()
              =lfReClcShp()
              
              select (lcinvHdr)
              locate
              SELECT(lcAls)
              
              *++nad
         
            ELSE

              *B119562,1 Display this message once per Account. [Begin]
              *=gfModalGen('INM00000B00000','ALERT','','', "Account " + InvHdrs.Account +' is consolidated account you can not select more than one invoice for consolidated customer at a time Invoice # '+invhdrs.csadinv +' will be ignored')
              IF !SEEK(InvHdrs.Account,lcTmpAccnt)
                INSERT INTO (lcTmpAccnt) VALUES (InvHdrs.Account)
                =gfModalGen('INM00000B00000','ALERT','','', "Account " + InvHdrs.Account +' has consolidated Invoice, you can not select more than one invoice for this customer.')
              ENDIF
              *B119562,1 Display this message once per Account. [End]

            ENDIF
          ELSE
             =gfModalGen('INM00000B00000','ALERT','','', InvHdrs.CsadInv +' is edited by another user Invoice # '+invhdrs.csadinv +' will be ignored')
          ENDIF
        ENDIF 
      ENDSCAN
      IF EOF(lcInvHdr)
         =gfModalGen('INM00000B00000','ALERT','','', 'No records to Display.')
         glQuitting=.T.
         CLEAR READ
         RETURN 
      ENDIF
      WAIT CLEAR
    ELSE
      =gfModalGen('INM00000B00000','ALERT','','', 'You Have to select Invoice range')
      glQuitting=.T.
      CLEAR READ
      RETURN
    ENDIF  
  
  ELSE
    glQuitting=.T.
    CLEAR READ
    RETURN  
  ENDIF

ELSE 

  llContin = .F.
  STORE IIF(laScrMode[1],'ENABLE','DISABLE') TO lcScopeStat
  SELECT (lcInvLine)
  =SEEK(laData[4]+laData[1]+laData[5]+laData[3])
  SCATTER MEMVAR MEMO
  SELECT (lcInvHdr)
  =SEEK(laData[4]+laData[1]+laData[5]+laData[3])
  SCATTER MEMVAR FIELDS &lcFlFields
  SCATTER FIELDS &lcScFields TO laData

ENDIF



llInvChrg = llIsEngland .AND. gfOpenFile(gcDataDir+'InvChrg',gcDataDir+'InvChrg','SH')
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

DECLARE laPanelObj[1,3]
laPanelObj[1,1] = 'pbScope'
laPanelObj[1,2] = gcBmpHome + 'SCOPE.BMP'
laPanelObj[1,3] = [VALID lfSetScope() MESSAGE 'Scope Orders' &lcScopeStat ]

llOpnFiles=.T.


PUSH MENU _MSYSMENU
lnBrowse = lfGtSavBar('GFCPBROW')
lnFilter = lfGtSavBar('GFSETFILTR')
 
GO TOP IN (lcInVHdr)

*B119562,1 SSE 09/08/2003 Disable ESC push button. [Begin]
*DO (gcScrDir+gcWinAppl+"\ARIINV.SPX")

PUSH KEY CLEAR
ON KEY LABEL ESC llDummy = .T.
DO (gcScrDir+gcWinAppl+"\ARIINV.SPX")
POP KEY
*B119562,1 SSE 09/08/2003 Disable ESC push button. [Begin]

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
  *B602179,1 AMM Erase temporary files when quitting the program
  IF USED(lcOrdCanLn)
    USE IN (lcOrdCanLn)
  ENDIF
  ERASE (gcWorkDir+lcOrdCanLn+".DBF")
  ERASE (gcWorkDir+lcOrdCanLn+".CDX")
  
  IF USED(lcConsInvH)
    USE IN (lcConsInvH)
  ENDIF
  ERASE (gcWorkDir+lcConsInvH+".DBF")
  ERASE (gcWorkDir+lcConsInvH+".CDX")

  IF USED(lcConsInvD)
    USE IN (lcConsInvD)
  ENDIF
  ERASE (gcWorkDir+lcConsInvD+".DBF")
  ERASE (gcWorkDir+lcConsInvD+".CDX")
  ERASE (gcWorkDir+lcConsInvD+".FPT")
  *B602179,1 AMM  end
  IF !TYPE(lcSadInvH) $'UL'
    IF USED(lcSadInvH)
      USE IN (lcSadInvH)
    ENDIF  
    ERASE (gcWorkDir+lcSadInvH+".DBF")
    ERASE (gcWorkDir+lcSadInvH+".CDX")

    IF USED(lcSadInvL)
      USE IN (lcSadInvL)
    ENDIF
    ERASE (gcWorkDir+lcSadInvL+".DBF")
    ERASE (gcWorkDir+lcSadInvL+".CDX")

    IF USED(lcSadUpsB)
      USE IN (lcSadUpsB)
    ENDIF
    ERASE (gcWorkDir+lcSadUpsB+".DBF")
    ERASE (gcWorkDir+lcSadUpsB+".CDX")

    IF USED(lcSadInsH)
      USE IN (lcSadInsH)
    ENDIF
    ERASE (gcWorkDir+lcSadInsH+".DBF")
    ERASE (gcWorkDir+lcSadInsH+".CDX")

    IF USED(lcSadInsL)
      USE IN (lcSadInsL)
    ENDIF
    ERASE (gcWorkDir+lcSadInsL+".DBF")
    ERASE (gcWorkDir+lcSadInsL+".CDX")

    IF USED(lcOrdCanLn)
      USE IN (lcOrdCanLn)
    ENDIF
    ERASE (gcWorkDir+lcOrdCanLn+".DBF")
    ERASE (gcWorkDir+lcOrdCanLn+".CDX")
 
  ENDIF

ENDIF


IF llInvChrg 
  USE IN InvChrg
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
*B802865,1 WAB - in case of conalidate dipslay field cConsStore else diplay field store
*lcFields = lcFields + "cOrd=IIF(Flag='O','******',Order) :H='Order#' :R,Custpo :H='Cust. P.O.' :R,Store :H='Store' :R,Invoice :H='Inv.#' :R,"
lcFields = lcFields + "cOrd=IIF(Flag='O','******',Order) :H='Order#' :R,Custpo :H='Cust. P.O.' :R,cstore=IIF(Consol='Y',cConStore,Store) :H='Store' :R,cSadInv :H='Inv.#' :R,"
*B802865,1 WAB - END
lcFields = lcFields + "Ordered :H='Order' :R, Ship :H= 'Ship' :R, ShipAmt :H='Ship Amnt.' :13 :R,Cartons :H='Cartons' :R,Weight :H='Weight' :R"
lcFields = lcFields + IIF('AL' $ gcCmpModules,",PikTkt :H='PikTkt' :R,Picked :H='Picked' :R",'')
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
SCATTER MEMVAR FIELDS &lcFlFields

*B603395,1 [Start] Refresh Sales Rep header commission 
IF !EOF() AND (laSetups[2,2] = 'Y')
  STORE 0 TO lnRep1Comm , lnRep2Comm
 *-- Refresh Sales Rep. header Commession For invoice
  =lfRefComm()
ENDIF
*B603395,1 [End  ]
*-- seek account+order+store+PickTkt
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

*B801954,1 Adjust select/ship invoice buttons.
*lcStatus=IIF(Consol <> 'Y' AND laScrMode[4],'ENABLE','DISABLE')
*SHOW GETS WINDOW (lcWinCh22) &lcStatus ONLY
*SHOW GET ibInvoice ENABLE
lcStatus=IIF(laScrMode[4],'ENABLE','DISABLE')
*B801954,1 (End)

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
*B801954,1 Adjust select/ship invoice buttons.
*lcStatus=IIF(Consol <> 'Y' AND laScrMode[4] AND cSelect <> SPACE(1) ,'ENABLE','DISABLE')
lcStatus=IIF(laScrMode[4] AND cSelect <> SPACE(1) ,'ENABLE','DISABLE')
*B801954,1 (End)

IF Ship = 0
  SHOW GET pbShipInv,1 &lcStatus PROMPT 'S\<hip'
ELSE
  SHOW GET pbShipInv,1 &lcStatus PROMPT 'Uns\<hip'
ENDIF

*E301226 (Start)
IF !EMPTY(laData[3]) AND lcUpdPik $ 'NA' 
  SHOW GET pbShipInv,1 DISABLE PROMPT 'Uns\<hip'
ENDIF
*E301226 (End)

lnCartons = nCartons
IF Consol='Y'

  *B603318,1 KHM 12/06/99  (Begin) Commenting the follwoing SQL command
  *B603318,1               in order to optimize it.
  *SELECT DIST IIF(EMPTY(DistCntr.Dba),DistCntr.StName,DistCntr.Dba),DistCntr.Store ;
  *FROM DistCntr,Customer WHERE ;
  *DistCntr.Type+DistCntr.Account+DistCntr.Store='S'+laData[4]+customer.Dist_Ctr ;
  *OR DistCntr.Type+DistCntr.Account+DistCntr.Store='M'+laData[4] ;
  *INTO ARRAY laShipName
  
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
    *B802865,1 WAB - declare the array defore copy cuse it give errore subscript out of bounds
    DECLARE laShipName[1,1]
    *B802865,1 WAB - END
    
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
  *B603318,1 KHM 12/06/99  (End)
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
lcFields = "cMarker=IIF(RECNO()=lnInvMark ,'>',' '):H=' ':R:1:W=.F.,Style :H=lcStyHdr :R,"
lcFields = lcFields + IIF(laSetups[8,2]='Y',"Dyelot :R,",'')
*B603506,1 ABD Increase amount sizes in Browse fields to fit currencies.
*lcFields = lcFields +IIF('AL' $ gcCmpModules,"","Group :H='Group' :R,")+;
           "TotBook :H='Ordered' :R,"+IIF('AL' $ gcCmpModules,"TotPik :H='Picked' :R,",'')+;
           "TotQty :H='Ship' :R,"+;
           "Gros_Price :H='Gross Price' :P='999999.99' :R,"+;
           "Disc_Pcnt :H='Disc. Pcnt.' :P='999.99' :R,Price :H='Net Price' :P='999999.99' :R,"+;
           "lnAmount=PRICE*TotQty :H='Ship Amount':R:P='9999999.99'"           

*B603713,5 10/01/2000 NAD (Start) Increase the picture of the gross price and the net price and shipamt.
*lcFields = lcFields +IIF('AL' $ gcCmpModules,"","Group :H='Group' :R,")+;
           "TotBook :H='Ordered' :R,"+IIF('AL' $ gcCmpModules,"TotPik :H='Picked' :R,",'')+;
           "TotQty :H='Ship' :R,"+;
           "Gros_Price :H='Gross Price' :P='999999.99' :R,"+;
           "Disc_Pcnt :H='Disc. Pcnt.' :P='999.99' :R,Price :H='Net Price' :P='999999.99' :R,"+;
           "lnAmount=PRICE*TotQty :H='Ship Amount':R:P='9999999999.99'"

lcFields = lcFields +IIF('AL' $ gcCmpModules,"","Group :H='Group' :R,")+;
           "TotBook :H='Ordered' :R,"+IIF('AL' $ gcCmpModules,"TotPik :H='Picked' :R,",'')+;
           "TotQty :H='Ship' :R,"+;
           "Gros_Price :H='Gross Price' :P='999999999.99' :R,"+;
           "Disc_Pcnt :H='Disc. Pcnt.' :P='999.99' :R,Price :H='Net Price' :P='999999999.99' :R,"+;
           "lnAmount=PRICE*TotQty :H='Ship Amount':R:P='99999999999.99'"

*B603713,5 NAD (End)
*B603506,1 ABD [End]

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
  *B802865,1 WAB - add condition to browse only when temp invhdr.consol = temp inline.consol
  *BROWSE FIELDS &lcFields ;
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
  BROWSE FIELDS &lcFields ;
         FOR Consol = &lcInvHdr..Consol;
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
  *B802865,1 WAB - END
ELSE
  SHOW WINDOW (lcWinCh42),(lcWinCh44),(lcWinCh45),(lcWinCh46) TOP
  *B802865,1 WAB - add condition to browse only when temp invhdr.consol = temp inline.consol
  *BROWSE FIELDS &lcFields ;
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
  BROWSE FIELDS &lcFields ;
         FOR Consol = &lcInvHdr..Consol;
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
  *B802865,1 WAB - END
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

*B603118 (Start)
*SHOW GETS WINDOW (lcWinCh42) ONLY
*SHOW GETS WINDOW (lcWinCh44) ONLY
*SHOW GETS WINDOW (lcWinCh45) ONLY
*SHOW GETS WINDOW (lcWinCh46) ONLY

*B603555,1 (Start) Show get windows upon a condition
*SHOW GETS WINDOW (lcWinCh42) ENABLE ONLY
*SHOW GETS WINDOW (lcWinCh44) ENABLE ONLY
*SHOW GETS WINDOW (lcWinCh45) ENABLE ONLY
*SHOW GETS WINDOW (lcWinCh46) ENABLE ONLY
*B603118 (End)

lcStatus=IIF(&lcInvHdr..Consol <> 'Y','ENABLE','DISABLE')
SHOW GETS WINDOW (lcWinCh42) &lcStatus ONLY
SHOW GETS WINDOW (lcWinCh44) &lcStatus ONLY
SHOW GETS WINDOW (lcWinCh45) &lcStatus ONLY
SHOW GETS WINDOW (lcWinCh46) &lcStatus ONLY
*B603555,1 (End)

lcLineStat = IIF(llZoom .OR. &lcInvHdr..Consol='Y' OR lNewLine OR laScrMode[1],'DISABLE','ENABLE')
IF m.TotQty = 0
  SHOW GET pbShipLine,1 &lcLineStat PROMPT '\<Ship'
ELSE
  SHOW GET pbShipLine,1 &lcLineStat PROMPT '\<Clear'
ENDIF

*E300956,1 Keep history of order line calceled quantity.
llCnRemain = !m.LBACKORD AND !m.lNewLine
IF lcBackOrd = 'I' AND ;
  (m.Book1 > m.Qty1 OR m.Book2 > m.Qty2 OR m.Book3 > m.Qty3 OR m.Book4 > m.Qty5 OR ;
   m.Book5 > m.Qty5 OR m.Book6 > m.Qty6 OR m.Book7 > m.Qty7 OR m.Book8 > m.Qty8)
  SHOW GET llCnRemain &lcLineStat
ELSE
  SHOW GET llCnRemain DISABLE
ENDIF
IF SEEK('O'+Order+STR(LineNo,6),lcOrdCanLn)
  SHOW GET pbCnRemain &lcLineStat
ELSE
  SHOW GET pbCnRemain DISABLE
ENDIF
*E300956,1 (End)

*B603118 (Start)
*lcLineStat = IIF(llZoom .OR. &lcInvHdr..Consol='Y' OR !lNewLine,'DISABLE','ENABLE')
lcLineStat = IIF(llZoom .OR. &lcInvHdr..Consol='Y' OR !m.lNewLine,'DISABLE','ENABLE')
*B603118 (End)

SHOW GET pbRemove &lcLineStat
=lfRefresh(lcWinCh42)
=lfRefresh(lcWinCh44)
=lfRefresh(lcWinCh46)

*E301226 (Start)
IF !EMPTY(laData[3])
*-- Not empty PikTkt
  DO CASE    && Modify Picking upon invoice Cases
    CASE lcUpdPik = 'A'   && Add
      SHOW GET pbNew ENABLE
      IF !&lcInvLine..lNewLine
        SHOW GET m.Qty1 DISABLE
        SHOW GET m.Qty2 DISABLE
        SHOW GET m.Qty3 DISABLE
        SHOW GET m.Qty4 DISABLE
        SHOW GET m.Qty5 DISABLE
        SHOW GET m.Qty6 DISABLE
        SHOW GET m.Qty7 DISABLE
        SHOW GET m.Qty8 DISABLE
      ENDIF
      
    CASE lcUpdPik = 'N'     && No
      SHOW GET pbNew DISABLE
      SHOW GET pbShipLine,1 DISABLE PROMPT '\<Ship'
      SHOW GET m.Qty1 DISABLE
      SHOW GET m.Qty2 DISABLE
      SHOW GET m.Qty3 DISABLE
      SHOW GET m.Qty4 DISABLE
      SHOW GET m.Qty5 DISABLE
      SHOW GET m.Qty6 DISABLE
      SHOW GET m.Qty7 DISABLE
      SHOW GET m.Qty8 DISABLE
  
  
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
*E301226 (End)

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
*B801947,1 adjust invoice lines browse
IF lnActFolder = 3
  SET KEY TO laData[4]+laData[1]+laData[5]+laData[3]
ELSE
  SET KEY TO
ENDIF
*B801947,1 (End)

DO CASE
  CASE lnActFolder = 1
    *B803634,1 NAD 09/03/2000 (Start) Call the function to recalculate shipped quantity & amount. 
    =lfReClcShp(.T.)
    *B803634,1 NAD  (End)
    lcStatus = IIF(laScrMode[4],'ENABLE','DISABLE')
    SHOW GETS WINDOW (lcWinCh3)  DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh40) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh42) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh44) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh45) DISABLE ONLY
    SHOW GETS WINDOW (lcWinChrg) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh22) &lcStatus ONLY

    *C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
    IF ASCAN(laEvntTrig , PADR('UPDATE',10)) <> 0
      IF RECCOUNT(LCINVHDR) > 0 AND RECCOUNT(LCJLCHARGS) = 0
        =gfDoTriger('ARIINV',PADR('UPDATE',10))
      ENDIF
    ENDIF
    *C102314,1 BWA 06/10/2001 [END]

    *B603118 (Start)
    =lfwOrdBrow()
    *B603118 (End)
    *--To handle Select, Unselect, Select all, Select None and Invert invoices
    
    
    
    =lfAdjSButt('')
    *E301226 (Start)
    IF !EMPTY(laData[3]) AND lcUpdPik $ 'NA'
      SHOW GET pbShipInv,1 DISABLE PROMPT 'Uns\<hip'
    ENDIF
    *E301226 (End)

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
    =gfwCodePop(@laCodes,'SEASON','T')
    =gfwCodePop(@laCodes,'CDIVISION','T')
    SHOW GETS WINDOW (lcWinCh3) &lcStatus ONLY
    =lfGetAddInfo()
    SHOW GET lnSeason    DISABLE
    SHOW GET lnDivision  DISABLE
    SHOW GET lnWareHouse DISABLE
    *B802865,1 WAB - Shipvia popup must be diable in case of the consolidate 
    *B802865,1 WAB - ship via is all
    *IF laScrMode[4] AND Consol <> 'Y'

    *B803060,1 ABD If Statement to cheak if you have more than one recored,
    *B803060,1 ABD Change ship via popup layout. [ Begin ]
    IF Consol = 'Y'
      laCodes[2,5]  = .T.
    ELSE 
      laCodes[2,5]  = .F.
    ENDIF
    *B803060,1 ABD [ END ]
    =gfwCodePop(@laCodes,'SHIPVIA','L')
    *-- change elemnt (All) in shipvia popup to be (Multi)
    =lfModShpAr()
    IF laScrMode[4] AND (Consol = 'Y' OR lfChekCons())
    *B802865,1 WAB - END
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
    lcSalesStat = IIF(laScrMode[4] AND laSetups[2,2]<>'Y' AND ;
                  IIF(Customer.Consol='Y',CONSOL='Y',.T.),'ENABLE','DISABLE')
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
      *B603118 (Start)
      *lcStatus = IIF(&lcInvHdr..Ship=0,'DISABLE','ENABLE')
      *SHOW GETS WINDOW (lcWinCh42) &lcStatus ONLY
      *SHOW GETS WINDOW (lcWinCh44) &lcStatus ONLY
      *SHOW GETS WINDOW (lcWinCh45) &lcStatus ONLY
      *SHOW GETS WINDOW (lcWinCh46) &lcStatus ONLY
      *B603118 (End)
      *B603555 (Start) Enable windows in case of not conslidated       
      SHOW GETS WINDOW (lcWinCh42) ENABLED ONLY
      SHOW GETS WINDOW (lcWinCh44) ENABLED ONLY
      SHOW GETS WINDOW (lcWinCh45) ENABLED ONLY
      SHOW GETS WINDOW (lcWinCh46) ENABLED ONLY
      *B603555 (End)
      IF laScrMode[4]
        SHOW GET pbNew ENABLE 
      ELSE
        SHOW GET pbNew DISABLE
      ENDIF
      IF laScrMode[4] .AND. !EMPTY(Style)
        SHOW GET pbRemove ENABLE
      ELSE
        SHOW GET pbRemove DISABLE
      ENDIF
      IF laSetups[3,2]='Y' .AND. !EMPTY(Style)
        SHOW GET pbLineNote ENABLE
      ELSE
        SHOW GET pbLineNote DISABLE
      ENDIF
    ENDIF
    =lfwBrow()
    _CUROBJ = OBJNUM(ibInv200E)
  CASE lnActFolder = 4

    *B803634,1 NAD 09/03/2000 (Start) Call the function to recalculate shipped quantity & amount. 
    =lfReClcShp(.T.)
    *B803634,1 NAD  (End)

    SELECT (lcInvHdr)
    llCod = (m.Cod_Flag='Y')
    *C102239,1 (Begin) Call standard UPS charge function for all customers except Bel05.
    *IF laScrMode[4]
    * =IIF(!llIsEngland .AND. lCompUps,lfvUpsChrg(),.T.)
    *ENDIF

    *C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
    IF ASCAN(laEvntTrig , PADR('SUMALL',10)) <> 0
      llUpsFlgJl = .F.
    ENDIF
    *C102314,1 BWA 06/10/2001 [END]

     IF laScrMode[4] AND !llIsEngland AND lCompUps
      IF !EMPTY(m.Piktkt) AND ASCAN(laEvntTrig , PADR('CRTCHRG',10)) <> 0
        llCalld   = .F.
        lcChrgTyp = ""
        =gfDoTriger('ARIINV',PADR('CRTCHRG',10))
      ELSE
        llUpsFlgJl = .T.
        =lfvUpsChrg()
      ENDIF  
    ENDIF 

    *C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
    IF ASCAN(laEvntTrig , PADR('SUMALL',10)) <> 0 AND laScrMode[4] AND !llUpsFlgJl
      llGetin = .T.
      =gfDoTriger('ARIINV',PADR('SUMALL',10))
    ENDIF
    *C102314,1 BWA 06/10/2001 [END]

    *C102239,1 (End)
    =gfRltFld(&lcInvHdr..ShipVia,@laVRltFld,'SHIPVIA')
    lcUpsTrack  = IIF(SUBSTR(lcUpsType,1,5)='USUPS','UPS COD Tracking#','COD Tracking#')
    lcUpsPrompt = IIF(SUBSTR(lcUpsType,1,5)='USUPS','UPS Insur.','Insurance ')
    llOpnCrdt = .F.
    IF laScrMode[4] AND llCod AND m.TotalChg > 0 AND (Consol='Y' OR EMPTY(Flag))
      *E301077,50 Inhance openning files to speed up transaction
      *SELECT CREDIT
      *=SEEK(laData[4])
      *LOCATE REST WHILE Account+Tran+DTOS(TranDate) = laData[4] FOR ;
      *cCurrCode = laData[14]
      *llOpnCrdt = FOUND()
      DO gfAcOpCrdt IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
                       laData[4],laData[14],'llOpnCrdt'
      *E301077,50 (End)
      SHOW GET m.Cod_Amt ENABLE
    ELSE
      SHOW GET m.Cod_Amt DISABLE
    ENDI


    =lfRefresh(lcWinChrg)
    lcStatus = IIF(laScrMode[4] AND m.ShipAmt > 0 AND Consol <> 'Y','ENABLE','DISABLE')
    SHOW GET m.Cartons &lcStatus
    SHOW GET m.Weight  &lcStatus
    SHOW GET m.Freight &lcStatus
    SHOW GET m.Insur   &lcStatus
    SHOW GET m.Cod     &lcStatus

    *B801953,1 (Start)  If UPS type not equal USUPS Disable the UPSBOX Push buttom
    *SHOW GET pbUpsBox  &lcStatus
    lcUpsSt= IIF(laScrMode[4] AND m.ShipAmt > 0 AND Consol <> 'Y' AND SUBSTR(lcUpsType,1,5)= "USUPS",'ENABLE','DISABLE')
    SHOW GET pbUpsBox  &lcUpsSt
    *B801953,1 (End)
    *C102239,1 (Begin) Always enable Cartns button for BEL05.
    IF !EMPTY(laData[3]) AND ASCAN(laEvntTrig , PADR('ENBLCRT',10)) <> 0
      llCalld   = .F.
      lcChrgTyp = ""
      =gfDoTriger('ARIINV',PADR('ENBLCRT',10))
    ENDIF  
    *C102239,1 (End)
   
    SHOW GET pbCharges &lcStatus
    SHOW GET m.DISCPCNT   &lcStatus
    SHOW GET m.DISCOUNT   &lcStatus
    SHOW GET lnpstrule    &lcStatus
    SHOW GET m.nPstRate   &lcStatus
    SHOW GET m.nPstAmt    &lcStatus
    SHOW GET m.Tax_Rate   &lcStatus
    *B602760,1 NAD Commented      
    *IF !(laScrMode[4] AND UPPER(ALLTRIM(gcContCode))='USA' AND Consol <> 'Y' ;
       AND &lcInvHdr..nTaxDue > 0 )
    *  SHOW GET m.TAX_AMT DISABLE
    *ELSE
      *SHOW GET m.TAX_AMT &lcStatus
    *ENDIF
    *B602760,1 NAD (End)
    *C102212,1 (Begin) Show get HST tax fileds if Use Tax 'Y'.
    SHOW GET m.nHstRate    &lcStatus
    SHOW GET m.nHstAmt     &lcStatus
    *C102212,1 (End)

    SHOW GET m.TAX_AMT &lcStatus
    lcStatus = IIF(laScrMode[4] AND m.ShipAmt > 0 AND (Consol='Y' OR EMPTY(Flag)),'ENABLE','DISABLE')
    SHOW GET m.BOL_NO     &lcStatus
    SHOW GET m.CODTAG     &lcStatus
    SHOW GET m.CCODTRCKNO &lcStatus
    SHOW GET llCod        &lcStatus
    SHOW GET m.lUpsIns,1  &lcStatus PROMPT lcUpsPrompt 
    SHOW GET m.TRDE_DISC  &lcStatus
    =gfRltFld(m.cTermCode,@laTRltFld,'CTERMCODE')
    
    *C200088,1 Calculate Due date corresponding to FP Requirements [Begin]
    IF ASCAN(laEvntTrig,"WHATISDUED") <> 0
      = gfDoTriger('ARIINV','WHATISDUED')
    ENDIF
    *C200088,1 Calculate Due date corresponding to FP Requirements [End  ]
    
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
*B802865,1 WAB - if it is a consalidate line replace store value into field
*B802865,1 WAB - cConStore, otherwise replace into field store
*REPLACE STORE WITH laData[5]
PRIVATE lcField
lcField = IIF(Consol = 'Y','cConStore','Store')
REPLACE &lcField WITH laData[5]
*B802865,1 WAB - END
UNLOCK
SELECT (lcInvLine)
SET ORDER TO TAG 'CONSOL'
=SEEK('Y')
*B802865,1 WAB - replace store value into field cConStore
*REPLACE REST STORE WITH laData[5] WHILE ;
             Consol+Account+cDivision+cCurrCode+Style = 'Y'
REPLACE REST cConStore WITH laData[5] WHILE ;
             Consol+Account+cDivision+cCurrCode+Style = 'Y'             
*B802865,1 WAB - END 
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
  =SEEK('O'+laData[1],'OrdHdr')
  IF OrdHdr.Alt_ShpTo
    *-- Alternate Ship to Address
    lcShipName = OrdHdr.StName
    lcShipAdd1 = OrdHdr.cAddress1
    lcShipAdd2 = OrdHdr.cAddress2
    lcShipAdd3 = OrdHdr.cAddress3
    lcShipAdd4 = OrdHdr.cAddress4
    lcShipAdd5 = OrdHdr.cAddress5
  ELSE
    *-- Customers ship to name or customer do business as name
    lcShipName =  IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
    *B802241 (Start)
    lnCusRecNo = RECNO('Customer')    
    IF !EMPTY(Customer.Dist_Ctr)
      lcDistCntr = Customer.Dist_Ctr   && Customer distribution center
      llNoThing = SEEK('S'+laData[4]+lcDistCntr,'CUSTOMER')
    ENDIF
    IF !llNoThing AND RECCOUNT('Customer') >= lnCusRecNo
      GO lnCusRecNo IN Customer
    ENDIF
    *B802241 (End)
    =gfGetAdr('CUSTOMER','','','',1,'')
    FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount = STR(laAddress[lnCount,1],1)
      lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    ENDFOR  
    IF RECCOUNT('Customer') >= lnCusRecNo
      GO lnCusRecNo IN Customer
    ENDIF
    *B802241 (End)

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

SELECT (lcFltFile)
SCAN
  IF SEEK (&lcFltFile..cSadInv,'INVHDRS')
    SELECT INVHDRS
    REPLACE REST lLok_Stat WITH .F. WHILE  cSadInv = &lcFltFile..cSadInv
  ENDIF
ENDSCAN  
SELECT unCmSess

*B803747,1 NAD (Start) Be sure that we are in the correct record before updating.
IF SEEK('O'+PADR('IINVOICE',10)+PADR(gcUser_id,10)+lcSession)
  REPLACE STATUS WITH 'I'
ENDIF
*B803747,1 NAD (End)
llContinue = .F.
UNLOCK
SELECT (lcInvHdr)
glQuitting=.T.
CLEAR READ
RETURN
   


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
  *E300817,1 Message : 40032
  *E300817,1 Must enter a default invoice date.
  *E300817,1 Button : 00000
  *E300817,1 Ok
  =gfModalGen('INM40032B00000','ALERT','invoice')
  _CUROBJ = OBJNUM(ldDefInvDate)
  RETURN
ENDIF
IF laSetups[4,2] = 'Y' .AND. EMPTY(ldDefPstDate)
  *E300817,1 Message : 40032
  *E300817,1 Must enter a default posting date.
  *E300817,1 Button : 00000
  *E300817,1 Ok
  =gfModalGen('INM40032B00000','ALERT','posting')
  _CUROBJ = OBJNUM(ldDefPstDate) 
  RETURN
ENDIF
IF laSetups[4,2] = 'Y' .AND. ldDefInvDate > ldDefPstDate
  *E300817,1 Message : 40086
  *E300817,1 invoice date cannot be greater than posting date
  *E300817,1 Button : 00000 
  *E300817,1 Ok
  =gfModalGen('TRM40086B00000','ALERT','')
  _CUROBJ = OBJNUM(ldDefPstDate) 
  RETURN
ENDIF
IF laSetups[4,2]='Y' AND !CHECKPRD(ldDefPstDate,'lcGlYear','lcGlPeriod','IN')
  _CUROBJ = OBJNUM(ldDefPstDate) 
  RETURN
ENDIF

*B605016,1 BWA 10/14/2001 Fix the bug of updating the dates.[START]
=lfUpdateSd()
*B605016,1.[END]

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

SHOW GET pbBrws DISABLE
DEFINE BAR lnBrowse OF P03PU03 PROMPT 'Fil\<ter' SKIP FOR .T.
DEFINE BAR lnFilter OF P03PU03 PROMPT '\<Browse' SKIP FOR .T.
DO CASE

lcSydKey=lcSvKey

  CASE laScrMode[1]
    lnLastMode = 1
    
    STORE 0 TO laQtyPaste
    SHOW GET pbScope ENABLE
    lcStatColor = gcObjColor
    STORE '' TO lcOrdStat
    llCancel = .F.

    *sameh
    *IF EMPTY(ldDefInvDate) .OR. EMPTY(ldDefPstDate)
    *  STORE gdSysDate TO ldDefInvDate,ldDefPstDate      
    *  DO (gcScrDir+gcWinAppl+"\ARINVSES.SPX")      
    *ENDIF
    
    STORE .T. TO laScrMode[4]
    STORE .F. TO laScrMode[1]
    STORE .F. TO laScrMode[2]
    STORE .F. TO laScrMode[3]
    GO TOP IN (lcInvHdr)
    
    SHOW GETS
    
    *B801942,1 Add the ability to cancel session defaults and terminate program
    IF llCancel
      SELECT (lcFltFile) 
        SCAN
          IF SEEK (&lcFltFile..cSadInv,'INVHDRS')
            SELECT INVHDRS
            REPLACE REST lLok_Stat WITH .F. WHILE  cSadInv = &lcFltFile..cSadInv
          ENDIF
        ENDSCAN   
      glQuitting=.T.
      CLEAR READ
      RETURN
    ENDIF
    *B801942,1 (End)
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
  CASE laScrMode[4]   && Add mode
    IF lnLastMode <> 4
    
      
      IF llOpnfiles  
        =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
        =gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')
      ENDIF
      
      
   llCUpdate  = .T.
    llGoAndChk = .T.
    SELECT (lcInvHdr)
    *B603695,1 (Start)   Commented out (not to change the current record)
    *GO TOP
    *B603695,1 (End) 
    SHOW GET pbScope   DISABLE
    SHOW GET ibStore   DISABLE
    SHOW GET laData[5] DISABLE    
    =lfActFolder()
    =lfwOrdBrow()
 ENDIF
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
PRIVATE XOrder,XAccount,XSTORE

IF llBrowse .OR. (!EMPTY(laData[1]) .AND. !SEEK('O'+laData[1],'OrdHdr'))
  XAccount = laData[4]
  XORDER   = laData[1]
  XSTORE   = laData[5]
  llBrowse = .T.
  laData[1] = IIF(IIF(EMPTY(XAccount),ORDBROWO(@XORDER, .T., 'O'),ORDBROWA(XAccount,.T.,'O')),XORDER,'')
ENDIF
llBrowse = .F.
IF !EMPTY(laData[1]) 
  laData[4] = OrdHdr.Account
  IF 'AL' $ gcCmpModules AND !SEEK(laData[1],'PIKTKT')    && no pick ticket for that order
    SHOW GET ibPikTkt  DISABLE
    SHOW GET laData[3] DISABLE
  ENDIF
  SHOW GET ibAccount DISABLE
  SHOW GET laData[4] DISABLE
  SHOW GET ibStore   ENABLE
  SHOW GET laData[5] ENABLE
  
  IF OrdHdr.Multi <> 'Y' .AND. ;
    (!('AL' $ gcCmpModules) .OR. !SEEK(laData[1],'PIKTKT'))
    *-- not multi store and no pick tickets
    IF lfGetOrder(laData[1],OrdHdr.Store,.F.,'','')
      *-- Add mode
      STORE .F. TO laScrMode
      STORE .T. TO laScrMode[4]
      GO TOP IN (lcInvHdr)
     
      SHOW GETS
    ELSE
      *-- clear the screen 
      =lfInitSel()
    ENDIF
  ENDIF
ENDIF

*-- end of lfvOrder.

*************************************
FUNCTION lfInitSel
STORE SPACE(6)  TO laData[1],laData[3]
STORE SPACE(15) TO laData[2]
STORE SPACE(5)  TO laData[4]
STORE SPACE(8)  TO laData[5]
SHOW GET ibOrder   ENABLE
SHOW GET laData[1] ENABLE
SHOW GET laData[2] ENABLE
SHOW GET ibPikTkt  ENABLE
SHOW GET laData[3] ENABLE
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
PRIVATE xStore,xAccount

*B603552,1 (Start) Not to disable the store push button Preventing bug alias not found  
IF !llBrowse AND MDOWN()
  RETURN
ENDIF
*B603552,1 (End)

IF llBrowse .OR. !EMPTY(laData[1]) .OR. ;
   (!EMPTY(laData[5]) .AND. !SEEK('S'+laData[4]+laData[5],'CUSTOMER'))

  IF EMPTY(laData[1])   
    xStore = laData[5]
    SELECT CUSTOMER
    =CUSBROWS(laData[4],.T.)
    laData[5] = xStore
  ELSE
    SET ORDER TO TAG ORDLINST IN ORDLINE
    IF llBrowse OR (!EMPTY(laData[5]) .AND. !SEEK('O'+laData[1]+laData[5],'ORDLINE'))
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
      SET RELATION TO 'O'+laData[1]+Store INTO ORDLINE
      laData[5]=IIF(ARIABROW("'S'+laData[4] FOR !EOF('ORDLINE')",;
                "Locations for Customer : "+laData[4],gnBrFSRow1, gnBrFSCol1,;
                 gnBrFSRow2, gnBrFSCol2,'','','STORE',"laBrowArr",.F.),Customer.Store,SPACE(8))
      SET RELATION OFF INTO ORDLINE
      =gfCloseFile('SycInt')
    ENDIF
    IF lfGetOrder(laData[1],laData[5],.F.,'','')
    
      STORE .F. TO laScrMode
      STORE .T. TO laScrMode[4]
      GO TOP IN (lcInvHdr)
         
      SHOW GETS
    ELSE
      =lfInitSel()
    ENDIF
  ENDIF
  llBrowse = .F.
ENDIF  

*!*************************************************************
*! Name      : lfvPikTkt
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1998
*! Purpose   : Validate Pick Ticket number
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvPikTkt()
*!*************************************************************
FUNCTION lfvPikTkt
PRIVATE lcBrFields,lcFile_Ttl
*B603342,1 WAB - do not excute the validation twice if we pushed the brwose 
*B603342,1 WAB - botton and the piktkt is the cuurent field the field piktkt 
IF MDOWN()
  RETURN
ENDIF
*B603342,1 WAB - END
IF !EMPTY(laData[1])
  SET ORDER TO TAG Ordpik IN PIKTKT
ELSE
  SET ORDER TO TAG PIKTKT IN PIKTKT
ENDIF
*B602543,1 Invoice all order pick tickets when the user does not select a specific pick ticket
*IF llBrowse .OR. ( (!EMPTY(laData[1]) .OR. !EMPTY(laData[3])) .AND. ;
   (!SEEK(ALLTRIM(laData[1])+laData[3],'PikTkt') OR (!EMPTY(laData[3]) AND PikTkt.Status='X')))
*B603022,1 MAN 07/20/1999 Check For Completed Piktkt    
*IF llBrowse .OR. ( !EMPTY(laData[3]) AND ;
     (!SEEK(ALLTRIM(laData[1])+laData[3],'PikTkt') OR PikTkt.Status='X') )
 IF llBrowse .OR. ( !EMPTY(laData[3]) AND ;
     (!SEEK(ALLTRIM(laData[1])+laData[3],'PikTkt') OR PikTkt.Status$'CX') )
*B602543,1 (End)

  SELECT PikTkt
  SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
  SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
  lcBrFields  = [PikTkt:H='PikTkt',]+;
                [Order:H='Order#',]+;
                [Store:H='Store',]+;
                [OrdHdr.Complete:H='Complete',]+;
                [Date:H='PikDate',]+;
                [Account:H='Account',]+;
                [Customer.BtName:H='Account Name']
  lcFile_Ttl = 'Pick Tickets'
  *B603022,1 MAN 07/20/1999 Check For Completed Piktkt
  *laData[3]=IIF(AriaBrow("ALLTRIM(laData[1]) FOR Status <> 'X'",lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,;
                         gnBrFSCol2,'','','PikTkt','laBrowArr'),PikTkt.PikTkt,'')
  laData[3]=IIF(AriaBrow("ALLTRIM(laData[1]) FOR !(Status $ 'CX')",lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,;
                         gnBrFSCol2,'','','PikTkt','laBrowArr'),PikTkt.PikTkt,'')                         
  SELECT PikTkt
  SET RELATION OFF INTO ORDHDR
  SET RELATION OFF INTO CUSTOMER
ENDIF
SET ORDER TO TAG Ordpik IN PIKTKT
llBrowse = .F.
IF !EMPTY(laData[3]) .AND. SEEK('O'+PikTkt.Order,'ORDHDR')
  *-- check if the order is valid or not
  IF lfGetOrder(PikTkt.Order,PikTkt.Store,.T.,laData[3],laData[3])
    STORE .F. TO laScrMode
    STORE .T. TO laScrMode[4]
    GO TOP IN (lcInvHdr)
    SHOW GETS
  ELSE
    *--clears the screen   
    =lfInitSel()
  ENDIF  
  RETURN
ENDIF
*-- if empty piktkt seek for the order number
IF EMPTY(laData[3]) .AND. !EMPTY(laData[1]) .AND. SEEK(laData[1],'PIKTKT') .AND. ;
  SEEK('O'+laData[1],'ORDHDR') 
  IF lfGetOrder(laData[1],laData[5],.T.,'','')
    STORE .F. TO laScrMode
    STORE .T. TO laScrMode[4]
    GO TOP IN (lcInvHdr)
    SHOW GETS
  ELSE
    =lfInitSel()
  ENDIF
  RETURN
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

ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+S 
ON KEY LABEL ALT+B
ON KEY LABEL ALT+A 
ON KEY LABEL ALT+N 
ON KEY LABEL ALT+V 
ON KEY LABEL ALT+H 

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
  _CUROBJ = OBJNUM(m.Qty1)
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
  IIF(WONTOP()=lcOrdBrow,'pbSelect',IIF(WONTOP()=lcInvBrow,'m.Style','pbTop'))
  ON KEY LABEL BACKTAB DO lpBackTab WITH lcFolder,IIF(WONTOP()=lcOrdBrow,'ibFolder[1]','ibFolder[3]')
ELSE

  *C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
  IF ASCAN(laEvntTrig , PADR('CHECK',10)) <> 0 AND llChangJL
    =lfrefresh(lcWinChrg)
  ENDIF
  *C102314,1 BWA 06/10/2001 [END]

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
lcKey = Account+Order+Store+PikTkt




DO CASE 

  CASE lcType = 'S'   &&  Select/UnSelect
    REPLACE cSelect WITH IIF(cSelect=SPACE(1),"�",SPACE(1))
    IF IIF(EMPTY(cSelect),Ship>0,Ship=0)
      SCATTER FIELDS &lcScFields TO laData
      SCATTER MEMVAR FIELDS &lcFlFields
      llUpCnsLine = Consol <> 'Y' and Flag $ 'OA'
      =lfvShipInv(.T.)
      *B603068 (Start)
      =lfwOrdBrow()
      *B603068 (End)
    ENDIF
  CASE INLIST(lcType,'A','N','V')
    SCAN FOR cSelect = IIF(lcType='A',' ',IIF(lcType='N',"�",''))
      REPLACE cSelect WITH IIF(lcType='A',"�",IIF(lcType='N',"",;
                           IIF(EMPTY(cSelect),"�",'')))
      IF Consol <> 'Y'
        SCATTER FIELDS &lcScFields TO laData
        SCATTER MEMVAR FIELDS &lcFlFields
        llUpCnsLine = Flag $ 'OA'
        *B803822,1 NAD 11/26/2000 (Start)Added Parameter to pass the button type.
        *=lfvShipInv()
        =lfvShipInv(.F.,lcType)
        *B803822,1 NAD 11/26/2000 (End)
      ENDIF
    ENDSCAN
  OTHERWISE
ENDCASE
*B801954,1 (End)

SET ORDER TO TAG 'SELECT'
=SEEK("�")
LOCATE REST WHILE cSelect="�" FOR Consol <> 'Y'
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
*! Calls     : ARINVSCP.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfSetFilter()
*!*************************************************************
FUNCTION lfSetScope

PRIVATE lcFromOrd,lcToOrd,lcFromPick,lcToPick,lcPriority,lnSeason,lnDivision,;
        ldFromStart,ldToStart,ldFromComp,ldToComp,ldFromPick,ldToPick,;
        cbPickedOrd,cbStyles,cbColors,cbCustomers,rbStatus,llColors,;
        laStySourc,laClrSourc,laCusSourc,laStyTarge,laClrTarge,laCusTarge

DIMENSION laScope[2,10],laStySourc[1],laClrSourc[1],laCusSourc[1],laStyTarge[1],;
          laClrTarge[1],laCusTarge[1],laItemSeg[1],laSeasons[1,2],laDivision[1,2]
STORE ''  TO laScope,laStySourc,laClrSourc,laCusSourc,laStyTarge,laClrTarge,;
             laCusTarge,laSeasons,laDivision

*E301249,1 Make priority field C(3) [start]
*STORE ' ' TO lcFromOrd,lcToOrd,lcFromPick,lcToPick,lcPriority
STORE ' ' TO lcFromOrd,lcToOrd,lcFromPick,lcToPick
STORE '   ' TO lcPriority  &&  Priority Field in the scope screen
*E301249,1 [end]

STORE 1   TO lnSeason,lnDivision
STORE {}  TO ldFromStart,ldToStart,ldFromComp,ldToComp,ldFromPick,ldToPick
STORE .F. TO cbPickedOrd,cbStyles,cbColors,cbCustomers,llColors
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
DO (gcScrDir+gcWinAppl+"\ARINVSCP.SPX")
GO TOP IN (lcInvHdr)
IF EOF(lcInvHdr)
  *E300817,1 Message : 40128
  *E300817,1 No orders match the selected scope.
  *E300817,1 Button : 00000
  *E300817,1 Ok
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

IF llBrowse .OR. (!EMPTY(&lcOrderNo) .AND. !SEEK('O'+&lcOrderNo,'OrdHdr'))
  XORDER     = &lcOrderNo
  llBrowse   = .T.
  &lcOrderNo = IIF(ORDBROWO(@XORDER,.T.,'O'),XORDER,'')
ENDIF
llBrowse = .F.

*!*************************************************************
*! Name      : lfvPickOrd
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1998
*! Purpose   : Valide function for check box 'Orders With Pick Tickets'
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvPickOrd()
*!*************************************************************
FUNCTION lfvPickOrd
PRIVATE lcPickStat

lcPickStat = IIF(cbPickedOrd,'ENABLE','DISABLE')
IF cbPickedOrd
  STORE .F. TO cbStyles,cbColors
  SHOW GET cbStyles DISABLE  
  SHOW GET cbColors DISABLE
  lnSeason = ASUBSCRIPT(laSeasons,ASCAN(laSeasons,'*'),1)
  SHOW GET lnSeason DISABLE
  DIMENSION laStyTarge[1],laClrTarge[1]
  STORE '' TO laStyTarge,laClrTarge
ELSE
  STORE SPACE(6) TO lcFromPick,lcToPick
  STORE {} TO ldFromPick,ldToPick
  SHOW GET cbStyles ENABLE
  SHOW GET cbColors ENABLE  
  SHOW GET lnSeason ENABLE
ENDIF  
SHOW GET ibFromPick &lcPickStat
SHOW GET lcFromPick &lcPickStat
SHOW GET ibToPick   &lcPickStat
SHOW GET lcToPick   &lcPickStat
SHOW GET ldFromPick &lcPickStat
SHOW GET ldToPick   &lcPickStat
RETURN

*!*************************************************************
*! Name      : lfvPikScp
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1998
*! Purpose   : Validate Pick Ticket number
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcPikTkt : Pick Ticket variable name
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvPikScp('lcFromPick')
*!*************************************************************
FUNCTION lfvPikScp
PARAMETERS lcPikTkt
PRIVATE lcBrFields,lcFile_Ttl

SET ORDER TO TAG PIKTKT IN PIKTKT
*B603640,1 Fix bug "Missing )" in pick ticket browse
*IF llBrowse .OR. (!EMPTY(&lcPikTkt) .AND. !SEEK(&Tkt,'PikTkt'))
IF llBrowse .OR. (!EMPTY(&lcPikTkt) .AND. !SEEK(&lcPikTkt,'PikTkt'))
*B603640,1 (End)
  SELECT PikTkt
  SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
  SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
  lcBrFields  = [PikTkt:H='PikTkt',]+;
                [Order:H='Order#',]+;
                [Store:H='Store',]+;
                [OrdHdr.Complete:H='Complete',]+;
                [Date:H='PikDate',]+;
                [Account:H='Account',]+;
                [Customer.BtName:H='Account Name']
  lcFile_Ttl = 'Pick Tickets'
  *B603022,1 MAN 07/20/1999 Check For Completed Piktkt 
  *&lcPikTkt=IIF(AriaBrow("FOR STATUS <> 'X'",lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,;
                         gnBrFSCol2,'','','PikTkt','laBrowArr'),PikTkt.PikTkt,'')
  &lcPikTkt=IIF(AriaBrow("FOR !(STATUS $ 'CX')",lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,;
                         gnBrFSCol2,'','','PikTkt','laBrowArr'),PikTkt.PikTkt,'')                         
  SELECT PikTkt
  SET RELATION OFF INTO ORDHDR
  SET RELATION OFF INTO CUSTOMER
ENDIF
SET ORDER TO TAG Ordpik IN PIKTKT
llBrowse = .F.
RETURN

*!*************************************************************
*! Name      : lfvStyles
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
*! Example   :  =lfvStyles()
*!*************************************************************
FUNCTION lfvStyles
PRIVATE lnAlias

IF EMPTY(laStySourc)
  lnAlias = SELECT()
  *B802562,1 MAN 08/26/99 Ensure that the style file is open
  =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
  SELECT Style
  SET ORDER TO TAG cSTyle
  SCAN
    laStySourc[ALEN(laStySourc)] = cStyMajor
    DIMENSION laStySourc[ALEN(laStySourc)+1]
  ENDSCAN
  DIMENSION laStySourc[ALEN(laStySourc)-1]
  SET ORDER TO TAG Style
  SELECT (lnAlias)
ENDIF  
=gfMover(@laStySourc,@laStyTarge,"Only these Styles : ",.T.)
cbStyles = !EMPTY(laStyTarge)
SHOW GET cbStyles ENABLE
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
PRIVATE lcCondition,lcStatus,lcBulk,lcExp,lnScpCount
*B803616,1 NAD 09/05/2000  (Start) Variable to hold the filter of the piktkt file
PRIVATE lcPkTCond
*B803616,1 NAD (End)
*E300817,1 Message : 40104
*E300817,1 Pick Ticket number range error!
*E300817,1 Button : 00000
*E300817,1 Ok

*E300817,1 Message : 40105
*E300817,1 Pick Ticket date range error!
*E300817,1 Button : 00000
*E300817,1 Ok

*E300817,1 Message : 40106
*E300817,1 Order number range error!
*E300817,1 Button : 00000
*E300817,1 Ok

*E300817,1 Message : 40107
*E300817,1 Order start date range error!
*E300817,1 Button : 00000
*E300817,1 Ok

*E300817,1 Message : 40108
*E300817,1 Order completion date range error!
*E300817,1 Button : 00000
*E300817,1 Ok

IF !EMPTY(lcFromPick) .AND. !EMPTY(lcToPick) .AND. lcToPick < lcFromPick ;
  .AND. gfModalGen('TRM40104B00000','ALERT')=1
  _CUROBJ = OBJNUM(lcFromPick)
  RETURN
ENDIF  
IF !EMPTY(ldFromPick) .AND. !EMPTY(ldToComp) .AND. ldToComp < ldFromPick ;
  .AND. gfModalGen('TRM40105B00000','ALERT')=1
  _CUROBJ = OBJNUM(ldFromPick)
  RETURN
ENDIF  
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
lcCondition=lcCondition+IIF(laSeasons[lnSeason,2]='*','',' AND Season=laSeasons[lnSeason,2]')
lcCondition=lcCondition+IIF(laDivision[lnDivision,2]='*','',' AND cDivision=laDivision[lnDivision,2]')
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
*E301077,50 Inhance openning files to speed up transaction
*B603119,1 (Start) open the files only if it wasn't opened by the uncomplete session
*=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
*=gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
*=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
IF llOpnFiles
  =gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
  =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
  =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
ENDIF 
*B603119,1 (End)
*B803616,1 NAD 09/05/2000 (Start) Open the customer file with a new alias to avoid
*B803616,1     changing the pointer during the scan (which will cause going out of the scan). 
USE gcDataDir+'Customer' IN 0 AGAIN ALIAS CUST ORDER Customer
*B803616,1 NAD 09/05/2000 (End)
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE
SELECT (lcInvLine)
SET RELATION TO STYLE INTO STYLE
SET RELATION TO Style+cWareCode+Dyelot INTO STYDYE ADDITIVE
*E301077,50 (End)

IF cbPickedOrd   && Orders With Pick Tickets Filter
  
  lcPkTCond="!(PikTkt.Status$'CX')"
  DO CASE
  *-- Pick Ticket range
    CASE !EMPTY(lcFromPick) AND EMPTY(lcToPick)
      lcPkTCond= lcPkTCond+ "AND PikTKt.PikTkt >= lcFromPick"
    CASE EMPTY(lcFromPick) AND !EMPTY(lcToPick)
      lcPkTCond= lcPkTCond+ "AND PikTKt.PikTkt <= lcToPick"
    CASE !EMPTY(lcFromPick) AND !EMPTY(lcToPick)
      lcPkTCond= lcPkTCond+ "AND BETWEEN(PikTKt.PikTkt,lcFromPick,lcToPick)"
  ENDCASE
  
  DO CASE
  *-- Pick Ticket date Filter
    CASE !EMPTY(ldFromPick) AND EMPTY(ldToPick)
      lcPkTCond= lcPkTCond+ "AND PikTKt.Date >= ldFromPick"
    CASE EMPTY(ldFromPick) AND !EMPTY(ldToPick)
      lcPkTCond= lcPkTCond+ "AND PikTKt.Date <= ldToPick"
    CASE !EMPTY(ldFromPick) AND !EMPTY(ldToPick)
      lcPkTCond= lcPkTCond+ "AND BETWEEN(PikTKt.Date,ldFromPick,ldToPick)"
  ENDCASE
  *B803616,1  NAD (End) 
ENDIF
SELECT ORDHDR
DO CASE
  CASE !EMPTY(laCusTarge)
    SET ORDER TO TAG ORDBULK
    FOR lnScpCount = 1 TO ALEN(laCusTarge)
      lcStatus = IIF(rbStatus=1,'O,',IIF(rbStatus=2,'H,','H,O,'))
      DO WHILE AT(',',lcStatus) > 0
        lcBulk = IIF(laSetups[21,2]='Y','Y,N,','N,')
        DO WHILE AT(',',lcBulk) > 0
          lcExp = SUBSTR(laCusTarge[lnScpCount],1,5)+;
                  SUBSTR(lcStatus,1,AT(',',lcStatus)-1)    +;
                  SUBSTR(lcBulk,1,AT(',',lcBulk)-1)+"O"
          =SEEK(lcExp+ALLTRIM(lcFromOrd))
          
          SCAN REST FOR   EVAL(lcCondition) ;
                    WHILE Account+Status+Bulk+cOrdType+Order=lcExp .AND. ;
                          IIF(EMPTY(lcToOrd),.T.,Order<=lcToOrd)
            *B803616,1 NAD (Start) Commented out and call it from the new function.
            *=lfGetOrder(Order,'',cbPickedOrd,lcFromPick,lcToPick,.T.)    
            =lfGetOrdPkT()
            *B803616,1 NAD (End)
          ENDSCAN
          lcBulk= SUBSTR(lcBulk,AT(',',lcBulk)+1)
        ENDDO
        lcStatus= SUBSTR(lcStatus,AT(',',lcStatus)+1)
      ENDDO
    ENDFOR
  CASE !EMPTY(lcFromOrd)
    lcCondition=lcCondition+IIF(laSetups[21,2]='Y',''," AND Bulk='N'")
    lcCondition=lcCondition+' AND '+;
                IIF(rbStatus=1,"STATUS='O'",IIF(rbStatus=2,"STATUS='H'",;
                    "INLIST(STATUS,'O','H')" ))
    SET ORDER TO TAG ORDHDR
    =SEEK('O'+lcFromOrd)
    SCAN REST FOR   EVAL(lcCondition);
              WHILE cOrdType+Order='O' AND IIF(EMPTY(lcToOrd),.T.,Order<=lcToOrd)
      *B803616,1 NAD (Start) Commented out and call it from the new function.          
      *=lfGetOrder(Order,'',cbPickedOrd,lcFromPick,lcToPick,.T.)
      =lfGetOrdPkT()   
       *B803616,1 NAD (End)
    ENDSCAN
  OTHERWISE
    SET ORDER TO TAG ORDBULK IN ORDHDR
    *B803616,1 NAD (Start) Scan with the new alias of the customer file. 
    *SELECT CUSTOMER
    SELECT CUST
    *B803616,1 NAD (End)
    =SEEK('M')
    SCAN REST FOR SEEK(Account,'ORDHDR') WHILE Type+Account+Store = 'M'
      lcAccount = Account
      lcStatus = IIF(rbStatus=1,'O,',IIF(rbStatus=2,'H,','H,O,'))
      DO WHILE AT(',',lcStatus) > 0
        lcBulk = IIF(laSetups[21,2]='Y','Y,N,','N,')
        DO WHILE AT(',',lcBulk) > 0
          lcExp = lcAccount+SUBSTR(lcStatus,1,AT(',',lcStatus)-1) +;
                  SUBSTR(lcBulk,1,AT(',',lcBulk)-1)+"O"
          SELECT ORDHDR
          =SEEK(lcExp)
          SCAN REST FOR   EVAL(lcCondition);
                    WHILE Account+Status+Bulk+cOrdType+Order=lcExp AND ;
                          IIF(EMPTY(lcToOrd),.T.,Order<=lcToOrd)
            *B803616,1 NAD (Start) Commented out and call it from the new function.
            *=lfGetOrder(Order,'',cbPickedOrd,lcFromPick,lcToPick,.T.)
            =lfGetOrdPkT()            
            *B803616,1 NAD (End)
          ENDSCAN
          lcBulk= SUBSTR(lcBulk,AT(',',lcBulk)+1)
        ENDDO
        lcStatus= SUBSTR(lcStatus,AT(',',lcStatus)+1)
      ENDDO
    ENDSCAN
ENDCASE
SET ORDER TO TAG ORDHDR IN ORDHDR
*B803616,1 NAD (Start) Close the new customer file alias. 
IF USED ('CUST') 
  USE IN CUST
ENDIF
*B803616,1 NAD (End)


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

*B602905,1 Declare variable hold no of cartons here. [Begin]
PRIVATE lnAllCartn
lnAllCartn = 0
*B602905,1 Declare variable hold no of cartons here. [End  ]

IF OrdHdr.Status='C'
  *E300817,1 Message : 40109
  *E300817,1 Order# 999999 has been shipped complete.
  *E300817,1 Button : 00000
  *E300817,1 Ok
  =gfModalGen('TRM40109B00000','ALERT',lcOrderNo)
  RETURN(.F.)
ENDIF
IF OrdHdr.Status = 'X'
  *E300817,1 Message : 40110
  *E300817,1 Order# 999999 has been cancelled! Cannot ship.
  *E300817,1 Button : 00000
  *E300817,1 Ok
  =gfModalGen('TRM40110B00000','ALERT',lcOrderNo)
  RETURN(.F.)
ENDIF
*B602659,1 Do not invoice bid orders
IF OrdHdr.Status = 'B'
  *B602659,1 Message : 40155
  *B602659,1 Order# 999999 is Bid. Cannot ship.
  *B602659,1 Button : 00000
  *B602659,1 Ok
  =gfModalGen('TRM40155B00000','ALERT',lcOrderNo)
  RETURN(.F.)
ENDIF
*B602659,1 (End)

IF !SEEK('M'+OrdHdr.Account,'Customer')
  *E300817,1 Message : 40112
  *E300817,1 Account XXXXX not found. Cannot Ship.
  *E300817,1 Button : 00000
  *E300817,1 Ok
  =gfModalGen('TRM40112B00000','ALERT','Account '+OrdHdr.Account)
  RETURN(.F.)
ENDIF
IF Customer.Status <> 'A'
  *E300817,1 Message : 40113
  *E300817,1 Account XXXXX is Non-Active. Invoicing is not allowed.
  *E300817,1 Button : 00000
  *E300817,1 Ok
  =gfModalGen('TRM40113B00000','ALERT','Account '+OrdHdr.Account)
  RETURN(.F.)
ENDIF
IF !EMPTY(lcStoreNo) .AND. !SEEK('S'+OrdHdr.Account+lcStoreNo,'Customer')
  *E300817,1 Message : 40112
  *E300817,1 Store XXXXX not found. Cannot Ship.
  *E300817,1 Button : 00000
  *E300817,1 Ok
  =gfModalGen('TRM40112B00000','ALERT','Store '+ALLTRIM(lcStoreNo))
  RETURN(.F.)
ENDIF
IF Customer.Status <> 'A'
  *E300817,1 Message : 40113
  *E300817,1 Store XXXXX is Non-Active. Invoicing is not allowed.
  *E300817,1 Button : 00000
  *E300817,1 Ok
  =gfModalGen('TRM40113B00000','ALERT','Store '+ALLTRIM(lcStoreNo))
  RETURN(.F.)
ENDIF
IF !SEEK('O'+lcOrderNo,'ordline')
  *E300817,1 Message : 40114
  *E300817,1 The lines for order# 999999 were not found.
  *E300817,1 Button : 00000
  *E300817,1 Ok
  =gfModalGen('TRM40114B00000','ALERT',lcOrderNo)
  RETURN(.F.)
ENDIF
*B602411,1 Check bulk orders.
IF OrdHdr.Bulk='Y' AND laSetups[21,2]='N'
  *B602411,1 Message : 40151
  *B602411,1 Order# 999999 is a Bulk order. Cannot Ship.
  *B602411,1 Button : 00000
  *B602411,1 Ok
  =gfModalGen('TRM40151B00000','ALERT',lcOrderNo)
  RETURN(.F.)
ENDIF
*B602411,1 (End)

IF OrdHdr.Status = 'H' 
  IF laSetups[22,2]='Y'
    *E300817,1 Message : 40111
    *E300817,1 Order# 999999 is On Hold.
    *E300817,1 Button : 40003
    *E300817,1 Proceed Cancel
    IF gfModalGen('QRM40111B40003','ALERT',lcOrderNo+'| ')=2
      RETURN(.F.)
    ENDIF
  ELSE
    *E300817,1 Message : 40111
    *E300817,1 Order# 999999 is On Hold. Cannot Ship.
    *E300817,1 Button : 00000
    *E300817,1 Ok
    =gfModalGen('TRM40111B00000','ALERT',lcOrderNo+'| Cannot Ship.')
    RETURN(.F.)
  ENDIF
ENDIF

*MAB HERE Check for entered invoice date.
*IF gdSysDate < ORDHDR.START
IF ldDefInvDate < ORDHDR.START
  *E300817,1 Message : 40116
  *E300817,1 Order# 999999 start date is not until 99/99/9999.
  *E300817,1 Button : 00000
  *E300817,1 Ok
  =gfModalGen('INM40116B00000','ALERT',lcOrderNo+'|'+DTOC(ORDHDR.START))
ENDIF
IF gdSysDate > ORDHDR.COMPLETE
  *E300817,1 Message : 40117
  *E300817,1 Order# 999999 is past completion date which was 99/99/9999.
  *E300817,1 Button : 00000
  *E300817,1 Ok
  =gfModalGen('INM40117B00000','ALERT',lcOrderNo+'|'+DTOC(ORDHDR.COMPLETE))
ENDIF
*E301077,50 Inhance openning files to speed up transaction
IF 'AL' $ gcCmpModules
  *B603119,1 (Start) Open Files only if it was not opened by the uncomplete session 
  *=gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH') 
  *=gfOpenFile(gcDataDir+'PACK_LIN',gcDataDir+'PACK_LIN','SH')
  IF llOpnFiles
    =gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH') 
    =gfOpenFile(gcDataDir+'PACK_LIN',gcDataDir+'PACK_LIN','SH')
  ENDIF
  *B603119,1 (End)
ENDIF

IF !llScope
  *-- if the function was not called from the scope screen
  *B603119,1 (Start) Open files only if it was not opened by the uncomplete session
  *=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
  *=gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
  *=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
  IF llOpnFiles
    =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
    =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
    =gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
  ENDIF  
  *B603119,1 (End)
  
  SELECT STYLE
  SET RELATION TO 'S'+SCALE INTO SCALE
  SELECT (lcInvLine)
  SET RELATION TO STYLE INTO STYLE
  SET RELATION TO Style+cWareCode+Dyelot INTO STYDYE ADDITIVE
ENDIF
*E301077,50 (End)

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
  lcLinesCond= "!Picked AND TotQty >0"
ENDIF
IF llScope AND !EMPTY(laStyTarge)
  *--Style major Filter
  lcLinesCond=lcLinesCond+" AND ASCAN(laStyTarge,ALLTRIM(SUBSTR(Style,1,LEN(lcMjrMsk))))>0"
ENDIF
*-- get the Term code related fields               
=gfRltFld(OrdHdr.cTermCode,@laTRltFld,'CTERMCODE')

*C200088,1 Calculate Due date corresponding to FP Requirements [Begin]
IF ASCAN(laEvntTrig,"WHATISDUED") <> 0
  = gfDoTriger('ARIINV','WHATISDUED')
ENDIF
*C200088,1 Calculate Due date corresponding to FP Requirements [End  ]

*E301217,1 AMM If no value entered, default by 20 
*lnEOMDay = IIF(lnEOMDay = 0,20,lnEOMDay-1)
lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)
*E301217,1 AMM end

IF llIsEngland
  ldDueDate = IIF(lcTEOM <> 'Y',ldDefInvDate+lnTDaysDue,;
                  CTOD('01'+SUBSTR(DTOC(GOMONTH(ldDefInvDate,1)),3))-1+lnTDaysDue)
ELSE                
  *E301217,1 AMM Calculate due to the end of month day variable which is 
  *E301217,1 AMM related field to the payment terms code. 
  *ldDueDate = IIF(lcTEOM <> 'Y',ldDefInvDate+lnTDaysDue,;
                  GOMONTH(CTOD(SUBSTR(DTOC(ldDefInvDate),1,3)+'10'+;
                  SUBSTR(DTOC(ldDefInvDate),6,5)),IIF(DAY(ldDefInvDate) > 20,2,1))+lnTDaysDue)
  ldDueDate = IIF(lcTEOM <> 'Y',ldDefInvDate+lnTDaysDue,;
                  GOMONTH(CTOD(SUBSTR(DTOC(ldDefInvDate),1,3)+'10'+;
                  SUBSTR(DTOC(ldDefInvDate),6,5)),IIF(DAY(ldDefInvDate) > lnEOMDay,2,1))+lnTDaysDue)
  *E301217,1 AMM end
ENDIF
lcCurrCode= IIF(EMPTY(OrdHdr.cCurrCode),gcBaseCurr,OrdHdr.cCurrCode)
STORE 1 TO lnExRate, lnCurrUnit
IF lcCurrCode <> gcBaseCurr
  lnExRate=gfChkRate('lnCurrUnit',lcCurrCode,ldDefInvDate,.T.,.F.,.F.,llEditExRt)
  IF lnExRate = 0
    IF llEditExRt
      *E300511,1 Message : 00262
      *E300511,1 A valid xxx to xxx exchange rate could not be found on xxx.
      *E300511,1 Button : 00000
      *E300511,1 Ok
      =gfModalGen('INM00262B00000','ALERT',ALLTRIM(lcCurrCode)+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(ldDefInvDate))
    ELSE
      lcCurrCode = gcBaseCurr
      STORE 1 TO lnExRate, lnCurrUnit
    ENDIF
  ENDIF
ENDIF
=SEEK('M'+OrdHdr.Account,'Customer')
*B603402,1 (Start) get the main store phone
IF EMPTY(OrdHdr.Store)  
  lcPhone=Customer.Phone1
ENDIF  
*B603402,1 (End)
llBackOrd = INLIST(IIF(EMPTY(Customer.cBackOrd),laSetups[23,2],Customer.cBackOrd),'A','I')
SELECT OrdLine
SET ORDER TO TAG Ordlinst
=SEEK('O'+lcOrderNo+ALLTRIM(lcStoreNo))
DO WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = 'O'+lcOrderNo+ALLTRIM(lcStoreNo)
  lcStore = Store
  =IIF(EMPTY(Store),SEEK('M'+Account,'Customer'),SEEK('S'+Account+Store,'Customer'))
  lcSaleRep1 = Ordhdr.Rep1
  lnComm1    = OrdHdr.Comm1
  lcSaleRep2 = Ordhdr.Rep2
  lnComm2    = OrdHdr.Comm2
  IF OrdHdr.Multi='Y' AND EMPTY(OrdHdr.Rep1) AND EMPTY(OrdHDr.Rep2)
    lcSaleRep1 = Customer.SalesRep
    lnComm1    = Customer.Comm
    lcSaleRep2 = Customer.Rep2
    lnComm2    = Customer.Comm2
  ENDIF

  SCAN REST FOR   EVAL(lcLinesCond) ;
            WHILE cOrdType+Order+Store+Style+STR(LineNo,6)='O'+lcOrderNo+lcStore
    llPacked = .F.
    IF 'AL' $ gcCmpModules AND ;
      SEEK(OrdLine.Order+OrdLine.Store+OrdLine.PikTkt,'Pack_Hdr') AND ;
      SEEK(PACK_HDR.Pack_No,'Pack_Lin')
      
      *B802979,1 Default invoice quantity to packed quantity
      *SELECT Pack_Lin
      *LOCATE REST FOR   nOrdLineNo=OrdLine.LineNo ;
      *            WHILE Pack_No=Pack_Hdr.Pack_No 
      *llPacked = FOUND()

      IF !SEEK(OrdLine.PikTkt+OrdLine.Order,lcPackLine)
        =lfPackLin(OrdLine.Order,OrdLine.PikTkt)
      ENDIF
      llPacked = SEEK(OrdLine.PikTkt+OrdLine.Order+STR(OrdLine.LineNo,6),lcPackLine)
      *B802979,1 (End)

      SELECT OrdLine
    ENDIF
    SCATTER MEMVAR MEMO
    m.Gros_Price = IIF(m.Gros_Price=0,m.Price,m.Gros_Price)
    =SEEK(m.Style,'Style')
    STORE 0 TO lnTaxQty,lnTaxRate
    IF llIsEngland .AND. laSetups[9,2]='Y' .AND. ;
       !Customer.lVatExem  .AND. Style.nTaxBreak <> 0
      =gfRltFld(Style.cTaxCode,@laEngStyTax,'CTAXCODE') 
      FOR lnCount = Style.nTaxBreak TO 8
        *B603168,1
        *B603321,1 SSH 12/07/99 Fix the bug of wrong calcualtion of tax amount if case
        *B603321,1 SSH          of using piktkt
        *lnTaxQty = lnTaxQty + EVAL('OrdLine.Qty'+STR(lnCount,1))
        *lnTaxQty = lnTaxQty + EVAL('OrdLine.'+IIF(EMPTY(laData[3]),'Qty','Pik')+STR(lnCount,1))
        lnTaxQty = lnTaxQty + EVAL('OrdLine.'+IIF(EMPTY(m.PikTkt),'Qty','Pik')+STR(lnCount,1))
        *B603321,1 SSH(End)
        *B603168,1
      ENDFOR
    ENDIF
    *B603318,1 KHM 12/06/99 (Begin) This is a custom process for FP in order
    *B603318,1              to check if there is one line that does not 
    *B603318,1              received in the piktkt then display a 
    *B603318,1              notification message and ingore this piktkt.
    IF ASCAN(laEvntTrig , PADR('NORECIV',10)) <> 0
      IF TYPE('lcHoldTkt') = 'U'
        lcHoldTkt = SPACE(6)
      ENDIF
      IF !gfDoTriger("ARIINV",PADR("NORECIV",10))
        LOOP
      ENDIF
    ENDIF
    *B603318,1 KHM 12/06/99 (End)
    m.lTaxable = Style.lTaxable

    *B802979,1 Default invoice quantity to packed quantity
    IF llPacked
      m.Pik1 = &lcPackLine..Qty1
      m.Pik2 = &lcPackLine..Qty2
      m.Pik3 = &lcPackLine..Qty3
      m.Pik4 = &lcPackLine..Qty4
      m.Pik5 = &lcPackLine..Qty5
      m.Pik6 = &lcPackLine..Qty6
      m.Pik7 = &lcPackLine..Qty7
      m.Pik8 = &lcPackLine..Qty8
      m.TotPik = &lcPackLine..TotQty

      IF !EMPTY(m.Prepak)  
        =SEEK('P'+m.Scale+m.Prepak,'Scale')
        IF Scale.pp1/Scale.PPTot*m.TotPik <> m.Pik1 OR ;
           Scale.pp2/Scale.PPTot*m.TotPik <> m.Pik2 OR ;
           Scale.pp3/Scale.PPTot*m.TotPik <> m.Pik3 OR ;
           Scale.pp4/Scale.PPTot*m.TotPik <> m.Pik4 OR ;
           Scale.pp5/Scale.PPTot*m.TotPik <> m.Pik5 OR ;
           Scale.pp6/Scale.PPTot*m.TotPik <> m.Pik6 OR ;
           Scale.pp7/Scale.PPTot*m.TotPik <> m.Pik7 OR ;
           Scale.pp8/Scale.PPTot*m.TotPik <> m.Pik8
          m.Prepak = ''
          m.PPQty  = 0
        ELSE
          m.PPQty = IIF(Scale.PPTot=0,0,m.TotPik /Scale.PPTot)
        ENDIF
        =SEEK('S'+m.Scale,'Scale')
      ENDIF
    ENDIF
    *802979,1 (End)
    
    INSERT INTO (lcInvLine);
    (Order,Account,LineNo,Store,PikTkt,Style,Dyelot,Note_Mem,Comm1,Comm2,Book1,Book2,;
     Book3,Book4,Book5,Book6,Book7,Book8,TotBook,Flag,Pik1,Pik2,Pik3,Pik4,Pik5,;
     Pik6,Pik7,Pik8,TotPik,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
     Price,Pack_Id,Gros_Price,Disc_Pcnt,lPacked,lBackOrd,nTaxRate,Group,Prepak,;
     Desc1,Season,PPQty,Scale,cWareCode,Consol,cDivision,cCurrCode,lTaxable,cDyeFlag,cwarecode);
     VALUES (m.Order,m.Account,m.LineNo,m.Store,m.PikTkt,m.Style,m.Dyelot,;
     m.Note_Mem,m.Comm1,m.Comm2,m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,;
     m.Qty7,m.Qty8,m.TotQty,IIF(llPicked,'B',' '),m.Pik1,m.Pik2,m.Pik3,m.Pik4,;
     m.Pik5,m.Pik6,m.Pik7,m.Pik8,m.TotPik,IIF(llPicked,m.Pik1,m.Qty1),;
     IIF(llPicked,m.Pik2,m.Qty2),IIF(llPicked,m.Pik3,m.Qty3),IIF(llPicked,m.Pik4,m.Qty4),;
     IIF(llPicked,m.Pik5,m.Qty5),IIF(llPicked,m.Pik6,m.Qty6),IIF(llPicked,m.Pik7,m.Qty7),;
     IIF(llPicked,m.Pik8,m.Qty8),IIF(llPicked,m.TotPik,m.TotQty),m.Price,m.Pack_Id,;
     m.Gros_Price,m.Disc_Pcnt,llPacked,llBackOrd,lnTaxRate,m.Group,m.Prepak,;
     m.Desc1,m.Season,m.PPQty,m.Scale,m.cWareCode,'N',OrdHdr.cDivision,OrdHdr.cCurrCode,m.lTaxable,Style.cDye_Flg,m.cwarecode)
    SELECT (lcInvLine)
    IF INLIST(laSetups[7,2],'F','L') AND SEEK(m.Style+m.cWareCode+m.Dyelot,'StyDye')
      REPLACE Qty1 WITH MAX(MIN(Qty1,StyDye.Stk1),0) ,;
              Qty2 WITH MAX(MIN(Qty2,StyDye.Stk2),0) ,;
              Qty3 WITH MAX(MIN(Qty3,StyDye.Stk3),0) ,;
              Qty4 WITH MAX(MIN(Qty4,StyDye.Stk4),0) ,;
              Qty5 WITH MAX(MIN(Qty5,StyDye.Stk5),0) ,;
              Qty6 WITH MAX(MIN(Qty6,StyDye.Stk6),0) ,;
              Qty7 WITH MAX(MIN(Qty7,StyDye.Stk7),0) ,;
              Qty8 WITH MAX(MIN(Qty8,StyDye.Stk8),0) ,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
    ENDIF
    SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
    SELECT (lcInvHdr)
    *B802837,1 SSH 11/29/1999 Get the no of carton from back_hdr
    llInvPck = !EMPTY(m.PikTkt) AND SEEK(m.Order+m.Store+m.PikTkt,'Pack_Hdr')
    *B802837,1 SSH 11/29/1999 (End)
    


    
    IF !SEEK(m.Account+m.Order+m.Store+m.PikTkt)
      APPEND BLANK
      REPLACE cSelect    WITH '�' ,;
              Order      WITH m.Order ,;
              Store      WITH m.Store ,;
              PikTkt     WITH m.PikTkt ,;
              CustPo     WITH IIF(EMPTY(m.CustPo),OrdHdr.CustPo,m.CustPo),;
              Dist_Ctr   WITH Customer.Dist_Ctr ,;
              Account    WITH OrdHdr.Account ,;
              cTermCode  WITH OrdHdr.cTermCode  ,;
              SpcInst    WITH OrdHdr.SpcInst ,;
              lUpsIns    WITH (OrdHdr.CINSUR='Y') ,;
              Rep1       WITH lcSaleRep1 ,;
              Comm1      WITH lnComm1    ,;
              Rep2       WITH lcSaleRep2 ,;
              Comm2      WITH lnComm2    ,;
              Note1      WITH OrdHdr.Note1 ,;
              Note2      WITH OrdHdr.Note2 ,;
              lCompUps   WITH .T. ,;
              Consol     WITH 'N' 
      *B803718,1 NAD 10/16/2000 (Start) Get the LastLine from OrdHdr.
      REPLACE LastLine   WITH OrdHdr.LastLine   
      *B803718,1 NAD 10/16/2000 (END)
      *B603402,1 Save the Phone for the main Store if the ordhdr.store is empty
      *REPLACE DiscPcnt   WITH OrdHdr.Disc      ,;
              InvDate    WITH ldDefInvDate     ,;
              ShipDate   WITH ldDefInvDate     ,;
              dPostDate  WITH ldDefPstDate     ,;
              DueDate    WITH ldDueDate        ,;
              Dept       WITH OrdHdr.Dept      ,; 
              cFacCode   WITH OrdHdr.cFacCode  ,;
              Approval   WITH OrdHdr.Approval  ,;
              Appramt    WITH OrdHdr.ApprAmt   ,;
              Season     WITH OrdHdr.Season    ,;
              cDivision  WITH OrdHdr.cDivision ,;
              UpsZone    WITH Customer.UpsZone ,;
              Phone      WITH Customer.Phone1  ,;
              cWareCode  WITH IIF(llPicked .AND. SEEK(m.Order+m.PikTkt,'PikTkt'),;
                                  PikTkt.cWareCode,OrdHdr.cWareCode) ,;
              Trde_Disc  WITH lnTerDiscR ,;
              Tax_Rate   WITH IIF(laSetups[9,2] <> 'Y',0,;
                              IIF(llIsCanada,laSetups[10,2],Customer.nTaxRate)) ,;
              nPstRate   WITH IIF(laSetups[9,2]='Y' AND llIsCanada,;
                               Customer.nTaxRate,0) ,;
              cTaxRule   WITH IIF(laSetups[9,2]='Y' AND llIsCanada,;
                              Customer.cTaxRule,'') ,;
              Cod_Flag   WITH IIF(lcTCod='Y','Y','N'),;
              Status     WITH OrdHdr.Status ,;
              cCurrCode  WITH lcCurrCode ,;
              nExRate    WITH lnExRate   ,;
              nCurrUnit  WITH lnCurrUnit ,;
              dAdd_Date  WITH gdSysDate  ,;
              cAdd_Time  WITH TIME()     ,;
              cAdd_User  WITH gcUser_id 

      REPLACE DiscPcnt   WITH OrdHdr.Disc      ,;
              InvDate    WITH ldDefInvDate     ,;
              ShipDate   WITH ldDefInvDate     ,;
              dPostDate  WITH ldDefPstDate     ,;
              DueDate    WITH ldDueDate        ,;
              Dept       WITH OrdHdr.Dept      ,; 
              cFacCode   WITH OrdHdr.cFacCode  ,;
              Approval   WITH OrdHdr.Approval  ,;
              Appramt    WITH OrdHdr.ApprAmt   ,;
              Season     WITH OrdHdr.Season    ,;
              cDivision  WITH OrdHdr.cDivision ,;
              UpsZone    WITH Customer.UpsZone ,;
              Phone      WITH IIF(EMPTY(Ordhdr.Store),lcPhone,Customer.Phone1)  ,;
              cWareCode  WITH IIF(llPicked .AND. SEEK(m.Order+m.PikTkt,'PikTkt'),;
                                  PikTkt.cWareCode,OrdHdr.cWareCode) ,;
              Trde_Disc  WITH lnTerDiscR ,;
              Tax_Rate   WITH IIF(laSetups[9,2] <> 'Y',0,;
                              IIF(llIsCanada,laSetups[10,2],Customer.nTaxRate)) ,;
              nPstRate   WITH IIF(laSetups[9,2]='Y' AND llIsCanada,;
                               Customer.nTaxRate,0) ,;
              cTaxRule   WITH IIF(laSetups[9,2]='Y' AND llIsCanada,;
                              Customer.cTaxRule,'') ,;
              Cod_Flag   WITH IIF(lcTCod='Y','Y','N'),;
              Status     WITH OrdHdr.Status ,;
              cCurrCode  WITH lcCurrCode ,;
              nExRate    WITH lnExRate   ,;
              nCurrUnit  WITH lnCurrUnit ,;
              dAdd_Date  WITH gdSysDate  ,;
              cAdd_Time  WITH TIME()     ,;
              cAdd_User  WITH gcUser_id
      *C102212,1 (Begin) Update HST Tax rate witn the default setup.
      REPLACE nHstRate   WITH IIF(laSetups[9,2] <> 'Y' OR !llIsCanada,0,laSetups[26,2])
      *C102212,1 (End) 
           *B603402,1 (End)
      *B802837,1 SSH 11/29/1999 Get the no of carton from back_hdr
      IF llInvPck
        REPLACE Weight    WITH Pack_Hdr.Tot_Wght ,;
                nCartons  WITH Pack_Hdr.Tot_Cart ,;
                Cartons   WITH Pack_Hdr.Tot_Cart
      ENDIF
      *B802837,1 SSH 11/29/1999 (End)
    ENDIF

    *B602905,1 Default No. of cartons must be 1 at least [Begin]
    *REPLACE LastLine  WITH LastLine + 1 ,;
            Ordered   WITH Ordered  + &lcInvLine..TotBook ,;
            Ship      WITH Ship     + m.TotQty ,;
            ShipAmt   WITH ShipAmt  + m.TotQty*m.Price ,;
            Discount  WITH -ShipAmt * DiscPcnt/100,;
            Weight    WITH Weight   + m.TotQty*Style.nStyWeight ,;
            nCartons  WITH nCartons + IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0) ,;
            Cartons   WITH CEILING(nCartons) ,;
            Picked    WITH Picked  + m.TotPik ,;
            ShipVia   WITH IIF(Customer.nBrkWeight <> 0 AND Weight > Customer.nBrkWeight,Customer.cAltShpVia,;
                           IIF(ALLTRIM(OrdHdr.ShipVia)='*',Customer.ShipVia,OrdHdr.ShipVia)) ,;
            nMerchTax WITH nMerchTax + lnTaxQty * m.Price * lnTaxRate/100 ,;
            Tax_Amt   WITH nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100  ,;
            Cod_Amt   WITH IIF(Cod_Flag='Y' AND llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,; 
            TotalChg  WITH IIF(llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,;
            nTaxDue   WITH nTaxDue + IIF(m.lTaxable,m.TotQty*m.Price,0)

    lnCartons  = nCartons + IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0)
    lnAllCartn = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))

    *B802837,1 SSH 11/29/1999 Get the no of carton from back_hdr
    *REPLACE LastLine  WITH LastLine + 1 ,;
            Ordered   WITH Ordered  + &lcInvLine..TotBook ,;
            Ship      WITH Ship     + m.TotQty ,;
            ShipAmt   WITH ShipAmt  + m.TotQty*m.Price ,;
            Discount  WITH -ShipAmt * DiscPcnt/100,;
            Weight    WITH Weight   + m.TotQty*Style.nStyWeight ,;
            nCartons  WITH lnCartons         ,;
            Cartons   WITH lnAllCartn        ,;
            Picked    WITH Picked  + m.TotPik ,;
            ShipVia   WITH IIF(Customer.nBrkWeight <> 0 AND Weight > Customer.nBrkWeight,Customer.cAltShpVia,;
                           IIF(ALLTRIM(OrdHdr.ShipVia)='*',Customer.ShipVia,OrdHdr.ShipVia)) ,;
            nMerchTax WITH nMerchTax + lnTaxQty * m.Price * lnTaxRate/100 ,;
            Tax_Amt   WITH nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100  ,;
            Cod_Amt   WITH IIF(Cod_Flag='Y' AND llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,; 
            TotalChg  WITH IIF(llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,;
            nTaxDue   WITH nTaxDue + IIF(m.lTaxable,m.TotQty*m.Price,0)
    *B602905,1 Default No. of cartons must be 1 at least [End  ]
    *B803718,1 NAD 10/16/2000 (Start) Not to accumulate LastLine because we get it from OrdHdr directly.
    *REPLACE LastLine  WITH LastLine + 1 ,;
            Ordered   WITH Ordered  + &lcInvLine..TotBook ,;
            Ship      WITH Ship     + m.TotQty ,;
            ShipAmt   WITH ShipAmt  + m.TotQty*m.Price ,;
            Discount  WITH -ShipAmt * DiscPcnt/100,;
            Weight    WITH Weight   + IIF(llInvPck,0,m.TotQty*Style.nStyWeight) ,;
            nCartons  WITH IIF(llInvPck,nCartons,lnCartons) ,;
            Cartons   WITH IIF(llInvPck,Cartons,lnAllCartn) ,;
            Picked    WITH Picked  + m.TotPik ,;
            ShipVia   WITH IIF(Customer.nBrkWeight <> 0 AND Weight > Customer.nBrkWeight,Customer.cAltShpVia,;
                           IIF(ALLTRIM(OrdHdr.ShipVia)='*',Customer.ShipVia,OrdHdr.ShipVia)) ,;
            nMerchTax WITH nMerchTax + lnTaxQty * m.Price * lnTaxRate/100 ,;
            Tax_Amt   WITH nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100  ,;
            Cod_Amt   WITH IIF(Cod_Flag='Y' AND llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,; 
            TotalChg  WITH IIF(llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,;
            nTaxDue   WITH nTaxDue + IIF(m.lTaxable,m.TotQty*m.Price,0)
    
    
    REPLACE Ordered   WITH Ordered  + &lcInvLine..TotBook ,;
            Ship      WITH Ship     + m.TotQty ,;
            ShipAmt   WITH ShipAmt  + m.TotQty*m.Price ,;
            Discount  WITH -ShipAmt * DiscPcnt/100,;
            Weight    WITH Weight   + IIF(llInvPck,0,m.TotQty*Style.nStyWeight) ,;
            nCartons  WITH IIF(llInvPck,nCartons,lnCartons) ,;
            Cartons   WITH IIF(llInvPck,Cartons,lnAllCartn) ,;
            Picked    WITH Picked  + m.TotPik ,;
            ShipVia   WITH IIF(Customer.nBrkWeight <> 0 AND Weight > Customer.nBrkWeight,Customer.cAltShpVia,;
                           IIF(ALLTRIM(OrdHdr.ShipVia)='*',Customer.ShipVia,OrdHdr.ShipVia)) ,;
            nMerchTax WITH nMerchTax + lnTaxQty * m.Price * lnTaxRate/100 ,;
            Tax_Amt   WITH nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100  ,;
            Cod_Amt   WITH IIF(Cod_Flag='Y' AND llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,; 
            TotalChg  WITH IIF(llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,;
            nTaxDue   WITH nTaxDue + IIF(m.lTaxable,m.TotQty*m.Price,0)
    
    *B803718,1 NAD 10/16/2000 (End)
    *B802837,1 SSH (END)

  ENDSCAN
ENDDO

IF !llScope AND EOF(lcInvHdr)
  *E300817,1 Message : 40128
  *E300817,1 No orders match the selected scope.
  *E300817,1 Button : 00000
  *E300817,1 Ok
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
    *E300817,1 Message : 40086
    *E300817,1 invoice date cannot be greater than posting date
    *E300817,1 Button : 00000 
    *E300817,1 Ok
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
  *B603505,1 Refresh payment terms discount
  m.Trde_Disc = Trde_Disc
  *B603505,1 (End)
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
  *E301217,1 AMM Calculate due to the end of month day variable which is 
  *E301217,1 AMM related field to the payment terms code. 
  *m.DueDate = IIF(lcTEOM <> 'Y',m.ShipDate+lnTDaysDue,;
                  GOMONTH(CTOD(SUBSTR(DTOC(m.ShipDate),1,3)+'10'+;
                  SUBSTR(DTOC(m.ShipDate),6,5)),IIF(DAY(m.ShipDate) > 20,2,1))+lnTDaysDue)
  m.DueDate = IIF(lcTEOM <> 'Y',m.ShipDate+lnTDaysDue,;
                  GOMONTH(CTOD(SUBSTR(DTOC(m.ShipDate),1,3)+'10'+;
                  SUBSTR(DTOC(m.ShipDate),6,5)),IIF(DAY(m.ShipDate) > lnEOMDay,2,1))+lnTDaysDue)
  *E301217,1 AMM end
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
  *E300817,1 Message : 40086
  *E300817,1 invoice date cannot be greater than posting date
  *E300817,1 Button : 00000 
  *E300817,1 Ok
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
  *B802865,1 WAB - can not select multi shipvia if it is not an consolidate header
  IF ALLTRIM(laShipVia[lnShipVia,2]) = '*' AND Consol = 'N'
    lnShipVia = ASUBSCRIPT(laShipVia,ASCAN(laShipVia,ALLTRIM(m.Shipvia)),1)
    RETURN 
  ENDIF
  *B802865,1 WAB - END
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
    *E300817,1 Message : 40037
    *E300817,1 The exchange rate must be greater than zero
    *E300817,1 Button : 00000 
    *E300817,1 Ok
    =gfModalGen('INM40037B00000','ALERT')
    m.nExRate = &lcInvHdr..nExRate
  ENDIF
  STORE .T. TO llCUpdate
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE nExRate WITH m.nExRate
  UNLOCK
ENDIF


******CHARGES FUNCTION
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

m.DiscPcnt = IIF(llPercent,m.DiscPcnt,ABS(m.Discount)*100/m.ShipAmt)
m.Discount = IIF(llPercent,-1*m.ShipAmt*m.DiscPcnt/100,-1*ABS(m.Discount))
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

*B803571,1 NAD 10/25/2000 (Start) Not to allow users to add negative values in the charges fields. 
IF lfNegChrg()
*B803571,1 NAD 10/25/2000 (End) 

  SELECT (lcInvHdr)  
  lnTaxAmount = IIF(UPPER(ALLTRIM(gcContCode))='USA',nTaxDue,ShipAmt)
  m.Tax_Amt = IIF(llRate,m.Tax_Rate*(IIF(laSetups[11,2]='M',lnTaxAmount,;
                  lnTaxAmount+Freight+Insur+COD)-DiscPcnt*lnTaxAmount/100)/100,m.Tax_Amt)
  IF m.Tax_Amt > IIF(laSetups[11,2]='M',ShipAmt,;
                 ShipAmt+Freight+Insur+COD)+Discount
    m.Tax_Amt = &lcInvHdr..Tax_Amt
    RETURN
  ENDIF

  *C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
  *m.Tax_Rate = IIF(!llRate,m.Tax_Amt*100/;
                  (IIF(laSetups[11,2]='M',lnTaxAmount,lnTaxAmount+Freight+;
                  Insur+COD)-DiscPcnt*lnTaxAmount/100),m.Tax_Rate)

  m.Tax_Rate = IIF(!llRate,;
                   IIF(m.Tax_Amt = 0 , 0 ,;
                   IIF(laSetups[11,2]='M' AND lnTaxAmount = 0 , 0 ,;
                   IIF(laSetups[11,2]#'M' AND lnTaxAmount+Freight+Insur+COD-DiscPcnt*lnTaxAmount/100 = 0 , 0 ,;
                   m.Tax_Amt*100/(IIF(laSetups[11,2]='M',lnTaxAmount,lnTaxAmount+Freight+Insur+COD)-DiscPcnt*lnTaxAmount/100))));
                  ,m.Tax_Rate)

  *C102314,1 BWA 06/10/2001 [END]

  m.nPstAmt  = IIF(llIsCanada .AND. VAL(cTaxRule)=2,;
               (ShipAmt+Freight+Insur+COD+m.Tax_Amt+Discount)*nPstRate/100,nPstAmt)
  IF Cod_Flag='Y'
  *B603989,1 NAD 10/26/2000 (Start) If the UPS Type is OTHER don't add the Frieght, COD charges to the COD amount.
  IF EMPTY(lcUPSType) OR lcUPSType='OTHER'
  
    m.Cod_Amt = ShipAmt-ShipAmt*m.DiscPcnt/100 +m.Tax_Amt+m.nPstAmt
  ELSE
  *B603989,1 NAD 10/26/2000 (End) 
    m.Cod_Amt = ShipAmt+Freight+Insur+COD+Discount+m.Tax_Amt+m.nPstAmt
  *B603989,1 NAD 10/26/2000 (Start) Added the ENDIF. 
  ENDIF
  *B603989,1 NAD 10/26/2000 (End) 
   
  *C102239,1 (Begin) Update COD for all customers except Bel05.
  *IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. ;
      Cartons=1 AND SEEK(Order+Store+PikTkt,lcUpsBox)  
  IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' ;
     AND (EMPTY(laData[3]) OR ASCAN(laEvntTrig , PADR('CRTCHRG',10)) = 0) AND ;
     Cartons=1 AND SEEK(Order+Store+PikTkt,lcUpsBox)
  *C102239,1 (End)
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH m.Cod_Amt
      UNLOCK
    ENDIF
  ENDIF
  SELECT (lcInvHdr)  
  lnOTaxAmnt=Tax_Amt
  *C102212,1 (Begin) Add HST Tax to the total charge.
  *m.TotalChg = ShipAmt+Freight+Insur+COD+Discount+m.Tax_Amt+m.nPstAmt


  m.TotalChg = ShipAmt+Freight+Insur+COD+Discount+m.Tax_Amt+m.nPstAmt+m.nHstAmt

  *C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
  IF ASCAN(laEvntTrig , PADR('SUMALL',10)) <> 0
    =gfDoTriger('ARIINV',PADR('SUMALL',10))
  ENDIF
  *C102314,1 BWA 06/10/2001 [END]

  *C102212,1 (End)
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
  *B602760,1 NAD Comment this lines and use when function to Prevent editing tax Amount
  *B602760,1     if the tax Percent is >0
  *IF m.Tax_Rate = 0 AND lnTaxAmount > 0
  *  SHOW GET m.Tax_Amt ENABLE 
  *ELSE
  *  SHOW GET m.Tax_Amt DISABLE
  *ENDIF
  *B602760,1 (End)
  RETURN
*B803571,1 NAD (Start) 10/25/2000 Added the ENDIF.
ENDIF
*B803571,1 NAD (End) 

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

*B803571,1 NAD (Start) 10/25/2000 Not to allow users to add negative values.
IF lfNegChrg()
*B803571,1 NAD (End) 

  SELECT (lcInvHdr)  
  m.cTaxRule = STR(lnPstRule,2)
  m.nPstAmt  = IIF(llRate,(ShipAmt+Freight+Insur+COD+;
               Discount+IIF(VAL(m.cTaxRule)=1,0,Tax_Amt))*;
               m.nPstRate/100,m.nPstAmt)
  m.nPstRate = IIF(!llRate,m.nPstAmt*100/(ShipAmt+Freight+Insur+COD+Discount+;
               IIF(VAL(m.cTaxRule)=1,0,Tax_Amt)),m.nPstRate)
  IF Cod_Flag='Y'
  *B603989,1 NAD 10/26/2000 (Start) If the UPS Type is OTHER don't add the Frieght, COD charges to the COD amount.
    IF EMPTY(lcUPSType) OR lcUPSType='OTHER'
      m.Cod_Amt = ShipAmt-ShipAmt*m.DiscPcnt/100 +m.Tax_Amt+m.nPstAmt
    ELSE
  *B603989,1 NAD 10/26/2000 (End)
   m.Cod_Amt = ShipAmt+Freight+Insur+COD+Discount+Tax_Amt+m.nPstAmt
  
  *B603989,1 NAD 10/26/2000 (Start) added the end if
  ENDIF
  *B603989,1 NAD  (End)
  *C102239,1 (Begin) Update COD for all customers except Bel05.
  *IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. ;
      Cartons=1 AND SEEK(Order+Store+PikTkt,lcUpsBox)  
  IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' ;
     AND (EMPTY(laData[3]) OR ASCAN(laEvntTrig , PADR('CRTCHRG',10)) = 0) AND ;
     Cartons=1 AND SEEK(Order+Store+PikTkt,lcUpsBox)
  *C102239,1 (End)
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH m.Cod_Amt
      UNLOCK
    ENDIF
  ENDIF
  SELECT (lcInvHdr)  
  lnOTaxAmnt = nPstAmt
  *C102212,1 (Begin) Add HST Tax to the total charge.
  *m.TotalChg = ShipAmt+Freight+Insur+COD+Discount+Tax_Amt+m.nPstAmt
  m.nHstAmt  = IIF(llRate,(ShipAmt+Freight+Insur+COD+;
               Discount+IIF(VAL(m.cTaxRule)=1,0,Tax_Amt))*;
               m.nHstRate/100,m.nHstAmt)

  m.TotalChg = ShipAmt+Freight+Insur+COD+Discount+Tax_Amt+m.nPstAmt+m.nHstAmt

  *C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
  IF ASCAN(laEvntTrig , PADR('SUMALL',10)) <> 0
    =gfDoTriger('ARIINV',PADR('SUMALL',10))
  ENDIF
  *C102314,1 BWA 06/10/2001 [END]

  *C102212,1 (End)
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
*B803571,1 NAD (Start) 10/25/2000 Added the ENDIF.
ENDIF
*B803571,1 NAD (End) 
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
FUNCTION lfvWeight

IF m.Weight <> &lcInvHdr..Weight 
  IF llIsEngland
    SELECT (lcInvHdr)  
    =RLOCK()
    REPLACE Weight WITH m.Weight
    UNLOCK
  ELSE
    *C102239,1 (Begin) Call standard UPS charge function for all customers except Bel05.
    *=lfvUpsChrg()
    IF !EMPTY(laData[3]) AND ASCAN(laEvntTrig , PADR('CRTCHRG',10)) <> 0
      llCalld   = .F.
      lcChrgTyp = ""
      =gfDoTriger('ARIINV',PADR('CRTCHRG',10))
    ELSE
      =lfvUpsChrg()
    ENDIF  
    *C102239,1 (End)
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
FUNCTION lfvCartons

IF m.Cartons <> &lcInvHdr..Cartons
  IF llIsEngland
    SELECT (lcInvHdr)  
    =RLOCK()
    REPLACE Cartons WITH m.Cartons
    UNLOCK
  ELSE
    *C102239,1 (Begin) Call standard UPS charge function for all customers except Bel05.
    *=lfvUpsChrg()
    IF !EMPTY(laData[3]) AND ASCAN(laEvntTrig , PADR('CRTCHRG',10)) <> 0
      llCalld   = .F.
      lcChrgTyp = ""
      =gfDoTriger('ARIINV',PADR('CRTCHRG',10))
    ELSE
      =lfvUpsChrg()
    ENDIF  
    *C102239,1 (End)
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

*B803571,1 NAD (Start) 10/25/2000 Not to allow users to add negative values.
IF lfNegChrg()
*B803571,1 NAD (End)

  SELECT (lcInvHdr)
  IF laSetups[9,2]='Y'     && Compute Tax
    IF llIsEngland
      SELECT (lcEngChrg)
      =SEEK(laData[1]+laData[5]+laData[3])
      SUM REST nChrgAmnt*(1-m.Trde_Disc/100)*(100-m.DiscPcnt)/100*nTaxRate/100 ;
      TO m.nChrgTax  ;
      WHILE Order+Store+PikTkt+cchrgcode = laData[1]+laData[5]+laData[3]
      SELECT (lcInvHdr)
      m.Tax_Amt = m.nChrgTax+nMerchTax*(100-m.DiscPcnt)/100*(100-m.Trde_Disc)/100
    ELSE  
      lnTaxAmount = IIF(UPPER(ALLTRIM(gcContCode))='USA',nTaxDue,ShipAmt)
      m.Tax_Amt = Tax_Rate*(IIF(laSetups[11,2]='M',lnTaxAmount,lnTaxAmount+m.COD+;
                  m.Insur+m.Freight)-m.DiscPcnt*lnTaxAmount/100)/100
      m.nPstAmt = IIF(llIsCanada,(ShipAmt+m.COD+m.Insur+m.Freight-;
                  ShipAmt*m.DiscPcnt/100+;
                  IIF(VAL(cTaxRule)=1,0,m.Tax_Amt))*nPstRate/100,0)
      *C102212,1 (Begin) Calculate HST Tax from amount after deducting discount.
      m.nHstAmt  = IIF(llIsCanada,(ShipAmt+m.COD+m.Insur+m.Freight-;
                  ShipAmt*m.DiscPcnt/100+IIF(VAL(cTaxRule)=1,0,m.Tax_Amt))*nHstRate/100,0)
      *C102212,1 (End)
    ENDIF
  ENDIF
  IF Cod_Flag='Y'

    *B603989,1 NAD 10/26/2000 (Start) If the UPS Type is OTHER don't add the Frieght, COD charges to the COD amount.
    IF EMPTY(lcUPSType) OR lcUPSType='OTHER'
      m.Cod_Amt = ShipAmt-ShipAmt*m.DiscPcnt/100 +m.Tax_Amt+m.nPstAmt
    ELSE
    *B603989,1 NAD 10/26/2000 (End)
      m.Cod_Amt = ShipAmt+m.Freight+m.Insur+m.COD+nCharges-;
                ShipAmt*m.DiscPcnt/100+m.Tax_Amt+m.nPstAmt
    *B603989,1 NAD 10/26/2000 (Start) Added the end if 
    ENDIF
    *B603989,1 NAD 10/26/2000 (End)

  *C102239,1 (Begin) Update COD for all customers except Bel05.
  *IF !llIsEngland .AND. laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. ;
       Cartons=1 .AND. SEEK(Order+Store+Piktkt,lcUpsBox)
  IF !llIsEngland .AND. laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' ;
     AND (EMPTY(laData[3]) OR ASCAN(laEvntTrig , PADR('CRTCHRG',10)) = 0) AND ;
     Cartons=1 AND SEEK(Order+Store+PikTkt,lcUpsBox)
  *C102239,1 (End)
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH m.Cod_Amt
      UNLOCK
    ENDIF
  ENDIF

  *B802674,1 (Start)
  *m.TotalChg = ShipAmt+m.Freight+m.Insur+m.COD+nCharges-;
               ShipAmt*m.DiscPcnt/100+m.Tax_Amt+m.nPstAmt
  *B802653,1 (Start)
  *m.TotalChg = ShipAmt+m.Freight+m.Insur+m.COD+nCharges-;
               ROUND(ShipAmt*m.DiscPcnt/100+m.Tax_Amt+m.nPstAmt,2)
  *C102212,1 (Begin) Add HST Tax to the total charge.
  *m.TotalChg = ShipAmt+m.Freight+m.Insur+m.COD+nCharges-;
               ROUND(ShipAmt*m.DiscPcnt/100,2)+m.Tax_Amt+m.nPstAmt
  m.TotalChg = ShipAmt+m.Freight+m.Insur+m.COD+nCharges-;
               ROUND(ShipAmt*m.DiscPcnt/100,2)+m.Tax_Amt+m.nPstAmt+m.nHstAmt

  *C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
  IF ASCAN(laEvntTrig , PADR('SUMALL',10)) <> 0
    =gfDoTriger('ARIINV',PADR('SUMALL',10))
  ENDIF
  *C102314,1 BWA 06/10/2001 [END]

  *C102212,1 (End)
  *B802653,1 (End)             
  *B802674,1 (End)

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
  *C102212,1 (Begin) Update HST Tax amount.
  REPLACE nHstAmt   WITH m.nHstAmt
  *C102212,1 (End)
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
      m.DiscPcnt = ABS(m.Discount)*100/ShipAmt
      =gfRltFld(ShipVia,@laVRltFld,'SHIPVIA')
      =lfvInvChrg(.T.)
    ENDIF
  ENDIF
  SET ORDER TO TAG (lcInvHdr)
  GO lnRecNo
  *C102212,1 (Begin) Add HST Tax.
  *SCATTER MEMVAR FIELDS COD,Insur,Freight,nPstAmt,Tax_Amt,Cod_Amt,TotalChg,DiscPcnt,Discount
  SCATTER MEMVAR FIELDS COD,Insur,Freight,nPstAmt,nHstAmt,Tax_Amt,Cod_Amt,TotalChg,DiscPcnt,Discount
  *C102212,1 (End)
  =gfRltFld(ShipVia,@laVRltFld,'SHIPVIA')
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
*B803571,1 NAD (Start) 10/25/2000 Added the ENDIF.
ENDIF
*B803571,1 NAD (End) 
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

*B803571,1 NAD (Start) 10/25/2000 Not to allow users to add negative values.
IF lfNegChrg()
*B803571,1 NAD (End) 
  *C102239,1 (Begin) Update COD for all customers except Bel05.
  *  IF !llIsEngland .AND. laSetups[15,2]='Y' .AND. m.Cartons=1 .AND. ;
     SEEK(laData[1]+laData[5]+laData[3],lcUpsBox)
  IF !llIsEngland .AND. laSetups[15,2]='Y' .AND. m.Cartons=1 ;
     AND (EMPTY(laData[3]) OR ASCAN(laEvntTrig , PADR('CRTCHRG',10)) = 0) AND ;
     SEEK(laData[1]+laData[5]+laData[3],lcUpsBox)
  *C102239,1 (End)
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
*B803571,1 NAD (Start) 10/25/2000 Added the ENDIF.
ENDIF
*B803571,1 NAD (End) .
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
*B801882,1 Recompute USA charges when needed.
*=lfvUpsChrg()
*C102239,1 (Begin) Call standard UPS charge function for all customers except Bel05.
*=lfvUpsChrg(.F.,'I')
IF !EMPTY(laData[3]) AND ASCAN(laEvntTrig , PADR('CRTCHRG',10)) <> 0
  llCalld   = .F.
  lcChrgTyp = "I"
  =gfDoTriger('ARIINV',PADR('CRTCHRG',10))
ELSE
  =lfvUpsChrg(.F.,'I')
ENDIF  
*C102239,1 (End)
*B801882,1 (End)

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
  m.Cod_Amt = IIF(m.Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0)
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
                 Cod_Amt  WITH IIF(llCod,ShipAmt+nCharges+Tax_Amt+Discount,0)  ;
    WHILE Consol+Account+cDivision+cCurrCode = 'N'+laData[4]+laData[13]+laData[14]
    =SEEK('Y'+laData[4]+laData[13]+laData[14])
    SET ORDER TO TAG (lcInvHdr)
  ENDIF
  SHOW GET m.Cod_Amt
  SHOW GET m.CODTAG
  SHOW GET m.CCODTRCKNO
ELSE
  *B801882,1 Recompute USA charges when needed.
  *=lfvUpsChrg()
  *C102239,1 (Begin) Call standard UPS charge function for all customers except Bel05.
  *=lfvUpsChrg(.F.,'C')
  IF !EMPTY(laData[3]) AND ASCAN(laEvntTrig , PADR('CRTCHRG',10)) <> 0
    llCalld   = .F.
    lcChrgTyp = "C"
     =gfDoTriger('ARIINV',PADR('CRTCHRG',10))
  ELSE
    =lfvUpsChrg(.F.,'C')
  ENDIF  
  *C102239,1 (End)
  *B801882,1 (End)

ENDIF
llOpnCrdt = .F.
IF llCod AND m.TotalChg > 0
  *E301077,50 Inhance openning files to speed up transaction
  *SELECT CREDIT
  *=SEEK(laData[4])
  *LOCATE REST WHILE Account+Tran+DTOS(TranDate) = laData[4] FOR ;
  *cCurrCode = laData[14]
  *llOpnCrdt = FOUND()
  DO gfAcOpCrdt IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
                   laData[4],laData[14],'llOpnCrdt'
  *E301077,50 (End)
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
*E301077,50 Inhance openning files to speed up transaction
IF !USED(lcAppCrdt)
  =gfOpenFile(gcDataDir+'ARHIST', gcDataDir+'ARHIST','SH')
  SELECT ARHIST
  =AFIELDS(laFileStru)
  =gfCloseFile('ARHIST')
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+6,4]
  laFileStru[lnFileStru+1,1] = 'Order'
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
  =gfCrtTmp(lcAppCrdt,@laFileStru,[Account+Order+cStore+PikTkt+Tran],lcAppCrdt)
ENDIF
*E301077,50 (End)

llKeyOff = &lcInvHdr..lKeyOff
DO gpOpnCredits IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
laData[4],laData[1],laData[5],laData[3],lnKeyOffAmnt,'llKeyOff',lcAppCrdt,laData[14]
SELECT (lcAppCrdt)
=SEEK(laData[4]+laData[1]+laData[5]+laData[3])
SUM REST Amount TO lnApplied WHILE ;
Account+Order+cStore+PikTkt=laData[4]+laData[1]+laData[5]+laData[3]


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

*B801882,1 Add new parameters for charge type
*PARAMETERS llCalled
PARAMETERS llCalled,lcChrgType
lcChrgType=IIF(TYPE('lcChrgType')='C',lcChrgType,'A')
*B801882,1 (End)

WAIT 'Computing Invoice Charges...' WINDOW NOWAIT
SELECT (lcInvHdr)
SET ORDER TO TAG CONSOL
lcKey = Account+cDivision+cCurrCode
IF Consol='Y'
  =SEEK('N'+lcKey)
  SCAN REST WHILE Consol+Account+cDivision+cCurrCode='N'+lcKey
    SCATTER FIELDS &lcScFields TO laData
    *C102212,1 (Begin) Add HST Tax.
    *SCATTER MEMVAR FIELDS SHIPAMT,DISCPCNT,TAX_RATE,cTaxRule,nPstRate,;
    ShipVia,COD_AMT,FREIGHT,INSUR,COD,CARTONS,WEIGHT,TAX_AMT,nPstAmt,TOTALCHG
    SCATTER MEMVAR FIELDS SHIPAMT,DISCPCNT,TAX_RATE,cTaxRule,nPstRate,nHstRate,;
    ShipVia,COD_AMT,FREIGHT,INSUR,COD,CARTONS,WEIGHT,TAX_AMT,nPstAmt,nHstAmt,TOTALCHG
    *C102212,1 (End)
    
    *B801882,1 Add new parameters for charge type
    *=lfvUpsChrg(.T.)
    =lfvUpsChrg(.T.,lcChrgType)
    *B801882,1 (End)
  ENDSCAN
  =SEEK('Y'+lcKey)
  =RLOCK()
  lnTaxAmount = IIF(UPPER(ALLTRIM(gcContCode))='USA',nTaxDue,ShipAmt)

  *B602443,1 Check numeric overflow when compute tax rate
  *REPLACE lCompUps WITH .F.        ,;
          Cod_Flag WITH m.Cod_Flag ,;
          lUpsIns  WITH m.lUpsIns  ,;
          Tax_Rate WITH Tax_Amt*100/(IIF(laSetups[11,2]='M',lnTaxAmount,;
                        lnTaxAmount+Freight+Insur+COD)-DiscPcnt*lnTaxAmount/100) ,;
          nPstRate WITH nPstAmt*100/(ShipAmt+Freight+Insur+COD+Discount+;
                        IIF(VAL(m.cTaxRule)=1,0,Tax_Amt))

  lnGstChrg = IIF(laSetups[11,2]='M',lnTaxAmount,;
                  lnTaxAmount+Freight+Insur+COD)-DiscPcnt*lnTaxAmount/100
  lnPstChrg = ShipAmt+Freight+Insur+COD+Discount+IIF(VAL(m.cTaxRule)=1,0,Tax_Amt)
  REPLACE lCompUps WITH .F.        ,;
          Cod_Flag WITH m.Cod_Flag ,;
          lUpsIns  WITH m.lUpsIns  ,;
          Tax_Rate WITH IIF(lnGstChrg=0,0,Tax_Amt*100/lnGstChrg) ,;
          nPstRate WITH IIF(lnPstChrg=0,0,nPstAmt*100/lnPstChrg)
  *C102212,1 (Begin) Update HST Tax.          
  REPLACE nHstRate WITH IIF(lnPstChrg=0,0,nHstAmt*100/lnPstChrg)
  *C102212,1 (End)
  *B602443,1 (End)
  UNLOCK
  SCATTER MEMVAR FIELDS &lcFlFields
  SCATTER FIELDS &lcScFields TO laData
ELSE
  *B801882,1 Add new parameters for charge type
  *DO gpUpsChrg IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
  laData[4],laData[5],laData[1],ladata[3],laData[7],m.SHIPAMT,m.Cod_Flag='Y',;
  m.lUpsIns,m.DISCPCNT,m.TAX_RATE,m.cTaxRule,m.nPstRate,lcUpsBox,'m.ShipVia',;
  'm.COD_AMT','m.FREIGHT','m.INSUR','m.COD','m.CARTONS','m.WEIGHT','m.TAX_AMT',;
  'm.nPstAmt','m.TOTALCHG',IIF(UPPER(ALLTRIM(gcContCode))='USA',&lcInvHdr..nTaxDue,m.SHIPAMT)

  DO gpUpsChrg IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
  laData[4],laData[5],laData[1],ladata[3],laData[7],m.SHIPAMT,m.Cod_Flag='Y',;
  m.lUpsIns,m.DISCPCNT,m.TAX_RATE,m.cTaxRule,m.nPstRate,lcUpsBox,'m.ShipVia',;
  'm.COD_AMT','m.FREIGHT','m.INSUR','m.COD','m.CARTONS','m.WEIGHT','m.TAX_AMT',;
  'm.nPstAmt',IIF(UPPER(ALLTRIM(gcContCode))='USA',&lcInvHdr..nTaxDue,;
  m.SHIPAMT),lcChrgType
  
  *B802653,1 (Start)
  *m.TotalChg = m.ShipAmt+m.Freight+m.Insur+m.Cod-ABS(m.ShipAmt*m.DiscPcnt/100)+;
               m.Tax_Amt+m.nPstAmt
  *C102212,1 (Begin) Calculate HST Tax and add it to the total charge.
  *m.TotalChg = m.ShipAmt+m.Freight+m.Insur+m.Cod+m.Discount+;
               m.Tax_Amt+m.nPstAmt
  m.nHstAmt  = IIF(llIsCanada,(ShipAmt+m.COD+m.Insur+m.Freight-;
                   M.ShipAmt*m.DiscPcnt/100+IIF(VAL(cTaxRule)=1,0,m.Tax_Amt))*nHstRate/100,0)

  m.TotalChg = m.ShipAmt+m.Freight+m.Insur+m.Cod+m.Discount+m.Tax_Amt+m.nPstAmt+m.nHstAmt
   
  *C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
  IF ASCAN(laEvntTrig , PADR('SUMALL',10)) <> 0
    =gfDoTriger('ARIINV',PADR('SUMALL',10))
  ENDIF
  *C102314,1 BWA 06/10/2001 [END]

  *C102212,1 (End)
  *B802653,1 (End)
  *B801882,1 (End)
  
  SELECT (lcInvHdr)
  lnFreight  = Freight
  lnInsur    = INSUR
  lnCod      = COD
  lnOldCart  = CARTONS
  lnWeight   = WEIGHT
  lnTaxAmnt  = TAX_AMT
  lnPstAmt   = nPstAmt
  *C102212,1 (Begin) Get HST Tax.          
  lnHstAmt   = nHstAmt
  *C102212,1 (End)
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
    *C102212,1 (Begin) Update HST Tax.          
    REPLACE nHstAmt WITH nHstAmt - lnHstAmt  + m.nHstAmt
    *C102212,1 (End)
    UNLOCK
  ENDIF
  GO lnRecNo
  m.lCompUps = .F.
  =RLOCK()
  *C102212,1 (Begin) Update HST Tax.          
  *GATHER MEMVAR FIELDS SHIPAMT,Cod_Flag,lUpsIns,DISCPCNT,TAX_RATE,cTaxRule,;
  nPstRate,ShipVia,COD_AMT,FREIGHT,INSUR,COD,CARTONS,WEIGHT,TAX_AMT,;
  nPstAmt,TOTALCHG,lCompUps
  GATHER MEMVAR FIELDS SHIPAMT,Cod_Flag,lUpsIns,DISCPCNT,TAX_RATE,cTaxRule,;
  nPstRate,ShipVia,COD_AMT,FREIGHT,INSUR,COD,CARTONS,WEIGHT,TAX_AMT,;
  nPstAmt,nHstAmt,TOTALCHG,lCompUps
  *C102212,1 (End)

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

*C102239,1 (Begin) Call BEL05 custom carton infromation screen.
IF !EMPTY(laData[3]) AND ASCAN(laEvntTrig , PADR('CALLCRT',10)) <> 0
  =gfDoTriger('ARIINV',PADR('CALLCRT',10))
  RETURN
ENDIF     
*C102239,1 (End)

DO gpUpsBox IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
lcUpsBox,laData[11],laData[4],laData[1],laData[5],laData[3],m.lUpsIns,;
(m.Cod_Flag='Y'),m.SHIPAMT,m.ShipVia,laData[7],'m.Cartons','m.Weight',;
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
DO gpCharges IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
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
          Cod_Amt   WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0) ,; 
          TotalChg  WITH ShipAmt+nCharges+Tax_Amt+Discount
  UNLOCK
ENDIF
SET ORDER TO TAG (lcInvHdr)
GO lnRecNo
=RLOCK()
REPLACE nCharges  WITH m.nCharges ,;
        nChrgTax  WITH m.nChrgTax ,;
        Tax_Amt   WITH nChrgTax+nMerchTax*(100-m.DiscPcnt)/100*(100-m.Trde_Disc)/100 ,;
        Cod_Amt   WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0) ,; 
        TotalChg  WITH ShipAmt+nCharges+Tax_Amt+Discount
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
DEFINE BAR 4 OF _INQURYPOP  PROMPT '\<Style Inquire'    SKIP FOR (lnactfolder<>3) .OR. EMPTY(m.Style)
DEFINE BAR 5 OF _INQURYPOP  PROMPT '\<Line Notepad'     SKIP FOR (lnactfolder<>3) .OR. laSetups[3,2]<>'Y' .OR. EMPTY(m.Style)
DEFINE BAR 6 OF _INQURYPOP  PROMPT '\<Copy Line'        SKIP FOR (lnactfolder<>3) .OR. EMPTY(m.Style) .OR. llZoom
DEFINE BAR 7 OF _INQURYPOP  PROMPT '\<Paste Line'       SKIP FOR (lnactfolder<>3) .OR. EMPTY(m.Style) .OR. llZoom
DEFINE BAR 8 OF _INQURYPOP  PROMPT 'Add Pac\<ks'        SKIP FOR laSetups[1,2]<>'Y' .OR. laSetups[8,2]='Y' OR lnactfolder<>3 .OR. laScrMode[1] .OR. llZoom
DEFINE BAR 9 OF _INQURYPOP  PROMPT '\<Zoom Invoice Lines' SKIP FOR (lnactfolder<>3)

*sameh
*DEFINE BAR 10 OF _INQURYPOP PROMPT 'Session \<Defaults'  SKIP FOR !laScrMode[1]
DEFINE BAR 10 OF _INQURYPOP PROMPT 'Session \<Defaults'  SKIP FOR .T.

DEFINE BAR 11 OF _INQURYPOP PROMPT '\<Batch Summary'  SKIP FOR laScrMode[1]
DEFINE BAR 12 OF _INQURYPOP PROMPT 'Scope \<Orders'   SKIP FOR !laScrMode[1]

*B603395,1 [Start] new bar if calculate Sales Rep. commesion for lines OR add mode
DEFINE BAR 13 OF _INQURYPOP PROMPT '\<Refresh Invoice Sales Rep. Commission' SKIP FOR laSetups[2,2] <> 'Y' OR !laScrMode[4]
*B603395,1 [End]

*C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
IF ASCAN(laEvntTrig , PADR('FOLDER',10)) <> 0
  =gfDoTriger('ARIINV',PADR('FOLDER',10))
ENDIF
*C102314,1 BWA 06/10/2001 [END]

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
  	lcParameter = "'"+m.Style+"',''"
    DO gpDoProg WITH 'AWRICSTYLE',.F.,'IC',lcParameter
  CASE BAR() = 5      &&  Invoice Line Notepad
    =lfvLineNote()
  CASE BAR() = 6      &&  Copy Invoice Line
   =lfvCopyQty()
  CASE BAR() = 7      &&  Paste Invoice LIne
   =lfvPasteQty()
  CASE BAR() = 8      &&  Add Packs
   =lfvPacks()
  CASE BAR() = 9      &&   Zoom Invoice Browse
    llZoom = !llZoom
    =lfvBrowZoom()
    SET MARK OF BAR 9 OF _INQURYPOP TO llZoom
  CASE BAR() = 10      &&   Session Defaults
    *B801942,1 Store session defaults
    llCancel=.F.
    ldOldInvDate = ldDefInvDate 
    ldOldPstDate = ldDefPstDate
    *B801942,1 (End)
    
    DO (gcScrDir+gcWinAppl+"\ARINVSES.SPX")
    *B801942,1 Restore session defaults
    IF llCancel
      ldDefInvDate = ldOldInvDate
      ldDefPstDate = ldOldPstDate
    ENDIF
    *B801942,1 (End)
    
  CASE BAR() = 11     &&   Batch Summary
    =lfBatchSum(.F.)
  CASE BAR() = 12     &&   Scope Orders
    =lfSetScope()

  *B603395,1 [Start] 
   CASE BAR() = 13     && Refresh sales rep. header commession befor saving
     =lfRefComm()
  *B603395,1 [End]

  *C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
  *--Show the charge screen for the selected order.
  CASE BAR() = 14
    IF ASCAN(laEvntTrig , PADR('CHARGSCR',10)) <> 0
      =gfDoTriger('ARIINV',PADR('CHARGSCR',10))
    ENDIF
  *C102314,1 BWA 06/10/2001 [END]

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
lcOrdKey = Account+Order+Store+PikTkt
SET ORDER TO TAG 'SELECT'
=SEEK("�")
*B803654,1 09/17/2000 NAD (Start) Show balance to pay instead of ship amount in the batch summary.
*SUM REST Ship,ShipAmt,Cartons,Weight  ;
         TO lnAllShip,lnAllAmnt,lnAllCtns,lnAllWght ;
WHILE cSelect+Account+Order+Store+PikTkt = "�" ;
FOR (Consol='Y' OR EMPTY(Flag))  && AND Ship > 0

SUM REST Ship,TotalChg,Cartons,Weight  ;
         TO lnAllShip,lnAllAmnt,lnAllCtns,lnAllWght ;
    WHILE cSelect+Account+Order+Store+PikTkt = "�" ;
    FOR   (Consol='Y' OR EMPTY(Flag))  && AND Ship > 0

*B803654,1 09/17/2000 NAD (End)
lnAllInvs = _TALLY
llProceed = .F.

IF lnAllInvs > 0
  DO (gcScrDir+gcWinAppl+"\ARIINVS.SPX") WITH llSave
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
=lfRefresh(lcWinCh42)
=lfRefresh(lcWinCh44)
SHOW GET ibStyle ENABLE
SHOW GET m.Style ENABLE
_CUROBJ = OBJNUM(m.Style)

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

*E300817,1 Message : 40011
*E300817,1 Are you sure you want to remove this xxx?
*E300817,1 Button : 40000
*E300817,1 Yes No
IF gfModalGen('QRM40011B40000','ALERT','invoice line')= 1

  *B602905,1 Default No. of cartons must be 1 at least [Begin]
  lnCartons  = &lcInvHdr..nCartons - IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0)
  m.Cartons  = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
  *B602905,1 Default No. of cartons must be 1 at least [End  ]
  m.Weight   = m.Weight - m.TotQty*Style.nStyWeight
  m.ShipAmt  = m.ShipAmt - m.TotQty*m.Price
  m.Ship     = m.Ship - m.TotQty
  m.Discount = -1*m.DiscPcnt*m.ShipAmt/100
  lnQuantity = 0
  IF laSetups[9,2]='Y' .AND. llIsEngland .AND. Style.nTaxBreak <> 0
    FOR lnCount = Style.nTaxBreak TO 8
      lnQuantity = lnQuantity - EVAL('m.Qty'+STR(lnCount,1))
    ENDFOR
  ENDIF
  m.nTaxDue = &lcInvHdr..nTaxDue - IIF(lTaxable,m.TotQty*m.Price,0)
  STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty
  =IIF(llUpCnsLine,lfUpCnsLine() AND lfUpCnsCtn(&lcInvHdr..Cartons,m.Cartons),.T.)
  SELECT (lcInvHdr)  
  =RLOCK()  
  REPLACE CARTONS   WITH m.Cartons ,;
          nCartons  WITH lnCartons ,;
          WEIGHT    WITH m.Weight  ,;
          SHIPAMT   WITH m.ShipAmt ,;
          SHIP      WITH m.Ship    ,;
          Discount  WITH m.Discount,;
          lCompUps  WITH !llIsEngland ,;
          nMerchTax WITH nMerchTax +  lnQuantity * Price * nTaxRate/100 ,;
          Tax_Amt   WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
          Cod_Amt   WITH IIF(Cod_Flag='Y',IIF(llIsEngland,ShipAmt+nCharges+;
                             Tax_Amt+Discount,Cod_Amt),0) ,; 
          TotalChg  WITH IIF(llIsEngland,ShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
          nTaxDue   WITH m.nTaxDue
  UNLOCK
  SELECT (lcInvLine)
  DELETE
  *B603916,1 NAD 09/26/2000 (Start) To refresh the quantities fields               
  GO TOP
  *B603916,1 (End)                
  SCATTER MEMVAR
  SHOW WINDOW (lcInvBrow) REFRESH SAME
  SHOW GETS WINDOW (lcWinCh42) ONLY
  SHOW GETS WINDOW (lcWinCh44) ONLY
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
PRIVATE laLineInfo,lcStyle,lcLineNo,lcTag
*B603648,1 (Start) Make lnMerchTax private variable
PRIVATE lnMerchTax
*B603648,1 (End) 
lcTag = ORDER(lcInvHdr)
SET ORDER TO TAG 'CONSOL' IN (lcInvHdr)
=SEEK('Y'+laData[4]+laData[13]+laData[14],lcInvHdr)
SELECT (lcInvLine)
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,Price,;
               Gros_Price TO laLineInfo
lnMerchTax = 0
IF laSetups[9,2]='Y' AND llIsEngland
  FOR lnSize = 1 TO Scale.Cnt
    lnMerchTax = lnMerchTax + IIF(BETWEEN(lnSize,Style.nTaxBreak,8),;
    EVAL('m.QTY'+STR(lnSize,1))*m.Price-laLineInfo[lnSize]*laLineInfo[10],0)
  ENDFOR
ENDIF
lnTaxDue = &lcInvHdr..nTaxDue + IIF(lTaxable,m.TotQty*m.Price-laLineInfo[9]*laLineInfo[10],0)
lcLineNo = STR(LineNo,6)
lcStyle  = Style
SET ORDER TO TAG 'CONSOL'
IF !SEEK('Y'+laData[4]+laData[13]+laData[14]+lcStyle)
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE LastLine WITH LastLine + 1
  UNLOCK
  INSERT INTO (lcInvLine) ;
  (Account,cDivision,cCurrCode,Order,Style,Season,Consol,cWareCode,LineNo,lNewLine) VALUES ;
  (laData[4],laData[13],laData[14],&lcInvHdr..Order,lcStyle,m.Season,'Y',;
   laData[7],&lcInvHdr..LastLine,.T.)
ELSE
  REPLACE Qty1       WITH Qty1 - laLineInfo[1] + m.Qty1 ,;
          Qty2       WITH Qty2 - laLineInfo[2] + m.Qty2 ,;
          Qty3       WITH Qty3 - laLineInfo[3] + m.Qty3 ,;
          Qty4       WITH Qty4 - laLineInfo[4] + m.Qty4 ,;
          Qty5       WITH Qty5 - laLineInfo[5] + m.Qty5 ,;
          Qty6       WITH Qty6 - laLineInfo[6] + m.Qty6 ,;
          Qty7       WITH Qty7 - laLineInfo[7] + m.Qty7 ,;
          Qty8       WITH Qty8 - laLineInfo[8] + m.Qty8 ,;
          TotQty     WITH TotQty - laLineInfo[9] + m.TotQty ,;
          nNetAmnt   WITH nNetAmnt - laLineInfo[9]*laLineInfo[10]+m.TotQty*m.Price,;
          nGrosAmnt  WITH nGrosAmnt- laLineInfo[9]*laLineInfo[11]+m.TotQty*m.Gros_Price,;
          Price      WITH IIF(TotQty=0,0,nNetAmnt/TotQty) ,;
          Gros_Price WITH IIF(TotQty=0,0,nGrosAmnt/TotQty),;
          Disc_Pcnt  WITH IIF(nGrosAmnt=0,0,(nGrosAmnt-nNetAmnt)/nGrosAmnt)
  IF TotQty = 0 AND lNewLine
    DELETE
  ENDIF
ENDIF  
SELECT (lcInvLine)
SET ORDER TO TAG (lcInvLine)

*B603068 (Start) comment this line
*SET KEY TO laData[4]+laData[1]+laData[5]+laData[3]
IF lnActFolder = 3
  SET KEY TO laData[4]+laData[1]+laData[5]+laData[3]
ELSE
  SET KEY TO
ENDIF
*B603068 (End)

=SEEK(laData[4]+laData[1]+laData[5]+laData[3]+lcLineNo)
SELECT (lcInvHdr)
=RLOCK()

REPLACE WEIGHT    WITH Weight+(m.TotQty-laLineInfo[9])*Style.nStyWeight  ,;
        SHIPAMT   WITH ShipAmt+(m.TotQty*m.Price-laLineInfo[9]*laLineInfo[10])   ,;
        SHIP      WITH Ship + (m.TotQty-laLineInfo[9]) ,;
        Discount  WITH -1*DiscPcnt*ShipAmt/100 ,;
        lCompUps  WITH !llIsEngland ,; 
        nMerchTax WITH nMerchTax + lnMerchTax*m.nTaxRate/100 ,;
        Tax_Amt   WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
        Cod_Amt   WITH IIF(Cod_Flag='Y',IIF(llIsEngland,ShipAmt+nCharges+;
                           Tax_Amt+Discount,Cod_Amt),0) ,; 
        TotalChg  WITH IIF(llIsEngland,ShipAmt+nCharges+Tax_Amt+Discount,TotalChg),;
        nTaxDue   WITH lnTaxDue ,;
        cSelect   WITH IIF(llSelect,IIF(SHIP=0,'','�'),cSelect)
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
*! Name      : FUNCTION lfvStyle
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate entered style
*!*************************************************************
*! Calls     : lfNavigate(),gfStyBrw(),gfModalGen(),gfStyPrice(),gpAdStyWar(),
*!             lfUpCnsLine(),lfRefresh()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle
PRIVATE llAddStyle
IF lfNavigate()
  _CUROBJ = _CUROBJ
  RETURN
ENDIF  
IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF

llAddStyle = .T.
IF llBrowse .OR. ;
   ( (!llAddLine .OR. m.Style <> lcEmptySty) .AND. !SEEK(m.Style,'Style'))
  m.Style = PADR(gfStyBrw("I" ,m.Style, "" , .F.),19)
  STORE IIF(EMPTY(m.Style),IIF(llAddLine,lcEmptySty,&lcInvLine..Style),m.Style) TO m.Style
ENDIF
llBrowse = .F.
IF (llAddLine .AND. m.Style=lcEmptySty ) .OR. ;
   (!llAddLine .AND. m.Style=&lcInvLine..Style)
  llAddStyle = .F.
ENDIF
SELECT (lcInvLine) 
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO STYDYE
llAddStyle = llAddStyle .AND. SEEK(m.Style,'Style')

IF llAddStyle
  *E300817,1 Message : 40006
  *E300817,1 Style scale not found in the scale file. Cannot accept.
  *E300817,1 Button : 00000 
  *E300817,1 Ok

  *E300817,1 Message : 40007
  *E300817,1 This is a canceled style. Not allowed to invoice.
  *E300817,1 Button : 00000 
  *E300817,1 Ok

  *E300817,1 Message : 40008
  *E300817,1 This style/color is on hold. 
  *E300817,1 Button : 40001
  *E300817,1 Accept Reenter

  *E300817,1 Message : 40009
  *E300817,1 Styles restricted to XXX!
  *E300817,1 Button : 00000
  *E300817,1 Ok

  *E300817,1 Message : 40010
  *E300817,1 Style XXX date is 
  *E300817,1 Button : 40001
  *E300817,1 Accept Reenter

  *E300817,1 Message : 40085
  *E300817,1 No dyelots found for style xxxx in warehouse xxxxx
  *E300817,1 Button : 00000
  *E300817,1 Ok
  IF (!SEEK('S'+Style.Scale,'Scale') .AND. ;
      gfModalGen('TRM40006B00000','ALERT')=1) ;
   .OR. (Style.Status='X' .AND. ;
        gfModalGen('TRM40007B00000','ALERT')=1) ;
   .OR. (laSetups[8,2]='Y' .AND. Style.cDye_Flg='Y' .AND. !lfStyDye() .AND.;
        gfModalGen('TRM40085B00000','ALERT',lcStyHdr+': '+m.Style+IIF(laSetups[5,2]='Y',' in warehouse '+laData[7],''))=1) ;
   .OR. (Style.cDivision <> laData[13] .AND. ;
        gfModalGen('TRM40009B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;
   .OR. (ALLTRIM(laData[8])<>'*' AND TRIM(Style.Season)<>'Y' AND Style.Season<>laData[8] .AND. ;
        gfModalGen('TRM40009B00000','ALERT','season '+ALLTRIM(laSeasons[lnSeason,1]))=1) ;
   .OR. (Style.Status='H' .AND. ;
        gfModalGen('QRM40008B40001','ALERT')=2) ;
   .OR. (!EMPTY(Style.Start) .AND. Style.Start > laData[6] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','start|'+DTOC(Style.Start))=2) ;
   .OR. (!EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[6] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2)

    llAddStyle = .F.
  ENDIF
ENDIF
IF llAddStyle
  *E301142,1 Select alternative price level
  **E300834,1 Get style price according to ordered quantity
  *lcPriceCatg  = IIF(lcPriceLvl='Q',IIF(m.TotQty >= Style.nAtQtyC,'C',;
  *               IIF(m.TotQty>=Style.nAtQtyB,'B','A')),lcPriceLvl)
  *m.Gros_Price = IIF(!llMulCurr OR laData[14]=gcBaseCurr,Style.Price&lcPriceCatg,;
                     gfStyPrice(m.Style,lcPriceCatg,laData[14]))
  m.Gros_Price  = lfGetprice(m.Style,lcPriceLvl,m.TotQty)
  *E301142,1 (End)

  *C102052,1 ABD - Add new validation on the discount code up on 
  *C102052,1 ABD - add new option about whole sale or retail sale [begin]
  *-- get the cDiscCode From stydye in every case
  lcDiscCode = IIF(SEEK(m.Style+laData[7]+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
  STORE 0 TO m.Disc_Pcnt
  IF !EMPTY(ALLTRIM(lcDiscCode))
    *-- Get the discount related field to know which 
    *-- type whole Sale Or Retail sale Or Both.
    DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
    STORE '' To lcDisType , ldstartDte ,ldEndDate
    *-- Array to get the Discount affect for DecCode.
    laDisType[1,1]  = 'CCOSTAFECT'
    laDisType[1,2]  = 'lcDisType'
    *-- Array to get the start date For DescCode.
    lastartDte[1,1] = 'START'
    lastartDte[1,2] = 'ldStartDte'
    *-- Array to get the end date For DescCode.
    laEndDate[1,1]  = 'DENDATE'
    laEndDate[1,2]  = 'ldEndDate'
    = gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
    = gfRltFld(lcDiscCode , @laStartDte,'CDISCCODE')
    = gfRltFld(lcDiscCode , @laEndDate, 'CDISCCODE')
    *E300834,1 Get Style discount percent and Net Price.
    lnDisc_Pcnt = 0
    IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[6],ldStartDte,ldEndDate)
      *C102052,1 ABD - [End]
      lnDisc_Pcnt = m.Disc_Pcnt
      *C102052,1 ABD - remark next line and get cDiscCode From stydye file. [Begin]
      *=IIF(EMPTY(Style.CDISCCODE),.T.,gfRltFld(Style.CDISCCODE,@laDisRltFld,'CDISCCODE'))
      =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
      *E300834,1 Calculate net price
      m.Disc_Pcnt = lnDisc_Pcnt
      *C102052,1 ABD - [End]      
      *E300834,1 (End)
      *C102052,1 ABD - end if for if statement. [Begin]
    ENDIF
  ENDIF  
  m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100  
  *C102052,1 ABD - [End]
  llAddStyle = m.Gros_Price >= 0
ENDIF
IF llAddStyle
  IF laSetups[5,2]='Y' .AND. !SEEK(m.Style+laData[7]+SPACE(10),'StyDye')
    *E300817,1 Message : 40012
    *E300817,1 Style xxx is not assigned to warehouse xxx
    *E300817,1 Button : 40002
    *E300817,1 Add Reenter
    IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.Style)+'|'+TRIM(laData[7]))=1
      DO gpAdStyWar WITH m.Style,SPACE(10),laData[7]
    ELSE
      llAddStyle = .F.
    ENDIF
  ENDIF
ENDIF
SELECT (lcInvLine) 
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO STYLE+CWARECODE+DYELOT INTO STYDYE ADDITIVE
IF llAddStyle  
  lnTaxRate = 0
  SET ORDER TO TAG Styles
  IF SEEK(laData[4]+laData[1]+laData[5]+laData[3]+m.Style)
    *E300817,1 Message : 40013
    *E300817,1 Warning! This style had been entered on this invoice
    *E300817,1 Button : 00000
    *E300817,1 Ok
    =gfModalGen('INM40013B00000','ALERT')
  ENDIF
  SET ORDER TO TAG (lcInvLine)
  IF llAddLine
    m.Order   = laData[1]
    m.PikTkt  = laData[3]
    m.Account = laData[4]
    m.Store   = laData[5]
    m.InvDate = laData[6]
    m.cDivision= laData[13]
    m.cCurrCode=laData[14]
    m.cWarecode= laData[7]
    m.lNewLine=.T.
    *B803174,1 (Start)  To be able to browse the new styles with refresh same.  
    m.Consol=&lcInvHdr..Consol
    *B803174,1  (End) 
    SELECT (lcInvHdr)
    =RLOCK()
    REPLACE LastLine WITH LastLine + 1
    UNLOCK
    SELECT (lcInvLine)
    m.LineNo  = &lcInvHdr..LastLine
    INSERT INTO (lcInvLine) FROM MEMVAR
    =SEEK(laData[4]+laData[1]+laData[5]+laData[3]+STR(m.LineNo,6))
  ELSE
    =SEEK(laData[4]+laData[1]+laData[5]+laData[3]+STR(m.LineNo,6))  
    REPLACE AltStyle WITH IIF(EMPTY(AltStyle),Style,AltStyle) ,;
            Style    WITH m.Style ,;
            Qty1     WITH IIF(Scale.Cnt>=1,Qty1,0) ,;
            Qty2     WITH IIF(Scale.Cnt>=2,Qty2,0) ,;
            Qty3     WITH IIF(Scale.Cnt>=3,Qty3,0) ,;
            Qty4     WITH IIF(Scale.Cnt>=4,Qty4,0) ,;
            Qty5     WITH IIF(Scale.Cnt>=5,Qty5,0) ,;
            Qty6     WITH IIF(Scale.Cnt>=6,Qty6,0) ,;
            Qty7     WITH IIF(Scale.Cnt>=7,Qty7,0) ,;
            Qty8     WITH IIF(Scale.Cnt=8,Qty8,0) ,;
            TotQty   WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
  ENDIF
  lnTaxRate =0
  IF llIsEngland .AND. !Customer.lVatExem  .AND. Style.nTaxBreak <> 0
    =gfRltFld(Style.cTaxCode,@laEngStyTax,'CTAXCODE')  
  ENDIF
  m.Desc1  = Style.Desc1
  m.Scale  = Style.Scale
  m.PrePak = Style.PrePak
  m.Comm1  = IIF(Style.commission,laData[9],0)
  m.Comm2  = IIF(Style.commission,laData[10],0)
  m.lTaxable = STYLE.lTAXABLE
  REPLACE Desc1    WITH m.Desc1 ,;
          Scale    WITH m.Scale ,;
          PrePak   WITH m.PrePak,;
          Season   WITH Style.Season ,;
          nTaxRate WITH lnTaxRate ,;
          Comm1    WITH m.Comm1 ,;
          Comm2    WITH m.Comm2 ,;
          lTaxable WITH m.lTaxable
  STORE .T. TO llCUpdate
  =IIF(llUpCnsLine,lfUpCnsLine(),.T.)
ELSE
  SCATTER MEMVAR MEMO
ENDIF
STORE .F. TO llAddLine
*B803656,1 NAD 09/14/2000 (Start) Set the filter again.
SET KEY TO laData[4]+laData[1]+laData[5]+laData[3]
*B803656,1 NAD 09/14/2000 (End)
SHOW WINDOW (lcInvBrow) REFRESH SAME
SHOW GETS WINDOW (lcWinCh42) ENABLE ONLY
SHOW GETS WINDOW (lcWinCh44) ENABLE ONLY
SHOW GETS WINDOW (lcWinCh45) ENABLE ONLY
=lfRefresh(lcWinCh42)
=lfRefresh(lcWinCh44)
=lfRefresh(lcWinCh46)
*B602760,1 NAD (Start) Disable Style Prepack code and quantity if style scale does not have prepacks
=lfPpStat()
*B602760,1 NAD (End)
*!*************************************************************
*! Name      : FUNCTION lfStyDye
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

*B603411,1 (Start) Fix the bug of long delay after entring style/color

*SELECT * FROM STYDYE WHERE style+cwarecode+dyelot+STR(RECNO(),7) = ;
*m.Style+laData[7] .AND. !EMPTY(Dyelot) INTO ARRAY laDyelots
*RETURN(_TALLY > 0)

IF SEEK (m.style+laData[7]+SPACE(10),'StyDye')
  SKIP IN StyDye
  RETURN (STYDYE.Style+STYDYE.cwarecode=m.Style+laData[7] AND !EMPTY(STYDYE.Dyelot))
ELSE
  RETURN .F.
ENDIF 
*B603411,1 (End)   
*!*************************************************************
*! Name      : FUNCTION lfvDyelot
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
IF (llBrowse .OR. EMPTY(lcDyelot) .OR. !SEEK(m.Style+laData[7]+lcDyelot,'StyDye')) .AND. ;
  !SDYEBROW(m.Style,@lcDyelot,.T.,IIF(laSetups[5,2]='Y',laData[7],''))
  lcDyelot = Dyelot
ENDIF
m.Dyelot = lcDyelot
llBrowse = .F.
IF Dyelot <> m.Dyelot
  IF !EMPTY(m.Dyelot)
    =RLOCK()
    REPLACE Dyelot WITH m.Dyelot
    UNLOCK
    IF INLIST(laSetups[7,2],'F','L')
      FOR lnCount = 1 TO Scale.Cnt
        IF EVAL('m.Qty'+STR(lnCount,1)) > 0 AND ;
           EVAL('m.Qty'+STR(lnCount,1)) > EVAL('StyDye.Stk'+STR(lnCount,1))
          *E300408,1 Message : 40137
          *E300408,1 Shipped Quantity For Size XXXXX Cannot Exceed On Hand
          *E300408,1 Quantity (99999).
          *E300408,1 Button : 00000 
          *E300408,1 OK
          =gfModalGen('TRM40137B00000','ALERT',ALLTRIM(EVAL('Scale.Sz'+STR(lnCount,1)))+'|('+ALLTRIM(STR(EVAL('StyDye.Stk'+STR(lnCount,1)),5))+')')
          STORE MAX(EVAL('StyDye.Stk'+STR(lnCount,1)),0) TO ('m.Qty'+STR(lnCount,1))
          SHOW GET ('m.Qty'+STR(lnCount,1))
        ENDIF
      ENDFOR
    ENDIF
    =lfChkTotQty()
  ENDIF
  llCUpdate = .T.
  SHOW WINDOW (lcInvBrow) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvPacks
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Calls packs screen
*!*************************************************************
*! Calls     : lfAccPacks,gfModalGen,arPacks.SPX
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPacks()
*!*************************************************************
FUNCTION lfvPacks
PRIVATE lnAlias,lnTopMark,lnBotMark

lnAlias = SELECT()
*E301077,50 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPck_Hdr','SH')
=gfOpenFile(gcDataDir+'SPck_Lin',gcDataDir+'SPck_Lin','SH')
*E301077,50 (End)
SELECT SPck_Lin
SET RELATION TO Style INTO Style
SET RELATION TO Style+laData[7]+SPACE(10) INTO StyDye ADDITIVE
IF !lfAccPacks()
  *E300817,1 Message : 40034
  *E300817,1 There are neither generic packs nor packs for account xxx.
  *E300817,1 Button : 00000
  *E300817,1 Ok
  =gfModalGen('INM40034B00000','ALERT',laData[2])
  SELECT (lnAlias)
  RETURN
ENDIF
STORE 0  TO lnTopMark,lnBotMark
SELECT (lcPackHdr)
lcAccount = laData[4]
SET RELATION TO Type+Account+Pack_Id INTO SPck_Lin
DO (gcScrDir+"arPacks.SPX")
SELECT (lcPackHdr)
USE
ERASE (gcWorkDir+lcPackHdr+".DBF")
ERASE (gcWorkDir+lcPackHdr+".CDX")

*E301077,50 Inhance openning files to speed up transaction
=gfCloseFile('SPck_Hdr')
=gfCloseFile('SPck_Lin')
*E301077,50 (End)

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfAccPacks
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Create temporary file holding account packs and generic packs
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAccPacks()
*!*************************************************************
FUNCTION lfAccPacks
PRIVATE lnPacks

CREATE DBF (gcWorkDir+lcPackHdr) ;
        (Type C(1),Account C(5),Pack_Id C(16),Desc C(20),Season C(6),;
         cDivision C(6),TotWip N(6),TotStk N(6),TotOrd N(6),TotQty N(6),PackQty N(6))
INDEX ON Account+Pack_Id TAG (lcPackHdr)
lnPacks = 0
SELECT SPck_Hdr
SET RELATION TO Type+Account+Pack_Id INTO SPck_Lin
IF SEEK('P'+laData[4],'SPck_Hdr')
  SCAN REST WHILE Type+Account='P'+laData[4] ;
       FOR IIF(ALLTRIM(laData[8])='*',.T.,Season=laData[8]) .AND. cDivision=laDAta[13]
    SELECT SPck_Lin
    SUM REST WHILE Type+Account+Pack_Id='P'+laData[4]+Spck_Hdr.Pack_Id ;
                   StyDye.TotStk,StyDye.TotOrd,StyDye.TotWip,Spck_Lin.TotQty ;
                   TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
    INSERT INTO (lcPackHdr) ;
    (Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
     VALUES ('P',SPck_Hdr.Account,SPck_Hdr.Pack_Id,SPck_Hdr.Desc,SPck_Hdr.Season,;
            SPck_Hdr.cDivision,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
    lnPacks = lnPacks + 1
  ENDSCAN
ENDIF
IF SEEK('P*****','SPck_Hdr')
  SCAN REST WHILE Type+Account='P*****' ;
       FOR IIF(ALLTRIM(laData[8])='*',.T.,Season=laData[8]) .AND. cDivision=laData[13]
    SELECT SPck_Lin
    SUM REST WHILE Type+Account+Pack_Id='P*****'+Spck_Hdr.Pack_Id ;
                   StyDye.TotStk,StyDye.TotOrd,StyDye.TotWip,Spck_Lin.TotQty ;
                   TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
    INSERT INTO (lcPackHdr) ;
    (Type,Account,Pack_Id,Desc,Season,cDivision,TotWip,TotStk,TotOrd,TotQty);
     VALUES ('P',SPck_Hdr.Account,SPck_Hdr.Pack_Id,SPck_Hdr.Desc,SPck_Hdr.Season,;
            SPck_Hdr.cDivision,lnPackStk, lnPackOrd, lnPackWip, lnPackQty)
    lnPacks = lnPacks + 1
  ENDSCAN
ENDIF
SELECT SPck_Hdr
SET RELATION OFF INTO SPck_Lin
RETURN(lnPacks>0)

*!*************************************************************
*! Name      : lfwAccPacks
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Browse Account and generic packs
*!*************************************************************
*! Calls     : gfCodDes,lfSAccPack,lfPackDet
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwAccPacks()
*!*************************************************************
FUNCTION lfwAccPacks

SELECT (lcPackHdr)
lnTopMark = RECNO()
BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnTopMark,'>',' '):H=' ':R:1:W=.F.,;
       Account  :H='Account' :R,;
       Pack_Id  :H='Pack Id' :R ,;
       Desc     :H='Description':R ,;
       PackQty  :H='Pack' ,;
       TotWip   :H='Wip':R,;
       TotStk   :H='Stock':R,;
       TotOrd   :H='Ordered' :R,;
       TotQty   :H='Qty.' :R,;
       lcSesDesc=gfCodDes(Season,'SEASON') :H="Season" :R :20 ,;
       lcDivDesc=gfCodDes(cDivision,'CDIVISION') :H="Division" :20 :R;
       WINDOW arpacks1  ;
       IN WINDOW arpacks;
       NOMENU           ;         
       NOAPPEND         ;
       NODELETE         ;         
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
	   FREEZE 'PackQty' ;
       WHEN lfSAccPack() ;
       TITLE lcBrTtl1 

SELECT Spck_Lin
lnBotMark = RECNO()
BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnBotMark,'>',' '):H=' ':R:1:W=.F.,;
       Style :H = lcStyHdr :R ,;
       StyDye.TotWip :H= 'WIP' :R ,;
       StyDye.TotStk :H= 'Stock' :R ,;
       StyDye.TotOrd :H= 'Ordered' :R ,;
       TotQty :H='Qty.' :R ;
       WINDOW arpacks2   ;
       IN WINDOW arpacks ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WHEN lfPackDet() ;
       TITLE lcBrTtl2

*!*************************************************************
*! Name      : lfPackDet
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Show pack details
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfPackDet()
*!*************************************************************
FUNCTION lfPackDet
lnBotMark = RECNO('Spck_Lin')
SHOW WINDOW (lcBrTtl2) REFRESH SAME

*!*************************************************************
*! Name      : lfSAccPack
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Show Account and generic packs
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSAccPack()
*!*************************************************************
FUNCTION lfSAccPack
lnTopMark = RECNO(lcPackHdr)
SHOW WINDOW (lcBrTtl1) REFRESH SAME
lnBotMark = RECNO('Spck_Lin')
SHOW WINDOW (lcBrTtl2) REFRESH SAME

*!*************************************************************
*! Name      : lfDAccPack
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Deactivate function for packs screen
*!*************************************************************
*! Calls     : lpTab,lpBackTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfDAccPack()
*!*************************************************************
FUNCTION lfDAccPack

IF WONTOP() = lcBrTtl1 OR WONTOP() = lcBrTtl2
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
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

*!*************************************************************
*! Name      : lfvZoom
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Show pack line details
*!*************************************************************
*! Calls     : arpacksz.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvZoom()
*!*************************************************************
FUNCTION lfvZoom
DO (gcScrDir+"arpacksz.SPX")

*!*************************************************************
*! Name      : lfvPckApply
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Apply selected packs to order lines
*!*************************************************************
*! Calls     : gfModalGen,lfValidPack,gpAdStyWar
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvPckApply()
*!*************************************************************
FUNCTION lfvPckApply
PARAMETERS llStores,lcStore
PRIVATE lnMerchTax

SELECT (lcPackHdr)
LOCATE FOR PackQty <> 0
IF !FOUND()
  *E300817,1 Message : 40035
  *E300817,1 No packs selected
  *E300817,1 Button : 00000
  *E300817,1 Ok
  =gfModalGen('INM40035B00000','ALERT')
  GO TOP
  RETURN
ENDIF
STORE 0 TO lnMerchTax
STORE &lcInvHdr..nTaxDue  TO m.nTaxDue
STORE &lcInvHdr..nCartons TO lnCartons
SCAN FOR PackQty <> 0
  IF !lfValidPack()
    LOOP
  ENDIF
  SELECT Spck_Lin
  SCAN REST WHILE Type+Account+Pack_Id=&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id

    WAIT 'Adding '+ALLTRIM(Pack_ID)+'/'+ALLTRIM(Style) WINDOW NOWAIT
    IF laSetups[5,2]='Y' .AND. !SEEK(Style+laData[7]+SPACE(10),'StyDye')
      DO gpAdStyWar WITH Style,SPACE(10),laData[7]
    ENDIF
    =SEEK(STYLE,'STYLE')
    *E300834,1 Get style price according to ordered quantity
    *lnPrice = IIF(!llMulCurr,Style.Price&lcPriceLvl,;
                  gfStyPrice(Style,lcPriceLvl,&lcInvHdr..cCurrCode))
    *E301142,1 Select alternative price level
    *lcPriceCatg  = IIF(lcPriceLvl='Q',IIF(&lcPackHdr..PackQty*TotQty >= Style.nAtQtyC,'C',;
    *               IIF(&lcPackHdr..PackQty*TotQty>=Style.nAtQtyB,'B','A')),lcPriceLvl)
    *m.Gros_Price = IIF(!llMulCurr OR &lcInvHdr..cCurrCode=gcBaseCurr,Style.Price&lcPriceCatg,;
    *                   gfStyPrice(Style,lcPriceCatg,&lcInvHdr..cCurrCode))
    m.Gros_Price  = lfGetprice(Style,lcPriceLvl,&lcPackHdr..PackQty*TotQty)
    *E301142,1 (End)

    *C102052,1 ABD - Add new validation on the discount code up on 
    *C102052,1 ABD - add new option about whole sale or retail sale [begin]
    *-- get the cDiscCode From stydye in every case
    lcDiscCode = IIF(SEEK(m.Style+laData[7]+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
    STORE 0 TO m.Disc_Pcnt
    IF !EMPTY(ALLTRIM(lcDiscCode))
      *-- Get the discount related field to know which 
      *-- type whole Sale Or Retail sale Or Both.
      DECLARE laDisType[1,2] , laStartDte[1,2] , laEndDate[1,2]
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
      = gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
      = gfRltFld(lcDiscCode,  @laStartDte,'CDISCCODE')
      = gfRltFld(lcDiscCode , @laEndDate, 'CDISCCODE')
      *E300834,1 Get Style discount percent and Net Price.
      lnDisc_Pcnt = 0
      IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[6],ldStartDte,ldEndDate)
        *C102052,1 ABD - [End]
        lnDisc_Pcnt = m.Disc_Pcnt
        *C102052,1 ABD - remark next line and get cDiscCode From stydye file. [Begin]
        *=IIF(EMPTY(Style.CDISCCODE),.T.,gfRltFld(Style.CDISCCODE,@laDisRltFld,'CDISCCODE'))
        =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
        *C102052,1 ABD - [End]      
        *E300834,1 Calculate net price
        m.Disc_Pcnt = lnDisc_Pcnt
        *E300834,1 (End)
        *C102052,1 ABD - end if for if statement. [Begin]
      ENDIF
    ENDIF  
    m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100    
    *C102052,1 ABD - [End]
    SELECT (lcInvHdr)
    =RLOCK()
    REPLACE LastLine WITH LastLine + 1
    UNLOCK
    SELECT (lcInvLine)
    APPEND BLANK 
    =RLOCK() 
    REPLACE Account    WITH laData[4] ,;
            Order      WITH laData[1] ,;
            Store      WITH laData[5] ,;
            cWareCode  WITH laData[7] ,;
            PikTkt     WITH laData[3] ,;
            InvDate    WITH laData[6] ,;
            LineNo     WITH &lcInvHdr..LastLine ,;
            Style      WITH Spck_Lin.Style ,;
            Desc1      WITH Style.Desc1 ,;
            Season     WITH Style.Season ,;
            Scale      WITH Style.Scale ,;
            Gros_Price WITH m.Gros_Price ,;
            Price      WITH m.Price ,;
            Disc_Pcnt  WITH m.Disc_Pcnt
    REPLACE Qty1       WITH Spck_Lin.Qty1*&lcPackHdr..PackQty,;
            Qty2       WITH Spck_Lin.Qty2*&lcPackHdr..PackQty,;
            Qty3       WITH Spck_Lin.Qty3*&lcPackHdr..PackQty,;
            Qty4       WITH Spck_Lin.Qty4*&lcPackHdr..PackQty,;
            Qty5       WITH Spck_Lin.Qty5*&lcPackHdr..PackQty,;
            Qty6       WITH Spck_Lin.Qty6*&lcPackHdr..PackQty,;
            Qty7       WITH Spck_Lin.Qty7*&lcPackHdr..PackQty,;
            Qty8       WITH Spck_Lin.Qty8*&lcPackHdr..PackQty,;
            TotQty     WITH Spck_Lin.TotQty*&lcPackHdr..PackQty ,;
            Comm1      WITH laData[9] ,;
            Comm2      WITH laData[10] ,;
            Pack_Id    WITH &lcPackHdr..Pack_Id ,;
            lNewLine   WITH .T. ,;
            lTaxable   WITH STYLE.lTaxable
    IF INLIST(laSetups[7,2],'F','L') AND SEEK(Spck_Lin.Style+laData[7]+SPACE(10),'StyDye')
      REPLACE Qty1 WITH MAX(MIN(Qty1,StyDye.Stk1),0) ,;
              Qty2 WITH MAX(MIN(Qty2,StyDye.Stk2),0) ,;
              Qty3 WITH MAX(MIN(Qty3,StyDye.Stk3),0) ,;
              Qty4 WITH MAX(MIN(Qty4,StyDye.Stk4),0) ,;
              Qty5 WITH MAX(MIN(Qty5,StyDye.Stk5),0) ,;
              Qty6 WITH MAX(MIN(Qty6,StyDye.Stk6),0) ,;
              Qty7 WITH MAX(MIN(Qty7,StyDye.Stk7),0) ,;
              Qty8 WITH MAX(MIN(Qty8,StyDye.Stk8),0) ,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
    ENDIF
    UNLOCK

    lnCartons = lnCartons + IIF(Style.Qty_Ctn>0,TotQty/Style.Qty_Ctn,0)
    m.ShipAmt = m.ShipAmt + TotQty*Price
    m.Ship    = m.Ship    + TotQty
    m.Weight  = m.Weight  + TotQty*Style.nStyWeight
    m.nTaxDue = m.nTaxDue + IIF(lTaxable,TotQty*Price,0)
    IF laSetups[9,2]='Y' .AND. llIsEngland .AND. Style.nTaxBreak <> 0
      lnQuantity = 0
      FOR lnCount = Style.nTaxBreak TO 8
        lnQuantity = lnQuantity+EVAL('Qty'+STR(lnCount,1))
      ENDFOR
      lnMerchTax = lnMerchTax + lnQuantity * Price * nTaxRate/100           
    ENDIF
  ENDSCAN
ENDSCAN

*B602905,1 Default No. of cartons must be 1 at least [Begin]
*m.Cartons   = CEILING(lnCartons)
m.Cartons = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
*B602905,1 Default No. of cartons must be 1 at least [End  ]

m.Discount  = -1*m.DiscPcnt*m.ShipAmt/100
m.nMerchTax = m.nMerchTax +  lnMerchTax
m.Tax_Amt   = IIF(llIsEngland,m.nChrgTax+m.nMerchTax*(100-m.DiscPcnt)/100*(100-m.Trde_Disc)/100,m.TAX_AMT)
m.Cod_Amt   = IIF(llCod,IIF(llIsEngland,m.ShipAmt+m.nCharges+;
                           m.Tax_Amt+m.Discount,m.Cod_Amt),0)

m.TotalChg  = IIF(llIsEngland,m.ShipAmt+m.nCharges+m.Tax_Amt+m.Discount,m.TotalChg)

*C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
IF ASCAN(laEvntTrig , PADR('SUMALL',10)) <> 0
  =gfDoTriger('ARIINV',PADR('SUMALL',10))
ENDIF
*C102314,1 BWA 06/10/2001 [END]

SELECT (lcInvHdr)  
=RLOCK()
REPLACE CARTONS   WITH m.Cartons ,;
        nCartons  WITH lnCartons ,;
        WEIGHT    WITH m.Weight  ,;
        SHIPAMT   WITH m.ShipAmt ,;
        SHIP      WITH m.Ship    ,;
        Discount  WITH m.Discount,;
        lCompUps  WITH !llIsEngland ,;
        nMerchTax WITH m.nMerchTax ,;
        Tax_Amt   WITH m.Tax_Amt ,;
        Cod_Amt   WITH m.Cod_Amt ,; 
        TotalChg  WITH m.TotalChg ,;
        nTaxDue   WITH m.nTaxDue
UNLOCK
SELECT (lcInvLine)
WAIT CLEAR
STORE .T. TO llCUpdate
CLEAR READ

*!*************************************************************
*! Name      : lfValidPack
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Validate packs before add to the order lines
*!*************************************************************
*! Calls     : gfModalGen,gfStyPrice
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfValidPack()
*!*************************************************************
FUNCTION lfValidPack
PRIVATE llValidPack,llOnHoldAll,llAssignAll

llValidPack = .T.
STORE .F. TO llOnHoldAll,llAssignAll
SELECT Spck_Lin
SCAN REST WHILE Type+Account+Pack_Id=&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id

  IF !SEEK('S'+Style.Scale,'Scale')
    *E300817,1 Message : 40029
    *E300817,1 Pack xxx contains one or more xxx. xxx
    *E300817,1 Button : 00000
    *E300817,1 Ok
    =gfModalGen('INM40029B00000','ALERT',&lcPackHdr..Pack_Id+'|'+'style with non existing scale'+'|'+'Pack will be ignored.')
    llValidPack = .F.
    EXIT
  ENDIF  
  IF Style.Status = 'X'
    *E300817,1 Message : 40029
    *E300817,1 Pack xxx contains one or more xxx. xxx
    *E300817,1 Button : 00000
    *E300817,1 Ok
    =gfModalGen('INM40029B00000','ALERT',&lcPackHdr..Pack_Id+'|'+'canceled style'+'|'+'Pack will be ignored.')
    llValidPack = .F.
    EXIT
  ENDIF
  IF !llOnHoldAll .AND. Style.Status = 'H'
    *E300817,1 Message : 40030
    *E300817,1 Pack xxx contains one or more on-hold style.
    *E300817,1 Button : 40003
    *E300817,1 Proceed Cancel
    IF gfModalGen('QRM40030B40003','ALERT',&lcPackHdr..Pack_Id)=1
      llOnHoldAll = .T.
    ELSE
      llValidPack= .F.
      EXIT
    ENDIF
  ENDIF
  IF laSetups[5,2]='Y' .AND. !llAssignAll .AND. ;
     !SEEK(Style+laData[7]+SPACE(10),'StyDye')
    *E300817,1 Message : 40031
    *E300817,1 Pack xxx contains one or more style that is not assign
    *E300817,1 to warehouse xxx. Assign all styles to this warehouse?
    *E300817,1 Button : 40000
    *E300817,1 Yes No
    IF gfModalGen('QRM40031B40000','ALERT',&lcPackHdr..Pack_Id+'|'+&lcInvHdr..cWareCode)=1
      llAssignAll = .T.
    ELSE
      llValidPack = .F.
      EXIT
    ENDIF
  ENDIF   
  *E301142,1 Select alternative price level
  *E300834,1 Get style price according to ordered quantity
  *lnPrice = IIF(!llMulCurr,Style.Price&lcPriceLvl,;
                gfStyPrice(Style,lcPriceLvl,&lcInvHdr..cCurrCode))
  *lcPriceCatg  = IIF(lcPriceLvl='Q',IIF(&lcPackHdr..PackQty*TotQty >= Style.nAtQtyC,'C',;
  *               IIF(&lcPackHdr..PackQty*TotQty>=Style.nAtQtyB,'B','A')),lcPriceLvl)
  *lnPrice = IIF(!llMulCurr OR &lcInvHdr..cCurrCode=gcBaseCurr ,Style.Price&lcPriceCatg,;
  *              gfStyPrice(Style,lcPriceLvl,&lcInvHdr..cCurrCode))
  **E300834,1 (End)
  *IF lnPrice < 0
  *  llValidPack = .F.
  *  EXIT
  *ENDIF
  *E301142,1 (End)
ENDSCAN
=SEEK(&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id)
RETURN(llValidPack)

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
*! Name      : FUNCTION lfvPrePack
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style prepak
*!*************************************************************
*! Calls     : gfPrePBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPrePack()
*!*************************************************************
FUNCTION lfvPrePack
PRIVATE lcBrFields,lcFile_Ttl

IF llBrowse .OR. (!EMPTY(m.Prepak) .AND. !SEEK('P'+m.Scale+m.Prepak,'Scale'))
  =gfPrePBrow(Scale,@m.Prepak)
ENDIF
llBrowse = .F.
=SEEK('S'+m.Scale,'Scale')
IF Prepak <> m.Prepak
  llCUpdate = .T.
  =RLOCK()
  REPLACE Prepak WITH m.Prepak
  UNLOCK
  SHOW GETS WINDOW (lcWinCh42) ONLY
  SHOW GETS WINDOW (lcWinCh44) ONLY
  SHOW GETS WINDOW (lcWinCh45) ONLY
ENDIF
IF EMPTY(Prepak)
  =RLOCK()
  REPLACE PPQty WITH 0
  UNLOCK
  m.PPQTY = 0
  *B602760,1 NAD (Start) Control object enable and disable in the screen
  *SHOW GET m.PPQty DISABLE
*ELSE
*  SHOW GET m.PPQty ENABLE
*B602760,1 NAD (End)

ENDIF
*!*************************************************************
*! Name      : FUNCTION lfvPPakQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style prepak quantity
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPPakQty()
*!*************************************************************
FUNCTION lfvPPakQty

IF m.PPQty = PPQty
  RETURN
ENDIF
IF m.PPQty <> 0
  =SEEK('P'+m.Scale+m.Prepak,'Scale')
  m.Qty1   = m.PPQty*Scale.PP1
  m.Qty2   = m.PPQty*Scale.PP2
  m.Qty3   = m.PPQty*Scale.PP3
  m.Qty4   = m.PPQty*Scale.PP4
  m.Qty5   = m.PPQty*Scale.PP5
  m.Qty6   = m.PPQty*Scale.PP6
  m.Qty7   = m.PPQty*Scale.PP7
  m.Qty8   = m.PPQty*Scale.PP8
  m.TotQty = m.PPQty*Scale.PPTot
  =SEEK('S'+m.Scale,'Scale')
ENDIF
SELECT (lcInvLine)
=lfChkTotQty()
SHOW GETS WINDOW (lcWinCh42) ONLY
SHOW GETS WINDOW (lcWinCh44) ONLY
SHOW GETS WINDOW (lcWinCh45) ONLY

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

*B803822,1 NAD 11/26/2000 (Start) Added Parameter to pass the button type.
*PARAMETERS llSelect
*PRIVATE llShip,lcSelect
PARAMETERS llSelect ,lcAct
PRIVATE llShip,lcSelect,lcAct
*B803822,1 NAD 11/26/2000 (End)
*B801954,1 Adjust select/ship invoice buttons.
IF Consol = 'Y'
  SET ORDER TO TAG 'CONSOL'
  lcConsKey  = Account+cDivision+cCurrCode
  llConsShip = (m.Ship=0)
  lcConsSel  = cSelect
  =SEEK('N'+lcConsKey)
  SCAN REST WHILE Consol+Account+cDivision+cCurrCode= 'N'+lcConsKey
    REPLACE cSelect WITH IIF(llSelect,lcConsSel,cSelect)
    IF IIF(llConsShip,SHIP=0,SHIP>0)
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
*B801954,1 (End)

llShip = (m.Ship=0)

*B803822,1 NAD 11/26/2000 (Start) Ship , UnShipp According to the Type of the button. 
IF !(TYPE('lcAct')$'UL')
  DO CASE 
    CASE lcAct $ 'AN'
      llShip=IIF(lcAct='A',.T.,.F.)
    CASE lcAct ='V'
      llShip=!EMPTY(&lcInvHdr..cSelect)
  ENDCASE
ENDIF      
*B803822,1 NAD 11/26/2000 (End)
SELECT (lcInvLine)
=SEEK(laData[4]+laData[1]+laData[5]+laData[3])
STORE 0 TO m.nMerchTax,lnCartons,m.Cartons,m.Weight,m.ShipAmt,m.Ship,m.Discount,m.nTaxdue
SCAN REST WHILE Account+Order+Store+PikTkt = laData[4]+laData[1]+laData[5]+laData[3]

  *E300956,1 Keep history of order line canceled quantity.
  IF SEEK('O'+Order+STR(LineNO,6),lcOrdCanLn)
    SELECT (lcOrdCanLn)
    BLANK
    DELETE
    SELECT (lcInvLine)
    m.lBackOrd = .T.
  ENDIF
  *E300956,1 (End)
  
  *B119562,1 Use the temp shipped qtys. [Begin]
  *STORE IIF(llShip,IIF(Flag='B',Pik1,Book1),0) TO m.Qty1
  *STORE IIF(llShip,IIF(Flag='B',Pik2,Book2),0) TO m.Qty2
  *STORE IIF(llShip,IIF(Flag='B',Pik3,Book3),0) TO m.Qty3
  *STORE IIF(llShip,IIF(Flag='B',Pik4,Book4),0) TO m.Qty4
  *STORE IIF(llShip,IIF(Flag='B',Pik5,Book5),0) TO m.Qty5
  *STORE IIF(llShip,IIF(Flag='B',Pik6,Book6),0) TO m.Qty6
  *STORE IIF(llShip,IIF(Flag='B',Pik7,Book7),0) TO m.Qty7
  *STORE IIF(llShip,IIF(Flag='B',Pik8,Book8),0) TO m.Qty8
  STORE IIF(llShip,IIF(Flag='B',Pik1,TmpShp1),0) TO m.Qty1
  STORE IIF(llShip,IIF(Flag='B',Pik2,TmpShp2),0) TO m.Qty2
  STORE IIF(llShip,IIF(Flag='B',Pik3,TmpShp3),0) TO m.Qty3
  STORE IIF(llShip,IIF(Flag='B',Pik4,TmpShp4),0) TO m.Qty4
  STORE IIF(llShip,IIF(Flag='B',Pik5,TmpShp5),0) TO m.Qty5
  STORE IIF(llShip,IIF(Flag='B',Pik6,TmpShp6),0) TO m.Qty6
  STORE IIF(llShip,IIF(Flag='B',Pik7,TmpShp7),0) TO m.Qty7
  STORE IIF(llShip,IIF(Flag='B',Pik8,TmpShp8),0) TO m.Qty8
  *B119562,1 Use the temp shipped qtys. [End]

  IF INLIST(laSetups[7,2],'F','L') AND llShip
    STORE MAX(MIN(m.Qty1,StyDye.Stk1),0) TO m.Qty1
    STORE MAX(MIN(m.Qty2,StyDye.Stk2),0) TO m.Qty2
    STORE MAX(MIN(m.Qty3,StyDye.Stk3),0) TO m.Qty3
    STORE MAX(MIN(m.Qty4,StyDye.Stk4),0) TO m.Qty4
    STORE MAX(MIN(m.Qty5,StyDye.Stk5),0) TO m.Qty5
    STORE MAX(MIN(m.Qty6,StyDye.Stk6),0) TO m.Qty6
    STORE MAX(MIN(m.Qty7,StyDye.Stk7),0) TO m.Qty7
    STORE MAX(MIN(m.Qty8,StyDye.Stk8),0) TO m.Qty8
  ENDIF
  STORE m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8 TO m.TotQty

  *B802922,1 Fix cancel ordered remaining quantity
  *SCATTER MEMVAR FIELDS Price,Gros_Price,nTaxRate,Season
  SCATTER MEMVAR FIELDS Price,Gros_Price,nTaxRate,Season,lBackOrd
  *B802922,1 (End)
  =IIF(llUpCnsLine,lfUpCnsLine(llSelect),.T.)
  IF llShip
    IF laSetups[9,2]='Y' AND llIsEngland
      lnTaxQty = 0
      FOR lnSize = 1 TO Scale.Cnt
        lnTaxQty = lnTaxQty + IIF(BETWEEN(lnSize,Style.nTaxBreak,8),;
                                  EVAL('m.Qty'+STR(lnSize,1)),0)
      ENDFOR
      m.nMerchTax = m.nMerchTax + lnTaxQty*Price*nTaxRate/100
    ENDIF
    lnCartons = lnCartons + IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0)
    m.Weight  = m.Weight  + m.TotQty*Style.nStyWeight
    m.ShipAmt = m.ShipAmt + m.TotQty*Price
    m.Ship    = m.Ship    + m.TotQty
    m.nTaxdue = m.nTaxdue + IIF(lTaxable,m.TotQty*Price,0)
  ENDIF
  =RLOCK()
  GATHER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,lBackOrd
  UNLOCK
ENDSCAN
*B802922,1 Fix cancel ordered remaining quantity
=SEEK(laData[4]+laData[1]+laData[5]+laData[3])
*B802922,1 (End)
*B602905,1 Default No. of cartons must be 1 at least [Begin]
*m.Cartons = CEILING(lnCartons)
*B803301,1 (Start) Default no. of cartons must be 1 only in case of shipping.
*m.Cartons = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))



IF llShip
  m.Cartons = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
ELSE
  m.Cartons = CEILING(lnCartons)
ENDIF  
*B803301,1 (End)
*B602905,1 Default No. of cartons must be 1 at least [End  ]

=IIF(llUpCnsLine,lfUpCnsCtn(&lcInvHdr..Cartons,m.Cartons),.T.)
SELECT (lcInvHdr)
=RLOCK()
REPLACE CARTONS   WITH m.Cartons    ,;
        nCartons  WITH lnCartons    ,;
        WEIGHT    WITH m.Weight     ,;
        SHIPAMT   WITH m.ShipAmt    ,;
        SHIP      WITH m.Ship       ,;
        Discount  WITH -ShipAmt * DiscPcnt/100,;
        lCompUps  WITH !llIsEngland ,;
        nMerchTax WITH m.nMerchTax  ,;
        Tax_Amt   WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
        Cod_Amt   WITH IIF(Cod_Flag='Y',IIF(llIsEngland,ShipAmt+nCharges+;
                           Tax_Amt+Discount,Cod_Amt),0) ,; 
        TotalChg  WITH IIF(llIsEngland,ShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
        nTaxdue   WITH m.nTaxdue
UNLOCK
SCATTER MEMVAR FIELDS &lcFlFields
IF m.Ship = 0
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
lcInvKey=Account+Order+Store+PikTkt+STR(LineNo,6)

SET KEY TO

*E300956,1 Keep history of order line calceled quantity.
IF SEEK('O'+Order+STR(LineNO,6),lcOrdCanLn)
  SELECT (lcOrdCanLn)
  BLANK
  DELETE
  SELECT (lcInvLine)
  m.lBackOrd = .T.
  llCnRemain = .F.
  SHOW GET llCnRemain DISABLE
  SHOW GET pbCnRemain DISABLE
ENDIF
*E300956,1 (End)

SELECT (lcInvLine)

*B119562,1 Use the temp shipped qtys. [Begin]
*STORE IIF(TotQty=0,IIF(Flag='B',Pik1,Book1),0) TO m.Qty1
*STORE IIF(TotQty=0,IIF(Flag='B',Pik2,Book2),0) TO m.Qty2
*STORE IIF(TotQty=0,IIF(Flag='B',Pik3,Book3),0) TO m.Qty3
*STORE IIF(TotQty=0,IIF(Flag='B',Pik4,Book4),0) TO m.Qty4
*STORE IIF(TotQty=0,IIF(Flag='B',Pik5,Book5),0) TO m.Qty5
*STORE IIF(TotQty=0,IIF(Flag='B',Pik6,Book6),0) TO m.Qty6
*STORE IIF(TotQty=0,IIF(Flag='B',Pik7,Book7),0) TO m.Qty7
*STORE IIF(TotQty=0,IIF(Flag='B',Pik8,Book8),0) TO m.Qty8
STORE IIF(TotQty=0,IIF(Flag='B',Pik1,TmpShp1),0) TO m.Qty1
STORE IIF(TotQty=0,IIF(Flag='B',Pik2,TmpShp2),0) TO m.Qty2
STORE IIF(TotQty=0,IIF(Flag='B',Pik3,TmpShp3),0) TO m.Qty3
STORE IIF(TotQty=0,IIF(Flag='B',Pik4,TmpShp4),0) TO m.Qty4
STORE IIF(TotQty=0,IIF(Flag='B',Pik5,TmpShp5),0) TO m.Qty5
STORE IIF(TotQty=0,IIF(Flag='B',Pik6,TmpShp6),0) TO m.Qty6
STORE IIF(TotQty=0,IIF(Flag='B',Pik7,TmpShp7),0) TO m.Qty7
STORE IIF(TotQty=0,IIF(Flag='B',Pik8,TmpShp8),0) TO m.Qty8
*B119562,1 Use the temp shipped qtys. [End]

IF INLIST(laSetups[7,2],'F','L') AND TotQty=0
  STORE MAX(MIN(m.Qty1,StyDye.Stk1),0) TO m.Qty1
  STORE MAX(MIN(m.Qty2,StyDye.Stk2),0) TO m.Qty2
  STORE MAX(MIN(m.Qty3,StyDye.Stk3),0) TO m.Qty3
  STORE MAX(MIN(m.Qty4,StyDye.Stk4),0) TO m.Qty4
  STORE MAX(MIN(m.Qty5,StyDye.Stk5),0) TO m.Qty5
  STORE MAX(MIN(m.Qty6,StyDye.Stk6),0) TO m.Qty6
  STORE MAX(MIN(m.Qty7,StyDye.Stk7),0) TO m.Qty7
  STORE MAX(MIN(m.Qty8,StyDye.Stk8),0) TO m.Qty8
ENDIF
STORE Price TO m.Price
STORE m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8 TO m.TotQty
*B802922,1 Fix cancel ordered remaining quantity
STORE lBackOrd TO m.lBackOrd
*B802922,1 (End)
*B605864,1 (Begin) Correct the way it calculate the cartons as when the division gives 
*B605864,1         fractions it gives a wrong Carton  no.  
*lnCartons  = &lcInvHdr..nCartons + IIF(Style.Qty_Ctn>0,(m.TotQty-TotQty)/Style.Qty_Ctn,0)
lnCartons  = ceiling(&lcInvHdr..nCartons  - IIF(Style.Qty_Ctn>0,ceiling(TotQty/Style.Qty_Ctn),0)+IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0))
*B605864,1 (End)

m.Weight   = m.Weight  + (m.TotQty-TotQty)*Style.nStyWeight

*B602905,1 Default No. of cartons must be 1 at least [Begin]
*m.Cartons = CEILING(lnCartons)
*B803301,1 (Start) Default no. of cartons must be one only in case of shipping.
*m.Cartons = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
IF m.TotQty =0
  m.Cartons = CEILING(lnCartons)
ELSE
  m.Cartons = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
ENDIF    
*B803301,1 (End)
*B602905,1 Default No. of cartons must be 1 at least [End  ]

m.ShipAmt  = m.ShipAmt + (m.TotQty-TotQty)*Price
m.Ship     = m.Ship    + (m.TotQty-TotQty)
m.Discount = -m.ShipAmt * m.DiscPcnt/100
=IIF(llUpCnsLine,lfUpCnsLine() AND lfUpCnsCtn(&lcInvHdr..Cartons,m.Cartons),.T.)
lnTaxQty = 0
IF laSetups[9,2]='Y' AND llIsEngland
  FOR lnSize = 1 TO Scale.Cnt
    lnTaxQty = lnTaxQty + IIF(BETWEEN(lnSize,Style.nTaxBreak,8),;
    IIF(TotQty=0,EVAL('m.Qty'+STR(lnSize,1)),EVAL('-Qty'+STR(lnSize,1))),0)
  ENDFOR
ENDIF
m.nTaxdue = &lcInvHdr..nTaxDue + IIF(lTaxable,(m.TotQty-TotQty)*Price,0)
=RLOCK()
GATHER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,lBackOrd
UNLOCK
STORE .T. TO llCUpdate
SELECT (lcInvHdr)
=RLOCK()
REPLACE CARTONS    WITH m.Cartons    ,;
        nCartons   WITH lnCartons    ,;
        WEIGHT     WITH m.Weight     ,;
        SHIPAMT    WITH m.ShipAmt    ,;
        SHIP       WITH m.Ship       ,;
        Discount   WITH m.Discount   ,;
        lCompUps   WITH !llIsEngland ,;
        nMerchTax  WITH nMerchTax +   ;
        IIF(laSetups[9,2]='Y' AND llIsEngland,lnTaxQty*m.Price*m.nTaxRate/100,0) ,;
        Tax_Amt    WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
        Cod_Amt    WITH IIF(Cod_Flag='Y',IIF(llIsEngland,ShipAmt+nCharges+;
                           Tax_Amt+Discount,Cod_Amt),0) ,; 
        TotalChg   WITH IIF(llIsEngland,ShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
        nTaxdue    WITH m.nTaxdue
UNLOCK
SCATTER MEMVAR FIELDS Tax_Amt,Cod_Amt,TotalChg
SELECT (lcInvLine)
SET KEY TO laData[4]+laData[1]+laData[5]+laData[3]
=SEEK(lcInvKey)
SHOW GETS WINDOW (lcWinCh42) ONLY
SHOW GETS WINDOW (lcWinCh44) ONLY
SHOW GETS WINDOW (lcWinCh45) ONLY
=lfRefresh(lcWinCh44)
IF m.TotQty = 0
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
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laQtyPaste

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

IF laQtyPaste[1]+laQtyPaste[2]+laQtyPaste[3]+laQtyPaste[4]+;
   laQtyPaste[5]+laQtyPaste[6]+laQtyPaste[7]+laQtyPaste[8]=0
  *E300817,1 Message : 40016
  *E300817,1  No line quantities copied to paste!
  *E300817,1  Button : 00000 
  *E300817,1  ok
  =gfModalGen('INM40016B00000','ALERT')
  RETURN
ENDIF
STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty
FOR lnCount = 1 TO Scale.Cnt
  lcCount = STR(lnCount,1)
  m.Qty&lcCount = laQtyPaste[lnCount]
  m.TotQty = m.TotQty + m.Qty&lcCount
ENDFOR
IF !lfChkTotQty()
  SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
  RETURN
ENDIF
SHOW GETS WINDOW (lcWinCh44) ONLY
=lfRefresh(lcWinCh44)
SHOW WINDOW (lcInvBrow) REFRESH SAME
_CUROBJ=OBJNUM(m.Qty1)

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
    IF Account+Order+Store+PikTkt <> laData[4]+laData[1]+laData[5]+laData[3] OR BOF()
      =SEEK(laData[4]+laData[1]+laData[5]+laData[3])
    ELSE 
      =lfwBrow()
    ENDIF
    RETURN
  CASE lnLastKey=24
    SKIP
    IF Account+Order+Store+PikTkt <> laData[4]+laData[1]+laData[5]+laData[3] OR EOF()
      SKIP -1
    ELSE 
      =lfwBrow()
    ENDIF
    RETURN
ENDCASE
RETURN(.F.)

*!*************************************************************
*! Name      : FUNCTION lfvSizeQty
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
*! Example   :  =lfvSizeQty(1)
*!*************************************************************
FUNCTION lfvSizeQty
PARAMETERS lcSize

*B603449,1 ABD If statement to cheak if user inter Negative values. [ Begin ]
IF m.Qty&lcSize < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Buttfon  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Qty&lcSize = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
*B603449,1 ABD [ End ]

IF lfNavigate()
  _CUROBJ = _CUROBJ 
  RETURN
ENDIF
=lfRefresh(lcWinCh44)

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

*B603342,1 WAB - variable to check if qty is changed when cleard preapack code
PRIVATE llQtyChng

llQtyChng = .F.
*B603342,1 WAB -
IF !EMPTY(m.Prepak) 
  llPrepck = SEEK('P'+m.Scale+m.Prepak,'Scale') AND ;
  MOD(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,Scale.PPTot) <> 0
  lnPPTot = Scale.PPTot
  =SEEK('S'+m.Scale,'Scale')
  IF llPrepck
    *E300408,1 Message : 40014
    *E300408,1 Total quantity is not evenly divisible by the prepak quantity xxx
    *E300408,1 Button : 00000 
    *E300408,1 Ok
    *B802571 (Start)
    *=gfModalGen('INM40014B00000','ALERT',STR(lnPPTot,4))
    *RETURN(.F.)
    IF gfModalGen('INM40014B00030','ALERT',ALLTRIM(STR(lnPPTot,4))+". Prepack code will be cleared") = 1
      m.PrePak = SPACE(1)
      m.PPQty  = 0
      *B603649,1  (Start) not to update the quantities here and leave it as is
      *REPLACE PrePak WITH m.PrePak,;
              PPQty  WITH m.PPQty ,;
              Qty1   WITH m.Qty1  ,;
              Qty2   WITH m.Qty2  ,;
              Qty3   WITH m.Qty3  ,;
              Qty4   WITH m.Qty4  ,;
              Qty5   WITH m.Qty5  ,;
              Qty6   WITH m.Qty6  ,;
              Qty7   WITH m.Qty7  ,;
              Qty8   WITH m.Qty8
              
      REPLACE PrePak WITH m.PrePak,;
              PPQty  WITH m.PPQty        
      *B603649,1 (End)
      *B603342,1 WAB - put flag llQtyChng ( .T. ) to detrermine that qty is changed 
      llQtyChng = .T.
      *B603342,1 WAB - END
      SHOW GET m.Prepak
      SHOW GET m.PPQty
    ELSE
      m.Qty1 = Qty1
      m.Qty2 = Qty2
      m.Qty3 = Qty3
      m.Qty4 = Qty4
      m.Qty5 = Qty5
      m.Qty6 = Qty6
      m.Qty7 = Qty7
      m.Qty8 = Qty8
      SHOW GET m.Qty1
      SHOW GET m.Qty2
      SHOW GET m.Qty3
      SHOW GET m.Qty4
      SHOW GET m.Qty5
      SHOW GET m.Qty6
      SHOW GET m.Qty7
      SHOW GET m.Qty8
      RETURN(.F.)
    ENDIF
    *B802571 (End)
  ENDIF
ENDIF
*B603342,1 WAB - also make all needed changed in case of cleared pre pack qty
*IF Qty1 <> m.Qty1 OR Qty2 <> m.Qty2 OR Qty3 <> m.Qty3 OR Qty4 <> m.Qty4 OR ;
   Qty5 <> m.Qty5 OR Qty6 <> m.Qty6 OR Qty7 <> m.Qty7 OR Qty8 <> m.Qty8  
IF Qty1 <> m.Qty1 OR Qty2 <> m.Qty2 OR Qty3 <> m.Qty3 OR Qty4 <> m.Qty4 OR ;
   Qty5 <> m.Qty5 OR Qty6 <> m.Qty6 OR Qty7 <> m.Qty7 OR Qty8 <> m.Qty8  ;   
   OR llQtyChng 
*B603342,1 WAB - END 
  IF INLIST(laSetups[7,2],'F','L')
    FOR lnCount = 1 TO Scale.Cnt
      IF EVAL('m.Qty'+STR(lnCount,1)) > EVAL('StyDye.Stk'+STR(lnCount,1))
        *E300408,1 Message : 40137
        *E300408,1 Shipped Quantity For Size XXXXX Cannot Exceed On Hand
        *E300408,1 Quantity (99999).
        *E300408,1 Button : 00000 
        *E300408,1 OK
        =gfModalGen('TRM40137B00000','ALERT',ALLTRIM(EVAL('Scale.Sz'+STR(lnCount,1)))+'|('+ALLTRIM(STR(EVAL('StyDye.Stk'+STR(lnCount,1)),5))+')')
        STORE MAX(EVAL('StyDye.Stk'+STR(lnCount,1)),0) TO ('m.Qty'+STR(lnCount,1))
        RETURN(.F.)
      ENDIF
    ENDFOR
  ENDIF
  m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
  *E300956,1 Keep history of order line calceled quantity.
  
  IF !m.lNewLine
    SELECT (lcOrdCanLn)
    *E301151,1 Display a query message when vackorder balance.
    *IF (!m.lBackOrd OR lcBackOrd='N') AND ;
      (m.Book1 > m.Qty1 OR m.Book2 > m.Qty2 OR m.Book3 > m.Qty3 OR m.Book4 > m.Qty5 OR ;
       m.Book5 > m.Qty5 OR m.Book6 > m.Qty6 OR m.Book7 > m.Qty7 OR m.Book8 > m.Qty8)

    *E301151,1 Message : 40154
    *E301151,1 Cancel remaining?
    *E301151,1 Button : 40000
    *E3301151,1 Yes No
    m.lBackOrd =.T.

    *C200088,1 FP Check for Allow back order Flag. [Begin]
    *IF (m.Book1 > m.Qty1 OR m.Book2 > m.Qty2 OR m.Book3 > m.Qty3 OR m.Book4 > m.Qty5 OR ;
    *    m.Book5 > m.Qty5 OR m.Book6 > m.Qty6 OR m.Book7 > m.Qty7 OR m.Book8 > m.Qty8) AND ;
    *   (lcBackOrd='N' OR (lcBackOrd='I' AND ;
    *    gfModalGen('QRM40154B40000','ALERT')=1) )
    *  m.lBackOrd =.F.
    *ENDIF
    
    *-- lcChkBkOrd : Empty variable overrided by "I" or "N" according to 
    *--            : Order Header Allow back order flag. 
    lcChkBkOrd = " "
    IF ASCAN(laEvntTrig,"CHKBKORDER") <> 0
      = gfDoTriger('ARIINV','CHKBKORDER')
    ENDIF  
    *B803174,1 (Start) Not to display the message if the booked quantities < the shipped quantities .
    *IF (m.Book1 > m.Qty1 OR m.Book2 > m.Qty2 OR m.Book3 > m.Qty3 OR m.Book4 > m.Qty5 OR ;
        m.Book5 > m.Qty5 OR m.Book6 > m.Qty6 OR m.Book7 > m.Qty7 OR m.Book8 > m.Qty8) AND ;
       (lcChkBkOrd="N" OR (EMPTY(lcChkBkOrd) AND lcBackOrd='N')) OR  ;
      ((lcChkBkOrd="I" OR (EMPTY(lcChkBkOrd) AND lcBackOrd='I')) AND ;
           gfModalGen('QRM40154B40000','ALERT')=1)
  
      *m.lBackOrd =.F.
        
    *ENDIF 
    IF (m.Book1 > m.Qty1 OR m.Book2 > m.Qty2 OR m.Book3 > m.Qty3 OR m.Book4 > m.Qty4 OR ;
        m.Book5 > m.Qty5 OR m.Book6 > m.Qty6 OR m.Book7 > m.Qty7 OR m.Book8 > m.Qty8) 
        
      IF   (lcChkBkOrd="N" OR (EMPTY(lcChkBkOrd) AND lcBackOrd='N')) OR  ;
           ((lcChkBkOrd="I" OR (EMPTY(lcChkBkOrd) AND lcBackOrd='I')) AND ;
           gfModalGen('QRM40154B40000','ALERT')=1)           
        m.lBackOrd =.F.    
      ENDIF 
          
    ENDIF        
    *B803174,1 (End) 

    IF !m.lBackOrd
    *E301151,1 (End)
      IF !SEEK('O'+&lcInvLine..Order+STR(&lcInvLine..LineNO,6))
        =gfwCodePop(@laCodes,'CCANCRESON','D')
        INSERT INTO (lcOrdCanLn) (cOrdType,Order,LineNo,cCancReson,Cancelled) VALUES ;
        ('O',m.Order,m.LineNo,laCanReason[lnCanReason,2],gdSysDate)
      ENDIF
      =RLOCK()
      REPLACE Qty1 WITH MAX(m.Book1-m.Qty1,0) ,;
              Qty2 WITH MAX(m.Book2-m.Qty2,0) ,;
              Qty3 WITH MAX(m.Book3-m.Qty3,0) ,;
              Qty4 WITH MAX(m.Book4-m.Qty4,0) ,;
              Qty5 WITH MAX(m.Book5-m.Qty5,0) ,;
              Qty6 WITH MAX(m.Book6-m.Qty6,0) ,;
              Qty7 WITH MAX(m.Book7-m.Qty7,0) ,;
              Qty8 WITH MAX(m.Book8-m.Qty8,0) ,;
              TotQTy WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      UNLOCK
      m.lBackOrd = .F.
      SHOW GET pbCnRemain ENABLE
    ELSE
      IF SEEK('O'+&lcInvLine..Order+STR(&lcInvLine..LineNO,6))
        DELETE
      ENDIF
      m.lBackOrd = .T.
      SHOW GET pbCnRemain DISABLE
    ENDIF
    llCnRemain = !m.lBackOrd

    *C200088,1 Disable / Enable Cancel Remain Check box According to 
    *C200088,1 new Variable [Begin]
    *IF lcBackOrd='I' AND ;
      (m.Book1 > m.Qty1 OR m.Book2 > m.Qty2 OR m.Book3 > m.Qty3 OR m.Book4 > m.Qty5 OR ;
       m.Book5 > m.Qty5 OR m.Book6 > m.Qty6 OR m.Book7 > m.Qty7 OR m.Book8 > m.Qty8)
    IF (lcChkBkOrd="I" OR (EMPTY(lcChkBkOrd) AND lcBackOrd='I')) AND ;
      (m.Book1 > m.Qty1 OR m.Book2 > m.Qty2 OR m.Book3 > m.Qty3 OR m.Book4 > m.Qty5 OR ;
       m.Book5 > m.Qty5 OR m.Book6 > m.Qty6 OR m.Book7 > m.Qty7 OR m.Book8 > m.Qty8)
    *C200088,1 new Variable [End  ]

      SHOW GET llCnRemain ENABLE
    ELSE
      SHOW GET llCnRemain DISABLE
    ENDIF    
    SELECT (lcInvLine)
  ENDIF
  *E300834,1 Get style price according to ordered quantity
  IF m.lNewLine AND lcPriceLvl='Q'
    *E301142,1 Select alternative price level
    *lcPriceCatg  = IIF(m.TotQty >= Style.nAtQtyC,'C',IIF(m.TotQty >= Style.nAtQtyB,'B','A'))
    *m.Gros_Price = IIF(!llMulCurr OR laData[14]=gcBaseCurr,Style.Price&lcPriceCatg,;
    *                   gfStyPrice(m.Style,lcPriceCatg,laData[14]))
    m.Gros_Price  = lfGetprice(m.Style,lcPriceLvl,m.TotQty)
    *E301142,1 (End)
    m.Price = m.Gros_Price*(100-m.Disc_Pcnt)/100
  ENDIF
  *E300834,1 (End)

  STORE .T. TO llCUpdate
  SELECT (lcInvLine)
  *B605864,1 (Begin) Correct the way it calculate the cartons as when the division gives 
  *B605864,1         fractions it gives a wrong Carton  no.  
  *lnCartons  = &lcInvHdr..nCartons  - IIF(Style.Qty_Ctn>0,(TotQty-m.TotQty)/Style.Qty_Ctn,0)
  lnCartons  = ceiling(&lcInvHdr..nCartons  - IIF(Style.Qty_Ctn>0,ceiling(TotQty/Style.Qty_Ctn),0)+IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0))
  *B605864,1 (End)
  m.Weight   = m.Weight  - (TotQty-m.TotQty)*Style.nStyWeight

  *B602905,1 Default No. of cartons must be 1 at least [Begin]
  *m.Cartons = CEILING(lnCartons)
  *B803301,1 (Start) Default no. of cartons must be one only in case of shipping.
  *m.Cartons = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
  IF m.TotQty =0
    m.Cartons = CEILING(lnCartons)
  ELSE
    m.Cartons = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
  ENDIF    
  *B803301,1 (End)
  *B602905,1 Default No. of cartons must be 1 at least [End  ]

  m.ShipAmt  = m.ShipAmt - TotQty*Price + m.TotQty*m.Price
  m.Ship     = m.Ship - TotQty+m.TotQty
  m.Discount =-1*m.DiscPcnt*m.ShipAmt/100  
  lnMerchTax = &lcInvHdr..nMerchTax  
  
  IF laSetups[9,2]='Y' AND llIsEngland
    FOR lnSize = 1 TO SCALE.CNT
      IF BETWEEN(lnSize,Style.nTaxBreak,8)
        lcSize = STR(lnSize,1)
        *B602891,1 AMM Adjust the equation
        *lnMerchTax = lnMerchTax + (m.Qty&lcSize*m.Price-Qty&lcSize*Price)*m.nTaxRate/100,0)
        lnMerchTax = lnMerchTax + (m.Qty&lcSize*m.Price-Qty&lcSize*Price)*m.nTaxRate/100
        *B602891,1 AMM end
      ENDIF
    ENDFOR
  ENDIF
  m.nTaxDue = &lcInvHdr..nTaxDue + ;
              IIF(lTaxable,m.TotQty*m.Price-TotQty*Price,0)
  =IIF(llUpCnsLine,lfUpCnsLine() AND lfUpCnsCtn(&lcInvHdr..Cartons,m.Cartons),.T.)
  SELECT (lcInvHdr)  
  =RLOCK()
  REPLACE CARTONS   WITH m.Cartons    ,;
          nCartons  WITH lnCartons    ,;
          WEIGHT    WITH m.Weight     ,;
          SHIPAMT   WITH m.ShipAmt    ,;
          SHIP      WITH m.Ship       ,;
          Discount  WITH m.Discount   ,;
          lCompUps  WITH !llIsEngland ,;
          nMerchTax WITH lnMerchTax   ,;
          Tax_Amt   WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
          Cod_Amt   WITH IIF(Cod_Flag='Y',IIF(llIsEngland,ShipAmt+nCharges+;
                             Tax_Amt+Discount,Cod_Amt),0) ,; 
          TotalChg  WITH IIF(llIsEngland,ShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
          nTaxDue   WITH m.nTaxDue   
  UNLOCK
  SCATTER MEMVAR FIELDS Tax_Amt,Cod_Amt,TotalChg
  SELECT (lcInvLine)
  =RLOCK()
  GATHER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
                       Gros_Price,Price,lBackOrd,PPQty
  UNLOCK
  =lfRefresh(lcWinCh44)
ENDIF
*-- end of lfChkTotQty.

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

*B603449,1 ABD If statement to cheak if user inter Negative values. [ Begin ]
IF m.Gros_Price < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Gros_Price = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
*B603449,1 ABD [ End ]

*B602446,1 Elimenate order amount roundation
*m.Price = m.Gros_Price*(100-m.Disc_Pcnt)/100
IF m.Gros_Price <> &lcInvLine..Gros_Price
  m.Price = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
  *B602446,1 (End)
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

*B602446,1 Elimenate order amount roundation
*m.Price = m.Gros_Price*(100-m.Disc_Pcnt)/100
*=lfvNPrice()
IF m.Disc_Pcnt <> &lcInvLine..Disc_Pcnt
  m.Price = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
  =lfvNPrice(.T.)
  *B602446,1 (End)
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

*B603449,1 ABD If statement to cheak if user inter Negative values. [ Begin ]
IF m.Price < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Price = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
*B603449,1 ABD [ End ]

IF lfNavigate()
  _CUROBJ = _CUROBJ
  RETURN
ENDIF  
SELECT (lcInvLine)
IF Price <> m.Price
  STORE .T. TO llCUpdate
  m.ShipAmt = m.ShipAmt - m.TotQty*(Price-m.Price)
  lnTaxQty = 0
  IF laSetups[9,2]='Y' AND llIsEngland
    FOR lnSize = 1 TO Scale.Cnt
      lnTaxQty = lnTaxQty + IIF(BETWEEN(lnSize,Style.nTaxBreak,8),;
      EVAL('Qty'+STR(lnSize,1)),0)
    ENDFOR
  ENDIF
  =IIF(llUpCnsLine,lfUpCnsLine() AND lfUpCnsCtn(&lcInvHdr..Cartons,m.Cartons),.T.)
  SELECT (lcInvHdr)  
  =RLOCK()
  REPLACE ShipAmt   WITH m.ShipAmt    ,;
          lCompUps  WITH !llIsEngland ,;
          nMerchTax WITH nMerchTax + lnTaxQty*(m.Price-&lcInvLine..Price)*m.nTaxRate/100 ,;
          Tax_Amt   WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
          Cod_Amt   WITH IIF(Cod_Flag='Y',IIF(llIsEngland,ShipAmt+nCharges+;
                             Tax_Amt+Discount,Cod_Amt),0) ,; 
          TotalChg  WITH IIF(llIsEngland,ShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
          nTaxDue   WITH nTaxDue + IIF(&lcInvLine..lTaxable,m.TotQty*(m.Price-&lcInvLine..Price),0)
  UNLOCK
   
  *B602446,1 Elimenate order amount roundation
  *m.Disc_Pcnt=IIF(m.Gros_Price=0,0,100-m.Price*100/m.Gros_Price)
  m.Disc_Pcnt = IIF(llPercent,m.Disc_Pcnt,IIF(m.Gros_Price=0,0,100-m.Price*100/m.Gros_Price))
  *B602446,1 (End)

  SELECT (lcInvLine)
  =RLOCK()
  REPLACE Gros_Price WITH m.Gros_Price ,;
          Disc_Pcnt  WITH m.Disc_Pcnt ,;
          Price      WITH m.Price
  UNLOCK
  SHOW GET m.Disc_Pcnt
  SHOW GET m.Price
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

*E300956,1 Keep history of order line calceled quantity.
IF !m.lNewLine
  SELECT (lcOrdCanLn)
  IF llCnRemain
    IF !SEEK('O'+&lcInvLine..Order+STR(&lcInvLine..LineNO,6))
      =gfwCodePop(@laCodes,'CCANCRESON','D')
      INSERT INTO (lcOrdCanLn) (cOrdType,Order,LineNo,cCancReson,Cancelled) VALUES ;
      ('O',&lcInvLine..Order,&lcInvLine..LineNo,laCanReason[lnCanReason,2],gdSysDate)
    ENDIF
    =RLOCK()
    REPLACE Qty1 WITH MAX(m.Book1-m.Qty1,0) ,;
            Qty2 WITH MAX(m.Book2-m.Qty2,0) ,;
            Qty3 WITH MAX(m.Book3-m.Qty3,0) ,;
            Qty4 WITH MAX(m.Book4-m.Qty4,0) ,;
            Qty5 WITH MAX(m.Book5-m.Qty5,0) ,;
            Qty6 WITH MAX(m.Book6-m.Qty6,0) ,;
            Qty7 WITH MAX(m.Book7-m.Qty7,0) ,;
            Qty8 WITH MAX(m.Book8-m.Qty8,0) ,;
            TotQTy WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
    UNLOCK
    SHOW GET pbCnRemain ENABLE
  ELSE
    IF SEEK('O'+&lcInvLine..Order+STR(&lcInvLine..LineNO,6))
      DELETE
    ENDIF
    SHOW GET pbCnRemain DISABLE
  ENDIF
  SELECT (lcInvLine)
ENDIF
*E300956,1 (End)
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

*B603119,1 (Start) Save the push button in the begaining of the function
SELECT UNCMSESS
REPLACE cCurrObj WITH 'pbSav'
USE IN UNCMSESS && Close the file in order not to corrupt then open it  
=gfOpenFile(gcDataDir+'UNCMSESS','TRANS','SH')
*B603119,1 (End)

*B803747,1 NAD (Start) 10/25/2000 Commented out and retyped after the gfChkSavInv.
*-- Display the summery batch screen
*IF !llContinue .AND. !lfBatchSum(.T.)
*  llCSave = .F.
*  RETURN
*ENDIF
*B803747,1 NAD (END)

*B602145,1 Force enter dyelots for styles use dyelots.
SET ORDER TO TAG 'DYELOT' IN (lcInvLine)
*B602145,1 (End)

SELECT (lcInvHdr)
*-- scan only for the selected orders
*B803747,1 NAD 10/26/2000 (Start) Save the current record.
PRIVATE lnPos
lnPos=RECNO(lcInvHdr)
*B803747,1 NAD 10/26/2000 (End)

SCAN FOR cSelect='�' 
  *B602145,1 Force enter dyelots for styles use dyelots.
  IF Consol <> 'Y' AND SEEK(Account+Order+Store+PikTkt+'Y'+SPACE(10),lcInvLine)
    *B603308,1 WAB - display message "one shipped styles has not been assigned 
    *B603308,1 WAB - dyelots". only in case ther is no dyelots and record not 
    *B603308,1 WAB - cleared ( totqty <> 0 ) 
    SELECT (lcInvline)
    LOCATE REST FOR Account+Order+Store+PikTkt+'Y'+SPACE(10) = &lcInvHdr..Account+;
           &lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+'Y'+SPACE(10) ;
           AND TotQty <> 0
    IF FOUND()
    *B603308,1 WAB - END
      *B602145,1 Message : 40153
      *B602145,1 One or more shipped styles has not been assigned dyelots.
      *B602145,1 Button : 00000
      *B602145,1 Ok
      =gfModalGen('TRM40153B00000','ALERT')
      llCSave = .F.
      EXIT
    *B603308,1 WAB - endif --> If found()
    ENDIF
    SELECT (lcInvHdr)
    *B603308,1 WAB - END
  ENDIF
  *B602145,1 (End)

  IF Consol='Y' OR EMPTY(Flag)
    llSaveInv=.F.
    *B801882,1 MAN The following bug 602737 is handled whith the fix for the 
    *B801882,1 MAN Bug 801882 So the code is commented
    *B602737,1 MAN 04/04/1999 Total Charges is saved with 0 values sometimes
    *B602737,1 Recompute Charges before Saving
    *IF !llIsEngland .AND. lCompUps
    *  PRIVATE lcCInvOrd  
    *  lcCInvOrd = ORDER()
    *  =lfvUpsChrg(.T.)
    *  SET ORDER TO TAG (lcCInvOrd)
    *ENDIF  
    *B602737,1 END
    *-- check the invoice before saving
    
    DO gfChkSavInv IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
    Account,Order,Store,PikTKt,lcInvHdr,IIF(USED(lcInstHdr),lcInstHdr,''),;
    IIF(USED(lcAppCrdt),lcAppCrdt,''),IIF(USED(lcUpsBox),lcUpsBox,''),'llSaveInv'
      
    IF !llSaveInv
      llCSave = .F.
      EXIT
    ENDIF
  ENDIF
ENDSCAN  
*B602145,1 Force enter dyelots for styles use dyelots.

*B803747,1 NAD 10/26/2000 (Start) go the the saved record.
IF BETWEEN (lnPos,1,RECCOUNT(lcInvHdr))
  GOTO lnPos IN (lcInvHDr)
ENDIF
*B803747,1 NAD 10/26/2000 (End)

SET ORDER TO TAG (lcInvLine) IN (lcInvLine)
IF !llCSave
  IF lnActFolder = 3
    SELECT (lcInvLine)
    SET KEY TO laData[4]+laData[1]+laData[5]+laData[3]
  ENDIF

  *B605864,1 (Begin) Fix the bug of "End of file" when pressing "Clear" after trying to save
  *B605864,1         an order without any qtys. 
  IF EOF(lcInvLine)
    GO TOP IN (lcInvLine)
  ENDIF  
  *B605864,1 (End)
  
  RETURN
ENDIF

*B803747,1 NAD (Start) 10/25/2000 Displayed the batch summary after the gfChkSavInv.
IF !llContinue .AND. !lfBatchSum(.T.)
  llCSave = .F.
  *B605864,1 (Begin) Fix the bug of "End of file" when pressing "Cancel" not "Proceed"
  *B605864,1         when approving saving.
  IF EOF(lcInvLine)
    GO TOP IN (lcInvLine)
  ENDIF  
  *B605864,1 (End)
  RETURN
ENDIF
*B803747,1 NAD (End)

*B602145,1 (End)
DELETE ALL FOR cSelect+Account+Order+Store+PikTkt = SPACE(1)
*B603119,1 (Start) commented out
*SELECT UNCMSESS
*REPLACE cCurrObj WITH 'pbSav'
*B603119,1 (End)
SET KEY TO 
*-- Array to hold invoice number
DECLARE laInv[1]
STORE '' TO laInv

*E300956,1 Pass order canceled quantity temp file name
*-- save the invoice
*B803634,1  NAD 09/03/2000  (Start) Call the function to recalculate shipped quantity & amount. 
=lfReClcShp()
*B803634,1 NAD (End)


IF gfModalGen('QRM00000B40000','DIALOG','','', 'Permenantly Update invoices') = 2
  
  lcSadInvH = gfTempName()  
  lcSadInvL = gfTempName()
  lcSadUpsB = gfTempName()
  lcSadInsH = gfTempName()  
  lcSadInsL = gfTempName()
  lcSadOrdC = gfTempName() 
  
  *sameh
  *=gfDoTriger('SDIINV',PADR('RESAVE',10))
  =gfDoTriger('REIINV',PADR('RESAVE',10))
  
   glQuitting=.T.
      CLEAR READ
      RETURN
ELSE
  DO gpSaveInv IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
  lcInvHdr,lcInvLine,IIF(USED(lcUpsBox),lcUpsBox,''),IIF(USED(lcEngChrg),lcEngChrg,''),;
  IIF(USED(lcInstHdr),lcInstHdr,''),IIF(USED(lcInstLin),lcInstLin,''),;
  IIF(USED(lcOrdCanLn),lcOrdCanLn,''),lcGlSession,'laInv'
ENDIF

*C200245,1 Remove the Lines in Custom invoice line file. [Begin]
SELECT InvLines
SET ORDER TO TAG InvLines
FOR lnI = 1 TO ALEN(laInv,1)
  IF SEEK(laInv[lnI])
    DELETE REST WHILE cSadInv = laInv[lnI]
  ENDIF
ENDFOR
*C200245,1 Remove the Lines in Custom invoice line file. [End]

SELECT unCmSess
*B603119,1 (Start) Get the record of this transaction
IF SEEK('O'+PADR('IINVOICE',10)+PADR(gcUser_id,10)+lcSession)
*B603119,1 (End)

  REPLACE STATUS WITH 'C'
  llContinue = .F.
  UNLOCK
*B603119,1 (Start)
ENDIF
*B603119,1 (End)
lnInvCount = ALEN(laInv)
DO CASE
  CASE EMPTY(laInv)
    llCSave = .F.
  CASE lnInvCount=1
    *E300817,1 Message : 40005
    *E300817,1 Invoice has been saves as.
    *E300817,1 Button : 00000 
    *E300817,1 Ok
    =gfModalGen('INM40005B00000','ALERT',laInv[1])
  OTHERWISE
    *E300817,1 Message : 40118
    *E300817,1 Invoices 999999 Through 999999.
    *E300817,1 Button : 00000 
    *E300817,1 Ok
    =gfModalGen('INM40118B00000','ALERT',laInv[1]+'|'+laInv[lnInvCount])
ENDCASE
SELECT (lcInvHdr)

 glQuitting=.T.
      CLEAR READ
      RETURN


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
  IF gfUnCompSession(lcProgID, lnSessNo,"Invoice Sales Orders")
  
    *B603119,1 NAD (Start)  open the files before the if condition    
    IF llFrmSetup
      =gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
      =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
      =gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')  
      =gfOpenFile(gcDataDir+'PACK_LIN',gcDataDir+'PACK_LIN','SH')
      =gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH')
      =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
      =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
      llOpnFiles=.F.      
    ENDIF 
    *B603119,1 NAD (END)  

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
        *MAN Opened the style & stydye file
        *B603119,1 (Start) commented out
        *=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
        *=gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
        *B603119,1 (End)
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
SELECT InvHdr
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
*B802865,1 WAB - add field  to temp file
*DIMENSION laFileStru[lnFileStru+13,4]
*DIMENSION laFileStru[lnFileStru+14,4]
DIMENSION laFileStru[lnFileStru+15,4]
*B802865,1 WAB - END
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
laFileStru[lnFileStru+7,4] = 2
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
*B605864,1 (Begin) The cartons only 5 chrs as not to case problem in CEILING() when calculating cartons.
*laFileStru[lnFileStru+12,3] = 11
*laFileStru[lnFileStru+12,4] = 5
laFileStru[lnFileStru+12,3] = 5
laFileStru[lnFileStru+12,4] = 0
*B605864,1 (End)
laFileStru[lnFileStru+13,1] = 'Ordered'
laFileStru[lnFileStru+13,2] = 'N'
laFileStru[lnFileStru+13,3] = 7
laFileStru[lnFileStru+13,4] = 0
*B802865,1 WAB - Add field cConStore to hold Store Code in case fo consolidate
laFileStru[lnFileStru+14,1] = 'cConStore'
laFileStru[lnFileStru+14,2] = 'C'
laFileStru[lnFileStru+14,3] = 8
laFileStru[lnFileStru+14,4] = 0
*B802865,1 WAB - END
laFileStru[lnFileStru+15,1] = 'cSadInv'
laFileStru[lnFileStru+15,2] = 'C'
laFileStru[lnFileStru+15,3] = 6
laFileStru[lnFileStru+15,4] = 0



*C102314,1 BWA Commented out
*laFileStru[ASCAN(laFileStru,'TAX_RATE')+2]=10
*laFileStru[ASCAN(laFileStru,'TAX_RATE')+3]=7
*laFileStru[ASCAN(laFileStru,'DISCPCNT')+2]=10
*laFileStru[ASCAN(laFileStru,'DISCPCNT')+3]=7
*C102314,1 BWA

DECLARE laIndex[3,2]
laIndex[1,1] = 'Account+Order+Store+PikTkt'
laIndex[1,2] = lcInvHdr
laIndex[2,1] = 'cSelect+Account+Order+Store+PikTkt'
laIndex[2,2] = 'Select'
laIndex[3,1] = 'Consol+Account+cDivision+cCurrCode'
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
SELECT OrdLine
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
*B802865,1 WAB -Add field To temp file
*DIMENSION laFileStru[lnFileStru+12,4]

*B119562,1 SSE 09/08/2003 Add cSadInv to Temp Invoice Line file. [Begin]
*DIMENSION laFileStru[lnFileStru+13,4]
DIMENSION laFileStru[lnFileStru+22,4]
*B119562,1 SSE 09/08/2003 Add cSadInv to Temp Invoice Line file. [End]

*B802865,1 WAB - END
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
*B603068 (Start) 
*laFileStru[lnFileStru+6,3] = 5
laFileStru[lnFileStru+6,3] = 3
*B603068 (End)
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

*B802865,1 WAB - Add field cConStore to hold Store Code in case fo consolidate
laFileStru[lnFileStru+13,1] = 'cConStore'
laFileStru[lnFileStru+13,2] = 'C'
laFileStru[lnFileStru+13,3] = 8
laFileStru[lnFileStru+13,4] = 0
*B802865,1 WAB - END

*B119562,1 SSE 09/08/2003 Add cSadInv to Temp Invoice Line file. [Begin]
laFileStru[lnFileStru+14,1] = 'cSadInv'
laFileStru[lnFileStru+14,2] = 'C'
laFileStru[lnFileStru+14,3] = 6
laFileStru[lnFileStru+14,4] = 0

laFileStru[lnFileStru+15,1] = 'TmpShp1'
laFileStru[lnFileStru+15,2] = 'N'
laFileStru[lnFileStru+15,3] = 6
laFileStru[lnFileStru+15,4] = 0

laFileStru[lnFileStru+16,1] = 'TmpShp2'
laFileStru[lnFileStru+16,2] = 'N'
laFileStru[lnFileStru+16,3] = 6
laFileStru[lnFileStru+16,4] = 0

laFileStru[lnFileStru+17,1] = 'TmpShp3'
laFileStru[lnFileStru+17,2] = 'N'
laFileStru[lnFileStru+17,3] = 6
laFileStru[lnFileStru+17,4] = 0

laFileStru[lnFileStru+18,1] = 'TmpShp4'
laFileStru[lnFileStru+18,2] = 'N'
laFileStru[lnFileStru+18,3] = 6
laFileStru[lnFileStru+18,4] = 0

laFileStru[lnFileStru+19,1] = 'TmpShp5'
laFileStru[lnFileStru+19,2] = 'N'
laFileStru[lnFileStru+19,3] = 6
laFileStru[lnFileStru+19,4] = 0

laFileStru[lnFileStru+20,1] = 'TmpShp6'
laFileStru[lnFileStru+20,2] = 'N'
laFileStru[lnFileStru+20,3] = 6
laFileStru[lnFileStru+20,4] = 0

laFileStru[lnFileStru+21,1] = 'TmpShp7'
laFileStru[lnFileStru+21,2] = 'N'
laFileStru[lnFileStru+21,3] = 6
laFileStru[lnFileStru+21,4] = 0

laFileStru[lnFileStru+22,1] = 'TmpShp8'
laFileStru[lnFileStru+22,2] = 'N'
laFileStru[lnFileStru+22,3] = 6
laFileStru[lnFileStru+22,4] = 0
*B119562,1 SSE 09/08/2003 Add cSadInv to Temp Invoice Line file. [End]

DECLARE laIndex[4,2]
laIndex[1,1] = 'Account+Order+Store+PikTkt+STR(LineNo,6)'
laIndex[1,2] = lcInvLine
laIndex[2,1] = 'Account+Order+Store+PikTkt+Style'
laIndex[2,2] = 'Styles'
laIndex[3,1] = 'Consol+Account+cDivision+cCurrCode+Style'
laIndex[3,2] = 'Consol'
*B602145,1 Force enter dyelots for styles use dyelots.
laIndex[4,1] = 'Account+Order+Store+PikTkt+cDyeFlag+Dyelot'
laIndex[4,2] = 'Dyelot'
*B602145,1 (End)

=gfCrtTmp(lcInvLine,@laFileStru,@laIndex)
*B603760,1 NAD 07/30/2000 (Start) Added the cWareCode to the Index expression to group lines by warehouse. 
*=gfCrtTmp(lcConsInvD,@laFileStru,[Consol+Account+cDivision+cCurrCode+Style],lcConsInvD)
=gfCrtTmp(lcConsInvD,@laFileStru,[Consol+Account+cDivision+cCurrCode+cWareCode+Style],lcConsInvD)
*B603760,1 (End)
SELECT (lcInvLine)
SET ORDER TO TAG (lcInvLine)
SCATTER MEMVAR BLANK MEMO

*E301077,50 Inhance openning files to speed up transaction
*SET RELATION TO STYLE INTO STYLE
*SET RELATION TO Style+cWareCode+Dyelot INTO STYDYE ADDITIVE
*E301077,50 (End)

IF laSetups[15,2] = 'Y'
  *E301077,50 Inhance openning files to speed up transaction
  =gfOpenFile(gcDataDir+'UPSBOX',gcDataDir+'UPSBOX','SH')
  =AFIELDS(laFileStru)
  =gfCloseFile('UPSBOX')
  *E301077,50 (End)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'Order'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'PikTkt'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 6
  laFileStru[lnFileStru+2,4] = 0
  
  =gfCrtTmp(lcUpsBox,@laFileStru,[Order+Store+PikTkt+STR(CARTONS,5)],lcUpsBox)
ENDIF
IF llIsEngland
  *--MAN If called from Invoice Sales Order the file is not opened
  IF !USED("InvChrg")
    =gfOpenFile(gcDataDir+'InvChrg',gcDataDir+'InvChrg','SH')
  ENDIF  
  
  SELECT InvChrg 
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'Order'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'PikTkt'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 6
  laFileStru[lnFileStru+2,4] = 0
  =gfCrtTmp(lcEngChrg,@laFileStru,[Order+cStore+PikTkt+cchrgcode],lcEngChrg)
ENDIF
=gfCrtTmp(lcInstHdr,[(Order C(6),Store C(8),PikTkt C(6),;
          cInstmType C(1), nInstmFreq N(3),nInstIAmnt N(11,2),dInstmStDt D,;
          nNoInstm N(3), cInstmRef C(30),nInstIPcnt N(6,2),nInstmAmnt N(10,2))],;
          [Order+Store+PikTkt],lcInstHdr)
=gfCrtTmp(lcInstLin,[(Order C(6),Store C(8),PikTkt C(6),;
            cInstalNo C(3),nInstmAmnt N(10,2),DueDate D,nInstmPcnt N(6,2),cInstmNote C(30))],;
            [Order+Store+PikTkt+cInstalNo],lcInstLin)

*E301077,50 Inhance openning files to speed up transaction
*E300956,1 New file to store Order lines canceled quantity
*SELECT ORDCANLN
*=AFIELDS(laFileStru)
*=gfCrtTmp(lcOrdCanLn,@laFileStru,[CORDTYPE+ORDER+STR(LINENO,6)],lcOrdCanLn)
=gfCrtTmp(lcOrdCanLn,[(cOrdType C(1),Order C(6),LineNo N(6),Qty1 N(6),;
            Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),;
            Qty8 N(6),TotQty N(7),Cancelled D,cCancReson C(6))],;
            [CORDTYPE+ORDER+STR(LINENO,6)],lcOrdCanLn)
*E301077,50 (End)
*B802979,1 Default invoice quantity to packed quantity
=gfCrtTmp(lcPackLine,[(Pack_No C(6),Order C(6),nOrdLineNo N(6),Qty1 N(6),;
            Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),;
            Qty8 N(6),TotQty N(7))],;
            [PACK_NO+ORDER+STR(nOrdLineNo,6)],lcPackLine)
*B802979,1 (End)

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
DO gpInvInstm IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
   laData[11],laData[1],laData[5],laData[3],ROUND(m.TotalChg,2),laData[6],;
   lcInstHdr,lcInstLin
   
FUNCTION lfGtSavBar
PARAMETERS lcBar

USE (gcSysHome+"SYCMenu") IN 0 ORDER Pross_ID AGAIN ALIAS MenuFile
lnRetVal = IIF(SEEK(lcBar, "MenuFile"), VAL(MenuFile.cBar_Pos),0)
USE IN MenuFile
RETURN (lnRetVal)

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
lnPrice = IIF(!llMulCurr OR laData[14]=gcBaseCurr,Style.Price&lcLevel,;
                      gfStyPrice(lcStyle,lcLevel,laData[14]))
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
lnPrice1 = IIF(!llMulCurr OR laData[14]=gcBaseCurr,Style.PriceA,;
                       gfStyPrice(lcStyle,'A',laData[14]))
lnPrice2 = IIF(!llMulCurr OR laData[14]=gcBaseCurr,Style.PriceB,;
                       gfStyPrice(lcStyle,'B',laData[14]))
lnPrice3 = IIF(!llMulCurr OR laData[14]=gcBaseCurr,Style.PriceC,;
                       gfStyPrice(lcStyle,'C',laData[14]))
lnPrice   = IIF(lnPrice1 >0,1,IIF(lnPrice2 >0,2,AT(lcLevel,'ABC')))
lcPrompt  = "Price level '"+lcLevel+"' is zero. Proceed with"
lcPrompt1 = 'Price level A, '+ALLTRIM(STR(lnPrice1,12,2))
lcPrompt2 = 'Price level B, '+ALLTRIM(STR(lnPrice2,12,2))
lcPrompt3 = 'Price level C, '+ALLTRIM(STR(lnPrice3,12,2))

DO (gcScrDir+"SOSTYPRI.SPX")
RETURN(EVAL('lnPrice'+STR(lnPrice,1)))

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
*B802865,1
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
*! B802865,1
*!************************************************************************
FUNCTION lfChekCons
PRIVATE lnRecNo,lcAccount,lcOrder,lcStore,lcPikTkt,lcConslFlag,llConAll

*B803060,1 ABD Change the flag to be true at any time unless the ShipVia <> '*' .  [ Begin ]
*llConAll = .F.
llConAll = .T.
*B803060,1 ABD [ End ]

lnRecNo = RECNO()
SET ORDER TO TAG 'CONSOL'
IF SEEK('Y'+Account+cDivision+cCurrCode)
  llConAll = (ShipVia = '*')
ENDIF

SET ORDER TO TAG (lcInvHdr)
GO lnRecNo
RETURN llConAll

*!*************************************************************
*! Name      : lfPackLinv
*! Developer : WAM
*! Date      : 01/22/2000
*! Purpose   : Compute order packed quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcOrder  : Order#
*!                       lcPackNO : Pack#
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfPackLin(lcOrder,lcPAckNO)
*!*************************************************************
FUNCTION lfPackLin
PARAMETERS lcOrder,lcPackNO
WAIT 'Computing packed quantity...' WINDOW NOWAIT
SELECT PACK_LIN
=SEEK(lcPackNO)
SCAN REST WHILE pack_no+STR(line_no,3)+style+cpackcolor = lcPackNO
  IF !SEEK(lcPackNO+lcOrder+STR(nordlineno,6),lcPackLine)
    INSERT INTO (lcPackLine) (Order,Pack_No,nOrdLineNo) VALUES ;
    (lcOrder,lcPackNO,Pack_Lin.nordlineno)
  ENDIF
  SELECT (lcPackLine)
  REPLACE Qty1 WITH Qty1 + Pack_Lin.Qty1 ,;
          Qty2 WITH Qty2 + Pack_Lin.Qty2 ,;
          Qty3 WITH Qty3 + Pack_Lin.Qty3 ,;
          Qty4 WITH Qty4 + Pack_Lin.Qty4 ,;
          Qty5 WITH Qty5 + Pack_Lin.Qty5 ,;
          Qty6 WITH Qty6 + Pack_Lin.Qty6 ,;
          Qty7 WITH Qty7 + Pack_Lin.Qty7 ,;
          Qty8 WITH Qty8 + Pack_Lin.Qty8 ,;
          TotQty WITH TotQty + Pack_Lin.TotQty
ENDSCAN
WAIT CLEAR


*B603395,1 [Start] added function
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
    = SEEK (&lcInvHdr..Account+&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt)
    *-- Accumulate sales Rep. commission
    SCAN REST WHILE Account+Order+Store+PikTkt = ;
                  &lcInvHdr..Account+&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt
      lnNetAmnt = &lcInvLine..TotQty*&lcInvLine..Price*(1-&lcInvHdr..DiscPcnt/100)*(1-&lcInvHdr..Trde_Disc/100)
      lnLnComm1   = lnLnComm1 + (lnNetAmnt * &lcInvLine..Comm1 / 100)
      lnLnComm2   = lnLnComm2 + (lnNetAmnt * &lcInvLine..Comm2 / 100)
    ENDSCAN

    *-- Calculate net shipment amount
    lnNetShipAmnt = &lcInvHdr..ShipAmt*(1-&lcInvHdr..DiscPcnt/100)*(1-&lcInvHdr..Trde_Disc/100)
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
*++ naD
select (LCINVHDR)

*B605193,1 Replace the memory variable with the field in the file. [Begin]
*REPLACE Discount WITH -1*ShipAmt*(m.DiscPcnt/100)
REPLACE Discount WITH -1*ShipAmt*(DiscPcnt/100)
*B605193,1 Replace the memory variable with the field in the file. [End]

SELECT (lnAlias)
WAIT CLEAR
*-- end of lfRefComm.
*B603395,1 [End]
*!*************************************************************
*! Name      : lfOldValue
*! Developer : Abdou ElGendi
*! Date      : 02/17/2000
*! Purpose   : When function of size quantities , total and net price
*!           : to store old value of the current filed.
*! Reference : *B603449,1
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

*:*************************************************************
*! Name      : lfPpStat
*! Developer : Nader Anis (NAD)
*! Date      : 03/28/2000
*! Purpose   : Disable Style Prepack code and quantity if style scale does not have prepacks
*! Reference : B602760,1
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************

FUNCTION lfPpStat

IF SEEK('P'+Scale,'SCALE')
  SHOW GET m.Prepak ENABLE
  SHOW GET m.PpQty  ENABLE
  SHOW GET ibPrePak ENABLE
ELSE
  SHOW GET m.PrePak DISABLE
  SHOW GET m.PpQty  DISABLE
  SHOW GET ibPrePak DISABLE
ENDIF   

*--End of lfPpStat

*:*************************************************************
*! Name      : lfwTaxAmt
*! Developer : Nader Anis (NAD)
*! Date      : 04/03/2000
*! Purpose   : To disable editing the Tax amount if the pecentage >0
*! Reference : B602760,1
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
    RETURN m.ShipAmt > 0 
  ENDIF
ENDIF
RETURN .F.

*:*************************************************************
*! Name      : lfReClcShp
*! Developer : Nader Anis (NAD)
*! Date      : 
*! Purpose   : Recalculate Ship Amount 
*! Reference : B803634,1 
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

*B119562,1 SSE 09/08/2003 Define variable for Book Quantity. [Begin]
PRIVATE lnTotBook
lntotBook = 0
*B119562,1 SSE 09/08/2003 Define variable for Book Quantity. [End]

IF llFrmFlder
  lnRec=RECNO(lcInvLine)
  IF SEEK(laData[4]+laData[1]+laData[5]+laData[3],lcInvLine)
    SELECT (lcInvline)
    
    *B119562,1 SSE 09/08/2003 Fix bug of wrong summation when getting 2 consol. invoices for same account. [Begin]
    *SUM REST TotQty,TotQty * Price FOR Consol =&lcInvHdr..Consol TO lnShp,lnShpAmt WHILE  Account+Order+Store+PikTkt = laData[4]+laData[1]+laData[5]+laData[3]
    SUM REST TotQty,TotQty * Price,TotBook FOR Consol =&lcInvHdr..Consol AND cSadInv =&lcInvHdr..cSadInv TO lnShp,lnShpAmt,lnTotBook WHILE  Account+Order+Store+PikTkt = laData[4]+laData[1]+laData[5]+laData[3]
    *B119562,1 SSE 09/08/2003 Fix bug of wrong summation when getting 2 consol. invoices for same account. [End]
    
    SELECT (lcInvHdr)
    REPLACE Ship    WITH lnShp 
    REPLACE ShipAmt WITH lnShpAmt

    *B119562,1 SSE 09/08/2003 Fix bug of wrong summation when getting 2 consol. invoices for same account. [Begin]
    REPLACE Ordered WITH lnTotBook
    *B119562,1 SSE 09/08/2003 Fix bug of wrong summation when getting 2 consol. invoices for same account. [End]

  ENDIF
  SELECT (lcInvline)
  IF BETWEEN(lnRec ,1,RECCOUNT())
    GOTO lnRec
  ENDIF   
ELSE
  SELECT (lcInvHdr) 
  SCAN
    STORE 0 TO lnShp,lnShpAmt
    IF SEEK(Account+Order+Store+PikTkt,lcInvLine)
      SELECT (lcInvline)
      
      *B119562,1 SSE 09/08/2003 Fix bug of wrong summation when getting 2 consol. invoices for same account. [Begin]
      *SUM REST TotQty,TotQty * Price FOR Consol = &lcInvHdr..Consol TO lnShp,lnShpAmt WHILE  Account+Order+Store+PikTkt = &lcInvHdr..Account+&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt
      SUM REST TotQty,TotQty * Price,TotBook FOR Consol =&lcInvHdr..Consol AND cSadInv =&lcInvHdr..cSadInv TO lnShp,lnShpAmt,lnTotBook WHILE  Account+Order+Store+PikTkt = &lcInvHdr..Account+&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt
      *B119562,1 SSE 09/08/2003 Fix bug of wrong summation when getting 2 consol. invoices for same account. [End]

      SELECT (lcInvHdr) 
      REPLACE Ship    WITH lnShp
      REPLACE ShipAmt WITH lnShpAmt

      *B605193,1 Replace the memory variable with the field in the file. [Begin]
      *REPLACE Discount WITH -1*ShipAmt*(m.DiscPcnt/100)
      REPLACE Discount WITH -1*ShipAmt*(DiscPcnt/100)
      *B605193,1 Replace the memory variable with the field in the file. [End]

      *B119562,1 SSE 09/08/2003 Fix bug of wrong summation when getting 2 consol. invoices for same account. [Begin]
      REPLACE Ordered WITH lnTotBook
      *B119562,1 SSE 09/08/2003 Fix bug of wrong summation when getting 2 consol. invoices for same account. [End]

    ENDIF
  ENDSCAN
ENDIF
SELECT (lnOAlias)

*:********************************************************************************
*! Name      : lfGetOrdPkT
*! Developer : Nader Anis (NAD)
*! Date      : 09/05/2000
*! Purpose   : Get the orders or the orders with pick ticket in the scope screen. 
*! Reference : B803616,1
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
IF cbPickedOrd

  IF SEEK (Order,'Piktkt')   
    lnOAlias=ALIAS()
    SELECT Piktkt
    *B803822,1 NAD  11/26/2000 (Start) Not to scan in the PikTkt File.    
    *SCAN REST FOR EVAL(lcPkTCond) WHILE Order+PikTkt=OrdHdr.Order
    PRIVATE llFndPik
    llFndPik=.F. 
    SCAN REST WHILE Order+PikTkt=OrdHdr.Order
      IF EVAL(lcPkTCond)
        llFndPik=.T.
         EXIT
       ENDIF 
    ENDSCAN
    SELECT(lnOAlias)
    IF llFndPik
    *B803822,1 NAD 11/26/2000 (End)                                   
      =lfGetOrder(Order,'',cbPickedOrd,lcFromPick,lcToPick,.T.)          
    *B803822,1 NAD 11/26/2000   (Start) Commented out . 
    ENDIF
    * ENDSCAN
    * SELECT(lnOAlias)
    *B803822,1 NAD 11/26/2000 (End)  . 
  ENDIF
ELSE
  =lfGetOrder(Order,'',cbPickedOrd,lcFromPick,lcToPick,.T.)          
ENDIF


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
  *Message : 42000
  *Negative values are not allowed.
  *Buttfon  : 40011
  *Ok
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
*C102212,1
FUNCTION lfvHstTax
PARAMETERS llRate,llCalled
PRIVATE lnOTaxAmnt
IF lfNegChrg()
  SELECT (lcInvHdr)  
  m.cTaxRule = STR(lnPstRule,2)
  m.nHstAmt  = IIF(llRate,(ShipAmt+Freight+Insur+COD+;
               Discount+IIF(VAL(m.cTaxRule)=1,0,Tax_Amt))*;
               m.nHstRate/100,m.nHstAmt)
  m.nHstRate = IIF(!llRate,m.nHstAmt*100/(ShipAmt+Freight+Insur+COD+Discount+;
               IIF(VAL(m.cTaxRule)=1,0,Tax_Amt)),m.nHstRate)
  IF Cod_Flag='Y'
    IF EMPTY(lcUPSType) OR lcUPSType='OTHER'
      m.Cod_Amt = ShipAmt-ShipAmt*m.DiscPcnt/100 +m.Tax_Amt+m.nHstAmt
    ELSE
      m.Cod_Amt = ShipAmt+Freight+Insur+COD+Discount+Tax_Amt+m.nHstAmt
    ENDIF
  *C102239,1 (Begin) Update COD for all customers except Bel05.
  *IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. ;
      Cartons=1 .AND. SEEK(Order+Store+PikTkt,lcUpsBox)
  IF laSetups[15,2]='Y' .AND. SUBSTR(lcUpsType,1,5)='USUPS' ;
     AND (EMPTY(laData[3]) OR ASCAN(laEvntTrig , PADR('CRTCHRG',10)) = 0) AND ;
     Cartons=1 .AND. SEEK(Order+Store+PikTkt,lcUpsBox)
  *C102239,1 (End)
      SELECT (lcUpsBox)
      =RLOCK()
      REPLACE TotalCod WITH m.Cod_Amt
      UNLOCK
    ENDIF
  ENDIF
  SELECT (lcInvHdr)  
  lnOTaxAmnt = nHstAmt

  m.TotalChg = ShipAmt+Freight+Insur+COD+Discount+Tax_Amt+m.nHstAmt+m.nPstAmt

  *C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
  IF ASCAN(laEvntTrig , PADR('SUMALL',10)) <> 0
    =gfDoTriger('ARIINV',PADR('SUMALL',10))
  ENDIF
  *C102314,1 BWA 06/10/2001 [END]

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

*B605016,1 BWA 10/14/2001 Fix the bug of updating the dates.[START]
*!*************************************************************
*! Name      : lfUpdateSd
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/14/2001
*! Purpose   : Function to update the dates of the invoice.
*!*************************************************************
*! Called from : lfvDefaults()
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfUpdateSd()
*!*************************************************************
FUNCTION lfUpdateSd
PRIVATE lcAlias , lcTermCode

lcAlias = ALIAS()
STORE SPACE(0) TO lcTermCode
SELECT (LCINVHDR)

SCAN

  REPLACE &LCINVHDR..INVDATE   WITH ldDefInvDate ,;
          &LCINVHDR..SHIPDATE  WITH ldDefInvDate ,;
          &LCINVHDR..DPOSTDATE WITH ldDefPstDate

  IF lcTermCode # &LCINVHDR..cTermCode
    =gfRltFld(&LCINVHDR..cTermCode,@laTRltFld,'CTERMCODE')
    lcTEOM = ALLTRIM(lcTEOM)
    lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)

    lcTermCode = &LCINVHDR..cTermCode
  ENDIF

  IF llIsEngland
    m.DueDate = IIF(lcTEOM <> 'Y',ldDefInvDate+lnTDaysDue,;
                    CTOD('01'+SUBSTR(DTOC(GOMONTH(ldDefInvDate,1)),3))-1+lnTDaysDue)
  ELSE                
    m.DueDate = IIF(lcTEOM <> 'Y',ldDefInvDate+lnTDaysDue,;
                    GOMONTH(CTOD(SUBSTR(DTOC(ldDefInvDate),1,3)+'10'+;
                    SUBSTR(DTOC(ldDefInvDate),6,5)),IIF(DAY(ldDefInvDate) > lnEOMDay,2,1))+lnTDaysDue)
  ENDIF

  REPLACE &LCINVHDR..DUEDATE WITH m.DueDate

ENDSCAN

*--End of lfUpdateSd.
*B605016,1.[END]

*!**************************************************************************
*! Name      : lfsrAcc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/08/2001
*! Purpose   : Rise change account flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrAcc()
*!**************************************************************************
*! Note      : S symbol is [S,Set]
*!**************************************************************************
*C200245,1 Add customer Set and Reset function in option grid.
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
ENDCASE
*-- End of lfsrAcc.

*!***************************************************************************
*! Name      : lfSRBillNo
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/08/2001
*! Purpose   : control browse BOL_No for Bill of Lading file
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example   : =lfSRBillNo()
*!***************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!***************************************************************************
*C200245,1 Add Bill no Set and Reset function in option grid.
FUNCTION lfSRBillNo
PARAMETERS lcParm

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT InvHdrs

DO CASE
  CASE lcParm = 'S'  && Set code
    GO TOP
  CASE lcParm = 'R'  && Reset code
    SELECT (lnAlias)
ENDCASE
*-- End of lfSRBillNo.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/08/2001
*! Purpose   : To get the position of the fixed filter in OG.
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*C200245,1 Get position of laOGFxFlt.
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*C200245,1 When function for the Option grid.
FUNCTION lfwRepWhen
lnBilPos = lfItmPos('INVHDRS.BOL_NO') && get Account Fixed filter Position.
lnAcctPos = lfItmPos('INVHDRS.ACCOUNT') && get Account Fixed filter Position.
lcBolName = laOGFxFlt[lnBilPos,6]
lcAcctName = laOGFxFlt[lnAcctPos,6]
*-- End of lfwRepWhen.

