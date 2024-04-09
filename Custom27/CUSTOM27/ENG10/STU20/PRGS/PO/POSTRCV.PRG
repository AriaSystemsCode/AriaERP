*:****************************************************************
*: Program file  : POSTRCV.PRG
*: Program desc. : Receive Styles against Purchase Orders.
*: For screen    : POSTRCV.SPR
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Style Paurchase Order (SO)
*: Developer     : TMI - TAREK MOHAMED IBRAHIM
*: Date          : 02/17/2004
*: Tracking #    : C#037439,1
*:****************************************************************

*-Open needed files and set relations
=lfOpenFls()

*:***************************************************************************1
* From here to the line ends with  ***2 is a copy from the file postrec.prg

*--If P/O you call program directly from menu without parameters.
*--Global program variable indicate the type of receiving.
lcPType  = IIF(TYPE('lcPType') $ 'UL','I',lcPType)
*--Global program variable flag indicate if it P/O or C/T receive.
llMFCall = ( lcPType $ 'MT' )
*--Initialize program variables.
STORE ' ' TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinCh3,lcWinCh4,lcWinCh5
STORE 0   TO lnstylewid,lnMjrWid
STORE ' ' TO lcStyHdr,lcStyPict,lcTCode,lcCostMth,lcSOrder,lcAOrder
STORE .F. TO llLinkToGl,llWareHous,llWareLoc,llDyelot,llPOSale
STORE .F. TO llImpCost,llMulCurr,llUseMCurr,llEditExRt,llLoc,llIgnorAll,llCostPrv
STORE ' ' TO lcCostImp,lcISlbl1,lcISlbl2,lcISlbl3,lcISlbl4,lcISlbl5,;
             lcIType1,lcIType2,lcIType3,lcIType4,lcIType5,lcDropLoc
STORE ' ' TO lcPfrnSmbl,lcDfrnSmbl,lcBaseSmbl,lcMasterLn
STORE ' ' TO lcTmpLine,lcTemLoc,lcGlDist,lcBlFields,lcGlSession,lcTmpCtPk,lcTempFile

*!B602698,1 [BEGIN] 
STORE ' ' TO lcTCodeOld &&-- variable to hold the last C/T or PO
*!B602698,1 [END..] 

*C101424,1 lcCurrSty : Variable to save current receive style [Begin]
*C101424,1             (Receive C/T by Style)
*STORE ' ' TO lcShpCode,lcBatch,lcBfields,lcStyle,lcStyDesc,lcOldValue,;
*             lcGlPeriod,lcGLFYear,lcCur1,lcCur2,lcAirWayB,lcShpRef,;
*             lcBStatus,lcBDesc,lcQltyKey,lcWareCode,lcDyelot,;
*             lcBrTpNme,lcLotNo,lcClrLstOp,lcPickIndx
STORE ' ' TO lcShpCode,lcBatch,lcBfields,lcStyle,lcStyDesc,lcOldValue,;
             lcGlPeriod,lcGLFYear,lcCur1,lcCur2,lcAirWayB,lcShpRef,;
             lcBStatus,lcBDesc,lcQltyKey,lcWareCode,lcDyelot,;
             lcBrTpNme,lcLotNo,lcClrLstOp,lcPickIndx,lcCurrSty,lcoSydKey
*C101424,1 lcCurrSty : Variable to save current receive style [Begin]

STORE 0   TO lnTotStk,lnTotDam,lnTotCan,lnCartons,lnBrRecNo,lnPolstln,;
             lnRate1,lnRate2
STORE 1   TO lnCurrUnt1,lnCurrUnt2,lnCrRt1,lnCrRt2
STORE .F. TO llBrowse,llByCarton,llNewItem,llEditLCst,llShpPO,llCont_Upd,llSpecHndl
STORE {}  TO ldEnterd,ldEta,ldBDate
STORE gdSysDate TO ldRcvDate,ldTrDate
llSOInstld = (OCCURS('SO',gcComp_mdl)<>0)
llPOInstld = (OCCURS('PO',gcComp_mdl)<>0)
*E301484,1 Check Id new communication module installed
*llCMInstld = (OCCURS('CM',gcComp_mdl)<>0)
llCMInstld = (OCCURS('NC',gcComp_mdl)<>0)
*E301484,1 (End)


*--Variables of opereation lots.
llSpecLot = .F.
DIME laLotArry[8]
laLotArry = 0

*--Variable for automatic Issue/receive.
lcAuto  = 'M'
llFirst = .T.

*B602698,1[BEGIN] Flag to indicate if the tmp file is created or not .
llFirstTmp = .F.
*B602698,1[END..]

*E300935,4 lcTmDyeRel : Temp. name to indicate our scope withen dye_rel file.
*E300935,4 llFabDye   : .T. if fabric use dyelots.
lcTmDyeRel = ''
llFabDye   = .F.
*E300935,4

*E300935,4 lcFabColor : variable that hold color for current fabric [begin]
lcFabColor = ''
*E300935,4 lcFabColor : variable that hold color for current fabric [end]

*--Screen Arrays.
*E301077,11 MAB No need to laOpFile array (Open with gfOpenFile Open files and  [Begin
*E301077,11 MAB add it to array laFileName).
*DIME laType[1,2],laWare[1],laECost[5],laOpFile[12]
DIME laType[1,2],laWare[1],laECost[5]
*E301077,11 MAB No need to laOpFile array (Open with gfOpenFile Open files and  [Begin

STORE ' ' TO laType,laWare[1]
STORE 0   TO laECost,lnWare

*--Uncomplte session variables(UNCSS).
DECLARE laVariables[15]
lcAProgm   = 'POSTREC'

lcUnCmPgId = 'RECIVING_'+lcPType
lnSessNo   = gnProgCopy
lcSession  = ' '
laVariables= ' '
llContinue = .F.
llGoAndChk = .T.

*--Uncomplete session variables.
laVariables[1]  = 'lcPType'
laVariables[2]  = 'lcShpCode'
laVariables[3]  = 'ldTrDate'
laVariables[4]  = 'ldRcvDate'
laVariables[5]  = 'lnTotStk'
laVariables[6]  = 'lnTotDam'
laVariables[7]  = 'lnTotCan'
laVariables[8]  = 'ldEnterd'
laVariables[9]  = 'lnCartons'
laVariables[10] = 'ldEta'
laVariables[11] = 'lcAirWayB'
laVariables[12] = 'lcShpRef'
laVariables[13] = 'lcGlSession'
laVariables[14] = 'lcBatch'
laVariables[15] = 'llMFCall'

*-- Variable to tell if are we going to check the existance of
*-- any incomplete session in the screen's setup or not.
llChkUnCom = .T.

IF !gfSetup()
  RETURN
ENDIF

*C200488,1 WAB (Start) call triger to create the temp. files 
IF ASCAN(laEvntTrig,PADR("CREATTMP",10)) <> 0
  =gfDoTriger("POSTREC",PADR("CREATTMP",10))
ENDIF
*C200488,1 WAB (End) 

*E301077,11 MAB Call gfGetMemVar Once for all memory variables [Begin]
*C200080,1 AMM Dimension the array of 22 anyway.
*DIMENSION laSetups[IIF(llMFCall,11,22),2]

*B101704,1 [Start] add new setup for PO
*DIMENSION laSetups[22,2]
DIMENSION laSetups[23,2]
*B101704,1 [End]

IF lcPType = 'A'
  lcSydKey = SPACE(0)
ENDIF
*C200080,1 AMM end
laSetups[1,1]  = 'M_WareHouse'
laSetups[2,1]  = 'M_WareLoc'
laSetups[3,1]  = 'M_Dyelot'
laSetups[4,1]  = 'M_MATDYE'
laSetups[5,1]  = 'M_Cost_Meth'
laSetups[6,1]  = 'M_Link_GL'
laSetups[7,1]  = 'M_DROPWARE'
laSetups[8,1]  = 'M_SYSTYPE'
laSetups[9,1]  = 'M_LImpCost'
laSetups[10,1] = 'llMulCurr' 
laSetups[11,1] = 'llEditExRa'
*C200080,1 AMM
*IF !llMFCall
*C200080,1 AMM end
laSetups[12,1] = 'M_cCostImp'
laSetups[13,1] = 'M_cISLbl1'
laSetups[14,1] = 'M_cISLbl2'
laSetups[15,1] = 'M_cISLbl3'
laSetups[16,1] = 'M_cISLbl4'
laSetups[17,1] = 'M_cISLbl5'
laSetups[18,1] = 'M_cIType1'
laSetups[19,1] = 'M_cIType2'
laSetups[20,1] = 'M_cIType3'
laSetups[21,1] = 'M_cIType4'
laSetups[22,1] = 'M_cIType5'      

*B101704,1 [Start]
laSetups[23,1] = 'M_GenStOrN'
llGenOrNum = .F.
*B101704,1 [End]
*C200080,1 AMM end if
*ENDIF
*C200080,1 AMM end
=gfGetMemVar(@laSetups,gcAct_Comp)
*E301077,11 MAB Call gfGetMemVar Once for all memory variables [End  ]

*E301077,11 MAB Change all variables corressponding to array col. 2 [Begin]
*--Setup use multi location y/n.
llWareHous = (laSetups[1,2]='Y')
llWareLoc  = (laSetups[2,2]='Y')
llDyelot   = (laSetups[3,2]='Y')
llFabDye   = (laSetups[4,2]='Y')
*E301077,11 MAB Change all variables corressponding to array col. 2 [End  ]

*--Cannot use the inter Location P/o if no multi location is used.
*E301291,4 AMM Consider Issue adornment order case.
*IF lcPType = 'N' AND !llWareHous

*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*IF lcPType $ 'NA' AND !llWareHous
IF lcPType $ 'NAH' AND !llWareHous
*C200170,1 AMH [End]

*E301291,4 AMM end
  *--The system has not been setup to use multiple locations. Cannot proceed.
  =gfModalGen('TRM42054B42001','DIALOG')
  RETURN
ENDIF
*E301077,11 MAB Open needed files [Begin


*E301077,11 MAB Open either Po Files or C/T files. [Begin]
*C200080,1 AMM (start)
*IF !llMFCall 
*C200080,1 AMM end
  =gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
  =gfOpenFile(gcDataDir+'POSLN','POSLN','SH')
*C200080,1 AMM end if
*ENDIF
*C200080,1 AMM end
*E301077,11 MAB Open either Po Files or C/T files. [End  ]

IF 'MA' $ gcComp_mdl
  = gfOpenFile(gcDataDir+"Fabric","Fabric","SH")
  = gfOpenFile(gcDataDir+"FabDye","FabDye","SH")
ENDIF  

*E301077,11 MAB Open Dyelot relationship table if style and material use dyelots. [Begin]
IF llDyelot AND llFabDye
  =gfOpenFile(gcDataDir+'DYE_REL','DYE_REL','SH')
ENDIF
*E301077,11 MAB Open Dyelot relationship table if style and material use dyelots. [End  ]

*B803337,1 WMA Open BOMLINE File[Begin]
=gfOpenFile(gcDataDir+'BOMLINE','BOMLINE','SH') 
*B803337,1 WMA [End]

*E301077,11 MAB Rise some flags to open files (in Prg not in module object) [Begin
STORE .F. TO llOpnBom,llOpnBomLn,llOpnBmCst,llOpnKtBom
*E301077,11 MAB Rise some flags to open files (in Prg not in module object) [End..

*E301077,11 MAB Open needed files [End..
*E301291,4 AMM Add Issue adornment order 
*IF lcPType = 'R'
  *laType[1,1]='Issue Return P/O'
  *laType[1,2]='R'
*ENDIF
*IF lcPType = 'N'
  *laType[1,1]='Issue Inter Location P/O'
  *laType[1,2]='N'
*ENDIF
DO CASE
  CASE lcPType = 'R'
    laType[1,1]='Issue Return P/O'
    laType[1,2]='R'
  CASE lcPType = 'N'
    laType[1,1]='Issue Inter Location P/O'
    laType[1,2]='N'
  CASE lcPType = 'A'
    laType[1,1]='Issue Adornment P/O'
    laType[1,2]='A'

  *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
  CASE lcPType = 'H'
    laType[1,1]='Issue Inter Location Batch'
    laType[1,2]='H'
  *C200170,1 AMH [End]

  *C102359,1 AMH Custome receive adornment aorder for JL [Start]
  CASE lcPType = 'E' AND ASCAN(laEvntTrig,PADR("RCVADORD",10)) <> 0
    laType[1,1]='Receive Adornment order'
    laType[1,2]='E'
  *C102359,1 AMH [End]
  
ENDCASE
*E301291,4 AMM end

*E301077,11 MAB No need to laOpFile.
*laOpFile=.F.

lnType  = 1

*--Control pannel additional elements. [begin]
*E301176,1 HDM 03/22/1999 [start] Prevent programs from displaying notepad icon
*                           as it's now controlled globally

*DECLARE laPanelObj[1,3]
*STORE '' TO laPanelObj
*laPanelObj[1,1] = 'pbPONote'
*laPanelObj[1,2] = gcBmpHome+'NOTES2.BMP'
*laPanelObj[1,3] = [VALID lfvNote() MESSAGE 'Notes' DISABLE]
*E301176,1 HDM 03/22/1999 [End]

*--Issue or Recieve operation.
*E301291,4 AMM Consider Issue adornment order case
*llIssue   = (lcPType $ 'RN')

*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*llIssue   = (lcPType $ 'RNA')
llIssue   = (lcPType $ 'RNAH')
*C200170,1 AMH [Start]

*E301291,4 AMM end

lcBrDtTtl  = IIF(llIssue,'Issuing Lines','Receiving Lines')

lcMasterLn = IIF(llMFCall,'CUTTKTL','POSLN')
*-Trancd no.
lcOthrTrCd = IIF(llMFCall,'3','4')
lcCanlTrCd = IIF(llMFCall,'4','5')

*E300935,4 lcSeekBomTp : variable that hold type of fabric item [begin]
lcSeekBomTp = ''
*E300935,4 lcSeekBomTp : variable that hold type of fabric item  [end]

IF .T. .OR. !WEXIST(gcBaseWind)

  *E300935,4 evaluate fabric type in bomline file [begin] 
  lcStyleTyp = IIF(llMFCall , "M" , "I")
  FOR lnCount = 1 TO 5
    lcCount  = STR(lnCount,1)
    lc&lcStyleTyp.TYPE&lcCount = gfGetMemVar('M_C'+lcStyleTyp+'TYPE'+lcCount , gcAct_Comp)
    IF lc&lcStyleTyp.TYPE&lcCount  = "F"
      lcSeekBomTp = lcCount
      EXIT
    ENDIF
    
  ENDFOR 
  *E300935,4 evaluate fabric type in bomline file [end] 

  *E300935,4 lcTmDyeRel : Temp. name to indicate our scope withen dye_rel file.
  lcTmDyeRel = gfTempName()
  *E300935,4

  *--Program variables.

  *-Style structure.
  lcStyHdr   = gfItemMask('HI')
  lcStyPict  = gfItemMask('PI')
  lnstylewid = LEN(lcStyPict)
  lnMjrWid   = LEN(gfItemMask('PM'))
  
  *E301077,11 MAB Assign setups col2 to all memvar [Begin]
  *-Global setups.
  *lcCostMth  = gfGetMemVar('M_Cost_Meth')
  lcCostMth  = laSetups[5,2]

  *llLinkToGl = gfGetMemVar('M_Link_GL')  ='Y'
  llLinkToGl = (laSetups[6,2] = 'Y')
  
  *--Restore the Drop Ship Location.
  *lcDropLoc  = gfGetMemVar('M_DROPWARE')
  lcDropLoc  = laSetups[7,2]

  *--System type Point of sale. 
  *llPOSale   = gfGetMemVar('M_SYSTYPE') = 'P'
  llPOSale   = (laSetups[8,2] = 'P')


  *-Costing setups.
  *llImpCost  = gfGetMemVar('M_LImpCost')
  llImpCost  = laSetups[9,2]
  IF EMPTY(llImpCost)
    llImpCost = .F.
  ENDIF

  *-Currency setups
  *llMulCurr  = gfGetMemVar('llMulCurr')
  *C200080,1 AMM No multi currency in Adornment or dye order
  *llMulCurr  = laSetups[10,2]
  llMulCurr  = IIF(lcPType $ 'DAE',.F.,laSetups[10,2])
  llUseMCurr = llMulCurr
  *C200080,1 AMM end

  *llEditExRt = gfGetMemVar('llEditExRa')
  llEditExRt = laSetups[11,2]
  *C200080,1 AMM (start) define the variable 
  *IF !llMFCall
  *C200080,1 AMM end
    *lcCostImp  = gfGetMemVar('M_cCostImp')
    lcCostImp  = laSetups[12,2]

    *lcISlbl1   = gfGetMemVar('M_cISLbl1')
    lcISlbl1   = laSetups[13,2]

    *lcISlbl2   = gfGetMemVar('M_cISLbl2')
    lcISlbl2   = laSetups[14,2]

    *lcISlbl3   = gfGetMemVar('M_cISLbl3')
    lcISlbl3   = laSetups[15,2]

    *lcISlbl4   = gfGetMemVar('M_cISLbl4')
    lcISlbl4   = laSetups[16,2]

    *lcISlbl5   = gfGetMemVar('M_cISLbl5')
    lcISlbl5   = laSetups[17,2]

    *lcIType1   = gfGetMemVar('M_cIType1')
    lcIType1   = laSetups[18,2]

    *lcIType2   = gfGetMemVar('M_cIType2')
    lcIType2   = laSetups[19,2]

    *lcIType3   = gfGetMemVar('M_cIType3')
    lcIType3   = laSetups[20,2]

    *lcIType4   = gfGetMemVar('M_cIType4')
    lcIType4   = laSetups[21,2]

    *lcIType5   = gfGetMemVar('M_cIType5')      
    lcIType5   = laSetups[22,2]
  *B101704,1 [Start]
  llGenOrNum = (UPPER(ALLTRIM(laSetups[23,2])) ='Y') 
  *B101704,1 [End]
  *C200080,1 AMM
  *ENDIF
  *C200080,1 AMM end
  *E301077,11 MAB Assign setups col2 to all memvar [End  ]

  llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')

  STORE SET('CURRENCY',1) TO lcPfrnSmbl,lcDfrnSmbl,lcBaseSmbl
  IF !llMulCurr
    STORE gcBaseCurr TO lcCur1,lcCur2  
  ENDIF 

  *--Program screen windows.
  lcWinCh0 = gfTempName()
  lcWinCh1 = gfTempName()
  lcWinCh2 = gfTempName()
  lcWinCh3 = gfTempName()
  lcWinCh4 = gfTempName()
  lcWinCh5 = gfTempName()

  *--Get a unique session number, to be used to update the uncomplete 
  *--session file(UNCSS).
  lcSession  = gfsequence('cSession')

  SELECT WAREHOUS
  IF llWareHous  
    IF llCMInstld AND llPOSale
      LOCATE FOR cSiteId = gcCurSite    
      laWare[1] = WAREHOUS.cWareCode+'-'+WAREHOUS.cDesc
      lnWare = 1
    ELSE
      
      *B606205,1 KHM 07/03/2002 (Begin) Get the ware houses that marked to be a style finished 
      *B606205,1                goods inventory only.      
      *SELECT cWareCode+'-'+cDesc FROM Warehous INTO ARRAY laWare
      SELECT cWareCode+'-'+cDesc FROM Warehous WHERE lStyInv INTO ARRAY laWare
      *B606205,1 KHM 07/03/2002 (End)
      
      DECLARE laWare [ALEN(laWare,1)+1,ALEN(laWare,2)] 
      =AINS('laWare',1)
      laWare[1,1] = "              N/A           "
    ENDIF
  ELSE
    GO TOP IN WAREHOUS
    laWare[1] = WAREHOUS.cWareCode+'-'+WAREHOUS.cDesc
  ENDIF

  *--Open needed files.
  *E301077,11 Start. Comment these files.
  *laOpFile[1] = gfOpenFile(gcSysHome+'SyuEror','cSession','SH')
  IF llMFCall OR (llImpCost AND !( lcPType $ 'RN' ))
    *-Used in cost sheet only.
    = gfOpenFile(gcDataDir+"MFGOprDt","MFGOprDt","SH")
*    laOpFile[2] = gfOpenFile(gcDataDir+"MFGOprHd","MFGOprHd","SH")

     *B602485,1 TAK comment out the following two lines before [Begin
     *B602485,1 but now I rewrite them. 
     *    laOpFile[4] = gfOpenFile(gcDataDir+"FabDye","FabDye","SH")
     *    laOpFile[5] = gfOpenFile(gcDataDir+"Fabric","Fabric","SH")
     *= gfOpenFile(gcDataDir+"FabDye","FabDye","SH")
     *= gfOpenFile(gcDataDir+"Fabric","Fabric","SH")
     *B602485,1 TAK comment out the following two lines before [End..

  ENDIF
  
  IF llSOInstld 
     =gfOpenFile(gcDataDir+"CutPick","Cutpick","SH")  
*    laOpFile[7] = gfOpenFile(gcDataDir+"OrdHdr","OrdHdr","SH")  
*    laOpFile[8] = gfOpenFile(gcDataDir+"OrdLine","OrdLine","SH")  
  ENDIF

*mab [Begin
*  IF llLinkToGl
*    laOpFile[9] = gfOpenFile(gcDataDir+'GLDIST','GlDistAc','SH')
*  ENDIF
*mab [End
  *E301077,11 End.

  IF llWareLoc 
    = gfOpenFile(gcDataDir+'WhsLoc','WhsLocSt','SH')  
  ENDIF  
  
  *E301077,11 MAB Comment out the following block open it if you need. [Begin
  *IF llPOInstld
  *  = gfOpenFile(gcDataDir+'ShpmtHdr','ShpmtHdr','SH')
  *ENDIF
  *E301077,11 MAB Comment out the following block open it if you need. [End  

  *--Temprorary files names.
  *B602698,1[BEGIN] Temp name for the Temp file
  lcTmpLine = gfTempName()
  *B602698,1[END..]
  lcTempFile  = gfTempName()
  IF llLinkToGl
    lcGlDist = gfTempName()   
  ENDIF
  IF llWareLoc 
    DIMENSION laSource[1],laTarget[1]
    lcTemLoc = gfTempName()   
  ENDIF  
  lcTmpCtPk = gfTempName()
  *--HDM 04/13/1999 E301176,1[Start] Assign value to lcBaseFile According to lcPType Parameter Value
  DO CASE
    *C200080,1 AMM Add the dye order type 'D'
    *CASE lcPType $ 'IRN'

    *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
    *CASE lcPType $ 'IRNDAE'
    CASE lcPType $ 'IRNDAEH'
    *C200170,1 AMH [End]

    *C200080,1 AMM end
      lcBaseFile = 'POSHDR'
      lcScFields = 'PO'
      *B802592,1 Added. 
      =lfSetFltr()
      *B802592,1 End. 
    CASE lcPType = 'M'
      lcScFields = 'cuttkt'
      lcBaseFile = 'CUTTKTH'
      *B802592,1 Added. 
      =lfSetFltr()
      *B802592,1 End. 
  ENDCASE
  *--HDM 04/13/1999 E301176,1[End]
  *E301291,4 AMM Get old note key variable
  lcoSydKey = lcSydKey
  *E301291,4 AMM end
ENDIF

*:***************************************************************************2


*-Get major style information
STORE '' TO lcTmpPoLn,lcBrowTitl
STORE 0  TO lnMrk
llBrowse = .F.
lcStyFrmat = GFITEMMASK("PI")
lnMajorLen = LEN(gfItemMask("PM"))
STORE 0 TO lnClrLen,lnClrPos
=lfGetClrD()

lcWinTitl = "Receive Styles against Purchase Orders."
lcBrowTitl = "Open PO's"
lcBrowFlds = ""
lnOldValue = 0

lcTmpLine = gfTempName()

STORE STRTRAN(lcStyFrmat,'X',' ') TO lcLastSty,m.STYLE
M.DESC   = ''

*-Create the temp file to collect data
=lfCrtTmpFl()

PUSH KEY
*ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowTitl)
ON KEY LABEL ALT+B DO lfBrowsLn
DO (gcScrDir+'PO\POSTRCV.SPX')
POP KEY

*:**************************************************************************
*:* Name        : lfvStyle
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/17/2004
*:* Purpose     : Valid function for style,collects all POs for the selected style
*:***************************************************************************
FUNCTION lfvStyle

IF llBrowse .OR. ;
  ( !EMPTY(SUBSTR(M.STYLE,1,lnMajorLen)) .AND. ;
    SUBSTR(lcLastSty,1,lnMajorlen) <> SUBSTR(M.STYLE,1,lnMajorlen)  .AND. ;
    !SEEK(PADR(m.Style,19),'Style') )
  *C037439,3  TMI [Start] select STYLE alias
  SELECT STYLE
  LOCATE
  *C037439,3  TMI [End  ] 
  m.Style = PADR(gfStyBrw("I" , m.Style , "" , .F.),19)  

  llBrowse = .F.
  IF EMPTY(m.Style)
    STORE SPACE(lnMajorLen) TO lcLastSty,m.STYLE
    _CUROBJ = OBJNUM(M.STYLE)
  ENDIF

ENDIF

*-If a style is selected then collect open PO's for it from Posln
IF !EMPTY(m.Style) .AND. lcLastSty <> M.STYLE
  llNewItem = .T.
  lcLastSty = m.Style
  SHOW GET Dumy ENABLE
  SHOW GET M.STYLE DISABLE
  SHOW GET ibStyle DISABLE
  
  lcSvOrder = ORDER('POSLN')
  SET ORDER TO POSLNS IN POSLN
  
  =SEEK('S'+STYLE.SCALE,'SCALE')
  
  m.Desc = STYLE.DESC
  SHOW GET m.Desc
  
  IF SEEK(M.STYLE,'POSLN')
    
    DO WHILE POSLN.STYLE+POSLN.CSTYTYPE+POSLN.PO = M.STYLE
      SELECT POSLN
      M.CSTYTYPE = POSLN.CSTYTYPE
      M.PO       = POSLN.PO
      M.LINENO   = POSLN.LINENO
      STORE 0 TO M.QTY1,M.QTY2,M.QTY3,M.QTY4,M.QTY5,M.QTY6,M.QTY7,M.QTY8
      SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = M.STYLE+M.CSTYTYPE+M.PO+STR(M.LINENO,6)
        
        DO CASE
          CASE TRANCD = '1'
            SCATTER FIELDS EXCEPT QTY* MEMVAR    
            M.QTY1 = POSLN.QTY1
            M.QTY2 = POSLN.QTY2
            M.QTY3 = POSLN.QTY3
            M.QTY4 = POSLN.QTY4
            M.QTY5 = POSLN.QTY5
            M.QTY6 = POSLN.QTY6
            M.QTY7 = POSLN.QTY7
            M.QTY8 = POSLN.QTY8

          CASE TRANCD $ '2|5'
            M.QTY1 = MAX( M.QTY1 - POSLN.QTY1 , 0 )
            M.QTY2 = MAX( M.QTY2 - POSLN.QTY2 , 0 )
            M.QTY3 = MAX( M.QTY3 - POSLN.QTY3 , 0 )
            M.QTY4 = MAX( M.QTY4 - POSLN.QTY4 , 0 )
            M.QTY5 = MAX( M.QTY5 - POSLN.QTY5 , 0 )
            M.QTY6 = MAX( M.QTY6 - POSLN.QTY6 , 0 )
            M.QTY7 = MAX( M.QTY7 - POSLN.QTY7 , 0 )
            M.QTY8 = MAX( M.QTY8 - POSLN.QTY8 , 0 )

        ENDCASE
      ENDSCAN
      
      M.TOTQTY = M.QTY1+M.QTY2++M.QTY3+M.QTY4+M.QTY5+M.QTY6+M.QTY7+M.QTY8
      
      *- SKip none open PO's
      *C037439,3  TMI [Start] Also skip return Po's and interlocation Po's
      *IF SEEK(M.CSTYTYPE+M.PO,'POSHDR') AND POSHDR.STATUS = 'O' .AND. M.TOTQTY > 0
      IF SEEK(M.CSTYTYPE+M.PO,'POSHDR') AND POSHDR.STATUS = 'O' .AND. POSHDR.CSTYTYPE = 'P' .AND. M.TOTQTY > 0
        *C037439,3  TMI [End  ] 

        INSERT INTO (lcTmpPoLn) FROM MEMVAR
  
        SELECT (lcTmpPoLn)
        REPLACE TRANCD    WITH '1' ;
                SHOWPO    WITH M.PO ;
                LNDESC    WITH 'O/S  Bal' ;
                AVAILABLE WITH POSHDR.AVAILABLE ;
                TOTQTY    WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
                TOTBAL    WITH TOTQTY ;
                SHOBAL    WITH TOTQTY
        lcTmpLine = lcTmpPoLn
        =lfGetInfo()
        *C037439,4  TMI [Start] Clear dylot field
        REPLACE DYELOT    WITH ''
        *C037439,4  TMI [End  ]         

        STORE 0 TO M.QTY1,M.QTY2,M.QTY3,M.QTY4,M.QTY5,M.QTY6,M.QTY7,M.QTY8,M.TOTQTY

        INSERT INTO (lcTmpPoLn) FROM MEMVAR            
        REPLACE TRANCD    WITH '2' ;
                LNDESC    WITH 'Receive' 

        INSERT INTO (lcTmpPoLn) FROM MEMVAR            
        REPLACE TRANCD    WITH '5' ;
                LNDESC    WITH 'Cancel'
        *C037439,4  TMI [Start] Clear dylot field
        REPLACE DYELOT    WITH ''
        *C037439,4  TMI [End  ]         
      
      ENDIF
      
    ENDDO
    

  ENDIF

  IF RECCOUNT(lcTmpPoLn) > 0
    SHOW GET pbClear ENABLE
    SHOW GET pbSave  ENABLE

    SELECT (lcTmpPoLn)
    SET ORDER TO TAG POSLN
    GO TOP
 
    KEYBOARD '{ALT+B}'   
    =lfRefresh('POSTRCV0')

    SET ORDER TO &lcSvOrder IN POSLN    
    
  ELSE
  
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"There is no open PO's for the style '"+M.STYLE+"' or it is received completly from all its open PO's.")  
    STORE SPACE(19) TO M.STYLE,lcLastSty
    SHOW GET M.STYLE ENABLE
    SHOW GET ibSTYLE ENABLE
    _CUROBJ = OBJNUM(M.STYLE)
    =SEEK(CHR(225),'SCALE')
    
  ENDIF
  
  lcTmpLine = gfTempName()

ENDIF
*-- end of lfvStyle.


*:**************************************************************************
*:* Name        : lfvClear
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/17/2004
*:* Purpose     : Clear recieved qtys for the selected style
*:***************************************************************************
FUNCTION lfvClear
PARAMETERS llAsk
IF llAsk
  lnResp = gfModalGen('INM00000B00006',.F.,.F.,.F.,;
            'Are you sure you want to clear the screen?')
  IF lnResp <> 1
    RETURN
  ENDIF          
ENDIF  

PRIVATE lnResp
lcBrowFlds = ''
SELECT (lcTmpPoLn)
ZAP

=SEEK(CHR(225),'SCALE')
=lfBrowsLn()

SHOW GET pbClear DISABLE
SHOW GET pbSave  DISABLE
SHOW GET M.STYLE ENABLE
SHOW GET ibStyle ENABLE

STORE SPACE(19) TO M.STYLE,lcLastSty,M.DESC
SHOW GET M.DESC

_CUROBJ = OBJNUM(M.STYLE)
=lfRefresh('POSTRCV0')

*-- end of lfvClear.

*:**************************************************************************
*:* Name        : lfvSave
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/17/2004
*:* Purpose     : Save recieved qtys for the selected style
*:***************************************************************************
FUNCTION lfvSave

*-Check first that there is at least one field is keyed
lcDelStat = SET('DELETED')
SET DELETED OFF

lnTot = 0
SELECT (lcTmpPoLn)
lnRecno = RECNO()

LOCATE 
SCAN FOR TRANCD $ '2|5'
  IF TOTQTY > 0
    lnTot = lnTot + TOTQTY
  ENDIF
ENDSCAN
SET DELETED &lcDelStat

IF lnTot = 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No Receiving lines was done.Cannot update.')
  GOTO (lnRecno)
  RETURN
ENDIF

lnResp = gfModalGen('INM00000B00006',.F.,.F.,.F.,;
          'Are you sure you want to Save,the system  will update all the received and cancelled quantities input?')
IF lnResp <> 1
  GOTO (lnRecno)
  RETURN
ENDIF  

*******1
********2

IF llMFCall
  lcMastFile = 'CUTTKTL'
ELSE
  lcMastFile = 'POSLN'
ENDIF  
SELECT (lcMastFile)

  lcTempFile = gfTempName()
  DIMENSION laFileStru[1,4]
  
  =AFIELDS(laFileStru)
  DIMENSION laFileStru[ALEN(laFileStru,1) , 4] 
  CREATE TABLE &gcWorkDir.&lcTempFile FROM ARRAY laFileStru
  IF llMFCall
    INDEX ON cuttkt+style+dyelot+trancd TAG (lcTempFile)
  ELSE
    INDEX ON cstytype+po+style+STR(lineno,6)+trancd TAG (lcTempFile)
  ENDIF


*-Remove lines with totqty = 0 and copy data to a new temp file 
IF USED(lcTmpLine)
  USE IN (lcTmpLine)
ENDIF  
=lfCrtUnComp()
SELECT (lcTmpPoLn)
LOCATE
SCAN
  IF TRANCD = '1'
  
    IF TOTQTY <> TOTBAL
      SCATTER MEMVAR 
      INSERT INTO (gcWorkDir+lcTmpLine) FROM MEMVAR
    ENDIF
    
  ELSE
  
    IF TOTQTY > 0
      SCATTER MEMVAR 
      INSERT INTO (gcWorkDir+lcTmpLine) FROM MEMVAR
    ENDIF
  
  ENDIF
ENDSCA

SELECT (lcTmpLine)
SET ORDER TO TMPLINE1
LOCATE

lcProc = SET('PROC')
SET PROCEDURE TO (gcapphome+'POUPDATE.PRG') ADDITIVE
IF gfSaveRec()
  *- Clear data in files
  =lfvClear()
ENDIF  
SET PROCEDURE TO &lcProc

SELECT (lcTmpPoLn)
IF BETWEEN(lnRecno,1,RECCOUNT(lcTmpPoLn))
  GOTO (lnRecno)
ENDIF

*-- end of lfvSave.

*:**************************************************************************
*:* Name        : lfvQuit
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/17/2004
*:* Purpose     : Close the screen
*:***************************************************************************
FUNCTION lfvQuit
IF RECCOUNT(lcTmpPoLn) > 0
  lnResp = gfModalGen('INM00000B00006',.F.,.F.,.F.,;
          'Are you sure you want to close the screen and lose all updates?')
  IF lnResp <> 1
    RETURN
  ENDIF
ENDIF

*-Remove temp file
IF USED(lcTmpPoLn)
  USE IN (lcTmpPoLn)
ENDIF
ERASE (gcWorkDir+lcTmpPoLn+'.DBF')
ERASE (gcWorkDir+lcTmpPoLn+'.CDX')

*-Close files
=lfClose('STYLE')
=lfClose('POSHDR')
=lfClose('POSLN')
=lfClose('SCALE')
=lfClose('SYDSUPRG')
=lfClose('SYUUSER')
=lfClose('STYDYE')
=lfClose('UNCMSESS')
=lfClose('F')
=lfClose('STYLE')
=lfClose('SCALE')
=lfClose('WAREHOUS')
=lfClose('APVENDOR')
=lfClose('SYCCOMP')
=lfClose('POSHDR')
=lfClose('POSLN')
=lfClose('FABRIC')
=lfClose('FABDYE')
=lfClose('BOMLINE')
=lfClose('MFGOPRDT')
=lfClose('CUTPICK')
=lfClose('WHSLOC')
=lfClose('FISHD')
=lfClose('FSPRD')
=lfClose('SYDFILES')
=lfClose('CTKTBOM')
=lfClose('BOMCOST')
=lfClose('BOM')
=lfClose('CODES')
=lfClose('MATINVJL')

CLEAR READ

*-- end of lfvQuit.

*:**************************************************************************
*:* Name        : lfClose
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 24/02/2005
*:* Purpose     : Close a file
*:***************************************************************************
FUNCTION lfClose
PARAMETERS lcFile
IF USED(lcFile)
  USE IN &lcFile
ENDIF

*-- end of lfClose.
*:**************************************************************************
*:* Name        : lfBrowsLn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/17/2004
*:* Purpose     : Browse details
*:***************************************************************************
*:* Called from : postrcv.spx
*:***************************************************************************
FUNCTION lfBrowsLn
SELECT (lcTmpPoLn)
lnMrk  = RECNO(lcTmpPoLn)

lcBrowFlds = "lcMarker=IIF(lnMrk=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             "SHOWPO :R :H=' Po no  ' :9 ,"+;
             "LNDESC :R :H='        ' :9  "+;             
                                     ", QTY1 :H='"+SCALE.Sz1+"' :6 :W=lfwFld(1) :V=lfvFld(1)"       +;
             IIF(!EMPTY(SCALE.Sz2) , ", QTY2 :H='"+SCALE.Sz2+"' :6 :W=lfwFld(2) :V=lfvFld(2)" , "" )+;
             IIF(!EMPTY(SCALE.Sz3) , ", QTY3 :H='"+SCALE.Sz3+"' :6 :W=lfwFld(3) :V=lfvFld(3)" , "" )+;
             IIF(!EMPTY(SCALE.Sz4) , ", QTY4 :H='"+SCALE.Sz4+"' :6 :W=lfwFld(4) :V=lfvFld(4)" , "" )+;
             IIF(!EMPTY(SCALE.Sz5) , ", QTY5 :H='"+SCALE.Sz5+"' :6 :W=lfwFld(5) :V=lfvFld(5)" , "" )+;
             IIF(!EMPTY(SCALE.Sz6) , ", QTY6 :H='"+SCALE.Sz6+"' :6 :W=lfwFld(6) :V=lfvFld(6)" , "" )+;
             IIF(!EMPTY(SCALE.Sz7) , ", QTY7 :H='"+SCALE.Sz7+"' :6 :W=lfwFld(7) :V=lfvFld(7)" , "" )+;
             IIF(!EMPTY(SCALE.Sz8) , ", QTY8 :H='"+SCALE.Sz8+"' :6 :W=lfwFld(8) :V=lfvFld(8)" , "" )+;
             IIF(llDyelot .AND. STYLE.CDYE_FLG='Y' , ",DYELOT  :H='Dyelot'  :12 :W=lfwDye()" , "" )+;
             ",TOTQTY    :R :H='Total' :W=.F."+;
             ",SHOBAL    :R :H='Bal. ' :W=.F."+;
             ",AVAILABLE :R :H='Available' :10 :W=.F."
          
BROWSE FIELDS &lcBrowFlds;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwBrowUp();
       TITLE lcBrowTitl ;
       WINDOW "POSTRCV1" IN WINDOW "POSTRCV"

ACTIVATE WINDOW (lcBrowTitl)
SHOW WINDOW (lcBrowTitl) REFRESH
=lfRefresh(lcBrowTitl)

*-- end of lfBrowsLn.

*:**************************************************************************
*:* Name        : lfwBrowUp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/24/2004
*:* Purpose     : When browse funtion
*:***************************************************************************
FUNCTION lfwBrowUp
lnMrk = RECNO(lcTmpPoLn)
SHOW WINDOW (lcBrowTitl) REFRESH

*-- end of lfwBrowUp.

*:**************************************************************************
*:* Name        : lfvqBrow
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/24/2004
*:* Purpose     : Valid browse function
*:***************************************************************************
FUNCTION lfvqBrow

*-- end of lfvqBrow.

*:**************************************************************************
*:* Name        : lfwFld
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/26/2004
*:* Purpose     : When function for the browse fields
*:***************************************************************************
FUNCTION lfwFld
PARAMETERS lnFld
PRIVATE lcFld,llRet
lcFld = STR(lnFld,1)
lnOldValue = QTY&lcFld

RETURN ( TRANCD <> '1' )

*-- end of lfwFld.

*:**************************************************************************
*:* Name        : lfwFld
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/11/2004
*:* Purpose     : When function for the browse field "DYELOT"
*:***************************************************************************
FUNCTION lfwDye
lnOldValue = DYELOT
RETURN ( TRANCD = '2' )

*-- end of lfwDye.
*:**************************************************************************
*:* Name        : lfvFld
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/24/2004
*:* Purpose     : Valid function for browse fields
*:***************************************************************************
FUNCTION lfvFld
PARAMETERS lnFldno
PRIVATE lcFldno,lcFld
lcFldno = STR(lnFldno,1)

IF QTY&lcFldno < 0
  WAIT WINDOW NOWAIT 'Negative values are not allowed!'
  REPLACE QTY&lcFldno WITH 0
  RETURN
ENDIF

IF QTY&lcFldno <> lnOldValue

  lnRecno = RECNO(lcTmpPoLn)

  lcKey = CSTYTYPE+PO+STYLE+STR(LINENO,6)
  =SEEK(lcKey+'1',lcTmpPoLn)
  lnOpenQty = QTY&lcFldno

  =SEEK(lcKey+'2',lcTmpPoLn)
  lnRecvQty = QTY&lcFldno

  =SEEK(lcKey+'5',lcTmpPoLn)
  lnCnclQty = QTY&lcFldno

  GOTO (lnRecno)
  DO CASE
    CASE TRANCD = '2'    && Receiving
      REPLACE TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
      lnTotSTK = TOTQTY
      =SEEK(lcKey+'1',lcTmpPoLn)
      REPLACE TOTSTK WITH lnTotSTK

    CASE TRANCD = '5'    && Cancelling
      IF lnRecvQty >= lnOpenQty
        WAIT WINDOW NOWAIT 'No cancel qty allowed'
        REPLACE QTY&lcFldno WITH 0
      ELSE
        IF QTY&lcFldno > lnOpenQty-lnRecvQty
          WAIT WINDOW NOWAIT 'RANGE: 0 to'+STR(lnOpenQty-lnRecvQty)
          REPLACE QTY&lcFldno WITH lnOpenQty-lnRecvQty
        ENDIF
      ENDIF    
      REPLACE TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
      lnCnclQty = TOTQTY
  ENDCASE

  GOTO (lnRecno)

  IF TRANCD = '2'
    lnRcv = QTY&lcFldno
    IF lnCnclQty > 0 .AND. ;
       lnCnclQty > lnOpenQty - lnRcv
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,"The canceled quantity is more than the new balance. It will be reduced.")
      =SEEK(lcKey+'5',lcTmpPoLn)
      REPLACE QTY&lcFldno WITH MAX(lnOpenQty - lnRcv,0)      
      REPLACE TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
      lnCnclQty = TOTQTY
    ENDIF
  ENDIF

  =SEEK(lcKey+'1',lcTmpPoLn)
  REPLACE TOTCAN WITH lnCnclQty 
  lnTotqty = TOTQTY
  =SEEK(lcKey+'2',lcTmpPoLn)

  *-Get balance
  SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8 TO laRcv
  =SEEK(lcKey+'5',lcTmpPoLn)
  SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8 TO laCan
  =SEEK(lcKey+'1',lcTmpPoLn)
  REPLACE TOTBAL WITH MAX(QTY1-(laRcv[1]+laCan[1]),0)+;
                      MAX(QTY2-(laRcv[2]+laCan[2]),0)+;
                      MAX(QTY3-(laRcv[3]+laCan[3]),0)+;
                      MAX(QTY4-(laRcv[4]+laCan[4]),0)+;
                      MAX(QTY5-(laRcv[5]+laCan[5]),0)+;
                      MAX(QTY6-(laRcv[6]+laCan[6]),0)+;
                      MAX(QTY7-(laRcv[7]+laCan[7]),0)+;
                      MAX(QTY8-(laRcv[8]+laCan[8]),0)
  REPLACE SHOBAL WITH TOTBAL

  GOTO (lnRecno)
  REPLACE TOTBAL WITH lnTotqty


  
ENDIF
*-- end of lfvFld.

*:**************************************************************************
*:* Name        : lfGetClrD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/24/2004
*:* Purpose     : Get color information
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetClrD()
*:***************************************************************************
FUNCTION lfGetClrD
DECLARE laItemSeg[1]
PRIVATE lnCount &&Tmi 07/15/2002
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*:**************************************************************************
*:* Name        : lfCrtTmpFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/24/2004
*:* Purpose     : Create temp file to collect data.
*:***************************************************************************
FUNCTION lfCrtTmpFl

lcTmpPoLn = gfTempName()

lnFileStru = 0
SELECT POSLN
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)

lnFileStru = lnFileStru + 1           && this field is used only to show the PO# in the browse
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'SHOWPO'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1          && Show a description on the browse for the curent line
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'LNDESC'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'AVAILABLE'
laFileStru[lnFileStru,2] = 'D'
laFileStru[lnFileStru,3] = 8
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'TOTSTK'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 6       
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'TOTDAM'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 6       
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'TOTCAN'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 6       
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'TOTBAL'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 6       
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'SHOBAL'  && Used to show balance field only
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 6       
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'CCARTON'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 3       
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'NLINENO'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 6       
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'LNEWLN'
laFileStru[lnFileStru,2] = 'L'
laFileStru[lnFileStru,3] = 1       
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'NSTEPS'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 2       
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'LALOCHG'
laFileStru[lnFileStru,2] = 'L'
laFileStru[lnFileStru,3] = 1       
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'CLASTOPR'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6       
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'LNEWLUPD'
laFileStru[lnFileStru,2] = 'L'
laFileStru[lnFileStru,3] = 1       
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'LCOSTMADE'
laFileStru[lnFileStru,2] = 'L'
laFileStru[lnFileStru,3] = 1       
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'LAUTOMODE'
laFileStru[lnFileStru,2] = 'L'
laFileStru[lnFileStru,3] = 1       
laFileStru[lnFileStru,4] = 0

CREATE TABLE (gcWorkDir+lcTmpPoLn) FROM ARRAY laFileStru
INDEX ON TRANCD+CCARTON+PO+STYLE+DYELOT+CWARECODE+STR(LINENO,6) TAG TMPLINE1
INDEX ON CCARTON+PO+STYLE+DYELOT+CWARECODE+STR(LINENO,6)+TRANCD TAG TMPLINE2
INDEX ON PO+STYLE+DYELOT+CWARECODE+STR(LINENO,6)+CCARTON+TRANCD TAG TMPLINE3
INDEX ON SHIPNO+CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD TAG POSLNSH
INDEX ON CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD TAG POSLN

*-- end of lfCrtTmpFl.

*:**************************************************************************
*:* Name        : lfOpenFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/24/2004
*:* Purpose     : *-Open needed files and set relations
*:***************************************************************************
FUNCTION lfOpenFls

*-Open needed files 
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
=gfOpenFile(gcDataDir+'POSLN','POSLNS','SH')
=gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
=gfOpenFile(gcSysHome+'SYDSUPRG','PRGCOMP','SH')
=gfOpenFile(gcSysHome+'SYUUSER','CUSER_ID','SH')
=gfOpenFile(gcSysHome+'SYUSTATC','CUSER_ID','SH')
=gfOpenFile(gcDataDir+'STYDYE','STYDYE','SH')
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
=gfOpenFile(gcDataDir+'WAREHOUS','WAREHOUS','SH')
=gfOpenFile(gcDataDir+'APVENDOR','VENCODE','SH')
=gfOpenFile(gcSysHome+'SYCCOMP','CCOMP_ID','SH')
=gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
=gfOpenFile(gcDataDir+'POSLN','POSLN','SH')
=gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')
=gfOpenFile(gcDataDir+'FABDYE','FABDYE','SH')
=gfOpenFile(gcDataDir+'BOMLINE','BOMLINE','SH')
=gfOpenFile(gcDataDir+'MFGOPRDT','MFGOPRDT','SH')
=gfOpenFile(gcDataDir+'CUTPICK','CUTPICK','SH')
=gfOpenFile(gcDataDir+'WHSLOC','WHSLOCST','SH')
=gfOpenFile(gcDataDir+'FISHD','COMPFYEAR','SH')
=gfOpenFile(gcDataDir+'FSPRD','COMFYRPRDI','SH')
=gfOpenFile(gcSysHome+'SYDFILES','CFILE_NAM','SH')
=gfOpenFile(gcDataDir+'CTKTBOM','CTKTBOM','SH')
=gfOpenFile(gcDataDir+'BOMCOST','POBOMCLS','SH')
=gfOpenFile(gcDataDir+'BOM','BOM','SH')
=gfOpenFile(gcDataDir+'CODES','CODES','SH')
=gfOpenFile(gcDataDir+'MATINVJL','MATINVJL','SH')

IF !USED('UNCMSESS')
  =gfOpenFile(gcDataDir+'UNCMSESS','TRANS','SH')
ENDIF  
IF !USED('F')
  =gfOpenFile(gcDataDir+'UNCMSESS','TRANS','SH','F',.T.)
ENDIF  

*-Locate scale to eof , to show no scale in the first browse
=SEEK(CHR(255),'SCALE')

*-SET RELATION
SELECT POSLN
SET RELATION TO CSTYTYPE+PO INTO POSHDR
LOCATE

*-- end of lfOpenFls.

*:**************************************************************************
*:* Name        : lpTab
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/25/2004
*:* Purpose     : Tab function key
*:***************************************************************************
PROCEDURE lpTab

IF WONTOP() = lcBrowTitl
  ACTIVATE WINDOW (lcBrowTitl)
  _CUROBJ=OBJNUM(pbSav)  
ELSE
  _CUROBJ=_CUROBJ+1
ENDIF

*!*************************************************************
*! Name      : lfDeactiv
*! Developer : Khalid Mhoi El-Din
*! Date      : 12/27/1998
*! Purpose   : To validate the deactivation of the screen.
*!*************************************************************
*! Example            :  lfDeactiv()
*!*************************************************************
*C101394,1 KHM 12/27/98 Added
*!*************************************************************
FUNCTION lfDeactiv

IF WONTOP() = lcBrowTitl
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  *ON KEY LABEL ESC DO lpTrapKey WITH 'NAP500C', 'PbPrnClose', .T.
  ON KEY LABEL ESC DO lfvQuit
  ON KEY LABEL CTRL+S DO lfvSave
  ON KEY LABEL CTRL+C DO lfvClear WITH .T.
  *ON KEY LABEL TAB DO lpTrapKey WITH 'POSTRCV1', 'pbSelect'
  *ON KEY LABEL BACKTAB DO lpTrapKey WITH 'NAP500C', 'PbPrnClose'
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfReadAct
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : The READ Activate function of NAP800
*!*************************************************************
*! Example            :  lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

ON KEY LABEL CTRL+Q    
ON KEY LABEL CTRL+W    
ON KEY LABEL CTRL+S
ON KEY LABEL CTRL+C
ON KEY LABEL CTRL+HOME 
ON KEY LABEL CTRL+END  
ON KEY LABEL ESC DO lfvQuit
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB 


*!*************************************************************
*! Name      : lpTrapKey
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To handle the Trapping of keys
*!*************************************************************
*! Example            :  lpTrapKey()
*!*************************************************************
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName, llToCheck

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF


*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************

* These functions are copied from postrec.prg file to make the posting of the 
* temp file of the received and canceled qty's


*:*************************************************************
*! Name     : lfChekAdj
*! Developer: Ahmed Salah Shalaby - (SSH)
*! Date     : 10/10/97               
*! Purpose  : Check if adjust cost for recieve.. 
*:*************************************************************

FUNCTION lfChekAdj
PARAMETERS lcTrType,lcTket,lcItem,lcColor,lcSess,lcStyQlt
*B802902,1 SSH 03/05/2000 if we select to preview log report, 
*B802902,1 SSH            the reports show the estimated p.price along 
*B802902,1 SSH            (with each style/color being received) 
*B802902,1 SSH            instead of the landed p.price. 
PRIVATE lnOldAls , llToREt
lnOldAls = SELECT(0)
SELECT BOMLINE
lnoldKey = ORDER()
SET ORDER TO TAG BomLine
lcItem=PADR(lcItem,19)

lcSeekKey = IIF(lcTrType='S','I',lcTrType)+'2'+lcTket
lcWhleCnd = "cIMTyp+cType+cTktNo+STR(LineNo,6)=lcSeekKey"

lcForCond = "Style=lcItem AND EMPTY(cRSession)"
lcQltFltr = IIF(TYPE('lcStyQlt') $ 'UL' ,".T.","cStyGrade = lcStyQlt")

=SEEK(lcSeekKey)
LOCATE REST WHILE &lcWhleCnd FOR &lcForCond AND &lcQltFltr

*--If found type '2' Return True.
SELECT BOMLINE
SET ORDER TO lnoldKey
llToREt = FOUND()
SELECT(lnOldAls)
RETURN llToREt


*:*************************************************************
*! Name    : lfActBrow
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Activate screen browse.
*:*************************************************************
FUNCTION lfActBrow


*:*************************************************************
*! Name      : lfCrUnSess
*! Developer : Timour A. K. 
*! Date      : 10/10/97               
*! Purpose   : Create/Update uncomplete session record.
*:*************************************************************
*! Call      : lfUpdVars()
*:*************************************************************
FUNCTION lfCrUnSess
PARA lcPushBt,lcUnSStat,llVarUpd

lnAlias = SELECT()
SELECT UNCMSESS
IF !SEEK('O'+lcUnCmPgId+PADR(gcUser_id,10)+lcSession)
  IF EMPTY(lcUnSStat) AND !llVarUpd
    *MAN Added OR !RLOCK()
    IF !SEEK('I'+lcUnCmPgId+PADR(gcUser_id,10)) OR !RLOCK()
      INSERT INTO UNCMSESS (Status,cUTranType,cUserId,cSession,cProgram,;
        cCurrScr,dTranDate,cTranTime,ccurrobj) VALUES ;
        ('O',lcUnCmPgId,gcUser_id,lcSession,lcAProgm,'',gdSysDate,TIME(),' ')
    ELSE
      REPLACE Status    WITH 'O',;
              cSession  WITH lcSession,;
              dTranDate WITH gdSysDate,;
              cTranTime WITH TIME(),;
              ccurrobj  WITH lcPushBt
    ENDIF
    =lfUpdVars()
    =RLOCK('UNCMSESS')
  ENDIF
ELSE
  *--Update uncomplete session.
  SELECT UNCMSESS
  IF llVarUpd
    =lfUpdVars()
  ELSE
    IF EMPTY(lcUnSStat)
      REPLACE cCurrObj WITH lcPushBt
      =lfUpdVars()
    ELSE
      *B603026,1 Comment the following lines so as not to create a new session
      *B603026,1 number
      *IF lcUnSStat='C'
      *ENDIF
      *B603026,1 end
      REPLACE Status WITH lcUnSStat
      UNLOCK
      llContinue = .F.
    ENDIF
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN


*:*************************************************************
*! Name    : lfAddNewLn
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Preparelines for Post.
*!           If new line add to P/o and update bom.
*:*************************************************************
FUNCTION lfAddNewLn

*--Update For new added P/o Lines.....
SELECT (lcTmpLine)
SCAN FOR TranCd='1'
  IF ! lNewLn
    LOOP
  ENDIF
        
  SCATTER MEMVAR
  *C200080,1 AMM Add the dye order type 'D'
  *=SEEK(IIF(lcPType='R','R','P')+&lcTmpLine..Po,'POSHDR')  
  =SEEK(IIF(lcPType$'RD',lcPType,'P')+&lcTmpLine..Po,'POSHDR')  
  *C200080,1 AMM end

  IF (llImpCost AND lcPType<>'R') AND !lfUpdBom('A')
    SELECT (lcTmpLine)
    LOOP
  ENDIF

  SELECT POSLN
  APPEND BLANK
  GATHER MEMVAR
  REPLACE cOwner WITH ' '
  =gfAdd_Info('POSLN')

  *--Call TraceKey global function.
  *=gfTraceKey('POSLN',POSLN.cstytype+POSLN.po+POSLN.style+STR(POSLN.lineno,6)+POSLN.trancd,'A')
  SELECT POSLN
  =gfTraceKey('POSLN',cStyType+Po+cRsession+Shipno+Style+STR(Lineno,6)+Trancd,'A')

  SELECT POSHDR
  =RLOCK()

  *E301077,11 Update Odded fields using gfAdd_Info Function [Begin]
  *Odded REPLACE LastLine   WITH POSLN.LineNo ,;
  *Odded         Mod_UserId WITH gcUser_id,;
  *Odded         Mod_Time   WITH TIME(),;
  *Odded         Mod_Date   WITH gdSysDate,;
  *Odded         Mod_Action WITH 'MODIFIED'

  REPLACE LastLine   WITH POSLN.LineNo
  =gfAdd_Info('POSHDR')
  *E301077,11 Update Odded fields using gfAdd_Info Function [End  ]

  UNLOCK
 
  SELECT (lcTmpLine)
  REPLACE lNewLn WITH .F.

  *--Call TraceKey global function.
  =gfTraceKey('POSHDR',POSHDR.cStyType+POSHDR.Po,'M')

ENDSCAN
RETURN

*:*************************************************************
*! Name    : lfGetOpen
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Get original open quantity on P/o and return
*!           the original warehouse.
*:*************************************************************
FUNCTION lfGetOpen
*E300935,4 Add new logical parameter if .T. skip subtract code.
PARA lcParmKey,llSubtNo
IF EMPTY(lcDyelot)
  lcParmKey = ALLTRIM(lcParmKey)
ENDIF

lnAlias = SELECT()
SELECT (lcMasterLn)
*B802566,1 Start, Save the record pointer
PRIVATE lnMsLnRNo 
lnMsLnRNo = RECNO()
*B802566,1 End
SEEK lcParmKey

lcOrjWareH = cWareCode
laOpnQty   = 0
lcWhlCndn  = IIF(llMFCall,"Cuttkt+Style+Dyelot","cStyType+PO+Style+STR(LineNo,6)")+"=lcParmKey"
*E301291,4 AMM Consider receive adornment order case
*IF lcPType <> 'O'

*E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
*IF !(lcPType $ 'OE')
IF !(lcPType $ 'OEL')
*E301480,1 NAD (End)

*E301291,4 AMM end
*B602698,1[BEGIN] Do it in case of C/T
  lcForCndn = IIF(llMFCall,".T.","TranCd <> '3'")
*  lcForCndn = "TranCd <> '3'"
*B602698,1[END..]  
  lcBaseTrCd = '1'
ELSE
  lcForCndn = "TranCd <> '1'"
  lcBaseTrCd = '6'
ENDIF

SCAN REST WHILE &lcWhlCndn FOR &lcForCndn
  FOR I=1 TO 8
    lcCnt=STR(I,1)
    *E300935,4 adjust get open quantity (P/O Batch case)
    *laOpnQty[I]=IIF(TranCd=lcBaseTrCd,laOpnQty[I]+Qty&lcCnt,MAX(laOpnQty[I]-Qty&lcCnt,0))
    *C200080,1 AMM  Add the dye order type 'D'
    *laOpnQty[I]= IIF(TranCd=lcBaseTrCd,laOpnQty[I]+Qty&lcCnt,;
                 IIF(lcPType $ 'IBS' AND !EMPTY(DYELOT),laOpnQty[I]-Qty&lcCnt,;
                 MAX(laOpnQty[I]-Qty&lcCnt,0))) 

    *B604502,1 MHM 06/11/2001 (Begin) Tacking care of inter-location PO
    *laOpnQty[I]= IIF(TranCd=lcBaseTrCd,laOpnQty[I]+Qty&lcCnt,;
                 IIF(lcPType $ 'IBSD' AND !EMPTY(DYELOT),laOpnQty[I]-Qty&lcCnt,;
                 MAX(laOpnQty[I]-Qty&lcCnt,0))) 
    laOpnQty[I]= IIF(TranCd=lcBaseTrCd,laOpnQty[I]+Qty&lcCnt,;
                 IIF(lcPType $ 'IBSD' AND !EMPTY(DYELOT),laOpnQty[I]-Qty&lcCnt,;
                 IIF(lcPType ='O',laOpnQty[I]-Qty&lcCnt,MAX(laOpnQty[I]-Qty&lcCnt,0)))) 
    *B604502,1 MHM 06/11/2001 (End)
    *C200080,1 AMM end
  ENDFOR
ENDSCAN

*B604502,1 MHM 06/11/2001 (Begin) Taking care of inter-location PO
IF lcPType = 'O'
  FOR lnCntr = 1 TO 8
    laOpnQty[lnCntr] = MAX(laOpnQty[lnCntr],0)
  ENDFOR
ENDIF
*B604502,1 MHM 06/11/2001 (End)

*E300935,4 Evaluate remain Open Quantity if there is another lines.[Begin]
*E300935,4 But this in case of system support dyelots and Imported styles or another warehouse.
*IF !llSubtNo AND !EMPTY(Dyelot) AND (lcPType $ 'IBS')
*B602698,1[BEGIN] Do it in case of C/T
*IF !llSubtNo AND (lcPType $ 'IBS')
*C200080,1 AMM Add the dye order type 'D'
*IF !llSubtNo AND (lcPType $ 'IBSM')
IF !llSubtNo AND (lcPType $ 'IBSMD')
*C200080,1 AMM end
*B602698,1[END..]
  SEEK lcParmKey
  SELECT (lcTmpLine)
  *B802566,1 Check on dyelot in case of c/t also.
  lcCurDyelot = IIF(lcPType $ 'BM',Dyelot,lcDyelot)
  lcCurWare   = IIF(lcPType $ 'BM',cWareCode,lcWareCode)
  lcGoAgain   = Trancd+cCarton+IIF(llMFCall,Cuttkt,Po)+Style+Dyelot+PADR(cWareCode,6)+STR(LineNo,6)
  *'TranCd+cCarton+Cuttkt+Style+Dyelot+cWareCode+STR(LineNo,6)'
  lcScanExpr  = IIF(lcPType = 'B',cCarton+IIF(llMFCall,Cuttkt,Po)+Style+STR(LineNo,6),;
                    '   '+IIF(llMFCall,Cuttktl.Cuttkt,POSLN.PO)+lcStyle+STR(IIF(llMFCall,Cuttktl.LINENO,POSLN.LINENO),6))
  *lcForExpr = IIF(lcPType = 'B',"(Dyelot+cWareCode # lcCurDyelot+lcCurWare) AND (Trancd # '1')",;
                                "Trancd # '1'")
  lcForExpr = IIF(lcPType $ 'BM',"(Dyelot+cWareCode # lcCurDyelot+lcCurWare) AND (Trancd # '1')",;
                                "Trancd # '1'")
  *B802566,1 End.
  
  *B602698,1[BEGIN]
  *B603275,1 AMM Adjust the scan condition
  *lcScanCond=cCarton+IIF(llMFCall,Cuttkt,Po)+Style+STR(LineNo,6)
  lcScanCond  = "cCarton"+IIF(llMFCall,"Cuttkt","Po")+"Style"+"STR(LineNo,6)"
  *B603275,1 AMM end

  *B602698,1[END..]
  GO TOP
  IF !EOF()
    SCAN REST WHILE  lcScanCond = lcScanExpr;
         FOR &lcForExpr
      FOR I=1 TO 8
        Z=STR(I,1)
        *E300935,4 Now MAX Function is on the screen.
        *laOpnQty[I] = MAX(laOpnQty[I]-Qty&Z,0)
        laOpnQty[I] = laOpnQty[I]-Qty&Z
      ENDFOR 
    ENDSCAN
    = SEEK(lcGoAgain)
  ENDIF  
ENDIF  
*E300935,4 Evaluate remain Open Quantity if there is another lines.[End]
*B802566,1 MAN Start, Restore the record pointer for the master line file
SELECT (lcMasterLn)
IF BETWEEN(lnMsLnRNo,1,RECCOUNT(lcMasterLn))
 GO lnMsLnRNo 
ENDIF
*B802566,1 End

SELECT(lnAlias)
RETURN (lcOrjWareH)

*:*************************************************************
*! Name     : lfGetLanded
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose  : Calculate landed costs case of detail costing. 
*:*************************************************************
FUNCTION lfGetLanded

*B803337,1 WMA [Begin]
*E301077,11 MAB Open BOMLINE file here [Begin
*IF !llOpnBomLn
*  llOpnBomLn = lfOpn_Rest(gcDatadir,'BOMLINE','BOMLINE')
*ENDIF  
*E301077,11 MAB Open BOMLINE file here [End..
*B803337,1 WMA [End]

SELECT BOMLINE
IF lcPType ='S'
  SET ORDER TO TAG BomLnShp
ELSE
  SET ORDER TO TAG BomLine
ENDIF

*B602966,1 Start.
SELECT (lcTmpLine)
SCATTER FIELDS Qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8 TO laLnQty

FOR lnCnt=1 to 5
  lcCnt=STR(lnCnt,1)
  IF llMFCall
    lcBomLKey = 'M2'+&lcTmpLine..Cuttkt
    lcWCondtn = "cImTyp+cType+cTktNo = lcBomLKey"
    lcFCondtn = "cBomTyp=lcCnt AND Style=&lcTmpLine..Style AND (EMPTY(cRSession) OR cRSession=lcGlSession)"

    *B605612,1 KHM 02/28/2002 (Begin) Adding the checing of the style grade.
    lcFCondtn = lcFCondtn + " .AND. cStyGrade = &lcTmpLine..cStyGrade "
    *B605612,1 KHM 02/28/2002 (End)

  ELSE
    *khm1
    *lcBomLKey = 'I2'+IIF(lcPType='S',&lcTmpLine..Shipno,'')+&lcTmpLine..Po+STR(&lcTmpLine..LineNo,6)+lcCnt+&lcTmpLine..Style
    lcBomLKey = IIF(lcPType='D','D','I')+'2'+IIF(lcPType='S',&lcTmpLine..Shipno,'')+&lcTmpLine..Po+STR(&lcTmpLine..LineNo,6)+lcCnt+&lcTmpLine..Style
    *khm1
    lcWCondtn = "cImTyp+cType+IIF(lcPType='S',ShipNo,'')+cTktNo+STR(LineNo,6)+cBomTyp+Style = lcBomLKey"
    lcFCondtn = "(EMPTY(cRSession) OR cRSession=lcGlSession) AND cStyGrade = &lcTmpLine..cStyGrade"
  ENDIF 
  
  lnNLCs&lcCnt = 0
  lnCurSQt = 0
  SELECT BOMLINE
  IF SEEK(lcBomLKey)
    REPLACE REST StyQty  WITH lfBomSzQt(),;
                 ItemQty WITH (StyQty*UnitQty),;
                 ItemAmt WITH (ItemQty*UnitCost);
           WHILE &lcWCondtn FOR &lcFCondtn 
    =SEEK(lcBomLKey)
    *B802443,1 Start, Divide by total qty in the line.
    *SUM REST (UnitCost*UnitQty) WHILE &lcWCondtn FOR &lcFCondtn TO lnNLCs&lcCnt
     SUM REST (UnitCost*UnitQty)*StyQty WHILE &lcWCondtn FOR &lcFCondtn TO lnNLCs&lcCnt
     lnNLCs&lcCnt = IIF(&lcTmpLine..TotQty<>0,(lnNLCs&lcCnt/&lcTmpLine..TotQty),0)
     
     *khalid1
     lnNLCs&lcCnt = IIF(TYPE("lnNLCs"+lcCnt) <> "N",0,lnNLCs&lcCnt)
     *khalid1
     
    *B802443,1 End.
  ENDIF 
  
  SELECT (lcTmpLine)
  REPLACE nLan_Cst&lcCnt WITH lnNLCs&lcCnt
  IF ! llMFCall
    IF llMulCurr
      lnRate1 = nLanPrRat
      lnRate2 = nLanDuRat
      =lfGetEqv(lcCnt,lnRate1,lnRate2,lnCurrUnt1,lnCurrUnt2,nLan_Cst1,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5)
      *khalid
      laECost[1] = IIF(TYPE("laECost[1]") <> "N", 0, laECost[1])
      *khalid
      
      *B607034,1 ARH 03/16/2003 (Start) Fixed numeric overflow bug in case of England
      laECost[1] = IIF(OCCURS('*',STR(laECost[1])) > 0,0,laECost[1])
      *B607034,1 ARH 03/16/2003 (End)
      
      REPLACE nELanCost&lcCnt WITH laECost[1]
    ELSE
      REPLACE nELanCost&lcCnt WITH lnNLCs&lcCnt
    ENDIF
  ENDIF
ENDFOR

SELECT (lcTmpLine)
RETURN
******************
FUNCTION lfBomSzQt
lnCurSQt = 0
FOR lnI=1 TO 8
  IF STR(lnI,1) $ BOMLINE.CSIZES
    lnCurSQt = lnCurSQt + laLnQty[lnI]
  ENDIF
ENDFOR
RETURN lnCurSQt
*B602966,1 End.

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
IF llWareLoc 
  lcFiles = "lcTmpLine," + lcTmpLine + "," + ORDER(lcTmpLine) + ";" +;
            "lcTemLoc,"  + lcTemLoc  + "," + ORDER(lcTemLoc)  + ";"
ENDIF
IF !llVarUpd AND EMPTY(lcUnSStat) AND llLinkToGl
  lcFiles = lcFiles + "lcGlDist," + lcGlDist + ", ;"
ENDIF
= gfSavSess(lcUnCmPgId, lcFiles, @laVariables,lcSession)
RETURN

*:*************************************************************
*! Name     : lfPrnLanded
*! Developer: Ahmed Salah Shalaby - (SSH)
*! Date     : 10/10/97               
*! Purpose  : Calculate landed costs case of detail costing. 
*:*************************************************************
FUNCTION lfPrnLanded
*B802902,1 SSH 03/05/2000 If we select to preview log report, 
*B802902,1 SSH            the reports show the estimated p.price along 
*B802902,1 SSH            (with each style/color being received) 
*B802902,1 SSH            instead of the landed p.price. 

*B803337,1 WMA [Begin]
*IF !llOpnBomLn
*  llOpnBomLn = lfOpn_Rest(gcDatadir,'BOMLINE','BOMLINE')
*ENDIF  
*B803337,1 WMA [End]

SELECT BOMLINE
lnOldKey = ORDER()
IF lcPType ='S'
  SET ORDER TO TAG BomLnShp
ELSE
  SET ORDER TO TAG BomLine
ENDIF
SELECT POSLN
SCATTER FIELDS Qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8 TO laLnQty

FOR lnCnt=1 to 5
  lcCnt=STR(lnCnt,1)
  *khm1
  *lcBomLKey = 'I2'+IIF(lcPType='S',POSLN.Shipno,'')+POSLN.Po+STR(POSLN.LineNo,6)+lcCnt+POSLN.Style
  lcBomLKey = IIF(lcPType='D','D','I')+'2'+IIF(lcPType='S',POSLN.Shipno,'')+POSLN.Po+STR(POSLN.LineNo,6)+lcCnt+POSLN.Style
  *khm1
  
  lcWCondtn = "cImTyp+cType+IIF(lcPType='S',ShipNo,'')+cTktNo+STR(LineNo,6)+cBomTyp+Style = lcBomLKey"
  lcFCondtn = "EMPTY(cRSession) AND cStyGrade = POSLN.cStyGrade"
  
  lnNLCs&lcCnt = 0
  lnCurSQt = 0
  SELECT BOMLINE
  IF SEEK(lcBomLKey)
    SUM REST (UnitCost*UnitQty)*lfBomSzQt() WHILE &lcWCondtn FOR &lcFCondtn TO lnNLCs&lcCnt
    lnNLCs&lcCnt = IIF(POSLN.TotQty<>0,(lnNLCs&lcCnt/POSLN.TotQty),0)
  ENDIF 
  SELECT POSLN
  REPLACE nLan_Cst&lcCnt WITH lnNLCs&lcCnt
  IF !llMFCall
    IF llMulCurr
      lnRate1 = nLanPrRat
      lnRate2 = nLanDuRat
      =lfGetEqv(lcCnt,lnRate1,lnRate2,lnCurrUnt1,lnCurrUnt2,nLan_Cst1,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5)
      REPLACE nELanCost&lcCnt WITH laECost[1]
    ELSE
      REPLACE nELanCost&lcCnt WITH lnNLCs&lcCnt
    ENDIF
  ENDIF
ENDFOR
SELECT BOMLINE
SET ORDER TO lnOldKey
SELECT POSLN
RETURN


*!*************************************************************
*! Name      : lfCrtUnComp
*! Developer : Timour A. K. 
*! Date      : 10/10/97               
*! Purpose   : To create all the uncompete session temprory files
*!             that is needed by this program.
*!*************************************************************
*! Call      : gfCrtTmp()
*!*************************************************************
*! Modification : *E301180,1 03/22/99
FUNCTION lfCrtUnComp

SELECT (lcMasterLn)
=AFIELDS(laFStru)
lnFStru = ALEN(laFStru,1)

DIMENSION laFStru[lnFStru+IIF(llMFCall,14,13),4]

laFStru[lnFStru+1,1] = 'TOTSTK'
laFStru[lnFStru+2,1] = 'TOTDAM'
laFStru[lnFStru+3,1] = 'TOTCAN'  
laFStru[lnFStru+4,1] = 'TOTBAL'
laFStru[lnFStru+5,1] = 'CCARTON'
laFStru[lnFStru+6,1] = 'NLINENO'
laFStru[lnFStru+7,1] = 'LNEWLN'
laFStru[lnFStru+8,1] = 'NSTEPS'      
laFStru[lnFStru+9,1] = 'LALOCHG'
laFStru[lnFStru+10,1]= 'CLASTOPR'
laFStru[lnFStru+11,1]= 'LNEWLUPD'
laFStru[lnFStru+12,1]= 'LCOSTMADE'
laFStru[lnFStru+13,1]= 'LAUTOMODE'

STORE 'N' TO laFStru[lnFStru+1,2],laFStru[lnFStru+2,2],;
             laFStru[lnFStru+3,2],laFStru[lnFStru+4,2],;
             laFStru[lnFStru+6,2],laFStru[lnFStru+8,2]
STORE 'C' TO laFStru[lnFStru+5,2],laFStru[lnFStru+10,2]

STORE 'L' TO laFStru[lnFStru+7,2],laFStru[lnFStru+9,2],;
             laFStru[lnFStru+11,2],laFStru[lnFStru+13,2]
laFStru[lnFStru+12,2]= 'L'

STORE  6  TO laFStru[lnFStru+1,3],laFStru[lnFStru+2,3],;
             laFStru[lnFStru+3,3],laFStru[lnFStru+4,3],;
             laFStru[lnFStru+6,3],laFStru[lnFStru+10,3]
STORE  3  TO laFStru[lnFStru+5,3]  
STORE  2  TO laFStru[lnFStru+8,3]
STORE  1  TO laFStru[lnFStru+7,3],laFStru[lnFStru+9,3],;
             laFStru[lnFStru+11,3],laFStru[lnFStru+13,3]

laFStru[lnFStru+12,3]= 1

STORE  0  TO laFStru[lnFStru+1,4],laFStru[lnFStru+2,4],;
             laFStru[lnFStru+3,4],laFStru[lnFStru+4,4],;
             laFStru[lnFStru+5,4],laFStru[lnFStru+6,4],;
             laFStru[lnFStru+7,4],laFStru[lnFStru+8,4],;
             laFStru[lnFStru+9,4],laFStru[lnFStru+10,4],;
             laFStru[lnFStru+11,4],laFStru[lnFStru+13,4]

laFStru[lnFStru+12,4]= 0

IF llMFCall
  laFStru[lnFStru+14,1] = 'cRcvBy'
  laFStru[lnFStru+14,2] = 'C'
  laFStru[lnFStru+14,3] = 19
  laFStru[lnFStru+14,4] = 0
ENDIF  

*--Index tags array.
DIMENSION laTags[3,2]
IF llMfCall
  laTags[1,1]='TranCd+cCarton+Cuttkt+Style+Dyelot+cWareCode+STR(LineNo,6)'
  laTags[2,1]='cCarton+Cuttkt+Style+Dyelot+cWareCode+STR(LineNo,6)+TranCd'
  laTags[3,1]='Cuttkt+Style+Dyelot+cWareCode+STR(LineNo,6)+cCarton+TranCd'
  
  laTags[1,2]='TmpLine1'
  laTags[2,2]='TmpLine2'
  laTags[3,2]='TmpLine3'
ELSE
  DIME laTags[5,2]
  laTags[1,1]='TranCd+cCarton+Po+Style+Dyelot+cWareCode+STR(LineNo,6)'
  laTags[2,1]='cCarton+PO+Style+Dyelot+cWareCode+STR(LineNo,6)+TranCd'
  laTags[3,1]='PO+Style+Dyelot+cWareCode+STR(LineNo,6)+cCarton+TranCd'
  laTags[4,1]='cstytype+po+style+STR(lineno,6)+TranCd'
  laTags[5,1]='shipno+cstytype+po+style+STR(lineno,6)+trancd'
  laTags[1,2]='TmpLine1'
  laTags[2,2]='TmpLine2'
  laTags[3,2]='TmpLine3'
  laTags[4,2]='POSLN'
  laTags[5,2]='POSLNSH'
ENDIF
=gfCrtTmp(lcTmpLine,@laFStru,@laTags)
SELECT (lcTmpLine)
SET ORDER TO TAG TmpLine1

*--Warehouse location tmp file.
IF llWareLoc 
  SELECT WhsLoc
  =AFIELDS(laFStru)
  =gfCrtTmp(lcTemLoc,@laFStru,'Style+cWareCode+cLocation',lcTemLoc)
ENDIF  
RETURN
*--End...

*!*************************************************************
*! Name    : lfClearInfo
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Clear information.
*!*************************************************************
FUNCTION lfClearInfo

*--Clear Header.
STORE ' ' TO lcAirWayB,lcShpRef,lcBDesc,lcBStatus,lcSOrder,lcAOrder
STORE {}  TO ldEnterd,ldEta,ldBDate
STORE 0   TO lnCartons,lnTotStk,lnTotDam,lnTotCan,lnPolstln
STORE .F. TO llByCarton

*C101424,1 [BEGIN]
*-- When <CANCEL> make the user free to receive by C/T or by style
IF llMFCall AND lcPtype='M'
  STORE ' ' TO lcStyle,lcTcode,lcStyDesc
 _CUROBJ=OBJNUM(lcTCode)
ENDIF
*C101424,1 [END..]

*--Clear Line.
=lfReadLine(.T.)

RETURN


*!*************************************************************
*! Name    : lfReadLine
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Read line information.
*!*************************************************************
FUNCTION lfReadLine
PARA llClearLn

lnAlias = SELECT()
IF ! llClearLn
  SELECT (lcTmpLine)
  lnBrRecNo=RECNO()
  lcCarton= cCarton
  lcTCode    = IIF(llMfCall,Cuttkt,Po)
  lcStyle = Style
  *E300935,4 lnware does not changed in Point of sell
  *lnWare  = ASCAN(laWare,cWareCode,1)
  lnWare  = IIF(llCMInstld AND llPOSale,1,ASCAN(laWare,cWareCode,1))
  lcRefer = Reference
 
  *E300935,4 Now Dyelot field is in C/T and P/O.
  *lcDyelot= IIF(llMfCall,Dyelot,'')
  lcDyelot= Dyelot
  *C200080,1 AMM Add the dye order type 'D'
  *E301291,4 AMM Consider cases of issue/receive adornment PO
  *lcSeekTyp = IIF(lcPType $ 'RNO',IIF(lcPType='O','N',lcPType),'P')
 
  *E301480,1 NAD (Start)  Add the Inter location Po batch to the condition.
  lcSeekTyp = IIF(lcPType $ 'RNODAE',IIF(lcPType='O','N',IIF(lcPType='E','A',lcPType)),'P')
  lcSeekTyp = IIF(lcPType $ 'RNODAEL',IIF(lcPType $'OL','N',IIF(lcPType='E','A',lcPType)),'P')
   *E301480,1 NAD (End)
   
  *E301291,4 AMM
  *C200080,1 AMM end
  IF !llMfCall
    =SEEK(lcSeekTyp+lcTCode,'POSHDR')
  ENDIF
  IF llMulCurr AND !llMfCall
    *-Read currency.
    lcCur1  = POSHDR.cPriceCur
    lcCur2  = POSHDR.cDutyCur
    *-Read rate.
    lnRate1 = nLanPrRat
    lnRate2 = nLanDuRat
    *-Read units.
    *=gfChkRate('lnCurrUnt1',lcCur1,ldRcvDate,.F.,gcAct_comp,.F.) 
    *=gfChkRate('lnCurrUnt2',lcCur2,ldRcvDate,.F.,gcAct_comp,.F.) 
    *-Read symble.
    
    *IF !laOpFile[12]
      = lfOpn_Rest(gcSysHome,'SYCCURR','CCURRCODE')
    *ENDIF
    
    lcPfrnsmbl= IIF(SEEK(lcCur1,'SycCurr'),SycCurr.cCurrSmbl,'')
    lcDfrnsmbl= IIF(SEEK(lcCur2,'SycCurr'),SycCurr.cCurrSmbl,'')
  ENDIF
  *E301291,4 AMM Consider case of issue adornment PO
  *lcWareCode = IIF(lcPType = 'N',PADR(POSHDR.Vendor,6),cWareCode)
  lcWareCode = IIF(lcPType $ 'NA',PADR(POSHDR.Vendor,6),cWareCode)
  *E301291,4 AMM end
  lcStyDesc = IIF(SEEK(lcStyle,'STYLE'),STYLE.Desc1,'')
  *B603237,1 Start.
  lcSOrder = Reference
  lcAOrder = IIF(lcPType $ 'AE' AND llSpecHndl,PO,'')
  lcTCode  = IIF(lcPType $ 'AE' AND llSpecHndl,ShipNo,lcTCode)
  *B603237,1 End.

ELSE
  STORE ' ' TO lcCarton,lcTCode,lcStyle,lcRefer,lcCur1,lcCur2,;
               lcStyDesc,lcDyelot,lcPfrnSmbl,lcDfrnSmbl,lcWareCode,lcSOrder,lcAOrder

  *C200170,4 AMH Add case of issue inter-location P/O Batch [Start]
  IF lcPType = 'H'
    lcSource = ''
  ENDIF
  *C200170,4 AMH [End]
  
  STORE 0   TO lnWare,lnRate1,lnRate2
  STORE 1   TO lnCurrUnt1,lnCurrUnt2
ENDIF
RETURN

*!*************************************************************
*! Name      : lfGetEqv
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Get eguevalent costs.
*!*************************************************************
FUNCTION lfGetEqv
PARA lcUpdCsts,lnPRate1,lnDRate2,lnCurUnt1,lnCurUnt2,;
     lnFCost1,lnFCost2,lnFCost3,lnFCost4,lnFCost5

DIME laECost[LEN(lcUpdCsts)]
lnPt = 1
IF '1' $ lcUpdCsts 
  laECost[lnPt] = lfvEquCost('1',lnFCost1,lnPRate1,lnCurUnt1)
  lnPt = lnPt + 1
ENDIF
IF '2' $ lcUpdCsts 
  laECost[lnPt] = lfvEquCost('2',lnFCost2,lnDRate2,lnCurUnt2)
  lnPt = lnPt + 1
ENDIF
IF '3' $ lcUpdCsts 
  laECost[lnPt] = lfvEquCost('3',lnFCost3,lnDRate2,lnCurUnt2)
  lnPt = lnPt + 1
ENDIF
IF '4' $ lcUpdCsts 
  laECost[lnPt] = lfvEquCost('4',lnFCost4,lnDRate2,lnCurUnt2)
  lnPt = lnPt + 1
ENDIF
IF '5' $ lcUpdCsts 
  laECost[lnPt] = lfvEquCost('5',lnFCost5,lnDRate2,lnCurUnt2)
  lnPt = lnPt + 1
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvEquCost
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Get eguevalent cost by pathing the foreign.
*!*************************************************************
FUNCTION lfvEquCost
PARAMETERS lcCstNo,lnFrnCost,lnCurRate,lnCurUnt

=SEEK(&lcTmpLine..CSTYTYPE+&lcTmpLine..PO,'POSHDR')
lcCur1  = POSHDR.cPriceCur
lcCur2  = POSHDR.cDutyCur

lnCstType = lcIType&lcCstNo
IF lnCstType $ 'PMD'
  STORE '' TO lcPMethod,lcPUnMeth,lcDMethod,lcDUnMeth
  IF lnCstType='P'
    lcPMethod = gfGetExSin(@lcPUnMeth,lcCur1)
    lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
    lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
    lnEquCost = lnFrnCost &lcPMethod lnCurRate &lcPUnMeth lnCurUnt
  ELSE
    lcDMethod = gfGetExSin(@lcDUnMeth,lcCur2)
    lcDMethod = IIF(EMPTY(lcDMethod),'*',lcDMethod)
    lcDUnMeth = IIF(EMPTY(lcDUnMeth),'/',lcDUnMeth)
    lnEquCost = lnFrnCost &lcDMethod lnCurRate &lcDUnMeth lnCurUnt
  ENDIF
ELSE
  lnEquCost = lnFrnCost
ENDIF
lnEquCost = ROUND(lnEquCost,2)
RETURN (lnEquCost)



*!*************************************************************
*! Name      : lfSetFltr                      *B802592,1 Added. 
*! Developer : TAK
*! Date      : 20/09/1999
*! Purpose   : Build a filter array that used in ariabrow base
*!             on base file defined.
*!*************************************************************
FUNCTION lfSetFltr
PRIVATE lcBFields

IF llMFCall
  lcBFields = "CutTkt,Style,Status,Entered,Complete,Season,cDivision,Pcs_Bud,Pcs_Rec,Pcs_Dam,Pcs_Opn"
ELSE
  lcBFields = "PO,Status,Vendor,Complete,nStyOrder,POTotal,Receive,Open"
ENDIF
llOndffl = gfOpenFile(gcSysHome+'SYDFLFLD','Cfile_nam','SH')
llOndfld = gfOpenFile(gcSysHome+'SYDFIELD','Cfld_name','SH')

DIMENSION gaTmpArray[1,3]
gaTmpArray = ' '

SELECT DIST SYDFIELD.CFLD_NAME, SYDFIELD.cfld_head, SYDFLFLD.NFLD_POS;
       FROM SYDFLFLD, SYDFIELD;
      WHERE SYDFIELD.CFLD_NAME = SYDFLFLD.CFLD_NAME  .AND. ;
              sydflfld.cfile_nam == lcBaseFile       .AND. ;
              UPPER(ALLTRIM(sydfield.cfld_name)) $  UPPER(lcBFields) ;
     ORDER BY SYDFLFLD.NFLD_POS;
     INTO ARRAY gaTmpArray

SELECT SYDFIELD.CFLD_NAME,SPACE(10) ;
    FROM SYDFLFLD, SYDFIELD;
   WHERE SYDFIELD.CFLD_NAME = SYDFLFLD.CFLD_NAME   .AND. ;
           sydflfld.cfile_nam == lcBaseFile        .AND. ;
           UPPER(ALLTRIM(sydfield.cfld_name)) $  UPPER(lcBFields) .AND. ;
           !EMPTY (sydfield.mventries) ;
   INTO ARRAY laArrayes

  gnArrayln = ALEN(gaTmpArray,1)
  DECLARE laField_H [gnArrayln,1] ,laField_N [gnArrayln,1]
  
  FOR lnElem = 1 TO gnArrayln
    laField_N[lnElem,1] = ALLTRIM(gaTmpArray[lnElem,1])
    laField_H[lnElem,1] = IIF(EMPTY(gaTmpArray[lnElem,2]),;
                  ALLTRIM(gaTmpArray[lnElem,1]),ALLTRIM(gaTmpArray[lnElem,2]))
  ENDFOR

IF llOndffl AND USED('SYDFLFLD')
  USE IN sydflfld
ENDIF
IF llOndfld AND USED('SYDFIELD')
  USE IN SYDFIELD
ENDIF
RETURN


*!*************************************************************
*! Name    : 
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Get information.
*!*************************************************************
FUNCTION lfGetInfo
PARAMETERS llEmpWare

IF llMulCurr
  lcCur1  = POSHDR.cPriceCur
  lnCrRt1 = IIF(lcCur1=gcBaseCurr,1,gfChkRate('lnCurrUnt1',lcCur1,ldRcvDate,llEditExRt,gcAct_comp,.F.)) 
  lcCur2  = POSHDR.cDutyCur
  lnCrRt2 = IIF(lcCur2=gcBaseCurr,1,gfChkRate('lnCurrUnt2',lcCur2,ldRcvDate,llEditExRt,gcAct_comp,.F.)) 
  *--Cannot accept zero or -ve rates.
  IF lnCrRt1=0 OR lnCrRt2=0
    IF !llEditExRt
      *--This line has currency with zero rate, it will be ignored.' 
      = gfModalGen('TRM34079B42000','DIALOG')
      STORE ' ' TO lcStyle,lcCur1,lcCur2
      SHOW GETS
      RETURN
    ELSE
      STORE 1 TO lnCrRt1,lnCrRt2
    ENDIF
  ENDIF
ENDIF

IF llNewItem
  SELECT STYLE
  SCATTER FIELDS nICost1,nICost2,nICost3,nICost4,nICost5 TO laEstiCost
  SELECT (lcTmpLine)      

  REPLACE nLanPrRat WITH lnCrRt1 ;
          nLanDuRat WITH lnCrRt2  

  IF lcCur1=STYLE.cPriceCur
    REPLACE nCost1  WITH laEstiCost[1],nLan_Cst1 WITH laEstiCost[1]
    =lfGetEqv('1',nLanPrRat,0,lnCurrUnt1,0,nLan_Cst1,0,0,0,0)
    REPLACE nECost1 WITH laECost[1],nELanCost1 WITH laECost[1]
  ENDIF
  IF lcCur2=STYLE.cDutyCur
    REPLACE nCost2    WITH laEstiCost[2],nCost3    WITH laEstiCost[3],;
            nCost4    WITH laEstiCost[4],nCost5    WITH laEstiCost[5],;
            nLan_Cst2 WITH laEstiCost[2],nLan_Cst3 WITH laEstiCost[3],;
            nLan_Cst4 WITH laEstiCost[4],nLan_Cst5 WITH laEstiCost[5]
    =lfGetEqv('2345',0,nLanDuRat,0,lnCurrUnt2,0,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5)
    GATHER FROM laECost FIELDS nECost2,nECost3,nECost4,nECost5
    GATHER FROM laECost FIELDS nELanCost2,nELanCost3,nELanCost4,nELanCost5
  ENDIF 
      
  SCATTER MEMVAR MEMO
ENDIF


*C037439,3  TMI [Start] fix the bug that "the function 'lfGetColor' not found"
*!*************************************************************
*! Name      : lfGetColor
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/30/1998
*! Purpose   : Get color for primary fabric From BOMLINE file
*!           : which updated in Cost sheet.
*!*************************************************************
*! Called from : lpSavScr
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : Style to get its corressponding color from BOMLINE table.
*!*************************************************************
*! Return      : Color
*!*************************************************************
*! Example     : = lfGetColor()
*!*************************************************************
*E300935,4
FUNCTION lfGetColor
PARAMETERS lcStyFab
PRIVATE lcCurAlias,lcBomOrder,lcStyOrder,lcStyKey
lcCurAlias = ALIAS()

lcStyKey   = STYLE.STYLE
lcStyOrder = ORDER('STYLE')
SET ORDER TO STYLE IN STYLE

= SEEK(lcStyFab,'STYLE')
lcUpDatFab = STYLE.FABRIC

= SEEK(lcStyKey,'STYLE')
SET ORDER TO &lcStyOrder IN STYLE

*E301480,1 NAD (Start) Add the Inter location Po batch to the condition
lcSeekTyp =IIF(llMFCall,'M','I')
lcSeekTyp =IIF(llMFCall,'M',IIF(lcPType='L','N','I'))
*E301480,1 NAD (End)

lcSeekClr = lcSeekTyp+'1'+IIF(llMFCall,&lcTmpLine..Cuttkt,&lcTmpLine..PO)+;
            STR(&lcTmpLine..LineNo,6)+lcSeekBomTp+;
            lcStyFab + SPACE(6) + lcUpDatFab 

*B803337,1 WMA [Begin]
*E301077,11 MAB Open BOMLINE file here [Begin
*IF !llOpnBomLn
*  llOpnBomLn = lfOpn_Rest(gcDatadir,'BOMLINE','BOMLINE')
*ENDIF  
*E301077,11 MAB Open BOMLINE file here [End..
*B803337,1 WMA [End]

SELECT BOMLINE
lcBomOrder = ORDER('BOMLINE')
SET ORDER TO BOMLINE IN BOMLINE

= SEEK(lcSeekClr,'BOMLINE')

SET ORDER TO &lcBomOrder IN BOMLINE

SELECT (lcCurAlias)
RETURN BOMLINE.ICLR
*-- end of lfGetColor.
*C037439,3  TMI [End  ]

*!*************************************************************
*! Name      : lfUpDatFab
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/10/1998
*! Purpose   : Update FabDye File.
*!*************************************************************
*! Called from : lpSavScr
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : Color
*!*************************************************************
*! Example     : = lfUpDatFab()
*!*************************************************************
*E300935,4
FUNCTION lfUpDatFab
PARAMETERS lcFabKey,lcClrKey,lcDyeKey,lcWareKey 
PRIVATE lcCurAlias,lcDyeOrder,lcFabOrder,lcMyFabKey
lcCurAlias = ALIAS()
lcDyeOrder = ORDER('FABDYE')
SET ORDER TO FABDYE IN FABDYE

lcFabOrder = ORDER('FABRIC')
lcMyFabKey = FABRIC.FABRIC + FABRIC.COLOR 
SET ORDER TO FABRIC IN FABRIC

SELECT FABDYE
*-- if you does not find location record in fabdye file, add it.
IF llWareHous AND !SEEK(lcFabKey+lcClrKey+lcWareKey)
  DO gpAdFabWar WITH lcFabKey,lcClrKey,SPACE(10),lcWareKey
  =gfTraceKey('FABDYE',lcFabKey+lcClrKey+SPACE(10)+lcWareKey,'A')
ENDIF  && end if you does not find location record in fabdye file, add it.

IF llDyelot AND llFabDye AND SEEK(lcFabKey + lcClrKey,'FABRIC') AND ;
   FABRIC.CDYE_FLG = 'Y' AND !SEEK(lcFabKey+lcClrKey+lcWareKey+lcDyeKey,'FABDYE')
  *-- add dyelot record and save tmpscope to arrange it later.
  DO gpAdFabWar WITH lcFabKey,lcClrKey,lcDyeKey,lcWareKey,lcTmDyeRel
  =gfTraceKey('FABDYE',lcFabKey+lcClrKey+lcDyeKey+lcWareKey,'A')
ENDIF              

SET ORDER TO &lcDyeOrder IN FABDYE

= SEEK(lcMyFabKey,'FABRIC')
SET ORDER TO &lcFabOrder IN FABRIC

SELECT (lcCurAlias)
RETURN
*-- end of lfUpDatFab.
