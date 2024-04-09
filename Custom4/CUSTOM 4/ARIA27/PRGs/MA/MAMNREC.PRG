*!**************************************************************************
*! Program file        : MAMNREC.PRG
*! Program description : Receive Material Manufacturing Order .
*! For screen          : AMNREC.SPR
*! For System          : Aria Advantage Series - Version 2.7
*! For Module          : MATERIAL - (MA)
*! Developer Name      : Walid Abou El-Magd (WAM) Due to E#301235,1
*! Last modi           : 
*!**************************************************************************
*! Calls               : MAMNREC.SPR & MARCVQ.SPR 
*!**************************************************************************
*! Passed Parameters   : None.
*!**************************************************************************
*! Example             : DO MAMNREC.SPR 
*!**************************************************************************
*! Modification        :
*!B803075,1 03/05/2000 enlarge TOTSTK,TOTDAM,TOTCAN fields width in the tmp lines file
*!B803591,1 AAN 28/08/2000 Correct spelling word applying in mat manu 
*!B603950,1 ABD 10/16/2000 Fix Bug 'Alias Not Found'
*!B603880,1 KAM 11/26/2000 fIX bug no display data in line browse 
*!C200255,1 ADEL 23/12/01 Update MATINVJL with custom Voucher no for CON10.
*!C200256,1 ADEL 27/12/01 Print Received Qty for CON10.
*!B605212,1 ADEL 01/28/02 Print Rolls and fix - along with this bug's specx - updating receiving
*!B605212,1               session and date.
*!C200254,1 ABD 02/04/2002 Issue and receive the material manufacturing 
*!C200254,1 ABD            Rework order from this program.
*!C200254,1 ABD - Issue and receive the material manufacturing 
*!C200254,1 ABD - Rework order from this program so I will Pass
*!C200254,1 ABD - the following parameter. [Begin]
*!B604856,1 AMH 08/21/2002 Fix the bug of wrong receiving the last operation.
*!B606984,1 KHM 04/09/2003 Fix the bug of not updating the landed costs in the header file
*!B606984,1                in case of receiving MMO.
*!B038431,1 MHM 08/24/2004 Fix Bug of press "C" to cancel then screen is not closed
*!*******************************************************************************************
*: Passed Parameters  : lcMatType Hold 'W' IF we issue OR receive Rework order.
PARAMETERS lcMatType
*- The order detail will contain Trans Code field 
*- TranCod = 1 	=== > 	Budget Qty 
*- TranCod = 2 	=== > 	Issued Qty to the vendor 
*- TranCod = 3 	=== > 	received from the vendor  
*- TranCod = 4 	=== > 	received to canceled 
*- TranCod = 5	=== > 	received to Second Quality
*- TranCod = 6	=== > 	received to Damage
*!C200254,1 ABD -  [End]

EXTERNAL ARRAY laDefProc

DIMENSION laware [1]

*-- Inetialize all logic variables
*-- llFirstTmp    && Flag to indicate if the tmp file is created or not .
*-- llWareLoc     && is a system sitting variable (is the system use locations )
*-- laDefProc[9]  && = .F.   : Disable the control panel save proc.  (lpSavScr)
*-- laDefProc[10] && = .F.   : Disable the control panel close proc. (lpClsScr)
*-- llWareHous    && Is this company Multi-Warehouse?
*-- llDyelot      && Dyelots used?
*-- llTrkRolls    && Keep Track of Mat. Rolls ?
*-- llLinkToGl    && Link to G/L?
*-- llRollBrow    && Does the system keep track of Mat. Rolls and current mat use rolls?
STORE .F. TO llWareHous , llWareLoc   , llDyelot , llFabDye   , llTrkRolls  , llLinkToGl ,;
             llNoShow   , laScrMode   , llBrowse , laScrMode  ,;
             llFirstTmp , llUseDyelot , llVldDye , llRollBrow , laDefProc[9],;
             laScrMode  , laDefProc[10]
STORE .T. TO laScrMode[1]
*-- Inetialize all string variables
*HDM[Start]
*STORE ''  TO lcClrDesc  , lcPatRen , lcTCode     , lcColor   , lcItem     , lcDyelot  , lcStyDesc,;
             lcRefer    , laware   , lcWinCh1    , lcWinCh2  , lcWinCh3   , lcOldVal  , lcBrwFlds,;
             lcWareCode , lcTmpRoll ,lcTmpRoll2  , lcTmpLine , TmpLine1   , lcTmpFile ,;
             lcGlDist   , lcTemLoc , lcTempName  , lcGLFYear , lcGLPeriod ,lcOthrTrCd ,;
             lcCanlTrCd , lcFJlSess

STORE ''  TO lcClrDesc  , lcPatRen , lcTCode     , lcColor   , lcItem     , lcDyelot  , lcStyDesc,;
             lcRefer    , laware   , lcWinCh1    , lcWinCh2  , lcWinCh3   , lcOldVal  , lcBrwFlds,;
             lcWareCode , lcTmpRoll ,lcTmpRoll2  , lcTmpLine , TmpLine1   , lcTmpFile ,;
             lcGlDist   , lcTemLoc , lcTempName  , lcGLFYear , lcGLPeriod ,lcOthrTrCd ,;
             lcCanlTrCd , lcFJlSess , lcFullRoll , lcTmpJour , lcLotNo
*HDM[End]             
lcBrowTitl='Receiving Lines'
*HDM[Start]
*--Variables of opereation lots.
llSpecLot = .F.
DIME laLotArry[8]
laLotArry = 0
*HDM[END]
*-- Inetialize all State control variables
STORE 'DISABLE' TO lcOrderSt , lcItemSt  , lcColorSt , lcPatrenSt , lcDyelotSt,;
                   lcWareSt  , lcReferSt , lcEditQSt , lcNewSt    , lcRemoveSt
*-- Inetialize all numeric variables             
STORE  0  TO lnTotStk , lnTotDam , lnTotCan , lnWare , lnPolstln , lnRecPos,;
             lnTopRow , lnBotRow , lnLftRow     
STORE  0  TO lnTotStk , lnTotDam , lnTotCan , lnWare , lnPolstln , lnRecPos,;
             lnTopRow , lnBotRow , lnLftRow , lnTotRec
lnMarker = 1
*B603950,1 ABD Define variable will use if company use rolls , when saving. [Begin]
*-- Define this variable to handel new rolls when recive on rolls.
lcKeyType = 'P'
lcType    = 'W'
*B603950,1 ABD [End]
*-- Inetialize all date variables
STORE gdSysDate TO ldRcvDate,ldPost
lcNewP     = gcBmpHome + "NEW.BMP"
lcRemveP   = gcBmpHome + "REM.BMP"
lcClse1Bmp = gcBmpHome + "CLOSE1.BMP"

*C200254,1 ABD - Issue and receive the material manufacturing 
*C200254,1 ABD - Rework order from this program so I will use
*C200254,1 ABD - the following Variables. [Begin]
*-- This variables will ise in rolls.
lcSelecP   = gcBmpHome + "SEL1.BMP"
lcUnSelP   = gcBmpHome + "UNSEL.BMP"

STORE .F. TO llRwIssRec , llOpRwHdr , llOpRwLin
STORE '' TO lcBfileHdr , lcBfileLin,lcOprtType
IF !(TYPE('lcMatType') $ 'UL')
  llRwIssRec = .T.
  DIMENSION laType[1,2]
  STORE ' ' TO laType  , lcOprtType
  *-- Open Needed files.
  IF !USED('MARWHDR')
    llOpRwHdr = gfOpenFile(gcDataDir+'MARWHDR',gcDataDir+'MARWLIN','SH')
  ENDIF

  IF !USED('MARWLIN')
    llOpRwLin = gfOpenFile(gcDataDir+'MARWLIN',gcDataDir+'MMFGORDDLN','SH')
  ENDIF
  lcBfileHdr = 'MARWHDR'
  lcBfileLin = 'MARWLIN'
ELSE
  lcBfileHdr = 'MMFGORDH'
  lcBfileLin = 'MMFGORDD'
ENDIF
lcComField = IIF(llRwIssRec,'cMfgrwNum','cmfgordno')
lcBrTrncd = IIF(!llRwIssRec,'1',IIF(lcOprtType = 'I','1','2'))
*-- This file will use While we issue and cost method lot.
lcTmpJour  = gfTempName()
lcTmpMtIv  = gfTempName()
*C200254,1 ABD - [End]

IF !gfSetup()
  RETURN
ENDIF

IF !WEXIST(gcBaseWind)
  *-- Get all neede memory variables [START]
  *B605212,1 (Begin) Add a new element for Cost method. 
  *DIMENSION laSetups[6,2]
  DIMENSION laSetups[7,2]
  *B605212,1 (End)
  laSetups[1,1]  = 'M_WareHouse'
  laSetups[2,1]  = 'M_WareLoc'       
  *HDM[START]
  *laSetups[3,1]  = 'M_Dyelot'
  laSetups[3,1]  = 'M_MATDYE'
  *HDM[END]
  laSetups[5,1]  = 'm_TrkRolls'
  laSetups[6,1]  = 'M_Link_GL'
  *B605212,1 (Begin) Add a new element for Cost method. 
  laSetUps[7,1]  = 'M_MatCstMt'
  *B605212,1 (End)
  =gfGetMemVar(@laSetups,gcAct_Comp)
  llWareHous = (laSetups[1,2] ='Y')
  llWareLoc  = (laSetups[2,2] ='Y')
  llDyelot   = (laSetups[3,2] ='Y')
  llFabDye   = (laSetups[4,2] ='Y')
  llTrkRolls = (laSetups[5,2] ='Y')
  llLinkToGl = (laSetups[6,2] ='Y')
  *B605212,1 (Begin) Add a new element for Cost method. 
  lcMtCstMth = laSetUps[7,2]
  *B605212,1 (End)

  *-- Get all neede memory variables [END..]
  IF llTrkRolls
    =gfOpenFile(gcDataDir+'ROLLS','','sh')
  ENDIF
  IF llWareLoc
    =gfOpenFile(gcDataDir+'WHSLOC','WHSLOCST','sh')
  ENDIF
  IF llLinkToGl
    =gfOpenFile(gcDataDir+'GLDIST','Gldistac','SH')
  ENDIF
  *-- lnRecPos  : This variable controls Receiving Date position
  lnRecPos        = IIF(llLinkToGl,03.000,01.750)
  lnTopRow        = IIF(llLinkToGl,2.833,1.583)
  lnBotRow        = IIF(llLinkToGl,4.083,2.833)
  lnLftRow        = IIF(llLinkToGl,2.917,1.667)

  *C200254,1 ABD - Defiene the cnacel variable in case rework.[Begin]
  *lcOthrTrCd      = '3'
  lcOthrTrCd      = IIF(llRwIssRec,'6','3')
  *C200254,1 ABD - [End]

  lcCanlTrCd      = '4'
  lcWinCh1        = gfTempName()
  lcWinCh2        = gfTempName()
  lcWinCh3        = gfTempName()
  lcTmpLine       = gfTempName()
  TmpLine1        = gfTempName()
  lcFJlSess       = gfSEQUENCE('GLSession') &&-- must be defined here not in saving
  lcTmpFile       = gfTempName() 
  IF llWareLoc
    lcTemLoc      = gfTempName()
  ENDIF 
  IF llLinkTOGl 
    lcGlDist     = gfTempName()
  ENDIF 
  lcTempName     = gfTempName()
  lcBaseFile=''
  SELECT WAREHOUS
  IF llWareHous  
    *-- only material Warehous are allowed here .
    SELECT cWareCode+'-'+cDesc FROM Warehous INTO ARRAY laWare HAVING Lmatinv=.T.
    *HDM[START] we shouldn't have an N/A option as the user must receice to a
    *           valid W/H
    *=AINS('laWare',1)
    *laWare[1,1] = "              N/A           "
  ELSE
    *-- Note that in this case the popup is not visible .
    GO TOP IN WAREHOUS
    laWare[1] = WAREHOUS.cWareCode
  ENDIF
  =lfCrtUnComp()
ENDIF
*=lfCrtUnComp()

*B603950,1 ABD Define variable will use if company use rolls , when saving. [Begin]
*-- Define this variable to handel new rolls when recive on rolls.
IF llTrkRolls
  lcKeyType = 'P'
  lcType    = 'W'
ENDIF  
*B603950,1 ABD [End]
PUSH KEY

*--Activate Browse window
*-- Define a menu bar with a short-cut key for browse window 
*-- in the program just to be able to activate the browse window
*-- using its menu bar short-cut key.
DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 100 OF P01PU01 PROMPT lcBrowTitl KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTitl)

**-------------------------- Call the screen
DO (gcScrDir+gcWinAppl+"\MAMNREC.SPX")
POP KEY
RELEASE WINDOW (lcBrowTitl)
RELEASE BAR 099 OF P01PU01
RELEASE BAR 100 OF P01PU01

IF glQuitting
  *-- Deactivate the filters
  *-- Erase tmp file if it was created
  IF USED(lcTmpLine)
    USE IN (lcTmpLine)
    ERASE (gcWorkDir+lcTmpLine+'.DBF')
    ERASE (gcWorkDir+TmpLine1+'.CDX')
  ENDIF  

  IF USED(lcTmpFile)
    USE IN (lcTmpFile)
    ERASE (gcWorkDir+lcTmpFile+'.DBF')
    ERASE (gcWorkDir+lcTmpFile+'.CDX')
  ENDIF  

  IF USED(lcTemLoc)
    USE IN (lcTemLoc)
    ERASE (gcWorkDir+lcTemLoc+'.DBF')
    ERASE (gcWorkDir+lcTemLoc+'.CDX')
  ENDIF  

  *!C200254,1 ABD -  [Begin]
  *-- Close Material rework header file if Open.
  IF llOpRwHdr
    = gfCloseFile("MARWHDR")
  ENDIF
  
  *-- Close Material rework line file if Open.  
  IF llOpRwLin
    = gfCloseFile("MARWLIN")
  ENDIF

  IF USED(lcTmpJour)
    USE IN (lcTmpJour)
    ERASE (gcWorkDir+lcTmpJour+'.DBF')
    ERASE (gcWorkDir+lcTmpJour+'.CDX')
    ERASE (gcWorkDir+lcTmpJour+'.FPT')
  ENDIF

  IF USED(lcTmpMtIv)
    USE IN (lcTmpMtIv)
    ERASE (gcWorkDir+lcTmpMtIv+'.DBF')
    ERASE (gcWorkDir+lcTmpMtIv+'.CDX')
    ERASE (gcWorkDir+lcTmpMtIv+'.FPT')
  ENDIF
  
  *!C200254,1 ABD -  [End]
  

ENDIF  


*!*************************************************************
*! Name      : lfRcvDate
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!             
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfRcvDate()
*!*************************************************************

FUNCTION lfRcvDate

IF LASTKEY() =  13
  llValdDate = .F.
  IF !llLinkToGl
    IF !CHECKPRD(ldPost,'lcGLFYear','lcGLPeriod','PO')
      _CUROBJ = _CUROBJ
    ELSE
      llValdDate = .T.  
    ENDIF    
  ELSE
    IF ldRcvDate > ldPost
      =gfModalGen('QRM36106B36000','ALERT')  
      _CUROBJ = OBJNUM(ldRcvDate)
    ELSE
      llValdDate = .T.  
    ENDIF  
  ENDIF  
  IF llValdDate
    SHOW GET ldRcvDate  DISABLE     
    STORE 'ENABLE' TO lcItemSt , lcOrderSt
    SHOW GET ibItem     &lcItemSt
    SHOW GET lcItem     &lcItemSt    
    SHOW GET ibTCode    &lcOrderSt
    SHOW GET lcTCode    &lcOrderSt

    
    
    *!C200254,1 ABD - Remark the next line and open the rework type. [End]
    *_CUROBJ = OBJNUM(lcTCode)
    IF llRwIssRec
      DIMENSION laType[2,2]
      laType[1,1]='Issue Rework Order'
      laType[1,2]='I'
      laType[2,1]='Receive Rework Order'
      laType[2,2]='R'
      SHOW GET lnType ENABLE
      _CUROBJ = OBJNUM(lnType)
      KEYBOARD '{SPACEBAR}'
    ELSE
     _CUROBJ = OBJNUM(lcTCode) 
    ENDIF    
    *!C200254,1 ABD -  [End]
  ENDIF  
ENDIF  
*!*************************************************************
*! Name      : lfvPostDate
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!             
*!*************************************************************
*! Calls     : 
*!             
*!             
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPostDate()
*!*************************************************************
FUNCTION lfvPostDate

IF MDOWN()
  RETURN
ENDIF  
IF LASTKEY() = 13
  IF !CheckPrd(ldPost,'lcGlFYear','lcGlPeriod','PO') OR EMPTY(ldPost)
    ldPost  = gdSysDate
    _CUROBJ = _CUROBJ
  ELSE
     SHOW GET ldPost    DISABLE  
     SHOW GET ldRcvDate ENABLE
     _CUROBJ = OBJNUM(ldRcvDate)
  ENDIF  
ENDIF  

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : get all the avialable warehouses from the warehous file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*!lfOldValue()
FUNCTION lfOldValue
lcOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : Valid function for get field lcTcode, 
*!             (material manufacturing order number)
*!*************************************************************
*! Calls     : gfMFGOrdBr()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOrder()
*!*************************************************************
FUNCTION lfvOrder

PRIVATE lnAlias,lcFbricCond
*HDM[START]
PRIVATE lnLinRec , lnLinCan , lnLinDam
STORE 0 TO lnLinRec , lnLinCan , lnLinDam
*HDM[end]
IF MDOWN() AND EMPTY(lcTcode) AND EMPTY(lcItem)
  lcTcode = SPACE(6)
  RETURN
ENDIF
lnAlias    = SELECT(0)

*-- lcFbricCond : Is a variable which is used as parameter for gfMFGOrdBr
  
lcFbricCond=IIF(EMPTY(lcItem),SPACE(7),lcItem)

*-- Call global validation for material MFG orders if the current 
*-- order number is changed or the  user want to browes .
IF llBrowse  .OR. !EMPTY(lcTCode)
  *-- If an order is invalid, not selected, return the old value,
  *-- if valid or [not valid but select one from brow] RETURN .T.
  *-- if not valid Browse only those orders with an 'O'pen or 'C'omplete status.
  *-- if Escap or cancle the browes (order heder browes) RETURN .F.
  
  *!C200254,1 ABD - remark the next line and check if we will
  *!C200254,1 ABD - browse the MFg Order Or Mfg Rework Order. [End]
  *IF gfMFGOrdBr(@lcTCode, lcFbricCond , [Status $ 'OCA'])
  *IF !SEEK(lcTCode,'MMFGORDD')
  IF IIF(llRwIssRec,gfRWOrdBr(@lcTCode, lcFbricCond , [Status $ 'OCA']),;
    gfMFGOrdBr(@lcTCode, lcFbricCond , [Status $ 'OCA']))
    *-- If an order is selected check for its lines in MMFGORDD file .
    IF (llRwIssRec .AND. !lfChekLn()) .OR. ;
       !SEEK(lcTCode,lcBfileLin)
       *!C200254,1 ABD - [End]
      *-- New meassage
      lcTCode=SPACE(6)
      SHOW GET lcTCode
      *- The lines for this XXX are missing ! cannot proceed.
      
      *!C200254,1 ABD - change the text up on the rewok case. [Begin]
      *=gfModalGen('TRM36138B42000','DIALOG','Order')
      lcMessage = IIF(llRwIssRec,'Rework Order','Order')
      =gfModalGen('TRM36138B42000','DIALOG',lcMessage)
      *!C200254,1 ABD - [End]
      
      _CUROBJ=OBJNUM(lcTCode)
      llBrowse = .F.
      SELECT(lnAlias)
      RETURN
    ENDIF &&end  for chech lines in MMFGORDD
    
    *!C200254,1 ABD - Select the current header file. [Begin]
    *IF UPPER(MMFGORDH.STATUS)='C'
    IF UPPER(EVAL(lcBFileHdr+'.Status')) = 'C'
      *!C200254,1 ABD - [End]
      *-- This Manufacturing Order is completely received. Do you want to continue <Yes> <No> ?
      *!C200254,1 ABD - change the text up on the rewok case. [Begin]
      *lnOption=gfModalGen('QRM36088B36001','DIALOG','This Manufacturing Order')
      lcMessage = IIF(llRwIssRec,'This Manufacturing Rework Order','This Manufacturing Order')
      lnOption=gfModalGen('QRM36088B36001','DIALOG',lcMessage)
      *!C200254,1 ABD - [End]
      
      IF lnOption = 2
        lcTCode=SPACE(6)
        _CUROBJ=OBJNUM(lcTCode)
        llBrowse = .F.
        SELECT(lnAlias)
        RETURN
      ENDIF
    ENDIF
    *--Browse lines to select from.
    IF lfBrowLines()
      *-- Get the selected line's information.
      =lfGetInfo()
      *-- MMFGORDD is the current alias and the record pointer is adjusted
      SCATTER MEMVAR MEMO
      m.TOTBAL=&lcTmpFile..nmfgtotqty
      *!C200254,1 ABD - get the warehouse from the current line file. [Begin]
      *m.cwarecode=IIF(!EMPTY(MMFGORDD.CWARECODE),MMFGORDD.CWARECODE,MMFGORDH.CWARECODE)
      m.cwarecode=IIF(!EMPTY(Eval(lcBfileLin+'.CWARECODE')),;
                             Eval(lcBfileLin+'.CWARECODE'),;
                             Eval(lcBfileHdr+'.CWARECODE'))
      *!C200254,1 ABD - [End]
      *HDM[START]
      M.TotStk = lnLinRec
      M.TotCan = lnLinCan
      M.TotDam = lnLinDam
      M.nMFgTotQty = M.nMFgTotQty - (M.TotStk + M.TotCan + M.TotDam)
      STORE 0 TO M.TotStk , M.TotCan , M.TotDam
      *HDM[END]
      INSERT INTO (lcTmpLine) FROM MEMVAR 
      *-- Update screen variables with the selected line valuse.
      =lfReadLine()
      *-- Disable the key & Enable the color field only . 
      STORE 'DISABLE' TO lcItemSt , lcOrderSt
      STORE 'ENABLE'  TO lcColorSt
      llBrowse    = .F.
      laScrMode   = .F.
      laScrMode[4]= .T.
      SHOW GETS
      SELECT(lnAlias)
      RETURN
    ELSE  && else  for if lfBrowLines()
       *lcTCode=SPACE(6)
      SELECT(lnAlias)
      _CUROBJ = OBJNUM(lcTCode)
      llBrowse = .F.
      SELECT(lnAlias)
      RETURN
    
    ENDIF
  ELSE   && Else if gfMFGOrdBr()
    
    lcTCode=SPACE(6)
    SELECT(lnAlias)
    _CUROBJ = OBJNUM(lcTCode)
    llBrowse = .F.
    SELECT(lnAlias)
    RETURN
  ENDIF        && Endif of if gfMFGOrdBr()       
    
ENDIF        && End IF llBrowse .OR. ...etc
STORE 0 TO lnLinRec , lnLinCan , lnLinDam
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvItem
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : Valid function for get field lcTcode, 
*!             (material manufacturing order number)
*!*************************************************************
*! Calls     : gfMFGOrdBr()
*!             lfSetLineInfo()
*!             lfRefresh() (through SHOW GETS command)
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvItem()
*!*************************************************************
FUNCTION lfvItem

PRIVATE lcxItem , lnAlias 

IF MDOWN() AND EMPTY(lcTcode) AND EMPTY(lcItem)
  RETURN
ENDIF
lnAlias = SELECT(0)
IF llBrowse .OR. !EMPTY(lcItem) 
  SELECT FABRIC 
  IF !SEEK(PADR(lcItem,7)) .OR. llBrowse .OR. '?' $ lcItem 
    *-- Browes all fabrics
    lcxItem = lcItem
    DO FABROW WITH lcxItem,'*' &&-- Parameter '*' means don not browes colors
    lcItem=lcxItem
  ENDIF
  STORE SPACE(6) TO lcTCode
  *-- Check the return value from all fabrics browes 

  *!C200254,1 ABD - remark the next line and check if we will
  *!C200254,1 ABD - browse the MFg Order Or Mfg Rework Order. [End]
  *IF !EMPTY(lcItem) .AND. gfMFGOrdBr(@lcTCode, lcItem, [Status $ 'OC'])
  *  IF !SEEK(lcTCode,'MMFGORDD') 
  IF !EMPTY(lcItem) .AND. IIF(llRwIssRec ,gfRWOrdBr(@lcTCode, lcItem, [Status $ 'OCA']),;
    gfMFGOrdBr(@lcTCode, lcItem, [Status $ 'OCA']))
    IF !SEEK(lcTCode,lcBFileLin) 
      *!C200254,1 ABD - [End]
      
      *- The lines for this XXX are missing ! cannot proceed.
      *-- New meassage
      lcTCode=SPACE(6)
      lcItem =SPACE(7)
      SHOW GET lcItem
      *!C200254,1 ABD - change the text up on the rewok case. [Begin]
      *=gfModalGen('TRM36138B42000','DIALOG','Order')
      lcMessage = IIF(llRwIssRec,'Rework Order','Order')
      =gfModalGen('TRM36138B42000','DIALOG',lcMessage)
      *!C200254,1 ABD - [End]
      SELECT(lnAlias)
      _CUROBJ=OBJNUM(lcItem)
      llBrowse = .F.
      RETURN
    ENDIF
    
    *!C200254,1 ABD - get the status from the current header file. [Begin]
    *IF UPPER(MMFGORDH.STATUS)='C'
    IF UPPER(&lcBFileHdr..Status)='C'
      *!C200254,1 ABD - [End]
      *-- This Manufacturing Order is completely received. Do you want to continue <Yes> <No> ?
      *!C200254,1 ABD - change the text up on the rewok case. [Begin]
      *lnOption=gfModalGen('QRM36088B36001','DIALOG','This Manufacturing Order')
      lcMessage = IIF(llRwIssRec,'This Manufacturing Rework Order','This Manufacturing Order')
      lnOption=gfModalGen('QRM36088B36001','DIALOG','This Manufacturing Order')
      *!C200254,1 ABD - [End]
      IF lnOption = 2
        lcTCode=SPACE(6)
        lcItem =SPACE(7)
        SELECT(lnAlias)
        _CUROBJ=OBJNUM(lcItem)
        llBrowse = .F.
        RETURN
      ENDIF
    ENDIF

    =lfBrowLines()

    SCATTER MEMVAR MEMO
    M.cRcvBy    = lcItem
    m.TOTBAL    = &lcTmpFile..nmfgtotqty
    *!C200254,1 ABD - get the warehose from the current line file. [Begin]
    *m.cwarecode = IIF(!EMPTY(MMFGORDD.CWARECODE),MMFGORDD.CWARECODE,MMFGORDH.CWARECODE)
    m.cwarecode = IIF(!EMPTY(&lcBFileLin..CWARECODE),&lcBFileLin..CWARECODE,&lcBFileHdr..CWARECODE)
    *!C200254,1 ABD - [End]
    INSERT INTO (lcTmpLine) FROM MEMVAR 
    =lfReadLine()
    *-- Disable the key & Enable the color field only . 
    STORE 'DISABLE' TO lcItemSt , lcOrderSt
    STORE 'ENABLE'  TO lcColorSt

    laScrMode   =.F.
    laScrMode[4]=.T.
    SHOW GETS
    llBrowse = .F.
    SELECT(lnAlias)
    RETURN
    
  
  ELSE &&--else IF !EMPTY(lcItem) .AND. gfMFGOrdBr(@lcTCode, lcItem, [Status $ 'OC'])
     STORE SPACE(7) TO lcItem
    _CUROBJ = _CUROBJ
  ENDIF
  llBrowse  = .F.  
  SELECT(lnAlias)
ENDIF   
*!*************************************************************
*! Name      : lfvColor
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : Valid function for Color get field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvColor()
*!*************************************************************
FUNCTION lfvColor
PRIVATE lnAlias , llModiOrd , lnOption

IF MDOWN()
  RETURN
ENDIF

lnAlias   = SELECT(0)
lcColor   = PADR(ALLTRIM(lcColor), 6)
llModiOrd = .F.

*-- Browse all valid fabric/Color, or validate an entry if it is changed.
IF llBrowse .OR. ('?' $ lcColor) .OR. lcOldVal <> lcColor 
  IF !EMPTY(lcColor) .AND. !SEEK(lcItem + lcColor, 'FABRIC')
    lcColor   = CHR(240) &&-- to be sure that FABROW() will operate correctily
    *-- Cancel from Fabric/Color Browse
    IF !FABROW(@lcItem, @lcColor, .T.) 
      lcColor   = lcOldVal
    ENDIF
  ENDIF
  *-- Now we have a color associated to this fabric 
  *-- the first source of it is : the orignal color which we selected from
  *-- lfvOrder or lfvItem and inserted it in the templine file
  *-- the second source of it is : from fabric/color browse (NEW COLOR)
ENDIF  


IF EMPTY(lcColor)
  *--The Color Field can't be empty
  =gfModalGen('TRM36094B36000','ALERT','Color') 
  lcColor = lcOldVal
  _CUROBJ = _CUROBJ
  RETURN  
ENDIF

*-- If an item/color is selected from Fabric/Color Browse
*-- check if it belongs to the current order.
*-- If the item is not on the current MFG order, inform the user
*-- and get an option whether to Modify, Browse, or Reenter.
*!C200254,1 ABD - Select the correct line file. [Begin]
*SELECT MMFGORDD
*SET ORDER TO TAG MMFGORDD

SELECT (lcBFileLin)
IF llRwIssRec
  SET ORDER TO TAG MARWLIN
ELSE
  SET ORDER TO TAG MMFGORDD
ENDIF
*!C200254,1 ABD - [End]

SELECT(lnAlias)
IF lcOldVal <> lcColor

  *-- The next block of code until ( )  is the new color aria  

  *!C200254,1 ABD - Seek in the correct header file. [Begin]
  *IF !SEEK(lcTcode + lcItem + lcColor, 'MMFGORDD')
  IF !SEEK(lcTcode + lcItem + lcColor,lcBFileLin)
    *!C200254,1 ABD - [End]
    *-- New meassage and Butt
    *-- Item/Color XXX / XXX is not on the manufacturing order .  Would you like to modify the manufacturing order ?
    lnOption =gfModalGen('QRM36139B36011','DIALOG',lcItem+'|'+lcColor)

    DO CASE   
      *-- Modify case
      CASE lnOption = 1
        SELECT (lcTmpLine) 
        SCATTER MEMVAR MEMO
        IF llWareHous
          *!C200254,1 ABD - Geth the correct warehouse from the current line file. [Begin]
          *lccWareCode  = IIF(SEEK(MMFGORDH.cWareCode, 'WAREHOUS'),;
                                  MMFGORDH.cWareCode, 'DEFDEF')

          lccWareCode  = IIF(SEEK(&lcBFileHdr..cWareCode, 'WAREHOUS'),;
                                  &lcBFileHdr..cWareCode, 'DEFDEF')
           *!C200254,1 ABD - [End]
        ELSE
          GO TOP IN WAREHOUS
          *!C200254,1 ABD - Geth the correct warehouse from the current line file. [Begin]
          *lccWareCode  = MMFGORDH.cWareCode
          lccWareCode  = &lcBFileHdr..cWareCode
          *!C200254,1 ABD - [End]
        ENDIF  
        

        lcDyelot    = SPACE(10)
        *-- Clear estimated cost fields in case they already have a value.
        STORE 0 TO m.nCost1    , m.nCost2    , m.nCost3    , m.nCost4    ,;
                   m.nLan_Cost1, m.nLan_Cost2, m.nLan_Cost3, m.nLan_Cost4,;
                   m.nAct_Cost1, m.nAct_Cost2, m.nAct_Cost3, m.nAct_Cost4,;
                   m.NmfgtotQty,m.TOTBAL           

        m.Dyelot    = lcDyelot
        m.Cwarecode = lccWareCode
        m.Color     = lcColor
        m.reference = SPACE(30)
        *!C200254,1 ABD - Geth the correct data from the current header file. [Begin]
        *lnPolstln   = MAX(lnPolstln+1,MMFGORDH.lastline+1)
        lnPolstln   = MAX(lnPolstln+1,&lcBFileHdr..lastline+1)
        *!C200254,1 ABD - Geth the correct warehouse from the current line file. [Begin]
        m.Lineno    = lnPolstln
        *HDM[START]
        M.TotStk = 0
        M.TotCan = 0
        M.TotDam = 0
        *HDM[END]
        *-- this field in temp indecates that this line is not in oreginal order .
        m.Newline   = .T.
        GATHER MEMVAR MEMO
        RELEASE m.nCost1     , m.nCost2     , m.nCost3     , m.nCost4     ,;
                m.nLan_Cost1 , m.nLan_Cost2 , m.nLan_Cost3 , m.nLan_Cost4 ,;
                m.nAct_Cost1 , m.nAct_Cost2 , m.nAct_Cost3 , m.nAct_Cost4 ,;
                m.NmfgtotQty , m.TOTBAL     , m.Dyelot     , m.Cwarecode  ,;
                m.Color      , m.reference  , m.Lineno     , m.Newline                     
        
        =lfReadLine()
        *!C200254,1 ABD - Call the lpContral WITH .F in case Issue Rework. [Begin]
        *DO lpControl WITH llUseDyelot
        IF llRwIssRec .AND. lcOprtType = 'I'
          DO lpControl WITH .F.
        ELSE
          DO lpControl WITH llUseDyelot
        ENDIF  
        *!C200254,1 ABD - [End]

        
        
        SHOW WINDOW (lcBrowTitl) REFRESH SAME
        *HDM[START]
        *=lpControl()
        *HDM[END]

      CASE lnOption = 2 .AND. !lfBrowLines() .OR. lnOption = 3
           
        lcColor    = lcOldVal
        _CUROBJ    = OBJNUM(lcColor)

      OTHERWISE
        *-- lnOption = 2 .AND. lfBrowLines()
        *!C200254,1 ABD - Geth the correct data from the current detail file. [Begin]
        *lcColor=MMFGORDD.color
        lcColor = &lcBFileLin..color
        *!C200254,1 ABD - [End]
        *-- if this is another line from line file
        IF lcOldVal <> lcColor
          *!C200254,1 ABD - Geth the correct data from the current detail file. [Begin]
          *SELECT MMFGORDD  
          SELECT (lcBFileLin)
          *!C200254,1 ABD - [End]
          
          SCATTER MEMVAR MEMO
          SELECT (lcTmpLine)
          GATHER MEMVAR MEMO
          =lfReadLine()
          
          *!C200254,1 ABD - Call the lpContral WITH .F in case Issue Rework. [Begin]
          *DO lpControl WITH llUseDyelot
          IF llRwIssRec .AND. lcOprtType = 'I'
            DO lpControl WITH .F.
          ELSE
            DO lpControl WITH llUseDyelot
          ENDIF  
          *!C200254,1 ABD - [End]
          SHOW WINDOW (lcBrowTitl) REFRESH SAME
          =lfwBrow()
        ENDIF
    ENDCASE
  
  ELSE &&-- elseif !SEEK(lcTcode + lcItem + lcColor, 'MMFGORDD')
    *-- this color is a line in this order
    *-- Save the record pointer of MMFGORDD
    *!C200254,1 ABD - Geth the correct data from the current detail file. [Begin]
    *SELECT MMFGORDD
    SELECT (lcBfileLin)
    *!C200254,1 ABD - [End]
    lnCurRec = RECNO()
    *-- Locate rest for another record with the same item/color
    *-- in the same order
    SKIP
    *!C200254,1 ABD - Geth the correct data from the current detail file. [Begin]
    *LOCATE REST WHILE cMFGOrdNo   + cFabric   + Color = ;
                      lcTcode + lcItem + lcColor ;
                      FOR TranCd = '1'
    LOCATE REST WHILE &lcComField + cFabric   + Color = ;
                      lcTcode + lcItem + lcColor ;
                      FOR TranCd = '1'
    *!C200254,1 ABD - [End]
    *-- Restore the record pointer.
    GO lnCurRec
    IF FOUND() 
      IF !lfBrowLines(lcColor) 
        lcColor    = lcOldVal
        _CUROBJ    = OBJNUM(lcColor)
      ENDIF
    ELSE
      SCATTER MEMVAR MEMO
      SELECT (lcTmpLine)
      GATHER MEMVAR MEMO
      =lfReadLine()
      DO lpControl WITH llUseDyelot
      
      *!C200254,1 ABD - Call the lpContral WITH .F in case Issue Rework. [Begin]
      *DO lpControl WITH llUseDyelot
      IF llRwIssRec .AND. lcOprtType = 'I'
        DO lpControl WITH .F.
      ELSE
        DO lpControl WITH llUseDyelot
      ENDIF  
      *!C200254,1 ABD - [End]
      
      SHOW WINDOW (lcBrowTitl) REFRESH SAME
      =lfwBrow()
    ENDIF  
  ENDIF &&-- endif !SEEK(lcTcode + lcItem + lcColor, 'MMFGORDD')      

ELSE  &&-- else IF lcOldVal <> lcColor
  *!C200254,1 ABD - Geth the correct data from the current detail file. [Begin]
  *llUseDyelot = !EMPTY(MMFGORDD.Dyelot)
  llUseDyelot = !EMPTY(&lcBFileLin..Dyelot)  
  *!C200254,1 ABD - [End]
  
  *!C200254,1 ABD - Call the lpContral WITH .F in case Issue Rework. [Begin]
  *DO lpControl WITH llUseDyelot
  IF llRwIssRec .AND. lcOprtType = 'I'
    DO lpControl WITH .F.
  ELSE
    DO lpControl WITH llUseDyelot
  ENDIF  
  *!C200254,1 ABD - [END]

  

ENDIF  &&-- endif IF lcOldVal <> lcColor
  *HDM[START]
 
 *!C200254,1 ABD - no need to get the opration in case the reowk order. [Begin]
  *IF !llUseDyelot
  IF !llUseDyelot .AND. !llRwIssRec
    *!C200254,1 ABD - [END]
    *!C200254,1 ABD - Geth the correct data from the current header file. [Begin]
    *REPLACE clastopr WITH MMFGORDH.clastopr
    REPLACE clastopr WITH &lcBFileHdr..clastopr
    *!C200254,1 ABD - [End]
    lcLotNo = lfSelLots()
    IF !EMPTY(lcLotNo)
      SELECT (lcTmpLine)
      REPLACE CLOTNO WITH lcLotNo
    ENDIF
  ENDIF
  *HDM[END]

*!C200254,1 ABD - select the correct current detail file. [Begin]
*SELECT MMFGORDD
SELECT (lcBFileLin)
*!C200254,1 ABD - [End]

SET ORDER TO TAG MMFGORDDLN
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvDyelot 
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : validate the dyelot field
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvDyelot 
PRIVATE lcDetOrd , lnRec
lcDyelot = PADR(ALLTRIM(lcDyelot), 10)
*-- Check for not empty of dyelot field .
IF lcOldVal <> lcDyelot
  *!C200254,1 ABD - Select Cuurent correct alias . [Begin]
  *SELECT MMFGORDD
   SELECT (lcBfileLin)
  *!C200254,1 ABD - [End]
  
  lcDetOrd = ORDER()
  *!C200254,1 ABD - Select the correct Order for current file. [Begin]
  IF llRwIssRec
    SET ORDER TO TAG MARWLIN
  ELSE
  *!C200254,1 ABD - [End]
    SET ORDER TO Mmfgordd
    *!C200254,1 ABD - End If for If statment. [Begin]
  ENDIF
  *!C200254,1 ABD - [End]

  
  *lnRec = RECNO('MMFGORDD')
  *!C200254,1 ABD - get the correct current field. [Begin]
  *llNwClr = !(SEEK(&lcTmpLine..cmfgordno + &lcTmpLine..cfabric+&lcTmpLine..COLOR))
  *IF !SEEK(&lcTmpLine..cmfgordno + &lcTmpLine..cfabric+&lcTmpLine..COLOR+PADR(lcDyelot,10),'MMFGORDD')

  llNwClr = !(SEEK(&lcTmpLine..&lcComField. + &lcTmpLine..cfabric+&lcTmpLine..COLOR))
  IF !SEEK(&lcTmpLine..&lcComField. + &lcTmpLine..cfabric+&lcTmpLine..COLOR+PADR(lcDyelot,10),lcBfileLin)
    *!C200254,1 ABD - [End]
    *-- New Dyelot for receiving
    SELECT(lcTmpLine)
    *!C200254,1 ABD - update with correct field. [Begin]
    *lnPolstln   = MAX(lnPolstln+1,MMFGORDH.lastline+1)
    lnPolstln   = MAX(lnPolstln+1,&lcBFileHdr..lastline+1)            
    *REPLACE DYELOT     WITH lcDyelot  ,;
            TRANCD     WITH '1'       ,;
            LINENO     WITH lnPolstln ,;
            lNewDyelot WITH !llNwClr  ,;
            NewLine    WITH .T.       ,;
            NMFGTOTQTY WITH 0         ,;
            TOTBAL     WITH 0
    REPLACE DYELOT     WITH lcDyelot  ,;
            TRANCD     WITH lcBrTrncd ,;
            LINENO     WITH lnPolstln ,;
            lNewDyelot WITH !llNwClr  ,;
            NewLine    WITH .T.       ,;
            NMFGTOTQTY WITH 0         ,;
            TOTBAL     WITH 0

    *!C200254,1 ABD - [End]            
  ELSE
    REPLACE Dyelot WITH lcDyelot
  ENDIF
  *GO lnRec IN MMFGORDD
  *!C200254,1 ABD - Set order to current file. [Begin]
  *SET ORDER TO TAG &lcDetOrd IN MMFGORDD
  SELECT (lcBfileLin)
  SET ORDER TO TAG &lcDetOrd
  SELECT(lcTmpLine)
  *!C200254,1 ABD - [End]
  
  SHOW WINDOW (lcBrowTitl) REFRESH SAME
  SELECT(lcTmpLine)
ENDIF

IF EMPTY(lcDyelot) 
  =gfModalGen('TRM36094B36000','ALERT','dyelot') 
  _CUROBJ = OBJNUM(lcDyelot)
ELSE
  
  *!C200254,1 ABD - get last oration in case not rework order. [Begin]
  IF !llRwIssRec
    *!C200254,1 ABD - [End]
    REPLACE clastopr WITH MMFGORDH.clastopr
    lcLotNo = lfSelLots()
    IF !EMPTY(lcLotNo)
      SELECT (lcTmpLine)
      REPLACE CLOTNO WITH lcLotNo
    ENDIF
      *!C200254,1 ABD - End IF For IF Statment. [Begin]
  ENDIF
  *!C200254,1 ABD - [End]
  
  DO lpControl WITH .F.
ENDIF
*!*************************************************************
*! Name      : lfBrowLines
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : Browse all lines for an order number.
*!*************************************************************
*! Calls     : lpGetOpen
*!             AriaBrow()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBrowLines()
*!*************************************************************
FUNCTION lfBrowLines
PARAMETERS lcForColor

DO lpGetOpen

PRIVATE lcBrFields, lnCurAlias
lcBrFields = [cFabric  :H='Item',]   +;
             [Color    :H='Color',]   +; 
             IIF(llWareHous,  [cWareCode : H = 'Warehouse',], []) +;
             IIF(llDyelot, [Dyelot    : H = 'Dyelot',], [])    +;               
             [&lcTmpFile..nmfgtotqty :R :H='Open' :10,]+;                 
             [nmfgtotqty   :H='Quantity',]+;
             [Reference:H='Reference']

lcForColor = IIF(EMPTY(lcForColor), [], m.cFabric + lcForColor)

*!C200254,1 ABD - Select the correct current detail file. [Begin]
*SELECT MMFGORDD
SELECT (lcBfileLin)
*!C200254,1 ABD - [End]

lcFields   = [cfabric,color,pattern,dyelot,cwarecode]

*!C200254,1 ABD - browse the correct data from the current detail file that
*!C200254,1 ABD - Will show the correct trancd . [Begin]
IF llRwIssRec
  lctrancd  = IIF(lcOprtType = 'I','1','2')
  RETURN AriaBrow(['] + lcTcode + lcForColor + ['  FOR TranCd = lctrancd ],;
              'Order # '+ lcTcode + ' Lines',;
              gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','',lcFields,'laTemp') 
ELSE
  RETURN AriaBrow(['] + lcTcode + lcForColor + ['  FOR TranCd = '1'],;
              'Order # '+ lcTcode + ' Lines',;
              gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','',lcFields,'laTemp') 
ENDIF
            
*!C200254,1 ABD - [End]
*!*************************************************************
*! Name      : lfvNewLn
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : Valid function for Color get field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************

FUNCTION lfvNewLn
*-- Get previus po no.
lcPrvTPO = lcTCode
*--Clear Line.
=lfReadLine(.T.)
STORE 'DISABLE' TO lcPatrenSt,lcDyelotSt,lcNewSt,;
                   lcWareSt,lcReferSt,lcEditQSt,lcRemoveSt
STORE 'ENABLE'  TO lcOrderSt,lcItemSt,lcColorSt
DO lpShowScrn

*-- WALID
IF EMPTY(cRcvBy)
  lcItem  = SPACE(7)
  lcTCode = lcPrvTPO
  _CUROBJ = OBJNUM(lcTCode)
ELSE
  lcItem  = cRcvBy
  lcTCode = SPACE(6)
  _CUROBJ = OBJNUM(lcItem)
ENDIF

*!*************************************************************
*! Name      : lfvRemLn
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************

FUNCTION lfvRemLn
*-Are you sure you want to delete this line?
*-- old meassage
IF gfModalGen('QRM34036B42002','DIALOG') = 1
  SELECT (lcTmpLine)
  
  *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
  *lcLKey     = cmfgordno+cfabric+color+dyelot+cWarecode+STR(lineno,6)
  *IF SEEK(lcLKey+'1')
  lcLKey     = &lcComField+cfabric+color+dyelot+cWarecode+STR(lineno,6)  
  lclTrnType = IIF(llRwIssRec .AND. lcOprtType = 'R','2','1')  
  IF SEEK(lcLKey+lclTrnType)
    *!C200254,1 ABD - [End]
    lnTotStk = lnTotStk-TotStk
    lnTotDam = lnTotDam-TotDam
    lnTotCan = lnTotCan-TotCan
    DELETE
  ENDIF
  
  IF SEEK(lcLKey + lcCanlTrCd)
    DELETE
  ENDIF
  
  IF SEEK(lcLKey + lcOthrTrCd)
    *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
    *DELETE REST WHILE cmfgordno+cfabric+color+dyelot+cWarecode+STR(Lineno,6)+Trancd=lcLKey+lcOthrTrCd
    DELETE REST WHILE &lcComField+cfabric+color+dyelot+;
                     cWarecode+STR(Lineno,6)+Trancd=lcLKey+lcOthrTrCd
    *!C200254,1 ABD - [End]
    
  ENDIF
  
  *!C200254,1 ABD - remark the next line and get the correct Type. [Begin]
  *IF SEEK(lcLKey + '2')
  lclTrnType = IIF(llRwIssRec .AND. lcOprtType = 'R','3','2')  
  IF SEEK(lcLKey + lclTrnType)
    *!C200254,1 ABD - [End]
    DELETE
  ENDIF
  
  GO TOP
  IF EOF()
    =lfReadLine(.T.)
    STORE 'DISABLE' TO lcOrderSt,lcItemSt,lcColorSt,lcPatrenSt,lcDyelotSt,;
                       lcWareSt,lcReferSt,lcEditQSt,lcRemoveSt
    STORE 'ENABLE'  TO lcNewSt
  ELSE
    =lfReadLine(.F.)
  ENDIF
  DO lpShowScrn  
  SHOW WINDOW (lcBrowTitl) REFRESH SAME
  =lfwBrow()
ENDIF


*!*************************************************************
*! Name      : lfvWare
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :=lfvWare()
*!*************************************************************
FUNCTION lfvWare
*-- lnWare : Assigned form lfReadLine()

IF (lnWare = lcOldVal) &&---OR (lcOldVal = 0) 
  RETURN
ENDIF  

lcWareCode = SUBSTR(laWare[lnWare],1,6)
REPLACE &lcTmpLine..cWareCode WITH lcWareCode

IF lfChkLine()
  *-- old meassage
  *-This XXXX has been entered on this XXXX.
  =gfModalGen('TRM42107B42000','DIALOG','Item/Color'+'|'+'receiving' )
  lnWare  = lcOldVal
  SHOW GET lnWare
  IF lnWare > 0
    REPLACE &lcTmpLine..cWareCode WITH SUBSTR(laWare[lnWare],1,6)
  ELSE
    REPLACE &lcTmpLine..cWareCode WITH ''
  ENDIF
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

*-- Search this value in fabdye file
IF !SEEK(lcItem+lcColor+lcWareCode+SPACE(10),'FABDYE')
  *-- old meassage by samah
  *-Item/Color xxx is not assigned to Location: xxx. "\<Add;\<Reenter"
  IF gfModalGen('QRM36049B42006','DIALOG',ALLTRIM(lcItem)+'/'+ALLTRIM(lcColor)+'|'+lcWareCode) = 1
    
    DO gpAdFabWar WITH lcItem,lcColor,SPACE(10),lcWareCode,lcTempName
    lcWareCode=SUBSTR(laWare[lnWare],1,6)
    IF llWareLoc
      =lfvLocWare()
    ENDIF
  ELSE
    lnWare = lcOldVal
    lcWareCode=SUBSTR(laWare[lnWare],1,6)
    SHOW GET lnWare
    REPLACE &lcTmpLine..cWareCode WITH lcWareCode
    RETURN
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvRefer
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : Valid function for Color get field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvRefer()
*!*************************************************************

FUNCTION lfvRefer
lcRefer = PADR(ALLTRIM(lcRefer), 30)
IF lcOldVal <> lcRefer 
  REPLACE reference WITH lcRefer
  SHOW WINDOW (lcBrowTitl) REFRESH SAME
ENDIF
*!*************************************************************
*! Name      : lfvPattren
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : Valid function for pattern get field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvPattren()
*!*************************************************************

FUNCTION lfvPattren
lcPatren = PADR(ALLTRIM(lcPatren), 10)
IF lcOldVal <> lcPatren
  REPLACE Pattern WITH lcPatren
  SHOW WINDOW (lcBrowTitl) REFRESH SAME
ENDIF

*:*************************************************************
*! Name     : lfBrow
*! Developer: Walid Abou El-Magd 
*! Date     : 07/01/1999               
*! Purpose  : Activate screen browse.
*:*************************************************************
FUNCTION lfBrow

SELECT (lcTmpLine)
lnRecNO=RECNO(lcTmpLine)
*!C200254,1 ABD - remark the next line and get the correct field. [Begin]
*lcBrwFlds =  [cMarker = IIF(lnMarker = RECNO(lcTmpLine),'>',' ') :H=' ',]+;
             [Cmfgordno:9 :H='Order#',]+;
             [Cfabric :9 :H='Item',]+;
             [Color :7 :H='Color' ,]+;
             IIF(llDyelot,[Dyelot :R :15,],"")+;
             IIF(llWareHous ,[cWareCode :H='Location':10,],"")+;
             [Pattern :10 :H='Pattern',]+;
             [nMFGTOTQTY  :R :H='Original':8,]+;
             [TotStk :8 :H='Stock',]+;
             [TotDam :8 :H='Other',]+;
             [TotCan :8 :H='Cancel',]+;
             [nTotBal = MAX(nMFGTOTQTY - TotStk - TotDam - TotCan,0) :R :H='Balance' :8,]+;
             [Reference :19]
*BROWSE FIELDS &lcBrwFlds      ; 
           FOR TRANCD='1'     ;
      	   SAVE               ;
       	   NOWAIT             ;
           NOAPPEND           ;
           NOCLEAR            ;
           NOEDIT             ;
           NOMENU             ;  
           NODELETE           ;
           WHEN lfwBrow()     ;
           VALID :F lfvBrow() ;         
    WINDOW (lcWinCh2)         ;
    IN WINDOW (gcBaseWind)    ; 
    TITLE  (lcBrowTitl)

lcBrwFlds =  [cMarker = IIF(lnMarker = RECNO(lcTmpLine),'>',' ') :H=' ',]+;
             [&lcComField:9 :H='Order#',]+;
             [Cfabric :9 :H='Item',]+;
             [Color :7 :H='Color' ,]+;
             IIF(llDyelot,[Dyelot :R :15,],"")+;
             IIF(llWareHous ,[cWareCode :H='Location':10,],"")+;
             [Pattern :10 :H='Pattern',]+;
             [nMFGTOTQTY  :R :H='Original':8,]+;
             [TotStk :8 :H='Stock',]+;
             [TotDam :8 :H='Other',]+;
             [TotCan :8 :H='Cancel',]+;
             [nTotBal = MAX(nMFGTOTQTY - TotStk - TotDam - TotCan,0) :R :H='Balance' :8,]+;
             [Reference :19]


BROWSE FIELDS &lcBrwFlds      ; 
           FOR TRANCD=lcBrTrncd;
      	   SAVE               ;
       	   NOWAIT             ;
           NOAPPEND           ;
           NOCLEAR            ;
           NOEDIT             ;
           NOMENU             ;  
           NODELETE           ;
           WHEN lfwBrow()     ;
           VALID :F lfvBrow() ;         
    WINDOW (lcWinCh2)         ;
    IN WINDOW (gcBaseWind)    ; 
    TITLE  (lcBrowTitl)
*!C200254,1 ABD - [End]
IF BETWEEN(lnRecNO,1,RECCOUNT(lcTmpLine))
  GOTO lnRecNO
ENDIF



*!*************************************************************
*! Name      : lfwBrow
*! Developer: Walid Abou El-Magd
*! Date     : 07/01/1999
*! Purpose   : When valid function for browse.
*!*************************************************************
FUNCTION lfwBrow
*HDM[START]
*PRIVATE lcDyeSt
*lcDyeSt = IIF(EMPTY(DYELOT),'DISABLE','ENABLED')
*HDM[END]

lnMarker   = RECNO(lcTmpLine)
glFromBrow = .T.
=lfReadLine(EOF()) 
SHOW GET lcColor
SHOW GET lcPatren
*HDM[START]
*SHOW GET lcDyelot
*SHOW GET lcDyelot &lcDyeSt
*HDM[END]
SHOW GET lnWare
SHOW GET lcRefer
=lfRefresh(lcWinCh1)  &&-- Refresh the say field .
=lfRefresh(lcWinCh3)  &&-- Refresh the say field .
SHOW WINDOW (lcBrowTitl) REFRESH SAME
*!*************************************************************
*! Name      : lfvBrow
*! Developer: Walid Abou El-Magd
*! Date     : 07/01/1999               
*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() 
*!             function
*!*************************************************************
FUNCTION lfVBrow
IF WONTOP() # (lcBrowTitl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*!*************************************************************
*! Name               : lpShow
*! Developer          : Walid Abou El-Magd
*! Date               : 07/01/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : lfRefresh() , lfBrowse() AND lfwBrow()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpShow
*!************************************************************
*!
PROCEDURE lpShow

DO CASE
  *-- S E L E C T  M O D E.
  CASE laScrMode[1]
    STORE ''  TO lcClrDesc,lcPatRen,lcTCode,lcColor,lcItem,lcDyelot,lcStyDesc
    STORE 'DISABLE' TO lcOrderSt,lcItemSt,lcColorSt,lcPatrenSt,lcDyelotSt,;
                   lcWareSt,lcReferSt,lcEditQSt,lcNewSt,lcRemoveSt
    IF llDyelot
      *!C200254,1 ABD - Show get the dyelot in case receive only not issue. [Begin]
      IF !llRwIssRec .OR. lcOprtType = 'R'
        *!C200254,1 ABD - [End]
        SHOW GET lcDyelot &lcDyelotSt
        *!C200254,1 ABD - End IF for If statment . [Begin]
      ENDIF
      *!C200254,1 ABD - [End]
    ENDIF
    
    *!C200254,1 ABD - Disable Field Warehouse in case Rewok. [Begin]
    IF llRwIssRec 
      SHOW GET lnWare   DISABLE
    ELSE
      SHOW GET lnWare   &lcWareSt
    ENDIF
    *!C200254,1 ABD - [End]
    
    SHOW GET lcPatRen &lcPatrenSt 

    SHOW GET pbEditQ  &lcEditQSt
    SHOW GET pbNew    &lcNewSt
    SHOW GET pbRemove &lcRemoveSt
    
    SHOW GET ibItem   &lcItemSt
    SHOW GET lcItem   &lcItemSt     

    SHOW GET ibTCode &lcOrderSt
    SHOW GET lcTCode &lcOrderSt
    
    SHOW GET ibColor &lcColorSt    
    SHOW GET lcColor &lcColorSt


   SHOW GET lcRefer  &lcReferSt


    IF llLinkToGl
      SHOW GET ldPost  ENABLE
      SHOW GET ldRcvDate DISABLE       
      _CUROBJ=OBJNUM(ldPost)
    ELSE
       SHOW GET ldRcvDate ENABLE       
      _CUROBJ=OBJNUM(ldRcvDate)      
    ENDIF

  *-- A D D  M O D E. &&-- IF CANCEL GO SELECT .
  CASE laScrMode[4]  

    llCUpDate  = .T.
    IF llDyelot 
      *!C200254,1 ABD - Show get the dyelot in case receive only not issue. [Begin]
      IF !llRwIssRec .OR. lcOprtType = 'R'
        *!C200254,1 ABD - [End]
        SHOW GET lcDyelot 
        SHOW GET lcDyelot &lcDyelotSt
        *!C200254,1 ABD - End IF For IF statment. [Begin]
      ENDIF
      *!C200254,1 ABD - [End]
      
    ENDIF

    *!C200254,1 ABD - Disable Field Warehouse in case Rewok. [Begin]
    IF llRwIssRec 
      SHOW GET lnWare   DISABLE
    ELSE
      SHOW GET lnWare   &lcWareSt
    ENDIF
    *!C200254,1 ABD - [End]
    
    SHOW GET lcPatRen &lcPatrenSt 

    SHOW GET pbEditQ  &lcEditQSt
    SHOW GET pbNew    &lcNewSt
    SHOW GET pbRemove &lcRemoveSt

    SHOW GET ibItem   &lcItemSt
    SHOW GET lcItem   &lcItemSt
    
    SHOW GET ibTCode  &lcOrderSt
    SHOW GET lcTCode  &lcOrderSt

    SHOW GET ibColor &lcColorSt    
    SHOW GET lcColor &lcColorSt

    IF llLinkToGl
      SHOW GET ldPost DISABLE 
    ENDIF
    SHOW GET ldRcvDate DISABLE
    *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
    IF llRwIssRec
      SHOW GET lnType DISABLE
    ENDIF
    *!C200254,1 ABD - [End]
ENDCASE

=lfBrow() AND lfwBrow()
*!*************************************************************
*! Name               : lpTrap
*! Developer          : Walid Abou El-Magd
*! Date               : 07/01/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpTrap
*!*************************************************************
*!
PROCEDURE lpTrap
PARAMETERS Win_Name,UNTRAP_KEY
ON KEY LABEL &UNTRAP_KEY        &&-- Avoiding infinit loop by untrap
ACTIVATE WINDOW &Win_Name
*!*************************************************************
*! Name               : lfDeact
*! Developer          : Walid Abou El-Magd
*! Date               : 07/01/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDeact()
*!*************************************************************
*!
FUNCTION lfDeact
*-- Trap keys in this function
IF WONTOP()=(lcBrowTitl)
  glFromBrow = .T.
  ON KEY LABEL TAB        DO lpTrap WITH (lcWinCh3),'TAB'
  ON KEY LABEL BACKTAB    DO lpTrap WITH (lcWinCh1),'BACKTAB'
  ON KEY LABEL ESCAPE     LLDUMMY1=.T.
ENDIF
RETURN .F.
*!*************************************************************
*! Name               : lfAct
*! Developer          : Walid Abou El-Magd
*! Date               : 07/01/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAct()
*!*************************************************************
*!
FUNCTION lfAct
*-- Untrap keys in this function
IF WONTOP() # (lcBrowTitl)  
  IF glFromBrow
    ON KEY LABEL TAB
    ON KEY LABEL BACKTAB
    *ON KEY LABEL ESCAPE
    ON KEY LABEL ESC DO gfEscap
    =gfStopBrow()
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfGetTmpFile
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : To create all the uncompete session temprory files
*!             that is needed by this program.
*!*************************************************************
*! Call      : gfCrtTmp()
*!*************************************************************
*!
FUNCTION lfCrtUnComp

*!C200254,1 ABD - Create the file from the correct used file.[Begin]
*SELECT MMFGORDD
SELECT (lcBFileLin)
*!C200254,1 ABD - [End]

=AFIELDS(laFStru)
lnFStru = ALEN(laFStru,1)

DIMENSION laFStru[lnFStru+8,4]


laFStru[lnFStru+1,1] = 'TOTSTK'
laFStru[lnFStru+2,1] = 'TOTDAM'
laFStru[lnFStru+3,1] = 'TOTCAN'  
laFStru[lnFStru+4,1] = 'TOTBAL'
laFStru[lnFStru+5,1] = 'NEWLINE'
laFStru[lnFStru+7,1] = 'CLASTOPR'
laFStru[lnFStru+8,1] = 'LNEWDYELOT'

STORE 'N' TO laFStru[lnFStru+1,2],laFStru[lnFStru+2,2],;
             laFStru[lnFStru+3,2],laFStru[lnFStru+4,2]



STORE 'L' TO laFStru[lnFStru+5,2] , laFStru[lnFStru+8,2]

*B803075,1 [START] enlarge these fields width
*STORE  6  TO laFStru[lnFStru+1,3],laFStru[lnFStru+2,3],;
             laFStru[lnFStru+3,3],laFStru[lnFStru+4,3] , laFStru[lnFStru+7,3]


STORE  9  TO laFStru[lnFStru+1,3],laFStru[lnFStru+2,3],;
             laFStru[lnFStru+3,3],laFStru[lnFStru+4,3] , laFStru[lnFStru+7,3]

*B803075,1 [End]
STORE  1  TO laFStru[lnFStru+5,3] , laFStru[lnFStru+8,3]
*HDM[START] Add Decimals to the structure

STORE 'C' TO laFStru[lnFStru+7,2]

STORE  3  TO laFStru[lnFStru+1,4],laFStru[lnFStru+2,4],;
             laFStru[lnFStru+3,4],laFStru[lnFStru+4,4]

*STORE  0  TO laFStru[lnFStru+1,4],laFStru[lnFStru+2,4],;
             laFStru[lnFStru+3,4],laFStru[lnFStru+4,4],;
             laFStru[lnFStru+5,4]

STORE  0  TO laFStru[lnFStru+5,4] , laFStru[lnFStru+7,4] ,laFStru[lnFStru+8,4]

*HDM[END]

laFStru[lnFStru+6,1] = 'cRcvBy'
laFStru[lnFStru+6,2] = 'C'
laFStru[lnFStru+6,3] = 7
laFStru[lnFStru+6,4] = 0

*--Index tags array.
DIMENSION laTags[1,2]

*!C200254,1 ABD - remark the next line and get the correct field. [Begin]
*laTags[1,1]='cmfgordno+cfabric+color+dyelot+cWarecode+STR(lineno,6)+trancd'
laTags[1,1]='&lcComField+cfabric+color+dyelot+cWarecode+STR(lineno,6)+trancd'
*!C200254,1 ABD - [End]

laTags[1,2]='TmpLine1'
=gfCrtTmp(lcTmpLine,@laFStru,@laTags)

SELECT (lcTmpLine)
SET ORDER TO TAG TmpLine1

*!C200254,1 ABD - remark the next line and get the correct field. [Begin]
*SET RELATION TO cfabric+cmfgordno INTO MMFGORDH ADDITIVE
*SET RELATION TO cmfgordno+STR(lineno,6)+trancd INTO MMFGORDD ADDITIVE
SET RELATION TO cfabric+&lcComField INTO MMFGORDH ADDITIVE
SET RELATION TO &lcComField+STR(lineno,6)+trancd INTO MMFGORDD ADDITIVE
*!C200254,1 ABD - [End]

SET RELATION TO cfabric+color INTO FABRIC ADDITIVE

*--Warehouse location tmp file.
IF llWareLoc 
  SELECT WhsLoc
  =AFIELDS(laFStru)
  =gfCrtTmp(lcTemLoc,@laFStru,'Style+Color+cWareCode+cLocation',lcTemLoc)
ENDIF  

*!*************************************************************
*! Name      : lpGetOpen
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : Calaulate prv. receive if any per each line
*!           : and brows it in the screen from which we select the
*!           : style/colors in a cutting ticket and PO
*!*************************************************************
*! Called from : lfvCuttkt & lfvPo
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpGetOpen
*!*************************************************************
*!
PROCEDURE lpGetOpen

PARAMETER llFullKey
PRIVATE lnRecNo , lnFilePos , lcMastFile , lcScanCond , lcTrancd
STORE 0 TO lnLinRec , lnLinCan , lnLinDam

*!C200254,1 ABD - remark the next line and get the correct File. [Begin]
*SELECT MMFGORDD
SELECT (lcBfileLin)
*!C200254,1 ABD - [End]

lnFilePos=RECNO()


*------------------ CREATE TEMP. FILE -> [BEGIN]
IF !llFirstTmp &&-- do it only one time  

  DIMENSION laFileStru[3,4]
  
  *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
  *laFileStru[1,1] = 'CMFGORDNO'
  laFileStru[1,1] = lcComField
  *!C200254,1 ABD - [End]
  laFileStru[1,2] = 'C'
  laFileStru[1,3] =  6
  laFileStru[1,4] =  0

  laFileStru[2,1] = 'NMFGTOTQTY' 
  laFileStru[2,2] = 'N'
  laFileStru[2,3] = 11
  laFileStru[2,4] = 3

  laFileStru[3,1] = 'LINENO'
  laFileStru[3,2] = 'N'
  laFileStru[3,3] =  6
  laFileStru[3,4] =  0

  CREATE TABLE &gcWorkDir.&lcTmpFile FROM ARRAY laFileStru
  
  *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
  *INDEX ON cMfgrwNum+STR(LineNo,6) TAG (lcTmpFile)
  *SELECT MMFGORDD
  *SET RELATION TO cMfgrwNum+STR(LineNo,6) INTO (lcTmpFile) ADDI      
  INDEX ON &lcComField+STR(LineNo,6) TAG (lcTmpFile)
  SELECT (lcBfileLin)
  SET RELATION TO &lcComField+STR(LineNo,6) INTO (lcTmpFile) ADDI 
  
  *-- Function to Create the lcTmpJour File.
  = lfCrtTmp()
  *!C200254,1 ABD - [End]
   
  llFirstTmp=.T.
ENDIF  
*-- CREATION OF THE STRUCTURE OF THE TEMP FILE [END]

*-- If we did the same calculations befor don't do it again .
*!C200254,1 ABD - remark the next line and get the correct data. [Begin]
*lnOrdLinNo = MMFGORDD.LINENO
lnOrdLinNo = EVAL(lcBfileLin+'.LINENO')
*!C200254,1 ABD - [End]

IF SEEK(lcTCode,lcTmpFile) AND !llFullKey
  *!C200254,1 ABD - Delete the data in case I will use the 
  *!C200254,1 ABD - File in Mfg Rework order. [Begin]
  IF llRwIssRec
    SELECT (lcTmpFile)
    DELE ALL
  ELSE
    *!C200254,1 ABD - [End]
    RETURN
    *!C200254,1 ABD - end if for if statment. [Begin]
  ENDIF  
  *!C200254,1 ABD - [End]
ENDIF

lcScanExp = IIF(llFullKey,PADR(lcTCode,6)+STR(lnOrdLinNo,6),PADR(lcTCode,6))

*!C200254,1 ABD - remark the next line and get the correct File. [Begin]
*SELECT MMFGORDD
SELECT (lcBfileLin)
*!C200254,1 ABD - [End]

*SCAN REST WHILE cmfgordno = lcTCode 
*cmfgordno+STR(lineno,6)+trancd
=SEEK(lcScanExp)
*!C200254,1 ABD - remark the next line and get the correct field. [Begin]
*SCAN REST WHILE cmfgordno+STR(LineNo,6)  = lcScanExp
*  IF !SEEK(cmfgordno+STR(LineNo,6), lcTmpFile)
SCAN REST WHILE &lcComField+STR(LineNo,6)  = lcScanExp
  IF !SEEK(&lcComField+STR(LineNo,6), lcTmpFile)
    *!C200254,1 ABD - [End]
    SCATTER MEMVAR MEMO
    m.Nmfgtotqty=0
    INSERT INTO (lcTmpFile) FROM MEMVAR
    RELEASE m.Nmfgtotqty
    STORE 0 TO lnLinRec , lnLinCan , lnLinDam
  ENDIF
  *hdm[start]
  *REPLACE &lcTmpFile..Nmfgtotqty WITH &lcTmpFile..Nmfgtotqty + ;
                                      IIF(trancd = '1',Nmfgtotqty,-Nmfgtotqty)
  *!C200254,1 ABD - Change the trancd in case Mfg Rework order. [Begin]
  IF llRwIssRec
    IF TRANCD <> '1'
      DO CASE
        CASE TRANCD = '3'
          lnLinRec = lnLinRec + Nmfgtotqty
        CASE TRANCD = '4'
          lnLinDam = lnLinDam + Nmfgtotqty
        CASE TRANCD = '6'
          lnLinCan = lnLinCan + Nmfgtotqty
      ENDCASE
    ENDIF
  ELSE
    *!C200254,1 ABD - [End]
    IF TRANCD<>'1'
      DO CASE
        CASE TRANCD = '2'
          lnLinRec = lnLinRec + Nmfgtotqty
        CASE TRANCD = '3'
          lnLinDam = lnLinDam + Nmfgtotqty
        CASE TRANCD = '4'
          lnLinCan = lnLinCan + Nmfgtotqty
      ENDCASE
    ENDIF
  ENDIF  
  SELECT(lcTmpFile)
  *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
  *REPLACE Nmfgtotqty WITH Nmfgtotqty + ;
                     IIF(MMFGORDD.trancd = '1' , MMFGORDD.Nmfgtotqty , (MMFGORDD.Nmfgtotqty * (-1)))

  DO CASE
    CASE !llRwIssRec .OR. (llRwIssRec .AND. lcOprtType = 'I') 
      REPLACE Nmfgtotqty WITH Nmfgtotqty + ;
                         IIF(EVAL(lcBfileLin+'.trancd') = '1' , EVAL(lcBfileLin+'.Nmfgtotqty') ,;
                          (EVAL(lcBfileLin+'.Nmfgtotqty') * (-1)))
    CASE llRwIssRec .AND. lcOprtType = 'R'
      IF &lcBFileLin..TRANCD <> '1'
        REPLACE Nmfgtotqty WITH Nmfgtotqty + ;
                           IIF(EVAL(lcBfileLin+'.trancd') = '2' , EVAL(lcBfileLin+'.Nmfgtotqty') ,;
                           (EVAL(lcBfileLin+'.Nmfgtotqty') * (-1)))
      ENDIF
  ENDCASE
  *!C200254,1 ABD - [End]
  *hdm[End]
ENDSCAN


SELECT(lcTmpFile)
WAIT WINDOW 'Sorting Recrds' NOWAIT
SCAN
  REPLACE Nmfgtotqty WITH MAX(Nmfgtotqty,0)
ENDSCAN
WAIT CLEAR
*B603880,1 KAM add select MMFGORDD to point the correct area[start]

*!C200254,1 ABD - remark the next line and get the correct File. [Begin]
*SELECT MMFGORDD
SELECT (lcBfileLin)
*!C200254,1 ABD - [End]
 
*B603880,1 KAM [end]
IF BETWEEN(lnFilePos,1,RECCOUNT())
  GO lnFilePos
ENDIF

*!*************************************************************
*! Name      : lfReadLine
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 07/01/1999
*! Purpose   :
*!*************************************************************
*! Called from : lfwBrow & lfvOrder & lfvItem
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfReadLine()
*!*************************************************************
*!
FUNCTION lfReadLine
PARAMETERS llBlankLine
PRIVATE lcCurrAlias
lcCurrAlias  = SELECT(0)
SELECT (lcTmpLine)
IF !llBlankLine
  *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
  *lcTcode     = Cmfgordno
  lcTcode      = &lcComField
  *!C200254,1 ABD - [End]
  lcItem      = CFABRIC
  lcColor     = COLOR
  lcClrDesc   = gfCodDes(&lcTmpLine..COLOR,'COLOR')
  lcPatren    = PATTERN
  lcRefer     = REFERENCE
  *lnWare     = ASCAN(laWare,IIF(!EMPTY(cWareCode),&lcTmpLine..cWareCode,MMFGORDH.cWareCode),1)
  lnWare      = ASCAN(laWare,&lcTmpLine..cWareCode,1)
  lcWareCode  = SUBSTR(laWare[IIF(lnWare=0,1,lnWare)],1,6)
  llVar       = .F.
  llVar       = IIF(FABRIC.cdye_flg='Y',.T.,.F.) 
  llUseDyelot = llDyelot AND llVar 
  *C200254,1 ABD - Issue and receive the material manufacturing 
  *C200254,1 ABD - Rework order from this program so I will Enabel
  *C200254,1 ABD - rolls button to enable lots when issue only.  [Begin]
  *llRollBrow  = llTrkRolls AND Fabric.lTrkRolls
  llRollBrow  = (llTrkRolls AND Fabric.lTrkRolls) .OR. (lcMtCstMth $ 'L' .AND. llRwIssRec .AND.lcOprtType = 'I')
  *C200254,1 ABD - [End]

  lcDyelot    = IIF(llUseDyelot , &lcTmpLine..DYELOT , SPACE(10))
ELSE
  STORE ''  TO lcTCode,lcItem,lcColor,lcClrDesc,lcPatRen,lcDyelot,lcRefer
  STORE 0   TO lnWare
ENDIF
SELECT(lcCurrAlias)
*!*************************************************************
*! Name      : lfChkLine
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : Check existance of entered line .
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : RETURN .T.  if line is exist before in the temp file.
*!*************************************************************
*! Example     : = lfChkLine()
*!*************************************************************
*
FUNCTION lfChkLine
PRIVATE lnAlias , llExist
lnAlias = SELECT(0)

*!C200254,1 ABD - remark the next line and get the correct field. [Begin]
*lcSeekExp= cmfgordno+cfabric+color+dyelot+cWarecode+STR(lineno,6)+'1'
lcSeekExp= &lcComField+cfabric+color+dyelot+cWarecode+STR(lineno,6)+'1'
*!C200254,1 ABD - [End]

lnTmpLPos=RECNO()
GO TOP
llExist   = .F.
llFrist   = .T.

IF SEEK(lcSeekExp)
  *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
  *SCAN REST WHILE cmfgordno+cfabric+color+dyelot+cWarecode+STR(lineno,6)+trancd = lcSeekExp
  SCAN REST WHILE &lcComField+cfabric+color+dyelot+cWarecode+STR(lineno,6)+trancd = lcSeekExp
    *!C200254,1 ABD - [End]
    IF !llFrist  
      llExist = .T.
    ENDIF
    llFrist   = .F.
  ENDSCAN
ENDIF

IF BETWEEN(lnTmpLPos,1,RECCOUNT())
  GOTO lnTmpLPos
ENDIF

SELECT(lnAlias)
RETURN (llExist)

*!*************************************************************
*! Name      : lpClsScr
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : 
*!*************************************************************
PROCEDURE lpClsScr
IF USED(lcTmpRoll)
  SELECT (lcTmpRoll)
  DELETE ALL
ENDIF

IF USED(lcTemLoc)
  SELECT (lcTemLoc)
  DELETE ALL
ENDIF

SELECT (lcTmpLine)
DELETE ALL
=lfClearInfo()
laScrMode=.F.
laScrMode[1]=.T.
*!*************************************************************
*! Name      : lpControl
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999 
*! Purpose   : 
*!*************************************************************
*! Called from : lfvColor & lfvdyelot
*!*************************************************************
*! Calls       : lfChkLine
*!*************************************************************
*! Passed Parameters : llDyeOrWhr = llUsedyelot if the call is from lfvColor  
*!                     llDyeOrWhr = .F.         if the call is from lfvdyelot  
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpControl WITH xxx
*!*************************************************************
*!
PROCEDURE lpControl
PARAMETERS llDyeOrWhr
IF !llDyeOrWhr
  IF lfChkLine()
    IF llWareHous
      lnWare=1
      SHOW GET lnWare
      REPLACE &lcTmpLine..cWarecode WITH SPACE(6)
      STORE 'ENABLE'  TO lcPatrenSt,lcWareSt,lcReferSt,lcEditQSt,lcNewSt,lcRemoveSt
      STORE 'DISABLE' TO lcColorSt
      DO lpShowScrn
    ELSE
      *-- old meassage
      *-This XXXX has been entered on this XXXX.
      =gfModalGen('TRM42107B42000','DIALOG','Item/Color'+'|'+'receiving' )
      lcVar=SYS(18)
      &lcVar =  lcOldVal 
      _CUROBJ = _CUROBJ
    ENDIF    
  ELSE
    STORE 'ENABLE'  TO lcPatrenSt,lcWareSt,lcReferSt,lcEditQSt,lcNewSt,lcRemoveSt
    STORE 'DISABLE' TO lcColorSt , lcDyelotSt
    DO lpShowScrn
  ENDIF
ELSE
  STORE 'ENABLE'  TO lcDyelotSt
  STORE 'DISABLE' TO lcColorSt
  SHOW GET lcColor  &lcColorSt   
  SHOW GET ibColor  &lcColorSt   
  
  *!C200254,1 ABD - Show get the dyelot in case receive only not issue. [Begin]
  IF !llRwIssRec .OR. lcOprtType = 'R'
    *!C200254,1 ABD - [End]
    SHOW GET lcDyelot &lcDyelotSt  
    *!C200254,1 ABD - End IF for if statment. [Begin]
  ENDIF
  *!C200254,1 ABD - [End]

  
   
ENDIF
*!*************************************************************
*! Name      : lpShowScrn
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : 
*!*************************************************************
*!
PROCEDURE lpShowScrn

SHOW GET ibTCode &lcOrderSt
SHOW GET lcTCode &lcOrderSt
    
SHOW GET ibItem   &lcItemSt
SHOW GET lcItem   &lcItemSt     
    

SHOW GET lcColor  &lcColorSt
SHOW GET ibColor  &lcColorSt

IF llDyelot
  *!C200254,1 ABD - Show get the dyelot in case receive only not issue. [Begin]
  IF !llRwIssRec .OR. lcOprtType = 'R'
    *!C200254,1 ABD - [End]
    SHOW GET lcDyelot &lcDyelotSt  
    *!C200254,1 ABD - End IF for IF statment. [Begin]
  ENDIF
  *!C200254,1 ABD - [End]
ENDIF
IF llWareHous

  *!C200254,1 ABD - Disable Field Warehouse in case Rewok. [Begin]
  IF llRwIssRec 
    SHOW GET lnWare   DISABLE
  ELSE
  SHOW GET lnWare   &lcWareSt
 ENDIF
 *!C200254,1 ABD - [End]
 
ENDIF
SHOW GET lcRefer  &lcReferSt
SHOW GET lcPatren &lcPatrenSt   
SHOW GET pbEditQ  &lcEditQSt
SHOW GET pbNew    &lcNewSt
SHOW GET pbRemove &lcRemoveSt

*!*************************************************************
*! Name    : lfClearInfo
*! Developer: Walid Abou El-Magd (WAM)
*! Date     : 07/01/1999
*! Purpose : Clear information.
*!*************************************************************
FUNCTION lfClearInfo

*--Clear Header.
STORE 0   TO lnTotStk,lnTotDam,lnTotCan,lnPolstln
STORE .F. TO llUseDyelot
STORE gdSysDate TO ldRcvDate,ldPost
_CUROBJ=OBJNUM(lcTCode)

*--Clear Line.
=lfReadLine(.T.)


*!*************************************************************
*! Name      : lfvEditQty
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************

FUNCTION lfvEditQty
PRIVATE lcRollSt,lcRollSt1,lcRollSt2,lnRecNo , lcMastKey
*HDM[Start] No need to check the ware house is element #1
*           as this popup now doesn't contain the N/A option
*-- From which we detect any changes in the Qty screen.
*SELECT (lcTmpLine)
*IF llWareHous AND lnWare=1
*  *-- Message : 'First, you must select location.'
*  =gfModalGen('TRM42150B42001','DIALOG')
*  _CUROBJ = OBJNUM(lnWare)
*  RETURN
*ENDIF
*HDM[End]

*!C200254,1 ABD - remark the next line and get the correct field. [Begin]
*lcLKey = cmfgordno + cfabric + color + dyelot + cWarecode + STR(lineno,6)
*lcMastKey = cmfgordno+STR(lineno,6)+'1'
lcLKey    = &lcComField + cfabric + color + dyelot + cWarecode + STR(lineno,6)
lclTrnType = IIF(llRwIssRec .AND. lcOprtType = 'R','2','1')
lcMastKey = &lcComField +STR(lineno,6)+lclTrnType
*!C200254,1 ABD - [End]

STORE 'DISABLE' TO lcSecItemSt,lcDamItemSt , lcRollSt1,lcRollSt2,lcRollSt
STORE 0         TO lnSok , lnDam1 , lnCan , lnBal , lnDam2 , lnOldDam1 , lnOldDam2
STORE ' '       TO lcRetFab1,lcRetClr1,lcRetFab2,lcRetClr2
*HDM[START]
*lnConst = &lcTmpLine..nmfgtotqty  &&-- Qty when make the order
*lnConst = IIF(SEEK(lcMastKey,'MMFGORDD') , &lcTmpLine..nmfgtotqty , 0)
*!C200254,1 ABD - remark the next line and get the correct file. [Begin]
*lnConst = IIF(SEEK(lcMastKey,'MMFGORDD') , MMFGORDD.nmfgtotqty , 0)
lnConst = IIF(SEEK(lcMastKey,lcBFileLin) , &lcBFileLin..nmfgtotqty , 0)
*!C200254,1 ABD - [End]

*HDM[END]
*walid now
*=SEEK(cmfgordno+STR(LineNo,6),lcTmpFile)
IF EOF()
  WAIT WINDOW 'EOF'
ENDIF
lnOrg   = &lcTmpFile..nmfgtotqty  &&-- Qty open to receive now from the lpGetOpen
lnBal   = &lcTmpLine..TOTBAL      &&-- Qty open to receive now when this record is first time inserted

*-- Adjust the titels according to the sort of material you receive .
STORE '' TO lcRetSHd,lcRetSHd1,lcRetSHd2,lcRetSHd3,lcRetSHd4

lcMFabQlty = FABRIC.cFabGrade

lcSndGrd   = "2"
lcTrdGrd   = "3"

DO CASE
  
  CASE lcMFabQlty = '1'
  
    lcRetSHd  = "1st Quality"
    lcRetSHd1 = "2nd Quality"
    lcRetSHd2 = "Damaged"
    lcRetSHd3 = "Second Quality"
    lcRetSHd4 = "Damaged"
  
  CASE lcMFabQlty = '2'
  
    lcRetSHd  = "2nd Quality"
    lcRetSHd1 = "1st Quality"
    lcRetSHd2 = "Damaged"
    lcRetSHd3 = "First Quality"
    lcRetSHd4 = "Damaged"
    lcSndGrd  = "1"
    lcTrdGrd  = "3"
  
  CASE lcMFabQlty = '3'
  
    lcRetSHd  = "Damaged"
    lcRetSHd1 = "1st Quality"
    lcRetSHd2 = "2nd Quality"    
    lcRetSHd3 = "First Quality"
    lcRetSHd4 = "Second Quality"
    lcSndGrd  = "1"
    lcTrdGrd  = "2"
ENDCASE
*-- Variables to control the accomulators in main secrren
lnOldStk = TotStk
lnOldDam = TotDam
lnOldCan = TotCan


*--1) Get Stock quantity.
*!C200254,1 ABD - remark the next line and get the correct Type. [Begin]
*IF SEEK(lcLKey+'2')
*-- Thai Variable Hold True In case We issue With Cost Method lot and Not Keep
*-- Tarke Rolls.
llIsueLots = .F.
IF llRwIssRec .AND. lcOprtType = 'I' .AND. lcMtCstMth $ 'L'
  llIsueLots = .T. 
ENDIF

lcTrnType = IIF(llRwIssRec .AND. lcOprtType = 'R','3','2')
IF SEEK(lcLKey+lcTrnType)
  *!C200254,1 ABD - [End]
  lnSok = nmfgtotqty
  lcRollSt=IIF(llRollBrow AND lnSok > 0 ,'ENABLE','DISABLE')
ENDIF

*--2) Get Damage quantity.
STORE 'DISABLE' TO lcSecItemSt,lcDamItemSt
IF SEEK(lcLKey+lcOthrTrCd)
  *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
  *SCAN REST WHILE cmfgordno+cfabric+color+dyelot+cWarecode+STR(lineno,6)+TranCd=lcLKey+lcOthrTrCd
  SCAN REST WHILE &lcComField+cfabric+color+dyelot+cWarecode+;
                  STR(lineno,6)+TranCd=lcLKey+lcOthrTrCd  
    *!C200254,1 ABD - [End]
    
    IF cFabGrade = lcSndGrd
      lnDam1     = nmfgtotqty
      lnOldDam1  = nmfgtotqty
      lcRetFab1  = cfabric1
      lcRetClr1  = cColor1
      lcSecItemSt= 'ENABLE'
      =SEEK(cfabric1+cColor1,'FABRIC')
      IF FABRIC.Ltrkrolls
        lcRollSt1='ENABLE'
      ENDIF
      SELECT (lcTmpLine) && -- the relation will adjust fabric pointer
    ELSE
      lnDam2     = nmfgtotqty
      lnOldDam2  = nmfgtotqty
      lcRetFab2  = cfabric1
      lcRetClr2  = cColor1
      lcDamItemSt= 'ENABLE'
      =SEEK(cfabric1+cColor1,'FABRIC')
      IF FABRIC.Ltrkrolls
        lcRollSt2= 'ENABLE'
      ENDIF
      SELECT (lcTmpLine) && -- the relation will adjust fabric pointer
    ENDIF
  ENDSCAN
ENDIF

*--3) Get Cancel quantity.
IF SEEK(lcLKey+lcCanlTrCd)
  lnCan = nmfgtotqty
ENDIF

lcSkMode = 'ENABLE'
lcOtMode = 'ENABLE'

*!C200254,1 ABD - remark the next line and get the correct Type. [Begin]
*=SEEK(lcLKey+'1')
lcTrnType = IIF(llRwIssRec .AND. lcOprtType = 'R','2','1')
=SEEK(lcLKey+lcTrnType)

*!C200254,1 ABD - [End]

*-- Save trapping setting .
lcHldTab = ON('KEY','TAB')
lcHldBtb = ON('KEY','BACKTAB')    
lcHldEnt = ON('KEY','ENTER')
ON KEY LABEL TAB    
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER

*!B038431,1 MHM 08/24/2004 Assign Function to C CHR[Start]
ON KEY LABEL ALT+C DO  lfvQtyOk
*!B038431,1 MHM [End]

*!C200254,1 ABD - Variable to disable the 2st Qult , Damge & cancel fields  [Begin]
lcIssuStat = IIF(llRwIssRec .AND. lcOprtType = 'I','DISABLE','ENABLE')
*!C200254,1 ABD - [End]

*--Call line quantity screen.
DO (gcScrDir+gcWinAppl+"\MARCVQ.SPX")


*-- Restore trapping setting .
ON KEY LABEL TAB     &lcHldTab
ON KEY LABEL BACKTAB &lcHldBtb
ON KEY LABEL ENTER   &lcHldEnt
PRIVATE llUpdRec
STORE .F. TO llUpdRec

=SEEK(lcLKey)
IF EOF()
  WAIT WINDOW 'EOF'
ENDIF

SCATTER MEMVAR
STORE 0 TO m.TotStk,m.TotDam,m.TotCan,m.TotBal

*--1) Update stock quantity.
*HDM[Start]

*!C200254,1 ABD - remark the next line and get the correct Type. [Begin]
*IF SEEK(lcLKey+'2')
lcTrnType = IIF(llRwIssRec .AND. lcOprtType = 'R','3','2')
IF SEEK(lcLKey+lcTrnType)
  *!C200254,1 ABD - [End]
  *-- if this recored is already exist update it.
  *--The user changed the quantity to 0
  IF lnSok = 0
    *-- Delete the old rolls if any[Start]
    IF USED (lcTmpRoll)
      *-- PARAMETERS lcFab,lcClr,lcTitel,lcFocus,lnQtyToComp,llToDelete
      =lfChkRoll(Cfabric,Color,"","",0,.T.)
    ENDIF
    *-- Delete the old rolls if any[End..]
  ENDIF

  *-- The user applied another quantity
  IF ABS(lnSok - lnOldStk) > 0 OR lnSok = 0
    *-- Means we're going to update this record
    REPLACE nmfgtotqty WITH lnSok
    llUpdRec = .T.
  ELSE
    *-- The user didn't do anything in stock 
    *-- So we will ignore lnSok From Updates
    *lnSok = 0
  ENDIF
ELSE
  IF lnSok > 0
    APPEND BLANK
    GATHER MEMVAR
    *!C200254,1 ABD - remark the next line and Update with the correct Type. [Begin]
    *REPLACE Trancd     WITH '2'        ,;
            cFabGrade  WITH lcMFabQlty ,;
            nmfgtotqty WITH lnSok      ,;
            totstk     WITH lnSok

    REPLACE Trancd     WITH lcTrnType  ,;
            cFabGrade  WITH lcMFabQlty ,;
            nmfgtotqty WITH lnSok      ,;
            totstk     WITH lnSok
      *!C200254,1 ABD - [End]
    llUpdRec = .T.
  ENDIF
ENDIF

*--2) Update Damaged quantity.

IF SEEK(lcLKey+lcOthrTrCd) && Second Quality
  *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
  *LOCATE WHILE CMFGORDNO+CFABRIC+COLOR+DYELOT+CWARECODE+STR(LINENO,6)+TRANCD=LCLKEY+lcOthrTrCd ;
  *       FOR cFabGrade=lcSndGrd
  LOCATE WHILE &lcComField+CFABRIC+COLOR+DYELOT+CWARECODE+STR(LINENO,6)+TRANCD=LCLKEY+lcOthrTrCd ;
         FOR cFabGrade=lcSndGrd
    *!C200254,1 ABD - [End]
           
  IF FOUND()
    IF lnDam1 = 0
      *-- Qty changed to zero.
      *-- Delete the old rolls if any[Start]
      IF USED (lcTmpRoll)
        *-- PARAMETERS lcFab,lcClr,lcTitel,lcFocus,lnQtyToComp,llToDelete
        =lfChkRoll(Cfabric1,Ccolor1,"","",0,.T.)
      ENDIF
      *-- Delete the old rolls if any[End..]
    ENDIF
    *-- The user applied another quantity
    IF ABS(lnDam1 - lnOldDam1) > 0 OR lnDam1 = 0
      *-- Means we're going to update this record
      REPLACE nmfgtotqty WITH lnDam1,;
              cFabric1 WITH lcRetFab1,cColor1 WITH lcRetClr1
      llUpdRec = .T.
    ELSE
      *-- The user didn't do anything
      *lnDam1 = 0
    ENDIF
  
  ELSE
    IF lnDam1 > 0
      *-- New Transaction line
      APPEND BLANK
      GATHER MEMVAR
      REPLACE Trancd    WITH lcOthrTrCd,;
              cFabGrade WITH lcSndGrd,;
              cFabric1 WITH lcRetFab1 , cColor1 WITH lcRetClr1,;
              nmfgtotqty WITH lnDam1
      llUpdRec = .T.
    ENDIF
  ENDIF
ELSE
  IF lnDam1 > 0
    *-- New Transaction line
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd    WITH lcOthrTrCd,;
            cFabGrade WITH lcSndGrd,;
            cFabric1 WITH lcRetFab1 , cColor1 WITH lcRetClr1,;
            nmfgtotqty WITH lnDam1
    llUpdRec = .T.
  ENDIF
ENDIF

IF SEEK(lcLKey+lcOthrTrCd) && Third Quality
  *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
  *LOCATE WHILE CMFGORDNO+CFABRIC+COLOR+DYELOT+CWARECODE+STR(LINENO,6)+TRANCD=LCLKEY+lcOthrTrCd ;
  *       FOR cFabGrade=lcTrdGrd
  LOCATE WHILE &lcComField+CFABRIC+COLOR+DYELOT+CWARECODE+;
               STR(LINENO,6)+TRANCD=LCLKEY+lcOthrTrCd ;
               FOR cFabGrade=lcTrdGrd
    *!C200254,1 ABD - [End]
  IF FOUND()
    IF lnDam2 = 0
      *-- Qty changed to zero.
      *-- Delete the old rolls if any[Start]
      IF USED (lcTmpRoll)
        *-- PARAMETERS lcFab,lcClr,lcTitel,lcFocus,lnQtyToComp,llToDelete
        =lfChkRoll(Cfabric1,Ccolor1,"","",0,.T.)
      ENDIF
      *-- Delete the old rolls if any[End..]
    ENDIF
    *-- The user applied another quantity
    IF ABS(lnDam2 - lnOldDam2) > 0 OR lnDam2 = 0
      *-- Means we're going to update this record
      REPLACE nmfgtotqty WITH lnDam2,;
              cFabric1 WITH lcRetFab2,cColor1 WITH lcRetClr2
      llUpdRec = .T.
    ELSE
      *-- The user didn't do anything
      *lnDam2 = 0
    ENDIF
  ELSE
    IF lnDam2 > 0
      *-- New Transaction line
      APPEND BLANK
      GATHER MEMVAR
      REPLACE Trancd    WITH lcOthrTrCd,;
              cFabGrade WITH lcTrdGrd,;
              cFabric1 WITH lcRetFab2 , cColor1 WITH lcRetClr2,;
              nmfgtotqty WITH lnDam2
      llUpdRec = .T.
    ENDIF
  ENDIF
ELSE
  IF lnDam2 > 0
    *-- New Transaction line
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd    WITH lcOthrTrCd,;
            cFabGrade WITH lcTrdGrd,;
            cFabric1 WITH lcRetFab2 , cColor1 WITH lcRetClr2,;
            nmfgtotqty WITH lnDam2
    llUpdRec = .T.
  ENDIF

ENDIF

*--3) Update cancel quantity.
IF SEEK(lcLKey+'4')
  *-- if this recored is already exist update it.
  *--The user changed the quantity to 0
  IF lnCan = 0
    **-- Delete the old rolls if any[Start]
    *IF USED (lcTmpRoll)
    *  *-- PARAMETERS lcFab,lcClr,lcTitel,lcFocus,lnQtyToComp,llToDelete
    *  =lfChkRoll(Cfabric,Color,"","",0,.T.)
    *ENDIF
    **-- Delete the old rolls if any[End..]
  ENDIF
  *-- The user applied another quantity
  IF ABS(lnCan - lnOldCan) > 0 OR lnCan = 0
    *-- Means we're going to update this record
    REPLACE nmfgtotqty WITH lnCan
    llUpdRec = .T.
  ELSE
    *-- The user didn't do anything in stock 
    *-- So we will ignore lnSok From Updates
  ENDIF
ELSE
  IF lnCan > 0
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd     WITH '4'        ,;
            cFabGrade  WITH lcMFabQlty ,;
            nmfgtotqty WITH lnCan
    llUpdRec = .T.
  ENDIF
ENDIF

IF !llUpdRec
  =SEEK(lcLKey)
  RETURN
ENDIF

SELECT (lcTmpLine)
*!C200254,1 ABD - remark the next line and get the correct Type. [Begin]
*IF SEEK(lcLKey+'1')
lcTrnType = IIF(llRwIssRec .AND. lcOprtType = 'R','2','1')
IF SEEK(lcLKey+lcTrnType)
  *!C200254,1 ABD - [End]
  *REPLACE TotStk WITH lnSok+lnOldStk,;
          TotDam WITH lnDam1+lnDam2+lnOldDam,;
          TotCan WITH lnCan+lnOldCan,;
          TotBal WITH lnBal
  REPLACE TotStk WITH lnSok         ,;
          TotDam WITH lnDam1+lnDam2 ,;
          TotCan WITH lnCan         ,;
          TotBal WITH lnBal
   
   *!C200254,1 ABD - Update Newline field with true to add new recored.[Begin]
   *-- check if this recored exist in the MARWLIN OR MMFORDD
   IF !llRwIssRec .OR. lcOprtType = 'R'
     SELECT (lcBFileLin)
     lnRecNoLin = RECNO()
     lnRecOrder = ORDER()
     IF !llRwIssRec
       SET ORDER TO Mmfgordd
     ELSE
       SET ORDER TO Marwlin
     ENDIF
     SELECT (lcTmpLine)
     IF SEEK (&lcComField. + cfabric + color + dyelot,lcBFileLin)
        SELECT (lcBFileLin)
        LOCATE REST WHILE &lcComField.+cfabric+color+dyelot+trancd  = ;
        &lcTmpLine..&lcComField. + &lcTmpLine..cfabric + &lcTmpLine..color + &lcTmpLine..dyelot;
        FOR cwarecode = &lcTmpLine..cwarecode .AND. trancd = lcBrTrncd  .AND. STR(lineNo,6) = STR(M.LineNo,6)        
        IF !FOUND()
          SELECT (lcTmpLine)
          REPLACE Newline WITH  .T.   
        ENDIF
     ELSE
       REPLACE Newline WITH  .T.   
     ENDIF
     SELECT (lcBFileLin)
     GOTO lnRecNoLin
     SET ORDER TO &lnRecOrder
     SELECT (lcTmpLine)
   ENDIF
  *!C200254,1 ABD - [End]

  IF Newline
    REPLACE nmfgtotqty WITH TotStk + TotDam
  ENDIF

ENDIF
*---- Uppdate the accomulators on the screen .
*HDM[START]

lnTotStk = lnTotStk + (TotStk-lnOldStk)
lnTotDam = lnTotDam + (TotDam-lnOldDam)
lnTotCan = lnTotCan + (TotCan-lnOldCan)

*lnTotStk = lnTotStk + TotStk
*lnTotDam = lnTotDam + TotDam
*lnTotCan = lnTotCan + TotCan
*HDM[END]

=lfRefresh(lcWinCh1)
SHOW WINDOW (lcBrowTitl) REFRESH SAME

*!*************************************************************
*! Name      : lfvRetFab
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : lcFQualty
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************

FUNCTION lfvRetFab
PARA lcFQualty

*AND !EMPTY(SUBSTR(lcRetFab&lcFQualty,1,1))
IF !llBrowse  AND lcRetFab&lcFQualty=IIF(EMPTY(lcOldVal),SPACE(7),lcOldVal)
  RETURN
ENDIF  

DO CASE
  CASE lcMFabQlty='1'
    lcQualty=IIF(lcFQualty='1','2','3')
  CASE lcMFabQlty='2'
    lcQualty=IIF(lcFQualty='1','1','3')
  CASE lcMFabQlty='3'    
    lcQualty=IIF(lcFQualty='1','1','2')
ENDCASE
lcOldClr=lcRetClr&lcFQualty
lnAlias=SELECT()
SELECT FABRIC

lcBrFabric=IIF(lcFQualty='1',lcRetFab1,lcRetFab2)

llValid=.F.

IF llBrowse OR (!EMPTY(lcBrFabric)) 
  llbrowse = .F.
  SET FILTER TO cFabGrade=lcQualty AND Make
  GO TOP
  IF EOF()
    *--No fabrics found for this quality. 
    *WALID new
    =gfModalGen('TRM36142B42000','DIALOG')
    lcBrFabric = SPACE(7)
  ELSE
    lcHdEsc1= ON('KEY','ESC')
    ON KEY LABEL ESC 

    lcxItem    = lcBrFabric
    DO FABROW WITH lcxItem , ' ' &&-- Parameter '*' means don not browes colors
    lcBrFabric = lcxItem

    lcRetClr&lcFQualty=FABRIC.COLOR
    =lfRefresh("MARCVQ") 
    ON KEY LABEL ESC &lcHdEsc1
  
  ENDIF 
  SET FILTER TO
ENDIF

lcRetFab&lcFQualty = lcBrFabric

IF lcFQualty='1'
  lcRollSt1=IIF(FABRIC.ltrkrolls,'ENABLE','DISABLE')
  SHOW GET pbRolls1 &lcRollSt1
ELSE
  lcRollSt2=IIF(FABRIC.ltrkrolls,'ENABLE','DISABLE')
  SHOW GET pbRolls2 &lcRollSt2
ENDIF

IF !EMPTY(lcBrFabric) AND llWareHous
  SELECT FABDYE

  IF !SEEK(lcBrFabric+lcRetClr&lcFQualty+lcWareCode+SPACE(10),'FABDYE')
    *-- old meassage by samah
    *-Item/Color xxx is not assigned to Location: xxx. "\<Add;\<Reenter"
    
    IF gfModalGen('QRM36049B42006','DIALOG',ALLTRIM(lcBrFabric)+'/'+ALLTRIM(lcRetClr&lcFQualty)+'|'+lcWareCode) = 1
      DO gpAdFabWar WITH lcBrFabric,lcRetClr&lcFQualty,SPACE(10),lcWareCode,lcTempName
    ELSE
      STORE lcOldVal TO lcRetFab&lcFQualty,lcBrFabric
      STORE lcOldClr TO lcRetClr&lcFQualty
      =lfRefresh("MARCVQ")
      _CUROBJ = _CUROBJ 
    ENDIF
  ENDIF
ENDIF

*-- Check if other FABRIC/COLOR is a dyelot yes style but the original FABRIC/COLOR was No.
IF llDyelot AND EMPTY(lcDyelot)
  IF !EMPTY(lcRetFab&lcFQualty) AND ;
    SEEK(lcRetFab&lcFQualty+lcRetClr&lcFQualty,'FABRIC') AND ;
    FABRIC.CDYE_FLG='Y'
    _CUROBJ = _CUROBJ
    *WALID
    *--The Item/Color xxx comes in dyelot but the original Item/Color xxxx did not use dyelots,
    *--Please make sure the the other quality Item/Color has same dyelot usage.
    *-- New meas 
    =gfModalGen('TRM36140B42000','DIALOG',ALLTRIM(lcRetFab&lcFQualty)+'/'+ALLTRIM(lcRetClr&lcFQualty)+'|'+ALLTRIM(lcItem)+'/'+ALLTRIM(lcColor))
    SELECT(lnAlias)
    *HDM[START]
    lcRetFab&lcFQualty = ''
    lcRetClr&lcFQualty = ''
    =lfRefresh('MARCVQ')
    *HDM[END]
    RETURN
  ENDIF
ENDIF
*-- Check if other FABRIC/COLOR is use Rolls
IF FABRIC.ltrkrolls
  

ENDIF
SELECT(lnAlias)
*!*************************************************************
*! Name     : lfvQty
*! Developer: Walid Abou El-Magd
*! Date     : 07/01/1999               
*! Purpose  : Edit line quantity.
*!*************************************************************
FUNCTION lfvQty
lnNewBal=lnOrg-(lnSok+lnDam1+lnDam2+lnCan)

IF _CUROBJ<>OBJNUM(lnCan) AND lnCan > 0  AND lnNewBal<0
  *--Cancel quantity more that new balance, Therefore it will be redused.
  =gfModalGen('INM34068B42000','DIALOG')
  lnBal=0
  lnCan=lnOrg-(lnSok+lnDam1+lnDam2)
  lnCan=IIF(lnCan < 0 ,0,lnCan)
  SHOW GET lnCan
ELSE
  lnBal=lnOrg-(lnSok+lnDam1+lnDam2+lnCan)
  lnBal=IIF(lnBal < 0,0,lnBal)
ENDIF

IF llRollBrow 
  lcRollSt=IIF(lnSok <> 0 ,'ENABLE','DISABLE')
  SHOW GET pbRolls &lcRollSt
ENDIF

IF lnDam1 <> 0
  SHOW GET lcRetFab1 ENABLE
  SHOW GET ibSecItem ENABLE
ELSE
  lcRollSt1='DISABLE'
  lcRetFab1=SPACE(7)
  lcRetClr1=SPACE(6)
  SHOW GET lcRetFab1   DISABLE
  SHOW GET ibSecItem   DISABLE
  IF llTrkRolls
    SHOW GET pbRolls1    &lcRollSt1
  ENDIF
ENDIF
IF lnDam2 <> 0
  SHOW GET lcRetFab2 ENABLE
  SHOW GET ibDamItem ENABLE
ELSE
  lcRollSt2='DISABLE'
  lcRetFab2=SPACE(7)
  lcRetClr2=SPACE(6)
  SHOW GET lcRetFab2 DISABLE
  SHOW GET ibDamItem DISABLE
  IF llTrkRolls
    SHOW GET pbRolls2  &lcRollSt2
  ENDIF
ENDIF

=lfRefresh("MARCVQ")


*!*************************************************************
*! Name    : lfvQtyOk
*! Developer: Walid Abou El-Magd
*! Date     : 07/01/1999
*! Purpose : Exit from edit line quantity screen.
*!*************************************************************
FUNCTION lfvQtyOk
PRIVATE lnAlias,lcVar
lnAlias=SELECT()
IF lnSok=0 AND lnDam1=0 AND lnDam2=0 
  CLEAR READ
  SELECT(lnAlias)
  RETURN
ELSE
  *Check dyelot changed
  IF &lcTmpLine..lNewDyelot
    IF !lfAloDye()
      RETURN
    ENDIF
  ENDIF
ENDIF  

IF lnDam1<>0 AND EMPTY(lcRetFab1)
  *walid new mess
  *-You cannot leave the XXXX fabric empty since the XXXX quantity was entered.
  =gfModalGen('TRM36141B42000','DIALOG',lcRetSHd3+'|'+lcRetSHd3)
  _CUROBJ=OBJNUM(lcRetFab1) 
  SELECT(lnAlias)
  RETURN
ENDIF

IF lnDam2<>0 AND EMPTY(lcRetFab2)
  *-You cannot leave the XXXX fabric empty since the XXXX quantity was entered.
  =gfModalGen('TRM36141B42000','DIALOG',lcRetSHd4+'|'+lcRetSHd4)
  _CUROBJ=OBJNUM(lcRetFab2) 
  SELECT(lnAlias)
  RETURN
ENDIF

*-- Now we have to check if the rolls was enterd or no for all kinds of fabics
*-- and if the Qtys of the fabrics was changed for the same fabric 
*-- make the user adjust the Qty from rolls screen

*-- in case of first time we add rolls the tmp roll file will not be created yet
*-- the next IF .. ENDIF block is to handel this case .
*C200254,1 ABD - Issue and receive the material manufacturing 
*C200254,1 ABD - Rework order from this program so I will Enabel
*C200254,1 ABD - rolls button to enable lots when issue only.  [Begin]
IF (lcMtCstMth $ 'FIL' .AND. llRwIssRec .AND.lcOprtType = 'I')
  IF !USED(lcTmpJour)
    =gfModalGen('INM36092B00000','ALERT','lots')
    SELECT(lnAlias)
    RETURN
  ELSE
    IF llRwIssRec .AND. lcOprtType = 'I' .AND. lcMtCstMth $ 'LFI' .AND. !FABRIC.lTrkRolls
      IF !lfOldLots ()
        *--No [Receiving Lines] was done, Cannot update.
        = gfModalGen('INM34061B42000','DIALOG','Receiving Lines')
        STORE .F. TO llShow,llCSave
        *-- Check of phisical record before go to it.
        RETURN .F.
      ENDIF
    ENDIF
    *- Check if the user Enetr the Qty
    llNotApply = .T.
    SELECT (lcTmpJour)
    LOCATE
    *-- Scan For Issue Qty in the temp file.
    SCAN WHILE !EOF() FOR cFabric+CColor+cWarecode+cDyelot = ;
      &lcTmpLine..cfabric+&lcTmpLine..COLOR+&lcTmpLine..cwarecode+PADR(&lcTmpLine..Dyelot,10)
      IF nApply <> 0
        llNotApply = .F.
        EXIT  
      ENDIF
    ENDSCAN

    *-- If user didn't enter any Qty to issue.
    IF llNotApply
      =gfModalGen('INM36092B00000','ALERT','lots')
      _CUROBJ=OBJNUM(lcRetFab2) 
      SELECT(lnAlias)
      RETURN
    ENDIF
  ENDIF
ELSE
  *C200254,1 ABD - [End]
  IF !USED(lcTmpRoll) AND ( lcRollSt='ENABLE' OR lcRollSt1='ENABLE' OR lcRollSt2='ENABLE')
    lcVar=IIF(lcRollSt='ENABLE',lcRetSHd,IIF(lcRollSt1='ENABLE',lcRetSHd3,lcRetSHd4))
    =gfModalGen('INM36165B42x000','DIALOG',lcVar)
    SELECT(lnAlias)
    RETURN
  ENDIF
  IF lcRollSt  = 'ENABLE' AND !lfChkRoll(lcItem,lcColor,lcRetSHd,pbRolls,lnSok)
    RETURN
  ENDIF  

  IF lcRollSt1 = 'ENABLE' AND !lfChkRoll(lcRetFab1,lcRetClr1,lcRetSHd3,pbRolls1,lnDam1)
    RETURN
  ENDIF  

  IF lcRollSt2 = 'ENABLE' AND !lfChkRoll(lcRetFab2,lcRetClr2,lcRetSHd4,pbRolls2,lnDam2)
    RETURN
  ENDIF  

  *C200254,1 ABD - End For I statment. [Begin]
ENDIF
*C200254,1 ABD - [End]
  
SELECT(lnAlias)
CLEAR READ


*!*************************************************************
*! Name      : lfChkRoll
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : check if the roll qty is entered or not 
*!             and modified or not .
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  .T. if no error & .F. otherwise
*!*************************************************************
*! Example            :  =lfChkRoll()
*!*************************************************************
FUNCTION lfChkRoll
PARAMETERS lcFab,lcClr,lcTitel,lcFocus,lnQtyToComp,llToDelete
PRIVATE lnAlias,lnApply,lcOldOrder,lnRecNo
lnAlias=SELECT()
SELECT (lcTmpRoll) 
lcOldOrder=ORDER(lcTmpRoll)
SET ORDER TO TAG lcTmpRoll2

*walid new mess
IF !SEEK(lcFab+lcClr+lcWareCode+IIF(llUseDyelot,lcDyelot,SPACE(10))+STR(&lcTmpLine..LineNo,6)) AND !llToDelete 
  IF llToDelete
    RETURN
  ENDIF
  *-- You have to enter rolls for XXX quantity
  =gfModalGen('INM36165B42000','DIALOG',lcTitel)
  SET ORDER TO TAG &lcOldOrder
  SELECT(lnAlias)
  _CUROBJ=OBJNUM(lcFocus)  
  RETURN (.F.)
ELSE
  lnRecNo=RECNO()
  lnApply=0
  SCAN REST WHILE Cfabric+Ccolor+Cwarecode+Cdyelot+STR(LineNo,6)=lcFab+lcClr+lcWareCode+IIF(llUseDyelot,lcDyelot,SPACE(10))+STR(&lcTmpLine..LineNo,6)
    IF llToDelete
      BLANK
      DELETE
    ELSE
      lnApply=lnApply+Napply
    ENDIF
  ENDSCAN
  GO lnRecNo
  IF lnApply # lnQtyToComp
    *-- XXX quantity has been modified,You have to adjust roll quantity
    =gfModalGen('INM36166B42000','DIALOG',lcTitel)
    SET ORDER TO TAG &lcOldOrder
    SELECT(lnAlias)
    _CUROBJ=OBJNUM(lcFocus)            
    RETURN (.F.)
  ENDIF
ENDIF
SET ORDER TO TAG &lcOldOrder
SELECT(lnAlias)

*-------------------------------------------------------------------------
*--------------------------- Location Section ----------------------------
*!*************************************************************
*! Name      : lfvLocWare
*! Developer: Walid Abou El-Magd
*! Date     : 07/01/1999
*! Purpose   : Valid Select locations.
*!*************************************************************
FUNCTION lfvLocWare

llLoc = .F.
SHOW GET llLoc

lcOldAlias = ALIAS() 
SELECT (lcTemLoc)
DELETE ALL FOR cWareCode<>lcWareCode AND Style=PADR(lcItem,7) AND Color=lcColor
SELECT WHSLOC 
SET ORDER TO TAG WhsLoc
llFound=SEEK(lcWareCode)
SET ORDER TO TAG WhsLocSt
IF llFound
  =lfvLocat()
ELSE
  *walid
  *--No bins have been assigned to location XXX .
  = gfModalGen('TRM42058B42000','DIALOG',ALLTRIM(lcWareCode))
ENDIF  
SELECT (lcOldAlias)


*!*************************************************************
*! Name      : lfvLocat
*! Developer: Walid Abou El-Magd
*! Date     : 07/01/1999
*! Purpose   : Do Functions mover to select locations.
*!*************************************************************
FUNCTION lfvLocat

SELECT WHsLoc
SEEK PADR(lcItem,7)+lcColor+lcWareCode
SCAN WHILE STYLE+COLOR+CWARECODE+CLOCATION=PADR(lcItem,7)+lcColor+lcWareCode
  SCATTER MEMVAR
  SELECT (lcTemLoc)
  IF !SEEK(PADR(lcItem,7)+lcColor+lcWareCode)
    APPEND BLANK
    GATHER MEMVAR 
  ENDIF
ENDSCAN

DIMENSION laSource[1],laTarget[1]
STORE ' ' TO laSource,laTarget
lsSource = 1
SELECT cLocation FROM WHSLOC ;
  WHERE Style+Color+cWareCode == SPACE(19)+SPACE(6)+lcWareCode ;
  INTO ARRAY laSource

SELECT cLocation FROM (lcTemLoc) ;
  WHERE Style+Color+cWareCode+cLocation=PADR(lcItem,7)+lcColor+lcWareCode;
  INTO ARRAY laTarget

=gfMover(@laSource,@laTarget,"Assign Bins",.T.,'lfvLoc')
SELECT (lcTemLoc)

DELETE FOR Style+Color+cWareCode+cLocation = PADR(lcItem,7)+lcColor+lcWareCode

FOR I = 1 TO ALEN(laTarget)
   APPEND BLANK
   REPLACE STYLE     WITH lcItem;
           Color     WITH lcColor;
           CWARECODE WITH lcWareCode;
           CLOCATION WITH laTarget[i]
ENDFOR
RETURN

*!*************************************************************
*! Name      : lfvLoc
*! Developer: Walid Abou El-Magd
*! Date     : 07/01/1999
*! Purpose   : Validate locations in mover when select.
*!*************************************************************
FUNCTION lfvLoc
PARAMETERS lnOption

DO CASE 
  CASE lnOption=1
    lcLocatin=laSource[lsSource]
    IF !SEEK(PADR(lcItem,7)+lcColor+lcWareCode+lcLocatin,"WhsLoc")
      *walid new
      *--Bin XXX is not assigned to fabric/color XXX/XXX in location XXX,'\!\<Assign;\<Cancel
      IF gfModalGen('TRM36163B42007','DIALOG',ALLTRIM(lcLocatin)+'|'+ALLTRIM(lcItem)+'/'+ALLTRIM(lcColor)+'|'+ALLTRIM(lcWareCode)) = 1
        RETURN .T.
      ELSE
        RETURN .F. 
      ENDIF
    ELSE
      RETURN .T.
    ENDIF
  CASE lnOption=2
    FOR I=1 TO ALEN(laSource,1)
      lcLocatin=laSource[I]
      IF !SEEK(PADR(lcItem,7)+lcColor+lcWareCode+lcLocatin,"WhsLoc")
        *walid new
        *--One or more Bin(s) are not assigned to '+'fabric/color XXX/XXX in location XXX,'\!\<Assign;\<Cancel
        IF gfModalGen('TRM36164B42007','DIALOG',ALLTRIM(lcItem)+'/'+ALLTRIM(lcColor)+'|'+ALLTRIM(lcWareCode)) = 1
          RETURN .T.
        ELSE
          RETURN .F. 
        ENDIF
      ENDIF
    ENDFOR
  CASE (lnOption=3 OR lnOption=4)
ENDCASE

*-------------------------------------------------------------------------
*--------------------------- Roll Section --------------------------------
*!*************************************************************
*! Name      : lfvRolls
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : gfmatcrl.prg
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  
*!*************************************************************

FUNCTION lfvRolls
PARAMETERS lcFQualty

PRIVATE lcKey , lnAdjStk , lcFDyelot , lcTrType , lnAlias , ldTrDate ,;
                ldPostDate , lcTrCode , lcRollNP , lcRollRP , lnLineNo

lnAlias=SELECT(0)
*-- 
lcTrType='1'
lcFDyelot=lcDyelot
llDyeLvl   = llDyelot
lcRollNP   = lcNewP
lcRollRP   = lcRemveP 
ldTrDate   = ldRcvDate
ldPostDate =IIF(llLinkToGl,ldPost,ldTrDate) &&-- Ask MAN
lcTrCode   =lcTcode
lnLineNo   = &lcTmpLine..LineNo

IF TYPE('lcFQualty') $ 'LU' 
  lcKey      =lcItem+lcColor
  lcFabric   =lcItem
  lnAdjStk   =lnSok  
ELSE
  lcKey      =lcRetFab&lcFQualty+lcRetClr&lcFQualty
  lcFabric   =lcRetFab&lcFQualty
  lcColor    =lcRetClr&lcFQualty
  lnAdjStk   =lnDam&lcFQualty
  =SEEK(lcKey,'FABRIC')
ENDIF
lcToWare = ''
lnOldAdjStk=lnAdjStk

*khalid
=SEEK(lcKey,'FABRIC')
lnFabConv = Fabric.Conv

*B603950,1 ABD Open the file if not open when company use roll. [Begin]
IF !USED('MATINVJL')
  =gfOpenFile(gcDataDir+'MATINVJL', 'MATINVJL', 'SH')
ENDIF
*B603950,1 ABD [End]

*!C200254,1 ABD - Define Some variable to issue from rolls . [Begin]
lnOldAdjStk = lnAdjStk
IF (llRwIssRec .AND. lcOprtType = 'I') 
  lnAdjStk   = -1 * lnAdjStk
  lcRollNP   = lcSelecP
  lcRollRP   = lcUnSelP
  lcCostMeth = lcMtCstMth
ENDIF
*!C200254,1 ABD - [End]


DO lfJrRoData.PRG IN gfMatCrl.prg WITH .T.


*!C200254,1 ABD - get the ADS for issue Qty. [Begin]
 lnAdjStk   = ABS(lnAdjStk)
*!C200254,1 ABD - [End]

IF lnOldAdjStk <> lnAdjStk
  IF TYPE('lcFQualty') $ 'LU' 
    lnSok = lnAdjStk
    SHOW GET lnSok
  ELSE
    lnDam&lcFQualty=lnAdjStk
    SHOW GET lnDam&lcFQualty
  ENDIF
  *-- Adjust the Qty
  =lfvQty()
ENDIF

SELECT(lnAlias)


*-------------------------------------------------------------------------
*-------------------------- ( Save section ) -----------------------------
*!************************************************************************
*! Name      : lpSavScr
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!************************************************************************
*! Calls     : None
*!************************************************************************
*! Passed Parameters  :  None
*!************************************************************************
*! Returns            :  None
*!************************************************************************
*! Example            :  
*!************************************************************************
PROCEDURE lpSavScr
PRIVATE lnCrRec , lnRecCnt , lnRecs
SELECT (lcTmpLine)
lnRecCnt = RECCOUNT()
STORE 0 TO lnRecs
lnCrRec = IIF(EOF(),0,RECNO())

*!C200254,1 ABD - remark the next line and get the correct Type. [Begin]
*LOCATE FOR TranCd <> '1'
IF llRwIssRec
  lcOTrnType = IIF(llRwIssRec .AND. lcOprtType = 'I','1','2')
ELSE
  lcOTrnType = '1'
ENDIF
LOCATE FOR TranCd <> lcOTrnType
*!C200254,1 ABD - [End]

IF !FOUND()
  *--No [Receiving Lines] was done, Cannot update.
  = gfModalGen('INM34061B42000','DIALOG','Receiving Lines')
  STORE .F. TO llShow,llCSave
  *-- Check of phisical record before go to it.
  IF BETWEEN(lnCrRec,1,RECCOUNT())
    GOTO lnCrRec
  ENDIF
  RETURN .F.
ENDIF



*!C200254,1 ABD - Check for the on hand Qty. [Begin]
*-- If cost methd is lot , we should check for the on hand Qty.
IF llRwIssRec .AND. lcOprtType = 'I' .AND. !lfchkOnHnd()
  *- Message Text   :- There is no stock to issue your quantity, cannot update.
  *- Message No.    :- 36184.
  *- Buttom Message :- Ok.
  *- Buttom Number  :- 00000.
  = gfModalGen('INM36184B00000','DIALOG')
  STORE .F. TO llShow,llCSave
  RETURN .F.  
ENDIF
*!C200254,1 ABD - [End]


=gfOpenFile(gcDataDir+'MATINVJL', 'MATINVJL', 'SH')
=gfOpenFile(gcDataDir+'BOM'     , 'BOM'     , 'SH')
=gfOpenFile(gcDataDir+'BOMLINE' , 'BOMLINE' , 'SH')
=gfOpenFile(gcDataDir+'CTKTBOM' , 'CTKTBOM' , 'SH')

*C200255,1 (Begin) Update MATINVJL with custom Voucher no for CON10.
IF ASCAN(laEvntTrig,PADR("GETVOUT",10)) <> 0
  lcVoucNo = ""
  llContVout = .T.
  = gfDoTriger("MAPOREC",PADR("GETVOUT",10))
  IF !llContVout
    llCSave = .F.
    RETURN
  ENDIF
ENDIF  
*C200255,1 (Begin)

*--Open GLDIST file to call G/L Distribution Procedure.

IF llLinkToGl 
  = gfOpenFile(gcDataDir+'GLDIST','GlDistAc','SH')
  SELECT GLDist
  =AFIELDS(laFStru)
  =gfCrtTmp(lcGlDist,@laFStru)
ENDIF

SELECT (lcTmpLine)
GO TOP
SCAN
  lnRecs = lnRecs + 1
  *-- Ignore any line has no Qtys
  IF TOTSTK+TOTDAM+TOTCAN = 0
    LOOP
  ENDIF

  WAIT WINDOW 'Posting all transactions ... Item/Color :' + cfabric + '/' + color NOWAIT
  *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
  *lcKey = cmfgordno+cfabric+color+dyelot+cWarecode+STR(lineno,6)+trancd
  lcKey = &lcComField+cfabric+color+dyelot+cWarecode+STR(lineno,6)+trancd
  *!C200254,1 ABD - [End]
  SCATTER MEMVAR


  *--First we have to update the mmfgordd file and its estimated record in BOMLine file
  IF Newline
    =lfAdDetLin()
  ENDIF
  
  *!C200254,1 ABD - don't do the following in case u are in the mfg Rework order. [Begin]
  IF !llRwIssRec
    *!C200254,1 ABD - [End]
    IF TRANCD = '1'
      IF LNEWDYELOT
        DO lpCrtBom WITH 'T',&lcTmpLine..cMfgOrdNo,&lcTmpLine..cFabric,&lcTmpLine..Color,lcFJlSess
      ELSE
        *-- Create records of type '2' in BOMLINE if they do not already exist.
        DO GpCrtBom WITH 'T',&lcTmpLine..cMfgOrdNo,&lcTmpLine..cFabric,&lcTmpLine..Color,lcFJlSess
      ENDIF
    ENDIF
    FOR lnCount  = 1 TO 4
      Z          = STR(lnCount,1)
      lnLan_Cost = 0
      SELECT BOMLINE

      IF SEEK('T2' + m.cMFGOrdNo + STR(m.Lineno,6) + Z +PADR(m.cFabric,19) + m.Color)
        *-- Scan to sum this catigory into variable ==> lnLan_Cost       
        SCAN REST WHILE cImTyp  + cType + cTktNo + STR(Lineno,6) +;
                        cBomTyp + Style + SClr            =;
                        'T2' + m.cMFGOrdNo + STR(m.Lineno,6) + Z + ;
                        PADR(m.cFabric,19) + m.Color                    ;
                        FOR EMPTY(cRSession) .OR. cRSession = lcFJlSess
        
        
          IF &lcTmpLine..TranCd $ '23'
            lnLan_Cost = 0
          ELSE
            lnLan_Cost = lnLan_Cost + UnitCost*UnitQty
          ENDIF

          *-- Replace the following fields as :
          *-- StyQty    = StyQty  + Received quantity.
          *-- ItemQty   = ItemQty + 
          *--             Received quantity * Unit Quantity
          *-- ItemAmt   = ItemAmt +
          *--             Received quantity * Unit Quantity * Unit Cost
          *-- cRSession = the current session number.
          IF gfObj_Lock(.T.)
            REPLACE StyQty    WITH StyQty   + m.NmfgtotQty                      ,;
                    ItemQty   WITH ItemQty  + m.NmfgtotQty * UnitQty            ,; 
                    ItemAmt   WITH ItemAmt  + m.NmfgtotQty * UnitQty * UnitCost ,; 
                    cRSession WITH lcFJlSess
            =gfAdd_Info('BOMLINE')
            =gfObj_Lock(.F.)
          ENDIF
        ENDSCAN     
      ENDIF
      *-- Update landed cost fields in lcTmpLine file.
      
      SELECT (lcTmpLine)
      REPLACE &lcTmpLine..nLan_Cost&Z WITH lnLan_Cost
    ENDFOR
    *-- Now we get the 4 landed costs updated in the record type 1 in the 
    *-- lcTmpLine file .
    
    
    SCATTER MEMVAR &&-- Uppdate the memory variables with the new land cost .
    
    *!C200254,1 ABD - End IF for IF Statment. [Begin]
  ENDIF
  *!C200254,1 ABD - [End]
  *-- Uppdate the landed cost in the other trancd recordes  .
  *-- in the lcTmpLine with the unit costs befor we change it to totales
  *-- accomutaled in record trancd 1 in lcTmpLine
  *-- Change them to totals is not the standerd but it is my style to quick
  *-- uppdating the header record in mmfgordh .

  *----------------------------------  
  *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
  *SCAN REST WHILE cmfgordno+cfabric+color+dyelot+cWarecode = M.cmfgordno+M.cfabric+M.color+M.dyelot+m.cWarecode
  *IF !(TRANCD = '1')  
  SCAN REST WHILE &lcComField +cfabric+color+dyelot+cWarecode = ;
                  M.&lcComField+M.cfabric+M.color+M.dyelot+m.cWarecode
    *--Update MfgOprDt file
    IF !(TRANCD = lcOTrnType)
      *!C200254,1 ABD - [End]
      SELECT (lcTmpLine)
      lcLastOpr = clastopr
      IF !EMPTY(lcLastOpr)
        DIMENSION laRecvQty[9]
        STORE 0 TO laRecvQty
        laRecvQty[1] = nMFGTotQty
        laRecvQty[9] = nMFGTotQty
        
        *!C200254,1 ABD - don't do the following in case you are in the mfg Rework order. [Begin]
        *IF laRecvQty[9] > 0 
        IF laRecvQty[9] > 0 .AND. !llRwIssRec
          *!C200254,1 ABD - [End]
          =lfUpdLot(CMFGORDNO,lcLastOpr,&lcTmpLine..cLotNo,'laRecvQty' , PADR(&lcTmpLine..cFabric,19) + &lcTmpLine..Color + &lcTmpLine..Dyelot)
        ENDIF
        SELECT (lcTmpLine)
        =RLOCK()
        UNLOCK
      ENDIF
      lnRecs = lnRecs + 1
    ELSE
      lnTotStk = TOTSTK
      lntotDam = TOTDAM
      LOOP
    ENDIF
    REPLACE nLan_Cost1 WITH m.nLan_Cost1 , nLan_Cost2 WITH m.nLan_Cost2 ,; 
            nLan_Cost3 WITH m.nLan_Cost3 , nLan_Cost4 WITH m.nLan_Cost4

    
    *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
    *WAIT WINDOW 'Updating Order Detail File Order# ' + CMFGORDNO NOWAIT
    WAIT WINDOW 'Updating Order Detail File Order# ' +  &lcComField  NOWAIT
    
    
    *-- Update landed cost with the Est Cost while Issue.
    IF llRwIssRec .AND. lcOprtType = 'I'
      REPLACE nLan_Cost1 WITH m.nCost1 ,;
              nLan_Cost2 WITH m.nCost2 ,; 
              nLan_Cost3 WITH m.nCost3 ,;
              nLan_Cost4 WITH m.nCost4
    ENDIF
  
    *-- the progrm must call the gfmaterial control before update the
    *-- material rework or material manufact. file because this will cause 
    *-- a bug in the on order field.
    SELECT (lcTmpLine)
    IF TRANCD = '4'
      =lfUpdCanc()
    ELSE
      *-- call gfMatCrl
      =lfUppdate(lnRecs=lnRecCnt)
    ENDIF
    *!C200254,1 ABD - [End]

    *-- Insert all transctions lines into Mmfgordd file
    SELECT (lcTmpLine)
    SCATTER TO laX
    *B605212,1 (Begin) As there is a relation between this temp file and MMFGORDD on line no,
    *B605212,1 so if we received more than once it will always update the first line only.
    *!C200254,1 ABD - remark the next line and get the correct file. [Begin]
    *SELECT MMFGORDD 
    *B605212,1 (End)
    *INSERT INTO MMFGORDD FROM ARRAY laX
    *REPLACE MMFGORDD.cRSession WITH lcFJlSess,;
    *        MMFGORDD.dRecvDate WITH gdSysDate
    *IF Lineno > MMFGORDH.LASTLINE
    *  REPLACE MMFGORDH.LASTLINE WITH Lineno
    *ENDIF
    *=gfAdd_Info('MMFGORDD')
    *IF TRANCD = '4'
    *  =lfUpdCanc()
    *ELSE
    *  *-- call gfMatCrl
    *  =lfUppdate(lnRecs=lnRecCnt)
    *  IF TRANCD $ '23'
    *    FOR lnX=1 TO 4
    *      SELECT(lcTmpLine)
    *      Z    = STR(lnX,1)
    *      lnTotLan = nLan_Cost&Z * (lnTotStk + lntotDam)
    *      REPLACE nLan_Cost&Z WITH nLan_Cost&Z + lnTotLan
    *    ENDFOR
    *  ENDIF
    *ENDIF
    
    
    SELECT (lcBfileLin)
    INSERT INTO (lcBfileLin) FROM ARRAY laX
    REPLACE &lcBfileLin..cRSession WITH lcFJlSess,;
            &lcBfileLin..dRecvDate WITH gdSysDate
    IF Lineno > &lcBfileHdr..LASTLINE
      REPLACE &lcBfileHdr..LASTLINE WITH Lineno
    ENDIF
    =gfAdd_Info(lcBFileLin)
    *-- the progrm must call the gfmaterial control before update the
    *-- material rework or material manufact. file because this will cause 
    *-- a bug in the on order field.
    *!B606984,1 KHM 04/09/2003 (Begin) Commented out because the the landed costs are 
    *!B606984,1                calculated before.
    *IF !llRwIssRec .AND. &lcTmpLine..TRANCD # '4'
    *  IF TRANCD $ '23'
    *    FOR lnX=1 TO 4
    *      SELECT(lcTmpLine)
    *      Z    = STR(lnX,1)
    *      lnTotLan = nLan_Cost&Z * (lnTotStk + lntotDam)
    *      REPLACE nLan_Cost&Z WITH nLan_Cost&Z + lnTotLan
    *    ENDFOR
    *  ENDIF
    *ENDIF
    *!B606984,1 KHM 04/09/2003 (End)
    *!C200254,1 ABD - [End]
    STORE '' TO laX
    lcDummTr = &lcTmpLine..TRANCD
  ENDSCAN
  *------------------------------------------------------------  
  *-- Adjust the record pointer in the lcTmpLine
  *-- lnDummRec = RECNO(lcTmpLine)

  =SEEK(LEFT(lcKey,LEN(lcKey)-1) + lcDummTr , lcTmpLine)
  
  *-- make the landed cost feilds in the lcTmpLine carry the totals
  *-- i.e accomulate them in trancd='1' record
  
  *FOR lnX=1 TO 4
  *  Z    = STR(lnX,1)
  *  lnTotLan = nLan_Cost&Z*(TOTSTK+TOTDAM)
  *  REPLACE nLan_Cost&Z WITH lnTotLan
  *ENDFOR

  *HDM[START]
  *-- Read the transaction from the temp. file
  PRIVATE lnRecv , lnCanc , lnDamg , lnTmpLin , lcSerchKey
  STORE 0 TO lnRecv , lnCanc , lnDamg , lnTmpLin
  lcSerchKey = ""
  *-- GET THE RECEIVED QTY
  *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
  *lcSerchKey = &lcTmpLine..cmfgordno + &lcTmpLine..cfabric        +;
  *             &lcTmpLine..color + &lcTmpLine..dyelot + cWarecode +;
  *             STR(&lcTmpLine..lineno,6)
  lcSerchKey = &lcTmpLine..&lcComField + &lcTmpLine..cfabric        +;
               &lcTmpLine..color + &lcTmpLine..dyelot + cWarecode +;
               STR(&lcTmpLine..lineno,6)
  *!C200254,1 ABD - [End]
  
  lnTmpLin = IIF(EOF(lcTmpLine),0,RECNO(lcTmpLine))
  *HDM[START]
  *!C200254,1 ABD - remark the next line and get the correct Type. [Begin]
  *IF SEEK(lcSerchKey+'1',lcTmpLine)
  lcTrnType = IIF(llRwIssRec .AND. lcOprtType = 'R','2','1')
  IF SEEK(lcSerchKey+lcTrnType,lcTmpLine)
    *!C200254,1 ABD - [End]
    lnRecv = &lcTmpLine..TOTSTK
    lnDamg = &lcTmpLine..TOTDAM
    lnCanc = &lcTmpLine..TOTCAN
  ENDIF
  
  *!C200254,1 ABD - remark the next line and get the correct Type. [Begin]
  *IF SEEK(lcSerchKey+'2',lcTmpLine)
  lcXTrnType = IIF(llRwIssRec .AND. lcOprtType = 'R','3','2')  
  IF SEEK(lcSerchKey+lcXTrnType,lcTmpLine)
    *!C200254,1 ABD - [End]
    lnRecv = &lcTmpLine..NMFGTOTQTY
  ENDIF
  *IF SEEK(lcSerchKey+'3',lcTmpLine)
  *  lnDamg = &lcTmpLine..NMFGTOTQTY
  *ENDIF
  IF SEEK(lcSerchKey+'4',lcTmpLine)
    lnCanc = &lcTmpLine..NMFGTOTQTY
  ENDIF
  *HDM[END]
  IF lnTmpLin >0
    GO lnTmpLin
  ENDIF
  *GO lnDummRec
  *HDM[END]
  
  *!C200254,1 ABD - remark the next line and get the correct File. [Begin]
  *SELECT MMFGORDH
  *=SEEK(&lcTmpLine..cFabric+&lcTmpLine..cmfgordno)  
  SELECT (lcBfileHdr)
  =SEEK(&lcTmpLine..cFabric+&lcTmpLine..&lcComField)
  *!C200254,1 ABD - [End]
  
  *IF gfObj_Lock(.T.)
    *-- now uppdate fields RECEIVE , DAMAGED , CANCELED in the header file . 
    *HDM[START]
    *REPLACE RECEIVED    WITH RECEIVED+&lcTmpLine..TOTSTK ,;
            DAMAGED     WITH DAMAGED +&lcTmpLine..TOTDAM ,;
            CANCELED    WITH CANCELED +&lcTmpLine..TOTCAN
    *!C200254,1 ABD - [End]
    IF !llRwIssRec .OR. (llRwIssRec .AND. lcOprtType = 'R')
      *!C200254,1 ABD - [End]
      REPLACE RECEIVED    WITH RECEIVED + lnRecv ,;
              DAMAGED     WITH DAMAGED  + lnDamg ,;
              CANCELED    WITH CANCELED + lnCanc

      *!C200254,1 ABD - End if For If Statment. [End]
    ENDIF          
     *lnDamg
    *HDM[END]

    *-- Decrease the open qty due to the origenal records only not the added ones .
    IF !NEWLINE
      *HDM[START]
      *REPLACE NmmfgOpen WITH NmmfgOpen-(&lcTmpLine..TOTSTK+&lcTmpLine..TOTDAM+&lcTmpLine..TOTCAN)
      REPLACE NmmfgOpen WITH NmmfgOpen-(lnRecv + lnDamg + lnCanc)
      *HDM[END]
      *!C200254,1 ABD - remark the next line and get the correct File. [Begin]
      *IF MMFGORDH.NmmfgOpen <= 0 
      IF &lcBfileHdr..NmmfgOpen <= 0
        *!C200254,1 ABD - [End]
        REPLACE NmmfgOpen WITH 0
        *!C200254,1 ABD - remark the next line and get the correct File. [Begin]        
        *-- I will complete the rework order if received >= buget Qty.
        lnTotRecv = &lcBfileHdr..RecEived + &lcBfileHdr..DAMAGED + &lcBfileHdr..CANCELED
        IF !llRwIssRec .OR. (llRwIssRec .AND. lcOprtType = 'R' .AND. lnTotRecv >= &lcBfileHdr..nMmgbudget )
          *!C200254,1 ABD - [End]
          REPLACE Status WITH 'C'
        ENDIF
        *!C200254,1 ABD - [End]
      ENDIF
    ENDIF

    **-- Now uppdate (Accomulate) the landed cost in the header file .
    *FOR lnX=1 TO 4
    *  Z    = STR(lnX,1)
    *  REPLACE nLan_Cost&Z WITH nLan_Cost&Z+&lcTmpLine..nLan_Cost&Z
    *ENDFOR
    
    *!C200254,1 ABD - remark the next line and update the correct File. [Begin]
    *=gfAdd_Info('MMFGORDH')
    =gfAdd_Info(lcBFileHdr)
    *!C200254,1 ABD - [End]
    =gfObj_Lock(.F.)
    * ENDIF && endif gfObj_Lock(.T.)  
ENDSCAN

*C200254,1 ABD - Update the landed cost with the Est. Cost.[Begin]
IF llRwIssRec .AND. lcOprtType = 'R'
  SELECT (lcBfileHdr)
  REPLACE nlan_Cost1 WITH nEst_COST1,;
          nlan_Cost2 WITH nEst_COST2,;
          nlan_Cost3 WITH nEst_COST3,;
          nlan_Cost4 WITH nEst_COST4
ENDIF
*C200254,1 ABD - [End]

*B606984,1 KHM 04/09/2003 (Begin) Function to update the landed costs in the header file
*B606984,1                in case of receiving material manufacturing order.
IF !llRwIssRec 
  =lfUpdLdCst()
ENDIF  
*B606984,1 KHM 04/09/2003 (End)

*--Update WhsLoc----------------------------------------------------
IF llWareLoc 
  SELECT WHSLOC
  SET ORDER TO TAG WhsLocSt
  SELECT (lcTemLoc)
  SCAN FOR !EMPTY(clocation)
    IF !SEEK(lcItem+lcColor+cWareCode+cLocation,'WHSLOC')
      SCATTER MEMVAR
      INSERT INTO WhsLoc FROM MEMVAR
      REPLACE cOwner WITH ' '
      =gfAdd_Info('WhsLoc')
    ENDIF
  ENDSCAN
  DELETE ALL
ENDIF

*--
*!C200254,1 ABD - remark the next line and don't do the following 
*!C200254,1 ABD - In case you are in the mfg Rework order. [Begin]
IF !llRwIssRec
  *!C200254,1 ABD - [End]
  SELECT BomLine
  SCAN
    IF SEEK('T2'+&lcTmpLine..cMFGOrdNo)
      DELETE REST WHILE CIMTYP+cType+cTktNo='T2'+&lcTmpLine..cMFGOrdNo FOR EMPTY(cRSession)
    ENDIF
  ENDSCAN
  *!C200254,1 ABD - End if for if statment. [Begin]
ENDIF
*!C200254,1 ABD - [End]

*--Update GlDist----------------------------------------------------
IF llLinkToGl
  SELECT (lcGlDist)
  REPLACE ALL GLSESSION WITH lcFJlSess
  SELECT GLDIST
  APPEND FROM (gcWorkDir+lcGlDist)
ENDIF  
*HDM[START] ADD IF Linked to GL Condition
*SELECT (lcGlDist)
*DELETE ALL

IF llLinkToGl
  SELECT (lcGlDist)
  DELETE ALL
ENDIF
*HDM[END]
*-- update landed cost in header file
**-- Now uppdate (Accomulate) the landed cost in the header file .

*!C200254,1 ABD - remark the next line and get the correct File. [Begin]
*SELECT MMFGORDD
SELECT (lcBFileLin)
*!C200254,1 ABD - [End]

lcLastOrd = ORDER()
SET ORDER TO Mfgrec
=SEEK(lcFJlSess)
*!C200254,1 ABD - remark the next line and get the correct field. [Begin]
*SCAN REST WHILE crsession+cmfgordno+cfabric+color+trancd = lcFJlSess FOR TRANCD $ '23'
SCAN REST WHILE crsession+&lcComField+cfabric+color+trancd = lcFJlSess FOR TRANCD $ '23'
  *!C200254,1 ABD - [End]
  *B803591 AAN Correcty spelling word Applieng to Applying and Order to order
  *WAIT WINDOW 'Applieng landed cost for Order header file' NOWAIT
  WAIT WINDOW 'Applying landed cost for order header file' NOWAIT
  *B803591 (END)
  
  *!C200254,1 ABD - remark the next line and get the correct File. [Begin]
  *SELECT MMFGORDD
  *=SEEK(MMFGORDD.CFABRIC+MMFGORDD.cmfgordno)  
  SELECT (lcBFileLin)
  =SEEK(&lcBFileLin..CFABRIC+&lcBFileLin..&lcComField)
  *!C200254,1 ABD - [End]
  FOR lnX=1 TO 4
    Z    = STR(lnX,1)
    
    *!C200254,1 ABD - remark the next line and get the correct File. [Begin]
    *REPLACE nLan_Cost&Z WITH nLan_Cost&Z + (MMFGORDD.nLan_Cost&Z * MMFGORDD.NMFGTOTQTY)
    REPLACE nLan_Cost&Z WITH nLan_Cost&Z + (&lcBFileLin..nLan_Cost&Z * &lcBFileLin..NMFGTOTQTY)
    *!C200254,1 ABD - [End]
    
  ENDFOR
ENDSCAN
SET ORDER TO (lcLastOrd)
*C200256,1 (Begin) Receive Qty for CON10.
IF ASCAN(laEvntTrig,PADR("PRNTIT",10)) <> 0
  SELECT (lcTmpLine)
  COUNT FOR TRANCD = "2" TO lnPrgnO  
  LOCATE FOR TRANCD = "2"
  IF FOUND()
    llFromCost = .F.
    SET FILTER TO TRANCD = "2"
    lnNoOfCop = 1
    lcTranTit = "Mfg #"
    lcTrans   = "cmfgordno"
    lcTotQty  = "nMfgTotQty"
    lcRecDate = "LDRCVDATE"
    IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Do you wish to print the received quantities?')=1)
      = gfDoTriger("MAPOREC",PADR("PRNTIT",10))    
   ENDIF
 ENDIF  
ENDIF  
*C200256,1 (Begin)

*B605212,1 (Begin) Call Bar Code Printing Report.
*                  Would you like to print Barcode labels ?
*B605761,1 WAB (Start)
*SELECT Rolls
*LOCATE
*IF llTrkRolls .AND. ( lcMtCstMth = "L" OR !EOF('Rolls') )
*  IF gfModalGen('QRM36171B42002','DIALOG','Rolls label') = 1
IF llTrkRolls 
  SELECT Rolls
  LOCATE
  IF ( lcMtCstMth = "L" OR !EOF('Rolls') ) AND ;
     gfModalGen('QRM36171B42002','DIALOG','Rolls label') = 1
    *B605761,1 WAB (End)
    PRIVATE lcRunRep , lcRepDir , lcCallFrom
    lcRunRep  = 'MABARC'           && Calling BarCode labels with another report ID
    lcRepDir = gcRepHome+gcAct_Appl+'REPORT'  && To call MaReport with 'MaBarc'
    lcCallFrom = "P"      && Report is called from Program from Transaction menu
    llFromMan = .T.    
    DO &lcRepDir WITH lcRunRep
  ENDIF
ENDIF
*B605212,1 (End)
SELECT (lcTmpFile)
ZAP
SELECT (lcTmpLine)
DELETE ALL
=lfClearInfo()
*!************************************************************************
*! Name      : lfUppdate
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!************************************************************************
*! Calls     : None
*!************************************************************************
*! Passed Parameters  :  None
*!************************************************************************
*! Returns            :  None
*!************************************************************************
*! Example            :  
*!************************************************************************
*!

FUNCTION lfUppdate
PARAMETER llLastLn
PRIVATE lnalias,lcSeekKey,lcLinkCode,lcLinkPO,lnNewCost,lnUseCost,lnUseQty,lcFabric,lcColor,lnLineNo,llExtCall
llExtCall = .T.

DIMENSION laOtherPar[6,2]

laOtherPar[1,1] = 'lnLineNo'
laOtherPar[1,2] = &lcTmpLine..LineNo
laOtherPar[2,1] = 'lcProg'
laOtherPar[2,2] = 'MAMNREC'

laOtherPar[3,1] = 'lcOrgFab'
laOtherPar[3,2] = IIF(EMPTY(&lcTmpLine..cFabric1) , '' , &lcTmpLine..cFabric)

laOtherPar[4,1] = 'lcOrgClr'
laOtherPar[4,2] = IIF(EMPTY(&lcTmpLine..cFabric1),'',&lcTmpLine..Color)

laOtherPar[5,1] = 'lcOrgDye'
laOtherPar[5,2] = IIF(EMPTY(&lcTmpLine..cFabric1),'',&lcTmpLine..Dyelot)

laOtherPar[6,1] = 'lllastrec'
laOtherPar[6,2] = llLastLn

lnLineNo   = &lcTmpLine..LineNo
lcLinkCode = IIF(EMPTY(FABDYE.Gl_Link),Fabric.Link_Code,FABDYE.Gl_Link)

*!C200254,1 ABD -  Get the Link Code from the selected header file. [Begin]
*lcLinkPO   = MMFGORDH.LINK_CODE
lcLinkPO   = &lcBFileHdr..LINK_CODE
*!C200254,1 ABD -  [End]

lcFabric   = IIF(EMPTY(&lcTmpLine..cFabric1),&lcTmpLine..cFabric,&lcTmpLine..cFabric1)
lcColor    = IIF(EMPTY(&lcTmpLine..cFabric1),&lcTmpLine..Color,&lcTmpLine..cColor1)

lnalias = SELECT(0)
lcSeekKey = IIF(EMPTY(&lcTmpLine..cFabric1),&lcTmpLine..cFabric+&lcTmpLine..Color,&lcTmpLine..cFabric1+&lcTmpLine..cColor1)

IF !EMPTY(&lcTmpLine..cFabric1)
  =SEEK(lcSeekKey,'Fabric')
ENDIF

SELECT FABDYE
*HDM[START]
*-- If the dyelot not found in fabdye file
*-- add it using (gpAdFabWar)
IF llDyelot AND !EMPTY(&lcTmpLine..Dyelot) AND !SEEK (lcSeekKey+&lcTmpLine..cWareCode+PADR(&lcTmpLine..Dyelot,10))
  =gpAdFabWar(IIF(EMPTY(&lcTmpLine..cFabric1),&lcTmpLine..cFabric,&lcTmpLine..cFabric1),;
              IIF(EMPTY(&lcTmpLine..cFabric1),&lcTmpLine..COLOR,&lcTmpLine..cCOLOR1),;
               &lcTmpLine..Dyelot , &lcTmpLine..cWareCode)
ENDIF

*IF SEEK (lcSeekKey+&lcTmpLine..cWareCode+SPACE(10))
IF SEEK (lcSeekKey+&lcTmpLine..cWareCode+PADR(&lcTmpLine..Dyelot,10))
  *-- Use fabric link_code if the warehouse has no link code

  *!C200254,1 ABD - get the cost from nCost not from nlan_cost while issue.  [Begin]
  IF llRwIssRec .AND. lcOprtType = 'I' 
    lnNewCost  = nCost1+nCost2+nCost3+nCost4  
  ELSE
    lnNewCost  = nLan_Cost1+nLan_Cost2+nLan_Cost3+nLan_Cost4
  ENDIF
  *!C200254,1 ABD - [End]

  lnUseCost  = lnNewCost/Fabric.Conv
  *HDM[START] If receive to cancell lnUseQty=0
  *lnUseQty   = &lcTmpLine..nMfgTotQty*Fabric.Conv
  *!C200254,1 ABD - Get the correct Type. [Begin]
  *lnUseQty   = IIF(&lcTmpLine..TRANCD='4',0,&lcTmpLine..nMfgTotQty*Fabric.Conv)  
  IF llRwIssRec .AND. lcOprtType = 'I'
    lnUseQty   = IIF(&lcTmpLine..TRANCD='4',0,-1*(&lcTmpLine..nMfgTotQty*Fabric.Conv))
  ELSE
    lnUseQty   = IIF(&lcTmpLine..TRANCD='4',0,&lcTmpLine..nMfgTotQty*Fabric.Conv)      
  ENDIF
  *!C200254,1 ABD - [End]
  *HDM[END]
  
  IF llLinkToGl
    DECLARE laGLDistAr[2,13]
    laGLDistAr[1,1] = lcLinkCode
    laGLDistAr[2,1] = lcLinkPO
    laGLDistAr[1,2] = '015'
    laGLDistAr[2,2] = '013'
    laGLDistAr[1,3] = 1
    laGLDistAr[2,3] = -1
    STORE 'MM'                  TO laGLDistAr[1,4],laGLDistAr[2,4]
    
    *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
    *STORE &lcTmpLine..cmfgordno TO laGLDistAr[1,5],laGLDistAr[2,5]
    STORE &lcTmpLine..&lcComField TO laGLDistAr[1,5],laGLDistAr[2,5]
    *!C200254,1 ABD - [End]
    
    STORE ldPost                TO laGLDistAr[1,6],laGLDistAr[2,6]
    STORE lcGlFYear             TO laGLDistAr[1,7],laGLDistAr[2,7]
    STORE lcGlPeriod            TO laGLDistAr[1,8],laGLDistAr[2,8]
    STORE lcGlDist              TO laGLDistAr[1,9],laGLDistAr[2,9]
    laGLDistAr[2,10] = ''
  ELSE
    DIME laGLDistAr[1,1]
    laGLDistAr = ''
  ENDIF
  *ldRcvDate
  *= gfMatCrl('1',lcFabric,lcColor,;
                 &lcTmpLine..cWareCode,&lcTmpLine..Dyelot,;
                 &lcTmpLine..Drecvdate,ldPost,&lcTmpLine..cmfgordno,;
                 lnUseQty,lnUseCost,;
                 &lcTmpLine..Reference,'',;
                 0,'','',@laGLDistAr,lcFJlSess,'','','','','','','',@laOtherPar)

  *!C200254,1 ABD - remark the next line and get the correct field. [Begin]
  *-- in case of issue don't assign to 'P' lcKeyType this variable till
  *-- the gfMatCrl to receive new roll not issue.
  IF llRwIssRec .AND. lcOprtType = 'I'
    lcKeyType = ''
  ENDIF
  *= gfMatCrl('1',lcFabric,lcColor,;
                 &lcTmpLine..cWareCode,&lcTmpLine..Dyelot,;
                 ldRcvDate,ldPost,&lcTmpLine..cmfgordno,;
                 lnUseQty,lnUseCost,;
                 &lcTmpLine..Reference,'',;
                 0,'','',@laGLDistAr,lcFJlSess,'','','','','','','',@laOtherPar)


  = gfMatCrl('1',lcFabric,lcColor,;
                 &lcTmpLine..cWareCode,&lcTmpLine..Dyelot,;
                 ldRcvDate,ldPost,&lcTmpLine..&lcComField,;
                 lnUseQty,lnUseCost,;
                 &lcTmpLine..Reference,'',;
                 0,'','',@laGLDistAr,lcFJlSess,'','','','','','','',@laOtherPar)

  IF llRwIssRec .AND. lcOprtType = 'I'
    lcKeyType = 'P'               
  ENDIF
  *!C200254,1 ABD - [End]
ENDIF
SELECT (lnalias)  

*!*************************************************************
*! Name      : lfvPrcDye
*! Developer : Hossam El Etreby [HDM]
*! Date      :
*! Purpose   : Valid function for proceed button in Dyelot 
*!           : Allocation Screen
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Returns   :
*!*************************************************************

FUNCTION lfvPrcDye

SELECT (lcTmpDyeAl)
IF lnOvr > 0
  WAIT WINDOW 'No allocations was done'
  CLEAR READ
  RETURN .F.
ENDIF


SCAN FOR !EMPTY(cCarton)
  *-- Add Cancellation record in tempfile for the allocated dyelots
  SELECT (lcTmpLine)

  *-- UPDATE THE BUDGET RECORD WITH THE Allocated dyelot QTY
  =SEEK(lcKey)
  *!C200254,1 ABD - Get the correct field. [Begin]
  *LOCATE REST WHILE cmfgordno + cfabric            + color            + dyelot =;
                    lcKey     + &lctmpLine..cFabric + &lctmpLine..Color + &lcTmpDyeAl..dyelot;
                    FOR TRANCD = '1'
  LOCATE REST WHILE &lcComField. + cfabric            + color            + dyelot =;
                    lcKey     + &lctmpLine..cFabric + &lctmpLine..Color + &lcTmpDyeAl..dyelot;
                    FOR TRANCD = lcBrTrncd
  
  *!C200254,1 ABD - [END]
  *=RLOCK()
  *REPLACE NMFGTOTQTY WITH NMFGTOTQTY + (&lcTmpDyeAl..NMFGTOTQTY - &lcTmpDyeAl..OldTOTQTY)
  *UNLOCK

  SCATTER MEMVAR MEMO

  *-- Add Cancellation record For Original Dyelot
  =SEEK(lcKey)
  
  *!C200254,1 ABD - Get the correct field. [Begin]
  *LOCATE REST WHILE cmfgordno+cfabric+color+dyelot = lcKey;
              FOR DYELOT = &lcTmpDyeAl..Dyelot AND TRANCD = '4'
  LOCATE REST WHILE &lcComField. +cfabric+color+dyelot = lcKey;
              FOR DYELOT = &lcTmpDyeAl..Dyelot AND TRANCD = '4'
  *!C200254,1 ABD - [End]
  
  IF FOUND()
    M.nMFGTotQty = nMFGTotQty
  ELSE
    M.nMFGTotQty = 0
  ENDIF
  IF !FOUND()
    APPEND BLANK
    M.NMFGTOTQTY = 0
  ENDIF
  IF !EOF()
    M.TRANCD     = '4'
    M.DYELOT     = &lcTmpDyeAl..DYELOT
    M.LINENO     = &lcTmpDyeAl..LINENO
    M.NEWLINE    = .F.
    M.LNEWDYELOT = .F.
    M.NMFGTOTQTY = M.NMFGTOTQTY + ((&lcTmpDyeAl..OldTOTQTY - &lcTmpDyeAl..NMFGTOTQTY))
    M.TOTCAN     = M.TOTCAN + M.NMFGTOTQTY
    GATHER MEMVAR MEMO
  ENDIF
  
ENDSCAN
CLEAR READ
*-- Add Budget record for the new dyelot
*-- Add Receiving Line for the new dyelot


*!*************************************************************
*! Name      : lfAloDye
*! Developer : Hossam El Etreby [HDM]
*! Date      :
*! Purpose   : Function to display the Dyelot allocation screen
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Returns   :
*!*************************************************************

FUNCTION lfAloDye
PARAMETER lcKey
PRIVATE lcOldMFTag , lnLastAl

lnLastAl = SELECT(0)
*-- Create Cursor that holds the Old Dyelots
lcTmpDyeAl = gfTempName()

DIMENSION laDyeStruc[8,4]

laDyeStruc[1,1] = 'cCarton'
laDyeStruc[1,2] = 'C'
laDyeStruc[1,3] = 2
laDyeStruc[1,4] = 0

*!C200254,1 ABD - Get the correct field. [Begin]
*laDyeStruc[2,1] = 'CMFGORDNO'
laDyeStruc[2,1]  = lcComField
*!C200254,1 ABD - [End]

laDyeStruc[2,2] = 'C'
laDyeStruc[2,3] = 6
laDyeStruc[2,4] = 0

laDyeStruc[3,1] = 'CFABRIC'
laDyeStruc[3,2] = 'C'
laDyeStruc[3,3] = 7
laDyeStruc[3,4] = 0

laDyeStruc[4,1] = 'COLOR'
laDyeStruc[4,2] = 'C'
laDyeStruc[4,3] = 6
laDyeStruc[4,4] = 0

laDyeStruc[5,1] = 'Dyelot'
laDyeStruc[5,2] = 'C'
laDyeStruc[5,3] = 10
laDyeStruc[5,4] = 0

laDyeStruc[6,1] = 'NMFGTOTQTY'
laDyeStruc[6,2] = 'N'
laDyeStruc[6,3] = 12
laDyeStruc[6,4] = 3

laDyeStruc[7,1] = 'OldTOTQTY'
laDyeStruc[7,2] = 'N'
laDyeStruc[7,3] = 12
laDyeStruc[7,4] = 3

laDyeStruc[8,1] = 'LINENO'
laDyeStruc[8,2] = 'N'
laDyeStruc[8,3] = 6
laDyeStruc[8,4] = 0


PRIVATE lnOrder
DIMENSION laReccs[1]
STORE 0 TO laReccs , lnOrder

CREATE CURSOR (lcTmpDyeAl) FROM ARRAY laDyeStruc
*!C200254,1 ABD - Get the correct Filek. [Begin]
*SELECT MMFGORDD
SELECT(lcBFileLin)
*!C200254,1 ABD - [End]

lcOldMFTag = ORDER()

*!C200254,1 ABD - Select the correct Order for current file. [Begin]
IF llRwIssRec
  SET ORDER TO TAG MARWLIN
ELSE
  *!C200254,1 ABD - [End]
  SET ORDER TO Mmfgordd
  *!C200254,1 ABD - End If for If statment. [Begin]
ENDIF
*!C200254,1 ABD - [End]



*cmfgordno+STR(lineno,6)+trancd
*!C200254,1 ABD - Get the correct field. [Begin]
*lcKey = &lcTmpLine..cmfgordno && + STR(&lcTmpLine..lineno,6) &&+ &lcTmpLine..trancd
lcKey = &lcTmpLine..&lcComField.
*!C200254,1 ABD - [End]

lnCount = 0

*!C200254,1 ABD - Get the correct field. [Begin]
*IF SEEK(lcKey,'MMFGORDD')
*  SCAN REST WHILE cmfgordno+STR(lineno,6)+trancd = lcKey;
*FOR CFABRIC+COLOR = &lcTmpLine..CFABRIC + &lcTmpLine..COLOR
IF SEEK(lcKey,lcBFileLin)
  SCAN REST WHILE &lcComField.+STR(lineno,6)+trancd = lcKey;
    FOR CFABRIC+COLOR = &lcTmpLine..CFABRIC + &lcTmpLine..COLOR
    *!C200254,1 ABD - [End]
    lcDetDye = DYELOT
    lcDyeLnNo = STR(lineno,6)
    *!C200254,1 ABD - Get the correct Type. [Begin]
    *IF TRANCD = '1'
    *lnOrder = MMFGORDD.NMFGTOTQTY    
    IF TRANCD = lcBrTrncd
      lnOrder  = &lcBFileLin..NMFGTOTQTY
      *!C200254,1 ABD - [End]
      lnToAdd = RECNO()
      llXFound = .F.
      *!C200254,1 ABD - Get the correct Type. [Begin]
      *SCAN REST WHILE cmfgordno+STR(lineno,6)+trancd = lcKey + lcDyeLnNo FOR TRANCD <> '1'
      SCAN REST WHILE &lcComField.+STR(lineno,6)+trancd = lcKey + lcDyeLnNo FOR TRANCD <> lcBrTrncd
        *!C200254,1 ABD - [End]
        lnOrder = lnOrder - NMFGTOTQTY
        *-- scan the temp file to get the old entered entreies
        SELECT(lcTmpLine)
        lnRecNo = RECNO()
      ENDSCAN
      SKIP -1
      IF lnOrder > 0
        lnCount = lnCount + 1
        DIMENSION laReccs[lnCount,2]
        laReccs[lnCount,1] = lnToAdd
        laReccs[lnCount,2] = lnOrder
      ENDIF
    ENDIF
  ENDSCAN
  FOR lnXLoop = 1 TO lnCount
    *!C200254,1 ABD - Get the correct File. [Begin]
    *SELECT MMFGORDD
    SELECT (lcBFileLin)
    *!C200254,1 ABD - [End]
    
    GO laReccs[lnXLoop,1]
    SCATTER MEMVAR MEMO
    SELECT (lcTmpDyeAl)
    APPEND BLANK
    *!C200254,1 ABD - Update With Correct Field. [Begin]
    *REPLACE CMFGORDNO  WITH M.CMFGORDNO        ,;
            CFABRIC    WITH M.cFABRIC          ,;
            COLOR      WITH M.COLOR            ,;
            DYELOT     WITH M.DYELOT           ,;
            NMFGTOTQTY WITH laReccs[lnXLoop,2] ,;
            OldTOTQTY  WITH laReccs[lnXLoop,2] ,;
            LINENO     WITH M.LINENO
    REPLACE &lcComField.  WITH M.&lcComField.  ,;
            CFABRIC       WITH M.cFABRIC          ,;
            COLOR         WITH M.COLOR            ,;
            DYELOT        WITH M.DYELOT           ,;
            NMFGTOTQTY    WITH laReccs[lnXLoop,2] ,;
            OldTOTQTY     WITH laReccs[lnXLoop,2] ,;
            LINENO        WITH M.LINENO
    *!C200254,1 ABD - [End]
  ENDFOR
ELSE
  WAIT WINDOW 'Order lines Not Found'
  RETURN .F.
ENDIF
lcDyeBrTl = 'Select Dyelot'
lcDyeKey  = lcKey

MAAloDy = gfTempName()
maalod1 = gfTempName()
maalod2 = gfTempName()
maalod3 = gfTempName()
GO TOP
SELECT (lcTmpDyeAl)

lnOvr = lnSok + lnDam1 + lnDam2
lcDispKey = lcTCode+'/' + &lcTmpLine..Cfabric + '/' + &lcTmpLine..color + '/' + &lcTmpLine..dyelot
DO (gcScrDir+gcWinAppl+"\MAAloDy.SPX")
*-- Restore environment settings
*!C200254,1 ABD - get the the old order to current file. [Begin]
*SET ORDER TO lcOldMFTag IN MMFGORDD
SELECT (lcBfileLin)
SET ORDER TO lcOldMFTag
*!C200254,1 ABD - [End]

SELECT(lnLastAl)
RETURN (lnOvr = 0)

*!*************************************************************
*! Name     : lfAloDBr
*! Developer: 
*! Date     : 10/10/97               
*! Purpose  : Function to browse available dyelots to alocate.
*!*************************************************************
FUNCTION lfAloDBr

lcBlDFields = "cCarton    :R :2 :H='  ',"+;
              "Dyelot     :R :H='Dyelot',"+;
              "NMFGTOTQTY :R :H='Total Qty.'"

BROWSE FIELDS &lcBlDFields ;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       TITLE (lcDyeBrTl) ;
       WHEN lfwDyEBr()   ;
       WINDOW MAALOD2 ;
       IN WINDOW MAAloDy
RETURN

*!*************************************************************
*! Name     : lfwDyEBr
*! Developer: 
*! Date     : 12/19/1999               
*! Purpose  : When function for the dyelots allocation browse
*!*************************************************************
FUNCTION lfwDyEBr

IF EMPTY(cCarton)
  IF lnOvr =0
    SHOW GET pbSel1  DISABLE
    SHOW GET pbUnSel DISABLE
    RETURN
  ELSE
    SHOW GET pbSel1  ENABLE
    SHOW GET pbUnSel DISABLE
    RETURN
  ENDIF
ELSE
  SHOW GET pbSel1  DISABLE
  SHOW GET pbUnSel ENABLE
  RETURN
ENDIF


*IF EMPTY(cCarton)
*  SHOW GET pbSel1  ENABLE
*  SHOW GET pbUnSel DISABLE
*ELSE
*  SHOW GET pbSel1  DISABLE
*  SHOW GET pbUnSel ENABLE
*ENDIF

*!*************************************************************
*! Name    : lfvSelDye
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Select dyelot.
*!*************************************************************
FUNCTION lfvSelDye

REPLACE cCarton WITH '>>'

REPLACE NMFGTOTQTY WITH MAX((NMFGTOTQTY-lnOvr),0)
lnOvr = MAX((lnOvr - OldTOTQTY),0)
=lfwDyEBr()

=lfRefresh('MAAloD1')
SHOW WINDOW (lcDyeBrTl) REFRESH
RETURN

*!*************************************************************
*! Name    : lfvUSlDye
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : UnSelect dyelot.
*!*************************************************************
FUNCTION lfvUSlDye

REPLACE cCarton WITH SPACE(3)

lnOvr = lnOvr + (OldTOTQTY - NMFGTOTQTY)
REPLACE NMFGTOTQTY WITH NMFGTOTQTY + (OldTOTQTY - NMFGTOTQTY)
=lfwDyEBr()

=lfRefresh('MAALOD1')
SHOW WINDOW (lcDyeBrTl) REFRESH
RETURN

**********************************************************
*B603880,1 KAM change function name because we have another one with the same name[start
*FUNCTION lfGetInfo
FUNCTION OLdlfGetInfo
PARAMETER lcRecKey
PRIVATE lnCurRec


SELECT MMFGORDD
lnCurRec = RECNO()

*-- Locate rest for another record with the same item/color
*-- in the same order


LOCATE REST WHILE cMFGOrdNo   + cFabric   + Color = ;
                  lcTcode + lcItem + lcColor ;
                  FOR TranCd = '1'
*-- Restore the record pointer.

GO lnCurRec

IF FOUND() 
  IF !lfBrowLines(lcColor) 
    lcColor    = lcOldVal
    _CUROBJ    = OBJNUM(lcColor)
  ENDIF
ELSE
  SCATTER MEMVAR MEMO
  SELECT (lcTmpLine)
  GATHER MEMVAR MEMO
  =lfReadLine()
  DO lpControl WITH llUseDyelot
  SHOW WINDOW (lcBrowTitl) REFRESH SAME
  =lfwBrow()
ENDIF  

************************************************************************
FUNCTION lfvObjects
PARAMETER lcObject

*-- Check if called from color field And it's the lase field in the key
*-- call lpGetOpn Proc.
*-- Otherwise if called from Dyelot field
*-- and it's the last object in the key
*-- call lpGetOpn Proc.
*-- otherwise
*-- DO NOTHING

DO CASE
  CASE lcObject ='D' AND llUseDyelot
    llCall = .T.
  CASE lcObject ='C' AND !llUseDyelot
    llCall = .T.
  OTHERWISE
    llCall = .F.
ENDCASE
IF llCall
*  DO lpGetOpen WITH .T.
ENDIF
******************************************
FUNCTION lfGetInfo
*B603880,1 KAM define variable and assign them withe values[start]
*--lcOldArea hold the coming area
PRIVATE lcOldArea,lnRecNo
lcOldArea=select()
lnRecNo  =RECNO()
*B603880,1 KAM [end]

STORE 0 TO lnLinRec ,lnLinDam ,lnLinCan

*!C200254,1 ABD - remark the next line and get the correct File. [Begin]
*SELECT MMFGORDD
SELECT (lcBfileLin)
*!C200254,1 ABD -  [End]

lnLine = LineNo

*!C200254,1 ABD - remark the next line and get the correct field. [Begin]
*lcOrd  = PADR(cMFGOrdNo,6)
*SCAN REST WHILE cmfgordno + STR(LineNo,6) + TRANCD  = lcOrd+STR(lnLine,6)
lcOrd  = PADR(&lcComField,6)
SCAN REST WHILE &lcComField + STR(LineNo,6) + TRANCD  = lcOrd+STR(lnLine,6)
*!C200254,1 ABD -  [End]
  
  *!C200254,1 ABD - Get the correct Types. [Begin]
  IF llRwIssRec
    IF TRANCD <> '1'
      DO CASE
        CASE TRANCD = '3'
          lnLinRec = lnLinRec + Nmfgtotqty
        CASE TRANCD = '4'
          lnLinDam = lnLinDam + Nmfgtotqty
        CASE TRANCD = '6'
          lnLinCan = lnLinCan + Nmfgtotqty
      ENDCASE
    ENDIF
  ELSE
    *!C200254,1 ABD - [End]
    
    IF TRANCD <> '1'
     DO CASE
        CASE TRANCD = '2'
          lnLinRec = lnLinRec + Nmfgtotqty
        CASE TRANCD = '3'
          lnLinDam = lnLinDam + Nmfgtotqty
        CASE TRANCD = '4'
          lnLinCan = lnLinCan + Nmfgtotqty
      ENDCASE
    ENDIF
    *!C200254,1 ABD - End for if statment. [Begin]
  ENDIF
  *!C200254,1 ABD - [End]
  
ENDSCAN
*B603880,1 KAM we cancel calling seek function and use to direct record[start]
*=SEEK(lcOrd+STR(lnLine,6),'MMFGORDD')
SELECT(lcOldArea)
IF BETWEEN(lnRecNo,1 ,RECCOUNT(lcOldArea))
  GO lnRecNo
ENDIF
*B603880,1 KAM [end]
*******************************************
FUNCTION lfAdDetLin
PRIVATE lnAlias

lnAlias = SELECT(0)

*!C200254,1 ABD - remark the next line and get the correct File. [Begin]
*IF !(MMFGORDH.lMultiWare)
*  M.cWareCode = MMFGORDH.cWareCode
*ENDIF
*SELECT MMFGORDD

IF !(&lcBfileHdr..lMultiWare)
  M.cWareCode = &lcBfileHdr..cWareCode
ENDIF
SELECT (lcBfileLin)
*!C200254,1 ABD - [End]

SET ORDER TO Mmfgorddln
*!C200254,1 ABD - remark the next line to add line in recive.[Begin]
*IF M.TRANCD = '1'
IF M.TRANCD = lcBrTrncd
   *!C200254,1 ABD - [End]
  *!C200254,1 ABD - remark the next line and get the correct field & also 
  *!C200254,1 ABD - check for the line with correct value. [Begin]
  *IF !(SEEK(M.cMFGOrdNo + STR(M.LineNo,6) + '1'))
  *IF !(SEEK(M.&lcComField + STR(M.LineNo,6) + '1'))
  lcOldOrdrL = ORDER()
  IF !llRwIssRec
    SET ORDER TO Mmfgordd
  ELSE
    SET ORDER TO Marwlin
  ENDIF
  IF SEEK (&lcComField. + cfabric + color + dyelot,lcBFileLin)
    SELECT (lcBFileLin)
    LOCATE REST WHILE &lcComField.+cfabric+color+dyelot+trancd  = ;
    &lcTmpLine..&lcComField. + &lcTmpLine..cfabric + &lcTmpLine..color + &lcTmpLine..dyelot;
    FOR cwarecode = &lcTmpLine..cwarecode .AND. trancd = lcBrTrncd  .AND. STR(lineNo,6) = STR(M.LineNo,6)
    IF !FOUND()
      APPEND BLANK
    ENDIF
   ELSE
    *!C200254,1 ABD - [End]
    APPEND BLANK
  ENDIF
  SET ORDER TO &lcOldOrdrL
ELSE
  APPEND BLANK
ENDIF
IF lNewDyelot
  STORE 0 TO M.NCOST1,M.NCOST2,M.NCOST3,M.NCOST4
ENDIF

*!C200254,1 ABD - Change the tranced while saving. [Begin]
*- I should Update the TRANCD With 1 Not 2 while saving,
*- This type '1' will be as an recored in the Marwlin file.
IF llRwIssRec .AND. lcOprtType = 'R' .AND. llDyelot .AND. &lctmpLine..Trancd = '2' 
  m.Trancd = '1'
ENDIF
*!C200254,1 ABD - [End]

GATHER MEMVAR MEMO

*!C200254,1 ABD - remark the next line and get the correct File. [Begin]
*SELECT MMFGORDH
SELECT (lcBfileHdr)
*!C200254,1 ABD - [End]

*-- UPDATE ORDER HEADER BUDGET FIELD
REPLACE NMMGBUDGET WITH NMMGBUDGET + M.Nmfgtotqty

*!C200254,1 ABD - don't do the following in case mfg Rework order. [Begin]
IF !llRwIssRec
  *!C200254,1 ABD - [End]
  *-- ADD the cost sheet budget line
  *-- Prepare the sent parameters to the global function.
  =gfOpenFile(gcDataDir+'CTKTBOM' , 'CTKTBOM' , 'SH')
  llNoThing  = SEEK("T"+CMFGORDNO, "CTktBom")
  llNoThing  = SEEK(CMFGORDNO    , "MMFGORDH")
  lcLinkCode = CTktBom.Link_Code
ENDIF

lnLineNo   = &lcTmpLine..LineNo
lcFabric   = &lcTmpLine..cFabric
lcColor    = &lcTmpLine..Color
lcDyelot   = &lcTmpLine..Dyelot
lcWareCode = &lcTmpLine..cWareCode

lnEst1     = &lcTmpLine..ncost1
lnEst2     = &lcTmpLine..ncost2
lnEst3     = &lcTmpLine..ncost3
lnEst4     = &lcTmpLine..ncost4

*!C200254,1 ABD - remark the next line and get the correct File. [Begin]
*lcOrdNo    = &lcTmpLine..CMFGORDNO
*=SEEK(&lcTmpLine..CFABRIC + lcOrdNo , 'MMFGORDH')
*lcLastOpr  = MMFGORDH.cLastOpr
lcOrdNo    = &lcTmpLine..&lcComField
=SEEK(&lcTmpLine..CFABRIC + lcOrdNo , lcBfileHdr)
lcLastOpr  = &lcBfileHdr..cLastOpr
*!C200254,1 ABD - [End]

*-- The array to be sent to the global function should hold
*-- the new values - the old values
SELECT (lcTmpLine)
DIMENSION laBomQty[9]
STORE 0 TO laBomQty
lnOfsQty = (nMFGTotQty)

laBomQty[1] = lnOfsQty
laBomQty[9] = lnOfsQty

*-- Open the BomLine file.

= gfOpenFile(gcDataDir+"BomLine","BomLine","SH")

*-- Call the global function to
*-- Update MMO line no in cost sheet files

*-- Make the active idex begins with order No.

*!C200254,1 ABD - don't do the following in case mfg rework Order. [Begin]
IF  !llRwIssRec
  *!C200254,1 ABD - [End]
  SELECT MMFGORDH
  lcHdOrd = ORDER()
  SET ORDER TO Mmfgordh

  WAIT WINDOW 'Updating Cost sheet for fabric/color: ' + lcFabric +"/"+ lcColor NOWAIT
  *lnRecNo = IIF(EOF(lcTmpMMOLn) , 0 , RECNO(lcTmpMMOLn))
  llRetVal = gfSheetItem("T"       , lcOrdNo   , lcLinkCode         , lcFabric          , lcColor   ,;
                         lnLineNo  , lcDyelot  , MMFGORDH.cWareCode , MMFGORDH.cMatWare , @laBomQty ,;
                         "Bom"     , "CtktBom" , "BomLine"          , "MFGOprHd"        , lcLastOpr ,;
                         0         , 0         , 0                  , 0                 , 0         ,;
                         0         )
  *!C200254,1 ABD - End For IF statment. [Begin]
ENDIF

*!C200254,1 ABD - Select Corect file. [Begin]
*SELECT MMFGORDH
SELECT (lcBfileHdr)
*!C200254,1 ABD - [Emd]


IF lNewDyelot
  REPLACE NEST_COST1 WITH NEST_COST1 - lnEst1,;
          NEST_COST2 WITH NEST_COST2 - lnEst2,;
          NEST_COST3 WITH NEST_COST3 - lnEst3,;
          NEST_COST4 WITH NEST_COST4 - lnEst4,;
          cLastOpr   WITH lcLastOpr
ENDIF
*!C200254,1 ABD - Set order to old order. [Begin]
IF !llRwIssRec
  *!C200254,1 ABD - [End]
  SET ORDER TO (lcHdOrd)
  *!C200254,1 ABD - End FI For IF Statment. [Begin]
ENDIF
*!C200254,1 ABD - [End]

WAIT CLEAR
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfUpdLot
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Function to update reciving quantity.
*!*************************************************************
*! Parameters: lcCuTick   ---> P/O or C/T number.
*!             lcOpration ---> Operation code.
*!             lcRecvLot  ---> Selected lot no.
*!             laQuantity ---> Array holding Received quantity
*!             lcRcvItem  ---> Received Style.
*!*************************************************************
FUNCTION lfUpdLot
PARAMETERS lcCTCode, lcOpration, lcRecvLot, laQuantity, lcRcvItem
PRIVATE laAddQty, laOpenQty, lcSaveRecKey, lnCount, lcCount, lcLotNo

DECLARE laAddQty[9], laOpenQty[9]

llLotFound = .F.
*If not found add a budjet record for last operaion.
SELECT MFGOPRDT
SET ORDER TO MFGOPRTR
*Get the right record of style/color/dyelot

*B604856,1 AMH Get the correct opration [Start]
*=SEEK('T'+lcCTCode+lcOpration+lcRecvLot+'1')
*LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
                'T'+lcCTCode+lcOpration+lcRecvLot+'1' ;
            FOR Item+COLOR+cDyelot = lcRcvItem
=SEEK('T'+lcCTCode+PADR(lcOpration,6)+lcRecvLot+'1')
LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
                'T'+lcCTCode+PADR(lcOpration,6)+lcRecvLot+'1' ;
            FOR Item+COLOR+cDyelot = lcRcvItem
*B604856,1 AMH [End]

*If found and not a cancelled record
IF !FOUND()
  IF !(&lcTmpLine..TranCd = '4' )

    *Add the operation to the seek expression
    =SEEK('T'+lcCTCode+lcOpration)
    SCATTER MEMVAR MEMO
    SELECT CODES
    
    *B604856,1 AMH Get the correct opration [Start]
    *=SEEK('N'+lcOpration+'Y'+'MFGCODE   ','CODES')
    =SEEK('N'+PADR(lcOpration,6)+'Y'+'MFGCODE   ','CODES')
    *B604856,1 AMH [End]
    
    LOCATE REST WHILE cDefCode+ccode_no+crltfield+cfld_name = ;
                      'N'+lcOpration+'Y'+'MFGCODE   ' FOR CRLTD_NAM = 'CCONTCODE'
    SELECT MFGOPRDT
    *Get dyelot to update in the file
    m.cDyelot = &lcTmpLine..Dyelot
    APPEND BLANK
    GATHER MEMVAR MEMO
    
    *B604856,1 AMH Fix the bug of wrong receiving the M.M.O. operation [Start]
    *REPLACE cIMtyp    WITH 'T',;
            cTktNo    WITH lcCTCode,;
            Item      WITH &lcTmpLine..CFABRIC,;
            COLOR     WITH &lcTmpLine..COLOR,;
            cLotNo    WITH lcRecvLot ,;
            cOprCode  WITH lcOpration,;
            cContcode WITH IIF(EMPTY(cContcode),ALLT(CODES.cRltd_vlu),cContcode),;
            dTranDate WITH gdSysDate ,;
            DueDate   WITH gdSysDate ,;
            TranCd    WITH '1'       ,;
            cTrgOpr   WITH SPACE(2)  ,;
            cTrgLot   WITH SPACE(2)  ,;
            nLotQty1   WITH 0,;
            nLotQty2   WITH 0,;
            nLotQty3   WITH 0,;
            nLotQty4   WITH 0,;
            nLotQty5   WITH 0,;
            nLotQty6   WITH 0,;
            nLotQty7   WITH 0,;
            nLotQty8   WITH 0,;
            nLotTotQty WITH 0
    REPLACE cIMtyp     WITH 'T',;
            cTktNo     WITH lcCTCode,;
            Item       WITH &lcTmpLine..CFABRIC,;
            COLOR      WITH &lcTmpLine..COLOR,;
            cLotNo     WITH lcRecvLot ,;
            cOprCode   WITH lcOpration,;
            cContcode  WITH IIF(EMPTY(cContcode),ALLT(CODES.cRltd_vlu),cContcode),;
            dTranDate  WITH gdSysDate ,;
            DueDate    WITH gdSysDate ,;
            TranCd     WITH '1'       ,;
            cTrgOpr    WITH SPACE(2)  ,;
            cTrgLot    WITH SPACE(2)  ,;
            nLotTotQty WITH EVALUATE(laQuantity+'[9]')
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1,0)
      REPLACE nLotQty&lcI. WITH EVALUATE(laQuantity+'['+lcI+']')
    ENDFOR
    *B604856,1 AMH [End]
    
  ENDIF
ENDIF
*B604856,1 AMH Get the correct opration [Start]
*=SEEK('T'+lcCTCode+lcOpration)
*DO WHILE cIMTyp+cTktNo+cOprCode = 'T'+lcCTCode+lcOpration AND !EOF()
=SEEK('T'+lcCTCode+PADR(lcOpration,6))
DO WHILE cIMTyp+cTktNo+cOprCode = 'T'+lcCTCode+PADR(lcOpration,6) AND !EOF()
*B604856,1 AMH [End]

  *Add dyelot to the comparing item
  IF Item+Color+cDyelot <> lcRcvItem
    SKIP
    LOOP
  ENDIF
  IF !EMPTY(lcRecvLot) AND cLotNo<>lcRecvLot
    SKIP
    LOOP
  ENDIF

  lcLotNo    = cLotNo
  llLotFound = .T.
  *--Check if there is any open quantity for this Opr/Lor/Clr
  STORE 0 TO laOpenQty
  
  *B604856,1 AMH Get the correct opration [Start]
  *SCAN REST WHILE cIMTyp+cTktNo+cOprCode+cLotNo='T'+lcCTCode+lcOpration+lcLotNo ;
              FOR Item + Color + cDyelot=lcRcvItem
  SCAN REST WHILE cIMTyp+cTktNo+cOprCode+cLotNo='T'+lcCTCode+PADR(lcOpration,6)+lcLotNo ;
              FOR Item + Color + cDyelot=lcRcvItem
  *B604856,1 AMH [End]
  
    laOpenQty[1] = MAX(laOpenQty[1] + ;
                       IIF(TranCd='1',nLotTOTQty,-nLotTOTQty),0)
    laOpenQty[9] = laOpenQty[9] + laOpenQty[1]
  ENDSCAN
  *--Comupte quantity to be added for this Opr/Lot/Clr
  STORE 0 TO laAddQty  
  IF laOpenQty[9] > 0  
    FOR lnCount = 1 TO 1
      laAddQty[lnCount]    = MIN(&laQuantity[lnCount],laOpenQty[lnCount])
      &laQuantity[lnCount] = &laQuantity[lnCount] - laAddQty[lnCount]
      laAddQty[9]          = laAddQty[9]    + laAddQty[lnCount]
      &laQuantity[9]       = &laQuantity[9] - laAddQty[lnCount]
    ENDFOR
    IF laAddQty[9]>0
      lnSavRec=RECNO()
      =lfAppDetRec(lcLotNo, &lcTmpLine..TranCd,'laAddQty')
      
      *Check of phisical record before go to it.
      IF BETWEEN(lnSavRec,1,RECCOUNT())
        GOTO lnSavRec
      ENDIF  

    ENDIF
  ENDIF  
ENDDO
*--If there is over received quantity, Update the last lot with the remaind quantity.
*Don't create the cancel record of over receiving

IF llLotFound AND &laQuantity[9] > 0 .AND. !((&lcTmpLine..TranCd = '4' ) .OR. (&lcTmpLine..TranCd='5'))
  =lfAppDetRec(lcLotNo, &lcTmpLine..TranCd,'&laQuantity')
ENDIF
SET ORDER TO MFGOPRDT IN MFGOPRDT
RETURN

*!*************************************************************
*! Name      : lfAppDetRec
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose   : Function to append transaction record in the
*!             MFG operation detailed file.   
*!*************************************************************
*! Parameters: lcLotNum   ---> Lot number
*!             lcTranCode ---> Transaction code
*!             laNewQty   ---> Array holding Quantity to be added
*!*************************************************************
FUNCTION lfAppDetRec
PARAMETERS lcLotNum, lcTranCode, laNewQty

*--lcTranCode = IIF(lcTranCode='4','3',IIF(lcTranCode='5','4',lcTranCode))

SELECT MFGOPRDT

*B604856,1 AMH Get the correct opration [Start]
*=SEEK('T'+lcCTCode+lcLastOpr+lcLotNum+'1')
=SEEK('T'+lcCTCode+PADR(lcLastOpr,6)+lcLotNum+'1')

*Get the right record of style/color/dyelot
*LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
                  'T'+lcCTCode+lcLastOpr+lcLotNum+'1' ;
              FOR Item+Color+cDyelot = lcRcvItem
LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
                  'T'+lcCTCode+PADR(lcLastOpr,6)+lcLotNum+'1' ;
              FOR Item+Color+cDyelot = lcRcvItem
*B604856,1 AMH [End]

SCATTER MEMVAR MEMO
APPEND BLANK
GATHER  MEMVAR MEMO
REPLACE nLotTotQty WITH &laNewQty[9] ,;
        dTranDate  WITH gdSysDate    ,;
        DueDate    WITH {}           ,;
        TranCd     WITH lcTranCode   ,;
        cTrgOpr    WITH SPACE(2)     ,;
        cTrgLot    WITH SPACE(2)     ,;
        cOwner     WITH ' '
=gfAdd_Info('MFGOPRDT')
*--Call TraceKey global function.
=gfTraceKey('MFGOPRDT',MFGOPRDT.cimtyp+MFGOPRDT.ctktno+MFGOPRDT.coprcode+MFGOPRDT.clotno+MFGOPRDT.trancd,'A')
RETURN(.T.)

*!*************************************************************
*! Name      : lfSelLots
*! Developer : HDM
*! Date      : 
*! Purpose   : Function to select lot number to receive to.
*!*************************************************************
*! Parameters: Style line no.
*!*************************************************************
*! Returns   : Selected lot number 
*!*************************************************************
FUNCTION lfSelLots
PARAMETER lcTktLn
PRIVATE llOpnBomLn , llOpnOprDt
lnAlias = SELECT()

llOpnBomLn = gfOpenFile(gcDatadir+'BOMLINE','BOMLINE','BOMLINE')
llOpnOprDt = gfOpenFile(gcDatadir+'MfgOprDt','MfgOprDt','Mfgopr')

SELECT MfgOprDt
lcMFOldTg = ORDER('MFGOPRDT')
SET ORDER TO TAG TktOpTrn

= gfOpenFile(gcDataDir+"CODES","CODES","SH")


lcLineNo = STR(&lcTmpLine..LineNo,6)
SELECT BOMLINE
lcBmOldTg = ORDER('BOMLINE')
SET ORDER TO TAG Mfgopr

*--Get the Last operation for this Style/Color LINE NO.
STORE ' ' TO lcLastMfgOr ,lcRecvLot

*-- Get the last operation from bomline file insted of mfgoprdt.
SELECT BOMLINE
SEEK 'T'+lcTCode
SCAN WHILE cIMTyp+cTktNo = 'T'+lcTCode ;
       FOR STR(LineNo,6) = lcLineNo AND cCatgTyp='M' AND !EMPTY(MfgCode)
  *--Check if this MFG operation code works as operation.
  llWrkAsOpr = .F.
  IF SEEK('N'+BOMLINE.MfgCode+'Y'+'MFGCODE   ','CODES')
    SELECT CODES
    LOCATE REST WHILE cDefCode+ccode_no+crltfield+cfld_name = ;
      'N'+BOMLINE.MfgCode+'Y'+'MFGCODE   ' FOR CRLTD_NAM = 'LMFGOPR'
    IF FOUND()
      llWrkAsOpr = ALLT(CODES.cRltd_vlu) = "T"
    ENDIF
  ENDIF
  SELECT BOMLINE
  IF !llWrkAsOpr
    LOOP
  ENDIF
  *--Read last operation.
  lcLastMfgOr = BOMLINE.MfgCode
ENDSCAN
*--If not empty last operation , read receive lot.

IF !EMPTY(lcLastMfgOr)
  lcStyle = PADR(&lcTmpLine..cfabric,19) + &lcTmpLine..COLOR
  SELECT MFGOPRDT
  =SEEK('T'+lcTCode+lcLastMfgOr + '1')
  lnNumLots = 0
  COUNT REST TO lnNumLots ;
          WHILE cIMTyp+cTktNo+cOprCode+TranCd = ;
                'T'+lcTCode+lcLastMfgOr+'1' FOR Item+Color+CDYELOT = lcStyle + &lcTmpLine..Dyelot .AND. nLotTotQty >0
  IF lnNumLots <> 0
    =SEEK('T'+lcTCode+lcLastMfgOr+'1')
    LOCATE REST WHILE cIMTyp+cTktNo+cOprCode+TranCd = ;
                 'T'+lcTCode+lcLastMfgOr+'1' FOR Item+Color = lcStyle .AND. cDyelot = lcDyelot
    DO CASE
      CASE lnNumLots = 1
        lcRecvLot = cLotNo
      CASE lnNumLots > 1
        *--Do you want to select a lot to receive to or distribute received quantity to all lots?,\<Select;\<Distribute
        IF gfModalGen('QRM42108B42011','DIALOG') = 1
          lcBrFields = [cLotNo    :H='Lot No.',]+;
                       [cOprCode  :H='Operation',]+;
                       [cContCode :H='Cont./Dept.',]+;
                       [cContName :H='Name',]+;             
                       [dTranDate :H='Trans.Date',]+;
                       [dueDate   :H='Due Date',]+;
                       [nLotTotQty :H='Total']
          DIMENSION laTemp[1]
          laTemp = ''
          =ARIABROW(['T'+lcTCode+lcLastMfgOr+'1' FOR Item + Color = lcStyle;
                  .AND. cDyelot = lcDyelot],'Lots',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','cLotNo','laTemp')
          lcRecvLot=laTemp[1]
          IF !EMPTY(lcRecvLot)
            llSpecLot = .T.
            laLotArry = 0
            SELECT MfgOprDt
            SEEK 'T'+lcTCode+lcLastMfgOr+'1'       
            SCAN WHILE cIMTyp+cTktNo+cOprCode = 'T'+lcTCode+lcLastMfgOr;
                  FOR Item + Color = lcStyle AND cLotNo = lcRecvLot
              FOR I=1 TO 8
                Z=STR(I,1)
                laLotArry[I]=laLotArry[I]+(NLOTQTY&Z * IIF(TranCd='1',1,-1))
              ENDFOR
            ENDSCAN
          ENDIF 
        ENDIF                
    ENDCASE      
  ELSE
    lcRecvLot = '01'
  ENDIF
ENDIF

*--Set Order Tags Back.
SELECT MfgOprDt
SET ORDER TO TAG &lcMFOldTg
SELECT BOMLINE
SET ORDER TO TAG &lcBmOldTg
SELECT (lnAlias)
IF EMPTY(lcRecvLot)
  lcRecvLot = '01'
ENDIF
*--Read last operation for color.
lcClrLstOp = lcLastMfgOr
lcRecvLot  = IIF(EMPTY(lcRecvLot),SPACE(2),lcRecvLot)
RETURN(lcRecvLot)

******************************************
FUNCTION lfUpdCanc

*-- Update Fabric File
IF SEEK(cFabric + Color,'Fabric')
  SELECT FABRIC
  REPLACE ONORDER WITH MAX((ONORDER - &lcTmpLine..nMFGTotQty),0)
ENDIF

*!C200254,1 ABD - Updet the correct record for the dyelot as the gfmatrctl. [Begin]
*IF SEEK(&lcTmpLine..cFabric + &lcTmpLine..Color + MMFGORDD.CWARECODE + &lcTmpLine..DYELOT,'FABDYE')
IF SEEK(&lcTmpLine..cFabric + &lcTmpLine..Color + &lcTmpLine..CWARECODE,'FABDYE')
  *!C200254,1 ABD - [End]
  SELECT FABDYE
  REPLACE ONORDER WITH MAX((ONORDER - &lcTmpLine..nMFGTotQty),0)
ENDIF
*****************************************
PROCEDURE lpCrtBom
PARAMETERS lcTrType,lcTket,lcItem,lcColor,lcSess,lcStyQlt

tktlntp1 = gfTempName()

SELECT BOMLINE
SET ORDER TO TAG BomLine

IF lcTrType='T'
  lcItem=PADR(lcItem,19)
ENDIF

lcSeekKey = IIF(lcTrType='S','I',lcTrType)+'2'+lcTket
lcWhleCnd = "cIMTyp+cType+cTktNo+IIF(lcTrType $ 'IS',STR(LineNo,6),'')=lcSeekKey"

lcForCond = "Style+IIF(lcTrType='T',SClr,'')=lcItem+IIF(lcTrType='T',lcColor,'') AND ( EMPTY(cRSession) OR cRSession=lcSess )"
lcQltFltr = IIF(TYPE('lcStyQlt') $ 'UL' ,".T.","cStyGrade = lcStyQlt")

=SEEK(lcSeekKey)
LOCATE REST WHILE &lcWhleCnd FOR &lcForCond AND &lcQltFltr

*--Chech for type '1'.
lcSeekKey=IIF(lcTrType='S','I',lcTrType)+'1'+lcTket
lcForCond="Style+IIF(lcTrType='T',SClr,'')=lcItem+IIF(lcTrType='T',lcColor,'') AND !lVoid AND LineNo = m.LineNo"
=SEEK(lcSeekKey)
LOCATE REST WHILE &lcWhleCnd FOR &lcForCond

*--Create '2' from '1'. 
IF FOUND()   
  COPY REST WHILE &lcWhleCnd FOR &lcForCond TO (gcWorkDir+TktLnTp1)
  =gfOpenFile(gcWorkDir+TktLnTp1,'','EX')
  SELECT (TktLnTp1)
  REPLACE ALL cType     WITH '2',;
              ShipNo    WITH IIF(lcTrType='S',lcColor,''),;
              cStyGrade WITH IIF(lcQltFltr<>".T.",lcStyQlt,''),;
              StyQty  WITH  0,;
              ItemQty WITH  0,;
              ItemAmt WITH  0,;
              lineno  WITH  m.LineNo
  USE
  SELECT BOMLINE
  APPEND FROM (gcWorkDir+TktLnTp1)
  ERASE (gcWorkDir+TktLnTp1+'.DBF')
ENDIF
RETURN

*:*************************************************************
*: Name       : lfvType
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 04/24/2002
*: Purpose    : Disable the Button After Selection.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfvType()
*:*************************************************************
*!C200254,1
FUNCTION lfvType

lcOprtType = laType[lntype,2]
lcBrTrncd = IIF(!llRwIssRec,'1',IIF(lcOprtType = 'I','1','2'))
SHOW GET lnType DISABLE

*-- End OF lfvType
*:*************************************************************
*: Name      : gfRWOrdBr
*: Developer : Abdou Elgendy
*: Date      : 04/24/2002
*: Purpose   : Global browse for material maunufacturing orders
*:             If a match is found, returns, otherwise, browses
*:             Positions the file pointer of MaRwhdr on the 
*:             selected or matching record.
*:             Does not require the file to be opened.
*:*************************************************************
*: Parameters: - lcMfgRwNum : (optional)
*:                            MFG Order number to be validated.
*:                            Browses all orders if not passed.
*:             - lcFabric   : (optional)
*:                            Use this parameter with a fabric
*:                            to select (or validate) a fabric, 
*:                            then browse only the orders that
*:                            use this fabric. 
*:             - lcForCond  : Allows further scoping of the 
*:                            dislayed records. (optional)                          
*:                            A condition that is to be browsed
*:                            for.
*:*************************************************************
*: Calls     : AriaBrow()
*:*************************************************************
*: Returns            :  .t. if a record is found or selected,
*:                       .f. otherwise
*:*************************************************************
*: Example            :   If only validating MFG Order number, 
*:                          llFound = gfRWOrdBr(@lcMFGOrder)
*:                        Or if browsing orders using a fabric 
*:                          llFound = gfRWOrdBr(@lcMFGOrder, 'COTTON')
*:                        If a condition is required :
*:                          (eg browse only open and complete orders)
*:                          llFound = gfRWOrdBr(@lcMFGOrder,;
*:                                               'COTTON',;
*:                                               'Status $ 'OC')
*:*************************************************************
FUNCTION gfRWOrdBr
PARAMETERS lcMfgRwNum, lcFabric, lcForCond

PRIVATE lcMfgRwNum, lcFabric, lcForCond, lcBrFields, lnCurAlias, ;
        lnCurTag, llFound, llOpenFile, laData


DECLARE laData[1]  && array to get values from browse
STORE '' TO laData

llBrowse   = IIF(TYPE('llBrowse')   = 'U', .F., llBrowse) 
lcMfgRwNum = IIF(TYPE('lcMfgRwNum') $ 'UL', SPACE(6), lcMfgRwNum)
lcFabric   = IIF(TYPE('lcFabric')   $ 'UL' , SPACE(7), lcFabric)
lcForCond  = IIF(EMPTY(lcForCond), '', ' FOR ' +lcForCond)

lcWareHouse  = gfGetMemVar('M_WareHouse')
lcBrFields   = [cMfgRwNum:H='Order#',]+;
               [cFabric  :H='Fabric',]+;
               IIF(lcWareHouse = 'Y',;
                   [cWareCode:H='Warehouse',],[])+;
               [Status   :H='S',]+;
               [Entered  :H='Entered',]+;
               [Complete :H='Complete']

lnCurAlias   = SELECT()
IF USED('MaRwhdr')
  llOpenFile = .F.
  SELECT MaRwhdr
ELSE  
  llOpenFile = gfOpenFile(gcDataDir+'MaRwhdr',gcDataDir+'MaRwlin','SH')
ENDIF  
lnCurTag     = VAL(SYS(21))
SET ORDER TO TAG MaRwlin

llFound = .F.
IF !EMPTY(lcFabric) .AND. SEEK(lcFabric) ;
   .OR. EMPTY(lcFabric) .AND. SEEK('')
  SET ORDER TO TAG MaRwhdr  
  IF !(SEEK(lcMfgRwNum)  ;
    .AND. (EMPTY(lcForCond) .OR. EVALUATE(STRTRAN(lcForCond, 'FOR')))) ;
    .OR. llBrowse 

    IF BETWEEN(RECNO(0), 1, RECCOUNT())
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF  
    IF EMPTY(lcFabric)
      llFound = ARIABROW(lcForCond,;
                "Material Rrework Orders",;
                gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","",;
                "MaRwhdr.cMfgRwNum","laData")
    ELSE             
      SET ORDER TO TAG MaRwLin
      llFound = ARIABROW([lcFabric] + lcForCond,;
                "Material Manufacturing Rework Orders",;
                gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","",;
                "MaRwhdr.cMfgRwNum","laData")
    ENDIF  
    IF llFound
      lcMfgRwNum = laData[1]
    ELSE
      lcMfgRwNum = SPACE(6)
    ENDIF  
  ELSE
    llFound  = .T.
  ENDIF  
ELSE
  llFound = .F.
  lcMfgRwNum = SPACE(6)
  IF EMPTY(lcFabric)
    =gfDialog('I','There are no material manufacturing rework orders on the file.')
  ELSE
    =gfDialog('I','There are no material manufacturing rework orders for item '+ALLTRIM(lcFabric) +'.')  
  ENDIF  
ENDIF

IF llOpenFile
  USE IN MaRwhdr
ELSE
  SET ORDER TO (lnCurTag)
ENDIF
SELECT (lnCurAlias)

RETURN llFound

*- End OF gfRWOrdBr
*:*************************************************************
*: Name       : lfChekLn
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 04/24/2002
*: Purpose    : check For Lines in the marwLin file.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfvType()
*:*************************************************************
*!C200254,1 
FUNCTION lfChekLn
PRIVATE llReturn , lctrancd , lnAlias , lcmfgrwnum,;
        lcfabric , lcColor  , lcdyelot, lnRecNo
lnAlias = SELECT(0)
llReturn = .F.
lctrancd = ''
IF SEEK(lcTCode,lcBfileLin)
  lctrancd  = IIF(lcOprtType = 'I','1','2')
  SELECT (lcBFileLin)
  lnRecNo    = RECNO()
  lcmfgrwnum = cmfgrwnum
  lcfabric   = cfabric
  lcColor    = color
  lcdyelot   = dyelot
  
  LOCATE REST WHILE cmfgrwnum+cfabric+color+dyelot+trancd = ;
                    lcmfgrwnum+lcfabric+lcColor+lcdyelot ; 
  FOR  trancd =  lctrancd
  IF FOUND()
    llReturn = .T.
  ENDIF
  
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF
ELSE
  llReturn = .F.
ENDIF

SELECT(lnAlias)

RETURN llReturn

*-- End OF lfChekLn
*:*************************************************************
*: Name       : lfchkOnHnd
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 04/24/2002
*: Purpose    : Check in the fabric file for the on hand Qty.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfchkOnHnd()
*:*************************************************************
*!C200254,1 
FUNCTION lfchkOnHnd
PRIVATE llReturn , lnAlias,lnRecNo,lnIssueQty
lnIssueQty = 0
llReturn = .F.

lnAlias = SELECT(0)
lnRecNo = RECNO()
IF SEEK(cFabric+Color+cWarecode+Dyelot,'FABDYE')
  *-- Check for the on hand Qty.
  IF FabDye.Onhand <= 0
    llReturn = .F.
  ELSE
   SELECT (lcTmpLine)
   LOCATE
   SCAN FOR TRANCD = '2'
     lnIssueQty = lnIssueQty + &lcTmpLine..nMfgTotQty
   ENDSCAN
   
   IF lnIssueQty > FabDye.Onhand
     llReturn = .F.
   ELSE
     llReturn = .T.
   ENDIF
   
  ENDIF
ENDIF
SELECT (lcTmpLine)
GOTO lnRecNo
SELECT (lnAlias)
RETURN llReturn
*-- End OF lfchkOnHnd
*:*************************************************************
*: Name       : lfCrtTmp
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 04/24/2002
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
*!C200254,1 
FUNCTION lfCrtTmp
PRIVATE lnAlias

lnAlias = SELECT(0)

IF !USED('MATINVJL')
  =gfOpenFile(gcDataDir+'MATINVJL', 'MATINVJL', 'SH')
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

*-- End OF lfCrtTmp
*:*************************************************************
*: Name       : lfOldLots
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 04/24/2002
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
*!C200254,1 
FUNCTION lfOldLots
PRIVATE llDone,lnAlias,lnLotQty,lnActQty,lcKey

lnAlias = SELECT()
llDone  = .T.
lcExp   = &lcTmpLine..cfabric+&lcTmpLine..COLOR+&lcTmpLine..cwarecode+PADR(&lcTmpLine..Dyelot,10)

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
  CASE lcMtCstMth $ 'LF' 
    SET ORDER TO TAG lcTmpJour ASCENDING
  CASE lcMtCstMth = 'I'
    SET ORDER TO TAG lcTmpJour DESCENDING
ENDCASE
IF lcMtCstMth $ 'FI' 
  lnLotQty = lnSok
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
*:*************************************************************
*: Name       : lfOldLots
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 04/24/2002
*: Purpose    : 
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfOldLots()
*:*************************************************************
*!C200254,1 

FUNCTION lfGetTran
PARAMETER lcField, lcExpr

RETURN LOOKUP(MATINVJL.&lcField,lcExpr,MATINVJL.cfabric,'MATINVJL')

*-- End Of lfGetTran
*:*************************************************************


*:*************************************************************
*: Name       : lfUpdLdCst
*: Developer  : Khalid Mohi El-Din (KHM)
*: Date       : 04/09/2003
*: Purpose    : 
*:*************************************************************
*: Example    : = lfUpdLdCst()
*:*************************************************************
*B606984,1 KHM 04/09/2003 
*:*************************************************************
FUNCTION lfUpdLdCst
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT(lcTmpLine)
LOCATE
SCAN FOR TranCd $ '23'
  IF SEEK(cFabric+cMfgOrdNo,lcBfileHdr)
    SELECT (lcBfileHdr)
    FOR lnCnt = 1 TO 4
      lcCnt   = STR(lnCnt,1)
      REPLACE nLan_Cost&lcCnt WITH nLan_Cost&lcCnt + (&lcTmpLine..nLan_Cost&lcCnt*&lcTmpLine..nMfgTotQty)
    ENDFOR
  ENDIF    
ENDSCAN
SELECT(lnAlias)