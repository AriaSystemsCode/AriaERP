*:****************************************************************
*: Program file  : MAMFRWK.PRG
*: Program desc. : Add,Change,Delete and Inquire Rework order.
*: For screen    : MAMFRWK.SPR
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Materials Module (MA)
*: Developer     : ABDOU ELGENDI -  (ABD)
*: Date          : 03/18/2001
*: Tracking Job Number : C#200254,1
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfActFolder , lfLnBrowse , lfwLnBrow
*:               :            : lfvBrow     , lfCrtTmpLn , lfClrTrap
*:               :            : lfTab       , lfShtTab   , lfTrap
*:               :            : lfChkLnKey  , lfShowLine , lfvMFGOrdNo
*:               :            : gfRWOrdBr   , lfvData_2  , lfvEntered
*:               :            : lfUpdVars   , lfvComplet , lfGenLines
*:               :            : lfwOldValm  , lfwOldVal  , lfvData27
*:               :            : lfvData4    , lfAdUnCmSR , lfUpdUnCmS
*:               :            : lfChkUnComS , lpClsScr   , lfShowEnd
*:               :            : lfvFldObj   , lfvQty     , lfUpdHEst
*:               :            : lfvRem      , lfChkPrc   , lfvNew
*:               :            : lfvColor    , lfVldColor , lfvSwitch
*:               :            : lfvDyelot   , lfChkQty   , lfTotalAct
*:               :            : lfGetMONum  , lfvMONumOk , lfSetStat
*:               :            : lfAddLine   , lfGtNewLn  , lfChngFolder
*:               :            : lfUpdStp    , lfUpdFabDye, lfModLine
*:               :            : lfDelLine   , lfDone     , lpDelScr
*:               :            : lfStDelBar  , lfGtDelBar , lfRestVar
*:               :            : lfActPad    , lpvInquiry , lpapendmfg
*:               :            : lfDeActPad  , lfApplin   , lfSelPoLin
*:               :            : lfBrPOLin   , lfwPOl     , lfvSelec
*:               :            : lfvUSelec   , lfAddWare  ,lFildata_2
*:               :            : lfvSeleOk   , lfwLink    , lfvLink
*:               -----------------------------------------------
*:               : PROCEDURE  : lpShow , lpSavScr
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200254,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*- The order detail will contain Trans Code field 
*- TranCod = 1 	=== > 	Budget Qty 
*- TranCod = 2 	=== > 	Issued Qty to the vendor 
*- TranCod = 3 	=== > 	received from the vendor  
*- TranCod = 4 	=== > 	received to canceled 
*- TranCod = 5	=== > 	received to Second Quality
*- TranCod = 6	=== > 	received to Damage

*-- Begin Declaration variables.
*-- laScrMode  : - Detrmine which mode we are in. [Edit , View....]
*-- laData     : - Array hold all filed on the screen.
*-- laKeyField : - Array hold the key filed in the screen.
*-- laFoldWinds: - Array hold folders screens.
*-- lcPMMONum : - Parameter to Detrmine if the program called from the system menu or from
*-- End Declaration variables.

PARAMETER lcPMMONum
EXTERNAL ARRAY laDefProc, laCtrStat ,laKeyField

llFromMenu = TYPE("lcPMMONum") # "C"

*-- Define the used arrays.
DIMENSION laScrMode[4], laData[27], laKeyField[1,4], laFoldWinds[3,2] ,;
          laAddress[6,3]
STORE .F. TO laScrMode , llOkSelect
laScrMode[1] = .T. && View Mode.

*--- Variable holed Appened Warhouse from header file.
STORE '' TO lcOrdNo , lcFabDesc , lcCanStat , lcSetExa , lcOldVal , laKeyField,;
            lcShipTo1, lcShipTo2, lcShipTo3, lcShipTo4, lcShipTo5,;
            lcVEnAdd1, lcVEnAdd2, lcVEnAdd3, lcVEnAdd4, lcVEnAdd5,;
            lcVendor , lcVenName , lcWareDEsc , laAddress , lcPOLine,;
            lcOldLink,lcApndWare ,lcLinkDesc , lcGl_link
STORE .F. TO llAllColrs , llDyeEmp , DyeYn , llMdWarRec , llMdDyeRec , llOMulWH , llNewRec, llOpMfgHdr , llOpMfgDD
STORE 0 TO lnEstTotal , lnActTotal , lnNumOfDets , lnOldVal , puWare , lnWarNum
DECLARE laBomQty[9]

*-- To force the control pannel to call the local save and close
*-- procedures (lpSavScr, lpClsScr , lpDelScr)

laDefProc[07] = .F.  && Disable the control panel delete proc.(lpDelScr).
laDefProc[09] = .F.  && Disable the control panel save proc.  (lpSavScr).
laDefProc[10] = .F.  && Disable the control panel close proc. (lpClsScr)

lnActBar = 1
*-- The screen keys.
laKeyField[1,1] = "laData[1]"
laKeyField[1,2] =.T.
laKeyField[1,3] = "MfgOrd#"
laKeyField[1,4] = 1

*-- The system/company setup variables.
STORE .F. TO llCostPrv, llMultiWar, llDyelot
lcGnCTBom  = SPACE(0)


ldOEntered = {}  && Material Manfacturning Order Entered date.
ldOComplet = {}  && Material Manfacturning Order Complete date.

*-- Set the displayed Manufacturing order status variable.
= lfSetStat()

*-- First folder variables.
*-- Flag to tell if we should generate the Material MO lines
*-- or not, this will only be true if the lines are generated, 
*-- and that's will happen if the user select the add mode and
*-- entered the Fabric major value, and entered the header warehouse
*-- and then activate the 2nd tab in the screen's folder (Details).
llGenLines = .T.

*-- Windows names.
lcCTChWin2 = SPACE(0)
lcCTChWin4 = SPACE(0)
lcCTChWn40 = SPACE(0)
lcCTChWn41 = SPACE(0)
lcCTChWn42 = SPACE(0)
lcCTChWn43 = SPACE(0)
lcCTChWn44 = SPACE(0)
lcCTChWn45 = SPACE(0)
lcCTChWin5 = SPACE(0)
lcCTChWin6 = SPACE(0)

*-- First variables.
lcTmpMmoLn = SPACE(0)
lcTmpRec   = SPACE(0)
lcLinesBrw = SPACE(0)
lcLnScFlds = SPACE(0)
lnMarker   = 0
lcOptBrwTl = SPACE(1)
lcRecStat  = SPACE(0)
lcTmpCtPk  = SPACE(0)
lcNonMaj   = SPACE(0)
lcONonMaj  = SPACE(0)
lcOLinWare = 0
lcScalFlsd = SPACE(0)
lnLnNo     = 0
lnOLnQty   = 0

*-- 2nd Folder variables.
lcOrderKy  = SPACE(0)
lcLabel1   = SPACE(0)
lcLabel2   = SPACE(0)
lcLabel3   = SPACE(0)
lcLabel4   = SPACE(0)

*-- Change the "Delete" button in the "Control Pannel" to be
*-- "Cancel/Uncancel"

lcCancel   = gcBmpHome + "TRASH.BMP"
lcUnCanc   = gcBmpHome + "UNTRASH.BMP"
lcPromp    = lcCancel
lcDelMesag = 'cancel'
lcMenProm  = PROPER(lcDelMesag) + " Material Manfacturning Rework Order"

*-- Variable to hold the alias name for the MARWLIN file when opened to
*-- calculate the totals per line.
lcMMOLnTot = SPACE(0)

*-- Following set of variables are used to save/restore the
*-- filter, index status of the used files, to be used to
*-- save/restore these status when branching to another prgram.

lcMMFGORD  = SPACE(0)
lcRecLogKy = SPACE(0)

*-- Initialize the Folders array.
lcFolder     = SPACE(0)
lcwFoldChng  = SPACE(0)
lcFoldPush   = SPACE(0)
laFoldWind   = SPACE(0)
lcFoldPrnt   = SPACE(0)
STORE 0 TO lnFolderCEnd , lnActFolder , lnFolderREnd


*-- Following variables are used to handel the incomplete program 
*-- session concept.
lcSession  = SPACE(1)                  && Sequence number to flag the program sessions.
lnThisSess = gnProgCopy                && Current running session number.
lnUnCmSeRc = 0                         && The incomplete session record number.
lcProgID   = PADR("MAMFRWK", 10)       && The program ID in the incomplete session file.
llUnComp   = .F.                       && Are we comming from an incomplete session?
lcScrMode  = SPACE(1)
llContinue = .F.

*-- Variables that need to be saved and restored when detecting an
*-- uncomplete program session.
DIMENSION laVars[09]
laVars[01] = "lcScrMode"
laVars[02] = "lnUnCmSeRc"
laVars[03] = "llGenLines"
laVars[04] = "lnActFolder"
laVars[05] = "llCUpDate"
laVars[06] = "lcMFStatus"
laVars[07] = "lcFabDesc"
laVars[08] = "laData"
laVars[09] = "llContinue"

*-- Add the screen header fields saved in the laData Array to the 
*-- list of the variables to be restored when detecting an uncomplete 
*-- program session.
FOR lnI = 1 TO 27
  DIMENSION laVars[ALEN(laVars)+1]
  laVars[ALEN(laVars)] = "laData[" + ALLTRIM(STR(lnI,2)) + "]"
ENDFOR


IF !gfSetup()
  RETURN
ENDIF

STORE "DISABLE" TO lcClrStat, lcLineStat, lcWareStat, lcDyeStat

*--Adding this variable to 2nd Folder variables to be used as Flag
*-- to indicate whether the new Added Push Button is pressed or not by
*-- default , its value is False (not pressed)
llByUnit = .F.


*-- Header screen variables.
llBrowse   = .F.
lcMFStatus = SPACE(0)

llAlowNew  = .F.             && Flag to allow adding new CTs.

*--View Order Receiving Log in the View Mode.
DECLARE laTrans[4]
laTrans[2] = 'Received '
laTrans[3] = 'Damaged  '
laTrans[4] = 'Cancelled'
lcRecvLog  = gfTempName()

lcTNegNums = 'Negative amounts are not allowed.'

lcNrmStat  = "DISABLE"
lcMajStat  = "ENABLE"

*-- Variables used to display the line totals in the "Transactions Totals"
*-- bar from the "Option" menu pad.
lcTNonMaj  = SPACE(0)                && The nonmajor code in this screen.
lnFabBud   = 0                       && Total budget for the active line.
lnFabRec   = 0                       && Total received for the active line.
lnFabDam   = 0                       && Total dameged for the active line.
lnFabCan   = 0                       && Total canceled for the active line.
lnFabOpn   = 0                       && Total open for the active line.

*-- 2nd Folder variables.
lcLabel1     = ALLTRIM(PROPER(gfGetMemVar('M_CTSLBL1')))
lcLabel2     = ALLTRIM(PROPER(gfGetMemVar('M_CTSLBL2')))
lcLabel3     = ALLTRIM(PROPER(gfGetMemVar('M_CTSLBL3')))
lcLabel4     = ALLTRIM(PROPER(gfGetMemVar('M_CTSLBL4')))

*-- Load the system/company setup variables.
llCostPrv    = gfUserPriv('IC','ICFABRIC','COSTING')
llMultiWar   = gfGetMemVar("M_WAREHOUSE") = "Y"
llDyelot     = gfGetMemVar("M_MATDYE")    = "Y"
llGlLink   = ALLTRIM(gfGetMemVar('M_LINK_GL'))  = 'Y'

*-- needed variables when append from mfg Order.
lcRec_Ttl = ""
lcMark    = '>'
lcUnMark   = SPACE(1)


IF !WEXIST(gcBaseWind)

  llAlowNew  = .T.

  SELECT (lcBaseFile)
  lcScFields = " cMfgRwNum  , cFabric    , Status     , cWareCode  , Entered    , " +;
               " Complete   , nMmgBudget , received   , damaged    ,              " +;
               " canceled   , nMmfgOpen  , nEst_Cost1 , nEst_Cost2 , nEst_Cost3 , " +;
               " nEst_Cost4 , nLan_Cost1 , nLan_Cost2 , nLan_Cost3 , nLan_Cost4 , " +;
               " nAct_Cost1 , nAct_Cost2 , nAct_Cost3 , nAct_Cost4 , lMultiWare , " +;
               " cMultiLot  , Pcs_Act    , cVendCode  , caddress1  , caddress2  , " +;
               " caddress3  , caddress4  , caddress5  , caddress6  , Link_Code"

  *-- laData Map
  *    laData[1]    laData[2]    laData[3]     laData[4]     laData[5]
  *    cMfgRwNum  , cFabric    , Status      , cWareCode  ,  Entered    
  *    laData[6]    laData[7]    laData[8]     laData[9]     laData[10]
  *    Complete   , nMmgBudget , received    , damaged    ,  canceled
  *    laData[11]   laData[12]   laData[13]    laData[14]    laData[15]
  *    nMmfgOpen  , nEst_Cost1 , nEst_Cost2  , nEst_Cost3 ,  nEst_Cost4
  *    laData[16]   laData[17]   laData[18]    laData[19]    laData[20]
  *    nLan_Cost1 , nLan_Cost2 , nLan_Cost3  , nLan_Cost4 ,  nAct_Cost1
  *    laData[21]   laData[22]   laData[23]    laData[24]    laData[25]
  *    nAct_Cost2 , nAct_Cost3 , nAct_Cost4  , lMultiWare ,  cMultiLot
  *    laData[26]   laData[27]   laData[28]    laData[29]    laData[30]
  *    Pcs_Act    , cVendCode  , caddress1   , caddress2  , caddress3
  *    laData[31]   laData[32]   laData[33]    ladata[34]
  *    caddress4   , caddress5 , caddress6  , Link_Code
  
  *-- End Data Map.
  
  *-- Windows Names.
  lcCTChWin2   = gfTempName()
  lcCTChWin4   = gfTempName()
  lcCTChWn40   = gfTempName()
  lcCTChWn41   = gfTempName()
  lcCTChWn42   = gfTempName()
  lcCTChWn43   = gfTempName()
  lcCTChWn44   = gfTempName()
  lcCTChWn45   = gfTempName()
  lcCTChWin5   = gfTempName()
  lcCTChWin6   = gfTempName()
  
  *-- Initialize the folders information.
  lcwFoldChng  = "= lfActFolder()"
  lcFoldPush   = "pbFolder"
  lnFolderCEnd = 102.50
  lnFolderREnd = 2.00
  lcFolder     = gfTempName()
  lcFoldPrnt   = gcBaseWind
  lnActFolder  = 1
  

  laFoldWinds[1,1] = "Header"
  laFoldWinds[1,2] = lcCTChWin6

  laFoldWinds[2,1] = "Detail"
  laFoldWinds[2,2] = lcCTChWin4
  
  laFoldWinds[3,1] = "Summary"
  laFoldWinds[3,2] = lcCTChWin5
  
  *-- Build the main temporary file.
  lcTmpMmoLn = gfTempName()
  lcTmpRec   = gfTempName()
  lcPOLine   = gfTempName()
  lcLinesBrw = "Materials Manufacturing Order Lines"
  lcLnScFlds= " Color , Pattern , cWareCode , Dyelot , nmfgtotqty , Reference , Lineno"

  *-- Get a session number, to be updated in the uncomplete session
  *-- number file foe each session.
  lcSession  = gfsequence('CSESSION')
  = lfCrtTmpLn()

  IF !llFromMenu AND SEEK(lcPMMONum)
    laData[1]    = lcPMMONum
    laScrMode    = .F.
    laScrMode[2] = .T.
    SCATTER FIELDS &lcScFields TO laData
    M.DYEYN = FABRIC.CDYE_FLG
  ELSE
    SCATTER FIELDS &lcScFields TO laData BLANK
    M.DYEYN = FABRIC.CDYE_FLG
  ENDIF

  
ENDIF


IF llGlLink
  =gfOpenFile(gcDataDir+'GL_LINK',gcDataDir+'GL_LINK1','SH')
  =gfOpenFile(gcDataDir+'GLDIST' ,'','SH')
ENDIF

*-- Empty out all the displayed objects on the screen.
m.cWareCode = laData[4]

SELECT (lcTmpMmoLn)
SCATTER FIELDS &lcLnScFlds MEMVAR BLANK
SHOW GET laData[2] DISABLE

lcDelPadNm = SPACE(0)
PUSH MENU _MSYSMENU
lnDelBarNo = lfGtDelBar()

= lfActPad()

llNoShow = .F. &&!(laScrMode[2] OR (laScrMode[4] AND !EMPTY(laData[1])))
= lfStDelBar()

DO (gcScrDir+gcWinAppl+"\MaMfRWK.SPR")

POP MENU _MSYSMENU

*--Deactivate options menue
=lfDeActPad()

SELECT MARWHDR
SET RELATION TO 

*-- Erase all the created temporary files, close all the conditionally
*-- opened aliases.
IF glQuitting
  IF FILE(gcWorkDir+lcTmpMmoLn+'.DBF') .AND. USED(lcTmpMmoLn)
    USE IN (lcTmpMmoLn)
    ERASE (gcWorkDir+lcTmpMmoLn +".DBF")
    ERASE (gcWorkDir+lcTmpMmoLn +".CDX")
    ERASE (gcWorkDir+lcTmpMmoLn +".FPT")
  ENDIF
    
  IF USED(lcTmpRec)
    USE IN (lcTmpRec)
    ERASE (gcWorkDir+lcTmpRec +".DBF")
    ERASE (gcWorkDir+lcTmpRec +".CDX")
    ERASE (gcWorkDir+lcTmpRec +".FPT")
  ENDIF

  IF FILE(gcWorkDir+lcPOLine+'.DBF')
    USE IN (lcPOLine)
    ERASE (gcWorkDir+lcPOLine+'.DBF')
    ERASE (gcWorkDir+lcPOLine+'.CDX')
    ERASE (gcWorkDir+lcPOLine+'.FPT')
  ENDIF
  
  IF llOpMfgHdr
    = gfCloseFile("MMFGORDH")
  ENDIF
  
  IF llOpMfgDD
    = gfCloseFile("MMFGORDD")
  ENDIF
  
  IF USED('MARWLIN')
    = gfCloseFile("MARWLIN")
  ENDIF  
ENDIF

*-- Functions and Procedures :
*:***************************************************************************
*: Name       : lpShow
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 06/03/1999
*: Purpose    : Screen modes controlling procedure.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : DO lpShow
*:***************************************************************************
*:
PROCEDURE lpShow

DO CASE
  *                ---> S E L E C T  M O D E <---
  CASE laScrMode[1]

    *-- Check if there is any previuosly uncompleted session of the 
    *-- program.
    IF lfChkUnComS()
      RETURN
    ENDIF
  
    SCATTER FIELDS &lcScFields MEMO TO laData BLANK
    SELECT (lcTmpMMOLn)
    BLANK ALL
    DELETE ALL
    SET FILTER TO cStatus <> 'D'
    llGenLines = .T.
    STORE SPACE(0) TO lcMFStatus, lcFabDesc,lcVEnAdd1 , lcVEnAdd2 , lcVEnAdd3 ,;
                      lcVEnAdd4 , lcVEnAdd5 , lcVenName,lcWareDEsc, lcApndWare

    lnLastFold = lnActFolder
    lnActFolder = 1
    llNoThing   = lfChngFolder(lnActFolder)
    SHOW GET ibFolder[1] DISABLE
    SHOW GET laData[5] ENABLE
    SHOW GET laData[6] ENABLE
    *-- Force the activation of the first object in the screen.
    *-- Prepare the variables used to switch the prompt of the "Delete"
    *-- bar in the system menu, and the status of the "Cancel" button in
    *-- the control panell.
    lcCanStat    = "DISABLE"
    laCtrStat[8] = lcCanStat
    lcPromp      = lcCancel
    = lfShowEnd()
    
    SHOW GET laData[5]  DISABLE
    SHOW GET laData[6]  DISABLE
    
  *                 --> V I E W  M O D E <--
  CASE laScrMode[2]
  
    
    STORE SPACE(0) TO lcFabDesc
    SCATTER FIELDS &lcScFields MEMO TO laData
    lcMajStat    = "DISABLE"
    lcNrmStat    = "DISABLE"

    *-- Get the header warehouse.
    lcSetExa     = SET('EXACT')
   
    *-- Adjust order status
    = lfSetStat(laData[3])
    
    
    lcEdtStat    = IIF(laData[3] $ "CX", "DISABLE", "ENABLE")
    laCtrStat[7] = lcEdtStat
    lcPromp      = IIF(laData[3]="X" , lcUnCanc, lcCancel )

    *- Check IF we issue or Receive Any Qty.
    lcCanStat    = IIF(laData[3]$"OX" .AND. !lfChkIsQty(), "ENABLE", "DISABLE")
    lcGnLinsSt   = "DISABLE"
    llCUpdate    = .F.
    lcScrMode    = "V"
    
    *-- Enable/Disable the controlled objectes.
    SHOW GET pbEdt       &lcEdtStat
    SHOW GET laData[1]   DISABLE
    SHOW GET ibFabBrow   DISABLE
    SHOW GET laData[2]   DISABLE
    SHOW GET laData[5]   DISABLE
    SHOW GET laData[6]   DISABLE
    SHOW GET laData[10]  DISABLE

    SHOW GET m.Color 
    SHOW GET ibColor DISABLE
    SHOW GET m.Dyelot   DISABLE
    =lfShowLine("D")
    SHOW GET pbMMONote    ENABLE
    
    =lfShowEnd()

    lcFabDesc = IIF(SEEK(laData[2] , 'Fabric') , FABRIC.Desc , ' ')

    IF !EMPTY(laData[4])
      =SEEK(laData[4],"WAREHOUS")
      lcWareDesc  = WAREHOUS.cDesc
    ENDIF
    
    *-- Get the vendor Address.
    = SEEK(laData[27],"ApVendor")
    lcVenName = LOOKUP(ApVendor.cvencomp ,laData[27] ,ApVendor.cvendcode)
    lcVEnAdd1 = gfGetAdr('ApVendor','','','',1,'')
    lcVEnAdd1 = lcVEnAdd1+IIF(LEN(lcVEnAdd1)<30,SPACE(30-LEN(lcVEnAdd1)),'')
    FOR lnCounter = 1 TO 5
      L=ALLTRIM(STR(lnCounter))
      lcTempVr=''
        FOR lnCount = 1 TO ALEN(laAddress,1)
          IF laAddress[lnCount,1] = lnCounter
            lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
            lcTempVr  = lcTempVr+IIF(EMPTY(lcTempVr) .OR. RIGHT(lcTempVr,1) = ',' ,'',', ') + lcAddPart
          ENDIF
        ENDFOR
        lcVEnAdd&L=lcTempVr+IIF(LEN(lcTempVr)<30,SPACE(30-LEN(lcTempVr)),'')
    ENDFOR
   
    IF lnActFolder = 2
      SHOW GET ibFolder[1]  ENABLE
      SHOW GET laData[24]   DISABLE    
      SHOW GET pbRem        DISABLE
      SHOW GET m.pattern    DISABLE
      SHOW GET m.reference  DISABLE
      SHOW GET m.nmfgtotqty DISABLE
      SHOW GET ibTrap04     ENABLE
      =lfLnBrowse()
      *- In case Dyelot.
      IF llDyelot .AND. m.DyeYN = 'N'
        SHOW GET m.Dyelot DISABLE
      ENDIF
    ENDIF  
    
  
  
  *                ----> E D I T  M O D E <----
  CASE laScrMode[3]
    IF !llContinue
      SELECT (lcTmpMMOLn)
      BLANK ALL
      DELETE ALL
      SET FILTER TO cStatus <> 'D'
    ELSE
      SELECT (lcTmpMMOLn)
      SCATTER FIELDS &lcScFields TO laData
    ENDIF
    
    SELECT MARWLIN
    SET ORDER TO TAG MARWLIN
    SET RELATION TO cFabric + Color INTO FABRIC ADDITIVE
    lnNumOfDets  = 0
    llCUpdate    = .T.
    IF !llContinue
      IF SEEK(laData[1])
        SCAN REST WHILE cMfgRwNum = laData[1]
          SCATTER MEMVAR MEMO
          lnNumOfDets = lnNumOfDets + 1
          m.cStatus = 'S'
          m.nRecNo  = RECNO()
          m.DyeYN   = FABRIC.cDye_Flg
          INSERT INTO (lcTmpMMOLn) FROM MEMVAR
        ENDSCAN
      ENDIF
    ELSE
      SELECT (lcTmpMMOLn)
      SCATTER FIELDS &lcScFields TO laData
    ENDIF
    SET RELATION OFF INTO FABRIC
    SELECT (lcTmpMMOLn)

    *-- Reinitialize some of the screen's objects.
    *-- This will only effictive when trying to reset an uncomplete 
    *-- session of the program (not all of the used variables are
    *-- saven in the uncomplete session file).
    lcPromp      = IIF(laData[3]="X", lcUnCanc, lcCancel) 
    lcCanStat    = "DISABLE"
    laCtrStat[8] = lcCanStat
    lcFabDesc    = IIF(SEEK(laData[2],"Fabric"), Fabric.Desc, "")
    
    llNoThing    = lfSetStat(laData[3])

    *-- Enable/Disable the controlled objectes.
    lcMajStat  = "DISABLE"
    lcNrmStat  = "ENABLE"
    lcGnLinsSt = IIF(laData[3]="H", "ENABLE", "DISABLE")
    SHOW GET ibOrdBrow  DISABLE
    SHOW GET laData[1]  DISABLE
    SHOW GET ibFabBrow  DISABLE
    SHOW GET laData[2]  DISABLE
    SHOW GET laData[5]  ENABLE
    SHOW GET laData[6]  ENABLE
    SHOW GET laData[27] DISABLE
    SHOW GET laData[4]  DISABLE
    SHOW GET ibData27   DISABLE
    SHOW GET ibData4    DISABLE
    
    
    *-- Case Multi warehouse enable and disable according to Multi Loc. field
    *-- Else (Single Warehouse) Disable warehouse selection and Multi Loc. field
    IF llDyelot .AND. m.DyeYN = 'N'
      SHOW GET m.Dyelot DISABLE
    ENDIF

    =lfShowLine("E")
    SHOW GET pbMONote   ENABLE
    SHOW GET lcMFStatus
    SHOW GET ibFolder[1] ENABLE
    SELECT (lcTmpMMOLn)
    = lfLnBrowse()
    IF EMPTY(lcTmpMMOLn) 
      SHOW GET pbRem DISABLED
    ENDIF
    
    SHOW GET llMultiLot ENABLE  
    SHOW GET pbNew ENABLE
    IF !EMPTY(lcTmpMMOLn)             
      SHOW GET pbRem ENABLE  
    ELSE
      SHOW GET pbRem DISABLE 
    ENDIF
   *-- Create an "Open" record in the uncomplete session file.
   *-- This will be done only if we are not comeing from 
   *-- an uncomplete session number.
   lcScrMode = "E"
   
   llNoThing = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.)
   =lfShowEnd()
  
  
  *                   ----> A D D  M O D E <----
  CASE laScrMode[4]
    IF EMPTY(laData[2])
      IF llContinue
        **-- Make sure the tepmorary file is empty
        SELECT (lcTmpMMOLn)
        BLANK ALL
        DELETE ALL
        SET FILTER TO cStatus <> 'D'
        *-- Clear fabric field, and position the cursor on that field
        laData[2]  = SPACE(7)
        lcFabDesc  = SPACE(1)
      ENDIF
      llCUpdate   = .T.
      puWare      = 1
      lnNumOfDets = 0
      lcMFStatus  =''
      laData[3]   = 'O'
      lcMfStatus  = 'Open'
      *-- Case Multi warehouse enable and disable according to Multi Loc. field
      *-- Else (Single Warehouse) Disable warehouse selection and Multi Loc. field
      IF llMultiWar
        SHOW GET ibData4    ENABLE
        SHOW GET laData[4]  ENABLE
      ELSE
        SHOW GET ibData4    DISABLE
        SHOW GET laData[4]  DISABLE       
      ENDIF
      
      SHOW GET ibData27   ENABLE
      SHOW GET ladata[27] ENABLE
      IF lnActFolder = 2
        SHOW GET m.Pattern DISABLE
        SHOW GET m.Reference DISABLE 
        SHOW GET m.nMfgTotQty DISABLE
        SHOW GET ibFolder[3] DISABLE
        IF llContinue
          SELECT (lcTmpMMOLn)
          DELETE ALL
          SET FILTER TO cStatus <> 'D'
          SCATTER MEMVAR MEMO BLANK
        ENDIF
        SHOW GET pbObjlnk   DISABLE
      ENDIF
      
      IF !llContinue
        SHOW GET ibFabBrow ENABLE
        SHOW GET laData[2] ENABLE
      ELSE
        SHOW GET ibFabBrow DISABLE
        SHOW GET laData[2] DISABLE
      ENDIF
      lcPromp      = lcCancel
      lcCanStat    = "DISABLE"
      laCtrStat[8] = lcCanStat
      SHOW GET pbNew DISABLE
      SHOW GET pbRem DISABLE
      _CUROBJ = OBJNUM(laData[2])

      IF lnActFolder = 2
        = lfLnBrowse()
      ENDIF  

      *-- Create an "Open" record in the uncomplete session file.
      *-- This will be done only if we are not comeing from 
      *-- an uncomplete session number.
      lcScrMode = "A"
      llNoThing = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.)
    ELSE
      lcMFStatus = ''
      laData[3]  = 'O'
      lcMfStatus = 'Open'
    
      IF lnActFolder = 2
        SHOW GET ibFabBrow DISABLE
        SHOW GET laData[2] DISABLE
        lcPromp      = lcCancel
        lcCanStat    = "DISABLE"
        laCtrStat[8] = lcCanStat
        SHOW GET pbNew ENABLE
        SHOW GET pbRem ENABLE
      ENDIF
      

      *-- Case Multi warehouse enable and disable according to Multi Loc. field
      *-- Else (Single Warehouse) Disable warehouse selection and Multi Loc. field

      SHOW GET pbObjlnk   DISABLE
    ENDIF
    SHOW GET laData[1] DISABLE
    _CUROBJ = OBJNUM(laData[2])
    laData[34] = 'DEFDEF'
    *- In case Dyelot.
    IF llDyelot  .AND. m.DyeYN = 'N'
      SHOW GET m.Dyelot DISABLE
    ENDIF
    
ENDCASE


*-- End OF lpShow.
*:***************************************************************************
*: Name       : lfActFolder
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Refresh the active folder objects and disable
*:              the other object in the other folders.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : = lfActFolder()
*:***************************************************************************
*:
FUNCTION lfActFolder

DO CASE
  *-- The Header folder.
  CASE lnActFolder = 1
    *-- Disble the other folders objects.
    SHOW GETS WINDOW (lcCTChWin5) DISABLE ONLY
    *-- Disble the other folders objects.
    SHOW GETS WINDOW (lcCTChWin4) DISABLE ONLY
    SHOW GETS WINDOW (lcCTChWn40) DISABLE ONLY
    SHOW GETS WINDOW (lcCTChWn41) DISABLE ONLY
    SHOW GETS WINDOW (lcCTChWn42) DISABLE ONLY


    llNoThing = lfRefresh(lcCTChWin6)
    SHOW WINDOW (lcCTChWin6)	 TOP
    
    SHOW GET ibFolder[1] ENABLE
    
    IF laScrMode[3] .OR. laScrMode[4]
      SHOW GET laData[5] ENABLE
      SHOW GET laData[6] ENABLE
      SHOW GET laData[28] ENABLE
      SHOW GET laData[29] ENABLE
      SHOW GET laData[30] ENABLE
      SHOW GET laData[31] ENABLE
      SHOW GET laData[32] ENABLE
    ENDIF
    
  *-- The details folder.
  CASE lnActFolder = 2
    *-- Disble the other folders objects.
    SHOW GET PbSwitch DISABLE
    SHOW GETS WINDOW (lcCTChWin5) DISABLE ONLY

    SHOW WINDOW (lcCTChWin4) TOP
    =lfLnBrowse()    
    SELECT (lcTmpMMOLn)
    GO TOP
    SCATTER MEMVAR MEMO
    =lfChkLnKey('',.T.)

    SHOW GET PbSwitch DISABLE     && Push Botton is Disabled 
    
    *-- If comming from another session or another active program
    *-- then refresh the browse and the window that's below it
    *-- according to the status of the selected bars in the 
    *-- "Option" menu pad.
    *-- The usage of this flag is usefull because we do not need to 
    *-- do this refresh every time the user change the folders, it's
    *-- only needed when comming from another session.
    =lfwLnBrow()

    *-- Always enable the Trapping invisable button.
    SHOW GET ibTrap04 ENABLE
    SHOW GET PbSwitch DISABLE
    SHOW GET ibFolder[3] ENABLE
    
    *-- in case Dyelot.
    IF llDyelot  .AND. m.DyeYN = 'Y' .AND. !(laScrMode[3] .OR. laScrMode[4])
      SHOW GET m.pattern    DISABLE
      SHOW GET m.reference  DISABLE
      SHOW GET m.nmfgtotqty DISABLE
    ENDIF
    
  CASE lnActFolder = 3 && The summary folder.
    *-- Refresh the says in this window.
    SHOW WINDOW (lcCTChWin5) TOP
    llNoThing = lfRefresh(lcCTChWin5)

    *-- Disble the other folders objects.
    SHOW GETS WINDOW (lcCTChWin4) DISABLE ONLY
    SHOW GETS WINDOW (lcCTChWn40) DISABLE ONLY
    SHOW GETS WINDOW (lcCTChWn41) DISABLE ONLY
    SHOW GETS WINDOW (lcCTChWn42) DISABLE ONLY

    IF laScrMode[1] = .T.  OR lnActFolder = 1        && Select Mode
      SHOW GET PbSwitch DISABLE     && Push Botton is Disabled 
    ELSE                            && View,Edit,Add Mode
      SHOW GET PbSwitch ENABLE      && Push Botton is Enabled 	
    ENDIF                          
    SHOW GET ibFolder[1] ENABLE
    
    IF laScrMode[4] .OR. laScrMode[3]
      SHOW GET ladata[34] ENABLE
      SHOW GET ibGLLink   ENABLE
    ELSE
      SHOW GET ladata[34] DISABLE
      SHOW GET ibGLLink   DISABLE
    ENDIF
ENDCASE

*-- End Of lfActFolder
*:***************************************************************************
*: Name       : lfLnBrowse
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Browse the Material Manfacturning Order lines.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : = lfLnBrowse()
*:***************************************************************************
*:
FUNCTION lfLnBrowse
PRIVATE lnAlias , lcFields

lnAlias    = SELECT(0)

IF WEXIST(lcLinesBrw) and lnActBar <> 2
  RELEASE WINDOW (lcLinesBrw)
ENDIF

lcFile     = IIF( laScrMode[2], "MARWLIN", lcTmpMMOLn)
SELECT (lcFile)
LOCATE
lnMarker = RECNO()

lcFields   = "cMarker=IIF(RECNO()=lnMarker,'>',' '):1:R:H=' ':W=.F.,"          +;
             "Color:12:H='Color':R,"+"Pattern:12:H='Pattern':R," +;
             "Reference:32:H='Reference':R,"            +;
             "nmfgtotqty:12:H='Quantity':R"                       +;
             IIF(llMultiWar,",cWareCode:R:H='Warehouse'","")                    +;
             IIF(llDyelot,",Dyelot:R:H='Dyelot'","")


BROWSE FIELDS &lcFields ;
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       FOR cMfgRwNum + cFabric + Color + Dyelot+TranCD = laData[1] .AND. TranCd = '1';
       WHEN lfwLnBrow();       
       VALID :F lfvBrow();
       TITLE (lcLinesBrw);
       WINDOW (lcCTChWn41) IN WINDOW (lcCTChWin4)

=lfRefresh(lcCTChWn41)
=lfwLnBrow()

SELECT(lnAlias)

*-- End OF lfLnBrowse.
*:***************************************************************************
*: Name       : lfwLnBrow
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : "WHEN" function of the Material MO lines browse.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : = lfwLnBrow()
*:***************************************************************************
*:
FUNCTION lfwLnBrow
PRIVATE lnAlias , lcObjPar ,lcDySataus
lcObjPar = 'C'

lnAlias    = SELECT(0)
lcFile     = IIF(laScrMode[2],  "MARWLIN", lcTmpMMOLn)
SELECT (lcFile)
lnMarker = RECNO()
SCATTER MEMVAR MEMO


*-- in case Dyelot
IF llDyelot  .AND. m.DyeYN = 'Y' .AND. laScrMode[4] .AND. !EMPTY(m.Color)

  lcDySataus = IIF(EMPTY(Dyelot),'DISABLE','ENABLE')
  SHOW GET m.Pattern    &lcDySataus
  SHOW GET m.Reference  &lcDySataus
  SHOW GET m.nMFGTotQty &lcDySataus

  IF EMPTY(DYELOT)
    SHOW GET m.Dyelot ENABLE
  ELSE
    SHOW GET m.Dyelot DISABLE
  ENDIF  
  SHOW GET m.Color
  
ELSE
  SHOW GET m.Color
  SHOW GET m.Pattern
  SHOW GET m.Reference
  SHOW GET m.nMFGTotQty

  IF llDyelot
    SHOW GET m.Dyelot DISABLE
  ENDIF
  
ENDIF

IF (laScrMode[4]AND !EMPTY(laData[2])) OR laScrMode[3]
  SHOW GET pbNew ENABLED
  IF !EOF(lcFile)
    SHOW GET pbRem ENABLED
  ENDIF
ENDIF

IF WEXIST(lcLinesBrw)
  SHOW WINDOW (lcLinesBrw) REFRESH SAME
ENDIF

IF WONTOP(lcLinesBrw)
  =lfvBrow()
ENDIF
SELECT(lnAlias)

*-- End of lfwLnBrow.
*:***************************************************************************
*: Name       : lfvBrow
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Valid function for all the screen's browses.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : = lfvBrow()
*:***************************************************************************
*:
FUNCTION lfvBrow

*-- Call the global function "gfStopBrow" if one of the screen's
*-- browses is active.
IF !INLIST(WONTOP(), lcLinesBrw, lcOptBrwTl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- End of lfvBrow.
*:***************************************************************************
*: Name       : lfCrtTmpLn
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : To create the program temp file.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : = lfCrtTmpLn()
*:***************************************************************************
*:
FUNCTION lfCrtTmpLn
PRIVATE lnAlias

lnAlias = SELECT(0)

*-- Read the Material MO line structure into an array.
SELECT MARWLIN
lnMOLnFlds = AFIELDS(laMOLnFlds)

*-- The "nStep" field is to handel the incomplete session.
lnMOLnFlds = lnMOLnFlds + 1
DIMENSION laMOLnFlds[lnMOLnFlds, 4]
laMOLnFlds[lnMOLnFlds, 1] = "nStep"
laMOLnFlds[lnMOLnFlds, 2] = "N"
laMOLnFlds[lnMOLnFlds, 3] = 2
laMOLnFlds[lnMOLnFlds, 4] = 0

lnMOLnFlds = lnMOLnFlds + 1
DIMENSION laMOLnFlds[lnMOLnFlds, 4]
laMOLnFlds[lnMOLnFlds, 1] = "cStatus"
laMOLnFlds[lnMOLnFlds, 2] = "C"
laMOLnFlds[lnMOLnFlds, 3] = 1
laMOLnFlds[lnMOLnFlds, 4] = 0

lnMOLnFlds = lnMOLnFlds + 1
DIMENSION laMOLnFlds[lnMOLnFlds, 4]
laMOLnFlds[lnMOLnFlds, 1] = "DyeYN"
laMOLnFlds[lnMOLnFlds, 2] = "C"
laMOLnFlds[lnMOLnFlds, 3] = 1
laMOLnFlds[lnMOLnFlds, 4] = 0

lnMOLnFlds = lnMOLnFlds + 1
DIMENSION laMOLnFlds[lnMOLnFlds, 4]
laMOLnFlds[lnMOLnFlds, 1] = "nMCost1"
laMOLnFlds[lnMOLnFlds, 2] = "N"
laMOLnFlds[lnMOLnFlds, 3] = 9
laMOLnFlds[lnMOLnFlds, 4] = 2

lnMOLnFlds = lnMOLnFlds + 1
DIMENSION laMOLnFlds[lnMOLnFlds, 4]
laMOLnFlds[lnMOLnFlds, 1] = "nMCost2"
laMOLnFlds[lnMOLnFlds, 2] = "N"
laMOLnFlds[lnMOLnFlds, 3] = 9
laMOLnFlds[lnMOLnFlds, 4] = 2

lnMOLnFlds = lnMOLnFlds + 1
DIMENSION laMOLnFlds[lnMOLnFlds, 4]
laMOLnFlds[lnMOLnFlds, 1] = "nMCost3"
laMOLnFlds[lnMOLnFlds, 2] = "N"
laMOLnFlds[lnMOLnFlds, 3] = 9
laMOLnFlds[lnMOLnFlds, 4] = 2

lnMOLnFlds = lnMOLnFlds + 1
DIMENSION laMOLnFlds[lnMOLnFlds, 4]
laMOLnFlds[lnMOLnFlds, 1] = "nMCost4"
laMOLnFlds[lnMOLnFlds, 2] = "N"
laMOLnFlds[lnMOLnFlds, 3] = 9
laMOLnFlds[lnMOLnFlds, 4] = 2

lnMOLnFlds = lnMOLnFlds + 1
DIMENSION laMOLnFlds[lnMOLnFlds, 4]
laMOLnFlds[lnMOLnFlds, 1] = "nRecNo"
laMOLnFlds[lnMOLnFlds, 2] = "N"
laMOLnFlds[lnMOLnFlds, 3] = 10 
laMOLnFlds[lnMOLnFlds, 4] = 0

*-- Create the temp file.
=gfCrtTmp(lcTmpMMOLn, @laMOLnFlds,;
         [cMfgRwNum+cfabric+color+cwarecode+dyelot+trancd] , lcTmpMMOLn)
SELECT (lcTmpMMOLn)
USE 
= gfOpenFile(gcWorkDir+lcTmpMMOLn,lcTmpMMOLn,'EX')
INDEX ON cfabric+color TAG Cfabric ADDITIVE
USE
= gfOpenFile(gcWorkDir+lcTmpMMOLn,lcTmpMMOLn,"SH")

SET FILTER TO cStatus <> 'D'
          
SELECT(lnAlias)

*-- End OF lfCrtTmpLn.
*:***************************************************************************
*: Name       : lfClrTrap
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Screen's "Activate" function that is used to 
*:              release the window trapping.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : = lfClrTrap()
*:***************************************************************************
*:
FUNCTION lfClrTrap

IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If none of the screen's browses is active then clear the 
*-- trapped keys.
IF !INLIST(WONTOP(), lcLinesBrw, lcOptBrwTl)
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL Ctrl+ENTER
  ON KEY LABEL Ctrl+HOME 
  ON KEY LABEL Ctrl+END
ENDIF

*-- End OF lfClrTrap.
*:***************************************************************************
*: Name       : lfTab
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : The "Tab" key trapping procedure.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : ON KEY LABEL TAB DO lfTab
*:***************************************************************************
*:
FUNCTION lfTab

DO CASE
  *-- If the active browse is the details browse..
  CASE WONTOP() = lcLinesBrw
    ON KEY LABEL TAB
    ACTIVATE WINDOW (lcCTChWn42)
    *-- If teh active browse is the Contractor browse or the Options
    *-- browse then activate the control pannel window.
  CASE INLIST(WONTOP(), lcOptBrwTl)
    ON KEY LABEL TAB
    ACTIVATE WINDOW gwcContrl1
ENDCASE

*-- End OF lfTab.
*:***************************************************************************
*: Name       : lfShtTab
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : The "Shift+Tab" key trapping procedure.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : ON KEY LABEL TAB DO lfShtTab
*:***************************************************************************
*:
FUNCTION lfShtTab

DO CASE
  *-- If the active browse is the "Line" browse then activate the 
  *-- invisable button to handel the next movement.
  CASE WONTOP() = lcLinesBrw
    ON KEY LABEL BACKTAB
    ACTIVATE WINDOW (lcCTChWn40)

  *-- If the active browse is the "Option" browse then activate the
  *-- "Line" browse.
  CASE WONTOP() = lcOptBrwTl
    KEYBOARD "{ALT+Q}" PLAIN
ENDCASE

*-- End OF lfshtTab.
*:***************************************************************************
*: Name       : lfTrap
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Screen's "Deactivate" function that is used to 
*:              establish the window trapping.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : = lfTrap()
*:***************************************************************************
FUNCTION lfTrap

*-- Set the global flag "glFromBrow" to true only if one of the
*-- screen's browses is active.

IF lnActFolder = 2
  glFromBrow = INLIST(WONTOP(), lcLinesBrw, lcOptBrwTl)
ENDIF

IF laScrMode[4] .AND. lnActFolder = 1 .AND. EMPTY(ALLTRIM(ladata[4]))
 STORE '' TO ladata[2]
 SHOW GET ladata[2]
ENDIF

*-- If any of the screen's browses is active then trap the 
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF glFromBrow
  ON KEY LABEL TAB     DO lfTab
  ON KEY LABEL BACKTAB DO lfShtTab
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
ENDIF  
*-- End OF lfTrap.
*:*************************************************************
*: Name      : lfChkLnKey
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose   : 
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*:
FUNCTION lfChkLnKey
PARAMETER lcObject , llNoAdd
PRIVATE lcLnKeyExp, lnAlias, llEndOfKey, llUpdLine,lnRecNo

lnAlias = SELECT(0)

llEndOfKey=.F.

*-- Check if the current fabric uses dyelots
IF llDyelot .AND. m.DyeYN = 'Y'
  IF !EMPTY(M.DYELOT)
    llEndOfKey = lcObject = "D"
    lcLnKeyExp = laData[1] + laData[2] + m.Color + m.cWareCode + m.Dyelot
    SHOW GET M.DYELOT DISABLE
    =lfShowLine("E")
  ELSE
    IF laScrMode[3] OR laScrMode[4]
      SHOW GET M.DYELOT ENABLED
    ENDIF
    =lfShowLine("D")
  ENDIF
ELSE
  *-- Check if the current order has multiple locations
  llEndOfKey = .T.
  IF laData[24]
    lcLnKeyExp = laData[1] + laData[2] + m.Color + m.cWareCode
  ELSE
    lcLnKeyExp = laData[1] + laData[2] + m.Color
  ENDIF
ENDIF  

llUpdLine = .T.

*-- Proceed with validation if it is the end of the key
IF llEndOfKey AND !EMPTY(laData[2])
  SELECT (lcTmpMMOLn)
  =SEEK(lcLnKeyExp)

  IF FOUND()
    IF !llDyelot or m.DyeYN = 'N' 
       lnRecPos=RECNO()
       llUpdLine = .F.
    ENDIF  
  ENDIF  
  lnRecNo = IIF(EOF(lcTmpMMOLn), 0 ,RECNO(lcTmpMMOLn))

  *-- if it already exists, delete the current record,
  IF EMPTY(CSTATUS)  AND SEEK(lcLnKeyExp) &&AND !llNoAdd
    GO lnRecNo
    *-- Position the record pointer on the existing record.
    SEEK(lcLnKeyExp)
    *-- Refresh display
    =lfwLnBrow()
    llUpdLine = .F.
  ELSE
    IF !EOF()
      IF lnRecNo > 0
        GO lnRecNo
      ENDIF

      *-- REPLACE CSTATUS in add  mode with A --> Add
      *--                    Edit mode with M --> Modified
      REPLACE cStatus WITH IIF(cStatus = "A" , "A" , "M")
    ENDIF
  ENDIF  
  lnNumOfDets = lnNumOfDets + 1
  *-- disable key objects
  *-- enable the rest of the objects 
  IF laScrMode[3] OR laScrMode[4]
    =lfShowLine("E")
  ENDIF
ELSE
  IF laScrMode[3] OR laScrMode[4]
  IF EMPTY(lcObject)
    =lfShowLine("E")
  ELSE
    =lfShowLine("D")
  ENDIF
  ENDIF
ENDIF
SELECT (lnAlias)

RETURN llUpdLine

*-- End OF lfChkLnKey.
*:***************************************************************************
*: Name       : lfShowLine
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : 
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None
*:***************************************************************************
*: Example    : = lfShowLine()
*:***************************************************************************
*:
FUNCTION lfShowLine
PARAMETERS lcShowSTat

IF lcShowSTat = "E"
  lcLineStat    = "ENABLE"
  SHOW GET m.Pattern    ENABLE
  SHOW GET m.Reference  ENABLE
  SHOW GET m.nMFGTotQty ENABLE
  SHOW GET m.Dyelot     ENABLE
ELSE
  lcLineStat    = "DISABLE"
  SHOW GET m.Pattern    DISABLE
  SHOW GET m.Reference  DISABLE
  SHOW GET m.nMFGTotQty DISABLE
  SHOW GET m.Dyelot     DISABLE

ENDIF  

*-- End OF lfShowLine
*:*************************************************************
*: Name       : lfvMFGOrdNo
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Valid function for get field m.cMfgRwNum, 
*:             (key field)
*:*************************************************************
*: Calls      : lfMFGOrdShow()
*:*************************************************************
*: Passed Parameters  :  None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  =lfvMFGOrdNo()
*:*************************************************************
*:
FUNCTION lfvMFGOrdNo
PRIVATE  lnCurAlias, lnOption
lnCurAlias = SELECT (0)
laData[1]  = PADR(ALLTRIM(laData[1]), 6)

IF MDOWN()
  RETURN
ENDIF

IF llBrowse .OR. !EMPTY(laData[1])
  lnCurAlias = SELECT(0)
  SELECT MaRwhdr
  *-- If browsing
  IF llBrowse .OR. !SEEK(laData[1])
     lcOrdNo = laData[1]
    IF gfRWOrdBr(@lcOrdNo)
      SCATTER FIELDS &lcScFields MEMO TO laData
      laScrMode=.F.
      laScrMode[2]=.T.
      SHOW GETS
    ELSE
     laData[1]   = ''
     _CUROBJ     = _CUROBJ
    ENDIF
  ELSE
     IF !SEEK(laData[1])
       *-- Browse the orders from the base file.  
       =ARIABROW()
    ELSE
      SCATTER FIELDS &lcScFields MEMO TO laData
      laScrMode   = .F.
      laScrMode[2]= .T.
      SHOW GETS
    ENDIF
  ENDIF
  llBrowse = .F.
ENDIF

SELECT (lnCurAlias)

*- End Of lfvMFGOrdNo.
*:*************************************************************
*: Name      : gfRWOrdBr
*: Developer : Abdou Elgendy
*: Date      : 03/18/2001
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
                "Material Rework Orders",;
                gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","",;
                "MaRwhdr.cMfgRwNum","laData")
    ELSE             
      SET ORDER TO TAG MaRwLin
      llFound = ARIABROW([lcFabric] + lcForCond,;
                "Material Manufacturing Orders",;
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
*: Name       : lfvData_2
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Valid function for get field laData[2]
*:             Called from lcCTCHWin2 
*:*************************************************************
*: Calls      : FABROW
*:*************************************************************
*: Passed Parameters  :  None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  =lfvData_2()
*:*************************************************************
*:
FUNCTION lfvData_2
PRIVATE lcSelFab, lnCurAlias

lnCurAlias = SELECT(0)
IF MDOWN()
  laData[2] = SPACE(7)
  RETURN
ENDIF

laData[2]    = PADR(ALLTRIM(laData[2]), 7)
IF llBrowse .OR. !EMPTY(laData[2]) .OR. LASTKEY() = 27
  lnCurAlias = SELECT()
  SELECT FABRIC
  lcSelFab = laData[2]
  IF llBrowse .OR. '?' $ laData[1] .OR. !SEEK(laData[2]) 
    IF FABROW(@lcSelFab, '*') 
      laData[2] = lcSelFab
    ELSE
      laData[2] = SPACE(7)
      _CUROBJ   = _CUROBJ
    ENDIF
  ENDIF
  SELECT (lnCurAlias)
  IF !EMPTY(laData[2])
    *--Enter M.F.G. Orders for Manufactured Materials Only.

    IF !Fabric.Make .AND. gfDialog('I','Only Manufactured Materials are allowed.')=1
      laData[2] = SPACE(7)
      _CUROBJ   = _CUROBJ
    ELSE
      *-- Get fabric information
      lcFabDesc  = FABRIC.Desc
      m.DyeYN    = FABRIC.cDye_Flg
      DO CASE
        CASE laScrMode[1]
          lcOrdNo=laData[1]
          IF gfRWOrdBr(@lcOrdNo,lcSelFab)
            laScrMode   =.F.
            laScrMode[2] =.T.
            SHOW GETS 
          ELSE
            laData[2]  = SPACE(7)
            _CUROBJ    = _CUROBJ
          ENDIF
      
        CASE laScrMode[4]
          *-- Add the Warehouse By Default first one.
          = lfAddWare ()
          *-- Default entered date with the system date
          laData[ 5] = gdSysDate
          *-- Default complete date with the system date + 90 days
          laData[ 6] = laData[ 5] + 90
          
          = lfvEntered()
          = lfvComplet()
          
          SHOW GET laData[2] DISABLE
          SHOW GET ibFabBrow DISABLE
          SHOW GET laData[ 5] ENABLE
          SHOW GET laData[ 6] ENABLE

          *-- we have to check the full key case there is no dyelots and no multi W/H
          =lfChkLnKey('C')
           
          *-- Don't ad new lines in case you append from Other Mfg order.
          =lfGenLines(laData[2])
          SHOW GET pbMONote    ENABLE
          SHOW GET ibFolder[3] ENABLE
          
          =lfRefresh(lcCTChWin2)
          
          llBrowse = .F.
       ENDCASE      
    ENDIF
    =gfUpdate()
    llBrowse = .F.
  ENDIF    && ENDIF !EMPTY(laData[2])
ENDIF
=lfUpdVars()

*-- flage to handel the Option menu.
SELECT (lnCurAlias)

*-- End OF lfvData_2.
*:***************************************************************************
*: Name       : lfvEntered
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Material Manfacturning Order Entered date valid function.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None
*:***************************************************************************
*: Example    : = lfvEntered()
*:***************************************************************************
*:
FUNCTION lfvEntered
PRIVATE lnPrevAl

lnPrevAl = SELECT(0)
SELECT (lcTmpMMOLn)

IF EMPTY(laData[5])
  laData[5] = ldOEntered
  _CUROBJ   = _CUROBJ
ENDIF


*-- Update the new value in the uncomltet session file.
llNoThing = lfUpdVars()

*-- End OF lfvEntered.
*:***************************************************************************
*: Name       : lfUpdVars
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : To update the incomplete session record with the
*:              needed files names and variables.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : = lfUpdVars()
*:***************************************************************************
*:
FUNCTION lfUpdVars
PRIVATE lnAlias
lnAlias=SELECT(0)
IF lnUnCmSeRc # 0
  *-- Build the files names string.
  lcFiles = "lcTmpMMOLn," + lcTmpMMOLn + ","   + ORDER(lcTmpMMOLn) + "; "
  *-- Go and update the incomplete session record.
  llNoThing = gfSavSess(lcProgID, lcFiles, @laVars, lcSession)
ENDIF
SELECT (lnAlias)

*-- End Of lfUpdVars.
*:***************************************************************************
*: Name       : lfvComplet
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Material Manfacturning Order Complete date valid function.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None
*:***************************************************************************
*: Example    : = lfvComplet()
*:***************************************************************************
*:
FUNCTION lfvComplet

IF EMPTY(laData[6])
  laData[6] = ldOComplet
  _CUROBJ   = _CUROBJ
ELSE
  IF laScrMode[3] AND MARWHDR.Complete # laData[6]
    *-- Would you like the system to store 99\99\99 as the
    *-- initial comp. date ?
    *-- < Yes > < No >
    lcDate = DTOC(MARWHDR.Complete)
    IF gfModalGen("QRM36146B38006","DIALOG",lcDate) = 1
      laData[6] = MARWHDR.Complete
      llNoThing  = lfRefresh(lcCTChWn3)
    ENDIF
  ENDIF
ENDIF

*-- Update the new value in the uncomltet session file.
llNoThing = lfUpdVars()

*-- End OF lfvComplet.
*:*************************************************************
*: Name      : lfGenLines
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose   : Generates details lines for MMO in ADD Mode.
*:*************************************************************
*: Calls     :  None.
*:*************************************************************
*: Parameters:  None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  lfGenLines()
*:*************************************************************
*:
FUNCTION lfGenLines
*-- Paramter that holds data of Fabric ,so if we're in Add Mode
*-- Colors,Patterns...etc,will be displayed in Order lines with 0 quantites
PARAMETERS lcFabric
PRIVATE lcSetExa, lnCurAlias

*--Having selected a fabric before reaching this step,
lnNumOfDets = 0
lnCurAlias = SELECT(0)

*-- Place the record pointer in FABRIC file on the current fabric.
SELECT FABRIC
IF SEEK(lcFabric)
  *-- Collect all available colors for the fabric, lcFabric, 
  *-- from FABRIC file. Place each color in a record as a default
  *-- detail line with zero quantities and Transaction code = '1',
  *-- giving it an 'A'dded record status.
  SCAN REST WHILE Fabric = lcFabric
  
    lnNumOfDets = lnNumOfDets + 1
    INSERT INTO (lcTmpMMOLn) ;
             (cFabric, Color, TranCd, Pattern, nmfgtotqty, cStatus, cWareCode, LineNo, DyeYN);
             VALUES (lcFabric, FABRIC.Color, '1', FABRIC.Pattern, 0.000, 'A',m.cWareCode,0, FABRIC.cDye_Flg)
  ENDSCAN
  GO TOP IN (lcTmpMMOLn)
ENDIF

=lfwLnBrow()

SELECT (lnCurAlias)
RETURN lnNumOfDets

*-- End of lfGenLines.
*:*************************************************************
*: Name      : lfwOldValm
*: Developer : Abdou Elgendy [ABD]
*: Date      : 03/18/2001
*: Purpose   : When function of get fields.
*:             Stores a fields old value
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  lfwOldVals()
*:*************************************************************
*:
FUNCTION lfwOldValm
lcOldVal = EVALUATE('m.'+SYS(18))

*-- End OF lfwOldValm.
*:*************************************************************
*: Name      : lfwOldVal
*: Developer : Abdou Elgendy [ABD]
*: Date      : 03/18/2001
*: Purpose   : When function of get fields.
*:             Stores a fields old value
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  lfwOldVals()
*:*************************************************************
*:
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))

*-- End OF lfwOldVal.
*:*************************************************************
*: Name      : lfvData27
*: Developer : Abdou Elgendy [ABD]
*: Date      : 03/18/2001
*: Purpose   : validate the vendor field
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  lfvData27()
*:*************************************************************
*:
FUNCTION lfvData27

IF MDOWN() 
  RETURN
ENDIF
IF llBrowse .OR. (!EMPTY(laData[27]) .AND. !SEEK(laData[27],"ApVendor"))
      lcVendor = laData[27]
      =gfApVnBrow (@lcVendor,.F.,'M')
      laData[27] = lcVendor
ENDIF
IF EMPTY(laData[27])
  STORE '' TO lcVEnAdd1 , lcVEnAdd2 , lcVEnAdd3 ,;
              lcVEnAdd4 , lcVEnAdd5 , lcVenName
  =lfRefresh()
  RETURN
ENDIF

lcVenName = LOOKUP(ApVendor.cvencomp ,laData[27] ,ApVendor.cvendcode)
lcVEnAdd1 = gfGetAdr('ApVendor','','','',1,'')
lcVEnAdd1 = lcVEnAdd1+IIF(LEN(lcVEnAdd1)<30,SPACE(30-LEN(lcVEnAdd1)),'')
FOR lnCounter = 1 TO 5
  L=ALLTRIM(STR(lnCounter))
  lcTempVr=''
    FOR lnCount = 1 TO ALEN(laAddress,1)
      IF laAddress[lnCount,1] = lnCounter
        lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
        lcTempVr  = lcTempVr+IIF(EMPTY(lcTempVr) .OR. RIGHT(lcTempVr,1) = ',' ,'',', ') + lcAddPart
      ENDIF
    ENDFOR
    lcVEnAdd&L=lcTempVr+IIF(LEN(lcTempVr)<30,SPACE(30-LEN(lcTempVr)),'')
ENDFOR

llBrowse = .F.

SELECT(lcBaseFile)

=lfRefresh()
*-- End OF lfvData27.
*:*************************************************************
*: Name      : lfvData4
*: Developer : Abdou Elgendy [ABD]
*: Date      : 03/18/2001
*: Purpose   : validate the store field
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  lfvData4()
*:*************************************************************
*:
FUNCTION lfvData4

IF llBRowse .OR. !SEEK( laData[4],  'WareHous' )
  llBRowse = .F.
  laData[4] = gfBrowWare( .T. )
  IF EMPTY(laData[4])
    IF EMPTY(lcOldVal)
      =gfModalGen('INM36001B36000','DIALOG')
      _CUROBJ = OBJNUM(laData[4])
      RETURN
    ELSE
      laData[4] = lcOldVal
    ENDIF  
  ENDIF
ENDIF

IF !EMPTY(laData[4]) AND laData[4] <> lcOldVal
  =SEEK(laData[4],"WAREHOUS")
  lcWareDesc  = WAREHOUS.cDesc
  ladata[28] = gfGetAdr('WareHous','','','',1,'')
  ladata[28] = ladata[28] +IIF(LEN(ladata[28])<30,SPACE(30-LEN(ladata[28])),'')
  FOR lnCounter = 1 TO 5
    L=ALLTRIM(STR(lnCounter))
    lcTempVr=''
      FOR lnCount = 1 TO ALEN(laAddress,1)
        IF laAddress[lnCount,1] = lnCounter
          lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
          lcTempVr = lcTempVr+IIF(EMPTY(lcTempVr) .OR. RIGHT(lcTempVr,1) = ',' ,'',', ') + lcAddPart
        ENDIF
      ENDFOR
      ladata[27+lnCounter] =lcTempVr+IIF(LEN(lcTempVr)<30,SPACE(30-LEN(lcTempVr)),'')
      SHOW GET ladata[27+lnCounter] ENABLE
  ENDFOR

  SELECT (lcTmpMMOLn)
  IF !EOF()
    REPLACE ALL cWareCode WITH laData[4]
  ENDIF

ENDIF

m.cWareCode = laData[4]



SELECT(lcBaseFile)
=lfRefresh()

*-- End OF lfvData4
*:*************************************************************
*: Name       : lfAdUnCmSR
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : To add an incomplete session record.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfAdUnCmSR()
*:*************************************************************
*:
FUNCTION lfAdUnCmSR
PRIVATE lnAlias

lnAlias = SELECT(0)

*-- Open the UnCmSess file.
= gfOpenFile(gcDataDir+"UnCmSess","TRANS","SH")

*-- Look for any record with the status "Initial".
SELECT UnCmSess
IF !SEEK('I')
  *-- If you do not find any, please add a new record.
  APPEND BLANK
ENDIF

*-- Save the record number..
lnUnCmSeRc = RECNO()

*-- Be sure to blank the record..
BLANK

*-- Update the record with my program information.
REPLACE Status     WITH 'O'       ,;
        cUTranType WITH lcProgID  ,;
        cProgram   WITH lcProgID  ,;
        cCurrScr   WITH lcProgID  ,;
        cUserId    WITH gcUser_id ,;
        cSession   WITH lcSession ,;
        cCurrObj   WITH SPACE(0)  ,;
        dTranDate  WITH gdSysDate ,;
        cTranTime  WITH TIME()
llNoThing = lfUpdVars()
llNoThing = RLOCK()

SELECT(lnAlias)

*-- End OF lfAdUnCmSR.
*:***************************************************************************
*: Name       : lfUpdUnCmS
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : To update the incomplete session record.
*:***************************************************************************
*: Parameters : lcStatus : Update the record with this status.
*:              lcCurObj : Update the record with this object name.
*:              llSetCon : Lock\Unlock the record.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : = lfUpdUnCmS()
*:***************************************************************************
*:
FUNCTION lfUpdUnCmS
PARAMETERS lcStatus, lcCurObj, llSetCon
PRIVATE lnAlias

*-- If there is an incomplete session record for this session..
IF lnUnCmSeRc <> 0
  lnAlias  = SELECT(0)
  lcStatus = UPPER(LEFT(lcStatus,1))

  *-- Go and update it..
  SELECT UnCmSess
  GOTO lnUnCmSeRc
  UNLOCK
  REPLACE cCurrObj WITH lcCurObj ,;
          Status   WITH lcStatus
  llNoThing = RLOCK()

  *-- If you are asked to unlock the record, please do..
  IF !llSetCon
    UNLOCK 
  ENDIF

  SELECT(lnAlias)
ENDIF

*-- End OF lfUpdUnCmS.
*:*************************************************************
*: Name       : lfChkUnComS
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : To check if there is any privous incomplete 
*:              session for the program.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : .T. -> Yes I found a privous incomplete session.
*:              .F. -> No I did not find any privous incomplete 
*:                     session.
*:*************************************************************
*: Example    : = lfChkUnComS()
*:*************************************************************
*:
FUNCTION lfChkUnComS
PRIVATE llFondSess, lnAlias

= gfOpenFile(gcDataDir+"UnCmSess","TRANS","SH")

*-- Is there any previous incomplete program session ???
IF gfUnCompSession(lcProgID, lnThisSess, "Material Manufacturing Order")
  *-- If yes, please tell me where did we stop in that session..
  llFondSess = .T.
  lcCurObj   = ALLTRIM(UnCmSess.cCurrObj)
  = lfRestVar()
  SHOW GETS
  IF !EMPTY(lcCurObj)
    _CUROBJ = OBJNUM(&lcCurObj)
    KEYBOARD "{ENTER}" PLAIN CLEAR
  ENDIF
ELSE
  *-- If there is no incomplete session, please create the temp
  *-- files.
  = lfCrtTmpLn()
  llFondSess = .F.
ENDIF

*-- Do all the needed browses..
lnAlias    = SELECT(0)
lcFile     = IIF(laScrMode[1] OR laScrMode[2], "MARWLIN", lcTmpMMOLn)
SELECT (lcFile)
GOTO TOP
llNoThing = lfLnBrowse() AND lfwLnBrow()
SELECT(lnAlias)

RETURN (llFondSess)

*-- End OF lfChkUnComS
*:*************************************************************
*: Name       : lpClsScr
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Local cancel function.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None
*:*************************************************************
*: Example    : = lpClsScr()
*:*************************************************************
*:
FUNCTION lpClsScr
PRIVATE lnAlias

llNoThing = lfUpdUnCmS("Complete", SPACE(0), .F.)


*- End OF lpClsScr.
*:*************************************************************
*: Name       : lfShowEnd
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : To be called at the end of the lpShow
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfShowEnd()
*:*************************************************************
*:
FUNCTION lfShowEnd

lcMmfOrd  = laData[1]
IF laScrMode[2]
  GOTO TOP IN MARWLIN
ENDIF
lcFltFile = IIF (laScrMode[1] OR laScrMode[2], "MARWLIN", lcTmpMMOLn)
lcOrderKy = IIF (laScrMode[3],&lcFltFile..cFabric,"1"+lcMmfOrd+&lcFltFile..cFabric)

*-- Refresh the "Delete" bar in the system menu, and the lines browse
*-- and the active folder.
llNoThing = lfStDelBar()
llNoThing = lfActFolder()

*-- End Of lfshowEnd.
*:*************************************************************
*: Name      : lfvFldObj
*: Developer : Abdou Elgendy [ABD]
*: Date      : 03/18/2001
*: Purpose   : Valid function of any free format get field
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example            :  =lfvFldObj()
*:*************************************************************
*:
FUNCTION lfvFldObj
PRIVATE lcFldName, lcObjVal, lnAlias

lcFldName = SYS(18)
lcObjVal  = EVALUATE("m."+lcFldName)
IF lcOldVal <> lcObjVal
  lnAlias    = SELECT(0)
  SELECT(lcTmpMMOLn)
  REPLACE &lcFldName WITH PADR(ALLTRIM(lcObjVal), FSIZE(lcFldName, "MARWLIN")),;
          cStatus WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)  
  =gfUpdate()
  SHOW WINDOW (lcLinesBrw) REFRESH SAME
  SELECT (lnAlias)        
ENDIF

*-- End OF lfvFldObj.
*:***************************************************************************
*: Name       : lfvQty
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Current edited quantity field valid function.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None
*:***************************************************************************
*: Example    : = lfvQty()
*:***************************************************************************
*:
FUNCTION lfvQty
PRIVATE lnAlias, lcVarNam , lnLineNo

IF m.nMFGTotQty  <> lcOldVal
  *-- If negative, present a message
  IF m.nMFGTotQty < 0
     m.nMFGTotQty = lcOldVal
    _CUROBJ = _CUROBJ
  ELSE
    lnAlias   = SELECT(0)
    llNewLine = laScrMode[4] OR &lcTmpMMOLn..cStatus = "A"

    SELECT (lcTmpMMOLn)
    lnNewTot   = m.nMFGTotQty
    lnOldTot   = lcOldVal
    lnLineNo   = &lcTmpMMOLn..LineNo
    llNoThing  = lfUpdHEst(lnOldTot, -1)
    
    *-- Seek In Fabric File to get Estimated cost
    *-- only in case of add mode
    *-- in case of Edit mode these cost elements contained in the MARWHDR file
    IF SEEK(laData[2]+M.Color, 'Fabric')
      
      REPLACE nCost1     WITH FABRIC.nMCost1    ,;
              nCost2     WITH FABRIC.nMCost2    ,;
              nCost3     WITH FABRIC.nMCost3    ,;
              nCost4     WITH FABRIC.nMCost4    ,;
              nmfgtotqty WITH m.nmfgtotqty ,;
              cStatus    WITH IIF(cStatus = "A" OR EMPTY(cStatus), "A", "M")
    ENDIF
   
    llNoThing =lfUpdHEst(lnNewTot, 1)
    IF llNewLine
      *-- Update budget amounts
      laData[7]  = laData[7]  - lnOldTot + lnNewTot
      *-- Update open amounts
      laData[11] = laData[11] - lnOldTot + lnNewTot
      REPLACE CWARECODE WITH ladata[4]
    ELSE
      IF laData[3] = "A"
        lnNewAct = laData[15]-(lnOldTot-lnNewTot)
        IF laData[15] <= lnNewAct
          laData[11] = laData[11] + (lnNewAct - laData[15])
          laData[15] = lnNewAct
        ELSE
          laData[10] = laData[10] + (laData[15] - lnNewAct)
          laData[11] = laData[11] - (laData[15] - lnNewAct)
        ENDIF
      ELSE
        lnNewBud = laData[7]-(lnOldTot-lnNewTot)
        IF laData[7] <= lnNewBud
          laData[11] = laData[11] + (lnNewBud - laData[7])
          laData[7] = lnNewBud
        ELSE
          laData[10] = laData[10] + (laData[7]-lnNewBud)
          laData[11] = laData[11] - (laData[7]-lnNewBud)
        ENDIF
      ENDIF
    ENDIF
    
    lNoThing = lfUpdVars()
    SELECT(lnAlias)
    SHOW WINDOW (lcLinesBrw) REFRESH SAME
  ENDIF  
ENDIF
SHOW GET PBSWITCH DISABLE

*-- End OF lfvQty.
*:*************************************************************
*: Name       : lfUpdHEst
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Update the MMO header estimated cost fields that are
*:              dislayed in the summary folder on the screen.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None
*:*************************************************************
*: Example    : = lfUpdHEst()
*:*************************************************************
*:
FUNCTION lfUpdHEst
PARAMETERS lnQty, lnSign
PRIVATE lnAlias, llComFrFab , lnFabQty , lnItemAmnt ,lnCstType

llComFrFab = .T.
IF laData[3] # "H"
  lnAlias = SELECT(0)

  *- Open the BomLine file.
  = gfOpenFile(gcDataDir+"BomLine","BomLine","SH")
  SELECT BomLine
  IF SEEK("T"+"1"+laData[1] + STR(LineNo,6))
    SCAN WHILE cIMTyp + cType + CtktNo    + STR(LineNo,6) =;
               "T"    + "1"   + laData[1] + STR(&lcTmpMMOLn..LineNo,6) ;
           FOR STYLE = PADR(&lcTmpMMOLn..cFabric,19) AND ;
               SCLR  = PADR(&lcTmpMMOLn..COLOR,6)    AND ;
               !lVoid
      lnFabQty          = lnQty
      lnItemAmnt        = lnFabQty * UnitQty * UnitCost
      lnCstType         = 11 + VAL(cBomTyp)
      laData[lnCstType] = laData[lnCstType]+(lnItemAmnt*lnSign)
      llComFrFab        = .F.
    ENDSCAN
  ENDIF
  SELECT(lnAlias)
ENDIF

IF llComFrFab
  laData[12] = laData[12] + (lnSign * FABRIC.nMCost1 * lnQty)
  laData[13] = laData[13] + (lnSign * FABRIC.nMCost2 * lnQty)
  laData[14] = laData[14] + (lnSign * FABRIC.nMCost3 * lnQty)
  laData[15] = laData[15] + (lnSign * FABRIC.nMCost4 * lnQty)
ENDIF

*-- End OF lfUpdHEst.
*:*************************************************************
*: Name       : lfvRem
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Remove line valid function.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None
*:*************************************************************
*: Example    : = lfvRem()
*:*************************************************************
*:
FUNCTION lfvRem
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcTmpMMOLn)

IF laScrMode[3] AND lfChkPrc()
  *-- There has been activity against this line. Not allowed to delete
  *-- < Ok > 
  = gfModalGen('TRM38101B00000')
ELSE
  *-- Are you sure you want to delete this line ?
  *-- < Yes > < No > 
  IF gfModalGen('QRM38102B38006') = 1
    llDelete = .T.
    IF llDelete
      SELECT (lcTmpMMOLn)
      llNoThing = lfUpdHEst(nMfgTotQty, -1)
      llNewLine = laScrMode[4] OR &lcTmpMMOLn..cStatus = "A"
      IF llNewLine
        laData[7]  = laData[7] - nMfgTotQty
        laData[11] = laData[11] - nMfgTotQty
      ELSE
        laData[10] = laData[10] + nMfgTotQty
        laData[11] = laData[11] - nMfgTotQty
      ENDIF
      IF cStatus = "A" OR EMPTY(cStatus)
        DELETE
      ELSE
        REPLACE cStatus WITH "D"
      ENDIF
      *DELETE
      GOTO TOP
      lnNumOfDets = lnNumOfDets - 1
      IF lnNumOfDets = 0
        SHOW GET pbRem DISABLE
        =lfShowLine("D")
      ENDIF
      = lfwLnBrow()
    ENDIF
  ENDIF
ENDIF

SELECT(lnAlias)

*-- End Of lfvRem.
*:*************************************************************
*: Name       : lfChkPrc
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Check that the active MMO line has no
*:              receivings, this check will be performed prior to 
*:              deleteing the active MMO line.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfChkPrc()
*:*************************************************************
*:
FUNCTION lfChkPrc

PRIVATE lcExp, lnAlias, llRetVal , lcOldTag

lnAlias    = SELECT(0)
llRetVal   = .F.
lcExp      = laData[1] + STR(m.LineNo,6)


SELECT MARWLIN
lcOldTag = ORDER()
SET ORDER TO TAG Mmfgorddln

IF SEEK(lcExp)
  LOCATE REST WHILE cMfgRwNum + STR(Lineno,6) = lcExp;
     FOR TranCD <> "1"
  llRetVal = FOUND()
ENDIF
SET ORDER TO (lcOldTag)
SELECT (lnAlias)
RETURN llRetVal

*-- End OF lfChkPrc.
*:*************************************************************
*: Name       : lfvNew
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Add new line valid function.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None
*:*************************************************************
*: Example    : = lfvNew()
*:*************************************************************
*:
FUNCTION lfvNew
PRIVATE lcSetDele, lnAlias


lnAlias = SELECT(0)
SELECT (lcTmpMMOLn)


*-- Prevent the user from adding more than one empty line
*-- Seek There is empty line 19 spaces ( 6-cMfgRwNum + 7-FABRIC + 6-COLOR )

IF !SEEK(SPACE(6)+laData[2]+SPACE(6))
  APPEND BLANK
ENDIF

m.cWareCode = laData[4]

REPLACE cMfgRwNum WITH laData[1] ,;
        cFabric   WITH laData[2] ,;
        cWareCode WITH m.cWareCode,;
        TranCd    WITH  '1'

lcClrStat     = "ENABLE"
SHOW GET m.Color ENABLE
SHOW GET ibColor ENABLE
=lfShowLine("D")
_CUROBJ       = OBJNUM(m.Color)
=lfwLnBrow()  
SELECT(lnAlias)
RETURN


lnAlias = SELECT(0)
SELECT (lcTmpMMOLn)
m.Color     = SPACE(6)

*-- Default the warehouse with the location of the header.
m.cWareCode = laData[4]

*-- Be sure that the user does not add more that one empty new line.
lcSetDele = SET("DELETED")
SET DELETED OFF

IF !SEEK(" ")
  =SEEK(" ")
  INSERT INTO (lcTmpMMOLn);
  	(cMfgRwNum, cFabric, cWareCode, TranCd, cStatus);
	VALUES (laData[1], laData[2], m.cWareCode,'1',"A")

ELSE
 REPLACE cMfgRwNum WITH laData[1] ,;
  		  cFabric   WITH laData[2] ,;
  		  cWareCode WITH m.cWareCode,;
  		  TranCd    WITH  '1',;
  		  cStatus   WITH "A"       
ENDIF

SET DELETED &lcSetDele
lcClrStat     = "ENABLE"
SHOW GET m.Color ENABLE
SHOW GET ibColor ENABLE
=lfShowLine("D")
_CUROBJ       = OBJNUM(m.Color)

=lfwLnBrow()  
SELECT(lnAlias)

*-- End OF lfvNew.
*:*************************************************************
*: Name      : lfvColor
*: Developer : Abdou Elgendy [ABD]
*: Date      : 03/18/2001
*: Purpose   : Validation color field
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*:
FUNCTION lfvColor
PRIVATE lnAlias , lnRecNo

*--lnRecNo  hold current record in lcTmpMMOLn file
*--lnAlias  hold the current number area
*--lnRecPos hold  the record number in case no dye for returng back it


lnRecPos=0
lnAlias = SELECT(0)
lnRecNo = IIF(EOF(lcTmpMMOLn) , 0 , RECNO(lcTmpMMOLn))
IF MDOWN()
  RETURN
ENDIF

*-- If browsing, or, if the field is not empty
IF llBrowse .OR. !EMPTY(m.Color)
  *-- If the color is a valid color from the Fabric file, 
  lnAlias = SELECT(0)
  IF lfVldColor(@m.Color)
    *-- Check for duplicate entries..
    lnRecNo = IIF(EOF(lcTmpMMOLn) , 0 , RECNO(lcTmpMMOLn))
    IF lfChkLnKey("C")
      IF lnRecNo > 0
        GO lnRecNo IN &lcTmpMMOLn
      ENDIF
      SELECT(lcTmpMMOLn)
      REPLACE Color    WITH m.Color,;
              DyeYN    WITH FABRIC.cDye_Flg,;
              PATTERN  WITH FABRIC.PATTERN

      m.Pattern = FABRIC.PATTERN
    ELSE   && lfchklnkey
      IF !llDyelot or m.DyeYN = 'N' 
        SELECT(lcTmpMMOLn)
        IF !EOF()
          GO lnRecNo
          BLANK
          DELETE
          GO lnRecPos
          =lfwLnBrow()
          lnRecPos=0
        ENDIF
      ENDIF  
    ENDIF && lfchklnkey
    SHOW GET m.Color DISABLE
    SHOW GET ibColor DISABLE
    lcClrStat      = "DISABLE"
    IF laData[24]
      SHOW GET puLnWare ENABLE
      lcWareStat = "ENABLE"
    ELSE
      m.cWareCode = ladata[4]
      REPLACE cWareCode WITH m.cWareCode

    ENDIF
    IF llDyelot
      IF laScrMode[4] .AND. m.DyeYN = 'Y'
        SHOW GET m.Dyelot ENABLE
        lcDyeStat = "ENABLE"
      ELSE
        SHOW GET m.Dyelot DISABLE
        lcDyeStat = "DISABLE"
      ENDIF
    ENDIF  
    *-- Else 
  ELSE && if lfVldColor
    *-- Clear the field, and make it the current object.
    SELECT(lcTmpMMOLn)
    IF !EOF()
      GO lnRecNo
      BLANK
      DELETE
      GO TOP
      =lfwLnBrow()
    ENDIF  
    SHOW GET ibColor DISABLE
    SHOW GET m.Color DISABLE
    SHOW GET m.pattern ENABLE
    SHOW GET m.reference ENABLE
    SHOW GET m.nmfgtotqty ENABLE
    _CUROBJ = OBJNUM(pbNew )
  ENDIF  && if lfVldColor
ELSE  && if llBrowse .OR. !EMPTY(m.Color)
  SELECT(lcTmpMMOLn)
  IF !EOF()
    GO lnRecNo
    BLANK
    DELETE
    GO TOP
    =lfwLnBrow()
  ENDIF  
  SHOW GET ibColor DISABLE
  SHOW GET m.Color DISABLE
  SHOW GET m.pattern ENABLE
  SHOW GET m.reference ENABLE
  SHOW GET m.nmfgtotqty ENABLE
  _CUROBJ = OBJNUM(pbNew )

ENDIF  && if llBrowse .OR. !EMPTY(m.Color)

SELECT (lnAlias)

*-- End Of lfvColor
*:*************************************************************
*: Name      : lfVldColor
*: Developer : Abdou Elgendy [ABD]
*: Date      : 03/18/2001
*: Purpose   : Validation color field
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*:
FUNCTION lfVldColor
PARAMETER lcColor
PRIVATE lcVldClr, llRetVal , lcAlias

lcAlias = SELECT(0)
lcVldClr = lcColor

*-- IF browsing or, the color is not found in fabric file

IF llBrowse .OR. !SEEK(laData[2] + lcVldClr, "Fabric")
  lcVldClr = CHR(240)
  IF FABROW(laData[2], @lcVldClr)
    llRetVal = .T.
    lcColor  = lcVldClr
  *-- Assign value to m.DyeYN variable
  m.DyeYN = FABRIC.cDye_Flg
  llRetVal = .T.  
  ELSE
    llRetVal = .F.
  ENDIF
ELSE
  *-- Assign value to m.DyeYN variable
  m.DyeYN = FABRIC.cDye_Flg
  llRetVal = .T.  
ENDIF
SELECT(lcAlias)
RETURN llRetVal


*-- End OF lfVldColor.
*:***************************************************************************
*: Name       : lpSavScr
*: Developer : Abdou Elgendy [ABD]
*: Date      : 03/18/2001
*: Purpose    : Local save function.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None
*:***************************************************************************
*: Example    : = lpSavScr()
*:***************************************************************************
PROCEDURE lpSavScr
PRIVATE lnAlias, lcFldNam

lnAlias    = SELECT(0)

*-- Perform saving validations
*-- If vendor is empty return
IF EMPTY(ladata[27])
  =gfModalGen('INM36002B36000','DIALOG','Vendor')
  _CUROBJ = OBJNUM(ladata[27])
  llCSave = .F.
  RETURN
ENDIF



*-- If total pieces are zero, do not save

IF !lfChkQty()
  *-- Total pieces is zero. Tansaction cancelled.
    = gfModalGen("INM38051B00000")
  llCSave = .F.
ELSE

  SELECT (lcTmpMMOLN)
  SET FILTER TO
  GO TOP

  *--Set Relation to Fabric File To Seek when FAbric+Color found
  SET RELATION TO cFabric+Color INTO Fabric ADDITIVE

  *-- Set Relation to FabDye File To Seek if FAbric + Color + WareHouse + Dyelot found
  SET RELATION TO cFabric + Color + cWareCode + IIF(EMPTY(&lcTmpMMOLn..Dyelot) , SPACE(10) ,  &lcTmpMMOLn..Dyelot) INTO FabDye ADDITIVE

  lnLstLine  = 0
  llRecSaved = .F.

  *-- Check if Manual Rework order number.
  llEdtMONum = gfGetMemVar('M_GENRWNUM') = "Y"
  laData[01] = IIF(laScrMode[4], lfGetMONum(), laData[1])

  *-- If there is no open quantity
  IF laScrMode[3] AND laData[11] = 0
    *-- Complete the order if it is received, otherwise, Cancel
    laData[3]    = IIF(laData[8] > 0, "C", "X")
    llNoThing    = lfSetStat(laData[3])
    lcPromp      = IIF(laData[3]="X", lcUnCanc, lcCancel) 
    lcCanStat    = "DISABLE"
    llNoThing    = lfRefresh() AND lfStDelBar()
    lcEdtStat    = "DISABLE"
    laCtrStat[7] = lcEdtStat
    SHOW GET pbEdt &lcEdtStat
  ENDIF

  *-- Open the BOM\FABDYE files.
  =gfOpenFile(gcDataDir+"BOM","BOM","SH")

  *-- Recall all the deleted lines.
  lnODelStat = SET ("DELETE")
  SET DELETE OFF
  SELECT (lcTmpMMOLn)
  RECALL FOR cMfgRwNum = laData[1] .AND. cStatus ='D'
  
  SET DELETE &lnODelStat

  *-- Count all the records to be processed to be used in the 
  *-- Thermometer.
  SELECT (lcTmpMMOLn)
  COUNT ALL TO lnTotLine FOR cFabric+cWareCode+Dyelot = "" AND !EMPTY(cFabric)
  lcHdrMsg  = "Updating Material Manufacturing Order..."
  lnCurLine = 0

  *-- These two files are required by the global function that 
  *-- updates the BOM files.
  llOpnOpr  = gfOpenFile(gcDataDir+"MFGOprHd","MFGOprHd","SH")

  *-- Update the uncomplete session file with the object name.
  llNoThing = lfUpdUnCmS("Open", "pbSav", .T.)

  SELECT (lcTmpMMOLn)
  GO TOP

  llNoThing  = RLOCK()
  REPLACE ALL cMfgRwNum WITH laData[1]
  UNLOCK

  SCAN FOR !EMPTY(cFabric)
    lnCurLine = lnCurLine + 1
    = gfThermo(lnTotLine, lnCurLine, lcHdrMsg, &lcTmpMMOLn..cFabric)
    SCATTER FIELDS nMfgTotQty  TO laNewQty

    DO CASE
  
      CASE cStatus = "A"
        llNoThing = IIF(nMfgTotQty > 0, lfAddLine(), .T.)
    
      CASE cStatus = "M"

        llNoThing = lfModLine()
      
      CASE cStatus = "D"
        llNoThing = lfDelLine()
      
      CASE cStatus = "S"
        *-- Nothing has to be done here.
     ENDCASE

  ENDSCAN
  
  *-- Update the uncomplete session file.
  llNoThing = lfUpdUnCmS("Complete", SPACE(0), .F.)
  
  lcScrMode = IIF(laScrMode[3], "V", "S")
  IF WEXIST('gwdThermo')
    =gfThermo(lnTotLine,lnTotLine,lcHdrMsg,"")
  ENDIF
  
  *-- If Add mode add new record to the header file
  *-- Else (Edit Mode) Update the header file form MEMVAR(s)
  
  IF laScrMode[4]
    = lfDone()
  ELSE
    SELECT MARWHDR
    GATHER FROM laData FIELDS &lcScFields
    REPLACE LastLine WITH lnLstLine
  ENDIF
  SELECT (lcTmpMMOLN)

  *-- Remove the relations
  SET RELATION TO
  SELECT MARWHDR
  SET RELATION TO
  SELECT MARWLIN
  SET RELATION TO
  
  *-- Select base file to make global save unlock the record in base file
  SELECT (lcBaseFile)


ENDIF
*-- End Of lpSavScr.
*:*************************************************************
*: Name       : lfvSwitch
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Controls the show & hide of the "Average Unit Cost" Screen
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfvSwitch()
*:*************************************************************
*:
FUNCTION lfvSwitch
PRIVATE lnOldAlias

lnOldAlias = SELECT()
SELECT MARWHDR
IF llByUnit
  SHOW GET pbSwitch,1 PROMPT 'C\<ost by Unit' 
  llByUnit = .F.
ELSE
  SHOW GET pbSwitch,1 PROMPT 'C\<ost by MMO'
  llByUnit = .T.
ENDIF
= lfRefresh()
SHOW GET PbSwitch
SELECT (lnOldAlias)


*-- End OF lfvSwitch.
*:*************************************************************
*: Name       : lfvDyelot
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : The Dyelot valid function.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None
*:*************************************************************
*: Example    : = lfvDyelot()
*:*************************************************************
*:
FUNCTION lfvDyelot
PRIVATE lnAlias, lnDyeRec
lnAlias = SELECT(0)
SELECT (lcTmpMMOLn)
lnDyeRec = IIF(EOF(),0,RECNO())
IF !EMPTY(m.Dyelot)
  lnRecTmp = IIF(EOF(),0,RECNO())
  lcExp = cMfgRwNum + cfabric + m.color + m.cwarecode + m.dyelot + trancd
  IF SEEK(lcExp) AND lnRecTmp <> RECNO()
    IF lnRecTmp > 0
      GOTO lnRecTmp
    ENDIF
    BLANK
    DELETE
    = SEEK(lcExp)
  ELSE
    SELECT (lcTmpMMOLn)
    IF lnRecTmp > 0
    GOTO lnRecTmp
    ENDIF
    llSaveRec = .T.

    IF llSaveRec
      REPLACE Dyelot WITH m.Dyelot,;
              DYEYN  WITH 'Y'
      =lfChkLnKey('D')
      SHOW GET m.Dyelot DISABLE
    ELSE
      m.Dyelot = SPACE(10)
      _CUROBJ  = _CUROBJ
    ENDIF
  ENDIF
ENDIF  
IF lnDyeRec >0
  SELECT (lcTmpMMOLn)
  GO lnDyeRec
ELSE
ENDIF
llNoThing = lfwLnBrow()

SELECT(lnAlias)

*-- End OF lfvDyelot.
*:*************************************************************
*: Name       : lfChkQty
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Check before saving that Sum of Order quantity <> zero
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : .T. IF quantity > 0
*:              .F. IF quantity = 0 
*:*************************************************************
*: Example    : = lfChkQty()
*:*************************************************************
*:
FUNCTION lfChkQty
PRIVATE lnOrdQty,llFlag,lnCurAlias 

lnCurAlias = SELECT(0)
lnOrdQty=0
SELECT (lcTmpMMOLn)

SCAN
 lnOrdQty = lnOrdQty + &lcTmpMMOLn..nMfgTotQty
ENDSCAN

IF lnOrdQty >0
 llFlag=.T.
ELSE
 llFlag=.F.
ENDIF
SELECT(lnCurAlias)
RETURN llFlag
 

*-- End OF lfChkQty
*:*************************************************************
*: Name      : lfTotalAct
*: Developer : Abdou Elgendy [ABD]
*: Date      : 03/18/2001
*: Purpose   : to get the TOTAL OF ACTUAL COST
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None
*:*************************************************************
*: Example   : = lfTotalAct()
*:*************************************************************
*:
FUNCTION lfTotalAct
PARAMETERS lcTotal
RETURN (IIF(laData[3] $ 'A' AND laData[26] > 0 ,;
        ROUND((laData[20]+laData[21]+ladata[22]+ladata[23])/laData[26],4),;
        IIF(laData[3]$'CS' AND laData[8]>0,ROUND((laData[20]+laData[21]+ladata[22]+ladata[23])/laData[13],4),;
        0)))
        
*-- End of lfTotalAct.
*:*************************************************************
*: Name       : lfGetMONum
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Generate a new MMO number.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None
*:*************************************************************
*: Example    : = lfGetMONum()
*:*************************************************************
*:
FUNCTION lfGetMONum
PRIVATE lcRetVal

IF llEdtMONum
  lcMONum = SPACE(0)
  PUSH KEY 
  ON KEY
  DO (gcScrDir+gcWinAppl+"\MAMFRWNU.SPX")
  POP KEY
  lcRetVal = lcMONum
ELSE
  lcRetVal = gfSequence("cMfgRwNum","","")
ENDIF

RETURN (lcRetVal)

*-- End OF lfGetMONum.
*:*************************************************************
*: Name       : lfvMONumOk
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Valid ok button in the new MO number screen.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None
*:*************************************************************
*: Example    : = lfvMONumOk()
*:*************************************************************
*:
FUNCTION lfvMONumOk
PRIVATE llRet 

*-- You cannot leave the Manufacturing Rework Order number empty.
*-- Manufacturing Rework Order number must be six digits.
*-- This Manufacturing Rework Order number is already exist.

llRet = .F.
IF EMPTY(lcMONum)
  lnNoThing = gfModalGen('TRM38047B00000','DIALOG',"Manufacturing Rework Order")
ELSE
  IF LEN(ALLTRIM(lcMONum)) < 6
    lnNoThing = gfModalGen('TRM38048B00000','DIALOG',"Manufacturing Rework Order")
  ELSE
    IF SEEK(lcMONum,"MARWHDR")
      lnNoThing = gfModalGen('TRM38049B00000','DIALOG',"Manufacturing Rework Order")
    ELSE
      llRet = .T.
    ENDIF
  ENDIF
ENDIF

IF llRet
  CLEAR READ
ELSE
  _CUROBJ = OBJNUM(lcMONum)
ENDIF

*-- End Of lfvMONumOk.
*:*************************************************************
*: Name       : lfSetStat
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : To convert the status character to words that will
*:              be displayed in the screen.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : lcStat : The status code.
*:*************************************************************
*: Returns    : The "lcMFStatus" variable is updated.
*:*************************************************************
*: Example    : = lfSetStat()
*:*************************************************************
*:
FUNCTION lfSetStat
PARAMETERS lcStat

lcStat = IIF(TYPE("lcStat")#"C","",lcStat)
IF !EMPTY(lcStat)
  lcAll      = PADR("Open",10)+PADR("Complete",10)+PADR("Canceled",10)
  lcMFStatus = ALLTRIM(SUBSTR(lcAll,ATC(lcStat,"OCX")*10-9,10))
ELSE
  lcMFStatus = SPACE(0)
ENDIF

*-- End OF lfSetStat.
*:*************************************************************
*: Name       : lfAddLine
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Add new line in the MMO to the master files.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None
*:*************************************************************
*: Example    : = lfAddLine()
*:*************************************************************
*:
FUNCTION lfAddLine
PRIVATE lnAlias, lnOrdQty   
lnAlias    = SELECT(0)
lnXLine  = IIF(Empty(LineNo), lfGtNewLn(), LineNo)

SELECT (lcTmpMMOLn)
lnOrdQty = nMFGTotQty

llRecSaved = .T.
IF &lcTmpMMOLn..nStep < 2
  = lfUpdStp(lcTmpMMOLn, 2)
ENDIF

IF &lcTmpMMOLn..nStep < 3
  = lfUpdFabDye()
  = lfUpdStp(lcTmpMMOLn, 3)
ENDIF

IF &lcTmpMMOLn..nStep < 4
  SCATTER MEMVAR
  SELECT MARWLIN
  IF SEEK(' ')
    GATHER MEMVAR MEMO
  ELSE  
    INSERT INTO MARWLIN FROM MEMVAR
    = gfAdd_Info('MARWLIN')
  ENDIF
  llNoThing = lfUpdStp(lcTmpMMOLn, 4)
ENDIF    

SELECT(lnAlias)

*-- End OF lfAddLine.
*:*************************************************************
*: Name       : lfGtNewLn
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Generate a new MO line number.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None
*:*************************************************************
*: Example    : = lfGtNewLn()
*:*************************************************************
*:
FUNCTION lfGtNewLn
PRIVATE lnAlias

lnAlias = SELECT(0)
IF lnLstLine  = 0
   lcMstMMOLn = SPACE(0)
  = gfOpenFile(gcDataDir+"MARWLIN","MARWLIN","SH", @lcMstMMOLn, .T.)
      
  SELECT (lcMstMMOLn)
  IF SEEK(laData[1])
    SCAN REST WHILE cMfgRwNum = laData[1]
      lnLstLine = MAX(lnLstLine, LineNo)
    ENDSCAN
  ENDIF
  = gfCloseFile(lcMstMMOLn)
ENDIF
lnLstLine = lnLstLine + 1

SELECT (lcTmpMMOLn)
llNoThing = RLOCK()
REPLACE LineNo WITH lnLstLine
m.lineno = lnLstLine
UNLOCK

SELECT(lnAlias)
RETURN lnLstLine

*-- End OF lfGtNewLn.
*:*************************************************************.
*: Name       : lfUpdStp
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : To update the nStep field in a specific temp file.
*:*************************************************************
*: Parameters : lcFileName : Please update this file.
*:              lnStepNo   : Please update the step field with this
*:                           number.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfUpdStp("ARHIST", 1)
*:*************************************************************
*:
FUNCTION lfUpdStp
PARAMETERS lcFileName, lnStepNo
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcFileName)
*-- Update the needed file with the new step number..
llNoThing  = RLOCK()
REPLACE nStep WITH lnStepNo
UNLOCK
SELECT(lnAlias)

*-- End OF lfUpdStp.
*:*************************************************************
*: Name       : lfUpdFabDye
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Add the MMO lines qty to the onorder 
*:            : qty in the FabDye file.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None
*:*************************************************************
*: Example    : = lfUpdFabDye()
*:*************************************************************
*:
FUNCTION lfUpdFabDye
PRIVATE lnCurAlias 
lnCurAlias = SELECT(0)
SELECT (lcTmpMMOLn)

*-- If This Fabric Not Found In Fabdye File Add It Considering The Dyelot.

IF !SEEK(cFabric+color+cWareCode+SPACE(10),'FABDYE')
  =gpAdFabWar(cFabric,color, SPACE(10) ,cWareCode)
ENDIF

IF !SEEK(cFabric + color + cWareCode + IIF(EMPTY(DYELOT) , SPACE(10) , PADR(DYELOT,10)),'FABDYE')
  =gpAdFabWar(cFabric,color, IIF(EMPTY(DYELOT) , SPACE(10) , DYELOT),cWareCode)
ENDIF

SELECT(lnCurAlias)

*-- End OF lfUpdFabDye.
*:*************************************************************
*: Name       : lfModLine
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Modify a line in the MMO in the master files.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None
*:*************************************************************
*: Example    : = lfModLine()
*:*************************************************************
*:
FUNCTION lfModLine
PRIVATE lnAlias , lnRecNo , lnMasRec , lnOfsQty

lnAlias  = SELECT(0)
SELECT (lcTmpMMOLn)
lnRecNo = RECNO()
=SEEK(&lcTmpMMOLn..cMfgRwNum+&lcTmpMMOLn..cFabric+&lcTmpMMOLn..Color+&lcTmpMMOLn..Dyelot,'MARWLIN')
GO (lnRecNo)
lnOfsQty = nMFGTotQty - MARWLIN.nMFGTotQty

lnMasRec = IIF(EOF(),0,RECNO())

IF lnMasRec > 0
  GO lnMasRec
ENDIF

SELECT MARWLIN
IF SEEK(&lcTmpMMOLn..cMfgRwNum+&lcTmpMMOLn..cfabric+&lcTmpMMOLn..color+&lcTmpMMOLn..Dyelot)
  REPLACE nmfgtotqty with m.nmfgtotqty
ENDIF

lcNewWare  = &lcTmpMMOLn..cWareCode
lcOldWare  = MARWLIN.cWareCode
lcOldKey   = MARWLIN.cFabric + lcOldWare + SPACE(10)
lcNewKey   = &lcTmpMMOLn..cFabric + lcNewWare + SPACE(10)

IF &lcTmpMMOLn..nStep < 2
  =lfUpdStp(lcTmpMMOLn, 2)
ENDIF  
IF &lcTmpMMOLn..nStep < 3
  IF lcNewWare = lcOldWare
    =lfUpdFabDye()
    =lfUpdStp(lcTmpMMOLn, 3)
  ENDIF  
ELSE
  =lfUpdFabDye()
  =lfUpdStp(lcTmpMMOLn, 3)
ENDIF  


IF &lcTmpMMOLn..nStep < 4
  SELECT (lcTmpMMOLn)
  SCATTER MEMVAR
  SELECT MARWLIN
  GATHER MEMVAR
ENDIF
SELECT(lnAlias)


*-- End OF lfModLine.
*:*************************************************************
*: Name       : lfDelLine
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Delete line in the CT in the master files.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None
*:*************************************************************
*: Example    : = lfDelLine()
*:*************************************************************
*:
FUNCTION lfDelLine
PRIVATE lnAlias, lnOrdQty
lnAlias = SELECT(0)

lnOrdQty = &lcTmpMMOLn..nmfgtotqty * -1

IF SEEK(laData[1]+&lcTmpMMOLn..cFabric + &lcTmpMMOLn..Color + &lcTmpMMOLn..Dyelot + "1" , "MARWLIN")

  IF &lcTmpMMOLn..nStep < 2
    = lfUpdStp(lcTmpMMOLn, 2)
  ENDIF  

  IF &lcTmpMMOLn..nStep < 3
    = lfUpdFabDye()
    = lfUpdStp(lcTmpMMOLn, 3)
  ENDIF  
  IF &lcTmpMMOLn..nStep < 4
    SELECT MARWLIN
    IF SEEK(laData[1] + &lcTmpMMOLn..cFabric + &lcTmpMMOLn..Color + &lcTmpMMOLn..Dyelot + "1" , "MARWLIN")
      llNoThing = RLOCK()
      BLANK
      DELETE
      UNLOCK
    ENDIF
    llNoThing = lfUpdStp(lcTmpMMOLn, 4)
  ENDIF
ENDIF
SELECT (lnAlias)

*-- End of lfDelLine.
*:*************************************************************
*: Name       : lfDone
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Take the last actions to be taken when creating 
*:              new MMO
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None
*:*************************************************************
*: Example    : = lfDone()
*:*************************************************************
*:
FUNCTION lfDone
PRIVATE lnAlias

lnAlias  = SELECT(0)
IF llRecSaved
  SHOW GET laData[1]
  SELECT MARWHDR
  APPEND BLANK
  llRLock = RLOCK()
  GATHER FROM laData FIELDS &lcScFields
  REPLACE LastLine WITH lnLstLine
  UNLOCK
  = IIF(llEdtMONum,.T.,gfModalGen("INM36181B00000","",laData[1]))

ELSE
  *-- Total pieces is zero. Tansaction cancelled.
  = gfModalGen("INM38051B00000")
  *--Reduce sequence file as MMO not saved
  llSeqOpen = gfOpenFile(gcDataDir+"SEQUENCE","cSeq_Type","SH")
  SELECT SEQUENCE
  IF SEEK('cmMfgOrdH')
    REPLACE nSeq_No WITH (nSeq_No - 1)
  ENDIF
  IF llSeqOpen
    USE IN SEQUENCE
  ENDIF
ENDIF
SELECT (lnAlias)

*-- End OF lfDone.
*:***************************************************************************
*: Name       : lpDelScr
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Local Delete to Cancel / UnCancel MMO
*:            : This Function will be called only 
*:            : if the Oreder status is 'H' OR 'X'
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None
*:***************************************************************************
*: Example    : = gfCPDelete()
*:***************************************************************************
*:
FUNCTION lpDelScr
PRIVATE lnSign , lnAlias , llCont , lnToCan
STORE .T. TO llCont
STORE 0 TO lnToCan

*-- Save current ENVIRONMENT settings
lnAlias = ALIAS()

*-- IF the status is Hold We're going to cancel this Order
*-- OTHERWISE Un-Cancel
IF laData[3] = 'O'
  STORE -1 TO lnSign
ELSE
  STORE 1 TO lnSign
ENDIF


SELECT MARWLIN

IF SEEK(laData[1])
  SCAN REST WHILE cMfgRwNum = laData[1] FOR TRANCD = '1'
    lnToCan = nMFGTotQty + lnToCan
  ENDSCAN
  IF llCont AND SEEK(laData[1],'MARWHDR')
    SELECT MARWHDR
    =RLOCK()
    REPLACE STATUS WITH IIF(lnSign < 0 ,'X','O')
    REPLACE NMMFGOPEN WITH NMMFGOPEN + (lnToCan * lnSign)  ,;
            CANCELED  WITH CANCELED  + (lnToCan * (-lnSign))
    UNLOCK
  ENDIF
ENDIF

*-- Restore current ENVIRONMENT settings
SELECT(lnAlias)

*-  End OF lpDelScr.
*:***************************************************************************
*: Name       : lfStDelBar
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Change the prompt of the "Delete" bar number in 
*:              the "File" pad in the system menu to be...
*:              "Cancel/Uncancel"
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfStDelBar()
*:*************************************************************
*:
FUNCTION lfStDelBar

lcDelMesag = IIF(lcPromp = lcCancel, "cancel", "uncancel") 
lcMenProm  = PROPER(lcDelMesag) + " Material Manufacturing Order "
DEFINE BAR lnDelBarNo OF (lcDelPadNm) PROMPT (lcMenProm)
ON SELECTION BAR lnDelBarNo OF (lcDelPadNm) DO gfCPDelete
SET SKIP OF  BAR lnDelBarNo OF (lcDelPadNm) lcCanStat  = "DISABLE"
SHOW GET pbDlt,1 PROMPT lcPromp
SHOW GET pbDlt &lcCanStat

*-- End Of lfStDelBar.
*:*************************************************************
*: Name       : lfGtDelBar
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : To specify the "Delete" bar number in the "File" 
*:              pad in the system menu.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : The "Delete" bar number in the "File" system menu.
*:*************************************************************
*: Example    : lnDelBarNo = lfGtDelBar()
*:*************************************************************
*:
FUNCTION lfGtDelBar

*-- Open the system menu file.
= gfOpenFile(gcSysHome+"SYCMenu","Pross_ID","SH")

IF SEEK("GFCPDELETE", "SYCMenu")
  lcDelPadNm = UPPER(ALLTRIM(SYCMenu.cMstr_Nam))
  lnRetVal   = VAL(SYCMenu.cBar_Pos)
ELSE
  *-- The default values.
  lcDelPadNm = "P03PU03"
  lnRetVal   = 10
ENDIF
= gfCloseFile("SYCMenu")

RETURN (lnRetVal)
*-- End OF lfGtDelBar.
*:*************************************************************
*: Name       : lfRestVar
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Reset the needed variables when finding any previous
*:              incomplete session.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfRestVar()
*:*************************************************************
*:
FUNCTION lfRestVar

laScrMode  = .F.
laScrMode[ATC(lcScrMode,"SVEA")] = .T.
llCUpdate  = .T.
llContinue = .T.
lcSession  = UnCmSess.cSession
= IIF(laScrMode[3], SEEK(laData[1], "MMFGORDH"), .T.)
llRefPage2 = .T.
= lfChngFolder(lnActFolder)

*-- End of lfRestVar.
*:*************************************************************
*: Name       : lfActPad
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : To create the "Option" menu pad.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfActPad()
*:*************************************************************
*:
FUNCTION lfActPad

DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
SET SKIP OF PAD _INQUIRY OF _MSYSMENU !EOF(lcTmpMMOLn)
ON PAD _INQUIRY OF _MSYSMENU ACTIVATE POPUP _INQURYPOP
DEFINE POPUP _INQURYPOP MARGIN SHADOW
DEFINE BAR 1  OF _INQURYPOP PROMPT '\<Append lines from Mfg Order' SKIP FOR !(laScrMode[3] .OR. laScrMode[4])
ON SELECTION POPUP _INQURYPOP DO lpvInquiry


*-- End OF lfActPad.
*:*************************************************************
*: Name       : lpvInquiry
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Bulid a new menu pad [Options]
*:*************************************************************
*: Calls     : 
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  =lpvInquiry()
*:*************************************************************
*:
FUNCTION lpvInquiry

DO CASE
  CASE BAR() = 1    
    *-- Append lines from the mfg Order.
  	DO lpapendmfg
ENDCASE

*-- End Of lpvInquiry.
*:*************************************************************
*: Name       : lpapendmfg
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Append line from the mfg Order .
*:**********x***************************************************
*: Calls     : 
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  Do lpapendmfg
*:*************************************************************
*:
PROCEDURE lpapendmfg
PRIVATE lnOldAlis,lcOrdNo


lnOldAlis  = SELECT (0)

*- Open Header file.
IF  USED('MMFGORDH')
  SELECT MMFGORDH
ELSE
  llOpMfgHdr = gfOpenFile(gcDataDir+"MMFGORDH","MMFGORDH","SH")
ENDIF

*-- this check if we found any data match the ladata[2].
IF !EMPTY(ladata[2])
  lcOldOrder = ORDER()
  SET ORDER TO Mmfgordd
  = SEEK(ladata[2])
  LOCATE REST WHILE cfabric+cmfgordno = ladata[2] FOR status $ 'CO'
  
  IF !FOUND()
    =gfDialog ("I","No records to display.")
    SET ORDER TO &lcOldOrder
    SELECT (lnOldAlis)
    RETURN
  ENDIF
  SET ORDER TO &lcOldOrder  
ENDIF


lcOrdNo = '?'
= gfMFGOrdBr(@lcOrdNo,ladata[2],"Status $ 'CO'")

*--- Variable holed Appened Warhouse from header file.
=SEEK(lcOrdNo,'MMFGORDH')
lcApndWare = MMFGORDH.cwarecode
lcGl_link = MMFGORDH.Link_Code
=SEEK("05"+lcGl_link, "GL_Link")
lcLinkDesc = GL_Link.LinkDesc




IF !EMPTY(lcOrdNo)
  IF  !USED('MMFGORDD')
    llOpMfgDD = gfOpenFile(gcDataDir+"MMFGORDD","MMFGORDD","SH")
  ENDIF
  = lfSelPoLin()
  SELECT (lcPOLine)
  LOCATE
  IF EOF()
    *-- not found any receiving lines.
    *- Message Text   :- No receiving lines for Mfg# XXX.
    *- Message No.    :- 36182.
    *- Buttom Message :- Ok.
    *- Buttom Number  :- 00000.
    =gfModalGen('INM36182B36000','DIALOG','Mfg# : '+ lcOrdNo)
    SELECT (lnOldAlis)
    RETURN
  ELSE
    *- Message Text   :- Copy all the receiving lines of Mfg# XXX.
    *- Message No.    :- 36183.
    *- Buttom Message :- Ok.
    *- Buttom Number  :- 00001.
    IF gfModalGen('INM36183B36001','DIALOG','Mfg# : '+ lcOrdNo) = 2
      llOkSelect = .F.     
      PUSH KEY
      ON KEY
      DO (gcScrDir+gcWinAppl+'\MaRwsele.SPX')
      POP KEY
      IF llOkSelect
        = lfApplin ()
        llOkSelect = .F.
      ENDIF
    ELSE
      *-- ADD ALLRECOREDS.
      REPLACE ALL cStatus WITH "»"
      =lfApplin()
    ENDIF
  ENDIF
ENDIF

SELECT (lnOldAlis)

*-- End OF lpapendmfg.
*:*************************************************************
*: Name       : lfDeActPad
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : To release the "Options" menu pad.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfDeActPad()
*:*************************************************************
*:
FUNCTION lfDeActPad

RELEASE PAD _INQUIRY OF _MSYSMENU

*-- End of lfDeActPad.
*:*************************************************************
*: Name       : lfApplin
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : To append lines.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfApplin()
*:*************************************************************
*:
FUNCTION lfApplin
PRIVATE lnAlias , lcOldOrder , llFrstTime


lnAlias = SELECT()

llFrstTime = .T.
SELECT (lcTmpMMOLn)
lcOldOrder = ORDER()
SET ORDER TO cFabric

SELECT(lcPOLine)
LOCATE

SCAN FOR !EMPTY(cStatus)
  *-- check if we add this recored before that.
  IF SEEK(&lcPOLine..cFABRIC+&lcPOLine..COLOR,lcTmpMMOLn) 
    IF &lcTmpMMOLn..nmfgtotqty <= 0 .OR. (&lcTmpMMOLn..nmfgtotqty > 0 .AND. ;
      gfModalGen('INM36020B36001','DIALOG',&lcPOLine..cFABRIC+ '|'+&lcPOLine..COLOR) = 1)
      SELECT(lcTmpMmoLn)
      REPLACE &lcTmpMmoLn..nmfgtotqty WITH &lcTmpMmoLn..nmfgtotqty + &lcPOLine..nmfgtotqty,;
              &lcTmpMmoLn..cStatus    WITH IIF(cStatus = "A" OR EMPTY(cStatus), "A", "M")
              
      *-- Update the Budget Field
      laData[7]  = laData[7]  + &lcPOLine..nmfgtotqty
      laData[11] = laData[11] + &lcPOLine..nmfgtotqty

      *-- Update the cost Fields on the screen.
      * -- [12,13,14,15]
      laData[12] = laData[12] + &lcPOLine..nmfgtotqty * &lcPOLine..nLan_Cost1
      laData[13] = laData[13] + &lcPOLine..nmfgtotqty * &lcPOLine..nLan_Cost2
      laData[14] = laData[14] + &lcPOLine..nmfgtotqty * &lcPOLine..nLan_Cost3
      laData[15] = laData[15] + &lcPOLine..nmfgtotqty * &lcPOLine..nLan_Cost4

    ENDIF
  ELSE
    IF llFrstTime .AND. lascrMode[4] .AND. EMPTY(ladata[2])
      *-- This function to update the screen with ladata[2]
      = lFildata_2 ()
      llFrstTime = .F.
    ENDIF
    
    SELECT(lcTmpMmoLn)
    lnNumOfDets = lnNumOfDets + 1
    APPEND BLANK
    REPLACE cMfgRwNum  WITH laData[1]            ,;
            cFabric    WITH &lcPOLine..cFabric   ,;
            Color      WITH &lcPOLine..Color     ,;
            Trancd     WITH '1'                  ,;
            nmfgtotqty WITH &lcPOLine..nmfgtotqty,;
            cStatus    WITH "A"                  ,;
            cWareCode  WITH ladata[4]            ,;
            LineNo     WITH lnNumOfDets          ,;
            dyelot     WITH &lcPOLine..dyelot    ,;
            PATTERN    WITH &lcPOLine..Pattern
            

    *-- update the Estimated cost.
    REPLACE nCost1   WITH &lcPOLine..nLan_Cost1,;
            nCost2   WITH &lcPOLine..nLan_Cost2,;
            nCost3   WITH &lcPOLine..nLan_Cost3,;
            nCost4   WITH &lcPOLine..nLan_Cost4

      *-- Update the Budget Field
      laData[7]  = laData[7]  + &lcPOLine..nmfgtotqty
      laData[11] = laData[11] + &lcPOLine..nmfgtotqty
      
      *-- Update the cost Fields on the screen.
      * -- [12,13,14,15]
      laData[12] = laData[12] + &lcPOLine..nmfgtotqty * &lcPOLine..nLan_Cost1
      laData[13] = laData[13] + &lcPOLine..nmfgtotqty * &lcPOLine..nLan_Cost2
      laData[14] = laData[14] + &lcPOLine..nmfgtotqty * &lcPOLine..nLan_Cost3
      laData[15] = laData[15] + &lcPOLine..nmfgtotqty * &lcPOLine..nLan_Cost4
  ENDIF
ENDSCAN


SELECT (lcTmpMMOLn)
SET ORDER TO &lcOldOrder
LOCATE

=lfRefresh()
SELECT(lnAlias)

*-- End Of lfApplin.
*:*************************************************************
*: Name       : lfSelPoLin
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : get all the recieved lines for the selected P/O
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfSelPoLin()
*:*************************************************************
*:
FUNCTION lfSelPoLin

lnAlias = SELECT()
SELECT *,'' AS cMarker,'' AS cStatus FROM mmfgordd;
  WHERE cmfgordno = lcOrdNo AND ;
        (Trancd = '2') ;
 INTO DBF (gcWorkDir+lcPOLine)
SELECT(lcPOLine)
INDEX ON cmfgordno+cfabric+color+dyelot+trancd TAG lcPOLine
SELECT (lnAlias)

*-- End of lfSelPoLin.
*:*************************************************************
*: Name       : lfBrPOLin
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : browse all the recieved lines for the selected P/O
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfBrPOLin()
*:*************************************************************
*:
FUNCTION lfBrPOLin
lcRec_Ttl = "Receiving lines For Mfg Order # :" + cmfgordno


lnAlias = SELECT()
lcBrowFlds = [cMarker :H=' ' :R :1 :W=.F.,]+;
             [cStatus :H='',]+;
             [cFabric  :10:H='Item':R,]+;
             [Color   :8:H='Color':R,]+;
             [cWareCode:12:H='Warehouse':R,]+;
             [drecvdate:10:H='Date':R,]+;
             IIF(llDyelot, [Dyelot:10:H='Dyelot':R,], [Reference:12:H='Reference':R,])+;
             [nmfgtotqty:11:H='Quantity':P='9999999.999':R] 
SELECT(lcPOLine)
GOTO TOP
lnMarker = RECNO()
BROWSE FIELDS &lcBrowFlds;
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwPOl();
       TITLE lcRec_Ttl    ;
       WINDOW MARWSEL1 IN WINDOW MARWSELE
ACTIVATE WINDOW (lcRec_Ttl)
SELECT (lnAlias)

*-- End Of lfBrPOLin.
*:*************************************************************
*: Name       : lfwPOl
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : when for the recievung lines browse
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfwPOl()
*:*************************************************************
*:
FUNCTION lfwPOl
PRIVATE lnNewRec 

lnAlias = SELECT()
SELECT (lcPOLine)
lnNewRec = RECNO()
REPLACE ALL cMarker WITH lcUnMark
GO IIF(lnNewRec < RECCOUNT(lcPOLine),lnNewRec ,RECCOUNT(lcPOLine))
REPLACE cMarker WITH lcMark
SHOW WINDOW (lcRec_Ttl) REFRESH
SELECT (lnAlias)

*-- End OF lfwPOl.
*:*************************************************************
*: Name       : lfvSelec
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : select one or all recieved lines
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfvSelec()
*:*************************************************************
*:
FUNCTION lfvSelec
PARAMETER llAll ,lnRecno

lnAlias = SELECT()
SELECT (lcPOLine)
lnRecno = RECNO()
IF llAll
  REPLACE ALL cStatus WITH "»"
ELSE
  REPLACE cStatus WITH "»"
ENDIF
SHOW WINDOW (lcRec_Ttl) REFRESH SAME
SHOW GET pbUSele  ENABLE 
SHOW GET pbUSeleA ENABLE 
SHOW GET pbSele   DISABLE 
SHOW GET pbSeleA  DISABLE 
GOTO TOP
SCAN
  IF EMPTY(cStatus)
    SHOW GET pbSele  ENABLE   
    SHOW GET pbSeleA ENABLE       
    EXIT
  ENDIF
ENDSCAN
GOTO lnRecno
=lfwPOl()
SELECT (lnAlias)
*-- End OF lfvSelec
*:*************************************************************
*: Name       : lfvUSelec
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : unselect line or all recieving lines 
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfvUSelec()
*:*************************************************************
*:
FUNCTION lfvUSelec
PARAMETER llAll

lnAlias = SELECT()
SELECT (lcPOLine)
IF llAll
  REPLACE ALL cStatus WITH SPACE(01)
ELSE
  REPLACE cStatus WITH SPACE(01)
ENDIF
SHOW WINDOW (lcRec_Ttl) REFRESH
SHOW GET pbSele  ENABLE 
SHOW GET pbSeleA ENABLE 
SHOW GET pbUSele  DISABLE 
SHOW GET pbUSeleA DISABLE 
SCAN
  IF !EMPTY(cStatus)
    SHOW GET pbUSele  ENABLE 
    SHOW GET pbUSeleA ENABLE 
    EXIT
  ENDIF
ENDSCAN
=lfwPOl()
SELECT (lnAlias)
*-- End of lfvUSelec.
*:*************************************************************
*: Name       : lfAddWare
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Add the defaulte warehouse by defaut.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfAddWare()
*:*************************************************************
*:
FUNCTION lfAddWare
PRIVATE lnOldAlis

IF EMPTY(laData[4])
  lnOldAlis = SELECT(0)
  SELECT WareHous
  LOCATE
  *-- if append from mmfordh Get the data[4] for appened warehouse.
  IF !EMPTY(lcApndWare)
    = SEEK(lcApndWare)
  ENDIF
  
  STORE cwarecode TO laData[4],m.cwarecode
  lcWareDesc  = WAREHOUS.cDesc
  ladata[28] = gfGetAdr('WareHous','','','',1,'')
  lcShipTo1 = ladata[28] +IIF(LEN(ladata[28])<30,SPACE(30-LEN(ladata[28])),'')
  FOR lnCounter = 1 TO 5
    L=ALLTRIM(STR(lnCounter))
    lcTempVr=''
    FOR lnCount = 1 TO ALEN(laAddress,1)
      IF laAddress[lnCount,1] = lnCounter
        lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
        lcTempVr = lcTempVr+IIF(EMPTY(lcTempVr) .OR. RIGHT(lcTempVr,1) = ',' ,'',', ') + lcAddPart
      ENDIF
    ENDFOR
    ladata[27+lnCounter] =lcTempVr+IIF(LEN(lcTempVr)<30,SPACE(30-LEN(lcTempVr)),'')
    SHOW GET ladata[27+lnCounter] ENABLE
  ENDFOR
  SELECT (lnOldAlis)
  SHOW GET ladata[4]
ENDIF

*-- End OF lfAddWare
*:*************************************************************
*: Name       : lFildata_2
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Update the ladata[2] with the default data.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lFildata_2()
*:*************************************************************
*:
FUNCTION lFildata_2
PRIVATE lcOldAlias

*-- lfvData_2

lcOldAlias = SELECT (0)
ladata[2] = &lcPOLine..cFabric
SELECT FABRIC
= SEEK(ladata[2])

lcFabDesc  = FABRIC.Desc
m.DyeYN    = FABRIC.cDye_Flg


*-- Add the Warehouse By Default first one.
= lfAddWare ()
*-- Default entered date with the system date
laData[ 5] = gdSysDate
*-- Default complete date with the system date + 90 days
laData[ 6] = laData[ 5] + 90
= lfvEntered()
= lfvComplet()

*-- we have to check the full key case there is no dyelots and no multi W/H
=lfChkLnKey('C')

SHOW GET pbMONote    ENABLE
SHOW GET ibFolder[3] ENABLE
SHOW GET laData[2]   DISABLE
SHOW GET ibFabBrow   DISABLE
SHOW GET laData[ 5]  ENABLE
SHOW GET laData[ 6]  ENABLE


=lfRefresh(lcCTChWin2)

=gfUpdate()

=lfUpdVars()

SELECT (lcOldAlias)

*-- End OF lFildata_2
*:*************************************************************
*: Name       : lfvSeleOk
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Clear read the Selected screen.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfvSeleOk()
*:*************************************************************
*:
FUNCTION lfvSeleOk

llOkSelect = .T.
CLEAR READ

*-- End OF lfvSeleOk.
*:*************************************************************
*: Name       : lfwLink
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Save The Old Link Code.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfwLink()
*:*************************************************************
*:
FUNCTION lfwLink

lcOldLink = laData[34]

*-- End OF lfwLink
*:*************************************************************
*: Name       : lfvLink
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : get the link code.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfvLink()
*:*************************************************************
*:
FUNCTION lfvLink

PRIVATE lnAlias

lnAlias = SELECT()
IF llBrowse .OR. (!EMPTY(laData[34]) AND !SEEK("05"+laData[34], "GL_Link"))
  IF llBrowse
    lcLink = '?'
  ELSE
    lcLink = laData[34]
  ENDIF
  llBrowse = .F.

  IF !gfGLBrowse('05',@lcLink ,@lcLinkDesc) 
    laData[34] = lcOldLink
  ELSE
    laData[34] = lcLink 
  ENDIF
ENDIF
IF EMPTY(laData[34])
  laData[34] = 'DEFDEF'
ENDIF
SHOW GET laData[34]
SELECT(lnAlias)

*-- End Of lfvLink
*:*************************************************************
*: Name       : lfChkIsQty
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Save The Old Link Code.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfChkIsQty()
*:*************************************************************
*:
FUNCTION lfChkIsQty
PRIVATE llParam , lnAlias

lnAlias = SELECT()
llParam = .F.

SELECT marwlin
*-- cmfgrwnum+cfabric+color+dyelot+trancd
=SEEK(laData[1])
LOCATE REST WHILE cmfgrwnum+cfabric+color+dyelot+trancd = ;
laData[1] FOR trancd = '2'

IF FOUND()
 llParam = .T.
ENDIF

SELECT(lnAlias)
RETURN llParam
*-- End OF lfChkIsQty.
*:*************************************************************