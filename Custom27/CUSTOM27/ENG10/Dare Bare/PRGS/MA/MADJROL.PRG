*:****************************************************************
*: Program file  : MADJROL.PRG
*: Program desc. : Material Inventory Adjustments by Roll. 
*:               : For Dare to Bare Ltd
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Material      -  [MA]
*: Developer     : ABDOU ELGENDI -  [ABD]
*: Date          : 09/01/2003
*: Tracking Job Number : C200581,1
*:****************************************************************
*: Calls         : G.FUNCTIONS: gfSetup 
*:               : FUNCTIONS  : lfTrapKeys , lpEsc      , lfClearTrap
*:               :            : lfvWare    , lfvFabric  , lfvColor
*:               :            : lfvRemove  , lfActBrow  , lfChMarker
*:               :            : lfvNew     , lfTrapKD   , lfInit
*:               :            : lfTrapKD   , lfvCancel  , lfvRolls
*:               :            : lfvlndAdj  , lfvpbUpdate,lfVldDate
*:               :            : lfvModify  . 
*:               -----------------------------------------------
*:               : PROCEDURE  : lpShiftTab , lpTab  , lpShow 
*:               :            : lpClsScr   , lpEscD , lpRepTemp
*:               :            : lpSavScr   .
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C200581,1
*:****************************************************************
*:Modifications  :
*:B037180,1 ABD 10/08/2003 Fix few mofification to the browse by rolls
*:B037180,1                And get the correct cost from the orginal record.
*:****************************************************************
*:
*-- lcWindTitle :- Window Titel.
*-- lcTmpAdj    :- Variable hold the detail line file for rolls.
*-- lcTmpRolls  :- Variable hold the temp name for rolls file.
*-- lcTemLoc    :- Variable hold the  werahouse file.

*-- Define needed Variables.
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DECLARE ladata[01],laKeyField[1,4],laAddress[6,3],laDefProc[10],;
        laVariables[1] , laAdjCode[1] , laCodInfo [1,10]

*-- Get the old trap of Esc Key
lcOldEscTrap = ON('Key','Esc')

*-- Allow the program to have its local Save and local Close
laDefProc[09]  = .F.
laDefProc[10] = .F.

STORE '' TO laData,laKeyField,laAddress,lcWinCh1,lcWinCh2,lcWinCh3,lcWinCh4,lcModal,;
            lctmpRolls ,lcFromWare, lcFrmDesc , lcGlFYear ,lcGlPeriod , lcTmpAdj ,;
            lctmpGlDis,lcAdjReason,lcAdjAcct,laAdjCode,laCodInfo,lcLinkCode

STORE .F. TO llBrowse
*-- Browse title of the mainbrowse of items details (screen Mat400_2)
lcBrowTtl    = "Transaction's lines"

lcFabric     = SPACE(07)
lcOldFab     = SPACE(07) 
lcColor      = SPACE(06)
lcDesc       = SPACE(20)
lcReason     = SPACE(25)
STORE {} TO ldPost
ldPost = gdSysDate
	
*-- End Define needed Variables.

IF !gfSetup()
  RETURN
ENDIF  

lcBaseFile = ''

*-- Convetrting lcChhoice to numeric and 
*-- then define lctype as character
lcType     = 'A'

*-- To allow the program to enter the lpShow procedure at the calling time
llNoShow   = .F.


DIMENSION laSetups[6,2]
laSetUps[1,1]  = 'M_WareHouse'
laSetUps[2,1]  = 'M_MATDYE'
laSetUps[3,1]  = 'M_LINK_GL'
laSetUps[4,1]  = 'M_WARELOC'
laSetUps[5,1]  = 'M_TrkRolls'
laSetUps[6,1]  = 'M_MatCstMt'
= gfGetMemVar(@laSetups)
llWareHous = laSetUps[1,2] = 'Y'
llDyelot   = laSetUps[2,2] = 'Y'
llGlLink   = laSetUps[3,2] = 'Y' 
llWareLoc  = laSetUps[4,2] = 'Y'
llTrkRolls = laSetUps[5,2] = 'Y'
lcMtCstMth = laSetUps[6,2]

lcWinTitle = 'Material Inventory Adjustments by Roll'
*-- Title of the screen Mainvct4
lcLineT      = SPACE(00)

*-- Initialize the variable of screen MADJROLL
lcRoll     = SPACE(20)
ldCurrDate   = {}
lnAdj        = 000
lnOld        = 000
lnNew        = 000
lnToStk      = 000
lnOldCost    = 000
lnNewRec     = 000
lcDesc       = SPACE(20)
lcReason     = SPACE(25)
lcrsession   = SPACE(06)

*-- For marking and unmarking in the browse
lcMark     = '>'
lcUnMark   = SPACE(1)


*-- Initialize the contets of push botttons
lcCloseP   = gcBmpHome + "CLS.BMP"
lcCanclP   = gcBmpHome + "CAN.BMP"
lcNewP     = gcBmpHome + "NEW.BMP"
lcRemveP   = gcBmpHome + "REM.BMP"
lcSelecP   = gcBmpHome + "SEL1.BMP"
lcUnSelP   = gcBmpHome + "UNSEL.BMP"
lcExtKey   = gcBmpHome + "ExtKey.BMP"

*-- Initialize the contents of pbs 
*-- to be New and remove in case of (+)ve adj. and llTrkRolls
lcRollNP     = lcNewP
lcRollRP     = lcRemveP
lnColorWid   = (06)

*-- Initialize the contents of pbs to be close untill the key is completed
lcPromp      = lcCloseP
lcDetP       = lcCloseP

IF !WEXIST(gcBaseWind)

  *-- In case the company is not keep trak rolls don't run the program.
  IF !llTrkRolls
    *-- Text message :- The Material module is not set to keep track  
    *-- Text message :- of material rolls. Can not proceed.
    *-- Message No.  :- 36174
    *-- Button Text  :- Ok
    *-- Button No.   :- 42001
    =gfModalGen('TRM36174B42001','DIALOG')
    glQuitting = .T.
    RETURN
  ENDIF

  *-- In case of Adj. ,check if there is any Adj. Reason code already defined ,
  *-- if not give the user a message and return
  lnAlias = SELECT()
  SELECT CODES
  lcCodTag=TAG()
  SET ORDER TO TAG cCode_no

  *-- According to removing comp_id from codes file (Start)
  IF !SEEK('N'+'CADJREASON','CODES')
    *-- According to removing comp_id from codes file (End)
    *--You have to edit the Adjustment reasons codes first, Cannot proceed.
    =gfModalGen('TRM42111B42001','DIALOG')
      
    SET ORDER TO TAG &lcCodTag
    SELECT(lnAlias)
    *-- Terminate the program if there is no adjust. reasons codes
    *-- in codes file
    glQuitting = .T.
    RETURN
    *-- Ending the program (END)
  ENDIF  
  SET ORDER TO TAG &lcCodTag
  SELECT(lnAlias)


  *-- Define the child windows 
  lcWinCh1 = gfTempName()
  lcWinCh2 = gfTempName()
  lcWinCh3 = gfTempName()
  lcWinCh4 = gfTempName()

  *-- If the system is setup to use the rolls.
  IF llTrkRolls .OR. lcMtCstMth $ "LFI"
    SELECT ROLLS
    lctmpRolls = gfTempName()
  ENDIF

  IF llGlLink
    *-- OPEN GLDIST FILE TO CALL GENERAL LEDGER DISTRIBUTION PROCEDURE 
    SELECT GLDIST
    *-- OPEN A TEMP FILE TO BE USED IN CALLING 'GLDIST' PROCEDURE.
    lctmpGlDis = gfTempName()
    COPY STRUCTURE TO &gcWorkDir.&lctmpGlDis
    SELECT 0
    USE (gcWorkDir+lctmpGlDis) EXCLUSIVE
  ENDIF  


  *-- Temp. File to have the transaction lines 
  lcTmpAdj   = gfTempName()

  *-- Openning The Files Used And Creating The Temp. Cursor 
  *-- which contains details of each item in the transaction
  SELECT FINVTADJ
   = AFIELDS(laFileStru)
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cMarker'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 1
  laFileStru[lnNewFld,4] = 0
  
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'Desc'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 20
  laFileStru[lnNewFld,4] = 0

  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'NewQty'
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 12
  laFileStru[lnNewFld,4] = 3

  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'TotStk'
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 8
  laFileStru[lnNewFld,4] = 0

  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'UnitCost'
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 9
  laFileStru[lnNewFld,4] = 3

  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cRSession'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 6
  laFileStru[lnNewFld,4] = 0

  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cISession'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 6
  laFileStru[lnNewFld,4] = 0
  
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'nOldCost'
  laFileStru[lnNewFld,2] = 'N'
  laFileStru[lnNewFld,3] = 9
  laFileStru[lnNewFld,4] = 3

  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cAdjReason'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 6
  laFileStru[lnNewFld,4] = 0

  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cAdjAcct'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 24
  laFileStru[lnNewFld,4] = 0


  *-- Add field for Material inventory control account (Start)
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cMIcAcct'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 24
  laFileStru[lnNewFld,4] = 0

  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'crollId'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 20
  laFileStru[lnNewFld,4] = 0
  
  
  *-- Add field for Material inventory control account (End)
  DECLARE laIndex[2,2]
  laIndex[1,1] = 'FABRIC+COLOR+DYELOT'
  laIndex[1,2] = 'lcTmpAdj'
  laIndex[2,1] = 'FABRIC+COLOR+cfromWare+cRollId'
  laIndex[2,2] = 'lcTmpAdjs'
  =gfCrtTmp(lcTmpAdj,@laFileStru,@laIndex)

  *-- Set relations between the lcTmpAdj 
  *-- file with Fabric and Fabdye files

  SET RELATION TO Fabric+Color                     INTO Fabric
  SET RELATION TO Fabric+Color+cFromWare+Dyelot    INTO FabDye

  *-- Initialize the array used for the Adj. Reason Code
  laCodInfo[1,01] = "CADJREASON"
  laCodInfo[1,02] = "laAdjCode"
  laCodInfo[1,03] = "lnAdjCode"
  laCodInfo[1,04] = ""
  laCodInfo[1,05] = .F.
  laCodInfo[1,06] = .F.
  laCodInfo[1,07] = "&lcTmpAdj"
  laCodInfo[1,08] = "&lcTmpAdj"
  laCodInfo[1,09] = "lcFabric+lcColor+lcDyelot"
  laCodInfo[1,10] = "cAdjReason"
  =gfwCodePop(@laCodInfo, "CADJREASON" , "D")
  lnAdjCode  = 1
  lcAdjReason= laAdjCode[lnAdjCode,2]
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  =gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
ENDIF


*-- Calling of the main screen MAINVCT
PUSH KEY
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowTtl)
DO (gcScrDir+gcWinAppl+'\MADJROL.SPX')
POP KEY

*-- Delete all the temp. files when closing the program
IF glQuitting
  USE IN (lcTmpAdj)
  ERASE (gcWorkDir+lcTmpAdj+'.DBF')
  ERASE (gcWorkDir+lcTmpAdj+'.CDX')
  ERASE (gcWorkDir+lcTmpAdj+'.FPT')
  
  IF llTrkRolls .OR. lcMtCstMth $ "LFI"
    IF USED (lcTmpRolls)
      USE IN (lcTmpRolls)
    ENDIF  
    ERASE (gcWorkDir+lcTmpRolls+'.DBF')
    ERASE (gcWorkDir+lcTmpRolls+'.CDX') 
    ERASE (gcWorkDir+lcTmpRolls+'.FPT') 
  ENDIF
  
  IF llGlLink
    USE IN (lctmpGlDis)
    ERASE (gcWorkDir+lctmpGlDis+".DBF")
  ENDIF  
ENDIF

*-- End OF Code.
*:*************************************************************
*: Name      : lfTrapKeys
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : Funtion to do the trapping between the screens.
*:*************************************************************
*: Calls     : 
*:             Procedures : lpTab , lpShiftTab, lpEsc
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfTrapKeys ()
*:*************************************************************
*:
FUNCTION lfTrapKeys

ON KEY LABEL CTRL+W     lnDummy = 1
ON KEY LABEL CTRL+Q     lnDummy = 1
ON KEY LABEL CTRL+HOME  lnDummy = 1
ON KEY LABEL CTRL+END   lnDummy = 1
ON KEY LABEL TAB        DO lpTab
ON KEY LABEL BACKTAB    DO lpShiftTab
ON KEY LABEL ESC        DO lpEsc
ON KEY LABEL CTRL+ENTER DO lpEsc

*-- End OF lfTrapKeys
*:*************************************************************
*: Name      : lpTab
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : Funtion to trapping of the back tab between screens.
*:*************************************************************
*: Calls     : 
*:             Procedures : lpTab , lpShiftTab, lpEsc
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : DO lpTab
*:*************************************************************
*:
PROCEDURE lpTab

DO CASE
  CASE WONTOP('lcWinCh1') .AND. _CUROBJ = OBJNUM(lcColor)
     ACTIVATE WINDOW (lcBrowTtl)
  CASE WONTOP(lcBrowTtl) 
    ACTIVATE WINDOW lcWinCh3
    _CUROBJ = OBJNUM(pbNew)
  CASE WONTOP('lcWinCh3') .AND. (_CUROBJ = OBJNUM(pbModify) OR ;
             (_CUROBJ = OBJNUM(pbNew) AND EOF(lcTmpAdj)))  
    ACTIVATE WINDOW ('gwcContrl1')
  CASE WONTOP('gwcContrl1') .AND. _CUROBJ = OBJNUM(pbCls)
    IF laScrMode[1]
      ACTIVATE WINDOW lcWinCh1
      _CUROBJ = OBJNUM(lcFromWare)
    ELSE
      ACTIVATE WINDOW (lcBrowTtl)
    ENDIF
  OTHERWISE
    _CUROBJ = _CUROBJ + 1
ENDCASE
ON KEY LABEL TAB DO lpTab      

*-- End OF lpTab
*:*************************************************************
*: Name      : lpEsc
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : Funtion to trapping of the back tab between screens.
*:*************************************************************
*: Calls     : 
*:             Procedures : lpTab , lpShiftTab, lpEsc
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : DO lpEsc
*:*************************************************************
*:
PROCEDURE  lpEsc

ACTIVATE WINDOW ('gwcContrl1')
_CUROBJ = OBJNUM(pbCls)
KEYBOARD CHR(13)

*-- End OF lpEsc
*:*************************************************************
*: Name      : lpShiftTab
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : Funtion to trapping of the back tab between screens.
*:*************************************************************
*: Calls     : 
*:             Procedures : lpTab , lpShiftTab, lpEsc
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : DO lpShiftTab
*:*************************************************************
*:
PROCEDURE lpShiftTab

ON KEY LABEL BACKTAB lnDummy = 1
DO CASE
  CASE WONTOP('lcWinCh1') .AND. _CUROBJ = OBJNUM(lcFromWare)
    ACTIVATE WINDOW ('gwcContrl1')
   _CUROBJ = OBJNUM(pbCls)

  CASE WONTOP(lcBrowTtl)
    IF laScrMode[1]
      ACTIVATE WINDOW lcWinCh1
      _CUROBJ = OBJNUM(lcColor)
    ELSE  
      ACTIVATE WINDOW ('gwcContrl1')
     _CUROBJ = OBJNUM(pbCls)
    ENDIF
  CASE WONTOP('lcWinCh3') .AND. _CUROBJ = OBJNUM(pbNew)
    ACTIVATE WINDOW (lcBrowTtl) 

  CASE WONTOP('gwcContrl1') .AND. _CUROBJ = OBJNUM(pbcptask)
    IF laScrMode[4]
      IF EOF(lcTmpAdj)
        _CUROBJ = OBJNUM(pbNew)
      ELSE
        _CUROBJ = OBJNUM(pbModify)
      ENDIF
    ELSE
      ACTIVATE WINDOW (lcBrowTtl) 
    ENDIF  
  OTHERWISE
    _CUROBJ = _CUROBJ - 1
ENDCASE
ON KEY LABEL BACKTAB DO lpShiftTab

RETURN
*-- End OF lpShiftTab
*:*************************************************************
*: Name      : lfClearTrap
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : to clear the trap after leaving the browse screen
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfClearTrap ()
*:*************************************************************
*:
FUNCTION lfClearTrap

IF WONTOP() <> lcBrowTtl
   ON KEY LABEL ESC &lcOldEscTrap
   ON KEY LABEL TAB
   ON KEY LABEL BACKTAB
ENDIF   

*-- End OF lfClearTrap
*:*************************************************************
*: Name      : lfvWare
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : To validate the warehouses codes.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : gfBrowWare
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvWare ()
*:*************************************************************
*:
FUNCTION lfvWare

PRIVATE lnAlias
lnAlias     = SELECT()

lcFromWare = IIF(llBrowse,PADR("?",6),lcFromWare)
SELECT WareHous
*-- To display the browse of WareHouses
IF !EMPTY(lcFromWare) AND !SEEK(lcFromWare)
  lcFromWare =  gfBrowWare(.T.)
  IF EMPTY(lcFromWare)
    _CUROBJ = OBJNUM(lcFromWare)
  ENDIF
ENDIF

*--To display the Desc. of the chosen Warehouse
lcFrmDesc = IIF(!EMPTY(lcFromWare), WareHous.cDesc, SPACE(0))

llNothing = lfRefresh()
SHOW GET lcFromWare
llBrowse = .F.

SELECT(lnAlias)
*-- End of lfvWare
*:*************************************************************
*: Name      : lpShow
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : Show function
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lpShow()
*:*************************************************************
*:
PROCEDURE lpShow

DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]
    STORE SPACE(06) TO lcFromWare , lcColor
    STORE SPACE(07) TO lcFabric
    STORE SPACE(00) TO lcDesc
    _CUROBJ   = OBJNUM(lcFromWare)
    SHOW GET pbBrws     DISABLE
    SHOW GET pbSav      DISABLE
    SHOW GET pbRemove   DISABLE
    SHOW GET pbModify   DISABLE

   =lfActBrow()
   =lfRefresh()
    
  
  ***--- V I E W   M O D E     ---***
  CASE laScrMode[2]

  ***--- E D I T   M O D E     ---***
  CASE laScrMode[3]

  ***--- A D D   M O D E       ---***
  CASE laScrMode[4]

    SHOW GET lcFromWare DISABLE
    SHOW GET ibFromBrow DISABLE
    SHOW GET ibToBrow   DISABLE
    SHOW GET pbNew      ENABLE
    SHOW GET pbBrws     DISABLE
    GOTO TOP IN (lcTmpAdj)
    IF !EOF()
      SHOW GET pbSav    ENABLE   
      SHOW GET pbRemove ENABLE   
      SHOW GET pbModify ENABLE   
    ELSE
      SHOW GET pbSav    DISABLE   
      SHOW GET pbRemove DISABLE
      SHOW GET pbModify DISABLE
    ENDIF
ENDCASE

*-- End OF lpShow
*:*************************************************************
*: Name      : lfvFabric
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : Validate the fabric code in the line entery screen.
*:*************************************************************
*: Calls     : 
*:             Procedures : gpWareFabs, FaBrow
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvFabric ()
*:*************************************************************
*:
FUNCTION lfvFabric
PRIVATE lnAlias

lnAlias = SELECT()
SELECT Fabric

lcFabric = IIF(llBrowse,PADR("?",6),lcFabric)
IF !EMPTY(lcFabric) AND !SEEK(lcFabric)
  DO FaBrow WITH lcFabric,'*'
  IF EMPTY(lcFabric)
    _CUROBJ = OBJNUM(lcFabric)
  ENDIF
    
ENDIF

*-- Initialize the Color and Dyelot variables
lcColor   = IIF(!EMPTY(lcFabric),IIF(!EMPTY(lcOldFab) AND lcFabric <> lcOldFab,SPACE(lnColorWid),lcColor),SPACE(lnColorWid))
llNothing = IIF(!EMPTY(lcColor),lfvColor(),.T.)
lcDyelot  = SPACE(10)
lcOldFab  = SPACE(07)
llBrowse = .F.
SELECT(lnAlias)

*-- End OF lfvFabric.
*:*************************************************************
*: Name      : lfvColor
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : Validate the entry of a fabric color
*:             code and display the browse if it is not found in 
*:             the Fabric file.
*:*************************************************************
*: Calls     : 
*:             Procedures : FaBrow
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvColor ()
*:*************************************************************
*:
FUNCTION lfvColor
PRIVATE lnAlias , lcOldOrder

lnAlias = SELECT()
SELECT Fabric
lcColor = IIF(llBrowse,PADR("?",6),lcColor)
*-- if the user types a wrong color
IF !EMPTY(lcColor) AND !SEEK(lcFabric+lcColor)

  *-- if empty(Fabric) browse all (fabrics + colors)
  *-- else browse the colors for the entered fabric.
  lcColor  = IIF(EMPTY(lcFabric),PADR(lcColor,lnColorWid),CHR(240))
  lcOldF   = lcFabric
  DO FaBrow WITH lcFabric,lcColor
  
  *-- Case Esc from the (Fabric + color) browse 
  *-- we return the old fabric value
  IF EMPTY(lcColor)
    lcFabric = lcOldF
    _CUROBJ = _CUROBJ
  ENDIF
ENDIF

*-- If empty either fabric or color 
IF !EMPTY(lcFabric) AND !EMPTY(lcColor)
  IF EMPTY(lcFromWare)
    SHOW GET lcFabric
    SHOW GET lcDesc  
    _CUROBJ = OBJNUM(lcFromWare)
  ELSE
    *-- Check if this fabric assign to this wearehouse or not into rolls file.
    SELECT Rolls
    lcOldOrder = Order()
    SET ORDER TO Rollitem

    llSeekFound = SEEK(lcFabric+lcColor+lcFromWare) 
    IF llSeekFound
      LOCATE FOR crollitem+color+cwarecode+dyelot+crollid+trancd+crsession =;
      lcFabric+lcColor+lcFromWare .AND. (nQtyBal > 0 AND TRANCD = "1")
    ENDIF
    SET ORDER TO &lcOldOrder
    IF llSeekFound .AND. FOUND()
      lcDesc  = Fabric.Desc
      SHOW GET lcFromWare DISABLE
      SHOW GET lcFabric   DISABLE
      SHOW GET lcColor    DISABLE
      laScrMode    = .F.
      laScrMode[4] = .T.
      SHOW GETS
    ELSE
      *-- Text Mesaage : No roll exists for
      lcMessage = 'Fabric-Color '+ lcFabric+ '-'+lcColor + ' Into wearhouse '+lcFromWare
      =gfModalGen('QRM36097B36000','ALERT',lcMessage)
      lcFromWare = SPACE(06)
      lcFabric   = SPACE(07)
      lcOldFab   = SPACE(07) 
      lcColor    = SPACE(06)
      lcDesc     = SPACE(20)
      _CUROBJ    = OBJNUM(lcFromWare)
      SHOW GET lcFabric
      SHOW GET lcColor 
    ENDIF
  ENDIF    
ENDIF
llBrowse = .F.
SELECT(lnAlias)

*-- End OF lfvColor
*:*************************************************************
*: Name      : lpClsScr
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : To do the validation of pbClose at the main
*:             screen and be sure that the user want to finish
*:             this program and Zap tmp.files and clear read.
*:*************************************************************
*: Calls     : 
*:             Procedures : .....
*:             Functions  : gfDialog
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : DO lpClsScr
*:*************************************************************
*:
PROCEDURE lpClsScr

*-- Zap the adjustment temp. file.
SELECT (lcTmpAdj)
DELETE ALL
    
*-- Zap the GL temp. file.
IF llGlLink
  = gfOpenFile (gcWorkDir+lctmpGlDis,"","EX")
  ZAP
ENDIF


SHOW GETS

*-- End of lpClsScr.
*:*************************************************************
*: Name      : lfvRemove
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : Remove an existing record in lcTmpAdj file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : lfActBrow.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvRemove ()
*:*************************************************************
*:
FUNCTION lfvRemove

lcChoice  = gfDialog('!','Are you sure you want to delete this line.',;
                     '\<Yes;\!\<No')
                     
*-- If the user wants to delete a record from the lcTmpAdj file                     
IF lcChoice = 1
  GO lnNewRec
  DELETE
  REPLACE ALL cMarker WITH lcUnMark
  GOTO TOP
  IF EOF()
    SHOW GET pbRemove DISABLE
    SHOW GET pbModify DISABLE
    SHOW GET pbSav    DISABLE
  ELSE
    REPLACE cMarker WITH lcMark
  ENDIF
  llNothing = lfActBrow()
  llNothing = lfChMarker()
ENDIF

*-- End OF lfvRemove
*:*************************************************************
*: Name      : lfActBrow
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : To define the header of the main browse for each
*: Purpose   : type and browse the fields of each item
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : lfActBrow.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvRemove ()
*:*************************************************************
*:
FUNCTION lfActBrow

SELECT (lcTmpAdj)

lcBrFields = "cMarker:H = ' ',crollId:H='Roll Id', cReason:H='Reason':25,"+;
             "Date :H='Date',OldQty:H='Existing', nMTotAdj:H='Adj(-)', NewQty:H='TotQty'"

BROWSE FIELDS &lcBrFields; 
       LOCK 0            ;   
       SAVE              ;
       NOWAIT            ;
       NOMENU            ;
       NODELETE          ;
       NOEDIT            ;
       WHEN lfChMarker() ;
       TITLE lcBrowTtl   ;
       WINDOW lcWinCh2  IN WINDOW (gcBaseWind)

*-- End OF lfActBrow
*:*************************************************************
*: Name      : lfChMarker
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : Set the marker in any browse
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : lfActBrow, lfRBrow, lfActBrwRo
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfChMarker()
*:*************************************************************
*:
FUNCTION lfChMarker

IF !EOF()
  lnNewRec = RECNO()
  REPLACE ALL cMarker WITH lcUnMark
  GO lnNewRec
  REPLACE cMarker WITH lcMark
ENDIF  

*-- End OF lfChMarker
*:*************************************************************
*: Name      : lfvNew
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : To call the screen to enter the details for
*: Purpose   : a line in the main browse.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvNew ()
*:*************************************************************
*:
FUNCTION lfvNew

*-- Screen title in case of adding a new transaction.
lcLineT    = " Transaction Line Entry Screen "

*-- Case of new record initialize all the varaibles
llModify   = .F.
lcRolStat  = 'ENABLE'
lcDatStat  = 'DISABLE'
=lfInit(.F.)
lnOldValue = 0

*-- Call the screen to enter the detail for each item
PUSH KEY
ON KEY
= lfTrapKD()
DO (gcScrDir+gcWinAppl+'\MADJROLL.SPX')
POP KEY

*-- End Of lfvNew
*:*************************************************************
*: Name      : lfTrapKD
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : To do the trapping for Mat400_ 4
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfTrapKD ()
*:*************************************************************
*:
FUNCTION lfTrapKD

ON KEY LABEL ESC        DO lpEscD
ON KEY LABEL CTRL+ENTER DO lpEscD


*-- End OF lfTrapKD
*:*************************************************************
*: Name      : lpEscD
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : Trapping the Escape button.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : DO lpEscD
*:*************************************************************
*:
PROCEDURE lpEscD

ACTIVATE WINDOW lcWinCh4
_CUROBJ = OBJNUM(pbCancel)
KEYBOARD CHR(13)

*-- End OF lpEscD
*:*************************************************************
*: Name      : lfInit
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : To initialize the variables used in the line 
*:           : entery screen either from the lcTmpAdj file or with 
*:           : new values.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : lfvNew, lfvColor, lfvDyelot
*:                        : lfvCancel, lfvlnOld, lfvpbUpdate
*:                        : lfvModify
*:*************************************************************
*:        Parameters : 
*:            llMode : To tell if we are going to initialize
*:                     the variables from the used temp file 
*:                     or with empty values.
*:                     .T. : Initialize from the temp file.
*:                     .F. : Initialize empty values.
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfInit()
*:*************************************************************
*:
FUNCTION lfInit
PARAMETERS llMode

*-- Initialize the variables from the file.
IF llMode
  lcRoll     = &lcTmpAdj..cRollId
  ldCurrDate = &lcTmpAdj..Date
  lcReason   = &lcTmpAdj..cReason
  lnOld      = &lcTmpAdj..OldQty
  lnNew      = &lcTmpAdj..NewQty
  lnToStk    = &lcTmpAdj..TotStk
  lnAdj      = &lcTmpAdj..nMTotAdj  

  SHOW GET lcRoll     &lcRolStat
  SHOW GET ibFabric   &lcRolStat
  SHOW GET lcReason   ENABLE
  SHOW GET ldCurrDate ENABLE
  SHOW GET lnNew      ENABLE
  SHOW GET lnAdj      ENABLE
  SHOW GET pbUpdate   ENABLE
ELSE


  *-- Initialize the variable of screen Mainvct4
  lcRoll     = SPACE(20)
  ldCurrDate   = {}
  lnAdj        = 000
  lnOld        = 000
  lnNew        = 000
  lnToStk      = 000
  lnOldCost    = 000
  lcDesc       = SPACE(20)
  lcReason     = SPACE(25)

  lcDetP     = lcCloseP
  = lfRefresh ()
  SHOW GET lcRoll   &lcRolStat
  SHOW GET ibFabric   ENABLE
  SHOW GET lcReason   DISABLE
  SHOW GET ldCurrDate DISABLE
  SHOW GET lnOld      DISABLE
  SHOW GET lnNew      DISABLE
  SHOW GET lnAdj      DISABLE
  SHOW GET lnAdjCode  DISABLE
  SHOW GET pbUpdate   DISABLE
  SHOW GET pbCancel,1 PROMPT lcDetP
ENDIF

*-- End Of lfInit
*:*************************************************************
*: Name      : lfTrapKD
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : To close the line entery screen after saving the 
*:             information , Or to cancel the information that 
*:             were taken in it .
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfTrapKD ()
*:*************************************************************
*:
FUNCTION lfvCancel
PRIVATE lcMessage 

*-- The push button will be "CLOSE" if the key fields are empty, 
*-- otherwise it will be "CANCEL"
*-- If the the p.b. is "Cancel"
IF lcDetP = lcCanclP
  lcMessage = "Are you sure ? You will lose all changes"
  IF gfModalGen('QRM36048B36001','ALERT') = 1
    IF llModify
      *-- Close the screen in case of modify mode.
      CLEAR READ
    ELSE
      *-- Reinitialize the variables.
      llNothing = lfInit(.F.)
      _CUROBJ   = OBJNUM(lcFabric)
    ENDIF
  ENDIF
ELSE
  CLEAR READ
ENDIF

*-- End Of lfvCancel
*:*************************************************************
*: Name      : lfvRolls
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : To browse the rolls for selected fabric.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvRolls ()
*:*************************************************************
*:
FUNCTION lfvRolls
PRIVATE lnAlias,lcBrFields
DECLARE laBrowArr[1]
lnAlias = SELECT(0)
SELECT Rolls
lcOldOrder = ORDER()
*-- Check if this roll is exist or not.
SET ORDER To Rollitem

lcRoll = IIF(llBrowse,PADR("?",6),lcRoll)
IF !EMPTY(lcRoll) AND !SEEK(lcFabric+lcColor+lcFromWare+SPACE(10)+lcRoll+'1')
  
  *B037180,1 ABD - Get the nearest record for the soft seek. [Begin]
  lnRecNo = RECNO(0)
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO RECNO(0)
  ENDIF  
  lnOldRecNo = RECNO()
  *B037180,1 ABD - [End]
  
  LOCATE FOR crollitem+color+cwarecode+dyelot+crollid+trancd+crsession =;
  lcFabric+lcColor+lcFromWare .AND. (nQtyBal > 0 AND TRANCD = "1")
  
  *B037180,1 ABD - Get the nearest record for the soft seek. [Begin]
  IF BETWEEN(lnOldRecNo,1,RECCOUNT())
    GOTO lnOldRecNo
  ENDIF
  *B037180,1 ABD - [End]
  
  IF FOUND()
    *B037180,1 ABD - Save the old order and set order to rolls for the soft seek. [Begin]
    lcOld_Ordr = Order()
    SET ORDER TO ROLLS
    *B037180,1 ABD - [End]
    lcTitle= 'Rolls for ' + ALLTRIM(lcFabric) + '/' + ALLTRIM(lcColor) 
    lcBrFields = [cRollId :H= 'Roll ID',] +;
                 IIF(llDyeLot  AND FABRIC.cDye_Flg = 'Y', [Dyelot :H='Dyelot',]  ,[])  +; 
                 [cWareCode :H='Location',NQtyBal :H= 'On Hand']
      lcRoll = IIF(ARIABROW([FOR crollitem+color+cwarecode+dyelot+crollid+trancd+crsession =lcFabric+lcColor+lcFromWare .AND. NQtyBal > 0 AND TRANCD = "1"],;
               lcTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','crollid','laBrowArr'),;
               Rolls.crollid,SPACE(20))
    *B037180,1 ABD - Return thr rolls file to old order.  [Begin]
    SET ORDER TO &lcOld_Ordr
    *B037180,1 ABD - [End]
    
    IF EMPTY(lcRoll)
      _CUROBJ   = OBJNUM(lcRoll)
    ENDIF
    
  ELSE
    =gfModalGen('QRM36030B36000','ALERT', 'rolls|'+ALLTRIM(lcFabric) + '/'+ALLTRIM(lcColor))
  ENDIF
ENDIF

IF !EMPTY(lcRoll)

  SELECT (lcTmpAdj)
  lcOldOrd = ORDER()
  SET ORDER TO lcTmpAdjs
  IF SEEK(lcFabric+lcColor+lcFromWare+lcRoll,lcTmpAdj)
    lnOld      = &lcTmpAdj..OldQty
    lnNew      = &lcTmpAdj..NewQty
    lcrsession = &lcTmpAdj..crsession
    lnAdj      = &lcTmpAdj..nMTotAdj
    lcReason   = &lcTmpAdj..cReason
    llModify   = .T.
  ELSE
    SELECT ROLLS
    SET ORDER To Rolls
    =SEEK(lcRoll)
    lnOld = ROLLS.NQtyBal
    lnNew = ROLLS.NQtyBal
    lcrsession = crsession
  ENDIF  
  *-- Get the Information.
  ldCurrDate = gdSysDate
  SHOW GET pbUpdate   ENABLE
  SHOW GET ldCurrDate ENABLE
  SHOW GET lcReason   ENABLE
  SHOW GET lnOld      DISABLE
  SHOW GET lnNew      DISABLE
  SHOW GET lnAdj      ENABLE
  SHOW GET pbCancel,1 PROMPT lcDetP
ENDIF

SELECT Rolls
SET ORDER TO &lcOldOrder
llBrowse = .F.
SELECT(lnAlias)

*-- End OF lfvRolls
*:*************************************************************
*: Name      : lfvlndAdj
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : To Adjustment the rolls . negative only.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvlndAdj ()
*:*************************************************************
*:
FUNCTION lfvlndAdj

IF lnAdj > 0
  lnAdj = -1 * lnAdj
ENDIF

DO CASE
  CASE lnAdj > 0
    *-- Text message :- You can enter only negative values in the Adjustment field.
    *-- Message No.  :- .F.
    *-- Button Text  :- Ok
    *-- Button No.   :- .F.
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'You can enter only negative values in the Adjustment field.')
    lnAdj   = lnOldValue 
    _CUROBJ = OBJNUM(lnAdj)

  CASE ABS(lnAdj) > lnOld
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'You can not adjustment  the quantity to be  more than the stock quantity.')
    lnAdj   = lnOldValue 
    _CUROBJ = OBJNUM(lnAdj)
    
  OTHERWISE
    lnNew = lnOld + lnAdj
ENDCASE
SHOW GET lnNew

*-- End OF lfvlndAdj
*:*************************************************************
*: Name      : lfvpbUpdate
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : Add a new record in lcTmpAdj file
*:           : or Update a modified record with new values
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvpbUpdate ()
*:*************************************************************
*:
FUNCTION lfvpbUpdate
PRIVATE lnAlias

lnAlias = SELECT()
SELECT (lcTmpAdj)
*-- Assigen Fabric link code value to variable lcLinkCode (Start)

IF llGlLink
  IF llWareHous
    =SEEK(lcFabric+lcColor+lcFromWare,'FABDYE')
    lcLinkCode = FabDye.GL_LINK
  ELSE
    =SEEK(lcFabric+lcColor,'FABRIC')
    lcLinkCode = Fabric.Link_Code
  ENDIF
ELSE
  lcLinkCode = ''
ENDIF  
IF EMPTY(lcLinkCode)
  lcLinkCode = 'DEFDEF'
ENDIF  

*-- to be used in updating field cMIcAcct
*-- AAMER 11/10/98
*-- Assigen Fabric link code value to variable lcLinkCode (End)
IF llModify
  *-- if modify mode update the TmpAj with new values  
  DO lpRepTemp
  CLEAR READ
ELSE
  *-- if new mode add new record im lcTmpAdj
  APPEND BLANK
  DO lpRepTemp
  =lfInit(.F.)
  _CUROBJ = OBJNUM(lcRoll)
ENDIF  
laScrmode[1] = .F.
laScrMode[4] = .T.
llCUpDate    = .T.
SHOW GETS
SELECT (lnAlias)

*-- End OF lfvpbUpdate.
*:*************************************************************
*: Name      : lpRepTemp
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : Replace in the lcTmpAdj file
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : lfvpbUpdate , lfChMarker
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : DO lpRepTemp
*:*************************************************************
*:
PROCEDURE lpRepTemp

PRIVATE lnCurAlias,lcCurTag
lnCurAlias = SELECT(0)
SELECT GL_Link
lcCurTag = ORDER()
SET ORDER TO GL_Link
IF SEEK(lcLinkCode+'015','GL_Link')
  lcMIcAcc = GL_Link.GLAcnt
ELSE 
  lcMIcAcc = ''  
ENDIF

SET ORDER TO &lcCurTag 
*-- if single warehouse get the oldcost from the FabDye file
IF !llWareHous
  =SEEK( lcFabric+lcColor,'FABRIC')
  lnOldCost = (IIF(lcMtCstMth $ 'LFIA',Fabric.nAveCstBuy,Fabric.CostBuy))/Fabric.Conv
ELSE
  *-- if multi warehouse get the oldcost from the Fabric file
  IF SEEK(lcFabric+lcColor+lcFromWare+SPACE(10),'FabDye')
    
    *B037180,1 ABD - get the cost from the Matinvjl file. [End]
    *lnOldCost = (IIF(lcMtCstMth $ 'LFIA',FabDye.nAveCstBuy,Fabric.CostBuy))/Fabric.Conv
    =SEEK( lcFabric+lcColor,'FABRIC')
    IF lcMtCstMth $ 'LFI'
      *-- get the cost from the Matinvjl file.
      *-- cfabric+ccolor+cwarecode+cdyelot+crsession+cisession
      IF SEEK (lcFabric+lcColor+lcFromWare+SPACE(10)+lcrsession,'Matinvjl')
        lnOldCost = Matinvjl.nUnitCost
      ELSE
        lnOldCost = Fabric.Costuse
      ENDIF
    ELSE
      lnOldCost = FabDye.Costuse
    ENDIF
    *B037180,1 ABD - [End]
  ENDIF
ENDIF


SELECT (lnCurAlias)
*-- Audit fields 
REPLACE crollId    WITH lcRoll     ,;
        Fabric     WITH lcFabric   ,;
        Color      WITH lcColor    ,;
        Desc       WITH lcDesc     ,;
        Type       WITH lcType     ,;        
        cReason    WITH lcReason   ,;
        Date       WITH ldCurrDate ,;
        OldQty     WITH lnOld      ,; 
        NewQty     WITH lnNew      ,;           
        TotStk     WITH lnToStk    ,;           
        nMTotAdj   WITH lnAdj      ,;
        UnitCost   WITH lnOldCost  ,;
        GlFYear    WITH lcGlFyear  ,;
        GlPeriod   WITH lcGlPeriod ,;
        cFromWare  WITH lcFromWare ,;
        nOldCost   WITH lnOldCost  ,;
        cAdjReason WITH lcAdjReason,;
        cAdjAcct   WITH lcAdjAcct  ,;
        nUntCstBuy WITH lnOldCost * Fabric.Conv,;
        cMIcAcct   WITH lcMIcAcc   ,;
        crsession  WITH lcrsession

= gfAdd_Info(lnCurAlias)

*-- Update Material inventory control GlAccount field (End)
llNothing = lfChMarker()

*-- End OF lpRepTemp
*:*************************************************************
*: Name      : lfVldDate
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : check in which period of Gl the date falls.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : .... 
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfVldDate ()
*:*************************************************************
*:
FUNCTION lfVldDate

IF LASTKEY() != 27 
  *-- Check if the date is in the Gl period
  llCheck =CheckPrd(ldCurrDate ,'lcGlFYear','lcGlPeriod','MA') 
  IF llGlLink
    IF ldCurrDate > ldPost
      =gfModalGen('QRM36106B36000','ALERT')  
      _CUROBJ = OBJNUM(ldCurrDate)
    ENDIF
  ELSE
    IF !llCheck  .OR. EMPTY(ldCurrDate)
      ldCurrDate = gdSysDate
      _CUROBJ = OBJNUM(ldCurrDate)
    ENDIF  
  ENDIF
ENDIF

*-- End OF lfvldDate.
*:*************************************************************
*: Name      : lfvModify
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : Modify an existing record in lcTmpAdj file
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : lfInit
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvModify()
*:*************************************************************
*:
FUNCTION lfvModify

llModify   = .T.
cbLocation = .F.

lcRolStat  = 'DISABLE'
lcDatStat  = 'ENABLE'
lcDetP     = lcCanclP

SHOW GET pbCancel,1 PROMPT lcDetP
GO lnNewRec
llNothing = lfInit(.T.)

*-- Call the Fabric details screen in the modify mode
PUSH KEY
ON KEY
DO (gcScrDir+gcWinAppl+'\MADJROLL.SPX')
POP KEY

*-- End OF lfvModify
*:*************************************************************
*: Name      : lpSavScr
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/01/2003
*: Purpose   : Scan lcTmpAdj file and calls the appropriate procedure
*:             due to lctype 
*:             if the system setup is Rolls or Lots calls browse 
*:             of either one of them
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : 
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : Do lpSavScr
*:*************************************************************
*:
PROCEDURE lpSavScr
PRIVATE lnPrvAls , lnLineNo , lnPrvQty,lnPrvVal
lnLineNo = 0

lnPrvAls = SELECT(0)

SELECT (lcTmpAdj)
SUM ABS(nMTotAdj) TO lnAdjQty 
IF lnAdjQty <= 0
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'There is no adjustment quantity to save.')
  llcSave = .F.
  RETURN
ENDIF


IF llGlLink
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  laGLDistAr[1,2] = '015'
  laGLDistAr[2,2] = '016'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE 'MA'      TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE ldPost    TO laGLDistAr[1,6],laGLDistAr[2,6]
ENDIF

*-- Generate a unique session number.
lcGlSess = gfSEQUENCE('GLSession')


SELECT (lcTmpAdj)
SCAN FOR !(nMTotAdj = 0)
  lnLineNo = lnLineNo + 1
  SELECT (lcTmpAdj)
  SCATTER MEMVAR
  *-- Store Fabric old cost, Old stock, And link code.
  SELECT Fabric
  =SEEK(M.Fabric+M.Color)
  lnOldStk   = OnHand
  lnOldCost  = (IIF(lcMtCstMth $ 'LFIA',nAveCstBuy,CostBuy))/Conv
  lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(Link_Code),Link_Code,'DEFDEF'),"")
  =SEEK(M.Fabric+M.Color+M.cFromWare,'FabDye')
  lnPrvQty = FabDye.OnHand
  lnPrvVal = FabDye.nStkVal
  
  *-- Update the Material InvJl.
  SELECT Matinvjl
  APPEN BLANK
  REPLACE cFabric    WITH M.Fabric    ,;
          cColor     WITH M.Color     ,;
          cWareCode  WITH M.cFromWare ,;
          Reference  WITH M.cReason   ,;
          cAdjReason WITH M.cAdjReason,;
          cGlMatAdj  WITH M.cAdjAcct  ,;
          dTranDate  WITH M.Date      ,;
          dPostDate  WITH ldPost      ,;
          cTranType  WITH '2'         ,;
          cTran      WITH lcGlSess    ,;
          nUnitCost  WITH M.UnitCost  ,;
          nUntCstBuy WITH M.nUntCstBuy,;
          cRSession  WITH M.crsession ,;
          cISession  WITH lcGlSess    ,;
          nIssued    WITH ABS(M.nMTotAdj),;
          nStkVal    WITH M.nMTotAdj * M.UnitCost ,;
          cTrn_Seq   WITH lcGlSess    ,;
          nMPrvSQty  WITH lnPrvQty    ,;
          nPrvSVal   WITH lnPrvVal    ,;
          llockflg   WITH .F. ,;
          LineNo     WITH lnLineNo

  *-- Call global function to add audit fields info.
  =gfAdd_Info('MATINVJL')

  *-- Update the FInvtAdj file from lcTmpAdj
  SELECT FInvtAdj
  APPEND BLANK
  M.OldQty = lnPrvQty
  GATHER MEMVAR

  IF !EOF('MATINVJL')
    REPLACE CTRN_SEQ   WITH MATINVJL.CTRN_SEQ ,;
            NFUNITCOST WITH MATINVJL.NUNITCOST
  ENDIF

  
  *-- Update the Fabric And Fabdye.
  SELECT FABRIC
  IF SEEK(M.Fabric+M.Color)
    REPLACE OnHand  WITH OnHand  - ABS(M.nMTotAdj),;
            nStkval WITH nStkval - (ABS(M.nMTotAdj) * M.UnitCost)
  ENDIF

  
  *-- Update the Fabdye file.
  SELECT FABDYE
  IF SEEK(M.Fabric+M.Color+M.cFromWare)
    REPLACE OnHand  WITH OnHand  - ABS(M.nMTotAdj),;
            nStkval WITH nStkval - (ABS(M.nMTotAdj) * M.UnitCost)
  ENDIF
  
  *-- Update the Rolls File.
  SELECT ROLLS
  lcOldOrder = Order()
  SET ORDER TO Rollitem
  APPEN BLANK
  REPLACE crollitem WITH M.Fabric    ,;
          color     WITH M.Color     ,;
          cwarecode WITH M.cFromWare ,;
          crollid   WITH M.crollid   ,;
          nqtybal   WITH ABS(M.nMTotAdj),;
          nqty      WITH ABS(M.nMTotAdj),;
          trancd    WITH '2'         ,;
          csession  WITH lcGlSess    ,;
          crsession WITH M.crsession ,;
          cisession WITH lcGlSess
  
  *-- Decrese the Qty at the main Record
  *-- crollitem+color+cwarecode+dyelot+crollid+trancd+crsession
  =SEEK(M.Fabric+M.Color+M.cFromWare+SPACE(10)+M.crollid+'1')
  REPLACE nQtyBal WITH nQtyBal + M.nMTotAdj
  SET ORDER TO &lcOldOrder 

  *-- Update the General Ledger file
  IF llGlLink
    STORE &lcTmpAdj..GLFYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
    STORE &lcTmpAdj..GLPeriod  TO laGLDistAr[1,8],laGLDistAr[2,8]
    STORE lctmpGlDis TO laGLDistAr[1,9],laGLDistAr[2,9]
    laGLDistAr[2,10] = &lcTmpAdj..cAdjAcct
    
    lcMatLink = IIF(EMPTY(FabDye.GL_Link),Fabric.Link_Code,FabDye.GL_Link)
    DO GLDIST WITH lcMatLink,'015',MatInvJl.nStkVal,'MA',;
                   MatInvJl.cTran,MatInvJl.dTranDate,laGLDistAr[1,7],;
                   laGLDistAr[1,8],laGLDistAr[1,9],'','','',''
    lnCurAlias = SELECT(0)
    SELECT MatInvJl
    REPLACE cMIcAcct WITH &laGLDistAr[1,9]..GLAccount
    SELECT (lnCurAlias)
    DO GLDIST WITH lcMatLink,'016',-MatInvJl.nStkVal,'MA',;
                   MatInvJl.cTran,MatInvJl.dTranDate,laGLDistAr[1,7],;
                   laGLDistAr[1,8],laGLDistAr[1,9],laGLDistAr[2,10],'','',''
    IF EMPTY(MatInvJl.cGLMatAdj)
      lnCurAlias = SELECT(0)
      SELECT MatInvJl
      REPLACE cGLMatAdj WITH &laGLDistAr[1,9]..GLAccount
      SELECT (lnCurAlias)
    ENDIF

    SELECT (lctmpGlDis)
    
    *-- If temp. file not empty
    IF RECCOUNT() <> 0
      REPLACE  ALL &lctmpGlDis..GlSession WITH lcGlSess
      SELECT GLDIST  
      APPEND FROM (gcWorkDir+lctmpGlDis)
    ENDIF
  
    SELECT (lctmpGlDis)
    SCAN
      BLANK
      DELETE
    ENDSCAN
  
    SELECT (lnCurAlias)
  ENDIF  
ENDSCAN

*--Would you like to print the Inventory Adjustment journal ?
IF gfModalGen('TRM42066B42002','DIALOG') = 1
  *-- IHB Calling report if user want to print it 
  PRIVATE lcRunRep,lcRepsDir
  lcRunRep  = 'MASTKJL'
  lcRepsDir = gcRepHome+gcAct_Appl+'REPORT'  && TO CALL MAREPORT WITH 'MASTKJL'
  DO &lcRepsDir WITH lcRunRep
ENDIF  

SELECT Rolls
LOCATE
IF llTrkRolls .AND. ( lcMtCstMth = "L" OR !EOF('Rolls') )
  *-- Message < Would you like to print Barcode labels ? >
  *-- Message < Would you like to print Rolls label ? >  
  *-- Buttons <               YES        NO              >
  IF gfModalGen('QRM36171B42002','DIALOG','Rolls label') = 1
    PRIVATE lcRunRep , lcRepDir , lcCallFrom
    lcRunRep  = 'MABARC'           && Calling BarCode labels with another report ID
    lcRepDir = gcRepHome+gcAct_Appl+'REPORT'  && To call MaReport with 'MaBarc'
    lcCallFrom = "P"      && Report is called from Program from Transaction menu
    DO &lcRepDir WITH lcRunRep
  ENDIF
ENDIF

IF llTrkRolls .OR. lcMtCstMth = "LFI"
  ERASE (gcWorkDir + lcTmpRolls."DBF")
  ERASE (gcWorkDir + lcTmpRolls."CDX")
ENDIF  

DO lpClsScr
SHOW GETS

SELECT(lnPrvAls)

*-- End Of lpSavScr
*!*************************************************************
