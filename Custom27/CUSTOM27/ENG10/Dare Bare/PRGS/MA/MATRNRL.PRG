*!*************************************************************
*! Program File : MATRNRL C#200474
*! Program Desc : Material Transfer by Roll - for Dare to Bare Ltd.
*! For Screen   : MATRNRL.SPR
*!       System : Aria Apparel Series(A27)
*!       Module : MATERIAL Module
*! Developer    : AHMED MAHER (AMH)
*!*************************************************************
*! Modifications :
*!*************************************************************
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DECLARE ladata[01],laKeyField[1,4],laAddress[6,3],laDefProc[10],;
        laVariables[1]

STORE '' TO laData,laKeyField,laAddress,lcWinCh1,lcWinCh2,lcWinCh3,lcWinCh4,lcModal,lcTmpAdj,;
            lcTmpRoll3,lcTmpRoll4,lcTmpRol33,lcTmpRol44,lcFromWare,lcToWare,;
            lcFrmDesc,lcToDesc,lcToWare,lctmpGlDis,lcAdjReason,lcAdjAcct,;
            lcGlFYear,lcGlPeriod

STORE {} TO ldPost

*-- Initialize the title of screen Mainvct
lcWinTitle = 'Material Transfer by Roll'

*-- To allow the program to enter the lpShow procedure at the calling time
llNoShow = .F.

*-- Get the old trap of Esc Key
lcOldEscTrap = ON('Key','Esc')

*-- Allow the program to have its local Save and local Close
laDefProc[9]  = .F.
laDefProc[10] = .F.

*-- Array used for the Adj. Code
DIMENSION  laAdjCode[1] , laCodInfo [1,10]
STORE "" TO laAdjCode, laCodInfo
lcTmpDyRel  = ' '

IF !gfSetup()
  RETURN
ENDIF  

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
llTrkRolls = laSetUps[5,2] = 'Y'
lcMtCstMth = laSetUps[6,2]

lcBaseFile = ''

*-- Convetrting lcChhoice to numeric and 
*-- then define lctype as character
lcType     = 'T'
lcTypeX = lcType

*-- For marking and unmarking in the browse
lcMark     = '>'
lcUnMark   = SPACE(1)

*-- Initialize the contets of push botttons
lcNewP     = gcBmpHome + "NEW.BMP"
lcRemveP   = gcBmpHome + "REM.BMP"
lcExtKey   = gcBmpHome + "ExtKey.BMP"
lnColorWid = (06)

*-- Initialize the status of pbnew 
lcNewStat    = IIF(llWareHous,'DISABLE','ENABLE')

*-- Initialize the lcFromWare with default warehouse if !llWareHous
*-- this variable will be used as the used warehouse in case of
*-- doing adjustement or Physical inventory.
IF !llWareHous
  qWareHouse = WAREHOUS.cWareCode
  lcFromWare = qWareHouse
ENDIF

*-- Initialize the lcWareAny with defau warehouse or lcFromWare
*-- This variable is used by the mover screen in case of using the locations.
lcWareAny = IIF(llWareHous, lcFromWare, qWareHouse)

lnAdj = 0
lnOld = 0
lnNew = 0
lnToStk = 0
lnNewCost = 0
lnOldCost = 0
lnNewRec = 0

*-- Browse title of the mainbrowse of items details (screen Mat400_2)
lcBrowTtl = "Transaction's lines"

*-- Initialize the llDyeLvl to be false and then it will be computed
*-- due to the setup of the system and each item setup
llDyeLvl = .F.

*-- Initialize the llModify to be false as it is new mode
*-- then change to true if in modify mode
llModify = .F.
*-- Variable tho check if the temp. file of rolls is empty
llEmpTmpRo = .F.

*-- Define the size of box in main screen 
lnCenBoxY = 4
lnCenBoxS = 18 - lnCenBoxY

*-- Set the required oreders for the files
SELECT ROLLS
SET ORDER TO TAG ROLLITEM

SELECT FABDYE
SET ORDER TO TAG FABDYE

SELECT FABRIC
SET ORDER TO TAG FABRIC

IF !WEXIST(gcBaseWind)
  IF !llWareHous
    glQuitting = .T.
    =gfModalGen('QRM36046B36000','ALERT')
    RETURN
  ENDIF
  
  *-- Define the child windows 
  lcWinCh1 = gfTempName()
  lcWinCh2 = gfTempName()
  lcWinCh3 = gfTempName()
  lcWinCh4 = gfTempName()
  
  IF llGlLink
    *-- OPEN GLDIST FILE TO CALL GENERAL LEDGER DISTRIBUTION PROCEDURE 
    SELECT GLDIST

    *-- OPEN A TEMP FILE TO BE USED IN CALLING 'GLDIST' PROCEDURE.
    lctmpGlDis = gfTempName()
    COPY STRUCTURE TO &gcWorkDir.&lctmpGlDis
    SELECT 0
    USE (gcWorkDir+lctmpGlDis) EXCLUSIVE
  ENDIF
  
  lcTmpRoll3 = gfTempName()
  lcTmpRoll4 = gfTempName()
  lcTmpRol33 = gfTempName()
  lcTmpRol44 = gfTempName()

  SELECT ROLLS
  =AFIELDS(laRollStru)
  
  CREATE CURSOR (lcTmpRoll4) FROM ARRAY laRollStru
  INDEX ON CROLLITEM+COLOR+CROLLID TAG (lcTmpRol44) OF (lcTmpRoll4)
  INDEX ON CROLLID TAG (lcTmpRoll4) OF (lcTmpRoll4)
  
  CREATE CURSOR (lcTmpRoll3) FROM ARRAY laRollStru
  INDEX ON CROLLITEM+COLOR+CROLLID TAG (lcTmpRol33) OF (lcTmpRoll3)
  INDEX ON CROLLID TAG (lcTmpRoll3) OF (lcTmpRoll3)

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
  
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'cMIcAcct'
  laFileStru[lnNewFld,2] = 'C'
  laFileStru[lnNewFld,3] = 24
  laFileStru[lnNewFld,4] = 0
  
  CREATE TABLE (gcWorkDir+lcTmpAdj) FROM ARRAY laFileStru
  INDEX ON FABRIC+COLOR+DYELOT TAG (lcTmpAdj)
  
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
* --------------------------------------------------------------

*-- Calling of the main screen MATRNRL
PUSH KEY
= lfTrapKeys()
DO (gcScrDir+gcWinAppl+'\MATRNRL.SPX')
POP KEY

*-- Delete all the temp. files when closing the program
IF glQuitting
  USE IN (lcTmpAdj)
  ERASE (gcWorkDir+lcTmpAdj+'.DBF')
  ERASE (gcWorkDir+lcTmpAdj+'.CDX')
  ERASE (gcWorkDir+lcTmpAdj+'.FPT')
  
  USE IN (lcTmpRoll3)
  USE IN (lcTmpRoll4)
  
  IF llGlLink
    USE IN (lctmpGlDis)
    ERASE (gcWorkDir+lctmpGlDis+".DBF")
  ENDIF  
  
ENDIF

*!*************************************************************
*! Name        : lpClsScr
*! Developer   : AHMED MAHER (AMH)
*! Date        : 02/27/2003
*!*************************************************************
*! Purpose     : To do the validation of pbClose at the main
*!               screen and be sure that the user want to finish
*!               this program and Zap tmp.files and clear read
*!*************************************************************
PROCEDURE lpClsScr

*-- Zap the adjustment temp. file.
SELECT (lcTmpAdj)
ZAP

*-- Zap the GL temp. file.
IF llGlLink
  SELECT (lctmpGlDis)
  ZAP
ENDIF
SHOW GETS

*!*************************************************************
*! Name       : lfActBrow
*! Developer  : AHMED MAHER (AMH)
*! Date       : 02/27/2003
*!*************************************************************
*! Purpose     : To define the header of the main browse for each
*!               type and browse the fields of each item
*!*************************************************************
FUNCTION lfActBrow

SELECT (lcTmpAdj)
lcType = lcTypeX

*-- Basic fields in every case of lctype
lcBasBrow  = "cMarker:H = ' ',Fabric:H='Item', Color:H='Color', Desc:H='Description':8"
lcDatBrow  = ",Date :H='Date'"
lcTBrow    = [OldQty:H="&lcFromWare"]+", nMTotAdj :H='Transfer'"+ [,NewQty :H="&lcToWare"] 
lcToUse    = "lc"+lcType+"Brow"
lcEndBrow  = &lcToUse

lcBrFields =  lcBasBrow+lcDatBrow+","+lcEndBrow

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

*!*************************************************************
*! Name       : lfTrapKeys
*! Developer  : AHMED MAHER (AMH)
*! Date       : 02/27/2003
*!*************************************************************
*! Purpose    : To do the trapping between the 3
*!               screens Mat400_1, Mat400_ 2, Mat400_3
*!*************************************************************
FUNCTION lfTrapKeys

ON KEY LABEL CTRL+W     lnDummy = 1
ON KEY LABEL CTRL+Q     lnDummy = 1
ON KEY LABEL CTRL+HOME  lnDummy = 1
ON KEY LABEL CTRL+END   lnDummy = 1
ON KEY LABEL TAB        DO lpTab
ON KEY LABEL BACKTAB    DO lpShiftTab
ON KEY LABEL ESC        DO lpEsc
ON KEY LABEL CTRL+ENTER DO lpEsc

*!*************************************************************
*! Name      : lpTab
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose    : To do the trapping of the tab key
*!               between the 3
*!               screens Mat400_1, Mat400_ 2, Mat400_3
*!*************************************************************
PROCEDURE lpTab

ON KEY LABEL TAB lnDummy = 1

DO CASE
  CASE WONTOP('lcWinCh1') .AND. _CUROBJ = IIF(llGlLink,OBJNUM(ldPost),OBJNUM(lcToWare))
     ACTIVATE WINDOW (lcBrowTtl)
  CASE WONTOP(lcBrowTtl) 
    ACTIVATE WINDOW lcWinCh3
    _CUROBJ = OBJNUM(pbNew)
  CASE WONTOP('lcWinCh3') .AND. (_CUROBJ = OBJNUM(pbModify) OR ;
             (_CUROBJ = OBJNUM(pbNew) AND EOF(lcTmpAdj)))  
    ACTIVATE WINDOW ('gwcContrl1')
  CASE WONTOP('gwcContrl1') .AND. _CUROBJ = OBJNUM(pbCls)
    DO CASE
      CASE llGlLink
        ACTIVATE WINDOW lcWinCh1
        _CUROBJ = OBJNUM(ldPost)
      CASE llWareHous AND !llGlLink
        ACTIVATE WINDOW lcWinCh1 
        _CUROBJ = OBJNUM(lcFromWare)
     OTHERWISE  
        ACTIVATE WINDOW lcWinCh3 
        _CUROBJ = OBJNUM(pbNew)
    ENDCASE  
  OTHERWISE
     _CUROBJ = _CUROBJ + 1
ENDCASE
ON KEY LABEL TAB DO lpTab      

*!*************************************************************
*! Name      : lpShiftTab
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose    : To do the trapping of the back tab 
*!              between the 3
*!               screens Mat400_1, Mat400_ 2, Mat400_3
*!*************************************************************
PROCEDURE lpShiftTab

ON KEY LABEL BACKTAB lnDummy = 1
DO CASE
  CASE WONTOP('lcWinCh1') .AND. _CUROBJ = OBJNUM(ldPost)
    ACTIVATE WINDOW ('gwcContrl1')
   _CUROBJ = OBJNUM(pbCls)

  CASE WONTOP(lcBrowTtl) 
    ACTIVATE WINDOW lcWinCh1
    _CUROBJ = OBJNUM(lcToWare)

  CASE WONTOP('lcWinCh3') .AND. _CUROBJ = OBJNUM(pbNew)
    ACTIVATE WINDOW (lcBrowTtl) 

  OTHERWISE
    _CUROBJ = _CUROBJ - 1
ENDCASE
ON KEY LABEL BACKTAB DO lpShiftTab

*!*************************************************************
*! Name      : lpEsc
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : Trapping the Escape button.
*!*************************************************************
PROCEDURE lpEsc

ACTIVATE WINDOW ('gwcContrl1')
_CUROBJ = OBJNUM(pbCls)
KEYBOARD CHR(13)

*!*************************************************************
*! Name      : lfvWare
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : To validate the warehouses codes.
*!*************************************************************
FUNCTION lfvWare
PARAMETERS lcPrmWrCod
PRIVATE lnAlias

lnAlias     = SELECT()
SELECT WareHous

*-- To tell if we're dealing with the TOWARE code in Transfer mode.
llTo        = (UPPER(lcPrmWrCod)="LCTOWARE")

*-- The description variable to be updated.
lcToUpdate  = "lc" + IIF(llTo, "To","Frm") + "Desc"

*-- Message appears in case of transfer and lcToWare = lcFromWare
lcMessage   = "You Can't Transfer "+IIF(llTo, "to","from")+" the same Warehouse."

*-- To display the browse of WareHouses
&lcPrmWrCod = IIF(!EMPTY(&lcPrmWrCod) AND !SEEK(&lcPrmWrCod), gfBrowWare(.T.), &lcPrmWrCod)

*--To display the Desc. of the chosen Warehouse
&lcToUpdate = IIF(!EMPTY(&lcPrmWrCod), WareHous.cDesc, SPACE(0))

*-- Case of transfer check if 
*-- lcToWarecode = lcFromWarecode and both are not empty
IF !EMPTY(lcFromWare+lcToWare)
  IF lcFromWare = lcToWare
    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,lcMessage)
    _CUROBJ      = _CUROBJ
    &lcPrmWrCod  = SPACE(06)
    &lcToUpdate  = SPACE(0)
  ELSE
  
  *-- if the key(s) is not empty, then disable the key object(s) 
  *-- and enable the rest of the objects.
    IF !EMPTY(lcFromWare) AND !EMPTY(lcToWare)
      SHOW GET lcFromWare DISABLE
      SHOW GET lcToWare   DISABLE
      SHOW GET ibFromBrow DISABLE
      SHOW GET ibToBrow   DISABLE
      SHOW GET pbNew      ENABLE
    ENDIF
  ENDIF  
ENDIF

*-- Refresh the main browse in case of transfer, with the codes of 
*-- "from" and "to" warecodes.
IF !EMPTY(&lcPrmWrCod) 
  llNothing = lfActBrow()
ENDIF
llNothing = lfRefresh()
SHOW GET &lcPrmWrCod 
laScrMode[1] = IIF(EMPTY(&lcPrmWrCod ),.T.,.F.)
laScrMode[4] = IIF(!EMPTY(&lcPrmWrCod ),.T.,.F.)
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfBroKey
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : Validate the browse invisible buttons for the 
*!             warehouses codes.
*!*************************************************************
FUNCTION lfBroKey

lcCurWare  = "LC"+IIF(SUBSTR(SYS(18),3,1)="F","FROM","TO")+"WARE"
&lcCurWare = PADR("?",6)
llNothing  = lfvWare(lcCurWare)
_CUROBJ    = OBJNUM(&lcCurWare)

*!*************************************************************
*! Name      : lfvNew
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : To call the screen to enter the details for
*!             a line in the main browse.
*!*************************************************************
FUNCTION lfvNew

*-- Case of new record initialize all the varaibles
cbLocation = .F.
lcLocStat  = "DISABLE"
llModify   = .F.
lcFabStat  = 'ENABLE'
lcDatStat  = 'DISABLE'
lcSavStat  = 'DISABLE'
=lfInit(.F.)

*-- Call the screen to enter the detail for each item
PUSH KEY
ON KEY
=lfNewModRl(.F.)
POP KEY

*!*************************************************************
*! Name      : lfwFabric
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : Get the fabric code in the line entery screen.
*!*************************************************************
FUNCTION lfwFabric

lcOldFab    = lcFabric

*!*************************************************************
*! Name      : lfvFabric
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : Validate the fabric code in the line entery screen.
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT Fabric

IF !EMPTY(lcFabric) AND !SEEK(lcFabric)
  DO FaBrow WITH lcFabric,'*'
ENDIF

*-- Initialize the Color and Dyelot variables
lcColor   = IIF(!EMPTY(lcFabric),IIF(!EMPTY(lcOldFab) AND lcFabric <> lcOldFab,SPACE(lnColorWid),lcColor),SPACE(lnColorWid))
llNothing = IIF(!EMPTY(lcColor),lfvColor(),.T.)
lcDyelot  = SPACE(10)
lcOldFab  = SPACE(07)
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvColor
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : Validate the entry of a fabric color
*!             code and display the browse if it is not found in 
*!             the Fabric file. Check if the Fabric + Color has 
*!             Dyelot or not.
*!*************************************************************
FUNCTION lfvColor
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT Fabric
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
*-- the push botton Cancel will be close.
IF !EMPTY(lcFabric) .AND. !EMPTY(lcColor)
  *-- Check if (Fabric + color) found in warehouse or not
  *-- if !found() and the user wants the enter a new(fabric + color)
  *-- initialize the variables with new
  IF lfFCDWCheck()
    llNoThing = lfInit(.F.)
    _CUROBJ   = OBJNUM(lcFabric)
  ELSE
    lnNew     = FABDYE.ONHAND
    =SEEK(lcFabric+PADR(lcColor,6)+lcFromWare+lcDyelot,'FABDYE')
    lcFabDesc = FABRIC.DESC
    lnOnHand  = FABDYE.ONHAND
    lnBalance = FABDYE.ONHAND
    lnOld     = FABDYE.ONHAND
    lnToStk   = FABDYE.ONHAND
    lnAdj     = 0
    lnOldCost = FabDye.nAveCstBuy/Fabric.Conv
    lnNewCost = FabDye.nAveCstBuy/Fabric.Conv

    SHOW GET lcFabric   DISABLE
    SHOW GET lcColor    DISABLE
    SHOW GET ibFabric   DISABLE
    SHOW GET ibColor    DISABLE
    SHOW GET lcFabDesc
    SHOW GET lnOnHand
    SHOW GET pbSavRol   ENABLE
    =lfGetRolls(.F.)
    _CUROBJ = OBJNUM(pbNewRol)
  ENDIF
ENDIF

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : To initialize the variables used in the line 
*!             entery screen either from the lcTmpAdj file or with 
*!             new values.
*!*************************************************************
FUNCTION lfInit
PARAMETERS llMode

*-- Initialize the variables from the file.
IF llMode
  lcFabric   = &lcTmpAdj..Fabric  
  lcColor    = &lcTmpAdj..Color
  lcDyelot   = &lcTmpAdj..Dyelot
  lcFabDesc  = &lcTmpAdj..Desc
  lnTran     = &lcTmpAdj..nMTotAdj
  =SEEK(lcFabric+PADR(lcColor,6)+lcFromWare+lcDyelot,'FABDYE')
  lnOnHand   = FABDYE.ONHAND
  lnBalance  = lnOnHand - lnTran
  lnOld      = &lcTmpAdj..OldQty
  lnNew      = &lcTmpAdj..NewQty
  lnToStk    = &lcTmpAdj..TotStk
  lnAdj      = &lcTmpAdj..nMTotAdj
  lnOldCost  = &lcTmpAdj..nOldCost
  lnNewCost  = &lcTmpAdj..UnitCost
ELSE
  *-- Empty the variables.
  lcFabric   = SPACE(07)
  lcColor    = SPACE(lnColorWid)
  llDyeLvl   = .F.
  lcDyelot   = SPACE(10)
  lcFabDesc  = SPACE(20)
  lnOld      = 0
  lnNew      = 0
  lnToStk    = 0
  lnAdj      = 0
  lnOldCost  = 0
  lnNewCost  = 0
  SHOW GET lcFabric   &lcFabStat
  SHOW GET lcColor    &lcFabStat
  SHOW GET ibFabric   ENABLE
  SHOW GET ibColor    ENABLE
ENDIF

*!*************************************************************
*! Name      : lfFabKey
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : Validate fabric browse button.
*!*************************************************************
FUNCTION lfFabKey

lcFabric  = PADR("?",7)
llNothing = lfvFabric()
_CUROBJ   = OBJNUM(lcFabric)

*!*************************************************************
*! Name      : lfColKey
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : To browse the Colors available for the chosen
*!             Fabric
*!*************************************************************
FUNCTION lfColKey

lcColor   = PADR("?",lnColorWid)
llNothing = lfvColor()

*!*************************************************************
*! Name      : lfFCDWCheck
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : To check if the Fabric + Color + Dyelot
*!             are assigned to the chosen Warehouse
*!             and if not you may add it to the ToWareHouse
*!             in case of transfer
*!*************************************************************
FUNCTION lfFCDWCheck
PRIVATE InAlias, llReEnter, lcWareCode

llReEnter  = .F.

InAlias    = SELECT(0)

*-- Check if the (Fabric + Color + Dyelot)
*-- are assigned to the FromWareHouse or not
SELECT FabDye

IF SEEK(lcFabric+PADR(lcColor,6)+lcDyelot,lcTmpAdj)
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Item/Color : "+ALLTRIM(lcFabric)+'/'+;
                ALLTRIM(lcColor)+" is selected before, Select anther Item/Color.")
  RETURN (.T.)
ENDIF

IF !SEEK (lcFabric+PADR(lcColor,6)+lcFromWare+lcDyelot)
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Item/Color : "+ALLTRIM(lcFabric)+'/'+;
                ALLTRIM(lcColor)+" is not assigned to location "+ALLTRIM(lcFromWare)+".")
  RETURN (.T.)
ELSE
  IF !SEEK(lcFabric+PADR(lcColor,6)+lcFromWare+lcDyelot,'ROLLS')
    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Item/Color : "+ALLTRIM(lcFabric)+'/'+;
                  ALLTRIM(lcColor)+" has no available rolls in the location "+ALLTRIM(lcFromWare)+".")
    RETURN (.T.)
  ELSE
    SELECT ROLLS
    LOCATE REST WHILE cRollItem+Color+cWareCode+Dyelot+cRollId+TranCd+cRsession =;
                      lcFabric+PADR(lcColor,6)+lcFromWare+lcDyelot;
                  FOR TRANCD = '1' .AND. NQTYBAL > 0
    IF !FOUND()
      = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Item/Color : "+ALLTRIM(lcFabric)+'/'+;
                    ALLTRIM(lcColor)+" has no available rolls in the location "+ALLTRIM(lcFromWare)+".")
      RETURN (.T.)
    ENDIF
    SELECT FABDYE
  ENDIF
ENDIF
  
*-- if it is transfer make this check for target warehouse

IF !SEEK (lcFabric+PADR(lcColor,6)+lcToWare+lcDyelot)
  llReEnter = gfModalGen('QRM36049B36001','ALERT',IIF(llDyeLvl,'/Dyelot','')+':'+ALLTRIM(lcFabric)+'/'+ALLTRIM(lcColor)+IIF(llDyeLvl,'/'+ ALLTRIM(lcDyelot),' ')+'|'+ALLTRIM(lcToWare)) = 2
  IF !llReEnter
    DO gpAdFabWar WITH lcFabric, lcColor, SPACE(10), lcToWare
    IF llDyeLvl
      DO gpAdFabWar WITH lcFabric, lcColor, lcDyelot, lcToWare , lcTmpDyRel
    ENDIF
  ENDIF
ENDIF

SELECT(Inalias)
RETURN (llReEnter)

*!*************************************************************
*! Name      : lfActWindw
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : Set the status of push buts. at screen Mat400_3
*!*************************************************************
FUNCTION lfActWindw
PRIVATE llCon, lcStatus

IF llWareHous
  llCon   = (!EMPTY(lcFromWare) AND IIF(lcType="T",!EMPTY(lcToWare), .T.))
  lcKeySt = IIF(llCon, "DISABLE", "ENABLE")
  lcNewSt = IIF(llCon, "ENABLE" , "DISABLE")
ELSE
  lcKeySt = "ENABLE"
  lcNewSt = "ENABLE"
ENDIF

GOTO TOP IN (lcTmpAdj)
IF EOF(lcTmpAdj)
  lcKeySt  = IIF(llWareHous, lcKeySt, "ENABLE")
  lcStatus = "DISABLE"
ELSE
  lcStatus = "ENABLE"
  lcKeySt  = "DISABLE"
ENDIF
SHOW GET lcFromWare &lcKeySt
SHOW GET lcToWare   &lcKeySt
SHOW GET ibFromBrow &lcKeySt
SHOW GET ibToBrow   &lcKeySt

llNoThing = lfRefresh()

*!*************************************************************
*! Name      : lfvModify
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : Modify an existing record in lcTmpAdj file
*!*************************************************************
FUNCTION lfvModify

llModify   = .T.
lcFabStat  = 'DISABLE'
lcSavStat  = 'ENABLE'
GO lnNewRec

*-- Call the Fabric details screen in the modify mode
PUSH KEY
ON KEY
=lfNewModRl(.T.)
POP KEY

*!*************************************************************
*! Name      : lfvRemove
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : Remove an existing record in lcTmpAdj file
*!*************************************************************
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
ENDIF

*!*************************************************************
*! Name      : lpRepTemp
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : Replace in the lcTmpAdj file
*!*************************************************************
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
SELECT (lnCurAlias)

REPLACE Fabric     WITH lcFabric   ,;
        Color      WITH lcColor    ,;
        Dyelot     WITH lcDyelot   ,;
        Desc       WITH lcFabDesc  ,;
        Type       WITH lcType     ,;
        cReason    WITH ''         ,;
        Date       WITH gdSysDate  ,;
        OldQty     WITH lnOld      ,; 
        NewQty     WITH lnNew      ,;           
        TotStk     WITH lnToStk    ,;           
        nMTotAdj   WITH lnAdj      ,;
        UnitCost   WITH lnNewCost  ,;
        GlFYear    WITH lcGlFyear  ,;
        GlPeriod   WITH lcGlPeriod ,;
        cFromWare  WITH lcFromWare ,;
        cToWare    WITH lcToWare   ,;
        nOldCost   WITH lnOldCost  ,;
        cAdjReason WITH ''         ,;
        cAdjAcct   WITH lcAdjAcct  ,;
        nUntCstBuy WITH lnNewCost*Fabric.Conv,;
        cMIcAcct   WITH lcMIcAcc

= gfAdd_Info(lnCurAlias)
llNothing = lfChMarker()
=lfSavTmpRl()

*!*************************************************************
*! Name      : lfChMarker
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : Set the marker in any browse
*!*************************************************************
FUNCTION lfChMarker

IF !EOF()
  lnNewRec = RECNO()
  REPLACE ALL cMarker WITH lcUnMark
  GO lnNewRec
  REPLACE cMarker WITH lcMark
ENDIF  

*!*************************************************************
*! Name      : lpSavScr
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : Scan lcTmpAdj file and calls the appropriate procedure
*!             due to lctype 
*!             if the system setup is Rolls or Lots calls browse 
*!             of either one of them
*!*************************************************************
PROCEDURE lpSavScr

*-- Generate a unique session number.
lcGlSess = gfSEQUENCE('GLSession')

SELECT (lcTmpAdj)
SCAN FOR ! ((nMTotAdj = 0) .AND. (lcType $ 'AT'))
  = SEEK (&lcTmpAdj..Fabric+&lcTmpAdj..Color , 'Fabric')
  llDyeLvl = llDyelot .AND. Fabric.cDye_Flg = 'Y'
  lcFab    = &lcTmpAdj..Fabric
  lcClr    = &lcTmpAdj..Color
  lcWare   = &lcTmpAdj..cFromWare
  lcDye    = &lcTmpAdj..Dyelot

  *-- Store Fabric old cost, Old stock, And link code.
  SELECT Fabric
  lnOldStk   = OnHand
  lnOldCost  = nAveCstBuy/Conv
  
  *-- Store the stock in the TO warehouse before updating the master
  *-- file.
  = SEEK (&lcTmpAdj..Fabric+&lcTmpAdj..Color+&lcTmpAdj..cToWare+;
          IIF(llDyeLvl, &lcTmpAdj..Dyelot,SPACE(10)),'FabDye')
  SELECT (lcTmpAdj)
  REPLACE nOldToQty WITH FabDye.OnHand
  
  *-- Update the MatInvJl and FabDye files
  =lfSvMatInv(nMTotAdj,UnitCost,cAdjAcct,cMIcAcct)
  
  *-- Update the FInvtAdj file
  SCATTER MEMVAR
  m.nFUnitCost = m.UnitCost
  m.cTrn_Seq   = lcGlSess
  SELECT FINVTADJ
  APPEND BLANK
  GATHER MEMVAR
  
  *-- Update the GlDist file
  =lfGlDist()
  
ENDSCAN

*-- Update the Rolls file
=lfSvRolls()

IF llGlLink
  SELECT (lctmpGlDis)
  *-- If temp. file not empty
  IF RECCOUNT() <> 0
    REPLACE ALL GLSESSION WITH lcGlSess,;
                TRAN_NO   WITH lcGlSess
    SELECT GLDIST
    APPEND FROM (gcWorkDir+lctmpGlDis)
  ENDIF
ENDIF

DO lpClsScr
SHOW GETS

*!*************************************************************
*! Name      : lpShow
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose   : for the show of objects for each mode in the screen
*!
*!*************************************************************
PROCEDURE lpShow

DO CASE
  CASE laScrMode[1]

   *-- Initialize the variable of Gl link (Year + Period)
   lcGlFYear    = SPACE(04)
   lcGlPeriod   = SPACE(02)
   lcFromWare   = IIF(llWareHous, SPACE(06), qWareHouse) 
   lcFrmDesc    = SPACE(00)
   lcToDesc     = SPACE(00)
   lcToWare     = SPACE(06)
   
   IF !llGlLink
     SHOW GET lcFromWare ENABLE
     SHOW GET lcToWare   ENABLE
     SHOW GET ibFromBrow ENABLE
     SHOW GET ibToBrow   ENABLE
     SHOW GET pbNew      &lcNewStat
   ENDIF
   
   SHOW GET pbBrws     DISABLE
   SHOW GET pbSav      DISABLE
   SHOW GET pbRemove   DISABLE
   SHOW GET pbModify   DISABLE
   =lfActBrow()
   =lfRefresh()
   ldPost = gdSysDate

   DO CASE
     CASE llGlLink
       _CUROBJ = OBJNUM(ldPost)
     CASE llWareHous AND !llGlLink
       _CUROBJ = OBJNUM(lcFromWare)
     OTHERWISE  
      _CUROBJ = OBJNUM(pbNew)
   ENDCASE
   
  CASE laScrMode[4]
    SHOW GET lcFromWare DISABLE
    SHOW GET lcToWare   DISABLE
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

*!*************************************************************
*! Name       : lfClearTrap
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose    : to clear the trap after leaving the browse screen
*!
*!*************************************************************
FUNCTION lfClearTrap

IF WONTOP() <> lcBrowTtl
   ON KEY LABEL ESC &lcOldEscTrap
   ON KEY LABEL TAB
   ON KEY LABEL BACKTAB
ENDIF   

*!*************************************************************
*! Name       : lfvDate
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/27/2003
*!*************************************************************
*! Purpose    : valodate the posting date
*!*************************************************************
FUNCTION lfvDate
 
IF LASTKEY() = 13
  IF !CheckPrd(ldPost,'lcGlFYear','lcGlPeriod',;
                  IIF(lcType='P','MP','MA')) OR EMPTY(ldPost)
    ldPost  = gdSysDate
    _CUROBJ = OBJNUM(ldPost)
  ENDIF  
  SHOW GET lcFromWare ENABLE
  SHOW GET lcToWare   ENABLE
  SHOW GET ibFromBrow ENABLE
  SHOW GET ibToBrow   ENABLE
  SHOW GET pbBrws     DISABLE
  SHOW GET pbNew      &lcNewStat
ENDIF

*!**************************************************************************
*! Name      : lfNewModRl
*! Developer : (AMH) Ahmed Maher
*! Date      : 03/05/2003
*! Purpose   : New/Modify By Roll
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*
FUNCTION lfNewModRl
PARAMETERS llMode

PRIVATE lnAlias,lnTran,lnBalance,lnOnHand
PRIVATE lcFabric,lcColor,lcRollID,lcRRollID,lcBrowseTl

STORE 0  TO lnTran,lnBalance,lnOnHand
STORE 1  TO lnDMarker,lnRMarker
STORE '' TO lcRollID,lcRRollID,lcFabric,lcColor,lcTmpRolls,lcTmpRoll2,lcFabDesc
STORE '' TO lcRollTit,lcAsRollTit,lcDyelot,lcOldFab,lcBrowseTl

lcRollTit   = 'Material Available Rolls'
lcAsRollTit = 'Transfer Rolls'

lcTmpRolls = gfTempName()
lcTmpRoll2 = gfTempName()

lnAlias = SELECT(0)
SELECT ROLLS
=AFIELDS(laRollStru)

CREATE CURSOR (lcTmpRoll2) FROM ARRAY laRollStru
INDEX ON CROLLID TAG (lcTmpRoll2) OF (lcTmpRoll2)

CREATE CURSOR (lcTmpRolls) FROM ARRAY laRollStru
INDEX ON CROLLID TAG (lcTmpRolls) OF (lcTmpRolls)

IF llMode
  = lfInit(.T.)
  =lfGetRolls(.T.)
ENDIF

lcOk     = gcBmpHome + "OK.BMP"
lcCancel = gcBmpHome + "CAN.BMP"
lcMoveR  = gcBmpHome + "NEXT.BMP"
lcMoveL  = gcBmpHome + "PRIOR.BMP"
DO (gcScrDir+gcWinAppl+'\MATRNRL4.SPX')

USE IN (lcTmpRolls)
USE IN (lcTmpRoll2)
=lfActBrow()
SELECT (lnAlias)
*-- end of lfNewModRl.

*!*************************************************************
*! Name      : lfBrowRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/05/2003
*! Purpose   : Browse Issued/Returned rolls
*!*************************************************************
*! Calls     : lfwRollBrs
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrowRoll()
*!*************************************************************
FUNCTION lfBrowRoll

SELECT (lcTmpRolls)
BROWSE FIELDS cMarker =IIF(RECNO()=lnDMarker,'>',' '):H='':W=.F.,;
              crollid:H='Roll ID' :R,;
              NQTYBAL:H='Balance' :R ;
              WINDOW MATRNRL5   ;
              IN WINDOW MATRNRL4;
              NOMENU            ;         
              NOAPPEND          ;
              NODELETE          ;
              NOWAIT            ;
              SAVE              ;
              NOCLEAR           ;
              WHEN lfwRollBrs() ;
              TITLE lcRollTit   ;
              FOR nQtyBal>0
SELECT (lcTmpRoll2)
BROWSE FIELDS cMarker =IIF(RECNO()=lnRMarker,'>',' '):H='':W=.F.,;
              crollid:H='Roll ID':R,;
              NQTYBAL:H='Assigned':R ;
              WINDOW MATRNRL6   ;
              IN WINDOW MATRNRL4;
              NOMENU            ;         
              NOAPPEND          ;
              NODELETE          ;
              NOWAIT            ;
              SAVE              ;
              NOCLEAR           ;
              WHEN lfwAsRoll()  ;
              TITLE lcAsRollTit ;
              FOR NQTYBAL>0

*!*************************************************************
*! Name      : lfwRollBrs
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/05/2003
*! Purpose   : Show Available rolls
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwRollBrs()
*!*************************************************************
FUNCTION lfwRollBrs

lndMarker = RECNO(lcTmpRolls)
SHOW WINDOW (lcRollTit) REFRESH SAME
lcRollID = EVALUATE(lcTmpRolls+'.crollid')
SHOW GET lcRollID

*!*************************************************************
*! Name      : lfwAsRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/05/2003
*! Purpose   : Show Assigned rolls
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwAsRoll()
*!*************************************************************
FUNCTION lfwAsRoll

lnRMarker = RECNO(lcTmpRoll2)
SHOW WINDOW (lcAsRollTit) REFRESH SAME
lcRRollID = EVALUATE(lcTmpRoll2+'.crollid')
SHOW GET lcRRollID

*!*************************************************************
*! Name      : lfAssRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/05/2003
*! Purpose   : Assign Rolls
*!*************************************************************
*! Calls     : gfModalGen,lfRefresh,lfwRollBrs,lfwAsRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfAssRoll()
*!*************************************************************
FUNCTION lfAssRoll

IF EOF(lcTmpRolls)
  RETURN
ENDIF

SELECT (lcTmpRoll2)
APPEND BLANK
REPLACE CROLLID    WITH EVALUATE(lcTmpRolls+'.cRollid'),;
        cRollItem  WITH lcFabric,;
        Color      WITH lcColor,;
        CwareCode  WITH EVALUATE(lcTmpRolls+'.cWareCode'),;
        Dyelot     WITH EVALUATE(lcTmpRolls+'.Dyelot'),;
        Trancd     WITH '1',;
        Ctktno     WITH EVALUATE(lcTmpRolls+'.cTktNo'),;
        Csession   WITH EVALUATE(lcTmpRolls+'.cSession'),;
        Crsession  WITH EVALUATE(lcTmpRolls+'.cRsession'),;
        Cisession  WITH EVALUATE(lcTmpRolls+'.cIsession'),;
        NQTY       WITH NQTY    + EVALUATE(lcTmpRolls+'.nQtyBal') ,;
        Nqtybal    WITH Nqtybal + EVALUATE(lcTmpRolls+'.nQtyBal')

SELECT (lcTmpRolls)
lnTran    = lnTran + nQtyBal
lnBalance = lnBalance - nQtyBal
lnOld     = lnOld - nQtyBal
lnNew     = lnNew + nQtyBal
lnAdj     = lnAdj + nQtyBal

REPLACE Nqtybal WITH 0
LOCATE REST FOR NQTYBAL > 0
IF EOF()
  LOCATE FOR NQTYBAL > 0
ENDIF

=lfRefresh('MATRNRL8')
=lfwRollBrs() .AND. lfwAsRoll()

*!*************************************************************
*! Name      : lfRetRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/05/2003
*! Purpose   : Return Rolls
*!*************************************************************
*! Calls     : gfModalGen,lfRefresh,lfwRollBrs,lfwAsRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfRetRoll()
*!*************************************************************
FUNCTION lfRetRoll

IF EOF(lcTmpRoll2)
  RETURN
ENDIF

SELECT (lcTmpRolls)
=SEEK(lcRRollID)
REPLACE Nqtybal WITH EVALUATE(lcTmpRoll2+'.nQtyBal')

SELECT (lcTmpRoll2)
lnTran    = lnTran - nQtyBal
lnBalance = lnBalance + nQtyBal
lnOld     = lnOld + nQtyBal
lnNew     = lnNew - nQtyBal
lnAdj     = lnAdj - nQtyBal

REPLACE NQTYBAL WITH 0
DELETE

LOCATE REST FOR NQTYBAL > 0
IF EOF()
  LOCATE FOR NQTYBAL > 0
ENDIF

=lfRefresh('MATRNRL8')
=lfwRollBrs() .AND. lfwAsRoll()

*!*************************************************************
*! Name      : lfDMainLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/05/2003
*! Purpose   : Deactivate main screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDMainLot()
*!*************************************************************
FUNCTION lfDMainLot

IF WONTOP()= lcRollTit .OR. WONTOP()=lcAsRollTit
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ENTER   DO lfMainRoll
  ON KEY LABEL TAB     DO lpTabKey WITH 'MATRNRL7','pbNewRol'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'MATRNRL8','lcColor'
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfvRolSav
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/05/2003
*! Purpose   : Validate Roll screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRolSav()
*!*************************************************************
FUNCTION lfvRolSav

PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcTmpAdj)

*-- Assigen Fabric link code value to variable lcLinkCode
IF llGlLink
  IF llWareHous
    lcLinkCode = FabDye.GL_LINK
  ELSE
    lcLinkCode = Fabric.Link_Code
  ENDIF
ELSE
  lcLinkCode = ''
ENDIF  
IF EMPTY(lcLinkCode)
  lcLinkCode = 'DEFDEF'
ENDIF  

IF llModify
  *-- if modify mode update the TmpAj with new values  
  DO lpRepTemp
  CLEAR READ
ELSE
  *-- if new mode add new record im lcTmpAdj
  APPEND BLANK
  DO lpRepTemp
  =lfInit(.F.)
  SHOW GET pbSavRol   DISABLE
  STORE 0  TO lnTran,lnBalance,lnOnHand
  SELECT (lcTmpRolls)
  ZAP
  SELECT (lcTmpRoll2)
  ZAP
  =lfBrowRoll()
  IF !EOF("FABRIC")
    SKIP 1 IN FABRIC
    lcFabric  = Fabric.Fabric 
    lcColor   = SPACE(lnColorWid)    
  ENDIF    
  _CUROBJ = OBJNUM(lcFabric)
ENDIF
laScrmode[1] = .F.
laScrMode[4] = .T.
llCUpDate    = .T.
SHOW GETS
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfMainRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/05/2003
*! Purpose   : Assign/Return Rolls
*!*************************************************************
*! Calls     : lfAssRoll,lfRetRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfMainRoll()
*!*************************************************************
PROCEDURE lfMainRoll

IF WONTOP() = lcRollTit
  =lfAssRoll()
ELSE
  =lfRetRoll()
ENDIF

*!*************************************************************
*! Name      : lfGetNewId
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/05/2003
*! Purpose   : Create new roll id to return into it for another warehouse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: Roll id
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGetNewId(m.cRollid)
*!*************************************************************
FUNCTION lfGetNewId
PARAMETERS lcOldRoll

PRIVATE lnCount,lcNewRoll
lnCount = 0
lcNewRoll = lcOldRoll + STRTRAN(STR(lnCount,4,0),' ','0')
DO WHILE SEEK(lcNewRoll,'ROLLS')
  lnCount   = lnCount + 1
  lcNewRoll = lcOldRoll + STRTRAN(STR(lnCount,4,0),' ','0')
ENDDO
RETURN lcNewRoll

*!*************************************************************
*! Name      : lfReadAct
*! Developer : AHMED MAHER
*! Date      : 03/05/2003
*! Purpose   : READ Activate function
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

*!*************************************************************
*! Name      : lfClearKey
*! Developer : AHMED MAHER
*! Date      : 03/05/2003
*! Purpose   : Clear Hot Keys
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfClearKey()
*!*************************************************************
FUNCTION lfClearKey

ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER

*!*************************************************************
*! Name      : lfGetRolls
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/06/2003
*!*************************************************************
*! Purpose   : Get the available rolls.
*!*************************************************************
FUNCTION lfGetRolls
PARAMETERS llMode

PRIVATE lnAlias
lnAlias = SELECT(0)
*-- Get Rolls from Temp. files.
IF llMode
  SELECT (lcTmpRoll3)
  SET ORDER TO TAG (lcTmpRol33)
  IF SEEK(lcFabric+lcColor)
    SCAN REST WHILE CROLLITEM+COLOR+CROLLID = lcFabric+lcColor
      SCATTER MEMVAR
      INSERT INTO (lcTmpRolls) FROM MEMVAR
    ENDSCAN
  ENDIF
  SET ORDER TO TAG (lcTmpRoll3)
  SELECT (lcTmpRoll4)
  SET ORDER TO TAG (lcTmpRol44)
  IF SEEK(lcFabric+lcColor)
    SCAN REST WHILE CROLLITEM+COLOR+CROLLID = lcFabric+lcColor
      SCATTER MEMVAR
      INSERT INTO (lcTmpRoll2) FROM MEMVAR
    ENDSCAN
  ENDIF
  SET ORDER TO TAG (lcTmpRoll4)
ELSE
  IF SEEK(lcFabric+PADR(lcColor,6)+lcFromWare+lcDyelot,'ROLLS')
    SELECT ROLLS
    SCAN REST WHILE cRollItem+Color+cWareCode+Dyelot+cRollId+TranCd+cRsession =;
                    lcFabric+PADR(lcColor,6)+lcFromWare+lcDyelot;
                FOR TRANCD = '1' .AND. NQTYBAL > 0
      SCATTER MEMVAR
      INSERT INTO (lcTmpRolls) FROM MEMVAR
    ENDSCAN
    SELECT (lcTmpRolls)
    LOCATE
  ENDIF
  =lfRefresh('MATRNRL8')
  =lfwRollBrs() .AND. lfwAsRoll()
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lpTabKey
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/06/2003
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTabKey WITH 'lcWinCh32', 'm.Store',.T.
*!*************************************************************
PROCEDURE lpTabKey
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL TAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lpBackTab
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/06/2003
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
*! Name      : lfSavTmpRl
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/11/2003
*!*************************************************************
*! Purpose   : Save the temp. rolls files.
*!*************************************************************
FUNCTION lfSavTmpRl

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpRolls)
SCAN
  SCATTER MEMVAR
  SELECT (lcTmpRoll3)
  IF !SEEK(m.cRollId)
    APPEND BLANK
  ENDIF
  GATHER MEMVAR
ENDSCAN
SELECT (lcTmpRoll4)
SET ORDER TO TAG (lcTmpRol44)
IF SEEK(lcFabric+lcColor)
  DELETE REST WHILE CROLLITEM+COLOR+CROLLID = lcfabric+lcColor
ENDIF
SET ORDER TO TAG (lcTmpRoll4)
SELECT (lcTmpRoll2)
SCAN
  SCATTER MEMVAR
  INSERT INTO (lcTmpRoll4) FROM MEMVAR
ENDSCAN
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfSvRolls
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/11/2003
*!*************************************************************
*! Purpose   : Update the Rolls file.
*!*************************************************************
FUNCTION lfSvRolls

PRIVATE lnAlias
lnAlias = SELECT(0)
SET ORDER TO TAG ROLLS IN ROLLS
SELECT (lcTmpRoll4)
SCAN
  SCATTER MEMVAR
  SELECT ROLLS
  IF SEEK(m.cRollId)
    *-- Update the balance of the reciving record of the rolls
    REPLACE nQtyBal WITH 0
    
    *-- Add new issu record from the old warehouse
    APPEND BLANK
    GATHER MEMVAR
    REPLACE cSession  WITH lcGlSess;
            cISession WITH lcGlSess;
            TranCd    WITH "2"
  
    *-- Add new recive record to the new warehouse
    m.cwarecode = lcToWare
    m.crsession = lcGlSess
    m.csession  = lcGlSess
    m.cisession = SPACE(6)
    m.trancd    = '1'
    m.cRollId   = lfGetNewId(SUBSTR(m.cRollId,1,16))
    APPEND BLANK
    GATHER MEMVAR
  ENDIF
ENDSCAN
SET ORDER TO TAG ROLLITEM IN ROLLS
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfSvMatInv
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/12/2003
*!*************************************************************
*! Purpose   : Update the MatInvJl and fabdye files.
*!*************************************************************
FUNCTION lfSvMatInv
PARAMETERS lnMTotAdj,lnUnitCost,lcAdjAcct,lcMIcAcct

PRIVATE lnAlias,lnMPrvsQty,lnPrvsVal
lnAlias = SELECT(0)

IF SEEK(lcFab+lcClr+lcWare+lcDye,'FABDYE')
  SELECT FABDYE
  lnMPrvsQty = ONHAND
  lnPrvsVal  = NSTKVAL
  REPLACE ONHAND  WITH ONHAND - lnMTotAdj,;
          NSTKVAL WITH NSTKVAL - (lnMTotAdj*lnUnitcost)
ELSE
  STORE 0 TO lnMPrvsQty,lnPrvsVal
ENDIF

SELECT CRSESSION , SUM(NQTYBAL) AS NMTOTADJ;
       FROM (lcTmpRoll4);
       GROUP BY CRSESSION;
       WHERE CROLLITEM+COLOR+CROLLID = lcFab+lcClr;
       INTO CURSOR ROLLSLOTS

SELECT ROLLSLOTS
SCAN
  SELECT MATINVJL
  APPEND BLANK
  REPLACE CTRN_SEQ   WITH lcGlSess,;
          CRSESSION  WITH ROLLSLOTS.CRSESSION,;
          CISESSION  WITH lcGlSess,;
          CFABRIC    WITH lcFab,;
          CCOLOR     WITH lcClr,;
          CWARECODE  WITH lcWare,;
          CDYELOT    WITH lcDye,;
          DTRANDATE  WITH gdSysDate,;
          CTRANTYPE  WITH '2',;
          CTRAN      WITH lcGlSess,;
          NUNITCOST  WITH lnUnitCost,;
          NISSUED    WITH ROLLSLOTS.NMTOTADJ,;
          NUNTCSTBUY WITH lnUnitCost*FABRIC.CONV,;
          CGLMATADJ  WITH lcAdjAcct,;
          CMICACCT   WITH lcMIcAcct,;
          DPOSTDATE  WITH ldPost,;
          NSTKVAL    WITH -1 * ROLLSLOTS.NMTOTADJ * lnUnitCost,;
          NMPRVSQTY  WITH lnMPrvsQty,;
          NPRVSVAL   WITH lnPrvsVal
  lnMPrvsQty = lnMPrvsQty - ROLLSLOTS.NMTOTADJ
  lnPrvsVal  = lnMPrvsQty * lnUnitCost
  =gfAdd_Info('MATINVJL')
ENDSCAN
USE IN ROLLSLOTS

IF SEEK(lcFab+lcClr+lcToWare+lcDye,'FABDYE')
  SELECT FABDYE
  lnMPrvsQty = ONHAND
  lnPrvsVal  = NSTKVAL
  REPLACE ONHAND  WITH ONHAND + lnMTotAdj,;
          NSTKVAL WITH NSTKVAL + (lnMTotAdj*lnUnitcost)
ELSE
  STORE 0 TO lnMPrvsQty,lnPrvsVal
ENDIF

SELECT MATINVJL
APPEND BLANK
REPLACE CTRN_SEQ   WITH lcGlSess,;
        CRSESSION  WITH lcGlSess,;
        CFABRIC    WITH lcFab,;
        CCOLOR     WITH lcClr,;
        CWARECODE  WITH lcToWare,;
        CDYELOT    WITH lcDye,;
        DTRANDATE  WITH gdSysDate,;
        CTRANTYPE  WITH '2',;
        CTRAN      WITH lcGlSess,;
        NUNITCOST  WITH lnUnitCost,;
        NRECEIVED  WITH lnMTotAdj,;
        NUNTCSTBUY WITH lnUnitCost*FABRIC.CONV,;
        CGLMATADJ  WITH lcAdjAcct,;
        CMICACCT   WITH lcMIcAcct,;
        DPOSTDATE  WITH ldPost,;
        NSTKVAL    WITH lnMTotAdj * lnUnitCost,;
        NMPRVSQTY  WITH lnMPrvsQty,;
        NPRVSVAL   WITH lnPrvsVal
=gfAdd_Info('MATINVJL')
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfGlDist
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/13/2003
*!*************************************************************
*! Purpose   : Update the GlDist file.
*!*************************************************************
FUNCTION lfGlDist

PRIVATE lnAlias
lnAlias = SELECT(0)
IF llGlLink
  =SEEK(lcFab+lcClr+lcWare+lcDye,'FABDYE')
  lcLinkCode = IIF(!EMPTY(FABDYE.GL_LINK),FABDYE.GL_LINK,'DEFDEF')
  lnGLEnAmount = -m.nMTotAdj * m.UnitCost
  
  DO GLDIST WITH lcLinkCode,'015',lnGlEnAmount,'MA','',ldPost,m.GlFYear,m.GlPeriod,;
                 lctmpGlDis,'','','',''
  
  =SEEK(lcFab+lcClr+lcToWare+lcDye,'FABDYE')
  lcLinkCode = IIF(!EMPTY(FABDYE.GL_LINK),FABDYE.GL_LINK,'DEFDEF')
  lnGLEnAmount = m.nMTotAdj * m.UnitCost
  
  DO GLDIST WITH lcLinkCode,'015',lnGlEnAmount,'MA','',ldPost,m.GlFYear,m.GlPeriod,;
                 lctmpGlDis,'','','',''
ENDIF
SELECT(lnAlias)