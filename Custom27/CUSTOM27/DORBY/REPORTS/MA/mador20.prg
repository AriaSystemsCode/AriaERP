*!*************************************************************
*! Program File : MADOR20
*! Program Desc : Customized material inventory adjustments for Dorby Frocks.
*! For Screen   : MAINVCT.SPR
*!       System : Aria Apparel Series(A27)
*!       Module : MATERIAL Module - C101511
*! Developer    : Hossam El Etreby[HDM]
*!*************************************************************
*! Passed :
*!        Parameters : 
*!           lnChoice : To endicate the program mode, which will be
*!                      one of the following numbers :
*!                1)  : Adjustments. 
*!                2)  : Physical inventory.
*!                3)  : Transfer inventory.
*!**********************************************************************
*! Example :
*!        DO MADOR20 WITH 1
*!**********************************************************************
PARAMETERS lnChoice

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DECLARE ladata[01],laKeyField[1,4],laAddress[6,3],laDefProc[10],;
        laVariables[1]

STORE '' TO laData,laKeyField,laAddress,lcWinCh1,lcWinCh2,lcWinCh3,lcWinCh4,lcModal,lcTmpAdj,;
            lctmpRolls ,lcFromWare,lcToWare,;
            lcFrmDesc , lcToDesc,lcToWare,lcTemLoc,lctmpGlDis,lcAdjReason,lcAdjAcct,;
            lcGlFYear ,lcGlPeriod 

STORE .F. TO llScrMode
STORE {} TO ldPost

*-- To allow the program to enter the lpShow procedure at the calling time
llNoShow   = .F.

*-- Get the old trap of Esc Key
lcOldEscTrap = ON('Key','Esc')

*-- Allow the program to have its local Save and local Close
laDefProc[9]  = .F.
laDefProc[10] = .F.

*-- Array used for the Adj. Code
DIMENSION  laAdjCode[1] , laCodInfo [1,10]
STORE "" TO laAdjCode, laCodInfo

*-- Give the user a message if the program is called form the 
*-- transfer option and the system is single warehouse,then return

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
llWareLoc  = laSetUps[4,2] = 'Y'
llTrkRolls = laSetUps[5,2] = 'Y'
lcMtCstMth = laSetUps[6,2]

lcBaseFile = ''

*-- Convetrting lcChhoice to numeric and
*-- then define lctype as character
lcType     = SUBSTR('APT',lnChoice,1)

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

*-- Initialize the contents of pbs to be close untill the key is completed
lcPromp      = lcCloseP
lcDetP       = lcCloseP

*-- Initialize the contents of pbs 
*-- to be New and remove in case of (+)ve adj. and llTrkRolls
lcRollNP     = lcNewP
lcRollRP     = lcRemveP
lnColorWid   = (06)

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
lcWareAny    = IIF(llWareHous, lcFromWare, qWareHouse)


*-- Browse title of the mainbrowse of items details (screen Mat400_2)
lcBrowTtl    = "Transaction's lines"

*-- Initialize the llDyeLvl to be false and then it will be computed
*-- due to the setup of the system and each item setup
llDyeLvl     = .F.

*-- Initialize the llModify to be false as it is new mode
*-- then change to true if in modify mode
llModify     = .F.
*-- Variable tho check if the temp. file of rolls is empty
llEmpTmpRo   = .F.

*-- Title of the screen Mainvct4
lcLineT      = SPACE(00)

*-- Initialize the variable of screen Mainvct4
ldCurrDate   = {}
lnAdj        = 000
lnOld        = 000
lnNew        = 000
lnToStk      = 000
lnNewCost    = 000
lnOldCost    = 000
lnNewRec     = 000
lcFabric     = SPACE(07)
lcOldFab     = SPACE(07) 
lcColor      = SPACE(06)
lcDyelot     = SPACE(10)
lcDesc       = SPACE(20)
lcReason     = SPACE(25)

*-- Initialize the variable to be used for the browse of lots or rolls
lnLotAmont   = 0
lnAvailStk   = 000
lnApplDQty   = 000  
llAddRolls   = .T.  
lnOldVal     = 0

*-- Initialize the variable of Title of rolls and lots browse
lcRollTitl   = Space(25)

*-- Define the size of box in main screen 
lnCenBoxY    = IIF(llWareHous, IIF(lnChoice = 3, 4, 3), 0) 
lnCenBoxS    = 18 - lnCenBoxY

*-- Initialize the title of screen Mainvct
lcWinTitle   = ALLTRIM(SUBSTR( PADR("Material Inventory Adjustment",60) +;
                               PADR("Material Physical Inventory"  ,60) +;
                               PADR("Material Inventory Transfer"  ,60) ,;
                               ((lnChoice-1)*60)+1,60))

*-- Set the required oreders for the files
SELECT ROLLS
SET ORDER TO TAG ROLLITEM

SELECT FABDYE
SET ORDER TO TAG FABDYE

SELECT FABRIC
SET ORDER TO TAG FABRIC

IF !WEXIST(gcBaseWind)
  IF lnChoice = 3 
    IF !llWareHous
      *-- B602114 AAMER 11/03/98 (START)
      *-- Ending the program if it is single warehouse and Transfer inventory
      glQuitting = .T.
      *-- B602114 AAMER 11/03/98 (End)

      =gfModalGen('QRM36046B36000','ALERT')
      RETURN
    ENDIF  
  ELSE  
    *-- In case of Phys. or Adj. ,check if there is any Adj. Reason code already defined ,
    *-- if not give the user a message and return

    lnAlias = SELECT()
    SELECT CODES
    lcCodTag=TAG()
    SET ORDER TO TAG cCode_no

    *-- According to removing comp_id from codes file (Start)
    *IF !SEEK(gcAct_comp+'CADJREASON','CODES')
    IF !SEEK('N'+'CADJREASON','CODES')
    *-- According to removing comp_id from codes file (End)
      *--You have to edit the Adjustment reasons codes first, Cannot proceed.
      =gfModalGen('TRM42111B42001','DIALOG')
      
      SET ORDER TO TAG &lcCodTag
      SELECT(lnAlias)

      *-- Ending the program (START)
      *-- AAMER   09/23/98 
      *-- Terminate the program if there is no adjust. reasons codes
      *-- in codes file
      glQuitting = .T.
      RETURN
      *-- Ending the program (END)
    ENDIF  
    SET ORDER TO TAG &lcCodTag
    SELECT(lnAlias)
  ENDIF

  *-- Define the child windows 
  lcWinCh1 = gfTempName()
  lcWinCh2 = gfTempName()
  lcWinCh3 = gfTempName()
  lcWinCh4 = gfTempName()

  *-- Variable that hold a trace code in file dyelot relation
  *-- for dyelot ordering
  lcTmpDyRel = gfTempName()

  IF llGlLink
    *-- OPEN GLDIST FILE TO CALL GENERAL LEDGER DISTRIBUTION PROCEDURE 
    SELECT GLDIST

    *-- OPEN A TEMP FILE TO BE USED IN CALLING 'GLDIST' PROCEDURE.
    lctmpGlDis = gfTempName()
    COPY STRUCTURE TO &gcWorkDir.&lctmpGlDis

    SELECT 0
    USE (gcWorkDir+lctmpGlDis) EXCLUSIVE
  ENDIF  
  
  *-- If the system is setup to use the warehose locations.
  IF llWareLoc 
    DIMENSION laSource[1],laTarget[1]
    SELECT WhsLoc
    = AFIELDS(laStru)
    lcTemLoc  = gfTempName()
    CREATE CURSOR (lcTemLoc) FROM ARRAY laStru
    INDEX ON STYLE+COLOR+CWARECODE+CLOCATION TAG (lcTemLoc) OF (gcWorkDir+lcTemLoc)
  ENDIF  

  *-- If the system is setup to use the rolls.
  IF llTrkRolls .OR. lcMtCstMth $ "LFI"
    SELECT ROLLS
    lctmpRolls = gfTempName()
  ENDIF

  *-- Temp. File to have the transaction lines 
  lcTmpAdj   = gfTempName()

  *-- Openning The Files Used And Creating The Temp. Cursor 
  *-- which contains details of each item in the transaction
  SELECT DFINVADJ
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

  *E301089 (Start)
  lnNewFld = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnNewFld,4]
  laFileStru[lnNewFld,1] = 'NewQty'
  laFileStru[lnNewFld,2] = 'N'
  *E301089 (Start)
  *laFileStru[lnNewFld,3] = 8
  *laFileStru[lnNewFld,4] = 0
  laFileStru[lnNewFld,3] = 12
  laFileStru[lnNewFld,4] = 3
  *E301089 (End)

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


  *-- Add field for Material inventory control account
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

ENDIF
* --------------------------------------------------------------

*-- Calling of the main screen MAINVCT
PUSH KEY
= lfTrapKeys()
DO (gcScrDir+gcWinAppl+'\MAINVCT.SPX')
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
  
  IF llWareLoc 
    USE IN (lcTemLoc)
    ERASE (gcWorkDir+lcTemLoc+'.DBF')
    ERASE (gcWorkDir+lcTemLoc+'.CDX') 
    ERASE (gcWorkDir+lcTemLoc+'.FPT') 
  ENDIF
ENDIF
*!*************************************************************
*! Name        : lpClsScr
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : To do the validation of pbClose at the main
*!               screen and be sure that the user want to finish
*!               this program and Zap tmp.files and clear read
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Screen : Mat400_3
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls       : 
*!               Functions  : gfDialog
*!*************************************************************
PROCEDURE lpClsScr

*-- Zap the adjustment temp. file.
SELECT (lcTmpAdj)
ZAP
*-- Zap the GL temp. file.
IF llGlLink
  *--E301077,66 (Start)
  *= gfOpenFile (gcWorkDir+lctmpGlDis,"","EX")
  *--E301077,66 (End)
  ZAP
ENDIF
SHOW GETS
*!*************************************************************
*! Name       : lfActBrow
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose     : To define the header of the main browse for each
*!               type and browse the fields of each item
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Screen : Mat400_2
*!       Function : lfvWare
*!                  lfvRemove
*!                  lpSvInMstr
*!*************************************************************
FUNCTION lfActBrow

SELECT (lcTmpAdj)
*-- Basic fields in every case of lctype
lcBasBrow  = "cMarker:H = ' ',Fabric:H='Item', Color:H='Color', Desc:H='Description':8"
lcDatBrow  = "Date :H='Date'"
lcDyeBrow  = IIF(llDyelot,",Dyelot :H='Dyelot',",",")

*-- Fields of browse case lctype = 'A'
lcABrow    = "OldQty:H='Existing', nMTotAdj:H='Adj(+/-)', NewQty:H='TotQty'"
*-- Fields of browse case lctype = 'P'
lcPBrow    = "OldQty:H='Existing', nMTotAdj:H='New Inv.', nMTotAdj:H='TotQty'"
*-- Fields of browse case lctype = 'T'
lcTBrow    = "OldQty:H='&lcFromWare', nMTotAdj :H='Transfer', NewQty :H='&lcToWare'"

lcToUse    = "lc"+lcType+"Brow"
lcEndBrow  = &lcToUse

lcBrFields =  lcBasBrow+lcDyeBrow+lcDatBrow+","+lcEndBrow

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
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose    : To do the trapping between the 3
*!               screens Mat400_1, Mat400_ 2, Mat400_3
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Program nMat400 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedures : lpTab , lpShiftTab, lpEsc
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
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose    : To do the trapping of the tab key
*!               between the 3
*!               screens Mat400_1, Mat400_ 2, Mat400_3
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Function : lfTrapKeys
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedure : lpTab 
*!*************************************************************
PROCEDURE lpTab

ON KEY LABEL TAB lnDummy = 1

DO CASE
  CASE WONTOP('lcWinCh1') .AND. _CUROBJ = IIF(llGlLink,OBJNUM(ldPost),IIF(lnChoice<3,OBJNUM(lcFromWare),OBJNUM(lcToWare)))
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
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose    : To do the trapping of the back tab 
*!              between the 3
*!               screens Mat400_1, Mat400_ 2, Mat400_3
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Function : lfTrapKeys
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedure : lpShiftTab
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*!*************************************************************
PROCEDURE lpShiftTab

ON KEY LABEL BACKTAB lnDummy = 1
DO CASE
  CASE WONTOP('lcWinCh1') .AND. _CUROBJ = OBJNUM(ldPost)
    ACTIVATE WINDOW ('gwcContrl1')
   _CUROBJ = OBJNUM(pbCls)

  CASE WONTOP(lcBrowTtl) 
    IF llWareHous AND lcPromp = lcCloseP
      ACTIVATE WINDOW lcWinCh1
      _CUROBJ = IIF(lnChoice < 3, OBJNUM(lcFromWare), OBJNUM(lcToWare))
    ELSE
      ACTIVATE WINDOW lcWinCh3
      _CUROBJ = OBJNUM(pbClose)
    ENDIF

  CASE WONTOP('lcWinCh3') .AND. _CUROBJ = OBJNUM(pbNew)
    ACTIVATE WINDOW (lcBrowTtl) 

  CASE WONTOP('lcWinCh3') .AND. _CUROBJ = OBJNUM(pbClose)
    IF lcPromp = lcCloseP
      ACTIVATE WINDOW (lcBrowTtl) 
    ELSE
      _CUROBJ = OBJNUM(pbUpdate)
    ENDIF
  CASE WONTOP('lcWinch8') .AND. _CUROBJ = OBJNUM(pbNew)
     ACTIVATE WINDOW (lcWinch6)
  OTHERWISE
    _CUROBJ = _CUROBJ - 1
ENDCASE
ON KEY LABEL BACKTAB DO lpShiftTab

*!*************************************************************
*! Name      : lpEsc
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Trapping the Escape button.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Function : lfTrapKeys
*!*************************************************************
PROCEDURE lpEsc

ACTIVATE WINDOW ('gwcContrl1')
_CUROBJ = OBJNUM(pbCls)
KEYBOARD CHR(13)

*!*************************************************************
*! Name      : lfvWare
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : To validate the warehouses codes.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Screen : Mat400_1
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Functions : lfRefresh, lfActBrow, gfBrowWare
*!*******************************************************"*****
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
IF !EMPTY(lcFromWare+IIF(lnChoice=3,lcToWare,""))
  IF lcFromWare = lcToWare
    = gfModalGen('QRM36047B36000','ALERT',IIF(llTo, "to","from"))
    _CUROBJ      = _CUROBJ
    &lcPrmWrCod  = SPACE(06)
    &lcToUpdate  = SPACE(0)
  ELSE
  
  *-- if the key(s) is not empty, then disable the key object(s) 
  *-- and enable the rest of the objects.
    IF !EMPTY(lcFromWare) AND IIF(lnChoice=3,!EMPTY(lcToWare),.T.)
      lcPromp   = lcCanclP
      SHOW GET pbClose,1 PROMPT lcPromp
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
IF lnChoice=3 AND !EMPTY(&lcPrmWrCod) 
  llNothing = lfActBrow()
ENDIF
llNothing = lfRefresh()
SHOW GET &lcPrmWrCod 
laScrMode[1] = IIF(EMPTY(&lcPrmWrCod ),.T.,.F.)
laScrMode[4] = IIF(!EMPTY(&lcPrmWrCod ),.T.,.F.)
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfBroKey
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Validate the browse invisible buttons for the 
*!             warehouses codes.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Screen : Mainvct1
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Functions : lfvWare
*!*************************************************************
FUNCTION lfBroKey

lcCurWare  = "LC"+IIF(SUBSTR(SYS(18),3,1)="F","FROM","TO")+"WARE"
&lcCurWare = PADR("?",6)
llNothing  = lfvWare(lcCurWare)
_CUROBJ    = OBJNUM(&lcCurWare)

*!*************************************************************
*! Name      : lfvNew
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : To call the screen to enter the details for
*!             a line in the main browse.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Screen : Mat400_3 (pbNew)
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Screen   : Mat400_4
*!         Function : lfInit
*!*************************************************************
FUNCTION lfvNew
PRIVATE lnResponse
lnResponse = gfModalGen('QRM36136B36001','DIALOG')
IF lnResponse = 1
  SELECT DFINVADJ
  GO TOP
  IF EOF('DFINVADJ')
    =gfModalGen('TRM00189B00000','DIALOG','Adjustments')
    RETURN
  ENDIF
  llScrMode = .T.
  llModify  = .T.
  *-- Screen title in case of adding a new transaction.
  lcLineT    = " Transaction Line editing Screen "
  *-- Case of new record initialize all the varaibles
  cbLocation = .F.
  lcLocStat  = "DISABLE"
  lcFabStat  = 'ENABLE'
  lcDatStat  = 'DISABLE'
  =lfInit(.F.)
ELSE
  llScrMode = .F.
  *-- Screen title in case of adding a new transaction.
  lcLineT    = " Transaction Line Entry Screen "
  *-- Case of new record initialize all the varaibles
  cbLocation = .F.
  lcLocStat  = "DISABLE"
  llModify   = .F.
  lcFabStat  = 'ENABLE'
  lcDatStat  = 'DISABLE'
  =lfInit(.F.)

ENDIF
*-- Call the screen to enter the detail for each item
PUSH KEY
ON KEY
= lfTrapKD()
DO (gcScrDir+gcWinAppl+'\MAINVCT4.SPX')
POP KEY
*!*************************************************************
*! Name      : lfwFabric
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Get the fabric code in the line entery screen.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Screen : Mat400_4 (lcFabric)
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function : lfwOldVals
*!*************************************************************
FUNCTION lfwFabric

lcOldFab    = lcFabric

*!*************************************************************
*! Name      : lfvFabric
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Validate the fabric code in the line entery screen.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Screen : Mat400_4 (lcFabric)
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedure : gpWareFabs, FaBrow
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias , llRetValue

lnAlias = SELECT()
IF llScrMode
  SELECT DFINVADJ
  SET ORDER TO TAG Dfinvadj
  *!EMPTY(lcFabric) AND 
  IF !SEEK(lcFromWare + lcFabric)
    DIMENSION laRetFld[1]
    STORE '' TO laRetFld
    lcBrFields = "FABRIC :H='Fabric',FABRIC.DESC:H='Description',nfUnitCost:H='Unit Cost',lcItmType = gfCodDes(FABRIC.item_Type,'ITEM_TYPE'):h='Type',"+;
                 "FABRIC.VENDOR :H='Vendor',FABRIC.Pattern :H='Pattern'"
    SELECT DFINVADJ
    SET ORDER TO TAG FABRIC
    SET RELATION TO Dfinvadj.fabric + Dfinvadj.color INTO Fabric ADDITIVE
    IF gfBrows('FOR CFROMWARE = lcFromWare','FABRIC','laRetFld')
      lcFabric = laRetFld[1]
    ELSE
      lcFabric = ''
      _CUROBJ = OBJNUM(lcFabric)
      *RETURN
    ENDIF
    SET RELATION TO
    SET ORDER TO TAG DFINVADJ
  ENDIF
ELSE
  SELECT Fabric
  IF !EMPTY(lcFabric) AND !SEEK(lcFabric)
    DO FaBrow WITH lcFabric,'*'
  ENDIF

  *-- Initialize the Color and Dyelot variables
  lcColor   = IIF(!EMPTY(lcFabric),IIF(!EMPTY(lcOldFab) AND lcFabric <> lcOldFab,SPACE(lnColorWid),lcColor),SPACE(lnColorWid))
  llNothing = IIF(!EMPTY(lcColor),lfvColor(),.T.)
  lcDyelot  = SPACE(10)
  lcOldFab  = SPACE(07)
  SHOW GET lcDyelot
  IF lcType $ 'AP' AND llGlLink
    lcAdjReason = laAdjCode[lnAdjCode,2]
  ENDIF  
  SELECT(lnAlias)
ENDIF



*!*************************************************************
*! Name      : lfvColor
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Validate the entry of a fabric color
*!             code and display the browse if it is not found in 
*!             the Fabric file. Check if the Fabric + Color has 
*!             Dyelot or not.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Screen : Mat400_4 (lcColor)
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedure : FaBrow
*!         Functions : lfGtOldCst, lfFCDWCheck 
*!                     lfDetRef,lfInit, lfvlnOld
*!*************************************************************
FUNCTION lfvColor
PRIVATE lnAlias

lnAlias = SELECT()

IF llScrMode
  DIMENSION laRetFld[1]
  STORE '' TO laRetFld
  SELECT DFINVADJ
  SET ORDER TO TAG DFINVADJ
  IF !SEEK(lcFromWare + lcFabric+lcColor)
    lcBrFields = "FABRIC :H='Fabric' , COLOR :H='Color' , FABRIC.DESC:H='Description',nMTotAdj:H='Adj. Qty.',nfUnitCost:H='Unit Cost',Date:H='Date',lcItmType = gfCodDes(FABRIC.item_Type,'ITEM_TYPE'):h='Type',"+;
               "FABRIC.VENDOR :H='Vendor',FABRIC.Pattern :H='Pattern'"
    IF gfBrows('FOR CFROMWARE + FABRIC = lcFromWare + lcFabric','COLOR','laRetFld')
      lcColor = laRetFld[1]
    ELSE
      lcColor = ''
      _CUROBJ = OBJNUM(lcColor)
      RETURN
    ENDIF
  ENDIF
  IF !SEEK(LCFABRIC+LCCOLOR,LCTMPADJ)
    SCATTER MEMVAR MEMO
    SELECT (lcTmpAdj)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ELSE
    = gfModalGen('TRM36137B00000','DIALOG',lcFabric + '/' + lcColor)
    =lfInit(.F.)
    _CUROBJ = OBJNUM(lcFabric)
    RETURN
  ENDIF
  
  =lfInit(.T.)
  lcDetP     = lcCanclP
  SHOW GET pbCancel,1 PROMPT lcDetP

  SHOW GET lcFabric   DISABLE
  SHOW GET lcColor    DISABLE
  SHOW GETS
ELSE
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
  
  IF SEEK(LCFABRIC+LCCOLOR,LCTMPADJ)
    = gfModalGen('TRM36137B00000','DIALOG',lcFabric + '/' + lcColor)
    =lfInit(.F.)
    _CUROBJ = OBJNUM(lcFabric)
    RETURN
  ENDIF

  *-- Initalize the dyelot variable.
  lcDyelot = SPACE(10)
  *-- If empty either fabric or color 
  *-- the push botton Cancel will be close.
  IF EMPTY(lcFabric) OR EMPTY(lcColor)
    llDyeLvl  = .F.
    lcDetP    = lcCloseP
    SHOW GET pbCancel,1 PROMPT lcDetP
  ELSE
    *-- If !empty(fabric) and !empty(color)
    *-- get the old cost of the fabric + color
    *-- and check if it has dyelot or not
    *-- set the prompt of the pb. cancel to be cancel
    llNoting   = lfGtOldCst()
    lcDetP     = lcCanclP
    lcDesc     = Fabric.Desc
    llNoThing  = lfDetRef()
    llDyeLvl   = (Fabric.cDye_Flg = "Y" AND llDyelot)
    *-- If the system is set to check on warehouse locations then
    *-- mark/unmark the location "check box" if there is any 
    *-- location assigned to the selected fabric+color.
    IF llWareLoc
      cbLocation = SEEK(PADR(lcFabric,19)+lcColor+lcFromWare,"WhsLoc") OR ;
                   SEEK(PADR(lcFabric,19)+lcColor+lcFromWare,lcTemLoc)
      lcLocStat  = "ENABLE"
      SHOW GET cbLocation &lcLocStat
    ENDIF  
    *-- If the system is not using dyelots. or the fabric does 
    *-- not use dyelots.
    IF !llDyeLvl
      *-- Check if (Fabric + color) found in warehouse or not
      *-- if !found() and the user wants the enter a new(fabric + color)
      *-- initialize the variables with new
      IF lfFCDWCheck()
        llNoThing = lfInit(.F.)
        _CUROBJ   = OBJNUM(lcFabric)
      ELSE
        ldCurrDate = gdSysDate
        SHOW GET pbUpdate   ENABLE
        SHOW GET lcFabric   DISABLE
        SHOW GET lcColor    DISABLE
        SHOW GET ibFabric   DISABLE
        SHOW GET ibColor    DISABLE
        SHOW GET ldCurrDate ENABLE
        SHOW GET lcReason   ENABLE
        SHOW GET lnOld      DISABLE
        SHOW GET lnNew      ENABLE
        SHOW GET lnAdj      ENABLE
        SHOW GET lnNewCost  ENABLE
        SHOW GET lnAdjCode  ENABLE
        SHOW GET pbCancel,1 PROMPT lcDetP
        llNoThing = lfvlnOld()
      ENDIF  
      SHOW GET lcDyelot DISABLE
      *-- get the old stock of the (Fabric + Color)
    ELSE    
      *-- If the system and the fabric entered use Dyelots, disable
      *-- the key objects. The warehouse validation will be at the 
      *-- dyelot level.
      SHOW GET pbCancel,1 PROMPT lcDetP
      SHOW GET lcFabric   DISABLE
      SHOW GET lcColor    DISABLE
      SHOW GET lcDyelot   ENABLE
      SHOW GET ibFabric   DISABLE
      SHOW GET ibColor    DISABLE
      SHOW GET ibDyelot   ENABLE    
    ENDIF    
  ENDIF
ENDIF
llNoThing = lfDetRef()
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvDyelot
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Validate the dyelot code.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Screen : Mat400_4 (lcDyelot)
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedure : gpFbDyBrow
*!         Functions : lfvlnOld, lfFCDWCheck, lfDetRef,lfInit
*!*************************************************************
FUNCTION lfvDyelot
PRIVATE lnAlias

lnAlias = SELECT()
SELECT FabDye

PRIVATE lcOldDye
lcOldDye   = lcDyelot
lcOldColor = lcColor
lcOldFab   = lcFabric

IF EMPTY(lcDyelot) OR "?" $ lcDyelot
  DO gpFbDyBrow WITH lcFabric,lcColor,lcDyelot,lcFromWare
  IF EMPTY(lcDyelot)
    lcDyelot = IIF("?" $ lcOldDye,lcDyelot,lcOldDye)
    lcColor  = lcOldColor
    lcFabric = lcOldFab
    IF EMPTY(lcDyelot)
      =gfModalGen('TRM36094B36000','ALERT','dyelot')
      _CUROBJ = OBJNUM(lcDyelot)
    ENDIF
  ENDIF
ENDIF
*--E301135 (End)    

IF !EMPTY(lcDyelot) &&&&&  AND LASTKEY() = 13
  *-- Check if (Fabric + Color + Dyelot) found in warehouse or not
  *-- if !found() and the user wants to enter a new(fabric + color)
  *-- initialize the variables with new
  IF lfFCDWCheck()
    llNoThing = lfInit(.F.)
    _CUROBJ   = OBJNUM(lcFabric)
  ELSE
    ldCurrDate = DATE()
    SHOW GET pbUpdate    ENABLE
    SHOW GET  lcDyelot   DISABLE
    SHOW GET  ibDyelot   DISABLE
    SHOW GET  ldCurrDate ENABLE
    SHOW GET  lcReason   ENABLE
    SHOW GET  lnOld      DISABLE
    SHOW GET  lnNew      ENABLE
    SHOW GET  lnAdj      ENABLE
    SHOW GET  lnAdjCode  ENABLE    
    SHOW GET  lnOldCost  ENABLE
    SHOW GET  lnNewCost  ENABLE
    llNoThing   = lfvlnOld()
  ENDIF
ENDIF


llNoThing = lfDetRef()
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvCancel
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : To close the line entery screen after saving the 
*!             information , Or to cancel the information that 
*!             were taken in it .
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Screen : Mat400_4 (pbCancel)
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Functions : lfInit
*!*************************************************************
FUNCTION lfvCancel
PRIVATE lcMessage 

*-- The push button will be "CLOSE" if the key fields are empty, 
*-- otherwise it will be "CANCEL"
*-- If the the p.b. is "Cancel"
IF lcDetP = lcCanclP
  lcMessage = "Are you sure ? You will lose all changes"
  IF gfModalGen('QRM36048B36001','ALERT') = 1
    IF llModify
      *--Check If Nothing Changed
        SELECT DFINVADJ
        IF SEEK(&lcTmpAdj..cfromware + &lcTmpAdj..fabric + &lcTmpAdj..color + &lcTmpAdj..ctrn_seq)
          IF DFINVADJ.NMTOTADJ = &lcTmpAdj..NMTOTADJ AND DFINVADJ.NFUNITCOST = &lcTmpAdj..NFUNITCOST
            SELECT (lcTmpAdj)
            DELETE
          ENDIF
        ENDIF
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

*!*************************************************************
*! Name      : lfInit
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : To initialize the variables used in the line 
*!             entery screen either from the lcTmpAdj file or with 
*!             new values.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Passed :
*!        Parameters : 
*!            llMode : To tell if we are going to initialize
*!                     the variables from the used temp file 
*!                     or with empty values.
*!                     .T. : Initialize from the temp file.
*!                     .F. : Initialize empty values.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Functions : lfvNew, lfvColor, lfvDyelot
*!                     lfvCancel, lfvlnOld, lfvpbUpdate
*!                     lfvModify
*!*************************************************************
FUNCTION lfInit
PARAMETERS llMode
PRIVATE lcOldFabOr
*-- Initialize the variables from the file.

IF llMode
  
  lcFabric   = &lcTmpAdj..Fabric  
  lcColor    = &lcTmpAdj..Color
  =SEEK(lcFabric+lcColor,'FABRIC')
  lcDyelot   = &lcTmpAdj..Dyelot
  lcDesc     = &lcTmpAdj..Desc
  ldCurrDate = &lcTmpAdj..Date
  lcReason   = &lcTmpAdj..cReason
  lnOld      = &lcTmpAdj..OldQty
  lnNew      = IIF(llScrMode, &lcTmpAdj..nMtotAdj + &lcTmpAdj..oldqty,&lcTmpAdj..NewQty)
  lnToStk    = &lcTmpAdj..TotStk
  lnAdj      = &lcTmpAdj..nMtotAdj
  lcOldFabOr = ORDER('FABRIC')
  SET ORDER TO TAG FABRIC IN FABRIC
  IF SEEK(lcFabric+lcColor,'FABRIC')
    lnOldCost = IIF(lnOldCost=0,Fabric.costBuy/Fabric.Conv,lnOldCost)
  ENDIF
  SET ORDER TO TAG &lcOldFabOr IN FABRIC
  *lnOldCost  = &lcTmpAdj..nOldCost
  
  lnNewCost  = &lcTmpAdj..NFUnitCost
  
  =gfwCodePop(@laCodInfo, "CADJREASON" , "T")
  IF llWareLoc
    cbLocation = SEEK(PADR(lcFabric,19)+lcColor+lcFromWare,"WhsLoc") OR ;
                 SEEK(PADR(lcFabric,19)+lcColor+lcFromWare,lcTemLoc)
    SHOW GET cbLocation ENABLE
  ENDIF  
  SHOW GET lcFabric   &lcFabStat
  SHOW GET lcColor    &lcFabStat
  SHOW GET lcDyelot   DISABLE
  SHOW GET ibFabric   DISABLE
  SHOW GET ibColor    DISABLE
  SHOW GET ibDyelot   DISABLE
  SHOW GET lcReason   ENABLE
  SHOW GET ldCurrDate ENABLE
  SHOW GET lnNew      ENABLE
  SHOW GET lnAdj      ENABLE
  SHOW GET lnAdjCode  ENABLE
  SHOW GET lnNewCost  ENABLE
  SHOW GET pbUpdate   ENABLE
ELSE
  *-- Empty the variables.
  IF lnChoice <> 3 
    =gfwCodePop(@laCodInfo, "CADJREASON" , "D")
    lnAdjCode  = 1
    lcAdjReason= laAdjCode[lnAdjCode,2]
    DECLARE laTrmRltFd[1,2]
    laTrmRltFd[1,1] = 'GLACCOUNT'
    laTrmRltFd[1,2] = 'lcAdjAcct'
    =gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
  ENDIF
  lcFabric   = SPACE(07)
  lcColor    = SPACE(lnColorWid)
  llDyeLvl   = .F.
  lcDyelot   = SPACE(10)
  lcDesc     = SPACE(20)
  ldCurrDate = {}
  lnOld      = 0
  lnNew      = 0
  lnToStk    = 0
  lnAdj      = 0
  lnOldCost  = 0
  lnNewCost  = 0
  cbLocation = .F.
  lcDetP     = lcCloseP
  llnothing  = IIF(WEXIST("lcWinCh4"),lfDetRef(),lfRefresh())
  SHOW GET cbLocation DISABLE
  SHOW GET lcFabric   &lcFabStat
  SHOW GET lcColor    &lcFabStat
  SHOW GET lcDyelot   DISABLE
  SHOW GET ibFabric   ENABLE
  SHOW GET ibColor    ENABLE
  SHOW GET ibDyelot   DISABLE
  SHOW GET lcReason   DISABLE
  SHOW GET ldCurrDate DISABLE
  SHOW GET lnOld      DISABLE
  SHOW GET lnNew      DISABLE
  SHOW GET lnAdj      DISABLE
  SHOW GET lnAdjCode  DISABLE
  SHOW GET lnNewCost  DISABLE
  SHOW GET pbUpdate   DISABLE
  SHOW GET pbCancel,1 PROMPT lcDetP
ENDIF

*!*************************************************************
*! Name      : lfFabKey
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Validate fabric browse button.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Screen : Mat400_4
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Functions : lfvFabric
*!*************************************************************
FUNCTION lfFabKey

lcFabric  = PADR("?",7)
llNothing = lfvFabric()

*!*************************************************************
*! Name      : lfColKey
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : To browse the Colors available for the chosen
*!             Fabric
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Screen : Mat400_4
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Functions : lfvColor
*!*************************************************************
FUNCTION lfColKey

lcColor   = PADR("?",lnColorWid)
llNothing = lfvColor()

*!*************************************************************
*! Name      : lfDyeKey
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : To browse the Dyelots available for the chosen
*!             Fabric + Color
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Screen : Mat400_4
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Functions : lfvDyelot
*!*************************************************************
FUNCTION lfDyeKey

lcDyelot  = PADR("?",10)
llNothing = lfvDyelot()

*!*************************************************************
*! Name      : lfFCDWCheck
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : To check if the Fabric + Color + Dyelot
*!             are assigned to the chosen Warehouse
*!             and if not you may add it to the ToWareHouse
*!             in case of transfer
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Functions : lfvColor , lfvDyelot
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedure : gpAdFabWar
*!*************************************************************
FUNCTION lfFCDWCheck
PRIVATE InAlias, llReEnter, lcWareCode

llReEnter  = .F.

InAlias    = SELECT()
lcMessage  = 'Fabric/Color'+IIF(llDyeLvl,'/Dyelot','')+':'  +;
             ALLTRIM(lcFabric)+'/'+ALLTRIM(lcColor)         +;
             IIF(llDyeLvl,'/'+ ALLTRIM(lcDyelot),'')        +;
             ' is not assigned to warehouse:'+ALLTRIM(lcFromWare)+'.'

*-- Check if the (Fabric + Color + Dyelot)
*-- are assigned to the FromWareHouse or not
SELECT FabDye             

IF !SEEK (lcFabric+PADR(lcColor,6)+lcFromWare+lcDyelot)
  llReEnter = gfModalGen('QRM36049B36001','ALERT',IIF(llDyeLvl,'/Dyelot','')+':'+ALLTRIM(lcFabric)+'/'+ALLTRIM(lcColor)+IIF(llDyeLvl,'/'+ ALLTRIM(lcDyelot),' ')+'|'+ALLTRIM(lcFromWare)) = 2
  IF !llReEnter
    IF llDyeLvl
      DO gpAdFabWar WITH lcFabric, lcColor, lcDyelot, lcFromWare ,lcTmpDyRel
    ELSE
      DO gpAdFabWar WITH lcFabric, lcColor, SPACE(10), lcFromWare
    ENDIF
  ENDIF
ENDIF
  
*-- if it is transfer make this check for target warehouse
IF llWareHous AND lcType = 'T'
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
*! Name      : lfLocCheck
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : To check if there is any locations  
*!             assigned to the chosen Warehouse
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Screen : Mat400_4
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedure : lpLocation
*!*************************************************************
FUNCTION lfLocCheck
PRIVATE lnAlias, lcMessage

lnAlias   = SELECT()
lcWareAny = IIF(llWareHous, lcFromWare, qWareHouse)
lcMessage = "No locations have been assinged to warehouse " + ALLTRIM(lcWareAny) + "."
SELECT WHSLOC
SET ORDER TO TAG WhsLoc
IF SEEK (lcWareAny)
  SET ORDER TO TAG WhsLocSt
  DO lpLocation
ELSE
  =gfModalGen('QRM36050B36000','ALERT', ALLTRIM(lcWareAny))
ENDIF  
cbLocation = SEEK(PADR(lcFabric,12)+lcColor+lcFromWare,"WhsLoc") OR ;
             SEEK(PADR(lcFabric,12)+lcColor+lcFromWare,lcTemLoc)
SHOW GET cbLocation
SELECT(lnAlias)

*!*************************************************************
*! Name      : lpLocation
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : To select the locations assigned to
*!             the chosen Warehouse from file WHSLOC
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Function : lfLocCheck
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function : lfMover,lfvLoc
*!*************************************************************
PROCEDURE lpLocation

*-- Initialize the source and target arrays.
DIMENSION laSource[1], laTarget[1]
STORE ' ' TO laSource, laTarget

*-- Initialize the used warehouse variable. the file pointer is set to
*-- the FromWare Code.
lcWareH = IIF(llWareHous, FabDye.cWareCode, lcFromWare)

*-- Select the location assigned for the warehouse in the source array.
SELECT cLocation FROM WHSLOC ;
 WHERE CWARECODE == lcWareAny AND EMPTY(STYLE+COLOR);
  INTO ARRAY laSource

IF _TALLY = 0
  *-- If there is no location in the ware house.
  =gfModalGen('QRM36050B36000','ALERT', ALLTRIM(lcWareH))
ELSE
  *-- Reinitialize the target array with the previosly selected locations.
  SELECT cLocation FROM (lcTemLoc) ;
   WHERE STYLE+COLOR+CWARECODE+CLOCATION=;
         PADR(lcFabric,12)+lcColor+lcWareAny;
    INTO ARRAY laTarget
  
  *-- Display the mover.
  =gfMover(@laSource,@laTarget," Assign Locations",.T.,'lfvLoc')
  
  *-- Update the temp file with the new selected locations.
  SELECT (lcTemLoc)
  DELETE FOR STYLE+COLOR+CWARECODE+CLOCATION = ;
             PADR(lcFabric,12)+lcColor+lcWareAny
  FOR lnLoc = 1 TO ALEN(laTarget)
    IF !EMPTY(laTarget[lnLoc])
      APPEND BLANK
      REPLACE STYLE     WITH lcFabric;
              COLOR     WITH LCcolor;
              CWARECODE WITH lcWareAny;
              CLOCATION WITH laTarget[lnLoc]
    ENDIF             
  ENDFOR
ENDIF  

*!*************************************************************
*! Name      : lfvLoc
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : To seek if the chosen location
*!             is assigned to the (Fabric + Color)
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedure : lpLocation
*!*************************************************************
FUNCTION lfvLoc
PARAMETERS lnOption
PRIVATE lnArray

IF lnOption=3 OR lnOption=4
  RETURN .T.
ELSE
  IF lnOption=2
    FOR lnArray = 1 TO ALEN(laSource)
      IF !SEEK(PADR(lcFabric,19)+lcColor+lcWareAny+laSource[lnArray],"WhsLoc")
        IF gfModalGen('QRM36051B36004','ALERT',ALLTRIM(laSource[lnArray])+'|'+ALLTRIM(lcFabric)+'/'+ALLTRIM(LCcolor)+IIF(llWareHous,' in warehouse '+ALLTRIM(lcWareAny),'' ))= 2
          RETURN .F.
        ENDIF
      ENDIF    
    ENDFOR           
  ELSE  
    RETURN  SEEK(PADR(lcFabric,19)+lcColor+lcWareAny+laSource[lsSource],"WhsLoc") ;
            OR ;
            gfModalGen('QRM36051B36004','ALERT',ALLTRIM(laSource[lsSource])+'|'+ALLTRIM(lcFabric)+'/'+ALLTRIM(LCcolor)+IIF(llWareHous,' in warehouse '+ALLTRIM(lcWareAny),'' ))= 1
  ENDIF  
ENDIF
*!*************************************************************
*! Name      : lfvMovmt
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : To move or remove one or more location
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Passed :
*!        Parameters : 
*!        lnMovmnts  : hold the choice if move or remove (one or all)
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Screen : lMover.Spx
*!*************************************************************
FUNCTION lfvMovmt
PARAMETERS lnMovmnts

DO CASE
  CASE lnMovmnts = 1
    _CUROBJ = OBJNUM(lsSource)
    KEYBOARD "{ENTER}"
  CASE lnMovmnts = 2
    DECLARE laTarget[ALEN('laSource',1)]
    =ACOPY(laSource,laTarget)
    SET SKIP OF POPUP puSource .T.
    SHOW GETS
  CASE lnMovmnts = 3
    _CUROBJ = OBJNUM(lsTarget)
    KEYBOARD "{ENTER}"
  CASE lnMovmnts = 4
    DECLARE laTarget[1]
    laTarget =' '
    SET SKIP OF POPUP puSource .F.
    _CUROBJ = OBJNUM(lsSource)
    KEYBOARD('{25}')
    SHOW GETS
ENDCASE

*!*************************************************************
*! Name      : lfvMSrce
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Assign a location for (Fabric + Color) in the warehouse
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Screen : lMover.Spx
*!*************************************************************
FUNCTION lfvMSrce

IF TYPE('lcValid') = 'C' AND !EMPTY(lcValid) AND !&lcValid
  RETURN .F.
ENDIF
IF lsSource <= ALEN('laSource',1) AND lsSource <> 0
  SET SKIP OF BAR lsSource OF puSource .T.
  IF !EMPTY(laTarget[1]) 
    DIMENSION laTarget[ALEN(laTarget)+1]
  ENDIF
  laTarget[ALEN(laTarget)]= ALLTRIM(laSource[lsSource])
ENDIF  
lnStart  = lsSource
lsSource = 0
FOR lnCount = lnStart TO CNTBAR('puSource')
  IF !SKPBAR('puSource',lnCount)
    lsSource = lnCount 
    EXIT
  ENDIF  
ENDFOR
IF lsSource = 0
  FOR lnCount = 1 TO CNTBAR('puSource')
    IF !SKPBAR('puSource',lnCount)
      lsSource = lnCount 
      EXIT
    ENDIF  
  ENDFOR
ENDIF  
_CUROBJ = OBJNUM(lsSource)
SHOW GETS

*!*************************************************************
*! Name      : lfVldDate
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : check in which period of Gl the date falls
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Screen : Mat400_4
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function : CheckPrd
*!*************************************************************
FUNCTION lfVldDate

IF LASTKEY() != 27 
  *-- Check if the date is in the Gl period
  IF llGlLink
    IF ldCurrDate > ldPost 
      =gfModalGen('QRM36106B36000','ALERT')  
      _CUROBJ = OBJNUM(ldCurrDate)
    ENDIF
  ELSE
    IF !CheckPrd(ldCurrDate ,'lcGlFYear','lcGlPeriod',;
                    IIF(lcType='P','MP','MA')) OR EMPTY(ldCurrDate)
      ldCurrDate = gdSysDate
      _CUROBJ = OBJNUM(ldCurrDate)
    ENDIF  
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvlnOld
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Get the old stock of the (Fabric + Color)
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Functions : lfvColor , lfvDyelot
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function : lfDetRef
*!*************************************************************
FUNCTION lfvlnOld
PRIVATE InAlias

InAlias = SELECT()

*-- Case single warehouse and !llDyeLvl get the old stock 
*-- from the Fabric file
IF !llWareHous .AND. !llDyeLvl
  lnOld   = Fabric.OnHand
  lnToStk = Fabric.OnHand
ELSE
  *-- Case multi warehouse or llDyeLvl
  *-- get the old stock from the FabDye file
  SELECT FabDye
  SEEK lcFabric+lcColor+lcFromWare+IIF(llDyeLvl,lcDyelot,SPACE(10))
  lnOld   = FabDye.OnHand
  lnToStk = FabDye.OnHand
  *-- Case of transfer and old stock <= 0 we cannot permit the user to
  *-- proceed in this transaction.
  IF lcType = 'T'
    IF lnOld <= 0
      =gfModalGen('QRM36052B36000','ALERT')
      llNoThing = lfInit(.F.)
      _CUROBJ   = OBJNUM(lcFabric)
    ELSE
      SEEK lcFabric+lcColor+lcToWare+IIF(llDyeLvl,lcDyelot,'')
      STORE OnHand TO lnNew, lnToStk
    ENDIF
  ENDIF
ENDIF  

*-- Case new mode and !transfer
*-- inialize the new stock with the old stock
IF (!llModify AND lcType <> 'T')
  lnNew = lnOld
ENDIF

llNoThing = lfDetRef()
SELECT(Inalias)

*!*************************************************************
*! Name      : lfvlndAdj
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Check if the adjustement amount is valid or not 
*!             due to lcType
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Screen : Mat400_4
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function : lfvlnNew
*!*************************************************************
FUNCTION lfvlndAdj

llValid    = .F.
llCostCond = .T.
SHOW GET lnNewCost ENABLE
DO CASE  
  CASE lcType = 'A'
    IF lnAdj <= 0
      SHOW GET lnNewCost DISABLE
    ENDIF  
    llValid   = .T.
  CASE lcType = 'P'
    IF lnAdj >= 0
      llValid = .T.
    ELSE
      =gfModalGen('QRM36053B36000','ALERT')
      lnAdj = 0
      SHOW GET lnAdj
      _CUROBJ = _CUROBJ
    ENDIF      
  CASE lcType = 'T'
    IF (lnAdj>=0 .AND. lnOld>=lnAdj)
      llValid = .T.
    ELSE
      =gfModalGen('QRM36054B36000','ALERT',ALLTRIM(STR(lnold)))
      lnAdj = 0
      SHOW GET lnAdj
      _CUROBJ = _CUROBJ      
    ENDIF      
ENDCASE
llNothing = IIF(llValid, lfvlnNew(), .F.) 

IF lnAdj > 0
  lnOldCost = IIF(lnOldCost=0 , Fabric.costBuy/Fabric.Conv , lnOldCost)
  lnNewCost = IIF(llScrMode,lnNewCost,lnOldCost)
  SHOW GET lnNewCost
  = lfDetRef()
ENDIF  

*!*************************************************************
*! Name      : lfvlnNew
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : calculate the new stock due to the adj.amount
*!             and the old stock
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Function : lfvlndAdj
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function : lfDetRef
*!*************************************************************
FUNCTION lfvlnNew

DO CASE  
  CASE lcType = 'A'
    lnNew = lnAdj + lnOld
  CASE lcType = 'P'
    lnNew = lnAdj
  CASE lcType = 'T'
    lnNew = lnAdj + lnToStk
ENDCASE
llNoThing = lfDetRef()

*!*************************************************************
*! Name      : lfGtOldCst
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Get the old cost of (Fabric + Color)
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Function : lfvColor
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function : lfDetRef
*!*************************************************************
FUNCTION lfGtOldCst

*-- if single warehouse get the oldcost from the FabDye file
IF !llWareHous
  lnOldCost = (IIF(lcMtCstMth $ 'LFIA',Fabric.nAveCstBuy,Fabric.CostBuy))/Fabric.Conv
ELSE
  *-- if multi warehouse get the oldcost from the Fabric file
  IF SEEK(lcFabric+lcColor+lcFromWare+SPACE(10),'FabDye')
    lnOldCost = (IIF(lcMtCstMth $ 'LFIA',FabDye.nAveCstBuy,Fabric.CostBuy))/Fabric.Conv
  ENDIF  
ENDIF 
 
*-- if new mode initialize the new cost with the old cost
lnNewCost = IIF(llModify, lnNewCost, lnOldCost)
llNoThing = lfDetRef()

*!*************************************************************
*! Name      : lfvpbUpdate
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Add a new record in lcTmpAdj file
*!             or Update a modified record with new values
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Screen : Mat400_4
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function  : lfInit
*!         Procedure : lpRepTemp
*!*************************************************************
FUNCTION lfvpbUpdate
PRIVATE lnAlias

lnAlias = SELECT()
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
*! Name      : lfActWindw
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Set the status of push buts. at screen Mat400_3
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Screens : Mat400, Mat400_4
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function  : lfRefresh
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
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Modify an existing record in lcTmpAdj file
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Screens : Mat400_3
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function  : lfInit
*!         Screen    : Mat400_4
*!*************************************************************
FUNCTION lfvModify

llModify   = .T.
cbLocation = .F.
lcLocStat  = "ENABLE"
lcFabStat  = 'DISABLE'
lcDatStat  = 'ENABLE'
lcDetP     = lcCanclP
SHOW GET pbCancel,1 PROMPT lcDetP
GO lnNewRec
llNothing = lfInit(.T.)

*-- Call the Fabric details screen in the modify mode
PUSH KEY
ON KEY
DO (gcScrDir+gcWinAppl+'\MAINVCT4.SPX')
POP KEY

*!*************************************************************
*! Name      : lfvRemove
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Remove an existing record in lcTmpAdj file
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Screens : Mat400_3
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function  : lfActBrow
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
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Replace in the lcTmpAdj file
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Function : lfvpbUpdate
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function  : lfChMarker
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
        Desc       WITH lcDesc     ,;
        Type       WITH lcType     ,;
        cReason    WITH lcReason   ,;
        Date       WITH ldCurrDate ,;
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
        cAdjReason WITH lcAdjReason,;
        cAdjAcct   WITH lcAdjAcct  ,;
        nUntCstBuy WITH lnNewCost*Fabric.Conv,;
        cMIcAcct   WITH lcMIcAcc,;
        NFUNITCOST WITH lnNewCost

= gfAdd_Info(lnCurAlias)
llNothing = lfChMarker()
*!*************************************************************
*! Name      : lfChMarker
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Set the marker in any browse
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Function : lfActBrow, lfRBrow, lfActBrwRo
*!*************************************************************
FUNCTION lfChMarker

IF !EOF()
  lnNewRec = RECNO()
  REPLACE ALL cMarker WITH lcUnMark
  GO lnNewRec
  REPLACE cMarker WITH lcMark
ENDIF  

*!*************************************************************
*! Name      : lpUpDtCost
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose    : Add a record in MatInvJl file with the new cost
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedure : lpUpdtPhy
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedure : lpSvInJrl
*!*************************************************************
PROCEDURE lpUpDtCost
PRIVATE lcTmpFile, lcSession, lnOldQty

*-- If it is Costing method in "SA" Just issue the onhand Qty
*-- with usecost from fabric or fabdye file 
IF lcMtCstMth $ 'AS'

  IF llWareHous
    =SEEK(&lcTmpAdj..Fabric+&lcTmpAdj..Color+&lcTmpAdj..cFromWare+SPACE(10),'FabDye')
    lnUntCst   = IIF(lcMtCstMth = 'A',FabDye.nAveCstBuy,Fabric.CostBuy)/Fabric.Conv
    lnUntCstBy = IIF(lcMtCstMth = 'A',FabDye.nAveCstBuy,Fabric.CostBuy)
    lnOldQty   = FabDye.OnHand
  ELSE
    lnUntCst   = IIF(lcMtCstMth $ 'A',Fabric.nAveCstBuy,Fabric.CostBuy)/Fabric.Conv
    lnUntCstBy = IIF(lcMtCstMth $ 'A',Fabric.nAveCstBuy,Fabric.CostBuy)
    lnOldQty   = Fabric.OnHand
  ENDIF
  
  *-- Passing parameter for updating Material inventory control GLAccount Field
  *-- AAMER 11/10/98
  *DO lpSvInJrl WITH &lcTmpAdj..Fabric,&lcTmpAdj..Color,&lcTmpAdj..cFromWare,;
                    &lcTmpAdj..Dyelot,&lcTmpAdj..Date,'3',;
                    SPACE(06),SPACE(06),lcGlSess,;
                    lnUntCst,0,lnOldQty,lnUntCstBy,&lcTmpAdj..cReason,;
                    &lcTmpAdj..cAdjReason,&lcTmpAdj..cAdjAcct,&lcTmpAdj..cMIcAcct

ELSE
*-- If it is Costing method in "SA" Just issue the onhand Qty (End)

  *-- Create a temp. file to collect all the receivig records for
  *-- the Fabric\Color\WareHouse\Dyelot in process to be used in :
  *-- 1) The Lot costing : To generate an issuing record for eacth
  *--                      receiving record in the temp. file.
  *-- 2) The Standard and Average cost :
  *--                      To accumulate the open quantities to generate
  *--                      an issuing record with the total open quantity.

  lcTmpFile = gfTempName()
  lcExp = &lcTmpAdj..Fabric+&lcTmpAdj..Color+&lcTmpAdj..cFromWare+&lcTmpAdj..Dyelot 
  SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,  ;
         nUnitCost,SUM(nReceived-nIssued) AS 'nBalance',nUntCstBuy  ;
  FROM   MATINVJL                                               ;
  WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = ;
         lcExp + MatInvJl.cRSession                             ;
  GROUP BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode, ;
           MatInvJl.cDyelot,MatInvJl.cRSession,MatInvJl.cTran   ;
  INTO DBF &gcWorkDir.&lcTmpFile
  INDEX ON cFabric+cColor+cWareCode+cDyelot+cRSession+cISession ;
  TAG      &lcTmpFile

  SELECT (lcTmpFile)
  DELETE ALL FOR nBalance = 0
  GOTO TOP
  IF !EOF()
      *-- Create an issuing record with the total quantity in the temp.
      *-- file.

      *-- The Stk and cost should be from the temp file which is created (Start)
      *-- from MatInvJl because the costing method is one of "FIL"
      SCAN
        *-- The Stk and cost should be from the temp file which is created (Start)
        *-- AAMER 11/09/98
        lnUntCst    = &lcTmpFile..nUnitCost
        lnUntCstBy  = &lcTmpFile..nUntCstBuy
        *-- The Stk and cost should be from the temp file which is created (End)

        *-- Passing parameter for updating Material inventory control GLAccount Field(Start)
        *-- AAMER 11/10/98
                          &lcTmpAdj..cAdjReason,&lcTmpAdj..cAdjAcct
        *DO lpSvInJrl WITH &lcTmpAdj..Fabric,&lcTmpAdj..Color,&lcTmpAdj..cFromWare,;
                          &lcTmpAdj..Dyelot, &lcTmpAdj..Date, '3',;
                          SPACE(06),&lcTmpFile..cRSession,lcGlSess,;
                          lnUntCst, 0,&lcTmpFile..nBalance,lnUntCstBy,&lcTmpAdj..cReason,;
                          &lcTmpAdj..cAdjReason,&lcTmpAdj..cAdjAcct,&lcTmpAdj..cMIcAcct
        *-- Passing parameter for updating Material inventory control GLAccount Field(End)
      ENDSCAN
  ENDIF  
  SELECT (lcTmpFile)
  ZAP
  USE
  ERASE (gcWorkDir+lcTmpFile+'.DBF')
  ERASE (gcWorkDir+lcTmpFile+'.CDX')
  ERASE (gcWorkDir+lcTmpFile+'.FPT')
  
ENDIF

*-- Create a new receiving record with the new quantity and the new cost.
IF &lcTmpAdj..nMTotAdj > 0
  *-- Passing parameter for updating Material inventory control GLAccount Field
  *DO lpSvInJrl WITH &lcTmpAdj..Fabric, &lcTmpAdj..Color, &lcTmpAdj..cFromWare,;
                    &lcTmpAdj..Dyelot, &lcTmpAdj..Date, '3',;
                    SPACE(06), lcGlSess, SPACE(6) ,;
                    &lcTmpAdj..UnitCost, &lcTmpAdj..nMTotAdj, 0,&lcTmpAdj..nUntCstBuy,&lcTmpAdj..cReason,;
                    &lcTmpAdj..cAdjReason,&lcTmpAdj..cAdjAcct,&lcTmpAdj..cMIcAcct
  *-- Passing parameter for updating Material inventory control GLAccount Field(End)
ENDIF

*!*************************************************************
*! Name      : lpSvInJrl
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Add a record in MatInvJl file with the new cost
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Passed :
*!        Parameters : 
*!           lcPFabric  : Fabric
*!           lcPColor   : Color
*!           lcPWarCode : Warecode
*!           lcPDyelot  : Dyelot
*!           ldPTrnDate : dTranDate  
*!           lcPTrnType : cTranType
*!           lcPTran    : cTran
*!           lcPRSesion : cRSession
*!           lcPISesion : cISession
*!           lnPUntCost : nUnitCost
*!           lnPRecived : nReceived
*!           lnPIssued  : nIssued
*!           lnPUntCstBy: nUntCstBuy
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedure : lpUpdtAdj, lpUpdtCost, lpUpdtTrnf 
*!*************************************************************
PROCEDURE lpSvInJrl

PARAMETERS lcPFabric, lcPColor, lcPWarCode, lcPDyelot, ldPTrnDate  ,;
           lcPTrnType, lcPTran, lcPRSesion, lcPISesion, lnPUntCost ,;
           lnPRecived, lnPIssued, lnPUntCstBy,lcPReason,lcPAdjCode,lcPAdjAcct,lcPMIcAcct

SELECT MatInvJl
APPEND BLANK

REPLACE cFabric    WITH lcPFabric  ,;
        cColor     WITH lcPColor   ,;
        cWareCode  WITH lcPWarCode ,;
        Reference  WITH lcPReason  ,;
        cDyelot    WITH lcPDyelot  ,;
        cAdjReason WITH lcPAdjCode ,;
        cGlMatAdj  WITH lcPAdjAcct ,;
        cMIcAcct   WITH lcPMIcAcct ,;
        dTranDate  WITH ldPTrnDate ,;
        cTranType  WITH lcPTrnType ,;
        cTran      WITH IIF(EMPTY(lcPISesion),lcPRSesion,lcPISesion) ,;
        cRSession  WITH lcPRSesion ,;
        cISession  WITH lcPISesion ,;
        nUnitCost  WITH lnPUntCost ,;
        nReceived  WITH lnPRecived ,;
        nIssued    WITH lnPIssued  ,;
        nUntCstBuy WITH lnPUntCstBy,;
        dPostDate  WITH ldPost     ,; 
        cTrn_Seq   WITH IIF(EMPTY(lcPISesion),lcPRSesion,lcPISesion)

*!*************************************************************
*! Name      : lpSavScr
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Scan lcTmpAdj file and calls the appropriate procedure
*!             due to lctype 
*!             if the system setup is Rolls or Lots calls browse 
*!             of either one of them
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Screen : Mat400_3
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedure : lpDelUnLock, lpRoMtBrow,lpPhUnLock,
*!                     lpUpdtAdj,lpUpdtPhy, lpUpdtTrnf,lfActBrow
*!         Function  : lfSvInGl, lfSvFInv
*!*************************************************************
PROCEDURE lpSavScr

*-- Generate a unique session number.
IF llScrMode
  lcGlSess = DFINVADJ.CTRN_SEQ
ELSE
  lcGlSess = gfSEQUENCE('GLSession')
ENDIF

SELECT (lcTmpAdj)
SCAN FOR ! ((nMTotAdj = 0) .AND. (lcType $ 'AT'))
  = SEEK (&lcTmpAdj..Fabric+&lcTmpAdj..Color , 'Fabric')
  llDyeLvl = llDyelot .AND. Fabric.cDye_Flg = 'Y'
  lcFab    = &lcTmpAdj..Fabric
  lcClr    = &lcTmpAdj..Color
  lcWare   = &lcTmpAdj..cFromWare
  lcDye    = &lcTmpAdj..Dyelot
  IF !lfPhyLock()
    DO lpDelUnLock WITH ('1')
    LOOP
  ENDIF

  *-- Store Fabric old cost, Old stock, And link code.
  SELECT Fabric
  lnOldStk   = OnHand
  lnOldCost  = (IIF(lcMtCstMth $ 'LFIA',nAveCstBuy,CostBuy))/Conv
  lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(Link_Code),Link_Code,'DEFDEF'),"")
  IF lcType = 'T'
    *-- Store the stock in the TO warehouse before updating the master
    *-- file.
    = SEEK (&lcTmpAdj..Fabric+&lcTmpAdj..Color+&lcTmpAdj..cToWare+;
            IIF(llDyeLvl, &lcTmpAdj..Dyelot,SPACE(10)),'FabDye')
    SELECT (lcTmpAdj)
    *REPLACE nOldToQty WITH FabDye.OnHand
    
    *-- We should be sure that the stock in the FROM warehouse is enougth
    *-- to be transfered to the TO warehouse.
    = SEEK (&lcTmpAdj..Fabric+&lcTmpAdj..Color+&lcTmpAdj..cFromWare+;
            IIF(llDyeLvl, &lcTmpAdj..Dyelot,SPACE(10)),'FabDye')
    IF FabDye.OnHand = 0
      lnChoice=gfModalGen('QRM36055B36000','ALERT',IIF(llDyelvl,'/Dyelot: ',': ')+ALLTRIM(Fabric)+'/'+ALLTRIM(Color)+IIF(llDyelvl,'/'+ALLTRIM(Dyelot)+' ',' ')+'|'+ALLTRIM(lcFromWare)) 
      SELECT (lcTmpAdj)
      DELETE 
      DELETE REST WHILE Fabric+Color+cFromWare+Dyelot = ;
                        lcFab+lcClr+lcWare+IIF(llDyeLvl,lcDye,SPACE(10))
      llNothing = lfActBrow()                  
      DO lpPhUnLock
      LOOP                  
    ENDIF

    *-- If there is not enougth stock to be transfered then please 
    *-- tell this to the user and ask him if he wants to transfer all 
    *-- the stock that exists in the FROM warehouse to the TO warehouse.

    IF &lcTmpAdj..nMTotAdj > FabDye.OnHand
      lnChoice=gfModalGen('QRM36056B36001','ALERT',IIF(llDyelvl,'/Dyelot:',':')+ALLTRIM(Fabric)+'/'+ALLTRIM(Color)+IIF(llDyelvl,'/'+ALLTRIM(Dyelot)+' ','')+'|'+ALLTRIM(lcFromWare))
      SELECT (lcTmpAdj)
      IF lnChoice = 1
        *-- If the user wants to transfer the stock, all we have to
        *-- do is to save the FROM warehouse stock in the temp. file
        *-- as the adjustments quantity.
        *E301089 (Start)
        *REPLACE TotAdj WITH FabDye.OnHand
        REPLACE nMTotAdj WITH FabDye.OnHand
        *E301089 (End)
      ELSE
        DELETE
        DO lpPhUnLock
        LOOP
      ENDIF
    ENDIF
  ENDIF
  
  DO CASE
    CASE lcType = 'A'
      DO lpUpdtAdj
  ENDCASE  
  *IF llWareLoc
  *  SELECT (lcTemLoc)
  *  SCAN FOR !EMPTY(cLocation)
  *    IF !SEEK(STYLE+COLOR+cWareCode+cLocation,'WHSLOC')
  *      SCATTER MEMVAR
  *      INSERT INTO WhsLoc FROM MEMVAR
  *    ENDIF
  *  ENDSCAN
  *ENDIF 

  DO lpPhUnLock
  SELECT (lcTmpAdj)
  SCATTER MEMVAR
  llNothing = lfSvFInv()
ENDSCAN

IF llTrkRolls .OR. lcMtCstMth = "LFI"
  ERASE (gcWorkDir + lcTmpRolls."DBF")
  ERASE (gcWorkDir + lcTmpRolls."CDX")
ENDIF  

*-- Update the General Ledger file
*=lfSvInGl()

IF llDyelot
*  =gfArDyRl('','',lcTmpDyRel,.T.)
ENDIF  

DO lpClsScr
SHOW GETS
*=lfInit(.F.)
*!*************************************************************
*! Name      : lfGetTran
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Get a required field from MATINVJL file
*!             from a record matching with a specified expression
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Passed :
*!        Parameters : 
*!           lcField : Field wanted to return 
*!           lcExpr  : expression we are looking for
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Function : lfRBrow
*!*************************************************************

FUNCTION lfGetTran
PARAMETER lcField, lcExpr
RETURN LOOKUP(MATINVJL.&lcField,lcExpr,MATINVJL.cfabric,'MATINVJL')

*!*************************************************************
*! Name      : lpUpdtAdj
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Case lcType = "A"
*!             Update Fabric, Fabdye file with new values 
*!             of stock and cost .
*!             Update Gl files with the new cost
*!             Create a new record in MatInvJl
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedure : lpSvInMstr
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedure : lpSvInJrl,lpGlCall
*!*************************************************************
PROCEDURE lpUpdtAdj

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
  STORE &lcTmpAdj..GLFYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE &lcTmpAdj..GLPeriod  TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lctmpGlDis TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = &lcTmpAdj..cAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF

*lnRet = gfMatCrl('2',&lcTmpAdj..Fabric,&lcTmpAdj..Color,;
                 &lcTmpAdj..cFromWare,&lcTmpAdj..Dyelot,;
                 &lcTmpAdj..Date,ldPost,SPACE(6),;
                 &lcTmpAdj..nMTotAdj,&lcTmpAdj..UnitCost,;
                 &lcTmpAdj..cReason,&lcTmpAdj..cAdjReason,;
                 0,'','',@laGLDistAr,lcGlSess)


*!*************************************************************
*! Name      : lpUpdtPhy
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Case lcType = "P"
*!             Update Fabric file with new values of stock 
*!             and cost .
*!             Update Gl files with the new cost
*!             Create a new record in MatInvJl
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedure : lpSvInMstr
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedure : lpSvInJrl,lpUpDtCost, lpGlCall
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*!*************************************************************
PROCEDURE lpUpdtPhy
PRIVATE lnAlias

IF llGlLink
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  laGLDistAr[1,2] = '015'
  laGLDistAr[2,2] = '016'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE 'MP'      TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE ldPost    TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE &lcTmpAdj..GLFYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE &lcTmpAdj..GLPeriod  TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lctmpGlDis TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = &lcTmpAdj..cAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF

*lnRet = gfMatCrl('3',&lcTmpAdj..Fabric,&lcTmpAdj..Color,;
                 &lcTmpAdj..cFromWare,&lcTmpAdj..Dyelot,;
                 &lcTmpAdj..Date,ldPost,SPACE(6),;
                 &lcTmpAdj..nMTotAdj,&lcTmpAdj..UnitCost,;
                 &lcTmpAdj..cReason,&lcTmpAdj..cAdjReason,;
                 0,'','',@laGLDistAr,lcGlSess)
                             
*!*************************************************************
*! Name      : lpUpdtTrnf
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose    : Case lcType = "T"
*!              Update Fabric file with new values of stock 
*!              and cost .
*!              Update Gl files with the new cost
*!              Create a new record in MatInvJl
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedure : lpSvInMstr
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedure : lpSvInJrl, lpGlCall
*!*************************************************************
PROCEDURE lpUpdtTrnf

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
  STORE &lcTmpAdj..GLFYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE &lcTmpAdj..GLPeriod  TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lctmpGlDis TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = &lcTmpAdj..cAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF

*-- issue from source warehouse
*lnRet = gfMatCrl('2',&lcTmpAdj..Fabric,&lcTmpAdj..Color,;
                 &lcTmpAdj..cFromWare,&lcTmpAdj..Dyelot,;
                 &lcTmpAdj..Date,ldPost,SPACE(6),;
                 -&lcTmpAdj..nMTotAdj,&lcTmpAdj..UnitCost,;
                 &lcTmpAdj..cReason,&lcTmpAdj..cAdjReason,;
                 0,'','',@laGLDistAr,lcGlSess)

*-- Receive to target warehouse
*lnRet = gfMatCrl('2',&lcTmpAdj..Fabric,&lcTmpAdj..Color,;
                 &lcTmpAdj..cToWare,&lcTmpAdj..Dyelot,;
                 &lcTmpAdj..Date,ldPost,SPACE(6),;
                 &lcTmpAdj..nMTotAdj,&lcTmpAdj..UnitCost,;
                 &lcTmpAdj..cReason,&lcTmpAdj..cAdjReason,;
                 0,'','',@laGLDistAr,lcGlSess)

*!*************************************************************
*! Name      : lfPhyLock
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Do the phisical lock on the used file 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedure : lpSvInMstr
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function : lfRecPLocc, lfRecPLock
*!*************************************************************

FUNCTION lfPhyLock

DO CASE
  CASE ! llWareHous .AND. ! llDyeLvl
    
    *-- lfRecPLock instead of lfRecPLocc
    llRetn = RLOCK('Fabric' ) .OR. lfRecPLock('Fabric' )

  CASE ! llWareHous .AND. llDyeLvl
    = SEEK (&lcTmpAdj..Fabric+&lcTmpAdj..Color+&lcTmpAdj..cFromWare+;
            &lcTmpAdj..Dyelot,'FabDye')
    llRetn = (RLOCK('FabDye') .OR. lfRecPLock('FabDye')) .AND. ;
             (RLOCK('Fabric') .OR. lfRecPLock('Fabric'))

  CASE llWareHous .AND. ! llDyeLvl
    = SEEK (&lcTmpAdj..Fabric+&lcTmpAdj..Color+&lcTmpAdj..cFromWare+;
            SPACE(10),'FabDye')
    llRetn = (RLOCK('FabDye') .OR. lfRecPLock('FabDye')) .AND. ;
             (RLOCK('Fabric') .OR. lfRecPLock('Fabric'))

  CASE llWareHous .AND. llDyeLvl
    SET MULTILOCKS ON
    = SEEK (&lcTmpAdj..Fabric+&lcTmpAdj..Color+&lcTmpAdj..cFromWare+;
            &lcTmpAdj..Dyelot,'FabDye')
    llRetn = (RLOCK('FabDye') .OR. lfRecPLock('FabDye')) .AND. ;
             (RLOCK('Fabric') .OR. lfRecPLock('Fabric'))
    IF llRetn
      = SEEK (&lcTmpAdj..Fabric+&lcTmpAdj..Color+&lcTmpAdj..cFromWare+;
              SPACE(10),'FabDye')
      llRetn = (RLOCK() .OR. lfRecPLock('FabDye'))
    ENDIF  
ENDCASE
RETURN (llRetn)

*!*************************************************************
*! Name      : lfRecPLock
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Do the phisical lock on the used record 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Passed :
*!        Parameters : 
*!           lcFile : The used file where we lock a record
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Function : lfPhyLock
*!*************************************************************
FUNCTION lfRecPLock
PARAMETERS lcFile

DO CASE
  CASE lcFile = 'Fabric'
    lcStr = 'Item record '
  CASE lcFile = 'FabDye'
    lcStr = 'Item dyelot record '
  CASE lcFile = 'MatInvJl'
    lcStr = 'Item receiving lot record '
ENDCASE

SET REPROCESS TO 5 SECONDS 
DO WHILE .T.
  lnChoice =gfModalGen('QRM36057B36005','ALERT',lcStr) 
  IF lnChoice = 1 
    IF !RLOCK(lcFile)
      LOOP
    ELSE
      lnRet = .T.
      EXIT
    ENDIF 
  ELSE
    lnRet = .F.
    EXIT   
  ENDIF
ENDDO
  
SET REPROCESS TO 0
RETURN (lnRet)

*!*************************************************************
*! Name      : lpPhUnLock
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Unlock for all the files
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedure : lpSvInMstr
*!*************************************************************
PROCEDURE lpPhUnLock

IF llWareHous .AND. llDyeLvl
  SET MULTILOCKS OFF
ELSE
  UNLOCK ALL
ENDIF

*!*************************************************************
*! Name      : lpDelUnLock
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Display a message for the user due to the case
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Passed :
*!        Parameters : 
*!           lcFlag : endicate wich message to display
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedure : lpSvInMstr
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedure : lpPhUnLock
*!*************************************************************
PROCEDURE lpDelUnLock
PARAMETERS lcFlag

SELECT (lcTmpAdj)
DO CASE
  CASE lcFlag = '1'
    =gfModalGen('QRM36057B36000','ALERT','|'+IIF(llDyelvl,'/Dyelot: ',':'+ALLTRIM(Fabric)+'/'+ALLTRIM(Color)+IIF(llDyelvl,'/'+ALLTRIM(Dyelot)+' ',' ')))
  CASE lcFlag = '3'
    IF llTrkRolls .AND. Fabric.ltrkrolls
      =gfModalGen('QRM36058B36000','ALERT') 
    ELSE
      =gfModalGen('QRM36059B36000','ALERT') 
    ENDIF            
ENDCASE
DELETE
DO lpPhUnLock

*!*************************************************************
*! Name      : lpGlCall
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Update the Gl files
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Passed :
*!        Parameters : 
*!           lnPAmount : Amount of adj.
*!           lcVType   : "MA"
*!           lcLink    : Gl linkcode
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedure : lpSvInMstr,lpUpdtAdj,lpUpdtPhy,lpUpdtTrnf
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedure : GLDist
*!*************************************************************
PROCEDURE lpGlCall
PARAMETERS lnPAmount, lcVType, lcLink

lnAlias = SELECT()
*-- 1) Inventory control.                
*-- Category key for "Inventory Control"........=> '015'.

*-- lcTranNo Variable that hold the transaction No (Start)
*-- AAMER 11/08/98
lcTranNo = MatInvJl.cTrn_Seq
*-- lcTranNo Variable that hold the transaction No (End)

DO GLDist WITH lcLink, '015', lnPAmount, lcVType, lcTranNo, &lcTmpAdj..DATE, ;
               &lcTmpAdj..GLFYear, &lcTmpAdj..GLPeriod, '&lctmpGlDis'

REPLACE  &lctmpGlDis..GlSession WITH lcGlSess             

DO GLDIST WITH lcLink, '016', -(lnPAmount), lcVType, lcTranNo, &lcTmpAdj..DATE, ;
               &lcTmpAdj..GLFYear, &lcTmpAdj..GLPeriod, '&lctmpGlDis',&lcTmpAdj..cAdjAcct

REPLACE  &lctmpGlDis..GlSession WITH lcGlSess                            
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvAplNegAdj
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Validate a (-)ve applied adj. in the roll browse.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Function : lfRBrow
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function  : lfRRefre
*!*************************************************************
FUNCTION lfvAplNegAdj

*-- If the applied value is greater than the current balance,
*-- or is negative,
*-- return the old value of the field, 
*-- otherwise update the corresponding fields
IF nQtyApl > nQtyBal .AND.;
  gfModalGen('QRM36062B36000','ALERT',ALLTRIM(cRollID)) = 1;
  .OR. nQtyApl < 0 .AND.;               
  gfModalGen('QRM36061B36000','ALERT') = 1

  REPLACE nQtyApl WITH lnOldVal                
ELSE
  *-- Update lcTmpRolls file 
  
  lnUsrApply = lnUsrApply + lnOldVal - nQtyApl
  
  lnLotAmont = lnLotAmont ;
               - lnOldVal * MATINVJL.nunitcost ;
               + (nQtyApl * MATINVJL.nunitcost)               
  REPLACE &lcTmpRolls..nApply    WITH &lcTmpRolls..nApply + nQtyApl
  REPLACE &lcTmpRolls..nBalance  WITH &lcTmpRolls..nBalance ;
                                    + lnOldVal - nQtyApl   
  REPLACE &lcTmpRolls..nunitcost WITH MATINVJL.nunitcost   ;                                               
          &lcTmpRolls..lStatus   WITH  'M'
  = lfRRefre()
ENDIF

*!*************************************************************
*! Name      : lfRBrow
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Prepare the browse fields and title
*!             in case of lots or rolls
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedure : lpRoMtBrow
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function : lfwOldVals, lfvAplNegAdj, lfChMarker,
*!                    lfGetTran, lfVAldAAp,lfVAldAply
*!*************************************************************
FUNCTION lfRBrow

lnAlias = SELECT()

SELECT MATINVJL
SET ORDER TO TAG MATINVJL
*-- get the stock after each line of transaction
lnAvailStk = IIF(llWareHous, ;
                 IIF(SEEK(lcFab + lcClr + lcWare + lcDye, 'FABDYE'),;
                 FABDYE.OnHand, 0),;
                 IIF(SEEK(lcFab + lcClr, 'FABRIC'),;
                 FABRIC.OnHand, 0))
SELECT(lnAlias)                 

*-- You have to use either Rolls with 'SAL'
*-- or to use 'FI' with out using rolls
DO CASE
  *-- This means it is not fifo or lifo because it uses rolls.
  CASE lcType = 'A' AND TotAdj < 0 .AND. llTrkRolls .AND. Fabric.ltrkrolls
    llAddRolls  = .F.
    lcRollTitl  = 'Available Rolls'
    lcRlBrFild  =   "cMarker :H= '':R:W=lfChMarker(),"+;
                    "cRSession : H='R.Sess.':R:W=.F.,"+;
                    "MATINVJL.dTranDate:H='Date':R:W=.F.,"+;
                    "cRollID   :H='Roll ID':R:W=.F.,"+;
                    "nQtyBal   :H='Roll Bal.':R:W=.F.,"+;
                    "nQtyApl   :H='Appl Qty.'"+;
                              ":W=lfwOldVals():V=lfvAplNegAdj(),"+;
                    "nNewBal   = nQtyBal - nQtyApl"+;
                              ":H='New Bal.':R:W=.F.,"+;
                    "MATINVJL.nunitcost:H='Unit Cost':R:W=.F."

    SELECT *, 00000000 AS nQtyApl,00000000 AS nApply, " " AS cMarker ,;
             " " AS cTrn_Seq,000000 AS nUnitcost ,000000 AS nBalance ,;
             "S" AS lStatus;
    FROM  Rolls ;
    WHERE cRollItem+COLOR+CWARECODE+DYELOT+CROLLID+TRANCD+CRSESSION =;
          lcFab+lcClr+lcWare+lcDye+CROLLID+"1";
          AND nQtyBal > 0;
    INTO DBF (gcWorkDir+lctmpRolls) 
    INDEX ON cRollItem+Color+cWareCode+Dyelot TAG lcTmpRolls
    llEmpTmpRo = IIF (_TALLY = 0, .T. , .F.)
    SET RELATION TO cRollItem+Color+cWareCode+Dyelot+cRsession +cIsession INTO MATINVJL 

  CASE (lcType = 'A' AND TotAdj > 0) .AND. llTrkRolls .AND. Fabric.ltrkrolls
    nQty        = 0 
    llAddRolls  = .T.  
    lcRollTitl  = 'New Rolls' 
    lcRlBrFild  = "cMarker :H='':W=lfChMarker() AND .F.,"+;
                  "cRollID :H='Roll ID':W = .F.,"+;
                  "nQtyBal :H='Roll Bal.':R:W = .F. "
    SELECT *, 00000000 AS nQtyApl,00000000 AS nApply, " " AS cMarker ,;
             " " AS cTrn_Seq, 0000 AS nUnitcost ,000000 AS nBalance ,SPACE(6) AS cIsession ,;
             "S" AS lStatus ,cRsession AS OldSession;
    FROM  Rolls ;
    WHERE cRollItem+COLOR+CWARECODE+DYELOT+CROLLID+TRANCD+CRSESSION =;
          lcFab+lcClr+lcWare+lcDye+CROLLID+"1";
          AND (nQty - nQtyBal)>0 AND nQtyBal > 0;           
    INTO DBF (gcWorkDir+lctmpRolls)
    INDEX ON cRollItem+Color+cWareCode+Dyelot ;
    TAG  lcTmpRolls
    lnApplDQty = nQty
     
  CASE (lcType = 'P' AND TotAdj => 0) .AND. llTrkRolls .AND. Fabric.ltrkrolls
    nQty        = 0 
    llAddRolls  = .T.  
    lcRollTitl  = 'New Rolls' 
    lcRlBrFild  = "cMarker :H='':W=lfChMarker() AND .F.,"+;
                  "cRollID :H='Roll ID':W = .F.,"+;
                  "nQtyBal :H='Roll Bal.':R:W = .F. "
    SELECT *, 00000000 AS nQtyApl,00000000 AS nApply, " " AS cMarker ,;
             " " AS cTrn_Seq, 0000 AS nUnitcost ,000000 AS nBalance ,SPACE(6) AS cIsession ,;
             "S" AS lStatus ,cRsession AS OldSession;
    FROM  Rolls ;
    WHERE cRollItem+COLOR+CWARECODE+DYELOT+CROLLID+TRANCD+CRSESSION =;
          lcFab+lcClr+lcWare+lcDye+CROLLID+"1";
          AND nQtyBal = 0 ; 
    GROUP BY ROLLS.cRollItem,ROLLS.COLOR,ROLLS.CWARECODE,ROLLS.DYELOT,ROLLS.CROLLID,ROLLS.TRANCD ;
    INTO DBF (gcWorkDir+lctmpRolls)
    INDEX ON cRollItem+Color+cWareCode+Dyelot ;
    TAG  lcTmpRolls
    lnApplDQty = nQty

  CASE lcType = 'T' .AND. llTrkRolls .AND. Fabric.ltrkrolls
    lcRollTitl  = 'Available Rolls in Warehouse ' + ALLTRIM(lcWare)
    lcRlBrFild  = "cMarker :H=' ':W=lfChMarker():V=lfEnter():F,"+;
                  "CHECK   :H=' ':R,"+;
                  "MatInvJL.cTran:H='Tran.':R,cRsession:H='R.Sess.':R,"+;
                  "MatInvJL.dtrandate:H='Date':R,crollid:H='Roll ID':R,"+;
                  "nQtyBal:H='Roll Bal.':R,MatInvJL.nUnitcost:H='Cost':R"
                  
    SELECT *, 0 AS nReceived, SPACE(1) AS cMarker,;
              "S" AS lStatus, SPACE(1) AS Check,;
              000000 AS nIssued ," " AS cTrn_Seq, 0 AS nApply ,0 AS nQTY ,;
              0 AS nUnitcost;
      FROM Rolls ;
     WHERE cRollItem+COLOR+CWARECODE+DYELOT+CROLLID+TRANCD+CRSESSION =;
           lcFab+lcClr+lcWare+lcDye+CROLLID+"1";
           AND nQtyBal>0;
      INTO DBF (gcWorkDir+lctmpRolls)
      SET RELATION TO cRollItem+COLOR+CWARECODE+DYELOT+CRSESSION +cIsession INTO MatInvJL
      llEmpTmpRo = IIF (_TALLY = 0, .T. , .F.)

  CASE (lcMtCstMth $ "LFI" AND (!llTrkRolls .OR. !Fabric.ltrkrolls)) AND ;
       ((lcType='A' AND lnTotApply<0) OR (lcType='T'))
    lcRollTitl  = 'Available Lots' 
    lcRlBrFild  = "cMarker :H='':R:W=lfChMarker(),"+;
                  "Ctran:H='Tran.':R:W=.F.,cRsession:H='R.Sess.':R:W=.F.,"+;
                  "dTrandate:H='Date':R:W=.F.,nunitcost:H='Unit Cost':R:W=.F.,"+;
                  "nReceived:H='Received':R:W=.F.,nIssued:H='Issued':R:W=.F.,"+;
                  "nApply:H='Apply':W=lfwOldVals():V=IIF(lcType='A' AND lcMtCstMth = 'L',lfVAldAAp(),lfVAldAply()),"+;
                  "nBalance:H='Balance':R:W=.F."                  

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
             lcFab+lcClr+lcWare+lcDye   ;
    GROUP BY cFabric,cColor,cWareCode,cDyelot,cRSession                         ;
    INTO DBF (gcWorkDir+lctmpRolls)
    
    INDEX ON cFabric+cColor+cWareCode+DTOC(dTranDate)+cDyelot+cRSession+cISession;
    TAG      lctmpRolls

    DELETE ALL FOR nBalance <= 0
    PACK

    IF lcMtCstMth = "I"
      SET ORDER TO TAG lcTmpRolls DESCENDING
    ENDIF
    llEmpTmpRo = IIF (_TALLY = 0 OR RECCOUNT() = 0, .T. , .F.)
    SET RELATION TO cFabric+cColor+cWareCode+cDyelot+cRSession+cISession INTO MATINVJL ADDITIVE
ENDCASE  

*!*************************************************************
*! Name      : lfTrapRoll
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Do the trapping between the 2 screens Mat400_6
*!             and Mat400_8
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Screen : Mat400_5
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedures : lpEscR, lpTabR, lpSTabR
*!*************************************************************
FUNCTION lfTrapRoll

ON KEY LABEL CTRL+W     lnDummy = 1
ON KEY LABEL CTRL+Q     lnDummy = 1
ON KEY LABEL CTRL+HOME  lnDummy = 1
ON KEY LABEL CTRL+END   lnDummy = 1
ON KEY LABEL ESC        DO lpEscR
ON KEY LABEL CTRL+ENTER DO lpEscR
ON KEY LABEL TAB        DO lpTabR
ON KEY LABEL BACKTAB    DO lpSTabR

*!*************************************************************
*! Name      : lpTabR
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Do the trapping of Tab key between the 
*!             2 screens Mat400_6 and Mat400_8
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedure : lfTrapRoll
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedures : lpTabR
*!*************************************************************
PROCEDURE lpTabR

ON KEY LABEL TAB lnD = 1
DO CASE
  CASE WONTOP('lcWinch8') AND _CUROBJ = OBJNUM(pbRClose)
    ACTIVATE WINDOW (lcRollTitl)
  CASE WONTOP(lcRollTitl)  
    ACTIVATE WINDOW ('lcWinch8')
  OTHERWISE
    _CUROBJ = _CUROBJ + 1
ENDCASE
ON KEY LABEL TAB DO lpTabR

*!*************************************************************
*! Name      : lpSTabR
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Do the trapping of Shift + Tab key between the 
*!             2 screens Mat400_6 and Mat400_8
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedure : lfTrapRoll
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedures : lpSTabR
*!*************************************************************
PROCEDURE lpSTabR

DO CASE
  CASE WONTOP(lcRollTitl)  
    ACTIVATE WINDOW ('lcWinch8')
    _CUROBJ = OBJNUM(pbRClose)
  CASE WONTOP('lcWinch8') AND _CUROBJ = OBJNUM(pbRNew) 
    ACTIVATE WINDOW (lcRollTitl)
  OTHERWISE
    _CUROBJ =  _CUROBJ - 1  
ENDCASE
ON KEY LABEL BACKTAB DO lpSTabR

*!*************************************************************
*! Name      : lpEscR
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Trapping the Escape button.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedure : lfTrapRoll
*!*************************************************************
PROCEDURE lpEscR

ACTIVATE WINDOW lcWinch8
_CUROBJ = OBJNUM(pbRClose)
KEYBOARD CHR(13)

*!*************************************************************
*! Name      : lfvRRemove
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Remove a record from lcTmpRolls (Case (+)adj.)
*!             or select roll from lcTmpRolls (Case transfer)
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Screen : Mat400_8
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function : lfActBrwRo, lfvSelect
*!*************************************************************
FUNCTION lfvRRemove
PRIVATE lcStst, lcMsg,lnAlias,llFound

lnAlias = SELECT()
llFound = .F.

IF lcRollRP = lcRemveP
  *-- Case the contents of the pbRremove = Remove
  SELECT Rolls
  lnRolTag = VAL(SYS(21))
  SET ORDER TO TAG ROLLS
  IF SEEK (&lcTmpRolls..cRollid+&lcTmpRolls..cRollItem+&lcTmpRolls..Color+"1", "Rolls") &&&&&AND lcType = 'A'
    llFound = .T.
  ENDIF
  SET ORDER TO (lnRolTag) IN Rolls
  lcMsg = 'Are you sure you want to delete this line.'
  IF gfDialog('!',lcMsg,"\<Yes;\!\<No") = 1
    IF llFound
      =gfModalGen('QRM36104B36000','ALERT')
      *=gfDialog('?','YOU ARE NOT ALLOWED TO DELETE THIS ROLL',"\<Yes;\!\<No") 
      SELECT(lnAlias)        
      RETURN
    ENDIF
    SELECT (lcTmpRolls)
    lnUsrApply = lnUsrApply  - nQtyBal
    llNothing  = lfRRefre()
    DELETE
    PACK
    REPLACE ALL cMarker WITH lcUnMark
    lcStst = IIF(lnUsrApply > 0 OR RECCOUNT(lcTmpRolls) > 0, "ENABLE", "DISABLE")
    SHOW GET pbRRemove &lcStst
    SHOW GET pbRModify &lcStst
    llNothing  = lfActBrwRo()
  ENDIF
ELSE
  *-- Case the contents of the pbRremove = UnSelect
  llNoThing = lfvSelect('U',.T.)
ENDIF
SELECT(lnAlias)        
*!*************************************************************
*! Name      : lfUpdRolls
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Update Rolls file in case of transfer and 
*!             rolls 
*!*************************************************************
FUNCTION lfUpdRolls
PARAMETERS lcNewWar,lcRsession,lcSession
PRIVATE lnAlias

lnAlias = SELECT()
SELECT (lcTmpRolls)
SET RELATION TO 
SET RELATION TO cRollItem+COLOR+CWARECODE+DYELOT+CROLLID+TRANCD+CRSESSION INTO Rolls ADDITIVE

*-- Update the master rolls file
SCAN FOR !EMPTY(CHECK)
  *-- Get the date from the receive record of the current
  *-- roll and update it's balance with 0
  SELECT Rolls
  SCATTER MEMVAR

  *-- Update the balance of the reciving record of the rolls
  REPLACE nQtyBal WITH 0

  *-- Add new issu record from the old warehouse
  APPEND BLANK
  GATHER MEMVAR
  REPLACE nQty      WITH m.nQtyBal;
          cSession  WITH lcSession;
          cISession WITH lcSession;
          TranCd    WITH "2"

  *-- Add new recive record to the new warehouse
  APPEND BLANK
  m.cwarecode = lcNewWar
  m.crsession = lcRsession
  m.csession  = lcRsession
  m.cisession = SPACE(6)
  m.trancd    = '1'
  GATHER MEMVAR
ENDSCAN
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvSelect
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Select or unselect a record from lcTmpRolls
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Passed :
*!        Parameters : 
*!           lcAll : To endicate if select "S"
*!                   or unselect "U" mode
*!           llAct : .T. if the functio is called from pbs.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Function : lfvpbNew, lfvRRemove,lfEnter
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function : lfRRefre,lfActBrwRo
*!*************************************************************
FUNCTION lfvSelect
PARAMETERS lcAll, llAct
PRIVATE llUnSele, llSele

llSele   = .F.
llUnSele = .F.

*-- This function maybe called from the select button or unselect 
llChecked  = !EMPTY(CHECK)
*-- if select all and the current record is selected do nothing
IF (lcAll = 'S' .AND. llChecked) .OR. (lcAll = 'U' .AND. !llChecked)
  RETURN
ENDIF
llChecked  = IIF(lcAll = 'S',.F.,.T.)
lnApply    = nQtyBal
nApply     = nQtyBal
lnFactor   = IIF(llChecked,-1,1)
lnUsrApply = lnUsrApply + (lnApply*lnFactor)

REPLACE CHECK    WITH IIF(llChecked," ","û"  );
        nIssued  WITH nIssued  + (lnApply*lnFactor)    ;
        lStatus  WITH 'M'

lnLotAmont = lnLotAmont  + ((lnApply * MatInvJL.nUnitCost)*lnFactor)
llNothing  = lfRRefre()

SCAN
  IF !EMPTY(CHECK)
    llUnSele = .T.
    EXIT
  ENDIF
ENDSCAN
 
SCAN
  IF EMPTY(CHECK)
    llSele = .T.
    EXIT
  ENDIF
ENDSCAN

GO lnNewRec
lcUnSele =  IIF(llUnSele,"ENABLE","DISABLE")
lcSele   =  IIF(llSele,  "ENABLE","DISABLE")
SHOW GET pbRRemove &lcUnSele
SHOW GET pbRNew    &lcSele

IF llAct
  SHOW WINDOW (lcRollTitl) REFRESH
  *llNothing = lfActBrwRo()
ENDIF  

*!*************************************************************
*! Name      : lfvGoToBr
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Do trapping between the 2 screens Mat400_6 and 
*!             Mat400_8
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Screen :Mat400_8
*!*************************************************************
FUNCTION lfvGoToBr

DO CASE 
  CASE LASTKEY() = 9
    ACTIVATE WINDOW (lcRollTitl)
  CASE LASTKEY() = 15
    _CURROBJ = OBJNUM(pbRClose)
ENDCASE

*!*************************************************************
*! Name      : lpRoMtBrow
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Display the browse of lots or rolls
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedure : lpSvInMstr
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Screen   : Mat400_5
*!         Function : lfRBrow
*!*************************************************************
PROCEDURE lpRoMtBrow
PRIVATE lnAlias

lnAlias = SELECT()
SELECT (lcTmpAdj)

*-- Initialize the variables before calling the screen
lnLotAmont = 0 
lcRlBrFild = SPACE(01)
lnTotApply = &lcTmpAdj..TotAdj
lnUsrApply = 0 
lcRLTit    = IIF(llTrkRolls .AND. Fabric.ltrkrolls,'Rolls','Lots')
llNothing  = lfRBrow()

IF lcType = 'A' AND lcMtCstMth $ "FI" AND !(llTrkRolls .AND. Fabric.ltrkrolls)
  =lfOldLots()
ENDIF
IF ((lcType = 'A'   .AND. lnTotApply < 0) .OR. ;
    (lcType = 'T' )) .AND. llEmpTmpRo
  DO lpDelUnLock WITH "3"
  SELECT (lcTmpAdj)
  REPLACE TotAdj WITH 0
  RETURN
ENDIF  
lcRNewSta  = IIF((lcType $ 'AP' .AND. lnTotApply > 0) .OR. (lcType = 'T' .AND. llTrkRolls .AND. Fabric.ltrkrolls .AND. !llEmpTmpRo),'ENABLE','DISABLE')
lcRRemSta  = IIF(lcType = 'P' AND RECCOUNT(lcTmpRolls) > 0 ,'ENABLE','DISABLE')
lcRModSta  = IIF(lcType $ 'AP' .AND. lnTotApply > 0 AND RECCOUNT(lcTmpRolls)>0,'ENABLE','DISABLE')
IF lcType = 'T' .AND. llTrkRolls .AND. Fabric.ltrkrolls
  lcRollNP = lcSelecP
  lcRollRP = lcUnSelP
ENDIF

*-- adjusting showing Lots or Rolls screen
IF !(lcMtCstMth $ 'FI')
  *-- Call the rolls or lots browse screen
  PUSH KEY
  ON KEY
  lcSysMen = SET('SYSMENU')
  SET SYSMENU ON
  DEFINE PAD _BROWSE OF _MSYSMENU PROMPT "" KEY CTRL+B
  ON SELECTION PAD _BROWSE OF _MSYSMENU ACTIVATE WINDOW (lcRollTitl)
  DO (gcScrDir+gcWinAppl+'\MAINVCT5.SPX')
  RELEASE PAD _BROWSE OF _MSYSMENU
  SET SYSMENU &lcSysMen
  POP KEY

ENDIF  
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvRModify
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Modify a record in lcTmpRolls
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Screen   : Mat400_8
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Screen   : Mat400_9
*!         Function : lfActBrwRo, lfRRefre
*!*************************************************************
FUNCTION lfvRModify

*-- Initilize the variables from lcTmprolls File
lcRollid  = &lcTmprolls..cRollid
lnRolBal  = &lcTmprolls..nQtyBal
lnOldVal  = &lcTmprolls..nQtyBal
llRModify = .T. 
lcRIdSta  = "DISABLE"

*-- Call the rolls entry screen
DO (gcScrDir+gcWinAppl+'\MAINVCT9.SPX')

llNothing = lfActBrwRo()
llNothing = lfRRefre()
lcStat    = IIF(lnUsrApply > 0 OR RECCOUNT(lcTmpRolls) > 0, "ENABLE", "DISABLE")
SHOW GET pbRRemove &lcStat
SHOW GET pbRModify &lcStat

*!*************************************************************
*! Name      : lfEnter
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : To select and unselect roll at transfer 
*!             on pressing enter in the browse
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Funnction   : lfRBrow
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Function : lfvSelect
*!*************************************************************
FUNCTION lfEnter

IF lcType = 'T' AND llTrkRolls .AND. Fabric.ltrkrolls AND LASTKEY() = 13
  llNothing = lfvSelect(IIF(EMPTY(Check), "S", "U"),.F.)
ENDIF  
*!*************************************************************
*! Name      : lfSvFInv
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Update the DFInvAdj file from lcTmpAdj
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Funnction   : lpSvInMstr
*!*************************************************************
FUNCTION lfSvFInv
PRIVATE lnAlias , lcOldOrd
STORE '' TO lcOldOrd
lnAlias = SELECT()

SELECT DFInvAdj
lcOldOrd = ORDER()
SET ORDER TO TAG DFInvAdj

IF EMPTY(&lcTmpAdj..CTRN_SEQ)
  APPEND BLANK
ELSE
  =SEEK(&lcTmpAdj..CFROMWARE + &lcTmpAdj..Fabric + &lcTmpAdj..Color + &lcTmpAdj..CTRN_SEQ,'DFInvAdj')
ENDIF

GATHER MEMVAR
REPLACE CTRN_SEQ   WITH IIF(EMPTY(&lcTmpAdj..CTRN_SEQ) , lcGlSess , &lcTmpAdj..CTRN_SEQ)

IF !EMPTY(lcOldOrd)
  SET ORDER TO TAG &lcOldOrd
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfSvInGl
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Update the GL files from lctmpGlDis
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Funnction   : lpSvInMstr
*!*************************************************************
FUNCTION lfSvInGl
PRIVATE lnAlias

lnAlias = SELECT()
IF llGlLink 
  SELECT (lctmpGlDis)
  *-- If temp. file not empty
  IF RECCOUNT() <> 0
    SELECT GLDIST  
    APPEND FROM (gcWorkDir+lctmpGlDis)
  ENDIF
ENDIF  
SELECT(lnAlias)

*!*************************************************************
*! Name      : lpEscD
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose   : Trapping the Escape button.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Function : lfTrapKeys
*!*************************************************************
PROCEDURE lpEscD

ACTIVATE WINDOW lcWinCh4
_CUROBJ = OBJNUM(pbCancel)
KEYBOARD CHR(13)

*!*************************************************************
*! Name       : lfTrapKD
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose    : To do the trapping for Mat400_ 4
*!
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Program nMat400 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedures : lpEscD
*!*************************************************************
FUNCTION lfTrapKD

ON KEY LABEL ESC        DO lpEscD
ON KEY LABEL CTRL+ENTER DO lpEscD

*!*************************************************************
*! Name       : lpShow
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose    : for the show of objects for each mode in the screen
*!
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Program mainvc
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedures : 
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
   SHOW GET lcFromWare ENABLE
   SHOW GET lcToWare   ENABLE
   SHOW GET ibFromBrow ENABLE
   SHOW GET ibToBrow   ENABLE
   SHOW GET pbBrws     DISABLE
   SHOW GET pbNew      &lcNewStat
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
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose    : to clear the trap after leaving the browse screen
*!
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Program mainvc
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedures : 
*!*************************************************************
FUNCTION lfClearTrap

IF WONTOP() <> lcBrowTtl
   ON KEY LABEL ESC &lcOldEscTrap
   ON KEY LABEL TAB
   ON KEY LABEL BACKTAB
ENDIF   

*!*************************************************************
*! Name       : lfPhyRoll
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose    : get the old rolls and replace the balance with zero in case of physical
*!
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Program mainvc
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedures : 
*!*************************************************************
FUNCTION lfPhyRoll
PRIVATE lnAlias

lnAlias = SELECT()
SELECT Rolls
lnRolTag = VAL(SYS(21))
SELECT * FROM  Rolls ,(lcTmpAdj);
  WHERE cRollItem+Rolls.COLOR+CWARECODE+ROLLS.DYELOT+CROLLID+TRANCD+Rolls.CRSESSION =;
        &lcTmpAdj..Fabric+&lcTmpAdj..Color+&lcTmpAdj..cFromWare+&lcTmpAdj..Dyelot ;
        AND nQtyBal>0 ;           
  INTO DBF (gcWorkDir+lctmpRolls) 
SET ORDER TO TAG ROLLS IN Rolls
SCAN
  SCATTER MEMVAR
  m.cIsession = lcGlSess
  m.cSession  = lcGlSess
  m.cRsession = SPACE(06) 
  m.nQty      = m.nQtyBal
  m.Color     = m.Color_a
  m.Trancd    = '2'
  SELECT ROLLS
  =SEEK(&lcTmpRolls..cRollId+&lcTmpRolls..cRollItem+&lcTmpRolls..Color_a)
  SCAN WHILE (cRollId+cRollItem+Color = &lcTmpRolls..cRollId+&lcTmpRolls..cRollItem+&lcTmpRolls..Color_a) AND TRANCD = '1'
    REPLACE nQtyBal WITH 0
  ENDSCAN
  APPEND BLANK
  GATHER MEMVAR
ENDSCAN  
ZAP
SET ORDER TO (lnRolTag) IN Rolls
SELECT(lnAlias)
*!*************************************************************
*! Name       : lfPhyRoll
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose    : get the old lots when the costing is lot or LIFO or FIFO
*!
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Program mainvc
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedures : 
*!*************************************************************

FUNCTION lfOldLots
PRIVATE lnLotQty,lnAlias

lnAlias = SELECT()
lnLotQty = ABS(lnTotApply)
SELECT(lcTmpRolls)
SCAN
  IF lnLotQty > 0 
    IF (nReceived-nIssued) <= lnLotQty
      REPLACE nApply   WITH nReceived-nIssued,;
              nBalance WITH 0,;
              lStatus  WITH 'M'
      lnLotQty = lnLotQty - nReceived + nIssued
      lnUsrApply = lnUsrApply - nApply
    ELSE  
      REPLACE nApply   WITH lnLotQty ,;
              nBalance WITH nReceived - (nIssued+lnLotQty),;
              lStatus  WITH 'M'
      lnLotQty = 0
      lnUsrApply = lnUsrApply - nApply
    ENDIF  
  ENDIF
ENDSCAN  
SELECT(lnAlias)

*!*************************************************************
*! Name       : lfvRsCode
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose    : validate the adj. code reason
*!
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Program mainvc
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedures : 
*!*************************************************************
FUNCTION lfvRsCode

lcAdjReason = laAdjCode[lnAdjCode,2]
DECLARE laTrmRltFd[1,2]
laTrmRltFd[1,1] = 'GLACCOUNT'
laTrmRltFd[1,2] = 'lcAdjAcct'
=gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")

*!*************************************************************
*! Name       : lfvDate
*! Developer   : Hossam El Etreby[HDM]
*! Date        : 05/10/1999
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ        
*! Purpose    : valodate the posting date
*!
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ         
*! Called from : 
*!         Program mainvc
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Calls : 
*!         Procedures : 
*!*************************************************************
FUNCTION lfvDate
 
IF LASTKEY() = 13
  IF !CheckPrd(ldPost,'lcGlFYear','lcGlPeriod',;
                  IIF(lcType='P','MP','MA')) OR EMPTY(ldPost)
    ldPost  = gdSysDate
    _CUROBJ = OBJNUM(ldPost)
  ENDIF  
ENDIF  
*!*************************************************************
