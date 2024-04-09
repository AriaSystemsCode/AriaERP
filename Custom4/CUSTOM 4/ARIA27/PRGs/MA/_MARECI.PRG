*:************************************************************************
*: Program file  : MARECI.PRG
*: Program desc. : Program to recieve Material P/O and Return material P/O
*:
*: System        : Aria Apparel System
*: Developer     : SWK - SAMAH WILSON KIROLLOS
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:*************************************************************
*: Passed Parameters  : lcScrType
*:*************************************************************
*B601927,1 Hesham El-Sheltawi 12/17/97
*B601927,1 make the program call the uncomplete session global functions
*B601927,1 instead of making these functions as local functions
*E300954,1 SWK 08/09/1998 - Replace the PO/Clrearing account('017') with WIP account('013')
*E300954,1                - Call the arranging dyelot screen in case using 
*E300954,1                  dyelots and calling the program with P/O 
*B602197,1 HDM 11/16/1998 Update On Order Field In Fabric And Fabdye Files
*B602179,1 AMM 11/25/98 Erase temporary files when quitting the program
*B602331,1 HDM 12/17/1998 We Need To assign the warehouse For 
*                         Both Returns and PO
*B602843   AHM 04/28/1999 fix the bug of receiving all fabrics received qty 
*B602843                  on the last received fabric
*B602851,1 HDM 08/17/1999 Fixing Bugs when restoring uncomplete session
*B602459,1 HDM 08/17/1999 WareHouse should be location in the lines browse
*B603233,1 HDM 10/22/1999 Declare lnRet and default it to 0
*                         remove lfUpdMatInvJl , lfUpdFabDye ,lfUpdFabric
*                         from program body as gfMatCrl does the same
*B603263,2 HDM 11/03/1999 Add new Parameter when calling gfMatCrl
*                         to Indicate it's the PO receiving program
*B603188,1 HDM 11/07/1999 Control rolls from gfMatcrl to void calling
*                         Rolls screen when saving
*                         and stop saving if ther is no rolls entered
*B802989,1 ADEL 05/18/2000 When recieving PO by Rolls and recieve quantity 
*B802989,1				   more or less the qty ordered then comes a message
*B802989,1				   "Applied quantity....". This message contains
*B802989,1                 the word 'adjustement'. So correct its spelling to 
*B802989,1                 'adjustment' in syddlobj file in message no 'M36067'
*B802989,1			       module 'MA'.
*B803360,1 BWA 08/09/2000 Fix the bug of the wrong spelling on Issue return material screen.
*B603290,1 ADEL 08/05/2000 When reveiving Material PO damage/Second quality lines, make the browse 
*B603290,1                 show the Purchased damage/Second quality fabrics only.
*B603812,1 ABD 09/12/2000 Fix bug that when you in issue return and press rolls button 
*B603812,1 ABD            And return rolles less than the PO Qty it return Zero.
*B603813,1 ABD 09/13/2000 Fix bug when recive Qty rolles less than the Orginal Qty the 
*B603813,1 ABD            The Tot.Rec filed will contain wrong figures.
*B803712,1 ABD 10/04/2000 Enable rolls button if recive only.
*B603950,1 ABD 10/12/2000 Receiving material P.O error.
*B604051,1 AAN 19/12/2000 when pressing the save button there is a message "one or more records has no lots".
*B803914,1 AAN 20/12/2000 Over receiving teh Material PO doesn't update POFHDR with status complete
*B803914,1 AAN 20/12/2000 and also leave the open qty as is.
*B604580,1 HBG 07/08/2001 Fix bug of Wrong Ordered qty on the locations level       
*B604678,1 MHM 08/20/2001 Fix the bug of on order QTY. in case of over receive is wrong
*C200255,1 ADEL 23/12/2001 Update MATINVJL with custom Voucher no for CON10.
*C200256,1 ADEL 27/12/2001 Print Received Qty for CON10.
*C102525,1 RAE 01/27/2002  view the PO returns issued against the selected PO.
*B605212,1 ADEL 01/28/2002 Print Rolls and fix - along with this bug's specx - updating receiving
*B605212,1                 session and date.
*E301797,1 ADEL 01/20/2002 Implement support for MATERIAL in transit.
*B605659,1 RAE 03/06/2002  Fix the problem of printing the Roll Label from Receive Material 
*						   Manufacturing Order
*B605944,1 KHM 05/22/2002 Changing the CTRL+C to be CTRL+S in order
*B605944,1                to allow the copy and past feature.
*C200376,1 AMH 07/28/2002 Trigger for customer PUFFA to update style critical path file.
*B606231,1 KHM 08/05/2002 Fix the bug of not displaying the correct figure of use quantity.
*B606339,1 KHM 08/26/2002 Fix the problem of record is in use by another user.
*B606455,1 KHM 09/16/2002 Fix the bug of changing the PO status to "C" when over receiving.
*B606285,1 ABD 10/09/2002 Fix many bugs in cost method 'FI' in the issue return PO. Where you can
*B606285,1                not issue return PO if there is no available qty (on hand).
*B606625,1 NAD 12/19/2002 Fix Bug Record is in use by another.   
*C200472,1 KHM 03/10/2003 Add a new custom option Rolls Entry screen for (Dare Bare)
*:*********************************************************************************************
PARAMETERS lcScrType
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DECLARE ladata[55],laKeyField[2,4],laDefProc[10],laVariables[6] 
*E301797,1 (Begin) Add shipment variables for uncomplete session.
DECLARE laVariables[13] 
*E301797,1 (End)
lcScrType = IIF(TYPE('lcScrType') ='C',lcScrType,'P')
*B603188,1 [Start] Define lcTmpRoll , lcTmpJour , lcFullRoll to be used by
*                  gfMatCrl When calling rolls screen
*STORE '' TO laData,laKeyField,lcWinCh1,lcWinCh2,lcWinCh3,lcModal,lcTmpPo,lcDesc,;
            lcPCurr, lcDCurr ,lcUnMark,lcWare,lcUBuy,lcUUse,lcOldVen,;
            lcRecSess,lcLinkCode,lcLinkPO,laVariables,lcOldWare,lcOldDye,;
            lcTmpRolls,lcDyelot,lcGrade,lcQuaGrade,lcFabGrade,lcQuaFab,lcQuaClr,;
            lcOldEscTrap,lcTmpMtIv,lcTmpJour,lcGlDTemp,lcTemLoc,lcGlPeriod,lcGLFYear

STORE '' TO laData,laKeyField,lcWinCh1,lcWinCh2,lcWinCh3,lcModal,lcTmpPo,lcDesc,;
            lcPCurr, lcDCurr ,lcUnMark,lcWare,lcUBuy,lcUUse,lcOldVen,;
            lcRecSess,lcLinkCode,lcLinkPO,laVariables,lcOldWare,lcOldDye,;
            lcTmpRolls,lcDyelot,lcGrade,lcQuaGrade,lcFabGrade,lcQuaFab,lcQuaClr,;
            lcOldEscTrap,lcTmpMtIv,lcTmpJour,lcGlDTemp,lcTemLoc,lcGlPeriod,lcGLFYear,;
            lcTmpRoll , lcFullRoll , lcFromPo
*E301797,1 (Begin) Initilize needed varaible.
STORE '' TO lcWinCh8,lcTmpPoSp,lcShpCode,lcAirWayB,lcShpRef
lnRecType = 1
lnCartons = 0
ldEnterd  = {}
ldETA     = {}
lcWinCh1  = ''
lcWinCh9  = ''
llShpPO   = .F.
llNewItem = .F.
llbrowse  =.F. 
STORE 1   TO lnCurrUnt1,lnCurrUnt2,lnCrRt1,lnCrRt2
STORE ''  TO lcIType1,lcIType2,lcIType3,lcIType4
= lfOpn_Rest(gcDatadir,'matshphd','matshphd')
= lfOpn_Rest(gcDatadir,'matshpdt','matshpdt')
lcDetTtl  = ' Receiving lines '
*E301797,1 (End)

*B603188,1 [End]

STORE 0  To lnTotQty1,lnTotQty2,lnTotQty3,lnNewRec,lnOldQty,lnQtyUse,;
            lnNewCost,lnOldStk,lnTotApply,lnUsrApply,lnOldBal,lnTotQty,lnLastRec,lnOldVal,;
            lnOldCost
STORE 1 TO  lnType,lnOldType,lnRateP,lnRateD,lnUnitP,lnUnitD
ldDate     = gdSysDate
ldPost     = gdSysDate
lnware     = 1
llNewLine  = .F.
llRollBrow = .T.
llFrEnter  = .T.
lcFrnSign1 = '*'
lcFrnSign2 = '*'
lnSessNo   = gnProgCopy


*-- E301077,65 Move this part after gfstup (Start)

*-- Retsore some global setting from the setup file 
*-- and load them to local variables 
*lcMtCstMth = gfGetMemVar('M_MatCstMth')  
*lcSession  = gfsequence('CSESSION')
*llAvgCost  = ALLTRIM(gfGetMemVar('M_COST_METH'))= 'A'
*llMultiWH  = ALLTRIM(gfGetMemVar('M_WareHouse'))= 'Y'
*llDyelot   = ALLTRIM(gfGetMemVar('M_MATDYE'))   = 'Y' 
*llTrkRolls = ALLTRIM(gfGetMemVar('M_TrkRolls')) = 'Y'
*llWareLoc  = ALLTRIM(gfGetMemVar('M_WARELOC'))  = 'Y'
*llGlLink   = ALLTRIM(gfGetMemVar('M_LINK_GL'))  = 'Y'
**MAN
*llCostPrv  = gfUserPriv('MA','STY100')
*llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')
*llMulCurr  = gfGetMemVar('llMulCurr')  
*llEditExRt = gfGetMemVar('LLEDITEXRA')
*llApIstall = (OCCURS('AP',gcCmpModules)<>0)
*-- E301077,65 Move this part after gfstup (End)

lcDet_Ttl  = 'Receiving lines'
lcRollTit  = 'Rolls'
lcLotlTit  = 'Available lots'
lcTrancd   = '2'
lcMark     = '>'
lcCloseP   = gcBmpHome + "CLS.BMP"
lcCanclP   = gcBmpHome + "CAN.BMP"
lcNewP     = gcBmpHome + "NEW.BMP"
lcRemveP   = gcBmpHome + "REM.BMP"
lcSelecP   = gcBmpHome + "SEL1.BMP"
lcUnSelP   = gcBmpHome + "UNSEL.BMP"
lcExtKey   = gcBmpHome + "ExtKey.BMP"
lcRollNP   = lcNewP
lcRollRP   = lcRemveP
llEdCost   = .F.
llNoshow   = .F.
lcOldEscTrap  = ON('Key','Esc')
laDefProc[9]  = .F.
laDefProc[10] = .F.
DECLARE laWare[1]

DIMENSION laType[4,2]
laType[1,1] = 'Receive'
laType[2,1] = 'Cancel'
laType[3,1] = 'Second Quality'
laType[4,1] = 'Damage'

*E301797,1 (Begin) Initilize needed varaible.
DIMENSION laRecType[2,2],laECost[5]
STORE ' ' TO laRecType,lcOldValue
STORE 0   TO laECost
laRecType[1,1]='Receive by P/O'
laRecType[1,2]='I'
laRecType[2,1]='Receive by Shipment'
laRecType[2,2]='S'
*E301797,1 (End)

lcScrMode = 'S' 
IF !gfSetup()
  RETURN
ENDIF

*-E301077 (Start)
DIMENSION laSetUp[9,2]
laSetup[1,1] = 'M_MatCstMth'
laSetup[2,1] = 'M_COST_METH'
laSetup[3,1] = 'M_WareHouse'
laSetup[4,1] = 'M_MATDYE'
laSetup[5,1] = 'M_TrkRolls'
laSetup[6,1] = 'M_WARELOC'
laSetup[7,1] = 'M_LINK_GL'
laSetup[8,1] = 'llMulCurr'
laSetup[9,1] = 'LLEDITEXRA'

= gfGetMemVar(@laSetUp)  

lcMtCstMth = laSetUp[1,2]
llAvgCost  = ALLTRIM(laSetUp[1,2]) = 'A'
llMultiWH  = ALLTRIM(laSetUp[3,2]) = 'Y'
llDyelot   = ALLTRIM(laSetUp[4,2]) = 'Y' 
llTrkRolls = ALLTRIM(laSetUp[5,2]) = 'Y'
llWareLoc  = ALLTRIM(laSetUp[6,2]) = 'Y'
llGlLink   = ALLTRIM(laSetUp[7,2]) = 'Y'
llMulCurr  = laSetUp[8,2]
llEditExRt = laSetUp[9,2]

llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')
llApIstall = (OCCURS('AP',gcCmpModules)<>0)
lcSession  = gfsequence('CSESSION')
*-E301077 (End)

lcBaseFile = ''
SET ORDER TO TAG FABRIC IN FABRIC
SET ORDER TO TAG FABDYE IN FABDYE
DO CASE
  CASE lcScrType = UPPER('P')
    lcType     = 'P/O #'
    lcKeyType  = 'P'
    lcDialog   = 'PO REC'
  CASE lcScrType = UPPER('R')
    lcType     = 'Return #'
    lcKeyType  = 'R'
    lcDialog   = 'RET REC'
ENDCASE
lcUnsmsPrg= PADR('MAT_POREC'+lcKeyType,10)
=lfAvilWare()
IF !WEXIST(gcBaseWind)
  qWareHouse = Warehous.cWareCode
  lcWinCh1   = gfTempName()
  lcWinCh2   = gfTempName()
  lcWinCh3   = gfTempName()
  lcWinCh4   = gfTempName()  
  lcWinCh5   = gfTempName()
  lcWinCh6   = gfTempName()    
  lcWinCh7   = gfTempName()      
  *E301797,1 (Begin) Initilize needed varaible.
  lcWinCh8   = gfTempName()      
  lcWinCh9   = gfTempName()      
  lcTmpPoSp  = gfTempName()
  *E301797,1 (End)

  lcTmpPo    = gfTempName()   
  lcTmpMtIv  = gfTempName()
  lcTmpJour  = gfTempName()
  laVariables[1] = 'lcScrMode'
  laVariables[2] = 'lnTotQty1'
  laVariables[3] = 'lnTotQty2'
  laVariables[4] = 'lnTotQty3'
  laVariables[5] = 'ldPost'
  laVariables[6] = 'ldDate'    
  *E301797,1 (Begin) Add shipment variables for uncomplete session.
  laVariables[7]  = 'lnRecType'
  laVariables[8]  = 'lcShpCode'
  laVariables[9]  = 'lcAirWayB'
  laVariables[10] = 'lcShpRef'
  laVariables[11] = 'lnCartons'
  laVariables[12] = 'ldEnterd'
  laVariables[13] = 'ldETA'
  *E301797,1 (End)
  
    SELECT MATINVJL
*   = AFields(laFileStru)
*    lnNewFld = ALEN(laFileStru,1)+1
*    DIMENSION laFileStru[lnNewFld,4]
*    laFileStru[lnNewFld,1] = 'cMarker'
*    laFileStru[lnNewFld,2] = 'C'
*    laFileStru[lnNewFld,3] = 1
*    laFileStru[lnNewFld,4] = 0

*    lnNewFld = ALEN(laFileStru,1)+1
*    DIMENSION laFileStru[lnNewFld,4]
*    laFileStru[lnNewFld,1] = 'cSele'
*    laFileStru[lnNewFld,2] = 'C'
*    laFileStru[lnNewFld,3] = 1
*    laFileStru[lnNewFld,4] = 0

*    lnNewFld = ALEN(laFileStru,1)+1
*    DIMENSION laFileStru[lnNewFld,4]
*    laFileStru[lnNewFld,1] = 'nApply'
*    laFileStru[lnNewFld,2] = 'N'
*    laFileStru[lnNewFld,3] = 8
*    laFileStru[lnNewFld,4] = 0

*    lnNewFld = ALEN(laFileStru,1)+1
*    DIMENSION laFileStru[lnNewFld,4]
*    laFileStru[lnNewFld,1] = 'nBalance'
*    laFileStru[lnNewFld,2] = 'N'
*    laFileStru[lnNewFld,3] = 8
*    laFileStru[lnNewFld,4] = 0
*    CREATE TABLE  (gcWorkDir+lcTmpJour) FROM ARRAY laFileStru
*    INDEX ON cFabric+cColor+cWareCode+cDyelot+cRsession+cIsession TAG (lcTmpJour) OF (gcWorkDir+lcTmpJour)
  
  IF llGlLink
    =gfOpenFile(gcDataDir+'GLDIST',gcDataDir+'Gldistac','SH')
*    = AFields(laFileStru)
    lcGlDTemp = gfTempName()
*    CREATE TABLE  (gcWorkDir+lcGlDTemp) FROM ARRAY laFileStru
  ENDIF
  IF llWareLoc 
    DIMENSION laSource[1],laTarget[1]
    SELECT WhsLoc
    SET ORDER TO TAG Whslocst
*    = AFIELDS(laStru)
    lcTemLoc  = gfTempName()
*    CREATE TABLE (lcTemLoc) FROM ARRAY laStru
*    INDEX ON STYLE+COLOR+CWARECODE+CLOCATION TAG (lcTemLoc) OF (gcWorkDir+lcTemLoc)
  ENDIF  
  *B603188,1 [Start] stop creating lcTmpRolls File
  *IF llTrkRolls
  *  SELECT ROLLS
  *  SET ORDER TO TAG ROLLITEM
  *B603188,1 [End]
  
*    = AFields(laFileStru)
*    lnNewFld = ALEN(laFileStru,1)+1
*   DIMENSION laFileStru[lnNewFld,4]
*   laFileStru[lnNewFld,1] = 'cMarker'
*   laFileStru[lnNewFld,2] = 'C'
*   laFileStru[lnNewFld,3] = 1
*    laFileStru[lnNewFld,4] = 0

*    lnNewFld = ALEN(laFileStru,1)+1
*    DIMENSION laFileStru[lnNewFld,4]
*    laFileStru[lnNewFld,1] = 'cSele'
*    laFileStru[lnNewFld,2] = 'C'
*    laFileStru[lnNewFld,3] = 1
*    laFileStru[lnNewFld,4] = 0

    *B603188,1 [Start] stop creating lcTmpRolls File
    *lcTmpRolls = gfTempName()
    *B603188,1 [End]
*    CREATE TABLE  (gcWorkDir+lcTmpRolls) FROM ARRAY laFileStru
*    INDEX ON cRollItem+Color+cWarecode+Dyelot+cRollid TAG (lcTmpRolls) OF (gcWorkDir+lcTmpRolls)
  ENDIF
  SELECT APVENDOR
  SET ORDER TO TAG VENCODE
 
  *--E301077,65 (Start)
  * =gfOpenFile(gcSysHome+'SyuEror',gcSysHome+'cSession','SH')   
  *--E301077,65 (End)
  
  SELECT POFLN
  SET ORDER TO TAG POFLN
*  = AFields(laFileStru)
*  lnNewFld = ALEN(laFileStru,1)+1
*  DIMENSION laFileStru[lnNewFld,4]
*  laFileStru[lnNewFld,1] = 'cMarker'
*  laFileStru[lnNewFld,2] = 'C'
*  laFileStru[lnNewFld,3] = 1
*  laFileStru[lnNewFld,4] = 0

*  lnNewFld = ALEN(laFileStru,1)+1
*  DIMENSION laFileStru[lnNewFld,4]
*  laFileStru[lnNewFld,1] = 'UnitP'
*  laFileStru[lnNewFld,2] = 'N'
*  laFileStru[lnNewFld,3] = 4
*  laFileStru[lnNewFld,4] = 0

*  lnNewFld = ALEN(laFileStru,1)+1
*  DIMENSION laFileStru[lnNewFld,4]
*  laFileStru[lnNewFld,1] = 'UnitD'
*  laFileStru[lnNewFld,2] = 'N'
*  laFileStru[lnNewFld,3] = 4
*  laFileStru[lnNewFld,4] = 0

*  lnNewFld = ALEN(laFileStru,1)+1
*  DIMENSION laFileStru[lnNewFld,4]
*  laFileStru[lnNewFld,1] = 'SignP'
*  laFileStru[lnNewFld,2] = 'C'
*  laFileStru[lnNewFld,3] = 1
*  laFileStru[lnNewFld,4] = 0

*  lnNewFld = ALEN(laFileStru,1)+1
*  DIMENSION laFileStru[lnNewFld,4]
*  laFileStru[lnNewFld,1] = 'SignD'
*  laFileStru[lnNewFld,2] = 'C'
*  laFileStru[lnNewFld,3] = 1
*  laFileStru[lnNewFld,4] = 0

*  lnNewFld = ALEN(laFileStru,1)+1
*  DIMENSION laFileStru[lnNewFld,4]
*  laFileStru[lnNewFld,1] = 'PoGrade'
*  laFileStru[lnNewFld,2] = 'C'
*  laFileStru[lnNewFld,3] = 1
*  laFileStru[lnNewFld,4] = 0

*  lnNewFld = ALEN(laFileStru,1)+1
*  DIMENSION laFileStru[lnNewFld,4]
*  laFileStru[lnNewFld,1] = 'CanQty'
*  laFileStru[lnNewFld,2] = 'N'
*  laFileStru[lnNewFld,3] = 11
*  laFileStru[lnNewFld,4] = 3

*  CREATE TABLE  (gcWorkDir+lcTmpPo) FROM ARRAY laFileStru
*  INDEX ON cType+POMAT+FABRIC+COLOR+cWareCode+Trancd+DYELOT+cFabGrade TAG (lcTmpPo) OF (gcWorkDir+lcTmpPo)
*  =gfOpenFile (gcWorkDir+lcTmpPo,gcWorkDir+lcTmpPo,"EX")

*B603188,1 [Start] stop creating lcTmpRolls File
*ENDIF
*B603188,1 [End]

PUSH KEY
=lfActPad()
*B605944,1 KHM 05/22/2002 (Begin) Changing the CTRL+C to be CTRL+S in order
*B605944,1                to allow the copy and past feature.
*DEFINE PAD _BROWSE OF _MSYSMENU PROMPT "" KEY CTRL+C
DEFINE PAD _BROWSE OF _MSYSMENU PROMPT "" KEY CTRL+S
*B605944,1 KHM 05/22/2002 (End)

ON SELECTION PAD _BROWSE OF _MSYSMENU DO lpTab
DO (gcScrDir+gcWinAppl+'\MAPOREC.SPX')
POP KEY
RELEASE PAD _Option OF _MSYSMENU
RELEASE PAD _WipOptn OF _MSYSMENU

IF glQuitting
  SELECT (lcTmpPo)
  USE
  ERASE (gcWorkDir+lcTmpPo+'.DBF')
  ERASE (gcWorkDir+lcTmpPo+'.CDX') 
  ERASE (gcWorkDir+lcTmpPo+'.FPT') 

  IF USED(lcTmpMtIv)
    USE IN (lcTmpMtIv)
    ERASE (gcWorkDir+lcTmpMtIv+".DBF")
  ENDIF  

  *B604051,1 AAN Check if the file is opened before close it[Start].
  *USE IN (lcTmpJour)
  IF USED(lcTmpJour)
    USE IN (lcTmpJour)
  ENDIF
  *B604051,1 AAN Check if the file is opened before close it[End].
  ERASE (gcWorkDir+lcTmpJour+".DBF")
  ERASE (gcWorkDir+lcTmpJour+'.CDX') 
  ERASE (gcWorkDir+lcTmpJour+'.FPT') 
  *B603188,1 [Start] Don't delete rolls file as gfMatCrl Will Handle this
  *IF llTrkRolls
  *  USE IN (lcTmpRolls)
  *  ERASE (gcWorkDir+lcTmpRolls+".DBF")
  *  ERASE (gcWorkDir+lcTmpRolls+'.CDX') 
  *  ERASE (gcWorkDir+lcTmpRolls+'.FPT') 
  *ENDIF
  *B603188,1 [End]

  *B602179,1 AMM Erase temporary files when quitting the program
  IF USED(lcGlDTemp)
    USE IN (lcGlDTemp)
    ERASE (gcWorkDir+lcGlDTemp+".DBF")
    ERASE (gcWorkDir+lcGlDTemp+'.CDX') 
  ENDIF
  IF USED(lcTemLoc)
    USE IN (lcTemLoc)
    ERASE (gcWorkDir+lcTemLoc+".DBF")
    ERASE (gcWorkDir+lcTemLoc+'.CDX') 
  ENDIF
  *B602179,1 AMM  end
ENDIF  
*!*************************************************************
*! Name      : lfBrLines
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : browse the main transaction lines
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfBrLines
PRIVATE lnAlias


lnAlias = SELECT()
SELECT(lcTmpPo)

*-- B602459,1 [Start] WareHouse should be location
*lcBrowfields = [cMarker :R:H='',]+;
               [POMAT   :R:H='Po #',Fabric :R:H='Item',] +;
               [Color   :R:H='Color',cTrancd=IIF(TRANCD ='2','R',IIF(TRANCD ='4','C','D')) :R:H='Type',]+;
               [Pattern :H='Pattern',]+;
               IIF(llDyelot , [Dyelot :R:H='Dyelot',],[])+;
               IIF(llMultiWH, [cWarecode :R:H='Warehouse',],[])+;               
               [nFabTotQty :R:H='Quantity']

*B603813,1 ABD Change type to 'D' if damage or 'S' if second quality. [Begin]
*lcBrowfields = [cMarker :R:H='',]+;
               [POMAT   :R:H='Po #',Fabric :R:H='Item',] +;
               [Color   :R:H='Color',cTrancd=IIF(TRANCD ='2','R',IIF(TRANCD ='4','C','D')) :R:H='Type',]+;
               [Pattern :H='Pattern',]+;
               IIF(llDyelot , [Dyelot :R:H='Dyelot',],[])+;
               IIF(llMultiWH, [cWarecode :R:H='Location',],[])+;               
               [nFabTotQty :R:H='Quantity']
lcBrowfields = [cMarker :R:H='',]+;
               [POMAT   :R:H='Po #',Fabric :R:H='Item',] +;
               [Color   :R:H='Color',cTrancd=IIF(TRANCD ='2','R',IIF(TRANCD ='4','C',IIF(cFabGrade = '3','D','S'))) :R:H='Type',]+;
               [Pattern :H='Pattern',]+;
               IIF(llDyelot , [Dyelot :R:H='Dyelot',],[])+;
               IIF(llMultiWH, [cWarecode :R:H='Location',],[])+;               
               [nFabTotQty :R:H='Quantity']
*B603813,1 ABD [End]

IF laRecType[lnRecType,2] = 'S'
  SELECT(lcTmpPoSp)
  BROWSE FIELDS &lcBrowfields;
         LOCK 0   ;
         NOAPPEND ;
         NOEDIT   ;
         NOCLEAR  ;
         NODELETE ;
         NOMENU   ;
         NOWAIT   ;
         SAVE     ;
         WHEN lfwBrowse() AND lfwLine();
         VALID :F lfvBrows() ;
         TITLE lcDetTtl     ;
         WINDOW (lcWinCh9) IN WINDOW (gcBaseWind)
  SELECT(lnAlias)
  RETURN  
ENDIF


              
*-- B602459,1 [End]
BROWSE FIELDS &lcBrowfields;
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwBrowse() AND lfwLine();
       VALID :F lfvBrows() ;
       TITLE lcDet_Ttl     ;
       WINDOW (lcWinCh2) IN WINDOW (gcBaseWind)
       
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfwBrowse
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : the when of the main browse
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfwBrowse

glFromBrow = .T.
llNewLine  = .F.
SHOW GETS WINDOW (lcWinCh1) ENABLE ONLY
SHOW GET lcPoNum    DISABLE
SHOW GET pbPo       DISABLE
SHOW GET lcQuaFab   DISABLE
SHOW GET pbQuaFab   DISABLE
SHOW GET lcQuaClr   DISABLE
SHOW GET pbQuaClr   DISABLE
SHOW GET pbRolls    DISABLE
SHOW GET pbRemove   DISABLE
SHOW GET pbLocation DISABLE

IF !llEditExRt OR lcPCurr = gcBaseCurr
  SHOW GET lnRateP DISABLE
ENDIF
IF !llEditExRt OR lcDCurr = gcBaseCurr
  SHOW GET lnRateD DISABLE  
ENDIF
*!*************************************************************
*! Name      : lpShow
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : to control the show of all objects on the screen with all modes
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
PROCEDURE lpShow

DO CASE
  CASE laScrMode[1]
    *B601927,1 Hesham El-Sheltawi (Start)    
*    SET ORDER TO TAG TRANS IN UNCMSESS
*    lnReprocess = SET('REPROCESS')
*    IF lnSessNo = 1 .AND. SEEK('O'+PADR('MAT_POREC',10)+gcUser_id,'UNCMSESS')
*      SET REPROCESS TO 1
*      SELECT UNCMSESS
*      LOCATE REST WHILE Status+cUTranType+gcUser_id=;
*                       'O'+PADR('MAT_POREC',10)+gcUser_id FOR RLOCK() AND ;
*                       cProgram = lcDialog
*      SET REPROCESS TO lnReprocess
*      IF FOUND()
*        IF SEEK(UNCMSESS.cSession,'SyuEror')
*          *-The system detected an incomplete XXX session due to program error :  . Please make sure that you resolve the problem before proceeding.
*          IF gfModalGen('QRM00257B00012','ALERT','Purchase order receive|'+ALLTRIM(SyuEror.Er_Msg)) = 2
*          	glQuitting = .T. 
*            CLEAR READ
*            RETURN
*          ENDIF
*        ELSE
*          *-The system detected an incomplete XXX session. 
*          =gfModalGen('INM00258B00000','ALERT', 'Purchase order receive')
*        ENDIF
*        *- The system will now attempt to resume processing on the uncompleted session.
*        =gfModalGen('INM00259B00000','ALERT')
*        IF lfGetTmpFile()
*          SELECT (lcTmpPo)
*          =lfwLine()
*          llContinue  =.T.
*          laScrMode   =.F.
*          llCUpDate   =.T.
*          ldDate = &lcTmpPO..DATE 
*          laScrMode[4]=(lcScrMode='A')
*          lcSession = UNCMSESS.cSession
*          IF !EMPTY(UnCmSess.cCurrObj)
*            DO lpSavScr
*          ELSE
*            SHOW GETS
*          ENDIF
*            =lfBrLines()
*            llFrEnter = .F.
*          RETURN
*        ENDIF
*      ENDIF
*    ELSE  
    *B601927,1 call the global function that check if there is
    *B601927,1 any uncomplete sessions
    *IF gfUnCompSession('MAT_POREC')
    IF gfUnCompSession(lcUnsmsPrg,lnSessNo,lcDialog)
      *E301797,1 (Begin) Add shipment variables for uncomplete session.
      IF laRecType[lnRecType,2] = 'S'
        SELECT (lcTmpPoSp)
        =lfBrLines()
        llContinue=.T.
        laScrMode =.F.
        llCUpDate =.T.
        ldDate    = DATE 
        laScrMode[4] = (lcScrMode='A')
        lcSession = UNCMSESS.cSession
        IF !EMPTY(UnCmSess.cCurrObj)
          DO lpSavScr
        ELSE
          SHOW GETS
        ENDIF
        =lfBrLines()
        llFrEnter = .F.
        RETURN
      ENDIF
      *E301797,1 (End)
      SELECT (lcTmpPo)
      =lfBrLines()
      llContinue  =.T.
      laScrMode   =.F.
      llCUpDate   =.T.
      ldDate = &lcTmpPO..DATE 
      laScrMode[4] = (lcScrMode='A')
      lcSession = UNCMSESS.cSession
      IF !EMPTY(UnCmSess.cCurrObj)
        DO lpSavScr
      ELSE
        SHOW GETS
      ENDIF
      =lfBrLines()
      llFrEnter = .F.
      RETURN
    ENDIF
    =lfCrtUnComp()
    =lfBrLines()      
    *B601927,1 Hesham El-Sheltawi (End)      
      ldPost = gdSysDate 
      ldDate = gdSysDate
      STORE '' TO lcGlPeriod,lcGLFYear
*B601927,1 Hesham El-Sheltawi (Start)        
*      IF llTrkRolls
*        SELECT (lcTmpRolls)
*        ZAP
*      ENDIF
*      SELECT (lcTmpJour)
*      ZAP
*      SELECT (lcTmpPo)
*      DELETE ALL
*B601927,1 Hesham El-Sheltawi (End)  
      *E301797,1 (Begin) Show proper window.
      *SHOW WINDOW (lcDet_Ttl) REFRESH SAME
      IF laRecType[lnRecType,2] = 'S'
        SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
      ELSE
        SHOW GETS WINDOW (lcWinCh9) DISABLE ONLY
        SHOW WINDOW (lcDet_Ttl) REFRESH SAME
      ENDIF
      *E301797,1 (End)
      
      llEdCost = .F.
      lcPCurr  = gcBaseCurr    
      lcDCurr  = gcBaseCurr
      lnRateP  = 1
      lnRateD  = 1
      STORE '' TO lcFabric,lcColor,lcPoNum,lcDesc,lcdyelot,lcReference,lcUUse,lcUBuy,lcOldVen
      STORE 0  TO lnTotQty1,lnTotQty2,lnTotQty3,lnCost1,lnCost2,lnCost3,lnCost4,lnQtyBuy,lnQtyUse
      SHOW GET ldPost    ENABLE
      SHOW GET ldDate    ENABLE
      SHOW GET ibToBrow  ENABLE
      SHOW GET pbBrws    DISABLE
      SHOW GET pbcpPrint DISABLE
      *E301797,1 (Begin) Show proper objects. 
      * _CUROBJ = IIF(llGlLink,OBJNUM(ldPost),OBJNUM(ldDate))
      IF laRecType[lnRecType,2] = 'S'
        =lfClearInfo()
        SHOW GET pbSType   ENABLE
        SHOW GET lcShpCode ENABLE
        SHOW GET lnRecType ENABLE
        SHOW GET pbNew     DISABLE
        _CUROBJ = OBJNUM(lcShpCode)
      ELSE
        _CUROBJ = IIF(llGlLink,OBJNUM(ldPost),OBJNUM(ldDate))
      ENDIF
      *E301797,1 (End)
      
 *   ENDIF

  CASE laScrMode[2]
    SHOW GET pbSlct    DISABLE  
    SHOW GET pbcpPrint DISABLE
    SHOW GET pbEdt     DISABLE
    SHOW GET pbDlt     DISABLE    

  CASE laScrMode[3]
    SELECT(lcTmpPo)
    GOTO TOP
    IF EOF() 
      SHOW GET pbSav DISABLE
    ELSE
      SHOW GET pbSav ENABLE
    ENDIF
    *E301797,1 (Begin) Show proper window.
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    SHOW GET lcPoNum ENABLE
    *E301797,1 (End)
    
    SHOW GET pbNew     ENABLE
    SHOW GET ibToBrow  ENABLE
    SHOW GET ldDate    DISABLE
    SHOW GET ldPost    DISABLE
   
  CASE laScrMode[4]
    SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
    SHOW GET ibToBrow  ENABLE
    *E301797,1 (Begin) Show proper window.
    *SELECT(lcTmpPo)
    IF laRecType[lnRecType,2] = 'S'
      SHOW GETS WINDOW (lcWinCh9) DISABLE ONLY
      SELECT(lcTmpPoSp)
    ELSE
      SHOW GETS WINDOW (lcWinCh9) ENABLE ONLY
      SELECT(lcTmpPo)
    ENDIF
    *E301797,1 (End)

    SHOW GET ibToBrow  ENABLE
    GOTO TOP
    IF EOF() 
      SHOW GET pbSav DISABLE
    ELSE
      SHOW GET pbSav ENABLE
    ENDIF
    *E301797,1 (Begin) Show proper window.
    *SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    IF laRecType[lnRecType,2] = 'S'
      SHOW GETS WINDOW (lcWinCh8) ENABLE ONLY
      SHOW GET  lcShpCode DISABLE
    ENDIF  
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    =lfwBrowse()
    SHOW GET lcPoNum ENABLE
    *E301797,1 (End)
    
    SHOW GET pbNew   ENABLE
    SHOW GET lcPoNum ENABLE
    SHOW GET pbPo    ENABLE
    =lfBrLines()
    =lfCrUnSess('') 
    *E301797,1 (Begin) Show proper object.
    *_CUROBJ = OBJNUM(lcPoNum)
    *--301797,9
    IF llDyelot AND FABRIC.cDye_Flg = 'Y' 
       SHOW GET lcDyelot ENABLE
    ELSE  
       SHOW GET lcDyelot DISABLE
    ENDIF
    *E301797,1 (End)
ENDCASE
*E301797,1 (Begin) Initilize needed varaible.
IF laRecType[lnRecType,2] <> 'S'
  SHOW GETS WINDOW (lcWinCh8) DISABLE ONLY 
ENDIF  
*E301797,1 (End)

*!*************************************************************
*! Name      : lfAvilWare
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : get all the avialable warehouses from the warehous file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfAvilWare

SELECT cWareCode     ;  
FROM   WAREHOUS      ;                                         
INTO ARRAY laWare
SHOW GET lnWare
lcWare = laWare[1]
*!*************************************************************
*! Name      : lfvDate
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the transaction date 
*!*************************************************************
*! Calls     : CHECKPRD()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvDate

IF LASTKEY() =  13
  IF !llGlLink
    IF !CHECKPRD(ldDate,'lcGLFYear','lcGLPeriod','PO')
      _CUROBJ = OBJNUM(ldDate)
    ELSE
      laScrMode[1] = .F.  
      laScrMode[3] = .T.  
      llCUpDate    = .T.  
      *B803712,1 ABD Go to the new button. [Begin]
      _CUROBJ = OBJNUM(pbNew)
      *B803712,1 ABD [End]
      SHOW GETS
    ENDIF    
  ELSE
    IF ldDate > ldPost
      =gfModalGen('QRM36106B36000','ALERT')  
      _CUROBJ = OBJNUM(ldDate)
    ELSE
      laScrMode[1] = .F.  
      laScrMode[3] = .T.  
      llCUpDate    = .T.
      *B803712,1 ABD Go to the new button. [Begin]
      _CUROBJ = OBJNUM(pbNew)
      *B803712,1 ABD [End]
      SHOW GETS
    ENDIF  
  ENDIF  
  SELECT UNCMSESS
  REPLACE mComent WITH lfVarStr(@laVariables)
ENDIF  

*E301797,1 (Begin) Fill the types array.
*DIME laRecType[IIF(!llWareLoc,3,5),2]
IF lcKeyType  = 'R'
  _CUROBJ = OBJNUM(lcPoNum)
  laScrMode = .F.  
  laScrMode[1] = .T.  
ELSE
  _CUROBJ=OBJNUM(lnRecType)
  laRecType[1,1]='Receive by P/O'
  laRecType[1,2]='I'
  laRecType[2,1]='Receive by Shipment'
  laRecType[2,2]='S'
  lnOldTyp = lnRecType
  lnRecType   = 0  && 
  KEYBOARD '{SPACEBAR}'
  lnRecType   = lnOldTyp
ENDIF  
*E301797,1 (End)

*!*************************************************************
*! Name      : lfvNew
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : to permit the user to add new transaction line
*!             and check if the last entered line is completed
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvNew

IF !lfvBrow() AND !EMPTY(lcFabric) AND !EMPTY(lcColor) 
  RETURN
ENDIF
lcPoNum      = SPACE(06)
lcFabric     = SPACE(07)
lcColor      = SPACE(06)
lcQuaFab     = SPACE(07)
lcQuaClr     = SPACE(06)
lcReference  = SPACE(30)
lcdyelot     = SPACE(10)
lcDesc       = SPACE(30)
lnTotQty     = 0
lnType       = 1
lcPCurr      = gcBaseCurr
lcDCurr      = gcBaseCurr
lnQtyBuy     = 0
lnQtyUse     = 0
lnrateP      = 1
lnRateD      = 1
llNewLine    = .T.
laScrMode[3] = .F.
laScrMode[4] = .T.
lcScrMode    = 'A'
SELECT UNCMSESS
REPLACE mComent WITH lfVarStr(@laVariables)

*E301797,1 (Begin) Get proper file.
*SELECT(lcTmpPo)
IF laRecType[lnRecType,2] = 'S'
  SELECT(lcTmpPoSp)
ELSE
  SELECT(lcTmpPo)
ENDIF
*E301797,1 (End)

SHOW GETS
*E301797,1 (Begin) Show proper obgect.
SHOW GET lnRecType    DISABLE
_CUROBJ = OBJNUM(lcPoNum)
*E301797,1 (End)

*!*************************************************************
*! Name      : lfvPO
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the entered po number
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvPO
PRIVATE lnalias,llRetValue,lcBrTitle,lcBrFields

IF MDOWN()
  RETURN
ENDIF
lnalias = SELECT()
*-- browse all the P/O's if the enterd po is not found in the pofhdr
IF !SEEK(lcKeyType+lcPoNum,'POFHDR') AND !EMPTY(lcPoNum)
  =POFBROW (lcKeyType,'',@lcPoNum)
ENDIF
IF EMPTY(lcPoNum)
  _CUROBJ = OBJNUM(lcPoNum)
  RETURN
ENDIF
*-- check the status of the selected P/O if it is bid or cancel the 
*-- user is not permied to complete
*B803914,1 AAN Not allow the user to receive any qty. in closed PO [Start].
IF POFHDR.STATUS = 'L'
  =gfModalGen('TRM36087B00000','ALERT', IIF(lcKeyType = 'R','return|','|')+'closed')
  lcPoNum = SPACE(06)
  _CUROBJ = OBJNUM(lcPoNum)
  RETURN
ENDIF
*B803914,1 AAN Not allow the user to receive any qty. in closed PO [End].
IF POFHDR.STATUS = 'X'
  =gfModalGen('TRM36087B00000','ALERT', IIF(lcKeyType = 'R','return|','|')+'cancelled')
  lcPoNum = SPACE(06)
  _CUROBJ = OBJNUM(lcPoNum)
  RETURN
ENDIF

IF POFHDR.STATUS = 'B'
  =gfModalGen('TRM36087B00000','ALERT', IIF(lcKeyType = 'R','return|','|')+'bid')
  lcPoNum = SPACE(06)
  _CUROBJ = OBJNUM(lcPoNum)
  RETURN
ENDIF

IF  POFHDR.STATUS = 'C'

  *B803360,1 BWA 08/09/2000 Fix the bug of the wrong spelling on Issue return material screen.[START]
  *IF gfModalGen('TRM36088B36001','ALERT', IIF(lcKeyType = 'R','Retrun purchace order','Purchace order'))=2
  IF gfModalGen('TRM36088B36001','ALERT', IIF(lcKeyType = 'R','Return purchase order','Purchace order'))=2  
  *B803360,1 [END]

    lcPoNum = SPACE(06)
    _CUROBJ = OBJNUM(lcPoNum)
    RETURN
  ENDIF
ENDIF  
*-- give a warning message if the entered date of 
*-- the selected P/O is prior to the transaction 
IF  POFHDR.ENTERED > ldDate
  IF gfModalGen('TRM36089B36001','ALERT' )=2 
    lcPoNum = SPACE(06)
    _CUROBJ = OBJNUM(lcPoNum)
    RETURN
  ENDIF
ENDIF  

*-- check if the P/O currencies have available rate on the transaction date or not
IF llMulCurr
  lcPCurr = POFHDR.cPriceCur
  lcDCurr = POFHDR.cDutyCur  
  =lfRefresh()
  lnRateP= gfchkrate('lnUnitP',lcPCurr,ldDate,.F.,gcAct_Comp,.T.) 
  IF lnRateP = 0
    =gfModalGen('TRM36090B36000','ALERT','price')
    lcPCurr = gcBaseCurr
    lcDCurr = gcBaseCurr
    lnRateP = 1
    lcPoNum = SPACE(06)
    _CUROBJ = OBJNUM(lcPoNum)
    =lfRefresh()
    RETURN
  ENDIF  
  lcFrnSign1 = gfGetExSin("",lcPCurr)
  lnRateD= gfchkrate('lnUnitD',lcDCurr,ldDate,.F.,gcAct_Comp,.T.) 
  IF lnRateD = 0
    =gfModalGen('TRM36090B36000','ALERT','duty')
    lcPCurr = gcBaseCurr
    lcDCurr = gcBaseCurr
    lnRateD = 1
    lcPoNum = SPACE(06)
    _CUROBJ = OBJNUM(lcPoNum)
    =lfRefresh()
    RETURN
  ENDIF  
  lcFrnSign2 = gfGetExSin("",lcDCurr)
ENDIF

*-- get to P/O grade
lcGrade = POFHDR.cFabGrade
DO CASE
  CASE lcGrade = '1'
    laType[3,1] = 'Second Quality'
    laType[4,1] = 'Damage'

  CASE lcGrade = '2'
    DIMENSION laType[4,2]
    laType[3,1] = 'First Quality'
    laType[4,1] = 'Damage'
  
  CASE lcGrade = '3'
    DIMENSION laType[4,2]
    laType[3,1] = 'First Quality'
    laType[4,1] = 'Second Quality'
ENDCASE
*-- get the P/O warehouse
lnWare  = ASCAN(laWare,POFHDR.cwarecode)
lcWare  = laWare[lnWare] 
SHOW GETS WINDOW (lcWinCh1) ENABLE ONLY
SHOW GET lcPoNum    DISABLE
SHOW GET pbPo       DISABLE
SHOW GET lcQuaFab   DISABLE
SHOW GET pbQuaFab   DISABLE
SHOW GET lcQuaClr   DISABLE
SHOW GET pbQuaClr   DISABLE
SHOW GET pbRolls    DISABLE
SHOW GET pbRemove   DISABLE
SHOW GET pbLocation DISABLE

IF !llEditExRt OR lcPCurr = gcBaseCurr
  SHOW GET lnRateP DISABLE
ENDIF
IF !llEditExRt OR lcDCurr = gcBaseCurr
  SHOW GET lnRateD DISABLE  
ENDIF

*-- browse the po lines 
SELECT POFLN
DECLARE laBrow[13]
laBrow=' '
lcBrTitle  = 'Purchase Orders lines'
lcBrFields = "POMAT   :R :H='P/O #':8," +;
             "FABRIC  :R :H='Item' :7," +;
             "COLOR   :R :H='Color':6," +;
  	         "NFABTOTQTY  :R :H='Tot.Qty.':7"
llRetValue = gfBrows([FOR cMatType+PoMat = lcKeyType+lcPoNum AND Trancd = '1'],"Fabric,Color,nfabTotQty,nCost1,nCost2,nCost3,;
                                                                             nCost4,Reference,Pattern,nECost1,nECost2,nECost3,;
                                                                           nECost4","laBrow",lcBrTitle)

IF llRetValue
  =SEEK(laBrow[1]+laBrow[2],'Fabric')
  IF Fabric.Make .AND. gfModalGen('INM36009B36000','DIALOG')=1
    lcFabric = SPACE(07)
    lcColor  = SPACE(06)
    _CUROBJ  = OBJNUM(lcFabric)
    RETURN
  ENDIF
  IF !lfFabInWar(laBrow[1],laBrow[2],lcWare)
    RETURN
  ENDIF
  =SEEK(lcKeyType+lcPoNum+laBrow[1]+laBrow[2],'POFLN')
  SELECT POFLN
  
  *C102525,1 RAE [BEGIN] Store the PO # of the returned PO in lcFromPO.
  IF ASCAN(laEvntTrig , PADR('SAVEPO',10)) <> 0
    lcFromPo = ''
    =gfDoTriger('MAREREC',PADR('SAVEPO',10))
  ENDIF
  *C102525,1 RAE [END]

  lnPOLn  = POFLN.LINENO
  SCAN WHILE cMatType+POMAT+FABRIC+COLOR = ;
             lcKeyType+lcPoNum+laBrow[1]+laBrow[2]
     lnTotQty = IIF(TRANCD='1',lnTotQty+nFabTotQty , MAX(lnTotQty-nFabTotQty , 0))             
  ENDSCAN

  *-- if the user select a line form the po lines browse add new line in the temp recieving file
  *E301797,1 (Begin) Show proper file.
  *SELECT (lcTmpPo) 
  IF laRecType[lnRecType,2] = 'S'
    SELECT (lcTmpPoSp) 
  ELSE
   SELECT (lcTmpPo) 
  ENDIF
  *E301797,1 (End)
  APPEND BLANK
  lnLastRec = RECNO()
  *B604580,1 HBG 07/08/2001 Save Orginal Warehouse of this PO [Begin]
  *REPLACE POMAT      WITH lcPoNum   ,;
          Vendor     WITH POFHDR.Vendor ,;
          cMatType   WITH lcKeyType ,;
          Fabric     WITH laBrow[1] ,;
          Color      WITH laBrow[2] ,;
          Trancd     WITH '2'       ,;
          lineno     WITH lnPOLn    ,; 
          nFabTotQty WITH lnTotQty  ,;
          nCost1     WITH laBrow[4] ,;
          nCost2     WITH laBrow[5] ,;
          nCost3     WITH laBrow[6] ,;
          nCost4     WITH laBrow[7] ,;
          nECost1    WITH laBrow[10],;
          nECost2    WITH laBrow[11],;
          nECost3    WITH laBrow[12],;
          nECost4    WITH laBrow[13],;
          nLan_Cost1 WITH laBrow[4] ,;
          nLan_Cost2 WITH laBrow[5] ,;
          nLan_Cost3 WITH laBrow[6] ,;
          nLan_Cost4 WITH laBrow[7] ,;                              
          Pattern    WITH laBrow[9] ,;
          Reference  WITH laBrow[8] ,;
          nlanPrRat  WITH lnRateP   ,; 
          nlanDuRat  WITH lnRateD   ,;
          cWareCode  WITH lcWare    ,;
          Date       WITH ldDate    ,;
          UnitP      WITH lnUnitP   ,;
          UnitD      WITH lnUnitD   ,;          
          SignP      WITH lcFrnSign1,;
          SignD      WITH lcFrnSign2,;
          PoGrade    WITH lcGrade   ,;
          cFabGrade  WITH lcGrade   ,;
          CanQty     WITH lnTotQty

  REPLACE POMAT      WITH lcPoNum   ,;
          Vendor     WITH POFHDR.Vendor ,;
          cMatType   WITH lcKeyType ,;
          Fabric     WITH laBrow[1] ,;
          Color      WITH laBrow[2] ,;
          Trancd     WITH '2'       ,;
          lineno     WITH lnPOLn    ,; 
          nFabTotQty WITH lnTotQty  ,;
          nCost1     WITH laBrow[4] ,;
          nCost2     WITH laBrow[5] ,;
          nCost3     WITH laBrow[6] ,;
          nCost4     WITH laBrow[7] ,;
          nECost1    WITH laBrow[10],;
          nECost2    WITH laBrow[11],;
          nECost3    WITH laBrow[12],;
          nECost4    WITH laBrow[13],;
          nLan_Cost1 WITH laBrow[4] ,;
          nLan_Cost2 WITH laBrow[5] ,;
          nLan_Cost3 WITH laBrow[6] ,;
          nLan_Cost4 WITH laBrow[7] ,;                              
          Pattern    WITH laBrow[9] ,;
          Reference  WITH laBrow[8] ,;
          nlanPrRat  WITH lnRateP   ,; 
          nlanDuRat  WITH lnRateD   ,;
          cWareCode  WITH lcWare    ,;
          cOrgWare   WITH lcWare    ,;
          Date       WITH ldDate    ,;
          UnitP      WITH lnUnitP   ,;
          UnitD      WITH lnUnitD   ,;          
          SignP      WITH lcFrnSign1,;
          SignD      WITH lcFrnSign2,;
          PoGrade    WITH lcGrade   ,;
          cFabGrade  WITH lcGrade   ,;
          CanQty     WITH lnTotQty
  *B604580,1 [End]      
  
  *C102525,1 RAE [BEGIN] Replace the value of PO in temp file 
  IF ASCAN(laEvntTrig , PADR('REPPOTMP',10)) <> 0
    =gfDoTriger('MAREREC',PADR('REPPOTMP',10))
  ENDIF
  *C102525,1 RAE [END]

  llCUpdate = .T.
  lnTotQty  = 0
  lnTotQty1 = lnTotQty1 + nFabTotQty
  SELECT UNCMSESS
  *B606625,1 NAD (Start)
  *REPLACE mComent WITH lfVarStr(@laVariables)
  IF SEEK('O'+lcUnsmsPrg+gcUser_id+lcSession) 
    REPLACE mComent WITH lfVarStr(@laVariables)
  ENDIF
  *B606625,1 NAD (End)
  SHOW GET pbSav ENABLE
  =lfwLine()
  llNewLine = .F.
ENDIF
*E301797,1 (Begin) Browse fields.
*301797,9 ADD AND llRetValue
IF laRecType[lnRecType,2] = 'S' AND llRetValue
  =lfBrLines()
ENDIF  
SHOW GET lnRecType    DISABLE
*--301797,9 Make laScrMode = .F. insetead of laScrMode[3] = .F.
laScrMode = .F.
laScrMode[4] = .T.
SHOW GETS
SHOW GET lcQuaFab DISABLE
SHOW GET lcQuaClr DISABLE
SHOW GET pbQuaFab DISABLE
SHOW GET pbQuaClr DISABLE
SHOW GET pbRolls    DISABLE
SHOW GET lcPoNum    DISABLE
SHOW GET pbPo       DISABLE
*--301797,9 Remove lines that makes fabric, color disable

IF llEdCost
  SHOW GET lnCost2 ENABLE
  SHOW GET lnCost3 ENABLE
  SHOW GET lnCost4 ENABLE
ELSE
  SHOW GET lnCost2 DISABLE
  SHOW GET lnCost3 DISABLE
  SHOW GET lnCost4 DISABLE
ENDIF  
*--301797,9 Get the correct record depend on lcTmpPoSp or lcTmpPo
IF laRecType[lnRecType,2] = 'S'
  IF BETWEEN(lnNewRec,1,RECCOUNT(lcTmpPoSp))
    GO lnNewRec IN (lcTmpPoSp)
  ENDIF
ELSE
  IF BETWEEN(lnNewRec,1,RECCOUNT(lcTmpPo))
    GO lnNewRec IN (lcTmpPo)
  ENDIF
ENDIF
*E301797,1 (End)

SELECT(lnalias)

*!*************************************************************
*! Name      : lfKPO
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the entered po number from the browse button
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfKPO

lcPoNum = PADR('?',6)
=lfvPO()
*!*************************************************************
*! Name      : lfwLine
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : the when function of the main browse
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfwLine
PRIVATE lnAlias

lnAlias = SELECT()
SHOW GET pbRemove DISABLE
SHOW GET pbRolls  DISABLE
SHOW GET pbSav    DISABLE
SHOW GET pbLocation ENABLE

*E301797,1 (Begin) Show proper file.
*SELECT(lcTmpPo)
llNewLine    = .T.
laScrMode = .F.
laScrMode[4] = .T.
lcScrMode    = 'A'
SELECT UNCMSESS
REPLACE mComent WITH lfVarStr(@laVariables)
IF laRecType[lnRecType,2] = 'S'
  SELECT(lcTmpPoSp)
  REPLACE CanQty  WITH NFABTOTQTY,;
          DATE    WITH ldDate
  SHOW GET lcFabric DISABLE
  SHOW GET lcColor  DISABLE
  SHOW GET pbFabric DISABLE
  SHOW GET pbColor  DISABLE
  lcPCurr = POFHDR.cPriceCur
  lcDCurr = POFHDR.cDutyCur  
  =lfRefresh()
  lnRateP= gfchkrate('lnUnitP',lcPCurr,ldDate,.F.,gcAct_Comp,.T.) 
  lnRateD= gfchkrate('lnUnitD',lcDCurr,ldDate,.F.,gcAct_Comp,.T.)   
  REPLACE UnitP      WITH lnUnitP,;
          Unitd      WITH lnUnitd
  REPLACE uncmsess.mTmpFiles WITH 'lcTmpPOSp,'   + lcTmpPOSp   + ',' + lcTmpPOSp   + ';' +;
                                'lcTmpPO,'     + lcTmpPO   + ',' + lcTmpPO   + ';' +;
                                'lcTmpJour,'   + lcTmpJour  + ',' + lcTmpJour  + ';' +;
                                'lcGlDTemp,'   + lcGlDTemp + ',' + '' + ';' +;
                                'lcTemLoc,'    + lcTemLoc  + ',' + lcTemLoc  + ';' +;
                                'lcTmpRoll,'   + lcTmpRoll+','+lcTmpRoll
ELSE
  SELECT(lcTmpPo)
  REPLACE uncmsess.mTmpFiles WITH 'lcTmpPO,'   + lcTmpPO   + ',' + lcTmpPO   + ';' +;
                          'lcTmpJour,'  + lcTmpJour  + ',' + lcTmpJour  + ';' +;
                           'lcGlDTemp,' + lcGlDTemp + ',' + lcGlDTemp + ';' +;
                           'lcTemLoc,'  + lcTemLoc  + ',' + lcTemLoc  + ';' +;
                           'lcTmpRoll,'+lcTmpRoll+','+lcTmpRoll
  
ENDIF

*E301797,1 (End)

IF !EOF() AND !BOF() AND !DELETED()
  lnNewRec = RECNO()
  REPLACE ALL cMarker WITH lcUnMark
  GO lnNewRec
  REPLACE cMarker WITH lcMark
  lcPoNum     = POMAT
  lcFabric    = Fabric
  lcColor     = Color
  lcQuaFab    = cFabric1
  lcQuaClr    = cColor1  
  =SEEK(lcFabric+lcColor,'Fabric')
  lcDesc      = Fabric.Desc
  IF !EMPTY(lcQuaFab) AND !EMPTY(lcQuaClr)
    =SEEK(lcQuaFab+lcQuaClr,'Fabric')
  ENDIF
  lcUBuy      = Fabric.UomBuy
  lcUUse      = Fabric.UomUse
  lnQtyBuy    = nFabTotqty
  lnQtyUse    = nFabTotqty*Fabric.Conv
  lnCost1     = nLan_Cost1
  lnCost2     = nLan_Cost2
  lnCost3     = nLan_Cost3
  lnCost4     = nLan_Cost4      
  lcReference = Reference
  lcPattern   = Pattern
  lcWare      = cWareCode
  lcDyelot    = Dyelot
  lcTrancd    = Trancd
  lcFabGrade  = cFabGrade
  *E301797,1 (Begin) Get it from Header file.
  *lcGrade     = PoGrade
  lcGrade = POFHDR.cFabGrade
  DO CASE
    CASE lcGrade = '1'
      laType[3,1] = 'Second Quality'
      laType[4,1] = 'Damage'

    CASE lcGrade = '2'
      DIMENSION laType[4,2]
      laType[3,1] = 'First Quality'
      laType[4,1] = 'Damage'
  
    CASE lcGrade = '3'
      DIMENSION laType[4,2]
      laType[3,1] = 'First Quality'
      laType[4,1] = 'Second Quality'
  ENDCASE
  *E301797,1 (End)
  
  lnRateP     = nLanPrRat
  lnRateD     = nLanDuRat  
  lcDCurr     = IIF(SEEK(lcKeyType+lcPoNum,'POFHDR'),POFHDR.cDutyCur,SPACE(05)) 
  lcPCurr     = IIF(SEEK(lcKeyType+lcPoNum,'POFHDR'),POFHDR.cPriceCur,SPACE(05))   
  SHOW GET pbRemove ENABLE
  IF ((llTrkRolls AND Fabric.lTrkRolls) OR (lcMtCstMth $ 'LFI' AND lcKeyType = 'R')) AND lcTrancd <> '4'
    *B803712,1 ABD Enable rolls button if recive only. [Begin]
    *B604051,1 AAN Add cond. to IF cond. that llTrkRolls = .T. AND Fabric.lTrkRolls =.T.[Start]
    *IF lcTrancd = '2'  AND llTrkRolls AND Fabric.lTrkRolls
    IF lcTrancd = '2'
    *B604051,1 AAN Add cond. [End]
      SHOW GET pbRolls ENABLE
    ELSE
      SHOW GET pbRolls DISABLE
    ENDIF
    *B803712,1 ABD [End]
  ENDIF  
  llRollBrow = llTrkRolls AND Fabric.lTrkRolls
  IF lcTrancd = '3'
    SHOW GET lcQuaFab ENABLE
    SHOW GET lcQuaClr ENABLE
    SHOW GET pbQuaFab ENABLE
    SHOW GET pbQuaClr ENABLE
  ELSE
    SHOW GET lcQuaFab DISABLE
    SHOW GET lcQuaClr DISABLE
    SHOW GET pbQuaFab DISABLE
    SHOW GET pbQuaClr DISABLE
  ENDIF  
  SHOW GET pbSav ENABLE
ELSE
  lcPONum     = SPACE(06)  
  lcFabric    = SPACE(07)  
  lcColor     = SPACE(06)
  lcDyelot    = SPACE(10)
  lcReference = SPACE(06)
  SHOW GETS
  RETURN
ENDIF
DO CASE
  CASE lcTrancd = '2'
    lnType = 1

  CASE lcTrancd = '4'
    lnType = 2

  CASE lcTrancd = '3' 
    DO CASE
      CASE lcGrade = '1' 
        IF lcFabGrade = '2'
          lnType = 3
        ELSE
          lnType = 4
        ENDIF  
      OTHERWISE
        IF lcFabGrade = '1'
          lnType = 3
        ELSE
          lnType = 4
        ENDIF  
    ENDCASE
ENDCASE
lnWare = ASCAN(laWare,lcWare)
IF llDyelot AND FABRIC.cDye_Flg = 'Y' 
  SHOW GET lcDyelot ENABLE
ELSE  
  SHOW GET lcDyelot DISABLE
ENDIF
SHOW GET lcPoNum
SHOW GET lcFabric 
SHOW GET lcColor   
SHOW GET lnQtyBuy
SHOW GET lnQtyUse
SHOW GET lnCost1
IF llEdCost
  SHOW GET lnCost2 ENABLE
  SHOW GET lnCost3 ENABLE
  SHOW GET lnCost4 ENABLE
ELSE
  SHOW GET lnCost2 DISABLE
  SHOW GET lnCost3 DISABLE
  SHOW GET lnCost4 DISABLE
ENDIF  
SHOW GET lcReference
SHOW GET lcPattern
SHOW GET lnWare
SHOW GET lnType
IF lcPCurr = gcBaseCurr
  SHOW GET lnRateP DISABLE
ELSE
  SHOW GET lnRateP ENABLE
ENDIF  
IF lcDCurr = gcBaseCurr
  SHOW GET lnRateD DISABLE
ELSE
  SHOW GET lnRateD ENABLE
ENDIF  
*E301797,1 (Begin) Show proper obgect.
*SHOW WINDOW (lcDet_Ttl) REFRESH SAME
IF laRecType[lnRecType,2] = 'S'
  *SHOW WINDOW (lcDetTtl) REFRESH SAME
  SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
  SHOW WINDOW (lcDet_Ttl) REFRESH SAME
ELSE
  SHOW GETS WINDOW (lcWinCh9) DISABLE ONLY
  SHOW WINDOW (lcDet_Ttl) REFRESH SAME
ENDIF
IF BETWEEN(lnNewRec,1,RECCOUNT(lcTmpPoSp))
  GO lnNewRec IN (lcTmpPoSp)
ENDIF
*E301797,1 (End)

=lfRefresh()
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the fabric field
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

IF MDOWN()
  RETURN
ENDIF
lnAlias = SELECT()
SELECT Fabric
SET FILTER TO cFabGrade = lcGrade
IF !EMPTY(lcFabric) AND !SEEK(lcFabric)
    DO FaBrow WITH lcFabric,'*',.T.
ENDIF
SET FILTER TO
SHOW GET lcFabric
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvColor
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the Color field
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvColor
PRIVATE lcOldF,llPoLine,lnAlias,lnTotQty


IF MDOWN()
  RETURN
ENDIF
lnAlias = SELECT()
llPoLine = .T.
lnTotQty = 0
IF !EMPTY(lcColor) AND !SEEK(lcFabric+lcColor,'Fabric')
  SELECT FABRIC
  SET FILTER TO cFabGrade = lcGrade
  lcColor  = IIF(EMPTY(lcFabric),PADR(lcColor,6),CHR(240))
  lcOldF   = lcFabric
  DO FaBrow WITH lcFabric,lcColor,.T.
  SET FILTER TO
  IF EMPTY(lcColor)
    lcFabric = lcOldF
    _CUROBJ = OBJNUM(lcColor)
    SELECT(lnAlias)
    RETURN
  ENDIF
ENDIF
*E301797,1 (Begin) Show proper obgect.
IF laRecType[lnRecType,2] = 'S'
  SET ORDER TO TAG matshpdt IN matshpdt
  *-- permit the user to enter only purchased Fabric/Color
  IF !EMPTY(lcFabric) AND !EMPTY(lcColor) AND LASTKEY() <> 15 AND LASTKEY() <> 9
    IF Fabric.Make .AND. gfModalGen('INM36009B36000','DIALOG')=1
      lcFabric = SPACE(07)
      lcColor  = SPACE(06)
      _CUROBJ = OBJNUM(lcFabric)
      SELECT(lnAlias)
      RETURN
    ENDIF
    *-- check if the fabric/color is already on the purchase order or not
    IF !SEEK(lcFabric+lcColor+lcKeyType+lcPoNum+'1','matshpdt')
      llPoLine = .F.
      lnTotQty = 0
      lnPOLn   = pofhdr.LASTLINE + 1
      IF gfModalGen('TRM36091B36001','ALERT',IIF(lcKeyType = 'R','Retrun purchace |','Purchace |')+IIF(lcKeyType = 'R','retrun purchace','purchace'))=2
        lcFabric = SPACE(07)
        lcColor  = SPACE(06)
        _CUROBJ = OBJNUM(lcFabric)
        SELECT(lnAlias)
        RETURN
      ENDIF  
      *E301797,1 (Begin) Add mode.
      llNewLine    = .T.
     *E301797,1 (End)
     ELSE
      SELECT matshpdt
      lnPOLn  = LINENO
      lnPORec = RECNO()
      SCAN WHILE fabric+color+cmattype+pomat+trancd = ;
                 lcFabric+lcColor+lcKeyType+lcPoNum
         lnTotQty = IIF(TRANCD='1',lnTotQty+nFabTotQty , MAX(lnTotQty-nFabTotQty , 0))
      ENDSCAN
      GOTO lnPORec
    ENDIF  
    IF !lfFabInWar(lcFabric,lcColor,lcWare)
      _CUROBJ = OBJNUM(lcFabric)
      SELECT(lnAlias)
      RETURN
    ENDIF
    DO CASE
      CASE lnType = 1
        lcTrancd = '2'
      CASE lnType = 2
        lcTrancd = '4'
      CASE lnType = 3 OR lnType = 4
        lcTrancd = '3'
    ENDCASE
    SELECT(lcTmpPoSp)
    IF llNewLine
      =SEEK(lcFabric+lcColor,'Fabric')
      APPEND BLANK
      lnLastRec = RECNO()
    ENDIF  
    REPLACE POMAT      WITH lcPoNum   ,;
            Vendor     WITH POFHDR.Vendor ,;
            cMatType   WITH lcKeyType ,;
            Fabric     WITH lcFabric  ,;
            Color      WITH lcColor   ,;
            Trancd     WITH lcTrancd  ,;
            nFabTotQty WITH lnTotQty  ,;
            lineno     WITH lnPOLn    ,; 
            nCost1     WITH IIF(llPoLine,matshpdt.nCost1,Fabric.nFabCost)  ,;
            nCost2     WITH IIF(llPoLine,matshpdt.nCost2,Fabric.nItm_Frt)  ,;
            nCost3     WITH IIF(llPoLine,matshpdt.nCost3,Fabric.nItem_Tax) ,;
            nCost4     WITH IIF(llPoLine,matshpdt.nCost4,Fabric.nItemQuota),;
            nECost1    WITH IIF(llPoLine,matshpdt.nECost1,Fabric.nFabCost)  ,;
            nECost2    WITH IIF(llPoLine,matshpdt.nECost2,Fabric.nItm_Frt)  ,;
            nECost3    WITH IIF(llPoLine,matshpdt.nECost3,Fabric.nItem_Tax) ,;
            nECost4    WITH IIF(llPoLine,matshpdt.nECost4,Fabric.nItemQuota),;
            nLan_Cost1 WITH IIF(llPoLine,matshpdt.nCost1,Fabric.nFabCost)  ,;
            nLan_Cost2 WITH IIF(llPoLine,matshpdt.nCost2,Fabric.nItm_Frt)  ,;
            nLan_Cost3 WITH IIF(llPoLine,matshpdt.nCost3,Fabric.nItem_Tax) ,;
            nLan_Cost4 WITH IIF(llPoLine,matshpdt.nCost4,Fabric.nItemQuota),;                              
            Pattern    WITH IIF(llPoLine,matshpdt.Pattern,Fabric.Pattern)  ,;
            Reference  WITH IIF(llPoLine,matshpdt.Reference ,SPACE(30))    ,;
            nlanPrRat  WITH lnRateP   ,; 
            nlanDuRat  WITH lnRateD   ,;
            cWareCode  WITH lcWare    ,;
            Date       WITH ldDate    ,;
            UnitP      WITH lnUnitP   ,;
            UnitD      WITH lnUnitD   ,;          
            SignP      WITH lcFrnSign1,;
            SignD      WITH lcFrnSign2,;
            PoGrade    WITH lcGrade   ,;
            cFabGrade  WITH lcGrade   ,;
            CanQty     WITH lnTotQty
    llCUpdate = .T.        
    lnTotQty1 = IIF(lcTrancd='2',lnTotQty1+IIF(llNewLine,nFabTotQty,IIF(!SEEK(lcFabric+lcColor+lcKeyType+lcPoNum+'1','matshpdt'),-lnQtyBuy,nFabTotQty-lnQtyBuy)),lnTotQty1)
    lnTotQty2 = IIF(lcTrancd='4',lnTotQty2+IIF(llNewLine,nFabTotQty,IIF(!SEEK(lcFabric+lcColor+lcKeyType+lcPoNum+'1','matshpdt'),-lnQtyBuy,nFabTotQty-lnQtyBuy)),lnTotQty2)
    lnTotQty3 = IIF(lcTrancd='3',lnTotQty3+IIF(llNewLine,nFabTotQty,IIF(!SEEK(lcFabric+lcColor+lcKeyType+lcPoNum+'1','matshpdt'),-lnQtyBuy,nFabTotQty-lnQtyBuy)),lnTotQty3)    
    llNewLine = .F.
    lnTotQty  = 0
    SELECT UNCMSESS
    REPLACE mComent WITH lfVarStr(@laVariables)
    SHOW GET pbSav ENABLE
    =lfwLine()
    *--E301797,9 
    =lfBrLines()
  ELSE
    IF LASTKEY() <> 15 AND LASTKEY() <> 9
      =gfModalGen('INM36013B36000','DIALOG')
      _CUROBJ = IIF(EMPTY(lcFabric),OBJNUM(lcFabric),OBJNUM(lcColor))    
    ENDIF  
    SELECT(lnAlias)
    RETURN
  ENDIF
  SELECT(lnAlias)
  RETURN
ENDIF  
*E301797,1 (End)
*-- permit the user to enter only purchased Fabric/Color
IF !EMPTY(lcFabric) AND !EMPTY(lcColor) AND LASTKEY() <> 15 AND LASTKEY() <> 9
  IF Fabric.Make .AND. gfModalGen('INM36009B36000','DIALOG')=1
    lcFabric = SPACE(07)
    lcColor  = SPACE(06)
    _CUROBJ = OBJNUM(lcFabric)
    SELECT(lnAlias)
    RETURN
  ENDIF

  *-- check if the fabric/color is already on the purchase order or not
  IF !SEEK(lcKeyType+lcPoNum+lcFabric+lcColor+'1','POFLN')
    llPoLine = .F.
    lnTotQty = 0
    lnPOLn   = POFHDR.LASTLINE + 1
    IF gfModalGen('TRM36091B36001','ALERT',IIF(lcKeyType = 'R','Retrun purchace |','Purchace |')+IIF(lcKeyType = 'R','retrun purchace','purchace'))=2
      lcFabric = SPACE(07)
      lcColor  = SPACE(06)
      _CUROBJ = OBJNUM(lcFabric)
      SELECT(lnAlias)
      RETURN
    ENDIF  
    *E301797,1 (Begin) Add mode.
    llNewLine    = .T.
    *E301797,1 (End)
  ELSE
    SELECT POFLN
    lnPOLn  = POFLN.LINENO
    lnPORec = RECNO()
    SCAN WHILE cMatType+POMAT+FABRIC+COLOR = ;
               lcKeyType+lcPoNum+lcFabric+lcColor
       lnTotQty = IIF(TRANCD='1',lnTotQty+nFabTotQty , MAX(lnTotQty-nFabTotQty , 0))             
    ENDSCAN
    GOTO lnPORec
  ENDIF  
  IF !lfFabInWar(lcFabric,lcColor,lcWare)
    _CUROBJ = OBJNUM(lcFabric)
    SELECT(lnAlias)
    RETURN
  ENDIF
  DO CASE
    CASE lnType = 1
      lcTrancd = '2'
    CASE lnType = 2
      lcTrancd = '4'
    CASE lnType = 3 OR lnType = 4
      lcTrancd = '3'
  ENDCASE
  SELECT(lcTmpPo)
  IF llNewLine
    =SEEK(lcFabric+lcColor,'Fabric')
    APPEND BLANK
    lnLastRec = RECNO()
  ENDIF  
  REPLACE POMAT      WITH lcPoNum   ,;
          Vendor     WITH POFHDR.Vendor ,;
          cMatType   WITH lcKeyType ,;
          Fabric     WITH lcFabric  ,;
          Color      WITH lcColor   ,;
          Trancd     WITH lcTrancd  ,;
          nFabTotQty WITH lnTotQty  ,;
          lineno     WITH lnPOLn    ,; 
          nCost1     WITH IIF(llPoLine,POFLN.nCost1,Fabric.nFabCost)  ,;
          nCost2     WITH IIF(llPoLine,POFLN.nCost2,Fabric.nItm_Frt)  ,;
          nCost3     WITH IIF(llPoLine,POFLN.nCost3,Fabric.nItem_Tax) ,;
          nCost4     WITH IIF(llPoLine,POFLN.nCost4,Fabric.nItemQuota),;
          nECost1    WITH IIF(llPoLine,POFLN.nECost1,Fabric.nFabCost)  ,;
          nECost2    WITH IIF(llPoLine,POFLN.nECost2,Fabric.nItm_Frt)  ,;
          nECost3    WITH IIF(llPoLine,POFLN.nECost3,Fabric.nItem_Tax) ,;
          nECost4    WITH IIF(llPoLine,POFLN.nECost4,Fabric.nItemQuota),;
          nLan_Cost1 WITH IIF(llPoLine,POFLN.nCost1,Fabric.nFabCost)  ,;
          nLan_Cost2 WITH IIF(llPoLine,POFLN.nCost2,Fabric.nItm_Frt)  ,;
          nLan_Cost3 WITH IIF(llPoLine,POFLN.nCost3,Fabric.nItem_Tax) ,;
          nLan_Cost4 WITH IIF(llPoLine,POFLN.nCost4,Fabric.nItemQuota),;                              
          Pattern    WITH IIF(llPoLine,POFLN.Pattern,Fabric.Pattern)  ,;
          Reference  WITH IIF(llPoLine,POFLN.Reference ,SPACE(30))    ,;
          nlanPrRat  WITH lnRateP   ,; 
          nlanDuRat  WITH lnRateD   ,;
          cWareCode  WITH lcWare    ,;
          Date       WITH ldDate    ,;
          UnitP      WITH lnUnitP   ,;
          UnitD      WITH lnUnitD   ,;          
          SignP      WITH lcFrnSign1,;
          SignD      WITH lcFrnSign2,;
          PoGrade    WITH lcGrade   ,;
          cFabGrade  WITH lcGrade   ,;
          CanQty     WITH lnTotQty
  llCUpdate = .T.        
  lnTotQty1 = IIF(lcTrancd='2',lnTotQty1+IIF(llNewLine,nFabTotQty,IIF(!SEEK(lcKeyType+lcPoNum+lcFabric+lcColor+'1','POFLN'),-lnQtyBuy,nFabTotQty-lnQtyBuy)),lnTotQty1)
  lnTotQty2 = IIF(lcTrancd='4',lnTotQty2+IIF(llNewLine,nFabTotQty,IIF(!SEEK(lcKeyType+lcPoNum+lcFabric+lcColor+'1','POFLN'),-lnQtyBuy,nFabTotQty-lnQtyBuy)),lnTotQty2)
  lnTotQty3 = IIF(lcTrancd='3',lnTotQty3+IIF(llNewLine,nFabTotQty,IIF(!SEEK(lcKeyType+lcPoNum+lcFabric+lcColor+'1','POFLN'),-lnQtyBuy,nFabTotQty-lnQtyBuy)),lnTotQty3)    
  llNewLine = .F.
  lnTotQty  = 0
  SELECT UNCMSESS
  REPLACE mComent WITH lfVarStr(@laVariables)
  SHOW GET pbSav ENABLE
  =lfwLine()
ELSE
  IF LASTKEY() <> 15 AND LASTKEY() <> 9
    =gfModalGen('INM36013B36000','DIALOG')
    _CUROBJ = IIF(EMPTY(lcFabric),OBJNUM(lcFabric),OBJNUM(lcColor))    
  ENDIF  
  SELECT(lnAlias)
  RETURN
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvType
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the transaction type field
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvType
PRIVATE lnAlias

*E301797,1 (Begin) Handle Shipment.
IF laRecType[lnRecType,2] = 'S'
  =lfvMType()
  RETURN
ELSE
  SELECT (lcTmpPo)  
ENDIF
*E301797,1 (End)

IF lnType <> lnOldType 
  DO CASE
    CASE lnType = 1
 *RRR
      REPLACE &lcTmpPo..TRANCD WITH '2'
      lnTotQty1 = lnTotQty1 + &lcTmpPo..nFabTotQty
      SHOW GET lcQuaFab DISABLE
      SHOW GET pbQuaFab DISABLE
      SHOW GET lcQuaClr DISABLE
      SHOW GET pbQuaClr DISABLE
      DO CASE
        CASE lnOldType = 2
          lnTotQty2 = lnTotQty2 - &lcTmpPo..nFabTotQty
        CASE lnOldType = 3 OR lnOldType = 4
          lnTotQty3 = lnTotQty3 - &lcTmpPo..nFabTotQty
      ENDCASE
      lcQuaFab = SPACE(07)
      lcQuacLR = SPACE(06)      
      
    CASE lnType = 2
      IF &lcTmpPo..CanQty = 0
        =gfModalGen('INM36105B00000','ALERT')
        lnType = lnOldType 
        SHOW GET lnType
        RETURN
      ENDIF
      IF lnQtyBuy > &lcTmpPo..CanQty
        =gfModalGen('INM36095B00000','ALERT',ALLTRIM(STR(&lcTmpPo..CanQty)))
        lnType = lnOldType 
        SHOW GET lnType
        RETURN
      ENDIF
      REPLACE &lcTmpPo..TRANCD WITH '4'
      lnTotQty2 = lnTotQty2 + &lcTmpPo..nFabTotQty
      SHOW GET lcQuaFab DISABLE
      SHOW GET pbQuaFab DISABLE
      SHOW GET lcQuaClr DISABLE
      SHOW GET pbQuaClr DISABLE
      DO CASE
        CASE lnOldType = 1
          lnTotQty1 = lnTotQty1 - &lcTmpPo..nFabTotQty
        CASE lnOldType = 3 OR lnOldType = 4
          lnTotQty3 = lnTotQty3 - &lcTmpPo..nFabTotQty
      ENDCASE   
      lcQuaFab = SPACE(07)
      lcQuacLR = SPACE(06)      
      
    CASE lnType = 3 OR lnType = 4
      REPLACE &lcTmpPo..TRANCD WITH '3'
      lnTotQty3 = lnTotQty3 + IIF(lnOldType<>3 AND lnOldType<>4,&lcTmpPo..nFabTotQty,0)
      SHOW GET lcQuaFab ENABLE
      SHOW GET pbQuaFab ENABLE
      SHOW GET lcQuaClr ENABLE
      SHOW GET pbQuaClr ENABLE
      SHOW GET lcDyelot DISABLE
      =SEEK(lcFabric+lcColor,'FABRIC')
      DO CASE
        CASE lcGrade = '1'
          IF lnType = 3
            lcFabGrade = '2'
            lcQuaFab = Fabric.cFabric1
            lcQuaClr = IIF(Fabric.cColor1<>'******',Fabric.cColor1,SPACE(06))
          ELSE
            lcFabGrade = '3'
            lcQuaFab = Fabric.cFabric2
            lcQuaClr = IIF(Fabric.cColor2<>'******',Fabric.cColor2,SPACE(06))
          ENDIF
          
        CASE lcGrade = '2'
          IF lnType = 3
            lcFabGrade = '1'         
            lcQuaFab = Fabric.cFabric1
            lcQuaClr = IIF(Fabric.cColor1<>'******',Fabric.cColor1,SPACE(06))
          ELSE
            lcFabGrade = '3'
            lcQuaFab = Fabric.cFabric2
            lcQuaClr = IIF(Fabric.cColor2<>'******',Fabric.cColor2,SPACE(06))
          ENDIF
          
        CASE lcGrade = '3'
          IF lnType = 3
            lcFabGrade = '1'         
            lcQuaFab = Fabric.cFabric1
            lcQuaClr = IIF(Fabric.cColor1<>'******',Fabric.cColor1,SPACE(06))
          ELSE
            lcFabGrade = '2'
            lcQuaFab = Fabric.cFabric2
            lcQuaClr = IIF(Fabric.cColor2<>'******',Fabric.cColor2,SPACE(06))
          ENDIF
      ENDCASE 
      REPLACE &lcTmpPo..cFabGrade WITH lcFabGrade
      DO CASE 
        CASE lnOldType = 1
          lnTotQty1 = lnTotQty1 - &lcTmpPo..nFabTotQty
        CASE lnOldType = 2
          lnTotQty2 = lnTotQty2 - &lcTmpPo..nFabTotQty
      ENDCASE    
  ENDCASE
ENDIF
lnAlias = SELECT()

*B603188,1 HDM [Start] Don't delete any records from lcTmpRolls as
*                      it's not used now
*IF lnOldType = 1 AND llTrkRolls
*  IF SEEK(lcFabric+lcColor+lcWare+lcDyelot,lcTmpRolls)
*    SELECT(lcTmpRolls)
*    DELETE ALL FOR cRollItem+Color+cWarecode+Dyelot = lcFabric+lcColor+lcWare+lcDyelot
*  ENDIF
*ENDIF
*B603188,1 HDM [End]

SELECT UNCMSESS
REPLACE mComent WITH lfVarStr(@laVariables)
SELECT(lnAlias)
REPLACE &lcTmpPo..cFabric1 WITH lcQuaFab
REPLACE &lcTmpPo..cColor1  WITH lcQuaClr
=lfwLine()

*!*************************************************************
*! Name      : lfvWare
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the warehouse field
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvWare
PRIVATE lnAlias,lcSeekKey

lnAlias = SELECT()
lcSeekKey = SPACE(01)
lcWare = laWare[lnWare] 
IF !EMPTY(lcFabric) AND !EMPTY(lcColor)
  IF !lfFabInWar(lcFabric,lcColor,lcWare)
    _CUROBJ = OBJNUM(lcFabric)
    =lfwLine()
    RETURN
  ENDIF
  IF !EMPTY(lcQuaFab) AND !EMPTY(lcQuaClr) AND !SEEK(lcQuaFab+lcQuaClr+lcWare,'FABDYE')
    DO gpAdFabWar WITH lcQuaFab,lcQuaClr,SPACE(10),lcWare
  ENDIF
  *E301797,1 (Begin) Get proper file.
  *IF EOF(lcTmpPo)
  IF IIF(laRecType[lnRecType,2] = 'S',EOF(lcTmpPoSp),EOF(lcTmpPo))
  *E301797,1 (End)
    _CUROBJ = OBJNUM(lcFabric)
    RETURN    
  ENDIF
  *E301797,1 (Begin) Get proper file.
  *REPLACE &lcTmpPo..cWareCode WITH lcWare
  IF laRecType[lnRecType,2] = 'S'
    REPLACE &lcTmpPoSp..cWareCode WITH lcWare
  ELSE
    REPLACE &lcTmpPo..cWareCode WITH lcWare
  ENDIF
  *E301797,1 (End)

  lcSeekKey = IIF(!EMPTY(lcQuaFab) AND !EMPTY(lcQuaClr),lcQuaFab+lcQuaClr,lcFabric+lcColor)
  *B603188,1 [Start] gfMatCrl will now handle rolls
  *IF llTrkRolls AND SEEK(lcSeekKey+lcOldWare+lcDyelot,lcTmpRolls)
  *  IF lcKeyType = 'P' 
  *    SELECT(lcTmpRolls)
  *    REPLACE cWarecode WITH lcWare ALL FOR cRollItem+Color+cWarecode+Dyelot = ;
  *                                          lcSeekKey+lcOldWare+lcDyelot
  *  ELSE
  *    SELECT(lcTmpRolls)
  *    	DELETE ALL FOR cRollItem+Color+cWareCode+Dyelot = ;
  *    	               lcSeekKey+lcOldWare+lcDyelot
  *    =gfModalGen('TRM36092B36000','ALERT')
  *  ENDIF
  *ENDIF
  *B603188,1 [End]

ENDIF  
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfFabInWar
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : check if the fabric/color is assigned to the warehouse
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfFabInWar
PARAMETER lcfFabric,lcfColor,lcfWare

IF !SEEK(lcfFabric+lcfColor+lcfWare,'FabDye')
*--HDM B602331,1 We Need This Check For Both return and PO[start]
*  IF lcKeyType = 'R'
*    lnChoice=gfModalGen('TRM36049B36000','ALERT',ALLTRIM(lcfFabric)+'/'+ALLTRIM(lcfColor)+'|'+ALLTRIM(lcfWare))
*    RETURN .F.                  
*  ELSE
  lnChoice=gfModalGen('TRM36049B36001','ALERT',ALLTRIM(lcfFabric)+'/'+ALLTRIM(lcfColor)+'|'+ALLTRIM(lcfWare)) 
*  ENDIF
*--HDM B602331,1 We Need This Check For Both return and PO[end]
  IF lnChoice = 1
    DO gpAdFabWar WITH lcfFabric, lcfColor,SPACE(10), lcfWare
  ELSE
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.
*!*************************************************************
*! Name      : lfwType
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : take the old type into a variable
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfwType

lnOldType = lnType
*!*************************************************************
*! Name      : lfvBrow
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the main browse
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvBrow
PARAMETER llFromBrow
PRIVATE lnRecno,lnAlias,llFromBrow

lnAlias = SELECT()
lnRecno = RECNO()
llNewLine  = .F.
SHOW GETS WINDOW (lcWinCh1) ENABLE ONLY
SHOW GET lcPoNum    DISABLE
SHOW GET pbPo       DISABLE
SHOW GET lcQuaFab   DISABLE
SHOW GET pbQuaFab   DISABLE
SHOW GET lcQuaClr   DISABLE
SHOW GET pbQuaClr   DISABLE
SHOW GET pbRolls    DISABLE
SHOW GET pbRemove   DISABLE
SHOW GET pbLocation DISABLE

IF !llEditExRt OR lcPCurr = gcBaseCurr
  SHOW GET lnRateP DISABLE
ENDIF
IF !llEditExRt OR lcDCurr = gcBaseCurr
  SHOW GET lnRateD DISABLE  
ENDIF
=lfwLine()
IF SEEK(lcKeyType+&lcTmpPo..POMat+&lcTmpPo..Fabric+&lcTmpPo..Color+&lcTmpPo..cWareCode+&lcTmpPo..Trancd+&lcTmpPo..Dyelot+&lcTmpPo..cFabGrade,lcTmpPo) AND RECNO(lcTmpPo) <> lnRecno
  =gfDialog ("I","This Item/Color/PO/Type is found in the same session .Cannot add.")  
  GOTO lnRecno 
  lcRecTran = &lcTmpPo..Trancd
  lnRecQty  = &lcTmpPo..nFabTotQty
  DELETE
  DO CASE
    CASE lcRecTran = '2'
    lnTotQty1 = lnTotQty1 - lnRecQty

    CASE lcRecTran = '3' 
    lnTotQty2 = lnTotQty2 - lnRecQty

    CASE lcRecTran = '4'
    lnTotQty3 = lnTotQty3 - lnRecQty
  ENDCASE
ENDIF
lnAlias = SELECT()
SELECT UNCMSESS
REPLACE mComent WITH lfVarStr(@laVariables)
SELECT(lnAlias)

IF !EMPTY(lcFabric) AND !EMPTY(lcColor) AND WONTOP() # (lcDet_Ttl)
  IF lcTrancd = '3' AND (EMPTY(lcQuaFab) OR EMPTY(lcQuacLR))
    =gfModalGen('TRM36094B36000','ALERT','Item/Color') 
    GOTO lnNewRec
    ACTIVATE WINDOW (lcWinCh1)
    =lfwLine()
    _CUROBJ = OBJNUM(lcQuaFab)
     RETURN .F.
  ENDIF
  IF llDyelot AND FABRIC.cDye_Flg = 'Y' AND EMPTY(lcDyelot)
    =gfModalGen('TRM36094B36000','ALERT','dyelot') 
    GOTO lnNewRec
    ACTIVATE WINDOW (lcWinCh1)
    =lfwLine()
    _CUROBJ = OBJNUM(lcDyelot)
     RETURN .F.
  ENDIF
ENDIF  
*=lfwLine()

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : to remove a transaction line
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvRemove
PRIVATE lnAlias

lnAlias = SELECT()
IF gfModalGen('INM36017B36001','DIALOG') = 1
  *E301797,1 (Begin) Handle qty porperly.
  *lcRecTran = &lcTmpPo..Trancd
  *lnRecQty  = &lcTmpPo..nFabTotQty
  IF laRecType[lnRecType,2] = 'S'
    lcRecTran = &lcTmpPoSp..Trancd
    lnRecQty  = &lcTmpPoSp..nFabTotQty
  ELSE
    lcRecTran = &lcTmpPo..Trancd
    lnRecQty  = &lcTmpPo..nFabTotQty
  ENDIF
  *E301797,1 (End)

  DELETE
  DO CASE
    CASE lcRecTran = '2'
    lnTotQty1 = lnTotQty1 - lnRecQty
    laVariables[2] = 'lnTotQty1'

    CASE lcRecTran = '4'
    lnTotQty2 = lnTotQty2 - lnRecQty
    laVariables[4] = 'lnTotQty2'

    CASE lcRecTran = '3'
    lnTotQty3 = lnTotQty3 - lnRecQty
    laVariables[3] = 'lnTotQty3'
  ENDCASE
ENDIF
SELECT UNCMSESS
REPLACE mComent WITH lfVarStr(@laVariables)
SELECT(lnAlias)
*E301797,1 (Begin) Show proper window.
*SHOW WINDOW (lcDet_Ttl) REFRESH SAME
IF laRecType[lnRecType,2] = 'S'
  SELECT (lcTmpPoSp)  
  =lfBrLines()
  *SHOW WINDOW (lcDet_Ttl) REFRESH SAME
  SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY  
  SELECT (lcTmpPoSp)  
ELSE
  SHOW GETS WINDOW (lcWinCh9) DISABLE ONLY
  SHOW WINDOW (lcDet_Ttl) REFRESH SAME
ENDIF
*E301797,1 (End)

GOTO TOP
=lfwLine()

*!*************************************************************
*! Name      : lfwQty
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : take the old quantity in a variable
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfwQty

lnOldQty = lnQtyBuy
*!*************************************************************
*! Name      : lfvQty
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the quantity field
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvQty
PRIVATE lnAlias,lcKey

IF lnQtyBuy <= 0
  =gfModalGen('INM36014B36000','DIALOG','Quantity')  
  lnQtyBuy = lnOldQty
  _CUROBJ  = OBJNUM(lnQtyBuy)
  RETURN .F.
ENDIF
*-- check if the tran. type is cancel the quantity cannot exceed the remainder quantity
*E301797,1 (Begin) Handle shipment also.
*IF lnQtyBuy > &lcTmpPo..CanQty AND lnType = 2
IF lnQtyBuy > IIF(laRecType[lnRecType,2] = 'S',&lcTmpPoSp..CanQty,&lcTmpPo..CanQty) AND lnType = 2
*E301797,1 (End)
  =gfModalGen('INM36095B00000','ALERT',ALLTRIM(STR(&lcTmpPo..CanQty)))
  lnQtyBuy = lnOldQty
  RETURN
ENDIF
lnAlias = SELECT()
*E301797,1 (Begin) Handle shipment also.
*REPLACE &lcTmpPo..nFabTotQty WITH lnQtyBuy
IF laRecType[lnRecType,2] = 'S'
  REPLACE &lcTmpPoSp..nFabTotQty WITH lnQtyBuy
ELSE
  REPLACE &lcTmpPo..nFabTotQty WITH lnQtyBuy
ENDIF
*E301797,1 (End)

lcKey = IIF(EMPTY(lcQuaFab),lcFabric+lcColor,lcQuaFab+lcQuaClr)
=SEEK(lcKey,'Fabric')
lnQtyUse = lnQtyBuy*Fabric.Conv
*-- check if the quantity changed and the user has already entered rolls
*-- give a warning message

*B603188,1 HDM [Start] Don't check Rolls Entry here
*IF lnQtyBuy <> lnOldQty AND llTrkRolls AND Fabric.lTrkRolls AND ;
*  SEEK(lcKey+lcWare+lcDyelot,lcTmpRolls) AND &lcTmpPo..Trancd <> '4'
* =gfModalGen('INM36092B00000','ALERT','rolls')
*ENDIF
*B603188,1 HDM [End]

IF lnQtyBuy <> lnOldQty AND (!llTrkRolls OR !Fabric.lTrkRolls) AND lcMtCstMth $ 'LFI' AND;
  SEEK(lcKey+lcWare+lcDyelot,lcTmpJour)
  =gfModalGen('INM36092B00000','ALERT','lots')
ENDIF

*E301797,1 (Begin) Handle shipment also.
IF laRecType[lnRecType,2] = 'S'
  DO CASE
    CASE &lcTmpPoSp..Trancd = '2'
      lnTotQty1 = lnTotQty1 - lnOldQty + lnQtyBuy
      laVariables[2] = 'lnTotQty1'
    CASE &lcTmpPoSp..Trancd = '4' 
      lnTotQty2 = lnTotQty2 - lnOldQty + lnQtyBuy
      laVariables[3] = 'lnTotQty2'
    CASE &lcTmpPoSp..Trancd = '3' 
      lnTotQty3 = lnTotQty3 - lnOldQty + lnQtyBuy
      laVariables[4] = 'lnTotQty3'
  ENDCASE  
  SELECT UNCMSESS
  REPLACE mComent WITH lfVarStr(@laVariables)
  SELECT(lnAlias)
  =lfRefresh()
  RETURN
ENDIF
*E301797,1 (End)

DO CASE
  CASE &lcTmpPo..Trancd = '2'
    lnTotQty1 = lnTotQty1 - lnOldQty + lnQtyBuy
    laVariables[2] = 'lnTotQty1'
  CASE &lcTmpPo..Trancd = '4' 
    lnTotQty2 = lnTotQty2 - lnOldQty + lnQtyBuy
    laVariables[3] = 'lnTotQty2'
  CASE &lcTmpPo..Trancd = '3' 
    lnTotQty3 = lnTotQty3 - lnOldQty + lnQtyBuy
    laVariables[4] = 'lnTotQty3'
ENDCASE  
SELECT UNCMSESS
REPLACE mComent WITH lfVarStr(@laVariables)
SELECT(lnAlias)
=lfRefresh()

*!*************************************************************
*! Name      : lfvCost
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the price field
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvCost
PARAMETER lnCostNum

lcCost = STR(lnCostNum,1)
IF lnCost&lcCost < 0
  DO CASE
    CASE lcCost = '1'
      lcDialog = 'Price'
    CASE lcCost = '2'
      lcDialog = 'Freight'
    CASE lcCost = '3'
      lcDialog = 'Tax'
    CASE lcCost = '4'
      lcDialog = 'Quota'
  ENDCASE
  =gfModalGen('INM36014B36000','DIALOG',lcDialog)
  lnCost&lcCost = 0
  SHOW GET lnCost&lcCost
  RETURN
ENDIF
*E301797,1 (Begin) Handle shipment also.
*REPLACE &lcTmpPo..nLan_Cost&lcCost WITH lnCost&lcCost
IF laRecType[lnRecType,2] = 'S'
  REPLACE &lcTmpPoSp..nLan_Cost&lcCost WITH lnCost&lcCost
ELSE
  REPLACE &lcTmpPo..nLan_Cost&lcCost WITH lnCost&lcCost
ENDIF
*E301797,1 (End)

*!*************************************************************
*! Name      : lfActPad
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : function to activate a new pad for the extra options
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfActPad

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Edit cost per line' SKIP FOR (!laScrMode[3])

*C200472,1 KHM 03/10/2003 (Begin) Add new custom option (Rolls Entry)
IF ASCAN(laEvntTrig , PADR('ADMABROP',10)) <> 0
  =gfDoTriger('MAPOREC',PADR('ADMABROP',10))
ENDIF
*C200472,1 KHM 03/10/2003 (End)

ON SELECTION POPUP _OPTIONPOP DO lpOptions WITH BAR()
ON KEY LABEL ALT+P ACTIVATE POPUP _OPTIONPOP
*!*************************************************************
*! Name      : lpOptions
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : done when one of the option bar if activated
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
PROCEDURE lpOptions
PARAMETERS lnBar

DO CASE
  CASE lnBar = 1
    llEdCost = !llEdCost
    SET MARK OF BAR 1 OF _OPTIONPOP TO llEdCost
ENDCASE    
*!*************************************************************
*! Name      : lfvDyelot 
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the dyelot field
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvDyelot 
PRIVATE lnAlias,llBrowDye,lcKey

lnAlias = SELECT()
lcKey   = IIF(!EMPTY(lcQuaFab) AND !EMPTY(lcQuaClr),lcQuaFab+lcQuaClr,lcFabric+lcColor)
*-- variable to check for the condition to browse the existing dyelots
llBrowDye = IIF(lcKeyType='P',EMPTY(lcDyelot),!SEEK(lcKey+lcWare+lcDyelot,'FabDye') OR EMPTY(lcDyelot))
IF llBrowDye
  IF llMultiWH
    DO gpFbDyBrow WITH IIF(!EMPTY(lcQuaFab) AND !EMPTY(lcQuaClr),lcQuaFab,lcFabric),;
                       IIF(!EMPTY(lcQuaFab) AND !EMPTY(lcQuaClr),lcQuaClr,lcColor) ,;
                       lcDyelot, lcWare
  ELSE
    DO FDYEBROW WITH IIF(!EMPTY(lcQuaFab) AND !EMPTY(lcQuaClr),lcQuaFab,lcFabric),;
                     IIF(!EMPTY(lcQuaFab) AND !EMPTY(lcQuaClr),lcQuaClr,lcColor) ,;
                     lcDYELOT
  ENDIF
ENDIF
IF EMPTY(lcDyelot) 
  =gfModalGen('TRM36094B36000','ALERT','dyelot') 
  _CUROBJ = IIF(lcKeyType = 'P' OR !llMultiWH,OBJNUM(lcDyelot),OBJNUM(lnWare))
ELSE
  *B603188,1 [Start] gfMatCrl Will handle rolls
  *IF llTrkRolls AND SEEK(lcKey+lcWare+lcOldDye,lcTmpRolls)
  *  SELECT(lcTmpRolls)
  *  REPLACE Dyelot WITH lcDyelot ALL FOR cRollItem+Color+cWarecode+Dyelot = ;
                                          lcKey+lcWare+lcOldDye
  *ENDIF
  *B603188,1 [End]
  
  SELECT(lnAlias)
 
  *E301797,1 (Begin) Get proper file.
  *REPLACE &lcTmpPo..Dyelot WITH lcDyelot 
  IF laRecType[lnRecType,2] = 'S'
    REPLACE &lcTmpPoSp..Dyelot WITH lcDyelot 
  ELSE
    REPLACE &lcTmpPo..Dyelot WITH lcDyelot 
  ENDIF
  
  *SHOW WINDOW (lcDet_Ttl) REFRESH SAME
  IF laRecType[lnRecType,2] = 'S'
    SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
  ELSE
    SHOW GETS WINDOW (lcWinCh9) DISABLE ONLY
    SHOW WINDOW (lcDet_Ttl) REFRESH SAME
  ENDIF
  *E301797,1 (End)

ENDIF  

if .f.
IF !SEEK (lcFabric+PADR(lcColor,6)+lcFromWare+lcDyelot)
  llReEnter = gfModalGen('QRM36049B36001','ALERT',IIF(llDyeLvl,'/Dyelot','')+':'+ALLTRIM(lcFabric)+'/'+ALLTRIM(lcColor)+IIF(llDyeLvl,'/'+ ALLTRIM(lcDyelot),' ')+'|'+ALLTRIM(lcFromWare)) = 2
  IF !llReEnter
    *DO gpAdFabWar WITH lcFabric, lcColor, SPACE(10), lcFromWare
    IF llDyeLvl
      *--E301135 (Start)
      *--B602678 (Start)
      *DO gpAdFabWar WITH lcFabric, lcColor, lcDyelot, lcFromWare 
      DO gpAdFabWar WITH lcFabric, lcColor, lcDyelot, lcFromWare ,lcTmpDyRel
      *--B602678 (Start)
      *--E301135 (End)
    *--B602678 (Start)
    ELSE
      DO gpAdFabWar WITH lcFabric, lcColor, SPACE(10), lcFromWare
    *--B602678 (End)
    ENDIF
  ENDIF
ENDIF
endif
*!*************************************************************
*! Name      : lfvKFabric
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the fabric field from the browse button 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvKFabric

lcFabric = PADR('?',7)
=lfvFabric()
*!*************************************************************
*! Name      : lfvKColor
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the color field from the browse button 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvKColor

lcColor = PADR('?',6)
=lfvColor()
*!*************************************************************
*! Name      : lpSavScr
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : local procedure to save
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
PROCEDURE lpSavScr
PRIVATE lnAlias,llRoll,lcSeekKey

llExtCall = .T.
*B602843 (Start)
*-- gfMatCrl (start)
*PRIVATE lcFabric,lcColor,lnRet
*lcFabric = IIF(EMPTY(&lcTmpPO..cFabric1),&lcTmpPO..Fabric,&lcTmpPO..cFabric1)
*lcColor  = IIF(EMPTY(&lcTmpPO..cFabric1),&lcTmpPO..Color,&lcTmpPO..cColor1)
*-- gfMatCrl (end)
*B602843 (End)






*E301797,1 (Begin) Save shipment.
*SELECT(lcTmpPO)
IF laRecType[lnRecType,2] = 'S'
  SELECT(lcTmpPOSp)
ELSE
  SELECT(lcTmpPO)
ENDIF
*E301797,1 (End)

IF lnLastRec > 0
  GOTO lnLastRec
ENDIF
=lfwLine()
IF !lfvBrow()
  llCSave = .F.
  RETURN
ENDIF



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
*E301797,1 (Begin) Save shipment.
IF laRecType[lnRecType,2] = 'S'
  =lfSavShp()
  RETURN
ENDIF
*E301797,1 (End)

*B606285,1 ABD - Add this code in order to do not issue return PO if the the
*B606285,1     costing method is "FI" and the is no available onhand [Begin]
IF lcKeyType = 'R' .AND. lcMtCstMth $ 'FI' .AND. !lfvAplyQty()
  llCSave = .F.  
  RETURN
ENDIF
*B606285,1 ABD - [End]


lcSeekKey = SPACE(01)
llRoll  = .F.
lnAlias = SELECT()
IF llTrkRolls AND lcKeyType = 'P'
  SELECT(lcTmpPo)
  SCAN FOR (nFabTotQty > 0 AND TRANCD <> '4')
    lcSeekKey = IIF(EMPTY(cFabric1),Fabric+Color,cFabric1+cColor1)
    =SEEK(lcSeekKey,'Fabric')
    *B603188,1 HDM [Start] Check rolls entered for each line
    *                      If not don't save
    IF Fabric.lTrkRolls
      llRoll = .T.
      IF !EMPTY(lcTmpRoll)
        SELECT(lcTmpRoll)
        lcTmpRTag = ORDER()
        SET ORDER TO lcTmpRoll2
        *B803712,1 ABD Cheak only for type recive. [Begin]
        IF lcTrancd = '2'
          *B803712,1 ABD [End]
          IF !SEEK(lcSeekKey+&lcTmpPo..cWarecode+&lcTmpPo..Dyelot,lcTmpRoll) OR;
             (SEEK(lcSeekKey+&lcTmpPo..cWarecode+&lcTmpPo..Dyelot,lcTmpRoll) AND NAPPLY = 0)
            SET ORDER TO &lcTmpRTag
            =gfModalGen('TRM36096B36000','ALERT','rolls')
            llCSave = .F.
            SELECT(lcTmpPo)
            RETURN
          ENDIF
          *B803712,1 ABD End for if statment. [Begin]
        ENDIF
        *B803712,1 ABD [End]
      ELSE
        =gfModalGen('TRM36096B36000','ALERT','rolls')
        llCSave = .F.
        SELECT(lcTmpPo)
        RETURN
      ENDIF
    ENDIF
    *B603188,1 HDM [End]
  ENDSCAN
ENDIF

IF (llTrkRolls OR lcMtCstMth $ 'LIF') AND lcKeyType = 'R'
  SELECT(lcTmpPo)
  SCAN FOR (nFabTotQty > 0 AND TRANCD <> '4')
    lcSeekKey = IIF(EMPTY(cFabric1),Fabric+Color,cFabric1+cColor1)
    =SEEK(lcSeekKey,'Fabric')
    IF Fabric.lTrkRolls
      llRoll = .T.
      *B603188,1 HDM [Start] Check rolls entered for each line
      *                      If not don't save
      IF !EMPTY(lcTmpRoll) AND USED(lcTmpRoll)
        SELECT(lcTmpRoll)
        lcTmpRTag = ORDER()
        SET ORDER TO lcTmpRoll2
        
        *-- IF  We can't find the roll record
        *-- OR  We found it
        *-- AND The user didn't apply any quantity
        *--         Stop the Saving process
        IF !SEEK(lcSeekKey+&lcTmpPo..cWarecode+&lcTmpPo..Dyelot,lcTmpRoll)
          SET ORDER TO &lcTmpRTag
          =gfModalGen('TRM36096B36000','ALERT','rolls')
          llCSave = .F.
          SELECT(lcTmpPo)
          RETURN
        ELSE
          = SEEK(lcSeekKey+&lcTmpPo..cWarecode+&lcTmpPo..Dyelot,lcTmpRoll)
          LOCATE REST WHILE cfabric+ccolor+cwarecode+cdyelot+STR(lineno,6) ;
                          = lcSeekKey + &lcTmpPo..cWarecode +  &lcTmpPo..Dyelot;
                          FOR NAPPLY > 0
          IF !FOUND()
            SET ORDER TO &lcTmpRTag
            =gfModalGen('TRM36096B36000','ALERT','rolls')
            llCSave = .F.
            SELECT(lcTmpPo)
            RETURN
          ENDIF
        ENDIF
      ELSE
        =gfModalGen('TRM36096B36000','ALERT','rolls')
        llCSave = .F.
        SELECT(lcTmpPo)
        RETURN
      ENDIF
      *B603188,1 HDM [End]
    ELSE
      IF lcMtCstMth $ 'LIF'
        IF !SEEK(lcSeekKey+&lcTmpPo..cWarecode+&lcTmpPo..Dyelot,lcTmpJour)
          =gfModalGen('TRM36096B36000','ALERT','lots')
          llCSave = .F.
          SELECT(lcTmpPo)
          RETURN
        ENDIF
        SELECT(lcTmpJour) 
        SCAN WHILE cFabric+cColor+cWareCode+cDyelot = ;
                   lcSeekKey+&lcTmpPo..cWarecode+&lcTmpPo..Dyelot
          IF nApply > 0
            llRoll = .T.
            EXIT
          ENDIF
        ENDSCAN
      ELSE
        llRoll = .T.
      ENDIF
    ENDIF  
  ENDSCAN
  IF !llRoll  
    =gfModalGen('TRM36096B36000','ALERT',IIF(Fabric.lTrkRolls,'rolls','lots'))
    llCSave = .F.
    SELECT(lcTmpPo)
    RETURN
  ENDIF  
ENDIF

lcRecSess = gfsequence('GLSESSION')

=SEEK('O'+lcUnsmsPrg+gcUser_id+lcSession,'UNCMSESS')
SELECT UNCMSESS
REPLACE cCurrObj WITH 'pbSav'

=lfUpdPofln()
SELECT(lcTmpPo)

lnRecCount = RECCOUNT()

*B602843 (Start)
*-- gfMatCrl (start)
PRIVATE lcFabric,lcColor,lnRet
*-- gfMatCrl (End)
*B602843 (End)

PRIVATE lnCurrRec
STORE 0 TO lnCurrRec

SCAN FOR nFabTotQty > 0 
  lnCurrRec = lnCurrRec + 1
  *B602843 (Start)  
  *-- gfMatCrl (start)
  lcFabric = IIF(EMPTY(&lcTmpPO..cFabric1),&lcTmpPO..Fabric,&lcTmpPO..cFabric1)
  lcColor  = IIF(EMPTY(&lcTmpPO..cFabric1),&lcTmpPO..Color,&lcTmpPO..cColor1)
  *-- gfMatCrl (end)
  *B602843 (End)
  
  =SEEK(lcKeyType+&lcTmpPo..POMAT,'POFHDR')
  =SEEK(lcKeyType+&lcTmpPo..POMAT+&lcTmpPo..FABRIC+&lcTmpPo..COLOR+'1','POFLN')
  =lfUpdVend()

  *B603233,1 HDM [START] Stop Calling these functions as gfMatcrl Updates
  *                      Fabric , FabDye , MatinvJl Files
  *=lfUpdFabDye()
  *=lfUpdFabric()
  *=lfUpdMatInvJl() 
  *B603233,1 HDM [End]

  *-- B602118  (Start)
  *-- AAMER 11/03/98
  *-- lnOldLinQty variable that hold the original line qty after update it
  PRIVATE lnOldLinQty
  
  *B606455,1 KHM 09/16/2002 (Begin) Getting the open quantity for each line by deducting
  *B606455,1                the budget quantity from the received one.
  *lnOldLinQty = POFLn.nFabTOTQTY
  lnOldLinQty = lfGetOpnQ()
  *B606455,1 KHM 09/16/2002 (End)
  
  *-- B602118  (End)

  lnRet = lfFilsUpd()

  IF lnRet <> 0
    =lfUpdLines()
    =lfUpdHdr()
  
    *--gfMatCrl
    if .f.
    IF llGlLink
      =lfUpdGlLink()
    ENDIF
    IF llTrkRolls 
      =lfUpdRolls()
    ENDIF
    endif
  
    IF llWareLoc  
      =lfUpdlOC()
    ENDIF
  ENDIF
ENDSCAN

IF llGlLink
  =lfUpdGlFile()
ENDIF  
=SEEK('O'+lcUnsmsPrg+gcUser_id+lcSession,'UNCMSESS')
SELECT UNCMSESS
REPLACE Status WITH 'C'
*E300954,1 SWK 08/09/1998 Call the screen of arranging dyelots in case of P/O
IF llDyelot AND lcScrType = 'P'
  =gfArDyRl('','',lcTmpPo,.T.)
ENDIF
*E300954,1 (End)

*B803712,1 ABD If save correct got to view mode. [Begin]
*laScrMode     =.F.
*laScrMode[1]  =.T.
*lcScrMode = 'S' 
*SHOW GETS
*B803712,1 ABD [End]
*C200256,1 (Begin) Receive Qty for CON10.
IF ASCAN(laEvntTrig,PADR("PRNTIT",10)) <> 0
  SELECT(lcTmpPo)
  COUNT FOR TRANCD = "2" TO lnPrgnO    
  LOCATE FOR TRANCD = "2"
  IF FOUND()
    llFromCost = .F.
    SET FILTER TO TRANCD = "2"
    lnNoOfCop = 1
    lcTranTit = IIF(cmattype = 'R','RET PO','PO')
    lcTrans   = "POMAT"
    lcTotQty  = "nFabTotQty"
    lcRecDate = "DATE"
    lcVenCode = "VENDOR"
    IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Do you wish to print the received quantities?')=1)
      = gfDoTriger("MAPOREC",PADR("PRNTIT",10))    
   ENDIF
 ENDIF  
ENDIF  
*C200256,1 (Begin)
*B605212,1 (Begin) Call Bar Code Printing Report.
*                  Would you like to print Barcode labels ?
SELECT Rolls
LOCATE
IF llTrkRolls .AND. ( lcMtCstMth = "L" OR !EOF('Rolls') )
  IF gfModalGen('QRM36171B42002','DIALOG','Rolls label') = 1
    PRIVATE lcRunRep , lcRepDir , lcCallFrom
    lcRunRep  = 'MABARC'           && Calling BarCode labels with another report ID
    lcRepDir = gcRepHome+gcAct_Appl+'REPORT'  && To call MaReport with 'MaBarc'
    lcCallFrom = "P"      && Report is called from Program from Transaction menu

    *B605659,1 RAE [START]
    llFromMan = .F.
    *B605659,1 RAE [END]

    DO &lcRepDir WITH lcRunRep
  ENDIF
ENDIF
*B605212,1 (End)

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfUpdPofln
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : function to update pofln file with the added line with trans. type = '1'
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfUpdPofln
PRIVATE lnalias

lnalias = SELECT()
*E301797,1 (Begin) Handle shipment.
IF laRecType[lnRecType,2] = 'S'
  SELECT(lcTmpPoSp)
  SCATTER MEMVAR MEMO
  SCAN
    IF !SEEK(&lcTmpPoSp..FABRIC+&lcTmpPoSp..COLOR+lckeyType+&lcTmpPoSp..POMAT+'1','matshpdt')
      SELECT matshpdt
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE cMatType  WITH lcKeyType          ,; 
              POMAT     WITH &lcTmpPoSp..POMAT    ,;
              FABRIC    WITH &lcTmpPoSp..FABRIC   ,;
              COLOR     WITH &lcTmpPoSp..COLOR    ,;
              VENDOR    WITH &lcTmpPoSp..VENDOR   ,;
              cWareCode WITH &lcTmpPoSp..cWareCode,;
              TRANCD    WITH '1',;
              LINENO    WITH &lcTmpPoSp..LineNo
    ENDIF
    IF SEEK(&lcTmpPoSp..FABRIC+&lcTmpPoSp..COLOR+lckeyType+&lcTmpPoSp..POMAT+'3','matshpdt')
      SELECT matshpdt 
      LOCATE REST WHILE fabric+color+cmattype+pomat+trancd = &lcTmpPoSp..FABRIC+&lcTmpPoSp..COLOR+lckeyType+&lcTmpPoSp..POMAT+'3';
                  FOR SHIPNO = lcShpCode
      =RLOCK()
      REPLACE nFabTotQty WITH MAX(nFabTotQty-&lcTmpPoSp..nFabTotQty,0)
      UNLOCK
    ENDIF
    IF !SEEK(lckeyType+&lcTmpPoSp..POMAT+&lcTmpPoSp..FABRIC+&lcTmpPoSp..COLOR+'1','POFLN')
      SELECT pofln
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE cMatType  WITH lcKeyType          ,; 
              POMAT     WITH &lcTmpPoSp..POMAT    ,;
              FABRIC    WITH &lcTmpPoSp..FABRIC   ,;
              COLOR     WITH &lcTmpPoSp..COLOR    ,;
              VENDOR    WITH &lcTmpPoSp..VENDOR   ,;
              cWareCode WITH &lcTmpPoSp..cWareCode,;
              TRANCD    WITH '1',;
              LINENO    WITH &lcTmpPoSp..LineNo
    ENDIF

  ENDSCAN
  SELECT(lnalias)
  RETURN
ENDIF
*E301797,1 (End)

SELECT(lcTmpPo)
SCAN
  IF !SEEK(lckeyType+&lcTmpPo..POMAT+&lcTmpPo..FABRIC+&lcTmpPo..COLOR+'1','POFLN')
    SELECT POFLN
    APPEND BLANK
    REPLACE cMatType  WITH lcKeyType          ,; 
            POMAT     WITH &lcTmpPo..POMAT    ,;
            FABRIC    WITH &lcTmpPo..FABRIC   ,;
            COLOR     WITH &lcTmpPo..COLOR    ,;
            VENDOR    WITH &lcTmpPo..VENDOR   ,;
            cWareCode WITH &lcTmpPo..cWareCode,;
            TRANCD WITH '1'
  ENDIF
ENDSCAN
SELECT(lnalias)

*!*************************************************************
*! Name      : lfUpdVend
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : update the vendor file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfUpdVend
PRIVATE lnalias

lnalias = SELECT()
*E301797,1 (Begin) Handle shipment.
IF laRecType[lnRecType,2] = 'S'
SELECT APVENDOR
  IF SEEK(&lcTmpPoSp..VENDOR)
    =RLOCK()
    REPLACE dVenLPord WITH IIF(lcKeyType = 'P',DATE(),dVenLPord),;
            nVenLPoRA WITH IIF(lcKeyType = 'R',nVenLPoRA,IIF(cVendCode=lcOldVen,nVenLPoRA+&lcTmpPoSp..nFaBTotQty*&lcTmpPoSp..NLAN_COST1,&lcTmpPoSp..nFaBTotQty*&lcTmpPoSp..NLAN_COST1)),;
            nVenOpnPO WITH IIF(&lcTmpPoSp..UnitP = 0,0,IIF(lcKeyType = 'P',nVenOpnPO - MIN(&lcTmpPoSp..nFaBTotQty,POFLN.nFaBTotQty)*(POFLN.nCost1*POFHDR.nPriceRat/&lcTmpPoSp..UnitP),;
                               nVenOpnPO + MIN(&lcTmpPoSp..nFaBTotQty,POFLN.nFaBTotQty)*(POFLN.nCost1*POFHDR.nPriceRat/&lcTmpPoSp..UnitP)))

    UNLOCK        
  ENDIF
  lcOldVen = cVendCode
  SELECT(lnalias)
  RETURN
ENDIF
*E301797,1 (End)

SELECT APVENDOR
IF SEEK(&lcTmpPo..VENDOR)
  =RLOCK()
  REPLACE dVenLPord WITH IIF(lcKeyType = 'P',DATE(),dVenLPord),;
           nVenLPoRA WITH IIF(lcKeyType = 'R',nVenLPoRA,IIF(cVendCode=lcOldVen,nVenLPoRA+&lcTmpPo..nFaBTotQty*&lcTmpPo..NLAN_COST1,&lcTmpPo..nFaBTotQty*&lcTmpPo..NLAN_COST1)),;
          nVenOpnPO WITH IIF(lcKeyType = 'P',nVenOpnPO - MIN(&lcTmpPo..nFaBTotQty,POFLN.nFaBTotQty)*(POFLN.nCost1*POFHDR.nPriceRat/&lcTmpPo..UnitP),;
                                             nVenOpnPO + MIN(&lcTmpPo..nFaBTotQty,POFLN.nFaBTotQty)*(POFLN.nCost1*POFHDR.nPriceRat/&lcTmpPo..UnitP))  

  UNLOCK        
ENDIF
lcOldVen = cVendCode
SELECT(lnalias)

*B603233,1 HDM [START] Remove this function as gfMatcrl Updates
*                      Fabric , FabDye , MatinvJl Files

*!*************************************************************
*! Name      : lfUpdFabDye
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : update the FabDye file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*!*************************************************************
*! Name      : lfUpdFabric
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : update the Fabric file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*!*************************************************************
*! Name      : lfUpdMatInvJl
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : update the MatInvJl file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*!*************************************************************
*! Name      : lfUpdLines
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : update the pofln file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfUpdLines
PRIVATE lnalias

lnalias = SELECT()
*E301797,1 (Begin) Handle shipment.
IF laRecType[lnRecType,2] = 'S'
  SELECT matshpdt
  APPEND BLANK
  REPLACE POMAT      WITH &lcTmpPoSp..POMAT       ,;
          cMatType   WITH &lcTmpPoSp..cMatType ,; 
          VENDOR     WITH &lcTmpPoSp..VENDOR      ,;
          FABRIC     WITH &lcTmpPoSp..FABRIC      ,;
          COLOR      WITH &lcTmpPoSp..COLOR       ,;
          lineno     WITH &lcTmpPoSp..lineno      ,; 
          cFABRIC1   WITH &lcTmpPoSp..cFABRIC1    ,;
          cCOLOR1    WITH &lcTmpPoSp..cCOLOR1     ,;
          cFabGrade  WITH &lcTmpPoSp..cFabGrade   ,;
          cWareCode  WITH &lcTmpPoSp..cWareCode   ,;
          PATTERN    WITH &lcTmpPoSp..PATTERN     ,;
          WIDTH      WITH &lcTmpPoSp..WIDTH       ,;
          SHIPNO     WITH SPACE(6)              ,;
          TRANCD     WITH &lcTmpPoSp..TRANCD      ,;
          nFabTOTQTY WITH &lcTmpPoSp..nFabTOTQTY  ,;
          DYELOT     WITH &lcTmpPoSp..DYELOT      ,;
          REFERENCE  WITH &lcTmpPoSp..REFERENCE   ,;
          DATE       WITH ldDate                  ,;
          DPostDate  WITH IIF(llGlLink,ldPost,ldDate)

  REPLACE NLAN_COST1 WITH &lcTmpPoSp..NLAN_COST1  ,;
          NLAN_COST3 WITH &lcTmpPoSp..NLAN_COST3  ,;
          NLAN_COST2 WITH &lcTmpPoSp..NLAN_COST2  ,;
          NLAN_COST4 WITH &lcTmpPoSp..NLAN_COST4
        
  REPLACE NELANCOST1 WITH IIF(&lcTmpPoSp..UNITP = 0,0,IIF(&lcTmpPoSp..SIGNP = '*',&lcTmpPoSp..NLAN_COST1*&lcTmpPoSp..NLANPRRAT/&lcTmpPoSp..UNITP,;
                                                    &lcTmpPoSp..NLAN_COST1/&lcTmpPoSp..NLANPRRAT/&lcTmpPoSp..UNITP)),;
          NELANCOST2 WITH IIF(&lcTmpPoSp..UNITD = 0,0,IIF(&lcTmpPoSp..SIGND = '*',&lcTmpPoSp..NLAN_COST2*&lcTmpPoSp..NLANDURAT/&lcTmpPoSp..UNITD,;
                                                    &lcTmpPoSp..NLAN_COST2/&lcTmpPoSp..NLANDURAT/&lcTmpPoSp..UNITD)),;
          NELANCOST3 WITH IIF(&lcTmpPoSp..UNITD = 0,0,IIF(&lcTmpPoSp..SIGND = '*',&lcTmpPoSp..NLAN_COST3*&lcTmpPoSp..NLANDURAT/&lcTmpPoSp..UNITD,;
                                                    &lcTmpPoSp..NLAN_COST3/&lcTmpPoSp..NLANDURAT/&lcTmpPoSp..UNITD)),;
          NELANCOST4 WITH IIF(&lcTmpPoSp..UNITD = 0,0,IIF(&lcTmpPoSp..SIGND = '*',&lcTmpPoSp..NLAN_COST4*&lcTmpPoSp..NLANDURAT/&lcTmpPoSp..UNITD,;
                                                    &lcTmpPoSp..NLAN_COST4/&lcTmpPoSp..NLANDURAT/&lcTmpPoSp..UNITD))

  REPLACE cRSession  WITH IIF(&lcTmpPoSp..TranCd <> '4',lcRecSess,SPACE(6)),;
          NCOST1     WITH &lcTmpPoSp..NCOST1      ,;
          NCOST2     WITH &lcTmpPoSp..NCOST2      ,;
          NCOST3     WITH &lcTmpPoSp..NCOST3      ,;
          NCOST4     WITH &lcTmpPoSp..NCOST4      ,;
          NECOST1    WITH &lcTmpPoSp..NECOST1     ,;
          NECOST2    WITH &lcTmpPoSp..NECOST2     ,;
          NECOST3    WITH &lcTmpPoSp..NECOST3     ,;
          NECOST4    WITH &lcTmpPoSp..NECOST4     ,;
          nLanPrRat  WITH lnRateP               ,; 
          nLanDuRat  WITH lnRateD

  SCAT MEMVAR MEMO
  SELE POFLN
  APPE BLAN
  GATH MEMVAR MEMO
  *C102525,1 RAE [BEGIN] Replace the value of PO in POFLN in return issue line.
  IF ASCAN(laEvntTrig , PADR('APPPO',10)) <> 0
    =gfDoTriger('MAREREC',PADR('APPPO',10))
  ENDIF
  *C102525,1 RAE [END]
  SELECT(lnalias)
  RETURN
ENDIF
*E301797,1 (End)

SELECT POFLN
APPEND BLANK
REPLACE POMAT      WITH &lcTmpPo..POMAT       ,;
        cMatType   WITH &lcTmpPo..cMatType ,; 
        VENDOR     WITH &lcTmpPo..VENDOR      ,;
        FABRIC     WITH &lcTmpPo..FABRIC      ,;
        COLOR      WITH &lcTmpPo..COLOR       ,;
        lineno     WITH &lcTmpPo..lineno      ,; 
        cFABRIC1   WITH &lcTmpPo..cFABRIC1    ,;
        cCOLOR1    WITH &lcTmpPo..cCOLOR1     ,;
        cFabGrade  WITH &lcTmpPo..cFabGrade   ,;
        cWareCode  WITH &lcTmpPo..cWareCode   ,;
        PATTERN    WITH &lcTmpPo..PATTERN     ,;
        WIDTH      WITH &lcTmpPo..WIDTH       ,;
        SHIPNO     WITH SPACE(6)              ,;
        TRANCD     WITH &lcTmpPo..TRANCD      ,;
        nFabTOTQTY WITH &lcTmpPo..nFabTOTQTY  ,;
        DYELOT     WITH &lcTmpPo..DYELOT      ,;
        REFERENCE  WITH &lcTmpPo..REFERENCE   ,;
        DATE       WITH &lcTmpPO..DATE        ,;
        DPostDate  WITH IIF(llGlLink,ldPost,ldDate)
        
REPLACE NLAN_COST1 WITH &lcTmpPo..NLAN_COST1  ,;
        NLAN_COST3 WITH &lcTmpPo..NLAN_COST3  ,;
        NLAN_COST2 WITH &lcTmpPo..NLAN_COST2  ,;
        NLAN_COST4 WITH &lcTmpPo..NLAN_COST4
        
REPLACE NELANCOST1 WITH IIF(&lcTmpPo..SIGNP = '*',&lcTmpPo..NLAN_COST1*&lcTmpPo..NLANPRRAT/&lcTmpPo..UNITP,;
                                                  &lcTmpPo..NLAN_COST1/&lcTmpPo..NLANPRRAT/&lcTmpPo..UNITP),;
        NELANCOST2 WITH IIF(&lcTmpPo..SIGND = '*',&lcTmpPo..NLAN_COST2*&lcTmpPo..NLANDURAT/&lcTmpPo..UNITD,;
                                                  &lcTmpPo..NLAN_COST2/&lcTmpPo..NLANDURAT/&lcTmpPo..UNITD),;
        NELANCOST3 WITH IIF(&lcTmpPo..SIGND = '*',&lcTmpPo..NLAN_COST3*&lcTmpPo..NLANDURAT/&lcTmpPo..UNITD,;
                                                  &lcTmpPo..NLAN_COST3/&lcTmpPo..NLANDURAT/&lcTmpPo..UNITD),;
        NELANCOST4 WITH IIF(&lcTmpPo..SIGND = '*',&lcTmpPo..NLAN_COST4*&lcTmpPo..NLANDURAT/&lcTmpPo..UNITD,;
                                                  &lcTmpPo..NLAN_COST4/&lcTmpPo..NLANDURAT/&lcTmpPo..UNITD)

REPLACE cRSession  WITH IIF(&lcTmpPo..TranCd <> '4',lcRecSess,SPACE(6)),;
        NCOST1     WITH &lcTmpPo..NCOST1      ,;
        NCOST2     WITH &lcTmpPo..NCOST2      ,;
        NCOST3     WITH &lcTmpPo..NCOST3      ,;
        NCOST4     WITH &lcTmpPo..NCOST4      ,;
        NECOST1    WITH &lcTmpPo..NECOST1     ,;
        NECOST2    WITH &lcTmpPo..NECOST2     ,;
        NECOST3    WITH &lcTmpPo..NECOST3     ,;
        NECOST4    WITH &lcTmpPo..NECOST4     ,;
        nLanPrRat  WITH lnRateP               ,; 
        nLanDuRat  WITH lnRateD

*C102525,1 RAE [BEGIN] Replace the value of PO in POFLN in return issue line.
IF ASCAN(laEvntTrig , PADR('APPPO',10)) <> 0
  =gfDoTriger('MAREREC',PADR('APPPO',10))
ENDIF
*C102525,1 RAE [END]

SELECT(lnalias)

*!*************************************************************
*! Name      : lfUpdHdr
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : update the pofhdr file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfUpdHdr
PRIVATE lnalias
lnalias = SELECT()

*E301797,1 (Begin) Handle shipment header file.
IF laRecType[lnRecType,2] = 'S'
  *--Update MatshpHd---------------------------------------------------
  *--Delete the shipment lines for coplete receive lines.
  SELECT matshpdt
  SET ORDER TO TAG Shipno
  =SEEK(lcShpCode)
  DELETE REST WHILE ShipNo = lcShpCode FOR TranCd = '3' AND nFabTotQty = 0
  IF SEEK(lcShpCode)
    LOCATE REST WHILE ShipNo = lcShpCode FOR TranCd = '3' AND SEEK(cmattype+POMAT,'POFHDR') AND POFHDR.STATUS <> 'C'
    lcShpStat = IIF(FOUND(),'O','C')
  ELSE
    lcShpStat = 'C'
  ENDIF     
  SELECT matshphd
  =SEEK(lcShpCode)
  =RLOCK()
  REPLACE Status     WITH lcShpStat ,;
          RecvStk    WITH RecvStk + lnTotQty1,;
          recvDam    WITH recvDam + lnTotQty3,;
          RecvCan    WITH RecvCan + lnTotQty2,;
          Reference  WITH lcShpRef  ,;
          Entered    WITH ldEnterd  ,;
          Cartons    WITH lnCartons ,;
          AirWayB    WITH lcAirWayB ,;
          Eta        WITH ldEta
  =gfAdd_Info('MatshpHd')
  UNLOCK
  *--Call TraceKey global function.
  =gfTraceKey('matshphd',lcShpCode,'M')
  SELECT POFHDR
  =RLOCK()
  *-- Update the total recieved quantity in the pofhdr file
  SELECT (lcTmpPoSp)
  SCAN
    =SEEK(cmattype+POMAT,'POFHDR')
    SELECT POFHDR
    *--E301797,9 Deduct the received quantity or the budject whichever less [in case of overreceive]
    =SEEK(lcKeyType+&lcTmpPoSp..POMAT+&lcTmpPoSp..FABRIC+&lcTmpPoSp..COLOR+'1','POFLN')
    REPLACE nPO_Open WITH Max(nPO_Open - MIN(POFLN.nFabTotQty,&lcTmpPoSp..nFabTotQty),0)

    REPLACE  nfbReceive WITH IIF(&lcTmpPoSp..TRANCD='2',nfbReceive + &lcTmpPoSp..nFabTOTQTY,nfbReceive),;
             nFabCancel WITH IIF(&lcTmpPoSp..TRANCD='4',nFabCancel + &lcTmpPoSp..nFabTOTQTY,nFabCancel),;
             nFabDamage WITH IIF(&lcTmpPoSp..TRANCD='3',nFabDamage + &lcTmpPoSp..nFabTOTQTY,nFabDamage)
    
    *-- Update the costing fields in the pofhdr file
    IF &lcTmpPoSp..TRANCD <> '4'
      REPLACE NLAN_COST1 WITH NLAN_COST1 +(&lcTmpPoSp..NLAN_COST1;
                                         * &lcTmpPoSp..nFabTOTQTY),;
              NLAN_COST3 WITH NLAN_COST3 +(&lcTmpPoSp..NLAN_COST3;
                                         * &lcTmpPoSp..nFabTOTQTY),;
              NLAN_COST4 WITH NLAN_COST4 +(&lcTmpPoSp..NLAN_COST4;
                                         * &lcTmpPoSp..nFabTOTQTY),;
              NLAN_COST2 WITH NLAN_COST2 +(&lcTmpPoSp..NLAN_COST2;
                                         * &lcTmpPoSp..nFabTOTQTY)
       IF &lcTmpPoSp..UNITP <> 0
         REPLACE NELANCOST1 WITH NELANCOST1 +IIF(&lcTmpPoSp..SIGNP = '*',&lcTmpPoSp..NLAN_COST1*&lcTmpPoSp..NLANPRRAT/&lcTmpPoSp..UNITP,;
                                                                      &lcTmpPoSp..NLAN_COST1/&lcTmpPoSp..NLANPRRAT/&lcTmpPoSp..UNITP)*&lcTmpPoSp..nFabTOTQTY
       ENDIF
       IF &lcTmpPoSp..UNITD <> 0
         REPLACE NELANCOST3 WITH NELANCOST3 +IIF(&lcTmpPoSp..SIGND = '*',&lcTmpPoSp..NLAN_COST3*&lcTmpPoSp..NLANDURAT/&lcTmpPoSp..UNITD,;
                                                 &lcTmpPoSp..NLAN_COST3/&lcTmpPoSp..NLANDURAT/&lcTmpPoSp..UNITD)* &lcTmpPoSp..nFabTOTQTY,;
                 NELANCOST4 WITH NELANCOST4 +IIF(&lcTmpPoSp..SIGND = '*',&lcTmpPoSp..NLAN_COST4*&lcTmpPoSp..NLANDURAT/&lcTmpPoSp..UNITD,;
                                                 &lcTmpPoSp..NLAN_COST4/&lcTmpPoSp..NLANDURAT/&lcTmpPoSp..UNITD)* &lcTmpPoSp..nFabTOTQTY,;
                 NELANCOST2 WITH NELANCOST2 +IIF(&lcTmpPoSp..SIGND = '*',&lcTmpPoSp..NLAN_COST2*&lcTmpPoSp..NLANDURAT/&lcTmpPoSp..UNITD,;
                                                 &lcTmpPoSp..NLAN_COST2/&lcTmpPoSp..NLANDURAT/&lcTmpPoSp..UNITD)* &lcTmpPoSp..nFabTOTQTY
       ENDIF                                                             
    ENDIF
    *-- update the total damaged quantity in the pofhdr file
    
    
  ENDSCAN
  SELECT POFHDR
  IF nPO_Open = 0
    REPLACE STATUS WITH 'C'
  ENDIF
  lnAlias = SELECT(0)
  RETURN
ENDIF
*E301797,1 (End)

SELECT POFHDR
=RLOCK()
*-- Update the total recieved quantity in the pofhdr file
REPLACE nfbReceive WITH IIF(&lcTmpPo..TRANCD='2',nfbReceive + &lcTmpPo..nFabTOTQTY,nfbReceive)
*-- Update the costing fields in the pofhdr file
IF &lcTmpPo..TRANCD <> '4'
  REPLACE NLAN_COST1 WITH NLAN_COST1 +(&lcTmpPo..NLAN_COST1;
                                     * &lcTmpPo..nFabTOTQTY),;
          NLAN_COST3 WITH NLAN_COST3 +(&lcTmpPo..NLAN_COST3;
                                     * &lcTmpPo..nFabTOTQTY),;
          NLAN_COST4 WITH NLAN_COST4 +(&lcTmpPo..NLAN_COST4;
                                     * &lcTmpPo..nFabTOTQTY),;
          NLAN_COST2 WITH NLAN_COST2 +(&lcTmpPo..NLAN_COST2;
                                     * &lcTmpPo..nFabTOTQTY)
   REPLACE NELANCOST1 WITH NELANCOST1 +IIF(&lcTmpPo..SIGNP = '*',&lcTmpPo..NLAN_COST1*&lcTmpPo..NLANPRRAT/&lcTmpPo..UNITP,;
                                                                &lcTmpPo..NLAN_COST1/&lcTmpPo..NLANPRRAT/&lcTmpPo..UNITP)*&lcTmpPo..nFabTOTQTY,;
          NELANCOST3 WITH NELANCOST3 +IIF(&lcTmpPo..SIGND = '*',&lcTmpPo..NLAN_COST3*&lcTmpPo..NLANDURAT/&lcTmpPo..UNITD,;
                                                                &lcTmpPo..NLAN_COST3/&lcTmpPo..NLANDURAT/&lcTmpPo..UNITD)* &lcTmpPo..nFabTOTQTY,;
          NELANCOST4 WITH NELANCOST4 +IIF(&lcTmpPo..SIGND = '*',&lcTmpPo..NLAN_COST4*&lcTmpPo..NLANDURAT/&lcTmpPo..UNITD,;
                                                                &lcTmpPo..NLAN_COST4/&lcTmpPo..NLANDURAT/&lcTmpPo..UNITD)* &lcTmpPo..nFabTOTQTY,;
          NELANCOST2 WITH NELANCOST2 +IIF(&lcTmpPo..SIGND = '*',&lcTmpPo..NLAN_COST2*&lcTmpPo..NLANDURAT/&lcTmpPo..UNITD,;
                                                                &lcTmpPo..NLAN_COST2/&lcTmpPo..NLANDURAT/&lcTmpPo..UNITD)* &lcTmpPo..nFabTOTQTY
ENDIF
*-- update the total damaged quantity in the pofhdr file
IF &lcTmpPo..TRANCD='3'  
  REPLACE nFabDamage WITH nFabDamage + &lcTmpPo..nFabTotQty
ENDIF
*-- update the total canceled quantity in the pofhdr file
IF &lcTmpPo..TRANCD='4'  
    REPLACE nFabCancel WITH nFabCancel + &lcTmpPo..nFabTotQty
ENDIF
*-- update the total open quantity in the pofhdr file

*--B602118 (Start)
*-- AAMER 11/03/98
*-- the comparison should be between the old qty and the new qty
*-- the old qty in POFLN file has been updated with Receiving Qty
*-- so we compare with lnOldLinQty which hold the line qty before updating
*REPLACE nPO_Open WITH Max(nPO_Open - MIN(POFLN.nFabTotQty,&lcTmpPo..nFabTotQty),0)

*B603950,1 ABD Decrease PO Qty only from one color not from all Open qty . [Begin]
*REPLACE nPO_Open WITH Max(nPO_Open - MIN(lnOldLinQty,&lcTmpPo..nFabTotQty),0)
lnAlias = SELECT(0)
SELECT POFLN
=SEEK(lcKeyType + &lcTmpPo..Pomat + &lcTmpPo..Fabric +&lcTmpPo..Color)
lnTotQty = 0

SCAN REST WHILE  cMattype+Pomat+Fabric+Color+Trancd = lcKeyType + &lcTmpPo..Pomat + &lcTmpPo..Fabric +&lcTmpPo..Color
  IF Trancd = '1'
    lnTotQty = lnTotQty + nFabTotQty
  ELSE
    lnTotQty = lnTotQty - nFabTotQty
  ENDIF
ENDSCAN
SELECT (lnAlias)
*B803914,1 AAN Remove a cond. that total qty. must be >=0 [Start].
*IF lnTotQty >= 0
*REPLACE nPO_Open WITH Max(nPO_Open - MIN(lnOldLinQty,&lcTmpPo..nFabTotQty),0)
*ENDIF
REPLACE nPO_Open WITH Max(nPO_Open - MIN(lnOldLinQty,&lcTmpPo..nFabTotQty),0)
*B803914,1 AAN Remove a cond. that total qty. must be >=0 [End].
*B603950,1 ABD [End]

*--B602118 (End)

*-- check if the oprn quantity reaches zero then the P/O sattus will be complete
IF nPO_Open = 0
  REPLACE STATUS WITH 'C'
  
  *C200376,1 AMH Trigger for customer PUFFA to update the style critical path file [Start]
  IF ASCAN(laEvntTrig , PADR('STYCRMAT',10)) <> 0
    =gfDoTriger('MAPOREC',PADR('STYCRMAT',10))
  ENDIF
  *C200376,1 AMH [End]
  
ENDIF
UNLOCK
SELECT(lnalias)

*!*************************************************************
*! Name      : lfUpdGlLink
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : update the temp. gldist file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfUpdGlLink
PRIVATE lnAlias

lnAlias = SELECT()
IF &lcTmpPo..TranCd <> '4'
  *-- Receive the quantity by => < DEBIT  > Inventory Control
  *--                            < CREDIT > P/O Clearing
  *-- Amount = Recv. Qty. * Cost ( Tot. landed cost )
  lnAmount1 = &lcTmpPo..nFabTotQty * &lcTmpPo..nLan_Cost1
  lnAmount2 = &lcTmpPo..nFabTotQty * (&lcTmpPo..nLan_Cost2+&lcTmpPo..nLan_Cost3+&lcTmpPo..nLan_Cost4)
                  
  *-- If it is return we should opposite the sigen (Start)
  *-- AAMER 11/11/98
  IF lcScrType = 'R'
    lnAmount1 = -lnAmount1
    lnAmount2 = -lnAmount2
  ENDIF
  *-- If it is return we should opposite the sigen (End)  
  
  *-- 1) Inventory Control                 < DEBIT >
  *-- Category key for "Material Inventory Control"........=> '015'
  DO GlDist WITH lcLinkCode, '015', lnAmount1, 'MO',&lcTmpPo..POMAT, ldPost,;
                 lcGlFYear, lcGlPeriod, '&lcGlDTemp',' ',POFHDR.cPriceCur,&lcTmpPo..UnitP,&lcTmpPo..nLanPrRat

  *E300954,1 SWK 08/09/1998 Replace the PO/Clrearing account('017') with WIP account('013')
  *-- 2) P/O Clearing                     < CREDIT >
  *-- Category key for "Material P/O clearing..............=> '013'
  *DO GlDist WITH lcLinkCode, '017', -(lnAmount1), 'MO',&lcTmpPo..POMAT, ldPost,;
                 lcGlFYear, lcGlPeriod, '&lcGlDTemp',' ',POFHDR.cPriceCur,&lcTmpPo..UnitP,&lcTmpPo..nLanPrRat
  DO GlDist WITH lcLinkPO, '013', -(lnAmount1), 'MO',&lcTmpPo..POMAT, ldPost,;
                 lcGlFYear, lcGlPeriod, '&lcGlDTemp',' ',POFHDR.cPriceCur,&lcTmpPo..UnitP,&lcTmpPo..nLanPrRat
  *E300954,1(End) 

  *-- 1) Inventory Control                 < DEBIT >
  *-- Category key for "Material Inventory Control"........=> '015'
  DO GlDist WITH lcLinkCode, '015', lnAmount2, 'MO',&lcTmpPo..POMAT, ldPost,;
                 lcGlFYear, lcGlPeriod, '&lcGlDTemp',' ',POFHDR.cDutyCur,&lcTmpPo..UnitD,&lcTmpPo..nLanDuRat

  *E300954,1 SWK 08/09/1998 Replace the PO/Clrearing account('017') with WIP account('013')
  *-- 2) P/O Clearing                     < CREDIT >
  *-- Category key for "Material P/O clearing..............=> '013'
  *DO GlDist WITH lcLinkCode, '017', -(lnAmount2), 'MO',&lcTmpPo..POMAT, ldPost,;
                 lcGlFYear, lcGlPeriod, '&lcGlDTemp',' ',POFHDR.cDutyCur,&lcTmpPo..UnitD,&lcTmpPo..nLanDuRat
  DO GlDist WITH lcLinkPO, '013', -(lnAmount2), 'MO',&lcTmpPo..POMAT, ldPost,;
                 lcGlFYear, lcGlPeriod, '&lcGlDTemp',' ',POFHDR.cDutyCur,&lcTmpPo..UnitD,&lcTmpPo..nLanDuRat
  *E300954,1 (End) 
  IF lcMtCstMth<> 'LFI' .AND. lnOldStk < 0 .AND. lnOldCost <> lnNewCost
    lnquantity = MIN(&lcTmpPo..nFabTotQty,ABS(lnOldStk)/Fabric.Conv)
    lnAmount   = (lnOldCost-lnNewCost)*lnQuantity
    IF lnAmount <> 0
      *-- NOTE: 
      *-- If lnAmount is greater than zero
      *-- Receive => DEBIT   Inventory Control
      *--            CREDIT  Inventory Adjustment
      *--
      *-- If lnAmount is less than zero
      *-- Issue   => CREDIT Inventory Control
      *--            DEBIT  Inventory Adjustment

      *-- 1) Inventory Control                 
      *-- Category key for "Material Inventory Control"....=> '015'
      DO GlDist WITH lcLinkCode, '015', lnAmount, 'MA', &lcTmpPo..POMAT,;
                     ldPost, lcGlFYear, lcGlPeriod, '&lcGlDTemp',' ','',1,1
       
      *-- 2) Inventory Adjustment          
      *-- Category key for "Material Inventory Adjustment..=> '016'
      DO GlDist WITH lcLinkCode, '016', -(lnAmount), 'MA', &lcTmpPo..POMAT,;
                     ldPost, lcGlFYear, lcGlPeriod, '&lcGlDTemp',' ','',1,1
    ENDIF  &&     IF lnAmount <> 0
  ENDIF    &&     IF (ONHAND < 0) .AND. (lnOldUCost <> lnNewCost)
ENDIF      &&     IF llGlLink

SELECT(lnAlias)
*!*************************************************************
*! Name      : lfUpdGlFile
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : update the gldist file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfUpdGlFile

lcGlSess = gfSEQUENCE('GLSESSION')
SELECT (lcGlDTemp)
REPLACE ALL GLSESSION WITH lcGlSess
*USE
SELECT GLDIST
APPEND FROM (gcWorkDir+lcGlDTemp)
*USE
*!*************************************************************
*! Name      : lfGetTmpFile
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : get the temp. files in case of uncomplete session
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfGetTmpFile
PRIVATE lcTmpFiles,lcPhyName,lcFileName,lcTagName

lcTmpFiles = UNCMSESS.mTmpFiles
FOR lnCount = 1 TO OCCURS(';',lcTmpFiles)
  lcPhyName  = SUBSTR(lcTmpFiles,1,AT(',',lcTmpFiles,1)-1)
  lcFileName = SUBSTR(lcTmpFiles,AT(',',lcTmpFiles,1)+1,AT(',',lcTmpFiles,2)-AT(',',lcTmpFiles,1)-1)
  lcTagName  = SUBSTR(lcTmpFiles,AT(',',lcTmpFiles,2)+1,AT(';',lcTmpFiles)-AT(',',lcTmpFiles,2)-1)
  
  IF !FILE(gcWorkDir+lcFileName+'.DBF') .OR. ;
     (!EMPTY(lcTagName) .AND. !FILE(gcWorkDir+lcFileName+'.CDX'))
    *-Cannot find temprary file xxx
    =gfModalGen('TRM00260B00000','DIALOG',lcFileName)
    *-The system failed to resume processing on the uncompleted session.
    =gfModalGen('TRM00261B00000','DIALOG')
    llContinue = .F.
    REPLACE UNCMSESS.STATUS WITH 'C'
    UNLOCK IN 'UNCMSESS'
    RETURN(.F.)
  ENDIF
  &lcPhyName = lcFileName

  *--E301077,65 (Start)
  *=gfOpenFile(gcWorkDir+lcFileName,IIF(EMPTY(lcTagName),'',gcWorkDir+lcTagName),'EX')
  SELECT 0
  USE (gcWorkDir+lcFileName) EXCLUSIVE
  SET ORDER TO IIF(EMPTY(lcTagName),'',lcTagName)
  *--E301077,65 (End)
    
  lcTmpFiles = SUBSTR(lcTmpFiles,AT(';',lcTmpFiles)+1)
ENDFOR
=lfGetVar()
*!*************************************************************
*! Name      : lfGetVar
*! Developer : Haytham El_Sheltawi
*! Date      : 05/14/1997
*! Purpose   : Function to restore the varibles that was saved in the field
*!             UNCMSESS.mComent
*!*************************************************************
*! Called from : lfFndUnCSe()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfGetVar
PRIVATE lcVarName , lcVarType , lcVarVal , lcVarStr

lcVarStr = UNCMSESS.mComent      && Varible to hold the field UNCMSESS.mComent
FOR lnCount = 1 TO OCCURS('~',lcVarStr)
  lcVarName = SUBSTR(lcVarStr , 1 , AT('|',lcVarStr) - 1)      && The varible name
  lcVarType = SUBSTR(lcVarStr , AT('|',lcVarStr) + 1 , 1)      && The varible Type
  lcVarVal = SUBSTR(lcVarStr , AT('|' , lcVarStr , 2) + 1 ,;
             AT('~' , lcVarStr) - AT('|' , lcVarStr , 2) -1)          && The varible value
  &lcVarName = IIF(lcVarType='N',VAL(lcVarVal) ,;
               IIF(lcVarType='D',CTOD(lcVarVal) ,;
               IIF(lcVarType='L',(lcVarVal='Y') ,lcVarVal)))
  lcVarStr = SUBSTR(lcVarStr,AT('~',lcVarStr)+1)
ENDFOR    && End of FOR Loop

*!*************************************************************
*! Name      : lfVarStr
*! Developer : Haytham El_Sheltawi
*! Date      : 05/14/1997
*! Purpose   : Function to create a string to be saved in the field
*!             UNCMSESS.mComent
*!*************************************************************
*! Called from : lfvScope()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Array name of a array that hold the varibles name
*!*************************************************************
*! Return      : The created string
*!*************************************************************
*
FUNCTION lfVarStr
PARAMETERS lcArrayNam
PRIVATE lnVarNum , lcReturnSt , lcVarNam

lcReturnSt = ''      && Varible to hold the created string
FOR lnVarNum = 1 TO ALEN(lcArrayNam)
  lcVarNam   = lcArrayNam[lnVarNum]       && The varible name
  lcReturnSt = lcReturnSt + lcVarNam + '|' + TYPE(lcVarNam) + '|' +;
               IIF(TYPE(lcVarNam) = 'N' , ALLTRIM(STR(EVALUATE(lcVarNam))) ,;
               IIF(TYPE(lcVarNam) = 'D' , ALLTRIM(DTOC(EVALUATE(lcVarNam))) ,;
               IIF(TYPE(lcVarNam) = 'L' , IIF(EVALUATE(lcVarNam),'Y','N'), &lcVarNam))) + '~'
ENDFOR    && End of FOR Loop
RETURN lcReturnSt
*!*************************************************************
*! Name        : lfCrUnSess
*! Developer   : Samah Wilson Kirollos
*! Date        : 06/01/97
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : Add record in UnCmSess file
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
FUNCTION lfCrUnSess
PARA lcPushBt
lnAlias = SELECT()
SELECT UNCMSESS

*B606339,1 KHM 08/26/2002 (Begin) Save the process
lnReprocesX = SET('REPROCESS')
*B606339,1 KHM 08/26/2002 (End)

IF !SEEK('O'+lcUnsmsPrg+gcUser_id+lcSession)
  
  *B606339,1 KHM 08/26/2002 (Begin) Add !RLOCK() to the IF command and set reprocess to 1.
  *IF !SEEK('I'+lcUnsmsPrg+gcUser_id) 
  SET REPROCESS TO 1
  IF !SEEK('I'+lcUnsmsPrg+gcUser_id) OR !RLOCK()
  *B606339,1 KHM 08/26/2002 (End)
  
    *-- B602851,1 [Start] Uncomplete session contains only lcTmpPo file information
    *--                   it should also contain the rest of temp. files
    *INSERT INTO UNCMSESS ;
       (Status,cUTranType,cUserId,cSession,cProgram,cCurrScr,dTranDate,cTranTime,mTmpFiles,mComent,ccurrobj) VALUES ;
       ('O',lcUnsmsPrg,gcUser_id,lcSession,lcDialog,'',gdSysDate,TIME(),'lcTmpPO,' + lcTmpPO + ',' + lcTmpPO   + ';'),;
       lfVarStr(@laVariables),'  ')
    *B603188,1 [Start] lcTmpRolls no longer used instead use lcTmpRoll
    *INSERT INTO UNCMSESS ;
       (Status,cUTranType,cUserId,cSession,cProgram,cCurrScr,dTranDate,cTranTime,mTmpFiles,mComent,ccurrobj) VALUES ;
       ('O',lcUnsmsPrg,gcUser_id,lcSession,lcDialog,'',gdSysDate,TIME(),'lcTmpPO,'   + lcTmpPO   + ',' + lcTmpPO   + ';' +'lcTmpJour,'  + lcTmpJour  + ',' + lcTmpJour  + ';' +'lcGlDTemp,' + lcGlDTemp + ',' + ';' +'lcTemLoc,'  + lcTemLoc  + ',' + lcTemLoc  + ';' +IIF(llTrkRolls,'lcTmpRolls,'+lcTmpRolls+','+lcTmpRolls+';',''),;
       lfVarStr(@laVariables),'  ')

    INSERT INTO UNCMSESS ;
       (Status,cUTranType,cUserId,cSession,cProgram,cCurrScr,dTranDate,cTranTime,mTmpFiles,mComent,ccurrobj) VALUES ;
       ('O',lcUnsmsPrg,gcUser_id,lcSession,lcDialog,'',gdSysDate,TIME(),'lcTmpPO,'   + lcTmpPO   + ',' + lcTmpPO   + ';' +'lcTmpJour,'  + lcTmpJour  + ',' + lcTmpJour  + ';' +'lcGlDTemp,' + lcGlDTemp + ',' + ';' +'lcTemLoc,'  + lcTemLoc  + ',' + lcTemLoc  + ';' +IIF(llTrkRolls,'lcTmpRoll,'+lcTmpRoll+','+lcTmpRoll+';',''),;
       lfVarStr(@laVariables),'  ')

   *-- B602851,1 [End]
  ELSE
    *B603188,1 [Start] lcTmpRolls no longer used instead use lcTmpRoll
    *REPLACE Status    WITH 'O',;
            cSession  WITH lcSession,;
            dTranDate WITH gdSysDate,;
            cTranTime WITH TIME(),;
            mTmpFiles WITH 'lcTmpPO,'   + lcTmpPO   + ',' + lcTmpPO   + ';' +;
                           'lcTmpJour,'  + lcTmpJour  + ',' + lcTmpJour  + ';' +;
                           'lcGlDTemp,' + lcGlDTemp + ',' + lcGlDTemp + ';' +;
                           'lcTemLoc,'  + lcTemLoc  + ',' + lcTemLoc  + ';' +;
                           'lcTmpRolls,'+lcTmpRolls+','+lcTmpRolls+';',;
            mComent   WITH lfVarStr(@laVariables),;
            ccurrobj  WITH lcPushBt

    REPLACE Status    WITH 'O',;
            cSession  WITH lcSession,;
            dTranDate WITH gdSysDate,;
            cTranTime WITH TIME(),;
            mTmpFiles WITH 'lcTmpPO,'   + lcTmpPO   + ',' + lcTmpPO   + ';' +;
                           'lcTmpJour,'  + lcTmpJour  + ',' + lcTmpJour  + ';' +;
                           'lcGlDTemp,' + lcGlDTemp + ',' + lcGlDTemp + ';' +;
                           'lcTemLoc,'  + lcTemLoc  + ',' + lcTemLoc  + ';' +;
                           'lcTmpRoll,'+lcTmpRoll+','+lcTmpRoll+';',;
            mComent   WITH lfVarStr(@laVariables),;
            ccurrobj  WITH lcPushBt
    *B603188,1 [End]
  ENDIF
  *B606339,1 KHM 08/26/2002 (Begin) Restore the old reprocess
  SET REPROCESS TO lnReprocesX
  *B606339,1 KHM 08/26/2002 (End)

  =RLOCK('UNCMSESS')
ENDIF
SELECT(lnAlias)
RETURN

*!*************************************************************
*! Name      : lpClsScr
*! Purpose   : Close screen P/O.
*!*************************************************************
PROCEDURE lpClsScr

IF SEEK('O'+lcUnsmsPrg+gcUser_id+lcSession,'UNCMSESS')
    SELECT unCmSess
    REPLACE STATUS WITH 'I'
    llContinue = .F.
ENDIF

*!*************************************************************
*! Name      : lfvRate
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the rate fields
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvRate
PARAMETER lcRate

IF lcRate = 'P'
  IF lnRateP <= 0
    =gfModalGen('INM36014B36000','DIALOG','rate')
    _CUROBJ = OBJNUM(lnRateP)
  ELSE
    REPLACE nLanPrRat WITH lnRateP
  ENDIF
ELSE
  IF lnRateD <= 0
    =gfModalGen('INM36014B36000','DIALOG','rate')
    _CUROBJ = OBJNUM(lnRateD)
  ELSE
    REPLACE nLanDuRat WITH lnRateD
  ENDIF
ENDIF  
*!*************************************************************
*! Name      : lfBrowRoll
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : browse the rolls 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfBrowRoll
PRIVATE lnAlias

lnAlias = SELECT()
SELECT(lcTmpRolls)
lcBrowfields = [cMarker :R:H='',]    +;
               IIF(lcKeyType = 'R',[cSele:R:H='',],[])+;
               [cRollID :R:H='Roll',]+;
               [nQtyBal :R:H='Quantity']

BROWSE FIELDS &lcBrowfields;
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwBRoll() ;
       TITLE lcRollTit     ;
       WINDOW lcWinCh5 IN WINDOW lcWinCh4 ;
       FOR &lcTmpRolls..cRollItem+&lcTmpRolls..Color+&lcTmpRolls..cWareCode+&lcTmpRolls..Dyelot=;
           IIF(!EMPTY(lcQuaFab),lcQuaFab+lcQuaClr,lcFabric+lcColor)+lcWare+lcDyelot;    

SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvRoll
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : called from the roll button to call the roll screen
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvRoll

*B603188,1 [Start] Stop calling local Rolls Screen
*                  and use lfJrRoData.PRG IN gfMatCrl.prg INSTEAD

*IF (lcGrade<>lcFabGrade) AND (EMPTY(lcQuaFab) OR EMPTY(lcQuaClr))
*  _CUROBJ = IIF(EMPTY(lcQuaFab),OBJNUM(lcQuaFab),OBJNUM(lcQuaClr))
*  RETURN
*ENDIF
*lcKey = IIF(EMPTY(lcQuaFab),lcFabric+lcColor,lcQuaFab+lcQuaClr)
*=SEEK(lcKey,'Fabric')
*IF llDyelot 
*  IF FABRIC.cDye_Flg = 'Y' AND EMPTY(lcDyelot)
*    =gfModalGen('TRM36082B36000','ALERT')
*    _CUROBJ = OBJNUM(lcDyelot)
*    RETURN
*  ENDIF  
*ENDIF

*IF lcKeyType = 'R' AND lcMtCstMth $ 'LFI' AND !FABRIC.lTrkRolls
*  IF !lfOldLots()
*    =gfModalGen('TRM36098B00000','ALERT', 'Purchase order receive') 
*    DELETE
*    llNewLine = .F.
*    DO CASE
*      CASE lcTrancd = '2'
*      lnTotQty1 = lnTotQty1 - lnQtyBuy
*  
*      CASE lcTrancd = '3' 
*      lnTotQty2 = lnTotQty2 - lnQtyBuy

*      CASE lcTrancd = '4'
*      lnTotQty3 = lnTotQty3 - lnQtyBuy
*    ENDCASE
*    GOTO TOP
*    =lfwLine()
*    SHOW WINDOW (lcDet_Ttl) REFRESH SAME
*    RETURN 
*  ENDIF
*ENDIF  

*IF lcKeyType  = 'P'
*  lcRollNP   = lcNewP
*  lcRollRP   = lcRemveP 
*ELSE
*  lcRollNP = lcSelecP
*  lcRollRP = lcUnSelP
*ENDIF
*IF lcKeyType = 'R' AND llTrkRolls AND FABRIC.lTrkRolls
*  IF !SEEK(lcKey+lcWare+lcDyelot,lcTmpRolls)
*    IF !lfOldrolls()
*      =gfModalGen('TRM36099B36000','ALERT', ALLTRIM(SUBSTR(lcKey,1,7)) + '/'+ ALLTRIM(SUBSTR(lcKey,8,6))+'/'+lcWare+'/'+lcDyelot)
*      RETURN
*    ENDIF
*  ENDIF
*ENDIF
*lnTotApply = lnQtyBuy
*lnUsrApply = lfRollQty()
*lcRRemSta  = IIF((lcKeyType='P' AND lfButStatus('M'))OR (lcKeyType='R' AND lfButStatus('U')),'ENABLE','DISABLE')
*lcRModSta  = IIF(lcKeyType='P'  AND lfButStatus('M') ,'ENABLE','DISABLE')
*lcrNewSta  = IIF((lcKeyType='R' AND lfButStatus('N') OR lcKeyType='P'),'ENABLE','DISABLE')
*SELECT(lcTmpRolls)

*DEFINE PAD _BROWSE OF _MSYSMENU PROMPT "" KEY CTRL+B
*IF llRollBrow
*  ON SELECTION PAD _BROWSE OF _MSYSMENU ACTIVATE WINDOW (lcRollTit)
*ELSE
*  ON SELECTION PAD _BROWSE OF _MSYSMENU ACTIVATE WINDOW (lcLotlTit)
*ENDIF
*PUSH KEY
**=lfTrapRoll()
*DO (gcScrDir+gcWinAppl+'\MAPOREC4.SPX')
*POP KEY

*Define needed variables for gfMatCrl used in calling rolls screen
PRIVATE lcKey , lnAdjStk , lcFDyelot , lcTrType , lnAlias , ldTrDate ,;
                ldPostDate , lcTrCode , lcRollNP , lcRollRP , lnLineNo

*B603813,1 ABD- let the Qty Buy  > 0 [Begin]
IF lnQtyBuy = 0 .AND. !lfvQty()
  RETURN
ENDIF
*B603813,1 ABD- [End]
lnAlias=SELECT(0)
*-- 
lcTrType    = '1'
lcFDyelot   = lcDyelot
llDyeLvl    = llDyelot
IF lcKeyType  = 'P'
  lcRollNP   = lcNewP
  lcRollRP   = lcRemveP 
ELSE
  lcRollNP = lcSelecP
  lcRollRP = lcUnSelP
ENDIF
ldTrDate    = ldDate
ldPostDate  = IIF(llGlLink,ldPost,ldDate) &&-- Ask MAN
*E301797,1 (Begin) Handle shipment.  
*lcTrCode    = &lcTmpPoSp..POMAT
*lnLineNo    = &lcTmpPoSp..LineNo
IF laRecType[lnRecType,2] = 'S'
  lcTrCode    = &lcTmpPoSp..POMAT
  lnLineNo    = &lcTmpPoSp..LineNo
ELSE
  lcTrCode    = &lcTmpPo..POMAT
  lnLineNo    = &lcTmpPo..LineNo
ENDIF  
*E301797,1 (End)

lcFJlSess   = lcSession
*lcTmpRoll   = lcTmpRolls
*lcFullRoll  = SPACE(0)
lcWareCode  = laWare[lnWare,1]
llWareHous  = llMultiWH
lcToWare    = ''

IF TYPE('lcFQualty') $ 'LU'
  lcKey      = lcFabric + lcColor
  *E301797,1 (Begin) Handle shipment.
  *lnAdjStk   = &lcTmpPo..nfabtotqty
  IF laRecType[lnRecType,2] = 'S'
    lnAdjStk   = &lcTmpPoSp..nfabtotqty
  ELSE
    lnAdjStk   = &lcTmpPo..nfabtotqty
  ENDIF
  *E301797,1 (End)
    
ELSE
  lcKey      =lcRetFab&lcFQualty+lcRetClr&lcFQualty
  lcFabric   =lcRetFab&lcFQualty
  lcColor    =lcRetClr&lcFQualty
  lnAdjStk   =lnDam&lcFQualty
  =SEEK(lcKey,'FABRIC')
ENDIF

lnOldAdjStk = lnAdjStk
lnAdjStk = IIF(lcKeyType  = 'P',lnAdjStk,lnAdjStk * -1) 

lcCostMeth = lcMtCstMth

*B606231,1 KHM 08/05/2002 (Begin) Initialize lnFabConv by Fabric.Conv to use it in lfJrRoData
lnFabConv = Fabric.Conv
*B606231,1 KHM 08/05/2002 (End)

DO lfJrRoData.PRG IN gfMatCrl.prg WITH .T.
*B603813,1 ABD Dont get in this rest of command in case you issue return .[Begin]
*IF lnOldAdjStk <> ABS(lnAdjStk)
*B603813,1 ABD [End]
IF lnOldAdjStk <> ABS(lnAdjStk) .AND. lcKeyType  # 'P'
  IF TYPE('lcFQualty') $ 'LU'
    lnSok = lnAdjStk
    SHOW GET lnSok
  ELSE
    lnDam&lcFQualty=lnAdjStk
    SHOW GET lnDam&lcFQualty
  ENDIF
  *-- Adjust the Qty
  *B603950,1 ABD Receiving material P.O error. [Begin]
  *lnQtyBuy = lnAdjStk
  lnQtyBuy  = IIF(lcKeyType = 'R',ABS(lnAdjStk),lnAdjStk)
  *B603950,1 ABD [End]

  *B603813,1 ABD Save the old value the old value that the filed is 
  *B603813,1     Not save the old value that miss calculate the Tot. Qty Filed .[Begin]
  *-- Save the old Qty if you issue return.
  lnOldQty = lnOldAdjStk
  *B603813,1 ABD [End]  
  *B603812,1 ABD [End]
  =lfvQty()
  *B603813,1 ABD Get the New Qty. [Begin]
ELSE
  lnQtyBuy  = ABS(lnAdjStk)
  
  *B606231,1 KHM 08/05/2002 (Begin) Multiply the buy quantity by fabric's conversion factor.
  *STORE lnQtyBuy TO lnQtyUse
  STORE lnQtyBuy * lnFabConv TO lnQtyUse
  *B606231,1 KHM 08/05/2002 (End)
  
  lnTotQty1 = lnTotQty1 - lnOldAdjStk + lnQtyBuy
  lnNewAls = SELECT (0)
  *E301797,1 (Begin) Get proper file.
  *SELECT (lcTmpPo) 
  IF laRecType[lnRecType,2] = 'S'
    SELECT (lcTmpPoSp) 
  ELSE
   SELECT (lcTmpPo) 
  ENDIF
  *E301797,1 (End)
  REPLACE nFabTotQty WITH lnQtyBuy
  SELECT (lnNewAls)
  *B603813,1 ABD [End]
ENDIF
lcTmpRolls = lcTmpRoll
SELECT(lnAlias)
*B603188,1 [End]
*!*************************************************************
*! Name      : lfvNewRoll
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : function to add new roll or to select an existing roll
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvNewRoll

IF lcRollNP = lcNewP
  lcRollid  = SPACE(20)
  lnRolBal  = 0
  llRModify = .F.
  lcRIdSta  = "ENABLE"
  lnOldBal  = 0
  DO (gcScrDir+gcWinAppl+'\MAPOREC8.SPX')
  IF lnUsrApply > 0
    SHOW GET pbRRemove ENABLE
    SHOW GET pbRModify ENABLE    
  ELSE    
    SHOW GET pbRRemove DISABLE
    SHOW GET pbRModify DISABLE
  ENDIF 
ELSE
  =lfvSelroll('S')
ENDIF  
*!*************************************************************
*! Name      : lfvRoll_ID
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the entered new Roll_Id
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvRoll_ID
PRIVATE lnRolTag,lcKey

lcKey = IIF(EMPTY(lcQuaFab),lcFabric+lcColor,lcQuaFab+lcQuaClr)
IF EMPTY(lcRollid)
  = gfModalGen('QRM36063B36000','ALERT')
  RETURN 0
ELSE
  *-- check if the roll ID is used before or not  
  SELECT (lcTmpRolls)
  IF SEEK(lcKey+lcWare+lcDyelot+lcRollid)
    = gfModalGen('QRM36064B36000','ALERT',ALLTRIM(lcRollid))
    lcRollid  = SPACE(20)
    SHOW GET lcRollid 
    _CUROBJ   = OBJNUM(lcRollId)
  ELSE
    SELECT Rolls
    lnRolTag = VAL(SYS(21))
    SET ORDER TO TAG ROLLS
    IF SEEK (lcRollid+lcKey+"1", "Rolls")
      = gfModalGen('QRM36064B36000','ALERT',ALLTRIM(lcRollid))
      lcRollid  = SPACE(20)
      SHOW GET lcRollid 
      _CUROBJ   = OBJNUM(lcRollId)
    ENDIF
    SET ORDER TO (lnRolTag) IN Rolls
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvBalance
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : validate the balance of the roll
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvBalance

*-- If the applied value is negative, return the old value of the field, 
*-- otherwise update the corresponding fields
IF lnRolBal = 0 
  = gfModalGen('QRM36060B36000','ALERT')
  lnRolBal = lnOldBal
  _CUROBJ  = OBJNUM(lnRolBal)
ENDIF

IF lnRolBal < 0
  = gfModalGen('QRM36061B36000','ALERT') 
  _CUROBJ = OBJNUM(lnRolBal)
ENDIF
*!*************************************************************
*! Name      : lfvOk
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : called form the ok button for the user to be sure that he want to add new roll
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvOk
PRIVATE lnAlias

lnAlias = SELECT()
SELECT(lcTmpRolls) 
lnUsrApply = lnUsrApply - lnOldBal+ lnRolBal 
IF !llRModify
  APPEND BLANK
ENDIF

*-- Fix the bug of saving rolls quantity by BuyQty amount (Start)
*-- it should be saved with the UsedQty
*-- AAMER 10/22/98
*REPLACE cRollId  WITH lcRollid,;
        nQty      WITH lnRolBal,;
        nQtyBal   WITH lnRolBal,;
        cRollItem WITH IIF(!EMPTY(lcQuaFab),lcQuaFab,lcFabric),;
        Color     WITH IIF(!EMPTY(lcQuaClr),lcQuaClr,lcColor ),; 
        Dyelot    WITH lcDyelot,;
        cWareCode WITH lcWare  ,;
        TRANCD    WITH '1' 
REPLACE cRollId   WITH lcRollid,;
        nQty      WITH lnRolBal * Fabric.Conv,;
        nQtyBal   WITH lnRolBal * Fabric.Conv,;
        cRollItem WITH IIF(!EMPTY(lcQuaFab),lcQuaFab,lcFabric),;
        Color     WITH IIF(!EMPTY(lcQuaClr),lcQuaClr,lcColor ),; 
        Dyelot    WITH lcDyelot,;
        cWareCode WITH lcWare  ,;
        TRANCD    WITH '1' 

*-- Fix the bug of saving rolls quantity by the use amount (End)        
CLEAR READ        
SELECT(lnAlias)
=lfRRefre()
SHOW WINDOW (lcRollTit) REFRESH SAME
*!*************************************************************
*! Name      : lfvClsRoll
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : called form the Close button of the roll screen
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvClsRoll
IF lnUsrApply <> lnTotApply
  IF gfModalGen('TRM36100B36001','ALERT' )=1
    DO CASE
      CASE &lcTmpPo..TRANCD = '2'
        lnTotqty1 =  lnTotqty1 - lnQtyBuy + lnUsrApply
      CASE &lcTmpPo..TRANCD = '4'
        lnTotqty2 =  lnTotqty2 - lnQtyBuy + lnUsrApply
      CASE &lcTmpPo..TRANCD = '3'
        lnTotqty3 =  lnTotqty3 - lnQtyBuy + lnUsrApply
    ENDCASE              
    lnQtyBuy = lnUsrApply
    CLEAR READ
    *E301797,1 (Begin) Get proper file.
    *SELECT (lcTmpPo) 
    IF laRecType[lnRecType,2] = 'S'
      SELECT (lcTmpPoSp) 
    ELSE
     SELECT (lcTmpPo) 
    ENDIF
    *E301797,1 (End)
    REPLACE nFabTotQty WITH lnQtyBuy
  ENDIF
ELSE
  CLEAR READ  
ENDIF

*!*************************************************************
*! Name      : lfwRoll
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : take the old quantity of the roll in a variable
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfwRoll

SHOW GET lnQtyBuy
=lfRefresh()
*E301797,1 (Begin) Show proper window.
*SHOW WINDOW (lcDet_Ttl) REFRESH SAME
IF laRecType[lnRecType,2] = 'S'
  *SHOW WINDOW (lcDetTtl) REFRESH SAME      
  SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY  
ELSE
  SHOW GETS WINDOW (lcWinCh9) DISABLE ONLY
  SHOW WINDOW (lcDet_Ttl) REFRESH SAME
ENDIF
*E301797,1 (End)

*!*************************************************************
*! Name      : lfRollQty
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : get the quantity of the existing rolls
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfRollQty
PRIVATE lnAlias,lnTotQty,lcSeekKey

lnAlias = SELECT()
lnTotQty = 0
lcSeekKey = IIF(!EMPTY(lcQuaFab),lcQuaFab+lcQuaClr,lcFabric+lcColor)+lcWare+lcDyelot
IF llRollBrow
  SELECT (lcTmpRolls)
  IF SEEK(lcSeekKey)
    SCAN WHILE cRollItem+Color+cWarecode+Dyelot = lcSeekKey
      lnTotQty = IIF(lcKeyType ='P',lnTotQty+nQty,IIF(!EMPTY(cSele),lnTotQty+nQtyBal,lnTotQty))
    ENDSCAN
  ENDIF
ELSE
  SELECT (lcTmpJour)
  IF SEEK(lcSeekKey)
    SCAN WHILE cFabric+cColor+cWarecode+cDyelot = lcSeekKey
      lnTotQty = lnTotQty+nApply
    ENDSCAN
  ENDIF  
ENDIF  
SELECT(lnAlias)
RETURN lnTotQty
*!*************************************************************
*! Name      : lfvRRemove
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : function to remove roll
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfvRRemove

IF lcKeyType = 'P'
  IF gfModalGen('INM36017B36001','DIALOG') = 1
    lnUsrApply = lnUsrApply - nQty
    DELETE
  ENDIF
  IF lnUsrApply <= 0
    SHOW GET pbRRemove DISABLE
    SHOW GET pbRModify DISABLE
  ENDIF
ELSE
  =lfvSelRoll('U')
ENDIF  
=lfRRefre()
SHOW WINDOW (lcRollTit) REFRESH SAME
*!*************************************************************
*! Name      : lfvRModify
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : function to modify roll
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfvRModify

lcRollid  = cRollId
lnRolBal  = nQty
llRModify = .T.
lcRIdSta  = "DISABLE"
SELECT(lcTmpRolls)
lnOldBal  = nQtyBal
DO (gcScrDir+gcWinAppl+'\MAPOREC8.SPX')

*!*************************************************************
*! Name      : lfwWare
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : get the old warehouse in a variable
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfwWare

lcOldWare = lcWare
*!*************************************************************
*! Name      : lfwDyelot
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : get the old dyelot in a variable
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfwDyelot

lcOldDye = lcDyelot 
*!*************************************************************
*! Name      : lfOldrolls
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : get the existing rolls from the roll file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfOldrolls
PRIVATE llRecExi,lcKey

llRecExi = .F.
lcKey    = IIF(EMPTY(lcQuaFab),lcFabric+lcColor,lcQuaFab+lcQuaClr)
IF SEEK(lcKey+lcWare+lcDyelot,'ROLLS')
  SELECT ROLLS
  SCAN WHILE ROLLS.cRollItem+ROLLS.Color+ROLLS.cWareCode+ROLLS.Dyelot = ;
                          lcKey+lcware+lcdyelot
    IF ROLLS.nQtyBal > 0 AND ROLLS.TRANCD  = '1'
      SCATTER MEMVAR
      SELECT(lcTmpRolls)
      APPEND BLANK
      GATHER MEMVAR
      llRecExi = .T.
    ENDIF
  ENDSCAN
ELSE                          
  llRecExi = .F.
ENDIF
RETURN llRecExi
*!*************************************************************
*! Name      : lfUpdRolls
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : function to update the roll file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfUpdRolls
PRIVATE lnAlias,lcKey

lnAlias = SELECT()
lcKey   = IIF(EMPTY(&lcTmpPo..cFabric1),&lcTmpPo..Fabric+&lcTmpPo..Color,&lcTmpPo..cFabric1+&lcTmpPo..cColor1)
SELECT (lcTmpRolls)
=SEEK(lcKey +&lcTmpPo..cWarecode+&lcTmpPo..Dyelot)
IF lcKeyType = 'P'  AND &lcTmpPo..Trancd <> '4'
  SCAN WHILE cRollItem+Color+cWarecode+Dyelot = ;
             lcKey+&lcTmpPo..cWarecode+&lcTmpPo..Dyelot
    SCATTER MEMVAR 
    m.cRsession = lcRecSess
    m.cSession  = lcRecSess
    SELECT ROLLS
    APPEND BLANK
    GATHER MEMVAR
  ENDSCAN
ELSE
  SET ORDER TO TAG RollItem IN ROLLS  
  SCAN WHILE cRollItem+Color+cWarecode+Dyelot = ;
             lcKey+&lcTmpPo..cWarecode+&lcTmpPo..Dyelot
    IF !EMPTY(cSele)         
      =SEEK(cRollItem+Color+cWarecode+Dyelot+cRollId,'ROLLS')
      REPLACE ROLLS.nQtyBal   WITH 0         ,;
              ROLLS.cIsession WITH lcRecSess ,;
              ROLLS.cSession  WITH lcRecSess
    ENDIF          
  ENDSCAN
ENDIF
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvSelRoll
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : function to select or unselect roll
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfvSelRoll
PARAMETERS lcAll
PRIVATE llUnSele, llSele,lnNewRec

llSele   = .F.
llUnSele = .F.

=lfRRefre()
SHOW WINDOW (lcRollTit) REFRESH SAME

*-- This function maybe called from the select button or unselect 
llChecked  = !EMPTY(cSele)
*-- If select all and the current record is selected do nothing
IF (lcAll = 'S' .AND. llChecked) .OR. (lcAll = 'U' .AND. !llChecked)
  RETURN
ENDIF
llChecked  = IIF(lcAll = 'S',.F.,.T.)
lnFactor   = IIF(llChecked,-1,1)
lnUsrApply = lnUsrApply + (nQtyBal*lnFactor)

REPLACE cSele WITH IIF(llChecked," ","�")
= lfRRefre()

lnNewRec = RECNO()
SCAN
  IF !EMPTY(cSele)
    llUnSele = .T.
    EXIT
  ENDIF
ENDSCAN
 
SCAN
  IF EMPTY(cSele)
    llSele = .T.
    EXIT
  ENDIF
ENDSCAN
GOTO lnNewRec
lcUnSele =  IIF(llUnSele,"ENABLE","DISABLE")
lcSele   =  IIF(llSele,  "ENABLE","DISABLE")
SHOW GET pbRRemove &lcUnSele
SHOW GET pbRNew    &lcSele
SHOW WINDOW (lcRollTit) REFRESH SAME
*!*************************************************************
*! Name      : lfwBRoll
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : when function for the roll browse
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfwBRoll
PRIVATE lnAlias,lnRRec

lnAlias = SELECT()
SELECT(lcTmpRolls)
IF !EOF()
  lnRRec = RECNO()
  REPLACE ALL cMarker WITH lcUnMark
  GO lnRRec
  REPLACE cMarker WITH lcMark
ENDIF  
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfButStatus
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : function to show the status for the buttons of the roll screen
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfButStatus
PARAMETER lcButton
PRIVATE lcButton,llStatus,lnAlias,llSeekKey

lnAlias = SELECT()
llStatus = .F.
llSeekKey = IIF(EMPTY(lcQuaFab),lcFabric+lcColor,lcQuaFab+lcQuaClr)+lcWare+lcDyelot
DO CASE 
  CASE lcButton = 'M'
    IF SEEK(llSeekKey,lcTmpRolls)
      llStatus = .T.
    ENDIF
  CASE lcButton = 'U'
    IF SEEK(llSeekKey,lcTmpRolls)
      SELECT(lcTmpRolls)
      SCAN WHILE cRollItem+Color+cWareCode+Dyelot = llSeekKey
        IF !EMPTY(cSele)  
          llStatus = .T.
          EXIT
        ENDIF
      ENDSCAN
    ENDIF
  CASE lcButton = 'N'
    IF SEEK(llSeekKey,lcTmpRolls)
      SELECT(lcTmpRolls)
      SCAN WHILE cRollItem+Color+cWareCode+Dyelot = llSeekKey
        IF EMPTY(cSele)  
          llStatus = .T.
          EXIT
        ENDIF
      ENDSCAN
    ENDIF
ENDCASE
SELECT(lnAlias)
RETURN llStatus
*!*************************************************************
*! Name      : lfvQuaFab
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : to validate the subs. fabric
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfvQuaFab
PRIVATE lnAlias
IF MDOWN()
  RETURN
ENDIF

lnAlias = SELECT()
SELECT Fabric
*B603290,1 (Begin) Restricted to Purchasded fabrics only.
*SET FILTER TO cFabGrade = lcFabGrade
SET FILTER TO cFabGrade = lcFabGrade AND !MAKE
*B603290,1 (End)
IF !EMPTY(lcQuaFab) AND !SEEK(lcQuaFab)
    DO FaBrow WITH lcQuaFab,'*',.T.
ENDIF
SET FILTER TO
*E301797,1 (Begin) Show proper obgect.
*REPLACE &lcTmpPo..cFabric1 WITH lcQuaFab
IF laRecType[lnRecType,2] = 'S'
  REPLACE &lcTmpPoSp..cFabric1 WITH lcQuaFab
ELSE
  REPLACE &lcTmpPo..cFabric1 WITH lcQuaFab
ENDIF
*E301797,1 (End)
SHOW GET lcQuaFab ENABLE
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvKQuaFab
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : to validate the subs. fabric from the browse button
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfvKQuaFab

lcQuaFab = PADR('?',7)
=lfvQuaFab()
*!*************************************************************
*! Name      : lfvQuaClr
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : to validate the subs. color 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfvQuaClr
PRIVATE lcOldF,lnAlias

lnAlias = SELECT()
SELECT FABRIC
SET FILTER TO cFabGrade = lcFabGrade
IF !EMPTY(lcQuaClr) AND !SEEK(lcQuaFab+lcQuaClr,'FABRIC') 
  lcQuaClr = IIF(EMPTY(lcQuaFab),PADR(lcQuaClr,6),CHR(240))
  lcOldF   = lcQuaFab
  DO FaBrow WITH lcQuaFab,lcQuaClr,.T.
  IF EMPTY(lcQuaClr)
    lcQuaFab = lcOldF
  ENDIF
ENDIF
IF EMPTY(lcQuaFab) OR EMPTY(lcQuaClr)
    =gfModalGen('INM36013B36000','DIALOG')
    _CUROBJ = IIF(EMPTY(lcQuaFab),OBJNUM(lcQuaFab),OBJNUM(lcQuaClr))    
ELSE
  IF !SEEK(lcQuaFab+lcQuaClr+lcWare+SPACE(10),'FABDYE')
    *E301797,1 (Begin) Pass proper file.
    *DO gpAdFabWar WITH lcQuaFab,lcQuaClr,SPACE(10),&lcTmpPo..cWareCode
    DO gpAdFabWar WITH lcQuaFab,lcQuaClr,SPACE(10),IIF(laRecType[lnRecType,2] = 'S',&lcTmpPoSp..cWareCode,&lcTmpPo..cWareCode)
    *E301797,1 (End)
  ENDIF
  *E301797,1 (Begin) Show proper obgect.
  *REPLACE &lcTmpPo..cFabric1 WITH lcQuaFab,;
          &lcTmpPo..cColor1  WITH lcQuaClr
  IF laRecType[lnRecType,2] = 'S'
    REPLACE &lcTmpPoSp..cFabric1 WITH lcQuaFab,;
            &lcTmpPoSp..cColor1  WITH lcQuaClr
  ELSE
    REPLACE &lcTmpPo..cFabric1 WITH lcQuaFab,;
            &lcTmpPo..cColor1  WITH lcQuaClr
  ENDIF
  *E301797,1 (End)
ENDIF
SET FILTER TO
=lfwLine()
SELECT(lnAlias)
*!*************************************************************
FUNCTION lfvKQuaClr

lcQuaClr = PADR('?',6)
=lfvQuaClr()
*!*************************************************************

FUNCTION lfOldLots
PRIVATE llDone,lnAlias,lnLotQty,lnActQty,lcKey

lnAlias = SELECT()
llDone = .T.
lcKey = IIF(!EMPTY(lcQuaFab) AND !EMPTY(lcQuaClr),lcQuaFab+lcQuaClr,lcFabric+lcColor)
lcExp = lcKey+lcWare+lcDyelot

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
  lnLotQty = &lcTmpPo..nFabTotQty
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
    ENDIF
  ENDSCAN  
ENDIF
SELECT(lnAlias)
RETURN .T.
*!*************************************************************
*! Name      : lfTrapKeys
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : trapping the keys in the main screen
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfTrapKeys

IF WONTOP() = lcDet_Ttl
  glFromBrow = .T.
ENDIF
ON KEY LABEL TAB        DO lpTab
ON KEY LABEL BACKTAB    DO lpShiftTab
ON KEY LABEL ESC        DO gfCPClose
*!*************************************************************
*! Name      :  lpShiftTab
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : trapping the ShiftTab in the main screen
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
PROCEDURE lpShiftTab

DO CASE
  CASE WONTOP(lcDet_Ttl)
    IF laScrMode[1]
      ACTIVATE WINDOW (lcWinCh3)
    ELSE
      ACTIVATE WINDOW gwcContrl1
      _CUROBJ = OBJNUM(pbCls)
    ENDIF  
ENDCASE    
*!*************************************************************
*! Name      : lpTab
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : trapping the Tab key in the main screen
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
PROCEDURE lpTab

*E301797,1 (Begin) Show proper window.
IF laRecType[lnRecType,2] = 'S'
  DO CASE
    CASE WONTOP()  = lcDet_Ttl
      ACTIVATE WINDOW (lcWinCh1)
    CASE !WONTOP(lcDet_Ttl)
      =lfBrLines()
  ENDCASE    
  RETURN
ENDIF
*E301797,1 (End)
DO CASE
  CASE !WONTOP(lcDet_Ttl)
    ACTIVATE WINDOW (lcDet_Ttl)
  CASE WONTOP(lcDet_Ttl)
    ACTIVATE WINDOW (lcWinCh1)
ENDCASE    
*!*************************************************************
*! Name      : lfClearTrap
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : to clear the trap when the browse is not active
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfClearTrap

IF glFromBrow
  =gfStopBrow()
ENDIF
IF WONTOP() <> lcDet_Ttl 
   ON KEY LABEL ESC &lcOldEscTrap
   ON KEY LABEL TAB
   ON KEY LABEL BACKTAB
ENDIF   
*!*************************************************************
*! Name      : lfTrapRoll
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : trapping keys in the roll screen
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
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
*! Developer : Samah Wilson Kirollos
*! Date      : 07/18/96
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose   : Do the trapping of Tab key between the 
*!             2 screens Mat400_6 and Mat400_8
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Called from : 
*!         Procedure : lfTrapRoll
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Calls : 
*!         Procedures : lpTabR
*!*************************************************************
PROCEDURE lpTabR

ON KEY LABEL TAB lnD = 1
DO CASE
  CASE WONTOP('lcWinch7') AND _CUROBJ = OBJNUM(pbRClose)
    IF llRollBrow
      ACTIVATE WINDOW (lcRollTit)
    ELSE
      ACTIVATE WINDOW (lcLotlTit)
    ENDIF  
  CASE WONTOP(lcRollTit) OR WONTOP(lcLotlTit)
    ACTIVATE WINDOW ('lcWinch7')
  OTHERWISE
    _CUROBJ = _CUROBJ + 1
ENDCASE
ON KEY LABEL TAB DO lpTabR

*!*************************************************************
*! Name      : lpSTabR
*! Developer : Samah Wilson Kirollos
*! Date      : 07/18/96
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose   : Do the trapping of Shift + Tab key between the 
*!             2 screens Mat400_6 and Mat400_8
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Called from : 
*!         Procedure : lfTrapRoll
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Calls : 
*!         Procedures : lpSTabR
*!*************************************************************
PROCEDURE lpSTabR

DO CASE
  CASE WONTOP(lcRollTit) OR WONTOP(lcLotlTit)
    ACTIVATE WINDOW ('lcWinch7')
    _CUROBJ = OBJNUM(pbRClose)
  CASE WONTOP('lcWinch7') AND _CUROBJ = OBJNUM(pbRNew) 
    IF llRollBrow
      ACTIVATE WINDOW (lcRollTit)
    ELSE
      ACTIVATE WINDOW (lcLotlTit)
    ENDIF  
  OTHERWISE
    _CUROBJ =  _CUROBJ - 1  
ENDCASE
ON KEY LABEL BACKTAB DO lpSTabR
*!*************************************************************
*! Name      : lfvLocation
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : called form the location button
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfvLocation
PRIVATE lcKey

DIMENSION laSource[1], laTarget[1]
STORE ' ' TO laSource, laTarget
*-- Select the location assigned for the warehouse in the source array.
SELECT cLocation FROM WHSLOC ;
 WHERE CWARECODE == lcWare AND EMPTY(STYLE+COLOR);
  INTO ARRAY laSource

IF _TALLY = 0
  *-- If there is no location in the ware house.
  =gfModalGen('QRM36050B36000','ALERT', ALLTRIM(lcWare))
ELSE
lcKey = IIF(!EMPTY(lcQuaFab) AND !EMPTY(lcQuaClr),PADR(&lcTmpPo..cFabric1,19)+&lcTmpPo..cColor1,PADR(&lcTmpPo..Fabric,19)+&lcTmpPo..Color)
  *-- Reinitialize the target array with the previously selected locations.
  SELECT cLocation FROM (lcTemLoc) ;
   WHERE STYLE+COLOR+CWARECODE+CLOCATION=;
         lcKey +lcWare;
    INTO ARRAY laTarget
  
  *-- Display the mover.
  =gfMover(@laSource,@laTarget," Assign Locations",.T.,'lfvLoc')
  
  *-- Update the temp file with the new selected locations.
  SELECT (lcTemLoc)
  DELETE FOR STYLE+COLOR+CWARECODE+CLOCATION = ;
             lcKey +lcWare
  FOR lnLoc = 1 TO ALEN(laTarget)
    IF !EMPTY(laTarget[lnLoc])
      APPEND BLANK
      REPLACE STYLE     WITH IIF(!EMPTY(lcQuaFab) AND !EMPTY(lcQuaClr),lcQuaFab,lcFabric);
              COLOR     WITH IIF(!EMPTY(lcQuaFab) AND !EMPTY(lcQuaClr),lcQuaClr,lCcolor);
              CWARECODE WITH lcWare;
              CLOCATION WITH laTarget[lnLoc]
    ENDIF             
  ENDFOR
ENDIF  
*!*************************************************************
*! Name      : lfvLoc
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : called form the gfmover (valid function)
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfvLoc
PARAMETERS lnOption
PRIVATE lnArray,lcKey 

lcKey = IIF(!EMPTY(lcQuaFab) AND !EMPTY(lcQuaClr),PADR(lcQuaFab,19)+lcQuaClr,PADR(lcFabric,19)+lcColor)
IF lnOption=3 OR lnOption=4
  RETURN .T.
ELSE
  IF lnOption=2
    FOR lnArray = 1 TO ALEN(laSource)
      IF !SEEK(lcKey+lcWare+laSource[lnArray],"WhsLoc")
        IF gfModalGen('QRM36051B36004','ALERT',ALLTRIM(laSource[lnArray])+'|'+ALLTRIM(SUBSTR(lcKey,1,19))+'/'+ALLTRIM(SUBSTR(lcKey,20,25))+'|'+ALLTRIM(lcWare)) = 2
          RETURN .F.
        ENDIF
      ENDIF    
    ENDFOR           
  ELSE  
    RETURN  SEEK(lcKey+lcWare+laSource[lsSource],"WhsLoc") ;
            OR ;
            gfModalGen('QRM36051B36004','ALERT',ALLTRIM(laSource[lsSource])+'|'+ALLTRIM(lcFabric)+'/'+ALLTRIM(LCcolor)+'|'+ALLTRIM(lcWare)) = 1
  ENDIF  
ENDIF
*!*************************************************************
*! Name      : lfUpdlOC
*! Developer : Samah Wilson Kirollos
*! Date      : 10/26/1997
*! Purpose   : update the whsloc file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!***************************************************
FUNCTION lfUpdlOC
PRIVATE lcKey,lnAlias

lnAlias = SELECT()
lcKey   = IIF(EMPTY(&lcTmpPo..cFabric1),PADR(&lcTmpPo..Fabric,19)+&lcTmpPo..Color,PADR(&lcTmpPo..cFabric1,19)+&lcTmpPo..cColor1)
SELECT(lcTemLoc)
IF SEEK(lcKey+&lcTmpPo..cWareCode)
  SCAN WHILE STYLE+COLOR+CWARECODE = lcKey+&lcTmpPo..cWareCode
    IF !SEEK(&lcTemLoc..STYLE+&lcTemLoc..COLOR+&lcTemLoc..CWARECODE ;
            +&lcTemLoc..CLOCATION,'WhsLoc')
      SCATTER MEMVAR
      SELECT WhsLoc
      APPEND BLANK
      GATHER MEMVAR      
    ENDIF        
  ENDSCAN
ENDIF
SELECT(lnAlias)
*!*************************************************************
FUNCTION lfBrowLot
PRIVATE lnAlias
lnAlias = SELECT()
SELECT(lcTmpJour)
lcLotlTit   = 'Available Lots' 
lcRlBrFild  = "cMarker :H='':R:W=lfChMarker(),"+;
              "Ctran:H='Tran.':R:W=.F.,cRsession:H='R.Sess.':R:W=.F.,"+;
              "dTrandate:H='Date':R:W=.F.,nunitcost:H='Unit Cost':R:W=.F.,"+;
              "nReceived:H='Received':R:W=.F.,nIssued:H='Issued':R:W=.F.,"+;
              "nApply:H='Apply':W=IIF(lcMtCstMth $ 'FI',.F.,lfwOldVals()):V=lfVAldAAp(),"+;
              "nBalance:H='Balance':R:W=.F."                  

BROWSE FIELDS &lcRlBrFild;
       LOCK 0   ;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwBRoll() ;
       TITLE lcLotlTit ;
       WINDOW lcWinCh5 IN WINDOW lcWinCh4 ;
       FOR &lcTmpJour..cFabric+&lcTmpJour..cColor+&lcTmpJour..cWareCode+&lcTmpJour..cDyelot=;
           IIF(!EMPTY(lcQuaFab),lcQuaFab+lcQuaClr,lcFabric+lcColor)+lcWare+lcDyelot;    

SELECT(lnAlias)

*!*************************************************************
PROCEDURE lpEscR

ACTIVATE WINDOW lcWinch7
_CUROBJ = OBJNUM(pbRClose)
KEYBOARD CHR(13)
*!*************************************************************
FUNCTION lfChMarker
PRIVATE lnRecNo

IF !EOF()
  lnRecNo = RECNO()
  REPLACE ALL cMarker WITH lcUnMark
  GOTO lnRecNo
  REPLACE cMarker WITH lcMark
ENDIF  
*!*************************************************************
FUNCTION lfwOldVals

lnOldVal  = EVALUATE(SYS(18))
*!*************************************************************
FUNCTION lfVAldAAp
PRIVATE llRetn,lnTmpUsrAp,lnTmpBalnc,lnMax 

llRetn     = .F.
lnTmpUsrAp = lnUsrApply - lnOldVal + nApply
lnTmpBalnc = nBalance   - nApply   + lnOldVal

*-- Check the range of the applied quantity
IF BETWEEN(nApply, 0, nReceived-nIssued) AND (lnTmpUsrAp + ABS(lnTotApply) >= 0)
  llRetn = .T.
  lnUsrApply = lnTmpUsrAp
  REPLACE nBalance  WITH lnTmpBalnc
ENDIF
IF !llRetn 
  lnMax = IIF ( (nReceived-nIssued) > lnUsrApply + ABS(lnTotApply), ;
                lnUsrApply + ABS(lnTotApply), nBalance)
  REPLACE nApply WITH 0
  WAIT IIF (ABS(lnTotApply) = lnUsrApply                 ,;
            'The entry quantity is applied completely.'  ,;
            'Range 0 .. ' + ALLTRIM(STR(lnMax))) WINDOW
ENDIF
=lfRRefre()
RETURN (llRetn)
*!*************************************************************
FUNCTION lfGetTran
PARAMETER lcField, lcExpr

RETURN LOOKUP(MATINVJL.&lcField,lcExpr,MATINVJL.cfabric,'MATINVJL')
*!*************************************************************
FUNCTION lfvRef

*E301797,1 (Begin) Show proper window.
*REPLACE &lcTmpPo..Reference WITH lcReference
*SHOW WINDOW (lcDet_Ttl) REFRESH SAME
IF laRecType[lnRecType,2] = 'S'
  REPLACE &lcTmpPoSp..Reference WITH lcReference
  SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY  
ELSE
  REPLACE &lcTmpPo..Reference WITH lcReference
  SHOW WINDOW (lcDet_Ttl) REFRESH SAME
  SHOW GETS WINDOW (lcWinCh9) DISABLE ONLY
ENDIF
*E301797,1 (End)

*!*************************************************************
FUNCTION lfvPattern

REPLACE &lcTmpPo..Pattern WITH lcPattern
*E301797,1 (Begin) Show proper window.
*SHOW WINDOW (lcDet_Ttl) REFRESH SAME
IF laRecType[lnRecType,2] = 'S'
  *SHOW WINDOW (lcDetTtl) REFRESH SAME      
  SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY  
ELSE
  SHOW GETS WINDOW (lcWinCh9) DISABLE ONLY
  SHOW WINDOW (lcDet_Ttl) REFRESH SAME
ENDIF
*E301797,1 (End)

*!*************************************************************
FUNCTION lfvPostDate

IF LASTKEY() = 13
  IF !CheckPrd(ldPost,'lcGlFYear','lcGlPeriod',;
                  IIF(lcType='P','MP','PO')) OR EMPTY(ldPost)
    ldPost  = gdSysDate
    _CUROBJ = OBJNUM(ldPost)
  ENDIF  
  SELECT UNCMSESS
  *B606625,1 NAD (Start)
  *REPLACE mComent WITH lfVarStr(@laVariables)
  IF SEEK('O'+lcUnsmsPrg+gcUser_id+lcSession)
    REPLACE mComent WITH lfVarStr(@laVariables)
  ENDIF 
  *B606625,1 NAD (END)
ENDIF  
*!*************************************************************

*B601927,1 Hesham El-Sheltawi (Start)  
*B601927,1 function to create all the uncompete session temprory files
*B601927,1 that is needed by the program

FUNCTION lfCrtUnComp

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

*B604051,1 AAN Add three other fields to tmp file "lctmpjour" [Start].
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
*B604051,1 AAN Add three other fields to tmp file "lctmpjour" [End].

=gfCrtTmp(lcTmpJour,@laFileStru,[cFabric+cColor+cWareCode+cDyelot+cRsession+cIsession],lcTmpJour)
  
IF llGlLink
  SELECT GLDIST
  = AFields(laFileStru)
  =gfCrtTmp(lcGlDTemp,@laFileStru)  
ENDIF
IF llWareLoc 
  DIMENSION laSource[1],laTarget[1]
  SELECT WhsLoc
  SET ORDER TO TAG Whslocst
  = AFIELDS(laStru)
  =gfCrtTmp(lcTemLoc,@laStru,[STYLE+COLOR+CWARECODE+CLOCATION],lcTemLoc)    
ENDIF  
*B603188,1 HDM [Start] Don't Create the temp Rolls File as it's created by gfMatCrl
*IF llTrkRolls
*  SELECT ROLLS
*  SET ORDER TO TAG ROLLITEM
*  = AFields(laFileStru)
*  lnNewFld = ALEN(laFileStru,1)+1
*  DIMENSION laFileStru[lnNewFld,4]
*  laFileStru[lnNewFld,1] = 'cMarker'
*  laFileStru[lnNewFld,2] = 'C'
*  laFileStru[lnNewFld,3] = 1
*  laFileStru[lnNewFld,4] = 0

*  lnNewFld = ALEN(laFileStru,1)+1
*  DIMENSION laFileStru[lnNewFld,4]
*  laFileStru[lnNewFld,1] = 'cSele'
*  laFileStru[lnNewFld,2] = 'C'
*  laFileStru[lnNewFld,3] = 1
*  laFileStru[lnNewFld,4] = 0
*  =gfCrtTmp(lcTmpRolls,@laFileStru,[cRollItem+Color+cWarecode+Dyelot+cRollid],lcTmpRolls)    
*ENDIF
*B603188,1 HDM [End]

SELECT POFLN
SET ORDER TO TAG POFLN
= AFields(laFileStru)
lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'cMarker'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'UnitP'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 4
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'UnitD'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 4
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'SignP'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'SignD'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'PoGrade'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'CanQty'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 11
laFileStru[lnNewFld,4] = 3

*B604580,1 HBG 07/08/2001 Add field to save orginal Warehouse of this PO [Begin]
lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'cOrgWare'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 6
laFileStru[lnNewFld,4] = 0
*B604580,1 [END]

*C102525,1 RAE [BEGIN] Create new field in temp file to hold PO #.
IF ASCAN(laEvntTrig , PADR('CRTEMPFILE',10)) <> 0
  =gfDoTriger('MARTMAT',PADR('CRTEMPFILE',10))
ENDIF
*C102525,1 RAE [END]

=gfCrtTmp(lcTmpPo,@laFileStru,[cMatType+POMAT+FABRIC+COLOR+cWareCode+Trancd+DYELOT+cFabGrade],lcTmpPo)    
*B601927,1 Hesham El-Sheltawi (End)  

*E301797,1 (Begin) Create shipment temp file.
SELECT matshpdt
SET ORDER TO TAG matshpdt
= AFields(laFileStru)
lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'cMarker'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'UnitP'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 4
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'UnitD'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 4
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'SignP'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'SignD'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'PoGrade'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'CanQty'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 11
laFileStru[lnNewFld,4] = 3

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'cOrgWare'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 6
laFileStru[lnNewFld,4] = 0
= gfCrtTmp(lcTmpPoSp,@laFileStru,[cMatType+POMAT+FABRIC+COLOR+cWareCode+Trancd+DYELOT+cFabGrade],lcTmpPoSp)
*E301797,1 (End)

******************************************************************
FUNCTION lfvBrows

*E301797,1 (Begin) Care window.
*IF WONTOP() # (lcDet_Ttl)
IF WONTOP() # IIF(laRecType[lnRecType,2] = 'S',(lcDetTtl),(lcDet_Ttl))
*E301797,1 (End)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************************

FUNCTION lfFilsUpd
PRIVATE lnalias,lcSeekKey,lcLinkCode,lcLinkPO,lnNewCost1,lnDuCost,lnNewCost2,;
        lnNewCost,lnUseCost,lnUseQty,lcCurTag,lnRet
*E301797,1 (Begin) Handle shipment.
IF laRecType[lnRecType,2] = 'S'
  lnRet = 0
  lnalias = SELECT(0)
  lcSeekKey = IIF(EMPTY(&lcTmpPoSp..cFabric1),&lcTmpPoSp..Fabric+&lcTmpPoSp..Color,&lcTmpPoSp..cFabric1+&lcTmpPoSp..cColor1)
  =SEEK(lcSeekKey,'Fabric')
  SELECT FABDYE
  IF llDyelot .AND. Fabric.cDye_Flg= 'Y'
    SELECT FabDye
    *-- Check if the fabric does not have a record with the entered 
    *-- dyelot in the FabDye file, it will be added
    IF !SEEK (lcSeekKey+&lcTmpPoSp..cWareCode+&lcTmpPoSp..Dyelot)
       DO gpAdFabWar WITH IIF(EMPTY(&lcTmpPoSp..cFabric1),&lcTmpPoSp..Fabric,&lcTmpPoSp..cFabric1),;
                          IIF(EMPTY(&lcTmpPoSp..cColor1),&lcTmpPoSp..Color,&lcTmpPoSp..cColor1),;
                          &lcTmpPoSp..Dyelot, &lcTmpPoSp..cWareCode,lcTmpPoSp
    ENDIF
  ENDIF
  IF SEEK (lcSeekKey+&lcTmpPoSp..cWareCode+SPACE(10))
    IF (&lcTmpPoSp..TRANCD  <> '4')
      *-- Use fabric link_code if the warehouse has no link code
      lcLinkCode = IIF(EMPTY(FABDYE.Gl_Link),Fabric.Link_Code,FABDYE.Gl_Link)
      lcLinkPO   = POFHDR.LINK_CODE
      lnNewCost1 = IIF(&lcTmpPoSp..UnitP = 0,0,IIF(&lcTmpPoSp..SignP = '/',&lcTmpPoSp..NLAN_COST1/&lcTmpPoSp..nlanPrRat,&lcTmpPoSp..NLAN_COST1*&lcTmpPoSp..nLanPrRat)/&lcTmpPoSp..UnitP)
      lnDuCost   = IIF(&lcTmpPoSp..UnitD = 0,0,(&lcTmpPoSp..NLAN_COST2+&lcTmpPoSp..NLAN_COST3+&lcTmpPoSp..NLAN_COST4)/&lcTmpPoSp..UnitD)
      lnNewCost2 = IIF(&lcTmpPoSp..SignD = '/',IIF(&lcTmpPoSp..nlanDuRat = 0,0,lnDuCost/&lcTmpPoSp..nlanDuRat),lnDuCost*&lcTmpPoSp..nLanDuRat)
      lnNewCost  = lnNewCost1 + lnNewCost2
      lnUseCost  = lnNewCost/Fabric.Conv
      lnUseQty   = IIF(lcScrType='R',-1,1) * &lcTmpPoSp..nFabTotQty*Fabric.Conv
      IF llGlLink
        DECLARE laGLDistAr[2,13]
        laGLDistAr[1,1] = lcLinkCode
        laGLDistAr[2,1] = lcLinkPO
        laGLDistAr[1,2] = '015'
        laGLDistAr[2,2] = '013'
        laGLDistAr[1,3] = 1
        laGLDistAr[2,3] = -1
        STORE 'MO'            TO laGLDistAr[1,4],laGLDistAr[2,4]
        STORE &lcTmpPoSp..POMAT TO laGLDistAr[1,5],laGLDistAr[2,5]
        STORE ldPost          TO laGLDistAr[1,6],laGLDistAr[2,6]
        STORE lcGlFYear       TO laGLDistAr[1,7],laGLDistAr[2,7]
        STORE lcGlPeriod      TO laGLDistAr[1,8],laGLDistAr[2,8]
        STORE lcGlDTemp       TO laGLDistAr[1,9],laGLDistAr[2,9]
        laGLDistAr[2,10] = ''
      ELSE
        DIME laGLDistAr[1,1]
        laGLDistAr = ''
      ENDIF
      PRIVATE laOtherPar
      DIMENSION laOtherPar[4,2]
      laOtherPar[1,1] = 'lnLineNo'
      laOtherPar[1,2] = &lcTmpPoSp..LINENO
      laOtherPar[2,1] = 'lcProg'
      laOtherPar[2,2] = 'MARECI'
      laOtherPar[3,1] = 'llLastRec'
      laOtherPar[3,2] = (lnRecCount = lnCurrRec)
      IF &lcTmpPoSp..cOrgWare <> &lcTmpPoSp..cWareCode
        laOtherPar[4,1] = 'lcPrvWare'
        laOtherPar[4,2] = &lcTmpPoSp..cOrgWare
      ELSE
        laOtherPar[4,1] = 'lcPrvWare'
        laOtherPar[4,2] = ""
      ENDIF  
      *khalid
      *lnRet = gfMatCrl('1',lcFabric,lcColor,;
                       &lcTmpPoSp..cWareCode,&lcTmpPoSp..Dyelot,;
                       &lcTmpPoSp..DATE,ldPost,&lcTmpPoSp..POMAT,;
                       lnUseQty,lnUseCost,;
                       &lcTmpPoSp..Reference,'',0,'','',@laGLDistAr,lcRecSess,'','','','','','','',@laOtherPar)
      lnRet = gfMatCrl('1',lcFabric,lcColor,;
                       &lcTmpPoSp..cWareCode,&lcTmpPoSp..Dyelot,;
                       ldDate,ldPost,&lcTmpPoSp..POMAT,;
                       lnUseQty,lnUseCost,;
                       &lcTmpPoSp..Reference,'',0,'','',@laGLDistAr,lcRecSess,'','','','','','','',@laOtherPar)

    ELSE
      lnUseQty   = IIF(lcScrType='R',-1,1) * &lcTmpPoSp..nFabTotQty*Fabric.Conv  
      SELECT FABRIC
      REPLACE OnORder WITH MAX(OnORder - IIF(lnUseQty > OnORder,OnOrder,lnUseQty),0)
      SELECT FABDYE
      REPLACE OnORder WITH MAX(OnORder - IIF(lnUseQty > OnORder,OnOrder,lnUseQty),0)
      lnRet = 1
    ENDIF
  ENDIF
  RETURN lnRet
ENDIF
*E301797,1 (End)

*B603233,1 HDM [Start] Declare lnRet and default it to 0
lnRet = 0
*B603233,1 HDM [End]
lnalias = SELECT(0)
lcSeekKey = IIF(EMPTY(&lcTmpPo..cFabric1),&lcTmpPo..Fabric+&lcTmpPo..Color,&lcTmpPo..cFabric1+&lcTmpPo..cColor1)
=SEEK(lcSeekKey,'Fabric')
SELECT FABDYE

IF llDyelot .AND. Fabric.cDye_Flg= 'Y'
  SELECT FabDye
  *-- Check if the fabric does not have a record with the entered 
  *-- dyelot in the FabDye file, it will be added
  IF !SEEK (lcSeekKey+&lcTmpPo..cWareCode+&lcTmpPo..Dyelot)
   DO gpAdFabWar WITH IIF(EMPTY(&lcTmpPo..cFabric1),&lcTmpPo..Fabric,&lcTmpPo..cFabric1),;
                      IIF(EMPTY(&lcTmpPo..cColor1),&lcTmpPo..Color,&lcTmpPo..cColor1),;
                      &lcTmpPo..Dyelot, &lcTmpPo..cWareCode,lcTmpPo
  ENDIF
ENDIF
IF SEEK (lcSeekKey+&lcTmpPo..cWareCode+SPACE(10))
  IF (&lcTmpPO..TRANCD  <> '4')
    *-- Use fabric link_code if the warehouse has no link code
    lcLinkCode = IIF(EMPTY(FABDYE.Gl_Link),Fabric.Link_Code,FABDYE.Gl_Link)
    lcLinkPO   = POFHDR.LINK_CODE
    lnNewCost1 = IIF(&lcTmpPo..SignP = '/',&lcTmpPo..NLAN_COST1/&lcTmpPo..nlanPrRat,&lcTmpPo..NLAN_COST1*&lcTmpPo..nLanPrRat)/&lcTmpPo..UnitP
    lnDuCost   = (&lcTmpPo..NLAN_COST2+&lcTmpPo..NLAN_COST3+&lcTmpPo..NLAN_COST4)/&lcTmpPo..UnitD
    lnNewCost2 = IIF(&lcTmpPo..SignD = '/',lnDuCost/&lcTmpPo..nlanDuRat,lnDuCost*&lcTmpPo..nLanDuRat)
    lnNewCost  = lnNewCost1 + lnNewCost2
    lnUseCost  = lnNewCost/Fabric.Conv
    lnUseQty   = IIF(lcScrType='R',-1,1) * &lcTmpPo..nFabTotQty*Fabric.Conv

    IF llGlLink
      DECLARE laGLDistAr[2,13]
      laGLDistAr[1,1] = lcLinkCode
      laGLDistAr[2,1] = lcLinkPO
      laGLDistAr[1,2] = '015'
      laGLDistAr[2,2] = '013'
      laGLDistAr[1,3] = 1
      laGLDistAr[2,3] = -1
      STORE 'MO'            TO laGLDistAr[1,4],laGLDistAr[2,4]
      STORE &lcTmpPo..POMAT TO laGLDistAr[1,5],laGLDistAr[2,5]
      STORE ldPost          TO laGLDistAr[1,6],laGLDistAr[2,6]
      STORE lcGlFYear       TO laGLDistAr[1,7],laGLDistAr[2,7]
      STORE lcGlPeriod      TO laGLDistAr[1,8],laGLDistAr[2,8]
      STORE lcGlDTemp       TO laGLDistAr[1,9],laGLDistAr[2,9]
      laGLDistAr[2,10] = ''
    ELSE
      DIME laGLDistAr[1,1]
      laGLDistAr = ''
    ENDIF
    *-- B602851,1 [Start] Add line no as a parameter to gfMatCrl
    *lnRet = gfMatCrl('1',lcFabric,lcColor,;
                     &lcTmpPO..cWareCode,&lcTmpPO..Dyelot,;
                     &lcTmpPO..DATE,ldPost,&lcTmpPo..POMAT,;
                     lnUseQty,lnUseCost,;
                     &lcTmpPO..Reference,'',;
                     0,'','',@laGLDistAr,lcRecSess)

    PRIVATE laOtherPar
    *B603263,2 HDM [Start] Add new Parameter when calling gfMatCrl
    *                      to Indicate it's the PO receiving program
    *DIMENSION laOtherPar[1,2]
    
    *B604580,1 HBG 07/08/2001 Add the Orginal WareHouse to the Array of the parameters [Begin]
    *DIMENSION laOtherPar[3,2]
    DIMENSION laOtherPar[4,2]
    *B604580,1 [End]
    
    *B603263,2 HDM [End]
    
    laOtherPar[1,1] = 'lnLineNo'
    laOtherPar[1,2] = &lcTmpPO..LINENO

    *B603263,2 HDM [Start] Add new Parameter when calling gfMatCrl
    *                      to Indicate it's the PO receiving program
    laOtherPar[2,1] = 'lcProg'
    laOtherPar[2,2] = 'MARECI'

    
    laOtherPar[3,1] = 'llLastRec'
    laOtherPar[3,2] = (lnRecCount = lnCurrRec)

    *B603263,2 HDM [End]
    
    *B604580,1 HBG 07/08/2001 IF WareHouse changed , send the Orginal WareHouse as 
    *B604580,1                   a parameters to gfMatCrl [Begin]    
    IF &lcTmpPO..cOrgWare <> &lcTmpPO..cWareCode
      laOtherPar[4,1] = 'lcPrvWare'
      laOtherPar[4,2] = &lcTmpPO..cOrgWare
    ELSE
      laOtherPar[4,1] = 'lcPrvWare'
      laOtherPar[4,2] = ""
    ENDIF  
    *B604580,1 [End]
    
    *B604678,1 MHM 08/20/2001 in case of over recive then don't increase onorder[Start]
    *IF  (POFHDR.STATUS = 'C') 
    *  lnUseQty = 0
    *ENDIF
    *B604678,1 MHM 08/20/2001 [end]
    lnRet = gfMatCrl('1',lcFabric,lcColor,;
                     &lcTmpPO..cWareCode,&lcTmpPO..Dyelot,;
                     &lcTmpPO..DATE,ldPost,&lcTmpPo..POMAT,;
                     lnUseQty,lnUseCost,;
                     &lcTmpPO..Reference,'',0,'','',@laGLDistAr,lcRecSess,'','','','','','','',@laOtherPar)

  *-- B602851,1 [End]
  ELSE
    lnUseQty   = IIF(lcScrType='R',-1,1) * &lcTmpPo..nFabTotQty*Fabric.Conv  
    SELECT FABRIC
    REPLACE OnORder WITH OnORder - IIF(lnUseQty > OnORder,OnOrder,lnUseQty)
    SELECT FABDYE
    REPLACE OnORder WITH OnORder - IIF(lnUseQty > OnORder,OnOrder,lnUseQty)
    lnRet = 1
  ENDIF
ENDIF


RETURN lnRet

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/20/02
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
*..E301797,1
FUNCTION lfoldvalue
lcOldValue = EVALUATE(SYS(18))
RETURN


*!*************************************************************
*! Name      : lfvRecType
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/20/02
*! Purpose   : Validate received by popup.
*!*************************************************************
*!..E301797,1
FUNCTION lfvRecType

laScrMode    = .F.
laScrMode[1] = .T.

IF lnRecType = lcOldValue
  RETURN
ENDIF
IF laRecType[lnRecType,2] = 'I'
  SHOW WINDOW (lcWinCh3) TOP
  SHOW WINDOW (lcWinCh2) TOP
ELSE
  SHOW WINDOW (lcWinCh1) TOP
ENDIF  
SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
SHOW GETS WINDOW (lcWinCh8) DISABLE ONLY

SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY

=lfActBrow()
STORE SPACE(6) TO lcBatch,lcShpCode
IF laRecType[lnRecType,2] = 'I'
  SHOW GET lcPoNum ENABLE
  SHOW GET pbPo    ENABLE
  _CUROBJ=OBJNUM(lcPoNum)
ELSE
  SHOW GET pbSType   ENABLE
  SHOW GET lcShpCode ENABLE
  _CUROBJ = OBJNUM(lcShpCode)
ENDIF  
=lfRefresh(lcWinCh1)
=lfRefresh(lcWinCh3)
lcnTpMode = IIF(lcScrType = 'R','DISABLE','ENABLE')
SHOW GET lnRecType &lcnTpMode
SHOW GET ldDate ENABLE
RETURN

*:*************************************************************
*! Name      : lfActBrow
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/20/02
*! Purpose   : Activate screen browse.
*:*************************************************************
FUNCTION lfActBrow

lcBrTpNme    = IIF(lcScrType = 'R','Ret #','P/O #')
lcBrInWin    = IIF(laRecType[lnRecType,2] = 'I',lcWinCh2,lcWinCh9)
lcBrowfields = [cMarker :R:H='',]+;
               [POMAT   :R:H='Po #',Fabric :R:H='Item',] +;
               [Color   :R:H='Color',cTrancd=IIF(TRANCD ='2','R',IIF(TRANCD ='4','C',IIF(cFabGrade = '3','D','S'))) :R:H='Type',]+;
               [Pattern :H='Pattern',]+;
               IIF(llDyelot , [Dyelot :R:H='Dyelot',],[])+;
               IIF(llMultiWH, [cWarecode :R:H='Location',],[])+;               
               [nFabTotQty :R:H='Quantity']
IF laRecType[lnRecType,2] = 'S'               
  SELECT(lcTmpPoSp)
ELSE
  SELECT(lcTmpPo)
ENDIF  

BROWSE FIELDS &lcBrowfields;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       TITLE (lcDetTtl) ;
       WHEN lfwBrowse() AND lfwLine();
       VALID :F lfvBrows() ;
       WINDOW (lcBrInWin) ;
       IN WINDOW (gcBaseWind)         
RETURN

*!*************************************************************
*! Name      : lfvTCode
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/20/02
*! Purpose   : Tran.Code Validation for S and B types.
*!*************************************************************
*..E301797,1
FUNCTION lfvTCode


IF !llBrowse AND (laRecType[lnRecType,2] = 'S' AND EMPTY(lcShpCode))
  RETURN
ENDIF
lnAlias =SELECT()
llBrowse=.F.
llAbort =!lfvShipmnt()
IF llAbort
  STORE SPACE(6) TO lcBatch,lcShpCode
  SELECT(lnAlias)
  _CUROBJ=OBJNUM(lcShpCode)
  RETURN
ENDIF
*SHOW GETS WINDOW (lcWinCh8) DISABLE ONLY
SHOW GET lnRecType DISABLE
SHOW GET PbSType   DISABLE
SHOW GET lcShpCode DISABLE
=lfRefresh(lcWinCh8)
SELECT(lnAlias)
RETURN

*!*************************************************************
*! Name    : lfvShipmnt
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/20/02
*! Purpose : Validate Shipments.
*!*************************************************************
*..E301797,1
FUNCTION lfvShipmnt

IF !SEEK(lcShpCode,'matshphd')
  SELECT matshphd
  lcBrFields = [ShipNo :8:H='Shipment #',]+;
               [Status :1:H="S",]+;
               [Entered :8:H='Entered',]+;
  	           [Cartons :5:H='Cartons',]+;
	           [AirWayB :15:H='Air-Way Bill #',]+;
    	       [ETA     :8:H='E.T.A.',]+;
    	       [nFabTotQty  :7:H='In-Transit',]+;
    	       [RecvStk :11:H='Received',]+;
    	       [RecvDam :11:H='Damaged',]+;
    	       [RecvCan :11:H='Canceled',]+;
    	       [cVessel :30:H='Airline/Vessel',]+;
    	       [Reference :H='Reference']

  SELECT matshphd
  DIME laTempData[1]
  STORE '' TO laTempData
  =gfBrows(.F.,'ShipNo','laTempData','Shipments')
  lcShpCode=laTempData[1]
ENDIF 
IF EMPTY(lcShpCode) OR EOF('matshphd')
  RETURN .F.
ENDIF
SHOW GET lcShpCode

*--Shipment validation.
IF matshphd.Status = 'C'
  *--This shipment has been received complete! unable to proceed.
  = gfModalGen('TRM34078B42000','DIALOG','received complete')
  RETURN .F.
ENDIF
IF matshphd.Status = 'X'
  *--This shipment has been canceled! unable to proceed.
  = gfModalGen('TRM34078B42000','DIALOG','canceled')
  RETURN .F.
ENDIF
SELECT matshpdt
SET ORDER TO TAG Shipno
IF !SEEK(lcShpCode)
  *--The shipment lines have not been found! unable to proceed.
  = gfModalGen('TRM34077B42000','DIALOG')
  RETURN .F.
ENDIF
llShpPO = .F.
=lfGetInfo()

IF RECCOUNT(lcTmpPoSp) = 0
  RETURN .F.
ENDIF
  
SELECT matshpdt
SET ORDER TO TAG matshpdt
RETURN .T.

*!*************************************************************
*! Name      : lfOpn_Rest
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/20/02
*! Purpose   : Open file then restore current alias
*!*************************************************************
*! Calls       : gfOpenFile
*!*************************************************************
*! Passed Parameters : File Directory, File Name, File Tag
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfOpn_Rest()
*!*************************************************************
*..E301797,1
FUNCTION lfOpn_Rest
PARAMETERS lcFileDir,lcFileN,lcIndN

PRIVATE lnCurrAls,llOpenIt 
lnCurrAls = SELECT(0)
llOpenIt = gfOpenFile(lcFileDir+lcFileN,lcIndN,'SH')
SELECT (lnCurrAls)
RETURN llOpenIt
*-- end of lfOpn_Rest.

*!*************************************************************
*! Name    : lfGetInfo
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/20/02
*! Purpose : Get information.
*!*************************************************************
*!..E301797,1
FUNCTION lfGetInfo
PARAMETERS llEmpWare

lnAlias =SELECT()
ldEnterd  =matshphd.Entered
lnCartons =matshphd.Cartons
ldEta     =matshphd.Eta
lcAirWayB =matshphd.AirWayB
lcShpRef  =matshphd.Reference
DIME laZero[9]
laZero = 0
*-Line info.
lcTPO=SPACE(6)
SELECT matshpdt
SCAN WHILE ShipNo = lcShpCode FOR TranCd = '3'
  IF !SEEK('P'+matshpdt.PoMAT,'POFHDR')
    LOOP
  ENDIF  
  WAIT WINDOW 'Shipment Fabric/Color : '+matshpdt.Fabric+matshpdt.Color NOWAIT
  IF !INLIST(POFHDR.Status,'O','A')
    IF POMAT=lcTPO
      LOOP
    ENDIF
    IF POFHDR.Status = 'S'
      *--The P/O &lcTPO status is Closed, the P/O will be skipped for the shipment.
      =gfModalGen('TRM34074B42000','DIALOG',POFHDR.po)        
    ENDIF
    IF POFHDR.Status = 'C'
      *--The P/O &lcTPO status is Closed, the P/O will be skipped for the shipment.
      =gfModalGen('QRM00000B42000',.F.,.F.,.F.,'The P/O '+POFHDR.poMAT+' status is Complete, the P/O will be skipped for the shipment.')
    ENDIF

    lcTPO = matshpdt.PoMAT
    LOOP
  ENDIF 
  IF llMulCurr
    lcCur1  = POFHDR.cPriceCur
    lnCrRt1 = IIF(lcCur1=gcBaseCurr,1,gfChkRate('lnCurrUnt1',lcCur1,ldDate,llEditExRt,gcAct_comp,.F.)) 
    lcCur2  = POFHDR.cDutyCur
    lnCrRt2 = IIF(lcCur2=gcBaseCurr,1,gfChkRate('lnCurrUnt2',lcCur2,ldDate,llEditExRt,gcAct_comp,.F.)) 
    lcPCurr = POFHDR.cPriceCur
    lcDCurr = POFHDR.cDutyCur  
    =lfRefresh()
    lnRateP= gfchkrate('lnUnitP',lcPCurr,ldDate,.F.,gcAct_Comp,.T.) 
    IF lnRateP = 0
      =gfModalGen('TRM36090B36000','ALERT','price')
      lcPCurr = gcBaseCurr
      lcDCurr = gcBaseCurr
      lnRateP = 1
      lcPoNum = SPACE(06)
      _CUROBJ = OBJNUM(lcPoNum)
      =lfRefresh()
      LOOP
    ENDIF  
    lcFrnSign1 = gfGetExSin("",lcPCurr)
    lnRateD= gfchkrate('lnUnitD',lcDCurr,ldDate,.F.,gcAct_Comp,.T.) 
    IF lnRateD = 0
      =gfModalGen('TRM36090B36000','ALERT','duty')
      lcPCurr = gcBaseCurr
      lcDCurr = gcBaseCurr
      lnRateD = 1
      lcPoNum = SPACE(06)
      _CUROBJ = OBJNUM(lcPoNum)
      =lfRefresh()
      LOOP
    ENDIF  
    lcFrnSign2 = gfGetExSin("",lcDCurr)

  ENDIF
  IF lnCrRt1=0 OR lnCrRt2=0
    IF !llEditExRt
      *--This line has currency with zero rate, it will be ignored.' 
      = gfModalGen('TRM34079B42000','DIALOG')
      LOOP
    ELSE
      STORE 1 TO lnCrRt1,lnCrRt2
    ENDIF
  ENDIF
  
  SELECT matshpdt
  SCATTER MEMVAR
  SCATTER FIELDS nCost1,nCost2,nCost3,nCost4 TO laEstiCost
  SELECT(lcTmpPoSp)
  APPEND BLANK
  GATHER MEMVAR
  REPLACE TranCd    WITH '2',;
          ShipNo    WITH lcShpCode,;
          nLanPrRat WITH lnCrRt1,;
          nLanDuRat WITH lnCrRt2
  REPLACE nlanPrRat  WITH lnRateP   ,; 
          nlanDuRat  WITH lnRateD   ,;
          UnitP      WITH lnUnitP   ,;
          UnitD      WITH lnUnitD   ,;          
          SignP      WITH lcFrnSign1,;
          SignD      WITH lcFrnSign2
        
  GATHER FROM laEstiCost FIELDS nLan_Cost1,nLan_Cost2,nLan_Cost3,nLan_Cost4
  IF llMulCurr
    =lfGetEqv('1234',nLanPrRat,nLanDuRat,lnCurrUnt1,lnCurrUnt2,;
               nLan_Cost1,nLan_Cost2,nLan_Cost3,nLan_Cost4)
  ELSE
    =ACOPY(laEstiCost,laECost)
  ENDIF
  GATHER FROM laECost FIELDS nELanCost1,nELanCost2,nELanCost3,nELanCost4
  lnTotQty1 = lnTotQty1 + nFabTotQty
ENDSCAN

WAIT CLEAR
SELECT(lcTmpPoSp)
GO TOP

*--Swich the mode if there is lines selected.
IF !EOF()
  laScrMode=.F.
  laScrMode[4]=.T.
ELSE
  GO TOP
  IF !EOF()
    laScrMode=.F.
    laScrMode[4]=.T.
  ENDIF
ENDIF

SHOW GETS
IF laRecType[lnRecType,2] = 'S' AND laScrMode[1]
  =lfClearInfo()
  SHOW GET  PbSType   ENABLE
  SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  SHOW GET  lcShpCode ENABLE
  SHOW GET  pbNew     DISABLE
  llAbort = .T.
  RETURN
ENDIF
*--Read line info.
SELECT(lcTmpPoSp)
=lfReadLine(EOF())
SELECT(lnAlias)
RETURN
*-- end of lfGetInfo. lcwarecode

*!*************************************************************
*! Name    : lfClearInfo
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/20/02
*! Purpose : Clear information.
*!*************************************************************
*!..E301797,1
FUNCTION lfClearInfo

*--Clear Header.
STORE ' ' TO lcShpCode,lcAirWayB,lcShpRef,lcBDesc,lcBStatus,lcSOrder,lcAOrder
STORE {}  TO ldEnterd,ldEta,ldBDate
STORE 0   TO lnCartons,lnTotStk,lnTotDam,lnTotCan,lnPolstln
STORE 0  To lnTotQty1,lnTotQty2,lnTotQty3,lnNewRec,lnOldQty,lnQtyUse
STORE .F. TO llByCarton
*--Clear Line.
=lfReadLine(.T.)
RETURN

*!*************************************************************
*! Name    : lfReadLine
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/20/02
*! Purpose : Read line information.
*!*************************************************************
*!..E301797,1
FUNCTION lfReadLine
PARA llClearLn

lnAlias = SELECT()
IF ! llClearLn
  =lfwLine()
ELSE
  STORE ' ' TO lcCarton,lcTCode,lcStyle,lcRefer,lcCur1,lcCur2,;
               lcStyDesc,lcDyelot,lcPfrnSmbl,lcDfrnSmbl,lcWareCode,lcSOrder,lcAOrder
  STORE 0   TO lnWare,lnRate1,lnRate2
  STORE 1   TO lnCurrUnt1,lnCurrUnt2
ENDIF
=lfShowScr()
*E301797,1 (Begin) Remark.
*SHOW WINDOW (lcDet_Ttl) REFRESH
*E301797,1 (End)
=lfRefresh(lcWinCh9)
SELECT(lnAlias)
RETURN


*:*************************************************************
*! Name    : lfShowScr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/20/02
*! Purpose : Show all objects in lcWinCh5 in Add Mode.
*:*************************************************************
*!..E301797,1
FUNCTION lfShowScr

IF laRecType[lnRecType,2] = 'S'
  SHOW GETS WINDOW (lcWinCh9) ENABLE ONLY      
  SHOW GET pbSType   DISABLE
  SHOW GET lcShpCode DISABLE
ELSE
  SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
ENDIF
IF laRecType[lnRecType,2] = 'S'
  SHOW GET pbNew    ENABLE
  IF !EOF(lcTmpPo)
    SHOW GET pbRemove ENABLE
    SHOW GET llLoc    ENABLE
    SHOW GET lcRefer  ENABLE
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfGetEqv
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/20/02
*! Purpose   : Get eguevalent costs.
*!*************************************************************
*!..E301797,1
FUNCTION lfGetEqv

PARA lcUpdCsts,lnPRate1,lnDRate2,lnCurUnt1,lnCurUnt2,;
     lnFCost1,lnFCost2,lnFCost3,lnFCost4

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
RETURN

*!*************************************************************
*! Name      : lfvEquCost
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/20/02
*! Purpose   : Get eguevalent cost by pathing the foreign.
*!*************************************************************
*!..E301797,1
FUNCTION lfvEquCost
PARAMETERS lcCstNo,lnFrnCost,lnCurRate,lnCurUnt

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
*! Name      : lfvMType
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/20/02
*! Purpose   : validate the transaction type field
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*!..E301797,1
FUNCTION lfvMType

PRIVATE lnAlias
SELECT (lcTmpPoSp)
IF lnType <> lnOldType 
  DO CASE
    CASE lnType = 1
      REPLACE &lcTmpPoSp..TRANCD WITH '2'
      lnTotQty1 = lnTotQty1 + &lcTmpPoSp..nFabTotQty
      SHOW GET lcQuaFab DISABLE
      SHOW GET pbQuaFab DISABLE
      SHOW GET lcQuaClr DISABLE
      SHOW GET pbQuaClr DISABLE
      DO CASE
        CASE lnOldType = 2
          lnTotQty2 = lnTotQty2 - &lcTmpPoSp..nFabTotQty
        CASE lnOldType = 3 OR lnOldType = 4
          lnTotQty3 = lnTotQty3 - &lcTmpPoSp..nFabTotQty
      ENDCASE
      lcQuaFab = SPACE(07)
      lcQuacLR = SPACE(06)      
    CASE lnType = 2
      IF &lcTmpPoSp..CanQty = 0
        =gfModalGen('INM36105B00000','ALERT')
        lnType = lnOldType 
        SHOW GET lnType
        RETURN
      ENDIF
      IF lnQtyBuy > &lcTmpPoSp..CanQty
        =gfModalGen('INM36095B00000','ALERT',ALLTRIM(STR(&lcTmpPoSp..CanQty)))
        lnType = lnOldType 
        SHOW GET lnType
        RETURN
      ENDIF
      REPLACE &lcTmpPoSp..TRANCD WITH '4'
      lnTotQty2 = lnTotQty2 + &lcTmpPoSp..nFabTotQty
      SHOW GET lcQuaFab DISABLE
      SHOW GET pbQuaFab DISABLE
      SHOW GET lcQuaClr DISABLE
      SHOW GET pbQuaClr DISABLE
      DO CASE
        CASE lnOldType = 1
          lnTotQty1 = lnTotQty1 - &lcTmpPoSp..nFabTotQty
        CASE lnOldType = 3 OR lnOldType = 4
          lnTotQty3 = lnTotQty3 - &lcTmpPoSp..nFabTotQty
      ENDCASE   
      lcQuaFab = SPACE(07)
      lcQuacLR = SPACE(06)      
    CASE lnType = 3 OR lnType = 4
      REPLACE &lcTmpPoSp..TRANCD WITH '3'
      lnTotQty3 = lnTotQty3 + IIF(lnOldType<>3 AND lnOldType<>4,&lcTmpPoSp..nFabTotQty,0)
      SHOW GET lcQuaFab ENABLE
      SHOW GET pbQuaFab ENABLE
      SHOW GET lcQuaClr ENABLE
      SHOW GET pbQuaClr ENABLE
      SHOW GET lcDyelot DISABLE
      =SEEK(lcFabric+lcColor,'FABRIC')
      DO CASE
        CASE lcGrade = '1'
          IF lnType = 3
            lcFabGrade = '2'
            lcQuaFab = Fabric.cFabric1
            lcQuaClr = IIF(Fabric.cColor1<>'******',Fabric.cColor1,SPACE(06))
          ELSE
            lcFabGrade = '3'
            lcQuaFab = Fabric.cFabric2
            lcQuaClr = IIF(Fabric.cColor2<>'******',Fabric.cColor2,SPACE(06))
          ENDIF
        CASE lcGrade = '2'
          IF lnType = 3
            lcFabGrade = '1'         
            lcQuaFab = Fabric.cFabric1
            lcQuaClr = IIF(Fabric.cColor1<>'******',Fabric.cColor1,SPACE(06))
          ELSE
            lcFabGrade = '3'
            lcQuaFab = Fabric.cFabric2
            lcQuaClr = IIF(Fabric.cColor2<>'******',Fabric.cColor2,SPACE(06))
          ENDIF
          
        CASE lcGrade = '3'
          IF lnType = 3
            lcFabGrade = '1'         
            lcQuaFab = Fabric.cFabric1
            lcQuaClr = IIF(Fabric.cColor1<>'******',Fabric.cColor1,SPACE(06))
          ELSE
            lcFabGrade = '2'
            lcQuaFab = Fabric.cFabric2
            lcQuaClr = IIF(Fabric.cColor2<>'******',Fabric.cColor2,SPACE(06))
          ENDIF
      ENDCASE 
      REPLACE &lcTmpPoSp..cFabGrade WITH lcFabGrade
      DO CASE 
        CASE lnOldType = 1
          lnTotQty1 = lnTotQty1 - &lcTmpPoSp..nFabTotQty
        CASE lnOldType = 2
          lnTotQty2 = lnTotQty2 - &lcTmpPoSp..nFabTotQty
      ENDCASE    
  ENDCASE
ENDIF
lnAlias = SELECT()
SELECT UNCMSESS
REPLACE mComent WITH lfVarStr(@laVariables)
SELECT(lnAlias)
REPLACE &lcTmpPoSp..cFabric1 WITH lcQuaFab
REPLACE &lcTmpPoSp..cColor1  WITH lcQuaClr
=lfwLine()


*!*************************************************************
*! Name      : lfSavShp
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/20/02
*! Purpose   : Save shipment.
*!*************************************************************
*!..E301797,1
FUNcTION lfSavShp

lcSeekKey = SPACE(01)
llRoll  = .F.
lnAlias = SELECT()
IF llTrkRolls AND lcKeyType = 'P'
  SELECT(lcTmpPoSp)
  SCAN FOR (nFabTotQty > 0 AND TRANCD <> '4')
    lcSeekKey = IIF(EMPTY(cFabric1),Fabric+Color,cFabric1+cColor1)
    =SEEK(lcSeekKey,'Fabric')
    IF Fabric.lTrkRolls
      llRoll = .T.
      IF !EMPTY(lcTmpRoll)
        SELECT(lcTmpRoll)
        lcTmpRTag = ORDER()
        SET ORDER TO lcTmpRoll2
        IF lcTrancd = '2'
          IF !SEEK(lcSeekKey+&lcTmpPoSp..cWarecode+&lcTmpPoSp..Dyelot,lcTmpRoll) OR;
             (SEEK(lcSeekKey+&lcTmpPoSp..cWarecode+&lcTmpPoSp..Dyelot,lcTmpRoll) AND NAPPLY = 0)
            SET ORDER TO &lcTmpRTag
            =gfModalGen('TRM36096B36000','ALERT','rolls')
            llCSave = .F.
            SELECT(lcTmpPoSp)
            RETURN
          ENDIF
        ENDIF
      ELSE
        =gfModalGen('TRM36096B36000','ALERT','rolls')
        llCSave = .F.
        SELECT(lcTmpPoSp)
        RETURN
      ENDIF
    ENDIF
  ENDSCAN
ENDIF
IF (llTrkRolls OR lcMtCstMth $ 'LIF') AND lcKeyType = 'R'
  SELECT(lcTmpPoSp)
  SCAN FOR (nFabTotQty > 0 AND TRANCD <> '4')
    lcSeekKey = IIF(EMPTY(cFabric1),Fabric+Color,cFabric1+cColor1)
    =SEEK(lcSeekKey,'Fabric')
    IF Fabric.lTrkRolls
      llRoll = .T.
      *--Check rolls entered for each line
      *  If not don't save
      IF !EMPTY(lcTmpRoll) AND USED(lcTmpRoll)
        SELECT(lcTmpRoll)
        lcTmpRTag = ORDER()
        SET ORDER TO lcTmpRoll2
        *-- IF  We can't find the roll record
        *-- OR  We found it
        *-- AND The user didn't apply any quantity
        *--         Stop the Saving process
        IF !SEEK(lcSeekKey+&lcTmpPoSp..cWarecode+&lcTmpPoSp..Dyelot,lcTmpRoll)
          SET ORDER TO &lcTmpRTag
          =gfModalGen('TRM36096B36000','ALERT','rolls')
          llCSave = .F.
          SELECT(lcTmpPoSp)
          RETURN
        ELSE
          = SEEK(lcSeekKey+&lcTmpPoSp..cWarecode+&lcTmpPoSp..Dyelot,lcTmpRoll)
          LOCATE REST WHILE cfabric+ccolor+cwarecode+cdyelot+STR(lineno,6) ;
                          = lcSeekKey + &lcTmpPoSp..cWarecode +  &lcTmpPoSp..Dyelot;
                          FOR NAPPLY > 0
          IF !FOUND()
            SET ORDER TO &lcTmpRTag
            =gfModalGen('TRM36096B36000','ALERT','rolls')
            llCSave = .F.
            SELECT(lcTmpPoSp)
            RETURN
          ENDIF
        ENDIF
      ELSE
        =gfModalGen('TRM36096B36000','ALERT','rolls')
        llCSave = .F.
        SELECT(lcTmpPoSp)
        RETURN
      ENDIF
    ELSE
      IF lcMtCstMth $ 'LIF'
        IF !SEEK(lcSeekKey+&lcTmpPoSp..cWarecode+&lcTmpPoSp..Dyelot,lcTmpJour)
          =gfModalGen('TRM36096B36000','ALERT','lots')
          llCSave = .F.
          SELECT(lcTmpPoSp)
          RETURN
        ENDIF
        SELECT(lcTmpJour) 
        SCAN WHILE cFabric+cColor+cWareCode+cDyelot = ;
                   lcSeekKey+&lcTmpPoSp..cWarecode+&lcTmpPoSp..Dyelot
          IF nApply > 0
            llRoll = .T.
            EXIT
          ENDIF
        ENDSCAN
      ELSE
        llRoll = .T.
      ENDIF
    ENDIF  
  ENDSCAN
  IF !llRoll  
    =gfModalGen('TRM36096B36000','ALERT',IIF(Fabric.lTrkRolls,'rolls','lots'))
    llCSave = .F.
    SELECT(lcTmpPoSp)
    RETURN
  ENDIF  
ENDIF

lcRecSess = gfsequence('GLSESSION')
=SEEK('O'+lcUnsmsPrg+gcUser_id+lcSession,'UNCMSESS')
SELECT UNCMSESS
REPLACE cCurrObj WITH 'pbSav'
=lfUpdPofln()
SELECT(lcTmpPoSp)
lnRecCount = RECCOUNT()
PRIVATE lcFabric,lcColor,lnRet

PRIVATE lnCurrRec
STORE 0 TO lnCurrRec
SCAN FOR nFabTotQty > 0 
  *E301797,1 (Begin) Update in transir qty.
  lcSeekKey = IIF(EMPTY(cFabric1),Fabric+Color,cFabric1+cColor1)
  =SEEK(lcSeekKey,'Fabric')
  =SEEK(lcSeekKey+lcWare+SPACE(10),'FabDye')
  REPLACE FABRIC.Matotintr WITH MAX(FABRIC.Matotintr-(NFABTOTQTY*Fabric.Conv),0)
  REPLACE FABDYE.Matotintr WITH MAX(FABDYE.Matotintr-(NFABTOTQTY*Fabric.Conv),0)
  
  *E301797,1 (End)
  lnCurrRec = lnCurrRec + 1
  lcFabric = IIF(EMPTY(&lcTmpPoSp..cFabric1),&lcTmpPoSp..Fabric,&lcTmpPoSp..cFabric1)
  lcColor  = IIF(EMPTY(&lcTmpPoSp..cFabric1),&lcTmpPoSp..Color,&lcTmpPoSp..cColor1)
  **SET ORDER TO Matln IN matshpdt
  =SEEK(ShipNo,'matshphd')
  =SEEK(lcKeyType+&lcTmpPoSp..POMAT,'POFHDR')
  =SEEK(&lcTmpPoSp..FABRIC+&lcTmpPoSp..COLOR+lckeyType+&lcTmpPoSp..POMAT+'1','matshpdt')
  =SEEK(lcKeyType+&lcTmpPoSp..POMAT+&lcTmpPoSp..FABRIC+&lcTmpPoSp..COLOR+'1','POFLN')
  =lfUpdVend()
  PRIVATE lnOldLinQty
  lnOldLinQty = POFLn.nFabTOTQTY
  lnRet = lfFilsUpd()

  IF lnRet <> 0
    =lfUpdLines()
    IF llWareLoc  
      =lfUpdlOC()
    ENDIF
  ENDIF
ENDSCAN
=lfUpdHdr()

IF llGlLink
  =lfUpdGlFile()
ENDIF  
=SEEK('O'+lcUnsmsPrg+gcUser_id+lcSession,'UNCMSESS')
SELECT UNCMSESS
REPLACE Status WITH 'C'
* Call the screen of arranging dyelots in case of P/O
IF llDyelot AND lcScrType = 'P'
  =gfArDyRl('','',lcTmpPoSp,.T.)
ENDIF
*C200256,1 (Begin) Receive Qty for CON10.
IF ASCAN(laEvntTrig,PADR("PRNTIT",10)) <> 0
  SELECT(lcTmpPoSp)
  COUNT FOR TRANCD = "2" TO lnPrgnO    
  LOCATE FOR TRANCD = "2"
  IF FOUND()
    llFromCost = .F.
    SET FILTER TO TRANCD = "2"
    lnNoOfCop = 1
    lcTranTit = IIF(cmattype = 'R','RET PO','PO')
    lcTrans   = "POMAT"
    lcTotQty  = "nFabTotQty"
    lcRecDate = "DATE"
    lcVenCode = "VENDOR"
    IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Do you wish to print the received quantities?')=1)
      = gfDoTriger("MAPOREC",PADR("PRNTIT",10))    
   ENDIF
 ENDIF  
ENDIF  
*C200256,1 (Begin)
*- Call Bar Code Printing Report.
*  Would you like to print Barcode labels ?
SELECT Rolls
LOCATE
IF llTrkRolls .AND. ( lcMtCstMth = "L" OR !EOF('Rolls') )
  IF gfModalGen('QRM36171B42002','DIALOG','Rolls label') = 1
    PRIVATE lcRunRep , lcRepDir , lcCallFrom
    lcRunRep  = 'MABARC'           && Calling BarCode labels with another report ID
    lcRepDir = gcRepHome+gcAct_Appl+'REPORT'  && To call MaReport with 'MaBarc'
    lcCallFrom = "P"      && Report is called from Program from Transaction menu
    
    *B605659,1 RAE [START]
    llFromMan = .F.
    *B605659,1 RAE [END]
    
    DO &lcRepDir WITH lcRunRep
  ENDIF
ENDIF

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfGetOpnQ
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 09/16/2002
*! Purpose   : Getting the open quantity for each line
*!*************************************************************
*B606455,1 KHM 09/16/2002 
*!*************************************************************
FUNCTION lfGetOpnQ
PRIVATE lnAlias, lnOpnQy

lnAlias = SELECT(0)

SELECT POFLN
=SEEK(lcKeyType + &lcTmpPo..Pomat + &lcTmpPo..Fabric +&lcTmpPo..Color)
lnOpnQy = 0

SCAN REST WHILE  cMattype+Pomat+Fabric+Color+Trancd = lcKeyType + &lcTmpPo..Pomat + &lcTmpPo..Fabric +&lcTmpPo..Color
  IF Trancd = '1'
    lnOpnQy = lnOpnQy + nFabTotQty
  ELSE
    lnOpnQy = MAX(lnOpnQy - nFabTotQty,0)
  ENDIF
ENDSCAN

=SEEK(lcKeyType + &lcTmpPo..Pomat + &lcTmpPo..Fabric +&lcTmpPo..Color)
SELECT(lnAlias)
RETURN lnOpnQy

*:*************************************************************
*: Name       : lfvAplyQty
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 10/09/2002
*: Purpose    : Check on the Apply Qty In Cost Method 'FI' against the onhand
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfvAplyQty()
*:*************************************************************
*!B606285,1
FUNCTION lfvAplyQty
*-- lnAlias    : - This Variable Hold The Cirrent Alias.
*-- llCompSave : - This Variable Will return True to complete saving
PRIVATE lnAlias , llCompSave
llCompSave = .T.

lnAlias = SELECt(0)

SELECT(lcTmpPo)
SCAN FOR (nFabTotQty > 0 AND TRANCD <> '4')
  lcFabric = Fabric
  lcColor  = Color
  lcWare   = cWarecode
  lcDyelot = Dyelot
  IF SEEK(lcFabric+lcColor+lcWare+Space(10),'FABDYE') .AND. nFabTotQty < Fabdye.Onhand
    IF !lfOldLots()
      llCompSave = .F.
      EXIT
    ENDIF
  ELSE
    llCompSave = .F.
  ENDIF
ENDSCAN

IF !llCompSave
  *-- Give message to the user.
  *-- Message Text: Issue Quantity cannot be greater than On Hand Quantity.  
  *-- Message No. : 04025
  *-- Button No.  : 00000 
  *-- Button text : OK
  =gfModalGen('TRM04025B00000','ALERT','Issue Quantity for fabric/Color '+lcFabric+'/'+lcColor+'|'+'On Hand Quantity')        
ENDIF

SELECT (lnAlias)

RETURN llCompSave

*-- End Of lfvAplyQty
*:*************************************************************