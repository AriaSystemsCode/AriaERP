*:***********************************************************************************
*:           File: ICUPDSV.PRG                                                     :*
*:         System: ARIA APPAREL SYSTEM 2.7                                         :*
*:         Module: Inventory Control, Material                                     :*
*:        Program: Adjust Stock Value.                                             :*
*:         Author: Wael M. Abo-Shawareb (WSH)                                      :*
*:           Date: 12/16/2004                                                      :* 
*:***********************************************************************************
*: Passed Parameters  :                                                            :*
*:       lcItemType : Item Type '0001 for Style and 0002 for Material'             :*
*:       lcItemCode : Item Code "Style Field 'Style'" or "Fabric+Color 'Material'" :*
*:       lcColorFil : Item Colors Temp Data Cursor                                 :*
*:       llNoScreen : Update Stock without displaying Screen.                      :*
*:***********************************************************************************
*: Modification       :
*B126371,1 MHM 02/13/2005 Fix error in opening file where Gl not installed
*B126312,1 EIH 03/13/2005 No edit costing in style screen when cost method standar and no adjustment reasons in codes file .
*B128128,1 HBG 11/20/2005 Add new parameter to allow calculate the cost according to the base currency
*B607971,1 TMI 02/13/2007 Use the default WH in case no multiple WH is selected. Use space if no dylots is used
*:***********************************************************************************
*B128128,1 HBG 11/20/2005 Add new parameter to allow calculate the cost according to the base currancy [Begin]
*PARAMETERS lcItemType, lcItemCode, lcColorFil, llNoScreen 
PARAMETERS lcItemType, lcItemCode, lcColorFil, llNoScreen , laCostFld
*B128128,1 [End]

*--Program variables initialization.
lnAdjCode = 1

PRIVATE lcItemCode, lcItemType, lnNewCost, lnOldCost

DIMENSION laAdjCode[1], laAdjInfo[1,10]
STORE ''  TO laAdjCode, laAdjInfo, lcAdjLink
STORE ' ' TO lcStyHdr,lcStyPict,laAdjCode,laAdjInfo
STORE ''  TO lcGlFYear,lcGlPeriod,lcRefer,lcStyCst
STORE ''  TO lcAdjReason, lcItemDye
STORE .F. to llDyeLvl, llDyelot, llMultiWH, llOpStyDye, llOpFabDye, llOpStyJl,  llOpMatJl

lcRefer      = "**Adjust Stock Value**"  && Reference to update tables with
llContinSave = .T.                       && Don't continue saving of Style or Fabric if Update failed
lcTmpAdj     = gfTempName()              && Temp Cursor for Adjustment file

*-- Get the setup variables from the memory files.
llGlLink  = gfGetMemVar('M_Link_GL')  ='Y'
llMultiWH = gfGetMemVar('M_WareHouse')='Y'

*B126312,1 EIH 03/13/2005 No edit costing in style screen when cost method 'S' and no adjustment reasons in codes.[Begin]
*-- If no Reason Codes, Return
*SELECT CODES
*lcCodTag = TAG()
*SET ORDER TO TAG cCode_no
*IF !SEEK("N"+'CADJREASON','CODES')
  *--You have to edit the Adjustment reasons codes first, Cannot proceed.
*  =gfModalGen('TRM42111B42001','DIALOG')
*  SET ORDER TO TAG &lcCodTag
*  llContinSave = .F.
*  RETURN 
*ENDIF  
*SET ORDER TO TAG &lcCodTag
*B126312,1 EIH 03/13/2005 [End]

*-- Get default Adjustment Reason Code
laAdjInfo[1,01] = "CADJREASON"
laAdjInfo[1,02] = "laAdjCode"
laAdjInfo[1,03] = "lnAdjCode"
laAdjInfo[1,04] = ""
laAdjInfo[1,05] = .F.
laAdjInfo[1,06] = .F.
laAdjInfo[1,07] = .F.
laAdjInfo[1,08] = .F.
laAdjInfo[1,09] = ""
laAdjInfo[1,10] = ""
=gfwCodePop(@laAdjInfo, "CADJREASON", "D")
lcAdjReason = laAdjCode[lnAdjCode,2]

*--Gl adjustment account.
IF llGlLink AND !EMPTY(lcAdjReason)
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjLink'
  = gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
ELSE
  lcAdjReason = ' '
ENDIF

*-- Open a temp file to be used in calling gl distributer proc.
IF llGlLink
  =gfOpenFile(gcDataDir+'GLDist','GLDistAc','SH')

  SELECT GLDist
  lcTmpGlDt = gfTempName()
  COPY STRU TO &gcWorkDir.&lcTmpGlDt

  USE (gcWorkDir+lcTmpGlDt) IN 0 EXCLUSIVE
ENDIF

DO CASE
  CASE lcItemType = '0001'
    lcItem    = 'STYLE'
    lcItemDye = 'STYDYE'
    =gfOpenFile(gcDataDir+"INVTADJ", "INVTADJ", "SH", @lcTmpAdj, .T.)
    
    IF !Used('StyInvJL')
      =gfOpenFile(gcDataDir+"StyInvJl", "StyInvJl", "SH")
      llOpStyJl = .T.
    ENDIF

    IF !Used('STYDYE')
      =gfOpenFile(gcDataDir+"StyDye", "StyDye", "SH")
      llOpStyDye = .T.
    ENDIF

    llDyelot   = gfGetMemVar('M_Dyelot') ='Y'

    lcStyHdr   = gfItemMask('HM')
    lnMajLen   = LEN(gfItemMask('PM'))
  
    *--Displayed Screen Message
    lcStyCst  = 'The total cost for ' + ALLTRIM(lcStyHdr) + ': "' + ;
                 ALLTRIM(SUBSTR(lcItemCode, 1, lnMajLen)) + ;
                '" has been changed.'

    *-- Get Gl Current Period and Year
    IF llNoScreen
      =CHECKPRD(gdSysDate,'lcGLFYear','lcGLPeriod', 'IP', .T.)
    ENDIF

  CASE lcItemType = '0002'
    lcItem    = 'FABRIC'
    lcItemDye = 'FABDYE'
    =gfOpenFile(gcDataDir+"FINVTADJ", "FINVTADJ", "SH", @lcTmpAdj, .T.)

    IF !Used('StyInvJL')
      =gfOpenFile(gcDataDir+"MatInvJl", "MatInvJl", "SH")
      llOpMatJl = .T.
    ENDIF
    
    IF !Used('FabDye')
      =gfOpenFile(gcDataDir+"FabDye", "FabDye", "SH")
      llOpFabDye = .T.
    ENDIF

    llDyelot   = gfGetMemVar('M_MATDYE') ='Y'

    *--Displayed Screen Message
    lcStyCst  = 'The cost use for Fabric: "' + ;
                 ALLTRIM(SUBSTR(lcItemCode, 1, 7)) + IIF(!EMPTY(SUBSTR(lcItemCode, 8, 6)), ' - ', '') + ALLTRIM(SUBSTR(lcItemCode, 8, 6)) + ;
                '" has been changed.'

    *-- Get Gl Current Period and Year
    IF llNoScreen
      =CHECKPRD(gdSysDate,'lcGLFYear','lcGLPeriod', 'MP', .T.)
    ENDIF
ENDCASE

PUSH KEY

*--Check if Style has transactions or not.
PRIVATE llFound
llFound = .F.

DIMENSION laStyStk[10]

SELECT (lcColorFil)
LOCATE
IF SEEK(lcItemCode)
  SCAN REST WHILE Style = lcItemCode
    lcFCond    = IIF(llDyeLvl, "!EMPTY(Dyelot)", IIF(llMultiWH, "EMPTY(Dyelot)", ".T."))
    lcWCond    = IIF(lcItemType = '0001', "Style = ", "Fabric+Color = ") + "&lcColorFil..Style"

    lcWorkFile = IIF(llMultiWH OR llDyeLvl, lcItemDye, lcItem)
    SELECT(lcWorkFile)
    =SEEK(&lcColorFil..Style)

    *-- Scan in ItemDye or Item Files with the Current Item Code
    *---- If we scan in Item file, there is only one record...
    *---- If we scan in ItemDye, the number of records depends on Warecodes and Dyelots for current Item Code...
    SCAN REST WHILE &lcWCond FOR &lcFCond
      =lfBalanced(IIF(lcItemType = '0001', 'S', 'M'))
      IF IIF(lcItemType = '0001', laStyStk[9]<>0 OR laStyStk[10] <> 0,laStyStk[1]<>0 OR laStyStk[2]<>0)
        llFound = .T.
        EXIT
      ENDIF      
    ENDSCAN
    
    IF llFound
      EXIT
    ENDIF
  ENDSCAN
ENDIF

*--If no transactions found, return
IF !llFound
  llContinSave = .T.
  RETURN
ENDIF

SELECT (lcColorFil)
LOCATE
IF SEEK(lcItemCode)
  SCAN REST WHILE Style = lcItemCode
    =SEEK(&lcColorFil..Style, lcItem)

    DO CASE
      CASE lcItemType = '0001'
        IF &lcColorFil..Make
          lnNewCost = &lcColorFil..nMCost1 + &lcColorFil..nMCost2 + &lcColorFil..nMCost3 + &lcColorFil..nMCost4 + &lcColorFil..nMCost5
          lnOldCost = Style.nMCost1 + Style.nMCost2 + Style.nMCost3 + Style.nMCost4 + Style.nMCost5
        ELSE
          *B128128,1 HBG 11/20/2005 Calculate the cost according to the base currancy [Begin]
          *lnNewCost = &lcColorFil..nICost1 + &lcColorFil..nICost2 + &lcColorFil..nICost3 + &lcColorFil..nICost4 + &lcColorFil..nICost5
          *lnOldCost = Style.nICost1 + Style.nICost2 + Style.nICost3 + Style.nICost4 + Style.nICost5
          STORE 0 TO lnNewCost , lnOldCost
          FOR lnI = 1 TO ALEN(laCostFld,1)
            lnOldCost = lnOldCost + laCostFld[lnI,1]
            lnNewCost = lnNewCost + laCostFld[lnI,2]
          ENDFOR
          *B128128,1 [End]
        ENDIF

      CASE lcItemType = '0002'
        lnNewCost = &lcColorFil..CostUse
        lnOldCost = Fabric.CostUse
    ENDCASE
    
    IF lnOldCost <> lnNewCost
      IF !llNoScreen
        llContinSave = .F.
        DO (gcScrDir + "ICUPDSV.SPX")

        IF !llContinSave
          EXIT
        ENDIF

        llNoScreen = .T.
      ELSE
        =lpModOK()
      ENDIF
    ENDIF
    SELECT (lcColorFil)

    IF !llContinSave
      EXIT
    ENDIF
  ENDSCAN
ENDIF

*-- Update distripution master file
*B126371,1 MHM 02/13/2005 move to work only if GL installed[Start]
*SELECT (lcTmpGlDt)
*LOCATE
*B126371,1 MHM [End]

IF llGlLink AND !EOF(lcTmpGlDt)
  *B126371,1 MHM 02/13/2005 move to work only if GL installed[Start]
  SELECT (lcTmpGlDt)
  LOCATE
  *B126371,1 MHM [End]
  
  WAIT WINDOW 'Updating the general ledger distribution file.' NOWAIT

  *-- Get a unique session number.
  lcGlSess = gfsequence('GLSESSION')
  REPLACE ALL GLSESSION WITH lcGlSess
  USE

  SELECT GLDIST  
  APPEND FROM &gcWorkDir.&lcTmpGlDt     

  ERASE (gcWorkDir+lcTmpGlDt+'.DBF')
ENDIF
WAIT CLEAR

POP KEY

*--Normal exit.
IF glQuitting
  USE IN (lcTmpAdj)
  
  IF llOpStyDye
    USE IN StyDye
  ENDIF
  
  IF llOpFabDye
    USE IN FabDye
  ENDIF

  IF llOpStyJl
   USE IN StyInvJl
  ENDIF

  IF llOpMatJl
   USE IN MatInvJl
  ENDIF
ENDIF

RETURN
*End...

*!*************************************************************
*! Name      : lpModOK()
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 12/16/2004
*! Purpose   : Valid Fuction for the Ok Button
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lpModOK

IF !llNoScreen
  =CHECKPRD(gdSysDate,'lcGLFYear','lcGLPeriod', IIF(lcItemType = '0001', 'IP', 'MP'), .T.)
ENDIF

PRIVATE lcWorkFile, llUpdRet, lcFCond, lnCurrRec
llDyeLvl   = (llDyelot AND &lcItem..cDye_Flg = 'Y')
lcWorkFile = IIF(llMultiWH OR llDyeLvl, lcItemDye, lcItem)
llUpdRet   = .T.
lcFCond    = IIF(llDyeLvl, "!EMPTY(Dyelot)", IIF(llMultiWH, "EMPTY(Dyelot)", ".T."))
lcWCond    = IIF(lcItemType = '0001', "Style = ", "Fabric+Color = ") + "&lcColorFil..Style"

SELECT (lcWorkFile)
=SEEK(&lcColorFil..Style)

*-- Scan in ItemDye or Item Files with the Current Item Code
*---- If we scan in Item file, there is only one record...
*---- If we scan in ItemDye, the number of records depends on Warecodes and Dyelots for current Item Code...
SCAN REST WHILE &lcWCond FOR &lcFCond
  SELECT (lcWorkFile)
  lnCurrRec = RECNO()
    
  SELECT IIF(!llMultiWH .AND. !llDyeLvl, lcItem, lcItemDye)
  DO CASE
    CASE lcItemType = '0001'
      IF !lfBalanced('S')
        GATHER FROM laStyStk FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk,nStkVal
      ENDIF

      *--If stock value equal zero, don't update
      IF TotStk = 0 AND nStkVal = 0
        LOOP
      ENDIF
                 
      SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
      lnTotStk = TotStk

      SELECT (lcTmpAdj)
      APPEND BLANK

      GATHER FROM laToSave FIELDS OldQty1,OldQty2,OldQty3,OldQty4 ,;
                                  OldQty5,OldQty6,OldQty7,OldQty8
      GATHER FROM laToSave FIELDS Adj1,Adj2,Adj3,Adj4 ,;
                                  Adj5,Adj6,Adj7,Adj8
      REPLACE Style     WITH &lcWorkFile..Style ,;
              TotOld    WITH lnTotStk  ,;
              TotAdj    WITH lnTotStk  ,;
              dPostDate WITH gdSysDate ,;
              cReason   WITH lcRefer   ,;
              Date      WITH gdSysDate ,;
              Type      WITH 'P'       ,; 
              Unt_Cost  WITH lnNewCost ,;
              Old_Cost  WITH lnOldCost

    CASE lcItemType = '0002'
      IF !lfBalanced('M')
        GATHER FROM laStyStk FIELDS OnHand,nStkVal
      ENDIF
           
      *--If stock value equal zero, don't update
      IF OnHand = 0 AND nStkVal = 0
        LOOP
      ENDIF
                 
      lnTotStk = OnHand

      SELECT (lcTmpAdj)
      APPEND BLANK
      REPLACE Fabric     WITH &lcWorkFile..Fabric ,;
              Color      WITH &lcWorkFile..Color  ,;
              OldQty     WITH lnTotStk  ,;
              nmTotAdj   WITH lnTotStk  ,;
              Date       WITH gdSysDate ,;
              cReason    WITH lcRefer   ,;
              Date       WITH gdSysDate ,;
              Type       WITH 'P'       ,;
              nFUnitCost WITH lnNewCost ,;
              nUntCstBuy WITH &lcColorFil..CostBuy
  ENDCASE

  SELECT (lcTmpAdj)
  REPLACE dAdd_Date WITH gdSysDate  ,;
          cAdd_Time WITH TIME()     ,;
          cAdd_User WITH gcUser_id  ,;
          GlFYear   WITH lcGlFyear  ,;
          GlPeriod  WITH lcGlPeriod
  
  IF llDyelvl
    REPLACE Dyelot WITH &lcWorkFile..Dyelot
  ENDIF
  
  IF llMultiWH
    REPLACE cFromWare WITH &lcWorkFile..cWareCode
  ENDIF
  
  PRIVATE lcWMsg
  
  DO CASE
    CASE lcItemType = '0001'
      lcWMsg = ALLTRIM(&lcColorFil..Style) + ;
               IIF(llDyelvl, '/' + &lcWorkFile..Dyelot, '')
    CASE lcItemType = '0002'
      lcWMsg = ALLTRIM(&lcColorFil..Fabric) + ' - ' + ALLTRIM(&lcColorFil..Color) + ;
               IIF(llDyelvl, '/' + &lcWorkFile..Dyelot, '')
  ENDCASE
  
  WAIT WINDOW 'Start updating => ' + lcWMsg NOWAIT

  *--Start Updating.
  llUpdRet = lfInvUpdt()
  IF !llUpdRet
    Exit
  ENDIF
  
  SELECT (lcTmpAdj)
  DO CASE
    CASE lcItemType = '0001'
      REPLACE cSession WITH StyInvJl.cSession
    CASE lcItemType = '0002'
      REPLACE cTrn_Seq WITH MatInvJl.cTrn_Seq
  ENDCASE
  
  SELECT (lcWorkFile)
  IF lnCurrRec <> 0
    GOTO lnCurrRec
  ENDIF
ENDSCAN

llContinSave = llUpdRet

*!*************************************************************
*! Name      : lfInvUpdt
*! Developer : Wael M. Abo-Shawarb (WSH)
*! Date      : 12/16/2004
*! Purpose   : Update Inventory Journal and GLDist Files
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpUnLock
*!*************************************************************
FUNCTION lfInvUpdt

PRIVATE lcWareFrom, lcDyeCode
STORE '' TO lcWareFrom, lcDyeCode, lcLinkCode

SELECT (lcWorkFile)

*B607971,1 TMI [Start] if single WH use the default warehous
*lcWareFrom = IIF(llMultiWH, &lcWorkFile..cWareCode, '')
lcWareFrom = IIF(llMultiWH, &lcWorkFile..cWareCode, WAREHOUS.CWARECODE )
*B607971,1 TMI [End  ] 
lcDyeCode  = IIF(llDyeLvl, &lcWorkFile..Dyelot, '')
lcLinkCode = IIF(llMultiWH OR llDyeLvl, IIF(!EMPTY(GL_Link), GL_Link, &lcItem..Link_Code), Link_Code)
lcLinkCode = IIF(llGlLink, IIF(!EMPTY(lcLinkCode), lcLinkCode, 'DEFDEF'), "")
  
*--G/L Array difinition and initialization.
IF llGlLink
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  DO CASE
    CASE lcItemType = '0001'
      laGLDistAr[1,2] = '006'
      laGLDistAr[2,2] = '007'
      STORE 'IP' TO laGLDistAr[1,4],laGLDistAr[2,4]
    CASE lcItemType = '0002'
      laGLDistAr[1,2] = '015'
      laGLDistAr[2,2] = '016'
      STORE 'MP' TO laGLDistAr[1,4],laGLDistAr[2,4]
  ENDCASE
  
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE gdSysDate  TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE lcGLPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcTmpGlDt  TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjLink
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF

*--Adjustment quantity array.
DECLARE laAdjust[9]
DO CASE
  CASE lcItemType = '0001'
    SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laAdjust
  CASE lcItemType = '0002'
    laAdjust[1] = OnHand
ENDCASE

DO CASE
  CASE lcItemType = '0001'
    *--Call the global function for update Style Inventory Control.
    lnRet = gfStyCrl('2',&lcWorkFile..Style,lcWareFrom,lcDyeCode,gdSysDate,'',@laAdjust,lnNewCost,;
                     lcRefer,.F.,lcAdjReason,0,'','',@laGLDistAr,0,"","")
  CASE lcItemType = '0002'
    *--Call the global function for update Material Inventory Control.
    lnRet = gfMatCrl('3',&lcWorkFile..Fabric,&lcWorkFile..Color,;
                     lcWareFrom,lcDyeCode,gdSysDate,gdSysDate,SPACE(6),laAdjust[1],lnNewCost,;
                     lcRefer,lcAdjReason,0,'','',@laGLDistAr)
ENDCASE

*--Return with no save if function return fulse.
IF lnRet = 0
  RETURN .F.
ENDIF  

RETURN .T.

*!**************************************************************************
*! Name      : lfBalanced
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/01/02
*! Purpose   : Check Stock.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*--B804152,1
FUNCTION lfBalanced
PARAMETERS lcType
PRIVATE lnAlias

lnAlias = ALIAS()

IF lcType = 'S'
  IF !USED('StyInvJl')
    =gfOpenFile(gcDataDir+"StyInvJl","StyInvJl","SH")
  ENDIF
  STORE 0 TO laStyStk
  SELECT (lcWorkFile)
  llDyeLvl = ( llDyelot AND &lcItem..cDye_Flg = 'Y' )
  lcKey    = IIF(!llMultiWH,STyle,Style+cWareCode+IIF(llDyeLvl,Dyelot,''))
  *B607971,1 TMI [Start] if no dylot used , update the variable with space
  *lcDyeLot = DyeLot
  lcDyeLot = IIF(llDyeLvl,DyeLot,'')
  *B607971,1 TMI [End  ] 
  IF SEEK(lcKey,'StyInvJl')
    SELECT StyInvJl
    SUM REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) =lcKey;
             FOR IIF(llDyelot AND &lcItem..cDye_Flg = 'Y',cDyeLot = lcDyeLot,.T.);
                    nStk1,nStk2,nStk3,nStk4,nStk5,nStk6,nStk7,nStk8,nTotStk,nStkVal;
         TO laStyStk[1],laStyStk[2],laStyStk[3],laStyStk[4],laStyStk[5],laStyStk[6],laStyStk[7],;
            laStyStk[8],laStyStk[9],laStyStk[10]
    SELECT (lcWorkFile)        
    IF Stk1 <> laStyStk[1] OR;
       Stk2 <> laStyStk[2] OR;
       Stk3 <> laStyStk[3] OR;
       Stk4 <> laStyStk[4] OR; 
       Stk5 <> laStyStk[5] OR;
       Stk6 <> laStyStk[6] OR;
       Stk7 <> laStyStk[7] OR;
       Stk8 <> laStyStk[8] OR;
       TotStk  <> laStyStk[9] OR; 
       nStkVal <> laStyStk[10]
       RETURN .F.
    ENDIF 
  ENDIF
ELSE
  IF !USED('MatInvJl')
    =gfOpenFile(gcDataDir+"MatInvJl","MatInvJl","SH")
  ENDIF
  STORE 0 TO laStyStk
  SELECT (lcWorkFile)
  llDyeLvl = ( llDyelot AND &lcItem..cDye_Flg = 'Y' )
  lcKey    = IIF(!llMultiWH,Fabric+Color,Fabric+Color+cWareCode+IIF(llDyeLvl,Dyelot,''))
  *B607971,1 TMI [Start] if no dylot used , update the variable with space
  *lcDyeLot = DyeLot
  lcDyeLot = IIF(llDyeLvl,DyeLot,'')
  *B607971,1 TMI [End  ] 
  IF SEEK(lcKey,'MatInvJl')
    SELECT MatInvJl
    SUM REST WHILE cfabric+ccolor+cwarecode+cdyelot+crsession+cisession = lcKey;
             FOR IIF(llDyelot AND &lcItem..cDye_Flg = 'Y',cDyeLot = lcDyeLot,.T.);
                    (nReceived-nIssued),nStkVal;
         TO laStyStk[1],laStyStk[2]
    
    SELECT (lcWorkFile)
    IF OnHand <> laStyStk[1] OR;
       nStkVal <> laStyStk[2]
       RETURN .F.
    ENDIF 
  ENDIF

ENDIF
SELECT (lnAlias)