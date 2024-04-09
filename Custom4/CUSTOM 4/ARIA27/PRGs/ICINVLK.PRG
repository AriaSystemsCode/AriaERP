******************************************************************************
*: Program file  : ICINVLK.PRG (E# 301271)
*: Program desc. : Style Invintory And MarkDown.
*: Module        : IC (Inventory Locking)
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls :
*:----------------------------------------------------------------------------
*:        PROCEDURES : lpOpenFiles,lpMainProc,lpTab,lpBackTab.
*:----------------------------------------------------------------------------
*:        FUNCTIONS  : lfKey,lfvWareCode,lfvLDate,lfVNew,lfvBatch,lfBrowBatch,
*:                     lfGetData,lfVScope,lfSelData,lfSelSingl,lfSelMRec,
*:                     lfActsBrow,lfUnTrap,lfTrap,lfvBrowse,lfwBrows,lpSavScr,
*:                     lfInitial,lpShow,lfEditMode,lpDelScr,lfVMkDwn,
*:                     lfVMOk,lfVQty,lfClearRep,lfvPost,lfvWhn,lfWB,lfFillBin
*:                     lfVBin,lfvPostDat,lfvDyelot,lfOpnInfo,lfWDyeBrow,lfAddLTemp
*:                     lfCheckSty,lfAddLin,lfLkStyInv,lfChekBin,lfPostMat,
*:                     lfPostSty,lfGetMem,lfRestMem,lfHoldP,lpClsScr,lfChkUnComS.
*:                     lfCrUnSess,lpvActBar,lfvCont,lfTmpBtchL,lfTempFile,lfGetAdjCd
*:                     lfUpCost.
*:----------------------------------------------------------------------------------
*****************************************************************************
*: Passed Parameters  : lcModulPara:
*:                       '1' ->  # For Style
*:                       '2' ->  # For Material
*****************************************************************************
*:Modification :
*: B#603426,1 SSH 30/01/00 Fix the bug of variable lcObjVal not found
*: B#603426,1 SSH          when post batch.
*: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
*: B603629,1 KHM 06/18/2000 Fix the bug of Subscript out of bound when not
*: B603629,1                linked to GL.
*: B603629,1 KHM 06/18/2000 Fix the bug of Subscript out of bound when not
*: B603629,1                linked to GL.
*: B803433,1 MHM 07/31/2000 Fix the bug of go to top of file when editing 
*: B803433,1                any record .
*: B603983,1 SSE 10/23/2000 Fix bug of replacing 'LK'  Inventory Locking type 
*: B603983,1                Instead of Inventory Adjustment 'IA' 
*: B604322,1 WAB 03/27/2001 Fix the bug of colleting data taked more that 5 minutes
*: B605395,1 BWA 02/13/2002 Fix the bug of not updating the style with the right values.
*: B605395,4 BWA 02/18/2002 Fix the bug of not updating the style when the oldstock = 0.
*: E301851,1 BWA 03/04/2002 Add new feature to collect from excel file.
*: B606068,1 BWA 06/30/2002 Fix the bug of "Window not Define".
*: B605925,1 BWA 07/11/2002 Comment the condition of not using this screen in case multi warehouse and standered cost.
*: B606191,1 BWA 08/14/2002 Fix the bug of Wrong Status Display and arrows is not working to move between fields in the browse.
*: E302011,1 SSE 08/24/2003 Add progress bar on posting data.
*: B037815,1 NNA 02/05/2004 Fix Bug of import from excel sheet has a problem that the program 
*: B037815,1 NNA            accumlate the quantities for the different colors for every style
*: B038829,1 ASH 12/25/2004 Fix bug of wrong saving stock & cost in material locking.
*: C123853,1 MHM 01/15/2005 modification in Custom Bin Location For David Luke
*: B039107,1 EIH 03/03/2005 Fix bug of disable the NewCost field in case of standard cost method .
*: B039660,1 NNA 09/04/2005 Fix the Bin location Problems and transfer all triggers to the Binmain.prg
*: B130752,1 MHM 08/02/2006 Add trigger to disable Bin check box [Start]
*: B130750,1 NNA 04/11/2006 Fix bug that you cann't add a new Bin at the batch file, specially if this bin not assigned
*: B130750,1 NNA            to a style in the Whbinloc File
*: C132193,1 NNA 05/18/2006 Add filtering by Bin Section to the option grid
*: B608128,1 TMI 06/14/2007 set the procedure to the icreport.APP that includes the gfDispRe global function ( T20070614.0003  )
*: B608299,1 NNA 10/01/2007 (T20070713.0001) fix Bug that inventory control programs deal with non inventory styles
*****************************************************************************
PARAMETERS lcModulPara

PRIVATE llStyDye,lcnotes
EXTERNAL ARRAY laData , laDefProc

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*lcScFields ='CBATTYPE,CBATCH,TYPE,content,DATE,DPOSTDATE,CLOCATION,CWARECODE'
lcScFields ='CBATTYPE,cLkBatch,TYPE,content,DATE,DPOSTDATE,CLOCATION,CWARECODE'
*B603596,1 KHM 06/18/2000 (End)

lcModulPara = IIF(TYPE('lcModulPara')='U',SPACE(01),lcModulPara)
STORE .F. TO llBrowse
lcToWare = ''

*E302011,1 SSE 08/24/2003 Variables used for Thermometer. [Begin]
STORE 0 TO lnCountLn , lnRest , lnTotRec
*E302011,1 SSE 08/24/2003 Variables used for Thermometer. [End]

*E301851,1 BWA 03/04/2002 Initialize the wanted variables.[START]
DIMENSION laTFile[1]
lcInport  = gfTempName()
lcRejSty  = gfTempName()
lcStyPic  = gfItemMask("HI")
lcMajPict = gfItemMask("PM")
lnMajLen  = LEN(lcMajPict)
lcSepar   = SUBSTR(lcstypic,lnMAjlen+1,1)
llwarHosM = .F.
lcWare    = SPACE(6)
*E301851,1 BWA 03/04/2002.[END]

*--- Check The Validity Of the parameter.
lcPara   = STR(lcModulPara,1)
lcParamStr = "12"
lcFiles    = ''
IF (TYPE('lcPara') <> "C" OR EMPTY(lcPara) OR !(lcPara $ lcParamStr))
  WAIT WINDOW 'Unable to run program Please Check parameter..!'
  RETURN
ENDIF

*B603629,1 KHM 06/18/2000 (Begin) Making the laAdjCode array with 2 dimension.
*DIMENSION laAdjCode[1],laCodInfo[1,10]
DIMENSION laAdjCode[1,2],laCodInfo[1,10]
*B603629,1 KHM 06/18/2000 (End)

STORE ' ' TO laAdjCode,laCodInfo
llBrowDye  = .F.
llAdjYNo   = .F.
llStyDye   = .F.
lcAdjReason = ''
lnLoc      = 1
lnAdjCode  = 1
lcGLFYear  = SPACE(04)      && Fiscal Year
lcGLPeriod = SPACE(02)      && Period
gcHost     = 'ICINVML0'
lcStatColor= gcObjColor
lctmpGlDis = gfTempName()
lcDetLin   = gfTempName()
lcLocExpr  = ''
lnTempCst  = 0
lcDefAdjCd = ''
*--- Variable to indicate if we are run this program for material.
llMatModul = IIF(STR(lcModulPara,1) = '2',.T.,.F.)
lcKey      = IIF(llMatModul,'M','S')
DECLARE laScrMode[4]
IF !llMatModul
  FOR I = 1 TO 8
    Z = STR (I,1)
    lcSz&Z = ''
  ENDFOR
ENDIF
*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*STORE '' TO  lcExpr,XVENDOR,lcRPVendor,lcRPFrmFab,lcRPToFab,lcRPTitle,lcRPSortBy,;
             lcRPItmTyp,lcRPForm,LFAB,HFAB,XTITLE,XSORT,XITEM_TYP,XFILTER,;
             lcSLoc,lcRpDoMimP,lcRpDoMimD,lcMy,lcStyle,;
             lcStat,lcDyelot,lcOldDye,lcReason,m.cBatch,lcGlSess
STORE '' TO  lcExpr,XVENDOR,lcRPVendor,lcRPFrmFab,lcRPToFab,lcRPTitle,lcRPSortBy,;
             lcRPItmTyp,lcRPForm,LFAB,HFAB,XTITLE,XSORT,XITEM_TYP,XFILTER,;
             lcSLoc,lcRpDoMimP,lcRpDoMimD,lcMy,lcStyle,;
             lcStat,lcDyelot,lcOldDye,lcReason,m.cLkBatch,lcGlSess
*B603596,1 KHM 06/18/2000 (End)

DIME laRpSource[1,1]
DIME laRpTarget[1,1]

lcStyPic   = gfItemMask('HI')
STORE 0 TO lnTotStk,lnCost,lnColorLen,lnClrPo,lnLNewTot,lnMCost,lnOldCost

*B604322,1 - WAB (Start) get the style title  
lcStyHdr = gfItemMask('HM')
*B604322,1 - WAB (End) 
*--Uncomplte session variables(UNCSS).
DECLARE laVariables[4]
lcUnCmPgId = PADR('INVLOCK_'+lcKey,10)
lnSessNo   = gnProgCopy
lcSession  = ' '
laVariables= ' '
lcPostScVar= '' 
llContinue = .F.
llGoAndChk = .T.
*-- Variable to tell if are we going to check the existance of
*-- any incomplete session in the screen's setup or not.
llChkUnCom = .T.
*--Uncomplete session variables.
laVariables[1] = 'laScrMode[2]'
laVariables[2] = 'laScrMode[3]'
laVariables[3] = 'laScrMode[4]'
laVariables[4] = 'lcPostScVar'
IF !gfSetup()
  RETURN
ENDIF

*C132193,1 NNA 05/18/2006 (BEGIN) initialize two custom arrays
IF ASCAN(laEvntTrig,PADR("FILLSEC",10)) <> 0 
  DIME laScSource[1,1]
  DIME laScTarget[1,1]
ENDIF
*C132193,1 NNA (END)

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*lcBrFields  = "cbatch :H='Batch',date :H='Lock Date',dpostdate :H='Post Date',cwarecode :H='Location',content :H='Notes'"
lcBrFields  = "cLkBatch :H='Batch',date :H='Lock Date',dpostdate :H='Post Date',cwarecode :H='Location',content :H='Notes'"
*B603596,1 KHM 06/18/2000 (En)

*--- Array to Hold opend file.
DIMENSION laHdFlt[1], laFxFlt[1], laVrFlt[1],laBrFldFlt[1,1]

*--- Control Panel part
llNoShow = .F.
llAlowNew = .T.
DIMENSION laPanelObj[2,3]    && Array to add new buttons to the control panel
STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbScop'
laPanelObj[1,2] = gcBmpHome + 'SCOPE.BMP'
laPanelObj[1,3] = 'VALID lfvScope() MESSAGE "Scope"'
DECLARE laMloc[1]
laDefProc[9]  = .F.  && Disable the control panel save proc.  (lpSavScr)
laDefProc[7]  = .F.  && Disable the control panel delete proc.(lpDelScr)
laDefProc[10] = .F.  && Disable the control panel close proc. (lpClsScr)

*-- Get the setup variables from the memory files.
lcMtBrowTt = IIF(llMatModul,'Selected Items', 'Selected '+lcStyPic)
lcMDBrowTt = IIF(llMatModul,'Detail Items', 'Detail '+lcStyPic)
DIMENSION laSetups[5,2]
laSetups[1,1] = IIF(llMatModul,'M_MATCSTMT','M_COST_MET')
laSetups[2,1] = 'M_WAREHOUS'
laSetups[3,1] = IIF(llMatModul,'M_MATDYE  ','M_DYELOT')
laSetups[4,1] = 'M_LINK_GL'
laSetups[5,1] = 'M_WARELOC'
=gfGetMemVar(@laSetups,gcAct_Comp)

llAvrCost = IIF (laSetups[1,2] = 'A' , .T. , .F.)
llWareHus = IIF( laSetups[2,2] = 'Y' , .T. , .F.)
llDyelot  = IIF( laSetups[3,2] = 'Y' , .T. , .F.)
llLinkGL  = IIF( laSetups[4,2] = 'Y' , .T. , .F.)
llLoc     = IIF( laSetups[5,2] = 'Y' , .T. , .F.)

IF llMatModule
  lcCostMeth = gfGetMemVar('M_MATCSTMT')
ELSE
  lcCostMeth = gfGetMemVar('M_COST_MET')
ENDIF
lnCurrRec = ''

*B605925,1 BWA 07/11/2002 Comment the condition of not using this screen in case multi warehouse and standered cost.[START]
*IF llWareHus .AND. !llAvrCost
*   *--- 'Markdown Inventory is not available when the system is setup to use
*   *---  multiple warehouse and standard cost method.'
*   =gfModalGen('TRM42174B00000','DIALOG')
*  RETURN
*ENDIF
*B605925,1 BWA 07/11/2002.[END]

IF llLinkGL
  laCodInfo[1,01] = "CADJREASON"
  laCodInfo[1,02] = "laAdjCode"
  laCodInfo[1,03] = "lnAdjCode"
  laCodInfo[1,04] = ""
  laCodInfo[1,05] = .F.
  laCodInfo[1,06] = .F.
  laCodInfo[1,10] = "cAdjReason"
ENDIF 

*--Get needed temprorary files names.
lcTmpQuery= gfTempName()
IF llLinkGL
  GLDTemp  = gfTempName()
ENDIF
lcGLFYear = SPACE(04)         && Fiscal Year
lcGLPeriod= SPACE(02)         && Period
ldDate   = DATE()             && Initialize Date variable.
*--Open databases files needed for each option.
DO lpOpenFiles
lcBatLin = gfTempName()
lcBatHdr = gfTempName()
lcStyInd = gfTempName()
lcBatInd = gfTempName()
IF llLinkGL
  =gfwCodePop(@laCodInfo, "CADJREASON", "D") 
  lcAReason = laAdjCode[lnAdjCode,2]
  =gfwCodePop(@laCodInfo, "CADJREASON", "L")
ENDIF

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*lcScFields = 'CBATTYPE,CBATCH,TYPE,content,DATE,DPOSTDATE,CLOCATION,CWARECODE'
lcScFields = 'CBATTYPE,cLkBatch,TYPE,content,DATE,DPOSTDATE,CLOCATION,CWARECODE'
*B603596,1 KHM 06/18/2000 (End)

SELECT MDINVNTH
SCATTER FIELDS &lcScFields MEMO TO laData BLANK
SET KEY TO lcKey
*--Call needed procedure.
DO lpMainProc
SELECT MDINVNTH
SET KEY TO 
RETURN

*****************************************************************************
*: Program file  : lpOpenFiles.PRG
*: Program desc. : Open nessasary Files.
*:      Developer: Ahmed Salah Shalaby -(SSH)
*:           Date: 01/07/99
*****************************************************************************
*! Called from :  Procedures : ICINVLK.PRG
*****************************************************************************
*: Passed Parameters  : lcPara
*****************************************************************************
*:EXAMPLE : DO lpOpenFiles
*****************************************************************************
PROCEDURE lpOpenFiles

=gfOpenFile(gcDataDir+'MDINVNTH' ,'MDINVNTH','SH')

=gfOpenFile(gcDataDir+'UNCMSESS' ,'TRANS','SH')

=gfOpenFile(gcDataDir+'GLDIST' ,'','SH')
=gfOpenFile(gcDataDir+'MDINVNTL' ,'MDINVNTL','SH')
=gfOpenFile(gcDataDir+'WareHous' ,'WareHous','SH')
=gfOpenFile(gcDataDir+'Scale' ,'Scale','SH')
=gfOpenFile(gcDataDir+'CODES' ,'CODES','SH')
SELECT Codes
SET ORDER TO TAG cCode_no
=SEEK("D"+'CADJREASON','CODES')
lcDefAdjCd = Codes.Ccode_No 
IF llLoc
  =gfOpenFile(gcDataDir+'WhsLoc',gcDataDir+'WhsLoc','SH')
ENDIF
IF llMatModul
  =gfOpenFile(gcDataDir+'FABRIC' ,'FABRIC','SH')
  =gfOpenFile(gcDataDir+'FabDye' ,'FabDye','SH')
  =gfOpenFile(gcDataDir+'matinvjl' ,'matinvjl','SH')
ELSE
  =gfOpenFile(gcDataDir+'Style' ,'Style','SH')
  =gfOpenFile(gcDataDir+'StyDye' ,'StyDye','SH')
  =gfOpenFile(gcDataDir+'StyInvJl' ,'StyInvJl','SH')
ENDIF
RETURN

*!*************************************************************
*! Name      : lpMainProc
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : LOCK THE INVENTORY For Both Multi and 
*!           : Single Warehouse.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpMainProc
*!*************************************************************
*!
PROCEDURE lpMainProc


DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP

DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Post Batch'  &&SKIP FOR !(laScrMode[2] AND LADATA[3]='M')
ON SELECTION POPUP _OPTIONPOP DO lpvActBar
DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 2 OF _OPTIONPOP PROMPT 'Hold Batch'
ON SELECTION POPUP _OPTIONPOP DO lpvActBar

laPanelObj[2,1] = 'pbPost'
laPanelObj[2,2] = gcBmpHome+'GENERAT.BMP'
laPanelObj[2,3] = [VALID lfvPost() MESSAGE 'Post Batch']

DO gcScrDir+("ICINVLK.SPR")
POP KEY
RELEASE WINDOW (lcMtBrowTt)
RELEASE WINDOW (lcMDBrowTt)
RELEASE PAD _Option OF _MSYSMENU

RELEASE BAR 099 OF P01PU01
RELEASE BAR 100 OF P01PU01

*!*************************************************************
*! Name      : lfKey
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Screen ICINVLK When Function.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfKey()
*!*************************************************************
*!
FUNCTION lfKey

*-- Activate Browse window
*-- Define a menu bar with a short-cut key for browse window 
*-- in the program just to be able to activate the browse window
*-- using its menu bar short-cut key.
ladata[5] = IIF(ladata[5] = {},gdSysDate,ladata[5])
IF llWareHus
  SHOW GET ladata[8]
  SHOW GET ladata[5]
ELSE
  SELECT WareHous
  GOTO TOP

  *E301851,1 BWA 03/04/2002 Replace the warehouse field with the warehouse from the imported data.[START]
  *ladata[8] = cWareCode
  IF llwarHosM AND RECCOUNT(lctmpquery) > 0
    ladata[8] = lcWare
  ELSE
    ladata[8] = cWareCode
  ENDIF
  *E301851,1 BWA 03/04/2002 .[END]

  SHOW GET ladata[8] DISABLE
ENDIF
PUSH KEY
DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 100 OF P01PU01 PROMPT lcMtBrowTt KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcMtBrowTt)

*!*************************************************************
*! Name      : lfKey
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Screen ICINVLK When Function.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfKey()
*!*************************************************************
*!
FUNCTION lfKey1

*-- Activate Browse window
*-- Define a menu bar with a short-cut key for browse window 
*-- in the program just to be able to activate the browse window
*-- using its menu bar short-cut key.
IF ALEN(laMloc) < 2
  SHOW GET lnBin DISABLE
ENDIF
IF laData[3] = 'P'
  SHOW GET pbOk DISABLE
  SHOW GET lnBin DISABLE
  IF llMatModule
    SHOW GET lnMMCost  DISABLE
    SHOW GET lnMNewTot DISABLE
    SHOW GET lcMReason DISABLE
    SHOW GET lnAdjCode DISABLE
  ELSE
    SHOW GET lnMCost   DISABLE
    SHOW GET lcReason  DISABLE
    SHOW GET lnAdjCode DISABLE
    FOR lnInd = 1 TO 8
      lcInd = STR(lnInd,1)
      SHOW GET lnStk&lcInd DISABLE
    ENDFOR
  ENDIF
ENDIF
PUSH KEY
DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 100 OF P01PU01 PROMPT lcMDBrowTt KEY ALT+D
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcMDBrowTt)

*B130752,1 MHM 08/02/2006 Add trigger to disable Bin check box [Start]
IF ASCAN(laEvntTrig,PADR("DISBINLC",10)) <> 0 
  =gfDoTriger("ICINVLK",PADR("DISBINLC",10))
ENDIF
*B130752,1 MHM [End]

*!*************************************************************
*! Name      : lfvWareCode
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : WareHouse Code Valid Function..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfvWareCode()
*!*************************************************************
*!
FUNCTION lfvWareCode

laData[8] = PADR(laData[8],6)
IF EMPTY(ladata[8]) OR !SEEK(ladata[8],'WareHous')
  ladata[8] = gfBrowWare(.F.)
*  SHOW GET ladata[8] DISABLE
ENDIF

*!*************************************************************
*! Name      : lfvLDate
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Lock Date Valid Function..
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfvLDate()
*!*************************************************************
*!
FUNCTION lfvLDate

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcBatHdr)
REPLACE Date WITH laData[5]
SELECT(lnAlias)
IF ladata[5] = {}
  _CUROBJ = OBJNUM(pbCls)
ELSE
  SHOW GET ladata[5] ENABLE
ENDIF

*!*************************************************************
*! Name      : lfVNew
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : New button valid function.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfVNew()
*!*************************************************************
*!
FUNCTION lfVNew

SHOW GET ladata[5] ENABLE
SHOW GET ladata[6] DISABLE
SHOW GET laData[2] DISABLE
SHOW GET ibBatch   DISABLE
SHOW GET pbPost    DISABLE
SHOW GET pbMkDwm   DISABLE
*: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
SHOW GET pbMkZer DISABLE
*: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
laData[3] = 'L'
lcStatColor = "RGB(255,255,255,0,128,0)"
IF llWareHus
   SHOW GET ladata[8] ENABLE
  _CUROBJ = OBJNUM(ladata[8])
ELSE
   SHOW GET ladata[8] DISABLE
  _CUROBJ = OBJNUM(ladata[5])
ENDIF
SELECT(lcTmpQuery)
IF !llContinue
  laData[2] = SPACE(6)
  laData[8] = SPACE(6)
  ladata[6] = {}
  laData[3] = 'L'
  DELETE ALL
  SELECT (lcBatLin)
  DELETE ALL
  SELECT (lcBatHdr)
  DELETE ALL
ENDIF
SELECT(lcTmpQuery)
GOTO TOP
=lfActsBrow()

*!*************************************************************
*! Name      : lfvBatch
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : New button valid function.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfvlcBatch()
*!*************************************************************
*!
FUNCTION lfvBatch

*E302011,1 SSE 08/24/2003 Count total records of Thermometer. [Begin]
lnTotRec = 0
*E302011,1 SSE 08/24/2003 Count total records of Thermometer. [End]

IF EMPTY(laData[2])
  lcStat  = ''
  lcStatColor = gcObjColor
  =lfRefresh('icinvlk1')
  RETURN
ENDIF

*B605395,1 BWA 02/13/2002 Zap the temporary files when run the screen for the 2nd time.[START]
PRIVATE lcALias

lcALias = SELECT(0)
*B606068,1 BWA 06/30/2002 Fix the bug of "Window not Define".[START]
*SELECT (lcTmpQuery)
*ZAP
IF RECCOUNT(lcTmpQuery) > 0
  SELECT (lcTmpQuery)
  ZAP
ENDIF
*B606068,1 BWA 06/30/2002.[END]

SELECT (lcBatLin)
ZAP
SELECT (lcBatHdr)
ZAP

SELECT(lcALias)
*B605395,1 BWA 02/13/2002.[END]

IF SEEK(lcKey+laData[2],'MDINVNTH')
  SELECT MDINVNTH
  ladata[5] = Date
  ladata[6] = DPostDate
  ladata[8] = cWareCode
  ladata[4] = content
  SHOW GET ladata[8] DISABLE
  SHOW GET ladata[5]
  SHOW GET ladata[6] DISABLE
  =lfGetData()
  IF MDINVNTH.Type <> 'P'
    lcStat  = 'Open'
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    SHOW GET pbMkZer ENABLE
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    IF MDINVNTH.Type = 'L'
      SHOW GET pbDlt       ENABLE
      SHOW GET pbPost      DISABLE
      SHOW GET ladata[6]   DISABLE
    ELSE
      ladata[6] = gdSysDate
      SHOW GET ladata[6] ENABLE
      SHOW GET pbDlt     DISABLE
    ENDIF
    lcStatColor = "RGB(255,255,255,0,128,0)"
  ELSE
    lcStat  = 'Posted'
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    SHOW GET pbMkZer DISABLE
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    lcStatColor = "RGB(255,255,255,255,0,0)"
  ENDIF
  IF MDINVNTH.Type = 'H'
    lcStatColor = "RGB(255,255,255,255,0,0)"
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    SHOW GET pbMkZer DISABLE
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
  ENDIF
  =lfRefresh('icinvlk1')
  SHOW GET pbCpNew   DISABLE
  SHOW GET pbScop    DISABLE
  SHOW GET laData[2] DISABLE
  SHOW GET ibBatch  DISABLE
  SHOW GET pbSav     DISABLE
ENDIF

IF (!EMPTY(laData[2]) AND !SEEK(lcKey+laData[2],'MDINVNTH'))
  IF !lfBrowBatch()
    laData[2] =SPACE(6)
    SHOW GET laData[2]
    RETURN
  ELSE
    SELECT MDINVNTH

    *B606191,1 BWA 08/14/2002 Fix the bug of Wrong Status Display.[START]
    ladata[3] = TYPE
    *B606191,1 BWA 08/14/2002.[END]

    ladata[5] = Date
    ladata[6] = DPostDate
    ladata[8] = cWareCode
    ladata[4] = content
    SHOW GET ladata[8]
    SHOW GET ladata[5]
    SHOW GET ladata[6]   DISABLE
    IF MDINVNTH.Type <> 'P'
      lcStat  = 'Open'
      *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
      SHOW GET pbMkZer ENABLE
      *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
      lcStatColor = "RGB(255,255,255,0,128,0)"
      IF MDINVNTH.Type = 'L'
        SHOW GET pbDlt  ENABLE
        SHOW GET pbPost DISABLE
        SHOW GET ladata[6]   DISABLE        
      ELSE
        ladata[6] = gdSysDate
        SHOW GET ladata[6]    ENABLE
        SHOW GET pbDlt   DISABLE
        *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
        SHOW GET pbMkZer DISABLE
        *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
      ENDIF
    ELSE
      lcStat  = 'Posted'
      lcStatColor = "RGB(255,255,255,255,0,0)"
    ENDIF
    IF MDINVNTH.Type = 'H'
      lcStatColor = "RGB(255,255,255,255,0,0)"
      *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
      SHOW GET pbMkZer DISABLE
      *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    ENDIF
    =lfRefresh('icinvlk1')
    SHOW GET pbCpNew DISABLE
    SHOW GET laData[2] DISABLE
    SHOW GET ibBatch  DISABLE
    =lfGetData()
  ENDIF
ENDIF
IF EMPTY(laData[2])
  _CUROBJ = OBJNUM(pbCpNew)
ELSE
  IF MDINVNTH.Type <> 'P'       && ! Posted
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    SHOW GET pbMkZer ENABLE
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    IF MDINVNTH.Type = 'L'      && Locked
      SHOW GET pbDlt ENABLE
      SHOW GET pbPost DISABLE
      SHOW GET ladata[6]   DISABLE      
    ELSE  && Marked Down
      SHOW GET pbDlt     DISABLE
      SHOW GET ladata[6] ENABLE
    ENDIF
    SHOW GET pbCpNew   DISABLE
    SHOW GET laData[2] DISABLE
    SHOW GET ibBatch  DISABLE
    lcStat  = 'Open'
    lcStatColor = "RGB(255,255,255,0,128,0)"
    =lfRefresh('icinvlk1')
  ELSE     && Posted
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    SHOW GET pbMkZer DISABLE
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    lcStatColor = "RGB(255,255,255,255,0,0)"
    lcStat  = 'Posted'
    ladata[6] = gdSysDate
    =lfRefresh('icinvlk1')
  ENDIF
  IF MDINVNTH.Type = 'H'
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    SHOW GET pbMkZer DISABLE
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    lcStatColor = "RGB(255,255,255,255,0,0)"
  ENDIF
  laScrMode = .F.
  laScrMode[2] = .T.
  SHOW GETS
ENDIF
SHOW GET pbSlct ENABLE

*!*************************************************************
*! Name      : lfBrowBatch
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Brows for batch.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfBrowBatch()
*!*************************************************************
*!
FUNCTION lfBrowBatch

SELECT MDINVNTH
SET ORDER TO TAG MDINVNTH
lcFile_Ttl  = 'Select Batch'
lcPbrowtitl = 'Batch No.'

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*lcBrField1  = "cBatch    :12   :H = 'Batch No.',"+;
              "Date      :12   :H = 'Lock Date',"+;
              "DPOSTDATE :12   :H = 'Post Date',"+;
              "CWareCode :15   :H = 'Location',"+;
              "lcdesc =IIF(SEEK(CWareCode,'WareHous'),WareHous.cDesc,'')  :35   :H = 'Location Desc'"

lcBrField1  = "cLkBatch    :12   :H = 'Batch No.',"+;
              "Date      :12   :H = 'Lock Date',"+;
              "DPOSTDATE :12   :H = 'Post Date',"+;
              "CWareCode :15   :H = 'Location',"+;
              "lcdesc =IIF(SEEK(CWareCode,'WareHous'),WareHous.cDesc,'')  :35   :H = 'Location Desc'"
*B603596,1 KHM 06/18/2000 (End)

DIMENSION latemp[1]
latemp = ''
IF SEEK(lcKey)
  *B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
  *llReturn=ARIABROW([lcKey],lcPbrowtitl, gnbrfsrow1, gnbrfscol1,;
                    gnbrfsrow2, gnbrfscol2,'','',;
                    'MDINVNTH.cBatch','laTemp')
  llReturn=ARIABROW([lcKey],lcPbrowtitl, gnbrfsrow1, gnbrfscol1,;
                    gnbrfsrow2, gnbrfscol2,'','',;
                    'MDINVNTH.cLkBatch','laTemp')
  *B603596,1 KHM 06/18/2000 (End)
  
  laData[2] = latemp[1]
  IF EMPTY(laData[2])
    RETURN(.F.)
  ENDIF
ELSE
  =gfModalGen('TRM00342B00000','DIALOG' )
  laData[2] = SPACE(6)
  RETURN(.F.)
ENDIF

*!*************************************************************
*! Name      : lfGetData
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function to get data.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfGetData()
*!*************************************************************
*!
FUNCTION lfGetData

SELECT MDINVNTH
=SEEK(lcKey+laData[2])
SCAT MEMVAR MEMO
INSERT INTO (lcBatHdr) FROM MEMVAR
SELECT MDINVNTL
SET ORDER TO MDINVNTL
=SEEK(lcKey+laData[2])

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*SCAN REST WHILE cBatType+cBAtch = lcKey+laData[2]  &&;
            FOR IIF(llDyelot AND lfStyDye(Style,Color),!EMPTY(Dyelot),.T.)
SCAN REST WHILE cBatType+cLkBatch = lcKey+laData[2]  &&;
            FOR IIF(llDyelot AND lfStyDye(Style,Color),!EMPTY(Dyelot),.T.)
*B603596,1 KHM 06/18/2000 (End)
  *B604322,1 - WAB (Start) - display a wait window while colecting data
  WAIT WINDOW 'Selecting ' + IIF(llMatModul,'items',lcStyHdr)+ ' : ' + Style NOWAIT
  *B604322,1 - WAB (End) 
  
  *E302011,1 SSE 08/24/2003 Count total records of Thermometer. [Begin]
  lnTotRec = lnTotRec + 1
  *E302011,1 SSE 08/24/2003 Count total records of Thermometer. [End]
  
  SCAT MEMVAR MEMO
  IF  (llDyelot AND lfStyDye(Style,Color) AND !EMPTY(Dyelot)) OR !llDyelot ; 
    .OR. (llDyelot AND !lfStyDye(Style,Color))
  
    IF llMatModul
      m.FabDesc = IIF(SEEK(ALLTRIM(m.Style),'FABRIC'),FABRIC.Desc,'')
      m.CLRDESC = gfCodDes(COLOR,'COLOR')
    ELSE
      m.StyDesc = IIF(SEEK(m.Style,'Style'),Style.Desc1,'')
    ENDIF
    m.ITEM    = IIF(llMatModul,STYLE,'')
    *B038829,1 ASH 12/25/2004 (Begin) Use the new field onhand instead of totstk in case of material.
    *m.Stock   = TotStk
    m.Stock   = IIF(llMatModul,OnHand,TotStk)
    *B038829,1 ASH 12/25/2004 (End)
    m.Cost    = COST
    *B038829,1 ASH 12/25/2004 (Begin) Use the new field oldqty instead of OldTotStk in case of material.
    *m.OStock  = OldTotStk
    m.OStock  = IIF(llMatModul,OldQty,OldTotStk)
    *B038829,1 ASH 12/25/2004 (End)
    m.OCost   = OldCOST

    *C123853,1 MHM 01/15/2005 assign bin location For David Luke[Start]

    *B039660,1 NNA 09/04/2005 (Begin) Check also for (use Bin Location Yes/No) to decrease the codes lines in the Binmain.prg
    *B039660,1 NNA            and not writing the Else satement again
    *IF ASCAN(laEvntTrig,PADR("DLASSBIN",10)) <> 0 
    IF ASCAN(laEvntTrig,PADR("DLASSBIN",10)) <> 0 AND gfGetMemVar('M_DLUSEBIN',gcAct_Comp)
    *B039660,1 NNA (End)

      =gfDoTriger("ICINVLK",PADR("DLASSBIN",10))
    ELSE
    *C123853,1 MHM 01/15/2005 [End]
    
      IF SEEK(m.STYLE+m.Color,lcTmpQuery)
        SELECT (lcTmpQuery)
        REPLACE Stock WITH Stock + m.Stock;
                oStock WITH oStock + m.oStock
      ELSE
        INSERT INTO (lcTmpQuery) FROM MEMVAR
      ENDIF
    *C123853,1 MHM 01/15/2005 assign bin location For David Luke[Start]
    ENDIF
    *C123853,1 MHM 01/15/2005 [End]
    
  ENDIF
  INSERT INTO (lcBatLin) FROM MEMVAR
ENDSCAN
*B604322,1 - WAB (Start) 
WAIT CLEAR
*B604322,1 - WAB (end) 
SELECT(lcTmpQuery)
GOTO TOP
=lfActsBrow()

*!*************************************************************
*! Name      : lfVScope
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Functio to valid scope but on menu bar.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfVScope()
*!*************************************************************
*!
FUNCTION lfVScope

DIMENSION laScopExpr[1,2] , laNormExpr[1,2]
laScopExpr = ''
laNormExpr = ''
ON KEY LABEL ALT+B
STORE .F. TO lcExpr1,lcExpr2
*--- Start Call Option Grid
lcOldPrc = SET("PROCEDURE")
IF llMatModul
  SET PROCEDURE TO gcrepHome+"MA\MAMATLST.FXP"
  lcExpr = gfOpGrid('MAMATLST' , .T.)  && Run selection grid.
  LFAB = lcRPFrmFab
  HFAB = lcRPToFab
  XVENDOR   = lcRPVendor
  LFAB      = lcRPFrmFab
  HFAB      = lcRPToFab
  XTITLE    = lcRPTitle
  XSORT     = lcRPSortBy
  XITEM_TYP = lcRPItmTyp
  XFILTER = ''
  XFILTER = XFILTER + IIF(!EMPTY(XVENDOR),'FABRIC.VENDOR=XVENDOR','')
  DO CASE
    CASE !EMPTY(xFilter) AND !EMPTY(xItem_Typ)
      xFilter = xFilter + ".AND. FABRIC.Item_Type = xItem_Typ"
    CASE EMPTY(xFilter) AND !EMPTY(xItem_Typ)
      xFilter = xFilter + " FABRIC.Item_Type = xItem_Typ"
  ENDCASE
  *xFilter = xFilter + IIF(EMPTY(xFilter) ,"",IIF(EMPTY(xItem_Typ),"",".AND. FABRIC.Item_Type = xItem_Typ"))
  IF !EMPTY(lcRPFrmFab) AND !EMPTY(lcRPToFab)
    IF EMPTY(XFILTER)
      XFILTER = XFILTER +' FABRIC>=LFAB .AND. FABRIC<=HFAB'
    ELSE
      XFILTER = XFILTER +' .AND. FABRIC>=LFAB .AND. FABRIC<=HFAB'
    ENDIF
  ENDIF
  IF !EMPTY(XFILTER)
    lcExpr = IIF(TYPE('lcExpr') = "L" AND !lcExpr,'.F.',lcExpr)
    lcExpr = lcExpr + ".AND." + XFILTER
  ENDIF  
  SET PROCEDURE TO &lcOldPrc
ELSE
  SET PROCEDURE TO gcrepHome+"IC\ICSTYLST.FXP"
  lcExpr = gfOpGrid('ICSTYLST' , .T.)  && Run selection grid.
  
  IF lcRpDomImp <>  'B' AND !(TYPE('lcExpr') = "L" AND !lcExpr)
    lcExpr = lcExpr + 'AND' + IIF(lcRpDomImp = 'I' , ' !(STYLE.Make)' , ' STYLE.Make' )
  ENDIF
  SET PROCEDURE TO &lcOldPrc
ENDIF
lcExpr = IIF(TYPE('lcExpr') = "L" AND !lcExpr,'.F.',lcExpr)

*B608299,1 NNA 10/01/2007 (Begin) check only for inventory styles
lcExpr = lcExpr + ' AND STYLE.LINVSTY' 
*B608299,1 NNA (End)

=lfSelData()

*!*************************************************************
*! Name      : lfSelData
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Select Data.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfSelData()
*!*************************************************************
*!
FUNCTION lfSelData

*E301851,1 BWA 03/04/2002 Add new parameter to differentiate between the main code and the Import code.[START]
PARAMETERS llLogic
*E301851,1 BWA 03/04/2002.[END]

SELECT(lcTmpQuery)
DELETE ALL
SELECT (lcBatLin)
DELETE ALL
SELECT (lcBatHdr)
DELETE ALL
=lfSelMRec()
SELECT(lcTmpQuery)
GOTO TOP
IF EOF()
  =gfModalGen('TRM00342B00000','DIALOG' )
  SHOW GET pbSav DISABLE
  SHOW GET pbMkDwm   DISABLE
  *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
  SHOW GET pbMkZer DISABLE
  *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
  SHOW GET ladata[8] ENABLE
ELSE
  SHOW GET ladata[8] DISABLE
  SHOW GET pbSav ENABLE
  SHOW GET pbMkDwm   ENABLE
  *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
  SHOW GET pbMkZer ENABLE
  *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
ENDIF
SELECT (lcBatLin)
IF llMatModule
  LOCATE FOR IIF(llDyelot AND lfStyDye(SUBSTR(Style,1,7),Color),!EMPTY(&lcBatLin..DYELOT),.T.)
ELSE
  LOCATE FOR IIF(llDyelot AND lfStyDye(Style ,Color),!EMPTY(&lcBatLin..DYELOT),.T.)  
ENDIF
IF !FOUND()
  SHOW GET pbSav DISABLE
ELSE
  SHOW GET pbSav ENABLE
ENDIF
SELECT(lcTmpQuery)
GOTO TOP
=lfActsBrow()
RETURN
*-- end of lfvScope.

*!*************************************************************
*! Name      : lfSelMRec
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Select Style in multi wareHouse.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfSelMRec()
*!*************************************************************
FUNCTION lfSelMRec
IF llMatModul
  SELECT FABDYE
  SET ORDER TO FABDYE
  SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE
  GOTO TOP
  m.Color = SPACE(06)
ELSE
  SELECT STYLE
  SET ORDER TO STYLE 
  SELECT STYDYE
  SET ORDER TO STYDYE
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  GOTO TOP
  m.Color = SPACE(06)
ENDIF
WAIT WINDOW "Selecting Record... Please Wait" NOWAIT

*E301851,1 BWA 03/04/2002 Use a new code for the imported data.[START]
*SCAN FOR &lcExpr AND IIF(llWareHus,CwareCode = ladata[8] ,.T.)
*  SCAT MEMVAR MEMO
*  IF llMatModul
*    m.FabDesc = FABRIC.Desc
*    m.CLRDESC = gfCodDes(COLOR,'COLOR')
*  ELSE
*    m.StyDesc = Style.Desc1
*  ENDIF
*  DO CASE
*    CASE  lcCostMeth = 'S'  && Standard
*      m.Cost    = IIF(llMatModul,FABRIC.CostUse,STYLE.TotCost)
*      m.oCost   = IIF(llMatModul,FABRIC.CostUse,STYLE.TotCost)
*    OTHERWISE       && 'A' Average
*      m.Cost    = IIF(llMAtModul,nFAve_Cost,Ave_Cost)
*      m.oCost   = IIF(llMAtModul,nFAve_Cost,Ave_Cost)
*  ENDCASE
*  m.ITEM    = IIF(llMatModul,Fabric,'')
*  m.STOCK   = IIF(llMatModul,OnHand,TotStk)
*  m.oSTOCK  = IIF(llMatModul,OnHand,TotStk)
*  m.nStkVal = nStkVal
*  IF lfChekBin()
*    SELECT MDINVNTL
*    SET ORDER TO MDINVNTLS
*    WAIT WINDOW "Selecting Record... Please Wait" NOWAIT
*    =lfAddLTemp(IIF(llMatModul,PADR(m.ITEM,19),m.Style),m.Color)
*  ENDIF
*ENDSCAN

IF llLogic
  SELECT (lcInport)
  SCAN FOR RECNO() >=5 AND !EMPTY(COLOR)
    SELECT STYDYE
    IF SEEK( EVAL(lcInport+'.STYLE') + lcSepar + EVAL(lcInport+'.COLOR') + lcWare , 'STYDYE')
      *B037815,1 NNA 02/05/2004 (Begin) scan for style and color
      *SCAN REST WHILE STYLE + CWARECODE + DYELOT = EVAL(lcInport+'.STYLE') AND IIF(llWareHus,CwareCode = ladata[8] ,.T.)
	  SCAN REST WHILE STYLE + CWARECODE + DYELOT = EVAL(lcInport+'.STYLE') + lcSepar + EVAL(lcInport+'.COLOR') AND IIF(llWareHus,CwareCode = ladata[8] ,.T.)
      *B037815,1 NNA (End)
        SCATTER MEMVAR MEMO
        STORE 0 TO lnTotStk
        FOR lnLop = 1 To 8
          lcStk  = 'Stk' + ALLTRIM(STR(lnLop))
          m.&lcStk = &lcInport..&lcStk
          lnTotStk = lnTotStk + &lcInport..&lcStk
        ENDFOR
        m.TotStk = lnTotStk
        m.StyDesc = Style.Desc1
        DO CASE
          CASE  lcCostMeth = 'S'  && Standard
            m.Cost    = STYLE.TotCost
            m.oCost   = STYLE.TotCost
          OTHERWISE       && 'A' Average
            m.Cost    = Ave_Cost
            m.oCost   = Ave_Cost
        ENDCASE
        m.ITEM    = ''
        m.STOCK   = lnTotStk 
        m.oSTOCK  = TotStk
        m.nStkVal = nStkVal
        *ash1
        *m.Style = &lcInport..Style
        *m.Color = &lcInport..Color
        m.Cadd_user = gcUserName
        m.Dadd_date = DATE()
        m.Cadd_time = TIME()
        IF lfChekBin()
          SELECT MDINVNTL
          SET ORDER TO MDINVNTLS
          WAIT WINDOW "Selecting Record... Please Wait" NOWAIT
          =lfAddLTemp(m.Style,m.Color)
          *ash1
          INSERT INTO (lcBatLin) FROM MEMVAR
          SELECT (lcBatLin)
          Replace Dyelot   WITH StyDye.Dyelot   ,;
                  cBatType WITH 'S'             ,;
                  Scale    WITH Style.Scale     ,;
                  OldCost  WITH StyDye.Ave_Cost ,;
                  OldStk1  WITH StyDye.Stk1     ,;
                  OldStk2  WITH StyDye.Stk2     ,;
                  OldStk3  WITH StyDye.Stk3     ,;
                  OldStk4  WITH StyDye.Stk4     ,;
                  OldStk5  WITH StyDye.Stk5     ,;
                  OldStk6  WITH StyDye.Stk6     ,;
                  OldStk7  WITH StyDye.Stk7     ,;
                  OldStk8  WITH StyDye.Stk8     ,;
                  OldTotStk WITH StyDye.TotStk
        ENDIF
      ENDSCAN
    ELSE
      SELECT (lcRejSty)
      APPEND BLANK
      REPLACE Style    WITH &lcInport..STYLE + lcSepar + &lcInport..COLOR ,;
              WareCode WITH lcWare                                        ,;
              Reason   WITH "The  " + lcStyPic + "  is not found in the style file."
    ENDIF
  ENDSCAN
ELSE
  *C123853,1 MHM 01/15/2005 select bin location For David Luke[Start]
  IF ASCAN(laEvntTrig,PADR("DLSELREC",10)) <> 0 
    =gfDoTriger("ICINVLK",PADR("DLSELREC",10))
  ELSE
  *C123853,1 MHM 01/15/2005 [End]

  SCAN FOR &lcExpr AND IIF(llWareHus,CwareCode = ladata[8] ,.T.)

    SCAT MEMVAR MEMO
    IF llMatModul
      m.FabDesc = FABRIC.Desc
      m.CLRDESC = gfCodDes(COLOR,'COLOR')
    ELSE
      m.StyDesc = Style.Desc1
    ENDIF
    DO CASE
      CASE  lcCostMeth = 'S'  && Standard
        m.Cost    = IIF(llMatModul,FABRIC.CostUse,STYLE.TotCost)
        m.oCost   = IIF(llMatModul,FABRIC.CostUse,STYLE.TotCost)
      OTHERWISE       && 'A' Average
        m.Cost    = IIF(llMAtModul,nFAve_Cost,Ave_Cost)
        m.oCost   = IIF(llMAtModul,nFAve_Cost,Ave_Cost)
    ENDCASE
    m.ITEM    = IIF(llMatModul,Fabric,'')
    m.STOCK   = IIF(llMatModul,OnHand,TotStk)
    m.oSTOCK  = IIF(llMatModul,OnHand,TotStk)
    m.nStkVal = nStkVal
    IF lfChekBin()
      SELECT MDINVNTL
      SET ORDER TO MDINVNTLS
      WAIT WINDOW "Selecting Record... Please Wait" NOWAIT
      =lfAddLTemp(IIF(llMatModul,PADR(m.ITEM,19),m.Style),m.Color)
    ENDIF
  ENDSCAN

  *C123853,1 MHM 01/15/2005  bin location For David Luke[Start]
  ENDIF  
  *C123853,1 MHM 01/15/2005 [End]
  
ENDIF

IF llLogic
  PRIVATE lcAlias
  lcAlias = SELECT(0)
  SELECT (lcRejSty)
  GO TOP
  IF !EOF()
     lnAnswer = gfModalGen("QRM00000B42017","DIALOG",.F.,.F.,'One or more '+ lcStyPic +' got rejected.')
     =lfPrnRep()
  ENDIF
  SELECT(lcAlias)
ENDIF
*E301851,1 BWA 03/04/2002.[END]

*---Function To Get Batch Line And Insert Into Temp File
=lfTmpBtchL()
*SHOW GET pbPost   ENABLE
SELECT IIF(llMatModul,'FabDye','StyDye')
SET RELATION TO
WAIT CLEAR

*!*************************************************************
*! Name      : lfActsBrow
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Activate the browse.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfActsBrow()
*!*************************************************************
FUNCTION lfActsBrow

SELECT &lcTmpQuery
*: B803433,1 MHM 07/31/2000 Fix the bug of go to top of file when editing any record .
*GO TOP
*: B803433,1 MHM 07/31/2000 Fix the bug of go to top of file when editing any record .
lnCurrRec = RECNO()

IF llMatModul
  lcBrfield1 = [ITEM :10 :H = 'Item'        :W=.F. ,] +;
               [COLOR    :H = 'Color'       :W=.F. ,] +;
               [FabDesc  :H = 'Item Desc.'  :W=.F. ,] +;
               [CLRDESC:20  :H = 'Color Desc.'  :W=.F. ,] +;
               [OStock   :H = 'Old OnHand'  :W=.F. ,] +;
               [OCost    :H = 'Old Cost'    :W=.F. ,] +;
               [Stock    :H = 'New OnHand'  :W=.F. ,] +;
               [Cost     :H = 'New Cost'    :W=.F. ]
ELSE
  *B606191,1 BWA 08/14/2002 Fix the bug of arrows is not working to move between fields in the browse.[START]
  *lcBrfield1 = [Style   :H = lcStyPic      :W=.F. ,] +; 
               [STYDESC:40 :H = 'Desc.'    :W=.F. ,] +;
               [OStock  :H = 'Old Stock'   :W=.F. ,] +;
               [OCost   :H = 'Old Cost'    :W=.F. ,] +;
               [Stock   :H = 'New Stock'   :W=.F. ,] +;
               [Cost    :H = 'New Cost'    :W=.F.]

  lcBrfield1 = [Style      :H = lcStyPic    ,] +; 
               [STYDESC:40 :H = 'Desc.'     ,] +;
               [OStock     :H = 'Old Stock' ,] +;
               [OCost      :H = 'Old Cost'  ,] +;
               [Stock      :H = 'New Stock' ,] +;
               [Cost       :H = 'New Cost'   ]
  *B606191,1 BWA 08/14/2002.[END]

  *C123853,1 MHM 01/15/2005 modify browser to bin location For David Luke[Start]
  IF ASCAN(laEvntTrig,PADR("DLBRWBIN",10)) <> 0 
    =gfDoTriger("ICINVLK",PADR("DLBRWBIN",10))
  ENDIF
  *C123853,1 MHM 01/15/2005 [End]
  
ENDIF
BROWSE FIELDS &lcBrField1;
       WHEN lfwBrows();
       VALID :F lfvBrowse() ;
       WINDOW ICINVLKb IN WINDOW (gcBaseWind);
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NOEDIT;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       TITLE lcMtBrowTt

*!*************************************************************
*! Name      : lfUnTrap
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : Activate screen function....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
FUNCTION lfUnTrap
* -- Clear Trap
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If TOP window is not one of the browses window                                        
IF (WONTOP() # ALLTRIM((lcMtBrowTt))) OR (WONTOP() # ALLTRIM((lcMDBrowTt)))
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL CTRL+TAB
  ON KEY LABEL CTRL+ENTER
  ON KEY LABEL CTRL+HOME
  ON KEY LABEL CTRL+END
  ON KEY LABEL CTRL+W
ENDIF
*-- end of lfUnTrap.

*!*************************************************************
*! Name      : lfTrap
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trap function for Browse....
*!           : Deactivate screen function.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpTab,lpBackTab
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
FUNCTION lfTrap

*-- if TOP window is one of the browse window
IF (WONTOP() = ALLTRIM((lcMtBrowTt))) OR (WONTOP() = ALLTRIM((lcMDBrowTt)))
  glFromBrow = .T.
  ON KEY LABEL CTRL+ENTER lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+HOME  lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+W     lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+END   lnDummy = 1		&&Do nothing
  IF WONTOP() = ALLTRIM((lcMtBrowTt))
    ON KEY LABEL TAB           DO lpTab
    ON KEY LABEL CTRL+TAB      DO lpTab
    ON KEY LABEL BACKTAB       DO lpBackTab
  ELSE
    ON KEY LABEL TAB           DO lpTab1
    ON KEY LABEL CTRL+TAB      DO lpTab1
    ON KEY LABEL BACKTAB       DO lpBackTab1
  ENDIF
ENDIF
*-- end of lfTrap.

*!*************************************************************
*! Name      : lpTab
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trapping TAB order for browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpTab
*!*************************************************************
PROCEDURE lpTab

ON KEY LABEL TAB
ON KEY LABEL CTRL+TAB
*-- Go to next window
IF WONTOP() = ALLTRIM((lcMtBrowTt))
   ACTIVATE WINDOW ICINVLK2
  _CUROBJ = OBJNUM(pbMkDwm)
ELSE
   ACTIVATE WINDOW (lcMtBrowTt)
  _CUROBJ = OBJNUM(ibBBrow)
ENDIF
*-- end of lpTab.

*!*************************************************************
*! Name      : lpTab1
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trapping TAB order for browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpTab1
*!*************************************************************
PROCEDURE lpTab1

ON KEY LABEL TAB
ON KEY LABEL CTRL+TAB
*-- Go to next window
IF WONTOP() = ALLTRIM((lcMDBrowTt))
  IF llMatModul
     ACTIVATE WINDOW icinvm51
*    _CUROBJ = OBJNUM(lnMNewTot)
    _CUROBJ = OBJNUM(lcMReason)
  ELSE
     ACTIVATE WINDOW icinvs51
    _CUROBJ = OBJNUM(lcReason)
  ENDIF
ELSE
   ACTIVATE WINDOW (lcMDBrowTt)
  _CUROBJ = OBJNUM(ibBBrow)
ENDIF
*-- end of lpTab.
*!*************************************************************
*! Name      : lpBackTab
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trapping BACKTAB order for browse window
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpBackTab
*!*************************************************************
PROCEDURE lpBackTab

ON KEY LABEL BACKTAB

IF WONTOP() = ALLTRIM((lcMtBrowTt))
  *-- Go to previous window
  ACTIVATE WINDOW ICINVLK1		&&Activate popup windows
  _CUROBJ = OBJNUM(ladata[6])
ELSE
   ACTIVATE WINDOW (lcMtBrowTt)
  _CUROBJ = OBJNUM(ibBBrow) && Activate control panel window
ENDIF  
*-- end of lpBackTab.

*!*************************************************************
*! Name      : lpBackTab
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trapping BACKTAB order for browse window
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpBackTab
*!*************************************************************
PROCEDURE lpBackTab1

ON KEY LABEL BACKTAB

IF WONTOP() = ALLTRIM((lcMDBrowTt))
  *-- Go to previous window
  IF llMatModul
     ACTIVATE WINDOW icinvml5		&&Activate popup windows
    _CUROBJ = OBJNUM(lnMMCost)
  ELSE
     ACTIVATE WINDOW icinvSl5		&&Activate popup windows
    _CUROBJ = OBJNUM(lnMCost)
  ENDIF
ELSE
   ACTIVATE WINDOW (lcMDBrowTt)
  _CUROBJ = OBJNUM(ibBBrow) && Activate control panel window
ENDIF  
*-- end of lpBackTab.
*!*************************************************************
*! Name      : lfvBrowse
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid Browse Temp. sequence file fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrowse()
*!*************************************************************
*!
FUNCTION lfvBrowse

IF WONTOP() # ALLTRIM((lcMtBrowTt)) OR WONTOP() # ALLTRIM((lcMDBrowTt))
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- end of lfvBrowse.

*!*************************************************************
*! Name      : lfvBrowse
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid Browse Temp. sequence file fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrowse()
*!*************************************************************
*!
FUNCTION lfvBrowse2

IF WONTOP() # ALLTRIM((lcMDBrowTt))
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- end of lfvBrowse.

*!*************************************************************
*! Name      : lfwBrows
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : When Browse Temp. sequence file fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrows()
*!*************************************************************
*!
FUNCTION lfwBrows

SHOW WINDOW (lcMtBrowTt) REFRESH
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfwBrows
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : When Browse Temp. sequence file fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrows()
*!*************************************************************
*!
FUNCTION lfwBrows2

SHOW WINDOW (lcMDBrowTt) REFRESH
*B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand instead of TotStk in case of material.
*lnMNewTot = &lcDetLin..TotStk
lnMNewTot = IIF(llMatModul,&lcDetLin..OnHand,&lcDetLin..TotStk)
*B038829,1 ASH 12/25/2004 (End)
SHOW GET lnMNewTot
=lfGetAdjCd()
lcMReason = &lcDetLin..cReason 
lcReason = &lcDetLin..cReason 
SHOW GET lcMReason
SHOW GET lcReason
FOR lnInd = 1 TO 8
  lcInd = STR(lnInd,1)
  lnStk&lcInd = STK&lcInd
  SHOW GET lnStk&lcInd
ENDFOR
lnNewTot = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
*B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand instead of TotStk in case of material.
*lnNewTot = TOTSTK
lnNewTot = IIF(llMatModul,OnHand,TOTSTK)
*B038829,1 ASH 12/25/2004 (End)
SHOW GET lnNewTot DISABLE
IF llMatModul
  =lfRefresh('icinvm51')
ELSE
  =lfRefresh('icinvs51')
ENDIF


*-- end of lfwBrows.

*!*************************************************************
*! Name      : lpSavScr
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for save Button.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpSavScr()
*!*************************************************************
*!
FUNCTION lpSavScr

*--Start locking...
SELECT(lcTmpQuery)
GOTO TOP
IF EOF()
  RETURN(.F.)
ENDIF

=lfCrUnSess('O','pbSav')
DO CASE
*--- Edit Mode
CASE laScrMode[3]
  =lfSavEdt()
CASE laScrMode[4]
  =lfSavNew()
ENDCASE
=lfCrUnSess('C','') 

SELECT(lcTmpQuery)
GOTO TOP
=lfActsBrow()
SELECT IIF(llMatModul,'Fabdye','Stydye')
SET RELATION TO
SELECT(lcTmpQuery)
GOTO TOP

*!*************************************************************
*! Name      : lfSavEdt
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function to save in edit mode.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSavEdt()
*!*************************************************************
*!
FUNCTION lfSavEdt
SELECT (lcBatHdr)
SCAT MEMVAR MEMO
SELECT MDINVNTH
m.Content = laData[4]
m.Type    = laData[3]
GATH MEMVAR MEMO
SELECT MDINVNTL
SET ORDER TO Mdinvntl
=SEEK(lcKey+laData[2])

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*DELETE REST WHILE cbattype+cbatch+style+color+dyelot+clocation = ;
                  lcKey+laData[2] ;
                  FOR IIF(llDyelot AND ;
                  lfStyDye(Style,Color),!EMPTY(DYELOT),.T.)
DELETE REST WHILE cbattype+cLkBatch+style+color+dyelot+clocation = ;
                  lcKey+laData[2] ;
                  FOR IIF(llDyelot AND ;
                  lfStyDye(Style,Color),!EMPTY(DYELOT),.T.)
*B603596,1 KHM 06/18/2000 (End)

SELECT (lcBatLin)
GOTO TOP
SCAN  FOR IIF(llDyelot AND lfStyDye(Style,Color),!EMPTY(DYELOT),.T.)
  SCATTER MEMVAR MEMO
  
  *B039660,1 NNA 09/04/2005 (Begin) if we're in edit mode then we have to save the edit information not the add information
  *m.Cadd_user = gcUserName
  *m.Dadd_date = DATE()
  *m.Cadd_time = TIME()
  m.Cedit_user = gcUserName
  m.Dedit_date = DATE()
  m.Cedit_time = TIME()
  *B039660,1 NNA (End)
  
  m.cAdjReason = IIF(EMPTY(m.cAdjReason),lcDefAdjCd,m.cAdjReason)
  INSERT INTO MDINVNTL FROM MEMVAR
ENDSCAN

*!*************************************************************
*! Name      : lfSavNew
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function to save in New mode.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSavNew()
*!*************************************************************
*!
FUNCTION lfSavNew

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*lcBatchNo = gfsequence('cBatch')
lcBatchNo = gfsequence('cLkBatch')
*B603596,1 KHM 06/18/2000 (End)

SELECT (lcBatHdr)

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*REPLACE ALL cBatch WITH lcBatchNo,cBatType WITH lcKey
REPLACE ALL cLkBatch WITH lcBatchNo,cBatType WITH lcKey
*B603596,1 KHM 06/18/2000 (End)

SELECT (lcBatLin)
*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*REPLACE ALL cBatch WITH lcBatchNo,cBatType WITH lcKey
REPLACE ALL cLkBatch WITH lcBatchNo,cBatType WITH lcKey
*B603596,1 KHM 06/18/2000 (End)

SELECT (lcBatHdr)
GOTO TOP
SCAT MEMVAR MEMO
IF nSteps < 1
  INSERT INTO MDINVNTH FROM MEMVAR
  SELECT (lcBatHdr)
  =RLOCK()
  REPLACE nSteps WITH 1
  UNLOCK
ENDIF

SELECT (lcBatLin)

SCAN
  SCATTER MEMVAR MEMO
  m.Cadd_user = gcUserName
  m.Dadd_date = DATE()
  m.Cadd_time = TIME()
  IF nSteps < 1
    m.cAdjReason = IIF(EMPTY(m.cAdjReason),lcDefAdjCd,m.cAdjReason)
    INSERT INTO MDINVNTL FROM MEMVAR
    IF llMatModule
      =lfLkStyInv(SUBSTR(M.STYLE,1,7),M.COLOR,.T.)
    ELSE
      =lfLkStyInv(M.STYLE,M.COLOR,.T.)
    ENDIF
    SELECT (lcBatLin)
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF
ENDSCAN
=gfModalGen('TRM42180B00000','DIALOG',lcBatchNo)

*!*************************************************************
*! Name      : lfInitial
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function to initialize screen variable.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfInitial()
*!*************************************************************
*!
FUNCTION lfInitial

lcStat  = ''
lcStatColor = gcObjColor
laData[2]   = SPACE(6)
ladata[8]   = SPACE(6)
ladata[5]   = gdSysDate
ladata[6]   = {}
SELECT(lcTmpQuery)
DELETE ALL
=lfActsBrow()
SHOW GET laData[2]  ENABLE
SHOW GET ibBatch    ENABLE
SHOW GET ladata[5]  DISABLE
SHOW GET ladata[6]  DISABLE
SHOW GET ladata[8]  DISABLE
SHOW GET pbScop     DISABLE
_CUROBJ = OBJNUM(laData[2])

*!*************************************************************
*! Name      : lpShow
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function to show screen variable.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpShow()
*!*************************************************************
*!
FUNCTION lpShow
DO CASE
  *--- Case Select Mode Screen
  CASE laScrMode[1]
    *--Check uncomplete session (UNCSS).
    IF llContinue AND llGoAndChk AND lfChkUnComS()
      *--Return UNcomplete session no (lcSession).
      *RETURN
    ELSE
      lcSession = IIF(EMPTY(lcSession),gfsequence('cSession'),lcSession)
    ENDIF
    SELECT (lcBatHdr)
    SCATTER FIELDS &lcScFields TO laData  BLANK
    SELECT mdinvnth
    SCATTER FIELDS &lcScFields TO laData  BLANK
    IF !llWareHus
       SHOW GET ladata[8] DISABLE
    ENDIF
    ACTIVATE WINDOW gwcContrl1
    SHOW GET pbScop DISABLE
    =lfInitial()
    *--Cancel unCmSess record.
    =lfCrUnSess('I',' ')

  *--- Case View Mode Screen
  CASE laScrMode[2]
     SELECT (lcBatHdr)
     *B604322,1 - WAB (Start) commit the next line and zap the temp file instead of delete and 
     *B604322,1 - WAB         coloceting the data only if the user press the navigate buttons 
     *B604322,1 - WAB         cause for the first record the program recollect the data
     *SCATTER FIELDS &lcScFields TO laData 
     *SELECT mdinvnth
     *SCATTER FIELDS &lcScFields TO laData
     *SELECT (lcTmpQuery)
     *DELETE ALL
     *SELECT (lcBatLin)
     *DELETE ALL
     *SELECT (lcBatHdr)
     *DELETE ALL
     *=lfGetData()
     IF lcKey+laData[2] <> &lcBatHdr..cBatType+&lcBatHdr..cLkBatch
       SCATTER FIELDS &lcScFields TO laData 
       SELECT mdinvnth
       SCATTER FIELDS &lcScFields TO laData
       SELECT (lcTmpQuery)
       ZAP
       SELECT (lcBatLin)
       ZAP
       SELECT (lcBatHdr)
       ZAP
       SELECT(lcTmpQuery)
       =lfActsBrow()
       =lfGetData()
     ENDIF
     *B604322,1 - WAB (end)        
     lcStatColor = IIF(laData[3]='P',"RGB(255,255,255,255,0,0)","RGB(255,255,255,0,128,0)")
     IF MDINVNTH.Type = 'H'
        lcStatColor = "RGB(255,255,255,255,0,0)"
     ENDIF
    SHOW GET pbSlct ENABLE
    SHOW GET pbSav   DISABLE
    SHOW GET pbMkDwm DISABLE
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    SHOW GET pbMkZer DISABLE
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    DO CASE
      CASE  MDINVNTH.Type = 'L'
      CASE  MDINVNTH.Type = 'M'
        SHOW GET pbDlt   DISABLE
      CASE  MDINVNTH.Type = 'P'
        SHOW GET pbMkDwm DISABLE
        SHOW GET pbPost  DISABLE
        SHOW GET PbEdt   DISABLE
        SHOW GET pbDlt   DISABLE
    ENDCASE
    *--Cancel unCmSess record.
    =lfCrUnSess('I',' ')

   *--- Case Edit Mode Screen
  CASE laScrMode[3]
    SHOW GET pbMkDwm   ENABLE
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    SHOW GET pbMkZer ENABLE
    *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
    =lfEditMode()
    IF !llContinue 
      =lfCrUnSess('O','') 
    ENDIF

    *E301851,1 BWA 03/04/2002 Disable the Imported bottom in case the edit.[START]
    SHOW GET pBInvert DISABLE
    *E301851,1 BWA 03/04/2002.[END]

  *--- Case Add New Mode Screen
  CASE laScrMode[4]
    ACTIVATE WINDOW gwcContrl1
    SHOW GET pbScop ENABLE
    IF !llContinue 
      =lfCrUnSess('O','') 
    ENDIF
    =lfvNew()
ENDCASE
SELECT (lcTmpQuery)
GOTO TOP
IF EOF()
  SHOW GET pbSav     DISABLE
ELSE
  IF !laScrMode[2]
    SHOW GET pbSav     ENABLE
  ENDIF
ENDIF
IF (laScrMode[3]) AND LADATA[3]<>'P'
  IF LADATA[3] = 'H'
    DEFINE BAR 2 OF _OPTIONPOP PROMPT 'Open Batch' SKIP FOR .F.
  ELSE
    DEFINE BAR 2 OF _OPTIONPOP PROMPT 'Hold Batch' SKIP FOR .F.
  ENDIF
ELSE
  IF LADATA[3] = 'H'
    DEFINE BAR 2 OF _OPTIONPOP PROMPT 'Open Batch' SKIP FOR .T.
  ELSE
    DEFINE BAR 2 OF _OPTIONPOP PROMPT 'Hold Batch' SKIP FOR .T.
  ENDIF
ENDIF

*B130750,1 NNA 04/11/2006 (Begin) Trigger to Add a new Option called(Add new Bin the Batch File) in the Option menu
IF ASCAN(laEvntTrig,PADR("ADDSTYBN",10)) <> 0 AND laScrMode[3]
  =gfDoTriger("ICINVLK",PADR("ADDSTYBN",10))
ENDIF
*B130750,1 NNA (End)

IF laScrMode[2] AND MDINVNTH.Type = 'M'
  SHOW GET pbPost ENABLE
  DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Post Batch' SKIP FOR .F.
ELSE
  SHOW GET pbPost DISABLE
  DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Post Batch' SKIP FOR .T.
ENDIF

llGoAndChk = .T.

*!*************************************************************
*! Name      : lfEditMode
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function for edit mode.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfEditMode()
*!*************************************************************
*!
FUNCTION  lfEditMode

SHOW GET ibBatch   DISABLE
SHOW GET laData[2] DISABLE
SHOW GET ibLocat   DISABLE
SHOW GET ladata[5] DISABLE
SHOW GET ladata[8] DISABLE
SHOW GET pbScop    DISABLE
IF laData[3] <> 'P'
  SHOW GET ladata[6] ENABLE
ELSE
  SHOW GET ladata[6] DISABLE
ENDIF
IF MDINVNTH.Type <> 'M'
  SHOW GET pbPost   DISABLE
  *ELSE
  *SHOW GET ladata[6] ENABLE
ENDIF
*!*************************************************************
*! Name      : lpDelScr
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function for delete batch.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpDelScr()
*!*************************************************************
*!
FUNCTION lpDelScr

SELECT MDINVNTH
=SEEK(lcKey+laData[2])
*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*DELETE REST WHILE cBattype+cBatch = lcKey+laData[2]
DELETE REST WHILE cBattype+cLkBatch = lcKey+laData[2]
*B603596,1 KHM 06/18/2000 (End)

SELECT MDINVNTL

SET ORDER TO MDINVNTL
=SEEK(lcKey+laData[2])
*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*SCAN REST WHILE cBattype+cBatch = lcKey+laData[2]
SCAN REST WHILE cBattype+cLkBatch = lcKey+laData[2]
*B603596,1 KHM 06/18/2000 (End)

  SCAT MEMVAR MEMO
  IF llMatModul
    =lfLkStyInv(SUBSTR(m.Style,1,7),m.Color,.F.)
  ELSE
    =lfLkStyInv(m.Style,m.Color,.F.)
  ENDIF
  SELECT MDINVNTL
  DELETE
ENDSCAN
=lfInitial()
laScrMode = .F.
laScrMode[1] = .T.
SHOW GETS

*!*************************************************************
*! Name      : lfVMkDwn
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid Function for MkDwn buton.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVMkDwn()
*!*************************************************************
*!
FUNCTION lfVMkDwn

IF llLinkGL
  lnoldals = SELECT(0)
  SELECT CODES
  lcCodTag=TAG()
  SET ORDER TO TAG cCode_no
  IF !SEEK("N"+'CADJREASON','CODES')
    *--You have to edit the Adjustment reasons codes first, Cannot proceed.
    =gfModalGen('TRM42111B42001','DIALOG')
    SET ORDER TO TAG &lcCodTag
    SELECT &lnoldals
    RETURN 
  ELSE
    =SEEK("D"+'CADJREASON','CODES')
    lcDefAdjCd = Codes.Ccode_No 
  ENDIF  
  SET ORDER TO TAG &lcCodTag
  SELECT(lnoldals)
ENDIF

SELECT (lcBatLin)
SET ORDER TO lcBatInd
IF llMatModul
  =SEEK(lcKey+laData[2]+&lcTmpQuery..Item+&lcTmpQuery..Color)
ELSE
  =SEEK(lcKey+laData[2]+&lcTmpQuery..Style+&lcTmpQuery..Color)
ENDIF
lcSty = Style  &&IIF(llMatModul,ITEM,Style)
lcClr = Color
*-cDye_Flg
IF llMatModul
  =SEEK(SUBSTR(lcSty,1,7)+lcClr,'Fabric')
  llStyDye = (Fabric.cDye_Flg='Y')
ELSE
  =SEEK(lcSty,'Style')
  llStyDye = (Style.cDye_Flg='Y')
ENDIF
SELECT (lcBatLin)
SET ORDER TO lcBatInd
=SEEK(lcKey+laData[2]+lcSty+lcClr)
IF llDyelot AND llStyDye
  *B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
  *LOCATE REST WHILE cbattype+cbatch+style+color+DYELOT =;
                    lcKey+laData[2]+lcSty+lcClr;
               FOR !EMPTY(DYELOT)
  LOCATE REST WHILE cbattype+cLkBatch+style+color+DYELOT =;
                    lcKey+laData[2]+lcSty+lcClr;
               FOR !EMPTY(DYELOT)
  *B603596,1 KHM 06/18/2000 (End)
  
  IF FOUND()
    lcDyelot = DYELOT
  ELSE
    =SEEK(lcKey+laData[2]+lcSty+lcClr)
  ENDIF
ENDIF
IF llLoc
 =lfFillBin()
ENDIF
SELECT (lcBatLin)
lnLoc = IIF(ASCAN(laMloc,cLocation)<>0,ASCAN(laMloc,cLocation),1)
IF llMatModule
  FOR I = 1 TO 8
    Z = STR (I,1)
    lcSz&Z = ''
    lnStk&Z = 0
  ENDFOR
ELSE
  IF SEEK('S'+Scale,'Scale')
    FOR I = 1 TO 8
      Z = STR (I,1)
      lcSz&Z = Scale.Sz&Z
    ENDFOR
  ENDIF
  FOR I = 1 TO 8
    Z = STR (I,1)
    lnStk&Z = Stk&Z
  ENDFOR
ENDIF
IF llMatModul
  *lnTOTSTK   = TotStk
  lnTOTSTK   = OnHand
  lnMMCost   = COST
  lnMOldCost = OldCOST
  lcMReason  = cReason
  lcMatDesc  = IIF(SEEK(SUBSTR(lcSty,1,7)+lcClr,'FABRIC'),FABRIC.Desc,'')
  lcClrDes   = gfCodDes(lcClr,'COLOR')
  lnMNewTot  = lnTOTSTK
ELSE
  lnMCost   = COST
  lnOldCost = OldCOST
  lcReason  = cReason
  lcStyDesc = IIF(SEEK(lcSty,'Style'),Style.Desc1,'')
  lnNewTot  = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
ENDIF
lcMat     = lcSty
lcMatClr  = lcClr
IF llMatModul
  *B039660,1 NNA 09/04/2005 (Begin) Create the Temp File at the work directory
  *SELECT * FROM (lcBatLin) WHERE Style+Color = lcSty + lcClr ;
                             AND IIF(llDyelot AND lfStyDye(lcSty ,lcClr),!EMPTY(&lcBatLin..DYELOT),.T.) INTO DBF (lcDetLin)
  SELECT * FROM (lcBatLin) WHERE Style+Color = lcSty + lcClr ;
                             AND IIF(llDyelot AND lfStyDye(lcSty ,lcClr),!EMPTY(&lcBatLin..DYELOT),.T.) INTO DBF (gcWorkDir+lcDetLin)
  *B039660,1 NNA (End)
  INDEX ON Style+color+dyelot+clocation TAG (lcDetLin)
  IF _TALLY = 0
    lcMes = 'Item - Color ' + lcSty+' - '+lcClr
    =gfModalGen('TRM42210B00000','DIALOG',lcMes)
    RETURN
  ENDIF
ELSE

  *C123853,1 MHM 01/15/2005 select temp bin location For David Luke[Start]
  IF ASCAN(laEvntTrig,PADR("DLSELTMP",10)) <> 0 
    =gfDoTriger("ICINVLK",PADR("DLSELTMP",10))
  ELSE
  *C123853,1 MHM 01/15/2005 [End]
    
    *B039660,1 NNA 09/04/2005 (Begin) Create the Temp File at the work directory
    *SELECT * FROM (lcBatLin) WHERE Style = lcSty  ;
                               AND IIF(llDyelot AND lfStyDye(lcSty),!EMPTY(&lcBatLin..DYELOT),.T.) INTO DBF (lcDetLin)
    SELECT * FROM (lcBatLin) WHERE Style = lcSty  ;
                               AND IIF(llDyelot AND lfStyDye(lcSty),!EMPTY(&lcBatLin..DYELOT),.T.) INTO DBF (gcWorkDir+lcDetLin)
    *B039660,1 NNA (End)
    
  *C123853,1 MHM 01/15/2005 bin location For David Luke[Start]
  ENDIF
  *C123853,1 MHM 01/15/2005 [End]

  INDEX ON style+color+dyelot+clocation TAG (lcDetLin)
  IF _TALLY = 0
    lcMes = lcStyPic +' '+ lcSty
    =gfModalGen('TRM42210B00000','DIALOG',lcMes)
    RETURN
  ENDIF
ENDIF
=lfGetAdjCd()

*C123853,1 MHM 01/15/2005 Fix Bug in handling Trap key ESC. [Start]
PUSH KEY
*C123853,1 MHM 01/15/2005 [End]

IF llMatModul
 IF (llDyelot AND lfStyDye(lcSty ,lcClr)) OR (llLoc AND ALEN(laMLoc) > 1)
   IF !EMPTY(cLocation)
     lnBin = 1
   ENDIF
   DO gcScrDir+("ICINVMLM.SPR")
 ELSE
   DO gcScrDir+("ICINVMM.SPR")
 ENDIF
ELSE
 IF (llDyelot AND lfStyDye(lcSty)) OR (llLoc AND ALEN(laMLoc) > 1)
   IF !EMPTY(cLocation)
     lnBin = 1
   ENDIF
   DO gcScrDir+("ICINVMLS.SPR")
 ELSE
   DO gcScrDir+("ICINVSS.SPR")
 ENDIF
ENDIF

*C123853,1 MHM 01/15/2005 Fix Bug in handling Trap key ESC. [Start]
POP KEY
*C123853,1 MHM 01/15/2005  [End]

=lfActsBrow()
lcStat  = 'Open'
=lfRefresh('icinvlk1')
IF laData[3] = 'L'
  SHOW GET pbDlt  ENABLE
  SHOW GET pbPost DISABLE
  SHOW GET ladata[6]   DISABLE  
ELSE
  SHOW GET pbDlt  DISABLE
  IF laData[3] = 'M'
    SHOW GET ladata[6] ENABLE
  ELSE
    SHOW GET ladata[6] DISABLE  
  ENDIF
ENDIF
SHOW GET pbSlct ENABLE

*!*************************************************************
*! Name      : lfVMOk
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid Function for Save button in mdWn screen buton.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVMOk()
*!*************************************************************
*!
FUNCTION lfVMOk

SELECT (lcDetLin)
GOTO TOP
*B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand instead of TotStk in case of material.
*SUM REST TotStk TO lnLNewTot
SUM REST IIF(llMatModul,OnHand,TotStk) TO lnLNewTot
*B038829,1 ASH 12/25/2004 (End)
GOTO TOP
SELECT(lcTmpQuery)
REPLACE STOCK WITH lnLNewTot;
        Cost  WITH IIF(llMatModul,lnMMCost,lnMCost)
SELECT (lcBatLin)
GOTO TOP
*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*DELETE ALL FOR cbattype+cbatch+style+color+DYELOT =;
               lcKey+laData[2]+&lcDetLin..Style+&lcDetLin..Color;
               AND IIF(llDyelot AND lfStyDye(&lcDetLin..Style,&lcDetLin..Color),!EMPTY(&lcBatLin..DYELOT),.T.)

*C123853,1 MHM 01/15/2005 delete all record for David luck [Start]

*B039660,1 NNA 09/04/2005 (Begin) Check also for (use Bin Location Yes/No) to decrease the codes lines in the Binmain.prg
*B039660,1 NNA            and not writing the Else satement again
*IF ASCAN(laEvntTrig,PADR("DLDELALL",10)) <> 0 
IF ASCAN(laEvntTrig,PADR("DLDELALL",10)) <> 0 AND gfGetMemVar('M_DLUSEBIN',gcAct_Comp)
*B039660,1 NNA (End)

  =gfDoTriger("ICINVLK",PADR("DLDELALL",10))
ELSE
*C123853,1 MHM 01/15/2005  [End]

  DELETE ALL FOR cbattype+cLkBatch+style+color+DYELOT =;
                 lcKey+laData[2]+&lcDetLin..Style+&lcDetLin..Color;
                 AND IIF(llDyelot AND lfStyDye(&lcDetLin..Style,&lcDetLin..Color),!EMPTY(&lcBatLin..DYELOT),.T.)

*C123853,1 MHM 01/15/2005 [Start]
ENDIF               
*C123853,1 MHM 01/15/2005 [End]
                 
*B603596,1 KHM 06/18/2000 (End)

*B039660,1 NNA 09/04/2005 (Begin) Create the Temp File at the work directory
*APPEND FROM (lcDetLin)
APPEND FROM (gcWorkDir+lcDetLin)
*B039660,1 NNA (End)

SELECT (lcDetLin)
ZAP
laData[3] = IIF(laData[3] = 'H','H','M')
SELECT (lcBatHdr)
GOTO TOP
REPLACE Type WITH laData[3]
CLEAR READ

*!*************************************************************
*! Name      : lfVQty
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid Function for Quantity.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVQty()
*!*************************************************************
*!
FUNCTION lfVQty
SELECT (lcDetLin)
IF !llMatModul
  FOR lnInd = 1 TO 8
    lcInd = STR(lnInd,1)
    REPLACE STK&lcInd WITH lnStk&lcInd
  ENDFOR
  lnNewTot = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
  *B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand instead of TotStk in case of material.
  REPLACE TOTSTK WITH lnNewTot
ELSE
  REPLACE OnHand WITH lnNewTot
ENDIF
*REPLACE TOTSTK WITH lnNewTot
*B038829,1 ASH 12/25/2004 (End)

SHOW GET lnNewTot DISABLE
IF (llDyelot AND llStyDye) OR (llLoc AND ALEN(laMLoc) > 1)
  =lfActDBrow()
ENDIF


*!*************************************************************
*! Name      : lfClearRep
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Dummy function .
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfClearRep()
*!*************************************************************
*!
FUNCTION lfClearRep

*--- This is dummy function to used by oprion grid.

*!*************************************************************
*! Name      : lfvPost
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for post batch .
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPost()
*!*************************************************************
*!
FUNCTION lfvPost

IF laData[3] = 'H'
  =gfModalGen('TRM42209B00000','DIALOG')
  RETURN
ENDIF
IF ladata[6] = {} OR ladata[5] > ladata[6]
  =gfModalGen('TRM42192B00000','DIALOG')
  SHOW GET ladata[6] ENABLE
  _CUROBJ = OBJNUM(ladata[6])
  RETURN
ENDIF
*---Conferm missage 'You will not be able to modify this batch after posting .Post now?'
IF gfModalGen('QRM42206B00006','DIALOG') = 2
  RETURN
ENDIF

*---Warning message 'This process affects all transaction files. It is HIGHLY RECOMMENDED to create backup before posting . Would you like to backup?'
IF gfModalGen('QRM42211B00006','DIALOG') = 1
  *--- Function to BackUp files.
  =lfBackUp()
ENDIF

*---VIP
IF !CHECKPRD(ladata[5],'lcGLFYear','lcGLPeriod','IA')
  RETURN
ENDIF

=lfCrUnSess('O','pbPost')
IF llLinkGL AND !USED(lctmpGlDis)
  SELECT GLDIST
  *-- OPEN A TEMP FILE TO BE USED IN CALLING 'GLDIST' PROCEDURE.
  COPY STRUCTURE TO &gcWorkDir.&lctmpGlDis
  SELECT 0
  USE (gcWorkDir+lctmpGlDis) EXCLUSIVE
ENDIF

WAIT WINDOW  'Updating the style real inventory...' NOWAIT
SELECT (lcBatHdr)
REPLACE TYPE      WITH 'P';
        dPostDate WITH ladata[6]

SELECT MDINVNTH
=SEEK(lcKey+ladata[2])
REPLACE TYPE      WITH 'P';
        dPostDate WITH ladata[6]

ldlock = date
ladata[6] = dPostDate
ladata[3] = 'P'
lcStat    = 'Posted'
lcStatColor = "RGB(255,255,255,255,0,0)"
=lfRefresh('ICINVLK1')

SELECT MDINVNTL
lcOldOrd = ORDER()
SET ORDER TO MDINVNTL
lcPType  = IIF(llMatModule,'M','S')
IF !EMPTY(lcPostScVar)
  =SEEK(lcPostScVar)
ELSE
  *B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
  *=SEEK(lcPType+MDINVNTH.cBatch)  
  =SEEK(lcPType+MDINVNTH.cLkBatch)
  *B603596,1 KHM 06/18/2000 (End)
ENDIF
*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*lcPBatch = MDINVNTH.cBatch
lcPBatch = MDINVNTH.cLkBatch
*B603596,1 KHM 06/18/2000 (End)

**---cbattype+cbatch+style+color
lcLinkCode = 'DEFDEF'
*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*DO WHILE !EOF() AND cbattype+cbatch+style+color+dyelot+clocation;
                    = lcPType+MDINVNTH.cBatch
DO WHILE !EOF() AND cbattype+cLkBatch+style+color+dyelot+clocation;
                    = lcPType+MDINVNTH.cLkBatch
  *lcPostScVar = cbattype+cbatch+style+color
  lcPostScVar = cbattype+cLkBatch+style+color
*B603596,1 KHM 06/18/2000 (End)

  = gfSavSess(lcUnCmPgId, lcFiles, @laVariables,lcSession) 
  lcPStyle = Style
  lcPColor = Color
  llAdjYNo = .F.
  =SEEK(IIF(llMatModule,PADR(lcPStyle,7)+lcPColor,lcPStyle),;
        IIF(llMatModule,'FABRIC','STYLE'))
  =SEEK(IIF(llMatModule,PADR(lcPStyle,7)+lcPColor,lcPStyle+IIF(llWareHus,ladata[8],'')),;
        IIF(llMatModule,'FABDYE','STYDYE'))
  *--- Get the link Code.
  lcLinkCode = IIF(llWareHus,IIF(llMatModule,FabDye.GL_LINK,StyDye.GL_LINK),;
                             IIF(llMatModule,Fabric.Link_Code,Style.Link_Code))
  DECLARE laAdjust[10]
  DIMENSION laOldStk[12]
  laOldStk[12] = 0
  STORE 0 TO laAdjust,laOldStk
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
  lcAdjAcct  = ''
  *B038829,1 ASH 12/25/2004 (Begin) Use the new field OldQty instead of OldTotStk in case of material.
  *lnTempCst  = IIF(EMPTY(MDINVNTL.Dyelot),MDINVNTL.nStkVal/MDINVNTL.OldTotStk,lnTempCst)
  lnTempCst  = IIF(EMPTY(MDINVNTL.Dyelot),MDINVNTL.nStkVal/IIF(llMatModule,MDINVNTL.OldQty,MDINVNTL.OldTotStk),lnTempCst)
  *B038829,1 ASH 12/25/2004 (End)
  IF llMatModul
    =SEEK(SUBSTR(lcPStyle,1,7)+lcPColor,'Fabric')
    llStyDye = (Fabric.cDye_Flg='Y')
  ELSE
    =SEEK(lcPStyle,'Style')
    llStyDye = (Style.cDye_Flg='Y')
  ENDIF
  SELECT MDINVNTL
  SET ORDER TO MDINVNTL
  IF llDyelot AND llStyDye
    *B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
    *LOCATE REST WHILE cbattype+cbatch+style+color+DYELOT =;
                      lcKey+laData[2]+lcPStyle+lcPColor;
                 FOR !EMPTY(DYELOT)
    LOCATE REST WHILE cbattype+cLkBatch+style+color+DYELOT =;
                      lcKey+laData[2]+lcPStyle+lcPColor;
                 FOR !EMPTY(DYELOT)
    *B603596,1 KHM 06/18/2000 (End)
  ENDIF
  lcDye1 = Dyelot
  *B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
  *SCAN REST WHILE cbattype+cbatch+style+color+DYELOT =;
                  lcPType +lcPBatch+lcPStyle+lcPColor+lcDye1;
            FOR   (OldTotStk <> 0 OR TotStk <> 0)

  *B605395,4 BWA 02/18/2002 Fix the bug of not updating the style when the oldstock = 0.[START]
  *SCAN REST WHILE cbattype+cLkBatch+style+color+DYELOT =;
                  lcPType +lcPBatch+lcPStyle+lcPColor+lcDye1;
            FOR   (OldTotStk <> 0 OR TotStk <> 0)
    
  *B038829,1 ASH 12/25/2004 (Begin) Use the new field OldQty instead of OldTotStk in case of material.
  *SCAN REST WHILE cbattype+cLkBatch+style+color+DYELOT =;
                  lcPType +lcPBatch+lcPStyle+lcPColor+lcDye1;
            FOR   ((OldTotStk<>0 OR OLDSTK1<>0 OR OLDSTK2<>0 OR ;
                                    OLDSTK3<>0 OR OLDSTK4<>0 OR ;
                                    OLDSTK5<>0 OR OLDSTK6<>0 OR ;
                                    OLDSTK7<>0 OR OLDSTK8<>0)   ;
                                    OR TotStk <> 0 )

  *C123853,1 MHM 01/15/2005 check for Bin Location first[Start]

  *B039660,1 NNA 09/04/2005 (Begin) Check also for (use Bin Location Yes/No) to decrease the codes lines in the Binmain.prg
  *B039660,1 NNA            and not writing the Else satement again
  *IF ASCAN(laEvntTrig,PADR("DLPSTLCK",10)) <> 0 
  IF ASCAN(laEvntTrig,PADR("DLPSTLCK",10)) <> 0 AND gfGetMemVar('M_DLUSEBIN',gcAct_Comp)
  *B039660,1 NNA (End)

    =gfDoTriger("ICINVLK",PADR("DLPSTLCK",10))
  ELSE
  *C123853,1 MHM 01/15/2005 [End]
    SCAN REST WHILE cbattype+cLkBatch+style+color+DYELOT =;
                    lcPType +lcPBatch+lcPStyle+lcPColor+lcDye1;
              FOR   ((OldTotStk<>0 OR OLDSTK1<>0 OR OLDSTK2<>0 OR ;
                                      OLDSTK3<>0 OR OLDSTK4<>0 OR ;
                                      OLDSTK5<>0 OR OLDSTK6<>0 OR ;
                                      OLDSTK7<>0 OR OLDSTK8<>0)   ;
                                      OR TotStk <> 0 OR OnHand <> 0 OR OldQty <> 0 OR nStkVal <> 0)
      *B038829,1 ASH 12/25/2004 (End)
      *B605395,4 BWA 02/18/2002.[END]
      *WAIT WINDOW "Posting Batch# " + MDINVNTH.cBatch + ;
                                       IIF(llMatModule,'  Item   - Color \ ' + PADR(STYLE,7)+'-'+COLOR ,;
                                       lcStyPic +[\ ]+Style)  NOWAIT
      WAIT WINDOW "Posting Batch# " + MDINVNTH.cLkBatch + ;
                                      IIF(llMatModule,'  Item   - Color \ ' + PADR(STYLE,7)+'-'+COLOR ,;
                                      lcStyPic +[\ ]+Style)  NOWAIT
    
      *E302011,1 SSE 08/24/2003 Proceed with Thermometer. [Begin]
      lnCountLn = lnCountLn + 1
      =gfThermo(lnTotRec,lnCountLn,'Posting')
      *E302011,1 SSE 08/24/2003 Proceed with Thermometer. [End]
    
      *B603596,1 KHM 06/18/2000 (End)

      *----Start Calculate Stk for posting
      lcAdjReason = MDINVNTL.cAdjReason
      IF !llMatModule
        FOR lnIndex = 1 TO 8
          lcSub = STR(lnIndex,1)
          laAdjust[lnIndex] = laAdjust[lnIndex] + MDINVNTL.Stk&lcSub
          laOldStk[lnIndex] = laOldStk[lnIndex] + MDINVNTL.OldStk&lcSub
        ENDFOR
      *B038829,1 ASH 12/25/2004 (Begin) To hold the stock value in case of material locking.
      ELSE
        laOldStk[2] = MDINVNTL.nStkVal
      ENDIF
      *B038829,1 ASH 12/25/2004 (End)
      *B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand&OldQty instead of TOTSTK&OldTotStk in case of material.
      *laAdjust[9]  = laAdjust[9] + MDINVNTL.TOTSTK
      *laOldStk[9]  = laOldStk[9] + MDINVNTL.OldTOTSTK    
      laAdjust[9]  = laAdjust[9] + IIF(llMatModule,MDINVNTL.OnHand,MDINVNTL.TOTSTK)
      laOldStk[9]  = laOldStk[9] + IIF(llMatModule,MDINVNTL.OldQty,MDINVNTL.OldTOTSTK)
      *B038829,1 ASH 12/25/2004 (End)
      laAdjust[10] = MDINVNTL.COST
    ENDSCAN
    *C123853,1 MHM 01/15/2005 [Start]
  ENDIF 
  *C123853,1 MHM 01/15/2005 [End]
    
  *B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
  *IF cbattype+cbatch+style+color+DYELOT <>;
                  lcPType +lcPBatch+lcPStyle+lcPColor
  IF cbattype+cLkBatch+style+color+DYELOT <>;
                  lcPType +lcPBatch+lcPStyle+lcPColor
  *B603596,1 KHM 06/18/2000 (End)
    llAdjYNo = .T.
    laOldStk[12] = 1
  ENDIF
  *: B#603426,1 SSH 30/01/00 Add this condition to post style with new<>0 or old<>0
  *: B#603426,1 SSH (Start)

  *B605395,4 BWA 02/18/2002 Fix the bug of not updating the style when the oldstock = 0.[START]
  *IF (laAdjust[9] <> 0 OR laOldStk[9] <> 0)
  IF (laAdjust[9]<>0 OR laOldStk[9]<>0 OR laOldStk[1]<>0 OR laOldStk[2]<>0 OR laOldStk[3]<>0 OR laOldStk[4]<>0 OR laOldStk[5]<>0 OR laOldStk[6]<>0 OR laOldStk[7]<>0 OR laOldStk[8]<>0)
  *B605395,4 BWA 02/18/2002.[END]

  *: B#603426,1 SSH (End)
    laOldStk[10] = laOldStk[9]*lnTempCst
    laOldStk[11] = laData[6]
    lnMarkQty    = laOldStk[9]
    lnMarkVal    = laOldStk[9]*lnTempCst
    IF llMatModule
      =lfPostMat()
      llDumy = (llAdjYNo AND lfLkStyInv(SUBSTR(lcPStyle,1,7),lcPColor,.F.))
    ELSE
      =lfPostSty()
      lcDummy = ( llAdjYNo AND lfLkStyInv(lcPStyle,lcPColor,.F.))
    ENDIF
  *: B#603426,1 SSH Even not posted style need to remove its flag from styinvjl(Start)
  ELSE
    IF llMatModule
      llDumy = (llAdjYNo AND lfLkStyInv(SUBSTR(lcPStyle,1,7),lcPColor,.F.))
    ELSE
      lcDummy = ( llAdjYNo AND lfLkStyInv(lcPStyle,lcPColor,.F.))
    ENDIF
  ENDIF
  *: B#603426,1 SSH (End)
ENDDO

*E302011,1 SSE 08/24/2003 Close the Thermometer. [Begin]
FOR lnRest = lnCountLn TO lnTotRec
  =gfThermo(lnTotRec , lnRest , 'Posting')
ENDFOR    && End of FOR Loop
*E302011,1 SSE 08/24/2003 Close the Thermometer. [End]

SELECT MDINVNTL
=SEEK(lcKey+ladata[2])
*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*REPLACE REST nSteps WITH 0 WHILE cbattype+cbatch+style+color+dyelot+clocation;
                                 = lcPType+MDINVNTH.cBatch
REPLACE REST nSteps WITH 0 WHILE cbattype+cLkBatch+style+color+dyelot+clocation;
                                 = lcPType+MDINVNTH.cLkBatch
*B603596,1 KHM 06/18/2000 (End)

SET ORDER TO &lcOldOrd
=lfCrUnSess('C','')

SHOW GET pbPost     DISABLE
SHOW GET ladata[6]  DISABLE
lnAlias = SELECT()
IF llLinkGL
  SELECT (lctmpGlDis)
  IF EMPTY(lcGlSess)
    lcGlSess   = gfSequence('GLSession')
  ENDIF
  REPLACE ALL GlSession WITH lcGlSess
  *-- If temp. file not empty
  IF RECCOUNT() <> 0
    SELECT GLDIST
    APPEND FROM (gcWorkDir+lctmpGlDis)
  ENDIF
  USE IN (lctmpGlDis)
  ERASE (gcWorkDir+lctmpGlDis+".DBF")
ENDIF
lcPostScVar = ''
=lfCrUnSess('C','')
SELECT(lnAlias)
SHOW GETS


*!*************************************************************
*! Name      : lfWB
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : When function for browse button .
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfWB()
*!*************************************************************
*!
FUNCTION lfWB

laData[2] = '?     '

*!*************************************************************
*! Name      : lfFillBin
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function to colect bins.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfFillBin()
*!*************************************************************
*!
FUNCTION lfFillBin

I=1
laMloc[1]  = ''
SELECT WhsLoc
SET ORDER TO Whslocst
IF SEEK(lcSty+lcClr+&lcBatLin..cWareCode)
  SCAN REST WHILE Style+Color+cWareCode=;
    lcSty+lcClr+&lcBatLin..cWareCode
    I=I+1
    DECLARE laMloc[I]
    laMloc[I]  = clocation
  ENDSCAN
ELSE
  DECLARE laMloc[1]
  laMloc = ''
  laMloc[1]  = 'All'
ENDIF
*!*************************************************************
*! Name      : lfVBin
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for bins popup.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVBin()
*!*************************************************************
*!
FUNCTION lfVBin
*--- To fill the array laMloc
=lfFillBin()
IF lnBin = 1
  =lfAddBin()
ELSE
  =lfRemBin()
ENDIF
SELECT (lcDetLin)
GOTO TOP
=lfActDBrow()
=lfwBrows2()

*!*************************************************************
*! Name      : lfAddBin
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for bins popup.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfAddBin()
*!*************************************************************
*!
FUNCTION lfAddBin

*--- Bin array = laMloc
IF ALEN(laMloc) < 2
  RETURN
ENDIF
SELECT (lcDetLin)
GOTO TOP
SCAN
  SCAT MEMVAR MEMO
  FOR lnInd1 = 2 TO ALEN(laMloc)
    m.cLocation = laMloc[lnInd1]
    IF lnInd1 > 2
      INSERT INTO (lcDetLin) FROM MEMVAR
      IF !llMatModule
        FOR lnInd2 = 1 TO 8
          lcInd2 = STR(lnInd2 ,1 )
          REPLACE Stk&lcInd2 WITH INT(Stk&lcInd2/(ALEN(laMloc)-1))
          REPLACE OldStk&lcInd2 WITH INT(OldStk&lcInd2/(ALEN(laMloc)-1))
        ENDFOR
        *B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand&OldQty instead of TOTSTK&OldTotStk in case of material.        
        REPLACE TotStk WITH INT(TotStk/(ALEN(laMloc)-1))
        REPLACE OldTotStk WITH INT(OldTotStk/(ALEN(laMloc)-1))
      ELSE
        REPLACE OnHand WITH INT(OnHand/(ALEN(laMloc)-1))
        REPLACE OldQty WITH INT(OldQty/(ALEN(laMloc)-1))
      ENDIF
      *REPLACE TotStk WITH INT(TotStk/(ALEN(laMloc)-1))
      *REPLACE OldTotStk WITH INT(OldTotStk/(ALEN(laMloc)-1))
      *B038829,1 ASH 12/25/2004 (End)
    ELSE
      GATHER MEMVAR MEMO
      *B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand&OldQty instead of TOTSTK&OldTotStk in case of material.
      *REPLACE TotStk WITH INT((TotStk/(ALEN(laMloc)-1)) )+ (IIF(TotStk<0,-1,1)*MOD(ABS(TotStk),(ALEN(laMloc)-1)))
      *REPLACE OldTotStk WITH INT((OldTotStk/(ALEN(laMloc)-1)) )+ (IIF(OldTotStk<0,-1,1)*MOD(ABS(OldTotStk),(ALEN(laMloc)-1)))
      *B038829,1 ASH 12/25/2004 (End)
      IF !llMatModule
        FOR lnInd2 = 1 TO 8
          lcInd2 = STR(lnInd2 ,1 )
          REPLACE Stk&lcInd2 WITH INT((Stk&lcInd2/(ALEN(laMloc)-1)) )+ (IIF(Stk&lcInd2<0,-1,1)*MOD(ABS(Stk&lcInd2),(ALEN(laMloc)-1)))
          REPLACE OldStk&lcInd2 WITH INT((OldStk&lcInd2/(ALEN(laMloc)-1)) )+ (IIF(OldStk&lcInd2<0,-1,1)*MOD(ABS(OldStk&lcInd2),(ALEN(laMloc)-1)))
        ENDFOR
        *B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand&OldQty instead of TOTSTK&OldTotStk in case of material.
        REPLACE TotStk WITH INT((TotStk/(ALEN(laMloc)-1)) )+ (IIF(TotStk<0,-1,1)*MOD(ABS(TotStk),(ALEN(laMloc)-1)))
        REPLACE OldTotStk WITH INT((OldTotStk/(ALEN(laMloc)-1)) )+ (IIF(OldTotStk<0,-1,1)*MOD(ABS(OldTotStk),(ALEN(laMloc)-1)))
      ELSE
        REPLACE OnHand WITH INT((OnHand/(ALEN(laMloc)-1)) )+ (IIF(OnHand<0,-1,1)*MOD(ABS(OnHand),(ALEN(laMloc)-1)))
        REPLACE OldQty WITH INT((OldQty/(ALEN(laMloc)-1)) )+ (IIF(OldQty<0,-1,1)*MOD(ABS(OldQty),(ALEN(laMloc)-1)))
      ENDIF
      *B038829,1 ASH 12/25/2004 (End)
    ENDIF
  ENDFOR
ENDSCAN

*!*************************************************************
*! Name      : lfRemBin
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for bins popup.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfRemBin()
*!*************************************************************
*!
FUNCTION lfRemBin
*--- Bin array = laMloc

IF ALEN(laMloc) < 2
  RETURN
ENDIF
SELECT (lcDetLin)
GOTO TOP
DO WHILE !EOF()
  lcS1 = Style
  lcC1 = Color
  lcD1 = Dyelot
  =SEEK(lcS1+lcC1+lcD1)
  SCAT MEMVAR MEMO
  *B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand&OldQty instead of TOTSTK&OldTotStk in case of material.
  *m.TotStk = 0
  *m.OldTotStk = 0
  *B038829,1 ASH 12/25/2004 (End)
  IF !llMatModule
    FOR lnInd = 1 TO 8
      lcInd = STR(lnInd ,1 )
      m.Stk&lcInd = 0
      m.OldStk&lcInd = 0
    ENDFOR
    *B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand&OldQty instead of TOTSTK&OldTotStk in case of material.
    m.TotStk = 0
    m.OldTotStk = 0
    *B038829,1 ASH 12/25/2004 (End)
  ELSE
    m.OnHand = 0
    m.OldQty = 0
  ENDIF
  SCAN REST WHILE Style+color+dyelot+clocation = ;
                  lcS1+lcC1+lcD1 FOR !EMPTY(clocation)
    m.cLocation  = ''
    *B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand&OldQty instead of TOTSTK&OldTotStk in case of material.
    *m.TotStk = m.TotStk+TotStk
    *m.OldTotStk = m.OldTotStk+OldTotStk
    *B038829,1 ASH 12/25/2004 (End)
    IF !llMatModule
      FOR lnInd = 1 TO 8
        lcInd = STR(lnInd ,1 )
        m.Stk&lcInd = m.Stk&lcInd+Stk&lcInd
        m.OldStk&lcInd = m.OldStk&lcInd+OldStk&lcInd
      ENDFOR
      *B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand&OldQty instead of TOTSTK&OldTotStk in case of material.
      m.TotStk = m.TotStk+TotStk
      m.OldTotStk = m.OldTotStk+OldTotStk
    ELSE
      m.OnHand = m.OnHand +OnHand 
      m.OldQty = m.OldQty +OldQty 
      *B038829,1 ASH 12/25/2004 (End)
    ENDIF
  ENDSCAN
  lcS1 = Style
  lcC1 = Color
  lcD1 = Dyelot
  INSERT INTO (lcDetLin) FROM MEMVAR
  =SEEK(lcS1+lcC1+lcD1)
ENDDO
DELETE ALL FOR !EMPTY(cLocation)

*!*************************************************************
*! Name      : lfvPostDat
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for Post date.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPostDat()
*!*************************************************************
*!
FUNCTION lfvPostDat

PRIVATE lnAlias
lnAlias = SELECT(0)
IF ladata[6] <> {} AND ladata[5] > ladata[6]
  =gfModalGen('TRM42192B00000','DIALOG')
  _CUROBJ = OBJNUM(ladata[6])
  RETURN
ENDIF
IF ladata[6] = {}
  _CUROBJ = OBJNUM(ladata[6])
  RETURN
ENDIF
SELECT (lcBatHdr)
REPLACE dPostDate WITH laData[6]
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfOpnInfo
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Dummy Function.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfOpnInfo()
*!*************************************************************
*!
FUNCTION lfOpnInfo

RETURN .F.


*!*************************************************************
*! Name      : lfAddLTemp
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Add lie to temp file.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : PARAMETER lcItem,lcColor
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfAddLTemp()
*!*************************************************************
*!
FUNCTION lfAddLTemp

PARAMETER lcItem,lcColor
PRIVATE llToRet , lcOldAls , lcOldInd

lcOldAls = SELECT(0)
IF SEEK(lcItem+lcColor,lcTmpQuery)
  llToRet = .F.
ENDIF
IF  !SEEK(lcItem+lcColor,lcTmpQuery) AND lfCheckSty(lcItem,lcColor)
    INSERT INTO (lcTmpQuery) FROM MEMVAR
    SELECT(lcTmpQuery)
    REPLACE nStkVal WITH m.nStkVal
ENDIF
SELECT(lcOldAls)

*!*************************************************************
*! Name      : lfCheckSty
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Check if style locked before.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : PARAMETER lcItem,lcColor
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCheckSty()
*!*************************************************************
*!
FUNCTION lfCheckSty

PARAMETER lcCSty,lcCClr

PRIVATE lcOldAls , lcOldInd

lcOldAls = SELECT(0)
SELECT MDINVNTL
SET ORDER TO MDINVNTLS
=SEEK(lcCSty+lcCClr)
*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*SCAN REST WHILE style+color+cbattype+cbatch = ;
                lcCSty+lcCClr ;
                FOR IIF(llWareHus,cWareCode = ladata[8],.T.)
SCAN REST WHILE style+color+cbattype+cLkBatch = ;
                lcCSty+lcCClr ;
                FOR IIF(llWareHus,cWareCode = ladata[8],.T.)
  *IF SEEK(lcKey+MDINVNTL.CBATCH,'MDINVNTH') AND MDINVNTH.Type $ 'HML'
  IF SEEK(lcKey+MDINVNTL.cLkBatch,'MDINVNTH') AND MDINVNTH.Type $ 'HML'
*B603596,1 KHM 06/18/2000 (End)
    RETURN(.F.)
  ENDIF
ENDSCAN
SELECT (lcOldAls)

*!*************************************************************
*! Name      : lfAddLin
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function to add line to mdinvntkl.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : PARAMETER lcItem,lcColor
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfAddLin()
*!*************************************************************
*!
FUNCTION lfAddLin

PARAMETER lcItem,lcColr

PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT IIF(llMatModul,'FABDYE','STYDYE')
=SEEK(lcItem+IIF(EMPTY(lcColr),'',lcColr)+;
             IIF(llWareHus,ladata[8],''))
DO CASE
  CASE  lcCostMeth = 'S'  && Standard
    m.Cost    = IIF(llMatModul,FABRIC.CostUse,STYLE.TotCost)
    m.OldCost = IIF(llMatModul,FABRIC.CostUse,STYLE.TotCost)
  OTHERWISE       && 'A' Average
    m.Cost    = IIF(llMAtModul,FABDYE.nFAve_Cost,STYDYE.Ave_Cost)
    m.OldCost = IIF(llMAtModul,FABDYE.nFAve_Cost,STYDYE.Ave_Cost)
ENDCASE

SCAN REST WHILE IIF(llMatModul,fabric+color+cwarecode+dyelot = SUBSTR(m.ITEM,1,7)+m.Color+IIF(llWareHus,ladata[8],''),;
                               style+cwarecode+dyelot = m.Style+IIF(llWareHus,ladata[8],''))

  IF !llMatModul
    FOR lnInd = 1 TO 8
      Index = STR(lnInd,1)
      m.OldStk&Index = STYDYE.Stk&Index

      *E301851,1 BWA 03/04/2002 Use the value in the memory in case the imported data.[START]
      *m.Stk&Index    = STYDYE.Stk&Index
      IF !llLogic
        m.Stk&Index    = STYDYE.Stk&Index
      ENDIF
      *E301851,1 BWA 03/04/2002.[END]

    ENDFOR
  ENDIF
  IF !llLogic
    *B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand&OldQty instead of TOTSTK&OldTotStk in case of material.
    *m.OldTotStk = IIF(llMatModul,FAbDye.OnHand,STYDYE.TotStk)
    *m.TotStk    = IIF(llMatModul,FAbDye.OnHand,STYDYE.TotStk)
    *m.Dyelot    = IIF(llMatModul,FABDYE.Dyelot,STYDYE.Dyelot)
    *m.Scale     = IIF(llMatModul,'F',Style.Scale)
    IF llMatModul
      m.OldQty   = FAbDye.OnHand
      m.OnHand   = FAbDye.OnHand
      m.Dyelot   = FABDYE.Dyelot
      m.Scale    = 'F'
    ELSE
      m.OldTotStk = STYDYE.TotStk
      m.TotStk    = STYDYE.TotStk
      m.Dyelot    = STYDYE.Dyelot
      m.Scale     = Style.Scale
    ENDIF
    *B038829,1 ASH 12/25/2004 (End)
    INSERT INTO (lcBatLin) FROM MEMVAR
  ENDIF
ENDSCAN
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfLkStyInv
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Functio to lock style or material in journal file.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : lcItem,lcColor,llFalg
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfLkStyInv()
*!*************************************************************
*!
FUNCTION lfLkStyInv

PARAMETER lcItem,lcColor,llFalg

PRIVATE lnAlias
lnAlias = SELECT(0)

IF llMatModul
  SELECT FABDYE
  =SEEK(lcItem+lcColor+IIF(llWareHus,ladata[8],''))
  REPLACE dLlokDate WITH ladata[5],;
          dLPost    WITH ladata[6]
  SELECT MATINVJL
  SET FILTER TO
  SET ORDER TO
  SET ORDER TO MATINVJL
  =SEEK(lcItem+lcColor+IIF(llWareHus,ladata[8],''))
  SCAN REST WHILE cfabric+ccolor+cwarecode+cdyelot+crsession+cisession = ;
                  lcItem+lcColor+IIF(llWareHus,ladata[8],'')
    REPLACE lLockFlg WITH llFalg
  ENDSCAN
ELSE
  SELECT StyDye
  =SEEK(lcItem+IIF(llWareHus,ladata[8],''))
  REPLACE dLlokDate WITH ladata[5],;
          dLPost    WITH ladata[6]
  SELECT STYINVJL
  SET FILTER TO
  SET ORDER TO
  SET ORDER TO STYINVJL
  =SEEK(lcItem+IIF(llWareHus,ladata[8],''))
  SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode = ;
                  lcItem+IIF(llWareHus,ladata[8],'')
    REPLACE lLockFlg WITH llFalg
  ENDSCAN
  SELECT MDINVNTL
ENDIF
SELECT(lnAlias)


*!*************************************************************
*! Name      : lfChekBin
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Functio to Check bin.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfChekBin()
*!*************************************************************
*!
FUNCTION lfChekBin

PRIVATE llToRet

llToRet = .T.
IF llLoc AND !EMPTY(laRpTarget)
  lnOldAls = SELECT(0)
  SELECT WhsLoc
  SET ORDER TO WhsLocSt
  IF  llWareHus
    =IIF(llMatModul,SEEK(PADR(m.ITEM,19)+m.Color+ladata[8]),SEEK(m.Style+m.Color+ladata[8]))
  ELSE
    =IIF(llMatModul,SEEK(PADR(m.ITEM,19)+m.Color),SEEK(m.Style+m.Color))
  ENDIF
  llToRet = .F.
  SCAN REST WHILE style+color+cwarecode+clocation = ;
                      IIF(llMatModul,PADR(m.ITEM,19),m.Style)+m.Color+IIF(llWareHus,ladata[8],'');
            FOR ASCAN(laRpTarget,ALLTRIM(clocation)) <> 0
    llToRet = .T.
    EXIT
  ENDSCAN
  SET ORDER TO WhsLoc
  SELECT (lnOldAls)
ENDIF

RETURN(llToRet)

*!*************************************************************
*! Name      : lfPostMat
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Functio to Post material.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPostMat()
*!*************************************************************
*!
FUNCTION lfPostMat

IF llLinkGL
  lcGlSess    = gfSequence('GLSession')
  lnAdjCode   = 1
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  =gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = IIF(EMPTY(lcLinkCode),'DEFDEF',lcLinkCode)
  laGLDistAr[2,1] = IIF(EMPTY(lcLinkCode),'DEFDEF',lcLinkCode)
  laGLDistAr[1,2] = '015'
  laGLDistAr[2,2] = '016'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
  *STORE 'IA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE 'MA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
  *: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
  STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE ladata[5]  TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE lcGLPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcTmpGlDis TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF
*B038829,1 ASH 12/25/2004 (Begin) Increase the array width to hold the stock value.
*DIME laToSend[4,2]
DIME laToSend[5,2]
*B038829,1 ASH 12/25/2004 (End)
laToSend[1,1] = "lnMarkQty"
laToSend[1,2] = lnMarkQty
laToSend[2,1] = "lnMarkVal"
laToSend[2,2] = lnMarkVal
laToSend[3,1] = "lnLineNo"
laToSend[3,2] = 0
laToSend[4,1] = "llAdjYes"
laToSend[4,2] = llAdjYNo
*B038829,1 ASH 12/25/2004 (Begin) To hold the stock value in case of material locking.
laToSend[5,1] = "lnMStkVl"
laToSend[5,2] = laOldStk[2]
*B038829,1 ASH 12/25/2004 (End)

lnRet = gfMatCrl('9',PADR(lcPStyle,7),lcPColor,;
                     ladata[8],lcDye1,;
                     ladata[6],ladata[5],SPACE(6),;
                     laAdjust[9],laAdjust[10],;
                     'MATERIAL LOCKING',lcAdjReason,;
                     0,'','',@laGLDistAr,lcGlSess,'','','','','','','',@laToSend)

*!*************************************************************
*! Name      : lfPostSty
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Functio to Post style.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPostSty()
*!*************************************************************
*!
FUNCTION lfPostSty

*--G/L Array difinition and initialization.
IF llLinkGL
  lcGlSess   = gfSequence('GLSession')
  lnAdjCode  = 1
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  =gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = IIF(EMPTY(lcLinkCode),'DEFDEF',lcLinkCode)
  laGLDistAr[2,1] = IIF(EMPTY(lcLinkCode),'DEFDEF',lcLinkCode)
  laGLDistAr[1,2] = '006'
  laGLDistAr[2,2] = '007'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  
  *B603983,1 Add Inventory Locking type 'LK' instead of Inventory Adjustment [Begin]
  *STORE 'IA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE 'LK' TO laGLDistAr[1,4],laGLDistAr[2,4]
  *B603983,1 Add Inventory Locking type 'LK' instead of Inventory Adjustment [End]
  
  STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE ladata[5]    TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE lcGLPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcTmpGlDis TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF

*C123853,1 MHM 01/15/2005 Custom GFSTYCRL For David Luke[Start]
IF ASCAN(laEvntTrig,PADR("DLSTYCRL",10)) <> 0 
  =gfDoTriger("ICINVLK",PADR("DLSTYCRL",10))
ELSE
*C123853,1 MHM 01/15/2005 [End]
  *B039660,1 NNA 09/04/2005 (Begin) Pass Posting date to the GfstyCrl instead of the Patch creation date
  *lnRet = gfStyCrl('9',lcPStyle,ladata[8],lcDye1,ladata[5],'',@laAdjust,laAdjust[10],;
                       '',.T.,lcAdjReason,1,'MDINVNTL','NSTEPS',@laGLDistAr,0,'','',@laOldStk)
  lnRet = gfStyCrl('9',lcPStyle,ladata[8],lcDye1,ladata[6],'',@laAdjust,laAdjust[10],;
                       '',.T.,lcAdjReason,1,'MDINVNTL','NSTEPS',@laGLDistAr,0,'','',@laOldStk)
  *B039660,1 NNA (End)
*C123853,1 MHM 01/15/2005 [Start]
ENDIF
*C123853,1 MHM 01/15/2005 [End]
                     
*!*************************************************************
*! Name      : lfUpCost
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Update Cost.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfUpCost()
*!*************************************************************
*!
FUNCTION lfUpCost
PARAMETER lckey,lcBat,lcSt,lcClr,lcDye1

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcBatLin)
=SEEK(lckey+lcBat+lcSt+lcClr)
*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*SCAN REST WHILE cbattype+cbatch+style+color+dyelot = ;
                lckey+lcBat+lcSt+lcClr
SCAN REST WHILE cbattype+cLkBatch+style+color+dyelot = ;
                lckey+lcBat+lcSt+lcClr
*B603596,1 KHM 06/18/2000 (End)

  REPLACE COST WITH IIF(llMatModul,lnMMCost,lnMCost)
ENDSCAN
SELECT(lnAlias)


*!*************************************************************
*! Name      : lfGetAdjCd
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Get Adj Reason Code.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetAdjCd()
*!*************************************************************
*!
FUNCTION lfGetAdjCd

FOR lnInd = 1 TO ALEN(laAdjCode,1)
  IF laAdjCode[lnInd,2] = IIF(!EMPTY(&lcDetLin..cAdjReason),&lcDetLin..cAdjReason,lcDefAdjCd)
    lnAdjCode = LnInd
    SHOW GET lnAdjCode
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfTempFile
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function to Create Temp File.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfTempFile()
*!*************************************************************
*!
FUNCTION lfTempFile

IF llMatModul
  *B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand&OldQty instead of TOTSTK&OldTotStk in case of material.
  *CREATE TABLE &gcWorkDir.&lcTmpQuery (ITEM C(19) , COLOR C(06) , FabDesc C(20),;
                                       CLRDESC C(30),STOCK N(10) , OSTOCK N(10), nStkVal N(12,2),;
                                       BIN C(10) , Cost N(10,2),OCost N(10,2),;
                                       CADD_USER C(10),CADD_TIME C(11),DADD_DATE D)
  CREATE TABLE &gcWorkDir.&lcTmpQuery (ITEM C(19) , COLOR C(06) , FabDesc C(20),;
                                       CLRDESC C(30),STOCK N(12,3) , OSTOCK N(12,3), nStkVal N(12,2),;
                                       BIN C(10) , Cost N(10,3),OCost N(10,3),;
                                       CADD_USER C(10),CADD_TIME C(11),DADD_DATE D)
  *B038829,1 ASH 12/25/2004 (End)
  INDEX ON ITEM+Color TAG &lcTmpQuery
ELSE
  CREATE TABLE &gcWorkDir.&lcTmpQuery (STYLE C(19) , COLOR C(06) , StyDesc C(60),;
                                       STOCK N(10) , OSTOCK N(10),nStkVal N(12,2),;
                                       BIN C(10) , Cost N(10,2),OCost N(10,2),;
                                       CADD_USER C(10),CADD_TIME C(11),DADD_DATE D)
  INDEX ON Style+Color TAG &lcTmpQuery
ENDIF
*--- Create Temp Header file
SELECT MDINVNTH
=AFIELDS(laTpFlds)
lnTpFlds = ALEN(laTpFlds,1)
DIME laTpFlds[lnTpFlds+1,4]
laTpFlds[lnTpFlds+1,1] = 'nSteps'
laTpFlds[lnTpFlds+1,2] = 'N'
laTpFlds[lnTpFlds+1,3] = 2
laTpFlds[lnTpFlds+1,4] = 0

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*=gfCrtTmp(lcBatHdr,@laTpFlds,[cbattype+cbatch],lcBatHdr)
=gfCrtTmp(lcBatHdr,@laTpFlds,[cbattype+cLkBatch],lcBatHdr)
*B603596,1 KHM 06/18/2000 (End)

SELECT (lcBatHdr)
USE
=gfOpenFile(gcWorkDir+lcBatHdr,lcBatHdr,'EX')
SET ORDER TO

*--- Create Temp Line file
SELECT MDINVNTL
=AFIELDS(laTpFlds)
DIME laIndexArr[2,2]

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*laIndexArr[1,1] = [cbattype+cbatch+style+color+dyelot+clocation]
laIndexArr[1,1] = [cbattype+cLkBatch+style+color+dyelot+clocation]
*B603596,1 KHM 06/18/2000 (End)

laIndexArr[1,2] = [lcBatInd]

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*laIndexArr[2,1] = [style+color+dyelot+clocation+cbattype+cbatch]
laIndexArr[2,1] = [style+color+dyelot+clocation+cbattype+cLkBatch]
*B603596,1 KHM 06/18/2000 (End)

laIndexArr[2,2] = [lcStyInd]
=gfCrtTmp(lcBatLin,@laTpFlds,@laIndexArr,lcBatLin)
SELECT (lcBatLin)
USE
=gfOpenFile(gcWorkDir+lcBatLin,'','EX')
SET ORDER TO



*!*************************************************************
*! Name      : lfTmpBtchL
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function to Update mdwn line and hewder.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfTmpBtchL()
*!*************************************************************
*!
FUNCTION lfTmpBtchL

IF llMatModul
  SELECT FABDYE
  GOTO TOP
ELSE
  SELECT STYDye
  GOTO TOP
ENDIF

*---Start Update Heder Temp.
m.Date      = ladata[5]
m.cWareCode = ladata[8]
m.Type      = ladata[3]
m.cBatType  = lcKey
m.content   = ladata[4]

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch.
*m.cBatch    = laData[2]
m.cLkBatch    = laData[2]
*B603596,1 KHM 06/18/2000 (End)

INSERT INTO (lcBatHdr) FROM MEMVAR
*---End Update Heder Temp.

*C123853,1 MHM 01/15/2005 get temp batch per bin location [Start]
IF ASCAN(laEvntTrig,PADR("DLTMPBAT",10)) <> 0 
  =gfDoTriger("ICINVLK",PADR("DLTMPBAT",10))
ELSE
*C123853,1 MHM 01/15/2005 [End]
  SELECT(lcTmpQuery)
  GOTO TOP
  SCAN
    SCAT MEMVAR MEMO
    SELECT IIF(llMatModul,'FABDYE','STYDYE')
    REPLACE dLlokDate WITH ladata[5]
    m.Style     = IIF(llMatModul,m.ITEM,m.Style)
    m.Creason   = IIF(llMatModul,'MATERIAL LOCK INVENTORY     ' ,;
                                 'STYLE LOCK INVENTORY     ')
    m.cWareCode = ladata[8]
    IF llMatModul
    *--- Parameters : 1-Item ,2-Color ,3-MultiMare
      =lfAddLin(SUBSTR(m.ITEM,1,7),m.Color)
    ELSE
      =lfAddLin(m.style,SPACE(06))
    ENDIF
  ENDSCAN

*C123853,1 MHM 01/15/2005 [Start]
ENDIF
*C123853,1 MHM 01/15/2005 [End]

*!*************************************************************
*! Name      : lfvCont
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for contents field.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCont()
*!*************************************************************
*!
FUNCTION lfvCont

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcBatHdr)
REPLACE Content WITH laData[4]
SELECT(lnAlias)


*!*************************************************************
*! Name      : lpvActBar
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Functio for bads.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lpvActBar()
*!*************************************************************
*!
FUNCTION lpvActBar

DO CASE
  CASE BAR() = 1
   *--POST
   =lfvPost()
  CASE BAR() = 2
  *---Hold
   =lfHoldP()
   SHOW GETS
ENDCASE


*!*************************************************************
*! Name      : lfCrUnSess
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Functio to create uncomplete session.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCrUnSess()
*!*************************************************************
*!
FUNCTION lfCrUnSess
PARA lcUnCmSta,lcPushBt

lnAlias = SELECT()
SELECT UNCMSESS
IF lcUnCmSta='O'
  IF !SEEK('O'+lcUnCmPgId+PADR(gcUser_id,10)+lcSession)
    IF !SEEK('I'+lcUnCmPgId+PADR(gcUser_id,10)) 
      INSERT INTO UNCMSESS ;
      (Status,cUTranType,cUserId,cSession,cProgram,cCurrScr,dTranDate,cTranTime,cCurrObj) VALUES ;
      ('O',lcUnCmPgId,gcUser_id,lcSession,'ICINVLK','',gdSysDate,TIME(),lcPushBt)
    ELSE
      REPLACE Status    WITH 'O',;
              cSession  WITH lcSession,;
              dTranDate WITH gdSysDate,;
              cTranTime WITH TIME(),;
              ccurrobj  WITH lcPushBt
    ENDIF
  ELSE
    REPLACE cCurrObj WITH lcPushBt
  ENDIF
  lcFiles = "lcBatLin,"+lcBatLin+","+[lcBatInd]+";lcBatHdr,"+lcBatHdr+","+lcBatHdr+";lcTmpQuery,"+lcTmpQuery+","+lcTmpQuery+";"
  IF UNCMSESS.cCurrObj = 'pbPost'
    lcFiles = lcFiles + "lctmpGlDis,"+lctmpGlDis+",;"
  ENDIF
  
  = gfSavSess(lcUnCmPgId, lcFiles, @laVariables,lcSession)
  =RLOCK('UNCMSESS')

ELSE  && lcUnCmSta $ 'IC'
 
  *--Cancel OR Complete unCmSess record.
  IF SEEK('O'+lcUnCmPgId+PADR(gcUser_id,10)+lcSession)

    REPLACE Status WITH lcUnCmSta
    UNLOCK

  ENDIF
  llContinue = .F.

ENDIF
SELECT(lnAlias)
RETURN


*!*************************************************************
*! Name     : lfChkUnComS
*! Developer: Ahmed Salah Shalaby -(SSH)
*! Date     : 20/11/99
*! Purpose  : Check if there an uncomplete session.
*!*************************************************************
FUNCTION lfChkUnComS
PARAMETERS llFrmSetup
PRIVATE    llFondSess

llGoAndChk = IIF(llFrmSetup, .F., llGoAndChk)
llFondSess = .F.
IF (llFrmSetup AND llChkUnCom) OR !llFrmSetup
  llChkUnCom = .F.
  laScrMode  = .F.
  lcMsgTxt   = 'Inventory Locking'
  IF gfUnCompSession(lcUnCmPgId, lnSessNo, lcMsgTxt)
    llFondSess = .T.
    llContinue = .T.
    llShow     = .F.
    llNoShow   = .F.
    lcSession  = UNCMSESS.cSession
    lcCurObj   = ALLTRIM(UnCmSess.cCurrObj)
    SELECT (lcBatHdr)
    SCATTER FIELDS &lcScFields TO laData
    SELECT mdinvnth
    SCATTER FIELDS &lcScFields TO laData
    SELECT(lcTmpQuery)
    GOTO TOP
    =lfActsBrow()
    llViewMode = laScrMode[3]
    IF !EMPTY(lcCurObj)
       DO CASE
         CASE lcCurObj = 'pbSav'
           DO lpSavScr
         CASE lcCurObj = 'pbPost'
           =LFVPOST()
         CASE lcCurObj = 'pbDlt'
           DO lpDelScr
       ENDCASE
       llGoAndChk = .T.
       laScrMode  = .F.
       IF llViewMode
         laScrMode[2] = .T.
       ELSE
         laScrMode[1] = .T.
       ENDIF
    ENDIF
    SHOW GETS 
  ELSE
   =lfTempFile()
   laScrMode[1]  = .T.
  ENDIF
ENDIF
RETURN (llFondSess)


*!*************************************************************
*! Name      : lpClsScr
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : VAlid Functio to close screen.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lpClsScr()
*!*************************************************************
*!
FUNCTION lpClsScr

*--Cancel unCmSess record.
=lfCrUnSess('I',' ')

*!*************************************************************
*! Name      : lfHoldP
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : VAlid Functio to hold batch.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lpClsScr()
*!*************************************************************
*!
FUNCTION lfHoldP

lnOld = SELECT(0)
SELECT (lcBatHdr)

IF laData[3] = 'H'
  laData[3] = 'M'
  REPLACE TYPE WITH 'M'
  lcStatColor = "RGB(255,255,255,0,128,0)"
ELSE
  laData[3] = IIF(laData[3]<>'P','H','P')
  REPLACE TYPE WITH 'H'
  lcStatColor = "RGB(255,255,255,255,0,0)"
ENDIF
=lfRefresh('Icinvlk1')
SELECT (lnOld)

*!*************************************************************
*! Name      : lfActDBrow
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Function to avtivate detail screen browse.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfActDBrow()
*!*************************************************************
FUNCTION lfActDBrow

SELECT (lcDetLin)
lnCurrRec = RECNO()
IF llMatModul
  IF llDyelot
    *B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand&OldQty instead of TOTSTK&OldTotStk in case of material.
    *lcBrfield1 = [Style:10    :H = 'Item'        :W=.F. ,] +;
                 [COLOR       :H = 'Color'       :W=.F. ,] +;
                 [Dyelot      :H = 'Dyelot'      :W=.F. ,] +;
                 [cLocation   :H = 'Bin'         :W=.F. ,] +;
                 [OldTotStk   :H = 'Old OnHand'  :W=.F. ,] +;
                 [TotStk      :H = 'New OnHand'  :W=.F. ]
    lcBrfield1 = [Style:10    :H = 'Item'        :W=.F. ,] +;
                 [COLOR       :H = 'Color'       :W=.F. ,] +;
                 [Dyelot      :H = 'Dyelot'      :W=.F. ,] +;
                 [cLocation   :H = 'Bin'         :W=.F. ,] +;
                 [OldQty      :H = 'Old OnHand'  :W=.F. ,] +;
                 [OnHand      :H = 'New OnHand'  :W=.F. ]                 
  ELSE
    *lcBrfield1 = [Style:10    :H = 'Item'        :W=.F. ,] +;
                 [COLOR       :H = 'Color'       :W=.F. ,] +;
                 [cLocation   :H = 'Bin'         :W=.F. ,] +;
                 [OldTotStk   :H = 'Old OnHand'  :W=.F. ,] +;
                 [TotStk      :H = 'New OnHand'  :W=.F. ]
    lcBrfield1 = [Style:10    :H = 'Item'        :W=.F. ,] +;
                 [COLOR       :H = 'Color'       :W=.F. ,] +;
                 [cLocation   :H = 'Bin'         :W=.F. ,] +;
                 [OldQty      :H = 'Old OnHand'  :W=.F. ,] +;
                 [OnHand      :H = 'New OnHand'  :W=.F. ]
  *B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand&OldQty instead of TOTSTK&OldTotStk in case of material.
  ENDIF
  BROWSE FIELDS &lcBrField1;
         WHEN lfwBrows2();
         VALID :F lfvBrowse2() ;
         WINDOW ICINVLMb IN WINDOW (gcHost);
         LOCK 0;
         NOAPPEND;
         NOCLEAR;
         NODELETE;
         NOMENU;
         NOWAIT;
         SAVE;
         TITLE lcMDBrowTt

ELSE
  IF llDyelot
    lcBrfield1 = [Style       :H = lcStyPic      :W=.F. ,] +;
                 [Dyelot      :H = 'Dyelot'      :W=.F. ,] +;
                 [cLocation   :H = 'Bin'         :W=.F. ,] +;
                 [OldTotStk   :H = 'Old OnHand'  :W=.F. ,] +;
                 [TotStk      :H = 'New OnHand'  :W=.F. ]
  ELSE
    lcBrfield1 = [Style       :H = lcStyPic      :W=.F. ,] +;
                 [cLocation   :H = 'Bin'         :W=.F. ,] +;
                 [OldTotStk   :H = 'Old OnHand'  :W=.F. ,] +;
                 [TotStk      :H = 'New OnHand'  :W=.F. ]
  ENDIF
  BROWSE FIELDS &lcBrField1;
         WHEN lfwBrows2();
         VALID :F lfvBrowse2() ;
         WINDOW ICINVLSb IN WINDOW (gcHost);
         LOCK 0;
         NOAPPEND;
         NOCLEAR;
         NODELETE;
         NOMENU;
         NOWAIT;
         SAVE;
         TITLE lcMDBrowTt

ENDIF

*!*************************************************************
*! Name      : lfvNewOn
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for new on hand in material detail screen.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvNewOn()
*!*************************************************************
*!
FUNCTION lfvNewOn

SELECT (lcDetLin)
IF llMatModul
  *B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand instead of TOTSTK in case of material.
  *REPLACE TOTSTK WITH lnMNewTot
  REPLACE OnHand WITH lnMNewTot
  *B038829,1 ASH 12/25/2004 (End)
  IF (llDyelot AND llStyDye) OR (llLoc AND ALEN(laMLoc) > 1)
    =lfActDBrow()
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvNewCs
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for new Cost in detail screen.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvNewCs()
*!*************************************************************
*!
FUNCTION lfvNewCs

SELECT (lcDetLin)
IF llMatModul
  REPLACE ALL COST WITH lnMMCost
ELSE
  REPLACE ALL COST WITH lnMCost
ENDIF
GOTO TOP

*!*************************************************************
*! Name      : lfvAdj
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for Adj reason code in detail screen.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvAdj()
*!*************************************************************
*!
FUNCTION lfvAdj

lcAReason = laAdjCode[lnAdjCode,2]
*IF (llDyelot AND llStyDye) OR (llLoc AND ALEN(laMLoc) > 1)
SELECT (lcDetLin)
*ELSE
*  SELECT (lcBatLin)
*ENDIF
REPLACE cAdjReason WITH lcAReason

*!*************************************************************
*! Name      : lfvRes
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid function for reason in detail screen.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvRes()
*!*************************************************************
*!
FUNCTION lfvRes

*IF (llDyelot AND llStyDye) OR (llLoc AND ALEN(laMLoc) > 1)
SELECT (lcDetLin)
*ELSE
*  SELECT (lcBatLin)
*ENDIF
IF llMatModul
  REPLACE cReason WITH lcMReason
ELSE
  REPLACE cReason WITH lcReason
ENDIF


*!*************************************************************
*! Name      : lfStyDye
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Functio to check if style/item is dyelot.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfStyDye()
*!*************************************************************
*!
FUNCTION lfStyDye
PARAMETER lcSty1,lcColr1

llRet = .F.
IF llMatModul
  =SEEK(SUBSTR(lcSty1,1,7)+lcColr1,'Fabric')
  llRet = (Fabric.cDye_Flg='Y')
ELSE
  =SEEK(lcSty1,'Style')
  llRet = (Style.cDye_Flg='Y')
ENDIF
llStyDye = llRet
RETURN (llRet)



*!*************************************************************
*! Name      : lfBackUp
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Functio to BackUp Data.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfBackUp()
*!*************************************************************
*! Commentes : This function save files in the ARIA27\Work\InvLBkup\
*!*************************************************************
*!
FUNCTION lfBackUp

PRIVATE lcPat 
lcPat = FULLPATH('')
IF IIF(llMatModule,!FILE(gcworkdir+'InvLBkup'+'\'+'Fabric.DBF'),;
                   !FILE(gcworkdir+'InvLBkup'+'\'+'Style.DBF'))
  SET DEFA TO &gcworkdir
  !MD InvLBkup
ELSE
  SET DEFA TO gcworkdir+'InvLBkup'
ENDIF

SET DEFA TO &gcDataDir
lcToDo = '!COPY MDINVNTH.* '+ gcworkdir+[InvLBkup]
&lcToDo
lcToDo = '!COPY MDINVNTL.* '+ gcworkdir+[InvLBkup]
&lcToDo
lcToDo = '!COPY GLDIST.* '+ gcworkdir+[InvLBkup]
&lcToDo
IF llMatModul
  lcToDo = '!COPY FABRIC.* '+ gcworkdir+[InvLBkup]
  &lcToDo
  lcToDo = '!COPY FABDYE.* '+ gcworkdir+[InvLBkup]
  &lcToDo
  lcToDo = '!COPY MATINVJL.* '+ gcworkdir+[InvLBkup]
  &lcToDo
ELSE
  lcToDo = '!COPY STYLE.* '+ gcworkdir+[InvLBkup]
  &lcToDo
  lcToDo = '!COPY STYDYE.* '+ gcworkdir+[InvLBkup]
  &lcToDo
  lcToDo = '!COPY STYINVJL.* '+ gcworkdir+[InvLBkup]
  &lcToDo
ENDIF
SET DEFA TO &lcPat


*!*************************************************************
*! Name      : lfVZeroStk
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : zero inventory
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfVZeroStk()
*!*************************************************************
*!
*: E#500339,1 SSH 19/04/00 Add new buttons to zero inventory
FUNCTION lfVZeroStk
PRIVATE lnOld

lnOld = SELECT(0)
SELECT (lcTmpQuery)
REPLACE ALL STOCK WITH 0
GOTO TOP
SELECT(lcBatLin)
FOR lnInd = 1 TO 8
  lcStr = STR(lnInd,1)
  REPLACE ALL Stk&lcStr WITH 0
ENDFOR
*B038829,1 ASH 12/25/2004 (Begin) Use the new field OnHand&OldQty instead of TOTSTK&OldTotStk in case of material.
IF llMatModul
  REPLACE ALL OnHand WITH 0
*B038829,1 ASH 12/25/2004 (End)
ELSE
  REPLACE ALL TotStk WITH 0
ENDIF
GOTO TOP
SELECT (lcBatHdr)
REPLACE TYPE  WITH 'M'
laData[3] = 'M'
=lfwBrows()
SELECT(lnOld)
*--- End Inventory Locking - (SSH)

*E301851,1 BWA 03/04/2002 The function of to get the EXCEL.CSV file.[START]
*!*************************************************************
*! Name      : lfvPathSt.PRG  (E# 301851 dueto C#101624)
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 02/27/2001
*! Purpose   : Valid Function For Path Button.
*!*************************************************************
*! Called from : ININVLK2.SPX
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example   : =lfvPathSt()
*!*************************************************************
FUNCTION lfvPathSt

lcMultWare = ladata[8]
DIMENSION laSFile[1,1]
STORE ' ' TO laSFile

CREATE TABLE (gcWorkDir+lcInport) (STYLE C(12) , COLOR C(6) , Desc C(20) ,;
                                   Stk1 N(10)  , Stk2 N(10) , Stk3 N(10) ,;
                                   Stk4 N(10)  , Stk5 N(10) , Stk6 N(10) ,;
                                   Stk7 N(10)  , Stk8 N(10) , TOTSTK N(7))
CREATE TABLE (gcWorkDir+lcRejSty) (WARECODE C(6), STYLE C(19), REASON C(60))
INDEX ON WARECODE + STYLE TAG (lcRejSty)

lcOldPath  = FULLPATH("")
lcPathName = GETFILE('CSV', 'Browse For Excel.CSV', 'Import',1) 
lcFilNamMu = SUBSTR(lcPathName , RAT( "\" , lcPathName) + 1)
lcPathName = LEFT(lcPathName , RAT( "\" , lcPathName) - 1 )

IF EMPTY(lcPathName)
  STORE SPACE(40) TO lcPathName
  =lfRefresh()
  SET DEFAULT TO (lcOldPath)
  RETURN
ELSE
  =lfRefresh()
ENDIF
SET DEFA TO (lcPathName)

lcDot = ATC("." , lcFilNamMu , 1)
IF SUBSTR(lcFilNamMu , lcDot + 1 , 3 ) # "CSV"
  SET DEFAULT TO (lcOldPath)
  *--- "This file cannot be selected. You must select a file of type CSV"
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'This file cannot be selected. You must select a file of type CSV.')
  lcPathName = SPACE(40)
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
SET DEFAUL TO (lcOldPath)
_CUROBJ = OBJNUM(pbMkZer)

SELECT (lcInport)
ZAP
APPEND FROM (ALLT(lcpathname)+IIF(RIGHT(lcPathName,1) <> "\","\","")+;
             lcFilNamMu) DELIMITED
GO 2
ldDate = CTOD(STYLE)
SKIP
lcWare = PADR(ALLT(STYLE),6)
ladata[8] = lcWare
ladata[5] = ldDate
llwarhosM = .T.
SHOW GET ladata[8]
SHOW GET ladata[5]

IF SEEK(ladata[8] , 'WareHous')
  =lfSelData(.T.)
ELSE
   *---Text : 'No Record Selected for the report..!'
  ladata[8] = IIF(llwarHosM,SPACE(6),lcMultWare)
  ladata[5] = gdSysDate
  SHOW GET ladata[8]
  SHOW GET ladata[5]
  =gfModalGen('TRM00052B00000','DIALOG')  
  _CUROBJ = OBJNUM(ladata[8])
  RETURN
ENDIF

IF llwarHosM AND RECCOUNT(lctmpquery) = 0
  ladata[5] = gdSysDate
  SHOW GET ladata[5]
  IF llWareHus
    ladata[8] = SPACE(6)
    SHOW GET ladata[8]
    _CUROBJ = OBJNUM(ladata[8])
  ENDIF
ENDIF

*--End of lfvPathSt.
*!*************************************************************
*! Name      : lfPrnRep.PRG  (E# 301851 dueto C#101624)
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 02/27/2001
*! Purpose   : Function To Print the rejected styles.
*!*************************************************************
*! Called from : ININVLK.PRG
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example   : =lfPrnRep()
*!*************************************************************
FUNCTION lfPrnRep

R_WIDTH = 'N'
lcRpForm = 'ICREJST'

DO CASE
  CASE  lnanswer =  1
    OGPlatForm = 'WINDOW'
    lcOGPlatForm = OGPlatForm
    gcDevice  = "SCREEN"
    gcOutFile = gcWorkDir+gfTempName()+".TXT"      
    SET PRINTER TO &gcOutFile     && set printer to temprory file name
    SET DEVICE TO PRINTER
  CASE  lnanswer =  2
    OGPlatForm = 'DOS'
    gcDevice   = "PRINTER"
    lcOGPlatForm = OGPlatForm
    IF !pSetup(.T.)
      llCancel = .T.
      SET DEVICE TO SCREEN
      RETURN
    ENDIF
    SET DEVICE TO SCREEN
    SET PRINTER TO &gcOutFile
    SET DEVICE TO PRINTER
  CASE  lnanswer =  3
    llCancel = .T. 
    SET DEVICE TO SCREEN
    RETURN
ENDCASE
lcDefa = FULLPATH('')
SET DEFA TO (gcrepHome)
SELECT (lcRejSty)
GOTO TOP
*B608128,1 TMI [Start] set the procedure to the icreport.APP that includes the gfDispRe global function 
lcProc = SET('PROC')
SET PROC TO (GCREPHOME+'icreport.APP')
*B608128,1 TMI [End  ] 
DO gfDispRe WITH ('ICREJST.FRX')
*B608128,1 TMI [Start] restore the old setting
SET PROC TO &lcProc
*B608128,1 TMI [End  ] 
SET DEFA TO (lcDefa)
SET DEVICE TO SCREEN

*--End of lfPrnRep.
*E301851,1 BWA 03/04/2002.[END]
 
*B039107,1 EIH 03/03/2005 Fix bug of disable the NewCost field in case of standard cost method . [start]
*!*************************************************************
*! Name      : lfCostICLK
*! Developer : Ehab Ismail Hamed  - (EIH)
*! Date      : 03/03/05
*! Purpose   : In screen ICINVT_A disable the Newcost field 
*!           : In case of standard cost method .
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  gfGetMemVar
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfCostICLK()
*!*************************************************************
FUNCTION  lfCostICLK
IF (ALLTRIM(lcCostMeth) == 'S')
  SHOW GET lnMCost DISABLE
ENDIF
*!*************************************************************
*! Name      : lfCostMALK
*! Developer : Ehab Ismail Hamed  - (EIH)
*! Date      : 03/03/05
*! Purpose   : In screen ICINVMM and  ICINVMLM disable the Newcost field 
*!           : In case of standard cost method .
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  gfGetMemVar
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfCostMALK()
*!*************************************************************
FUNCTION  lfCostMALK
IF (ALLTRIM(lcCostMeth) == 'S')
  SHOW GET lnMMCost DISABLE
ENDIF
*B039107,1 EIH 03/03/2005 [end]