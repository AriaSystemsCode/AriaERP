*:************************************************************************
*: Program file  : POISSBC.PRG
*: Program desc. : Program To Issue Inter Location Batch by bar code
*: For screen    : POISSBC/0/1/2/3 .Scx/Sct
*:         System: ARIA BUSINESS SYSTEM
*:         Module: - Style Purchase Order
*:      Developer: Ahmed Maher (AMH)
*:************************************************************************
*: Documented *C200170,1
*:************************************************************************
*: Calls         : Screens : POISSBC.Spx
*:************************************************************************
*:  Modifications :
*:B604651,1 AMH 07/10/2001 Use poreport.app instead of gfdispre to 
*:B604651,1                print log report.
*:************************************************************************
*

*--Cannot use the inter Location P/o if no multi location is used.
llWareHous = ALLTRIM(gfGetMemVar('M_WareHouse',gcAct_Comp)) = 'Y'
IF !llWareHous
  *** The system has not been setup to use multiple locations. Cannot proceed. ***
  =gfModalGen('TRM34155B00000','DIALOG')
  glQuitting = .T.
  RETURN
ENDIF

*-- Assign the temporary screens names'.
STORE " " TO lcRcAll0 , lcRcAll1 , lcRcAll2 , lcRcAll3

*-- Assign the main temprary files names'.
STORE " " TO lcTmpCurs

*-- Assign the temp. files.
STORE " " TO lcTmpLine , lcGlDist

*-- Get the prompt of the available pictures on the screen.
lcTBrwBmp = gcBmpHome + "ExtKey.BMP"
lcTNewBmp = gcBmpHome + "New1.BMP"
lcTRemBmp = gcBmpHome + "Rem1.BMP"
lcTProBmp = gcBmpHome + "Proceed.BMP"
lcTEdtBmp = gcBmpHome + "Edit3.BMP"

*-- Define arrays.
*-- Array hold separators & its start position & segment lenght.
DECLARE laSepartor[1,3]
*-- Array hold the segmants specifications of the style code structure.
DECLARE laStySeg[1,1]

*-- Array hold the setup parameters.
DECLARE laCrSetups[14,2] 

laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)

*-- Define variables hold needed parameters from setup file.
llWareHous = .F.             && Single warehouse or multi warehouse.
llWareLoc  = .F.             && Warehouse location.
llStyDye   = .F.             && System use Style dyelot or not.
llLoc      = .F.             && If Multiple location or not.
llConfirm  = .F.             && If set confirmation is on or off.
lcStyUPC   = "S"             && If system works by style or UPC.
lcSysType  = ""              && System type if Point of sale.

llAcpZroPc = .F.
llAcpZrStk = .F.
lcCostMth  = ' '

lcCnfrm    = ""              && Variable hold the confirmation setting.

*-- Define the main variables in all transactions tyes.
llCollect  = .F.             && Flag to define start processing or not.
rbMethod   = 2               && Radio button (Import file , Scan line by line)
rbScanBy   = 1               && Radio button Scan by (Style , UPC)
lcItem     = "\<Style      " && Prompt of the item in item radio button (Style , Material)
lcCollect  = IIF(rbMethod = 1 , "\<Collect" , "S\<tart    ")     && Prompt of the collect & Start button.
lcImpFile  = ""              && Variable hold the file to be imported.
llUPC      = (rbScanBy = 2)  && Flag to know if validate item or UPC.
lnMarker   = 0               && Variable hold the record # in the browse.
llBrowse   = .F.             && Flag to know if press the browse icon or not.
llStyle    = .T.             && Flag to know if validating style or material
lcOldValue = ""              && Variable hold old value of any string object.
lnOldValue = 0               && Variable hold old value of any numeric object.
ldDate     = gdSysDate       && Variable hold the receiving date.
lcDesc     = ""              && Variable hold the header description.

*-- Flag defines if I'm in add mode or not.
llCUpDate = .F.

*-- Define the variables needed for each line.
rbByLin    = 1               && Radio button on the line level to define UPC or item.
lcRefer    = ""              && Variable hold the reference.
lcStyle    = ""              && Variable hold the style.
lcStyDesc  = ""              && Variable hold the style description.
lcUPC      = ""              && Validate UPC #.
lnQty      = 0               && Line Qty.
lnLineCnt  = 0               && Liens counter hold the no. of lines in the browse.
lcLinStat  = "DISABLE"       && Variable hold if the lines are enabled or disabled.

llGenOrNum = .F.
lcSession  = ""
llMulCurr  = .F.
llLinkToGl = .F.
ldOldDate  = {}
STORE SPACE(0) TO lcbatch, lcSource, lcTarget
llImpCost  = .F.
STORE gdSysDate TO ldTrDate , ldRcvDate

*-- Define variable to know if saved inter location transfer or not.
llSav_ILT  = .F.

*-- Define variables hold the charcteristics of the style object.
lcStyHdr   = ""              && Variable hold the style header.
lcStyPic   = ""              && Variable hold the style picture.
lnStyLen   = 1               && Variable hold the style lenght.
lnItmLen   = 1               && Variable hold the style lenght.
lcUPCPic   = "X"+REPLICATE("9",11) && Variable hold the UPC pic.

lcMessage  = "Inter Location Batch"

*-- Variables hold the black & invisible color.
lcInvColor = "RGB(192,192,192,192,192,192)"   && Invisable color.
lcNorm2C   = "RGB(0,0,,,,,)"                  && Black Color.

llCostPrv  = .F.

*-- Variable hold the Browse name.
lcTmpBrow  = "Issue inter location batch"
*-- Variable hold the main base file.
lcMainFile = "CTKTRCVH"
*-- Seek expression to seek in the PosLn file.
lcSeek     = "N"
*-- Variable hold browse fields.
lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=.F. ,"+;
           "Style:R:28 , cSzDesc:R:9:H='Size' , Desc1:R:20:H='Description',"+;
           "lcReject=IIF(lAccepted,'Accepted','Rejected'):H='Status':R:10"

llNoShow  = .F.     && Flag to force the execution of the show procedure.

*-- Call global function in the main program to do the following : _
*-- Intialise all the variables & open all the files needed in
*-- this session and controling disabling and enabling of the
*-- menu bars and writting the screen names in the window bars.
IF !gfSetup()
  RETURN
ENDIF

lcScFields = "cStyType"


*-- If entering the screen for the first time.
IF !WEXIST(gcBaseWind)

  llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')
  *-- Style header.
  lcStyHdr = gfItemMask('HI')
  
  *-- Style picture + Size Picture.
  lcStyPic = gfItemMask('PI')+'9'
  
  *-- Style picture.
  lnItmLen = LEN(gfItemMask('PI'))
  
  *-- Get the style lenght.
  lnStyLen = LEN(lcStyPic)
  *-- Fill an array with the segments strucure, & loop in it to know
  *-- if there is separators or not in the style code strucure.
  =gfItemMask(@laStySeg)
  FOR lnCnt = 1 TO ALEN(laStySeg,1)
    IF !EMPTY(laStySeg[lnCnt,6])
      IF !EMPTY(laSepartor[1,1])
        DIMENSION laSepartor[ALEN(laSepartor,1)+1,3]
      ENDIF
      laSepartor[ALEN(laSepartor,1),1] = laStySeg[lnCnt,6]        && Separator.
      laSepartor[ALEN(laSepartor,1),2] = laStySeg[lnCnt,4]        && Start position of the segment.
      laSepartor[ALEN(laSepartor,1),3] = LEN(laStySeg[lnCnt,3])   && Lenght of the segment.
    ENDIF
  ENDFOR
  
  *-- Call gfGetMemVar once for all variables.
  DIMENSION laCrSetups[14,2]
  laCrSetups[1,1] = 'M_WareHouse'
  laCrSetups[2,1] = 'M_WareLoc'
  laCrSetups[3,1] = 'M_Dyelot'
  laCrSetups[4,1] = 'M_CONFIRM'
  laCrSetups[5,1] = 'M_STYLEUPC'
  *-- If create & issue inter location transfer.
  laCrSetups[6,1] = 'M_SYSTYPE'
  laCrSetups[7,1] = 'M_DROPWARE'
  laCrSetups[8,1] = 'M_GenStOrN'
  laCrSetups[9,1] = 'llMulCurr'
  laCrSetups[10,1]= 'M_Link_GL'
  laCrSetups[11,1]= 'M_LImpCost'
  laCrSetups[12,1]= 'M_ACPZROPC'
  laCrSetups[13,1]= 'M_ACPZRSTK'
  laCrSetups[14,1]= 'M_Cost_Meth'
  
  =gfGetMemVar(@laCrSetups,gcAct_Comp)
  llWareHous = (laCrSetups[1,2] = 'Y')
  llWareLoc  = (laCrSetups[2,2] = 'Y')
  llStyDye   = (laCrSetups[3,2] = 'Y')
  llConfirm  = (ALLTRIM(laCrSetups[4,2]) = "Y")
  lcStyUPC   = (ALLTRIM(laCrSetups[5,2]))
  rbScanBy   = IIF(OCCURS('PS',gcCmpModules) <> 0 , IIF(lcStyUPC = "U" , 2 , 1) , 2)
  rbByLin    = rbScanBy
  *-- If create & issue inter location transfer.
  lcSysType  = laCrSetups[6,2]
  llMulCurr  = laCrSetups[9,2]
  llLinkToGl = (laCrSetups[10,2] = 'Y')
  llImpCost  = laCrSetups[11,2]
  llAcpZroPc = (laCrSetups[12,2] = 'Y')
  llAcpZrStk = (laCrSetups[13,2] = 'Y')
  lcCostMth  = laCrSetups[14,2]
  
  *-- If create & issue inter location transfer.
  IF lcSysType = 'P'
    *-- If the global variable of the site ID is not defined.
    IF EMPTY(gcCurSite)
      *-- Flag to know if the istallation file is open or not.
      llOpenInst = .F.
      *-- Check if the installation program is open or not.
      IF !USED("SYCINST")
        *-- Set flag to true to close the istallation file.
        llOpenInst = .T.
        =gfOpenFile(gcSysHome+'SYCINST','','SH')
      ENDIF
      *-- Get the Site ID field.
      SELECT SYCINST
      lcSiteId = IIF(TYPE('SYCINST.CCURSITEID')#'C',SPACE(6),SYCINST.CCURSITEID)
      *-- If the installation file was closed before, close it.
      IF llOpenInst
        USE IN SYCINST
      ENDIF
    ELSE
      *-- If the global variable of the site ID is defined.
      lcSiteId = gcCurSite
    ENDIF
    *-- Get the location that has the site ID.
    SELECT WAREHOUS
    LOCATE FOR cSiteId = lcSiteId
    IF !FOUND()
      GO TOP
    ENDIF
  ENDIF
  
  *-- Assign temporary name for the cursor will hold the styles.
  lcTmpCurs = gfTempName()
  CREATE CURSOR (lcTmpCurs) (Item C(20) , lFoundUPC L(1))
  
  lcTmpLine = gfTempName()

  *-- If issue inter location batch.
  IF llLinkToGl
    lcGlDist = gfTempName()
  ENDIF
  *-- Call local function to create the temp. files needed for each program.
  =lfCreatLin()
  
  *-- Assign the temporary window names.
  lcRcAll0  = gfTempName()
  lcRcAll1  = gfTempName()
  lcRcAll2  = gfTempName()
  lcRcAll3  = gfTempName()

ENDIF

*-- Set filter to the suitable styles.
SELECT STYLE
SET FILTER TO !Make 

*-- Save the confirmation setting.
lcSetConf  = SET('CONFIRM')

*-- Restore if the lines are enabled or disabled.
lcLinStat = IIF(lnLineCnt > 0 .AND. ISSBATCH.lAccepted , "ENABLE" , "DISABLE")

SELECT (lcMainFile)

PUSH KEY
PUSH MENU _MSYSMENU

*-- Define bar to activate the browse.
DEFINE BAR 100 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 101 OF P01PU01 PROMPT lcTmpBrow KEY ALT+B
*-- Activate the browse when selecting its bar.
ON SELECTION BAR 101 OF P01PU01 ACTIVATE WINDOW (lcTmpBrow)

*-- Call the screen.
DO (gcScrDir+gcWinAppl+'\POISSBC.SPX')

*-- Release the browse window.
RELEASE WINDOW (lcTmpBrow)

POP MENU _MSYSMENU
POP KEY

*-- Release the style filter.
SELECT STYLE
SET FILTER TO

*-- Restore confirmation setting.
SET CONFIRM &lcSetConf

*-- If quitting the program erase the temp. files.
IF glQuitting
  
  *-- Close the cursor.
  USE IN (lcTmpCurs)
ENDIF

*!*************************************************************
*! Name      : lpShow
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Show procedure
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lpShow()
*!*************************************************************
*
FUNCTION lpShow

*-- To disable the edit bar in the record pad in the system menu.
laCtrStat[10] = 'DISABLE'
laCtrStat[1] = 'DISABLE'
laCtrStat[2] = 'DISABLE'
laCtrStat[3] = 'DISABLE'
laCtrStat[4] = 'DISABLE'
laCtrStat[5] = 'DISABLE'
laCtrStat[7] = 'DISABLE'
*-- Disable the browse button on the control pannel all the time.
SHOW GET pbBrws DISABLE

*-- Enable the browse invisable button.
SHOW GET ibTab ENABLE

DO CASE
  CASE laScrMode[1]
    *-- Define variable to know if saved inter location transfer or not.
    llSav_ILT  = .F.
    
    *-- Blank the temp. files.
    SELECT (lcTmpCurs)
    ZAP
    
    *-- Call local function to create the temp. files related to each program.
    =lfCreatLin()
    
    *-- Blank the needed variables.
    lcItem     = "\<Style      " && Prompt of the item in item radio button (Style , Material)
    STORE 2   TO rbMethod 
    lcCollect  = IIF(rbMethod = 1 , "\<Collect" , "S\<tart    ")   && Prompt of the collect & Start button.
    llUPC      = (rbScanBy = 2)  && Flag to know if validate item or UPC.
    rbByLin    = rbScanBy
    ldDate     = gdSysDate       && Variable hold the receiving date.
    lcLinStat  = "DISABLE"       && Variable hold if the lines are enabled or disabled.
    llStyle    = .T.
    STORE .F. TO llCollect , llBrowse , cbByCrtn
    STORE 0   TO lnMarker , lnOldValue , lnLineCnt, lnQty
    STORE ""  TO lcImpFile , lcOldValue , lcDesc , ;
                 lcStyle , lcStyDesc , lcUPC , lcRefer , ;
                 lcCarton , lcCuttkt , lcbatch, lcSource, lcTarget
    *-- Flag defines if I'm in add mode or not.
    llCUpDate = .F.
    *-- Blank the needed variables for Create & issue inter location transfer.
    STORE gdSysdate TO ldTrDate , ldRcvDate
    SHOW GET ldTrDate ENABLE
    SHOW GET ldRcvDate  ENABLE
    
    SHOW GET pbTarget ENABLE
    SHOW GET ibPO     ENABLE
    SHOW GET lcbatch     ENABLE
    
    =lfwMainBrw()
    *-- Refresh the objects in the first window "lcRcAll0"
    SHOW GET cbByCrtn  ENABLE
    SHOW GETS WINDOW (lcRcAll3) DISABLE ONLY
    SHOW GET ibTab    ENABLE
    SHOW GET ldDate   DISABLE
    SHOW GET lcDesc   DISABLE
    ACTIVATE WINDOW (lcRcAll0)
    ACTIVATE WINDOW (lcRcAll1)
  
  CASE laScrMode[4]
    SHOW GET ldTrDate  DISABLE
    SHOW GET ldRcvDate DISABLE
    SHOW GET pbSiteLoc DISABLE
    SHOW GET pbTarget  DISABLE
    SHOW GET ibPO      DISABLE
    SHOW GET lcbatch      DISABLE
    SHOW GET cbByCrtn DISABLE
    
    IF rbByLin = 1
      *-- Activate the style object to add new line.
      SHOW GET lcUPC   DISABLE
      SHOW GET lcStyle ENABLE
      _CUROBJ   = OBJNUM(lcStyle)
    ELSE
      *-- Activate the UPC object to add new line.
      SHOW GET lcStyle DISABLE
      SHOW GET lcUPC   ENABLE
      _CUROBJ   = OBJNUM(lcUPC)
    ENDIF
    SHOW GET ldDate   ENABLE
    SHOW GET lcDesc   ENABLE
ENDCASE

*-- Refresh the objects in the first window "lcRcAll0"
SHOW GET rbMethod  ENABLE
SHOW GET rbScanBy  ENABLE
IF rbMethod = 2
  lcImpFile = ""
  SHOW GET pbGetFil  DISABLE
  SHOW GET lcImpFile DISABLE
ELSE
  SHOW GET pbGetFil  ENABLE
  SHOW GET lcImpFile ENABLE
ENDIF

*-- Refresh the objects in the second window "lcRcAll1"
SHOW GET pbCollect,1 PROMPT lcCollect ENABLE
SHOW GET pbNewCt  DISABLE

IF !llCollect
  *-- If not start to collect items, enable all the objects in the 1st window.
  *-- Refresh window "MfRcAll0" objects.
  SHOW GET rbMethod  ENABLE
  IF rbMethod = 2
    lcImpFile = ""
    SHOW GET pbGetFil  DISABLE
    SHOW GET lcImpFile DISABLE
  ELSE
    SHOW GET pbGetFil  ENABLE
    SHOW GET lcImpFile ENABLE
  ENDIF
  SHOW GET rbScanBy  ENABLE
  
  *-- Refresh window "MfRcAll1" objects.
  SHOW GET pbNewCt   DISABLE
  SHOW GET pbCollect ENABLE
  
  *-- Refresh window "MfRcAll2" objects.
  SHOW GET rbByLin   DISABLE
  *-- Point to the right object.
  _CUROBJ = OBJNUM(lcbatch)
ELSE
  *-- If start to collect items, disable all the objects in the 1st window.
  *-- Refresh window "MfRcAll0" objects.
  SHOW GET rbMethod  DISABLE
  SHOW GET pbGetFil  DISABLE
  SHOW GET lcImpFile DISABLE
  SHOW GET rbScanBy  DISABLE
  
  *-- Refresh window "MfRcAll1" objects.
  SHOW GET pbCollect DISABLE
  
ENDIF

SELECT (lcMainFile)

*!*************************************************************
*! Name      : lfwMainBrw
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Function call the browse called from main screen
*!           : Setup.
*!*************************************************************
*! Calls     : lfWhenBrow, lfVldBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwMainBrw()
*!*************************************************************
*
FUNCTION lfwMainBrw

*-- Display the transactions lines in the browse from the temp. files
SELECT ISSBATCH
lnMarker = RECNO()

BROWSE FIELDS &lcRcvFld ;
       WINDOW  (lcRcAll2) ;
       IN WINDOW (gcBaseWind) ;
       NOMENU           ;
       NOEDIT           ;
       NOAPPEND         ;
       NODELETE         ;
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
	   KEY PADR(lcbatch,6) ;
       WHEN lfWhenBrow()  ;
       VALID :F lfVldBrow() ;
       TITLE lcTmpBrow

*!*************************************************************
*! Name      : lfVldBrow
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Valid function for the browse.
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfVldBrow()
*!*************************************************************
*
FUNCTION lfVldBrow

*-- Valid function executed inside the browse. to stop the browse if the 
*-- current window not the browse window.

IF !WONTOP(lcTmpBrow)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfWhenBrow
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : When function for the browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfWhenBrow()
*!*************************************************************
*
FUNCTION lfWhenBrow

*-- Refresh the objects in the screen if any movement happened in the browse.
SELECT ISSBATCH
*-- Change the record pointer.
lnMarker   = RECNO()
glFromBrow = .T.

IF BOF() .OR. EOF() .OR. lnLineCnt = 0
  lcLinStat = "DISABLE"
  SHOW GET pbRemLn   DISABLE
  SHOW GET pbSav     DISABLE
  SHOW GET pbDlt     DISABLE
  lnQty = 0
  SHOW GET lnQty
ELSE
  lcStyDesc  = ISSBATCH.DESC1
  lcLinStat  = IIF(ISSBATCH.lAccepted , "ENABLE" , "DISABLE")
  SHOW GET pbRemLn   ENABLE
  lnQty = 1
  SHOW GET lnQty
  IF llCollect
    SHOW GET pbSav     ENABLE
  ELSE
    SHOW GET pbDlt     ENABLE
  ENDIF
ENDIF

SHOW GET ldDate
SHOW GET lcDesc
*-- Refresh the objects in window MfRcAll3.

IF llCollect
  SHOW GET pbNewLn   ENABLE
  SHOW GET rbByLin   ENABLE
  IF rbByLin = 1
    *-- Activate the style object to add new line.
    SHOW GET lcUPC   DISABLE
    SHOW GET lcStyle ENABLE
    _CUROBJ   = OBJNUM(lcStyle)
  ELSE
    *-- Activate the UPC object to add new line.
    SHOW GET lcStyle DISABLE
    SHOW GET lcUPC   ENABLE
    _CUROBJ   = OBJNUM(lcUPC)
  ENDIF
ELSE
  SHOW GET pbNewLn   DISABLE
  SHOW GET rbByLin   DISABLE
ENDIF


SHOW GET lcStyDesc &lcLinStat

*-- Call function in the screen spr to refresh the say fields 
*-- in the main screen
=lfRefresh()

SHOW WINDOW (lcTmpBrow) REFRESH SAME

*!*************************************************************
*! Name      : lfActMain
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Activate function for the main screen
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfActMain()
*!*************************************************************
*
FUNCTION lfActMain

*-- If coming from the browse, Call global function to stop the browse.
IF glFromBrow
  = gfStopBrow()
  *-- Set the browse flag to false.
  glFromBrow = .F.
ENDIF

*-- If the screen browse is not active then clear the trapped keys.
IF !INLIST(WONTOP(), lcTmpBrow)
  *-- Clear all the trapped keys.
  ON KEY LABEL CTRL+Q
  ON KEY LABEL CTRL+W
  ON KEY LABEL Ctrl+ENTER
  ON KEY LABEL Ctrl+HOME
  ON KEY LABEL Ctrl+END
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
ENDIF

*!*************************************************************
*! Name      : lfDactMain
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Deactivate function for the main screen
*!*************************************************************
*! Calls     : lpTab , lpShTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfDactMain()
*!*************************************************************
*
FUNCTION lfDactMain

*-- Set the global flag "glFromBrow" to true only the screen browse is active.
glFromBrow = INLIST(WONTOP() , lcTmpBrow)

*-- If any of the screen's browses is active then trap the 
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF glFromBrow
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL TAB     DO lpTab
  ON KEY LABEL BACKTAB DO lpShTab
ENDIF

*!*************************************************************
*! Name      : lpTab
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpTab
*!*************************************************************
*
PROCEDURE lpTab

*-- Clear the TAB trap.
ON KEY LABEL TAB

DO CASE
  CASE laScrMode[4] .OR. laScrMode[2]
    ACTIVATE WINDOW gwcContrl1
    _CUROBJ = OBJNUM(pbSlct)
  CASE laScrMode[1]
    ACTIVATE WINDOW gwcContrl1
    _CUROBJ = OBJNUM(pbcptask)
ENDCASE

*-- Trap the TAB key.
ON KEY LABEL TAB DO lpTab

*!*************************************************************
*! Name      : lpShTab
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Trap of backtab key.
*!*************************************************************
*! Calls     : lpShTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpShTab
*!*************************************************************
*
PROCEDURE lpShTab

*-- Clear the backtab key.
ON KEY LABEL BACKTAB

DO CASE
  CASE laScrMode[4]
    ACTIVATE WINDOW (lcRcAll1)
    _CUROBJ = OBJNUM(lcbatch)
  CASE laScrMode[1] .OR. laScrMode[2]
    ACTIVATE WINDOW (lcRcAll1)
    _CUROBJ = OBJNUM(pbCollect)
ENDCASE

*-- Trap the BACKTAB key.
ON KEY LABEL BACKTAB DO lpShTab

*!*************************************************************
*! Name      : lfvMethod
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Valid function for method radio button:
*!           : (Import file , Scan line by line)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvMethod()
*!*************************************************************
*
FUNCTION lfvMethod

lcCollect = IIF(rbMethod = 1 , "\<Collect" , "S\<tart    ")

IF rbMethod = 2
  lcImpFile = ""
  SHOW GET pbGetFil  DISABLE
  SHOW GET lcImpFile DISABLE
ELSE
  SHOW GET pbGetFil  ENABLE
  SHOW GET lcImpFile ENABLE
ENDIF
SHOW GET pbCollect,1 PROMPT lcCollect

*!*************************************************************
*! Name      : lfvGetFil
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Valid function for Push button : <Get File>
*!           : Valid if select Import file method.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvGetFil()
*!*************************************************************
*
FUNCTION lfvGetFil

*-- Get the text file hold the styles.
lcImpFile = GETFILE('TXT', 'Select File', 'Select')
*-- Refresh the imported file variable.
SHOW GET lcImpFile

*!*************************************************************
*! Name      : lfvScanBy
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Valid function for radio button : (Style/Material or UPC)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvScanBy()
*!*************************************************************
*
FUNCTION lfvScanBy

*-- Flag to know if validate item or UPC
llUPC   = (rbScanBy = 2)

*-- Refresh the related objects on the lines data window.
rbByLin = rbScanBy
SHOW GET rbByLin
=lfRefresh(lcRcAll3)

*!*************************************************************
*! Name      : lfcOldVal
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/18/2001
*! Purpose   : When function for all string obejcts
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfcOldVal()
*!*************************************************************
*
FUNCTION lfcOldVal

lcOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfnOldVal
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/18/2001
*! Purpose   : When function for all nymeric obejcts
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfnOldVal()
*!*************************************************************
*
FUNCTION lfnOldVal

lnOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvBatch
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/21/2001
*! Purpose   : Valid function for batch object if issue inter 
*!           : location batch.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvBatch()
*!*************************************************************
*
FUNCTION lfvBatch
PRIVATE lcAlias

lcbatch = IIF(llBrowse , '?' , lcbatch)
IF !llBrowse .AND. (EMPTY(lcbatch) OR (lcbatch=lcOldValue .AND. MDOWN()))
  RETURN
ENDIF

llBrowse  = .F.
lcAlias   = ALIAS()
IF !SEEK("N"+lcbatch , 'CTKTRCVH')
  SELECT CTKTRCVH
  DIMENSION laTempData[1]
  STORE '' TO laTempData
  lcBrFields = [TmpRcvNum :8:H='Recv. #',]+;
               [cStatus:1:H="S",]+;
               [cDesc:24:H="Description",]+;
               [dDate:10:H="Date",]+;
               [nTotStk:8:H="Tot.Stk",]+;
               [nTotDam:8:H="Tot.Oth",]+;
               [nTotCan:8:H="Tot.Can"]

  =gfBrows(['N'],'TMPRCVNUM','laTempData','Temp. Receive Batchs')
   
  lcbatch = laTempData[1]
ENDIF 

IF EMPTY(lcbatch) OR EOF('CTKTRCVH')
  SELECT (lcAlias)
  _CUROBJ = OBJNUM(lcbatch)
  RETURN
ENDIF
SHOW GET lcbatch

*--Batch Validation.
llAbort   = .F.

IF !EMPTY(lcbatch)

  DO CASE
    CASE CTKTRCVH.CStatus = 'P'
      *--This temporary receiving batch is posted. Cannot proceed.
      = gfModalGen('TRM34070B42000','DIALOG','is posted')
      llAbort = .T.
    CASE CTKTRCVH.CStatus = 'I'
      *--This temporary receiving batch is posted. Cannot proceed.
      = gfModalGen('TRM34070B42000','DIALOG','is issued')
      llAbort = .T.
    CASE CTKTRCVH.CStatus <> 'A'
      *--This temporary receiving batch is not approved. Cannot proceed.
      = gfModalGen('TRM34070B42000','DIALOG','is not approved')
      llAbort = .T.
  ENDCASE

  IF llAbort
    SELECT (lcAlias)
    lcbatch    = SPACE(6)
    _CUROBJ = OBJNUM(lcbatch)
    RETURN
  ENDIF
ENDIF

lcSource = CTKTRCVH.VENDOR
lcTarget = CTKTRCVH.CWARECODE
laScrMode    = .F.
laScrMode[2] = .T.
SHOW GETS

SHOW GET pbEdt     DISABLE
SHOW GET pbBtm     DISABLE
SHOW GET pbNxt     DISABLE
SHOW GET pbPrvs    DISABLE
SHOW GET pbTop     DISABLE
SHOW GET lcbatch   DISABLE
SHOW GET lcSource
SHOW GET lcTarget
SHOW GET ibPo      DISABLE
  
IF SEEK(lcbatch,'ISSBATCH')
  lnLineCnt = 1
  SHOW GET pbRemLn ENABLE
  lnQty = 1
  SHOW GET lnQty
ELSE
  lnLineCnt = 0
  SHOW GET pbRemLn DISABLE
  SHOW GET pbDlt   DISABLE
  laCtrStat[8] = 'DISABLE'
ENDIF
laCtrStat[1] = 'DISABLE'
laCtrStat[2] = 'DISABLE'
laCtrStat[3] = 'DISABLE'
laCtrStat[4] = 'DISABLE'
laCtrStat[5] = 'DISABLE'
laCtrStat[7] = 'DISABLE'
SELECT (lcBaseFile)
=gfObj_Lock(.F.)
SELECT (lcAlias)

=lfwMainBrw()

*!*************************************************************
*! Name      : lfvTrDate
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Valid function for posting date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvTrDate()
*!*************************************************************
*
FUNCTION lfvTrDate

IF llLinkToGl
  IF ldTrDate <> ldOldDate .AND. LASTKEY() = 13
    IF !CHECKPRD(ldTrDate,'lcGLFYear','lcGLPeriod','PO')
      ldTrDate = ldOldDate
      _CUROBJ = OBJNUM(ldTrDate)
      RETURN
    ENDIF
    SHOW GET ldTrDate  DISABLE
    SHOW GET ldRcvDate ENABLE
    _CUROBJ = OBJNUM(ldRcvDate)
  ENDIF
ELSE
  ldRcvDate = ldTrDate
  =lfRvDate()
ENDIF

*!*************************************************************
*! Name      : lfRvDate
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Valid function for issuing date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfRvDate()
*!*************************************************************
*
FUNCTION lfRvDate

IF llLinkToGl AND ldRcvDate > ldTrDate
  *** The xxxx date cannot be after than the posting date. ***
  =gfModalGen('TRM34152B00000','DIALOG','issuing')
  ldRcvDate = ldOldDate
  _CUROBJ   = OBJNUM(ldRcvDate)
  RETURN
ENDIF

*!*************************************************************
*! Name      : lfvCollect
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Valid function for Collect push button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvCollect()
*!*************************************************************
*
FUNCTION lfvCollect

IF EMPTY(lcbatch)
  lcTmpTxt = lcMessage + "|" + IIF(rbMethod = 1 , "collecting items" , "start scanning")
  *** You have to enter Inter Location Batch before collecting items. ***
  =gfModalGen("INM34142B00000" , "DIALOUG" , lcTmpTxt)
  _CUROBJ = OBJNUM(lcbatch)
  RETURN
ENDIF

*-- If import data from text file, & there was no text file selected.
IF rbMethod =1 .AND. EMPTY(lcImpFile)
  *** You have to select the file you will use to import your items. ***
  =gfModalGen("INM34143B00000" , "DIALOUG")
  _CUROBJ = OBJNUM(lcImpFile)
  RETURN
ENDIF

IF rbMethod = 1
  *-- If import items from text file.
  IF !EMPTY(lcImpFile)
    *-- Append the items into cursor.
    SELECT (lcTmpCurs)
    APPEND FROM (lcImpFile) TYPE DELIMITED
    LOCATE
    IF !EOF()
      *-- If scanning styles not UPC # or materials, get the styles masks.
      llDummy = IIF(!llUPC .AND. llStyle .AND. !EMPTY(laSepartor[1,1]) , lfGetMask() , .T.)
      *-- Call function to get styles for the selected UPC#.
      llDummy = IIF(llUPC , lfGetUPC() , .T.)
      
      *-- Flag defines if I'm in add mode or not.
      llCUpDate = .T.
      llCollect = .T.
      SELECT (lcTmpCurs)
      SCAN
        SELECT ISSBATCH
        APPEND BLANK
        REPLACE TMPRCVNUM WITH lcbatch ;
                Style     WITH LEFT(RTRIM(&lcTmpCurs..Item) , LEN(RTRIM(&lcTmpCurs..Item))-1);
                Size      WITH RIGHT(ALLTRIM(&lcTmpCurs..Item),1)
        lnLineCnt = lnLineCnt + 1
      ENDSCAN
    ENDIF
 
    SELECT (lcTmpCurs)
    ZAP
    
    *-- Call Function to validate styles.
    =lfVldSty()
    
    SELECT ISSBATCH
    LOCATE FOR !lAccepted
    IF FOUND()
      SHOW GET pbVerify ENABLE
    ELSE
      SHOW GET pbVerify DISABLE
    ENDIF
    =lfWhenBrow()
  ENDIF
ELSE
  llCollect  = .T.
  *-- Flag defines if I'm in add mode or not.
  llCUpDate = .T.
  SHOW GET pbVerify DISABLE
ENDIF

IF !llCollect
  *-- If not start to collect items, enable all the objects in the 1st window.
  SHOW GET pbNewCt   DISABLE
  SHOW GET rbMethod  ENABLE
  IF rbMethod = 2
    lcImpFile = ""
    SHOW GET pbGetFil  DISABLE
    SHOW GET lcImpFile DISABLE
  ELSE
    SHOW GET pbGetFil  ENABLE
    SHOW GET lcImpFile ENABLE
  ENDIF
  SHOW GET rbScanBy  ENABLE
  _CUROBJ = OBJNUM(lcbatch)
ELSE
  *-- If start to collect items, disable all the objects in the 1st window.
  IF !EOF('ISSBATCH')
    SHOW GET pbSav     ENABLE
    laCtrStat[11] = 'ENABLE'
  ENDIF
  SHOW GET rbMethod  DISABLE
  SHOW GET pbGetFil  DISABLE
  SHOW GET lcImpFile DISABLE
  SHOW GET rbScanBy  DISABLE
  SHOW GET pbCollect DISABLE
  
  *-- Refresh the objects in window POISSBC3.
  SHOW GET pbNewLn   ENABLE
  lcLinStat = IIF(lnLineCnt > 0 , "ENABLE" , "DISABLE")
  SHOW GET lcStyDesc &lcLinStat
  SHOW GET rbByLin   &lcLinStat
  SHOW GET llLoc     &lcLinStat
  SHOW GET lcRefer   &lcLinStat
  SHOW GET pbRemLn   &lcLinStat
  lnQty = 1
  SHOW GET lnQty
  IF rbByLin = 1
    *-- Activate the style object to add new line.
    SHOW GET lcUPC   DISABLE
    SHOW GET lcStyle ENABLE
    _CUROBJ   = OBJNUM(lcStyle)
  ELSE
    *-- Activate the UPC object to add new line.
    SHOW GET lcStyle DISABLE
    SHOW GET lcUPC   ENABLE
    _CUROBJ   = OBJNUM(lcUPC)
  ENDIF
ENDIF

laScrMode = .F.
STORE .T. TO laScrMode[4], laScrMode[1]

SELECT ISSBATCH

*!*************************************************************
*! Name      : lfGetMask
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Function to get masks for selected styles.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfGetMask()
*!*************************************************************
*
FUNCTION lfGetMask
PRIVATE lcCurSty , lcCurSiz

SELECT (lcTmpCurs)
SCAN
  *-- Current style.
  lcCurSty = LEFT(RTRIM(&lcTmpCurs..Item) , LEN(RTRIM(&lcTmpCurs..Item))-1)
  *-- Current Size.
  lcCurSiz = RIGHT(RTRIM(&lcTmpCurs..Item),1)
  *-- Loop in the separator array.
  FOR lnCnt = 1 TO ALEN(laSepartor,1)
    *-- If the current separator does not exist in its position in the style field.
    IF SUBSTR(lcCurSty , laSepartor[lnCnt,2]+laSepartor[lnCnt,3],1) <>  laSepartor[lnCnt,1]
      *-- Insert the separator in its position in the style position.
      lcCurSty = STUFF(lcCurSty , laSepartor[lnCnt,2]+laSepartor[lnCnt,3] , 0 , laSepartor[lnCnt,1])
    ENDIF
  ENDFOR
  *-- Replace the item field with its style with the separators + size.
  REPLACE Item WITH PADR(lcCurSty,19) + lcCurSiz
ENDSCAN

*!*************************************************************
*! Name      : lfGetUPC
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/15/2001
*! Purpose   : Function to get the styles for the current UPC.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfGetUPC()
*!*************************************************************
*
FUNCTION lfGetUPC
PRIVATE lcCurUPC , lcCurSty , llFoundUPC

*-- Validate the items if from the cursor file.
SELECT (lcTmpCurs)
SCAN 
  *-- Get the current item or UPC# from the cursor file.
  lcCurUPC   = &lcTmpCurs..Item
  lcCurSty   = &lcTmpCurs..Item
  llFoundUPC = .F.
  *-- Seek for the current UPC in the style file.
  SELECT STYLEUPC
  IF SEEK(ALLTRIM(&lcTmpCurs..Item) , "STYLEUPC")
    *-- If found the UPC in the file, get its style+size in the cursor.
    lcCurSty   = STYLEUPC.Style+STYLEUPC.Size
    llFoundUPC = .T.
  ENDIF
  *-- If not found the UPC, consider this item as rejected.
  SELECT (lcTmpCurs)
  REPLACE Item      WITH lcCurSty ;
          lFoundUPC WITH llFoundUPC
ENDSCAN

*!*************************************************************
*! Name      : lfVldSty
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/15/2001
*! Purpose   : Function to validate styles from style file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfVldSty()
*!*************************************************************
*
FUNCTION lfVldSty

*-- Validate the items if from the cursor file.
SELECT ISSBATCH
*-- Scan for selected styles and have status rejected.
SEEK lcBatch
SCAN REST WHILE TMPRCVNUM+STYLE+SIZE = lcBatch FOR !lAccepted
  llAccepted = .F.
  lcStyDesc  = ""
  *-- If validate style not material.
  IF llStyle
    *-- Seek the style in the style file.
    SELECT STYLE
    IF SEEK(ISSBATCH.Style , "STYLE")
      lcStyDesc  = STYLE.Desc1

      *-- Validate the size to see if it is a valid size or not.
      lcISize = IIF(!EMPTY(ISSBATCH.SIZE) AND ISSBATCH.SIZE $ '12345678',ISSBATCH.SIZE,'1')
      lnTCnt  = IIF(SEEK('S'+STYLE.Scale,'SCALE'),SCALE.Cnt,1)

      IF lnTCnt < VAL(lcISize)
        *** Invalid size for this style scale. ***
        *** <  OK  > ***
        =gfModalGen('TRM34150B00000','DIALOG','size for scale of style '+ALLTRIM(ISSBATCH.STYLE))
      ENDIF

      SELECT CTKTRCVL
      SEEK lcSeek + lcbatch
      LOCATE REST WHILE cType+TmpRcvNum+cCarton+CutTkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)+TranCd = 'N'+lcbatch;
                  FOR Style = ISSBATCH.STYLE
      IF FOUND()
        llAccepted = .T.
      ELSE
        *** Style: {ALLTRIM(ISSBATCH.Style)} does not exist in the selected {lcMessage}: {lcCuttkt}. ***
        *** <  OK  > ***
        lcTmpStr   = ALLTRIM(ISSBATCH.Style) + "|" + lcMessage + "|" + lcbatch
        =gfModalGen("QRM34144B00000" , "DIALOG" , lcTmpStr)
      ENDIF
    ELSE
      = gfModalGen("TRM00000B00000","DIALOG","",.F.,"Invalid style : "+ALLTRIM(ISSBATCH.STYLE)+".")
    ENDIF
    
    *-- Update the status with accepted.
    IF llAccepted
      SELECT ISSBATCH
      REPLACE lAccepted WITH llAccepted ;
              DESC1     WITH IIF(EMPTY(DESC1) , lcStyDesc , DESC1);
              cSzDesc   WITH EVALUATE('Scale.Sz'+ALLTRIM(SIZE))
    ENDIF
            
  ENDIF
  SELECT ISSBATCH
ENDSCAN

*!*************************************************************
*! Name      : lfvNewLn
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Valid function for <New> line push button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvNewLn()
*!*************************************************************
*
FUNCTION lfvNewLn

lcStyle = ""
lcUPC   = ""
rbByLin = rbScanBy
SHOW GET rbByLin
lnQty = 1
SHOW GET lnQty
=lfRefresh(lcRcAll3)
IF rbByLin = 1
  *-- Activate the style object to add new line.
  SHOW GET lcUPC   DISABLE
  SHOW GET lcStyle ENABLE
  _CUROBJ   = OBJNUM(lcStyle)
ELSE
  *-- Activate the UPC object to add new line.
  SHOW GET lcStyle DISABLE
  SHOW GET lcUPC   ENABLE
  _CUROBJ   = OBJNUM(lcUPC)
ENDIF

*!*************************************************************
*! Name      : lfvRemLn
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/18/2001
*! Purpose   : Valid function for <Remove> line push button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRemLn()
*!*************************************************************
*
FUNCTION lfvRemLn

*** Are you sure you want to "Remove" this record? ***
*** < Yes > - < No > ***
IF gfModalGen("QRM00002B00006","ALERT","remove") = 1
  *-- Delete current record in the browse.
  
  SELECT ISSBATCH
  BLANK
  DELETE
  SEEK lcbatch
  *-- Decrease the number of lines with 1 record.
  lnLineCnt = lnLineCnt - 1
  lnLineCnt = MAX(lnLineCnt,IIF(EOF(),0,1))
  *-- Call the when function to refresh the screen.
  =lfWhenBrow()
  
ENDIF

*!*************************************************************
*! Name      : lfvByLin
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Valid function to validate styles or UPC on line level.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvByLin()
*!*************************************************************
*
FUNCTION lfvByLin

IF lnOldValue <> rbByLin
  llUPCLine = (rbByLin = 2)
  
  *-- Refresh the browse.
  =lfWhenBrow()
  
  IF rbByLin = 1
    SHOW GET lcUPC   DISABLE
    SHOW GET lcStyle ENABLE
    _CUROBJ = OBJNUM(lcStyle)
  ELSE
    SHOW GET lcStyle DISABLE
    SHOW GET lcUPC   ENABLE
    _CUROBJ = OBJNUM(lcUPC)
  ENDIF
  =lfRefresh(lcRcAll3)
ENDIF
*RAM

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Valid function to validate style or UPC #.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: "S" -> For style.
*!           : "U" -> For UPC.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvStyle("S") OR =lfvStyle("U")
*!*************************************************************
*
FUNCTION lfvStyle
PARAMETERS lcValTyp
PRIVATE lcOldAlias

*-- Return if there is no entry.
IF (lcValTyp = "S" .AND. EMPTY(lcStyle)) .OR. (lcValTyp = "U" .AND. EMPTY(lcUPC))
  RETURN
ENDIF

*-- If called from the UPC object.
IF lcValTyp = "U"
  lcOldAlias = ALIAS()
  SELECT STYLEUPC
  SET ORDER TO STYUPCN
  *-- If not valid UPC #.
  IF !SEEK(PADR(lcUPC,12))
    *** Invalid UPC #. ***
    *** < Browse > - < Reenter > ***
    IF ("?" $ lcUPC) .OR. (gfModalGen("QRM34146B34010" , "DIALOG") = 1)
      *-- Call UPC browse.
      =lfUpcBrow()
    ELSE
      lcStyle = ""
      lcUPC   = ""
      _CUROBJ   = OBJNUM(lcUPC)
      RETURN
    ENDIF
  ELSE
    *-- If valid UPC# , Fill with style & size.
    lcStyle = STYLEUPC.Style+STYLEUPC.Size
  ENDIF
  SET ORDER TO STYLEUPC
  SELECT (lcOldAlias)
  *-- Return if there is no entry.
  IF EMPTY(SUBSTR(lcStyle,1,1))
    lcStyle = ""
    lcUPC   = ""
    RETURN
  ENDIF
ENDIF

*-- Trancate the style & the size values.
lcISize  = RIGHT(ALLTRIM(lcStyle),1)
lcIStyle = SUBSTR(lcStyle,1,lnItmLen)

lcStyDesc  = ""

*-- Validate the style object from the style file.
IF !SEEK(lcIStyle,'STYLE')
  *-- If the style from the upc browse.
  IF lcValTyp = "U"
    *** The style for the selected UPC is invalid. ***
    *** < OK >
    =gfModalGen("QRM34156B00000" , "DIALOG")
    lcStyle = ""
    lcUPC   = ""
    _CUROBJ = _CUROBJ
    RETURN
  ELSE
    *** Invalid Style. ***
    *** < Browse > - < Reenter > ***
    IF ("?" $ lcIStyle) .OR. (gfModalGen("QRM34145B34010" , "DIALOG") = 1)
      *-- Call style browse.
      lcIStyle = gfStyBrw('I',lcIStyle,'',.F.)
      *-- Refresh the lines browse.
      =lfwMainBrw()
    ELSE
      lcStyle = ""
      lcUPC   = ""
      _CUROBJ   = OBJNUM(lcStyle)
      RETURN
    ENDIF
    IF EMPTY(SUBSTR(lcIStyle,1,1))
      lcStyle = ""
      lcUPC   = ""
      RETURN
    ELSE
      *-- Get the style description & dyelot flag.
      lcStyDesc  = STYLE.Desc1
    ENDIF
  ENDIF
ELSE
  *-- Get the style description & dyelot flag.
  lcStyDesc  = STYLE.Desc1
ENDIF

IF rbByLin =1 .AND. SEEK('S'+STYLE.Scale,'SCALE') AND SCALE.Cnt <> 1
  SELECT SCALE
  SCATTER FIELDS SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8 TO laStySizes
  DIME laStySizes[SCALE.Cnt]
  lnStySizes = 1
  =lfSelSize()
  lcISize = ALLTRIM(STR(lnStySizes,1))
ENDIF

*-- Validate the size to see if it is a valid size or not.
lcISize = IIF(!EMPTY(lcISize) AND lcISize $ '12345678',lcISize,'1')
lnTCnt  = IIF(SEEK('S'+STYLE.Scale,'SCALE'),SCALE.Cnt,1)

IF lnTCnt < VAL(lcISize)
  *** Invalid size for this style scale. ***
  *** <  OK  > ***
  =gfModalGen('TRM34150B00000','DIALOG','size for this style scale')
  lcStyle = ""
  lcUPC   = ""
  RETURN
ENDIF

lcStyle  = lcIStyle + lcISize

*-- Validate the style in the selected Batch
llFondLin  = .T.
llAdjustPO = .F.
SELECT CTKTRCVL
SEEK 'N'+lcbatch
LOCATE REST WHILE cType+TmpRcvNum+cCarton+CutTkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)+TranCd = 'N'+lcbatch;
            FOR Style = lcIStyle
IF !FOUND()
  *** Style: {ALLTRIM(lcIStyle)} does not exist in the selected {lcMessage}: {lcCuttkt}. ***
  *** <  OK  > ***
  lcTmpStr   = ALLTRIM(lcIStyle) + "|" + lcMessage + "|" + lcbatch
  =gfModalGen("QRM34144B00000" , "DIALOG" , lcTmpStr)
  lcStyle = ""
  lcUPC   = ""
  RETURN
  llFondLin = .F.
ENDIF
    
SELECT ISSBATCH
APPEND BLANK
*-- If Temp. Receiving PO/CT, update carton, PO/CT & get the dyelot, warehouse, 
*-- Ref. & line # from the master line file.
REPLACE TMPRCVNUM WITH lcbatch
    
*-- Update style, size, Qty., style desc. & the status with accepted.
SELECT ISSBATCH
REPLACE Style     WITH lcIStyle ;
        Size      WITH lcISize ;
        lAccepted WITH .T. ;
        DESC1     WITH lcStyDesc
REPLACE cSzDesc   WITH Scale.Sz&lcISize

lnLineCnt = lnLineCnt + 1

lcStyle = ""
lcUPC   = ""

*-- Call the when function to refresh the screen.
=lfWhenBrow()

*!*************************************************************
*! Name      : lfvVerify
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Valid function for <Verify> push button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvVerify()
*!*************************************************************
*
FUNCTION lfvVerify

IF llUPC .OR. rbScanBy = 2
  SELECT ISSBATCH
  SCAN FOR !lAccepted
    *-- Seek for the current UPC in the style file.
    SELECT STYLEUPC
    IF SEEK(ALLTRIM(ISSBATCH.Style) , "STYLEUPC")
      *-- If found the UPC in the file, get its style+size in the cursor.
      lcCurSty   = STYLEUPC.Style+STYLEUPC.Size
      *-- If not found the UPC, consider this item as rejected.
      SELECT ISSBATCH
      REPLACE Style     WITH STYLEUPC.Style ;
              Size      WITH STYLEUPC.Size
    ENDIF
  ENDSCAN
ENDIF

*-- Validate the founded styles.
=lfVldSty()

*!*************************************************************
*! Name      : lfUpcBrow
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Function to browse UPC number.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfUpcBrow()
*!*************************************************************
*
FUNCTION lfUpcBrow

lcPBrowTitl = "Style UPC Number"   && Browse title.
lcTmpFld    = lcBrFields           && Save browse fields.
lcBrFields  = "cTemp = cUPCNUM1+cUPCNUM2+cUPCNUM3 :R:15:H='UPC',"+;
              "STYLE :R:20:H='Style',"+;
              "Size = IIF(SEEK(STYLE,'STYLE'),IIF(SEEK('S'+STYLE.SCALE,'SCALE'),EVAL('Scale.Sz'+LEFT(Size,1)),''),''):R"

*-- Array hold the style & size values.
DIMENSION latemp[2]
latemp   = ''
*-- Call aria browse to display the available UPC
llReturn = ariabrow([FOR !lUpcPack],lcPBrowTitl, gnBrFsRow1, gnBrFsCol1,;
                    gnBrFsRow2, gnBrFsCol2,'','',;
                    'STYLE,SIZE','laTemp')
*-- Fill with style + size.
lcStyle = latemp[1]+latemp[2]

*-- Restore browse fields.
lcBrFields = lcTmpFld

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Save Function.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lpSavScr()
*!*************************************************************
*
FUNCTION lpSavScr

SELECT ISSBATCH
*-- seek for batch
*-- locate rest while batch for laccepted
SEEK lcbatch
LOCATE REST WHILE TmprcvNum+Style+Size = lcbatch FOR lAccepted
IF !FOUND()
  *** There is no lines. Cannot save! ***
  *** <  Ok  > ***
  =gfModalGen("QRM34154B00000" , "DIALOG")
  STORE .F. TO llCSave
  RETURN
ENDIF

lcTmpDiff = gfTempName()
*-- Create cursor to collect styles and sizes.
CREATE CURSOR (lcTmpDiff) (CSTYLE C(19), NQTY1 N(8,0), NQTY2 N(8,0), NQTY3 N(8,0), NQTY4 N(8,0),;
                         NQTY5 N(8,0), NQTY6 N(8,0), NQTY7 N(8,0), NQTY8 N(8,0), CTYPE C(1))
INDEX ON CSTYLE+CTYPE TAG (lcTmpDiff)

*-- Collect needed Qty. from batch.
SELECT CTKTRCVL
SEEK 'N'+lcbatch
SCAN REST WHILE cType+TmpRcvNum+cCarton+CutTkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)+Trancd = 'N'+lcbatch
  SELECT (lcTmpDiff)
  IF !SEEK(CTKTRCVL.STYLE+'1')
    APPEND BLANK
    REPLACE CSTYLE WITH CTKTRCVL.STYLE CTYPE WITH '1'
  ENDIF
  lnI = 0
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    REPLACE NQTY&lcI. WITH EVALUATE('NQTY'+lcI) + EVALUATE('CTKTRCVL.QTY'+lcI)
  ENDFOR
  IF !SEEK(CTKTRCVL.STYLE+'3')
    APPEND BLANK
    REPLACE CSTYLE WITH CTKTRCVL.STYLE CTYPE WITH '3'
  ENDIF
  lnI = 0
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    REPLACE NQTY&lcI. WITH EVALUATE('NQTY'+lcI) + EVALUATE('CTKTRCVL.QTY'+lcI)
  ENDFOR
ENDSCAN

*-- Get the difference between batch and scaned lines.
SELECT ISSBATCH
SEEK lcbatch
SCAN REST WHILE TMPRCVNUM+STYLE+SIZE = lcbatch FOR LACCEPTED
  SELECT (lcTmpDiff)
  lcSize = ALLTRIM(ISSBATCH.SIZE)
  IF !SEEK(ISSBATCH.STYLE+'2')
    APPEND BLANK
    REPLACE CSTYLE WITH ISSBATCH.STYLE CTYPE WITH '2'
  ENDIF
  REPLACE NQTY&lcSize WITH EVALUATE('NQTY'+lcSize) + 1
  IF SEEK(ISSBATCH.STYLE+'3')
    REPLACE NQTY&lcSize WITH EVALUATE('NQTY'+lcSize) - 1
  ENDIF
ENDSCAN

SELECT (lcTmpDiff)
LOCATE
llBal = .T.
SCAN FOR CTYPE = '3'
  lnI = 0
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    IF EVALUATE('NQTY'+lcI) <> 0
      llBal = .F.
      EXIT
    ENDIF
  ENDFOR
  IF !llBal
    lnSel = 0
    DO WHILE lnSel < 3
      *** There is no lines. Cannot save! ***
      *** <  Ok  > ***
      lnSel = gfModalGen("TRM00000B34018","DIALOG","",.F.,;
                         "There is a difference between the scaned lines and the original batch."+;
                         " Do you want to print Log report ?")
      IF lnSel = 3
        USE IN (lcTmpDiff)
      ELSE
        LOCATE
        SET RELATION TO CSTYLE INTO STYLE
        SELECT STYLE
        SET RELATION TO 'S'+SCALE INTO SCALE
        gcDevice = IIF(lnSel = 2,'PRINTER','SCREEN')
        lcOgPlatForm = 'WINDOWS'
        lcTime = TIME()          && Variable to hold the Time
        llOpenComp = .F.
        IF !USED('SYCCOMP')
          =gfOpenFile(gcSysHome+'SYCCOMP','','SH')
          llOpenComp = .T.
        ENDIF
        SELECT SYCCOMP
        SEEK gcAct_Comp
        DECLARE laCompAdd[5,1]
        laCompAdd = ''                     && Array to hold the Company address
        lcCompName = cCom_Name             && Variable to hold the Company Name
        lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
        lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
        lcCompFax = cCom_Fax               && Variable to hold the Company Fax
        laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
        laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
        laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
        laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
        laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
        lcCompFax = TRANSFORM(lcCompFax , lcPhonPict)  && Fax No. Pic
        lcXphone = TRANSFORM(lcCompPhon , lcPhonPict) && variable hold the phone format to use it
                                  && in the forms befor shifting the array.
        
        =lfAdrShift('laCompAdd')
        SELECT (lcTmpDiff)
        DEFINE WINDOW WINPRINT FROM 1,1 TO 2,2
        ACTIVATE WINDOW WINPRINT
        
        *B604651,1 AMH Use poreport.app insted of gfdispre [Start]
        *DO (gcRepHome + 'GFDISPRE') WITH 'POISSBC'
        lcCons = SET('CONS')
        SET CONS OFF
        DO (gcRepHome +  'POREPORT.APP') WITH 'POISSBC', .T.
        SET CONS &lcCons
        *B604651,1 AMH [End]
        
        IF llOpenComp
          USE IN SYCCOMP
        ENDIF
        RELEASE WINDOW WINPRINT
        SELECT STYLE 
        SET RELATION TO
        SELECT (lcTmpDiff)
        SET RELATION TO
      ENDIF
    ENDDO
    STORE .F. TO llCSave
    RETURN
  ENDIF
ENDSCAN

*-- Define the variable needed before calling save procedure of 
*-- issue inter location transfer.
lcPType    = 'H'
lcBrDtTtl  = 'Issuing Lines'
llDyelot   = .F.

STOR .F.  TO llContinue , llMFCall , llByCarTon , llSpecHndl
STORE " " TO lcShpCode , lcGLFYear , lcGLPeriod , lcAirWayB , ;
             lcShpRef , lcGlSession
STORE 0   TO lnTotStk , lnTotDam , lnTotCan , lnCartons
STORE {}  TO ldEnterd , ldEta
    
*=CHECKPRD(ldTrDate,'lcGLFYear','lcGLPeriod','PO')
    
*--Uncomplete session variables.
DECLARE laVariables[14]
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
    
*-- Call local function to create the temp. files needed.
=lfCreatLin()
        
*-- Update the temp file will be send to the save procedure of the receiving program.
SELECT CTKTRCVL
SEEK 'N'+lcbatch
SCAN REST WHILE cType+TmpRcvNum+cCarton+CutTkt+Style+Dyelot+STR(nLineNo,6)+STR(LineNo,6)+Trancd = 'N'+lcbatch
  IF SEEK('N'+CUTTKT+STYLE+STR(NLINENO,6)+'1','POSLN')
    SELECT POSLN
    SCATTER MEMVAR
    SELECT (lcTmpLine)
    APPEND BLANK
    GATHER MEMVAR
    SELECT CTKTRCVL
    SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,TRANCD MEMVAR 
    SELECT (lcTmpLine)
    APPEND BLANK
    GATHER MEMVAR
  ENDIF
ENDSCAN

STORE .T. TO llCSave
*--Call the Issuing inter location transfer Update function.
IF !USED('APVENDOR')
  USE (gcDataDir+'APVENDOR') IN 0 ORDER VENCODE
ENDIF
DO lpSavScr IN (gcapphome+'POSTREC.FXP') WITH .T.
IF USED('APVENDOR')
  USE IN APVENDOR
ENDIF
SELECT (lcBaseFile)

*!*************************************************************
*! Name      : lfCreatLin
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Function to create the temp. lines file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCreatLin()
*!*************************************************************
*
FUNCTION lfCreatLin

*-- In case of create & issue or receive inter location transfer.
SELECT POSLN
=AFIELDS(laFStru)
lnFStru = ALEN(laFStru,1)
DIMENSION laFStru[lnFStru+13,4]
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
DIMENSION laTags[5,2]
laTags[1,1]='TranCd+cCarton+Po+Style+Dyelot+cWareCode+STR(LineNo,6)'
laTags[2,1]='cCarton+PO+Style+Dyelot+cWareCode+STR(LineNo,6)+TranCd'
laTags[3,1]='PO+Style+Dyelot+cWareCode+STR(LineNo,6)+cCarton+TranCd'
laTags[4,1]='TranCd+style+po+STR(LineNo,6)'
laTags[5,1]='cstytype+po+style+STR(lineno,6)+TranCd'
laTags[1,2]='TmpLine1'
laTags[2,2]='TmpLine2'
laTags[3,2]='TmpLine3'
laTags[4,2]='TmpLine4'
laTags[5,2]='POSLN'

=gfCrtTmp(lcTmpLine,@laFStru,@laTags)
SELECT (lcTmpLine)
SET ORDER TO TAG TmpLine4

*:*************************************************************
*! Name      : lpDelScr
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Delete all records.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lpSavScr()
*:*************************************************************
FUNCTION lpDelScr

SELECT ISSBATCH
SEEK lcbatch
DELETE REST WHILE TMPRCVNUM+STYLE+SIZE = lcbatch
laScrMode    = .F.
laScrMode[1] = .T.
=lfwMainBrw()
RETURN

*!*************************************************************
*! Name      : lfSelSize
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : select style size if style was selected from browse.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfSelSize()
*!*************************************************************
*E500331
FUNCTION lfSelSize

DEFINE WINDOW lcstysize AT  0.000, 0.000 SIZE 4.923,41.000 ;
		TITLE "Select Size" FONT "MS Sans Serif", 8 ;
		FLOAT CLOSE NOMINIMIZE SYSTEM COLOR RGB(,,,192,192,192)
MOVE WINDOW lcstysize CENTER

ACTIVATE WINDOW lcstysize
@ 1.615,15.800 GET lnStySizes ;
	PICTURE "@^" ;
	FROM laStySizes ;
	SIZE 1.538,11.833 ;
	DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	 STYLE "B" ;
	VALID lfvSelSize()
@ 1.923,7.400 SAY "Size :"  ;
	FONT "MS Sans Serif", 8 ;
	 STYLE "BT"
@ 0.385,1.200 TO 4.154,38.800 PEN 1, 8
@ 0.462,1.400 TO 4.231,39.000 PEN 1, 8 COLOR RGB(255,255,255,,,,)
READ CYCLE MODAL
RELEASE WINDOW lcstysize

*-- End OF lfSelSize.
*!*************************************************************
*! Name      : lfvSelSize
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/13/2001
*! Purpose   : Vaild style size.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSelSize()
*!*************************************************************
*E500331
FUNCTION lfvSelSize     &&  lnStySizes VALID
CLEAR READ

*- End Of lfvSelSize

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Ahmed Maher (AMH)
*! Date      : 04/17/2001
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

FOR lnCount = 1 TO 5
  
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

FOR lnCount = 1 TO 5
  
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop