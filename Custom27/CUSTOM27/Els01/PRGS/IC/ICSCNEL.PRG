*:************************************************************************
*: Program file  : ICSCNEL.PRG
*: Program desc. : 
*: For screen    : ICSCNEL
*:         System: ARIA BUSINESS SYSTEM
*:         Module: - IC
*:      Developer: (WAB) Walid A. Wahab
*:************************************************************************
*: Documented 	
*:************************************************************************
*: Calls         : Screens : ICSCNEL.Spx
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*: Example            :
*:*************************************************************
*:Modifications :
*:C200169,1 TMI 09/11/2001 When a style is ont in a Location do not ask the user to add the 
*:C200169,1                selected style to the current location.
*:C200169,1                Also add a field to count the total quantity selected.
*:B606201,1 ASH 07/18/2002 Fix the bug of always overwrite the text file.
*:*************************************************************
*lcTranTyp = "I"
*-- Assign the temporary screens names'.
STORE " " TO lcRcAll0 , lcRcAll1 , lcRcAll2 , lcRcAll3

*-- Assign the main temprary files names'.
STORE " " TO lcTmpRecv , lcTmpCurs

*-- Assign the temp. files.
STORE " " TO lcPOHdr , lcPOLine , lcPOTran , lcTemLoc , ;
             lcTmpLine , lcGlDist , lcTmpPOLn

*-- Define variables hold the cost types.
STORE "" TO lcIType1 , lcIType2 , lcIType3 , lcIType4 , lcIType5

*C200169,1 TMI [Start] Define a variable to hold the "update" state button
lcUpdStat = ''
*C200169,1 TMI [End  ]


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
*-- Array hold the available warehoses.
DECLARE laWare[1,1]
laWare = ""

DECLARE laCrSetups[7 , 2] 

*-- Arrays to calculate the open qty. in case of receive inter location transfer.
DECLARE laOpn[9]
STORE 0 TO laOpn

*-- Force to local Save procedure  (lpSavScr)
laDefProc[9]  = .F.

*-- Define variables hold needed parameters from setup file.
llWareHous = .F.             && Single warehouse or multi warehouse.
llWareLoc  = .F.             && Warehouse location.
llStyDye   = .F.             && System use Style dyelot or not.
llLoc      = .F.             && If Multiple location or not.
llConfirm  = .F.             && If set confirmation is on or off.
lcStyUPC   = "S"             && If system works by style or UPC.
lcSysType  = ""              && System type if Point of sale.


DECLARE laAdjCode[1,1]		 && hold the code adjustment codes 
DECLARE laCodInfo[1,10]		 && hold codes infomation nedd it for gfwCodePop()
lnAdjCode = 1 				 &&hold the number of code element select from the popup 
lcAReason = ''
*Flag hold point of sale setup "Accept items with 0 price".
llAcpZroPc = .F.
*Flag hold point of sale setup "Allow zero inventory".
llAcpZrStk = .F.
lcCostMth  = ' '

lcCnfrm    = ""              && Variable hold the confirmation setting.
llOverite = .T.
*-- Define the main variables in all transactions tyes.
llCollect  = .F.             && Flag to define start processing or not.
rbMethod   = 2               && Radio button (Import file , Scan line by line)
puItem     = 1               && Popup (Style , Material)
rbScanBy   = 1               && Radio button Scan by (Style , UPC)
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
lcShip     = ""              && Variable hold the shipment #.

*-- Flag defines if I'm in add mode or not.
llCUpDate = .F.

*-- Define the variables needed for each line.
rbByLin    = 1               && Radio button on the line level to define UPC or item.
lcRefer    = ""              && Variable hold the reference.
lcWareCode = ""              && Variable hold the warehouse code.
lcDyelot   = ""              && Variable hold the dyelot.
lnWare     = 1
lcStyle    = ""              && Variable hold the style.
lcStyDesc  = ""              && Variable hold the style description.
lcUPC      = ""              && Validate UPC #.
llCrStyDye = .F.             && Flag hold if the current style has dyelot or not.
lnQty      = 0               && Variable hold the style qty.
*C200169,1 TMI [Start] Define a variable to hold the total qty
lnTotalQty = 0
*C200169,1 TMI [End  ]

lnLineCnt  = 0               && Liens counter hold the no. of lines in the browse.
lcLinStat  = "DISABLE"       && Variable hold if the lines are enabled or disabled.

*-- Variables related to create & issue or receive inter location transfer.
lcSiteLoc  = ""              && Variable hold source location.
lcTarget   = ""
lcDropLoc  = ""
llGenOrNum = .F.
lcSession  = ""
llMulCurr  = .F.
llGlLink   = .F.
llLinkToGl = .F.
lnPrice    = 0
ldOldDate  = {}
lcPO       = ""
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

*-- Variables hold the black & invisible color.
lcInvColor = "RGB(192,192,192,192,192,192)"   && Invisable color.
lcNorm2C   = "RGB(0,0,,,,,)"                  && Black Color.

*-- Master files to be use to validate the styles entered.

*-- Variable hold the Browse name.
lcTmpBrow  = "Physical Inventory"
*-- Variable hold the main base file.
lcMainFile = "STYLE"
*-- Seek expression to seek in the PosLn file.
lcSeek     = "N"
lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=.F. ,"+;
           "Style:R:28 , SzDesc:R:9:H='Size' , cStyDesc:R:20:H='Description' , cWareCode:R:H='Location',"+;
           "nQty:R:H='Qty.':6"

*-- Call global function in the main program to do the following : _
*-- Intialise all the variables & open all the files needed in
*-- this session and controling disabling and enabling of the
*-- menu bars and writting the screen names in the window bars.
IF !gfSetup()
  RETURN
ENDIF
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')

lcScFields = "cStyType"

llNoShow  = .F.     && Flag to force the execution of the show procedure.

*-- If entering the screen for the first time.
IF !WEXIST(gcBaseWind)

  *-- Style header.
  lcStyHdr = gfItemMask('HI')
  
  *-- Style picture + Size Picture.
  lcStyPic = gfItemMask('PI')+'9'
  
  *-- Style picture.
  lnItmLen = LEN(gfItemMask('PI'))
  llOverite = .T.  
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
  DIMENSION laCrSetups[7 , 2]
  laCrSetups[1,1] = 'M_WareHouse'
  laCrSetups[2,1] = 'M_WareLoc'
  laCrSetups[3,1] = 'M_Dyelot'
  laCrSetups[4,1] = 'M_CONFIRM'
  laCrSetups[5,1] = 'M_STYLEUPC'
  laCrSetups[6,1] = 'M_Link_GL'
  laCrSetups[7,1] = 'M_SYSTYPE'
  =gfGetMemVar(@laCrSetups,gcAct_Comp)
  llWareHous = (laCrSetups[1,2] = 'Y')
  llWareLoc  = (laCrSetups[2,2] = 'Y')
  llStyDye   = (laCrSetups[3,2] = 'Y')
  llConfirm  = (ALLTRIM(laCrSetups[4,2]) = "Y")
  lcStyUPC   = (ALLTRIM(laCrSetups[5,2]))
  rbScanBy   = IIF(OCCURS('PS',gcCmpModules) <> 0 , IIF(lcStyUPC = "U" , 2 , 1) , 2)
  rbByLin    = rbScanBy
  llLinkToGl = (laCrSetups[6,2] = 'Y')
  llGlLink = llLinkToGl
  lcSysType  = laCrSetups[7,2]
  IF llGlLink
    lnAdjCode = 1
    laCodInfo[1,01] = "CADJREASON"
    laCodInfo[1,02] = "laAdjCode"
    laCodInfo[1,03] = "lnAdjCode"
    laCodInfo[1,04] = ""
    laCodInfo[1,05] = .F.
    laCodInfo[1,06] = .F.
    laCodInfo[1,07] = ""
    laCodInfo[1,08] = ""
    laCodInfo[1,09] = ""
    laCodInfo[1,10] = "cAdjReason"
    =gfwCodePop(@laCodInfo, "CADJREASON", "D") 
    lcAReason = laAdjCode[1,2]
  ENDIF 
  *--Fill warehouse array.
  IF llWareHous  
    SELECT cWareCode+'-'+cDesc FROM Warehous INTO ARRAY laWare
  ELSE
    GO TOP IN WAREHOUS
    laWare[1]  = WAREHOUS.cWareCode+'-'+WAREHOUS.cDesc
    lcWareCode = WAREHOUS.cWareCode
  ENDIF
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
  *-- Get the current site.
  lcSiteLoc = IIF(lcSysType = 'P',WAREHOUS.cWareCode," ")
  
  lcSession  = gfsequence('cSession')
  
  *-- Assign temporary name for the cursor will hold the styles.
  lcTmpCurs = gfTempName()
  CREATE CURSOR (lcTmpCurs) (Item C(20) , lFoundUPC L(1))
  
  *-- Assign the temporary files names.
  lcTmpRecv = gfTempName()
  CREATE DBF (gcWorkDir+lcTmpRecv) (cWareCode C(6) , cStatus C(1), Dyelot C(10) , Style C(19) , Size C(1) , ;
              cStyDesc C(60) , lAccepted L(1) , nQty N(6) , lByUpc L(1) , ;
              lFoundUPC L(1) , Reference C(30) , nLineNo N(6) , ;
              lCrStyDye L(1) , lAdjustPO L(1) , nPrice N(8,2) , SzDesc C(5))
  INDEX ON Style + STR(nLineNo,6) TAG (lcTmpRecv)
  
  *-- If company was setup to keep track of bins.
  IF llWareLoc
    *-- Create temp file for bins.
    DIMENSION laSource[1],laTarget[1]
    =gfOpenFile(gcDataDir+'WhsLoc','WhsLocSt','SH')
    =AFIELDS(laStrc)
    lcTemLoc  = gfTempName()
    CREATE DBF (gcWorkDir+lcTemLoc) FROM ARRAY laStrc
    INDEX ON STYLE+CWARECODE+CLOCATION TAG (lcTemLoc)
  ENDIF
  
  lcTmpLine = gfTempName()
  
  *-- If issue or receive inter location transfer.
  IF llLinkToGl
    lcGlDist = gfTempName()
  ENDIF
  *-- Call local function to create the temp. files needed for each program.
  =lfTempFile()
  
  *-- Assign the temporary window names.
  lcRcAll0  = gfTempName()
  lcRcAll1  = gfTempName()
  lcRcAll2  = gfTempName()
  lcRcAll3  = gfTempName()

ENDIF

*-- Set filter to the suitable styles.
SELECT STYLE
*-- Save the confirmation setting.
lcSetConf  = SET('CONFIRM')

*-- Restore if the lines are enabled or disabled.
lcLinStat = IIF(lnLineCnt > 0 .AND. &lcTmpRecv..lAccepted , "ENABLE" , "DISABLE")
lcItem     =  gfItemMask('HM')
*lcItem = 'SS'
SELECT (lcMainFile)

PUSH KEY
PUSH MENU _MSYSMENU

*-- Define bar to activate the browse.
DEFINE BAR 100 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 101 OF P01PU01 PROMPT lcTmpBrow KEY ALT+B
*-- Activate the browse when selecting its bar.
ON SELECTION BAR 101 OF P01PU01 ACTIVATE WINDOW (lcTmpBrow)

*-- Call the screen.
DO (gcScrDir+gcWinAppl+"\ICSCNEL.SPX")

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
  IF USED(lcTmpLine)
    USE IN (lcTmpLine)
  ENDIF
  ERASE (gcWorkDir+lcTmpLine+'.DBF')
  ERASE (gcWorkDir+lcTmpLine+'.CDX')
  IF llWareLoc
    IF USED(lcTemLoc)
      USE IN (lcTemLoc)
    ENDIF
    ERASE (gcWorkDir+lcTemLoc+'.DBF')
    ERASE (gcWorkDir+lcTemLoc+'.CDX')
  ENDIF
  
  IF USED(lcTmpRecv)
    USE IN (lcTmpRecv)
  ENDIF
  ERASE &gcWorkDir.&lcTmpRecv..DBF
  ERASE &gcWorkDir.&lcTmpRecv..CDX
  
  *-- Close the cursor.
  USE IN (lcTmpCurs)
ENDIF


*!*************************************************************
*! Name      : lpShow
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/13/1999
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

*-- Disable the browse button on the control pannel all the time.
SHOW GET pbBrws DISABLE

*-- Enable the browse invisable button.
SHOW GET ibTab ENABLE
*C200169,1 TMI [Start] Prevent any one to update unless he is an Adminstrator
SHOW GET pbVerify DISABLE
lcUpdStat  = IIF(SYUUSER.CUSR_LEVL='A','ENABLE','DISABLE')
*C200169,1 TMI [End  ]            

DO CASE
  CASE laScrMode[1]
    *-- Define variable to know if saved inter location transfer or not.
    llSav_ILT  = .F.
    
    *-- Blank the temp. files.
    SELECT (lcTmpCurs)
    ZAP
    SELECT (lcTmpRecv)
    ZAP
    
    IF llWareLoc 
      SELECT (lcTemLoc)
      ZAP
    ENDIF  
    
    *-- Call local function to create the temp. files related to each program.
    =lfTempFile()
    
    *-- Blank the needed variables.
    lcItem     =  gfItemMask('HM')
    STORE 2   TO rbMethod 
    lcCollect  = IIF(rbMethod = 1 , "\<Collect" , "S\<tart    ")   && Prompt of the collect & Start button.
    llUPC      = (rbScanBy = 2)  && Flag to know if validate item or UPC.
    rbByLin    = rbScanBy
    ldDate     = gdSysDate       && Variable hold the receiving date.
    lcLinStat  = "DISABLE"       && Variable hold if the lines are enabled or disabled.
    llStyle    = .T.
    STORE .F. TO llCollect , llBrowse , llCrStyDye , cbByCrtn
    STORE 1   TO puItem , lnWare
    STORE 0   TO lnMarker , lnOldValue , lnQty , lnLineCnt , lnPrice
    STORE ""  TO lcImpFile , lcOldValue , lcDesc , lcShip , lcWareCode , ;
                 lcDyelot , lcStyle , lcStyDesc , lcUPC , lcRefer , ;
                 lcCarton , lcCuttkt , lcPO
    *-- Flag defines if I'm in add mode or not.
    llCUpDate = .F.
    *-- Blank the needed variables for Create & issue inter location transfer.
    lcTarget = ""
    STORE gdSysdate TO ldTrDate , ldRcvDate
    SHOW GET ldTrDate ENABLE
    SHOW GET ldRcvDate  ENABLE
    SHOW GET pbSiteLoc ENABLE
    SHOW GET lcSiteLoc ENABLE
    
    SHOW GET pbTarget ENABLE
    SHOW GET lcTarget ENABLE
    SHOW GET ibPO     ENABLE
    SHOW GET lcPO     ENABLE
    SHOW GET rbScanBy ENABLE
    
    =lfwMainBrw()
    *-- Refresh the objects in the first window "lcRcAll0"
    SHOW GET cbByCrtn  ENABLE
    SHOW GETS WINDOW (lcRcAll3) DISABLE ONLY
    SHOW GET ibTab    ENABLE
    SHOW GET ldDate   DISABLE
    SHOW GET lcDesc   DISABLE
    SHOW GET pbVerify DISABLE
    IF EMPTY(lcSiteLoc)
      SHOW GET pbCollect DISABLE
    ELSE
      SHOW GET pbCollect ENABLED
    ENDIF

    ACTIVATE WINDOW (lcRcAll0)
    ACTIVATE WINDOW (lcRcAll1)
  
  CASE laScrMode[4]
    SHOW GET ldTrDate  DISABLE
    SHOW GET ldRcvDate DISABLE
    SHOW GET pbSiteLoc DISABLE
    SHOW GET lcSiteLoc DISABLE
    SHOW GET pbTarget  DISABLE
    SHOW GET lcTarget  DISABLE
    SHOW GET ibPO      DISABLE
    SHOW GET lcPO      DISABLE
    SHOW GET rbScanBy  DISABLE  
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
    
    *C200169,1 TMI [Start] Update the lnTotalQty variable
    =lfUpdTotQt()
    *C200169,1 TMI [End  ]
    
ENDCASE

*-- Refresh the objects in the first window "lcRcAll0"
*SHOW GET rbMethod  ENABLE
SHOW GET puItem    DISABLE
*SHOW GET rbScanBy  ENABLE
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
  *-- Refresh window "ICSCNEL0" objects.
  SHOW GET rbMethod  ENABLE
  IF rbMethod = 2
    lcImpFile = ""
    SHOW GET pbGetFil  DISABLE
    SHOW GET lcImpFile DISABLE
  ELSE
    SHOW GET pbGetFil  ENABLE
    SHOW GET lcImpFile ENABLE
  ENDIF
  *SHOW GET puItem    ENABLE
  SHOW GET puItem    DISABLE
  SHOW GET rbScanBy  ENABLE
  
  *-- Refresh window "ICSCNEL1" objects.
  SHOW GET pbNewCt   DISABLE
  SHOW GET pbCollect ENABLE
  
  *-- Refresh window "ICSCNEL2" objects.
  SHOW GET rbByLin   DISABLE
  *-- Point to the right object.
ELSE
  *-- If start to collect items, disable all the objects in the 1st window.
  *-- Refresh window "ICSCNEL0" objects.
  SHOW GET rbMethod  DISABLE
  SHOW GET pbGetFil  DISABLE
  SHOW GET lcImpFile DISABLE
  SHOW GET puItem    DISABLE
  SHOW GET rbScanBy  DISABLE
  
  *-- Refresh window "ICSCNEL1" objects.
  SHOW GET pbCollect DISABLE
  
  *-- Refresh window "ICSCNEL2" objects.
  SHOW GET lcShip    DISABLE
  SHOW GET ibShip    DISABLE
  
  *-- Point to the push button new object.
ENDIF
IF EMPTY(lcSiteLoc)
  SHOW GET pbCollect DISABLE
ELSE
  SHOW GET pbCollect ENABLE
ENDIF
SELECT (lcMainFile)

*!*************************************************************
*! Name      : lfwMainBrw
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/13/1999
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
SELECT (lcTmpRecv)
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
       WHEN lfWhenBrow()  ;
       VALID :F lfVldBrow() ;
       TITLE lcTmpBrow

*!*************************************************************
*! Name      : lfVldBrow
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/13/1999
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
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/13/1999
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
SELECT (lcTmpRecv)
*-- Change the record pointer.
lnMarker   = RECNO()

glFromBrow = .T.

IF BOF() .OR. EOF() .OR. lnLineCnt = 0
*IF EOF() .OR. lnLineCnt = 0
  lcLinStat = "DISABLE"
  SHOW GET pbRemLn   DISABLE
ELSE
  lcWareCode = &lcTmpRecv..cWareCode
  lcDyelot   = &lcTmpRecv..Dyelot
  lnQty      = &lcTmpRecv..nQty
  lnPrice    = &lcTmpRecv..nPrice
  lcStyDesc  = &lcTmpRecv..cStyDesc
  lcRefer    = &lcTmpRecv..Reference
*  lnWare     = IIF(llWareHous , IIF(ASCAN(laWare,lcWareCode,1)>0 , ASCAN(laWare,lcWareCode,1) , 1) , 1)
  llCrStyDye = &lcTmpRecv..lCrStyDye
  lcLinStat  = IIF(&lcTmpRecv..lAccepted , "ENABLE" , "DISABLE")
  SHOW GET pbRemLn   ENABLE
  SHOW GET pbNewLn   ENABLE
ENDIF

*-- Refresh the objects in window ICSCNEL3.

*IF llCollect
  SHOW GET pbNewLn   ENABLE
  SHOW GET rbByLin   ENABLE
*ELSE
*  SHOW GET pbNewLn   DISABLE
*  SHOW GET rbByLin   DISABLE
*ENDIF

IF rbByLin = 1
  *-- Activate the style object to add new line.
  lcStyle = &lcTmpRecv..Style
  SHOW GET lcUPC   DISABLE
  SHOW GET lcStyle DISABLE
  _CUROBJ   = OBJNUM(lcStyle)
ELSE
  *-- Activate the UPC object to add new line.
  lcUPC = &lcTmpRecv..Style
  SHOW GET lcStyle DISABLE
  SHOW GET lcUPC   DISABLE
  _CUROBJ   = OBJNUM(lcUPC)
ENDIF

SHOW GET lcStyDesc &lcLinStat
*ash1
*SHOW GET lnQty     &lcLinStat
SHOW GET lnQty     DISABLE
SHOW GET lnPrice   &lcLinStat
SHOW GET lnWare    &lcLinStat
SHOW GET llLoc     &lcLinStat
*SHOW GET lcRefer   &lcLinStat

*-- Call function in the screen spr to refresh the say fields 
*-- in the main screen
=lfRefresh()

SHOW WINDOW (lcTmpBrow) REFRESH SAME

*!*************************************************************
*! Name      : lfActMain
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/13/1999
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
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/13/1999
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
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/13/1999
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
  CASE laScrMode[1]
    ACTIVATE WINDOW gwcContrl1
    _CUROBJ = OBJNUM(pbcptask)
  CASE laScrMode[4]
    *C200169,1 TMI [Start] Activate the browse window so that the totala qty is updated
    ACTIVATE WINDOW (lcRcAll2)    
    *C200169,1 TMI [End  ]
    
    ACTIVATE WINDOW (lcRcAll3)
    IF rbByLin = 1
      _CUROBJ = OBJNUM(lcStyle)
    ELSE
      _CUROBJ = OBJNUM(lcUPC)
    ENDIF
ENDCASE

*-- Trap the TAB key.
ON KEY LABEL TAB DO lpTab

*!*************************************************************
*! Name      : lpShTab
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/13/1999
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
  CASE laScrMode[1]
    ACTIVATE WINDOW (lcRcAll1)
    _CUROBJ = OBJNUM(pbCollect)
  CASE laScrMode[4]
    ACTIVATE WINDOW (lcRcAll1)
*    _CUROBJ = OBJNUM(pbNewCt)
    _CUROBJ = OBJNUM(lcTarget)

ENDCASE

*-- Trap the BACKTAB key.
ON KEY LABEL BACKTAB DO lpShTab

*!*************************************************************
*! Name      : lfvMethod
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/13/1999
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
IF EMPTY(lcSiteLoc) AND rbMethod =  2
  SHOW GET pbCollect DISABLE
ELSE
  SHOW GET pbCollect ENABLED
ENDIF

*!*************************************************************
*! Name      : lfvGetFil
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/13/1999
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
lcImpFile = GETFILE('TXT', 'Select File', 'Select',1)
*-- Refresh the imported file variable.
SHOW GET lcImpFile

*!*************************************************************
*! Name      : lfvScanBy
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/13/1999
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
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/18/1999
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
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/18/1999
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
*! Name      : lfvSiteLoc
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/20/1999
*! Purpose   : Valid function for the Source location if current
*!           : program is create & issue inter location transfer.
*!           : & system type is not point of sale.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSiteLoc()
*!*************************************************************
*
FUNCTION lfvSiteLoc

IF !llBrowse .AND. !EMPTY(lcOldValue) .AND. lcSiteLoc = lcOldValue
  RETURN
ENDIF

*Give the posibility to be out from this object.
*-- Seek for the typed target location in the waehouse file.
IF llBrowse OR (!EMPTY(lcSiteLoc) .AND. !SEEK(PADR(lcSiteLoc,6) , 'WareHous'))
  *-- Call global browse to validate the target.
  lcSiteLoc = gfBrowWare(.T.)
ENDIF

llBrowse   = .F.
IF EMPTY(lcSiteLoc) AND rbMethod =  2
  SHOW GET pbCollect DISABLE
ELSE
  SHOW GET pbCollect ENABLED
ENDIF

SHOW GET lcSiteLoc

*!*************************************************************
*! Name      : lfvTrDate
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/21/1999
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
*! Name      : lfvCollect
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/13/1999
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

*-- If import data from text file, & there was no text file selected.
IF rbMethod =1 .AND. EMPTY(lcImpFile)
  *** You have to select the file you will use to import your items. ***
  =gfModalGen("INM34143B00000" , "DIALOUG")
  _CUROBJ = OBJNUM(lcImpFile)
  RETURN
ENDIF


IF rbMethod = 1
  =lfGetData()
ELSE
*wab
  llOverite = .T.
  IF FILE(gcDataDir+ALLTRIM(lcSiteLoc)+'.TXT')
    *ash1
    *lnAns =gfModalGen('QRM00000B42019','DIALOG',.F.,.F.,"The text file already exist.")
    lnAns = 2
    DO CASE 
      CASE lnAns = 1
        llOverite = .T.
      CASE lnAns = 2
        llOverite = .F.
        lcImpFile = gcDataDir+ALLTRIM(lcSiteLoc)+'.TXT'
        =lfGetData()
      CASE lnAns = 3 
       laScrMode    = .F.
       laScrMode[1] = .T.
       SHOW GETS
       RETURN
    ENDCASE
  ENDIF  
 
  lnLineCnt  = 0
  *-- Flag defines if I'm in add mode or not.
  llCUpDate = .T.
*  SHOW GET pbVerify DISABLE
  SHOW GET pbNewLn  DISABLE
ENDIF

*IF !llCollect
*  *-- If not start to collect items, enable all the objects in the 1st window.
*  SHOW GET pbNewCt   DISABLE
*  SHOW GET rbMethod  ENABLE
*  IF rbMethod = 2
*    lcImpFile = ""
*    SHOW GET pbGetFil  DISABLE
*    SHOW GET lcImpFile DISABLE
*  ELSE
*    SHOW GET pbGetFil  ENABLE
*    SHOW GET lcImpFile ENABLE
*  ENDIF
*  *SHOW GET puItem    ENABLE
*  SHOW GET puItem    DISABLE
*  SHOW GET rbScanBy  ENABLE
*  _CUROBJ = OBJNUM(lcTarget)
*ELSE
IF laScrMode[1]
  laScrMode    = .F.
  laScrMode[4] = .T.
ENDIF
lcStyle = '' 
lcUpc=  ''
lcStyDesc = ''
lnqty = 0
SHOW GETS
SHOW GET pbCollect DISABLE
SHOW GET lnAdjCode DISABLE 
SHOW GET pbNewLn   DISABLE
*C200169,1 TMI [Start] Disable Update button for operators, endble for admn
*SHOW GET pbVerify  ENABLE
SHOW GET pbVerify &lcUpdStat
*C200169,1 TMI [End  ]
SHOW GET rbScanBy  DISABLE  
SHOW GET rbMethod  DISABLE  
SHOW GET pbGetFil  DISABLE  

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
*ENDIF

SELECT (lcTmpRecv)
*C200169,1 TMI [Start] Update the lnTotalQty field value
=lfUpdTotQt()
*C200169,1 TMI [End  ]

*!*************************************************************
*! Name      : lfGetMask
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/13/1999
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
    IF SUBSTR(lcCurSty , laSepartor[lnCnt,2] , laSepartor[lnCnt,3]) <>  laSepartor[lnCnt,1]
      *-- Insert the separator in its position in the style position.
      lcCurSty = STUFF(lcCurSty , laSepartor[lnCnt,2]+laSepartor[lnCnt,3] , 0 , laSepartor[lnCnt,1])
    ENDIF
  ENDFOR
  *-- Replace the item field with its style with the separators + size.
  REPLACE Item WITH PADR(lcCurSty,19) + lcCurSiz
ENDSCAN

*!*************************************************************
*! Name      : lfGetUPC
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/15/1999
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
*! Name      : lfvNewLn
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/18/1999
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
lcStyDesc = ''
lnQty = 0
rbByLin = rbScanBy
SHOW GET rbByLin

=lfRefresh(lcRcAll3)
SHOW GET pbNewLn   DISABLE
SHOW GET lnQty     ENABLE
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
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/18/1999
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
*B603595,1 ABD Fix Message paremeters [Begin]
*IF gfModalGen("QRM00002B00006","ALERT") = 1
IF gfModalGen("QRM00002B00006","ALERT","remove") = 1
  *B603595,1 ABD [End]
  *-- Delete current record in the browse.
  SELECT (lcTmpRecv)  
  DELETE
  GO TOP
  *-- Decrease the number of lines with 1 record.
  lnLineCnt = lnLineCnt - 1
  *C200169,1 TMI [Start] Update the lnTotalQty variable
  =lfUpdTotQt(.T.)
  *C200169,1 TMI [End  ]

  *-- Call the when function to refresh the screen.
  =lfWhenBrow()
ENDIF

*!*************************************************************
*! Name      : lfvByLin
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/15/1999
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
  
  *-- Update the current line in the browse.
*  SELECT (lcTmpRecv)
*  REPLACE lByUpc WITH llUPCLine
  
  *-- Refresh the browse.
  =lfWhenBrow()
  lcStyDesc = ''
  lnqty = 0
  IF rbByLin = 1
    lcStyle = '' 
    SHOW GET lcUPC   DISABLE
    SHOW GET lcStyle ENABLE
    _CUROBJ = OBJNUM(lcStyle)
  ELSE
    lcUpc  = ''
    SHOW GET lcStyle DISABLE
    SHOW GET lcUPC   ENABLE
    _CUROBJ = OBJNUM(lcUPC)
  ENDIF
  *wab
  =lfRefresh(lcRcAll3)
  SHOW GET lnQty     ENABLE
  SHOW GET pbNewLn   DISABLE
  *wab
ENDIF
*RAM

*!*************************************************************
*! Name      : lfvRefer
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/18/1999
*! Purpose   : Valid function for Reference object.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRefer()
*!*************************************************************
*
FUNCTION lfvRefer

IF lcRefer <> lcOldValue
  *-- Update the reference object.
  SELECT (lcTmpRecv)
  REPLACE Reference WITH lcRefer
ENDIF

*!*************************************************************
*! Name      : lfvQty
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/18/1999
*! Purpose   : Valid function for Qty. object.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvQty()
*!*************************************************************
*
FUNCTION lfvQty

IF lnQty <> lnOldValue
  SELECT (lcTmpRecv)
  =SEEK(PADR(Style,19) + PADR(lcSiteLoc,6) + SPACE(10) , 'STYDYE')   
  lcChkSty   = Style
  lcChkSiz   = Size
  lnRecvNo   = RECNO()
  lnRecvdQty = 0
  *c#200169 WAB (Start) no need for this lines
  *SUM ALL nQty FOR Style = PADR(lcChkSty,19) .AND. Size = lcChkSiz .AND. lAccepted TO lnRecvdQty
  *IF lnRecvNo > 0 .AND. lnRecvNo <= RECCOUNT()
  *      GOTO lnRecvNo
  *ENDIF
  *lnRecvdQty = lnRecvdQty + lnQty - lnOldValue
  *   IF StyDye.Stk&lcChkSiz - lnRecvdQty < 0
  *      *** Style: ALLTRIM(lcChkSty) has negative stock. Cannot accept. ***
  *      *** <  Ok  > ***
  *      =gfModalGen("INM34165B00000" , "DIALOG" , ALLTRIM(lcChkSty))
  *      lnQty = lnOldValue
  *      RETURN  
  *   ENDIF
  *ENDIF   
  *c#200169 - WAB (End)
  *-- Update the Qty. object.
  SELECT (lcTmpRecv)
  REPLACE nQty WITH lnQty
  SHOW WINDOW (lcTmpBrow) REFRESH
  
ENDIF

*!*************************************************************
*! Name      : lfvPrice
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/20/1999
*! Purpose   : Valid function for price object.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvPrice()
*!*************************************************************
*
FUNCTION lfvPrice

IF lnPrice <> lnOldValue
  *-- Update the Qty. object.
  SELECT (lcTmpRecv)
  REPLACE nPrice WITH lnPrice
  SHOW WINDOW (lcTmpBrow) REFRESH
ENDIF

*!*************************************************************
*! Name      : lfvDyelot
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/18/1999
*! Purpose   : Valid function for Dyelot object.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDyelot()
*!*************************************************************
*
FUNCTION lfvDyelot

IF lcDyelot <> lcOldValue
  *-- Update the dyelot object.
  SELECT (lcTmpRecv)
  REPLACE Dyelot WITH lcDyelot
  SHOW WINDOW (lcTmpBrow) REFRESH
ENDIF

*!*************************************************************
*! Name      : lfvWare
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/18/1999
*! Purpose   : Valid function for warehouse popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvWare()
*!*************************************************************
*
FUNCTION lfvWare

*-- If system is setup as single warehouse & keep track of bins.
IF !llWareHous AND llWareLoc
  STORE 1 TO lnWare , lnOldValue
ENDIF

lnWare     = IIF(lnWare=0,1,lnWare)
lcWareCode = SUBSTR(laWare[lnWare],1,6)

*-- If select new warehouse.
IF lnWare <> lnOldValue
  *-- Check if the style was assigned to this warehouse befor or not.
  IF !SEEK(&lcTmpRecv..Style+lcWareCode+SPACE(10),'STYDYE')
    *** Style: xxx is not assigned to warehouse: xxx. ***
    *** <  Add  > - < Reenter > ***
    IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(&lcTmpRecv..Style)+'|'+lcWareCode) = 1
      DO gpAdStyWar WITH &lcTmpRecv..Style,SPACE(10),lcWareCode
    ELSE
      *-- Restore old value.
      lnWare  = lnOldValue
      SHOW GET lnWare
      RETURN
    ENDIF
  ENDIF
  *-- Update with the new warehouse.
  SELECT (lcTmpRecv)
  REPLACE cWareCode WITH lcWareCode
ENDIF

*-- If company was setup to keep track of bins.
IF llWareLoc
  SELECT (lcTemLoc)
  DELETE ALL FOR cWareCode <> lcWareCode AND Style = &lcTmpRecv..Style
  SELECT WHSLOC
  SET ORDER TO TAG WhsLoc
  llFound = SEEK(lcWareCode)
  SET ORDER TO TAG WhsLocSt
  IF llFound
    =lfvLocat()
  ELSE
    *** No locations have been assigned to warehouse XXXXXX . ***
    *** <  OK  > ***
    = gfModalGen('TRM34147B00000','DIALOG',ALLTRIM(lcWareCode))
  ENDIF  
  *IF !llWareHous
    llLoc = .F.
    SHOW GET llLoc
  *ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvLocat
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/18/1999
*! Purpose   : Function called from warehouse valid function 
*!           : to validate bins.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvLocat()
*!*************************************************************
*
FUNCTION lfvLocat

*-- Get the available locations.
SELECT WHsLoc
IF SEEK(PADR(&lcTmpRecv..Style,19)+SPACE(6)+lcWareCode)
  SCAN WHILE STYLE+COLOR+CWARECODE+CLOCATION=PADR(&lcTmpRecv..Style,19)+SPACE(6)+lcWareCode
    SCATTER MEMVAR
    SELECT (lcTemLoc)
    IF !SEEK(PADR(&lcTmpRecv..Style,19)+lcWareCode)
      APPEND BLANK
      GATHER MEMVAR 
    ENDIF
  ENDSCAN
ENDIF

*-- Define the source array that hold all bins and the target that hold the selected bins.
DIMENSION laSource[1],laTarget[1]
STORE ' ' TO laSource,laTarget
lsSource = 1
*-- Select all bins in the source array.
SELECT cLocation ;
  FROM WHSLOC ;
 WHERE Style+Color+cWareCode == ;
       SPACE(19)+SPACE(6)+lcWareCode ;
  INTO ARRAY laSource

*-- Select selected bins in the target array.
lcCurStyle = PADR(&lcTmpRecv..Style,19)
SELECT cLocation ;
  FROM (lcTemLoc) ;
 WHERE Style+cWareCode+cLocation = ;
       lcCurStyle+lcWareCode;
  INTO ARRAY laTarget

*-- Call global mover to select bins.
=gfMover(@laSource,@laTarget,"Assign Locations",.T.,'lfvLoc')

*-- Delete all the bins.
SELECT (lcTemLoc)
DELETE ALL FOR Style+cWareCode+cLocation = PADR(&lcTmpRecv..Style,19)+lcWareCode

*-- Add the selected bins in the bins file.
FOR lnCount = 1 TO ALEN(laTarget)
   APPEND BLANK
   REPLACE STYLE     WITH &lcTmpRecv..Style ;
           CWARECODE WITH lcWareCode ;
           CLOCATION WITH laTarget[lnCount]
ENDFOR

*!*************************************************************
*! Name      : lfvLoc
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/18/1999
*! Purpose   : Function called from warehouse valid function 
*!           : to validate bins.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvLoc()
*!*************************************************************
*
FUNCTION lfvLoc
PARAMETERS lnOption

DO CASE 
  CASE lnOption = 1
    lcLocatin = laSource[lsSource]
    IF !SEEK(PADR(&lcTmpRecv..Style,19)+SPACE(6)+lcWareCode+lcLocatin,"WhsLoc")
      *** Location XXX is not assigned to '+'style XXX in warehouse XXX. ***
      *** <  Assign  > - <  Cancel  > ***
      IF gfModalGen('TRM34148B34011','DIALOG',ALLTRIM(lcLocatin)+'|'+ALLTRIM(&lcTmpRecv..Style)+'|'+ALLTRIM(lcWareCode)) = 1
        RETURN .T.
      ELSE
        RETURN .F.
      ENDIF
    ELSE
      RETURN .T.
    ENDIF
  CASE lnOption = 2
    FOR lnCount = 1 TO ALEN(laSource,1)
      lcLocatin=laSource[lnCount]
      IF !SEEK(PADR(&lcTmpRecv..Style,19)+SPACE(6)+lcWareCode+lcLocatin,"WhsLoc")
        *** One or more Location(s) are not assigned to '+'style XXX in warehouse XXX. ***
        *** <  Assign  > - <  Cancel  > ***
        IF gfModalGen('TRM34149B34011','DIALOG',ALLTRIM(&lcTmpRecv..Style)+'|'+ALLTRIM(lcWareCode)) = 1
          RETURN .T.
        ELSE
          RETURN .F. 
        ENDIF
      ENDIF
    ENDFOR
  CASE (lnOption=3 OR lnOption=4)
ENDCASE

*!*************************************************************
*! Name      : lfvStyle
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/18/1999
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
  IF !SEEK(ALLTRIM(lcUPC))
    *** Invalid UPC #. ***
    *** < Browse > - < Reenter > ***
    IF ("?" $ lcUPC) .OR. (gfModalGen("QRM34146B34010" , "DIALOG") = 1)
      *-- Call UPC browse.
      =lfUpcBrow()
    ELSE
      lcStyle = ""
      lcUPC   = ""
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
llCrStyDye = .F.
lnPrice    = 0

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
      RETURN
    ENDIF
    IF EMPTY(SUBSTR(lcIStyle,1,1))
      lcStyle = ""
      lcUPC   = ""
      RETURN
    ELSE
      *-- Get the style description & dyelot flag.
      lnPrice    = Style.nICost1
      lcStyDesc  = STYLE.Desc1
      llCrStyDye = (Style.cDye_Flg = "Y")
    ENDIF
  ENDIF
ELSE
  *-- Get the style description & dyelot flag.
  lnPrice    = Style.nICost1
  lcStyDesc  = STYLE.Desc1
  llCrStyDye = (Style.cDye_Flg = "Y")
ENDIF

*Add Screen Sizes If Browse style Only. [Begin]
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

*-- Validate the style in the selected CT/PO
llFondLin  = .T.
llAdjustPO = .F.
*-- Check if the style is assigned to the source warehouse.
IF !SEEK(PADR(lcIStyle,19) + PADR(lcSiteLoc,6) + SPACE(10) , 'STYDYE')
  *** Style: xxx is not assigned to location: xxx. ***
  *** <  Assign  > - < Cancel > ***
*  =gfModalGen('QRM34048B00000','DIALOG',ALLTRIM(lcIStyle)+'|'+lcSiteLoc)
  *C200169,1 TMI [Start] Show a message with the "Reenter" button only
  *IF gfModalGen('TRM42025B42006','DIALOG',ALLTRIM(lcIStyle)+'|'+lcSiteLoc) = 1
  *   DO gpAdStyWar WITH PADR(lcIStyle,19) , SPACE(10) , lcSiteLoc
  *ELSE
  =gfModalGen('INM42025B42005','DIALOG',ALLTRIM(lcIStyle)+'|'+lcSiteLoc)  
  *C200169,1 TMI [End  ]  
  lcStyle = ""
  lcUPC   = ""
  *C200169,1 TMI [Start] Set focus to the style field
  _CUROBJ = OBJNUM(lcStyle)
  *C200169,1 TMI [End  ]  
  RETURN
  *C200169,1 TMI [Start] Comment the ENDIF statement
  *ENDIF
  *C200169,1 TMI [End  ]  
ENDIF
*-- Update the status with accepted.
SELECT (lcTmpRecv)
APPEND BLANK
REPLACE cWareCode WITH lcTarget

*-- Update style, size, Qty., style desc. & the status with accepted.
SELECT (lcTmpRecv)
REPLACE cStatus   WITH "O" ;
        lFoundUPC WITH  IIF(rbByLin=1,.T.,.F.) ;
        Style     WITH lcIStyle ;
        Size      WITH lcISize ;
        cWareCode WITH lcSiteLoc;
        lAccepted WITH .T. ;
        nQty      WITH 1 ;
        cStyDesc  WITH lcStyDesc ;
        lCrStyDye WITH llCrStyDye 
*Replace the size description in the file to be displayed in the browse.
REPLACE SzDesc WITH Scale.Sz&lcISize

lnLineCnt = lnLineCnt + 1

lcStyle = ""
lcUPC   = ""

*C200169,1 TMI [Start] Update the lnTotalQty variable
=lfUpdTotQt()
*C200169,1 TMI [End  ]

*-- Call the when function to refresh the screen.
=lfWhenBrow()
lcStyle = '' 
lcUpc = ''
lcStyDesc = ''
lnqty = 0
=lfRefresh(lcRcAll3)
IF rbByLin = 1
  SHOW GET lcStyle   ENABLE
ELSE
  SHOW GET lcUpc    ENABLE
ENDIF
SHOW GET lnQty     ENABLE
SHOW GET pbNewLn   DISABLE

*!*************************************************************
*! Name      : lfvVerify
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/18/1999
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
  SELECT (lcTmpRecv)
  SCAN FOR !lAccepted .AND. !lFoundUPC 
    *-- Seek for the current UPC in the style file.
    SELECT STYLEUPC
    IF SEEK(ALLTRIM(&lcTmpRecv..Style) , "STYLEUPC")
      *-- If found the UPC in the file, get its style+size in the cursor.
      lcCurSty   = STYLEUPC.Style+STYLEUPC.Size
      *-- If not found the UPC, consider this item as rejected.
      SELECT (lcTmpRecv)
      REPLACE Style     WITH STYLEUPC.Style ;
              Size      WITH STYLEUPC.Size ;
              lFoundUPC WITH .T.
    ENDIF
  ENDSCAN
ENDIF

*-- Validate the founded styles.
=lfVldSty()

*!*************************************************************
*! Name      : lfUpcBrow
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/18/99
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
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/19/99
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
SELECT (lcTmpRecv)
IF FILE(gcDataDir+ALLTRIM(lcSiteLoc)+'.TXT')  
  ERASE gcDataDir+ALLTRIM(lcSiteLoc)+'.TXT'
ENDIF
*IF !llOverite AND FILE(gcDataDir+ALLTRIM(lcSiteLoc)+'.TXT') 
*  SELECT (lcTmpRecv)
*  APPEND FROM gcDataDir+ALLTRIM(lcSiteLoc)+'.TXT' TYPE DELIMITED
*  ERASE gcDataDir+ALLTRIM(lcSiteLoc)+'.TXT'
*ENDIF
*GO BOTTOM
*APPEND BLANK
*REPLACE STYLE WITH "AriaPhysclInvByBrcd"
*B606201,1 ASH 07/18/2002 (Begin) Fix the bug of always overwrite the text file.
*COPY TO gcDataDir+ALLTRIM(lcSiteLoc)+'.TXT' TYPE DELIMITED 
COPY TO gcDataDir+SUBSTR(ALLTRIM(lcSiteLoc),4,3)+SUBSTR(TIME(),1,2)+'-'+SUBSTR(TIME(),4,2)+'.TXT' TYPE DELIMITED 
*B606201,1 ASH 07/18/2002 (End)
*C200169,1 TMI [Start] Reset lnTotalQty to 0 when saveing the file
lnTotalQty = 0
*C200169,1 TMI [End  ]
  
 

*!*************************************************************
*! Name      : lfTempFile
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/20/99
*! Purpose   : Function to create the needed temp. files.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfTempFile()
*!*************************************************************
*
FUNCTION lfTempFile
return



*!**************************************************************************
*! Name      : lfPicture  
*! Developer : Sameh Aldesouki 
*! Date      : 01/17/2000
*! Purpose   : Change Picture of PO.
*!**************************************************************************
FUNCTION lfPicture

IF lcTranTyp <> 'M' 
  RETURN "@! XXXXXX" 
ELSE
  RETURN "@! X99999"
ENDIF  
*C101704,1 [End]

*!*************************************************************
*! Name      : lfSelSize
*! Developer : Abdou Elgendi [ABD]
*! Date      : 06/28/2000
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
*! Developer : Abdou Elgendi [ABD]
*! Date      : 06/28/2000
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
*! Name      : lfRvDate
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/21/1999
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
  =gfModalGen('TRM34152B00000','DIALOG',IIF(lcTranTyp = "C" , 'issuing' , 'receiving'))
  ldRcvDate = ldOldDate
  _CUROBJ   = OBJNUM(ldRcvDate)
  RETURN
ENDIF

**********WAB***************
FUNCTION lfUpdate
PRIVATE lcRejReas,lcFileName,lcReason,ldPstDate,lcOldValue,lcTmpAdj,llGlLink,;
        llMultiWH,lcCostMth,lnRejCont,lcSysType,lcTmpGlDt          

*--lcSysType  ---> hold the system type ( vack office , point of sale
*--lcTmpGlDt  ---> hold the gldist temp file
*--lcReason   ---> hold the phiscal inventory reason
*--ldPstDate  ---> hold the posted date
*--lcFileName ---> hold the path for the excel file contain the upc no
*--lcOldValue ---> hold the old value before the validation 
*--lcTmpAdj   ---> hold the file name contain the style and qty to create phiscal inventory
*--llGlLink   ---> hold .T. if linked to GL module
*--llMultiWH  ---> hold .T. if multy warehouses
*--lcCostMth  ---> hold cost method type
*--llChkDate  ---> hold true to validate the posted date
*--lnRejCont  ---> hold no of record created in the rejected file
*--llCmInstld ---> hold .T. if Cominication module installed



STORE "" TO lcFileName ,lcRejReas,lcOldValue,lcReason 
ldPstDate = ldTrDate


*--get system type ( back office , point of sale
lcSysType = gfGetMemVar('M_SYSTYPE')
llChkDate = .F.
lnRejCont = 0 
lcRejReas  = gfTempName()
lcTmpAdj   = gfTempName()
lcTmpGlDt  = gfTempName()
lcCostMth  = gfGetMemVar('M_Cost_Meth')
llMultiWH  = gfGetMemVar('M_WareHouse')='Y'
llGlLink   = gfGetMemVar('M_Link_GL')  ='Y'
llCmInstld = (OCCURS('NC',gcCmpModules)<>0)


*-- open needed files
=lfOpenFile()


*-- Creat temp files
=lfCreatFle()

*--call the convert screens
=lfvProceed()
*--call function to erase temp file
=lfCloseTmp()
=gfCPSave()
*!*************************************************************
*! Name      : lfOpenFile
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/09/2000
*! Purpose   : Open files
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfOpenFile()
*!*************************************************************
*
FUNCTION lfOpenFile
*=gfOpenFile(gcDataDir+'UPCSEQ',gcDataDir+'UPCSEQ','SH')
=gfOpenFile(gcDataDir+'CODESET',gcDataDir+'FILDNAME','SH')
=gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
=gfOpenFile(gcDataDir+'STYINVJL',gcDataDir+'STYINVJL','SH')
=gfOpenFile(gcDataDir+'INVTADJ',gcDataDir+'INVTADJ','SH')


*!*************************************************************
*! Name      : lfCreatFle
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/09/2000
*! Purpose   : Creat temp files
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCreatFle()
*!*************************************************************
*
FUNCTION lfCreatFle

*-- create temp file  to convert from excel  sheet
*wab
*CREATE TABLE (gcWorkDir+lcTempUpc) (cWareCode C(6), nQty N(7), cUpcNo C(13))
*INDEX ON UPPER(cWareCode)+cUpcNo TAG (lcTempUpc)

*-- create temp file for holding the style and qty for phisical inventory
SELECT INVTADJ
=AFIELDS(laFStru)
lnNo1=ASCAN(laFStru,'UNT_COST')
lnNo2=ASCAN(laFStru,'OLD_COST')
*--Make the lenth of this two fields as ave_cost field.
STORE 15 TO laFStru(lnNo1+2),laFStru(lnNo2+2)
STORE  7 TO laFStru(lnNo1+3),laFStru(lnNo2+3)
lnFStru = ALEN(laFStru,1)
DIMENSION laFStru[lnFStru+2,4]
laFStru[lnFStru+1,1] = 'cAdjReason'
laFStru[lnFStru+1,2] = 'C'
laFStru[lnFStru+1,3] = 6
laFStru[lnFStru+1,4] = 0
laFStru[lnFStru+2,1] = 'cRefer'
laFStru[lnFStru+2,2] = 'C'
laFStru[lnFStru+2,3] = 6
laFStru[lnFStru+2,4] = 0
CREATE DBF (gcWorkDir+lcTmpAdj) FROM ARRAY laFStru
INDEX ON STYLE+cFromWare TAG (lcTmpAdj)

*--create temp file for reject records
CREATE TABLE (gcDataDir+lcRejReas) (Type C(1),cUpcNo C(13), cWareCode C(6),Reason C(30))
INDEX ON Type+cUpcNo TAG (lcRejReas)

IF llGlLink
  =gfOpenFile(gcDataDir+'GLDist','GLDistAc','SH')
  *-- Open a temp file to be used in calling gl distributer proc.
  COPY STRU TO &gcWorkDir.&lcTmpGlDt
  USE (gcWorkDir+lcTmpGlDt) IN 0 EXCLUSIVE
  SELECT (lcTmpGlDt)
ENDIF
*--End of lfCreatFle



*!*************************************************************
*! Name      : lfOldValue
*! Developer : WAB - Walid A. Wahab 
*! Date      : 01/25/2001
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfoldvalue()
*!*************************************************************
FUNCTION lfoldvalue

lcOldValue = EVALUATE(SYS(18))
RETURN


*!*************************************************************
*! Name      : lfvProceed
*! Developer : WAB - Walid A. Wahab 
*! Date      : 01/25/2001
*! Purpose   : Get the file to be convert
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvProceed()
*!*************************************************************
FUNCTION lfvProceed
PRIVATE llError ,llValid , lcStyle ,lcSize ,lcOnError

llError = .F.
lcOnError = ON('ERROR')
ON ERROR llError = .T.
SELECT (lcTmpRecv)

*--convert from the excel file
*WAIT 'Prepare data. Please wait...' WINDOW NOWAIT
*APPEND FROM &lcFileName FOR !EMPTY(&lcTempUpc..cwarecode)  TYPE XLS
*WAIT CLEAR
*IF llError
*  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,"Invalid Excel file or invalid format. Make sure that you select Excel file format 4.0,3.0, or 2.1")
*  lcFileName = ""
*  =lfRefresh('ICUPCCNV')
*  RETURN
*ENDIF
ON ERROR &lcOnError

*-- scan the temp file after the converting and 
*-- check if we have the warecode  in the warehouse file 
*-- and in case of the system type is point of sale we must check if the the warehouse
*-- have the same siteId 
*-- after that we check if we have the upc no in the StyleUpc file
*-- if all above checking is true so we create on record for the style in the temp file
*-- contain the style and warecode and all sizes and the total qty cause the converison file
*-- have a one record for every style/color/size
*-- if any of all above checking is False we create record in the reject file with the reason
SELECT (lcTmpRecv)

lnRejCont = 0 
SCAN
  IF SEEK(cWareCode,"WAREHOUS") 
    IF lcSysType<>"P" .OR. !llCmInstld .OR. (lcSysType = "P" .AND. WAREHOUS.cSiteId = gcCurSite)
      IF SEEK(&lcTmpRecv..Style,"STYLE")
        lcStyle = &lcTmpRecv..STYLE
        lcSize  = SUBSTR(ALLTRIM(&lcTmpRecv..SIZE),1)
        SELECT (lcTmpAdj)
        IF !SEEK(lcStyle+&lcTmpRecv..cWAreCode)
          APPEND BLANK
          REPLACE STYLE      WITH lcStyle					                       ,;
                  cFromWare  WITH UPPER(&lcTmpRecv..cWAreCode)                     ,;
                  cReason    WITH lcReason 					                       ,;
                  DATE 		 WITH ldPstDate                                        ,;
                  DPOSTDATE  WITH ldPstDate                                        ,;
                  TYPE       WITH 'P' 						                       ,;
                  cAdjReason WITH lcAReason								    	   ,;                            
                  UNT_COST   WITH IIF(lcCostMth<>'S',STYLE.Ave_Cost,STYLE.TotCost) ,;
                  OLD_COST   WITH UNT_COST
        ENDIF
        REPLACE ADJ&lcSize WITH ADJ&lcSize + &lcTmpRecv..nQty
        REPLACE TOTADJ     WITH TOTADJ + &lcTmpRecv..nQty
      ENDIF
    ENDIF
  ENDIF
ENDSCAN  

*-- call function to create  phisical transaction 
SELECT (lcTmpAdj)
LOCATE
IF !EOF()
  =lfCrtPhscal()
ENDIF
=gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,'Physical Inventory has been created successfully.')
WAIT CLEAR
RETURN
  




*!*************************************************************
*! Name      : lfCrtPhscal
*! Developer : WAB - Walid A. Wahab 
*! Date      : 01/25/2001
*! Purpose   : create phisical transaction
*!*************************************************************
*! Calls     : lfInvUpdt()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCrtPhscal()
*!*************************************************************
FUNCTION lfCrtPhscal
PRIVATE laToSave,laTemp,lnOldStk,lnOldCost,lcLinkCode,lnTotStk,lcGlSess

*--laToSave   ---> hold the current stock qty per size
*--lnOldStk   ---> hold current total stock before create the phisical transaction
*--lnOldCost  ---> hold current cost before create the phisical transaction
*--lcLinkCode ---> hold Gl link code 
*--lnTotStk   ---> hold the current total stock qty
*--lcGlSess   ---> hold gl session 

*--Save to master files.
DIMENSION laToSave[8],laTemp[8]

SELECT (lcTmpAdj)
SCAN
  = SEEK(Style,'STYLE')
  IF !SEEK( Style + cFromWare + SPACE(10),'STYDYE')
    DO gpAdStyWar WITH &lcTmpAdj..Style,SPACE(10),&lcTmpAdj..cFromWare
  ENDIF
  SELECT (lcTmpAdj)
  *-- Store Style old cost, Old stock, And link code.
  SELECT Style
  lnOldStk   = TotStk
  lnOldCost  = IIF(lcCostMth<>'S',Ave_Cost,TotCost)  
  lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(Link_Code),Link_Code,'DEFDEF'),"")
  SELECT (lcTmpAdj)

  *--Saving the old quantity in the master file
  *--and the checking of the FROM stock.
   
  SELECT IIF(!llMultiWH , 'Style', 'StyDye')
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
  lnTotStk = TotStk
  SELECT (lcTmpAdj)
  GATHER FROM laToSave FIELDS OldQty1,OldQty2,OldQty3,OldQty4 ,;
                              OldQty5,OldQty6,OldQty7,OldQty8
  REPLACE TotOld    WITH lnTotStk
  REPLACE dPostDate WITH ldPstDate
  
  WAIT WINDOW 'Start updating => '+ALLTRIM(Style)  NOWAIT

  *-- Store WAREHOUSE link code. If warehouse link code is empty default
  *-- warehouse link code to style link code. If the last one is empty
  *-- default to 'DEF' link code.
  lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,lcLinkCode), "")
  *--Start Updating.
  IF ! lfInvUpdt()
    SELECT (lcTmpAdj)
    LOOP
  ENDIF
  SELECT (lcTmpAdj)
  REPLACE cSession WITH STYINVJL.cSession
  SELECT (lcTmpAdj)
ENDSCAN

*--Update master adjustment file.
WAIT WINDOW ' Updating the master adjustment file.' NOWAIT
USE IN (lcTmpAdj)
SELECT InvtAdj
APPEND FROM &gcWorkDir.&lcTmpAdj FOR !DELETED()

*-- Update distripution master file
IF llGlLink 
  WAIT WINDOW 'Updating the general ledger distribution file.' NOWAIT
  SELECT (lcTmpGlDt)
  *-- Generate a unique session number.
  lcGlSess = gfsequence('GLSESSION')
  REPLACE ALL GLSESSION WITH lcGlSess
  SELECT GLDIST  
  APPEND FROM &gcWorkDir.&lcTmpGlDt     
ENDIF  
WAIT CLEAR


*!*************************************************************
*! Name      : lfInvUpdt
*! Developer : WAB - Walid A. Wahab 
*! Date      : 01/25/2001
*! Purpose   : update the inventory
*!*************************************************************
*! Calls     : gfStyCrl()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfInvUpdt()
*!*************************************************************
FUNCTION lfInvUpdt
PRIVATE lcAdjAcct,lcAdjReason,laGLDistAr,lcAdjWareH,laAdjust,lnACost,lcAdjTyp,lcRefer,lnRet

*--lcAdjAcct   ---> hold the adjusment account
*--lcAdjReason ---> hold the adjustment reason
*--laGLDistAr  ---> hold the G/L Array difinition
*--lcAdjWareH  ---> hold the adjusment warecode
*--laAdjust    ---> hold the phisical qty 
*--lnACost     ---> hold the cost for the phiscal qty
*--lcAdjTyp    ---> hold type '2' phicical type ( in global function style control)
*--lcRefer     ---> hold the reference 
*--lnRet       ---> the return value from the gfStyctr function

SELECT (lcTmpAdj)
*--Gl adjustment account.
lcAdjAcct = ' '

IF llGlLink AND !EMPTY(cAdjReason)
  lcAdjReason = cAdjReason
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  = gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
ELSE
  lcAdjReason = ' '
ENDIF


*--G/L Array difinition and initialization.
IF llGlLink
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  laGLDistAr[1,2] = '006'
  laGLDistAr[2,2] = '007'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE 'IP'      TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE ldPstDate TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE GLFYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE GLPeriod  TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcTmpGlDt TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF


SELECT (lcTmpAdj)
*--Adjustment quantity array TRANSFER Case.

*--Warehouse Code.
lcAdjWareH = &lcTmpAdj..cFromWare

*--Adjustment quantity array.
DECLARE laAdjust[9]
SCATTER FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8,TotAdj TO laAdjust
*--Adjustment cost.
lnACost = IIF(TotAdj>0,Unt_Cost,Old_Cost)
*--Type of the adjustment.
lcAdjTyp = '2'

*--Call the global function for update style inventory control.
*-- Fixing the bug of wrong updating of ctrcode field in styinvjl (Start)
PRIVATE lcRefer 
lcRefer = IIF(EMPTY(cReason),gfCodDes(cAdjReason,'CADJREASON'),cReason)


lnRet=gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                 lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"","")

*--Return with no save if function return fulse.
IF lnRet = 0
  RETURN .F.

*E301558,1 AHM Send product Activity (Start)
ELSE
  = lfAddPodAct()
*E301558,1 (End)  

ENDIF  
RETURN

*!*************************************************************
*! Name      : lfAddPodAct
*! Developer : AHM - Ahmed Amer
*! Date      : 02/12/2001
*! Purpose   : Add record in EDITrans table for Product Activity
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAddPodAct()
*!*************************************************************

FUNCTION lfAddPodAct

*E301558,1 AHM Send product Activity (Start)

IF 'NC' $ gcCmpModules
  lcFromWare = &lcTmpAdj..cFromWare
  =gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
  SELECT CODES
  SET ORDER TO TAG Idrltfname
  =SEEK('NYCSITEID')
  LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'NYCSITEID' ;
              FOR   cRltd_Nam = 'CCMSITETYP' AND cRltd_Vlu= 'B'
  lcBackSite = ALLTRIM(Codes.cCode_No)
  =SEEK(lcFromWare,'WareHous')
  lcFromSite = ALLTRIM(WareHous.cSiteId)

  STORE '' TO lcSendTo1
  IF gcCurSite = lcBackSite
    IF lcFromSite <> lcBackSite
      lcSendTo1 = lcFromSite
    ENDIF
  ELSE
    lcSendTo1 = lcBackSite
  ENDIF
  IF !EMPTY(lcSendTo1)
    =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
    =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
    =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')

    SELECT Date FROM (lcTmpAdj) GROUP BY Date INTO CURSOR Dates
    SELECT Dates
    SCAN
      lcDateKey = DTOS(Date)+'-'+DTOS(Date)
      IF !EMPTY(lcSendTo1)
        SELECT EdiAcPrt
        LOCATE FOR cSiteId = lcSendTo1
        IF FOUND() AND SEEK(cpartcode+'852','EDIPD')
          SELECT EdiTrans
          IF !SEEK('852'+PADR(lcDateKey,20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
            INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,LINTERCOMP) VALUES ;
                                     ('852',lcDateKey,EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
          ENDIF
          REPLACE cStatus WITH 'N'
          =gfAdd_Info('EDITRANS')
        ENDIF
      ENDIF
    ENDSCAN
    USE IN Dates
  ENDIF
ENDIF
*E301558,1 AHM (End)

*!*************************************************************
*! Name      : lfCloseTmp
*! Developer : WAB - Walid A. Wahab 
*! Date      : 01/25/2001
*! Purpose   : close & erase temp file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCloseTmp()
*!*************************************************************
FUNCTION lfCloseTmp
IF USED(lcTmpGlDt)
  USE IN (lcTmpGlDt)
  IF FILE(gcWorkDir+lcTmpGlDt+'.DBF') 
    ERASE(gcWorkDir+lcTmpGlDt+'.DBF')
  ENDIF
  IF FILE(gcWorkDir+lcTmpGlDt+'.FPT') 
    ERASE(gcWorkDir+lcTmpGlDt+'.FPT')
  ENDIF
  IF FILE(gcWorkDir+lcTmpGlDt+'.CDX') 
    ERASE(gcWorkDir+lcTmpGlDt+'.CDX')
  ENDIF
ENDIF

IF USED(lcTmpAdj)
  USE IN (lcTmpAdj)
  IF FILE(gcWorkDir+lcTmpAdj+'.DBF') 
    ERASE(gcWorkDir+lcTmpAdj+'.DBF')
  ENDIF
  IF FILE(gcWorkDir+lcTmpAdj+'.FPT') 
    ERASE(gcWorkDir+lcTmpAdj+'.FPT')
  ENDIF
  IF FILE(gcWorkDir+lcTmpAdj+'.CDX') 
    ERASE(gcWorkDir+lcTmpAdj+'.CDX')
  ENDIF
ENDIF
RETURN

FUNCTION lfGetData
  *-- If import items from text file.
  IF !EMPTY(lcImpFile)
    *-- Append the items into cursor.
    SELECT (lcTmpRecv)
    APPEND FROM (lcImpFile) TYPE DELIMITED
    IF !SEEK(&lcTmpRecv..Style,'Style')
      =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,"Invalid File Format.")
      DELETE ALL
      _CUROBJ = OBJNUM(lcImpFile)
      RETURN
    ENDIF
    *wab
    lnLineCnt = RECCOUNT()
    *wab
    GO TOP
    lcSiteLoc = cWareCode
    SHOW GET lcSiteLoc DISABLE
    rbByLin  = IIF(lFoundUPC,1,2)
    rbScanBy = rbByLin
    SHOW GET rbScanBy DISABLE
    =lfWhenBrow()
  ENDIF
RETURN

*C200169,1 TMI [Start] Update the variable lnTotalQty
*:**************************************************************************
*:* Name        : lfUpdTotQt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/12/2001
*:* Purpose     : Update the var. lnTotalQty
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUpdTotQt()
*:***************************************************************************
FUNCTION lfUpdTotQt
PARAMETERS llRmv
PRIVATE lnSlct,lnRec
lnSlct = SELECT()
SELECT (lcTmpRecv)
lnRec=RECNO()
LOCATE
COUNT TO lnTotalQty
SHOW GET lnTotalQty
IF BETWEEN(lnRec,1,RECCOUNT())
  GO (lnRec)
  IF llRmv
    SKIP
  ENDIF
ELSE
  GO BOTTOM
ENDIF
SELECT (lnSlct)
*--End of lfUpdTotQt.
*C200169,1 TMI [End  ]
