*:************************************************************************
*: Program file  : SOALOCT.PRG
*: Program desc. : 1- Cutting Ticket Allocation
*:               : 2- Style Purchase Orders Allocation
*: For screen    : SOALOCT.SCX (0,1,2,3)
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Shared between Purchase orders & Manufactering Modules
*:      Developer: Reham Al-Allamy
*:************************************************************************
*: Documented *E300777,1 Rewrite the program to work in the 2.7 version
*:************************************************************************
*: Calls         : 
*:         Programs   : None
*:         Screens    : SO\SOALOCT.SPX
*:                      SO\SOALOMD.SPX
*:         Procedures : lpTab
*:                      lpSHTab
*:         Functions  : gfSetup
*:                      lfActPad
*:                      lpInquire
*:                      lpShow
*:                      lfGetData
*:                      lfvData_1
*:                      lfvData_2
*:                      lfClrTrap
*:                      lfReadAct
*:                      lfDactMain
*:                      lfBrowMain
*:                      lfvBrow
*:                      lfwMain
*:                      lfBrowOrd
*:                      lfwOrder
*:                      lfvRel
*:                      lfvRelAll
*:                      lfRelLin
*:                      lfvLink
*:                      lfwFltrOrd
*:                      lfvFltrOrd
*:                      lpSavScr
*:                      lpClsScr
*:                      lfvOrder
*:                      lfvEdtQty
*:                      lfvModOK
*:*************************************************************
*: Passed Parameters  : lcAllocate : 'C' Cutting Ticket Allocation
*:                                   'P' Style Purchase Orders Allocation
*:*************************************************************
*: Example            :
*:  DO gpDoProg WITH 'AWRSOALOCT',.F.,'MF','C'
*:*************************************************************
*:  Modifications :
*B602179,1 AMM 11/25/98 Erase temporary files when quitting the program
*E301077,17 Reham On 12/30/98
*E301077,17 Reduce the opened file on the program level:
*E301077,17 1- For the main record "SOALOCT", change the files in SydObjct:
*E301077,17    From : CUTTKTH,CUTTKTL,CUTPICK,STYLE,SCALE,ORDHDR,ORDLINE,APVENDOR|CUTTKTH,CUTTKTL,CUTPKORD,STYLE,SCALE,ORDHDR,ORDLINE,VENCODE
*E301077,17    To   : CUTTKTH,CUTTKTL,CUTPICK,STYLE,SCALE,ORDHDR,ORDLINE|CUTTKTH,CUTTKTL,CUTPKORD,STYLE,SCALE,ORDHDR,ORDLINE
*E301077,17 2- For purchase orders allocation program, change the files in SydObjct:
*E301077,17    From : POSHDR,POSLN,CUTPICK,STYLE,SCALE,ORDHDR,ORDLINE,APVENDOR|POSHDR,POSLN,CUTPKORD,STYLE,SCALE,ORDHDR,ORDLINE,VENCODE
*E301077,17    To   : POSHDR,POSLN,CUTPICK,STYLE,SCALE,ORDHDR,ORDLINE|POSHDR,POSLN,CUTPKORD,STYLE,SCALE,ORDHDR,ORDLINE
*E301077,17 3- Open the removed files from SydObjct in the current program.
*B602578,1  Reham on 02/24/99
*B602578,1  Make the global browse in the control panel displays only the purchase order 
*B602578,1  with Poshdr.cstytype= "P" if accessing Style Purchase Order Allocation
*B602578,1  program from the menu..
*B602540,1  Reham on 02/25/99
*B602540,1  Point to the top of the files to void the following errors:
*B602540,1  1- Duplicate records of the order lines.
*B602540,1  2- Not updating the ORD fields in the CutTktL file.
*B602540,1  3- Ability to link the same line twice.
*B802057,1  Reham On 03/22/99
*B802057,1  1- Display dyelots in Pos, Cts & order lines.
*E301182,4  Reham On 03/23/99
*E301182,4  Change the Cutpkord index in the CutPick file to include the cTktLineNo field
*B802147,1  Reham On 05/02/99
*B802147,1  Include the style scale description if CT allocation & not extended size scale.
*B602776,1  Reham On 05/04/99
*B602776,1  Update the order header file with right qty.
*E301289,1 WAB 08/25/1999 Adding selling price & gross margin on PO lines & CT lines
*B101704,1 SAM 01/17/2000 Allow alpha numeric PO# manualy
*C101944,1 HBG 10/29/2000 Modify the C\T allocation screen and PO screen o allow the 
*C101944,1                user to allocate a multi store order to C\T or PO without 
*C101944,1                having to select each store individually
*:************************************************************************************
*
PARAMETERS lcAllocate
EXTERNAL ARRAY laKeyField , laDefProc


IF lcAllocate = "C"
  DECLARE laData[1]
ELSE
  DECLARE laData[2]
ENDIF
laData     = ""

IF lcAllocate = "C"
  lcKeyCode  = '1'
  DECLARE laKeyField [1,4]
  laKeyField[1,1] = 'laData[1]'
  laKeyField[1,2] = .T.
  laKeyField[1,3] = 'CUTTKTH'
  laKeyField[1,4] = 1
ELSE
  lcKeyCode  = '2'
  DECLARE laKeyField [2,4]
  laKeyField[1,1] = 'laData[1]'
  laKeyField[1,2] = .F.
  laKeyField[1,3] = 'POSHDR'
  laKeyField[1,4] = 1
  laKeyField[2,1] = 'laData[2]'
  laKeyField[2,2] = .T.
  laKeyField[2,3] = 'POSHDR'
  laKeyField[2,4] = 2
ENDIF


*E301289,1 WAB - Get Setting Variable ( Display selling Price & gross Margin )
*E301289,1 WAB - setting (M_PoDspPrc) if prog . for allocate PO  
*E301289,1 WAB - setting (M_MfDspPrc) if prog . for allocate Ct
*E301289,1 WAB - initialize variables & calculate total generate qty of order line
*E301289,1 WAB - START
*------- lnTotQty    -> Hold TOTAl generate Qty Of  Order  line
*------- lnSelPrice  -> Hold Average Selling Price
*------- lnOldQty    -> Hold Total order  Qty of Po line 
*------- lnRotSub    -> Hold Cost Price Or Selling price Depend on llStyMark
*------- lnGrosMrgn  -> Hold Gross Margin
*------- llDispPric  -> hold setting variable to display selling price & gross margin
llDispPric  = .F.
llStyMark   = .F.
*B101704,1 [Start]
llGenOrNum = .F.
*B101704,1 [End]
llDispPric = IIF(lcAllocate='P',gfGetMemVar('M_PoDspPrc'),gfGetMemVar('M_MfDspPrc'))
llStyMark  = gfGetMemVar('M_stymark')   ='T'
IF llDispPric
  STORE 0 TO lnTotQty,lnSelPrice,lnOldQty,lnRotSub,lnGrosMrgn
ENDIF
*E301289,1 WAB - END

*E301182,4 Reham On 03/23/99   *** Begin ***
*E301182,4 Variable hold the line no. in CT/PO to seek in the CutPick file.
lcKeyLine = ""
*E301182,4 Reham On 03/23/99   *** End   ***

*-- Define the base file.
lcAlocFile   = IIF(lcAllocate = 'C' , 'CUTTKTH' , 'POSHDR')
*-- Force to local Save procedure  (lpSavScr)
laDefProc[9] = .F.

*-- Force to local Cancel procedure  (lpClsScr)
laDefProc[10] = .F.

*-- Define the needed arrays in this file.
DECLARE laSize[9] , laOrd[9] , laAvl[9]
STORE "" TO laSize , laOrd , laAvl
STORE 0  TO lnMarkA , lnMarkB
lcCurKey   = ""     && Hold the main keys to prevent double collecting the data.
lcKeyFld   = ""     && Hold the P/O # or C/T #.
lcCurOrder = ""     && Hold the current order no.
lcScFields = ""     && Hold the main file fields used by array laData.
lcStyHdr   = ""     && Hold the style header.
lnOldFlt   = 1      && Hold the old value of the show popup,
puShow     = 1      && Hold the current value of the show popup,


*B802147,1 Reham On 05/02/99    *** Begin ***
*B802147,1 If cuttink ticket allocation, include the style scale desc. in the browse header.
*lcBrwQty   = IIF(lcAllocate = "C" , "Cut " , "PO  ")
llExtSizSc = .F.
lcBrwQty   = IIF(lcAllocate = "C" , IIF(llExtSizSc , "Cut " , "Sz ") , "PO  ")
*B802147,1 Define 8 variables to hold the scale desc. in the browse.
FOR lnCount = 1 TO 8
  lcCount = STR(lnCount,1)
  lcBrwQty&lcCount = lcBrwQty + lcCount
ENDFOR
*B802147,1 Reham On 05/02/99    *** End   ***

*-- None of the screen's objects has been updated.
llCUpDate  = .F.

*-- Variables that hold the red and green colors used to display the 
*-- "Remaining" values in the modify screen.
lcRedClr   = "RGB(,,,255,0,0)"
lcGreenClr = "RGB(,,,0,128,0)"

*-- Variable hold the status of the push buttons.
STORE "DISABLE" TO lcAllStat , lcRelStat , lcLnkStat , lcOrdStat

*-- Get the prompt of the available pictures on the screen.
lcABrowBmp = gcBmpHome + "ExtKey.Bmp"
lcAOkBmp   = gcBmpHome + "Ok.Bmp"
lcACanBmp  = gcBmpHome + "Can.Bmp"

*-- Define the temp. names variables.
STORE '' To lcAloct0 , lcAloct1 , lcAloct2 , lcAloct3 , ;
            lcMainLin , lcOrderLin , lcOrdStLin , lcCutPick

STORE .F. TO llBrowse

*B802057,1 Reham On 03/22/99   *** Begin ***
*B802057,1 If system use style Dyelots or not.
llDyelot  = .F.
*B802057,1 Reham On 03/22/99   *** End   ***

lcMainBrwT = IIF(lcAllocate = "C" , "Cutting Ticket Lines" , "Style Purchase Order Lines")
lcOrdBrwT  = "Order Lines"

*-- None of the screen's objects has been updated.
llCUpDate  = .F.

*C101944,1 HBG 10/29/200 Decleare variabels and arrays to collect the data 
*C101944,1               of the multi stor order [Begin] 
llAloMulSt = .F.      && Variabel to get the user choice , if he/She want to allocate all stores for this line or not
DIMENSION laStrQty[9]      && Array to hold the Qty of all lines of the multi stor order
DIMENSION laStrCut[9]      && Array to hold the Cut of all lines of the multi stor order
DIMENSION laLinQty[9]      && Array to hold the Qty for each line of the multi stor order
DIMENSION laStrlnCut[9]    && Array to hold the Cut for each line of the multi stor order which will be store 
STORE 0 TO laStrQty , laStrCut , laLinQty , laStrlnCut , laOrd
lcOrder = ''               
lcStyle = ''
llOrder = .F.        && Variabel to check if we change the allocation Qty from Order button
*C101944,1 [End]

*-- Call global function in the main program to do the following : _
*-- Intialise all the variables & open all the files needed in
*-- this session and controling disabling and enabling of the
*-- menu bars and writting the screen names in the window bars.
IF !gfSetup()
  RETURN
ENDIF

lcScFields = IIF(lcAllocate = "C" , "CutTkt" , "cStyType , PO")

llNoShow   = .F.     && Flag to force the execution of the show procedure.

*E301077,17 Reham On 12/30/98   *** Begin ***
*E301077,17 Open the company file.
=gfOpenFile(gcSysHome+'SycComp',gcSysHome+'cComp_Id','SH')
*E301077,17 Reham On 12/30/98   *** End   ***

*-- If entering the screen for the first time.
IF !WEXIST(gcBaseWind)
  SCATTER FIELDS &lcScFields TO laData BLANK
  lcAloct0   = gfTempName()
  lcAloct1   = gfTempName()
  lcAloct2   = gfTempName()
  lcAloct3   = gfTempName()
  lcCutPick  = gfTempName()
  lcOrderLin = gfTempName()
  lcOrdStLin = gfTempName()
  lcMainLin  = gfTempName()
  
  *B802057,1 Reham On 03/22/99   *** Begin ***
  *B802057,1 If system use style Dyelots or not.
  llDyelot  = ALLTRIM(gfGetMemVar('M_DYELOT',gcAct_Comp))    = 'Y'
  *B802057,1 Reham On 03/22/99   *** End   ***
  *B101704,1 [Start]
  *llGenOrNum = (UPPER(ALLTRIM(gfGetMemVar('M_GenStOrN')='Y')))
  llGenOrNum = ALLTRIM(gfGetMemVar('M_GenStOrN'))='Y'
  *B101704,1 [End]
  *B802147,1  Reham On 05/02/99    *** Begin ***
  llExtSizSc = gfGetMemVar('M_USEEXSSC',gcAct_Comp)
  lcBrwQty   = IIF(lcAllocate = "C" , IIF(llExtSizSc , "Cut " , "Sz ") , "PO  ")
  *B802147,1  Define 8 variables to hold the scale description in the browse header.
  FOR lnCount = 1 TO 8
    lcCount = STR(lnCount,1)
    lcBrwQty&lcCount = lcBrwQty + lcCount
  ENDFOR
  *B802147,1  Reham On 05/02/99    *** End   ***
  
  *-- Get the style header.
  lcStyHdr   = gfItemMask("HI")
  
  *-- Prepare the order lines temp. file.
  SELECT OrdLine
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2 , 4]
  laFileStru[lnFileStru+1,1] = 'nRecno'
  laFileStru[lnFileStru+1,2] = 'N'
  laFileStru[lnFileStru+1,3] = 10
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'cStatus'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 1
  laFileStru[lnFileStru+2,4] = 0
  CREATE DBF (gcWorkDir+lcOrderLin) FROM ARRAY laFileStru
  INDEX ON Style TAG (lcOrdStLin)
  INDEX ON cOrdType+Order+STR(LineNo,6) TAG (lcOrderLin)
  *B602745,1 Reham On 05/02/99   *** Begin ***
  SET ORDER TO TAG (lcOrderLin)
  *B602745,1 Reham On 05/02/99   *** End   ***
  
  *-- Prepare the cutpick temp file.
  SELECT CutPick
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2 , 4]
  laFileStru[lnFileStru+1,1] = 'nRecno'
  laFileStru[lnFileStru+1,2] = 'N'
  laFileStru[lnFileStru+1,3] = 10
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'cStatus'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 1
  laFileStru[lnFileStru+2,4] = 0
  CREATE TABLE (gcWorkDir+lcCutPick) FROM ARRAY laFileStru
  *E301182,4  Reham On 03/23/99   *** Begin ***
  *E301182,4  Change the Cutpkord index in the CutPick file to include the cTktLineNo field
  *INDEX ON TRANCD+CTKTNO+ORDER+STYLE+CORDLINE TAG (lcCutPick)
  INDEX ON TRANCD+CTKTNO+cTktLineNo+ORDER+STYLE+CORDLINE TAG (lcCutPick)
  *E301182,4  Reham On 03/23/99   *** End   ***

  IF lcAllocate = "C"
    *-- Prepare the cutting ticket lines temp. file if C/T allocation.
    SELECT CutTktL
  ELSE
    *-- Prepare the style purchase order lines temp. file if P/O allocation.
    SELECT PosLn
  ENDIF
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'nRecno'
  laFileStru[lnFileStru+1,2] = 'N'
  laFileStru[lnFileStru+1,3] = 10
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'cStatus'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 1
  laFileStru[lnFileStru+2,4] = 0
  IF lcAllocate = "C"
    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru+1,4]
    laFileStru[lnFileStru+1,1] = 'Scale'
    laFileStru[lnFileStru+1,2] = 'C'
    laFileStru[lnFileStru+1,3] = 3
    laFileStru[lnFileStru+1,4] = 0
  ENDIF
  CREATE TABLE (gcWorkDir+lcMainLin) FROM ARRAY laFileStru   && Temp main file.
  INDEX ON Style TAG (lcMainLin)
ENDIF

*-- Set filter to hide the canceled status.
SELECT (lcAlocFile)
SET FILTER TO
*B602578,1  Reham on 02/24/99   *** Begin ***
*B602578,1  Make the global browse in the control panel displays only the purchase order 
*B602578,1  with Poshdr.cstytype= "P" if accessing Style Purchase Order Allocation
*B602578,1  program from the menu..
*SET FILTER TO Status <> "X"
IF lcAllocate = "C"
  SET FILTER TO Status <> "X"
ELSE
  SET FILTER TO cStyType = "P" AND Status <> "X"
ENDIF
*B602578,1  Reham on 02/24/99   *** End   ***

*-- Define the browses bars.
PUSH MENU _MSYSMENU
DEFINE BAR 100 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 101 OF P01PU01 PROMPT lcMainBrwT KEY ALT+B
DEFINE BAR 102 OF P01PU01 PROMPT lcOrdBrwT  KEY ALT+Z

*-- Activate the browses when selecting there bars.
ON SELECTION BAR 101 OF P01PU01 ACTIVATE WINDOW (lcMainBrwT)
ON SELECTION BAR 102 OF P01PU01 ACTIVATE WINDOW (lcOrdBrwT)

*-- Call the screen.
DO (gcScrDir+"SO\SOALOCT.SPX")

*-- Clear the main file filter.
SELECT (lcAlocFile)
SET FILTER TO

*-- Release the browse window.
RELEASE WINDOW (lcMainBrwT)
RELEASE WINDOW (lcOrdBrwT)
POP MENU _MSYSMENU

*-- If quitting the program erase the temp. files.
IF glQuitting
  IF USED(lcCutPick)
    USE IN (lcCutPick)
  ENDIF
  ERASE &gcWorkDir.&lcCutPick..DBF
  ERASE &gcWorkDir.&lcCutPick..CDX
  
  IF USED(lcOrderLin)
    USE IN (lcOrderLin)
  ENDIF
  ERASE &gcWorkDir.&lcOrderLin..DBF
  ERASE &gcWorkDir.&lcOrderLin..CDX
  ERASE &gcWorkDir.&lcOrdStLin..CDX
  *B602179,1 AMM 11/25/98 Erase temporary files when quitting the program
  ERASE &gcWorkDir.&lcOrderLin..FPT
  *B602179,1 AMM end
  IF USED(lcMainLin)
    USE IN (lcMainLin)
  ENDIF
  ERASE &gcWorkDir.&lcMainLin..DBF
  ERASE &gcWorkDir.&lcMainLin..CDX
ENDIF

*-- Release the Inquire C/T or P/O pad from the system menu.
RELEASE PAD _INQUIRE OF _MSYSMENU

*!*************************************************************
*! Name      : lfActPad
*! Developer : Reham Al-Allamy
*! Date      : 10/06/1997
*! Purpose   : Bulid a new menu pad [Option] in the when of the
*!           : main screen
*!*************************************************************
*! Calls     : lpInquire
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfActPad()
*!*************************************************************
*
FUNCTION lfActPad

*-- Define a pad called options have 2 bars :
*-- 1- Inquire the current C/T or P/O.
*-- 2- Inquire the current order no.
DEFINE PAD _INQUIRE OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
SET SKIP OF PAD _INQUIRE OF _MSYSMENU (laScrMode[1])
ON PAD _INQUIRE OF _MSYSMENU ACTIVATE POPUP _INQUIREPOP

DEFINE POPUP    _INQUIREPOP MARGIN SHADOW
DEFINE BAR 1 OF _INQUIREPOP PROMPT IIF(lcAllocate="C" , "\<C/T Inquire" , "\<P/O Inquire") SKIP FOR laScrMode[1]
DEFINE BAR 2 OF _INQUIREPOP PROMPT "Sales \<Orders Inquire" SKIP FOR laScrMode[1] .OR. EMPTY(lcCurOrder)
ON SELECTION POPUP    _INQUIREPOP DO lpInquire

*!*************************************************************
*! Name      : lpInquire
*! Developer : Reham Al-Allamy
*! Date      : 12/29/1997
*! Purpose   : 
*!*************************************************************
*! Calls     : gpDoProg 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpInquire
*!*************************************************************
*
FUNCTION lpInquire

DO CASE
  *-- C/T or P/O screen.
  CASE BAR() = 1
    IF lcAllocate = "C"
      lcCTInq = "'"+laData[1]+"'"
      DO gpDoProg WITH 'AWRMFCUTKT',.F.,'MF',lcCTInq
    ELSE
      lcPOInq = "'P"+"','"+laData[2]+"'"
      DO gpDoProg WITH 'AWRPOSTYLE',.F.,'PO',lcPOInq
    ENDIF
  *-- Sales order screen.
  CASE BAR() = 2
    lcSOInq = "'O','"+lcCurOrder+"'"
    DO gpDoProg WITH 'AWRSOORD',.F.,'SO',lcSOInq
ENDCASE

*!*************************************************************
*! Name      : lpShow
*! Developer : Reham Al-Allamy
*! Date      : 12/23/1997
*! Purpose   : Show function for the whole screen.
*!*************************************************************
*! Calls     : lfBrowMain, lfBrowOrd, lfGetData, lfwMain, lfwOrder
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpShow()
*!*************************************************************
*
FUNCTION lpShow

SELECT (lcAlocFile)

*-- Fill the first row of ladata with type "P" if current program 
*-- is Style purchase order allocation.
IF lcAllocate = "P"
  laData[1] = "P"
ENDIF

*-- Enable the control keys all the time.
SHOW GET ibTab1 ENABLE
SHOW GET ibTab2 ENABLE
SHOW GET ibTab3 ENABLE

*-- Disable the delete button all the time in the control pannel.
SHOW GET pbDlt DISABLE

*-- Disable the delete bar in the edit pad in the system menu all the time.
laCtrStat[8]  = "DISABLE"

DO CASE
  *-- If select mode, zap the temp. files & refresh the browse.
  CASE laScrMode[1]
    lcCurKey   = ""
    *-- Fill the status variables with "Disable".
    STORE "DISABLE" TO lcAllStat , lcRelStat , lcLnkStat , lcOrdStat
    *-- Blank all the arrays.
    STORE     ""    TO laSize , laOrd , laAvl
    lcCurOrder  = ""         && Hold the current order no.
    lcKeyFld    = SPACE(6)   && Hold the C/T# or P/O#.
    
    *E301182,4 Reham On 03/23/99   *** Begin ***
    *E301182,4 Variable hold the line no. in CT/PO to seek in the CutPick file.
    lcKeyLine = ""
    *E301182,4 Reham On 03/23/99   *** End   ***
    
    *-- Zap all the temp. files in the current program.
    SELECT (lcMainLin)
    ZAP
    SELECT (lcOrderLin)
    ZAP
    SELECT (lcCutPick)
    ZAP
    *-- Call the main browse from the temp. file of CUTKTL or POSLN
    =lfBrowMain()
    *-- Call the order browse from the temp. file of ORDLINE.
    =lfBrowOrd()
  CASE laScrMode[2]
    *-- Flag to know if the screen's objects has been updated or not.
    llCUpDate  = .F.
    *-- Fill the status variables with "Disable".
    STORE "DISABLE" TO lcAllStat , lcRelStat , lcLnkStat , lcOrdStat
    
    *-- Call function to get the available data from the following files :
    *-- 1- Cutting Ticket Lines File or Style Purchase Orders Lines.
    *-- 2- Sales Orders Lines File.
    *-- 3- Cutting Picking File.
    =lfGetData()
    
    *-- Call the browses function.
    =lfBrowMain()     && Call the main browse.
    =lfBrowOrd()      && Call the order browse.
    SHOW GET puShow ENABLE
      
  CASE laScrMode[3]
    *-- Blank the variable to force collecting the data if canceled or saved.
    *lcCurKey = " "
    
    *-- Call function to get the available data from the following files :
    *-- 1- Cutting Ticket Lines File or Style Purchase Orders Lines.
    *-- 2- Sales Orders Lines File.
    *-- 3- Cutting Picking File.
    =lfGetData()
    
    *-- Call the When function for the browse.
    =lfwMain()        && Call the order browse when to refresh the status.
    =lfwOrder()       && Call the main browse when to refresh the status.
ENDCASE

*-- Disable the option pad in the select mode.
SET SKIP OF PAD _INQUIRE OF _MSYSMENU (laScrMode[1])
*-- Disable the sales order bar in the option pad if there is no order#.
SET SKIP OF BAR 2 OF PAD _INQUIRE OF _MSYSMENU (EMPTY(lcCurOrder))

*-- Select the main file that the navigation works on.
*-- "CUTTKTH" if C/T allocation & "POSHDR" if P/O allocation.
SELECT (lcAlocFile)

*!*************************************************************
*! Name      : lfGetData
*! Developer : Reham Al-Allamy
*! Date      : 01/28/1998
*! Purpose   : Function to get the available data.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfGetData()
*!*************************************************************
*
FUNCTION lfGetData

*-- If the key change, get the data for the current key.
IF (lcAllocate = "C" .AND. lcCurKey <> laData[1]) .OR. ;
   (lcAllocate = "P" .AND. lcCurKey <> "P" + laData[2])
  IF lcAllocate = "P"
    *-- If Current program is P/O allocate.
    lcKeyFld    = laData[2]   && Fill with the P/O #.
    *-- Get the current PO# lines.
    SELECT POsLn.* , RECNO() AS nRecNo , 'S' AS cStatus ;
      FROM POsLn ;
     WHERE POsLn.cStyType + POsLn.PO = "P" + laData[2] .AND. TranCd = '1' ;
      INTO DBF (gcWorkDir+lcMainLin)
  ELSE
    *-- If Current program is C/T allocate.
    lcKeyFld   = laData[1]   && Fill with the C/T #.
    *-- Get the current C/T lines.
    SELECT CUTTKTL.* , STYLE.SCALE , ;
           RECNO() AS nRecNo , 'S' AS cStatus ;
      FROM CUTTKTL , STYLE ;
     WHERE CutTktL.CUTTKT + CutTktL.STYLE + ;
           CutTktL.DYELOT + CutTktL.TRANCD = ;
           laData[1] .AND. TranCd = '1' .AND. ;
           CutTktL.Style = Style.Style ;
      INTO DBF (gcWorkDir+lcMainLin)
  ENDIF
  *-- Set index of the temp. file on style field.
  INDEX ON Style TAG (lcMainLin)
  
  *-- Select all order lines for the styles in the P/O or C/T
  *-- with order header status not 'C' or 'X'.
  SELECT OrdLine
  SET ORDER TO TAG OrdLineS
  
  *-- Zap the temp. order lines file.
  SELECT (lcOrderLin)
  ZAP
  
  *-- Loop in the main file to get the order lines for each style.
  SELECT (lcMainLin)
  SCAN
    SELECT OrdLine
    *-- Seek for the current style in the order lines file.
    IF SEEK(&lcMainLin..Style)
      *-- Scan all the order lines that contain the current style.
      SCAN REST WHILE STYLE+DTOS(COMPLETE)+ORDER+STORE+STR(LINENO,6) = ;
                      &lcMainLin..Style ;
                  FOR TotQty <> 0
        *-- If the order header status is not "C" or "X" for the current 
        *-- order line, include this order line in the order temp file.
        IF SEEK("O"+OrdLine.Order , 'OrdHdr') .AND. !(OrdHdr.Status $ 'CX')
          *B602745,1 Reham On 05/02/99   *** Begin ***
          IF !SEEK(OrdLine.cOrdType+OrdLine.Order+STR(OrdLine.LineNo,6) , lcOrderLin)
          *B602745,1 Reham On 05/02/99   *** End   ***
            *-- Insert the current order line in the order temp file.
            SCATTER MEMVAR MEMO
            INSERT INTO (lcOrderLin) FROM MEMVAR
            *-- Replace the status with "S".
            REPLACE &lcOrderLin..cStatus WITH 'S' ;
                    &lcOrderLin..nRecNo  WITH RECNO("ORDLINE")
            *-- Call global function to add audit fields info.
            =gfAdd_Info(lcOrderLin)
          *B602745,1 Reham On 05/02/99   *** Begin ***
          ENDIF
          *B602745,1 Reham On 05/02/99   *** End   ***
        ENDIF
      ENDSCAN
    ENDIF
  ENDSCAN
  SELECT OrdLine
  SET ORDER TO TAG OrdLine
  
  *-- Zap the temp. cutting picking file.
  SELECT (lcCutPick)
  ZAP
  
  SELECT CutPick
  *-- Seek if the current c/t or po has records in the cutpick file.
  IF SEEK(lcKeyCode + lcKeyFld)
    *-- Scan to get all the lines related to the current c/t or po.
    SCAN REST WHILE TranCd + cTktNo = lcKeyCode + lcKeyFld
      *-- Insert the current line in the cutpick temp file.
      SCATTER MEMVAR MEMO
      INSERT INTO (lcCutPick) FROM MEMVAR
      *-- Replace the status with "S".
      REPLACE &lcCutPick..cStatus WITH 'S' ;
              &lcCutPick..nRecNo  WITH RECNO("CUTPICK")
      *-- Call global function to add audit fields info.
      =gfAdd_Info(lcCutPick)
    ENDSCAN
  ENDIF
  lcCurKey = IIF(lcAllocate = "C" , laData[1] , "P" + laData[2])
  
  *B802147,1 Reham 05/02/99   *** Begin ***
  *B802147,1 Go top in the main lines file.
  GO TOP IN (lcMainLin)
  IF lcAllocate = "C" .AND. !llExtSizSc
  *B802147,1 If CT allocation & not extended size scale, seek with the current style to get 
  *B802147,1 its scale description to be displayed in the browse header.
    IF SEEK(&lcMainLin..Style , "Style") .AND. SEEK("S" + Style.Scale , "Scale")
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        lcBrwQty&lcCount = IIF(!EMPTY(Scale.Sz&lcCount) , Scale.Sz&lcCount , lcBrwQty + lcCount)
      ENDFOR
    ELSE
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        lcBrwQty&lcCount = lcBrwQty + lcCount
      ENDFOR
    ENDIF
  ENDIF
  *B802147,1 Reham 05/02/99   *** End   ***
ENDIF

*!*************************************************************
*! Name      : lfvData_1
*! Developer : Reham Al-Allamy
*! Date      : 12/24/1997
*! Purpose   : Valid function for the cutting ticket no.
*!*************************************************************
*! Calls     : CUTBROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_1()
*!*************************************************************
*
FUNCTION lfvData_1

*-- Fill the cuttkt# with zeros on the left if not browsing.
llBrowse  = IIF((!llBrowse .AND. "?" $ laData[1]) .OR. llBrowse , .T. , .F.)
laData[1] = IIF(llBrowse .OR. EMPTY(laData[1]) , laData[1] , PADL(ALLTRIM(laData[1]) , 6 , "0"))

IF (llBrowse .OR. !EMPTY(laData[1])) .AND. LASTKEY() = 13
  IF !SEEK(laData[1] , lcAlocFile) .OR. llBrowse
    lcCutTkt  = laData[1]
    =CUTBROW(@lcCutTkt , " ")
    laData[1] = lcCutTkt
  ENDIF
  *-- Refresh the objects.
  SHOW GET laData[1]
  IF !EMPTY(laData[1])
    laScrMode    = .F.
    laScrMode[2] = .T.
    SHOW GETS
  ELSE
    _CUROBJ = OBJNUM(laData[1])
  ENDIF
ENDIF
llBrowse = .F.

SELECT (lcAlocFile)

*!*************************************************************
*! Name      : lfvData_2
*! Developer : Reham Al-Allamy
*! Date      : 12/24/1997
*! Purpose   : Valid function for the PO no.
*!*************************************************************
*! Calls     : POSBROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_2()
*!*************************************************************
*
FUNCTION lfvData_2

*-- Fill the po# with zeros on the left if not browsing.
llBrowse  = IIF((!llBrowse .AND. "?" $ laData[2]) .OR. llBrowse , .T. , .F.)
laData[2] = IIF(llBrowse .OR. EMPTY(laData[2]) , laData[2] , PADL(ALLTRIM(laData[2]) , 6 , "0"))
laData[1] = 'P'

IF (llBrowse .OR. !EMPTY(laData[2])) .AND. LASTKEY() = 13
  IF !SEEK("P" + laData[2] , lcAlocFile) .OR. llBrowse
    lcPO      = laData[2]
    =POSBROW(@lcPO , '' , 'P')
    laData[2] = lcPO
  ENDIF
  *-- Refresh the objects.
  SHOW GET laData[2]
  IF !EMPTY(laData[2])
    laData[1]    = 'P'
    laScrMode    = .F.
    laScrMode[2] = .T.
    SHOW GETS
  ELSE
    _CUROBJ = OBJNUM(laData[1])
  ENDIF
ENDIF

laData[1] = 'P'
llBrowse  = .F.
SELECT (lcAlocFile)

*!*************************************************************
*! Name      : lfClrTrap
*! Developer : Reham Al-Allamy
*! Date      : 12/23/1997
*! Purpose   : Clear the trapped key.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfClrTrap()
*!*************************************************************
*
FUNCTION lfClrTrap

*-- Clear all the trapped keys.
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL Ctrl+ENTER
ON KEY LABEL Ctrl+HOME
ON KEY LABEL Ctrl+END
ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Reham Al-Allamy
*! Date      : 12/23/1997
*! Purpose   : READ Activate function of the main screen.
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfReadAct()
*!*************************************************************
*
FUNCTION lfReadAct

IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If none of the screen's browses is active then clear the 
*-- trapped keys.
IF !INLIST(WONTOP(), lcMainBrwT, lcOrdBrwT) 
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL CTRL+Q
  ON KEY LABEL CTRL+W
  ON KEY LABEL Ctrl+HOME
  ON KEY LABEL Ctrl+END
  ON KEY LABEL Ctrl+ENTER
ENDIF

*!*************************************************************
*! Name      : lfDactMain
*! Developer : Reham Al-allamy
*! Date      : 12/23/1997
*! Purpose   : Deactivate function for the base screen.
*!*************************************************************
*! Calls     : lpTab, lpShTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfDactMain()
*!*************************************************************
*
FUNCTION lfDactMain

*-- Set the global flag "glFromBrow" to true only if one of the
*-- screen's browses is active.
glFromBrow = INLIST(WONTOP(), lcMainBrwT, lcOrdBrwT) 

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
*! Developer : Reham Al-Allamy
*! Date      : 12/23/1997
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

ON KEY LABEL TAB
DO CASE
  CASE WONTOP() = lcMainBrwT 
    ACTIVATE WINDOW (lcAloct2)
    _CUROBJ = OBJNUM(PBREL)
  CASE WONTOP() = lcOrdBrwT
    ACTIVATE WINDOW ('gwcContrl1')
    _CUROBJ = OBJNUM(pbTop)
ENDCASE
ON KEY LABEL TAB DO lpTab

*!*************************************************************
*! Name      : lpShTab
*! Developer : Reham Al-Allamy
*! Date      : 01/08/1998
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

ON KEY LABEL BACKTAB
DO CASE
  CASE WONTOP() = lcMainBrwT 
    ACTIVATE WINDOW (lcAloct0)
    _CUROBJ = OBJNUM(ibTab1)
    
  CASE WONTOP() = lcOrdBrwT
    ACTIVATE WINDOW (lcAloct2)
    _CUROBJ = OBJNUM(ibTab2)
ENDCASE
ON KEY LABEL BACKTAB DO lpShTab

*!*************************************************************
*! Name      : lfBrowMain
*! Developer : Reham Al-Allamy
*! Date      : 12/23/1997
*! Purpose   : Browse c/T or po.
*!*************************************************************
*! Calls     : lfwMain, lfvBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrowMain()
*!*************************************************************
*
FUNCTION lfBrowMain

SELECT (lcMainLin)
lnMarkA = RECNO()

*B802057,1 Reham On 03/22/99   *** Begin ***
*B802057,1 If system use style Dyelots or not.
*-- If browse from cutting tickets header or style PO header file.
IF llDyelot
  *B802147,1 Reham On 05/02/99   *** Begin ***
  *B802147,1 Display the scale description in the browse header.
  *lcMainFld =  "cMarker = IIF(RECNO()=lnMarkA,'>',' '):H=' ':R:1:W=.F., Style,"+;
             "TotQty:H=IIF(lcAllocate = 'C' , 'Tot. Cut','Tot. PO '),Dyelot,"+;
             "Qty1:H=lcBrwQty+'1', Qty2:H=lcBrwQty+'2', Qty3:H=lcBrwQty+'3',"+;
             "Qty4:H=lcBrwQty+'4', Qty5:H=lcBrwQty+'5', Qty6:H=lcBrwQty+'6',"+;
             "Qty7:H=lcBrwQty+'7', Qty8:H=lcBrwQty+'8',TotOrd:H='Tot. Ord',"+;
             "Ord1:H='Ord 1', Ord2:H='Ord 2', Ord3:H='Ord 3',"+;
             "Ord4:H='Ord 4', Ord5:H='Ord 5', Ord6:H='Ord 6',"+;
             "Ord7:H='Ord 7', Ord8:H='Ord 8'"
  lcMainFld =  "cMarker = IIF(RECNO()=lnMarkA,'>',' '):H=' ':R:1:W=.F., Style,"+;
             "TotQty:H=IIF(lcAllocate = 'C' , 'Tot. Cut','Tot. PO '),Dyelot,"+;
             "Qty1:H=(lcBrwQty1), Qty2:H=(lcBrwQty2), Qty3:H=(lcBrwQty3),"+;
             "Qty4:H=(lcBrwQty4), Qty5:H=(lcBrwQty5), Qty6:H=(lcBrwQty6),"+;
             "Qty7:H=(lcBrwQty7), Qty8:H=(lcBrwQty8),TotOrd:H='Tot. Ord',"+;
             "Ord1:H='Ord 1', Ord2:H='Ord 2', Ord3:H='Ord 3',"+;
             "Ord4:H='Ord 4', Ord5:H='Ord 5', Ord6:H='Ord 6',"+;
             "Ord7:H='Ord 7', Ord8:H='Ord 8'"
  *B802147,1 Reham On 05/02/99   *** End   ***
ELSE
*B802057,1 Reham On 03/22/99   *** End   ***
  *B802147,1 Reham On 05/02/99   *** Begin ***
  *B802147,1 Display the scale description in the browse header.
  *lcMainFld =  "cMarker = IIF(RECNO()=lnMarkA,'>',' '):H=' ':R:1:W=.F., Style,"+;
             "TotQty:H=IIF(lcAllocate = 'C' , 'Tot. Cut','Tot. PO '),"+;
             "Qty1:H=lcBrwQty+'1', Qty2:H=lcBrwQty+'2', Qty3:H=lcBrwQty+'3',"+;
             "Qty4:H=lcBrwQty+'4', Qty5:H=lcBrwQty+'5', Qty6:H=lcBrwQty+'6',"+;
             "Qty7:H=lcBrwQty+'7', Qty8:H=lcBrwQty+'8',TotOrd:H='Tot. Ord',"+;
             "Ord1:H='Ord 1', Ord2:H='Ord 2', Ord3:H='Ord 3',"+;
             "Ord4:H='Ord 4', Ord5:H='Ord 5', Ord6:H='Ord 6',"+;
             "Ord7:H='Ord 7', Ord8:H='Ord 8'"
  lcMainFld =  "cMarker = IIF(RECNO()=lnMarkA,'>',' '):H=' ':R:1:W=.F., Style,"+;
             "TotQty:H=IIF(lcAllocate = 'C' , 'Tot. Cut','Tot. PO '),"+;
             "Qty1:H=(lcBrwQty1), Qty2:H=(lcBrwQty2), Qty3:H=(lcBrwQty3),"+;
             "Qty4:H=(lcBrwQty4), Qty5:H=(lcBrwQty5), Qty6:H=(lcBrwQty6),"+;
             "Qty7:H=(lcBrwQty7), Qty8:H=(lcBrwQty8),TotOrd:H='Tot. Ord',"+;
             "Ord1:H='Ord 1', Ord2:H='Ord 2', Ord3:H='Ord 3',"+;
             "Ord4:H='Ord 4', Ord5:H='Ord 5', Ord6:H='Ord 6',"+;
             "Ord7:H='Ord 7', Ord8:H='Ord 8'"
  *B802147,1 Reham On 05/02/99   *** End   ***
*B802057,1 Reham On 03/22/99   *** Begin ***
ENDIF
*B802057,1 Reham On 03/22/99   *** End   ***

*E301289,1 WAB - Add Selling Price & Gross Margin to browse fileds 
*E301289,1 WAB - ( PO / CT ) when the llDispPric  is true 
*E301289,1 WAB - START
IF llDispPric
  lcMainFld =  lcMainFld + [,nSelPrice :H='Selling Price' :R:P='999999999.99',;
                               nGrosMrgn :H='Gross Margin' :R:P='9999.99']
ENDIF
*E301289,1 WAB - END

BROWSE FIELDS &lcMainFld ;
       WHEN lfwMain()    ;
       VALID :F lfvBrow() ;
       NOEDIT           ;
       LOCK 0           ;
       NOMENU           ;
       NOAPPEND         ;
       NODELETE         ;
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       WINDOW (lcAloct1)  ;
       IN WINDOW (gcBaseWind) ;
       TITLE lcMainBrwT

=lfwMain()

SELECT (lcAlocFile)

*!*************************************************************
*! Name      : lfvBrow
*! Developer : Reham Al-Allamy
*! Date      : 12/23/1997
*! Purpose   : Valid function for the main browse.
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvBrow()
*!*************************************************************
*
FUNCTION lfvBrow

*-- If the current window not any of the browses, call global function stop browse.
IF !INLIST(WONTOP(), lcMainBrwT, lcOrdBrwT)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfwMain
*! Developer : Reham Al-Allamy
*! Date      : 12/23/1997
*! Purpose   : When function for the main browse.
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwMain()
*!*************************************************************
*
FUNCTION lfwMain

SELECT (lcMainLin)
lnMarkA    = RECNO(lcMainLin)

*-- Point to the current style in the order temp file.
SELECT (lcOrderLin)

LOCATE FOR Style = &lcMainLin..Style 

lcCurOrder = &lcOrderLin..Order

SELECT (lcMainLin)
*E301182,4 Reham On 03/23/99   *** Begin ***
*E301182,4 Save the current CT/PO line no.
lcKeyLine = STR(&lcMainLin..LineNo,6)
*E301182,4 Reham On 03/23/99   *** End   ***

IF SEEK("S"+&lcMainLin..Scale , "Scale")
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laSize[lnCount] = Scale.Sz&lcCount
  ENDFOR
ELSE
  STORE "" TO laSize
ENDIF

*-- Display the available qty. from the order lines.
IF !EMPTY(&lcOrderLin..Order)
  STORE 0 TO laAvl
  FOR lnCount = 1 TO 8
    lcCount = ALLTRIM(STR(lnCount))
    laAvl[lnCount] = &lcOrderLin..Qty&lcCount - &lcOrderLin..Cut&lcCount
    laAvl[9]       = laAvl[9] + laAvl[lnCount]
  ENDFOR
ELSE
  STORE 0 TO laAvl
ENDIF

*-- Enable the release all button if the total order qty. greater than zero.
IF &lcMainLin..TotOrd <> 0
  lcAllStat = IIF(laScrMode[3] , "ENABLE" , "DISABLE")
ELSE
  lcAllStat = "DISABLE"
ENDIF
SHOW GET pbRelAll &lcAllStat

*-- Enable the release button & the order button to modify the qty.
*-- if the total order qty. greater than zero & record exist in the 
*-- temp. cutpick file.
SELECT (lcMainLin)

*E301182,4 Reham On 03/23/99   *** Begin ***
*E301182,4 Change the Cutpkord index in the CutPick file to include the cTktLineNo field
*IF &lcMainLin..TotOrd <> 0 .AND. ;
   SEEK(lcKeyCode+lcKeyFld+&lcOrderLin..Order+&lcOrderLin..Style+STR(&lcOrderLin..LineNo,6),lcCutPick)
IF &lcMainLin..TotOrd <> 0 .AND. ;
   SEEK(lcKeyCode+lcKeyFld+STR(&lcMainLin..LineNo,6)+&lcOrderLin..Order+&lcOrderLin..Style+STR(&lcOrderLin..LineNo,6),lcCutPick)
*E301182,4  Reham On 03/23/99   *** End   ***
  *C101944,1 HBG 10/29/200 If This line of the order is not allocated Disabel the 
  *C101944,1                Release button [Begin]
  *lcRelStat = IIF(laScrMode[3] , "ENABLE" , "DISABLE")
  *lcOrdStat = IIF(laScrMode[3] , "ENABLE" , "DISABLE")
  *SELECT (lcCutPick)
  *SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOrd
  *SELECT (lcMainLin)  
  IF &lcOrderLin..TotCut = 0 
    lcRelStat = "DISABLE"
    lcOrdStat = "DISABLE"
    laOrd = 0
  ELSE
    lcRelStat = IIF(laScrMode[3] , "ENABLE" , "DISABLE")
    lcOrdStat = IIF(laScrMode[3] , "ENABLE" , "DISABLE")
    SELECT (lcCutPick)
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOrd
    SELECT (lcMainLin)
  ENDIF
  *C101944,1 [End]
ELSE
  lcRelStat = "DISABLE"
  lcOrdStat = "DISABLE"
  laOrd = 0
ENDIF
SHOW GET pbRel   &lcRelStat
SHOW GET pbOrder &lcOrdStat

*-- Check if there is available qty. to be allocated, enable the link buuton.
IF (((&lcMainLin..Qty1-&lcMainLin..Ord1<>0) .AND. (&lcOrderLin..Qty1-&lcOrderLin..Cut1<>0)) OR;
   ((&lcMainLin..Qty2-&lcMainLin..Ord2<>0) .AND.  (&lcOrderLin..Qty2-&lcOrderLin..Cut2<>0)) OR;
   ((&lcMainLin..Qty3-&lcMainLin..Ord3<>0) .AND.  (&lcOrderLin..Qty3-&lcOrderLin..Cut3<>0)) OR;
   ((&lcMainLin..Qty4-&lcMainLin..Ord4<>0) .AND.  (&lcOrderLin..Qty4-&lcOrderLin..Cut4<>0)) OR;
   ((&lcMainLin..Qty5-&lcMainLin..Ord5<>0) .AND.  (&lcOrderLin..Qty5-&lcOrderLin..Cut5<>0)) OR;
   ((&lcMainLin..Qty6-&lcMainLin..Ord6<>0) .AND.  (&lcOrderLin..Qty6-&lcOrderLin..Cut6<>0)) OR;            
   ((&lcMainLin..Qty7-&lcMainLin..Ord7<>0) .AND.  (&lcOrderLin..Qty7-&lcOrderLin..Cut7<>0)) OR;            
   ((&lcMainLin..Qty8-&lcMainLin..Ord8<>0) .AND.  (&lcOrderLin..Qty8-&lcOrderLin..Cut8<>0))) 
  
  lcLnkStat = IIF(laScrMode[3] , "ENABLE" , "DISABLE")
ELSE
  lcLnkStat = "DISABLE"
ENDIF
SHOW GET pbLink &lcLnkStat

*-- Disable the sales order bar if the there is no order.
SET SKIP OF BAR 2 OF PAD _INQUIRE OF _MSYSMENU (EMPTY(lcCurOrder))

*-- Refresh the say objects in the screen.
=lfRefresh(lcAloct2)

IF WEXIST(lcOrdBrwT)
  SHOW WINDOW (lcOrdBrwT) REFRESH
ENDIF

SHOW WINDOW (lcMainBrwT) REFRESH TOP

*!*************************************************************
*! Name      : lfBrowOrd
*! Developer : Reham Al-Allamy
*! Date      : 12/23/1997
*! Purpose   : Browse order lines.
*!*************************************************************
*! Calls     : lfwOrder, lfvBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrowOrd()
*!*************************************************************
*
FUNCTION lfBrowOrd

SELECT (lcOrderLin)
lnMarkB = RECNO()

*E301182,4 Reham On 03/23/99   *** Begin ***
*E301182,4 Change the Cutpkord index in the CutPick file to include the cTktLineNo field
*B802057,1 Reham On 03/22/99   *** Begin ***
*B802057,1 If system use style Dyelots or not.
IF llDyelot
  *-- Browse from the temp order lines file.
  lcOrdFld = "cMarker = IIF(RECNO()=lnMarkB,'>',' '):H=' ':R:1:W=.F.,"+;
           "lcAlo=IIF(TotCut<>0,IIF(SEEK(lcKeyCode+lcKeyFld+STR(&lcMainLin..LineNo,6)+ORDER+STYLE+STR(LINENO,6),lcCutPick) , IIF(TotCut = &lcCutPick..TOTQTY,'*','»*'),'»'),' '):H='Allocated',"+;
           "Order,Complete,Account,Store,Style,Dyelot,"+;
           "TotQty:H='Tot. Ord',Qty1:H='Ord 1', Qty2:H='Ord 2', Qty3:H='Ord 3',"+;
           "Qty4:H='Ord 4', Qty5:H='Ord 5', Qty6:H='Ord 6',"+;
           "Qty7:H='Ord 7', Qty8:H='Ord 8', CustPo:H='Cust Po', Price:H='Price'"
ELSE
*B802057,1 Reham On 03/22/99   *** End   ***
  *-- Browse from the temp order lines file.
  lcOrdFld = "cMarker = IIF(RECNO()=lnMarkB,'>',' '):H=' ':R:1:W=.F.,"+;
           "lcAlo=IIF(TotCut<>0,IIF(SEEK(lcKeyCode+lcKeyFld+STR(&lcMainLin..LineNo,6)+ORDER+STYLE+STR(LINENO,6),lcCutPick) , IIF(TotCut = &lcCutPick..TOTQTY,'*','»*'),'»'),' '):H='Allocated',"+;
           "Order,Complete,Account,Store,Style,"+;
           "TotQty:H='Tot. Ord',Qty1:H='Ord 1', Qty2:H='Ord 2', Qty3:H='Ord 3',"+;
           "Qty4:H='Ord 4', Qty5:H='Ord 5', Qty6:H='Ord 6',"+;
           "Qty7:H='Ord 7', Qty8:H='Ord 8', CustPo:H='Cust Po', Price:H='Price'"
*B802057,1 Reham On 03/22/99   *** Begin ***
ENDIF
*B802057,1 Reham On 03/22/99   *** End   ***
*E301182,4 Reham On 03/23/99   *** End   ***

BROWSE FIELDS &lcOrdFld ;
       FOR &lcOrderLin..Style = &lcMainLin..Style ;
       WHEN lfwOrder()    ;
       VALID :F lfvBrow() ;
       NOEDIT           ;
       LOCK 0           ;
       NOMENU           ;
       NOAPPEND         ;
       NODELETE         ;
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       WINDOW (lcAloct3)  ;
       IN WINDOW (gcBaseWind) ;
       TITLE lcOrdBrwT

=lfwOrder()

SELECT (lcAlocFile)

*!*************************************************************
*! Name      : lfwOrder
*! Developer : Reham Al-Allamy
*! Date      : 12/23/1997
*! Purpose   : When function for the order lines browse.
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwOrder()
*!*************************************************************
*
FUNCTION lfwOrder

SELECT (lcOrderLin)
lnMarkB     = RECNO(lcOrderLin)
lcCurOrder  = &lcOrderLin..Order

*-- Display the available qty. from the order lines.
IF !EMPTY(&lcOrderLin..Order)
  STORE 0 TO laAvl
  FOR lnCount = 1 TO 8
    lcCount = ALLTRIM(STR(lnCount))
    laAvl[lnCount] = &lcOrderLin..Qty&lcCount - &lcOrderLin..Cut&lcCount
    laAvl[9]       = laAvl[9] + laAvl[lnCount]
  ENDFOR
ELSE
  STORE 0 TO laAvl
ENDIF

*E301182,4 Reham On 03/23/99   *** Begin ***
*E301182,4 Change the Cutpkord index in the CutPick file to include the cTktLineNo field
*IF &lcMainLin..TotOrd <> 0 .AND. ;
   SEEK(lcKeyCode+lcKeyFld+&lcOrderLin..Order+&lcOrderLin..Style+STR(&lcOrderLin..LineNo,6),lcCutPick)
IF &lcMainLin..TotOrd <> 0 .AND. ;
   SEEK(lcKeyCode+lcKeyFld+STR(&lcMainLin..LineNo,6)+&lcOrderLin..Order+&lcOrderLin..Style+STR(&lcOrderLin..LineNo,6),lcCutPick)
*E301182,4 Reham On 03/23/99   *** Begin ***

  lcRelStat = IIF(laScrMode[3] , "ENABLE" , "DISABLE")
  lcOrdStat = IIF(laScrMode[3] , "ENABLE" , "DISABLE")
  SELECT (lcCutPick)
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOrd
  SELECT (lcOrderLin)
ELSE
  lcRelStat = "DISABLE"
  lcOrdStat = "DISABLE"
  laOrd = 0
ENDIF
SHOW GET pbRel   &lcRelStat
SHOW GET pbOrder &lcOrdStat

*-- Check if there is available qty. to be allocated, enable the link buuton.
IF(((&lcMainLin..Qty1-&lcMainLin..Ord1<>0) .AND. (&lcOrderLin..Qty1-&lcOrderLin..Cut1<>0)) OR;
   ((&lcMainLin..Qty2-&lcMainLin..Ord2<>0) .AND. (&lcOrderLin..Qty2-&lcOrderLin..Cut2<>0)) OR;
   ((&lcMainLin..Qty3-&lcMainLin..Ord3<>0) .AND. (&lcOrderLin..Qty3-&lcOrderLin..Cut3<>0)) OR;
   ((&lcMainLin..Qty4-&lcMainLin..Ord4<>0) .AND. (&lcOrderLin..Qty4-&lcOrderLin..Cut4<>0)) OR;
   ((&lcMainLin..Qty5-&lcMainLin..Ord5<>0) .AND. (&lcOrderLin..Qty5-&lcOrderLin..Cut5<>0)) OR;
   ((&lcMainLin..Qty6-&lcMainLin..Ord6<>0) .AND. (&lcOrderLin..Qty6-&lcOrderLin..Cut6<>0)) OR;            
   ((&lcMainLin..Qty7-&lcMainLin..Ord7<>0) .AND. (&lcOrderLin..Qty7-&lcOrderLin..Cut7<>0)) OR;            
   ((&lcMainLin..Qty8-&lcMainLin..Ord8<>0) .AND. (&lcOrderLin..Qty8-&lcOrderLin..Cut8<>0)))
  lcLnkStat = IIF(laScrMode[3] , "ENABLE" , "DISABLE")
ELSE
  lcLnkStat = "DISABLE"
ENDIF
SHOW GET pbLink &lcLnkStat

*-- Disable the sales order bar if the there is no order.
SET SKIP OF BAR 2 OF PAD _INQUIRE OF _MSYSMENU (EMPTY(lcCurOrder))

*-- Refresh the say objects in the screen.
=lfRefresh(lcAloct2)

SHOW WINDOW (lcOrdBrwT) REFRESH

*!*************************************************************
*! Name      : lfvRel
*! Developer : Reham Al-Allamy
*! Date      : 12/25/1997
*! Purpose   : Valid function for the push button <RELEASE>
*!*************************************************************
*! Calls     : lfRelLin, lfwMain
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRel()
*!*************************************************************
*
FUNCTION lfvRel

*** Remove relation between the current   ***
*** {IIF(lcAllocate="C" , "C/T" , "P/O")} ***
*** line and the current order line?      ***
*** < Yes > - < No > ***
*C101944,1 HBG 10/29/2000 IF this order multi Store , Ask user if want to 
*C101944,1                 Release the link with all avilabel stores[Begin]
STORE .F. TO llRelMulSt , llMultiStr 
IF SEEK('O'+ &lcCutPick..Order,'OrdHdr')
  IF ALLTRIM(OrdHdr.multi) = 'Y'
    *-- Check if this Order include same style in Multi stores
    = lfChkMultS()
    IF llMultiStr 
      lcMessage = "Release the link with"
      llRelMulSt = IIF(gfModalGen("INM34176B34001" , "DIALOG" ,lcMessage)=1,.T.,.F.)
    ENDIF  
  ENDIF
ENDIF
*-- IF user want to release all stores 
IF llRelMulSt
  SELECT (lcCutPick)
  lcCurOrder = Order
  GO TOP
  *-- Loop in all the cutpick file lines to release the relation between
  *-- them & the related order lines.
  SCAN FOR &lcCutPick..Style = &lcMainLin..Style AND &lcCutPick..Order = lcCurOrder
    IF SEEK("O" + &lcCutPick..Order + &lcCutPick..cOrdLine , lcOrderLin)
      *-- Call function to release the relation for the current C/T or 
      *-- P/O line and the current order line.
      =lfRelLin()
    ENDIF
    SELECT (lcCutPick)
  ENDSCAN
  *-- Call the main browse when function to refresh the the objects 
  *-- in the screen after changing.
  =lfwMain()
ELSE
*C101944,1 [End]
  IF gfModalGen("QRM32050B00006" , "DIALOG" , IIF(lcAllocate="C" , "C/T" , "P/O")) = 1
    SELECT (lcOrderLin)
    IF SEEK("O" + &lcCutPick..Order + &lcCutPick..cOrdLine)
      *-- Call function to release the relation for the current C/T or 
      *-- P/O line and the current order line.
      =lfRelLin()
      SELECT (lcCutPick)
      *DELETE
      *-- Call the main browse when function to refresh the the objects 
      *-- in the screen after changing.
      =lfwMain()
    ENDIF
  ENDIF
*C101944,1 HBG 10/29/2000 End IF user want to release all stores [Begin]
ENDIF
*C101944,1 [End]
SELECT (lcAlocFile)

*!*************************************************************
*! Name      : lfvRelAll
*! Developer : Reham Al-Allamy
*! Date      : 12/25/1997
*! Purpose   : Valid function for the push button <RELEASE ALL>
*!*************************************************************
*! Calls     : lfRelLin, lfwMain
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRelAll()
*!*************************************************************
*
FUNCTION lfvRelAll

*** Remove relation between the current   ***
*** {IIF(lcAllocate="C" , "C/T" , "P/O")} ***
*** line and all order lines? ***
*** < Yes > - < No > ***
IF gfModalGen("QRM32051B00006" , "DIALOG" , IIF(lcAllocate="C" , "C/T" , "P/O")) = 1
  SELECT (lcCutPick)
  GO TOP
  *-- Loop in all the cutpick file lines to release the relation between
  *-- them & the related order lines.
  SCAN FOR &lcCutPick..Style = &lcMainLin..Style
    IF SEEK("O" + &lcCutPick..Order + &lcCutPick..cOrdLine , lcOrderLin)
      *-- Call function to release the relation for the current C/T or 
      *-- P/O line and the current order line.
      =lfRelLin()
    ENDIF
    SELECT (lcCutPick)
  ENDSCAN
  *DELETE ALL FOR cStatus = "D"
  *-- Call the main browse when function to refresh the the objects 
  *-- in the screen after changing.
  =lfwMain()
ENDIF
SELECT (lcAlocFile)

*!*************************************************************
*! Name      : lfRelLin
*! Developer : Reham Al-Allamy
*! Date      : 12/25/1997
*! Purpose   : Function to release the current c/t or p/o line
*!           : and the current order line.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfRelLin()
*!*************************************************************
*
FUNCTION lfRelLin

*-- The screen's objects has been updated.
llCUpDate  = .T.

*E301289,1 WAB - CalCulate Order Qty before release the relation between
*E301289,1 WAB - (Po/CT) and Order Line 
*E301289,1 WAB - START
IF llDispPric
  lnOldQty=0
  For lnI = 1 To 8
    lcI = ALLTRIM(STR(lnI))
    lnOldQty = lnOldqty + &lcMainLin..Ord&lcI
  ENDFOR
  lnSelPrice  = &lcMainLin..nSelPrice  && selling price before release relation
ENDIF
*E301289,1 WAB - END

*-- Update the C/T or P/O lines with the new qty.
REPLACE &lcMainLin..Ord1    WITH &lcMainLin..Ord1   - &lcCutPick..Qty1 ;
        &lcMainLin..Ord2    WITH &lcMainLin..Ord2   - &lcCutPick..Qty2 ;
        &lcMainLin..Ord3    WITH &lcMainLin..Ord3   - &lcCutPick..Qty3 ;
        &lcMainLin..Ord4    WITH &lcMainLin..Ord4   - &lcCutPick..Qty4 ;
        &lcMainLin..Ord5    WITH &lcMainLin..Ord5   - &lcCutPick..Qty5 ;
        &lcMainLin..Ord6    WITH &lcMainLin..Ord6   - &lcCutPick..Qty6 ;
        &lcMainLin..Ord7    WITH &lcMainLin..Ord7   - &lcCutPick..Qty7 ;
        &lcMainLin..Ord8    WITH &lcMainLin..Ord8   - &lcCutPick..Qty8 ;
        &lcMainLin..TotOrd  WITH &lcMainLin..TotOrd - &lcCutPick..TotQty
REPLACE &lcMainLin..cStatus WITH IIF(&lcMainLin..cStatus = 'A' , 'A' ,'M')

*E301289,1 WAB - CalCulate Order Qty after release the relation between
*E301289,1 WAB - (Po/CT) and Order Line & return the old selling price to (PO/CT) line
*E301289,1 WAB - START

IF llDispPric
  STORE 0 to lnTotQty,lnRotSub,lnGrosMrgn
  For lnI = 1 To 8
    lcI = ALLTRIM(STR(lnI))
    lnTotQty = lnTotQty + &lcMainLin..Ord&lcI
  ENDFOR
  lnTotQty = lnOldQty - lnTotQty
  lnSelPrice  = ((lnOldQty * lnSelPrice)-(lnTotQty * &lcOrderLin..Price))/lnTotQty
  =SEEK(&lcMainLin..Style,'Style')
  IF lnSelPrice = 0 
    lnSElPrice = Style.PriceA
  ENDIF
  IF lcAllocate = 'P'
    lnTotCost   = &lcMainLin..nEcost1 + &lcMainLin..nEcost2 + &lcMainLin..nEcost3 +;
                  &lcMainLin..nEcost4 + &lcMainLin..nEcost5
  ELSE
    lnTotCost   = STYLE.TOTCOST
  ENDIF
  lnRotSub    = IIF(llStyMark,lnTotCost,lnSElPrice)
  lnGrosMrgn = IIF(lnRotSub=0,0,((lnSElPrice - lnTotCost)/lnRotSub)*100)
  REPLACE &lcMainLin..nSelPrice WITH lnSelPrice,;
          &lcMainLin..nGrosMrgn WITH lnGrosMrgn
ENDIF
*E301289,1 WAB - END

*-- Subtract the cutpick qty. from the order line.
REPLACE &lcOrderLin..Cut1    WITH &lcOrderLin..Cut1   - &lcCutPick..Qty1 ;
        &lcOrderLin..Cut2    WITH &lcOrderLin..Cut2   - &lcCutPick..Qty2 ;
        &lcOrderLin..Cut3    WITH &lcOrderLin..Cut3   - &lcCutPick..Qty3 ;
        &lcOrderLin..Cut4    WITH &lcOrderLin..Cut4   - &lcCutPick..Qty4 ;
        &lcOrderLin..Cut5    WITH &lcOrderLin..Cut5   - &lcCutPick..Qty5 ;
        &lcOrderLin..Cut6    WITH &lcOrderLin..Cut6   - &lcCutPick..Qty6 ;
        &lcOrderLin..Cut7    WITH &lcOrderLin..Cut7   - &lcCutPick..Qty7 ;
        &lcOrderLin..Cut8    WITH &lcOrderLin..Cut8   - &lcCutPick..Qty8 ;
        &lcOrderLin..TotCut  WITH &lcOrderLin..TotCut - &lcCutPick..TotQty
REPLACE &lcOrderLin..cStatus WITH IIF(&lcOrderLin..cStatus = 'A' , 'A' ,'M')

*-- Blank the qty. in the cutpick file & delete the record.
SELECT (lcCutPick)
REPLACE &lcCutPick..cStatus WITH SUBSTR('DDS',AT(&lcCutPick..cStatus,'SMA'),1) ;
        &lcCutPick..QTY1    WITH 0   ;
        &lcCutPick..QTY2    WITH 0   ;
        &lcCutPick..QTY3    WITH 0   ;
        &lcCutPick..QTY4    WITH 0   ;
        &lcCutPick..QTY5    WITH 0   ;
        &lcCutPick..QTY6    WITH 0   ;
        &lcCutPick..QTY7    WITH 0   ;
        &lcCutPick..QTY8    WITH 0   ;
        &lcCutPick..TOTQTY  WITH 0
DELETE

*!*************************************************************
*! Name      : lfvLink
*! Developer : Reham Al-Allamy
*! Date      : 12/25/1997
*! Purpose   : Valid function for the push button <LINK>
*!*************************************************************
*! Calls     : SOALOMD.SPX, lfwMain
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvLink()
*!*************************************************************
*
FUNCTION lfvLink
PRIVATE laQty

*B802057,1 Reham On 03/24/99  *** Begin ***
llRepDye = .F.    && Flag to know if replace cyurrent order line with the current CT/PO line.
*-- If system is set to use dyelot.
IF llDyelot
  *-- Seek to know if the current style is dyelot yes or not.
  llStyDye = .F.
  SELECT STYLE
  IF SEEK(&lcMainLin..Style , "STYLE")
    llStyDye = (Style.cdye_flg = "Y")
  ENDIF
  *-- If the current style use dyelots.
  IF llStyDye
    DO CASE
      *-- If there is dyelot in CT/PO line & there is no dyelot in the order line, 
      *-- Allocate & replace the CT/PO line dyelot in the order line dyelot.
      CASE !EMPTY(&lcMainLin..Dyelot) .AND. EMPTY(&lcOrderLin..Dyelot)
        llRepDye  = .T.
      *-- If there is dyelot in both CT/PO line & order line.
      CASE !EMPTY(&lcMainLin..Dyelot) .AND. !EMPTY(&lcOrderLin..Dyelot)
        *-- If both CT/PO line & order line dyelots are different, Cannot allocate.
        IF &lcMainLin..Dyelot <> &lcOrderLin..Dyelot
          *** {IIF(lcAllocate="C" , "C/T" , "P/O")} line and the current ***
          *** order line have different dyelots. Cannot allocate!        ***
          *** <  OK  > ***
          =gfModalGen("INM32064B00000" , "DIALOG" , IIF(lcAllocate="C" , "C/T" , "P/O"))
          SELECT (lcAlocFile)
          RETURN
        ELSE
          *-- If both CT/PO line & order line dyelots are the same, check for any
          *-- other previously allocated CT/PO line.
          SELECT CUTPICK
          LOCATE FOR Cutpick.order+Cutpick.cordline+Cutpick.style = ;
                     &lcOrderLin..Order+STR(&lcOrderLin..LineNo,6)+&lcOrderLin..Style ;
                     .AND. Cutpick.ctktno <> lcKeyFld
          IF FOUND()
            *-- Prepare the seek condition & the search file.
            lcSeekLin = IIF(lcAllocate="C" , laData[1] , 'P'+laData[2]) + &lcOrderLin..Style
            lcSeekFil = IIF(lcAllocate="C" , "CutTktL" , "PosLn")
            IF SEEK(lcSeekLin , lcSeekFil)
              *-- If the current order line has been allocated befor to another CT/PO
              *-- line has different line, do not allocate.
              IF &lcSeekFil..Dyelot <> &lcMainLin..Dyelot
                *** Current order line have been allocated befor to another        ***
                *** IIF(lcAllocate="C" , "C/T" , "P/O") that has different dyelot. ***
                *** Cannot Allocate! ***
                *** < Ok > ***
                =gfModalGen("INM32065B00000" , "DIALOG" , IIF(lcAllocate="C" , "C/T" , "P/O"))
                SELECT (lcAlocFile)
                RETURN
              ELSE
                *-- If current order line has been allocated befor to another CT/PO line
                *-- has the same dyelot, warn the user to allocate or cancel.
                
                *** Current order line have been allocated befor to another       ***
                *** IIF(lcAllocate="C" , "C/T" , "P/O") that has the same dyelot. ***
                *** < Allocate > - < Cancel > ***
                IF gfModalGen("INM32066B32007" , "DIALOG" , IIF(lcAllocate="C" , "C/T" , "P/O")) = 2
                  SELECT (lcAlocFile)
                  RETURN
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
    ENDCASE
  ENDIF
  SELECT (lcAlocFile)
ENDIF
*B802057,1 Reham On 03/24/99  *** End   ***

*-- The screen's objects has been updated.
llCUpDate  = .T.

*-- Save the delete settings.

*C101944,1 HBG 10/29/2000 Cooment this block of code because it is no longer used here [Begin]
*lcDelSet = SET("DELETE")
*SET DELETE OFF
*C101944,1 [End]
 
DECLARE laQty[9]

*-- Seek for the current line in the temp cutpick file.
*C101944,1 HBG 10/29/2000 Comment this block of code to fix bug of 
*C101944,1                Allocating the orderline even if user 
*C101944,1                chose Cancel [Begin]
SELECT (lcCutPick)
*E301182,4 Reham On 03/23/99   *** Begin ***
*E301182,4 Change the Cutpkord index in the CutPick file to include the cTktLineNo field
*IF !SEEK(lcKeyCode+lcKeyFld+&lcOrderLin..Order+&lcOrderLin..Style+STR(&lcOrderLin..LineNo,6),lcCutPick)
*IF !SEEK(lcKeyCode+lcKeyFld+STR(&lcMainLin..LineNo,6)+&lcOrderLin..Order+&lcOrderLin..Style+STR(&lcOrderLin..LineNo,6),lcCutPick)
*E301182,4 Reham On 03/23/99   *** End   ***
  *-- Add new record to the cutpick file if does not exist in the cutpick file.
 * STORE 0 TO laQty
 * llNew = .T.
 * APPEND BLANK
*ELSE
  *-- If the record exist & was deleted before.
 * IF DELETED()
    *-- Recall the current record in the temp. cutpick file.
  *  RECALL
 * ENDIF
  *E301182,4 Reham On 03/23/99   *** Begin ***
  *E301182,4 Change the Cutpkord index in the CutPick file to include the cTktLineNo field
  *-- Set flag to determine the status of the record.
  *llNew = !SEEK(lcKeyCode+lcKeyFld+&lcOrderLin..Order+&lcOrderLin..Style+STR(&lcOrderLin..LineNo,6),'CUTPICK')
  *llNew = !SEEK(lcKeyCode+lcKeyFld+STR(&lcMainLin..LineNo,6)+&lcOrderLin..Order+&lcOrderLin..Style+STR(&lcOrderLin..LineNo,6),'CUTPICK')
  *E301182,4 Reham On 03/23/99   *** End   ***
  *-- Fill the qty. array.
  *laQty[9] = 0
  *FOR lnCount = 1 TO 8
   * lcCount   = ALLTRIM(STR(lnCount))
   * laQty[lnCount] = &lcCutPick..Qty&lcCount
   * laQty[9]       = laQty[9] + laQty[lnCount]
  *ENDFOR
*ENDIF
*C101944,1 [End]

*C101944,1 HBG 10/29/2000 IF this order multi Store , Ask user if want to 
*C101944,1                Allocate all stores[Begin]

= ACOPY(laOrd,laOldLaOrd)

llMultiStr =  .F.

STORE 0 TO laOrd
IF SEEK('O'+ &lcOrderLin..Order,'OrdHdr')
  IF ALLTRIM(OrdHdr.multi) = 'Y'
    *-- Check if this Order include same style in Multi stores 
    = lfChkMultS()
    IF llMultiStr 
      lcMessage = "make the allocation from"
      llAloMulSt = IIF(gfModalGen("INM34176B34001" , "DIALOG" ,lcMessage)=1,.T.,.F.)
    ENDIF  
  ENDIF
ENDIF

*-- If user want to allocate all stores , Collect the Qty and the Cut of all lines for this order
IF llAloMulSt 
  lcCurAlias = SELECT()
  SELECT(lcOrderLin)
  lcOrder = &lcOrderLin..Order
  lcStyle = &lcOrderLin..Style  
  *LOCATE
  =SEEK('O'+lcOrder)
  SCAN REST WHILE cOrdType + order + STR(LineNo,6) = 'O'+ lcOrder;
            FOR style = lcStyle
    *-- If this line line is completely allocated , ignor it
    IF TotCut = TotQty
      LOOP
    ENDIF  && End If this line line is completely allocated , ignor it
    FOR lnI = 1 TO 8
      lcI = ALLTRIM(STR(lnI))
      IF Cut&lcI = 0 OR Qty&lcI-Cut&lcI <> 0
        laStrQty[lnI] = laStrQty[lnI] + IIF(Cut&lcI = 0 , Qty&lcI , (Qty&lcI-Cut&lcI))
        laStrCut[lnI] = laStrCut[lnI] + Cut&lcI
      ENDIF  
    ENDFOR 
    IF TotCut = 0 OR TotQty-TotCut <> 0
      laStrQty[9] = laStrQty[9] + IIF(TotCut = 0 , TotQty , (TotQty-TotCut))
      laStrCut[9] = laStrCut[9] + TotCut
    ENDIF
  ENDSCAN 
  GO TOP
  SELECT(lcCurAlias)
ENDIF 
*C101944,1 [End]


*C101944,1 HBG 10/29/2000 Comment this block of code to fix bug of Allocating the orderline even if user chose Cancel [Begin]
*E301182,4 Reham On 03/23/99   *** Begin ***
*E301182,4 Change the Cutpkord index in the CutPick file to include the cTktLineNo field
*REPLACE cTktNo   WITH lcKeyFld  ;
        TranCd   WITH lcKeyCode ;
        Order    WITH &lcOrderLin..Order ;
        cOrdLine WITH STR(&lcOrderLin..LINENO,6) ;
        Style    WITH &lcOrderLin..Style ;
        cStatus  WITH IIF(llNew , 'A' , 'M') ; 
        nRecNo   WITH IIF(llNew , 0 , RECNO("CUTPICK"))
*REPLACE cTktNo     WITH lcKeyFld  ;
        TranCd     WITH lcKeyCode ;
        cTktLineNo WITH STR(&lcMainLin..LineNo,6) ;
        Order      WITH &lcOrderLin..Order ;
        cOrdLine   WITH STR(&lcOrderLin..LINENO,6) ;
        Style      WITH &lcOrderLin..Style ;
        cStatus    WITH IIF(llNew , 'A' , 'M') ; 
        nRecNo     WITH IIF(llNew , 0 , RECNO("CUTPICK"))
*E301182,4 Reham On 03/23/99   *** End   ***

*-- Update the cutpick file with the modified qty.
 * REPLACE Qty1     WITH Qty1 + MIN((&lcOrderLin..Qty1-&lcOrderLin..Cut1),(&lcMainLin..Qty1-&lcMainLin..Ord1)) ;
          Qty2     WITH Qty2 + MIN((&lcOrderLin..Qty2-&lcOrderLin..Cut2),(&lcMainLin..Qty2-&lcMainLin..Ord2)) ;
          Qty3     WITH Qty3 + MIN((&lcOrderLin..Qty3-&lcOrderLin..Cut3),(&lcMainLin..Qty3-&lcMainLin..Ord3)) ;
          Qty4     WITH Qty4 + MIN((&lcOrderLin..Qty4-&lcOrderLin..Cut4),(&lcMainLin..Qty4-&lcMainLin..Ord4)) ;
          Qty5     WITH Qty5 + MIN((&lcOrderLin..Qty5-&lcOrderLin..Cut5),(&lcMainLin..Qty5-&lcMainLin..Ord5)) ;
          Qty6     WITH Qty6 + MIN((&lcOrderLin..Qty6-&lcOrderLin..Cut6),(&lcMainLin..Qty6-&lcMainLin..Ord6)) ;
          Qty7     WITH Qty7 + MIN((&lcOrderLin..Qty7-&lcOrderLin..Cut7),(&lcMainLin..Qty7-&lcMainLin..Ord7)) ;
          Qty8     WITH Qty8 + MIN((&lcOrderLin..Qty8-&lcOrderLin..Cut8),(&lcMainLin..Qty8-&lcMainLin..Ord8)) ;
          TotQty   WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        
*SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOrd
*-- Call global function to add audit fields info.
*=gfAdd_Info(lcCutPick)
*C101944,1 [End]

*C101944,1 HBG 10/29/2000 Get the order Qty[Bgin]
FOR lnI = 1 TO 8
  lcI = ALLTRIM(STR(lnI))
  laOrd[lnI] = laOrd[lnI] + MIN(IIF(llAloMulSt,(laStrQty[lnI]),;
                             (&lcOrderLin..Qty&lcI-&lcOrderLin..Cut&lcI)),;
                             (&lcMainLin..Qty&lcI-&lcMainLin..Ord&lcI)) 
  laOrd[9]   = laOrd[9] + laOrd[lnI] 
ENDFOR
*C101944,1 [End]


*C101944,1 HBG 10/29/2000 Comment this block of code to fix bug of Allocating the 
*C101944,1                 orderline even if user chose Cancel [Begin]
*-- Restore the delete setting.
*SET DELETE & lcDelSet

*-- Update the temp P/O or C/T lines with the modified qty.
*SELECT (lcMainLin)

*E301289,1 WAB - Calculate  total Order Qty from (Po/Ct) line
*E301289,1 WAB - START
*IF llDispPric
 * lnOldQty = 0
 * FOR lnI = 1 TO 8
  *  lcI = ALLTRIM(STR(lnI))
  *  lnOldQty = lnOldQty + ord&lcI
  *ENDFOR
*ENDIF
*E301289,1 WAB - END

*REPLACE Ord1    WITH Ord1 - laQty[1] + &lcCutPick..Qty1 ;
        Ord2    WITH Ord2 - laQty[2] + &lcCutPick..Qty2 ;
        Ord3    WITH Ord3 - laQty[3] + &lcCutPick..Qty3 ;
        Ord4    WITH Ord4 - laQty[4] + &lcCutPick..Qty4 ;
        Ord5    WITH Ord5 - laQty[5] + &lcCutPick..Qty5 ;
        Ord6    WITH Ord6 - laQty[6] + &lcCutPick..Qty6 ;
        Ord7    WITH Ord7 - laQty[7] + &lcCutPick..Qty7 ;
        Ord8    WITH Ord8 - laQty[8] + &lcCutPick..Qty8 ;
        TotOrd  WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
*REPLACE cStatus WITH IIF(cStatus = 'A' , 'A' ,'M')

*-- Update the temp. order lines file with the modified qty.
*SELECT (lcOrderLin)

 * REPLACE Cut1    WITH Cut1 - laQty[1] + &lcCutPick..Qty1 ;
          Cut2    WITH Cut2 - laQty[2] + &lcCutPick..Qty2 ;
          Cut3    WITH Cut3 - laQty[3] + &lcCutPick..Qty3 ;
          Cut4    WITH Cut4 - laQty[4] + &lcCutPick..Qty4 ;
          Cut5    WITH Cut5 - laQty[5] + &lcCutPick..Qty5 ;
          Cut6    WITH Cut6 - laQty[6] + &lcCutPick..Qty6 ;
          Cut7    WITH Cut7 - laQty[7] + &lcCutPick..Qty7 ;
          Cut8    WITH Cut8 - laQty[8] + &lcCutPick..Qty8 ;
          TotCut  WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
      
*REPLACE cStatus WITH IIF(cStatus = 'A' , 'A' ,'M')

*B802057,1 Reham On 03/24/99   *** Begin ***
*-- If there is dyelot in CT/PO line & there is no dyelot in the order line, 
*-- replace the CT/PO line dyelot in the order line dyelot.
*IF llRepDye
 * REPLACE Dyelot WITH &lcMainLin..Dyelot
*ENDIF
*B802057,1 Reham On 03/24/99   *** End   ***
*C101944,1 [End]

lcModTitle = 'Generated Qty For ' + lcStyHdr + ' : ' + &lcOrderLin..Style + ' Against Order: ' + lcCurOrder

*= ACOPY(laOrd,laEdtQty)
= ACOPY(laOrd,laEdtQty1)


*-- Call the Edit Cost screen for the current style.
PUSH KEY
ON KEY
DO (gcScrDir+"SO\SOALOMD.SPX")
POP KEY

=lfwMain()
SELECT (lcAlocFile)

*!*************************************************************
*! Name      : lfwFltrOrd
*! Developer : Reham Al-Allamy
*! Date      : 12/28/1997
*! Purpose   : When function for the popup filter.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwFltrOrd()
*!*************************************************************
*
FUNCTION lfwFltrOrd

*-- Save the old value of the filter popup.
lnOldFlt = puShow

*!*************************************************************
*! Name      : lfvFltrOrd
*! Developer : Reham Al-Allamy
*! Date      : 12/28/1997
*! Purpose   : Valid function for the popup filter.
*!*************************************************************
*! Calls     : lfwOrder
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFltrOrd()
*!*************************************************************
*
FUNCTION lfvFltrOrd

*-- If the popup value changed.
IF !(lnOldFlt == puShow)
  SELECT (lcOrderLin)
  SET FILTER TO
  *B602540,1  Reham on 02/25/99   *** Begin ***
  *B602540,1  Point to the top of the files to void the following errors:
  *B602540,1  1- Duplicate records of the order lines.
  *B602540,1  2- Not updating the ORD fields in the CutTktL file.
  *B602540,1  3- Ability to link the same line twice.
  GO TOP IN (lcOrderLin)
  *B602540,1  Reham on 02/25/99   *** End   ***
  DO CASE
    *-- Show linked order lines only.
    CASE puShow = 2
      *E301182,4 Reham On 03/23/99   *** Begin ***
      *E301182,4 Change the Cutpkord index in the CutPick file to include the cTktLineNo field
      *SET FILTER TO &lcOrderLin..TotCut <> 0 .AND. ;
          SEEK(lcKeyCode + lcKeyFld + &lcOrderLin..ORDER + ;
               &lcOrderLin..STYLE + STR(&lcOrderLin..LineNo,6) , lcCutPick)
      SET FILTER TO &lcOrderLin..TotCut <> 0 .AND. ;
          SEEK(lcKeyCode + lcKeyFld + STR(&lcMainLin..LineNo,6) + &lcOrderLin..ORDER + ;
               &lcOrderLin..STYLE + STR(&lcOrderLin..LineNo,6) , lcCutPick)
      *E301182,4 Reham On 03/23/99   *** End   ***
    *-- Show order lines with available qty.
    CASE puShow = 3
      SET FILTER TO (&lcOrderLin..TotQty - &lcOrderLin..TotCut) <> 0
    *-- Show both linked order lines & order lines with available qty.
    CASE puShow = 4
      *E301182,4 Reham On 03/23/99   *** Begin ***
      *E301182,4 Change the Cutpkord index in the CutPick file to include the cTktLineNo field
      *SET FILTER TO ((&lcOrderLin..TotQty - &lcOrderLin..TotCut) <> 0) .OR. ;
                    ((&lcOrderLin..TotCut) <> 0 .AND. ;
          SEEK(lcKeyCode + lcKeyFld + &lcOrderLin..ORDER + ;
               &lcOrderLin..STYLE + STR(&lcOrderLin..LineNo,6) , lcCutPick))
      SET FILTER TO ((&lcOrderLin..TotQty - &lcOrderLin..TotCut) <> 0) .OR. ;
                    ((&lcOrderLin..TotCut) <> 0 .AND. ;
          SEEK(lcKeyCode + lcKeyFld + STR(&lcMainLin..LineNo,6) + &lcOrderLin..ORDER + ;
               &lcOrderLin..STYLE + STR(&lcOrderLin..LineNo,6) , lcCutPick))
      *E301182,4 Reham On 03/23/99   *** End   ***
  ENDCASE
  *-- Call the order browse when function to refresh the objects
  *-- and the order browse.
  =lfwOrder()
ENDIF
SELECT (lcAlocFile)

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Reham Al-Allamy
*! Date      : 12/23/1997
*! Purpose   : Local function save called from the gcpSave function.
*!*************************************************************
*! Calls     : gfTmp2Mast, lfvFltrOrd
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpSavScr()
*!*************************************************************
*
FUNCTION lpSavScr

*-- Clear the filter in the order lines temp file till finish saving.
SELECT (lcOrderLin)
SET FILTER TO
*B602776,1 Reham On 05/04/99    *** Begin ***
SET RELATION TO &lcOrderLin..cordtype+&lcOrderLin..order+STR(&lcOrderLin..lineno,6) INTO Ordline
lnOldQty = 0
*B602776,1 Reham On 05/04/99    *** End   ***
GO TOP

*-- Update the total cut qty. field in the order header file.
lnTotQty = 0                    && Var. hold the total qty. for the first order.
lcOrder  = &lcOrderLin..Order   && Var. hold the first order.
SCAN
  *-- If the order no. changed.
  IF lcOrder <> &lcOrderLin..Order
    *-- Seek for the order no. in the order header file to update 
    *-- the total order qty. field.
    IF SEEK("O" + lcOrder , "OrdHdr")
      SELECT OrdHdr
      *B602776,1 Reham On 05/04/99   *** Begin ***
      *B602776,1 Update the order header file with right qty.
      *REPLACE TotCut WITH lnTotQty
      REPLACE TotCut WITH TotCut - lnOldQty + lnTotQty
      *B602776,1 Reham On 05/04/99   *** End   ***
      SELECT (lcOrderLin)
    ENDIF
    *B602776,1 Reham On 05/05/99   *** Begin ***
    lnOldQty = OrdLine.TotCut
    *B602776,1 Reham On 05/05/99   *** End   ***
    *-- Acumelate the total order qty. for the current order.
    lnTotQty = &lcOrderLin..TotCut
    *-- Change the order variable with the new order no.
    lcOrder  = &lcOrderLin..Order
  ELSE
    *B602776,1 Reham On 05/05/99   *** Begin ***
    lnOldQty = lnOldQty + OrdLine.TotCut
    *B602776,1 Reham On 05/05/99   *** End   ***
    *-- Acumelate the total order qty. for the current order.
    lnTotQty = lnTotQty + &lcOrderLin..TotCut
  ENDIF
ENDSCAN

*-- Seek for the order no. in the order header file to update 
*-- the total order qty. field.
IF SEEK("O" + lcOrder , "OrdHdr")
  SELECT OrdHdr
  *B602776,1 Reham On 05/04/99   *** Begin ***
  *B602776,1 Update the order header file with right qty.
  *REPLACE TotCut WITH lnTotQty
  REPLACE TotCut WITH TotCut - lnOldQty + lnTotQty
  *B602776,1 Reham On 05/04/99   *** End   ***
ENDIF

*B602776,1 Reham On 05/05/99   *** Begin ***
*B602776,1 Release the relation between the temp. order line & the master order line.
SELECT (lcOrderLin)
SET RELATION TO
*B602776,1 Reham On 05/05/99   *** End   ***

*-- Update the total ordered qty. field in the C/T or P/O header file.
lnTotQty = 0
SELECT (lcMainLin)
SUM TotOrd FOR cStatus <> "D" TO lnTotQty

IF lcAllocate = "C"
  *-- Update the total order qty. field in the cutting ticket header file.
  IF SEEK(laData[1] , "CUTTKTH")
    SELECT CutTktH
    REPLACE TotOrd WITH lnTotQty
  ENDIF
ELSE
  *-- Update the total order qty. field in the style purchase order header file.
  IF SEEK("P" + laData[2] , "POSHDR")
    SELECT PosHdr
    REPLACE TotOrd WITH lnTotQty
  ENDIF
ENDIF

*-- Update the C/T or P/O lines according to the current program.
=gfTmp2Mast(IIF(lcAllocate="C", "CUTTKTL" , "POSLN") , lcMainLin , ;
            "Update the "+IIF(lcAllocate="C" , "cutting ticket" , ;
            "style purchase order")+" lines file...")

*-- Update the cutpick file.
=gfTmp2Mast('CUTPICK' , lcCutPick , 'Update the CutPick file...')

*-- Update the order line file.
=gfTmp2Mast('ORDLINE' , lcOrderLin , 'Update the order line file...')

*B602540,1  Reham on 02/25/99   *** Begin ***
*B602540,1  Point to the top of the files to void the following errors:
*B602540,1  1- Duplicate records of the order lines.
*B602540,1  2- Not updating the ORD fields in the CutTktL file.
*B602540,1  3- Ability to link the same line twice.
GO TOP IN (lcMainLin)
GO TOP IN (lcOrderLin)
GO TOP IN (lcCutPick)
*B602540,1  Reham on 02/25/99   *** End   ***

*-- Set the ld filter again.
lnOldFlt = 0
=lfvFltrOrd()

*-- Blank the variable to force collecting the data again.
lcCurKey = " "

SELECT (lcAlocFile)

*!*************************************************************
*! Name      : lpClsScr
*! Developer : Reham Al-Allamy
*! Date      : 01/28/1998
*! Purpose   : Local cancel function called if cancel the current modification.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpClsScr()
*!*************************************************************
*
FUNCTION lpClsScr

*-- Blank the variable to force collecting the data again.
lcCurKey = " "

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Reham Al-Allamy
*! Date      : 12/25/1997
*! Purpose   : Valid function for the push button <Order>
*!*************************************************************
*! Calls     : SOALOMD.SPX, lfwMain
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvOrder()
*!*************************************************************
*
FUNCTION lfvOrder

SELECT (lcOrderLin)
=SEEK("O" + &lcCutPick..Order + &lcCutPick..cOrdLine)

lcModTitle = 'Generated Qty For ' + lcStyHdr + ' : ' + &lcOrderLin..Style + ' Against Order: ' + lcCurOrder

*C101944,1 HBG 10/29/2000 Set the flag to true because we change 
*C101944,1                the allocation Qty from Order button[Begin]
= ACOPY(laOrd,laOldLaOrd)
llOrder = .T.
*C101944,1 [End]

*=ACOPY(laOrd,laEdtQty)
=ACOPY(laOrd,laEdtQty1)
*-- Call the Edit Cost screen for the current style.
PUSH KEY
ON KEY
DO (gcScrDir+"SO\SOALOMD.SPX")
POP KEY

=lfwMain()
SELECT (lcAlocFile)

*!***************************************************************
*****              Functions for SOALOMD Screen             *****
***** Screen to modify the allocated qty. in the C/T or P/O *****
*!***************************************************************

*!*************************************************************
*! Name      : lfvEdtQty
*! Developer : Reham Al-Allamy
*! Date      : 12/29/1997
*! Purpose   : Valid function for the generated qty. in the 
*!           : modify screen.
*!*************************************************************
*! Calls     : lfModRef
*!*************************************************************
*! Passed Parameters  :  lnNo -> Which qty. no.
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvEdtQty()
*!*************************************************************
*
FUNCTION lfvEdtQty
PARAMETERS lnNo

*-- The screen's objects has been updated.
llCUpDate  = .T.

lcNo  = ALLTRIM(STR(lnNo))

*-- Get the maximum limit for entering the current qty.
*C101944,1 HBG 10/29/2000  Validate the Allocated field in the allocating 
*C101944,1                 screen depending on: if order is multi store and 
*C101944,1                 user want to alocate all stores or not [Begin]
*lnMax = MIN((&lcMainLin..Qty&lcNo - &lcMainLin..Ord&lcNo + laOrd[lnNo]),;
            (&lcOrderLin..Qty&lcNo - &lcOrderLin..Cut&lcNo + laOrd[lnNo]))

*IF laEdtQty1[lnNo] < 0 .OR. laEdtQty1[lnNo] > lnMax .OR. laEdtQty1[lnNo] > &lcOrderLin..Qty&lcNo
 * WAIT WINDOW 'RANGE: 0 to '+ ALLTRIM(STR(MIN(lnMax , &lcOrderLin..Qty&lcNo))) NOWAIT
 * laEdtQty1[lnNo] = laOrd[lnNo]
 * _CUROBJ        = OBJNUM(laEdtQty1[lnNo])
*ENDIF

IF llAloMulSt 
  lnMax =  MIN((&lcMainLin..Qty&lcNo - &lcMainLin..Ord&lcNo),(laStrQty[lnNo]))

  IF laEdtQty1[lnNo] < 0 .OR. laEdtQty1[lnNo] > lnMax .OR. laEdtQty1[lnNo] > laStrQty[lnNo]
    WAIT WINDOW 'RANGE: 0 to '+ ALLTRIM(STR(MIN(lnMax ,laStrQty[lnNo]))) NOWAIT
    laEdtQty1[lnNo] = laOrd[lnNo]
    _CUROBJ        = OBJNUM(laEdtQty1[lnNo])
  ENDIF
ELSE
  lnMax = MIN((&lcMainLin..Qty&lcNo - &lcMainLin..Ord&lcNo),;
             (&lcOrderLin..Qty&lcNo - &lcOrderLin..Cut&lcNo))
  lnMax = IIF(llOrder,lnMax + laOrd[lnNo],lnMax)

  IF laEdtQty1[lnNo] < 0 .OR. laEdtQty1[lnNo] > lnMax .OR. laEdtQty1[lnNo] > &lcOrderLin..Qty&lcNo
    WAIT WINDOW 'RANGE: 0 to '+ ALLTRIM(STR(MIN(lnMax , &lcOrderLin..Qty&lcNo))) NOWAIT
    laEdtQty1[lnNo] = laOrd[lnNo]
    _CUROBJ        = OBJNUM(laEdtQty1[lnNo])
  ENDIF
ENDIF
*C101944,1 [End]

*-- Calculate the total qty.
laEdtQty1[9] = laEdtQty1[1] + laEdtQty1[2] + laEdtQty1[3] + laEdtQty1[4] +;
              laEdtQty1[5] + laEdtQty1[6] + laEdtQty1[7] + laEdtQty1[8]

*-- Call the refresh function for the modify screen.
=lfModRef()

*!*************************************************************
*! Name      : lfvModOK
*! Developer : Reham Al-Allamy
*! Date      : 12/29/1997
*! Purpose   : Valid function for the ok button.
*!*************************************************************
*! Calls     : lfRelLin
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvModOK()
*!*************************************************************
*
FUNCTION lfvModOK

*-- The screen's objects has been updated.
llCUpDate  = .T.
=ACOPY(laEdtQty1,laEdtQty)

IF laEdtQty[9] = 0
  *C101944,1 HBG 10/29/2000 Change the message , because no relations have 
  *C101944,1                been made yet[Begin]
  *** Total Qty has become 0.  Remove  relation between ***
  *** the current {IIF(lcAllocate="C" , "C/T" , "P/O")} ***
  *** line and the current order line ? ***
  *** < YES > - < NO > ***
  *IF gfModalGen("QRM32052B00006" , "DIALOG" , IIF(lcAllocate="C" , "C/T" , "P/O")) = 1
    *-- Call function to release the relation for the current C/T or 
    *-- P/O line and the current order line.
   * =lfRelLin()
  *ENDIF
  *** The total Qty generated is 0.  No link can be made between
  *** the current {IIF(lcAllocate="C" , "C/T" , "P/O")} 
  *** line and the current order line ? 
  *** < YES > - < NO > ***
  = gfModalGen("QRM34178B000006" , "DIALOG" , IIF(lcAllocate="C" , "C/T" , "P/O"))
  *C101944,1 [End]
ELSE
  *C101944,1 HBG 10/29/2000 IF order is multi store and user want to 
  *C101944,1                allocate all stores , call Function to allocte 
  *C101944,1                them [Begin]
  IF llAloMulSt AND !llOrder
    *-- IF Generated Qty less Than Total Order Qty
    IF laEdtQty[9] < laStrQty[9]
      *-- Generated Qty less Than Total Order Qty. please Confirm allocation
      *-- <CONFIRM> <NO>
      IF gfModalGen("INM34177B34017" , "DIALOG") = 1
        = lfAloMulSt()
      ELSE
        RETURN
      ENDIF
    ELSE
      = lfAloMulSt()
    ENDIF
  ELSE
    *C101944,1 [End]
    *-- Update the cutpick file with the modified qty.
    SELECT (lcCutPick)
  
    *C101944,1 HBG 10/29/2000 Update the CutPick file in case of allocating
    *C101944,1                 each store separetly [Begin]
    lcDelSet = SET("DELETE")
    SET DELETE OFF
    
    IF !SEEK(lcKeyCode+lcKeyFld+STR(&lcMainLin..LineNo,6)+&lcOrderLin..Order+&lcOrderLin..Style+STR(&lcOrderLin..LineNo,6),lcCutPick)
      *-- Add new record to the cutpick file if does not exist in the cutpick file.
      STORE 0 TO laQty
      llNew = .T.
      APPEND BLANK
    ELSE
      *-- If the record exist & was deleted before.
      IF DELETED()
        *-- Recall the current record in the temp. cutpick file.
        RECALL
      ENDIF
      *-- Set flag to determine the status of the record.
      llNew = !SEEK(lcKeyCode+lcKeyFld+STR(&lcMainLin..LineNo,6)+&lcOrderLin..Order+&lcOrderLin..Style+STR(&lcOrderLin..LineNo,6),'CUTPICK')
    ENDIF
    REPLACE cTktNo     WITH lcKeyFld  ;
            TranCd     WITH lcKeyCode ;
            cTktLineNo WITH STR(&lcMainLin..LineNo,6) ;
            Order      WITH &lcOrderLin..Order ;
            cOrdLine   WITH STR(&lcOrderLin..LINENO,6) ;
            Style      WITH &lcOrderLin..Style ;
            cStatus    WITH IIF(llNew , 'A' , 'M') ; 
            nRecNo     WITH IIF(llNew , 0 , RECNO("CUTPICK"))
    REPLACE Qty1    WITH IIF(llOrder,0,Qty1)   + laEdtQty[1] ;
            Qty2    WITH IIF(llOrder,0,Qty2)   + laEdtQty[2] ;
            Qty3    WITH IIF(llOrder,0,Qty3)   + laEdtQty[3] ;
            Qty4    WITH IIF(llOrder,0,Qty4)   + laEdtQty[4] ;
            Qty5    WITH IIF(llOrder,0,Qty5)   + laEdtQty[5] ;
            Qty6    WITH IIF(llOrder,0,Qty6)   + laEdtQty[6] ;
            Qty7    WITH IIF(llOrder,0,Qty7)   + laEdtQty[7] ;
            Qty8    WITH IIF(llOrder,0,Qty8)   + laEdtQty[8] ;
            TotQty  WITH IIF(llOrder,0,TotQty) + laEdtQty[9] ;        
            cStatus WITH IIF(cStatus = 'A' , 'A' ,'M')
    *C101944,1 [End]
    *E301289,1 WAB -  calculate total generate qty of order line
    *E301289,1 WAB - START

    IF llDispPric
      lnTotQty = 0
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)      
        lnTotQty = lnTotQty + laEdtQty[lnI]
      ENDFOR
      lnSelPrice = lnTotQty * &lcOrderLin..Price 
    ENDIF  
    *E301289,1 WAB - END
    
    *C101944,1 HBG 10/29/2000 Retern the setting of the DELETE [Begin] 
    SET DELETE &lcDelSet 
    *C101944,1 [End]
    
    *-- Update the temp P/O or C/T lines with the modified qty.
    SELECT(lcMainLin)

    *E301289,1 WAB - calculate average selling price and calculate grosmargin 
    *E301289,1 WAB - [devide the diferent between price and cost [by cost or 
    *E301289,1 WAB - by selling price] depense on llstymark (setup) 
    *E301289,1 WAB - & replace seeling price,grossmargin in (PO / CT) line
    *E301289,1 WAB - START
    IF llDispPric  
      lnSelPrice = ( lnSelPrice + lnOldQty * nSelPrice ) / (lnTotQty+lnOldQty)
      =SEEK(&lcMainLin..Style,'Style')
      IF lcAllocate = 'P'
        lnTotCost   = &lcMainLin..nEcost1 + &lcMainLin..nEcost2 + &lcMainLin..nEcost3 +;
                      &lcMainLin..nEcost4 + &lcMainLin..nEcost5
      ELSE
        lnTotCost   = STYLE.TOTCOST
      ENDIF
      lnRotSub    = IIF(llStyMark,lnTotCost,lnSelPrice)
      lnGrosMrgn = IIF(lnRotSub=0,0,((lnSElPrice - lnTotCost)/lnRotSub)*100)
      REPLACE &lcMainLin..nSelPrice WITH lnSelPrice,;
              &lcMainLin..nGrosMrgn WITH lnGrosMrgn
    ENDIF

    *E301289,1 WAB - END
    *C101944,1 HBG 10/29/2000 Replace the Ord fields with itself blus the new Qty [Begin] 
    *REPLACE &lcMainLin..Ord1    WITH &lcMainLin..Ord1 - laOrd[1] + laEdtQty[1] ;
            &lcMainLin..Ord2    WITH &lcMainLin..Ord2 - laOrd[2] + laEdtQty[2] ;
            &lcMainLin..Ord3    WITH &lcMainLin..Ord3 - laOrd[3] + laEdtQty[3] ;
            &lcMainLin..Ord4    WITH &lcMainLin..Ord4 - laOrd[4] + laEdtQty[4] ;
            &lcMainLin..Ord5    WITH &lcMainLin..Ord5 - laOrd[5] + laEdtQty[5] ;
            &lcMainLin..Ord6    WITH &lcMainLin..Ord6 - laOrd[6] + laEdtQty[6] ;
            &lcMainLin..Ord7    WITH &lcMainLin..Ord7 - laOrd[7] + laEdtQty[7] ;
            &lcMainLin..Ord8    WITH &lcMainLin..Ord8 - laOrd[8] + laEdtQty[8] ;
            &lcMainLin..TotOrd  WITH &lcMainLin..TotOrd  - laOrd[9] + laEdtQty[9]
    
    REPLACE &lcMainLin..Ord1    WITH IIF(llOrder,&lcMainLin..Ord1 - laOrd[1],;
                                                 &lcMainLin..Ord1) + laEdtQty[1] ;
            &lcMainLin..Ord2    WITH IIF(llOrder,&lcMainLin..Ord2 - laOrd[2],;
                                                 &lcMainLin..Ord2) + laEdtQty[2] ;
            &lcMainLin..Ord3    WITH IIF(llOrder,&lcMainLin..Ord3 - laOrd[3],;
                                                 &lcMainLin..Ord3) + laEdtQty[3] ;
            &lcMainLin..Ord4    WITH IIF(llOrder,&lcMainLin..Ord4 - laOrd[4],;
                                                 &lcMainLin..Ord4) + laEdtQty[4] ;
            &lcMainLin..Ord5    WITH IIF(llOrder,&lcMainLin..Ord5 - laOrd[5],;
                                                 &lcMainLin..Ord5) + laEdtQty[5] ;
            &lcMainLin..Ord6    WITH IIF(llOrder,&lcMainLin..Ord6 - laOrd[6],;
                                                 &lcMainLin..Ord6) + laEdtQty[6] ;
            &lcMainLin..Ord7    WITH IIF(llOrder,&lcMainLin..Ord7 - laOrd[7],;
                                                 &lcMainLin..Ord7) + laEdtQty[7] ;
            &lcMainLin..Ord8    WITH IIF(llOrder,&lcMainLin..Ord8 - laOrd[8],;
                                                 &lcMainLin..Ord8) + laEdtQty[8] ;
            &lcMainLin..TotOrd  WITH IIF(llOrder,&lcMainLin..TotOrd  - laOrd[9],;
                                                 &lcMainLin..TotOrd) + laEdtQty[9]
    REPLACE &lcMainLin..cStatus WITH IIF(&lcMainLin..cStatus = 'A' , 'A' ,'M')
    *C101944,1 [End]
    
    *-- Update the temp. order lines file with the modified qty.
    SELECT (lcOrderLin)
    *C101944,1 HBG 10/29/2000 Replace the Cut fields with itself blus the 
    *C101944,1                new Qty [Begin] 
    *REPLACE &lcOrderLin..Cut1    WITH &lcOrderLin..Cut1 - laOrd[1] + laEdtQty[1] ;
            &lcOrderLin..Cut2    WITH &lcOrderLin..Cut2 - laOrd[2] + laEdtQty[2] ;
            &lcOrderLin..Cut3    WITH &lcOrderLin..Cut3 - laOrd[3] + laEdtQty[3] ;
            &lcOrderLin..Cut4    WITH &lcOrderLin..Cut4 - laOrd[4] + laEdtQty[4] ;
            &lcOrderLin..Cut5    WITH &lcOrderLin..Cut5 - laOrd[5] + laEdtQty[5] ;
            &lcOrderLin..Cut6    WITH &lcOrderLin..Cut6 - laOrd[6] + laEdtQty[6] ;
            &lcOrderLin..Cut7    WITH &lcOrderLin..Cut7 - laOrd[7] + laEdtQty[7] ;
            &lcOrderLin..Cut8    WITH &lcOrderLin..Cut8 - laOrd[8] + laEdtQty[8] ;
            &lcOrderLin..TotCut  WITH &lcOrderLin..TotCut - laOrd[9] + laEdtQty[9]  
    
    REPLACE &lcOrderLin..Cut1    WITH IIF(llOrder,0,&lcOrderLin..Cut1) + laEdtQty[1] ;
            &lcOrderLin..Cut2    WITH IIF(llOrder,0,&lcOrderLin..Cut2) + laEdtQty[2] ;
            &lcOrderLin..Cut3    WITH IIF(llOrder,0,&lcOrderLin..Cut3) + laEdtQty[3] ;
            &lcOrderLin..Cut4    WITH IIF(llOrder,0,&lcOrderLin..Cut4) + laEdtQty[4] ;
            &lcOrderLin..Cut5    WITH IIF(llOrder,0,&lcOrderLin..Cut5) + laEdtQty[5] ;
            &lcOrderLin..Cut6    WITH IIF(llOrder,0,&lcOrderLin..Cut6) + laEdtQty[6] ;
            &lcOrderLin..Cut7    WITH IIF(llOrder,0,&lcOrderLin..Cut7) + laEdtQty[7] ;
            &lcOrderLin..Cut8    WITH IIF(llOrder,0,&lcOrderLin..Cut8) + laEdtQty[8] ;
            &lcOrderLin..TotCut  WITH IIF(llOrder,0,&lcOrderLin..TotCut) + laEdtQty[9]
    REPLACE &lcOrderLin..cStatus WITH IIF(&lcOrderLin..cStatus = 'A' , 'A' ,'M')
    *C101944,1 [End]
    
  *C101944,1 HBG 10/29/2000 End If user want to allocate all stores[Begin]
  ENDIF
  *C101944,1 [End]
ENDIF

*C101944,1 HBG 10/29/2000 Reset the Allocating for all Multi Stores 
*C101944,1                flag & Order Button flag [Begin]
STORE .F. TO llOrder , llAloMulSt 
*C101944,1 [End]

*=ACOPY(laEdtQty,laOrd)

*-- Select the main file "POSHDR" if P/O allocation or "CUTTKTH" if C/T allocation.
SELECT (lcAlocFile)

*-- Terminate the read of the modify screen.
CLEAR READ

*B101704,1 [Start]
*!**************************************************************************
*! Name      : lfPicture  
*! Developer : Sameh Aldesouki 
*! Date      : 01/17/2000
*! Purpose   : Change Picture of PO.
*!**************************************************************************
FUNCTION lfPicture

IF llGenOrNum
  RETURN "@! XXXXXX" 
ELSE
  RETURN "@! X99999"
ENDIF  
*B101704,1 [End]

*!*************************************************************
*! Name      : lfAloMulSt
*! Developer : HEND GHANEM
*! Date      : 10/31/2000
*! Purpose   : Allocate in case of multi store
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAloMulSt()
*!*************************************************************
**C101944,1
FUNCTION lfAloMulSt

SELECT(lcOrderLin)
lnTotQty = 0

=SEEK('O'+lcOrder)
SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O' + lcOrder;
          FOR Style = lcStyle
  IF TotCut = TotQty
    LOOP
  ENDIF
  FOR lnI = 1 TO 8
    lcI = ALLTRIM(STR(lnI))
    laLinQty[lnI] = IIF(Cut&lcI = 0 , Qty&lcI , Qty&lcI-Cut&lcI)
  ENDFOR 
  laLinQty[9] = TotQty

  STORE 0 TO laStrlnCut
  FOR lnJ = 1 TO 8
    lcJ = ALLTRIM(STR(lnJ))
    IF laEdtQty[lnJ] > laLinQty[lnJ]
      laStrlnCut[lnJ] = laLinQty[lnJ]
      laEdtQty[lnJ] = laEdtQty[lnJ] - laLinQty[lnJ]
    ELSE
      laStrlnCut[lnJ] = laEdtQty[lnJ]
      laEdtQty[lnJ] = 0  
    ENDIF
    laStrlnCut[9] = laStrlnCut[9] + laStrlnCut[lnJ]
  ENDFOR
  REPLACE &lcOrderLin..Cut1    WITH &lcOrderLin..Cut1 + laStrlnCut[1] ;
          &lcOrderLin..Cut2    WITH &lcOrderLin..Cut2 + laStrlnCut[2] ;
          &lcOrderLin..Cut3    WITH &lcOrderLin..Cut3 + laStrlnCut[3] ;
          &lcOrderLin..Cut4    WITH &lcOrderLin..Cut4 + laStrlnCut[4] ;
          &lcOrderLin..Cut5    WITH &lcOrderLin..Cut5 + laStrlnCut[5] ;
          &lcOrderLin..Cut6    WITH &lcOrderLin..Cut6 + laStrlnCut[6] ;
          &lcOrderLin..Cut7    WITH &lcOrderLin..Cut7 + laStrlnCut[7] ;
          &lcOrderLin..Cut8    WITH &lcOrderLin..Cut8 + laStrlnCut[8] ;
          &lcOrderLin..TotCut  WITH &lcOrderLin..TotCut + laStrlnCut[9]
  REPLACE &lcOrderLin..cStatus WITH IIF(&lcOrderLin..cStatus = 'A' , 'A' ,'M')
   
  lcDelSet = SET("DELETE")
  SET DELETE OFF 
     
  SELECT (lcCutPick)
  IF !SEEK(lcKeyCode+lcKeyFld+STR(&lcMainLin..LineNo,6)+&lcOrderLin..Order+&lcOrderLin..Style+STR(&lcOrderLin..LineNo,6),lcCutPick)
    *-- Add new record to the cutpick file if does not exist in the cutpick file.
    STORE 0 TO laQty
    llNew = .T.
    APPEND BLANK
  ELSE
    *-- If the record exist & was deleted before.
    IF DELETED()
      *-- Recall the current record in the temp. cutpick file.
      RECALL
    ENDIF
    *-- Set flag to determine the status of the record.
    llNew = !SEEK(lcKeyCode+lcKeyFld+STR(&lcMainLin..LineNo,6)+&lcOrderLin..Order+&lcOrderLin..Style+STR(&lcOrderLin..LineNo,6),'CUTPICK')
  ENDIF
  REPLACE cTktNo     WITH lcKeyFld  ;
          TranCd     WITH lcKeyCode ;
          cTktLineNo WITH STR(&lcMainLin..LineNo,6) ;
          Order      WITH &lcOrderLin..Order ;
          cOrdLine   WITH STR(&lcOrderLin..LINENO,6) ;
          Style      WITH &lcOrderLin..Style ;
          cStatus    WITH IIF(llNew , 'A' , 'M') ; 
          nRecNo     WITH IIF(llNew , 0 , RECNO("CUTPICK"))
  REPLACE Qty1    WITH Qty1 + laStrlnCut[1] ;
          Qty2    WITH Qty2 + laStrlnCut[2] ;
          Qty3    WITH Qty3 + laStrlnCut[3] ;
          Qty4    WITH Qty4 + laStrlnCut[4] ;
          Qty5    WITH Qty5 + laStrlnCut[5] ;
          Qty6    WITH Qty6 + laStrlnCut[6] ;
          Qty7    WITH Qty7 + laStrlnCut[7] ;
          Qty8    WITH Qty8 + laStrlnCut[8] ;
          TotQty  WITH TotQty + laStrlnCut[9] ;
          cStatus WITH IIF(cStatus = 'A' , 'A' ,'M')

  *E301289,1 WAB -  calculate total generate qty of order line
  *E301289,1 WAB - START

  IF llDispPric
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)      
      lnTotQty = lnTotQty + laStrlnCut[lnI]
    ENDFOR
    lnSelPrice = lnSelPrice  + (lnTotQty * &lcOrderLin..Price )
  ENDIF  
  *E301289,1 WAB - END
   
  SET DELETE &lcDelSet 
   
ENDSCAN

 *-- Update the temp P/O or C/T lines with the modified qty.
SELECT(lcMainLin)
IF llDispPric  
  lnSelPrice = ( lnSelPrice + lnOldQty * nSelPrice ) / (lnTotQty+lnOldQty)
  =SEEK(&lcMainLin..Style,'Style')
  IF lcAllocate = 'P'
    lnTotCost   = &lcMainLin..nEcost1 + &lcMainLin..nEcost2 + &lcMainLin..nEcost3 +;
                  &lcMainLin..nEcost4 + &lcMainLin..nEcost5
  ELSE
    lnTotCost   = STYLE.TOTCOST
  ENDIF
  lnRotSub    = IIF(llStyMark,lnTotCost,lnSelPrice)
  lnGrosMrgn = IIF(lnRotSub=0,0,((lnSElPrice - lnTotCost)/lnRotSub)*100)
  REPLACE &lcMainLin..nSelPrice WITH lnSelPrice,;
          &lcMainLin..nGrosMrgn WITH lnGrosMrgn
ENDIF

REPLACE &lcMainLin..Ord1    WITH &lcMainLin..Ord1 + laEdtQty1[1] ;
        &lcMainLin..Ord2    WITH &lcMainLin..Ord2 + laEdtQty1[2] ;
        &lcMainLin..Ord3    WITH &lcMainLin..Ord3 + laEdtQty1[3] ;
        &lcMainLin..Ord4    WITH &lcMainLin..Ord4 + laEdtQty1[4] ;
        &lcMainLin..Ord5    WITH &lcMainLin..Ord5 + laEdtQty1[5] ;
        &lcMainLin..Ord6    WITH &lcMainLin..Ord6 + laEdtQty1[6] ;
        &lcMainLin..Ord7    WITH &lcMainLin..Ord7 + laEdtQty1[7] ;
        &lcMainLin..Ord8    WITH &lcMainLin..Ord8 + laEdtQty1[8] ;
        &lcMainLin..TotOrd  WITH &lcMainLin..TotOrd + laEdtQty1[9]        
REPLACE &lcMainLin..cStatus WITH IIF(&lcMainLin..cStatus = 'A' , 'A' ,'M')
  

STORE 0 TO laStrQty , laStrCut , laLinQty , laStrlnCut 

*-- End of lfAloMulSt


*!*************************************************************
*! Name      : lfvCancel
*! Developer : HEND GHANEM
*! Date      : 10/31/2000
*! Purpose   : Valid function for cancel button
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCancel()
*!*************************************************************
**C101944,1
FUNCTION lfvCancel

*C101944,1 Reset the Allocating for all Multi Stores flag & Order Button flag [Begin]
STORE .F. TO llOrder , llAloMulSt 
*C101944,1 [End]

STORE 0 TO laStrQty , laStrCut , laLinQty , laStrlnCut 
= ACOPY(laOldLaOrd,laOrd)


*--End of lfvCancel

*!*************************************************************
*! Name      : lfChkMultS
*! Developer : HEND GHANEM
*! Date      : 10/31/2000
*! Purpose   : Check If this is style is in more than store
*!             in case of Multi Store
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfChkMultS()
*!*************************************************************
**C101944,1
FUNCTION lfChkMultS
PRIVATE lcThisOrd , lcThisSty , lcStore , lnNoSamStr 

SELECT(lcOrderLin)

lnRecNo = RECNO()
lcThisOrd = Order
lcThisSty = Style
lcStore = SPACE(8)
lnNoSamStr = 0

=SEEK('O'+lcThisOrd)
SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O' + lcThisOrd;
          FOR Style = lcThisSty

  IF STORE <> lcStore 
    lcStore = STORE
    lnNoSamStr = lnNoSamStr + 1
  ENDIF
ENDSCAN
llMultiStr = IIF(lnNoSamStr > 1, .T.,.F.)

IF BETWEEN(lnRecNo, 1 , RECCOUNT())
  GOTO lnRecNo
ENDIF

*--End of lfChkMultS