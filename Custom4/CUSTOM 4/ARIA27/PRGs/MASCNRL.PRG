*:***************************************************************************
*: Program file  : MASCNRL          C#101768,1
*: Program desc. : Issue rolls by bar code
*: For screen    : MASCNRL.SCX
*:        System : Aria Advantage Series.
*:        Module : Materials (MA)
*:        Date   : 04/23/2000
*:     Developer : Ramy Mabrouk (RAMY)
*:***************************************************************************
*: Parameters    : None
*:***************************************************************************
*: Example : DO MASCNRL -- C101768,1
*:***************************************************************************
*: Modifications :
*: B:803716,1 SSH Dupport the program for dyelot.
*:***************************************************************************

*check if the Setting Keep Track Of Matrerial Rolls = 'Y' to continue proceed 
*with this screen
llTrkRolls = ALLTRIM(gfGetMemVar('M_TrkRolls')) = 'Y'
IF !llTrkRolls
  * Message Text : The Material module is not set to keep track of material rolls. 
  * Message Text : Can not proceed.
  * Message No.  : 36174
  * Buttom  Text : YES  
  * Buttom  No.  : 00000
  =gfModalGen("INM36174B00000" , "DIALOG")
  RETURN
ENDIF


*--- Variables Delaration [start]
DECLARE laSetups[8,2]
rbScanBy   = 2               && Radio button Scan by (Roll , Material)
lcOldValue = ""              && Variable hold old value of any string object.
lnOldValue = 0               && Variable hold old value of any numeric object.
lnMarker   = 0               && Variable hold the record # in the browse.
lnLineCnt  = 0               && Liens counter hold the no. of lines in the browse.
llCollect  = .F.             && Flag to define start processing or not.
llConfirm  = .F.             && If set confirmation is on or off.
ldDate     = gdSysDate       && Variable hold the receiving date.
lcLinStat  = "DISABLE"       && Variable hold if the lines are enabled or disabled.
lcTmpBrow  = "Temporary Receiving Rolls"
llBrowse   = .F.             && Flag to know if press the browse icon or not.
lcCuttkt   = ""              && Variable hold the CT or PO object.
rbMethod   = 2               && Radio button (Import file , Scan line by line)
lcCollect  = IIF(rbMethod = 1 , " \<Collect" , "    S\<tart ")     && Prompt of the collect & Start button.
lcDesc     = ""              && Variable hold the header description.
lcColStat  = "ENABLE"
lcRolStat  = "ENABLE"
llToOpen   = .T.
llUseDye   = .F.
rbByLin    = 2               && Radio button on the line level to define UPC or item.
lcStyDesc  = ""              && Variable hold the style description.
lnQty      = 0               && Variable hold the style qty.
lcCartTl   = 'Rolls'
cbByRoll   = .F.
lcFab      = ''
lcColor    = ''
lnReturn   = 0
llEnaQty   = .T.
lcTBrwBmp = gcBmpHome + "ExtKey.BMP"
lcTNewBmp = gcBmpHome + "New1.BMP"
lcTRemBmp = gcBmpHome + "Rem1.BMP"
lcTProBmp = gcBmpHome + "Proceed.BMP"
lcTEdtBmp = gcBmpHome + "Edit3.BMP"
lcTBrwBmp = gcBmpHome + "ExtKey.BMP"
*: B:803716,1 SSH warehouse variable
lcWareCode = ''
lnIssWare  = 1
lcDyeObj   = ''
*: B:803716,1 SSH warehouse variable[END]

*--- nReturn  = 0 ==> No Issued or apply before 
*--- nReturn  = 1 ==> Issued With out apply
*--- nReturn  = 2 ==> Issued With out apply and issue remaining and apply

*---   IIF(nReturn = 1 , Issue_Qty , Req_Qty - Issue_Qty)
STORE " " TO lcRcAll0 , lcRcAll1   , lcRcAll2 , lcRcAll3  , lcGlYear   , lcGlPeriod   , lcFabric , ;
  lcDyelot , lcGlDTemp , lcTmpIndx , lcStyIndx  , lcRolIndx , lcRcAll0     , lcRcAll1 , ;
  lcRcAll2 , lcRcAll3  , lcTmpRecv , lcTmpCurs  , lcTranType
lcRcvFld = "CUTTKT:H='C/T # ':R , cRollid:R:H='Roll' ,"+;
  "Fabric:R , cFabClr:R:H='Color' , Dyelot:R , lnDummy=Aval:R:H='Available',"+;
  "lcTemp = RecTyp :R:H='Type',lnreq=Req_Qty :R:H='Required',lnIssQty = Issue_Qty :R:H='Qty' :P = '999999.999'"

*--- IIF(lnQty<>0,lnQty,IIF(nReturn = 0 , IIF(lnQty<>0,lnQty,Req_Qty) , IIF(nReturn = 1 , Issue_Qty , Req_Qty - Issue_Qty)))
*--- Variables Delaration [end]
*--- IIF(nReturn = 0 , IIF(lnQty<>0,lnQty,Req_Qty) , IIF(nReturn = 1 , Issue_Qty , Req_Qty - Issue_Qty))

*-- Force to local Save procedure  (lpSavScr)
laDefProc[9]  = .F.
laDefProc[10] = .F.     && close procedure(lpClsScr)
DECLARE laMatWare[1,2]
STORE ' ' TO laMatWare
=gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
SELECT SUBSTR(cDesc,1,20),cWareCode FROM WAREHOUS INTO ARRAY laMatWare
lcWareCode = laMatWare[1,2]
=gfCloseFile('WAREHOUS')
IF !gfSetup()
  RETURN
ENDIF

IF !WEXIST(gcBaseWind)

  laSetups[1,1]  = 'M_LINK_GL'
  laSetups[2,1]  = 'M_DYELOT'
  laSetups[3,1]  = 'M_CMTYPE1'
  laSetups[4,1]  = 'M_CMTYPE2'
  laSetups[5,1]  = 'M_CMTYPE3'
  laSetups[6,1]  = 'M_CMTYPE4'
  laSetups[7,1]  = 'M_CMTYPE5'
  laSetups[8,1]  = 'M_WAREHOUS'
  =gfGetMemVar(@laSetups , gcAct_Comp)
  FOR ln=1 TO ALEN(laSEtups,1)
    IF laSetups[ln,2]=='F'
      lcTranType = SUBSTR(laSetups[ln,1],LEN(laSetups[ln,1]),1)
      EXIT
    ENDIF
  ENDFOR
  llUseDye = IIF(laSetups[2,2] = 'N' , .F. , .T.)
  lcBOMIndx = gfTempName()   && Temp. index to BOMLINE file
  lcGlDTemp = gfTempName()   && General Ledger temporary file
  lcTmpIndx = gfTempName()   && Temp. index to BOMCOST file
  lcStyIndx = gfTempName()   && Temp. index to STYLE file
  lcRolIndx = gfTempName()   && Temp. index to ROLLS file
  lcRcAll0  = gfTempName()   && The windows Temp. name
  lcRcAll1  = gfTempName()   && The windows Temp. name
  lcRcAll2  = gfTempName()   && The windows Temp. name
  lcRcAll3  = gfTempName()   && The windows Temp. name
  lcTmpRecv = gfTempName()   && The working temp. file
  lcTmpRecv1= gfTempName()   
  *: B:803716,1 SSH Dyelot temp filea
  lcDyeTemp = gfTempName()   && Dyelot Temp File.
  *: B:803716,1 SSH warehouse variable
  CREATE TABLE (gcWorkDir+lcTmpRecv) (CUTTKT C(6) , BYROLL L(1) , ;
    cRollid C(20) , Dyelot C(10) , FABRIC C(7)    , cFabClr C(6), ;
    cWareCode C(6) ,cDesc C(20)   , Req_Qty N(10,3) , Issue_Qty N(10,3) ,;
    REFERENCE C(30) , lIssue L(1) , nReturn n(1),Aval N(10,3) , RecTyp C(10))
  INDEX ON cRollid + FABRIC + cFabClr + CUTTKT TAG (lcTmpRecv)
  INDEX ON FABRIC  + cFabClr + CUTTKT TAG (lcTmpRecv1)

  *-- Open the FABDYE file used by the "gfMatCrl"
  =gfOpenFile(gcDataDir + 'FABDYE' , 'FABDYE' , 'SH' , '' , .T.)
  *-- Open the CUTTKTH used in tthe browse
  =gfOpenFile(gcDataDir + 'CUTTKTH' , 'CUTTKTH' , 'SH' , '' , .T.)

  *-- Open the GLDIST file if the system linked to GL
  IF laSetups[1,2]='Y'
    IF !USED('GLDIST')
      =gfOpenFile(gcDataDir + 'GLDIST' , '' , 'SH' , '' , .T.)
    ENDIF
    SELECT GLDIST
    COPY STRUCTURE TO (gcWorkDir+lcGlDTemp)
    =gfOpenFile(gcWorkDir+lcGlDTemp,'','EX')
  ENDIF

  *-- Temp Cursor to be used if collecting from text file
  lcTmpCurs = gfTempName()
  CREATE CURSOR (lcTmpCurs) (ROLL C(20) , lFound L(1))

ENDIF


lcColStat = IIF(laScrMode[1] , "ENABLE" , "DISABLE")

*-- Define bar to activate the browse.
DEFINE BAR 100 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 101 OF P01PU01 PROMPT lcTmpBrow KEY ALT+B
*-- Activate the browse when selecting its bar.
ON SELECTION BAR 101 OF P01PU01 ACTIVATE WINDOW (lcTmpBrow)

*-- Running the Screen
*khm1
*DO (gcScrDir+"MA\MASCNRL.SPX")
DO (gcScrDir+"MASCNRL.SPX")
*khm1

*-- If we have opend the data files close them and delete the temp files
IF !llToOpen
  SELECT ROLLS
  SET ORDER TO
  USE
  ERASE (gcWorkDir + lcRolIndx + '.CDX')

  SELECT BOMCOST
  SET ORDER TO
  USE
  ERASE (gcWorkDir + lcTmpIndx + '.CDX')

  SELECT STYLE
  SET ORDER TO
  USE
  ERASE (gcWorkDir + lcStyIndx + '.CDX')

  SELECT BOMLINE
  SET ORDER TO
  USE
  ERASE (gcWorkDir + lcBOMIndx + '.CDX')

ENDIF


IF USED('CUTTKTH')
  USE IN CUTTKTH
ENDIF

*-- Release the menu bar used to activate the browse
RELEASE BAR 100 OF P01PU01
RELEASE BAR 101 OF P01PU01

*!*************************************************************
*! Name      : lfActMain
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : The main screen activate function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfActMain
*!*************************************************************
FUNCTION lfActMain

*-- If coming from the browse, Call global function to stop the browse.
SHOW GET pbBrws DISABLE
IF glFromBrow
  = gfStopBrow()
  *-- Set the browse flag to false.
  glFromBrow = .F.
ENDIF

*-- If the screen browse is not active then clear the trapped keys.
IF ALLTRIM(WONTOP()) = ALLTRIM(lcTmpBrow)
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
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : The main screen deactivate function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfDactMain
*!*************************************************************
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
*! Developer : Ramy Mabrouk (RAMY)
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
  ACTIVATE WINDOW (lcRcAll3)
  _CUROBJ = OBJNUM(lcRolls)
ENDCASE

*-- Trap the TAB key.
ON KEY LABEL TAB DO lpTab

*!*************************************************************
*! Name      : lpShTab
*! Developer : Ramy Mabrouk (RAMY)
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

PROCEDURE lpShTab


*-- Clear the backtab key.
ON KEY LABEL BACKTAB

DO CASE
CASE laScrMode[1]
  ACTIVATE WINDOW (lcRcAll1)
  _CUROBJ = OBJNUM(pbCollect)
CASE laScrMode[4]
  ACTIVATE WINDOW (lcRcAll3)
*  _CUROBJ = OBJNUM(pbNewCt)
ENDCASE

*-- Trap the BACKTAB key.
ON KEY LABEL BACKTAB DO lpShTab


*!*************************************************************
*! Name      : lfVldBrow
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : Valid function executed inside the browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfVldBrow()
*!*************************************************************
FUNCTION lfVldBrow

*-- Valid function executed inside the browse. to stop the browse if the
*-- current window not the browse window.

IF !WONTOP(lcTmpBrow)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF


*!*************************************************************
*! Name      : lfWhenBrow
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : When function executed inside the browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfWhenBrow()
*!*************************************************************
FUNCTION lfWhenBrow

*-- Refresh the objects in the screen if any movement happened in the browse.
SELECT (lcTmpRecv)
*-- Change the record pointer.
lnMarker   = RECNO()
glFromBrow = .T.

IF BOF() .OR. EOF()
  lcLinStat = "DISABLE"
  SHOW GET pbRemLn   DISABLE
ELSE
  lcCutTkt   = CUTTKT
  lcRolls    = &lcTmpRecv..cRollid
*  IF lnQty = 0
*    lnQty      = IIF(nReturn = 0 , Req_Qty , IIF(nReturn = 1 , Issue_Qty , Req_Qty - Issue_Qty))
*    lnQty      = IIF(EMPTY(cRollid),Req_Qty - Issue_Qty ,lnQty)
*  ENDIF
  *lnQty      = IIF(Issue_Qty<>0,Issue_Qty,lnQty)
  lnQty      = &lcTmpRecv..Issue_Qty
  lcStyDesc  = &lcTmpRecv..cDesc
  lcRefer    = &lcTmpRecv..Reference
  lcFab      = &lcTmpRecv..Fabric + ' ' + &lcTmpRecv..cFabClr
  lcLinStat  = "ENABLE"
  SHOW GET pbRemLn   ENABLE
ENDIF

SHOW GET lcCutTkt
*SHOW GET lcRollNO
SHOW GET ldDate
SHOW GET lcDesc

*-- Refresh the objects in window MfRcAll3.

*IF llCollect
*  SHOW GET pbNewLn   ENABLE
*  SHOW GET rbByLin   ENABLE
*ELSE
*  SHOW GET pbNewLn   DISABLE
*  SHOW GET rbByLin   DISABLE
*ENDIF

*-- Activate the style object to add new line.
IF rbByLin = 2
  SHOW GET lcRolls ENABLE
  _CUROBJ   = OBJNUM(lcRolls)
ELSE
  SHOW GET lcFab ENABLE
  _CUROBJ   = OBJNUM(lcFab)

ENDIF
SHOW GET lcStyDesc &lcLinStat
IF llEnaQty
  SHOW GET lnQty     &lcLinStat
ENDIF
SHOW GET lcRefer   &lcLinStat

*-- Call function in the screen spr to refresh the say fields
*-- in the main screen
=lfRefresh()

SHOW WINDOW (lcTmpBrow) REFRESH SAME

*!*************************************************************
*! Name      : lfvMethod
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : The Select Method radio button valid function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvMethod()
*!*************************************************************
FUNCTION lfvMethod

*-- The button caption
lcCollect = IIF(rbMethod = 1 , "\<Collect" , "   S\<tart ")

*-- If we will scan the roll id bar code
IF rbMethod = 2
  lcImpFile = ""
  SHOW GET pbGetFil  DISABLE
  SHOW GET lcImpFile DISABLE
ELSE  && if we will get from text file
  SHOW GET pbGetFil  ENABLE
  SHOW GET lcImpFile ENABLE
ENDIF

lcColStat = "ENABLE"
SHOW GET pbCollect,1 PROMPT lcCollect &lcColStat

*!*************************************************************
*! Name      : lfvGetFil
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : The Get File button valid function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvGetFil()
*!*************************************************************
FUNCTION lfvGetFil

*-- Get the text file hold the Rolls.
lcImpFile = GETFILE('TXT', 'Select File', 'Select',1)
IF !FILE(lcImpFile)
  =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'File dos not exist. Cannot proceed.')  
  lcImpFile = ''
ENDIF

*-- Refresh the imported file variable.
SHOW GET lcImpFile

*!*************************************************************
*! Name      : lfvScanBy
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : The Scan by radio button valid function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvScanBy()
*!*************************************************************
FUNCTION lfvScanBy

*-- Refresh the related objects on the lines data window.
rbByLin = rbScanBy
IF rbByLin = 1
  SHOW GET lcRolls   DISABLE
  SHOW GET lcFab     ENABLE
  SHOW GET lcFab     DISABLE
  *_CUROBJ = OBJNUM(lcFab)
ELSE
  SHOW GET lcFab   DISABLE
  SHOW GET lcRolls ENABLE
  SHOW GET lcRolls DISABLE
  *_CUROBJ = OBJNUM(lcRolls)
ENDIF

SHOW GET rbByLin
=lfRefresh(lcRcAll3)

*!*************************************************************
*! Name      : lfcOldVal
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : Old value function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfcOldVal()
*!*************************************************************
FUNCTION lfcOldVal

lcOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfnOldVal
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : Old value function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfnOldVal()
*!*************************************************************
FUNCTION lfnOldVal

lnOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvByRoll
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : Check Box by roll valid function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvByRoll()
*!*************************************************************
FUNCTION lfvByRoll


RETURN

*-- Refresh the screen hold the carton objects.
=lfRefresh(lcRcAll1)

IF cbByRoll
  *-- Enable carton object & disable CT .
  *SHOW GET lcRollNo  ENABLE
  SHOW GET lcCuttkt  DISABLE
  SHOW GET ibtkt     DISABLE
  *_CUROBJ = OBJNUM(lcRollNo)
ELSE
  *-- Disable carton object & enable CT or PO.
  *SHOW GET lcRollNo  DISABLE
  SHOW GET lcCuttkt  ENABLE
  SHOW GET ibtkt     ENABLE
  _CUROBJ = OBJNUM(lcCutTkt)
ENDIF

*!*************************************************************
*! Name      : lfvNewCt
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : Command button "New" valid function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvNewCt()
*!*************************************************************
FUNCTION lfvNewCt

llCollect = .F.
lcImpFile = ""
lcRolls  = ""
lcCutTkt  = ""
lcCollect = IIF(rbMethod = 1 , "\<Collect" , "   S\<tart ")

*-- If not start to collect items, enable all the objects in the 1st window.
SHOW GET ibTkt
SHOW GET lcRolls   ENABLE
SHOW GET lcCutTkt  ENABLE
SHOW GET ibPO      ENABLE
SHOW GET lcPO      ENABLE
*SHOW GET pbNewCt   DISABLE
SHOW GET rbMethod  ENABLE
IF rbMethod = 2
  lcImpFile = ""
  SHOW GET pbGetFil  DISABLE
  SHOW GET lcImpFile DISABLE
ELSE
  SHOW GET pbGetFil  ENABLE
  SHOW GET lcImpFile ENABLE
ENDIF

SHOW GET pbOrdBrow ENABLE
SHOW GET rbScanBy  ENABLE
lcColStat = "ENABLE"
SHOW GET pbCollect,1 PROMPT lcCollect &lcColStat


*!*************************************************************
*! Name      : lfwMainBrw
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : The main browse when function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwMainBrw()
*!*************************************************************
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
*! Name      : lpShow
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   :
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpShow()
*!*************************************************************
FUNCTION lpShow

*-- To disable the edit bar in the record pad in the system menu.
laCtrStat[10] = 'DISABLE'
*-- Disable the browse button on the control pannel all the time.
SHOW GET pbBrws DISABLE

*-- Enable the browse invisable button.
SHOW GET ibTab ENABLE

DO CASE
CASE laScrMode[1]

  lcCutTkt = ''
  lnQty    = 0
  ldDate   = gdsysdate
  SHOW GET  pbOrdBrow ENABLE
  SHOW GET  lcCutTkt  ENABLE
  SHOW GET  ldDate    DISABLE
  SHOW GET  pbCollect ENABLE
  SHOW GET  pbVerify  DISABLE
*  SHOW GET  pbNewCt   DISABLE
  SHOW GET  lcRolls   DISABLE
  SHOW GET  lnQty     DISABLE
  SHOW GET  lcRefer   DISABLE
  SHOW GET  rbByLin   DISABLE
  SHOW GET  pbNewLn   DISABLE
  SHOW GET  pbRemLn   DISABLE
  SHOW GET  llLoc     DISABLE
  SHOW GET  rbMethod  ENABLE
  SHOW GET  rbScanBy  ENABLE
  IF !EOF(lcTmpRecv)
    SELECT (lcTmpRecv)
    ZAP
    =lfwMainBrw()
  ENDIF

CASE laScrMode[4]

  *SHOW GET cbByRoll DISABLE
  *-- Activate the style object to add new line.
  *SHOW GET lcRollNo ENABLE
  *_CUROBJ   = OBJNUM(lcRollNo)
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
lcColStat = "ENABLE"
SHOW GET pbCollect,1 PROMPT lcCollect &lcColStat
*SHOW GET pbNewCt  DISABLE

*IF cbByRoll
*-- Enable carton object & disable CT or PO.
*SHOW GET lcRollNo  ENABLE
*  SHOW GET lcCuttkt  DISABLE
*  SHOW GET ibtkt     DISABLE
*ELSE
*-- Disable carton object & enable CT or PO.
*SHOW GET lcRollNo  DISABLE
SHOW GET lcCuttkt  ENABLE
SHOW GET ibtkt     ENABLE
*ENDIF

IF rbMethod = 2
  lcImpFile = ""
  SHOW GET pbGetFil  DISABLE
  SHOW GET lcImpFile DISABLE
ELSE
  SHOW GET pbGetFil  ENABLE
  SHOW GET lcImpFile ENABLE
ENDIF

=lfRefresh()

*!*************************************************************
*! Name      : lfvTkt
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : The cut ticket object valid function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvTkt()
*!*************************************************************
FUNCTION lfvTkt
PRIVATE lcCurAlias

*-- Save the current alias.
lcCurAlias = SELECT(0)

llBrowse = (llBrowse .OR. '?' $ lcCuttkt)
IF llBrowse .OR. !MDOWN()
  SELECT CUTTKTH
  IF llBrowse .OR. (!EMPTY(lcCuttkt) AND !SEEK(lcCuttkt))
    llBrowse = .F.
    =CutBrow(@lcCuttkt,'')
    IF EMPTY(lcCuttkt)
      lcCuttkt = lcOldValue
      SHOW GET lcCuttkt
      _CUROBJ  = OBJNUM(lcCuttkt)
      RETURN
    ENDIF
    lcDesc = CUTTKTH.desc
    *-- Check the validity of the selected CutTkt or PO.
    IF !EMPTY(lcCuttkt)
      IF !lfChkCtkt(lcCuttkt)
        lcCuttkt = SPACE(6)
        lcDesc   = SPACE(6)
        _CUROBJ  = OBJNUM(lcCuttkt)
        SELECT (lcCurAlias)
        RETURN
      ENDIF
    ENDIF
  ENDIF
ENDIF
IF SEEK(lcCuttkt,'CutTktH')  .AND. !(CutTktH.Status $ 'AO')
  =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'Only open or actualize cutting tickets are allowed. Cannot proceed.')
  SELECT (lcCurAlias)
  lcCuttkt = ''
  RETURN
ENDIF
SHOW GET lcDesc
SHOW GET lcCuttkt

SELECT (lcCurAlias)


*!*************************************************************
*! Name      : lfvCollect
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : The push button collect valid function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvTkt()
*!*************************************************************
FUNCTION lfvCollect

*-- If there is no CT entered cananot start.
*IF (!cbByRoll .AND. EMPTY(lcCuttkt)) .OR. (cbByRoll .AND. (EMPTY(lcRollNo) .OR. EMPTY(lcCuttkt)))
IF EMPTY(lcCuttkt)
  *lcTmpTxt = IIF(cbByRoll , "carton/" + "Cutting ticket" , "Cutting ticket") +"|" + ;
  IIF(rbMethod = 1 , "collecting items" , "start scanning")

  lcTmpTxt = "Cutting ticket"+"|" + IIF(rbMethod = 1 , "collecting items" , "start scanning")
  *** You have to enter carton/CT before collecting items. ***
  =gfModalGen("INM34142B00000" , "DIALOG" , lcTmpTxt)
  _CUROBJ = OBJNUM(lcCutTkt)
  RETURN
ELSE
  IF !SEEK(lcCuttkt,'CutTktH')  .OR. !(CutTktH.Status $ 'AO')
    _CUROBJ = OBJNUM(lcCutTkt)
    RETURN
  ENDIF
ENDIF

*-- If import data from text file, & there was no text file selected.
IF rbMethod =1 .AND. EMPTY(lcImpFile)
  *** You have to select the file you will use to import your items. ***
  =gfModalGen("INM34143B00000" , "DIALOG")
  _CUROBJ = OBJNUM(lcImpFile)
  RETURN
ENDIF


IF rbMethod = 1
  SELECT (lcTmpCurs)
  APPEND FROM (lcImpFile) TYPE DELIMITED
  GO TOP
  IF !EOF()
    llCollect = .T.
    IF llToOpen
      =gfOpenFile(gcDataDir + 'ROLLS' , '' , 'SH')
      INDEX ON cROllID + cRollItem + cTktNo +Color+Dyelot+cWareCode TAG lcRolIndx OF (gcWorkDir + lcRolIndx)
      =gfOpenFile(gcDataDir + 'BOMCOST' , gcDataDir + 'BOMCOST' , 'SH')
      INDEX ON cTktNo + Item + IClr TAG lcTmpIndx OF (gcWorkDir + lcTmpIndx)
      =gfOpenFile(gcDataDir + 'FABRIC' , gcDataDir + 'FABRIC' , 'SH')
      =gfOpenFile(gcDataDir + 'CTKTBOM' , gcDataDir + 'CTKTYP' , 'SH')
      =gfOpenFile(gcDataDir + 'STYLE' , '' , 'SH')
      INDEX ON CSTYMAJOR + FABRIC TAG lcStyIndx OF (gcWorkDir + lcStyIndx)
      =gfOpenFile(gcDataDir + 'BOMLINE' , '' , 'SH')
      INDEX ON cIMTyp + cTktNO + Item + IClr + cCatgTyp TAG lcBOMIndx OF (gcWorkDir + lcBOMIndx)
      llToOpen = .F.
    ENDIF
    SELECT (lcTmpCurs)
    SCAN
      llIgnore = .F.
      =lfvRoll(&lcTmpCurs..Roll)
      *-- Open files
    ENDSCAN
    IF llIgnore
      =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'One or more rolls has been ignored ')
      lcRolls = ''
      SHOW GET lcRolls
    ENDIF
  ENDIF
  SELECT (lcTmpCurs)
  ZAP
  *-- Call Function to validate styles.
  =lfWhenBrow()
  IF laScrMode[1]
    laScrMode    = .F.
    laScrMode[4] = .T.
    SHOW GETS
  ENDIF
ELSE
  lnLineCnt  = 0
  llCollect  = .T.
  SHOW GET pbVerify DISABLE
ENDIF


*-- Selecting cut ticket
IF laScrMode[1]
  laScrMode    = .F.
  laScrMode[4] = .T.
  SHOW GETS
ENDIF
*-- If start to collect items, disable all the objects in the 1st window.
IF llCollect
  SHOW GET pbOrdBrow ENABLE
*  SHOW GET pbNewCt   ENABLE
  SHOW GET rbMethod  DISABLE
  SHOW GET pbGetFil  DISABLE
  SHOW GET lcImpFile DISABLE
  SHOW GET rbScanBy  DISABLE
  lcColStat = "DISABLE"
  SHOW GET pbCollect &lcColStat
ENDIF

*-- Refresh the objects in window MfRcAll3.
SHOW GET pbNewLn   ENABLE
*lcLinStat = IIF(llCollect , "ENABLE" , "DISABLE")
lcLinStat = "DISABLE"
SHOW GET lcStyDesc &lcLinStat
SHOW GET rbByLin   &lcLinStat
IF llEnaQty
  SHOW GET lnQty     &lcLinStat
ENDIF
SHOW GET lcRefer   &lcLinStat
SHOW GET pbRemLn   &lcLinStat
SHOW GET lnPrice   &lcLinStat
SHOW GET pbOrdBrow DISABLE
SHOW GET lcCuttkt  DISABLE
_CUROBJ = OBJNUM(pbNewLn)
llcUpDate = .T.
lcWareCode = laMatWare[lnIssWare,2]

*!*************************************************************
*! Name      : lfvNewLn
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : Push button "New Line" valid function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvNewLn()
*!*************************************************************
FUNCTION lfvNewLn

lcRolls = ""
lcFab   = ""
lnQty   = 0
SHOW GET lnQty
*SHOW GET lcRolls ENABLE

IF USED('Rolls')
  SELECT Rolls
  INDEX ON cROllID + cRollItem + cTktNo +Color+Dyelot+cWareCode TAG lcRolIndx OF (gcWorkDir + lcRolIndx)
  SET ORDER TO lcRolIndx
ENDIF
rbByLin = rbScanBy
SHOW GET rbByLin ENABLE
=lfRefresh(lcRcAll3)

IF rbByLin = 2
  SHOW GET lcRolls ENABLE
  _CUROBJ = OBJNUM(lcRolls)
ELSE
  SHOW GET lcFab ENABLE
  _CUROBJ = OBJNUM(lcFab)
ENDIF

*!*************************************************************
*! Name      : lfvRemLn
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : Push button "Remove Line" valid function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRemLn()
*!*************************************************************
FUNCTION lfvRemLn

*** Are you sure you want to "Remove" this record? ***
*** < Yes > - < No > ***
IF gfModalGen("QRM00002B00006","ALERT" , "delete") = 1
  *-- Delete current record in the browse.
  SELECT (lcTmpRecv)
  DELETE
  GO TOP
  *-- Decrease the number of lines with 1 record.
  lnLineCnt = lnLineCnt - 1
  *-- Call the when function to refresh the screen.
  lnQty = 0
  lcRolls=''
  SHOW GET lnQty
  SHOW GET lcRolls
  =lfWhenBrow()
ENDIF

*!*************************************************************
*! Name      : lfChkCtkt
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : To check the cut ticket status
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfChkCtkt()
*!*************************************************************

FUNCTION lfChkCtkt
PARAMETERS lcTrCode
PRIVATE lcTrCode

DO CASE
CASE CUTTKTH.Status = 'H'
  *** Cutting ticket status is Hold , No transaction can be done***
  *** <  Ok > ***
  = gfModalGen('INM34055B00000','DIALOG', "Cutting ticket" + '|' + 'Hold' + '|' + 'Transaction')
  lcCuttkt = lcOldValue
  SHOW GET lcCuttkt
  RETURN .F.
CASE CUTTKTH.Status = 'X'
  *** Cutting ticket status is caneled , No transaction can be done***
  *** <  Ok > ***
  = gfModalGen('INM34055B00000','DIALOG', "Cutting ticket" + '|' + 'Canceled' + '|' + 'Transaction')
  lcCuttkt = lcOldValue
  SHOW GET lcCuttkt
  RETURN .F.
CASE CUTTKTH.Status = 'C'
  *** Cutting ticket status is received , No transaction can be done***
  *** <  Ok > ***
  = gfModalGen('INM34055B00000','DIALOG', "Cutting ticket" + '|' + 'Received' + '|' + 'Transaction')
  lcCuttkt = lcOldValue
  SHOW GET lcCuttkt
  RETURN .F.

CASE CUTTKTH.Status = 'S'
  *** Cutting ticket status is closed , No transaction can be done***
  *** <  Ok > ***
  = gfModalGen('INM34055B00000','DIALOG', "Cutting ticket" + '|' + 'Closed' + '|' + 'Transaction')
  lcCuttkt = lcOldValue
  SHOW GET lcCuttkt
  RETURN .F.

ENDCASE


*!*************************************************************
*! Name      : lfvQty
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : The Qty object valid function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvQty()
*!*************************************************************
FUNCTION lfvQty

*IF lnQty > &lcTmpRecv..Req_Qty
IF lnQty > &lcTmpRecv..Aval
  =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'Issued quantity cannot be greater than the available quantity. Cannot proceed.')
  lnQty = lnOldValue
  =lfWhenBrow()
  SHOW GET lnQty
ENDIF
*IF lnQty > 0
*  *--- crollid+crollitem+color+trancd
*  lcFABRIC = &lcTmpRecv..Fabric
*  lcColor  = &lcTmpRecv..cFabClr
*  lcOldInd = ORDER('Rolls')
*  SET ORDER TO Rolls IN Rolls
*  IF SEEK(PADR(lcRolls,20) + lcFABRIC + lcColor +'1', 'ROLLS');
*     .AND. lnQty > Rolls.nQty
*    =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'Issued quantity cannot be greater than the roll on hand quantity. Cannot proceed.')
*    lnQty = lnOldValue
*  ENDIF
*  IF lnQty <> lnOldValue .AND.  (rbByLin=1 .OR. (SEEK(PADR(lcRolls,20) + lcFABRIC + lcColor, 'ROLLS') .AND. BETWEEN(lnQty , 0 , Rolls.nQtyBal)))
*    LOCATE REST WHILE PADR(lcRolls,20) + lcFabric + lcColor = cRollId + Fabric +cFabClr and trancd = '1'
*    SELECT (lcTmpRecv)
*    REPLACE Issue_Qty WITH lnQty
*    SHOW WINDOW (lcTmpBrow) REFRESH
*  ELSE
*    lnQty = lnOldValue 
*  ENDIF
*  SET ORDER TO lcOldInd IN Rolls
IF lnQty < 0
  WAIT WINDOW "Zero or negative value are not allowed."
  lnQty = lnOldValue
ENDIF
SELECT (lcTmpRecv)
REPLACE Issue_Qty WITH lnQty
=lfWhenBrow()
SHOW GET lnQty

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : Screen Saving Function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr
lcOldValue = ""
SELECT (lcTmpRecv)
GO TOP

IF !EOF()
  ldIssDate  = gdSysDate
  =CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
  lcISession = gfsequence('GLSession')
  lcRSession = gfsequence('GLSession')
  IF laScrMode[4]
    SELECT (lcTmpRecv)
    SCAN
      IF RecTyp = "Apply"
          SELECT ROLLS
          SET ORDER TO TAG lcRolIndx
          *--- cROllID + cRollItem + cTktNo +Color+Dyelot+cWareCode
          lcRollID = &lcTmpRecv..cRollid
          IF SEEK('***** N/A *****     '+lcFabric+&lcTmpRecv..cuttkt+&lcTmpRecv..cFabClr+&lcTmpRecv..Dyelot+&lcTmpRecv..cWareCode)
            APPEND BLANK
            REPLACE cRollItem   WITH &lcTmpRecv..Fabric         ,;
                    COLOR       WITH &lcTmpRecv..cFabClr        ,;
                    Cwarecode   WITH &lcTmpRecv..cWareCode,;
                    Dyelot      WITH &lcTmpRecv..Dyelot         ,;
                    Crollid     WITH lcRollID                   ,;
                    Nqty        WITH Nqty    + &lcTmpRecv..Issue_Qty ,;
                    Nqtybal     WITH (Nqtybal + &lcTmpRecv..Issue_Qty) ,;
                    Trancd      WITH '2'                        ,;
                    Ctktno      WITH &lcTmpRecv..cuttkt         ,;
                    Csession    WITH lcISession                 ,;
                    Crsession   WITH lcRSession                 ,;
                    Cisession   WITH lcISession
            =SEEK('***** N/A *****     '+&lcTmpRecv..Fabric+&lcTmpRecv..cuttkt+&lcTmpRecv..cFabClr+&lcTmpRecv..Dyelot+&lcTmpRecv..cWareCode)
            IF nQtyBal-&lcTmpRecv..Issue_Qty = 0 .OR. nQtyBal-&lcTmpRecv..Issue_Qty < 0
              BLANK
              DELETE
            ELSE
              REPLACE nQtyBal WITH MAX(nQtyBal-&lcTmpRecv..Issue_Qty,0)              
            ENDIF
            SET ORDER TO Rolls
            *--- crollid+crollitem+color+trancd
            =SEEK( PADR(lcRollID,20) + &lcTmpRecv..Fabric + &lcTmpRecv..cFabClr + '1')
            REPLACE Nqtybal WITH MAX(Nqtybal - &lcTmpRecv..Issue_Qty,0)
          ENDIF
      ELSE
        =SEEK(&lcTmpRecv..CUTTKT, 'CUTTKTH')
        IF SEEK(&lcTmpRecv..Fabric + &lcTmpRecv..cFabClr, 'FABRIC')
          lcFabLinkCode   = IIF(EMPTY(Fabric.Link_Code),'DEFDEF',Fabric.Link_Code)
        ENDIF
        IF  laSetups[1,2]<>'Y' 
          DIME laGLDistAr[1,1]
          laGLDistAr = ''
        ELSE
          DECLARE laGLDistAr[2,13]
          laGLDistAr[1,1] = lcFabLinkCode
          laGLDistAr[2,1] = CUTTKTH.LINK_CODE
          laGLDistAr[1,2] = '015'
          laGLDistAr[2,2] = '013'
          laGLDistAr[1,3] = 1
          laGLDistAr[2,3] = -1
          STORE 'MA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
          STORE lcCutTkt   TO laGLDistAr[1,5],laGLDistAr[2,5]
          STORE ldIssDate  TO laGLDistAr[1,6],laGLDistAr[2,6]
          STORE lcGlYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
          STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
          STORE lcGlDTemp  TO laGLDistAr[1,9],laGLDistAr[2,9]
          STORE ''         TO laGLDistAr[1,10],laGLDistAr[2,10]
        ENDIF
        lnTmpCst=lfGetCost( &lcTmpRecv..cRollid , &lcTmpRecv..Fabric , &lcTmpRecv..cFabClr,&lcTmpRecv..cWareCode , &lcTmpRecv..Dyelot)
        =gfMatCrl('4' , &lcTmpRecv..Fabric , &lcTmpRecv..cFabClr , &lcTmpRecv..cWareCode , &lcTmpRecv..Dyelot , ldIssDate , ldIssDate ,;
          CutTkt , -1*&lcTmpRecv..Issue_Qty , lnTmpCst/IIF(Fabric.Conv=0,1,Fabric.Conv) , '' , '' , 0 , '' ,;
          '' , @laGLDistAr , '' , 'M' , CutTkt , SPACE(6) , lcISession,;
            '' , '' , '' )
        IF !lIssue
          SELECT ROLLS
          lcOldInd = ORDER()
          SET ORDER TO TAG Rolapl
          lcRollID = &lcTmpRecv..cRollid
          IF !EMPTY(lcRollID)
            IF !SEEK(lcRSession+lcISession+&lcTmpRecv..Fabric+&lcTmpRecv..cFabClr+&lcTmpRecv..cWareCode+lcDyelot+lcRollID)
              APPEND BLANK
            ENDIF
            REPLACE cRollItem   WITH &lcTmpRecv..Fabric         ,;
                    COLOR       WITH &lcTmpRecv..cFabClr        ,;
                    Cwarecode   WITH &lcTmpRecv..cWareCode,;
                    Dyelot      WITH &lcTmpRecv..Dyelot         ,;
                    Crollid     WITH lcRollID                   ,;
                    Nqty        WITH Nqty    + &lcTmpRecv..Issue_Qty ,;
                    Nqtybal     WITH (Nqtybal + &lcTmpRecv..Issue_Qty) ,;
                    Trancd      WITH '2'                        ,;
                    Ctktno      WITH &lcTmpRecv..cuttkt         ,;
                    Csession    WITH lcISession                 ,;
                    Crsession   WITH lcRSession                 ,;
                    Cisession   WITH lcISession
            SET ORDER TO Rolls
            *--- crollid+crollitem+color+trancd
            =SEEK( PADR(lcRollID,20) + &lcTmpRecv..Fabric + &lcTmpRecv..cFabClr + '1')
            REPLACE Nqtybal WITH MAX(Nqtybal - &lcTmpRecv..Issue_Qty,0)
            SET ORDER TO lcOldInd
          ELSE
             *lcRollID = '***** N/A *****'
          ENDIF
        ENDIF
        IF !USED('CTktBom')
          =gfOpenFile(gcDataDir + 'CTKTBOM' , 'CTKTBOM' , 'SH' , '' , .T.)
        ELSE
          SELECT CTktBom
          SET ORDER TO CTKTBOM
        ENDIF
        IF SEEK('M' + lcCutTkt + lcTranType + PADR(&lcTmpRecv..Fabric , 19) + &lcTmpRecv..cFabClr)
  *-  -- Issue_Qty Instead of Req_Qty
          REPLACE Used_Qty  WITH Used_Qty  + &lcTmpRecv..Issue_Qty   ,;
                  Issue_Qty WITH Issue_Qty + MAX(&lcTmpRecv..Issue_Qty,0)  ,;
                  Dyelot    WITH lcDyelot
        ENDIF
        IF !USED('BomCost')
          =gfOpenFile(gcDataDir + 'BOMCOST' , 'Bomcstkt' , 'SH' , '' , .T.)
        ELSE
          SELECT BomCost
          SET ORDER TO Bomcstkt
        ENDIF
        SELECT BomCost
        SET DELETE OFF
        IF !SEEK( lcTranType + 'M' + lcCutTkt + PADR(&lcTmpRecv..Fabric,19) + &lcTmpRecv..cFabClr)
          APPEND BLANK
        ELSE
          IF DELETED()
            BLANK
            RECALL
          ENDIF
        ENDIF
        SET DELETE ON
        REPLACE cTktNo    WITH lcCutTkt   ,;
                cWareCode WITH &lcTmpRecv..cWareCode ,;
                cDyelot   WITH &lcTmpRecv..Dyelot   ,;
                Item      WITH &lcTmpRecv..Fabric     ,;
                IClr      WITH &lcTmpRecv..cFabClr    ,;
                cBomType  WITH lcTranType ,;
                cIMTyp    WITH 'M'        ,;
                MfgCode   WITH SPACE(6)   ,;
                nTotQty   WITH nTotQty + &lcTmpRecv..Issue_Qty ,;
                nTotCst   WITH nTotCst + &lcTmpRecv..Issue_Qty * lnTmpCst / IIF(Fabric.Conv = 0 , 1 , Fabric.Conv) ,;
                dTranDate WITH ldIssDate  ,;
                cRSession WITH lcRSession ,;
                cISession WITH lcISession ,;
                cCostType WITH ''         ,;
                nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty) ,;
                nUnitACst WITH nUnitCst  ,;
                nTotACst  WITH nTotCst   ,;
                cOprCode  WITH ''        ,;
                cLotNo    WITH ''        ,;
                Actualize WITH 'Y'
        IF nTOtQty = 0
          DELETE
        ENDIF
        IF !EMPTY(lcTranType)
          SELECT CUTTKTH
          =SEEK(&lcTmpRecv..CutTkt)
          =RLOCK()
          REPLACE nAct_Cost&lcTranType WITH nAct_Cost&lcTranType + &lcTmpRecv..Issue_Qty * lnTmpCst / IIF(Fabric.Conv = 0 , 1 , Fabric.Conv)
          UNLOCK
        ENDIF
        *ELSE
        *  lcRollID = &lcTmpRecv..cRollid
        *  IF !EMPTY(lcRollID)
        *    SELECT(lcTmpRecv)
        *    SELECT ROLLS
        *    lcOldInd = ORDER()
        *    SET ORDER TO TAG Rolapl
        *    lcRollID = &lcTmpRecv..cRollid
        *    IF !SEEK(lcRSession+lcISession+&lcTmpRecv..Fabric+&lcTmpRecv..cFabClr+&lcTmpRecv..cWareCode+lcDyelot+lcRollID)
        *      APPEND BLANK
        *    ENDIF
        *    REPLACE cRollItem   WITH lcFabric                   ,;
        *            COLOR       WITH &lcTmpRecv..cFabClr        ,;
        *            Cwarecode   WITH &lcTmpRecv..cWareCode      ,;
        *            Dyelot      WITH lcDyelot                   ,;
        *            Crollid     WITH lcRollID                   ,;
        *            Nqty        WITH Nqty    + &lcTmpRecv..Issue_Qty ,;
        *            Nqtybal     WITH (Nqtybal + &lcTmpRecv..Issue_Qty) ,;
        *            Trancd      WITH '2'                        ,;
        *            Ctktno      WITH &lcTmpRecv..cuttkt         ,;
        *            Csession    WITH lcISession                 ,;
        *            Crsession   WITH lcRSession                 ,;
        *            Cisession   WITH lcISession
        *    SET ORDER TO Rolls
        *    *--- crollid+crollitem+color+trancd
        *    =SEEK( PADR(lcRollID,20) + lcFabric + &lcTmpRecv..cFabClr + '1')
        *    REPLACE Nqtybal WITH MAX(Nqtybal - &lcTmpRecv..Issue_Qty,0)
        *    SET ORDER TO lcOldInd
        *  ENDIF
        *ENDIF
        SELECT(lcTmpRecv)
        DO CASE
          CASE  nReturn = 0
            *SELECT ROLLS
            *INSERT INTO Rolls  (cRollItem          , cRollId             , nQty                , TranCd , CTktNo             , cSession   , cRSession  , cISession) ;
            *            VALUES (&lcTmpRecv..Fabric , &lcTmpRecv..cRollId , &lcTmpRecv..Req_Qty , '2'    , &lcTmpRecv..Cuttkt , lcISession , lcRSession , lcISession)
  
          CASE  nReturn = 1
            *INSERT INTO Rolls  (cRollItem          , cRollId             , nQty                  , TranCd , CTktNo            , cSession   , cRSession  , cISession) ;
            *            VALUES (&lcTmpRecv..Fabric , &lcTmpRecv..cRollId , &lcTmpRecv..Issue_Qty , '2'    , &lcTmpRecv..Cuttkt, lcISession , lcRSession , lcISession)
          
          CASE  nReturn = 2
            *INSERT INTO Rolls  (cRollItem          , cRollId             , nQty                , TranCd , CTktNo            , cSession   , cRSession  , cISession) ;
            *            VALUES (&lcTmpRecv..Fabric , &lcTmpRecv..cRollId , &lcTmpRecv..Req_Qty , '2'    , &lcTmpRecv..Cuttkt, lcISession , lcRSession , lcISession)
        ENDCASE
      ENDIF
    ENDSCAN
  ENDIF
  SELECT (lcTmpRecv)
  ZAP
  =lfwMainBrw()
ELSE
  =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'No records to save.')
  STORE .F. TO llCSave
  RETURN .F.
ENDIF
SHOW GETS
*!*************************************************************
*! Name      : lfvByLin
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : the radio button by line valid
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvByLin()
*!*************************************************************
FUNCTION lfvByLin

IF rbByLin = 1
  SHOW GET lcRolls   DISABLE
  SHOW GET lcFab     ENABLE
  _CUROBJ = OBJNUM(lcFab)
ELSE
  SHOW GET lcFab   DISABLE
  SHOW GET lcRolls ENABLE
  _CUROBJ = OBJNUM(lcRolls)
ENDIF

=lfRefresh(lcRcAll3)


*!*************************************************************
*! Name      : lfvBin
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : Bins text box valid function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvBin()
*!*************************************************************
FUNCTION lfvBin

RETURN

*--------------------------------------------------------
FUNCTION lfvFab
PARAMETER lcTxtFab

IF !EMPTY(lcTxtFab)
  lcFab = lcTxtFab
ENDIF

IF EMPTY(lcFab) OR lcFab == lcOldValue
  *: B:803716,1 SSH Do not empty the file
  *lcFab = ''
  *: B:803716,1 SSH 
  SHOW GET lcFab
  RETURN
ENDIF
*-- Open files
IF llToOpen

  =gfOpenFile(gcDataDir + 'ROLLS' , '' , 'SH')
  INDEX ON cROllID + cRollItem + cTktNo +Color+Dyelot+cWareCode TAG lcRolIndx OF (gcWorkDir + lcRolIndx)

  =gfOpenFile(gcDataDir + 'BOMCOST' , gcDataDir + 'BOMCOST' , 'SH')
  INDEX ON cTktNo + Item + IClr TAG lcTmpIndx OF (gcWorkDir + lcTmpIndx)

  =gfOpenFile(gcDataDir + 'FABRIC' , gcDataDir + 'FABRIC' , 'SH')

  =gfOpenFile(gcDataDir + 'CTKTBOM' , gcDataDir + 'CTKTYP' , 'SH')

  =gfOpenFile(gcDataDir + 'STYLE' , '' , 'SH')
  INDEX ON CSTYMAJOR + FABRIC TAG lcStyIndx OF (gcWorkDir + lcStyIndx)

  =gfOpenFile(gcDataDir + 'BOMLINE' , '' , 'SH')
  INDEX ON cIMTyp + cTktNO + Item + IClr + cCatgTyp TAG lcBOMIndx OF (gcWorkDir + lcBOMIndx)

  llToOpen = .F.
ENDIF

lcFabClr = RIGHT(lcFab , 6)
lcFabric = LEFT(lcFab , 7)
lcColor  = RIGHT(lcFab , 6)
SET ORDER TO lcBOMIndx IN BOMLINE
IF !SEEK('M' + lcCutTkt + PADR(lcFabric , 19) + lcFabClr + 'F' , 'BOMLINE')
  SELECT FABRIC
  DIME laTempData[2]
  STORE '' TO laTempData
  lcTitle= 'Select Roll '
  lcBrFields =  [Fabric :H= 'Fabric',]     +;
    [Color  :H= 'Color' ,]     +;
    [Desc   :H= 'Descreption' ,] +;
    [OnHand :H= 'On Hand']
  = ARIABROW([FOR .T. ],lcTitle,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','' , 'FABRIC,Color' , 'laTempData')
  IF !EMPTY(laTempData)
    lcFabric = laTempData[1]
    lcColor  = laTempData[2]
    lcFabClr = lcColor
    lcFab    = lcFabric + lcColor
  ELSE
    lcFabric = ""
    lcColor  = ""
    lcFab    = ""
    _CUROBJ = OBJNUM(lcFab)
    RETURN
  ENDIF
ENDIF

IF !SEEK('M' + lcCutTkt + PADR(lcFabric , 19) + lcFabClr + 'F' , 'BOMLINE')
  =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'This selected fabric / color does not exist in the selected cutting ticket. Cannot proceed.')  
  lcFabric = ""
  lcColor  = ""
  lcFab    = ""
  _CUROBJ = OBJNUM(lcFab)
  RETURN
ENDIF

*: B:803716,1 SSH Get the material dyelot
IF llUseDye .AND. SEEK(lcFabric + lcColor,'Fabric') .AND. Fabric.cDye_Flg='Y'
  lcDyeObj = BOMLINE.Dyelot
ENDIF
*: B:803716,1 SSH Do not empty the file

*--- Start
lcOldAls = SELECT(0)
SELECT (lcTmpRecv)
SET ORDER TO TAG (lcTmpRecv1)
IF SEEK(lcFabric +lcColor +lcCuttkt)
  =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'This fabric/color is scanned from before on this session. Cannot proceed.')
  =lfWhenBrow()
  lcFab    = ''
  lnReturn = 0
  SHOW GET lcFab
  _CUROBJ = OBJNUM(pbNewLn)
  SHOW GET pbNewLn  ENABLE
  SHOW GET lcFab  DISABLE
  RETURN
ENDIF
SET ORDER TO TAG (lcTmpRecv)
SELECT(lcOldAls)
*--- End
lnMAvail = 0
IF SEEK('M' + lcCutTkt + PADR(lcFabric , 19) + lcColor , 'CTKTBOM')
  *: B:803716,1 SSH Commented out
  *IF CTKTBOM.Req_Qty <>  CTKTBOM.Used_Qty
  *: B:803716,1 SSH 
    lnMAvail = lfGetAval('F')
    IF CTKTBOM.Req_Qty >  CTKTBOM.Used_Qty
      lnToIss = MIN(CTKTBOM.Req_Qty - CTKTBOM.Used_Qty,lnMAvail)
    ELSE
      lnToIss = MIN(0,lnMAvail)
    ENDIF
    *INSERT INTO (lcTmpRecv) (CUTTKT   , cDesc        , Req_Qty         , Issue_Qty         , Fabric   , cFabClr  , cWareCode        , lIssue) ;
      VALUES           (lcCutTkt , CUTTKTH.desc , CTKTBOM.Req_Qty , CTKTBOM.Req_Qty - CTKTBOM.Used_Qty , lcFabric , lcColor , CUTTKTH.cMatWare , .T.)
    SELECT (lcTmpRecv)
    APPEND BLANK
    REPLACE CUTTKT    WITH lcCutTkt,;
            cDesc     WITH CUTTKTH.desc ,;
            Req_Qty   WITH CTKTBOM.Req_Qty,; 
            Issue_Qty WITH lnToIss,;
            Fabric    WITH lcFabric,;
            cFabClr   WITH lcColor,;
            cWareCode WITH lcWareCode,;
            Aval      WITH lnMAvail,;
            lIssue    WITH .T.,;
            Dyelot    WITH lcDyeObj

    *lnQty = CTKTBOM.Req_Qty - CTKTBOM.Used_Qty
    lnQty = lnToIss
    SHOW GET lnQty 
    llcUpDate = .T.
  *ELSE
  *  =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'This roll is already issued on the current cutting ticket. Cannot proceed.')
  *ENDIF
ENDIF

=lfWhenBrow()
SHOW GET lcFab
_CUROBJ = OBJNUM(pbNewLn)
SHOW GET pbNewLn  ENABLE
SHOW GET lcFab  DISABLE

*!*************************************************************
*! Name      : lfvRoll
*! Developer : Ramy Mabrouk (RAMY)
*! Date      : 04/23/2000
*! Purpose   : Valid function for the roll id
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRoll()
*!*************************************************************
*: B:803716,1 SSH Rrewrite this funciton
*FUNCTION lfvRoll
FUNCTION olfvRoll
PARAMETER lcImpRoll

lcRolls   = IIF(TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll),lcImpRoll,lcRolls)
IF EMPTY(lcRolls) OR lcRolls == lcOldValue
  SHOW GET lcRolls
  RETURN
ENDIF
llFound = .F.
llEnaQty   = .T.
*-- Open files
IF llToOpen

  =gfOpenFile(gcDataDir + 'ROLLS' , '' , 'SH')
  INDEX ON cROllID + cRollItem + cTktNo +Color+Dyelot+cWareCode TAG lcRolIndx OF (gcWorkDir + lcRolIndx)

  =gfOpenFile(gcDataDir + 'BOMCOST' , gcDataDir + 'BOMCOST' , 'SH')
  INDEX ON cTktNo + Item + IClr TAG lcTmpIndx OF (gcWorkDir + lcTmpIndx)

  =gfOpenFile(gcDataDir + 'FABRIC' , gcDataDir + 'FABRIC' , 'SH')

  =gfOpenFile(gcDataDir + 'CTKTBOM' , gcDataDir + 'CTKTYP' , 'SH')

  =gfOpenFile(gcDataDir + 'STYLE' , '' , 'SH')
  INDEX ON CSTYMAJOR + FABRIC TAG lcStyIndx OF (gcWorkDir + lcStyIndx)

  =gfOpenFile(gcDataDir + 'BOMLINE' , '' , 'SH')
  INDEX ON cIMTyp + cTktNO + Item +iClr +cCatgTyp TAG lcBOMIndx OF (gcWorkDir + lcBOMIndx)


  llToOpen = .F.
ENDIF
IF !SEEK(lcRolls , 'ROLLS')
  IF TYPE('lcImpRoll')='C'
    llIgnore = .T.
    RETURN
  ENDIF
  SELECT ROLLS
  lcTitle= 'Select Roll '
  DIME laTempData[3]
  STORE '' TO laTempData
  lcBrFields =  [cRollId :H= 'Roll ID',] +;
  IIF(llUseDye  AND FABRIC.cDye_Flg = 'Y', [Dyelot :H='Dyelot',]  ,[])  +;
  [NQtyBal :H= 'On Hand' , ] + ;
  [cRollItem :H= 'Fabric' , ] +;
  [Color :H= 'Color']
  = ARIABROW([FOR TRANCD = "1"],;
  lcTitle,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,.F.,'' , 'cRollId,cRollItem,Color' , 'laTempData')
  IF !EMPTY(laTempData)
    lcFabric = Rolls.cRollItem
    lcColor  = Rolls.Color
    llCollect  = .F.
    llFound = .F.
    IF SEEK('M' + lcCutTkt + PADR(Rolls.cRollItem , 19) , 'BOMLINE')
      lcFabric = Rolls.cRollItem
      lcColor  = Rolls.Color
      llFound = .T.
    ENDIF
    lcRolls  = laTempData[1]
    lcFabric = laTempData[2]
    lcColor  = laTempData[3]
  ELSE
    lcRolls  = ""
    lcFabric = ""
    lcColor  = ""
    _CUROBJ = OBJNUM(lcRolls)
    RETURN
  ENDIF
ELSE
  lcFabric = Rolls.cRollItem
  lcColor  = Rolls.Color
  llCollect  = .F.
  llFound = .F.
  IF SEEK('M' + lcCutTkt + PADR(Rolls.cRollItem , 19) , 'BOMLINE')
    lcFabric = Rolls.cRollItem
    lcColor  = Rolls.Color
    llFound = .T.
  ELSE
    llFound = .F.
  ENDIF
ENDIF

IF !llFound
  IF (TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll))
    llIgnore = .T.
  ELSE
    =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'This roll fabric / color does not exist in the selected cutting ticket. Cannot proceed.')  
  ENDIF
  lcRolls = ""
  _CUROBJ = OBJNUM(lcRolls)
  RETURN
ENDIF


IF SEEK(PADR(lcRolls , 20) + lcFabric +lcColor +lcCuttkt, lcTmpRecv)
  IF TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll)
    llIgnore = .T.
  ELSE
    =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'This roll is scanned from before on this session. Cannot proceed.')
  ENDIF
  =lfWhenBrow()
  lcRolls  = ''
  lnReturn = 0
  SHOW GET lcRolls
  _CUROBJ = OBJNUM(pbNewLn)
  SHOW GET pbNewLn  ENABLE
  SHOW GET lcRolls  DISABLE
  RETURN
ENDIF


*: B:803716,1 SSH Commented out
*lcOldAls = SELECT(0)
*SELECT (lcTmpRecv)
*SET ORDER TO TAG (lcTmpRecv1)
*IF SEEK(lcFabric +lcColor +lcCuttkt)
*  IF TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll)
*    llIgnore = .T.
*  ELSE
*    =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'This fabric/color is scanned from before on this session. Cannot proceed.')
*  ENDIF
*  =lfWhenBrow()
*  lcFab    = ''
*  lnReturn = 0
*  SHOW GET lcFab
*  _CUROBJ = OBJNUM(pbNewLn)
*  SHOW GET pbNewLn  ENABLE
*  SHOW GET lcFab  DISABLE
*  RETURN
*ENDIF
*SET ORDER TO TAG (lcTmpRecv)
*SELECT(lcOldAls)
*: B:803716,1 SSH Commented out[END]


SELECT ROLLS
*: B:803716,1 SSH Commented out
*lnRecNo = RECNO()
*lnApplied = 0
*GO TOP
*SCAN FOR cRollItem + Color + cTktNo = lcFabric + lcColor +lcCutTkt AND TranCd = '2'
*  lnApplied = lnApplied + nQty
*ENDSCAN

*IF USED(lcTmpRecv)
*  *--- INDEX ON FABRIC  + cFabClr + CUTTKT TAG (lcTmpRecv1)
*  SELECT (lcTmpRecv)
*  SET ORDER TO TAG (lcTmpRecv1)
*  IF SEEK( lcFabric + lcColor + lcCutTkt)
*    SCAN REST WHILE FABRIC  + cFabClr + CUTTKT=;
*                    lcFabric + lcColor + lcCutTkt
*      lnApplied = lnApplied + Issue_Qty
*    ENDSCAN
*  ENDIF
*ENDIF
*: B:803716,1 SSH Commented out[END]
IF SEEK(lcRolls + lcFabric + lcCutTkt , 'ROLLS')
  SELECT ROLLS
  LOCATE REST WHILE cROllID + cRollItem + cTktNo =  lcRolls + lcFabric + lcCutTkt AND TranCd = '2';
                    FOR Color = lcColor
  IF FOUND()
    SET ORDER TO CTKTYP IN CTKTBOM
    IF SEEK('M' + lcCutTkt + PADR(lcFabric,19) + lcColor , 'CTKTBOM')
      SELECT CTKTBOM
      IF Req_Qty = CTKTBOM.Used_Qty .OR. Used_Qty > Req_Qty
        IF TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll)
          llIgnore = .T.
        ELSE
          =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'This roll is already issued on the current cutting ticket. Cannot proceed.')
        ENDIF
        lnReturn = 0
        =lfWhenBrow()
        lcRolls = ''
        SHOW GET lcRolls
        _CUROBJ = OBJNUM(pbNewLn)
        SHOW GET pbNewLn  ENABLE
        SHOW GET lcRolls  DISABLE
        RETURN
  *: B:803716,1 SSH Commented out
      *ELSE
        *IF !EOF('Rolls')
        *  IF !(TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll)) .AND. gfModalGen('TRM00000B00006',.F.,.F.,.F.,'Current Fabric / color is partially applied. Do you wish to apply the rest of issued quantity?') = 2
        *    RETURN
        *  ELSE
        *    IF CTKTBOM.Req_Qty - CTKTBOM.Used_Qty = 0
        *      IF TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll)
        *        llIgnore = .T.
        *      ELSE
        *        =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No issued quantity found to apply. Cannot proceed.')
        *      ENDIF      
        *      RETURN
        *   ELSE  
        *      lnQty = CTKTBOM.Req_Qty - CTKTBOM.Used_Qty
        *      llEnaQty   = .F.
        *      lnReturn = 3
        *    ENDIF  
        *  ENDIF
        *ELSE
        *  lnReturn = 2
        *ENDIF
    *: B:803716,1 SSH Commented out
      ENDIF
    ENDIF
  ENDIF
ELSE
  SET ORDER TO CTKTYP IN CTKTBOM
  IF SEEK('M' + lcCutTkt + PADR(lcFabric,19) + lcColor , 'CTKTBOM')
    SELECT CTKTBOM
    IF rbByLin = 1 .AND. Req_Qty = CTKTBOM.Used_Qty .OR. Used_Qty > Req_Qty
      IF TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll)
        llIgnore = .T.
      ELSE
        =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'This roll is already issued on the current cutting ticket. Cannot proceed.')
      ENDIF
      lnReturn = 0
      =lfWhenBrow()
      lcRolls = ''
      SHOW GET lcRolls
      _CUROBJ = OBJNUM(pbNewLn)
      SHOW GET pbNewLn  ENABLE
      SHOW GET lcRolls  DISABLE
      RETURN
    ELSE
      IF lnApplied > 0
        IF !(TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll)) .AND. gfModalGen('TRM00000B00006',.F.,.F.,.F.,'Current Fabric / color is partially applied. Do you wish to apply the rest of issued quantity?') = 2
          RETURN
        ELSE
          IF cTktBom.Req_Qty - CTKTBOM.Used_Qty = 0
            IF TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll)
              llIgnore = .T.
            ELSE
              =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No issued quantity found to apply. Cannot proceed.')
            ENDIF
            RETURN
          ELSE  
            lnQty = cTktBom.Req_Qty - CTKTBOM.Used_Qty
            lnReturn = 3
            llEnaQty   = .F.
          ENDIF  
        ENDIF
      ELSE
        IF CTKTBOM.Used_Qty = CTKTBOM.Req_Qty .OR. CTKTBOM.Req_Qty > CTKTBOM.Used_Qty .AND. CTKTBOM.Used_Qty<>0
          IF TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll)
            llIgnore = .T.
            lnReturn = 1
          ELSE
            IF CTKTBOM.Used_Qty = CTKTBOM.Req_Qty
              lnReturn = 1
            ELSE
              lnReturn = gfModalGen('TRM00000B36015',.F.,.F.,.F.,'This roll is partially issued, Do you want to apply the roll on the issued quantity or issue the rest of the quantity and apply all the quantity?')
            ENDIF           
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF

DO CASE

  CASE lnReturn = 0
    SET ORDER TO CTKTYP IN CTKTBOM
    IF SEEK('M' + lcCutTkt + PADR(lcFabric,19) + lcColor , 'CTKTBOM')
      lnToIss = IIF(CTKTBOM.Issue_Qty<>0,CTKTBOM.Req_Qty-CTKTBOM.Used_Qty,CTKTBOM.Req_Qty)
      INSERT INTO (lcTmpRecv) (CUTTKT   , cDesc        , Req_Qty         ,  Issue_Qty         , cRollid , Fabric   , cFabClr , cWareCode        , lIssue , nReturn) ;
             VALUES           (lcCutTkt , CUTTKTH.desc , CTKTBOM.Req_Qty ,  lnToIss , lcRolls , lcFabric , lcColor , CUTTKTH.cMatWare , .T.    , 0      )
    ENDIF

  CASE lnReturn = 1
    INSERT INTO (lcTmpRecv) (CUTTKT   , cDesc        , Req_Qty         ,  Issue_Qty         , cRollid , Fabric   , cFabClr , cWareCode        , lIssue , nReturn) ;
           VALUES           (lcCutTkt , CUTTKTH.desc , CTKTBOM.Req_Qty ,  CTKTBOM.Used_Qty , lcRolls , lcFabric , lcColor , CUTTKTH.cMatWare , .F.    , 1      )
    llcUpDate = .T.

  CASE lnReturn = 2
    lnToIss = IIF(CTKTBOM.Issue_Qty<>0,CTKTBOM.Req_Qty-CTKTBOM.Used_Qty,CTKTBOM.Req_Qty)
    INSERT INTO (lcTmpRecv) (CUTTKT   , cDesc        , Req_Qty         ,  Issue_Qty         , cRollid , Fabric   , cFabClr , cWareCode        , lIssue , nReturn) ;
           VALUES           (lcCutTkt , CUTTKTH.desc , CTKTBOM.Req_Qty , lnToIss , lcRolls , lcFabric , lcColor , CUTTKTH.cMatWare , .T.    , 2      )
    llcUpDate = .T.
  
  CASE lnReturn = 3
    INSERT INTO (lcTmpRecv) (CUTTKT   , cDesc        , Req_Qty         ,  Issue_Qty         , cRollid , Fabric   , cFabClr , cWareCode        , lIssue , nReturn) ;
           VALUES           (lcCutTkt , CUTTKTH.desc , CTKTBOM.Req_Qty , lnQty , lcRolls , lcFabric , lcColor , CUTTKTH.cMatWare , .T.    , 2      )
    llcUpDate = .T.
ENDCASE


=lfWhenBrow()
lnReturn = 0
SHOW GET lcRolls
_CUROBJ = OBJNUM(pbNewLn)
SHOW GET pbNewLn  ENABLE
SHOW GET lcRolls  DISABLE
IF llEnaQty
  SHOW GET lnQty ENABLE
ELSE
  SHOW GET lnQty DISABLE
ENDIF

*: B:803716,1 SSH valid dyelot funciton
FUNCTION lfvDyelot
*: B:803716,1 SSH Commented out
PRIVATE lnOldAls
lnOldAls = SELECT(0)
DIME laTempData[2]
STORE '' TO laTempData
lcTitle= 'Select Dyelot'
lcBrFields =  [Fabric :H= 'Fabric',]     +;
[Color  :H= 'Color' ,]     +;
[Dyelot   :H= 'Dyelot']
SELECT(lcDyeTemp)
INDEX ON Dyelot+Fabric+Color TAG (lcDyeTemp)
GO TOP
= ARIABROW([FOR .T. ],lcTitle,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','' , 'Dyelot' , 'laTempData')
IF !EMPTY(laTempData)
  lcDyeObj = laTempData
  SHOW GET lcDyeObj ENABLE
ENDIF
SELECT(lnOldAls)
*: B:803716,1 SSH valid dyelot funciton

*: B:803716,1 SSH Get the quantity to issue or apply
FUNCTION lfGetAval
PARAMETER lcTyp
PRIVATE lnOldAls , lnAvail
lnOldAls = SELECT(0)

IF lcTyp='F'
  lnAvail = 0 
  SELECT FabDye
  SET ORDER TO Fabdye
  *--- fabric+color+cwarecode+dyelot
  IF SEEK(lcFabric + lcColor + lcWareCode + lcDyeObj)
    lnAvail = FabDye.OnHand
  ENDIF
  SELECT (lcTmpRecv)
  SET ORDER TO TAG (lcTmpRecv1)
  IF SEEK(lcFabric +lcColor +lcCuttkt)
    SCAN REST WHILE FABRIC  + cFabClr + CUTTKT=;
                    lcFabric +lcColor +lcCuttkt;
              FOR Dyelot = lcDyeObj
      lnAvail = MIN(lnAvail-Issue_Qty,0)
    ENDSCAN
  ENDIF
  
ELSE
  SELECT ROLLS
  LOCATE REST WHILE cROllID + cRollItem + cTktNo =  lcRolls + lcFabric AND TranCd = '1';
                    FOR Color = lcColor .AND. Dyelot = lcDyeObj
  lnAvail = nQtyBal
  SELECT (lcTmpRecv)
  SET ORDER TO TAG (lcTmpRecv1)
  IF SEEK(lcFabric +lcColor +lcCuttkt)
    SUM REST Issue_Qty WHILE FABRIC  + cFabClr + CUTTKT=; 
                    lcFabric +lcColor +lcCuttkt FOR cRollID = lcRolls TO lnTotRollIs 
    lnAvail = lnAvail - lnTotRollIs  
  ENDIF
ENDIF
SELECT(lnOldAls)
RETURN (lnAvail)
*: B:803716,1 SSH Get the quantity to issue or apply[END]

*: B:803716,1 SSH Rrewrite this funciton
FUNCTION lfvRoll
PARAMETER lcImpRoll

lcRolls   = IIF(TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll),lcImpRoll,lcRolls)
IF EMPTY(lcRolls) OR lcRolls == lcOldValue
  SHOW GET lcRolls
  RETURN
ENDIF
llFound = .F.
llEnaQty   = .T.
*-- Open files
IF llToOpen

  =gfOpenFile(gcDataDir + 'ROLLS' , '' , 'SH')
  INDEX ON cROllID + cRollItem + cTktNo +Color+Dyelot+cWareCode TAG lcRolIndx OF (gcWorkDir + lcRolIndx)

  =gfOpenFile(gcDataDir + 'BOMCOST' , gcDataDir + 'BOMCOST' , 'SH')
  INDEX ON cTktNo + Item + IClr TAG lcTmpIndx OF (gcWorkDir + lcTmpIndx)

  =gfOpenFile(gcDataDir + 'FABRIC' , gcDataDir + 'FABRIC' , 'SH')

  =gfOpenFile(gcDataDir + 'CTKTBOM' , gcDataDir + 'CTKTYP' , 'SH')

  =gfOpenFile(gcDataDir + 'STYLE' , '' , 'SH')
  INDEX ON CSTYMAJOR + FABRIC TAG lcStyIndx OF (gcWorkDir + lcStyIndx)

  =gfOpenFile(gcDataDir + 'BOMLINE' , '' , 'SH')
  INDEX ON cIMTyp + cTktNO + Item +iClr +cCatgTyp TAG lcBOMIndx OF (gcWorkDir + lcBOMIndx)


  llToOpen = .F.
ENDIF
IF !SEEK(lcRolls , 'ROLLS') .OR. ROLLS.nQtyBal=0
  IF TYPE('lcImpRoll')='C'
    llIgnore = .T.
    RETURN
  ENDIF
  SELECT ROLLS
  lcTitle= 'Select Roll '
  DIME laTempData[4]
  STORE '' TO laTempData
  lcBrFields =  [cRollId :H= 'Roll ID',] +;
  IIF(llUseDye  AND FABRIC.cDye_Flg = 'Y', [Dyelot :H='Dyelot',]  ,[])  +;
  [NQtyBal :H= 'On Hand' , ] + ;
  [cRollItem :H= 'Fabric' , ] +;
  [Color :H= 'Color']
  LOCATE FOR cWareCode = lcWareCode .AND. TRANCD = "1" .AND. ROLLS.nQtyBal<>0 .AND. SEEK('M' + lcCutTkt + PADR(ROLLS.cRollItem,19) + ROLLS.Color , 'CTKTBOM')
  IF FOUND()
    GO TOP
    = ARIABROW([FOR cWareCode = lcWareCode .AND. TRANCD = "1" .AND. ROLLS.nQtyBal<>0 .AND. SEEK('M' + lcCutTkt + PADR(ROLLS.cRollItem,19) + ROLLS.Color , 'CTKTBOM')],;
    lcTitle,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,.F.,'' , 'cRollId,cRollItem,Color,Dyelot' , 'laTempData')
  ELSE
    =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'No records to browse.')
  ENDIF
  IF !EMPTY(laTempData)
    lcFabric = Rolls.cRollItem
    lcColor  = Rolls.Color
    lcDyeObj = Rolls.Dyelot
    llCollect  = .F.
    llFound = .F.
    IF SEEK('M' + lcCutTkt + PADR(Rolls.cRollItem , 19) , 'BOMLINE')
      lcFabric = Rolls.cRollItem
      lcColor  = Rolls.Color
      lcDyeObj = Rolls.Dyelot
      llFound = .T.
    ENDIF
    lcRolls  = laTempData[1]
    lcFabric = laTempData[2]
    lcColor  = laTempData[3]
    lcDyeObj = laTempData[4]
  ELSE
    lcRolls  = ""
    lcFabric = ""
    lcColor  = ""
    lcDyeObj = ""
    _CUROBJ = OBJNUM(lcRolls)
    RETURN
  ENDIF
ELSE
  SELECT ROLLS
  LOCATE REST WHILE cROllID + cRollItem + cTktNo=;
                    lcRolls;
              FOR cWareCode = lcWareCode
  IF FOUND()
    lcFabric = Rolls.cRollItem
    lcColor  = Rolls.Color
    lcDyeObj = Rolls.Dyelot
    llCollect  = .F.
    llFound = .F.
    IF SEEK('M' + lcCutTkt + PADR(Rolls.cRollItem , 19) , 'BOMLINE')
      lcFabric = Rolls.cRollItem
      lcColor  = Rolls.Color
      lcDyeObj = Rolls.Dyelot
      llFound = .T.
    ELSE
      llFound = .F.
    ENDIF
  ELSE
    =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'This roll ID is not assigned to the selected loction. Cannot proceed.')
    lcRolls   = ''
    SHOW GET lcRolls 
  ENDIF
ENDIF

IF !llFound
  IF (TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll))
    llIgnore = .T.
  ELSE
    =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'This roll fabric / color does not exist in the selected cutting ticket. Cannot proceed.')  
  ENDIF
  lcRolls = ""
  _CUROBJ = OBJNUM(lcRolls)
  RETURN
ENDIF
SELECT(lcTmpRecv)
SET ORDER TO (lcTmpRecv)
IF SEEK(PADR(lcRolls , 20) + lcFabric +lcColor +lcCuttkt, lcTmpRecv)
  IF TYPE('lcImpRoll')='C' .AND. !EMPTY(lcImpRoll)
    llIgnore = .T.
  ELSE
    =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'This roll is scanned from before on this session. Cannot proceed.')
  ENDIF
  =lfWhenBrow()
  lcRolls  = ''
  lnReturn = 0
  SHOW GET lcRolls
  _CUROBJ = OBJNUM(pbNewLn)
  SHOW GET pbNewLn  ENABLE
  SHOW GET lcRolls  DISABLE
  RETURN
ENDIF
*--- Start Get the Available.
SELECT ROLLS
lnToIss = 0
lnCAvail= 0
SET ORDER TO lcRolIndx
*--- cROllID + cRollItem + cTktNo +Color+Dyelot+cWareCode

IF SEEK( "***** N/A *****     " + lcFabric+ lcCutTkt +lcColor+lcDyeObj +lcWareCode) .AND.  (lfCurrAppl() < Rolls.nQtyBal)
  IF SEEK('M' + lcCutTkt + PADR(lcFabric,19) + lcColor , 'CTKTBOM')
    SELECT(lcTmpRecv)
    IF Rolls.nQty = 0
      =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'Cannot apply zero quantity .')
    ELSE
      lnToApply = Rolls.nQtyBal
      lnCurrApply = lfCurrAppl()
      SELECT ROLLS
      =SEEK( lcRolls + lcFabric+ SPACE(06) +lcColor+lcDyeObj +lcWareCode)
      lnAvailable = Rolls.nQtyBal
      =SEEK( "***** N/A *****     " + lcFabric+ lcCutTkt +lcColor+lcDyeObj +lcWareCode)
      lnDfQty2Ap  = MIN(lnToApply - lnCurrApply, lnAvailable)
      IF lnDfQty2Ap > 0
        SELECT(lcTmpRecv)
        APPEND BLANK
        REPLACE CUTTKT    WITH lcCutTkt,;
                cDesc     WITH CUTTKTH.desc ,;
                Req_Qty   WITH CTKTBOM.Req_Qty,; 
                Issue_Qty WITH lnDfQty2Ap,;
                Fabric    WITH lcFabric,;
                cFabClr   WITH lcColor,;
                cWareCode WITH lcWareCode,;
                Aval      WITH lnAvailable ,;
                lIssue    WITH .F.,;
                Dyelot    WITH lcDyeObj,;
                cRollid   WITH lcRolls,;
                RecTyp    WITH "Apply"
      ELSE
        =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'No available quantity to apply.')
      ENDIF
    ENDIF
  ENDIF
ELSE
  IF SEEK(lcRolls + lcFabric , 'ROLLS')
    lnCAvail = lfGetAval('R')
    lnCAvail = IIF(lnCAvail <0,0,lnCAvail)
    SET ORDER TO CTKTYP IN CTKTBOM
    IF SEEK('M' + lcCutTkt + PADR(lcFabric,19) + lcColor , 'CTKTBOM')
      SELECT CTKTBOM
      IF CTKTBOM.Req_Qty > CTKTBOM.Used_Qty
        lnToIss = MIN((CTKTBOM.Req_Qty - CTKTBOM.Used_Qty) ,lnCAvail)
      ELSE
        lnToIss = MAX(0 ,lnCAvail)
      ENDIF
      SELECT(lcTmpRecv)
      IF lnToIss = 0
        =gfModalGen('TRM00000B36000',.F.,.F.,.F.,'Cannot issue zero quantity .')
      ELSE
        APPEND BLANK
        REPLACE CUTTKT    WITH lcCutTkt,;
                cDesc     WITH CUTTKTH.desc ,;
                Req_Qty   WITH CTKTBOM.Req_Qty,; 
                Issue_Qty WITH lnToIss,;
                Fabric    WITH lcFabric,;
                cFabClr   WITH lcColor,;
                cWareCode WITH lcWareCode,;
                Aval      WITH lnCAvail,;
                lIssue    WITH .F.,;
                Dyelot    WITH lcDyeObj,;
                cRollid   WITH lcRolls,;
                RecTyp    WITH "Issue_Apply"
      ENDIF
    ENDIF
  ENDIF
ENDIF

=lfWhenBrow()
*--- KOKO

FUNCTION lpClsScr

SELECT(lcTmpRecv)
lcRolls  = ''
lcWareCode = ''
lcColor   = ''
lnToIss = 0
lcFabric = ''

FUNCTION lfvWare
lcWareCode = laMatWare[lnIssWare,2]


FUNCTION lfCurrAppl

PRIVATE lnOldAls , lnToRet
lnOldAls = SELECT(0)
SELECT (lcTmpRecv)
SET ORDER TO (lcTmpRecv1)
*--- cRollid + FABRIC + cFabClr + CUTTKT
lnToRet = 0
*--- lcFabric+ lcCutTkt +lcColor+lcDyeObj +lcWareCode
=SEEK(lcFabric + lcColor)
SCAN REST WHILE FABRIC + cFabClr= lcFabric + lcColor;
          FOR RecTyp = "Apply" .AND. Dyelot=lcDyeObj
  lnToRet = lnToRet + Issue_Qty
ENDSCAN
SELECT(lnOldAls)
RETURN (lnToRet)

*: B:803716,1 SSH Get cost function
FUNCTION lfGetCost
PARAMETER lcItmRol , lcItem , lcClr , lcWare , lcDye

PRIVATE lnOldAls , lnToRet
lnOldAls = SELECT(0)
SELECT ROLLS
SET ORDER TO ROLLS
lnToRet = 0
IF SEEK(lcItmRol + lcItem + lcClr)
  lcTempResSes = cRSession
  =gfOpenFile(gcDataDir+'Matinvjl',gcDataDir+'Matinvjl','SH')
  SELECT MATINVJL
  *--- cfabric+ccolor+cwarecode+cdyelot+crsession+cisession
  IF SEEK(lcItem + lcClr + lcWare + lcDye + lcTempResSes)
    lnToRet = MATINVJL.nUntCstBuy
  ENDIF
ENDIF
SELECT(lnOldAls)
RETURN (lnToRet)