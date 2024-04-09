NOTE  REMOVE the lines commented as :
*E303369,1 TMI 03/31/2013 [Start] test code
*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\SMMENU.prg
*:  Module      : System Manager 
*: Program desc. : Menu
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 03/10/2013
*:  Reference   : *E303369,1
NOTE As I didn't do any change in the functionality of the screen I kept all the comments as they are, each function with the 
NOTE programmer name who first created it.

*:************************************************************************
lcRunScx = lfGetScx("SM\SMMENU.scx")
DO FORM (lcRunScx) 

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : Get the scx path to run in SaaS environemt
************************************************************
FUNCTION lfGetScx
PARAMETERS lcScx
LOCAL lcRunScx
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.clientscreenhome+lcScx)
  lcRunScx = oAriaApplication.clientscreenhome+lcScx
ELSE
  lcRunScx = oAriaApplication.screenhome+lcScx
ENDIF   
RETURN lcRunScx
 *- End of lfGetScx.

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

*loFormSet.AddProperty('lcProgName','SMMENU')

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE 
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE 

*- Open tables 
=lfOpenPRGFILES('SMMENU')

IF USED('SYDOBJCT')
  USE IN SYDOBJCT
ENDIF   
gfOpenFile(oAriaApplication.caria4syspath+'SYDOBJCT','CAPOBJNAM','SH')

IF !lfDefineVars(loFormSet)
  RETURN .F.
ENDIF   

WITH loFormSet.Ariaform1
  .laApp.RowSource = 'Thisformset.laApp'
  .laApp.Value = loFormSet.laApp[1,2]
  .laApp.Refresh()
ENDWITH   


*- End of lfFormInit.
************************************************************
*! Name      : lfFormdestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/17/2013
*! Purpose   : lfFormdestroy
************************************************************
FUNCTION lfFormdestroy
PARAMETERS loFormSet

lcMenu_Tmp = loFormSet.lcMenu_Tmp

*** Clear all the filters.  & modify the last indices. ***
SELECT SYDAPPL
lcPrv1_Tag = loFormSet.lcPrv1_Tag
SET ORDER TO TAG &lcPrv1_Tag
SET FILTER TO

SELECT SYDREPRT
lcPrv2_Tag = loFormSet.lcPrv2_Tag
SET ORDER TO TAG &lcPrv2_Tag

SET FILTER TO cVer<>"A40"
SELECT SYDOBJCT
lcPrv3_Tag = loFormSet.lcPrv3_Tag
SET ORDER TO TAG &lcPrv3_Tag
SET FILTER TO

*** Erase the temp. files. ***
IF USED(lcMenu_Tmp)
  USE IN &lcMenu_Tmp
ENDIF  
gcWorkDir = oAriaApplication.WorkDir
ERASE (gcWorkDir+loFormSet.lcMenu_Tmp+'.DBF')
ERASE (gcWorkDir+loFormSet.lcMenu_Tmp+'.CDX')
ERASE (gcWorkDir+loFormSet.lcMenu_Tmp+'.FPT')

*** Erase the Back up MENU file. ***
lcFile = gcWorkDir + "SYCMENU.DBF"
IF FILE(lcFile)
  ERASE (gcWorkDir+"SYCMENU.DBF")
  ERASE (gcWorkDir+"SYCMENU.CDX")
ENDIF

*- End of lfFormdestroy.

*/************************ original code *********************************/*

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/10/2013
*! Purpose   : 
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

loFormSet.AddProperty('laApp[1,2]',' ')
loFormSet.AddProperty('laMenu[1,19]',' ')
loFormSet.AddProperty('laDefPop[1,2]',' ')
loFormSet.AddProperty('laProc2Del[1]',' ')
loFormSet.AddProperty('laStruc[1,19]',' ')
loFormSet.AddProperty('laDefSPop[1,2]',' ')
loFormSet.AddProperty('laBar_Com[1,2]',' ')

*STORE " " TO loFormSet.laApp,loFormSet.laMenu,loFormSet.laDefPop,loFormSet.laProc2Del,;
             loFormSet.laStruc,loFormSet.laDefSPop,loFormSet.laBar_Com

DECLARE loFormSet.laDefPop[3,2]
loFormSet.laDefPop[1,1] = "\<File"
loFormSet.laDefPop[1,2] = "01"
loFormSet.laDefPop[2,1] = "\<Transaction"
loFormSet.laDefPop[2,2] = "04"
loFormSet.laDefPop[3,1] = "\<Output"
loFormSet.laDefPop[3,2] = "05"

DECLARE loFormSet.laDefSPop[9,2]
loFormSet.laDefSPop[1,1] = "\<File"
loFormSet.laDefSPop[1,2] = "01"
loFormSet.laDefSPop[2,1] = "\<Edit"
loFormSet.laDefSPop[2,2] = "02"
loFormSet.laDefSPop[3,1] = "\<Record"
loFormSet.laDefSPop[3,2] = "03"
loFormSet.laDefSPop[4,1] = "\<Transaction"
loFormSet.laDefSPop[4,2] = "04"
loFormSet.laDefSPop[5,1] = "\<Output"
loFormSet.laDefSPop[5,2] = "05"
loFormSet.laDefSPop[6,1] = "\<User"
loFormSet.laDefSPop[6,2] = "06"
loFormSet.laDefSPop[7,1] = "\<Modules"
loFormSet.laDefSPop[7,2] = "07"
loFormSet.laDefSPop[8,1] = "\<Window"
loFormSet.laDefSPop[8,2] = "08"
loFormSet.laDefSPop[9,1] = "\<Help"
loFormSet.laDefSPop[9,2] = "09"

DECLARE loFormSet.laBar_Com[25,2]

loFormSet.laBar_Com[1,1] = "Help..."
loFormSet.laBar_Com[1,2] = "_MST_HELP"

loFormSet.laBar_Com[2,1] = "Printer Setup..."
loFormSet.laBar_Com[2,2] = "_MFI_SETUP"

loFormSet.laBar_Com[3,1] = "Print..."
loFormSet.laBar_Com[3,2] = "_MFI_PRINT"

loFormSet.laBar_Com[4,1] = "Cut"
loFormSet.laBar_Com[4,2] = "_MED_CUT"

loFormSet.laBar_Com[5,1] = "Copy"
loFormSet.laBar_Com[5,2] = "_MED_COPY"

loFormSet.laBar_Com[6,1] = "Paste"
loFormSet.laBar_Com[6,2] = "_MED_PASTE"

loFormSet.laBar_Com[7,1] = "Select All"
loFormSet.laBar_Com[7,2] = "_MED_SLCTA"

loFormSet.laBar_Com[8,1] = "Goto Line..."
loFormSet.laBar_Com[8,2] = "_MED_GOTO"

loFormSet.laBar_Com[9,1] = "Find..."
loFormSet.laBar_Com[9,2] = "_MED_FIND"

loFormSet.laBar_Com[10,1] = "Find Again"
loFormSet.laBar_Com[10,2] = "_MED_FINDA"

loFormSet.laBar_Com[11,1] = "Replace And Find Again"
loFormSet.laBar_Com[11,2] = "_MED_REPL"

loFormSet.laBar_Com[12,1] = "Replace All"
loFormSet.laBar_Com[12,2] = "_MED_REPLA"

loFormSet.laBar_Com[13,1] = "Clear"
loFormSet.laBar_Com[13,2] = "_MWI_CLEAR"

loFormSet.laBar_Com[14,1] = "Move" 
loFormSet.laBar_Com[14,2] = "_MWI_MOVE"

loFormSet.laBar_Com[15,1] = "Size"
loFormSet.laBar_Com[15,2] = "_MWI_SIZE"

loFormSet.laBar_Com[16,1] = "Zoom "
loFormSet.laBar_Com[16,2] = "_MWI_ZOOM"

loFormSet.laBar_Com[17,1] = "Zoom "
loFormSet.laBar_Com[17,2] = "_MWI_MIN"

loFormSet.laBar_Com[18,1] = "Cycle"
loFormSet.laBar_Com[18,2] = "_MWI_ROTAT"

loFormSet.laBar_Com[19,1] = "Debug"
loFormSet.laBar_Com[19,2] = "_MWI_DEBUG"

loFormSet.laBar_Com[20,1] = "Trace"
loFormSet.laBar_Com[20,2] = "_MWI_TRACE"

loFormSet.laBar_Com[21,1] = "Macros..."
loFormSet.laBar_Com[21,2] = "_MST_MACRO"

loFormSet.laBar_Com[22,1] = "Calculator"
loFormSet.laBar_Com[22,2] = "_MST_CALCU"

loFormSet.laBar_Com[23,1] = "Calendar/Diary"
loFormSet.laBar_Com[23,2] = "_MST_DIARY"

loFormSet.laBar_Com[24,1] = "Filer"
loFormSet.laBar_Com[24,2] = "_MST_FILER"

loFormSet.laBar_Com[25,1] = "View"
loFormSet.laBar_Com[25,2] = "_MWI_VIEW"


*!*	*ON KEY LABEL LEFTMOUSE    DO lfMDrag
*!*	*ON KEY LABEL CTRL+DNARROW DO lfCtrDn
*!*	*ON KEY LABEL CTRL+UPARROW DO lfCtrUp

loFormSet.AddProperty('llMen_Chng' , .F.)
loFormSet.AddProperty('llSaved'    , .T.)
loFormSet.AddProperty('llDontHid'  , .T.)
loFormSet.AddProperty('llFirstm'   , .T.)
*loFormSet.AddProperty('lnNewBar'   ,  1)
*loFormSet.AddProperty('llMdown'    , .T.)
loFormSet.AddProperty('lnAryNo'    , 1)
loFormSet.AddProperty('lnSysPrg'   , 1)
loFormSet.AddProperty('lnBarLevel' , 1)
loFormSet.AddProperty('lnCurLevel' , 1)
loFormSet.AddProperty('lnFirstBar' , 1)
loFormSet.AddProperty('lsMenu'     , 1)
loFormSet.AddProperty('lcProgID' , " ")
loFormSet.AddProperty('lcProgName' , " ")
loFormSet.AddProperty('lcProgPram' , " "     )
&& Variable to hold the program Prameters
loFormSet.AddProperty('lcApp_Id'   , " ")
loFormSet.AddProperty('lcApp_Desc' , " ")
*loFormSet.AddProperty('loFormSet.laApp'      , " ")
*loFormSet.AddProperty('loFormSet.lcMenu_Tmp' , " ")

loFormSet.AddProperty('rbSysPrg'  , 1)
loFormSet.AddProperty('llSysPrg'  , .T.)

loFormSet.AddProperty('lcExtPrg'  , " ")
loFormSet.AddProperty('puPop'     , " ")
*loFormSet.AddProperty('ibPopApp'  , 1)
loFormSet.AddProperty('llFrsPop'  , .T.)

loFormSet.AddProperty('lcPrv1_Tag' , "")
loFormSet.AddProperty('lcPrv2_Tag' , "")
loFormSet.AddProperty('lcPrv3_Tag' , "")

loFormSet.AddProperty('lcHotKey'   , "")
loFormSet.AddProperty('lcMesage'   , "")

*** Variables hold the charcters displayed in ***
*** the list in case of "WINDOWS"...
loFormSet.AddProperty('lcSpace'    , " ")
loFormSet.AddProperty('lcUpLeft'   , "Ú")
loFormSet.AddProperty('lcUpRght'   , "¿")
loFormSet.AddProperty('lcDnLeft'   , "À")
loFormSet.AddProperty('lcDnRght'   , "Ù")
loFormSet.AddProperty('lcHzLine'   , "Ä")
loFormSet.AddProperty('lcVrLine'   , "³")
loFormSet.AddProperty('lcHfBloc'   , " ")
*loFormSet.AddProperty('loFormSet.lcHfBloc'   , " ")
loFormSet.AddProperty('lcBlock'   , " ")
loFormSet.AddProperty('lcUpBloc'   , " ")
loFormSet.AddProperty('lcBranch'   , "")
loFormSet.AddProperty('lcLnLeft'   , "Ã")
loFormSet.AddProperty('lcLnRght'   , "´")
loFormSet.AddProperty('lcHfBlocs'  , REPLICATE(loFormSet.lcHfBloc,30))
loFormSet.AddProperty('ibPopApp' , 1)

*** Variables hold the column no. of the ***
*** array that hold the list data...
loFormSet.AddProperty('lnPad_Pos',0)
loFormSet.AddProperty('lnPop_Pos',0)
loFormSet.AddProperty('lnBar_Pos',0)
loFormSet.AddProperty('lnPop_Levl',0)
loFormSet.AddProperty('lnSub_Prpt',0)
loFormSet.AddProperty('lnSub_Typ',0)
loFormSet.AddProperty('lnSub_Ctg',0)
loFormSet.AddProperty('lnProssId',0)
loFormSet.AddProperty('lnProcType',0)
loFormSet.AddProperty('lnProcPath',0)
loFormSet.AddProperty('llDefault',0)
loFormSet.AddProperty('lnMstr_nam',0)
loFormSet.AddProperty('lnBar_Msg',0)
loFormSet.AddProperty('lnHot_key',0)
loFormSet.AddProperty('lnParamPos',0)


loFormSet.AddProperty('lnUpgrade'  , 0     )      && Col. No. of the upgrade level)

loFormSet.AddProperty('lnBarMod'   , 0     )      && Col. No. of the bar Module that filled for the Edi Module almost.)

*** The text of the 1st & 2nd prompt of the radio button. ***
*loFormSet.AddProperty('lcPrmp1'   , "System P\<rg.")
*loFormSet.AddProperty('lcPrmp2'   , "System \<Bar")
*loFormSet.AddProperty('lcPrmp4'   , "\<Custom Prg.")
WITH loFormSet.Ariaform1.rbSysPrg
  .lcPrmp1.Caption = "System P\<rg."
  .lcPrmp2.Caption = "System \<Bar"
  .lcPrmp4.Caption = "\<Custom Prg."
ENDWITH 

loFormSet.AddProperty('lcMenu_Tmp' , gfTempName())
*- End of lfDefineVars.

*** Get the available application in the system. ***
    SET ENGINEBEHAVIOR 70
SELECT cApp_Name,cApp_Id ;
       FROM (oAriaApplication.SysPath+"sydAppl") ;
       INTO ARRAY loFormSet.laApp ;
       ORDER BY cApp_Name
    SET ENGINEBEHAVIOR 90
    
IF EMPTY(loFormSet.laApp[1,1])
  STORE " " TO loFormSet.lcApp_Id,loFormSet.lcApp_Desc,loFormSet.laApp
ELSE
  loFormSet.lcApp_Id   = loFormSet.laApp[1,2]
  loFormSet.ibPopApp   = 1
ENDIF

*** Set filter into the module dictionary file ***
*** to all modules except "SY"...
SELECT SYDAPPL
loFormSet.lcPrv1_Tag = SYS(22)
SET ORDER TO TAG CAPPNAME
SET FILTER TO
SET FILTER TO CAPP_ID <> "SY"
LOCATE

*** Set filter into the report dictionary file ***
*** to the current module + "R" type...
SELECT SYDREPRT

loFormSet.lcPrv2_Tag = SYS(22)
SET ORDER TO TAG CREPNAME

lcF = ALLTRIM(loFormSet.lcApp_Id)
SET FILTER TO CAPP_ID + CAPOBJTYP + CMAINENTR = "&lcF" + "P" + "Y" .AND. cVer<>"A40"

LOCATE

*** Set filter into the module object file ***
*** to the current module + "P" type...
SELECT SYDOBJCT
loFormSet.lcPrv3_Tag = SYS(22)
SET ORDER TO TAG OBJLNGNAM
SET FILTER TO
SET FILTER TO CAPP_ID+CAPOBJTYP="&lcF"+"P"
LOCATE

SELECT SYCMENU
SET ORDER TO TAG APPPOPBAR

*** Back up the master file before process anything. ***
*** And save the back up file in the work directory. ***
gcWorkDir = oAriaApplication.WorkDir
COPY TO &gcWorkDir.SYCMENU CDX

*** Copy to the temp.  file which will be use in the ***
*** running of this program.  This temp file will be ***
*** use to update the master file during saving...

lcMenu_Tmp = loFormSet.lcMenu_Tmp
COPY TO &gcWorkDir.&lcMenu_Tmp CDX

SELECT 0
lcMenu_Tmp = loFormSet.lcMenu_Tmp
USE &gcWorkDir.&lcMenu_Tmp EXCLUSIVE
SET ORDER TO TAG APPPOPBAR

*** Get data for the current module from the menu file. ***
=lfSelData(loFormSet)

DEFINE POPUP puMenu RELATIVE SCROLL	MARK loFormSet.lcHfBloc
DEFINE BAR 1 OF puMenu PROMPT " "

*** Build the popup that has the drawing menu. ***
=lfBldBars(loFormSet)

*- Define control source
WITH loFormset.AriaForm1
  .laApp.RowSource = 'Thisformset.laApp'
  .rbSysPrg.ControlSource = 'Thisformset.rbSysPrg'
  .lcExtPrg.ControlSource = 'Thisformset.lcExtPrg'
  .puPop.ControlSource = 'ThisFormSet.lcProgID'
ENDWITH   

*DO (gcScrDir + gcWinAppl + '\SMMENU.SPR')
*!*	lcRunScx = lfGetScx("SM\SMMENU.scx")
*!*	DO FORM (lcRunScx) WITH loFormSet

*RELEASE POPUP puMenu
*RELEASE POPUP PUPOP1
*RELEASE POPUP PUPOP2
*RELEASE POPUP PUPOP3

*!*	*** Clear all the filters.  & modify the last indices. ***
*!*	SELECT SYDAPPL
*!*	SET ORDER TO TAG &loFormSet.lcPrv1_Tag
*!*	SET FILTER TO

*!*	SELECT SYDREPRT
*!*	SET ORDER TO TAG &loFormSet.lcPrv2_Tag

*!*	SET FILTER TO cVer<>"A40"
*!*	SELECT SYDOBJCT
*!*	SET ORDER TO TAG &loFormSet.lcPrv3_Tag
*!*	SET FILTER TO

*!*	*** Erase the temp. files. ***
*!*	IF USED(loFormSet.lcMenu_Tmp)
*!*	  USE IN &loFormSet.lcMenu_Tmp
*!*	ENDIF  

*!*	ERASE (gcWorkDir+loFormSet.lcMenu_Tmp+'.DBF')
*!*	ERASE (gcWorkDir+loFormSet.lcMenu_Tmp+'.CDX')
*!*	ERASE (gcWorkDir+loFormSet.lcMenu_Tmp+'.FPT')

*!*	*** Erase the Back up MENU file. ***
*!*	lcFile = gcWorkDir + "SYCMENU.DBF"
*!*	IF FILE(lcFile)
*!*	  ERASE (gcWorkDir+"SYCMENU.DBF")
*!*	  ERASE (gcWorkDir+"SYCMENU.CDX")
*!*	ENDIF

**ON KEY

*!**************************************************************************
*!
*!      Function: lfShow
*!
*!**************************************************************************
*
FUNCTION lfShow
PARAMETERS loFormSet

IF loFormSet.lsMenu <> 0
  *** Get the array no. of the current bar. ***
  loFormSet.lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)),;
             RAT("+",PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)))-3,3))
ENDIF

IF loFormSet.lnAryNo > 0
  IF LEFT(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt],2) = "\-" .OR. ;
     EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt]) .OR. ;
     AT(loFormSet.lcUpLeft,PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu))) > 0 .OR. ;
     AT(loFormSet.lcDnLeft,PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu))) > 0
    SHOW GET pbHotKey  DISABLE
    SHOW GET pbMessage DISABLE
  ELSE
    SHOW GET pbHotKey  ENABLE
    SHOW GET pbMessage ENABLE
  ENDIF
ENDIF

IF loFormSet.rbSysPrg = 2
  IF loFormSet.lnAryNo > 0
    IF loFormSet.lcApp_Id = "SY"
      IF VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 7

        *** Disable the screen objects that has ***
        *** {The program to do}...
        *** Enable & activate the screen objects ***
        *** that has {Get external program}...
        *SHOW GETS WINDOW SMMENU2 ENABLE  ONLY
        *ACTIVATE WINDOW SMMENU2 TOP
        *SHOW GETS WINDOW SMMENU1 DISABLE ONLY
      ELSE
        *** Disable the screen objects that has ***
        *** {Get external program}...
        *** Enable & activate the screen objects ***
        *** that has {The program to do}...
        *SHOW GETS WINDOW SMMENU1 ENABLE  ONLY
        *ACTIVATE WINDOW SMMENU1 TOP
        *SHOW GETS WINDOW SMMENU2 DISABLE ONLY
      ENDIF
    ELSE
      *** Disable the screen objects that has ***
      *** {Get external program}...
      *** Enable & activate the screen objects ***
      *** that has {The program to do}...
      *SHOW GETS WINDOW SMMENU2 ENABLE  ONLY
      *ACTIVATE WINDOW SMMENU2 TOP
      *SHOW GETS WINDOW SMMENU1 DISABLE ONLY
    ENDIF
  ENDIF
ELSE
  *** Disable the screen objects that has ***
  *** {Get external program}...
  *** Enable & activate the screen objects ***
  *** that has {The program to do}...
  *SHOW GETS WINDOW SMMENU1 ENABLE  ONLY
  *ACTIVATE WINDOW SMMENU1 TOP
  *SHOW GETS WINDOW SMMENU2 DISABLE ONLY
ENDIF

*** If the current module is the main system , Disable ***
*** the radio button 3 {Custom program} else enable it...
IF loFormSet.lcApp_Id = "SY"
  IF loFormSet.lnAryNo > 0
    IF VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 7    
      loFormSet.Ariaform1.rbSysPrg.lcPrmp3.Enabled = .F.
      loFormSet.Ariaform1.rbSysPrg.lcPrmp4.Enabled = .T.
    ELSE
      loFormSet.Ariaform1.rbSysPrg.lcPrmp3.Enabled = .T.
      loFormSet.Ariaform1.rbSysPrg.lcPrmp4.Enabled = .F.
    ENDIF
  ELSE
    loFormSet.Ariaform1.rbSysPrg.lcPrmp3.Enabled = .T.
    loFormSet.Ariaform1.rbSysPrg.lcPrmp4.Enabled = .F.
  ENDIF
ELSE
  loFormSet.Ariaform1.rbSysPrg.lcPrmp3.Enabled = .T.
  loFormSet.Ariaform1.rbSysPrg.lcPrmp4.Enabled = .T.
ENDIF

*** Activate the screen that has the list on top. ***
*- ACTIVATE WINDOW SMMENU0 TOP -*

*!**************************************************************************
*!
*!      Function: lfvClrLink
*!
*!**************************************************************************
*
*** Valid function of the push button pbClrLink. { Clear Link } ***

FUNCTION lfvClrLink
PARAMETERS loFormSet

IF loFormSet.lsMenu <> 0
  *** Get the array no. of the current bar. ***
  loFormSet.lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)),;
             RAT("+",PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)))-3,3))

  IF loFormSet.lnAryNo <> 0
    IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.llDefault]
      *** You can not change anything in default bars. ***
      *** <  Ok  > ***
      =gfModalGen("TRM00148B00000","DIALOG")
      RETURN
    ENDIF
    
    loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = ""
    loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath] = ""
    
    IF !EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]) .AND. loFormSet.lcApp_Id <> "SY"
      IF !EMPTY(loFormSet.laProc2Del[1])
        DIMENSION loFormSet.laProc2Del[ALEN(loFormSet.laProc2Del,1)+1]
        =AINS(loFormSet.laProc2Del,1)
      ENDIF
      loFormSet.laProc2Del[1] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]
    ENDIF
    
    loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]  = ""
    loFormSet.lcProgName = " "
    loFormSet.lcProgID = ' '
    loFormSet.lcExtPrg   = " "
    loFormSet.rbSysPrg   = 0
    *SHOW GET loFormSet.rbSysPrg
        loFormSet.puPop = " "
        *SHOW GET loFormSet.puPop
    
    *** Flag to say that the current ***
    *** module need to be save...
    loFormSet.llSaved = .F.

  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfvHotKey
*!
*!**************************************************************************
*
*** Valid function of the push button pbHotKey. { Hot Key } ***

FUNCTION lfvHotKey
PARAMETERS loFormSet

IF loFormSet.lsMenu <> 0
  *** Get the array no. of the current bar. ***
  loFormSet.lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)),;
             RAT("+",PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)))-3,3))

  IF loFormSet.lnAryNo <> 0
    
    *** Clear the trapped keys. ***
    *ON KEY
    
    *** You cannot edit the hot key of a pad. ***
    lcHotStat = IIF(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ] = "P","DISABLE","ENABLE")
    
    loFormSet.lcHotKey  = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnHot_key]
*    DO (gcScrDir + gcWinAppl + '\SMHOTKY.SPR')
     lcRunScx = lfGetScx("SM\SMHOTKY.scx")
     DO FORM (lcRunScx) WITH loFormSet

    *** Trap the previous key again
    **ON KEY LABEL LEFTMOUSE    DO lfMDrag
    *ON KEY LABEL CTRL+DNARROW DO lfCtrDn
    **ON KEY LABEL CTRL+UPARROW DO lfCtrUp

  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfvHot_Ok
*!
*!**************************************************************************
*
*** Valid function of the push button < OK > in the Hot Key screen. ***

FUNCTION lfvHot_Ok
PARAMETERS loFormSet

loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnHot_key]  = loFormSet.lcHotKey

*** Flag to say that the current ***
*** module need to be save...
loFormSet.llSaved = .F.

*!**************************************************************************
*!
*!      Function: lfvMessage
*!
*!**************************************************************************
*
*** Valid function of the push button pbMessage. { Message } ***

FUNCTION lfvMessage
PARAMETERS loFormSet

IF loFormSet.lsMenu <> 0
  *** Get the array no. of the current bar. ***
  loFormSet.lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)),;
             RAT("+",PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)))-3,3))

  IF loFormSet.lnAryNo <> 0
    
    *** Clear the trapped keys
    *ON KEY
    
    loFormSet.lcMesage = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnBar_Msg]
*    DO (gcScrDir + gcWinAppl + '\SMMESAG.SPR')
    lcRunScx = lfGetScx("SM\SMMESAG.scx")
    DO FORM (lcRunScx) WITH loFormSet

    *** Trap the previous keys again. ***
    *ON KEY LABEL LEFTMOUSE    DO lfMDrag
    *ON KEY LABEL CTRL+DNARROW DO lfCtrDn
    *ON KEY LABEL CTRL+UPARROW DO lfCtrUp

  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfvMsg_Ok
*!
*!**************************************************************************
*
*** Valid function of the push button < OK > in the Message screen. ***

FUNCTION lfvMsg_Ok
PARAMETERS loFormSet

loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnBar_Msg]  = loFormSet.lcMesage

*** Flag to say that the current ***
*** module need to be save...
loFormSet.llSaved = .F.

*!**************************************************************************
*!
*!      Function: lfwSysPrg
*!
*!**************************************************************************
*
*** When function of the radio button loFormSet.rbSysPrg. ***

FUNCTION lfwSysPrg
PARAMETERS loFormSet,loFld

loFormSet.lnSysPrg = loFormSet.rbSysPrg
RETURN loFormSet.llSysPrg
 
*!**************************************************************************
*!
*!      Function: lfvSysPrg
*!
*!**************************************************************************
*** Valid function of the radio button loFormSet.rbSysPrg. ***
FUNCTION lfvSysPrg
PARAMETERS loFormSet,loFld

*IF loFormSet.lsMenu <> 0 .AND. loFormSet.lnSysPrg <> loFormSet.rbSysPrg
IF loFormSet.lsMenu <> 0 
  *** Get the array no. of the current bar. ***
  loFormSet.lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)),;
             RAT("+",PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)))-3,3))

  IF loFormSet.lnAryNo <> 0
    IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.llDefault]
      *** You can not change anything in default bars. ***
      *** <  Ok  > ***
      =gfModalGen("TRM00148B00000","DIALOG")
      loFormSet.rbSysPrg = loFormSet.lnSysPrg
      *SHOW GET loFormSet.rbSysPrg
      RETURN
    ENDIF
    
    loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnUpgrade]  = IIF(loFormSet.rbSysPrg = 4 , "U" , IIF(loFormSet.lcApp_Id = "SY" OR loFormSet.lcApp_Id = "SM" , "S" , "A"))
    
    lfShowpuPop(.T.)

    DO CASE
      CASE loFormSet.rbSysPrg = 1
        DO CASE
          CASE VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 5
            *** If current bar is calling a report. ***
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "R"
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath] = ""
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ]  = "C"
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]  = ""
            loFormSet.lcProgName = " "
            loFormSet.lcProgID = ' '
            loFormSet.lcExtPrg   = " "
          CASE loFormSet.lcApp_Id = "SY" .AND. ;
               VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 7
            *** If current bar is calling a module. ***
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "M"
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath] = ""
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ]  = "C"
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]  = ""
            loFormSet.lcProgName = " "
            loFormSet.lcProgID = ' '
            loFormSet.lcExtPrg   = " "
          OTHERWISE
            *** If current bar is calling a program. ***
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "P"
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath] = ""
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ]  = "C"
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]  = ""
            loFormSet.lcProgName = " "
            loFormSet.lcProgID = ' '
            loFormSet.lcExtPrg   = " "
        ENDCASE
      CASE loFormSet.rbSysPrg = 2
      
        lfShowpuPop(.F.)

        IF loFormSet.lcApp_Id = "SY"
          *** If current bar is calling a system bar. ***
          loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "S"
          loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath] = ""
          loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ]  = "B"
          loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]  = ""
          loFormSet.lcProgName = " "
          loFormSet.lcProgID = ' '
          loFormSet.lcExtPrg   = " "
        ELSE
          IF VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 5
            *** If current is calling an external report. ***
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "R"
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath] = ""
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ]  = "C"
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]  = ""
            loFormSet.lcProgName = " "
            loFormSet.lcProgID = ' '
            loFormSet.lcExtPrg   = " "
          ELSE
            *** If current is calling an external program. ***
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "E"
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath] = ""
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ]  = "C"
            loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]  = ""
            loFormSet.lcProgName = " "
            loFormSet.lcProgID = ' '
            loFormSet.lcExtPrg   = " "
          ENDIF
        ENDIF
      CASE loFormSet.rbSysPrg = 3
        *** If current bar is calling a global function or procedure. ***
        loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "G"
        loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath] = ""
        loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ]  = "C"
        loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]  = ""
        loFormSet.lcProgName = " "
        loFormSet.lcProgID = ' '
        loFormSet.lcExtPrg   = " "
        
      CASE loFormSet.rbSysPrg = 4
        *** If current bar is calling a custom program or report. ***
        loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = IIF(VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 5,"R","C")
        loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath] = ""
        loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ]  = "C"
        loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]  = ""
        loFormSet.lcProgName = " "
        loFormSet.lcProgID = ' '
        loFormSet.lcExtPrg   = " "
    ENDCASE

    *** Flag to say that the current ***
    *** module need to be save...
    loFormSet.llSaved = .F.

  ENDIF
  
  *** To refresh the changed say fields. ***

  IF loFormSet.rbSysPrg = 2
    IF loFormSet.lnAryNo > 0
      IF loFormSet.lcApp_Id = "SY"
        IF VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 7
          *SHOW GETS WINDOW SMMENU2 ENABLE  ONLY
          *ACTIVATE WINDOW SMMENU2 TOP
          *SHOW GETS WINDOW SMMENU1 DISABLE ONLY
        ELSE
          *SHOW GETS WINDOW SMMENU1 ENABLE  ONLY
          *ACTIVATE WINDOW SMMENU1 TOP
          *SHOW GETS WINDOW SMMENU2 DISABLE ONLY
        ENDIF
      ELSE
        *SHOW GETS WINDOW SMMENU2 ENABLE  ONLY
        *ACTIVATE WINDOW SMMENU2 TOP
        *SHOW GETS WINDOW SMMENU1 DISABLE ONLY
      ENDIF
    ENDIF
  ELSE
    *SHOW GETS WINDOW SMMENU1 ENABLE  ONLY
    *ACTIVATE WINDOW SMMENU1 TOP
    *SHOW GETS WINDOW SMMENU2 DISABLE ONLY
  ENDIF

ENDIF
************************************************************
*! Name      : lfShowpuPop
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/01/2013
*! Purpose   : lfShowpuPop
************************************************************
FUNCTION lfShowpuPop
PARAMETERS llVisible

loFormSet.Ariaform1.Arialabel2.Visible = llVisible
loFormSet.Ariaform1.Arialabel3.Visible = llVisible
loFormSet.AriaForm1.pupop.Visible = llVisible
loFormSet.AriaForm1.lcProgPram.Visible = llVisible
loFormSet.AriaForm1.cmdSelectProgram.Visible = !llVisible
loFormSet.AriaForm1.lcExtPrg.Visible = !llVisible

*- End of lfShowpuPop.

*!**************************************************************************
*!
*!      Function: lfvGetEPrg
*!
*!**************************************************************************
*
*** Valid function of the push button pbGetEPrg. ***

FUNCTION lfvGetEPrg
PARAMETERS loFormSet,loFld

*** Save the previous external program or report. ***
lcOldProg = loFormSet.lcExtPrg

IF loFormSet.lsMenu <> 0
  *** Get the array element of the current bar
  loFormSet.lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)),;
             RAT("+",PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)))-3,3))
 
  IF loFormSet.lnAryNo <> 0

    *** Save the current default
    lcDefSav = SET("DEFAULT")
    
    *** Choose a frx, fxp, prg, app as an external program or report. ***
    loFormSet.lcExtPrg  = GETFILE('FRX;FXP;PRG;APP','Select external program','Select',1)

    *** Restore the previous defaults
    SET DEFAULT TO &lcDefSav
    
    *** If choose nothing, restore the old value. ***
    loFormSet.lcExtPrg  = IIF(EMPTY(loFormSet.lcExtPrg),lcOldProg,loFormSet.lcExtPrg)

    *** Put the process type in the array element. ***
    loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = IIF(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType]="R","R","E")

    *** Put the process id path in the array element. ***
    loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath] = loFormSet.lcExtPrg

    *** Put the process id in the array element. ***
    IF !EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]) .AND. loFormSet.lcApp_Id <> "SY"
      IF !EMPTY(loFormSet.laProc2Del[1])
        DIMENSION loFormSet.laProc2Del[ALEN(loFormSet.laProc2Del,1)+1]
        =AINS(loFormSet.laProc2Del,1)
      ENDIF
      loFormSet.laProc2Del[1] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]
    ENDIF
    loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]  = ;
                           GFSTRIPEXT(SUBSTR(loFormSet.lcExtPrg,RAT('\',loFormSet.lcExtPrg)+1))
    *** Flag to say that the current ***
    *** module need to be save...
    loFormSet.llSaved   = .F.
    
  ENDIF
  
ENDIF

SHOW GET loFormSet.lcExtPrg
*x*
*!**************************************************************************
*!
*!      Function: lfvPrg2do
*!
*!**************************************************************************
FUNCTION lfvPrg2do
PARAMETERS loFormSet,loFld

*** Get the array element of the current bar. ***
loFormSet.lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)),;
              RAT("+",PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)))-3,3))

loFormSet.Ariaform1.puPop.RowSource = ''
IF loFormSet.lnAryNo > 0
  
  IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.llDefault]
    *** You can not change anything in default bars. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00148B00000","DIALOG")
    RETURN loFld.Value <> 2
  ENDIF

  IF loFormSet.rbSysPrg = 0
    *** You have to specify the process type ***
    *** for this bar before linking. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00212B00000","DIALOG")
    RETURN loFld.Value <> 2
  ENDIF
  
  *** If the current bar is a normal bar ***
  *** and it's not disable...
  IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ] $ "CB" .AND.;
    LEFT(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt],2) <> "\-" .AND. loFld.Value <> 2

    *** If the process id type not system. ***
    IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] <> "S" .AND. ;
       loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] <> "M"
      
      *** If pad positon of the current par  say ***
      *** it is output  pad , select the  report ***
      *** dictionary file else select the module ***
      *** object file...
      IF VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 5 .AND. ;
         loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] <> "G"
        SELECT SYDREPRT
        
        lcApp = loFormSet.lcApp_Id
        lcFlt = '"&lcApp"+"Y" .AND. '+IIF(loFormSet.laMenu[loFormSet.lnAryNo,14] = 'U','Cupgrdlvl="U"','Cupgrdlvl$"SA"')+' .AND.  SYDREPRT.cVer<>"A40"'
        SET FILTER TO CAPP_ID+CMAINENTR = &lcFlt
      ELSE
        SELECT SYDOBJCT
        SET FILTER TO
        IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "G"
          lcV = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType]
          lcFlt = "'SY'+ '&lcV'"
          SET FILTER TO CAPP_ID+CAPOBJTYP=&lcFlt
        ELSE
          lcApp = loFormSet.lcApp_Id
          lcV = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType]
          lcFlt = "'&lcApp'+'&lcV'"
          SET FILTER TO CAPP_ID+CAPOBJTYP=&lcFlt
        ENDIF
      ENDIF
      
      *** Set filter to the current module and ***
      *** the current process id  type...
      LOCATE
      IF !FOUND()
        *** No (processes|reports) have been defined in ***
        *** the (process|report dictionary) file...
        *** <  Ok  > ***
        =gfModalGen("TRM00142B00000","DIALOG",;
                IIF(VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 5,;
                "reports|report dictionary","processes|process"))
        RETURN loFld.Value <> 2
      ENDIF
    ENDIF   
    
    loFormSet.Ariaform1.puPop.RowSourceType = 6
    DO CASE
        
      *** If current pad position says it is an output pad. ***
      CASE VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 5 .AND. ;
           loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] <> "G"
        
        *** Link the popup to the report dictionary file. ***
        loFormSet.puPop = SYDREPRT.CREP_NAME
        *ACTIVATE POPUP puPop1
        *DEFINE POPUP pupop1 PROMPT FIELD CREP_NAME SCROLL 
        loFormSet.Ariaform1.puPop.RowSource = 'CREP_NAME,CREP_ID'
        *IF INLIST(LASTKEY(),13,32)
        loFormSet.puPop = SYDREPRT.CREP_NAME
        IF !EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]) .AND. loFormSet.lcApp_Id <> "SY"
          IF !EMPTY(loFormSet.laProc2Del[1])
            DIMENSION loFormSet.laProc2Del[ALEN(loFormSet.laProc2Del,1)+1]
            =AINS(loFormSet.laProc2Del,1)
          ENDIF
          loFormSet.laProc2Del[1] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]
        ENDIF
        *loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId] = SYDREPRT.CREP_ID
        loFormSet.lcProgName = SYDREPRT.CREP_NAME
        loFormSet.lcProgID = SYDREPRT.CREP_ID
        *SHOW GET loFormSet.puPop
        *ENDIF
        
      *** If the process type of the current bar calling ***
      *** module, branch from the module dictionary file ***
      CASE loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "M"
        
        *** Link the popup to the module dictionary file. ***
        loFormSet.puPop = SYDAPPL.CAPP_NAME
        *ACTIVATE POPUP puPop2
        *DEFINE POPUP pupop2 PROMPT FIELD CAPP_NAME SCROLL 
        loFormSet.Ariaform1.puPop.RowSource = 'CAPP_NAME,CAPP_ID'
        *IF INLIST(LASTKEY(),13,32)
        loFormSet.puPop = SYDAPPL.CAPP_NAME
        *loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId] = SYDAPPL.CAPP_ID
        loFormSet.lcProgName = SYDAPPL.CAPP_NAME
        loFormSet.lcProgID = SYDAPPL.CAPP_ID
        *SHOW GET loFormSet.puPop
        *ENDIF
        
      *** If the process type of the  current bar  is ***
      *** system  propgram  or custom  program or and ***
      *** the global function or procedure bar is not ***
      *** bar is not existed in the output pad...
      CASE loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] $ "PCG"
      
        *** Link the popup to the module object file. ***
        loFormSet.puPop = SYDOBJCT.CPRGLNAME
        *ACTIVATE POPUP puPop3
        *DEFINE POPUP pupop3 PROMPT FIELD CPRGLNAME SCROLL 
        loFormSet.Ariaform1.puPop.RowSource = 'CPRGLNAME,CAPOBJNAM'
        *IF INLIST(LASTKEY(),13,32)
        loFormSet.puPop = SYDOBJCT.CPRGLNAME
        IF !EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]) .AND. loFormSet.lcApp_Id <> "SY"
          IF !EMPTY(loFormSet.laProc2Del[1])
            DIMENSION loFormSet.laProc2Del[ALEN(loFormSet.laProc2Del,1)+1]
            =AINS(loFormSet.laProc2Del,1)
          ENDIF
          loFormSet.laProc2Del[1] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]
        ENDIF
        *loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId] = SYDOBJCT.CAPOBJNAM
        loFormSet.lcProgName = SYDOBJCT.CPRGLNAME
        loFormSet.lcProgID = SYDOBJCT.CAPOBJNAM
        *SHOW GET loFormSet.puPop
        *ENDIF
        
      *** If the process type of the current pad is system bar. ***
      CASE loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "S"
        
        *** Link the popup to the array hold the system bars. ***            
        loFormSet.Ariaform1.puPop.RowSourceType = 5
        loFormSet.Ariaform1.puPop.RowSource = 'ThisFormSet.laBar_Com'
        *loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId] = ;
             gfActPop(18.75,15.7,25.55,56.8,'loFormSet.laBar_Com',2,1,@loFormSet.puPop,gcBaseWind)
             
    ENDCASE
    
    IF ALLTRIM(UPPER(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt])) = "NEW BAR"
      *loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt] = ALLTRIM(loFormSet.lcProgName) + ;
                                      SPACE(45-LEN(ALLTRIM(loFormSet.lcProgName)))
      lnBarNo    = GETBAR("puMenu",loFormSet.lsMenu)
      lcPrompt   = PRMBAR("puMenu",lnBarNo)
      loFormSet.lnBarLevel = VAL(SUBSTR(lcPrompt,RAT("+",lcPrompt)+1))
      lcWhichPad = "^"+ALLTRIM(STR(VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos])))
      DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 loFormSet.lcHfBloc+loFormSet.lcHfBloc+loFormSet.lcVrLine+;
                 IIF(loFormSet.lnBarLevel=1,"",REPLICATE("  "+loFormSet.lcVrLine,loFormSet.lnBarLevel-1))+;
                 SUBSTR(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt],1,41)+" "+;
                 loFormSet.lcVrLine+loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+lcWhichPad+ ;
                 RIGHT(STR(loFormSet.lnAryNo),3)+"+"+STR(loFormSet.lnBarLevel,1)
    ENDIF
    *** Flag to say that the current module need to be save. ***
    loFormSet.llSaved = .F.
  ENDIF
ENDIF
loFormSet.Ariaform1.puPop.Refresh()
RETURN 
*loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType]
*- End of lfvPrg2do.
*!**************************************************************************
*!
*!      Function: lfwMenu
*!
*!**************************************************************************
*
*** When function of the list loFormSet.lsMenu. ***

FUNCTION lfwMenu
PARAMETERS loFormSet,loFld

IF loFormSet.lsMenu = 0
  RETURN
ENDIF
DO msg
*** Get the array element of the current bar. ***
loFormSet.lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)),;
              RAT("+",PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu)))-3,3))
loFormSet.lcProgPram = " "

IF loFormSet.lnAryNo > 0
  loFormSet.lcProgPram = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnParamPos]
  
  *** If the current bar is a normal bar and it's not ***
  *** a separator and its prompt is not empty...
  IF  loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ] $ "CB"          .AND.;
      LEFT(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt],2) <>"\-" .AND. ;
      !EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt])
    
    *** Flag to say that you've access to the radio button. ***
    loFormSet.llSysPrg = .T.
    
    DO CASE
      
      *** If the current bar exist in the output pad. ***
      CASE VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 5
        WITH loFormSet.Ariaform1.rbSysPrg
          .lcPrmp1.Caption   = "System \<Rep."
          .lcPrmp2.Caption   = "E\<xternal Rep."
          .lcPrmp4.Caption   = "\<Custom Rep."
        ENDWITH 
        
        IF EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath])
          
          *** If it is a system report, link to the report dictionary. ***
          IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "G"
            SELECT SYDOBJCT
            SET FILTER TO
            loFormSet.lcProgName = IIF(!EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]),;
                         ALLTRIM(LOOKUP(SYDOBJCT.CPRGLNAME,;
                         ALLTRIM("SY"+loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]),;
                         SYDOBJCT.CAPOBJNAM,'CAPP_ID'))," ")
            loFormSet.lcProgID = SYDOBJCT.CAPOBJNAM
          ELSE
            SELECT SYDREPRT
             
            SET FILTER TO SYDREPRT.cVer<>"A40" 

            loFormSet.lcProgName = IIF(!EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]),;
                         ALLTRIM(LOOKUP(SYDREPRT.CREP_NAME,;
                         ALLTRIM(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]),;
                         SYDREPRT.CREP_ID,'CREP_ID'))," ")
            loFormSet.lcProgID = SYDREPRT.CREP_ID
          ENDIF
          
          loFormSet.lcExtPrg   = " "
          DO CASE
            CASE loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "R"
              loFormSet.rbSysPrg = IIF(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnUpgrade] = "U" , 4 , 1)
            CASE loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "G"
              loFormSet.rbSysPrg = 3
            CASE loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "C"
              loFormSet.rbSysPrg = 4
            CASE EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType])
              loFormSet.rbSysPrg = 0
          ENDCASE
        ELSE
          
          *** If it is an external report. ***
          loFormSet.lcProgName = " "
          loFormSet.lcProgID = ' '
          loFormSet.lcExtPrg   = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath]
          loFormSet.rbSysPrg   = 2
        ENDIF
      
      *** If the current bar exist in the module pad. ***
      CASE VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 7
        WITH loFormSet.Ariaform1.rbSysPrg
          .lcPrmp1.Caption   = "\<Module"
          .lcPrmp2.Caption   = "E\<xternal Prg."
          .lcPrmp4.Caption   = "\<Custom Prg."
        ENDWITH 
        DO CASE
          CASE loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "M"
            loFormSet.rbSysPrg   = 1
            loFormSet.lcProgName = IIF(!EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]),;
                         ALLTRIM(LOOKUP(SYDAPPL.CAPP_NAME,;
                         ALLTRIM(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]),;
                         SYDAPPL.CAPP_ID,'CAPP_ID'))," ")
            loFormSet.lcExtPrg   = " "
            loFormSet.lcProgID = SYDAPPL.CAPP_ID
          CASE loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "E"
            loFormSet.rbSysPrg   = 2
            loFormSet.lcProgName = " "
            loFormSet.lcProgID = ' '
            loFormSet.lcExtPrg   = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath]
          CASE loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "C"
            loFormSet.rbSysPrg   = 4
            SELECT SYDOBJCT
            SET FILTER TO 
            loFormSet.lcProgName = IIF(!EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]),;
                         ALLTRIM(LOOKUP(SYDOBJCT.CPRGLNAME,;
                         ALLTRIM(loFormSet.lcApp_Id+loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]),;
                         SYDOBJCT.CAPOBJNAM,'CAPP_ID'))," ")
            loFormSet.lcExtPrg   = " "
            loFormSet.lcProgID = SYDAPPL.CAPP_ID
        ENDCASE
      
      *** If the process type of the current bar is :_ ***
      *** system program or custom program...
      CASE loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] $ "PCG" .AND. ;
           VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) <> 5

        IF loFormSet.lcApp_Id = "SY"
          WITH loFormSet.Ariaform1.rbSysPrg
            .lcPrmp1.Caption  = "System P\<rg."
            .lcPrmp2.Caption  = "System \<Bar"
            .lcPrmp4.Caption  = "\<Custom Prg."
          ENDWITH 
          loFormSet.rbSysPrg = IIF(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType]="P",1,3)
        ELSE
          WITH loFormSet.Ariaform1.rbSysPrg
            .lcPrmp1.Caption  = "System P\<rg."
            .lcPrmp2.Caption  = "E\<xternal Prg."
            .lcPrmp4.Caption  = "\<Custom Prg."
          ENDWITH 
          DO CASE
            CASE loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "P"
              loFormSet.rbSysPrg = 1
            CASE loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "G"
              loFormSet.rbSysPrg = 3
            CASE loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "C"
              loFormSet.rbSysPrg = 4
            CASE EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType])
              loFormSet.rbSysPrg = 0
          ENDCASE
        ENDIF
        
        *** Link to the module object file. ***
        SELECT SYDOBJCT
        SET FILTER TO 
        IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "G"
          loFormSet.lcProgName = IIF(!EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]),;
                       ALLTRIM(LOOKUP(SYDOBJCT.CPRGLNAME,;
                       ALLTRIM("SY"+loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]),;
                       SYDOBJCT.CAPOBJNAM,'CAPP_ID'))," ")
          loFormSet.lcProgID = SYDOBJCT.CAPOBJNAM
        ELSE
          loFormSet.lcProgName = IIF(!EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]),;
                       ALLTRIM(LOOKUP(SYDOBJCT.CPRGLNAME,;
                       ALLTRIM(loFormSet.lcApp_Id+loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]),;
                       SYDOBJCT.CAPOBJNAM,'CAPP_ID'))," ")
          loFormSet.lcProgID = SYDOBJCT.CAPOBJNAM
        ENDIF
        loFormSet.lcExtPrg   = " "
      
      *** If the process type of the current bar is a system bar. ***
      CASE loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "S"
        WITH loFormSet.Ariaform1.rbSysPrg
          .lcPrmp1.Caption   = "System P\<rg."
          .lcPrmp2.Caption   = "System \<Bar"
          .lcPrmp4.Caption   = "\<Custom Prg."
        ENDWITH 
        IF !EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId])
          lcExact = SET("EXACT")
          SET EXACT ON
          IF ASCAN(loFormSet.laBar_Com,loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]) > 0
            *** Link with the system array that ***
            *** hold the system bars prompt...
            loFormSet.lcProgName = loFormSet.laBar_Com[ASUBSCRIPT(loFormSet.laBar_Com,;
                         ASCAN(loFormSet.laBar_Com,loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]),1),1]
            loFormSet.lcProgID = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]
          ELSE
            loFormSet.lcProgName = " "
            loFormSet.lcProgID =  ' '
          ENDIF
          SET EXACT &lcExact
        ELSE
          loFormSet.lcProgName = " "
          loFormSet.lcProgID =  ' '
        ENDIF
        loFormSet.lcExtPrg  = " "
        loFormSet.rbSysPrg  = 2
      
      *** If the process type of the current bar is external. ***
      CASE loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "E"
        WITH loFormSet.Ariaform1.rbSysPrg
          .lcPrmp1.Caption    = "System P\<rg."
          .lcPrmp2.Caption    = "E\<xternal Prg."
          .lcPrmp4.Caption    = "\<Custom Prg."
        ENDWITH 
        loFormSet.lcProgName = " "
        loFormSet.lcProgID =  ' '
        loFormSet.lcExtPrg   = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath]
        loFormSet.rbSysPrg   = 2
      
      *** If the process type of the current bar is anything else. ***
      OTHERWISE
        IF loFormSet.lcApp_Id = "SY"
          WITH loFormSet.Ariaform1.rbSysPrg
            .lcPrmp1.Caption  = "System P\<rg."
            .lcPrmp2.Caption  = "System \<Bar"
            .lcPrmp4.Caption  = "\<Custom Prg."
          ENDWITH 
        ELSE
          WITH loFormSet.Ariaform1.rbSysPrg
            .lcPrmp1.Caption    = "System P\<rg."
            .lcPrmp2.Caption    = "E\<xternal Prg."
            .lcPrmp4.Caption    = "\<Custom Prg."
          ENDWITH 
        ENDIF
        loFormSet.lcProgName = " "
        loFormSet.lcProgID =  ' '
        loFormSet.lcExtPrg   = " "
        loFormSet.rbSysPrg   = 0
    ENDCASE
    
  ELSE
    *** If the current bar exist in the output pad. ***
    IF VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 5
      WITH loFormSet.Ariaform1.rbSysPrg
        .lcPrmp1.Caption   = "System \<Rep."
        .lcPrmp2.Caption   = "E\<xternal Rep."
        .lcPrmp4.Caption   = "\<Custom Rep."
      ENDWITH 
    ELSE
      IF loFormSet.lcApp_Id = "SY"
        *** If the current module is the main system. ***
        WITH loFormSet.Ariaform1.rbSysPrg
          .lcPrmp1.Caption    = IIF(VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 7,;
                                "\<Module","System P\<rg.")
          .lcPrmp2.Caption    = IIF(VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 7,;
                                "E\<xternal Prg.","System \<Bar")
          .lcPrmp4.Caption    = "\<Custom Prg."
        ENDWITH 
      ELSE
        WITH loFormSet.Ariaform1.rbSysPrg
          .lcPrmp1.Caption    = "System P\<rg."
          .lcPrmp2.Caption    = "E\<xternal Prg."
          .lcPrmp4.Caption    = "\<Custom Prg."
        ENDWITH 
      ENDIF
    ENDIF
    loFormSet.lcProgName = " "
    loFormSet.lcProgID =  ' '
    loFormSet.lcExtPrg   = " "
    loFormSet.rbSysPrg   = 0
    loFormSet.llSysPrg   = .F.     && You cannot access the radio button
  ENDIF  
ELSE
  loFormSet.lcProgName = " "
  loFormSet.lcProgID =  ' '
  loFormSet.lcExtPrg   = " "
  loFormSet.rbSysPrg   = 0
  loFormSet.llSysPrg   = .F.
  loFormSet.lcProgPram = " "
ENDIF

*SHOW GET loFormSet.rbSysPrg
loFormSet.puPop = loFormSet.lcProgID
*SHOW GET loFormSet.puPop

IF loFormSet.rbSysPrg = 2
  IF loFormSet.lnAryNo > 0
    IF loFormSet.lcApp_Id = "SY"
      IF VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 7
        *SHOW GETS WINDOW SMMENU2 ENABLE  ONLY
        *ACTIVATE WINDOW SMMENU2 TOP
        *SHOW GETS WINDOW SMMENU1 DISABLE ONLY
      ELSE
        *SHOW GETS WINDOW SMMENU1 ENABLE  ONLY
        *ACTIVATE WINDOW SMMENU1 TOP
        *SHOW GETS WINDOW SMMENU2 DISABLE ONLY
      ENDIF
    ELSE
      *SHOW GETS WINDOW SMMENU2 ENABLE  ONLY
      *ACTIVATE WINDOW SMMENU2 TOP
      *SHOW GETS WINDOW SMMENU1 DISABLE ONLY
    ENDIF
  ELSE
    *SHOW GETS WINDOW SMMENU1 ENABLE  ONLY
    *ACTIVATE WINDOW SMMENU1 TOP
    *SHOW GETS WINDOW SMMENU2 DISABLE ONLY
  ENDIF
ELSE
  *SHOW GETS WINDOW SMMENU1 ENABLE  ONLY
  *ACTIVATE WINDOW SMMENU1 TOP
  *SHOW GETS WINDOW SMMENU2 DISABLE ONLY
ENDIF

IF loFormSet.lnAryNo > 0
  IF LEFT(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt],2) = "\-" .OR. ;
     EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt]) .OR. ;
     AT(loFormSet.lcUpLeft,PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu))) > 0 .OR. ;
     AT(loFormSet.lcDnLeft,PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu))) > 0
    SHOW GET pbHotKey  DISABLE
    SHOW GET pbMessage DISABLE
  ELSE
    SHOW GET pbHotKey  ENABLE
    SHOW GET pbMessage ENABLE
  ENDIF
ENDIF

*** If the current module is the main system, ***
*** disable the  3rd bar of the radio  button ***
*** {Custom program} else enable it...
*SHOW GET loFormSet.rbSysPrg,1 PROMPT loFormSet.lcPrmp1
*SHOW GET loFormSet.rbSysPrg,2 PROMPT loFormSet.lcPrmp2

IF loFormSet.lcApp_Id = "SY"
  IF loFormSet.lnAryNo > 0
    IF VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 7
      loFormSet.Ariaform1.rbSysPrg.lcPrmp3.Enabled = .F.
      loFormSet.Ariaform1.rbSysPrg.lcPrmp4.Enabled = .T.
    ELSE
      loFormSet.Ariaform1.rbSysPrg.lcPrmp3.Enabled = .T.
      loFormSet.Ariaform1.rbSysPrg.lcPrmp4.Enabled = .F.
    ENDIF
  ELSE
    loFormSet.Ariaform1.rbSysPrg.lcPrmp3.Enabled = .T.
    loFormSet.Ariaform1.rbSysPrg.lcPrmp4.Enabled = .F.
  ENDIF
ELSE
  loFormSet.Ariaform1.rbSysPrg.lcPrmp3.Enabled = .T.
  loFormSet.Ariaform1.rbSysPrg.lcPrmp4.Enabled = .T.
ENDIF

lfShowpuPop(loFormSet.Ariaform1.rbSysPrg.Value<>2)
loFormSet.Ariaform1.Refresh()


lfvPrg2do(loFormSet,loFormSet.AriaForm1.rbSysPrg)
RETURN 

*!**************************************************************************
*!
*!      Function: lfvMenu
*!
*!**************************************************************************
*
*** Valid function of the list. ***

FUNCTION lfvMenu
PARAMETERS loFormSet,loFld

IF loFormSet.lsMenu = 0
  RETURN
ENDIF

*** Get all the needed information of the ***
*** current bar that I need to edit it...
lnBarNo    = GETBAR("puMenu",loFormSet.lsMenu)
lcPrompt   = PRMBAR("puMenu",lnBarNo)
loFormSet.lnAryNo    = VAL(SUBSTR(lcPrompt,RAT("+",lcPrompt)-3,3))
loFormSet.lnBarLevel = VAL(SUBSTR(lcPrompt,RAT("+",lcPrompt)+1))

IF loFormSet.lnAryNo = 0
  RETURN
ENDIF

lcWhichPad = "^"+ALLTRIM(STR(VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos])))

*** You cannot edit any separator. ***
IF ALLTRIM(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt]) = "\-"
  RETURN
ENDIF

*DO (gcScrDir + gcWinAppl + '\SMMENED.SPR')
lcRunScx = lfGetScx("SM\SMMENED.scx")
DO FORM (lcRunScx) WITH loFormSet

IF loFormSet.lnBarLevel = 0
  *** If I am editting a pad title. ***
      DEFINE BAR lnBarNo OF puMenu PROMPT ;
             loFormSet.lcHfBloc+SUBSTR(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt],1,41)+;
             IIF('\<' $ loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt],"  ","")+;
             loFormSet.lcBranch+loFormSet.lcHfBlocs+RIGHT(STR(loFormSet.lnAryNo),3)+;
             "+"+SPACE(1)
ELSE
  *** If I am editting any other bar. ***
       DEFINE BAR lnBarNo OF puMenu PROMPT ;
              loFormSet.lcHfBloc+loFormSet.lcHfBloc+loFormSet.lcVrLine+;
              IIF(loFormSet.lnBarLevel=1,"",REPLICATE("  "+loFormSet.lcVrLine,loFormSet.lnBarLevel-1))+;
              SUBSTR(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt],1,41)+;
              IIF(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ]="S",;
              IIF('\<' $ loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt],"  ","")+loFormSet.lcBranch,;
              IIF('\<' $ loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt],"   "," "))+;
              loFormSet.lcVrLine+loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+lcWhichPad+ ;
              RIGHT(STR(loFormSet.lnAryNo),3)+"+"+STR(loFormSet.lnBarLevel,1)
ENDIF

_CUROBJ = OBJNUM(loFormSet.lsMenu)

*** Flag to say that the current module need to be save. ***
loFormSet.llSaved = .F.

*!**************************************************************************
*!
*!      Function: lfvAppId
*!
*!**************************************************************************
*
*** Valid function of the popup hold all the ***
*** available modules in the system...

FUNCTION lfvAppId
PARAMETERS loFormSet,loFld

*** Save the old application. ***
lcOldApp = loFormSet.lcApp_Id

    loFormSet.lcApp_Id = loFormSet.laApp[loFormSet.ibPopApp,2]
    *SHOW GET loFormSet.ibPopApp

*** If the module is changed. ***
IF lcOldApp <> loFormSet.lcApp_Id

  =lfvSav_App(loFormSet,lcOldApp)     && Save the old module data.
  
  =lfSelData(loFormSet)              && Select the current module data.

  =lfBldBars(loFormSet)              && Build the popup for the current module data.
  
ENDIF
IF loFormSet.lcApp_Id = "EB" .OR. loFormSet.lcApp_Id= "AS" .OR. loFormSet.lcApp_Id= "UP"
  SHOW GET pbBarMod ENABLE
ELSE
  SHOW GET pbBarMod DISABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfvNewBrSp
*!
*!**************************************************************************
*
*** Valid function of the 2 push buttons:_
*** <New bar> - <New separator>...
FUNCTION lfvNewBrSp
PARAMETERS loFormSet,lnBr_or_Sp

IF loFormSet.lsMenu = 0
  *** You must mark a list item first. ***
  *** <  Ok  > ***
  = gfModalGen("TRM00008B00000","Dialog")
  RETURN
ENDIF

lnCurBar = GETBAR("puMenu",loFormSet.lsMenu)

*** Get the array element of the current bar. ***
loFormSet.lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",lnCurBar),;
           RAT("+",PRMBAR("puMenu",lnCurBar))-3,3))

IF loFormSet.lnAryNo = 0 .OR. loFormSet.lnAryNo > ALEN(loFormSet.laMenu,1)
  RETURN
ENDIF

IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.llDefault] .AND. ;
   VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 6 .AND. ;
   loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ] <> "S"
  *** You can not add any thing in the default popup. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00149B00000","DIALOG")
  RETURN
ENDIF

*** Get the level of the current bar. ***
loFormSet.lnBarLevel = VAL(SUBSTR(PRMBAR("puMenu",lnCurBar),;
             RAT("+",PRMBAR("puMenu",lnCurBar))+1))

IF loFormSet.lnBarLevel = 0
  *** You cannot add new bar outside any pad. ***

  =gfModalGen("TRM00138B00000","DIALOG","bar")
  RETURN
ENDIF

*** Hide the popup before adding any thing to it. ***
*HIDE POPUP puMenu SAVE

*** If the prompt of the current bar is empty. ***
*** No need to insert an array element, Add it ***
*** in the current array element...
IF EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt])
  loFormSet.laMenu[loFormSet.lnAryNo,1]          = loFormSet.lcApp_Id
  loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ]  = "C"
  loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt] = IIF(lnBr_or_Sp=1,"New Bar"+SPACE(34),"\-")
  loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]  = PADL(ALLTRIM(SUBSTR(PRMBAR("puMenu",lnCurBar),;
                               AT("^",PRMBAR("puMenu",lnCurBar))+1,1)),2,"0")
  DO CASE
    CASE VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 5
      loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "R"
    CASE VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 7
      loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "M"
    OTHERWISE
      loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "P"
  ENDCASE
  loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath] = ""
  loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]  = ""
  loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnBar_Msg]  = ""
  loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnHot_key]  = ""
  loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.llDefault]  = .F.
  
  loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnParamPos] = ""
  loFormSet.lcProgPram = ""
  loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnUpgrade]  = IIF(loFormSet.rbSysPrg = 4 , "U" , IIF(loFormSet.lcApp_Id = "SY" OR loFormSet.lcApp_Id = "SM" , "S" , "A"))
  
  loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnBarMod]  = ""
  
  loFormSet.rbSysPrg   = 1
  lcWhichPad = "^"+ALLTRIM(STR(VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos])))
  
  lnBarNo  = lnCurBar
  IF lnBr_or_Sp = 1
    *** If it is a bar. ***
        DEFINE BAR lnBarNo OF puMenu PROMPT ;
               loFormSet.lcHfBloc+loFormSet.lcHfBloc+loFormSet.lcVrLine+;
               IIF(loFormSet.lnBarLevel=1,"",REPLICATE("  "+loFormSet.lcVrLine,loFormSet.lnBarLevel-1))+;
               SUBSTR(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt],1,41)+;
               IIF('\<' $ loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt],"   "," ")+;
               loFormSet.lcVrLine+loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+lcWhichPad+ ;
               RIGHT(STR(loFormSet.lnAryNo),3)+"+"+;
               RIGHT(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPop_Levl],1)
  ELSE
    *** If it is a separator. ***
        DEFINE BAR lnBarNo OF puMenu PROMPT ;
               loFormSet.lcHfBloc+loFormSet.lcHfBloc+;
               IIF(loFormSet.lnBarLevel=1,"",REPLICATE(loFormSet.lcVrLine+"  ",loFormSet.lnBarLevel-1))+;
               loFormSet.lcLnLeft+REPLICATE(loFormSet.lcHzLine,42)+ ;
               loFormSet.lcLnRght+loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+;
               lcWhichPad+RIGHT(STR(loFormSet.lnAryNo),3)+"+"+;
               STR(loFormSet.lnBarLevel,1)
  ENDIF
ELSE
  *** The prompt of the current array is not empty. ***
  DIMENSION loFormSet.laMenu [ALEN(loFormSet.laMenu,1)+1,ALEN(loFormSet.laMenu,2)]
  loFormSet.laMenu [ALEN(loFormSet.laMenu,1),1]          = loFormSet.lcApp_Id
  loFormSet.laMenu [ALEN(loFormSet.laMenu,1),loFormSet.lnPop_Levl] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPop_Levl]
  loFormSet.laMenu [ALEN(loFormSet.laMenu,1),loFormSet.lnSub_Typ]  = "C"
  loFormSet.laMenu [ALEN(loFormSet.laMenu,1),loFormSet.lnSub_Prpt] = IIF(lnBr_or_Sp=1,;
                                       "New Bar"+SPACE(34),"\-")
  loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnPad_Pos]   = PADL(ALLTRIM(SUBSTR(PRMBAR("puMenu",;
                                       lnCurBar),AT("^",PRMBAR("puMenu",;
                                       lnCurBar))+1,1)),2,"0")
  DO CASE
    CASE VAL(loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnPad_Pos]) = 5
      loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnProcType] = "R"
    CASE VAL(loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnPad_Pos]) = 7
      loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnProcType] = "M"
    OTHERWISE
      loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnProcType] = "P"
  ENDCASE

  loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnProcPath]  = ""
  loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnProssId]   = ""
  loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnBar_Msg]   = ""
  loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnHot_key]   = ""
  loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.llDefault]   = .F.

  loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnParamPos] = ""
  loFormSet.lcProgPram = ""
  
  loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnUpgrade]  = IIF(loFormSet.rbSysPrg = 4 , "U" , IIF(loFormSet.lcApp_Id = "SY" OR loFormSet.lcApp_Id = "SM" , "S" , "A"))
  
  loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnBarMod]   = ""
  
  lnBarNo  = CNTBAR('puMenu')+1
  lnBefore = GETBAR('puMenu',loFormSet.lsMenu)

  lcWhichPad = "^"+ALLTRIM(STR(VAL(loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnPad_Pos])))
  
  IF lnBr_or_Sp = 1
    *** If it a bar. ***
        DEFINE BAR lnBarNo OF puMenu PROMPT ;
               loFormSet.lcHfBloc+loFormSet.lcHfBloc+loFormSet.lcVrLine+;
               IIF(loFormSet.lnBarLevel=1,"",REPLICATE("  "+loFormSet.lcVrLine,loFormSet.lnBarLevel-1))+;
               SUBSTR(loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnSub_Prpt],1,41)+;
               IIF('\<' $ loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnSub_Prpt],"   "," ")+;
               loFormSet.lcVrLine+loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+lcWhichPad+ ;
               RIGHT(STR(ALEN(loFormSet.laMenu,1)),3)+"+"+;
               RIGHT(loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnPop_Levl],1) ;
               BEFORE lnBefore
  ELSE
    *** If it is a separator. ***
        DEFINE BAR lnBarNo OF puMenu PROMPT ;
               loFormSet.lcHfBloc+loFormSet.lcHfBloc+;
               IIF(loFormSet.lnBarLevel=1,"",REPLICATE(loFormSet.lcVrLine+"  ",loFormSet.lnBarLevel-1))+;
               loFormSet.lcLnLeft+REPLICATE(loFormSet.lcHzLine,42)+ ;
               loFormSet.lcLnRght+loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+lcWhichPad+;
               RIGHT(STR(ALEN(loFormSet.laMenu,1)),3)+"+"+;
               STR(loFormSet.lnBarLevel,1) BEFORE lnBefore
  ENDIF
ENDIF

*** Flag to say that the current module need to be save. ***
loFormSet.llSaved = .F.

*** Show the popup to display the new bar or the new separator. ***
*SHOW POPUP puMenu
WITH loFormset.AriaForm1
  .puMenu.RowSource = 'puMenu'
  .puMenu.FontName = 'FoxFont'
  .Refresh()
ENDWITH   

*SHOW GET loFormSet.rbSysPrg
*** Show the list. ***
*!*	SHOW GET   loFormSet.lsMenu
*!*	_CUROBJ = OBJNUM(loFormSet.lsMenu)

*!**************************************************************************
*!
*!      Function: lfvRemBar
*!
*!**************************************************************************
*
*** Valid function of the push button <remove bar>

FUNCTION lfvRemBar
PARAMETERS loFormSet

IF loFormSet.lsMenu = 0
  *** You must mark a list item first. ***
  *** <  Ok  > ***
  = gfModalGen("TRM00008B00000","Dialog")
  RETURN
ENDIF

lnBarNo  = GETBAR('puMenu',loFormSet.lsMenu)

*** Get the array element of the current bar. ***
loFormSet.lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",lnBarNo),;
           RAT("+",PRMBAR("puMenu",lnBarNo))-3,3))

IF loFormSet.lnAryNo = 0 .OR. loFormSet.lnAryNo > ALEN(loFormSet.laMenu,1) .OR. loFormSet.lsMenu = 0
  RETURN
ENDIF

IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.llDefault]
  *** You can not change anything in default bars. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00148B00000","DIALOG")
  RETURN
ENDIF

*** Get the level of the current bar. ***
loFormSet.lnBarLevel  = VAL(SUBSTR(PRMBAR("puMenu",lnBarNo),;
              RAT("+",PRMBAR("puMenu",lnBarNo))+1))

IF loFormSet.lnBarLevel = 0
  *** You cannot remove any pad. ***
  =gfModalGen("TRM00139B00000","DIALOG")
  RETURN
ENDIF

*HIDE POPUP puMenu SAVE 

*** If this bar is a branched bar and has a popup. ***
IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ] = "S"
  
  *** This bar is a branching bar. ***
  *** Therefore, the popup will be removed. ***
  *** < Remove >  -  < Remove All >  -  < Cancel > ***
  lnOption = gfModalGen("TRM00132B00024","ALERT")

  DO CASE
    
    *** Remove the branching bar & its popup. ***
    CASE lnOption = 1
      *** Flag to say that the current ***
      *** module need to be save. ***
      loFormSet.llSaved = .F.
      =lfRemove(loFormSet.lsMenu,.F.)
    *** Remove all the nesting  popups ***
    *** related to this branching bar. ***
    CASE lnOption = 2
      *** Flag to say that the current ***
      *** module need to be save. ***
      loFormSet.llSaved = .F.
      =lfRemove(loFormSet.lsMenu,.T.)
    *** Cancel the whole removing. ***
    CASE lnOption = 3
      RETURN
  ENDCASE
ELSE
  *** Flag to say that the current ***
  *** module need to be save. ***
  loFormSet.llSaved = .F.

  IF !EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]) .AND. loFormSet.lcApp_Id <> "SY"
    IF !EMPTY(loFormSet.laProc2Del[1])
      DIMENSION loFormSet.laProc2Del[ALEN(loFormSet.laProc2Del,1)+1]
      =AINS(loFormSet.laProc2Del,1)
    ENDIF
    loFormSet.laProc2Del[1] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]
  ENDIF

  IF AT(loFormSet.lcUpLeft,PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu-1))) > 0 .AND.;
     AT(loFormSet.lcDnLeft,PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu+1))) > 0
    
    *** If last element in the popup, replace the prompt ***
    *** text with blank instead of removing it...
    loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt] = SPACE(41)
    loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ ] = "C"
    loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = ""
        DEFINE BAR lnBarNo OF puMenu PROMPT ;
               loFormSet.lcHfBloc+loFormSet.lcHfBloc+loFormSet.lcVrLine+;
               IIF(loFormSet.lnBarLevel=1,"",REPLICATE("  "+loFormSet.lcVrLine,loFormSet.lnBarLevel-1))+;
               SUBSTR(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt],1,41)+;
               IIF('\<' $ loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt],"   "," ")+;
               loFormSet.lcVrLine+loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+ ;
               RIGHT(STR(loFormSet.lnAryNo),3)+"+"+STR(loFormSet.lnBarLevel,1)
  ELSE
    *** Move a bar from the popup. ***
    RELEASE BAR lnBarNo OF puMenu
  ENDIF
ENDIF

*SHOW POPUP puMenu
WITH loFormset.AriaForm1
  .puMenu.RowSource = 'puMenu'
  .puMenu.FontName = 'FoxFont'
  .Refresh()
ENDWITH   

SHOW GET   loFormSet.lsMenu
_CUROBJ = OBJNUM(loFormSet.lsMenu)

*!**************************************************************************
*!
*!      Function: lfvRemove
*!
*!**************************************************************************
*
*** This funcion is called from the valid function of the remove bar. ***

FUNCTION lfRemove
PARAMETERS lnList_Val,llRem_All

PRIVATE lnList_Val,llRem_All,lnCurLevel,lcLstPrmpt,lnLstLevel,lnLstAryNo

*** Get the current level of the branching ***
*** bar we want to remove. ***
lnCurLevel = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",lnList_Val)),;
             RAT("+",PRMBAR("puMenu",GETBAR("puMenu",lnList_Val)))+1))

lnOldAryNo = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",lnList_Val)),;
             RAT("+",PRMBAR("puMenu",GETBAR("puMenu",lnList_Val)))-3,3))

*** Loop from the element we want to remove ***
*** till the end of the popup. ***
DO WHILE VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",lnList_Val+2)),;
         RAT("+",PRMBAR("puMenu",GETBAR("puMenu",lnList_Val+2)))+1));
          > lnCurLevel .AND. ;
         AT(loFormSet.lcDnLeft,PRMBAR("puMenu",GETBAR("puMenu",lnList_Val+2))) = 0

  *** Get the prompt of the current bar. ***
  lcLstPrmpt = PRMBAR("puMenu",GETBAR("puMenu",lnList_Val+2))
  
  *** Get the level of branching of the current bar. ***
  lnLstLevel = VAL(SUBSTR(lcLstPrmpt,RAT("+",lcLstPrmpt)+1))
  
  *** Get the array element no. of the current bar. ***
  lnLstAryNo = VAL(SUBSTR(lcLstPrmpt,RAT("+",lcLstPrmpt)-3,3))
  
  *** If this bar is an element from the array ***
  *** that hold all the menu elements ,  so we ***
  *** have to remove it from the array.
  IF lnLstAryNo > 0
    IF loFormSet.laMenu[lnLstAryNo,loFormSet.lnSub_Typ] = "S"
      *** The function will call its self again if ***
      *** the current bar is also a branching bar. ***
      IF llRem_All
        =lfRemove(lnList_Val+2,.T.)
      ELSE
        *** This bar is a branching bar. ***
        *** Therefore, the popup will be removed. ***
        *** < Remove >  -  < Remove All >  -  < Cancel > ***
        lnOption = gfModalGen("TRM00132B00024","ALERT")
        DO CASE
          CASE lnOption = 1
            =lfRemove(lnList_Val+2,.F.)
          CASE lnOption = 2
            =lfRemove(lnList_Val+2,.T.)
          CASE lnOption = 3
            RETURN
        ENDCASE
      ENDIF
    ENDIF
  ENDIF

  IF !EMPTY(loFormSet.laMenu[lnLstAryNo,loFormSet.lnProssId]) .AND. loFormSet.lcApp_Id <> "SY"
    IF !EMPTY(loFormSet.laProc2Del[1])
      DIMENSION loFormSet.laProc2Del[ALEN(loFormSet.laProc2Del,1)+1]
      =AINS(loFormSet.laProc2Del,1)
    ENDIF
    loFormSet.laProc2Del[1] = loFormSet.laMenu[lnLstAryNo,loFormSet.lnProssId]
  ENDIF
  
  *** Remove the bar from the popup. ***
  RELEASE BAR GETBAR("puMenu",lnList_Val+2) OF puMenu
ENDDO

*** Release opening popup, closing popup & the shadow. ***
FOR lnCont6 = 1 TO 3
  RELEASE BAR GETBAR("puMenu",lnList_Val+1) OF puMenu
ENDFOR

IF !EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]) .AND. loFormSet.lcApp_Id <> "SY"
  IF !EMPTY(loFormSet.laProc2Del[1])
    DIMENSION loFormSet.laProc2Del[ALEN(loFormSet.laProc2Del,1)+1]
    =AINS(loFormSet.laProc2Del,1)
  ENDIF
  loFormSet.laProc2Del[1] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]
ENDIF

IF AT(loFormSet.lcUpLeft,PRMBAR("puMenu",GETBAR("puMenu",lnList_Val-1))) > 0 .AND.;
   AT(loFormSet.lcDnLeft,PRMBAR("puMenu",GETBAR("puMenu",lnList_Val+1))) > 0
    
  *** If last element in the popup, replace the prompt ***
  *** text with blank instead of removing it...
  IF lnOldAryNo <> 0 .AND. lnOldAryNo < ALEN(loFormSet.laMenu,1)
    loFormSet.laMenu[lnOldAryNo,loFormSet.lnSub_Prpt] = SPACE(41)
    loFormSet.laMenu[lnOldAryNo,loFormSet.lnSub_Typ ] = "C"
    loFormSet.laMenu[lnOldAryNo,loFormSet.lnProcType] = ""
        DEFINE BAR GETBAR("puMenu",lnList_Val) OF ;
               puMenu PROMPT loFormSet.lcHfBloc+loFormSet.lcHfBloc+loFormSet.lcVrLine+;
               IIF(lnCurLevel=1,"",REPLICATE("  "+loFormSet.lcVrLine,lnCurLevel-1))+;
               SUBSTR(loFormSet.laMenu[lnOldAryNo,loFormSet.lnSub_Prpt],1,41)+;
               IIF('\<' $ loFormSet.laMenu[lnOldAryNo,loFormSet.lnSub_Prpt],"   "," ")+;
               loFormSet.lcVrLine+loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+ ;
               RIGHT(STR(lnOldAryNo),3)+"+"+STR(lnCurLevel,1)
  ENDIF
ELSE
  *** Release the main branching bar, opening ***
  *** popup, closing popup & the shadow. ***
  RELEASE BAR GETBAR("puMenu",lnList_Val) OF puMenu
ENDIF


*!**************************************************************************
*!
*!      Function: lfvNewPop
*!
*!**************************************************************************
*
*** Valid function of the push button <New popup>

FUNCTION lfvNewPop
PARAMETERS loFormSet

IF loFormSet.lsMenu = 0
  *** You must mark a list item first. ***
  *** <  Ok  > ***
  = gfModalGen("TRM00008B00000","Dialog")
  RETURN
ENDIF

lnCurBar = GETBAR("puMenu",loFormSet.lsMenu)

*** Get the array element of the current bar. ***
loFormSet.lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",lnCurBar),;
           RAT("+",PRMBAR("puMenu",lnCurBar))-3,3))

IF loFormSet.lnAryNo = 0 .OR. loFormSet.lnAryNo > ALEN(loFormSet.laMenu,1)
  RETURN
ENDIF

IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.llDefault] .AND. ;
   VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 6 .AND. ;
   loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ] <> "S"
  *** You can not add any thing in the default popup. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00149B00000","DIALOG")
  RETURN
ENDIF

*** Get the level of the current popup. ***
loFormSet.lnBarLevel  = VAL(SUBSTR(PRMBAR("puMenu",lnCurBar),;
           RAT("+",PRMBAR("puMenu",lnCurBar))+1))

IF loFormSet.lnBarLevel = 0
  *** You cannot add new popup outside any pad. ***
  =gfModalGen("TRM00138B00000","DIALOG","popup")
  RETURN
ENDIF

IF loFormSet.lnBarLevel = 4
  *** The maximum nesting levels is 4 popups. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00137B00000","DIALOG")
  RETURN
ENDIF

*HIDE POPUP puMenu SAVE

*** If there is an empty bar, use it instead of adding new one. ***
IF EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt])
  llTempFl = .F.
  lnTempNo = loFormSet.lnAryNo
  lnBarNo  = lnCurBar
  lnBefore = GETBAR('puMenu',loFormSet.lsMenu+1)
ELSE
  llTempFl = .T.
  DIMENSION loFormSet.laMenu [ALEN(loFormSet.laMenu,1)+1,ALEN(loFormSet.laMenu,2)]
  lnTempNo = ALEN(loFormSet.laMenu,1)
  lnBarNo  = CNTBAR('puMenu')+1
  lnBefore = GETBAR('puMenu',loFormSet.lsMenu)
ENDIF
loFormSet.laMenu [lnTempNo,1]          = loFormSet.lcApp_Id
loFormSet.laMenu [lnTempNo,loFormSet.lnPad_Pos]  = PADL(ALLTRIM(SUBSTR(PRMBAR("puMenu",lnCurBar),;
                               AT("^",PRMBAR("puMenu",lnCurBar))+1,1)),2,"0")
loFormSet.laMenu [lnTempNo,loFormSet.lnPop_Levl] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPop_Levl]
loFormSet.laMenu [lnTempNo,loFormSet.lnSub_Prpt] = "New popup"+SPACE(32)
loFormSet.laMenu [lnTempNo,loFormSet.lnSub_Typ]  = "S"
loFormSet.laMenu [lnTempNo,loFormSet.lnProcType] = ""
loFormSet.laMenu [lnTempNo,loFormSet.lnProssId]  = ""
loFormSet.laMenu [lnTempNo,loFormSet.lnBar_Msg]  = ""
loFormSet.laMenu [lnTempNo,loFormSet.lnHot_key]  = ""
loFormSet.laMenu [lnTempNo,loFormSet.llDefault]  = .F.

loFormSet.laMenu [lnTempNo,loFormSet.lnParamPos] = ""
loFormSet.lcProgPram = ""

loFormSet.laMenu[lnTempNo,loFormSet.lnUpgrade]  = IIF(loFormSet.rbSysPrg = 4 , "U" , IIF(loFormSet.lcApp_Id = "SY" OR loFormSet.lcApp_Id = "SM" , "S" , "A"))

loFormSet.laMenu[lnTempNo,loFormSet.lnBarMod]  = ""

lcWhichPad = "^"+ALLTRIM(STR(VAL(loFormSet.laMenu[lnTempNo,loFormSet.lnPad_Pos])))

*** Define the title of the popup. (the branching bar). ***
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           loFormSet.lcHfBloc+loFormSet.lcHfBloc+loFormSet.lcVrLine+;
           IIF(loFormSet.lnBarLevel=1,"",REPLICATE("  "+loFormSet.lcVrLine,loFormSet.lnBarLevel-1))+;
           SUBSTR(loFormSet.laMenu[lnTempNo,loFormSet.lnSub_Prpt],1,41)+;
           IIF('\<' $ loFormSet.laMenu[lnTempNo,loFormSet.lnSub_Prpt],"  ","")+;
           loFormSet.lcBranch+loFormSet.lcVrLine+loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+lcWhichPad+ ;
           RIGHT(STR(lnTempNo),3)+"+"+;
           RIGHT(loFormSet.laMenu[lnTempNo,loFormSet.lnPop_Levl],1) ;
           BEFORE lnBefore

lnBarNo  = CNTBAR('puMenu')+1

*** Increase the level with 1. ***
loFormSet.lnBarLevel  = loFormSet.lnBarLevel + 1

*** Define the opening of the popup. ***
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           loFormSet.lcHfBloc+loFormSet.lcHfBloc+;
           REPLICATE(loFormSet.lcVrLine+"  ",loFormSet.lnBarLevel-1)+;
           loFormSet.lcUpLeft+REPLICATE(loFormSet.lcHzLine,42)+;
           loFormSet.lcUpRght+loFormSet.lcHfBlocs+lcWhichPad+"+"+STR(loFormSet.lnBarLevel,1);
           BEFORE lnBefore
    **SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.

lnBarNo  = CNTBAR('puMenu')+1

*** Define on bar in the popup. ***
DIMENSION loFormSet.laMenu [ALEN(loFormSet.laMenu,1)+1,ALEN(loFormSet.laMenu,2)]
loFormSet.laMenu [ALEN(loFormSet.laMenu,1),1]          = loFormSet.lcApp_Id
loFormSet.laMenu [ALEN(loFormSet.laMenu,1),loFormSet.lnPad_Pos]  = PADL(ALLTRIM(SUBSTR(PRMBAR("puMenu",lnCurBar),;
                                     AT("^",PRMBAR("puMenu",lnCurBar))+1,1)),2,"0")
loFormSet.laMenu [ALEN(loFormSet.laMenu,1),loFormSet.lnPop_Levl] = STR(loFormSet.lnBarLevel,2)
loFormSet.laMenu [ALEN(loFormSet.laMenu,1),loFormSet.lnSub_Prpt] = "New bar"+SPACE(34)
loFormSet.laMenu [ALEN(loFormSet.laMenu,1),loFormSet.lnSub_Typ]  = "C"
DO CASE
  CASE VAL(loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnPad_Pos]) = 5
    loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnProcType] = "R"
  CASE VAL(loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnPad_Pos]) = 7
    loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnProcType] = "M"
  OTHERWISE
    loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnProcType] = "P"
ENDCASE
loFormSet.laMenu [ALEN(loFormSet.laMenu,1),loFormSet.lnProcPath] = ""
loFormSet.laMenu [ALEN(loFormSet.laMenu,1),loFormSet.lnProssId]  = ""
loFormSet.laMenu [ALEN(loFormSet.laMenu,1),loFormSet.lnBar_Msg]  = ""
loFormSet.laMenu [ALEN(loFormSet.laMenu,1),loFormSet.lnHot_key]  = ""
loFormSet.laMenu [ALEN(loFormSet.laMenu,1),loFormSet.llDefault]  = .F.

loFormSet.laMenu [ALEN(loFormSet.laMenu,1),loFormSet.lnParamPos] = ""
loFormSet.lcProgPram = ""

loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnUpgrade]  = IIF(loFormSet.rbSysPrg = 4 , "U" , IIF(loFormSet.lcApp_Id = "SY" OR loFormSet.lcApp_Id = "SM" , "S" , "A"))

loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnBarMod]  = ""

loFormSet.rbSysPrg = 1

    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           loFormSet.lcHfBloc+loFormSet.lcHfBloc+loFormSet.lcVrLine+;
           IIF(loFormSet.lnBarLevel=1,"",REPLICATE("  "+loFormSet.lcVrLine,loFormSet.lnBarLevel-1))+;
           SUBSTR(loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnSub_Prpt],1,41)+;
           IIF('\<' $ loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnSub_Prpt],"   "," ")+;
           loFormSet.lcVrLine+loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+lcWhichPad+ ;
           RIGHT(STR(ALEN(loFormSet.laMenu,1)),3)+"+"+;
           RIGHT(loFormSet.laMenu[ALEN(loFormSet.laMenu,1),loFormSet.lnPop_Levl],1) ;
           BEFORE lnBefore

lnBarNo  = CNTBAR('puMenu')+1

*** Define the closing of the popup. ***
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           loFormSet.lcHfBloc+loFormSet.lcHfBloc+;
           REPLICATE(loFormSet.lcVrLine+"  ",loFormSet.lnBarLevel-1)+;
           loFormSet.lcDnLeft+REPLICATE(loFormSet.lcHzLine,42)+loFormSet.lcDnRght+;
           loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+lcWhichPad+"+"+;
           STR(loFormSet.lnBarLevel,1) BEFORE lnBefore
    **SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.

*** Flag to say that the current ***
*** module need to be saved. ***
loFormSet.llSaved = .F.

*SHOW POPUP puMenu
WITH loFormset.AriaForm1
  .puMenu.RowSource = 'puMenu'
  .puMenu.FontName = 'FoxFont'
  .Refresh()
ENDWITH   

*SHOW GET loFormSet.rbSysPrg
*SHOW GET loFormSet.lsMenu
_CUROBJ = OBJNUM(loFormSet.lsMenu)

*!**************************************************************************
*!
*!      Function: lfvMovDn
*!
*!**************************************************************************
*
*** Valid function of the push button <Move down>. ***

FUNCTION lfvMovDn
PARAMETERS loFormSet

IF loFormSet.lsMenu  = 0
  RETURN
ENDIF

lnCBarNo   = GETBAR("puMenu",loFormSet.lsMenu)
lcCPrompt  = PRMBAR("puMenu",lnCBarNo)
lnCAryNo   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)-3,3)))

IF lnCAryNo = 0
  RETURN
ENDIF

lnCLevel   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)+1)))
lnCrLevel  = lnCLevel
lcCBarType = loFormSet.laMenu[lnCAryNo,loFormSet.lnSub_Typ]

lnHotPos = AT('\<',loFormSet.laMenu[lnCAryNo,loFormSet.lnSub_Prpt])
IF lnHotPos > 0
  lcWithout = ALLTRIM(STRTRAN(loFormSet.laMenu[lnCAryNo,loFormSet.lnSub_Prpt],'\<',''))
  lcCPrompt = STRTRAN(lcCPrompt,lcWithout,ALLTRIM(loFormSet.laMenu[lnCAryNo,;
              loFormSet.lnSub_Prpt]))
ENDIF

IF lcCBarType = 'S'
  lnFBarNo   = GETBAR("puMenu",loFormSet.lsMenu+1)
  lcFPrompt  = PRMBAR("puMenu",lnFBarNo)
  lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
  lnMenu     = 2
  lnFBarNo   = 1 
    
  DO WHILE lnFLevel > lnCLevel
    lnMenu     = lnMenu +1
    lnFBarNo   = GETBAR("puMenu",loFormSet.lsMenu+lnMenu)
    lcFPrompt  = PRMBAR("puMenu",lnFBarNo)
    lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
  ENDDO

  lnFAryNo   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)-3,3)))
  IF lnFAryNo > 0
    lcNBarType = loFormSet.laMenu[lnFAryNo,loFormSet.lnSub_Typ]
  ELSE 
    _CUROBJ = OBJNUM(loFormSet.lsMenu)
    RETURN
  ENDIF
ELSE
  lcNPrompt  = PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu+1))
  lnNAryNo   = INT(VAL(SUBSTR(lcNPrompt,RAT("+",lcNPrompt)-3,3)))
  lnNLevel   = INT(VAL(SUBSTR(lcNPrompt,RAT("+",lcNPrompt)+1)))
  IF lnNAryNo > 0
    lcNBarType = loFormSet.laMenu[lnNAryNo,loFormSet.lnSub_Typ]
  ELSE
    _CUROBJ = OBJNUM(loFormSet.lsMenu)
    RETURN
  ENDIF  
ENDIF

*HIDE POPUP puMenu SAVE

DO CASE
  *** Current bar is normal and next is normal
  CASE lcCBarType $ 'CB' .AND. lcNBarType $ 'CB'
    
    *** Redefine the 2 bars 
    
    lnHotPos = AT('\<',loFormSet.laMenu[lnNAryNo,loFormSet.lnSub_Prpt])
    IF lnHotPos > 0
      lcWithout = ALLTRIM(STRTRAN(loFormSet.laMenu[lnNAryNo,loFormSet.lnSub_Prpt],'\<',''))
      lcNPrompt = STRTRAN(lcNPrompt,lcWithout,ALLTRIM(loFormSet.laMenu[lnNAryNo,;
                  loFormSet.lnSub_Prpt]))
    ENDIF

    DEFINE BAR GETBAR("puMenu",loFormSet.lsMenu)  ;
           OF puMenu PROMPT lcNPrompt
    DEFINE BAR GETBAR("puMenu",loFormSet.lsMenu+1);
           OF puMenu PROMPT lcCPrompt
 
    loFormSet.lsMenu  = loFormSet.lsMenu + 1  
  
  *** Current bar is normal and next is branching
  CASE lcCBarType $ 'CB' .AND. lcNBarType = 'S'

    lnCBarNo   = GETBAR("puMenu",loFormSet.lsMenu)
    lcFPrompt  = PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu+2))
    lnFAryNo   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)-3,3)))
    lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
    lnMenu     = 2
    lnFBarNo   = 1 
    
    DO WHILE lnFLevel > lnCLevel
      lnMenu     = lnMenu +1
      lnFBarNo   = GETBAR("puMenu",loFormSet.lsMenu+lnMenu)
      lcFPrompt  = PRMBAR("puMenu",lnFBarNo)
      lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
    ENDDO

    DEFINE BAR GETBAR("puMenu",loFormSet.lsMenu) OF puMenu PROMPT lcCPrompt ;
           BEFORE lnFBarNo 

    loFormSet.lsMenu = loFormSet.lsMenu + (lnMenu-1)
  
  *** Current bar is branching and next is normal
  CASE lcCBarType = 'S'  .AND. lcNBarType $ 'CB'

    lnHotPos = AT('\<',loFormSet.laMenu[lnFAryNo,loFormSet.lnSub_Prpt])
    IF lnHotPos > 0
      lcWithout = ALLTRIM(STRTRAN(loFormSet.laMenu[lnFAryNo,loFormSet.lnSub_Prpt],'\<',''))
      lcFPrompt = STRTRAN(lcFPrompt,lcWithout,ALLTRIM(loFormSet.laMenu[lnFAryNo,;
                  loFormSet.lnSub_Prpt]))
    ENDIF

    DEFINE BAR lnFBarNo OF puMenu PROMPT lcFPrompt ;
           BEFORE lnCBarNo 
    loFormSet.lsMenu = loFormSet.lsMenu + 1

  *** Current bar is branching and next is branching
  CASE lcCBarType = 'S'  .AND. lcNBarType = 'S'

    lnABarNo   = GETBAR("puMenu",loFormSet.lsMenu+(lnMenu-1))
    lcAPrompt  = PRMBAR("puMenu",lnABarNo)
    lnALevel   = INT(VAL(SUBSTR(lcAPrompt,RAT("+",lcAPrompt)+1)))
    lnABarNo   = 1 
    
    DO WHILE lnALevel > lnCLevel
      lnMenu     = lnMenu +1
      lnABarNo   = GETBAR("puMenu",loFormSet.lsMenu+lnMenu)
      lcAPrompt  = PRMBAR("puMenu",lnABarNo)
      lnALevel   = INT(VAL(SUBSTR(lcAPrompt,RAT("+",lcAPrompt)+1)))
    ENDDO

    DEFINE BAR lnCBarNo OF puMenu PROMPT lcCPrompt ;
           BEFORE lnABarNo 
    
    lnCBarNo   = GETBAR("puMenu",loFormSet.lsMenu)
    lcCPrompt  = PRMBAR("puMenu",lnCBarNo)
    lnCAryNo   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)-3,3)))
    lnCLevel   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)+1)))
  
    DO WHILE lnCLevel > lnCrLevel 

      lcSkipStat = IIF(SKPBAR('puMenu',lnCBarNo),'\','')
      IF lnCAryNo > 0
        lnHotPos = AT('\<',loFormSet.laMenu[lnCAryNo,loFormSet.lnSub_Prpt])
        IF lnHotPos > 0
          lcWithout = ALLTRIM(STRTRAN(loFormSet.laMenu[lnCAryNo,loFormSet.lnSub_Prpt],'\<',''))
          lcCPrompt = STRTRAN(lcCPrompt,lcWithout,ALLTRIM(loFormSet.laMenu[lnCAryNo,;
                      loFormSet.lnSub_Prpt]))
        ENDIF
      ENDIF  

      DEFINE BAR lnCBarNo OF puMenu PROMPT lcSkipStat+lcCPrompt ;
                 BEFORE lnABarNo 

      lnMenu     = lnMenu - 1
      lnCBarNo   = GETBAR("puMenu",loFormSet.lsMenu)
      lcCPrompt  = PRMBAR("puMenu",lnCBarNo)
      lnCAryNo   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)-3,3)))
      lnCLevel   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)+1)))
    ENDDO

    loFormSet.lsMenu = loFormSet.lsMenu + (lnMenu-1)

ENDCASE

*** Flag to say that the current ***
*** module need to be saved. ***
loFormSet.llSaved = .F.

*SHOW POPUP pumenu
WITH loFormset.AriaForm1
  .puMenu.RowSource = 'puMenu'
  .puMenu.FontName = 'FoxFont'
  .puMenu.ListIndex = loFormSet.lsMenu
  .Refresh()
ENDWITH   

SHOW GET   loFormSet.lsMenu
_CUROBJ = OBJNUM(loFormSet.lsMenu)

lnNewBar = loFormSet.lsMenu
RETURN 

*!**************************************************************************
*!
*!      Function: lfvMovUp
*!
*!**************************************************************************
*
*** Valid function of the push button <Move up>. ***

FUNCTION lfvMovUp
PARAMETERS loFormSet

IF loFormSet.lsMenu  = 0
  RETURN
ENDIF

lnCBarNo   = GETBAR("puMenu",loFormSet.lsMenu)
lcCPrompt  = PRMBAR("puMenu",lnCBarNo)
lnCAryNo   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)-3,3)))

IF lnCAryNo = 0
  RETURN
ENDIF

lnCLevel   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)+1)))
lnCrLevel  = lnCLevel
lcCBarType = loFormSet.laMenu[lnCAryNo,loFormSet.lnSub_Typ]

lnHotPos = AT('\<',loFormSet.laMenu[lnCAryNo,loFormSet.lnSub_Prpt])
IF lnHotPos > 0
  lcWithout = ALLTRIM(STRTRAN(loFormSet.laMenu[lnCAryNo,loFormSet.lnSub_Prpt],'\<',''))
  lcCPrompt = STRTRAN(lcCPrompt,lcWithout,ALLTRIM(loFormSet.laMenu[lnCAryNo,;
              loFormSet.lnSub_Prpt]))
ENDIF

IF lcCBarType = 'S'
  lnFBarNo   = GETBAR("puMenu",loFormSet.lsMenu-1)
  lcFPrompt  = PRMBAR("puMenu",lnFBarNo)
  lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
  lnMenu     = 2
  lnFBarNo   = 1 
    
  DO WHILE lnFLevel > lnCLevel
    lnMenu     = lnMenu +1
    lnFBarNo   = GETBAR("puMenu",loFormSet.lsMenu-lnMenu)
    lcFPrompt  = PRMBAR("puMenu",lnFBarNo)
    lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
  ENDDO

  lnFAryNo   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)-3,3)))
  IF lnFAryNo > 0
    lcNBarType = loFormSet.laMenu[lnFAryNo,loFormSet.lnSub_Typ]
  ELSE 
    IF lnFLevel > lnCLevel
      lcNBarType = "S" 
    ELSE
      _CUROBJ = OBJNUM(loFormSet.lsMenu)
      RETURN
    ENDIF
  ENDIF
ELSE
  lcNPrompt  = PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu-1))
  lnNAryNo   = INT(VAL(SUBSTR(lcNPrompt,RAT("+",lcNPrompt)-3,3)))
  lnNLevel   = INT(VAL(SUBSTR(lcNPrompt,RAT("+",lcNPrompt)+1)))
  IF lnNAryNo > 0
    lcNBarType = loFormSet.laMenu[lnNAryNo,loFormSet.lnSub_Typ]
  ELSE
    IF lnNLevel > lnCLevel
      lcNBarType = "S" 
    ELSE
      _CUROBJ = OBJNUM(loFormSet.lsMenu)
      RETURN
    ENDIF
  ENDIF  
ENDIF

*HIDE POPUP puMenu SAVE

DO CASE
  *** Current bar is normal and next is normal
  CASE lcCBarType $ 'CB' .AND. lcNBarType $ 'CB'
    
    *** Redefine the 2 bars 
    
    lnHotPos = AT('\<',loFormSet.laMenu[lnNAryNo,loFormSet.lnSub_Prpt])
    IF lnHotPos > 0
      lcWithout = ALLTRIM(STRTRAN(loFormSet.laMenu[lnNAryNo,loFormSet.lnSub_Prpt],'\<',''))
      lcNPrompt = STRTRAN(lcNPrompt,lcWithout,ALLTRIM(loFormSet.laMenu[lnNAryNo,;
                  loFormSet.lnSub_Prpt]))
    ENDIF

    DEFINE BAR GETBAR("puMenu",loFormSet.lsMenu)  ;
           OF puMenu PROMPT lcNPrompt
    DEFINE BAR GETBAR("puMenu",loFormSet.lsMenu-1);
           OF puMenu PROMPT lcCPrompt
 
    loFormSet.lsMenu  = loFormSet.lsMenu - 1
  
  *** Current bar is normal and next is branching
  CASE lcCBarType $ 'CB' .AND. lcNBarType = 'S'

    lnCBarNo   = GETBAR("puMenu",loFormSet.lsMenu)
    lcFPrompt  = PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu-2))
    lnFAryNo   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)-3,3)))
    lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
    lnMenu     = 2
    lnFBarNo   = 1 
    
    DO WHILE lnFLevel > lnCLevel
      lnMenu     = lnMenu +1
      lnFBarNo   = GETBAR("puMenu",loFormSet.lsMenu-lnMenu)
      lcFPrompt  = PRMBAR("puMenu",lnFBarNo)
      lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
    ENDDO

    DEFINE BAR GETBAR("puMenu",loFormSet.lsMenu) OF puMenu PROMPT lcCPrompt ;
           BEFORE lnFBarNo 

    loFormSet.lsMenu = loFormSet.lsMenu - lnMenu
  
  *** Current bar is branching and next is normal
  CASE lcCBarType = 'S'  .AND. lcNBarType $ 'CB'

    lnHotPos = AT('\<',loFormSet.laMenu[lnFAryNo,loFormSet.lnSub_Prpt])
    IF lnHotPos > 0
      lcWithout = ALLTRIM(STRTRAN(loFormSet.laMenu[lnFAryNo,loFormSet.lnSub_Prpt],'\<',''))
      lcCPrompt = STRTRAN(lcFPrompt,lcWithout,ALLTRIM(loFormSet.laMenu[lnFAryNo,;
                  loFormSet.lnSub_Prpt]))
    ENDIF

    lnCBarNo   = GETBAR("puMenu",loFormSet.lsMenu)
    lcFPrompt  = PRMBAR("puMenu",GETBAR("puMenu",loFormSet.lsMenu+2))
    lnFAryNo   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)-3,3)))
    lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
    lnMenu     = 2
    lnFBarNo   = 1 
    
    DO WHILE lnFLevel > lnCLevel
      lnMenu     = lnMenu +1
      lnFBarNo   = GETBAR("puMenu",loFormSet.lsMenu+lnMenu)
      lcFPrompt  = PRMBAR("puMenu",lnFBarNo)
      lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
    ENDDO

    DEFINE BAR GETBAR("puMenu",loFormSet.lsMenu-1) OF puMenu PROMPT lcCPrompt ;
           BEFORE lnFBarNo 

    loFormSet.lsMenu = loFormSet.lsMenu - 1

  *** Current bar is branching and next is branching
  CASE lcCBarType = 'S'  .AND. lcNBarType = 'S'

    DEFINE BAR lnCBarNo OF puMenu PROMPT lcCPrompt ;
           BEFORE lnFBarNo

    lnCBarNo   = GETBAR("puMenu",loFormSet.lsMenu+1)
    lcCPrompt  = PRMBAR("puMenu",lnCBarNo)
    lnCAryNo   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)-3,3)))
    lnCLevel   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)+1)))

    lnMenu = 1
    DO WHILE lnCLevel > lnCrLevel 

      lcSkipStat = IIF(SKPBAR('puMenu',lnCBarNo),'\','')
      IF lnCAryNo > 0
        lnHotPos = AT('\<',loFormSet.laMenu[lnCAryNo,loFormSet.lnSub_Prpt])
        IF lnHotPos > 0
          lcWithout = ALLTRIM(STRTRAN(loFormSet.laMenu[lnCAryNo,loFormSet.lnSub_Prpt],'\<',''))
          lcCPrompt = STRTRAN(lcCPrompt,lcWithout,ALLTRIM(loFormSet.laMenu[lnCAryNo,;
                      loFormSet.lnSub_Prpt]))
        ENDIF
      ENDIF  

      DEFINE BAR lnCBarNo OF puMenu PROMPT lcSkipStat+lcCPrompt ;
                 BEFORE lnFBarNo

      lnMenu     = lnMenu + 1
      lnCBarNo   = GETBAR("puMenu",loFormSet.lsMenu+lnMenu)
      lcCPrompt  = PRMBAR("puMenu",lnCBarNo)
      lnCAryNo   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)-3,3)))
      lnCLevel   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)+1)))
    ENDDO

    loFormSet.lsMenu = loFormSet.lsMenu - (lnMenu - 2)

ENDCASE

*** Flag to say that the current ***
*** module need to be saved. ***
loFormSet.llSaved = .F.

*SHOW POPUP pumenu
WITH loFormset.AriaForm1
  .puMenu.RowSource = 'puMenu'
  .puMenu.FontName = 'FoxFont'
  .puMenu.ListIndex = loFormSet.lsMenu
  .Refresh()
ENDWITH   

SHOW GET   loFormSet.lsMenu
_CUROBJ = OBJNUM(loFormSet.lsMenu)

lnNewBar = loFormSet.lsMenu

*!**************************************************************************
*!
*!      Function: lfSelData
*!
*!**************************************************************************
*
*** Function called at the beginning of the program ***
*** and  everytime we change the module to  collect ***
*** the current module data to be displayed in  the ***
*** popup...

FUNCTION lfSelData
PARAMETERS loFormSet

gcWorkDir = oAriaApplication.WorkDir
**loFormSet.lcMenu_Tmp = loFormSet.lcMenu_Tmp

DECLARE loFormSet.laMenu[1,ALEN(loFormSet.laMenu,2)]
loFormSet.laMenu       = " " 
    SET ENGINEBEHAVIOR 70
SELECT CAPP_ID,CPAD_POS,CPOP_POS,CBAR_POS,;
       CPOP_LEVL,CSUB_PRPT,CSUB_TYP,CSUB_CTG,;
       CSUB_POS,CMSTR_NAM,CSUB_MSG,CSUB_HKEY,;
       CPROSS_ID,CUPGRDLVL,CPROCTYPE,CPROCPATH,LDEFAULT,cMenuParam,cBarModule;
       FROM (gcWorkDir+loFormSet.lcMenu_Tmp) ;
       WHERE CAPP_ID+CPAD_POS+CPOP_POS+CPOP_LEVL+CBAR_POS = loFormSet.lcApp_Id ;
       ORDER BY CAPP_ID,CPAD_POS,CPOP_POS,CPOP_LEVL,CBAR_POS,cBarModule ;
       INTO ARRAY loFormSet.laMenu
    SET ENGINEBEHAVIOR 90
    
loFormSet.lnPad_Pos  = 2           && Col no. of pad pos
loFormSet.lnPop_Pos  = 3           && Col no. of popup pos
loFormSet.lnBar_Pos  = 4           && Col no. of bar pos
loFormSet.lnPop_Levl = 5           && Col no. of popup level
loFormSet.lnSub_Prpt = 6           && Col no. of text prompt
loFormSet.lnSub_Typ  = 7           && Col no. of type
loFormSet.lnSub_Ctg  = 8           && Col no. of category
loFormSet.lnMstr_nam = 10          && Col no. of master name
loFormSet.lnBar_Msg  = 11          && Col no. of Bar message
loFormSet.lnHot_key  = 12          && Col no. of Bar hot key
loFormSet.lnProssId  = 13          && Col no. of prosses ID 
loFormSet.lnProcType = 15          && Col no. of process type (P,S,E,R,G,M)
loFormSet.lnProcPath = 16          && Col no. of process path
loFormSet.llDefault  = 17          && Col no. of default bar
loFormSet.lnParamPos = 18          && Col no. of program parameters

loFormSet.lnUpgrade  = 14          && Col. No. of the upgrade level

loFormSet.lnBarMod   = 19          && Col. No. of the bar Module that filled for the Edi Module almost.

*** Define the prompt of the popups if the current ***
*** module is the main system or not...
IF loFormSet.lcApp_Id = "SY"
  WITH loFormSet.Ariaform1.rbSysPrg
    .lcPrmp1.Caption   = "System P\<rg."
    .lcPrmp2.Caption   = "System \<Bar"
    .lcPrmp4.Caption   = "\<Custom Prg."
  ENDWITH 
  *SHOW GET loFormSet.rbSysPrg,1 PROMPT loFormSet.lcPrmp1
  *SHOW GET loFormSet.rbSysPrg,2 PROMPT loFormSet.lcPrmp2
  *SHOW GET loFormSet.rbSysPrg,4 PROMPT loFormSet.lcPrmp4
ELSE
  WITH loFormSet.Ariaform1.rbSysPrg
    .lcPrmp1.Caption   = "System P\<rg."
    .lcPrmp2.Caption   = "E\<xternal Prg."
    .lcPrmp4.Caption   = "\<Custom Prg."
  ENDWITH
  *SHOW GET loFormSet.rbSysPrg,1 PROMPT loFormSet.lcPrmp1
  *SHOW GET loFormSet.rbSysPrg,2 PROMPT loFormSet.lcPrmp2
  *SHOW GET loFormSet.rbSysPrg,4 PROMPT loFormSet.lcPrmp4
ENDIF

*** If there is no data in the menu file for the selected module. ***
IF EMPTY(loFormSet.laMenu[1,1])
  IF loFormSet.lcApp_Id = "SY"
    *** 18 elements to define 9 pads. ***
    DECLARE loFormSet.laMenu[19,ALEN(loFormSet.laMenu,2)]
  ELSE
    *** 6 elements to define 3 pads. ***
    DECLARE loFormSet.laMenu[6,ALEN(loFormSet.laMenu,2)]
  ENDIF
  
  lnCont8     = 1
  lnCont9     = 1

  loFormSet.laMenu      = " "
  DO WHILE .T.
    IF lnCont8 >= ALEN(loFormSet.laMenu,1)
      EXIT
    ENDIF
    
    loFormSet.laMenu[lnCont8,1]            = loFormSet.lcApp_Id
    loFormSet.laMenu[lnCont8,loFormSet.lnPad_Pos]    = IIF(loFormSet.lcApp_Id = "SY",;
                                   loFormSet.laDefSPop[lnCont9,2],loFormSet.laDefPop[lnCont9,2])
    
    loFormSet.laMenu[lnCont8,loFormSet.lnSub_Prpt]   = IIF(loFormSet.lcApp_Id = "SY",;
                                   ALLTRIM(loFormSet.laDefSPop[lnCont9,1]),;
                                   ALLTRIM(loFormSet.laDefPop[lnCont9,1]))+;
                                   SPACE(41-IIF(loFormSet.lcApp_Id = "SY",;
                                   LEN(loFormSet.laDefSPop[lnCont9,1]),;
                                   LEN(loFormSet.laDefPop[lnCont9,1])))
    loFormSet.laMenu[lnCont8,loFormSet.lnSub_Typ]    = "P"
    loFormSet.laMenu[lnCont8,loFormSet.llDefault]    = .F.
    
    loFormSet.laMenu[lnCont8+1,1]          = loFormSet.lcApp_Id
    loFormSet.laMenu[lnCont8+1,loFormSet.lnPad_Pos]  = IIF(loFormSet.lcApp_Id = "SY",;
                                   loFormSet.laDefSPop[lnCont9,2],loFormSet.laDefPop[lnCont9,2])
    loFormSet.laMenu[lnCont8+1,loFormSet.lnPop_Pos]  = "01"
    loFormSet.laMenu[lnCont8+1,loFormSet.lnBar_Pos]  = "01"
    
    IF loFormSet.lcApp_Id = "SY"
      DO CASE
        CASE VAL(loFormSet.laMenu[lnCont8+1,loFormSet.lnPad_Pos]) = 1
          loFormSet.laMenu[lnCont8+1,loFormSet.lnPop_Levl] = "01"
          loFormSet.laMenu[lnCont8+1,loFormSet.lnSub_Prpt] = "\<Exit"+SPACE(35)
          loFormSet.laMenu[lnCont8+1,loFormSet.lnSub_Typ]  = "C"
          loFormSet.laMenu[lnCont8+1,loFormSet.lnProcType] = "G"
          loFormSet.laMenu[lnCont8+1,loFormSet.lnProssId]  = "GPEXIT"
          loFormSet.laMenu[lnCont8+1,loFormSet.llDefault]  = .T.
        CASE VAL(loFormSet.laMenu[lnCont8+1,loFormSet.lnPad_Pos]) = 6
          loFormSet.laMenu[lnCont8+1,loFormSet.lnPop_Levl] = "01"
          loFormSet.laMenu[lnCont8+1,loFormSet.lnSub_Prpt] = "\<Select company"+SPACE(25)
          loFormSet.laMenu[lnCont8+1,loFormSet.lnSub_Typ]  = "S"
          loFormSet.laMenu[lnCont8+1,loFormSet.llDefault]  = .T.
          loFormSet.laMenu[lnCont8+1,loFormSet.lnProssId]  = "_COMPANIES"

          lnCont8 = lnCont8 + 1
          loFormSet.laMenu[lnCont8+1,1]          = loFormSet.lcApp_Id
          loFormSet.laMenu[lnCont8+1,loFormSet.lnPad_Pos]  = loFormSet.laDefSPop[lnCont9,2]
          loFormSet.laMenu[lnCont8+1,loFormSet.lnPop_Pos]  = "02"
          loFormSet.laMenu[lnCont8+1,loFormSet.lnBar_Pos]  = "01"
          loFormSet.laMenu[lnCont8+1,loFormSet.lnPop_Levl] = "02"
          loFormSet.laMenu[lnCont8+1,loFormSet.lnSub_Prpt] = "\<No companies available"+SPACE(17)
          loFormSet.laMenu[lnCont8+1,loFormSet.lnSub_Typ]  = "C"
          loFormSet.laMenu[lnCont8+1,loFormSet.llDefault]  = .T.
          loFormSet.laMenu[lnCont8+1,loFormSet.lnMstr_nam] = "_COMPANIES"
        OTHERWISE
          loFormSet.laMenu[lnCont8+1,loFormSet.lnPop_Levl] = "01"
          loFormSet.laMenu[lnCont8+1,loFormSet.lnSub_Prpt] = SPACE(41)
          loFormSet.laMenu[lnCont8+1,loFormSet.lnSub_Typ]  = "C"
          loFormSet.laMenu[lnCont8+1,loFormSet.llDefault]  = .F.
      ENDCASE
    ELSE
      loFormSet.laMenu[lnCont8+1,loFormSet.lnPop_Levl] = "01"
      loFormSet.laMenu[lnCont8+1,loFormSet.lnSub_Prpt] = SPACE(41)
      loFormSet.laMenu[lnCont8+1,loFormSet.lnSub_Typ]  = "C"
      loFormSet.laMenu[lnCont8+1,loFormSet.llDefault]  = .F.
    ENDIF
    
    lnCont8 = lnCont8 + 2
    lnCont9 = lnCont9 + 1
    
  ENDDO
  
  *** Flag to say that the current ***
  *** module need to be saved. ***
  loFormSet.llSaved = .F.
ENDIF

*!**************************************************************************
*!
*!      Function: lfMDrag
*!
*!**************************************************************************
*
*** Funcion called if the mouse moved inside the list up or down. ***

FUNCTION lfMDrag
PARAMETERS loFormSet

IF SYS(18)='loFormSet.lsMenu'     .AND. ;
   BETWEEN(MROW(),3,15) .AND. ;
   BETWEEN(MCOL(),1,59) 

*  IF llFirstm
*    llFirstm = .F.  
*    KEYBOARD CHR(151)
*    SHOW GET loFormSet.lsMenu
*    RETURN
*  ENDIF  
*  llFirstm = .T.  

  lnCurRow = MROW()
  DO WHILE llMdown
    DO CASE
      CASE MROW() > lnCurRow 
        lnCurRow = MROW()
        =lfvMovDn(loFormSet)
        lnMouse  = INKEY(0.01,'M')
      CASE MROW() < lnCurRow 
        lnCurRow = MROW()
        =lfvMovUp(loFormSet)
        lnMouse  = INKEY(0.01,'M')
    ENDCASE
    llMdown  = MDOWN()
  ENDDO
  llMdown  = .T.
ENDIF  

*!**************************************************************************
*!
*!      Function: lfCtrDn
*!
*!**************************************************************************
*
*** Function called if press CTRL+ARROW DOWN. ***

FUNCTION lfCtrDn
PARAMETERS loFormSet

*** Call the move down function. ***
=lfvMovDn(loFormSet)

*!**************************************************************************
*!
*!      Function: lfCtrUp
*!
*!**************************************************************************
*
*** Function called if press CTRL+ARROW UP. ***

FUNCTION lfCtrUp
PARAMETERS loFormSet

*** Call the move up function. ***

=lfvMovUp(loFormSet)

*!**************************************************************************
*!
*!      Function: lfBldBars
*!
*!**************************************************************************
*
*** Funcion called at the beginning of the program and ***
*** everytime  we change the  module after calling the ***
*** select  data function.  And this function to build ***
*** the popup from the array hold the current module data...

FUNCTION lfBldBars
PARAMETERS loFormSet


    IF !loFormSet.llDontHid
      *HIDE POPUP puMenu SAVE
    ENDIF  

    lcWhichPad = ""
    lnBarNo    = 1
    loFormSet.lnBarLevel = 1
    lnNewPlvl  = 1

    llFrst_Pad  = .T.

    RELEASE BAR  ALL OF puMenu

    FOR lnCount = 1 TO ALEN(loFormSet.laMenu,1)
      lnNewPlvl  = INT(VAL(loFormSet.laMenu[lnCount,loFormSet.lnPop_Levl]))

      DO CASE
        *** New pad 
        CASE loFormSet.laMenu[lnCount,loFormSet.lnSub_Typ] = "P"

          *** If defining new pad dont close any thing from before 
          IF llFrst_Pad
            *** Turn the flag off so next time we will close previous
            *** poups
            llFrst_Pad = .F.
          ELSE
            *** If the previous pad have one or more opend popup and
            *** not closed yet close them all 
            IF loFormSet.lnBarLevel > 1
              =lfClosPops(loFormSet)
            ENDIF

            *** Close any previous pad. ***
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   loFormSet.lcHfBloc+loFormSet.lcHfBloc+loFormSet.lcDnLeft+;
                   REPLICATE(loFormSet.lcHzLine,42)+loFormSet.lcDnRght+;
                   loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+"+"+;
                   STR(loFormSet.lnBarLevel,1) COLOR RGB(0,0,0,255,255,255)

            **SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.
            
            lnBarNo = lnBarNo + 1

            DEFINE BAR lnBarNo OF puMenu PROMPT "\-"

            lnBarNo = lnBarNo + 1

          ENDIF
      
          lcWhichPad = "^"+ALLTRIM(STR(VAL(loFormSet.laMenu[lnCount,loFormSet.lnPad_Pos])))

          *** define text of the current pad. ***
          DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 loFormSet.lcHfBloc+SUBSTR(loFormSet.laMenu[lnCount,loFormSet.lnSub_Prpt],1,41)+;
                 IIF('\<' $ loFormSet.laMenu[lnCount,loFormSet.lnSub_Prpt],"  ","")+;
                 loFormSet.lcBranch+loFormSet.lcHfBlocs+lcWhichPad+RIGHT(STR(lnCount),3)+;
                 "+"+SPACE(1)
          lnBarNo = lnBarNo + 1

          *** Start the separator of the current pad. ***
          DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 loFormSet.lcHfBloc+loFormSet.lcHfBloc+loFormSet.lcUpLeft+;
                 REPLICATE(loFormSet.lcHzLine,42)+loFormSet.lcUpRght+;
                 loFormSet.lcHfBlocs+"+"+STR(loFormSet.lnBarLevel,1) ;
                 COLOR RGB(0,0,0,255,255,255)

          *SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.
          
          lnBarNo = lnBarNo + 1

          *** Branching bar. ***
        CASE loFormSet.laMenu[lnCount,loFormSet.lnSub_Typ] = "S"
          *** Check if there is any opend popup on a higher level than ***
          *** this new branching bar...
          IF lnNewPlvl < loFormSet.lnBarLevel
            =lfClosPops(loFormSet)
          ENDIF
          *** Define the branching bar with branching mark  ***
          DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 loFormSet.lcHfBloc+loFormSet.lcHfBloc+;
                 IIF(loFormSet.lnBarLevel = 1,loFormSet.lcVrLine,;
                 REPLICATE(loFormSet.lcVrLine+"  ",loFormSet.lnBarLevel-1)+loFormSet.lcVrLine)+;
                 SUBSTR(loFormSet.laMenu[lnCount,loFormSet.lnSub_Prpt],1,41) + ;
                 IIF('\<' $ loFormSet.laMenu[lnCount,loFormSet.lnSub_Prpt],"  ","")+;
                 loFormSet.lcBranch+loFormSet.lcVrLine+loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+lcWhichPad+ ;
                 RIGHT(STR(lnCount),3)+"+"+STR(loFormSet.lnBarLevel,1)
             
          lnBarNo    = lnBarNo    + 1

          loFormSet.lnBarLevel = loFormSet.lnBarLevel + 1
      
          DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 loFormSet.lcHfBloc+loFormSet.lcHfBloc+;
                 REPLICATE(loFormSet.lcVrLine+"  ",loFormSet.lnBarLevel-1)+;
                 loFormSet.lcUpLeft+REPLICATE(loFormSet.lcHzLine,42)+;
                 loFormSet.lcUpRght+loFormSet.lcHfBlocs+lcWhichPad+"+"+;
                 STR(loFormSet.lnBarLevel,1) COLOR RGB(0,0,0,255,255,255)
      
          *SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.
          
          lnBarNo = lnBarNo + 1
      
          *** Normal bar or separator. ***
        OTHERWISE

          *** Check if there is any opend popup on a higher level than ***
          *** this new branching bar...
          IF lnNewPlvl < loFormSet.lnBarLevel
            =lfClosPops(loFormSet)
          ENDIF

          IF ALLTRIM(loFormSet.laMenu[lnCount,loFormSet.lnSub_Prpt]) = "\-"
            *** Separator. ***
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   loFormSet.lcHfBloc+loFormSet.lcHfBloc+;
                   IIF(loFormSet.lnBarLevel=1,"",REPLICATE(loFormSet.lcVrLine+"  ",loFormSet.lnBarLevel-1))+;
                   loFormSet.lcLnLeft+REPLICATE(loFormSet.lcHzLine,42)+ ;
                   loFormSet.lcLnRght+loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+lcWhichPad+;
                   RIGHT(STR(lnCount),3)+"+"+;
                   STR(loFormSet.lnBarLevel,1)
          ELSE
            *** Normal bar
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   loFormSet.lcHfBloc+loFormSet.lcHfBloc+loFormSet.lcVrLine+;
                   IIF(loFormSet.lnBarLevel=1,"",REPLICATE("  "+loFormSet.lcVrLine,loFormSet.lnBarLevel-1))+;
                   SUBSTR(loFormSet.laMenu[lnCount,loFormSet.lnSub_Prpt],1,41)+;
                   IIF('\<' $ loFormSet.laMenu[lnCount,loFormSet.lnSub_Prpt],"   "," ")+;
                   loFormSet.lcVrLine+loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+lcWhichPad+ ;
                   RIGHT(STR(lnCount),3)+"+"+STR(loFormSet.lnBarLevel,1)
          ENDIF
          lnBarNo = lnBarNo + 1
      ENDCASE
    ENDFOR

    IF loFormSet.lnBarLevel > 1
      *** Call the function to close any popus. ***
      lnNewPlvl  = 1
      =lfClosPops(loFormSet)
    ENDIF

    *** Close any previous pad. ***
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           loFormSet.lcHfBloc+loFormSet.lcHfBloc+loFormSet.lcDnLeft+;
           REPLICATE(loFormSet.lcHzLine,42)+loFormSet.lcDnRght+;
           loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+lcWhichPad+"+"+;
           STR(loFormSet.lnBarLevel,1) COLOR RGB(0,0,0,255,255,255)

    *SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.
    
    loFormSet.lsMenu  = 1

    IF !loFormSet.llDontHid
      *SHOW POPUP puMenu
    ELSE
      loFormSet.llDontHid = .F.
    ENDIF  

SHOW GET loFormSet.lsMenu
_CUROBJ = OBJNUM(loFormSet.lsMenu)
*- Define control source
WITH loFormset.AriaForm1
  .puMenu.RowSource = 'puMenu'
  .puMenu.FontName = 'FoxFont'
  .Refresh()
ENDWITH   
RETURN 
*!**************************************************************************
*!
*!      Function: lfClosPops
*!
*!**************************************************************************
*
*** This funcion called from building the popup function ***
*** to close any openning popups...

FUNCTION lfClosPops
PARAMETERS loFormSet

    *** Close all the opened popups on levels ***
    *** greater than the current level. ***
    FOR lnCont1 = loFormSet.lnBarLevel TO lnNewPlvl+1  STEP - 1

      *** Incase of creating new pad while we have 1 or more ***
      *** opened popups greater than level 1 in previous pad ***
      *** Terminate the loop before level goes down to 0.
      IF lnCont1 <= 1
        EXIT
      ENDIF

      *** Close any popup. ***
      DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 loFormSet.lcHfBloc+loFormSet.lcHfBloc+;
                 REPLICATE(loFormSet.lcVrLine+"  ",loFormSet.lnBarLevel-1)+;
                 loFormSet.lcDnLeft+REPLICATE(loFormSet.lcHzLine,42)+loFormSet.lcDnRght+;
                 loFormSet.lcBlock+loFormSet.lcBlock+loFormSet.lcHfBlocs+lcWhichPad+"+"+;
                 STR(loFormSet.lnBarLevel,1) COLOR RGB(0,0,0,255,255,255)
      *SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.
      lnBarNo = lnBarNo + 1

      *** Current level is moving down till we hit the new current level. ***
      *** This variable should reach only to the new current level not below ***
      loFormSet.lnBarLevel = loFormSet.lnBarLevel - 1
    ENDFOR

*!**************************************************************************
*!
*!      Function: lfvSav_App
*!
*!**************************************************************************
*
*** Function called to save a specific application. ***
*** Parameter is application ID. ***

FUNCTION lfvSav_App
PARAMETERS loFormSet,lcApp2Sav

*** No need to save if this module ***
*** was saved before...
IF loFormSet.llSaved
  RETURN
ENDIF

*** The first bar in the main system in pad {FILE} ***
*** should not be system bar it should be a system ***
*** program...
*IF lcApp2Sav = "SY"
*ENDIF

*** This array to hold the master names of ***
*** popups of different levels...
DECLARE laMstr_Nam[6],laBarPos[6]
laMstr_Nam = " "
laBarPos   = 0

DECLARE loFormSet.laStruc[1,19]
loFormSet.laStruc    = " "
lnCont10   = 0
lnCPop_pos = 1
lnAryRow   = 1
loFormSet.lnCurLevel = 0

lnCurBpos  = 1
STORE 0 TO lnOldBpos1,lnOldBpos2,lnOldBpos3,lnOldBpos4,lnOldBpos5

lnCurSpos  = 0
STORE 0 TO lnOldSpos1,lnOldSpos2,lnOldSpos3,lnOldSpos4,lnOldSpos5

lnPross_No = 0

lcMast_nam = ""
lcPross_Id = ""

FOR lnCount = 1 TO CNTBAR("puMenu")

  *=gfThermo(CNTBAR("puMenu"),lnCount,"Saving current module to the temp. file...","")

  *** Get the phisical no. of the bar. ***
  lnBarNo  = GETBAR("puMenu",lnCount)
  
  *** Get the prompt of the bar. ***
  lcPrompt = PRMBAR("puMenu",lnBarNo)
  
  *** Get the array no. of the bar. ***
  loFormSet.lnAryNo  = INT(VAL(SUBSTR(lcPrompt,RAT("+",lcPrompt)-3,3)))
  
  *** Get the level of the current bar. ***
  loFormSet.lnBarLevel  = INT(VAL(SUBSTR(lcPrompt,RAT("+",lcPrompt)+1)))
  *E303369,1 TMI 03/31/2013 [Start] 
  lnOldSpos  = "lnOldSpos" + ALLTRIM(STR(loFormSet.lnBarLevel))
  *E303369,1 TMI 03/31/2013 [End  ] 
  
  IF loFormSet.lnAryNo <> 0
    lnCont10 = IIF(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ]="P",lnCont10+1,lnCont10)
    IF !EMPTY(loFormSet.laStruc[1,1])
      lnAryRow = ALEN(loFormSet.laStruc,1)+1
      DIMENSION loFormSet.laStruc[lnAryRow,19]
    ENDIF
    *** Get the module ID.                         *** [1] ***
    loFormSet.laStruc[lnAryRow,1] = lcApp2Sav
    
    *** Get the pad position.                      *** [2] ***
    loFormSet.laStruc[ALEN(loFormSet.laStruc,1),loFormSet.lnPad_Pos] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]
    
    *** Get the popup position. ***                *** [3] ***
    lnCPop_pos = IIF(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ]="P",0,lnCPop_pos + 1)
    loFormSet.laStruc[lnAryRow,loFormSet.lnPop_Pos] = ;
                 IIF(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ]="P",;
                 "  ",PADL(ALLTRIM(STR(lnCPop_pos)),2,"0"))
    
    *** Get the bar position. ***                  *** [4] ***
    IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ] = "P"
      loFormSet.laStruc[lnAryRow,loFormSet.lnBar_Pos] = "  "
      laBarPos = 0
    ELSE
      IF loFormSet.lnBarLevel < loFormSet.lnCurLevel
        laBarPos[loFormSet.lnCurLevel] = 0
      ENDIF
      laBarPos[loFormSet.lnBarLevel] = laBarPos[loFormSet.lnBarLevel] + 1
      IF lcApp2Sav = "SY" .AND. loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos] = "01"
        loFormSet.laStruc[lnAryRow,loFormSet.lnBar_Pos] = ALLTRIM(STR(laBarPos[loFormSet.lnBarLevel]+50))
      ELSE
        loFormSet.laStruc[lnAryRow,loFormSet.lnBar_Pos] = PADL(ALLTRIM(STR(laBarPos[loFormSet.lnBarLevel])),2,"0")
      ENDIF
    ENDIF
    
    *** Get the popup level. ***                   *** [5] ***
    loFormSet.laStruc[lnAryRow,loFormSet.lnPop_Levl] = ;
                 IIF(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ]="P","  ",;
                 PADL(ALLTRIM(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPop_Levl]),2,"0"))
    
    *** Get the text to be save as a prompt. ***   *** [6] ***
    loFormSet.laStruc[lnAryRow,loFormSet.lnSub_Prpt] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt]
    
    *** Get the type bar. ***                      *** [7] ***
    loFormSet.laStruc[lnAryRow,loFormSet.lnSub_Typ] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ]
    
    *** Get the category. ***                      *** [8] ***
    IF lcApp2Sav = "SY"
      IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos] $ "01060709"
        loFormSet.laStruc[lnAryRow,loFormSet.lnSub_Ctg] = "I"
      ELSE
        loFormSet.laStruc[lnAryRow,loFormSet.lnSub_Ctg] = "S"
      ENDIF
    ELSE
      loFormSet.laStruc[lnAryRow,loFormSet.lnSub_Ctg] = "A"
    ENDIF
    
    *** Get the sub position. ***                  *** [9] ***
    IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ] = "P"
      loFormSet.laStruc[lnAryRow,9] = "  "
      lnCurSpos = 0
    ELSE
      IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ] = "S"
        *E303369,1 TMI 03/31/2013 [Start] move up
        *lnOldSpos  = "lnOldSpos" + ALLTRIM(STR(loFormSet.lnBarLevel))
        *E303369,1 TMI 03/31/2013 [End  ] 
        &lnOldSpos = lnCurSpos
        lnCurSpos  = lnCurSpos + 2
      ELSE
        IF loFormSet.lnBarLevel < loFormSet.lnCurLevel
          lnCurSpos = &lnOldSpos + 1
        ELSE
          lnCurSpos = lnCurSpos + 1
        ENDIF
      ENDIF
      loFormSet.laStruc[lnAryRow,9] = PADL(ALLTRIM(STR(lnCurSpos)),2,"0")
    ENDIF

    *** Get the sub message. ***                   *** [11] ***
    loFormSet.laStruc[lnAryRow,11] = IIF(EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnBar_Msg])," ",;
                               loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnBar_Msg])
    
    *** Get the sub hot key. ***                   *** [12] ***
    IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ] = "P"
      loFormSet.laStruc[lnAryRow,12] = "ALT+" + ;
             IIF(AT("\<",loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt]) > 0 ,;
             SUBSTR(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt],;
             AT("\<",loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Prpt])+2,1)," ")
    ELSE
      loFormSet.laStruc[lnAryRow,12] = IIF(EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnHot_key])," ",;
                                 loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnHot_key])
    ENDIF
    
    *** Get the process ID. ***                    *** [13] ***
    IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.llDefault] .AND. VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 6
      loFormSet.laStruc[lnAryRow,13] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]
    ELSE
      IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ] = "P"
        lcPross_Id = "P"  + loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos] +;
                     "PU" + loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]
        lnPross_No = 0
      
        laMstr_Nam[1] = lcPross_Id
      ELSE
        IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ] = "S"
          lnPross_No = lnPross_No + 1

          lcPross_Id = "P" + loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos] + "PU" + ;
                       loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos] + ;
                       PADL(ALLTRIM(STR(lnPross_No)),2,"0")
        
          laMstr_Nam[loFormSet.lnBarLevel+1] = lcPross_Id
        ENDIF
      ENDIF
      loFormSet.laStruc[lnAryRow,13] = IIF(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ] $ "SP",;
                             lcPross_Id,IIF(!EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId]),;
                             loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProssId],""))
    ENDIF
    
    *** Get the master name. ***                   *** [10] ***
    IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ] = "P"
      lcMast_nam = "P" + loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]
    ELSE
      lcMast_nam = laMstr_Nam[loFormSet.lnBarLevel]
    ENDIF
    loFormSet.laStruc[lnAryRow,10] = IIF(!EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnMstr_nam]);
                           .AND. loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.llDefault],;
                           loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnMstr_nam],lcMast_nam)

    *** Get the upgrade level. ***                 *** [14] ***
    IF lcApp2Sav = "SY" .AND. ;
       loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnSub_Typ] <> "P" .AND. ;
       VAL(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]) = 7
      loFormSet.laStruc[lnAryRow,14] = "A"
    ELSE
      loFormSet.laStruc[lnAryRow,14] = IIF(lcApp2Sav = "SY" .OR. lcApp2Sav = "SM","S",loFormSet.laMenu[lnAryRow,14])
    ENDIF
    
    *** Get the process type. ***                *** [15] ***
    loFormSet.laStruc[lnAryRow,15] = IIF(EMPTY(loFormSet.laStruc[lnAryRow,13]),"",;
                               loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType])
    
    *** Get the process id path. ***             *** [16] ***
    IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "E"
      loFormSet.laStruc[lnAryRow,16] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath]
    ELSE
      IF loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcType] = "R" .AND. ;
         !EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath])
        loFormSet.laStruc[lnAryRow,16] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnProcPath]
      ELSE
        loFormSet.laStruc[lnAryRow,16] = ""
      ENDIF
    ENDIF
    
    *** Get if this bar is a default bar or not. ***     *** [17] ***
    loFormSet.laStruc[lnAryRow,17] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.llDefault]

    loFormSet.laStruc[lnAryRow,18] = loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnParamPos]
    *** Save the level of the previous bar. ***
    loFormSet.lnCurLevel = loFormSet.lnBarLevel

    loFormSet.laStruc[lnAryRow,19] = loFormSet.laMenu[loFormSet.lnAryNo,19]
  ENDIF
ENDFOR

*loFormSet.lcMenu_Tmp = loFormSet.lcMenu_Tmp
SELECT (loFormSet.lcMenu_Tmp)
IF SEEK(lcApp2Sav)
  DELETE REST WHILE cApp_ID+cPad_pos+cPop_pos+cPop_levl+cBar_pos = lcApp2Sav
ENDIF
APPEND FROM ARRAY loFormSet.laStruc ;
       FIELDS CAPP_ID,CPAD_POS,CPOP_POS,CBAR_POS,;
              CPOP_LEVL,CSUB_PRPT,CSUB_TYP,CSUB_CTG,;
              CSUB_POS,CMSTR_NAM,CSUB_MSG,;
              CSUB_HKEY,CPROSS_ID,CUPGRDLVL,CPROCTYPE,;
              CPROCPATH,LDEFAULT,cMenuParam,cBarModule

loFormSet.llSaved    = .T.

*!**************************************************************************
*!
*!      Function: lfvSave
*!
*!**************************************************************************
*
FUNCTION lfvSave
PARAMETERS loFormSet


*loFormSet.lcMenu_Tmp = loFormSet.lcMenu_Tmp

lcCurAryNo = loFormSet.lnAryNo

lcSavAlias = SELECT()
SELECT SYDREPRT
lcRpFilter = FILTER()
SET FILTER TO cVer<>"A40" 
SELECT SYDOBJCT
lcObFilter = FILTER()
SET FILTER TO

SELECT (lcSavAlias)

*** check first that there is new modification need to be updated to the 
*** temp file insted of wasting more time

=lfvSav_App(loFormSet,loFormSet.lcApp_Id)

*** Lock the master file. ***
IF gfFlock('SYCMENU',.T.)
  
  *** Get the maximam no. of records to be process. ***
  lnTotRec = RECCOUNT(loFormSet.lcMenu_Tmp)
  lnCurRec = 0
  
  *** Get the time to be save in the file for each record. ***
  lcTime = gfGetTime()

  *** Save the settings of delete status. ***
  lcDeletSav = SET("DELETE")
  
  *** Replace all the records in the master file with blank. ***
  SELECT SYCMENU
  REPLACE ALL SYCMENU.CAPP_ID WITH " " ;
          FOR CAPP_ID+CPAD_POS+CPOP_POS+CPOP_LEVL+CBAR_POS = ""
  GO TOP
  SELECT (loFormSet.lcMenu_Tmp)
  SCAN
    *** Call the thermometer function. ***
    lnCurRec = lnCurRec + 1
    *=gfThermo(lnTotRec,lnCurRec,"Update master file...","")
    
    *** Set the delete to "OFF" to see the ***
    *** deleted records in the master file ***
    SET DELETED OFF
    
    SELECT SYCMENU
    
    *** Seek for blank record to replace it with new data. ***
    IF SEEK(" ")
      
      *** Set delete to "ON" to avoid the ***
      *** deleted records in the temp. file...
      SET DELETED ON
      
      *** Save data from temp. file to memory variables. ***
      SELECT (loFormSet.lcMenu_Tmp)
      SCATTER MEMVAR MEMO
    
      *** Set the delete to "OFF" to see the ***
      *** deleted records in the master file ***
      SET DELETED OFF
      
      *** Save data from memory variables to the master file. ***
      SELECT SYCMENU
      GATHER MEMVAR
    
      *** Add audit information to the master file
      REPLACE CADD_USER WITH oAriaApplication.USER_ID   ;
              CADD_TIME WITH lcTime ;
              DADD_DATE WITH DATE()
      
      *** If the record was marked for deletion, recall this record. ***
      IF DELETED()
        RECALL
      ENDIF
    ELSE
      *** Insert new record in the master file ***
      *** from the temp. file...
      lcMenu_Tmp = loFormSet.lcMenu_Tmp
      INSERT INTO SYCMENU ;
             (CAPP_ID,CPAD_POS,CPOP_POS,CBAR_POS,CPOP_LEVL,CSUB_PRPT,;
              CSUB_TYP,CSUB_CTG,CSUB_POS,CMSTR_NAM,CSUB_MSG,;
              CSUB_HKEY,CPROSS_ID,CUPGRDLVL,CPROCTYPE,CPROCPATH,;
              LDEFAULT,CADD_USER,CADD_TIME,cBarModule,DADD_DATE) ;
      VALUES (&lcMenu_Tmp..CAPP_ID,&lcMenu_Tmp..CPAD_POS,;
              &lcMenu_Tmp..CPOP_POS,&lcMenu_Tmp..CBAR_POS,;
              &lcMenu_Tmp..CPOP_LEVL,&lcMenu_Tmp..CSUB_PRPT,;
              &lcMenu_Tmp..CSUB_TYP,&lcMenu_Tmp..CSUB_CTG,;
              &lcMenu_Tmp..CSUB_POS,&lcMenu_Tmp..CMSTR_NAM,;
              &lcMenu_Tmp..CSUB_MSG,&lcMenu_Tmp..CSUB_HKEY,;
              &lcMenu_Tmp..CPROSS_ID,&lcMenu_Tmp..CUPGRDLVL,;
              &lcMenu_Tmp..CPROCTYPE,&lcMenu_Tmp..CPROCPATH,;
              &lcMenu_Tmp..LDEFAULT,oAriaApplication.USER_ID,lcTime,&lcMenu_Tmp..cBarModule,DATE())              
    ENDIF

      SELECT SYCMENU
      DO CASE
        CASE CPROCTYPE = 'R' AND !(cApp_ID $ 'SY|SM')
          lcUpGrd = LOOKUP(SYDREPRT.CUPGRDLVL,SUBSTR(CPROSS_ID,1,8),SYDREPRT.CREP_ID,'CREP_ID')
          REPLACE CUPGRDLVL WITH IIF(EMPTY(lcUpGrd),'A',lcUpGrd)
        CASE CPROCTYPE  $ 'CP' AND !(cApp_ID $ 'SY|SM')          
          lcUpGrd = LOOKUP(SYDOBJCT.CUPGRDLVL,CPROSS_ID,SYDOBJCT.CAPOBJNAM,'CAPOBJNAM')
          REPLACE CUPGRDLVL WITH IIF(EMPTY(lcUpGrd),'A',lcUpGrd)
      ENDCASE
    
    *** Set delete to "ON" to avoid the ***
    *** deleted records in the temp. file...
    SET DELETE ON
    
    SELECT (loFormSet.lcMenu_Tmp)
    
  ENDSCAN

  *** Restore the status of delete settings. ***
  SET DELETED &lcDeletSav
  
  *** Delete all for blank records. ***
  SELECT SYCMENU
  DELETE ALL FOR CAPP_ID+CPAD_POS+CPOP_POS+CPOP_LEVL+CBAR_POS = " "

  *** Close the thermometer. ***
  IF lnTotRec > lnCurRec
    FOR lnThCount = lnCurRec TO lnTotRec
      *=gfThermo(lnTotRec,lnThCount,"Update master file...","")
    ENDFOR
  ENDIF

  *** Unlock the master file. ***
  =gfFlock('SYCMENU',.F.)
  
  *** This flag to say if the menu changed or not. ***
  loFormSet.llMen_Chng = .T.

ENDIF

loFormSet.lnAryNo = IIF(loFormSet.lnAryNo = 0 , lcCurAryNo , loFormSet.lnAryNo)

SELECT SYDREPRT
SET FILTER TO &lcRpFilter 

SELECT SYDOBJCT
SET FILTER TO &lcObFilter

SELECT (lcSavAlias)


*!**************************************************************************
*!
*!      Function: lfvClose
*!
*!**************************************************************************
FUNCTION lfvClose
PARAMETERS loFormSet

IF !loFormSet.llSaved
  *** There are changes happened in the current module. ***
  *** Do you want to save these changes ? ***
  *** <  Yes  > - <  No  > - <  Cancel  > ***
  lnOption = gfModalGen("QRM00141B00025","ALERT")
  DO CASE
    *** <  Yes  > ***
    CASE lnOption = 1
      =lfvSave(loFormSet)
    *** <  Cancel  > ***
    CASE lnOption = 3
      RETURN
  ENDCASE
ENDIF

*** Delete all the processes ID from the privilege file ***
*** according to if it was deleted from the menu or not ***
SELECT SYUUSRPR
IF !EMPTY(loFormSet.laProc2Del)
  FOR lnCount2 = 1 TO ALEN(loFormSet.laProc2Del,1)
    DELETE ALL FOR SYUUSRPR.CPROSS_ID = PADR(ALLTRIM(loFormSet.laProc2Del[lnCount2]),fsize("cPross_ID"))
  ENDFOR
ENDIF

IF loFormSet.llMen_Chng .AND. (loFormSet.lcApp_Id = "SY" .OR. loFormSet.lcApp_Id = "SM")
  *** Menu strucure was modified during this session. ***
  *** Do you wish to rebuild the menu now...
  *** <  Yes  > - <  No  > ***
  IF gfModalGen("QRM00143B00006","ALERT") = 1
*    WAIT "Rebuilding system menu...!" WINDOW NOWAIT
    =gfWait("00209","NOWAIT")
    HIDE MENU _MSYSMENU SAVE
    *RELEASE POPUPS ALL EXTENDED
    SET SYSMENU TO
    *** Call the menu builder to rebuild the intial menu. ***
    =gfMenuBld("SY","I","NOSHOW")
    *** Call the menu builder to rebuild the second menu. ***
    =gfMenuBld("SY","S","NOSHOW")
    *** Call the menu builder to rebuild the current module. ***
    =gfMenuBld(gcAct_Appl,"A")
    SHOW MENU _MSYSMENU
    WAIT CLEAR
  ENDIF
ENDIF

loFormSet.Release()

*!**************************************************************************
*!
*!      Function: lfvProgParm
*!
*!**************************************************************************
* Valid Function for Field Object Program Parameters
FUNCTION lfvProgParm
PARAMETERS loFormSet,loFld
IF loFormSet.lnAryNo <> 0
  loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnParamPos] = loFormSet.lcProgPram
  loFormSet.llSaved = .F.
ENDIF

*!*************************************************************
*! Name      : lfvBarMod
*! Developer : Hend Ghanem (HBG)
*! Date      : 12/09/2000
*! Purpose   : Valid Function of bar's Module Button
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvBarMod()
*!*************************************************************

FUNCTION lfvBarMod
PARAMETERS loFormSet
PRIVATE lnI,lcActvTag,lcbarmodul,lcActvTagm ,llNewBar 


IF loFormSet.lnAryNo = 0
 RETURN
ENDIF

*loFormSet.lcMenu_Tmp = loFormSet.lcMenu_Tmp

lcbarmodul = ""
llNewBar = TYPE('loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPop_Pos]') = "L" 
IF loFormSet.laMenu[loFormSet.lnAryNo,1] = "EB" .OR. loFormSet.laMenu[loFormSet.lnAryNo,1]= "AS" .OR. loFormSet.laMenu[loFormSet.lnAryNo,1]= "UP" .OR. loFormSet.laMenu[loFormSet.lnAryNo,1]= "NC"
  SELECT sydappl
  lcActvTag = ORDER()
  SET ORDER TO TAG Capp_id
  IF SEEK(loFormSet.laMenu[loFormSet.lnAryNo,1])
  
    DECLARE laSource[4]
  
    laSource[1] = PADL(cbarmodule,2)
    laSource[2] = SUBSTR(PADL(cbarmodule,5),4)
    laSource[3] = SUBSTR(PADL(cbarmodule,8),7)
    
    laSource[4] = SUBSTR(PADL(cbarmodule,11),10)
    
    DECLARE laTarget[1]
    IF !llNewBar
      SELECT (loFormSet.lcMenu_Tmp)
      lcActvTagm = ORDER()
      SET ORDER TO TAG Apppopbar
      IF SEEK(loFormSet.laMenu[loFormSet.lnAryNo,1]+loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]+loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPop_Pos]++;
              loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPop_Levl]+loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnBar_Pos])
        IF !EMPTY(cbarmodule)
          IF !EMPTY(PADL(cbarmodule,2))
            laTarget[1] = PADL(cbarmodule,2)
          ENDIF  
          IF !EMPTY(SUBSTR(cbarmodule,4,2))
            DIMENSION laTarget[2]
            laTarget[2] = SUBSTR(cbarmodule,4,2)
          ENDIF  
          IF !EMPTY(SUBSTR(cbarmodule,7,2))
            DIMENSION laTarget[3]
            laTarget[3] = SUBSTR(cbarmodule,7,2)
          ENDIF  
          
          IF !EMPTY(SUBSTR(cbarmodule,10,2))
            DIMENSION laTarget[4]
            laTarget[3] = SUBSTR(cbarmodule,10,2)
          ENDIF  
          
        ENDIF 
      ENDIF
      SELECT (loFormSet.lcMenu_Tmp)
      SET ORDER TO TAG lcActvTagm 
    ELSE
      IF !EMPTY(loFormSet.laMenu[loFormSet.lnAryNo,19])
        IF !EMPTY(PADL(loFormSet.laMenu[loFormSet.lnAryNo,19],2))
          laTarget[1] = PADL(loFormSet.laMenu[loFormSet.lnAryNo,19],2)
        ENDIF  
        IF !EMPTY(SUBSTR(loFormSet.laMenu[loFormSet.lnAryNo,19],4,2))
          DIMENSION laTarget[2]
          laTarget[2] = SUBSTR(loFormSet.laMenu[loFormSet.lnAryNo,19],4,2)
        ENDIF  
        IF !EMPTY(SUBSTR(loFormSet.laMenu[loFormSet.lnAryNo,19],7,2))
          DIMENSION laTarget[3]
          laTarget[3] = SUBSTR(loFormSet.laMenu[loFormSet.lnAryNo,19],7,2)
        ENDIF  
        
        IF !EMPTY(SUBSTR(loFormSet.laMenu[loFormSet.lnAryNo,19],10,2))
          DIMENSION laTarget[4]
          laTarget[4] = SUBSTR(loFormSet.laMenu[loFormSet.lnAryNo,19],10,2)
        ENDIF  
        
      ENDIF     
    ENDIF
    IF gfMover(@laSource,@laTarget,'Select Order Status',.T.,'')
      IF !EMPTY(laTarget[1])
        FOR lnI = 1 TO ALEN(laTarget,1)
          lcbarmodul = lcbarmodul+IIF(EMPTY(lcbarmodul),"",",")+ laTarget[lnI]
          loFormSet.llSaved = .F.
        ENDFOR
      ENDIF  
      IF !llNewBar  
        SELECT (loFormSet.lcMenu_Tmp)
        IF SEEK(loFormSet.laMenu[loFormSet.lnAryNo,1]+loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPad_Pos]+loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPop_Pos]++;
                loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnPop_Levl]+loFormSet.laMenu[loFormSet.lnAryNo,loFormSet.lnBar_Pos])
          REPLACE cBarModules WITH lcbarmodul 
        ENDIF
      ELSE
        loFormSet.laMenu[loFormSet.lnAryNo,19] = lcbarmodul 
      ENDIF  
    ENDIF  
    loFormSet.laMenu[loFormSet.lnAryNo,19] = lcbarmodul
  ENDIF  
  SELECT sydappl
  SET ORDER TO lcActvTag
ENDIF 
IF !EMPTY(lcbarmodul)
  IF TYPE('laTarget[1]') <> "U"
    laTarget[1] = PADL(lcbarmodul,2)
  ENDIF
  IF TYPE('laTarget[2]') <> "U"
    laTarget[2] = SUBSTR(PADL(lcbarmodul,5),4)
  ENDIF
  IF TYPE('laTarget[3]') <> "U"
    laTarget[3] = SUBSTR(PADL(lcbarmodul,8),7)
  ENDIF
  
  IF TYPE('laTarget[4]') <> "U"
    laTarget[4] = SUBSTR(PADL(lcbarmodul,11),10)
  ENDIF
ENDIF
*--End of lfvBarMod


*!*************************************************************
*! Name      : gfStripExt
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : To extract the extention from file name
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : file name with extention
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->
FUNCTION gfStripExt
PARAMETERS lcFileName
PRIVATE lnDotPos,lnTermintr

lnDotPos   = RAT(".",lcFileName)
lnTermintr = MAX(RAT("\",lcFileName),RAT(":",lcFileName))
IF lnDotPos > lnTermintr
  lcFileName = LEFT( lcFileName ,lnDotPos-1)
ENDIF
RETURN lcFileName
