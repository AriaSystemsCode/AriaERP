*:************************************************************************
*:
*: Procedure file: SMMENU.PRG 
*:                 (Menu)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*:************************************************************************
*B601052,1 Hesham El-Sheltawi 02/05/96
*B601052,1 get the upgrade level of the report or the program
*B601052,1 so that the menu bar will have the same upgrade level
*E300581,1 Hesham El-Sheltawi 02/16/97
*E300581,1 Make the menu Can access programs with paramters
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*B802583,1 AMM 09/06/1999 Fix the bug of erasing previlages when entering the menu program
*B602599,1 Reham On 11/12/1999 
*B602599,1 1- Define the array element of the program parameters to void adding .F.
*B602599,1 2- Fix the error of blanking the curret bar after selecting program 2 do.
*B602599,1 3- Detect the error of "Invalid Subscript Reference" upon saving.
*B603564,1 SSH Although you specify that this is a custom report when attaching a
*B603564,1 SSH menu bar to it you'll not find the report in the custom reports popup 
*B603564,1 SSH but you'll find it in the system reports popup.
*B603560,1 SSH There is a field cBarModule in the SycMenu is getting blanked 
*B603560,1 SSH upon saving.
*B603865,1 HBG 12/09/2000 Add Button to the screan to Select the bar's Modules
*B603865,1                in case of EDI Modules
*E038033,1 HMA 04/03/2004 Select only records belongs to A27(to split 
*E038033,1                A27 records of SYDREPRT File from A4XP records).
*:************************************************************************

*E300581,1 Hesham El-Sheltawi (Start)
*DECLARE laApp[1,2],laMenu[1,17],laDefPop[1,2],laProc2Del[1],;
        laStruc[1,17],laDefSPop[1,2],laBar_Com[1,2]

*B603560,1 SSH There is a field cBarModule in the SycMenu is getting blanked 
*B603560,1 SSH upon saving.

*DECLARE laApp[1,2],laMenu[1,18],laDefPop[1,2],laProc2Del[1],;
        laStruc[1,18],laDefSPop[1,2],laBar_Com[1,2]

DECLARE laApp[1,2],laMenu[1,19],laDefPop[1,2],laProc2Del[1],;
        laStruc[1,19],laDefSPop[1,2],laBar_Com[1,2]
*B603560,1 SSH [End]

***************************************************************************
STORE " " TO laApp,laMenu,laDefPop,laProc2Del,;
             laStruc,laDefSPop,laBar_Com

DECLARE laDefPop[3,2]
laDefPop[1,1] = "\<File"
laDefPop[1,2] = "01"
laDefPop[2,1] = "\<Transaction"
laDefPop[2,2] = "04"
laDefPop[3,1] = "\<Output"
laDefPop[3,2] = "05"

DECLARE laDefSPop[9,2]
laDefSPop[1,1] = "\<File"
laDefSPop[1,2] = "01"
laDefSPop[2,1] = "\<Edit"
laDefSPop[2,2] = "02"
laDefSPop[3,1] = "\<Record"
laDefSPop[3,2] = "03"
laDefSPop[4,1] = "\<Transaction"
laDefSPop[4,2] = "04"
laDefSPop[5,1] = "\<Output"
laDefSPop[5,2] = "05"
laDefSPop[6,1] = "\<User"
laDefSPop[6,2] = "06"
laDefSPop[7,1] = "\<Modules"
laDefSPop[7,2] = "07"
laDefSPop[8,1] = "\<Window"
laDefSPop[8,2] = "08"
laDefSPop[9,1] = "\<Help"
laDefSPop[9,2] = "09"

DECLARE laBar_Com[25,2]

laBar_Com[1,1] = "Help..."
laBar_Com[1,2] = "_MST_HELP"

laBar_Com[2,1] = "Printer Setup..."
laBar_Com[2,2] = "_MFI_SETUP"

laBar_Com[3,1] = "Print..."
laBar_Com[3,2] = "_MFI_PRINT"

laBar_Com[4,1] = "Cut"
laBar_Com[4,2] = "_MED_CUT"

laBar_Com[5,1] = "Copy"
laBar_Com[5,2] = "_MED_COPY"

laBar_Com[6,1] = "Paste"
laBar_Com[6,2] = "_MED_PASTE"

laBar_Com[7,1] = "Select All"
laBar_Com[7,2] = "_MED_SLCTA"

laBar_Com[8,1] = "Goto Line..."
laBar_Com[8,2] = "_MED_GOTO"

laBar_Com[9,1] = "Find..."
laBar_Com[9,2] = "_MED_FIND"

laBar_Com[10,1] = "Find Again"
laBar_Com[10,2] = "_MED_FINDA"

laBar_Com[11,1] = "Replace And Find Again"
laBar_Com[11,2] = "_MED_REPL"

laBar_Com[12,1] = "Replace All"
laBar_Com[12,2] = "_MED_REPLA"

laBar_Com[13,1] = "Clear"
laBar_Com[13,2] = "_MWI_CLEAR"

laBar_Com[14,1] = "Move" 
laBar_Com[14,2] = "_MWI_MOVE"

laBar_Com[15,1] = "Size"
laBar_Com[15,2] = "_MWI_SIZE"

laBar_Com[16,1] = "Zoom "
laBar_Com[16,2] = "_MWI_ZOOM"

laBar_Com[17,1] = "Zoom "
laBar_Com[17,2] = "_MWI_MIN"

laBar_Com[18,1] = "Cycle"
laBar_Com[18,2] = "_MWI_ROTAT"

laBar_Com[19,1] = "Debug"
laBar_Com[19,2] = "_MWI_DEBUG"

laBar_Com[20,1] = "Trace"
laBar_Com[20,2] = "_MWI_TRACE"

laBar_Com[21,1] = "Macros..."
laBar_Com[21,2] = "_MST_MACRO"

laBar_Com[22,1] = "Calculator"
laBar_Com[22,2] = "_MST_CALCU"

laBar_Com[23,1] = "Calendar/Diary"
laBar_Com[23,2] = "_MST_DIARY"

laBar_Com[24,1] = "Filer"
laBar_Com[24,2] = "_MST_FILER"

laBar_Com[25,1] = "View"
laBar_Com[25,2] = "_MWI_VIEW"
***************************************************************************


ON KEY LABEL LEFTMOUSE    DO lfMDrag
ON KEY LABEL CTRL+DNARROW DO lfCtrDn
ON KEY LABEL CTRL+UPARROW DO lfCtrUp

llMen_Chng = .F.
llSaved    = .T.
llDontHid  = .T.
llFirstm   = .T.
lnNewBar   =  1
llMdown    = .T.
lnAryNo    = 1
lnSysPrg   = 1
lnBarLevel = 1
lnCurLevel = 1
lnFirstBar = 1
lsMenu     = 1
lcProgName = " "
*E300581,1 Hesham El-Sheltawi (Start)
lcProgPram = " "     && Variable to hold the program Prameters
*E300581,1 Hesham El-Sheltawi (End)
lcApp_Id   = " "
lcApp_Desc = " "
laApp      = " "
lcMenu_Tmp = " "

rbSysPrg  = 1
llSysPrg  = .T.

lcExtPrg  = " "
puPop     = " "
ibPopApp  = 1
llFrsPop  = .T.

lcPrv1_Tag = ""
lcPrv2_Tag = ""
lcPrv3_Tag = ""

lcHotKey   = ""
lcMesage   = ""

DO CASE
  CASE _DOS
    *** Variables hold the charcters displayed in ***
    *** the list in case of "DOS"...
    lcSpace    = " "
    lcUpLeft   = "Ú"
    lcUpRght   = "¿"
    lcDnLeft   = "À"
    lcDnRght   = "Ù"
    lcHzLine   = "Ä"
    lcVrLine   = "³"
    lcHfBloc   = "±"
    lcHfBloc   = "°"
    lcBlock    = "Û"
    lcUpBloc   = "ß"
    lcBranch   = ""
    lcLnLeft   = "Ã"
    lcLnRght   = "´"
    lcHfBlocs  = REPLICATE(lcHfBloc,30)
  CASE _WINDOWS
    *** Variables hold the charcters displayed in ***
    *** the list in case of "WINDOWS"...
    lcSpace    = " "
    lcUpLeft   = "Ú"
    lcUpRght   = "¿"
    lcDnLeft   = "À"
    lcDnRght   = "Ù"
    lcHzLine   = "Ä"
    lcVrLine   = "³"
    lcHfBloc   = " "
    lcHfBloc   = " "
    lcBlock    = " "
    lcUpBloc   = " "
    lcBranch   = ""
    lcLnLeft   = "Ã"
    lcLnRght   = "´"
    lcHfBlocs  = REPLICATE(lcHfBloc,30)
    ibPopApp = 1
ENDCASE

*** Variables hold the column no. of the ***
*** array that hold the list data...
*E300581,1 Hesham El-Sheltawi (Start)
*E300581,1 add new variable lnParamPos to assign the array element that hold
*E300581,1 the Program Parameter
*STORE 0 TO lnPad_Pos,lnPop_Pos,lnBar_Pos,;
           lnPop_Levl,lnSub_Prpt,lnSub_Typ,;
           lnSub_Ctg,lnProssId,lnProcType,;
           lnProcPath,llDefault,lnMstr_nam,lnBar_Msg,lnHot_key
STORE 0 TO lnPad_Pos,lnPop_Pos,lnBar_Pos,;
           lnPop_Levl,lnSub_Prpt,lnSub_Typ,;
           lnSub_Ctg,lnProssId,lnProcType,;
           lnProcPath,llDefault,lnMstr_nam,lnBar_Msg,lnHot_key,lnParamPos
*E300581,1 Hesham El-Sheltawi (End)           

*B603564,1 Reham On 04/27/2000    *** Start ***
lnUpgrade  = 0           && Col. No. of the upgrade level
*B603564,1 Reham On 04/27/2000    *** End   ***

*B603560,1 Reham On 04/27/2000    *** Start ***
lnBarMod   = 0           && Col. No. of the bar Module that filled for the Edi Module almost.
*B603560,1 Reham On 04/27/2000    *** End   ***

*** The text of the 1st & 2nd prompt of the radio button. ***
lcPrmp1   = "System P\<rg."
lcPrmp2   = "System \<Bar"
lcPrmp4   = "\<Custom Prg."

DO CASE
  CASE _DOS
    *** Copy scheme 2 {scheme of the popup colors} to scheme 24. ***
    SET COLOR OF SCHEME 24 TO SCHEME 2
    *** Save the color pair of the normal text. ***
    lcColor = SCHEME(2,2)
    *** Set the color of the disable option ***
    *** to the color of the normal text...
    SET COLOR OF SCHEME 24 TO &lcColor
ENDCASE

IF !gfSetup()    
  RETURN
ENDIF  

lcMenu_Tmp = gfTempName()

*** Get the available application in the system. ***
SELECT cApp_Name,cApp_Id ;
       FROM (gcSysHome+"sydAppl") ;
       INTO ARRAY laApp ;
       ORDER BY cApp_Name

IF EMPTY(laApp[1,1])
  STORE " " TO lcApp_Id,lcApp_Desc,laApp
ELSE
  lcApp_Id   = laApp[1,2]

  DO CASE
    CASE _DOS
      lcApp_Desc = laApp[1,1]
    CASE _WINDOWS
      ibPopApp   = 1
  ENDCASE
ENDIF

*** Set filter into the module dictionary file ***
*** to all modules except "SY"...
SELECT SYDAPPL
lcPrv1_Tag = SYS(22)
SET ORDER TO TAG CAPPNAME
SET FILTER TO
SET FILTER TO CAPP_ID <> "SY"
LOCATE

*** Set filter into the report dictionary file ***
*** to the current module + "R" type...
SELECT SYDREPRT

lcPrv2_Tag = SYS(22)
SET ORDER TO TAG CREPNAME

*E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]

*SET FILTER TO
*SET FILTER TO CAPP_ID + CAPOBJTYP + CMAINENTR = ALLTRIM(lcApp_Id) + "P" + "Y" 
 SET FILTER TO CAPP_ID + CAPOBJTYP + CMAINENTR = ALLTRIM(lcApp_Id) + "P" + "Y" .AND. cVer<>"A40"

*E038033,1 HMA [END]
LOCATE

*** Set filter into the module object file ***
*** to the current module + "P" type...
SELECT SYDOBJCT
lcPrv3_Tag = SYS(22)
SET ORDER TO TAG OBJLNGNAM
SET FILTER TO
SET FILTER TO CAPP_ID+CAPOBJTYP=lcApp_Id+"P"
LOCATE

SELECT SYCMENU
SET ORDER TO TAG APPPOPBAR

*** Back up the master file before process anything. ***
*** And save the back up file in the work directory. ***
COPY TO &gcWorkDir.SYCMENU CDX

*** Copy to the temp.  file which will be use in the ***
*** running of this program.  This temp file will be ***
*** use to update the master file during saving...
COPY TO &gcWorkDir.&lcMenu_Tmp CDX

SELECT 0
USE &gcWorkDir.&lcMenu_Tmp EXCLUSIVE
SET ORDER TO TAG APPPOPBAR

*** Get data for the current module from the menu file. ***
=lfSelData()

DO CASE
  CASE _DOS
    DEFINE POPUP puMenu RELATIVE SCROLL	MARK lcHfBloc COLOR SCHEME 24
    DEFINE BAR 1 OF puMenu PROMPT " "
  CASE _WINDOWS
    DEFINE POPUP puMenu RELATIVE SCROLL	MARK lcHfBloc
    DEFINE BAR 1 OF puMenu PROMPT " "
ENDCASE

*** Build the popup that has the drawing menu. ***
=lfBldBars()

*E300683,4 Call *.SPR from screens directory
* DO SMMENU.SPR 
DO (gcScrDir + gcWinAppl + '\SMMENU.SPR')
*E300683,4 end  
RELEASE POPUP puMenu
RELEASE POPUP PUPOP1
RELEASE POPUP PUPOP2
RELEASE POPUP PUPOP3

*** Clear all the filters.  & modify the last indices. ***
SELECT SYDAPPL
SET ORDER TO TAG &lcPrv1_Tag
SET FILTER TO

SELECT SYDREPRT
SET ORDER TO TAG &lcPrv2_Tag

*E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]

*SET FILTER TO
SET FILTER TO cVer<>"A40"
*E038033,1 HMA  [END]
SELECT SYDOBJCT
SET ORDER TO TAG &lcPrv3_Tag
SET FILTER TO

*** Erase the temp. files. ***
IF USED(lcMenu_Tmp)
  USE IN &lcMenu_Tmp
ENDIF  

ERASE (gcWorkDir+lcMenu_Tmp+'.DBF')
ERASE (gcWorkDir+lcMenu_Tmp+'.CDX')
ERASE (gcWorkDir+lcMenu_Tmp+'.FPT')

*** Erase the Back up MENU file. ***
lcFile = gcWorkDir + "SYCMENU.DBF"
IF FILE(lcFile)
  ERASE (gcWorkDir+"SYCMENU.DBF")
  ERASE (gcWorkDir+"SYCMENU.CDX")
ENDIF

ON KEY

*!**************************************************************************
*!
*!      Function: lfShow
*!
*!**************************************************************************
*
FUNCTION lfShow

IF lsMenu <> 0
  *** Get the array no. of the current bar. ***
  lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",lsMenu)),;
             RAT("+",PRMBAR("puMenu",GETBAR("puMenu",lsMenu)))-3,3))
ENDIF

IF lnAryNo > 0
  IF LEFT(laMenu[lnAryNo,lnSub_Prpt],2) = "\-" .OR. ;
     EMPTY(laMenu[lnAryNo,lnSub_Prpt]) .OR. ;
     AT(lcUpLeft,PRMBAR("puMenu",GETBAR("puMenu",lsMenu))) > 0 .OR. ;
     AT(lcDnLeft,PRMBAR("puMenu",GETBAR("puMenu",lsMenu))) > 0
    SHOW GET pbHotKey  DISABLE
    SHOW GET pbMessage DISABLE
  ELSE
    SHOW GET pbHotKey  ENABLE
    SHOW GET pbMessage ENABLE
  ENDIF
ENDIF

IF rbSysPrg = 2
  IF lnAryNo > 0
    IF lcApp_Id = "SY"
      IF VAL(laMenu[lnAryNo,lnPad_Pos]) = 7

        *** Disable the screen objects that has ***
        *** {The program to do}...
        *** Enable & activate the screen objects ***
        *** that has {Get external program}...
        SHOW GETS WINDOW SMMENU2 ENABLE  ONLY
        ACTIVATE WINDOW SMMENU2 TOP
        SHOW GETS WINDOW SMMENU1 DISABLE ONLY
      ELSE
        *** Disable the screen objects that has ***
        *** {Get external program}...
        *** Enable & activate the screen objects ***
        *** that has {The program to do}...
        SHOW GETS WINDOW SMMENU1 ENABLE  ONLY
        ACTIVATE WINDOW SMMENU1 TOP
        SHOW GETS WINDOW SMMENU2 DISABLE ONLY
      ENDIF
    ELSE
      *** Disable the screen objects that has ***
      *** {Get external program}...
      *** Enable & activate the screen objects ***
      *** that has {The program to do}...
      SHOW GETS WINDOW SMMENU2 ENABLE  ONLY
      ACTIVATE WINDOW SMMENU2 TOP
      SHOW GETS WINDOW SMMENU1 DISABLE ONLY
    ENDIF
  ENDIF
ELSE
  *** Disable the screen objects that has ***
  *** {Get external program}...
  *** Enable & activate the screen objects ***
  *** that has {The program to do}...
  SHOW GETS WINDOW SMMENU1 ENABLE  ONLY
  ACTIVATE WINDOW SMMENU1 TOP
  SHOW GETS WINDOW SMMENU2 DISABLE ONLY
ENDIF

*** If the current module is the main system , Disable ***
*** the radio button 3 {Custom program} else enable it...
IF lcApp_Id = "SY"
  IF lnAryNo > 0
    IF VAL(laMenu[lnAryNo,lnPad_Pos]) = 7
      SHOW GET rbSysPrg,3 DISABLE
      SHOW GET rbSysPrg,4 ENABLE
    ELSE
      SHOW GET rbSysPrg,3 ENABLE
      SHOW GET rbSysPrg,4 DISABLE
    ENDIF
  ELSE
    SHOW GET rbSysPrg,3 ENABLE
    SHOW GET rbSysPrg,4 DISABLE
  ENDIF
ELSE
  SHOW GET rbSysPrg,3 ENABLE
  SHOW GET rbSysPrg,4 ENABLE
ENDIF

*** Activate the screen that has the list on top. ***
ACTIVATE WINDOW SMMENU0 TOP

*!**************************************************************************
*!
*!      Function: lfvClrLink
*!
*!**************************************************************************
*
*** Valid function of the push button pbClrLink. { Clear Link } ***

FUNCTION lfvClrLink

IF lsMenu <> 0
  *** Get the array no. of the current bar. ***
  lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",lsMenu)),;
             RAT("+",PRMBAR("puMenu",GETBAR("puMenu",lsMenu)))-3,3))

  IF lnAryNo <> 0
    IF laMenu[lnAryNo,llDefault]
      *** You can not change anything in default bars. ***
      *** <  Ok  > ***
      =gfModalGen("TRM00148B00000","DIALOG")
      RETURN
    ENDIF
    
    laMenu[lnAryNo,lnProcType] = ""
    laMenu[lnAryNo,lnProcPath] = ""
    
    IF !EMPTY(laMenu[lnAryNo,lnProssId]) .AND. lcApp_Id <> "SY"
      IF !EMPTY(laProc2Del[1])
        DIMENSION laProc2Del[ALEN(laProc2Del,1)+1]
        =AINS(laProc2Del,1)
      ENDIF
      laProc2Del[1] = laMenu[lnAryNo,lnProssId]
    ENDIF
    
    laMenu[lnAryNo,lnProssId]  = ""
    lcProgName = " "
    lcExtPrg   = " "
    rbSysPrg   = 0
    SHOW GET rbSysPrg
    DO CASE
      CASE _DOS
        =lfRefresh()
      CASE _WINDOWS
        puPop = " "
        SHOW GET puPop
    ENDCASE
    
    *** Flag to say that the current ***
    *** module need to be save...
    llSaved = .F.

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

IF lsMenu <> 0
  *** Get the array no. of the current bar. ***
  lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",lsMenu)),;
             RAT("+",PRMBAR("puMenu",GETBAR("puMenu",lsMenu)))-3,3))

  IF lnAryNo <> 0
    
    *** Clear the trapped keys. ***
    ON KEY
    
    *** You cannot edit the hot key of a pad. ***
    lcHotStat = IIF(laMenu[lnAryNo,lnSub_Typ] = "P","DISABLE","ENABLE")
    
    lcHotKey  = laMenu[lnAryNo,lnHot_key]
    *E300683,4 Call *.SPR from screens directory
    * DO SMHOTKY.SPR 
    DO (gcScrDir + gcWinAppl + '\SMHOTKY.SPR')
    *E300683,4 end  
    *** Trap the previous key again
    ON KEY LABEL LEFTMOUSE    DO lfMDrag
    ON KEY LABEL CTRL+DNARROW DO lfCtrDn
    ON KEY LABEL CTRL+UPARROW DO lfCtrUp

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

laMenu[lnAryNo,lnHot_key]  = lcHotKey

*** Flag to say that the current ***
*** module need to be save...
llSaved = .F.

*!**************************************************************************
*!
*!      Function: lfvMessage
*!
*!**************************************************************************
*
*** Valid function of the push button pbMessage. { Message } ***

FUNCTION lfvMessage

IF lsMenu <> 0
  *** Get the array no. of the current bar. ***
  lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",lsMenu)),;
             RAT("+",PRMBAR("puMenu",GETBAR("puMenu",lsMenu)))-3,3))

  IF lnAryNo <> 0
    
    *** Clear the trapped keys
    ON KEY
    
    lcMesage = laMenu[lnAryNo,lnBar_Msg]
    *E300683,4 Call *.SPR from screens directory
    * DO SMMESAG.SPR 
    DO (gcScrDir + gcWinAppl + '\SMMESAG.SPR')
    *E300683,4 end  
    *** Trap the previous keys again. ***
    ON KEY LABEL LEFTMOUSE    DO lfMDrag
    ON KEY LABEL CTRL+DNARROW DO lfCtrDn
    ON KEY LABEL CTRL+UPARROW DO lfCtrUp

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

laMenu[lnAryNo,lnBar_Msg]  = lcMesage

*** Flag to say that the current ***
*** module need to be save...
llSaved = .F.

*!**************************************************************************
*!
*!      Function: lfwSysPrg
*!
*!**************************************************************************
*
*** When function of the radio button rbSysPrg. ***

FUNCTION lfwSysPrg

lnSysPrg = rbSysPrg
RETURN llSysPrg
 
*!**************************************************************************
*!
*!      Function: lfvSysPrg
*!
*!**************************************************************************
*
*** Valid function of the radio button rbSysPrg. ***

FUNCTION lfvSysPrg

IF lsMenu <> 0 .AND. lnSysPrg <> rbSysPrg
  *** Get the array no. of the current bar. ***
  lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",lsMenu)),;
             RAT("+",PRMBAR("puMenu",GETBAR("puMenu",lsMenu)))-3,3))

  IF lnAryNo <> 0
    IF laMenu[lnAryNo,llDefault]
      *** You can not change anything in default bars. ***
      *** <  Ok  > ***
      =gfModalGen("TRM00148B00000","DIALOG")
      rbSysPrg = lnSysPrg
      SHOW GET rbSysPrg
      RETURN
    ENDIF
    
    *B603564,1 Reham On 04/27/2000   *** Begin ***
    *B603564,1 1- If the value of radio button is custom, the upgrade levl will be "U"
    *B603564,1 2- The upgrade level will be "S" for main system or system manager
    *B603564,1 3- otherwise, it will be "A" for all the other modules.
    laMenu[lnAryNo,lnUpgrade]  = IIF(rbSysPrg = 4 , "U" , IIF(lcApp_Id = "SY" OR lcApp_Id = "SM" , "S" , "A"))
    *B603564,1 Reham On 04/27/2000   *** End   ***
    
    DO CASE
      CASE rbSysPrg = 1
        DO CASE
          CASE VAL(laMenu[lnAryNo,lnPad_Pos]) = 5
            *** If current bar is calling a report. ***
            laMenu[lnAryNo,lnProcType] = "R"
            laMenu[lnAryNo,lnProcPath] = ""
            laMenu[lnAryNo,lnSub_Typ]  = "C"
            laMenu[lnAryNo,lnProssId]  = ""
            lcProgName = " "
            lcExtPrg   = " "
          CASE lcApp_Id = "SY" .AND. ;
               VAL(laMenu[lnAryNo,lnPad_Pos]) = 7
            *** If current bar is calling a module. ***
            laMenu[lnAryNo,lnProcType] = "M"
            laMenu[lnAryNo,lnProcPath] = ""
            laMenu[lnAryNo,lnSub_Typ]  = "C"
            laMenu[lnAryNo,lnProssId]  = ""
            lcProgName = " "
            lcExtPrg   = " "
          OTHERWISE
            *** If current bar is calling a program. ***
            laMenu[lnAryNo,lnProcType] = "P"
            laMenu[lnAryNo,lnProcPath] = ""
            laMenu[lnAryNo,lnSub_Typ]  = "C"
            laMenu[lnAryNo,lnProssId]  = ""
            lcProgName = " "
            lcExtPrg   = " "
        ENDCASE
      CASE rbSysPrg = 2
        IF lcApp_Id = "SY"
          *** If current bar is calling a system bar. ***
          laMenu[lnAryNo,lnProcType] = "S"
          laMenu[lnAryNo,lnProcPath] = ""
          laMenu[lnAryNo,lnSub_Typ]  = "B"
          laMenu[lnAryNo,lnProssId]  = ""
          lcProgName = " "
          lcExtPrg   = " "
        ELSE
          IF VAL(laMenu[lnAryNo,lnPad_Pos]) = 5
            *** If current is calling an external report. ***
            laMenu[lnAryNo,lnProcType] = "R"
            laMenu[lnAryNo,lnProcPath] = ""
            laMenu[lnAryNo,lnSub_Typ]  = "C"
            laMenu[lnAryNo,lnProssId]  = ""
            lcProgName = " "
            lcExtPrg   = " "
          ELSE
            *** If current is calling an external program. ***
            laMenu[lnAryNo,lnProcType] = "E"
            laMenu[lnAryNo,lnProcPath] = ""
            laMenu[lnAryNo,lnSub_Typ]  = "C"
            laMenu[lnAryNo,lnProssId]  = ""
            lcProgName = " "
            lcExtPrg   = " "
          ENDIF
        ENDIF
      CASE rbSysPrg = 3
        *** If current bar is calling a global function or procedure. ***
        laMenu[lnAryNo,lnProcType] = "G"
        laMenu[lnAryNo,lnProcPath] = ""
        laMenu[lnAryNo,lnSub_Typ]  = "C"
        laMenu[lnAryNo,lnProssId]  = ""
        lcProgName = " "
        lcExtPrg   = " "
      CASE rbSysPrg = 4
        *** If current bar is calling a custom program or report. ***
        *B603564,1 SSH Change report type.
        *laMenu[lnAryNo,lnProcType] = "C"
        laMenu[lnAryNo,lnProcType] = IIF(VAL(laMenu[lnAryNo,lnPad_Pos]) = 5,"R","C")
        *B603564,1 SSH [End]
        laMenu[lnAryNo,lnProcPath] = ""
        laMenu[lnAryNo,lnSub_Typ]  = "C"
        laMenu[lnAryNo,lnProssId]  = ""
        lcProgName = " "
        lcExtPrg   = " "
    ENDCASE

    *** Flag to say that the current ***
    *** module need to be save...
    llSaved = .F.

  ENDIF
  
  *** To refresh the changed say fields. ***
  IF _DOS
    =lfRefresh()
  ENDIF

  IF rbSysPrg = 2
    IF lnAryNo > 0
      IF lcApp_Id = "SY"
        IF VAL(laMenu[lnAryNo,lnPad_Pos]) = 7
          SHOW GETS WINDOW SMMENU2 ENABLE  ONLY
          ACTIVATE WINDOW SMMENU2 TOP
          SHOW GETS WINDOW SMMENU1 DISABLE ONLY
        ELSE
          SHOW GETS WINDOW SMMENU1 ENABLE  ONLY
          ACTIVATE WINDOW SMMENU1 TOP
          SHOW GETS WINDOW SMMENU2 DISABLE ONLY
        ENDIF
      ELSE
        SHOW GETS WINDOW SMMENU2 ENABLE  ONLY
        ACTIVATE WINDOW SMMENU2 TOP
        SHOW GETS WINDOW SMMENU1 DISABLE ONLY
      ENDIF
    ENDIF
  ELSE
    SHOW GETS WINDOW SMMENU1 ENABLE  ONLY
    ACTIVATE WINDOW SMMENU1 TOP
    SHOW GETS WINDOW SMMENU2 DISABLE ONLY
  ENDIF

ENDIF

*!**************************************************************************
*!
*!      Function: lfvGetEPrg
*!
*!**************************************************************************
*
*** Valid function of the push button pbGetEPrg. ***

FUNCTION lfvGetEPrg

*** Save the previous external program or report. ***
lcOldProg = lcExtPrg

IF lsMenu <> 0
  *** Get the array element of the current bar
  lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",lsMenu)),;
             RAT("+",PRMBAR("puMenu",GETBAR("puMenu",lsMenu)))-3,3))
 
  IF lnAryNo <> 0

    *** Save the current default
    lcDefSav = SET("DEFAULT")
    
    *** Choose a frx, fxp, prg, app as an external program or report. ***
    lcExtPrg  = GETFILE('FRX;FXP;PRG;APP','Select external program','Select',1)

    *** Restore the previous defaults
    SET DEFAULT TO &lcDefSav
    
    *** If choose nothing, restore the old value. ***
    lcExtPrg  = IIF(EMPTY(lcExtPrg),lcOldProg,lcExtPrg)

    *** Put the process type in the array element. ***
    laMenu[lnAryNo,lnProcType] = IIF(laMenu[lnAryNo,lnProcType]="R","R","E")

    *** Put the process id path in the array element. ***
    laMenu[lnAryNo,lnProcPath] = lcExtPrg

    *** Put the process id in the array element. ***
    IF !EMPTY(laMenu[lnAryNo,lnProssId]) .AND. lcApp_Id <> "SY"
      IF !EMPTY(laProc2Del[1])
        DIMENSION laProc2Del[ALEN(laProc2Del,1)+1]
        =AINS(laProc2Del,1)
      ENDIF
      laProc2Del[1] = laMenu[lnAryNo,lnProssId]
    ENDIF
    laMenu[lnAryNo,lnProssId]  = ;
                           GFSTRIPEXT(SUBSTR(LCEXTPRG,RAT('\',LCEXTPRG)+1))
    *** Flag to say that the current ***
    *** module need to be save...
    llSaved   = .F.
    
  ENDIF
  
ENDIF

SHOW GET lcExtPrg

*!**************************************************************************
*!
*!      Function: lfvPrg2do
*!
*!**************************************************************************
*
FUNCTION lfvPrg2do

*** Get the array element of the current bar. ***
lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",lsMenu)),;
              RAT("+",PRMBAR("puMenu",GETBAR("puMenu",lsMenu)))-3,3))

IF lnAryNo > 0
  
  IF laMenu[lnAryNo,llDefault]
    *** You can not change anything in default bars. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00148B00000","DIALOG")
    RETURN
  ENDIF

  IF rbSysPrg = 0
    *** You have to specify the process type ***
    *** for this bar before linking. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00212B00000","DIALOG")
    RETURN
  ENDIF
  
  *** If the current bar is a normal bar ***
  *** and it's not disable...
  IF laMenu[lnAryNo,lnSub_Typ] $ "CB" .AND.;
     LEFT(laMenu[lnAryNo,lnSub_Prpt],2) <> "\-"

    *** If the process id type not system. ***
    IF laMenu[lnAryNo,lnProcType] <> "S" .AND. ;
       laMenu[lnAryNo,lnProcType] <> "M"
      
      *** If pad positon of the current par  say ***
      *** it is output  pad , select the  report ***
      *** dictionary file else select the module ***
      *** object file...
      IF VAL(laMenu[lnAryNo,lnPad_Pos]) = 5 .AND. ;
         laMenu[lnAryNo,lnProcType] <> "G"
        SELECT SYDREPRT

        *B603564,1 SSH Set filter depend on upgrade level.
        *SET FILTER TO CAPP_ID+CAPOBJTYP+CMAINENTR=lcApp_Id+laMenu[lnAryNo,lnProcType]+"Y"
        
        *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]
        
        *SET FILTER TO
        *SET FILTER TO CAPP_ID+CMAINENTR=lcApp_Id+"Y" ;
        *    .AND. IIF(laMenu[lnAryNo,14] = 'U',Cupgrdlvl='U',Cupgrdlvl$'SA')
        SET FILTER TO CAPP_ID+CMAINENTR=lcApp_Id+"Y" ;
            .AND. IIF(laMenu[lnAryNo,14] = 'U',Cupgrdlvl='U',Cupgrdlvl$'SA')  .AND.  SYDREPRT.cVer<>"A40"
        *E038033,1 HMA [END]
        
        *B603564,1 SSH Change report type.[End]
      ELSE
        SELECT SYDOBJCT
        SET FILTER TO
        IF laMenu[lnAryNo,lnProcType] = "G"
          SET FILTER TO CAPP_ID+CAPOBJTYP='SY'+laMenu[lnAryNo,lnProcType]
        ELSE
          SET FILTER TO CAPP_ID+CAPOBJTYP=lcApp_Id+laMenu[lnAryNo,lnProcType]
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
                IIF(VAL(laMenu[lnAryNo,lnPad_Pos]) = 5,;
                "reports|report dictionary","processes|process"))
        RETURN
      ENDIF
    ENDIF
    
    DO CASE
      CASE _DOS
        DO CASE
          
          *** If current pad position says it is an output pad. ***
          CASE VAL(laMenu[lnAryNo,lnPad_Pos]) = 5 .AND. ;
               laMenu[lnAryNo,lnProcType] <> "G"
            
            *** Link the popup to the report dictionary file. ***
            IF !EMPTY(laMenu[lnAryNo,lnProssId]) .AND. lcApp_Id <> "SY"
              IF !EMPTY(laProc2Del[1])
                DIMENSION laProc2Del[ALEN(laProc2Del,1)+1]
                =AINS(laProc2Del,1)
              ENDIF
              laProc2Del[1] = laMenu[lnAryNo,lnProssId]
            ENDIF
            laMenu[lnAryNo,lnProssId] = ;
                            gfActPop(10,18,18,65,'SYDREPRT',;
                            'CREP_ID','CREP_NAME',@lcProgName,gcBaseWind)
          
          *** If the process type of the current bar calling ***
          *** module, branch from the module dictionary file ***
          CASE laMenu[lnAryNo,lnProcType] = "M"
            *** Link the popup to the module dictionary file. ***
            laMenu[lnAryNo,lnProssId] = ;
                            gfActPop(10,18,18,65,'SYDAPPL',;
                            'CAPP_ID','CAPP_NAME',@lcProgName,gcBaseWind)
          
          *** If the process type of the  current bar  is ***
          *** system  propgram  or custom  program or and ***
          *** the global function or procedure bar is not ***
          *** bar is not existed in the output pad...
          CASE laMenu[lnAryNo,lnProcType] $ "PCG"
          
            *** Link the popup to the module object file. ***
            IF !EMPTY(laMenu[lnAryNo,lnProssId]) .AND. lcApp_Id <> "SY"
              IF !EMPTY(laProc2Del[1])
                DIMENSION laProc2Del[ALEN(laProc2Del,1)+1]
                =AINS(laProc2Del,1)
              ENDIF
              laProc2Del[1] = laMenu[lnAryNo,lnProssId]
            ENDIF
            laMenu[lnAryNo,lnProssId] = ;
                            gfActPop(10,18,18,65,'SYDOBJCT',;
                            'CAPOBJNAM','CPRGLNAME',@lcProgName,gcBaseWind)
          
          *** If the process type of the current pad is system bar. ***
          CASE laMenu[lnAryNo,lnProcType] = "S"
            
            *** Link the popup to the array hold the system bars. ***
            laMenu[lnAryNo,lnProssId] = ;
                 gfActPop(10,18,18,65,'laBar_Com',2,1,@lcProgName,gcBaseWind)
        
        ENDCASE
        =lfRefresh()
      CASE _WINDOWS
        DO CASE
          
          *** If current pad position says it is an output pad. ***
          CASE VAL(laMenu[lnAryNo,lnPad_Pos]) = 5 .AND. ;
               laMenu[lnAryNo,lnProcType] <> "G"
            
            *** Link the popup to the report dictionary file. ***
            PUPOP = SYDREPRT.CREP_NAME
            ACTIVATE POPUP puPop1
            IF INLIST(LASTKEY(),13,32)
              PUPOP = SYDREPRT.CREP_NAME
              IF !EMPTY(laMenu[lnAryNo,lnProssId]) .AND. lcApp_Id <> "SY"
                IF !EMPTY(laProc2Del[1])
                  DIMENSION laProc2Del[ALEN(laProc2Del,1)+1]
                  =AINS(laProc2Del,1)
                ENDIF
                laProc2Del[1] = laMenu[lnAryNo,lnProssId]
              ENDIF
              laMenu[lnAryNo,lnProssId] = SYDREPRT.CREP_ID
              *B602599,1 Reham On 11/14/1999   *** Begin ***
              *B602599,1 Fix the error of blanking the curret bar after selecting program 2 do.
              lcProgName = SYDREPRT.CREP_NAME
              *B602599,1 Reham On 11/14/1999   *** End   ***
              SHOW GET puPop
            ENDIF
            
          *** If the process type of the current bar calling ***
          *** module, branch from the module dictionary file ***
          CASE laMenu[lnAryNo,lnProcType] = "M"
            
            *** Link the popup to the module dictionary file. ***
            PUPOP = SYDAPPL.CAPP_NAME
            ACTIVATE POPUP puPop2
            IF INLIST(LASTKEY(),13,32)
              PUPOP = SYDAPPL.CAPP_NAME
              laMenu[lnAryNo,lnProssId] = SYDAPPL.CAPP_ID
              *B602599,1 Reham On 11/14/1999   *** Begin ***
              *B602599,1 Fix the error of blanking the curret bar after selecting program 2 do.
              lcProgName = SYDAPPL.CAPP_NAME
              *B602599,1 Reham On 11/14/1999   *** End   ***
              SHOW GET puPop
            ENDIF
            
          *** If the process type of the  current bar  is ***
          *** system  propgram  or custom  program or and ***
          *** the global function or procedure bar is not ***
          *** bar is not existed in the output pad...
          CASE laMenu[lnAryNo,lnProcType] $ "PCG"
          
            *** Link the popup to the module object file. ***
            PUPOP = SYDOBJCT.CPRGLNAME
            ACTIVATE POPUP puPop3
            IF INLIST(LASTKEY(),13,32)
              PUPOP = SYDOBJCT.CPRGLNAME
              IF !EMPTY(laMenu[lnAryNo,lnProssId]) .AND. lcApp_Id <> "SY"
                IF !EMPTY(laProc2Del[1])
                  DIMENSION laProc2Del[ALEN(laProc2Del,1)+1]
                  =AINS(laProc2Del,1)
                ENDIF
                laProc2Del[1] = laMenu[lnAryNo,lnProssId]
              ENDIF
              laMenu[lnAryNo,lnProssId] = SYDOBJCT.CAPOBJNAM
              *B602599,1 Reham On 11/14/1999   *** Begin ***
              *B602599,1 Fix the error of blanking the curret bar after selecting program 2 do.
              lcProgName = SYDOBJCT.CPRGLNAME
              *B602599,1 Reham On 11/14/1999   *** End   ***
              SHOW GET puPop
            ENDIF
            
          *** If the process type of the current pad is system bar. ***
          CASE laMenu[lnAryNo,lnProcType] = "S"
            
            *** Link the popup to the array hold the system bars. ***
            
            laMenu[lnAryNo,lnProssId] = ;
                 gfActPop(18.75,15.7,25.55,56.8,'laBar_Com',2,1,@puPop,gcBaseWind)
            
        ENDCASE
    ENDCASE
    IF ALLTRIM(UPPER(laMenu[lnAryNo,lnSub_Prpt])) = "NEW BAR"
      laMenu[lnAryNo,lnSub_Prpt] = ALLTRIM(lcProgName) + ;
                                      SPACE(45-LEN(ALLTRIM(lcProgName)))
      lnBarNo    = GETBAR("puMenu",lsMenu)
      lcPrompt   = PRMBAR("puMenu",lnBarNo)
      lnBarLevel = VAL(SUBSTR(lcPrompt,RAT("+",lcPrompt)+1))
      lcWhichPad = "^"+ALLTRIM(STR(VAL(laMenu[lnAryNo,lnPad_Pos])))
      DO CASE
        CASE _DOS
          DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 lcHfBloc+lcHfBloc+lcVrLine+;
                 IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
                 SUBSTR(laMenu[lnAryNo,lnSub_Prpt],1,41)+" "+;
                 lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
                 RIGHT(STR(lnAryNo),3)+"+"+STR(lnBarLevel,1) ;
                 COLOR SCHEME 24
        CASE _WINDOWS
          DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 lcHfBloc+lcHfBloc+lcVrLine+;
                 IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
                 SUBSTR(laMenu[lnAryNo,lnSub_Prpt],1,41)+" "+;
                 lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
                 RIGHT(STR(lnAryNo),3)+"+"+STR(lnBarLevel,1)
      ENDCASE
    ENDIF
    *** Flag to say that the current module need to be save. ***
    llSaved = .F.
  ENDIF  
ENDIF

*!**************************************************************************
*!
*!      Function: lfwMenu
*!
*!**************************************************************************
*
*** When function of the list lsMenu. ***

FUNCTION lfwMenu


IF lsMenu = 0
  RETURN
ENDIF



*** Get the array element of the current bar. ***
lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",GETBAR("puMenu",lsMenu)),;
              RAT("+",PRMBAR("puMenu",GETBAR("puMenu",lsMenu)))-3,3))
*E300581,1 Hesham El-Sheltawi (Start)
lcProgPram = " "
*E300581,1 Hesham El-Sheltawi (End)

IF lnAryNo > 0
*E300581,1 Hesham El-Sheltawi (Start)
lcProgPram = laMenu[lnAryNo,lnParamPos]
*E300581,1 Hesham El-Sheltawi (End)
  
  *** If the current bar is a normal bar and it's not ***
  *** a separator and its prompt is not empty...
  IF  laMenu[lnAryNo,lnSub_Typ] $ "CB"          .AND.;
      LEFT(laMenu[lnAryNo,lnSub_Prpt],2) <>"\-" .AND. ;
      !EMPTY(laMenu[lnAryNo,lnSub_Prpt])
    
    *** Flag to say that you've access to the radio button. ***
    llSysPrg = .T.
    
    DO CASE
      
      *** If the current bar exist in the output pad. ***
      CASE VAL(laMenu[lnAryNo,lnPad_Pos]) = 5
        lcPrmp1   = "System \<Rep."
        lcPrmp2   = "E\<xternal Rep."
        lcPrmp4   = "\<Custom Rep."
        
        IF EMPTY(laMenu[lnAryNo,lnProcPath])
          
          *** If it is a system report, link to the report dictionary. ***
          IF laMenu[lnAryNo,lnProcType] = "G"
            SELECT SYDOBJCT
            SET FILTER TO
            lcProgName = IIF(!EMPTY(laMenu[lnAryNo,lnProssId]),;
                         ALLTRIM(LOOKUP(SYDOBJCT.CPRGLNAME,;
                         ALLTRIM("SY"+laMenu[lnAryNo,lnProssId]),;
                         SYDOBJCT.CAPOBJNAM,'CAPP_ID'))," ")
          ELSE
            SELECT SYDREPRT
             
            *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]

            *SET FILTER TO
            SET FILTER TO SYDREPRT.cVer<>"A40" 

            *E038033,1 HMA  [END]
            lcProgName = IIF(!EMPTY(laMenu[lnAryNo,lnProssId]),;
                         ALLTRIM(LOOKUP(SYDREPRT.CREP_NAME,;
                         ALLTRIM(laMenu[lnAryNo,lnProssId]),;
                         SYDREPRT.CREP_ID,'CREP_ID'))," ")
          ENDIF
          
          lcExtPrg   = " "
          DO CASE
            CASE laMenu[lnAryNo,lnProcType] = "R"
              *B603564,1 Reham On 04/27/2000    *** Begin ***
              *B603564,1 Check if the upgrade level of the current bar is a custom report
              *B603564,1 or a standard report.
              *rbSysPrg = 1
              rbSysPrg = IIF(laMenu[lnAryNo,lnUpgrade] = "U" , 4 , 1)
              *B603564,1 Reham On 04/27/2000    *** End   ***
            CASE laMenu[lnAryNo,lnProcType] = "G"
              rbSysPrg = 3
            CASE laMenu[lnAryNo,lnProcType] = "C"
              rbSysPrg = 4
            CASE EMPTY(laMenu[lnAryNo,lnProcType])
              rbSysPrg = 0
          ENDCASE
        ELSE
          
          *** If it is an external report. ***
          lcProgName = " "
          lcExtPrg   = laMenu[lnAryNo,lnProcPath]
          rbSysPrg   = 2
        ENDIF
      
      *** If the current bar exist in the module pad. ***
      CASE VAL(laMenu[lnAryNo,lnPad_Pos]) = 7
        lcPrmp1   = "\<Module"
        lcPrmp2   = "E\<xternal Prg."
        lcPrmp4   = "\<Custom Prg."
        DO CASE
          CASE laMenu[lnAryNo,lnProcType] = "M"
            rbSysPrg   = 1
            lcProgName = IIF(!EMPTY(laMenu[lnAryNo,lnProssId]),;
                         ALLTRIM(LOOKUP(SYDAPPL.CAPP_NAME,;
                         ALLTRIM(laMenu[lnAryNo,lnProssId]),;
                         SYDAPPL.CAPP_ID,'CAPP_ID'))," ")
            lcExtPrg   = " "
          CASE laMenu[lnAryNo,lnProcType] = "E"
            rbSysPrg   = 2
            lcProgName = " "
            lcExtPrg   = laMenu[lnAryNo,lnProcPath]
          CASE laMenu[lnAryNo,lnProcType] = "C"
            rbSysPrg   = 4
            SELECT SYDOBJCT
            SET FILTER TO 
            lcProgName = IIF(!EMPTY(laMenu[lnAryNo,lnProssId]),;
                         ALLTRIM(LOOKUP(SYDOBJCT.CPRGLNAME,;
                         ALLTRIM(lcApp_Id+laMenu[lnAryNo,lnProssId]),;
                         SYDOBJCT.CAPOBJNAM,'CAPP_ID'))," ")
            lcExtPrg   = " "
        ENDCASE
      
      *** If the process type of the current bar is :_ ***
      *** system program or custom program...
      CASE laMenu[lnAryNo,lnProcType] $ "PCG" .AND. ;
           VAL(laMenu[lnAryNo,lnPad_Pos]) <> 5

        IF lcApp_Id = "SY"
          lcPrmp1  = "System P\<rg."
          lcPrmp2  = "System \<Bar"
          lcPrmp4  = "\<Custom Prg."
          rbSysPrg = IIF(laMenu[lnAryNo,lnProcType]="P",1,3)
        ELSE
          lcPrmp1  = "System P\<rg."
          lcPrmp2  = "E\<xternal Prg."
          lcPrmp4  = "\<Custom Prg."
          DO CASE
            CASE laMenu[lnAryNo,lnProcType] = "P"
              rbSysPrg = 1
            CASE laMenu[lnAryNo,lnProcType] = "G"
              rbSysPrg = 3
            CASE laMenu[lnAryNo,lnProcType] = "C"
              rbSysPrg = 4
            CASE EMPTY(laMenu[lnAryNo,lnProcType])
              rbSysPrg = 0
          ENDCASE
        ENDIF
        
        *** Link to the module object file. ***
        SELECT SYDOBJCT
        SET FILTER TO 
        IF laMenu[lnAryNo,lnProcType] = "G"
          lcProgName = IIF(!EMPTY(laMenu[lnAryNo,lnProssId]),;
                       ALLTRIM(LOOKUP(SYDOBJCT.CPRGLNAME,;
                       ALLTRIM("SY"+laMenu[lnAryNo,lnProssId]),;
                       SYDOBJCT.CAPOBJNAM,'CAPP_ID'))," ")
        ELSE
          lcProgName = IIF(!EMPTY(laMenu[lnAryNo,lnProssId]),;
                       ALLTRIM(LOOKUP(SYDOBJCT.CPRGLNAME,;
                       ALLTRIM(lcApp_Id+laMenu[lnAryNo,lnProssId]),;
                       SYDOBJCT.CAPOBJNAM,'CAPP_ID'))," ")
        ENDIF
        lcExtPrg   = " "
      
      *** If the process type of the current bar is a system bar. ***
      CASE laMenu[lnAryNo,lnProcType] = "S"
        lcPrmp1   = "System P\<rg."
        lcPrmp2   = "System \<Bar"
        lcPrmp4   = "\<Custom Prg."
        IF !EMPTY(laMenu[lnAryNo,lnProssId])
          lcExact = SET("EXACT")
          SET EXACT ON
          IF ASCAN(laBar_Com,laMenu[lnAryNo,lnProssId]) > 0
            *** Link with the system array that ***
            *** hold the system bars prompt...
            lcProgName = laBar_Com[ASUBSCRIPT(laBar_Com,;
                         ASCAN(laBar_Com,laMenu[lnAryNo,lnProssId]),1),1]
          ELSE
            lcProgName = " "
          ENDIF
          SET EXACT &lcExact
        ELSE
          lcProgName = " "
        ENDIF
        lcExtPrg  = " "
        rbSysPrg  = 2
      
      *** If the process type of the current bar is external. ***
      CASE laMenu[lnAryNo,lnProcType] = "E"
        lcPrmp1    = "System P\<rg."
        lcPrmp2    = "E\<xternal Prg."
        lcPrmp4    = "\<Custom Prg."
        lcProgName = " "
        lcExtPrg   = laMenu[lnAryNo,lnProcPath]
        rbSysPrg   = 2
      
      *** If the process type of the current bar is anything else. ***
      OTHERWISE
        IF lcApp_Id = "SY"
          lcPrmp1  = "System P\<rg."
          lcPrmp2  = "System \<Bar"
          lcPrmp4  = "\<Custom Prg."
        ELSE
          lcPrmp1    = "System P\<rg."
          lcPrmp2    = "E\<xternal Prg."
          lcPrmp4    = "\<Custom Prg."
        ENDIF
        lcProgName = " "
        lcExtPrg   = " "
        rbSysPrg   = 0
    ENDCASE
    
  ELSE
    *** If the current bar exist in the output pad. ***
    IF VAL(laMenu[lnAryNo,lnPad_Pos]) = 5
      lcPrmp1   = "System \<Rep."
      lcPrmp2   = "E\<xternal Rep."
      lcPrmp4   = "\<Custom Rep."
    ELSE
      IF lcApp_Id = "SY"
        *** If the current module is the main system. ***
        lcPrmp1    = IIF(VAL(laMenu[lnAryNo,lnPad_Pos]) = 7,;
                         "\<Module","System P\<rg.")
        lcPrmp2    = IIF(VAL(laMenu[lnAryNo,lnPad_Pos]) = 7,;
                         "E\<xternal Prg.","System \<Bar")
        lcPrmp4    = "\<Custom Prg."
      ELSE
        lcPrmp1    = "System P\<rg."
        lcPrmp2    = "E\<xternal Prg."
        lcPrmp4    = "\<Custom Prg."
      ENDIF
    ENDIF
    lcProgName = " "
    lcExtPrg   = " "
    rbSysPrg   = 0
    llSysPrg   = .F.     && You cannot access the radio button
  ENDIF  
ELSE
  lcProgName = " "
  lcExtPrg   = " "
  rbSysPrg   = 0
  llSysPrg   = .F.
  lcProgPram = " "
ENDIF


IF _DOS
  =lfRefresh()
ENDIF

SHOW GET rbSysPrg
puPop = lcProgName
SHOW GET puPop

IF rbSysPrg = 2
  IF lnAryNo > 0
    IF lcApp_Id = "SY"
      IF VAL(laMenu[lnAryNo,lnPad_Pos]) = 7
        SHOW GETS WINDOW SMMENU2 ENABLE  ONLY
        ACTIVATE WINDOW SMMENU2 TOP
        SHOW GETS WINDOW SMMENU1 DISABLE ONLY
      ELSE
        SHOW GETS WINDOW SMMENU1 ENABLE  ONLY
        ACTIVATE WINDOW SMMENU1 TOP
        SHOW GETS WINDOW SMMENU2 DISABLE ONLY
      ENDIF
    ELSE
      SHOW GETS WINDOW SMMENU2 ENABLE  ONLY
      ACTIVATE WINDOW SMMENU2 TOP
      SHOW GETS WINDOW SMMENU1 DISABLE ONLY
    ENDIF
  ELSE
    SHOW GETS WINDOW SMMENU1 ENABLE  ONLY
    ACTIVATE WINDOW SMMENU1 TOP
    SHOW GETS WINDOW SMMENU2 DISABLE ONLY
  ENDIF
ELSE
  SHOW GETS WINDOW SMMENU1 ENABLE  ONLY
  ACTIVATE WINDOW SMMENU1 TOP
  SHOW GETS WINDOW SMMENU2 DISABLE ONLY
ENDIF

IF lnAryNo > 0
  IF LEFT(laMenu[lnAryNo,lnSub_Prpt],2) = "\-" .OR. ;
     EMPTY(laMenu[lnAryNo,lnSub_Prpt]) .OR. ;
     AT(lcUpLeft,PRMBAR("puMenu",GETBAR("puMenu",lsMenu))) > 0 .OR. ;
     AT(lcDnLeft,PRMBAR("puMenu",GETBAR("puMenu",lsMenu))) > 0
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
SHOW GET rbSysPrg,1 PROMPT lcPrmp1
SHOW GET rbSysPrg,2 PROMPT lcPrmp2

IF lcApp_Id = "SY"
  IF lnAryNo > 0
    IF VAL(laMenu[lnAryNo,lnPad_Pos]) = 7
      SHOW GET rbSysPrg,3 DISABLE
      SHOW GET rbSysPrg,4 PROMPT lcPrmp4 ENABLE
    ELSE
      SHOW GET rbSysPrg,3 ENABLE
      SHOW GET rbSysPrg,4 PROMPT lcPrmp4 DISABLE
    ENDIF
  ELSE
    SHOW GET rbSysPrg,3 ENABLE
    SHOW GET rbSysPrg,4 PROMPT lcPrmp4 DISABLE
  ENDIF
ELSE
  SHOW GET rbSysPrg,3 ENABLE
  SHOW GET rbSysPrg,4 PROMPT lcPrmp4 ENABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfvMenu
*!
*!**************************************************************************
*
*** Valid function of the list. ***

FUNCTION lfvMenu

IF lsMenu = 0
  RETURN
ENDIF

*** Get all the needed information of the ***
*** current bar that I need to edit it...
lnBarNo    = GETBAR("puMenu",lsMenu)
lcPrompt   = PRMBAR("puMenu",lnBarNo)
lnAryNo    = VAL(SUBSTR(lcPrompt,RAT("+",lcPrompt)-3,3))
lnBarLevel = VAL(SUBSTR(lcPrompt,RAT("+",lcPrompt)+1))

IF lnAryNo = 0
  RETURN
ENDIF

lcWhichPad = "^"+ALLTRIM(STR(VAL(laMenu[lnAryNo,lnPad_Pos])))

*** You cannot edit any separator. ***
IF ALLTRIM(laMenu[lnAryNo,lnSub_Prpt]) = "\-"
  RETURN
ENDIF

*E300683,4 Call *.SPR from screens directory
* DO SMMENED.SPR 
DO (gcScrDir + gcWinAppl + '\SMMENED.SPR')
*E300683,4 end  
IF lnBarLevel = 0
  *** If I am editting a pad title. ***
  DO CASE
    CASE _DOS
      DEFINE BAR lnBarNo OF puMenu PROMPT ;
             lcHfBloc+SUBSTR(laMenu[lnAryNo,lnSub_Prpt],1,41)+;
             IIF('\<' $ laMenu[lnAryNo,lnSub_Prpt],"  ","")+;
             lcBranch+lcHfBlocs+RIGHT(STR(lnAryNo),3)+;
             "+"+SPACE(1) COLOR SCHEME 24
    CASE _WINDOWS
      DEFINE BAR lnBarNo OF puMenu PROMPT ;
             lcHfBloc+SUBSTR(laMenu[lnAryNo,lnSub_Prpt],1,41)+;
             IIF('\<' $ laMenu[lnAryNo,lnSub_Prpt],"  ","")+;
             lcBranch+lcHfBlocs+RIGHT(STR(lnAryNo),3)+;
             "+"+SPACE(1)
  ENDCASE
ELSE
  *** If I am editting any other bar. ***
  DO CASE
     CASE _DOS
       DEFINE BAR lnBarNo OF puMenu PROMPT ;
              lcHfBloc+lcHfBloc+lcVrLine+;
              IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
              SUBSTR(laMenu[lnAryNo,lnSub_Prpt],1,41)+;
              IIF(laMenu[lnAryNo,lnSub_Typ]="S",;
              IIF('\<' $ laMenu[lnAryNo,lnSub_Prpt],"  ","")+lcBranch,;
              IIF('\<' $ laMenu[lnAryNo,lnSub_Prpt],"   "," "))+;
              lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
              RIGHT(STR(lnAryNo),3)+"+"+STR(lnBarLevel,1) ;
              COLOR SCHEME 24
    CASE _WINDOWS
       DEFINE BAR lnBarNo OF puMenu PROMPT ;
              lcHfBloc+lcHfBloc+lcVrLine+;
              IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
              SUBSTR(laMenu[lnAryNo,lnSub_Prpt],1,41)+;
              IIF(laMenu[lnAryNo,lnSub_Typ]="S",;
              IIF('\<' $ laMenu[lnAryNo,lnSub_Prpt],"  ","")+lcBranch,;
              IIF('\<' $ laMenu[lnAryNo,lnSub_Prpt],"   "," "))+;
              lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
              RIGHT(STR(lnAryNo),3)+"+"+STR(lnBarLevel,1)
  ENDCASE
ENDIF

_CUROBJ = OBJNUM(lsMenu)

*** Flag to say that the current module need to be save. ***
llSaved = .F.

*!**************************************************************************
*!
*!      Function: lfvAppId
*!
*!**************************************************************************
*
*** Valid function of the popup hold all the ***
*** available modules in the system...

FUNCTION lfvAppId

*** Save the old application. ***
lcOldApp = lcApp_Id

DO CASE
  CASE _DOS
    lcApp_Id = gfActPop(1,18,7,65,'laApp',2,1,@lcApp_Desc,gcBaseWind)
    =lfRefresh()
  CASE _WINDOWS
    lcApp_Id = laApp[ibPopApp,2]
    SHOW GET ibPopApp
ENDCASE

*** If the module is changed. ***
IF lcOldApp <> lcApp_Id

  =lfvSav_App(lcOldApp)     && Save the old module data.
  
  =lfSelData()              && Select the current module data.

  =lfBldBars()              && Build the popup for the current module data.
  
ENDIF
*B603865,1 HBG 12/09/2000 IF EDI modules Enables bar's Module button [Begin]
IF lcApp_Id = "EB" .OR. lcApp_Id= "AS" .OR. lcApp_Id= "UP"
  SHOW GET pbBarMod ENABLE
ELSE
  SHOW GET pbBarMod DISABLE
ENDIF
*B603865,1 HBG [End]

*!**************************************************************************
*!
*!      Function: lfvNewBrSp
*!
*!**************************************************************************
*
*** Valid function of the 2 push buttons:_
*** <New bar> - <New separator>...

FUNCTION lfvNewBrSp
PARAMETERS lnBr_or_Sp

IF lsMenu = 0
  *** You must mark a list item first. ***
  *** <  Ok  > ***
  = gfModalGen("TRM00008B00000","Dialog")
  RETURN
ENDIF

lnCurBar = GETBAR("puMenu",lsMenu)

*** Get the array element of the current bar. ***
lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",lnCurBar),;
           RAT("+",PRMBAR("puMenu",lnCurBar))-3,3))

IF lnAryNo = 0 .OR. lnAryNo > ALEN(laMenu,1)
  RETURN
ENDIF

IF laMenu[lnAryNo,llDefault] .AND. ;
   VAL(laMenu[lnAryNo,lnPad_Pos]) = 6 .AND. ;
   laMenu[lnAryNo,lnSub_Typ] <> "S"
  *** You can not add any thing in the default popup. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00149B00000","DIALOG")
  RETURN
ENDIF

*** Get the level of the current bar. ***
lnBarLevel = VAL(SUBSTR(PRMBAR("puMenu",lnCurBar),;
             RAT("+",PRMBAR("puMenu",lnCurBar))+1))

IF lnBarLevel = 0
  *** You cannot add new bar outside any pad. ***

  =gfModalGen("TRM00138B00000","DIALOG","bar")
  RETURN
ENDIF

*** Hide the popup before adding any thing to it. ***
HIDE POPUP puMenu SAVE

*** If the prompt of the current bar is empty. ***
*** No need to insert an array element, Add it ***
*** in the current array element...
IF EMPTY(laMenu[lnAryNo,lnSub_Prpt])
  laMenu[lnAryNo,1]          = lcApp_Id
  laMenu[lnAryNo,lnSub_Typ]  = "C"
  laMenu[lnAryNo,lnSub_Prpt] = IIF(lnBr_or_Sp=1,"New Bar"+SPACE(34),"\-")
  laMenu[lnAryNo,lnPad_Pos]  = PADL(ALLTRIM(SUBSTR(PRMBAR("puMenu",lnCurBar),;
                               AT("^",PRMBAR("puMenu",lnCurBar))+1,1)),2,"0")
  DO CASE
    CASE VAL(laMenu[lnAryNo,lnPad_Pos]) = 5
      laMenu[lnAryNo,lnProcType] = "R"
    CASE VAL(laMenu[lnAryNo,lnPad_Pos]) = 7
      laMenu[lnAryNo,lnProcType] = "M"
    OTHERWISE
      laMenu[lnAryNo,lnProcType] = "P"
  ENDCASE
  laMenu[lnAryNo,lnProcPath] = ""
  laMenu[lnAryNo,lnProssId]  = ""
  laMenu[lnAryNo,lnBar_Msg]  = ""
  laMenu[lnAryNo,lnHot_key]  = ""
  laMenu[lnAryNo,llDefault]  = .F.
  
  *B602599,1 Reham On 11/12/1999   *** Begin ***
  *B602599,1 Define the array element of the program parameters to void adding .F.
  laMenu[lnAryNo,lnParamPos] = ""
  lcProgPram = ""
  *B602599,1 Reham On 11/12/1999   *** Begin ***

  *B603564,1 Reham On 04/27/2000   *** Begin ***
  *B603564,1 1- If the value of radio button is custom, the upgrade levl will be "U"
  *B603564,1 2- The upgrade level will be "S" for main system or system manager
  *B603564,1 3- otherwise, it will be "A" for all the other modules.
  laMenu[lnAryNo,lnUpgrade]  = IIF(rbSysPrg = 4 , "U" , IIF(lcApp_Id = "SY" OR lcApp_Id = "SM" , "S" , "A"))
  *B603564,1 Reham On 04/27/2000   *** End   ***
  
  *B603560,1 Reham On 04/27/2000   *** Begin ***
  laMenu[lnAryNo,lnBarMod]  = ""
  *B603560,1 Reham On 04/27/2000   *** End   ***
  
  rbSysPrg   = 1
  lcWhichPad = "^"+ALLTRIM(STR(VAL(laMenu[lnAryNo,lnPad_Pos])))
  
  lnBarNo  = lnCurBar
  IF lnBr_or_Sp = 1
    *** If it is a bar. ***
    DO CASE
      CASE _DOS
        DEFINE BAR lnBarNo OF puMenu PROMPT ;
               lcHfBloc+lcHfBloc+lcVrLine+;
               IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
               SUBSTR(laMenu[lnAryNo,lnSub_Prpt],1,41)+;
               IIF('\<' $ laMenu[lnAryNo,lnSub_Prpt],"   "," ")+;
               lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
               RIGHT(STR(lnAryNo),3)+"+"+;
               RIGHT(laMenu[lnAryNo,lnPop_Levl],1) ;
               COLOR SCHEME 24
      CASE _WINDOWS
        DEFINE BAR lnBarNo OF puMenu PROMPT ;
               lcHfBloc+lcHfBloc+lcVrLine+;
               IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
               SUBSTR(laMenu[lnAryNo,lnSub_Prpt],1,41)+;
               IIF('\<' $ laMenu[lnAryNo,lnSub_Prpt],"   "," ")+;
               lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
               RIGHT(STR(lnAryNo),3)+"+"+;
               RIGHT(laMenu[lnAryNo,lnPop_Levl],1)
    ENDCASE
  ELSE
    *** If it is a separator. ***
    DO CASE
      CASE _DOS
        DEFINE BAR lnBarNo OF puMenu PROMPT ;
               lcHfBloc+lcHfBloc+;
               IIF(lnBarLevel=1,"",REPLICATE(lcVrLine+"  ",lnBarLevel-1))+;
               lcLnLeft+REPLICATE(lcHzLine,42)+ ;
               lcLnRght+lcBlock+lcBlock+lcHfBlocs+;
               lcWhichPad+RIGHT(STR(lnAryNo),3)+"+"+;
               STR(lnBarLevel,1) COLOR SCHEME 24
      CASE _WINDOWS
        DEFINE BAR lnBarNo OF puMenu PROMPT ;
               lcHfBloc+lcHfBloc+;
               IIF(lnBarLevel=1,"",REPLICATE(lcVrLine+"  ",lnBarLevel-1))+;
               lcLnLeft+REPLICATE(lcHzLine,42)+ ;
               lcLnRght+lcBlock+lcBlock+lcHfBlocs+;
               lcWhichPad+RIGHT(STR(lnAryNo),3)+"+"+;
               STR(lnBarLevel,1)
    ENDCASE
  ENDIF
ELSE
  *** The prompt of the current array is not empty. ***
  DIMENSION laMenu [ALEN(laMenu,1)+1,ALEN(laMenu,2)]
  laMenu [ALEN(laMenu,1),1]          = lcApp_Id
  laMenu [ALEN(laMenu,1),lnPop_Levl] = laMenu[lnAryNo,lnPop_Levl]
  laMenu [ALEN(laMenu,1),lnSub_Typ]  = "C"
  laMenu [ALEN(laMenu,1),lnSub_Prpt] = IIF(lnBr_or_Sp=1,;
                                       "New Bar"+SPACE(34),"\-")
  laMenu[ALEN(laMenu,1),lnPad_Pos]   = PADL(ALLTRIM(SUBSTR(PRMBAR("puMenu",;
                                       lnCurBar),AT("^",PRMBAR("puMenu",;
                                       lnCurBar))+1,1)),2,"0")
  DO CASE
    CASE VAL(laMenu[ALEN(laMenu,1),lnPad_Pos]) = 5
      laMenu[ALEN(laMenu,1),lnProcType] = "R"
    CASE VAL(laMenu[ALEN(laMenu,1),lnPad_Pos]) = 7
      laMenu[ALEN(laMenu,1),lnProcType] = "M"
    OTHERWISE
      laMenu[ALEN(laMenu,1),lnProcType] = "P"
  ENDCASE

  laMenu[ALEN(laMenu,1),lnProcPath]  = ""
  laMenu[ALEN(laMenu,1),lnProssId]   = ""
  laMenu[ALEN(laMenu,1),lnBar_Msg]   = ""
  laMenu[ALEN(laMenu,1),lnHot_key]   = ""
  laMenu[ALEN(laMenu,1),llDefault]   = .F.

  *B602599,1 Reham On 11/12/1999   *** Begin ***
  *B602599,1 Define the array element of the program parameters to void adding .F.
  laMenu[ALEN(laMenu,1),lnParamPos] = ""
  lcProgPram = ""
  *B602599,1 Reham On 11/12/1999   *** Begin ***
  
  *B603564,1 Reham On 04/27/2000   *** Begin ***
  *B603564,1 1- If the value of radio button is custom, the upgrade levl will be "U"
  *B603564,1 2- The upgrade level will be "S" for main system or system manager
  *B603564,1 3- otherwise, it will be "A" for all the other modules.
  laMenu[ALEN(laMenu,1),lnUpgrade]  = IIF(rbSysPrg = 4 , "U" , IIF(lcApp_Id = "SY" OR lcApp_Id = "SM" , "S" , "A"))
  *B603564,1 Reham On 04/27/2000   *** End   ***
  
  *B603560,1 Reham On 04/27/2000   *** Begin ***
  laMenu[ALEN(laMenu,1),lnBarMod]   = ""
  *B603560,1 Reham On 04/27/2000   *** End   ***
  
  lnBarNo  = CNTBAR('puMenu')+1
  lnBefore = GETBAR('puMenu',lsMenu)

  lcWhichPad = "^"+ALLTRIM(STR(VAL(laMenu[ALEN(laMenu,1),lnPad_Pos])))
  
  IF lnBr_or_Sp = 1
    *** If it a bar. ***
    DO CASE
      CASE _DOS
        DEFINE BAR lnBarNo OF puMenu PROMPT ;
               lcHfBloc+lcHfBloc+lcVrLine+;
               IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
               SUBSTR(laMenu[ALEN(laMenu,1),lnSub_Prpt],1,41)+;
               IIF('\<' $ laMenu[ALEN(laMenu,1),lnSub_Prpt],"   "," ")+;
               lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
               RIGHT(STR(ALEN(laMenu,1)),3)+"+"+;
               RIGHT(laMenu[ALEN(laMenu,1),lnPop_Levl],1) ;
               COLOR SCHEME 24 ;
               BEFORE lnBefore
      CASE _WINDOWS
        DEFINE BAR lnBarNo OF puMenu PROMPT ;
               lcHfBloc+lcHfBloc+lcVrLine+;
               IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
               SUBSTR(laMenu[ALEN(laMenu,1),lnSub_Prpt],1,41)+;
               IIF('\<' $ laMenu[ALEN(laMenu,1),lnSub_Prpt],"   "," ")+;
               lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
               RIGHT(STR(ALEN(laMenu,1)),3)+"+"+;
               RIGHT(laMenu[ALEN(laMenu,1),lnPop_Levl],1) ;
               BEFORE lnBefore
    ENDCASE
  ELSE
    *** If it is a separator. ***
    DO CASE
      CASE _DOS
        DEFINE BAR lnBarNo OF puMenu PROMPT ;
               lcHfBloc+lcHfBloc+;
               IIF(lnBarLevel=1,"",REPLICATE(lcVrLine+"  ",lnBarLevel-1))+;
               lcLnLeft+REPLICATE(lcHzLine,42)+ ;
               lcLnRght+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+;
               RIGHT(STR(ALEN(laMenu,1)),3)+"+"+;
               STR(lnBarLevel,1) COLOR SCHEME 24;
               BEFORE lnBefore
      CASE _WINDOWS
        DEFINE BAR lnBarNo OF puMenu PROMPT ;
               lcHfBloc+lcHfBloc+;
               IIF(lnBarLevel=1,"",REPLICATE(lcVrLine+"  ",lnBarLevel-1))+;
               lcLnLeft+REPLICATE(lcHzLine,42)+ ;
               lcLnRght+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+;
               RIGHT(STR(ALEN(laMenu,1)),3)+"+"+;
               STR(lnBarLevel,1) BEFORE lnBefore
    ENDCASE
  ENDIF
ENDIF

*** Flag to say that the current module need to be save. ***
llSaved = .F.

*** Show the popup to display the new bar or the new separator. ***
SHOW POPUP puMenu
SHOW GET rbSysPrg
*** Show the list. ***
SHOW GET   lsMenu
_CUROBJ = OBJNUM(lsMenu)

*!**************************************************************************
*!
*!      Function: lfvRemBar
*!
*!**************************************************************************
*
*** Valid function of the push button <remove bar>

FUNCTION lfvRemBar

IF lsMenu = 0
  *** You must mark a list item first. ***
  *** <  Ok  > ***
  = gfModalGen("TRM00008B00000","Dialog")
  RETURN
ENDIF

lnBarNo  = GETBAR('puMenu',lsMenu)

*** Get the array element of the current bar. ***
lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",lnBarNo),;
           RAT("+",PRMBAR("puMenu",lnBarNo))-3,3))

IF lnAryNo = 0 .OR. lnAryNo > ALEN(laMenu,1) .OR. lsMenu = 0
  RETURN
ENDIF

IF laMenu[lnAryNo,llDefault]
  *** You can not change anything in default bars. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00148B00000","DIALOG")
  RETURN
ENDIF

*** Get the level of the current bar. ***
lnBarLevel  = VAL(SUBSTR(PRMBAR("puMenu",lnBarNo),;
              RAT("+",PRMBAR("puMenu",lnBarNo))+1))

IF lnBarLevel = 0
  *** You cannot remove any pad. ***
  =gfModalGen("TRM00139B00000","DIALOG")
  RETURN
ENDIF

HIDE POPUP puMenu SAVE 

*** If this bar is a branched bar and has a popup. ***
IF laMenu[lnAryNo,lnSub_Typ] = "S"
  
  *** This bar is a branching bar. ***
  *** Therefore, the popup will be removed. ***
  *** < Remove >  -  < Remove All >  -  < Cancel > ***
  lnOption = gfModalGen("TRM00132B00024","ALERT")

  DO CASE
    
    *** Remove the branching bar & its popup. ***
    CASE lnOption = 1
      *** Flag to say that the current ***
      *** module need to be save. ***
      llSaved = .F.
      =lfRemove(lsMenu,.F.)
    *** Remove all the nesting  popups ***
    *** related to this branching bar. ***
    CASE lnOption = 2
      *** Flag to say that the current ***
      *** module need to be save. ***
      llSaved = .F.
      =lfRemove(lsMenu,.T.)
    *** Cancel the whole removing. ***
    CASE lnOption = 3
      RETURN
  ENDCASE
ELSE
  *** Flag to say that the current ***
  *** module need to be save. ***
  llSaved = .F.

  IF !EMPTY(laMenu[lnAryNo,lnProssId]) .AND. lcApp_Id <> "SY"
    IF !EMPTY(laProc2Del[1])
      DIMENSION laProc2Del[ALEN(laProc2Del,1)+1]
      =AINS(laProc2Del,1)
    ENDIF
    laProc2Del[1] = laMenu[lnAryNo,lnProssId]
  ENDIF

  IF AT(lcUpLeft,PRMBAR("puMenu",GETBAR("puMenu",lsMenu-1))) > 0 .AND.;
     AT(lcDnLeft,PRMBAR("puMenu",GETBAR("puMenu",lsMenu+1))) > 0
    
    *** If last element in the popup, replace the prompt ***
    *** text with blank instead of removing it...
    laMenu[lnAryNo,lnSub_Prpt] = SPACE(41)
    laMenu[lnAryNo,lnSub_Typ ] = "C"
    laMenu[lnAryNo,lnProcType] = ""
    DO CASE
      CASE _DOS
        DEFINE BAR lnBarNo OF puMenu PROMPT ;
               lcHfBloc+lcHfBloc+lcVrLine+;
               IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
               SUBSTR(laMenu[lnAryNo,lnSub_Prpt],1,41)+;
               IIF('\<' $ laMenu[lnAryNo,lnSub_Prpt],"   "," ")+;
               lcVrLine+lcBlock+lcBlock+lcHfBlocs+ ;
               RIGHT(STR(lnAryNo),3)+"+"+STR(lnBarLevel,1) ;
               COLOR SCHEME 24
      CASE _WINDOWS
        DEFINE BAR lnBarNo OF puMenu PROMPT ;
               lcHfBloc+lcHfBloc+lcVrLine+;
               IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
               SUBSTR(laMenu[lnAryNo,lnSub_Prpt],1,41)+;
               IIF('\<' $ laMenu[lnAryNo,lnSub_Prpt],"   "," ")+;
               lcVrLine+lcBlock+lcBlock+lcHfBlocs+ ;
               RIGHT(STR(lnAryNo),3)+"+"+STR(lnBarLevel,1)
    ENDCASE
  ELSE
    *** Move a bar from the popup. ***
    RELEASE BAR lnBarNo OF puMenu
  ENDIF
ENDIF

SHOW POPUP puMenu
SHOW GET   lsMenu
_CUROBJ = OBJNUM(lsMenu)

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
         AT(lcDnLeft,PRMBAR("puMenu",GETBAR("puMenu",lnList_Val+2))) = 0

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
    IF laMenu[lnLstAryNo,lnSub_Typ] = "S"
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

  IF !EMPTY(laMenu[lnLstAryNo,lnProssId]) .AND. lcApp_Id <> "SY"
    IF !EMPTY(laProc2Del[1])
      DIMENSION laProc2Del[ALEN(laProc2Del,1)+1]
      =AINS(laProc2Del,1)
    ENDIF
    laProc2Del[1] = laMenu[lnLstAryNo,lnProssId]
  ENDIF
  
  *** Remove the bar from the popup. ***
  RELEASE BAR GETBAR("puMenu",lnList_Val+2) OF puMenu
ENDDO

*** Release opening popup, closing popup & the shadow. ***
FOR lnCont6 = 1 TO 3
  RELEASE BAR GETBAR("puMenu",lnList_Val+1) OF puMenu
ENDFOR

IF !EMPTY(laMenu[lnAryNo,lnProssId]) .AND. lcApp_Id <> "SY"
  IF !EMPTY(laProc2Del[1])
    DIMENSION laProc2Del[ALEN(laProc2Del,1)+1]
    =AINS(laProc2Del,1)
  ENDIF
  laProc2Del[1] = laMenu[lnAryNo,lnProssId]
ENDIF

IF AT(lcUpLeft,PRMBAR("puMenu",GETBAR("puMenu",lnList_Val-1))) > 0 .AND.;
   AT(lcDnLeft,PRMBAR("puMenu",GETBAR("puMenu",lnList_Val+1))) > 0
    
  *** If last element in the popup, replace the prompt ***
  *** text with blank instead of removing it...
  IF lnOldAryNo <> 0 .AND. lnOldAryNo < ALEN(laMenu,1)
    laMenu[lnOldAryNo,lnSub_Prpt] = SPACE(41)
    laMenu[lnOldAryNo,lnSub_Typ ] = "C"
    laMenu[lnOldAryNo,lnProcType] = ""
    DO CASE
      CASE _DOS
        DEFINE BAR GETBAR("puMenu",lnList_Val) OF ;
               puMenu PROMPT lcHfBloc+lcHfBloc+lcVrLine+;
               IIF(lnCurLevel=1,"",REPLICATE("  "+lcVrLine,lnCurLevel-1))+;
               SUBSTR(laMenu[lnOldAryNo,lnSub_Prpt],1,41)+;
               IIF('\<' $ laMenu[lnOldAryNo,lnSub_Prpt],"   "," ")+;
               lcVrLine+lcBlock+lcBlock+lcHfBlocs+ ;
               RIGHT(STR(lnOldAryNo),3)+"+"+STR(lnCurLevel,1) ;
               COLOR SCHEME 24
      CASE _WINDOWS
        DEFINE BAR GETBAR("puMenu",lnList_Val) OF ;
               puMenu PROMPT lcHfBloc+lcHfBloc+lcVrLine+;
               IIF(lnCurLevel=1,"",REPLICATE("  "+lcVrLine,lnCurLevel-1))+;
               SUBSTR(laMenu[lnOldAryNo,lnSub_Prpt],1,41)+;
               IIF('\<' $ laMenu[lnOldAryNo,lnSub_Prpt],"   "," ")+;
               lcVrLine+lcBlock+lcBlock+lcHfBlocs+ ;
               RIGHT(STR(lnOldAryNo),3)+"+"+STR(lnCurLevel,1)
    ENDCASE
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

IF lsMenu = 0
  *** You must mark a list item first. ***
  *** <  Ok  > ***
  = gfModalGen("TRM00008B00000","Dialog")
  RETURN
ENDIF

lnCurBar = GETBAR("puMenu",lsMenu)

*** Get the array element of the current bar. ***
lnAryNo  = VAL(SUBSTR(PRMBAR("puMenu",lnCurBar),;
           RAT("+",PRMBAR("puMenu",lnCurBar))-3,3))

IF lnAryNo = 0 .OR. lnAryNo > ALEN(laMenu,1)
  RETURN
ENDIF

IF laMenu[lnAryNo,llDefault] .AND. ;
   VAL(laMenu[lnAryNo,lnPad_Pos]) = 6 .AND. ;
   laMenu[lnAryNo,lnSub_Typ] <> "S"
  *** You can not add any thing in the default popup. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00149B00000","DIALOG")
  RETURN
ENDIF

*** Get the level of the current popup. ***
lnBarLevel  = VAL(SUBSTR(PRMBAR("puMenu",lnCurBar),;
           RAT("+",PRMBAR("puMenu",lnCurBar))+1))

IF lnBarLevel = 0
  *** You cannot add new popup outside any pad. ***
  =gfModalGen("TRM00138B00000","DIALOG","popup")
  RETURN
ENDIF

IF lnBarLevel = 4
  *** The maximum nesting levels is 4 popups. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00137B00000","DIALOG")
  RETURN
ENDIF

HIDE POPUP puMenu SAVE

*** If there is an empty bar, use it instead of adding new one. ***
IF EMPTY(laMenu[lnAryNo,lnSub_Prpt])
  llTempFl = .F.
  lnTempNo = lnAryNo
  lnBarNo  = lnCurBar
  lnBefore = GETBAR('puMenu',lsMenu+1)
ELSE
  llTempFl = .T.
  DIMENSION laMenu [ALEN(laMenu,1)+1,ALEN(laMenu,2)]
  lnTempNo = ALEN(laMenu,1)
  lnBarNo  = CNTBAR('puMenu')+1
  lnBefore = GETBAR('puMenu',lsMenu)
ENDIF
laMenu [lnTempNo,1]          = lcApp_Id
laMenu [lnTempNo,lnPad_Pos]  = PADL(ALLTRIM(SUBSTR(PRMBAR("puMenu",lnCurBar),;
                               AT("^",PRMBAR("puMenu",lnCurBar))+1,1)),2,"0")
laMenu [lnTempNo,lnPop_Levl] = laMenu[lnAryNo,lnPop_Levl]
laMenu [lnTempNo,lnSub_Prpt] = "New popup"+SPACE(32)
laMenu [lnTempNo,lnSub_Typ]  = "S"
laMenu [lnTempNo,lnProcType] = ""
laMenu [lnTempNo,lnProssId]  = ""
laMenu [lnTempNo,lnBar_Msg]  = ""
laMenu [lnTempNo,lnHot_key]  = ""
laMenu [lnTempNo,llDefault]  = .F.

*B602599,1 Reham On 11/14/1999   *** Begin ***
*B602599,1 Define the array element of the program parameters to void adding .F.
laMenu [lnTempNo,lnParamPos] = ""
lcProgPram = ""
*B602599,1 Reham On 11/14/1999   *** Begin ***

*B603564,1 Reham On 04/27/2000   *** Begin ***
*B603564,1 1- If the value of radio button is custom, the upgrade levl will be "U"
*B603564,1 2- The upgrade level will be "S" for main system or system manager
*B603564,1 3- otherwise, it will be "A" for all the other modules.
laMenu[lnTempNo,lnUpgrade]  = IIF(rbSysPrg = 4 , "U" , IIF(lcApp_Id = "SY" OR lcApp_Id = "SM" , "S" , "A"))
*B603564,1 Reham On 04/27/2000   *** End   ***

*B603560,1 Reham On 04/27/2000   *** Begin ***
laMenu[lnTempNo,lnBarMod]  = ""
*B603560,1 Reham On 04/27/2000   *** End   ***

lcWhichPad = "^"+ALLTRIM(STR(VAL(laMenu[lnTempNo,lnPad_Pos])))

*** Define the title of the popup. (the branching bar). ***
DO CASE
  CASE _DOS
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           lcHfBloc+lcHfBloc+lcVrLine+;
           IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
           SUBSTR(laMenu[lnTempNo,lnSub_Prpt],1,41)+;
           IIF('\<' $ laMenu[lnTempNo,lnSub_Prpt],"  ","")+;
           lcBranch+lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
           RIGHT(STR(lnTempNo),3)+"+"+;
           RIGHT(laMenu[lnTempNo,lnPop_Levl],1) ;
           COLOR SCHEME 24 ;
           BEFORE lnBefore
  CASE _WINDOWS
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           lcHfBloc+lcHfBloc+lcVrLine+;
           IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
           SUBSTR(laMenu[lnTempNo,lnSub_Prpt],1,41)+;
           IIF('\<' $ laMenu[lnTempNo,lnSub_Prpt],"  ","")+;
           lcBranch+lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
           RIGHT(STR(lnTempNo),3)+"+"+;
           RIGHT(laMenu[lnTempNo,lnPop_Levl],1) ;
           BEFORE lnBefore
ENDCASE

lnBarNo  = CNTBAR('puMenu')+1

*** Increase the level with 1. ***
lnBarLevel  = lnBarLevel + 1

*** Define the opening of the popup. ***
DO CASE
  CASE _DOS
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           "\"+lcHfBloc+lcHfBloc+;
           REPLICATE(lcVrLine+"  ",lnBarLevel-1)+;
           lcUpLeft+REPLICATE(lcHzLine,42)+;
           lcUpRght+lcHfBlocs+lcWhichPad+"+"+STR(lnBarLevel,1);
           COLOR SCHEME 24 BEFORE lnBefore
  CASE _WINDOWS
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           lcHfBloc+lcHfBloc+;
           REPLICATE(lcVrLine+"  ",lnBarLevel-1)+;
           lcUpLeft+REPLICATE(lcHzLine,42)+;
           lcUpRght+lcHfBlocs+lcWhichPad+"+"+STR(lnBarLevel,1);
           BEFORE lnBefore
    SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.
ENDCASE

lnBarNo  = CNTBAR('puMenu')+1

*** Define on bar in the popup. ***
DIMENSION laMenu [ALEN(laMenu,1)+1,ALEN(laMenu,2)]
laMenu [ALEN(laMenu,1),1]          = lcApp_Id
laMenu [ALEN(laMenu,1),lnPad_Pos]  = PADL(ALLTRIM(SUBSTR(PRMBAR("puMenu",lnCurBar),;
                                     AT("^",PRMBAR("puMenu",lnCurBar))+1,1)),2,"0")
laMenu [ALEN(laMenu,1),lnPop_Levl] = STR(lnBarLevel,2)
laMenu [ALEN(laMenu,1),lnSub_Prpt] = "New bar"+SPACE(34)
laMenu [ALEN(laMenu,1),lnSub_Typ]  = "C"
DO CASE
  CASE VAL(laMenu[ALEN(laMenu,1),lnPad_Pos]) = 5
    laMenu[ALEN(laMenu,1),lnProcType] = "R"
  CASE VAL(laMenu[ALEN(laMenu,1),lnPad_Pos]) = 7
    laMenu[ALEN(laMenu,1),lnProcType] = "M"
  OTHERWISE
    laMenu[ALEN(laMenu,1),lnProcType] = "P"
ENDCASE
laMenu [ALEN(laMenu,1),lnProcPath] = ""
laMenu [ALEN(laMenu,1),lnProssId]  = ""
laMenu [ALEN(laMenu,1),lnBar_Msg]  = ""
laMenu [ALEN(laMenu,1),lnHot_key]  = ""
laMenu [ALEN(laMenu,1),llDefault]  = .F.

*B602599,1 Reham On 11/14/1999   *** Begin ***
*B602599,1 Define the array element of the program parameters to void adding .F.
laMenu [ALEN(laMenu,1),lnParamPos] = ""
lcProgPram = ""
*B602599,1 Reham On 11/14/1999   *** Begin ***

*B603564,1 Reham On 04/27/2000   *** Begin ***
*B603564,1 1- If the value of radio button is custom, the upgrade levl will be "U"
*B603564,1 2- The upgrade level will be "S" for main system or system manager
*B603564,1 3- otherwise, it will be "A" for all the other modules.
laMenu[ALEN(laMenu,1),lnUpgrade]  = IIF(rbSysPrg = 4 , "U" , IIF(lcApp_Id = "SY" OR lcApp_Id = "SM" , "S" , "A"))
*B603564,1 Reham On 04/27/2000   *** End   ***

*B603560,1 Reham On 04/27/2000   *** Begin ***
laMenu[ALEN(laMenu,1),lnBarMod]  = ""
*B603560,1 Reham On 04/27/2000   *** End   ***

rbSysPrg = 1

DO CASE 
  CASE _DOS
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           lcHfBloc+lcHfBloc+lcVrLine+;
           IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
           SUBSTR(laMenu[ALEN(laMenu,1),lnSub_Prpt],1,41)+;
           IIF('\<' $ laMenu[ALEN(laMenu,1),lnSub_Prpt],"   "," ")+;
           lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
           RIGHT(STR(ALEN(laMenu,1)),3)+"+"+;
           RIGHT(laMenu[ALEN(laMenu,1),lnPop_Levl],1) ;
           COLOR SCHEME 24 ;
           BEFORE lnBefore
  CASE _WINDOWS
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           lcHfBloc+lcHfBloc+lcVrLine+;
           IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
           SUBSTR(laMenu[ALEN(laMenu,1),lnSub_Prpt],1,41)+;
           IIF('\<' $ laMenu[ALEN(laMenu,1),lnSub_Prpt],"   "," ")+;
           lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
           RIGHT(STR(ALEN(laMenu,1)),3)+"+"+;
           RIGHT(laMenu[ALEN(laMenu,1),lnPop_Levl],1) ;
           BEFORE lnBefore
ENDCASE

lnBarNo  = CNTBAR('puMenu')+1

*** Define the closing of the popup. ***
DO CASE
  CASE _DOS
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           "\"+lcHfBloc+lcHfBloc+;
           REPLICATE(lcVrLine+"  ",lnBarLevel-1)+;
           lcDnLeft+REPLICATE(lcHzLine,42)+lcDnRght+;
           lcBlock+lcBlock+lcHfBlocs+lcWhichPad+"+"+;
           STR(lnBarLevel,1) COLOR SCHEME 24;
           BEFORE lnBefore
    lnBarNo  = CNTBAR('puMenu')+1

    *** Define the shadow of the popup. ***
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           "\"+REPLICATE(lcHfBloc,2)+;
           REPLICATE(lcVrLine+"  ",lnBarLevel-1)+;
           SPACE(2)+REPLICATE(lcBlock,44)+;
           lcHfBlocs+lcWhichPad+"+"+STR(lnBarLevel,1) ;
           COLOR SCHEME 24 BEFORE lnBefore
  CASE _WINDOWS
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           lcHfBloc+lcHfBloc+;
           REPLICATE(lcVrLine+"  ",lnBarLevel-1)+;
           lcDnLeft+REPLICATE(lcHzLine,42)+lcDnRght+;
           lcBlock+lcBlock+lcHfBlocs+lcWhichPad+"+"+;
           STR(lnBarLevel,1) BEFORE lnBefore
    SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.
ENDCASE

*** Flag to say that the current ***
*** module need to be saved. ***
llSaved = .F.

SHOW POPUP puMenu
SHOW GET rbSysPrg
SHOW GET lsMenu
_CUROBJ = OBJNUM(lsMenu)

*!**************************************************************************
*!
*!      Function: lfvMovDn
*!
*!**************************************************************************
*
*** Valid function of the push button <Move down>. ***

FUNCTION lfvMovDn

IF lsMenu  = 0
  RETURN
ENDIF

lnCBarNo   = GETBAR("puMenu",lsMenu)
lcCPrompt  = PRMBAR("puMenu",lnCBarNo)
lnCAryNo   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)-3,3)))

IF lnCAryNo = 0
  RETURN
ENDIF

lnCLevel   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)+1)))
lnCrLevel  = lnCLevel
lcCBarType = laMenu[lnCAryNo,lnSub_typ]

lnHotPos = AT('\<',laMenu[lnCAryNo,lnSub_Prpt])
IF lnHotPos > 0
  lcWithout = ALLTRIM(STRTRAN(laMenu[lnCAryNo,lnSub_Prpt],'\<',''))
  lcCPrompt = STRTRAN(lcCPrompt,lcWithout,ALLTRIM(laMenu[lnCAryNo,;
              lnSub_Prpt]))
ENDIF

IF lcCBarType = 'S'
  lnFBarNo   = GETBAR("puMenu",lsMenu+1)
  lcFPrompt  = PRMBAR("puMenu",lnFBarNo)
  lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
  lnMenu     = 2
  lnFBarNo   = 1 
    
  DO WHILE lnFLevel > lnCLevel
    lnMenu     = lnMenu +1
    lnFBarNo   = GETBAR("puMenu",lsMenu+lnMenu)
    lcFPrompt  = PRMBAR("puMenu",lnFBarNo)
    lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
  ENDDO

  lnFAryNo   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)-3,3)))
  IF lnFAryNo > 0
    lcNBarType = laMenu[lnFAryNo,lnSub_typ]
  ELSE 
    _CUROBJ = OBJNUM(lsMenu)
    RETURN
  ENDIF
ELSE
  lcNPrompt  = PRMBAR("puMenu",GETBAR("puMenu",lsMenu+1))
  lnNAryNo   = INT(VAL(SUBSTR(lcNPrompt,RAT("+",lcNPrompt)-3,3)))
  lnNLevel   = INT(VAL(SUBSTR(lcNPrompt,RAT("+",lcNPrompt)+1)))
  IF lnNAryNo > 0
    lcNBarType = laMenu[lnNAryNo,lnSub_typ]
  ELSE
    _CUROBJ = OBJNUM(lsMenu)
    RETURN
  ENDIF  
ENDIF

HIDE POPUP puMenu SAVE

DO CASE
  *** Current bar is normal and next is normal
  CASE lcCBarType $ 'CB' .AND. lcNBarType $ 'CB'
    
    *** Redefine the 2 bars 
    
    lnHotPos = AT('\<',laMenu[lnNAryNo,lnSub_Prpt])
    IF lnHotPos > 0
      lcWithout = ALLTRIM(STRTRAN(laMenu[lnNAryNo,lnSub_Prpt],'\<',''))
      lcNPrompt = STRTRAN(lcNPrompt,lcWithout,ALLTRIM(laMenu[lnNAryNo,;
                  lnSub_Prpt]))
    ENDIF

    DEFINE BAR GETBAR("puMenu",lsMenu)  ;
           OF puMenu PROMPT lcNPrompt
    DEFINE BAR GETBAR("puMenu",lsMenu+1);
           OF puMenu PROMPT lcCPrompt
 
    lsMenu  = lsMenu + 1  
  
  *** Current bar is normal and next is branching
  CASE lcCBarType $ 'CB' .AND. lcNBarType = 'S'

    lnCBarNo   = GETBAR("puMenu",lsMenu)
    lcFPrompt  = PRMBAR("puMenu",GETBAR("puMenu",lsMenu+2))
    lnFAryNo   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)-3,3)))
    lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
    lnMenu     = 2
    lnFBarNo   = 1 
    
    DO WHILE lnFLevel > lnCLevel
      lnMenu     = lnMenu +1
      lnFBarNo   = GETBAR("puMenu",lsMenu+lnMenu)
      lcFPrompt  = PRMBAR("puMenu",lnFBarNo)
      lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
    ENDDO

    DEFINE BAR GETBAR("puMenu",lsMenu) OF puMenu PROMPT lcCPrompt ;
           BEFORE lnFBarNo 

    lsMenu = lsMenu + (lnMenu-1)
  
  *** Current bar is branching and next is normal
  CASE lcCBarType = 'S'  .AND. lcNBarType $ 'CB'

    lnHotPos = AT('\<',laMenu[lnFAryNo,lnSub_Prpt])
    IF lnHotPos > 0
      lcWithout = ALLTRIM(STRTRAN(laMenu[lnFAryNo,lnSub_Prpt],'\<',''))
      lcFPrompt = STRTRAN(lcFPrompt,lcWithout,ALLTRIM(laMenu[lnFAryNo,;
                  lnSub_Prpt]))
    ENDIF

    DEFINE BAR lnFBarNo OF puMenu PROMPT lcFPrompt ;
           BEFORE lnCBarNo 
    lsMenu = lsMenu + 1

  *** Current bar is branching and next is branching
  CASE lcCBarType = 'S'  .AND. lcNBarType = 'S'

    lnABarNo   = GETBAR("puMenu",lsMenu+(lnMenu-1))
    lcAPrompt  = PRMBAR("puMenu",lnABarNo)
    lnALevel   = INT(VAL(SUBSTR(lcAPrompt,RAT("+",lcAPrompt)+1)))
    lnABarNo   = 1 
    
    DO WHILE lnALevel > lnCLevel
      lnMenu     = lnMenu +1
      lnABarNo   = GETBAR("puMenu",lsMenu+lnMenu)
      lcAPrompt  = PRMBAR("puMenu",lnABarNo)
      lnALevel   = INT(VAL(SUBSTR(lcAPrompt,RAT("+",lcAPrompt)+1)))
    ENDDO

    DEFINE BAR lnCBarNo OF puMenu PROMPT lcCPrompt ;
           BEFORE lnABarNo 
    
    lnCBarNo   = GETBAR("puMenu",lsMenu)
    lcCPrompt  = PRMBAR("puMenu",lnCBarNo)
    lnCAryNo   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)-3,3)))
    lnCLevel   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)+1)))
  
    DO WHILE lnCLevel > lnCrLevel 

      lcSkipStat = IIF(SKPBAR('puMenu',lnCBarNo),'\','')
      IF lnCAryNo > 0
        lnHotPos = AT('\<',laMenu[lnCAryNo,lnSub_Prpt])
        IF lnHotPos > 0
          lcWithout = ALLTRIM(STRTRAN(laMenu[lnCAryNo,lnSub_Prpt],'\<',''))
          lcCPrompt = STRTRAN(lcCPrompt,lcWithout,ALLTRIM(laMenu[lnCAryNo,;
                      lnSub_Prpt]))
        ENDIF
      ENDIF  

      DEFINE BAR lnCBarNo OF puMenu PROMPT lcSkipStat+lcCPrompt ;
                 BEFORE lnABarNo 

      lnMenu     = lnMenu - 1
      lnCBarNo   = GETBAR("puMenu",lsMenu)
      lcCPrompt  = PRMBAR("puMenu",lnCBarNo)
      lnCAryNo   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)-3,3)))
      lnCLevel   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)+1)))
    ENDDO

    lsMenu = lsMenu + (lnMenu-1)

ENDCASE

*** Flag to say that the current ***
*** module need to be saved. ***
llSaved = .F.

SHOW POPUP pumenu
SHOW GET   lsMenu
_CUROBJ = OBJNUM(lsMenu)

lnNewBar = lsMenu

*!**************************************************************************
*!
*!      Function: lfvMovUp
*!
*!**************************************************************************
*
*** Valid function of the push button <Move up>. ***

FUNCTION lfvMovUp

IF lsMenu  = 0
  RETURN
ENDIF

lnCBarNo   = GETBAR("puMenu",lsMenu)
lcCPrompt  = PRMBAR("puMenu",lnCBarNo)
lnCAryNo   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)-3,3)))

IF lnCAryNo = 0
  RETURN
ENDIF

lnCLevel   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)+1)))
lnCrLevel  = lnCLevel
lcCBarType = laMenu[lnCAryNo,lnSub_typ]

lnHotPos = AT('\<',laMenu[lnCAryNo,lnSub_Prpt])
IF lnHotPos > 0
  lcWithout = ALLTRIM(STRTRAN(laMenu[lnCAryNo,lnSub_Prpt],'\<',''))
  lcCPrompt = STRTRAN(lcCPrompt,lcWithout,ALLTRIM(laMenu[lnCAryNo,;
              lnSub_Prpt]))
ENDIF

IF lcCBarType = 'S'
  lnFBarNo   = GETBAR("puMenu",lsMenu-1)
  lcFPrompt  = PRMBAR("puMenu",lnFBarNo)
  lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
  lnMenu     = 2
  lnFBarNo   = 1 
    
  DO WHILE lnFLevel > lnCLevel
    lnMenu     = lnMenu +1
    lnFBarNo   = GETBAR("puMenu",lsMenu-lnMenu)
    lcFPrompt  = PRMBAR("puMenu",lnFBarNo)
    lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
  ENDDO

  lnFAryNo   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)-3,3)))
  IF lnFAryNo > 0
    lcNBarType = laMenu[lnFAryNo,lnSub_typ]
  ELSE 
    IF lnFLevel > lnCLevel
      lcNBarType = "S" 
    ELSE
      _CUROBJ = OBJNUM(lsMenu)
      RETURN
    ENDIF
  ENDIF
ELSE
  lcNPrompt  = PRMBAR("puMenu",GETBAR("puMenu",lsMenu-1))
  lnNAryNo   = INT(VAL(SUBSTR(lcNPrompt,RAT("+",lcNPrompt)-3,3)))
  lnNLevel   = INT(VAL(SUBSTR(lcNPrompt,RAT("+",lcNPrompt)+1)))
  IF lnNAryNo > 0
    lcNBarType = laMenu[lnNAryNo,lnSub_typ]
  ELSE
    IF lnNLevel > lnCLevel
      lcNBarType = "S" 
    ELSE
      _CUROBJ = OBJNUM(lsMenu)
      RETURN
    ENDIF
  ENDIF  
ENDIF

HIDE POPUP puMenu SAVE

DO CASE
  *** Current bar is normal and next is normal
  CASE lcCBarType $ 'CB' .AND. lcNBarType $ 'CB'
    
    *** Redefine the 2 bars 
    
    lnHotPos = AT('\<',laMenu[lnNAryNo,lnSub_Prpt])
    IF lnHotPos > 0
      lcWithout = ALLTRIM(STRTRAN(laMenu[lnNAryNo,lnSub_Prpt],'\<',''))
      lcNPrompt = STRTRAN(lcNPrompt,lcWithout,ALLTRIM(laMenu[lnNAryNo,;
                  lnSub_Prpt]))
    ENDIF

    DEFINE BAR GETBAR("puMenu",lsMenu)  ;
           OF puMenu PROMPT lcNPrompt
    DEFINE BAR GETBAR("puMenu",lsMenu-1);
           OF puMenu PROMPT lcCPrompt
 
    lsMenu  = lsMenu - 1
  
  *** Current bar is normal and next is branching
  CASE lcCBarType $ 'CB' .AND. lcNBarType = 'S'

    lnCBarNo   = GETBAR("puMenu",lsMenu)
    lcFPrompt  = PRMBAR("puMenu",GETBAR("puMenu",lsMenu-2))
    lnFAryNo   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)-3,3)))
    lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
    lnMenu     = 2
    lnFBarNo   = 1 
    
    DO WHILE lnFLevel > lnCLevel
      lnMenu     = lnMenu +1
      lnFBarNo   = GETBAR("puMenu",lsMenu-lnMenu)
      lcFPrompt  = PRMBAR("puMenu",lnFBarNo)
      lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
    ENDDO

    DEFINE BAR GETBAR("puMenu",lsMenu) OF puMenu PROMPT lcCPrompt ;
           BEFORE lnFBarNo 

    lsMenu = lsMenu - lnMenu
  
  *** Current bar is branching and next is normal
  CASE lcCBarType = 'S'  .AND. lcNBarType $ 'CB'

    lnHotPos = AT('\<',laMenu[lnFAryNo,lnSub_Prpt])
    IF lnHotPos > 0
      lcWithout = ALLTRIM(STRTRAN(laMenu[lnFAryNo,lnSub_Prpt],'\<',''))
      lcCPrompt = STRTRAN(lcFPrompt,lcWithout,ALLTRIM(laMenu[lnFAryNo,;
                  lnSub_Prpt]))
    ENDIF

    lnCBarNo   = GETBAR("puMenu",lsMenu)
    lcFPrompt  = PRMBAR("puMenu",GETBAR("puMenu",lsMenu+2))
    lnFAryNo   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)-3,3)))
    lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
    lnMenu     = 2
    lnFBarNo   = 1 
    
    DO WHILE lnFLevel > lnCLevel
      lnMenu     = lnMenu +1
      lnFBarNo   = GETBAR("puMenu",lsMenu+lnMenu)
      lcFPrompt  = PRMBAR("puMenu",lnFBarNo)
      lnFLevel   = INT(VAL(SUBSTR(lcFPrompt,RAT("+",lcFPrompt)+1)))
    ENDDO

    DEFINE BAR GETBAR("puMenu",lsMenu-1) OF puMenu PROMPT lcCPrompt ;
           BEFORE lnFBarNo 

    lsMenu = lsMenu - 1

  *** Current bar is branching and next is branching
  CASE lcCBarType = 'S'  .AND. lcNBarType = 'S'

    DEFINE BAR lnCBarNo OF puMenu PROMPT lcCPrompt ;
           BEFORE lnFBarNo

    lnCBarNo   = GETBAR("puMenu",lsMenu+1)
    lcCPrompt  = PRMBAR("puMenu",lnCBarNo)
    lnCAryNo   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)-3,3)))
    lnCLevel   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)+1)))

    lnMenu = 1
    DO WHILE lnCLevel > lnCrLevel 

      lcSkipStat = IIF(SKPBAR('puMenu',lnCBarNo),'\','')
      IF lnCAryNo > 0
        lnHotPos = AT('\<',laMenu[lnCAryNo,lnSub_Prpt])
        IF lnHotPos > 0
          lcWithout = ALLTRIM(STRTRAN(laMenu[lnCAryNo,lnSub_Prpt],'\<',''))
          lcCPrompt = STRTRAN(lcCPrompt,lcWithout,ALLTRIM(laMenu[lnCAryNo,;
                      lnSub_Prpt]))
        ENDIF
      ENDIF  

      DEFINE BAR lnCBarNo OF puMenu PROMPT lcSkipStat+lcCPrompt ;
                 BEFORE lnFBarNo

      lnMenu     = lnMenu + 1
      lnCBarNo   = GETBAR("puMenu",lsMenu+lnMenu)
      lcCPrompt  = PRMBAR("puMenu",lnCBarNo)
      lnCAryNo   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)-3,3)))
      lnCLevel   = INT(VAL(SUBSTR(lcCPrompt,RAT("+",lcCPrompt)+1)))
    ENDDO

    lsMenu = lsMenu - (lnMenu - 2)

ENDCASE

*** Flag to say that the current ***
*** module need to be saved. ***
llSaved = .F.

SHOW POPUP pumenu
SHOW GET   lsMenu
_CUROBJ = OBJNUM(lsMenu)

lnNewBar = lsMenu

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

DECLARE laMenu[1,ALEN(laMenu,2)]
laMenu       = " "
*E300581,1 Hesham El-Sheltawi (Start)
*SELECT CAPP_ID,CPAD_POS,CPOP_POS,CBAR_POS,;
       CPOP_LEVL,CSUB_PRPT,CSUB_TYP,CSUB_CTG,;
       CSUB_POS,CMSTR_NAM,CSUB_MSG,CSUB_HKEY,;
       CPROSS_ID,CUPGRDLVL,CPROCTYPE,CPROCPATH,LDEFAULT;
       FROM (gcWorkDir+lcMenu_Tmp) ;
       WHERE CAPP_ID+CPAD_POS+CPOP_POS+CPOP_LEVL+CBAR_POS = lcApp_Id ;
       ORDER BY CAPP_ID,CPAD_POS,CPOP_POS,CPOP_LEVL,CBAR_POS ;
       INTO ARRAY laMenu

*B603560,1 SSH There is a field cBarModule in the SycMenu is getting blanked 
*B603560,1 SSH upon saving.

*SELECT CAPP_ID,CPAD_POS,CPOP_POS,CBAR_POS,;
       CPOP_LEVL,CSUB_PRPT,CSUB_TYP,CSUB_CTG,;
       CSUB_POS,CMSTR_NAM,CSUB_MSG,CSUB_HKEY,;
       CPROSS_ID,CUPGRDLVL,CPROCTYPE,CPROCPATH,LDEFAULT,cMenuParam;
       FROM (gcWorkDir+lcMenu_Tmp) ;
       WHERE CAPP_ID+CPAD_POS+CPOP_POS+CPOP_LEVL+CBAR_POS = lcApp_Id ;
       ORDER BY CAPP_ID,CPAD_POS,CPOP_POS,CPOP_LEVL,CBAR_POS ;
       INTO ARRAY laMenu
SELECT CAPP_ID,CPAD_POS,CPOP_POS,CBAR_POS,;
       CPOP_LEVL,CSUB_PRPT,CSUB_TYP,CSUB_CTG,;
       CSUB_POS,CMSTR_NAM,CSUB_MSG,CSUB_HKEY,;
       CPROSS_ID,CUPGRDLVL,CPROCTYPE,CPROCPATH,LDEFAULT,cMenuParam,cBarModule;
       FROM (gcWorkDir+lcMenu_Tmp) ;
       WHERE CAPP_ID+CPAD_POS+CPOP_POS+CPOP_LEVL+CBAR_POS = lcApp_Id ;
       ORDER BY CAPP_ID,CPAD_POS,CPOP_POS,CPOP_LEVL,CBAR_POS,cBarModule ;
       INTO ARRAY laMenu

*B603560,1 SSH [End]

*E300581,1 Hesham El-Sheltawi (End)

lnPad_Pos  = 2           && Col no. of pad pos
lnPop_Pos  = 3           && Col no. of popup pos
lnBar_Pos  = 4           && Col no. of bar pos
lnPop_Levl = 5           && Col no. of popup level
lnSub_Prpt = 6           && Col no. of text prompt
lnSub_Typ  = 7           && Col no. of type
lnSub_Ctg  = 8           && Col no. of category
lnMstr_nam = 10          && Col no. of master name
lnBar_Msg  = 11          && Col no. of Bar message
lnHot_key  = 12          && Col no. of Bar hot key
lnProssId  = 13          && Col no. of prosses ID 
lnProcType = 15          && Col no. of process type (P,S,E,R,G,M)
lnProcPath = 16          && Col no. of process path
llDefault  = 17          && Col no. of default bar
*E300581,1 Hesham El-Shetlatwi (Start)
lnParamPos = 18          && Col no. of program parameters
*E300581,1 Hesham El-Sheltawi (End)

*B603564,1 Reham On 04/27/2000    *** Start ***
lnUpgrade  = 14          && Col. No. of the upgrade level
*B603564,1 Reham On 04/27/2000    *** End   ***

*B603560,1 Reham On 04/27/2000    *** Start ***
lnBarMod   = 19          && Col. No. of the bar Module that filled for the Edi Module almost.
*B603560,1 Reham On 04/27/2000    *** End   ***


*** Define the prompt of the popups if the current ***
*** module is the main system or not...
IF lcApp_Id = "SY"
  lcPrmp1   = "System P\<rg."
  lcPrmp2   = "System \<Bar"
  lcPrmp4   = "\<Custom Prg."
  SHOW GET rbSysPrg,1 PROMPT lcPrmp1
  SHOW GET rbSysPrg,2 PROMPT lcPrmp2
  SHOW GET rbSysPrg,4 PROMPT lcPrmp4
ELSE
  lcPrmp1   = "System P\<rg."
  lcPrmp2   = "E\<xternal Prg."
  lcPrmp4   = "\<Custom Prg."
  SHOW GET rbSysPrg,1 PROMPT lcPrmp1
  SHOW GET rbSysPrg,2 PROMPT lcPrmp2
  SHOW GET rbSysPrg,4 PROMPT lcPrmp4
ENDIF

*** If there is no data in the menu file for the selected module. ***
IF EMPTY(laMenu[1,1])
  IF lcApp_Id = "SY"
    *** 18 elements to define 9 pads. ***
    DECLARE laMenu[19,ALEN(laMenu,2)]
  ELSE
    *** 6 elements to define 3 pads. ***
    DECLARE laMenu[6,ALEN(laMenu,2)]
  ENDIF
  
  lnCont8     = 1
  lnCont9     = 1

  laMenu      = " "
  DO WHILE .T.
    IF lnCont8 >= ALEN(laMenu,1)
      EXIT
    ENDIF
    
    laMenu[lnCont8,1]            = lcApp_Id
    laMenu[lnCont8,lnPad_Pos]    = IIF(lcApp_Id = "SY",;
                                   laDefSPop[lnCont9,2],laDefPop[lnCont9,2])
    
    laMenu[lnCont8,lnSub_Prpt]   = IIF(lcApp_Id = "SY",;
                                   ALLTRIM(laDefSPop[lnCont9,1]),;
                                   ALLTRIM(laDefPop[lnCont9,1]))+;
                                   SPACE(41-IIF(lcApp_Id = "SY",;
                                   LEN(laDefSPop[lnCont9,1]),;
                                   LEN(laDefPop[lnCont9,1])))
    laMenu[lnCont8,lnSub_Typ]    = "P"
    laMenu[lnCont8,llDefault]    = .F.
    
    laMenu[lnCont8+1,1]          = lcApp_Id
    laMenu[lnCont8+1,lnPad_Pos]  = IIF(lcApp_Id = "SY",;
                                   laDefSPop[lnCont9,2],laDefPop[lnCont9,2])
    laMenu[lnCont8+1,lnPop_Pos]  = "01"
    laMenu[lnCont8+1,lnBar_Pos]  = "01"
    
    IF lcApp_Id = "SY"
      DO CASE
        CASE VAL(laMenu[lnCont8+1,lnPad_Pos]) = 1
          laMenu[lnCont8+1,lnPop_Levl] = "01"
          laMenu[lnCont8+1,lnSub_Prpt] = "\<Exit"+SPACE(35)
          laMenu[lnCont8+1,lnSub_Typ]  = "C"
          laMenu[lnCont8+1,lnProcType] = "G"
          laMenu[lnCont8+1,lnProssId]  = "GPEXIT"
          laMenu[lnCont8+1,llDefault]  = .T.
        CASE VAL(laMenu[lnCont8+1,lnPad_Pos]) = 6
          laMenu[lnCont8+1,lnPop_Levl] = "01"
          laMenu[lnCont8+1,lnSub_Prpt] = "\<Select company"+SPACE(25)
          laMenu[lnCont8+1,lnSub_Typ]  = "S"
          laMenu[lnCont8+1,llDefault]  = .T.
          laMenu[lnCont8+1,lnProssId]  = "_COMPANIES"

          lnCont8 = lnCont8 + 1
          laMenu[lnCont8+1,1]          = lcApp_Id
          laMenu[lnCont8+1,lnPad_Pos]  = laDefSPop[lnCont9,2]
          laMenu[lnCont8+1,lnPop_Pos]  = "02"
          laMenu[lnCont8+1,lnBar_Pos]  = "01"
          laMenu[lnCont8+1,lnPop_Levl] = "02"
          laMenu[lnCont8+1,lnSub_Prpt] = "\<No companies available"+SPACE(17)
          laMenu[lnCont8+1,lnSub_Typ]  = "C"
          laMenu[lnCont8+1,llDefault]  = .T.
          laMenu[lnCont8+1,lnMstr_nam] = "_COMPANIES"
        OTHERWISE
          laMenu[lnCont8+1,lnPop_Levl] = "01"
          laMenu[lnCont8+1,lnSub_Prpt] = SPACE(41)
          laMenu[lnCont8+1,lnSub_Typ]  = "C"
          laMenu[lnCont8+1,llDefault]  = .F.
      ENDCASE
    ELSE
      laMenu[lnCont8+1,lnPop_Levl] = "01"
      laMenu[lnCont8+1,lnSub_Prpt] = SPACE(41)
      laMenu[lnCont8+1,lnSub_Typ]  = "C"
      laMenu[lnCont8+1,llDefault]  = .F.
    ENDIF
    
    lnCont8 = lnCont8 + 2
    lnCont9 = lnCont9 + 1
    
  ENDDO
  
  *** Flag to say that the current ***
  *** module need to be saved. ***
  llSaved = .F.
ENDIF

*!**************************************************************************
*!
*!      Function: lfMDrag
*!
*!**************************************************************************
*
*** Funcion called if the mouse moved inside the list up or down. ***

FUNCTION lfMDrag

IF SYS(18)='LSMENU'     .AND. ;
   BETWEEN(MROW(),3,15) .AND. ;
   BETWEEN(MCOL(),1,59) 

*  IF llFirstm
*    llFirstm = .F.  
*    KEYBOARD CHR(151)
*    SHOW GET lsMenu
*    RETURN
*  ENDIF  
*  llFirstm = .T.  

  lnCurRow = MROW()
  DO WHILE llMdown
    DO CASE
      CASE MROW() > lnCurRow 
        lnCurRow = MROW()
        =lfvMovDn()
        lnMouse  = INKEY(0.01,'M')
      CASE MROW() < lnCurRow 
        lnCurRow = MROW()
        =lfvMovUp()
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

*** Call the move down function. ***
=lfvMovDn()

*!**************************************************************************
*!
*!      Function: lfCtrUp
*!
*!**************************************************************************
*
*** Function called if press CTRL+ARROW UP. ***

FUNCTION lfCtrUp

*** Call the move up function. ***

=lfvMovUp()

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

DO CASE
  CASE _DOS
    IF !llDontHid
      HIDE POPUP puMenu SAVE
    ENDIF  

    lcWhichPad = ""
    lnBarNo    = 1
    lnBarLevel = 1
    lnNewPlvl  = 1

    llFrst_Pad  = .T.

    RELEASE BAR  ALL OF puMenu

    DEFINE BAR lnBarNo OF puMenu PROMPT "\"+;
           REPLICATE(lcHfBloc,70) COLOR SCHEME 24

    lnBarNo = lnBarNo + 1

    FOR lnCount = 1 TO ALEN(laMenu,1)
      lnNewPlvl  = INT(VAL(laMenu[lnCount,lnPop_Levl]))

      DO CASE
        *** New pad 
        CASE laMenu[lnCount,lnSub_Typ] = "P"

          *** If defining new pad dont close any thing from before 
          IF llFrst_Pad
            *** Turn the flag off so next time we will close previous
            *** poups
            llFrst_Pad = .F.
          ELSE
            *** If the previous pad have one or more opend popup and
            *** not closed yet close them all 
            IF lnBarLevel > 1
              =lfClosPops()
            ENDIF

            *** Close any previous pad. ***
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   "\"+lcHfBloc+lcHfBloc+lcDnLeft+;
                   REPLICATE(lcHzLine,42)+lcDnRght+;
                   lcBlock+lcBlock+lcHfBlocs+"+"+;
                   STR(lnBarLevel,1) COLOR SCHEME 24

            lnBarNo = lnBarNo + 1

            *** Shadow of the previous pad. ***
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   "\"+REPLICATE(lcHfBloc,4) + ;
                   REPLICATE(lcBlock,44)+lcHfBlocs+"+"+;
                   STR(lnBarLevel,1) COLOR SCHEME 24

            lnBarNo = lnBarNo + 1
      
            *** Define a separator between each pad. ***
            DEFINE BAR lnBarNo OF puMenu PROMPT "\"+;
                   REPLICATE(lcHfBloc,70) COLOR SCHEME 24

            lnBarNo = lnBarNo + 1

            DEFINE BAR lnBarNo OF puMenu PROMPT "\-" COLOR SCHEME 24

            lnBarNo = lnBarNo + 1

            DEFINE BAR lnBarNo OF puMenu PROMPT "\"+;
                   REPLICATE(lcHfBloc,70) COLOR SCHEME 24

            lnBarNo = lnBarNo + 1
          ENDIF
      
          lcWhichPad = "^"+ALLTRIM(STR(VAL(laMenu[lnCount,lnPad_Pos])))

          *** define text of the current pad. ***
          DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 lcHfBloc+SUBSTR(laMenu[lnCount,lnSub_Prpt],1,41)+;
                 IIF('\<' $ laMenu[lnCount,lnSub_Prpt],"  ","")+;
                 lcBranch+lcHfBlocs+lcWhichPad+RIGHT(STR(lnCount),3)+;
                 "+"+SPACE(1) COLOR SCHEME 24
          lnBarNo = lnBarNo + 1

          *** Start the separator of the current pad. ***
          DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 "\"+lcHfBloc+lcHfBloc+lcUpLeft+;
                 REPLICATE(lcHzLine,42)+lcUpRght+;
                 lcHfBlocs+"+"+STR(lnBarLevel,1) ;
                 COLOR SCHEME 24

          lnBarNo = lnBarNo + 1

          *** Branching bar. ***
        CASE laMenu[lnCount,lnSub_Typ] = "S"
          *** Check if there is any opend popup on a higher level than ***
          *** this new branching bar...
          IF lnNewPlvl < lnBarLevel
            =lfClosPops()
          ENDIF
          *** Define the branching bar with branching mark  ***
          DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 lcHfBloc+lcHfBloc+;
                 IIF(lnBarLevel = 1,lcVrLine,;
                 REPLICATE(lcVrLine+"  ",lnBarLevel-1)+lcVrLine)+;
                 SUBSTR(laMenu[lnCount,lnSub_Prpt],1,41) + ;
                 IIF('\<' $ laMenu[lnCount,lnSub_Prpt],"  ","")+;
                 lcBranch+lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
                 RIGHT(STR(lnCount),3)+"+"+STR(lnBarLevel,1) COLOR SCHEME 24
             
          lnBarNo    = lnBarNo    + 1

          lnBarLevel = lnBarLevel + 1
      
          DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 "\"+lcHfBloc+lcHfBloc+;
                 REPLICATE(lcVrLine+"  ",lnBarLevel-1)+;
                 lcUpLeft+REPLICATE(lcHzLine,42)+;
                 lcUpRght+lcHfBlocs+lcWhichPad+"+"+STR(lnBarLevel,1) ;
                 COLOR SCHEME 24
      
          lnBarNo = lnBarNo + 1
      
          *** Normal bar or separator. ***
        OTHERWISE

          *** Check if there is any opend popup on a higher level than ***
          *** this new branching bar...
          IF lnNewPlvl < lnBarLevel
            =lfClosPops()
          ENDIF

          IF ALLTRIM(laMenu[lnCount,lnSub_Prpt]) = "\-"
            *** Separator. ***
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   lcHfBloc+lcHfBloc+;
                   IIF(lnBarLevel=1,"",REPLICATE(lcVrLine+"  ",lnBarLevel-1))+;
                   lcLnLeft+REPLICATE(lcHzLine,42)+ ;
                   lcLnRght+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+;
                   RIGHT(STR(lnCount),3)+"+"+;
                   STR(lnBarLevel,1) COLOR SCHEME 24
          ELSE
            *** Normal bar
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   lcHfBloc+lcHfBloc+lcVrLine+;
                   IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
                   SUBSTR(laMenu[lnCount,lnSub_Prpt],1,41)+;
                   IIF('\<' $ laMenu[lnCount,lnSub_Prpt],"   "," ")+;
                   lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
                   RIGHT(STR(lnCount),3)+"+"+STR(lnBarLevel,1) ;
                   COLOR SCHEME 24
          ENDIF
          lnBarNo = lnBarNo + 1
      ENDCASE
    ENDFOR

    IF lnBarLevel > 1
      *** Call the function to close any popus. ***
      lnNewPlvl  = 1
      =lfClosPops()
    ENDIF

    *** Close any previous pad. ***
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           "\"+lcHfBloc+lcHfBloc+lcDnLeft+;
           REPLICATE(lcHzLine,42)+lcDnRght+;
           lcBlock+lcBlock+lcHfBlocs+lcWhichPad+"+"+;
           STR(lnBarLevel,1) COLOR SCHEME 24

    lnBarNo = lnBarNo + 1

    *** Shadow of the previous pad. ***
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           "\"+REPLICATE(lcHfBloc,4) + ;
           REPLICATE(lcBlock,44)+lcHfBlocs+lcWhichPad+"+"+;
           STR(lnBarLevel,1) COLOR SCHEME 24
    lnBarNo = lnBarNo + 1

    DEFINE BAR lnBarNo OF puMenu PROMPT "\"+;
           REPLICATE(lcHfBloc,70) COLOR SCHEME 24

    lsMenu  = 2

    IF !llDontHid
      SHOW POPUP puMenu
    ELSE
      llDontHid = .F.
    ENDIF  
  **********************************************************************
  CASE _WINDOWS
    IF !llDontHid
      HIDE POPUP puMenu SAVE
    ENDIF  

    lcWhichPad = ""
    lnBarNo    = 1
    lnBarLevel = 1
    lnNewPlvl  = 1

    llFrst_Pad  = .T.

    RELEASE BAR  ALL OF puMenu

    FOR lnCount = 1 TO ALEN(laMenu,1)
      lnNewPlvl  = INT(VAL(laMenu[lnCount,lnPop_Levl]))

      DO CASE
        *** New pad 
        CASE laMenu[lnCount,lnSub_Typ] = "P"

          *** If defining new pad dont close any thing from before 
          IF llFrst_Pad
            *** Turn the flag off so next time we will close previous
            *** poups
            llFrst_Pad = .F.
          ELSE
            *** If the previous pad have one or more opend popup and
            *** not closed yet close them all 
            IF lnBarLevel > 1
              =lfClosPops()
            ENDIF

            *** Close any previous pad. ***
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   lcHfBloc+lcHfBloc+lcDnLeft+;
                   REPLICATE(lcHzLine,42)+lcDnRght+;
                   lcBlock+lcBlock+lcHfBlocs+"+"+;
                   STR(lnBarLevel,1) COLOR RGB(0,0,0,255,255,255)

            SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.
            
            lnBarNo = lnBarNo + 1

            DEFINE BAR lnBarNo OF puMenu PROMPT "\-"

            lnBarNo = lnBarNo + 1

          ENDIF
      
          lcWhichPad = "^"+ALLTRIM(STR(VAL(laMenu[lnCount,lnPad_Pos])))

          *** define text of the current pad. ***
          DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 lcHfBloc+SUBSTR(laMenu[lnCount,lnSub_Prpt],1,41)+;
                 IIF('\<' $ laMenu[lnCount,lnSub_Prpt],"  ","")+;
                 lcBranch+lcHfBlocs+lcWhichPad+RIGHT(STR(lnCount),3)+;
                 "+"+SPACE(1)
          lnBarNo = lnBarNo + 1

          *** Start the separator of the current pad. ***
          DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 lcHfBloc+lcHfBloc+lcUpLeft+;
                 REPLICATE(lcHzLine,42)+lcUpRght+;
                 lcHfBlocs+"+"+STR(lnBarLevel,1) ;
                 COLOR RGB(0,0,0,255,255,255)

          SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.
          
          lnBarNo = lnBarNo + 1

          *** Branching bar. ***
        CASE laMenu[lnCount,lnSub_Typ] = "S"
          *** Check if there is any opend popup on a higher level than ***
          *** this new branching bar...
          IF lnNewPlvl < lnBarLevel
            =lfClosPops()
          ENDIF
          *** Define the branching bar with branching mark  ***
          DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 lcHfBloc+lcHfBloc+;
                 IIF(lnBarLevel = 1,lcVrLine,;
                 REPLICATE(lcVrLine+"  ",lnBarLevel-1)+lcVrLine)+;
                 SUBSTR(laMenu[lnCount,lnSub_Prpt],1,41) + ;
                 IIF('\<' $ laMenu[lnCount,lnSub_Prpt],"  ","")+;
                 lcBranch+lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
                 RIGHT(STR(lnCount),3)+"+"+STR(lnBarLevel,1)
             
          lnBarNo    = lnBarNo    + 1

          lnBarLevel = lnBarLevel + 1
      
          DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 lcHfBloc+lcHfBloc+;
                 REPLICATE(lcVrLine+"  ",lnBarLevel-1)+;
                 lcUpLeft+REPLICATE(lcHzLine,42)+;
                 lcUpRght+lcHfBlocs+lcWhichPad+"+"+;
                 STR(lnBarLevel,1) COLOR RGB(0,0,0,255,255,255)
      
          SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.
          
          lnBarNo = lnBarNo + 1
      
          *** Normal bar or separator. ***
        OTHERWISE

          *** Check if there is any opend popup on a higher level than ***
          *** this new branching bar...
          IF lnNewPlvl < lnBarLevel
            =lfClosPops()
          ENDIF

          IF ALLTRIM(laMenu[lnCount,lnSub_Prpt]) = "\-"
            *** Separator. ***
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   lcHfBloc+lcHfBloc+;
                   IIF(lnBarLevel=1,"",REPLICATE(lcVrLine+"  ",lnBarLevel-1))+;
                   lcLnLeft+REPLICATE(lcHzLine,42)+ ;
                   lcLnRght+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+;
                   RIGHT(STR(lnCount),3)+"+"+;
                   STR(lnBarLevel,1)
          ELSE
            *** Normal bar
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   lcHfBloc+lcHfBloc+lcVrLine+;
                   IIF(lnBarLevel=1,"",REPLICATE("  "+lcVrLine,lnBarLevel-1))+;
                   SUBSTR(laMenu[lnCount,lnSub_Prpt],1,41)+;
                   IIF('\<' $ laMenu[lnCount,lnSub_Prpt],"   "," ")+;
                   lcVrLine+lcBlock+lcBlock+lcHfBlocs+lcWhichPad+ ;
                   RIGHT(STR(lnCount),3)+"+"+STR(lnBarLevel,1)
          ENDIF
          lnBarNo = lnBarNo + 1
      ENDCASE
    ENDFOR

    IF lnBarLevel > 1
      *** Call the function to close any popus. ***
      lnNewPlvl  = 1
      =lfClosPops()
    ENDIF

    *** Close any previous pad. ***
    DEFINE BAR lnBarNo OF puMenu PROMPT ;
           lcHfBloc+lcHfBloc+lcDnLeft+;
           REPLICATE(lcHzLine,42)+lcDnRght+;
           lcBlock+lcBlock+lcHfBlocs+lcWhichPad+"+"+;
           STR(lnBarLevel,1) COLOR RGB(0,0,0,255,255,255)

    SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.
    
    lsMenu  = 1

    IF !llDontHid
      SHOW POPUP puMenu
    ELSE
      llDontHid = .F.
    ENDIF  
ENDCASE

SHOW GET lsMenu
_CUROBJ = OBJNUM(lsMenu)

*!**************************************************************************
*!
*!      Function: lfClosPops
*!
*!**************************************************************************
*
*** This funcion called from building the popup function ***
*** to close any openning popups...

FUNCTION lfClosPops

DO CASE
  CASE _DOS
    *** Close all the opened popups on levels ***
    *** greater than the current level. ***
    FOR lnCont1 = lnBarLevel TO lnNewPlvl+1  STEP - 1

      *** Incase of creating new pad while we have 1 or more ***
      *** opened popups greater than level 1 in previous pad ***
      *** Terminate the loop before level goes down to 0.
      IF lnCont1 <= 1
        EXIT
      ENDIF

      *** Close any popup. ***
      DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 "\"+lcHfBloc+lcHfBloc+;
                 REPLICATE(lcVrLine+"  ",lnBarLevel-1)+;
                 lcDnLeft+REPLICATE(lcHzLine,42)+lcDnRght+;
                 lcBlock+lcBlock+lcHfBlocs+lcWhichPad+"+"+;
                 STR(lnBarLevel,1) COLOR SCHEME 24
      lnBarNo = lnBarNo + 1
      *** Shadow of any popup. ***
      DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 "\"+REPLICATE(lcHfBloc,2)+;
                 REPLICATE(lcVrLine+"  ",lnBarLevel-1)+;
                 SPACE(2)+REPLICATE(lcBlock,44)+;
                 lcHfBlocs+lcWhichPad+"+"+STR(lnBarLevel,1) COLOR SCHEME 24
      lnBarNo = lnBarNo + 1

      *** Current level is moving down till we hit the new current level. ***
      *** This variable should reach only to the new current level not below ***
      lnBarLevel = lnBarLevel - 1
    ENDFOR
  CASE _WINDOWS
    *** Close all the opened popups on levels ***
    *** greater than the current level. ***
    FOR lnCont1 = lnBarLevel TO lnNewPlvl+1  STEP - 1

      *** Incase of creating new pad while we have 1 or more ***
      *** opened popups greater than level 1 in previous pad ***
      *** Terminate the loop before level goes down to 0.
      IF lnCont1 <= 1
        EXIT
      ENDIF

      *** Close any popup. ***
      DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 lcHfBloc+lcHfBloc+;
                 REPLICATE(lcVrLine+"  ",lnBarLevel-1)+;
                 lcDnLeft+REPLICATE(lcHzLine,42)+lcDnRght+;
                 lcBlock+lcBlock+lcHfBlocs+lcWhichPad+"+"+;
                 STR(lnBarLevel,1) COLOR RGB(0,0,0,255,255,255)
      SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.
      lnBarNo = lnBarNo + 1

      *** Current level is moving down till we hit the new current level. ***
      *** This variable should reach only to the new current level not below ***
      lnBarLevel = lnBarLevel - 1
    ENDFOR
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvSav_App
*!
*!**************************************************************************
*
*** Function called to save a specific application. ***
*** Parameter is application ID. ***

FUNCTION lfvSav_App
PARAMETERS lcApp2Sav

*** No need to save if this module ***
*** was saved before...
IF llSaved
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

*B603560,1 SSH There is a field cBarModule in the SycMenu is getting blanked 
*B603560,1 SSH upon saving.
*DECLARE laStruc[1,18]
DECLARE laStruc[1,19]
*B603560,1 SSH [End]
laStruc    = " "
lnCont10   = 0
lnCPop_pos = 1
lnAryRow   = 1
lnCurLevel = 0

lnCurBpos  = 1
STORE 0 TO lnOldBpos1,lnOldBpos2,lnOldBpos3,lnOldBpos4,lnOldBpos5

lnCurSpos  = 0
STORE 0 TO lnOldSpos1,lnOldSpos2,lnOldSpos3,lnOldSpos4,lnOldSpos5

lnPross_No = 0

lcMast_nam = ""
lcPross_Id = ""

FOR lnCount = 1 TO CNTBAR("puMenu")

  =gfThermo(CNTBAR("puMenu"),lnCount,"Saving current module to the temp. file...","")

  *** Get the phisical no. of the bar. ***
  lnBarNo  = GETBAR("puMenu",lnCount)
  
  *** Get the prompt of the bar. ***
  lcPrompt = PRMBAR("puMenu",lnBarNo)
  
  *** Get the array no. of the bar. ***
  lnAryNo  = INT(VAL(SUBSTR(lcPrompt,RAT("+",lcPrompt)-3,3)))
  
  *** Get the level of the current bar. ***
  lnBarLevel  = INT(VAL(SUBSTR(lcPrompt,RAT("+",lcPrompt)+1)))
  
  IF lnAryNo <> 0
    lnCont10 = IIF(laMenu[lnAryNo,lnSub_Typ]="P",lnCont10+1,lnCont10)
    IF !EMPTY(laStruc[1,1])
      lnAryRow = ALEN(laStruc,1)+1
      *B603560,1 SSH There is a field cBarModule in the SycMenu is getting blanked 
      *B603560,1 SSH upon saving.
      *DIMENSION laStruc[lnAryRow,18]
      DIMENSION laStruc[lnAryRow,19]
      *B603560,1 SSH [End]
    ENDIF
    *** Get the module ID.                         *** [1] ***
    laStruc[lnAryRow,1] = lcApp2Sav
    
    *** Get the pad position.                      *** [2] ***
    laStruc[ALEN(laStruc,1),lnPad_pos] = laMenu[lnAryNo,lnPad_Pos]
    
    *** Get the popup position. ***                *** [3] ***
    lnCPop_pos = IIF(laMenu[lnAryNo,lnSub_Typ]="P",0,lnCPop_pos + 1)
    laStruc[lnAryRow,lnPop_pos] = ;
                 IIF(laMenu[lnAryNo,lnSub_Typ]="P",;
                 "  ",PADL(ALLTRIM(STR(lnCPop_pos)),2,"0"))
    
    *** Get the bar position. ***                  *** [4] ***
    IF laMenu[lnAryNo,lnSub_Typ] = "P"
      laStruc[lnAryRow,lnBar_pos] = "  "
      laBarPos = 0
    ELSE
      IF lnBarLevel < lnCurLevel
        laBarPos[lnCurLevel] = 0
      ENDIF
      laBarPos[lnBarLevel] = laBarPos[lnBarLevel] + 1
      IF lcApp2Sav = "SY" .AND. laMenu[lnAryNo,lnPad_Pos] = "01"
        laStruc[lnAryRow,lnBar_pos] = ALLTRIM(STR(laBarPos[lnBarLevel]+50))
      ELSE
        laStruc[lnAryRow,lnBar_pos] = PADL(ALLTRIM(STR(laBarPos[lnBarLevel])),2,"0")
      ENDIF
    ENDIF
    
    *** Get the popup level. ***                   *** [5] ***
    laStruc[lnAryRow,lnPop_Levl] = ;
                 IIF(laMenu[lnAryNo,lnSub_Typ]="P","  ",;
                 PADL(ALLTRIM(laMenu[lnAryNo,lnPop_Levl]),2,"0"))
    
    *** Get the text to be save as a prompt. ***   *** [6] ***
    laStruc[lnAryRow,lnSub_prpt] = laMenu[lnAryNo,lnSub_Prpt]
    
    *** Get the type bar. ***                      *** [7] ***
    laStruc[lnAryRow,lnSub_Typ] = laMenu[lnAryNo,lnSub_Typ]
    
    *** Get the category. ***                      *** [8] ***
    IF lcApp2Sav = "SY"
      IF laMenu[lnAryNo,lnPad_pos] $ "01060709"
        laStruc[lnAryRow,lnSub_Ctg] = "I"
      ELSE
        laStruc[lnAryRow,lnSub_Ctg] = "S"
      ENDIF
    ELSE
      laStruc[lnAryRow,lnSub_Ctg] = "A"
    ENDIF
    
    *** Get the sub position. ***                  *** [9] ***
    IF laMenu[lnAryNo,lnSub_Typ] = "P"
      laStruc[lnAryRow,9] = "  "
      lnCurSpos = 0
    ELSE
      IF laMenu[lnAryNo,lnSub_Typ] = "S"
        lnOldSpos  = "lnOldSpos" + ALLTRIM(STR(lnBarLevel))
        &lnOldSpos = lnCurSpos
        lnCurSpos  = lnCurSpos + 2
      ELSE
        IF lnBarLevel < lnCurLevel
          lnCurSpos = &lnOldSpos + 1
        ELSE
          lnCurSpos = lnCurSpos + 1
        ENDIF
      ENDIF
      laStruc[lnAryRow,9] = PADL(ALLTRIM(STR(lnCurSpos)),2,"0")
    ENDIF

    *** Get the sub message. ***                   *** [11] ***
    laStruc[lnAryRow,11] = IIF(EMPTY(laMenu[lnAryNo,lnBar_Msg])," ",;
                               laMenu[lnAryNo,lnBar_Msg])
    
    *** Get the sub hot key. ***                   *** [12] ***
    IF laMenu[lnAryNo,lnSub_Typ] = "P"
      laStruc[lnAryRow,12] = "ALT+" + ;
             IIF(AT("\<",laMenu[lnAryNo,lnSub_Prpt]) > 0 ,;
             SUBSTR(laMenu[lnAryNo,lnSub_Prpt],;
             AT("\<",laMenu[lnAryNo,lnSub_Prpt])+2,1)," ")
    ELSE
      laStruc[lnAryRow,12] = IIF(EMPTY(laMenu[lnAryNo,lnHot_key])," ",;
                                 laMenu[lnAryNo,lnHot_key])
    ENDIF
    
    *** Get the process ID. ***                    *** [13] ***
    IF laMenu[lnAryNo,llDefault] .AND. VAL(laMenu[lnAryNo,lnPad_Pos]) = 6
      laStruc[lnAryRow,13] = laMenu[lnAryNo,lnProssId]
    ELSE
      IF laMenu[lnAryNo,lnSub_Typ] = "P"
        lcPross_Id = "P"  + laMenu[lnAryNo,lnPad_Pos] +;
                     "PU" + laMenu[lnAryNo,lnPad_Pos]
        lnPross_No = 0
      
        laMstr_Nam[1] = lcPross_Id
      ELSE
        IF laMenu[lnAryNo,lnSub_Typ] = "S"
          lnPross_No = lnPross_No + 1

          lcPross_Id = "P" + laMenu[lnAryNo,lnPad_Pos] + "PU" + ;
                       laMenu[lnAryNo,lnPad_Pos] + ;
                       PADL(ALLTRIM(STR(lnPross_No)),2,"0")
        
          laMstr_Nam[lnBarLevel+1] = lcPross_Id
        ENDIF
      ENDIF
      laStruc[lnAryRow,13] = IIF(laMenu[lnAryNo,lnSub_Typ] $ "SP",;
                             lcPross_Id,IIF(!EMPTY(laMenu[lnAryNo,lnProssId]),;
                             laMenu[lnAryNo,lnProssId],""))
    ENDIF
    
    *** Get the master name. ***                   *** [10] ***
    IF laMenu[lnAryNo,lnSub_Typ] = "P"
      lcMast_nam = "P" + laMenu[lnAryNo,lnPad_Pos]
    ELSE
      lcMast_nam = laMstr_Nam[lnBarLevel]
    ENDIF
    laStruc[lnAryRow,10] = IIF(!EMPTY(laMenu[lnAryNo,lnMstr_nam]);
                           .AND. laMenu[lnAryNo,llDefault],;
                           laMenu[lnAryNo,lnMstr_nam],lcMast_nam)

    *** Get the upgrade level. ***                 *** [14] ***
    IF lcApp2Sav = "SY" .AND. ;
       laMenu[lnAryNo,lnSub_Typ] <> "P" .AND. ;
       VAL(laMenu[lnAryNo,lnPad_Pos]) = 7
      laStruc[lnAryRow,14] = "A"
    ELSE
      *B603564,1 SSH add custom type.
      *laStruc[lnAryRow,14] = IIF(lcApp2Sav = "SY" .OR. ;
                                 lcApp2Sav = "SM","S","A")
      *laStruc[lnAryRow,14] = IIF(lcApp2Sav = "SY" .OR. ;
                                 lcApp2Sav = "SM","S",IIF(!EMPTY(laMenu[lnAryRow,14]),laMenu[lnAryRow,14],'A'))
      laStruc[lnAryRow,14] = IIF(lcApp2Sav = "SY" .OR. lcApp2Sav = "SM","S",laMenu[lnAryRow,14])
     *B603564,1 SSH add custom type.[End]
    ENDIF
    
    *** Get the process type. ***                *** [15] ***
    laStruc[lnAryRow,15] = IIF(EMPTY(laStruc[lnAryRow,13]),"",;
                               laMenu[lnAryNo,lnProcType])
    
    *** Get the process id path. ***             *** [16] ***
    IF laMenu[lnAryNo,lnProcType] = "E"
      laStruc[lnAryRow,16] = laMenu[lnAryNo,lnProcPath]
    ELSE
      IF laMenu[lnAryNo,lnProcType] = "R" .AND. ;
         !EMPTY(laMenu[lnAryNo,lnProcPath])
        laStruc[lnAryRow,16] = laMenu[lnAryNo,lnProcPath]
      ELSE
        laStruc[lnAryRow,16] = ""
      ENDIF
    ENDIF
    
    *** Get if this bar is a default bar or not. ***     *** [17] ***
    laStruc[lnAryRow,17] = laMenu[lnAryNo,llDefault]

    laStruc[lnAryRow,18] = laMenu[lnAryNo,lnParamPos]
    *** Save the level of the previous bar. ***
    lnCurLevel = lnBarLevel

    *B603560,1 SSH There is a field cBarModule in the SycMenu is getting blanked 
    *B603560,1 SSH upon saving.
    laStruc[lnAryRow,19] = laMenu[lnAryNo,19]
    *B603560,1 SSH [End]
  ENDIF
ENDFOR

SELECT (lcMenu_Tmp)
IF SEEK(lcApp2Sav)
  DELETE REST WHILE cApp_ID+cPad_pos+cPop_pos+cPop_levl+cBar_pos = lcApp2Sav
ENDIF
*E300581,1 Hesham El-Sheltawi (Start)
*APPEND FROM ARRAY laStruc ;
       FIELDS CAPP_ID,CPAD_POS,CPOP_POS,CBAR_POS,;
              CPOP_LEVL,CSUB_PRPT,CSUB_TYP,CSUB_CTG,;
              CSUB_POS,CMSTR_NAM,CSUB_MSG,;
              CSUB_HKEY,CPROSS_ID,CUPGRDLVL,CPROCTYPE,;
              CPROCPATH,LDEFAULT
*B603560,1 SSH There is a field cBarModule in the SycMenu is getting blanked 
*B603560,1 SSH upon saving.
*APPEND FROM ARRAY laStruc ;
       FIELDS CAPP_ID,CPAD_POS,CPOP_POS,CBAR_POS,;
              CPOP_LEVL,CSUB_PRPT,CSUB_TYP,CSUB_CTG,;
              CSUB_POS,CMSTR_NAM,CSUB_MSG,;
              CSUB_HKEY,CPROSS_ID,CUPGRDLVL,CPROCTYPE,;
              CPROCPATH,LDEFAULT,cMenuParam
APPEND FROM ARRAY laStruc ;
       FIELDS CAPP_ID,CPAD_POS,CPOP_POS,CBAR_POS,;
              CPOP_LEVL,CSUB_PRPT,CSUB_TYP,CSUB_CTG,;
              CSUB_POS,CMSTR_NAM,CSUB_MSG,;
              CSUB_HKEY,CPROSS_ID,CUPGRDLVL,CPROCTYPE,;
              CPROCPATH,LDEFAULT,cMenuParam,cBarModule

*B603560,1 SSH [End]
*E300581,1 Hesham El-Sheltawi (End)
llSaved    = .T.

*!**************************************************************************
*!
*!      Function: lfvSave
*!
*!**************************************************************************
*
FUNCTION lfvSave

*B602599,1 Reham On 11/12/1999   *** Begin ***
*B602599,1 Detect the error of "Invalid Subscript Reference" upon saving.
lcCurAryNo = lnAryNo
*B602599,1 Reham On 11/12/1999   *** End   ***

lcSavAlias = SELECT()
SELECT SYDREPRT
lcRpFilter = FILTER()
*E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]

*SET FILTER TO
SET FILTER TO cVer<>"A40" 
*E038033,1 HMA  [END]
SELECT SYDOBJCT
lcObFilter = FILTER()
SET FILTER TO

SELECT (lcSavAlias)

*** check first that there is new modification need to be updated to the 
*** temp file insted of wasting more time

=lfvSav_App(lcApp_ID)

*** Lock the master file. ***
IF gfFlock('SYCMENU',.T.)
  
  *** Get the maximam no. of records to be process. ***
  lnTotRec = RECCOUNT(lcMenu_Tmp)
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
  SELECT (lcMenu_Tmp)
  SCAN
    *** Call the thermometer function. ***
    lnCurRec = lnCurRec + 1
    =gfThermo(lnTotRec,lnCurRec,"Update master file...","")
    
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
      SELECT (lcMenu_Tmp)
      SCATTER MEMVAR MEMO
    
      *** Set the delete to "OFF" to see the ***
      *** deleted records in the master file ***
      SET DELETED OFF
      
      *** Save data from memory variables to the master file. ***
      SELECT SYCMENU
      GATHER MEMVAR
    
      *** Add audit information to the master file
      REPLACE CADD_USER WITH gcUser_ID   ;
              CADD_TIME WITH lcTime ;
              DADD_DATE WITH DATE()
      
      *** If the record was marked for deletion, recall this record. ***
      IF DELETED()
        RECALL
      ENDIF
    ELSE
      *** Insert new record in the master file ***
      *** from the temp. file...
      *B603560,1 SSH There is a field cBarModule in the SycMenu is getting blanked 
      *B603560,1 SSH upon saving.
      *INSERT INTO SYCMENU ;
             (CAPP_ID,CPAD_POS,CPOP_POS,CBAR_POS,CPOP_LEVL,CSUB_PRPT,;
              CSUB_TYP,CSUB_CTG,CSUB_POS,CMSTR_NAM,CSUB_MSG,;
              CSUB_HKEY,CPROSS_ID,CUPGRDLVL,CPROCTYPE,CPROCPATH,;
              LDEFAULT,CADD_USER,CADD_TIME,DADD_DATE) ;
      VALUES (&lcMenu_Tmp..CAPP_ID,&lcMenu_Tmp..CPAD_POS,;
              &lcMenu_Tmp..CPOP_POS,&lcMenu_Tmp..CBAR_POS,;
              &lcMenu_Tmp..CPOP_LEVL,&lcMenu_Tmp..CSUB_PRPT,;
              &lcMenu_Tmp..CSUB_TYP,&lcMenu_Tmp..CSUB_CTG,;
              &lcMenu_Tmp..CSUB_POS,&lcMenu_Tmp..CMSTR_NAM,;
              &lcMenu_Tmp..CSUB_MSG,&lcMenu_Tmp..CSUB_HKEY,;
              &lcMenu_Tmp..CPROSS_ID,&lcMenu_Tmp..CUPGRDLVL,;
              &lcMenu_Tmp..CPROCTYPE,&lcMenu_Tmp..CPROCPATH,;
              &lcMenu_Tmp..LDEFAULT,gcUser_ID,lcTime,DATE())

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
              &lcMenu_Tmp..LDEFAULT,gcUser_ID,lcTime,&lcMenu_Tmp..cBarModule,DATE())

      *B603560,1 SSH [End]
    ENDIF

      *B601052,1 Hesham El-Sheltawi (Start)
      *B601052,1 get the upgrade level of the report or the program
      *B601052,1 so that the menu bar will have the same upgrade level
     
      SELECT SYCMENU
      DO CASE
        CASE CPROCTYPE = 'R' AND !(cApp_ID $ 'SY|SM')
          lcUpGrd = LOOKUP(SYDREPRT.CUPGRDLVL,SUBSTR(CPROSS_ID,1,8),SYDREPRT.CREP_ID,'CREP_ID')
          REPLACE CUPGRDLVL WITH IIF(EMPTY(lcUpGrd),'A',lcUpGrd)
        CASE CPROCTYPE  $ 'CP' AND !(cApp_ID $ 'SY|SM')          
          lcUpGrd = LOOKUP(SYDOBJCT.CUPGRDLVL,CPROSS_ID,SYDOBJCT.CAPOBJNAM,'CAPOBJNAM')
          REPLACE CUPGRDLVL WITH IIF(EMPTY(lcUpGrd),'A',lcUpGrd)
      ENDCASE
      *B601052,1 Hesham El-Sheltawi (End)             
    
    *** Set delete to "ON" to avoid the ***
    *** deleted records in the temp. file...
    SET DELETE ON
    
    SELECT (lcMenu_Tmp)
    
  ENDSCAN

  *** Restore the status of delete settings. ***
  SET DELETED &lcDeletSav
  
  *** Delete all for blank records. ***
  SELECT SYCMENU
  DELETE ALL FOR CAPP_ID+CPAD_POS+CPOP_POS+CPOP_LEVL+CBAR_POS = " "

  *** Close the thermometer. ***
  IF lnTotRec > lnCurRec
    FOR lnThCount = lnCurRec TO lnTotRec
      =gfThermo(lnTotRec,lnThCount,"Update master file...","")
    ENDFOR
  ENDIF

  *** Unlock the master file. ***
  =gfFlock('SYCMENU',.F.)
  
  *** This flag to say if the menu changed or not. ***
  llMen_Chng = .T.

ENDIF

*B602599,1 Reham On 11/12/1999   *** Begin ***
*B602599,1 Detect the error of "Invalid Subscript Reference" upon saving.
lnAryNo = IIF(lnAryNo = 0 , lcCurAryNo , lnAryNo)
*B602599,1 Reham On 11/12/1999   *** End   ***

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
*
FUNCTION lfvClose

IF !llSaved
  *** There are changes happened in the current module. ***
  *** Do you want to save these changes ? ***
  *** <  Yes  > - <  No  > - <  Cancel  > ***
  lnOption = gfModalGen("QRM00141B00025","ALERT")
  DO CASE
    *** <  Yes  > ***
    CASE lnOption = 1
      =lfvSave()
    *** <  Cancel  > ***
    CASE lnOption = 3
      RETURN
  ENDCASE
ENDIF

*** Delete all the processes ID from the privilege file ***
*** according to if it was deleted from the menu or not ***
SELECT SYUUSRPR
*B802583,1 AMM Fix the bug of deleting all previlage when entering the menu program
*FOR lnCount2 = 1 TO ALEN(laProc2Del,1)
  *LOCATE FOR ALLTRIM(SYUUSRPR.CPROSS_ID) = ALLTRIM(laProc2Del[lnCount2])
  *DO WHILE FOUND()
    *DELETE
    *CONTINUE
  *ENDDO
*ENDFOR
IF !EMPTY(laProc2Del)
  FOR lnCount2 = 1 TO ALEN(laProc2Del,1)
    DELETE ALL FOR SYUUSRPR.CPROSS_ID = PADR(ALLTRIM(laProc2Del[lnCount2]),fsize("cPross_ID"))
  ENDFOR
ENDIF
*B802583,1 AMM end

IF llMen_Chng .AND. (lcApp_Id = "SY" .OR. lcApp_Id = "SM")
  *** Menu strucure was modified during this session. ***
  *** Do you wish to rebuild the menu now...
  *** <  Yes  > - <  No  > ***
  IF gfModalGen("QRM00143B00006","ALERT") = 1
*    WAIT "Rebuilding system menu...!" WINDOW NOWAIT
    =gfWait("00209","NOWAIT")
    HIDE MENU _MSYSMENU SAVE
    RELEASE POPUPS ALL EXTENDED
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

glQuitting = .T.

CLEAR READ

*E300581,1 Hesham El-Sheltawi (Start)
*!**************************************************************************
*!
*!      Function: lfvProgParm
*!
*!**************************************************************************
* Valid Function for Field Object Program Parameters
FUNCTION lfvProgParm
IF lnAryNo <> 0
  laMenu[lnAryNo,lnParamPos] = lcProgPram
  llSaved = .F.
ENDIF
*E300581,1 Hesham El-Sheltawi (End)

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
*B603865

FUNCTION lfvBarMod
PRIVATE lnI,lcActvTag,lcbarmodul,lcActvTagm ,llNewBar 


IF lnAryNo = 0
 RETURN
ENDIF

lcbarmodul = ""
llNewBar = TYPE('laMenu[lnAryNo,lnPop_Pos]') = "L" 
*B606218 WAB (Start) - add also the NC module 
*IF laMenu[lnAryNo,1] = "EB" .OR. laMenu[lnAryNo,1]= "AS" .OR. laMenu[lnAryNo,1]= "UP"
IF laMenu[lnAryNo,1] = "EB" .OR. laMenu[lnAryNo,1]= "AS" .OR. laMenu[lnAryNo,1]= "UP" .OR. laMenu[lnAryNo,1]= "NC"
*B606218 WAB (End)
  SELECT sydappl
  lcActvTag = ORDER()
  SET ORDER TO TAG Capp_id
  IF SEEK(laMenu[lnAryNo,1])
  
    *B606218 WAB (Start)
    *DECLARE laSource[3]
    DECLARE laSource[4]
    *B606218 WAB (End)
  
    laSource[1] = PADL(cbarmodule,2)
    laSource[2] = SUBSTR(PADL(cbarmodule,5),4)
    laSource[3] = SUBSTR(PADL(cbarmodule,8),7)
    
    *B606218 WAB (Start) 
    laSource[4] = SUBSTR(PADL(cbarmodule,11),10)
    *B606218 WAB (End) 
    
    DECLARE laTarget[1]
    IF !llNewBar
      SELECT (lcMenu_Tmp)
      lcActvTagm = ORDER()
      SET ORDER TO TAG Apppopbar
      IF SEEK(laMenu[lnAryNo,1]+laMenu[lnAryNo,lnPad_Pos]+laMenu[lnAryNo,lnPop_Pos]++;
              laMenu[lnAryNo,lnPop_Levl]+laMenu[lnAryNo,lnBar_Pos])
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
          
          *B606218 WAB (Start) 
          IF !EMPTY(SUBSTR(cbarmodule,10,2))
            DIMENSION laTarget[4]
            laTarget[3] = SUBSTR(cbarmodule,10,2)
          ENDIF  
          *B606218 WAB (End) 
          
        ENDIF 
      ENDIF
      SELECT (lcMenu_Tmp)
      SET ORDER TO TAG lcActvTagm 
    ELSE
      IF !EMPTY(laMenu[lnAryNo,19])
        IF !EMPTY(PADL(laMenu[lnAryNo,19],2))
          laTarget[1] = PADL(laMenu[lnAryNo,19],2)
        ENDIF  
        IF !EMPTY(SUBSTR(laMenu[lnAryNo,19],4,2))
          DIMENSION laTarget[2]
          laTarget[2] = SUBSTR(laMenu[lnAryNo,19],4,2)
        ENDIF  
        IF !EMPTY(SUBSTR(laMenu[lnAryNo,19],7,2))
          DIMENSION laTarget[3]
          laTarget[3] = SUBSTR(laMenu[lnAryNo,19],7,2)
        ENDIF  
        
        *B606218 WAB (Start) 
        IF !EMPTY(SUBSTR(laMenu[lnAryNo,19],10,2))
          DIMENSION laTarget[4]
          laTarget[4] = SUBSTR(laMenu[lnAryNo,19],10,2)
        ENDIF  
        *B606218 WAB (End) 
        
      ENDIF     
    ENDIF
    IF gfMover(@laSource,@laTarget,'Select Order Status',.T.,'')
      IF !EMPTY(laTarget[1])
        FOR lnI = 1 TO ALEN(laTarget,1)
          lcbarmodul = lcbarmodul+IIF(EMPTY(lcbarmodul),"",",")+ laTarget[lnI]
          llSaved = .F.
        ENDFOR
      ENDIF  
      IF !llNewBar  
        SELECT (lcMenu_Tmp)
        IF SEEK(laMenu[lnAryNo,1]+laMenu[lnAryNo,lnPad_Pos]+laMenu[lnAryNo,lnPop_Pos]++;
                laMenu[lnAryNo,lnPop_Levl]+laMenu[lnAryNo,lnBar_Pos])
          REPLACE cBarModules WITH lcbarmodul 
        ENDIF
      ELSE
        laMenu[lnAryNo,19] = lcbarmodul 
      ENDIF  
    ENDIF  
    laMenu[lnAryNo,19] = lcbarmodul
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
  
  *B606218 WAB (Start) 
  IF TYPE('laTarget[4]') <> "U"
    laTarget[4] = SUBSTR(PADL(lcbarmodul,11),10)
  ENDIF
  *B606218 WAB (End) 
ENDIF
*--End of lfvBarMod