*:************************************************************************
*: Program file  : GFPICOLR.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
IF _WINDOWS
 DEFINE WINDOW O_home IN SCREEN;
    FROM SROWS()-1, 0 TO SROWS()-1, SCOLS()-1;
    NONE COLOR SCHEME 24
  ACTIVATE WINDOW O_HOME NOSHOW            
  =gfWinFn('COLOR')
  =gfWinFn('COLOR')  
  WAIT ""
*  READ MODAL &&TIMEOUT 1
*  WAIT STR(READKEY()) WINDOW
  RELEASE WINDOW O_HOME
  RETURN
ENDIF

DECLARE laColrFor[16,1],laColrBac[16,1],laClrSchem[12,1],laClrPair[10,1]
*** laColrFor    Array to store foreground colors
*** laColrBac    Array to store background colors
*** laClrSchem   Array to store color schemes' names
*** laClrPair    Array to make a combination between
***              (fore/back)ground to collect colors
     
DECLARE laScmObj[10,1]
*** laScmObj     Array to display schemes' objects
***              of the current color scheme......

DECLARE laScmObj1[10,1],laScmObj2[10,1],laScmObj3[10,1],laScmObj4[10,1]
DECLARE laScmObj5[10,1],laScmObj6[10,1],laScmObj7[10,1],laScmObj8[10,1]
DECLARE laScmObj9[10,1],laScmObj10[10,1],laScmObj11[10,1],laScmObj12[10,1]
*** laScmObj[1---12]  12 Arrays for the schemes'objects
***                   one array for each color scheme..

DECLARE laColSet[1,1]
*** laColSet     Array to collect the colorset from
***              the resource file ................

*** laColrFor    Array to store foreground colors
laColrFor[1]  ="N"
laColrFor[2]  ="W"
laColrFor[3]  ="R"
laColrFor[4]  ="B"
laColrFor[5]  ="G"
laColrFor[6]  ="GR"
laColrFor[7]  ="RB"
laColrFor[8]  ="BG"
laColrFor[9]  ="N+"
laColrFor[10] ="W+"
laColrFor[11] ="R+"
laColrFor[12] ="B+"
laColrFor[13] ="G+"
laColrFor[14] ="GR+"
laColrFor[15] ="RB+"
laColrFor[16] ="BG+"

*** laColrBac    Array to store background colors
laColrBac[1]  ="N"
laColrBac[2]  ="W"
laColrBac[3]  ="R"
laColrBac[4]  ="B"
laColrBac[5]  ="G"
laColrBac[6]  ="GR"
laColrBac[7]  ="RB"
laColrBac[8]  ="BG"
laColrBac[9]  ="N*"
laColrBac[10] ="W*"
laColrBac[11] ="R*"
laColrBac[12] ="B*"
laColrBac[13] ="G*"
laColrBac[14] ="GR*"
laColrBac[15] ="RB*"
laColrBac[16] ="BG*"

*** laClrSchem   Array to store color schemes' names
laClrSchem[1]  = "User Winds"
laClrSchem[2]  = "User Menus"
laClrSchem[3]  = "Menu Bar"
laClrSchem[4]  = "Menu Pops"
laClrSchem[5]  = "Dialogs"
laClrSchem[6]  = "Dlog Pops"
laClrSchem[7]  = "Alert"
laClrSchem[8]  = "Windows"
laClrSchem[9]  = "Wind Pops"
laClrSchem[10] = "Browse"
laClrSchem[11] = "Report"
laClrSchem[12] = "Alert Pops"

*** laScmObj1   Array for scheme (1) objects *** User Wind ***
laScmObj1[1,1]  = "Say field"
laScmObj1[1,2]  = "Get field"
laScmObj1[1,3]  = "Border"
laScmObj1[1,4]  = "Title, active"
laScmObj1[1,5]  = "Title, idle & message"
laScmObj1[1,6]  = "Selected item"
laScmObj1[1,7]  = "Clock, Hot keys"
laScmObj1[1,8]  = "Shadow"
laScmObj1[1,9]  = "Enabled ctrl."
laScmObj1[1,10] = "Disabled ctrl."

*** laScmObj2   Array for scheme (2) objects *** User Menus ***
laScmObj2[1,1]  = "Disabled opt."
laScmObj2[1,2]  = "Enabled opt."
laScmObj2[1,3]  = "Border"
laScmObj2[1,4]  = "Menu titles"
laScmObj2[1,5]  = "Message"
laScmObj2[1,6]  = "Selected opt."
laScmObj2[1,7]  = "Hot Keys"
laScmObj2[1,8]  = "Shadow"
laScmObj2[1,9]  = "Enabed ctrl."
laScmObj2[1,10] = "Disabled ctrl."

*** laScmObj3   Array for scheme (3) objects *** Menu Bar ***
laScmObj3[1,1]  = "Disabled pads"
laScmObj3[1,2]  = "Enabled pads"
laScmObj3[1,3]  = "Border"
laScmObj3[1,4]  = "Title"
laScmObj3[1,5]  = "Message"
laScmObj3[1,6]  = "Selected pad"
laScmObj3[1,7]  = "Hot Keys"
laScmObj3[1,8]  = "Shadow"
laScmObj3[1,9]  = "Enabled ctrl."
laScmObj3[1,10] = "Disabled ctrl."

*** laScmObj4   Array for scheme (4) objects *** Menu Pops ***
laScmObj4[1,1]  = "Disabled opt."
laScmObj4[1,2]  = "Enabled opt."
laScmObj4[1,3]  = "Border"
laScmObj4[1,4]  = "Title"
laScmObj4[1,5]  = "Message"
laScmObj4[1,6]  = "Selected opt."
laScmObj4[1,7]  = "Hot keys"
laScmObj4[1,8]  = "Shadow"
laScmObj4[1,9]  = "Enabled ctrl."
laScmObj4[1,10] = "Disabled ctrl."

*** laScmObj5   Array for scheme (5) objects *** Dialogs ***
laScmObj5[1,1]  = "Normal text" 
laScmObj5[1,2]  = "Text box"
laScmObj5[1,3]  = "Border"
laScmObj5[1,4]  = "Title"
laScmObj5[1,5]  = "Message"
laScmObj5[1,6]  = "Selected opt."
laScmObj5[1,7]  = "Hot keys"
laScmObj5[1,8]  = "Shadow"
laScmObj5[1,9]  = "Enabled ctrl."
laScmObj5[1,10] = "Disabled ctrl."

*** laScmObj6   Array for scheme (6) objects *** Dlog Pops ***
laScmObj6[1,1]  = "Disable opt."
laScmObj6[1,2]  = "Enable opt."
laScmObj6[1,3]  = "Border"
laScmObj6[1,4]  = "Title"
laScmObj6[1,5]  = "Message"
laScmObj6[1,6]  = "Selected item"
laScmObj6[1,7]  = "Hot keys"
laScmObj6[1,8]  = "Shadow"
laScmObj6[1,9]  = "Enabled ctrl."
laScmObj6[1,10] = "Disabled ctrl."

*** laScmObj7   Array for scheme (7) objects *** Alert ***
laScmObj7[1,1]  = "Normal Text"
laScmObj7[1,2]  = "Text box"
laScmObj7[1,3]  = "Border"
laScmObj7[1,4]  = "Title"
laScmObj7[1,5]  = "Message"
laScmObj7[1,6]  = "Selected item"
laScmObj7[1,7]  = "Hot keys"
laScmObj7[1,8]  = "Shadow"
laScmObj7[1,9]  = "Enabled ctrl."
laScmObj7[1,10] = "Disabled ctrl."

*** laScmObj8   Array for scheme (8) objects *** Windows ***
laScmObj8[1,1]  = "Normal Text"
laScmObj8[1,2]  = "Text box"
laScmObj8[1,3]  = "Border"
laScmObj8[1,4]  = "Title, active"
laScmObj8[1,5]  = "Title, idle"
laScmObj8[1,6]  = "Selected text"
laScmObj8[1,7]  = "Hot keys"
laScmObj8[1,8]  = "Shadow"
laScmObj8[1,9]  = "Enabled ctrl."
laScmObj8[1,10] = "Disabled ctrl."

*** laScmObj9   Array for scheme (9) objects *** Wind Pops ***
laScmObj9[1,1]  = "Disabled opt."
laScmObj9[1,2]  = "Enabled opt."
laScmObj9[1,3]  = "Border"
laScmObj9[1,4]  = "Title"
laScmObj9[1,5]  = "Message"
laScmObj9[1,6]  = "Selected opt."
laScmObj9[1,7]  = "Hot keys"
laScmObj9[1,8]  = "Shadow"
laScmObj9[1,9]  = "Enabled ctrl."
laScmObj9[1,10] = "Disabled ctrl."

*** laScmObj10   Array for scheme (10) objects *** Browse ***
laScmObj10[1,1]  = "Other records"
laScmObj10[1,2]  = "Curr. field"
laScmObj10[1,3]  = "Border"
laScmObj10[1,4]  = "Title, active"
laScmObj10[1,5]  = "Title, idle"
laScmObj10[1,6]  = "Selected text"
laScmObj10[1,7]  = "Curr. record"
laScmObj10[1,8]  = "Shadow"
laScmObj10[1,9]  = "Enabled ctrl."
laScmObj10[1,10] = "Disabled ctrl."

*** laScmObj11   Array for scheme (11) objects *** Report ***
laScmObj11[1,1]  = "Text & B full"
laScmObj11[1,2]  = "Report field"
laScmObj11[1,3]  = "Border"
laScmObj11[1,4]  = "Title, active"
laScmObj11[1,5]  = "Title, idle"
laScmObj11[1,6]  = "Selected item"
laScmObj11[1,7]  = "Band A, empty"
laScmObj11[1,8]  = "Shadow"
laScmObj11[1,9]  = "Band A, full"
laScmObj11[1,10] = "Band B, empty"

*** laScmObj12   Array for scheme (12) objects *** Alert Pops ***
laScmObj12[1,1]  = "Disabled opt."
laScmObj12[1,2]  = "Enabled opt."
laScmObj12[1,3]  = "Border"
laScmObj12[1,4]  = "Title"
laScmObj12[1,5]  = "Message"
laScmObj12[1,6]  = "Selected opt."
laScmObj12[1,7]  = "Hot keys"
laScmObj12[1,8]  = "Shadow"
laScmObj12[1,9]  = "Enabled ctrl."
laScmObj12[1,10] = "Disabled ctrl."


llScmFlg  = .T. 
*** lfvClrSchem  --  Set to .T. if there is new color scheme ***
*** lfvLodSav    --  Set to .T. if load new colorset........ ***
*** lpScmNam  - check - if .T. clear the aria to display the color scheme ***

llObjFlg  = .F.
*** lfvColFor    --  Set to .T. if there is new foreground color ***
*** lfvColBac    --  Set to .T. if there is new background color ***
*** lfShow    - check - if .T. cobine the fore & back ground in laClrPair ***

llAllObjs = .T.
*** lpScmNam     -- Set to .T. if there is new color scheme to ***
***                 display the whole objects of the window .. ***
*** lpScmNam  - check - in the begining of each case to  draw ***
***                     the whole objects of the color scheme ***
***                     if it's .T.  ........................ ***

llUpdtClr = .T.
*** lfvScmObj    -- Set to .T. if it's new scheme objects ***
*** lfvClrSchem  -- Set to .T. if it's new color scheme . ***
*** lfvLodSav    -- Set to .T. if load new colorset ..... ***
*** lfShow    - check - if .T. change the displayed colors ***
***                     of fore/background ............... ***

lnOldOb   = 1             && Variable to store current scheme object no.

lnOldObj  = 1             && Variable to store current color no.
***                       in (fore/back)ground


lnOldScm  = 1             && Variable to store current color scheme no.

lnScmNo   = 1             && Parameter in lpScmNam proc. to know
***                       the scheme no. to display its objects.

lnContFor = 1             && Var. to store the new color no. in
***                       the foreground colors ...............

lnContBac = 1             && Var. to store the new color no. in
***                       the background colors ...............

puClrSchem= 1             && Set the default popup no. to 1 to
***                       display color scheme no. 1..........

laClrPair = " "           && Store spaces in laClrPair array.

lcButton  = ""            && variable to store current push button
***                       " Save " or " Load " ...................

lcPathFile = SYS(2005)    && Variable to store the path of the
***                       resource file ......................

lcColSet = ""             && Variable to save the colorset name
***                       in the resource file ................

lcScmNam  = "laScmObj1"   && Variable to contain the  scheme's objects
***                       array name & it begin with the default color
***                       scheme no. 1

=ACOPY(&lcScmNam,laScmObj)  && Copy the contents of the array that
***                         lcScmNam variable pointing to its name
***                         to laScmObj array...

*** Put the first scheme colors in laClrPair array ***
For lnCount = 1 TO 10
  laClrPair[lnCount] = SCHEME(puClrSchem,lnCount)
ENDFOR

*** Save the default colors before any change ***
*** to get it back if press cancel .......... ***
CREATE COLOR SET lcDefault


*** Display the screen ****
DO syPiColr.SPR

*!**************************************************************************
*!
*!      FUNCTION : lfShow
*!
*!**************************************************************************
*

FUNCTION lfShow

*** If any change happen in the (fore/back)ground colors ***
*** the laClrPair colors of the object should change ... ***
IF llObjFlg
  laClrPair[puScmObj] = laColrFor[lnContFor]+"/"+laColrBac[lnContBac] 
  llObjFlg = .F.
ENDIF  

*** Choose the colors displayed in the push buttons ***
*** depending on the colors in the scheme object .. ***
IF llUpdtClr
  llUpdtClr = .F.
  lcForClr  = SUBSTR(laClrPair[puScmObj],1,AT("/",laClrPair[puScmObj])-1)
  lcBacClr  = SUBSTR(laClrPair[puScmObj],AT("/",laClrPair[puScmObj])+1)
  lnContFor = ASCAN(laColrFor,lcForClr)
  lnContBac = ASCAN(laColrBac,lcBacClr)
ENDIF

*** Display the (fore/back)ground colors in the push buttons ***
lcColrBac = "/"+laColrBac[lnContBac] 
@ 02,59 SAY "ÛÛÛÛÛ" COLOR &laColrFor[lnContFor] 
@ 07,59 SAY "     " COLOR &lcColrBac

RETURN



*!**************************************************************************
*!
*!      FUNCTION : lfwScmObj
*!
*!**************************************************************************
*

FUNCTION lfwScmObj

*** Store the current puScmObj no. (scheme object no.)  ***
*** in variable to know in the valid clause if this no. ***
*** change or not ..................................... ***
lnOldOb = puScmObj
RETURN



*!**************************************************************************
*!
*!      FUNCTION : lfvScmObj
*!
*!**************************************************************************
*

FUNCTION lfvScmObj

*** If any change happen in the scheme objects (means: change ***
*** happen in puScmObj no.) should set llUpdtClr to true and  ***
*** excute the lfShow to display the colors of the new object ***
IF lnOldOb <> puScmObj
  llUpdtClr = .T.
  SHOW GETS
ENDIF
RETURN



*!**************************************************************************
*!
*!      FUNCTION : lfwClrSchem
*!
*!**************************************************************************
*

FUNCTION lfwClrSchem

*** Store the current puClrSchem no. (color scheme no.) ***
*** in variable to know in the valid clause if this no. ***
*** change or not ..................................... ***
lnOldScm = puClrSchem
RETURN



*!**************************************************************************
*!
*!      FUNCTION : lfvClrSchem
*!
*!**************************************************************************
*

FUNCTION lfvClrSchem

*** Combine the array name depending on the color scheme no. ***
*** to know which color scheme to display & its objects .... ***
lcScmNam = "laScmObj" + ALLTRIM(STR(puClrSchem))

*** Set the scheme objects to 1 to display the first scheme ***
*** object in the popup ................................... ***
puScmObj = 1


*** If any change happen in the colors scheme ***
*** (means:change happened in puClrSchem no.) ***
IF lnOldScm <> puClrSchem

  *** Should copy the array elements in lcScmStr to set ***
  *** the current color scheme to the changed colors .. ***
  lcScmStr = ""
  FOR lnCount = 1 TO 10 
    lcScmStr = lcScmStr + IIF(EMPTY(lcScmStr), "" , ",") + laClrPair[lnCount]
  ENDFOR
  SET COLOR OF SCHEME lnOldScm TO &lcScmStr
  
  *** Put the new color scheme in laClrPair array ***
  For lnCount = 1 TO 10
    laClrPair[lnCount] = SCHEME(puClrSchem,lnCount)
  ENDFOR

  *** Set llUpdtClr to true to change the displayed  ***
  *** colors of the first scheme object from the new ***
  *** color scheme ................................. ***
  llUpdtClr = .T.
  
  *** Set llScmFlg to true to clear the aria to draw  ***
  *** the new objects window for the new color scheme ***
  llScmFlg = .T.

  *** Copy  the contents  of the  array that ***
  *** lcScmNam variable pointing to its name ***
  *** to laScmObj array..................... ***
  =ACOPY(&lcScmNam,laScmObj)
ELSE

  *** If there is no change in  the current color  scheme  ***
  *** set the llScmFlg to false to avoid clear the current ***
  *** window while excuting the lpScmNam proc. ........... ***
  llScmFlg = .F.
ENDIF

*** Excute lfShow to display the new changes if there is ***
SHOW GETS

*** Excute the lpScmNam with the new color scheme no. ***
DO lpScmNam WITH puClrSchem

RETURN



*!**************************************************************************
*!
*!      FUNCTION : lfwColFor
*!
*!**************************************************************************
*

FUNCTION lfwColFor

*** Store  the current  foreground  color in variable to ***
*** know in the valid clause if this color change or not ***
lnOldObj = lnContFor
RETURN



*!**************************************************************************
*!
*!      FUNCTION : lfvColFor
*!
*!**************************************************************************
*

FUNCTION lfvColFor

*** Change the displayed colors depending on pressing ***
*** the increase arrow or the decrease arrow ........ ***
DO CASE
  CASE pbColFor = 1
    lnContFor = IIF(lnContFor + 1 <= 16 , lnContFor + 1 , 1)
  CASE pbColFor = 2 
    lnContFor = IIF(lnContFor - 1 > 0 , lnContFor - 1 , 16)
ENDCASE

*** If any change happen in the foreground color ***
*** set llObjFlg to true to change the contents  ***
*** of laClrPair array in lfShow ............... ***
IF lnOldObj <> lnContFor
  llObjFlg = .T.
ENDIF

*** Excute lfShow to display the new changes if there is ***
SHOW GETS

*** Excute the lpScmNam with the new color scheme no. ***
DO lpScmNam WITH puClrSchem

RETURN



*!**************************************************************************
*!
*!      FUNCTION : lfwColBac
*!
*!**************************************************************************
*

FUNCTION lfwColBac

*** Store  the current  background  color in variable to ***
*** know in the valid clause if this color change or not ***
lnOldObj = lnContBac
RETURN



*!**************************************************************************
*!
*!      FUNCTION : lfvColBac
*!
*!**************************************************************************
*

FUNCTION lfvColBac

*** Change the displayed colors depending on press the ***
*** increase arrow or the decrease arrow ............. ***
DO CASE
  CASE pbColBac = 1
    lnContBac = IIF(lnContBac + 1 <= 16 , lnContBac + 1 , 1)
  CASE pbColBac = 2 
    lnContBac = IIF(lnContBac - 1 > 0 , lnContBac - 1 , 16)
ENDCASE

*** If any change happen in the background color ***
*** set llObjFlg to true to change the contents  ***
*** of laClrPair array in lfShow ............... ***
IF lnOldObj <> lnContBac
  llObjFlg = .T.
ENDIF

*** Excute lfShow to display the new changes if there is ***
SHOW GETS

*** Excute the lpScmNam with the new color scheme no. ***
DO lpScmNam WITH puClrSchem

RETURN



*!**************************************************************************
*!
*!      FUNCTION : lfvLodSav
*!
*!**************************************************************************
*

FUNCTION lfvLodSav

*** Collecting the colorsets from the resource file ***
SELECT NAME FROM &lcPathFile WHERE TYPE="PREF2.5" .AND. ID="COLORSET";
       INTO ARRAY laColSet

DO CASE

  *** If press the load button ***
  CASE pbLodSav = 1

    lcButton = "Load"

    *** Should copy the array elements in lcScmStr to ***
    *** set the color scheme to save the colorset.... ***
    lcScmStr = ""
    FOR lnCount = 1 TO 10
      lcScmStr = lcScmStr + IIF(EMPTY(lcScmStr), "" , ",") + laClrPair[lnCount]
    ENDFOR
    SET COLOR OF SCHEME lnOldScm TO &lcScmStr
    
    *** Excute the colset screen to load new ***
    *** coloset from the resource file ..... ***
    DO COLSET.SPR
    
    *** Put the new color scheme in laClrPair array ***
    For lnCount = 1 TO 10
      laClrPair[lnCount] = SCHEME(puClrSchem,lnCount)
    ENDFOR
  
    *** Set llAllFlg to true to refresh the whole objects ***
    *** of the first color scheme from the new colorset.. ***
    llAllFlg = .T.
    
    *** Set the llScmFlg to true to clear the current ***
    *** window while excuting the lpScmNam proc. .... ***
    llScmFlg = .T.

    *** Excute the lpScmNam with the new color scheme no. ***
    DO lpScmNam WITH puClrSchem

    *** Set llUpdtClr to true and excute the  lfShow to ***
    *** display the colors  of the first object  in the ***
    *** first color scheme from the new loaded colorset ***
    llUpdtClr = .T.

    *** Excute lfShow to display the new changes if there is ***
    SHOW GETS
  
  *** If press the save button ***
  CASE pbLodSav = 2

    lcButton = "Save"
    
    *** Should copy the array elements in lcScmStr to ***
    *** set the color scheme to save the colorset.... ***
    lcScmStr = ""
    FOR lnCount = 1 TO 10
      lcScmStr = lcScmStr + IIF(EMPTY(lcScmStr), "" , ",") + laClrPair[lnCount]
    ENDFOR
    SET COLOR OF SCHEME lnOldScm TO &lcScmStr
    
    *** Excute the colset screen to save the ***
    *** current coloset in the resource file ***
    DO COLSET.SPR
    
ENDCASE    
RETURN



*!**************************************************************************
*!
*!      FUNCTION : lfvExit
*!
*!**************************************************************************
*

FUNCTION lfvExit

IF pbExit = 1

  *** If press " Ok " ***
  *** Gather the array elements in string to set color ***
  *** scheme to the new colors in the laClrPair array  ***
  lcScmStr = ""
  FOR lnCount = 1 TO 10 
    lcScmStr =lcScmStr+IIF(EMPTY(lcScmStr), "" , ",")+laClrPair[lnCount]
  ENDFOR
  SET COLOR OF SCHEME lnOldScm TO &lcScmStr
ELSE

  *** If press " Cancel " ***
  *** Get the default colors that we save ***
  *** in the begining of the program .... ***
  SET COLOR SET TO lcDefault
ENDIF

*** Get the alias of the resource file ***
lcPathFile = gfStripExt(lcPathFile)

*** Close the resource file if it is open ***
IF USED(lcPathFile)
  USE IN ALIAS(lcPathFile)
ENDIF

RETURN



*!**************************************************************************
*!
*!      PROCEDURE : lpScmNam
*!
*!**************************************************************************
*

PROCEDURE lpScmNam

PARAMETERS lnScmNo

*** If llScmFlg  that's  mean  the color  scheme no. is ***
*** changed : so the aria  should be clear  for the new ***
*** color scheme & set the llAllObjs to true to display ***
*** the all objects of the new window ................. ***
IF llScmFlg
  llScmFlg  = .F.
  llAllObjs = .T.
  @ 5,1 TO 18,50 CLEAR
ELSE
  *** If not llScmFlg that's mean the color scheme no. ***
  *** does not change so llAllFlg  should be  false to ***
  *** refresh only the changed object ................ ***
  llAllObjs = .F.
ENDIF

DO CASE
  CASE lnScmNo = 1
    DO lpNo1          && User Wind
  CASE lnScmNo = 2
    DO lpNo2          && User Menu
  CASE lnScmNo = 3
    DO lpNo3          && Menu Bar
  CASE lnScmNo = 4
    DO lpNo4          && Menu Pops
  CASE lnScmNo = 5
    DO lpNo5          && Dialogs
  CASE lnScmNo = 6
    DO lpNo6          && Dlog Pops
  CASE lnScmNo = 7
    DO lpNo7          && Alert
  CASE lnScmNo = 8
    DO lpNo8          && Windows
  CASE lnScmNo = 9
    DO lpNo9          && Wind Pops
  CASE lnScmNo = 10
    DO lpNo10         && Browse
  CASE lnScmNo = 11
    DO lpNo11         && Report
  CASE lnScmNo = 12
    DO lpNo12         && Alert Pops
ENDCASE


*!**************************************************************************
*!
*!      PROCEDURE : lpNo1      ****  User Wind  ****
*!
*!**************************************************************************
*

PROCEDURE lpNo1

*** SCHEME (1) ***

IF llAllObjs
  llAllObjs = .F.
  
  ** COLOR PAIR 1 SAY FIELD **
  @ 6,7 FILL TO 14,45 COLOR &laClrPair[1]
  @ 9,29 TO 13,44 COLOR &laClrPair[1]
  @ 9,8 TO 13,26  COLOR &laClrPair[1]
  @ 6,9 SAY "Say Field :"    COLOR &laClrPair[1]
  @ 7,9 SAY "Say Field :"    COLOR &laClrPair[1]
  @ 9,9 SAY " Say Field :"  COLOR &laClrPair[1]
  @ 9,30 SAY " Say Field :" COLOR &laClrPair[1]
  
  ** COLOR PAIR 2 GET FIELD **
  @ 6,21 SAY "Get Field" COLOR &laClrPair[2]
  
  ** COLOR PAIR 3 BORDER **
  @ 5,6  FILL TO 5,46  COLOR &laClrPair[3]
  @ 5,6  FILL TO 15,6  COLOR &laClrPair[3]
  @ 15,6 FILL TO 15,46 COLOR &laClrPair[3]
  @ 5,46 FILL TO 15,46 COLOR &laClrPair[3]
  @ 5,6 SAY "þ"      COLOR &laClrPair[3]
  @ 5,46 SAY "ð"     COLOR &laClrPair[3]
  @ 15,7 SAY ""    COLOR &laClrPair[3]
  @ 15,45 SAY "."   COLOR &laClrPair[3]
  @ 6,46 SAY ""     COLOR &laClrPair[3]
  @ 7,46 SAY ""     COLOR &laClrPair[3]
  @ 14,46 SAY ""    COLOR &laClrPair[3]
  
  ** COLOR PAIR 4 TITLE, ACTIVE **
  @ 5,18 SAY "Tit_act" COLOR &laClrPair[4]
  
  ** COLOR PAIR 5 TITLE, IDLE & MESSAGE **
  @ 5,28 SAY "Tit_idl" COLOR &laClrPair[5]
  @ 18,6 SAY " User Window Message          " COLOR &laClrPair[5]
  
  ** COLOR PAIR 8 SHADOW **
  @ 16,8 FILL TO 16,46 COLOR &laClrPair[8]
  @ 6,47 FILL TO 16,47 COLOR &laClrPair[8]
  @ 6,48 FILL TO 16,48 COLOR &laClrPair[8]
  
  ** COLOR PAIR 6 SELECTED ITEM **
  @ 7,21 SAY "Selected item" COLOR &laClrPair[6]
  
  ** COLOR PAIR 10 DISABLED CTRL. **
  @ 11,9 SAY "( ) Radio Button1" COLOR &laClrPair[10]
  @ 12,9 SAY "() Radio Button2" COLOR &laClrPair[10]
  @ 10,31 SAY "ÚÄÄÄÄÄÄÄÄÄÄ·"    COLOR &laClrPair[10]
  @ 11,31 SAY "³  Popup   º"    COLOR &laClrPair[10]
  @ 12,31 SAY "ÔÍÍÍÍÍÍÍÍÍÍ¼"    COLOR &laClrPair[10]
  @ 14,28 SAY "<Disable ctrl.>" COLOR &laClrPair[10]
 
  ** COLOR PAIR 9 ENABLED CTRL. **
  @ 14,10 SAY "< nable ctrl.>"  COLOR &laClrPair[9]
  
  ** COLOR PAIR 7 CLOCK, HOT KEYS **
  @ 14,11 SAY "E"               COLOR &laClrPair[7]
  @ 18,36 SAY "12:23:45 pm" COLOR &laClrPair[7]

ELSE
  DO CASE
    CASE puScmObj = 1
      ** COLOR PAIR 1 SAY FIELD **
      @ 6,7 SAY "  Say Field : " COLOR &laClrPair[1]
      @ 7,7 SAY "  Say Field : " COLOR &laClrPair[1]
      @ 6,30 FILL TO 6,45        COLOR &laClrPair[1]
      @ 7,34 FILL TO 7,45        COLOR &laClrPair[1]
      @ 8,7  FILL TO 8,45        COLOR &laClrPair[1]
      @ 9,7  FILL TO 14,7        COLOR &laClrPair[1]
      @ 9,45 FILL TO 14,45       COLOR &laClrPair[1]
      @ 9,29 TO 13,44            COLOR &laClrPair[1]
      @ 9,8 TO 13,26             COLOR &laClrPair[1]
      @ 9,9 SAY " Say Field :"   COLOR &laClrPair[1]
      @ 9,30 SAY " Say Field :"  COLOR &laClrPair[1]
      @ 10,9  FILL TO 10,25      COLOR &laClrPair[1]
      @ 9,27  FILL TO 14,28      COLOR &laClrPair[1]
      @ 10,30 FILL TO 12,30      COLOR &laClrPair[1]
      @ 10,43 FILL TO 12,43      COLOR &laClrPair[1]
      @ 14,8 SAY "  "    COLOR &laClrPair[1]
      @ 14,24 SAY "    " COLOR &laClrPair[1]
      @ 14,43 SAY "  "   COLOR &laClrPair[1]
  
    CASE puScmObj = 2
      ** COLOR PAIR 2 GET FIELD **
      @ 6,21 SAY "Get Field" COLOR &laClrPair[2]
  
    CASE puScmObj = 3
      ** COLOR PAIR 3 BORDER **
      @ 5,6  FILL TO 5,17  COLOR &laClrPair[3]
      @ 5,25 FILL TO 5,27  COLOR &laClrPair[3]
      @ 5,35 FILL TO 5,46  COLOR &laClrPair[3]
      @ 5,6  FILL TO 15,6  COLOR &laClrPair[3]
      @ 15,6 FILL TO 15,46 COLOR &laClrPair[3]
      @ 5,46 FILL TO 15,46 COLOR &laClrPair[3]
      @ 5,6 SAY "þ"      COLOR &laClrPair[3]
      @ 5,46 SAY "ð"     COLOR &laClrPair[3]
      @ 15,7 SAY ""    COLOR &laClrPair[3]
      @ 15,45 SAY "."   COLOR &laClrPair[3]
      @ 6,46 SAY ""     COLOR &laClrPair[3]
      @ 7,46 SAY ""     COLOR &laClrPair[3]
      @ 14,46 SAY ""    COLOR &laClrPair[3]

    CASE puScmObj = 4
      ** COLOR PAIR 4 TITLE, ACTIVE **
      @ 5,18 SAY "Tit_act" COLOR &laClrPair[4]

    CASE puScmObj = 5
      ** COLOR PAIR 5 TITLE, IDLE & MESSAGE **
      @ 5,28 SAY "Tit_idl" COLOR &laClrPair[5]
      @ 18,6 SAY " User Window Message          " COLOR &laClrPair[5]

    CASE puScmObj = 6
      ** COLOR PAIR 6 SELECTED ITEM **
      @ 7,21 SAY "Selected item" COLOR &laClrPair[6]

    CASE puScmObj = 7
      ** COLOR PAIR 7 CLOCK, HOT KEYS **
      @ 14,11 SAY "E"               COLOR &laClrPair[7]
      @ 18,36 SAY "12:23:45 pm" COLOR &laClrPair[7]

    CASE puScmObj = 8
      ** COLOR PAIR 8 SHADOW **
      @ 16,8 FILL TO 16,46 COLOR &laClrPair[8]
      @ 6,47 FILL TO 16,47 COLOR &laClrPair[8]
      @ 6,48 FILL TO 16,48 COLOR &laClrPair[8]

    CASE puScmObj = 9
      ** COLOR PAIR 9 ENABLED CTRL. **
      @ 14,10 SAY "< nable ctrl.>"  COLOR &laClrPair[9]
      @ 14,11 SAY "E"               COLOR &laClrPair[7]

    CASE puScmObj = 10
      ** COLOR PAIR 10 DISABLED CTRL. **
      @ 11,9 SAY "( ) Radio Button1" COLOR &laClrPair[10]
      @ 12,9 SAY "() Radio Button2" COLOR &laClrPair[10]
      @ 10,31 SAY "ÚÄÄÄÄÄÄÄÄÄÄ·"    COLOR &laClrPair[10]
      @ 11,31 SAY "³  Popup   º"    COLOR &laClrPair[10]
      @ 12,31 SAY "ÔÍÍÍÍÍÍÍÍÍÍ¼"    COLOR &laClrPair[10]
      @ 14,28 SAY "<Disable ctrl.>" COLOR &laClrPair[10]

  ENDCASE
ENDIF

RETURN

*!**************************************************************************
*!
*!      PROCEDURE : lpNo2      ****  User Menus  ****
*!
*!**************************************************************************
*

PROCEDURE lpNo2

*** SCHEME (2) ***

IF llAllObjs
  llAllObjs = .F.
  @ 6,6 TO 16,46
  
  ** COLOR PAIR 2 ENABLE OPT. **
  @ 7,13 SAY "nabled "  COLOR &laClrPair[2]
  @ 11,32 SAY "nabled " COLOR &laClrPair[2]

  ** COLOR PAIR 7 HOT KEYS **
  @ 7,12 SAY "E"  COLOR &laClrPair[7]
  @ 11,31 SAY "E" COLOR &laClrPair[7]

  ** COLOR PAIR 6 SELECTED 
  @ 7,30 SAY " Selected " COLOR &laClrPair[6]
  @ 9,31 SAY "Selected"   COLOR &laClrPair[6]
 
  ** COLOR PAIR 1 DISABLED OPT. **
  @ 7,20 SAY " Disabled " COLOR &laClrPair[1]
  @ 10,31 SAY "Disabled"  COLOR &laClrPair[1]

  ** COLOR PAIR 3 BORDER **
  @ 8,30 TO 12,39 COLOR &laClrPair[3]

  ** COLOR PAIR 8 SHADOW **
  @ 9,40 FILL TO 13,40  COLOR &laClrPair[8]
  @ 9,41 FILL TO 13,41  COLOR &laClrPair[8]
  @ 13,32 FILL TO 13,39 COLOR &laClrPair[8]

  ** COLOR PAIR 5 MESSAGE **
  @ 15,13 SAY "This is a User Menu Message" COLOR &laClrPair[5]

ELSE
  DO CASE
    CASE puScmObj = 1
      ** COLOR PAIR 1 DISABLED OPT. **
      @ 7,20 SAY " Disabled " COLOR &laClrPair[1]
      @ 10,31 SAY "Disabled"  COLOR &laClrPair[1]

    CASE puScmObj = 2
      ** COLOR PAIR 2 ENABLE OPT. **
      @ 7,13 SAY "nabled "  COLOR &laClrPair[2]
      @ 11,32 SAY "nabled " COLOR &laClrPair[2]

    CASE puScmObj = 3
      ** COLOR PAIR 3 BORDER **
      @ 8,30 TO 12,39 COLOR &laClrPair[3]

    CASE puScmObj = 5
      ** COLOR PAIR 5 MESSAGE **
      @ 15,13 SAY "This is a User Menu Message" COLOR &laClrPair[5]

    CASE puScmObj = 6
      ** COLOR PAIR 6 SELECTED 
      @ 7,30 SAY " Selected " COLOR &laClrPair[6]
      @ 9,31 SAY "Selected"   COLOR &laClrPair[6]

    CASE puScmObj = 7
      ** COLOR PAIR 7 HOT KEYS **
      @ 7,12 SAY "E"  COLOR &laClrPair[7]
      @ 11,31 SAY "E" COLOR &laClrPair[7]

    CASE puScmObj = 8
      ** COLOR PAIR 8 SHADOW **
      @ 9,40 FILL TO 13,40  COLOR &laClrPair[8]
      @ 9,41 FILL TO 13,41  COLOR &laClrPair[8]
      @ 13,32 FILL TO 13,39 COLOR &laClrPair[8]

  ENDCASE
ENDIF

RETURN

*!**************************************************************************
*!
*!      PROCEDURE : lpNo3      ****  Menu Bar  ****
*!
*!**************************************************************************
*

PROCEDURE lpNo3

*** SCHEME (3) ***

IF llAllObjs
  llAllObjs = .F.
  @ 6,4 TO 14,48
  ** COLOR PAIR 2 ENABLED PADS **
  @ 7,6  SAY "  ysmenu "  COLOR &laClrPair[2]
  
  ** COLOR PAIR 1 DISABLED PADS **
  @ 7,15 SAY " Disabled " COLOR &laClrPair[1]
  
  ** COLOR PAIR 6 SELECTED PAD **
  @ 7,25 SAY " Selected " COLOR &laClrPair[6]
  
  ** COLOR PAIR 2 ENABLED PADS **
  @ 7,35 SAY "  nabled "  COLOR &laClrPair[2]
  
  ** COLOR PAIR 7 HOT KEYS **
  @ 7,7  SAY "S" COLOR &laClrPair[7]
  @ 7,36 SAY "E" COLOR &laClrPair[7]
  
  @ 8,26 TO 12,43       COLOR(SCHEME(4,3))
  @ 9,27  SAY " nabled         "  COLOR(SCHEME(4,2))
  @ 11,27 SAY "Selected        " COLOR(SCHEME(4,6))
  @ 10,27 SAY "Disabled        " COLOR(SCHEME(4,1))
  @ 9,27  SAY "E"       COLOR(SCHEME(4,7))

  @ 13,28 FILL TO 13,43 COLOR(SCHEME(4,8))
  @ 9,44  FILL TO 13,44 COLOR(SCHEME(4,8))
  @ 9,45  FILL TO 13,45 COLOR(SCHEME(4,8))

ELSE
  DO CASE
    CASE puScmObj = 1
      ** COLOR PAIR 1 DISABLED PADS **
      @ 7,15 SAY " Disabled " COLOR &laClrPair[1]
   
    CASE puScmObj = 2
      ** COLOR PAIR 2 ENABLED PADS **
      @ 7,6  SAY "  ysmenu "  COLOR &laClrPair[2]
      @ 7,7  SAY "S" COLOR &laClrPair[7]
      @ 7,35 SAY "  nabled "  COLOR &laClrPair[2]
      @ 7,36 SAY "E" COLOR &laClrPair[7]
      
    CASE puScmObj = 6
      ** COLOR PAIR 6 SELECTED PAD **
      @ 7,25 SAY " Selected " COLOR &laClrPair[6]
    
    CASE puScmObj = 7
      ** COLOR PAIR 7 HOT KEYS **
      @ 7,7  SAY "S" COLOR &laClrPair[7]
      @ 7,36 SAY "E" COLOR &laClrPair[7]

  ENDCASE
ENDIF

RETURN

*!**************************************************************************
*!
*!      PROCEDURE : lpNo4      ****  Menu Pops  ****
*!
*!**************************************************************************
*

PROCEDURE lpNo4

*** SCHEME (4) ***

IF llAllObjs
  llAllObjs = .F.

  @ 6,4 TO 14,48
  @ 7,6  SAY "  ysmenu "  COLOR(SCHEME(3,2))
  @ 7,15 SAY " Disabled " COLOR(SCHEME(3,1))
  @ 7,25 SAY " Selected " COLOR(SCHEME(3,6))
  @ 7,35 SAY "  nabled "  COLOR(SCHEME(3,2))
  @ 7,7  SAY "S" COLOR(SCHEME(3,7))
  @ 7,36 SAY "E" COLOR(SCHEME(3,7))
  
  ** COLOR PAIR 3 BORDER **
  @ 8,26 TO 12,43       COLOR &laClrPair[3]
  
  ** COLOR PAIR 2 ENABLED OPT. **
  @ 9,27  SAY " nabled         " COLOR &laClrPair[2]
  
  ** COLOR PAIR 6 SELECTED OPT. **
  @ 11,27 SAY "Selected        " COLOR &laClrPair[6]
  
  ** COLOR PAIR 1 DISABLED OPT. **  
  @ 10,27 SAY "Disabled        " COLOR &laClrPair[1]
  
  ** COLOR PAIR 7 HOT KEYS **
  @ 9,27  SAY "E"       COLOR &laClrPair[7]

  ** COLOR PAIR 8 SHOADOW **
  @ 13,28 FILL TO 13,43 COLOR &laClrPair[8]
  @ 9,44  FILL TO 13,44 COLOR &laClrPair[8]
  @ 9,45  FILL TO 13,45 COLOR &laClrPair[8]
ELSE
  DO CASE
    CASE puScmObj = 1
      ** COLOR PAIR 1 DISABLED OPT. **  
      @ 10,27 SAY "Disabled        " COLOR &laClrPair[1]
  
    CASE puScmObj = 2
      ** COLOR PAIR 2 ENABLED OPT. **
      @ 9,27  SAY " nabled         " COLOR &laClrPair[2]
      @ 9,27  SAY "E"       COLOR &laClrPair[7]
  
    CASE puScmObj = 3
      ** COLOR PAIR 3 BORDER **
      @ 8,26 TO 12,43       COLOR &laClrPair[3]
  
    CASE puScmObj = 6
      ** COLOR PAIR 6 SELECTED OPT. **
      @ 11,27 SAY "Selected        " COLOR &laClrPair[6]
  
    CASE puScmObj = 7
      ** COLOR PAIR 7 HOT KEYS **
      @ 9,27  SAY "E"       COLOR &laClrPair[7]

    CASE puScmObj = 8
      ** COLOR PAIR 8 SHOADOW **
      @ 13,28 FILL TO 13,43 COLOR &laClrPair[8]
      @ 9,44  FILL TO 13,44 COLOR &laClrPair[8]
      @ 9,45  FILL TO 13,45 COLOR &laClrPair[8]

  ENDCASE
ENDIF

RETURN
  
  
*!**************************************************************************
*!
*!      PROCEDURE : lpNo5      ****  Dialogs  ****
*!
*!**************************************************************************
*

PROCEDURE lpNo5

*** SCHEME (5) ***

IF llAllObjs
  llAllObjs = .F.

  ** COLOR PAIR 1 NORMAL TEXT **
  @ 6,4 FILL TO 12, 45 COLOR &laClrPair[1]
  @ 8,17  SAY "Dialog Message" SIZE 1,14, 0 COLOR &laClrPair[1]

  ** COLOR PAIR 3 BORDER **
  @ 6,4 TO 12,45 DOUBLE COLOR &laClrPair[3]

  ** COLOR PAIR 6 SELECTED OPT. **
  @ 11,32 SAY "<Selected>"     SIZE 1,10, 0 COLOR &laClrPair[6]

  ** COLOR PAIR 8 SHADOW **
  @ 7,46 FILL TO 13,46 COLOR &laClrPair[8]
  @ 7,47 FILL TO 13,47 COLOR &laClrPair[8]
  @ 13,5 FILL TO 13,46 COLOR &laClrPair[8]

  ** COLOR PAIR 9 ENABLED CTRL. **
  @ 11,7  SAY "< nabled>"      SIZE 1,9, 0 COLOR &laClrPair[9]
 
  ** COLOR PAIR 10 DISABLED CTRL **
  @ 11,19 SAY "<Disabled>"     SIZE 1,10, 0 COLOR &laClrPair[10]

  ** COLOR PAIR 7 HOT KEYS **
  @ 11,8  SAY "E" SIZE 1,1, 0 COLOR &laClrPair[7]
  
ELSE
  DO CASE
    CASE puScmObj = 1
      ** COLOR PAIR 1 NORMAL TEXT **
      @ 7,5 FILL TO 10,44 COLOR &laClrPair[1]
      @ 8,17  SAY "Dialog Message" SIZE 1,14, 0 COLOR &laClrPair[1]
      @ 11,5  SAY "  "  COLOR &laClrPair[1]
      @ 11,16 SAY "   " COLOR &laClrPair[1]
      @ 11,29 SAY "   " COLOR &laClrPair[1]
      @ 11,42 SAY "   " COLOR &laClrPair[1]


    CASE puScmObj = 3
      ** COLOR PAIR 3 BORDER **
      @ 6,4 TO 12,45 DOUBLE COLOR &laClrPair[3]

    CASE puScmObj = 6
      ** COLOR PAIR 6 SELECTED OPT. **
      @ 11,32 SAY "<Selected>"     SIZE 1,10, 0 COLOR &laClrPair[6]

    CASE puScmObj = 7
      ** COLOR PAIR 7 HOT KEYS **
      @ 11,8  SAY "E" SIZE 1,1, 0 COLOR &laClrPair[7]

    CASE puScmObj = 8
      ** COLOR PAIR 8 SHADOW **
      @ 7,46 FILL TO 13,46 COLOR &laClrPair[8]
      @ 7,47 FILL TO 13,47 COLOR &laClrPair[8]
      @ 13,5 FILL TO 13,46 COLOR &laClrPair[8]

    CASE puScmObj = 9
      ** COLOR PAIR 9 ENABLED CTRL. **
      @ 11,7  SAY "< nabled>"      SIZE 1,9, 0 COLOR &laClrPair[9]
      @ 11,8  SAY "E" SIZE 1,1, 0 COLOR &laClrPair[7]

    CASE puScmObj = 10
      ** COLOR PAIR 10 DISABLED CTRL **
      @ 11,19 SAY "<Disabled>"     SIZE 1,10, 0 COLOR &laClrPair[10]
   
  ENDCASE
ENDIF  
RETURN


*!**************************************************************************
*!
*!      PROCEDURE : lpNo6      Dlog Pops  ****
*!
*!**************************************************************************
*

PROCEDURE lpNo6

*** SCHEME 6 ***

IF llAllObjs
  llAllObjs = .F.

  @ 6,7 FILL TO 15,44           COLOR(SCHEME(5,1))
  @ 16,9 FILL TO 16,44 COLOR(SCHEME(5,8))
  @ 7,45  FILL TO 16,45 COLOR(SCHEME(5,8))
  @ 7,46  FILL TO 16,46 COLOR(SCHEME(5,8))
  @ 6,7 TO 15,44 DOUBLE         COLOR(SCHEME(5,3))
  @ 7,9 SAY "Dialog Popups:"    COLOR(SCHEME(5,1))
  @ 13,27 SAY "<Disable ctrl.>" COLOR(SCHEME(5,10))
  @ 11,27 SAY "< nable ctrl. >" COLOR(SCHEME(5,9))
  @ 11,28 SAY "E" COLOR(SCHEME(5,7))

  ** COLOR PAIR 3 BORDER **
  @ 9,9 TO 13,22  COLOR &laClrPair[3]

  ** COLOR PAIR 2 ENABLE OPT. **
  @ 10,10 SAY " nable      " COLOR &laClrPair[2]
  
  ** COLOR PAIR 7 HOT KEYS **
  @ 10,10 SAY "E"            COLOR &laClrPair[7]

  ** COLOR PAIR 6 SELECTED ITEM **
  @ 11,10 SAY "Selected    " COLOR &laClrPair[6]

  ** COLOR PAIR 1 DISABLE OPT. **
  @ 12,10 SAY "Disable     " COLOR &laClrPair[1]
  
ELSE
  DO CASE
    CASE puScmObj = 1
      ** COLOR PAIR 1 DISABLE OPT. **
      @ 12,10 SAY "Disable     " COLOR &laClrPair[1]
  
    CASE puScmObj = 2
      ** COLOR PAIR 2 ENABLE OPT. **
      @ 10,10 SAY " nable      " COLOR &laClrPair[2]
      @ 10,10 SAY "E"            COLOR &laClrPair[7]
    
    CASE puScmObj = 3
      ** COLOR PAIR 3 BORDER **
      @ 9,9 TO 13,22 COLOR &laClrPair[3]
    
    CASE puScmObj = 6
      ** COLOR PAIR 6 SELECTED ITEM **
      @ 11,10 SAY "Selected    " COLOR &laClrPair[6]
    
    CASE puScmObj = 7
      ** COLOR PAIR 7 HOT KEYS **
      @ 10,10 SAY "E"            COLOR &laClrPair[7]
    
  ENDCASE
ENDIF
RETURN

*!**************************************************************************
*!
*!      PROCEDURE : lpNo7      ****  Alert  ****
*!
*!**************************************************************************
*

PROCEDURE lpNo7

*** SCHEME 7 ***

IF llAllObjs
  llAllObjs = .F.

  ** COLOR PAIR 1 NORMAL TEXT **
  @ 6,4 FILL TO 12,45        COLOR &laClrPair[1]
  @ 8,19 SAY "Alert Message" COLOR &laClrPair[1]

  ** COLOR PAIR 3 BORDER **
  @ 6,4 TO 12,45 DOUBLE      COLOR &laClrPair[3]

  ** COLOR PAIR 8 SHADOW **
  @ 7,46 FILL TO 13,46 COLOR &laClrPair[8]
  @ 7,47 FILL TO 13,47 COLOR &laClrPair[8]
  @ 13,5 FILL TO 13,46 COLOR &laClrPair[8]

  ** COLOR PAIR 9 ENABLED CTRL. **
  @ 11,8 SAY "< nabled>" SIZE 1,9, 0 COLOR &laClrPair[9]
 
  ** COLOR PAIR 10 DISABLED CTRL. **
  @ 11,20 SAY "<Disabled>" SIZE 1,10, 0 COLOR &laClrPair[10]

  ** COLOR PAIR 6 SELECTED ITEM **
  @ 11,33 SAY "<Selected>" SIZE 1,10, 0 COLOR &laClrPair[6]

  ** COLOR PAIR 7 HOT KEYS **
  @ 11,9 SAY "E" SIZE 1,1, 0  COLOR &laClrPair[7]

ELSE
  DO CASE
    CASE puScmObj = 1
      ** COLOR PAIR 1 NORMAL TEXT **
      @ 7,5 FILL TO 10,44         COLOR &laClrPair[1]
      @ 8,19  SAY "Alert Message" COLOR &laClrPair[1]
      @ 11,5  SAY "   " COLOR &laClrPair[1]
      @ 11,17 SAY "   " COLOR &laClrPair[1]
      @ 11,30 SAY "   " COLOR &laClrPair[1]
      @ 11,43 SAY "  "  COLOR &laClrPair[1]
     
    CASE puScmObj = 3
      ** COLOR PAIR 3 BORDER **
      @ 6,4 TO 12,45 DOUBLE      COLOR &laClrPair[3]
    
    CASE puScmObj = 6
      ** COLOR PAIR 6 SELECTED ITEM **
      @ 11,33 SAY "<Selected>" SIZE 1,10, 0 COLOR &laClrPair[6]
    
    CASE puScmObj = 7
      ** COLOR PAIR 7 HOT KEYS **
      @ 11,9 SAY "E" SIZE 1,1, 0  COLOR &laClrPair[7]
    
    CASE puScmObj = 8
      ** COLOR PAIR 8 SHADOW **
      @ 7,46 FILL TO 13,46 COLOR &laClrPair[8]
      @ 7,47 FILL TO 13,47 COLOR &laClrPair[8]
      @ 13,5 FILL TO 13,46 COLOR &laClrPair[8]
      
    CASE puScmObj = 9
      ** COLOR PAIR 9 ENABLED CTRL. **
      @ 11,8 SAY "< nabled>" SIZE 1,9, 0 COLOR &laClrPair[9]
      @ 11,9 SAY "E" SIZE 1,1, 0  COLOR &laClrPair[7]
    
    CASE puScmObj = 10
      ** COLOR PAIR 10 DISABLED CTRL. **
      @ 11,20 SAY "<Disabled>" SIZE 1,10, 0 COLOR &laClrPair[10]

  ENDCASE
ENDIF

RETURN

*!**************************************************************************
*!
*!      PROCEDURE : lpNo8      ****  Windows  ****
*!
*!**************************************************************************
*

PROCEDURE lpNo8

*** SCHEME 8 ***

IF llAllObjs
  llAllObjs = .F.
  
  ** COLOR PAIR 1 NORMAL TEXT **
  @ 6,4 FILL TO 14,42  COLOR &laClrPair[1]
  @ 8,5 SAY " Prompt : " COLOR &laClrPair[1]

  ** COLOR PAIR 3 BORDER **
  @ 6,4 TO 14,42 COLOR &laClrPair[3]
  
  ** COLOR PAIR 4 TITLE, ACTIVE **
  @ 6,15 SAY "Tit_act" COLOR &laClrPair[4] 
  
  ** COLOR PAIR 5 TITLE, IDLE **
  @ 6,24 SAY "Tit_idl" COLOR &laClrPair[5]
  
  ** COLOR PAIR 2 TEXT BOX **  
  @ 8,15 SAY "Text box" COLOR &laClrPair[2]

  @ 8,27 TO 12,40   COLOR(SCHEME(9,3))
  @ 9,28  SAY "E"   COLOR(SCHEME(9,7))
  @ 9,29  SAY "nable      "  COLOR(SCHEME(9,2))
  @ 10,28 SAY "Disable     " COLOR(SCHEME(9,1))
  @ 11,28 SAY "Selected    " COLOR(SCHEME(9,6))

  ** COLOR PAIR 9 ENABLE CTRL. **
  @ 13,7 SAY "< nable>" COLOR &laClrPair[9]

  ** COLOR PAIR 7 HOT KEYS **
  @ 13,8 SAY "E"        COLOR &laClrPair[7]

  ** COLOR PAIR 10 DISABLE CTRL. **
  @ 13,18 SAY "<Disable>" COLOR &laClrPair[10]

  ** COLOR PAIR 6 SELECTED OPTION
  @ 13,30 SAY "<Selected>" COLOR &laClrPair[6]
  
  ** COLOR PAIR 8 SHADOW **
  @ 15,6 FILL TO 15,42 COLOR &laClrPair[8]
  @ 7,43 FILL TO 15,43 COLOR &laClrPair[8]
  @ 7,44 FILL TO 15,44 COLOR &laClrPair[8]
ELSE
  DO CASE
    CASE puScmObj = 1
      ** COLOR PAIR 1 NORMAL TEXT **
      @ 7,5 FILL TO 7,41   COLOR &laClrPair[1]
      @ 8,5 SAY " Prompt : " COLOR &laClrPair[1]
      @ 8,23 SAY "    "    COLOR &laClrPair[1]
      @ 9,5 FILL TO 12,26  COLOR &laClrPair[1]
      @ 8,41 FILL TO 12,41 COLOR &laClrPair[1]
      @ 13,5 SAY "  "      COLOR &laClrPair[1]
      @ 13,15 SAY "   "    COLOR &laClrPair[1]
      @ 13,27 SAY "   "    COLOR &laClrPair[1]
      @ 13,40 SAY "  "     COLOR &laClrPair[1]
      

    CASE puScmObj = 2
      ** COLOR PAIR 2 TEXT BOX **  
      @ 8,15 SAY "Text box" COLOR &laClrPair[2]

    CASE puScmObj = 3
      ** COLOR PAIR 3 BORDER **
      @ 6,4 TO 14,42 COLOR &laClrPair[3]
      @ 6,15 SAY "Tit_act" COLOR &laClrPair[4] 
      @ 6,24 SAY "Tit_idl" COLOR &laClrPair[5]
      
  
    CASE puScmObj = 4
      ** COLOR PAIR 4 TITLE, ACTIVE **
      @ 6,15 SAY "Tit_act" COLOR &laClrPair[4] 
  
    CASE puScmObj = 5
      ** COLOR PAIR 5 TITLE, IDLE **
      @ 6,24 SAY "Tit_idl" COLOR &laClrPair[5]
  
    CASE puScmObj = 6
      ** COLOR PAIR 6 SELECTED OPTION
      @ 13,30 SAY "<Selected>" COLOR &laClrPair[6]
  
    CASE puScmObj = 7
      ** COLOR PAIR 7 HOT KEYS **
      @ 13,8 SAY "E"        COLOR &laClrPair[7]

    CASE puScmObj = 8
      ** COLOR PAIR 8 SHADOW **
      @ 15,6 FILL TO 15,42 COLOR &laClrPair[8]
      @ 7,43 FILL TO 15,43 COLOR &laClrPair[8]
      @ 7,44 FILL TO 15,44 COLOR &laClrPair[8]

    CASE puScmObj = 9
      ** COLOR PAIR 9 ENABLE CTRL. **
      @ 13,7 SAY "< nable>" COLOR &laClrPair[9]
      @ 13,8 SAY "E"        COLOR &laClrPair[7]

    CASE puScmObj = 10
      ** COLOR PAIR 10 DISABLE CTRL. **
      @ 13,18 SAY "<Disable>" COLOR &laClrPair[10]

  ENDCASE
ENDIF
RETURN


*!**************************************************************************
*!
*!      PROCEDURE : lpNo9      ****  Wind Pops  ****
*!
*!**************************************************************************
*

PROCEDURE lpNo9

*** SCHEME 9 ***

IF llAllObjs
  llAllObjs = .F.
  @ 6,4 FILL TO 14,42  COLOR(SCHEME(8,1))
  @ 8,5 SAY " Prompt : " COLOR(SCHEME(8,1))
  @ 6,4 TO 14,42 COLOR(SCHEME(8,3))
  @ 6,15 SAY "Tit_act" COLOR(SCHEME(8,4))
  @ 6,24 SAY "Tit_idl" COLOR(SCHEME(8,5))
  @ 8,15 SAY "Text box" COLOR(SCHEME(8,2))

  ** COLOR PAIR 3 BORDER **
  @ 8,27 TO 12,40   COLOR &laClrPair[3]
  
  ** COLOR PAIR 7 HOT KEYS **
  @ 9,28  SAY "E"   COLOR &laClrPair[7]
  
  ** COLOR PAIR 2 ENABLE OPT. **
  @ 9,29  SAY "nable      "  COLOR &laClrPair[2]
  
  ** COLOR PAIR 1 DISABLE OPT. **
  @ 10,28 SAY "Disable     " COLOR &laClrPair[1]
  
  ** COLOR PAIR 6 SELECTED OPT. **
  @ 11,28 SAY "Selected    " COLOR &laClrPair[6]


  @ 13,7 SAY "< nable>" COLOR(SCHEME(8,9))
  @ 13,8 SAY "E"        COLOR(SCHEME(8,7))
  @ 13,18 SAY "<Disable>" COLOR(SCHEME(8,10))
  @ 13,30 SAY "<Selected>" COLOR(SCHEME(8,6))
  @ 15,6 FILL TO 15,42 COLOR(SCHEME(8,8))
  @ 7,43 FILL TO 15,43 COLOR(SCHEME(8,8))
  @ 7,44 FILL TO 15,44 COLOR(SCHEME(8,8))
ELSE
  DO CASE
    CASE puScmObj = 1
      ** COLOR PAIR 1 DISABLE OPT. **
      @ 10,28 SAY "Disable     " COLOR &laClrPair[1]
  
    CASE puScmObj = 2
      ** COLOR PAIR 2 ENABLE OPT. **
      @ 9,29  SAY "nable      "  COLOR &laClrPair[2]
  
    CASE puScmObj = 3
      ** COLOR PAIR 3 BORDER **
      @ 8,27 TO 12,40   COLOR &laClrPair[3]

    CASE puScmObj = 6  
      ** COLOR PAIR 6 SELECTED OPT. **
      @ 11,28 SAY "Selected    " COLOR &laClrPair[6]

    CASE puScmObj = 7  
      ** COLOR PAIR 7 HOT KEYS **
      @ 9,28  SAY "E"   COLOR &laClrPair[7]

  ENDCASE

ENDIF

RETURN


*!**************************************************************************
*!
*!      PROCEDURE : lpNo10     ****  Browse  ****
*!
*!**************************************************************************
*

PROCEDURE lpNo10

*** SCHEME 10 ***
IF llAllObjs
  llAllObjs = .F.

  ** COLOR PAIR 1 OTHER RECORDS **
  @ 5,7 FILL TO 14,41   COLOR &laClrPair[1]
  @ 6,18 TO 13,18       COLOR &laClrPair[1]
  @ 6,29 TO 13,29       COLOR &laClrPair[1]
  @ 7,8 TO 7,40 DOUBLE  COLOR &laClrPair[1]
  @ 7,18  SAY "Ø"  SIZE 1,1, 0 COLOR &laClrPair[1]
  @ 7,29  SAY "Ø"  SIZE 1,1, 0 COLOR &laClrPair[1]

  ** COLOR PAIR 3 BORDER **
  lcBorClr = SUBSTR(laClrPair[3],AT("/",laClrPair[3])+1)
  @ 5,7   FILL TO 5,41       COLOR &laClrPair[3]
  @ 6,7   FILL TO 14,7       COLOR &laClrPair[3]
  @ 14,8  FILL TO 14,41      COLOR &laClrPair[3]
  @ 6,41  FILL TO 13,41      COLOR &laClrPair[3]
  @ 5,7   SAY CHR(254)       COLOR &laClrPair[3]
  @ 5,41  SAY CHR(240)       COLOR &laClrPair[3]
  @ 14,7  SAY CHR(29)        COLOR &laClrPair[3]
  @ 14,41 SAY CHR(46)        COLOR &laClrPair[3]
  @ 14,8  SAY CHR(17)+CHR(4) COLOR &laClrPair[3]
  @ 14,40 SAY CHR(16)        COLOR &laClrPair[3]
  @ 6,41  SAY CHR(30)        COLOR &laClrPair[3]
  @ 7,41  SAY CHR(4)         COLOR &laClrPair[3]
  @ 13,41 SAY CHR(31)        COLOR &laClrPair[3]

  ** COLOR PAIR 8 SHADOW **
  @ 6,42 FILL TO 15,42 COLOR &laClrPair[8]
  @ 6,43 FILL TO 15,43 COLOR &laClrPair[8]
  @ 15,9 FILL TO 15,41 COLOR &laClrPair[8]

  ** COLOR PAIR 4 TITLE ACTIVE **
  @ 5,16 SAY "Tit_Act"  SIZE 1,7, 0 COLOR &laClrPair[4]

  ** COLOR PAIR 5 TITLE IDLE **
  @ 5,26 SAY "Tit_Idl"  SIZE 1,7, 0 COLOR &laClrPair[5]

  ** COLOR PAIR 1 OTHER RECORDS **
  @ 6,9   SAY "Field1"   SIZE 1,6, 0 COLOR &laClrPair[1]
  @ 6,19  SAY "Field2"   SIZE 1,6, 0 COLOR &laClrPair[1]
  @ 6,30  SAY "Field3"   SIZE 1,6, 0 COLOR &laClrPair[1]
  @ 8,9   SAY "Other"    SIZE 1,5, 0 COLOR &laClrPair[1]
  @ 8,19  SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]
  @ 8,30  SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]
  @ 10,9  SAY "Other"    SIZE 1,5, 0 COLOR &laClrPair[1]
  @ 10,19 SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]
  @ 10,30 SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]
  @ 11,9  SAY "Other"    SIZE 1,5, 0 COLOR &laClrPair[1]
  @ 11,19 SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]
  @ 11,30 SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]
  @ 12,9  SAY "Other"    SIZE 1,5, 0 COLOR &laClrPair[1]
  @ 12,19 SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]
  @ 12,30 SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]

  ** COLOR PAIR 2 CURRENT RECORD **
  @ 9,9   SAY "Cur. rec "  COLOR &laClrPair[2]
  @ 9,19  SAY "Data      "  COLOR &laClrPair[2]
  @ 9,30  SAY "Data       "  COLOR &laClrPair[2]

ELSE
  DO CASE

    CASE puScmObj = 1
      ** COLOR PAIR 1 OTHER RECORDS **
      @ 6,8   FILL TO 8,40  COLOR &laClrPair[1]
      @ 9,8   SAY " "       COLOR &laClrPair[1]
      @ 10,8  FILL TO 13,40 COLOR &laClrPair[1]
      
      @ 6,18 TO 13,18       COLOR &laClrPair[1]
      @ 6,29 TO 13,29       COLOR &laClrPair[1]
      @ 7,8 TO 7,40 DOUBLE  COLOR &laClrPair[1]
      @ 7,18  SAY "Ø"  SIZE 1,1, 0 COLOR &laClrPair[1]
      @ 7,29  SAY "Ø"  SIZE 1,1, 0 COLOR &laClrPair[1]
      @ 6,9   SAY "Field1"   SIZE 1,6, 0 COLOR &laClrPair[1]
      @ 6,19  SAY "Field2"   SIZE 1,6, 0 COLOR &laClrPair[1]
      @ 6,30  SAY "Field3"   SIZE 1,6, 0 COLOR &laClrPair[1]
      @ 8,9   SAY "Other"    SIZE 1,5, 0 COLOR &laClrPair[1]
      @ 8,19  SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]
      @ 8,30  SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]
      @ 10,9  SAY "Other"    SIZE 1,5, 0 COLOR &laClrPair[1]
      @ 10,19 SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]
      @ 10,30 SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]
      @ 11,9  SAY "Other"    SIZE 1,5, 0 COLOR &laClrPair[1]
      @ 11,19 SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]
      @ 11,30 SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]
      @ 12,9  SAY "Other"    SIZE 1,5, 0 COLOR &laClrPair[1]
      @ 12,19 SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]
      @ 12,30 SAY "Data"     SIZE 1,4, 0 COLOR &laClrPair[1]

    CASE puScmObj = 2
      ** COLOR PAIR 2 CURRENT RECORD **
      @ 9,9   SAY "Cur. rec "    COLOR &laClrPair[2]
      @ 9,19  SAY "Data      "   COLOR &laClrPair[2]
      @ 9,30  SAY "Data       "  COLOR &laClrPair[2]
    
    CASE puScmObj = 3
      ** COLOR PAIR 3 BORDER **
      @ 5,7   FILL TO 5,41       COLOR &laClrPair[3]
      @ 6,7   FILL TO 14,7       COLOR &laClrPair[3]
      @ 14,8  FILL TO 14,41      COLOR &laClrPair[3]
      @ 6,41  FILL TO 13,41      COLOR &laClrPair[3]
      @ 5,7   SAY CHR(254)       COLOR &laClrPair[3]
      @ 5,41  SAY CHR(240)       COLOR &laClrPair[3]
      @ 14,7  SAY CHR(29)        COLOR &laClrPair[3]
      @ 14,41 SAY CHR(46)        COLOR &laClrPair[3]
      @ 14,8  SAY CHR(17)+CHR(4) COLOR &laClrPair[3]
      @ 14,40 SAY CHR(16)        COLOR &laClrPair[3]
      @ 6,41  SAY CHR(30)        COLOR &laClrPair[3]
      @ 7,41  SAY CHR(4)         COLOR &laClrPair[3]
      @ 13,41 SAY CHR(31)        COLOR &laClrPair[3]
      
      @ 5,16 SAY "Tit_Act"  SIZE 1,7, 0 COLOR &laClrPair[4]
      @ 5,26 SAY "Tit_Idl"  SIZE 1,7, 0 COLOR &laClrPair[5]
    
    CASE puScmObj = 4
      ** COLOR PAIR 4 TITLE ACTIVE **
      @ 5,16 SAY "Tit_Act"  SIZE 1,7, 0 COLOR &laClrPair[4]

    CASE puScmObj = 5
      ** COLOR PAIR 5 TITLE IDLE **
      @ 5,26 SAY "Tit_Idl"  SIZE 1,7, 0 COLOR &laClrPair[5]

    CASE puScmObj = 8
      ** COLOR PAIR 8 SHADOW **
      @ 6,42 FILL TO 15,42 COLOR &laClrPair[8]
      @ 6,43 FILL TO 15,43 COLOR &laClrPair[8]
      @ 15,9 FILL TO 15,41 COLOR &laClrPair[8]
    
  ENDCASE
ENDIF
RETURN

*!**************************************************************************
*!
*!      PROCEDURE : lpNo11     ****  Alert Pops  ****
*!
*!**************************************************************************
*

PROCEDURE lpNo11

*** SCHEME 11 ***
IF llAllObjs
  llAllObjs = .F.

  ** COLOR PAIR 1 TEXT & B FULL **
  @ 6,6 FILL TO 14,45       COLOR &laClrPair[1]
  @ 8,23 SAY "Report Title" COLOR &laClrPair[1]
  @ 14,7,14,44 BOX REPLICATE(CHR(177),9)

  ** COLOR PAIR 3 BORDER **
  @ 6,6  FILL TO 6,45  COLOR &laClrPair[3]
  @ 6,6  FILL TO 15,6  COLOR &laClrPair[3]
  @ 15,6 FILL TO 15,45 COLOR &laClrPair[3]
  @ 6,45 FILL TO 15,45 COLOR &laClrPair[3]
  @ 6,6 SAY "þ"        COLOR &laClrPair[3]
  @ 6,45 SAY "ð"       COLOR &laClrPair[3]
  @ 15,7 SAY ""       COLOR &laClrPair[3]
  @ 15,44 SAY ""      COLOR &laClrPair[3]
  @ 15,45 SAY "."      COLOR &laClrPair[3]
  @ 15,8 SAY ""       COLOR &laClrPair[3]
  
  ** COLOR PAIR 4 TITLE, ACTIVE **
  @ 6,15 SAY "Rpt_Actv" COLOR &laClrPair[4]
  
  ** COLOR PAIR 5 TITLE, IDLE **
  @ 6,29 SAY "Rpt_Idle" COLOR &laClrPair[5]
  
  ** COLOR PAIR 9 BAND A, FULL **
  @ 7,7 SAY " R:  4 C:  17 º Move º  Page Header  º" COLOR &laClrPair[9]
  lcPanClr = SUBSTR(laClrPair[9],AT("/",laClrPair[9])+1)
  @ 8,14 FILL TO 13,14  COLOR &laClrPair[9]
  @ 13,7 FILL TO 13,44  COLOR &laClrPair[9]
  @ 10,8 SAY "Detail"   COLOR &laClrPair[9]

  ** COLOR PAIR 7 BAND A, EMPTY **
  @ 11,8 SAY "Detail" COLOR &laClrPair[7]
  
  ** COLOR PAIR 10 BAND B, EMPTY **
  @ 12,8 SAY "PgFoot" COLOR &laClrPair[10]
    
  ** COLOR PAIR 2 REPORT FIELD **
  @ 10,18 SAY "Report field" COLOR &laClrPair[2]
  
  ** COLOR PAIR 6 SELECTED ITEM **
  @ 10,35 SAY "Selected" COLOR &laClrPair[6]
  
  ** COLOR PAIR 8 SHADOW **
  @ 16,7 FILL TO 16,45 COLOR &laClrPair[8]
  @ 7,46 FILL TO 16,46 COLOR &laClrPair[8]
  @ 7,47 FILL TO 16,47 COLOR &laClrPair[8]

ELSE
  DO CASE
    CASE puScmObj = 1  
      ** COLOR PAIR 1 TEXT & B FULL **
      @ 8,7 FILL TO 12,7  COLOR &laClrPair[1]
      @ 8,8 FILL TO 9,13  COLOR &laClrPair[1]
      @ 8,15 FILL TO 9,44 COLOR &laClrPair[1]
      @ 10,15 SAY "   "   COLOR &laClrPair[1]
      @ 10,30 SAY "     " COLOR &laClrPair[1]
      @ 10,43 SAY "  "    COLOR &laClrPair[1]
      @ 11,15 FILL TO 12,44 COLOR &laClrPair[1]
      @ 8,23 SAY "Report Title" COLOR &laClrPair[1]

    CASE puScmObj = 3
      ** COLOR PAIR 3 BORDER **
      @ 6,6  FILL TO 6,14  COLOR &laClrPair[3]
      @ 6,23 FILL TO 6,28  COLOR &laClrPair[3]
      @ 6,37 FILL TO 6,45  COLOR &laClrPair[3]
      @ 6,6  FILL TO 15,6  COLOR &laClrPair[3]
      @ 15,6 FILL TO 15,45 COLOR &laClrPair[3]
      @ 6,45 FILL TO 15,45 COLOR &laClrPair[3]
      @ 6,6 SAY "þ"        COLOR &laClrPair[3]
      @ 6,45 SAY "ð"       COLOR &laClrPair[3]
      @ 15,7 SAY ""       COLOR &laClrPair[3]
      @ 15,44 SAY ""      COLOR &laClrPair[3]
      @ 15,45 SAY "."      COLOR &laClrPair[3]
      @ 15,8 SAY ""       COLOR &laClrPair[3]
      @ 6,15 SAY "Rpt_Actv" COLOR &laClrPair[4]
      @ 6,29 SAY "Rpt_Idle" COLOR &laClrPair[5]
  
    CASE puScmObj = 4
      ** COLOR PAIR 4 TITLE, ACTIVE **
      @ 6,15 SAY "Rpt_Actv" COLOR &laClrPair[4]
  
    CASE puScmObj = 5
      ** COLOR PAIR 5 TITLE, IDLE **
      @ 6,29 SAY "Rpt_Idle" COLOR &laClrPair[5]
  
    CASE puScmObj = 9
      ** COLOR PAIR 9 BAND A, FULL **
      @ 7,7 SAY " R:  4 C:  17 º Move º  Page Header  º" COLOR &laClrPair[9]
      lcPanClr = SUBSTR(laClrPair[9],AT("/",laClrPair[9])+1)
      @ 8,14 FILL TO 13,14 COLOR &laClrPair[9]
      @ 13,7 FILL TO 13,44 COLOR &laClrPair[9]
      @ 10,8 SAY "Detail"  COLOR &laClrPair[9]

    CASE puScmObj = 7
      ** COLOR PAIR 7 BAND A, EMPTY **
      @ 11,8 SAY "Detail" COLOR &laClrPair[7]
  
    CASE puScmObj = 10
      ** COLOR PAIR 10 BAND B, EMPTY **
      @ 12,8 SAY "PgFoot" COLOR &laClrPair[10]
    
    CASE puScmObj = 2
      ** COLOR PAIR 2 REPORT FIELD **
      @ 10,18 SAY "Report field" COLOR &laClrPair[2]
  
    CASE puScmObj = 6
      ** COLOR PAIR 6 SELECTED ITEM **
      @ 10,35 SAY "Selected" COLOR &laClrPair[6]
  
    CASE puScmObj = 8
      ** COLOR PAIR 8 SHADOW **
      @ 16,7 FILL TO 16,45 COLOR &laClrPair[8]
      @ 7,46 FILL TO 16,46 COLOR &laClrPair[8]
      @ 7,47 FILL TO 16,47 COLOR &laClrPair[8]
  
  ENDCASE
ENDIF

RETURN


*!**************************************************************************
*!
*!      PROCEDURE : lpNo12     ****  Alert Pops  ****
*!
*!**************************************************************************
*

PROCEDURE lpNo12

*** SCHEME 12 ***

IF llAllObjs
  llAllObjs = .F.

  @ 6,7 FILL TO 15,44           COLOR(SCHEME(7,1))
  @ 16,9 FILL TO 16,44 COLOR(SCHEME(7,8))
  @ 7,45  FILL TO 16,45 COLOR(SCHEME(7,8))
  @ 7,46  FILL TO 16,46 COLOR(SCHEME(7,8))
  @ 6,7 TO 15,44 DOUBLE         COLOR(SCHEME(7,3))
  @ 7,9 SAY "Alert Popups:"    COLOR(SCHEME(7,1))
  @ 13,27 SAY "<Disable ctrl.>" COLOR(SCHEME(7,10))
  @ 11,27 SAY "< nable ctrl. >" COLOR(SCHEME(7,9))
  @ 11,28 SAY "E" COLOR(SCHEME(7,7))

  ** COLOR PAIR 3 BORDER **
  @ 9,9 TO 13,22  COLOR &laClrPair[3]

  ** COLOR PAIR 2 ENABLED OPT. **
  @ 10,10 SAY " nable      " COLOR &laClrPair[2]
  
  ** COLOR PAIR 7 HOT KEYS **
  @ 10,10 SAY "E"            COLOR &laClrPair[7]

  ** COLOR PAIR 6 SELECTED ITEM **
  @ 11,10 SAY "Selected    " COLOR &laClrPair[6]

  ** COLOR PAIR 1 DISABLED OPT. **
  @ 12,10 SAY "Disable     " COLOR &laClrPair[1]
  
ELSE
  DO CASE
    CASE puScmObj = 1
      ** COLOR PAIR 1 DISABLED OPT. **
      @ 12,10 SAY "Disable     " COLOR &laClrPair[1]
  
    CASE puScmObj = 2
      ** COLOR PAIR 2 ENABLED OPT. **
      @ 10,10 SAY " nable      " COLOR &laClrPair[2]
      @ 10,10 SAY "E"            COLOR &laClrPair[7]
    
    CASE puScmObj = 3
      ** COLOR PAIR 3 BORDER **
      @ 9,9 TO 13,22 COLOR &laClrPair[3]
    
    CASE puScmObj = 6
      ** COLOR PAIR 6 SELECTED ITEM **
      @ 11,10 SAY "Selected    " COLOR &laClrPair[6]
    
    CASE puScmObj = 7
      ** COLOR PAIR 7 HOT KEYS **
      @ 10,10 SAY "E"            COLOR &laClrPair[7]
    
  ENDCASE
ENDIF
RETURN



*!**************************************************************************
*!
*!       FUNCTION : lfvColSet
*!
*!**************************************************************************
*

FUNCTION lfvColSet

IF lcButton = "Save"

  *** If the choice save enable the push button save ***
  SHOW GET pbLdSv,1 PROMPT lcButton ENABLE
  
  *** If the choice save will overwrite any of the array elements ***
  *** (colorset names) : store the name in lcColSet variable .... ***
  lcColSet = ALLTRIM(laColSet[lsColset])
ENDIF

*** If choose any name from  the list ***
*** should convert the current object ***
*** to save push button ............. ***
pbLdSv = 1
_CUROBJ=OBJNUM(pbLdSv)
KEYBOARD "{ENTER}"
RETURN


*!**************************************************************************
*!
*!       FUNCTION : lfvLdSv
*!
*!**************************************************************************
*

FUNCTION lfvLdSv

DO CASE
  CASE pbLdSv = 1
  
    *** If press " Load " or " Save " ***
    IF lcButton = "Load"
    
      *** If press " Load " ***
      *** Refering to the choosen colorset to load ***
      lcClSt = ALLTRIM(laColSet[lsColset])
      SET COLOR SET TO &lcClSt
    ELSE
    
      *** If press " Save " ***
      *** Save the current colorset in the resource file ***
      CREATE COLOR SET (lcColSet)
      DECLARE laOneRec[1,1]
      SELECT * FROM (SET('RESOURCE',1)) ;
             WHERE UPPER(ALLTRIM(ID)) = 'COLORSET' .AND. ;
             UPPER(ALLTRIM(NAME))== UPPER(ALLTRIM(lcColSet));
             INTO ARRAY laOneRec

      SELECT SycResrc
      LOCATE FOR UPPER(ALLTRIM(ID))   == 'COLORSET' .AND. ;
                 UPPER(ALLTRIM(NAME)) == UPPER(ALLTRIM(lcColSet))
      IF FOUND()
        GATHER FROM laOneRec MEMO 
      ELSE
        INSERT INTO &gcSysHome.SycResrc FROM ARRAY laOneRec 
      ENDIF

     ENDIF
    
  CASE pbLdSv = 2
    *** If press " Cancel " ***
    CLEAR READ
ENDCASE
RETURN


*!**************************************************************************
*!
*!       FUNCTION : lfvColrSet
*!
*!**************************************************************************
*

FUNCTION lfvColrSet

*** If any name written to save the current ***
*** colorset , the save push button should  ***
*** be enable ............................  ***
IF !EMPTY(lcColSet)
  SHOW GET pbLdSv,1 PROMPT lcButton ENABLE
  _CUROBJ = OBJNUM(pbLdSv)
ENDIF

RETURN


*!**************************************************************************
*!
*!       FUNCTION : lfShowSet
*!
*!**************************************************************************
*

FUNCTION lfShowSet

DO CASE
  CASE lcButton = "Load"
  
    *** Enable the load push button and disable ***
    *** the get field (colorset name) ......... ***
    SHOW GET pbLdSv,1 PROMPT lcButton ENABLE
    SHOW GET lcColSet DISABLE
    
  CASE lcButton = "Save"
  
    *** Disable the save push button & enable ***
    *** the get field (colorset name)........ ***
    SHOW GET pbLdSv,1 PROMPT lcButton DISABLE
    SHOW GET lcColSet ENABLE
ENDCASE
