*:************************************************************************
*:
*: Procedure file: SMPRTES.PRG 
*:                 (Printers Escape Seq.)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Hesham El-Sheltawy
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*:************************************************************************
PARAMETER lcPrnt_Nam,lnScrMode,lcFl_Nam,lcUsdFdTmp
          && The file name,the screen state,the calling file

*
* This screen is called from four places to do on of the following
*      1- Show the data of a single field; [lnScrMode = 2]
*      2- Allow you to create new fields (if called from menu); and
*      3- Allow you to create new fields, and assign them to an
*         application [lnScrMode = 4]
*      4- Allow you to modify existing field [lnScrMode = 3]
*

EXTERNAL ARRAY laData,laDefProc,laScrMode
DECLARE laKeyField[1,4],laFiles[1,1],laPorts[4,1]

*** If no parameter set lnScrMode to null
IF PARAMETERS() < 2
  lnScrMode = 0
ENDIF

IF PARAMETERS() < 3
  lcUsdFdTmp = ' '
ENDIF

lcOutPort = ""
ibPopOut  = 1
llBrowse  = .F.

IF !gfSetup()    
  RETURN
ENDIF  

*** This settings are executated once (if the window dosen't exist')
IF !WEXIST(lcBaseWind)
  laKeyField[1,1] = 'laData[1]'
  laKeyField[1,2] =.T.
  laKeyField[1,3] = 'P_NAME'
  laKeyField[1,4] = 1  
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  laPorts[1,1] = "COM1"
  laPorts[2,1] = "COM2"
  laPorts[3,1] = "LPT1"
  laPorts[4,1] = "LPT2"    
ENDIF

*** This settings are executated once (if the window dosen't exist')
*** this settings must be done after setup
IF !WEXIST('awrSmPrtEs')  && If the window is already exist don't
                          && change any thing but else :

  *** If the screen is for viewing/modifying single file
  IF (lnScrMode = 2) OR (lnScrMode = 3)
  SET ORDER TO 1
    SEEK lcPrnt_Nam
    SCATTER FIELDS &lcScFields MEMO TO laData 
    laScrMode=.F.
    laScrMode[lnScrMode] = .T.
  ELSE
    IF lnScrMode = 4    && The screen is called from the file screen

    ENDIF
  ENDIF
  lsoutport = "COM1"
ENDIF

*** If the screen is called from the file screen the lTmpMode = .F.
lTmpMode = .F.
IF lcUsdFdTmp <> " "
  lTmpMode = .T.
ENDIF

*** A statment added to the read clause of the screen so ***
*** it can be opened MODAL in case of view only mode...
lcModal=IIF (lnScrMode = 2,"MODAL"," ")


*E300683,4 Call *.SPR from screens directory
* DO SMPrtEs.SPR 
DO (gcScrDir + gcWinAppl + '\SMPrtEs.SPR')
*E300683,4 end  
*!****************************************************************
*!
*!      PROCEDURE : lpShow 
*!
*!****************************************************************
*
PROCEDURE lpShow
EXTERNAL ARRAY laScrMode,laData

DO CASE
  CASE lnScrMode = 2           && If screen is for viewing single field
    SHOW GETS DISABLE ONLY     && Disable all fields but the close button
    SHOW GET pbCls     ENABLE
    SHOW GET pbSetCode ENABLE
    SHOW GET PBOUT     ENABLE
  CASE laScrMode[1]
    IF lnScrMode = 4   && The screen is called from file screen

    ENDIF               
ENDCASE

DO CASE
  CASE _DOS
    lcOutPort = laData[2]
  CASE _WINDOWS
    ibPopOut  = ASCAN(laPorts,laData[2])
ENDCASE

*!****************************************************************
*!
*!      FUNCTION : LPTRAP
*!
*!****************************************************************
*
FUNCTION LPTRAP

lcCurObj = VARREAD()
FOR I= 1 TO OCCURS('CHR(',UPPER(&lcCurObj))
  &lcCurObj=IIF( SUBSTR(&lcCurObj, AT("CHR(",UPPER(&lcCurObj))+4,1)$"0123456789";
                ,SUBSTR(&lcCurObj,1,AT("CHR(",UPPER(&lcCurObj))-1);
                +CHR( VAL( SUBSTR( &lcCurObj,AT( "CHR(",UPPER(&lcCurObj))+4,;
                AT(')',&lcCurObj)-(AT( "CHR(",UPPER(&lcCurObj))+4))));
                +SUBSTR(&lcCurObj,AT(")",&lcCurObj)+1),&lcCurObj)
ENDFOR             
SHOW GET &lcCurObj             
=gfUpdate()

*!****************************************************************
*!
*!      FUNCTION : lfvData_1
*!
*!****************************************************************
*
FUNCTION lfvData_1

IF llBrowse .OR. (LASTKEY()=13 AND !EMPTY(laData[1]))
  =gfSeekRec() 
ENDIF 
llBrowse = .F.

*!****************************************************************
*!
*!      FUNCTION : lfvSetCode
*!
*!****************************************************************
*
FUNCTION lfvSetCode

*E300683,4 Call *.SPR from screens directory
* DO SMPRTCD.SPR 
DO (gcScrDir + gcWinAppl + '\SMPRTCD.SPR')
*E300683,4 end  
*!****************************************************************
*!
*!      FUNCTION : lfSetShow
*!
*!****************************************************************
*
FUNCTION lfSetShow

IF laScrMode[3] or laScrMode[4]
  SHOW GETS ENABLE ONLY
ENDIF
 
*!****************************************************************
*!
*!      FUNCTION : lfvOutPort
*!
*!****************************************************************
*
FUNCTION lfvOutPort

DO CASE
  CASE _DOS
    laData[2] = gfActPop(1,60,8,72,'laPorts',1,1,@lcOutPort)
    =lfRefresh()
  CASE _WINDOWS
    laData[2] = laPorts[ibPopOut,1]
    SHOW GET ibPopOut
ENDCASE
