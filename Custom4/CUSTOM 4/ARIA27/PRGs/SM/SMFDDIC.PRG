*:************************************************************************
*:
*: Procedure file: SMFDDIC.PRG 
*:                 (Field dictionary)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*
*E300278,1 Reham On 08/23/95
*E300278,1 1- Set default for the defined by radio button at the beginning.
*B600984,1 Hesham El-Sheltawi 03/31/96
*B600984,1 Change the validation of the push button <Valid Expr.>
*B600984,1 in the screen to call the gfZoom instead the screen
*B600984,1 called SMVLDST.SPR because this was a feature that
*B600984,1 in not needed and at the same time we did not complete
*B600984,1 this feature. So note that the function called lfvVld_str
*B600984,1 is calling SMVLDST.SPR and this function lfvVld_str is
*B600984,1 not used any more and all the validation and when functions
*B600984,1 exist in the screen SMVLDST.SPR is not used any more
*E300643,1  HS 04/10/97 Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS] and added
*E300643,1              the field cFld_Name to the end of the endix tag
*E300643,1              [CODES] exprision.
*E300683,4 AMM 06/08/97 Add prgs directory path to the calling of programs
*E300867,1 AMM 05/06/98 Add the ability to define any related field as 
*E300867,1 AMM          mandatory field.
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*B803923,1 ADEL 02/15/20001 Set a defalt value for any onn-logic user defined field.
*B606682,1 RAE 01/02/2003 Let the width of the related field 6 characters if it is a valid codes.
*B606751,1 RAE 01/05/2003 Check the width of all the data types before saving.
*:***********************************************************************************************

PARAMETER pcAddNewFd
*** The file name,the screen state,the calling file

* This screen is called from four places to do on of the following
*      1- Show the data of a single field; [lnScrMode = 2]
*      2- Allow you to create new fields (if called from menu); and
*      3- Allow you to create new fields, and assign them to an
*         application [lnScrMode = 4]
*      4- Allow you to modify existing field [lnScrMode = 3]
*
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc,LACTRSTAT
*E300867,1 AMM Add two arrays to hold the related fields and old values of it.
DECLARE laRltFlds[1], laOldFlds[1]
laRltFlds = ''
laOldFlds = ''
*B803923,1 (Begin) Intialize needed variables.
STORE 0 TO lnSetDefa,lnIncAll,lnDefaNo
*B803923,1 (End)
*E300867,1 AMM end
DECLARE laKeyField[1,4],laFiles[1,1],laFld_Type[6,2],;
        laVldEntry[1,1],laApp[1,1]

laFld_Type[1,1] = "Character"
laFld_Type[1,2] = "C"
laFld_Type[2,1] = "Number"
laFld_Type[2,2] = "N"
laFld_Type[3,1] = "Date"
laFld_Type[3,2] = "D"
laFld_Type[4,1] = "Logic"
laFld_Type[4,2] = "L"
laFld_Type[5,1] = "Memo"
laFld_Type[5,2] = "M"
laFld_Type[6,1] = "General"
laFld_Type[6,2] = "G"

lcFld_Type = laFld_Type[1,1]

lcShowFun  = ""

pufld_typ  = 1
rbDfnBy    = IIF(EMPTY(GETENV('DEVELOP')),3,2)

lcAddNewFd = " "
lcFld_Nam  = " "
lnScrMode  = 0
lcFl_Nam   = " "
llBrowse   = .F.
lcDiamond  = ""
lcTSelect  = ""  && Hold the word "S\<elect"
lcTUnSelct = ""  && Hold the word "\<Unselect"
lcPrompt   = ""
lcOldRltd  = ""
llOld_12   = .F.
lcOld_13   = ""
llRelated  = .F.
lcCodStat  = ""
lcRltdStat = ""

IF !gfSetup()    
  RETURN
ENDIF  

IF TYPE("pcAddNewFd") = "C"
  lcAddNewFd = pcAddNewFd

  SELECT (lcAddNewFd)
  lcFl_Nam   = cFile_Nam
  lcFld_Nam  = cFld_Name
  lnScrMode  = nScrMode
  lcFil_Strc = mFil_Strc
  DO CASE
    CASE lnScrMode = 2
      SELECT sydField
      SET FILTER TO "|"+ALLTRIM(cFld_name)+"|" $ lcFil_Strc
    CASE lnScrMode = 3 OR lnScrMode = 4
  ENDCASE    
  SELECT sydField
ELSE
  lnScrMode = 0
ENDIF

*** This settings are executated once (if the window dosen't exist')
IF !WEXIST(lcBaseWind)
  laDefProc[7]    = .F.     && Don't use the default delete procedure
  laDefProc[9]    = .F.     && Don't use the default save procedure
  laDefProc[10]   = .F.     && Don't use the default cancel procedure  
  
  laKeyField[1,1] = 'laData[1]'
  laKeyField[1,2] =.T.
  laKeyField[1,3] = 'CFLD_NAME'
  laKeyField[1,4] = 1  
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
ENDIF

*** This settings are executated once (if the window dosen't exist')
*** this settings must be done after setup
IF !WEXIST(lcBaseWind)  && If the window is already exist don't
                          && change any thing but else :
  puFld_Typ    = 1
  lcFld_Type   = laFld_Type[1,1]
  laData [3]   = 0
  laFiles[1,1] = " "     

  *** If the screen is for viewing/modifying single file
  IF (lnScrMode = 2) OR (lnScrMode = 3)
    IF SEEK(ALLTRIM(UPPER(lcFld_Nam)),"sydField")
      SCATTER FIELDS &lcScFields MEMO TO laData 
      laScrMode            = .F.
      laScrMode[lnScrMode] = .T.
      *** List of files using this field
      SELECT DISTINCT IIF(!EMPTY(sydFiles.cFile_ttl),;
             sydFiles.cFile_ttl,sydFiles.cFile_nam+;
             SPACE(34-LEN(sydFiles.cFile_nam)));
             FROM (gcSysHome+"sydFlfld"),(gcSysHome+"sydFiles");
             WHERE sydFlfld.cFld_Name = lcFld_Nam ;
             .AND. sydFlfld.cFile_Nam = sydFiles.cFile_Nam;
             INTO ARRAY laFiles;
             ORDER BY sydFiles.cFile_ttl
    ENDIF
  ELSE
    IF lnScrMode = 4    && The screen is called from the file screen
      laFiles[1,1] = lcFl_Nam
    ENDIF
  ENDIF
ENDIF

*** Get the available application in the system. ***
*E301077,78 Hesham (Start)
=gfOpenFile(gcSysHome+'SYDAPPL')
*E301077,78 Hesham (End)
SELECT "  "+cApp_Id+"  "+cApp_Name ;
       FROM (gcSysHome+"sydAppl") ;
       INTO ARRAY laApp ;
       WHERE CAPP_ID <> "SY"

SELECT sydField
  
*** A statment added to the read clause of the screen, so ***
*** it can be opened MODAL in case of view only mode..... ***
lcModal   = IIF(lnScrMode <> 0 ,"MODAL"," ")

*** To execute the show procedure at the ***
*** beginning set this flag to .T. 
llDoLocal  = IIF(lnScrMode = 0,.F.,.T.)

DO CASE
  CASE lnScrMode  = 2
    laCtrStat     = "DISABLE"
    laCtrStat[10] = "ENABLE"
    laCtrStat[12] = "ENABLE"
  CASE lnScrMode  = 3
    laCtrStat     = "DISABLE"
    laCtrStat[11] = "ENABLE"
    laCtrStat[12] = "ENABLE"
  CASE lnScrMode  = 4
    laCtrStat     = "DISABLE"
    laCtrStat[12] = "ENABLE"
ENDCASE

*E300278,1 Reham On 08/23/95   *** Begin ***
*E300278,1 Set default for the radio button "Define by" at the beginning.
*rbDfnBy    = IIF(EMPTY(GETENV('DEVELOP')),3,AT(laData[10],'SAU'))
rbDfnBy    = IIF(EMPTY(GETENV('DEVELOP')),3,IIF(!EMPTY(laData[10]),AT(laData[10],'SAU'),2))
*E300278,1 Reham On 08/23/95   *** End   ***

lcCodStat  = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE",IIF(laData[16],"DISABLE","ENABLE"))
lcRltdStat = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE",IIF(laData[11],"ENABLE","DISABLE"))

*E300683,4 Call *.SPR from screens directory
* DO SMFDDIC.SPR 
DO (gcScrDir + gcWinAppl + '\SMFDDIC.SPR')
*E300683,4 end   

SELECT sydField
SET FILTER TO

*!***************************************************************
*!
*!      Procedure: lpShow
*!
*!****************************************************************
*
PROCEDURE lpShow
EXTERNAL ARRAY laScrMode,laData

*** Get the available application in the system. ***
*E301077,78 Hesham (Start)
=gfOpenFile(gcSysHome+'SYDAPPL')
*E301077,78 Hesham (End)
SELECT "  "+cApp_Id+"  "+cApp_Name ;
       FROM (gcSysHome+"sydAppl") ;
       INTO ARRAY laApp ;
       WHERE CAPP_ID <> "SY"

DO CASE
  CASE lnScrMode = 2           && If screen is for viewing single field
    DO lpDspButns
  CASE laScrMode[1]
    rbDfnBy    = IIF(EMPTY(GETENV('DEVELOP')),3,2)
    laData[10] = IIF(EMPTY(GETENV('DEVELOP')),"U","A")
    lsFiles    = 0
    laFiles    = ' '
    puFld_Typ  = 1
    lcFld_Type = laFld_Type[1,1]
    laData[2]  = laFld_Type[1,2]
    llRelated  = .F.
    laData[16] = .F.
    lcOldRltd  = ""
    SHOW GET laData[11] DISABLE
    SHOW GET laData[12] DISABLE
    *E300867,1 AMM Disable the list and check box in select mode
    *SHOW GET laData[13] DISABLE
    laRltFlds = ''
    laOldFlds = ''
    SHOW GET lnRltFlds DISABLE
    SHOW GET lnMand    DISABLE
    *E300867,1 AMM end
  CASE laScrMode[2]
    llRelated  = laData[16]
    lsFiles    = 0
    laFiles    = ' '
    rbDfnBy    = AT(laData[10],'SAU')
    llOld_12   = laData[12]
    lcOld_13   = laData[13]
    lcOldRltd  = laData[13]
    
    DO lpDspButns
    *** List of files using this field
    SELECT DISTINCT IIF(!EMPTY(sydFiles.cFile_ttl),;
           sydFiles.cFile_ttl,sydFiles.cFile_nam+;
           SPACE(34-LEN(sydFiles.cFile_nam)));
           FROM (gcSysHome+"sydFlfld"),(gcSysHome+"sydFiles");
           WHERE sydFlfld.cFld_Name = laData[1] ;
           .AND. sydFlfld.cFile_Nam = sydFiles.cFile_Nam;
           INTO ARRAY laFiles;
           ORDER BY sydFiles.cFile_ttl
    SHOW GET laData[11] DISABLE
    SHOW GET laData[12] DISABLE
    SHOW GET laData[13] DISABLE
    *E300867,1 AMM Fill the related feilds array and display it.
    lnRel = OCCURS('|',laData[13])
    DIMENSION laRltFlds[lnRel+1]
    laRltFlds=''
    =gfSubStr(laData[13],@laRltFlds,"|")
    *E300867,1 AMM if the related fields saved with the old format, add to it 
    *E300867,1 AMM one space to fit the new format.
    FOR lnCount=1 TO ALEN(laRltFlds)
      laRltFlds[lnCount] = IIF(LEFT(laRltFlds[lnCount],1)='$', ''+;
                           SUBSTR(laRltFlds[lnCount],2) , ;
                           IIF(LEFT(laRltFlds[lnCount],1) # SPACE(1),;
                           SPACE(1)+laRltFlds[lnCount],laRltFlds[lnCount]))  
    ENDFOR
    *E300867,1 AMM fill the old value array
    =ACOPY(laRltFlds,laOldFlds)
    SHOW GET lnRltFlds DISABLE
    SHOW GET lnMand DISABLE
    *E300867,1 AMM end
    
  CASE laScrMode[3]
    rbDfnBy    = AT(laData[10],'SAU')
    IF laData[16]
      SHOW GET laData[11] DISABLE
      SHOW GET laData[12] DISABLE
      *E300867,1 AMM In edit mode disable the list which hold the  related 
      *E300867,1 AMM fields if the main field is related to another.
      *SHOW GET laData[13] DISABLE
      SHOW GET lnRltFlds DISABLE
      SHOW GET lnMand DISABLE
      *E300867,1 AMM end
    ELSE
      SHOW GET laData[11] ENABLE
      IF laData[11]
        SHOW GET laData[12] ENABLE
        *E300867,1 AMM Enable the list and check box in edit mode in case of 
        *E300867,1 AMM the main field is not related to another and the 
        *E300867,1 AMM check box "valid code" enabled.
        *SHOW GET laData[13] ENABLE
        SHOW GET lnRltFlds ENABLE
        SHOW GET lnMand ENABLE
        *E300867,1 AMM end
      ELSE
        SHOW GET laData[12] DISABLE
        *E300867,1 AMM Disable the list and check box in case of not a 
        *E300867,1 AMM "valid code"
        *SHOW GET laData[13] DISABLE
        SHOW GET lnRltFlds DISABLE
        SHOW GET lnMand DISABLE
        *E300867,1 AMM end
      ENDIF
    ENDIF
    *B606682,1 RAE Let the width disable if the valid codes is checked. [start]
    IF laData[11]
      SHOW GET laData[3] DISABLE
      SHOW GET puFld_typ DISABLE
    ENDIF
    *B606682,1 RAE [end]
  CASE laScrMode[4]
    lcOldRltd  = ""
    laData[3]  = 10
    laData[10] = SUBSTR('SAU',rbDfnBy,1)
    laData[16] = .F.
    llRelated  = .F.
    SHOW GET laData[11] ENABLE
    SHOW GET laData[12] ENABLE
    *E300867,1 AMM Enable the list and mandatory check box in add mode
    *SHOW GET laData[13] ENABLE
    SHOW GET lnRltFlds ENABLE
    SHOW GET lnMand ENABLE
    *E300867,1 AMM end
ENDCASE

IF !laScrMode[1]              && Lists are always enabled ...
  SHOW GET lsFiles ENABLE     && except in select mode
  SHOW GET pbNotes ENABLE
ENDIF

IF laScrMode[3] .AND. laData[2] ="N"  && Show the decimal field
  SHOW GET laData[4] ENABLE           && enable if the type is
ELSE                                  && Numeric only.  
  SHOW GET laData[4] DISABLE
ENDIF

*laData[1]=IIF(laScrMode[1],LEFT(laData[1],LEN(laData[1])-1),laData[1])
*B803923,1 (Begin) Disable the valid entries button if the field is logic.
IF laData[2] = "L"
  SHOW GET pbVldEnt DISABLE
ENDIF  
*B803923,1 (End)

*!**************************************************************************
*!
*!      Function lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1

IF llBrowse .OR. (!EMPTY(laData[1]) .AND. LASTKEY() =  13)
  IF ALLTRIM(laData[1]) <> '?' .AND. !llBrowse
    IF !lfChkFld(laData[1]) 
      _CUROBJ = OBJNUM(laData[1])
      RETURN
    ENDIF 
  ENDIF   
  =gfSeekRec()
ENDIF

llBrowse  = .F.
SHOW GET laData[4] DISABLE

*!**************************************************************************
*!
*!      Function lfvData_7
*!
*!**************************************************************************
*
FUNCTION lfvData_7

IF !EMPTY(laData[7])
  IF EMPTY(laData[8])
    laData[8] = 'Enter the ' + LOWER(laData[7])
  ENDIF
  SHOW GET laData[7]
  SHOW GET laData[8]
ENDIF

*!*******************************************************
*!
*!      PROCEDURE lfvFld_Typ
*!
*!*******************************************************
*
PROCEDURE lfvFld_Typ

*** Field type popup valid clause. ***

DO CASE
  CASE _DOS
    laData[2] = gfActPop(4,11,12,23,'laFld_Type',2,1,@lcFld_Type)
    =lfRefresh()
  CASE _WINDOWS
  *B606751,1 RAE [start]
    *laData[2] = SUBSTR("CNDLMG",puFld_Typ)
    laData[2] = SUBSTR("CNDLMG",puFld_Typ,1)
  *B606751,1 RAE [END]  
ENDCASE

laData[4] = 0
SHOW GET laData[4] DISABLE
*B803923,1 (Begin) Enable it untill the user select a type.
SHOW GET pbVldEnt ENABLE
*B803923,1 (End)

*B606751,1 RAE [start]
*IF laScrMode[4]
*  DO CASE
*      CASE laData[2] $ "CMG"
*      laData[3] = 10
*      SHOW GET laData[4] DISABLE
*    CASE laData[2] ="N"
*      laData[3] = 10
*      SHOW GET laData[4] ENABLE
*    CASE laData[2] = "D"
*      laData[3] = 8
*      SHOW GET laData[4] DISABLE
*    CASE laData[2] = "L"
*      laData[3] = 1
*      SHOW GET laData[4] DISABLE
*  ENDCASE
*ENDIF

DO CASE
 CASE laData[2] $ "CMG"
    laData[3] = 10
    SHOW GET laData[3] ENABLE
    SHOW GET laData[4] DISABLE
  CASE laData[2] ="N"
    laData[3] = 10
    SHOW GET laData[3] ENABLE
    SHOW GET laData[4] ENABLE
  CASE laData[2] = "D"
    laData[3] = 8
    SHOW GET laData[3] DISABLE
    SHOW GET laData[4] DISABLE
  CASE laData[2] = "L"
    laData[3] = 1
    SHOW GET laData[3] DISABLE
    SHOW GET laData[4] DISABLE
ENDCASE
*B606751,1 RAE [end]

SHOW GET laData[3]
*B803923,1 (Begin) Disable the valid entries button if the field is logic.
IF laData[2] = "L"
  SHOW GET pbVldEnt DISABLE
ENDIF  
*B803923,1 (End)

*!**************************************************************************
*!
*!      Function lfvData_10
*!
*!**************************************************************************
*
FUNCTION lfvData_10

laData[10] = SUBSTR('SAU',rbDfnBy,1)

*!**************************************************************************
*!
*!      Function lfvData_11
*!
*!E300867,1 AMM Valid function of the check box "Valid code"
*!**************************************************************************
*
FUNCTION lfvData_11
*E300867,1 AMM if this check box is checked, enable the related fields 
*E300867,1 AMM check box and list else disable them.

IF laData[11]
  laData[12] = llOld_12
  laData[13] = lcOld_13
  *E300867,1 AMM start, Get the old related field array
  *SHOW GET laData[13] ENABLE
  =ACOPY(laOldFlds,laRltFlds)
  SHOW GET lnRltFlds ENABLE
  SHOW GET lnMand ENABLE
  *E300867,1 AMM end
  SHOW GET laData[12] ENABLE

  *B606682,1 RAE Let the width field enable if the valid codes is checked.[start]
  laData[3] = 6
  laData[2] = laFld_Type[1,2]
  SHOW GET laData[3] DISABLE
  SHOW GET puFld_typ DISABLE
  *B606682,1 RAE [end]
ELSE
  llOld_12   = laData[12]
  lcOld_13   = laData[13]
  laData[12] = .F.
  laData[13] = ""
  *E300867,1 AMM Save the old values
  *SHOW GET laData[13] DISABLE
  =ACOPY(laRltFlds,laOldFlds)
  SHOW GET lnRltFlds DISABLE
  SHOW GET lnMand DISABLE
  *E300867,1 AMM end
  SHOW GET laData[12] DISABLE

  *B606682,1 RAE Let the width field enable if the valid codes isn't checked. [start]
  laData[3] = 6
  laData[2] = laFld_Type[1,2]
  SHOW GET laData[3] ENABLE
  SHOW GET puFld_typ ENABLE
  *B606682,1 RAE [end]
ENDIF

*!*******************************************************
*!
*!      PROCEDURE lfChkFld
*!
*!*******************************************************
*
PROCEDURE lfChkFld
PARAMETER lcStr

*** Check if the field is valid foxpro field name
lcStr = ALLTRIM(lcStr)

IF !EMPTY(lcStr)

  lcStrChr   = LEFT (lcStr,1)
  lnStrLngth = LEN(lcStr)
  lcStr      = RIGHT(lcStr,lnStrLngth - 1)
 
  IF ! lcStrChr $ 'CNLDMG'
    RETURN .F. 
  ENDIF

  IF !EMPTY(lcStr)
    FOR lnCount = 2 TO lnStrLngth
      lcStrChr  = LEFT (lcStr,1)
      lcStr     = RIGHT(lcStr,LEN(lcStr) - 1)
      IF !BETWEEN(ASC(lcStrChr),65,90) .AND.;
         !BETWEEN(ASC(lcStrChr),48,57) .AND.;
         ASC(lcStrChr) <> 95)
        RETURN .F. 
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Procedure lpDspButns
*!
*!**************************************************************************
*
PROCEDURE lpDspButns
*
SHOW GET laData[4] DISABLE  && The decimals field initialy disabled

DO CASE        && Field type
  CASE laData[2] = 'N' 
    puFld_Typ  = 2
    lcFld_Type = laFld_Type[2,1]
    IF !laScrMode [2]
      SHOW GET laData[4] ENABLE
    ENDIF
  CASE laData[2] = 'D'
    puFld_Typ  = 3
    lcFld_Type = laFld_Type[3,1]
    SHOW GET laData[3] DISABLE
    SHOW GET laData[4] DISABLE
  CASE laData[2] = 'L'
    puFld_Typ  = 4
    lcFld_Type = laFld_Type[4,1]
    SHOW GET laData[4] DISABLE
  CASE laData[2] = 'M'
    puFld_Typ  = 5
    lcFld_Type = laFld_Type[5,1]
    SHOW GET laData[3] DISABLE
    SHOW GET laData[4] DISABLE
  CASE laData[2] = 'G'
    puFld_Typ  = 6
    lcFld_Type = laFld_Type[6,1]
    SHOW GET laData[4] DISABLE
  OTHERWISE
    puFld_Typ  = 1
    lcFld_Type = laFld_Type[1,1]
    SHOW GET laData[4] DISABLE
ENDCASE

IF _DOS
  =lfRefresh()
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfvVld_str
*!
*!**************************************************************************
*
FUNCTION lfvVld_str

lcOldStr  = laData[6]
lcOldLoc  = laData[15]

lcVldStat = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE","ENABLE")
lcLocStat = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE","ENABLE")

FOR lnCount2 = 1 TO ALEN(laApp,1)
  IF SUBSTR(laApp[lnCount2,1],3,2) $ ALLTRIM(laData[15])
    laApp[lnCount2,1] = lcDiamond + SUBSTR(laApp[lnCount2,1],2)
  ELSE
    laApp[lnCount2,1] = " " + SUBSTR(laApp[lnCount2,1],2)
  ENDIF
ENDFOR

lcPrompt  = IIF(LEFT(laApp[1,1],1) = " ",lcTSelect,lcTUnSelct)

*E300683,4 Call *.SPR from screens directory
* DO SMVLDST.SPR 
DO (gcScrDir + gcWinAppl + '\SMVLDST.SPR')
*E300683,4 end   
*!**************************************************************************
*!
*!      Function: lfvSel
*!
*!**************************************************************************
*
FUNCTION lfvSel

=lfvModules()

*!**************************************************************************
*!
*!      Function: lfvAll
*!
*!**************************************************************************
*
FUNCTION lfvAll

FOR lnCont1 = 1 TO ALEN(laApp,1)
  IF LEFT(laApp[lnCont1,1],1) = " "
    laApp[lnCont1,1] = lcDiamond + SUBSTR(laApp[lnCont1,1],2)
  ENDIF
ENDFOR

SHOW GET lsModules

=lfStatus()

*!**************************************************************************
*!
*!      Function: lfvNon
*!
*!**************************************************************************
*
FUNCTION lfvNon

FOR lnCont1 = 1 TO ALEN(laApp,1)
  IF LEFT(laApp[lnCont1,1],1) = lcDiamond
    laApp[lnCont1,1] = " " + SUBSTR(laApp[lnCont1,1],2)
  ENDIF
ENDFOR

SHOW GET lsModules

=lfStatus()

*!**************************************************************************
*!
*!      Function: lfvInv
*!
*!**************************************************************************
*
FUNCTION lfvInv

FOR lnCont1 = 1 TO ALEN(laApp,1)
  IF LEFT(laApp[lnCont1,1],1) = " "
    laApp[lnCont1,1] = lcDiamond + SUBSTR(laApp[lnCont1,1],2)
  ELSE
    laApp[lnCont1,1] = " " + SUBSTR(laApp[lnCont1,1],2)
  ENDIF
ENDFOR

SHOW GET lsModules

=lfStatus()

*!**************************************************************************
*!
*!      Function: lfStatus
*!
*!**************************************************************************
*
FUNCTION lfStatus

IF LEFT(laApp[lsModules,1],1) = " "
  lcPrompt = lcTSelect
  SHOW GET pbSel,1 PROMPT lcTSelect
ELSE
  lcPrompt = lcTUnSelct
  SHOW GET pbSel,1 PROMPT lcTUnSelct
ENDIF

*!**************************************************************************
*!
*!      Function: lfvModules
*!
*!**************************************************************************
*** Valid function for the list that hold all the modules ***
*** linked to this company in the SMINSET screen...
*
FUNCTION lfvModules

IF lsModules = 0 .OR. lsModules > ALEN(laApp,1)
  RETURN
ENDIF

IF LEFT(laApp[lsModules,1],1) = " "
  laApp[lsModules,1] = lcDiamond + SUBSTR(laApp[lsModules,1],2)
ELSE
  laApp[lsModules,1] = " " + SUBSTR(laApp[lsModules,1],2)
ENDIF

_CUROBJ = _CUROBJ
SHOW GET lsModules
=lfStatus()

*!**************************************************************************
*!
*!      FUNCTION : lfvVldOk
*!
*!**************************************************************************
*
FUNCTION lfvVldOk

IF !laScrMode[2]
  laData[15] = ""
  FOR lnCount2 = 1 TO ALEN(laApp,1)
    IF LEFT(laApp[lnCount2,1],1) = lcDiamond
      laData[15] = laData[15] + ;
                   IIF(EMPTY(laData[15]),"","|") + ;
                   SUBSTR(laApp[lnCount2,1],3,2)
    ENDIF
  ENDFOR
  =gfUpdate()
ENDIF

IF laData[16]
  IF !laScrMode[2] .AND. (EMPTY(laData[6]) .AND. !EMPTY(laData[15])) .OR. ;
     (!EMPTY(laData[6]) .AND. EMPTY(laData[15]))
    *** Both of the valid function and its location ***
    *** should  not be empty or  you have to  blank ***
    *** both of them. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00202B00000","DIALOG")
    _CUROBJ = IIF(EMPTY(laData[6]),OBJNUM(laData[6]),OBJNUM(lsModules))
    RETURN
  ELSE
    CLEAR READ
  ENDIF
ELSE
  CLEAR READ
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfvVldEntry
*!
*!**************************************************************************
*
*** Valid function of valid entries object. ***

FUNCTION lfvVldEntry
DECLARE laDes_Tmp[1,1],laCod_Tmp[1,1]

*** Get the descriptions of all the valid entries in array. ***
lcAll_Desc = SUBSTR(laData[14],1,AT("~",laData[14])-1)
*B803923,1 (Begin) If the usr selcted to Include All before, Remove it from the string
*B803923,1         and update the check box.
IF ATC("ALL",lcAll_Desc) >0
  lcAll_Desc  = STRTRAN(lcAll_Desc,'All|','')
  lnIncAll = 1
ELSE
  lnIncAll = 0 
ENDIF
*B803923,1 (End)
=gfSubStr(lcAll_Desc,@laDes_Tmp,'|')

*** Get the codes of all the valid entries in array. ***
lcAll_Code = SUBSTR(laData[14],AT("~",laData[14])+1)
*B803923,1 (Begin) If the usr selcted to Include All before, Remove "|" from codes string.
IF SUBSTR(lcAll_Code,1,1) = "|" AND lnIncAll = 1
  lcAll_Code = SUBSTR(lcAll_Code,2)
ENDIF
*B803923,1 (End)
=gfSubStr(lcAll_Code,@laCod_Tmp,'|')

*** Prepare an array for the list lines hold from both arrays. ***
*** The codes array and the descriptions array.
DECLARE laVldEntry[ALEN(laCod_Tmp),1]
FOR lnCount = 1 TO ALEN(laVldEntry,1)
  *B803923,1 (Begin) If the user set a default value update the check box and remove the
  *B803923,1         special charcter "@" from the array.
  IF ATC("@",ALLTRIM(laCod_Tmp[lnCount,1]))>0
    lnDefaNo = lnCount
    laCod_Tmp[lnCount,1] = STRTRAN(laCod_Tmp[lnCount,1],"@","")
  ENDIF
  *B803923,1 (End)
  laVldEntry[lnCount,1] = ALLTRIM(laCod_Tmp[lnCount,1]) + ;
                          SPACE(12 - LEN(ALLTRIM(laCod_Tmp[lnCount,1]))) + ;
                          "   "+ALLTRIM(laDes_Tmp[lnCount,1])
ENDFOR

*E300683,4 Call *.SPR from screens directory
* DO SMVLDEN.SPR 
DO (gcScrDir + gcWinAppl + '\SMVLDEN.SPR')
*E300683,4 end   
*!***************************************************************
*!
*!      Procedure: lpVldShow
*!
*!****************************************************************
*
*** Show procedure of the valid entries screen. ***

PROCEDURE lpVldShow
EXTERNAL ARRAY laScrMode,laData

IF laScrMode[2]
  SHOW GETS DISABLE ONLY
  SHOW GET  pbOk       ENABLE
  SHOW GET  lsVldEntry ENABLE
ELSE
  SHOW GETS ENABLE ONLY
ENDIF

*B803923,1 (Begin) If value list is empty, clear the check boxes and initialize the defalut value.
IF EMPTY(laVldEntry)
  STORE 0 TO lnSetDefa,lnIncAll,lnDefaNo
  SHOW GET lnSetDefa DISABLE
  SHOW GET lnIncAll  DISABLE
ENDIF
*B803923,1 (End)

*!**************************************************************************
*!
*!      FUNCTION : lfwVld_Ent
*!
*!**************************************************************************
*
*** When function of the valid entries list. ***

FUNCTION lfwVld_Ent

IF ALEN(laCod_Tmp,1) = 1 .AND. EMPTY(laCod_Tmp[lsVldEntry,1]) .AND. ;
   ALEN(laDes_Tmp,1) = 1 .AND. EMPTY(laDes_Tmp[lsVldEntry,1])
   
  lsVldEntry = 1
  _CUROBJ    = OBJNUM(pbNew)
  RETURN .F.
ENDIF

lcCode    = laCod_Tmp[lsVldEntry,1]
lcDescrip = laDes_Tmp[lsVldEntry,1]

SHOW GET lcCode 
SHOW GET lcDescrip

*B803923,1 (Begin) Refresh 'Set as defaut' check box properly.
IF lsVldEntry = lnDefaNo
  lnSetDefa = 1
ELSE
  lnSetDefa = 0
ENDIF
SHOW GET lnSetDefa
*B803923,1 (End)

*!**************************************************************************
*!
*!      FUNCTION : lfvCode
*!
*!**************************************************************************
*
*** Valid function of the lcCode variable the hold the code. ***

FUNCTION lfvCode

IF !EMPTY(lcCode) .AND. LASTKEY() = 13
  laCod_Tmp [lsVldEntry,1] = lcCode
  laVldEntry[lsVldEntry,1] = ALLTRIM(laCod_Tmp[lsVldEntry,1]) + ;
                             SPACE(12 - LEN(ALLTRIM(laCod_Tmp[lsVldEntry,1]))) + ;
                             "   "+ALLTRIM(laDes_Tmp[lsVldEntry,1])
  SHOW GET pbRem      ENABLE
  SHOW GET pbNew      ENABLE
  SHOW GET lsVldEntry
  _CUROBJ = OBJNUM(lcDescrip)
ENDIF
*B803923,1 (Begin) Refresh check boxes.
IF EMPTY(laVldEntry)
  STORE 0 TO lnSetDefa,lnIncAll,lnDefaNo
ENDIF
IF !EMPTY(laCod_Tmp)
  SHOW GET lnSetDefa ENABLE
  SHOW GET lnIncAll  ENABLE
ELSE
  SHOW GET lnSetDefa DISABLE
  SHOW GET lnIncAll  DISABLE
ENDIF
*B803923,1 (End)

*!**************************************************************************
*!
*!      FUNCTION : lfvDescrip
*!
*!**************************************************************************
*
*** Valid function of the lcDescrip variable that hold the description. ***

FUNCTION lfvDescrip

IF !EMPTY(lcDescrip) .AND. LASTKEY() = 13
  laDes_Tmp [lsVldEntry,1] = lcDescrip
  laVldEntry[lsVldEntry,1] = ALLTRIM(laCod_Tmp[lsVldEntry,1]) + ;
                             SPACE(12 - LEN(ALLTRIM(laCod_Tmp[lsVldEntry,1]))) + ;
                             "   "+ALLTRIM(laDes_Tmp[lsVldEntry,1])
  SHOW GET pbRem      ENABLE
  SHOW GET pbNew      ENABLE
  SHOW GET lsVldEntry
  _CUROBJ = OBJNUM(lsVldEntry)
  *B803923,1 (Begin) User cannot use 'Include All' when valid entries are "Yes No".
  IF (UPPER(ALLTRIM(lcDescrip)) = 'YES' OR UPPER(ALLTRIM(lcDescrip)) = 'NO')
    lnIncAll = 0
    SHOW GET lnIncAll DISABLE
  ENDIF
  *B803923,1 (End)
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfvNew
*!
*!**************************************************************************
*
*** Valid function of the push button < NEW >. ***

FUNCTION lfvNew

IF ALEN(laCod_Tmp,1) = 1 .AND. EMPTY(laCod_Tmp[1,1]) .AND. ;
   ALEN(laDes_Tmp,1) = 1 .AND. EMPTY(laDes_Tmp[1,1])

  SHOW GET pbRem  DISABLE
  lsVldEntry = 1
ELSE
  DECLARE laVldEntry[ALEN(laVldEntry,1)+1,1]
  DECLARE laCod_Tmp [ALEN(laCod_Tmp,1) +1,1]
  DECLARE laDes_Tmp [ALEN(laDes_Tmp,1) +1,1]
  SHOW GET pbRem  ENABLE
  lsVldEntry = ALEN(laVldEntry,1)
  STORE " " TO laVldEntry[lsVldEntry,1],;
               laCod_Tmp [lsVldEntry,1],;
               laDes_Tmp [lsVldEntry,1]
ENDIF

lcCode    = SPACE(12)
lcDescrip = SPACE(30)

SHOW GET pbNew    DISABLE
SHOW GET lcCode
SHOW GET lcDescrip
SHOW GET lsVldEntry

_CUROBJ = OBJNUM(lcCode)

*!**************************************************************************
*!
*!      FUNCTION : lfvRem
*!
*!**************************************************************************
*
*** Valid function of the push button < REM >. ***

FUNCTION lfvRem

IF gfModalGen("QRM00007B00007","ALERT") = 1
  IF ALEN(laCod_Tmp,1) = 1 .AND. ALEN(laDes_Tmp,1) = 1
    laCod_Tmp [1,1] = ""
    laDes_Tmp [1,1] = ""
    laVldEntry[1,1] = ""
  ELSE
    =ADEL(laCod_Tmp,lsVldEntry)
    DECLARE laCod_Tmp[ALEN(laCod_Tmp,1)-1,1]
  
    =ADEL(laDes_Tmp,lsVldEntry)
    DECLARE laDes_Tmp[ALEN(laDes_Tmp,1)-1,1]
  
    =ADEL(laVldEntry,lsVldEntry)
    DECLARE laVldEntry[ALEN(laVldEntry,1)-1,1]
  ENDIF

  lsVldEntry = 1

  lcCode    = laCod_Tmp[lsVldEntry,1]
  lcDescrip = laDes_Tmp[lsVldEntry,1]

  SHOW GET lcCode 
  SHOW GET lcDescrip
  SHOW GET lsVldEntry
  SHOW GET pbNew      ENABLE

  IF ALEN(laCod_Tmp,1) = 1 .AND. EMPTY(laCod_Tmp[1,1]) .AND. ;
     ALEN(laDes_Tmp,1) = 1 .AND. EMPTY(laDes_Tmp[1,1])
    SHOW GET pbRem DISABLE
  ELSE
    SHOW GET pbRem ENABLE
  ENDIF
ENDIF
*B803923,1 (Begin) Refresh check boxes properly.
IF EMPTY(laVldEntry)
  STORE 0 TO lnSetDefa,lnIncAll,lnDefaNo
ENDIF
IF !EMPTY(laCod_Tmp)
  SHOW GET lnSetDefa ENABLE
  SHOW GET lnIncAll  ENABLE
ELSE
  SHOW GET lnSetDefa DISABLE
  SHOW GET lnIncAll  DISABLE
ENDIF
*B803923,1 (End)

*!**************************************************************************
*!
*!      FUNCTION : lfvOk
*!
*!**************************************************************************
*
*** Valid function of the push button < OK >. ***

FUNCTION lfvOk

IF !laScrMode[2]
  IF !EMPTY(laCod_Tmp[1,1]) .AND. !EMPTY(laDes_Tmp[1,1])

    llEmpCod    = .F.
    llEmpDes    = .F.

    lcAll_Desc  = ""
    FOR lnCount = 1 TO ALEN(laDes_Tmp,1)
      IF EMPTY(laDes_Tmp[lnCount,1])
        *** You cannot leave any empty description. ***
        *** You have to enter its value or remove this line. ***
        *** <  Ok  > ***
        = gfModalGen("TRM00123B00000","DIALOG","description")
        llEmpDes = .T.
        EXIT
      ENDIF

      lcAll_Desc = lcAll_Desc + ;
                   IIF(EMPTY(lcAll_Desc),"","|") + ;
                   ALLTRIM(laDes_Tmp[lnCount,1])
    ENDFOR

    IF llEmpDes
      _CUROBJ = OBJNUM(lsVldEntry)
      RETURN
    ENDIF

    *B803923,1 (Begin) If the usr selcted to Include All, Include it.
    IF lnIncAll = 1
      lcAll_Desc = "All|"+lcAll_Desc
    ENDIF
    *B803923,1 (End)
    
    lcAll_Code  = ""

    FOR lnCount = 1 TO ALEN(laCod_Tmp,1)
      IF EMPTY(laCod_Tmp[lnCount,1])
        *** You cannot leave any empty code. ***
        *** You have to enter its value or remove this line. ***
        *** <  Ok  > ***
        = gfModalGen("TRM00123B00000","DIALOG","code")
        llEmpCod = .T.
        EXIT
      ENDIF
      *B803923,1 (Begin) If the usr set any value as default, set it.
      *lcAll_Code = lcAll_Code + ;
                   IIF(EMPTY(lcAll_Code),"","|") + ;
                   ALLTRIM(laCod_Tmp[lnCount,1])
      lcAll_Code = lcAll_Code + ;
                   IIF(EMPTY(lcAll_Code),"","|") + ;
                   IIF(lnCount=lnDefaNo,"@","")+ALLTRIM(laCod_Tmp[lnCount,1])
      *B803923,1 (End)
    ENDFOR

    IF llEmpCod
      _CUROBJ = OBJNUM(lsVldEntry)
      RETURN
    ENDIF
    *B803923,1 (Begin) If the usr selcted to Include All, Include it.
    IF lnIncAll = 1
      lcAll_Code = "|"+lcAll_Code
    ENDIF
    *B803923,1 (End)
    IF LEN(lcAll_Code) + LEN(lcAll_Desc) + 1 > 253
      *** This codes and descriptions are exeeded the maximum ***
      *** length of the permitted valid entries. You have to ***
      *** reduce the no. of lines or reduce the lenght of the ***
      *** entered values for codes and descriptions. ***
      =gfModalGen("TRM00124B00000","DIALOG")
      _CUROBJ = OBJNUM(pbRem)
      RETURN
    ELSE
      laData[14] = lcAll_Desc + "~" + lcAll_Code
    ENDIF
  ELSE
    laData[14]   = ""
  ENDIF
ENDIF

CLEAR READ

*!**************************************************************************
*!
*!      Procedure lpSavScr
*!
*!**************************************************************************
*
PROCEDURE lpSavScr

EXTERNAL ARRAY laData,laScrMode

*B606751,1 RAE [start]
lcMsg = ''
DO CASE
  *-- If the field data type is Character.
  
  CASE laData[2] ="C"
    IF laData[3] > 254
      lcMsg = "You have exceeded the maximum allowed field width, which is 254 characters. Cannot save."
    ENDIF
  
  *-- If the field data type is Numeric.  
  CASE laData[2] ="N" 
    IF laData[3] > 20
      lcMsg = "You have exceeded the maximum allowed field width, which is 20. Cannot save."
    ENDIF    
ENDCASE

IF !EMPTY(lcMsg)
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
  _CUROBJ = OBJNUM(laData[3])
  llCSave = .F.    && Flag to say that no save happen.
  RETURN
ENDIF
*B606751,1 RAE [end]

IF laData[3] = 0     
  *** If field width = 0 stop saving ***
  =gfModalGen('TRM00012B00000','Dialog'," ")
  _CUROBJ = OBJNUM(laData[3])
  llCSave = .F.    && Flag to say that no save happen.
  RETURN
ENDIF

IF EMPTY(laData[7])
  *** You cannot leave the header description empty. ***
  *** <  Ok  > ***
  = gfModalGen("TRM00128B00000","DIALOG")
  _CUROBJ = OBJNUM(laData[7])
  llCSave = .F.    && Flag to say that no save happen.
  RETURN
ENDIF

IF laData[11] .AND. EMPTY(laData[15])
  *** You have to specify the module this field belong to. ***
  *** <  Ok  > ***
  = gfModalGen("TRM00224B00000","DIALOG")
  =lfvVld_str()
  llCSave = .F.    && Flag to say that no save happen.
  RETURN
ENDIF

IF laScrMode[4]
  APPEND BLANK
ENDIF
*E300867,1 AMM collect the memo field that hold related field in laData[13] 
*E300867,1 AMM from the array.
laData[13] = ''
FOR lnCount=1 TO ALEN(laRltFlds,1)
  laData[13]= laData[13]+IIF(LEFT(laRltFlds[lnCount],1)=lcDiamond,;
         '$'+SUBSTR(laRltFlds[lnCount],2),SUBSTR(laRltFlds[lnCount],2))+'|'

ENDFOR
laData[13]=LEFT(laData[13],LEN(laData[13])-1)
*E300867,1 AMM end

GATHER FROM laData FIELDS &lcScFields MEMO
=gfAdd_Info()

lnFldRec = RECNO()
*** Clear the previous related fields. ***
IF laScrMode[3] .AND. !EMPTY(lcOldRltd)
  DECLARE laRltField[1]
  laRltField = " "
  =gfSubStr(lcOldRltd,@laRltField,"|")
  IF !EMPTY(laRltField[1])
    FOR lnCount3 = 1 TO ALEN(laRltField,1)
      *E300867,1 AMM Remove the diamond or space from the name then seek on it.
      *IF SEEK(ALLTRIM(UPPER(laRltField[lnCount3])))
      IF SEEK(ALLTRIM(UPPER( IIF(LEFT(laRltField[lnCount3],1) $ ' ',;
              SUBSTR(laRltField[lnCount3],2),laRltField[lnCount3] ))))
      *E300867,1 AMM end
        REPLACE lRelated  WITH .F.
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*** Set flag for the new related fields. ***
IF laData[12] .AND. !EMPTY(laData[13])
  *E300867,1 AMM the array already exist, don't create it again.
  *DECLARE laRltField[1]
  *laRltField = " "
  *=gfSubStr(laData[13],@laRltField,"|")
  *IF !EMPTY(laRltField[1])
    *FOR lnCount3 = 1 TO ALEN(laRltField,1)
      *IF SEEK(ALLTRIM(UPPER(laRltField[lnCount3])))
        *REPLACE lRelated  WITH .T.
      *ENDIF
    *ENDFOR
  *ENDIF
  IF !EMPTY(laRltFlds[1])
    FOR lnCount = 1 TO ALEN(laRltFlds,1)
      IF SEEK(ALLTRIM(UPPER(SUBSTR(laRltFlds[lnCount],2))))
        REPLACE lRelated  WITH .T.
      ENDIF
    ENDFOR
  ENDIF
  *E300867,1 AMM end

ENDIF
    
IF lnFldRec > 0 .AND. lnFldRec <= RECCOUNT()
  GO lnFldRec
ENDIF

*** Add This field in the temp. file sended from file dictionary. ***
IF !EMPTY(lcAddNewFd)
  SELECT (lcAddNewFd)
  REPLACE cFld_name WITH laData[1] ;
          cFld_head WITH laData[7] ;
          nScrMode  WITH 0 ;
          lAdd_New  WITH .T.
  SELECT sydField
ENDIF

*** Adjust screen mode. ***
*** If the screen is for modifying single file. ***
IF lnScrMode = 3 .OR. lnScrMode = 4
  glQuitting = .T.
  CLEAR READ
  RETURN
ENDIF

SELECT sydField

*!**************************************************************************
*!
*!      Procedure lpDelScr
*!
*!**************************************************************************
*
PROCEDURE lpDelScr

EXTERNAL ARRAY laData,laScrMode

SELECT SYDFLFLD
lcPrevTag = SYS(22)
SET ORDER TO TAG CFLD_NAME

IF SEEK(laData[1],"SYDFLFLD")
  *** You cannot delete this field.  This field is ***
  *** involved in one or more than database.   You ***
  *** have to delete it from the database strucure ***
  *** first thrue the the file dictionary screen.  ***
  *** <  Ok  > ***
  =gfModalGen("TRM00118B00000","DIALOG")
ELSE

  IF laData[11]

    *E300643,1 Change this lines for the changes we have made to 
    *          SYCCODES [Begin]
    *SELECT SYCCODES
    *LOCATE FOR ALLTRIM(UPPER(syccodes.cfld_name)) = ALLTRIM(UPPER(laData[1]))
    *IF FOUND()
    *  *** This field has codes in the code file. ***
    *  *** You cannot delete this field. ***
    *  *** <  Ok  > ***
    *  =gfModalGen("TRM00225B00000","DIALOG")
    *  SELECT SYDFLFLD
    *  SET ORDER TO TAG &lcPrevTag
    *  SELECT SydField
    *  RETURN
    *ENDIF

    llDelCod = .F.      && Flag to know if the user is going to delete the codes
    llClosComF = .F.    && Flag to know if the SYCCOMP file was not open
    
    *E300643,1 IF Statment to check if the file SYCCOMP was not open
    *E301077,78 Hesham (Start)
    =gfOpenFile(gcSysHome+'SYCCOMP','CCOMP_ID')
    *IF !USED('SYCCOMP')
    *  llClosComF = .T.
    *  USE (gcSysHome + 'SYCCOMP') IN 0
    *ENDIF    && End of IF
    *SELECT SYCCOMP    
    *E301077,78 Hesham (End)
    

    lnCmpFRecN = RECNO()    && Varible to save the SYCCOMP record number
    lcCodFTmpN = gfTempName()    && Varible to hold a temp. name to open the CODES file with this name 

    *E300643,1 SCAN Loop to scan the SYCCOMP
    SCAN
      SELECT 0
      *E301098,1 Hesham (Start)
      *USE (ALLTRIM(SYCCOMP.cCom_DDir) + 'CODES') ORDER TAG CCODE_NO AGAIN ALIAS &lcCodFTmpN
      USE (gfGetDataDir(ALLTRIM(SYCCOMP.cCom_DDir)) + 'CODES') ORDER TAG CCODE_NO AGAIN ALIAS &lcCodFTmpN
      *E301098,1 Hesham (End)

      *E300643,1 IF Statment to check if there is m for this Field
      *E300789,1 Hesham (Start)
      *IF SEEK(SYCCOMP.cComp_Id + laData[1])
      IF SEEK('N' + laData[1])
      *E300789,1 Hesham (End)
        *E300643,1 IF Statment to check if this is the first founded code
        IF !llDelCod
          *** This field has codes in the code file. ***
          *** You cannot delete this field. ***
          *** <  Ok  > ***
          llDelCod = gfModalGen("TRM00225B00002","DIALOG") = 1
        ENDIF    && End of IF
        
        *E300643,1 IF Statment to check if the user has choose cancel
        IF !llDelCod
          SELECT SYDFLFLD
          SET ORDER TO TAG &lcPrevTag
          SELECT SydField
          RETURN
        ELSE    && Else
          
          *E300643,1 SCAN Loop to scan the codes file WHILE the company id 
          *          and the field name has not changed
          *E300789,1 Hesham (Start)
          *SCAN REST;
               WHILE cComp_Id + cFld_Name = SYCCOMP.cComp_Id + laData[1]
           SCAN REST;
               WHILE cdefcode + cFld_Name = 'N' + laData[1]
               *E300789,1 Hesham (End)

            DELETE
          ENDSCAN    && End of SCAN Loop
        ENDIF   && End of IF
      ENDIF   && End of IF
      USE
      SELECT SYCCOMP
    ENDSCAN    && End of SCAN Loop
    
    *E300643,1 IF Statment to check if the file SYCCOMP was not open
    *E301077,78 Hesham (Start)
    *IF llClosComF
    *  USE
    *ELSE    && Else
    *  IF lnCmpFRecN <= RECCOUNT()
    *    GO lnCmpFRecN
    *  ENDIF
    *ENDIF    && End of IF
    *E301077,78 Hesham (End)
    *E300643,1 Change this lines [End]

  ENDIF

  *** Delete the field from the database. ***
  SELECT SYDFIELD
  SCATTER MEMVAR MEMO BLANK
  GATHER  MEMVAR MEMO 
  DELETE
  
  *** If the field was a related field for any other fields. ***
  DECLARE laAdjRlt[1,2]
  SELECT cFld_name,mRltFields ;
         FROM (gcSysHome+"sydField") ;
         WHERE lRltFields ;
         INTO ARRAY laAdjRlt
  IF _TALLY > 0 .AND. !EMPTY(laAdjRlt[1,1])
    SELECT sydField
    FOR lnCount = 1 TO ALEN(laAdjRlt,1)
      lnExist = AT(RTRIM(laData[1]),laAdjRlt[lnCount,2])
      IF lnExist > 0
        laAdjRlt[lnCount,2]=STRTRAN(laAdjRlt[lnCount,2],RTRIM(laData[1]),"")
        DO CASE
          CASE LEFT(laAdjRlt[lnCount,2],1) = "|"
            laAdjRlt[lnCount,2] = SUBSTR(laAdjRlt[lnCount,2],2)
          CASE RIGHT(laAdjRlt[lnCount,2],1) = "|"
            laAdjRlt[lnCount,2] = SUBSTR(laAdjRlt[lnCount,2],1,LEN(laAdjRlt[lnCount,2])-1)
          CASE AT("||",laAdjRlt[lnCount,2]) > 0
            laAdjRlt[lnCount,2] = STRTRAN(laAdjRlt[lnCount,2],"||","|")
        ENDCASE
        IF SEEK(laAdjRlt[lnCount,1])
          REPLACE mRltFields WITH laAdjRlt[lnCount,2];
                  lRltFields WITH IIF(EMPTY(laAdjRlt[lnCount,2]),.F.,.T.)
        ENDIF
      ENDIF
    ENDFOR
  ENDIF

  *** Back to the select mode. ***
  laScrMode    = .F.
  lascrMode[1] = .T.
ENDIF

SELECT SYDFLFLD
SET ORDER TO TAG &lcPrevTag
SELECT SydField

*!**************************************************************************
*!
*!      Procedure lpClsScr
*!
*!**************************************************************************
*
PROCEDURE lpClsScr

EXTERNAL ARRAY laData,laScrMode
*** If the screen is for modifying single file. ***
IF lnScrMode = 4
  glQuitting = .T.
  CLEAR READ
  RETURN
ENDIF

IF laScrMode[3] .OR. lnScrMode = 3
  SCATTER FIELDS &lcScFields MEMO TO laData
ENDIF  

*!**************************************************************************
*!
*!      Procedure lfvData_12
*!
*!**************************************************************************
*
FUNCTION lfvData_12

DECLARE laAllField[1,1],laRltField[1]
STORE " " TO laAllField,laRltField

SELECT RTRIM(cFld_name)+SPACE(11-LEN(RTRIM(cFld_name)))+ ;
       IIF(!EMPTY(cFld_head), ;
       RTRIM(cFld_head),RTRIM(cFld_name)+ ;
       SPACE(25-LEN(RTRIM(cFld_name)))) ;
       FROM  (gcSysHome+"sydField") ;
       WHERE cFld_name <> laData[1] ;
             .AND. !lvldentry ;
       INTO  ARRAY laAllField ;
       ORDER BY cFld_name 

IF !EMPTY(laData[13])
  *E300867,1 AMM start, Copy related fields array without the diamond sign.
  *=gfSubStr(laData[13],@laRltField,"|")
  =ACOPY(laRltFlds,laRltField)
  FOR lnCount=1 TO ALEN(laRltField,1)
    laRltField[lnCount] = SUBSTR(laRltField[lnCount],2)
  ENDFOR
  *E300867,1 AMM end
ENDIF  

IF !EMPTY(laRltField[1])
  SELECT SYDFIELD
  lnRecFld = RECNO()
  
  FOR lnCount1 = 1 TO ALEN(laRltField,1)
    laRltField[lnCount1] = ALLTRIM(laRltField[lnCount1]) + ;
                           SPACE(10-LEN(ALLTRIM(laRltField[lnCount1])))
    IF SEEK(laRltField[lnCount1],"SYDFIELD")
      laRltField[lnCount1] = laRltField[lnCount1] + " " + ;
                             IIF(!EMPTY(sydField.cFld_head), ;
                             ALLTRIM(sydField.cFld_head), ;
                             ALLTRIM(sydField.cFld_name) + ;
                             SPACE(25-LEN(ALLTRIM(sydField.cFld_name))))
    ENDIF
  ENDFOR
  
  IF lnRecFld > 0 .AND. lnRecFld <= RECCOUNT()
    GO lnRecFld
  ENDIF
ENDIF

=gfMover(@laAllField,@laRltField,"Select related fields")

IF !EMPTY(laRltField[1])
  FOR lnCount  = 1 TO ALEN(laRltField,1)
    *E300867,1 AMM Comment out
    *laData[13] = laData[13] + IIF(EMPTY(laData[13]),'','|')+;
                 ALLTRIM(SUBSTR(laRltField[lnCount],1,10))
    *E300867,1 AMM, compare each element in the new array with the old one, 
    *E300867,1 AMM if it is exist get it with its diamond status, else put  
    *E300867,1 AMM a space instead.
    lnNum = ASCAN(laRltFlds,SPACE(1)+LEFT(laRltField[lnCount],10)) 
    IF lnNum = 0
      lnNum = ASCAN(laRltFlds,lcDiamond+LEFT(laRltField[lnCount],10))
    ENDIF
    IF lnNum = 0
      laRltField[lnCount] = SPACE(1) + LEFT(laRltField[lnCount],10)
    ELSE
      laRltField[lnCount] = laRltFlds[lnNum]
    ENDIF
    *E300867,1 AMM end
  ENDFOR
  *E300867,1 AMM copy the array to refresh the screen
  DIMENSION laRltFlds[1]
  =ACOPY(laRltField,laRltFlds)
  *E300867,1 AMM end
  laData[12] = .T.
ELSE
  *E300867,1 AMM comment , no need to it.
  *laData[13] = ' '
  *E300867,1 AMM Empty the related field array
  laRltFlds = ''
  *E300867,1 AMM end
  laData[12] = .F.
ENDIF    
SHOW GET laData[12]
*E300867,1 AMM Comment out
*SHOW GET laData[13]
SHOW GET lnRltFlds
*E300867,1 AMM end

=gfUpdate()

*!*************************************************************
*! Name      : lfvMand
*! Developer : Ahmed Mohammed
*! Date      : 05/06/1998
*! Purpose   : Valid function of the mandatory check box
*! REF       : *E300867,1
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvMand()
*!*************************************************************
FUNCTION lfvMand
IF lnMand = 1
  laRltFlds[lnRltFlds] = lcDiamond + SUBSTR(laRltFlds[lnRltFlds],2)
ELSE
  laRltFlds[lnRltFlds] = SPACE(1)  + SUBSTR(laRltFlds[lnRltFlds],2)
ENDIF
SHOW GET lnRltFlds

*!*************************************************************
*! Name      : lfwRltFlds
*! Developer : Ahmed Mohammed
*! Date      : 05/06/1998
*! Purpose   : When function of the related fields list box, to 
*!             update the mandatory check box
*! REF       : *E300867,1
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfwRltFlds()
*!*************************************************************

FUNCTION lfwRltFlds
IF lcDiamond $ laRltFlds[lnRltFlds]
  lnMand = 1
ELSE
  lnMand = 0
ENDIF
SHOW GET lnMand