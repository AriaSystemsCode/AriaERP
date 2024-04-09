*:************************************************************************
*:
*: Procedure file: SMPRVL.PRG 
*:                 (Privillage)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*E300709,1 Hesham 07/27/97
*E300709,1 make the program can add priv. for the main system
*B802188,1 RENEE 05/10/99 Alias not found when clicking any option on the menu 
*B802188,1 				  options list in the user priv. screen
*B602998,1 MAN   06/20/99 Fix the security to allow access per 
*B602998,1 MAN            Module + Process
*B603422,1 RAMY  05/02/2000 Fix the module id selection bug in case the user select Modules 
*B603422,1                  more than one time
*B804169,1 AME   06/20/2001 fix the bug of no access if only upc module installed
*B605621,1 WAB   03/11/2002 fix the bug of user group privilige doe not get copied
*B119794,1 ASH   09/25/2003 Fix the bug of user who has access on PO screen cannot edit it from generate PO from SO screen.
*B127769,1 TMI 06/12/2005 Allow to refresh buttons in View mode of the SMPRVL.scx screen
*:************************************************************************
PARAMETERS lcGrpOrUsr,lcGrpUsrID,lcGrpUsrNM

*** Check if there are no companies or modules setup then give message
*** and do not allow them to go to the privileges screen
***
llOk       = .T.
lcCmporMdl =  ''
lnPop      = 1



*B804169,1 AME [Start] Variable to hold kid module
lcKid = ''
*B804169,1 AME [End]

*B802188,1 Use SYDOBJCT
=gfOpenFile(gcSysHome+'SYDOBJCT','CAPP_ID','SH')
*B802188,1 end

SELECT SYDAPPL
LOCATE FOR cApp_ID <> 'SY'
IF !FOUND()
  llOk       = .F.
  lcCmporMdl = 'Modules'
ENDIF

SELECT SYCCOMP
LOCATE
IF !FOUND()
  lcCmporMdl = IIF(!llOk, lcCmporMdl+' Companies', 'Companies')
  llOk       = .F.
ENDIF

IF !llok
  *** No module/company have not been setup. ***
  *** Privileges cannot be assigned.         ***
  =gfModalGen("TRM00188B00000","DIALOG",LOWER(lcCmporMdl))
  RETURN
ENDIF

DECLARE laApp[1,2],laComp[1,2],laUsers[1],laSubProc[1,2],laMainSub[1,2]

llDontHid  = .T.
llFirstm   = .T.

lnAryNo    = 1
lnBarLevel = 1
lnCurLevel = 1
lnFirstBar = 1
lsMenu     = 1

lcApp_Id   = " "
lcApp_Desc = " "
laApp      = " "

lcCom_Id   = " "
lcCom_Desc = " "
laComp     = " "

lnRecNo    = 0
lcRecNo    = ""
lcAddDmnd  = CHR(255)
lcAllPrmp  = "\<All "
lcSubStat  = "DISABLE"

STORE .F. TO cbAdd,llOldAdd,cbEdit,llOldEdit,cbDel,llOldDel

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
    lcDiamond  = ""
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
    lcDiamond  = ""
    lcHfBlocs  = REPLICATE(lcHfBloc,30)
    ibPopApp   = 1
    ibPopComp  = 1
ENDCASE

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

*** Get the available application in the system. ***
*E300709,1 Hesham (Start)
*E300709,1 select the main system with the module
*E300709,1 so we can make the program add priv. for 
*E300709,1 the main system
*SELECT cApp_Name,cApp_Id ;
       FROM (gcSysHome+"sydAppl") ;
       INTO ARRAY laApp ;
       WHERE CAPP_ID <> "SY" ;
       ORDER BY cApp_Name

SELECT cApp_Name,cApp_Id ;
       FROM (gcSysHome+"sydAppl") ;
       INTO ARRAY laApp ;
       ORDER BY cApp_Name

*E300709,1 Hesham (End)
IF _TALLY = 0
  STORE " " TO lcApp_Id,lcApp_Desc,laApp
ELSE
  *** Default the application ID to the first ***
  *** element of the application array...
  *B804169,1 AME [Start] In case of existing of parent module
  *B804169,1 AME         make it the search module and save the choosed
  *B804169,1 AME         module as kid module
  *lcApp_Id = laApp[1,2]
  =SEEK(laApp[1,2],'SydAppl')
  IF !EMPTY(SydAppl.CModParent)
    lcApp_Id = SydAppl.CModParent
    lcKid = laApp[1,2]
  ELSE
     lcApp_Id = laApp[1,2]
  ENDIF
  *B804169,1 AME [End]

  DO CASE
    CASE _DOS
      lcApp_Desc = laApp[1,1]
    CASE _WINDOWS
      ibPopApp   = 1
  ENDCASE
ENDIF

*** Get the available companies in the system. ***
SELECT ccomp_id+"-"+ccom_name,ccomp_id ;
       FROM (gcSysHome+"syccomp") ;
       INTO ARRAY laComp ;
       ORDER BY ccom_name

IF _TALLY = 0
  STORE " " TO lcCom_Id,lcCom_Desc,laComp
ELSE
  *** Default the company ID to the first ***
  *** element of the company array...
  lcCom_Id   = laComp[1,2]
  DO CASE
    CASE _DOS
      lcCom_Desc = laComp[1,1]
    CASE _WINDOWS
      ibPopComp   = 1
  ENDCASE
ENDIF

*** Get all the available users in the system except the current user. ***
laUsers = ''
SELECT SUBSTR(cUser_ID+"-"+cUsr_Name,1,41)+"³ USER " ;
       FROM (gcSysHome+"syuUser") ;
       WHERE !EMPTY(cUser_ID) ;
       .AND. cUser_ID <> lcGrpUsrID ;
       .AND. cUsr_Levl <> "A" ;
UNION ;
SELECT SUBSTR(cGroup_ID+"      -"+cGroup_Nam,1,41)+"³ GROUP" ;
       FROM (gcSysHome+"syuGroup") ;
       WHERE !EMPTY(cGroup_ID) ;
       .AND. cGroup_ID <> lcGrpUsrID ;
       INTO ARRAY laUsers

*** Set relation between the menu file and the ***
*** user privilege temp. file...
SELECT SYCMENU
SET ORDER TO TAG APPPOPBAR
SET RELATION TO
SET RELATION TO ALLTRIM(SYCMENU.cApp_Id) + ALLTRIM(SYCMENU.cPross_Id) + ;
                SYCMENU.CPROCTYPE + ALLTRIM(lcCom_Id) INTO &lcPrv_Tmp2

*** Define the list from popup before calling the SPR...
DO CASE
  CASE _DOS
    DEFINE POPUP puMenu RELATIVE SCROLL	MARK lcHfBloc COLOR SCHEME 24
    DEFINE BAR 1 OF puMenu PROMPT " "
  CASE _WINDOWS
    DEFINE POPUP puMenu RELATIVE SCROLL	MARK lcHfBloc
    DEFINE BAR 1 OF puMenu PROMPT " "
ENDCASE

*** Build the popup from the master menu ***
*** file according to the first module & ***
*** the first company...

=lfBldBars()

*E300683,4 Call *.SPR from screens directory
* DO SMPRVL.SPR 
DO (gcScrDir + gcWinAppl + '\SMPRVL.SPR')
*E300683,4 end   
RELEASE POPUP puMenu

SELECT SYCMENU
SET RELATION TO

RETURN 

*!**************************************************************************
*!
*!      Function: lfShow
*!
*!**************************************************************************
*
*** Show function. ***

FUNCTION lfShow

IF laScrMode[2]
  SHOW GET pbCopy    DISABLE
  SHOW GET pbAll_Non DISABLE
  SHOW GET cbAdd     DISABLE
  SHOW GET cbEdit    DISABLE
  SHOW GET cbDel     DISABLE
  SHOW GET pbSubProc DISABLE
  SHOW GET pbSelAll  DISABLE
  SHOW GET pbSelNon  DISABLE
  *B127769,1  TMI [Start] Enable the cancel button in the Edit mode 
  *SHOW GET pbCan     DISABLE
  SHOW GET pbCan     ENABLE
  *B127769,1  TMI [End  ] 
  
  SHOW GET ibPopApp  ENABLE
  SHOW GET ibPopComp ENABLE
  SHOW GET lsMenu    ENABLE
  *B127769,1  TMI [Start] Disable the Ok button in the Edit mode
  *SHOW GET pbOk      ENABLE
  SHOW GET pbOk      DISABLE
  *B127769,1  TMI [End  ] 
ELSE
  =lfwMenu()
ENDIF

*!**************************************************************************
*!
*!      Function: lfvCopy
*!
*!**************************************************************************
*

FUNCTION lfvCopy

lsUsers  = 1

llChange = .F.

IF EMPTY(laUsers[1])
  *** No users/groups available. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00189B00000","DIALOG","users/groups")
ELSE
  *E300683,4 Call *.SPR from screens directory
  * DO SMCPUSR.SPR 
  DO (gcScrDir + gcWinAppl + '\SMCPUSR.SPR')
  *E300683,4 end  
  IF llChange
    =lfBldBars()       && Build the popup for the new changes.
  ENDIF
ENDIF


*!**************************************************************************
*!
*!      Function: lfvUsers
*!
*!**************************************************************************
*

FUNCTION lfvUsers

lcUsr2Copy = ALLTRIM(SUBSTR(laUsers[lsUsers],1,10))
*B605621,1 WAB (Start) - we have to check if the program called from User screen or Group screen 
*B605621,1 WAB         - then replace in the filed cGrpouser 'G' OR 'U' ( user or Group )
*SELECT SYUUSRPR
*lcSelFlds = "lcGrpUsrID AS cUser_id,cApp_id,cComp_id,"+;
*            "cPross_id,cProctype,'U' AS cGrporuser,"+;
*            "lAddrec,lEditrec,lDelerec,lSubproc,mSubproc"
llFrmGrp =  (UPPER(lcGrpOrUsr) == 'GROUP')
SELECT SYUUSRPR
lcSelFlds = "lcGrpUsrID AS cUser_id,cApp_id,cComp_id,"+;
            "cPross_id,cProctype,IIF(llFrmGrp,'G','U') AS cGrporuser,"+;
            "lAddrec,lEditrec,lDelerec,lSubproc,mSubproc"
*B605621,1 WAB (End)

SELECT &lcSelFlds,'M' AS CSTATUS,RECNO() AS NRECNO,.T. AS LEXIST;
       FROM (gcSysHome+"SYUUSRPR") ;
       WHERE cuser_id=lcGrpUsrID AND ;
             capp_id+ccomp_id+cpross_id+cproctype;
   IN (SELECT capp_id+ccomp_id+cpross_id+cproctype;
               FROM (gcSysHome+"SYUUSRPR");
               WHERE cuser_id=lcUsr2Copy) UNION ;
   SELECT &lcSelFlds,'D' AS CSTATUS,RECNO() AS NRECNO,.T. AS LEXIST;
          FROM (gcSysHome+"SYUUSRPR");
          WHERE cuser_id=lcGrpUsrID AND ;
                capp_id+ccomp_id+cpross_id+cproctype;
   NOT IN (SELECT capp_id+ccomp_id+cpross_id+cproctype;
                  FROM (gcSysHome+"SYUUSRPR");
                  WHERE cuser_id=lcUsr2Copy) UNION ;
   SELECT &lcSelFlds,'A' AS CSTATUS,0 AS NRECNO,.F. AS LEXIST;
          FROM (gcSysHome+"SYUUSRPR");
          WHERE cuser_id=lcUsr2Copy AND ;
                capp_id+ccomp_id+cpross_id+cproctype ;
   NOT IN (SELECT capp_id+ccomp_id+cpross_id+cproctype;
                  FROM (gcSysHome+"SYUUSRPR");
                  WHERE cuser_id=lcGrpUsrID) ;
   INTO DBF (gcWorkDir+lcCopUsTmp)

SELECT (lcPrv_Tmp2)
ZAP
APPEND FROM (gcWorkDir+lcCopUsTmp)

IF UPPER(LEFT(lcGrpOrUsr,1)) = "U" .AND. !EMPTY(lcUsr_grup)
  SELECT * FROM (gcSysHome+"SYUUSRPR") ;
         WHERE cuser_id = lcUsr_grup ;
         INTO DBF (gcWorkDir+lcGrp_Tmp)
  SELECT (lcGrp_Tmp)
  SCAN
    SELECT (lcPrv_Tmp2)
    IF SEEK(ALLTRIM(&lcGrp_Tmp..cApp_Id)+ALLTRIM(&lcGrp_Tmp..cPross_Id)+ALLTRIM(&lcGrp_Tmp..cProcType)+ALLTRIM(&lcGrp_Tmp..cComp_Id))
      REPLACE cGrporuser WITH "G" ;
              cStatus    WITH "S"
      SELECT (lcGrp_Tmp)
      DELETE
    ENDIF
    SELECT (lcGrp_Tmp)
  ENDSCAN
ENDIF

llChange = .T.

*!**************************************************************************
*!
*!      Function: lfvAppId
*!
*!**************************************************************************
*
*** Valid function of the popup hold all the ***
*** available modules in the system...

FUNCTION lfvAppId


*B804169,1 AME [Start] Variable to hold kid module
lcKid = ''
*B804169,1 AME [End]




*** Save the old application. ***
lcOldApp   = lcApp_Id
lcOldApNam = lcApp_Desc

DO CASE
  CASE _DOS
    lcApp_Id = gfActPop(3,22,9,69,'laApp',2,1,@lcApp_Desc)
    SHOW GETS
  CASE _WINDOWS
    *B804169,1 AME [Start] In case of existing of parent module
    *B804169,1 AME         make it the search module and save the choosed
    *B804169,1 AME         module as kid module
    *lcApp_Id = laApp[ibPopApp,2]
    =SEEK(laApp[ibPopApp,2],'SydAppl')
    IF !EMPTY(SydAppl.CModParent)
      lcApp_Id = SydAppl.CModParent
      lcKid = laApp[ibPopApp,2]
    ELSE
      lcApp_Id = laApp[ibPopApp,2]
    ENDIF
    *B804169,1 AME [End]
    SHOW GET ibPopApp
ENDCASE

*** If the module is changed. ***
DO CASE
  CASE _DOS
    IF lcOldApp <> lcApp_Id
      IF !lfBldBars()              && Build the popup for the current module data.
        lcApp_Id   = lcOldApp  
        lcApp_Desc = lcOldApNam 
        SHOW GETS
      ENDIF
    ENDIF
  CASE _WINDOWS
*    WAIT str(lnPop)+str(ibPopApp) window
    IF lnPop <> ibPopApp
*      wait "1" window
      IF !lfBldBars()              && Build the popup for the current module data.
*        wait "2" window
        ibPopApp   = lnPop
        SHOW GET ibPopApp
      *B603422,1 RAMY Add the 'else' to update the 'lnPop' variable with the new 
      *B603422,1      selected value [start]
      ELSE
        lnPop = ibPopApp
      *B603422,1 RAMY [end]
      ENDIF
    ENDIF
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvCompId
*!
*!**************************************************************************
*
*** Valid function of the popup hold all the ***
*** available companies in the system...

FUNCTION lfvCompId

*** Save the old application. ***
lcOldComp = lcCom_Id

DO CASE
  CASE _DOS
    lcCom_Id = gfActPop(5,22,11,52,'laComp',2,1,@lcCom_Desc)
    SHOW GETS
  CASE _WINDOWS
    lcCom_Id = laComp[ibPopComp,2]
    SHOW GET ibPopComp
ENDCASE

*** If the company is changed. ***
IF lcOldComp <> lcCom_Id
  =lfBldBars()              && Build the popup for the current module data.
ENDIF

*!**************************************************************************
*!
*!      Function: lfwMenu
*!
*!**************************************************************************
*
*** When function of the list lsMenu. ***

FUNCTION lfwMenu

*B127769,1  TMI [Start] Allow to refresh fields in edit mode
*IF lsMenu = 0 .OR. laScrMode[2]
IF lsMenu = 0
  *B127769,1  TMI [End  ] 
  RETURN
ENDIF

lcPrompt = PRMBAR("puMenu",GETBAR("puMenu",lsMenu))

IF AT("+",lcPrompt) > 0
  lnRecNo = INT(VAL(SUBSTR(lcPrompt,AT("+",lcPrompt))))
  IF lnRecNo > 0 .AND. lnRecNo <= RECCOUNT(lcPrv_Tmp2) .AND. AT(lcDiamond,lcPrompt) > 0
    GO lnRecNo IN &lcPrv_Tmp2
    cbAdd  = &lcPrv_Tmp2..lAddRec
    cbEdit = &lcPrv_Tmp2..lEditRec
    cbDel  = &lcPrv_Tmp2..lDeleRec
    lcAllPrmp = IIF(cbAdd .AND. cbEdit .AND. cbDel , "\<None" , "\<All " )

    lcPross_Id = IIF(AT("*",lcPrompt) > 0,;
                     ALLTRIM(SUBSTR(lcPrompt,AT("*",lcPrompt)+1,10)),"")
    lcProcType = IIF(AT("@",lcPrompt) > 0,;
                     SUBSTR(lcPrompt,AT("@",lcPrompt)+1,1),"")
    
    IF lcProcType = "P"
      *B602998,1 MAN   06/20/99 Fix the security to allow access per 
      *B602998,1 MAN            Module + Process Start
      PRIVATE lcSvOrd
      SELECT SYDOBJCT
      lcSvOrd = ORDER()
      SET ORDER TO TAG capobjnam IN sydObjct
      IF SEEK(PADR(ALLTRIM(lcPross_Id),10),"sydObjct")
      *IF SEEK(lcApp_Id+lcPross_Id,"sydObjct")
      *B602998,1 MAN End
        lcSubStat = IIF(sydObjct.lSubProc,"ENABLE","DISABLE")
        lcAllStat = IIF(SUBSTR(sydObjct.cBaseWind,3,1) <> "R","DISABLE","ENABLE")
      ELSE
        lcSubStat = "DISABLE"
        lcAllStat = "DISABLE"
      ENDIF
      *B602998,1 MAN   06/20/99 Fix the security to allow access per 
      *B602998,1 MAN            Module + Process Start
      SET ORDER TO TAG (lcSvOrd) IN sydObjct
      *B602998,1 MAN End
    ELSE
      lcSubStat = "DISABLE"
      lcAllStat = "DISABLE"
    ENDIF
  ELSE
    cbAdd  = .F.
    cbEdit = .F.
    cbDel  = .F.
    lcAllPrmp = "\<All "
    lcSubStat = "DISABLE"
    lcAllStat = "DISABLE"
  ENDIF
ELSE
  cbAdd  = .F.
  cbEdit = .F.
  cbDel  = .F.
  lcAllPrmp = "\<All "
  lcSubStat = "DISABLE"
  lcAllStat = "DISABLE"
ENDIF

SHOW GET pbAll_Non,1 PROMPT lcAllPrmp &lcAllStat
SHOW GET cbAdd     &lcAllStat
SHOW GET cbEdit    &lcAllStat
SHOW GET cbDel     &lcAllStat
SHOW GET pbSubProc &lcSubStat

*B127769,1  TMI [Start] Disable privilage buttons in view mode
IF laScrMode[2]
  SHOW GET cbAdd DISABLE
  SHOW GET cbEdit DISABLE
  SHOW GET cbDel DISABLE
  SHOW GET pbAll_Non DISABLE
ENDIF
*B127769,1  TMI [End  ] 

*!**************************************************************************
*!
*!      Function: lfvMenu
*!
*!**************************************************************************
*
FUNCTION lfvMenu

IF laScrMode[2]
  RETURN
ENDIF

lcPrompt = PRMBAR("puMenu",GETBAR("puMenu",lsMenu))

IF AT("+",lcPrompt) <> 0
  lnRecNo = INT(VAL(SUBSTR(lcPrompt,AT("+",lcPrompt))))
  IF lnRecNo > 0 .AND. lnRecNo <= RECCOUNT(lcPrv_Tmp2)
    GO lnRecNo IN &lcPrv_Tmp2
    IF UPPER(LEFT(lcGrpOrUsr,1)) <> "G"
      IF &lcPrv_Tmp2..cGrpORUser <> "U"
        *** You cannot edit this bar because it ***
        *** is a group access not a user access ***
        *** <  Ok  > ***
        =gfModalGen("TRM00152B00000","DIALOG")
        _CUROBJ = _CUROBJ
        RETURN
      ENDIF
    ENDIF
  ENDIF
ELSE
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

lcPross_Id = IIF(AT("*",lcPrompt) > 0,;
                 ALLTRIM(SUBSTR(lcPrompt,AT("*",lcPrompt)+1,10)),"")

IF EMPTY(lcPross_Id)
  *** You have to link this menu bar to  ***
  *** a system program before giving its ***
  *** right to any user or group. ***
  =gfModalGen("TRM00156B00000","DIALOG",LOWER(lcGrpORUsr))
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

lcProcType = IIF(AT("@",lcPrompt) > 0,;
                 SUBSTR(lcPrompt,AT("@",lcPrompt)+1,1),"")

DO CASE
  CASE AT(lcDiamond,lcPrompt) > 0
    SELECT (lcPrv_Tmp2)
    GO lnRecNo IN &lcPrv_Tmp2
    IF 	&lcPrv_Tmp2..cStatus='A'
      DELETE 
    ELSE  
      lcStatus = IIF(&lcPrv_Tmp2..lExist,"D","S")
      REPLACE &lcPrv_Tmp2..cStatus WITH lcStatus
    ENDIF  
    
    lcPrompt = STRTRAN(lcPrompt,lcDiamond,CHR(255))
    lnPos    = IIF(AT("$",lcPrompt) > 0,;
                   INT(VAL(SUBSTR(lcPrompt,AT("$",lcPrompt)+1,3))),0)
    lcPrompt = IIF(lnPos > 0 , STUFF(lcPrompt,lnPos,0,"\<") , lcPrompt)
    
    DEFINE BAR GETBAR("puMenu",lsMenu) OF puMenu PROMPT lcPrompt
    
  CASE AT(CHR(255),lcPrompt) > 0
    SELECT (lcPrv_Tmp2)
    *E300709,1 Hesham (Start)
    *E300709,1 if the selected module is the main system and
    *E300709,1 the bar type is not a program the return
    IF ALLTRIM(lcApp_Id)='SY' AND ALLTRIM(lcProcType) <>'P'
      RETURN
    ENDIF
    *E300709,1 Hesham (End)
    IF SEEK(ALLTRIM(lcApp_Id)+ALLTRIM(lcPross_Id)+ALLTRIM(lcProcType)+ALLTRIM(lcCom_Id))
      lcStatus = IIF(&lcPrv_Tmp2..lExist,"M","A")
      REPLACE &lcPrv_Tmp2..lAddRec  WITH .T.;
              &lcPrv_Tmp2..lEditRec WITH .T.;
              &lcPrv_Tmp2..lDeleRec WITH .T.;
              &lcPrv_Tmp2..cStatus WITH lcStatus
      
      lcPrompt = STRTRAN(lcPrompt,CHR(255),lcDiamond)
      lnPos    = IIF(AT("$",lcPrompt) > 0,;
                     INT(VAL(SUBSTR(lcPrompt,AT("$",lcPrompt)+1,3))),0)
      lcPrompt = IIF(lnPos > 0 , STUFF(lcPrompt,lnPos,0,"\<") , lcPrompt)

      DEFINE BAR GETBAR("puMenu",lsMenu) OF puMenu PROMPT lcPrompt
    ELSE
      APPEND BLANK
      REPLACE cuser_id   WITH lcGrpUsrID ;
              capp_id    WITH lcApp_Id ;
              ccomp_id   WITH lcCom_Id ;
              cpross_id  WITH lcPross_Id ;
              cProcType  WITH lcProcType ;
              cgrporuser WITH UPPER(LEFT(lcGrpOrUsr,1)) ;
              lAddRec    WITH .T.;
              lEditRec   WITH .T.;
              lDeleRec   WITH .T.;
              cStatus    WITH "A" ;
              nRecno     WITH RECNO(lcPrv_Tmp2) ;
              cadd_user  WITH gcUser_ID ;
              dadd_date  WITH DATE() ;
              cadd_time  WITH gfGetTime()
              
      lcRecNo  = ALLTRIM(STR(RECNO(lcPrv_Tmp2)))
      lcPrompt = STRTRAN(lcPrompt,CHR(255),lcDiamond)
      lcPrompt = SUBSTR(lcPrompt,1,AT("+",lcPrompt)) + lcRecNo
      lnPos    = IIF(AT("$",lcPrompt) > 0,;
                     INT(VAL(SUBSTR(lcPrompt,AT("$",lcPrompt)+1,3))),0)
      lcPrompt = IIF(lnPos > 0 , STUFF(lcPrompt,lnPos,0,"\<") , lcPrompt)

      DEFINE BAR GETBAR("puMenu",lsMenu) OF puMenu PROMPT lcPrompt
      
      *B119794,1 ASH 09/25/2003 (Begin) Add 'POSTYLE' to the user privelages to allow the user to edit the PO screen if opened from generate PO from SO screen.
      IF lcPross_Id='POSTY'
        SCATTER MEMVAR MEMO
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE cPross_Id WITH 'POSTYLE'
      ENDIF
      *B119794,1 ASH 09/25/2003 (End)
    ENDIF
ENDCASE

_CUROBJ = _CUROBJ

*!**************************************************************************
*!
*!      Function: lfvAll
*!
*!**************************************************************************
*
FUNCTION lfvAll

lnRecNo = lfUsrORGrp()
IF lnRecNo = 0 .OR. lnRecNo > RECCOUNT(lcPrv_Tmp2)
  RETURN
ENDIF

SELECT (lcPrv_Tmp2)
GO lnRecNo IN &lcPrv_Tmp2

lcStatus = SUBSTR("MAM",AT(&lcPrv_Tmp2..cStatus,"MAS"),1)

IF lcAllPrmp  = "\<All "
  REPLACE &lcPrv_Tmp2..lAddRec  WITH .T.;
          &lcPrv_Tmp2..lEditRec WITH .T.;
          &lcPrv_Tmp2..lDeleRec WITH .T.;
          &lcPrv_Tmp2..cStatus  WITH lcStatus
  
  cbAdd  = .T.
  cbEdit = .T.
  cbDel  = .T.
  lcAllPrmp  = "\<None"
ELSE
  REPLACE &lcPrv_Tmp2..lAddRec  WITH .F.;
          &lcPrv_Tmp2..lEditRec WITH .F.;
          &lcPrv_Tmp2..lDeleRec WITH .F.;
          &lcPrv_Tmp2..cStatus  WITH lcStatus
  
  cbAdd  = .F.
  cbEdit = .F.
  cbDel  = .F.
  lcAllPrmp  = "\<All "
ENDIF

SHOW GET pbAll_Non,1 PROMPT lcAllPrmp ENABLE
SHOW GET cbAdd
SHOW GET cbEdit
SHOW GET cbDel

*!**************************************************************************
*!
*!      Function: lfvAdd
*!
*!**************************************************************************
*

FUNCTION lfvAdd

lnRecNo = lfUsrORGrp()
IF lnRecNo = 0 .OR. lnRecNo > RECCOUNT(lcPrv_Tmp2)
  cbAdd = llOldAdd
  SHOW GET cbAdd
  RETURN
ENDIF

SELECT (lcPrv_Tmp2)
GO lnRecNo IN &lcPrv_Tmp2

lcStatus = SUBSTR("MAM",AT(&lcPrv_Tmp2..cStatus,"MAS"),1)

REPLACE &lcPrv_Tmp2..lAddRec  WITH cbAdd;
        &lcPrv_Tmp2..cStatus  WITH lcStatus

lcAllPrmp = IIF(cbAdd .AND. cbEdit .AND. cbDel,"\<None","\<All ")
SHOW GET pbAll_Non,1 PROMPT lcAllPrmp ENABLE

*!**************************************************************************
*!
*!      Function: lfvEdit
*!
*!**************************************************************************
*
FUNCTION lfvEdit

lnRecNo = lfUsrORGrp()
IF lnRecNo = 0 .OR. lnRecNo > RECCOUNT(lcPrv_Tmp2)
  cbEdit = llOldEdit
  SHOW GET cbEdit
  RETURN
ENDIF

SELECT (lcPrv_Tmp2)
GO lnRecNo IN &lcPrv_Tmp2

lcStatus = SUBSTR("MAM",AT(&lcPrv_Tmp2..cStatus,"MAS"),1)

REPLACE &lcPrv_Tmp2..lEditRec WITH cbEdit;
        &lcPrv_Tmp2..cStatus WITH lcStatus

lcAllPrmp = IIF(cbAdd .AND. cbEdit .AND. cbDel,"\<None","\<All ")
SHOW GET pbAll_Non,1 PROMPT lcAllPrmp ENABLE

*!**************************************************************************
*!
*!      Function: lfvDel
*!
*!**************************************************************************
*

FUNCTION lfvDel

lnRecNo = lfUsrORGrp()
IF lnRecNo = 0 .OR. lnRecNo > RECCOUNT(lcPrv_Tmp2)
  cbDel = llOldDel
  SHOW GET cbDel
  RETURN
ENDIF

SELECT (lcPrv_Tmp2)
GO lnRecNo IN &lcPrv_Tmp2

lcStatus = SUBSTR("MAM",AT(&lcPrv_Tmp2..cStatus,"MAS"),1)

REPLACE &lcPrv_Tmp2..lDeleRec WITH cbDel;
        &lcPrv_Tmp2..cStatus WITH lcStatus

lcAllPrmp = IIF(cbAdd .AND. cbEdit .AND. cbDel,"\<None","\<All ")
SHOW GET pbAll_Non,1 PROMPT lcAllPrmp ENABLE

*!**************************************************************************
*!
*!      Function: lfvSubProc
*!
*!**************************************************************************
*

FUNCTION lfvSubProc

lcPrompt = PRMBAR("puMenu",GETBAR("puMenu",lsMenu))

lcPross_Id = IIF(AT("*",lcPrompt) > 0,;
                 ALLTRIM(SUBSTR(lcPrompt,AT("*",lcPrompt)+1,10)),"")

DECLARE laMainSub[1,2]
laMainSub = " "
SELECT sydObjct
IF SEEK(lcApp_Id+lcPross_Id)
  IF sydObjct.lSubProc .AND. !EMPTY(sydObjct.mSubProc)
    =gfSubStr(sydObjct.mSubProc,@laMainSub,",|")
    FOR lnCount = 1 TO ALEN(laMainSub,1)
      laMainSub[lnCount,1] = ALLTRIM(laMainSub[lnCount,1]) + ;
                             SPACE(11-LEN(ALLTRIM(laMainSub[lnCount,1])))+ ;
                             ALLTRIM(laMainSub[lnCount,2])
    ENDFOR
    *** Delete the second column. ***
    =gfADel(@laMainSub,2,2)
    DECLARE laMainSub[ALEN(laMainSub,1)]
  ELSE
    RETURN
  ENDIF
ELSE
  RETURN
ENDIF

SELECT (lcPrv_Tmp2)
IF !EMPTY(&lcPrv_Tmp2..mSubProc)
  DECLARE laSubProc[1,2]
  laSubProc = " "
  =gfSubStr(&lcPrv_Tmp2..mSubProc,@laSubProc,",|")
  FOR lnCount = 1 TO ALEN(laSubProc,1)
    laSubProc[lnCount,1] = ALLTRIM(laSubProc[lnCount,1]) + ;
                           SPACE(11-LEN(ALLTRIM(laSubProc[lnCount,1])))+ ;
                           ALLTRIM(laSubProc[lnCount,2])
  ENDFOR
  *** Delete the second column. ***
  =gfADel(@laSubProc,2,2)
  DECLARE laSubProc[ALEN(laSubProc,1)]
ELSE
  DECLARE laSubProc[1]
  laSubProc = " "
ENDIF  

=gfMover(@laMainSub,@laSubProc,"Select Subprocess control")


IF !EMPTY(laSubProc[1])
  lcSubstr = ''
  lcFisSub = ''
  lcLogSub = ''
  FOR lnCount = 1 TO ALEN(laSubProc,1)
    lcFisSub = lcFisSub + IIF(EMPTY(lcFisSub),'',',')+;
               ALLTRIM(SUBSTR(laSubProc[lnCount],1,11))
    lcLogSub = lcLogSub + IIF(EMPTY(lcLogSub),'',',')+;
               ALLTRIM(SUBSTR(laSubProc[lnCount],12))
  ENDFOR
  lcSubstr = lcFisSub + "|" + lcLogSub
ELSE
  lcSubstr = " "
ENDIF    

lcStatus = SUBSTR("MAM",AT(&lcPrv_Tmp2..cStatus,"MAS"),1)
REPLACE &lcPrv_Tmp2..mSubProc WITH lcSubStr ;
        &lcPrv_Tmp2..lSubProc WITH IIF(EMPTY(lcSubStr),.F.,.T.);
        &lcPrv_Tmp2..cStatus  WITH lcStatus


*!**************************************************************************
*!
*!      Function: lfvSelAll
*!
*!**************************************************************************
*

FUNCTION lfvSelAll

SELECT (lcPrv_Tmp2)

FOR lnCount = 1 TO CNTBAR("puMenu")

  lcPrompt = PRMBAR("puMenu",GETBAR("puMenu",lnCount))

  IF AT("+",lcPrompt) <> 0
  
    IF AT(CHR(255),lcPrompt) > 0
      lcPross_Id = IIF(AT("*",lcPrompt) > 0,;
                       ALLTRIM(SUBSTR(lcPrompt,AT("*",lcPrompt)+1,10)),"")
      
      IF !EMPTY(lcPross_Id)
        lcProcType = IIF(AT("@",lcPrompt) > 0,;
                         SUBSTR(lcPrompt,AT("@",lcPrompt)+1,1),"")
        *E300709,1 Hesham (Start)
        *E300709,1 if the selected module is the main system and
        *E300709,1 the bar type is not a program the return
        IF ALLTRIM(lcApp_Id)='SY' AND ALLTRIM(lcProcType)<>'P'
          LOOP
        ENDIF
        *E300709,1 Hesham (End)       
        IF SEEK(ALLTRIM(lcApp_Id)+ALLTRIM(lcPross_Id)+ALLTRIM(lcProcType)+ALLTRIM(lcCom_Id))
          lcStatus = IIF(&lcPrv_Tmp2..lExist,"M","A")
          REPLACE &lcPrv_Tmp2..lAddRec  WITH .T.;
                  &lcPrv_Tmp2..lEditRec WITH .T.;
                  &lcPrv_Tmp2..lDeleRec WITH .T.;
                  &lcPrv_Tmp2..cStatus WITH lcStatus
      
          lcPrompt = STRTRAN(lcPrompt,CHR(255),lcDiamond)
          lnPos    = IIF(AT("$",lcPrompt) > 0,;
                         INT(VAL(SUBSTR(lcPrompt,AT("$",lcPrompt)+1,3))),0)
          lcPrompt = IIF(lnPos > 0 , STUFF(lcPrompt,lnPos,0,"\<") , lcPrompt)
        
          DEFINE BAR GETBAR("puMenu",lnCount) OF puMenu PROMPT lcPrompt
        ELSE
          APPEND BLANK
          REPLACE cuser_id   WITH lcGrpUsrID ;
                  capp_id    WITH lcApp_Id ;
                  ccomp_id   WITH lcCom_Id ;
                  cpross_id  WITH lcPross_Id ;
                  cProcType  WITH lcProcType ;
                  cgrporuser WITH UPPER(LEFT(lcGrpOrUsr,1)) ;
                  lAddRec    WITH .T.;
                  lEditRec   WITH .T.;
                  lDeleRec   WITH .T.;
                  cStatus    WITH "A" ;
                  nRecno     WITH RECNO(lcPrv_Tmp2);
                  cadd_user  WITH gcUser_ID ;
                  dadd_date  WITH DATE() ;
                  cadd_time  WITH gfGetTime()
      
          lcRecNo  = ALLTRIM(STR(RECNO(lcPrv_Tmp2)))
          lcPrompt = STRTRAN(lcPrompt,CHR(255),lcDiamond)
          lcPrompt = SUBSTR(lcPrompt,1,AT("+",lcPrompt)) + lcRecNo
          lnPos    = IIF(AT("$",lcPrompt) > 0,;
                         INT(VAL(SUBSTR(lcPrompt,AT("$",lcPrompt)+1,3))),0)
          lcPrompt = IIF(lnPos > 0 , STUFF(lcPrompt,lnPos,0,"\<") , lcPrompt)

          DEFINE BAR GETBAR("puMenu",lnCount) OF puMenu PROMPT lcPrompt
          *B119794,1 ASH 09/25/2003 (Begin) Add 'POSTYLE' to the user privelages to allow the user to edit the PO screen if opened from generate PO from SO screen.          
          IF lcPross_Id='POSTY'
            SCATTER MEMVAR MEMO
            APPEND BLANK
            GATHER MEMVAR MEMO
            REPLACE cPross_Id WITH 'POSTYLE'
          ENDIF
          *B119794,1 ASH 09/25/2003 (End)
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDFOR

*B127769,1  TMI [Start] refresh the screen after select all
=lfwMenu()
*B127769,1  TMI [End  ] 

*!**************************************************************************
*!
*!      Function: lfvSelNon
*!
*!**************************************************************************
*

FUNCTION lfvSelNon

SELECT (lcPrv_Tmp2)

FOR lnCount = 1 TO CNTBAR("puMenu")

  lcPrompt = PRMBAR("puMenu",GETBAR("puMenu",lnCount))

  IF AT("+",lcPrompt) <> 0
    lnRecNo = INT(VAL(SUBSTR(lcPrompt,AT("+",lcPrompt))))
    
    IF lnRecNo > 0 .AND. lnRecNo <= RECCOUNT(lcPrv_Tmp2)
      
      IF AT(lcDiamond,lcPrompt) > 0
        GO lnRecNo IN &lcPrv_Tmp2
        IF &lcPrv_Tmp2..cGrpORUser = "U" .OR. UPPER(LEFT(lcGrpOrUsr,1)) = "G"
          lcPross_Id = IIF(AT("*",lcPrompt) > 0,;
                           ALLTRIM(SUBSTR(lcPrompt,AT("*",lcPrompt)+1,10)),"")

          IF &lcPrv_Tmp2..cStatus='A'
            DELETE 
          ELSE   
            lcStatus = IIF(&lcPrv_Tmp2..lExist,"D","S")
            REPLACE &lcPrv_Tmp2..cStatus WITH lcStatus
          ENDIF  
          
          lcPrompt = STRTRAN(lcPrompt,lcDiamond,CHR(255))
          lnPos    = IIF(AT("$",lcPrompt) > 0,;
                         INT(VAL(SUBSTR(lcPrompt,AT("$",lcPrompt)+1,3))),0)
          lcPrompt = IIF(lnPos > 0 , STUFF(lcPrompt,lnPos,0,"\<") , lcPrompt)

          DEFINE BAR GETBAR("puMenu",lnCount) OF puMenu PROMPT lcPrompt
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDFOR

*B127769,1  TMI [Start] refresh the screen after select none
=lfwMenu()
*B127769,1  TMI [End  ] 

*!**************************************************************************
*!
*!      Function: lfvOk
*!
*!**************************************************************************
*

FUNCTION lfvOk

IF !laScrMode[1]
  SELECT * FROM (gcWorkDir+lcPrv_Tmp2) ;
         INTO DBF (gcWorkDir+lcPrv_Tmp1)
  llUpdPriv = .T.
ENDIF

*!**************************************************************************
*!
*!      Function: lfvCan
*!
*!**************************************************************************
*

FUNCTION lfvCan

*!**************************************************************************
*!
*!      Function: lfBldBars
*!
*!**************************************************************************
*
*** Function called at the beginning of the program  and ***
*** everytime we change the module  or the company after ***
*** calling the select data function.  And this function ***
*** to build the  popup from the  array hold the current ***
*** module data...

FUNCTION lfBldBars

SELECT SYCMENU

DO CASE
  CASE _DOS
    IF SEEK(lcApp_Id)

      IF !llDontHid
        HIDE POPUP puMenu SAVE
      ENDIF  

      lnBarNo    = 1
      lnBarLevel = 1
      lnNewPlvl  = 1

      lcRecNo    = ""
      lcAddDmnd  = CHR(255)

      llFrst_Pad = .T.

      RELEASE BAR  ALL OF puMenu

      DEFINE BAR lnBarNo OF puMenu PROMPT "\"+;
             REPLICATE(lcHfBloc,70) COLOR SCHEME 24

      lnBarNo = lnBarNo + 1
    
      SCAN REST WHILE SYCMENU.cApp_Id = lcApp_Id
        
        IF !EOF(lcPrv_Tmp2) .AND. &lcPrv_Tmp2..cStatus <> "D"
          lcRecNo   = ALLTRIM(STR(RECNO(lcPrv_Tmp2)))
          lcAddDmnd = lcDiamond
        ELSE
          lcRecNo   = ""
          lcAddDmnd = CHR(255)
        ENDIF

        lnNewPlvl  = INT(VAL(SYCMENU.cPop_Levl))

        DO CASE
          *** New pad 
          CASE SYCMENU.cSub_Typ = "P"

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
                     lcBlock+lcBlock+lcHfBlocs COLOR SCHEME 24

              lnBarNo = lnBarNo + 1
 
              *** Shadow of the previous pad. ***
              DEFINE BAR lnBarNo OF puMenu PROMPT ;
                     "\"+REPLICATE(lcHfBloc,4) + ;
                     REPLICATE(lcBlock,44)+lcHfBlocs COLOR SCHEME 24

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
      
            *** define text of the current pad. ***
            DEFINE BAR lnBarNo OF puMenu PROMPT "\"+ ;
                   lcHfBloc+SUBSTR(SYCMENU.cSub_Prpt,1,41)+;
                   IIF('\<' $ SYCMENU.cSub_Prpt,"  ","")+;
                   lcBranch+lcHfBlocs COLOR SCHEME 24
            lnBarNo = lnBarNo + 1

            *** Start the separator of the current pad. ***
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   "\"+lcHfBloc+lcHfBloc+lcUpLeft+;
                   REPLICATE(lcHzLine,42)+lcUpRght+;
                   lcHfBlocs COLOR SCHEME 24

            lnBarNo = lnBarNo + 1

            *** Branching bar. ***
          CASE SYCMENU.cSub_Typ = "S"
            *** Check if there is any opend popup on a higher level than ***
            *** this new branching bar...
            IF lnNewPlvl < lnBarLevel
              =lfClosPops()
            ENDIF
            *** Define the branching bar with branching mark  ***
            DEFINE BAR lnBarNo OF puMenu PROMPT "\"+ ;
                   lcHfBloc+lcHfBloc+;
                   IIF(lnBarLevel = 1,lcVrLine,;
                   REPLICATE(lcVrLine+"  ",lnBarLevel-1)+lcVrLine)+" "+;
                   SUBSTR(SYCMENU.cSub_Prpt,1,40) + ;
                   IIF('\<' $ SYCMENU.cSub_Prpt,"  ","")+;
                   lcBranch+lcVrLine+lcBlock+lcBlock+;
                   lcHfBlocs COLOR SCHEME 24
             
            lnBarNo    = lnBarNo    + 1

            lnBarLevel = lnBarLevel + 1
      
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   "\"+lcHfBloc+lcHfBloc+;
                   REPLICATE(lcVrLine+"  ",lnBarLevel-1)+;
                   lcUpLeft+REPLICATE(lcHzLine,42)+;
                   lcUpRght+lcHfBlocs COLOR SCHEME 24
      
            lnBarNo = lnBarNo + 1
       
            *** Normal bar or separator. ***
          OTHERWISE

            *** Check if there is any opend popup on a higher level than ***
            *** this new branching bar...
            IF lnNewPlvl < lnBarLevel
              =lfClosPops()
            ENDIF

            IF ALLTRIM(SYCMENU.cSub_Prpt) = "\-"
              *** Separator. ***
              DEFINE BAR lnBarNo OF puMenu PROMPT "\"+ ;
                     lcHfBloc+lcHfBloc+;
                     IIF(lnBarLevel=1,"",;
                     REPLICATE(lcVrLine+"  ",lnBarLevel-1))+;
                     lcLnLeft+REPLICATE(lcHzLine,42)+ ;
                     lcLnRght+lcBlock+lcBlock+lcHfBlocs ;
                     COLOR SCHEME 24
            ELSE
              *** Normal bar
              lcPrompt = lcHfBloc+lcHfBloc+lcVrLine+;
                         IIF(lnBarLevel=1,"",REPLICATE("  "+;
                         lcVrLine,lnBarLevel-1))+lcAddDmnd+;
                         SUBSTR(SYCMENU.cSub_Prpt,1,40)+;
                         IIF('\<' $ SYCMENU.cSub_Prpt,"   "," ")+;
                         lcVrLine+lcBlock+lcBlock+lcHfBlocs+;
                         "*"+SYCMENU.cPross_Id+;
                         "@"+SYCMENU.cProcType
                         
              lcPos = STR(AT("\<",lcPrompt),3)
              DEFINE BAR lnBarNo OF puMenu PROMPT ;
                     lcPrompt+"$"+lcPos+"+"+lcRecNo ;
                     COLOR SCHEME 24
            ENDIF
            lnBarNo = lnBarNo + 1
        ENDCASE
      ENDSCAN

      IF lnBarLevel > 1
        *** Call the function to close any popus. ***
        lnNewPlvl  = 1
        =lfClosPops()
      ENDIF

      *** Close any previous pad. ***
      DEFINE BAR lnBarNo OF puMenu PROMPT ;
             "\"+lcHfBloc+lcHfBloc+lcDnLeft+;
             REPLICATE(lcHzLine,42)+lcDnRght+;
             lcBlock+lcBlock+lcHfBlocs ;
             COLOR SCHEME 24

      lnBarNo = lnBarNo + 1

      *** Shadow of the previous pad. ***
      DEFINE BAR lnBarNo OF puMenu PROMPT ;
             "\"+REPLICATE(lcHfBloc,4) + ;
             REPLICATE(lcBlock,44)+lcHfBlocs;
             COLOR SCHEME 24
      lnBarNo = lnBarNo + 1

      DEFINE BAR lnBarNo OF puMenu PROMPT "\"+;
             REPLICATE(lcHfBloc,70) COLOR SCHEME 24

      lsMenu  = 6
  
      IF !llDontHid
        SHOW POPUP puMenu
      ELSE
        llDontHid = .F.
      ENDIF
    ELSE
      *** No records for this module. ***
      *** <  Ok  > ***
      =gfModalGen("TRM00155B00000","DIALOG")
      RETURN .F.
    ENDIF
  
  *****************************************************************
  CASE _WINDOWS
    IF SEEK(lcApp_Id)

      IF !llDontHid
        HIDE POPUP puMenu SAVE
      ENDIF  

      lnBarNo    = 1
      lnBarLevel = 1
      lnNewPlvl  = 1

      lcRecNo    = ""
      lcAddDmnd  = CHR(255)

      llFrst_Pad = .T.

      RELEASE BAR  ALL OF puMenu

      *B804169,1 AME [Start] in case of search for parent moduel check that kid module is
      *B804169,1 AME         check that kid module is included in Bar module field
      *SCAN REST WHILE SYCMENU.cApp_Id = lcApp_Id
      SCAN REST WHILE SYCMENU.cApp_Id = lcApp_Id FOR IIF(EMPTY(lcKid),.T.,lcKid $ CBarModule)
      *B804169,1 AME [End]
        
        IF !EOF(lcPrv_Tmp2) .AND. &lcPrv_Tmp2..cStatus <> "D"
          lcRecNo   = ALLTRIM(STR(RECNO(lcPrv_Tmp2)))
          lcAddDmnd = lcDiamond
        ELSE
          lcRecNo   = ""
          lcAddDmnd = CHR(255)
        ENDIF

        lnNewPlvl  = INT(VAL(SYCMENU.cPop_Levl))

        DO CASE
          *** New pad 
          CASE SYCMENU.cSub_Typ = "P"

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
                     lcBlock+lcBlock+lcHfBlocs ;
                     COLOR RGB(0,0,0,255,255,255)
      
              SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.

              lnBarNo = lnBarNo + 1
 
              DEFINE BAR lnBarNo OF puMenu PROMPT "\-"

              lnBarNo = lnBarNo + 1
  
            ENDIF
      
            *** define text of the current pad. ***
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   lcHfBloc+SUBSTR(SYCMENU.cSub_Prpt,1,41)+;
                   IIF('\<' $ SYCMENU.cSub_Prpt,"  ","")+;
                   lcBranch+lcHfBlocs
            lnBarNo = lnBarNo + 1

            *** Start the separator of the current pad. ***
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   lcHfBloc+lcHfBloc+lcUpLeft+;
                   REPLICATE(lcHzLine,42)+lcUpRght+;
                   lcHfBlocs COLOR RGB(0,0,0,255,255,255)
      
            SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.

            lnBarNo = lnBarNo + 1

            *** Branching bar. ***
          CASE SYCMENU.cSub_Typ = "S"
            *** Check if there is any opend popup on a higher level than ***
            *** this new branching bar...
            IF lnNewPlvl < lnBarLevel
              =lfClosPops()
            ENDIF
            *** Define the branching bar with branching mark  ***
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   lcHfBloc+lcHfBloc+;
                   IIF(lnBarLevel = 1,lcVrLine,;
                   REPLICATE(lcVrLine+"  ",lnBarLevel-1)+lcVrLine)+" "+;
                   SUBSTR(SYCMENU.cSub_Prpt,1,40) + ;
                   IIF('\<' $ SYCMENU.cSub_Prpt,"  ","")+;
                   lcBranch+lcVrLine+lcBlock+lcBlock+;
                   lcHfBlocs
             
            lnBarNo    = lnBarNo    + 1

            lnBarLevel = lnBarLevel + 1
      
            DEFINE BAR lnBarNo OF puMenu PROMPT ;
                   lcHfBloc+lcHfBloc+;
                   REPLICATE(lcVrLine+"  ",lnBarLevel-1)+;
                   lcUpLeft+REPLICATE(lcHzLine,42)+;
                   lcUpRght+lcHfBlocs ;
                   COLOR RGB(0,0,0,255,255,255)
      
            SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.
      
            lnBarNo = lnBarNo + 1
       
            *** Normal bar or separator. ***
          OTHERWISE

            *** Check if there is any opend popup on a higher level than ***
            *** this new branching bar...
            IF lnNewPlvl < lnBarLevel
              =lfClosPops()
            ENDIF

            IF ALLTRIM(SYCMENU.cSub_Prpt) = "\-"
              *** Separator. ***
              DEFINE BAR lnBarNo OF puMenu PROMPT ;
                     lcHfBloc+lcHfBloc+;
                     IIF(lnBarLevel=1,"",;
                     REPLICATE(lcVrLine+"  ",lnBarLevel-1))+;
                     lcLnLeft+REPLICATE(lcHzLine,42)+ ;
                     lcLnRght+lcBlock+lcBlock+lcHfBlocs
            ELSE
                *** Normal bar
                lcPrompt = lcHfBloc+lcHfBloc+lcVrLine+;
                           IIF(lnBarLevel=1,"",REPLICATE("  "+;
                           lcVrLine,lnBarLevel-1))+lcAddDmnd+;
                           SUBSTR(SYCMENU.cSub_Prpt,1,40)+;
                           IIF('\<' $ SYCMENU.cSub_Prpt,"   "," ")+;
                           lcVrLine+lcBlock+lcBlock+lcHfBlocs+;
                           "*"+SYCMENU.cPross_Id+;
                           "@"+SYCMENU.cProcType
                         
                lcPos = STR(AT("\<",lcPrompt),3)
                DEFINE BAR lnBarNo OF puMenu PROMPT ;
                           lcPrompt+"$"+lcPos+"+"+lcRecNo
              
            ENDIF
            lnBarNo = lnBarNo + 1
        ENDCASE
      ENDSCAN

      IF lnBarLevel > 1
        *** Call the function to close any popus. ***
        lnNewPlvl  = 1
        =lfClosPops()
      ENDIF

      *** Close any previous pad. ***
      DEFINE BAR lnBarNo OF puMenu PROMPT ;
             lcHfBloc+lcHfBloc+lcDnLeft+;
             REPLICATE(lcHzLine,42)+lcDnRght+;
             lcBlock+lcBlock+lcHfBlocs ;
             COLOR RGB(0,0,0,255,255,255)
      
      SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.

      lsMenu  = 1
  
      IF !llDontHid
        SHOW POPUP puMenu
      ELSE
        llDontHid = .F.
      ENDIF
    ELSE
      *** No records for this module. ***
      *** <  Ok  > ***
      =gfModalGen("TRM00155B00000","DIALOG")
      RETURN .F.
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
                 lcBlock+lcBlock+lcHfBlocs;
                 COLOR SCHEME 24
      lnBarNo = lnBarNo + 1
      *** Shadow of any popup. ***
      DEFINE BAR lnBarNo OF puMenu PROMPT ;
                 "\"+REPLICATE(lcHfBloc,2)+;
                 REPLICATE(lcVrLine+"  ",lnBarLevel-1)+;
                 SPACE(2)+REPLICATE(lcBlock,44)+;
                 lcHfBlocs COLOR SCHEME 24
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
                 lcBlock+lcBlock+lcHfBlocs COLOR RGB(0,0,0,255,255,255)
      
      SET SKIP OF BAR lnBarNo OF POPUP puMenu .T.
      
      lnBarNo = lnBarNo + 1

      *** Current level is moving down till we hit the new current level. ***
      *** This variable should reach only to the new current level not below ***
      lnBarLevel = lnBarLevel - 1
    ENDFOR
ENDCASE

*!**************************************************************************
*!
*!      Function: lfUsrORGrp
*!
*!**************************************************************************
*

FUNCTION lfUsrORGrp

lcPrompt = PRMBAR("puMenu",GETBAR("puMenu",lsMenu))

lnReturn = 0

IF AT("+",lcPrompt) > 0
  lnRecNo = INT(VAL(SUBSTR(lcPrompt,AT("+",lcPrompt))))
  IF lnRecNo > 0 .AND. lnRecNo <= RECCOUNT(lcPrv_Tmp2)
    IF UPPER(LEFT(lcGrpOrUsr,1)) = "G"
      lnReturn = lnRecNo
    ELSE
      GO lnRecNo IN &lcPrv_Tmp2
      IF &lcPrv_Tmp2..cGrpORUser <> "U"
        *** You cannot edit this bar because it ***
        *** is a group access not a user access ***
        *** <  Ok  > ***
        =gfModalGen("TRM00152B00000","DIALOG")
        lnReturn = 0
      ELSE
        lnReturn = lnRecNo
      ENDIF
    ENDIF
  ELSE
    lnReturn = 0
  ENDIF
ELSE
  lnReturn = 0
ENDIF

RETURN lnReturn

