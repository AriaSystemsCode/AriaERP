*:************************************************************************
*: Program file  : GFLOG_IN.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: Yasser Saad Ibrahim 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
*E300271,1 ORABY 08/20/95 Output the maximum number of users and the currently logged users in the screen
*B800215,1 M.H   10/05/95 The user is trapped in the User name field.
*B800280,1 M.H   11/02/95 Fix the error that appear under windows if the user enter a wrong pass word.
*E300355,1 ORABY 03/04/96 In the screen : Change the Copyright year to be 1996 instead of 1995
*E300380,1 Oraby  04/09/96 Make the counting of the number of logged users faster
*E600823,1 Hesham El-Sheltawi 05/16/96
*E600823,1 check for exist of the user resource file
*E600823,1 if not exist then create one and use it
*B601286,1 MAN 10/10/96 Solve insufficient memory on Win95
*E300581,1 Hesham El_Sheltawi 03/05/97
*E300581,1 Get the user Level in the global variable gcUser_lvl
*B601919,1 RENEE 04/28/98 Fix : - Variable not found at login
*B601919,1 						- Where is file?
*B601919,1 				  bugs at login, and relogin.
*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it
*E301874,1 Abd 07/03/2002 Modify the activation key file to be as EXE not a Bin File
*E301874,1 Abd            And get our needed dat from this exe.
*B606713,1 Abd 12/19/2002 Problem with the login screen.
*B037227,1 MHM 01/05/2004 Modify User Name and Password.
*B124066,1 MHM 10/27/2004 Fix Bug of when we open 2 sessions from Aria one for regular Aria27 and the 
*B124066,1                other one for Aria EDI, which the system count these two as 2 deferent users
*:************************************************************************
*
FUNCTION gfLog_In
*B601919,1 Declare private variable lnCurAlias to hold the current alias
PRIVATE lnCurAlias
*B601919,1 

DIMEN laResStru[7,4]

laResStru[1,1] = 'TYPE'
laResStru[1,2] = 'C'
laResStru[1,3] = 12
laResStru[1,4] = 0

laResStru[2,1] = 'ID'
laResStru[2,2] = 'C'
laResStru[2,3] = 12
laResStru[2,4] = 0

laResStru[3,1] = 'NAME'
laResStru[3,2] = 'C'
laResStru[3,3] = 24
laResStru[3,4] = 0

laResStru[4,1] = 'READONLY'
laResStru[4,2] = 'L'
laResStru[4,3] = 1
laResStru[4,4] = 0

laResStru[5,1] = 'CKVAL'
laResStru[5,2] = 'N'
laResStru[5,3] = 6
laResStru[5,4] = 0

laResStru[6,1] = 'DATA'
laResStru[6,2] = 'M'
laResStru[6,3] = 10
laResStru[6,4] = 0

laResStru[7,1] = 'UPDATED'
laResStru[7,2] = 'D'
laResStru[7,3] = 8
laResStru[7,4] = 0


IF !USED('syuuser')
  SELECT 0
  USE (gcSysHome+"syuuser") ORDER TAG CUSER_ID
ELSE
  SELECT SYUUSER
  SET ORDER TO TAG CUSER_ID   
ENDIF  

DECLARE laUsr_Pass[8]

laUsr_Pass    = SPACE(1)
lcUser_ID     = IIF(!EMPTY(GETENV('USERID')),UPPER(SUBSTR(GETENV('USERID'),1,10)),SPACE(10))
lcUsr_Pass    = SPACE(8)

llRelog       = .F.
llRet_flag    = .F.
lcBellStat    = SET("BELL")
llUsErChang   = .T.
llNochang     = .F.

SET BELL OFF
SET CONFIRM OFF
*B601286,1 MAN 10/10/96 Solve insufficient memory on Win95
*SET PALETT OFF
IF _WINDOWS
  IF !USED('syclogo')
    SELECT 0
    USE(gcSysHome+"syclogo")
  ENDIF
ENDIF

IF !EMPTY(lcUser_ID) 
  llNochang = .T.
  KEYBOARD "{ENTER}" CLEAR
ENDIF
*E301077,78 Hesham (Start)
llInstUse=gfSysOpen(gcSysHome+'SYCINST')
*E301077,78 Hesham (End)

DO SYLOGIN.SPR

SET CONFIRM ON
SET BELL &lcBellStat

IF USED('syclogo')
  USE IN syclogo
ENDIF  

IF llRet_flag
  IF syuuser.lusr_1st .AND. syuuser.cusr_levl <> "A"
    llRet_flag = (ALLTRIM(UPPER(gcStation))==ALLTRIM(UPPER(syuuser.cusr_dwks)))
    IF !llRet_flag
     =gfModalgen("QRM00017B00000","ALERT") 
    ENDIF
  ENDIF
ENDIF


*** If user was permited to login
IF llRet_flag
  lcOldUser = gcUser_ID
  gcUser_ID = lcUser_ID
  *B601919,1 Get the user level from SYUUSER file
  *E300581,1 Hesham El_Sheltawi (Start)
  *E300581,1 Get the user Level in a global variable
  *gcUser_lvl = cUsr_Levl
  gcUser_lvl = SYUUSER.cUsr_Levl
  *E300581,1 Hesham El_Sheltawi (End)  
  *B601919,1 end

  *B037227,1 MHM 01/05/2004 Modify User Name and Password[Start]
  *IF lcUser_ID = "ADMN"
  IF lcUser_ID = "ELSHERKA"
  *B037227,1 MHM 01/05/2004 Modify User Name and Password[End]

    glLog_Requ = .F.  
    gcUserName = "Administrator"
    *E300581,1 Hesham El_Sheltawi (Start)
    *E300581,1 Get the user Level in a global variable
    gcUser_lvl = 'A'
    *E300581,1 Hesham El_Sheltawi (End)      
  ELSE

    *** Check if user still permited 

    IF (!EMPTY(syuuser.dUsr_Begn) .AND. !EMPTY(syuuser.dUsr_End) .AND. ;
         !BETWEEN(DATE(),syuuser.dUsr_Begn,syuuser.dUsr_End))    .OR.  ;
       (!EMPTY(syuuser.cUsr_BegT) .AND. !EMPTY(syuuser.cUsr_BegT).AND. ;
        !BETWEEN(TIME(),syuuser.cUsr_BegT,syuuser.cUsr_EndT)) 
    
       =gfModalgen("INM00017B00000","ALERT")
    
       gcUser_ID  = lcOldUser
       llRet_flag = .F.
    ELSE
      *** Check if there any user loged befor with same User_ID
      IF syuuser.lusr_logd .AND. !glLog_Requ
        IF gfModalgen("QRM00014B00006","ALERT",ALLTRIM(syuuser.cusr_name)) = 1
          *B601919,1 Select an alias
          lnCurAlias = SELECT(0)
		  SELECT SYUUSER
		  *B601919,1 end
          REPLACE syuuser.lusr_logd WITH  .T.,;
                  syuuser.dusr_lld  WITH DATE(),;
                  syuuser.cusr_llt  WITH gfGetTime()
          *B601919,1 Reset alias
          SELECT (lnCurAlias)
          *B601919,1 end
          gcUser_Com = syuuser.cUsr_Dcom
          llRelog = .T.
        ELSE
          gcUser_ID  = lcOldUser
          llRet_flag = .F.
        ENDIF
      ELSE
        SELECT syuuser

        lnSaveRec =RECNO()

        IF SEEK(lcOldUser)
          REPLACE syuuser.lusr_logd WITH  .F.
        ENDIF
      
        GO lnSaveRec      
      
        gcUserName = syuuser.cUsr_Name
        gcUser_Com = syuuser.cUsr_Dcom
        gcPrntr_ID = syuuser.cUsr_dPrt
        gcUser_Grp = syuuser.cUsr_grup
        gcUser_Mdl = syuuser.cUsr_dMdl
        *(MAN)
        gcExtPrg   = syuuser.cExtPrg
        glToolActv = IIF(_WINDOWS,syuuser.lusr_ustb ,.F.)
        glUser_Tsk = syuuser.lUsr_dTask
        glMsgRem   = syuuser.lUsr_msgr   
        glChngPass = syuuser.lUsr_chgp
        glLog_Requ = (syuuser.cusr_levl='O')
        gcUser_bmp = ALLTRIM(syuUser.cDef_BMP)

        IF EMPTY(syuuser.cusr_Resr)
          REPLACE syuuser.cusr_Resr WITH gfTempName()
        ENDIF
        *--man start
        IF !EMPTY(syuuser.cUsr_dwks)
          PRIVATE llWrkstUsd
          llWrkstUsd = .F.
          IF !USED("SYCWRKST")
            llWrkstUsd = .T. 
            USE (gcSyshome+"SYCWRKST") ORDER TAG CWRKST_ID IN 0 
          ENDIF  
          IF SEEK(syuuser.cUsr_dwks,"SYCWRKST")
            IF sycwrkst.lhavelocl
              IF !EMPTY(sycwrkst.ctemppath) .AND.;
                 gfValdPath(ALLTRIM(sycwrkst.ctemppath))
                gcWorkDir = ALLTRIM(sycwrkst.ctemppath)
                gcWorkDir = IIF(RIGHT(gcWorkDir,1)="\",gcWorkDir,gcWorkDir+"\")
              ENDIF     
            ENDIF
          ENDIF
          IF llWrkstUsd 
            USE IN SYCWRKST
          ENDIF
        ENDIF  
        *--man end        

        IF !EMPTY(syuuser.cusr_Resr) .AND. !syuuser.lusr_logd
          *E600823,1 Hesham El-Sheltawi (Start)
          *E600823,1 check for exist of the user resource file
          *E600823,1 if not exist then create one and use it
*          IF FILE(gcRsrcHome+syuuser.cusr_Resr+".DBF")
*            IF UPPER(SET("RESOURCE",1)) <> UPPER (gcRsrcHome+syuuser.cusr_Resr)
*              SET RESOURCE TO (gcRsrcHome+syuuser.cusr_Resr)
*            ENDIF  
*          ENDIF
          
          IF FILE(gcRsrcHome+syuuser.cusr_Resr+".DBF")
            IF UPPER(SET("RESOURCE",1)) <> UPPER (gcRsrcHome+syuuser.cusr_Resr)
              SET RESOURCE TO (gcRsrcHome+syuuser.cusr_Resr)
            ENDIF  
            *E600823,1 Hesham El-Sheltawi (Start)
            *E600823,1 check if the system resource file for the user
            *E600823,1 does not exist then create new one
            =lfCrtUsrFile()
            *E600823,1 Hesham El-Sheltawi (Start)            
          ELSE
*            SELECT * FROM  (gcSysHome+"SYCRESRC") INTO DBF (gcRsrcHome+syuuser.cusr_Resr)              
             CREATE DBF (gcRsrcHome+syuuser.cusr_Resr) FROM ARRAY laResStru
            IF USED(syuuser.cusr_Resr)
              USE IN (syuuser.cusr_Resr)
            ENDIF
            SET RESOURCE TO (gcRsrcHome+syuuser.cusr_Resr)            
            *E600823,1 Hesham El-Sheltawi (Start)
            *E600823,1 check if the system resource file for the user
            *E600823,1 does not exist then create new one else zap the file
            =lfCrtUsrFile(.T.)
            *E600823,1 Hesham El-Sheltawi (Start)                        
          ENDIF  
          *E600823,1 Hesham El-Sheltawi (End)          
          *E600823,1 Hesham El-Sheltawi (Start)
          *E600823,1 check for exist of the user resource file
          *E600823,1 if not exist then create one and use it
*          IF FILE(gcRsrcHome+syuuser.cusr_Resr+".DBF")
*            IF UPPER(SET("RESOURCE",1)) <> UPPER (gcRsrcHome+syuuser.cusr_Resr)
*              SET RESOURCE TO (gcRsrcHome+syuuser.cusr_Resr)
*            ENDIF  
*          ENDIF
          
          IF FILE(gcRsrcHome+syuuser.cusr_Resr+".DBF")
            IF UPPER(SET("RESOURCE",1)) <> UPPER (gcRsrcHome+syuuser.cusr_Resr)
              SET RESOURCE TO (gcRsrcHome+syuuser.cusr_Resr)
            ENDIF  
            *E600823,1 Hesham El-Sheltawi (Start)
            *E600823,1 check if the system resource file for the user
            *E600823,1 does not exist then create new one
            =lfCrtUsrFile()
            *E600823,1 Hesham El-Sheltawi (Start)            
          ELSE
*            SELECT * FROM  (gcSysHome+"SYCRESRC") INTO DBF (gcRsrcHome+syuuser.cusr_Resr)              
            CREATE DBF (gcRsrcHome+syuuser.cusr_Resr) FROM ARRAY laResStru            
            IF USED(syuuser.cusr_Resr)
              USE IN (syuuser.cusr_Resr)
            ENDIF
            SET RESOURCE TO (gcRsrcHome+syuuser.cusr_Resr)            
            *E600823,1 Hesham El-Sheltawi (Start)
            *E600823,1 check if the system resource file for the user
            *E600823,1 does not exist then create new one else zap the file
            =lfCrtUsrFile(.T.)
            *E600823,1 Hesham El-Sheltawi (Start)                        
          ENDIF  
          *E600823,1 Hesham El-Sheltawi (End)          
        ENDIF
        SELECT SYUUSER            
        REPLACE syuuser.lusr_logd WITH  .T.,;
                syuuser.dusr_lld  WITH DATE(),;
                syuuser.cusr_llt  WITH gfGetTime()

        IF USED ('syuckmsg')
          IF SEEK (gcUser_ID,'syuckmsg')
            gnMsgRec = RECNO('syuckmsg')
          ELSE
            gnMsgRec  = 0
            gnTimeOut = 0
          ENDIF 
        ENDIF  

      ENDIF     
      glLog_Requ = sycInst.linsLogRq .AND. ALLTRIM(UPPER(syuuser.cUsr_levl))<>"A"
    ENDIF  
  ENDIF
ENDIF  
*E301077,78 Hesham (Start)
USE IN IIF(llInstUse,'SYCINST',0)
*E301077,78 Hesham (End)

*E300254,1 initializing the user default printer escape sequence under both
*E300254,1 platforms "DOS,WINDOWS"
*IF _DOS
  DO (_GENPD) WITH 4,gcPrntr_ID
*ENDIF  

RETURN llRet_flag

*!*******************************************************************
*!
*!             FUNCTION: lfvUsr_ID
*!
*!*******************************************************************
*B800215,1 M.H 10/05/95 The user is trapped in the User name field.
*
FUNCTION lfvUsr_ID

lcExactStat = SET('EXACT')
SET EXACT ON
IF !EMPTY(lcUser_ID)

  *B606713,1 Abd - Problem with the login screen. [Begin]

  *B037227,1 MHM 01/05/2004 Modify User Name [Start]
  *IF !glReLogIn .AND. !sycinst.linspassw .AND. !(lcUser_ID = "ADMN")
  IF !glReLogIn .AND. !sycinst.linspassw .AND. !(lcUser_ID = "ELSHERKA")
  *B037227,1 MHM 01/05/2004 Modify User Name [End]

    lnCurUsers = gfUserList(.T.)
    *B124066,1 MHM 10/27/2004 check if user log mor than one time[Start]
    *ash Hold based on yasser oraby comment.
    *DECLARE laUsrLst[1]
    *laUsrLst = " "
    *SELECT DISTINCT CUSER_ID ;
       FROM (gcSysHome+"SYUSTATC");
       WHERE COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
              'INI'+'OLDVARS' ;
       .AND.;
             gfCheckUser(SYUSTATC.CUSER_ID,CSTATION) ;
       INTO ARRAY  laUsrLst
       
    *IF gnMaxUsers > ALEN(laUsrLst)
    *  IF lnCurUsers > 1
    *     lnCurUsers  = ALEN(laUsrLst)
    *  ENDIF
    *ENDIF 
    *B124066,1 MHM [End]
    
    IF IIF(llRelog,(lnCurUsers -1),lnCurUsers) >= gnMaxUsers
      IF !llNochang
        *-- Max number of users are loged in  
        =gfModalGen(.F.,.F.,.F.,.F.,'The system has reached the maximum number of users.'+;
                    chr(13)+chr(10)+'You can not log in now.')
        CLEAR READ
      ENDIF  
      llRet_flag = .F.
      RETURN llRet_flag    
    ENDIF
  ENDIF
  *B606713,1 Abd - [End]



  *B037227,1 MHM 01/05/2004 Modify User Name [Start]
  *IF SEEK(ALLTRIM(lcUser_ID),'SYUUSER') .OR. lcUser_ID ="ADMN"
  IF SEEK(ALLTRIM(lcUser_ID),'SYUUSER') .OR. lcUser_ID ="ELSHERKA"
  *B037227,1 MHM 01/05/2004 Modify User Name [End]

    lcUser_ID = ALLTRIM(lcUser_ID) + SPACE(10-LEN(ALLTRIM(lcUser_ID)))
    IF llNochang
      llUsErChang   = .F.  
    ENDIF
    
    IF !sycinst.linspassw 
      IF _WINDOWS
        FOR lnypos = WLCOL('gwdlog_in') TO -20 STEP -1
          MOVE WINDOW gwdlog_in BY -1,0
        ENDFOR
      ENDIF  

      llRet_flag = .T. 
      CLEAR READ
    ENDIF
  ELSE
    IF !llNochang
      WAIT "Unlisted user name, try again...!" WINDOW NOWAIT
    ENDIF  

    llNochang     = .F.    
    llUsErChang   = .T.
    lcUser_ID =SPACE(10)
    SHOW GET lcUser_ID
    _CUROBJ = _CUROBJ
  ENDIF  
*B800215,1 M.H 10/05/95
ELSE
  _CUROBJ = OBJNUM(pbOk_Cncl)
  *E300380,1 Stop enableing of the OK button  
  *SHOW GET pbOk_Cncl ENABLE
ENDIF
*B800215,1 End.

IF lcExactStat = 'ON'
  SET EXACT ON
ELSE
  SET EXACT OFF
ENDIF  

*!*******************************************************************
*!
*!             FUNCTION: lfwUsr_Pass
*!
*!*******************************************************************
*B800215,1 M.H 10/05/95 The user is trapped in the User name field.
*
FUNCTION lfwUsr_Pass

IF EMPTY(lcUser_ID)
  _CUROBJ = OBJNUM(pbOk_Cncl)
  SHOW GET pbOk_Cncl ENABLE
  RETURN
ENDIF 

*!*******************************************************************
*!
*!             FUNCTION: lfvUsr_Pass
*!
*!*******************************************************************
*
FUNCTION lfvUsr_Pass

lnChrPos = VAL(SUBSTR(SYS(18),AT("(",SYS(18))+1))

*E300271,1 If the user pressed tab or enter leave the passwod and go to the OK button
IF INLIST(LASTKEY(),13,9) 
  _CUROBJ    = OBJNUM(pbOk_Cncl)
   RETURN
ENDIF

IF LASTKEY() = 15 .OR. (LASTKEY() = 19 .AND. lnChrPos = 1)
  _CUROBJ    = OBJNUM(lcUser_ID)
  lcUsr_Pass = SPACE(8)
  laUsr_Pass = " "
  SHOW GETS
  RETURN
ENDIF

IF  EVAL(SYS(18)) <> "*" .AND. ASC(EVAL(SYS(18))) > 32
  lcUsr_Pass = STUFF(lcUsr_Pass,lnChrPos,1,EVAL(SYS(18)))
  laUsr_Pass[lnChrPos] = "*"
  SHOW GET laUsr_Pass[lnChrPos] ;
       COLOR &gcReadClr,&gcReadClr,;
       &gcReadClr,&gcReadClr,&gcReadClr,;
       &gcReadClr,&gcReadClr,&gcReadClr,&gcReadClr,&gcReadClr
ENDIF  

*E300271,1 move the checking of the pass word from the valid function 
*E300271,1 to the OK button

RETURN


*IF INLIST(LASTKEY(),13,24,5,9,18,3) .OR. lnChrPos = 8

*  IF ALLTRIM(SYS(2007,ALLTRIM(lcUsr_Pass))) == ALLTRIM(syuuser.cUsr_Pass)  .OR.;
*     (lcUser_ID ="ADMN" .AND. ALLTRIM(lcUsr_Pass) == gcAdmnPass)

*    IF _WINDOWS
*      FOR lnypos = WLCOL('gwdlog_in') TO -20 STEP -1
*        MOVE WINDOW gwdlog_in BY -1,0
*      ENDFOR
*    ENDIF  
*    llRet_flag = .T.
*    CLEAR READ
*  ELSE
*    WAIT "Invalid password, try again...!" WINDOW NOWAIT
*    lcUsr_Pass = SPACE(8)
*    laUsr_Pass = " "
*    SHOW GETS
*    _CUROBJ = OBJNUM(laUsr_Pass[1])
*    llRet_flag = .F.
*  ENDIF
*ENDIF

*!*************************************************************
*! Name      : lfwOK_CNCL
*! Developer : Yasser EL Oraby
*! Date      : 08/20/1995
*! Purpose   : *E300271,1 When function of the OK and Cancel buttons
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->

FUNCTION lfwOK_CNCL

*E300271,1  If comming to the OK button with enter automatically validate the pass word
*B800215,1  The user is trapped in the user name field.
*B800280,1  Fix the error that appear under windows if the user enter a wrong pass word.
*IF !EMPTY(lcUser_ID) .AND. LASTKEY() = 13
IF !EMPTY(lcUser_ID) .AND. !EMPTY(laUsr_Pass) .AND. LASTKEY() = 13
*B800280,1  End.
  KEYBOARD '{ENTER}'
ENDIF

*!*************************************************************
*! Name      : lfvOK_CNCL
*! Developer : Yasser EL Oraby
*! Date      : 08/20/1995
*! Purpose   : *E300271,1 Valid function of the OK and Cancel buttons
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->
FUNCTION lfvOK_CNCL

IF pbOk_Cncl = 1

  *B606713,1 Abd - Problem with the login screen. [Begin]
  *B037227,1 MHM 01/05/2004 Modify User Name [Start]
  *IF !glReLogIn .AND. !(lcUser_ID ="ADMN" .AND. ALLTRIM(lcUsr_Pass) == gcAdmnPass)
  IF !glReLogIn .AND. !(lcUser_ID ="ELSHERKA" .AND. ALLTRIM(lcUsr_Pass) == gcAdmnPass)
  *B037227,1 MHM 01/05/2004 Modify User Name [End]

    lnCurUsers = gfUserList(.T.)

    *B124066,1 MHM 10/27/2004 check if user log mor than one time[Start]
    *ash Hold based on yasser oraby comment.
    *DECLARE laUsrLst[1]
    *laUsrLst = " "
    *SELECT DISTINCT CUSER_ID ;
       FROM (gcSysHome+"SYUSTATC");
       WHERE COBJ_TYP+ALLTRIM(COBJ_NAME)+SYUSTATC.CUSER_ID+CSTATION=;
              'INI'+'OLDVARS' ;
       .AND.;
             gfCheckUser(SYUSTATC.CUSER_ID,CSTATION) ;
       INTO ARRAY  laUsrLst
       
    *IF gnMaxUsers > ALEN(laUsrLst)
    *  IF lnCurUsers > 1
    *     lnCurUsers  = ALEN(laUsrLst)
    *  ENDIF
    *ENDIF 
    *B124066,1 MHM [End]
    
    IF lnCurUsers >=gnMaxUsers
      *-- Max number of users are loged in  
      =gfModalGen(.F.,.F.,.F.,.F.,'The system has reached the maximum number of users.'+;
                 chr(13)+chr(10)+'You can not log in now.')
      CLEAR READ
      llRet_flag = .F.
      RETURN llRet_flag    
    ENDIF
  ENDIF
  *B606713,1 Abd - [End]
  
  *B037227,1 MHM 01/05/2004 Modify User Name [Start]
  *IF ALLTRIM(SYS(2007,ALLTRIM(lcUsr_Pass))) == ALLTRIM(syuuser.cUsr_Pass) .OR.;
  *   (lcUser_ID ="ADMN" .AND. ALLTRIM(lcUsr_Pass) == gcAdmnPass)
  IF ALLTRIM(SYS(2007,ALLTRIM(lcUsr_Pass))) == ALLTRIM(syuuser.cUsr_Pass) .OR.;
     (lcUser_ID ="ELSHERKA" .AND. ALLTRIM(lcUsr_Pass) == gcAdmnPass)
  *B037227,1 MHM 01/05/2004 Modify User Name [End]

    IF _WINDOWS
      FOR lnypos = WLCOL('gwdlog_in') TO -20 STEP -1
        MOVE WINDOW gwdlog_in BY -1,0
      ENDFOR
    ENDIF  
    llRet_flag = .T.
    CLEAR READ
  ELSE
    IF EMPTY(lcUser_ID)
      IF pbOk_Cncl = 1 
        WAIT "Invalid User ID, try again...!" WINDOW NOWAIT
        _CUROBJ = OBJNUM(lcUser_ID)
        llRet_flag = .F.
      ENDIF  
      SHOW GETS
    ELSE        
      WAIT "Invalid password, try again...!" WINDOW NOWAIT
      lcUsr_Pass = SPACE(8)
      laUsr_Pass = " "
      _CUROBJ = OBJNUM(laUsr_Pass[1])
      SHOW GETS
    ENDIF  
    llRet_flag = .F.
  ENDIF
ELSE
  CLEAR READ
  llRet_flag = .F.
ENDIF
RETURN llRet_flag


*E600823,1 Hesham El-Sheltawi (Start)            
*E600823,1 new function to create the user system resource file
*E600823,1 if needed or zap it if we are going to create new resource
*E600823,1 file for the user and the system resource file for the user
*E600823,1 already exist
FUNCTION lfCrtUsrFile
PARAMETERS llZap
lnActWorkArea = SELECT()
*E600823,1 Hesham El-Sheltawi (Start)
*E600823,1 SET the resource to off
SET RESOURCE OFF
*E600823,1 Hesham El-Sheltawi (End)
IF !FILE(gcRsrcHome+'X'+SYS(2007,PADR(gcUser_ID,10))+'.DBF')
  CREATE DBF (gcRsrcHome+'X'+SYS(2007,PADR(gcUser_ID,10))) (CUSERPREF C(10),MBROWFIELD M,CPREFDESC C(30))
  INDEX ON CUSERPREF TAG CUSERPREF ADDITIVE
ELSE
  IF llZap
    SELECT 0
    USE (gcRsrcHome+'X'+SYS(2007,PADR(gcUser_ID,10))) EXCL
    ZAP
  ENDIF  
ENDIF
IF USED('X'+SYS(2007,PADR(gcUser_ID,10)))
  USE IN ('X'+SYS(2007,PADR(gcUser_ID,10)))
ENDIF
SELECT (lnActWorkArea)
*E600823,1 Hesham El-Sheltawi (End)