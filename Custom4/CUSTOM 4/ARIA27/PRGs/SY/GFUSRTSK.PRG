*:************************************************************************
*: Program file  : GFUSRTSK.PRG
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
*E038033,1 HMA 04/03/2004 Select only records belongs to A27(to split 
*E038033,1                A27 records of SYDREPRT File from A4XP records).
*:************************************************************************
PARAMETERS lcFromMenu 

*** This variable will contain 'Yes' if comming from menu and 'NO' if
*** comming form the main control procedure
lcFromMenu = IIF(TYPE('lcFromMenu')='C',lcFromMenu,'YES')

*** If the array is empty thats mean the user have no task to do in the
*** SYUDTASK file of he finished the tasks for today but he want to restart the
*** daily task again...!


IF EMPTY(gaUsr_task[1,1])

  *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]

  *SELECT SYUDTASK.CPRIORTY ,SYUDTASK.CPROCTYPE, SYUDTASK.CPROSS_ID,;
  *      SYDOBJCT.CBASEWIND,SYUDTASK.CAPP_ID  , SYUDTASK.CCOMP_ID ,;
  *      SUBSTR(SYDOBJCT.CPRGLNAME,1,35)+" "+SYUDTASK.CAPP_ID+"      "+ ;
  *      SYUDTASK.CCOMP_ID AS "NAME";
  *FROM  (gcSysHome+"SYUDTASK"),(gcSysHome+"SYDOBJCT"); 
  *WHERE SYUDTASK.CPROSS_ID = SYDOBJCT.CAPOBJNAM AND ;
  *      SYUDTASK.CUSER_ID  = gcUser_ID AND ;
  *       SYUDTASK.CPROCTYPE = 'P';
  *UNION ;
  * SELECT SYUDTASK.CPRIORTY,SYUDTASK.CPROCTYPE, SYUDTASK.CPROSS_ID,;
  *        SPACE(10) ,SYUDTASK.CAPP_ID,SYUDTASK.CCOMP_ID ,;
  *        SUBSTR(SYDREPRT.CREP_NAME,1,35)+" "+SYUDTASK.CAPP_ID+"      "+ ;
  *        SYUDTASK.CCOMP_ID AS "NAME"; 
  *  FROM  (gcSysHome+"SYUDTASK"),(gcSysHome+"SYDREPRT");
  *  WHERE SYUDTASK.CPROSS_ID = SYDREPRT.CREP_ID AND ;
  *        SYUDTASK.CUSER_ID  = gcUser_ID AND ;
  *        SYUDTASK.CPROCTYPE = 'R';
  *  INTO  ARRAY  gaUsr_task ;      
  *  ORDER BY 1 
  SELECT SYUDTASK.CPRIORTY ,SYUDTASK.CPROCTYPE, SYUDTASK.CPROSS_ID,;
         SYDOBJCT.CBASEWIND,SYUDTASK.CAPP_ID  , SYUDTASK.CCOMP_ID ,;
         SUBSTR(SYDOBJCT.CPRGLNAME,1,35)+" "+SYUDTASK.CAPP_ID+"      "+ ;
         SYUDTASK.CCOMP_ID AS "NAME";
   FROM  (gcSysHome+"SYUDTASK"),(gcSysHome+"SYDOBJCT"); 
   WHERE SYUDTASK.CPROSS_ID = SYDOBJCT.CAPOBJNAM AND ;
         SYUDTASK.CUSER_ID  = gcUser_ID AND ;
         SYUDTASK.CPROCTYPE = 'P';
   UNION ;
   SELECT SYUDTASK.CPRIORTY,SYUDTASK.CPROCTYPE, SYUDTASK.CPROSS_ID,;
         SPACE(10) ,SYUDTASK.CAPP_ID,SYUDTASK.CCOMP_ID ,;
         SUBSTR(SYDREPRT.CREP_NAME,1,35)+" "+SYUDTASK.CAPP_ID+"      "+ ;
         SYUDTASK.CCOMP_ID AS "NAME"; 
   FROM  (gcSysHome+"SYUDTASK"),(gcSysHome+"SYDREPRT");
   WHERE SYUDTASK.CPROSS_ID = SYDREPRT.CREP_ID AND ;
         SYUDTASK.CUSER_ID  = gcUser_ID AND ;
         SYDREPRT.cVer     != "A40" AND ;
         SYUDTASK.CPROCTYPE = 'R';
   INTO  ARRAY  gaUsr_task ;      
   ORDER BY 1 
   *E038033,1 HMA  [END]

  IF lcFromMenu = "NO"
    IF _TALLY = 0 
      glUser_Tsk = .F.
      =gfMarkBars()
      RETURN
    ENDIF
  ENDIF
ENDIF

*** Declare all the arrays needed in this program ***
*** array  to hold the  available application and ***
*** another array to hold the companies...
DECLARE laApp[1,2],laComp[1,2]
STORE " " TO laApp ,lcApp_Id,lcApp_Desc
STORE " " TO laComp,lcCom_Id,lcCom_Desc

*** Get 2 names for the 2 temp. files , one ***
*** to hold the programs and another one to ***
*** hold the reports...
lcPrg_Tmp = gfTempName()
lcRep_Tmp = gfTempName()

*** Define the main popup that hold ***
*** all the tasks in the program...
DEFINE POPUP puUsr_task;
       MARGIN  MARK "" MOVER  RELATIVE SCROLL

=gfFillPop('puUsr_task','gaUsr_task',7)

*** Flag to prove that I went through ***
*** the adding of new task...
llFrsTime  = .T.

lsUsr_task = 1
lcNextPrg  = " "+gaUsr_task[lsUsr_task,7]

DO SYDTASK.SPR

*** Arrange the array after coming from the main screen ***
=gfPopArang('puUsr_task',@gaUsr_task)

RELEASE POPUP puUsr_task

IF glUser_Tsk .AND. lsUsr_task > 0
  lcType     = gaUsr_task[lsUsr_task,2]
  lcProgram  = ALLTRIM(gaUsr_task[lsUsr_task,3])
  lcWindow   = ALLTRIM(gaUsr_task[lsUsr_task,4])
  lcPrgModul = gaUsr_task[lsUsr_task,5]
  lcNewComp  = gaUsr_task[lsUsr_task,6]   
  
  =ADEL(gaUsr_task,lsUsr_task)
  
  IF ALEN(gaUsr_task,1) > 1
    DECLARE gaUsr_task[ALEN(gaUsr_task,1)-1,ALEN(gaUsr_task,2)]
  ELSE
    glUser_Tsk = .F.
    =gfMarkBars()
  ENDIF

  IF lcNewComp <> gcAct_Comp
    =gpChangCom (lcNewComp,.F.) 
  ENDIF

  IF lcPrgModul <> gcAct_appl 
    =gpChngModl(lcPrgModul)
  ENDIF 


  CLEAR TYPEAHEAD
  
  DO CASE
    CASE lcType = 'P'
      DO gpDoProg WITH lcWindow,'',lcPrgModul
    CASE lcType = 'R'
      DO gpRpPrint WITH lcProgram
  ENDCASE    

ENDIF

*** Close both the temp files after coming *** 
*** from the main screen & erase them from ***
*** the disk...
IF USED(lcPrg_Tmp)
  USE IN ALIAS(lcPrg_Tmp)
ENDIF

ERASE (gcWorkDir+lcPrg_Tmp+".DBF")

IF USED(lcRep_Tmp)
  USE IN ALIAS(lcRep_Tmp)
ENDIF
ERASE (gcWorkDir+lcRep_Tmp+".DBF")

*!**************************************************************************
*!                        SYDTASK.SPR     FUNCTIONS
*!**************************************************************************

*!*************************************************************************
*!
*!      Function : lfwUsr_Tsk
*!
*!*************************************************************************
*
FUNCTION lfwUsr_Tsk

lcNextPrg = " "+PRMBAR('puUsr_task',GETBAR('puUsr_task',lsUsr_task))

SHOW GET lcNextPrg

*!*************************************************************************
*!
*!      Function : lfvNew
*!
*!*************************************************************************
*
FUNCTION lfvNew

lcSelCont  = SCHEME(1,6) 
lcEnbCont  = SCHEME(1,2) 
lcDisCont  = SCHEME(1,10) 


IF llFrsTime
  llFrsTime = .F.
  IF glLog_Requ
    *** If the current user is an operator. ***

    *** Get all the available modules. ***
    SELECT DISTINCT SYDAPPL.cApp_name,SYUUSRPR.cApp_Id ;
           FROM (gcSysHome+"SYUUSRPR"),(gcSysHome+"SYDAPPL") ;
           WHERE ALLTRIM(SYUUSRPR.cUser_Id) = ALLTRIM(gcUser_ID) .AND. ;
                 SYUUSRPR.cApp_Id = SYDAPPL.cApp_Id ;
           INTO ARRAY laApp ;
           ORDER BY SYDAPPL.cApp_Name
  
    IF _TALLY > 0
      *** Default the application ID to the first ***
      *** element of the application array...
      lcApp_Id   = laApp[1,2]
      DO CASE
        CASE _DOS
          lcApp_Desc = laApp[1,1]
        CASE _WINDOWS
          ibPopApp   = 1
      ENDCASE
    ENDIF
  
    *** Get all the available companies. ***
    SELECT DISTINCT SYCCOMP.cCom_name,SYUUSRPR.cComp_Id ;
           FROM (gcSysHome+"SYUUSRPR"),(gcSysHome+"SYCCOMP") ;
           WHERE ALLTRIM(SYUUSRPR.cUser_Id) = ALLTRIM(gcUser_ID) .AND. ;
                 SYUUSRPR.cComp_Id = SYCCOMP.cComp_Id ;
           INTO ARRAY laComp

    IF _TALLY > 0
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

    *** Collect the programs that current user have its rights. ***
    SELECT DISTINCT SYDOBJCT.cApp_Id,SYDOBJCT.cApobjnam,;
           SYDOBJCT.cPrglname,SYDOBJCT.cBasewind ;
           FROM  (gcSysHome+"SYDOBJCT"),(gcSysHome+"SYUUSRPR") ;
           WHERE SYDOBJCT.cApobjtyp = "P" .AND. ;
                 SYUUSRPR.cProcType = "P" .AND. ;
                 SYDOBJCT.cApp_Id   = SYUUSRPR.cApp_Id .AND. ;
                 SYDOBJCT.cApobjnam = SYUUSRPR.cPross_Id ;
           INTO DBF (gcWorkDir+lcPrg_Tmp)

    SELECT (lcPrg_Tmp)
    SET FILTER TO cApp_Id = lcApp_Id

    *** Collect the reports that current user have its rights. ***

   *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]

   *SELECT DISTINCT SYDREPRT.cRep_Id,SYDREPRT.cRep_name,SYDREPRT.cApp_Id ;
   *       FROM (gcSysHome+"SYDREPRT"),(gcSysHome+"SYUUSRPR") ;
   *       WHERE SYDREPRT.cApobjtyp = "R" .AND. ;
   *             SYUUSRPR.cProcType = "R" .AND. ;
   *             SYDREPRT.cApp_Id   = SYUUSRPR.cApp_Id   .AND. ;
   *             SYDREPRT.cRep_Id   = SYUUSRPR.cPross_Id .AND. ;
   *             SYDREPRT.cMainentr = "Y" ;
   *       INTO DBF (gcWorkDir+lcRep_Tmp)
   SELECT DISTINCT SYDREPRT.cRep_Id,SYDREPRT.cRep_name,SYDREPRT.cApp_Id ;
          FROM (gcSysHome+"SYDREPRT"),(gcSysHome+"SYUUSRPR") ;
          WHERE SYDREPRT.cApobjtyp = "R" .AND. ;
                SYDREPRT.cVer     != "A40" .AND. ;
                SYUUSRPR.cProcType = "R" .AND. ;
                SYDREPRT.cApp_Id   = SYUUSRPR.cApp_Id   .AND. ;
                SYDREPRT.cRep_Id   = SYUUSRPR.cPross_Id .AND. ;
                SYDREPRT.cMainentr = "Y" ;
          INTO DBF (gcWorkDir+lcRep_Tmp)

   *E038033,1 HMA [END]

    SELECT (lcRep_Tmp)
    SET FILTER TO cApp_Id = lcApp_Id

  ********************************************************************************
  ELSE
    *** If the current user is an Administrator. ***

    *** Get all the available modules. ***
    SELECT cApp_name,cApp_Id ;
           FROM  (gcSysHome+"SYDAPPL") ;
           WHERE SYDAPPL.cApp_Id <> "SY" ;
           INTO  ARRAY laApp

    IF _TALLY > 0
      *** Default the application ID to the first ***
      *** element of the application array...
      lcApp_Id   = laApp[1,2]
      DO CASE
        CASE _DOS
          lcApp_Desc = laApp[1,1]
        CASE _WINDOWS
          ibPopApp   = 1
      ENDCASE
    ENDIF

    *** Get all the available companies. ***
    SELECT cCom_name,cComp_Id ;
           FROM (gcSysHome+"SYCCOMP") ;
           INTO ARRAY laComp
  
    IF _TALLY > 0
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
    
    *** Collect the programs that current user have its rights. ***
    SELECT cApp_Id,cApobjnam,cPrglname,cBasewind ;
           FROM  (gcSysHome+"SYDOBJCT") ;
           WHERE SYDOBJCT.cApobjtyp = "P" ;
           INTO DBF (gcWorkDir+lcPrg_Tmp)

    SELECT (lcPrg_Tmp)
    SET FILTER TO cApp_Id = lcApp_Id

    *** Collect the reports that current user have its rights. ***
 
    *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]
 
    * SELECT cRep_Id,cRep_name,cApp_Id ;
    *      FROM (gcSysHome+"SYDREPRT") ;
    *      WHERE cApobjtyp = "R" .AND. cMainentr = "Y" ;
    *      INTO DBF (gcWorkDir+lcRep_Tmp)
    SELECT cRep_Id,cRep_name,cApp_Id ;
           FROM (gcSysHome+"SYDREPRT") ;
           WHERE cApobjtyp = "R" .AND. cMainentr = "Y" ;
           .AND. SYDREPRT.cVer != "A40" ;
           INTO DBF (gcWorkDir+lcRep_Tmp)

    *E038033,1 HMA [END]

    SELECT (lcRep_Tmp)
    SET FILTER TO cApp_Id = lcApp_Id

  ENDIF
ENDIF

*** Save the main array that hold all the tasks manager. ***
DECLARE laTempAry[ALEN(gaUsr_task,1),ALEN(gaUsr_task,2)]
=ACOPY(gaUsr_task,laTempAry)

lnOldTyp   = 1
rbProcTyp  = 1
lsAllProc  = 1
lsSelProc  = 1
lcAllProc = "&lcPrg_Tmp..cPrglname"

SELECT (lcPrg_Tmp)

DEFINE POPUP puSelProc;
       MARGIN  MARK "" MOVER  RELATIVE SCROLL

=gfFillPop('puSelProc','gaUsr_task',7)

DO NEWTASK.SPR

*** Arrange the array after coming from the main screen ***
=gfPopArang('puSelProc',@gaUsr_task)

RELEASE POPUP puSelProc

HIDE POPUP puUsr_task SAVE
=gfFillPop('puUsr_task','gaUsr_task',7)
SHOW POPUP puUsr_task


lsUsr_task = 1

SHOW GET lsUsr_task

*!*************************************************************************
*!
*!      Function : lfvRemove
*!
*!*************************************************************************
*
FUNCTION lfvRemove

=ADEL(gaUsr_task,lsUsr_task)

IF ALEN(gaUsr_task,1) > 1
  DECLARE gaUsr_task[ALEN(gaUsr_task,1)-1,ALEN(gaUsr_task,2)]
  HIDE POPUP puUsr_task SAVE
  =gfFillPop('puUsr_task','gaUsr_task',7)
  SHOW POPUP puUsr_task
  lsUsr_task = IIF(lsUsr_task < ALEN(gaUsr_task,1),lsUsr_task,1)
  SHOW GET   lsUsr_task
ELSE
  glUser_Tsk = .F.
  =gfMarkBars()  
  CLEAR READ
ENDIF

*!**************************************************************************
*!
*!      Function: lfvSavChng
*!
*!**************************************************************************
*
FUNCTION lfvSavChng

*** Arrange the array after coming from the main screen ***
=gfPopArang('puUsr_task',@gaUsr_task)

SELECT SYUDTASK
SET ORDER TO TAG USRPRIORTY
IF SEEK(gcUser_ID)
  SCAN FOR cUser_ID = gcUser_ID
    SCATTER MEMVAR MEMO BLANK
    GATHER  MEMVAR MEMO 
    DELETE
  ENDSCAN
ENDIF

SET DELETE OFF
lcTime     = gfGetTime()
lnAllTAsk  = ALEN(gaUsr_task,1)

FOR lnAryCount  = 1 TO lnAllTAsk
  =gfThermo(lnAllTAsk,lnAryCount,"Saving task :",;
            ALLTRIM(SUBSTR(gaUsr_task[lnAryCount,7],1,35)))
  IF SEEK(' ') 
    RECALL
  ELSE
    APPEND BLANK
  ENDIF
  REPLACE cUser_ID  WITH gcUser_ID              ;
          cPriorty  WITH PADL(lnAryCount,3,'0') ;
          cApp_ID   WITH gaUsr_task[lnAryCount,5];
          cComp_ID  WITH gaUsr_task[lnAryCount,6];
          cPross_ID WITH gaUsr_task[lnAryCount,3];
          cProcType WITH gaUsr_task[lnAryCount,2];
          cAdd_User WITH gcUser_ID              ; 
          cAdd_Time WITH lcTime                 ;
          dAdd_Date WITH DATE()
ENDFOR
SET DELETE ON


*!**************************************************************************
*!
*!      Function: lfvRestart
*!
*!**************************************************************************
*
FUNCTION lfvRestart

gaUsr_task = " "
glUser_Tsk = .T.
lsUsr_task = 0
=gfMarkBars()
CLEAR READ
RETURN      


*!**************************************************************************
*!                        NEWTASK.SPR     FUNCTIONS
*!**************************************************************************


*!**************************************************************************
*!
*!      Function: lfvProcTyp
*!
*!**************************************************************************
*
*** Valid function of the radio button " (.) Programs  ( ) Reports "

FUNCTION lfvProcTyp

IF lnOldTyp <> rbProcTyp
  IF rbProcTyp = 1
    SELECT (lcPrg_Tmp)
    lcAllProc = "&lcPrg_Tmp..cPrglname"
  ELSE
    SELECT (lcRep_Tmp)
    lcAllProc = "&lcRep_Tmp..cRep_name"
  ENDIF
ENDIF

SHOW GETS

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
    lcApp_Id = gfActPop(2,27,8,74,'laApp',2,1,@lcApp_Desc)
    SHOW GETS
  CASE _WINDOWS
    lcApp_Id = laApp[ibPopApp,2]
    SHOW GET ibPopApp
ENDCASE

*** If the module is changed. ***
IF lcOldApp <> lcApp_Id
  SHOW GET lsAllProc 
ENDIF

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
    lcCom_Id = gfActPop(4,27,10,57,'laComp',2,1,@lcCom_Desc)
    SHOW GETS
  CASE _WINDOWS
    lcCom_Id = laComp[ibPopComp,2]
    SHOW GET ibPopComp
ENDCASE

*** If the company is changed. ***
IF lcOldComp <> lcCom_Id
  SHOW GET lsAllProc  
ENDIF

*!**************************************************************************
*!
*!      Function: lfvMov
*!
*!**************************************************************************
*
*** Valid funcyion of the push button < move >...

FUNCTION lfvMov

IF !EMPTY(gaUsr_task[1,3]) 
  lnNewDim = ALEN(gaUsr_task,1)+1
  DIMENSION gaUsr_task[lnNewDim,ALEN(gaUsr_task,2)]
ELSE
  lnNewDim = 1
ENDIF  

*=AINS(gaUsr_task,GETBAR('puSelProc',lsSelProc))

IF rbProcTyp = 1
  SELECT (lcPrg_Tmp)
  gaUsr_task[lnNewDim,1] = " "
  gaUsr_task[lnNewDim,2] = "P"
  gaUsr_task[lnNewDim,3] = &lcPrg_Tmp..cApobjnam
  gaUsr_task[lnNewDim,4] = &lcPrg_Tmp..cBaseWind
  gaUsr_task[lnNewDim,5] = lcApp_Id
  gaUsr_task[lnNewDim,6] = lcCom_Id
  gaUsr_task[lnNewDim,7] = ;
         SUBSTR(&lcPrg_Tmp..cPrglname,1,35)+" "+lcApp_Id+"      "+lcCom_Id
ELSE
  SELECT (lcRep_Tmp)
  gaUsr_task[lnNewDim,1] = " "
  gaUsr_task[lnNewDim,2] = "R"
  gaUsr_task[lnNewDim,3] = &lcRep_Tmp..cRep_Id
  gaUsr_task[lnNewDim,4] = SPACE(10)
  gaUsr_task[lnNewDim,5] = lcApp_Id
  gaUsr_task[lnNewDim,6] = lcCom_Id
  gaUsr_task[lnNewDim,7] = ;
          SUBSTR(&lcRep_Tmp..cRep_name,1,35)+" "+lcApp_Id+"      "+lcCom_Id
ENDIF

HIDE POPUP puSelProc SAVE
=gfFillPop('puSelProc','gaUsr_task',7)
SHOW POPUP puSelProc


SHOW GET lsSelProc

*!**************************************************************************
*!
*!      Function: lfvMovAll
*!
*!**************************************************************************
*
*** Valid funcyion of the push button < move all >...

FUNCTION lfvMovAll

IF rbProcTyp = 1
  SELECT (lcPrg_Tmp)
ELSE
  SELECT (lcRep_Tmp)
ENDIF

DIMENSION gaUsr_task[1,ALEN(gaUsr_task,2)]
gaUsr_task = " "

SCAN
  IF !EMPTY(gaUsr_task[1,1])
    DIMENSION gaUsr_task[ALEN(gaUsr_task,1)+1,ALEN(gaUsr_task,2)]
    =AINS(gaUsr_task,ALEN(gaUsr_task,1))
  ENDIF
  gaUsr_task[ALEN(gaUsr_task,1),1] = " "
  gaUsr_task[ALEN(gaUsr_task,1),2] = IIF(rbProcTyp = 1,"P","R")
  gaUsr_task[ALEN(gaUsr_task,1),3] = IIF(rbProcTyp = 1,;
                         &lcPrg_Tmp..cApobjnam,&lcRep_Tmp..cRep_Id)
  gaUsr_task[ALEN(gaUsr_task,1),4] = IIF(rbProcTyp = 1,;
                         &lcPrg_Tmp..cBaseWind,SPACE(10))
  gaUsr_task[ALEN(gaUsr_task,1),5] = lcApp_Id
  gaUsr_task[ALEN(gaUsr_task,1),6] = lcCom_Id
  gaUsr_task[ALEN(gaUsr_task,1),7] = IIF(rbProcTyp = 1,;
       SUBSTR(&lcPrg_Tmp..cPrglname,1,35)+" "+lcApp_Id+"      "+lcCom_Id,;
       SUBSTR(&lcRep_Tmp..cRep_name,1,35)+" "+lcApp_Id+"      "+lcCom_Id)
         
ENDSCAN

HIDE POPUP puSelProc SAVE
=gfFillPop('puSelProc','gaUsr_task',7)
SHOW POPUP puSelProc

SHOW GET lsSelProc

*!**************************************************************************
*!
*!      Function: lfvRem
*!
*!**************************************************************************
*
*** Valid funcyion of the push button < Remove >...

FUNCTION lfvRem

IF ALEN(gaUsr_task,1) <> 1
  =ADEL(gaUsr_task,GETBAR('puSelProc',lsSelProc)) 
  DIMENSION gaUsr_task [ALEN(gaUsr_task,1)-1,ALEN(gaUsr_task,2)]
ELSE
  gaUsr_task = ' '  
ENDIF
HIDE POPUP puSelProc
=gfFillPop('puSelProc','gaUsr_task',7)
SHOW POPUP puSelProc

SHOW GET lsSelProc

*!**************************************************************************
*!
*!      Function: lfvRemAll
*!
*!**************************************************************************
*
*** Valid funcyion of the push button < Remove All >...

FUNCTION lfvRemAll

DECLARE gaUsr_task[1,ALEN(gaUsr_task,2)]
gaUsr_task = " "

HIDE POPUP puSelProc SAVE
=gfFillPop('puSelProc','gaUsr_task',7)
SHOW POPUP puSelProc

SHOW GET lsSelProc

*!**************************************************************************
*!
*!      Function: lfvOk
*!
*!**************************************************************************
*
*** Valid funcyion of the push button ok...

FUNCTION lfvOk

*** Arrange the array after coming from the main screen ***
=gfPopArang('puSelProc',@gaUsr_task)

FOR lnCount = 1 TO ALEN(gaUsr_task,1)
  gaUsr_task[lnCount,1] = ALLTRIM(STR(lnCount))
ENDFOR

*!**************************************************************************
*!
*!      Function: lfvCan
*!
*!**************************************************************************
*
*** Valid funcyion of the push button cancel...

FUNCTION lfvCan

DECLARE gaUsr_task[ALEN(laTempAry,1),ALEN(laTempAry,2)]

=ACOPY(laTempAry,gaUsr_task)
