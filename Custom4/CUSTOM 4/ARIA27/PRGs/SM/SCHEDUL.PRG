*:************************************************************************
*:
*: Procedure file: SCHEDUL.PRG
*:               : Schedule
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Aria Apparel System
*:           Date: April,24 1996
*:         Author: Reham Aly Al-Allamy
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:                 
*:      Documented  /  /
*:************************************************************************
*C200027,4 V.I.P.
*C200027,4 When you arrange the schedule screen, the objects has to be in
*C200027,4 the following order : ibDummi1 , ibDummi2 , the rest objects.
*B800704,1 M.H 08/08/96 Initialize the parameters is the program is stand allon.
*C200027,8 M.H 09/02/96 Display the task on the schedule list according to the priority.
*E300711,1 HESHAM 07/28/97
*E300711,1 control the buttons the use to Manepulate data using the user
*E300711,1 priviliges
*B602697,1 Reham On 03/25/99
*B602697,1 1- Use the equal sign "=" instead of variable lcHzLine because of changing 
*B602697,1 the TODO list to use MS Sans Serif font.
*:************************************************************************
*:  Modifications :
*:************************************************************************
*
PARAMETERS lcContType , lcContId , lcStore
PRIVATE lcContId


*B800704,1 M.H Begin.
IF TYPE('lcContType') <> 'C'
*  lcContType = 'C'
   lcContType = ' '
ENDIF

IF TYPE('lcContId') <> 'C'
  lcContId = SPACE(5)
ENDIF


IF TYPE('lcStore') <> 'C'
  lcStore = SPACE(8)
ENDIF

STORE .T. TO llHid1 , llHid2 , llHid3 , llHid4 , llHid5 , llDontHid,llNoContrl
STORE 0 TO lsToDo,lsTasks,lsTsk1 , lsTsk2 , lsTsk3 , lsTsk4 , lsTsk5,;
           lnActFolder,lnCalRStr,lnCalCStr
DECLARE laToDo[1,21],laScdTask[1,21],laTasks[48],laScdWkTsk[1,7],laCalnd[6,7],;
        laFoldWinds[2,2],laFileName[1,1]
STORE ' ' TO lcWinCh1,lcWinCh2,lcOldUsr,lcUser_id,laToDo,lcUser_id,lcWklUsr,;
             ldStart,ldCurDate,lcDayStr,lcDateExp,ldBgMonth,ldEnMonth,llCalRfrsh,;
             lcWklCont,lcWklSub,lcEdtStat,lcFolder,lcWindTitl,lcWinCh3,lcCalender,;
             lcWCalnder
STORE .F. TO llDeact,lcWindOnTop
*B800704,1 M.H End.

IF !gfSetup()
  EXIT
ENDIF

lcSavDate = SET('DATE')
SET DATE TO AMERICAN

*C200027,4 Store the directories of all the bmps.
lcBmp1 = gcBmpHome + "EDIT1.BMP"
lcBmp2 = gcBmpHome + "CALLS1.BMP"
lcBmp3 = gcBmpHome + "APPOINT.BMP"
lcBmp4 = gcBmpHome + "TODO1.BMP"
lcBmp5 = gcBmpHome + "CLS.BMP"
lcBmp6 = gcBmpHome + "WEEKPRV.BMP"
lcBmp7 = gcBmpHome + "WEEKNXT.BMP"

*C200027,4 Array hold the screen windows.
DECLARE laSelWind[2,2]
laSelWind[1,1] = "Daily schedule"
laSelWind[1,2] = 1
laSelWind[2,1] = "Weekly schedule"
laSelWind[2,2] = 2
lcSelWind      = laSelWind[1,1]
lnWhichWind    = laSelWind[1,2]

*C200027,4 Flag to know that I'm running the screen for first time.
llRun1Tim = .T.

*C200027,4 You cannot run this program if there is no users in the user file.
IF EMPTY(gcUser_ID)
  =gfDialog("I","You cannot use the calendar if you are logged in with no user ID.")  
  RETURN
ENDIF

lcStore = IIF(EMPTY(lcStore) , SPACE(8) , lcStore)

*:************************************************************************

*C200027,4 Open the resources file.
*llResOpn = .F.
*IF !USED("RESOURCE")
*  =gfOpenFile(QDD+'RESOURCE',QDD+'RESOURCE','SH')
*  llResOpn = .T.
*ENDIF

*C200027,4 Open the code file.
*llCodOpn = .F.
*IF !USED("CODE")
*  =gfOpenFile(QDD+'CODE',QDD+'CODE','SH')
*  llCodOpn = .T.
*ENDIF

*C200027,4 Open the CONTACT file.
llConOpn = .F.
IF !USED("CONTACT")
  =gfOpenFile(gcDataDir+'CONTACT',gcDataDir+'CONTACT','SH')
  llConOpn = .T.
ENDIF

*C200027,4 Open the users file.
llUsrOpn = .F.
IF !USED("SYUUSER")
  =gfOpenFile(gcSysHome+'SYUUSER',gcSysHome+'SYUUSER','SH')
  llUsrOpn = .T.
ENDIF
*C200027,4 Declare array to hold the users Ids.
DECLARE laUsers[1]
*C200027,4 Select the available users from the users file.
SELECT SYUUSER.cUser_Id FROM (gcSysHome+"SYUUSER ") INTO ARRAY laUsers

IF EMPTY(laUsers[1])
  =gfDialog("I","You cannot use the calendar if you are logged in with no user ID.")  
  RETURN
ENDIF

*C200027,4 Open the SYSCHDUL file with the account index.
llScdOpn = .F.
IF !USED("SYSCHDUL")
*  =gfOpenFile(gcSysHome+'SYSCHDUL',gcSysHome+'SCHUSER','SH')
  USE (gcSysHome+'SYSCHDUL') AGAIN IN 0 ORDER TAG SCHUSER
  llScdOpn = .T.
ELSE
  SELECT SYSCHDUL
  SET ORDER TO TAG SCHUSER
ENDIF

puUsers  = 1

*C200027,4 Variable hold the list texture.
lcMarker   = " "
lcHzLine   = "Ä"
lcUpLeft   = "Ú"
lcVrLine   = "³"
lcDnLeft   = "À"
lcHfBloc   = "°"
IF !WEXIST(gcBaseWind)
  STORE .T. TO llHid1 , llHid2 , llHid3 , llHid4 , llHid5 , llDontHid
  STORE 1 TO lsTasks,lsToDo,lsTsk1 , lsTsk2 , lsTsk3 , lsTsk4 , lsTsk5,;
             lnActFolder
  STORE gcUser_ID TO lcUser_id,lcWklUsr
  lcFolder = gfTempName()  
  lcWinCh1 = gfTempName()  
  lcWinCh2 = gfTempName()
  lcWinCh3 = gfTempName()  
  lcCalender = lcWinCh1 
  ldCurDate = DATE()
  lcDayStr  = PADC(CDOW(ldCurDate) , 9  , " " )  
  ldBgMonth  = (ldCurDate - DAY(ldCurDate) + 1)
  ldEnMonth  = (ldBgMonth - DAY(ldBgMonth) + 32)
  ldEnMonth  = (ldEnMonth - DAY(ldEnMonth))
  lcDateExp = PADC(MDY(ldCurDate), IIF(_DOS, 19, 10))
  ldStart = DATE() - (DOW(DATE())-2)
  llCalRfrsh = .T.
  lcEdtStat = "DISABLE"
  lnFrsTim = 12
  lcAM_PM  = " AM"
  FOR lnCount = 1 TO 48 STEP 2
    laTasks[lnCount]   = PADL(ALLTRIM(STR(lnFrsTim,2)),2,"0") + ":00" + lcAM_PM
    laTasks[lnCount+1] = PADL(ALLTRIM(STR(lnFrsTim,2)),2,"0") + ":30" + lcAM_PM
    lnFrsTim = IIF(lnFrsTim + 1 > 12 , 1 , lnFrsTim + 1)
    lcAM_PM  = IIF(lnCount <= 21 , " AM" , " PM")
  ENDFOR
ENDIF


laFoldWinds[1,1] = 'Daily schedule'
laFoldWinds[1,2] = lcWinCh1
laFoldWinds[2,1] = 'Weekly schedule'
laFoldWinds[2,2] = lcWinCh2
lcFoldPrnt       = gcBaseWind
*lnFolderCSt        = 3.125 &&IIF(SCOLS()>80,13.125,3.125)
lnFolderCEnd       = 98.000 &&97.750
*lnFolderRSt        = 3.000 &&IIF(SCOLS()>80,8.000,3.000)
lnfolderrend       = 2.00 &&1.500
lcWFoldChng = '=lfvSelWind()'
*lnFolderREnd       = 2.583
lnCalRStr = 7.00
lnCalCStr = 5.50
lcWCalnder=  'lfObjRef()'
*C200027,4 Define the list popup.
DEFINE POPUP puTasks SCROLL 
DEFINE BAR 1 OF puTasks PROMPT " "



*C200027,4 Define the list popups in the weekly schedule screen.
DEFINE POPUP puTsk1 SCROLL 
DEFINE BAR 1 OF puTsk1 PROMPT " "
DEFINE POPUP puTsk2 SCROLL 
DEFINE BAR 1 OF puTsk2 PROMPT " "
DEFINE POPUP puTsk3 SCROLL 
DEFINE BAR 1 OF puTsk3 PROMPT " "
DEFINE POPUP puTsk4 SCROLL 
DEFINE BAR 1 OF puTsk4 PROMPT " "
DEFINE POPUP puTsk5 SCROLL 
DEFINE BAR 1 OF puTsk5 PROMPT " "


*:************************************************************************

STORE 0 TO Z , Y , lnOldz , lnOldy , lnNewz , lnNewy

puUserId  = ASCAN(laUsers,lcUser_id) 
puWklUsr  = ASCAN(laUsers,lcUser_id) 
puDUserId = ASCAN(laUsers,lcUser_id) 
IF PUUSERID = 0
  STORE 1 TO PUUSERID,puWklUsr,puDUserId 
  lcUser_ID = laUsers[1]
ENDIF
*C200027,4 Call the schedule screen.
DO (gcScrDir+gcWinAppl+'\SCHEDUL.SPX')

IF llDeact AND !EMPTY(lcWindOnTop)
  ACTIVATE WINDOW (lcWindOnTop) TOP
ENDIF

SET DATE TO &lcSavDate

RELEASE POPUP puTasks

*C200027,4 Close the opened files.
IF llUsrOpn .AND. USED("SYUUSER")
  USE IN SYUUSER
ENDIF

IF llScdOpn .AND. USED("SYSCHDUL")
  USE IN SYSCHDUL
ENDIF

IF llConOpn .AND. USED("CONTACT")
  USE IN CONTACT
ENDIF

*IF llCodOpn .AND. USED("CODE")
*  USE IN CODE
*ENDIF

*IF llResOpn .AND. USED("RESOURCE")
*  USE IN RESOURCE
*ENDIF

*!***********************************************************************
*! Name      : lfObjRef
*! Developer : Reham Al-Allamy
*! Date      : 04/25/1996
*! Purpose   : Function called from the execution of the calendar to 
*!             refresh the other objects related to the changed date.
*!***********************************************************************
*! Example   :  =lfObjRef()
*!***********************************************************************
*
FUNCTION lfObjRef

*C200027,4 Refresh the date charcter & the date expression.
lcDayStr  = PADC(CDOW(ldCurDate) , 9  , " " )
lcDateExp = PADC(MDY(ldCurDate)  , 19 , " " )

*C200027,4 Call function to refresh the says fields.
=lfScdRef()

*C200027,4 Call function to do the following for the daily schedule :
*C200027,4 1- Select the data from the schedule file.
*C200027,4 2- Draw & build the list from the prepared array.
=lfGetDlTsk()

*C200027,4 Call function to get the scheduled todo.
=lfSelDaly2()

*!***********************************************************************
*! Name      : lfGetDlTsk
*! Developer : Reham Al-Allamy
*! Date      : 05/02/1996
*! Purpose   : Function called with every change happend with the
*!             calendar date or with the user Id (Daily schedule).
*!***********************************************************************
*! Calls     : =lfSelDaly1() -> Select the data from the schedule file.
*!           : =lfBldDlBrs()  -> Draw & build the list from the prepared array.
*!***********************************************************************
*! Example   :  =lfGetDlTsk()
*!***********************************************************************
*
FUNCTION lfGetDlTsk

*C200027,4 Call function to select the data from the schedule file.
=lfSelDaly1()

*C200027,4 Draw & build the list from the prepared array.
=lfBldDlBrs()

*!***********************************************************************
*! Name      : lfSelDaly1
*! Developer : Reham Al-Allamy
*! Date      : 05/02/1996
*! Purpose   : Function called at the beginning of the program to
*!             select the current users tasks for current account.
*!***********************************************************************
*! Example   :  =lfSelDaly1()
*!***********************************************************************
*
FUNCTION lfSelDaly1

*C200027,4 Array hold the selected tasks from the schedule file.
DECLARE laScdTask[1,21]
STORE " " TO laScdTask

*C200027,4 Select the open & completed scheduled tasks for the current 
*C200027,4 user and current date (Appointments - Calls)
*C200027,8 M.H Begin.
*SELECT cseqnumber , user_id , cconttype , ccont_id , store ,;
       contact , cphone , csubject , ctrantype , dtrandate ,;
       ctrantime , nestdur , ctranreson , mnotes , ccompleted ,;
       cresultcd , ccmpltuser , dcmpltdate , nactdur ;
  FROM (gcSysHome+"SYSCHDUL") ;
 WHERE USER_ID+DTOS(DTRANDATE)+CCOMPLETED+CCONTTYPE+CCONT_ID+STORE+CTRANTIME+CSEQNUMBER = ;
       lcUser_Id+DTOS(ldCurDate) .AND. ;
       (cTranType = "C" .OR. cTranType = "A") ;
 ORDER BY CTRANTIME , CSEQNUMBER ;
  INTO ARRAY laScdTask

SELECT cseqnumber,cUser_ID,cconttype,ccont_id,store,;
       contact,cphone,csubject,ctrantype,dtrandate,;
       ctrantime,nestdur,ctranreson,mnotes,ccompleted,;
       cresultcd,ccmpltuser,dcmpltdate,nactdur,CPRIORITY,cComp_ID;
  FROM (gcSysHome+"SYSCHDUL");
 WHERE cUser_ID+DTOS(DTRANDATE)+CCOMPLETED+CCONTTYPE+CCONT_ID+STORE+CTRANTIME+CSEQNUMBER = ;
       lcUser_Id+DTOS(ldCurDate) .AND.       ;
       (cTranType = "C" .OR. cTranType = "A") ;
 ORDER BY CTRANTIME , CPRIORITY , CSEQNUMBER ;
  INTO ARRAY laScdTask
  
*C200027,8 M.H End.

*!***********************************************************************
*! Name      : lfSelDaly2
*! Developer : Reham Al-Allamy
*! Date      : 05/02/1996
*! Purpose   : Function called at the beginning of the program to
*!             select the current users tasks for current account.
*!***********************************************************************
*! Example   :  =lfSelDaly2()
*!***********************************************************************
*
FUNCTION lfSelDaly2

*C200027,4 Array hold the selected tasks from the schedule file.
DECLARE laToDo[1,21]
STORE " " TO laToDo

*C200027,4 Select the open scheduled tasks for the current user and
*C200027,4 current date (ToDo)

*C200027,8 M.H Begin.
*SELECT "("+cTranType+") "+ALLTRIM(SUBSTR(csubject,1,30))+REPLICATE(lcHzLine,18) , ;
       cseqnumber , user_id , cconttype , ccont_id , store ,;
       contact , cphone , csubject , ctrantype , dtrandate ,;
       ctrantime , nestdur , ctranreson , mnotes , ccompleted ,;
       cresultcd , ccmpltuser , dcmpltdate , nactdur ;
  FROM (gcSysHome+"SYSCHDUL") ;
 WHERE USER_ID+DTOS(DTRANDATE)+CCOMPLETED+CCONTTYPE+CCONT_ID+STORE+CTRANTIME+CSEQNUMBER = ;
       lcUser_Id+DTOS(ldCurDate) .AND. cTranType = "T" .AND. cCompleted = "N" ;
 ORDER BY DTRANDATE , CTRANTIME ;
  INTO ARRAY laToDo

*B602697,1 Reham On 03/25/99   *** Begin ***
*B602697,1 Use the equal sign "=" instead of variable lcHzLine because of changing 
*B602697,1 the TODO list to use MS Sans Serif font.
*SELECT "("+cTranType+") "+ALLTRIM(SUBSTR(csubject,1,30))+REPLICATE(lcHzLine,18) , ;
       cseqnumber , cUser_ID , cconttype , ccont_id , store ,;
       contact , cphone , csubject , ctrantype , dtrandate ,;
       ctrantime , nestdur , ctranreson , mnotes , ccompleted ,;
       cresultcd , ccmpltuser , dcmpltdate , nactdur,cComp_ID ;
  FROM (gcSysHome+"SYSCHDUL") ;
 WHERE cUser_ID+DTOS(DTRANDATE)+CCOMPLETED+CCONTTYPE+CCONT_ID+STORE+CTRANTIME+CSEQNUMBER = ;
       lcUser_Id+DTOS(ldCurDate) .AND. cTranType = "T" .AND. cCompleted = "N" ;
 ORDER BY DTRANDATE , CTRANTIME , CPRIORITY ;
  INTO ARRAY laToDo
SELECT "("+cTranType+") "+ALLTRIM(SUBSTR(csubject,1,30))+REPLICATE("=",18) , ;
       cseqnumber , cUser_ID , cconttype , ccont_id , store ,;
       contact , cphone , csubject , ctrantype , dtrandate ,;
       ctrantime , nestdur , ctranreson , mnotes , ccompleted ,;
       cresultcd , ccmpltuser , dcmpltdate , nactdur,cComp_ID ;
  FROM (gcSysHome+"SYSCHDUL") ;
 WHERE cUser_ID+DTOS(DTRANDATE)+CCOMPLETED+CCONTTYPE+CCONT_ID+STORE+CTRANTIME+CSEQNUMBER = ;
       lcUser_Id+DTOS(ldCurDate) .AND. cTranType = "T" .AND. cCompleted = "N" ;
 ORDER BY DTRANDATE , CTRANTIME , CPRIORITY ;
  INTO ARRAY laToDo
*B602697,1 Reham On 03/25/99   *** End   ***
*C200027,8 M.H End.

SHOW GET lsToDo

*!***********************************************************************
*! Name      : lfBldDlBrs
*! Developer : Reham Al-Allamy
*! Date      : 05/02/1996
*! Purpose   : Funcion called at the beginning of the program and
*!             everytime we change the user after calling the select  
*!             data function.  And this function to build the popup
*!             from the array hold the current user schedule...
*!***********************************************************************
*! Example   :  =lfBldDlBrs()
*!***********************************************************************
*
FUNCTION lfBldDlBrs

IF !llDontHid
  HIDE POPUP puTasks SAVE
ENDIF
RELEASE BAR  ALL OF puTasks

FOR lnCount = 1 TO ALEN(laTasks,1)
  DEFINE BAR lnCount OF puTasks PROMPT " "+laTasks[lnCount] + ;
         IIF(BETWEEN(lnCount,19,35)," ",REPLICATE(lcHfBloc,20))
ENDFOR

*C200027,4 Reham On 05/06/96    *** Begin ***
*C200027,4 If there is any tasks for the current user in the 
*C200027,4 current date and have time.
IF !EMPTY(laScdTask[1,1])
  FOR lnCount = 1 TO ALEN(laScdTask,1)
    IF !EMPTY(laScdTask[lnCount,11])
      lcHzLine = IIF( laScdTask[lnCount,15] = "N" , "Ä" , "Í")
      lcUpLeft = IIF( laScdTask[lnCount,15] = "N" , "Ú" , "É")
      lcVrLine = IIF( laScdTask[lnCount,15] = "N" , "³" , "º")
      lcDnLeft = IIF( laScdTask[lnCount,15] = "N" , "À" , "È")
      *C200027,4 Get the bar no. where the current task exist.
      DO CASE
        CASE SUBSTR(laScdTask[lnCount,11],1,2) = "12"
          lnBarOrd = 1 + IIF(SUBSTR(laScdTask[lnCount,11],7,1) = "A" , ;
                     IIF(VAL(SUBSTR(laScdTask[lnCount,11],4,2)) = 30 , 1 , 0 ) , ;
                     IIF(VAL(SUBSTR(laScdTask[lnCount,11],4,2)) = 30 , 25 , 24 ))
         OTHERWISE
           lnBarOrd = 1 + (VAL(SUBSTR(laScdTask[lnCount,11],1,2)) * 2) + ;
                      IIF(VAL(SUBSTR(laScdTask[lnCount,11],4,2)) = 30 , 1 , 0 ) + ;
                      IIF(SUBSTR(laScdTask[lnCount,11],7,1) = "A" ,0 , 24)
       ENDCASE
       *C200027,4 There is no open task in this list bar.
       IF lnBarOrd > 0 AND AT("Ú" , PRMBAR("puTasks",lnBarOrd)) = 0 AND ;
                            AT("Ä" , PRMBAR("puTasks",lnBarOrd)) = 0
         *C200027,4 Draw the task in the list.
         DO CASE
           *C200027,4 If the task duration less than 30 minutes.
           CASE laScdTask[lnCount,12] < 30
             DEFINE BAR lnBarOrd OF puTasks PROMPT ;
                    " "+laTasks[lnBarOrd]+;
                    IIF(BETWEEN(lnBarOrd,19,35)," ",lcHfBloc)+lcHzLine+ ;
                    "("+laScdTask[lnCount,9]+")"+REPLICATE(lcHzLine,1)+ ;
                    ALLTRIM(SUBSTR(laScdTask[lnCount,6],1,30))+REPLICATE(lcHzLine,11)+ ;
                    +"*"+laScdTask[lnCount,1]
           *C200027,4 If the task duration equal or more than 30 minutes.
           CASE laScdTask[lnCount,12] >= 30
             DEFINE BAR lnBarOrd OF puTasks PROMPT ;
                    " "+laTasks[lnBarOrd]+;
                    IIF(BETWEEN(lnBarOrd,19,35)," ",lcHfBloc)+lcUpLeft+ ;
                    "("+laScdTask[lnCount,9]+")"+REPLICATE(lcHzLine,1)+ ;
                    ALLTRIM(SUBSTR(laScdTask[lnCount,6],1,30))+REPLICATE(lcHzLine,11)+ ;
                    +"*"+laScdTask[lnCount,1]
             lnBarOrd = lnBarOrd + 1
             IF laScdTask[lnCount,12] > 30
               FOR lnCount1 = 1 TO INT(laScdTask[lnCount,12] / 30) - 1
                 DEFINE BAR lnBarOrd OF puTasks PROMPT ;
                        " "+laTasks[lnBarOrd]+;
                        IIF(BETWEEN(lnBarOrd,19,35)," ",lcHfBloc)+lcVrLine+;
                        IIF(BETWEEN(lnBarOrd,19,35)," ",REPLICATE(lcHfBloc,16))
                 lnBarOrd = lnBarOrd + 1
               ENDFOR
             ENDIF
             DEFINE BAR lnBarOrd OF puTasks PROMPT ;
                    " "+laTasks[lnBarOrd]+;
                    IIF(BETWEEN(lnBarOrd,19,35)," ",lcHfBloc)+lcDnLeft+ ;
                    REPLICATE(lcHzLine,21)
         ENDCASE
       ENDIF
     ENDIF
   ENDFOR
 ENDIF
*C200027,4 Reham On 05/06/96    *** End   ***

*C200027,4 Set the task list variable to 29+6 to display the first work
*C200027,4 hour "09:00 AM" as the first bar in the list
lsTasks = 29+6

IF !llDontHid
  SHOW POPUP puTasks
ELSE
  llDontHid = .F.
ENDIF

SHOW GET lsTasks

*C200027,4 Get the status of the edit button.
lcEdtStat = IIF(AT("*",PRMBAR("puTasks",GETBAR("puTasks",lsTasks))) > 0,;
                "ENABLE" , "DISABLE")

*C200027,4 Refresh the edit push button.
SHOW GET pbEdit &lcEdtStat

*!***********************************************************************
*! Name      : lfwTasks
*! Developer : Reham Al-Allamy
*! Date      : 05/06/1996
*! Purpose   : When function for the taks list.
*!***********************************************************************
*! Example   :  =lfwTasks()
*!***********************************************************************
*
FUNCTION lfwTasks

IF lsTasks = 0
  RETURN
ENDIF     

*C200027,4 If the current bar in the list has a task.
*E300711,1 HESHAM (Start)
*E300711,1 check if the user have the right to edit records
*E300711,1 so enable the edit push button else disable it
*IF AT("*",PRMBAR("puTasks",GETBAR("puTasks",lsTasks))) > 0 
IF AT("*",PRMBAR("puTasks",GETBAR("puTasks",lsTasks))) > 0 AND llEditRec
*E300711,1 HESHAM (End)
  *C200027,4 Enable the edit button
  lcEdtStat = "ENABLE"
  SHOW GET pbEdit &lcEdtStat
ELSE
  *C200027,4 Disable the edit button.
  lcEdtStat = "DISABLE"
  SHOW GET pbEdit &lcEdtStat
ENDIF
*E300711,1 HESHAM (Start)
*E300711,1 check if the user have the right to add records
*E300711,1 so enable the task type push buttons else disable it
IF !llAddRec
  SHOW GET pbCal  DISABLE
  SHOW GET pbApp  DISABLE
  SHOW GET pbToDo DISABLE    
ELSE
  SHOW GET pbCal  ENABLE
  SHOW GET pbApp  ENABLE
  SHOW GET pbToDo ENABLE    
ENDIF
*E300711,1 HESHAM (End)
*!***********************************************************************
*! Name      : lfwToDo
*! Developer : Reham Al-Allamy
*! Date      : 05/06/1996
*! Purpose   : When function for the taks list.
*!***********************************************************************
*! Example   :  =lfwToDo()
*!***********************************************************************
*
FUNCTION lfwToDo

IF lsToDo = 0
  RETURN .F.
ENDIF     

*!***********************************************************************
*! Name      : lfvUserId
*! Developer : Reham Al-Allamy
*! Date      : 05/05/1996
*! Purpose   : Funcion called from the user Id popup to select a user.
*!***********************************************************************
*! Example   :  =lfvUserId()
*!***********************************************************************
*
FUNCTION lfvUserId

*C200027,4 Store the current user Id.
lcOldUsr = lcUser_id

DO CASE
  CASE _DOS .OR. _UNIX
    lcUser_id = gfActPop(4,62,10,74,'laUsers',1,1,@lcUser_id)
    =lfScdRef()
  CASE _WINDOWS .OR. _MAC
    lcUser_id = laUsers[puDUserId,1]
    SHOW GET puUserId
    SHOW GET puUsers
ENDCASE

*C200027,4 Call function to do the following for the daily schedule :
*C200027,4 1- Select the data from the schedule file.
*C200027,4 2- Draw & build the list from the prepared array.
=lfGetDlTsk()

*C200027,4 Call function to get the scheduled todo.
=lfSelDaly2()

*!***********************************************************************
*! Name      : lfvEdit
*! Developer : Reham Al-Allamy
*! Date      : 05/07/1996
*! Purpose   : Valid function for the tasks list (Appointments - calls) 
*!           : and the edit push button
*!***********************************************************************
*! Example   :  =lfvEdit()
*!***********************************************************************
*
FUNCTION lfvEdit

PRIVATE lcOldCont,lcOldId,lcOldStore
lcOldCont  = lcContType
lcOldId    = lcContId
lcOldStore = lcStore

*C200027,4 If there is a schedule task in the current bar.
IF AT("*",PRMBAR("puTasks",GETBAR("puTasks",lsTasks))) > 0
  
  *C200027,4 Get the sequence no. for the current task.
  lcCurSeq = SUBSTR(PRMBAR("puTasks",GETBAR("puTasks",lsTasks)),;
             AT("*",PRMBAR("puTasks",GETBAR("puTasks",lsTasks)))+1,6)
  llCanEdit = .T.           
  lnTaskNo = ASCAN(laScdTask,lcCurSeq)
  IF lnTaskNo<>0
    lcContType = laScdTask[ASUBSCRIPT(laScdTask,lnTaskNo,1),3]  
    IF laScdTask[ASUBSCRIPT(laScdTask,lnTaskNo,1),21] <> gcAct_Comp
       =gfModalGen("TRM00255B00000","DIALOG")
      llCanEdit = .F.
    ENDIF
  ENDIF  
  IF llCanEdit
    IF AT("Í",PRMBAR("puTasks",GETBAR("puTasks",lsTasks))) > 0 .OR. ;
       AT("É",PRMBAR("puTasks",GETBAR("puTasks",lsTasks))) > 0
      *C200027,4 call the complete schedule screen.
      *E300711,1 HESHAM (Start)
      *E300711,1 check if the user have the right to edit records
      *E300711,1 so enable the complete task screen to modify the record
      *E300711,1 else run the complete task screen to view the record only
*      DO (gcAppHome+gcWinAppl+'\TASKCOM.PRG') WITH "M" , lcCurSeq , 0 , .F.      
      DO (gcAppHome+gcWinAppl+'\TASKCOM.PRG') WITH IIF(!llEditRec,'V',"M") , lcCurSeq , 0 , .F.
      *E300711,1 HESHAM (End)
    ELSE
      *C200027,4 Call the schedule type screen with the right parameter.
      *E300711,1 HESHAM (Start)
      *E300711,1 check if the user have the right to edit records
      *E300711,1 so enable the schedule screen to modify the record
      *E300711,1 else run the schedule screen to view the record only
*      DO (gcAppHome+gcWinAppl+'\TASKTYP.PRG') WITH "M" , lcCurSeq , 0 , lcContType , "" , "" , "" , {} , ""      
      DO (gcAppHome+gcWinAppl+'\TASKTYP.PRG') WITH IIF(!llEditRec,'V',"M") , lcCurSeq , 0 , lcContType , "" , "" , "" , {} , ""
      *E300711,1 HESHAM (End)      
    ENDIF
  
    *C200027,4 Call function to do the following for the daily schedule :
    *C200027,4 1- Select the data from the schedule file.
    *C200027,4 2- Draw & build the list from the prepared array.
    =lfGetDlTsk()
  ENDIF  
ENDIF

lcContType = lcOldCont
lcContId   = lcOldId
lcStore    = lcOldStore

*!***********************************************************************
*! Name      : lfvToDo
*! Developer : Reham Al-Allamy
*! Date      : 05/08/1996
*! Purpose   : Valid function for the tasks list (Todo)
*!***********************************************************************
*! Example   :  =lfvToDo()
*!***********************************************************************
*
FUNCTION lfvToDo

*C200027,4 Get the sequence no. for the current task.
lcOldCont  = lcContType
lcOldId    = lcContId
lcOldStore = lcStore

IF !EMPTY(laToDo[lsToDo,2])
  IF laToDo[lsToDo,21] <> gcAct_Comp
    =gfModalGen("TRM00255B00000","DIALOG")
  ELSE
    lcCurSeq = laToDo[lsToDo,2]
    lcContType = laToDo[lsToDo,4]
    *C200027,4 Call the schedule type screen with the right parameter.
    DO (gcAppHome+gcWinAppl+'\TASKTYP.PRG') WITH "M" , lcCurSeq , 0 , lcContType , "" , "" , "" , {} , ""
    =lfSelDaly2()
  ENDIF  
ENDIF
lcContType = lcOldCont
lcContId   = lcOldId
lcStore    = lcOldStore

*!***********************************************************************
*! Name      : lfvScdTyp
*! Developer : Reham Al-Allamy
*! Date      : 05/05/1996
*! Purpose   : Valid funcion for all the schedule types (Calls- Appointments-
*!           : - Things Todo )
*!           : This will call the schedule type screen according to the
*!           : sent parameters.
*!***********************************************************************
*! Example   :  =lfvScdTyp()
*!***********************************************************************
*
FUNCTION lfvScdTyp
PARAMETERS lnType , lcCurMode
PRIVATE lcOldCont,lcOldId,lcOldStore

lcLstTim   = SUBSTR(PRMBAR("puTasks",GETBAR("puTasks",lsTasks)),2,8)
lcOldCont  = lcContType
lcOldId    = lcContId
lcOldStore = lcStore

*C200027,4 Call the schedule type screen with the right parameter.
DO (gcAppHome+gcWinAppl+'\TASKTYP.PRG') WITH lcCurMode , "" , lnType , lcContType , lcContId , lcStore , lcUser_id , ldCurDate , lcLstTim

lcContType = lcOldCont
lcContId   = lcOldId
lcStore    = lcOldStore

IF lnType = 1 .OR. lnType = 2
  *C200027,4 Call function to do the following for the daily schedule :
  *C200027,4 1- Select the data from the schedule file.
  *C200027,4 2- Draw & build the list from the prepared array.
  =lfGetDlTsk()
ELSE
  *C200027,4 Call function to get the scheduled todo.
  =lfSelDaly2()
ENDIF

*!***********************************************************************
*! Name      : lfvSelWind
*! Developer : Reham Al-Allamy
*! Date      : 05/14/1996
*! Purpose   : Valid function for the folders invisable buttons to switch
*!           : between the folders.
*!***********************************************************************
*! Example   :  =lfvSelWind()
*!***********************************************************************
*
FUNCTION lfvSelWind
*WAIT gcBaseWind+','+lcBaseWind WINDOW
DO CASE
  CASE lnActFolder = 1
    *C200027,4 Enable the behind window before activating.
    SHOW GETS WINDOW (lcWinCh1) ENABLE ONLY
    SHOW GET pbEdit &lcEdtStat
*    ACTIVATE WINDOW (lcWinCh1) TOP
   _CUROBJ = OBJNUM(PBCLOSE)
  CASE lnActFolder = 2
    *C200027,4 Call function to do the following for the weekly schedule :
    *C200027,4 1- Select the current week scheduled tasks into an array.
    *C200027,4 2- Draw the five list of the five working days.
    ldStart = ldCurDate
    =lfGetWkTsk()
    SHOW GETS WINDOW (lcWinCh2) ENABLE ONLY
*    ACTIVATE WINDOW (lcWinCh2) TOP
   _CUROBJ = OBJNUM(PBCLOSE1)
ENDCASE


*!***********************************************************************
*! Name      : lfvWklUsr
*! Developer : Reham Al-Allamy
*! Date      : 05/14/1996
*! Purpose   : Valid function for the users poup in the weekly schedule
*!           : screen.
*!***********************************************************************
*! Example   : =lfvWklUsr()
*!***********************************************************************
*
FUNCTION lfvWklUsr

*C200027,4 Store the current user Id.
lcOldUsr = lcWklUsr

DO CASE
  CASE _DOS .OR. _UNIX
    lcWklUsr = gfActPop(2,24,8,37,'laUsers',1,1,@lcWklUsr)
    =lfScdRef()
  CASE _WINDOWS .OR. _MAC
    lcWklUsr = laUsers[puWklUsr,1]
    SHOW GET puWklUsr
ENDCASE

*C200027,4 If user Id changed.
IF !(lcOldUsr == lcWklUsr)
  *C200027,4 Call function to do the following for the weekly schedule :
  *C200027,4 1- Select the current week scheduled tasks into an array.
  *C200027,4 2- Draw the five list of the five working days.
  =lfGetWkTsk()
ENDIF
lcWklCont = ' '
lcWklSub  = ' '
SHOW GET lcWklCont
SHOW GET lcWklSub

*!***********************************************************************
*! Name      : lfvPrvWeek
*! Developer : Reham Al-Allamy
*! Date      : 05/21/1996
*! Purpose   : Valid function for the previous push button.
*!***********************************************************************
*! Example   :  =lfvPrvWeek()
*!***********************************************************************
*
FUNCTION lfvPrvWeek

*C200027,4 Subtract week from the start date for the weekly view.
ldStart   = ldStart - 7
lcWklCont = " "
lcWklSub  = " "

*C200027,4 Call the refresh function to refresh the say fields.
=lfScdRef()
SHOW GET lcWklCont
SHOW GET lcWklSub

*C200027,4 Call function to do the following for the weekly schedule :
*C200027,4 1- Select the current week scheduled tasks into an array.
*C200027,4 2- Draw the five list of the five working days.
=lfGetWkTsk()

*!***********************************************************************
*! Name      : lfvNxtWeek
*! Developer : Reham Al-Allamy
*! Date      : 05/21/1996
*! Purpose   : Valid function for the next push button.
*!***********************************************************************
*! Example   :  =lfvNxtWeek()
*!***********************************************************************
*
FUNCTION lfvNxtWeek

*C200027,4 Add week to the start date for the weekly view.
ldStart   = ldStart + 7
lcWklCont = " "
lcWklSub  = " "

*C200027,4 Call the refresh function to refresh the say fields.
=lfScdRef()
SHOW GET lcWklCont
SHOW GET lcWklSub

*C200027,4 Call function to do the following for the weekly schedule :
*C200027,4 1- Select the current week scheduled tasks into an array.
*C200027,4 2- Draw the five list of the five working days.
=lfGetWkTsk()

*!***********************************************************************
*! Name      : lfwTsk
*! Developer : Reham Al-Allamy
*! Date      : 05/26/1996
*! Purpose   : When function for the tasks lists in the weekly view screen.
*!***********************************************************************
*! Passed Parameters  :  lcTskNo -> No. of the list
*!***********************************************************************
*! Example            :  =lfwTsk()
*!***********************************************************************
*
FUNCTION lfwTsk
PARAMETERS lcTskNo

*C200027,4 If there is a schedule task in the current bar.
IF lsTsk&lcTskNo = 0
  RETURN
ENDIF     
IF AT("*",PRMBAR("puTsk&lcTskNo",GETBAR("puTsk&lcTskNo",lsTsk&lcTskNo))) > 0
  *C200027,4 Get the contact for the current task in the current list.
  lcWklCont = SUBSTR(PRMBAR("puTsk&lcTskNo",GETBAR("puTsk&lcTskNo",lsTsk&lcTskNo)),;
             AT("*",PRMBAR("puTsk&lcTskNo",GETBAR("puTsk&lcTskNo",lsTsk&lcTskNo)))+1,30)
  *C200027,4 Get the subject for the current task in the current list.
  lcWklSub  = SUBSTR(PRMBAR("puTsk&lcTskNo",GETBAR("puTsk&lcTskNo",lsTsk&lcTskNo)),;
             AT("*",PRMBAR("puTsk&lcTskNo",GETBAR("puTsk&lcTskNo",lsTsk&lcTskNo)))+32,30)
ELSE
  lcWklCont = " "
  lcWklSub  = " "
ENDIF

SHOW GET lcWklCont
SHOW GET lcWklSub

*!***********************************************************************
*! Name      : lfGetWkTsk
*! Developer : Reham Al-Allamy
*! Date      : 05/14/1996
*! Purpose   : Function called with every change happend with the
*!             the user Id or <Next> <Previous> buttons (Weekly schedule).
*!***********************************************************************
*! Calls     : =lfSelWkly()  -> Select the data from the schedule file.
*!           : =lfBldWkBrs() -> Draw & build the list from the prepared array.
*!***********************************************************************
*! Example   :  =lfGetWkTsk()
*!***********************************************************************
*
FUNCTION lfGetWkTsk

*C200027,4 1- Select the current week scheduled tasks into an array.
=lfSelWkly()

*C200027,4 2- Draw the five list of the five working days.
=lfBldWkBrs()

*!***********************************************************************
*! Name      : lfSelWkly
*! Developer : Reham Al-Allamy
*! Date      : 05/14/1996
*! Purpose   : Function to select the current users tasks for the 
*!           : current week.
*!***********************************************************************
*! Example   :  =lfSelWkly()
*!***********************************************************************
*
FUNCTION lfSelWkly

*C200027,4 Array hold the selected tasks from the schedule file.
DECLARE laScdWkTsk[1,7]
STORE " " TO laScdWkTsk

*C200027,4 Select the open & completed scheduled tasks for the current 
*C200027,4 user and current date (Appointments - Calls)
SELECT contact , ctrantype , dtrandate , ctrantime , ;
       nestdur , cCompleted , csubject ;
  FROM (gcSysHome+"SYSCHDUL") ;
 WHERE cUser_ID = lcWklUsr  .AND. ;
       (cTranType = "C" .OR. cTranType = "A") .AND. ;
       BETWEEN(DTRANDATE , ldStart , ldStart+5 ) ;
 ORDER BY DTRANDATE , CTRANTIME , CCOMPLETED , CSEQNUMBER ;
  INTO ARRAY laScdWkTsk

*!***********************************************************************
*! Name      : lfBldWkBrs
*! Developer : Reham Al-Allamy
*! Date      : 05/14/1996
*! Purpose   : Function to build the weekly lists for the current user
*!           : and current week.
*!***********************************************************************
*! Example            :  =lfBldWkBrs()
*!***********************************************************************
*
FUNCTION lfBldWkBrs

FOR lnCount1 = 1 TO 5
  lcCount1 = ALLTRIM(STR(lnCount1))
  IF !llHid&lcCount1
    HIDE POPUP puTsk&lcCount1 SAVE
  ENDIF
ENDFOR

ldBegin  = ldStart

*DO CASE
*  CASE _DOS
    FOR lnCount1 = 1 TO 5
      *** Call the thermometer function. ***
      *llNothing= lfTherm(5,lnCount1,"Preparing data...")
      llNothing= gfThermo(5,lnCount1,"Preparing data...",'')

      lcCount1 = ALLTRIM(STR(lnCount1))
      RELEASE BAR  ALL OF puTsk&lcCount1
      
      FOR lnCount2 = 1 TO ALEN(laTasks,1)
        DEFINE BAR lnCount2 OF puTsk&lcCount1 PROMPT " "+laTasks[lnCount2] + ;
               IIF(BETWEEN(lnCount2,19,35)," ",REPLICATE(lcHfBloc,17))
      ENDFOR
      IF !EMPTY(laScdWkTsk[1,1])
        FOR lnCount3 = 1 TO ALEN(laScdWkTsk,1)
          IF laScdWkTsk[lnCount3,3] = ldBegin
            IF !EMPTY(laScdWkTsk[lnCount3,4])
              lcHzLine = IIF( laScdWkTsk[lnCount3,6] = "N" , "Ä" , "Í")
              lcUpLeft = IIF( laScdWkTsk[lnCount3,6] = "N" , "Ú" , "É")
              lcVrLine = IIF( laScdWkTsk[lnCount3,6] = "N" , "³" , "º")
              lcDnLeft = IIF( laScdWkTsk[lnCount3,6] = "N" , "À" , "È")
              *C200027,4 Get the bar no. where the current task exist.
              DO CASE
                CASE SUBSTR(laScdWkTsk[lnCount3,4],1,2) = "12"
                  lnBarOrd = 1 + IIF(SUBSTR(laScdWkTsk[lnCount3,4],7,1) = "A" , ;
                             IIF(VAL(SUBSTR(laScdWkTsk[lnCount3,4],4,2)) = 30 , 1 , 0 ) , ;
                             IIF(VAL(SUBSTR(laScdWkTsk[lnCount3,4],4,2)) = 30 , 25 , 24 ))
                OTHERWISE
                  lnBarOrd = 1 + (VAL(SUBSTR(laScdWkTsk[lnCount3,4],1,2)) * 2) + ;
                             IIF(VAL(SUBSTR(laScdWkTsk[lnCount3,4],4,2)) = 30 , 1 , 0 ) + ;
                             IIF(SUBSTR(laScdWkTsk[lnCount3,4],7,1) = "A" ,0 , 24)
              ENDCASE
              *C200027,4 There is no open task in this list bar.
              IF AT("Ú" , PRMBAR("puTsk&lcCount1",lnBarOrd)) = 0
                *C200027,4 Draw the task in the list.
                DO CASE
                  *C200027,4 If the task duration less than 30 minutes.
                  CASE laScdWkTsk[lnCount3,5] < 30
                    DEFINE BAR lnBarOrd OF puTsk&lcCount1 PROMPT ;
                           " "+laTasks[lnBarOrd]+lcHzLine+ ;
                           laScdWkTsk[lnCount3,2]+REPLICATE(lcHzLine,2) + ;
                           "*"+laScdWkTsk[lnCount3,1]+" "+laScdWkTsk[lnCount3,7]
                  *C200027,4 If the task duration equal or more than 30 minutes.
                  CASE laScdWkTsk[lnCount3,5] >= 30
                    DEFINE BAR lnBarOrd OF puTsk&lcCount1 PROMPT ;
                           " "+laTasks[lnBarOrd]+lcUpLeft+ ;
                           laScdWkTsk[lnCount3,2]+REPLICATE(lcHzLine,2) + ;
                           "*"+laScdWkTsk[lnCount3,1]+" "+laScdWkTsk[lnCount3,7]
                    lnBarOrd = lnBarOrd + 1
                    IF laScdWkTsk[lnCount3,5] > 30
                      FOR lnCount4 = 1 TO INT(laScdWkTsk[lnCount3,5] / 30) - 1
                        DEFINE BAR lnBarOrd OF puTsk&lcCount1 PROMPT ;
                               " "+laTasks[lnBarOrd]+lcVrLine+;
                               IIF(BETWEEN(lnBarOrd,19,35)," ",REPLICATE(lcHfBloc,5))
                        lnBarOrd = lnBarOrd + 1
                      ENDFOR
                    ENDIF
                    DEFINE BAR lnBarOrd OF puTsk&lcCount1 PROMPT ;
                           " "+laTasks[lnBarOrd]+lcDnLeft+ ;
                           REPLICATE(lcHzLine,5)
                ENDCASE
              ENDIF
            ENDIF
          ENDIF
        ENDFOR
      ENDIF
      
      ldBegin = ldBegin + 1
  ENDFOR
*ENDCASE

FOR lnCount1 = 1 TO 5
  lcCount1 = STR(lnCount1,1)
  STORE 28 TO lsTsk&lcCount1
  IF !llHid&lcCount1
    SHOW POPUP puTsk&lcCount1
  ELSE
    llHid&lcCount1 = .F.
  ENDIF
  SHOW GET lsTsk&lcCount1
ENDFOR


FUNCTION lfvClose
glQuitting = .T.
CLEAR READ


FUNCTION lfStopRead
lcWindOnTop = WONTOP()
llDeact = gfStopRead()
RETURN llDeact