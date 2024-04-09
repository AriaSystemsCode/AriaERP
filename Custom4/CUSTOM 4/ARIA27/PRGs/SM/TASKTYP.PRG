*:************************************************************************
*:
*: Procedure file: TASKTYP.PRG
*:               : Tasks Scheduling
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Aria Apparel System
*:           Date: May,05 1996
*:         Author: Reham Aly Al-Allamy
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:                 
*:      Documented  /  /
*:************************************************************************
*:  Modifications :
*B601162,1 M.H 07/16/96 Make the validation function for the time object.
*B800685,1 Reham 17/22/96 Remark the wrong calling of time validation
*B800685,1 Reham 17/22/96 because it give an error while compiling.
*B800704,1 M.H 08/08/96 Fix the invalid subscript reference when the screen is rumming stand alon.
*C200027,8 09/02/96 M.H Add the priority on the calls & appointement.
*E300711,1 HESHAM 07/28/97
*E300711,1 add new mode to view data only without give the user the access
*E300711,1 to modify data
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*B602697,1 Reham On 03/25/99
*B602697,1 1- Fill the task reasons popups with the its codes.
*B602697,1 2- Refresh the contact popup to show all contacts.
*B602697,1 3- Do not display blank lines in the contact popup.
*B602697,1 4- Use Customer phone1 or phone2 if the contact phone is empty.
*B803577,1 08/10/2000 Fix bug When we schedule ( From the customer screen)
*B803577,1            A task for a contact that has a phone number, 
*B803577,1            the phone number does not appear.
*B603817,1 MHM 10/02/2000 Fix the bug Invalid Subscript Reference In case of Open 
*B603817,1                the user popup then select empty space (No user selected)
*:B606208,1 RAE Fix incorrect contact phone number.
*B606742,1 AMH 08/07/2003 Fix the bug of not validate the vendor code correctly.
*:B122747,1 NNA 05/27/2004 fix bug that if you complete a task (on the User Task list) and flag 
*:B122747,1           it as unsuccessful and mark for Schedule Follow-up call. it will updates the 
*:B122747,1           task to the history but it will remove it from the Pending and that is the bug
*:************************************************************************
*
PARAMETERS lcMode , lcSeq , lnScdType , lcCntTyp , lcCont_Id , lcAccStore , lcUsr , ldDate , lcTime

DECLARE laCodes[1,10],laReason[1,2]
STORE '' TO laCodes
*lnReason = 1
lnReason = 0

laCodes[1,1] = 'CTRANRESON'
laCodes[1,2] = 'laReason'
laCodes[1,3] = 'lnReason'
laCodes[1,4] = ''
laCodes[1,5] = .F.
laCodes[1,6] = .F.
laCodes[1,7] = 'SYSCHDUL'
laCodes[1,8] = 'Cseqnum'
laCodes[1,9] = 'lcSeq'
laCodes[1,10] = 'CTRANRESON'


*C200027,4 Example : If you modifying an existing task.
*C200027,4 DO TASKTYP.PRG WITH "M" , lcCurSeq , 0 , lcContType , "" , "" , "" , {} , ""
*C200027,4 Example : If you adding a new task.
*C200027,4 DO TASKTYP.PRG WITH lcCurMode , "" , lnType , lcContType , lcContId , lcStore , lcUser_id , ldCur_Date , lcLstTim

*C200027,4 P.S. Current time should be "HH:MM AM"

*C200027,4 lcMode     -> (M A) Modify or Add Mode
*C200027,4 lcSeq      -> If modify mode, send the task sequence no.
*C200027,4 lnScdType  -> 1 For Call, 2 For Appointment, 3 For Todo.
*C200027,4 lcCntTyp   -> C For Customer, V For Vendor, O For Others.
*C200027,4 lcCont_Id  -> Account Or Vendor Code.
*C200027,4 lcAccStore ->  Account Store
*C200027,4 lcUsr      -> Current User
*C200027,4 ldDate     -> Current date
*C200027,4 lcTime     -> The sent time.

*RESTORE FROM adpara&qsyscode ADDITIVE
*llApLink   = m_ApLink    = 'Y'
*lcApDir    = ALLTRIM (M_ApDataDir)

*B602697,1 Reham On 03/25/99   *** Begin ***
*B602697,1 Use Customer phone1 or phone2 if the contact phone is empty.
STORE "" TO lcPhone1 , lcPhone2
*B602697,1 Reham On 03/25/99   *** Begin ***

*E300320,1 Store the bmps directories.
lcTskBmp1 = gcBmpHome + "EXTKEY.BMP"
lcTskBmp2 = gcBmpHome + "ZOOM.BMP"
lcTskBmp3 = gcBmpHome + "SEL.BMP"
lcTskBmp4 = gcBmpHome + "OK.BMP"
lcTskBmp5 = gcBmpHome + "CAN.BMP"

lcOldVal   = " "
lnCurObj   = 1
llClrRead  = .F.

*B122747,1 NNA 05/27/2004 (BEGIN) New Variable that hold the user Action in the tasktyp Screen
*B122747,1 NNA             to know if he Press [Cancel] Button or [Ok] Button
llCancel   = .F.
*B122747,1 NNA (END)

lcRunFun   = ' '
lnBrowsRec = 0
lcCodTitle = ''                   && For the code browse used by
                                  && the new gpcodechek
lcTskNote  = ''                                  

*C200027,4 Flag to know if open customer file in this sission or not.
llOpnCust = .F.
*C200027,4 Open the customer file.
IF !USED("CUSTOMER") AND 'AR' $ gcCmpModul
  =gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
  *C200027,4 Set flag to true to close the customer file if 
  *C200027,4 closing this program.
  llOpnCust = .T.
ENDIF

*C200027,4 Flag to know if the vendor file open or not.
llOpnVend = .F.
*IF llApLink
  IF !USED("APVENDOR")
    =gfOpenFile(gcDataDir+'APVENDOR','VENCODE','SH')
    llOpnVend = .T.
  ENDIF
  lcVendFile = "APVENDOR"
*ELSE
*  IF !USED("VENDOR")
*    =gfOpenFile(gcDataDir+'VENDOR',gcDataDir+'VENDOR','SH')
*    llOpnVend = .T.
*  ENDIF
*  lcVendFile = "VENDOR"
*ENDIF

*C200027,4 Flag to know if open CONTACT file in this sission or not.
llOpnContc = .F.
*C200027,4 Open the CONTACT file.
IF !USED("CONTACT")
  =gfOpenFile(gcDataDir+'CONTACT',gcDataDir+'CONTACT','SH')
  *C200027,4 Set flag to true to close the CONTACT file if 
  *C200027,4 closing this program.
  llOpnContc = .T.
ENDIF

*C200027,4 Flag to know if open code file in this sission or not.
llOpnCode = .F.
*C200027,4 Open the code file.
IF !USED("CODES")
  =gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
  *C200027,4 Set flag to true to close the code file if closing this program.
  llOpnCode = .T.
ENDIF

*C200027,4 Flag to know if open resources file in this sission or not.
llOpnResc = .F.
*C200027,4 Open the resources file.
IF !USED("RESOURCE")
   USE gcDataDir+'RESOURCE' IN 0 ORDER TAG SEQUENCE
  *C200027,4 Set flag to true to close the resources file if closing this program.
  llOpnResc = .T.
ENDIF

*C200027,4 Flag to know if coming from the browse icon or not.
llBrowse = .F.

*C200027,4 Variable hold the old value of the account store.
lcOldStore = " "

*C200027,4 Disable the task type radio button.
lcTypStat = IIF( lcMode = "M" , "DISABLE" , "ENABLE")

*C200027,4 Variable hold the old customer.
STORE " " TO lcOldCnId , lnOldValue

*C200027,4 Array hold the available resources from the resources file.
DECLARE laAllRes[1]
laAllRes = " "
SELECT CODES
*SELECT SUBSTR(CODE.CODE,2,3) + " " + CODE.cData ;
  FROM (gcDataDir+"CODE") ;
 WHERE SUBSTR(CODE.CODE,1,1) = "Y" ;
  INTO ARRAY laAllRes
*E300789,1 Hesham (Start)
*SELECT CODES.cCODE_NO + " " + CODES.cDISCREP ;
  FROM (gcDataDir+"CODES") ;
 WHERE CODES.CFLD_NAME = "CRESO_CODE" ;
       AND !EMPTY(CCOMP_ID);
  INTO ARRAY laAllRes

SELECT CODES.cCODE_NO + " " + CODES.cDISCREP ;
  FROM (gcDataDir+"CODES") ;
 WHERE CODES.CFLD_NAME = "CRESO_CODE" ;
       AND cdefcode = 'N';
  INTO ARRAY laAllRes

*E300789,1 Hesham (End)
*C200027,4 Declare array to hold the users Ids.
DECLARE laAllUsr[1]
*C200027,4 Select the available users from the users file.
SELECT SYUUSER.cUser_Id FROM (gcSysHome+"SYUUSER") INTO ARRAY laAllUsr

DECLARE laContTyp[4,2]
laContTyp[1,1] = IIF('AR' $ gcCmpModul,'','\')+"Customer"
laContTyp[1,2] = 'C'
laContTyp[2,1] = IIF('AP' $ gcCmpModul,'','\')+"Vendor"
laContTyp[2,2] = 'V'
laContTyp[3,1] = "Others"
laContTyp[3,2] = 'O'
laContTyp[4,1] = "None"
laContTyp[4,2] = 'N'

DO CASE
  *C200027,4 If adding new schedule task.
  CASE lcMode = "A"
    *C200027,4 Set flag to true to enable the zoom screen.
    llEnable = .T.
    
    *C200027,4 Array hold the resources for the current task.
    DECLARE laResource[1]
    STORE " " TO laResource
    
    *C200027,4 Default the radio button to the sent parameters.
    rbTskTyp   = lnScdType
    
    *C200027,4 Set the user to the current user in the main schedule screen.
    lcTskUsr   = lcUsr
    
    *C200027,4 Fill the contact type popup.
    IF EMPTY(lcCntTyp)
      lnCount = 1
      FOR lnCount = 1 TO ALEN(laContTyp,1)
        IF LEFT(laContTyp[lnCount,1],1)<>'\'
          lcCntTyp  = laContTyp[lnCount,2]
          EXIT
        ENDIF
      ENDFOR
    ENDIF  
    lcTskCtId = lcCntTyp
    lcTskCtTyp = IIF(EMPTY(lcCntTyp) , laContTyp[lnCount,2] , laContTyp[ASUBS(laContTyp,ASCAN(laContTyp,lcCntTyp),1),2])
    puTskCtTy  = ASUBS(laContTyp,ASCAN(laContTyp,lcCntTyp),1)
    
    *C200027,4 Variable hold the customer Id.
    IF LEN(lcCont_Id) = 5
      lcContId   = IIF(!EMPTY(lcCont_Id) , lcCont_Id+SPACE(3) , SPACE(8))
    ELSE
      lcContId   = IIF(!EMPTY(lcCont_Id) , lcCont_Id , SPACE(8))
    ENDIF
    *C200027,4 Variable hold the contact name.
    DO CASE
      CASE lcTskCtId = "C" 
        lcCnIdDesc = IIF(!EMPTY(lcContId) , ;
           LOOKUP(CUSTOMER.StName,"M"+SUBSTR(lcContId,1,5),CUSTOMER.ACCOUNT,"CUSTOMER") , " ")
      CASE lcTskCtId = "V"
*        IF llApLink
          lcCnIdDesc = IIF(!EMPTY(lcContId) , ;
             LOOKUP(APVENDOR.CVENCOMP,lcContId,APVENDOR.CVENDCODE,"VENCODE") , " ")
*        ELSE
*          lcCnIdDesc = IIF(!EMPTY(lcContId) , ;
*             LOOKUP(VENDOR.Name,lcContId,VENDOR.VENDOR,"VENDOR") , " ")
*        ENDIF
    ENDCASE
    
    *C200027,4 If the passed contact id was customer.
    lcStore    = IIF(lcTskCtId = "C" , lcAccStore , SPACE(8) )
    lcStrStat  = IIF(lcTskCtId = "C" , "ENABLE" , "DISABLE" )
    
    *C200027,4 Define the contact variable.
    lcTskCont  = SPACE(30)
    puTskCont  = 0 &&SPACE(30)
    
    *C200027,4 Disable the contact popup if there is no account.
*B800704,1 M.H Begin.
*    lcContStat = IIF(EMPTY(lcContId),"DISABLE","ENABLE")
    
    *B606742,1 AMH Consider case of Vendor [Start]
    *lcContStat = IIF(EMPTY(lcContId) .OR. lcTskCtId = "C" ,"ENABLE","DISABLE")    
    lcContStat = IIF(EMPTY(lcContId) .OR. lcTskCtId $ "CV" ,"ENABLE","DISABLE")    
    *B606742,1 AMH [End]
    
*B800704,1 M.H End.
    *C200027,4 Variable hold the contact phone no.
    lcTskPhn   = " "
    
    *C200027,4 Variable hold the subject.
    lcTskSub   = " "
    
    *B602697,1 Reham On 03/25/99  *** Begin ***
    *C200027,4 Define the reason variable.
    *lcRsn      = SPACE(3)
    =gfwCodePop(@laCodes,'CTRANRESON','D')
    lcRsn  = ALLTRIM(laReason[lnReason,2])
    *B602697,1 Reham On 03/25/99  *** End   ***
    
    *C200027,4 Define the reason description variable.
*    lcRsn_Desc = " "
    
    *C200027,4 Variable hold the priority if scheduling todo task.
*C200027,8 M.H Begin.
*    lcTskPrt  = IIF(rbTskTyp = 3 , "9" , " ")
    lcTskPrt  = "9"
*    lcPrtStat = IIF(rbTskTyp = 3 , "ENABLE" , "DISABLE" )
    lcPrtStat = "ENABLE"
*C200027,8 M.H End.

    *C200027,4 Define the tasks notes variable.
    lcTskNote  = " "
    
    *C200027,4 Define the resources list variable.
    lsResource = 1
    
    *C200027,4 Define the current task date.
    lcTskDat   = IIF(EMPTY(ldDate) , DATE() , ldDate)
    
    *C200027,4 Define the current task time.
*    lcTskTim   = IIF(EMPTY(lcTime) , "00:00 AM" , lcTime)    
    IF TYPE('lcTime') = 'C'
      lcTskTim   = lcTime
    ELSE  
      IF VAL(SUBSTR(TIME(),1,2)) < 12
        lcTskTim   = SUBSTR(TIME(),1,5) + " AM"
      ELSE
        lcHour     = IIF(EVAL(SUBSTR(TIME(),1,2))-12=0,;
                         SUBSTR(TIME(),1,2),ALLTRIM(STR(EVAL(SUBSTR(TIME(),1,2))-12)))
        lcTskTim   = IIF(LEN(lcHour)=1,'0','') + lcHour + SUBSTR(TIME(),3,3)+" PM"
      ENDIF
    ENDIF  

    
    *C200027,4 Define the current task duration.
    lnTskDur   = 0
    
    *C200027,4 Variable to know if the task open or complete.
    lcComplete = "N"
    
  *C200027,4 If modifing an existing schedule task.
  *E300711,1 HESHAM (Start)
  *E300711,1 add new mode to view data only without give the user the access
  *E300711,1 to modify data
*  CASE lcMode = "M" 
  CASE lcMode = "M" OR lcMode = "V"
  *E300711,1 HESHAM (End)  
    *C200027,4 Set flag to true to enable the zoom screen.
    llEnable = .T.
    
    SELECT syschdul
    LOCATE FOR cseqnumber = lcSeq AND CCOMP_ID=gcAct_Comp
    IF FOUND()
      STORE AT(ctrantype,"CAT") TO rbTskTyp , lnScdType
      lcTskUsr   = cUser_ID
      lcTskCtId  = cconttype
      lcTskCtTyp = laContTyp[ASUBS(laContTyp,ASCAN(laContTyp,lcCntTyp),1),2]
      
      *C200027,4 Variable hold the customer Id.
      lcContId   = CCONT_ID
      DO CASE
        CASE cconttype = "C" 
          lcCnIdDesc = IIF(!EMPTY(lcContId) , ;
             LOOKUP(CUSTOMER.StName,"M"+SUBSTR(lcContId,1,5),CUSTOMER.ACCOUNT,"CUSTOMER") , " ")
        CASE cconttype = "V"
*          IF llApLink
            lcCnIdDesc = IIF(!EMPTY(lcContId) , ;
               LOOKUP(APVENDOR.CVENCOMP,lcContId,APVENDOR.CVENDCODE,"VENCODE") , " ")
*          ELSE
*            lcCnIdDesc = IIF(!EMPTY(lcContId) , ;
*               LOOKUP(VENDOR.Name,lcContId,VENDOR.VENDOR,"VENDOR") , " ")
*          ENDIF
      ENDCASE
      lcStore    = store
      lcStrStat  = IIF(lcTskCtId = "C" , "ENABLE" , "DISABLE" )
      lcTskCont  = contact
      
      *B606742,1 AMH Consider case of Vendor [Start]
      *lcContStat = IIF(EMPTY(lcContId) , "DISABLE" , "ENABLE")
      lcContStat = IIF(EMPTY(lcContId) .OR. lcTskCtId $ "CV" ,"ENABLE","DISABLE")    
      *B606742,1 AMH [End]
      
      lcTskPhn   = cPhone
      lcTskSub   = cSubject
      lcRsn      = ctranreson
      *B602697,1 Reham On 03/25/99   *** Begin ***
      =gfwCodePop(@laCodes,'CTRANRESON','V,'+lcRsn)
      *=gfwCodePop(@laCodes,'CTRANRESON','T')
      *B602697,1 Reham On 03/25/99   *** End   ***

*      lcRsn_Desc = LOOKUP(CODE.cData,"W"+lcRsn,CODE.CODE,"CODE")
      lcTskPrt   = cpriority
*C200027,8 M.H Begin.
*      lcPrtStat = IIF(rbTskTyp = 3 , "ENABLE" , "DISABLE" )
      lcPrtStat  = "ENABLE"
*C200027,8 M.H End.
      lcTskNote  = mnotes
      lcTskDat   = dtrandate
      lcTskTim   = ctrantime
      lnTskDur   = nestdur
      lcComplete = cCompleted
    ENDIF
    
    *C200027,4 Array hold the resources for the current task.
    DECLARE laResource[1]
    STORE " " TO laResource
    
    SELECT RESOURCE
*    SELECT RESOURCE.creso_code + " " + CODES.cDISCREP ;
      FROM (gcDataDir+"RESOURCE") , (gcDataDir+"CODE");
     WHERE RESOURCE.cseqnumber == lcSeq .AND. ;
           "Y"+RESOURCE.creso_code = CODES.cCODE_NO ;
      INTO ARRAY laResource

*E300789,1 Hesham (Start)
   *SELECT DIST RESOURCE.creso_code + " " + CODES.cDISCREP ;
     FROM (gcDataDir+"RESOURCE") , (gcDataDir+"CODES");
     WHERE RESOURCE.cseqnumber == lcSeq .AND. ;
           RESOURCE.creso_code = CODES.cCODE_NO ;
           AND CODES.CRLTFIELD='N' AND !EMPTY(CODES.CCOMP_ID);
      INTO ARRAY laResource

   SELECT DIST RESOURCE.creso_code + " " + CODES.cDISCREP ;
     FROM (gcDataDir+"RESOURCE") , (gcDataDir+"CODES");
     WHERE RESOURCE.cseqnumber == lcSeq .AND. ;
           RESOURCE.creso_code = CODES.cCODE_NO ;
           AND CODES.CRLTFIELD='N' AND CODES.CDEFCODE='N';
      INTO ARRAY laResource

*E300789,1 Hesham (End)
    *C200027,4 Define the resources list variable.
    lsResource = 1
ENDCASE

lcCompStat = IIF(lcMode = "M" .AND. lcComplete = "Y" , "DISABLE" , "ENABLE")

*C200027,4 Flag in the when function of the contact popup.
llInv     = IIF(lcTskCtId $ "CV" , .T. , .F.)
lcInvStat = IIF(lcTskCtId $ "CV" , "ENABLE" , "DISABLE")
*C200027,4 Flag in the when function of the contact field.


*B800704,1 M.H 08/11/96 Begin.
*llGet     = IIF(lcTskCtId $ "CV" , .F. , .T.)
*lcGetStat = IIF(lcTskCtId $ "CV" , "DISABLE" , "ENABLE")

*B606742,1 AMH Consider case of vendor [Start]
*llGet     = IIF(lcTskCtId = "C" , .F. , .T.)
*lcGetStat = IIF(lcTskCtId = "C" , "DISABLE" , "ENABLE")
llGet     = IIF(lcTskCtId $ "CV" , .F. , .T.)
lcGetStat = IIF(lcTskCtId $ "CV" , "DISABLE" , "ENABLE")
*B606742,1 AMH [End]

*B800704,1 M.H 08/11/96 End.

*C200027,4 Variable hold an arrow if contact id is accout or vendor 
*C200027,4 and hold a space if contact id is other or none.
lcArrow = IIF(lcTskCtId = "C" , "" , " ")

*C200027,4 If there an contact type & contact Id & the contact type is
*C200027,4 customer or vendor,get his contacts from the CONTACT file.
DECLARE laContact[1,2]
laContact = " "

IF lcTskCtId $ "CV" .AND. !EMPTY(lcContId)
 =lfFillCont()
ENDIF

puTskCtTy = ASUBS(laContTyp,ASCAN(laContTyp,lcCntTyp),1)
puTskUsr  = ASCAN(laAllUsr,lcTskUsr)
lnContPos = ASCAN(lacontact,lcTskCont)
puTskCont = IIF(lnContPos = 0,1,ASUBS(lacontact,lnContPos,1)) &&lcTskCont
lcTskCont = IIF(EMPTY(lcTskCont),lacontact[puTskCont,1],lcTskCont)

*B803577,1 ABD - Fix bug When we schedule ( From the customer screen)
*B803577,1 ABD - A task for a contact that has a phone number, 
*B803577,1 ABD - the phone number does not appear. [Begin]
*:B606208,1 RAE Fix incorrect contact phone number.
*lcTskPhn = IIF(laContact[1,2]= "N/A","",laContact[1,2])
lcTskPhn = IIF(laContact[1,2]= "N/A","",laContact[puTskCont,2])
*:B606208,1 RAE Fix incorrect contact phone number.
*B803577,1 ABD [End]


*C200027,4 Call the schedule type screen.
DO WHILE !llClrRead
  DO (gcScrDir+gcWinAppl+'\TASKTYP.SPX')
  IF !EMPTY(lcRunFun)
    &lcRunFun
    lcRunFun = ' '
  ENDIF
ENDDO
*B122747,1 NNA 05/27/2004 (BEGIN) if the user press [Cancel] after he Assign a task as Completed
*B122747,1 NNA             what ever 'Success.' or 'Unsucc.' the compleated flag already assigned
*B122747,1 NNA             with 'Y' as Completed So I return it to uncompleted Task
IF llCancel .AND. Syschdul.cCompleted = 'Y'
  REPLACE Syschdul.cCompleted WITH 'N'
ENDIF
*B122747,1 NNA (END)

*C200027,4 Close the opened files.

IF llOpnContc .AND. USED("CONTACT")
  USE IN CONTACT
ENDIF

IF llOpnCode .AND. USED("CODES")
  USE IN CODES
ENDIF

IF llOpnResc .AND. USED("RESOURCE")
  USE IN RESOURCE
ENDIF

IF llOpnCust .AND. USED("CUSTOMER")
  USE IN CUSTOMER
ENDIF

IF llOpnVend
  SELECT (lcVendFile)
  USE
ENDIF

*!***********************************************************************
*! Name      : lfvTskTyp
*! Developer : Reham Al-Allamy
*! Date      : 05/14/1996
*! Purpose   : Valid function of the task type radio button.
*!***********************************************************************
*! Example   :  =lfvTskTyp()
*!***********************************************************************
*
FUNCTION lfvTskTyp

IF !(lnOldValue == rbTskTyp)
  *C200027,4 Variable hold the priority if scheduling todo task.
*  lcTskPrt  = IIF(rbTskTyp = 3 , "9" , " ")
  lcTskPrt  = "9"
*  lcPrtStat = IIF(rbTskTyp = 3 , "ENABLE" , "DISABLE" )
  lcPrtStat = "ENABLE"
*C200027,8 M.H End.

  SHOW GET lcTskPrt &lcPrtStat
  
  IF rbTskTyp <> 3 .AND. lcTskCtId = "N"
    *C200027,4 If choose a task and it was not a todo task and the contact 
    *C200027,4 type was none, restore the radio button old value.
    =gfDialog("I","Only a todo task is allowed for a contact type (None).")
    *C200027,4 Restore the old value.
    rbTskTyp = lnOldValue
    SHOW GET rbTskTyp
  ENDIF
ENDIF

*!***********************************************************************
*! Name      : lfvTskUsr
*! Developer : Reham Al-Allamy
*! Date      : 05/05/1996
*! Purpose   : Funcion called from the user Id popup to select a user.
*!***********************************************************************
*! Calls     : lfTskRef
*!***********************************************************************
*! Example   :  =lfvTskUsr()
*!***********************************************************************
*
FUNCTION lfvTskUsr

*C200027,4 Validate the user popup.
DO CASE
  CASE _DOS .OR. _UNIX
    lcTskUsr = gfActPop(5,5,12,18,'laAllUsr',1,1,@lcTskUsr)
    =lfTskRef()
  CASE _WINDOWS .OR. _MAC
    
    *B603817,1 MHM 10/02/2000 [Start] initialized variable to get first position if no user select
    puTskUsr = IIF ( puTskUsr <> 0 , puTskUsr , 1 ) 
    *B603817,1 MHM 10/02/2000 [End]
    
    lcTskUsr = laAllUsr[puTskUsr,1]
    SHOW GET puTskUsr
ENDCASE

*!***********************************************************************
*! Name      : lfvTskCtId
*! Developer : Reham Al-Allamy
*! Date      : 05/12/1996
*! Purpose   : Funcion called from the contact type popup.
*!***********************************************************************
*! Calls     : lfTskRef
*!***********************************************************************
*! Example   :  =lfvTskCtId()
*!***********************************************************************
*
FUNCTION lfvTskCtTy

*C200027,4 Save the old value of the contact type.
lcOldVal = lcTskCtId

*C200027,4 Validate the contact type popup.
DO CASE
  CASE _DOS .OR. _UNIX
    lcTskCtId = SUBSTR(gfActPop(2,5,9,18,'laContTyp',1,1,@lcTskCtTyp),1,1)
    =lfTskRef()
  CASE _WINDOWS .OR. _MAC
    lcTskCtId = laContTyp[puTskCtTy,2]
    SHOW GET puTskCtTy
ENDCASE

IF lcTskCtId = "N" .AND. rbTskTyp <> 3
  *C200027,4 If choose contact type "None" & you are not scheduling
  *C200027,4 a todo task..
  =gfDialog("I","You can choose this contact type only if you are scheduling a todo task.")
  *C200027,4 Restore the old value.
  lcTskCtId  = lcOldVal
  lcTskCtTyp = laContTyp[ASUBS(laContTyp,ASCAN(laContTyp,lcCntTyp),1),2]
  puTskCtTy  = ASUBS(laContTyp,ASCAN(laContTyp,lcCntTyp),1)
  *C200027,4 Refresh the variable.
  DO CASE
    CASE _DOS .OR. _UNIX
      =lfTskRef()
    CASE _WINDOWS .OR. _MAC
      SHOW GET puTskCtTy
  ENDCASE
ENDIF

*C200027,4 Refresh the contact Id according to the contact type.
IF !(lcOldVal == lcTskCtId)
  *C200027,4 If the contact type was customer or vendor.
  IF lcTskCtId = "C" .OR. lcTskCtId = "V"
    IF lcTskCtId = lcCntTyp
      *C200027,4 If the selected type is the same as the passed parameters
      *C200027,4 Restore the default passed parameters..
      lcContId   = lcCont_Id
      IF lcTskCtId = "C" 
        lcCnIdDesc = LOOKUP(CUSTOMER.StName,"M"+SUBSTR(lcContId,1,5),CUSTOMER.ACCOUNT,"CUSTOMER")
      ELSE
*        IF llApLink
          lcCnIdDesc = LOOKUP(APVENDOR.CVENCOMP,lcContId,APVENDOR.CVENDCODE,"VENCODE")
*        ELSE
*          lcCnIdDesc = LOOKUP(VENDOR.Name,lcContId,VENDOR.VENDOR,"VENDOR")
*        ENDIF
      ENDIF
      lcStore    = IIF(lcTskCtId = "C" , lcAccStore , SPACE(8) )
    ELSE
      *C200027,4 Blank the contact Id & its name & the store.
      lcContId   = SPACE(8)
      lcCnIdDesc = " "
      lcStore    = SPACE(8)
    ENDIF
    IF EMPTY(lcContId)
      lcContId   = SPACE(8)
      lcCnIdDesc = " "
    ENDIF

    IF EMPTY(lcStore)
      lcStore    = SPACE(8)
    ENDIF
    
    *C200027,4 Enable the contact Id, its browse icon, its name.
    SHOW GET ibKeyBrow1 ENABLE
    SHOW GET lcContId   ENABLE
    SHOW GET lcCnIdDesc ENABLE
    IF lcTskCtId = "C" AND LEFT(laContTyp[1,1],1)='C'
      *C200027,4 If contact type was customer, enable the store object
      *C200027,4 and its browse icon.
      SHOW GET ibKeyBrow2 ENABLE
      SHOW GET lcStore    ENABLE
    ELSE
      *C200027,4 If contact type was vendor, disable the store object
      *C200027,4 and its browse icon.
      SHOW GET ibKeyBrow2 DISABLE
      SHOW GET lcStore    DISABLE
    ENDIF
    *C200027,4 Point to the contact id.
    _CUROBJ = OBJNUM(lcContId)
  ELSE
    *C200027,4 If the contact type was other or none..
    *C200027,4 Blank the contact id, its name, its store.
    lcContId   = SPACE(8)
    lcCnIdDesc = " "
    lcStore    = SPACE(8)
    *C200027,4 Disable the contact id, its browse icon and its name.
    *C200027,4 Disable the store and its browse icon.
    SHOW GET ibKeyBrow1 DISABLE
    SHOW GET lcContId   DISABLE
    SHOW GET lcCnIdDesc DISABLE
    SHOW GET ibKeyBrow2 DISABLE
    SHOW GET lcStore    DISABLE
    *C200027,4 Point to the contact name field.
    _CUROBJ = OBJNUM(lcTskCont)
  ENDIF
  *C200027,4 Blank the contact & the phone.
  lcTskCont = SPACE(30)
  lcTskPhn  = " "
  
  *B606742,1 AMH Enable the contact popup in the vendor case [Start]
  *IF lcTskCtId = "C"
  IF lcTskCtId $ "CV"
  *B606742,1 AMH [End]
  
    SHOW GET lcTskCont DISABLE
    SHOW GET puTskCont ENABLE
    SHOW GET ibTskCont ENABLE
  ELSE
    SHOW GET puTskCont DISABLE
    SHOW GET ibTskCont DISABLE
    SHOW GET lcTskCont ENABLE
  ENDIF

  *C200027,4 Flag in the when function of the contact popup.
  llInv     = IIF(lcTskCtId $ "CV" , .T. , .F.)
  lcInvStat = IIF(lcTskCtId $ "CV" , "ENABLE" , "DISABLE")

  *C200027,4 Flag in the when function of the contact field.
  *B800704,1 M.H 08/11/96 Begin.
  *llGet     = IIF(lcTskCtId $ "CV" , .F. , .T.)
  *lcGetStat = IIF(lcTskCtId $ "CV" , "DISABLE" , "ENABLE")
  
  *B606742,1 AMH Consider case of vendor [Start]
  *llGet     = IIF(lcTskCtId = "C" , .F. , .T.)
  *lcGetStat = IIF(lcTskCtId = "C" , "DISABLE" , "ENABLE")
  llGet     = IIF(lcTskCtId $ "CV" , .F. , .T.)
  lcGetStat = IIF(lcTskCtId $ "CV" , "DISABLE" , "ENABLE")
  *B606742,1 AMH [End]
  
  *B800704,1 M.H 08/11/96 End.

  *C200027,4 Variable hold an arrow if contact id is accout or vendor 
  *C200027,4 and hold a space if contact id is other or none.
  lcArrow = IIF(lcTskCtId = "C" , "" , " ")
  *C200027,4 Refresh the contact and the phone.
  IF _DOS
    SHOW GET lcTskCont
    SHOW GET ibTskCont
    SHOW GET lcArrow
    SHOW GET lcTskPhn
  ENDIF
ENDIF
*!***********************************************************************
*! Name      : lfwActBrow
*! Developer : Reham Al-Allamy
*! Date      : 05/05/1996
*! Purpose   : Check if mouse clicked or not.
*!***********************************************************************
*! Example   :  =lfwActBrow()
*!**************************************************************************
*
FUNCTION lfwActBrow

RETURN MDOWN()

*!***********************************************************************
*! Name      : lfvActBrow
*! Developer : Reham Al-Allamy
*! Date      : 05/05/1996
*! Purpose   : Fill browse flag with true and activate field validation.
*!***********************************************************************
*! Passed Parameters  : lcObjName
*!***********************************************************************
*! Example            :  =lfvActBrow()
*!**************************************************************************
*
FUNCTION lfvActBrow
PARAMETERS lcObjName

_CUROBJ  = OBJNUM(&lcObjName)
llBrowse = .T.
KEYBOARD "{ENTER}"

*!***********************************************************************
*! Name      : lfvTskCnId
*! Developer : Reham Al-Allamy
*! Date      : 05/06/1996
*! Purpose   : Valid function for the customer Id
*!***********************************************************************
*! Calls     : lfTskRef
*!           : CUSBROWM
*!           : gfDialog
*!***********************************************************************
*! Example   :  =lfvTskCnId()
*!***********************************************************************
*
FUNCTION lfvTskCnId

*C200027,4 If the new value is different.
IF !(lcOldCnId == lcContId) .OR. llBrowse
  DO CASE
    *C200027,4 If the contact type was customer.
    CASE lcTskCtId = "C" 
      SELECT CUSTOMER
      *C200027,4 and the user did not activate browse.
      IF EMPTY(lcContId) .AND. !llBrowse
        lcContId   = SPACE(8)
        lcCnIdDesc = " "
      ELSE
        *C200027,4 check if there is any customer in the file.
        IF !SEEK('M')
          =gfDialog("I","Customer file is empty. No records to browse.")
          lcContId   = SPACE(8)
          lcCnIdDesc = " "
        ELSE
          IF !EMPTY(lcContId) .OR. llBrowse
            *C200027,4 Seek in case of search by account.
            IF SEEK("M"+SUBSTR(lcContId,1,5)) .AND. !llbrowse
              lcContId   = Account+SPACE(3)
              lcCnIdDesc = StName
            ELSE
              *C200027,4 Activate global function customer browse.
              IF CUSBROWM(SUBSTR(lcContId,1,5),'','M')
                lcContId   = Account+SPACE(3)
                lcCnIdDesc = StName
              ELSE
                lcContId   = lcOldCnId
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    *C200027,4 If the contact type was vendor.
    CASE lcTskCtId = "V"
*      IF llApLink
        *** Vendor browse from the other system vendor file
      *B606742,1 AMH Fix the bug of not validate the vendor code correctly [Start]
      IF EMPTY(lcContId) .AND. !llBrowse
        lcContId   = SPACE(8)
        lcCnIdDesc = " "
      ELSE
        IF llBrowse .OR. !SEEK(PADR(lcContId,8),'APVENDOR')
        = gfApVnBrow (@lcContId)
        ENDIF
        lcCnIdDesc = IIF(EMPTY(lcContId) , " " , APVENDOR.CVENCOMP)
      ENDIF
      *B606742,1 AMH [End]
*      ELSE
*        *** Vendor browse if no gl link is established
*        DO VENBROW WITH lcContId
*        lcCnIdDesc = IIF(EMPTY(lcContId) , " " , VENDOR.NAME )
*      ENDIF
  ENDCASE
  lcTskCont  = SPACE(30)
  lcTskPhn   = " "
ENDIF

*C200027,4 Disable the contact popup if there is no account.
lcContStat = IIF(EMPTY(lcContId) , "DISABLE" , "ENABLE")

SHOW GET ibTskCont &lcContStat

*B606742,1 AMH Consider case of Vendor [Start]
IF lcTskCtId $ "CV" .AND. !MDOWN()
  _CUROBJ = OBJNUM(puTskCont)
ENDIF
*B606742,1 AMH [End]

lcStore    = IIF(lcTskCtId = "C" , lcStore  , SPACE(8))
lcStrStat  = IIF(lcTskCtId = "C" , "ENABLE" , "DISABLE" )
SHOW GET lcContId
SHOW GET lcCnIdDesc
SHOW GET ibKeyBrow2 &lcStrStat
SHOW GET lcStore    &lcStrStat

*C200027,4 Function to fill the contact array.
IF !(lcOldCnId == lcContId)
  lcTskCont  = SPACE(30)
  =lfFillCont()
  *C200027,4 Refrash the contact name.
  SHOW GET lcTskCont
ENDIF
SHOW GET lcTskPhn

llBrowse = .F.

*!***********************************************************************
*! Name      : lfvStore
*! Developer : Reham Al-Allamy
*! Date      : 05/12/1996
*! Purpose   : Valid function for the store field.
*!***********************************************************************
*! Example   :  =lfvStore()
*!***********************************************************************
*
FUNCTION lfvStore
IF '?' $ lcStore
  llBrowse = .T.
ENDIF

IF !(lcOldStore == lcStore) .OR. llBrowse
  *C200027,4 Check if there is any accounts in the file
  *C200027,4 then check if the account selected has stores.
  SELECT CUSTOMER
  IF !SEEK('M',"CUSTOMER")
    =gfDialog("I","Customer file is empty. No records to browse.")
    RETURN
  ELSE
    IF !SEEK('S'+SUBSTR(lcContId,1,5),"CUSTOMER")
      =gfDialog("I","There is no stores for this account")
      lcStore = SPACE(8)
      SHOW GET lcStore
      RETURN
    ENDIF
  ENDIF
  
  xAccount = SUBSTR(lcContId,1,5)
  xStore   = lcStore
  IF SEEK("S"+SUBSTR(lcContId,1,5)+lcStore,"CUSTOMER") .AND. !llBrowse
    lcStore = CUSTOMER.Store
  ELSE
    *C200027,4 Call the store browse to browse the current account stores.
    IF CUSBROWS(xAccount,.F.)
      lcStore = CUSTOMER.Store
    ELSE
      lcStore = lcOldStore
    ENDIF
  ENDIF
  =lfFillCont()
  SHOW GET lcStore
ENDIF
llBrowse = .F.

*!***********************************************************************
*! Name      : lfvTskCont
*! Developer : Reham Al-Allamy
*! Date      : 05/06/1996
*! Purpose   : Valid function for the contact popup.
*!***********************************************************************
*! Calls     : lfTskRef
*!***********************************************************************
*! Example   :  =lfvTskCont()
*!***********************************************************************
*
FUNCTION lfvTskCont

*C200027,4 Validate the contact name.

*C200027,4 If the contact type was customer or vendor, call the popup
*C200027,4 that hold all the contacts to select one.
*B800704,1 M.H 08/11/96 Begin.
*IF lcTskCtId $ "CV"

*B606742,1 AMH Consider case of vendor [Start]
*IF lcTskCtId = "C"
IF lcTskCtId $ "CV"
*B606742,1 AMH [End]

*B800704,1 M.H 08/11/96 End.
  DO CASE
    CASE _DOS .OR. _UNIX
      lcTskPhn  = gfActPop(3,40,10,72,'laContact',2,1,@lcTskCont)
      SHOW GET lcTskCont
    CASE _WINDOWS .OR. _MAC
      lcTskPhn = laContact[puTskCont,2]
      lcTskCont = laContact[puTskCont,1]
*      lcTskPhn  = gfActPop(5.10,50,12,94.5,'laContact',2,1,@lcTskCont)
*      puTskCont = lcTskCont
*      SHOW GET puTskCont ENABLE
*      SHOW GET ibTskCont ENABLE
  ENDCASE
  
  *B602697,1 Reham On 03/25/99   *** Begin ***
  *B602697,1 Use Customer phones if the contacts' are empty.
  IF EMPTY(ALLTRIM(lcTskPhn)) .AND. (!EMPTY(lcPhone1) .OR. !EMPTY(lcPhone2))
    *** The selected contact phone is empty.  Do you wish to use the customer phones? ***
    *** <  Yes  > - <  No  > ***
    IF gfModalGen("QRM00348B00006" , "DIALOG") = 1
      lcTskPhn = IIF(!EMPTY(lcPhone1) , lcPhone1 , lcPhone2)
    ENDIF
  ENDIF
  *B602697,1 Reham On 03/25/99   *** End   ***
  
  IF EMPTY(lcTskCont)
    lcTskPhn  = ''
  ENDIF
  SHOW GET lcTskPhn
ELSE
  *C200027,4 If the contact type was other or none, just refresh the object.
  SHOW GET lcTskCont
ENDIF

*!***********************************************************************
*! Name      : lfvResourc
*! Developer : Reham Al-Allamy
*! Date      : 05/20/1996
*! Purpose   : Valid function for the invisable button recorces to add new
*!           : resources or edit existing resources...
*!***********************************************************************
*! Example   :  =lfvResourc()
*!***********************************************************************
*
FUNCTION lfvResourc

lcRes = SPACE(3)

*C200027,4 Call the global function that validate codes to add new
*C200027,4 resources or editting new resources.
*DO gpCodeChek IN CUS100.PRG WITH "Y" , lcRes , "Resources" , .T. , .T. , .T.

*C200027,4 Reselect the available resources just in case if any resources
*C200027,4 was added.
DECLARE laAllRes[1]
laAllRes = " "
**SELECT CODE
*SELECT SUBSTR(CODE.CODE,2,3) + " " + CODE.cData ;
  FROM (gcDataDir+"CODE") ;
 WHERE SUBSTR(CODE.CODE,1,1) = "Y" ;
  INTO ARRAY laAllRes

SELECT CODES
*E300789,1 Hesham (Start)
*SELECT CODES.CCODE_NO + " " + CODES.cDiscrep ;
  FROM (gcDataDir+"CODES") ;
 WHERE CODES.CFLD_NAME = "CRESO_CODE" ;
       AND CRLTFIELD='N' AND !EMPTY(CCOMP_ID);
  INTO ARRAY laAllRes

SELECT CODES.CCODE_NO + " " + CODES.cDiscrep ;
  FROM (gcDataDir+"CODES") ;
 WHERE CODES.CFLD_NAME = "CRESO_CODE" ;
       AND CRLTFIELD='N' AND CDEFCODE='N';
  INTO ARRAY laAllRes

*E300789,1 Hesham (END)
*!***********************************************************************
*! Name      : lfvSelect
*! Developer : Reham Al-Allamy
*! Date      : 05/08/1996
*! Purpose   : Valid function for select push button.
*!***********************************************************************
*! Example   :  =lfvSelect()
*!***********************************************************************
*
FUNCTION lfvSelect

*C200027,4 If empty of the array that hold the available resources.
IF EMPTY(laAllRes[1])
  =gfDialog("I","There is no resources to be displayed.")
ELSE
  *C200027,4 Call the mover to select resources.
  =gfMover(@laAllRes,@laResource,"Select Resources",.T.)
  SHOW GET lsResource
ENDIF

*!***********************************************************************
*! Name      : lfFillCont
*! Developer : Reham Al-Allamy
*! Date      : 05/12/1996
*! Purpose   : Function to fill the contact array called from the 
*!           : begining of the program & the contact ID validation.
*!***********************************************************************
*! Example   :  =lfFillCont()
*!***********************************************************************
*
FUNCTION lfFillCont

*C200027,4 If there an account get his contacts from the CONTACT file.
DECLARE laContact[1,2]
laContact = " "

IF !EMPTY(lcContId)
  *C200027,4 Get all the contacts from the CONTACT file.
  _TALLY = 0
  SELECT CONTACT
  SELECT PADR(contact,LEN(CONTACT)) , Phone;
    FROM (gcDataDir+"CONTACT") ;
   WHERE cconttype + ccont_id + Store = ;
         lcTskCtId + lcContId + lcStore ;
    INTO ARRAY laContact
  IF lcTskCtId = "C"
    *B602697,1 Reham On 03/25/99   *** Begin ***
    *IF _TALLY > 0
    *  *C200027,4 Redimension the array to add the account first name.
    *  DIMENSION laContact[ALEN(laContact,1)+1,2]
    *  =AINS(laContact,1)
    *ENDIF
    *C200027,4 Add the buyer to the contacts list.
    *IF 'AR' $ gcCmpModul
    *  laContact[1,1] = PADR(LOOKUP(CUSTOMER.Buyer,"M"+SUBSTR(lcContId,1,5),CUSTOMER.ACCOUNT,"CUSTOMER"),30)
    *  laContact[1,2] = LOOKUP(CUSTOMER.Phone1,"M"+SUBSTR(lcContId,1,5),CUSTOMER.ACCOUNT,"CUSTOMER")
    *  *C200027,4 Redimension the array to add the account first name.
    *  DIMENSION laContact[ALEN(laContact,1)+1,2]
    *  =AINS(laContact,1)
    *  *C200027,4 Add the b.keeper to the contacts list.
    *  laContact[1,1] = PADR(LOOKUP(CUSTOMER.Keeper,"M"+SUBSTR(lcContId,1,5),CUSTOMER.ACCOUNT,"CUSTOMER"),30)
    *  laContact[1,2] = LOOKUP(CUSTOMER.Phone2,"M"+SUBSTR(lcContId,1,5),CUSTOMER.ACCOUNT,"CUSTOMER")
    *ENDIF
    *B602697,1 Prevent adding blank lines in the contact popup.
    IF 'AR' $ gcCmpModul
      IF SEEK("M"+SUBSTR(lcContId,1,5) , "CUSTOMER")
        lcPhone1 = IIF(!EMPTY(CUSTOMER.Phone1) , CUSTOMER.Phone1 , "")
        lcPhone2 = IIF(!EMPTY(CUSTOMER.Phone2) , CUSTOMER.Phone2 , "")
        IF !EMPTY(CUSTOMER.Buyer)
          IF !EMPTY(laContact[1,1])
            DIMENSION laContact[ALEN(laContact,1)+1,2]
            =AINS(laContact,1)
          ENDIF
          laContact[1,1] = PADR(CUSTOMER.Buyer,30)
          laContact[1,2] = CUSTOMER.Phone1
        ENDIF
        IF !EMPTY(CUSTOMER.Keeper)
          IF !EMPTY(laContact[1,1])
            DIMENSION laContact[ALEN(laContact,1)+1,2]
            =AINS(laContact,1)
          ENDIF
          laContact[1,1] = PADR(CUSTOMER.Keeper,30)
          laContact[1,2] = CUSTOMER.Phone2
        ENDIF
      ENDIF
    ENDIF
    IF EMPTY(laContact[1,1])
      laContact[1,1] = "N/A"
      laContact[1,2] = ""
    ENDIF
    *B602697,1 Reham On 03/25/99   *** End   ***
  ENDIF
ENDIF
SHOW GET ibTskCont &lcContStat

*B602697,1 Reham On 03/25/99   *** Begin ***
*B602697,1 Refresh the contact popup to show all contacts.
SHOW GET puTskCont &lcContStat
*B602697,1 Reham On 03/25/99   *** End   ***

*!***********************************************************************
*! Name      : lfvCodeKey
*! Developer : Reham Al-Allamy
*! Date      : 05/14/1996
*! Purpose   : Valid function for the all codes.
*!***********************************************************************
*! Example   :  =lfvCodeKey()
*!***********************************************************************
*
FUNCTION lfvCodeKey
PARAMETERS lcCodTyp , lcCurFld , lcCurTit , lcDesc

IF !(lcOldVal == &lcCurFld) .OR. llBrowse
  SELECT CODE
  IF SEEK(lcCodTyp + &lcCurFld) .AND. !llBrowse
    &lcDesc = CODE.cData
  ELSE
    DO gpCodeChek IN CUS100.PRG WITH lcCodTyp , &lcCurFld , lcCurTit , .T. , .T. , .T.
    &lcDesc = LOOKUP(CODE.cData,lcCodTyp+SUBSTR(&lcCurFld,1,3),CODE.CODE,"CODE")
  ENDIF
  SHOW GET &lcCurFld
  SHOW GET &lcDesc
  llBrowse = .F.
ENDIF

*!***********************************************************************
*! Name      : lfvComplet
*! Developer : Reham Al-Allamy
*! Date      : 05/07/1996
*! Purpose   : Valid function for complete push button.
*!***********************************************************************
*! Example   :  =lfvComplet()
*!***********************************************************************
*
FUNCTION lfvComplet

*lcTskTypWnd = WONTOP()  && M.H
lcTskTypWnd = "TASKTYP"

*C200027,4 Confirm entering all the needed data.
IF EMPTY(lcTskUsr)
  =gfDialog("I","You have to enter the user Id before saving.")
  llClrRead = .F.
  RETURN
ENDIF
*C200027,4 If empty contact Id & the task was appointment or call &
*C200027,4 the contact type was customer or vendor
IF EMPTY(lcContId) .AND. (rbTskTyp = 1 .OR. rbTskTyp = 2) ;
   .AND. lcTskCtId $ "CV"
  =gfDialog("I","You have to enter the contact Id before saving.")
  llClrRead = .F.
  RETURN
ENDIF
*C200027,4 If the contact name is empty & the contact type was
*C200027,4 customer , vendor or other...
IF EMPTY(lcTskCont) .AND. lcTskCtId $ "CVO"
  =gfDialog("I","You have to enter the contact name before saving.")
  llClrRead = .F.
  RETURN
ENDIF
IF EMPTY(lcTskDat)
  =gfDialog("I","You have to enter the task date before saving.")
  llClrRead = .F.
  RETURN
ENDIF

*C200027,4 Flag to know if complete the current task or not.
llComplete = .F.
*C200027,4 Flag to know that the calling from the task screen.
llFromTask = .T.

*C200027,4 Flag to know if schedule a new call or not.
llNewTask  = .F.

*C200027,4 lcMode     -> "M" for modify or "A" for add
*C200027,4 lcSeq      -> If modify mode send the current sequence no.
*C200027,4 rbTskTyp   -> 1 for call , 2 for appointment , 3 for todo.
*C200027,4 llFromTask -> True if coming from task screen.

*C200027,4 Call the complete task screen.
DO (gcAppHome+gcWinAppl+'\TASKCOM.PRG') WITH lcMode , lcSeq , rbTskTyp , llFromTask

*C200027,4 If complete the current task, close the task type screen.
DO CASE
  CASE llComplete .AND. !llNewTask
    llClrRead = .T.
    RELEASE WINDOW (lcTskTypWnd)
  CASE llComplete .AND. llNewTask
    llClrRead = .F.
    *C200027,4 Convert the mode to be add mode.
    lcMode = "A"
    
    *C200027,4 Define the current task date.
    lcTskDat   = DATE()
    
    *C200027,4 Define the current task time.
    IF VAL(SUBSTR(TIME(),1,2)) < 12
      lcTskTim   = SUBSTR(TIME(),1,5) + " AM"
    ELSE
      lcHour     = IIF(EVAL(SUBSTR(TIME(),1,2))-12=0,;
                       SUBSTR(TIME(),1,2),ALLTRIM(STR(EVAL(SUBSTR(TIME(),1,2))-12)))
      lcTskTim   = IIF(LEN(lcHour)=1,'0','') + lcHour + SUBSTR(TIME(),3,3)+" PM"
    ENDIF
    
    *C200027,4 Define the current task duration.
    lnTskDur   = 0
    
    *C200027,4 Variable to know if the task open or complete.
    lcComplete = "N"
    
    SHOW GET lcTskDat
    SHOW GET lcTskTim
    SHOW GET lnTskDur
    
    *C200027,4 Array hold the resources for the current task.
    DECLARE laResource[1]
    STORE " " TO laResource
    SHOW GET lsResource
ENDCASE

*!***********************************************************************
*! Name      : lfvTskOk
*! Developer : Reham Al-Allamy
*! Date      : 05/06/1996
*! Purpose   : Valid function for task type Ok push button.
*!***********************************************************************
*! Example   :  =lfvTskOk()
*!***********************************************************************
*
FUNCTION lfvTskOk

*C200027,4 Confirm entering all the needed data.
IF EMPTY(lcTskUsr)
  =gfDialog("I","You have to enter the user Id before saving.")
  _CUROBJ = OBJNUM(ibTskUsr)
  llClrRead = .F.
  RETURN
ENDIF

*C200027,4 If empty contact Id & the task was appointment or call &
*C200027,4 the contact type was customer or vendor
IF EMPTY(lcContId) .AND. (rbTskTyp = 1 .OR. rbTskTyp = 2) ;
   .AND. lcTskCtId $ "CV"
  =gfDialog("I","You have to enter the contact Id before saving.")
  _CUROBJ = OBJNUM(lcContId)
  llClrRead = .F.
  RETURN
ENDIF

*B602697,1 Reham On 03/25/99   *** Begin ***
*B602697,1 Do not accept contact ID if "N/A"
*C200027,4 If the contact name is empty & the contact type was
*C200027,4 customer , vendor or other...
*IF EMPTY(lcTskCont) .AND. lcTskCtId $ "CVO"
IF (EMPTY(lcTskCont) .OR. lcTskCont= "N/A") .AND. lcTskCtId $ "CVO"
*B602697,1 Reham On 03/25/99   *** End   ***
  =gfDialog("I","You have to enter the contact name before saving.")
  IF lcTskCtId $ "CV"
    _CUROBJ = OBJNUM(puTskCont)
  ELSE
    _CUROBJ = OBJNUM(lcTskCont)
  ENDIF
  llClrRead = .F.
  RETURN
ENDIF

*C200027,4 If the task date was empty.
IF EMPTY(lcTskDat)
  =gfDialog("I","You have to enter the task date before saving.")
  _CUROBJ = OBJNUM(lcTskDat)
  llClrRead = .F.
  RETURN
ENDIF

*C200027,4 See if there is conflict between resources or not in the case
*C200027,4 of scheduling an appointment.
IF !EMPTY(laResource[1]) .AND. rbTskTyp = 2 .AND. ;
   (VAL(SUBSTR(lcTskTim,1,2))*60)+VAL(SUBSTR(lcTskTim,4,2)) > 0 .AND. lnTskDur > 0
  *C200027,4 Get the current task time in minutes.
  lnCurTim = (IIF("PM" $ lcTskTim .AND. VAL(SUBSTR(lcTskTim,1,2))<> 12 ,12,0)+VAL(SUBSTR(lcTskTim,1,2))) * 60 + ;
              VAL(SUBSTR(lcTskTim,4,2))

  *C200027,4 Set relation between the resources & the schedule files
  *C200027,4 on the sequence no.
  SELECT syschdul
  
  *C200027,4 Get the current task sequence no.
  lcCurTsk = IIF(lcMode = "A" , " " , syschdul.cSeqNumber)

  SET ORDER TO TAG CSEQNUM
  SELECT RESOURCE
  SET RELATION TO RESOURCE.cseqnumber INTO syschdul ADDITIVE
  *C200027,4 Flag to know if there is conflict or not.
  llConflict = .F.

  *C200027,4 Scan in the resources files for only the resources 
  *C200027,4 that exist in the selected resources array.
  SCAN FOR ASCAN(laResource , RESOURCE.creso_code) > 0
    *C200027,4 If any of the selected resources used in the same date
    *C200027,4 & the same time, warn the user.
    IF syschdul.dtrandate = lcTskDat .AND. ;
      BETWEEN((IIF("PM" $ lcTskTim,12,0)+VAL(SUBSTR(lcTskTim,1,2)))* 60+VAL(SUBSTR(syschdul.ctrantime,4,2)),;
      lnCurTim,lnCurTim+lnTskDur) .AND. cSeqNumber <> lcCurTsk 
      *C200027,4 Set flag to true.
      llConflict = .T.
      EXIT
    ENDIF
  ENDSCAN

  *C200027,4 Remove the relation.
  SELECT RESOURCE
  SET RELATION TO
  *C200027,4 If there is conflict.
  IF llConflict
    *C200027,4 Give the user a warnning message to ignore or reselect
    *C200027,4 resources.
    IF gfDialog("?","Resources conflict...","\<Ignore;\<Edit") = 2
      _CUROBJ = OBJNUM(pbSelect)
      llClrRead = .F.
      RETURN
    ENDIF
  ENDIF
ENDIF

*C200027,4 See if there is any time conflict or not.
*C200027,4 Get the current time in minutes.
lnCurTim = (IIF("PM" $ lcTskTim .AND. VAL(SUBSTR(lcTskTim,1,2))<> 12 ,12,0)+VAL(SUBSTR(lcTskTim,1,2))) * 60 + ;
            VAL(SUBSTR(lcTskTim,4,2))

*C200027,4 Get the current task sequence no.
lcCurTsk = IIF(lcMode = "A" , " " , syschdul.cSeqNumber)
  
llFound = .F.
SELECT syschdul
SET ORDER TO TAG SCHUSER
DO CASE
  *C200027,4 If empty of current time.
  CASE EMPTY(lnCurTim) .AND. rbTskTyp <> 3
    LOCATE FOR cUser_ID+DTOS(DTRANDATE)+CCOMPLETED = lcTskUsr+DTOS(lcTskDat)+"N" ;
       .AND. !(cSeqNumber == lcCurTsk) .AND. BETWEEN(lnTskDur,;
       (VAL(SUBSTR(ctrantime,1,2))*60)+VAL(SUBSTR(ctrantime,4,2)),;
       (VAL(SUBSTR(ctrantime,1,2))*60)+VAL(SUBSTR(ctrantime,4,2))+nestdur)
    llFound = FOUND()
  *C200027,4 If not empty of current time.
  CASE !EMPTY(lnCurTim)
    =SEEK(lcTskUsr)

    SCAN REST FOR cUser_ID+DTOS(dTranDate)+cCompleted = lcTskUsr+DTOS(lcTskDat)+"N"
      IF cSeqNumber <> lcCurTsk 
        IF "PM" $ cTranTime .AND. VAL(SUBSTR(lcTskTim,1,2))<> 12
          lnFileStrt = (VAL(SUBSTR(cTranTime,1,2)) + 12 )* 60 + VAL(SUBSTR(cTranTime,4,2))
        ELSE
          lnFileStrt = (VAL(SUBSTR(cTranTime,1,2))      )* 60 + VAL(SUBSTR(cTranTime,4,2))
        ENDIF
        lnFileEnd  = lnFileStrt + nEstDur
        IF BETWEEN(lnCurTim+1,lnFileStrt,lnFileEnd)
          llFound  = .T.
          EXIT
        ENDIF
      ENDIF
    ENDSCAN
ENDCASE
  
IF llFound
  *C200027,4 If there is time conflict, give the user a warning message.
  IF gfDialog("?","Time conflict.","\<Ignore;\<Edit") = 2
    _CUROBJ  = OBJNUM(lcTskTim)
    llClrRead = .F.
    RETURN
  ENDIF
ENDIF

SELECT syschdul
llFound = .F.
DO CASE
  *C200027,4 If add mode.
  CASE lcMode = "A"
    SET DELETE OFF
    IF SEEK(" ") AND DELETED()
      RECALL
      BLANK
    ELSE
      APPEND BLANK
    ENDIF
    SET DELETE ON
*    lcSeq = gfSequence('CSEQNUMBER')
    REPLACE cseqnumber WITH gfSequence('CSEQNUMBER')
    llFound = .T.
  *C200027,4 If editting an existing record.
  CASE lcMode = "M"
    LOCATE FOR cseqnumber = lcSeq AND CCOMP_ID=gcAct_Comp
    IF FOUND()
      llFound = .T.
    ENDIF
ENDCASE

IF llFound
  *C200027,4 Update the task record.
*  REPLACE user_id    WITH lcTskUsr ;
          cconttype  WITH lcTskCtId ;
          ccont_id   WITH lcContId ;
          store      WITH lcStore ;
          contact    WITH lcTskCont ;
          cphone     WITH lcTskPhn ;
          csubject   WITH lcTskSub ;
          cpriority  WITH IIF(rbTskTyp = 3 , lcTskPrt , " " ) ;
          ctrantype  WITH SUBSTR("CAT",rbTskTyp,1) ;
          dtrandate  WITH lcTskDat ;
          ctrantime  WITH lcTskTim ;
          nestdur    WITH lnTskDur ;
          ctranreson WITH lcRsn ;
          mnotes     WITH lcTskNote ;
          ccompleted WITH lccomplete ;
          add_userid WITH gcUser_ID ;
          add_date   WITH DATE() ;
          add_time   WITH TIME()
*C200027,8 M.H Begin.  
  REPLACE cuser_id    WITH lcTskUsr ;
          cconttype  WITH lcTskCtId ;
          ccont_id   WITH lcContId ;
          store      WITH lcStore ;
          contact    WITH lcTskCont ;
          cphone     WITH lcTskPhn ;
          csubject   WITH lcTskSub ;
          cpriority  WITH lcTskPrt ;
          ctrantype  WITH SUBSTR("CAT",rbTskTyp,1) ;
          dtrandate  WITH lcTskDat ;
          ctrantime  WITH lcTskTim ;
          nestdur    WITH lnTskDur ;
          ctranreson WITH lcRsn ;
          mnotes     WITH lcTskNote ;
          ccompleted WITH lccomplete ;
          cComp_ID   WITH gcAct_Comp;
          cadd_user WITH gcUser_ID ;
          dadd_date   WITH DATE() ;
          cadd_time   WITH TIME()
*C200027,8 M.H End.
  IF !EMPTY(laResource[1])
    SELECT RESOURCE
    SET ORDER TO TAG SEQUENCE
    *C200027,4 If editting an existing record.
    IF lcMode = "M"
      *C200027,4 Delete related resources.
      SELECT RESOURCE
      SCAN FOR RESOURCE.cseqnumber = syschdul.cseqnumber
        REPLACE cseqnumber WITH " " ;
                cReso_Code WITH " "
        DELETE
      ENDSCAN
    ENDIF

    FOR lnCount = 1 TO ALEN(laResource,1)
      SET DELETE OFF
      IF SEEK(" ") AND DELETED()
        RECALL
        BLANK
      ELSE
        APPEND BLANK
      ENDIF
      SET DELETE ON
      REPLACE cSeqNumber WITH syschdul.cSeqNumber ;
              cReso_Code WITH SUBSTR(laResource[lnCount],1,LEN(cReso_Code))
    ENDFOR
  ENDIF
ENDIF
IF lcMode = 'A'
  =gfDoTriger('SCHEDULE','INSERT')
ENDIF
llClrRead = .T.
*C200027,4 Close the session.
CLEAR READ

*:************************************************************************
*: Program file  : lfvClrRead.PRG
*: Program desc. : Clear read.
*:         Module: Aria Apparel Series.
*:      Developer: Mohamed Fahmy Mohamed (MFM)
*:************************************************************************
FUNCTION lfvClrRead
PARAMETERS lcFunToDo

lcRunFun = lcFunToDo
lnCurObj = _CUROBJ

CLEAR READ

*B800685,1 Reham 17/22/96    ** Begin **
*B800685,1 Reham 17/22/96 Remark the wrong calling of time validation
*B800685,1 Reham 17/22/96 because it give an error while compiling.
*lfvTskTime()
*B800685,1 Reham 17/22/96    ** End   **

*:************************************************************************
*: Program file  : lfvTskTime.PRG
*: Program desc. : Time validation.
*:         Module: Aria Apparel Series.
*:      Developer: Mohamed Hassan
*:************************************************************************
*B601162,1 M.H 07/16/96 Make the validation function for the time object.
*
FUNCTION lfvTskTime

DO CASE
  CASE VAL(SUBSTR(lcTskTim,1,2)) > 12
    =gfDialog('!','Invalid time')
    _CUROBJ = OBJNUM(lcTskTim)
    RETURN
    
  CASE VAL(SUBSTR(lcTskTim,4,2)) > 59
    =gfDialog('!','Invalid time')
    _CUROBJ = OBJNUM(lcTskTim)
    RETURN
  
*  CASE !(SUBSTR(lcTskTim,7,2) $ 'AMPM')
  CASE  SUBSTR(lcTskTim,7,2) <> 'AM' AND SUBSTR(lcTskTim,7,2) <>'PM'
    =gfDialog('!','Invalid time')
    _CUROBJ = OBJNUM(lcTskTim)
    RETURN
ENDCASE


FUNCTION lfTypShow

*B602697,1 Reham On 03/25/99   *** Begin ***
*=gfwCodePop(@laCodes,'CTRANRESON','T')
*=lfvReason()
*B602697,1 Reham On 03/25/99   *** End   ***

*E300711,1 HESHAM (Start)
*E300711,1 if the screen is running in view mode the disable all the 
*E300711,1 objects but the cancel push button
IF lcMode = 'V'
  SHOW GETS DISABLE ONLY
  SHOW GET pbCan ENABLE
ENDIF
*E300711,1 HESHAM (End)


FUNCTION lfvReason
lcRsn  = ALLTRIM(laReason[lnReason,2])

