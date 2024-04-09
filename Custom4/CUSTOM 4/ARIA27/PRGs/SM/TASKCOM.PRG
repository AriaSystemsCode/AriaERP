*:************************************************************************
*:
*: Procedure file: TASKCOM.PRG
*:               : Complete a schedule task.
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Aria Apparel System
*:           Date: May,16 1996
*:         Author: Reham Aly Al-Allamy
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:                 
*:      Documented  /  /
*:************************************************************************
*:  Modifications :
*:************************************************************************
*:E300711,1 HESHAM 07/28/97
*:E300711,1 add new mode to view data only without give the user the access
*:E300711,1 to modify data
*:B602697,1 Reham On 03/25/99   *** Begin ***
*:B602697,1 1- changing the subject & notes fonts to use MS Sans Serif font.
*:B602697,1 2- Get the right data for reason & result popups.
*:B122747,1 NNA 05/27/2004 fix bug that if you complete a task (on the User Task list) and flag 
*:B122747,1                it as unsuccessful and mark for Schedule Follow-up call. it will updates the 
*:B122747,1                task to the history but it will remove it from the Pending and that is the bug
*:************************************************************************
*
PARAMETERS lcMode , lcSeq , lnTyp , llFromTsk

DECLARE laCodes[2,10],laReason[1,2],laResult[1,2]
STORE '' TO laCodes
lnReason = 1
lnResult = 1


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

laCodes[2,1] = 'CRESULTCD'
laCodes[2,2] = 'laResult'
laCodes[2,3] = 'lnResult'
laCodes[2,4] = ''
laCodes[2,5] = .F.
laCodes[2,6] = .F.
laCodes[2,7] = 'SYSCHDUL'
laCodes[2,8] = 'Cseqnum'
laCodes[2,9] = 'lcSeq'
laCodes[2,10] = 'CRESULTCD'




*C200027,4 lcMode    -> "M" for modify or "A" for add
*C200027,4 lcSeq     -> If modify mode send the current sequence no.
*C200027,4 lnTyp     -> 1 for call , 2 for appointment , 3 for todo.
*C200027,4 llFromTsk -> True if coming from task screen.

*E300320,1 Store the bmps directories.
lcComBmp1 = gcBmpHome + "EXTKEY.BMP"
lcComBmp2 = gcBmpHome + "ZOOM.BMP"
lcComBmp3 = gcBmpHome + "OK.BMP"
lcComBmp4 = gcBmpHome + "CAN.BMP"

lcOldVal = " "
llBrowse = .F.

*C200027,4 Flag to know if open resources file in this sission or not.
llOpResc = .F.
*C200027,4 Open the resources file.
IF !USED("RESOURCE")
  USE gcDataDir+'RESOURCE' IN 0 ORDER TAG SEQUENCE
  *C200027,4 Set flag to true to close the resources file if closing this program.
  llOpResc = .T.
ENDIF

*C200027,4 Flag to know if open schedule file in this sission or not.
llOpScd = .F.
*C200027,4 Open the schedule file.
IF !USED("syschdul")
  =gfOpenFile(gcSysHome+'syschdul',gcSysHome+'SCHUSER','SH')
  *C200027,4 Set flag to true to close the schedule file if closing this program.
  llOpScd = .T.
ENDIF

*C200027,4 Flag to know if open CODE file in this sission or not.
llOpCod = .F.
*C200027,4 Open the CODE file.
IF !USED("CODES")
  =gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
  *C200027,4 Set flag to true to close the CODE file if closing this program.
  llOpCod = .T.
ENDIF

llOpUsr = .F.

IF !USED("SYUUSER")
  =gfOpenFile(gcSysHome+'SYUUSER',gcSysHome+'SYUUSER','SH')
  llOpUsr = .T.
ENDIF

SELECT SYUUSER.cUser_Id FROM (gcSysHome+"SYUUSER ") INTO ARRAY laAllUsr

DO CASE
  *E300711,1 HESHAM (Start)
  *E300711,1 add new mode to view data only without give the user the access
  *E300711,1 to modify data
*  CASE lcMode = "M" .AND. !llFromTsk  
  CASE (lcMode = "M" OR lcMode = "V").AND. !llFromTsk
  *E300711,1 HESHAM (End)  
    *C200027,4 Get the data for the editted record from the file,
    *C200027,4 If the record was completed, Get the completed data as default.
    *C200027,4 If the record was open, Get the open data as default.
    SELECT syschdul
    LOCATE FOR cseqnumber = lcSeq
    IF FOUND()
      lnTyp      = AT(ctrantype,"CAT")
      lcComUsr   = IIF(ccompleted = "N" , cUser_ID , ccmpltuser)
      lcComDat   = IIF(ccompleted = "N" , DATE()  , dcmpltdate)
      lnComDur   = IIF(ccompleted = "N" , nestdur , nactdur)
      lcComSub   = cSubject
      lcComNote  = mNotes
      lcComRsn   = cTranReson
*      lcComRsnDs = LOOKUP(CODE.cData,"W"+lcComRsn,CODE.CODE,"CODE")
      lcComRst   = cresultcd
*      lcComRstDs = LOOKUP(CODE.cData,"X"+lcComRst,CODE.CODE,"CODE")
      *B602697,1 Reham On 03/25/99   *** Begin ***
      =gfwCodePop(@laCodes,'CTRANRESON','V,'+lcComRsn)
      =gfwCodePop(@laCodes,'CRESULTCD','V,'+lcComRst)

      *B122747,1 NNA 05/27/2004 (Begin) 
      lcComRst   = ALLTRIM(laResult[lnResult,2])
      *B122747,1 NNA (End)

      *B602697,1 Reham On 03/25/99   *** End   ***
    ENDIF
  CASE llFromTsk
    lcComUsr   = lcTskUsr
    lcComDat   = DATE()
    lnComDur   = lnTskDur
    lcComSub   = lcTskSub
    lcComNote  = lcTskNote
    lcComRsn   = lcRsn
*    lcComRsnDs = lcRsn_Desc
*    lcComRst   = SPACE(3)
*    lcComRstDs = " "
    *B602697,1 Reham On 03/25/99   *** Begin ***
    =gfwCodePop(@laCodes,'CTRANRESON','V,'+lcComRsn)
    =gfwCodePop(@laCodes,'CRESULTCD','D')
    lcComRst   = ALLTRIM(laResult[lnResult,2])
    *B602697,1 Reham On 03/25/99   *** End   ***
ENDCASE

*C200027,4 Get the title of the complete screen.
DO CASE
  CASE lnTyp = 1
    lcTitle = "Call"
  CASE lnTyp = 2
    lcTitle = "Appointment"
  CASE lnTyp = 3
    lcTitle = "Todo"
ENDCASE

rbSuccess  = 1
cbFolCall  = 0

*C200027,4 Set flag to true to enable the zoom screen.
llEnable = .T.

PUComUsr = ASCAN(laAllUsr,lcComUsr)



*C200027,4 Call the complete task screen.
DO (gcScrDir+gcWinAppl+'\TASKCOM.SPX')

IF llOpResc .AND. USED("RESOURCE")
  USE IN RESOURCE
ENDIF

IF llOpScd .AND. USED("syschdul")
  USE IN syschdul
ENDIF

IF llOpCod .AND. USED("CODES")
  USE IN CODES
ENDIF

IF llOpUsr .AND. USED("SYUUSER")
  USE IN SYUUSER
ENDIF

*!***********************************************************************
*! Name      : lfvComUsr
*! Developer : Reham Al-Allamy
*! Date      : 05/09/1996
*! Purpose   : Valid function for complete user popup.
*!***********************************************************************
*! Example   :  =lfvComUsr()
*!***********************************************************************
*
FUNCTION lfvComUsr

DO CASE
  CASE _DOS .OR. _UNIX
    lcComUsr = gfActPop(2,5,9,18,'laAllUsr',1,1,@lcComUsr)
    =lfComRef()
  CASE _WINDOWS .OR. _MAC
    lcComUsr = laAllUsr[puComUsr,1]
    SHOW GET puComUsr
ENDCASE

*!***********************************************************************
*! Name      : lfwActBrow
*! Developer : Reham Al-Allamy
*! Date      : 05/16/1996
*! Purpose   : Check if mouse clicked or not.
*!***********************************************************************
*! Example   :  =lfwActBrow()
*!**************************************************************************
*
FUNCTION lfwActBrow

RETURN MDOWN()

*!***********************************************************************
*! Name      : lfvCodeKey
*! Developer : Reham Al-Allamy
*! Date      : 05/16/1996
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
*    DO gpCodeChek IN CUS100.PRG WITH lcCodTyp , &lcCurFld , lcCurTit , .T. , .T. , .T.
    &lcDesc = LOOKUP(CODE.cData,lcCodTyp+SUBSTR(&lcCurFld,1,3),CODE.CODE,"CODE")
  ENDIF
  SHOW GET &lcCurFld
  SHOW GET &lcDesc
  llBrowse = .F.
ENDIF

*!***********************************************************************
*! Name      : lfvSuccess
*! Developer : Reham Al-Allamy
*! Date      : 05/09/1996
*! Purpose   : Valid function for the success radio button.
*!***********************************************************************
*! Example   :  =lfvSuccess()
*!***********************************************************************
*
FUNCTION lfvSuccess

IF rbSuccess = 1
  cbFolCall = 0
ELSE
  cbFolCall = 1
ENDIF
SHOW GET cbFolCall

*!***********************************************************************
*! Name      : lfvComOk
*! Developer : Reham Al-Allamy
*! Date      : 05/09/1996
*! Purpose   : Valid function for complete Ok push button.
*!***********************************************************************
*! Example   :  =lfvComOk()
*!***********************************************************************
*
FUNCTION lfvComOk

llComplete = .T.
lcSeq = IIF(lcMode = "M" , lcSeq , gfSequence('CSEQNUMBER'))

lcComNote = lcComNote + IIF(syschdul.ccompleted = "N", ;
               IIF(EMPTY(lcComNote) , "" , CHR(13)) + ;
               IIF(rbSuccess = 1 , "Successful "+lcTitle , ;
                   "Unsuccessful "+lcTitle) , "")

DO CASE
  CASE lcMode = "M"
    IF syschdul.ccompleted = "N"
      *C200027,4 Delete related resources.
      SELECT RESOURCE
      SCAN FOR RESOURCE.cseqnumber = syschdul.cseqnumber
        REPLACE cseqnumber WITH " " ;
                cReso_Code WITH " "
        DELETE
      ENDSCAN
    ENDIF

    SELECT syschdul
    LOCATE FOR cseqnumber = lcSeq
    IF FOUND()
      
      *B122747,1 NNA 05/27/2004 (Begin) if the user come in to the screen from [user task list]
      *B122747,1 NNA            I'll take the Record that unSuccessful Completed to add it in a new Task 
      IF !llFromTsk
        lcTskUsr  = cUser_ID
        lcTskCtId = cconttype
        lcContId  = CCONT_ID
        lcTskNote = ' '                                  
        lcStore   = store
        lcTskCont = contact
        lcTskPhn  = cPhone
        lcTskSub  = cSubject
        lcRsn     = ctranreson
        lcTskPrt  = cpriority
        lcTskDat  = dtrandate
        lcTskTim  = ctrantime
        lnTskDur  = nestdur
        STORE AT(ctrantype,"CAT") TO rbTskTyp
      ENDIF
      *B122747,1 NNA (End)

      REPLACE mnotes     WITH lcComNote ;
              ccompleted WITH "Y"       ;
              ctranreson WITH lcComRsn  ;              
              csubject   WITH lcComSub  ;
              cresultcd  WITH lcComRst  ;
              ccmpltuser WITH lcComUsr  ;
              dcmpltdate WITH lcComDat  ;
              nactdur    WITH lnComDur  ;
              cComp_ID   WITH gcAct_Comp;
              cadd_user  WITH gcUser_ID ;
              dadd_date  WITH DATE()    ;
              cadd_time  WITH TIME()

      *B122747,1 NNA 05/27/2004 (Begin) if the user mark Task as unsuccessful and Schedule follow up
      *B122747,1 NNA            and that only in the [User Task list Screen] .I add a new Record
      *B122747,1 NNA            with this task but with a new Sequence NO. and Uncompleted Flag
      IF RBSUCCESS = 2 .AND. CBFOLCALL = 1 .AND. !llFromTsk
        APPEND BLANK
        REPLACE cseqnumber WITH gfSequence('CSEQNUMBER') ;
                cuser_id   WITH lcTskUsr                 ;
                cconttype  WITH lcTskCtId                ;
                ccont_id   WITH lcContId                 ;
                store      WITH lcStore                  ;
                contact    WITH lcTskCont                ;
                cphone     WITH lcTskPhn                 ;
                csubject   WITH lcTskSub                 ;
                cpriority  WITH lcTskPrt                 ;
                ctrantype  WITH SUBSTR("CAT",rbTskTyp,1) ;
                dtrandate  WITH lcTskDat                 ;
                ctrantime  WITH lcTskTim                 ;
                nestdur    WITH lnTskDur                 ;
                ctranreson WITH lcRsn                    ;
                mnotes     WITH lcTskNote                ;
                ccompleted WITH 'N'                      ;
                cComp_ID   WITH gcAct_Comp               ; 
                cadd_user WITH gcUser_ID                 ;
                dadd_date   WITH DATE()                  ;
                cadd_time   WITH TIME()
      ENDIF
      *B122747,1 NNA (End)
    ENDIF
    
  CASE lcMode = "A"
    lcSetDel = SET("DELETE")
    SET DELETE OFF
    SELECT syschdul
    IF SEEK(" ") AND DELETED()
      RECALL
      BLANK
    ELSE
      APPEND BLANK
    ENDIF
    SET DELETE &lcSetDel
    REPLACE cseqnumber WITH gfSequence('CSEQNUMBER') ;
            cuser_id    WITH lcTskUsr ;
            cconttype  WITH lcTskCtId ;
            ccont_id   WITH lcContId ;
            store      WITH lcStore ;
            contact    WITH lcTskCont ;
            cphone     WITH lcTskPhn ;
            csubject   WITH lcComSub ;
            cpriority  WITH IIF(lnTyp = 3 , lcTskPrt , " " ) ;
            ctrantype  WITH SUBSTR("CAT",lnTyp,1) ;
            dtrandate  WITH lcTskDat ;
            ctrantime  WITH lcTskTim ;
            nestdur    WITH lnTskDur ;
            ctranreson WITH lcComRsn ;
            mnotes     WITH lcComNote ;
            ccompleted WITH "Y" ;
            cresultcd  WITH lcComRst ;
            ccmpltuser WITH lcComUsr ;
            dcmpltdate WITH lcComDat ;
            nactdur    WITH lnComDur ;
            cComp_ID   WITH gcAct_Comp;            
            cadd_user WITH gcUser_ID ;
            dadd_date   WITH DATE() ;
            cadd_time   WITH TIME()
ENDCASE

*C200027,4 If schedule a follow up call.
IF cbFolCall = 1
  IF llFromTsk
    *C200027,4 If coming from the task type screen set flag to true.
    llNewTask  = .T.
  ENDIF
ENDIF

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


FUNCTION lfvReason
lcComRsn  = ALLTRIM(laReason[lnReason,2])


FUNCTION lfvResult
lcComRst = ALLTRIM(laResult[lnResult,2])


FUNCTION lfCompShow
*B602697,1 Reham On 03/25/99   *** Begin ***
*=gfwCodePop(@laCodes,'CTRANRESON','T')
*=gfwCodePop(@laCodes,'CRESULTCD','T')
*B602697,1 Reham On 03/25/99   *** End   ***

*E300711,1 HESHAM (Start)
*E300711,1 if the screen is running in view mode the disable all the 
*E300711,1 objects but the cancel push button
IF lcMode = "V"
  SHOW GETS DISABLE ONLY
  SHOW GET pbCanCom ENABLE
ENDIF
*E300711,1 HESHAM (End)