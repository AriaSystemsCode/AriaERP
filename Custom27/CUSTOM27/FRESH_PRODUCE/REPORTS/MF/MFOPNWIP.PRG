*:************************************************************************
*: Program file  : MFOPNWIP.Prg
*: Program desc. : Custom Open Work In Process Report for Fresh Produce
*: System        : Aria Advantage Series VER. 2.7
*: Module        : MF
*: Developer     : AHMED MAHER
*: Date          : 01/08/2002
*:************************************************************************
*: Calls : 
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO MFOPNWIP
*:************************************************************************

PRIVATE lcTmpFile,lcKeyExp,lnI,lcStages,lcCuttkt,lnIssue,lnReceive
PRIVATE lcCompName,lcCompPhon,lcPhonPict,llLogo,lcTime
lcTmpFile = gfTempName()   && Cursor tmp. name
lcKeyExp = 'CCONTCODE'     && Index expration of the cursor

*--Get the company information
SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName = cCom_Name
DIMENSION laCompAdd[6]                 && Array to hold company addresses
STORE SPACE(0) TO laCompAdd
STORE .F. TO llLogo
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
* Get the company addresses
laCompAdd[1]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
laCompAdd[6]    = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)

DO lfShiftArr WITH laCompAdd

lcLogoPic = gfTempName()
IF SEEK('*' + 'LOGO' , 'OBJLINK') AND SEEK(OBJLINK.cObject_ID,'OBJECTS')
  llLogo = .T.
  lcObj_Id = OBJLINK.cObject_ID
  *-- Make cursor contain one field and one record holding the company logo
  SELECT gobject;
   FROM Objects         ;
   WHERE Objects.cobject_id = lcObj_Id ;
   INTO CURSOR (lcLogoPic)
ENDIF
lcTime   = TIME()                       && Variable to hold the Time

*-- build the index expration
lnI = 0
IF ALEN(laRpTarSrt,1) = 0
  lcKeyExp = lcKeyExp + '+CSTAGE+DCOMPDATE+CUTTKT'
ELSE
  FOR lnI = 1 TO ALEN(laRpTarSrt,1)
    DO CASE
      CASE laRpTarSrt[lnI] = 'Stage'
        lcKeyExp = lcKeyExp + '+CSTAGE'
      CASE laRpTarSrt[lnI] = 'Completion date'
        lcKeyExp = lcKeyExp + '+DTOS(DCOMPDATE)'
      CASE laRpTarSrt[lnI] = 'Cutting #'
        lcKeyExp = lcKeyExp + '+CUTTKT'
    ENDCASE
  ENDFOR
ENDIF

lcStages = ''              && all selected stages

*-- build the variable contined all selected stages
IF !EMPTY(laRpTarStg)
  FOR lnI = 1 TO ALEN(laRpTarStg,1)
    lcStages = lcStages + LEFT(laRpTarStg[lnI],6) + '|'
  ENDFOR
ENDIF

*-- creating the cursor
CREATE CURSOR (lcTmpFile) (CCONTCODE C(8),CSTAGE C(6),CUTTKT C(6),CSTATUS C(1),CSEASON C(6),;
                CSTYLE C(12),CPATTERN C(10),CDESC C(20),NTOTUNT N(7,0),NRECVUNT N(7,0),;
                DATE1 D,DATE2 D,DATE3 D, DATE4 D,DCOMPDATE D,CREMARK C(50),CCONTNAME C(30))
INDEX ON &lcKeyExp. TAG (lcTmpFile) OF (lcTmpFile)
SET ORDER TO TAG (lcTmpFile)

*-- Relation ship
SELECT MFGOPRDT
SET RELATION TO CTKTNO INTO CUTTKTH ADDITIVE
SELECT CUTTKTH
SET RELATION TO SUBSTR(STYLE,1,12) INTO STYLE ADDITIVE
SET RELATION TO 'I'+CUTTKT INTO NOTEPAD ADDITIVE

*-- collecting Data.
IF EMPTY(lcRpCont)
  SELECT CUTTKTH
  SCAN FOR STATUS $ 'HOA'
    lcCutTkt = CUTTKT
    IF SEEK('M'+lcCuttkt,'MFGOPRHD')
      =lfChkStage(.T.)
    ELSE
      IF IIF(EMPTY(laRpTarStg),.T.,'******'$lcStages)
        SELECT (lcTmpFile)
        APPEND BLANK
        REPLACE CCONTCODE WITH '',;
                CCONTNAME WITH '',;
                CSTAGE    WITH '******',;
                CUTTKT    WITH lcCuttkt,;
                CSTATUS   WITH 'H',;
                CSEASON   WITH STYLE.SEASON,;
                CSTYLE    WITH CUTTKTH.STYLE,;
                CPATTERN  WITH STYLE.PATTERN,;
                CDESC     WITH STYLE.DESC,;
                NTOTUNT   WITH 0,;
                NRECVUNT  WITH 0,;
                DCOMPDATE WITH CUTTKTH.COMPLETE,;
                CREMARK   WITH NOTEPAD.MNOTES
        IF !EMPTY(laRpTarSDt)
          FOR lnI = 1 TO ALEN(laRpTarSDt,1)
            lcI = STR(lnI,1)
            REPLACE ('DATE'+lcI) WITH lfGetStgDt(lnI)
          ENDFOR
        ENDIF
        =SEEK(lcCuttkt,'CUTTKTH')
      ENDIF
    ENDIF
  ENDSCAN
ELSE
  IF SEEK('M'+lcRpCont,'MFGOPRDT')
    SELECT MFGOPRDT
    lcCuttkt = SPACE(6)
    SCAN REST WHILE cImTyp+cContCode+cTktNo+cOprCode = 'M'+lcRpCont
      IF CTKTNO # lcCuttkt
        lcCuttkt = CTKTNO
        IF CUTTKTH.STATUS $ 'HOA' .AND. SEEK('M'+lcCuttkt,'MFGOPRHD')
          =lfChkStage()
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ENDIF

*-- Check if there is data to print
SELECT (lcTmpFile)
LOCATE
IF EOF()
  SET DEVICE TO SCREEN
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF

*--- Start printing report
DO gfDispRe WITH EVAL('lcRpForm')
SET DEVICE TO SCREEN

*-- Close the tmp. cursor and clear relationship
IF USED(lcTmpFile)
  USE IN (lcTmpFile)
ENDIF
SELECT CUTTKTH
SET RELATION TO
SELECT MFGOPRDT
SET RELATION TO
*--end of Main Program.

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : AHMED MAHER
*! Date      : 01/08/2002
*! Purpose   : When function of the option grid
*!*************************************************************
*! Called from : MFOPTR.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

*-- Declear Stages array that be used in Stages mover
DECLARE laRpSorStg[1] , laRpTarStg[1] , laRpSorSDt[1] , laRpTarSDt[1] ,;
        laRpSorSrt[3] , laRpTarSrt[1]
STORE SPACE(0) TO laRpSorStg , laRpTarStg , laRpSorSDt , laRpTarSDt , laRpSorSrt , laRpTarSrt
laRpSorSrt[1] = 'Stage'
laRpSorSrt[2] = 'Completion date'
laRpSorSrt[3] = 'Cutting #'

*-- Collecting the stages.
STORE '****** - Planned' TO laRpSorStg[1] , laRpSorSDt
SELECT CODES
lcCode_No = SPACE(6)
SEEK 'N'+'MFGCODE   '
SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N'+'MFGCODE   '
  IF CRLTFIELD = 'Y' .AND. CRLTD_NAM = 'LMFGOPR   ' .AND. ALLTRIM(CRLTD_VLU) = 'T'
    lcCode_No = cCode_No
  ENDIF
  IF CRLTFIELD = 'N' .AND. cCode_No = lcCode_No
    DECLARE laRpSorStg[ALEN(laRpSorStg,1)+1] , laRpSorSDt[ALEN(laRpSorSDt,1)+1]
    STORE lcCode_No + ' - ' + cDiscrep TO laRpSorStg[ALEN(laRpSorStg,1)] , laRpSorSDt[ALEN(laRpSorStg,1)]
  ENDIF
ENDSCAN
*--end of lfwOGWhen.

*!*************************************************************
*! Name      : lfvCont
*! Developer : AHMED MAHER
*! Date      : 01/08/2002
*! Purpose   : Valid function of the contractor setting in the option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfApVnBrow()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCont()
*!*************************************************************
FUNCTION lfvCont

PRIVATE lcVar, lcObj
lnAlias = SELECT(0)
lcVar = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(  ( EVALUATE(SYS(18)) )  )   && Varible to hold the current field value
SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcObj)
  IF !SEEK(lcObj , 'APVENDOR'))
    =gfApVnBrow(@lcObj)
    &lcVar = lcObj      && Update the field
  ENDIF
ENDIF
SELECT (lnAlias)
*--end of lfvCont.

*!*************************************************************
*! Name      : lfvStages
*! Developer : AHMED MAHER
*! Date      : 01/09/2002
*! Purpose   : Valid function of the stages setting in the option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfMover()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStages()
*!*************************************************************
FUNCTION lfvStages

= gfMover(@laRpSorStg,@laRpTarStg,'Select Stage',.T.,'')  && call mover function.
*-- end of lfvStageas.

*!*************************************************************
*! Name      : lfvStgDate
*! Developer : AHMED MAHER
*! Date      : 01/09/2002
*! Purpose   : Valid function of the stages for Dates setting in the option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfMover()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStgDate()
*!*************************************************************
FUNCTION lfvStgDate

lcVldPrd = 'lfVldPrd'
= gfMover(@laRpSorSDt,@laRpTarSDt,'Select Stages for Dates',.T.,@lcVldPrd,.T.)  && call mover function.
*-- end of lfvStgDate.

*!*************************************************************
*! Name      : lfVldPrd
*! Developer : AHMED MAHER
*! Date      : 01/09/2002
*! Purpose   : Valid function of the mover of stages for Dates setting in the option grid
*!*************************************************************
*! Called from : gfMover()
*!*************************************************************
*! Passed Parameters : lnMove
*!*************************************************************
*! Return      : .T. for valid , .F. for invalid
*!*************************************************************
*! Example     : = lfVldPrd(1)
*!*************************************************************
FUNCTION lfVldPrd
PARAMETERS lnMove

PRIVATE lnReturn
lnReturn = .T.
IF lnMove < 3 .AND. ALEN(laTarget,1) = 4
  lnReturn = .F.
ENDIF
RETURN lnReturn
*-- end of lfVldPrd.

*!*************************************************************
*! Name      : lfvSort
*! Developer : AHMED MAHER
*! Date      : 01/09/2002
*! Purpose   : Valid function of the Sort setting in the option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfMover()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSort()
*!*************************************************************
FUNCTION lfvSort

= gfMover(@laRpSorSrt,@laRpTarSrt,'Select Sorting Order',.T.,'')  && call mover function.
*-- end of lfvStageas.

*!*************************************************************
*! Name      : lfGetStgDt
*! Developer : AHMED MAHER
*! Date      : 01/13/2002
*! Purpose   : Function to get Due Date for selected stages for date
*!*************************************************************
*! Passed Parameters : lnStageOrd = Stage order
*!*************************************************************
*! Return      : ldStageDt
*!*************************************************************
*! Example     : = lfGetStgDt(lnI)
*!*************************************************************
FUNCTION lfGetStgDt
PARAMETERS lnStageOrd

PRIVATE lnAlias,lcOprCode,lnLeadTime,lcMfgKey
STORE 0 TO lnLeadTime
DECLARE laLeadTime[1,2]
laLeadTime[1,1] = 'LEADTIME  '
laLeadTime[1,2] = 'lnLeadTime'
lcOprCode = LEFT(laRpTarSDt[lnStageOrd],6)
= gfRltFld(lcOprCode , @laLeadTime , 'MFGCODE   ')
lnAlias = SELECT(0)
SELECT MFGOPRDT
lcMfgKey = EVALUATE(KEY())
IF SEEK('M'+lcRpCont+lcCuttkt+lcOprCode)
  ldStageDt = MFGOPRDT.DUEDATE
ELSE
  IF lnStageOrd = 1
    =SEEK(lcCuttkt,'CUTTKTH')
    ldStageDt = CUTTKTH.ENTERED + lnLeadTime
  ELSE
    ldStageDt = EVALUATE(lcTmpFile+'.DATE'+STR(lnStageOrd-1,1)) + lnLeadTime
  ENDIF
ENDIF
SEEK lcMfgKey
SELECT (lnAlias)
RETURN ldStageDt
*-- end of lfGetStgDt.

*!*************************************************************
*! Name      : lfChkStage
*! Developer : AHMED MAHER
*! Date      : 01/14/2002
*! Purpose   : Function to check stage for printing or not
*!*************************************************************
*! Passed Parameters : llAllCont = (.T.=all contractors/.F.=one contractor)
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfChkStage(.T.)
*!*************************************************************
FUNCTION lfChkStage
PARAMETERS llAllCont

SELECT MFGOPRHD
SCAN REST WHILE cImTyp+cTktNo+cOperSeq = 'M'+lcCuttkt;
          FOR IIF(EMPTY(laRpTarStg),.T.,IIF(llAllCont .AND. '******'$lcStages,.T.,cOprCode$lcStages)) .AND.;
              IIF(llAllCont,.T.,cContCode=lcRpcont)

  STORE 0 TO lnIssue,lnReceive
  IF SEEK('M'+cContCode+lcCuttkt+cOprCode,'MFGOPRDT')
    SELECT MFGOPRDT
    SUM REST WHILE cImTyp+cContCode+cTktNo+cOprCode = 'M'+MFGOPRHD.CCONTCODE+lcCuttkt+MFGOPRHD.cOprCode;
             FOR TRANCD = '1' NLOTTOTQTY TO lnIssue
    SEEK 'M'+MFGOPRHD.CCONTCODE+lcCuttkt+MFGOPRHD.cOprCode
    SUM REST WHILE cImTyp+cContCode+cTktNo+cOprCode = 'M'+MFGOPRHD.CCONTCODE+lcCuttkt+MFGOPRHD.cOprCode;
             FOR TRANCD # '1' NLOTTOTQTY TO lnReceive
    SEEK 'M'+MFGOPRHD.CCONTCODE+lcCuttkt+MFGOPRHD.cOprCode
    SELECT MFGOPRHD
  ENDIF
  IF lnIssue > lnReceive
    IF !llAllCont .OR. IIF(EMPTY(laRpTarStg),.T.,cOprCode$lcStages)
      =SEEK(MFGOPRHD.CCONTCODE,'APVENDOR')
      SELECT (lcTmpFile)
      APPEND BLANK
      REPLACE CCONTCODE WITH MFGOPRHD.CCONTCODE,;
              CCONTNAME WITH APVENDOR.CVENCOMP,;
              CSTAGE    WITH MFGOPRHD.COPRCODE,;
              CUTTKT    WITH MFGOPRHD.CTKTNO,;
              CSTATUS   WITH CUTTKTH.STATUS,;
              CSEASON   WITH STYLE.SEASON,;
              CSTYLE    WITH CUTTKTH.STYLE,;
              CPATTERN  WITH STYLE.PATTERN,;
              CDESC     WITH STYLE.DESC,;
              NTOTUNT   WITH IIF(CUTTKTH.STATUS='A',CUTTKTH.PCS_ACT,lnIssue),;
              NRECVUNT  WITH CUTTKTH.PCS_REC,;
              DCOMPDATE WITH CUTTKTH.COMPLETE,;
              CREMARK   WITH NOTEPAD.MNOTES
      IF !EMPTY(laRpTarSDt)
        FOR lnI = 1 TO ALEN(laRpTarSDt,1)
          lcI = STR(lnI,1)
          REPLACE ('DATE'+lcI) WITH lfGetStgDt(lnI)
        ENDFOR
      ENDIF
      =SEEK(lcCuttkt,'CUTTKTH')
      IF !llAllCont .OR. lnReceive = 0
        EXIT
      ENDIF
    ENDIF
  ELSE
    IF llAllCont .AND. lnIssue = 0 .AND. IIF(EMPTY(laRpTarStg),.T.,'******'$lcStages);
                 .AND. cOperSeq = '1'
      =SEEK(lcCuttkt,'CUTTKTH')
      SELECT (lcTmpFile)
      APPEND BLANK
      REPLACE CCONTCODE WITH '',;
              CCONTNAME WITH '',;
              CSTAGE    WITH '******',;
              CUTTKT    WITH lcCuttkt,;
              CSTATUS   WITH 'O',;
              CSEASON   WITH STYLE.SEASON,;
              CSTYLE    WITH CUTTKTH.STYLE,;
              CPATTERN  WITH STYLE.PATTERN,;
              CDESC     WITH STYLE.DESC,;
              NTOTUNT   WITH 0,;
              NRECVUNT  WITH 0,;
              DCOMPDATE WITH CUTTKTH.COMPLETE,;
              CREMARK   WITH NOTEPAD.MNOTES
      IF !EMPTY(laRpTarSDt)
        FOR lnI = 1 TO ALEN(laRpTarSDt,1)
          lcI = STR(lnI,1)
          REPLACE ('DATE'+lcI) WITH lfGetStgDt(lnI)
        ENDFOR
      ENDIF
      =SEEK(lcCuttkt,'CUTTKTH')
      EXIT
    ENDIF
  ENDIF
ENDSCAN
=SEEK(lcCuttkt,'CUTTKTH')
*--end of lfChkStage.

*!*************************************************************
*! Name        : lfShiftArr
*! Developer   : AHMED MAHER (AMH)
*! Date        : 01/14/2002
*! Purpose     : Function to Pack the passed array
*!*************************************************************
*! Calls       : 
*!              Procedures : None
*!              Functions  : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : DO lfShiftArr WITH laCompAdd
*!*************************************************************
FUNCTION lfShiftArr

PARAMETERS laArray
PRIVATE lnAlen,lnCount, lnC
* Get length of the array
lnALen = ALEN(laArray,1)
* check each element of the array if it is empty
FOR lnCount = 1 TO lnALen
  IF EMPTY(laArray[lnCount])
    * If any element is empty shift down the later elements
    FOR lnC = lnCount TO lnALen-1
      laArray[lnC]=laArray[lnC+1]
    ENDFOR
    laArray[lnAlen]=''
  ENDIF
ENDFOR
*--end of lfShiftArr.