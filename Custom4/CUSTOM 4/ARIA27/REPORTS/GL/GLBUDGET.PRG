*:************************************************************************
*:
*: Procedure file: GLBUDGET.RPR
*:               : Budget version
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:29/03/94
*:
*:  Procs & Fncts: 
*:                lfChngCond    
*:                lfvBudYear
*:
*:      Documented   /  /
*:************************************************************************
*
*B600344,1 Reham On 05/28/95 Fix the upper case for : Budget code
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid
*B600365,1 Reham On 06/01/95 Add validation for the budget code in
*B600365,1 in the between. & fix the error that it was not printing
*B600365,1 even if you enter an existing budget & year.
*E300789,7 WALID (WAM) 03/04/99 make required modifications due to
*          removing ccomp_id field .
*
*B606995,1 KHM 03/19/2003 Fix the syntax error because of not initializing lcRpFields
*B606995,1                that is used in the SELECT SQL
*:************************************************************************
DO CASE
  CASE lcRpForm = "GLBUDGTS"    && if use the summary report
    SELECT GLBUDHD
    GO TOP
    DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp 
     
  CASE lcRpForm = "GLBUDGTD"    && If use the detail report
    *** Point to the chart of account file to see the account discription
    *** in a fast way without using select SQL or LOOKUP() funcion in the
    *** report form
    SET ORDER TO TAG ACCTCODE IN GLACCHAR

    lcRpFiles  = "GLBUDDT,GLBUDHD"  && Get slected files name

  ***   Create select  statment
    IF llOGFltCh OR glEscPrs
      lcRpExp = lcRpExp + ' '
      *** Save escape setting
      lcSaveEsc = SET('ESCAP')
      lcSaveOnEs = ON('ESCAPE')
      SET ESCAP ON
      ON ESCAP DO gpSQLBrak
        
      *** Intialize the varliable that count rows selected
      _TALLY = 0
        
      *** Activate the system select therom.
      SET TALK ON
      
      WAIT 'Collecting data...' WINDOW NOWAIT     
      
      *B606995,1 KHM 03/19/2003 (Begin) Initialize the fields to be selected.
      lcRpFields = "GlBudHd.cBudCode, GlBudHd.cBudYear, GlBudHd.cBudDes, GlBudHd.cBudComnt,"+;
                   "GlBudDt.cAcctCode, GlBudDt.cBudPrd, GlBudDt.nAmount"
      *B606995,1 KHM 03/19/2003 (End)
      
      *** Create select  statment and
      *** Select data from file(s)
      SELECT &lcRpFields.;
        FROM  &lcRpFiles. ;
        WHERE  &lcRpExp. .AND. lfWaitMsg();
        INTO CURSOR  (lcRpTargt);
        &lcRpOrder
          
      *** Restore all enviroment 
      WAIT CLEAR
      SET TALK OFF
      ON ESCAPE  &lcSaveOnEs
      SET ESCAPE &lcSaveEsc
        
      IF _TALLY > 0         
        IF EMPTY(RELATION(1))
          *** Make a relation between temporary file and glacchar 
          SET RELATION TO &lcRpTargt..cacctcode INTO GLACCHAR ADDITIVE
          *** Display the report to screen , file or printer
        ENDIF  
      ELSE
        ** NO recoeds hove been collected
        =gfModalGen("INM00052B00000","DIALOG")
        RETURN
      ENDIF
    ELSE
      SELECT (lcRpTargt)
    ENDIF        
    DO gfDispRe WITH EVAL('lcRpForm')     
    glEscPrs = .F.
ENDCASE

*!************************************************************************
*!
*!      Function : lfChngCond
*!
*!************************************************************************
*
FUNCTION lfChngCond

llOGFilter=IIF(lcRpForm="GLBUDGTD",.F.,.T.)
DO CASE
  CASE lcRpForm="GLBUDGTD"
      =lfChangeGrid('GLBUDGT2')
  CASE lcRpForm="GLBUDGTS"
      =lfChangeGrid('GLBUDGET')  
ENDCASE  

*!************************************************************************
*!
*!      Function lfvBudYear
*!
*!************************************************************************
*
****  Check if current company has this entried year or not
FUNCTION lfvBudYear

llFindYear = .T.     && Found or not

 && Check If year field is empty
IF .NOT.EMPTY(ALLTRIM(EVALUATE(SYS(18))))  
  lcOldAlias = ALIAS()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [Begin]
  
  *IF NOT USED("SYCFISHD") 
  *  SELECT 0
  *  USE &gcSysHome.SYCFISHD ORDER TAG compfyear
  *  llUesdBefo = .T.
  *ENDIF
  *SELECT SYCFISHD
  IF NOT USED("FISHD") 
    SELECT 0
    USE &gcDataDir.FISHD ORDER TAG compfyear
    llUesdBefo = .T.
  ENDIF
  SELECT FISHD
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [End]
  
  *** Search for the current company+year
  llFindYear = SEEK(gcPrnt_Cmp+ALLTRIM(EVALUATE(SYS(18))))
  IF llUesdBefo       && .F.- this file used by the system
    
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [Begin]
    *USE IN SYCFISHD
    USE IN FISHD
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [End]
    
  ENDIF
  IF NOT EMPTY(lcOldAlias)
    SELECT (lcOldAlias)
  ENDIF    
ENDIF
RETURN llFindYear       && Return the .T. if found .F. if not

*!************************************************************************
*!
*!      Function : lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

IF USED(lcRpTargt)
  USE IN ALIAS(lcRpTargt)
ENDIF 
glEscPrs =.F.

*!************************************************************************
*!
*!      FUNCTION : lfGetPic
*!
*!************************************************************************
*
FUNCTION lfGetPic

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]

*IF NOT USED("SYCACCOD") 
*  SELECT 0
*  USE &gcSysHome.SYCACCOD ORDER TAG COMPID
*  llUesdBefo = .T.
*ENDIF
*
*SELECT SYCACCOD
IF NOT USED("ACCOD") 
  SELECT 0
  *E300789,7 [BEGIN]
  *USE &gcDataDir.ACCOD ORDER TAG COMPID
  USE &gcDataDir.ACCOD
  *E300789,7 [END..]
  llUesdBefo = .T.
ENDIF

SELECT ACCOD

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
*E300789,7 [BEGIN]
*IF SEEK(gcAct_Comp)
GO TOP
IF !EOF()
*E300789,7 [END..]  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
  *lcRpSegMas = ALLTRIM(SYCACCOD.cacsmask)
  lcRpSegMas = ALLTRIM(ACCOD.cacsmask)
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
  
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', '9',2) 
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', 'X',1,1) 
ELSE
  lcRpSegMas = " "
ENDIF
IF llUesdBefo       && .F.- this file used by the system
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
  *USE IN SYCACCOD
  USE IN ACCOD
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
  
ENDIF
IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF    
RETURN lcRpSegMas

