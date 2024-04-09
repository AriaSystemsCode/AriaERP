*:************************************************************************
*:
*: Procedure file: SMMDOBJ.RPR
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: System Manager
*:         Author: Reham Alallami
*:      Copyright (c)
*:  Last modified:01/05/95
*:
*:  Procs & Funcs: 
*:
*:************************************************************************

DO CASE
  CASE lcRpForm = "SMODULS"
    *** Run the summary report. ***
    SELECT SYDAPPL
    GO TOP
    DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),'FOR ','')+lcRpExp
  CASE lcRpForm = "SMMDOBJ"
    *** Run the detailed report. ***
    
    *** Get all the files with its modules in a temp. file. ***
    SELECT CAPP_ID,CFILE_NAM,CFILE_TTL,'F' AS CTYPE ;
           FROM (gcSysHome+'SYDAPPL'),(gcSysHome+'SYDFILES') ;
           WHERE CAPP_ID $ MFILE_APP ;
           INTO DBF (gcWorkDir+lcFilTmRep)
    INDEX ON CAPP_ID+CTYPE+CFILE_NAM TO (gcWorkDir+lcFilTmRep)
    
    *** Get all the module's objects in a temp. file. ***
    SELECT *,'C' AS CTYPE ;
           FROM (gcSysHome+'SYDOBJCT') ;
           WHERE CAPP_ID + CAPOBJNAM = CAPP_ID + CAPOBJNAM ;
           INTO DBF (gcWorkDir+lcObjTmRep)
    INDEX ON CAPP_ID + CTYPE + CAPOBJNAM TO (gcWorkDir+lcObjTmRep)
    
    *** Get double of the modules:_
    *** 1 for the files, 2 for the objects. ***
    SELECT *,'F' AS CTYPE ;
           FROM (gcSysHome+'SYDAPPL') ;
           WHERE CAPP_ID = CAPP_ID ;
           UNION SELECT *,'C' AS CTYPE ;
                 FROM (gcSysHome+'SYDAPPL') ;
                 WHERE CAPP_ID = CAPP_ID ;
                 INTO DBF (gcWorkDir+lcAppTmRep)
    
    *** Close sydappl. ***
    llOpenApp = .F.
    IF USED("SYDAPPL")
      llOpenApp = .T.
      SELECT SYDAPPL
      USE
    ENDIF
    
    *** Open the app. temp. file with sydappl alias. ***
    SELECT (lcAppTmRep)
    USE
    USE (gcWorkDir+lcAppTmRep) ALIAS SYDAPPL
    INDEX ON CAPP_ID+CTYPE TO (gcWorkDir+lcAppTmRep)
    
    *** Set relation to run the report. ***
    SELECT SYDAPPL
    SET RELATION TO CAPP_ID+CTYPE INTO &lcFilTmRep,CAPP_ID+CTYPE INTO &lcObjTmRep
    SET SKIP TO (lcFilTmRep),(lcObjTmRep)
    
    DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp
    
    SELECT SYDAPPL
    SET SKIP TO
    SET RELATION TO
    
    IF USED("SYDAPPL")
      USE IN ALIAS("SYDAPPL")
    ENDIF
    ERASE (gcWorkDir+lcAppTmRep+".DBF")
    ERASE (gcWorkDir+lcAppTmRep+".FPT")
    ERASE (gcWorkDir+lcAppTmRep+".IDX")
    
    IF USED(lcFilTmRep)
      USE IN ALIAS(lcFilTmRep)
    ENDIF
    ERASE (gcWorkDir+lcFilTmRep+".DBF")
    ERASE (gcWorkDir+lcFilTmRep+".FPT")
    ERASE (gcWorkDir+lcFilTmRep+".IDX")
    
    IF USED(lcObjTmRep)
      USE IN ALIAS(lcObjTmRep)
    ENDIF
    ERASE (gcWorkDir+lcObjTmRep+".DBF")
    ERASE (gcWorkDir+lcObjTmRep+".FPT")
    ERASE (gcWorkDir+lcObjTmRep+".IDX")
    
    IF llOpenApp .AND. !USED("SYDAPPL")
      llOpenApp = .F.
      SELECT 0
      USE (gcSysHome+"SYDAPPL") ALIAS SYDAPPL ORDER TAG CAPP_ID
    ENDIF
ENDCASE

*!************************************************************************
*!
*!      FUNCTION lfChngCond
*!
*!************************************************************************
*
FUNCTION lfChngCond

llOGFilter = IIF(lcRpForm = "SMODULS",.T.,.F.)

DO CASE
  CASE lcRpForm="SMMDOBJ"
    =lfChangeGrid("SMMDOBJ")
  CASE lcRpForm="SMODULS"
    =lfChangeGrid("SMODULS")
ENDCASE

*!************************************************************************
*!
*!      FUNCTION lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

IF USED(lcRpTargt)
  USE IN ALIAS(lcRpTargt)
ENDIF
