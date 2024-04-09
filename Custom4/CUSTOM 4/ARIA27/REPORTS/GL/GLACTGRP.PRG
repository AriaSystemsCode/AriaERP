*:************************************************************************
*:
*: Procedure file: GLACTGRP.RPR
*:               : Account groups
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:12/05/94
*:
*:  Procs & Fncts: 
*:                lfChngCond    
*:                
*:
*:      Documented   /  /
*:************************************************************************
*
*B600344,1 Reham On 05/28/95 Fix the upper case for : Added user , 
*B600344,1 Reham On 05/28/95 Acc. group & let them browse from their files
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid
*B600344,4 Reham On 06/11/95 Add seek to the condition in group code validation
*B602724,1 Walid On 03/31/99 Fix Account group code field .
*E038033,1 HMA 04/03/2004 Select only records belongs to A27(to split 
*E038033,1                A27 records of SYDREPRT File from A4XP records).
*B038431,1 NNA 09/05/2004 Fix bug that when you Preview the Report in Detail form you will see 
*B038431,1 NNA            the Descriptions but When you Repreview it you will see it empty 
*:************************************************************************
DO CASE
  CASE lcRpForm = "GLACTGRS"

    SELECT GLGRPHD
    GO TOP
    DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),'FOR ','')+lcRpExp
     
  CASE lcRpForm = "GLACTGRD"
    SET ORDER TO TAG ACCTCODE IN GLACCHAR
    
    IF llOGFltCh OR glEscPrs
        
      *** Save escape setting
      lcSaveEsc = SET('ESCAP')
      lcSaveOnEs = ON('ESCAPE')
      SET ESCAP ON
      ON ESCAP DO gpSQLBrak
        
      *** Intialize the varliable that count rows selected
      _TALLY = 0
        
      *** Activate the system select therom.
      SET TALK ON
      lcRpFiles ="GLGRPHD, GLGRPDT "
      
      WAIT 'Collecting data...' WINDOW NOWAIT     
      
      SELECT DISTINCT &lcRpFields;
      FROM  &lcRpFiles;
      WHERE &lcRpExp .AND. lfWaitMsg();
      INTO CURSOR &lcRpTargt
      
      *** Restore all enviroment 
      WAIT CLEAR
      SET TALK OFF
      ON ESCAPE  &lcSaveOnEs
      SET ESCAPE &lcSaveEsc
      
      IF _TALLY = 0        && No records collected
        ** NO recoeds hove been collected
        =gfModalGen("INM00052B00000","DIALOG")
      ELSE
        SET RELATION TO &lcRpTargt..cacctcode INTO GLACCHAR ADDITIVE             
        DO gfDispRe WITH EVAL('lcRpForm')
      ENDIF
    ELSE
      SELECT (lcRpTargt)              

      *B038431,1 NNA 09/05/2004 (Begin) set Relation to get the Account Description in case of Repreview
      SET RELATION TO &lcRpTargt..cacctcode INTO GLACCHAR ADDITIVE             
      *B038431,1 NNA (End)

      DO gfDispRe WITH EVAL('lcRpForm')
    ENDIF  
    SET RELATION TO
    glEscPrs = .F.
ENDCASE

*!************************************************************************
*!
*!      Function lfChngCond
*!
*!************************************************************************
*
FUNCTION lfChngCond

llOGFilter=IIF(lcRpForm="GLACTGRD",.F.,.T.)
DO CASE
  CASE lcRpForm="GLACTGRD"
    SELECT SYDREPRT
    SEEK('GLACTGRP')
    *E038033,1 HMA 04/03/2003 Select only Records belong to ARIA27 [BEGIN]

    *IF !EMPTY(MREPHDFLT)
    *  RESTORE FROM MEMO MREPHDFLT ADDI
    *ENDIF
    LOCATE  WHILE cRep_id= "GLACTGRP" FOR cVer<>"A40"
    IF FOUND('SYDREPRT')
      IF !EMPTY(MREPHDFLT)
	    RESTORE FROM MEMO MREPHDFLT ADDI
	  ENDIF
    ENDIF 
  CASE lcRpForm="GLACTGRS"
    DIMENSION laOGHDFlt[1,7]
    STORE '' TO laOGHdFlt
ENDCASE  
    

     *E038033,1 HMA [END]
*!************************************************************************
*!
*!      FUNCTION lfvGrpCode
*!
*!************************************************************************
*
FUNCTION lfvGrpCode
DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CGrpCode:H="Code",CGrplnhed:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld    = VARREAD()

*B600344,4 Add seek to the condition to validate the entered value 
*B600344,4 from the group file.
SELECT GLGRPHD
SET ORDER TO TAG GRPCODE
*IF '?' $ &lcRpCurFld.

*B602724,1 [BEGIN] Modify this condition to be able to handel the case of
*B602724,1 Empty GRPCODE file
*IF '?' $ &lcRpCurFld. .OR. !SEEK(&lcRpCurFld.,"GLGRPHD")  
IF !EMPTY(EVAL(lcRpCurFld)) AND ('?' $ &lcRpCurFld. .OR. !SEEK(&lcRpCurFld.,"GLGRPHD"))  
*B602724,1 [END..]  
  =gfBrows(" ",'CGrpCode',"laRpRetFld",'Codes File',.F.)
  &lcRpCurFld = laRpRetFld[1]
  SHOW GET (lcRpCurFld)
ENDIF
lcBrFields = lcOldBrFld

*!************************************************************************
*!
*!      Function lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

IF USED(lcRpTargt)
  USE IN ALIAS(lcRpTargt)
ENDIF
glEscPrs = .F.

*!************************************************************************
*!
*!      FUNCTION lfwOldVal
*!
*!************************************************************************
*
FUNCTION lfwOldVal

lcRpOld=EVAL(VARREAD())

