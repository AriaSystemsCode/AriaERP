*:************************************************************************
*:
*: Procedure file: GLAUTALT.RPR
*:               : Automatic allocations
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:30/03/94
*:
*:  Procs & Fncts: 
*:                lfChngCond    
*:                
*:
*:      Documented   /  /
*:************************************************************************
*
*B600344,1 Reham On 05/28/95 Add browse for S.J.
*B600344,1 Reham On 05/28/95 Fix the upper case for : Allocation code & S.J.
*B600340,1 Fix some spelling mistakes in the layout.
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid

*** Report Setup
lcRpFiles  = "GLAUTDT,GLAUTHD,GLACCHAR"  && Get slected files name

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
  
  WAIT 'Collecting data...' WINDOW NOWAIT
  lcRpFields=IIF(EMPTY(lcRpFields),"*",lcRpFields)
  ***   Create select  statment
  SELECT   DISTINCT &lcRpFields;
    FROM  &lcRpFiles ;
    WHERE  &lcRpExp  .AND. lfWaitMsg();
     &lcRpOrder.;
    INTO CURSOR &lcRpTargt
    
  *** Restore all enviroment 
  WAIT CLEAR
  SET TALK OFF
  ON ESCAPE  &lcSaveOnEs
  SET ESCAPE &lcSaveEsc
  
  *** Display the report to screen , file or printer
  *** and check if there is any record or not
  *** before that check if press Escape or not
  IF _TALLY = 0        && No records collected
    *** NO recoeds hove been collected
    =gfModalGen("INM00052B00000","DIALOG")
  ELSE
    DO gfDispRe WITH EVAL('lcRpForm')
  ENDIF
ELSE
  SELECT (lcRpTargt)
  DO gfDispRe WITH EVAL('lcRpForm')
ENDIF 
glEscPrs = .F.  

*!************************************************************************
*!
*!      FUNCTION lfChngCond
*!
*!************************************************************************
*
***  Function to switch between two FRX reports
FUNCTION lfChngCond

llOGFilter=IIF(lcRpForm="GLAUTALD",.F.,.T.)
DO CASE
  CASE lcRpForm =   "GLAUTALD"
      =lfChangeGrid('GLAUTAL2')
  CASE lcRpForm =   "GLAUTALS"
      =lfChangeGrid('GLAUTALT')  
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
glEscPrs = .F.

*!************************************************************************
*!
*!      FUNCTION lfvAutCode
*!
*!************************************************************************
*
FUNCTION lfvAutCode

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CAutCode:H="Code",CAutDes:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld      = VARREAD()
SELECT GLAUTHD
SET ORDER TO TAG typecode
IF ('?' $ &lcRpCurFld. .OR. !SEEK('A'+&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
  =gfBrows(["A"],'CAutCode',"laRpRetFld",'Codes File',.F.)
  &lcRpCurFld = laRpRetFld[1]
  SHOW GET (lcRpCurFld)
ENDIF
lcBrFields = lcOldBrFld
SET ORDER TO

*!************************************************************************
*!
*!      FUNCTION lfwOldVal
*!
*!************************************************************************
*
FUNCTION lfwOldVal

lcRpOld=EVAL(VARREAD())

