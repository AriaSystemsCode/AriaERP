*:************************************************************************
*:
*: Procedure file: GLAUTDIS.RPR
*:               : Automatic distributions
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Hany Waide
*:      Copyright (c) 
*:  Last modified:03/04/94
*:
*:  Procs & Fncts: 
*:                lfChngCond    
*:                
*:
*:      Documented   /  /
*:************************************************************************
*
*B600344,1 Reham On 05/28/95 Fix the upper case for : Distribution code.
*B600346,1 Reham On 05/29/95 Spelling mistakes.
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid

*** Report Setup
 
lcRpFiles  = "GLAUTDT,GLAUTHD,GLACCHAR"  && Get slected files name

***   Create select  statment
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
  
  SELECT &lcRpDist   &lcRpFields.;
    FROM  &lcRpFiles. ;
    WHERE  &lcRpExp.  .AND. lfWaitMsg();
    &lcRpOrder.;
    INTO CURSOR (lcRpTargt)
    
  *** Restore the old setting  
  WAIT CLEAR
  SET TALK OFF
  SET ESCAPE &lcSaveEsc
  ON ESCAPE &lcSaveOnEs
  
  *** Display the report to screen , file or printer
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
*!      Function : lfChngCond
*!
*!************************************************************************
*
***  Function to switch between two FRX reports
FUNCTION lfChngCond

llOGFilter=IIF(lcRpForm="GLAUTDSD",.F.,.T.)

DO CASE
  CASE lcRpForm =   "GLAUTDSD"
       =lfChangeGrid('GLAUTDS2')
  CASE lcRpForm =   "GLAUTDSS"
       =lfChangeGrid('GLAUTDIS')  
ENDCASE  

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
glEscPrs = .F.

*!************************************************************************
*!
*!      FUNCTION : lfvAutCode
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
IF ('?' $ &lcRpCurFld. .OR. !SEEK('D'+&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
  =gfBrows(["D"],'CAutCode',"laRpRetFld",'Codes File',.F.)
  &lcRpCurFld = laRpRetFld[1]
  SHOW GET (lcRpCurFld)
ENDIF
lcBrFields = lcOldBrFld
SET ORDER TO

*!************************************************************************
*!
*!      FUNCTION : lfwOldVal
*!
*!************************************************************************
*
FUNCTION lfwOldVal

lcRpOld=EVAL(VARREAD())
