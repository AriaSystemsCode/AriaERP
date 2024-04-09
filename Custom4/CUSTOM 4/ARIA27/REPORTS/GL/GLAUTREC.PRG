*:************************************************************************
*:
*: Procedure file: GLAUTREC.RPR
*:               : Automatic reccurings
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
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
*B600346,1 Reham On 05/29/95 Spelling mistakes.
*B600344,1 Reham On 05/28/95 Fix the upper case for : rec. code. & S.J.
*B600349,1 Reham On 05/29/95 Remove the sort option from the option grid.
*B600338,1 Reham On 05/28/95 Increase the date width to 10 ch.
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid
*B600358,1 Reham On 05/30/95 Add validation for recurring code.

*** Report Setup

DO CASE
  CASE lcRpForm = "GLAUTRES"    && if use the summary report
    SELECT GLAUTHD
    GO TOP
    DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp 
     
  CASE lcRpForm = "GLAUTRED"    && If use the detail report
    

    lcRpFiles  = "GLAUTDT,GLAUTHD,GLACCHAR"  && Get slected files name

    lnCount    = 0
    lnTotal    = RECCOUNT('GLAUTDT')
    lnOldAlias = SELECT() 
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
      
      ***   Create select  statment
      SELECT  DISTINCT &lcRpFields.;
          FROM  &lcRpFiles. ;
         WHERE  &lcRpExp.  .AND. lfWaitMsg();
          &lcRpOrder.;
         INTO CURSOR &lcRpTargt
         
      *** Restore the old setting
      WAIT CLEAR
      SET TALK OFF   
      SET ESCAPE &lcSaveEsc  
      On  ESCAPE &lcSaveOnEs 
      
      IF _TALLY = 0        && No records collected
        ** NO recoeds hove been collected
        =gfModalGen("INM00052B00000","DIALOG")
      ELSE
        *** Display the report to screen , file or printer
        DO gfDispRe WITH EVAL('lcRpForm')
      ENDIF
    ELSE  
      SELECT (lcRpTargt)
      DO gfDispRe WITH EVAL('lcRpForm')
    ENDIF  
    SELECT (lnOldAlias)
ENDCASE

glEscPrs = .F.

*!************************************************************************
*!
*!      FUNCTION : lfChngCond
*!
*!************************************************************************
*
***  Function to switch between two FRX reports
FUNCTION lfChngCond

llOGFilter=IIF(lcRpForm="GLAUTRED",.F.,.T.)
DO CASE
  CASE lcRpForm =   "GLAUTRED"
      =lfChangeGrid('GLAUTRE2')
  CASE lcRpForm =   "GLAUTRES"
      =lfChangeGrid('GLAUTREC')  
ENDCASE  

*!************************************************************************
*!
*!      FUNCTION : lfClearRep
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
IF ('?' $ &lcRpCurFld. .OR. !SEEK('R'+&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
  =gfBrows(["R"],'CAutCode',"laRpRetFld",'Codes File',.F.)
  &lcRpCurFld = laRpRetFld[1]
  SHOW GET (lcRpCurFld)
ENDIF
lcBrFields = lcOldBrFld
SET ORDER TO
