*:************************************************************************
*:
*: Procedure file: GLAUTTEM.RPR
*:               : Template entries
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:05/04/94
*:
*:  Procs & Fncts: 
*:                lfChngCond    
*:                
*:
*:      Documented   /  /
*:************************************************************************
*
*** Report Setup
*B600346,1 Reham 05/29/95 Spelling mistakes.   
*B600344,1 Reham 05/28/95 Fix the upper case for : template code.
*B600349,1 Reham 05/29/95 Remove the sort option from the option grid.
*B600339,1 Reham 05/28/95 "Added By" new object in Option Grid
*B600518,1 M.H   12/11/95 Change the currency sign if "%" in the FRX.
*B602243,1 KAM   08/31/2000 remove distinct keyword from select statment to allow 
*B602243,1       more occurance value to appear in report
******************************************************************************
lcRpFiles  = "GLAUTDT,GLAUTHD,GLACCHAR"  && Get slected files name

***   Get the field order ****
lcRpOrder  = "ORDER BY GLAUTHD.cautcode"

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
  
  *B602243 -KAM on 08/31/2000   (Begin)
  *SELECT  DISTINCT  &lcRpFields;
   FROM  &lcRpFiles ;
   WHERE  &lcRpExp  .AND. lfWaitMsg();
   &lcRpOrder.;
   INTO CURSOR (lcRpTargt)
  
  SELECT  &lcRpFields;
   FROM  &lcRpFiles ;
   WHERE  &lcRpExp  .AND. lfWaitMsg();
   &lcRpOrder.;
   INTO CURSOR (lcRpTargt)

   *B602243 -KAM on 08/31/2000   (End)

 *** Restore the old setting  
  WAIT CLEAR
  SET TALK OFF
  SET ESCAPE &lcSaveEsc 
  ON  ESCAPE &lcSaveOnEs 
  
  *** Display the report to screen , file or printer
  *** and check if there is any record or not
  IF _TALLY = 0        && No records collected
    ** NO recoeds hove been collected
    =gfModalGen("INM00052B00000","DIALOG")
  ELSE
    DO gfDispRe WITH EVAL('lcRpForm')
  ENDIF
ELSE
  SELECT (lcRpTargt)
  DO gfDispRe WITH EVAL('lcRpForm')
ENDIF 
SELECT (lnOldAlias)  

glEscPrs = .F.    

*!************************************************************************
*!
*!      FUNCTION : lfChngCond
*!
*!************************************************************************
*
***  Function to switch between two FRX reports
FUNCTION lfChngCond

llOGFilter=IIF(lcRpForm="GLAUTTED",.F.,.T.)
DO CASE
  CASE lcRpForm =   "GLAUTTED"
      =lfChangeGrid('GLAUTTE2')
  CASE lcRpForm =   "GLAUTTES"
      =lfChangeGrid('GLAUTTEM')  
ENDCASE  

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

IF ('?' $ &lcRpCurFld. .OR. !SEEK('T'+&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
  =gfBrows(["T"],'CAutCode',"laRpRetFld",'Codes File',.F.)
  &lcRpCurFld = laRpRetFld[1]
  SHOW GET (lcRpCurFld)
ENDIF
lcBrFields = lcOldBrFld
SET ORDER TO

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
