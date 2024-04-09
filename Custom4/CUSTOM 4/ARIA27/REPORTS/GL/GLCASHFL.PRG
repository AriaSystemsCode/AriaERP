*:************************************************************************
*:
*: Procedure file: GLCASHFL.RPR
*:               : Cash flow
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:13/04/1994
*:
*:  Procs & Fncts: 
*:               
*:      Documented   /  /
*:************************************************************************
*
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid
*B600362,1 Reham On 05/31/95 Fix the report layout to print the codes 
*B600362,1 in the right positions & add validation for the codes in list

*** Report Setup

SELECT GLCFITEM
SET ORDER TO TAG CFICODE

GO TOP
IF RECNO() < RECCOUNT()
  SKIP 2
ENDIF  

DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"REST FOR ",'REST ')+lcRpExp 

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
