*:************************************************************************
*:
*: Procedure file: GLSORJOR.RPR
*:               : Source journal
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:07/04/1994
*:
*:  Procs & Fncts: 
*:                lfClearRep
*:      Documented   /  /
*:************************************************************************
*
*B600344,1 Reham On 05/28/95 Fix the upper case for : S.J.
*B600346,1 Reham On 05/29/95 Spelling mistakes.
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid

SELECT GLSUBJOR

DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp 

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
