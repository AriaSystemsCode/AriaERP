
*:************************************************************************
*:
*: Procedure file: SMPRINT.RPR
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:14/04/1994
*:
*:  Procs & Fncts: 
*:               
*:      Documented   /  /
*:************************************************************************
*

*!************************************************************************
*!
*!      SMPRINT.RPR
*!
*!************************************************************************
*
*** Report Setup

SELECT SYCPRINT
DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp 


*********************** part of UDFs ***************************

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