*:************************************************************************
*:
*: Procedure file: SMINSTAL.RPR
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: System Manager
*:         Author: Reham Alallami
*:      Copyright (c)
*:  Last modified:01/03/95
*:
*:  Procs & Funcs: 
*:
*:************************************************************************
SELECT SYCCOMP

SET ORDER TO TAG CCOMP_ID

SELECT SYCINST

SET RELATION TO sycinst.cinsdfcom INTO SYCCOMP ADDITIVE

DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp

SELECT SYCINST

SET RELATION TO

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
