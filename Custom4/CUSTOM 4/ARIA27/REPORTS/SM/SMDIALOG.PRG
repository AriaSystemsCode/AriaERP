*:************************************************************************
*:
*: Procedure file: SMDIALOG.RPR
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
SELECT SYDAPPL

SET ORDER TO TAG CAPP_ID

SELECT SYDDLOBJ

SET ORDER TO TAG CAPPID

SET RELATION TO CAPP_ID INTO SYDAPPL ADDITIVE

GO TOP

DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),'FOR ','')+lcRpExp

SELECT SYDDLOBJ

SET RELATION TO

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

