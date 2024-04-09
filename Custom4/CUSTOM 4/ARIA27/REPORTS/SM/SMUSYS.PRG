*:************************************************************************
*:
*: Procedure file: SMUSYS.RPR
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: System Manager
*:         Author: Reham Alallami
*:      Copyright (c)
*:  Last modified:01/04/95
*:
*:  Procs & Funcs: 
*:
*:************************************************************************

DECLARE laAudit[4]
laAudit[1] = "None"             && Audit = "N"
laAudit[2] = "Logging"          && Audit = "A"
laAudit[3] = "Process"          && Audit = "P"
laAudit[4] = "Log & Process"    && Audit = "B"

SELECT SYUUSER

GO TOP

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
