*:************************************************************************
*:
*: Procedure file: SMFACTOR.RPR
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: System Manager
*:         Author: Reham Alallami
*:      Copyright (c)
*:  Last modified:01/02/95
*:
*:  Procs & Funcs: 
*:
*:************************************************************************
*E300266,6 Reham On 08/07/95   
*E300266,6 In (SMFACTD.FRX) Call gfGetAdr() from the FRX in the address lines.
*:************************************************************************

SELECT SYCFACT

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
