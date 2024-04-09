*:************************************************************************
*:
*: Procedure file: SMUDEF.RPR
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

SELECT SYCCOMP
SET ORDER TO TAG CCOMP_ID
SELECT SYDAPPL
SET ORDER TO TAG CAPP_ID
SELECT SYCPRINT
SET ORDER TO TAG CPRINT_ID
SELECT SYCWRKST
SET ORDER TO TAG CWRKST_ID
SELECT SYUUSER
SET RELATION TO syuuser.cusr_dcom INTO SYCCOMP ADDITIVE
SET RELATION TO syuuser.cusr_dmdl INTO SYDAPPL ADDITIVE
SET RELATION TO syuuser.cusr_dprt INTO SYCPRINT ADDITIVE
SET RELATION TO syuuser.cusr_dwks INTO SYCWRKST ADDITIVE
LOCATE

DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp

SELECT SYUUSER
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
