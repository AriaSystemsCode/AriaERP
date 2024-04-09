*:*********************************************************************************
*: Program file  : SMINSTAL.PRG
*: Program desc. : Print System Information Report
*:        System : Aria4 XP.
*:        Module : SM (System Manager).
*:     Developer : Saber A Razek (SAB)
*:     Entry     : E303336
*:*********************************************************************************

SELECT SYCCOMP
SET ORDER TO TAG CCOMP_ID

SELECT SYCINST
SET RELATION TO sycinst.cinsdfcom INTO SYCCOMP ADDITIVE

DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp

SELECT SYCINST
SET RELATION TO


*!*************************************************************
*! Name      : lfClearRep
*: Developer : Saber A Rzek (SAB)
*: Date      : 12/30/2012
*! Purpose   : Reset Report
*!*************************************************************
FUNCTION lfClearRep

IF TYPE('lcRpTargt') = 'C' .AND. USED(lcRpTargt)
  USE IN ALIAS(lcRpTargt)
ENDIF

ENDFUNC
*- End of lfClearRep