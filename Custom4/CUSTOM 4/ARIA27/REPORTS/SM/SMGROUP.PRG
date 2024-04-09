*:************************************************************************
*:
*: Procedure file: SMGROUP.RPR
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

SELECT SYUUSER
SET ORDER TO TAG &lcRpIndex

SELECT SYUGROUP
SET RELATION TO syugroup.cgroup_id INTO SYUUSER ADDITIVE
SET SKIP TO SYUUSER

DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp

SELECT SYUGROUP
SET SKIP TO
SET RELATION TO

*!************************************************************************
*!
*!      FUNCTION lfGropShow
*!
*!************************************************************************
*
FUNCTION lfGropShow

SELECT SYUUSER

INDEX ON cUsr_grup TO &gcWorkDir.&lcRpIndex

*!************************************************************************
*!
*!      FUNCTION lfGropVald
*!
*!************************************************************************
*
FUNCTION lfGropVald

IF USED("SYUUSER")
  SELECT SYUUSER
  USE 
  ERASE (gcWorkDir+lcRpIndex+".IDX")
  USE (gcSysHome+"SYUUSER") ORDER TAG CUSER_ID
ELSE
  ERASE (gcWorkDir+lcRpIndex+".IDX")
ENDIF

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

