*:************************************************************************
*: Program file  : SMCLRLCK.PRG
*: Program desc. : CLEAR LOCKS FOR SQL TABLES OF ARIA 4XP.
*:         System: ARIA 4.0 XP
*:         Module: SM and SYSTEM INFORMATION
*:      Developer: AHMED MAHER (AMH)
*:       Issue # : N039468
*:************************************************************************
LPARAMETERS lcTempMemo

IF TYPE('lcTempMemo')#'C'
  RETURN
ENDIF

RESTORE FROM (lcTempMemo) ADDITIVE

DO FORM SMCLRLCK WITH lcXComp_ID,lcXSysHome
READ EVENTS
