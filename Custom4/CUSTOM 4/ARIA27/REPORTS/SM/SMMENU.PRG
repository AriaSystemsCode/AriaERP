*:************************************************************************
*:
*: Procedure file: SMMENU.RPR
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Hany Waide
*:      Copyright (c) 
*:  Last modified:03/04/94
*:
*:  Procs & Fncts: 
*:
*:      Documented 03/11/1994
*:************************************************************************
*
*** Report Setup
 
    SELECT SYCMENU

    GO TOP
    DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),'FOR ','')+lcRpExp

*!************************************************************************
*!
*!      Function lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

