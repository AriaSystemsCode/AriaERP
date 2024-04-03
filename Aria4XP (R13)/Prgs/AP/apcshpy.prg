*:************************************************************************
*:
*: Procedure file: APCSHPY.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Tarek Mohamed Ibrahim
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:  
*:  This program "Cash payments" is calling from  
*:  AP menu-->   
*:            Transactions-->
*:                           Payments-->
*:                                      Cash payments
*E303014 TMI 02/25/2013 [Start] 
*:************************************************************************
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.ClientProgramHome+'AP\APMNCHP.FXP')
  lcRunProgram = oAriaApplication.ClientProgramHome+'AP\APMNCHP.FXP'
ELSE
  lcRunProgram = oAriaApplication.ProgramHome+'AP\APMNCHP.FXP'
ENDIF
DO (lcRunProgram) WITH 3
