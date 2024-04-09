*:************************************************************************
*:
*: Procedure file: APMNCHK.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Tarek Mohamed Ibrahim
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:  
*:  This program "Manual Check payments" is calling from  
*:  AP menu-->   
*:            Transactions-->
*:                           Payments-->
*:                                      Manual Checks
*:  E303014 TMI 02/25/2013 
******************************************************************************
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.ClientProgramHome+'AP\APMNCHP.FXP')
  lcRunProgram = oAriaApplication.ClientProgramHome+'AP\APMNCHP.FXP'
ELSE
  lcRunProgram = oAriaApplication.ProgramHome+'AP\APMNCHP.FXP'
ENDIF
DO (lcRunProgram) WITH 1
