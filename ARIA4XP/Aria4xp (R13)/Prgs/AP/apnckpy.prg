*:************************************************************************
*:
*: Procedure file: APNCKPY.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Tarek Mohamed Ibrahim
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:  
*:  This program "Non check payments" is calling from  
*:  AP menu-->   
*:            Transactions-->
*:                           Payments-->
*:                                      Non check payments
*E303014,1   TMI 12/14/2011 create the screen.Convert Manual check to aria4xp
*:************************************************************************
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.ClientProgramHome+'AP\APMNCHP.FXP')
  lcRunProgram = oAriaApplication.ClientProgramHome+'AP\APMNCHP.FXP'
ELSE
  lcRunProgram = oAriaApplication.ProgramHome+'AP\APMNCHP.FXP'
ENDIF
DO (lcRunProgram) WITH 2
