*:************************************************************************
*:
*: Procedure file: APNCKPY.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Hisham Ramsis Philips
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
*:      Documented 00/00/1994
*E300683,1 AHMED 06/04/97 Add prgs directory path to the calling of programs
*:************************************************************************
* 
*E300683,1 Call programs from PRGS directory
*DO APMNCHP WITH 2
*DO (gcAppHome + gcWinAppl + '\APMNCHP ') WITH 2
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.ClientProgramHome+'AP\APMNCHP.FXP')
  lcRunProgram = oAriaApplication.ClientProgramHome+'AP\APMNCHP.FXP'
ELSE
  lcRunProgram = oAriaApplication.ProgramHome+'AP\APMNCHP.FXP'
ENDIF   
DO (lcRunProgram) WITH 2
