*:************************************************************************
*:
*: Procedure file: APCSHPY.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Hisham Ramsis Philips
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
*:      Documented 00/00/1994
*E300683,1 AHMED 06/04/97 Add prgs directory path to the calling of programs
*:************************************************************************
* 


*E300683,1 Call programs from PRGS directory
*DO APMNCHP WITH 3 
DO (gcAppHome + gcWinAppl + '\APMNCHP ') WITH  3
*E300683,1 end