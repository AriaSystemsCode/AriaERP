*:************************************************************************
*:
*: Procedure file: APADCHK.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Hisham Ramsis Philips
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:  
*:  This program "Advance manual check payment" is calling from  
*:  AP menu-->   
*:            Transactions-->
*:                          Advance payments-->
*:                                            manual check payments
*:      Documented 00/00/1994
*E300683,1 AHMED 06/04/97 Add prgs directory path to the calling of programs
*:************************************************************************
* 

*E300683,1 Call programs from PRGS directory
*DO APADVPAY WITH 1 
DO (gcAppHome + gcWinAppl + '\APADVPAY ') WITH 1 
*E300683,1 end
