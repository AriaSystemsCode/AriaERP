*:************************************************************************
*:
*: Procedure file: APADNCK.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Hisham Ramsis Philips
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:  
*:  This program "Advance non check payment" is calling from  
*:  AP menu-->   
*:            Transactions-->
*:                          Advance payments-->
*:                                            Non check payments
*:
*:      Documented 00/00/1994
*E300683,1 AHMED 06/04/97 Add prgs directory path to the calling of programs
*:************************************************************************
* 

*E300683,1 Call programs from PRGS directory
*DO APADVPAY WITH 2 
DO (gcAppHome + gcWinAppl + '\APADVPAY ') WITH 2
*E300683,1 end