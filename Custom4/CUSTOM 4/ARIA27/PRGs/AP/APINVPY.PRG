*:************************************************************************
*:
*: Procedure file: APINVOIS.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Mohamed Hassan Mohamed
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:  
*:  This program "Payable invoice" is calling from  
*:  AP menu-->   
*:            Transactions-->
*:                     Payable Invoice
*:      Documented 00/00/1994
*:************************************************************************
*E300683,1 AHMED 06/04/97 Add prgs directory path to the calling of programs

*E300683,1 Call programs from PRGS directory
*DO APPyInv WITH  .F.,.F.,0
DO (gcAppHome + gcWinAppl + '\APPyInv') WITH  .F.,.F.,0
*E300683,1 end