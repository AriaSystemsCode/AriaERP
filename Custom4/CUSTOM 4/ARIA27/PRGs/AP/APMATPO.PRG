*:************************************************************************
*:
*: Procedure file: APMATPO.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Mohamed Hassan Mohamed
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:  
*:  This program "Material PO receipts" is calling from  
*:  AP menu-->   
*:            Transactions-->
*:                     Invoice receipts-->
*:                                  Material PO receipts
*:      Documented 00/00/1994
*E300683,1 AHMED 06/04/97 Add prgs directory path to the calling of programs
*:************************************************************************
* 
*E300683,1 Call programs from PRGS directory
*DO APPyInv WITH  .F.,.F.,1
DO (gcAppHome + gcWinAppl + '\APPyInv ') WITH  .F.,.F.,1
*E300683,1 end