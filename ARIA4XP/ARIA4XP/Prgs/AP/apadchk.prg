***********************************************************************
*:  Program File: APADCHK.FXP
*:  Desc.       : Advanced Payment program
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 02/01/2012
*:  Reference   : *E303015,1 
*:************************************************************************
*:  This program "Advance manual check payment" is calling from  
*:  AP menu-->   
*:            Transactions-->
*:                          Advance payments-->
*:                                            manual check payments
*:************************************************************************
LOCAL lcProgram,lcRunProgram
lcProgram = 'APADVPAY.FXP'
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.ClientProgramHome+'AP\&lcProgram')
  lcRunProgram = oAriaApplication.ClientProgramHome+'AP\&lcProgram'
ELSE
  lcRunProgram = oAriaApplication.ProgramHome+'AP\&lcProgram'
ENDIF   
DO (lcRunProgram) WITH 1
