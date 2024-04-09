***********************************************************************
*:  Program File: APADCSH.FXP
*:  Desc.       : Advanced Payment program
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 02/01/2012
*:  Reference   : *E303015,1
*:************************************************************************
*:  This program   "Cash payment"  is calling from
*:  AP menu-->
*:            Transactions-->
*:                          Advance payments-->
*:                                            Cash payments
*:************************************************************************
LOCAL lcProgram,lcRunProgram
lcProgram = 'APADVPAY.FXP'
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.ClientProgramHome+'AP\&lcProgram')
  lcRunProgram = oAriaApplication.ClientProgramHome+'AP\&lcProgram'
ELSE
  lcRunProgram = oAriaApplication.ProgramHome+'AP\&lcProgram'
ENDIF
DO (lcRunProgram) WITH 3
