******************************************************************
*! Name      : APUpdLNo
*! Developer : Amin Khodary 
*! Date      : 11/05/2001
*! Purpose   : Update new fields cApdGlAct and cApdLinNo in APInvTkt file with the proper values
*!             get from APInvDtl file
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   :  None
*!*************************************************************

PRIVATE lcPath ,  lcDataDir ,llUsApDtl , llUsApTkt , llExitApDtl ,llExitApTkt

lcPath = SET('PATH')

STORE .T. TO llUsApDtl , llUsApTkt 
STORE .F. TO llExitApDtl ,llExitApTkt

lcDataDir = GETDIR('','Select data directory')

IF TYPE('l