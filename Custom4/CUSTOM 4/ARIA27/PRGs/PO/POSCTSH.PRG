*:************************************************************************
*: Program file  : POSCTSH.PRG
*: Program desc. : Imported Style Cost Sheet
*: For screen    : MFCSTSH.SCX (0,1,2,3,4,5,6)
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Style Purchase Oreder Module
*:      Developer: Reham Al-Allamy
*:************************************************************************
*: Documented *E300722,1 Rewrite the program to work in the 2.7 version
*:************************************************************************
*: Calls         : 
*:         Programs   : MFCSTSH.Fxp (Main Cost Sheet Program)
*:         Screens    : None
*:         Procedures : None
*:         Functions  : None
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example            :
*:  Call the program with "I" parameter for Imported Style Cost Sheet.
*:    DO (gcAppHome+'\MFCSTSH') WITH 'I'
*:************************************************************************
*:  Modifications :
*:************************************************************************
*
*-- Call the program with "I" parameter for Imported Style Cost Sheet.
DO (gcAppHome+'\MFCSTSH') WITH 'I'
