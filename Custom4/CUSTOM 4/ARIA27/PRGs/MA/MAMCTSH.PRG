*:************************************************************************
*: Program file  : MAMCTSH.PRG
*: Program desc. : Material Cost Sheet
*: For screen    : MFCSTSH.SCX (0,1,2,3,4,5,6)
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Material Module
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
*:   Call the program with "T" parameter for Material Cost Sheet.
*:    DO (gcAppHome+'\MFCSTSH') WITH 'T'
*:************************************************************************
*:  Modifications :
*:************************************************************************
*
*-- Call the program with "T" parameter for Material Cost Sheet.
DO (gcAppHome+'\MFCSTSH') WITH 'T'
