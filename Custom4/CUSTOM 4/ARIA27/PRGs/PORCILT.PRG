*:************************************************************************
*: Program file  : PORCILT.PRG
*: Program desc. : Program for Bar Code Receiving Inter Location Transfer
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Manufacturing Module
*:      Developer: Reham Al-Allamy
*:************************************************************************
*: Documented *C101496,1 Bar Code Receiving Inter Location Transfer
*:************************************************************************
*: Calls         : 
*:         Procedures : None
*:         Programs   : MFRcAll.FXP 
*:         Screens    : None
*:         Functions  : None
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Example            :
*:  DO gpDoProg WITH 'AWRPORCILT',.F.,'PO'
*:*************************************************************
*:  Modifications :
*:************************************************************************
*
*-- Call the program with "R" parameter for Receiving Inter Location Transfer.
DO (gcAppHome+'MFRCALL') WITH 'R'
