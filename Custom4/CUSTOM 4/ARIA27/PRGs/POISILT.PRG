*:************************************************************************
*: Program file  : POISILT.PRG
*: Program desc. : Program for Bar Code Create & Issue Inter Location Transfer
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Manufacturing Module
*:      Developer: Reham Al-Allamy
*:************************************************************************
*: Documented *C101496,1 Bar Code Create & Issue Inter Location Transfer
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
*:  DO gpDoProg WITH 'AWRPOISILT',.F.,'PO'
*:*************************************************************
*:  Modifications :
*:************************************************************************
*
*-- Call the program with "C" parameter for Create & Issue Inter Location Transfer.
DO (gcAppHome+'MFRCALL') WITH 'C'
