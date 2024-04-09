*:************************************************************************
*: Program file  : MfRecCT.PRG
*: Program desc. : Program for Bar Code temp. receiving cutting Ticket
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Manufacturing Module
*:      Developer: Reham Al-Allamy
*:************************************************************************
*: Documented *C101496,1 Bar Code temp. receiving CT
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
*:  DO gpDoProg WITH 'AWRMFRcCT',.F.,'MF'
*:*************************************************************
*:  Modifications :
*:************************************************************************
*
*-- Call the program with "M" parameter for Temp. Receiving Cutting Ticket.
DO (gcAppHome+'MFRCALL') WITH 'M'
