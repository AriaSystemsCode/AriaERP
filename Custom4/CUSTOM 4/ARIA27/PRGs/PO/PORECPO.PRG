*:************************************************************************
*: Program file  : PoRecPO.PRG
*: Program desc. : Program for Bar Code temp. receiving style purchase order
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Style Purchase Order Module
*:      Developer: Reham Al-Allamy
*:************************************************************************
*: Documented *C101496,1 Bar Code temp. receiving PO
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
*-- Call the program with "I" parameter for Temp. Receiving Style Purchase Order.
DO (gcAppHome+'MFRCALL') WITH 'I'
