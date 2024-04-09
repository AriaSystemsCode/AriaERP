*:************************************************************************
*: Program file  : ARDBADJ.PRG
*: Program desc. : Program To Enter Debit Adjustment.
*: For screen    : ARDCADJ.SCX (0,1,2)
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Accounts Receivable Module
*:      Developer: Reham Al-Allamy
*:************************************************************************
*: Documented *E300459,1 Rewrite the program to work in the 2.7 version
*:************************************************************************
*: Calls         : 
*:         Procedures : None
*:         Programs   : ARDCADJ.FXP (Main Debit & Credit Adjustment Program)
*:         Screens    : None
*:         Functions  : None
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Example            :
*:  DO gpDoProg WITH 'AWRARDBADJ',.F.,'AR'
*:*************************************************************
*:  Modifications :
*:************************************************************************
*
*-- Call the program with "D" parameter for Debit Adjustment.
*DO (gcAppHome+gcWinAppl+'\ARDCADJ') WITH 'D'
DO (gcAppHome+'ARDCADJ') WITH 'D'